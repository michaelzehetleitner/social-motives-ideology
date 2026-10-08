# Small hand-offs between targets: the four data-file targets into the
# data-files summary and the companion file of the shared file, the deviations
# register into its report table, and real files into the calculation receipt.
# Every producer and consumer is the real one, chained through the route reader
# (tests/support/targets-route.R), which evaluates the actual `_targets.R`
# commands. No model is fitted here: the AP3 fill is the deterministic median
# stub of make_fill_stubs(). Nothing is written into data/ or report/: the four
# file targets write into a temporary directory.

local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) {
    parent <- dirname(dir)
    if (identical(parent, dir)) stop("project root not found from ", getwd())
    dir <- parent
  }
  for (f in c("config.R", "io_qualtrics.R", "ap1_exclusions.R", "ap3_preprocessing.R",
              "ap3_preparation.R", "ap3_fill.R", "ap3_data_files.R", "ap3_pipeline.R",
              "report_helpers.R", "calculation_receipt.R", "result_outputs.R")) {
    source(file.path(dir, "R", f), local = FALSE)
  }
  source(file.path(dir, "tests", "support", "targets-route.R"), local = FALSE)
})

pcl_root <- zm_root()
pcl_cfg <- zm_config("full", file.path(pcl_root, "config", "analysis_plan.yaml"))
pcl_book <- zm_codebook(pcl_cfg)
pcl_raw <- read_qualtrics_export(file.path(pcl_root, "data", "synthetic", "zm_panel_synthetic.sav"))
pcl_route <- tr_route(pcl_root, tr_intake(pcl_raw, pcl_cfg), pcl_cfg, pcl_book)
# The route's own environment: the production verbs the reader calls live here,
# so an interception must rebind them here and not in the global environment.
pcl_values <- environment(pcl_route)$values
pcl_env <- parent.env(pcl_values)

# Every file this test writes goes here and is removed when the file is done.
pcl_tmp <- tempfile("pipeline-closures-")
dir.create(pcl_tmp)
withr::defer(unlink(pcl_tmp, recursive = TRUE), testthat::teardown_env())

# ---- The four file targets as the input of the summary and the companion
# file, with only ap3_write_data_file()'s destination redirected. The real
# target commands and the real writers run.
pcl_files <- local({
  write_original <- get("ap3_write_data_file", pcl_env)
  written <- 0L
  rlang::local_bindings(ap3_write_data_file = function(data, path) {
    written <<- written + 1L
    write_original(data, file.path(pcl_tmp, sprintf("%02d-%s", written, basename(path))))
  }, .env = pcl_env)
  paths <- c(scientific = pcl_route("scientific_use_file"),
             demographics = pcl_route("demographics_file"),
             filled_cells = pcl_route("filled_cells_file"),
             dropped = pcl_route("dropped_respondents_file"))
  list(paths = paths, summary = pcl_route("data_files_summary"))
})

# ---- four file paths -> zm_data_files_summary() -----------------------------

test_that("the data-files summary has one row per written file with its own counts", {
  summary <- pcl_files$summary
  expect_identical(nrow(summary), 4L)
  expect_identical(summary$file,
                   c("scientific-use file", "deidentified demographics", "filled cells", "dropped respondents"))
  expect_identical(summary$path, unname(pcl_files$paths))
  expect_true(all(file.exists(summary$path)))

  for (i in seq_len(nrow(summary))) {
    back <- readr::read_csv(summary$path[i], col_types = readr::cols(.default = readr::col_character()),
                            progress = FALSE)
    expect_identical(summary$n_rows[i], nrow(back))
    expect_identical(summary$n_cols[i], length(names(back)))
    # The columns string is the file's header, in file order.
    expect_identical(strsplit(summary$columns[i], "; ", fixed = TRUE)[[1L]], names(back))
  }
})

# ---- data_files_summary -> zm_scientific_use_readme() -----------------------

test_that("the companion file states the shared file and the fill records", {
  summary <- pcl_files$summary
  path <- file.path(pcl_tmp, "scientific_use.README.txt")
  returned <- zm_scientific_use_readme(summary, pcl_cfg, path = path)
  expect_identical(returned, path)
  expect_true(file.exists(path))
  text <- paste(readLines(path, warn = FALSE), collapse = "\n")

  row <- summary[summary$file == "scientific-use file", , drop = FALSE]
  expect_match(text, basename(row$path[1]), fixed = TRUE)
  expect_match(text, paste0("Rows in the scientific-use file: ", row$n_rows[1], "; columns: ", row$n_cols[1], "."),
               fixed = TRUE)
  expect_match(text, "Items and scale scores contain the registered AP3 fills; gender is not imputed.", fixed = TRUE)
  expect_match(text, "Age and income per household member are shared as their analysis bands and representatives.", fixed = TRUE)
  expect_match(text, "each filled cell and its model, value and predictive interval", fixed = TRUE)
  expect_match(text, "preserve exclusions and model checks", fixed = TRUE)
  expect_match(text, "independently shuffled, with missing values preserved", fixed = TRUE)
  expect_match(text, "Its rows must not be joined to each other or to the response data.", fixed = TRUE)

  expect_error(zm_scientific_use_readme(summary[summary$file != "scientific-use file", , drop = FALSE],
                                        pcl_cfg, path = file.path(pcl_tmp, "unused.txt")),
               "must contain one scientific-use file")
})

# ---- zm_deviations_table() -> rh_deviations_table() -------------------------

test_that("the empty deviations register renders as the report's 'None to date' row", {
  deviations <- zm_deviations_table()
  expect_identical(nrow(deviations), 0L)
  expect_named(deviations, c("date", "preregistered", "deviation", "reason"))
  rendered <- paste(as.character(rh_deviations_table(deviations, engine = "kable",
                                                     target = "deviations_register")), collapse = "\n")
  expect_match(rendered, "None to date", fixed = TRUE)
  expect_match(rendered, "Deviation", fixed = TRUE)
  expect_match(rendered, "Source: target `deviations_register`.", fixed = TRUE)
})

# ---- real files -> record_calculation_receipt() -----------------------------

test_that("the receipt fingerprints the real input and source files", {
  input_files <- file.path(pcl_root, "data", "synthetic", "zm_panel_synthetic.sav")
  source_files <- c(file.path(pcl_root, "_targets.R"),
                    file.path(pcl_root, "R", "calculation_receipt.R"))
  receipt <- record_calculation_receipt(source_files, input_files, pcl_cfg)

  expect_identical(receipt$cfg, pcl_cfg)
  expect_identical(receipt$hash_algorithm, "sha256")
  # The receipt lists every named file, relative to the root, with its hash.
  expect_identical(receipt$input_files$path, "data/synthetic/zm_panel_synthetic.sav")
  expect_identical(receipt$source_files$path, c("_targets.R", "R/calculation_receipt.R"))
  expect_identical(receipt$input_files$sha256,
                   digest::digest(file = input_files, algo = "sha256"))
  expect_identical(receipt$source_files$sha256,
                   vapply(source_files, function(p) digest::digest(file = p, algo = "sha256"),
                          character(1), USE.NAMES = FALSE))
  expect_identical(receipt$r_version, R.version.string)
})
