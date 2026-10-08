# All inputs, including real-source preparation tests, are temporary synthetic files.
local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) dir <- dirname(dir)
  for (file in list.files(file.path(dir, "R"), pattern = "[.]R$", full.names = TRUE)) source(file, local = FALSE)
})
intake_cfg <- zm_config(profile = "full")
intake_book <- zm_codebook(intake_cfg)
intake_raw <- read_qualtrics_export(file.path(intake_cfg$root, "data/synthetic/zm_panel_synthetic.sav"))
intake_required <- c(unique(unlist(intake_book$scales$item_codes)), "demo_age", "demo_hh_members", "demo_income_hh_net")
intake_raw <- intake_raw[intake_raw$survey_status == "complete" & intake_raw$demo_gender %in% 1:2 &
                         stats::complete.cases(intake_raw[intake_required]), ][1:20, ]
intake_raw$Kommentarfeld <- ""

intake_fixture <- function(source = "synthetic", raw = intake_raw) {
  cfg <- intake_cfg
  cfg$root <- withr::local_tempdir(.local_envir = parent.frame())
  cfg$meta$codebook_dir <- "codebooks"
  dir.create(file.path(cfg$root, "codebooks"))
  books <- c("codebook_items.csv", "codebook_scales.csv", "codebook_factors.csv")
  file.copy(file.path(intake_cfg$root, intake_cfg$meta$codebook_dir, books), file.path(cfg$root, "codebooks", books))
  input <- file.path(cfg$root, "original.sav")
  write_qualtrics_sav(raw, input, file.path(intake_cfg$root, "data/synthetic/template/ZM-ASC-panel_random-data_2026-09-05.sav"))
  approval <- file.path(cfg$root, "approved.yaml")
  ap3_intake_write_proposal(ap3_intake_proposal(raw, cfg), cfg, approval, source)
  list(cfg = cfg, input = input, approval = approval, source = source,
       paths = zm_clean_intake_paths(source, cfg))
}
prepare_fixture <- function(x) zm_prepare_clean_intake(x$input, x$cfg, x$source, approval = x$approval)
read_fixture <- function(x, ready = TRUE) zm_validate_clean_intake(
  x$paths[["data"]], x$paths[["receipt"]], x$cfg, x$source, require_ready = ready)

test_that("preparation writes filled scientific values and separate observed margins", {
  x <- intake_fixture(); prepare_fixture(x)
  data <- read_fixture(x)
  receipt <- check_intake_approval(x$paths, x$source, x$cfg)
  demographics <- read_prepared_demographics(receipt)
  results <- readRDS(x$paths[["preparation"]])
  expect_true(file.exists(x$input))
  expect_identical(receipt$state, "ready")
  expect_identical(names(data), ap3_allowlist_expand(x$cfg$data_files$scientific_use_file$keep, intake_book))
  expect_false(any(c("demo_age", "demo_hh_members", "demo_income_hh_net", "education", "east_west",
                     "Duration__in_seconds_", "ResponseId", "Kommentarfeld") %in% names(data)))
  expect_setequal(data$respondent_id, seq_len(nrow(intake_raw)))
  expect_false(identical(data$respondent_id, seq_len(nrow(intake_raw))))
  expect_identical(names(demographics), c("age", "income", "education", "east_west", "duration"))
  expect_equal(sort(demographics$age), sort(intake_raw$demo_age))
  expect_equal(sort(demographics$duration), sort(intake_raw$Duration__in_seconds_))
  expect_equal(results$exclusions$log, apply_study_exclusions(intake_raw, intake_cfg)$log)
  expect_null(results$political_sample$scores)
  expect_true(all(vapply(results$political_sample$histogram_data, function(x) is.null(x) ||
                          all(c("xmin", "xmax", "count") %in% names(x)), logical(1))))
  expect_identical(receipt$profile, "full")
  recursive_classes <- function(x) c(class(x), if (is.list(x)) unlist(lapply(x, recursive_classes)))
  expect_false(any(c("brmsfit", "stanfit", "CmdStanMCMC") %in% recursive_classes(results)))
})

test_that("observed missing values survive preparation even where the fill supplies values", {
  x <- intake_fixture()
  real_prepare <- prepare_analysis_at_intake
  env <- new.env(parent = environment(zm_prepare_clean_intake))
  env$prepare_analysis_at_intake <- function(data, codebook, analysis_plan) {
    prepared <- real_prepare(data, codebook, analysis_plan)
    prepared$observed$age[1] <- NA_real_
    prepared$observed$income[2] <- NA_real_
    prepared$imputation$imputed_cells <- tibble::tibble(respondent_id = prepared$data$respondent_id[1],
      variable = "demo_age", kind = "demographic", model = "demo_age", value = 42.5,
      lower = 29, upper = 57, n_fit_rows = 19L)
    prepared
  }
  prepare <- zm_prepare_clean_intake; environment(prepare) <- env
  prepare(x$input, x$cfg, x$source, approval = x$approval)
  receipt <- check_intake_approval(x$paths, x$source, x$cfg)
  demographics <- read_prepared_demographics(receipt)
  expect_equal(sum(is.na(demographics$age)), 1L)
  expect_equal(sum(is.na(demographics$income)), 1L)
  expect_false(anyNA(read_fixture(x)$age_band_midpoint))
  cells <- readRDS(x$paths[["preparation"]])$imputation$imputed_cells
  expect_equal(cells$value, 42.5)
  expect_equal(c(cells$lower, cells$upper), c(29, 57))
  expect_equal(cells$variable, "demo_age")
})

test_that("real preparation retains the original and all comments while making analysis ready", {
  raw <- intake_raw
  raw$Kommentarfeld[1:2] <- c("Synthetic retained comment one", "Synthetic retained comment two")
  x <- intake_fixture("real", raw)
  original_hash <- zm_intake_hash(x$input)
  prepare_fixture(x)
  receipt <- yaml::read_yaml(x$paths[["receipt"]])
  expect_identical(receipt$state, "ready")
  expect_identical(zm_intake_hash(x$input), original_hash)
  expect_equal(nrow(read_fixture(x)), nrow(raw))
  comments <- readr::read_csv(receipt$comments_file, show_col_types = FALSE)
  expect_equal(comments$comment, raw$Kommentarfeld[1:2])
  expect_length(unique(comments$respondent_id), 2L)
  before <- vapply(x$paths, zm_intake_hash, character(1))
  # Simulate MZ's later deletion, only inside this disposable synthetic fixture.
  unlink(x$input)
  expect_false(file.exists(x$input))
  expect_equal(nrow(read_fixture(x)), nrow(raw))
  expect_identical(vapply(x$paths, zm_intake_hash, character(1)), before)
  expect_equal(readr::read_csv(receipt$comments_file, show_col_types = FALSE)$comment,
               raw$Kommentarfeld[1:2])
})

test_that("failed preparation read-back retains the original and private comments", {
  raw <- intake_raw; raw$Kommentarfeld[1] <- "Synthetic retained comment"
  x <- intake_fixture("real", raw)
  original_hash <- zm_intake_hash(x$input)
  env <- new.env(parent = environment(zm_prepare_clean_intake))
  read_data <- read_clean_study_data
  env$read_clean_study_data <- function(files) {
    data <- read_data(files)
    if (endsWith(files[["data"]], "_comments.csv")) data <- data[0, ]
    data
  }
  prepare <- zm_prepare_clean_intake; environment(prepare) <- env
  expect_error(prepare(x$input, x$cfg, x$source, approval = x$approval), "read-back values differ")
  expect_identical(zm_intake_hash(x$input), original_hash)
  receipt <- yaml::read_yaml(x$paths[["receipt"]])
  expect_identical(receipt$state, "prepared")
  expect_error(read_fixture(x), "not ready")
  expect_equal(readr::read_csv(receipt$comments_file, show_col_types = FALSE)$comment,
               raw$Kommentarfeld[1])
})

test_that("changed prepared inputs are rejected while the original and comments remain intact", {
  raw <- intake_raw; raw$Kommentarfeld[1] <- "Synthetic retained comment"
  x <- intake_fixture("real", raw); prepare_fixture(x)
  comments_path <- yaml::read_yaml(x$paths[["receipt"]])$comments_file
  original_hash <- zm_intake_hash(x$input)
  comments_hash <- zm_intake_hash(comments_path)
  for (name in c("data", "demographics", "preparation")) {
    bytes <- readBin(x$paths[[name]], "raw", n = file.info(x$paths[[name]])$size)
    cat("changed", file = x$paths[[name]], append = TRUE)
    expect_error(read_fixture(x), "hash")
    expect_identical(zm_intake_hash(x$input), original_hash)
    expect_identical(zm_intake_hash(comments_path), comments_hash)
    writeBin(bytes, x$paths[[name]])
  }
  expect_equal(nrow(read_fixture(x)), nrow(raw))
})

test_that("receipt write failures retain the original and comments", {
  for (fail_at in c("prepared", "ready")) {
    raw <- intake_raw; raw$Kommentarfeld[1] <- "Synthetic retained comment"
    x <- intake_fixture("real", raw)
    original_hash <- zm_intake_hash(x$input)
    env <- new.env(parent = environment(zm_prepare_clean_intake))
    write_receipt <- zm_intake_write_receipt
    env$zm_intake_write_receipt <- function(value, path) {
      if (identical(value$state, fail_at)) stop("Injected receipt write failure.")
      write_receipt(value, path)
    }
    prepare <- zm_prepare_clean_intake; environment(prepare) <- env
    expect_error(prepare(x$input, x$cfg, x$source, approval = x$approval), "Injected receipt")
    expect_identical(zm_intake_hash(x$input), original_hash)
    comments_path <- file.path(x$cfg$root, "data/private/real_comments.csv")
    expect_equal(readr::read_csv(comments_path, show_col_types = FALSE)$comment, raw$Kommentarfeld[1])
    expect_error(read_fixture(x), "missing|not ready")
  }
})

test_that("existing outputs, bad originals and mismatched approvals stop preparation", {
  x <- intake_fixture(); prepare_fixture(x)
  hashes <- vapply(x$paths, zm_intake_hash, character(1))
  expect_error(prepare_fixture(x), "overwrite")
  expect_identical(vapply(x$paths, zm_intake_hash, character(1)), hashes)
  for (bad in c("duplicate", "blank", "test")) {
    raw <- intake_raw
    if (bad == "duplicate") raw$ResponseId[2] <- raw$ResponseId[1]
    if (bad == "blank") raw$ResponseId[1] <- " "
    if (bad == "test") raw$Status[1] <- "Survey Preview"
    x <- intake_fixture(raw = raw)
    expect_error(prepare_fixture(x), "IDs|preview/test")
    expect_false(any(file.exists(x$paths)))
  }
  x <- intake_fixture(); y <- yaml::read_yaml(x$approval); y$data_source <- "real"; yaml::write_yaml(y, x$approval)
  expect_error(prepare_fixture(x), "approved for")
  expect_true(file.exists(x$input))
})

test_that("receipt, schema, profiles and observed margin files are verified before analysis", {
  x <- intake_fixture(); prepare_fixture(x)
  y <- yaml::read_yaml(x$paths[["receipt"]])
  for (edit in list(
    function(y) { y$data_source <- "real"; y },
    function(y) { y$state <- "prepared"; y },
    function(y) { y$profile <- "smoke"; y },
    function(y) { y$checks$unique_response_ids <- FALSE; y },
    function(y) { y$input_sha256 <- NULL; y },
    function(y) { y$files$data$schema[[1]]$name <- "ResponseId"; y }
  )) {
    yaml::write_yaml(edit(y), x$paths[["receipt"]])
    expect_error(read_fixture(x), "match|not ready|profile|checks|hash|schema")
  }
  yaml::write_yaml(y, x$paths[["receipt"]])
  typed <- add_col_types(read_clean_study_data(x$paths), y$files$data$schema)
  expect_error(check_study_data_matches_approval(typed[-1, ], y, x$cfg), "dimensions")
  typed$respondent_id[2] <- typed$respondent_id[1]
  expect_error(check_study_data_matches_approval(typed, y, x$cfg), "unique")
})

test_that("all selected prepared inputs are tracked with the configured codebooks", {
  for (source in c("synthetic", "real")) {
    files <- select_analysis_input_files(source, intake_cfg$root, intake_cfg$meta$codebook_dir)
    expect_identical(names(files), c("analysis_plan", "codebook_items", "codebook_scales", "codebook_factors",
                                     "data", "demographics", "preparation", "receipt"))
    expect_identical(files[c("data", "demographics", "preparation", "receipt")], zm_clean_intake_paths(source, intake_cfg))
    expect_false(any(grepl("(^|/)data/(private|raw)/|original", files)))
  }
  expect_identical(check_run_spec(list(data_source = " Synthetic "))$data_source, "synthetic")
  expect_error(check_run_spec(list(data_source = "export")), "synthetic or real")
})
