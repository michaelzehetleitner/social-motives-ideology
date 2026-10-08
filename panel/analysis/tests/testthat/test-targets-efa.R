# The SRQ1 EFA section of _targets.R: the nine producers, the S1 target that
# builds its EFA tables from them, and the theoretical-count rule of those
# tables.

local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) {
    parent <- dirname(dir)
    if (identical(parent, dir)) stop("project root not found from ", getwd())
    dir <- parent
  }
  source(file.path(dir, "R", "config.R"), local = FALSE)
  source(file.path(dir, "R", "ap9_pipeline.R"), local = FALSE)
  source(file.path(dir, "R", "report_supplement_measurement.R"), local = FALSE)
})

efa_targets_root <- zm_root()

efa_targets_manifest <- withr::with_dir(
  efa_targets_root,
  withr::with_envvar(
    c(ZM_PROFILE = "smoke", ZM_DATA = "synthetic"),
    withCallingHandlers(
      targets::tar_manifest(callr_function = NULL, envir = new.env(parent = globalenv())),
      warning = function(w) {
        if (grepl("built under R version|unter R Version", conditionMessage(w))) {
          invokeRestart("muffleWarning")
        }
      }
    )
  )
)

efa_command_of <- function(name) {
  command <- efa_targets_manifest$command[efa_targets_manifest$name == name]
  if (length(command) != 1) stop("target '", name, "' not found exactly once in the manifest")
  gsub("\\s+", " ", command)
}

efa_producers <- c(
  "efa_item_sets", "efa_factor_number_results", "efa_factor_numbers",
  "efa_rotation_specifications", "efa_fits", "efa_item_assignments",
  "efa_item_correspondence", "efa_loading_clarity",
  "efa_explained_variance")

test_that("the nine SRQ1 producers are in the pipeline map", {
  for (target in efa_producers) {
    expect_true(target %in% efa_targets_manifest$name, info = target)
  }
  expect_length(efa_producers, 9L)
})

test_that("each SRQ1 producer runs its one operation on its own input", {
  # `targets` deparses the pipeline's `|>` steps as ordinary nested calls
  expect_identical(efa_command_of("efa_item_sets"),
                   paste("{ define_efa_item_sets(data_descriptive_reliability, analysis_inputs$config,",
                         "analysis_inputs$codebook) }"))
  expect_identical(
    efa_command_of("efa_factor_number_results"),
    paste("{ apply_factor_number_methods(estimate_polychoric_correlations(efa_item_sets),",
          "analysis_inputs$config) }"))
  expect_identical(
    efa_command_of("efa_factor_numbers"),
    paste("{ add_theoretical_factor_numbers(extract_suggested_factor_numbers(efa_factor_number_results),",
          "analysis_inputs$config) }"))
  expect_identical(
    efa_command_of("efa_rotation_specifications"),
    paste("{ add_efa_rotation_methods(efa_factor_numbers, one_factor = \"none\",",
          "multiple_factors = analysis_inputs$config$factor_analysis$rotation_primary) }"))
  expect_identical(efa_command_of("efa_fits"),
                   "{ fit_efa_models(efa_rotation_specifications, analysis_inputs$config) }")
  expect_identical(efa_command_of("efa_item_assignments"),
                   "{ extract_efa_item_assignments(efa_fits, analysis_inputs$config) }")
  expect_identical(efa_command_of("efa_item_correspondence"),
                   "{ extract_efa_item_correspondence(efa_item_assignments) }")
  expect_identical(efa_command_of("efa_loading_clarity"),
                   "{ extract_efa_loading_clarity(efa_item_correspondence, analysis_inputs$config) }")
  expect_identical(efa_command_of("efa_explained_variance"),
                   "{ extract_efa_explained_variance(efa_fits) }")
})

test_that("the S1 target builds its EFA tables from the SRQ1 results", {
  command <- efa_command_of("supplement_measurement")
  for (input in c("efa_fits", "efa_factor_numbers",
                  "efa_explained_variance", "efa_loading_clarity")) {
    expect_match(command, input, fixed = TRUE, info = input)
  }
  expect_false(grepl("data_descriptive_reliability", command, fixed = TRUE))
  body <- paste(deparse(body(assemble_supplement_measurement)), collapse = "\n")
  for (builder in c("tabulate_efa_scales(", "tabulate_efa_sets(", "tabulate_efa_correspondence(")) {
    expect_match(body, builder, fixed = TRUE, info = builder)
  }
  expect_match(efa_command_of("ap4_summary_files"),
               "write_measurement_summary_files(data_descriptive_reliability, codebook, analysis_plan)", fixed = TRUE)
})

test_that("the report table selects the theoretical count, never an empirical one", {
  flatten <- function(f) gsub("\\s+", " ", paste(deparse(body(f)), collapse = " "))
  projection <- flatten(build_efa_record)
  expect_match(projection, "zm_efa_theoretical_count(efa_factor_numbers, item_set)", fixed = TRUE)
  expect_match(projection, "n_factors_used = n_factors", fixed = TRUE)
  expect_match(flatten(zm_efa_theoretical_count), "\"theoretical\"", fixed = TRUE)
  # exactly one theoretical row per item set is required, so a projection can
  # never fall back to a count a criterion suggested
  numbers <- tibble::tibble(
    item_set = c("aa", "aa"), n_factors = c(2L, 3L),
    type = factor(c("estimated", "estimated"), levels = c("estimated", "theoretical")))
  expect_error(zm_efa_theoretical_count(numbers, "aa"), "0 theoretical rows", fixed = TRUE)
  numbers$type <- factor(c("theoretical", "theoretical"), levels = c("estimated", "theoretical"))
  expect_error(zm_efa_theoretical_count(numbers, "aa"), "2 theoretical rows", fixed = TRUE)
  numbers$type <- factor(c("estimated", "theoretical"), levels = c("estimated", "theoretical"))
  expect_identical(zm_efa_theoretical_count(numbers, "aa"), 3L)
})

