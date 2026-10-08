local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) dir <- dirname(dir)
  source(file.path(dir, "R", "config.R"), local = FALSE)
  source(file.path(dir, "R", "report_saved_inputs.R"), local = FALSE)
  source(file.path(dir, "R", "report_helpers.R"), local = FALSE)
  assign("saved_inputs_test_root", dir, envir = .GlobalEnv)
})

saved_inputs_fixture <- function(root = saved_inputs_test_root) {
  values <- setNames(rep(list(NULL), length(report_input_names())), report_input_names())
  values$pipeline_config <- zm_config("full", file.path(saved_inputs_test_root, "config/analysis_plan.yaml"))
  values$pipeline_config$root <- root
  values$data_source_used <- "synthetic"
  values$imputation_reporting_data <- list(model_status = data.frame(model = "age", status = "ok", unused = 1),
    n_unfilled = 0L, participants = data.frame(unused = 99))
  values$scale_score_distributions <- structure(data.frame(mean = 3), histograms = list(a = data.frame(count = 2L)))
  values$joint_correlation_changes <- list(draws = matrix(1, 2, 2), reporting = list(available = TRUE))
  values$joint_asc_aggregation <- list(draws = matrix(1, 2, 2), reporting = list(available = FALSE))
  values$supplement_measurement <- list(imputation = list(unused = 1),
    imputation_model_status = data.frame(unused = 1), filled_cells = data.frame(participant_id = "random-id", value = 3))
  values$supplement_prior_sensitivity <- list(model_card = list(analysis_data = list(
    gender = factor(c("female", "male", "female"), levels = c("male", "female")))))
  values$supplement_model_checks <- list(predictive_density = data.frame(x = 1:3, density = .2),
    predictive_draws = structure(data.frame(value = 1:3), unavailable_predictions = data.frame(note = "failed")))
  values$report_political_sample <- list(scores = data.frame(unused = 1), histogram_data = list(age = data.frame(count = 3)))
  values$report_provenance <- list(profile = "full", git_head = "original-calculation", built_at = "original-date")
  receipt <- list(cfg = values$pipeline_config, git_head = "original-calculation", built_at = "original-date",
    source_files = data.frame(path = character(), sha256 = character()),
    input_files = data.frame(path = character(), sha256 = character()), reproduction_mode = "models",
    reused_resampling = list(network = list(calculation_receipt = list(cfg = list(root = "/old/machine")))))
  list(values = values, receipt = receipt)
}

test_that("replay keeps displayed results and gates without unused records or fits", {
  fixture <- saved_inputs_fixture()
  reduced <- reduce_report_inputs(fixture$values)
  expect_identical(names(reduced), report_input_names())
  expect_identical(reduced$supplement_measurement$filled_cells, fixture$values$supplement_measurement$filled_cells)
  expect_identical(reduced$joint_asc_aggregation$reporting, fixture$values$joint_asc_aggregation$reporting)
  expect_identical(attr(reduced$scale_score_distributions, "histograms"), attr(fixture$values$scale_score_distributions, "histograms"))
  expect_null(reduced$joint_correlation_changes$draws)
  expect_null(reduced$supplement_model_checks$predictive_draws)
  expect_null(reduced$report_political_sample$scores)
  expect_named(reduced$imputation_reporting_data, c("model_status", "n_unfilled"))
  expect_named(reduced$imputation_reporting_data$model_status, c("model", "status"))
  expect_identical(attr(reduced$supplement_model_checks$predictive_density, "unavailable_predictions"), data.frame(note = "failed"))
  gender <- fixture$values$supplement_prior_sensitivity$model_card$analysis_data$gender
  expect_identical(rh_gender_coding_text(gender), rh_gender_coding_text(counts = reduced$supplement_prior_sensitivity$model_card$gender_counts))
  expect_error(assert_reduced_report_inputs(list(hidden = new.env())), "fitted model or executable")
  expect_error(assert_reduced_report_inputs(structure(1, hidden = list(draws = 1))), "Unused raw records")
  parser <- structure(data.frame(x = 1), class = c("spec_tbl_df", "tbl_df", "tbl", "data.frame"), problems = new.env())
  expect_silent(assert_reduced_report_inputs(strip_report_parser_metadata(parser)))
})

test_that("portable replay preserves calculation provenance and refuses profile mismatch or tampering", {
  root <- tempfile(); dir.create(root); on.exit(unlink(root, recursive = TRUE))
  fixture <- saved_inputs_fixture(root)
  path <- write_report_inputs(fixture$values, fixture$receipt, fixture$values$pipeline_config)
  saved <- read_report_inputs(path, root, expected_profile = "full")
  expect_identical(saved$values$pipeline_config$root, normalizePath(root))
  expect_identical(saved$calculation_receipt$cfg$root, ".")
  expect_identical(saved$calculation_receipt$reused_resampling$network$calculation_receipt$cfg$root, ".")
  expect_identical(saved$calculation_receipt$reproduction_mode, "models")
  expect_identical(saved$values$report_provenance$git_head, "original-calculation")
  expect_error(read_report_inputs(path, root, "smoke"), "cannot change the calculation profile")
  broken <- readRDS(path); broken$calculation_receipt$cfg$profile_name <- NULL; saveRDS(broken, path)
  expect_error(read_report_inputs(path, root, ""), "absent or inconsistent")
  broken$values$data_source_used <- "tampered"; saveRDS(broken, path)
  expect_error(read_report_inputs(path, root, ""), "integrity")
  expect_error(read_report_inputs(file.path(root, "absent.rds"), root), "Saved report inputs are absent")
})

test_that("real report inputs stay private while private replay remains available", {
  root <- tempfile(); dir.create(root); on.exit(unlink(root, recursive = TRUE))
  fixture <- saved_inputs_fixture(root); fixture$values$data_source_used <- "real"
  expect_error(write_report_inputs(fixture$values, fixture$receipt, fixture$values$pipeline_config), "synthetic only")
  private <- file.path(root, "data/private/report_inputs.rds")
  expect_identical(write_report_inputs(fixture$values, fixture$receipt, fixture$values$pipeline_config, private), normalizePath(private))
  expect_identical(read_report_inputs(private, root, "full")$values$data_source_used, "real")
})

test_that("freshness ignores presentation edits but reports changed or unavailable scientific inputs", {
  root <- tempfile(); dir.create(root); on.exit(unlink(root, recursive = TRUE))
  for (dir in c("config", "R", "report", "data", "codebook")) dir.create(file.path(root, dir))
  plan <- readLines(file.path(saved_inputs_test_root, "config/analysis_plan.yaml"))
  plan <- sub('codebook_dir: "../preregistration"', 'codebook_dir: "codebook"', plan, fixed = TRUE)
  writeLines(plan, file.path(root, "config/analysis_plan.yaml"))
  file.copy(list.files(file.path(saved_inputs_test_root, "../preregistration"), pattern = "^codebook_.*[.]csv$", full.names = TRUE), file.path(root, "codebook"))
  for (path in c("R/analysis.R", "R/report_helpers.R", "report/results_draft.qmd", "data/prepared.csv")) writeLines("original", file.path(root, path))
  fixture <- saved_inputs_fixture(root)
  paths <- c("R/analysis.R", "R/report_helpers.R", "report/results_draft.qmd")
  hashes <- function(paths) data.frame(path = paths, sha256 = vapply(file.path(root, paths), function(p) digest::digest(file = p, algo = "sha256"), ""), row.names = NULL)
  fixture$receipt$source_files <- hashes(paths)
  fixture$receipt$input_files <- hashes("data/prepared.csv")
  bundle <- list(values = fixture$values, calculation_receipt = fixture$receipt)
  expect_identical(check_report_input_freshness(bundle, root)$state, "current")
  writeLines("presentation changed", file.path(root, "R/report_helpers.R"))
  writeLines("wording changed", file.path(root, "report/results_draft.qmd"))
  expect_identical(check_report_input_freshness(bundle, root)$state, "current")
  writeLines("science changed", file.path(root, "R/analysis.R"))
  expect_identical(check_report_input_freshness(bundle, root)$state, "outdated")
  writeLines("original", file.path(root, "R/analysis.R")); unlink(file.path(root, "data/prepared.csv"))
  expect_identical(check_report_input_freshness(bundle, root)$state, "unavailable")
  bundle$values$pipeline_config$regression$prior_slope_sd <- -999
  expect_identical(check_report_input_freshness(bundle, root)$state, "outdated")
})

test_that("complete report uses the explicit bundle with no store fallback", {
  qmd <- paste(readLines(file.path(saved_inputs_test_root, "report/results_draft.qmd")), collapse = "\n")
  names <- regmatches(qmd, gregexpr("read_report_input\\([[:alnum:]_]+", qmd))[[1]]
  expect_setequal(sub("read_report_input\\(", "", names), report_input_names())
  expect_false(grepl("tar_read|tar_config_get|library\\(targets\\)", qmd))
  script <- paste(readLines(file.path(saved_inputs_test_root, "scripts/render_saved_report.R")), collapse = "\n")
  expect_false(grepl("tar_make|tar_read|source.*_targets", script))
})
