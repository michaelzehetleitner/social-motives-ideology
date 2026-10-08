local({
  root <- normalizePath(getwd())
  while (!file.exists(file.path(root, "config", "analysis_plan.yaml"))) {
    parent <- dirname(root)
    if (identical(parent, root)) stop("analysis root not found")
    root <- parent
  }
  source(file.path(root, "R", "result_review_exports.R"), local = FALSE)
  source(file.path(root, "R", "report_synthetic_data.R"), local = FALSE)
  assign("review_export_root", root, envir = .GlobalEnv)
})

make_review_export_fixture <- function(output_dir) {
  table <- data.frame(outcome = c("a", "b"), estimate = c(pi / 17, NA_real_), fit_valid = c(TRUE, FALSE))
  contingency <- data.frame(n_cases = 2L, n_checked = 19L)
  attr(contingency, "fits") <- c(primary = 4L, sweep = 8L)
  list(data_source = "synthetic", coefficient_summaries = table,
       prior_width_sensitivity = list(list(interval = c("2.5%" = -0.1234567890123456, "97.5%" = pi))),
       prediction_decisions = table, planted_checks = list(contingencies = contingency),
       generating_comparison = table,
       calculation_receipt = list(cfg = list(root = "/saved/machine/root", profile_name = "smoke"),
         input_files = data.frame(path = "input.csv", sha256 = "input-fingerprint"),
         source_files = data.frame(path = "R/code.R", sha256 = "source-fingerprint"),
         hash_algorithm = "sha256", git_head = "saved-revision", git_dirty = TRUE,
         built_at = "2026-10-07T18:09:33Z", r_version = "saved R version"),
       analysis_plan = list(root = review_export_root), output_dir = output_dir)
}

test_that("compact exports retain numerical values, saved provenance and nested labels", {
  folder <- withr::local_tempdir()
  args <- make_review_export_fixture(folder)
  paths <- do.call(write_synthetic_review_exports, args)
  expect_length(paths, 7L)
  expect_true(all(file.exists(paths)))
  for (name in c("coefficients", "prediction_decisions", "generating_comparison")) {
    back <- readr::read_csv(file.path(folder, paste0(name, ".csv")), show_col_types = FALSE)
    expect_equal(as.data.frame(back), args$coefficient_summaries, tolerance = 1e-15)
  }
  receipt <- jsonlite::read_json(file.path(folder, "calculation_receipt.json"), simplifyVector = TRUE)
  expected <- args$calculation_receipt
  expected$cfg$root <- "."
  expect_equal(receipt, expected)
  expect_identical(args$calculation_receipt$cfg$root, "/saved/machine/root")
  widths <- jsonlite::read_json(file.path(folder, "prior_width_sensitivity.json"))
  expect_equal(widths[[1]]$interval[["2.5%"]], -0.1234567890123456, tolerance = 1e-15)
  expect_equal(widths[[1]]$interval[["97.5%"]], pi, tolerance = 1e-15)
  checks <- jsonlite::read_json(file.path(folder, "planted_checks.json"))
  expect_equal(checks$contingencies$attributes$fits$primary, 4)
  manifest <- jsonlite::read_json(file.path(folder, "manifest.json"))
  expect_identical(manifest$saved_run$profile, "smoke")
  expect_identical(manifest$saved_run$built_at, args$calculation_receipt$built_at)
  expect_true(manifest$saved_run$git_dirty)
  for (entry in manifest$files) {
    expect_identical(entry$sha256, digest::digest(file = file.path(folder, entry$file), algo = "sha256"))
  }
  args$variable_dictionary <- data.frame(key = "a", label = "Outcome A")
  args$generating_values <- list(true_beta = list(a = c(motive = 0.2)))
  expect_length(do.call(write_synthetic_review_exports, args), 9L)
  expect_equal(jsonlite::read_json(file.path(folder, "generating_values.json"))$true_beta$a$motive, 0.2)
})

test_that("real data and missing provenance fail before creating output", {
  folder <- file.path(withr::local_tempdir(), "not-created")
  args <- make_review_export_fixture(folder)
  args$data_source <- "real"
  expect_error(do.call(write_synthetic_review_exports, args), "data_source")
  expect_false(dir.exists(folder))
  args$data_source <- "synthetic"
  args$calculation_receipt$built_at <- NULL
  expect_error(do.call(write_synthetic_review_exports, args), "receipt is incomplete")
  expect_false(dir.exists(folder))
})

test_that("generating comparisons reject mismatched terms and retain gender conversion", {
  plan <- list(root = review_export_root, regression = list(motives = "motive", outcomes = "outcome"))
  truth <- list(true_beta = list(outcome = c(motive = 0.2, male = 0.1)))
  latent <- data.frame(male = c(0, 1, 0, 1))
  coefficients <- data.frame(outcome = "outcome", term = c("motive", "gendermale"),
    term_type = c("predictor", "covariate"), estimate = c(0.19, 0.17), fit_valid = TRUE,
    q2.5 = c(0.1, 0.05), q97.5 = c(0.3, 0.3))
  result <- compare_saved_synthetic_coefficients(coefficients, truth, latent, plan)
  expect_equal(nrow(result), 2L)
  expect_equal(result$generating_compared, c(0.2, 0.1 / stats::sd(latent$male)))
  coefficients$term[1] <- "renamed_motive"
  expect_error(compare_saved_synthetic_coefficients(coefficients, truth, latent, plan), "do not match")
})
