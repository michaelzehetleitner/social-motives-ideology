# Tests for the AP4 CFA reporting core (R/ap4_cfa_reporting.R).

local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) {
    parent <- dirname(dir)
    if (identical(parent, dir)) stop("project root not found from ", getwd())
    dir <- parent
  }
  for (file in c("config.R", "ap4_reliability.R", "ap4_cfa_reporting.R",
                 "report_supplement_measurement.R", "report_helpers.R")) {
    source(file.path(dir, "R", file), local = FALSE)
  }
})

cfa_report_model <- list(model = "two_factor", model_label = "Two-factor CFA")

test_that("typed empty reporting retains a failed fit as data", {
  empty <- ap4_cfa_reporting_empty(cfa_report_model, issue = "model did not fit")

  expect_named(empty, c(
    "loadings", "factor_correlations", "residuals", "ranked_residuals",
    "ave", "issues"
  ))
  expect_equal(nrow(empty$loadings), 0L)
  expect_equal(empty$issues$model, "two_factor")
  expect_equal(empty$issues$severity, "error")
  expect_match(empty$issues$message, "did not fit")
})

create_cfa_residual_export_fixture <- function(model, error = NA_character_) {
  codebook <- list(
    items = tibble::tibble(item_code = c("item_one", "item_two"),
      source_label = c("Item 1", "Item 2"), item_text_de = c("First item", "Second item")),
    scales = tibble::tibble(scale_key = "factor_one", label = "Factor one"))
  supplement <- list(
    fixed_definition = list(label = model,
      factors = list(factor_one = c("Item 1", "Item 2"))),
    available_estimates = NULL, warnings = character(), error = error,
    residual_issue = NA_character_,
    full_residual_output = if (is.na(error)) tibble::tibble(
      item_1 = "item_one", item_2 = "item_two",
      residual_correlation = 0.02, standardized_residual_correlation = 0.4) else tibble::tibble())
  status <- tibble::tibble(model = model, n = 80L,
    converged = is.na(error), admissible = is.na(error))
  tabulate_cfa_model_row(status, supplement, codebook)
}

test_that("all failed CFA models export the ten residual headings and retain their reasons", {
  scales <- create_cfa_residual_export_fixture("scale", "scale model did not converge")
  sets <- create_cfa_residual_export_fixture("set", "set model was not admissible")
  plan <- list(root = withr::local_tempdir(), factor_analysis = list(cfa_reporting =
    list(residual_data_file = "cfa/residuals.csv")))
  path <- write_full_cfa_residual_output(scales, sets, plan)
  expect_identical(path, normalizePath(file.path(plan$root, "cfa/residuals.csv")))
  residuals <- readr::read_csv(path, show_col_types = FALSE)
  expect_equal(nrow(residuals), 0L)
  expect_identical(names(residuals), c(
    "model", "model_label", "item_1", "item_1_label", "item_2", "item_2_label",
    "bentler_residual", "std_residual", "abs_std_residual", "rank_abs_std_residual"))
  retained <- ap4_cfa_collect_reporting(scales, sets, component = "residuals")
  expect_identical(unname(vapply(retained, typeof, character(1))),
    c(rep("character", 6L), rep("double", 3L), "integer"))
  issues <- ap4_cfa_collect_reporting(scales, sets, component = "issues")
  expect_identical(issues$message, c(scales$error, sets$error))
  messages <- rh_cfa_estimation_messages(scales, sets, labels = c(scale = "Scale"))
  expect_identical(messages$message, c(scales$error, sets$error))
})

test_that("successful CFA residuals export unchanged alongside failed models", {
  scales <- create_cfa_residual_export_fixture("scale")
  sets <- create_cfa_residual_export_fixture("set")
  plan <- list(root = withr::local_tempdir(), factor_analysis = list(cfa_reporting =
    list(residual_data_file = "residuals.csv")))
  path <- write_full_cfa_residual_output(scales, sets, plan)
  read_residuals <- function() readr::read_csv(path, show_col_types = FALSE,
    col_types = readr::cols(rank_abs_std_residual = readr::col_integer()))
  expected <- ap4_cfa_collect_reporting(scales, sets, component = "residuals")
  expect_equal(as.data.frame(read_residuals()), as.data.frame(expected))

  failed <- create_cfa_residual_export_fixture("failed_set", "controlled fit failure")
  mixed <- dplyr::bind_rows(sets, failed)
  expect_identical(write_full_cfa_residual_output(scales, mixed, plan), path)
  expect_equal(as.data.frame(read_residuals()), as.data.frame(expected))
  messages <- rh_cfa_estimation_messages(scales, mixed, labels = c(scale = "Scale"))
  expect_identical(messages$model, "failed_set")
  expect_identical(messages$message, "controlled fit failure")
})

test_that("CFA residual exports reject malformed tables including within a mixed collection", {
  scales <- create_cfa_residual_export_fixture("scale")
  failed <- create_cfa_residual_export_fixture("failed_set", "controlled fit failure")
  plan <- list(root = withr::local_tempdir(), factor_analysis = list(cfa_reporting =
    list(residual_data_file = "residuals.csv")))
  residuals <- failed$reporting[[1]]$residuals
  wrong_type <- residuals
  wrong_type$rank_abs_std_residual <- numeric()
  extra_column <- residuals
  extra_column$unexpected <- character()
  for (malformed in list(tibble::tibble(), residuals[, -1L], wrong_type, extra_column)) {
    failed$reporting[[1]]$residuals <- malformed
    expect_error(write_full_cfa_residual_output(scales, failed, plan), "expected columns and types")
  }
})


create_cfa_fit_measure_fixture <- function() {
  c(chisq.scaled = 11, df.scaled = 9, pvalue.scaled = .21,
    chisq = 12, df = 10, pvalue = .31,
    cfi.robust = .93, cfi.scaled = .91, cfi = .89,
    tli.robust = .94, tli.scaled = .92, tli = .90,
    rmsea.robust = .05, rmsea.scaled = .08, rmsea = .10,
    rmsea.ci.lower.robust = .03, rmsea.ci.lower.scaled = .06, rmsea.ci.lower = .08,
    rmsea.ci.upper.robust = .07, rmsea.ci.upper.scaled = .10, rmsea.ci.upper = .12,
    srmr = .04, srmr.scaled = .14)
}

test_that("CFA fit indices use the specified versions when alternatives also exist", {
  result <- extract_cfa_fit_indices(create_cfa_fit_measure_fixture(), "two_factor")
  expect_equal(result$fit_indices, tibble::tibble(model = "two_factor",
    chisq = 11, df = 9, pvalue = .21, cfi = .93, tli = .94, rmsea = .05,
    rmsea_90_lower = .03, rmsea_90_upper = .07, srmr = .04))
  expect_identical(result$issue, NA_character_)
})

test_that("omitted robust CFA indices stay unavailable despite other versions", {
  values <- create_cfa_fit_measure_fixture()
  omitted <- c("cfi.robust", "tli.robust", "rmsea.robust",
               "rmsea.ci.lower.robust", "rmsea.ci.upper.robust")
  result <- extract_cfa_fit_indices(values[!names(values) %in% omitted], "two_factor")
  expect_true(all(is.na(result$fit_indices[c("cfi", "tli", "rmsea",
                                            "rmsea_90_lower", "rmsea_90_upper")])))
  expect_identical(result$fit_indices$chisq, 11)
  expect_identical(result$fit_indices$srmr, .04)
  for (name in omitted) expect_match(result$issue, name, fixed = TRUE)
})

test_that("named robust CFA NAs remain unavailable despite other versions", {
  values <- create_cfa_fit_measure_fixture()
  values[c("cfi.robust", "tli.robust", "rmsea.robust",
           "rmsea.ci.lower.robust", "rmsea.ci.upper.robust")] <- NA_real_
  result <- extract_cfa_fit_indices(values, "two_factor")
  expect_true(all(is.na(result$fit_indices[c("cfi", "tli", "rmsea",
                                            "rmsea_90_lower", "rmsea_90_upper")])))
  # An index returned as NA was present; it is not labelled as omitted.
  expect_identical(result$issue, NA_character_)
})

test_that("omitted scaled CFA indices never use the ordinary versions", {
  values <- create_cfa_fit_measure_fixture()
  omitted <- c("chisq.scaled", "df.scaled", "pvalue.scaled")
  result <- extract_cfa_fit_indices(values[!names(values) %in% omitted], "two_factor")
  expect_true(all(is.na(result$fit_indices[c("chisq", "df", "pvalue")])))
  expect_identical(result$fit_indices$cfi, .93)
  for (name in omitted) expect_match(result$issue, name, fixed = TRUE)
})

test_that("exact omitted-index reasons reach the current CFA report messages", {
  values <- create_cfa_fit_measure_fixture()
  result <- extract_cfa_fit_indices(values[names(values) != "cfi.robust"], "two_factor")
  codebook <- list(
    items = tibble::tibble(item_code = "item_one", source_label = "Item 1",
                          item_text_de = "First item"),
    scales = tibble::tibble(scale_key = "factor_one", label = "Factor one"))
  supplement <- list(
    fixed_definition = list(label = "Test CFA", factors = list(factor_one = "Item 1")),
    available_estimates = list(fit_indices = result$fit_indices,
      loadings = tibble::tibble(), factor_correlations = tibble::tibble(), ave = tibble::tibble()),
    extraction_issues = tibble::tibble(component = "fit_indices", message = result$issue),
    warnings = "Existing estimation warning", error = NA_character_,
    residual_issue = NA_character_, full_residual_output = tibble::tibble())
  status <- tibble::tibble(model = "two_factor", n = 80L, converged = TRUE, admissible = TRUE)
  row <- tabulate_cfa_model_row(status, supplement, codebook)
  expect_true(is.na(row$cfi))
  expect_match(row$note, "Existing estimation warning", fixed = TRUE)
  expect_match(row$note, result$issue, fixed = TRUE)
  expect_identical(row$reporting[[1]]$issues$message, result$issue)
  messages <- rh_cfa_estimation_messages(row, row[FALSE, ], labels = c(two_factor = "Test CFA"))
  expect_equal(nrow(messages), 1L)
  expect_match(messages$message, "cfi.robust", fixed = TRUE)
  expect_match(messages$message, result$issue, fixed = TRUE)
})

test_that("CFA participant counts prefer the recorded model-specific sample", {
  count <- extract_cfa_participant_count(list(n = 29L, fit = NULL))
  expect_identical(count$n, 29L)
  expect_identical(count$note, NA_character_)
  expect_identical(extract_cfa_participant_count(list(n = 0L, fit = NULL))$n, 0L)
})

test_that("CFA participant counts can be read from fitted metadata", {
  local_mocked_bindings(lavInspect = function(object, what) {
    expect_identical(what, "nobs")
    c(12L, 17L)
  }, .package = "lavaan")
  count <- extract_cfa_participant_count(list(fit = structure(list(), class = "lavaan")))
  expect_identical(count$n, 29L)
  expect_identical(count$note, NA_character_)
})

test_that("missing CFA counts stay unavailable with a reason in the report", {
  model <- list(label = "Test CFA", factors = list(factor_one = "Item 1"))
  models <- list(
    data = data.frame(common_input = seq_len(900L)),
    models = list(two_factor = model),
    questions = list(two_factor = "scale unidimensionality"),
    fits = list(two_factor = list(fit = NULL, syntax = "factor_one =~ item_one",
      warnings = character(), error = "controlled failure")),
    assessment = list(two_factor = list(converged = FALSE, admissible = FALSE,
      note = "controlled failure")))
  reporting <- build_cfa_reporting_data(models, list())
  expect_identical(reporting$model_status$n, NA_integer_)
  codebook <- list(
    items = tibble::tibble(item_code = "item_one", source_label = "Item 1",
                          item_text_de = "First item"),
    scales = tibble::tibble(scale_key = "factor_one", label = "Factor one"))
  row <- tabulate_cfa_scale_models(reporting, codebook)
  expect_identical(row$n, NA_integer_)
  expect_match(row$note, "CFA participant count unavailable", fixed = TRUE)
  messages <- rh_cfa_estimation_messages(row, row[FALSE, ], labels = c(two_factor = "Test CFA"))
  expect_match(messages$message, "CFA participant count unavailable", fixed = TRUE)
  expect_identical(rh_cfa_fit_constant_text(row, row[FALSE, ], row[FALSE, ]), "")
})
