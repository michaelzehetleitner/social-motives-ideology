# LAYER 2 tests — the facts of the S6 and S7 answers
# (R/report_supplement_sampling_diagnostics.R, R/report_supplement_explained_variance.R).

local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) {
    parent <- dirname(dir)
    if (identical(parent, dir)) stop("project root not found from ", getwd())
    dir <- parent
  }
  source(file.path(dir, "R", "report_supplement_sampling_diagnostics.R"), local = FALSE)
  source(file.path(dir, "R", "report_supplement_explained_variance.R"), local = FALSE)
})

test_that("a failed fit is listed with the diagnostics it failed, or the gate alone", {
  diagnostics <- tibble::tibble(
    outcome = c("asc_agg", "asc_sub", "asc_conv"), slope_sd = .2, family = "gaussian",
    ess_ok = TRUE, rhat_ok = TRUE, divergences_ok = TRUE, treedepth_ok = TRUE,
    bfmi_ok = c(TRUE, FALSE, TRUE),
    ok = c(TRUE, FALSE, FALSE), gate_status = c("ok", "not_interpretable", "not_interpretable")
  )
  checked <- check_sampling_diagnostics(diagnostics)
  expect_length(checked$components, 5L)
  expect_identical(checked$failed$outcome, c("asc_sub", "asc_conv"))
  expect_identical(checked$failed$failed_components, list("BFMI", character()))
})

test_that("a retried fit that passed is not a failure", {
  diagnostics <- tibble::tibble(
    outcome = "asc_agg", slope_sd = .2, family = "gaussian",
    ess_ok = TRUE, rhat_ok = TRUE, divergences_ok = TRUE, treedepth_ok = TRUE, bfmi_ok = TRUE,
    ok = TRUE, gate_status = "retried_ok"
  )
  expect_equal(nrow(check_sampling_diagnostics(diagnostics)$failed), 0L)
})

test_that("the least and most explained variance come from the primary rows with a value", {
  r2 <- tibble::tibble(outcome = c("a", "b", "c", "a"), role = c("primary", "primary", "primary", "sweep"),
                       r2_median = c(.1, .3, NA, .9))
  checked <- check_explained_variance(r2, c("a", "b", "c"))
  expect_identical(checked$least, "a")
  expect_identical(checked$most, "b")
  expect_identical(checked$missing, "c")
  none <- check_explained_variance(r2[0, ], c("a", "b"))
  expect_true(is.na(none$least))
  expect_identical(none$missing, c("a", "b"))
})
