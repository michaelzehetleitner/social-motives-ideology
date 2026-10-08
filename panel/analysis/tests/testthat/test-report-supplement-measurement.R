# LAYER 2 tests — the facts of the S1 answer (R/report_supplement_measurement.R).

local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) {
    parent <- dirname(dir)
    if (identical(parent, dir)) stop("project root not found from ", getwd())
    dir <- parent
  }
  source(file.path(dir, "R", "report_supplement_measurement.R"), local = FALSE)
})

sm_plan <- list(factor_analysis = list(sets = list(
  list(key = "one", label = "One", scales = "s1"),
  list(key = "pair", label = "Pair", scales = c("s1", "s2"))
)))

test_that("a missing reliability and a failed confirmatory model are named", {
  answer <- check_measurement_completeness(
    reliability = tibble::tibble(scale_key = "s1", method = "alpha", omega_t = NA_real_, alpha = .7),
    cfa_scales = tibble::tibble(model = "s1", converged = TRUE, admissible = TRUE),
    analysis_plan = sm_plan
  )
  expect_false(answer$reliability_complete)
  expect_identical(answer$reliability_missing, "s2")
  expect_false(answer$cfa_complete)
  expect_identical(answer$cfa_missing, "s2")
})

test_that("complete results pass every check, and omega is read where it was the method", {
  answer <- check_measurement_completeness(
    reliability = tibble::tibble(scale_key = c("s1", "s2"), method = c("omega", "alpha"),
                                 omega_t = c(.8, NA_real_), alpha = c(NA_real_, .7)),
    cfa_scales = tibble::tibble(model = c("s1", "s2"), converged = TRUE, admissible = TRUE),
    analysis_plan = sm_plan
  )
  expect_true(answer$reliability_complete)
  expect_true(answer$cfa_complete)
})

test_that("an inadmissible model fails the confirmatory check", {
  answer <- check_measurement_completeness(
    reliability = tibble::tibble(scale_key = c("s1", "s2"), method = "alpha",
                                 omega_t = NA_real_, alpha = .7),
    cfa_scales = tibble::tibble(model = c("s1", "s2"), converged = TRUE, admissible = c(TRUE, FALSE)),
    analysis_plan = sm_plan
  )
  expect_false(answer$cfa_complete)
  expect_identical(answer$cfa_missing, "s2")
})
