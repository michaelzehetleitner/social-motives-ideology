# LAYER 2 tests — the facts of the S4 answer (R/report_supplement_model_checks.R).

local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) {
    parent <- dirname(dir)
    if (identical(parent, dir)) stop("project root not found from ", getwd())
    dir <- parent
  }
  for (f in c("config.R", "ap10_inference.R", "report_helpers.R", "report_supplement_model_checks.R")) {
    source(file.path(dir, "R", f), local = FALSE)
  }
  source(file.path(dir, "tests", "support", "likelihood-pairs.R"), local = FALSE)
  assign("model_checks_root", dir, envir = .GlobalEnv)
})

test_that("unavailable checks are reported as unavailable, not as passed", {
  answer <- check_model_check_outcomes(
    prior_predictive = tibble::tibble(outcome = "asc_agg", share_below_min = NA_real_,
                                      share_above_max = NA_real_),
    posterior_predictive = tibble::tibble(observed = NA_real_, rep_lo = NA_real_, rep_hi = NA_real_),
    tails = tibble::tibble(outcome = "asc_agg", heavy_tails = NA),
    student_coefs = NULL
  )
  expect_false(answer$prior_any)
  expect_false(answer$pp_any)
  expect_false(answer$tail_any)
})

test_that("a placeholder row of an untriggered refit is not a refit", {
  answer <- check_model_check_outcomes(
    prior_predictive = tibble::tibble(outcome = c("asc_agg", "asc_sub"), share_below_min = .1,
                                      share_above_max = .1),
    posterior_predictive = tibble::tibble(outcome = c("asc_agg", "asc_sub"), stat = "kurtosis",
                                          observed = c(1, 3), rep_lo = c(0, 0), rep_hi = c(2, 2)),
    tails = tibble::tibble(outcome = c("asc_agg", "asc_sub"), heavy_tails = c(TRUE, TRUE)),
    student_coefs = tibble::tibble(outcome = c("asc_agg", "asc_sub"), term = c("zm_power", NA_character_),
                                   fit_available = c(TRUE, FALSE))
  )
  expect_true(answer$prior_all_span)
  expect_identical(answer$n_pp_outside, 1L)
  expect_identical(answer$triggered, c("asc_agg", "asc_sub"))
  expect_identical(answer$refitted, "asc_agg")
})

test_that("the likelihood comparison decides credibility per family and compares only valid pairs", {
  plan <- zm_config(profile = "full", path = file.path(model_checks_root, "config", "analysis_plan.yaml"))
  g <- tibble::tibble(
    outcome = c("asc_agg", "asc_agg", "sdo_dom"), term = c("zm_power", "zm_prestige", "zm_power"),
    term_type = "predictor", estimate = c(0.20, 0.05, 0.02), q2.5 = c(0.10, -0.05, -0.08),
    q97.5 = c(0.30, 0.15, 0.12), fit_valid = TRUE, gate_status = "ok"
  )
  s <- tibble::tibble(
    outcome = "asc_agg", term = c("zm_power", "zm_prestige"), estimate = c(0.19, -0.12),
    q2.5 = c(0.09, -0.20), q97.5 = c(0.29, -0.02), fit_valid = TRUE, gate_status = c("ok", "retried_ok"),
    nu_fixed = 4
  )
  cmp <- tabulate_likelihood_comparison(likelihood_pairs(g, s), g, s, plan)
  expect_equal(cmp$outcome, g$outcome)
  expect_equal(cmp$term, g$term)
  expect_equal(cmp$credible_gaussian, c(TRUE, FALSE, FALSE))
  expect_equal(cmp$credible_student, c(TRUE, TRUE, NA))
  expect_equal(cmp$fitted_student, c(TRUE, TRUE, FALSE))
  expect_equal(cmp$comparison_valid, c(TRUE, TRUE, FALSE))
  expect_equal(cmp$interval_exclusion_changes, c(FALSE, TRUE, NA))
  expect_equal(cmp$direction_changes, c(FALSE, TRUE, NA))
  expect_equal(attr(cmp, "ci_level"), plan$regression$ci_level)
  expect_equal(attr(cmp, "nu_fixed"), 4)
  # a refit that failed the gate is fitted but not interpretable
  s_bad <- s
  s_bad$gate_status <- "not_interpretable"
  bad <- tabulate_likelihood_comparison(likelihood_pairs(g, s_bad), g, s_bad, plan)
  expect_equal(bad$fitted_student, c(TRUE, TRUE, FALSE))
  expect_true(all(is.na(bad$credible_student)))
  expect_false(any(bad$comparison_valid))
})
