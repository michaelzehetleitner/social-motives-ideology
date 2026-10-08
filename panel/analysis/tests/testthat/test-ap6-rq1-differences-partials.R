# AP6 Coefficient Differences and Partial Correlations of the Motives (RQ1):
# the pairs follow the predicted-signs table, draws are summarised by AP10,
# the partial correlations follow the inverse of the correlation matrix, and
# the validity gate covers the residual correlations of the motive model only.

local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) dir <- dirname(dir)
  for (f in c("config.R", "ap10_inference.R", "ap6_regressions.R", "ap6_rq1_differences_partials.R")) {
    source(file.path(dir, "R", f), local = FALSE)
  }
  assign("rq1dp_root", dir, envir = .GlobalEnv)
})

rq1dp_plan <- zm_config("smoke", file.path(rq1dp_root, "config", "analysis_plan.yaml"))

test_that("the pairs are the motives with opposite predicted signs, 15 in all", {
  pairs <- ap6_list_predicted_difference_pairs(rq1dp_plan)
  expect_equal(nrow(pairs), 15L)
  counts <- table(pairs$outcome_key)
  expect_equal(as.integer(counts[c("asc_agg", "asc_sub", "asc_conv", "sdo_dom")]), c(4L, 3L, 4L, 4L))
  table <- rq1dp_plan$predictions$table
  for (i in seq_len(nrow(pairs))) {
    expect_identical(table[[pairs$outcome_key[i]]][[pairs$motive_positive[i]]], "+")
    expect_identical(table[[pairs$outcome_key[i]]][[pairs$motive_negative[i]]], "-")
  }
  expect_false(any(c(pairs$motive_positive, pairs$motive_negative) == "zm_prestige"))
})

test_that("a quantity is credible when its central interval excludes zero", {
  set.seed(1)
  above <- ap6_summarise_quantity_draws(stats::rnorm(4000, 0.3, 0.05), 0.95)
  expect_true(above$credible)
  expect_identical(above$sign, "positive")
  around <- ap6_summarise_quantity_draws(stats::rnorm(4000, 0.02, 0.1), 0.95)
  expect_false(around$credible)
  expect_lt(around$lower, 0)
  expect_gt(around$upper, 0)
})

test_that("partial correlations follow the inverse of the correlation matrix", {
  R <- matrix(c(1, .5, .3, .5, 1, .4, .3, .4, 1), 3)
  P <- ap6_convert_correlations_to_partial(R)
  expect_equal(P[1, 2], (.5 - .3 * .4) / sqrt((1 - .3^2) * (1 - .4^2)), tolerance = 1e-12)
  expect_equal(P, t(P))
  expect_equal(unname(diag(P)), rep(1, 3))
})

test_that("the validity gate covers residual correlations of the motive model only", {
  vars <- c("b_zmsecurityz_Intercept", "b_zmarousalz_age_z", "rescor__zmsecurityz__zmarousalz", "sigma_zmsecurityz")
  motive <- brms::bf(zm_security_z ~ age_z) + brms::bf(zm_arousal_z ~ age_z) + brms::set_rescor(TRUE)
  expect_identical(ap6_gated_rescor_vars(motive, vars), "rescor__zmsecurityz__zmarousalz")
  facets <- brms::bf(asc_agg_z ~ zm_security_z) + brms::bf(sdo_dom_z ~ zm_security_z) + brms::set_rescor(TRUE)
  expect_identical(ap6_gated_rescor_vars(facets, c("rescor__ascaggz__sdodomz")), character(0))
  expect_identical(ap6_gated_rescor_vars(brms::bf(asc_agg_z ~ zm_security_z), vars), character(0))
})

test_that("the pipeline builds the four RQ1 steps", {
  targets_text <- paste(readLines(file.path(rq1dp_root, "_targets.R"), warn = FALSE), collapse = "\n")
  expect_match(targets_text, "calculate_coefficient_differences(regression_primary_fits", fixed = TRUE)
  expect_match(targets_text, "define_motive_model(data_regressions, regression_model_set", fixed = TRUE)
  expect_match(targets_text, "apply_validity_gate(list(motives = fit)", fixed = TRUE)
  expect_match(targets_text, "calculate_motive_partial_correlations(motive_model_fit$motives, motive_model", fixed = TRUE)
})
