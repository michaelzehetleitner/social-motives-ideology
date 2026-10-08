# Connected execution of the joint-model route on a real CmdStan fit:
# define_joint_regression_model() -> fit_joint_regression()
# -> apply_validity_gate() -> extract_joint_posterior() -> the three RQ2
# comparison packages -> the RQ2 report target. Every number below comes from actual posterior
# draws; nothing here is stubbed.

local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) {
    parent <- dirname(dir)
    if (identical(parent, dir)) stop("project root not found from ", getwd())
    dir <- parent
  }
  # config.R and ap6_regressions.R as test-ap6-regressions.R sources them;
  # ap6_pipeline.R, ap10_inference.R, ap7_joint_comparisons.R,
  # report_results_joint.R and report_helpers.R for the AP7 packages, the RQ2
  # report target and its tables (test-report.R sources the report side the
  # same way).
  for (f in c("config.R", "ap6_regressions.R", "ap6_pipeline.R", "report_supplement_prior_sensitivity.R", "report_supplement_model_checks.R", "report_supplement_sampling_diagnostics.R", "report_supplement_explained_variance.R", "report_supplement_software.R", "report_results_associations.R", "ap10_inference.R",
              "report_results_network.R", "report_supplement_network_detail.R", "ap7_joint_comparisons.R", "report_results_joint.R",
              "report_helpers.R", "calculate_joint_asc_aggregation.R", "calculate_joint_correlation_changes.R")) {
    source(file.path(dir, "R", f), local = FALSE)
  }
})

root <- zm_root()
analysis_plan <- zm_config(profile = "smoke", path = file.path(root, "config", "analysis_plan.yaml"))
# the reduced sampling of test-ap6-regressions.R: 2 chains, and an ESS target
# the gate can reach with the short chains of a test
analysis_plan$regression$chains <- 2L
analysis_plan$regression$cores <- 2L
analysis_plan$regression$warmup <- 500L
analysis_plan$regression$iter_per_chain <- 500L
analysis_plan$regression$ess_target <- 100
reg <- analysis_plan$regression
outcomes <- as.character(reg$outcomes)
motives <- as.character(reg$motives)

# Stan executables of the tests go to the scratch directory, not the project
withr::local_options(list(cmdstanr_write_stan_file_dir = file.path(tempdir(), "zm_stan_tests")), .local_envir = teardown_env())
dir.create(file.path(tempdir(), "zm_stan_tests"), showWarnings = FALSE, recursive = TRUE)

cmdstan_ok <- tryCatch({
  zm_setup()
  nzchar(cmdstanr::cmdstan_version())
}, error = function(e) FALSE)

# The fit times, the Stan code hashes and the two compared medians are recorded
# in the test output. testthat muffles `message()` under every reporter, so the
# line is written to both streams: the condition is still signalled for a
# caller that handles it, and the text is visible in the run.
jr_note <- function(...) {
  text <- sprintf(...)
  message(text)
  cat(text, "\n", sep = "")
  invisible(text)
}

# ---- the fixture: 200 rows --------------------------------------------------

# identical to make_frame() of test-ap6-regressions.R
make_frame <- function(n = 200, seed = 42) {
  set.seed(seed)
  d <- data.frame(
    zm_security_z = rnorm(n), zm_achievement_z = rnorm(n), zm_power_z = rnorm(n),
    zm_prestige_z = rnorm(n), zm_arousal_z = rnorm(n), age_z = rnorm(n),
    gender = factor(sample(c("male", "female"), n, replace = TRUE), levels = c("male", "female")),
    income_z = rnorm(n)
  )
  female <- as.numeric(d$gender == "female")
  z <- function(x) as.numeric(scale(x))
  d$asc_agg_z <- z(0.30 * d$zm_power_z - 0.20 * d$zm_achievement_z + 0.25 * female + rnorm(n, sd = 0.9))
  d$asc_sub_z <- z(-0.20 * d$zm_arousal_z - 0.15 * d$zm_achievement_z + rnorm(n, sd = 0.95))
  d$asc_conv_z <- z(0.20 * d$zm_security_z - 0.20 * d$zm_arousal_z + 0.15 * d$age_z + rnorm(n, sd = 0.9))
  d$sdo_dom_z <- z(0.25 * d$zm_power_z + 0.15 * d$zm_prestige_z + 0.35 * female + rnorm(n, sd = 0.9))
  attr(d, "z_params") <- tibble::tibble(
    var = c("asc_agg", "asc_sub", "asc_conv", "sdo_dom"),
    z_col = paste0(c("asc_agg", "asc_sub", "asc_conv", "sdo_dom"), "_z"),
    mean = c(3.0, 2.9, 4.1, 2.5),
    sd = c(0.8, 0.8, 0.8, 0.8)
  )
  tibble::as_tibble(d) |> structure(z_params = attr(d, "z_params"))
}
d <- make_frame()
d_id <- tibble::add_column(d, respondent_id = seq_len(nrow(d)), .before = 1)

# `data_regressions` as the target graph hands it to the joint definition: the
# known-gender rows, the saved z columns and their constants (the shape of
# tr_regression_input() in test-targets-regressions.R), built from the rows
# of `d_id`.
jr_z_columns <- c("asc_agg_z", "asc_sub_z", "asc_conv_z", "sdo_dom_z",
                  "zm_security_z", "zm_achievement_z", "zm_power_z", "zm_prestige_z", "zm_arousal_z",
                  "age_z", "income_z")
data_regressions <- d_id[, c("respondent_id", "gender", jr_z_columns)]
attr(data_regressions, "regression_participants") <- data_regressions$respondent_id
attr(data_regressions, "z_parameters") <- tibble::tibble(
  var = sub("_z$", "", jr_z_columns), source = sub("_z$", "", jr_z_columns),
  transform = "identity", z_col = jr_z_columns,
  mean = 3, sd = 1.5, n = nrow(d_id)
)

# the joint model carries 36 population-level coefficients, 4 residual scales
# and 6 residual correlations: 1,000 post-warmup draws per chain are the
# smallest seed-deterministic fit whose R-hat clears the gate without a retry
cfg_mv <- analysis_plan
cfg_mv$regression$warmup <- 1000L
cfg_mv$regression$iter_per_chain <- 1000L

jr_responses <- c("ascaggz", "ascsubz", "ascconvz", "sdodomz")
jr_predictors <- c("Intercept", "zm_security_z", "zm_achievement_z", "zm_power_z", "zm_prestige_z",
                   "zm_arousal_z", "age_z", "genderfemale", "income_z")
jr_b_vars <- as.vector(outer(jr_responses, jr_predictors,
                             function(r, p) paste0("b_", r, "_", p)))
# the joint posterior names each motive by its key, in the codebook's order
jr_motive_names <- c("zm_security", "zm_arousal", "zm_power", "zm_prestige", "zm_achievement")
# the explicit map between the configured outcome keys and the AP7 display
# names of the residual-correlation columns
jr_outcome_key <- c(asc_agg = "aggression", asc_sub = "submission",
                    asc_conv = "conventionalism", sdo_dom = "sdo_d")

jr_direction_labels <- c("credible positive association", "credible negative association",
                         "unresolved in direction")
# the strength differences of RQ2a: |first| - |second| within each draw
jr_difference_labels <- c("credibly stronger for the first-named outcome",
                          "credibly weaker for the first-named outcome",
                          "unresolved strength difference")
jr_residual_labels <- c("greater average within ASC", "greater average ASC-SDO-D",
                        "unresolved ordering")

# ---- the connected run, computed once ---------------------------------------

joint_model <- NULL
fit <- NULL
gated <- NULL
joint_seconds <- NA_real_

if (cmdstan_ok) {
  joint_model <- define_joint_regression_model(
    data_regressions, define_regression_model_set(analysis_plan), analysis_plan
  )
  joint_seconds <- unname(system.time({
    fit <- fit_joint_regression(joint_model, cfg_mv)
  })[["elapsed"]])
  gated <- apply_validity_gate(list(joint = fit), cfg_mv)
  jr_note("[joint route] fit_joint_regression() elapsed: %.1f s", joint_seconds)
}

# ---- 1. the joint route reaches a gated, interpretable joint fit --------------

test_that("the joint route fits and passes the shared AP6 validity gate", {
  skip_if_not(cmdstan_ok, "CmdStan not available")
  jr_note("[joint route] fit time recorded in this test: %.1f s", joint_seconds)
  expect_s3_class(gated$joint, "brmsfit")
  expect_equal(gated$joint$backend, "cmdstanr")
  expect_true(attr(gated$joint, "gate_status") %in% c("ok", "retried_ok"))
  expect_true(attr(gated$joint, "diagnostics")$ok)

  prov <- attr(gated$joint, "provenance")
  expect_s3_class(prov, "tbl_df")
  expect_equal(nrow(prov), 1L)
  expect_named(prov, c("outcome", "slope_sd", "family", "formula", "nobs", "priors",
                       "stan_code_hash", "hash_algorithm", "chains", "warmup",
                       "iter_per_chain", "seed", "gate_status", "brms_version",
                       "cmdstanr_version", "cmdstan_version"))

  vars <- posterior::variables(gated$joint)
  expect_length(jr_b_vars, 36L)
  expect_true(all(jr_b_vars %in% vars))
  expect_setequal(grep("^b_", vars, value = TRUE), jr_b_vars)
  rescor_vars <- grep("^rescor__", vars, value = TRUE)
  expect_length(rescor_vars, 6L)
  pairs <- utils::combn(jr_responses, 2L)
  for (k in seq_len(ncol(pairs))) {
    candidates <- c(paste0("rescor__", pairs[1, k], "__", pairs[2, k]),
                    paste0("rescor__", pairs[2, k], "__", pairs[1, k]))
    expect_length(intersect(candidates, rescor_vars), 1L)
  }

  expect_equal(ap6_gate_resp(gated$joint), jr_responses)
  expect_equal(stats::nobs(gated$joint), 200L)
})

# ---- 2. the AP7 posterior extraction on those draws ------------------------

test_that("extract_joint_posterior() shapes the real draws without losing pairing", {
  skip_if_not(cmdstan_ok, "CmdStan not available")
  joint_posterior <- extract_joint_posterior(gated, analysis_plan)
  draws <- posterior::as_draws_df(gated$joint)
  n_draws <- posterior::ndraws(draws)

  expect_equal(nrow(joint_posterior$coefficients), 5L * n_draws)
  expect_identical(unique(joint_posterior$coefficients$motive), jr_motive_names)
  expect_named(joint_posterior$coefficients,
               c(".draw", "motive", "aggression", "submission", "conventionalism", "sdo_d"))

  expect_equal(nrow(joint_posterior$residual_correlations), n_draws)
  expect_named(joint_posterior$residual_correlations,
               c(".draw", "aggression_submission", "aggression_conventionalism",
                 "submission_conventionalism", "aggression_sdo_d", "submission_sdo_d",
                 "conventionalism_sdo_d"))

  expect_true(joint_posterior$validity$fit_valid)

  # the draw identifiers are the fit's own, per motive and for the residuals
  expect_identical(joint_posterior$residual_correlations$.draw, draws$.draw)
  for (motive in jr_motive_names) {
    rows <- joint_posterior$coefficients[joint_posterior$coefficients$motive == motive, ]
    expect_identical(rows$.draw, draws$.draw)
  }

  # one motive and one outcome, cell by cell against the fit's own draws
  power <- joint_posterior$coefficients[joint_posterior$coefficients$motive == "zm_power", ]
  expect_identical(power$sdo_d, draws$b_sdodomz_zm_power_z)
  expect_identical(power$aggression, draws$b_ascaggz_zm_power_z)
})

# ---- 3. the three RQ2 comparison packages ----------------------------------

test_that("the three comparison packages are built from the real joint posterior", {
  skip_if_not(cmdstan_ok, "CmdStan not available")
  joint_posterior <- extract_joint_posterior(gated, analysis_plan)
  level <- analysis_plan$regression$ci_level

  # the three target bodies of _targets.R, statement for statement
  overall_asc_vs_sdo <- {
    direction <- ap8_result_overall_asc_sdo_directions(joint_posterior$coefficients,
      interval_level = level)
    differences <- ap8_result_overall_asc_sdo_coefficient_differences(joint_posterior$coefficients,
      interval_level = level)
    residuals <- ap8_result_overall_asc_sdo_residual_difference(joint_posterior$residual_correlations,
      interval_level = level)
    withhold_classification_when_fit_invalid(
      list(direction = direction, differences = differences, residuals = residuals),
      joint_posterior$validity)
  }
  joint_within_asc <- {
    direction <- extract_joint_within_asc_directions(joint_posterior$coefficients,
      interval_level = level)
    differences <- extract_joint_within_asc_coefficient_differences(joint_posterior$coefficients,
      interval_level = level)
    residuals <- extract_joint_within_asc_residual_correlations(joint_posterior$residual_correlations,
      interval_level = level)
    withhold_classification_when_fit_invalid(
      list(direction = direction, differences = differences, residuals = residuals),
      joint_posterior$validity)
  }
  joint_asc_components_sdo <- {
    direction <- extract_joint_asc_components_sdo_directions(joint_posterior$coefficients,
      interval_level = level)
    differences <- extract_joint_asc_components_sdo_coefficient_differences(joint_posterior$coefficients,
      interval_level = level)
    residuals <- extract_joint_asc_components_sdo_residual_correlations(joint_posterior$residual_correlations,
      interval_level = level)
    withhold_classification_when_fit_invalid(
      list(direction = direction, differences = differences, residuals = residuals),
      joint_posterior$validity)
  }

  for (package in list(overall_asc_vs_sdo, joint_within_asc, joint_asc_components_sdo)) {
    expect_named(package, c("direction", "differences", "residuals", "validity"))
  }
  expect_equal(nrow(overall_asc_vs_sdo$direction), 10L)
  expect_equal(nrow(overall_asc_vs_sdo$differences), 5L)
  expect_equal(nrow(overall_asc_vs_sdo$residuals), 1L)
  expect_equal(nrow(joint_within_asc$direction), 15L)
  expect_equal(nrow(joint_within_asc$differences), 15L)
  expect_equal(nrow(joint_within_asc$residuals), 3L)
  expect_equal(nrow(joint_asc_components_sdo$direction), 20L)
  expect_equal(nrow(joint_asc_components_sdo$differences), 15L)
  expect_equal(nrow(joint_asc_components_sdo$residuals), 3L)

  allowed <- list(
    list(tbl = overall_asc_vs_sdo$direction, labels = jr_direction_labels),
    list(tbl = overall_asc_vs_sdo$differences, labels = jr_difference_labels),
    list(tbl = overall_asc_vs_sdo$residuals, labels = jr_residual_labels),
    list(tbl = joint_within_asc$direction, labels = jr_direction_labels),
    list(tbl = joint_within_asc$differences, labels = jr_difference_labels),
    list(tbl = joint_asc_components_sdo$direction, labels = jr_direction_labels),
    list(tbl = joint_asc_components_sdo$differences, labels = jr_difference_labels)
  )
  for (entry in allowed) {
    expect_false(any(is.na(entry$tbl$classification)))
    expect_true(all(entry$tbl$classification %in% entry$labels))
  }

  every_summary <- c(overall_asc_vs_sdo[c("direction", "differences", "residuals")],
                     joint_within_asc[c("direction", "differences", "residuals")],
                     joint_asc_components_sdo[c("direction", "differences", "residuals")])
  for (tbl in every_summary) {
    expect_true(all(tbl$lower <= tbl$posterior_median))
    expect_true(all(tbl$posterior_median <= tbl$upper))
  }

  # the arithmetic of the overall strength difference, recomputed by hand from
  # the draws: |SDO-D| minus |mean of the three signed ASC coefficients|
  for (motive in jr_motive_names) {
    coefficients <- joint_posterior$coefficients[joint_posterior$coefficients$motive == motive, ]
    hand <- stats::median(abs(coefficients$sdo_d) -
                            abs((coefficients$aggression + coefficients$submission +
                                   coefficients$conventionalism) / 3))
    reported <- overall_asc_vs_sdo$differences$posterior_median[
      overall_asc_vs_sdo$differences$motive == motive]
    expect_equal(length(reported), 1L)
    expect_equal(unname(reported), hand, tolerance = 1e-12)
  }
})

# ---- 4. the invalid-gate path on the same real fit -------------------------

test_that("an invalid gate withholds every classification and keeps every median", {
  skip_if_not(cmdstan_ok, "CmdStan not available")
  level <- analysis_plan$regression$ci_level
  build_packages <- function(joint_posterior) {
    list(
      overall = withhold_classification_when_fit_invalid(
        list(
          direction = ap8_result_overall_asc_sdo_directions(joint_posterior$coefficients, interval_level = level),
          differences = ap8_result_overall_asc_sdo_coefficient_differences(joint_posterior$coefficients, interval_level = level),
          residuals = ap8_result_overall_asc_sdo_residual_difference(joint_posterior$residual_correlations, interval_level = level)
        ),
        joint_posterior$validity),
      within = withhold_classification_when_fit_invalid(
        list(
          direction = extract_joint_within_asc_directions(joint_posterior$coefficients, interval_level = level),
          differences = extract_joint_within_asc_coefficient_differences(joint_posterior$coefficients, interval_level = level),
          residuals = extract_joint_within_asc_residual_correlations(joint_posterior$residual_correlations, interval_level = level)
        ),
        joint_posterior$validity),
      components = withhold_classification_when_fit_invalid(
        list(
          direction = extract_joint_asc_components_sdo_directions(joint_posterior$coefficients, interval_level = level),
          differences = extract_joint_asc_components_sdo_coefficient_differences(joint_posterior$coefficients, interval_level = level),
          residuals = extract_joint_asc_components_sdo_residual_correlations(joint_posterior$residual_correlations, interval_level = level)
        ),
        joint_posterior$validity)
    )
  }

  valid_packages <- build_packages(extract_joint_posterior(gated, analysis_plan))

  copy <- gated
  attr(copy$joint, "gate_status") <- "not_interpretable"
  attr(copy$joint, "diagnostics")$ok <- FALSE
  invalid_posterior <- extract_joint_posterior(copy, analysis_plan)
  expect_false(invalid_posterior$validity$fit_valid)
  invalid_packages <- build_packages(invalid_posterior)

  for (name in names(invalid_packages)) {
    for (part in c("direction", "differences", "residuals")) {
      invalid_tbl <- invalid_packages[[name]][[part]]
      valid_tbl <- valid_packages[[name]][[part]]
      if ("classification" %in% names(invalid_tbl)) {
        expect_true(all(is.na(invalid_tbl$classification)),
                    info = paste(name, part))
        expect_false(any(is.na(valid_tbl$classification)), info = paste(name, part))
      }
      expect_identical(invalid_tbl$posterior_median, valid_tbl$posterior_median,
                       info = paste(name, part))
    }
  }

  # the RQ2 report target on both
  valid_correlations <- calculate_joint_correlation_changes(gated,
    data_regressions = data_regressions)
  invalid_correlations <- calculate_joint_correlation_changes(copy,
    data_regressions = data_regressions)
  valid_report <- assemble_report_joint(valid_packages$within, valid_packages$components,
                                      valid_correlations, analysis_plan)
  invalid_report <- assemble_report_joint(invalid_packages$within, invalid_packages$components,
                                        invalid_correlations, analysis_plan)
  expect_true(valid_report$gate_passed)
  expect_false(invalid_report$gate_passed)
  expect_equal(nrow(invalid_report$credible_directions), 0L)
  expect_equal(nrow(invalid_report$credible_differences), 0L)
  expect_identical(invalid_report$differences$posterior_median, valid_report$differences$posterior_median)
})
