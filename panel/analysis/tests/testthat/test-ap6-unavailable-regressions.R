# AP3 can leave one or every model without usable cases. These checks use a
# stand-in sampler and retained draws; no regression is fitted.
local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) {
    parent <- dirname(dir)
    if (identical(parent, dir)) stop("project root not found")
    dir <- parent
  }
  unavailable_test_root <<- dir
})
unavailable_env <- new.env(parent = globalenv())
for (file in c("config.R", "classify_importance_sampling.R", "ap3_imputation_validity.R",
               "ap6_regressions.R", "ap6_interval_comparison.R", "ap6_pipeline.R", "ap6_rq1_differences_partials.R",
               "ap10_inference.R", "ap7_joint_comparisons.R", "calculate_joint_correlation_changes.R",
               "calculate_joint_asc_aggregation.R", "report_results_associations.R", "report_helpers.R",
               "report_joint_additions.R", "report_supplement_model_checks.R",
               "report_supplement_prior_sensitivity.R", "report_supplement_explained_variance.R")) {
  sys.source(file.path(unavailable_test_root, "R", file), envir = unavailable_env)
}
source(file.path(unavailable_test_root, "tests", "support", "fake-brmsfit.R"), local = FALSE)
fb_register_methods()
unavailable_plan <- unavailable_env$zm_config("smoke", file.path(unavailable_test_root, "config", "analysis_plan.yaml"))
unavailable_models <- unavailable_env$define_regression_model_set(unavailable_plan)
unavailable_book <- unavailable_env$zm_codebook(unavailable_plan)

prepare_unavailable_regression_fixture <- function(all_unavailable = FALSE) {
  n <- 12L
  columns <- unique(unlist(lapply(unavailable_models$formulas, all.vars)))
  data <- tibble::tibble(respondent_id = seq_len(n),
                        gender = factor(rep(c("female", "male"), 6L), levels = c("female", "male")))
  for (column in setdiff(columns, "gender")) data[[column]] <- ((seq_len(n) + match(column, columns)) %% 5) - 2
  z_columns <- setdiff(columns, "gender")
  attr(data, "z_parameters") <- tibble::tibble(var = sub("_z$", "", z_columns), source = sub("_z$", "", z_columns),
    transform = "identity", z_col = z_columns, mean = 3, sd = 1.5, n = n)
  empty <- data[FALSE, , drop = FALSE]
  attr(empty, "excluded_imputation") <- tibble::tibble(respondent_id = seq_len(n), variables = "asc_agg")
  inputs <- lapply(seq_along(unavailable_models$formulas), function(i) {
    list(variables = all.vars(unavailable_models$formulas[[i]]),
         data = if (all_unavailable || i == 1L) empty else data)
  })
  motive_columns <- names(unavailable_plan$regression$term_keys)[
    unavailable_plan$regression$term_keys %in% unavailable_plan$regression$motives]
  inputs$motives <- list(variables = c(motive_columns, "age_z", "gender", "income_z"),
                        data = if (all_unavailable) empty else data)
  attr(empty, "regression_model_inputs") <- inputs
  empty
}

run_unavailable_regression_fixture <- function(all_unavailable = FALSE) {
  data <- prepare_unavailable_regression_fixture(all_unavailable)
  calls <- list()
  testthat::local_mocked_bindings(brm = function(formula, data, family = "gaussian", sample_prior = "no", ...) {
    calls[[length(calls) + 1L]] <<- list(response = all.vars(formula)[[1L]], sample_prior = sample_prior, n = nrow(data))
    if (!nrow(data)) stop("An unavailable model reached the sampler")
    fit <- fb_fake_brmsfit(formula, data, family = family, sigma = 2, noise = "heavy",
                          chains = 2L, draws_per_chain = 64L, seed = 731L)
    attr(fit, "seed") <- 731L
    attr(fit, "outcome") <- names(unavailable_models$formulas)[
      match(all.vars(formula)[[1L]], vapply(unavailable_models$formulas, function(f) all.vars(f)[[1L]], character(1)))]
    attr(fit, "gate_status") <- "ok"
    diagnostics <- unavailable_env$zm_regression_unavailable_diagnostics(unavailable_plan)
    diagnostics$ok <- diagnostics$ess_ok <- diagnostics$rhat_ok <- diagnostics$divergences_ok <-
      diagnostics$treedepth_ok <- diagnostics$bfmi_ok <- TRUE
    attr(fit, "diagnostics") <- diagnostics
    attr(fit, "n_obs") <- nrow(data)
    fit
  }, .package = "brms")
  rlang::local_bindings(ap6_run_priorsense = fb_priorsense, .env = unavailable_env)
  primary <- unavailable_env$fit_primary_regressions(data, unavailable_models, unavailable_plan)
  widths <- unavailable_env$fit_prior_width_comparisons(data, unavailable_models, unavailable_plan)
  predictive <- list(prior = unavailable_env$check_prior_predictions(data, unavailable_models, unavailable_plan),
                     posterior = unavailable_env$check_posterior_predictions(primary, unavailable_plan))
  student <- unavailable_env$fit_student_t_if_needed(data, unavailable_models, primary, predictive, unavailable_plan)
  register <- unavailable_env$extract_regression_result_records(primary, widths, student, unavailable_plan)
  coefs <- unavailable_env$extract_regression_coefficient_summaries(primary, widths, student, unavailable_plan)
  r2 <- unavailable_env$extract_regression_r2_summaries(primary, widths, student, unavailable_plan)
  likelihood <- unavailable_env$extract_regression_likelihood_robustness(coefs, r2)
  stability <- unavailable_env$extract_regression_prior_width_sensitivity(primary, widths, unavailable_plan)
  powerscale <- unavailable_env$extract_regression_prior_sensitivity(primary, unavailable_plan)
  prior_supplement <- unavailable_env$assemble_supplement_prior_sensitivity(coefs, register, powerscale,
    stability, data, unavailable_book, unavailable_plan)
  primary_table <- unavailable_env$zm_primary_coefs(
    unavailable_env$tabulate_regression_coefficients(coefs, register, unavailable_plan, roles = "primary"))
  checks_supplement <- unavailable_env$assemble_supplement_model_checks(predictive, register, coefs,
    primary_table, likelihood, unavailable_plan)
  decisions <- unavailable_env$extract_regression_prediction_decisions(coefs, unavailable_plan)
  list(data = data, primary = primary, widths = widths, predictive = predictive, student = student,
       register = register, coefs = coefs, r2 = r2, stability = stability,
       prior_supplement = prior_supplement, primary_table = primary_table,
       checks_supplement = checks_supplement, decisions = decisions, calls = calls)
}

test_that("one excluded outcome keeps its reason while unaffected regression results continue", {
  result <- run_unavailable_regression_fixture()
  missing_outcome <- names(unavailable_models$formulas)[[1L]]
  missing_key <- unavailable_models$outcome_keys[[missing_outcome]]
  expect_s3_class(result$primary[[missing_outcome]], "imputation_unavailable")
  expect_true(all(vapply(result$primary[-1L], inherits, logical(1), "brmsfit")))
  expect_false(any(vapply(result$calls, function(call) identical(call$response, "asc_agg_z"), logical(1))))
  prior <- result$checks_supplement$prior_predictive
  expect_equal(sum(prior$outcome == missing_key), 4L)
  expect_true(all(is.na(prior$q50[prior$outcome == missing_key])))
  expect_true(all(is.finite(prior$q50[prior$outcome != missing_key])))
  pp <- result$checks_supplement$posterior_predictive
  expect_equal(sum(pp$outcome == missing_key), 6L)
  expect_true(all(is.na(pp$observed[pp$outcome == missing_key])))
  expect_true(all(is.finite(pp$observed[pp$outcome != missing_key])))
  expect_match(unique(pp$note[pp$outcome == missing_key]), "no respondents remain")
  expect_false(missing_key %in% result$checks_supplement$predictive_draws$outcome)
  expect_true(nrow(result$checks_supplement$predictive_draws) > 0L)
  expect_true(is.na(attr(result$student, "heavy_tail_triggers")[[missing_outcome]]))
  expect_match(conditionMessage(result$student[[missing_outcome]]), "no respondents remain")
  sensitivity <- result$prior_supplement$sensitivity
  expect_equal(sum(sensitivity$outcome == missing_key), 5L)
  expect_true(all(is.na(sensitivity$interval_exclusion_changes[sensitivity$outcome == missing_key])))
  expect_true(all(sensitivity$comparison_valid[sensitivity$outcome != missing_key]))
  expect_match(unique(sensitivity$note[sensitivity$outcome == missing_key]), "no respondents remain")
  powerscale <- result$prior_supplement$priorsense
  expect_false(any(powerscale$interpretable[powerscale$outcome == missing_key]))
  expect_true(all(powerscale$interpretable[powerscale$outcome != missing_key]))
  expect_match(unique(powerscale$note[powerscale$outcome == missing_key]), "no respondents remain")
  expect_true(all(is.na(result$decisions$verdict[result$decisions$outcome_key == missing_key])))
  expect_true(all(result$decisions$fit_valid[result$decisions$outcome_key != missing_key]))
})

test_that("every excluded outcome retains schemas, unavailable answers and visible reasons", {
  result <- run_unavailable_regression_fixture(TRUE)
  expect_length(result$calls, 0L)
  expect_equal(nrow(result$decisions), 20L)
  expect_true(all(is.na(result$decisions$verdict)))
  expect_true(all(is.na(result$decisions$estimate)))
  expect_equal(nrow(result$prior_supplement$sensitivity), 20L)
  expect_false(any(result$prior_supplement$sensitivity$comparison_valid))
  expect_false(any(result$prior_supplement$priorsense$interpretable))
  expect_equal(result$checks_supplement$answer$n_pp_unavailable, 24L)
  expect_equal(nrow(result$checks_supplement$predictive_draws), 0L)
  expect_equal(nrow(attr(result$checks_supplement$predictive_draws, "unavailable_predictions")), 4L)
  expect_true(all(is.na(result$checks_supplement$tails$heavy_tails)))
  expect_match(unavailable_env$rh_tail_text(result$checks_supplement$tails,
    result$checks_supplement$posterior_predictive, result$checks_supplement$student_coefs,
    unavailable_plan), "could not be evaluated")
  pp_plot <- unavailable_env$rh_pp_plot(result$checks_supplement$predictive_draws)
  coefficient_plot <- unavailable_env$rh_coef_plot(result$primary_table, analysis_plan = unavailable_plan)
  expect_s3_class(pp_plot, "ggplot")
  expect_s3_class(coefficient_plot, "ggplot")
  expect_match(pp_plot$layers[[1L]]$aes_params$label, "no respondents remain")
  expect_match(coefficient_plot$layers[[1L]]$aes_params$label, "no respondents remain")
  prior_table <- as.character(unavailable_env$rh_prior_pred_table(result$checks_supplement$prior_predictive,
    engine = "kable", analysis_plan = unavailable_plan))
  posterior_table <- as.character(unavailable_env$rh_pp_stats_table(result$checks_supplement$posterior_predictive,
    engine = "kable", analysis_plan = unavailable_plan))
  # Each table displays four comparisons for each of the four outcomes.
  expect_equal(sum(grepl("no respondents remain", prior_table, fixed = TRUE)), 16L)
  expect_equal(sum(grepl("no respondents remain", posterior_table, fixed = TRUE)), 16L)
  explained <- unavailable_env$assemble_supplement_explained_variance(result$r2, unavailable_plan)
  expect_setequal(explained$missing, unavailable_plan$regression$outcomes)
})

test_that("empty joint and motive samples stop at their declared AP3 boundaries", {
  data <- prepare_unavailable_regression_fixture(TRUE)
  testthat::local_mocked_bindings(brm = function(...) stop("Unavailable joint model reached the sampler"), .package = "brms")
  joint <- unavailable_env$define_joint_regression_model(data, unavailable_models, unavailable_plan)
  fit <- unavailable_env$fit_joint_regression(joint, unavailable_plan)
  expect_s3_class(fit, "imputation_unavailable")
  posterior <- unavailable_env$extract_joint_posterior(list(joint = fit), unavailable_plan)
  expect_false(posterior$validity$fit_valid)
  expect_match(posterior$validity$note, "no respondents remain")
  changes <- unavailable_env$calculate_joint_correlation_changes(list(joint = fit), data_regressions = data)
  expect_equal(nrow(changes$summaries), 7L)
  expect_false(any(changes$summaries$interpretable))
  expect_match(unavailable_env$note_joint_addition_validity(changes), "no respondents remain")
  motive <- unavailable_env$define_motive_model(data, unavailable_models, unavailable_plan)
  motive_fit <- unavailable_env$fit_motive_model(motive, unavailable_plan)
  expect_s3_class(motive_fit, "imputation_unavailable")
  partials <- unavailable_env$calculate_motive_partial_correlations(motive_fit, motive, unavailable_plan)
  expect_equal(nrow(partials), 10L)
  expect_true(all(is.na(partials$credible)))
  expect_match(unique(partials$note), "no respondents remain")
})

test_that("ordinary programming errors propagate across AP3 availability boundaries", {
  expect_error(unavailable_env$ap6_capture_imputation_unavailability(stop("invalid formula")), "invalid formula")
  rlang::local_bindings(require_valid_imputation_inputs = function(...) stop("missing model variable"),
                        .env = unavailable_env)
  data <- prepare_unavailable_regression_fixture(TRUE)
  expect_error(unavailable_env$check_prior_predictions(data, unavailable_models, unavailable_plan), "missing model variable")
  expect_error(unavailable_env$define_joint_regression_model(data, unavailable_models, unavailable_plan), "missing model variable")
  expect_error(unavailable_env$define_motive_model(data, unavailable_models, unavailable_plan), "missing model variable")
})
