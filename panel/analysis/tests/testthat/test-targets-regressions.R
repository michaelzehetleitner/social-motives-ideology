# The AP6 single-outcome regression slice: how the model set, fits, gate,
# checks and results are wired into the target graph, and what the report
# tables built from them carry.
#
# No test here fits a model: every fit is a stub carrying the attributes the
# validity gate records, and the fitting verbs are exercised against a stubbed
# `brms::brm()`.

local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) {
    parent <- dirname(dir)
    if (identical(parent, dir)) stop("project root not found from ", getwd())
    dir <- parent
  }
  for (f in c("config.R", "ap6_regressions.R", "ap6_pipeline.R", "report_supplement_prior_sensitivity.R", "report_supplement_model_checks.R", "report_supplement_sampling_diagnostics.R", "report_supplement_explained_variance.R", "report_supplement_software.R", "report_results_associations.R", "ap10_inference.R")) {
    source(file.path(dir, "R", f), local = FALSE)
  }
})

tr_root <- normalizePath(getwd())
while (!file.exists(file.path(tr_root, "config", "analysis_plan.yaml"))) tr_root <- dirname(tr_root)
tr_cfg <- zm_config()
tr_codebook <- zm_codebook(tr_cfg)

tr_manifest <- withr::with_envvar(
  c(ZM_PROFILE = "smoke", ZM_DATA = "synthetic"),
  withr::with_dir(tr_root, targets::tar_manifest(callr_function = NULL))
)
tr_command <- function(name) {
  command <- tr_manifest$command[tr_manifest$name == name]
  if (length(command) != 1L) stop("target '", name, "' not found exactly once in the manifest")
  gsub("\\s+", " ", command)
}

tr_outcomes <- c("authoritarian_aggression", "authoritarian_submission",
                 "conventionalism", "sdo_d_dominance")
tr_keys <- c("asc_agg", "asc_sub", "asc_conv", "sdo_dom")
tr_variables <- c("b_Intercept", "b_zm_security_z", "b_zm_achievement_z", "b_zm_power_z",
                  "b_zm_prestige_z", "b_zm_arousal_z", "b_age_z", "b_genderfemale",
                  "b_income_z", "sigma")

# --- fixtures: a gated fit and the collections the fitting targets return ----

tr_diagnostics <- function(ok = TRUE) {
  tibble::tibble(
    ok = ok, ess_ok = ok, rhat_ok = ok, divergences_ok = TRUE, treedepth_ok = TRUE,
    bfmi_ok = TRUE, ess_bulk_min = if (ok) 12000 else 400, ess_tail_min = if (ok) 11000 else 300,
    rhat_max = if (ok) 1.001 else 1.2, n_divergent = 0L, n_treedepth_hits = 0L,
    bfmi_min = 0.9, mcse_median_max = 0.002, mcse_q_max = 0.004,
    ess_target = 10000, rhat_limit = 1.01, divergences_max = 0, treedepth_hits_max = 0,
    bfmi_limit = 0.2, max_treedepth_used = 10L,
    gate_note = if (ok) "ok" else "ESS shortfall"
  )
}

tr_provenance <- function(gate_status) {
  tibble::tibble(
    outcome = NA_character_, slope_sd = NA_real_, family = "gaussian",
    formula = "asc_agg_z ~ zm_security_z", nobs = 600L, priors = "b: normal(0, 0.2)",
    stan_code_hash = "0f", hash_algorithm = "sha256", chains = 4L, warmup = 500L,
    iter_per_chain = 1000L, seed = 20260905L, gate_status = gate_status,
    brms_version = "2.22.0", cmdstanr_version = "0.8.1", cmdstan_version = "2.36.0"
  )
}

tr_fit <- function(gate_status = "ok", ok = TRUE, retries = 0L, provenance = TRUE) {
  fit <- structure(list(), class = "brmsfit")
  attr(fit, "diagnostics") <- tr_diagnostics(ok)
  attr(fit, "gate_status") <- gate_status
  attr(fit, "iter_used") <- 1000L
  attr(fit, "warmup_used") <- 500L
  attr(fit, "n_obs") <- 600L
  attr(fit, "ess_log") <- tibble::tibble(
    attempt = 1L, iter_per_chain = 1000L, min_ess_bulk = 12000, min_ess_tail = 11000, ok = ok)
  attr(fit, "retry_log") <- tibble::tibble(
    step = rep(1L, retries), reason = rep("ESS shortfall: iterations doubled", retries),
    warmup = rep(500L, retries), iter_per_chain = rep(2000L, retries),
    adapt_delta = rep(0.99, retries), max_treedepth = rep(15L, retries), ok = rep(TRUE, retries))
  if (provenance) attr(fit, "provenance") <- tr_provenance(gate_status)
  fit
}

# One primary fit per outcome; the second is retried, the third failed its gate.
tr_primary <- function() {
  stats::setNames(list(tr_fit(), tr_fit("retried_ok", retries = 1L),
                       tr_fit("not_interpretable", ok = FALSE), tr_fit()), tr_outcomes)
}

tr_comparison <- function() {
  list(
    "0.10" = stats::setNames(lapply(tr_outcomes, function(o) tr_fit()), tr_outcomes),
    "0.40" = stats::setNames(lapply(tr_outcomes, function(o) tr_fit()), tr_outcomes)
  )
}

# The first outcome triggered its Student refit; the other three did not.
tr_student <- function() {
  fits <- stats::setNames(rep(list(NULL), length(tr_outcomes)), tr_outcomes)
  fits[[1]] <- list(fit = tr_fit())
  attr(fits, "heavy_tail_triggers") <- stats::setNames(
    c(TRUE, FALSE, FALSE, FALSE), tr_outcomes)
  fits
}

tr_register <- function() {
  extract_regression_result_records(tr_primary(), tr_comparison(), tr_student(), tr_cfg)
}

# Distinguishable coefficient values per outcome and term, so that a swapped
# outcome identity cannot pass a test that only counts matching cells.
tr_estimate <- function(outcome_key, variable) {
  (match(outcome_key, tr_keys) * 100 + match(variable, tr_variables)) / 1000
}

tr_coefficients <- function(record) {
  estimate <- vapply(tr_variables, function(v) tr_estimate(record$outcome_key, v), numeric(1),
                     USE.NAMES = FALSE)
  out <- tibble::tibble(
    variable = tr_variables, estimate = estimate, sd = 0.05,
    q2.5 = estimate - 0.01, q97.5 = estimate + 0.01,
    p_positive = 0.99, p_negative = 0.01
  )
  out
}

tr_precision <- function(variables) {
  tibble::tibble(
    variable = variables, ess_bulk = 12000, ess_tail = 11000, rhat = 1.001,
    mcse_median = 0.002, mcse_q_lo = 0.003, mcse_q_hi = 0.004
  )
}

tr_hdi <- function(variables) {
  coefficients <- setdiff(variables, "sigma")
  tibble::tibble(variable = coefficients, hdi_lo = -0.5, hdi_hi = 0.5)
}

tr_summaries <- function(register) {
  evidence <- lapply(register, function(record) {
    if (!record$fit_available) return(ap6_unavailable_regression_summary(record))
    coefficients <- tr_coefficients(record)
    ap6_store_regression_coefficient_evidence(
      ap6_pack_regression_coefficient_summary(
        coefficients, tr_precision(coefficients$variable), record, tr_cfg),
      tr_hdi(coefficients$variable)
    )
  })
  ap6_bind_regression_coefficient_evidence(evidence)
}

tr_r2 <- function(register) {
  dplyr::bind_rows(lapply(register, function(record) {
    if (!record$fit_available) return(ap6_unavailable_regression_summary(record))
    ap6_attach_regression_result_context(
      tibble::tibble(r2_median = 0.3, r2_lo = 0.2, r2_hi = 0.4), record)
  }))
}

tr_posterior_leaf <- function(kurtosis_observed = 0.1, with_draws = TRUE) {
  statistic <- function(observed, lo, hi) {
    list(statistic = "x", observed = observed,
         replicated_interval = c(`2.5%` = lo, `97.5%` = hi), replicated_median = (lo + hi) / 2,
         interval_membership = if (observed >= lo && observed <= hi) "inside" else "outside",
         posterior_predictive_p_value = 0.4)
  }
  checks <- list(
    mean = statistic(0, -0.1, 0.1), sd = statistic(1, 0.9, 1.1),
    skewness = statistic(0.05, -0.2, 0.2),
    excess_kurtosis = statistic(kurtosis_observed, -0.2, 0.2),
    min = statistic(-2.5, -3, -2), max = statistic(2.5, 2, 3)
  )
  for (name in names(checks)) checks[[name]]$statistic <- name
  attr(checks, "slope_sd") <- 0.2
  if (with_draws) {
    attr(checks, "predictive_draws") <- tibble::tibble(
      outcome = "placeholder", type = c("y", "y_rep"), draw = c(NA_integer_, 1L),
      obs = c(1L, 1L), value = c(0.5, 0.4))
  }
  checks
}

tr_prior_leaf <- function(outcome, family, slope_sd) {
  list(
    outcome = outcome, response_column = "asc_agg_z",
    fixed_z_constants = tibble::tibble(var = "asc_agg", z_col = "asc_agg_z", mean = 3, sd = 1),
    family = family, slope_sd = slope_sd,
    standardised_predictions = matrix(0, nrow = 2, ncol = 3),
    raw_scale_predictions = matrix(3, nrow = 2, ncol = 3),
    standardised_summary = list(n_draws = 2L, n_observations = 3L, mean = 0, sd = 1,
                                p95_absolute = 1.9),
    raw_scale_summary = list(
      raw_mean = 3, raw_sd = 1, raw_quantiles = c(`5%` = 1.5, `50%` = 3, `95%` = 4.5),
      raw_range = c(1, 5), response_range = c(min = 1, max = 6),
      share_below_min = 0, share_above_max = 0, share_outside_range = 0)
  )
}

tr_predictive_checks <- function(with_draws = TRUE) {
  list(
    prior = lapply(tr_outcomes, function(outcome) {
      list(tr_prior_leaf(outcome, "gaussian", 0.1), tr_prior_leaf(outcome, "gaussian", 0.2),
           tr_prior_leaf(outcome, "gaussian", 0.4), tr_prior_leaf(outcome, "student", 0.2))
    }),
    posterior = stats::setNames(
      lapply(tr_outcomes, function(o) tr_posterior_leaf(with_draws = with_draws)), tr_outcomes)
  )
}

tr_prior_sensitivity <- function(with_matrices = TRUE) {
  block <- function(name) {
    list(block = name, prior_selection = if (name == "all_priors") "all" else name,
         matrix = tibble::tibble(
           variable = c("b_zm_security_z", "bayes_R2"), prior = c(0.02, 0.06),
           likelihood = c(0.01, 0.03), diagnosis = c("-", "prior-data conflict")),
         sensitivity = 0.06, pareto_k_max = 0.4, importance_sampling_draws = 4000L,
         importance_sampling_status = "reliable")
  }
  blocks <- stats::setNames(lapply(tr_cfg$sensitivity$powerscale$blocks, block),
                            tr_cfg$sensitivity$powerscale$blocks)
  sensitivity <- list(
    variables = "b_zm_security_z",
    settings = list(components = c("prior", "likelihood"), lower_alpha = 0.99,
                    upper_alpha = 1.01, div_measure = "cjs_dist", sensitivity_threshold = 0.05),
    priorsense_version = "1.1.0", blocks = blocks)
  summary <- list(diagnostic_threshold = 0.05,
                  fit_valid = TRUE, fit_gate_status = "ok",
                  blocks = lapply(blocks, function(b) list(interpretable = TRUE)))
  if (with_matrices) summary$sensitivity <- sensitivity
  stats::setNames(lapply(tr_outcomes, function(o) summary), tr_outcomes)
}

# --- the wiring of the regression targets ------------------------------------

test_that("the regression targets call the AP6 verbs on the regression table", {
  expect_match(tr_command("regression_model_set"),
               "define_regression_model_set(analysis_inputs$config)", fixed = TRUE)
  # The four fitting calls pass the regression table itself.
  for (name in c("regression_primary_fits", "regression_prior_width_fits",
                 "regression_predictive_checks", "regression_student_t_robustness")) {
    expect_match(tr_command(name), "data_regressions,", fixed = TRUE, info = name)
  }
  expect_match(tr_command("regression_primary_fits"), "fit_primary_regressions(", fixed = TRUE)
  expect_match(tr_command("regression_prior_width_fits"), "fit_prior_width_comparisons(", fixed = TRUE)
  expect_match(tr_command("regression_predictive_checks"), "check_prior_predictions(", fixed = TRUE)
  expect_match(tr_command("regression_predictive_checks"), "check_posterior_predictions(", fixed = TRUE)
  expect_match(tr_command("regression_student_t_robustness"), "fit_student_t_if_needed(", fixed = TRUE)
  # Every fitting target passes its collection through the one shared gate.
  for (name in c("regression_primary_fits", "regression_prior_width_fits",
                 "regression_student_t_robustness")) {
    expect_match(tr_command(name), "apply_validity_gate(fits, analysis_inputs$config)",
                 fixed = TRUE, info = name)
  }
  expect_match(tr_command("regression_fit_register"), "extract_regression_result_records(", fixed = TRUE)
  expect_match(tr_command("regression_coefficient_summaries"),
               "extract_regression_coefficient_summaries(", fixed = TRUE)
  expect_match(tr_command("regression_r2_summaries"), "extract_regression_r2_summaries(", fixed = TRUE)
  expect_match(tr_command("regression_prior_width_sensitivity"),
               "extract_regression_prior_width_sensitivity(", fixed = TRUE)
  expect_match(tr_command("regression_prior_sensitivity"),
               "extract_regression_prior_sensitivity(regression_primary_fits, analysis_inputs$config)",
               fixed = TRUE)
  expect_match(tr_command("regression_likelihood_robustness"),
               "extract_regression_likelihood_robustness(regression_coefficient_summaries, regression_r2_summaries)",
               fixed = TRUE)
})

test_that("the regression report tables read the AP6 results only", {
  expect_match(tr_command("report_primary_coefficients"),
               "zm_primary_coefs(tabulate_regression_coefficients(regression_coefficient_summaries, regression_fit_register",
               fixed = TRUE)
  expect_match(tr_command("supplement_explained_variance"),
               "assemble_supplement_explained_variance(regression_r2_summaries, analysis_plan)", fixed = TRUE)
  expect_match(tr_command("supplement_sampling_diagnostics"),
               "assemble_supplement_sampling_diagnostics(regression_fit_register, analysis_plan)", fixed = TRUE)
  expect_match(tr_command("supplement_model_checks"),
               "assemble_supplement_model_checks(regression_predictive_checks, regression_fit_register,",
               fixed = TRUE)
  expect_match(tr_command("supplement_prior_sensitivity"),
               paste("assemble_supplement_prior_sensitivity(regression_coefficient_summaries,",
                     "regression_fit_register, regression_prior_sensitivity, regression_prior_width_sensitivity,",
                     "data_regressions, codebook, analysis_plan)"),
               fixed = TRUE)
  expect_match(tr_command("supplement_software"),
               "assemble_supplement_software(regression_fit_register, joint_regression_fit, analysis_plan)",
               fixed = TRUE)
  # No projection fits, refits or predicts again.
  for (name in c("report_primary_coefficients", "supplement_explained_variance", "supplement_sampling_diagnostics",
                 "supplement_model_checks",
                 "supplement_prior_sensitivity", "supplement_software")) {
    for (forbidden in c("brms::", "brms::posterior_predict", "priorsense::", "ap6_fit_")) {
      expect_false(grepl(forbidden, tr_command(name), fixed = TRUE), info = paste(name, forbidden))
    }
  }
})

# --- stable outcome identity -------------------------------------------------

test_that("every model name resolves to its configured key and response column", {
  model_set <- define_regression_model_set(tr_cfg)
  expect_identical(
    model_set$outcome_keys,
    c(authoritarian_aggression = "asc_agg", authoritarian_submission = "asc_sub",
      conventionalism = "asc_conv", sdo_d_dominance = "sdo_dom")
  )
  identity <- ap6_regression_outcome_identity(tr_cfg)
  expect_identical(identity$outcome, tr_outcomes)
  expect_identical(identity$outcome_key, tr_keys)
  expect_identical(identity$response_column,
                   c("asc_agg_z", "asc_sub_z", "asc_conv_z", "sdo_dom_z"))
  expect_identical(identity$response_column, unname(zm_z_col(identity$outcome_key, tr_codebook)))
})

test_that("a duplicate or unknown outcome mapping is refused", {
  duplicated <- list(a = asc_agg_z ~ zm_security_z, b = asc_agg_z ~ zm_security_z)
  expect_error(ap6_regression_outcome_keys(duplicated, tr_cfg), "one to one")
  unknown <- list(a = not_an_outcome_z ~ zm_security_z)
  expect_error(ap6_regression_outcome_keys(unknown, tr_cfg), "no prediction key")
  not_registered <- list(a = zm_security_z ~ age_z)
  expect_error(ap6_regression_outcome_keys(not_registered, tr_cfg), "does not register")
  expect_error(ap6_regression_outcome_keys(list(), tr_cfg), "no formula")
})

test_that("every downstream row keeps the name, the key and the response column", {
  register <- tr_register()
  summaries <- tr_summaries(register)
  for (column in c("outcome", "outcome_key", "response_column")) {
    expect_true(column %in% names(summaries), info = column)
    expect_false(anyNA(summaries[[column]]), info = column)
  }
  expect_setequal(unique(summaries$outcome), tr_outcomes)
  expect_setequal(unique(summaries$outcome_key), tr_keys)
  r2 <- tr_r2(register)
  expect_true(all(c("outcome", "outcome_key", "response_column") %in% names(r2)))
})

test_that("a swapped outcome identity fails even when every cell still joins", {
  register <- tr_register()
  summaries <- tr_summaries(register)
  decisions <- extract_regression_prediction_decisions(summaries, tr_cfg)
  expect_equal(nrow(decisions), 20L)
  expected <- tr_estimate(decisions$outcome_key,
                          paste0("b_", zm_z_col(decisions$predictor, tr_codebook)))
  expect_equal(decisions$estimate, expected)

  swapped <- summaries
  aggression <- swapped$outcome_key == "asc_agg"
  submission <- swapped$outcome_key == "asc_sub"
  swapped$outcome_key[aggression] <- "asc_sub"
  swapped$outcome_key[submission] <- "asc_agg"
  swapped_decisions <- extract_regression_prediction_decisions(swapped, tr_cfg)
  swapped_expected <- tr_estimate(
    swapped_decisions$outcome_key,
    paste0("b_", zm_z_col(swapped_decisions$predictor, tr_codebook)))
  expect_equal(nrow(swapped_decisions), 20L)
  expect_false(isTRUE(all.equal(swapped_decisions$estimate, swapped_expected)))
})

# --- the fit register and its lifecycle --------------------------------------

test_that("the register keeps one record per specified model or variant", {
  register <- tr_register()
  expect_equal(length(register), 4L + 8L + 4L)
  roles <- vapply(register, function(record) record$role, character(1))
  expect_equal(as.integer(table(roles)[c("primary", "sweep", "student_refit")]), c(4L, 8L, 4L))
  for (record in register) {
    for (field in c("outcome", "outcome_key", "response_column", "role", "slope_sd", "family",
                    "nu_fixed", "label", "fit_available", "fit_valid", "gate_status", "note")) {
      expect_true(field %in% names(record), info = field)
    }
  }
  widths <- vapply(register[roles == "sweep"], function(record) record$slope_sd, numeric(1))
  expect_setequal(widths, c(0.1, 0.4))
  labels <- vapply(register[roles == "primary"], function(record) record$label, character(1))
  expect_true(all(labels == as.character(tr_cfg$priors$sweep_labels[["0.2"]])))
})

test_that("successful, retried, failed and untriggered fits keep their identity", {
  register <- tr_register()
  primary <- Filter(function(record) record$role == "primary", register)
  names(primary) <- vapply(primary, function(record) record$outcome, character(1))
  expect_true(primary[["authoritarian_aggression"]]$fit_valid)
  expect_identical(primary[["authoritarian_submission"]]$gate_status, "retried_ok")
  expect_true(primary[["authoritarian_submission"]]$fit_valid)
  expect_equal(nrow(primary[["authoritarian_submission"]]$retry_history), 1L)
  expect_false(primary[["conventionalism"]]$fit_valid)
  expect_identical(primary[["conventionalism"]]$gate_status, "not_interpretable")
  expect_identical(primary[["conventionalism"]]$note, "Fit is not interpretable.")

  student <- Filter(function(record) record$role == "student_refit", register)
  names(student) <- vapply(student, function(record) record$outcome, character(1))
  expect_true(student[["authoritarian_aggression"]]$fit_available)
  expect_false(student[["conventionalism"]]$fit_available)
  # An untriggered refit is distinct from a failed one and never valid.
  expect_identical(student[["conventionalism"]]$note, "not triggered")
  expect_true(is.na(student[["conventionalism"]]$fit_valid))
  expect_identical(student[["conventionalism"]]$gate_status, "unavailable")
  expect_identical(student[["conventionalism"]]$outcome_key, "asc_conv")
  expect_equal(student[["authoritarian_aggression"]]$nu_fixed,
               as.numeric(tr_cfg$sensitivity$student_t$nu_fixed))
})

test_that("a missing final gate result stays unavailable and never passes", {
  fits <- tr_primary()
  attr(fits[[1]], "gate_status") <- NULL
  attr(fits[[1]], "diagnostics") <- NULL
  register <- extract_regression_result_records(fits, tr_comparison(), tr_student(), tr_cfg)
  first <- register[[1]]
  expect_true(first$fit_available)
  expect_true(is.na(first$fit_valid))
  expect_identical(first$gate_status, "unavailable")
  expect_match(first$note, "unavailable")
  # The projection reports the unavailable gate rather than a passing one.
  coefficients <- tabulate_regression_coefficients(tr_summaries(register), register, tr_cfg,
                                                    roles = c("primary", "sweep"))
  aggression <- coefficients[coefficients$outcome == "asc_agg" & coefficients$role == "primary", ]
  expect_true(all(is.na(aggression$fit_valid)))
  expect_false(check_prior_width_fits(register, tr_cfg))
})

test_that("a fit collection of the wrong size or with an unknown outcome stops", {
  short <- tr_comparison()
  short[["0.10"]] <- unname(short[["0.10"]][1:3])
  expect_error(
    extract_regression_result_records(tr_primary(), short, tr_student(), tr_cfg),
    "lost its outcome labels"
  )
  renamed <- tr_primary()
  names(renamed)[1] <- "not_a_model"
  expect_error(extract_regression_result_records(renamed, tr_comparison(), tr_student(), tr_cfg),
               "does not declare")
})

# --- the fits reuse the saved standardisation constants ----------------------

tr_regression_input <- function() {
  columns <- c("asc_agg_z", "asc_sub_z", "asc_conv_z", "sdo_dom_z", "zm_security_z", "zm_achievement_z",
               "zm_power_z", "zm_prestige_z", "zm_arousal_z", "age_z", "income_z")
  n <- 12L
  data <- tibble::tibble(
    respondent_id = seq_len(n),
    gender = factor(rep(c("male", "female"), length.out = n), levels = c("male", "female")))
  for (i in seq_along(columns)) data[[columns[i]]] <- ((seq_len(n) + i) %% 5) - 2
  attr(data, "z_parameters") <- tibble::tibble(
    var = sub("_z$", "", columns), source = sub("_z$", "", columns), transform = "identity",
    z_col = columns, mean = 3, sd = 1.5, n = n)
  data
}

test_that("the primary fits read the saved z columns and standardise nothing again", {
  data <- tr_regression_input()
  model_set <- define_regression_model_set(tr_cfg)
  captured <- list()
  testthat::local_mocked_bindings(
    brm = function(...) {
      captured[[length(captured) + 1L]] <<- list(...)
      structure(list(), class = "brmsfit")
    }, .package = "brms")
  fits <- fit_primary_regressions(data, model_set, tr_cfg)
  expect_named(fits, tr_outcomes)
  expect_length(captured, 4L)
  for (call in captured) {
    # The table itself reaches brms; no column is rescaled on the way.
    expect_identical(call$data, data)
    design <- colnames(stats::model.matrix(call$formula, data = call$data))
    expect_setequal(design, c("(Intercept)", "zm_security_z", "zm_achievement_z", "zm_power_z",
                              "zm_prestige_z", "zm_arousal_z", "age_z", "genderfemale", "income_z"))
    expect_identical(call$thin, 1)
    expect_equal(call$seed, tr_cfg$regression$seed)
    expect_equal(call$warmup, tr_cfg$regression$warmup)
  }
  responses <- vapply(captured, function(call) all.vars(call$formula)[[1]], character(1))
  expect_identical(responses, c("asc_agg_z", "asc_sub_z", "asc_conv_z", "sdo_dom_z"))
})

test_that("the prior-predictive check reads exactly one saved constant row per outcome", {
  data <- tr_regression_input()
  parameters <- attr(data, "z_parameters")
  row <- ap6_select_outcome_standardisation("asc_agg_z", parameters)
  expect_equal(nrow(row), 1L)
  expect_identical(names(row), c("var", "z_col", "mean", "sd"))
  expect_error(ap6_select_outcome_standardisation("absent_z", parameters),
               "Exactly one fixed standardisation row")
  doubled <- dplyr::bind_rows(parameters, parameters[1, ])
  expect_error(ap6_select_outcome_standardisation("asc_agg_z", doubled),
               "Exactly one fixed standardisation row")
})

test_that("the two comparison widths are exactly the non-primary registered ones", {
  data <- tr_regression_input()
  model_set <- define_regression_model_set(tr_cfg)
  testthat::local_mocked_bindings(
    brm = function(...) structure(list(), class = "brmsfit"), .package = "brms")
  comparisons <- fit_prior_width_comparisons(data, model_set, tr_cfg)
  expect_named(comparisons, c("0.10", "0.40"))
  expect_true(all(vapply(comparisons, function(width) identical(names(width), tr_outcomes),
                         logical(1))))
})

# --- the report tables -------------------------------------------------------

test_that("the coefficient table keeps every key, interval and gate field", {
  register <- tr_register()
  coefficients <- tabulate_regression_coefficients(tr_summaries(register), register, tr_cfg,
                                                    roles = c("primary", "sweep"))
  required <- c("label", "outcome", "slope_sd", "role", "term", "term_type", "estimate", "sd",
                "p_positive", "p_negative", "ess_bulk", "ess_tail", "rhat", "mcse_median",
                "mcse_q_lo", "mcse_q_hi", "fit_valid", "gate_status", "ess_ok", "rhat_ok",
                "divergences_ok", "treedepth_ok", "bfmi_ok", "ess_target", "rhat_limit",
                "n_divergent", "n_treedepth_hits", "bfmi_min", "mcse_median_max", "mcse_q_max",
                "n_obs", "iter_used", "family", "nu_fixed", "variable", "hdi_lo", "hdi_hi")
  expect_true(all(required %in% names(coefficients)))
  # The interval columns are named by the configured level, not hard-coded.
  ci <- zm_regression_ci_names(tr_cfg)
  expect_identical(ci, c("q2.5", "q97.5"))
  expect_true(all(ci %in% names(coefficients)))
  expect_equal(nrow(coefficients), 12L * length(tr_variables))
  expect_setequal(unique(coefficients$outcome), tr_keys)
  expect_setequal(unique(coefficients$slope_sd), c(0.1, 0.2, 0.4))
  expect_setequal(unique(coefficients$term_type),
                  c("intercept", "predictor", "covariate", "sigma"))
  # The gate fields come from the register, by fit identity.
  failed <- coefficients[coefficients$outcome == "asc_conv" & coefficients$role == "primary", ]
  expect_true(all(failed$gate_status == "not_interpretable"))
  expect_true(all(!failed$fit_valid))
  expect_true(all(!failed$ess_ok))
  expect_equal(unique(failed$n_obs), 600L)
  expect_equal(unique(failed$iter_used), 1000L)
  passing <- coefficients[coefficients$outcome == "asc_agg" & coefficients$role == "primary", ]
  expect_true(all(passing$fit_valid & passing$gate_status %in% ap7_gate_statuses_ok()))
  # A fit whose gate failed is not a valid prior sweep.
  expect_false(check_prior_width_fits(register, tr_cfg))
})

test_that("the prior sweep counts as interpretable when every width's gate passes", {
  fits <- stats::setNames(lapply(tr_outcomes, function(o) tr_fit()), tr_outcomes)
  register <- extract_regression_result_records(fits, tr_comparison(), tr_student(), tr_cfg)
  expect_true(check_prior_width_fits(register, tr_cfg))
})

test_that("an untriggered Student refit keeps its row, its columns and its note", {
  register <- tr_register()
  summaries <- tr_summaries(register)
  student <- tabulate_regression_coefficients(summaries, register, tr_cfg,
                                               roles = "student_refit", emit_role = "student")
  expect_equal(nrow(student), length(tr_variables) + 3L)
  expect_true(all(student$role == "student"))
  untriggered <- student[student$outcome != "asc_agg", ]
  expect_equal(nrow(untriggered), 3L)
  expect_true(all(is.na(untriggered$estimate)))
  expect_true(all(is.na(untriggered$term)))
  expect_true(all(untriggered$note == "not triggered"))
  expect_true(all(is.na(untriggered$fit_valid)))
  expect_true(all(untriggered$nu_fixed == 4))
  # The tail display asks which refits ran; it reads the recorded availability,
  # so the placeholder rows of this projection must not read as a refit.
  expect_equal(rh_fitted_outcomes(student), "asc_agg")
  tails <- tabulate_heavy_tail_checks(register)
  expect_equal(ifelse(tails$outcome %in% rh_fitted_outcomes(student), "yes", "no"),
               ifelse(tails$outcome == "asc_agg", "yes", "no"))
})

test_that("the diagnostics and Student-diagnostics tables keep the gate shape", {
  register <- tr_register()
  diagnostics <- tabulate_sampling_diagnostics(register, tr_cfg, roles = c("primary", "sweep"))
  expect_equal(nrow(diagnostics), 12L)
  expect_identical(names(diagnostics)[1:8],
                   c("outcome", "slope_sd", "role", "label", "family", "n_obs", "iter_used",
                     "gate_status"))
  expect_true(all(c("ok", "ess_bulk_min", "rhat_max", "bfmi_min", "mcse_q_max",
                    "max_treedepth_used", "gate_note") %in% names(diagnostics)))
  student <- tabulate_sampling_diagnostics(register, tr_cfg, roles = "student_refit",
                                              available_only = TRUE)
  expect_equal(nrow(student), 1L)
  expect_identical(student$role, "student_refit")
  expect_identical(student$outcome, "asc_agg")
})

test_that("the R2 projection keeps the Gaussian primary and sweep rows with their definition", {
  register <- tr_register()
  r2 <- tabulate_explained_variance(tr_r2(register), tr_cfg)
  expect_identical(names(r2), c("outcome", "slope_sd", "role", "label", "family",
                                "r2_median", "r2_lo", "r2_hi", "r2_definition"))
  expect_equal(nrow(r2), 12L)
  expect_true(all(r2$family == "gaussian"))
  expect_true(all(r2$r2_definition == "var(mu) / (var(mu) + sigma^2)"))
  expect_equal(zm_regression_r2_definition("student", 4),
               "var(mu) / (var(mu) + 2 * sigma^2) [student, nu = 4 fixed]")
})

test_that("the predictive projections read the retained producer output", {
  checks <- tr_predictive_checks()
  prior <- tabulate_prior_predictive_checks(checks, tr_cfg)
  expect_identical(names(prior), c("outcome", "status", "note", "family", "slope_sd", "role", "label", "n_draws",
                                   "n_obs", "mean_z", "sd_z", "p95_abs_prediction", "mean", "sd",
                                   "q5", "q50", "q95", "share_below_min", "share_above_max",
                                   "share_outside_range", "range_min", "range_max",
                                   "z_mean", "z_sd"))
  expect_equal(nrow(prior), 16L)
  expect_true(all(prior$status == "available"))
  expect_true(all(is.na(prior$note)))
  expect_setequal(unique(prior$role), c("primary", "sweep", "student_refit"))
  expect_setequal(unique(prior$outcome), tr_keys)

  posterior <- tabulate_posterior_predictive_stats(checks, tr_cfg)
  expect_identical(names(posterior), c("outcome", "status", "note", "slope_sd", "stat", "observed", "rep_median",
                                       "rep_lo", "rep_hi", "p_value"))
  expect_true(all(posterior$status == "available"))
  expect_true(all(is.na(posterior$note)))
  expect_identical(unique(posterior$stat), c("mean", "sd", "min", "max", "kurtosis", "skew"))
  expect_setequal(unique(posterior$outcome), tr_keys)

  draws <- collect_posterior_predictive_draws(checks, tr_cfg)
  expect_identical(names(draws), c("outcome", "type", "draw", "obs", "value"))
  expect_setequal(unique(draws$outcome), tr_keys)
})

test_that("the prior-sensitivity projection reads the retained power-scaling matrices", {
  table <- tabulate_power_scaling(tr_prior_sensitivity(), tr_cfg)
  expect_identical(names(table), c("outcome", "slope_sd", "block", "term", "term_type",
                                   "prior_selection", "components", "lower_alpha", "upper_alpha",
                                   "div_measure", "sensitivity_threshold", "prior_sens",
                                   "lik_sens", "diagnosis", "diagnosis_raw", "note", "fit_gate_status", "fit_valid", "interpretable", "pareto_k_max",
                                   "importance_sampling_draws", "pareto_k_threshold",
                                   "importance_sampling_status", "settings_status",
                                   "priorsense_version"))
  expect_setequal(unique(table$block), as.character(tr_cfg$sensitivity$powerscale$blocks))
  expect_true("bayes_R2" %in% table$term)
  expect_identical(unique(table$term_type[table$term == "bayes_R2"]), "derived")
  expect_equal(unique(table$prior_sens[table$term == "zm_security"]), 0.02)
  expect_true(all(is.na(table$note)))
})

test_that("the heavy-tail projection reports the trigger the refit producer recorded", {
  register <- tr_register()
  tails <- tabulate_heavy_tail_checks(register)
  expect_identical(names(tails), c("outcome", "heavy_tails", "note"))
  expect_identical(tails$outcome, tr_keys)
  expect_identical(tails$heavy_tails, c(TRUE, FALSE, FALSE, FALSE))
  expect_identical(tails$note, c(NA_character_, rep("not triggered", 3L)))
})

test_that("the provenance projection carries the fit-time record with its role and label", {
  register <- tr_register()
  provenance <- tabulate_regression_provenance(register)
  expect_identical(names(provenance)[1:2], c("role", "label"))
  expect_equal(nrow(provenance), 13L)
  expect_setequal(unique(provenance$role), c("primary", "sweep", "student_refit"))
  expect_setequal(unique(provenance$outcome), tr_keys)
  expect_true(all(c("stan_code_hash", "hash_algorithm", "brms_version", "cmdstan_version",
                    "seed", "gate_status") %in% names(provenance)))
  expect_setequal(unique(provenance$slope_sd), c(0.1, 0.2, 0.4))
})

test_that("an absent retained producer is a prerequisite error, never a fabricated value", {
  # Posterior-predictive draws.
  without_draws <- tr_predictive_checks(with_draws = FALSE)
  expect_error(collect_posterior_predictive_draws(without_draws, tr_cfg),
               "retained posterior-predictive draws")
  # Full power-scaling matrices.
  expect_error(tabulate_power_scaling(tr_prior_sensitivity(with_matrices = FALSE), tr_cfg),
               "retained power-scaling matrices")
  # Fit-time provenance.
  fits <- tr_primary()
  attr(fits[[1]], "provenance") <- NULL
  register <- extract_regression_result_records(fits, tr_comparison(), tr_student(), tr_cfg)
  expect_error(tabulate_regression_provenance(register), "provenance")
})

# --- typed empty results -----------------------------------------------------

test_that("an unavailable fit keeps a typed row rather than losing its columns", {
  register <- tr_register()
  untriggered <- Filter(function(record) !record$fit_available, register)
  expect_equal(length(untriggered), 3L)
  row <- ap6_unavailable_regression_summary(untriggered[[1]])
  expect_equal(nrow(row), 1L)
  for (column in c("outcome", "outcome_key", "response_column", "role", "slope_sd", "label",
                   "family", "nu_fixed", "fit_available", "fit_valid", "gate_status", "note")) {
    expect_true(column %in% names(row), info = column)
  }
  bound <- ap6_bind_regression_coefficient_evidence(list(row))
  expect_equal(nrow(bound), 1L)
})

test_that("the Gaussian-Student pairing represents every Gaussian row", {
  register <- tr_register()
  summaries <- tr_summaries(register)
  paired <- extract_regression_likelihood_robustness(summaries, tr_r2(register))
  expect_equal(nrow(paired$coefficients),
               sum(summaries$role == "primary"))
  expect_true(all(c("estimate_gaussian", "estimate_student", "note_gaussian", "note_student") %in%
                    names(paired$coefficients)))
  expect_equal(nrow(paired$r2), 4L)
  expect_true(all(c("r2_median_gaussian", "r2_median_student") %in% names(paired$r2)))
  # The untriggered outcomes keep their Gaussian row with an unavailable Student side.
  expect_true(any(is.na(paired$r2$r2_median_student)))
})

# --- the coefficient-evidence and predictive-check helpers -------------------

test_that("the highest-density intervals use HDInterval on the coefficients only", {
  set.seed(5)
  draws <- posterior::as_draws_df(data.frame(
    b_Intercept = stats::rnorm(500), b_zm_security_z = stats::rnorm(500),
    sigma = stats::runif(500)))
  intervals <- ap6_highest_density_coefficient_intervals(draws, interval_level = 0.95)
  expect_identical(names(intervals), c("variable", "hdi_lo", "hdi_hi"))
  expect_setequal(intervals$variable, c("b_Intercept", "b_zm_security_z"))
  expected <- unname(HDInterval::hdi(draws$b_zm_security_z, credMass = 0.95))
  expect_equal(intervals$hdi_lo[intervals$variable == "b_zm_security_z"], expected[1])
  expect_equal(intervals$hdi_hi[intervals$variable == "b_zm_security_z"], expected[2])
})

test_that("the stored evidence joins the intervals", {
  register <- tr_register()
  record <- register[[1]]
  coefficients <- tr_coefficients(record)
  summary <- ap6_pack_regression_coefficient_summary(
    coefficients, tr_precision(coefficients$variable), record, tr_cfg)
  stored <- ap6_store_regression_coefficient_evidence(
    summary, tr_hdi(coefficients$variable))
  expect_true(all(c("hdi_lo", "hdi_hi") %in% names(stored)))
  expect_true(is.na(stored$hdi_lo[stored$variable == "sigma"]))
})

test_that("the one-sided tail trigger fires only on the three declared comparisons", {
  expect_false(ap6_student_tail_trigger(tr_posterior_leaf(kurtosis_observed = 0.1)))
  expect_true(ap6_student_tail_trigger(tr_posterior_leaf(kurtosis_observed = 0.9)))
  # A lighter tail never fires.
  expect_false(ap6_student_tail_trigger(tr_posterior_leaf(kurtosis_observed = -0.9)))
})

test_that("the predictive statistics keep the observed value, the interval and the median", {
  set.seed(6)
  observed <- stats::rnorm(50)
  replicated <- matrix(stats::rnorm(50 * 40), nrow = 40)
  statistics <- ap6_posterior_predictive_statistics(
    observed, replicated, statistics = c("mean", "excess_kurtosis"), interval = c(0.025, 0.975))
  expect_named(statistics, c("mean", "excess_kurtosis"))
  expect_equal(statistics$mean$observed, mean(observed))
  expect_equal(statistics$excess_kurtosis$observed, psych::kurtosi(observed, type = 3))
  expect_length(statistics$mean$replicated_interval, 2L)
  expect_true(is.finite(statistics$mean$replicated_median))
})

test_that("the skewness and kurtosis definitions are the finite-sample type 3 estimators", {
  # Hand-computed type 3: b1 = m3 / m2^(3/2) times ((n - 1) / n)^(3/2), and
  # b2 = m4 / m2^2 times ((n - 1) / n)^2, minus 3 for the excess.
  x <- c(1, 2, 3, 4, 10)
  n <- length(x)
  centred <- x - mean(x)
  moment <- function(k) sum(centred^k) / n
  expected_skew <- moment(3) / moment(2)^1.5 * ((n - 1) / n)^1.5
  expected_kurtosis <- moment(4) / moment(2)^2 * ((n - 1) / n)^2 - 3
  statistics <- ap6_posterior_predictive_statistics(
    x, matrix(x, nrow = 1L), statistics = c("skewness", "excess_kurtosis"),
    interval = c(0.025, 0.975))
  expect_equal(statistics$skewness$observed, expected_skew)
  expect_equal(statistics$excess_kurtosis$observed, expected_kurtosis)
  # The other psych types would give different numbers, so the choice is pinned.
  expect_false(isTRUE(all.equal(expected_skew, psych::skew(x, type = 1))))
  expect_false(isTRUE(all.equal(expected_kurtosis, psych::kurtosi(x, type = 1))))
})

# --- the fit-collection map the gate needs -----------------------------------

test_that("the gate map preserves the collection shape, its names and its attributes", {
  student <- tr_student()
  mapped <- ap6_map_regression_fits(student, function(fit, outcome, slope_sd) {
    if (is.null(fit)) return(NULL)
    structure(fit, gated_outcome = outcome)
  })
  expect_named(mapped, tr_outcomes)
  expect_equal(length(mapped), 4L)
  expect_null(mapped[["conventionalism"]])
  expect_identical(attr(mapped[[1]]$fit, "gated_outcome"), "authoritarian_aggression")
  expect_identical(attr(mapped, "heavy_tail_triggers"), attr(student, "heavy_tail_triggers"))

  nested <- ap6_map_regression_fits(tr_comparison(), function(fit, outcome, slope_sd) {
    structure(fit, gated_width = slope_sd)
  })
  expect_equal(attr(nested[["0.40"]][["conventionalism"]], "gated_width"), 0.4)
})
