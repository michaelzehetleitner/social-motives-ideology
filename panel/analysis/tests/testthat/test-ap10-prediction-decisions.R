# The Table 3 decision route: which coefficient rows decide, which cell each
# value lands in and which verdict each cell receives. No test here fits a
# model: every fit is the stub of test-targets-regressions.R carrying the
# attributes the validity gate records.

local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) {
    parent <- dirname(dir)
    if (identical(parent, dir)) stop("project root not found from ", getwd())
    dir <- parent
  }
  for (f in c("config.R", "ap6_regressions.R", "ap6_pipeline.R", "report_supplement_prior_sensitivity.R", "report_supplement_model_checks.R", "report_supplement_sampling_diagnostics.R", "report_supplement_explained_variance.R", "report_supplement_software.R", "report_results_associations.R", "ap6_interval_comparison.R",
              "ap10_inference.R", "report_results_network.R", "report_supplement_network_detail.R", "report_helpers.R")) {
    source(file.path(dir, "R", f), local = FALSE)
  }
})

tr_cfg <- zm_config(profile = "smoke",
                    path = file.path(zm_root(), "config", "analysis_plan.yaml"))

tr_outcomes <- c("authoritarian_aggression", "authoritarian_submission",
                 "conventionalism", "sdo_d_dominance")
tr_keys <- c("asc_agg", "asc_sub", "asc_conv", "sdo_dom")
tr_variables <- c("b_Intercept", "b_zm_security_z", "b_zm_achievement_z", "b_zm_power_z",
                  "b_zm_prestige_z", "b_zm_arousal_z", "b_age_z", "b_genderfemale",
                  "b_income_z", "sigma")

# --- fixtures, copied from test-targets-regressions.R ------------------------

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
  tibble::tibble(
    variable = tr_variables, estimate = estimate, sd = 0.05,
    q2.5 = estimate - 0.01, q97.5 = estimate + 0.01,
    p_positive = 0.99, p_negative = 0.01
  )
}

# test-targets-regressions.R gives every fit the same simulation precision. Here
# the precision is the fit's own gate measurement, because in production both
# are read off the same draws: the register stores the gate values, and the
# coefficient rows carry the same values, so the two can only be pinned against
# each other when the stub does not make them disagree. `tr_fixed_precision()` below keeps the constant variant, to show
# which of the two the projection reads.
tr_precision <- function(variables, record) {
  diagnostics <- record$diagnostics
  tibble::tibble(
    variable = variables, ess_bulk = diagnostics$ess_bulk_min,
    ess_tail = diagnostics$ess_tail_min, rhat = diagnostics$rhat_max,
    mcse_median = 0.002, mcse_q_lo = 0.003, mcse_q_hi = 0.004
  )
}

tr_fixed_precision <- function(variables, record) {
  tibble::tibble(
    variable = variables, ess_bulk = 12000, ess_tail = 11000, rhat = 1.001,
    mcse_median = 0.002, mcse_q_lo = 0.003, mcse_q_hi = 0.004
  )
}

tr_hdi <- function(variables) {
  coefficients <- setdiff(variables, "sigma")
  tibble::tibble(variable = coefficients, hdi_lo = -0.5, hdi_hi = 0.5)
}

# The coefficient summaries the production verb would build from those stubs.
tr_summaries <- function(register, precision = tr_precision) {
  evidence <- lapply(register, function(record) {
    if (!record$fit_available) return(ap6_unavailable_regression_summary(record))
    coefficients <- tr_coefficients(record)
    ap6_store_regression_coefficient_evidence(
      ap6_pack_regression_coefficient_summary(
        coefficients, precision(coefficients$variable, record), record, tr_cfg),
      tr_hdi(coefficients$variable)
    )
  })
  ap6_bind_regression_coefficient_evidence(evidence)
}

# --- the fixture this file decides on ---------------------------------------

register <- tr_register()
summaries <- tr_summaries(register)
identity <- ap6_regression_outcome_identity(tr_cfg)
predictions <- tr_cfg$predictions$table
cells <- tibble::tibble(
  outcome_key = rep(names(predictions), vapply(predictions, length, integer(1))),
  predictor = unlist(lapply(predictions, names), use.names = FALSE)
)

# The primary Gaussian row of one cell of the summaries.
tr_cell_row <- function(summaries, outcome_key, motive) {
  which(summaries$role == "primary" & summaries$family == tr_cfg$regression$family &
          summaries$slope_sd == tr_cfg$priors$slope_sd_primary &
          summaries$outcome_key == outcome_key & summaries$term == motive)
}

# Hand-set one cell's median and central interval.
tr_set_cell <- function(summaries, outcome_key, motive, lo, hi) {
  row <- tr_cell_row(summaries, outcome_key, motive)
  summaries$q_lo[row] <- lo
  summaries$q_hi[row] <- hi
  summaries$estimate[row] <- (lo + hi) / 2
  summaries
}

# --- (a) the cells, their order and their identity ---------------------------

test_that("the decisions are the configured cells, each carrying its own coefficient", {
  decisions <- extract_regression_prediction_decisions(summaries, tr_cfg)
  expect_equal(nrow(decisions), 20L)
  expect_equal(decisions$outcome_key, cells$outcome_key)
  expect_equal(decisions$predictor, cells$predictor)
  # every cell holds the coefficient of its own outcome and its own motive
  expected <- vapply(seq_len(nrow(cells)), function(i) {
    tr_estimate(cells$outcome_key[[i]], paste0("b_", cells$predictor[[i]], "_z"))
  }, numeric(1))
  expect_equal(decisions$estimate, expected)
  # the readable name travels beside the key and agrees with the model set
  expect_equal(decisions$outcome,
               identity$outcome[match(decisions$outcome_key, identity$outcome_key)])
  expect_equal(names(decisions)[1:4],
               c("outcome_key", "outcome", "predictor", "predicted_sign"))
  expect_true(all(c("variable", "estimate", "sd", "q_lo", "q_hi", "p_positive", "p_negative",
                    "ess_bulk", "ess_tail", "rhat", "mcse_median", "mcse_q_lo", "mcse_q_hi",
                    "term_type", "hdi_lo", "hdi_hi") %in% names(decisions)))
  expect_equal(names(decisions)[(ncol(decisions) - 6L):ncol(decisions)],
               c("fit_valid", "gate_status", "note",
                 "credible_raw", "credible", "observed_sign", "verdict"))
})

# --- (b) identity is the key, and an unusable key stops ----------------------

test_that("the cells follow the outcome key, and a broken key mapping stops", {
  decisions <- extract_regression_prediction_decisions(summaries, tr_cfg)
  swapped <- summaries
  aggression <- swapped$outcome_key == "asc_agg"
  submission <- swapped$outcome_key == "asc_sub"
  swapped$outcome_key[aggression] <- "asc_sub"
  swapped$outcome_key[submission] <- "asc_agg"
  moved <- extract_regression_prediction_decisions(swapped, tr_cfg)
  for (motive in tr_cfg$regression$motives) {
    for (pair in list(c("asc_agg", "asc_sub"), c("asc_sub", "asc_agg"))) {
      here <- decisions$outcome_key == pair[[1]] & decisions$predictor == motive
      there <- moved$outcome_key == pair[[1]] & moved$predictor == motive
      expect_false(isTRUE(all.equal(decisions$estimate[here], moved$estimate[there])),
                   info = paste(pair[[1]], motive))
      expect_equal(moved$estimate[there],
                   tr_estimate(pair[[2]], paste0("b_", motive, "_z")))
    }
  }
  unknown <- summaries
  unknown$outcome_key[unknown$outcome_key == "sdo_dom"] <- "sdo_e"
  expect_error(extract_regression_prediction_decisions(unknown, tr_cfg),
               "A primary coefficient row names an outcome key the model set does not declare: sdo_e.",
               fixed = TRUE)
  duplicated_cell <- summaries[c(seq_len(nrow(summaries)),
                                 tr_cell_row(summaries, "asc_agg", "zm_power")), ]
  expect_error(extract_regression_prediction_decisions(duplicated_cell, tr_cfg),
               "More than one primary coefficient for: asc_agg zm_power.", fixed = TRUE)
  unknown_prediction <- tr_cfg
  names(unknown_prediction$predictions$table)[[2]] <- "asc_subordination"
  expect_error(extract_regression_prediction_decisions(summaries, unknown_prediction),
               "The prediction table names outcome key(s) the model set does not declare: asc_subordination.",
               fixed = TRUE)
})

# --- (c) only the primary normal(0, .20) Gaussian fits decide ----------------

test_that("sweep and Student rows do not reach Table 3", {
  decisions <- extract_regression_prediction_decisions(summaries, tr_cfg)
  shifted <- summaries
  other <- shifted$role != "primary"
  expect_true(any(other))
  shifted$estimate[other] <- shifted$estimate[other] + 10
  shifted$q_lo[other] <- shifted$q_lo[other] + 10
  shifted$q_hi[other] <- shifted$q_hi[other] + 10
  expect_equal(extract_regression_prediction_decisions(shifted, tr_cfg), decisions)
  # the Student refit of the first outcome carries the primary prior SD, so the
  # role, not the prior, keeps it out
  student <- shifted$role == "student_refit" & shifted$fit_available
  expect_true(any(student))
  expect_true(all(shifted$slope_sd[student] == tr_cfg$priors$slope_sd_primary))
})

# --- (d) a failed final gate withholds the verdict, a retry does not ---------

test_that("the failed fit reports its interval but no verdict, the retried fit decides", {
  decisions <- extract_regression_prediction_decisions(summaries, tr_cfg)
  failed <- decisions[decisions$outcome_key == "asc_conv", ]
  expect_equal(nrow(failed), 5L)
  expect_equal(unique(failed$gate_status), "not_interpretable")
  expect_true(all(is.na(failed$credible)))
  expect_true(all(is.na(failed$verdict)))
  expect_true(all(!is.na(failed$credible_raw)))
  expect_true(all(!is.na(failed$estimate)))
  expect_equal(unique(failed$observed_sign), "+")
  retried <- decisions[decisions$outcome_key == "asc_sub", ]
  expect_equal(unique(retried$gate_status), "retried_ok")
  expect_true(all(retried$credible))
  expect_true(all(!is.na(retried$verdict)))
})

# --- (e) every verdict label, and the endpoint exactly at zero ---------------

test_that("each verdict label follows from the predicted sign and the interval", {
  labelled <- summaries |>
    tr_set_cell("asc_agg", "zm_security", 0.1, 0.3) |>    # predicted "+", credible "+"
    tr_set_cell("asc_agg", "zm_power", -0.3, -0.1) |>    # predicted "+", credible "-"
    tr_set_cell("asc_agg", "zm_prestige", 0.1, 0.3) |>    # marked ±, credible "+"
    tr_set_cell("sdo_dom", "zm_prestige", -0.3, -0.1) |>  # marked ±, credible "-"
    tr_set_cell("asc_sub", "zm_prestige", -0.1, 0.2) |>   # marked ±, not credible
    tr_set_cell("sdo_dom", "zm_security", -0.1, 0.3) |>   # predicted "+", not credible
    tr_set_cell("sdo_dom", "zm_power", 0, 0.3)           # predicted "+", endpoint at zero
  decisions <- extract_regression_prediction_decisions(labelled, tr_cfg)
  verdict_of <- function(key, motive) {
    decisions$verdict[decisions$outcome_key == key & decisions$predictor == motive]
  }
  sign_of <- function(key, motive) {
    decisions$observed_sign[decisions$outcome_key == key & decisions$predictor == motive]
  }
  credible_of <- function(key, motive) {
    decisions$credible[decisions$outcome_key == key & decisions$predictor == motive]
  }
  expect_equal(verdict_of("asc_agg", "zm_security"), "confirmed")
  expect_equal(sign_of("asc_agg", "zm_security"), "+")
  expect_equal(verdict_of("asc_agg", "zm_power"), "disconfirmed")
  expect_equal(sign_of("asc_agg", "zm_power"), "-")
  # a cell marked ± is judged by credibility and sign only, in either direction
  expect_equal(verdict_of("asc_agg", "zm_prestige"), "exploratory_credible")
  expect_equal(sign_of("asc_agg", "zm_prestige"), "+")
  expect_equal(verdict_of("sdo_dom", "zm_prestige"), "exploratory_credible")
  expect_equal(sign_of("sdo_dom", "zm_prestige"), "-")
  expect_equal(verdict_of("asc_sub", "zm_prestige"), "exploratory_not_credible")
  # a directional cell whose interval includes zero gets no verdict (AP10)
  expect_false(credible_of("sdo_dom", "zm_security"))
  expect_true(is.na(verdict_of("sdo_dom", "zm_security")))
  # an interval whose endpoint is exactly zero does not exclude zero
  expect_false(credible_of("sdo_dom", "zm_power"))
  expect_true(is.na(verdict_of("sdo_dom", "zm_power")))
  expect_equal(sign_of("sdo_dom", "zm_power"), "+")
  expect_setequal(
    unique(stats::na.omit(decisions$verdict)),
    c("confirmed", "disconfirmed", "exploratory_credible", "exploratory_not_credible")
  )
})

# --- (h) a primary fit the summaries never mention ---------------------------

test_that("a missing primary fit leaves explicit empty cells", {
  incomplete <- summaries[!(summaries$outcome_key == "sdo_dom" & summaries$role == "primary"), ]
  decisions <- extract_regression_prediction_decisions(incomplete, tr_cfg)
  missing <- decisions[decisions$outcome_key == "sdo_dom", ]
  expect_equal(nrow(missing), 5L)
  expect_true(all(is.na(missing$estimate)))
  expect_true(all(is.na(missing$q_lo)))
  expect_true(all(is.na(missing$fit_valid)))
  expect_true(all(is.na(missing$outcome)))
  expect_true(all(is.na(missing$credible)))
  expect_true(all(is.na(missing$verdict)))
  # the other three outcomes still decide
  expect_true(all(!is.na(decisions$verdict[decisions$outcome_key %in% c("asc_agg", "asc_sub")])))
})
