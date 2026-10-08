# LAYER 3 tests — synthetic-data summaries and recovery helpers.
# Generating values, planted problems and recovery comparisons are checked
# against hand-built saved results.

local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) {
    parent <- dirname(dir)
    if (identical(parent, dir)) stop("project root not found from ", getwd())
    dir <- parent
  }
  for (f in c("config.R", "simulate_testdata.R", "report_helpers.R", "result_outputs.R", "report_synthetic_data.R")) {
    source(file.path(dir, "R", f), local = FALSE)
  }
  source(file.path(dir, "scripts", "check_synthetic_recovery.R"), local = FALSE)
  assign("synthetic_test_root", dir, envir = .GlobalEnv)
})

root <- synthetic_test_root
truth_path <- file.path(root, "config", "simulation_truth.yaml")
# Coefficient rows as target `report_primary_coefficients` holds them.
synthetic_coefficients <- function(terms, estimates, outcome = "asc_agg", fit_valid = TRUE) {
  tibble::tibble(
    outcome = outcome, term = terms,
    term_type = ifelse(terms == "Intercept", "intercept", ifelse(startsWith(terms, "zm_"), "predictor", "covariate")),
    estimate = estimates, q2.5 = estimates - 0.1, q97.5 = estimates + 0.1, fit_valid = fit_valid
  )
}

# The labels for every scale come from the codebook.
synthetic_labels <- function() {
  rh_labels(zm_codebook(zm_config("smoke", file.path(root, "config", "analysis_plan.yaml"))))
}

test_that("the generating coefficients are those of the export scenario and match the file's declared counts", {
  base <- zm_truth(truth_path)
  truth <- read_export_generating_values(truth_path)
  expect_identical(truth$scenario, base$scenarios$export)
  generating <- tabulate_generating_coefficients(truth)
  expect_equal(nrow(generating), sum(lengths(base$true_beta)))
  overrides <- base$scenarios[[base$scenarios$export]]$true_beta_overrides
  for (outcome in names(overrides)) {
    for (predictor in names(overrides[[outcome]])) {
      expect_equal(generating$generating[generating$outcome == outcome & generating$predictor == predictor],
                   overrides[[outcome]][[predictor]])
    }
  }
  plan <- zm_config(profile = "full", path = file.path(root, "config", "analysis_plan.yaml"))
  design <- summarise_generating_design(truth, plan)
  expect_identical(design$n_coefficients, as.integer(base$null_proportion$cells))
  expect_identical(design$n_zero, as.integer(base$null_proportion$null_cells))
  # every motive cell of the prediction table is placed
  cells <- design$prediction_cells
  expect_identical(cells$n_directional + cells$n_open, design$n_motive_coefficients)
})

test_that("the calibration counts follow from the generating file's calibration reference", {
  truth <- list(calibration_reference = list(
    n_observations = 1000, n_outcomes = 2, n_predictors = 5, interval_level = 0.95,
    n_intervals_including_zero = 6, abs_posterior_mean_range_intervals_excluding_zero = c(0.3, 0.1)
  ))
  out <- summarise_calibration_reference(truth)
  expect_identical(out$n_coefficients, 10L)
  expect_identical(out$n_excluding_zero, 4L)
  expect_equal(out$share_including_zero, 0.6)
  expect_equal(out$excluding_zero_range, c(0.1, 0.3))
  truth$calibration_reference$n_predictors <- NULL
  expect_error(summarise_calibration_reference(truth), "n_predictors")
  # the generating file carries the complete block
  expect_no_error(summarise_calibration_reference(zm_truth(truth_path)))
})

test_that("the design summary counts zero cells and places the motive cells on the prediction table", {
  truth <- list(
    n_total = 10,
    true_beta = list(y1 = list(m1 = 0.2, m2 = 0, age = 0.1, male = 0),
                     y2 = list(m1 = -0.1, m2 = 0, age = 0, male = 0.3)),
    motive_correlations = list(order = c("m1", "m2"), matrix = matrix(c(1, .3, .3, 1), 2)),
    outcome_residual_correlations = list(order = c("y1", "y2"), matrix = matrix(c(1, .4, .4, 1), 2)),
    measurement = list(loading = 0.65)
  )
  plan <- list(scales = list(response_min = 1L, response_max = 6L),
               predictions = list(table = list(y1 = list(m1 = "+", m2 = "-"), y2 = list(m1 = "+", m2 = "±"))))
  design <- summarise_generating_design(truth, plan)
  expect_identical(design$n_coefficients, 8L)
  expect_identical(design$n_zero, 4L)
  expect_equal(design$motive_nonzero_range, c(0.1, 0.2))
  expect_identical(design$covariates_nonzero$predictor, c("age", "male"))
  cells <- design$prediction_cells
  expect_identical(c(cells$n_directional, cells$n_directional_effect, cells$n_directional_opposite,
                     cells$n_directional_zero), c(3L, 1L, 1L, 1L))
  expect_identical(c(cells$n_open, cells$n_open_effect, cells$n_open_zero), c(1L, 0L, 1L))
})

test_that("the gender coefficient is compared as a difference between men and women", {
  generating <- tibble::tibble(outcome = "asc_agg", predictor = c("zm_power", "male", "age"), generating = c(0.25, 0.20, 0))
  coefficients <- synthetic_coefficients(c("Intercept", "zm_power", "gendermale", "age", "sigma"),
                                         c(-0.1, 0.22, 0.35, 0.05, 0.9))
  coefficients$term_type[coefficients$term == "sigma"] <- "sigma"
  out <- compare_estimates_with_generating_values(coefficients, generating, male_sd = 0.5)
  expect_identical(out$predictor, c("zm_power", "male", "age"))
  expect_identical(out$term, c("zm_power", "gendermale", "age"))
  expect_equal(out$generating_compared, c(0.25, 0.40, 0))
  expect_equal(out$estimate, c(0.22, 0.35, 0.05))
  expect_identical(out$ci_level, rep(95, 3))
  expect_identical(out$excludes_zero, c(TRUE, TRUE, FALSE))
  expect_identical(out$contains_generating, c(TRUE, TRUE, TRUE))
})

test_that("a contrast against men is compared with its sign and its bounds reversed", {
  generating <- tibble::tibble(outcome = "asc_agg", predictor = "male", generating = 0.20)
  out <- compare_estimates_with_generating_values(synthetic_coefficients("genderfemale", -0.35), generating, male_sd = 0.5)
  expect_equal(out$estimate, 0.35)
  expect_equal(c(out$lower, out$upper), c(0.25, 0.45))
  expect_true(out$contains_generating)
})

test_that("a coefficient of a fit that failed its sampling checks shows no estimate", {
  generating <- tibble::tibble(outcome = c("asc_agg", "asc_sub"), predictor = "zm_power", generating = c(0.25, 0))
  coefficients <- dplyr::bind_rows(
    synthetic_coefficients("zm_power", 0.22),
    synthetic_coefficients("zm_power", 0.3, outcome = "asc_sub", fit_valid = FALSE)
  )
  out <- compare_estimates_with_generating_values(coefficients, generating, male_sd = 0.5)
  expect_equal(out$estimate, c(0.22, NA))
  expect_true(is.na(out$excludes_zero[2]))
  text <- describe_generating_agreement(out, synthetic_labels(), "zm_power")
  expect_match(text, "The nonzero generating coefficient had a CrI that excluded zero", fixed = TRUE)
  expect_match(text, "One coefficient has no estimate, because the fit did not pass its sampling checks.", fixed = TRUE)
})

test_that("the agreement sentence counts the pattern and names every exception with its estimate", {
  comparison <- tibble::tibble(
    outcome = "asc_agg", predictor = c("zm_power", "zm_security", "income"), generating = c(0.25, 0, 0),
    generating_compared = c(0.25, 0, 0), estimate = c(0.22, 0.01, -0.08), lower = c(0.14, -0.07, -0.15),
    upper = c(0.29, 0.09, -0.01)
  )
  comparison$excludes_zero <- comparison$lower > 0 | comparison$upper < 0
  comparison$contains_generating <- comparison$generating_compared >= comparison$lower &
    comparison$generating_compared <= comparison$upper
  text <- describe_generating_agreement(comparison, synthetic_labels(), c("zm_power", "zm_security"))
  expect_match(text, "The nonzero generating coefficient had a CrI that excluded zero on the side of the generating value.",
               fixed = TRUE)
  expect_match(text, paste("Of the two coefficients generated as zero, one had a CrI that included zero;",
                           "the exception was income for ASC aggression, −0.08 [−0.15, −0.01]."), fixed = TRUE)
  expect_match(text, "for 2 of the 3 coefficients, including both motive coefficients.", fixed = TRUE)
  # with every interval as generated, the sentences say so without exceptions
  comparison$estimate[3] <- 0
  comparison$lower[3] <- -0.07
  comparison$upper[3] <- 0.07
  comparison$excludes_zero[3] <- FALSE
  comparison$contains_generating[3] <- TRUE
  text <- describe_generating_agreement(comparison, synthetic_labels(), c("zm_power", "zm_security"))
  expect_match(text, "Both coefficients generated as zero had a CrI that included zero.", fixed = TRUE)
  expect_match(text, "The CrI contained the generating value for all 3 coefficients.", fixed = TRUE)
  expect_false(grepl("exception", text, fixed = TRUE))
})

test_that("gender is converted with the indicator's standard deviation and income's log is named", {
  comparison <- tibble::tibble(outcome = c("asc_agg", "sdo_dom", "asc_agg"), predictor = c("male", "male", "income"),
                               generating = c(0.2, 0.35, 0), generating_compared = c(0.4, 0.7, 0))
  text <- describe_gender_conversion(comparison, 0.5, c(asc_agg = "ASC aggression", sdo_dom = "SDO-D"))
  expect_match(text, "0.20 for ASC aggression and 0.35 for SDO-D", fixed = TRUE)
  expect_match(text, "standard deviation in the generated data, 0.50, they give", fixed = TRUE)
  expect_match(text, "estimate, 0.40 and 0.70", fixed = TRUE)
  expect_match(describe_income_comparison(comparison), "zero for every outcome", fixed = TRUE)
})

test_that("the comparison table has one row per predictor and a generating and an estimate column per outcome", {
  plan <- list(regression = list(outcomes = c("asc_agg", "sdo_dom"), motives = "zm_power"))
  comparison <- tibble::tibble(
    outcome = rep(c("asc_agg", "sdo_dom"), each = 4), predictor = rep(c("zm_power", "age", "male", "income"), 2),
    generating_compared = c(0.25, 0, 0.4, 0, 0.25, 0, 0.7, 0),
    estimate = c(0.22, 0, 0.35, -0.08, 0.21, NA, 0.67, -0.02),
    lower = c(0.14, -0.07, 0.21, -0.15, 0.14, NA, 0.53, -0.08),
    upper = c(0.29, 0.07, 0.5, -0.01, 0.29, NA, 0.8, 0.05), ci_level = 95
  )
  labels <- synthetic_labels()
  labels["sdo_dom"] <- "SDO-D"
  tab <- tabulate_generating_comparison(comparison, labels, plan, engine = "gt")
  data <- tab[["_data"]]
  expect_identical(names(data), c("predictor", "generating_asc_agg", "estimate_asc_agg",
                                  "generating_sdo_dom", "estimate_sdo_dom"))
  expect_identical(data$predictor, c("Power", "Age band", "Gender (men − women)", "Income band per household member"))
  expect_identical(data$estimate_asc_agg[1], "0.22 [0.14, 0.29]")
  expect_identical(data$estimate_sdo_dom[2], "—")
  expect_identical(data$generating_sdo_dom[3], "0.70")
  expect_setequal(unlist(tab[["_spanners"]]$spanner_label), c("ASC aggression", "SDO-D"))
})

test_that("planted exclusion records are compared rule by rule with the records the steps removed", {
  truth <- list(n_total = 100, covariates = list(gender_divers_n = 2),
                exclusions = list(consent_refused = 2, incomplete = 3, age_under_18 = 1, quota_full = 4,
                                  attention_1_failed = 2, attention_2_failed = 1))
  steps <- tibble::tibble(
    criterion = c("not_consenting", "incomplete", "age_outside_range", "attention_1_wrong", "attention_2_wrong",
                  "gender_divers", "dropped_unfillable_gaps"),
    n_excluded = c(2L, 3L, 1L, 2L, 1L, 2L, 0L)
  )
  flow <- list(steps = steps, n_quota_full = 4L, n_started = 100L, n_analysis = 85L)
  out <- compare_planted_exclusions(truth, flow)
  expect_identical(out$rules$rule, c("quota_full", "consent_refused", "incomplete", "age_under_18",
                                     "attention_1_failed", "attention_2_failed", "gender_divers"))
  expect_identical(out$rules$planted, out$rules$removed)
  expect_identical(out$n_started, 100L)
  expect_identical(out$n_analysis, 85L)
  flow$steps$n_excluded[2] <- 2L
  mismatch <- compare_planted_exclusions(truth, flow)$rules
  expect_identical(mismatch$planted[mismatch$rule == "incomplete"], 3L)
  expect_identical(mismatch$removed[mismatch$rule == "incomplete"], 2L)
  # a planted rule without a known exclusion step stops the comparison
  truth$exclusions$new_rule <- 1
  expect_error(compare_planted_exclusions(truth, flow), "new_rule")
})

test_that("planted gaps are compared with the cells the fill wrote, and gender is never filled", {
  codebook <- list(scales = tibble::tibble(scale_key = c("asc_agg", "zm_security"),
                                           item_codes = list(c("ASC_aag_1", "ASC_aag_2"), c("UMS_int_1", "UMS_int_2"))))
  truth <- list(missingness = list(cells = list(
    list(column = "ASC_aag_2", n_missing = 2L), list(column = "demo_age", n_missing = 1L),
    list(column = "demo_gender", n_missing = 1L)
  )))
  imputation <- list(imputed_cells = tibble::tibble(variable = c("ASC_aag_2", "ASC_aag_2", "demo_age")))
  sizes <- list(n_analysis = 50L, n_regressions = 49L, n_network = 50L, n_omitted_for_missing_gender = 1L)
  plan <- list(missing_data = list(fill = list(demographics = list(order = c("demo_age", "demo_hh_members",
                                                                             "demo_income_hh_net")))))
  gaps <- compare_planted_gaps(truth, imputation, sizes, codebook, plan)
  expect_identical(gaps$cells$position, c(2L, NA, NA))
  expect_identical(gaps$cells$filled, c(2L, 1L, 0L))
  expect_identical(gaps$cells$fillable, c(TRUE, TRUE, FALSE))
  expect_identical(gaps$cells$planted, c(2L, 1L, 1L))
  expect_identical(gaps$n_analysis, 50L)
  expect_identical(gaps$n_regressions, 49L)
  expect_identical(gaps$n_omitted_for_missing_gender, 1L)
  # an unfilled item cell is reported, not hidden
  imputation$imputed_cells <- imputation$imputed_cells[-1, ]
  partially_filled <- compare_planted_gaps(truth, imputation, sizes, codebook, plan)$cells
  expect_identical(partially_filled$planted, c(2L, 1L, 1L))
  expect_identical(partially_filled$filled, c(1L, 1L, 0L))
})

test_that("the planted cross-loading is read from the expected solutions and the leading residuals", {
  items <- c("A1", "A2", "S1", "S2", "C1", "C2", "C3")
  scales <- rep(c("asc_agg", "asc_sub", "asc_conv"), c(2, 2, 3))
  codebook <- list(scales = tibble::tibble(scale_key = c("asc_agg", "asc_sub", "asc_conv"),
                                           item_codes = list(c("A1", "A2"), c("S1", "S2"), c("C1", "C2", "C3"))))
  loadings <- matrix(0.02, length(items), 3, dimnames = list(items, c("F2", "F1", "F3")))
  loadings[c("A1", "A2"), "F2"] <- 0.6
  loadings[c("S1", "S2"), "F1"] <- 0.6
  loadings[c("C1", "C2", "C3"), "F3"] <- 0.6
  loadings["C3", "F1"] <- -0.31
  membership <- tibble::tibble(item = items, theoretical_scale = scales,
                               assigned_factor = rep(c("F2", "F1", "F3"), c(2, 2, 3)))
  expected <- list(set_name = "asc", factors = 3L, expected_reference = TRUE, membership = membership,
                   loadings = loadings)
  alternative <- expected
  alternative$expected_reference <- FALSE
  clarity <- list(solutions = list(expected, alternative))
  cfa <- list(
    loadings = data.frame(model = "asc_three_factor", factor = scales, item = items),
    largest_residual_correlations = tibble::tibble(
      model = "asc_three_factor", item_1 = c("C3", "C3", "A1"), item_2 = c("S2", "S1", "C1"),
      residual_correlation = c(0.09, 0.11, -0.10), standardized_residual_correlation = c(3.8, 4.3, -3.6)
    )
  )
  latent <- tibble::tibble(asc_conv = c(-1, 0, 1, 2), asc_sub = c(-1, 0, 1, 2))
  truth <- list(measurement = list(loading = 0.65),
                cross_loadings = list(list(item = "C3", factor = "asc_sub", loading = 0.45)))
  plan <- list(factor_analysis = list(loading_display_cutoff = 0.40))
  found <- locate_planted_cross_loadings(truth, latent, clarity, cfa, codebook, plan)[[1]]
  # the generator's rescaling, at a latent correlation of 1
  expect_equal(found$expected_cross_loading, 0.45 / sqrt(1 + 0.45^2 + 2 * 0.65 * 0.45))
  expect_identical(found$position, 3L)
  expect_identical(nrow(found$exploratory), 1L)
  expect_identical(found$exploratory$other_factor, "F1")
  expect_equal(found$exploratory$loading_other_factor, 0.31)
  expect_identical(found$exploratory$n_loadings_at_cutoff, 1L)
  # the leading run, in the order of the absolute standardised residual
  expect_identical(found$confirmatory$leading$partner, c("S1", "S2"))
})

test_that("the heavy-tail rule is one-sided: kurtosis and maximum above, minimum below the band", {
  truth <- list(residual_contamination = list(outcome = "asc_agg", share = 0.05, multiplier = 4))
  checks <- list(
    posterior_predictive = tibble::tibble(
      outcome = "asc_agg", stat = c("mean", "kurtosis", "min", "max"), observed = c(0, 0.8, -2.8, 4.2),
      rep_lo = c(-0.1, -0.3, -4.0, 2.6), rep_hi = c(0.1, 0.4, -2.6, 4.1)
    ),
    answer = list(triggered = "asc_agg", refitted = "asc_agg")
  )
  plan <- list(sensitivity = list(student_t = list(trigger_stats = c("kurtosis", "min", "max"))))
  tail <- locate_planted_heavy_tail(truth, checks, plan)
  expect_identical(tail$statistics$stat, c("kurtosis", "min", "max"))
  expect_identical(tail$statistics$beyond, c(TRUE, FALSE, TRUE))
  expect_identical(tail$triggered, "asc_agg")
  expect_identical(tail$refitted, "asc_agg")
  # a minimum below its band counts
  checks$posterior_predictive$observed[3] <- -4.5
  expect_true(locate_planted_heavy_tail(truth, checks, plan)$statistics$beyond[2])
  checks$answer <- list(triggered = character(0), refitted = character(0))
  untriggered <- locate_planted_heavy_tail(truth, checks, plan)
  expect_identical(untriggered$triggered, character(0))
  expect_identical(untriggered$refitted, character(0))
  expect_null(locate_planted_heavy_tail(list(), checks, plan))
})

test_that("coefficients of opposite sign are found with their predicted signs", {
  comparison <- tibble::tibble(
    outcome = c("asc_conv", "sdo_dom", "asc_agg"), predictor = c("zm_prestige", "zm_prestige", "zm_power"),
    generating = c(-0.15, 0.15, 0.25), estimate = c(-0.09, 0.12, 0.22), lower = c(-0.17, 0.04, 0.14),
    upper = c(-0.01, 0.19, 0.29), excludes_zero = TRUE
  )
  plan <- list(predictions = list(table = list(asc_conv = list(zm_prestige = "±"), sdo_dom = list(zm_prestige = "±"),
                                               asc_agg = list(zm_prestige = "±", zm_power = "+"))))
  contrasts <- locate_planted_sign_contrasts(comparison, plan)
  expect_identical(contrasts$outcome, c("asc_conv", "sdo_dom"))
  expect_identical(contrasts$predicted_sign, c("±", "±"))
})

test_that("contingencies count checked cases and triggered rules", {
  flow <- list(steps = tibble::tibble(criterion = "dropped_unfillable_gaps", n_before = 700L, n_excluded = 0L))
  reliability <- list(n_scales = 9L, n_alpha_reported = 0L, n_omega_without_interval = 0L)
  cfa <- list(model_status = tibble::tibble(converged = c(TRUE, TRUE), admissible = c(TRUE, TRUE)))
  record <- function(role, status) list(fit_available = TRUE, role = role, gate_status = status)
  register <- list(record("primary", "ok"), record("sweep", "ok"),
                   list(fit_available = FALSE, role = "student_refit", gate_status = "unavailable"))
  joint <- list(validity = list(fit_valid = TRUE, gate_status = "ok"))
  network <- list(fits = list(list(succeeded = TRUE, attempts = tibble::tibble(seed = 1L)),
                              list(succeeded = TRUE, attempts = tibble::tibble(seed = 2L))))
  clarity <- list(loading_items = tibble::tibble(set_name = c("a", "a", "b"), factors = c(1L, 1L, 2L),
                                                 crossloading = FALSE))
  out <- check_contingencies_exercised(flow, reliability, cfa, register, joint, network, clarity)
  expect_identical(out$n_cases, rep(0L, 6))
  expect_identical(out$n_checked, c(700L, 9L, 2L, 3L, 2L, 2L))
  expect_identical(attr(out, "fits"), c(primary = 1L, sweep = 1L, student_refit = 0L))
  # A retried fit and a retried resample are counted as exercised cases.
  register[[2]]$gate_status <- "retried_ok"
  network$fits[[2]]$attempts <- tibble::tibble(seed = c(2L, 100002L))
  out <- check_contingencies_exercised(flow, reliability, cfa, register, joint, network, clarity)
  expect_identical(out$n_cases[out$contingency %in% c("regression_retry_or_failure", "network_retry_or_failure")],
                   c(1L, 1L))
})
