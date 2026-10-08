# LAYER 2 tests — the report targets of the Results sections Sample,
# Measurement and Adjusted associations (R/report_results_*.R): each fact the
# sections print, on small hand-made inputs with known answers.

local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) dir <- dirname(dir)
  for (f in c("config.R", "ap3_imputation_validity.R", "report_results_sample.R", "report_results_measurement.R",
              "report_results_associations.R", "report_imputation.R", "report_participation.R")) {
    source(file.path(dir, "R", f), local = FALSE)
  }
  assign("report_pilot_root", dir, envir = .GlobalEnv)
})

pilot_plan <- zm_config("smoke", file.path(report_pilot_root, "config", "analysis_plan.yaml"))
pilot_codebook <- zm_codebook(pilot_plan)

# --- Sample -------------------------------------------------------------------

pilot_preparation <- list(
  exclusions = tibble::tibble(
    step = 1:2, criterion = c("not_consenting", "gender_divers"),
    n_before = c(100L, 95L), n_excluded = c(5L, 3L), n_after = c(95L, 92L)
  ),
  n_started = 104L, n_quota_full = 4L,
  quota_group_computed = 4L, quota_group_mismatch = 1L
)
pilot_imputation <- list(excluded_participants = tibble::tibble(respondent_id = c(7L, 9L)))

test_that("the flow appends the drop of unfillable respondents and reconciles", {
  flow <- report_exclusion_steps(pilot_preparation, pilot_imputation)
  expect_identical(flow$steps$criterion, c("not_consenting", "gender_divers", "dropped_unfillable_gaps"))
  expect_identical(flow$steps$n_before[3], 92L)
  expect_identical(flow$steps$n_after[3], 90L)
  expect_identical(flow$n_started, 104L)
  expect_identical(flow$n_quota_full, 4L)
  expect_identical(flow$n_admitted, 100L)
  expect_identical(flow$n_excluded, 10L)
  expect_identical(flow$n_analysis, 90L)
  expect_identical(flow$n_admitted - flow$n_excluded, flow$n_analysis)
  expect_identical(flow$quota_cells_computed, 4L)
  expect_identical(flow$quota_cells_disagreeing, 1L)
})

test_that("the sample sizes name the shared regression sample and the gender omissions", {
  flow <- report_exclusion_steps(pilot_preparation, pilot_imputation)
  sizes <- report_analysis_sample_sizes(
    flow, list(n_used = 89L, omitted_gender = tibble::tibble(respondent_id = 3L)),
    data.frame(x = seq_len(90)))
  expect_identical(sizes, list(n_analysis = 90L, n_regressions = 89L,
                               n_regressions_by_outcome = integer(),
                               n_omitted_for_missing_gender = 1L, n_network = 90L,
                               network_unavailable_reason = NULL, regression_unavailable_reason = NULL))
})

test_that("the sample-characteristics rows carry counts or means, never both", {
  composition <- list(
    categorical = tibble::tibble(label = "Gender", level = c("female", "male"), n = c(50L, 40L),
                                 pct = c(55.6, 44.4)),
    numeric = tibble::tibble(label = "Age", n = 90L, mean = 44.1, sd = 12.3)
  )
  rows <- report_sample_characteristic_rows(composition)
  expect_identical(rows$group, c("female", "male", "M (SD)"))
  expect_true(all(is.na(rows$mean[1:2])) && all(is.na(rows$pct[3])))
  expect_equal(rows$mean[3], 44.1)
})

test_that("sample reporting facts save exclusions, participation and unresolved fills", {
  imputation <- list(excluded_participants = pilot_imputation$excluded_participants,
    imputed_cells = data.frame(variable = c("item", "demo_age")),
    n_unfilled = 1L, model_status = data.frame(status = "failed"))
  plan <- pilot_plan
  plan$recruitment$invited_n <- 200L
  flow <- add_sample_reporting_facts(report_exclusion_steps(pilot_preparation, imputation), imputation, plan)
  expect_identical(flow$exclusion_hits, c(not_consenting = TRUE, incomplete = FALSE,
    age_outside_range = FALSE, attention_1_wrong = FALSE, attention_2_wrong = FALSE))
  expect_equal(flow$n_excluded_gender_divers, 3)
  expect_identical(flow$imputation, list(n_filled = 2L, failed = TRUE, has_models = TRUE))
  expect_equal(flow$participation$proportion, 104 / 200)
  plan$recruitment$invited_n <- NULL
  flow <- add_sample_reporting_facts(report_exclusion_steps(pilot_preparation, imputation), imputation, plan)
  expect_identical(flow$participation, list(available = FALSE))
})

test_that("sample education rankings preserve ties and category rows", {
  composition <- list(categorical = tibble::tibble(
    variable = c("demo_gender", rep("demo_edu_school", 3)),
    label = c("Gender", rep("School education", 3)), level = c("female", "first", "second", "third"),
    n = c(40L, 10L, 20L, 20L), pct = c(40, 20, 40, 40)),
    numeric = tibble::tibble(label = "Age", n = 90L, mean = 44.1, sd = 12.3))
  rows <- report_sample_characteristic_rows(composition)
  expect_identical(rows$group, c("female", "first", "second", "third", "M (SD)"))
  expect_identical(attr(rows, "education_rank")$level, c("second", "third", "first"))
  expect_identical(attr(rows, "education_rank")$pct, c(40, 40, 20))
})

# --- Measurement ----------------------------------------------------------------

pilot_reliability <- list(
  reported = tibble::tibble(
    scale_key = c("a", "b", "c"), n_items = c(6L, 6L, 5L), n = 90L,
    omega_t = c(0.8, 0.7, NA), alpha = c(0.78, 0.69, 0.6),
    reported_method = c("omega", "omega", "alpha"), estimate = c(0.8, 0.7, 0.6),
    interval_lower = c(0.75, NA_real_, 0.5), interval_upper = c(0.85, NA_real_, 0.7),
    interval_source = c("omega_bootstrap", "unavailable_omega_bootstrap",
                        "alpha_fallback_omega_model_failed")
  ),
  bootstrap = list(
    a = list(alpha_interval = c(0.72, 0.83)),
    b = list(alpha_interval = c(0.62, 0.76)),
    c = list(alpha_interval = c(0.5, 0.7))
  )
)

test_that("the reliability table keeps the reported interval and its reason apart from alpha's", {
  table <- report_reliability_by_scale(pilot_reliability)
  expect_identical(table$reported_interval_source[2], "unavailable_omega_bootstrap")
  expect_equal(table$alpha_interval_lower, c(0.72, 0.62, 0.5))
  expect_equal(table$reported_interval_lower, c(0.75, NA_real_, 0.5))
})

test_that("the measurement counts separate alpha reported from omega without an interval", {
  counts <- report_count_reliability_substitutions(report_reliability_by_scale(pilot_reliability))
  expect_identical(counts$n_scales, 3L)
  expect_identical(counts$n_alpha_reported, 1L)
  expect_identical(counts$n_omega_without_interval, 1L)
  expect_equal(counts$omega_range, c(0.7, 0.8))
})

# --- Adjusted associations --------------------------------------------------------

pilot_cells <- tibble::tibble(
  outcome_key = c("asc_agg", "asc_agg", "sdo_dom"),
  outcome = c("authoritarian_aggression", "authoritarian_aggression", "social_dominance_orientation"),
  predictor = c("zm_security", "zm_power", "zm_power"),
  predicted_sign = c("+", "", "+"),
  estimate = c(0.10, -0.30, 0.20),
  interval_lower = c(0.001, -0.40, 0.05),
  interval_upper = c(0.20, -0.20, 0.35),
  fit_valid = c(TRUE, TRUE, FALSE),
  credible = c(TRUE, TRUE, NA),
  observed_sign = c("+", "-", "+"),
  verdict = c("confirmed", "exploratory_credible", NA),
  n = 89L
)

pilot_stability <- function(stable_intim = TRUE, stable_dom = TRUE) {
  cell <- function(outcome, coefficient, stable, medians = c(0.1, 0.1, 0.1),
                   excludes = c(TRUE, TRUE, TRUE)) {
    by_width <- stats::setNames(lapply(1:3, function(i) list(
      median = medians[i], interval = c(0, 1), interval_excludes_zero = excludes[i])),
      c("0.10", "0.20", "0.40"))
    list(outcome = outcome, coefficient = coefficient, by_width = by_width,
         interval_exclusion_stable = stable, median_direction_stable = all(medians > 0) || all(medians < 0))
  }
  list(
    list(cell("authoritarian_aggression", "b_zm_security_z", stable_intim,
              excludes = if (stable_intim) c(TRUE, TRUE, TRUE) else c(FALSE, TRUE, TRUE)),
         cell("authoritarian_aggression", "b_zm_power_z", stable_dom, medians = c(-0.3, -0.3, -0.3))),
    list(cell("social_dominance_orientation", "b_zm_power_z", TRUE, medians = c(-0.01, 0.2, 0.2)))
  )
}
pilot_register <- list(
  list(role = "primary", outcome_key = "asc_agg", fit_valid = TRUE),
  list(role = "sweep", outcome_key = "asc_agg", fit_valid = TRUE),
  list(role = "primary", outcome_key = "sdo_dom", fit_valid = TRUE),
  list(role = "sweep", outcome_key = "sdo_dom", fit_valid = FALSE),
  list(role = "student_refit", outcome_key = "sdo_dom", fit_valid = FALSE)
)

test_that("the cells are ordered by the regression outcomes, then the motives", {
  shuffled <- report_prediction_cells(
    dplyr::mutate(pilot_cells[c(3, 2, 1), ], q_lo = interval_lower, q_hi = interval_upper),
    list(n_regressions = 89L), pilot_plan)
  expect_identical(shuffled$outcome_key[1:2], c("asc_agg", "asc_agg"))
  expect_identical(shuffled$predictor[1:2], c("zm_security", "zm_power"))
})

test_that("prior robustness is withheld when any width's fit of the outcome failed the gate", {
  robust <- report_prior_width_robustness(pilot_cells, pilot_stability(stable_intim = FALSE),
                                          pilot_register, pilot_codebook)
  expect_identical(robust$prior_robust, c(FALSE, TRUE, NA))
})

test_that("the counts and the credible cells of each facet come from the table", {
  table <- report_prior_width_robustness(pilot_cells,
                                         pilot_stability(), pilot_register, pilot_codebook)
  counts <- report_count_prediction_verdicts(table)
  expect_identical(counts$n_directional, 2L)
  expect_identical(counts$n_directional_withheld, 1L)
  expect_identical(counts$n_prior_comparable, 2L)
  expect_identical(counts$n_prior_dependent, 0L)
  credible <- report_credible_cells_by_outcome(table)
  expect_identical(credible$interpretable, c(TRUE, FALSE))
  # in the table's order, which is the codebook's order of the motives
  expect_identical(credible$predictor[[1]], c("zm_security", "zm_power"))
})

test_that("only the cells whose classification or direction changes are listed, with their widths", {
  changes <- report_cells_changing_with_prior_width(pilot_cells, pilot_stability(stable_intim = FALSE),
                                                    pilot_codebook)
  expect_identical(paste(changes$outcome_key, changes$predictor),
                   c("asc_agg zm_security", "sdo_dom zm_power"))
  expect_identical(changes$widths_including_zero[[1]], "0.10")
  expect_identical(changes$widths_negative_median[[2]], "0.10")
})
