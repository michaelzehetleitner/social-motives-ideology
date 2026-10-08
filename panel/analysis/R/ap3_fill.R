# AP3 missing-value imputation (once, before the scale scores)
# Implements the missing-data processing of AP3: every
# missing value is imputed ONCE, outside the models, before the scale scores, so
# that every model reads the same table. A respondent with more than the
# configured number of missing items in one scale, or with at least the
# configured number of the three demographics missing, is excluded from all
# analyses instead of being imputed
# (exclude_participants_with_high_missingness()). Items are imputed by one
# ordinal item model per scale that has a gap (impute_items()); age,
# household size and income band are imputed by one regression each, in that
# order, each using the demographics imputed before it (impute_age(),
# impute_household_size(), impute_income_band()), from the predictor
# sets select_demographic_imputation_predictors() decides once. Gender is
# never imputed: a respondent without gender keeps the missing value and is not
# excluded for it; both the item and the demographic imputation reach that row.
#
# The imputed value per cell and the posterior predictive interval reported
# beside it at the configured `missing_data$fill$interval` level are recorded in
# the attribute `imputed_cells`; the excluded
# participants and the threshold each exceeded travel beside the retained data.
# build_imputation_reporting_data() assembles both into the reporting object.
# The interval is report-only; nothing downstream reads it. The exclusion
# removes respondents from every analysis, so it also enters the participant
# flow as a step of its own (report_exclusion_steps()).
#
# The input is the wide respondent frame after reverse_items() and the
# gender factor: one row per respondent, `respondent_id`, the item columns in
# one keying direction, `gender`, `demo_age`, `demo_hh_members` (1..8) and
# `demo_income_hh_net` (band 1..13). The scale scores the demographic
# regressions use are computed on the fly from the imputed items with
# average_items_into_subscales() (raw means, not z); they are not written
# into the frame.
#
# Every numeric decision comes from analysis_plan (config/analysis_plan.yaml,
# `missing_data$fill`): the prior widths, the interval, the drop thresholds, the
# response bounds and the sampler settings. The four brms formulas are written
# out literally in the fitting functions below, and each prior string is built
# there from the width the plan states, as in fit_primary_regressions(); the formulas in
# the configuration mirror them for the fill cards. One card stands above each
# of the four verbs: the route card above the exclusion, one model card above
# each fit.
#
# Depends on R/config.R (zm_*) and R/ap3_preparation.R
# (average_items_into_subscales()).

# BEGIN GENERATED PARAMETER CARD: AP3 FILL ROUTE
# Automatically generated from analysis_plan.yaml
# Rule: one fill before the scale scores; one value per empty cell, the same table for every model.
# Dropped from all analyses instead of filled: more than 2 missing items in one scale, or at least 2
#   of the three demographics missing.
# Order: the drop, then the items of every scale with a gap, then demo_age, demo_hh_members,
#   demo_income_hh_net.
# Gender: never filled; rows without gender are omitted from the four regressions and the joint
#   model only.
# END GENERATED PARAMETER CARD: AP3 FILL ROUTE
#' AP3 — exclude the participants the imputation must not reach
#'
#' A respondent with more than `analysis_plan$missing_data$fill$drop$items_per_scale_more_than`
#' missing items in any one scale, or with at least
#' `analysis_plan$missing_data$fill$drop$demographics_missing_at_least` of the three
#' demographics missing, is removed before any fit. Gender is not a demographic
#' in this sense: it is never imputed and never a reason to exclude.
#'
#' @param data Wide respondent frame (see the section comment).
#' @param codebook Codebook from [zm_codebook()].
#' @param analysis_plan Configuration from [zm_config()].
#' @return `list(data, reporting_data)`: the retained frame, and one row per
#'   excluded participant with the threshold(s) it exceeded. Each threshold
#'   keeps its own detail field beside its flag: `scales_over_threshold` names
#'   the scales in codebook order, `n_demographics_missing` counts the three.
exclude_participants_with_high_missingness <- function(data, codebook, analysis_plan) {
  max_items <- analysis_plan$missing_data$fill$drop$items_per_scale_more_than
  min_demographics <- analysis_plan$missing_data$fill$drop$demographics_missing_at_least
  # max_items = 2: a maximum above 2 means 3+ missing items in one scale.
  item_counts <- sapply(codebook$scales$item_codes, function(items) {
    rowSums(is.na(data[items]))
  })
  colnames(item_counts) <- codebook$scales$scale_key
  over_threshold <- item_counts > max_items
  too_many_items_missing <- apply(over_threshold, 1, any)
  # The scales a participant exceeded, in codebook order; NA when none.
  scales_over_threshold <- apply(over_threshold, 1, function(over) {
    scales <- codebook$scales$scale_key[over]
    if (length(scales) == 0L) NA_character_ else paste(scales, collapse = ", ")
  })
  # min_demographics = 2: count only age, household size, and income band.
  demographics <- c("demo_age", "demo_hh_members", "demo_income_hh_net")
  n_demographics_missing <- rowSums(is.na(data[demographics]))
  too_many_demographics_missing <- n_demographics_missing >= min_demographics
  excluded <- too_many_items_missing | too_many_demographics_missing
  kept <- data[!excluded, , drop = FALSE]
  attr(kept, "reversed_items") <- attr(data, "reversed_items", exact = TRUE)
  list(
    data = kept,
    reporting_data = tibble::tibble(
      respondent_id = data$respondent_id[excluded],
      too_many_items_missing = too_many_items_missing[excluded],
      scales_over_threshold = scales_over_threshold[excluded],
      too_many_demographics_missing = too_many_demographics_missing[excluded],
      n_demographics_missing = as.integer(n_demographics_missing[excluded]))
  )
}

#' AP3 — the ordered predictor sets of the three demographic imputation models
#'
#' If gender is observed for every retained respondent, all three models use it;
#' if any retained respondent has a missing gender, all three omit it. The
#' models run in the order age, household size, income band, each adding the
#' demographics imputed before it. Gender itself is never imputed.
#'
#' @inheritParams exclude_participants_with_high_missingness
#' @return `list(demo_age, demo_hh_members, demo_income_hh_net)` of predictor
#'   column names.
select_demographic_imputation_predictors <- function(data, codebook, analysis_plan) {
  scale_scores <- codebook$scales$scale_key
  shared_predictors <- if (all(data$known_gender)) {
    c(scale_scores, "gender")   # gender observed for everyone: it is a predictor
  } else {
    scale_scores                # any gender missing: gender omitted from all three
  }
  list(
    demo_age = shared_predictors,
    demo_hh_members = c(shared_predictors, "demo_age"),
    demo_income_hh_net = c(shared_predictors, "demo_age", "demo_hh_members")
  )
}

# BEGIN GENERATED PARAMETER CARD: AP3 FILL ITEMS
# Automatically generated from analysis_plan.yaml
# One fit per scale with at least one gap.
# Formula:
#   answer ~ 1 + (1 | item) + (1 | respondent_id)
# Family: cumulative(logit).
# Response categories: 1 to 6   (scales.response_min, scales.response_max).
# Priors: intercept normal(0, 1.5); sd normal(0, 1).
# Value per cell: posterior median of the expected answer (fractional allowed).
# Interval per filled cell: 95% posterior predictive.
# END GENERATED PARAMETER CARD: AP3 FILL ITEMS
#' AP3 — impute the items of every scale that has a gap
#'
#' A scale without a gap is passed over unfitted. Otherwise one ordinal item
#' model per scale with a gap, fitted on the answered cells of that scale; the
#' imputed value is the posterior median of the expected answer and may be
#' fractional. The imputed-cell record is initialised with its full schema
#' before the loop, so a run in which no cell needs imputation still hands on a
#' typed empty record.
#'
#' @inheritParams exclude_participants_with_high_missingness
#' @return `data` with every registered scale-item cell observed and the
#'   appended `imputed_cells` rows.
impute_items <- function(data, codebook, analysis_plan) {
  priors <- analysis_plan$missing_data$fill$items$priors
  # The typed empty record every later bind_rows() and the reporting builder see.
  attr(data, "imputed_cells") <- ap3_fill_empty_cells()
  attr(data, "imputation_model_status") <- create_empty_imputation_model_status()
  imputation_specifications <- ap3_item_imputation_specifications(data, codebook, analysis_plan)
  for (specification in imputation_specifications) {
    observed_item_answers <- ap3_observed_item_answers(
      data, specification, specification$categories)
    result <- gate_imputation_fit(brms::brm(
      answer ~ 1 + (1 | item) + (1 | respondent_id),
      data = observed_item_answers, family = brms::cumulative("logit"),
      prior = c(
        brms::set_prior(paste0("normal(0, ", priors$intercept_sd, ")"), class = "Intercept"),
        brms::set_prior(paste0("normal(0, ", priors$sd_sd, ")"), class = "sd")),
      backend = "cmdstanr", chains = specification$sampling$chains,
      cores = specification$sampling$cores, seed = specification$sampling$seed,
      iter = specification$sampling$iter, warmup = specification$sampling$warmup,
      thin = specification$sampling$thin, refresh = specification$sampling$refresh,
      silent = specification$sampling$silent), specification$sampling, analysis_plan)
    data <- record_imputation_model_status(data, result, specification$scale, "item",
      specification$items, nrow(specification$missing_item_cells))
    fit <- result$fit
    if (is.null(fit)) next
    missing_item_cells <- specification$missing_item_cells
    imputed_answers <- ap3_posterior_median_expected_answers(
      fit, missing_item_cells)
    data <- ap3_add_item_imputations(
      data, missing_item_cells, imputed_answers)
    data <- ap3_add_item_imputation_reporting(
      data, fit, missing_item_cells, imputed_answers,
      specification$probabilities, specification$scale)
  }
  data <- identify_unavailable_imputation_variables(data, codebook)
  check_all_scale_items_imputed(data, codebook)
}

# BEGIN GENERATED PARAMETER CARD: AP3 FILL AGE
# Automatically generated from analysis_plan.yaml
# Formula:
#   demo_age ~ zm_security + zm_arousal + zm_power + zm_prestige + zm_achievement + asc_agg +
#     asc_sub + asc_conv + sdo_dom + gender (only when observed for every retained participant)
# Family: gaussian.
# Priors: intercept normal(45, 20); b normal(0, 5); sigma normal(0, 15).
# Value per cell: posterior median of the expected value.
# Interval per filled cell: 95% posterior predictive.
# END GENERATED PARAMETER CARD: AP3 FILL AGE
#' AP3 — impute age
#'
#' Pass-through when `demo_age` has no missing value. Otherwise one gaussian
#' regression on the predictor set decided upstream, fitted on the rows with an
#' observed age; the imputed value is the posterior median of the expected
#' value.
#'
#' @inheritParams exclude_participants_with_high_missingness
#' @param predictors Predictor columns from
#'   [select_demographic_imputation_predictors()].
#' @return `data` with the imputed ages and the appended `imputed_cells` rows.
impute_age <- function(data, codebook, analysis_plan, predictors) {
  priors <- analysis_plan$missing_data$fill$demographics$demo_age$priors
  specification <- prepare_demographic_imputation_cases(ap3_age_imputation_specification(
    data, codebook, analysis_plan, predictors))
  if (!specification$needed) return(data)
  data <- record_blocked_imputation_predictions(data, specification)
  blocked <- record_blocked_demographic_imputation(data, specification)
  if (!is.null(blocked)) return(identify_unavailable_imputation_variables(blocked, codebook))
  result <- gate_imputation_fit(brms::brm(specification$formula, data = specification$observed_values, family = stats::gaussian(),
    prior = c(
      brms::set_prior(paste0("normal(", priors$intercept_mean, ", ", priors$intercept_sd, ")"), class = "Intercept"),
      brms::set_prior(paste0("normal(0, ", priors$b_sd, ")"), class = "b"),
      brms::set_prior(paste0("normal(0, ", priors$sigma_sd, ")"), class = "sigma")),
    backend = "cmdstanr", chains = specification$sampling$chains, cores = specification$sampling$cores,
    seed = specification$sampling$seed, iter = specification$sampling$iter, warmup = specification$sampling$warmup),
    specification$sampling, analysis_plan)
  data <- record_imputation_model_status(data, result, specification$variable, "demographic",
    specification$variable, specification$n_missing, cases = specification)
  fit <- result$fit
  if (is.null(fit)) return(identify_unavailable_imputation_variables(data, codebook))
  imputed_ages <- ap3_posterior_median_expected_values(
    fit, specification$participants_with_missing_value)
  data <- ap3_add_demographic_imputations(
    data, specification, imputed_ages)
  data <- ap3_add_demographic_imputation_reporting(
    data, fit, specification, imputed_ages)
  identify_unavailable_imputation_variables(data, codebook)
}

# BEGIN GENERATED PARAMETER CARD: AP3 FILL HOUSEHOLD SIZE
# Automatically generated from analysis_plan.yaml
# Formula:
#   demo_hh_members ~ zm_security + zm_arousal + zm_power + zm_prestige + zm_achievement + asc_agg +
#     asc_sub + asc_conv + sdo_dom + gender (only when observed for every retained participant) +
#     demo_age
# Family: poisson.
# Priors: intercept normal(0, 1.5); b normal(0, 0.5).
# Value per cell: posterior median of the expected count, rounded, kept within 1..8.
# Interval per filled cell: 95% posterior predictive.
# END GENERATED PARAMETER CARD: AP3 FILL HOUSEHOLD SIZE
#' AP3 — impute household size
#'
#' Pass-through when `demo_hh_members` has no missing value. Otherwise one
#' poisson regression on the predictor set decided upstream, which adds the
#' imputed age; the imputed value is the posterior median of the expected count,
#' rounded and kept within the configured range.
#'
#' @inheritParams impute_age
#' @return `data` with the imputed household sizes and the appended
#'   `imputed_cells` rows.
impute_household_size <- function(data, codebook, analysis_plan, predictors) {
  priors <- analysis_plan$missing_data$fill$demographics$demo_hh_members$priors
  specification <- prepare_demographic_imputation_cases(ap3_household_imputation_specification(
    data, codebook, analysis_plan, predictors))
  if (!specification$needed) return(data)
  data <- record_blocked_imputation_predictions(data, specification)
  blocked <- record_blocked_demographic_imputation(data, specification)
  if (!is.null(blocked)) return(identify_unavailable_imputation_variables(blocked, codebook))
  result <- gate_imputation_fit(brms::brm(specification$formula, data = specification$observed_values, family = stats::poisson(),
    prior = c(brms::set_prior(paste0("normal(0, ", priors$intercept_sd, ")"), class = "Intercept"),
      brms::set_prior(paste0("normal(0, ", priors$b_sd, ")"), class = "b")),
    backend = "cmdstanr", chains = specification$sampling$chains, cores = specification$sampling$cores,
    seed = specification$sampling$seed, iter = specification$sampling$iter, warmup = specification$sampling$warmup),
    specification$sampling, analysis_plan)
  data <- record_imputation_model_status(data, result, specification$variable, "demographic",
    specification$variable, specification$n_missing, cases = specification)
  fit <- result$fit
  if (is.null(fit)) return(identify_unavailable_imputation_variables(data, codebook))
  imputed_household_sizes <- round(ap3_posterior_median_expected_values(
    fit, specification$participants_with_missing_value))
  imputed_household_sizes <- pmin(specification$range[[2]], pmax(specification$range[[1]], imputed_household_sizes))
  data <- ap3_add_demographic_imputations(
    data, specification, imputed_household_sizes)
  data <- ap3_add_demographic_imputation_reporting(
    data, fit, specification, imputed_household_sizes)
  identify_unavailable_imputation_variables(data, codebook)
}

# BEGIN GENERATED PARAMETER CARD: AP3 FILL INCOME BAND
# Automatically generated from analysis_plan.yaml
# Formula:
#   demo_income_hh_net ~ zm_security + zm_arousal + zm_power + zm_prestige + zm_achievement +
#     asc_agg + asc_sub + asc_conv + sdo_dom + gender (only when observed for every retained
#     participant) + demo_age + demo_hh_members
# Family: cumulative(logit).
# Priors: intercept normal(0, 1.5); b normal(0, 0.5).
# Value per cell: median band of the posterior predictive draws.
# Interval per filled cell: 95% posterior predictive.
# END GENERATED PARAMETER CARD: AP3 FILL INCOME BAND
#' AP3 — impute the income band
#'
#' Pass-through when `demo_income_hh_net` has no missing value. Otherwise one
#' cumulative-logit regression on the predictor set decided upstream, which adds
#' the imputed age and household size, with the band as an ordered outcome; the
#' imputed value is the median band of the posterior predictive draws. Its final
#' call establishes that age, household size and income are complete.
#'
#' @inheritParams impute_age
#' @return `data` with the imputed income bands and the appended
#'   `imputed_cells` rows.
impute_income_band <- function(data, codebook, analysis_plan, predictors) {
  priors <- analysis_plan$missing_data$fill$demographics$demo_income_hh_net$priors
  specification <- prepare_demographic_imputation_cases(ap3_income_imputation_specification(
    data, codebook, analysis_plan, predictors))
  if (!specification$needed)
    return(check_demographic_imputations_complete(data))
  data <- record_blocked_imputation_predictions(data, specification)
  blocked <- record_blocked_demographic_imputation(data, specification)
  if (!is.null(blocked)) return(identify_unavailable_imputation_variables(blocked, codebook))
  result <- gate_imputation_fit(brms::brm(specification$formula, data = specification$observed_values, family = brms::cumulative("logit"),
    prior = c(brms::set_prior(paste0("normal(0, ", priors$intercept_sd, ")"), class = "Intercept"),
      brms::set_prior(paste0("normal(0, ", priors$b_sd, ")"), class = "b")),
    backend = "cmdstanr", chains = specification$sampling$chains, cores = specification$sampling$cores,
    seed = specification$sampling$seed, iter = specification$sampling$iter, warmup = specification$sampling$warmup),
    specification$sampling, analysis_plan)
  data <- record_imputation_model_status(data, result, specification$variable, "demographic",
    specification$variable, specification$n_missing, cases = specification)
  fit <- result$fit
  if (is.null(fit)) return(identify_unavailable_imputation_variables(data, codebook))
  imputed_income_bands <- ap3_posterior_median_income_bands(
    fit, specification$participants_with_missing_value, specification$band_codes)
  data <- ap3_add_demographic_imputations(
    data, specification, imputed_income_bands)
  data <- ap3_add_demographic_imputation_reporting(
    data, fit, specification, imputed_income_bands)
  data <- identify_unavailable_imputation_variables(data, codebook)
  check_demographic_imputations_complete(data)
}

#' AP3 — the missing-data-processing facts, assembled for reporting
#'
#' Reporting boundary: both inputs are already-calculated facts — the
#' imputed-cell record the imputation verbs attached, and the participants the
#' excessive-missingness rule excluded. No threshold, fit, imputed value or
#' analysis row is chosen here.
#'
#' @param study_imputed Frame returned by [impute_income_band()].
#' @param excluded_participants `reporting_data` of
#'   [exclude_participants_with_high_missingness()].
#' @return `list(imputed_cells, excluded_participants)`.
build_imputation_reporting_data <- function(study_imputed, excluded_participants) {
  models <- attr(study_imputed, "imputation_model_status", exact = TRUE)
  if (is.null(models)) models <- create_empty_imputation_model_status()
  cells <- tibble::as_tibble(attr(study_imputed, "imputed_cells"))
  models$n_filled <- vapply(models$model, function(model) sum(cells$model == model), integer(1),
    USE.NAMES = FALSE)
  models$n_unfilled <- models$n_missing - models$n_filled
  unresolved_variables <- intersect(extract_unresolved_imputation_variables(study_imputed), names(study_imputed))
  unfilled <- dplyr::bind_rows(lapply(unresolved_variables, function(variable) {
    tibble::tibble(respondent_id = study_imputed$respondent_id[is.na(study_imputed[[variable]])],
      variable = variable)
  }))
  if (!length(unresolved_variables)) unfilled <- tibble::tibble(
    respondent_id = study_imputed$respondent_id[FALSE], variable = character())
  blocked_predictions <- attr(study_imputed, "blocked_imputation_predictions", exact = TRUE)
  if (is.null(blocked_predictions)) blocked_predictions <- create_empty_blocked_imputation_predictions()
  list(
    imputed_cells = cells,
    excluded_participants = tibble::as_tibble(excluded_participants),
    model_status = tibble::as_tibble(models),
    blocked_predictions = blocked_predictions,
    participants = tibble::tibble(respondent_id = study_imputed$respondent_id,
      known_gender = if ("gender" %in% names(study_imputed)) !is.na(study_imputed$gender) else TRUE),
    unfilled_cells = unfilled,
    unavailable_variables = attr(study_imputed, "unavailable_imputation_variables", exact = TRUE),
    unavailable_known_gender_variables = attr(study_imputed,
      "unavailable_imputation_variables_known_gender", exact = TRUE),
    n_unfilled = nrow(unfilled)
  )
}

# Imputation mechanics ---------------------------------------------------------
# One operation each, read without the plan's values: every threshold, width,
# category and probability arrives as an argument from the body above.

#' The item-imputation specification of every scale that has a gap
#'
#' Finds the missing cells scale by scale, prepares the response categories, the
#' interval probabilities and the sampling controls, and returns a specification
#' only for the scales that contain a missing response.
#'
#' @inheritParams exclude_participants_with_high_missingness
#' @return List of specifications.
ap3_item_imputation_specifications <- function(data, codebook, analysis_plan) {
  categories <- seq(analysis_plan$scales$response_min, analysis_plan$scales$response_max)
  interval <- analysis_plan$missing_data$fill$interval
  probabilities <- c((1 - interval) / 2, 1 - (1 - interval) / 2)
  sampling <- ap3_imputation_sampling(analysis_plan)
  specifications <- vector("list", nrow(codebook$scales))
  for (i in seq_len(nrow(codebook$scales))) {
    scale <- codebook$scales$scale_key[[i]]
    items <- codebook$scales$item_codes[[i]]
    gaps <- which(is.na(as.matrix(data[items])), arr.ind = TRUE)
    if (nrow(gaps) > 0L) specifications[[i]] <- list(
      scale = scale, items = items, categories = categories,
      probabilities = probabilities, sampling = sampling, missing_item_cells = data.frame(
        respondent_id = data$respondent_id[gaps[, "row"]],
        item = items[gaps[, "col"]]))
  }
  Filter(Negate(is.null), specifications)
}

#' The observed answers of one scale, one model row per respondent–item answer
#'
#' @inheritParams exclude_participants_with_high_missingness
#' @param specification One entry of [ap3_item_imputation_specifications()].
#' @param categories Response categories, in order.
#' @return Tibble `respondent_id`, `item`, ordered `answer`.
ap3_observed_item_answers <- function(data, specification, categories) {
  tidyr::pivot_longer(
    dplyr::select(data, respondent_id, dplyr::all_of(specification$items)),
    cols = dplyr::all_of(specification$items),
    names_to = "item", values_to = "answer") |>
    dplyr::filter(!is.na(.data$answer)) |>
    dplyr::mutate(
      item = factor(.data$item, levels = specification$items),
      answer = ordered(.data$answer, levels = categories))
}

#' The posterior median expected answer of every missing item cell
#'
#' Turns each posterior category-probability draw into an expected response and
#' returns its posterior median.
#'
#' @param fit A brms fit.
#' @param missing_item_cells One row per cell to impute.
#' @return Numeric vector, one value per cell.
ap3_posterior_median_expected_answers <- function(fit, missing_item_cells) {
  categories <- as.numeric(levels(model.frame(fit)$answer))
  probabilities <- brms::posterior_epred(fit, newdata = missing_item_cells)
  expected <- apply(probabilities, c(1, 2), function(draw) {
    sum(draw * categories)
  })
  apply(expected, 2, stats::median)
}

#' Write the already-chosen imputed responses into their cells
#'
#' @param data Wide respondent frame.
#' @param missing_item_cells One row per cell to impute.
#' @param imputed_answers Value of each imputed cell.
#' @return `data` with the cells written.
ap3_add_item_imputations <- function(data, missing_item_cells, imputed_answers) {
  for (i in seq_len(nrow(missing_item_cells))) {
    item <- missing_item_cells$item[[i]]
    row <- match(missing_item_cells$respondent_id[[i]], data$respondent_id)
    data[[item]][row] <- imputed_answers[[i]]
  }
  data
}

#' One reporting row per imputed item cell
#'
#' One fit per scale serves every gap of that scale, so the model of an item
#' cell is named by that scale: the filled-cell display labels the scale key
#' as it labels every other scale, and the demographic rows name their
#' variable in the same column.
#'
#' @inheritParams ap3_add_item_imputations
#' @param fit A brms fit.
#' @param scale Scale key of the fit that served these cells.
#' @param probabilities The two interval bounds of the specification, from the
#'   configured `missing_data$fill$interval`.
#' @return `data` with the rows appended to its `imputed_cells` attribute.
ap3_add_item_imputation_reporting <- function(data, fit, missing_item_cells, imputed_answers,
                                              probabilities, scale) {
  predictive <- brms::posterior_predict(fit, newdata = missing_item_cells)
  interval <- apply(predictive, 2, stats::quantile, probs = probabilities)
  reporting <- tibble::tibble(
    respondent_id = missing_item_cells$respondent_id,
    variable = missing_item_cells$item, kind = "item",
    model = as.character(scale), value = imputed_answers,
    lower = interval[1, ], upper = interval[2, ],
    n_fit_rows = stats::nobs(fit))
  attr(data, "imputed_cells") <- dplyr::bind_rows(
    attr(data, "imputed_cells", exact = TRUE), reporting)
  data
}

#' AP3 postcondition — every registered scale item is present and observed
#'
#' Established once, immediately after item imputation. Later scoring,
#' reliability and CFA rely on this guarantee.
#'
#' @param data Frame returned by the item imputation.
#' @param codebook Codebook from [zm_codebook()].
#' @return `data`.
check_all_scale_items_imputed <- function(data, codebook) {
  items <- unique(unlist(codebook$scales$item_codes, use.names = FALSE))
  absent <- setdiff(items, names(data))
  if (length(absent))
    stop("AP3: registered scale item(s) are absent: ",
      paste(absent, collapse = ", "), ".")
  missing_cells <- which(is.na(as.matrix(data[items])), arr.ind = TRUE)
  unexplained <- has_unexplained_imputation_cells(data, items)
  if (unexplained)
    stop("AP3: item imputation left ", nrow(missing_cells),
      " registered scale-item value(s) missing.")
  data
}

#' The age-imputation specification
#'
#' @inheritParams impute_age
#' @return List with `needed`, `variable`, `formula`, `observed_values`,
#'   `participants_with_missing_value`, `respondent_id`, `probabilities` and
#'   `sampling`.
ap3_age_imputation_specification <- function(data, codebook, analysis_plan, predictors) {
  persons <- average_items_into_subscales(data, codebook)
  interval <- analysis_plan$missing_data$fill$interval
  missing <- is.na(persons$demo_age)
  list(needed = any(missing), variable = "demo_age",
    formula = stats::reformulate(predictors, response = "demo_age"),
    observed_values = persons[!missing, c("demo_age", predictors), drop = FALSE],
    participants_with_missing_value = persons[missing, predictors, drop = FALSE],
    respondent_id = persons$respondent_id[missing],
    probabilities = c((1 - interval) / 2, 1 - (1 - interval) / 2),
    sampling = ap3_imputation_sampling(analysis_plan))
}

#' The household-size imputation specification
#'
#' @inheritParams impute_household_size
#' @return As [ap3_age_imputation_specification()], plus the configured `range`.
ap3_household_imputation_specification <- function(data, codebook, analysis_plan, predictors) {
  persons <- average_items_into_subscales(data, codebook)
  interval <- analysis_plan$missing_data$fill$interval
  missing <- is.na(persons$demo_hh_members)
  list(needed = any(missing), variable = "demo_hh_members",
    formula = stats::reformulate(predictors, response = "demo_hh_members"),
    observed_values = persons[!missing, c("demo_hh_members", predictors), drop = FALSE],
    participants_with_missing_value = persons[missing, predictors, drop = FALSE],
    respondent_id = persons$respondent_id[missing],
    probabilities = c((1 - interval) / 2, 1 - (1 - interval) / 2),
    range = analysis_plan$missing_data$fill$demographics$demo_hh_members$range,
    sampling = ap3_imputation_sampling(analysis_plan))
}

#' The income-band imputation specification
#'
#' @inheritParams impute_income_band
#' @return As [ap3_age_imputation_specification()], plus the ordered
#'   `band_codes`.
ap3_income_imputation_specification <- function(data, codebook, analysis_plan, predictors) {
  persons <- average_items_into_subscales(data, codebook)
  interval <- analysis_plan$missing_data$fill$interval
  band_codes <- seq_len(analysis_plan$missing_data$fill$demographics$demo_income_hh_net$bands)
  persons$demo_income_hh_net <- ordered(persons$demo_income_hh_net, levels = band_codes)
  missing <- is.na(persons$demo_income_hh_net)
  list(needed = any(missing), variable = "demo_income_hh_net",
    formula = stats::reformulate(predictors, response = "demo_income_hh_net"),
    observed_values = persons[!missing, c("demo_income_hh_net", predictors), drop = FALSE],
    participants_with_missing_value = persons[missing, predictors, drop = FALSE],
    respondent_id = persons$respondent_id[missing], band_codes = band_codes,
    probabilities = c((1 - interval) / 2, 1 - (1 - interval) / 2),
    sampling = ap3_imputation_sampling(analysis_plan))
}

#' The per-participant posterior median expected value
#'
#' @param fit A brms fit.
#' @param newdata One row per participant with a missing value.
#' @return Numeric vector.
ap3_posterior_median_expected_values <- function(fit, newdata) {
  expected <- brms::posterior_epred(fit, newdata = newdata)
  apply(expected, 2, stats::median)
}

#' The median of the ordered income bands, per participant
#'
#' @inheritParams ap3_posterior_median_expected_values
#' @param band_codes The ordered band codes.
#' @return Numeric vector of band codes.
ap3_posterior_median_income_bands <- function(fit, newdata, band_codes) {
  draws <- brms::posterior_predict(fit, newdata = newdata)
  apply(draws, 2, function(x) {
    cumulative <- cumsum(tabulate(as.integer(x), nbins = length(band_codes)))
    band_codes[which(cumulative >= length(x) / 2)[1L]]
  })
}

#' Write the already-chosen imputed demographic values into their column
#'
#' @param data Wide respondent frame.
#' @param specification The verb's imputation specification.
#' @param values Value of each imputed cell.
#' @return `data` with the cells written.
ap3_add_demographic_imputations <- function(data, specification, values) {
  rows <- match(specification$respondent_id, data$respondent_id)
  data[[specification$variable]][rows] <- values
  data
}

#' One reporting row per imputed demographic cell
#'
#' @inheritParams ap3_add_demographic_imputations
#' @param fit A brms fit.
#' @return `data` with the rows appended to its `imputed_cells` attribute.
ap3_add_demographic_imputation_reporting <- function(data, fit, specification, values) {
  predictive <- brms::posterior_predict(fit, newdata = specification$participants_with_missing_value)
  interval <- apply(predictive, 2, stats::quantile, probs = specification$probabilities)
  reporting <- tibble::tibble(respondent_id = specification$respondent_id,
    variable = specification$variable, kind = "demographic", model = specification$variable,
    value = values, lower = interval[1, ], upper = interval[2, ], n_fit_rows = stats::nobs(fit))
  attr(data, "imputed_cells") <- dplyr::bind_rows(attr(data, "imputed_cells", exact = TRUE), reporting)
  data
}

#' AP3 postcondition — age, household size and income are complete
#'
#' Established by the final demographic-imputation verb, before covariate
#' derivation. Gender is deliberately excluded: a missing gender remains
#' allowed.
#'
#' @param data Frame returned by the demographic imputation.
#' @return `data`.
check_demographic_imputations_complete <- function(data) {
  variables <- c("demo_age", "demo_hh_members", "demo_income_hh_net")
  absent <- setdiff(variables, names(data))
  if (length(absent))
    stop("AP3: demographic variable(s) are absent: ",
      paste(absent, collapse = ", "), ".")
  missing_cells <- which(is.na(as.matrix(data[variables])), arr.ind = TRUE)
  unexplained <- has_unexplained_imputation_cells(data, variables)
  if (unexplained)
    stop("AP3: demographic imputation left ", nrow(missing_cells),
      " age, household-size, or income value(s) missing.")
  data
}

# Imputation support -----------------------------------------------------------
# The schema of the imputed-cell record and the sampler settings of its fits.

ap3_fill_empty_cells <- function() {
  tibble::tibble(
    respondent_id = integer(0), variable = character(0), kind = character(0),
    model = character(0), value = numeric(0), lower = numeric(0), upper = numeric(0),
    n_fit_rows = integer(0)
  )
}

# The sampler settings of the fits, from the configured regression settings.
ap3_imputation_sampling <- function(analysis_plan) {
  regression <- analysis_plan$regression
  warmup <- as.integer(regression$warmup)
  list(
    chains = as.integer(regression$chains), cores = as.integer(regression$cores),
    seed = as.integer(regression$seed),
    iter = as.integer(warmup + as.integer(regression$iter_per_chain)), warmup = warmup, thin = 1L,
    refresh = 0, silent = 2
  )
}
