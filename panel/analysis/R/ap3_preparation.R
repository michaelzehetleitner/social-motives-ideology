# Common study preparation and the three analysis inputs.
# The scientific sequence lives in _targets.R. These helpers perform one
# calculation or expose its records; they do not choose another sequence.

# Region ----------------------------------------------------------------------

# BEGIN GENERATED PARAMETER CARD: AP3 EAST WEST
# Automatically generated from analysis_plan.yaml
# East/West from the federal-state code
#   East: 11, 12, 13, 14, 15, 16   (east_west.east)
#   West: 1, 2, 3, 4, 5, 6, 7, 8, 9, 10   (east_west.west)
# END GENERATED PARAMETER CARD: AP3 EAST WEST
#' AP3 — East/West from the federal state
#'
#' East = the codes `analysis_plan$east_west$east` lists, Berlin among them;
#' West = the codes `analysis_plan$east_west$west` lists. The federal state is
#' never imputed, so this runs once on the eligible study, before the
#' imputation.
#'
#' @param data Frame after [prepare_gender()].
#' @param analysis_plan Configuration from [zm_config()].
#' @return `data` with the factor `east_west` (levels west, east).
derive_east_west <- function(data, analysis_plan) {
  east_codes <- unlist(analysis_plan$east_west$east)
  west_codes <- unlist(analysis_plan$east_west$west)
  if (any(!is.na(data$demo_bundesland) & !data$demo_bundesland %in% c(east_codes, west_codes)))
    stop("Covariate input contains an unrecognised federal-state code.")
  data$east_west <- factor(ifelse(data$demo_bundesland %in% east_codes, "east",
    ifelse(data$demo_bundesland %in% west_codes, "west", NA_character_)),
    levels = c("west", "east"))
  data
}

# Covariates ------------------------------------------------------------------

# BEGIN GENERATED PARAMETER CARD: AP3 COVARIATES
# Automatically generated from analysis_plan.yaml
# Income per household member: band representative / min(household size, 8)
#   Band representatives, euro per month: 1 = 250; 2 = 625; 3 = 875; 4 = 1125; 5 = 1375; 6 = 1750;
#     7 = 2250; 8 = 2750; 9 = 3500; 10 = 4500; 11 = 6250; 12 = 8750; 13 = 12500
#     (income.band_representative)
#   Household size top value: 8   (income.hh_size_top_value)
# Age and income per household member enter the analyses in bands, formed after the fill
#   Age bands, completed years, each as its midpoint   (age_bands)
#     18–24 = 21; 25–29 = 27; 30–34 = 32; 35–39 = 37; 40–44 = 42; 45–49 = 47; 50–54 = 52;
#     55–59 = 57; 60–64 = 62; 65–69 = 67
#   Income bands per household member, euro per month, right-open, each as the geometric_mean of its limits
#     Limits: 357, 500, 630, 800, 1050, 1400, 2000, 2800, 12500   (income_bands_per_member.limits)
#     Representatives, rounded to whole euro: <500 = 422; 500–<630 = 561; 630–<800 = 710;
#       800–<1050 = 917; 1050–<1400 = 1212; 1400–<2000 = 1673; 2000–<2800 = 2366; 2800+ = 5916
# END GENERATED PARAMETER CARD: AP3 COVARIATES
#' AP3 — derive the preregistered age, income, education and quota group
#'
#' `age` from the raw `demo_age`; `income` = the band representative per
#' household member, with the household size capped at
#' `analysis_plan$income$hh_size_top_value`; both exact, as the descriptives
#' use them, and in their analysis bands ([add_age_and_income_bands()]):
#' `age_band` with `age_band_midpoint`, and `income_band` with
#' `income_band_value` and its natural logarithm `income_band_value_log`, the
#' sources the models standardise; `education` as the ordered survey
#' categories; and `quota_group`, the exported M4 cell where
#' the export has one and the value the party-scalometer rule computes where it
#' is blank.
#'
#' Reverse scoring, the gender factor and East/West ([derive_east_west()])
#' precede the imputation and are not repeated here. During intake preparation,
#' the function runs on the filled frame used for analysis and scientific-use
#' values, and on the observed frame used for the unlinked demographic margins.
#'
#' @param data Frame after [reverse_items()] and [prepare_gender()],
#'   imputed or unimputed.
#' @param analysis_plan Configuration from [zm_config()].
#' @param codebook Codebook from [zm_codebook()] (education labels).
#' @return `data` with the derived columns, `quota_group_source`, and the
#'   attributes `quota_group_computed` and `quota_group_mismatch`.
derive_covariates <- function(data, analysis_plan, codebook) {
  check_covariate_input_codes(data, analysis_plan)
  income_band_representatives <- unname(unlist(analysis_plan$income$band_representative))
  data$age <- as.numeric(data$demo_age)
  data$income <- income_band_representatives[data$demo_income_hh_net] /
    pmin(data$demo_hh_members, analysis_plan$income$hh_size_top_value)
  data <- add_age_and_income_bands(data, analysis_plan)
  data$education <- ap3_order_education_categories(
    data$demo_edu_school, codebook)
  data <- ap3_complete_missing_quota_groups(data)
  data
}

#' Technical input check — the covariate source codes are registered ones
#'
#' [check_analysis_plan()] has already established that the East and West sets
#' are disjoint. This data-boundary check rejects observed or imputed codes
#' outside the registered income and household-size sets.
#'
#' @param data Frame entering [derive_covariates()].
#' @param analysis_plan Configuration from [zm_config()].
#' @return `NULL`, invisibly.
check_covariate_input_codes <- function(data, analysis_plan) {
  income_bands <- seq_along(analysis_plan$income$band_representative)
  invalid_income_band <- !is.na(data$demo_income_hh_net) &
    !data$demo_income_hh_net %in% income_bands
  invalid_household_size <- !is.na(data$demo_hh_members) &
    !data$demo_hh_members %in% seq_len(analysis_plan$income$hh_size_top_value)
  if (any(invalid_income_band | invalid_household_size))
    stop("Covariate input contains an unrecognised code.")
  invisible(NULL)
}

#' The school-education categories in survey order, with their German labels
#'
#' [check_codebook()] has already established one nonempty, uniquely ordered
#' value set for `demo_edu_school`. This mechanism applies its German labels in
#' survey order and rejects only observed participant codes outside that set.
#'
#' @param values Numeric vector `demo_edu_school`.
#' @param codebook Codebook from [zm_codebook()].
#' @return Ordered factor.
ap3_order_education_categories <- function(values, codebook) {
  value_set <- codebook$items$value_set_id[
    match("demo_edu_school", codebook$items$item_code)]
  categories <- codebook$factors[
    codebook$factors$set_id == value_set, , drop = FALSE]
  categories <- categories[order(categories$order), , drop = FALSE]
  if (any(!is.na(values) & !values %in% categories$value_corr))
    stop("An observed education code is not in the registered value set.")
  factor(values, levels = categories$value_corr,
    labels = categories$label_de, ordered = TRUE)
}

#' Complete the blank quota cells by the M4 party-scalometer rule
#'
#' A party is liked when its scalometer is strictly positive. Liking a left
#' party only gives `left_leaning`, a conservative party only
#' `conservative_leaning`, both or neither `mixed`. Every recognised exported
#' group is preserved; only a blank is derived. The number of derived cells and
#' the number of exported cells that disagree with the rule are recorded as
#' attributes.
#'
#' @param data Frame with the six `pol_symp_*` columns and `quota_group`.
#' @return `data` with `quota_group`, `quota_group_source`, and the attributes
#'   `quota_group_computed` and `quota_group_mismatch`.
ap3_complete_missing_quota_groups <- function(data) {
  liked_any <- function(columns) {
    ratings <- as.matrix(data[columns])
    liked <- rowSums(ratings > 0, na.rm = TRUE) > 0
    liked[!liked & rowSums(is.na(ratings)) > 0] <- NA
    liked
  }
  likes_left <- liked_any(c("pol_symp_spd", "pol_symp_linke", "pol_symp_greens"))
  likes_conservative <- liked_any(
    c("pol_symp_afd", "pol_symp_cdu_csu", "pol_symp_fdp"))
  computed <- rep(NA_character_, nrow(data))
  decided <- !is.na(likes_left) & !is.na(likes_conservative)
  computed[decided] <- "mixed"
  computed[decided & likes_left & !likes_conservative] <- "left_leaning"
  computed[decided & likes_conservative & !likes_left] <- "conservative_leaning"
  exported <- as.character(data$quota_group)
  present <- !is.na(exported) & nzchar(exported)
  allowed <- c("left_leaning", "conservative_leaning", "mixed")
  if (any(present & !exported %in% allowed))
    stop("An exported quota group is not one of the three registered groups.")
  data$quota_group <- factor(ifelse(present, exported, computed),
    levels = allowed)
  data$quota_group_source <- ifelse(present, "export", "computed")
  attr(data, "quota_group_mismatch") <-
    sum(present & !is.na(computed) & exported != computed)
  attr(data, "quota_group_computed") <- sum(!present)
  data
}

# Scale scores and standardisation ---------------------------------------------

#' AP3 — each codebook-defined subscale as its row mean
#'
#' Each scale score, named by `scale_key`, is the participant's mean over the
#' scale's `item_codes`; a missing item yields a missing score.
#'
#' @param data Frame whose items are already in their scoring direction.
#' @param codebook Codebook from [zm_codebook()].
#' @return `data` with one column per scale (`zm_achievement` … `sdo_dom`).
average_items_into_subscales <- function(data, codebook) {
  for (i in seq_len(nrow(codebook$scales))) {
    scale <- codebook$scales$scale_key[[i]]
    items <- codebook$scales$item_codes[[i]]
    data[[scale]] <- rowMeans(data[items], na.rm = FALSE)
  }
  data
}

# BEGIN GENERATED PARAMETER CARD: AP3 STANDARDISATION
# Automatically generated from analysis_plan.yaml
# All retained rows: the network nodes
#   zm_security, zm_arousal, zm_power, zm_prestige, zm_achievement, asc_agg, asc_sub, asc_conv,
#   sdo_dom
#   (network.nodes)
# Rows with a known gender: the regression variables
#   Covariates: age, gender, income; the metric ones are standardised from their bands: age as the band midpoint, income as the log of the band representative
#   Within each model's own sample: yes   (missing_data.standardise_within_model)
# END GENERATED PARAMETER CARD: AP3 STANDARDISATION
#' AP3 — all-row network z scores and their saved constants
#'
#' The population has available values on every configured network node after
#' the excessive-missingness exclusion and imputation failure exclusions.
#' Each mean, SD and n is saved in the attribute `z_parameters_all` and used
#' unchanged downstream; an empirically undefined SD stops the pipeline.
#'
#' @param data Frame with the scale scores.
#' @param codebook Codebook from [zm_codebook()]; it names the z columns.
#' @param analysis_plan Configuration from [zm_config()].
#' @return `data` with the physical all-row z columns and the attribute
#'   `z_parameters_all`.
standardise_all <- function(data, codebook, analysis_plan) {
  vars <- ap3_model_spec(analysis_plan, codebook)$network$z_vars
  retained <- !find_rows_with_failed_imputations(data, vars)
  rows <- vector("list", length(vars))
  for (i in seq_along(vars)) {
    var <- vars[[i]]; values <- data[[var]][retained]
    insufficient <- length(values) < 2L && any(!retained)
    mean_value <- if (insufficient) NA_real_ else mean(values)
    sd_value <- if (insufficient) NA_real_ else stats::sd(values)
    if (!insufficient && (!is.finite(sd_value) || sd_value <= 0))
      stop("All-row standardisation has undefined SD for ", var, ".")
    z_col <- zm_z_col(var, codebook, population = "all")
    data[[z_col]] <- NA_real_
    data[[z_col]][retained] <- (values - mean_value) / sd_value
    rows[[i]] <- tibble::tibble(var, source = var, transform = "identity", z_col,
      mean = mean_value, sd = sd_value, n = length(values))
  }
  attr(data, "z_parameters_all") <- dplyr::bind_rows(rows)
  data
}

#' AP3 — known-gender z scores and their shared constants
#'
#' The physical population has a known gender and available values for the
#' joint regression. Separate equation samples are prepared after failures. The direct sources are the four outcomes and the five motives;
#' age enters as `age_band_midpoint` and income as `income_band_value_log`
#' ([ap3_z_columns()]). Unknown-gender rows keep `NA` in these
#' physical z columns. Source, transform, mean, SD and n are saved in the
#' attribute `z_parameters_known_gender`.
#'
#' @inheritParams standardise_all
#' @return `data` with the physical known-gender z columns and the attribute
#'   `z_parameters_known_gender`.
standardise_known_gender <- function(data, codebook, analysis_plan,
                                        vars = ap3_regression_z_variables(analysis_plan, codebook)) {
  known_gender <- !is.na(data$gender)
  if (sum(known_gender) < 2L &&
      !isTRUE(nrow(attr(data, "excluded_imputation", exact = TRUE)) > 0L))
    stop("Fewer than two known-gender rows.")
  spec <- ap3_z_columns(analysis_plan, codebook)
  sources <- select_z_source_columns(vars, analysis_plan, codebook)
  transforms <- stats::setNames(spec$transform[match(vars, spec$var)], vars)
  transforms[is.na(transforms)] <- "identity"
  retained <- known_gender & !find_rows_with_failed_imputations(data, unname(sources))
  insufficient <- sum(retained) < 2L && (any(known_gender & !retained) ||
    isTRUE(nrow(attr(data, "excluded_imputation", exact = TRUE)) > 0L))
  rows <- vector("list", length(vars))
  for (i in seq_along(vars)) {
    var <- vars[[i]]; source <- sources[[var]]
    values <- data[[source]][retained]
    mean_value <- if (insufficient) NA_real_ else mean(values)
    sd_value <- if (insufficient) NA_real_ else stats::sd(values)
    if (!insufficient && (!is.finite(sd_value) || sd_value <= 0))
      stop("Known-gender standardisation has undefined SD for ", source, ".")
    z_col <- zm_z_col(var, codebook, population = "known_gender")
    data[[z_col]] <- NA_real_
    data[[z_col]][retained] <- (values - mean_value) / sd_value
    rows[[i]] <- tibble::tibble(var, source, transform = transforms[[var]], z_col,
      mean = mean_value, sd = sd_value, n = sum(retained))
  }
  attr(data, "z_parameters_known_gender") <- dplyr::bind_rows(rows)
  data
}

#' The variables the four regressions and the joint model standardise
#'
#' @param analysis_plan Configuration from [zm_config()].
#' @param codebook Codebook from [zm_codebook()].
#' @return Character vector, in model-specification order.
ap3_regression_z_variables <- function(analysis_plan, codebook) {
  models <- ap3_model_spec(analysis_plan, codebook)[as.character(analysis_plan$regression$outcomes)]
  unique(unlist(lapply(models, function(model) model$z_vars), use.names = FALSE))
}

# Reporting boundaries ---------------------------------------------------------
# These assemble already-calculated facts. They cannot change the analysis table.

#' AP3 — the preparation facts, assembled for reporting
#'
#' @param data The prepared study (`study_analysis_ready`).
#' @param exclusions The AP1 result from [apply_study_exclusions()]: its log and
#'   the counts of started responses and quota-full exits.
#' @param imputation_reporting_data From [build_imputation_reporting_data()].
#' @return Named list of preparation facts.
build_preparation_reporting_data <- function(data, exclusions, imputation_reporting_data) {
  gender_counts <- tibble::tibble(
    category = c(levels(data$gender), "missing"),
    n = c(as.integer(table(data$gender)), sum(is.na(data$gender))))
  list(
    exclusions = exclusions$log,
    n_started = exclusions$n_started,
    n_quota_full = exclusions$n_quota_full,
    imputation = imputation_reporting_data,
    reversed_items = attr(data, "reversed_items"),
    gender_counts = gender_counts,
    gender_reference = levels(data$gender)[1],
    quota_group_computed = attr(data, "quota_group_computed"),
    quota_group_mismatch = attr(data, "quota_group_mismatch"),
    z_parameters_all = attr(data, "z_parameters_all"),
    z_parameters_known_gender = attr(data, "z_parameters_known_gender")
  )
}

#' AP3 — the regression-population facts, extracted for reporting
#'
#' @param data The regression input (`data_regressions`).
#' @return `list(n_input, n_used, participants, omitted_gender, z_parameters)`.
build_regression_input_reporting_data <- function(data) {
  list(
    n_input = attr(data, "regression_n_input"),
    n_used = if (is.null(describe_unavailable_imputation_inputs(data, setdiff(names(data), "respondent_id"))))
      nrow(data) else NA_integer_,
    unavailable_reason = describe_unavailable_imputation_inputs(data, setdiff(names(data), "respondent_id")),
    participants = attr(data, "regression_participants"),
    omitted_gender = attr(data, "omitted_gender"),
    z_parameters = attr(data, "z_parameters"),
    excluded_imputation = attr(data, "excluded_imputation"),
    model_samples = lapply(attr(data, "regression_model_inputs"), function(input) list(
      n_used = nrow(input$data), participants = input$data$respondent_id,
      excluded_imputation = attr(input$data, "excluded_imputation"),
      z_parameters = attr(input$data, "z_parameters")))
  )
}

# The three analysis inputs ----------------------------------------------------
# Each input excludes unresolved values only where they are required.
# After an imputation failure, the regression selector also prepares each
# equation's standardisation once for reuse by all of its sensitivity fits.

#' AP3 — the all-retained descriptive, reliability and factor-analysis input
#'
#' Respondent id, the 57 imputed item values, the nine raw scale scores, exact
#' age, gender, school education, East/West, exact income per household member
#' and the quota group. The population includes the rows with a missing gender. Neither
#' physical z-score family is carried.
#'
#' The measurement definitions name items by their questionnaire label. Every
#' verb that selects item columns receives the codebook and resolves the labels
#' through [zm_item_columns()].
#'
#' @param data The prepared study (`study_analysis_ready`).
#' @param codebook Codebook from [zm_codebook()].
#' @return Tibble with exactly those columns.
select_descriptive_reliability_input <- function(data, codebook) {
  descriptive_variables <- c(
    "respondent_id", unique(unlist(codebook$scales$item_codes)),
    codebook$scales$scale_key, "gender", "quota_group")
  dplyr::select(data, dplyr::all_of(descriptive_variables))
}

# BEGIN GENERATED PARAMETER CARD: AP3 NETWORK INPUT
# Automatically generated from analysis_plan.yaml
# The standardised nodes, in the order of codebook_scales.csv   (network.nodes)
#   zm_security, zm_arousal, zm_power, zm_prestige, zm_achievement, asc_agg, asc_sub, asc_conv,
#   sdo_dom
# END GENERATED PARAMETER CARD: AP3 NETWORK INPUT
#' AP3 — the all-retained network input in configured node order
#'
#' The saved complete-node z scores under their canonical codebook names, in
#' configured node order. Respondents with unresolved node values leave here.
#'
#' @inheritParams select_descriptive_reliability_input
#' @param analysis_plan Configuration from [zm_config()].
#' @return Tibble with `respondent_id` and the nine node columns; attribute
#'   `z_parameters`.
select_network_input <- function(data, codebook, analysis_plan) {
  model <- ap3_model_spec(analysis_plan, codebook)$network
  data <- exclude_rows_with_failed_imputations(data, model$z_vars)
  ap3_select_z_population(
    data, codebook, attr(data, "z_parameters_all"), model$z_vars)
}

#' AP3 — the known-gender regression population and its omission record
#'
#' Gender is never imputed. The rows without it stay in the descriptive,
#' reliability, factor-analysis and network inputs, and leave the four
#' regressions and the joint model here.
#'
#' @inheritParams select_descriptive_reliability_input
#' @return `data` without the missing-gender rows; attributes
#'   `regression_n_input`, `omitted_gender` and `regression_participants`.
drop_rows_without_gender <- function(data) {
  known_gender <- !is.na(data$gender)
  n_input <- nrow(data)
  participants_without_gender <- tibble::tibble(
    respondent_id = data$respondent_id[!known_gender])
  data <- dplyr::filter(data, !is.na(.data$gender))
  attr(data, "regression_n_input") <- n_input
  attr(data, "omitted_gender") <- participants_without_gender
  attr(data, "regression_participants") <- data$respondent_id
  data
}

# BEGIN GENERATED PARAMETER CARD: AP3 REGRESSION INPUT
# Automatically generated from analysis_plan.yaml
#   Outcomes: asc_agg, asc_sub, asc_conv, sdo_dom   (regression.outcomes)
#   Motives: zm_security, zm_arousal, zm_power, zm_prestige, zm_achievement   (regression.motives)
#   Covariates: age, gender, income   (regression.covariates)
# END GENERATED PARAMETER CARD: AP3 REGRESSION INPUT
#' AP3 — the regression input, with its saved z constants carried along
#'
#' The four outcomes, the five motives and the covariates `age_z`, `gender`,
#' `income_z`, on known-gender rows complete for the joint regression. If a
#' required fill failed, separate samples and fixed constants are retained
#' for the four individual regressions and the joint motive model.
#'
#' @inheritParams select_network_input
#' @param data The known-gender population from [drop_rows_without_gender()].
#' @return Tibble with `respondent_id`, `gender` and the z columns; attributes
#'   `regression_n_input`, `omitted_gender`, `regression_participants`,
#'   `z_parameters`.
select_regression_input <- function(data, codebook, analysis_plan) {
  vars <- ap3_regression_z_variables(analysis_plan, codebook)
  sources <- unname(select_z_source_columns(vars, analysis_plan, codebook))
  retained <- exclude_rows_with_failed_imputations(data, sources)
  regression_z_scores <- ap3_select_z_population(
    retained, codebook, attr(data, "z_parameters_known_gender"), vars)
  z_parameters <- attr(regression_z_scores, "z_parameters")
  regression_data <- dplyr::bind_cols(
    dplyr::select(retained, respondent_id, gender),
    dplyr::select(regression_z_scores, -respondent_id))
  attr(regression_data, "regression_n_input") <- attr(data, "regression_n_input")
  attr(regression_data, "omitted_gender") <- attr(data, "omitted_gender")
  attr(regression_data, "regression_participants") <- retained$respondent_id
  attr(regression_data, "z_parameters") <- z_parameters
  regression_data <- copy_imputation_status(regression_data, retained)
  if (any(find_rows_with_failed_imputations(data, sources))) {
    models <- ap3_model_spec(analysis_plan, codebook)[as.character(analysis_plan$regression$outcomes)]
    inputs <- lapply(models, function(model) {
      prepare_regression_input_for_variables(data, model$z_vars, codebook, analysis_plan)
    })
    motive_vars <- c(as.character(analysis_plan$regression$motives), "age", "income")
    inputs$motives <- prepare_regression_input_for_variables(data, motive_vars, codebook, analysis_plan)
    inputs$joint <- list(variables = c("gender", zm_z_col(vars, codebook)), data = regression_data)
    attr(regression_data, "regression_model_inputs") <- inputs
  }
  regression_data
}

#' Prepare a regression's retained cases and fixed standardisation once
prepare_regression_input_for_variables <- function(data, variables, codebook, analysis_plan) {
  sources <- unname(select_z_source_columns(variables, analysis_plan, codebook))
  sample <- exclude_rows_with_failed_imputations(data, sources)
  sample <- standardise_known_gender(sample, codebook, analysis_plan, variables)
  scores <- ap3_select_z_population(sample, codebook,
    attr(sample, "z_parameters_known_gender"), variables)
  result <- dplyr::bind_cols(dplyr::select(sample, respondent_id, gender),
    dplyr::select(scores, -respondent_id))
  attr(result, "z_parameters") <- attr(scores, "z_parameters")
  result <- copy_imputation_status(result, sample)
  list(variables = c("gender", zm_z_col(variables, codebook)), data = result)
}

#' Select one standardisation population's z scores under canonical names
#'
#' Verifies the requested variables and saved constants, orders both by the
#' caller's configured analysis order, maps the physical z columns to their
#' collision-free canonical names, and returns the selected data with its
#' constants. Formulae and report code keep their frozen canonical names while
#' the common study retains both physical families.
#'
#' @param data The prepared study.
#' @param codebook Codebook from [zm_codebook()].
#' @param standardisation_parameters The saved constants of one population.
#' @param variables The variables to select, in the caller's order.
#' @return Tibble with `respondent_id` and the canonical z columns; attribute
#'   `z_parameters`.
ap3_select_z_population <- function(data, codebook, standardisation_parameters, variables) {
  required <- c("var", "source", "transform", "z_col", "mean", "sd", "n")
  if (is.null(standardisation_parameters) ||
      !all(required %in% names(standardisation_parameters)))
    stop("Selected z parameters are incomplete.")
  if (anyDuplicated(standardisation_parameters$var) ||
      !all(variables %in% standardisation_parameters$var))
    stop("Selected z parameters do not contain each requested variable once.")
  index <- match(variables, standardisation_parameters$var)
  standardisation_parameters <- standardisation_parameters[index, , drop = FALSE]
  if (!all(standardisation_parameters$z_col %in% names(data)))
    stop("A selected physical z column is absent from the data.")
  canonical <- zm_z_col(variables, codebook)
  untouched <- setdiff(names(data), standardisation_parameters$z_col)
  if (anyDuplicated(canonical) || any(canonical %in% untouched))
    stop("Canonical z-column mapping collides with the selected data.")
  selected_data <- dplyr::select(data,
    dplyr::any_of("respondent_id"),
    dplyr::all_of(standardisation_parameters$z_col))
  names(selected_data)[match(standardisation_parameters$z_col,
    names(selected_data))] <- canonical
  standardisation_parameters$z_col <- canonical
  attr(selected_data, "z_parameters") <- standardisation_parameters
  copy_imputation_status(selected_data, data)
}
