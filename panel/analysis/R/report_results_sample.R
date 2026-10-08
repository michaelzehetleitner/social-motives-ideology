# Report targets of the Results section "Sample".
#
# Each function builds one report target (_targets.R, REPORTING · RESULTS ·
# SAMPLE). The report words and formats what these return; it computes nothing.

#' Participant flow of the Sample section
#'
#' Everyone who started the survey, those turned away at a full quota cell, then
#' the AP1 exclusion steps of the admitted respondents in their preregistered
#' order and the AP3 drop of respondents whose gaps exceed the fill limit, as
#' the last step of the flow.
#'
#' @param preparation_reporting_data List from [build_preparation_reporting_data()].
#' @param imputation_reporting_data List from [build_imputation_reporting_data()].
#' @return List `steps` (tibble: step, criterion, n_before, n_excluded, n_after
#'   and the AP1 step fields), `n_started`, `n_quota_full`, `n_admitted`,
#'   `n_excluded`, `n_analysis`,
#'   `quota_cells_computed`, `quota_cells_disagreeing`.
report_exclusion_steps <- function(preparation_reporting_data, imputation_reporting_data) {
  ap1_steps <- preparation_reporting_data$exclusions
  n_after_ap1 <- ap1_steps$n_after[nrow(ap1_steps)]
  n_dropped_unfillable <- nrow(imputation_reporting_data$excluded_participants)
  unfillable_step <- tibble::tibble(
    step = max(ap1_steps$step) + 1L,
    criterion = "dropped_unfillable_gaps",
    n_before = n_after_ap1,
    n_excluded = n_dropped_unfillable,
    n_after = n_after_ap1 - n_dropped_unfillable
  )
  steps <- dplyr::bind_rows(ap1_steps, unfillable_step)
  list(
    steps = steps,
    n_started = preparation_reporting_data$n_started,
    n_quota_full = preparation_reporting_data$n_quota_full,
    n_admitted = steps$n_before[1],
    n_excluded = sum(steps$n_excluded),
    n_analysis = steps$n_after[nrow(steps)],
    quota_cells_computed = preparation_reporting_data$quota_group_computed,
    quota_cells_disagreeing = preparation_reporting_data$quota_group_mismatch
  )
}

#' Save sample-section calculations before rendering
add_sample_reporting_facts <- function(flow, imputation_reporting_data, analysis_plan) {
  criteria <- c("not_consenting", "incomplete", "age_outside_range", "attention_1_wrong", "attention_2_wrong")
  count <- function(criterion) sum(flow$steps$n_excluded[flow$steps$criterion %in% criterion])
  flow$exclusion_hits <- vapply(criteria, count, numeric(1)) > 0
  flow$n_excluded_gender_divers <- count("gender_divers")
  flow$participation <- calculate_participation_reporting_data(flow$n_started, analysis_plan)
  flow$imputation <- summarise_imputation_processing(imputation_reporting_data)
  flow
}

#' Sample size of every model family
#'
#' Each regression uses its retained known-gender sample; the network uses
#' respondents with every node available. Separate equation counts are
#' retained when AP3 failure exclusions differ.
#'
#' @param report_participant_flow List from [report_exclusion_steps()].
#' @param regression_input_reporting_data List from
#'   [build_regression_input_reporting_data()].
#' @param data_network The network input.
#' @return List `n_analysis`, `n_regressions`, `n_omitted_for_missing_gender`,
#'   `n_network`.
report_analysis_sample_sizes <- function(report_participant_flow, regression_input_reporting_data,
                                         data_network) {
  list(
    n_analysis = report_participant_flow$n_analysis,
    n_regressions = regression_input_reporting_data$n_used,
    n_regressions_by_outcome = vapply(regression_input_reporting_data$model_samples[
      intersect(names(regression_input_reporting_data$model_samples), c("asc_agg", "asc_sub", "asc_conv", "sdo_dom"))],
      function(sample) sample$n_used, integer(1)),
    n_omitted_for_missing_gender = nrow(regression_input_reporting_data$omitted_gender),
    n_network = if (is.null(describe_unavailable_imputation_inputs(data_network, setdiff(names(data_network), "respondent_id"))))
      nrow(data_network) else NA_integer_,
    network_unavailable_reason = describe_unavailable_imputation_inputs(data_network, setdiff(names(data_network), "respondent_id")),
    regression_unavailable_reason = regression_input_reporting_data$unavailable_reason
  )
}

#' Rows of the sample-characteristics table
#'
#' Every category with its count and percentage, every numeric variable with
#' its mean and standard deviation, from the sample composition.
#'
#' @param sample_composition List from [describe_sample_composition()].
#' @return Tibble `characteristic`, `group`, `n`, `pct`, `mean`, `sd`.
report_sample_characteristic_rows <- function(sample_composition) {
  rows <- dplyr::bind_rows(
    tibble::tibble(
      characteristic = sample_composition$categorical$label,
      group = sample_composition$categorical$level,
      n = sample_composition$categorical$n,
      pct = sample_composition$categorical$pct,
      mean = NA_real_,
      sd = NA_real_
    ),
    tibble::tibble(
      characteristic = sample_composition$numeric$label,
      group = "M (SD)",
      n = sample_composition$numeric$n,
      pct = NA_real_,
      mean = sample_composition$numeric$mean,
      sd = sample_composition$numeric$sd
    )
  )
  categorical <- sample_composition$categorical
  education <- if ("variable" %in% names(categorical)) {
    categorical[categorical$variable %in% "demo_edu_school", , drop = FALSE]
  } else categorical[FALSE, , drop = FALSE]
  attr(rows, "education_rank") <- education[order(-education$pct), , drop = FALSE]
  attr(rows, "n_missing_education") <- sum(education$n[education$level %in% "(missing)"])
  rows
}

#' Political sample descriptions and numeric histogram inputs
#'
#' The descriptive population fixes both the categorical percentage denominator
#' and the rows used for numeric summaries. Party choice is conditional on Yes
#' to voting intention; unasked responses remain separate from unanswered
#' questions, and contribute to the denominator without becoming a display row.
#'
#' @param data The prepared study (`study_analysis_ready`).
#' @param descriptive_data The retained descriptive input, with respondent ids.
#' @param codebook Codebook containing the political response value sets.
#' @param config Analysis plan containing the survey age limits.
#' @return List `sample_rows`, `numeric_rows`, `scores`, `histogram_specs`,
#'   `sample_size` and `party_question_counts`. Histogram specifications map raw
#'   characteristic labels to score columns; `bin_width = 1` identifies the six
#'   integer party ratings. Other variables retain continuous histogram bins.
describe_sample_politics_for_report <- function(data, descriptive_data, codebook, config) {
  sympathy_columns <- c("pol_symp_spd", "pol_symp_cdu_csu", "pol_symp_greens",
                        "pol_symp_fdp", "pol_symp_afd", "pol_symp_linke")
  numeric_columns <- c(sympathy_columns, "pol_left_right")
  required <- c("respondent_id", "age", "income", "pol_vote_would", "pol_party_vote", numeric_columns)
  if (!all(required %in% names(data)) || !"respondent_id" %in% names(descriptive_data)) {
    stop("Political sample descriptions require prepared numeric columns and descriptive respondent ids.")
  }
  if (anyNA(data$respondent_id) || anyNA(descriptive_data$respondent_id) ||
      anyDuplicated(data$respondent_id) || anyDuplicated(descriptive_data$respondent_id)) {
    stop("Political sample descriptions require unique, observed respondent ids.")
  }
  row_indices <- match(descriptive_data$respondent_id, data$respondent_id)
  if (anyNA(row_indices)) stop("A descriptive respondent is absent from the prepared study.")
  data <- data[row_indices, , drop = FALSE]
  for (variable in c("age", "income")) {
    if (!variable %in% names(descriptive_data) ||
        !isTRUE(all.equal(data[[variable]], descriptive_data[[variable]], check.attributes = FALSE))) {
      stop("Histogram values must match the descriptive ", variable, " values.")
    }
  }
  n_sample <- nrow(data)
  read_levels <- function(set_id) {
    values <- codebook$factors[codebook$factors$set_id == set_id, , drop = FALSE]
    if (!nrow(values)) stop("Missing political response value set: ", set_id)
    values[order(values$order), , drop = FALSE]
  }
  voting_levels <- read_levels("yes_no_1_2")
  party_levels <- read_levels("gles_party_vote")
  voting_labels <- c("Ja" = "Yes", "Nein" = "No")
  party_labels <- c("CDU/CSU" = "CDU/CSU", "SPD" = "SPD", "FDP" = "FDP",
                    "Bündnis 90/Die Grünen" = "The Greens", "Die Linke" = "The Left", "AfD" = "AfD",
                    "Bündnis Sahra Wagenknecht (BSW)" = "Sahra Wagenknecht Alliance (BSW)",
                    "Eine andere Partei und zwar" = "Other party", "Ich würde ungültig wählen" = "Invalid vote")
  if (!all(voting_levels$label_de %in% names(voting_labels)) ||
      !all(party_levels$label_de %in% names(party_labels))) {
    stop("A political response category has no English display label.")
  }
  if (!all(stats::na.omit(data$pol_vote_would) %in% voting_levels$value_corr) ||
      !all(stats::na.omit(data$pol_party_vote) %in% party_levels$value_corr)) {
    stop("Political responses contain a code outside their codebook value set.")
  }
  yes_code <- voting_levels$value_corr[voting_levels$label_de == "Ja"]
  if (length(yes_code) != 1L) stop("Voting intention requires one Yes response code.")
  party_asked <- !is.na(data$pol_vote_would) & data$pol_vote_would == yes_code
  if (any(!is.na(data$pol_party_vote[!party_asked]))) {
    stop("Party choice is answered for a respondent who was not asked the question.")
  }
  voting <- factor(data$pol_vote_would, levels = voting_levels$value_corr,
                   labels = unname(voting_labels[voting_levels$label_de]))
  party <- as.character(factor(data$pol_party_vote, levels = party_levels$value_corr,
                              labels = unname(party_labels[party_levels$label_de])))
  party[!party_asked] <- "Not asked"
  party <- factor(party, levels = c(unname(party_labels[party_levels$label_de]), "Not asked"))
  categories <- dplyr::bind_rows(
    ap5_count_categories(voting, "pol_vote_would", "Voting intention"),
    ap5_count_categories(party, "pol_party_vote", "Party choice")
  )
  categories$level[categories$level == "(missing)"] <- "Missing"
  categories <- categories[categories$level != "Not asked", , drop = FALSE]
  numeric_labels <- c(paste0("Party sympathy: ", c("SPD", "CDU/CSU", "The Greens", "FDP", "AfD", "The Left")),
                      "Left–right placement")
  numeric_rows <- dplyr::bind_rows(lapply(seq_along(numeric_columns), function(i) {
    ap5_summarise_numeric(data[[numeric_columns[i]]], numeric_columns[i], numeric_labels[i])
  }))
  numeric_rows$scale_key <- numeric_rows$variable
  numeric_rows$missing <- n_sample - numeric_rows$n
  sample_rows <- report_sample_characteristic_rows(list(categorical = categories, numeric = numeric_rows))
  observed_income <- data$income[is.finite(data$income)]
  income_range <- if (length(observed_income)) range(observed_income) else c(NA_real_, NA_real_)
  if (all(is.finite(income_range)) && income_range[1] == income_range[2]) {
    income_range <- income_range + c(-1, 1) * max(abs(income_range[1]) * .05, .5)
  }
  sympathy_range <- range(read_levels("party_sympathy_minus3_3")$value_corr)
  left_right_range <- range(read_levels("left_right_0_10")$value_corr)
  histogram_specs <- tibble::tibble(
    characteristic = c("Age (years)", "Net household income per person", numeric_labels),
    variable = c("age", "income", numeric_columns),
    response_min = c(config$exclusions$min_age, income_range[1], rep(sympathy_range[1], 6), left_right_range[1]),
    response_max = c(config$exclusions$max_age, income_range[2], rep(sympathy_range[2], 6), left_right_range[2]),
    bin_width = c(NA_real_, NA_real_, rep(1, 6), NA_real_)
  )
  list(
    sample_rows = sample_rows,
    numeric_rows = numeric_rows,
    scores = data[c("age", "income", numeric_columns)],
    histogram_specs = histogram_specs,
    sample_size = n_sample,
    party_question_counts = tibble::tibble(
      status = c("asked", "not_asked", "missing_when_asked"),
      n = c(sum(party_asked), sum(!party_asked), sum(is.na(data$pol_party_vote[party_asked])))
    )
  )
}
