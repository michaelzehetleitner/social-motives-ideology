# Numeric summaries for compact synthetic-result exports and recovery checks.
#
# The synthetic preregistration data are drawn from config/simulation_truth.yaml
# by scripts/make_synthetic_data.R (R/simulate_testdata.R). The functions below
# describe the generating design and the handling of planted problems;
# assemble_synthetic_report_values() collects them for the saved review exports.
# Nothing here fits a model: every result comes from an existing target.
# Shared wording and formatting helpers at the end also serve the main report.

# ---- inputs -------------------------------------------------------------------

#' Read the generating values of the synthetic export
#'
#' The generating file declares one base design and the scenarios that deviate
#' from it; the export in `data/synthetic/` is drawn under the scenario that
#' `scenarios$export` names, so the comparison uses that scenario.
#'
#' @param truth_path Path of `config/simulation_truth.yaml`.
#' @return The truth resolved to the export scenario ([zm_truth_scenario()]).
read_export_generating_values <- function(truth_path) {
  zm_truth_scenario(zm_truth(truth_path))
}

#' Read the latent scores the synthetic export was drawn from
#'
#' One row per generated record, excluded records included
#' (`data/synthetic/zm_panel_synthetic_latent.csv`, written beside the export
#' by `scripts/make_synthetic_data.R`).
#'
#' @param latent_path Path of the latent-score file.
#' @return Tibble: the nine latent scores, `male` and the exclusion reason.
read_generated_latent_scores <- function(latent_path) {
  readr::read_csv(latent_path, show_col_types = FALSE, progress = FALSE)
}

#' Find the scale and the position of items in the codebook
#'
#' @param items Item codes.
#' @param codebook Codebook from [zm_codebook()].
#' @return Tibble `item`, `scale_key` and `position` (the item's place in its
#'   scale's item list); both `NA` for a column that is no scale item.
locate_items_in_scales <- function(items, codebook) {
  scales <- codebook$scales
  items <- as.character(items)
  owner <- vapply(items, function(item) {
    hit <- which(vapply(scales$item_codes, function(codes) item %in% codes, logical(1)))
    if (length(hit) == 1L) as.character(scales$scale_key[hit]) else NA_character_
  }, character(1), USE.NAMES = FALSE)
  position <- vapply(seq_along(items), function(i) {
    if (is.na(owner[i])) return(NA_integer_)
    match(items[i], scales$item_codes[[match(owner[i], scales$scale_key)]])
  }, integer(1))
  tibble::tibble(item = items, scale_key = owner, position = position)
}

# ---- the generating coefficients beside the estimates ---------------------------

#' The generating coefficients, one row per outcome and predictor
#'
#' @param truth Truth from [read_export_generating_values()].
#' @return Tibble `outcome`, `predictor`, `generating`, in the order of the
#'   generating file ([order_generating_rows()] puts them in the report's).
tabulate_generating_coefficients <- function(truth) {
  tb <- truth$true_beta
  tibble::tibble(
    outcome = rep(names(tb), lengths(tb)),
    predictor = unlist(lapply(tb, names), use.names = FALSE),
    generating = as.numeric(unlist(tb, use.names = FALSE))
  )
}

#' Rows of the generating coefficients in the order the report names them
#'
#' The generating file keeps its own order. The report lists the predictors
#' as the comparison table does, the motives in the configured order (the
#' codebook's) and then age, gender and income, and within a predictor the
#' outcomes in the configured order (the codebook's).
#'
#' @param rows Tibble with `outcome` and `predictor` (keys of the generating
#'   file: motive keys, `age`, `male`, `income`).
#' @param analysis_plan Configuration (`regression$outcomes`, `regression$motives`).
#' @return `rows`, reordered.
order_generating_rows <- function(rows, analysis_plan) {
  predictors <- c(as.character(analysis_plan$regression$motives), "age", "male", "income")
  outcomes <- as.character(analysis_plan$regression$outcomes)
  rows[order(match(rows$predictor, predictors), match(rows$outcome, outcomes)), , drop = FALSE]
}

#' The predicted signs of the preregistration's prediction table
#'
#' @param analysis_plan Configuration (`predictions$table`).
#' @return Tibble `outcome`, `predictor`, `predicted_sign` ("+", "-" or "±").
tabulate_predicted_signs <- function(analysis_plan) {
  table <- analysis_plan$predictions$table
  tibble::tibble(
    outcome = rep(names(table), lengths(table)),
    predictor = unlist(lapply(table, names), use.names = FALSE),
    predicted_sign = as.character(unlist(table, use.names = FALSE))
  )
}

#' Summarise the generating design the report states
#'
#' The counts and ranges the report gives for the generating file: its records,
#' the share of exactly zero coefficients, the range of the nonzero motive
#' coefficients, the nonzero demographic ones, the correlation ranges, the item
#' loading, the answer categories, and how the zero and nonzero motive
#' coefficients fall on the cells of the preregistration's prediction table.
#'
#' @param truth Truth from [read_export_generating_values()].
#' @param analysis_plan Configuration (`scales`, `predictions$table`).
#' @return List `n_records`, `n_coefficients`, `n_zero`, `share_zero`,
#'   `n_motive_coefficients`, `motive_nonzero_range` (absolute values),
#'   `covariates_nonzero` (tibble `outcome`, `predictor`, `generating`),
#'   `motive_correlation_range`, `residual_correlation_range`, `loading`,
#'   `response_range` and `prediction_cells` (list of counts: `n_directional`,
#'   `n_directional_effect` in the predicted direction, `n_directional_opposite`,
#'   `n_directional_zero`, `n_open`, `n_open_effect`, `n_open_zero`).
summarise_generating_design <- function(truth, analysis_plan) {
  generating <- order_generating_rows(tabulate_generating_coefficients(truth), analysis_plan)
  motives <- truth$motive_correlations$order
  is_motive <- generating$predictor %in% motives
  nonzero <- generating$generating != 0
  off_diagonal <- function(m) m[upper.tri(m)]
  motive_nonzero <- abs(generating$generating[is_motive & nonzero])
  cells <- dplyr::inner_join(generating[is_motive, , drop = FALSE], tabulate_predicted_signs(analysis_plan),
                             by = c("outcome", "predictor"))
  directional <- cells$predicted_sign %in% c("+", "-")
  effect <- cells$generating != 0
  agrees <- sign(cells$generating) == ifelse(cells$predicted_sign == "+", 1, -1)
  list(
    n_records = as.integer(truth$n_total),
    n_coefficients = nrow(generating),
    n_zero = sum(!nonzero),
    share_zero = mean(!nonzero),
    n_motive_coefficients = sum(is_motive),
    motive_nonzero_range = if (length(motive_nonzero)) range(motive_nonzero) else c(NA_real_, NA_real_),
    covariates_nonzero = generating[!is_motive & nonzero, , drop = FALSE],
    motive_correlation_range = range(off_diagonal(truth$motive_correlations$matrix)),
    residual_correlation_range = range(off_diagonal(truth$outcome_residual_correlations$matrix)),
    loading = as.numeric(truth$measurement$loading),
    response_range = c(analysis_plan$scales$response_min, analysis_plan$scales$response_max),
    prediction_cells = list(
      n_directional = sum(directional),
      n_directional_effect = sum(directional & effect & agrees),
      n_directional_opposite = sum(directional & effect & !agrees),
      n_directional_zero = sum(directional & !effect),
      n_open = sum(!directional),
      n_open_effect = sum(!directional & effect),
      n_open_zero = sum(!directional & !effect)
    )
  )
}

#' Summarise the earlier study the generating coefficients are calibrated to
#'
#' The generating file records the earlier study's counts in
#' `calibration_reference`; the number of coefficients, the count of intervals
#' that excluded zero and the share follow from them.
#'
#' @param truth Truth with `calibration_reference`.
#' @return List `n_observations`, `n_outcomes`, `n_predictors`,
#'   `n_coefficients`, `interval_level`, `n_including_zero`,
#'   `n_excluding_zero`, `share_including_zero` and `excluding_zero_range`
#'   (absolute posterior means of the coefficients whose interval excluded
#'   zero).
summarise_calibration_reference <- function(truth) {
  reference <- truth$calibration_reference
  needed <- c("n_observations", "n_outcomes", "n_predictors", "interval_level",
              "n_intervals_including_zero", "abs_posterior_mean_range_intervals_excluding_zero")
  missing <- setdiff(needed, names(reference))
  if (length(missing) > 0L) {
    stop("simulation_truth.yaml: calibration_reference lacks ", paste(missing, collapse = ", "), ".")
  }
  n_coefficients <- as.integer(reference$n_outcomes) * as.integer(reference$n_predictors)
  n_including <- as.integer(reference$n_intervals_including_zero)
  list(
    n_observations = as.integer(reference$n_observations),
    n_outcomes = as.integer(reference$n_outcomes),
    n_predictors = as.integer(reference$n_predictors),
    n_coefficients = n_coefficients,
    interval_level = as.numeric(reference$interval_level),
    n_including_zero = n_including,
    n_excluding_zero = n_coefficients - n_including,
    share_including_zero = n_including / n_coefficients,
    excluding_zero_range = sort(as.numeric(unlist(reference$abs_posterior_mean_range_intervals_excluding_zero)))
  )
}

# ---- the planted problems in the saved results ----------------------------------

#' Compare the planted exclusion cases with the records the exclusions removed
#'
#' The generating file plants records for every exclusion rule; the
#' participant flow records how many each rule removed. Quota-full exits leave
#' before the questionnaire and are counted beside the steps.
#'
#' @param truth Truth (`exclusions`, `covariates$gender_divers_n`, `n_total`).
#' @param participant_flow Target `report_participant_flow`.
#' @return List `rules` (tibble `rule`, `planted`, `removed`, in the order of
#'   the survey flow), `n_generated`, `n_started` and `n_analysis`.
compare_planted_exclusions <- function(truth, participant_flow) {
  steps <- participant_flow$steps
  removed_by <- function(criterion) {
    n <- steps$n_excluded[steps$criterion == criterion]
    if (length(n) == 1L) as.integer(n) else NA_integer_
  }
  # The generating file's name of each rule, and the step that applies it.
  step_of_rule <- c(
    consent_refused = "not_consenting", incomplete = "incomplete", age_under_18 = "age_outside_range",
    attention_1_failed = "attention_1_wrong", attention_2_failed = "attention_2_wrong"
  )
  unknown <- setdiff(names(truth$exclusions), c("quota_full", names(step_of_rule)))
  if (length(unknown) > 0L) {
    stop("compare_planted_exclusions(): no exclusion step is known for the planted rule(s) ",
         paste(unknown, collapse = ", "), ".")
  }
  rules <- c("quota_full", names(step_of_rule), "gender_divers")
  planted <- c(
    quota_full = truth$exclusions$quota_full,
    unlist(truth$exclusions[names(step_of_rule)]),
    gender_divers = truth$covariates$gender_divers_n
  )
  removed <- c(
    quota_full = participant_flow$n_quota_full,
    vapply(step_of_rule, removed_by, integer(1)),
    gender_divers = removed_by("gender_divers")
  )
  list(
    rules = tibble::tibble(rule = rules, planted = as.integer(planted[rules]), removed = as.integer(removed[rules])),
    n_generated = as.integer(truth$n_total),
    n_started = as.integer(participant_flow$n_started),
    n_analysis = as.integer(participant_flow$n_analysis)
  )
}

#' Compare the planted gaps with the cells the fill wrote
#'
#' The generating file blanks answers among the retained records. The
#' preregistered fill covers every scale item and the demographics of its
#' configured order; gender is never filled, and a respondent without gender
#' leaves the regressions.
#'
#' @param truth Truth (`missingness$cells`).
#' @param imputation_reporting_data Target `imputation_reporting_data`.
#' @param sample_sizes Target `report_sample_sizes`.
#' @param codebook Codebook from [zm_codebook()].
#' @param analysis_plan Configuration (`missing_data$fill$demographics$order`).
#' @return List `cells` (tibble `column`, `scale_key`, `position`, `planted`,
#'   `filled`, `fillable`) and the sample sizes `n_analysis`, `n_regressions`,
#'   `n_network`, `n_omitted_for_missing_gender`.
compare_planted_gaps <- function(truth, imputation_reporting_data, sample_sizes, codebook, analysis_plan) {
  cells <- truth$missingness$cells
  columns <- vapply(cells, function(cell) as.character(cell$column), character(1))
  planted <- vapply(cells, function(cell) as.integer(cell$n_missing), integer(1))
  written <- as.character(imputation_reporting_data$imputed_cells$variable)
  filled <- vapply(columns, function(column) sum(written == column), integer(1), USE.NAMES = FALSE)
  items <- locate_items_in_scales(columns, codebook)
  fillable <- !is.na(items$scale_key) | columns %in% analysis_plan$missing_data$fill$demographics$order
  cells <- tibble::tibble(column = columns, scale_key = items$scale_key, position = items$position,
                          planted = planted, filled = filled, fillable = fillable)
  # the item gaps first, scale by scale in the codebook's order, then the
  # demographic ones in the order of the generating file
  cells <- cells[order(is.na(cells$scale_key), match(cells$scale_key, codebook$scales$scale_key),
                       cells$position), , drop = FALSE]
  list(
    cells = cells,
    n_analysis = as.integer(sample_sizes$n_analysis),
    n_regressions = as.integer(sample_sizes$n_regressions),
    n_network = as.integer(sample_sizes$n_network),
    n_omitted_for_missing_gender = as.integer(sample_sizes$n_omitted_for_missing_gender)
  )
}

#' The planted item in the expected exploratory solutions
#'
#' Every expected-count solution with several factors that holds the item and
#' items of the scale it also loads on: the item's absolute loading on its own
#' factor and on the factor most items of that other scale are assigned to.
#'
#' @param item,own,other Item code, its scale and the scale of its second
#'   loading.
#' @param efa_loading_clarity Target `efa_loading_clarity`.
#' @param cutoff The preregistered loading threshold.
#' @return Tibble `set_name`, `factors`, `own_factor`, `other_factor`,
#'   `loading_own_factor`, `loading_other_factor`, `n_loadings_at_cutoff`.
locate_item_in_expected_solutions <- function(item, own, other, efa_loading_clarity, cutoff) {
  rows <- lapply(efa_loading_clarity$solutions, function(solution) {
    membership <- solution$membership
    if (!isTRUE(solution$expected_reference) || is.null(membership) || solution$factors < 2L) return(NULL)
    if (!item %in% membership$item || !any(membership$theoretical_scale %in% other)) return(NULL)
    other_factors <- membership$assigned_factor[membership$theoretical_scale %in% other]
    other_factor <- names(sort(table(other_factors), decreasing = TRUE))[1]
    own_factor <- membership$assigned_factor[membership$item == item][1]
    loadings <- abs(unclass(solution$loadings)[item, ])
    tibble::tibble(
      set_name = as.character(solution$set_name), factors = as.integer(solution$factors),
      own_factor = own_factor, other_factor = other_factor,
      loading_own_factor = unname(loadings[own_factor]),
      loading_other_factor = unname(loadings[other_factor]),
      n_loadings_at_cutoff = sum(loadings >= cutoff)
    )
  })
  dplyr::bind_rows(rows)
}

#' The planted item among the largest residuals of the confirmatory models
#'
#' Every confirmatory model with a factor for the item's own scale and one for
#' the scale it also loads on: the leading run of its ranked residuals (by
#' absolute standardised residual) that pair the item with another item.
#'
#' @param item,own,other See [locate_item_in_expected_solutions()].
#' @param cfa_reporting Target `confirmatory_factor_structure_reporting_data`.
#' @param codebook Codebook from [zm_codebook()].
#' @return List `models` (the qualifying model keys) and `leading` (tibble
#'   `model`, `rank`, `partner`, `partner_scale`, `partner_position`,
#'   `residual_correlation`, `standardised_residual`).
locate_item_in_cfa_residuals <- function(item, own, other, cfa_reporting, codebook) {
  loadings <- tibble::as_tibble(cfa_reporting$loadings)
  models <- unique(as.character(loadings$model))
  models <- models[vapply(models, function(model) {
    rows <- loadings[loadings$model == model, , drop = FALSE]
    item %in% rows$item && all(c(own, other) %in% rows$factor)
  }, logical(1))]
  residuals <- tibble::as_tibble(cfa_reporting$largest_residual_correlations)
  leading <- lapply(models, function(model) {
    r <- residuals[residuals$model == model, , drop = FALSE]
    r <- r[order(-abs(r$standardized_residual_correlation)), , drop = FALSE]
    involves <- r$item_1 == item | r$item_2 == item
    run <- if (length(involves) && involves[1]) seq_len(which(c(!involves, TRUE))[1] - 1L) else integer(0)
    if (length(run) == 0L) return(NULL)
    partner <- ifelse(r$item_1[run] == item, r$item_2[run], r$item_1[run])
    located <- locate_items_in_scales(partner, codebook)
    tibble::tibble(
      model = model, rank = run, partner = partner, partner_scale = located$scale_key,
      partner_position = located$position, residual_correlation = r$residual_correlation[run],
      standardised_residual = r$standardized_residual_correlation[run]
    )
  })
  list(models = models, leading = dplyr::bind_rows(leading))
}

#' Find the planted cross-loading in the saved measurement results
#'
#' The generator rescales a cross-loading item to unit variance
#' ([sim_items_from_latent()]), so its expected standardised loadings are the
#' two generating loadings divided by `sqrt(1 + c^2 + 2 * l * c * r)`, with `l`
#' the common loading, `c` the second loading and `r` the correlation of the
#' two latent scores in the generated data.
#'
#' @param truth Truth (`cross_loadings`, `measurement$loading`).
#' @param latent_scores Tibble from [read_generated_latent_scores()].
#' @param efa_loading_clarity Target `efa_loading_clarity`.
#' @param cfa_reporting Target `confirmatory_factor_structure_reporting_data`.
#' @param codebook Codebook from [zm_codebook()].
#' @param analysis_plan Configuration (`factor_analysis$loading_display_cutoff`).
#' @return List, one entry per planted cross-loading: `item`, `scale_key`,
#'   `position`, `other_key`, `loading`, `cross_loading`, `latent_correlation`,
#'   `expected_loading`, `expected_cross_loading`, `cutoff`, `exploratory`
#'   ([locate_item_in_expected_solutions()]) and `confirmatory`
#'   ([locate_item_in_cfa_residuals()]).
locate_planted_cross_loadings <- function(truth, latent_scores, efa_loading_clarity, cfa_reporting,
                                          codebook, analysis_plan) {
  cutoff <- as.numeric(analysis_plan$factor_analysis$loading_display_cutoff)
  loading <- as.numeric(truth$measurement$loading)
  lapply(truth$cross_loadings, function(entry) {
    item <- as.character(entry$item)
    other <- as.character(entry$factor)
    located <- locate_items_in_scales(item, codebook)
    own <- located$scale_key
    cross <- as.numeric(entry$loading)
    rho <- stats::cor(latent_scores[[own]], latent_scores[[other]])
    v <- 1 + cross^2 + 2 * loading * cross * rho
    list(
      item = item, scale_key = own, position = located$position, other_key = other,
      loading = loading, cross_loading = cross, latent_correlation = rho,
      expected_loading = loading / sqrt(v), expected_cross_loading = cross / sqrt(v), cutoff = cutoff,
      exploratory = locate_item_in_expected_solutions(item, own, other, efa_loading_clarity, cutoff),
      confirmatory = locate_item_in_cfa_residuals(item, own, other, cfa_reporting, codebook)
    )
  })
}

#' Find the planted heavy-tailed residual in the saved model checks
#'
#' The preregistered rule is one-sided: an outcome has heavier than normal
#' tails when its excess kurtosis or its maximum lies above, or its minimum
#' below, the central band of the data sets simulated from the fitted model.
#'
#' @param truth Truth (`residual_contamination`).
#' @param model_checks Target `supplement_model_checks`.
#' @param analysis_plan Configuration (`sensitivity$student_t$trigger_stats`).
#' @return `NULL` without a planted contamination, else list `outcome`,
#'   `share`, `multiplier`, `statistics` (tibble `stat`, `observed`, `lower`,
#'   `upper`, `beyond`), `triggered` and `refitted` (outcomes).
locate_planted_heavy_tail <- function(truth, model_checks, analysis_plan) {
  contamination <- truth$residual_contamination
  if (is.null(contamination)) return(NULL)
  outcome <- as.character(contamination$outcome)
  rule_stats <- as.character(analysis_plan$sensitivity$student_t$trigger_stats)
  checks <- tibble::as_tibble(model_checks$posterior_predictive)
  rows <- checks[checks$outcome == outcome & checks$stat %in% rule_stats, , drop = FALSE]
  rows <- rows[order(match(rows$stat, rule_stats)), , drop = FALSE]
  beyond <- ifelse(rows$stat == "min", rows$observed < rows$rep_lo, rows$observed > rows$rep_hi)
  list(
    outcome = outcome,
    share = as.numeric(contamination$share),
    multiplier = as.numeric(contamination$multiplier),
    statistics = tibble::tibble(stat = rows$stat, observed = rows$observed, lower = rows$rep_lo,
                                upper = rows$rep_hi, beyond = beyond),
    triggered = as.character(model_checks$answer$triggered),
    refitted = as.character(model_checks$answer$refitted)
  )
}

#' Find the predictors whose generating coefficients differ in sign across outcomes
#'
#' @param comparison Generating rows from [tabulate_generating_coefficients()].
#' @param analysis_plan Configuration (`predictions$table`).
#' @return The comparison rows of those predictors' nonzero coefficients, with
#'   their `predicted_sign` (`NA` off the prediction table).
locate_planted_sign_contrasts <- function(comparison, analysis_plan) {
  nonzero <- comparison[comparison$generating != 0, , drop = FALSE]
  both <- tapply(sign(nonzero$generating), nonzero$predictor, function(s) all(c(-1, 1) %in% s))
  contrasts <- names(both)[both %in% TRUE]
  rows <- nonzero[nonzero$predictor %in% contrasts, , drop = FALSE]
  dplyr::left_join(rows, tabulate_predicted_signs(analysis_plan), by = c("outcome", "predictor"))
}

#' Check which contingency rules of the plan this run gave a case
#'
#' Counts, for each preregistered contingency, the cases this run met and the
#' units it checked: respondents dropped for excessive missingness, scales that
#' fell back to alpha or to alpha's interval, confirmatory models that did not
#' converge or were inadmissible, regression and joint fits that needed a retry
#' or failed their sampling checks, network resamples that needed a retry or
#' failed, and items with at least two loadings at the preregistered cutoff.
#'
#' @param participant_flow Target `report_participant_flow`.
#' @param reliability_counts Target `report_reliability_counts`.
#' @param cfa_reporting Target `confirmatory_factor_structure_reporting_data`.
#' @param fit_register Target `regression_fit_register`.
#' @param joint_posterior Target `joint_posterior` (`validity`).
#' @param network_bootstrap_fits Target `network_bootstrap_fits`.
#' @param efa_loading_clarity Target `efa_loading_clarity`.
#' @return Tibble `contingency`, `n_cases`, `n_checked`, with the attribute
#'   `fits` (counts of the fitted regressions by role: `primary`, `sweep`,
#'   `student_refit`).
check_contingencies_exercised <- function(participant_flow, reliability_counts, cfa_reporting, fit_register,
                                          joint_posterior, network_bootstrap_fits, efa_loading_clarity) {
  drop_step <- participant_flow$steps[participant_flow$steps$criterion == "dropped_unfillable_gaps", , drop = FALSE]
  status <- tibble::as_tibble(cfa_reporting$model_status)
  fitted <- Filter(function(record) isTRUE(record$fit_available), fit_register)
  fit_status <- vapply(fitted, function(record) as.character(record$gate_status), character(1))
  roles <- vapply(fitted, function(record) as.character(record$role), character(1))
  joint_status <- as.character(joint_posterior$validity$gate_status)
  network_fits <- network_bootstrap_fits$fits
  network_failed <- sum(!vapply(network_fits, function(fit) isTRUE(fit$succeeded), logical(1)))
  network_retried <- sum(vapply(network_fits, function(fit) NROW(fit$attempts) > 1L, logical(1)))
  items <- efa_loading_clarity$loading_items
  out <- tibble::tibble(
    contingency = c("excessive_missingness", "reliability_fallback", "failed_confirmatory_model",
                    "regression_retry_or_failure", "network_retry_or_failure", "cross_loading"),
    n_cases = as.integer(c(
      sum(drop_step$n_excluded),
      reliability_counts$n_alpha_reported + reliability_counts$n_omega_without_interval,
      sum(!(status$converged %in% TRUE & status$admissible %in% TRUE)),
      sum(fit_status != "ok") + sum(!joint_status %in% "ok"),
      network_failed + network_retried,
      sum(items$crossloading %in% TRUE)
    )),
    n_checked = as.integer(c(
      sum(drop_step$n_before),
      reliability_counts$n_scales,
      nrow(status),
      length(fitted) + 1L,
      length(network_fits),
      nrow(unique(items[, c("set_name", "factors")]))
    ))
  )
  attr(out, "fits") <- c(primary = sum(roles == "primary"), sweep = sum(roles == "sweep"),
                         student_refit = sum(roles == "student_refit"))
  out
}

#' Assemble numeric values for compact synthetic-result exports
#'
#' Describes the generating design and handling of planted problems in the
#' synthetic preregistration data from saved results and configuration.
#'
#' @param truth Truth from [read_export_generating_values()].
#' @param latent_scores Tibble from [read_generated_latent_scores()].
#' @param participant_flow Target `report_participant_flow`.
#' @param sample_sizes Target `report_sample_sizes`.
#' @param imputation_reporting_data Target `imputation_reporting_data`.
#' @param reliability_counts Target `report_reliability_counts`.
#' @param efa_loading_clarity Target `efa_loading_clarity`.
#' @param cfa_reporting Target `confirmatory_factor_structure_reporting_data`.
#' @param model_checks Target `supplement_model_checks`.
#' @param fit_register Target `regression_fit_register`.
#' @param joint_posterior Target `joint_posterior`.
#' @param network_bootstrap_fits Target `network_bootstrap_fits`.
#' @param codebook Codebook from [zm_codebook()].
#' @param analysis_plan Configuration from [zm_config()].
#' @return List `scenario`, `design`, `calibration`,
#'   `exclusions`, `gaps`, `cross_loadings`, `heavy_tail`, `sign_contrasts`
#'   and `contingencies`.
assemble_synthetic_report_values <- function(truth, latent_scores, participant_flow,
                                             sample_sizes, imputation_reporting_data, reliability_counts,
                                             efa_loading_clarity, cfa_reporting, model_checks, fit_register,
                                             joint_posterior, network_bootstrap_fits, codebook, analysis_plan) {
  list(
    scenario = truth$scenario,
    design = summarise_generating_design(truth, analysis_plan),
    calibration = summarise_calibration_reference(truth),
    exclusions = compare_planted_exclusions(truth, participant_flow),
    gaps = compare_planted_gaps(truth, imputation_reporting_data, sample_sizes, codebook, analysis_plan),
    cross_loadings = locate_planted_cross_loadings(truth, latent_scores, efa_loading_clarity, cfa_reporting,
                                                   codebook, analysis_plan),
    heavy_tail = locate_planted_heavy_tail(truth, model_checks, analysis_plan),
    sign_contrasts = locate_planted_sign_contrasts(tabulate_generating_coefficients(truth), analysis_plan),
    contingencies = check_contingencies_exercised(participant_flow, reliability_counts, cfa_reporting,
                                                  fit_register, joint_posterior, network_bootstrap_fits,
                                                  efa_loading_clarity)
  )
}

# ---- shared wording and formatting helpers -------------------------------------

#' Join words as "a, b and c"
#'
#' @param x Character vector.
#' @return Character scalar.
join_words_with_and <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(x)]
  if (length(x) <= 1L) return(paste(x, collapse = ""))
  paste0(paste(x[-length(x)], collapse = ", "), " and ", x[length(x)])
}

#' Name scales in running text
#'
#' The report's construct label, lower-cased unless it opens with an
#' abbreviation ("ASC aggression" and "SDO-D" stay as they are).
#'
#' @param keys Scale keys.
#' @param labels Labels from [rh_labels()].
#' @return Character vector.
name_scales_in_text <- function(keys, labels) {
  x <- rh_label(keys, labels)
  ifelse(grepl("^[A-Z][a-z]", x), paste0(tolower(substr(x, 1, 1)), substring(x, 2)), x)
}

#' Name items in running text, grouped by scale ("ASC submission items 1 and 4")
#'
#' @param scale_keys,positions Scale and position of each item.
#' @param labels Labels from [rh_labels()].
#' @return Character scalar.
name_items_in_text <- function(scale_keys, positions, labels) {
  parts <- vapply(unique(scale_keys), function(key) {
    p <- positions[scale_keys == key]
    paste0(name_scales_in_text(key, labels), if (length(p) > 1L) " items " else " item ",
           join_words_with_and(rh_fmt_n(p)))
  }, character(1))
  join_words_with_and(parts)
}

#' Name a predictor in running text
#'
#' @param keys Predictor keys of the generating file (motive keys, `age`,
#'   `male`, `income`).
#' @param labels Labels from [rh_labels()].
#' @return Character vector.
name_predictors_in_text <- function(keys, labels) {
  covariates <- c(age = "age", male = "gender", income = "income")
  out <- name_scales_in_text(keys, labels)
  out[keys %in% names(covariates)] <- unname(covariates[keys[keys %in% names(covariates)]])
  out
}

#' An estimate with its interval in one cell, "0.22 [0.14, 0.29]"
#'
#' @param estimate,lower,upper Numeric vectors.
#' @return Character vector; "—" without an estimate.
format_estimate_with_interval <- function(estimate, lower, upper) {
  out <- paste(rh_fmt(estimate), rh_fmt_ci(lower, upper))
  out[is.na(estimate)] <- "—"
  out
}

#' Save the generating quantities used in the synthetic Methods
build_synthetic_methods_reporting_data <- function(truth, latent, codebook, analysis_plan) {
  design <- summarise_generating_design(truth, analysis_plan)
  excluded <- unlist(truth$exclusions)
  cross_loading <- NULL
  if (length(truth$cross_loadings)) {
    entry <- truth$cross_loadings[[1L]]
    located <- locate_items_in_scales(as.character(entry$item), codebook)
    own <- located$scale_key
    other <- as.character(entry$factor)
    cross <- as.numeric(entry$loading)
    correlation <- stats::cor(latent[[own]], latent[[other]])
    variance <- 1 + cross^2 + 2 * design$loading * cross * correlation
    cross_loading <- list(located = located, own = own, other = other, loading = cross,
      correlation = correlation, variance = variance,
      standardised_loading = design$loading / sqrt(variance))
  }
  list(truth = truth, design = design, excluded = excluded,
    attention = sum(excluded[grepl("^attention_", names(excluded))]),
    n_kept = as.integer(truth$n_total) - sum(excluded),
    n_divers = as.integer(truth$covariates$gender_divers_n),
    n_analysis = as.integer(truth$n_total) - sum(excluded) - as.integer(truth$covariates$gender_divers_n),
    n_after_quota = as.integer(truth$n_total) - excluded[["quota_full"]],
    n_excluded_planned = sum(excluded) - excluded[["quota_full"]],
    n_motives = length(analysis_plan$regression$motives),
    n_facets = length(analysis_plan$regression$outcomes),
    n_scales = nrow(codebook$scales), n_items = sum(codebook$scales$item_count),
    n_categories = design$response_range[2] - design$response_range[1] + 1,
    location_range = range(unlist(truth$measurement$scale_location)),
    cross_loading = cross_loading)
}
