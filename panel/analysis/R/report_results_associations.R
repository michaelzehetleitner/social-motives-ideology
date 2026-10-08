# Report targets of the Results section "Adjusted associations (RQ1)".
#
# Each Look-2 function builds one report target or one step of it (_targets.R,
# REPORTING · RESULTS · ADJUSTED ASSOCIATIONS). The report words and formats
# what these return; it computes nothing.

#' One row per preregistered cell, as the associations table prints it
#'
#' @param regression_prediction_decisions Tibble from
#'   [extract_regression_prediction_decisions()].
#' @param report_sample_sizes List from [report_analysis_sample_sizes()].
#' @param analysis_plan Configuration from [zm_config()]; the row order.
#' @return Tibble with one row per cell.
report_prediction_cells <- function(regression_prediction_decisions, report_sample_sizes, analysis_plan) {
  cells <- tibble::tibble(
    outcome_key = regression_prediction_decisions$outcome_key,
    outcome = regression_prediction_decisions$outcome,
    predictor = regression_prediction_decisions$predictor,
    predicted_sign = regression_prediction_decisions$predicted_sign,
    estimate = regression_prediction_decisions$estimate,
    interval_lower = regression_prediction_decisions$q_lo,
    interval_upper = regression_prediction_decisions$q_hi,
    fit_valid = regression_prediction_decisions$fit_valid,
    credible = regression_prediction_decisions$credible,
    observed_sign = regression_prediction_decisions$observed_sign,
    verdict = regression_prediction_decisions$verdict,
    n = report_sample_sizes$n_regressions
  )
  per_outcome <- report_sample_sizes$n_regressions_by_outcome
  if (length(per_outcome)) {
    matched <- match(cells$outcome_key, names(per_outcome))
    cells$n[!is.na(matched)] <- unname(per_outcome[matched[!is.na(matched)]])
  }
  # rows in the order of the regression outcomes, and within a facet of the motives
  cells[order(match(cells$outcome_key, analysis_plan$regression$outcomes),
              match(cells$predictor, analysis_plan$regression$motives)), , drop = FALSE]
}

#' Whether each cell's classification is the same under every prior width
#'
#' Read from the stability result of the prior widths. Decided only when every
#' width's fit of that outcome passed the validity gate; otherwise `NA`.
#'
#' @param report_prediction_cells Tibble of cells.
#' @param regression_prior_width_sensitivity List from
#'   [extract_regression_prior_width_sensitivity()].
#' @param regression_fit_register The fit register.
#' @param codebook Codebook from [zm_codebook()].
#' @return The cells with `prior_robust`.
report_prior_width_robustness <- function(report_prediction_cells, regression_prior_width_sensitivity,
                                          regression_fit_register, codebook) {
  stability <- report_flatten_stability(regression_prior_width_sensitivity)
  coefficient <- paste0("b_", zm_z_col(report_prediction_cells$predictor, codebook))
  matched <- match(paste(report_prediction_cells$outcome, coefficient),
                   paste(stability$outcome, stability$coefficient))
  all_widths_valid <- report_all_prior_widths_valid(regression_fit_register)
  comparable <- unname(all_widths_valid[report_prediction_cells$outcome_key])
  report_prediction_cells$prior_robust <- ifelse(
    comparable, stability$interval_exclusion_stable[matched], NA
  )
  report_prediction_cells
}

#' The counts the RQ1 prose states
#'
#' @param report_association_table Tibble from the association pipe.
#' @return List of counts.
report_count_prediction_verdicts <- function(report_association_table) {
  # the directional predictions are the cells marked + or - (RQ1, Table 3)
  directional <- report_association_table$predicted_sign %in% c("+", "-")
  list(
    n_cells = nrow(report_association_table),
    n_directional = sum(directional),
    n_directional_withheld = sum(directional & !report_association_table$fit_valid %in% TRUE),
    n_by_verdict = table(report_association_table$verdict, useNA = "no"),
    n_prior_comparable = sum(!is.na(report_association_table$prior_robust)),
    n_prior_dependent = sum(report_association_table$prior_robust %in% FALSE)
  )
}

#' Per facet: whether its model is interpretable, and its credible cells
#'
#' The facets and, within a facet, the credible cells keep the order of the
#' association table, which is the codebook's order of the facets and of the
#' motives ([report_prediction_cells()]).
#'
#' @param report_association_table Tibble from the association pipe.
#' @return Tibble with one row per facet.
report_credible_cells_by_outcome <- function(report_association_table) {
  keys <- report_association_table$outcome_key
  by_outcome <- split(report_association_table, factor(keys, levels = unique(keys)))
  dplyr::bind_rows(lapply(by_outcome, function(cells) {
    credible <- cells[cells$credible %in% TRUE, , drop = FALSE]
    tibble::tibble(
      outcome_key = cells$outcome_key[1],
      interpretable = all(cells$fit_valid %in% TRUE),
      predictor = list(credible$predictor),
      observed_sign = list(credible$observed_sign)
    )
  }))
}

#' The prediction cells whose classification or direction changes across widths
#'
#' With the prior widths under which the interval excludes or includes zero,
#' and under which the posterior median is positive or negative.
#'
#' @param report_association_table Tibble of the prediction cells.
#' @param regression_prior_width_sensitivity List from
#'   [extract_regression_prior_width_sensitivity()].
#' @param codebook Codebook from [zm_codebook()].
#' @return Tibble with one row per changed cell.
report_cells_changing_with_prior_width <- function(report_association_table, regression_prior_width_sensitivity,
                                       codebook) {
  stability <- report_flatten_stability(regression_prior_width_sensitivity)
  coefficient <- paste0("b_", zm_z_col(report_association_table$predictor, codebook))
  matched <- match(paste(report_association_table$outcome, coefficient),
                   paste(stability$outcome, stability$coefficient))
  cells <- dplyr::bind_cols(report_association_table[c("outcome_key", "predictor")],
                            stability[matched, setdiff(names(stability), c("outcome", "coefficient"))])
  changed <- !cells$interval_exclusion_stable | !cells$median_direction_stable
  cells[changed %in% TRUE, , drop = FALSE]
}

# Look 3 ----------------------------------------------------------------------

#' Technical reshape — the stability result as one row per outcome and coefficient
#'
#' @param stability List outcome -> coefficient -> width from
#'   [extract_regression_prior_width_sensitivity()].
#' @return Tibble.
report_flatten_stability <- function(stability) {
  rows <- unlist(stability, recursive = FALSE)
  dplyr::bind_rows(lapply(rows, function(cell) {
    widths <- names(cell$by_width)
    excludes <- vapply(cell$by_width, function(w) isTRUE(w$interval_excludes_zero), logical(1))
    medians <- vapply(cell$by_width, function(w) w$median, numeric(1))
    tibble::tibble(
      outcome = cell$outcome,
      coefficient = cell$coefficient,
      interval_exclusion_stable = cell$interval_exclusion_stable,
      median_direction_stable = cell$median_direction_stable,
      widths_excluding_zero = list(widths[excludes]),
      widths_including_zero = list(widths[!excludes]),
      widths_positive_median = list(widths[medians > 0]),
      widths_negative_median = list(widths[medians < 0])
    )
  }))
}

#' Technical lookup — per outcome, did every prior width's fit pass the gate?
#'
#' @param regression_fit_register The fit register.
#' @return Named logical vector keyed by outcome key.
report_all_prior_widths_valid <- function(regression_fit_register) {
  width_fits <- Filter(function(record) record$role %in% c("primary", "sweep"), regression_fit_register)
  valid <- vapply(width_fits, function(record) isTRUE(record$fit_valid), logical(1))
  outcome <- vapply(width_fits, function(record) record$outcome_key, character(1))
  vapply(split(valid, outcome), all, logical(1))
}

# ---- the tables of the section, built from the accepted results ----------

#' Coefficient rows of the roles a consumer reads
#'
#' The coefficient evidence of the accepted result, joined by fit identity with
#' the gate fields, the fitted N and the iterations the register carries.
#'
#' @param summaries `regression_coefficient_summaries`.
#' @param register Records from `extract_regression_result_records()`.
#' @param analysis_plan Analysis configuration.
#' @param roles Model roles to keep.
#' @param emit_role The `role` value of the rows (the Student-t coefficient
#'   table labels its rows `"student"`); `NULL` keeps the role.
#' @return Tibble of coefficient rows (interval columns named by their
#'   quantile, fit-level gate fields), plus `variable`, the
#'   highest-density interval and the stable outcome identity.
tabulate_regression_coefficients <- function(summaries, register, analysis_plan, roles, emit_role = NULL) {
  rows <- tibble::as_tibble(summaries)
  rows <- rows[rows$role %in% roles, , drop = FALSE]
  gate <- zm_regression_register_table(register, analysis_plan)
  index <- match(
    zm_regression_fit_key(rows$outcome_key, rows$slope_sd, rows$role, rows$family),
    zm_regression_fit_key(gate$outcome, gate$slope_sd, gate$role, gate$family)
  )
  if (anyNA(index)) {
    stop("tabulate_regression_coefficients(): the fit register has no gate row for ",
         paste(unique(zm_regression_fit_key(rows$outcome_key, rows$slope_sd, rows$role,
                                            rows$family)[is.na(index)]), collapse = ", "), ".")
  }
  gate <- gate[index, , drop = FALSE]
  ci <- zm_regression_ci_names(analysis_plan)
  out <- tibble::tibble(
    label = as.character(rows$label),
    outcome = as.character(rows$outcome_key),
    slope_sd = as.numeric(rows$slope_sd),
    role = if (is.null(emit_role)) as.character(rows$role) else emit_role,
    term = as.character(zm_regression_column(rows, "term", NA_character_)),
    term_type = as.character(zm_regression_column(rows, "term_type", NA_character_)),
    estimate = as.numeric(zm_regression_column(rows, "estimate", NA_real_)),
    sd = as.numeric(zm_regression_column(rows, "sd", NA_real_)),
    q_lo = as.numeric(zm_regression_column(rows, "q_lo", NA_real_)),
    q_hi = as.numeric(zm_regression_column(rows, "q_hi", NA_real_)),
    p_positive = as.numeric(zm_regression_column(rows, "p_positive", NA_real_)),
    p_negative = as.numeric(zm_regression_column(rows, "p_negative", NA_real_)),
    ess_bulk = as.numeric(zm_regression_column(rows, "ess_bulk", NA_real_)),
    ess_tail = as.numeric(zm_regression_column(rows, "ess_tail", NA_real_)),
    rhat = as.numeric(zm_regression_column(rows, "rhat", NA_real_)),
    mcse_median = as.numeric(zm_regression_column(rows, "mcse_median", NA_real_)),
    mcse_q_lo = as.numeric(zm_regression_column(rows, "mcse_q_lo", NA_real_)),
    mcse_q_hi = as.numeric(zm_regression_column(rows, "mcse_q_hi", NA_real_)),
    fit_valid = as.logical(rows$fit_valid),
    gate_status = as.character(rows$gate_status),
    ess_ok = as.logical(gate$ess_ok),
    rhat_ok = as.logical(gate$rhat_ok),
    divergences_ok = as.logical(gate$divergences_ok),
    treedepth_ok = as.logical(gate$treedepth_ok),
    bfmi_ok = as.logical(gate$bfmi_ok),
    ess_target = as.numeric(gate$ess_target),
    rhat_limit = as.numeric(gate$rhat_limit),
    n_divergent = as.integer(gate$n_divergent),
    n_treedepth_hits = as.integer(gate$n_treedepth_hits),
    bfmi_min = as.numeric(gate$bfmi_min),
    mcse_median_max = as.numeric(gate$mcse_median_max),
    mcse_q_max = as.numeric(gate$mcse_q_max),
    n_obs = as.integer(gate$n_obs),
    iter_used = as.integer(gate$iter_used),
    family = as.character(rows$family),
    nu_fixed = as.numeric(rows$nu_fixed),
    variable = as.character(zm_regression_column(rows, "variable", NA_character_)),
    hdi_lo = as.numeric(zm_regression_column(rows, "hdi_lo", NA_real_)),
    hdi_hi = as.numeric(zm_regression_column(rows, "hdi_hi", NA_real_)),
    outcome_key = as.character(rows$outcome_key),
    response_column = as.character(rows$response_column),
    fit_available = as.logical(rows$fit_available),
    note = as.character(rows$note)
  )
  names(out)[names(out) == "q_lo"] <- ci[1]
  names(out)[names(out) == "q_hi"] <- ci[2]
  out
}

#' Save the coefficient-difference and motive-partial facts used in the prose
add_association_reporting_facts <- function(counts, associations, differences, partials, analysis_plan) {
  predicted <- differences[differences$credible %in% TRUE & differences$median > 0, , drop = FALSE]
  opposite <- differences[differences$credible %in% TRUE & differences$median < 0, , drop = FALSE]
  added <- predicted[!(predicted$positive_credible %in% TRUE & predicted$negative_credible %in% TRUE), , drop = FALSE]
  counts$differences <- list(interpretable = any(differences$interpretable %in% TRUE),
    n = nrow(differences), predicted = predicted, opposite = opposite, added = added,
    n_predicted = nrow(predicted), n_opposite = nrow(opposite), n_added = nrow(added))
  open <- associations[associations$fit_valid %in% TRUE & associations$credible %in% FALSE, , drop = FALSE]
  credible <- partials[partials$credible %in% TRUE, , drop = FALSE]
  overlapping <- unique(c(credible$motive_a, credible$motive_b))
  top <- credible[order(-abs(credible$median)), , drop = FALSE][seq_len(min(2L, nrow(credible))), , drop = FALSE]
  counts$partials <- list(interpretable = any(partials$interpretable %in% TRUE),
    n_open = nrow(open), n_open_overlap = sum(open$predictor %in% overlapping), top = top)
  counts$n_prior_unchanged <- counts$n_prior_comparable - counts$n_prior_dependent
  counts$n_prior_widths <- length(as.numeric(analysis_plan$priors$slope_sd_sweep))
  counts
}
