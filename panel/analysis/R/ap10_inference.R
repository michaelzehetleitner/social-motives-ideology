# AP10 inference criteria of the regressions
#
# Implements AP10: a coefficient is credible when its credible interval
# under the PRIMARY slope prior excludes zero. A cell of Table 3 with a
# predicted sign is confirmed when credible with that sign, disconfirmed when
# credible with the opposite sign; an interval covering zero gets no verdict
# (AP10: not evidence of absence). A cell marked ± (either direction possible) or left empty is
# exploratory: credibility and sign are reported, no directional verdict
# (RQ1, AP10). The prior sweep and priorsense are
# descriptive: their comparisons do not change the primary directional verdict.
# Every cell decision
# is gated on the validity gate of its fit (bulk and tail ESS, R-hat,
# divergent transitions, tree-depth hits, BFMI; ap6_fit_diagnostics()): a
# fit whose gate_status is "not_interpretable" after the failure rule of
# apply_validity_gate() gets no verdict (NA).
#
# The scientific rules are implemented in the named functions below.
# analysis_plan$inference is their descriptive mirror; its prose is not
# executed. Prediction signs and numeric settings are read
# from analysis_plan$predictions, analysis_plan$priors and analysis_plan$regression (analysis_plan.yaml).

#' Credibility rule: the interval excludes zero
#'
#' @param lo Lower interval bounds.
#' @param hi Upper interval bounds.
#' @return Logical vector (`NA` where a bound is `NA`).
ap7_credible <- function(lo, hi) {
  out <- lo > 0 | hi < 0
  out[is.na(lo) | is.na(hi)] <- NA
  out
}

#' Gate statuses under which a fit decides
#'
#' The statuses of a fit after the failure rule of
#' `analysis_plan$regression$validity_gate`: `"ok"` (passed without retry) and
#' `"retried_ok"` (passed after the automated retry). `"not_interpretable"`
#' withholds every AP10 verdict of the fit.
#'
#' @return Character vector.
ap7_gate_statuses_ok <- function() {
  c("ok", "retried_ok")
}

#' Sign of a point estimate as `"+"`, `"-"` or `""`
#'
#' @param x Numeric vector.
#' @return Character vector (`NA` where `x` is `NA`).
ap7_sign <- function(x) {
  out <- rep(NA_character_, length(x))
  ok <- !is.na(x)
  out[ok] <- ifelse(x[ok] > 0, "+", ifelse(x[ok] < 0, "-", ""))
  out
}

#' Does a cell of Table 3 carry a directional prediction?
#'
#' `"+"` and `"-"` predict a direction. `"±"` (either direction possible) and
#' an empty cell predict none: such a cell is judged by credibility and sign
#' only and receives no directional verdict (RQ1, AP10).
#'
#' @param predicted_sign Character vector of configured signs.
#' @return Logical vector, `TRUE` for `"+"` and `"-"`.
ap7_is_directional <- function(predicted_sign) {
  predicted_sign %in% c("+", "-")
}

# ---- AP10 — the Table 3 decision route ----

# For a cell without a directional prediction, exploratory_credible means only
# that its interval excludes zero; it does not corroborate a prediction. An
# interval including zero does not establish absence or equality.

# BEGIN GENERATED PARAMETER CARD: RQ1 PREDICTION SIGNS
# Automatically generated from analysis_plan.yaml
# Predicted sign of each motive's slope   (predictions.table; ± = either direction; . = unspecified)
#             zm_security    zm_arousal     zm_power       zm_prestige    zm_achievement
#   asc_agg   +              -              +              ±              -
#   asc_sub   +              -              -              ±              -
#   asc_conv  +              -              +              ±              -
#   sdo_dom   +              -              +              ±              -
# END GENERATED PARAMETER CARD: RQ1 PREDICTION SIGNS
#' Apply the registered Table 3 rules to the four primary Gaussian regressions.
#'
#' AP10 — Directional prediction decisions; analysis_plan.yaml:
#' predictions.table and inference. Only primary normal(0, .20) Gaussian fits
#' determine Table 3. A central 95% interval excluding zero in the predicted
#' direction is confirmed; exclusion in the opposite direction is disconfirmed;
#' an interval including zero receives no verdict. Cells without a directional
#' prediction report credibility and sign only. Invalid or unavailable final
#' gate results receive no verdict. The prior-width, Student and joint
#' summaries do not replace these decisions.
#'
#' @param coefficient_summaries The `regression_coefficient_summaries` result.
#' @param analysis_plan Configuration from [zm_config()].
#' @return Tibble with one row per prediction cell: `outcome_key`, `outcome`,
#'   `predictor`, `predicted_sign`, the matched coefficient columns, the fit's
#'   `fit_valid`, `gate_status` and `note`, and `credible_raw`, `credible`,
#'   `observed_sign`, `verdict`.
extract_regression_prediction_decisions <- function(coefficient_summaries, analysis_plan) {
  primary_coefficients <- coefficient_summaries |>
    dplyr::filter(
      .data$role == "primary",
      .data$family == analysis_plan$regression$family,
      dplyr::near(.data$slope_sd, analysis_plan$priors$slope_sd_primary)
    )
  prediction_cells <- ap7_match_regression_predictions(primary_coefficients, analysis_plan)
  prediction_cells |>
    dplyr::mutate(
      credible_raw = .data$q_lo > 0 | .data$q_hi < 0,
      credible = dplyr::if_else(.data$fit_valid, .data$credible_raw, NA),
      observed_sign = dplyr::case_when(
        .data$estimate > 0 ~ "+",
        .data$estimate < 0 ~ "-",
        .data$estimate == 0 ~ ""
      ),
      verdict = dplyr::case_when(
        is.na(.data$credible) ~ NA_character_,
        !ap7_is_directional(.data$predicted_sign) & .data$credible ~ "exploratory_credible",
        !ap7_is_directional(.data$predicted_sign) ~ "exploratory_not_credible",
        .data$credible & .data$observed_sign == .data$predicted_sign ~ "confirmed",
        .data$credible ~ "disconfirmed",
        TRUE ~ NA_character_
      )
    )
}

#' Match the configured prediction cells to the primary coefficient rows
#'
#' Converts the configured prediction table to its outcome-by-motive cells and
#' matches the primary coefficient rows to them. A cell without a coefficient
#' row stays in the table with no interval or verdict.
#'
#' @param primary_coefficients The primary Gaussian rows of
#'   `regression_coefficient_summaries`.
#' @param analysis_plan Configuration from [zm_config()].
#' @return Tibble of the prediction cells with their coefficient rows.
ap7_match_regression_predictions <- function(primary_coefficients, analysis_plan) {
  predictions <- lapply(names(analysis_plan$predictions$table), function(outcome) {
    cells <- analysis_plan$predictions$table[[outcome]]
    tibble::tibble(
      # the prediction table is keyed by the configured outcome key, not the readable name
      outcome_key = outcome,
      predictor = names(cells),
      predicted_sign = vapply(cells, function(sign) {
        if (is.null(sign) || is.na(sign)) "" else as.character(sign)
      }, character(1))
    )
  }) |>
    dplyr::bind_rows()
  # the keys of both sides are resolved against the model set before the join
  unknown <- setdiff(predictions$outcome_key, as.character(analysis_plan$regression$outcomes))
  if (length(unknown) > 0) {
    stop("The prediction table names outcome key(s) the model set does not declare: ",
         paste(unique(unknown), collapse = ", "), ".")
  }
  unknown <- setdiff(primary_coefficients$outcome_key, as.character(analysis_plan$regression$outcomes))
  if (length(unknown) > 0) {
    stop("A primary coefficient row names an outcome key the model set does not declare: ",
         paste(unique(unknown), collapse = ", "), ".")
  }
  # no cell of the prediction table may be stated twice
  cell <- paste(predictions$outcome_key, predictions$predictor)
  if (anyDuplicated(cell) > 0) {
    stop("More than one prediction for cell(s): ",
         paste(unique(cell[duplicated(cell)]), collapse = ", "), ".")
  }
  columns <- c("term", "term_type", "estimate", "q_lo", "q_hi")
  for (column in setdiff(columns, names(primary_coefficients))) {
    primary_coefficients[[column]] <- if (column %in% c("term", "term_type")) NA_character_ else NA_real_
  }
  # the status of a fit carries its readable name beside the key the join uses
  fit_status <- primary_coefficients |>
    dplyr::distinct(.data$outcome, .data$outcome_key, .data$fit_valid, .data$gate_status, .data$note)
  if (anyDuplicated(fit_status$outcome_key) > 0) {
    stop("More than one fit status for outcome key(s): ",
         paste(unique(fit_status$outcome_key[duplicated(fit_status$outcome_key)]), collapse = ", "), ".")
  }
  coefficients <- primary_coefficients |>
    dplyr::filter(.data$term_type == "predictor") |>
    # `outcome` travels with the status rows, so only the key remains on the coefficients
    dplyr::select(-dplyr::all_of(c("outcome", "fit_valid", "gate_status", "note"))) |>
    dplyr::rename(predictor = "term")
  # a cell is decided by one coefficient or by none
  cell <- paste(coefficients$outcome_key, coefficients$predictor)
  if (anyDuplicated(cell) > 0) {
    stop("More than one primary coefficient for: ",
         paste(unique(cell[duplicated(cell)]), collapse = ", "), ".")
  }
  predictions |>
    dplyr::left_join(coefficients, by = c("outcome_key", "predictor")) |>
    dplyr::left_join(fit_status, by = "outcome_key") |>
    # the readable name stands beside its key, ahead of the cell
    dplyr::relocate("outcome", .after = "outcome_key")
}
