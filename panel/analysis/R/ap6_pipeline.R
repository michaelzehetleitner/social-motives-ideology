# AP6 table helpers: the row filters, labels, keys and gate rows the
# regression report tables share.

#' The primary-prior rows of a per-fit table
#'
#' @param coefs Coefficient rows of the register's fits
#'   ([tabulate_regression_coefficients()]).
#' @return The rows whose `role` is the primary one.
zm_primary_coefs <- function(coefs) {
  dplyr::filter(coefs, .data$role == "primary")
}

#' Label the plan gives a fit outside the prior-width sweep
#'
#' The sweep fits are labelled by `analysis_plan$priors$sweep_labels`; the Student-t
#' refit and the joint model are labelled by `analysis_plan$regression$fit_roles`.
#'
#' @param analysis_plan Analysis configuration.
#' @param role Role key of the fit.
#' @return Character scalar.
zm_fit_label <- function(analysis_plan, role) {
  as.character(analysis_plan$regression$fit_roles[[role]])
}

#' Add identifying columns to a per-fit table when the table lacks them
#'
#' @param tbl Tibble returned by a per-fit function.
#' @param ... Named scalars (e.g. `outcome = "asc_agg"`) to add as columns
#'   unless a column of that name already exists.
#' @return The tibble with the tag columns placed first.
zm_tag <- function(tbl, ...) {
  tags <- list(...)
  tbl <- tibble::as_tibble(tbl)
  for (nm in names(tags)) {
    if (!nm %in% names(tbl)) tbl[[nm]] <- tags[[nm]]
  }
  dplyr::relocate(tbl, dplyr::any_of(names(tags)))
}

# ---- AP6: shared helpers of the regression tables ---------------------------

#' A column of a result table, or typed missing values of its height
#'
#' An unavailable-fit row carries its provenance and status but no coefficient
#' column, so a table that binds available and unavailable fits needs the
#' missing column in its declared type rather than a dropped column.
#'
#' @param tbl A result table.
#' @param name Column name.
#' @param empty The typed missing value.
#' @return The column, or `empty` repeated.
zm_regression_column <- function(tbl, name, empty) {
  if (name %in% names(tbl)) return(tbl[[name]])
  rep(empty, nrow(tbl))
}

#' The two configured interval column names of the coefficient tables
#'
#' @param analysis_plan Analysis configuration.
#' @return Character vector of length two, e.g. `c("q2.5", "q97.5")`.
zm_regression_ci_names <- function(analysis_plan) {
  paste0("q", zm_sweep_key(ap6_ci_probs(analysis_plan$regression$ci_level) * 100))
}

#' Identity key of one fit of the register
#'
#' @param outcome Outcome key.
#' @param slope_sd Slope prior SD.
#' @param role Model role.
#' @param family Residual family.
#' @return Character vector.
zm_regression_fit_key <- function(outcome, slope_sd, role, family) {
  paste(outcome, zm_sweep_key(as.numeric(slope_sd)), role, family, sep = "|")
}

#' The diagnostics columns of a fit whose gate result is unavailable
#'
#' The observed values stay missing; only the configured thresholds are filled,
#' because they are settings rather than measurements.
#'
#' @param analysis_plan Analysis configuration.
#' @return One-row tibble with the [ap6_fit_diagnostics()] columns.
zm_regression_unavailable_diagnostics <- function(analysis_plan) {
  gate <- ap6_gate_settings(analysis_plan$regression)
  tibble::tibble(
    ok = NA, ess_ok = NA, rhat_ok = NA, divergences_ok = NA, treedepth_ok = NA, bfmi_ok = NA,
    ess_bulk_min = NA_real_, ess_tail_min = NA_real_, rhat_max = NA_real_,
    n_divergent = NA_integer_, n_treedepth_hits = NA_integer_, bfmi_min = NA_real_,
    mcse_median_max = NA_real_, mcse_q_max = NA_real_,
    ess_target = gate$ess_target, rhat_limit = gate$rhat_max,
    divergences_max = gate$divergences_max, treedepth_hits_max = gate$treedepth_hits_max,
    bfmi_limit = gate$bfmi_min, max_treedepth_used = NA_integer_,
    gate_note = NA_character_
  )
}

#' The fit register as one gate row per specified model or variant
#'
#' The shape the appendix sampling-diagnostics table reads: the fit identity,
#' the fitted N and iterations captured at the gate, the final gate status and
#' the final diagnostics.
#'
#' @param register Records from `extract_regression_result_records()`.
#' @param analysis_plan Analysis configuration.
#' @return Tibble with one row per record, in register order.
zm_regression_register_table <- function(register, analysis_plan) {
  dplyr::bind_rows(lapply(register, function(record) {
    diagnostics <- record$diagnostics
    if (is.null(diagnostics)) diagnostics <- zm_regression_unavailable_diagnostics(analysis_plan)
    dplyr::bind_cols(
      tibble::tibble(
        outcome = record$outcome_key,
        slope_sd = as.numeric(record$slope_sd),
        role = record$role,
        label = record$label,
        family = record$family,
        n_obs = if (is.null(record$n_obs)) NA_integer_ else as.integer(record$n_obs),
        iter_used = if (is.null(record$iter_used)) NA_integer_ else as.integer(record$iter_used),
        gate_status = as.character(record$gate_status)
      ),
      tibble::as_tibble(diagnostics)
    )
  }))
}

#' The model-based R2 definition of one fit's family
#'
#' The wording `ap6_r2_draws()` attaches to its draws, rebuilt from the family
#' and the fixed degrees of freedom every R2 summary row carries.
#'
#' @param family `"gaussian"` or `"student"`.
#' @param nu_fixed Fixed degrees of freedom (`NA` for gaussian fits).
#' @return Character vector.
zm_regression_r2_definition <- function(family, nu_fixed) {
  vapply(seq_along(family), function(i) {
    if (identical(family[[i]], "gaussian")) return("var(mu) / (var(mu) + sigma^2)")
    nu <- as.numeric(nu_fixed[[i]])
    sprintf("var(mu) / (var(mu) + %s * sigma^2) [student, nu = %s fixed]",
            format(nu / (nu - 2)), format(nu))
  }, character(1))
}

#' The fit role of one prior-predictive specification (family and prior width)
#'
#' @param family `"gaussian"` or `"student"`.
#' @param slope_sd Slope prior SD.
#' @param analysis_plan Analysis configuration.
#' @return Character scalar.
zm_regression_prior_predictive_role <- function(family, slope_sd, analysis_plan) {
  if (identical(family, "student")) return("student_refit")
  if (dplyr::near(as.numeric(slope_sd), as.numeric(analysis_plan$priors$slope_sd_primary))) "primary" else "sweep"
}

#' Short names of the posterior-predictive statistics
#'
#' The posterior-predictive checks name excess kurtosis and skewness in full;
#' the S4 tables print the abbreviations.
#'
#' @return Named character vector: check name -> short name.
zm_regression_pp_statistic_names <- function() {
  c(mean = "mean", sd = "sd", min = "min", max = "max",
    excess_kurtosis = "kurtosis", skewness = "skew")
}
