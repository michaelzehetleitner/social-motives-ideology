# Report target of the supplement section S4 "Model checks".
#
# Builds the one target (_targets.R, SUPPLEMENT · S4) the section and its
# tables read. The report words and formats what this returns.

#' S4 model checks
#'
#' The prior- and posterior-predictive checks, the heavy-tail flags, the
#' Student-t refits and the likelihood comparison; the counts and the
#' statistics outside their replicated band that the section states; and the
#' facts of the answer the RQ1 section prints.
#'
#' @param regression_predictive_checks,regression_fit_register,regression_coefficient_summaries
#'   The accepted regression results (targets of the same names).
#' @param coefs_primary The primary coefficients (target `report_primary_coefficients`).
#' @param analysis_plan Configuration from [zm_config()].
#' @return List `prior_predictive`, `posterior_predictive`, `predictive_draws`,
#'   `tails`, `student_coefs`, `likelihood_comparison`, `section` (counts and
#'   rows the section states) and `answer`.
assemble_supplement_model_checks <- function(regression_predictive_checks, regression_fit_register,
                                             regression_coefficient_summaries, coefs_primary,
                                             regression_likelihood_robustness, analysis_plan) {
  prior_predictive <- tabulate_prior_predictive_checks(regression_predictive_checks, analysis_plan)
  posterior_predictive <- tabulate_posterior_predictive_stats(regression_predictive_checks, analysis_plan)
  tails <- tabulate_heavy_tail_checks(regression_fit_register)
  student_coefs <- tabulate_regression_coefficients(
    regression_coefficient_summaries, regression_fit_register, analysis_plan,
    roles = "student_refit", emit_role = "student")
  ppc <- tibble::as_tibble(posterior_predictive)
  # Which posterior-predictive statistics fall outside their replicated band,
  # in either direction.
  outside <- ppc[!is.na(ppc$observed) & !is.na(ppc$rep_lo) & !is.na(ppc$rep_hi) &
                   (ppc$observed < ppc$rep_lo | ppc$observed > ppc$rep_hi), , drop = FALSE]
  list(
    prior_predictive = prior_predictive,
    posterior_predictive = posterior_predictive,
    predictive_draws = collect_posterior_predictive_draws(regression_predictive_checks, analysis_plan),
    tails = tails,
    student_coefs = student_coefs,
    likelihood_comparison = tabulate_likelihood_comparison(regression_likelihood_robustness, coefs_primary,
                                                           student_coefs, analysis_plan),
    section = list(
      stats = unique(as.character(ppc$stat)),
      n_heavy = sum(tails$heavy_tails %in% TRUE),
      # The refits that exist, never the rows of the refit table: an
      # untriggered outcome keeps a placeholder row there.
      n_student_outcomes = length(rh_fitted_outcomes(student_coefs)),
      outside = tibble::tibble(
        stat = as.character(outside$stat),
        outcome = as.character(outside$outcome),
        above = outside$observed > outside$rep_hi
      )
    ),
    answer = check_model_check_outcomes(prior_predictive, posterior_predictive, tails, student_coefs)
  )
}

#' The Gaussian fit against the Student-t refit, coefficient by coefficient
#'
#' The accepted route pairs the two fits' summaries per outcome and term
#' (target `regression_likelihood_robustness`). This keeps the slope and
#' covariate rows the report tables show, and applies the AP10 reading to each
#' family: a coefficient is credible when its interval excludes zero and its fit
#' passed the validity gate; the comparison holds only when both fits passed,
#' and then records whether the interval decision or the direction changes.
#'
#' @param regression_likelihood_robustness List `coefficients` (and `r2`).
#' @param coefs_primary The primary coefficient table (for the rows and their order).
#' @param student_coefs The Student-t coefficient table (for the fixed nu).
#' @param analysis_plan Configuration from [zm_config()].
#' @return Tibble `outcome`, `term` and per family (`_gaussian`, `_student`)
#'   `estimate`, `lo`, `hi`, `fitted`, `diagnostics_ok`, `credible`,
#'   `observed_sign`; then `comparison_valid`, `interval_exclusion_changes`,
#'   `direction_changes`; attributes `ci_level` and `nu_fixed`.
tabulate_likelihood_comparison <- function(regression_likelihood_robustness, coefs_primary, student_coefs,
                                           analysis_plan) {
  pairs <- tibble::as_tibble(regression_likelihood_robustness$coefficients)
  pairs$outcome_key <- dplyr::coalesce(pairs$outcome_key_gaussian, pairs$outcome_key_student)
  shown <- tibble::as_tibble(coefs_primary)
  shown <- shown[shown$term_type %in% c("predictor", "covariate"), c("outcome", "term"), drop = FALSE]
  rows <- pairs[match(paste(shown$outcome, shown$term), paste(pairs$outcome_key, pairs$term)), , drop = FALSE]
  family <- function(suffix) {
    col <- function(name) rows[[paste0(name, "_", suffix)]]
    estimate <- as.numeric(col("estimate"))
    lo <- as.numeric(col("q_lo"))
    hi <- as.numeric(col("q_hi"))
    ok <- col("gate_status") %in% ap7_gate_statuses_ok() & col("fit_valid") %in% TRUE &
      is.finite(estimate) & is.finite(lo) & is.finite(hi)
    tibble::tibble(
      estimate = estimate, lo = lo, hi = hi,
      fitted = col("fit_available") %in% TRUE,
      diagnostics_ok = ok,
      credible = dplyr::if_else(ok, ap7_credible(lo, hi), NA),
      observed_sign = dplyr::if_else(ok, ap7_sign(estimate), NA_character_)
    )
  }
  gaussian <- family("gaussian")
  student <- family("student")
  names(gaussian) <- paste0(names(gaussian), "_gaussian")
  names(student) <- paste0(names(student), "_student")
  out <- dplyr::bind_cols(shown, gaussian, student) |>
    dplyr::mutate(
      comparison_valid = .data$diagnostics_ok_gaussian & .data$diagnostics_ok_student,
      interval_exclusion_changes = dplyr::if_else(
        .data$comparison_valid, .data$credible_gaussian != .data$credible_student, NA
      ),
      direction_changes = dplyr::if_else(
        .data$comparison_valid,
        (.data$observed_sign_gaussian == "+" & .data$observed_sign_student == "-") |
          (.data$observed_sign_gaussian == "-" & .data$observed_sign_student == "+"),
        NA
      )
    )
  attr(out, "ci_level") <- analysis_plan$regression$ci_level
  attr(out, "nu_fixed") <- if (is.data.frame(student_coefs) && "nu_fixed" %in% names(student_coefs)) {
    unique(stats::na.omit(as.numeric(student_coefs$nu_fixed)))
  } else numeric(0)
  out
}

#' What the three model checks found, for the S4 answer
#'
#' @param prior_predictive,posterior_predictive,tails,student_coefs The S4 tables.
#' @return List: `prior_any` (some prior-predictive coverage is available),
#'   `prior_all_span` (every row available and extending beyond the scale at
#'   both ends), `prior_unavailable_outcomes`; `pp_any`, `n_pp_unavailable`,
#'   `n_pp_outside` (among the available statistics); `tail_any`,
#'   `tail_unavailable_outcomes`, `triggered` and `refitted` (outcomes).
check_model_check_outcomes <- function(prior_predictive, posterior_predictive, tails, student_coefs) {
  prior_available <- is.finite(prior_predictive$share_below_min) &
    is.finite(prior_predictive$share_above_max)
  prior_spans_scale <- prior_available & prior_predictive$share_below_min > 0 &
    prior_predictive$share_above_max > 0
  pp_available <- is.finite(posterior_predictive$observed) & is.finite(posterior_predictive$rep_lo) &
    is.finite(posterior_predictive$rep_hi)
  outside <- pp_available &
    (posterior_predictive$observed < posterior_predictive$rep_lo |
       posterior_predictive$observed > posterior_predictive$rep_hi)
  tail_available <- !is.na(tails$heavy_tails)
  list(
    prior_any = nrow(prior_predictive) > 0L && any(prior_available),
    prior_all_span = all(prior_available) && all(prior_spans_scale),
    prior_unavailable_outcomes = unique(as.character(prior_predictive$outcome[!prior_available])),
    pp_any = nrow(posterior_predictive) > 0L && any(pp_available),
    n_pp_unavailable = sum(!pp_available),
    n_pp_outside = sum(outside),
    tail_any = nrow(tails) > 0L && any(tail_available),
    tail_unavailable_outcomes = unique(as.character(tails$outcome[!tail_available])),
    triggered = unique(as.character(tails$outcome[tail_available & tails$heavy_tails %in% TRUE])),
    # An untriggered refit leaves a placeholder row, so which refits exist is
    # read from the recorded fit availability, not from the rows.
    refitted = rh_fitted_outcomes(student_coefs)
  )
}

# ---- the tables of the section, built from the accepted results ----------

#' Prior-predictive summary rows
#'
#' @param predictive_checks `regression_predictive_checks`.
#' @param analysis_plan Analysis configuration.
#' @return Tibble in the `ap6_prior_predictive()` summary shape.
tabulate_prior_predictive_checks <- function(predictive_checks, analysis_plan) {
  identity <- ap6_regression_outcome_identity(analysis_plan)
  leaves <- unlist(predictive_checks$prior, recursive = FALSE)
  dplyr::bind_rows(lapply(leaves, function(leaf) {
    key <- identity$outcome_key[match(leaf$outcome, identity$outcome)]
    if (is.na(key)) stop("A prior-predictive leaf names an undeclared outcome: ", leaf$outcome, ".")
    standardised <- leaf$standardised_summary
    raw <- leaf$raw_scale_summary
    # Every configured percentile of the raw-scale answers, named by its
    # probability (q5, q50, q95): Methods promises all three.
    quantiles <- unlist(raw$raw_quantiles)
    quantile_columns <- stats::setNames(
      as.list(unname(as.numeric(quantiles))),
      paste0("q", sub("%$", "", names(quantiles)))
    )
    row <- tibble::tibble(
      outcome = key,
      status = if (is.null(leaf$status)) "available" else as.character(leaf$status),
      note = if (is.null(leaf$note)) NA_character_ else as.character(leaf$note),
      family = as.character(leaf$family),
      slope_sd = as.numeric(leaf$slope_sd),
      role = zm_regression_prior_predictive_role(leaf$family, leaf$slope_sd, analysis_plan),
      label = ap6_sweep_label(leaf$slope_sd, analysis_plan),
      n_draws = as.integer(standardised$n_draws),
      n_obs = as.integer(standardised$n_observations),
      mean_z = as.numeric(standardised$mean),
      sd_z = as.numeric(standardised$sd),
      p95_abs_prediction = as.numeric(standardised$p95_absolute),
      mean = as.numeric(raw$raw_mean),
      sd = as.numeric(raw$raw_sd),
      share_below_min = as.numeric(raw$share_below_min),
      share_above_max = as.numeric(raw$share_above_max),
      share_outside_range = as.numeric(raw$share_outside_range),
      range_min = unname(as.numeric(raw$response_range[["min"]])),
      range_max = unname(as.numeric(raw$response_range[["max"]])),
      z_mean = as.numeric(leaf$fixed_z_constants$mean),
      z_sd = as.numeric(leaf$fixed_z_constants$sd)
    )
    do.call(tibble::add_column, c(list(row), quantile_columns, list(.after = "sd")))
  }))
}

#' Posterior-predictive statistics rows
#'
#' @param predictive_checks `regression_predictive_checks`.
#' @param analysis_plan Analysis configuration.
#' @return Tibble `outcome`, `slope_sd`, `stat`, `observed`, `rep_median`,
#'   `rep_lo`, `rep_hi`, `p_value`.
tabulate_posterior_predictive_stats <- function(predictive_checks, analysis_plan) {
  identity <- ap6_regression_outcome_identity(analysis_plan)
  names_map <- zm_regression_pp_statistic_names()
  dplyr::bind_rows(lapply(names(predictive_checks$posterior), function(outcome) {
    checks <- predictive_checks$posterior[[outcome]]
    key <- identity$outcome_key[match(outcome, identity$outcome)]
    if (is.na(key)) stop("A posterior-predictive leaf names an undeclared outcome: ", outcome, ".")
    dplyr::bind_rows(lapply(names(names_map), function(statistic) {
      check <- checks[[statistic]]
      if (is.null(check)) {
        stop("The posterior-predictive checks of '", outcome, "' have no ", statistic, " statistic.")
      }
      tibble::tibble(
        outcome = key,
        status = if (is.null(attr(checks, "status", exact = TRUE))) "available" else attr(checks, "status", exact = TRUE),
        note = if (is.null(attr(checks, "note", exact = TRUE))) NA_character_ else attr(checks, "note", exact = TRUE),
        slope_sd = as.numeric(attr(checks, "slope_sd", exact = TRUE)),
        stat = unname(names_map[[statistic]]),
        observed = as.numeric(check$observed),
        rep_median = as.numeric(check$replicated_median),
        rep_lo = unname(as.numeric(check$replicated_interval[[1]])),
        rep_hi = unname(as.numeric(check$replicated_interval[[2]])),
        p_value = as.numeric(check$posterior_predictive_p_value)
      )
    }))
  }))
}

#' Posterior-predictive draw table
#'
#' The retained producer output of `ap6_pp_check_data()`, kept at the
#' posterior-check boundary; nothing is predicted again here.
#'
#' @param predictive_checks `regression_predictive_checks`.
#' @param analysis_plan Analysis configuration.
#' @return Tibble `outcome`, `type`, `draw`, `obs`, `value`.
collect_posterior_predictive_draws <- function(predictive_checks, analysis_plan) {
  identity <- ap6_regression_outcome_identity(analysis_plan)
  result <- dplyr::bind_rows(lapply(names(predictive_checks$posterior), function(outcome) {
    draws <- attr(predictive_checks$posterior[[outcome]], "predictive_draws", exact = TRUE)
    if (is.null(draws)) {
      stop("The retained posterior-predictive draws of '", outcome, "' are absent; the ",
           "predictive-draw producer must keep them at the check boundary.")
    }
    draws <- tibble::as_tibble(draws)
    draws$outcome <- rep(identity$outcome_key[match(outcome, identity$outcome)], nrow(draws))
    draws
  }))
  unavailable <- Filter(function(checks) identical(attr(checks, "status", exact = TRUE), "unavailable"),
                        predictive_checks$posterior)
  attr(result, "unavailable_predictions") <- tibble::tibble(
    outcome = identity$outcome_key[match(names(unavailable), identity$outcome)],
    note = vapply(unavailable, function(checks) attr(checks, "note", exact = TRUE), character(1)))
  result
}

#' Heavy-tail trigger table
#'
#' The trigger the Student refit producer recorded per outcome; the projection
#' never evaluates it again.
#'
#' @param register Records from `extract_regression_result_records()`.
#' @return Tibble `outcome`, `heavy_tails`.
tabulate_heavy_tail_checks <- function(register) {
  student <- Filter(function(record) identical(record$role, "student_refit"), register)
  if (length(student) == 0L) stop("tabulate_heavy_tail_checks(): the register holds no Student record.")
  dplyr::bind_rows(lapply(student, function(record) {
    if (is.na(record$heavy_tail_trigger) && record$fit_available %in% TRUE) {
      stop("The Student refit producer recorded no heavy-tail trigger for '", record$outcome, "'.")
    }
    tibble::tibble(outcome = record$outcome_key, heavy_tails = as.logical(record$heavy_tail_trigger),
                   note = as.character(record$note))
  }))
}

#' Save model-check counts and ranges before rendering their sentences
add_model_check_reporting_facts <- function(report, r2_summaries) {
  prior <- tibble::as_tibble(report$prior_predictive)
  primary <- prior[prior$family %in% "gaussian" & prior$role %in% "primary", , drop = FALSE]
  gaussian <- prior[prior$family %in% "gaussian", , drop = FALSE]
  report$prior_range <- list(
    primary_min = if (nrow(primary)) min(primary$share_outside_range) else NA_real_,
    primary_max = if (nrow(primary)) max(primary$share_outside_range) else NA_real_,
    gaussian_max = if (nrow(gaussian)) max(gaussian$share_outside_range) else NA_real_,
    response_min = primary$range_min[1], response_max = primary$range_max[1])
  comparison <- report$likelihood_comparison
  attempted <- comparison$fitted_student %in% TRUE
  valid <- comparison$comparison_valid %in% TRUE
  changed <- valid & comparison$interval_exclusion_changes %in% TRUE
  sign_changed <- valid & comparison$direction_changes %in% TRUE
  attr(report$likelihood_comparison, "report_summary") <- list(
    n = nrow(comparison), any_attempted = any(attempted), n_attempted = sum(attempted),
    n_valid = sum(valid), n_changed = sum(changed), n_sign_changed = sum(sign_changed),
    changed_outcomes = unique(comparison$outcome[changed]))
  report$student_r2 <- collect_saved_student_r2(r2_summaries)
  report$predictive_density <- calculate_predictive_density_data(report$predictive_draws)
  report
}
