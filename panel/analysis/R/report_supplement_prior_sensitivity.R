# Report target of the supplement section S3 "Prior sensitivity".
#
# Builds the one target (_targets.R, SUPPLEMENT · S3) the section and its
# tables read. The report words and formats what this returns.

#' S3 prior sensitivity
#'
#' The prior-width sweep and the local power-scaling diagnostics, the counts the
#' section states about both, and the facts of the answer the RQ1 section
#' prints: which models or prior blocks power-scaling failed for, and how many
#' coefficients it flagged.
#'
#' @param regression_coefficient_summaries,regression_fit_register,regression_prior_sensitivity,regression_prior_width_sensitivity
#'   The accepted regression results (targets of the same names).
#' @param codebook Codebook from [zm_codebook()].
#' @param data_regressions The regression input (target of the same name), for
#'   the gender coding the Methods list states.
#' @param analysis_plan Configuration from [zm_config()].
#' @return List `model_card` (`analysis_data`: the gender column as fitted),
#'   `coefficients_primary` (for the prior-posterior figure),
#'   `sensitivity`, `priorsense`, `sweep` (counts of the prior-width sweep),
#'   `powerscale` (counts of the power-scaling diagnostics) and `answer`.
assemble_supplement_prior_sensitivity <- function(regression_coefficient_summaries, regression_fit_register,
                                                  regression_prior_sensitivity, regression_prior_width_sensitivity,
                                                  data_regressions, codebook, analysis_plan) {
  coefs <- tabulate_regression_coefficients(regression_coefficient_summaries, regression_fit_register,
                                             analysis_plan, roles = c("primary", "sweep"))
  priorsense <- tabulate_power_scaling(regression_prior_sensitivity, analysis_plan)
  sensitivity <- tabulate_prior_width_sweep(regression_prior_width_sensitivity, regression_fit_register,
                                            priorsense, codebook, analysis_plan)
  sens <- tibble::as_tibble(sensitivity)
  sign_change_rows <- sens[sens$comparison_valid %in% TRUE & sens$direction_changes %in% TRUE, , drop = FALSE]
  ps <- tibble::as_tibble(priorsense)
  ps_diagnosis <- as.character(ps$diagnosis)
  interpretable <- ps$interpretable %in% TRUE
  diagnosed <- interpretable & !is.na(ps_diagnosis) & !ps_diagnosis %in% c("-", "", "none") &
    !grepl("^error", ps_diagnosis)
  finite_max <- function(values) {
    finite <- values[is.finite(values)]
    if (length(finite)) max(finite) else NA_real_
  }
  list(
    model_card = list(
      analysis_data = tibble::tibble(gender = as.factor(data_regressions$gender))
    ),
    coefficients_primary = zm_primary_coefs(coefs),
    sensitivity = sensitivity,
    priorsense = priorsense,
    # The entries power scaling flags with a diagnosis; all entries are in the
    # data file analysis_plan$data_files$result_files$prior_power_scaling (AP6).
    priorsense_flagged = ps[diagnosed | !interpretable |
      (!is.na(ps_diagnosis) & grepl("^error", ps_diagnosis)), , drop = FALSE],
    sweep = list(
      n_cells = nrow(sens),
      n_widths = max(sens$n_sds, na.rm = TRUE),
      interpretable = check_prior_width_fits(regression_fit_register, analysis_plan),
      n_interval_changes = sum(sens$interval_exclusion_changes %in% TRUE),
      n_sign_changes = nrow(sign_change_rows),
      n_sign_changes_credible = sum(sign_change_rows$n_credible > 0, na.rm = TRUE)
    ),
    powerscale = list(
      n_flagged = sum(diagnosed),
      n_interpretable = sum(interpretable),
      blocks = setdiff(unique(as.character(ps$block)), "all_priors"),
      prior_max = finite_max(ps$prior_sens[interpretable]),
      lik_max = finite_max(ps$lik_sens[interpretable])
    ),
    answer = check_powerscale_outcome(priorsense)
  )
}

#' The prior-width sweep, one row per outcome and motive
#'
#' The accepted route summarises each coefficient at every registered slope
#' prior width and records whether its interval decision and the direction of
#' its median are the same at every width (target
#' `regression_prior_width_sensitivity`). This lays the motive coefficients out
#' as the S3 table shows them: the median and interval per width, the widths
#' whose interval excludes zero, and the stability class. A width whose fit did
#' not pass the validity gate shows no numbers, and a coefficient is compared
#' across the widths only when every planned width's fit passed; `ci_changes`
#' takes precedence over `direction_changes`, because the classification AP6
#' speaks about is the interval decision.
#'
#' @param regression_prior_width_sensitivity List outcome -> coefficient of
#'   [extract_regression_prior_width_sensitivity()].
#' @param regression_fit_register The fit register (the gate of each width's fit).
#' @param priorsense The power-scaling table of [tabulate_power_scaling()].
#' @param codebook Codebook from [zm_codebook()].
#' @param analysis_plan Configuration from [zm_config()].
#' @return Tibble `outcome`, `term`, `term_type`, `est_<sd>`, `lo_<sd>`,
#'   `hi_<sd>`, `credible_under`, `n_sds`, `n_credible`, `stability`,
#'   `comparison_valid`, `interval_exclusion_changes`, `direction_changes`,
#'   `robust_to_prior`, the four width lists, `prior_sens`, `lik_sens`,
#'   `diagnosis`.
tabulate_prior_width_sweep <- function(regression_prior_width_sensitivity, regression_fit_register, priorsense,
                                       codebook, analysis_plan) {
  motives <- as.character(analysis_plan$regression$motives)
  motive_of <- stats::setNames(motives, paste0("b_", zm_z_col(motives, codebook)))
  width_label <- function(x) format(as.numeric(x), trim = TRUE, drop0trailing = TRUE, scientific = FALSE)
  width_fits <- Filter(function(record) record$role %in% c("primary", "sweep"), regression_fit_register)
  width_ok <- stats::setNames(
    vapply(width_fits, function(record) isTRUE(record$fit_valid) &&
             record$gate_status %in% ap7_gate_statuses_ok(), logical(1)),
    vapply(width_fits, function(record) paste(record$outcome_key, width_label(record$slope_sd)), character(1))
  )
  planned <- width_label(analysis_plan$priors$slope_sd_sweep)
  cells <- Filter(function(cell) cell$coefficient %in% names(motive_of),
                  unlist(regression_prior_width_sensitivity, recursive = FALSE))
  rows <- lapply(cells, function(cell) {
    outcome <- width_fits[[which(vapply(width_fits, function(r) identical(r$outcome, cell$outcome), logical(1)))[1]]]$outcome_key
    widths <- width_label(names(cell$by_width))
    ok <- unname(width_ok[paste(outcome, widths)]) %in% TRUE
    median <- vapply(cell$by_width, function(w) as.numeric(w$median), numeric(1))
    lo <- vapply(cell$by_width, function(w) as.numeric(w$interval[1]), numeric(1))
    hi <- vapply(cell$by_width, function(w) as.numeric(w$interval[2]), numeric(1))
    ok <- ok & is.finite(median) & is.finite(lo) & is.finite(hi)
    median[!ok] <- NA_real_
    lo[!ok] <- NA_real_
    hi[!ok] <- NA_real_
    excludes <- vapply(cell$by_width, function(w) isTRUE(w$interval_excludes_zero), logical(1)) & ok
    includes <- !excludes & ok
    widths_num <- as.numeric(names(cell$by_width))
    valid <- all(ok) && setequal(widths, planned)
    interval_changes <- if (valid) !isTRUE(cell$interval_exclusion_stable) else NA
    direction_changes <- if (valid) !isTRUE(cell$median_direction_stable) else NA
    row <- tibble::tibble(outcome = outcome, term = unname(motive_of[cell$coefficient]), term_type = "predictor")
    for (i in seq_along(widths)) row[[paste0("est_", widths[i])]] <- unname(median[i])
    for (i in seq_along(widths)) row[[paste0("lo_", widths[i])]] <- unname(lo[i])
    for (i in seq_along(widths)) row[[paste0("hi_", widths[i])]] <- unname(hi[i])
    dplyr::bind_cols(row, tibble::tibble(
      credible_under = if (valid) paste(widths[excludes], collapse = "; ") else NA_character_,
      n_sds = length(widths),
      n_credible = if (valid) sum(excludes) else NA_integer_,
      stability = if (!valid) "not_interpretable" else if (interval_changes) "ci_changes" else
        if (direction_changes) "direction_changes" else "stable",
      comparison_valid = valid,
      note = paste(unique(vapply(cell$by_width[!ok], function(w) {
        if (is.null(w$note)) "Prior-width fit unavailable or invalid." else as.character(w$note)
      }, character(1))), collapse = "; "),
      interval_exclusion_changes = interval_changes,
      direction_changes = direction_changes,
      robust_to_prior = !interval_changes,
      interval_excludes_zero_sds = list(if (valid) widths_num[excludes] else NULL),
      interval_includes_zero_sds = list(if (valid) widths_num[includes] else NULL),
      positive_median_sds = list(if (valid) widths_num[median > 0] else NULL),
      negative_median_sds = list(if (valid) widths_num[median < 0] else NULL)
    ))
  })
  out <- dplyr::bind_rows(rows)
  ps <- tibble::as_tibble(priorsense)
  ps <- ps[!is.na(ps$block) & ps$block == "metric_slopes", intersect(c("outcome", "term", "prior_sens", "lik_sens", "diagnosis"), names(ps))]
  ps <- ps[!duplicated(ps[, c("outcome", "term")]), , drop = FALSE]
  out <- dplyr::left_join(out, ps, by = c("outcome", "term"))
  for (col in c("prior_sens", "lik_sens", "diagnosis")) {
    if (!col %in% names(out)) out[[col]] <- if (col == "diagnosis") NA_character_ else NA_real_
  }
  out
}

#' Whether every planned prior width of every outcome has a fit that passed
#'
#' @param regression_fit_register The fit register.
#' @param analysis_plan Configuration from [zm_config()].
#' @return `TRUE` when the Gaussian fits of the primary width and the sweep
#'   cover every outcome at every planned width and all passed the validity gate.
check_prior_width_fits <- function(regression_fit_register, analysis_plan) {
  fits <- Filter(function(record) record$role %in% c("primary", "sweep") &&
                   record$family %in% analysis_plan$regression$family, regression_fit_register)
  if (length(fits) == 0L) return(FALSE)
  found <- vapply(fits, function(record) paste(record$outcome_key, record$slope_sd), character(1))
  expected <- tidyr::expand_grid(outcome = analysis_plan$regression$outcomes,
                                 slope_sd = analysis_plan$priors$slope_sd_sweep)
  ok <- vapply(fits, function(record) isTRUE(record$fit_valid) && record$gate_status %in% ap7_gate_statuses_ok(),
               logical(1))
  setequal(found, paste(expected$outcome, expected$slope_sd)) && all(ok)
}

#' Which power-scaling runs failed, and how many coefficients the rest flagged
#'
#' A model counts as completely failed when every planned prior block errored
#' for it; the other errored runs are listed by model and block.
#'
#' @param priorsense The power-scaling table of [tabulate_power_scaling()].
#' @return List `failed_models` (outcomes), `failed_blocks` (tibble `outcome`,
#'   `block`), `any_successful` and `n_flagged` (distinct outcome-term pairs
#'   flagged for potential prior-data conflict).
check_powerscale_outcome <- function(priorsense) {
  diagnoses <- as.character(priorsense$diagnosis)
  raw_diagnoses <- if ("diagnosis_raw" %in% names(priorsense)) as.character(priorsense$diagnosis_raw) else diagnoses
  status <- priorsense$importance_sampling_status
  if (is.null(status)) status <- rep("unavailable", nrow(priorsense))
  fit_valid <- priorsense$fit_valid
  if (is.null(fit_valid)) fit_valid <- rep(NA, nrow(priorsense))
  fit_gate_status <- priorsense$fit_gate_status
  if (is.null(fit_gate_status)) fit_gate_status <- rep("unavailable", nrow(priorsense))
  reliable <- can_interpret_power_scaling(fit_valid, fit_gate_status, status)
  errored <- priorsense[
    !is.na(raw_diagnoses) & grepl("^error", raw_diagnoses), c("outcome", "block"), drop = FALSE
  ]
  errored <- unique(errored)
  planned_blocks <- unique(as.character(priorsense$block))
  failed_models <- unique(as.character(errored$outcome))
  completely_failed <- failed_models[vapply(failed_models, function(outcome) {
    all(planned_blocks %in% as.character(errored$block[errored$outcome %in% outcome]))
  }, logical(1))]
  flagged <- priorsense[
    reliable & !is.na(diagnoses) & grepl("potential prior-data conflict", diagnoses, fixed = TRUE),
    c("outcome", "term"), drop = FALSE
  ]
  list(
    failed_fit_models = unique(as.character(priorsense$outcome[!is.na(fit_valid) & !is.na(fit_gate_status) &
      !fit_gate_status %in% "unavailable" &
      (!fit_valid %in% TRUE | !fit_gate_status %in% c("ok", "retried_ok"))])),
    unavailable_fit_models = unique(as.character(priorsense$outcome[is.na(fit_valid) | is.na(fit_gate_status) |
      fit_gate_status %in% "unavailable"])),
    failed_models = completely_failed,
    failed_blocks = errored[!errored$outcome %in% completely_failed, , drop = FALSE],
    any_successful = any(reliable & !is.na(diagnoses) & !grepl("^error", diagnoses)),
    unreliable_blocks = unique(priorsense[status %in% "unreliable", c("outcome", "block"), drop = FALSE]),
    unavailable_blocks = unique(priorsense[!status %in% c("reliable", "unreliable"), c("outcome", "block"), drop = FALSE]),
    n_flagged = nrow(unique(flagged))
  )
}

# ---- the tables of the section, built from the accepted results ----------

#' Local prior-sensitivity table
#'
#' Projects the per term and block power-scaling matrices the accepted result
#' retains; no sensitivity is calculated here.
#'
#' @param prior_sensitivity `regression_prior_sensitivity`.
#' @param analysis_plan Analysis configuration.
#' @return Tibble in the `ap6_priorsense()` shape.
tabulate_power_scaling <- function(prior_sensitivity, analysis_plan) {
  identity <- ap6_regression_outcome_identity(analysis_plan)
  dplyr::bind_rows(lapply(names(prior_sensitivity), function(outcome) {
    summary <- prior_sensitivity[[outcome]]
    sensitivity <- summary$sensitivity
    if (is.null(sensitivity)) {
      stop("The retained power-scaling matrices of '", outcome, "' are absent; the ",
           "prior-sensitivity producer must keep them with its diagnostics.")
    }
    key <- identity$outcome_key[match(outcome, identity$outcome)]
    settings <- sensitivity$settings
    fit_valid <- if (is.null(summary$fit_valid)) NA else summary$fit_valid
    fit_gate_status <- if (is.null(summary$fit_gate_status)) "unavailable" else summary$fit_gate_status
    dplyr::bind_rows(lapply(sensitivity$blocks, function(one_block) {
      matrix <- tibble::as_tibble(one_block$matrix)
      importance_status <- classify_importance_sampling(one_block$pareto_k_max, one_block$importance_sampling_draws)
      terms <- ap6_clean_terms(matrix$variable, analysis_plan$regression)
      terms$term[matrix$variable == "bayes_R2"] <- "bayes_R2"
      terms$term_type[matrix$variable == "bayes_R2"] <- "derived"
      tibble::tibble(
        outcome = key,
        slope_sd = as.numeric(analysis_plan$priors$slope_sd_primary),
        block = as.character(one_block$block),
        term = terms$term,
        term_type = terms$term_type,
        prior_selection = as.character(one_block$prior_selection),
        components = paste(as.character(settings$components), collapse = ";"),
        lower_alpha = as.numeric(settings$lower_alpha),
        upper_alpha = as.numeric(settings$upper_alpha),
        div_measure = as.character(settings$div_measure),
        sensitivity_threshold = as.numeric(settings$sensitivity_threshold),
        prior_sens = as.numeric(matrix$prior),
        lik_sens = as.numeric(matrix$likelihood),
        diagnosis = describe_power_scaling_diagnosis(matrix$diagnosis, fit_valid, fit_gate_status, importance_status),
        diagnosis_raw = as.character(matrix$diagnosis),
        note = if (is.null(matrix[["note"]])) NA_character_ else as.character(matrix[["note"]]),
        fit_gate_status = as.character(fit_gate_status),
        fit_valid = as.logical(fit_valid),
        interpretable = can_interpret_power_scaling(fit_valid, fit_gate_status, importance_status),
        pareto_k_max = as.numeric(one_block$pareto_k_max),
        importance_sampling_draws = if (is.null(one_block$importance_sampling_draws)) NA_real_ else as.numeric(one_block$importance_sampling_draws),
        pareto_k_threshold = calculate_importance_sampling_threshold(one_block$importance_sampling_draws),
        importance_sampling_status = importance_status,
        settings_status = "explicit",
        priorsense_version = as.character(sensitivity$priorsense_version)
      )
    }))
  }))
}

#' Save remaining power-scaling counts and density coordinates for reporting
add_prior_reporting_facts <- function(report, analysis_plan) {
  report$prior_posterior_density <- calculate_prior_posterior_density_data(report$coefficients_primary, analysis_plan)
  report$powerscale$n_blocks <- length(report$powerscale$blocks)
  report$powerscale$threshold <- unique(stats::na.omit(report$priorsense$sensitivity_threshold))
  report$answer$n_unreliable <- nrow(report$answer$unreliable_blocks) + nrow(report$answer$unavailable_blocks)
  report$answer$n_unreliable_blocks <- nrow(report$answer$unreliable_blocks)
  report$answer$n_unavailable_blocks <- nrow(report$answer$unavailable_blocks)
  report
}
