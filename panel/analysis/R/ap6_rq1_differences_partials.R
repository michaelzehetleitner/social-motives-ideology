# AP6 — Coefficient Differences and Partial Correlations of the Motives (RQ1)
#
# The two quantities AP6 adds to the four regressions: for every pair of motives with opposite predicted signs for an
# outcome, the coefficient of the motive predicted positive minus that of the
# motive predicted negative, in every posterior draw of that outcome's
# regression; and the partial correlations of the five motives, from one joint
# model of the motive scores on the covariates with correlated residuals,
# converted from the residual correlation matrix in every draw. Both are
# classified by AP10: credible when the central interval excludes zero.

#' The pairs of motives with opposite predicted signs, per outcome
#'
#' Read from the predicted-signs table of the configuration (the block the
#' decision code reads), so the list follows the preregistration's table: four
#' pairs for authoritarian aggression, conventionalism and SDO-D, three for
#' authoritarian submission.
#'
#' @param analysis_plan Configuration from [zm_config()].
#' @return Tibble `outcome_key`, `motive_positive`, `motive_negative`.
ap6_list_predicted_difference_pairs <- function(analysis_plan) {
  table <- analysis_plan$predictions$table
  outcomes <- as.character(analysis_plan$regression$outcomes)
  motives <- as.character(analysis_plan$regression$motives)
  dplyr::bind_rows(lapply(outcomes, function(outcome) {
    signs <- vapply(motives, function(m) as.character(table[[outcome]][[m]]), character(1))
    positive <- motives[signs == "+"]
    negative <- motives[signs == "-"]
    pairs <- expand.grid(motive_positive = positive, motive_negative = negative, stringsAsFactors = FALSE)
    tibble::tibble(outcome_key = rep(outcome, nrow(pairs)),
                   motive_positive = pairs$motive_positive, motive_negative = pairs$motive_negative)
  }))
}

#' Summarise the draws of one quantity by AP10
#'
#' @param draws Numeric vector of posterior draws.
#' @param ci_level Interval level (0.95).
#' @return One-row tibble `median`, `lower`, `upper`, `credible`, `sign`.
ap6_summarise_quantity_draws <- function(draws, ci_level) {
  tail <- (1 - ci_level) / 2
  bounds <- stats::quantile(draws, c(tail, 1 - tail), names = FALSE)
  median <- stats::median(draws)
  tibble::tibble(
    median = median,
    lower = bounds[[1]],
    upper = bounds[[2]],
    credible = bounds[[1]] > 0 | bounds[[2]] < 0,
    sign = ifelse(median > 0, "positive", ifelse(median < 0, "negative", "zero"))
  )
}

#' Whether a gated fit passed the validity gate
#'
#' @param fit A fit returned by [apply_validity_gate()].
ap6_fit_passed_gate <- function(fit) {
  if (is.null(fit) || inherits(fit, "error")) return(FALSE)
  diagnostics <- attr(fit, "diagnostics", exact = TRUE)
  isTRUE(diagnostics$ok) && attr(fit, "gate_status", exact = TRUE) %in% ap7_gate_statuses_ok()
}

#' AP6 Coefficient Differences: the motive predicted positive minus the motive predicted negative
#'
#' Within each of the four primary regressions, the difference is formed in
#' every posterior draw, so that the dependence between the two coefficients
#' enters its interval. Each row also says whether each of the two
#' coefficients alone is credible, because a difference adds information only
#' where at least one of them is not. A fit that did not pass the validity
#' gate gives no classification (AP6).
#'
#' @param primary_fits The gated `regression_primary_fits`, named by outcome.
#' @param analysis_plan Configuration from [zm_config()].
#' @return Tibble with one row per pair: `outcome_key`, `motive_positive`,
#'   `motive_negative`, `median`, `lower`, `upper`, `credible`, `sign`,
#'   `positive_credible`, `negative_credible`, `interpretable`.
calculate_coefficient_differences <- function(primary_fits, analysis_plan) {
  pairs <- ap6_list_predicted_difference_pairs(analysis_plan)
  identity <- ap6_regression_outcome_identity(analysis_plan)
  ci_level <- as.numeric(analysis_plan$regression$ci_level)
  column_of <- function(motive) names(analysis_plan$regression$term_keys)[analysis_plan$regression$term_keys == motive][1]
  dplyr::bind_rows(lapply(seq_len(nrow(pairs)), function(i) {
    pair <- pairs[i, ]
    outcome <- identity$outcome[match(pair$outcome_key, identity$outcome_key)]
    fit <- primary_fits[[outcome]]
    interpretable <- ap6_fit_passed_gate(fit)
    if (is.null(fit) || inherits(fit, "error")) {
      return(tibble::tibble(pair, median = NA_real_, lower = NA_real_, upper = NA_real_, credible = NA,
                            sign = NA_character_, positive_credible = NA, negative_credible = NA,
                            interpretable = FALSE))
    }
    draws <- posterior::as_draws_df(fit)
    b_positive <- draws[[paste0("b_", column_of(pair$motive_positive))]]
    b_negative <- draws[[paste0("b_", column_of(pair$motive_negative))]]
    if (is.null(b_positive) || is.null(b_negative)) {
      stop("The regression of ", pair$outcome_key, " has no coefficient for ", pair$motive_positive,
           " or ", pair$motive_negative, ".", call. = FALSE)
    }
    summary <- ap6_summarise_quantity_draws(b_positive - b_negative, ci_level)
    if (!interpretable) summary$credible <- NA
    tibble::tibble(
      pair, summary,
      positive_credible = if (interpretable) ap6_summarise_quantity_draws(b_positive, ci_level)$credible else NA,
      negative_credible = if (interpretable) ap6_summarise_quantity_draws(b_negative, ci_level)$credible else NA,
      interpretable = interpretable
    )
  }))
}

#' AP6 Partial Correlations of the Motives: the joint model of the motive scores
#'
#' One equation per motive score on the covariates of the regressions (age,
#' gender and income), fitted on the regressions' cases with correlated
#' residuals; the AP6 priors for every equation and LKJ(2) on the residual
#' correlations, as the call in AP6 states.
#'
#' @param data `data_regressions`, the known-gender cases of the four regressions.
#' @param regression_model_set `regression_model_set`; its formulas name the covariates.
#' @param analysis_plan Configuration from [zm_config()].
#' @return `list(data, formula, priors, response_ids, motive_keys)`.
define_motive_model <- function(data, regression_model_set, analysis_plan) {
  motive_keys <- as.character(analysis_plan$regression$motives)
  term_keys <- analysis_plan$regression$term_keys
  motive_columns <- vapply(motive_keys, function(m) names(term_keys)[term_keys == m][1], character(1))
  predictors <- all.vars(regression_model_set$formulas[[1L]])[-1L]
  covariates <- setdiff(predictors, motive_columns)  # age_z, gender, income_z
  data <- select_regression_data_for_model(data, c(motive_columns, covariates))
  response_ids <- stats::setNames(gsub("[^[:alnum:]]", "", motive_columns), motive_keys)
  unavailable <- ap6_capture_imputation_unavailability(
    require_valid_imputation_inputs(data, c(motive_columns, covariates))
  )
  if (inherits(unavailable, "imputation_unavailable")) {
    return(list(data = data, response_ids = response_ids, motive_keys = motive_keys, unavailable = unavailable))
  }
  motive_formula <- Reduce(`+`, lapply(unname(motive_columns), function(column) {
    brms::bf(stats::as.formula(paste(column, "~", paste(covariates, collapse = " + "))),
             family = brms::brmsfamily("gaussian"))
  })) + brms::set_rescor(TRUE)
  metric_covariates <- covariates[covariates %in% names(term_keys)]
  priors <- c(
    ap6_build_joint_metric_priors(response_ids, metric_covariates, analysis_plan$priors$slope_sd_primary),  # normal(0, .20)
    ap6_build_joint_gender_contrast_priors(motive_formula, data, response_ids, analysis_plan$priors$gender_sd),  # normal(0, .40)
    do.call(c, unname(lapply(response_ids, function(r) {
      c(brms::set_prior(paste0("normal(0, ", analysis_plan$priors$intercept_sd, ")"), class = "Intercept", resp = r, tag = "intercept"),
        brms::set_prior(paste0("normal(0, ", analysis_plan$priors$sigma_sd, ")"), class = "sigma", resp = r, tag = "sigma"))
    }))),
    brms::set_prior(analysis_plan$priors$rescor, class = "rescor", tag = "rescor")  # LKJ(2)
  )
  list(data = data, formula = motive_formula, priors = priors, response_ids = response_ids,
       motive_keys = motive_keys)
}

#' Fit the joint model of the motive scores
#'
#' The AP6 sampling settings (4 chains; 2,000 warm-up; 6,000 post-warm-up draws
#' per chain in the full profile; seed 20260905); the caller passes the fit to
#' [apply_validity_gate()], which for this model also checks every
#' residual correlation ([ap6_gated_rescor_vars()]).
#'
#' @param motive_model The model of [define_motive_model()].
#' @param analysis_plan Configuration from [zm_config()].
#' @return A `brmsfit`, or the error of a failed sampler call.
fit_motive_model <- function(motive_model, analysis_plan) {
  if (inherits(motive_model$unavailable, "imputation_unavailable")) return(motive_model$unavailable)
  sampling <- analysis_plan$regression
  ap6_capture_joint_fit(brms::brm(
    formula = motive_model$formula,
    data = motive_model$data,
    prior = motive_model$priors,
    sample_prior = "no",
    backend = sampling$backend,
    chains = sampling$chains,
    cores = sampling$cores,
    warmup = sampling$warmup,
    iter = sampling$warmup + sampling$iter_per_chain,
    thin = 1,
    seed = sampling$seed,
    refresh = 0
  ))
}

#' The partial correlations implied by one correlation matrix
#'
#' partial(i, j) = -(R^-1)_ij / sqrt((R^-1)_ii (R^-1)_jj): the correlation of
#' i and j conditional on every other variable of R.
#'
#' @param R A symmetric, positive-definite correlation matrix.
#' @return The matrix of partial correlations, with ones on the diagonal.
ap6_convert_correlations_to_partial <- function(R) {
  precision <- solve(R)
  scale <- sqrt(diag(precision))
  partial <- -precision / outer(scale, scale)
  diag(partial) <- 1
  partial
}

#' AP6 Partial Correlations of the Motives, by AP10
#'
#' In every posterior draw the residual correlation matrix of the motive model
#' is inverted and converted into partial correlations, each conditional on
#' the other three motives, age, gender and income. Each of the ten pairs is
#' summarised by its median and central interval and classified as credible
#' when the interval excludes zero; a fit that did not pass the validity gate
#' gives no classification.
#'
#' @param motive_fit The gated fit of the motive model (`motive_partial_fit$motives`).
#' @param motive_model The model of [define_motive_model()] (response ids and keys).
#' @param analysis_plan Configuration from [zm_config()].
#' @return Tibble `motive_a`, `motive_b`, `median`, `lower`, `upper`,
#'   `credible`, `sign`, `interpretable`.
calculate_motive_partial_correlations <- function(motive_fit, motive_model, analysis_plan) {
  keys <- motive_model$motive_keys
  ids <- unname(motive_model$response_ids[keys])
  pairs <- utils::combn(seq_along(keys), 2L)
  empty <- tibble::tibble(motive_a = keys[pairs[1, ]], motive_b = keys[pairs[2, ]], median = NA_real_,
                          lower = NA_real_, upper = NA_real_, credible = NA, sign = NA_character_,
                          interpretable = FALSE, note = describe_unavailable_regression_fit(motive_fit))
  if (is.null(motive_fit) || inherits(motive_fit, "error")) return(empty)
  interpretable <- ap6_fit_passed_gate(motive_fit)
  draws <- posterior::as_draws_df(motive_fit)
  rescor_of <- function(a, b) {
    name <- c(paste0("rescor__", a, "__", b), paste0("rescor__", b, "__", a))
    name <- name[name %in% names(draws)]
    if (length(name) == 0L) stop("The motive model has no residual correlation of ", a, " and ", b, ".", call. = FALSE)
    draws[[name[[1]]]]
  }
  n <- length(ids)
  cors <- matrix(list(), n, n)
  for (i in seq_len(n)) for (j in seq_len(n)) if (i < j) cors[[i, j]] <- rescor_of(ids[i], ids[j])
  n_draws <- nrow(draws)
  partial_draws <- array(NA_real_, dim = c(n_draws, n, n))
  for (d in seq_len(n_draws)) {
    R <- diag(n)
    for (i in seq_len(n)) for (j in seq_len(n)) if (i < j) R[i, j] <- R[j, i] <- cors[[i, j]][d]
    partial_draws[d, , ] <- ap6_convert_correlations_to_partial(R)
  }
  ci_level <- as.numeric(analysis_plan$regression$ci_level)
  out <- dplyr::bind_rows(lapply(seq_len(ncol(pairs)), function(k) {
    i <- pairs[1, k]; j <- pairs[2, k]
    tibble::tibble(motive_a = keys[i], motive_b = keys[j],
                   ap6_summarise_quantity_draws(partial_draws[, i, j], ci_level))
  }))
  if (!interpretable) out$credible <- NA
  out$interpretable <- interpretable
  out$note <- NA_character_
  out
}
