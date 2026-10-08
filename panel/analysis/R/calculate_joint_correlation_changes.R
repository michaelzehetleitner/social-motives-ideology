# Approved extension: compare the joint model's residual correlations with
# its model-implied marginal correlations over the empirical predictor
# distribution of the same analysis cases. Every difference is paired by draw.

#' Derive paired marginal, residual and difference correlations from saved draws
#'
#' The casewise expected-outcome covariance uses divisor N: it is covariance
#' under the empirical distribution giving each observed predictor row weight
#' 1/N. Adding the residual covariance applies the law of total covariance.
#' No predictor-distribution uncertainty or simulated residual noise is added.
#'
#' @param expected_outcomes Array: posterior draw x analysis case x response.
#' @param residual_covariances Array: the same draw x response x response.
#' @param draw_ids Optional unique identifiers for the paired draws.
#' @param response_ids Optional unique response identifiers. If both arrays
#'   carry response names, covariance axes are explicitly matched by name.
#' @param total_weights Optional named raw-facet weights in standardised-response
#'   coordinates; when supplied, append ASC total versus SDO to the facet pairs.
#' @param sdo_response Response identifier for SDO.
#' @return Data frame with one row per draw and retained outcome pair.
derive_correlation_changes_from_draws <- function(
    expected_outcomes, residual_covariances, draw_ids = NULL, response_ids = NULL,
    total_weights = NULL, sdo_response = "sdodomz") {
  expected_dims <- dim(expected_outcomes)
  covariance_dims <- dim(residual_covariances)
  if (length(expected_dims) != 3L || length(covariance_dims) != 3L ||
      expected_dims[1L] < 1L || expected_dims[2L] < 2L || expected_dims[3L] < 2L ||
      !identical(as.integer(covariance_dims), as.integer(expected_dims[c(1L, 3L, 3L)]))) {
    stop("Expected-outcome and residual-covariance arrays need matching draw/response dimensions and at least two cases.", call. = FALSE)
  }
  if (!is.numeric(expected_outcomes) || !is.numeric(residual_covariances) ||
      any(!is.finite(expected_outcomes)) || any(!is.finite(residual_covariances))) {
    stop("Expected outcomes and residual covariances must be finite numeric values.", call. = FALSE)
  }
  n_draws <- expected_dims[1L]
  n_cases <- expected_dims[2L]
  n_responses <- expected_dims[3L]
  expected_names <- dimnames(expected_outcomes)[[3L]]
  if (is.null(response_ids)) {
    response_ids <- if (is.null(expected_names)) paste0("response_", seq_len(n_responses)) else expected_names
  }
  response_ids <- as.character(response_ids)
  if (length(response_ids) != n_responses || anyNA(response_ids) ||
      any(!nzchar(response_ids)) || anyDuplicated(response_ids)) {
    stop("response_ids must uniquely identify every response.", call. = FALSE)
  }
  if (!is.null(expected_names)) {
    if (anyDuplicated(expected_names) || !setequal(expected_names, response_ids)) {
      stop("Expected-outcome names do not match response_ids.", call. = FALSE)
    }
    expected_outcomes <- expected_outcomes[, , match(response_ids, expected_names), drop = FALSE]
  }
  covariance_names <- dimnames(residual_covariances)
  for (axis in 2:3) {
    names_on_axis <- covariance_names[[axis]]
    if (!is.null(names_on_axis)) {
      if (anyDuplicated(names_on_axis) || !setequal(names_on_axis, response_ids)) {
        stop("Residual-covariance names do not match response_ids.", call. = FALSE)
      }
      if (axis == 2L) residual_covariances <- residual_covariances[, match(response_ids, names_on_axis), , drop = FALSE]
      if (axis == 3L) residual_covariances <- residual_covariances[, , match(response_ids, names_on_axis), drop = FALSE]
    }
  }
  if (is.null(draw_ids)) draw_ids <- seq_len(n_draws)
  if (length(draw_ids) != n_draws || anyNA(draw_ids) || anyDuplicated(draw_ids)) {
    stop("draw_ids must uniquely identify each paired posterior draw.", call. = FALSE)
  }
  if (!is.null(total_weights)) {
    if (!is.numeric(total_weights) || any(!is.finite(total_weights)) ||
        is.null(names(total_weights)) || anyDuplicated(names(total_weights)) ||
        !setequal(names(total_weights), response_ids) ||
        !sdo_response %in% response_ids || any(total_weights < 0) ||
        !any(total_weights > 0) || total_weights[[sdo_response]] != 0) {
      stop("Total weights must name all responses, be finite nonnegative and exclude SDO.", call. = FALSE)
    }
    total_weights <- total_weights[response_ids]
  }
  pairs <- utils::combn(seq_len(n_responses), 2L)
  indices <- cbind(pairs[1L, ], pairs[2L, ])
  n_pairs <- ncol(pairs) + as.integer(!is.null(total_weights))
  unadjusted <- residual <- matrix(NA_real_, n_draws, n_pairs)
  total_correlation <- function(covariance) {
    sdo <- match(sdo_response, response_ids)
    as.numeric(crossprod(total_weights, covariance[, sdo]) /
      sqrt(crossprod(total_weights, covariance %*% total_weights) * covariance[sdo, sdo]))
  }
  for (draw in seq_len(n_draws)) {
    covariance <- residual_covariances[draw, , ]
    if (!isTRUE(isSymmetric(unname(covariance))) ||
        inherits(tryCatch(chol(covariance), error = identity), "error")) {
      stop("Residual covariance is not symmetric positive definite at draw ", draw_ids[draw], ".", call. = FALSE)
    }
    expected <- expected_outcomes[draw, , ]
    centered <- sweep(expected, 2L, colMeans(expected), "-")
    total_covariance <- crossprod(centered) / n_cases + covariance
    unadjusted[draw, seq_len(ncol(pairs))] <- stats::cov2cor(total_covariance)[indices]
    residual[draw, seq_len(ncol(pairs))] <- stats::cov2cor(covariance)[indices]
    if (!is.null(total_weights)) {
      unadjusted[draw, n_pairs] <- total_correlation(total_covariance)
      residual[draw, n_pairs] <- total_correlation(covariance)
    }
  }
  outcome_1 <- response_ids[pairs[1L, ]]
  outcome_2 <- response_ids[pairs[2L, ]]
  if (!is.null(total_weights)) {
    outcome_1 <- c(outcome_1, "asc_total")
    outcome_2 <- c(outcome_2, sdo_response)
  }
  data.frame(
    .draw = rep(draw_ids, each = n_pairs),
    outcome_1 = rep(outcome_1, times = n_draws),
    outcome_2 = rep(outcome_2, times = n_draws),
    unadjusted = as.vector(t(unadjusted)), residual = as.vector(t(residual)),
    difference = as.vector(t(residual - unadjusted)), stringsAsFactors = FALSE
  )
}

#' Summarise the paired correlation quantities without evidence classifications
#'
#' Invalid-fit summaries remain visible and explicitly non-interpretable, in
#' accordance with the existing joint-model gate. The gate is not redefined.
summarise_joint_correlation_changes <- function(draws, n, validity, interval_level = .95) {
  if (length(interval_level) != 1L || !is.finite(interval_level) ||
      interval_level <= 0 || interval_level >= 1) {
    stop("interval_level must lie strictly between zero and one.", call. = FALSE)
  }
  pairs <- unique(draws[c("outcome_1", "outcome_2")])
  probabilities <- c(.5, (1 - interval_level) / 2, 1 - (1 - interval_level) / 2)
  rows <- lapply(seq_len(nrow(pairs)), function(i) {
    pair <- pairs[i, , drop = FALSE]
    rows <- draws$outcome_1 == pair$outcome_1 & draws$outcome_2 == pair$outcome_2
    for (quantity in c("unadjusted", "residual", "difference")) {
      values <- stats::quantile(draws[[quantity]][rows], probabilities, names = FALSE)
      for (j in seq_along(values)) pair[[paste0(quantity, "_", c("median", "lower", "upper")[j])]] <- values[j]
    }
    pair$n <- as.integer(n)
    pair$interpretable <- isTRUE(validity$fit_valid)
    pair
  })
  do.call(rbind, rows)
}

#' Compare residual and model-implied unadjusted correlations of the AP7 fit
#'
#' @param joint_regression_fit Existing gated list(joint = brmsfit) target.
#' @param interval_level Probability of the central equal-tailed CrI.
#' @param data_regressions Saved same-sample regression data and scaling constants.
#' @return List summaries (six facet pairs plus ASC total–SDO-D), paired draws, the unchanged AP7 validity
#'   record, and metadata stating the sample and fixed-design estimand. No
#'   coefficients, predictions or model fits are changed.
calculate_joint_correlation_changes <- function(joint_regression_fit, interval_level = .95,
                                                data_regressions = NULL) {
  if (length(interval_level) != 1L || !is.finite(interval_level) ||
      interval_level <= 0 || interval_level >= 1) {
    stop("interval_level must lie strictly between zero and one.", call. = FALSE)
  }
  response_keys <- c(ascaggz = "asc_agg", ascsubz = "asc_sub",
                     ascconvz = "asc_conv", sdodomz = "sdo_dom")
  fit <- joint_regression_fit$joint
  validity <- ap8_extract_joint_fit_validity(fit)
  metadata <- list(
    interval_level = interval_level, subtraction = "residual minus model-implied unadjusted",
    estimand = "Marginal outcome correlation over the empirical joint predictor distribution of the joint-model analysis cases",
    predictor_distribution = "Fixed observed predictor rows, including motives and demographics",
    weighting = "Equal case weights, 1/N", covariance_divisor = "N",
    source = "Existing AP7 joint posterior", n = NA_integer_, n_draws = 0L
  )
  if (!inherits(fit, "brmsfit")) {
    pairs <- utils::combn(unname(response_keys), 2L)
    summaries <- data.frame(outcome_1 = c(pairs[1L, ], "asc_total"),
                            outcome_2 = c(pairs[2L, ], "sdo_dom"), stringsAsFactors = FALSE)
    for (quantity in c("unadjusted", "residual", "difference")) {
      for (statistic in c("median", "lower", "upper")) summaries[[paste0(quantity, "_", statistic)]] <- NA_real_
    }
    summaries$n <- NA_integer_
    summaries$interpretable <- FALSE
    draws <- data.frame(.draw = integer(), outcome_1 = character(), outcome_2 = character(),
                        unadjusted = numeric(), residual = numeric(), difference = numeric())
    return(list(summaries = summaries, draws = draws, validity = validity, metadata = metadata))
  }
  if (!all(vapply(fit$family, function(family) {
    identical(family$family, "gaussian") && identical(family$link, "identity")
  }, logical(1)))) {
    stop("The approved correlation comparison requires the existing multivariate Gaussian joint model.", call. = FALSE)
  }
  if (NROW(fit$ranef) > 0L || !is.null(fit$autocor) ||
      !isTRUE(fit$formula$rescor) ||
      any(vapply(fit$formula$forms, function(form) length(form$pforms) > 0L, logical(1)))) {
    stop("The approved comparison requires constant residual covariance and no multilevel or autocorrelation structure.", call. = FALSE)
  }
  sample <- reconstruct_joint_asc_aggregation_sample(fit$data, data_regressions)
  weights <- stats::setNames(c(unname(sample$facet_sd) / 3, 0), names(response_keys))
  metadata$raw_facet_weights <- weights
  metadata$sample_alignment_verified <- TRUE
  expected <- brms::posterior_epred(fit, resp = names(response_keys))
  covariances <- brms::VarCorr(fit, summary = FALSE)$residual__$cov
  paired_draws <- derive_correlation_changes_from_draws(
    expected, covariances, response_ids = names(response_keys), total_weights = weights)
  outcome_keys <- c(response_keys, asc_total = "asc_total")
  paired_draws$outcome_1 <- unname(outcome_keys[paired_draws$outcome_1])
  paired_draws$outcome_2 <- unname(response_keys[paired_draws$outcome_2])
  metadata$n <- as.integer(dim(expected)[2L])
  metadata$n_draws <- as.integer(dim(expected)[1L])
  list(
    summaries = summarise_joint_correlation_changes(
      paired_draws, metadata$n, validity, interval_level),
    draws = paired_draws, validity = validity, metadata = metadata
  )
}
