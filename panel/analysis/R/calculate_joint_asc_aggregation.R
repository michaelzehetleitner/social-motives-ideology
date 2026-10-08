# Equal-weight raw-score ASC total, derived from the joint posterior.
# Total–facet comparisons use standardised coefficients; no model is refitted.

#' Derive ASC aggregate associations on the joint model's verified sample
#'
#' The aggregate is the mean of the three raw, already-preprocessed facet
#' scores. Saved known-gender standardisation constants reverse the outcome
#' scaling. All contrasts retain posterior draw pairing. No new validity rule
#' is introduced: numerical summaries from an invalid fit are diagnostic only.
#'
#' @param joint_regression_fit Gated collection containing the joint brmsfit.
#' @param data_regressions Saved regression data with respondent ids and the
#'   `z_parameters` and `regression_participants` attributes.
#' @param interval_level Central equal-tailed credible-interval level.
#' @param analysis_plan Configuration from [zm_config()]; names the motives and
#'   their order ([extract_joint_posterior()]). Not read without a fit.
#' @return List with aggregate_coefficients (five rows, one per motive key, in
#'   the plan's order), facet_comparisons (15 rows), validity, metadata,
#'   interpretation_allowed, and paired draws.
calculate_joint_asc_aggregation <- function(joint_regression_fit,
                                          data_regressions,
                                          interval_level,
                                          analysis_plan) {
  fit <- joint_regression_fit$joint
  validity <- ap8_extract_joint_fit_validity(fit)
  if (!inherits(fit, "brmsfit")) {
    result <- calculate_joint_asc_aggregation_from_draws(
      tibble::tibble(.draw = integer(), motive = character(),
                     aggression = numeric(), submission = numeric(),
                     conventionalism = numeric()),
      c(aggression = 1, submission = 1, conventionalism = 1),
      aggregate_sd = 1, interval_level = interval_level, validity = validity)
    result$metadata$aggregate_sd <- NA_real_
    result$metadata$facet_sd[] <- NA_real_
    result$metadata$n <- NA_integer_
    result$metadata$sample_alignment_verified <- FALSE
    return(result)
  }
  sample <- reconstruct_joint_asc_aggregation_sample(fit$data, data_regressions)
  posterior <- extract_joint_posterior(joint_regression_fit, analysis_plan)
  result <- calculate_joint_asc_aggregation_from_draws(
    posterior$coefficients, sample$facet_sd, sample$aggregate_sd,
    interval_level, posterior$validity)
  result$metadata$n <- nrow(data_regressions)
  result$metadata$aggregate_mean <- mean(sample$aggregate_scores)
  result$metadata$scaling_parameters <- sample$scaling_parameters
  result$metadata$sample_alignment_verified <- TRUE
  result$metadata$sample_alignment_method <- paste(
    "Exact ordered equality of every joint model variable; source respondent",
    "ids equal saved regression_participants. The fit does not retain ids.")
  result$metadata$participants <- data_regressions$respondent_id
  result
}

#' Verify the joint rows and reconstruct raw facets from their saved constants
#'
#' No row is selected or reordered. Numerical equality of every model column
#' protects against silently applying the all-row constants or another sample.
#' brms may attach contrasts to gender; its ordered values and levels must
#' agree, while the extra contrasts attribute is irrelevant to row alignment.
reconstruct_joint_asc_aggregation_sample <- function(fit_data, data_regressions) {
  facet_keys <- c(aggression = "asc_agg", submission = "asc_sub",
                  conventionalism = "asc_conv")
  # Every column of the joint model, checked one by one (a set; the motives are
  # written in the codebook's order).
  model_columns <- c(paste0(unname(facet_keys), "_z"), "sdo_dom_z",
                     "zm_security_z", "zm_arousal_z", "zm_power_z", "zm_prestige_z",
                     "zm_achievement_z", "age_z", "gender", "income_z")
  if (!is.data.frame(fit_data) || !is.data.frame(data_regressions) ||
      nrow(data_regressions) < 2L || nrow(fit_data) != nrow(data_regressions) ||
      !all(model_columns %in% names(fit_data)) ||
      !all(c("respondent_id", model_columns) %in% names(data_regressions))) {
    stop("Joint fit and regression data must contain the same complete model sample.")
  }
  participants <- data_regressions$respondent_id
  if (anyNA(participants) || anyDuplicated(participants) ||
      !identical(participants, attr(data_regressions, "regression_participants", exact = TRUE))) {
    stop("Regression respondent ids must match the saved unique participant order.")
  }
  for (column in model_columns) {
    fitted <- fit_data[[column]]
    saved <- data_regressions[[column]]
    if (column == "gender") {
      agrees <- !anyNA(saved) && identical(as.character(fitted), as.character(saved)) &&
        identical(levels(fitted), levels(saved))
    } else {
      agrees <- is.numeric(fitted) && is.numeric(saved) &&
        all(is.finite(saved)) && identical(as.numeric(fitted), as.numeric(saved))
    }
    if (!isTRUE(agrees)) {
      stop("Joint fit and regression data differ in ordered model column: ", column, ".")
    }
  }
  parameters <- attr(data_regressions, "z_parameters", exact = TRUE)
  required <- c("var", "source", "transform", "z_col", "mean", "sd", "n")
  if (!is.data.frame(parameters) || !all(required %in% names(parameters)) ||
      anyDuplicated(parameters$var) || !all(facet_keys %in% parameters$var)) {
    stop("Saved regression standardisation parameters are incomplete or duplicated.")
  }
  parameters <- parameters[match(facet_keys, parameters$var), , drop = FALSE]
  if (anyNA(parameters) || !all(parameters$source == facet_keys) ||
      !all(parameters$transform == "identity") ||
      !all(parameters$z_col == paste0(facet_keys, "_z")) ||
      !all(is.finite(parameters$mean)) || !all(is.finite(parameters$sd)) ||
      any(parameters$sd <= 0) || !all(parameters$n == nrow(data_regressions))) {
    stop("Saved facet constants must describe identity scaling on the joint sample.")
  }
  raw_facets <- vapply(seq_along(facet_keys), function(index) {
    z <- data_regressions[[parameters$z_col[[index]]]]
    if (abs(mean(z)) > 1e-8 || abs(stats::sd(z) - 1) > 1e-8) {
      stop("Saved facet z scores are not standardised within the joint sample.")
    }
    z * parameters$sd[[index]] + parameters$mean[[index]]
  }, numeric(nrow(data_regressions)))
  colnames(raw_facets) <- names(facet_keys)
  aggregate_scores <- rowMeans(raw_facets)
  aggregate_sd <- stats::sd(aggregate_scores)
  if (!is.finite(aggregate_sd) || aggregate_sd <= 0) {
    stop("The raw-facet ASC aggregate must have a positive empirical SD.")
  }
  list(raw_facets = raw_facets, aggregate_scores = aggregate_scores,
       facet_sd = stats::setNames(parameters$sd, names(facet_keys)),
       aggregate_sd = aggregate_sd, scaling_parameters = parameters)
}

#' Calculate aggregate and attenuation summaries from paired coefficient draws
#'
#' gamma_k = facet_sd[k] * b_k; gamma_T = mean(gamma_k);
#' b_T = gamma_T / aggregate_sd; D_k = abs(b_k) - abs(b_T).
#' Signed differences are b_k - b_T, in standardised units.
#' This pure calculation is separately testable without a model or store.
#' A motive is named by its key (`motive` and `predictor_key` both carry it).
#'
#' @param motives The motive keys, in the order the result lists them; by
#'   default as the draws bring them ([extract_joint_posterior()] brings
#'   them in the plan's order). A draw of any other motive stops.
calculate_joint_asc_aggregation_from_draws <- function(coefficients, facet_sd,
                                                     aggregate_sd, interval_level,
                                                     validity,
                                                     motives = unique(as.character(coefficients$motive))) {
  facets <- c(aggression = "asc_agg", submission = "asc_sub", conventionalism = "asc_conv")
  motives <- as.character(motives)
  if (!is.numeric(interval_level) || length(interval_level) != 1L ||
      !is.finite(interval_level) || interval_level <= 0 || interval_level >= 1) {
    stop("interval_level must be a number strictly between zero and one.")
  }
  if (!is.numeric(facet_sd) || length(facet_sd) != 3L ||
      !setequal(names(facet_sd), names(facets)) || anyDuplicated(names(facet_sd)) ||
      any(!is.finite(facet_sd)) || any(facet_sd <= 0) ||
      !is.numeric(aggregate_sd) || length(aggregate_sd) != 1L ||
      !is.finite(aggregate_sd) || aggregate_sd <= 0) {
    stop("Facet SDs and aggregate_sd must be finite and positive.")
  }
  if (!is.list(validity) || !all(c("fit_valid", "gate_status", "note") %in% names(validity))) {
    stop("The saved joint validity result is required.")
  }
  required <- c(".draw", "motive", names(facets))
  if (!is.data.frame(coefficients) || !all(required %in% names(coefficients)) ||
      anyNA(coefficients[required]) || any(!coefficients$motive %in% motives) ||
      !is.numeric(coefficients$.draw) || any(!is.finite(coefficients$.draw)) ||
      anyDuplicated(coefficients[c(".draw", "motive")]) ||
      any(!vapply(coefficients[names(facets)], is.numeric, logical(1))) ||
      any(!is.finite(as.matrix(coefficients[names(facets)])))) {
    stop("Coefficient draws require finite facets, known motives and unique paired draw ids.")
  }
  included_motives <- motives[motives %in% coefficients$motive]
  if (length(included_motives)) {
    first_ids <- sort(coefficients$.draw[coefficients$motive == included_motives[[1]]])
    if (any(vapply(included_motives, function(motive) {
      !identical(sort(coefficients$.draw[coefficients$motive == motive]), first_ids)
    }, logical(1)))) stop("Every motive must retain the same posterior draw ids.")
  }
  aggregate_names <- c("b_aggregate", "gamma_aggregate")
  comparison_names <- c("b_facet", "b_aggregate", "gamma_facet", "gamma_aggregate",
                        "signed_difference", "attenuation")
  empty_summary <- function(comparisons = FALSE) {
    result <- tibble::tibble(motive = character(), predictor_key = character())
    if (comparisons) {
      result$facet <- character()
      result$outcome_key <- character()
    }
    for (quantity in if (comparisons) comparison_names else aggregate_names) {
      for (statistic in c("median", "lower", "upper")) {
        result[[paste0(quantity, "_", statistic)]] <- numeric()
      }
    }
    if (comparisons) {
      result$classification <- character()
      result$direction_comparison <- character()
    }
    result
  }
  summarise_values <- function(values, prefix) {
    stats::setNames(as.list(c(stats::median(values), stats::quantile(values,
      probs = c((1 - interval_level) / 2, (1 + interval_level) / 2), names = FALSE))),
      paste0(prefix, c("_median", "_lower", "_upper")))
  }
  aggregate_rows <- comparison_rows <- aggregate_draws <- comparison_draws <- list()
  for (motive in included_motives) {
    motive_key <- motive
    draws <- coefficients[coefficients$motive == motive, , drop = FALSE]
    gamma <- sweep(as.matrix(draws[names(facets)]), 2, facet_sd[names(facets)], "*")
    gamma_aggregate <- rowMeans(gamma)
    b_aggregate <- gamma_aggregate / aggregate_sd
    aggregate_rows[[motive]] <- tibble::as_tibble(c(
      list(motive = motive, predictor_key = motive_key),
      summarise_values(b_aggregate, "b_aggregate"),
      summarise_values(gamma_aggregate, "gamma_aggregate")))
    aggregate_draws[[motive]] <- tibble::tibble(
      .draw = draws$.draw, motive = motive, predictor_key = motive_key,
      b_aggregate = b_aggregate, gamma_aggregate = gamma_aggregate)
    for (facet in names(facets)) {
      difference <- draws[[facet]] - b_aggregate
      attenuation <- abs(draws[[facet]]) - abs(b_aggregate)
      key <- paste(motive, facet)
      values <- list(b_facet = draws[[facet]], b_aggregate = b_aggregate, gamma_facet = gamma[, facet],
                     gamma_aggregate = gamma_aggregate, signed_difference = difference,
                     attenuation = attenuation)
      summaries <- unlist(lapply(names(values), function(quantity) {
        summarise_values(values[[quantity]], quantity)
      }), recursive = FALSE)
      strength <- ap8_summarise_strength_differences(
        tibble::tibble(.draw = draws$.draw, motive = motive,
                       facet = draws[[facet]], total = b_aggregate),
        list("facet minus total" = c("facet", "total")), interval_level)
      allowed <- isTRUE(validity$fit_valid) && validity$gate_status %in% c("ok", "retried_ok")
      summaries$classification <- if (allowed) strength$classification else NA_character_
      summaries$direction_comparison <- if (allowed) strength$direction_comparison else NA_character_
      comparison_rows[[key]] <- tibble::as_tibble(c(list(
        motive = motive, predictor_key = motive_key, facet = facet,
        outcome_key = unname(facets[[facet]])), summaries))
      comparison_draws[[key]] <- tibble::as_tibble(c(list(
        .draw = draws$.draw, motive = rep(motive, nrow(draws)),
        predictor_key = rep(motive_key, nrow(draws)),
        facet = rep(facet, nrow(draws)), outcome_key = rep(unname(facets[[facet]]), nrow(draws))), values))
    }
  }
  bind_rows <- function(rows, empty) {
    if (!length(rows)) return(empty)
    result <- do.call(rbind, unname(rows))
    rownames(result) <- NULL
    tibble::as_tibble(result)
  }
  list(
    aggregate_coefficients = bind_rows(aggregate_rows, empty_summary()),
    facet_comparisons = bind_rows(comparison_rows, empty_summary(TRUE)),
    validity = validity,
    interpretation_allowed = isTRUE(validity$fit_valid) &&
      validity$gate_status %in% c("ok", "retried_ok"),
    metadata = list(
      score_definition = "Arithmetic mean of the three raw ASC facet scores after preprocessing",
      facet_weights = stats::setNames(rep(1 / 3, 3), names(facets)),
      facet_sd = facet_sd[names(facets)], aggregate_sd = aggregate_sd,
      interval_level = interval_level,
      coefficient_units = "Aggregate-score SD per predictor SD",
      comparison_units = "Outcome-score SD per predictor SD",
      signed_difference_definition = "b_facet - b_aggregate",
      attenuation_definition = "abs(b_facet) - abs(b_aggregate)",
      sample_alignment_verified = FALSE),
    draws = list(
      aggregate = bind_rows(aggregate_draws, tibble::tibble(
        .draw = integer(), motive = character(), predictor_key = character(),
        b_aggregate = numeric(), gamma_aggregate = numeric())),
      facets = bind_rows(comparison_draws, tibble::tibble(
        .draw = integer(), motive = character(), predictor_key = character(),
        facet = character(), outcome_key = character(), b_facet = numeric(), b_aggregate = numeric(),
        gamma_facet = numeric(), gamma_aggregate = numeric(),
        signed_difference = numeric(), attenuation = numeric())))
  )
}
