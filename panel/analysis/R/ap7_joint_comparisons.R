# AP7 / RQ2 joint-model summaries
#
# These summaries never decide Table 3: the four separate primary regressions
# of AP6 alone carry the preregistered predictions and their verdicts.
# A joint fit that fails the AP6 validity gate receives no classification; its
# posterior medians and intervals remain visible, withheld of every verdict.

#' AP7 / RQ2 — the joint coefficient and residual draws
#'
#' Extract the five registered motive coefficients for aggression, submission,
#' conventionalism, and SDO-D plus the six residual correlations from the valid
#' joint posterior. Keep posterior draw identities paired for every later mean
#' and contrast. A motive is named by its scale key (`zm_security`); the motives
#' are those of the plan, in its (the codebook's) order, and the report labels
#' them from the codebook.
#'
#' @param joint_regression_fit The gated collection `list(joint = <fit>)` of
#'   the `joint_regression_fit` target.
#' @param analysis_plan Configuration from [zm_config()]: the motives
#'   (`regression$motives`) and their standardised columns (`regression$term_keys`).
#' @return `list(coefficients, residual_correlations, validity)`.
extract_joint_posterior <- function(joint_regression_fit, analysis_plan) {
  # A missing or failed joint fit yields the same tables with no rows: every
  # consumer keeps its columns, and the withheld validity travels with them.
  if (!inherits(joint_regression_fit$joint, "brmsfit")) {
    return(list(
      coefficients = tibble::tibble(
        .draw = integer(0), motive = character(0), aggression = numeric(0),
        submission = numeric(0), conventionalism = numeric(0), sdo_d = numeric(0)
      ),
      residual_correlations = tibble::tibble(
        .draw = integer(0), aggression_submission = numeric(0),
        aggression_conventionalism = numeric(0), submission_conventionalism = numeric(0),
        aggression_sdo_d = numeric(0), submission_sdo_d = numeric(0),
        conventionalism_sdo_d = numeric(0)
      ),
      validity = ap8_extract_joint_fit_validity(joint_regression_fit$joint)
    ))
  }
  motive_columns <- ap8_joint_motive_columns(analysis_plan)
  response_ids <- c(
    aggression = "ascaggz",
    submission = "ascsubz",
    conventionalism = "ascconvz",
    sdo_d = "sdodomz"
  )
  fit <- joint_regression_fit$joint
  draws <- posterior::as_draws_df(fit)
  coefficients <- ap8_shape_joint_coefficient_draws(
    draws, response_ids, motive_columns
  )
  residual_correlations <- ap8_shape_joint_residual_draws(draws, response_ids)
  validity <- ap8_extract_joint_fit_validity(fit)
  list(
    coefficients = coefficients,
    residual_correlations = residual_correlations,
    validity = validity
  )
}

# BEGIN GENERATED PARAMETER CARD: AP7 INTERVAL LEVEL
# Automatically generated from analysis_plan.yaml
# Every ap8_result_* verb below receives this level as its interval_level argument.
#   Interval level: 0.95   (regression.ci_level)
#   Central, equal-tailed: quantiles 0.025 and 0.975
# END GENERATED PARAMETER CARD: AP7 INTERVAL LEVEL
#' AP7 / RQ2 — directions of the mean ASC and SDO-D coefficients
#'
#' Overall ASC versus SDO-D, direction evidence: for each
#' motive, report the direction of the draw-wise equal-weight mean ASC
#' coefficient and the SDO-D coefficient using posterior medians and central
#' configured-level equal-tailed intervals.
#'
#' @param coefficients The paired coefficient draws of [extract_joint_posterior()].
#' @param interval_level Central interval level (`analysis_plan$regression$ci_level`).
#' @return One row per motive and outcome.
ap8_result_overall_asc_sdo_directions <- function(coefficients, interval_level) {
  probabilities <- c((1 - interval_level) / 2, 1 - (1 - interval_level) / 2)
  coefficients |>
    dplyr::mutate(
      mean_asc = (aggression + submission + conventionalism) / 3
    ) |>
    tidyr::pivot_longer(
      cols = c(mean_asc, sdo_d), names_to = "outcome", values_to = "estimate"
    ) |>
    dplyr::group_by(motive, outcome) |>
    dplyr::summarise(
      posterior_median = stats::median(estimate),
      lower = stats::quantile(estimate, probs = probabilities[[1]]),
      upper = stats::quantile(estimate, probs = probabilities[[2]]),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      classification = dplyr::case_when(
        lower > 0 ~ "credible positive association",
        upper < 0 ~ "credible negative association",
        TRUE ~ "unresolved in direction"
      )
    )
}

#' AP7 / RQ2 — strength of SDO-D against the draw-wise mean ASC coefficient
#'
#' Overall ASC versus SDO-D, difference evidence, in the form
#' the preregistration's RQ2a states: within every paired draw, the
#' absolute SDO-D coefficient minus the absolute equal-weight mean ASC
#' coefficient, the three signed ASC coefficients being averaged before the
#' absolute value is taken. The subtraction order is AP7's (`SDO-D − mean
#' ASC`); a positive difference means a stronger association with SDO-D. The
#' signs of the two coefficients are compared separately
#' ([ap8_summarise_strength_differences()]).
#'
#' @param coefficients The paired coefficient draws of [extract_joint_posterior()].
#' @param interval_level Central interval level (`analysis_plan$regression$ci_level`).
#' @return One row per motive and contrast; see
#'   [ap8_summarise_strength_differences()].
ap8_result_overall_asc_sdo_coefficient_differences <- function(coefficients, interval_level) {
  coefficients |>
    dplyr::mutate(
      mean_asc = (aggression + submission + conventionalism) / 3
    ) |>
    ap8_summarise_strength_differences(
      pairs = list("|SDO-D| - |mean ASC|" = c("sdo_d", "mean_asc")),
      interval_level = interval_level
    )
}

#' AP7 / RQ2 — the within-ASC minus ASC-SDO-D residual-correlation difference
#'
#' Overall ASC versus SDO-D, residual-relationship
#' evidence: within every draw, subtract the mean of the three ASC–SDO-D
#' residual correlations from the mean of the three within-ASC residual
#' correlations. Report its posterior median, central configured-level
#' equal-tailed interval, and ordering, and beside it the two averages
#' themselves, summarised the same way from the same draws, which the
#' preregistration's RQ2b answer reads together with the difference.
#'
#' @param residual_correlations The paired residual draws of [extract_joint_posterior()].
#' @param interval_level Central interval level (`analysis_plan$regression$ci_level`).
#' @return One row: the difference (`posterior_median`, `lower`, `upper`,
#'   `classification`), then the within-ASC average (`within_median`,
#'   `within_lower`, `within_upper`) and the ASC–SDO-D average
#'   (`between_median`, `between_lower`, `between_upper`).
ap8_result_overall_asc_sdo_residual_difference <- function(residual_correlations, interval_level) {
  probabilities <- c((1 - interval_level) / 2, 1 - (1 - interval_level) / 2)
  residual_correlations |>
    dplyr::transmute(
      .draw,
      within = (aggression_submission + aggression_conventionalism + submission_conventionalism) / 3,
      between = (aggression_sdo_d + submission_sdo_d + conventionalism_sdo_d) / 3,
      difference = within - between
    ) |>
    dplyr::summarise(
      posterior_median = stats::median(difference),
      lower = stats::quantile(difference, probs = probabilities[[1]]),
      upper = stats::quantile(difference, probs = probabilities[[2]]),
      within_median = stats::median(within),
      within_lower = stats::quantile(within, probs = probabilities[[1]]),
      within_upper = stats::quantile(within, probs = probabilities[[2]]),
      between_median = stats::median(between),
      between_lower = stats::quantile(between, probs = probabilities[[1]]),
      between_upper = stats::quantile(between, probs = probabilities[[2]])
    ) |>
    dplyr::mutate(
      classification = dplyr::case_when(
        lower > 0 ~ "greater average within ASC",
        upper < 0 ~ "greater average ASC-SDO-D",
        TRUE ~ "unresolved ordering"
      ),
      .after = upper
    )
}

#' AP7 / RQ2 — directions of the three ASC component coefficients
#'
#' Within ASC, direction evidence: for each motive, report
#' the aggression, submission, and conventionalism coefficient directions using
#' posterior medians and central configured-level equal-tailed intervals.
#'
#' @param coefficients The paired coefficient draws of [extract_joint_posterior()].
#' @param interval_level Central interval level (`analysis_plan$regression$ci_level`).
#' @return One row per motive and outcome.
extract_joint_within_asc_directions <- function(coefficients, interval_level) {
  probabilities <- c((1 - interval_level) / 2, 1 - (1 - interval_level) / 2)
  coefficients |>
    tidyr::pivot_longer(
      cols = c(aggression, submission, conventionalism),
      names_to = "outcome", values_to = "estimate"
    ) |>
    dplyr::group_by(motive, outcome) |>
    dplyr::summarise(
      posterior_median = stats::median(estimate),
      lower = stats::quantile(estimate, probs = probabilities[[1]]),
      upper = stats::quantile(estimate, probs = probabilities[[2]]),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      classification = dplyr::case_when(
        lower > 0 ~ "credible positive association",
        upper < 0 ~ "credible negative association",
        TRUE ~ "unresolved in direction"
      )
    )
}

#' AP7 / RQ2 — the three within-ASC strength differences
#'
#' Within ASC, difference evidence, in the form the
#' preregistration's RQ2a states: for each motive, the absolute
#' aggression coefficient minus the absolute submission coefficient, absolute
#' aggression minus absolute conventionalism, and absolute submission minus
#' absolute conventionalism within every posterior draw. A positive difference
#' means a stronger association with the first-named facet; the signs are
#' compared separately ([ap8_summarise_strength_differences()]).
#'
#' @param coefficients The paired coefficient draws of [extract_joint_posterior()].
#' @param interval_level Central interval level (`analysis_plan$regression$ci_level`).
#' @return One row per motive and contrast; see
#'   [ap8_summarise_strength_differences()].
extract_joint_within_asc_coefficient_differences <- function(coefficients, interval_level) {
  ap8_summarise_strength_differences(
    coefficients,
    pairs = list(
      "|Aggression| - |submission|" = c("aggression", "submission"),
      "|Aggression| - |conventionalism|" = c("aggression", "conventionalism"),
      "|Submission| - |conventionalism|" = c("submission", "conventionalism")
    ),
    interval_level = interval_level
  )
}

#' AP7 / RQ2 — the three within-ASC residual correlations
#'
#' Within ASC, residual-relationship evidence: report the
#' three within-ASC residual correlations descriptively with posterior medians
#' and central configured-level equal-tailed intervals.
#'
#' @param residual_correlations The paired residual draws of [extract_joint_posterior()].
#' @param interval_level Central interval level (`analysis_plan$regression$ci_level`).
#' @return One row per response pair.
extract_joint_within_asc_residual_correlations <- function(residual_correlations, interval_level) {
  probabilities <- c((1 - interval_level) / 2, 1 - (1 - interval_level) / 2)
  residual_correlations |>
    tidyr::pivot_longer(
      cols = c(
        aggression_submission, aggression_conventionalism,
        submission_conventionalism
      ),
      names_to = "pair", values_to = "correlation"
    ) |>
    dplyr::group_by(pair) |>
    dplyr::summarise(
      posterior_median = stats::median(correlation),
      lower = stats::quantile(correlation, probs = probabilities[[1]]),
      upper = stats::quantile(correlation, probs = probabilities[[2]]),
      .groups = "drop"
    )
}

#' AP7 / RQ2 — directions of the four component and SDO-D coefficients
#'
#' Each ASC component versus SDO-D, direction evidence:
#' for each motive, report the aggression, submission, conventionalism, and
#' SDO-D coefficient directions using posterior medians and central
#' configured-level equal-tailed intervals.
#'
#' @param coefficients The paired coefficient draws of [extract_joint_posterior()].
#' @param interval_level Central interval level (`analysis_plan$regression$ci_level`).
#' @return One row per motive and outcome.
extract_joint_asc_components_sdo_directions <- function(coefficients, interval_level) {
  probabilities <- c((1 - interval_level) / 2, 1 - (1 - interval_level) / 2)
  coefficients |>
    tidyr::pivot_longer(
      cols = c(aggression, submission, conventionalism, sdo_d),
      names_to = "outcome", values_to = "estimate"
    ) |>
    dplyr::group_by(motive, outcome) |>
    dplyr::summarise(
      posterior_median = stats::median(estimate),
      lower = stats::quantile(estimate, probs = probabilities[[1]]),
      upper = stats::quantile(estimate, probs = probabilities[[2]]),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      classification = dplyr::case_when(
        lower > 0 ~ "credible positive association",
        upper < 0 ~ "credible negative association",
        TRUE ~ "unresolved in direction"
      )
    )
}

#' AP7 / RQ2 — each ASC component's strength against SDO-D
#'
#' Each ASC component versus SDO-D, difference evidence, in
#' the form the preregistration's RQ2a states: for each motive, the
#' absolute aggression, submission and conventionalism coefficient minus the
#' absolute SDO-D coefficient within every posterior draw. A positive
#' difference means a stronger association with the ASC component; the signs
#' are compared separately ([ap8_summarise_strength_differences()]).
#'
#' @param coefficients The paired coefficient draws of [extract_joint_posterior()].
#' @param interval_level Central interval level (`analysis_plan$regression$ci_level`).
#' @return One row per motive and contrast; see
#'   [ap8_summarise_strength_differences()].
extract_joint_asc_components_sdo_coefficient_differences <- function(coefficients, interval_level) {
  ap8_summarise_strength_differences(
    coefficients,
    pairs = list(
      "|Aggression| - |SDO-D|" = c("aggression", "sdo_d"),
      "|Submission| - |SDO-D|" = c("submission", "sdo_d"),
      "|Conventionalism| - |SDO-D|" = c("conventionalism", "sdo_d")
    ),
    interval_level = interval_level
  )
}

#' AP7 / RQ2 — each ASC component's residual correlation with SDO-D
#'
#' Each ASC component versus SDO-D, residual-relationship
#' evidence: report each ASC component's residual correlation with SDO-D
#' descriptively with its posterior median and central configured-level
#' equal-tailed interval.
#'
#' @param residual_correlations The paired residual draws of [extract_joint_posterior()].
#' @param interval_level Central interval level (`analysis_plan$regression$ci_level`).
#' @return One row per response pair.
extract_joint_asc_components_sdo_residual_correlations <- function(residual_correlations, interval_level) {
  probabilities <- c((1 - interval_level) / 2, 1 - (1 - interval_level) / 2)
  residual_correlations |>
    tidyr::pivot_longer(
      cols = c(aggression_sdo_d, submission_sdo_d, conventionalism_sdo_d),
      names_to = "pair", values_to = "correlation"
    ) |>
    dplyr::group_by(pair) |>
    dplyr::summarise(
      posterior_median = stats::median(correlation),
      lower = stats::quantile(correlation, probs = probabilities[[1]]),
      upper = stats::quantile(correlation, probs = probabilities[[2]]),
      .groups = "drop"
    )
}

#' AP7 / RQ2 — withhold every classification while the joint fit is not valid
#'
#' A joint fit that has not passed the AP6 validity gate carries no
#' interpretation: every classification of the package — the interval
#' classification and the comparison of two coefficients' signs — becomes
#' unavailable while its posterior medians and intervals stay visible. The
#' descriptive residual tables carry no classification and are unchanged. The
#' validity record travels with the package either way.
#'
#' @param summaries The `list(direction, differences, residuals)` of one RQ2
#'   comparison package.
#' @param validity The `validity` record of [extract_joint_posterior()].
#' @return The package plus its `validity` element.
withhold_classification_when_fit_invalid <- function(summaries, validity) {
  if (isTRUE(validity$fit_valid)) {
    return(c(summaries, list(validity = validity)))
  }
  withheld <- lapply(summaries, function(summary) {
    if (is.data.frame(summary)) {
      for (column in intersect(c("classification", "direction_comparison"), names(summary))) {
        summary[[column]] <- NA_character_
      }
    }
    summary
  })
  c(withheld, list(validity = validity))
}

# ---- AP7 — technical helpers of the joint-model summaries -------------------

#' Strength differences of paired coefficients and the comparison of their signs
#'
#' Serves the three `ap8_result_*_coefficient_differences()` verbs. For each
#' named pair of coefficient columns it forms, within every paired draw, the
#' absolute first coefficient minus the absolute second one, and summarises it
#' per motive by its posterior median and central interval. The preregistration
#' (AP7, Interpretation) classifies that interval: wholly above zero, a
#' credibly stronger association with the first-named outcome; wholly below
#' zero, a credibly weaker one; including zero, an unresolved strength
#' difference. Separately, it compares the two coefficients' own interval
#' classifications: matching credible signs are the same direction, one
#' credibly positive and one credibly negative coefficient a sign flip, and
#' anything else leaves the direction comparison unresolved. A sign flip can
#' occur without a strength difference and a strength difference without a
#' sign flip, so neither is read from the other.
#'
#' @param coefficients Paired coefficient draws, one row per draw and motive,
#'   carrying `.draw`, `motive` and every column the pairs name.
#' @param pairs Named list; each name is the contrast label stating the
#'   subtraction order, each element the two column names (first, second).
#' @param interval_level Central interval level (`analysis_plan$regression$ci_level`).
#' @return Tibble `motive`, `contrast`, `first_outcome`, `second_outcome`,
#'   `posterior_median`, `lower`, `upper` (of the strength difference),
#'   `classification` and `direction_comparison`; one row per motive and pair.
ap8_summarise_strength_differences <- function(coefficients, pairs, interval_level) {
  probabilities <- c((1 - interval_level) / 2, 1 - (1 - interval_level) / 2)
  # The direction rule of AP7 applied to one coefficient's draws.
  sign_of <- function(x) {
    if (length(x) == 0L) return(NA_character_)
    bounds <- stats::quantile(x, probs = probabilities, names = FALSE)
    if (bounds[[1]] > 0) "positive" else if (bounds[[2]] < 0) "negative" else "unresolved"
  }
  rows <- lapply(names(pairs), function(label) {
    first <- pairs[[label]][[1]]
    second <- pairs[[label]][[2]]
    coefficients |>
      dplyr::transmute(
        .draw, motive,
        first_value = .data[[first]], second_value = .data[[second]],
        difference = abs(first_value) - abs(second_value)
      ) |>
      dplyr::group_by(motive) |>
      dplyr::summarise(
        contrast = label,
        first_outcome = first,
        second_outcome = second,
        posterior_median = stats::median(difference),
        lower = stats::quantile(difference, probs = probabilities[[1]]),
        upper = stats::quantile(difference, probs = probabilities[[2]]),
        first_sign = sign_of(first_value),
        second_sign = sign_of(second_value),
        .groups = "drop"
      )
  })
  dplyr::bind_rows(c(
    list(tibble::tibble(
      motive = character(), contrast = character(), first_outcome = character(),
      second_outcome = character(), posterior_median = numeric(), lower = numeric(),
      upper = numeric(), first_sign = character(), second_sign = character()
    )),
    rows
  )) |>
    dplyr::arrange(motive, contrast) |>
    dplyr::mutate(
      classification = dplyr::case_when(
        lower > 0 ~ "credibly stronger for the first-named outcome",
        upper < 0 ~ "credibly weaker for the first-named outcome",
        TRUE ~ "unresolved strength difference"
      ),
      direction_comparison = dplyr::case_when(
        first_sign %in% c("positive", "negative") & first_sign == second_sign ~ "same direction",
        first_sign %in% c("positive", "negative") & second_sign %in% c("positive", "negative") ~ "sign flip",
        TRUE ~ "unresolved direction comparison"
      )
    ) |>
    dplyr::select(-first_sign, -second_sign)
}

#' The joint fit's final validity-gate result
#'
#' Serves [extract_joint_posterior()]. It reads the diagnostics and gate
#' status the shared AP6 gate records on the fit and applies exactly the rule
#' [extract_regression_result_records()] applies to every separate fit. It measures
#' nothing and declares no threshold of its own.
#'
#' @param fit The gated joint fit, `NULL`, or the `error` of a failed fit.
#' @return `list(fit_valid, gate_status, note)`.
ap8_extract_joint_fit_validity <- function(fit) {
  if (!inherits(fit, "brmsfit")) {
    return(list(
      fit_valid = FALSE,
      gate_status = "unavailable",
      note = if (inherits(fit, "error")) conditionMessage(fit) else "No joint fit available."
    ))
  }
  diagnostics <- attr(fit, "diagnostics", exact = TRUE)
  gate_status <- attr(fit, "gate_status", exact = TRUE)
  if (is.null(gate_status) || is.null(diagnostics$ok)) {
    return(list(
      fit_valid = NA,
      gate_status = "unavailable",
      note = "Final validity-gate result unavailable."
    ))
  }
  fit_valid <- isTRUE(diagnostics$ok) && gate_status %in% ap7_gate_statuses_ok()
  list(
    fit_valid = fit_valid,
    gate_status = gate_status,
    note = if (fit_valid) NA_character_ else "Fit is not interpretable."
  )
}

#' The motive columns of the joint model, by motive key
#'
#' The motives of the plan in its order, which [zm_config()] has set to the
#' codebook's, each with its standardised predictor column.
#'
#' @param analysis_plan Configuration from [zm_config()].
#' @return Named character vector, motive key -> standardised column
#'   (`c(zm_security = "zm_security_z", ...)`).
ap8_joint_motive_columns <- function(analysis_plan) {
  motives <- as.character(analysis_plan$regression$motives)
  term_keys <- analysis_plan$regression$term_keys
  columns <- names(term_keys)[match(motives, term_keys)]
  if (length(motives) == 0L || anyNA(columns)) {
    stop("analysis_plan$regression$term_keys has no standardised column for the motive(s): ",
         paste(motives[is.na(columns)], collapse = ", "), ".")
  }
  stats::setNames(columns, motives)
}

#' Resolve and shape the joint coefficient draws
#'
#' Serves [extract_joint_posterior()]. It resolves the response names and
#' the canonical motive columns to brms coefficient names, verifies their
#' presence, and shapes one row per paired draw and motive. It does not choose
#' comparisons or summarise posterior values.
#'
#' @param draws The joint posterior as a draws data frame.
#' @param response_ids The four brms response names.
#' @param motive_columns The five registered motive predictor columns, named by
#'   motive key ([ap8_joint_motive_columns()]).
#' @return Tibble `.draw`, `motive` (the motive key), and the four outcome columns.
ap8_shape_joint_coefficient_draws <- function(draws, response_ids, motive_columns) {
  parameters <- vapply(names(motive_columns), function(motive) {
    paste0("b_", response_ids, "_", motive_columns[[motive]])
  }, character(length(response_ids)))
  dimnames(parameters) <- list(names(response_ids), names(motive_columns))
  missing <- setdiff(as.vector(parameters), names(draws))
  if (length(missing)) {
    stop("Joint posterior lacks coefficient parameters: ", paste(missing, collapse = ", "))
  }
  rows <- lapply(names(motive_columns), function(motive) {
    data.frame(
      .draw = draws$.draw,
      motive = motive,
      aggression = draws[[parameters["aggression", motive]]],
      submission = draws[[parameters["submission", motive]]],
      conventionalism = draws[[parameters["conventionalism", motive]]],
      sdo_d = draws[[parameters["sdo_d", motive]]],
      check.names = FALSE
    )
  })
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  tibble::as_tibble(result)
}

#' Resolve and shape the six joint residual-correlation draws
#'
#' Serves [extract_joint_posterior()]. It resolves either brms ordering of
#' each response pair and shapes the six values beside their shared
#' draw identifier. It does not average, contrast, classify, or interpret
#' correlations.
#'
#' @param draws The joint posterior as a draws data frame.
#' @param response_ids The four brms response names.
#' @return Tibble `.draw` and the six response-pair columns.
ap8_shape_joint_residual_draws <- function(draws, response_ids) {
  pairs <- list(
    aggression_submission = c("aggression", "submission"),
    aggression_conventionalism = c("aggression", "conventionalism"),
    submission_conventionalism = c("submission", "conventionalism"),
    aggression_sdo_d = c("aggression", "sdo_d"),
    submission_sdo_d = c("submission", "sdo_d"),
    conventionalism_sdo_d = c("conventionalism", "sdo_d")
  )
  parameters <- vapply(pairs, function(pair) {
    candidates <- c(
      paste0("rescor__", response_ids[[pair[[1]]]], "__", response_ids[[pair[[2]]]]),
      paste0("rescor__", response_ids[[pair[[2]]]], "__", response_ids[[pair[[1]]]])
    )
    present <- candidates[candidates %in% names(draws)]
    if (length(present) != 1L) {
      stop("Joint posterior must contain exactly one ordering of residual pair ",
           pair[[1]], " / ", pair[[2]], ".")
    }
    present
  }, character(1))
  result <- data.frame(.draw = draws$.draw, check.names = FALSE)
  for (pair in names(parameters)) {
    result[[pair]] <- draws[[parameters[[pair]]]]
  }
  tibble::as_tibble(result)
}
