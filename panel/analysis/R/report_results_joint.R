# Report the approved RQ2 comparisons; their target is named report_joint.
# The two facet packages supply twenty signed coefficients and thirty paired
# absolute-strength differences. The correlation package adds ASC total–SDO-D
# to the six facet residual correlations. `analysis_plan` supplies the motives
# in the codebook's order (zm_config()).
assemble_report_joint <- function(joint_within_asc, joint_asc_components_sdo, joint_correlation_changes, analysis_plan) {
  # The packages name each motive by its key; the report labels it from the
  # codebook and lists the motives in the plan's (the codebook's) order.
  motives <- as.character(analysis_plan$regression$motives)
  in_motive_order <- function(x) {
    x$motive_key <- as.character(x$motive)
    x[order(match(x$motive_key, motives)), , drop = FALSE]
  }
  direction <- dplyr::bind_rows(joint_within_asc$direction, joint_asc_components_sdo$direction)
  direction <- in_motive_order(direction[!duplicated(direction[c("motive", "outcome")]), , drop = FALSE])
  differences <- in_motive_order(dplyr::bind_rows(
    joint_within_asc$differences, joint_asc_components_sdo$differences
  ))
  within_residuals <- tibble::as_tibble(joint_within_asc$residuals)
  component_residuals <- tibble::as_tibble(joint_asc_components_sdo$residuals)
  total <- joint_correlation_changes$summaries
  total <- total[total$outcome_1 == "asc_total" & total$outcome_2 == "sdo_dom", , drop = FALSE]
  total_residual <- tibble::tibble(pair = "asc_total_sdo_d",
    posterior_median = total$residual_median, lower = total$residual_lower,
    upper = total$residual_upper)
  residuals <- dplyr::bind_rows(within_residuals, component_residuals, total_residual)
  credible_sign <- function(classification) {
    unname(dplyr::case_when(
      grepl("positive", classification) ~ "positive",
      grepl("negative", classification) ~ "negative",
      TRUE ~ NA_character_
    ))
  }
  credible_direction <- direction[grepl("^credible ", direction$classification), , drop = FALSE]
  credible_difference <- differences[grepl("^credibly (stronger|weaker) ", differences$classification), , drop = FALSE]
  validity <- joint_within_asc$validity
  correlation_valid <- isTRUE(joint_correlation_changes$validity$fit_valid) &&
    nrow(joint_correlation_changes$summaries) == 7L &&
    all(joint_correlation_changes$summaries$interpretable %in% TRUE)
  gate_passed <- isTRUE(validity$fit_valid) &&
    isTRUE(joint_asc_components_sdo$validity$fit_valid) && correlation_valid
  invalid_inputs <- Filter(function(x) !isTRUE(x$fit_valid),
    list(validity, joint_asc_components_sdo$validity, joint_correlation_changes$validity))
  gate_status <- if (length(invalid_inputs)) {
    as.character(invalid_inputs[[1L]]$gate_status)
  } else if (gate_passed) as.character(validity$gate_status) else "joint_results_invalid"
  if (!gate_passed) {
    direction$classification <- NA_character_
    differences$classification <- NA_character_
    differences$direction_comparison <- NA_character_
    credible_direction <- credible_direction[FALSE, , drop = FALSE]
    credible_difference <- credible_difference[FALSE, , drop = FALSE]
  }
  list(
    direction = direction,
    differences = differences,
    residuals = residuals,
    gate_passed = gate_passed,
    gate_status = gate_status,
    credible_directions = tibble::tibble(
      motive_key = credible_direction$motive_key, outcome = credible_direction$outcome,
      sign = credible_sign(credible_direction$classification)
    ),
    credible_differences = tibble::tibble(
      motive_key = credible_difference$motive_key, contrast = credible_difference$contrast,
      stronger = as.character(ifelse(grepl("^credibly stronger ", credible_difference$classification),
                                     "first", "second"))
    ),
    all_residuals_positive = nrow(residuals) > 0L && isTRUE(all(residuals$lower > 0, na.rm = FALSE))
  )
}
