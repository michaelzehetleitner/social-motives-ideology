# Report targets of the Results section "Measurement".
#
# Each function builds one report target (_targets.R, REPORTING · RESULTS ·
# MEASUREMENT). The report words and formats what these return.

#' Internal consistency of every scale, as the reliability table prints it
#'
#' Both coefficients, the coefficient and interval the AP4 fallback rule
#' reported, the reason for that interval, and alpha's own interval.
#'
#' @param scale_reliability List from [apply_reliability_fallback()].
#' @return Tibble with one row per scale.
report_reliability_by_scale <- function(scale_reliability) {
  reported <- scale_reliability$reported
  alpha_intervals <- scale_reliability$bootstrap[reported$scale_key]
  tibble::tibble(
    scale_key = reported$scale_key,
    n_items = reported$n_items,
    n = reported$n,
    omega = reported$omega_t,
    alpha = reported$alpha,
    reported_coefficient = reported$reported_method,
    reported_estimate = reported$estimate,
    reported_interval_lower = reported$interval_lower,
    reported_interval_upper = reported$interval_upper,
    reported_interval_source = reported$interval_source,
    alpha_interval_lower = vapply(alpha_intervals, function(b) b$alpha_interval[1], numeric(1), USE.NAMES = FALSE),
    alpha_interval_upper = vapply(alpha_intervals, function(b) b$alpha_interval[2], numeric(1), USE.NAMES = FALSE)
  )
}

#' The counts the Measurement prose states
#'
#' How many scales report alpha instead of omega, how many report omega without
#' an interval, and the ranges of both coefficients.
#'
#' @param report_reliability_table Tibble from [report_reliability_by_scale()].
#' @return List of counts and ranges.
report_count_reliability_substitutions <- function(report_reliability_table) {
  list(
    n_scales = nrow(report_reliability_table),
    n_alpha_reported = sum(report_reliability_table$reported_coefficient == "alpha"),
    n_omega_without_interval = sum(
      report_reliability_table$reported_coefficient == "omega" &
        report_reliability_table$reported_interval_source == "unavailable_omega_bootstrap"
    ),
    omega_range = range(report_reliability_table$omega, na.rm = TRUE),
    alpha_range = range(report_reliability_table$alpha, na.rm = TRUE)
  )
}
