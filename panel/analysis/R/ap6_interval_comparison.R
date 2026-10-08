# AP6 highest-density intervals of the coefficients -------------------------
#
# The equal-tailed interval is the primary interval everywhere. This module
# adds the highest-density interval of every population-level coefficient to
# the coefficient summaries; it never refits a model.

#' Highest-density interval of every population-level coefficient of one fit
#'
#' One row per coefficient with `variable`, `hdi_lo` and `hdi_hi`, on the same
#' selected draws the equal-tailed summary uses; the residual scale is omitted.
#' The coefficient identity is retained. This interval does not determine
#' predictions. The mechanism is `HDInterval::hdi()`.
#'
#' @param draws Coefficient draws from [ap6_select_regression_coefficient_draws()].
#' @param interval_level Credible mass from `analysis_plan$regression$ci_level`.
#' @return Tibble `variable`, `hdi_lo`, `hdi_hi`.
ap6_highest_density_coefficient_intervals <- function(draws, interval_level) {
  variables <- setdiff(posterior::variables(draws), "sigma")
  if (length(variables) == 0L) {
    stop("AP6 highest-density intervals need at least one population-level coefficient.")
  }
  matrix <- posterior::as_draws_matrix(posterior::subset_draws(draws, variable = variables))
  bounds <- vapply(variables, function(variable) {
    values <- as.numeric(matrix[, variable])
    values <- values[is.finite(values)]
    if (length(values) < 2L) stop("AP6 highest-density intervals need at least two finite draws.")
    unname(HDInterval::hdi(values, credMass = interval_level))
  }, numeric(2L))
  tibble::tibble(
    variable = variables,
    hdi_lo = as.numeric(bounds[1L, ]),
    hdi_hi = as.numeric(bounds[2L, ])
  )
}
