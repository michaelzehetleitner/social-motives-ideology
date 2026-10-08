# Report target of the supplement section S7 "Explained variance".
#
# Builds the one target (_targets.R, SUPPLEMENT · S7) the section and its table
# read. The report words and formats what this returns.

#' S7 explained variance
#'
#' The Bayesian R-squared of the four primary fits, and the facts of the answer
#' the RQ1 section prints: which model explained the least and which the most
#' variance, and for which it was unavailable.
#'
#' @param regression_r2_summaries The accepted R-squared summaries (target of the same name).
#' @param analysis_plan Configuration from [zm_config()].
#' @return List `r2_primary` (the primary rows), `least`, `most` and `missing` (see
#'   [check_explained_variance()]).
assemble_supplement_explained_variance <- function(regression_r2_summaries, analysis_plan) {
  r2 <- tabulate_explained_variance(regression_r2_summaries, analysis_plan)
  c(list(r2_primary = r2[r2$role %in% "primary", , drop = FALSE]),
    check_explained_variance(r2, as.character(analysis_plan$regression$outcomes)))
}

#' Which primary model explained the least and which the most variance
#'
#' @param r2 R-squared rows with `outcome`, `role` and `r2_median`.
#' @param outcomes The configured outcomes.
#' @return List `least`, `most` (outcomes, `NA` when no primary R-squared is
#'   available) and `missing` (outcomes without one).
check_explained_variance <- function(r2, outcomes) {
  available <- r2[r2$role %in% "primary" & is.finite(r2$r2_median), , drop = FALSE]
  list(
    least = if (nrow(available) == 0L) NA_character_ else available$outcome[which.min(available$r2_median)],
    most = if (nrow(available) == 0L) NA_character_ else available$outcome[which.max(available$r2_median)],
    missing = setdiff(outcomes, as.character(available$outcome))
  )
}

# ---- the tables of the section, built from the accepted results ----------

#' Bayesian R2 rows of the Gaussian primary and sweep fits
#'
#' @param r2_summaries `regression_r2_summaries`.
#' @param analysis_plan Analysis configuration.
#' @return Tibble `outcome`, `slope_sd`, `role`, `label`, `family`,
#'   `r2_median`, `r2_lo`, `r2_hi`, `r2_definition`.
tabulate_explained_variance <- function(r2_summaries, analysis_plan) {
  rows <- tibble::as_tibble(r2_summaries)
  rows <- rows[rows$role %in% c("primary", "sweep") &
                 rows$family == analysis_plan$regression$family, , drop = FALSE]
  tibble::tibble(
    outcome = as.character(rows$outcome_key),
    slope_sd = as.numeric(rows$slope_sd),
    role = as.character(rows$role),
    label = as.character(rows$label),
    family = as.character(rows$family),
    r2_median = as.numeric(zm_regression_column(rows, "r2_median", NA_real_)),
    r2_lo = as.numeric(zm_regression_column(rows, "r2_lo", NA_real_)),
    r2_hi = as.numeric(zm_regression_column(rows, "r2_hi", NA_real_)),
    r2_definition = zm_regression_r2_definition(rows$family, rows$nu_fixed)
  )
}
