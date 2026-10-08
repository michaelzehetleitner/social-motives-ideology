# AP9 wiring for the S1 records: the solutions the EFA records show and the
# SRQ1 evidence of one item set, read from the accepted results.

# ---- SRQ1 EFA: the solutions the S1 records show ---------------------------

#' The theoretical count of one item set, from the typed factor-number table
#'
#' @param efa_factor_numbers Tibble from `add_theoretical_factor_numbers()`.
#' @param item_set Accepted item-set name.
#' @return Integer scalar.
zm_efa_theoretical_count <- function(efa_factor_numbers, item_set) {
  rows <- efa_factor_numbers[
    efa_factor_numbers$item_set == item_set &
      as.character(efa_factor_numbers$type) == "theoretical", , drop = FALSE]
  if (nrow(rows) != 1L) {
    stop("The factor-number table carries ", nrow(rows),
         " theoretical rows for item set '", item_set, "'; exactly one is required.")
  }
  as.integer(rows$n_factors[[1]])
}

#' The primary rotation of a solution at one factor count
#'
#' `analysis_plan$factor_analysis$rotation_primary`, except that a one-factor solution is
#' the unrotated one.
#'
#' @param n_factors The factor count.
#' @param analysis_plan Analysis configuration.
#' @return Single rotation name.
zm_efa_solution_rotation <- function(n_factors, analysis_plan) {
  if (as.integer(n_factors) == 1L) "none" else as.character(analysis_plan$factor_analysis$rotation_primary)
}

#' The fitted solutions of one item set at one count in its primary rotation
#'
#' @param efa_fits List from `fit_efa_models()`.
#' @param item_set Accepted item-set name.
#' @param n_factors The factor count.
#' @param analysis_plan Analysis configuration.
#' @return List of the matching packed solutions; normally of length one.
zm_efa_solutions_at_count <- function(efa_fits, item_set, n_factors, analysis_plan) {
  rotation <- zm_efa_solution_rotation(n_factors, analysis_plan)
  Filter(function(solution) {
    identical(solution$set_name, item_set) &&
      identical(solution$factors, as.integer(n_factors)) &&
      identical(solution$rotation, rotation)
  }, efa_fits)
}

#' The theoretical-count primary solution of one item set
#'
#' The primary rotation is `analysis_plan$factor_analysis$rotation_primary`; the
#' one-factor solution is the unrotated one.
#'
#' @param efa_fits List from `fit_efa_models()`.
#' @param item_set Accepted item-set name.
#' @param n_factors The theoretical count.
#' @param analysis_plan Analysis configuration.
#' @return One packed solution.
zm_efa_primary_solution <- function(efa_fits, item_set, n_factors, analysis_plan) {
  selected <- zm_efa_solutions_at_count(efa_fits, item_set, n_factors, analysis_plan)
  if (length(selected) != 1L) {
    stop("The fitted solutions carry ", length(selected), " records for item set '", item_set,
         "' at ", n_factors, " factor(s) in the ",
         zm_efa_solution_rotation(n_factors, analysis_plan), " rotation; exactly one is required.")
  }
  selected[[1]]
}

#' The complete accepted SRQ1 evidence of one item set
#'
#' Every fitted count and rotation, every factor-number method result, the
#' typed factor-number rows and the explained
#' variance of this item set. The participants' item values are not repeated
#' here; they remain the input of the accepted producers.
#'
#' @param item_set Accepted item-set name.
#' @param efa_fits,efa_factor_numbers,efa_explained_variance
#'   The accepted SRQ1 results.
#' @return List of the retained evidence.
zm_efa_srq1_evidence <- function(item_set, efa_fits, efa_factor_numbers,
                                 efa_explained_variance) {
  solutions <- Filter(function(solution) identical(solution$set_name, item_set), efa_fits)
  solutions <- lapply(solutions, function(solution) {
    solution$set$data <- NULL
    solution
  })
  list(
    item_set = item_set,
    solutions = solutions,
    factor_numbers = efa_factor_numbers[efa_factor_numbers$item_set == item_set, , drop = FALSE],
    factor_number_results = attr(efa_factor_numbers, "factor_number_results")[[item_set]],
    method_counts = attr(efa_factor_numbers, "method_counts_by_item_set")[[item_set]],
    explained_variance = efa_explained_variance[
      efa_explained_variance$set_name == item_set, , drop = FALSE]
  )
}
