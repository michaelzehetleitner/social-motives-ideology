# The Gaussian-Student pairs of the target regression_likelihood_robustness,
# built from two small coefficient tables (`outcome`, `term`, `estimate`,
# `q2.5`, `q97.5`, `fit_valid`, `gate_status`). A NULL or zero-row Student table
# is the untriggered refit: its side of every pair is empty.
likelihood_pairs <- function(gaussian, student = NULL) {
  side <- function(x, suffix) {
    out <- tibble::tibble(
      outcome = as.character(x$outcome), term = as.character(x$term),
      estimate = as.numeric(x$estimate), q_lo = as.numeric(x$q2.5), q_hi = as.numeric(x$q97.5),
      outcome_key = as.character(x$outcome), fit_available = rep(TRUE, nrow(x)),
      fit_valid = as.logical(x$fit_valid), gate_status = as.character(x$gate_status)
    )
    names(out)[-(1:2)] <- paste0(names(out)[-(1:2)], "_", suffix)
    out
  }
  empty <- gaussian[0, c("outcome", "term", "estimate", "q2.5", "q97.5", "fit_valid", "gate_status")]
  student <- if (is.null(student) || nrow(student) == 0L) empty else student
  list(coefficients = dplyr::full_join(side(gaussian, "gaussian"), side(student, "student"),
                                       by = c("outcome", "term")))
}
