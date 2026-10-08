# Participation-percentage reporting slot -----------------------------------

#' Save the participation numerator, denominator and proportion when known
calculate_participation_reporting_data <- function(n_started, analysis_plan) {
  invited <- analysis_plan$recruitment$invited_n
  if (is.null(invited) || length(invited) == 0L || is.na(invited)) {
    return(list(available = FALSE))
  }
  invited <- as.numeric(invited)
  n_started <- as.numeric(n_started)
  if (length(invited) != 1L || !is.finite(invited) || invited <= 0 ||
      length(n_started) != 1L || !is.finite(n_started) || n_started < 0 ||
      n_started > invited) {
    stop("Participation reporting needs 0 <= recorded responses <= invited respondents.")
  }
  list(available = TRUE, n_started = n_started, invited = invited,
       proportion = n_started / invited)
}

#' Participation-percentage sentence
#'
#' @param n_started Number of responses recorded, everyone who started the survey.
#' @param analysis_plan Configuration carrying `recruitment$invited_n`.
#' @param participation Saved facts from [calculate_participation_reporting_data()], or NULL.
#' @return The participation-percentage sentence. Without the number of
#'   invitations the report prints the preregistration's sentence (M4) instead
#'   of calling this function.
rh_participation_text <- function(n_started, analysis_plan, participation = NULL) {
  if (is.null(participation)) participation <- calculate_participation_reporting_data(n_started, analysis_plan)
  if (!participation$available) {
    stop("Participation reporting needs the number of invited panel members (recruitment$invited_n).")
  }
  invited <- participation$invited
  n_started <- participation$n_started
  paste0(
    "The participation percentage was ", rh_fmt_pct(participation$proportion, 1, scale = "proportion"),
    " (", rh_fmt_n(n_started), " recorded responses from ",
    rh_fmt_n(invited), " invitations)."
  )
}
