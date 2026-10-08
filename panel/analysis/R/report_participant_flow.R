# Participant-flow display ---------------------------------------------------
#
# Pure presentation code. Counts come from the stored AP1 exclusion log; no
# number is typed into the report or inferred from prose.

#' Participant-flow counts from the sequential AP1 log
#'
#' @param log Tibble of the exclusion steps (`report_exclusion_steps()$steps`).
#' @param n_started Responses recorded: everyone who started the survey.
#' @param n_quota_full Of them, those turned away at a full quota cell.
#' @param analysis_plan Configuration, for the numbers in the criterion labels
#'   ([rh_criterion_label()]); `NULL` names the rules without them.
#' @return List with started, turned away, admitted, excluded, analysed and
#'   labelled reason rows.
rh_participant_flow_data <- function(log, n_started, n_quota_full, analysis_plan = NULL) {
  started <- as.integer(n_started)
  quota_full <- as.integer(n_quota_full)
  admitted <- as.integer(log$n_before[1L])
  analysed <- as.integer(log$n_after[nrow(log)])
  excluded <- as.integer(sum(log$n_excluded))
  if (!identical(started - quota_full, admitted)) {
    stop("Participant-flow counts do not reconcile: started minus quota-full exits differs from admitted.")
  }
  if (!identical(admitted - excluded, analysed)) {
    stop("Participant-flow counts do not reconcile: admitted minus exclusions differs from analysed.")
  }
  reasons <- tibble::tibble(
    criterion = as.character(log$criterion),
    label = rh_criterion_label(log$criterion, analysis_plan),
    n = as.integer(log$n_excluded)
  )
  list(started = started, quota_full = quota_full, admitted = admitted,
       excluded = excluded, analysed = analysed, reasons = reasons)
}

#' Participant-flow figure
#'
#' @inheritParams rh_participant_flow_data
#' @return A ggplot flow figure.
rh_participant_flow_plot <- function(log, n_started, n_quota_full, analysis_plan = NULL) {
  flow <- rh_participant_flow_data(log, n_started, n_quota_full, analysis_plan)
  reasons <- paste0(
    flow$reasons$label, ": ", rh_fmt_n(flow$reasons$n),
    collapse = "\n"
  )
  boxes <- tibble::tibble(
    x = 0,
    y = c(4, 3, 2, 1),
    label = c(
      paste0("Started the survey\nN = ", rh_fmt_n(flow$started),
             "\nQuota cell full: ", rh_fmt_n(flow$quota_full)),
      paste0("Admitted to the questionnaire\nN = ", rh_fmt_n(flow$admitted)),
      paste0("Responses excluded\nn = ", rh_fmt_n(flow$excluded), "\n", reasons),
      paste0("Analysis sample\nN = ", rh_fmt_n(flow$analysed))
    )
  )
  arrows <- tibble::tibble(x = 0, xend = 0, y = c(3.72, 2.72, 1.72), yend = c(3.28, 2.28, 1.28))
  ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = arrows,
      ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
      arrow = grid::arrow(length = grid::unit(0.16, "inches")),
      linewidth = 0.5
    ) +
    ggplot2::geom_label(
      data = boxes,
      ggplot2::aes(x = x, y = y, label = label),
      size = 3.7,
      lineheight = 1.05,
      label.padding = grid::unit(0.18, "lines"),
      linewidth = 0.35,
      fill = "white"
    ) +
    ggplot2::coord_cartesian(xlim = c(-1.6, 1.6), ylim = c(0.7, 4.3), clip = "off") +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = ggplot2::margin(8, 8, 8, 8))
}
