# Presentation for the separate pairwise Bayesian correlation target.
# Shared APA formatting and table styling come from report_helpers.R.

#' Format the accepted correlation evidence marker as a superscript
format_correlation_marker <- function(evidence, format = c("html", "markdown", "plotmath")) {
  format <- match.arg(format)
  marker <- ifelse(is.na(evidence), "", ifelse(evidence == "present", "†",
                    ifelse(evidence == "absent", "0", "")))
  if (format == "html") return(ifelse(marker == "", "", paste0("<sup>", marker, "</sup>")))
  if (format == "markdown") return(ifelse(marker == "", "", paste0("^", marker, "^")))
  ifelse(marker == "", "", paste0("^\"", marker, "\""))
}

#' Format a correlation Bayes factor from its log value on the report's display rule
#'
#' The report's one rule for Bayes factors ([rh_fmt_bf()]): two decimals from 0.01 to 9.99, whole numbers from 10 to 100,
#' then only the power of ten the value exceeds, mirrored below 0.01. The
#' Bayes factor of the correlation test is computed, not sampled, so every
#' rung is resolvable; a log value beyond the numerical range of `exp()`
#' still lands on the outermost rung.
#'
#' @param log_bf10 Numeric vector of natural-log Bayes factors.
#' @return Character vector.
format_correlation_bf <- function(log_bf10) {
  rh_fmt_bf(exp(as.numeric(log_bf10)))
}

#' Monte Carlo precision of the correlation estimates, for the table note
#'
#' The smallest effective sample size of the posterior draws and the largest
#' Monte Carlo standard error of a posterior median, over the estimated pairs.
#'
#' @param correlations Output of the correlation target (its `pairs`).
#' @return One or two sentences.
describe_correlation_sampling <- function(correlations) {
  facts <- correlations$report
  if (is.null(facts)) facts <- build_correlation_reporting_data(correlations)$report
  if (facts$sampling_unavailable) {
    return("Monte Carlo diagnostics for the correlations were unavailable.")
  }
  paste0(
    "Across the estimated pairs, the smallest effective sample size of the posterior draws was ",
    if (is.finite(facts$ess_min)) rh_fmt_n(facts$ess_min) else "unavailable",
    " and the largest Monte Carlo standard error of a posterior median was ",
    if (is.finite(facts$mcse_max)) rh_fmt(facts$mcse_max, 4) else "unavailable", ".",
    if (facts$sampling_incomplete)
      " Diagnostics were unavailable for one or more estimated pairs." else ""
  )
}

#' Full pairwise posterior summaries and Bayes factors for an appendix
build_bayesian_correlation_details <- function(correlations, labels = rh_labels(),
                                              engine = "auto", target = NULL) {
  if (is.null(correlations$report)) correlations <- build_correlation_reporting_data(correlations)
  pairs <- correlations$pairs
  format <- if (rh_use_gt(engine)) "html" else "markdown"
  # Estimate and interval in one cell; the evidence
  # marker follows the estimate it qualifies.
  interval <- rh_fmt_ci(pairs$lower, pairs$upper, bounded = TRUE)
  display <- data.frame(
    scale_1 = rh_blank_repeated(rh_label(pairs$var_i, labels)), scale_2 = rh_label(pairs$var_j, labels),
    median = ifelse(is.na(pairs$median), "—", paste0(rh_fmt(pairs$median, bounded = TRUE),
                    format_correlation_marker(pairs$evidence, format), ifelse(interval == "—", "", paste0(" ", interval)))),
    bf10 = rh_fmt_bf(pairs$bf10), stringsAsFactors = FALSE
  )
  threshold <- correlations$metadata$bf_cutoff
  note <- paste0("ρ is the posterior median of the correlation, estimated assuming a correlation exists, ",
                 "with its central ", 100 * correlations$metadata$interval_level, "% credible interval (CrI). ",
                 "BF<sub>10</sub> states how many times more likely the data are with a correlation than with none (ρ = 0). ",
                 "Superscript †: BF<sub>10</sub> ≥ ", threshold, ", evidence for a correlation; superscript 0: ",
                 "BF<sub>10</sub> ≤ 1/", threshold,
                 ", evidence for no correlation; unmarked estimates: inconclusive evidence. ",
                 "Evidence for no correlation does not establish practical equivalence. ",
                 describe_correlation_sampling(correlations))
  unavailable <- pairs$status != "ok"
  if (any(unavailable)) {
    note <- paste0(note, " Dashes indicate estimates unavailable because of ",
                   paste(unique(tolower(pairs$status[unavailable])), collapse = "; "), ".")
  }
  rh_table(display,
           col_labels = c(scale_1 = "Scale or subscale 1", scale_2 = "Scale or subscale 2",
                          median = paste0("ρ [", 100 * correlations$metadata$interval_level, "% CrI]"),
                          bf10 = "BF<sub>10</sub>"), engine = engine, target = target,
           # The Bayes factors are plain text: a rung such as "> 100" would
           # open a Markdown block quote.
           source_note = note, markdown = "median")
}

#' Lower-triangle posterior correlation table with BF evidence superscripts
build_bayesian_correlation_table <- function(correlations, labels = rh_labels(),
                                            engine = "auto", target = NULL) {
  keys <- rownames(correlations$matrix)
  display <- data.frame(score = paste0(seq_along(keys), ". ", rh_label(keys, labels)),
                        stringsAsFactors = FALSE)
  value_columns <- paste0("score_", seq_along(keys))
  for (column in value_columns) display[[column]] <- ""
  format <- if (rh_use_gt(engine)) "html" else "markdown"
  for (k in seq_len(nrow(correlations$pairs))) {
    pair <- correlations$pairs[k, ]
    column <- match(pair$var_i, keys)
    row <- match(pair$var_j, keys)
    display[row, value_columns[column]] <- paste0(
      rh_fmt(pair$median, bounded = TRUE), format_correlation_marker(pair$evidence, format))
  }
  for (i in seq_along(keys)) display[i, value_columns[i]] <- "—"
  threshold <- correlations$metadata$bf_cutoff
  note <- paste0("Entries are posterior medians. Superscript †: BF10 ≥ ", threshold,
                 "; superscript 0: BF10 ≤ 1/", threshold,
                 "; unmarked estimates: inconclusive evidence. BF10 compares a nonzero correlation with zero.")
  unavailable <- sum(correlations$pairs$status != "ok")
  if (unavailable) note <- paste0(note, " ", unavailable, " pair(s) could not be estimated.")
  rh_table(display,
           col_labels = c(score = "Score", stats::setNames(seq_along(keys), value_columns)),
           engine = engine, target = target, source_note = note, markdown = value_columns)
}

#' Blue-red heatmap using exactly the posterior medians shown in the table
#'
#' Positive correlations are vermilion and negative correlations blue. Signed
#' numbers and BF superscripts retain the information without colour.
#' No joint positive-definiteness is implied.
plot_bayesian_correlation_heatmap <- function(correlations, labels = rh_labels()) {
  keys <- rownames(correlations$matrix)
  pairs <- correlations$pairs
  pairs$x <- factor(rh_label(pairs$var_i, labels), levels = rh_label(keys, labels))
  pairs$y <- factor(rh_label(pairs$var_j, labels), levels = rev(rh_label(keys, labels)))
  pairs$text <- paste0('"', rh_fmt(pairs$median, bounded = TRUE), '"',
                       format_correlation_marker(pairs$evidence, "plotmath"))
  ggplot2::ggplot(pairs, ggplot2::aes(x = x, y = y, fill = median)) +
    ggplot2::geom_tile(colour = "white", linewidth = .8) +
    ggplot2::geom_text(ggplot2::aes(label = text), parse = TRUE, size = 3, colour = "black") +
    ggplot2::scale_fill_gradient2(low = "#0072B2", mid = "grey96", high = "#D55E00",
                                   midpoint = 0, limits = c(-1, 1),
                                   na.value = "white", name = "ρ") +
    ggplot2::coord_equal() + ggplot2::labs(x = NULL, y = NULL) + rh_theme() +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(angle = 40, hjust = 1))
}

#' Save the numerical summaries used by the correlation displays and prose
build_correlation_reporting_data <- function(correlations) {
  pairs <- correlations$pairs
  available <- pairs$status %in% "ok" & is.finite(pairs$median)
  values <- pairs$median[available]
  sampled <- pairs$status %in% "ok"
  ess <- pairs$posterior_ess[sampled & is.finite(pairs$posterior_ess)]
  mcse <- pairs$median_mcse[sampled & is.finite(pairs$median_mcse)]
  if ("log_bf10" %in% names(pairs)) correlations$pairs$bf10 <- exp(as.numeric(pairs$log_bf10))
  correlations$report <- list(
    n_available = length(values), n_unavailable = sum(!available),
    n_positive = sum(available & is.finite(pairs$lower) & pairs$lower > 0),
    n_negative = sum(available & is.finite(pairs$upper) & pairs$upper < 0),
    n_present = sum(pairs$evidence %in% "present"),
    n_absent = sum(pairs$evidence %in% "absent"),
    n_inconclusive = sum(pairs$evidence %in% "inconclusive"),
    range = if (length(values)) range(values) else c(NA_real_, NA_real_),
    ess_min = if (length(ess)) min(ess) else NA_real_,
    mcse_max = if (length(mcse)) max(mcse) else NA_real_,
    sampling_unavailable = !length(ess) && !length(mcse),
    sampling_incomplete = length(ess) < sum(sampled) || length(mcse) < sum(sampled)
  )
  correlations
}
