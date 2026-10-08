# Item-response distributions for AP4/AP5 reporting -------------------------
# Use the retained AP1 item responses before reverse scoring. These counts
# describe the observed categories; they do not enter an analysis or select
# an estimator.

#' Counts and percentages of every response category for every scale item
#'
#' @param items_data Retained responses before reverse scoring (`ap1_data`).
#' @param codebook Codebook from `zm_codebook()`.
#' @param analysis_plan Analysis configuration with the response bounds in `scales`.
#' @return A presentation tibble: Scale, Item, N, Missing, then one column per
#'   response category. Percentages use the item's non-missing responses.
#'   Empty categories are shown explicitly.
rh_item_distribution_table <- function(items_data, codebook, analysis_plan) {
  bounds <- c(analysis_plan$scales$response_min, analysis_plan$scales$response_max)
  if (length(bounds) != 2L || any(!is.finite(bounds)) ||
      any(bounds != as.integer(bounds)) || bounds[1] >= bounds[2]) {
    stop("Item distributions need integer response bounds in analysis_plan$scales.")
  }
  categories <- seq.int(bounds[1], bounds[2])
  scales <- codebook$scales
  rows <- list()
  for (i in seq_len(nrow(scales))) {
    for (item in scales$item_codes[[i]]) {
      x <- items_data[[item]]
      # A response outside the admissible categories would count in N and in no
      # category, so the percentages would no longer sum to 100.
      if (!is.numeric(x) || any(!is.na(x) & !(x %in% categories))) {
        stop("Item '", item, "' has responses outside ", bounds[1], "..", bounds[2], ".")
      }
      n <- sum(!is.na(x))
      counts <- tabulate(match(x, categories), nbins = length(categories))
      # An item with no observed response has no percentage, not a NaN one: the
      # denominator is zero, so the share is undefined and is displayed as such.
      cells <- if (n > 0L) {
        sprintf("%d (%.1f%%)", counts, 100 * counts / n)
      } else {
        rep("0 (n/a)", length(categories))
      }
      row <- tibble::tibble(Scale = scales$label[i], Item = item, N = n, Missing = sum(is.na(x)))
      for (j in seq_along(categories)) row[[paste("Response", categories[j])]] <- cells[j]
      rows[[length(rows) + 1L]] <- row
    }
  }
  dplyr::bind_rows(rows)
}
