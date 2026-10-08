# AP4 CFA input ---------------------------------------------------------------
#
# The one scientific difference between the CFA input and the common
# descriptive/reliability table: the item imputation can leave a fractional
# expected answer in a cell that was empty, and an ordered-item CFA needs a
# response category there. The rounding is to the digit with halves going UP;
# it touches exactly the recorded fill cells
# of the items; every observed answer, every demographic fill and every
# non-item column stays as the common table has it. Reliability, EFA and the
# descriptives keep reading the common table.

#' AP4 — the CFA-only copy with the imputed item cells rounded
#'
#' `filled_cells` records every cell the AP3 imputation wrote. Its `item` rows
#' name the originally empty item cells by respondent and column; those cells,
#' and only those, are rounded to the nearest integer, and a half goes UP:
#' 2.5 becomes 3, 3.5 becomes 4, 4.5 becomes 5. `floor(x + 0.5)`
#' is that half-up rule. R's `round()` is deliberately NOT used: it applies the
#' half-to-even rule (`round(2.5)` is 2, `round(4.5)` is 4), which would send
#' half of the ties downwards. No other rounding rule is applied, and no cell
#' the record does not name is touched.
#'
#' A recorded cell that is not an item column of this table, or a recorded
#' respondent that is not in it, is an inconsistency between the record and the
#' data and stops here rather than being skipped.
#'
#' @param data The common descriptive/reliability input
#'   (`data_descriptive_reliability`).
#' @param filled_cells The table of filled cells ([build_filled_cells_table()]) with `respondent_id`,
#'   `variable` and `kind`.
#' @param codebook Codebook from [zm_codebook()]; its label-to-column map names
#'   the item columns of this table.
#' @return The same tibble with the recorded item cells rounded half-up, carrying every
#'   attribute of `data`.
round_imputed_items_for_cfa <- function(data, filled_cells, codebook) {
  item_columns <- zm_item_column_map(codebook)
  cells <- filled_cells[filled_cells$kind == "item", , drop = FALSE]
  items <- intersect(unname(item_columns), names(data))
  unknown <- setdiff(unique(as.character(cells$variable)), items)
  if (length(unknown)) {
    stop("The imputation record names cells outside the CFA item columns: ",
         paste(unknown, collapse = ", "), ".")
  }
  rows <- match(as.integer(cells$respondent_id), as.integer(data$respondent_id))
  if (anyNA(rows)) {
    stop("The imputation record names respondents absent from the CFA input: ",
         paste(unique(cells$respondent_id[is.na(rows)]), collapse = ", "), ".")
  }
  rounded <- data
  for (column in unique(as.character(cells$variable))) {
    hit <- rows[as.character(cells$variable) == column]
    # Half-up, not R's round(): 2.5 -> 3, 3.5 -> 4, 4.5 -> 5.
    rounded[[column]][hit] <- floor(rounded[[column]][hit] + 0.5)
  }
  attributes(rounded) <- attributes(data)
  rounded
}
