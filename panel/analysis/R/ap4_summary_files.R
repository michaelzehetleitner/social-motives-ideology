# AP4 summary statistics for secondary analysis
#
# JARS-Quant Table 7 asks a report of a categorical measurement model to make
# the sufficient summary statistics available, which for ordinal indicators
# under WLSMV are the polychoric correlation matrix and the item thresholds —
# the two quantities lavaan actually fits. They are too large for a printed
# table and are therefore written as machine-readable files beside the pairwise
# residual CSV that AP4 already ships, and named in the supplement.
#
# The matrix is computed with the same estimator settings the reliability and
# factor analyses use — psych::polychoric with the plan's continuity
# correction — so that the shipped matrix is the matrix the analyses saw, not a
# second one produced for the appendix. Nothing here decides anything: the two
# files are output, and no analysis reads them back.

#' AP4 — write the polychoric correlation matrix and the item thresholds
#'
#' One file each, beside `analysis_plan$factor_analysis$cfa_reporting$residual_data_file`:
#'
#'   * `cfa_polychoric_correlations.csv` — the full item-by-item polychoric
#'     correlation matrix, first column `item`;
#'   * `cfa_item_thresholds.csv` — one row per item and threshold, with the
#'     threshold's index and its value on the latent normal scale.
#'
#' @param items Reverse-scored item data (target `data_descriptive_reliability`).
#' @param codebook Codebook from `zm_codebook()`; the item order of
#'   `codebook$items` fixes the row and column order when it is available.
#' @param analysis_plan Configuration from `zm_config()`.
#' @return Character vector of the two written paths (normalised).
write_measurement_summary_files <- function(items, codebook, analysis_plan) {
  base <- analysis_plan$factor_analysis$cfa_reporting$residual_data_file
  if (is.null(base) || length(base) != 1L || is.na(base) || !nzchar(base)) {
    stop("AP4: analysis_plan$factor_analysis$cfa_reporting$residual_data_file is required to place the summary files.")
  }
  if (!grepl("^(/|~)", base)) base <- file.path(analysis_plan$root, base)
  dir <- dirname(base)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  items <- exclude_rows_with_failed_imputations(items, unique(unlist(codebook$scales$item_codes)))
  require_valid_imputation_inputs(items, unique(unlist(codebook$scales$item_codes)))
  x <- ap4_summary_item_data(items, codebook)
  poly <- suppressWarnings(psych::polychoric(as.data.frame(x), correct = ap4_polychoric_correct))
  R <- poly$rho
  if (!is.matrix(R) || any(!is.finite(R))) {
    stop("AP4: the polychoric step returned no finite correlation matrix over the item set.")
  }
  corr_path <- file.path(dir, "cfa_polychoric_correlations.csv")
  corr_out <- data.frame(item = rownames(R), stringsAsFactors = FALSE, check.names = FALSE)
  corr_out <- cbind(corr_out, as.data.frame(unname(R), stringsAsFactors = FALSE))
  names(corr_out) <- c("item", colnames(R))
  readr::write_csv(corr_out, corr_path, na = "", progress = FALSE)

  tau_path <- file.path(dir, "cfa_item_thresholds.csv")
  readr::write_csv(ap4_thresholds_long(poly$tau), tau_path, na = "", progress = FALSE)

  c(normalizePath(corr_path), normalizePath(tau_path))
}

#' The item columns the summary files cover, in codebook order
#'
#' @param items Reverse-scored item data.
#' @param codebook Codebook from `zm_codebook()`, or `NULL`.
#' @return Data frame of the numeric item columns.
ap4_summary_item_data <- function(items, codebook = NULL) {
  d <- as.data.frame(items, stringsAsFactors = FALSE)
  # The 57 scale items only: codebook$items also lists the attention checks and
  # the consent field, which are not part of any measurement model.
  if (is.null(codebook$scales)) {
    stop("AP4: the codebook must carry `scales` with the item codes of every scale.")
  }
  keys <- as.character(unlist(codebook$scales$item_codes, use.names = FALSE))
  cols <- intersect(keys, names(d))
  cols <- cols[vapply(d[cols], is.numeric, logical(1))]
  if (length(cols) < 2L) {
    stop("AP4: fewer than two numeric item columns are available for the polychoric summary files.")
  }
  spread <- vapply(d[cols], function(v) stats::sd(v, na.rm = TRUE), numeric(1))
  flat <- cols[!is.finite(spread) | spread == 0]
  if (length(flat) > 0L) {
    stop("AP4: item(s) without variance cannot enter the polychoric matrix: ", paste(flat, collapse = ", "))
  }
  d[, cols, drop = FALSE]
}

#' Item thresholds in long form
#'
#' `psych::polychoric()` returns the thresholds as an item-by-threshold matrix
#' with `Inf` for the open ends of the scale; the long form keeps the finite
#' thresholds only, which is what a secondary analysis needs.
#'
#' @param tau Threshold matrix from `psych::polychoric()`.
#' @return Tibble `item`, `threshold`, `value`.
ap4_thresholds_long <- function(tau) {
  m <- as.matrix(tau)
  # psych returns items in rows and thresholds in columns; older versions
  # transpose it, so the orientation is read off the dimnames rather than
  # assumed.
  if (!is.null(colnames(m)) && all(grepl("^[0-9]+$", colnames(m)))) {
    # already item x threshold
  } else if (!is.null(rownames(m)) && all(grepl("^[0-9]+$", rownames(m)))) {
    m <- t(m)
  }
  items <- rownames(m)
  if (is.null(items)) items <- paste0("item_", seq_len(nrow(m)))
  out <- tibble::tibble(
    item = rep(items, times = ncol(m)),
    threshold = rep(seq_len(ncol(m)), each = nrow(m)),
    value = as.numeric(m)
  )
  out <- out[is.finite(out$value), , drop = FALSE]
  out[order(match(out$item, items), out$threshold), , drop = FALSE]
}
