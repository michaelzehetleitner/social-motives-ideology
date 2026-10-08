# Pipeline input selection
#
# Target-facing helpers for choosing the approved cleaned input. Scientific
# intake and verification rules remain in io_clean_intake.R.

#' Data source chosen by the environment variable ZM_DATA
#'
#' @param value Raw value of `ZM_DATA` (default read from the environment).
#' @return `"synthetic"` or `"real"`; stops on any other value.
zm_data_source <- function(value = Sys.getenv("ZM_DATA", "synthetic")) {
  value <- tolower(trimws(value))
  if (!nzchar(value)) value <- "synthetic"
  if (!value %in% c("synthetic", "real")) {
    stop("ZM_DATA must be 'synthetic' or 'real', got '", value, "'.")
  }
  value
}

#' Check and normalise the requested synthetic or real source
#'
#' `analysis_input_files` validates the run specification once before
#' constructing its source-dependent paths.
#'
#' @param run_spec List with the element `data_source`.
#' @return `list(data_source)` with the normalised source.
check_run_spec <- function(run_spec) {
  source <- tolower(trimws(run_spec$data_source))
  if (length(source) != 1L || is.na(source) ||
      !source %in% c("synthetic", "real")) {
    stop("run_spec$data_source must be synthetic or real.")
  }
  list(data_source = source)
}

#' The canonical names of the tracked input paths, in their fixed order
#'
#' The one definition of that order: [select_analysis_input_files()] builds the
#' paths in it, and [zm_name_input_files()] reads it back onto them, so the two
#' cannot drift apart.
#'
#' @return Character vector of the eight names.
zm_analysis_input_names <- function() {
  c("analysis_plan", "codebook_items", "codebook_scales", "codebook_factors",
    "data", "demographics", "preparation", "receipt")
}

#' Re-attach the canonical names to the tracked input paths
#'
#' Technical helper. A `format = "file"` target returns its paths from the
#' targets store as an unnamed character vector; the readers of the bundle index
#' it by name. The names are restored by position, in the one order
#' [zm_analysis_input_names()] defines.
#'
#' @param paths The eight input paths in that order, named or not.
#' @return The same paths, named.
zm_name_input_files <- function(paths) {
  expected <- zm_analysis_input_names()
  if (length(paths) != length(expected)) {
    stop("The tracked analysis input paths must be ", length(expected),
         " in the order of zm_analysis_input_names(), got ", length(paths), ".")
  }
  stats::setNames(unname(paths), expected)
}

#' Produce the complete tracked set of analysis input paths
#'
#' Receives the already validated source and tracks the analysis plan, all three
#' codebooks, and the selected filled CSV, observed demographics, preparation results and receipt. The targets route never
#' opens the original export: the one-time intake reads the Qualtrics CSV or SAV,
#' validates it and persists only the prepared analysis inputs and their receipt.
#'
#' @param source `"synthetic"` or `"real"`, already checked by [check_run_spec()].
#' @param root Analysis project root (`analysis_plan$root`).
#' @param codebook_dir Codebook directory relative to `root`
#'   (`analysis_plan$meta$codebook_dir`); the same directory [zm_codebook()] reads.
#' @return Named character vector `analysis_plan`, `codebook_items`,
#'   `codebook_scales`, `codebook_factors`, `data`,
#'   `receipt`.
select_analysis_input_files <- function(source, root, codebook_dir) {
  # analysis_input_files checked and normalised source before this call.
  zm_name_input_files(c(
    file.path(root, "config", "analysis_plan.yaml"),
    file.path(root, codebook_dir, "codebook_items.csv"),
    file.path(root, codebook_dir, "codebook_scales.csv"),
    file.path(root, codebook_dir, "codebook_factors.csv"),
    file.path(root, "data", "intake", paste0(source, ".csv")),
    file.path(root, "data", "intake", paste0(source, "_demographics.csv")),
    file.path(root, "data", "intake", paste0(source, "_preparation.rds")),
    file.path(root, "data", "intake", paste0(source, ".yaml"))
  ))
}
