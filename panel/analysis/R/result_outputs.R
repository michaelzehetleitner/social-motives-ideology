# Result outputs: the tables and files the targets write for the write-up

#' The empty deviations register of the preregistration (AP11, Deviations Policy)
#'
#' "Any departure from the preregistration is documented in a deviations
#' section of the final write-up." The section renders "None to date" while
#' this table has no row. A
#' deviation is added here, with its date and the reason, when one occurs.
#'
#' @return Zero-row tibble `date`, `preregistered`, `deviation`, `reason`.
zm_deviations_table <- function() {
  tibble::tibble(
    date = character(0),
    preregistered = character(0),
    deviation = character(0),
    reason = character(0)
  )
}

#' The R files a report sources when it is rendered
#'
#' A rendered report reads its numbers from targets, but its wording and
#' display helpers come from R files it sources itself. `tar_quarto()` tracks
#' those files only when they are named, so the report target is given the list
#' read from the report's own `source()` calls: a changed helper then renders
#' the report again.
#'
#' @param report_path Path of the `.qmd` file, relative to the project root.
#' @return Character vector of `R/<file>.R` paths, in the order the report
#'   sources them.
find_report_source_files <- function(report_path) {
  lines <- readLines(report_path, warn = FALSE)
  hits <- regmatches(lines, regexpr('source\\(file\\.path\\(analysis_root, "R", "[A-Za-z0-9_.]+\\.R"\\)', lines))
  files <- sub('.*"R", "([A-Za-z0-9_.]+\\.R)"\\)$', "\\1", hits)
  file.path("R", unique(files))
}


#' Write one of the supplement's complete-number files
#'
#' The supplement prints compact tables; the complete numbers behind them go
#' to the machine-readable files named in `analysis_plan$data_files$result_files`
#' (AP4, AP6). One long CSV per key, written as given.
#'
#' @param table Tibble to write.
#' @param analysis_plan Configuration from [zm_config()].
#' @param key Name of the file in `analysis_plan$data_files$result_files`.
#' @return Normalised path of the written CSV file.
write_result_file <- function(table, analysis_plan, key) {
  path <- analysis_plan$data_files$result_files[[key]]
  if (is.null(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    stop("analysis_plan$data_files$result_files$", key, " is required.")
  }
  if (!grepl("^(/|~)", path)) path <- file.path(analysis_plan$root, path)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(table, path, na = "", progress = FALSE)
  normalizePath(path)
}
