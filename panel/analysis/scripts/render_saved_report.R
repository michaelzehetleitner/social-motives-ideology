#!/usr/bin/env Rscript
# Render committed report inputs; never evaluate a pipeline or fit a model.
arguments <- commandArgs(trailingOnly = TRUE)
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1L])
root <- normalizePath(file.path(dirname(script), ".."), winslash = "/", mustWork = TRUE)
input <- file.path(root, "data", "derived", "report_inputs.rds")
output <- file.path(root, "report")
html_only <- FALSE
while (length(arguments)) {
  option <- arguments[[1L]]
  if (option == "--html-only") {
    html_only <- TRUE
    arguments <- arguments[-1L]
  } else if (option %in% c("--inputs", "--output-dir") && length(arguments) >= 2L) {
    if (option == "--inputs") input <- arguments[[2L]] else output <- arguments[[2L]]
    arguments <- arguments[-c(1L, 2L)]
  } else if (option == "--help") {
    cat("Usage: Rscript scripts/render_saved_report.R [--inputs FILE] [--output-dir DIR] [--html-only]\n")
    quit(status = 0L)
  } else stop("Unknown or incomplete argument: ", option)
}
source(file.path(root, "R", "report_saved_inputs.R"))
input <- normalizePath(input, winslash = "/", mustWork = TRUE)
bundle <- read_report_inputs(input, root)
dir.create(output, recursive = TRUE, showWarnings = FALSE)
output <- normalizePath(output, winslash = "/", mustWork = TRUE)
quarto <- Sys.which("quarto")
if (!nzchar(quarto)) stop("Report replay requires the project's Quarto installation.")
Sys.setenv(ZM_REPORT_INPUTS = input, RENV_CONFIG_AUTOLOADER_ENABLED = "FALSE",
           R_LIBS_USER = paste(.libPaths(), collapse = .Platform$path.sep))
run <- function(command, args) {
  status <- system2(command, vapply(args, shQuote, character(1)))
  if (!identical(status, 0L)) stop("Report rendering failed: ", basename(command), " (", status, ").")
}
# Quarto resolves its resource paths from the project directory. Use that
# directory even when this script is invoked from elsewhere.
setwd(file.path(root, "report"))
quarto_args <- c("render", "results_draft.qmd", "--to", "html", "--output", "results_draft.html")
if (!identical(output, normalizePath(getwd(), winslash = "/"))) {
  quarto_args <- c(quarto_args, "--output-dir", output)
}
run(quarto, quarto_args)
if (!html_only) {
  python <- Sys.getenv("REPORT_PDF_PYTHON", "")
  if (!nzchar(python)) python <- if (file.exists("/opt/report-pdf/bin/python")) "/opt/report-pdf/bin/python" else Sys.which("python3")
  if (!nzchar(python)) stop("PDF replay requires the existing report PDF Python environment.")
  run(python, c(file.path(root, "scripts", "render_results_pdf.py"),
    "--input-html", file.path(output, "results_draft.html"),
    "--output-pdf", file.path(output, "results_draft.pdf"),
    "--qa-dir", file.path(output, "results-pdf-qa"),
    "--source-commit", bundle$values$report_provenance$git_head))
}
cat("Rendered saved calculation results from ", bundle$values$report_provenance$git_head,
    " to ", output, ". No models were fitted.\n", sep = "")
