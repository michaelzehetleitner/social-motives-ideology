#!/usr/bin/env Rscript
# One-time AP1/AP3 intake. Originals and private comments are retained.
# MZ controls all study-data deletion outside this script.
args <- commandArgs(trailingOnly = TRUE)
if ("--help" %in% args || !length(args)) {
  cat(paste0(
    "Usage: Rscript scripts/prepare_intake.R --source=synthetic|real --input=PATH\n",
    "       [--propose] [--proposal=PATH] [--approval=PATH] [--out-dir=DIR]\n",
    "\n",
    "--propose writes column decisions for review and retains the original.\n",
    "Preparation applies AP1/AP3 once and writes the filled scientific-use CSV,\n",
    "the shuffled observed-demographics CSV, preparation results and receipt.\n",
    "The original export and private comments are retained.\n",
    "Review the files, mask identifying text in comments and commit preparation results.\n",
    "All study-data deletion, including on Qualtrics, is controlled by MZ.\n",
    "--out-dir is available only for temporary synthetic preparation.\n"
  ))
  quit(status = if ("--help" %in% args) 0L else 1L)
}
flags <- "--propose"
values <- c("source", "input", "proposal", "approval", "out-dir")
opts <- list()
for (arg in args) {
  if (arg %in% flags) {
    key <- sub("^--", "", arg)
    if (!is.null(opts[[key]])) stop("Duplicate argument: ", key)
    opts[[key]] <- TRUE
  } else {
    if (!grepl("^--[^=]+=.+$", arg)) stop("Unknown or unsupported argument: ", arg, "; see --help.")
    key <- sub("^--([^=]+)=.*$", "\\1", arg)
    if (!key %in% values || !is.null(opts[[key]])) stop("Unknown or duplicated argument name.")
    opts[[key]] <- sub("^--[^=]+=", "", arg)
  }
}
if (is.null(opts$source) || is.null(opts$input)) stop("Both --source and --input are required.")
for (file in list.files("R", pattern = "[.]R$", full.names = TRUE)) source(file)
analysis_plan <- zm_config()
source_name <- zm_clean_intake_source(opts$source)
if (isTRUE(opts$propose)) {
  if (!is.null(opts[["out-dir"]])) stop("--propose only writes the column proposal.")
  path <- if (is.null(opts$proposal)) file.path(analysis_plan$root, analysis_plan$data_files$intake$proposal_file) else opts$proposal
  if (file.exists(path)) stop("Proposal exists; choose a new --proposal path.")
  raw <- read_qualtrics_export(opts$input)
  assert_raw_schema(raw)
  ap3_intake_write_proposal(ap3_intake_proposal(raw, analysis_plan), analysis_plan, path, data_source = source_name)
  cat("Column-only proposal written. Original retained.\n")
} else {
  if (!is.null(opts$proposal)) stop("Use --help for the preparation arguments.")
  approval <- if (is.null(opts$approval)) file.path(analysis_plan$root, analysis_plan$data_files$intake$approval_file) else opts$approval
  zm_setup()
  paths <- zm_prepare_clean_intake(opts$input, analysis_plan, source_name,
    approval = approval, out_dir = opts[["out-dir"]])
  cat("Verified preparation files:\n", paste(unname(paths), collapse = "\n"),
      "\nOriginal and private comments retained. Study-data deletion is controlled by MZ.\n", sep = "")
}
