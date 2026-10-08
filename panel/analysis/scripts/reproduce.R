#!/usr/bin/env Rscript
# Run from panel/analysis. Intake preparation remains a separate one-time step.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L || !args %in% c("report", "models", "full")) {
  cat("Usage: Rscript scripts/reproduce.R report|models|full\n",
      "  report  Render the committed report inputs, without targets or model fits.\n",
      "  models  Rerun ordinary models and checks; reuse verified resampling results.\n",
      "  full    Rerun all main analyses and the separate prior-recovery simulation.\n",
      "Prepared intake is required for models/full; these commands never delete originals.\n", sep = "")
  quit(status = if (identical(args, "--help")) 0L else 1L)
}
source("R/config.R")
source("R/reproduction.R")
run_reproduction(args[[1L]])
