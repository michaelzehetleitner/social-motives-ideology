#!/usr/bin/env Rscript

# Manually refresh or check the human-readable parameter cards. tar_make()
# refreshes them automatically through the parameter_card_file target before
# participant eligibility and the calculation archive.
#
# Usage from panel/analysis:
#   Rscript scripts/update_parameter_cards.R
#   Rscript scripts/update_parameter_cards.R --check

root <- normalizePath(getwd(), mustWork = TRUE)
while (!file.exists(file.path(root, "R", "parameter_cards.R"))) {
  parent <- dirname(root)
  if (identical(parent, root)) stop("Could not find R/parameter_cards.R from ", getwd(), ".")
  root <- parent
}
source(file.path(root, "R", "parameter_cards.R"))

pc_main <- function(args = commandArgs(trailingOnly = TRUE)) {
  unknown <- setdiff(args, "--check")
  if (length(unknown) > 0L) stop("Unknown argument(s): ", paste(unknown, collapse = ", "), ".")
  changed <- pc_update_parameter_cards(check = "--check" %in% args)
  if (!"--check" %in% args) {
    message(if (changed) "Updated parameter cards." else "Parameter cards are current.")
  }
  invisible(changed)
}

pc_running_as_script <- function() {
  file_args <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_args) != 1L) return(FALSE)

  invoked <- sub("^--file=", "", file_args)
  identical(
    normalizePath(invoked, mustWork = TRUE),
    normalizePath("scripts/update_parameter_cards.R", mustWork = TRUE)
  )
}

if (pc_running_as_script()) pc_main()
