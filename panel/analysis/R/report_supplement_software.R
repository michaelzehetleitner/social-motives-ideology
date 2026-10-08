# Report target of the supplement section "Software".
#
# Builds the one target (_targets.R, SUPPLEMENT · SOFTWARE) the section reads.
# The report words and formats what this returns.

#' Software of the fits
#'
#' The provenance of every regression fit and of the joint fit, and the
#' versions of the three packages that produced the fits, as recorded with the
#' fits themselves.
#'
#' @param regression_fit_register,joint_regression_fit The gated fits (targets
#'   of the same names).
#' @param analysis_plan Configuration from [zm_config()].
#' @return List `provenance` (one row per fit) and `fitted_versions` (named
#'   `brms`, `cmdstanr`, `CmdStan`; the distinct recorded versions joined by
#'   `" / "`, `NA` when none is recorded).
assemble_supplement_software <- function(regression_fit_register, joint_regression_fit, analysis_plan) {
  provenance <- dplyr::bind_rows(
    tabulate_regression_provenance(regression_fit_register),
    tabulate_joint_provenance(joint_regression_fit, analysis_plan)
  )
  recorded <- function(col) {
    v <- unique(as.character(provenance[[col]]))
    v <- v[!is.na(v) & nzchar(v)]
    if (length(v) == 0) NA_character_ else paste(v, collapse = " / ")
  }
  list(
    provenance = provenance,
    fitted_versions = c(brms = recorded("brms_version"), cmdstanr = recorded("cmdstanr_version"),
                        CmdStan = recorded("cmdstan_version"))
  )
}

#' Provenance of the build: the commit, the fingerprints and the run
#'
#' The report states which code produced it: the code at the commit named in
#' T10 is part of the preregistration (M1). Everything comes from the
#' calculation receipt; nothing is read from the environment at render time.
#'
#' @param calculation_receipt The `calculation_receipt` target
#'   ([record_calculation_receipt()]).
#' @param data_source The `data_source_used` target: `"synthetic"` or `"real"`.
#' @return List `git_head`, `git_dirty`, `built_at`, `r_version`, `profile`,
#'   `data_source` and `files`, a data frame `role`, `path`, `sha256` with one
#'   row each for the package lockfile, the analysis plan and the input data
#'   file; a file the receipt does not carry has `NA` path and fingerprint.
assemble_report_provenance <- function(calculation_receipt, data_source) {
  sources <- calculation_receipt$source_files
  inputs <- calculation_receipt$input_files
  pick <- function(table, pattern) {
    hit <- which(grepl(pattern, table$path))
    if (length(hit) == 0L) return(c(NA_character_, NA_character_))
    c(table$path[hit[1L]], table$sha256[hit[1L]])
  }
  lockfile <- pick(sources, "(^|/)renv\\.lock$")
  plan <- pick(sources, "(^|/)analysis_plan\\.yaml$")
  # the input data file: the CSV of the inputs that is not a codebook
  data_rows <- inputs[grepl("\\.csv$", inputs$path) & !grepl("codebook", basename(inputs$path)), ,
                      drop = FALSE]
  data_file <- if (nrow(data_rows)) c(data_rows$path[1L], data_rows$sha256[1L]) else
    c(NA_character_, NA_character_)
  list(
    git_head = calculation_receipt$git_head,
    git_dirty = calculation_receipt$git_dirty,
    built_at = calculation_receipt$built_at,
    r_version = calculation_receipt$r_version,
    profile = as.character(calculation_receipt$cfg$profile_name),
    data_source = as.character(data_source),
    reproduction_mode = calculation_receipt$reproduction_mode,
    reused_resampling = calculation_receipt$reused_resampling,
    files = data.frame(
      role = c("lockfile", "analysis plan", "input data"),
      path = c(lockfile[1L], plan[1L], data_file[1L]),
      sha256 = c(lockfile[2L], plan[2L], data_file[2L]),
      stringsAsFactors = FALSE
    )
  )
}

# ---- the tables of the section, built from the accepted results ----------

#' Fit-provenance rows of the register
#'
#' Every row is the provenance `ap6_fit_provenance()` captured at the fit
#' boundary; the register supplies its identity, role and label.
#'
#' @param register Records from `extract_regression_result_records()`.
#' @return Tibble in the `ap6_fit_provenance()` shape with `role` and
#'   `label`.
tabulate_regression_provenance <- function(register) {
  rows <- lapply(register, function(record) {
    if (!isTRUE(record$fit_available)) return(NULL)
    if (is.null(record$provenance)) {
      stop("The fit-time provenance of '", record$outcome, "' (", record$role,
           ") is absent; it must be captured at the fit boundary.")
    }
    provenance <- tibble::as_tibble(record$provenance)
    provenance$outcome <- as.character(record$outcome_key)
    provenance$slope_sd <- as.numeric(record$slope_sd)
    zm_tag(provenance, role = record$role, label = record$label)
  })
  dplyr::bind_rows(rows)
}

#' Provenance row of the joint model
#'
#' The joint row of the provenance table, read off the provenance the gate stored
#' on the fit. The gate records the collection key `"joint"`; the row names
#' the fit by its model, `"multivariate"`.
#'
#' @param joint_regression_fit The gated collection `list(joint = <fit>)` of
#'   the `joint_regression_fit` target.
#' @param analysis_plan Analysis configuration.
#' @return One-row tibble in the `ap6_fit_provenance()` shape with `role` and
#'   `label`; `NULL` when no joint fit is available.
tabulate_joint_provenance <- function(joint_regression_fit, analysis_plan) {
  fit <- joint_regression_fit$joint
  if (!inherits(fit, "brmsfit")) return(NULL)
  provenance <- attr(fit, "provenance", exact = TRUE)
  if (is.null(provenance)) {
    stop("The fit-time provenance of the joint model is absent; it must be ",
         "captured at the fit boundary.")
  }
  provenance <- tibble::as_tibble(provenance)
  provenance$outcome <- "multivariate"
  provenance$slope_sd <- as.numeric(attr(fit, "slope_sd", exact = TRUE))
  zm_tag(provenance, role = "multivariate", label = zm_fit_label(analysis_plan, "multivariate"))
}
