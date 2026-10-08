# Report target of the supplement section S6 "Sampling diagnostics of the
# regressions".
#
# Builds the one target (_targets.R, SUPPLEMENT · S6) the section, its table
# and the RQ1 estimation paragraph read. The report words and formats what this
# returns.

#' S6 sampling diagnostics
#'
#' One row per primary and prior-sweep fit with its diagnostics and validity
#' gate, and the fits that did not pass, each with the diagnostics it failed.
#'
#' @param regression_fit_register The accepted fit register (target of the same name).
#' @param analysis_plan Configuration from [zm_config()].
#' @return List `diagnostics`, `components` and `failed` (see
#'   [check_sampling_diagnostics()]).
assemble_supplement_sampling_diagnostics <- function(regression_fit_register, analysis_plan) {
  diagnostics <- tabulate_sampling_diagnostics(regression_fit_register, analysis_plan,
                                                  roles = c("primary", "sweep"))
  c(list(diagnostics = diagnostics), check_sampling_diagnostics(diagnostics))
}

#' Which fits failed the validity gate, and on which diagnostics
#'
#' @param diagnostics One row per fit with the five `*_ok` flags, `ok` and
#'   `gate_status`.
#' @return List `components` (the names of the five diagnostics) and `failed`
#'   (tibble `outcome`, `slope_sd`, `family` and the list column
#'   `failed_components`; an empty element where only the gate status failed).
check_sampling_diagnostics <- function(diagnostics) {
  component_ok <- cbind(
    "bulk and tail ESS" = diagnostics$ess_ok %in% TRUE,
    "R-hat" = diagnostics$rhat_ok %in% TRUE,
    "divergent transitions" = diagnostics$divergences_ok %in% TRUE,
    "maximum tree depth" = diagnostics$treedepth_ok %in% TRUE,
    "BFMI" = diagnostics$bfmi_ok %in% TRUE
  )
  passed <- diagnostics$ok %in% TRUE & diagnostics$gate_status %in% c("ok", "retried_ok")
  failed <- diagnostics[!passed, , drop = FALSE]
  list(
    n_fits = nrow(diagnostics), n_failed = nrow(failed),
    components = colnames(component_ok),
    failed = tibble::tibble(
      outcome = failed$outcome,
      slope_sd = failed$slope_sd,
      family = failed$family,
      failed_components = lapply(which(!passed), function(i) colnames(component_ok)[!component_ok[i, ]])
    )
  )
}

# ---- the tables of the section, built from the accepted results ----------

#' Diagnostics rows of the roles a consumer reads
#'
#' @param register Records from `extract_regression_result_records()`.
#' @param analysis_plan Analysis configuration.
#' @param roles Model roles to keep.
#' @param available_only Keep only records whose fit exists (an untriggered
#'   Student-t refit gets no row).
#' @return Tibble in the `zm_fit_diagnostics_row()` shape.
tabulate_sampling_diagnostics <- function(register, analysis_plan, roles, available_only = FALSE) {
  keep <- vapply(register, function(record) {
    record$role %in% roles && (!available_only || isTRUE(record$fit_available))
  }, logical(1))
  zm_regression_register_table(register[keep], analysis_plan)
}

#' Save the extrema and additional fitted-model diagnostics used in the report
add_sampling_reporting_facts <- function(report, regression_fit_register, joint_regression_fit,
                                         motive_model_fit) {
  report$additional <- collect_saved_additional_diagnostics(
    regression_fit_register, joint_regression_fit, motive_model_fit)
  rows <- report$diagnostics
  report$extrema <- summarise_sampling_diagnostic_extrema(rows)
  attr(report$diagnostics, "report_extrema") <- report$extrema
  report$n_fits <- nrow(rows)
  report$n_failed <- nrow(report$failed)
  report$n_refits <- sum(!is.na(report$additional$outcome))
  report$has_joint <- any(report$additional$kind %in% "joint")
  report$has_motives <- any(report$additional$kind %in% "motives")
  motives <- report$additional[report$additional$kind %in% "motives", , drop = FALSE]
  report$motive_gate_passed <- nrow(motives) == 1L && motives$gate_passed %in% TRUE &&
    motives$gate_status %in% c("ok", "retried_ok")
  report$all_passed <- nrow(rows) > 0L && !nrow(report$failed) &&
    nrow(report$additional) > 0L && all(report$additional$gate_passed %in% TRUE)
  report
}

#' Save the finite diagnostic extrema without inventing values for absent rows
summarise_sampling_diagnostic_extrema <- function(rows) {
  extreme <- function(key, direction) {
    x <- as.numeric(rows[[key]])
    x <- x[is.finite(x)]
    if (!length(x)) return(NA_real_)
    if (direction == "maximum") max(x) else min(x)
  }
  list(
    rhat_max = extreme("rhat_max", "maximum"),
    ess_bulk_min = extreme("ess_bulk_min", "minimum"),
    ess_tail_min = extreme("ess_tail_min", "minimum"),
    bfmi_min = extreme("bfmi_min", "minimum"),
    n_divergent = extreme("n_divergent", "maximum"),
    n_treedepth_hits = extreme("n_treedepth_hits", "maximum"))
}
