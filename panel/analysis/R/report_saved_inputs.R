# Portable report inputs: tables, calculated plot series and saved-run provenance.
# Model fits, posterior draw arrays and unused respondent records stay local.

report_input_names <- function() {
  c("imputation_reporting_data",
    "pipeline_config",
    "data_source_used",
    "scale_score_distributions",
    "report_bayesian_correlations",
    "report_primary_coefficients",
    "report_joint",
    "joint_correlation_changes",
    "joint_asc_aggregation",
    "deviations_register",
    "supplement_software",
    "report_provenance",
    "report_network",
    "supplement_measurement",
    "supplement_prior_sensitivity",
    "supplement_model_checks",
    "supplement_network_detail",
    "supplement_sampling_diagnostics",
    "regression_r2_summaries",
    "supplement_explained_variance",
    "report_participant_flow",
    "report_sample_sizes",
    "report_sample_characteristics",
    "report_political_sample",
    "report_reliability_table",
    "report_reliability_counts",
    "report_association_table",
    "rq1_coefficient_differences",
    "rq1_motive_partial_correlations",
    "report_association_counts",
    "report_credible_associations",
    "report_prior_width_changes",
    "codebook",
    "report_synthetic_methods")
}

#' Keep exactly the objects the complete report displays
reduce_report_inputs <- function(inputs) {
  missing <- setdiff(report_input_names(), names(inputs))
  if (length(missing)) stop("Report inputs are missing: ", paste(missing, collapse = ", "))
  inputs <- inputs[report_input_names()]
  inputs$pipeline_config$root <- "."
  status <- inputs$imputation_reporting_data$model_status
  columns <- c("model", "kind", "n_missing", "status", "attempts", "parameter_count",
    "ess_bulk_min", "ess_tail_min", "rhat_max", "n_divergent", "n_treedepth_hits", "bfmi_min", "note")
  if (!is.null(status)) status <- status[intersect(columns, names(status))]
  inputs$imputation_reporting_data <- list(model_status = status,
    n_unfilled = inputs$imputation_reporting_data$n_unfilled)
  for (name in c("joint_correlation_changes", "joint_asc_aggregation")) inputs[[name]]$draws <- NULL
  inputs$supplement_measurement$imputation <- NULL
  inputs$supplement_measurement$imputation_model_status <- NULL
  gender <- inputs$supplement_prior_sensitivity$model_card$analysis_data$gender
  if (!is.null(gender)) {
    gender <- as.factor(gender)
    inputs$supplement_prior_sensitivity$model_card <- list(
      gender_counts = stats::setNames(as.integer(table(gender)[levels(gender)]), levels(gender)))
  }
  density <- inputs$supplement_model_checks$predictive_density
  if (is.null(density)) stop("Saved posterior-predictive density series are missing; rebuild report calculations first.")
  unavailable <- attr(inputs$supplement_model_checks$predictive_draws, "unavailable_predictions", exact = TRUE)
  if (!is.null(unavailable)) attr(density, "unavailable_predictions") <- unavailable
  inputs$supplement_model_checks$predictive_density <- density
  inputs$supplement_model_checks$predictive_draws <- NULL
  inputs$report_political_sample$scores <- NULL
  if (is.null(attr(inputs$scale_score_distributions, "histograms", exact = TRUE)) ||
      is.null(inputs$report_political_sample$histogram_data)) {
    stop("Saved scale or sample histogram bins are missing; rebuild report calculations first.")
  }
  inputs <- strip_report_parser_metadata(inputs)
  assert_reduced_report_inputs(inputs)
  inputs
}

# CSV parser pointers are session-local metadata, not reported quantities.
strip_report_parser_metadata <- function(x) {
  if (inherits(x, "spec_tbl_df")) {
    attr(x, "problems") <- NULL
    attr(x, "spec") <- NULL
    class(x) <- setdiff(class(x), "spec_tbl_df")
  }
  if (is.list(x)) for (i in seq_along(x)) x[i] <- list(strip_report_parser_metadata(x[[i]]))
  x
}

#' Reject model objects and unused raw-data containers, including hidden attributes
assert_reduced_report_inputs <- function(x, path = "inputs") {
  if (is.environment(x) || is.function(x) || isS4(x) || typeof(x) %in% c("externalptr", "weakref") ||
      inherits(x, c("brmsfit", "stanfit", "CmdStanMCMC", "BDgraph", "lavaan"))) {
    stop("A fitted model or executable object is not a report input: ", path)
  }
  if (is.list(x)) {
    forbidden <- intersect(names(x), c("draws", "predictive_draws", "analysis_data", "responses"))
    if (length(forbidden)) stop("Unused raw records remain in report inputs: ", path, "$", forbidden[[1L]])
    for (i in seq_along(x)) assert_reduced_report_inputs(x[[i]], paste0(path, "$", if (is.null(names(x))) i else names(x)[i]))
  }
  attrs <- attributes(x)
  attrs <- attrs[setdiff(names(attrs), c("names", "class", "row.names", "dim", "dimnames"))]
  for (name in names(attrs)) assert_reduced_report_inputs(attrs[[name]], paste0(path, " attr(", name, ")"))
  invisible(TRUE)
}

#' Save report-ready results without carrying a targets store or machine paths
write_report_inputs <- function(inputs, calculation_receipt, analysis_plan,
                                path = file.path(analysis_plan$root, "data/derived/report_inputs.rds")) {
  if (!identical(inputs$data_source_used, "synthetic")) {
    private <- paste0(normalizePath(analysis_plan$root, winslash = "/", mustWork = TRUE), "/data/private/")
    destination <- normalizePath(path, winslash = "/", mustWork = FALSE)
    original_root <- paste0(analysis_plan$root, "/")
    if (startsWith(destination, original_root)) {
      destination <- paste0(normalizePath(analysis_plan$root, winslash = "/", mustWork = TRUE),
                            "/", substring(destination, nchar(original_root) + 1L))
    }
    if (!startsWith(destination, private) || ".." %in% strsplit(destination, "/", fixed = TRUE)[[1L]]) {
      stop("Real-data report inputs must use an explicit path under data/private; the public derived path is synthetic only.")
    }
  }
  values <- reduce_report_inputs(inputs)
  portable_receipt <- function(receipt) {
    receipt$cfg$root <- "."
    for (family in c("network", "reliability")) {
      if (!is.null(receipt$reused_resampling[[family]]$calculation_receipt)) {
        receipt$reused_resampling[[family]]$calculation_receipt$cfg$root <- "."
      }
    }
    receipt
  }
  receipt <- portable_receipt(calculation_receipt)
  for (family in c("network", "reliability")) {
    if (!is.null(values$report_provenance$reused_resampling[[family]]$calculation_receipt)) {
      values$report_provenance$reused_resampling[[family]]$calculation_receipt <-
        portable_receipt(values$report_provenance$reused_resampling[[family]]$calculation_receipt)
    }
  }
  bundle <- list(version = 1L, values = values, calculation_receipt = receipt,
                 values_sha256 = digest::digest(values, algo = "sha256"))
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(".report-inputs-", tmpdir = dirname(path))
  on.exit(unlink(temporary), add = TRUE)
  saveRDS(bundle, temporary, compress = "xz", version = 3)
  if (!file.rename(temporary, path)) stop("Could not install saved report inputs: ", path)
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

#' Read a portable snapshot; its source run keeps its recorded provenance
read_report_inputs <- function(path, root, expected_profile = Sys.getenv("ZM_PROFILE", "")) {
  if (!file.exists(path)) stop("Saved report inputs are absent: ", path, ". Run the analysis/export before report-only replay.")
  bundle <- readRDS(path)
  if (!identical(bundle$version, 1L) || !identical(names(bundle$values), report_input_names())) {
    stop("Saved report inputs have an unsupported or incomplete schema.")
  }
  if (!identical(bundle$values_sha256, digest::digest(bundle$values, algo = "sha256"))) {
    stop("Saved report inputs failed their integrity check.")
  }
  profiles <- c(bundle$values$pipeline_config$profile_name,
                bundle$calculation_receipt$cfg$profile_name, bundle$values$report_provenance$profile)
  if (length(profiles) != 3L || anyNA(profiles) || any(!nzchar(profiles)) ||
      length(unique(profiles)) != 1L) stop("Saved report inputs have absent or inconsistent calculation profiles.")
  if (nzchar(expected_profile) && !identical(profiles[[1L]], expected_profile)) {
    stop("Saved report inputs use profile '", profiles[[1L]], "', but '", expected_profile,
         "' was requested. Report-only replay cannot change the calculation profile.")
  }
  assert_reduced_report_inputs(bundle$values)
  bundle$values$pipeline_config$root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  bundle
}

#' Fetch one explicitly named report input without evaluating targets
read_report_input <- function(name, inputs) {
  key <- as.character(substitute(name))
  if (length(key) != 1L || !key %in% names(inputs$values)) stop("Unknown saved report input: ", paste(key, collapse = " "))
  inputs$values[[key]]
}

#' Compare available calculation inputs and scientific sources, never presentation files
check_report_input_freshness <- function(inputs, root) {
  receipt <- inputs$calculation_receipt
  sources <- receipt$source_files
  empty_files <- data.frame(path = character(), sha256 = character())
  if (is.null(sources) || !all(c("path", "sha256") %in% names(sources))) sources <- empty_files
  source_inputs <- receipt$input_files
  if (is.null(source_inputs) || !all(c("path", "sha256") %in% names(source_inputs))) source_inputs <- empty_files
  # Report prose, layout and display helpers may be edited without refitting.
  sources <- sources[grepl("^(_targets[.]R$|R/|config/|renv[.]lock$)", sources$path) &
    !grepl("^R/(report_|result_outputs[.]R$|pipeline_view|parameter_cards)", sources$path) &
    sources$path != "config/analysis_plan.yaml", , drop = FALSE]
  files <- unique(rbind(source_inputs, sources))
  state <- "unavailable"; changed <- character()
  if (nrow(files) && all(c("path", "sha256") %in% names(files))) {
    paths <- file.path(root, files$path)
    available <- file.exists(paths)
    hashes <- vapply(paths[available], function(path) digest::digest(file = path, algo = "sha256"), character(1))
    changed <- files$path[available][hashes != files$sha256[available]]
    state <- if (length(changed)) "outdated" else if (all(available)) "current" else "unavailable"
  }
  plan_path <- file.path(root, "config", "analysis_plan.yaml")
  if (file.exists(plan_path) && exists("zm_config", mode = "function")) {
    current <- tryCatch(zm_config(inputs$values$pipeline_config$profile_name, plan_path), error = function(e) NULL)
    scientific <- function(plan) plan[setdiff(names(plan), c("root", "meta", "report"))]
    if (is.null(current) && state != "outdated") state <- "unavailable" else if (!is.null(current) && (!isTRUE(all.equal(
      scientific(current), scientific(inputs$values$pipeline_config), check.attributes = FALSE)))) {
      state <- "outdated"
      changed <- unique(c(changed, "config/analysis_plan.yaml"))
    }
  } else if (state == "current") state <- "unavailable"
  list(state = state, outdated = if (state == "outdated") "report_inputs_file" else character(),
       other_outdated = character(), changed_files = changed,
       reason = if (state == "unavailable") "Some recorded calculation inputs or scientific sources are unavailable." else NULL)
}
