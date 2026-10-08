# Compact saved results accompanying the synthetic-data analysis.

#' Compare saved primary coefficients with their matching generating values
#'
#' Sources the existing comparison helper without running its command-line entry.
#' Every generating cell must have a fitted term; renamed or unmatched inputs
#' stop the comparison instead of silently appearing as missing estimates.
compare_saved_synthetic_coefficients <- function(primary_coefficients, generating_values,
                                                  latent_scores, analysis_plan) {
  helpers <- new.env(parent = environment())
  sys.source(file.path(analysis_plan$root, "scripts", "check_synthetic_recovery.R"), envir = helpers)
  generating <- order_generating_rows(tabulate_generating_coefficients(generating_values), analysis_plan)
  male_sd <- stats::sd(latent_scores$male)
  if (!is.finite(male_sd) || male_sd <= 0) stop("The generated gender indicator needs a positive finite SD.")
  comparison <- helpers$compare_estimates_with_generating_values(primary_coefficients, generating, male_sd)
  if (anyNA(comparison$term)) {
    stop("Generating values do not match the saved coefficient terms; use matching inputs from the saved run.")
  }
  comparison
}

#' Preserve named quantities and additional table attributes in JSON
prepare_nested_result_export <- function(value) {
  if (is.data.frame(value)) {
    extra <- attributes(value)[setdiff(names(attributes(value)), c("names", "row.names", "class"))]
    if (length(extra)) return(list(rows = value, attributes = lapply(extra, prepare_nested_result_export)))
    return(value)
  }
  if (is.matrix(value)) return(list(values = unname(value), dimnames = dimnames(value)))
  if (is.list(value)) return(lapply(value, prepare_nested_result_export))
  if (is.factor(value)) return(as.character(value))
  if (!is.null(names(value))) return(lapply(as.list(value), prepare_nested_result_export))
  value
}

#' Export small saved summaries for the synthetic analysis
#'
#' No target is built and no fit or draw is read. The receipt is the supplied
#' saved receipt; export time does not replace its build time. Its machine-local
#' cfg$root is replaced by "." and this transformation is recorded in the manifest.
#' @return Character vector of the written file paths, for a file target.
write_synthetic_review_exports <- function(data_source, coefficient_summaries,
                                            prior_width_sensitivity, prediction_decisions,
                                            planted_checks, generating_comparison,
                                            calculation_receipt, analysis_plan,
                                            output_dir = file.path(analysis_plan$root, "data", "derived", "synthetic_review"),
                                            variable_dictionary = NULL, generating_values = NULL) {
  if (!identical(data_source, "synthetic")) stop("Compact synthetic exports require data_source = 'synthetic'.")
  tables <- list(coefficients = coefficient_summaries, prediction_decisions = prediction_decisions,
                 generating_comparison = generating_comparison)
  if (!is.null(variable_dictionary)) {
    dictionary_columns <- intersect(c("key", "scale_key", "z_col", "scale", "subscale", "label", "short_label", "order"), names(variable_dictionary))
    if (!length(dictionary_columns)) stop("Variable dictionary needs labelled atomic columns.")
    tables$variable_dictionary <- variable_dictionary[, dictionary_columns, drop = FALSE]
  }
  if (!all(vapply(tables, is.data.frame, logical(1)))) stop("Coefficient, decision and comparison exports must be tables.")
  if (any(vapply(tables, function(x) any(vapply(x, is.list, logical(1))), logical(1)))) {
    stop("CSV exports cannot contain list columns.")
  }
  receipt_fields <- c("cfg", "input_files", "source_files", "hash_algorithm", "git_head", "git_dirty", "built_at", "r_version")
  if (!all(receipt_fields %in% names(calculation_receipt))) stop("The saved calculation receipt is incomplete.")
  if (is.null(calculation_receipt$cfg$profile_name)) stop("The saved calculation receipt needs its run profile.")
  receipt <- calculation_receipt
  receipt$cfg$root <- "."
  nested <- list(prior_width_sensitivity = prior_width_sensitivity,
                 planted_checks = planted_checks, calculation_receipt = receipt)
  if (!is.null(generating_values)) nested$generating_values <- generating_values
  descriptions <- c(
    variable_dictionary = "Variable keys and readable labels from the codebook used for the saved results.",
    generating_values = "Resolved generating scenario used in the comparison, including coefficient keys and values.",
    coefficients = "Saved regression coefficient summaries, including primary and prior-width fits; role, slope_sd and fit_valid identify each result.",
    prediction_decisions = "Saved primary-prior directional classifications, with computational validity and predicted signs.",
    generating_comparison = "Saved primary estimates compared with latent generating coefficients; observed item-average estimands can differ. Gender generating coefficients are converted to the fitted contrast scale.",
    prior_width_sensitivity = "Saved coefficient medians, credible intervals and direction/interval-exclusion stability at each prior width.",
    planted_checks = "Saved synthetic design and checks of planted exclusions, missingness, cross-loadings, heavy tails and analysis contingencies.",
    calculation_receipt = "Saved run settings, source/input fingerprints, revision, dirty-state flag, build time and R version; cfg.root alone is replaced by a portable relative root."
  )
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  files <- c(stats::setNames(paste0(names(tables), ".csv"), names(tables)),
             stats::setNames(paste0(names(nested), ".json"), names(nested)))
  for (key in names(tables)) readr::write_csv(tables[[key]], file.path(output_dir, files[[key]]), na = "", progress = FALSE)
  write_json <- function(value, path) jsonlite::write_json(prepare_nested_result_export(value), path,
    auto_unbox = TRUE, pretty = TRUE, digits = NA, na = "null", null = "null", dataframe = "rows")
  for (key in names(nested)) write_json(nested[[key]], file.path(output_dir, files[[key]]))
  manifest <- list(
    schema_version = 1L, data_source = "synthetic",
    saved_run = list(profile = receipt$cfg$profile_name, built_at = receipt$built_at,
                     git_head = receipt$git_head, git_dirty = receipt$git_dirty),
    provenance_transform = "calculation_receipt.cfg.root replaced by '.'; saved fingerprints and other receipt fields retained",
    encoding = list(csv_missing = "empty field", json_missing = "null", named_vectors = "JSON objects",
                    table_attributes = "Tables with additional attributes are encoded as rows and attributes"),
    files = lapply(names(files), function(key) list(file = unname(files[[key]]),
      type = if (key %in% names(tables)) "csv" else "json",
      rows = if (key %in% names(tables)) nrow(tables[[key]]) else NULL,
      columns = if (key %in% names(tables)) names(tables[[key]]) else NULL,
      column_types = if (key %in% names(tables)) vapply(tables[[key]], function(x) class(x)[1L], character(1)) else NULL,
      definition = unname(descriptions[[key]]),
      sha256 = digest::digest(file = file.path(output_dir, files[[key]]), algo = "sha256")))
  )
  write_json(manifest, file.path(output_dir, "manifest.json"))
  normalizePath(file.path(output_dir, c(unname(files), "manifest.json")), mustWork = TRUE)
}
