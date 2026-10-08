# Current AP3 processing facts, available independently of fitted-result targets.

#' Save the filled-cell count and imputation availability used in sample reporting
summarise_imputation_processing <- function(reporting_data) {
  models <- reporting_data$model_status
  list(n_filled = nrow(reporting_data$imputed_cells),
       failed = has_failed_imputation(reporting_data),
       has_models = !is.null(models) && nrow(models) > 0L)
}

describe_imputation_processing <- function(reporting_data, facts = NULL) {
  if (is.null(facts)) facts <- summarise_imputation_processing(reporting_data)
  n_filled <- facts$n_filled
  filled_count <- paste0(n_filled, if (n_filled == 1L) " missing value was" else " missing values were", " filled.")
  if (facts$failed) {
    remaining <- reporting_data$n_unfilled
    paste0(filled_count, " ", remaining, if (remaining == 1L) " value remains" else " values remain",
      " missing because a required imputation fit or an individual prediction was unavailable. ",
      "Respondents with unresolved values were excluded from analyses requiring those values.")
  } else if (n_filled == 0L) {
    "No value had to be filled."
  } else if (!facts$has_models) {
    paste(filled_count, "Model validity metadata are unavailable; AP3 imputation processing needs to be refreshed.")
  } else paste0(sub(" filled[.]$", "", filled_count),
    " filled using imputation models that passed the AP3 validity gate. ",
    "The completed values are treated as fixed in subsequent analyses.")
}

tabulate_imputation_analysis_availability <- function(reporting_data, codebook, analysis_plan) {
  definitions <- list()
  add <- function(label, variables, population = "all") {
    definitions[[length(definitions) + 1L]] <<- list(label = label, variables = variables, population = population)
  }
  scales <- as.character(codebook$scales$scale_key)
  labels <- stats::setNames(as.character(codebook$scales$label), scales)
  for (i in seq_along(scales)) {
    add(paste("Scale distribution:", labels[[scales[i]]]), scales[i])
    add(paste("Reliability and single-scale factor models:", labels[[scales[i]]]),
      codebook$scales$item_codes[[i]])
  }
  for (set in analysis_plan$factor_analysis$sets) {
    keys <- as.character(unlist(set$scales))
    columns <- unlist(codebook$scales$item_codes[match(keys, scales)], use.names = FALSE)
    add(paste("Combined factor models:", set$label), columns)
  }
  for (outcome in as.character(analysis_plan$regression$outcomes)) {
    add(paste("Adjusted regression:", labels[[outcome]]),
      c(outcome, as.character(analysis_plan$regression$motives), "age", "income"), population = "known_gender")
  }
  add("Joint regression and joint comparisons", c(as.character(analysis_plan$regression$outcomes),
    as.character(analysis_plan$regression$motives), "age", "income"), population = "known_gender")
  add("Network analyses", as.character(analysis_plan$network$nodes))
  pairs <- utils::combn(scales, 2L)
  for (i in seq_len(ncol(pairs))) {
    add(paste("Zero-order correlations:", paste(labels[pairs[, i]], collapse = " / ")), pairs[, i])
  }
  add("Age description", "age")
  add("Income-per-person description", "income")
  dplyr::bind_rows(lapply(definitions, function(definition) {
    sources <- unique(unlist(lapply(definition$variables, function(variable) {
      if (variable %in% scales) return(codebook$scales$item_codes[[match(variable, scales)]])
      switch(variable, age = "demo_age", income = c("demo_hh_members", "demo_income_hh_net"), variable)
    }), use.names = FALSE))
    participants <- reporting_data$participants
    if (definition$population == "known_gender") participants <- participants[participants$known_gender, , drop = FALSE]
    cells <- reporting_data$unfilled_cells
    cells <- cells[cells$variable %in% sources & cells$respondent_id %in% participants$respondent_id, , drop = FALSE]
    excluded <- unique(cells$respondent_id)
    n_used <- nrow(participants) - length(excluded)
    tibble::tibble(analysis = definition$label,
      status = if (n_used == 0L) "Not performed: no eligible respondents" else if (length(excluded))
        "Available after AP3 exclusions" else "Not withheld by AP3",
      n_input = nrow(participants), n_excluded = length(excluded), n_eligible = n_used,
      unavailable_variables = paste(unique(cells$variable), collapse = ", "))
  }))
}

has_failed_imputation <- function(reporting_data) {
  if (!is.null(reporting_data$n_unfilled)) return(reporting_data$n_unfilled > 0L)
  models <- reporting_data$model_status
  !is.null(models) && any(models$status %in% c("failed", "blocked"))
}

render_imputation_processing_html <- function(reporting_data, analysis_availability) {
  escape <- function(x) {
    x <- as.character(x); x[is.na(x)] <- "unavailable"
    for (pair in list(c("&", "&amp;"), c("<", "&lt;"), c(">", "&gt;"), c('"', "&quot;"))) {
      x <- gsub(pair[1], pair[2], x, fixed = TRUE)
    }
    x
  }
  table <- function(data) {
    if (!nrow(data)) return(if (nrow(reporting_data$imputed_cells) == 0L)
      "<p>No imputation model was required.</p>" else
      "<p>Model validity metadata are unavailable; AP3 imputation processing needs to be refreshed.</p>")
    rows <- apply(as.data.frame(lapply(data, as.character)), 1L, function(row) {
      paste0("<tr><td>", paste(escape(row), collapse = "</td><td>"), "</td></tr>")
    })
    paste0("<table><thead><tr><th>", paste(escape(names(data)), collapse = "</th><th>"),
      "</th></tr></thead><tbody>", paste(rows, collapse = ""), "</tbody></table>")
  }
  models <- reporting_data$model_status
  model_columns <- c("model", "kind", "n_missing", "n_training", "n_eligible", "n_blocked",
    "n_filled", "n_unfilled", "status", "note", "attempts", "parameter_count",
    "ess_bulk_min", "ess_tail_min", "rhat_max", "n_divergent", "n_treedepth_hits", "bfmi_min")
  if (is.null(models)) {
    models <- create_empty_imputation_model_status()
    models$n_filled <- integer(); models$n_unfilled <- integer()
  }
  model_table <- models[model_columns]
  history <- if (!is.null(models) && nrow(models)) dplyr::bind_rows(lapply(seq_len(nrow(models)), function(i) {
    entries <- models$retry_history[[i]]
    if (!nrow(entries)) return(NULL)
    cbind(model = models$model[i], entries)
  })) else tibble::tibble()
  blocked_predictions <- reporting_data$blocked_predictions
  title <- if (has_failed_imputation(reporting_data)) "Analysis report: unresolved AP3 values" else "AP3 imputation processing"
  paste0("<h1>", title, "</h1><p>", escape(describe_imputation_processing(reporting_data)), "</p>",
    if (has_failed_imputation(reporting_data)) paste0(
      "<p>This is the current processing-status report. Previous fitted results are not shown. ",
      "The table shows the respondents eligible for each analysis after excluding its unresolved values; ",
      "eligibility is not a claim that a fit completed.</p>") else "",
    "<h2>Imputation model status</h2>",
    "<p>Eligible predictions have all required predictors and an available training sample. ",
    "Blocked predictions lack a required predictor or an available training sample. ",
    "Filled and unfilled counts partition the original missing cells.</p>", table(model_table),
    if (nrow(history)) paste0("<h2>Numerical attempts</h2>", table(history)) else "",
    if (!is.null(blocked_predictions) && nrow(blocked_predictions)) paste0(
      "<h2>Unfilled individual predictions</h2>", table(blocked_predictions)) else "",
    "<h2>Analysis availability</h2>", table(analysis_availability))
}

#' Write a report even when affected fitted-result targets cannot complete
#'
#' On an AP3 failure, replace the generated full-results HTML with the current
#' failure report. A successful report from an earlier run must not appear to
#' describe the new input. No manuscript or fitted-data file is modified.
write_imputation_processing_report <- function(reporting_data, codebook, analysis_plan) {
  availability <- tabulate_imputation_analysis_availability(reporting_data, codebook, analysis_plan)
  fragment <- render_imputation_processing_html(reporting_data, availability)
  document <- paste0('<!doctype html><html lang="en"><head><meta charset="utf-8">',
    '<title>AP3 imputation processing</title><style>body{font-family:system-ui,sans-serif;',
    'max-width:1500px;margin:2rem auto;padding:0 2rem}table{border-collapse:collapse;',
    'font-size:.85rem}th,td{padding:.4rem;border:1px solid #ddd;text-align:left}',
    'h2{margin-top:2rem}</style></head><body>', fragment, '</body></html>')
  path <- file.path(analysis_plan$root, "report", "imputation-processing.html")
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  writeLines(document, path)
  if (has_failed_imputation(reporting_data)) {
    writeLines(document, file.path(analysis_plan$root, "report", "results_draft.html"))
  }
  path
}
