# AP4 CFA reporting core ------------------------------------------------------
#
# Pure extractors for publication-facing CFA diagnostics.  They operate on an
# already fitted lavaan object: no model is fitted and no pipeline target is
# read or rebuilt here.  Selection limits (for example, how many ranked local
# residuals to print) belong to the analysis plan and are deliberately absent.

#' Normalise optional CFA report metadata
#'
#' @param metadata `NULL`, a named character vector, or a data frame.
#' @param key Name of the code column.
#' @param label Name of the output label column.
#' @return A two-column tibble.
ap4_cfa_metadata <- function(metadata, key, label) {
  empty <- tibble::tibble(
    !!key := character(),
    !!label := character()
  )
  if (is.null(metadata)) return(empty)

  if (is.character(metadata) && !is.null(names(metadata))) {
    out <- tibble::tibble(
      !!key := as.character(names(metadata)),
      !!label := as.character(unname(metadata))
    )
  } else if (is.data.frame(metadata)) {
    label_source <- if (label %in% names(metadata)) {
      label
    } else if ("label" %in% names(metadata)) {
      "label"
    } else {
      key
    }
    out <- tibble::tibble(
      !!key := as.character(metadata[[key]]),
      !!label := as.character(metadata[[label_source]])
    )
  } else {
    stop("CFA report metadata must be NULL, a named character vector, or a data frame.")
  }

  out
}

#' Normalise model metadata for CFA report tables
#'
#' @param model_metadata `NULL`, a scalar model code, a named list, or a
#'   one-row data frame. Named inputs provide `model` and `model_label`.
#' @return A one-row tibble with `model` and `model_label`.
ap4_cfa_model_metadata <- function(model_metadata = NULL) {
  if (is.null(model_metadata)) {
    return(tibble::tibble(model = NA_character_, model_label = NA_character_))
  }
  if (is.character(model_metadata) && length(model_metadata) == 1L) {
    return(tibble::tibble(
      model = as.character(model_metadata),
      model_label = as.character(model_metadata)
    ))
  }
  if (is.list(model_metadata) && !is.data.frame(model_metadata)) {
    model_metadata <- as.data.frame(model_metadata, stringsAsFactors = FALSE)
  }
  if (!is.data.frame(model_metadata) || nrow(model_metadata) != 1L ||
      !"model" %in% names(model_metadata)) {
    stop("CFA model metadata must be NULL, a scalar code, or one row containing 'model'.")
  }
  tibble::tibble(
    model = as.character(model_metadata$model),
    model_label = as.character(model_metadata$model_label)
  )
}

ap4_cfa_attach_model <- function(x, model_metadata) {
  model <- ap4_cfa_model_metadata(model_metadata)
  if (nrow(x) == 0L) {
    x$model <- character()
    x$model_label <- character()
    return(x[, c("model", "model_label", setdiff(names(x), c("model", "model_label"))),
             drop = FALSE])
  }
  x$model <- rep(model$model, nrow(x))
  x$model_label <- rep(model$model_label, nrow(x))
  x[, c("model", "model_label", setdiff(names(x), c("model", "model_label"))), drop = FALSE]
}

ap4_cfa_add_label <- function(x, metadata, key, label) {
  lookup <- ap4_cfa_metadata(metadata, key, label)
  fallback <- as.character(x[[key]])
  if (nrow(lookup) == 0L) {
    x[[label]] <- fallback
  } else {
    matched <- match(x[[key]], lookup[[key]])
    value <- lookup[[label]][matched]
    unlabelled <- is.na(value) | !nzchar(value)
    if (any(unlabelled)) {
      stop(
        "CFA report metadata carries no '", label, "' for ", key, "(s): ",
        paste(unique(fallback[unlabelled]), collapse = ", "), "."
      )
    }
    x[[label]] <- value
  }
  x
}

#' Typed empty publication-facing CFA reporting object
#'
#' @param model_metadata Optional model code and label.
#' @param issue Optional error text explaining why no fit was available.
#' @return Named list of the typed empty tibbles `loadings`,
#'   `factor_correlations`, `residuals`, `ranked_residuals` and `ave`, and the
#'   `issues` tibble, which holds one `fit` error row when `issue` is given.
ap4_cfa_reporting_empty <- function(model_metadata = NULL, issue = NULL) {
  empty <- list(
    loadings = tibble::tibble(
      model = character(), model_label = character(),
      factor = character(), factor_label = character(),
      item = character(), item_label = character(),
      estimate_unstd = numeric(), se_unstd = numeric(),
      estimate_std = numeric(), se_std = numeric()
    ),
    factor_correlations = tibble::tibble(
      model = character(), model_label = character(),
      factor_1 = character(), factor_1_label = character(),
      factor_2 = character(), factor_2_label = character(),
      estimate_unstd = numeric(), se_unstd = numeric(),
      estimate_std = numeric(), se_std = numeric()
    ),
    residuals = tibble::tibble(
      model = character(), model_label = character(),
      item_1 = character(), item_1_label = character(),
      item_2 = character(), item_2_label = character(),
      bentler_residual = numeric(), std_residual = numeric(),
      abs_std_residual = numeric(), rank_abs_std_residual = integer()
    ),
    ranked_residuals = tibble::tibble(
      model = character(), model_label = character(),
      item_1 = character(), item_1_label = character(),
      item_2 = character(), item_2_label = character(),
      bentler_residual = numeric(), std_residual = numeric(),
      abs_std_residual = numeric(), rank_abs_std_residual = integer()
    ),
    ave = tibble::tibble(
      model = character(), model_label = character(),
      factor = character(), factor_label = character(), ave = numeric()
    )
  )
  empty$issues <- tibble::tibble(
    component = character(), severity = character(), message = character()
  )
  if (!is.null(issue) && length(issue) > 0L && !is.na(issue) && nzchar(issue)) {
    empty$issues <- ap4_cfa_attach_model(
      tibble::tibble(component = "fit", severity = "error", message = as.character(issue)),
      model_metadata
    )
  } else {
    empty$issues <- ap4_cfa_attach_model(empty$issues, model_metadata)
  }
  empty
}

#' Codebook metadata for publication-facing CFA tables
#'
#' @param codebook Codebook list from [zm_codebook()].
#' @param items Optional item codes to retain.
#' @param factors Optional scale/factor codes to retain.
#' @return A list with `items` and `factors` metadata tibbles.
ap4_cfa_codebook_metadata <- function(codebook, items = NULL, factors = NULL) {
  item_rows <- tibble::tibble(
    item = as.character(codebook$items$item_code),
    item_label = as.character(codebook$items$item_text_de)
  )
  factor_rows <- tibble::tibble(
    factor = as.character(codebook$scales$scale_key),
    factor_label = as.character(codebook$scales$label)
  )
  if (!is.null(items)) item_rows <- item_rows[item_rows$item %in% items, , drop = FALSE]
  if (!is.null(factors)) factor_rows <- factor_rows[factor_rows$factor %in% factors, , drop = FALSE]
  list(items = item_rows, factors = factor_rows)
}

#' Collect one component from CFA result rows
#'
#' @param ... CFA result tibbles carrying a `reporting` list-column.
#' @param component Name of one component of the per-model reporting list
#'   (`loadings`, `factor_correlations`, `residuals`, `ranked_residuals`, `ave`
#'   or `issues`).
#' @return Bound tibble of that component across every model.
ap4_cfa_collect_reporting <- function(..., component) {
  inputs <- list(...)
  rows <- unlist(lapply(inputs, function(x) {
    lapply(x$reporting, function(reporting) reporting[[component]])
  }), recursive = FALSE)
  if (length(rows) == 0L) return(tibble::tibble())
  dplyr::bind_rows(rows)
}

#' Write all pairwise CFA residuals to the configured data file
#'
#' The file is deliberately long rather than a wide p x p print matrix: it
#' remains machine-readable for the larger item sets and retains both the raw
#' Bentler correlation residual and its standardised counterpart.
#' When every model fails, the file contains the column headings and no rows.
#'
#' @param cfa_scales,cfa_sets The single-scale and multifactor CFA tables
#'   ([tabulate_cfa_scale_models()], [tabulate_cfa_set_models()]).
#' @param analysis_plan Configuration from [zm_config()].
#' @return Normalised path of the written CSV file.
write_full_cfa_residual_output <- function(cfa_scales, cfa_sets, analysis_plan) {
  path <- analysis_plan$factor_analysis$cfa_reporting$residual_data_file
  if (is.null(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    stop("AP4: analysis_plan$factor_analysis$cfa_reporting$residual_data_file is required.")
  }
  if (!grepl("^(/|~)", path)) path <- file.path(analysis_plan$root, path)
  residual_schema <- ap4_cfa_reporting_empty()$residuals
  reporting <- c(cfa_scales$reporting, cfa_sets$reporting)
  valid_residuals <- vapply(reporting, function(model) {
    is.list(model) && is.data.frame(model$residuals) &&
      identical(names(model$residuals), names(residual_schema)) &&
      identical(vapply(model$residuals, typeof, character(1)),
                vapply(residual_schema, typeof, character(1)))
  }, logical(1))
  if (!length(reporting) || !all(valid_residuals)) {
    stop("AP4: CFA residual tables must have the expected columns and types.")
  }
  residuals <- ap4_cfa_collect_reporting(cfa_scales, cfa_sets, component = "residuals")
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(residuals, path, na = "", progress = FALSE)
  normalizePath(path)
}

#' Read the participant count of the model-specific CFA sample
#'
#' Fit records preserve the sample after AP3 exclusions, including failed
#' fitting attempts. Lavaan's stored case counts identify the fitted sample.
#' The count is unavailable when neither source provides a usable sample size.
#'
#' @param fit_record One record returned by fit_cfa_models().
#' @return List with n and a note when the count is unavailable.
extract_cfa_participant_count <- function(fit_record) {
  valid_count <- function(x) is.numeric(x) && length(x) == 1L &&
    is.finite(x) && x >= 0 && x <= .Machine$integer.max && x == floor(x)
  if (valid_count(fit_record$n)) {
    return(list(n = as.integer(fit_record$n), note = NA_character_))
  }
  fitted_counts <- if (is.null(fit_record$fit)) NULL else tryCatch(
    lavaan::lavInspect(fit_record$fit, "nobs"), error = function(error) NULL)
  if (is.numeric(fitted_counts) && length(fitted_counts) &&
      all(vapply(as.list(fitted_counts), valid_count, logical(1))) &&
      valid_count(sum(fitted_counts))) {
    return(list(n = as.integer(sum(fitted_counts)), note = NA_character_))
  }
  list(n = NA_integer_, note = paste(
    "CFA participant count unavailable:",
    "the fit record and fitted model contain no usable sample size"))
}

# AP4 — the reporting boundary of the confirmatory factor structure -----------
#
# Reporting consumes the one empirical CFA target. It cannot change its model
# definitions, fits, convergence findings, or admissibility findings.

# BEGIN GENERATED PARAMETER CARD: AP4 CFA RESIDUALS
# Automatically generated from analysis_plan.yaml
#   Residual correlations shown per model: the 3 largest in absolute standardised value   (factor_analysis.cfa_reporting.ranked_residuals_n)
#   Descriptive; no fit cutoff and no pass/fail.
# END GENERATED PARAMETER CARD: AP4 CFA RESIDUALS
#' AP4 — assemble confirmatory factor structure reporting output
#'
#' Extract registered descriptions; preserve unavailable planned models.
#'
#' @param cfa_models The assessed confirmatory models (`cfa_models`).
#' @param analysis_plan Configuration from [zm_config()].
#' @return Named list of reporting tables: `model_status`, `fit_indices`,
#'   `loadings`, `ave`, `factor_correlations`,
#'   `largest_residual_correlations`, `full_residual_output` and
#'   `technical_supplement`.
build_cfa_reporting_data <- function(cfa_models, analysis_plan) {
  rows <- names(cfa_models$models) |>
    lapply(function(model_name) {
    question <- as.character(cfa_models$questions[[model_name]])
    model_definition <- cfa_models$models[[model_name]]
    fit_record <- cfa_models$fits[[model_name]]
    assessment <- cfa_models$assessment[[model_name]]
    participant_count <- extract_cfa_participant_count(fit_record)
    estimates <- if (is.null(fit_record$fit)) NULL else
      ap4_extract_cfa_evidence(
        fit_record$fit, model_name, model_definition$label)
    residuals <- if (is.null(fit_record$fit)) NULL else
      ap4_extract_and_rank_cfa_residuals(
        fit_record$fit, model_name)
    available <- isTRUE(assessment$converged) && isTRUE(assessment$admissible)
    list(
      model = model_name, question = question,
      n = participant_count$n, converged = assessment$converged,
      admissible = assessment$admissible,
      admissibility_note = assessment$note,
      status = if (available) "converged and admissible" else
        "planned model unavailable",
      main_report = if (available) list(
        fit_indices = estimates$fit_indices,       # descriptive; no cutoffs
        loadings = estimates$loadings,             # unstandardized + SE; std.all + SE
        ave = estimates$ave,                       # descriptive; no threshold
        factor_correlations = estimates$factor_correlations, # no threshold
        largest_residual_correlations = utils::head(
          residuals$ranked_by_absolute_standardized,
          analysis_plan$factor_analysis$cfa_reporting$ranked_residuals_n),
        full_residual_output = residuals$all_pairs) else NULL,
      technical_supplement = list(
        fixed_definition = model_definition,
        # The syntax recorded at fitting time, in executable column names,
        # rather than one rebuilt here from the source labels.
        syntax = fit_record$syntax,
        available_estimates = estimates,
        full_residual_output = if (is.null(residuals)) NULL else residuals$all_pairs,
        extraction_issues = if (is.null(estimates)) NULL else estimates$issues,
        residual_issue = if (is.null(residuals)) NA_character_ else residuals$issue,
        warnings = unique(c(fit_record$warnings, assessment$note, participant_count$note)),
        error = fit_record$error),
      required_follow_up = if (available) NA_character_ else
        "check coding and numerical estimation; document any technical correction")
  })
  ap4_assemble_cfa_reporting_tables(rows)
}

#' Extract exactly the preregistered versions of the CFA fit indices
#'
#' Missing requested names stay unavailable even when another version exists.
#' Values already recorded as NA are preserved.
#'
#' @param values Named fit measures returned by lavaan::fitMeasures().
#' @param model Model name attached to the reporting row.
#' @return List with the fit_indices tibble and an omitted-index issue, if any.
extract_cfa_fit_indices <- function(values, model) {
  missing_fit_indices <- character(0)
  pick <- function(name) {
    if (!name %in% names(values)) {
      missing_fit_indices <<- c(missing_fit_indices, name)
      NA_real_
    } else unname(as.numeric(values[name]))
  }
  result <- tibble::tibble(model = model,
    chisq = pick("chisq.scaled"),
    df = pick("df.scaled"), pvalue = pick("pvalue.scaled"),
    cfi = pick("cfi.robust"),
    tli = pick("tli.robust"),
    rmsea = pick("rmsea.robust"),
    rmsea_90_lower = pick("rmsea.ci.lower.robust"),
    rmsea_90_upper = pick("rmsea.ci.upper.robust"),
    srmr = pick("srmr"))
  list(fit_indices = result,
    issue = if (length(missing_fit_indices))
      paste("lavaan omitted requested fit index/indices:",
        paste(unique(missing_fit_indices), collapse = ", ")) else NA_character_)
}

#' Extract the registered descriptive evidence of one fitted CFA
#'
#' Extracts the registered descriptive fit indices, both loading metrics and
#' their standard errors, factor correlations, and AVE. Each component failure
#' or omitted requested fit index yields typed unavailable output plus an issue;
#' it applies no threshold.
#'
#' @param fit A fitted lavaan object.
#' @param model Model name.
#' @param label Publication label of the model.
#' @return List `model`, `label`, `fit_indices`, `loadings`,
#'   `factor_correlations`, `ave`, `issues`.
ap4_extract_cfa_evidence <- function(fit, model, label) {
  issues <- list()
  capture <- function(component, expression, unavailable) {
    tryCatch(expression, error = function(error) {
      issues[[component]] <<- conditionMessage(error)
      unavailable
    })
  }
  fit_indices <- capture("fit_indices", {
    extracted <- extract_cfa_fit_indices(lavaan::fitMeasures(fit), model)
    if (!is.na(extracted$issue)) issues[["fit_indices"]] <- extracted$issue
    extracted$fit_indices
  }, tibble::tibble())
  parameters <- capture("parameters", {
    unstandardized <- as.data.frame(lavaan::parameterEstimates(fit))
    standardized <- as.data.frame(lavaan::standardizedSolution(fit, type = "std.all"))
    keys <- intersect(c("lhs", "op", "rhs", "group", "block", "level"),
      intersect(names(unstandardized), names(standardized)))
    joined <- merge(unstandardized[c(keys, "est", "se")],
      stats::setNames(standardized[c(keys, "est.std", "se")],
        c(keys, "estimate_std", "se_std")), by = keys, sort = FALSE)
    latent <- lavaan::lavNames(fit, type = "lv")
    list(
      loadings = dplyr::filter(joined, .data$op == "=~") |>
        dplyr::transmute(model = model, factor = .data$lhs, item = .data$rhs,
          estimate_unstd = .data$est, se_unstd = .data$se,
          estimate_std = .data$estimate_std, se_std = .data$se_std),
      factor_correlations = dplyr::filter(joined, .data$op == "~~",
          .data$lhs != .data$rhs, .data$lhs %in% latent, .data$rhs %in% latent) |>
        dplyr::transmute(model = model, factor_1 = .data$lhs, factor_2 = .data$rhs,
          estimate_unstd = .data$est, se_unstd = .data$se,
          estimate_std = .data$estimate_std, se_std = .data$se_std))
  }, list(loadings = tibble::tibble(), factor_correlations = tibble::tibble()))
  ave <- capture("ave", {
    value <- semTools::AVE(fit, return.df = TRUE)
    if (is.data.frame(value) || is.matrix(value)) {
      value <- as.matrix(value)
      if (ncol(value) == 1L) tibble::tibble(
        model = model, factor = rownames(value), ave = as.numeric(value[, 1L]))
      else if (nrow(value) == 1L) tibble::tibble(
        model = model, factor = colnames(value), ave = as.numeric(value[1L, ]))
      else stop("AVE output does not identify one value per factor.")
    } else tibble::tibble(
      model = model, factor = names(value), ave = as.numeric(value))
  }, tibble::tibble())
  list(model = model, label = label, fit_indices = fit_indices,
    loadings = parameters$loadings,
    factor_correlations = parameters$factor_correlations,
    ave = ave, issues = tibble::enframe(unlist(issues),
      name = "component", value = "message"))
}

#' Extract and rank the pairwise residual correlations of one fitted CFA
#'
#' Extracts every unique item pair from lavaan's Bentler correlation residual
#' and elementwise standardized residual matrices, sorts a copy by absolute
#' standardized residual, and retains the full unsliced output. Failure yields
#' unavailable residual output plus an issue.
#'
#' @param fit A fitted lavaan object.
#' @param model Model name.
#' @return List `all_pairs`, `ranked_by_absolute_standardized`, `issue`.
ap4_extract_and_rank_cfa_residuals <- function(fit, model) {
  tryCatch({
    residual <- lavaan::lavResiduals(fit, type = "cor.bentler",
      summary = FALSE, elementwise = TRUE, zstat = TRUE)
    if (is.list(residual) && is.null(residual$cov) && length(residual) == 1L)
      residual <- residual[[1L]]
    pairs <- which(lower.tri(residual$cov, diag = FALSE), arr.ind = TRUE)
    all_pairs <- tibble::tibble(
      model = model, item_1 = rownames(residual$cov)[pairs[, "row"]],
      item_2 = colnames(residual$cov)[pairs[, "col"]],
      residual_correlation = as.numeric(residual$cov[pairs]),
      standardized_residual_correlation = as.numeric(residual$cov.z[pairs]))
    ranked <- all_pairs[order(abs(all_pairs$standardized_residual_correlation),
      decreasing = TRUE, na.last = TRUE), , drop = FALSE]
    list(all_pairs = all_pairs,
      ranked_by_absolute_standardized = ranked, issue = NA_character_)
  }, error = function(error) list(
    all_pairs = tibble::tibble(),
    ranked_by_absolute_standardized = tibble::tibble(),
    issue = conditionMessage(error)))
}

#' Turn the fourteen model records into the named reporting tables
#'
#' Preserves one explicit status row for every planned model. It does not
#' refit, reinterpret, filter, or change a model.
#'
#' @param rows The per-model reporting records.
#' @inherit build_cfa_reporting_data return
ap4_assemble_cfa_reporting_tables <- function(rows) {
  available <- function(row, component) {
    value <- row$main_report[[component]]
    if (is.null(value)) tibble::tibble() else value
  }
  list(
    model_status = dplyr::bind_rows(lapply(rows, function(row)
      tibble::tibble(model = row$model, question = row$question, n = row$n,
        converged = row$converged, admissible = row$admissible,
        admissibility_note = row$admissibility_note, status = row$status,
        required_follow_up = row$required_follow_up))),
    fit_indices = dplyr::bind_rows(lapply(rows, available, "fit_indices")),
    loadings = dplyr::bind_rows(lapply(rows, available, "loadings")),
    ave = dplyr::bind_rows(lapply(rows, available, "ave")),
    factor_correlations = dplyr::bind_rows(
      lapply(rows, available, "factor_correlations")),
    largest_residual_correlations = dplyr::bind_rows(
      lapply(rows, available, "largest_residual_correlations")),
    full_residual_output = dplyr::bind_rows(lapply(rows, function(row) {
      value <- row$technical_supplement$full_residual_output
      if (is.null(value) || !nrow(value)) tibble::tibble(
        model = row$model, item_1 = NA_character_, item_2 = NA_character_,
        residual_correlation = NA_real_,
        standardized_residual_correlation = NA_real_) else value
    })),
    technical_supplement = lapply(rows, `[[`, "technical_supplement")
  )
}
