# AP4 factor structure across scales (descriptive only)
# The confirmatory models of the factor-analytic sets configured in
# config/analysis_plan.yaml under `factor_analysis`: a set names the scales
# that are analysed together, and its model gives every intended scale one
# freely correlating factor.
#
# This file keeps:
#   1. the configured sets, read from the plan;
#   2. the confirmatory models of the sets (WLSMV, ordered items, std.lv).
#
# The exploratory route itself — item sets, polychoric correlations, the nine
# factor-number methods, the rotation specifications and every fitted solution
# — is in R/ap9_efa.R. The per-scale and per-set EFA
# tables the report reads are built from it in R/report_supplement_measurement.R.
# Each set uses complete cases over its own items, which AP3 has already
# reversed. These restrictions apply only to that set’s analyses; they do not
# remove participants from the study or select items on the basis of results.

# --- configuration -----------------------------------------------------------

#' The factor-analytic sets of the analysis plan
#'
#' @param analysis_plan Configuration from [zm_config()]; `analysis_plan$factor_analysis$sets` must
#'   exist.
#' @param kind `"efa"` or `"cfa"`: keep the sets whose flag of that name is true.
#'   `NULL` keeps every set.
#' @return List of set definitions (each a list with key, label, scales,
#'   expected_factors, efa, cfa).
ap4_factor_sets <- function(analysis_plan, kind = NULL) {
  sets <- analysis_plan$factor_analysis$sets
  if (is.null(sets) || length(sets) == 0) {
    stop("AP4: analysis_plan$factor_analysis$sets is missing from the analysis plan.")
  }
  keys <- vapply(sets, function(s) as.character(s$key), character(1))
  if (anyNA(keys) || any(!nzchar(keys)) || anyDuplicated(keys) > 0) {
    stop("AP4: every factor_analysis set needs a unique, non-empty 'key'.")
  }
  if (is.null(kind)) return(stats::setNames(sets, keys))
  kind <- match.arg(kind, c("efa", "cfa"))
  keep <- vapply(sets, function(s) isTRUE(s[[kind]]), logical(1))
  stats::setNames(sets[keep], keys[keep])
}

# AP4 — the preregistered confirmatory factor structure -----------------------
#
# Fourteen fixed models in five families: the nine one-factor scale models and
# the five multi-factor models. They are listed in `confirmatory_models` of
# config/analysis_plan.yaml; the verbs below read that list and add nothing to
# it. A model names its factors and, for every factor, the registered scale
# whose items load on it; the items are that scale's `item_codes` in the
# codebook, by the questionnaire SOURCE LABEL, and the executable data column is
# resolved once, at fitting time, through the codebook the fitting verb receives
# as an explicit argument.
# No result may add, remove, reassign, or generate an alternative model.

#' The planned confirmatory models of one family
#'
#' @param analysis_plan Configuration from [zm_config()];
#'   `analysis_plan$confirmatory_models$models` must exist.
#' @param family One of the families the plan names (`subscale`, `ums_dopl`,
#'   `asc`, `social_motives`, `auth_orientation`).
#' @return List of plan entries of that family, in the order the plan lists them.
ap4_planned_cfa_models <- function(analysis_plan, family) {
  planned <- analysis_plan$confirmatory_models$models
  if (!is.list(planned) || length(planned) == 0L) {
    stop("AP4: analysis_plan.yaml states no confirmatory_models$models.")
  }
  keep <- vapply(planned, function(entry) identical(as.character(entry$family), family), logical(1))
  if (!any(keep)) {
    stop("AP4: analysis_plan.yaml states no confirmatory model of family '", family, "'.")
  }
  planned[keep]
}

#' The source labels of one registered scale, in codebook order
#'
#' The scale codebook lists a scale's items by their executable column; a model
#' definition names them by their questionnaire SOURCE LABEL, which the
#' codebook's label-to-column map supplies. The resolution is checked in both
#' directions, so a definition can only carry labels that resolve back to the
#' codebook's own item set of that scale, in its order.
#'
#' @param scale_key Scale key of the codebook.
#' @param codebook Codebook from [zm_codebook()].
#' @return Character vector of item source labels.
ap4_cfa_scale_labels <- function(scale_key, codebook) {
  row <- match(scale_key, as.character(codebook$scales$scale_key))
  if (is.na(row)) {
    stop("AP4 confirmatory model: the codebook has no scale '", scale_key, "'.")
  }
  columns <- as.character(codebook$scales$item_codes[[row]])
  item_columns <- zm_item_column_map(codebook)
  labels <- names(item_columns)[match(columns, unname(item_columns))]
  if (anyNA(labels)) {
    stop("AP4 confirmatory model: scale '", scale_key, "' has item column(s) ",
         "without a source label: ", paste(columns[is.na(labels)], collapse = ", "), ".")
  }
  stopifnot(identical(zm_item_columns(labels, item_columns), columns))
  labels
}

#' AP4 — add the planned confirmatory models of one family to a model set
#'
#' @param cfa_model_set The CFA model set built so far.
#' @param analysis_plan Configuration from [zm_config()].
#' @param codebook Codebook from [zm_codebook()]; its scales supply the items of
#'   a factor and its label-to-column map validates them.
#' @param family The family of the plan whose models are added.
#' @return The set with those models appended, in the order the plan lists them.
ap4_define_cfa_models <- function(cfa_model_set, analysis_plan, codebook, family) {
  for (entry in ap4_planned_cfa_models(analysis_plan, family)) {
    key <- as.character(entry$key)
    if (!is.list(entry$factors) || length(entry$factors) == 0L) {
      stop("AP4 confirmatory model '", key, "': the plan names no factors.")
    }
    cfa_model_set$models[[key]] <- list(
      label = as.character(entry$label),
      factors = lapply(entry$factors, function(scale_key) {
        ap4_cfa_scale_labels(as.character(scale_key), codebook)
      }))
    cfa_model_set$questions[[key]] <- as.character(entry$question)
  }
  cfa_model_set
}

# BEGIN GENERATED PARAMETER CARD: AP4 CFA SUBSCALE
# Automatically generated from analysis_plan.yaml
#   zm_security = zm_security         "security"
#   zm_arousal = zm_arousal           "arousal"
#   zm_power = zm_power               "power"
#   zm_prestige = zm_prestige         "prestige"
#   zm_achievement = zm_achievement   "achievement"
#   asc_agg = asc_agg                 "Authoritarian aggression"
#   asc_sub = asc_sub                 "Authoritarian submission"
#   asc_conv = asc_conv               "Conventionalism"
#   sdo_dom = sdo_dom                 "Social dominance orientation: dominance"
# Items per factor: the codebook's item set of that scale, in codebook order.
# END GENERATED PARAMETER CARD: AP4 CFA SUBSCALE

#' AP4 — the one-factor models of the registered scales
#'
#' Each factor is one registered scale, its items load on it only, and factors correlate freely.
#'
#' @param data The CFA input (`data_cfa_input`).
#' @param analysis_plan Configuration from [zm_config()].
#' @param codebook Codebook from [zm_codebook()].
#' @return CFA model set: `question`, `data`, `models`, `questions`, empty `fits`
#'   and `assessment`.
define_subscale_models <- function(data, analysis_plan, codebook) {
  list(question = "confirmatory factor structure", data = data,
    models = list(), questions = list(), fits = NULL, assessment = NULL) |>
    ap4_define_cfa_models(analysis_plan, codebook, "subscale")
}

# BEGIN GENERATED PARAMETER CARD: AP4 CFA UMS DOPL
# Automatically generated from analysis_plan.yaml
#   ums_achievement_intimacy = zm_security + zm_achievement   "UMS-6 (intimacy and achievement)"
#   dopl_dominance_prestige = zm_power + zm_prestige          "DoPL-6 (dominance and prestige)"
# Items per factor: the codebook's item set of that scale, in codebook order.
# END GENERATED PARAMETER CARD: AP4 CFA UMS DOPL

#' AP4 — the two-factor UMS and DoPL models
#'
#' Each factor is one registered scale, its items load on it only, and factors correlate freely.
#'
#' @param cfa_model_set The CFA model set built so far.
#' @inheritParams define_subscale_models
#' @inherit define_subscale_models return
define_ums_dopl_models <- function(cfa_model_set, analysis_plan, codebook) {
  ap4_define_cfa_models(cfa_model_set, analysis_plan, codebook, "ums_dopl")
}

# BEGIN GENERATED PARAMETER CARD: AP4 CFA SOCIAL MOTIVES
# Automatically generated from analysis_plan.yaml
#   social_motives_five_factor = zm_security + zm_arousal + zm_power + zm_prestige + zm_achievement   "Five social-motive factors"
# Items per factor: the codebook's item set of that scale, in codebook order.
# END GENERATED PARAMETER CARD: AP4 CFA SOCIAL MOTIVES

#' AP4 — the five-factor social-motives model
#'
#' Each factor is one registered scale, its items load on it only, and factors correlate freely.
#'
#' @inheritParams define_ums_dopl_models
#' @inherit define_subscale_models return
define_social_motives_model <- function(cfa_model_set, analysis_plan, codebook) {
  ap4_define_cfa_models(cfa_model_set, analysis_plan, codebook, "social_motives")
}

# BEGIN GENERATED PARAMETER CARD: AP4 CFA ASC
# Automatically generated from analysis_plan.yaml
#   asc_three_factor = asc_agg + asc_sub + asc_conv   "ASC three-factor model"
# Items per factor: the codebook's item set of that scale, in codebook order.
# END GENERATED PARAMETER CARD: AP4 CFA ASC

#' AP4 — the three-factor ASC model
#'
#' Each factor is one registered scale, its items load on it only, and factors correlate freely.
#'
#' @inheritParams define_ums_dopl_models
#' @inherit define_subscale_models return
define_asc_model <- function(cfa_model_set, analysis_plan, codebook) {
  ap4_define_cfa_models(cfa_model_set, analysis_plan, codebook, "asc")
}

# BEGIN GENERATED PARAMETER CARD: AP4 CFA AUTH ORIENTATION
# Automatically generated from analysis_plan.yaml
#   authoritarian_orientation_four_factor = asc_agg + asc_sub + asc_conv + sdo_dom   "Three ASC factors and SDO-D"
# Items per factor: the codebook's item set of that scale, in codebook order.
# END GENERATED PARAMETER CARD: AP4 CFA AUTH ORIENTATION

#' AP4 — the four-factor authoritarian-orientation model
#'
#' Each factor is one registered scale, its items load on it only, and factors correlate freely.
#'
#' @inheritParams define_ums_dopl_models
#' @inherit define_subscale_models return
define_auth_orientation_model <- function(cfa_model_set, analysis_plan, codebook) {
  ap4_define_cfa_models(cfa_model_set, analysis_plan, codebook, "auth_orientation")
}

# BEGIN GENERATED PARAMETER CARD: AP4 CFA ESTIMATION
# Automatically generated from analysis_plan.yaml
#   Estimator: WLSMV   (factor_analysis.cfa_settings.estimator)
#   Items treated as ordered: yes   (factor_analysis.cfa_settings.ordered)
#   Latent variances fixed to 1: yes   (factor_analysis.cfa_settings.std_lv)
#   Factors orthogonal: no   (factor_analysis.cfa_settings.orthogonal)
# END GENERATED PARAMETER CARD: AP4 CFA ESTIMATION
#' AP4 — fit the preregistered confirmatory models
#'
#' Fit ordered-item WLSMV models without changing their definitions. The fitted
#' syntax and the fitted columns are kept on the fit record, so the technical
#' supplement can show the syntax that was estimated instead of rebuilding one
#' from the source labels.
#'
#' @param cfa_model_set The assembled definition set.
#' @param codebook Codebook from [zm_codebook()]; its label-to-column map
#'   resolves the fixed definitions to the executable data columns.
#' @param analysis_plan Configuration from [zm_config()].
#' @return The set with its `fits`.
fit_cfa_models <- function(cfa_model_set, codebook, analysis_plan) {
  cfa_model_set$fits <- cfa_model_set$models |>
    lapply(function(model_definition) {
      executable <- ap4_cfa_item_columns(model_definition, codebook)
      syntax <- ap4_build_lavaan_factor_syntax(executable$factors)
      sample <- exclude_rows_with_failed_imputations(cfa_model_set$data, executable$columns)
      fit_record <- ap4_capture_cfa_fit({
        require_valid_imputation_inputs(sample, executable$columns)
        lavaan::cfa(
          model = syntax,
          data = sample[executable$columns],
          ordered = analysis_plan$factor_analysis$cfa_settings$ordered,       # every supplied indicator column is ordinal
          estimator = analysis_plan$factor_analysis$cfa_settings$estimator,
          std.lv = analysis_plan$factor_analysis$cfa_settings$std_lv,       # every latent variance fixed to 1
          orthogonal = analysis_plan$factor_analysis$cfa_settings$orthogonal   # multi-factor correlations freely estimated
        )
      })
      fit_record$n <- nrow(sample)
      fit_record$syntax <- syntax
      fit_record$columns <- executable$columns
      fit_record$excluded_imputation <- attr(sample, "excluded_imputation")
      fit_record
    })
  cfa_model_set
}

#' AP4 — assess the preregistered confirmatory models
#'
#' Apply the registered convergence and admissibility rule without model
#' selection.
#'
#' @param cfa_fit_results The fitted definition set.
#' @return The set with its `assessment`.
assess_cfa_models <- function(cfa_fit_results) {
  cfa_fit_results$assessment <- cfa_fit_results$fits |>
    lapply(function(one_fit_result) {
      if (!one_fit_result$succeeded) {
        return(list(
          converged = NA,
          admissible = NA,
          note = one_fit_result$error
        ))
      }
      converged <- isTRUE(lavaan::lavInspect(one_fit_result$fit, "converged"))
      admissibility <- ap4_check_cfa_admissibility(one_fit_result$fit)
      note <- ap4_combine_cfa_notes(
        if (!converged) "model did not converge",
        admissibility$note
      )
      list(
        converged = converged,
        admissible = admissibility$admissible,
        note = note)
    })
  cfa_fit_results
}

# Technical helpers of the confirmatory models --------------------------------

#' Translate fixed factor definitions into lavaan loading syntax
#'
#' The generator has already validated every fixed factor and its ordered item
#' mapping. This helper only translates those lists into lavaan loading syntax;
#' it adds no loading, covariance, factor, or alternative model.
#'
#' @param factors Named list of item vectors, one per factor.
#' @return lavaan model syntax.
ap4_build_lavaan_factor_syntax <- function(factors) {
  paste(vapply(names(factors), function(factor) {
    paste0(factor, " =~ ", paste(factors[[factor]], collapse = " + "))
  }, character(1)), collapse = "\n")
}

#' Executable item columns of one fixed CFA definition
#'
#' Resolves the fixed factor definitions from their source labels to the
#' executable data columns through the codebook's explicit label-to-column map,
#' flattens them, removes duplicate column names while preserving their declared
#' order, and makes no item-selection decision.
#'
#' @param model_definition One fixed model definition.
#' @param codebook Codebook from [zm_codebook()].
#' @return List `factors` (the resolved per-factor columns) and `columns`.
ap4_cfa_item_columns <- function(model_definition, codebook) {
  factors <- lapply(model_definition$factors, zm_item_columns,
    item_columns = zm_item_column_map(codebook))
  list(
    factors = factors,
    columns = unique(unname(unlist(factors, use.names = FALSE))))
}

#' Capture one lavaan fit call with its warnings and its error
#'
#' Captures and muffles lavaan warnings, retains an exact fitting error, and
#' records whether the fit call technically succeeded. `succeeded` does not mean
#' converged or admissible; this helper performs no assessment or reporting.
#'
#' @param expression The fit call, forced here.
#' @return List `succeeded`, `fit`, `warnings`, `error`.
ap4_capture_cfa_fit <- function(expression) {
  warnings <- character(0)
  result <- tryCatch(
    withCallingHandlers(force(expression), warning = function(warning) {
      warnings <<- c(warnings, conditionMessage(warning))
      invokeRestart("muffleWarning")
    }),
    error = identity)
  list(
    succeeded = !inherits(result, "error"),
    fit = if (inherits(result, "error")) NULL else result,
    warnings = unique(warnings),
    error = if (inherits(result, "error"))
      conditionMessage(result) else NA_character_)
}

#' Join the technical notes of one assessed model
#'
#' Removes absent and empty technical notes and joins the remaining messages.
#' It adds no assessment rule.
#'
#' @param ... Note texts.
#' @return One character scalar, or `NA_character_`.
ap4_combine_cfa_notes <- function(...) {
  notes <- unlist(list(...), use.names = FALSE)
  notes <- notes[!is.na(notes) & nzchar(notes)]
  if (!length(notes)) return(NA_character_)
  paste(notes, collapse = "; ")
}

#' The registered admissibility conditions of one fitted model
#'
#' Checks finite standardized loadings within bounds, finite nonnegative
#' variances, and every latent covariance matrix for positive definiteness.
#' Convergence is assessed by the caller. It returns reasons and never modifies
#' the fit.
#'
#' @param fit A fitted lavaan object.
#' @param tolerance Numerical tolerance of the bounds.
#' @return List `admissible` and `note`.
ap4_check_cfa_admissibility <- function(fit, tolerance = 1e-6) {
  tryCatch({
    notes <- character(0)
    standardized <- lavaan::standardizedSolution(fit, type = "std.all")
    loadings <- standardized[standardized$op == "=~", , drop = FALSE]
    variances <- standardized[standardized$op == "~~" &
      standardized$lhs == standardized$rhs, , drop = FALSE]
    if (!nrow(loadings) || any(!is.finite(loadings$est.std)) ||
        any(abs(loadings$est.std) > 1 + tolerance))
      notes <- c(notes, "standardized loadings unavailable or outside [-1, 1]")
    if (any(!is.finite(variances$est.std)) ||
        any(variances$est.std < -tolerance))
      notes <- c(notes, "variances unavailable or negative")
    latent_covariance <- lavaan::lavInspect(fit, "cov.lv")
    matrices <- if (is.matrix(latent_covariance)) list(latent_covariance)
      else latent_covariance
    positive_definite <- length(matrices) && all(vapply(matrices, function(x) {
      is.matrix(x) && nrow(x) && all(is.finite(x)) &&
        min(eigen(x, symmetric = TRUE, only.values = TRUE)$values) > 0
    }, logical(1)))
    if (!positive_definite)
      notes <- c(notes, "latent covariance matrix is not positive definite")
    list(admissible = !length(notes),
      note = if (length(notes)) paste(notes, collapse = "; ") else NA_character_)
  }, error = function(error) list(admissible = FALSE,
    note = paste("admissibility check failed:", conditionMessage(error))))
}
