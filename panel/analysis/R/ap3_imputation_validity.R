# AP3 numerical validity and failure propagation. Ordinary AP6 regression
# diagnostics retain their existing b-parameter scope.

#' Diagnose every sampled parameter used by an imputation model
#'
#' Includes ordinal thresholds, residual scales and item/respondent effects,
#' rather than only population slopes. Log density and log prior quantities
#' are not prediction parameters. Missing diagnostics fail the gate.
extract_imputation_diagnostics <- function(fit, analysis_plan) {
  draws <- posterior::as_draws_array(fit)
  # brms saves fixed distributional quantities too (e.g. ordinal disc = 1).
  # Identify them from the model, not from empirical zero variance: estimated
  # parameters with degenerate draws must still fail diagnostics.
  terms <- brms::brmsterms(fit$formula)
  fixed_parameters <- names(terms$fdpars)
  variables <- setdiff(posterior::variables(draws), c("lp__", "lprior", fixed_parameters))
  if (!length(variables)) stop("AP3: no imputation parameters available for diagnostics.")
  summaries <- suppressMessages(posterior::summarise_draws(
    posterior::subset_draws(draws, variable = variables),
    ess_bulk = posterior::ess_bulk, ess_tail = posterior::ess_tail,
    rhat = posterior::rhat))
  gate <- ap6_gate_settings(analysis_plan$regression)
  minimum <- function(x) if (!length(x) || any(!is.finite(x))) NA_real_ else min(x)
  maximum <- function(x) if (!length(x) || any(!is.finite(x))) NA_real_ else max(x)
  nuts <- tryCatch(brms::nuts_params(fit), error = identity)
  sampler <- if (inherits(nuts, "error")) {
    list(n_divergent = NA_integer_, n_treedepth_hits = NA_integer_, bfmi_min = NA_real_)
  } else ap6_sampler_summary(nuts, ap6_max_treedepth(fit), posterior::nchains(draws))
  bulk <- minimum(summaries$ess_bulk); tail <- minimum(summaries$ess_tail)
  rhat <- maximum(summaries$rhat)
  flags <- c(
    ess_ok = is.finite(bulk) && is.finite(tail) && bulk >= gate$ess_target && tail >= gate$ess_target,
    rhat_ok = is.finite(rhat) && rhat <= gate$rhat_max,
    divergences_ok = !is.na(sampler$n_divergent) && sampler$n_divergent <= gate$divergences_max,
    treedepth_ok = !is.na(sampler$n_treedepth_hits) && sampler$n_treedepth_hits <= gate$treedepth_hits_max,
    bfmi_ok = is.finite(sampler$bfmi_min) && sampler$bfmi_min >= gate$bfmi_min)
  list(ok = all(flags), ess_ok = unname(flags["ess_ok"]),
    sampler_retry_needed = !all(flags[c("rhat_ok", "divergences_ok", "treedepth_ok")]),
    ess_bulk_min = bulk, ess_tail_min = tail, rhat_max = rhat,
    n_divergent = sampler$n_divergent, n_treedepth_hits = sampler$n_treedepth_hits,
    bfmi_min = sampler$bfmi_min, parameter_count = length(variables),
    parameters = summaries,
    note = if (all(flags)) "ok" else paste(names(flags)[!flags], collapse = "; "))
}

#' Apply AP6's ordered numerical remedies before predicting missing values
#'
#' The initial fit expression is evaluated here so fitting errors are recorded
#' beside diagnostic failures. Updates preserve model, predictors and priors.
gate_imputation_fit <- function(fit_expression, sampling, analysis_plan) {
  fit <- tryCatch(force(fit_expression), error = identity)
  history <- tibble::tibble(attempt = integer(), reason = character(), warmup = integer(),
    iter_per_chain = integer(), adapt_delta = numeric(), max_treedepth = integer(),
    ok = logical(), note = character())
  warmup <- sampling$warmup
  draws_per_chain <- sampling$iter - sampling$warmup
  control <- NULL
  gate <- ap6_gate_settings(analysis_plan$regression)
  diagnose <- function() {
    if (inherits(fit, "error")) return(list(ok = FALSE, ess_ok = FALSE,
      sampler_retry_needed = FALSE, note = conditionMessage(fit)))
    tryCatch(extract_imputation_diagnostics(fit, analysis_plan), error = function(error) {
      list(ok = FALSE, ess_ok = FALSE, sampler_retry_needed = FALSE,
        note = paste("diagnostics unavailable:", conditionMessage(error)))
    })
  }
  record <- function(reason, diagnostics) {
    history <<- tibble::add_row(history, attempt = nrow(history) + 1L, reason = reason,
      warmup = as.integer(warmup), iter_per_chain = as.integer(draws_per_chain),
      adapt_delta = if (is.null(control)) NA_real_ else control$adapt_delta,
      max_treedepth = if (is.null(control)) NA_integer_ else as.integer(control$max_treedepth),
      ok = isTRUE(diagnostics$ok), note = diagnostics$note)
  }
  refit <- function() {
    arguments <- list(object = fit, recompile = FALSE, chains = sampling$chains,
      cores = sampling$cores, seed = sampling$seed, iter = warmup + draws_per_chain,
      warmup = warmup, thin = sampling$thin, refresh = sampling$refresh,
      silent = sampling$silent)
    if (!is.null(control)) arguments$control <- control
    tryCatch(do.call(stats::update, arguments), error = identity)
  }
  diagnostics <- diagnose()
  record("initial fit", diagnostics)
  doublings <- 0L
  while (!inherits(fit, "error") && !isTRUE(diagnostics$ess_ok) &&
         doublings < gate$max_ess_doublings) {
    doublings <- doublings + 1L
    draws_per_chain <- draws_per_chain * 2L
    fit <- refit()
    diagnostics <- diagnose()
    record("ESS shortfall: post-warmup iterations doubled", diagnostics)
  }
  if (!inherits(fit, "error") && isTRUE(diagnostics$sampler_retry_needed)) {
    warmup <- warmup * 2L
    control <- list(adapt_delta = gate$adapt_delta_retry,
      max_treedepth = gate$max_treedepth_retry)
    fit <- refit()
    diagnostics <- diagnose()
    record("R-hat / divergences / tree depth: doubled warmup and retry controls", diagnostics)
  }
  status <- if (!isTRUE(diagnostics$ok)) "failed" else if (nrow(history) > 1L) "retried_ok" else "ok"
  list(fit = if (isTRUE(diagnostics$ok)) fit else NULL, status = status,
    diagnostics = diagnostics, history = history)
}

create_empty_imputation_model_status <- function() {
  tibble::tibble(model = character(), kind = character(), n_missing = integer(),
    n_training = integer(), n_eligible = integer(), n_blocked = integer(),
    status = character(), note = character(), attempts = integer(),
    ess_bulk_min = numeric(), ess_tail_min = numeric(), rhat_max = numeric(),
    n_divergent = integer(), n_treedepth_hits = integer(), bfmi_min = numeric(),
    parameter_count = integer(), variables = list(), retry_history = list(), parameters = list())
}

record_imputation_model_status <- function(data, result, model, kind, variables, n_missing,
                                          cases = NULL) {
  diagnostics <- result$diagnostics
  value <- function(name, missing = NA_real_) {
    if (is.null(diagnostics[[name]])) missing else diagnostics[[name]]
  }
  row <- tibble::tibble(model = as.character(model), kind = kind, n_missing = as.integer(n_missing),
    n_training = if (is.null(cases)) NA_integer_ else as.integer(cases$n_training),
    n_eligible = if (is.null(cases)) as.integer(n_missing) else as.integer(cases$n_eligible),
    n_blocked = if (is.null(cases)) 0L else as.integer(nrow(cases$blocked_predictions)),
    status = result$status, note = diagnostics$note, attempts = as.integer(nrow(result$history)),
    ess_bulk_min = value("ess_bulk_min"), ess_tail_min = value("ess_tail_min"),
    rhat_max = value("rhat_max"), n_divergent = as.integer(value("n_divergent")),
    n_treedepth_hits = as.integer(value("n_treedepth_hits")), bfmi_min = value("bfmi_min"),
    parameter_count = as.integer(value("parameter_count")), variables = list(variables),
    retry_history = list(result$history), parameters = list(diagnostics$parameters))
  attr(data, "imputation_model_status") <- dplyr::bind_rows(
    attr(data, "imputation_model_status", exact = TRUE), row)
  data
}

#' Select complete training cases and recipients for the specified predictors
prepare_demographic_imputation_cases <- function(specification) {
  required <- all.vars(specification$formula)[-1L]
  training <- specification$observed_values
  recipients <- specification$participants_with_missing_value
  complete_training <- stats::complete.cases(training)
  complete_recipients <- stats::complete.cases(recipients)
  missing_predictors <- is.na(as.matrix(recipients[required]))
  blocked <- create_empty_blocked_imputation_predictions()
  if (any(!complete_recipients)) {
    blocked <- tibble::tibble(
      respondent_id = specification$respondent_id[!complete_recipients],
      variable = specification$variable,
      missing_predictors = apply(missing_predictors[!complete_recipients, , drop = FALSE], 1L,
        function(row) paste(required[row], collapse = ", ")),
      reason = "Required predictor values are unavailable")
  }
  specification$n_missing <- length(specification$respondent_id)
  specification$observed_values <- training[complete_training, , drop = FALSE]
  specification$participants_with_missing_value <- recipients[complete_recipients, , drop = FALSE]
  specification$respondent_id <- specification$respondent_id[complete_recipients]
  specification$n_eligible <- length(specification$respondent_id)
  specification$n_training <- nrow(specification$observed_values)
  if (!specification$n_training && specification$n_eligible) {
    blocked <- dplyr::bind_rows(blocked, tibble::tibble(
      respondent_id = specification$respondent_id,
      variable = specification$variable, missing_predictors = "",
      reason = "No observed-outcome respondents have all required predictors"))
    specification$n_eligible <- 0L
    specification$participants_with_missing_value <- specification$participants_with_missing_value[FALSE, , drop = FALSE]
    specification$respondent_id <- specification$respondent_id[FALSE]
  }
  specification$blocked_predictions <- blocked
  specification
}

create_empty_blocked_imputation_predictions <- function() {
  tibble::tibble(respondent_id = integer(), variable = character(),
    missing_predictors = character(), reason = character())
}

record_blocked_imputation_predictions <- function(data, specification) {
  attr(data, "blocked_imputation_predictions") <- dplyr::bind_rows(
    attr(data, "blocked_imputation_predictions", exact = TRUE), specification$blocked_predictions)
  data
}

record_blocked_demographic_imputation <- function(data, specification) {
  if (specification$n_training && specification$n_eligible) return(NULL)
  reason <- if (!specification$n_training) {
    "Imputation not fitted: no observed-outcome respondents have all required predictors"
  } else "Imputation not fitted: no recipients have all required predictors"
  result <- list(status = "blocked", history = tibble::tibble(),
    diagnostics = list(note = reason))
  record_imputation_model_status(data, result, specification$variable, "demographic",
    specification$variable, specification$n_missing, cases = specification)
}

extract_failed_imputation_model_variables <- function(data) {
  models <- attr(data, "imputation_model_status", exact = TRUE)
  if (is.null(models) || !nrow(models)) return(character())
  unique(unlist(models$variables[models$status %in% c("failed", "blocked")], use.names = FALSE))
}

has_unexplained_imputation_cells <- function(data, variables) {
  failed <- extract_failed_imputation_model_variables(data)
  blocked <- attr(data, "blocked_imputation_predictions", exact = TRUE)
  for (variable in setdiff(variables, failed)) {
    missing_ids <- data$respondent_id[is.na(data[[variable]])]
    blocked_ids <- if (is.null(blocked)) data$respondent_id[FALSE] else
      blocked$respondent_id[blocked$variable == variable]
    if (any(!missing_ids %in% blocked_ids)) return(TRUE)
  }
  FALSE
}

extract_unresolved_imputation_variables <- function(data) {
  failed <- extract_failed_imputation_model_variables(data)
  blocked <- attr(data, "blocked_imputation_predictions", exact = TRUE)
  unique(c(failed, if (!is.null(blocked)) blocked$variable))
}

#' Propagate unavailable source columns to their derived scores and covariates
identify_unavailable_imputation_variables <- function(data, codebook) {
  raw <- extract_unresolved_imputation_variables(data)
  derive_columns <- function(raw_variables) {
    scales <- codebook$scales$scale_key[vapply(codebook$scales$item_codes,
      function(items) any(items %in% raw_variables), logical(1))]
    derived <- c(scales, if ("demo_age" %in% raw_variables) c("age", "age_band", "age_band_midpoint"),
      if (any(c("demo_hh_members", "demo_income_hh_net") %in% raw_variables))
        c("income", "income_band", "income_band_value", "income_band_value_log"))
    unique(c(raw_variables, derived,
      unlist(lapply(c(NA_character_, "all", "known_gender"), function(population) {
        keys <- intersect(derived, c(codebook$scales$scale_key, codebook$covariates$covariate))
        zm_z_col(keys, codebook, population = if (is.na(population)) NULL else population)
      }), use.names = FALSE)))
  }
  attr(data, "unavailable_imputation_variables") <- derive_columns(raw)
  known_gender <- if ("gender" %in% names(data)) !is.na(data$gender) else rep(TRUE, nrow(data))
  present_raw <- intersect(raw, names(data))
  missing_known_gender <- present_raw[vapply(data[known_gender, present_raw, drop = FALSE], anyNA, logical(1))]
  attr(data, "unavailable_imputation_variables_known_gender") <- derive_columns(missing_known_gender)
  data
}

describe_unavailable_imputation_inputs <- function(data, variables) {
  if (!nrow(data) && isTRUE(nrow(attr(data, "excluded_imputation", exact = TRUE)) > 0L)) {
    return("Not performed: no respondents remain after AP3 imputation exclusions")
  }
  unavailable <- intersect(variables, attr(data, "unavailable_imputation_variables", exact = TRUE))
  unavailable <- unavailable[vapply(unavailable, function(variable) {
    !variable %in% names(data) || anyNA(data[[variable]])
  }, logical(1))]
  if (!length(unavailable)) return(NULL)
  paste("Not performed: failed or blocked AP3 imputation required for",
    paste(unavailable, collapse = ", "))
}

find_rows_with_failed_imputations <- function(data, variables) {
  affected <- intersect(variables, attr(data, "unavailable_imputation_variables", exact = TRUE))
  if (!length(affected)) return(rep(FALSE, nrow(data)))
  if (!all(affected %in% names(data))) stop("An AP3 analysis input column is absent.")
  rowSums(is.na(as.matrix(data[affected]))) > 0L
}

#' Exclude respondents only where an unresolved AP3 value is required
#'
#' Failed model metadata identify the affected columns. Observed values remain
#' usable, and a missing value outside the requested columns excludes no row.
#' The omission record belongs to this analysis sample, not to study eligibility.
exclude_rows_with_failed_imputations <- function(data, variables) {
  affected <- intersect(variables, attr(data, "unavailable_imputation_variables", exact = TRUE))
  if (!length(affected)) return(data)
  if (!all(affected %in% names(data))) stop("An AP3 analysis input column is absent.")
  missing <- is.na(as.matrix(data[affected]))
  exclude <- find_rows_with_failed_imputations(data, variables)
  if (!any(exclude)) return(data)
  ids <- if ("respondent_id" %in% names(data)) data$respondent_id else seq_len(nrow(data))
  record <- tibble::tibble(respondent_id = ids[exclude],
    variables = apply(missing[exclude, , drop = FALSE], 1L,
      function(row) paste(affected[row], collapse = ", ")))
  previous <- attr(data, "excluded_imputation", exact = TRUE)
  result <- data[!exclude, , drop = FALSE]
  result <- copy_imputation_status(result, data)
  attr(result, "excluded_imputation") <- dplyr::bind_rows(previous, record)
  result
}

#' Read the AP3-prepared cases and constants for a regression's variables
#'
#' Normal complete inputs share the existing data. After an imputation failure,
#' AP3 retains separate prepared samples for equations with different outcomes.
select_regression_data_for_model <- function(data, variables) {
  inputs <- attr(data, "regression_model_inputs", exact = TRUE)
  if (is.null(inputs)) return(data)
  matching <- vapply(inputs, function(input) setequal(input$variables, variables), logical(1))
  if (!any(matching)) stop("No AP3-prepared sample matches the regression variables.")
  inputs[[which(matching)[1L]]]$data
}

require_valid_imputation_inputs <- function(data, variables) {
  note <- describe_unavailable_imputation_inputs(data, variables)
  if (!is.null(note)) stop(structure(list(message = note, call = NULL),
    class = c("imputation_unavailable", "error", "condition")))
  invisible(NULL)
}

copy_imputation_status <- function(data, source) {
  for (name in c("imputation_model_status", "blocked_imputation_predictions",
                 "unavailable_imputation_variables",
                 "unavailable_imputation_variables_known_gender", "excluded_imputation")) {
    attr(data, name) <- attr(source, name, exact = TRUE)
  }
  data
}
