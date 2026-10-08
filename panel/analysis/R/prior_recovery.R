# AP6, Justification of Prior Choice — the outcome-blind prior-recovery simulation
#
# The functions of the targets project `prior_recovery`
# (_targets_prior_recovery.R). Each task simulates one data set of
# analysis_plan$prior_recovery$n_per_replicate respondents from the motive
# correlations and the covariate distributions of config/simulation_truth.yaml
# under one effect scenario, prepares it with the AP3 verbs that build
# `data_regressions`, fits it with the AP6 verbs of the main pipeline at every
# slope SD of analysis_plan$prior_recovery$slope_sds, applies the AP6 validity
# gate and keeps the motive coefficients. Only the sampler settings differ from
# the main pipeline: analysis_plan$prior_recovery$sampling replaces those of
# analysis_plan$regression ([apply_recovery_sampling()]).
#
# Per scenario x slope SD the summary records the outcomes named in
# analysis_plan$prior_recovery$outcomes_recorded:
#   coverage_95              share of motive coefficients whose 95% CrI covers the true value
#   directional_error_rate   share of motive coefficients whose CrI excludes zero with the
#                            wrong sign (any credible claim counts as an error when the truth is 0)
#   confirmed_rate           share of motive coefficients with a credible interval of the true
#                            sign (rows per true effect size, plus an "all" row)
#   bias                     mean of (posterior median - true value)
# Every rate rests on the fits that passed the validity gate, because a fit that
# fails the gate gets no verdict (AP6); share_fits_valid states the share of
# fits that passed, and n_cells_valid counts the cells the rates rest on.
# A fit whose sampling stops with an error does not pass either: n_cells_error
# counts its cells, so n_cells - n_cells_valid - n_cells_error cells failed the
# gate. Any other error stops the task, so that targets runs it again.
#
# Effect scenarios (standardised partial coefficients of the motives; the
# covariates keep their working-range values in every scenario):
#   zero                  every motive coefficient 0
#   working_range         true_beta of config/simulation_truth.yaml, the base truth: the
#                         scenario of the synthetic export (scenarios.export) does not reach it
#   close_anchor_<a>      every predicted cell of analysis_plan$predictions$table at +/-a with
#                         its predicted sign, every other cell 0; a is read from the name
#   opposing_signs_among_correlated_motives
#                         the most strongly correlated motive pair of the truth matrix gets
#                         coefficients of opposite sign in every outcome (magnitude: the mean
#                         absolute non-zero motive coefficient of the working range); the
#                         remaining motives 0
#
# The covariates are drawn independently of the motives.

#' The recovery block of the analysis plan, checked
#'
#' Stops unless `analysis_plan$prior_recovery` holds positive whole numbers of
#' respondents, replicates and smoke replicates, slope SDs from the prior sweep
#' including the primary width, known effect scenarios and a complete sampling
#' block.
#'
#' @param analysis_plan Configuration from [zm_config()].
#' @return `analysis_plan`, invisibly.
check_recovery_settings <- function(analysis_plan) {
  recovery <- analysis_plan$prior_recovery
  if (is.null(recovery)) stop("analysis_plan$prior_recovery is missing from the analysis plan.")
  whole <- function(x) is.numeric(x) && length(x) == 1L && is.finite(x) && x >= 1 && x == round(x)
  for (field in c("n_per_replicate", "replicates", "smoke_replicates")) {
    if (!whole(recovery[[field]])) stop("analysis_plan$prior_recovery$", field, " must be a positive whole number.")
  }
  for (field in c("chains", "warmup", "iter_per_chain", "ess_target")) {
    if (!whole(recovery$sampling[[field]])) {
      stop("analysis_plan$prior_recovery$sampling$", field, " must be a positive whole number.")
    }
  }
  slope_sds <- as.numeric(unlist(recovery$slope_sds))
  sweep <- as.numeric(unlist(analysis_plan$priors$slope_sd_sweep))
  if (length(slope_sds) == 0L || !all(zm_sweep_key(slope_sds) %in% zm_sweep_key(sweep)) ||
      !zm_sweep_key(analysis_plan$priors$slope_sd_primary) %in% zm_sweep_key(slope_sds)) {
    stop("analysis_plan$prior_recovery$slope_sds must be values of priors$slope_sd_sweep, the primary width among them.")
  }
  scenarios <- as.character(unlist(recovery$effect_scenarios))
  anchors <- suppressWarnings(as.numeric(sub("^close_anchor_", "", scenarios)))
  known <- scenarios %in% c("zero", "working_range", "opposing_signs_among_correlated_motives") |
    (startsWith(scenarios, "close_anchor_") & is.finite(anchors) & anchors > 0)
  if (length(scenarios) == 0L || !all(known) || anyDuplicated(scenarios) > 0L) {
    stop("analysis_plan$prior_recovery$effect_scenarios names an unknown or repeated scenario: ",
         paste(scenarios[!known | duplicated(scenarios)], collapse = ", "), ".")
  }
  invisible(analysis_plan)
}

#' The recovery sampler settings in place of the regression's
#'
#' Copies `analysis_plan$prior_recovery$sampling` into
#' `analysis_plan$regression`: chains, warmup, post-warmup draws per chain and
#' the ESS target of the validity gate; every chain on its own core. Every
#' other regression setting — formula, family, priors, seed, gate thresholds and
#' retries — stays as configured.
#'
#' @param analysis_plan Configuration from [zm_config()].
#' @return `analysis_plan` with the recovery sampler settings.
apply_recovery_sampling <- function(analysis_plan) {
  sampling <- analysis_plan$prior_recovery$sampling
  analysis_plan$regression$chains <- as.integer(sampling$chains)
  analysis_plan$regression$cores <- as.integer(sampling$chains)
  analysis_plan$regression$warmup <- as.integer(sampling$warmup)
  analysis_plan$regression$iter_per_chain <- as.integer(sampling$iter_per_chain)
  analysis_plan$regression$ess_target <- as.numeric(sampling$ess_target)
  analysis_plan
}

#' Read the analysis plan of the recovery project
#'
#' The `full` profile, the recovery block checked and its sampler settings
#' applied.
#'
#' @param path Path to `config/analysis_plan.yaml`.
#' @return Configuration list.
read_recovery_plan <- function(path = file.path(zm_root(), "config", "analysis_plan.yaml")) {
  analysis_plan <- zm_config(profile = "full", path = path)
  check_recovery_settings(analysis_plan)
  apply_recovery_sampling(analysis_plan)
}

#' The run the targets project selects: replicates and output files
#'
#' `TAR_PROJECT=prior_recovery` runs
#' `analysis_plan$prior_recovery$replicates` replicates per scenario and writes
#' the files under `data/derived/prior_recovery_*`;
#' `TAR_PROJECT=prior_recovery_smoke` runs `smoke_replicates` and writes
#' `data/derived/prior_recovery_smoke_*`, so a smoke run never overwrites the
#' files of the full run.
#'
#' @param analysis_plan Configuration from [read_recovery_plan()].
#' @param project The targets project, `Sys.getenv("TAR_PROJECT")`.
#' @return `list(project, replicates, paths)`; `paths` names `summary`,
#'   `figure` and `coefficients`.
select_recovery_run <- function(analysis_plan, project = Sys.getenv("TAR_PROJECT")) {
  recovery <- analysis_plan$prior_recovery
  replicates <- switch(project,
    prior_recovery = recovery$replicates,
    prior_recovery_smoke = recovery$smoke_replicates,
    stop("The prior-recovery script runs under TAR_PROJECT=prior_recovery or ",
         "TAR_PROJECT=prior_recovery_smoke, not '", project, "'.")
  )
  derived <- file.path(analysis_plan$root, "data", "derived")
  list(
    project = project,
    replicates = as.integer(replicates),
    paths = c(
      summary = file.path(derived, paste0(project, "_summary.csv")),
      figure = file.path(derived, paste0(project, "_summary.png")),
      coefficients = file.path(derived, paste0(project, "_coefficients.csv"))
    )
  )
}

#' The files the recovery project reads
#'
#' @param root Analysis project root.
#' @param codebook_dir Codebook directory relative to `root`
#'   (`analysis_plan$meta$codebook_dir`).
#' @return Named character vector `analysis_plan`, `simulation_truth`,
#'   `codebook_items`, `codebook_scales`, `codebook_factors`.
select_recovery_input_files <- function(root, codebook_dir) {
  c(
    analysis_plan = file.path(root, "config", "analysis_plan.yaml"),
    simulation_truth = file.path(root, "config", "simulation_truth.yaml"),
    codebook_items = file.path(root, codebook_dir, "codebook_items.csv"),
    codebook_scales = file.path(root, codebook_dir, "codebook_scales.csv"),
    codebook_factors = file.path(root, codebook_dir, "codebook_factors.csv")
  )
}

#' The regression model set of the recovery fits
#'
#' The model set of the main pipeline ([define_regression_model_set()]),
#' with the prior widths of `analysis_plan$prior_recovery$slope_sds`.
#'
#' @param analysis_plan Configuration from [read_recovery_plan()].
#' @return The model set.
define_recovery_model_set <- function(analysis_plan) {
  model_set <- define_regression_model_set(analysis_plan)
  model_set$metric_slope_priors <- as.numeric(unlist(analysis_plan$prior_recovery$slope_sds))
  model_set
}

#' The generating coefficients of every effect scenario
#'
#' @param truth The truth from [zm_truth()].
#' @param analysis_plan Configuration from [read_recovery_plan()].
#' @return Named list (one entry per scenario) of named lists (one entry per
#'   outcome key) of named numeric vectors: the motives in
#'   `analysis_plan$regression$motives` order, then `age`, `male`, `income`.
build_recovery_scenario_coefficients <- function(truth, analysis_plan) {
  outcomes <- as.character(analysis_plan$regression$outcomes)
  motives <- as.character(analysis_plan$regression$motives)
  correlations <- truth$motive_correlations$matrix
  if (!setequal(colnames(correlations), motives)) {
    stop("The motives of simulation_truth.yaml differ from analysis_plan$regression$motives.")
  }
  covariates <- c("age", "male", "income")
  working <- lapply(truth$true_beta, unlist)
  nonzero <- unlist(lapply(working[outcomes], function(b) b[motives][b[motives] != 0]))
  opposing_magnitude <- mean(abs(nonzero))
  scenarios <- as.character(unlist(analysis_plan$prior_recovery$effect_scenarios))
  coefficients <- lapply(scenarios, function(scenario) {
    stats::setNames(lapply(outcomes, function(outcome) {
      base <- working[[outcome]]
      beta <- stats::setNames(rep(0, length(motives)), motives)
      if (identical(scenario, "working_range")) {
        beta[motives] <- base[motives]
      } else if (startsWith(scenario, "close_anchor_")) {
        anchor <- as.numeric(sub("^close_anchor_", "", scenario))
        beta[motives] <- anchor * read_predicted_signs(analysis_plan, outcome)
      } else if (identical(scenario, "opposing_signs_among_correlated_motives")) {
        pairs <- find_most_correlated_motive_pairs(correlations)
        used <- character(0)
        for (k in seq_len(nrow(pairs))) {
          first <- colnames(correlations)[pairs[k, 1]]
          second <- colnames(correlations)[pairs[k, 2]]
          if (first %in% used || second %in% used) next
          beta[first] <- opposing_magnitude
          beta[second] <- -opposing_magnitude
          used <- c(used, first, second)
        }
      }
      c(beta, base[covariates])
    }), outcomes)
  })
  stats::setNames(coefficients, scenarios)
}

#' The predicted sign of every motive for one outcome, as +1, -1 or 0
#'
#' @param analysis_plan Configuration from [zm_config()].
#' @param outcome Outcome key.
#' @return Named numeric vector over `analysis_plan$regression$motives`; a
#'   cell without a directional prediction is 0.
read_predicted_signs <- function(analysis_plan, outcome) {
  cells <- analysis_plan$predictions$table[[outcome]]
  motives <- as.character(analysis_plan$regression$motives)
  vapply(motives, function(motive) {
    sign <- cells[[motive]]
    if (identical(sign, "+")) 1 else if (identical(sign, "-")) -1 else 0
  }, numeric(1))
}

#' The motive pairs with the largest absolute correlation
#'
#' @param correlations Motive correlation matrix with dimnames.
#' @param k Number of pairs.
#' @return Two-column integer matrix of row and column indices, strongest first.
find_most_correlated_motive_pairs <- function(correlations, k = 2L) {
  pairs <- which(upper.tri(correlations), arr.ind = TRUE)
  ordered <- order(-abs(correlations[pairs]))
  pairs[ordered[seq_len(k)], , drop = FALSE]
}

#' One task per scenario and replicate, with its data seed
#'
#' The data seed of a replicate is `analysis_plan$regression$seed + 1000 x` the
#' scenario's position in `analysis_plan$prior_recovery$effect_scenarios` `+`
#' the replicate number, so a replicate keeps its data set whatever the number
#' of replicates of the run.
#'
#' @param analysis_plan Configuration from [read_recovery_plan()].
#' @param replicates Replicates per scenario ([select_recovery_run()]).
#' @return Tibble `scenario`, `scenario_index`, `replicate`, `data_seed`, one
#'   row per task, replicates within scenarios.
build_recovery_tasks <- function(analysis_plan, replicates) {
  scenarios <- as.character(unlist(analysis_plan$prior_recovery$effect_scenarios))
  tasks <- tidyr::expand_grid(scenario = scenarios, replicate = seq_len(replicates))
  tasks$scenario_index <- match(tasks$scenario, scenarios)
  tasks$data_seed <- as.integer(analysis_plan$regression$seed) + 1000L * tasks$scenario_index + tasks$replicate
  tasks[, c("scenario", "scenario_index", "replicate", "data_seed")]
}

#' The regression input of one simulated data set
#'
#' The simulated scale scores and covariates pass through the AP3 verbs that
#' build `data_regressions` in the main pipeline: the analysis bands of age and
#' income, the gender reference, the known-gender standardisation, the
#' known-gender population and the regression input.
#'
#' @param task One row of [build_recovery_tasks()].
#' @param scenario_coefficients From [build_recovery_scenario_coefficients()].
#' @param truth The truth from [zm_truth()].
#' @param analysis_plan Configuration from [read_recovery_plan()].
#' @param codebook Codebook from [zm_codebook()].
#' @return The regression input, in the shape of `data_regressions`.
simulate_recovery_dataset <- function(task, scenario_coefficients, truth, analysis_plan, codebook) {
  draw_recovery_scores(task, scenario_coefficients, truth, analysis_plan) |>
    add_age_and_income_bands(analysis_plan) |>
    set_gender_reference() |>
    standardise_known_gender(codebook, analysis_plan) |>
    drop_rows_without_gender() |>
    select_regression_input(codebook, analysis_plan)
}

#' Draw the scale scores and covariates of one simulated data set
#'
#' Under the task's data seed: the motives as multivariate normal scores with
#' the truth's correlations; age, a binary gender and income per household
#' member drawn independently of the motives from the truth's covariate
#' distributions; each outcome as its linear predictor (motives and
#' standardised exact covariates) plus a normal residual that brings its
#' variance to one, or to the linear predictor's variance plus 0.2 when that
#' exceeds 0.8.
#'
#' @inheritParams simulate_recovery_dataset
#' @return Tibble `respondent_id`, the motive and outcome scale scores under
#'   their keys, `age`, `gender` (factor) and `income`.
draw_recovery_scores <- function(task, scenario_coefficients, truth, analysis_plan) {
  coefficients <- scenario_coefficients[[task$scenario]]
  outcomes <- as.character(analysis_plan$regression$outcomes)
  motives <- as.character(analysis_plan$regression$motives)
  covariates <- c("age", "male", "income")
  correlations <- truth$motive_correlations$matrix
  distributions <- truth$covariates
  n <- as.integer(analysis_plan$prior_recovery$n_per_replicate)
  set.seed(task$data_seed)
  scores <- matrix(stats::rnorm(n * ncol(correlations)), n, ncol(correlations)) %*% chol(correlations)
  colnames(scores) <- colnames(correlations)
  age <- pmin(pmax(round(stats::rnorm(n, distributions$age$mean, distributions$age$sd)),
                   distributions$age$min), distributions$age$max)
  p_male <- distributions$gender_probs$male /
    (distributions$gender_probs$male + distributions$gender_probs$female)
  male <- stats::rbinom(n, 1, p_male)
  band <- sample(seq_along(distributions$income_band_probs), n, replace = TRUE,
                 prob = unlist(distributions$income_band_probs))
  household <- sample(seq_along(distributions$hh_size_probs), n, replace = TRUE,
                      prob = unlist(distributions$hh_size_probs))
  income <- as.numeric(analysis_plan$income$band_representative[band]) / household
  standardised_covariates <- cbind(age = as.numeric(scale(age)), male = as.numeric(scale(male)),
                                   income = as.numeric(scale(log(income))))
  data <- tibble::as_tibble(as.data.frame(scores))
  for (outcome in outcomes) {
    beta <- coefficients[[outcome]]
    linear <- scores[, motives, drop = FALSE] %*% beta[motives] +
      standardised_covariates %*% beta[covariates]
    residual_sd <- sqrt(max(1 - stats::var(as.numeric(linear)), 0.2))
    data[[outcome]] <- as.numeric(linear) + stats::rnorm(n, 0, residual_sd)
  }
  data$age <- age
  data$gender <- factor(ifelse(male == 1, "male", "female"), levels = c("female", "male"))
  data$income <- income
  data$respondent_id <- seq_len(n)
  data[, c("respondent_id", motives, outcomes, "age", "gender", "income")]
}

#' Fit, gate and summarise the regressions of one simulated data set
#'
#' Every regression of the model set at each of its slope SDs, one fit at a
#' time through [fit_one_recovery_regression()], then the coefficient
#' summaries of the AP6 result verb. A fit whose sampling stopped with an error
#' contributes one row per motive without an estimate, with `gate_status`
#' `"sampler_error"` and the error message in `sampler_error`; the other fits
#' of the data set keep their rows. Any other error stops the call. The Stan
#' output files of the fits go to a directory of their own, and that directory
#' and the Stan data files the fits wrote to the session's temporary directory
#' are deleted when the call ends.
#'
#' @param data Regression input from [simulate_recovery_dataset()].
#' @param model_set From [define_recovery_model_set()].
#' @param analysis_plan Configuration from [read_recovery_plan()].
#' @return Tibble of the motive coefficients of the primary and prior-width
#'   fits: `outcome_key`, `slope_sd`, `term`, `estimate`, `q_lo`, `q_hi`,
#'   `fit_valid`, `gate_status`, `sampler_error` (`NA` for a fit that sampled).
fit_recovery_regressions <- function(data, model_set, analysis_plan) {
  zm_setup()
  output_dir <- tempfile("prior-recovery-fits-")
  dir.create(output_dir, recursive = TRUE)
  previous <- options(cmdstanr_output_dir = output_dir)
  data_files_before <- list.files(tempdir(), pattern = "\\.json$", full.names = TRUE)
  on.exit({
    options(previous)
    unlink(output_dir, recursive = TRUE)
    unlink(setdiff(list.files(tempdir(), pattern = "\\.json$", full.names = TRUE), data_files_before))
  }, add = TRUE)
  primary <- model_set$primary_slope_sd
  widths <- model_set$metric_slope_priors
  widths <- widths[zm_sweep_key(widths) != zm_sweep_key(primary)]
  fits <- tidyr::expand_grid(slope_sd = c(primary, widths), outcome = names(model_set$formulas))
  fits$result <- lapply(seq_len(nrow(fits)), function(i) {
    fit_one_recovery_regression(data, model_set, fits$outcome[i], fits$slope_sd[i], analysis_plan)
  })
  # The collections in the shapes of the two fitting verbs: by outcome, and by
  # width, then outcome; a sampler error stands in the place of its fit.
  collect_width <- function(slope_sd) {
    at <- zm_sweep_key(fits$slope_sd) == zm_sweep_key(slope_sd)
    stats::setNames(fits$result[at], fits$outcome[at])
  }
  primary_fits <- collect_width(primary)
  prior_width_fits <- stats::setNames(lapply(widths, collect_width), sprintf("%.2f", widths))
  coefficients <- extract_regression_coefficient_summaries(primary_fits, prior_width_fits, NULL, analysis_plan)
  keep <- coefficients$role %in% c("primary", "sweep") & coefficients$term_type %in% "predictor"
  sampled <- coefficients[keep, c("outcome_key", "slope_sd", "term", "estimate", "q_lo", "q_hi",
                                  "fit_valid", "gate_status")]
  sampled$sampler_error <- NA_character_
  failed <- fits[vapply(fits$result, inherits, logical(1), what = "recovery_sampler_error"), ]
  sampler_errors <- tidyr::expand_grid(
    tibble::tibble(outcome_key = unname(model_set$outcome_keys[failed$outcome]),
                   slope_sd = failed$slope_sd,
                   sampler_error = vapply(failed$result, conditionMessage, character(1))),
    term = as.character(analysis_plan$regression$motives)
  )
  dplyr::bind_rows(sampled, tibble::tibble(
    outcome_key = sampler_errors$outcome_key,
    slope_sd = sampler_errors$slope_sd,
    term = sampler_errors$term,
    estimate = NA_real_,
    q_lo = NA_real_,
    q_hi = NA_real_,
    fit_valid = FALSE,
    gate_status = "sampler_error",
    sampler_error = sampler_errors$sampler_error
  ))
}

#' Fit and gate one regression of the recovery model set
#'
#' The model set narrowed to one outcome and one slope SD passes through the
#' fitting verb of the main pipeline for that width —
#' [fit_primary_regressions()] at the primary width,
#' [fit_prior_width_comparisons()] at any other — and through the validity
#' gate. A sampler error of the fit or of a refit of the gate is returned as
#' its condition ([capture_recovery_sampler_error()]); every other error, and
#' a fit the verbs return without a result, stops the call.
#'
#' @param data Regression input from [simulate_recovery_dataset()].
#' @param model_set From [define_recovery_model_set()].
#' @param outcome Readable outcome name, one of `names(model_set$formulas)`.
#' @param slope_sd Slope prior SD, one of `model_set$metric_slope_priors`.
#' @param analysis_plan Configuration from [read_recovery_plan()].
#' @return The gated `brmsfit`, or a condition of class `recovery_sampler_error`.
fit_one_recovery_regression <- function(data, model_set, outcome, slope_sd, analysis_plan) {
  one_model <- model_set
  one_model$formulas <- model_set$formulas[outcome]
  one_model$metric_slope_priors <- slope_sd
  primary <- identical(zm_sweep_key(slope_sd), zm_sweep_key(model_set$primary_slope_sd))
  gated <- capture_recovery_sampler_error(
    if (primary) {
      fit_primary_regressions(data, one_model, analysis_plan) |>
        apply_validity_gate(analysis_plan)
    } else {
      fit_prior_width_comparisons(data, one_model, analysis_plan) |>
        apply_validity_gate(analysis_plan)
    }
  )
  if (inherits(gated, "recovery_sampler_error")) return(gated)
  fit <- if (primary) gated[[outcome]] else gated[[1L]][[outcome]]
  if (!inherits(fit, "brmsfit")) {
    stop("The recovery fit of ", outcome, " at slope SD ", slope_sd, " has no result",
         if (inherits(fit, "condition")) paste0(": ", conditionMessage(fit)) else "", ".", call. = FALSE)
  }
  fit
}

#' Return a sampler error of a fitting expression as its value
#'
#' Evaluates `expr`. An error signalled while brms samples — inside its fitting
#' step `brms:::fit_model()`, which every first fit and every refit of the
#' validity gate runs — is returned as a condition of class
#' `recovery_sampler_error` with the original message. Every other error (data
#' preparation, priors, model compilation, the gate's diagnostics) propagates.
#'
#' @param expr The fitting expression.
#' @return The value of `expr`, or the `recovery_sampler_error` condition.
capture_recovery_sampler_error <- function(expr) {
  sampler_step <- utils::getFromNamespace("fit_model", "brms")
  signalled_while_sampling <- function() {
    any(vapply(seq_len(sys.nframe()), function(frame) identical(sys.function(frame), sampler_step),
               logical(1)))
  }
  tryCatch(
    withCallingHandlers(expr, error = function(condition) {
      if (signalled_while_sampling()) {
        stop(structure(list(message = conditionMessage(condition), call = conditionCall(condition)),
                       class = c("recovery_sampler_error", "error", "condition")))
      }
    }),
    recovery_sampler_error = identity
  )
}

#' The motive coefficients of one task beside their generating values
#'
#' One row per outcome x slope SD x motive. A cell whose fit stopped with a
#' sampler error has no estimate, counts as a fit that did not pass the gate
#' and carries the error message in `sampler_error`. A cell without a row in
#' `coefficients` stops the call.
#'
#' @param coefficients From [fit_recovery_regressions()].
#' @param task One row of [build_recovery_tasks()].
#' @param scenario_coefficients From [build_recovery_scenario_coefficients()].
#' @param analysis_plan Configuration from [read_recovery_plan()].
#' @return Tibble `outcome`, `slope_sd`, `term`, `true`, `estimate`, `lo`,
#'   `hi`, `fit_valid`, `gate_status`, `sampler_error`, `scenario`,
#'   `replicate`, `data_seed`.
summarise_recovery_fit <- function(coefficients, task, scenario_coefficients, analysis_plan) {
  outcomes <- as.character(analysis_plan$regression$outcomes)
  motives <- as.character(analysis_plan$regression$motives)
  slope_sds <- as.numeric(unlist(analysis_plan$prior_recovery$slope_sds))
  cells <- tidyr::expand_grid(outcome = outcomes, slope_sd = slope_sds, term = motives)
  truth <- scenario_coefficients[[task$scenario]]
  cells$true <- vapply(seq_len(nrow(cells)), function(i) {
    as.numeric(truth[[cells$outcome[i]]][[cells$term[i]]])
  }, numeric(1))
  cell_key <- function(outcome, slope_sd, term) paste(outcome, zm_sweep_key(slope_sd), term)
  found <- match(cell_key(cells$outcome, cells$slope_sd, cells$term),
                 cell_key(coefficients$outcome_key, coefficients$slope_sd, coefficients$term))
  if (anyNA(found)) {
    absent <- unique(paste(cells$outcome, "at slope SD", cells$slope_sd)[is.na(found)])
    stop("The fits of scenario ", task$scenario, ", replicate ", task$replicate,
         " have no coefficient for ", paste(absent, collapse = "; "), ".", call. = FALSE)
  }
  pick <- function(column) coefficients[[column]][found]
  tibble::tibble(
    outcome = cells$outcome,
    slope_sd = cells$slope_sd,
    term = cells$term,
    true = cells$true,
    estimate = as.numeric(pick("estimate")),
    lo = as.numeric(pick("q_lo")),
    hi = as.numeric(pick("q_hi")),
    fit_valid = as.logical(pick("fit_valid")) %in% TRUE,
    gate_status = as.character(pick("gate_status")),
    sampler_error = as.character(pick("sampler_error")),
    scenario = task$scenario,
    replicate = as.integer(task$replicate),
    data_seed = as.integer(task$data_seed)
  )
}

#' Score every coefficient cell against its generating value
#'
#' @param rows Bound rows of [summarise_recovery_fit()].
#' @return `rows` with `credible` (the interval excludes zero), `sign_est`,
#'   `sign_true`, `covered` (the interval holds the true value),
#'   `directional_error` (a credible interval of the wrong sign, or any
#'   credible interval at a true value of zero), `confirmed` (a credible
#'   interval of the true, non-zero sign) and `error` (estimate minus true value).
score_recovery_rows <- function(rows) {
  rows$credible <- ap7_credible(rows$lo, rows$hi)
  rows$sign_est <- ap7_sign(rows$estimate)
  rows$sign_true <- ap7_sign(rows$true)
  rows$covered <- !is.na(rows$lo) & rows$lo <= rows$true & rows$true <= rows$hi
  rows$directional_error <- !is.na(rows$credible) & rows$credible &
    (rows$sign_true == "" | rows$sign_est != rows$sign_true)
  rows$confirmed <- !is.na(rows$credible) & rows$credible & rows$sign_true != "" &
    rows$sign_est == rows$sign_true
  rows$error <- rows$estimate - rows$true
  rows
}

#' The recovery summary per scenario x slope SD x generating value
#'
#' One `all` row per scenario and slope SD over every motive coefficient, then
#' one row per non-zero generating value. The rates rest on the cells whose fit
#' passed the validity gate; `n_cells_error` counts the cells whose fit stopped
#' with a sampler error, apart from the cells whose fit failed the gate.
#'
#' @param scored From [score_recovery_rows()].
#' @param analysis_plan Configuration from [read_recovery_plan()].
#' @return Tibble `scenario`, `slope_sd`, `true_effect`, `n_cells`,
#'   `n_cells_valid`, `n_replicates`, `share_fits_valid`, `n_cells_error`,
#'   `coverage_95`, `directional_error_rate`, `confirmed_rate`, `bias`, `n_per_replicate`,
#'   `chains`, `iter_per_chain`, `warmup`, `ess_target`, `ci_level`.
summarise_recovery_rows <- function(scored, analysis_plan) {
  summary <- scored |>
    dplyr::group_by(scenario, slope_sd) |>
    dplyr::group_modify(function(cells, key) {
      nonzero <- cells[cells$true != 0, , drop = FALSE]
      by_value <- if (nrow(nonzero) == 0L) NULL else {
        nonzero |>
          dplyr::group_by(true) |>
          dplyr::group_modify(function(value_cells, value) {
            summarise_recovery_block(value_cells, format(value$true))
          }) |>
          dplyr::ungroup() |>
          dplyr::select(-"true")
      }
      dplyr::bind_rows(summarise_recovery_block(cells, "all"), by_value)
    }) |>
    dplyr::ungroup()
  regression <- analysis_plan$regression
  summary$n_per_replicate <- as.integer(analysis_plan$prior_recovery$n_per_replicate)
  summary$chains <- as.integer(regression$chains)
  summary$iter_per_chain <- as.integer(regression$iter_per_chain)
  summary$warmup <- as.integer(regression$warmup)
  summary$ess_target <- as.numeric(regression$ess_target)
  summary$ci_level <- as.numeric(regression$ci_level)
  summary
}

#' The recovery rates of one block of coefficient cells
#'
#' @param cells Scored cells of one scenario x slope SD (x generating value).
#' @param true_effect Label of the block: `"all"` or the generating value.
#' @return One-row tibble.
summarise_recovery_block <- function(cells, true_effect) {
  valid <- cells[cells$fit_valid %in% TRUE, , drop = FALSE]
  directional <- valid$sign_true != ""
  tibble::tibble(
    true_effect = true_effect,
    n_cells = nrow(cells),
    n_cells_valid = nrow(valid),
    n_replicates = length(unique(cells$replicate)),
    share_fits_valid = mean(cells$fit_valid %in% TRUE),
    n_cells_error = sum(!is.na(cells$sampler_error)),
    coverage_95 = mean(valid$covered),
    directional_error_rate = mean(valid$directional_error),
    confirmed_rate = if (any(directional)) mean(valid$confirmed[directional]) else NA_real_,
    bias = mean(valid$error, na.rm = TRUE)
  )
}

#' Write a recovery table as CSV
#'
#' @param table The summary or the scored coefficient cells.
#' @param path Output path from [select_recovery_run()].
#' @return `path`.
write_recovery_table <- function(table, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(table, path, na = "", progress = FALSE)
  path
}

#' Write the recovery figure: the three rates per scenario and slope SD
#'
#' @param summary From [summarise_recovery_rows()].
#' @param path Output path from [select_recovery_run()].
#' @return `path`.
write_recovery_figure <- function(summary, path) {
  rates <- summary[summary$true_effect == "all", , drop = FALSE] |>
    tidyr::pivot_longer(c("coverage_95", "directional_error_rate", "confirmed_rate"),
                        names_to = "metric", values_to = "value")
  settings <- summary[1L, , drop = FALSE]
  figure <- ggplot2::ggplot(rates, ggplot2::aes(x = factor(.data$slope_sd), y = .data$value,
                                                colour = .data$metric, group = .data$metric)) +
    ggplot2::geom_line() +
    ggplot2::geom_point(size = 2) +
    ggplot2::facet_wrap(~scenario, ncol = 2) +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::labs(
      x = "Slope prior SD", y = "Rate over motive coefficients", colour = NULL,
      title = "Prior recovery of the regressions (motive coefficients)",
      subtitle = paste0(max(summary$n_replicates), " replicates x n = ", settings$n_per_replicate, "; ",
                        settings$chains, " chains, ", settings$warmup, "/", settings$iter_per_chain,
                        " iterations; CrI ", round(100 * settings$ci_level), "%")
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(legend.position = "bottom")
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(path, figure, width = 8, height = 6, dpi = 150)
  path
}
