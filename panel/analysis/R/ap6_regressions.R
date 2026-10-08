# Four outcomes, one regression each: shared predictors, three slope-prior widths,
# the primary width decides. The pipeline fits through
# fit_primary_regressions() and the verbs after it (_targets.R), and so does
# the prior-recovery simulation (R/prior_recovery.R). The two stages at the top
# of this file are the retry and the final check of the validity gate
# (apply_validity_gate()).

# BEGIN GENERATED PARAMETER CARD: AP6 RETRY
# Automatically generated from analysis_plan.yaml
# Effective sample size required of every coefficient: 10000 bulk and 10000 tail.
# Profile smoke instead: 2000 bulk and 2000 tail.
# Profile presentation instead: 200 bulk and 200 tail.
# On a shortfall: posterior draws per chain doubled, at most 3 times.
# R-hat at most 1.01.
# Divergent transitions at most 0.
# Transitions at the maximum tree depth at most 0.
# BFMI of every chain at least 0.2.
# Monte Carlo SE reported for medians and the 95% interval endpoints.
# On R-hat, divergences or tree-depth hits: one refit with doubled warmup, adapt_delta 0.99, max_treedepth 15.
# END GENERATED PARAMETER CARD: AP6 RETRY
ap6_refit_if_needed <- function(model, analysis_plan) {
  gate <- ap6_gate_settings(analysis_plan$regression)
  fit <- model$fit
  warmup <- model$warmup
  draws_per_chain <- model$draws_per_chain
  control <- model$control
  ess <- model$ess
  refit <- function(fit, warmup, draws_per_chain, control) {
    do.call(stats::update, c(list(object = fit, recompile = FALSE),
                             ap6_sampling_args(model$regression, warmup, draws_per_chain, control)))
  }
  # The gate measured ESS on the first fit; this stage refits for ESS, then checks sampler diagnostics.
  n_doublings <- 0L
  while (!ess$ok && n_doublings < model$max_doublings) {
    n_doublings <- n_doublings + 1L
    draws_per_chain <- draws_per_chain * 2L
    fit <- refit(fit, warmup, draws_per_chain, control)
    ess <- ap6_ess_check(fit, analysis_plan)
    model$ess_log <- tibble::add_row(
      model$ess_log, attempt = n_doublings + 1L, iter_per_chain = draws_per_chain,
      min_ess_bulk = ess$min_ess_bulk, min_ess_tail = ess$min_ess_tail, ok = ess$ok
    )
    model$retry_log <- tibble::add_row(
      model$retry_log, step = 1L, reason = "ESS shortfall: iterations doubled", warmup = warmup,
      iter_per_chain = draws_per_chain, adapt_delta = ap6_adapt_delta(fit),
      max_treedepth = ap6_max_treedepth(fit), ok = ess$ok
    )
  }
  diagnostics <- ap6_fit_diagnostics(fit, analysis_plan)
  # R-hat / divergences / tree depth -> ONE refit with doubled warmup and retry control.
  sampler_failed <- !diagnostics$rhat_ok || !diagnostics$divergences_ok || !diagnostics$treedepth_ok
  if (sampler_failed) {
    warmup <- warmup * 2L
    control <- list(adapt_delta = gate$adapt_delta_retry, max_treedepth = as.integer(gate$max_treedepth_retry))
    fit <- refit(fit, warmup, draws_per_chain, control)
    diagnostics <- ap6_fit_diagnostics(fit, analysis_plan)
    model$retry_log <- tibble::add_row(
      model$retry_log, step = 2L,
      reason = paste0("R-hat / divergences / tree depth: refit once with doubled warmup, adapt_delta ",
                      gate$adapt_delta_retry, ", max_treedepth ", gate$max_treedepth_retry),
      warmup = warmup, iter_per_chain = draws_per_chain, adapt_delta = gate$adapt_delta_retry,
      max_treedepth = as.integer(gate$max_treedepth_retry), ok = diagnostics$ok
    )
  }
  model$fit <- fit
  model$warmup <- warmup
  model$draws_per_chain <- draws_per_chain
  model$control <- control
  model$ess <- ess
  model
}

# BEGIN GENERATED PARAMETER CARD: AP6 FINAL CONVERGENCE
# Automatically generated from analysis_plan.yaml
# Effective sample size required of every coefficient: 10000 bulk and 10000 tail.
# Profile smoke instead: 2000 bulk and 2000 tail.
# Profile presentation instead: 200 bulk and 200 tail.
# R-hat at most 1.01.
# Divergent transitions at most 0.
# Transitions at the maximum tree depth at most 0.
# BFMI of every chain at least 0.2.
# Monte Carlo SE reported for medians and the 95% interval endpoints.
# END GENERATED PARAMETER CARD: AP6 FINAL CONVERGENCE
ap6_check_final_convergence <- function(model, analysis_plan) {
  fit <- model$fit
  # The gate on the fit that is returned, whether or not this fit was refitted.
  diagnostics <- ap6_fit_diagnostics(fit, analysis_plan)
  gate_status <- if (!diagnostics$ok) {
    # A failed fit receives no AP10 verdict.
    "not_interpretable"
  } else if (nrow(model$retry_log) > 0L) {
    # This fit passed after at least one retry.
    "retried_ok"
  } else {
    # This fit passed directly.
    "ok"
  }
  ap6_record_fit(fit, model, analysis_plan, diagnostics, gate_status)
}

# Implements AP6 (regressions) and its sensitivity block: each of the
# four z-standardised outcomes is regressed on the five z-standardised motives
# plus age (z), gender (factor, treatment contrasts) and income per household
# member (z) with a gaussian likelihood; normal(0, slope_sd) priors on the
# standardised metric slopes, a separate normal(0, gender_sd) prior on gender
# contrasts, normal(0, intercept_sd) on the intercept and a half-normal(sigma_sd)
# on the residual SD. Only the primary metric-slope SD decides (AP10); the other
# SDs of the sweep (analysis_plan$priors$slope_sd_sweep, labelled by
# analysis_plan$priors$sweep_labels) are fitted for descriptive sensitivity of the
# standardised-coefficient prior block.
#
# Validity gate (analysis_plan$regression$validity_gate): every population-level
# coefficient of a fit must reach the profile's bulk and tail ESS target, R-hat
# at most rhat_max, no divergent transitions, no transition at the maximum tree
# depth, BFMI of every chain at least bfmi_min; the Monte Carlo SE of the
# medians and interval endpoints is reported. Failure rule (on_failure), in
# every profile: (1) an ESS shortfall doubles the post-warmup iterations, at
# most three times; (2) an R-hat, divergence or tree-depth failure triggers ONE
# refit with doubled warmup, adapt_delta_retry and max_treedepth_retry; (3)
# reparameterisation is never automated; (4) a fit that still fails is returned
# with gate_status "not_interpretable" and gets no AP10 verdict.
#
# Missing data (analysis_plan$missing_data$fill): every missing value is filled once,
# before the scale scores (R/ap3_fill.R), so a model sample carries no gap in
# a model variable, and nothing is estimated inside a regression. Gender is
# never filled; AP3 omits the rows without gender from the regressions.
#
# Student-t robustness refit (analysis_plan$sensitivity$student_t): descriptive; runs
# only when the one-sided tail trigger fires (an observed excess kurtosis,
# minimum or maximum MORE extreme than its posterior-predictive band). The
# degrees of freedom are fixed at nu_fixed via a constant prior; the scale
# gets a half-normal(sigma_scale_prior_sd) prior so that the induced residual
# SD prior matches the gaussian model. Bayesian R2 uses one model-based
# definition for both families (analysis_plan$regression$r2): var(mu) / (var(mu) +
# residual variance), residual variance sigma^2 (gaussian) or
# sigma^2 * nu / (nu - 2) (student; 2 * sigma^2 at nu = 4).
#
# Prior sensitivity (analysis_plan$sensitivity$powerscale): priorsense power-scaling per
# prior block (metric slopes, gender contrasts, intercept, sigma; each prior
# carries its block's tag) plus a labelled all-priors run, inspecting every
# population-level coefficient and the Bayesian R2.
#
# Joint estimation across outcomes (fit_joint_regression(), RQ2) uses one
# multivariate model with correlated residuals. AP3 prepares its known-gender
# sample with every required value available and computes its own standardisation
# constants. It provides paired posterior draws for coefficient differences.
# Its marginal posteriors can differ from separate fits even on common rows;
# the separate primary fits determine every RQ1 Table 3 verdict.
#
# Every numeric setting (prior SDs, chains, cores, iterations, ESS target,
# gate thresholds, retry settings, CI level, seed, response range) is read
# from analysis_plan (config/analysis_plan.yaml).

#' Deparsed text of a model formula
#'
#' One string for a plain formula or a `brmsformula`; the `+`-joined response
#' formulas of a `mvbrmsformula`.
#'
#' @param formula A formula, `brmsformula` or `mvbrmsformula`.
#' @return Character scalar.
ap6_formula_text <- function(formula) {
  if (inherits(formula, "formula")) return(deparse1(formula))
  if (inherits(formula, "mvbrmsformula")) {
    parts <- vapply(formula$forms, function(x) deparse1(x$formula), character(1))
    txt <- paste(parts, collapse = " + ")
    if (isFALSE(formula$rescor)) txt <- paste0(txt, " + set_rescor(FALSE)")
    if (isTRUE(formula$rescor)) txt <- paste0(txt, " + set_rescor(TRUE)")
    return(txt)
  }
  deparse1(formula$formula)
}

#' brms response name of the focal outcome
#'
#' brms names the responses of a multivariate model — the joint RQ2 fit — by
#' stripping every non-alphanumeric character (`asc_agg_z` -> `ascaggz`) and
#' prefixes the parameters of each response with that name. A univariate model
#' carries no prefix: the empty string is returned.
#'
#' @param x A formula, `brmsformula`, `mvbrmsformula` or `brmsfit`.
#' @return Character scalar (`""` for a univariate model).
ap6_focal_resp <- function(x) {
  formula <- if (inherits(x, "brmsfit")) x$formula else x
  if (inherits(formula, "formula")) return("")
  bt <- brms::brmsterms(formula)
  if (!inherits(bt, "mvbrmsterms")) return("")
  unname(as.character(bt$responses[1]))
}

#' Regular expression of the focal population-level parameters
#'
#' `b_` parameters (and `bsp_`, brms's class for special population-level
#' terms) of the focal response.
#'
#' @param resp Focal response name from [ap6_focal_resp()].
#' @return Character scalar (a regular expression).
ap6_focal_pattern <- function(resp = "") {
  if (!nzchar(resp)) "^(b|bsp)_" else paste0("^(b|bsp)_", resp, "_")
}

#' Responses whose coefficients the validity gate covers
#'
#' The focal response alone for a univariate fit, but every response of the
#' joint RQ2 model: AP7 requires the joint fit to pass the validity gate of
#' AP6, and all four of its equations are substantive. The joint model is recognised by
#' `set_rescor(TRUE)`, which no other formula of this file carries.
#'
#' @param x A formula, `brmsformula`, `mvbrmsformula` or `brmsfit`.
#' @return Character vector of response names; `""` for a univariate model.
ap6_gate_resp <- function(x) {
  stored <- attr(x, "gate_responses", exact = TRUE)
  if (!is.null(stored)) return(as.character(stored))
  formula <- if (inherits(x, "brmsfit")) x$formula else x
  if (inherits(formula, "mvbrmsformula") && isTRUE(formula$rescor)) {
    return(unname(as.character(brms::brmsterms(formula)$responses)))
  }
  ap6_focal_resp(x)
}

#' Focal population-level parameter names of a fit
#'
#' @param fit A `brmsfit`.
#' @param resp Response names to collect the parameters of; by default the
#'   responses the gate covers ([ap6_gate_resp()]), i.e. the focal response of
#'   a univariate fit and all four responses of the joint RQ2 fit.
#' @return Character vector of parameter names (`b_...`, `bsp_...`).
ap6_focal_b_vars <- function(fit, resp = ap6_gate_resp(fit)) {
  vars <- posterior::variables(fit)
  c(unique(unlist(lapply(as.character(resp), function(r) grep(ap6_focal_pattern(r), vars, value = TRUE)))),
    ap6_gated_rescor_vars(fit, vars))
}

#' Residual correlations the validity gate covers
#'
#' AP6: in the joint model of the motives every residual correlation meets the
#' ESS and R-hat criteria; in the joint model of AP7 they are exempt. The
#' motive model is the one model with correlated residuals whose responses are
#' all motive scores (columns `zm_*`, response ids `zm...`).
#'
#' @param fit A `brmsfit`.
#' @param vars Its variable names.
#' @return Character vector of `rescor__` names, empty for every other model.
ap6_gated_rescor_vars <- function(fit, vars = posterior::variables(fit)) {
  formula <- if (inherits(fit, "brmsfit")) fit$formula else fit
  if (!inherits(formula, "mvbrmsformula") || !isTRUE(formula$rescor)) return(character(0))
  responses <- as.character(formula$responses)
  if (length(responses) == 0L || !all(grepl("^zm", responses))) return(character(0))
  grep("^rescor__", vars, value = TRUE)
}

#' Name of the focal outcome's family
#'
#' `brms::family()` of the focal response (`resp` for a multivariate fit,
#' where the fit-level family is a list).
#'
#' @param fit A `brmsfit`.
#' @return `"gaussian"` or `"student"`.
ap6_focal_family <- function(fit) {
  resp <- ap6_focal_resp(fit)
  fam <- if (nzchar(resp)) stats::family(fit, resp = resp) else stats::family(fit)
  as.character(fam$family)
}

#' Name of the focal residual-scale parameter
#'
#' @param fit A `brmsfit`.
#' @return `"sigma"` or `"sigma_<resp>"`.
ap6_sigma_var <- function(fit) {
  resp <- ap6_focal_resp(fit)
  if (nzchar(resp)) paste0("sigma_", resp) else "sigma"
}

#' Record the result of the final convergence check on a fit
#'
#' @param fit The fitted model returned by the fitting stage.
#' @param model The model list after the retry stage.
#' @param analysis_plan Configuration from [zm_config()].
#' @param diagnostics Result of [ap6_fit_diagnostics()] for `fit`.
#' @param gate_status Status selected by [ap6_check_final_convergence()].
#' @return `fit` with the AP6 result attributes and provenance.
ap6_record_fit <- function(fit, model, analysis_plan, diagnostics, gate_status) {
  attr(fit, "outcome") <- model$outcome
  attr(fit, "slope_sd") <- model$slope_sd
  attr(fit, "family_name") <- model$family_name
  attr(fit, "sample_prior") <- model$sample_prior
  attr(fit, "iter_used") <- model$draws_per_chain
  attr(fit, "warmup_used") <- model$warmup
  attr(fit, "ess_log") <- model$ess_log
  attr(fit, "retry_log") <- model$retry_log
  attr(fit, "gate_status") <- gate_status
  attr(fit, "nu_fixed") <- attr(model$priors, "nu_fixed")
  attr(fit, "ci_level") <- model$regression$ci_level
  attr(fit, "seed") <- as.integer(model$regression$seed)
  attr(fit, "profile") <- analysis_plan$profile_name
  attr(fit, "cfg_regression") <- model$regression
  attr(fit, "cfg_sensitivity") <- analysis_plan$sensitivity
  attr(fit, "formula_text") <- ap6_formula_text(model$formula)
  attr(fit, "diagnostics") <- diagnostics
  attr(fit, "provenance") <- ap6_fit_provenance(fit)
  fit
}

#' Regression settings of a fit
#'
#' Returns `analysis_plan$regression` when `analysis_plan` is given, else the copy stored on the
#' fit by the validity gate ([ap6_record_fit()]).
#'
#' @param fit A `brmsfit` gated by [apply_validity_gate()].
#' @param analysis_plan Configuration from `zm_config()` or `NULL`.
#' @return The `regression` list.
ap6_reg_settings <- function(fit, analysis_plan = NULL) {
  reg <- if (!is.null(analysis_plan)) analysis_plan$regression else attr(fit, "cfg_regression")
  reg
}

#' Thresholds and retry settings of the validity gate
#'
#' Reads `analysis_plan$regression$validity_gate`. The ESS threshold is the run
#' profile's `analysis_plan$regression$ess_target` (full 10,000; smoke 2,000) for bulk
#' and tail ESS alike; the other thresholds come from the gate block.
#'
#' @param reg The `regression` block (see [ap6_reg_settings()]).
#' @return `list(ess_target, rhat_max, divergences_max, treedepth_hits_max,
#'   bfmi_min, max_ess_doublings, adapt_delta_retry, max_treedepth_retry)`.
ap6_gate_settings <- function(reg) {
  gate <- reg$validity_gate
  list(
    ess_target = as.numeric(reg$ess_target),
    rhat_max = as.numeric(gate$rhat_max),
    divergences_max = as.numeric(gate$divergences_max),
    treedepth_hits_max = as.numeric(gate$treedepth_hits_max),
    bfmi_min = as.numeric(gate$bfmi_min),
    max_ess_doublings = as.integer(gate$max_ess_doublings),
    adapt_delta_retry = as.numeric(gate$adapt_delta_retry),
    max_treedepth_retry = as.numeric(gate$max_treedepth_retry)
  )
}

#' Quantile probabilities of a central credible interval
#'
#' @param level Interval level, e.g. 0.95.
#' @return Numeric vector of length two (lower, upper).
ap6_ci_probs <- function(level) {
  c((1 - level) / 2, 1 - (1 - level) / 2)
}

#' Evaluate an expression with a temporary RNG seed
#'
#' Restores the global RNG state afterwards, so draws for figures and checks
#' are reproducible without disturbing the caller.
#'
#' @param seed Integer seed.
#' @param expr Expression to evaluate.
#' @return Value of `expr`.
ap6_reg_with_seed <- function(seed, expr) {
  had <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  old <- if (had) get(".Random.seed", envir = globalenv(), inherits = FALSE) else NULL
  on.exit({
    if (had) {
      assign(".Random.seed", old, envir = globalenv())
    } else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      rm(".Random.seed", envir = globalenv())
    }
  }, add = TRUE)
  set.seed(seed)
  expr
}

#' Effective sample size check of the population-level coefficients
#'
#' @param fit A `brmsfit`.
#' @param analysis_plan Configuration from `zm_config()` or `NULL` (then the settings
#'   stored on the fit are used).
#' @return `list(ok, min_ess_bulk, min_ess_tail, target, summary)`; `ok` is
#'   `TRUE` when every focal `b_` parameter has bulk and tail ESS at or above
#'   `analysis_plan$regression$ess_target`.
ap6_ess_check <- function(fit, analysis_plan = NULL) {
  target <- ap6_reg_settings(fit, analysis_plan)$ess_target
  draws <- posterior::as_draws_array(fit)
  b_vars <- ap6_focal_b_vars(fit)
  s <- suppressMessages(posterior::summarise_draws(
    posterior::subset_draws(draws, variable = b_vars),
    ess_bulk = posterior::ess_bulk, ess_tail = posterior::ess_tail
  ))
  ok <- all(is.finite(s$ess_bulk)) && all(is.finite(s$ess_tail)) &&
    all(s$ess_bulk >= target) && all(s$ess_tail >= target)
  list(
    ok = ok,
    min_ess_bulk = min(s$ess_bulk),
    min_ess_tail = min(s$ess_tail),
    target = target,
    summary = s
  )
}

#' Maximum tree depth a fit was sampled with
#'
#' Read from the sampler arguments brms stores (cmdstanr backend).
#'
#' @param fit A `brmsfit`.
#' @return Integer scalar.
ap6_max_treedepth <- function(fit) {
  args <- fit$fit@stan_args[[1]]
  value <- args$control$max_treedepth
  if (is.null(value)) value <- args$max_depth
  as.integer(value)
}

#' Adapt delta a fit was sampled with
#'
#' @param fit A `brmsfit`.
#' @return Numeric scalar.
ap6_adapt_delta <- function(fit) {
  args <- fit$fit@stan_args[[1]]
  value <- args$control$adapt_delta
  if (is.null(value)) value <- args$delta
  as.numeric(value)
}

#' Bayesian fraction of missing information per chain
#'
#' `sum(diff(E)^2) / sum((E - mean(E))^2)` over the post-warmup Hamiltonian
#' energies of each chain (the Stan definition).
#'
#' @param nuts Tibble from `brms::nuts_params(fit)`.
#' @return Numeric vector, one value per chain (`NA` when energies are missing).
ap6_bfmi <- function(nuts) {
  e <- nuts[nuts$Parameter == "energy__", , drop = FALSE]
  if (nrow(e) == 0L) return(NA_real_)
  chains <- sort(unique(e$Chain))
  vapply(chains, function(ch) {
    x <- e$Value[e$Chain == ch][order(e$Iteration[e$Chain == ch])]
    denom <- sum((x - mean(x))^2)
    if (length(x) < 2L || !is.finite(denom) || denom == 0) return(NA_real_)
    sum(diff(x)^2) / denom
  }, numeric(1))
}

#' Sampler diagnostics, requiring finite observations from every fitted chain
ap6_sampler_summary <- function(nuts, max_treedepth, chains) {
  complete <- function(parameter) {
    d <- nuts[nuts$Parameter == parameter, , drop = FALSE]
    nrow(d) > 0L && all(is.finite(d$Value)) &&
      setequal(unique(d$Chain), seq_len(chains))
  }
  divergence <- nuts$Value[nuts$Parameter == "divergent__"]
  depth <- nuts$Value[nuts$Parameter == "treedepth__"]
  bfmi <- if (complete("energy__")) ap6_bfmi(nuts) else NA_real_
  list(
    n_divergent = if (complete("divergent__")) as.integer(sum(divergence > 0)) else NA_integer_,
    n_treedepth_hits = if (is.finite(max_treedepth) && complete("treedepth__"))
      as.integer(sum(depth >= max_treedepth)) else NA_integer_,
    bfmi_min = if (length(bfmi) == chains && all(is.finite(bfmi))) min(bfmi) else NA_real_
  )
}

#' Validity gate of one fit (analysis_plan$regression$validity_gate)
#'
#' Evaluates the focal population-level coefficients (`b_` parameters)
#' against the profile's bulk and tail ESS target and the gate's maximum
#' R-hat, and the sampler against the maximum numbers of divergent
#' transitions and of transitions at the maximum tree depth and the minimum
#' BFMI per chain. The Monte Carlo SEs of the posterior medians and of the
#' interval endpoints (`posterior::mcse_median`, `posterior::mcse_quantile`
#' at the `analysis_plan$regression$ci_level` bounds) are reported, not gated. It
#' reports rather than stops; [apply_validity_gate()] applies the failure rule and AP10
#' uses `ok` / `gate_status` as its decision gate.
#'
#' @param fit A `brmsfit`.
#' @param analysis_plan Configuration from `zm_config()` or `NULL` (settings stored on
#'   the fit).
#' @return One-row tibble: observed `ess_bulk_min`, `ess_tail_min`,
#'   `rhat_max`, `n_divergent`, `n_treedepth_hits`, `bfmi_min`,
#'   `mcse_median_max`, `mcse_q_max`; thresholds `ess_target`, `rhat_limit`,
#'   `divergences_max`, `treedepth_hits_max`, `bfmi_limit`;
#'   `max_treedepth_used`; component flags `ess_ok`, `rhat_ok`,
#'   `divergences_ok`, `treedepth_ok`, `bfmi_ok`; `ok`; `gate_note`.
ap6_fit_diagnostics <- function(fit, analysis_plan = NULL) {
  reg <- ap6_reg_settings(fit, analysis_plan)
  gate <- ap6_gate_settings(reg)
  probs <- ap6_ci_probs(reg$ci_level)
  ess <- ap6_ess_check(fit, analysis_plan)
  draws <- posterior::as_draws_array(fit)
  b_vars <- ap6_focal_b_vars(fit)
  s <- suppressMessages(posterior::summarise_draws(
    posterior::subset_draws(draws, variable = b_vars),
    rhat = posterior::rhat, mcse_median = posterior::mcse_median,
    ~posterior::mcse_quantile(.x, probs = probs)
  ))
  mcse_q_cols <- grep("^mcse_q", names(s), value = TRUE)
  finite_max <- function(x) if (length(x) == 0L || any(!is.finite(x))) NA_real_ else max(x)
  rhat_max <- finite_max(s$rhat)
  mcse_median_max <- finite_max(s$mcse_median)
  mcse_q_max <- finite_max(unlist(s[, mcse_q_cols, drop = FALSE]))
  rhat_ok <- is.finite(rhat_max) && rhat_max <= gate$rhat_max

  nuts <- tryCatch(suppressWarnings(brms::nuts_params(fit)), error = function(e) e)
  notes <- character(0)
  if (inherits(nuts, "error")) {
    n_divergent <- NA_integer_
    n_treedepth_hits <- NA_integer_
    bfmi_min <- NA_real_
    notes <- c(notes, paste0("sampler diagnostics unavailable: ", conditionMessage(nuts)))
  } else {
    sampler <- ap6_sampler_summary(nuts, ap6_max_treedepth(fit), posterior::nchains(draws))
    n_divergent <- sampler$n_divergent
    n_treedepth_hits <- sampler$n_treedepth_hits
    bfmi_min <- sampler$bfmi_min
  }
  divergences_ok <- !is.na(n_divergent) && n_divergent <= gate$divergences_max
  treedepth_ok <- !is.na(n_treedepth_hits) && n_treedepth_hits <= gate$treedepth_hits_max
  bfmi_ok <- is.finite(bfmi_min) && bfmi_min >= gate$bfmi_min
  fmt <- function(x) format(x, digits = 4)
  if (!isTRUE(ess$ok)) {
    notes <- c(notes, paste0("ESS shortfall: bulk ", fmt(ess$min_ess_bulk), ", tail ", fmt(ess$min_ess_tail),
                             " < target ", fmt(gate$ess_target)))
  }
  if (!rhat_ok) notes <- c(notes, if (!is.finite(rhat_max)) "R-hat unavailable" else paste0("R-hat ", fmt(rhat_max), " > ", fmt(gate$rhat_max)))
  if (!divergences_ok) notes <- c(notes, if (is.na(n_divergent)) "divergence diagnostics unavailable" else paste0(n_divergent, " divergent transitions > ", fmt(gate$divergences_max)))
  if (!treedepth_ok) {
    notes <- c(notes, paste0(if (is.na(n_treedepth_hits)) "tree-depth hits unknown" else
      paste0(n_treedepth_hits, " transitions at max_treedepth > ", fmt(gate$treedepth_hits_max))))
  }
  if (!bfmi_ok) notes <- c(notes, if (!is.finite(bfmi_min)) "BFMI unavailable for one or more chains" else paste0("BFMI ", fmt(bfmi_min), " < ", fmt(gate$bfmi_min)))
  ok <- isTRUE(ess$ok) && rhat_ok && divergences_ok && treedepth_ok && bfmi_ok
  tibble::tibble(
    ok = ok,
    ess_ok = isTRUE(ess$ok),
    rhat_ok = rhat_ok,
    divergences_ok = divergences_ok,
    treedepth_ok = treedepth_ok,
    bfmi_ok = bfmi_ok,
    ess_bulk_min = as.numeric(ess$min_ess_bulk),
    ess_tail_min = as.numeric(ess$min_ess_tail),
    rhat_max = rhat_max,
    n_divergent = n_divergent,
    n_treedepth_hits = n_treedepth_hits,
    bfmi_min = bfmi_min,
    mcse_median_max = mcse_median_max,
    mcse_q_max = mcse_q_max,
    ess_target = gate$ess_target,
    rhat_limit = gate$rhat_max,
    divergences_max = gate$divergences_max,
    treedepth_hits_max = gate$treedepth_hits_max,
    bfmi_limit = gate$bfmi_min,
    max_treedepth_used = ap6_max_treedepth(fit),
    gate_note = if (length(notes) == 0L) "ok" else paste(notes, collapse = "; ")
  )
}

#' Sampling arguments of a brms call
#'
#' @param reg The `regression` block.
#' @param warmup Warmup iterations per chain.
#' @param draws_per_chain Post-warmup draws per chain.
#' @param control Optional `control` list (`adapt_delta`, `max_treedepth`).
#' @return Named list of arguments for `brms::brm()` / `update()`.
ap6_sampling_args <- function(reg, warmup, draws_per_chain, control = NULL) {
  out <- list(
    chains = as.integer(reg$chains), cores = as.integer(reg$cores), seed = as.integer(reg$seed),
    iter = as.integer(warmup + draws_per_chain), warmup = as.integer(warmup),
    # The chains are not thinned. Although this is the brms default, the
    # preregistered setting is passed explicitly so that a library default
    # cannot change it silently and the run specification can print it.
    thin = 1L,
    refresh = 0, silent = 2
  )
  if (!is.null(control)) out$control <- control
  out
}

#' Label of a slope SD of the sweep
#'
#' Looks the SD up in `analysis_plan$priors$sweep_labels` by its key ([zm_sweep_key()]).
#'
#' @param sd Numeric vector of slope SDs.
#' @param analysis_plan Configuration from `zm_config()`.
#' @return Character vector of labels.
ap6_sweep_label <- function(sd, analysis_plan) {
  labels <- analysis_plan$priors$sweep_labels
  vapply(zm_sweep_key(sd), function(k) as.character(labels[[k]]), character(1), USE.NAMES = FALSE)
}

#' Key of a model column, as the codebook names it
#'
#' `analysis_plan$regression$term_keys` carries the codebook's column-to-key pairs
#' (built once in [zm_config()]), so a coefficient is reported by its key
#' without any function stripping a suffix. A term that is not a model column
#' — a gender contrast, brms's `Intercept` — is returned unchanged.
#'
#' @param term Character vector of model terms.
#' @param reg The `regression` block of the configuration.
#' @return Character vector of keys.
ap6_term_key <- function(term, reg) {
  key <- unname(reg$term_keys[as.character(term)])
  ifelse(is.na(key), as.character(term), key)
}

#' Clean parameter names and classify terms
#'
#' `b_zm_security_z` -> `zm_security` (predictor), `b_age_z` -> `age` (covariate),
#' `b_genderfemale` -> `genderfemale` (covariate, kept as brms names it),
#' `b_Intercept` -> `Intercept` (intercept), `sigma` -> `sigma`, `nu`
#' (student family) -> `auxiliary`. In a multivariate fit the response prefix
#' of the focal outcome (`b_ascaggz_...`, `sigma_ascaggz`) is stripped as
#' well.
#'
#' @param variables Character vector of brms parameter names.
#' @param reg The `regression` block of the configuration.
#' @param resp Focal response name ([ap6_focal_resp()]); `""` when univariate.
#' @return Tibble `variable`, `term`, `term_type`.
ap6_clean_terms <- function(variables, reg, resp = "") {
  motives <- as.character(reg$motives)
  cov_base <- as.character(reg$covariate_keys)
  term <- variables
  term_type <- rep("other", length(variables))
  is_b <- grepl(ap6_focal_pattern(resp), variables)
  term[is_b] <- sub(ap6_focal_pattern(resp), "", variables[is_b])
  is_int <- is_b & term == "Intercept"
  term_type[is_int] <- "intercept"
  slope <- is_b & !is_int
  term[slope] <- ap6_term_key(term[slope], reg)
  is_pred <- slope & term %in% motives
  term_type[is_pred] <- "predictor"
  is_cov <- slope & !is_pred &
    vapply(term, function(t) any(startsWith(t, cov_base)), logical(1))
  term_type[is_cov] <- "covariate"
  sigma_name <- if (nzchar(resp)) paste0("sigma_", resp) else "sigma"
  nu_name <- if (nzchar(resp)) paste0("nu_", resp) else "nu"
  term[variables == sigma_name] <- "sigma"
  term_type[variables == sigma_name] <- "sigma"
  term[variables == nu_name] <- "nu"
  term_type[variables == nu_name] <- "auxiliary"
  tibble::tibble(variable = variables, term = term, term_type = term_type)
}

#' Residual-variance factor of a fit's family
#'
#' The residual variance is `sigma^2 * factor`: 1 for the gaussian family,
#' `nu / (nu - 2)` for the student family (2 at the fixed nu = 4).
#'
#' @param fit A `brmsfit` gated by [apply_validity_gate()].
#' @return `list(factor, nu)`; `nu` is `NA` for gaussian fits.
ap6_residual_variance_factor <- function(fit) {
  family_name <- ap6_focal_family(fit)
  if (identical(family_name, "gaussian")) return(list(factor = 1, nu = NA_real_))
  nu <- attr(fit, "nu_fixed")
  if (nu <= 2) stop("nu must exceed 2 for a finite residual variance.")
  list(factor = nu / (nu - 2), nu = as.numeric(nu))
}

#' Draws of the model-based Bayesian R2 (analysis_plan$regression$r2)
#'
#' Draw-wise `var(mu) / (var(mu) + residual variance)`, where `mu` is the
#' expected outcome of every observation (`brms::posterior_epred`), the
#' variance is taken over observations within a draw, and the residual
#' variance is `sigma^2` (gaussian) or `sigma^2 * nu / (nu - 2)` (student;
#' `2 * sigma^2` at nu = 4). One definition for both families.
#'
#' @param fit A `brmsfit` gated by [apply_validity_gate()].
#' @return Numeric vector, one value per posterior draw, with attribute
#'   `definition` (character).
ap6_r2_draws <- function(fit) {
  resp <- ap6_focal_resp(fit)
  args <- list(object = fit)
  if (nzchar(resp)) args$resp <- resp
  mu <- do.call(brms::posterior_epred, args)
  var_mu <- apply(mu, 1, stats::var)
  sigma <- as.numeric(posterior::as_draws_matrix(fit)[, ap6_sigma_var(fit)])
  if (length(sigma) != length(var_mu)) stop("sigma draws and epred draws differ in length.")
  rv <- ap6_residual_variance_factor(fit)
  r2 <- var_mu / (var_mu + rv$factor * sigma^2)
  attr(r2, "definition") <- if (is.na(rv$nu)) {
    "var(mu) / (var(mu) + sigma^2)"
  } else {
    sprintf("var(mu) / (var(mu) + %s * sigma^2) [student, nu = %s fixed]", format(rv$factor), format(rv$nu))
  }
  r2
}

#' Observed response of a fit
#'
#' The response column is read off the fitted formula, whose response is the
#' model sample's standardised outcome ([define_regression_model_set()]).
#'
#' @param fit A `brmsfit` gated by [apply_validity_gate()].
#' @return Numeric vector over the rows brms kept.
ap6_response <- function(fit) {
  z_col <- all.vars(stats::formula(fit)$formula)[1]
  as.numeric(fit$data[[z_col]])
}

#' Posterior (or prior) predictive draws of the focal outcome
#'
#' @param fit A `brmsfit` gated by [apply_validity_gate()].
#' @param ... Passed to `brms::posterior_predict()` (e.g. `ndraws`).
#' @return Draws x observations matrix.
ap6_predict_focal <- function(fit, ...) {
  args <- list(object = fit, ...)
  resp <- ap6_focal_resp(fit)
  if (nzchar(resp)) args$resp <- resp
  do.call(brms::posterior_predict, args)
}

#' Posterior predictive draws in long form (for figures)
#'
#' @param fit A `brmsfit` gated by [apply_validity_gate()].
#' @param n_draws Number of replicated data sets.
#' @return Tibble `outcome`, `type` (`"y"` observed, `"y_rep"` replicated),
#'   `draw` (`NA` for observed), `obs` (row index), `value`.
ap6_pp_check_data <- function(fit, n_draws = 100) {
  y <- ap6_response(fit)
  seed <- attr(fit, "seed")
  if (is.null(seed)) stop("ap6_pp_check_data(): the fit has no seed attribute; pass a fit gated by apply_validity_gate().")
  yrep <- ap6_reg_with_seed(seed, ap6_predict_focal(fit, ndraws = n_draws))
  n <- length(y)
  outcome <- attr(fit, "outcome")
  observed <- tibble::tibble(
    outcome = outcome, type = "y", draw = NA_integer_, obs = seq_len(n), value = y
  )
  replicated <- tibble::tibble(
    outcome = outcome, type = "y_rep",
    draw = rep(seq_len(nrow(yrep)), times = n),
    obs = rep(seq_len(n), each = nrow(yrep)),
    value = as.vector(yrep)
  )
  dplyr::bind_rows(observed, replicated)
}

#' Bayesian R2 draws in the shape priorsense expects
#'
#' A `draws_array` (iterations x chains x 1) named `bayes_R2`, computed by
#' [ap6_r2_draws()]; passed as the `prediction` function of
#' `priorsense::powerscale_sensitivity()` so that the derived quantity is
#' power-scaled like a parameter.
#'
#' @param fit A `brmsfit` gated by [apply_validity_gate()].
#' @param ... Ignored (priorsense forwards its own arguments).
#' @return A `draws_array`.
ap6_r2_prediction <- function(fit, ...) {
  r2 <- as.numeric(ap6_r2_draws(fit))
  n_chains <- posterior::nchains(fit)
  out <- posterior::as_draws_array(array(r2, dim = c(length(r2) / n_chains, n_chains, 1L)))
  posterior::variables(out) <- "bayes_R2"
  out
}

#' Largest Pareto k of the power-scaling importance weights
#'
#' `priorsense::powerscale()` at the lower and upper alpha for the prior
#' (with the block's selection) and the likelihood; the maximum `khat` of the
#' four runs.
#'
#' @param fit A `brmsfit`.
#' @param prior_selection Prior tag or `NULL` (all priors).
#' @param lower_alpha,upper_alpha Power-scaling alphas.
#' @param variable One parameter name (the weights do not depend on it).
#' @param extra Named list of extra arguments (e.g. `resp`).
#' @return Numeric scalar, `NA` when any run fails.
ap6_pareto_k_max <- function(fit, prior_selection, lower_alpha, upper_alpha, variable, extra = list()) {
  runs <- list(
    list(component = "prior", alpha = lower_alpha), list(component = "prior", alpha = upper_alpha),
    list(component = "likelihood", alpha = lower_alpha), list(component = "likelihood", alpha = upper_alpha)
  )
  ks <- vapply(runs, function(r) {
    args <- c(list(x = fit, component = r$component, alpha = r$alpha, variable = variable), extra)
    if (r$component == "prior") args$selection <- prior_selection
    res <- tryCatch(suppressWarnings(do.call(priorsense::powerscale, args)), error = function(e) NULL)
    k <- attr(res, "powerscaling")$diagnostics$khat
    if (is.null(k) || length(k) != 1L) NA_real_ else as.numeric(k)
  }, numeric(1))
  if (anyNA(ks)) NA_real_ else max(ks)
}

#' Provenance of one fit
#'
#' Everything an external reviewer needs to reproduce the fit: outcome, slope
#' SD, family, the deparsed formula, the number of observations, the prior
#' table (`brms::prior_summary()` as one string), a hash of the Stan program
#' (`digest::digest()` of `brms::stancode()`, sha256) and the versions of
#' brms, cmdstanr and CmdStan; also the sampling settings and the gate
#' status. [ap6_record_fit()] stores the row as attribute `provenance`.
#'
#' @param fit A `brmsfit` gated by [apply_validity_gate()].
#' @return One-row tibble `outcome`, `slope_sd`, `family`, `formula`, `nobs`,
#'   `priors`, `stan_code_hash`, `hash_algorithm`,
#'   `chains`, `warmup`, `iter_per_chain`, `seed`, `gate_status`,
#'   `brms_version`, `cmdstanr_version`, `cmdstan_version`.
ap6_fit_provenance <- function(fit) {
  pr <- as.data.frame(brms::prior_summary(fit))
  qualifier <- paste0(
    ifelse(nzchar(pr$coef), paste0("[", pr$coef, "]"), ""),
    ifelse(nzchar(pr$resp), paste0("@", pr$resp), ""),
    if ("tag" %in% names(pr)) ifelse(nzchar(pr$tag), paste0(" {", pr$tag, "}"), "") else ""
  )
  prior_text <- paste(paste0(pr$class, qualifier, ": ", pr$prior), collapse = "; ")
  code <- brms::stancode(fit)
  outcome <- attr(fit, "outcome")
  slope_sd <- attr(fit, "slope_sd")
  gate_status <- attr(fit, "gate_status")
  formula_text <- attr(fit, "formula_text")
  cmdstan_version <- cmdstanr::cmdstan_version()
  tibble::tibble(
    outcome = outcome,
    slope_sd = as.numeric(slope_sd),
    family = ap6_focal_family(fit),
    formula = formula_text,
    nobs = as.integer(stats::nobs(fit)),
    priors = prior_text,
    stan_code_hash = digest::digest(code, algo = "sha256", serialize = FALSE),
    hash_algorithm = "sha256",
    chains = as.integer(fit$fit@sim$chains),
    warmup = as.integer(fit$fit@sim$warmup),
    iter_per_chain = as.integer(fit$fit@sim$iter - fit$fit@sim$warmup),
    seed = as.integer(attr(fit, "seed")),
    gate_status = gate_status,
    brms_version = as.character(utils::packageVersion("brms")),
    cmdstanr_version = as.character(utils::packageVersion("cmdstanr")),
    cmdstan_version = as.character(cmdstan_version)
  )
}

# ---- AP7 — joint estimation for RQ2 (one multivariate model, correlated residuals) ----
#
# The four outcomes are additionally estimated in one multivariate model with
# correlated residuals. It uses the same predictors and primary coefficient
# and residual-SD priors as the separate regressions. AP3 prepares its
# known-gender sample with every required value available and computes the
# standardisation constants on that sample. The residual correlation matrix
# receives analysis_plan$priors$rescor (LKJ(2)).
#
# Shared predictors and common rows do not guarantee identical marginal
# posteriors: the correlated likelihood and informative priors can change the
# estimates even on common rows. RQ2 reads the direction of one motive's adjusted
# coefficients and the differences between them from paired draws of this
# joint posterior. Each coefficient uses its outcome's joint-sample
# standardisation constants. The fit also reports residual correlations among
# the facets and is subject to the same validity gate as the separate models.
# The separate primary fits determine every RQ1 Table 3 verdict.

# ---- AP6 — the regression architecture of the pipeline -----------------------
#
# The four preregistered regressions: one model-set noun, the fitting verbs,
# one validity gate, the preregistered checks, and the result verbs the report
# and the AP10 decisions read. The validity gate runs the two stages at the top
# of this file, ap6_refit_if_needed() and ap6_check_final_convergence().

#' AP6 — the stable prediction key of every readable outcome name
#'
#' The model definitions carry readable names (`authoritarian_aggression`);
#' the prediction table and every AP10 join use the configured keys
#' (`asc_agg`). The relationship is resolved once, here, from the formulas' own
#' response columns and `analysis_plan$regression$term_keys`, so neither side of the map
#' is typed a second time. A duplicate or unknown mapping stops.
#'
#' @param formulas The named formulas of the model set.
#' @param analysis_plan Configuration from [zm_config()].
#' @return Named character vector: readable outcome name -> configured key.
ap6_regression_outcome_keys <- function(formulas, analysis_plan) {
  if (length(formulas) == 0L) stop("The regression model set declares no formula.")
  outcome_names <- names(formulas)
  if (is.null(outcome_names) || any(!nzchar(outcome_names))) {
    stop("Every regression formula needs its readable outcome name.")
  }
  response_columns <- vapply(formulas, function(formula) all.vars(formula)[[1L]], character(1),
                             USE.NAMES = FALSE)
  keys <- unname(analysis_plan$regression$term_keys[response_columns])
  if (anyNA(keys)) {
    stop("analysis_plan$regression$term_keys has no prediction key for the response column(s): ",
         paste(response_columns[is.na(keys)], collapse = ", "), ".")
  }
  unknown <- setdiff(keys, as.character(analysis_plan$regression$outcomes))
  if (length(unknown) > 0L) {
    stop("The regression formulas name outcome key(s) the plan does not register: ",
         paste(unknown, collapse = ", "), ".")
  }
  if (anyDuplicated(keys) > 0L || anyDuplicated(outcome_names) > 0L ||
      anyDuplicated(response_columns) > 0L) {
    stop("Outcome names, response columns and prediction keys must be one to one.")
  }
  stats::setNames(keys, outcome_names)
}

#' AP6 — outcome name, prediction key and response column of every model
#'
#' The one table every result record resolves its identity from. It is derived
#' from the model set, which `analysis_plan` alone determines, so no consumer recovers an
#' identity from a row position.
#'
#' @param analysis_plan Configuration from [zm_config()].
#' @return Tibble `outcome`, `outcome_key`, `response_column`.
ap6_regression_outcome_identity <- function(analysis_plan) {
  model_set <- define_regression_model_set(analysis_plan)
  tibble::tibble(
    outcome = names(model_set$formulas),
    outcome_key = unname(model_set$outcome_keys[names(model_set$formulas)]),
    response_column = vapply(model_set$formulas, function(formula) all.vars(formula)[[1L]],
                             character(1), USE.NAMES = FALSE)
  )
}

#' Label the plan gives one fit of the register
#'
#' @param role `"primary"`, `"sweep"` or `"student_refit"`.
#' @param slope_sd Slope prior SD of the fit.
#' @param analysis_plan Configuration from [zm_config()].
#' @return Character scalar.
ap6_regression_fit_label <- function(role, slope_sd, analysis_plan) {
  if (role %in% c("primary", "sweep")) return(ap6_sweep_label(slope_sd, analysis_plan))
  label <- analysis_plan$regression$fit_roles[[role]]
  if (is.null(label)) stop("analysis_plan$regression$fit_roles has no label for the fit role '", role, "'.")
  as.character(label)
}

# BEGIN GENERATED PARAMETER CARD: AP6 FORMULA
# Automatically generated from analysis_plan.yaml
# One regression per outcome:
#   asc_agg_z | asc_sub_z | asc_conv_z | sdo_dom_z
# The same predictors in every regression:
#   zm_security_z + zm_arousal_z + zm_power_z + zm_prestige_z + zm_achievement_z + age_z + gender +
#   income_z
# END GENERATED PARAMETER CARD: AP6 FORMULA
# BEGIN GENERATED PARAMETER CARD: AP6 FAMILY
# Automatically generated from analysis_plan.yaml
# Residuals: gaussian.
# END GENERATED PARAMETER CARD: AP6 FAMILY
# BEGIN GENERATED PARAMETER CARD: AP6 PRIORS
# Automatically generated from analysis_plan.yaml
# Slopes of the metric predictors: normal(0, 0.2).
# Gender contrasts: normal(0, 0.4).
# Centred intercept: normal(0, 0.2).
# Residual SD: half-normal(0, 1).
# Sweep of the slope width: 0.1 (skeptical) | 0.4 (permissive).
# Robustness refit with Student-t residuals: nu fixed at 4; scale half-normal(0, 0.7071).
# END GENERATED PARAMETER CARD: AP6 PRIORS
#' AP6 — declare the four preregistered regression models
#'
#' The formulas, population, likelihood, and priors are fixed before fitting.
#' One formula per configured outcome, in the plan's (the codebook's) order of
#' the outcomes: the outcome's standardised column on the predictor side the
#' plan states, `analysis_plan$regression$predictors`, which [zm_config()]
#' builds from the motives in codebook order and the covariates, e.g.
#' `asc_agg_z ~ zm_security_z + zm_arousal_z + zm_power_z + zm_prestige_z + zm_achievement_z + age_z + gender + income_z`.
define_regression_model_set <- function(analysis_plan) {
  # The readable name of each outcome's model, by its key; the names identify
  # the fits in every result record.
  model_names <- c(asc_agg = "authoritarian_aggression", asc_sub = "authoritarian_submission",
                   asc_conv = "conventionalism", sdo_dom = "sdo_d_dominance")
  outcomes <- as.character(analysis_plan$regression$outcomes)
  unnamed <- setdiff(outcomes, names(model_names))
  if (length(unnamed) > 0L) {
    stop("The regression model set has no model name for the outcome(s): ", paste(unnamed, collapse = ", "), ".")
  }
  term_keys <- analysis_plan$regression$term_keys
  response_columns <- names(term_keys)[match(outcomes, term_keys)]
  formulas <- stats::setNames(lapply(response_columns, function(column) {
    formula <- stats::as.formula(paste(column, "~", analysis_plan$regression$predictors))
    environment(formula) <- globalenv()   # so the fit does not carry analysis_plan along
    formula
  }), unname(model_names[outcomes]))
  list(
    formulas = formulas,
    # The one explicit relationship between the readable model names and the
    # configured prediction keys.
    outcome_keys = ap6_regression_outcome_keys(formulas, analysis_plan),
    family = brms::brmsfamily(analysis_plan$regression$family),  # registered Gaussian
    metric_slope_priors = analysis_plan$priors$slope_sd_sweep,  # .10, .20 primary, .40
    primary_slope_sd = analysis_plan$priors$slope_sd_primary,   # .20
    gender_prior_sd = analysis_plan$priors$gender_sd,           # normal(0, .40)
    intercept_prior_sd = analysis_plan$priors$intercept_sd,     # normal(0, .20)
    sigma_prior_sd = analysis_plan$priors$sigma_sd              # half-normal(0, 1)
  )
}

# BEGIN GENERATED PARAMETER CARD: AP6 SAMPLING
# Automatically generated from analysis_plan.yaml
# Chains: 4.
# Warmup draws per chain: 2000.
# Posterior draws per chain: 6000.
# Seed: 20260905.
# Backend: cmdstanr.
# Sampler control: the sampler's defaults.
# Profile smoke instead: warmup draws per chain 500, posterior draws per chain 1000.
# Profile presentation instead: warmup draws per chain 300, posterior draws per chain 300.
# END GENERATED PARAMETER CARD: AP6 SAMPLING
#' AP6 — fit the four primary Gaussian regressions
#'
#' Each direct brms call uses the registered formula, known-gender data, and
#' primary priors.
fit_primary_regressions <- function(data, model_set, analysis_plan) {
  sampling <- analysis_plan$regression  # full profile: 4 chains, 2,000 warmup, 6,000 post-warmup, seed 20260905
  ap6_iterate_regression_fits(model_set$formulas, function(formula) {
    data <- select_regression_data_for_model(data, all.vars(formula))
    require_valid_imputation_inputs(data, all.vars(formula))
    priors <- c(
      brms::set_prior(paste0("normal(0, ", model_set$primary_slope_sd, ")"), class = "b", tag = "metric_slopes"),
      ap6_build_gender_contrast_priors(formula, data, model_set$gender_prior_sd),
      brms::set_prior(paste0("normal(0, ", model_set$intercept_prior_sd, ")"), class = "Intercept", tag = "intercept"),
      brms::set_prior(paste0("normal(0, ", model_set$sigma_prior_sd, ")"), class = "sigma", tag = "sigma")
    )
    brms::brm(
      formula = formula,
      data = data,
      family = model_set$family,
      prior = priors,
      backend = analysis_plan$regression$backend,
      chains = sampling$chains,
      cores = sampling$cores,
      warmup = sampling$warmup,
      iter = sampling$warmup + sampling$iter_per_chain,
      thin = 1,  # preregistered: no thinning
      seed = sampling$seed,
      refresh = 0
    )
  })
}

#' AP6 — fit the two non-primary prior-width comparisons
#'
#' Refit each equation only at .10 and .40; .20 is already in
#' `regression_primary_fits`.
fit_prior_width_comparisons <- function(data, model_set, analysis_plan) {
  sampling <- analysis_plan$regression  # full profile: 4 chains, 2,000 warmup, 6,000 post-warmup
  comparison_priors <- model_set$metric_slope_priors[
    model_set$metric_slope_priors != model_set$primary_slope_sd
  ]  # registered .10 skeptical and .40 permissive
  comparison_fits <- comparison_priors |>
    lapply(function(slope_sd) {
    ap6_iterate_regression_fits(model_set$formulas, function(formula) {
      data <- select_regression_data_for_model(data, all.vars(formula))
      require_valid_imputation_inputs(data, all.vars(formula))
      priors <- c(
        brms::set_prior(paste0("normal(0, ", slope_sd, ")"), class = "b", tag = "metric_slopes"),
        ap6_build_gender_contrast_priors(formula, data, model_set$gender_prior_sd),
        brms::set_prior(paste0("normal(0, ", model_set$intercept_prior_sd, ")"), class = "Intercept", tag = "intercept"),
        brms::set_prior(paste0("normal(0, ", model_set$sigma_prior_sd, ")"), class = "sigma", tag = "sigma")
      )
      brms::brm(
        formula = formula,
        data = data,
        family = model_set$family,
        prior = priors,
        backend = analysis_plan$regression$backend,
        chains = sampling$chains,
        cores = sampling$cores,
        warmup = sampling$warmup,
        iter = sampling$warmup + sampling$iter_per_chain,
        thin = 1,
        seed = sampling$seed,
        refresh = 0
      )
    })
  })
  names(comparison_fits) <- sprintf("%.2f", comparison_priors)
  comparison_fits
}

# BEGIN GENERATED PARAMETER CARD: AP6 INTERVAL LEVEL PRIOR WIDTH
# Automatically generated from analysis_plan.yaml
# Coefficient stability across the three registered prior widths
#   Interval level: 0.95   (regression.ci_level)
#   Central, equal-tailed: quantiles 0.025 and 0.975
# END GENERATED PARAMETER CARD: AP6 INTERVAL LEVEL PRIOR WIDTH
#' AP6 — report coefficient stability across the three registered prior widths
extract_regression_prior_width_sensitivity <- function(primary_fits, comparison_fits, analysis_plan) {
  width_fits <- ap6_arrange_prior_width_fits(
    primary_fits, comparison_fits,
    widths = analysis_plan$priors$slope_sd_sweep,
    primary_width = analysis_plan$priors$slope_sd_primary
  )
  ap6_summarise_coefficient_stability(
    width_fits,
    widths = names(width_fits),
    interval_level = analysis_plan$regression$ci_level,
    indicators = c("interval_excludes_zero", "median_direction"),
    declared_coefficients = c("b_Intercept", paste0("b_", names(analysis_plan$regression$term_keys)))
  )
}

# BEGIN GENERATED PARAMETER CARD: AP6 INITIAL CONVERGENCE
# Automatically generated from analysis_plan.yaml
# Effective sample size required of every coefficient: 10000 bulk and 10000 tail.
# Profile smoke instead: 2000 bulk and 2000 tail.
# Profile presentation instead: 200 bulk and 200 tail.
# ESS retries allowed after this check: at most 3 doublings.
# END GENERATED PARAMETER CARD: AP6 INITIAL CONVERGENCE
#' AP6 — apply the preregistered sampling-validity gate
#'
#' Remedy failures in this order; no prior or scientific model changes to make
#' a fit pass. Every fit of the collection runs the two stages at the top of
#' this file ([ap6_gate_one_regression_fit()]); a fit that still fails is
#' marked not interpretable. The final diagnostics, gate status and retry
#' history are attached to each fit. It never changes a formula, prior, data
#' row, or standardisation constant.
apply_validity_gate <- function(fits, analysis_plan) {
  max_doublings <- analysis_plan$regression$validity_gate$max_ess_doublings  # 3
  ap6_map_regression_fits(fits, function(fit, outcome, slope_sd) {
    if (is.null(fit) || inherits(fit, "error")) return(fit)
    ap6_gate_one_regression_fit(fit, outcome, slope_sd, max_doublings, analysis_plan)
  })
}

# BEGIN GENERATED PARAMETER CARD: AP6 PRIOR PREDICTIVE SUMMARY
# Automatically generated from analysis_plan.yaml
# Descriptive summaries of the prior-only draws
#   Percentile of the absolute standardised prediction: 0.95   (regression.prior_predictive.absolute_summary_percentile)
#   Quantiles reported on the response scale: 0.05, 0.5, 0.95   (regression.prior_predictive.raw_scale_quantiles)
# Response range the raw-scale draws are compared against
#   1 to 6   (scales.response_min, scales.response_max)
# END GENERATED PARAMETER CARD: AP6 PRIOR PREDICTIVE SUMMARY
#' AP6 — assess what the registered priors imply before posterior interpretation
#'
#' Fit prior-only versions at Gaussian .10/.20/.40 and Student nu = 4 at .20.
check_prior_predictions <- function(data, model_set, analysis_plan) {
  sampling <- analysis_plan$regression
  student <- analysis_plan$sensitivity$student_t
  specifications <- rbind(
    data.frame(family = "gaussian", slope_sd = model_set$metric_slope_priors),
    data.frame(family = "student", slope_sd = model_set$primary_slope_sd)
  )
  names(model_set$formulas) |>
    lapply(function(outcome) {
    formula <- model_set$formulas[[outcome]]
    data <- select_regression_data_for_model(data, all.vars(formula))
    unavailable <- ap6_capture_imputation_unavailability(
      require_valid_imputation_inputs(data, all.vars(formula))
    )
    if (inherits(unavailable, "imputation_unavailable")) {
      return(lapply(seq_len(nrow(specifications)), function(i) {
        build_unavailable_prior_prediction_record(outcome, formula, specifications[i, ], unavailable, analysis_plan)
      }))
    }
    response_column <- all.vars(formula)[[1]]
    outcome_standardisation <- ap6_select_outcome_standardisation(
      response_column,
      attr(data, "z_parameters")
    )
    lapply(seq_len(nrow(specifications)), function(i) {
      specification <- specifications[i, ]
      is_student <- specification$family == "student"
      priors <- c(
        brms::set_prior(paste0("normal(0, ", specification$slope_sd, ")"), class = "b", tag = "metric_slopes"),
        ap6_build_gender_contrast_priors(formula, data, model_set$gender_prior_sd),
        brms::set_prior(paste0("normal(0, ", model_set$intercept_prior_sd, ")"), class = "Intercept", tag = "intercept"),
        brms::set_prior(paste0("normal(0, ", if (is_student) student$sigma_scale_prior_sd else model_set$sigma_prior_sd, ")"), class = "sigma", tag = "sigma"),
        if (is_student) brms::set_prior(paste0("constant(", student$nu_fixed, ")"), class = "nu")
      )
      prior_fit <- brms::brm(
        formula = formula,
        data = data,
        family = brms::brmsfamily(specification$family),
        prior = priors,
        sample_prior = "only",
        backend = analysis_plan$regression$backend,
        chains = sampling$chains,
        cores = sampling$cores,
        warmup = sampling$warmup,
        iter = sampling$warmup + sampling$iter_per_chain,
        thin = 1,
        seed = sampling$seed,
        refresh = 0
      )
      standardised_predictions <- brms::posterior_predict(prior_fit)
      raw_scale_predictions <- standardised_predictions * outcome_standardisation$sd +
        outcome_standardisation$mean
      response_min <- analysis_plan$scales$response_min
      response_max <- analysis_plan$scales$response_max
      list(
        outcome = outcome,
        status = "available", note = NA_character_,
        response_column = response_column,
        fixed_z_constants = outcome_standardisation,
        family = specification$family,
        slope_sd = specification$slope_sd,
        standardised_predictions = standardised_predictions,
        raw_scale_predictions = raw_scale_predictions,
        # The registered prior-predictive report states these descriptive
        # quantities. They are summarised here, where the draws are made, so that
        # no report target has to summarise the retained draw matrices itself.
        standardised_summary = list(
          n_draws = nrow(standardised_predictions),
          n_observations = ncol(standardised_predictions),
          mean = mean(standardised_predictions),
          sd = stats::sd(standardised_predictions),
          p95_absolute = unname(stats::quantile(
            abs(standardised_predictions),
            analysis_plan$regression$prior_predictive$absolute_summary_percentile  # 0.95
          ))
        ),
        raw_scale_summary = list(
          raw_mean = mean(raw_scale_predictions),
          raw_sd = stats::sd(raw_scale_predictions),
          raw_quantiles = stats::quantile(
            raw_scale_predictions,
            probs = analysis_plan$regression$prior_predictive$raw_scale_quantiles  # .05, .50, .95
          ),
          raw_range = range(raw_scale_predictions),
          response_range = c(min = response_min, max = response_max),
          share_below_min = mean(raw_scale_predictions < response_min),
          share_above_max = mean(raw_scale_predictions > response_max),
          share_outside_range = mean(
            raw_scale_predictions < response_min | raw_scale_predictions > response_max
          )
        )
      )
    })
  })
}

# BEGIN GENERATED PARAMETER CARD: AP6 INTERVAL LEVEL POSTERIOR CHECK
# Automatically generated from analysis_plan.yaml
# Posterior-predictive replication summaries
#   Interval level: 0.95   (regression.ci_level)
#   Central, equal-tailed: quantiles 0.025 and 0.975
# END GENERATED PARAMETER CARD: AP6 INTERVAL LEVEL POSTERIOR CHECK
#' AP6 — compare the primary fits with replicated outcome distributions
#'
#' Mean and SD are included as preregistered checks even though standardisation
#' fixes them by construction.
check_posterior_predictions <- function(primary_fits, analysis_plan) {
  probabilities <- c((1 - analysis_plan$regression$ci_level) / 2,
                     1 - (1 - analysis_plan$regression$ci_level) / 2)  # central 95%
  primary_fits |>
    lapply(function(fit) {
    if (is.null(fit) || inherits(fit, "error")) {
      return(build_unavailable_posterior_prediction_checks(fit, analysis_plan))
    }
    response_column <- all.vars(stats::formula(fit)$formula)[[1]]
    observed <- fit$data[[response_column]]
    replicated <- brms::posterior_predict(fit)
    posterior_checks <- ap6_posterior_predictive_statistics(
      observed,
      replicated,
      statistics = c("mean", "sd", "skewness", "excess_kurtosis", "min", "max"),
      interval = probabilities
    )
    checks <- posterior_checks |>
      lapply(function(check) {
      list(
        statistic = check$statistic,
        observed = check$observed,
        replicated_interval = check$replicated_interval,
        replicated_median = check$replicated_median,
        interval_membership = if (
          check$observed >= check$replicated_interval[[1]] &&
            check$observed <= check$replicated_interval[[2]]
        ) "inside" else "outside",
        posterior_predictive_p_value = check$p_value
      )
    })
    # The replicated draws stay with the checks as an attribute, so that no
    # report target reruns `posterior_predict()` and the list keeps one entry
    # per statistic.
    attr(checks, "predictive_draws") <- ap6_pp_check_data(fit)
    attr(checks, "response_column") <- response_column
    attr(checks, "slope_sd") <- as.numeric(analysis_plan$priors$slope_sd_primary)
    attr(checks, "status") <- "available"
    attr(checks, "note") <- NA_character_
    checks
  })
}

# BEGIN GENERATED PARAMETER CARD: AP6 PRIOR SENSITIVITY
# Automatically generated from analysis_plan.yaml
#   Prior blocks: metric_slopes, gender_contrasts, intercept, sigma, all_priors   (sensitivity.powerscale.blocks)
#   Components scaled: prior, likelihood   (sensitivity.powerscale.component)
#   Power-scaling alphas: 0.99 and 1.01   (sensitivity.powerscale.lower_alpha, upper_alpha)
#   Divergence measure: cjs_dist   (sensitivity.powerscale.div_measure)
#   Sensitivity flagged at or above: 0.05   (sensitivity.powerscale.sensitivity_threshold)
#   Descriptive; no AP10 classification changes.
# END GENERATED PARAMETER CARD: AP6 PRIOR SENSITIVITY
#' AP6 — assess local prior and likelihood sensitivity of primary fits
#'
#' These diagnostics describe dependence on local power scaling and never alter
#' AP10 decisions.
extract_regression_prior_sensitivity <- function(primary_fits, analysis_plan) {
  powerscale <- analysis_plan$sensitivity$powerscale
  primary_fits |>
    lapply(function(fit) {
    if (is.null(fit) || inherits(fit, "error")) {
      return(build_unavailable_power_scaling_result(fit, powerscale, analysis_plan))
    }
    sensitivity <- ap6_run_priorsense(
      fit,
      blocks = powerscale$blocks,
      components = powerscale$component,
      lower_alpha = powerscale$lower_alpha,              # .99
      upper_alpha = powerscale$upper_alpha,              # 1.01
      divergence_measure = powerscale$div_measure,       # CJS
      sensitivity_threshold = powerscale$sensitivity_threshold  # .05
    )
    ap6_summarise_powerscale_diagnostics(
      sensitivity,
      diagnostic_threshold = powerscale$sensitivity_threshold,  # .05
      fit = fit
    )
  })
}

#' AP6 — conditionally refit a primary model with a Student-t likelihood
#'
#' The one-sided heavy-tail trigger decides whether each descriptive refit
#' exists.
fit_student_t_if_needed <- function(data, model_set, primary_fits, predictive_checks, analysis_plan) {
  sampling <- analysis_plan$regression
  student <- analysis_plan$sensitivity$student_t
  triggers <- vapply(names(primary_fits), function(outcome) {
    ap6_student_tail_trigger(predictive_checks$posterior[[outcome]])
  }, logical(1))
  fits <- names(primary_fits) |>
    lapply(function(outcome) {
    trigger <- triggers[[outcome]]
    if (is.na(trigger)) {
      fit <- primary_fits[[outcome]]
      if (inherits(fit, "error")) return(fit)
      if (is.null(fit)) return(simpleError(describe_unavailable_regression_fit(fit)))
      stop("A Student refit has no predictive trigger for an available primary fit: ", outcome, ".")
    }
    if (!trigger) return(NULL)
    formula <- model_set$formulas[[outcome]]
    data <- select_regression_data_for_model(data, all.vars(formula))
    require_valid_imputation_inputs(data, all.vars(formula))
    student_fit <- brms::brm(
      formula = formula,
      data = data,
      family = brms::brmsfamily("student"),
      prior = c(
        brms::set_prior(paste0("normal(0, ", model_set$primary_slope_sd, ")"), class = "b", tag = "metric_slopes"),
        ap6_build_gender_contrast_priors(formula, data, model_set$gender_prior_sd),
        brms::set_prior(paste0("normal(0, ", model_set$intercept_prior_sd, ")"), class = "Intercept", tag = "intercept"),
        brms::set_prior(paste0("normal(0, ", student$sigma_scale_prior_sd, ")"), class = "sigma", tag = "sigma"),  # 1/sqrt(2)
        brms::set_prior(paste0("constant(", student$nu_fixed, ")"), class = "nu")  # nu = 4
      ),
      backend = analysis_plan$regression$backend,
      chains = sampling$chains,
      cores = sampling$cores,
      warmup = sampling$warmup,
      iter = sampling$warmup + sampling$iter_per_chain,
      thin = 1,
      seed = sampling$seed,
      refresh = 0
    )
    list(fit = student_fit)
  })
  names(fits) <- names(primary_fits)
  # Which outcome the
  # one-sided trigger fired for is decided exactly once, here. An untriggered
  # refit stays `NULL` and is therefore distinguishable from a failed one.
  attr(fits, "heavy_tail_triggers") <- triggers
  fits
}

# BEGIN GENERATED PARAMETER CARD: AP6 INTERVAL LEVEL COEFFICIENTS
# Automatically generated from analysis_plan.yaml
# Regression coefficients and residual scale
#   Interval level: 0.95   (regression.ci_level)
#   Central, equal-tailed: quantiles 0.025 and 0.975
# END GENERATED PARAMETER CARD: AP6 INTERVAL LEVEL COEFFICIENTS
#' Summarise coefficients and residual scale from every separate regression fit.
#'
#' Central intervals determine predictions; HDIs are descriptive. Keep all
#' evidence with its final fit validity and simulation precision.
extract_regression_coefficient_summaries <- function(
    primary_fits, comparison_fits, student_fits, analysis_plan) {
  fit_results <- extract_regression_result_records(
    primary_fits, comparison_fits, student_fits, analysis_plan
  )
  probabilities <- c((1 - analysis_plan$regression$ci_level) / 2,
                     1 - (1 - analysis_plan$regression$ci_level) / 2)
  fit_results |>
    lapply(function(one_fit_result) {
      if (!one_fit_result$fit_available) {
        return(ap6_unavailable_regression_summary(one_fit_result))
      }
      coefficient_draws <- ap6_select_regression_coefficient_draws(
        one_fit_result$fit
      )
      coefficients <- suppressMessages(posterior::summarise_draws(
        coefficient_draws,
        estimate = stats::median,
        sd = stats::sd,
        ~posterior::quantile2(.x, probs = probabilities),
        p_positive = ~mean(.x > 0),
        p_negative = ~mean(.x < 0)
      ))
      precision <- ap6_coefficient_simulation_precision(
        coefficient_draws, probabilities
      )
      highest_density_intervals <- ap6_highest_density_coefficient_intervals(
        coefficient_draws, interval_level = analysis_plan$regression$ci_level
      )
      coefficient_summary <- ap6_pack_regression_coefficient_summary(
        coefficients, precision, one_fit_result, analysis_plan
      )
      ap6_store_regression_coefficient_evidence(
        coefficient_summary, highest_density_intervals
      )
    }) |>
    ap6_bind_regression_coefficient_evidence()
}

# BEGIN GENERATED PARAMETER CARD: AP6 INTERVAL LEVEL R2
# Automatically generated from analysis_plan.yaml
# Explained variance (Bayesian R2)
#   Interval level: 0.95   (regression.ci_level)
#   Central, equal-tailed: quantiles 0.025 and 0.975
# END GENERATED PARAMETER CARD: AP6 INTERVAL LEVEL R2
#' Summarise explained variance with the same definition in Gaussian and
#' Student fits.
extract_regression_r2_summaries <- function(
    primary_fits, comparison_fits, student_fits, analysis_plan) {
  fit_results <- extract_regression_result_records(
    primary_fits, comparison_fits, student_fits, analysis_plan
  )
  probabilities <- c((1 - analysis_plan$regression$ci_level) / 2,
                     1 - (1 - analysis_plan$regression$ci_level) / 2)
  fit_results |>
    lapply(function(one_fit_result) {
      if (!one_fit_result$fit_available) {
        return(ap6_unavailable_regression_summary(one_fit_result))
      }
      residual_draws <- ap6_extract_regression_residual_draws(one_fit_result$fit)
      expected_outcomes <- brms::posterior_epred(
        one_fit_result$fit,
        draw_ids = residual_draws$draw_ids
      )
      predicted_variance <- apply(expected_outcomes, 1, stats::var)
      residual_variance <- residual_draws$sigma^2
      if (one_fit_result$family == "student") {
        nu <- one_fit_result$nu_fixed  # preregistered: 4
        residual_variance <- residual_variance * nu / (nu - 2)
      }
      r2_draws <- predicted_variance / (predicted_variance + residual_variance)
      r2_summary <- tibble::tibble(
        r2_median = stats::median(r2_draws),
        r2_lo = unname(stats::quantile(r2_draws, probabilities[1])),
        r2_hi = unname(stats::quantile(r2_draws, probabilities[2]))
      )
      ap6_attach_regression_result_context(r2_summary, one_fit_result)
    }) |>
    dplyr::bind_rows()
}

#' Compare the same coefficient and R2 summaries between Gaussian and Student
#' fits.
#'
#' Missing or invalid Student results remain labelled unavailable; no
#' robustness verdict is added.
extract_regression_likelihood_robustness <- function(coefficient_summaries, r2_summaries) {
  coefficients <- ap6_pair_gaussian_student_summaries(
    coefficient_summaries,
    gaussian_role = "primary",
    student_role = "student_refit",
    match_by = c("outcome", "term"),
    quantities = c("estimate", "q_lo", "q_hi", "p_positive", "p_negative")
  )
  r2 <- ap6_pair_gaussian_student_summaries(
    r2_summaries,
    gaussian_role = "primary",
    student_role = "student_refit",
    match_by = "outcome",
    quantities = c("r2_median", "r2_lo", "r2_hi")
  )
  list(coefficients = coefficients, r2 = r2)
}

# BEGIN GENERATED PARAMETER CARD: AP7 JOINT RESIDUAL PRIOR
# Automatically generated from analysis_plan.yaml
#   Residual-correlation prior: lkj(2)   (priors.rescor; brms class rescor)
# The four outcome equations are fitted with correlated residuals; the other
# priors of the joint fit are the ones on the AP6 PRIORS card.
# END GENERATED PARAMETER CARD: AP7 JOINT RESIDUAL PRIOR
#' AP7 / RQ2 — define the one correlated-residual joint regression
#'
#' AP7 / RQ2 — Model Specification: the four AP6 outcome regressions are
#' fitted jointly with their retained formulas and priors. AP3 supplies the
#' joint sample and its own standardisation constants. Residuals correlate
#' under LKJ(2), and the AP6 validity gate must pass before interpretation.
#'
#' @param data The AP3-prepared joint regression table: known-gender rows
#'   with every required value available, their saved z columns and constants.
#' @param regression_model_set The model set of
#'   [define_regression_model_set()], whose formulas are retained.
#' @param analysis_plan Configuration from [zm_config()].
#' @return `list(data, formula, priors, response_ids)`.
define_joint_regression_model <- function(data, regression_model_set, analysis_plan) {
  # AP3 supplies the joint sample and its fixed z constants for all four equations.
  formulas <- regression_model_set$formulas
  # brms names each response by its column without non-alphanumerics (asc_agg_z -> ascaggz).
  response_ids <- vapply(formulas, function(formula) gsub("[^[:alnum:]]", "", all.vars(formula)[[1L]]),
                         character(1))
  unavailable <- ap6_capture_imputation_unavailability(
    require_valid_imputation_inputs(data, unique(unlist(lapply(formulas, all.vars))))
  )
  if (inherits(unavailable, "imputation_unavailable")) {
    return(list(data = data, response_ids = response_ids, unavailable = unavailable))
  }
  # The four primary formulas, in their declared order, as the equations of one
  # model with correlated residuals.
  joint_formula <- Reduce(`+`, lapply(unname(formulas), function(formula) {
    brms::bf(formula, family = brms::brmsfamily("gaussian"))
  })) + brms::set_rescor(TRUE)
  # The metric predictors: every predictor column that carries a key (the
  # standardised motives and metric covariates), in the formula's order;
  # gender is a factor with its own contrast priors.
  predictors <- all.vars(formulas[[1L]])[-1L]
  metric_predictors <- predictors[predictors %in% names(analysis_plan$regression$term_keys)]
  priors <- c(
    ap6_build_joint_metric_priors(
      response_ids, metric_predictors, analysis_plan$priors$slope_sd_primary  # normal(0, .20)
    ),
    ap6_build_joint_gender_contrast_priors(joint_formula, data, response_ids, analysis_plan$priors$gender_sd),  # normal(0, .40)
    brms::set_prior(paste0("normal(0, ", analysis_plan$priors$intercept_sd, ")"), class = "Intercept", resp = response_ids[["authoritarian_aggression"]], tag = "intercept"),  # normal(0, .20)
    brms::set_prior(paste0("normal(0, ", analysis_plan$priors$intercept_sd, ")"), class = "Intercept", resp = response_ids[["authoritarian_submission"]], tag = "intercept"),
    brms::set_prior(paste0("normal(0, ", analysis_plan$priors$intercept_sd, ")"), class = "Intercept", resp = response_ids[["conventionalism"]], tag = "intercept"),
    brms::set_prior(paste0("normal(0, ", analysis_plan$priors$intercept_sd, ")"), class = "Intercept", resp = response_ids[["sdo_d_dominance"]], tag = "intercept"),
    brms::set_prior(paste0("normal(0, ", analysis_plan$priors$sigma_sd, ")"), class = "sigma", resp = response_ids[["authoritarian_aggression"]], tag = "sigma"),  # half-normal(0, 1)
    brms::set_prior(paste0("normal(0, ", analysis_plan$priors$sigma_sd, ")"), class = "sigma", resp = response_ids[["authoritarian_submission"]], tag = "sigma"),
    brms::set_prior(paste0("normal(0, ", analysis_plan$priors$sigma_sd, ")"), class = "sigma", resp = response_ids[["conventionalism"]], tag = "sigma"),
    brms::set_prior(paste0("normal(0, ", analysis_plan$priors$sigma_sd, ")"), class = "sigma", resp = response_ids[["sdo_d_dominance"]], tag = "sigma"),
    brms::set_prior(analysis_plan$priors$rescor, class = "rescor", tag = "rescor")  # LKJ(2)
  )
  list(
    data = data,
    formula = joint_formula,
    priors = priors,
    response_ids = response_ids
  )
}

#' AP7 / RQ2 — fit the four equations jointly
#'
#' AP7 / RQ2 — Model Specification: fit the retained four AP6
#' equations as one multivariate Gaussian model with correlated residuals. The
#' fit uses the AP6 sampling settings and must pass the existing AP6 validity
#' gate before interpretation.
#'
#' @param joint_model The joint model of [define_joint_regression_model()].
#' @param analysis_plan Configuration from [zm_config()].
#' @return A `brmsfit` of the four responses.
fit_joint_regression <- function(joint_model, analysis_plan) {
  if (inherits(joint_model$unavailable, "imputation_unavailable")) return(joint_model$unavailable)
  sampling <- analysis_plan$regression  # 4 chains; 2,000 warmup; 6,000 post-warmup; seed 20260905
  ap6_capture_joint_fit(brms::brm(
    formula = joint_model$formula,
    data = joint_model$data,
    prior = joint_model$priors,
    sample_prior = "no",
    backend = analysis_plan$regression$backend,
    chains = sampling$chains,
    cores = sampling$cores,
    warmup = sampling$warmup,
    iter = sampling$warmup + sampling$iter_per_chain,
    thin = 1,
    seed = sampling$seed,
    refresh = 0
  ))
}

# ---- AP6 — technical helpers of the regression architecture -----------------

#' Preserve the designated AP3 unavailability condition; other errors propagate
ap6_capture_imputation_unavailability <- function(expression) {
  tryCatch(force(expression), imputation_unavailable = identity)
}

#' Reason recorded when a planned fit is unavailable
describe_unavailable_regression_fit <- function(fit) {
  if (inherits(fit, "error")) conditionMessage(fit) else "No fit available."
}

#' One unavailable record per planned prior-predictive specification
build_unavailable_prior_prediction_record <- function(outcome, formula, specification, condition, analysis_plan) {
  probabilities <- analysis_plan$regression$prior_predictive$raw_scale_quantiles
  list(
    outcome = outcome, response_column = all.vars(formula)[[1L]],
    family = as.character(specification$family), slope_sd = as.numeric(specification$slope_sd),
    status = "unavailable", note = conditionMessage(condition),
    fixed_z_constants = list(mean = NA_real_, sd = NA_real_),
    standardised_summary = list(n_draws = 0L, n_observations = 0L, mean = NA_real_,
                                sd = NA_real_, p95_absolute = NA_real_),
    raw_scale_summary = list(raw_mean = NA_real_, raw_sd = NA_real_,
      raw_quantiles = stats::setNames(rep(NA_real_, length(probabilities)), paste0(100 * probabilities, "%")),
      response_range = c(min = analysis_plan$scales$response_min, max = analysis_plan$scales$response_max),
      share_below_min = NA_real_, share_above_max = NA_real_, share_outside_range = NA_real_)
  )
}

#' Keep six unavailable posterior checks and an empty, typed draw table
build_unavailable_posterior_prediction_checks <- function(fit, analysis_plan) {
  statistics <- c("mean", "sd", "skewness", "excess_kurtosis", "min", "max")
  checks <- stats::setNames(lapply(statistics, function(statistic) {
    list(statistic = statistic, observed = NA_real_, replicated_interval = c(NA_real_, NA_real_),
         replicated_median = NA_real_, interval_membership = NA_character_,
         posterior_predictive_p_value = NA_real_)
  }), statistics)
  attr(checks, "predictive_draws") <- tibble::tibble(
    outcome = character(), type = character(), draw = integer(), obs = integer(), value = double())
  attr(checks, "slope_sd") <- as.numeric(analysis_plan$priors$slope_sd_primary)
  attr(checks, "status") <- "unavailable"
  attr(checks, "note") <- describe_unavailable_regression_fit(fit)
  checks
}

#' Preserve planned power-scaling blocks when their original fit is unavailable
build_unavailable_power_scaling_result <- function(fit, powerscale, analysis_plan) {
  variables <- c("b_Intercept", paste0("b_", names(analysis_plan$regression$term_keys)), "bayes_R2")
  blocks <- as.character(unlist(powerscale$blocks))
  note <- describe_unavailable_regression_fit(fit)
  sensitivity <- list(
    settings = list(components = powerscale$component, lower_alpha = powerscale$lower_alpha,
                    upper_alpha = powerscale$upper_alpha, div_measure = powerscale$div_measure,
                    sensitivity_threshold = powerscale$sensitivity_threshold),
    priorsense_version = NA_character_, note = note,
    blocks = stats::setNames(lapply(blocks, function(block) {
      list(block = block, prior_selection = if (block == "all_priors") "all" else block,
           matrix = tibble::tibble(variable = variables, prior = NA_real_, likelihood = NA_real_,
                                  diagnosis = "unavailable", note = note),
           sensitivity = NA_real_, pareto_k_max = NA_real_, importance_sampling_draws = NA_real_)
    }), blocks)
  )
  ap6_summarise_powerscale_diagnostics(sensitivity, powerscale$sensitivity_threshold,
    fit = fit)
}

#' Preserve a failed joint-model fit as unavailable
#'
#' This boundary catches only the sampler call supplied by
#' [fit_joint_regression()]. The shared validity gate records the original
#' condition as an unavailable fit; errors in later gate and result code still
#' stop normally.
ap6_capture_joint_fit <- function(expression) {
  tryCatch(force(expression), error = identity)
}

#' Combine the primary and comparison fits in configured width order
#'
#' Decimal formatting only supplies stable list keys; no fit or scientific
#' value changes.
ap6_arrange_prior_width_fits <- function(primary_fits, comparison_fits, widths, primary_width) {
  keys <- function(values) sprintf("%.2f", values)
  names(comparison_fits) <- keys(as.numeric(names(comparison_fits)))
  fits <- c(comparison_fits, stats::setNames(list(primary_fits), keys(primary_width)))
  fits[keys(widths)]
}

#' Priors of the observed non-reference gender contrasts
#'
#' Given a declared formula, the known-gender data and the fixed SD, it
#' discovers the treatment contrasts and assigns their priors. It does not
#' choose gender, its reference group, or the prior width.
ap6_build_gender_contrast_priors <- function(formula, data, sd) {
  design <- stats::model.matrix(formula, data = data)
  contrasts <- grep("^gender", colnames(design), value = TRUE)
  rows <- lapply(contrasts, function(contrast) {
    brms::set_prior(paste0("normal(0, ", sd, ")"), class = "b", coef = contrast,
      tag = "gender_contrasts")
  })
  # `c.brmsprior()` refuses a bare list ("Cannot add 'list' objects to the
  # prior"), so the rows are combined into one `brmsprior`.
  do.call(c, c(list(brms::empty_prior()), rows))
}

#' One fit per declared formula, in the declared outcome order
#'
#' The caller supplies the literal `brms::brm()` call and has already fixed the
#' scientific model.
ap6_iterate_regression_fits <- function(formulas, fit_one) {
  fits <- lapply(names(formulas), function(outcome) {
    tryCatch(fit_one(formulas[[outcome]]), imputation_unavailable = identity)
  })
  names(fits) <- names(formulas)
  fits
}

#' Apply one function to every native fit of a collection, keeping its shape
#'
#' A collection is flat (one fit per outcome), nested by prior width, or holds
#' Student entries `list(fit)`; an untriggered refit is `NULL`. The map carries
#' each fit's outcome name and prior width down to it. Attributes of the
#' collection and of every nested level survive, and a `NULL` entry stays an
#' entry rather than disappearing from its list.
#'
#' @param fits A regression fit collection.
#' @param apply_one `function(fit, outcome, slope_sd)`.
#' @param outcome,slope_sd Identity carried down from the enclosing level.
#' @return The collection with each fit replaced by `apply_one()`'s value.
ap6_map_regression_fits <- function(fits, apply_one, outcome = NA_character_,
                                    slope_sd = NA_real_) {
  if (is.null(fits) || inherits(fits, "brmsfit") || inherits(fits, "error")) {
    return(apply_one(fits, outcome, slope_sd))
  }
  if (!is.list(fits)) {
    stop("A regression fit collection carries an entry that is neither a fit nor a collection.")
  }
  if ("fit" %in% names(fits)) {
    fits["fit"] <- list(apply_one(fits$fit, outcome, slope_sd))
    return(fits)
  }
  keys <- names(fits)
  if (is.null(keys) || any(!nzchar(keys))) stop("A regression fit collection has lost its labels.")
  out <- fits
  for (key in keys) {
    width <- suppressWarnings(as.numeric(key))
    out[key] <- list(ap6_map_regression_fits(
      fits[[key]], apply_one,
      outcome = if (is.na(width)) key else outcome,
      slope_sd = if (is.na(width)) slope_sd else width
    ))
  }
  out
}

#' Execute the ordered validity-gate remedies on one fit
#'
#' Runs the two stages at the top of this file: [ap6_refit_if_needed()] doubles the
#' post-warmup draws while the ESS target is missed and then refits once on an
#' R-hat, divergence or tree-depth failure, and [ap6_check_final_convergence()]
#' records the final diagnostics, the gate status, the retry history and the
#' fit-time provenance on the fit itself.
#'
#' @param fit A `brmsfit` returned by one of the fitting verbs.
#' @param outcome Readable outcome name of the fit.
#' @param slope_sd Slope prior SD of the fit (`NA` outside the width sweep).
#' @param max_doublings Most doublings of the post-warmup draws on an ESS
#'   shortfall (`regression$validity_gate$max_ess_doublings`).
#' @param analysis_plan Configuration from [zm_config()].
#' @return The fit with the AP6 result attributes and its provenance.
ap6_gate_one_regression_fit <- function(fit, outcome, slope_sd, max_doublings, analysis_plan) {
  regression <- analysis_plan$regression
  family_name <- ap6_focal_family(fit)
  nu_fixed <- if (identical(family_name, "student")) {
    as.numeric(analysis_plan$sensitivity$student_t$nu_fixed)
  } else {
    NA_real_
  }
  warmup <- as.integer(regression$warmup)
  draws_per_chain <- as.integer(regression$iter_per_chain)
  ess <- ap6_ess_check(fit, analysis_plan)
  model <- list(
    outcome = outcome,
    slope_sd = if (is.na(slope_sd)) analysis_plan$priors$slope_sd_primary else slope_sd,
    family_name = family_name,
    sample_prior = "no",
    data = fit$data,
    formula = stats::formula(fit),
    fit = fit,
    regression = regression,
    warmup = warmup,
    draws_per_chain = draws_per_chain,
    control = NULL,
    priors = structure(list(), nu_fixed = nu_fixed),
    ess = ess,
    ess_log = tibble::tibble(
      attempt = 1L, iter_per_chain = draws_per_chain,
      min_ess_bulk = ess$min_ess_bulk, min_ess_tail = ess$min_ess_tail, ok = ess$ok
    ),
    retry_log = tibble::tibble(
      step = integer(0), reason = character(0), warmup = integer(0), iter_per_chain = integer(0),
      adapt_delta = numeric(0), max_treedepth = integer(0), ok = logical(0)
    ),
    max_doublings = as.integer(max_doublings)
  )
  gated <- ap6_check_final_convergence(ap6_refit_if_needed(model, analysis_plan), analysis_plan)
  # The fitted N the report's diagnostics and coefficient tables state, taken
  # from the fit rather than recomputed by a report target.
  attr(gated, "n_obs") <- as.integer(stats::nobs(gated))
  gated
}

#' Observed and replicated values of the preregistered predictive statistics
#'
#' It calculates each requested observed statistic and its value in every
#' replicated draw, then returns the central replicated interval. It does not
#' choose the preregistered statistics or the Student-t action.
ap6_posterior_predictive_statistics <- function(observed, replicated, statistics, interval) {
  statistic <- list(
    mean = mean,
    sd = stats::sd,
    # finite-sample estimator type 3 (psych default)
    skewness = function(x) psych::skew(x, type = 3),
    excess_kurtosis = function(x) psych::kurtosi(x, type = 3),
    min = min,
    max = max
  )
  results <- lapply(statistics, function(name) {
    values <- apply(replicated, 1, statistic[[name]])
    list(
      statistic = name,
      observed = statistic[[name]](observed),
      replicated_interval = stats::quantile(values, interval),
      # The report's predictive-check table shows the replicated median beside
      # the interval.
      replicated_median = unname(stats::median(values)),
      p_value = mean(values >= statistic[[name]](observed))
    )
  })
  names(results) <- statistics
  results
}

#' The one saved standardisation row of a standardised outcome column
#'
#' It selects and verifies that outcome's saved mean/SD row. It does not
#' estimate, alter, or reapply standardisation.
ap6_select_outcome_standardisation <- function(response_column, z_parameters) {
  outcome_standardisation <- z_parameters[
    z_parameters$z_col == response_column,
    c("var", "z_col", "mean", "sd"),
    drop = FALSE
  ]
  if (nrow(outcome_standardisation) != 1L) {
    stop("Exactly one fixed standardisation row is required per outcome.")
  }
  outcome_standardisation
}

#' Interval exclusion and median direction across the fitted prior widths
#'
#' It extracts coefficient medians and central intervals from the three already
#' fitted width sets, then records the two declared stability indicators. It
#' does not choose a width or change a primary classification.
ap6_summarise_coefficient_stability <- function(fits, widths, interval_level, indicators,
                                                declared_coefficients = character()) {
  interval_probabilities <- c((1 - interval_level) / 2, 1 - (1 - interval_level) / 2)
  lapply(names(fits[[1]]), function(outcome) {
    outcome_fits <- lapply(fits, `[[`, outcome)
    available <- vapply(outcome_fits, function(fit) !is.null(fit) && !inherits(fit, "error"), logical(1))
    draws_by_width <- lapply(seq_along(outcome_fits), function(i) {
      if (available[[i]]) posterior::as_draws_df(outcome_fits[[i]]) else NULL
    })
    coefficients <- unique(unlist(lapply(draws_by_width, function(draws) {
      grep("^b_", names(draws), value = TRUE)
    }), use.names = FALSE))
    if (!length(coefficients)) coefficients <- declared_coefficients
    lapply(coefficients, function(coefficient) {
      summaries <- lapply(seq_along(draws_by_width), function(i) {
        draws <- draws_by_width[[i]]
        if (!available[[i]]) {
          return(list(median = NA_real_, interval = c(NA_real_, NA_real_),
                      interval_excludes_zero = NA, status = "unavailable",
                      note = describe_unavailable_regression_fit(outcome_fits[[i]])))
        }
        values <- draws[[coefficient]]
        if (is.null(values)) stop("A prior-width fit is missing coefficient ", coefficient, ".")
        interval <- stats::quantile(values, interval_probabilities)
        list(
          median = stats::median(values),
          interval = interval,
          interval_excludes_zero = interval[[1]] > 0 || interval[[2]] < 0,
          status = "available", note = NA_character_
        )
      })
      list(
        outcome = outcome,
        coefficient = coefficient,
        by_width = stats::setNames(summaries, widths),
        interval_exclusion_stable = if (!all(available)) NA else length(unique(vapply(
          summaries, `[[`, logical(1), "interval_excludes_zero"
        ))) == 1L,
        median_direction_stable = if (!all(available)) NA else length(unique(vapply(
          summaries, function(summary) sign(summary$median), numeric(1)
        ))) == 1L,
        indicators = indicators
      )
    })
  })
}

#' Power-scaling sensitivity of one fit, per registered prior block
#'
#' It passes the configured blocks, axes, alpha values and CJS threshold to
#' priorsense and preserves the per term and block matrices and the
#' importance-sampling diagnostics that the prior-sensitivity supplement reads
#' ([tabulate_power_scaling()]).
ap6_run_priorsense <- function(fit, blocks, components, lower_alpha,
                                upper_alpha, divergence_measure, sensitivity_threshold) {
  coefficient_variables <- ap6_focal_b_vars(fit)
  n_draws <- posterior::ndraws(posterior::as_draws_array(fit))
  block_names <- as.character(unlist(blocks))
  if (length(block_names) == 0L) stop("analysis_plan$sensitivity$powerscale$blocks is empty.")
  results <- lapply(block_names, function(block) {
    prior_selection <- if (block == "all_priors") NULL else block
    coefficients <- priorsense::powerscale_sensitivity(
      x = fit,
      variable = coefficient_variables,
      component = components,
      lower_alpha = lower_alpha,
      upper_alpha = upper_alpha,
      div_measure = divergence_measure,
      sensitivity_threshold = sensitivity_threshold,
      prior_selection = prior_selection
    )
    bayesian_r2 <- priorsense::powerscale_sensitivity(
      x = fit,
      variable = "bayes_R2",
      component = components,
      lower_alpha = lower_alpha,
      upper_alpha = upper_alpha,
      div_measure = divergence_measure,
      sensitivity_threshold = sensitivity_threshold,
      prior_selection = prior_selection,
      prediction = ap6_r2_prediction
    )
    matrix <- dplyr::bind_rows(
      tibble::as_tibble(as.data.frame(coefficients)),
      tibble::as_tibble(as.data.frame(bayesian_r2))
    )
    pareto_k_max <- ap6_pareto_k_max(
      fit, prior_selection, lower_alpha, upper_alpha, coefficient_variables[[1L]]
    )
    list(
      block = block,
      prior_selection = if (is.null(prior_selection)) "all" else prior_selection,
      coefficients = coefficients,
      bayesian_r2 = bayesian_r2,
      matrix = matrix,
      sensitivity = suppressWarnings(max(c(as.numeric(matrix$prior),
                                           as.numeric(matrix$likelihood)), na.rm = TRUE)),
      pareto_k_max = pareto_k_max,
      importance_sampling_draws = n_draws,
      pareto_k_threshold = calculate_importance_sampling_threshold(n_draws),
      importance_sampling_status = classify_importance_sampling(pareto_k_max, n_draws)
    )
  })
  names(results) <- block_names
  list(
    variables = coefficient_variables,
    settings = list(
      components = components, lower_alpha = lower_alpha, upper_alpha = upper_alpha,
      div_measure = divergence_measure, sensitivity_threshold = sensitivity_threshold
    ),
    priorsense_version = as.character(utils::packageVersion("priorsense")),
    blocks = results
  )
}

#' Importance-sampling status and interpretability of one fit's power scaling
#'
#' It records whether the original fit passed its gate and whether the
#' importance-sampling calculation of each block is reliable; a block is
#' interpretable only when both hold. The full per term and block matrices stay
#' attached, because the prior-sensitivity supplement extracts them
#' ([tabulate_power_scaling()]).
ap6_summarise_powerscale_diagnostics <- function(sensitivity, diagnostic_threshold, fit = NULL) {
  fit_gate_status <- attr(fit, "gate_status", exact = TRUE)
  diagnostics <- attr(fit, "diagnostics", exact = TRUE)
  fit_valid <- if (is.null(fit_gate_status) || is.null(diagnostics$ok)) NA else
    isTRUE(diagnostics$ok) && fit_gate_status %in% c("ok", "retried_ok")
  if (is.null(fit_gate_status)) fit_gate_status <- "unavailable"
  list(
    diagnostic_threshold = diagnostic_threshold,
    fit_gate_status = fit_gate_status,
    fit_valid = fit_valid,
    blocks = lapply(sensitivity$blocks, function(block) {
      importance_status <- classify_importance_sampling(block$pareto_k_max, block$importance_sampling_draws)
      interpretable <- can_interpret_power_scaling(fit_valid, fit_gate_status, importance_status)
      list(
        importance_sampling_status = importance_status,
        interpretable = interpretable
      )
    }),
    sensitivity = sensitivity
  )
}

#' One record per specified regression model or variant
#'
#' Adapts the gated primary, nested prior-width and optional Student fit
#' collections without refitting. Each record carries the stable outcome
#' identity, the model role, the native fit, the final diagnostics, the gate
#' status and the retry history. Missing metadata stays unavailable; it never
#' implies success. Student results retain the primary outcome order even when
#' a refit was not triggered.
#'
#' @param primary_fits,comparison_fits,student_fits Gated fit collections.
#' @param analysis_plan Configuration from [zm_config()].
#' @return List of records.
extract_regression_result_records <- function(primary_fits, comparison_fits, student_fits, analysis_plan) {
  identity <- ap6_regression_outcome_identity(analysis_plan)
  triggers <- attr(student_fits, "heavy_tail_triggers", exact = TRUE)
  collect <- function(fits, role, slope_sd, family) {
    outcomes <- names(fits)
    if (is.null(outcomes)) outcomes <- names(primary_fits)
    if (is.null(outcomes)) outcomes <- identity$outcome
    if (length(fits) == 0L) fits <- stats::setNames(rep(list(NULL), length(outcomes)), outcomes)
    if (length(fits) != length(outcomes)) stop("Fit collection has lost its outcome labels.")
    lapply(seq_along(fits), function(i) {
      row <- identity[match(outcomes[[i]], identity$outcome), , drop = FALSE]
      if (nrow(row) != 1L || is.na(row$outcome_key)) {
        stop("A regression fit collection names an outcome the model set does not declare: ",
             outcomes[[i]], ".")
      }
      result <- fits[[i]]
      fit <- if (is.list(result) && !inherits(result, "brmsfit") &&
                 "fit" %in% names(result)) result$fit else result
      fit_available <- !is.null(fit) && !inherits(fit, "error")
      diagnostics <- attr(fit, "diagnostics", exact = TRUE)
      gate_status <- attr(fit, "gate_status", exact = TRUE)
      gate_available <- !is.null(gate_status) && !is.null(diagnostics$ok)
      fit_valid <- if (gate_available) {
        isTRUE(diagnostics$ok) && gate_status %in% ap7_gate_statuses_ok()
      } else NA
      triggered <- if (is.null(triggers) || !outcomes[[i]] %in% names(triggers)) {
        NA
      } else {
        triggers[[outcomes[[i]]]]
      }
      note <- if (!fit_available) {
        if (inherits(fit, "error")) {
          conditionMessage(fit)
        } else if (identical(role, "student_refit") && isFALSE(triggered)) {
          "not triggered"
        } else {
          "No fit available; Student refit may not have been triggered."
        }
      } else if (!gate_available) {
        "Final validity-gate result unavailable."
      } else if (!fit_valid) "Fit is not interpretable." else NA_character_
      list(
        fit = fit, fit_available = fit_available, outcome = outcomes[[i]],
        outcome_key = row$outcome_key, response_column = row$response_column,
        role = role, slope_sd = slope_sd, family = family,
        nu_fixed = if (family == "student") as.numeric(analysis_plan$sensitivity$student_t$nu_fixed) else NA_real_,
        label = ap6_regression_fit_label(role, slope_sd, analysis_plan),
        fit_valid = fit_valid,
        gate_status = if (gate_available) gate_status else "unavailable",
        diagnostics = diagnostics,
        retry_history = attr(fit, "retry_log", exact = TRUE),
        ess_history = attr(fit, "ess_log", exact = TRUE),
        iter_used = attr(fit, "iter_used", exact = TRUE),
        warmup_used = attr(fit, "warmup_used", exact = TRUE),
        n_obs = attr(fit, "n_obs", exact = TRUE),
        provenance = attr(fit, "provenance", exact = TRUE),
        heavy_tail_trigger = if (identical(role, "student_refit")) triggered else NA,
        note = note
      )
    })
  }
  comparison_records <- lapply(names(comparison_fits), function(width) {
    collect(comparison_fits[[width]], "sweep", as.numeric(width), "gaussian")
  })
  c(
    collect(primary_fits, "primary", analysis_plan$priors$slope_sd_primary, "gaussian"),
    unlist(comparison_records, recursive = FALSE),
    collect(student_fits, "student_refit", analysis_plan$priors$slope_sd_primary, "student")
  )
}

#' Population-level coefficients and residual scale of one fit, with chain identity
#'
#' Fixed Student degrees of freedom are recorded as metadata, not as an
#' estimated coefficient.
ap6_select_regression_coefficient_draws <- function(fit) {
  draws <- posterior::as_draws_df(fit)
  variables <- posterior::variables(draws)
  parameters <- c(grep("^b_", variables, value = TRUE), "sigma")
  posterior::subset_draws(draws, variable = parameters)
}

#' Monte Carlo precision of the reported coefficient summaries
#'
#' These are simulation precision measures, separate from posterior
#' uncertainty.
ap6_coefficient_simulation_precision <- function(draws, probabilities) {
  suppressMessages(posterior::summarise_draws(
    draws,
    ess_bulk = posterior::ess_bulk,
    ess_tail = posterior::ess_tail,
    rhat = posterior::rhat,
    mcse_median = posterior::mcse_median,
    mcse_q_lo = ~unname(posterior::mcse_quantile(.x, probs = probabilities[1])),
    mcse_q_hi = ~unname(posterior::mcse_quantile(.x, probs = probabilities[2]))
  ))
}

#' Label the selected parameters and assemble one fit's coefficient summary
#'
#' It combines already calculated summaries and precision with provenance and
#' final validity; it does not classify evidence.
ap6_pack_regression_coefficient_summary <- function(coefficients, precision, one_fit_result, analysis_plan) {
  quantiles <- grep("^q[0-9.]+$", names(coefficients), value = TRUE)
  quantiles <- quantiles[order(as.numeric(sub("^q", "", quantiles)))]
  names(coefficients)[match(quantiles, names(coefficients))] <- c("q_lo", "q_hi")
  coefficients <- dplyr::left_join(coefficients, precision, by = "variable")
  raw_terms <- sub("^b_", "", coefficients$variable)
  governed_keys <- unname(analysis_plan$regression$term_keys[raw_terms])
  coefficients$term <- ifelse(is.na(governed_keys), raw_terms, governed_keys)
  coefficients$term_type <- dplyr::case_when(
    coefficients$term == "Intercept" ~ "intercept",
    coefficients$term == "sigma" ~ "sigma",
    coefficients$term %in% analysis_plan$regression$motives ~ "predictor",
    TRUE ~ "covariate"
  )
  ap6_attach_regression_result_context(coefficients, one_fit_result)
}

#' Attach the fit identity and final gate state to every extracted row
#'
#' The summary values remain available for diagnosing a failed fit, but its
#' scientific verdict is unavailable. The readable outcome name, the stable
#' prediction key and the response column travel together on every row.
ap6_attach_regression_result_context <- function(summary, one_fit_result) {
  context <- tibble::tibble(
    outcome = one_fit_result$outcome,
    outcome_key = one_fit_result$outcome_key,
    response_column = one_fit_result$response_column,
    role = one_fit_result$role,
    slope_sd = one_fit_result$slope_sd,
    label = one_fit_result$label,
    family = one_fit_result$family,
    nu_fixed = one_fit_result$nu_fixed,
    fit_available = one_fit_result$fit_available,
    fit_valid = one_fit_result$fit_valid,
    gate_status = one_fit_result$gate_status,
    note = one_fit_result$note
  )
  dplyr::bind_cols(context[rep(1L, nrow(summary)), ], summary)
}

#' The provenance and status row of a missing or failed planned fit
#'
#' It supplies no coefficient or R2 value and cannot create a scientific
#' verdict.
ap6_unavailable_regression_summary <- function(one_fit_result) {
  ap6_attach_regression_result_context(
    tibble::tibble(unavailable = TRUE, term = NA_character_, term_type = NA_character_,
                   variable = NA_character_, estimate = NA_real_, sd = NA_real_,
                   q_lo = NA_real_, q_hi = NA_real_, p_positive = NA_real_, p_negative = NA_real_,
                   r2_median = NA_real_, r2_lo = NA_real_, r2_hi = NA_real_), one_fit_result
  )
}

#' Residual-scale draws paired with the draw IDs given to posterior_epred()
ap6_extract_regression_residual_draws <- function(fit) {
  draws <- posterior::as_draws_df(fit)
  list(draw_ids = draws$.draw, sigma = draws$sigma)
}

#' Store one fit's coefficient evidence beside its highest-density intervals
#'
#' The highest-density intervals join the labelled summary by variable.
#' Provenance, validity and unavailable notes are preserved and no extra
#' decision is made.
#'
#' @param coefficient_summary Labelled summary from [ap6_pack_regression_coefficient_summary()].
#' @param highest_density_intervals From [ap6_highest_density_coefficient_intervals()].
#' @return The summary with `hdi_lo` and `hdi_hi`.
ap6_store_regression_coefficient_evidence <- function(coefficient_summary, highest_density_intervals) {
  dplyr::left_join(coefficient_summary, highest_density_intervals, by = "variable")
}

#' Bind the per-fit coefficient evidence into the one coefficient-summary result
#'
#' Unavailable-fit rows are kept.
#'
#' @param fit_evidence List of [ap6_store_regression_coefficient_evidence()]
#'   values and unavailable-fit rows.
#' @return Coefficient tibble.
ap6_bind_regression_coefficient_evidence <- function(fit_evidence) {
  dplyr::bind_rows(fit_evidence)
}

#' Match Gaussian primary and Student-refit summaries by the stated keys
#'
#' The requested quantities appear side by side with `_gaussian` and `_student`
#' labels, both final validity statuses and both notes. Every Gaussian outcome
#' remains represented when its Student refit was not triggered or failed. No
#' sweep or joint result is substituted, no unavailable note is erased, no
#' independent posterior draws are paired and no similarity is classified.
#'
#' @param summaries A coefficient or R2 summary table.
#' @param gaussian_role,student_role The two roles to pair.
#' @param match_by Key columns of the pairing.
#' @param quantities Columns reported side by side.
#' @return One row per Gaussian row of the summary.
ap6_pair_gaussian_student_summaries <- function(summaries, gaussian_role, student_role,
                                                match_by, quantities) {
  summaries <- tibble::as_tibble(summaries)
  absent <- setdiff(c(match_by, "role"), names(summaries))
  if (length(absent) > 0L) {
    stop("ap6_pair_gaussian_student_summaries(): the summary lacks the column(s) ",
         paste(absent, collapse = ", "), ".")
  }
  status_columns <- c("outcome_key", "response_column", "fit_available", "fit_valid",
                      "gate_status", "note")
  rows_of <- function(role) {
    rows <- summaries[!is.na(summaries$role) & summaries$role == role, , drop = FALSE]
    keep <- unique(c(match_by, intersect(c(quantities, status_columns), names(rows))))
    rows <- rows[, keep, drop = FALSE]
    if (anyDuplicated(rows[, match_by, drop = FALSE]) > 0L) {
      stop("ap6_pair_gaussian_student_summaries(): the ", role,
           " rows are not unique on the matching keys.")
    }
    rows
  }
  dplyr::left_join(rows_of(gaussian_role), rows_of(student_role), by = match_by,
                   suffix = c("_gaussian", "_student"))
}

#' The one-sided heavy-tail trigger of the descriptive Student-t refit
#'
#' It evaluates the three already declared one-sided comparisons and returns
#' true when any one fires. It does not inspect skewness, choose a likelihood,
#' or change the Gaussian primary result.
ap6_student_tail_trigger <- function(checks) {
  if (identical(attr(checks, "status", exact = TRUE), "unavailable")) return(NA)
  kurtosis_flag <- checks$excess_kurtosis$observed > checks$excess_kurtosis$replicated_interval[[2]]
  minimum_flag <- checks$min$observed < checks$min$replicated_interval[[1]]
  maximum_flag <- checks$max$observed > checks$max$replicated_interval[[2]]
  isTRUE(kurtosis_flag || minimum_flag || maximum_flag)
}

#' The primary metric slope prior, repeated across the joint responses
#'
#' Serves [define_joint_regression_model()]. It repeats the primary
#' normal(0, .20) prior only across the seven declared metric predictors and
#' four responses. It never touches treatment-coded gender contrasts.
#'
#' @param response_ids The four brms response names of the joint formula.
#' @param metric_predictors The seven declared metric predictor columns.
#' @param sd Slope prior SD.
#' @return A `brmsprior`.
ap6_build_joint_metric_priors <- function(response_ids, metric_predictors, sd) {
  do.call(c, unname(unlist(lapply(response_ids, function(response) {
    lapply(metric_predictors, function(predictor) {
      brms::set_prior(paste0("normal(0, ", sd, ")"), class = "b", coef = predictor,
                      resp = response, tag = "metric_slopes")
    })
  }), recursive = FALSE)))
}

#' Priors of the observed gender contrasts, repeated across the joint responses
#'
#' Serves [define_joint_regression_model()]. It discovers observed
#' non-reference gender contrasts and repeats the fixed normal(0, .40) prior
#' for each joint response. It does not choose the gender factor, reference
#' group, or prior width.
#'
#' @param joint_formula The joint `mvbrmsformula`.
#' @param data The model sample with the `gender` factor.
#' @param response_ids The four brms response names of the joint formula.
#' @param sd Gender prior SD.
#' @return A `brmsprior`.
ap6_build_joint_gender_contrast_priors <- function(joint_formula, data, response_ids, sd) {
  design <- stats::model.matrix(joint_formula$forms[[1]]$formula, data = data)
  contrasts <- grep("^gender", colnames(design), value = TRUE)
  do.call(c, unname(unlist(lapply(response_ids, function(response) {
    lapply(contrasts, function(contrast) {
      brms::set_prior(paste0("normal(0, ", sd, ")"), class = "b", coef = contrast,
                      resp = response, tag = "gender_contrasts")
    })
  }), recursive = FALSE)))
}
