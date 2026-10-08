# The retry state machine of the validity gate (apply_validity_gate()):
# ap6_gate_one_regression_fit() measures ESS on the fitted model, then runs the
# retry stage (ap6_refit_if_needed()) and the final check
# (ap6_check_final_convergence()). No sampler runs: the refits and the
# diagnostics are stand-ins.

local({
  root <- normalizePath(getwd())
  while (!file.exists(file.path(root, "config", "analysis_plan.yaml"))) {
    parent <- dirname(root)
    if (identical(parent, root)) stop("project root not found from ", getwd())
    root <- parent
  }
  current <- new.env(parent = globalenv())
  sys.source(file.path(root, "R", "config.R"), current)
  sys.source(file.path(root, "R", "ap6_regressions.R"), current)
  assign("ap6_stage_test_env", current, envir = globalenv())
})

stage_cfg <- ap6_stage_test_env$zm_config(profile = "smoke")
stage_book <- ap6_stage_test_env$zm_codebook(stage_cfg)
stage_formula <- stats::as.formula(paste("sdo_dom_z ~", stage_cfg$regression$predictors))
environment(stage_formula) <- globalenv()

stage_data <- function() {
  set.seed(41)
  tibble::tibble(
    zm_security_z = rnorm(10), zm_achievement_z = rnorm(10), zm_power_z = rnorm(10),
    zm_prestige_z = rnorm(10), zm_arousal_z = rnorm(10), age_z = rnorm(10),
    gender = factor(rep(c("male", "female"), 5), levels = c("male", "female")),
    income_z = rnorm(10), sdo_dom_z = rnorm(10)
  )
}

stage_diag <- function(ok = TRUE, sampler_ok = TRUE, bfmi_ok = TRUE) {
  tibble::tibble(
    ok = ok, rhat_ok = sampler_ok, divergences_ok = sampler_ok,
    treedepth_ok = sampler_ok, bfmi_ok = bfmi_ok
  )
}

stage_ess <- function(ok) {
  list(min_ess_bulk = if (ok) 12000 else 25,
       min_ess_tail = if (ok) 11000 else 20, ok = ok)
}


# `ess` is the queue of ap6_ess_check() results: the gate's first measurement,
# then one per doubling. `diagnostics` is the queue of ap6_fit_diagnostics()
# results, one per call in the order the stages ask for them: the retry
# stage's evaluations, and last the final check's evaluation of the fit it
# returns (the same fit as the retry stage's last evaluation, hence the same
# result).
stage_scenarios <- list(
  gaussian = list(ess = TRUE, diagnostics = list(stage_diag(), stage_diag())),
  student = list(family = "student", ess = TRUE,
                 diagnostics = list(stage_diag(), stage_diag())),
  ess_success = list(ess = c(FALSE, TRUE), diagnostics = list(stage_diag(), stage_diag())),
  ess_cap = list(ess = c(FALSE, FALSE, FALSE), max_doublings = 2L,
                 diagnostics = list(stage_diag(FALSE), stage_diag(FALSE))),
  sampler_success = list(ess = TRUE,
                         diagnostics = list(stage_diag(FALSE, FALSE), stage_diag(), stage_diag())),
  sampler_failure = list(ess = TRUE,
                         diagnostics = list(stage_diag(FALSE, FALSE), stage_diag(FALSE, FALSE),
                                            stage_diag(FALSE, FALSE))),
  ess_then_sampler = list(ess = c(FALSE, TRUE),
                          diagnostics = list(stage_diag(FALSE, FALSE), stage_diag(), stage_diag())),
  bfmi_only = list(ess = TRUE, diagnostics = list(stage_diag(FALSE, TRUE, FALSE),
                                                  stage_diag(FALSE, TRUE, FALSE))),
  zero_doublings = list(ess = FALSE, max_doublings = 0L,
                        diagnostics = list(stage_diag(FALSE), stage_diag(FALSE)))
)

stage_run <- function(env, scenario) {
  calls <- list()
  next_id <- 0L
  ess_values <- lapply(scenario$ess, stage_ess)
  diagnostics <- scenario$diagnostics
  ess_i <- 0L
  diagnostics_i <- 0L
  family <- scenario$family %||% "gaussian"
  new_fit <- function(origin) {
    next_id <<- next_id + 1L
    structure(list(id = next_id, origin = origin, formula = stage_formula, data = stage_data()),
              class = "brmsfit", focal_family = family)
  }

  rlang::local_bindings(
    update.brmsfit = function(object, ...) {
      args <- list(...)
      calls[[length(calls) + 1L]] <<- c(list(kind = "update", object_id = object$id), args)
      new_fit("update")
    },
    nobs.brmsfit = function(object, ...) nrow(object$data),
    .env = .GlobalEnv
  )
  rlang::local_bindings(
    ap6_ess_check = function(...) {
      ess_i <<- ess_i + 1L
      if (ess_i > length(ess_values)) stop("unexpected ESS call")
      ess_values[[ess_i]]
    },
    ap6_fit_diagnostics = function(...) {
      diagnostics_i <<- diagnostics_i + 1L
      if (diagnostics_i > length(diagnostics)) stop("unexpected diagnostics call")
      diagnostics[[diagnostics_i]]
    },
    ap6_fit_provenance = function(...) tibble::tibble(source = "mock"),
    ap6_adapt_delta = function(...) 0.8,
    ap6_max_treedepth = function(...) 10L,
    ap6_focal_family = function(fit) attr(fit, "focal_family"),
    .env = env
  )

  fit <- env$ap6_gate_one_regression_fit(
    structure(list(id = 0L, origin = "fit", formula = stage_formula, data = stage_data()),
              class = "brmsfit", focal_family = family),
    "sdo_d_dominance", 0.2, scenario$max_doublings %||% 3L, stage_cfg
  )
  list(fit = fit, calls = calls, ess_calls = ess_i, diagnostics_calls = diagnostics_i)
}

stage_fit_attrs <- function(fit) {
  keep <- c(
    "outcome", "slope_sd", "family_name", "sample_prior", "iter_used",
    "warmup_used", "ess_log", "retry_log", "gate_status",
    "nu_fixed", "ci_level", "seed", "profile",
    "cfg_regression", "cfg_sensitivity", "formula_text", "diagnostics",
    "provenance"
  )
  stats::setNames(lapply(keep, attr, x = fit), keep)
}

test_that("the validity gate follows every specified retry path on a fitted model", {
  results <- lapply(names(stage_scenarios), function(name) {
    scenario <- stage_scenarios[[name]]
    current <- stage_run(ap6_stage_test_env, scenario)
    expect_identical(current$ess_calls, length(scenario$ess), info = name)
    expect_identical(current$diagnostics_calls, length(scenario$diagnostics), info = name)
    list(name = name, result = current)
  })

  by_name <- stats::setNames(lapply(results, `[[`, "result"), vapply(results, `[[`, "", "name"))
  # every call is a refit; the first fit comes from the fitting verb
  expect_equal(vapply(by_name, function(x) length(x$calls), integer(1)),
               c(gaussian = 0L, student = 0L, ess_success = 1L, ess_cap = 2L,
                 sampler_success = 1L, sampler_failure = 1L,
                 ess_then_sampler = 2L, bfmi_only = 0L, zero_doublings = 0L))
  expect_identical(attr(by_name$ess_then_sampler$fit, "iter_used"),
                   2L * as.integer(stage_cfg$regression$iter_per_chain))
  expect_equal(attr(by_name$ess_then_sampler$fit, "ess_log")$attempt, 1:2)
  expect_identical(attr(by_name$bfmi_only$fit, "gate_status"), "not_interpretable")
  expect_equal(nrow(attr(by_name$bfmi_only$fit, "retry_log")), 0L)
  # the first ESS measurement is recorded without a refit
  expect_equal(attr(by_name$zero_doublings$fit, "ess_log")$attempt, 1L)
  expect_false(attr(by_name$zero_doublings$fit, "ess_log")$ok)
  expect_equal(nrow(attr(by_name$zero_doublings$fit, "retry_log")), 0L)

  expected_status <- c(
    gaussian = "ok", student = "ok", ess_success = "retried_ok", ess_cap = "not_interpretable",
    sampler_success = "retried_ok", sampler_failure = "not_interpretable",
    ess_then_sampler = "retried_ok", bfmi_only = "not_interpretable",
    zero_doublings = "not_interpretable"
  )
  expect_identical(vapply(by_name, function(x) attr(x$fit, "gate_status"), ""), expected_status)
  expect_equal(attr(by_name$ess_success$fit, "retry_log")$step, 1L)
  expect_equal(attr(by_name$ess_cap$fit, "retry_log")$step, c(1L, 1L))
  expect_equal(attr(by_name$sampler_success$fit, "retry_log")$step, 2L)
  expect_equal(attr(by_name$ess_then_sampler$fit, "retry_log")$step, c(1L, 2L))

  for (name in names(by_name)) {
    fit <- by_name[[name]]$fit
    scenario <- stage_scenarios[[name]]
    expect_identical(attr(fit, "outcome"), "sdo_d_dominance", info = name)
    expect_identical(attr(fit, "slope_sd"), 0.2, info = name)
    expect_identical(attr(fit, "family_name"), scenario$family %||% "gaussian", info = name)
    expect_identical(attr(fit, "sample_prior"), "no", info = name)
    expect_identical(attr(fit, "n_obs"), 10L, info = name)
    expect_equal(attr(fit, "diagnostics"), tail(scenario$diagnostics, 1L)[[1L]], info = name)
    expect_equal(attr(fit, "provenance"), tibble::tibble(source = "mock"), info = name)
  }
  expect_true(is.na(attr(by_name$gaussian$fit, "nu_fixed")))
  expect_equal(attr(by_name$student$fit, "nu_fixed"), stage_cfg$sensitivity$student_t$nu_fixed)

  warmup <- as.integer(stage_cfg$regression$warmup)
  postwarmup <- as.integer(stage_cfg$regression$iter_per_chain)
  ess_retry <- by_name$ess_success$calls[[1L]]
  expect_identical(ess_retry$object_id, 0L)
  expect_identical(ess_retry$iter, warmup + 2L * postwarmup)
  expect_identical(ess_retry$warmup, warmup)
  expect_false(ess_retry$recompile)
  expect_identical(
    vapply(by_name$ess_cap$calls, `[[`, integer(1), "iter"),
    c(warmup + 2L * postwarmup, warmup + 4L * postwarmup)
  )
  # each doubling refits the previous refit
  expect_identical(vapply(by_name$ess_cap$calls, `[[`, integer(1), "object_id"), c(0L, 1L))
  expect_true(all(vapply(by_name$ess_cap$calls, function(x) !x$recompile, logical(1))))

  sampler_retry <- by_name$sampler_success$calls[[1L]]
  expect_identical(sampler_retry$iter, 2L * warmup + postwarmup)
  expect_identical(sampler_retry$warmup, 2L * warmup)
  expect_identical(sampler_retry$control$adapt_delta,
                   stage_cfg$regression$validity_gate$adapt_delta_retry)
  expect_identical(sampler_retry$control$max_treedepth,
                   as.integer(stage_cfg$regression$validity_gate$max_treedepth_retry))
  expect_false(sampler_retry$recompile)

  ess_then_sampler_retry <- by_name$ess_then_sampler$calls[[2L]]
  expect_identical(ess_then_sampler_retry$iter, 2L * warmup + 2L * postwarmup)
  expect_identical(ess_then_sampler_retry$warmup, 2L * warmup)
  expect_false(ess_then_sampler_retry$recompile)
})

test_that("the validity gate hands every fit the configured number of ESS doublings", {
  seen <- list()
  rlang::local_bindings(
    ap6_gate_one_regression_fit = function(fit, outcome, slope_sd, max_doublings, analysis_plan) {
      seen[[length(seen) + 1L]] <<- list(outcome = outcome, slope_sd = slope_sd,
                                         max_doublings = max_doublings)
      fit
    },
    .env = ap6_stage_test_env
  )
  fit <- structure(list(id = 0L), class = "brmsfit")
  ap6_stage_test_env$apply_validity_gate(list("0.10" = list(sdo_d_dominance = fit)), stage_cfg)
  expect_length(seen, 1L)
  expect_identical(seen[[1]]$outcome, "sdo_d_dominance")
  expect_equal(seen[[1]]$slope_sd, 0.1)
  expect_identical(seen[[1]]$max_doublings, stage_cfg$regression$validity_gate$max_ess_doublings)
})

test_that("the final convergence check evaluates the gate once, on the fit it returns", {
  diagnostics_calls <- 0L
  provenance_input_attrs <- NULL
  rlang::local_bindings(
    ap6_fit_diagnostics = function(...) {
      diagnostics_calls <<- diagnostics_calls + 1L
      stage_diag()
    },
    ap6_fit_provenance = function(fit) {
      provenance_input_attrs <<- names(attributes(fit))
      tibble::tibble(source = "mock")
    },
    .env = ap6_stage_test_env
  )
  model <- list(
    fit = structure(list(), class = "brmsfit"),
    outcome = "sdo_dom", slope_sd = 0.2, family_name = "gaussian", sample_prior = "no",
    draws_per_chain = 1000L, warmup = 500L,
    ess_log = tibble::tibble(attempt = 1L),
    retry_log = tibble::tibble(step = integer(0)),
    priors = structure(list(), nu_fixed = NA_real_),
    formula = stage_formula,
    regression = stage_cfg$regression, cfg = stage_cfg
  )
  fit <- ap6_stage_test_env$ap6_check_final_convergence(model, stage_cfg)
  expect_identical(diagnostics_calls, 1L)
  expected <- list(
    outcome = "sdo_dom",
    slope_sd = 0.2,
    family_name = "gaussian",
    sample_prior = "no",
    iter_used = 1000L,
    warmup_used = 500L,
    ess_log = model$ess_log,
    retry_log = model$retry_log,
    gate_status = "ok",
    nu_fixed = NA_real_,
    ci_level = stage_cfg$regression$ci_level,
    seed = as.integer(stage_cfg$regression$seed),
    profile = stage_cfg$profile_name,
    cfg_regression = stage_cfg$regression,
    cfg_sensitivity = stage_cfg$sensitivity,
    formula_text = ap6_stage_test_env$ap6_formula_text(model$formula),
    diagnostics = stage_diag(),
    provenance = tibble::tibble(source = "mock")
  )
  expect_identical(stage_fit_attrs(fit), expected)
  recorded_names <- names(expected)
  expect_identical(intersect(names(attributes(fit)), recorded_names), recorded_names)
  expect_identical(intersect(provenance_input_attrs, recorded_names), head(recorded_names, -1L))
  # With analysis_plan omitted, downstream diagnostics recover the same regression block
  # from the attribute recorded on the fit.
  expect_identical(ap6_stage_test_env$ap6_reg_settings(fit), stage_cfg$regression)
})
