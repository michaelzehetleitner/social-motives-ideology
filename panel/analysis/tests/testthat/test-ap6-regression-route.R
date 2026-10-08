# The whole AP6 regression route, run as the pipeline runs it: the real target
# commands of _targets.R, on the real regression table the preparation route
# builds, with a stand-in sampler.
#
# What this file adds to test-targets-regressions.R (which fabricates empty
# `brmsfit` shells and asserts the wiring and the projections on them): here
# every producer actually executes and every consumer reads that producer's
# output. The validity gate, the result verbs, the report tables and the
# AP10 verbs all run on REAL posterior draws — of a stand-in fit.
#
# The one substitution: `brms::brm()` is mocked to return `fb_fake_brmsfit()`
# (tests/support/fake-brmsfit.R), whose draws, predictions and sampler
# diagnostics are deterministic but real, so no sampler runs; the four helpers
# that read Stan internals (`ap6_max_treedepth`, `ap6_adapt_delta`,
# `ap6_fit_provenance`, `ap6_run_priorsense`) are rebound. Everything between the sampler and AP10 is
# production code.

local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) {
    parent <- dirname(dir)
    if (identical(parent, dir)) stop("project root not found from ", getwd())
    dir <- parent
  }
  for (f in c("config.R", "ap3_imputation_validity.R", "io_qualtrics.R", "ap1_exclusions.R", "ap3_preprocessing.R",
              "ap3_preparation.R", "ap3_fill.R", "ap3_data_files.R", "ap3_pipeline.R")) {
    source(file.path(dir, "R", f), local = FALSE)
  }
  source(file.path(dir, "tests", "support", "targets-route.R"), local = FALSE)
  source(file.path(dir, "tests", "support", "fake-brmsfit.R"), local = FALSE)
})
fb_register_methods()

rg_root <- zm_root()
# Smoke profile: 4 chains, 500 warmup, 1000 post-warmup draws, ESS target 2000.
rg_cfg <- zm_config("smoke", file.path(rg_root, "config", "analysis_plan.yaml"))
rg_book <- zm_codebook(rg_cfg)
rg_raw <- read_qualtrics_export(file.path(rg_root, "data", "synthetic", "zm_panel_synthetic.sav"))
rg_read <- tr_route(rg_root, tr_intake(rg_raw, rg_cfg), rg_cfg, rg_book)
# The environment source_pipeline_functions() filled: the production functions the target
# commands resolve, and therefore the ones a stand-in must be rebound in.
rg_prod <- parent.env(environment(rg_read)$values)
# The shared diagnostic report also accepts summaries of models outside this
# single-outcome route. Mark those inputs unavailable; do not fit other models
# merely to exercise the regression report's real producer/consumer chain.
assign("joint_regression_fit", list(joint = NULL), envir = environment(rg_read)$values)
assign("motive_model_fit", NULL, envir = environment(rg_read)$values)

rg_data <- rg_read("data_regressions")
rg_model_set <- rg_read("regression_model_set")
rg_outcomes <- names(rg_model_set$formulas)
rg_keys <- unname(rg_model_set$outcome_keys[rg_outcomes])
rg_responses <- unname(vapply(rg_model_set$formulas, function(f) all.vars(f)[[1L]], ""))
rg_vars <- fb_b_name(fb_design_columns(rg_model_set$formulas[[1L]], rg_data))
rg_ci <- c((1 - rg_cfg$regression$ci_level) / 2, 1 - (1 - rg_cfg$regression$ci_level) / 2)

# --- the declared posterior of every stand-in fit ----------------------------
# One distinct median per outcome x term, on the scheme tr_estimate() uses in
# test-targets-regressions.R, so that a swapped outcome identity cannot pass a
# test that only counts matching cells. Some motive coefficients are negative
# and each prior width is offset, so interval exclusion and median direction
# genuinely vary across outcomes and widths.
rg_negative <- list(asc_conv = c("b_zm_power_z", "b_zm_prestige_z"),
                    sdo_dom = c("b_zm_security_z", "b_zm_arousal_z"))
rg_shift <- c(primary = 0, sweep_low = -0.12, sweep_high = 0.12, student = 0.55)
rg_mean <- function(key, variable, shift = 0) {
  base <- (match(key, rg_keys) * 100 + match(variable, rg_vars)) / 1000
  (if (variable %in% rg_negative[[key]]) -1 else 1) * base + shift
}
rg_means <- function(key, shift) {
  stats::setNames(vapply(rg_vars, rg_mean, 0, key = key, shift = shift), rg_vars)
}

# --- the scenario the mocked sampler plays -----------------------------------
# authoritarian_aggression passes the gate directly; authoritarian_submission
# misses the ESS target once and passes after ONE doubling; conventionalism
# never reaches it and is not interpretable after the configured three
# doublings; sdo_d_dominance passes directly.
# The residual scale and the shape of the predictive noise decide the one-sided
# heavy-tail trigger: a tight, light-tailed (uniform) replication for
# authoritarian_aggression puts the observed kurtosis above and the observed
# minimum below the replicated band, while a wide, heavy-tailed (t3)
# replication leaves the other three observed statistics far inside it.
rg_primary_plan <- list(
  asc_agg  = list(draws = 1000L, update_draws = 1000L, sigma = 0.30, noise = "light"),
  asc_sub  = list(draws =   60L, update_draws = 1000L, sigma = 2.00, noise = "heavy"),
  asc_conv = list(draws =   60L, update_draws =   60L, sigma = 2.00, noise = "heavy"),
  sdo_dom  = list(draws = 1000L, update_draws = 1000L, sigma = 2.00, noise = "heavy"))

rg_targets <- c(
  "regression_primary_fits", "regression_prior_width_fits", "regression_predictive_checks",
  "regression_student_t_robustness", "regression_fit_register",
  "regression_coefficient_summaries", "regression_r2_summaries",
  "regression_prior_width_sensitivity", "regression_prior_sensitivity",
  "regression_likelihood_robustness",
  "report_primary_coefficients", "supplement_explained_variance", "supplement_sampling_diagnostics",
  "supplement_model_checks",
  "supplement_prior_sensitivity",
  "regression_prediction_decisions")

# Reads every target of the route, in dependency order, while the stand-ins are
# in force. The reader evaluates the actual `_targets.R` commands.
rg_run <- function() {
  recorder <- new.env(parent = emptyenv())
  recorder$brm_calls <- list()
  recorder$update_calls <- list()
  testthat::local_mocked_bindings(brm = function(...) {
    args <- list(...)
    family_name <- as.character(args$family$family)
    sample_prior <- if (is.null(args$sample_prior)) "no" else as.character(args$sample_prior)
    slope_sd <- fb_slope_sd_of(args$prior)
    response <- all.vars(args$formula)[[1L]]
    key <- rg_keys[match(response, rg_responses)]
    kind <- if (identical(sample_prior, "only")) {
      "prior_predictive"
    } else if (identical(family_name, "student")) {
      "student_refit"
    } else if (isTRUE(all.equal(slope_sd, rg_cfg$priors$slope_sd_primary))) {
      "primary"
    } else {
      "sweep"
    }
    spec <- switch(
      kind,
      primary = c(rg_primary_plan[[key]], list(shift = rg_shift[["primary"]])),
      sweep = list(draws = 1000L, update_draws = 1000L, sigma = 1, noise = "normal",
                   shift = if (slope_sd < rg_cfg$priors$slope_sd_primary)
                     rg_shift[["sweep_low"]] else rg_shift[["sweep_high"]]),
      student_refit = list(draws = 1000L, update_draws = 1000L, sigma = 1, noise = "normal",
                           shift = rg_shift[["student"]]),
      # Nothing reads a prior-only fit's ESS; fewer draws keep the sixteen
      # prior-predictive replications cheap.
      prior_predictive = list(draws = 100L, update_draws = 100L, sigma = 1,
                              noise = "normal", shift = 0))
    recorder$brm_calls[[length(recorder$brm_calls) + 1L]] <- list(
      kind = kind, response = response, outcome_key = key, family = family_name,
      sample_prior = sample_prior, slope_sd = slope_sd, formula = args$formula,
      prior = args$prior, data = args$data, chains = args$chains, cores = args$cores,
      warmup = args$warmup, iter = args$iter, thin = args$thin, seed = args$seed,
      backend = args$backend, refresh = args$refresh)
    fb_fake_brmsfit(
      args$formula, args$data, family_name, rg_means(key, spec$shift), sigma = spec$sigma,
      chains = args$chains, draws_per_chain = spec$draws,
      seed = 1000L * match(key, rg_keys) + length(recorder$brm_calls),
      outcome = rg_outcomes[match(key, rg_keys)], role = kind, noise = spec$noise,
      update_draws = spec$update_draws, recorder = recorder)
  }, .package = "brms")
  rlang::local_bindings(
    # Stan internals a stand-in fit has no counterpart for.
    ap6_max_treedepth = function(fit) 10L,
    ap6_adapt_delta = function(fit) 0.8,
    ap6_fit_provenance = fb_fit_provenance,
    # priorsense power-scales the log prior and log likelihood of a compiled
    # Stan model; only its result can be a stand-in here. The real
    # ap6_summarise_powerscale_diagnostics() runs on it.
    ap6_run_priorsense = fb_priorsense,
    .env = rg_prod)
  values <- list()
  for (name in rg_targets) values[[name]] <- rg_read(name)
  list(values = values, recorder = recorder)
}

rr <- rg_run()
rg_v <- rr$values
rg_rec <- rr$recorder

# --- helpers that recompute expectations from the fits' own draws ------------

rg_calls <- function(kind) Filter(function(call) identical(call$kind, kind), rg_rec$brm_calls)
rg_prior_of <- function(prior, class, coef = "") {
  rows <- as.data.frame(prior)
  rows$prior[rows$class == class & rows$coef == coef]
}
rg_record <- function(role, key) {
  hit <- Filter(function(r) identical(r$role, role) && identical(r$outcome_key, key),
                rg_v$regression_fit_register)
  hit[[1L]]
}
# The draws of a stand-in fit, read directly rather than through the verb under
# test, so that the expectation is independent of the production extraction.
rg_own_draws <- function(fit) posterior::as_draws_matrix(fit$fake_draws)
rg_own_summary <- function(fit, variable) {
  x <- as.numeric(rg_own_draws(fit)[, variable])
  quantiles <- unname(stats::quantile(x, rg_ci, type = 7))
  c(estimate = stats::median(x), q_lo = quantiles[1L], q_hi = quantiles[2L],
    p_positive = mean(x > 0), p_negative = mean(x < 0))
}
rg_own_r2 <- function(fit, family, nu) {
  draws <- rg_own_draws(fit)
  design <- stats::model.matrix(fit$fb$formula_raw, data = fit$data)
  expected <- as.matrix(draws[, fit$fb$b_vars, drop = FALSE]) %*% t(design)
  predicted_variance <- apply(expected, 1L, stats::var)
  residual_variance <- as.numeric(draws[, "sigma"])^2
  if (identical(family, "student")) residual_variance <- residual_variance * nu / (nu - 2)
  r2 <- predicted_variance / (predicted_variance + residual_variance)
  c(r2_median = stats::median(r2), r2_lo = unname(stats::quantile(r2, rg_ci[1L])),
    r2_hi = unname(stats::quantile(r2, rg_ci[2L])))
}
rg_cell <- function(table, key, term, column) {
  table[[column]][table$outcome == key & table$term == term]
}

# --- the fitting verbs and the prior-predictive producer --------------------

test_that("the fitting verbs call brms with the route's own table and the registered priors", {
  expect_equal(length(rg_rec$brm_calls), 4L + 8L + 16L + 1L)
  expect_equal(vapply(rg_rec$brm_calls, `[[`, "", "kind") |> table() |> as.integer() |> sort(),
               sort(c(primary = 4L, sweep = 8L, prior_predictive = 16L, student_refit = 1L)) |>
                 as.integer())

  primary <- rg_calls("primary")
  # The four declared formulas, in declared order, on the regression table the
  # route itself built, not a copy.
  expect_equal(vapply(primary, function(call) deparse1(call$formula), ""),
               vapply(rg_model_set$formulas, deparse1, "", USE.NAMES = FALSE))
  for (call in primary) {
    expect_identical(call$data, rg_data)
    expect_identical(attr(call$data, "z_parameters"), attr(rg_data, "z_parameters"))
    expect_identical(rg_prior_of(call$prior, "b"), "normal(0, 0.2)")
    # The gender priors sit on the contrast columns of the actual design matrix.
    contrasts <- grep("^gender", fb_design_columns(call$formula, rg_data), value = TRUE)
    expect_gt(length(contrasts), 0L)
    for (contrast in contrasts) {
      expect_identical(rg_prior_of(call$prior, "b", contrast),
                       paste0("normal(0, ", rg_cfg$priors$gender_sd, ")"))
    }
    expect_identical(rg_prior_of(call$prior, "Intercept"),
                     paste0("normal(0, ", rg_cfg$priors$intercept_sd, ")"))
    expect_identical(rg_prior_of(call$prior, "sigma"),
                     paste0("normal(0, ", rg_cfg$priors$sigma_sd, ")"))
    expect_equal(call$chains, rg_cfg$regression$chains)
    expect_equal(call$warmup, rg_cfg$regression$warmup)
    expect_equal(call$iter, rg_cfg$regression$warmup + rg_cfg$regression$iter_per_chain)
    expect_equal(call$thin, 1)
    expect_equal(call$seed, rg_cfg$regression$seed)
    expect_identical(call$backend, rg_cfg$regression$backend)
    expect_identical(call$family, "gaussian")
  }

  # The comparison widths are exactly the two non-primary registered ones, and
  # their slope priors carry those widths.
  sweep <- rg_calls("sweep")
  expect_setequal(vapply(sweep, `[[`, 0, "slope_sd"), c(0.10, 0.40))
  for (call in sweep) {
    expect_identical(rg_prior_of(call$prior, "b"), paste0("normal(0, ", call$slope_sd, ")"))
  }
  expect_equal(sort(names(rg_v$regression_prior_width_fits)), c("0.10", "0.40"))

  # The descriptive Student refit: student family, primary slope prior, the
  # Student residual-scale prior and the fixed degrees of freedom.
  student <- rg_calls("student_refit")
  expect_equal(length(student), 1L)
  expect_identical(student[[1]]$family, "student")
  expect_identical(student[[1]]$response, "asc_agg_z")
  expect_identical(rg_prior_of(student[[1]]$prior, "b"), "normal(0, 0.2)")
  expect_identical(rg_prior_of(student[[1]]$prior, "sigma"),
                   paste0("normal(0, ", rg_cfg$sensitivity$student_t$sigma_scale_prior_sd, ")"))
  expect_identical(rg_prior_of(student[[1]]$prior, "nu"),
                   paste0("constant(", rg_cfg$sensitivity$student_t$nu_fixed, ")"))
})

test_that("the prior-predictive producer runs the four registered specifications", {
  prior <- rg_calls("prior_predictive")
  expect_equal(length(prior), 16L)
  expect_true(all(vapply(prior, `[[`, "", "sample_prior") == "only"))
  for (key in rg_keys) {
    calls <- Filter(function(call) identical(call$outcome_key, key), prior)
    expect_equal(vapply(calls, `[[`, "", "family"),
                 c("gaussian", "gaussian", "gaussian", "student"))
    expect_equal(vapply(calls, `[[`, 0, "slope_sd"), c(0.10, 0.20, 0.40, 0.20))
    expect_identical(rg_prior_of(calls[[4]]$prior, "sigma"),
                     paste0("normal(0, ", rg_cfg$sensitivity$student_t$sigma_scale_prior_sd, ")"))
    expect_identical(rg_prior_of(calls[[4]]$prior, "nu"),
                     paste0("constant(", rg_cfg$sensitivity$student_t$nu_fixed, ")"))
  }
  # Every prior check reuses the saved standardisation row of its own response
  # column; nothing is standardised again.
  z_parameters <- attr(rg_data, "z_parameters")
  leaves <- unlist(rg_v$regression_predictive_checks$prior, recursive = FALSE)
  expect_equal(length(leaves), 16L)
  for (leaf in leaves) {
    saved <- z_parameters[z_parameters$z_col == leaf$response_column,
                          c("var", "z_col", "mean", "sd")]
    expect_equal(as.data.frame(leaf$fixed_z_constants), as.data.frame(saved),
                 ignore_attr = TRUE, info = leaf$response_column)
    expect_equal(nrow(saved), 1L)
  }
})

# --- the validity gate on real draws -----------------------------------------

test_that("the validity gate executes its ordered remedies on each fit's own draws", {
  fits <- rg_v$regression_primary_fits
  expect_equal(names(fits), rg_outcomes)
  status <- vapply(fits, function(fit) attr(fit, "gate_status"), "")
  expect_equal(unname(status), c("ok", "retried_ok", "not_interpretable", "ok"))
  retries <- vapply(fits, function(fit) nrow(attr(fit, "retry_log")), 0L)
  expect_equal(unname(retries), c(0L, 1L, 3L, 0L))
  attempts <- vapply(fits, function(fit) nrow(attr(fit, "ess_log")), 0L)
  expect_equal(unname(attempts), c(1L, 2L, 4L, 1L))
  for (name in rg_outcomes) {
    fit <- fits[[name]]
    diagnostics <- attr(fit, "diagnostics")
    expect_equal(isTRUE(diagnostics$ok), !identical(name, "conventionalism"), info = name)
    # The ESS decision is the profile's, measured on the fit's own draws.
    expect_equal(diagnostics$ess_target, rg_cfg$regression$ess_target, info = name)
    expect_equal(diagnostics$ess_ok, diagnostics$ess_bulk_min >= rg_cfg$regression$ess_target,
                 info = name)
    expect_true(diagnostics$rhat_ok && diagnostics$divergences_ok &&
                  diagnostics$treedepth_ok && diagnostics$bfmi_ok, info = name)
    expect_equal(attr(fit, "n_obs"), nrow(rg_data), info = name)
  }
  # Only the ESS remedy fired; the reason of every retry row says so.
  expect_true(all(grepl("^ESS shortfall", attr(fits[[2]], "retry_log")$reason)))
  expect_true(all(grepl("^ESS shortfall", attr(fits[[3]], "retry_log")$reason)))
  # The iterations the ESS remedy doubled are recorded per attempt.
  expect_equal(attr(fits[[3]], "ess_log")$iter_per_chain, c(1000L, 2000L, 4000L, 8000L))
  expect_equal(attr(fits[[2]], "iter_used"), 2000L)
  expect_equal(attr(fits[[3]], "iter_used"), 8000L)

  # update() was called exactly as the scenario says: once for the outcome that
  # recovered, three times (the configured cap) for the one that never did.
  updates <- rg_rec$update_calls
  expect_equal(length(updates), 4L)
  expect_equal(vapply(updates, `[[`, "", "outcome"),
               c("authoritarian_submission", rep("conventionalism", 3L)))
  expect_equal(vapply(updates, `[[`, 0, "iter"), c(2500, 2500, 4500, 8500))
  expect_true(all(vapply(updates, `[[`, 0, "warmup") == rg_cfg$regression$warmup))


  # The prior-width and Student collections pass the same gate.
  for (width in names(rg_v$regression_prior_width_fits)) {
    widths <- rg_v$regression_prior_width_fits[[width]]
    expect_equal(names(widths), rg_outcomes, info = width)
    expect_true(all(vapply(widths, function(fit) attr(fit, "gate_status"), "") == "ok"),
                info = width)
  }
  student <- rg_v$regression_student_t_robustness
  expect_equal(names(student), rg_outcomes)
  expect_identical(attr(student[[1]]$fit, "gate_status"), "ok")
  expect_null(student[[2]])
})

# --- the heavy-tail trigger of the posterior-predictive checks ---------------

test_that("the one-sided trigger fires for the outcome whose replication is light-tailed", {
  checks <- rg_v$regression_predictive_checks$posterior
  expect_equal(names(checks), rg_outcomes)
  fired <- checks[["authoritarian_aggression"]]
  expect_gt(fired$excess_kurtosis$observed, fired$excess_kurtosis$replicated_interval[[2]])
  expect_lt(fired$min$observed, fired$min$replicated_interval[[1]])
  for (name in setdiff(rg_outcomes, "authoritarian_aggression")) {
    quiet <- checks[[name]]
    expect_lt(quiet$excess_kurtosis$observed, quiet$excess_kurtosis$replicated_interval[[2]])
    expect_gt(quiet$min$observed, quiet$min$replicated_interval[[1]])
    expect_lt(quiet$max$observed, quiet$max$replicated_interval[[2]])
  }
  expect_equal(unname(attr(rg_v$regression_student_t_robustness, "heavy_tail_triggers")),
               c(TRUE, FALSE, FALSE, FALSE))
  # The observed statistics are the regression table's own column.
  expect_equal(fired$min$observed, min(rg_data$asc_agg_z))
})

# --- the register on the real gated fits -------------------------------------
# The section "the fit register and its lifecycle" of test-targets-regressions.R
# fixes the register's field list, role counts and failure branches on
# fabricated fits. What only real fits can show is asserted here: the
# diagnostics, histories, fitted N and provenance the gate captured, and the
# identity each record resolves.

test_that("the register carries the gate result of every real fit it names", {
  register <- rg_v$regression_fit_register
  expect_equal(length(register), 4L + 8L + 4L)
  primary_keys <- vapply(Filter(function(r) r$role == "primary", register), `[[`, "", "outcome_key")
  expect_equal(primary_keys, rg_keys)
  expect_equal(vapply(rg_keys, function(k) rg_record("primary", k)$fit_valid, TRUE),
               stats::setNames(c(TRUE, TRUE, FALSE, TRUE), rg_keys))
  expect_equal(vapply(rg_keys, function(k) rg_record("primary", k)$gate_status, ""),
               stats::setNames(c("ok", "retried_ok", "not_interpretable", "ok"), rg_keys))
  for (key in rg_keys) {
    record <- rg_record("primary", key)
    expect_identical(record$response_column, rg_responses[match(key, rg_keys)])
    expect_identical(record$outcome, rg_outcomes[match(key, rg_keys)])
    expect_equal(record$slope_sd, rg_cfg$priors$slope_sd_primary)
    expect_identical(record$family, "gaussian")
    expect_true(is.na(record$nu_fixed))
    expect_identical(record$label, as.character(rg_cfg$priors$sweep_labels[["0.2"]]))
    expect_equal(record$n_obs, nrow(rg_data))
    expect_equal(nrow(record$provenance), 1L)
    expect_identical(record$provenance$gate_status, record$gate_status)
  }
  expect_equal(nrow(rg_record("primary", "asc_sub")$retry_history), 1L)
  expect_equal(nrow(rg_record("primary", "asc_conv")$retry_history), 3L)
  expect_equal(nrow(rg_record("primary", "asc_agg")$ess_history), 1L)

  student <- Filter(function(r) r$role == "student_refit", register)
  expect_equal(vapply(student, `[[`, TRUE, "fit_available"), c(TRUE, FALSE, FALSE, FALSE))
  expect_equal(vapply(student, `[[`, TRUE, "heavy_tail_trigger"), c(TRUE, FALSE, FALSE, FALSE))
  expect_equal(vapply(student[-1], `[[`, "", "note"), rep("not triggered", 3L))
  expect_equal(rg_record("student_refit", "asc_agg")$nu_fixed,
               as.numeric(rg_cfg$sensitivity$student_t$nu_fixed))
  expect_equal(vapply(Filter(function(r) r$role == "sweep", register), `[[`, 0, "slope_sd"),
               rep(c(0.10, 0.40), each = 4L))
})

test_that("a fit collection the model set does not declare stops in the register", {
  renamed <- rg_v$regression_prior_width_fits
  names(renamed[["0.10"]])[1] <- "not_a_model"
  expect_error(
    rg_prod$extract_regression_result_records(
      rg_v$regression_primary_fits, renamed,
      rg_v$regression_student_t_robustness, rg_cfg),
    "does not declare")
  short <- unname(rg_v$regression_primary_fits[1:3])
  expect_error(
    rg_prod$extract_regression_result_records(
      short, rg_v$regression_prior_width_fits,
      rg_v$regression_student_t_robustness, rg_cfg),
    "lost its outcome labels")
})

# --- the coefficient and R2 result verbs -------------------------------------

test_that("the coefficient result summarises each fit's own posterior draws", {
  summaries <- rg_v$regression_coefficient_summaries
  register <- rg_v$regression_fit_register
  # Thirteen available fits (4 primary, 8 sweep, 1 Student refit) times ten
  # parameters, plus the three untriggered refits' unavailable rows.
  expect_equal(nrow(summaries), 13L * (length(rg_vars) + 1L) + 3L)
  for (role in c("primary", "sweep", "student_refit")) {
    for (key in rg_keys) {
      record <- rg_record(role, key)
      if (!record$fit_available) next
      rows <- summaries[summaries$role == role & summaries$outcome_key == key, , drop = FALSE]
      if (identical(role, "sweep")) rows <- rows[rows$slope_sd == 0.10, , drop = FALSE]
      expect_equal(nrow(rows), length(rg_vars) + 1L, info = paste(role, key))
      fit <- if (identical(role, "sweep")) {
        rg_record("sweep", key)$fit
      } else {
        record$fit
      }
      for (variable in c(rg_vars, "sigma")) {
        expected <- rg_own_summary(fit, variable)
        row <- rows[rows$variable == variable, , drop = FALSE]
        expect_equal(row$estimate, unname(expected[["estimate"]]), info = variable)
        expect_equal(row$q_lo, unname(expected[["q_lo"]]), info = variable)
        expect_equal(row$q_hi, unname(expected[["q_hi"]]), info = variable)
        expect_equal(row$p_positive, unname(expected[["p_positive"]]), info = variable)
        expect_equal(row$p_negative, unname(expected[["p_negative"]]), info = variable)
      }
      # The identity travels with the values: the medians are this fit's.
      expect_equal(rows$estimate[rows$variable == "b_zm_power_z"],
                   rg_mean(key, "b_zm_power_z",
                           if (identical(role, "primary")) 0 else
                             if (identical(role, "sweep")) rg_shift[["sweep_low"]] else
                               rg_shift[["student"]]))
    }
  }
  # The term keys are the configured ones; gender keeps its contrast name.
  primary <- summaries[summaries$role == "primary" & summaries$outcome_key == "asc_agg", ]
  expect_equal(primary$term[primary$variable == "b_zm_security_z"],
               unname(rg_cfg$regression$term_keys[["zm_security_z"]]))
  expect_equal(primary$term_type[primary$variable == "b_zm_security_z"], "predictor")
  expect_equal(primary$term[primary$variable == "b_gendermale"], "gendermale")
  expect_equal(primary$term_type[primary$variable == "b_gendermale"], "covariate")
  expect_equal(primary$term_type[primary$variable == "sigma"], "sigma")

  # Highest-density intervals are attached evidence of the same draws.
  expect_true(all(c("hdi_lo", "hdi_hi") %in% names(summaries)))
  expect_true(all(is.na(summaries$hdi_lo[summaries$variable == "sigma"])))

  # The not-interpretable fit keeps its rows and its verdict-free status.
  failed <- summaries[summaries$role == "primary" & summaries$outcome_key == "asc_conv", ]
  expect_equal(nrow(failed), length(rg_vars) + 1L)
  expect_true(all(failed$fit_valid == FALSE))
  expect_true(all(failed$gate_status == "not_interpretable"))
  # The untriggered refits keep one unavailable row each.
  unavailable <- summaries[summaries$role == "student_refit" & !summaries$fit_available, ]
  expect_equal(nrow(unavailable), 3L)
  expect_true(all(unavailable$note == "not triggered"))

  # The result verb rebuilds the register internally; both instances must agree
  # about every fit's gate.
  gate_of <- function(records) {
    vapply(records, function(r) paste(r$role, r$outcome_key, r$slope_sd, r$gate_status), "")
  }
  from_summary <- unique(paste(summaries$role, summaries$outcome_key, summaries$slope_sd,
                               summaries$gate_status))
  expect_setequal(from_summary, gate_of(register))
})

test_that("the R2 result recomputes explained variance from the same draws", {
  r2 <- rg_v$regression_r2_summaries
  for (role in c("primary", "student_refit")) {
    for (key in rg_keys) {
      record <- rg_record(role, key)
      if (!record$fit_available) next
      row <- r2[r2$role == role & r2$outcome_key == key, , drop = FALSE]
      expect_equal(nrow(row), 1L, info = paste(role, key))
      expected <- rg_own_r2(record$fit, record$family, record$nu_fixed)
      expect_equal(row$r2_median, unname(expected[["r2_median"]]), info = paste(role, key))
      expect_equal(row$r2_lo, unname(expected[["r2_lo"]]), info = paste(role, key))
      expect_equal(row$r2_hi, unname(expected[["r2_hi"]]), info = paste(role, key))
    }
  }
  # The Student definition inflates the residual variance by nu / (nu - 2).
  student <- rg_record("student_refit", "asc_agg")
  gaussian_definition <- rg_own_r2(student$fit, "gaussian", NA_real_)
  expect_false(isTRUE(all.equal(unname(gaussian_definition[["r2_median"]]),
                                r2$r2_median[r2$role == "student_refit" &
                                               r2$outcome_key == "asc_agg"])))
  expect_equal(sum(!is.na(r2$r2_median)), 13L)
})

# --- prior width, local sensitivity, Gaussian vs Student ---------------------

test_that("the prior-width stability reads the three fitted widths", {
  stability <- rg_v$regression_prior_width_sensitivity
  expect_equal(length(stability), length(rg_outcomes))
  for (i in seq_along(rg_outcomes)) {
    coefficients <- stability[[i]]
    expect_equal(length(coefficients), length(rg_vars))
    for (entry in coefficients) {
      expect_identical(entry$outcome, rg_outcomes[[i]])
      expect_equal(names(entry$by_width), c("0.10", "0.20", "0.40"))
      key <- rg_keys[[i]]
      shifts <- c(rg_shift[["sweep_low"]], 0, rg_shift[["sweep_high"]])
      for (w in seq_along(shifts)) {
        expect_equal(entry$by_width[[w]]$median,
                     rg_mean(key, entry$coefficient, shifts[[w]]),
                     info = paste(key, entry$coefficient, w))
        interval <- entry$by_width[[w]]$interval
        expect_equal(entry$by_width[[w]]$interval_excludes_zero,
                     interval[[1]] > 0 || interval[[2]] < 0)
      }
      expect_equal(entry$interval_exclusion_stable,
                   length(unique(vapply(entry$by_width, `[[`, TRUE,
                                        "interval_excludes_zero"))) == 1L)
      expect_equal(entry$median_direction_stable,
                   length(unique(sign(vapply(entry$by_width, `[[`, 0, "median")))) == 1L)
      expect_equal(entry$indicators, c("interval_excludes_zero", "median_direction"))
    }
  }
  # The offsets make both indicators genuinely vary; a constant column would
  # make the assertions above vacuous.
  flat <- unlist(lapply(stability, function(o) vapply(o, `[[`, TRUE, "median_direction_stable")))
  expect_true(any(flat) && any(!flat))
  excl <- unlist(lapply(stability, function(o) vapply(o, `[[`, TRUE, "interval_exclusion_stable")))
  expect_true(any(excl) && any(!excl))
})

test_that("the local prior sensitivity keeps the power-scaling matrices per block", {
  sensitivity <- rg_v$regression_prior_sensitivity
  expect_equal(names(sensitivity), rg_outcomes)
  blocks <- as.character(rg_cfg$sensitivity$powerscale$blocks)
  for (outcome in names(sensitivity)) {
    summary <- sensitivity[[outcome]]
    expect_equal(names(summary$blocks), blocks)
    expect_equal(summary$diagnostic_threshold, rg_cfg$sensitivity$powerscale$sensitivity_threshold)
    if (outcome == "conventionalism") {
      expect_false(summary$fit_valid)
      expect_equal(summary$fit_gate_status, "not_interpretable")
      expect_false(any(vapply(summary$blocks, `[[`, TRUE, "interpretable")))
    } else {
      expect_true(summary$fit_valid)
      expect_true(all(vapply(summary$blocks, `[[`, TRUE, "interpretable")))
    }
    expect_equal(names(summary$sensitivity$blocks), blocks)
    expect_equal(summary$sensitivity$settings$lower_alpha, rg_cfg$sensitivity$powerscale$lower_alpha)
  }
})

test_that("the Gaussian-Student comparison pairs only the triggered outcome", {
  paired <- rg_v$regression_likelihood_robustness
  expect_equal(nrow(paired$r2), 4L)
  expect_false(is.na(paired$r2$r2_median_student[paired$r2$outcome == "authoritarian_aggression"]))
  expect_true(all(is.na(paired$r2$r2_median_student[
    paired$r2$outcome != "authoritarian_aggression"])))
  coefficients <- paired$coefficients
  expect_equal(nrow(coefficients), 4L * (length(rg_vars) + 1L))
  triggered <- coefficients[coefficients$outcome == "authoritarian_aggression", ]
  expect_false(any(is.na(triggered$estimate_student)))
  expect_true(all(is.na(coefficients$estimate_student[
    coefficients$outcome != "authoritarian_aggression"])))
  expect_equal(triggered$estimate_student[triggered$term == "zm_power"],
               rg_mean("asc_agg", "b_zm_power_z", rg_shift[["student"]]))
  expect_equal(triggered$estimate_gaussian[triggered$term == "zm_power"],
               rg_mean("asc_agg", "b_zm_power_z"))
})

test_that("an entirely failed original-fit set keeps raw power-scaling values without summary flags", {
  sensitivity <- rg_v$regression_prior_sensitivity
  for (outcome in names(sensitivity)) {
    sensitivity[[outcome]]$fit_valid <- FALSE
    sensitivity[[outcome]]$fit_gate_status <- "not_interpretable"
  }
  result <- assemble_supplement_prior_sensitivity(
    rg_v$regression_coefficient_summaries, rg_v$regression_fit_register,
    sensitivity, rg_v$regression_prior_width_sensitivity,
    rg_data, rg_book, rg_cfg
  )
  expect_true(all(is.finite(result$priorsense$prior_sens)))
  expect_false(any(result$priorsense$interpretable))
  expect_equal(nrow(result$priorsense_flagged), nrow(result$priorsense))
  expect_identical(result$powerscale$n_flagged, 0L)
  expect_identical(result$powerscale$n_interpretable, 0L)
  expect_true(is.na(result$powerscale$prior_max))
  expect_true(is.na(result$powerscale$lik_max))
  expect_false(result$answer$any_successful)
  expect_identical(result$answer$n_flagged, 0L)
})

# --- the report tables of the real results -----------------------------------

test_that("the coefficient, R2 and diagnostics tables carry the real fits", {
  coefs <- rg_prod$tabulate_regression_coefficients(
    rg_v$regression_coefficient_summaries, rg_v$regression_fit_register, rg_cfg,
    roles = c("primary", "sweep"))
  expect_equal(nrow(coefs), 12L * (length(rg_vars) + 1L))
  # The key, interval, precision and gate columns the report reads (their full
  # 37-name list is pinned by the test-targets-regressions.R test "the
  # coefficient table keeps every key, interval and gate field"; here they must
  # survive the real verbs).
  for (column in c("label", "outcome", "slope_sd", "role", "term", "term_type", "estimate",
                   "sd", "q2.5", "q97.5", "p_positive", "p_negative", "ess_bulk", "ess_tail",
                   "rhat", "mcse_median", "fit_valid", "gate_status", "n_obs", "iter_used",
                   "family", "nu_fixed", "variable", "hdi_lo", "hdi_hi", "outcome_key",
                   "response_column", "fit_available", "note")) {
    expect_true(column %in% names(coefs), info = column)
  }
  expect_setequal(unique(coefs$slope_sd), c(0.1, 0.2, 0.4))
  expect_setequal(unique(coefs$outcome), rg_keys)
  # The gate fields joined by fit identity, not by row position.
  for (key in rg_keys) {
    rows <- coefs[coefs$outcome == key & coefs$role == "primary", ]
    expect_equal(unique(rows$gate_status), rg_record("primary", key)$gate_status, info = key)
    expect_equal(unique(rows$n_obs), nrow(rg_data), info = key)
    expect_equal(unique(rows$iter_used), attr(rg_record("primary", key)$fit, "iter_used"),
                 info = key)
  }
  primary <- rg_v$report_primary_coefficients
  expect_equal(nrow(primary), 4L * (length(rg_vars) + 1L))
  expect_true(all(primary$role == "primary"))
  # Exact cells: a swapped outcome identity would move every one of them.
  expect_equal(rg_cell(primary, "asc_agg", "zm_security", "estimate"), rg_mean("asc_agg", "b_zm_security_z"))
  expect_equal(rg_cell(primary, "asc_sub", "zm_power", "estimate"), rg_mean("asc_sub", "b_zm_power_z"))
  expect_equal(rg_cell(primary, "asc_conv", "zm_prestige", "estimate"), rg_mean("asc_conv", "b_zm_prestige_z"))
  expect_equal(rg_cell(primary, "sdo_dom", "zm_arousal", "estimate"), rg_mean("sdo_dom", "b_zm_arousal_z"))
  # Every coefficient of every outcome is its own value, so a swap is visible.
  expect_equal(anyDuplicated(primary$estimate[primary$term_type != "sigma"]), 0L)

  r2 <- rg_prod$tabulate_explained_variance(rg_v$regression_r2_summaries, rg_cfg)
  expect_identical(rg_v$supplement_explained_variance$r2_primary, r2[r2$role %in% "primary", , drop = FALSE])
  expect_equal(nrow(r2), 12L)
  expect_true(all(r2$family == "gaussian"))
  expect_equal(r2$r2_median[r2$outcome == "asc_agg" & r2$role == "primary"],
               unname(rg_own_r2(rg_record("primary", "asc_agg")$fit, "gaussian", NA_real_)[["r2_median"]]))

  diagnostics <- rg_v$supplement_sampling_diagnostics$diagnostics
  expect_equal(nrow(diagnostics), 12L)
  expect_equal(diagnostics$gate_status[diagnostics$outcome == "asc_conv" &
                                         diagnostics$role == "primary"], "not_interpretable")
  expect_equal(unique(diagnostics$max_treedepth_used), 10L)

  student_coefs <- rg_v$supplement_model_checks$student_coefs
  expect_true(all(student_coefs$role == "student"))
  expect_equal(sum(is.na(student_coefs$term)), 3L)
  expect_equal(unique(student_coefs$nu_fixed), 4)
  expect_equal(sum(student_coefs$note == "not triggered", na.rm = TRUE), 3L)
})

test_that("the tail and predictive projections read the producers", {
  expect_equal(rg_v$supplement_model_checks$tails$outcome, rg_keys)
  expect_equal(rg_v$supplement_model_checks$tails$heavy_tails,
               unname(attr(rg_v$regression_student_t_robustness, "heavy_tail_triggers")))

  prior_summary <- rg_v$supplement_model_checks$prior_predictive
  expect_equal(nrow(prior_summary), 16L)
  expect_equal(prior_summary$outcome, rep(rg_keys, each = 4L))
  expect_equal(prior_summary$family, rep(c("gaussian", "gaussian", "gaussian", "student"), 4L))
  expect_setequal(unique(prior_summary$role), c("primary", "sweep", "student_refit"))
  expect_true(all(prior_summary$n_obs == nrow(rg_data)))

  stats_tbl <- rg_v$supplement_model_checks$posterior_predictive
  expect_equal(nrow(stats_tbl), 4L * 6L)
  expect_equal(unique(stats_tbl$outcome), rg_keys)
  expect_setequal(unique(stats_tbl$stat), c("mean", "sd", "min", "max", "kurtosis", "skew"))
  expect_equal(stats_tbl$observed[stats_tbl$outcome == "asc_agg" & stats_tbl$stat == "min"],
               min(rg_data$asc_agg_z))

  draws_tbl <- rg_v$supplement_model_checks$predictive_draws
  expect_equal(unique(draws_tbl$outcome), rg_keys)
  expect_setequal(unique(draws_tbl$type), c("y", "y_rep"))
  expect_true("draw" %in% names(draws_tbl))
  expect_true(all(is.na(draws_tbl$draw[draws_tbl$type == "y"])))
  expect_equal(nrow(draws_tbl), 4L * nrow(rg_data) * 101L)

  priorsense <- rg_v$supplement_prior_sensitivity$priorsense
  expect_equal(nrow(priorsense), 4L * length(rg_cfg$sensitivity$powerscale$blocks) * 2L)
  expect_setequal(unique(priorsense$block), as.character(rg_cfg$sensitivity$powerscale$blocks))
  expect_true("derived" %in% priorsense$term_type)
  expect_equal(unique(priorsense$settings_status), "explicit")
})

# --- the Table 3 decisions: AP10 ---------------------------------------------
# The decisions of regression_prediction_decisions on the real primary
# coefficients. The decision verb itself is asserted in
# test-ap7-joint-route.R ("the Table 3 decisions carry each primary fit's own
# cell and verdict").

test_that("the AP10 verb decides on the real primary coefficients", {
  decisions <- rg_v$regression_prediction_decisions
  expect_equal(nrow(decisions), 20L)
  failed <- decisions[decisions$outcome_key == "asc_conv", ]
  expect_true(all(is.na(failed$verdict)))
  expect_true(all(is.na(failed$credible)))
  expect_false(any(failed$fit_valid))
  decided <- decisions[decisions$outcome_key != "asc_conv", ]
  expect_false(any(is.na(decided$verdict)))
  # The sign of each cell is the sign of the stand-in's declared median.
  for (i in seq_len(nrow(decisions))) {
    variable <- paste0("b_", names(rg_cfg$regression$term_keys)[
      match(decisions$predictor[[i]], unname(rg_cfg$regression$term_keys))])
    expect_equal(decisions$estimate[[i]], rg_mean(decisions$outcome_key[[i]], variable),
                 info = paste(decisions$outcome_key[[i]], decisions$predictor[[i]]))
    expect_equal(decisions$observed_sign[[i]],
                 if (decisions$estimate[[i]] > 0) "+" else "-")
  }

  likelihood <- rg_v$supplement_model_checks$likelihood_comparison
  expect_true(all(likelihood$fitted_student[likelihood$outcome == "asc_agg"]))
  expect_false(any(likelihood$fitted_student[likelihood$outcome != "asc_agg"]))
  expect_equal(likelihood$estimate_gaussian[likelihood$outcome == "asc_agg" &
                                              likelihood$term == "zm_security"],
               rg_mean("asc_agg", "b_zm_security_z"))

  sensitivity <- rg_v$supplement_prior_sensitivity$sensitivity
  expect_equal(nrow(sensitivity), 20L)
  expect_true(all(c("est_0.1", "est_0.2", "est_0.4") %in% names(sensitivity)))
  expect_equal(sensitivity$est_0.1[sensitivity$outcome == "asc_agg" &
                                     sensitivity$term == "zm_security"],
               rg_mean("asc_agg", "b_zm_security_z", rg_shift[["sweep_low"]]))
  expect_equal(sensitivity$est_0.4[sensitivity$outcome == "asc_agg" &
                                     sensitivity$term == "zm_security"],
               rg_mean("asc_agg", "b_zm_security_z", rg_shift[["sweep_high"]]))
})
