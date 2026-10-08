# The whole AP7 / RQ2 joint route, run as the pipeline runs it: the real target
# commands of _targets.R, on the real regression table the preparation route
# builds, with a stand-in sampler.
#
# What this file adds to the other joint tests: test-targets-joint.R reads the
# target graph and exercises the verbs on stubs that carry no draws,
# test-ap7-joint-comparisons.R tests the AP7 verbs on hand-written draw frames,
# and test-joint-route-fit.R runs the route once on a real CmdStan fit. Here
# every producer of the route actually executes inside the target graph and
# every consumer reads that producer's output: the shared validity gate, the
# AP7 result verbs, the Table 3 decisions and the report tables all run on REAL
# posterior draws — of a multivariate stand-in fit.
#
# The one substitution is the sampler, so no model is fitted here.
# `brms::brm()` is mocked (tests/support/fake-brmsfit.R): the four primary fits,
# the two prior-width sweeps and the sixteen prior-predictive runs get the
# univariate stand-in, the joint call gets the multivariate one, whose
# parameters carry brms's response prefixes and whose six `rescor__` draws are
# real. The four helpers that read Stan internals (`ap6_max_treedepth`,
# `ap6_adapt_delta`, `ap6_fit_provenance`, `ap6_run_priorsense`) are rebound exactly as
# test-ap6-regression-route.R rebinds them. Everything else between the sampler
# and the tables is production code; the joint provenance row therefore
# carries the stand-in's formula text, Stan hash and versions.
#
# The sections, in order:
#   J1  the joint model definition (data identity, formulas, priors, response ids)
#   J2  the recorded joint `brms::brm()` call
#   J3  the shared validity gate on a passing joint fit
#   J4  the shared validity gate on a joint fit that never reaches the ESS target
#   J5  `extract_joint_posterior()` on the joint fit's own draws
#   J6  the overall ASC vs SDO-D package
#   J7  the within-ASC package
#   J8  the ASC-component vs SDO-D package
#   J9  an invalid joint fit withholds classification, keeps numbers
#   J10 a joint fit the sampler never returns, carried as unavailable through
#       the actual target route and every report target that reads the joint fit
#   J11 the Table 3 decisions of the four primary fits
#   J12 the joint provenance row
#   J13 the RQ2 report target

local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) {
    parent <- dirname(dir)
    if (identical(parent, dir)) stop("project root not found from ", getwd())
    dir <- parent
  }
  for (f in c("config.R", "io_qualtrics.R", "ap1_exclusions.R", "ap3_preprocessing.R",
              "ap3_preparation.R", "ap3_fill.R", "ap3_data_files.R", "ap3_pipeline.R")) {
    source(file.path(dir, "R", f), local = FALSE)
  }
  source(file.path(dir, "tests", "support", "targets-route.R"), local = FALSE)
  source(file.path(dir, "tests", "support", "fake-brmsfit.R"), local = FALSE)
})
fb_register_methods()

jt_root <- zm_root()
# Smoke profile: 4 chains, 500 warmup, 1000 post-warmup draws, ESS target 2000.
jt_cfg <- zm_config("smoke", file.path(jt_root, "config", "analysis_plan.yaml"))
jt_book <- zm_codebook(jt_cfg)
jt_level <- jt_cfg$regression$ci_level
jt_ci <- c((1 - jt_level) / 2, 1 - (1 - jt_level) / 2)
jt_intake <- tr_intake(
  read_qualtrics_export(file.path(jt_root, "data", "synthetic", "zm_panel_synthetic.sav")),
  jt_cfg)

# Each scenario reads through its OWN reader: a reader caches every target it
# has evaluated, so a second scenario on the same reader would silently reuse
# the first scenario's joint fit.
jt_reader <- function() {
  read <- tr_route(jt_root, jt_intake, jt_cfg, jt_book)
  # The environment source_pipeline_functions() filled: the production functions the target
  # commands resolve, and therefore the ones a stand-in must be rebound in.
  prod <- parent.env(environment(read)$values)
  list(read = read, prod = prod)
}

jt_design <- jt_reader()
jt_data <- jt_design$read("data_regressions")
jt_model_set <- jt_design$read("regression_model_set")
jt_outcomes <- names(jt_model_set$formulas)
jt_keys <- unname(jt_model_set$outcome_keys[jt_outcomes])
jt_responses <- unname(vapply(jt_model_set$formulas, function(f) all.vars(f)[[1L]], ""))
jt_vars <- fb_b_name(fb_design_columns(jt_model_set$formulas[[1L]], jt_data))
jt_model <- jt_design$read("joint_regression_model")
jt_resp <- unname(jt_model$response_ids)
jt_motive_cols <- c("zm_security_z", "zm_achievement_z", "zm_power_z", "zm_prestige_z", "zm_arousal_z")
# the configured term key of a model column, read backwards: predictor -> column
jt_column_of <- stats::setNames(names(jt_cfg$regression$term_keys),
                                unname(unlist(jt_cfg$regression$term_keys)))

# The AP7 display names, written out rather than derived, so that a swapped
# response identity cannot pass a test that only counts matching cells.
jt_outcome_resp <- c(aggression = "ascaggz", submission = "ascsubz",
                     conventionalism = "ascconvz", sdo_d = "sdodomz")
# The joint posterior names each motive by its key, in the codebook's order.
jt_motive_col <- c(zm_security = "zm_security_z", zm_arousal = "zm_arousal_z", zm_power = "zm_power_z",
                   zm_prestige = "zm_prestige_z", zm_achievement = "zm_achievement_z")
jt_pair_rescor <- c(
  aggression_submission = "rescor__ascaggz__ascsubz",
  aggression_conventionalism = "rescor__ascaggz__ascconvz",
  submission_conventionalism = "rescor__ascsubz__ascconvz",
  aggression_sdo_d = "rescor__ascaggz__sdodomz",
  submission_sdo_d = "rescor__ascsubz__sdodomz",
  conventionalism_sdo_d = "rescor__ascconvz__sdodomz")

# --- the declared posterior of every stand-in fit ----------------------------
# One distinct median per outcome x term everywhere, chosen so that each rule
# under test genuinely varies: the 95% interval of a coefficient is about
# +/- .098 wide, so a median below that is unresolved and a larger one is
# credible; the interval of a difference of two coefficients is about +/- .14.

# The four univariate fits, one designed value per prediction cell, so that
# every Table 3 verdict label occurs at least once.
jt_uni_cell <- matrix(c(
   0.05,  0.03,  0.31,  0.01, -0.35,
   0.37,  0.39, -0.41,  0.43, -0.45,
   0.47, -0.49,  0.51, -0.53, -0.55,
   0.57,  0.07,  0.61, -0.63,  0.65),
  nrow = 4L, byrow = TRUE, dimnames = list(jt_keys, jt_motive_cols))
jt_uni_other <- c("(Intercept)" = 1.10, age_z = 1.20, gendermale = -1.30, income_z = 1.40)
jt_uni_mean <- function(key, variable, shift = 0) {
  column <- sub("^b_", "", variable)
  if (column %in% jt_motive_cols) return(unname(jt_uni_cell[key, column]) + shift)
  name <- if (identical(column, "Intercept")) "(Intercept)" else column
  base <- jt_uni_other[[name]]
  unname(sign(base) * (abs(base) + 0.01 * match(key, jt_keys)) + shift)
}
jt_uni_means <- function(key, shift = 0) {
  stats::setNames(vapply(jt_vars, jt_uni_mean, 0, key = key, shift = shift), jt_vars)
}

# The joint fit: the SDO-D row sits above the ASC mean for Intimacy and
# Prestige, the three ASC rows separate for Power and Prestige alone, and the
# Achievement column stays inside every interval.
jt_joint_cell <- matrix(c(
   0.30,  0.02,  0.44, -0.20, -0.34,
   0.22, -0.04,  0.16,  0.24, -0.28,
   0.26,  0.06,  0.12, -0.16, -0.30,
   0.62, -0.02,  0.18,  0.40, -0.32),
  nrow = 4L, byrow = TRUE, dimnames = list(jt_resp, jt_motive_cols))
jt_joint_other <- c(Intercept = 1.10, age_z = 1.20, gendermale = -1.30, income_z = 1.40)
jt_joint_means <- function() {
  columns <- fb_mv_design_columns(jt_model$formula, jt_data)
  out <- numeric(0)
  for (resp in jt_resp) {
    clean <- ifelse(columns[[resp]] == "(Intercept)", "Intercept", columns[[resp]])
    values <- vapply(clean, function(column) {
      if (column %in% jt_motive_cols) return(unname(jt_joint_cell[resp, column]))
      base <- jt_joint_other[[column]]
      unname(sign(base) * (abs(base) + 0.01 * match(resp, jt_resp)))
    }, 0)
    out <- c(out, stats::setNames(values, fb_mv_b_names(resp, columns[[resp]])))
  }
  out
}
jt_sigma <- stats::setNames(c(0.9, 1.0, 1.1, 1.2), paste0("sigma_", jt_resp))
# within-ASC mean .36, ASC-SDO-D mean -.08: the residual difference resolves.
jt_rescor <- stats::setNames(c(0.42, 0.36, -0.12, 0.30, 0.08, -0.20),
                             fb_mv_rescor_names(jt_resp))

# --- the scenario the mocked sampler plays -----------------------------------
# The four primary fits pass the gate directly on 4 x 1000 draws; their wide,
# heavy-tailed replication leaves every observed statistic inside its band, so
# the one-sided tail trigger never fires and no Student refit is requested.
# Only the joint fit varies between the three runs.

jt_targets <- c(
  "regression_model_set", "regression_primary_fits", "regression_prior_width_fits",
  "regression_predictive_checks", "regression_student_t_robustness",
  "regression_fit_register", "regression_coefficient_summaries",
  "regression_prediction_decisions", "joint_regression_model", "joint_regression_fit",
  "joint_posterior", "joint_correlation_changes", "joint_within_asc", "joint_asc_components_sdo",
  "report_joint", "supplement_software")

jt_run <- function(joint = list(draws = 1000L, update_draws = 1000L),
                   targets = jt_targets, reader = jt_reader()) {
  read <- reader$read
  recorder <- new.env(parent = emptyenv())
  recorder$brm_calls <- list()
  recorder$update_calls <- list()
  testthat::local_mocked_bindings(brm = function(...) {
    args <- list(...)
    if (inherits(args$formula, "mvbrmsformula")) {
      recorder$brm_calls[[length(recorder$brm_calls) + 1L]] <- list(
        kind = "joint", formula = args$formula, prior = args$prior, data = args$data,
        sample_prior = as.character(args$sample_prior), chains = args$chains,
        cores = args$cores, warmup = args$warmup, iter = args$iter, thin = args$thin,
        seed = args$seed, backend = args$backend, refresh = args$refresh)
      if (isTRUE(joint$fail)) stop("stand-in sampler: the joint model did not fit.")
      return(fb_fake_brmsfit(
        args$formula, args$data, slope_means = jt_joint_means(), sigma = jt_sigma,
        rescor_means = jt_rescor, chains = args$chains, draws_per_chain = joint$draws,
        seed = 90001L, outcome = "joint", role = "joint",
        update_draws = joint$update_draws, recorder = recorder))
    }
    family_name <- as.character(args$family$family)
    sample_prior <- if (is.null(args$sample_prior)) "no" else as.character(args$sample_prior)
    slope_sd <- fb_slope_sd_of(args$prior)
    response <- all.vars(args$formula)[[1L]]
    key <- jt_keys[match(response, jt_responses)]
    kind <- if (identical(sample_prior, "only")) {
      "prior_predictive"
    } else if (identical(family_name, "student")) {
      "student_refit"
    } else if (isTRUE(all.equal(slope_sd, jt_cfg$priors$slope_sd_primary))) {
      "primary"
    } else {
      "sweep"
    }
    spec <- switch(
      kind,
      primary = list(draws = 1000L, sigma = 2, noise = "heavy", shift = 0),
      sweep = list(draws = 1000L, sigma = 1, noise = "normal",
                   shift = if (slope_sd < jt_cfg$priors$slope_sd_primary) -0.12 else 0.12),
      student_refit = list(draws = 1000L, sigma = 1, noise = "normal", shift = 0.55),
      # Nothing reads a prior-only fit's ESS; fewer draws keep the sixteen
      # prior-predictive replications cheap.
      prior_predictive = list(draws = 100L, sigma = 1, noise = "normal", shift = 0))
    recorder$brm_calls[[length(recorder$brm_calls) + 1L]] <- list(
      kind = kind, response = response, outcome_key = key, family = family_name,
      sample_prior = sample_prior, slope_sd = slope_sd)
    fb_fake_brmsfit(
      args$formula, args$data, family_name, jt_uni_means(key, spec$shift),
      sigma = spec$sigma, chains = args$chains, draws_per_chain = spec$draws,
      seed = 1000L * match(key, jt_keys) + length(recorder$brm_calls),
      outcome = jt_outcomes[match(key, jt_keys)], role = kind, noise = spec$noise,
      update_draws = spec$draws, recorder = recorder)
  }, .package = "brms")
  rlang::local_bindings(
    # Stan internals a stand-in fit has no counterpart for.
    ap6_max_treedepth = function(fit) 10L,
    ap6_adapt_delta = function(fit) 0.8,
    ap6_fit_provenance = fb_fit_provenance,
    ap6_run_priorsense = fb_priorsense,
    .env = reader$prod)
  # The stand-in has a consumer of multivariate expected outcomes and
  # residual covariances. Adapt its stored draws to brms's two array APIs;
  # every covariance transformation and summary remains production code.
  original_epred <- brms::posterior_epred
  testthat::local_mocked_bindings(
    posterior_epred = function(object, resp = NULL, ...) {
      if (!isTRUE(object$fb$multivariate)) return(original_epred(object, ...))
      draws <- as.matrix(posterior::as_draws_matrix(object$fake_draws))
      ids <- if (is.null(resp)) object$fb$responses else resp
      values <- array(NA_real_, c(nrow(draws), nrow(object$data), length(ids)),
                      dimnames = list(NULL, NULL, ids))
      for (i in seq_along(ids)) {
        design <- model.matrix(object$formula$forms[[ids[i]]]$formula, object$data)
        columns <- fb_mv_b_names(ids[i], colnames(design))
        values[, , i] <- draws[, columns, drop = FALSE] %*% t(design)
      }
      values
    },
    VarCorr = function(x, summary = FALSE, ...) {
      draws <- as.matrix(posterior::as_draws_matrix(x$fake_draws))
      ids <- x$fb$responses
      values <- array(NA_real_, c(nrow(draws), length(ids), length(ids)),
                      dimnames = list(NULL, ids, ids))
      for (i in seq_along(ids)) for (j in seq_along(ids)) {
        correlation <- if (i == j) 1 else {
          key <- paste0("rescor__", ids[min(i,j)], "__", ids[max(i,j)])
          draws[, key]
        }
        values[, i, j] <- correlation * draws[, paste0("sigma_", ids[i])] *
          draws[, paste0("sigma_", ids[j])]
      }
      list(residual__ = list(cov = values))
    }, .package = "brms")
  values <- list()
  for (name in targets) values[[name]] <- read(name)
  list(values = values, recorder = recorder, read = read, prod = reader$prod)
}

# (A) the joint fit passes the gate; (B) it never reaches the ESS target.
jt_a <- jt_run(reader = jt_design)
av <- jt_a$values
jt_prod <- jt_a$prod
jt_fit <- av$joint_regression_fit$joint
jt_b <- jt_run(joint = list(draws = 60L, update_draws = 60L))
bv <- jt_b$values

# --- helpers that recompute expectations from the fit's own draws ------------

# The draws of a stand-in fit, read directly rather than through the verb under
# test, so that the expectation is independent of the production extraction.
jt_own_draws <- function(fit) posterior::as_draws_matrix(fit$fake_draws)
jt_draw <- function(fit, variable) as.numeric(jt_own_draws(fit)[, variable])
# Posterior median and the two equal-tailed type-7 interval bounds of a vector.
jt_summary <- function(x) {
  c(median = stats::median(x), unname(stats::quantile(x, jt_ci, type = 7)))
}
jt_expect_summary <- function(row, x, info) {
  expected <- jt_summary(x)
  expect_equal(unname(row$posterior_median), unname(expected[[1L]]), info = info)
  expect_equal(unname(row$lower), unname(expected[[2L]]), info = info)
  expect_equal(unname(row$upper), unname(expected[[3L]]), info = info)
}
# The AP7 interval rule, stated once here and applied to my own bounds.
jt_class <- function(x, labels) {
  bounds <- jt_summary(x)
  if (bounds[[2L]] > 0) labels[[1L]] else if (bounds[[3L]] < 0) labels[[2L]] else labels[[3L]]
}
# The one summarised row a package must carry for `...`: its median and both
# bounds recomputed from `values`, and its classification under the same rule.
jt_expect_row <- function(table, values, labels, info, ...) {
  row <- jt_pick(table, ...)
  expect_equal(nrow(row), 1L, info = info)
  jt_expect_summary(row, values, info)
  if (!is.null(labels)) {
    expect_identical(row$classification, jt_class(values, labels), info = info)
  }
  invisible(row)
}
jt_direction_labels <- c("credible positive association", "credible negative association",
                         "unresolved in direction")
# The strength differences of RQ2a: |first| - |second| within each draw.
jt_difference_labels <- c("credibly stronger for the first-named outcome",
                          "credibly weaker for the first-named outcome",
                          "unresolved strength difference")
# The comparison of the two coefficients' signs, restated from my own bounds.
jt_direction_comparison <- function(first, second) {
  sign_of <- function(x) jt_class(x, c("positive", "negative", "unresolved"))
  a <- sign_of(first)
  b <- sign_of(second)
  if (a %in% c("positive", "negative") && identical(a, b)) return("same direction")
  if (a %in% c("positive", "negative") && b %in% c("positive", "negative")) return("sign flip")
  "unresolved direction comparison"
}
jt_pick <- function(tbl, ...) {
  keep <- rep(TRUE, nrow(tbl))
  for (column in names(list(...))) keep <- keep & tbl[[column]] == list(...)[[column]]
  tbl[keep, , drop = FALSE]
}
# One motive's four outcome coefficient draw vectors, straight off the fit.
jt_motive_draws <- function(fit, motive) {
  vapply(jt_outcome_resp, function(resp) {
    jt_draw(fit, paste0("b_", resp, "_", jt_motive_col[[motive]]))
  }, numeric(posterior::ndraws(fit$fake_draws)))
}
jt_mean_asc <- function(m) (m[, "aggression"] + m[, "submission"] + m[, "conventionalism"]) / 3

# --- J1: the joint model definition ------------------------------------------

test_that("J1 the joint definition reuses the route's own regression table, formulas and priors", {
  model <- av$joint_regression_model
  # the same object, standardisation row for standardisation row
  expect_identical(model$data, jt_data)
  expect_identical(attr(model$data, "z_parameters"), attr(jt_data, "z_parameters"))

  # the four primary formulas, in the declared order, with the declared responses
  expect_equal(vapply(model$formula$forms, function(f) deparse1(f$formula), ""),
               stats::setNames(vapply(jt_model_set$formulas, deparse1, "", USE.NAMES = FALSE),
                               jt_resp))
  expect_equal(unname(vapply(model$formula$forms, function(f) all.vars(f$formula)[[1L]], "")),
               jt_responses)
  expect_true(isTRUE(model$formula$rescor))
  # the brms-ised response names, resolved from the response columns themselves
  expect_equal(unname(model$response_ids), gsub("[^[:alnum:]]", "", jt_responses))
  expect_equal(names(model$response_ids), jt_outcomes)

  priors <- as.data.frame(model$priors)
  # one prior row per response x column, at the registered width
  expect_slopes <- function(columns, sd) {
    for (resp in jt_resp) for (column in columns) {
      row <- priors[priors$class == "b" & priors$coef == column & priors$resp == resp, ]
      expect_equal(nrow(row), 1L, info = paste(resp, column))
      expect_identical(row$prior, paste0("normal(0, ", sd, ")"), info = paste(resp, column))
    }
  }
  expect_slopes(c("zm_security_z", "zm_achievement_z", "zm_power_z", "zm_prestige_z", "zm_arousal_z",
                  "age_z", "income_z"), jt_cfg$priors$slope_sd_primary)  # normal(0, .20)
  # the gender priors sit on the contrast columns of the actual design matrix
  contrasts <- grep("^gender", fb_design_columns(jt_model_set$formulas[[1L]], jt_data),
                    value = TRUE)
  expect_gt(length(contrasts), 0L)
  expect_slopes(contrasts, jt_cfg$priors$gender_sd)
  for (class in c("Intercept", "sigma")) {
    rows <- priors[priors$class == class, ]
    expect_setequal(rows$resp, jt_resp)
    expect_identical(unique(rows$prior), paste0("normal(0, ", if (identical(class, "Intercept"))
      jt_cfg$priors$intercept_sd else jt_cfg$priors$sigma_sd, ")"), info = class)
  }
  rescor <- priors[priors$class == "rescor", ]
  expect_equal(nrow(rescor), 1L)
  expect_identical(rescor$prior, as.character(jt_cfg$priors$rescor))
  expect_identical(rescor$resp, "")
})

# --- J2: the recorded joint brms call ----------------------------------------

test_that("J2 the joint fitting verb calls brms once with the definition and the smoke settings", {
  calls <- Filter(function(call) identical(call$kind, "joint"), jt_a$recorder$brm_calls)
  expect_equal(length(calls), 1L)
  call <- calls[[1L]]
  expect_identical(call$formula, av$joint_regression_model$formula)
  expect_identical(call$prior, av$joint_regression_model$priors)
  expect_identical(call$data, jt_data)
  expect_identical(attr(call$data, "z_parameters"), attr(jt_data, "z_parameters"))
  expect_identical(call$sample_prior, "no")
  expect_equal(call$chains, jt_cfg$regression$chains)
  expect_equal(call$cores, jt_cfg$regression$cores)
  expect_equal(call$warmup, jt_cfg$regression$warmup)
  expect_equal(call$iter, jt_cfg$regression$warmup + jt_cfg$regression$iter_per_chain)
  expect_equal(call$thin, 1)
  expect_equal(call$seed, jt_cfg$regression$seed)
  expect_identical(call$backend, jt_cfg$regression$backend)
  # the joint call is one call beside the four primary, eight sweep and sixteen
  # prior-predictive ones; the tail trigger stays quiet, so no Student refit
  kinds <- vapply(jt_a$recorder$brm_calls, `[[`, "", "kind")
  expect_equal(length(kinds), 1L + 4L + 8L + 16L)
  for (kind in names(c(joint = 1L, primary = 4L, sweep = 8L, prior_predictive = 16L))) {
    expect_equal(sum(kinds == kind),
                 c(joint = 1L, primary = 4L, sweep = 8L, prior_predictive = 16L)[[kind]],
                 info = kind)
  }
  expect_equal(unname(attr(av$regression_student_t_robustness, "heavy_tail_triggers")),
               rep(FALSE, 4L))
})

# --- J3: the shared validity gate on the joint fit ---------------------------

test_that("J3 the shared gate covers all four equations and passes the joint fit", {
  expect_equal(names(av$joint_regression_fit), "joint")
  expect_s3_class(jt_fit, "brmsfit")
  expect_identical(attr(jt_fit, "gate_status"), "ok")
  diagnostics <- attr(jt_fit, "diagnostics")
  expect_true(diagnostics$ok)
  expect_true(diagnostics$ess_ok && diagnostics$rhat_ok && diagnostics$divergences_ok &&
                diagnostics$treedepth_ok && diagnostics$bfmi_ok)
  expect_equal(diagnostics$ess_target, jt_cfg$regression$ess_target)
  expect_gte(diagnostics$ess_bulk_min, jt_cfg$regression$ess_target)
  expect_equal(attr(jt_fit, "n_obs"), nrow(jt_data))
  expect_equal(nrow(attr(jt_fit, "retry_log")), 0L)
  expect_equal(nrow(attr(jt_fit, "ess_log")), 1L)

  # the gate reads all four responses off the mvbrmsformula, not the first one
  expect_equal(jt_prod$ap6_gate_resp(jt_fit), jt_resp)
  b_vars <- jt_prod$ap6_focal_b_vars(jt_fit)
  expect_equal(length(b_vars), 4L * length(jt_vars))
  for (resp in jt_resp) {
    expect_equal(sum(startsWith(b_vars, paste0("b_", resp, "_"))), length(jt_vars), info = resp)
  }
  expect_identical(jt_prod$ap6_focal_family(jt_fit), "gaussian")
})

# --- J4: the gate on a joint fit that never reaches the ESS target -----------

test_that("J4 a joint fit that never reaches the ESS target is not interpretable", {
  fit <- bv$joint_regression_fit$joint
  expect_equal(names(bv$joint_regression_fit), "joint")
  expect_identical(attr(fit, "gate_status"), "not_interpretable")
  expect_false(attr(fit, "diagnostics")$ok)
  expect_false(attr(fit, "diagnostics")$ess_ok)
  # the configured number of doublings, each recorded once
  doublings <- jt_cfg$regression$validity_gate$max_ess_doublings
  retries <- attr(fit, "retry_log")
  expect_equal(nrow(retries), doublings)
  expect_true(all(grepl("^ESS shortfall", retries$reason)))
  expect_false(any(retries$ok))
  expect_equal(nrow(attr(fit, "ess_log")), doublings + 1L)
  expect_equal(attr(fit, "ess_log")$iter_per_chain,
               as.integer(jt_cfg$regression$iter_per_chain * 2^(0:doublings)))
  expect_equal(attr(fit, "iter_used"),
               as.integer(jt_cfg$regression$iter_per_chain * 2^doublings))
  updates <- jt_b$recorder$update_calls
  expect_equal(length(updates), doublings)
  expect_true(all(vapply(updates, `[[`, "", "outcome") == "joint"))
  expect_true(all(vapply(updates, `[[`, 0, "warmup") == jt_cfg$regression$warmup))
  expect_equal(attr(fit, "n_obs"), nrow(jt_data))
})

# --- J5: the AP7 posterior extraction ----------------------------------------

test_that("J5 the joint posterior is the fit's own draws, paired by draw identity", {
  posterior_result <- av$joint_posterior
  draws <- posterior::as_draws_df(jt_fit)
  n_draws <- posterior::ndraws(jt_fit$fake_draws)
  expect_named(posterior_result$coefficients,
               c(".draw", "motive", "aggression", "submission", "conventionalism", "sdo_d"))
  expect_equal(nrow(posterior_result$coefficients), 5L * n_draws)
  expect_equal(unique(posterior_result$coefficients$motive), names(jt_motive_col))
  # every cell of every motive, against the parameter the AP7 map names
  for (motive in names(jt_motive_col)) {
    rows <- jt_pick(posterior_result$coefficients, motive = motive)
    expect_identical(rows$.draw, draws$.draw, info = motive)
    for (outcome in names(jt_outcome_resp)) {
      expect_identical(
        rows[[outcome]],
        draws[[paste0("b_", jt_outcome_resp[[outcome]], "_", jt_motive_col[[motive]])]],
        info = paste(motive, outcome))
    }
  }
  expect_named(posterior_result$residual_correlations, c(".draw", names(jt_pair_rescor)))
  expect_equal(nrow(posterior_result$residual_correlations), n_draws)
  expect_identical(posterior_result$residual_correlations$.draw, draws$.draw)
  for (pair in names(jt_pair_rescor)) {
    expect_identical(posterior_result$residual_correlations[[pair]],
                     draws[[jt_pair_rescor[[pair]]]], info = pair)
  }
  expect_true(posterior_result$validity$fit_valid)
  expect_identical(posterior_result$validity$gate_status, "ok")
  expect_true(is.na(posterior_result$validity$note))
})

# --- J6: the total–SDO-D correlation is part of the seven paired results -------

test_that("J6 the joint comparison retains seven paired results including the total", {
  comparison <- av$joint_correlation_changes
  expect_equal(nrow(comparison$summaries), 7L)
  expect_equal(sum(comparison$summaries$outcome_1 == "asc_total"), 1L)
  expect_true(all(comparison$summaries$interpretable))
  expect_true(all(is.finite(comparison$summaries$residual_median)))
})

# --- J7: the within-ASC package ----------------------------------------------

test_that("J7 the within-ASC package carries three ordered contrasts per motive", {
  package <- av$joint_within_asc
  expect_equal(nrow(package$direction), 15L)
  expect_equal(nrow(package$differences), 15L)
  expect_equal(nrow(package$residuals), 3L)
  contrasts <- list(
    `|Aggression| - |submission|` = c("aggression", "submission"),
    `|Aggression| - |conventionalism|` = c("aggression", "conventionalism"),
    `|Submission| - |conventionalism|` = c("submission", "conventionalism"))
  for (motive in names(jt_motive_col)) {
    m <- jt_motive_draws(jt_fit, motive)
    for (outcome in c("aggression", "submission", "conventionalism")) {
      jt_expect_row(package$direction, m[, outcome], jt_direction_labels,
                    paste(motive, outcome), motive = motive, outcome = outcome)
    }
    for (label in names(contrasts)) {
      pair <- contrasts[[label]]
      # the label carries the subtraction order of the absolute values: left minus right
      row <- jt_expect_row(package$differences, abs(m[, pair[[1L]]]) - abs(m[, pair[[2L]]]),
                           jt_difference_labels, paste(motive, label),
                           motive = motive, contrast = label)
      expect_identical(c(row$first_outcome, row$second_outcome), pair, info = paste(motive, label))
      expect_identical(row$direction_comparison, jt_direction_comparison(m[, pair[[1L]]], m[, pair[[2L]]]),
                       info = paste(motive, label))
    }
  }
  # Power was designed to separate: its aggression coefficient is the
  # strongest of the three. Prestige's aggression and submission coefficients
  # have opposite credible signs but nearly the same size: a sign flip without
  # a strength difference, which a signed contrast would have called credible.
  power <- jt_pick(package$differences, motive = "zm_power", contrast = "|Aggression| - |submission|")
  expect_identical(power$classification, "credibly stronger for the first-named outcome")
  prestige <- jt_pick(package$differences, motive = "zm_prestige", contrast = "|Aggression| - |submission|")
  expect_identical(prestige$classification, "unresolved strength difference")
  expect_identical(prestige$direction_comparison, "sign flip")
  expect_true(any(package$differences$classification == "unresolved strength difference"))
  # the descriptive residual table: three pairs, no classification
  expect_false("classification" %in% names(package$residuals))
  for (pair in names(jt_pair_rescor)[1:3]) {
    jt_expect_row(package$residuals, jt_draw(jt_fit, jt_pair_rescor[[pair]]), NULL,
                  pair, pair = pair)
  }
  expect_setequal(package$residuals$pair, names(jt_pair_rescor)[1:3])
})

# --- J8: the ASC-component vs SDO-D package ----------------------------------

test_that("J8 each ASC component is compared with SDO-D inside the paired draw", {
  package <- av$joint_asc_components_sdo
  expect_equal(nrow(package$direction), 20L)
  expect_equal(nrow(package$differences), 15L)
  expect_equal(nrow(package$residuals), 3L)
  labels <- c(aggression = "|Aggression| - |SDO-D|", submission = "|Submission| - |SDO-D|",
              conventionalism = "|Conventionalism| - |SDO-D|")
  for (motive in names(jt_motive_col)) {
    m <- jt_motive_draws(jt_fit, motive)
    for (outcome in names(jt_outcome_resp)) {
      jt_expect_row(package$direction, m[, outcome], jt_direction_labels,
                    paste(motive, outcome), motive = motive, outcome = outcome)
    }
    for (component in names(labels)) {
      row <- jt_expect_row(package$differences, abs(m[, component]) - abs(m[, "sdo_d"]),
                           jt_difference_labels, paste(motive, component),
                           motive = motive, contrast = labels[[component]])
      expect_identical(row$direction_comparison, jt_direction_comparison(m[, component], m[, "sdo_d"]),
                       info = paste(motive, component))
    }
  }
  # Intimacy's SDO-D coefficient was designed far above its aggression one:
  # the weaker association of the first-named facet occurs too.
  intimacy <- jt_pick(package$differences, motive = "zm_security", contrast = "|Aggression| - |SDO-D|")
  expect_identical(intimacy$classification, "credibly weaker for the first-named outcome")
  expect_false("classification" %in% names(package$residuals))
  for (pair in names(jt_pair_rescor)[4:6]) {
    jt_expect_row(package$residuals, jt_draw(jt_fit, jt_pair_rescor[[pair]]), NULL,
                  pair, pair = pair)
  }
  expect_setequal(package$residuals$pair, names(jt_pair_rescor)[4:6])
})

# --- J9: the invalid joint fit withholds only the classification -------------

test_that("J9 an invalid joint fit loses every classification and keeps every number", {
  for (name in c("joint_within_asc", "joint_asc_components_sdo")) {
    invalid <- bv[[name]]
    expect_false(invalid$validity$fit_valid, info = name)
    expect_identical(invalid$validity$gate_status, "not_interpretable", info = name)
    expect_identical(invalid$validity$note, "Fit is not interpretable.", info = name)
    for (part in c("direction", "differences", "residuals")) {
      table <- invalid[[part]]
      expect_equal(nrow(table), nrow(av[[name]][[part]]), info = paste(name, part))
      for (column in intersect(c("classification", "direction_comparison"), names(table))) {
        expect_true(all(is.na(table[[column]])), info = paste(name, part, column))
        expect_false(any(is.na(av[[name]][[part]][[column]])), info = paste(name, part, column))
      }
      # every median and both bounds are still present and still the fit's own
      expect_false(any(is.na(table$posterior_median)), info = paste(name, part))
      expect_false(any(is.na(table$lower)), info = paste(name, part))
      expect_false(any(is.na(table$upper)), info = paste(name, part))
      expect_true(all(table$lower <= table$posterior_median), info = paste(name, part))
      expect_true(all(table$posterior_median <= table$upper), info = paste(name, part))
    }
  }
  # the medians are the failed fit's own draws, not the passing fit's
  m <- jt_motive_draws(bv$joint_regression_fit$joint, "zm_power")
  jt_expect_row(bv$joint_within_asc$differences, abs(m[, "aggression"]) - abs(m[, "submission"]), NULL,
                "Power |aggression| - |submission|",
                motive = "zm_power", contrast = "|Aggression| - |submission|")
})

# --- J10: a joint fit the sampler never returns ------------------------------

test_that("J10 a failing joint sampler stays unavailable through the actual route", {
  failed <- jt_run(joint = list(fail = TRUE))$values
  unavailable <- failed$joint_regression_fit
  expect_s3_class(unavailable$joint, "error")
  expect_identical(conditionMessage(unavailable$joint),
                   "stand-in sampler: the joint model did not fit.")
  result <- failed$joint_posterior
  expect_false(result$validity$fit_valid)
  expect_identical(result$validity$gate_status, "unavailable")
  expect_identical(result$validity$note, "stand-in sampler: the joint model did not fit.")
  expect_equal(nrow(result$coefficients), 0L)
  expect_named(result$coefficients, names(av$joint_posterior$coefficients))
  expect_equal(nrow(result$residual_correlations), 0L)
  expect_named(result$residual_correlations, names(av$joint_posterior$residual_correlations))

  empty <- function(verb, input) jt_prod[[verb]](input, interval_level = jt_level)
  for (verb in c("ap8_result_overall_asc_sdo_directions",
                 "ap8_result_overall_asc_sdo_coefficient_differences",
                 "extract_joint_within_asc_directions",
                 "extract_joint_within_asc_coefficient_differences",
                 "extract_joint_asc_components_sdo_directions",
                 "extract_joint_asc_components_sdo_coefficient_differences")) {
    table <- empty(verb, result$coefficients)
    expect_equal(nrow(table), 0L, info = verb)
    expect_true("classification" %in% names(table), info = verb)
  }
  for (verb in c("extract_joint_within_asc_residual_correlations",
                 "extract_joint_asc_components_sdo_residual_correlations")) {
    expect_equal(nrow(empty(verb, result$residual_correlations)), 0L, info = verb)
  }
  # The one summary without a grouping: dplyr returns one all-missing row, and
  # the withholding verb then removes its classification.
  residual <- empty("ap8_result_overall_asc_sdo_residual_difference",
                    result$residual_correlations)
  expect_equal(nrow(residual), 1L)
  expect_true(all(is.na(residual$posterior_median)))
  withheld <- jt_prod$withhold_classification_when_fit_invalid(
    list(residuals = residual), result$validity)
  expect_true(is.na(withheld$residuals$classification))

  # Every report target that reads the joint fit completes, preserving a typed
  # schema without presenting missing posterior quantities as successful values.
  expect_false(any(failed$supplement_software$provenance$role == "multivariate"))
  rq3 <- failed$report_joint
  expect_false(rq3$gate_passed)
  expect_identical(rq3$gate_status, "unavailable")
  expect_equal(nrow(rq3$credible_directions), 0L)
  expect_equal(nrow(rq3$credible_differences), 0L)
  expect_false(rq3$all_residuals_positive)
})

# --- J11: the Table 3 decisions ----------------------------------------------

# The configured sign of one prediction cell, and the verdict rule applied to my
# own interval — the rule is restated here rather than read from the result.
jt_sign <- function(key, predictor) {
  value <- jt_cfg$predictions$table[[key]][[predictor]]
  if (is.null(value) || is.na(value)) "" else as.character(value)
}
jt_verdict <- function(estimate, q_lo, q_hi, sign, valid) {
  if (!isTRUE(valid)) return(NA_character_)
  credible <- q_lo > 0 || q_hi < 0
  observed <- if (estimate > 0) "+" else if (estimate < 0) "-" else ""
  if (!sign %in% c("+", "-")) return(if (credible) "exploratory_credible" else "exploratory_not_credible")
  if (credible && identical(observed, sign)) return("confirmed")
  if (credible) return("disconfirmed")
  NA_character_
}

test_that("J11 the Table 3 decisions carry each primary fit's own cell and verdict", {
  decisions <- av$regression_prediction_decisions
  expect_equal(nrow(decisions), 20L)
  # the identity chain through the route's own objects: model name -> key -> response
  expect_equal(jt_model_set$outcome_keys[["authoritarian_aggression"]], "asc_agg")
  expect_equal(jt_model_set$outcome_keys[["authoritarian_submission"]], "asc_sub")
  expect_equal(jt_model_set$outcome_keys[["conventionalism"]], "asc_conv")
  expect_equal(jt_model_set$outcome_keys[["sdo_d_dominance"]], "sdo_dom")
  expect_equal(stats::setNames(jt_responses, jt_keys)[c("asc_agg", "asc_sub", "asc_conv", "sdo_dom")],
               c(asc_agg = "asc_agg_z", asc_sub = "asc_sub_z", asc_conv = "asc_conv_z",
                 sdo_dom = "sdo_dom_z"))
  for (i in seq_len(nrow(decisions))) {
    key <- decisions$outcome_key[[i]]
    expect_identical(decisions$outcome[[i]], jt_outcomes[match(key, jt_keys)])
    expect_identical(decisions$response_column[[i]], jt_responses[match(key, jt_keys)])
    variable <- paste0("b_", jt_column_of[[decisions$predictor[[i]]]])
    expect_equal(decisions$estimate[[i]], jt_uni_mean(key, variable),
                 info = paste(key, decisions$predictor[[i]]))
    expect_identical(decisions$predicted_sign[[i]], jt_sign(key, decisions$predictor[[i]]))
    expect_identical(decisions$verdict[[i]],
                     jt_verdict(decisions$estimate[[i]], decisions$q_lo[[i]],
                                decisions$q_hi[[i]], decisions$predicted_sign[[i]],
                                decisions$fit_valid[[i]]),
                     info = paste(key, decisions$predictor[[i]]))
  }
  # all twenty cells distinct, so a swapped identity moves every one of them
  expect_equal(anyDuplicated(decisions$estimate), 0L)
  # every verdict label the rule can produce occurs, and so does the no-verdict case
  expect_setequal(unique(stats::na.omit(decisions$verdict)),
                  c("confirmed", "disconfirmed",
                    "exploratory_credible", "exploratory_not_credible"))
  expect_true(anyNA(decisions$verdict))
})

test_that("J11 only the primary Gaussian rows at the registered width decide Table 3", {
  summaries <- av$regression_coefficient_summaries
  decisions <- av$regression_prediction_decisions
  non_primary <- !(summaries$role == "primary" &
                     summaries$family == jt_cfg$regression$family &
                     dplyr::near(summaries$slope_sd, jt_cfg$priors$slope_sd_primary))
  expect_gt(sum(non_primary), 0L)
  perturbed <- summaries
  for (column in c("estimate", "q_lo", "q_hi")) {
    perturbed[[column]][non_primary] <- perturbed[[column]][non_primary] + 7.5
  }
  expect_equal(jt_prod$extract_regression_prediction_decisions(perturbed, jt_cfg),
               decisions)

  # A swapped outcome key is NOT refused: the verb joins by the key, so the two
  # outcomes exchange their cells and the table changes visibly.
  swapped <- summaries
  exchange <- c(asc_agg = "asc_sub", asc_sub = "asc_agg")
  hit <- swapped$outcome_key %in% names(exchange)
  swapped$outcome_key[hit] <- unname(exchange[swapped$outcome_key[hit]])
  after <- jt_prod$extract_regression_prediction_decisions(swapped, jt_cfg)
  expect_equal(nrow(after), 20L)
  expect_false(isTRUE(all.equal(after$estimate, decisions$estimate)))
  expect_equal(jt_pick(after, outcome_key = "asc_sub", predictor = "zm_power")$estimate,
               jt_uni_mean("asc_agg", "b_zm_power_z"))
  expect_identical(jt_pick(after, outcome_key = "asc_sub")$outcome[[1L]],
                   "authoritarian_aggression")
  # the rows of the untouched outcomes keep their verdicts
  expect_equal(jt_pick(after, outcome_key = "asc_conv")$verdict,
               jt_pick(decisions, outcome_key = "asc_conv")$verdict)

  # An invalid primary gate withholds the verdict of that outcome alone.
  invalid <- summaries
  failing <- invalid$role == "primary" & invalid$outcome_key == "sdo_dom"
  invalid$fit_valid[failing] <- FALSE
  invalid$gate_status[failing] <- "not_interpretable"
  withheld <- jt_prod$extract_regression_prediction_decisions(invalid, jt_cfg)
  failed_rows <- jt_pick(withheld, outcome_key = "sdo_dom")
  expect_equal(nrow(failed_rows), 5L)
  expect_true(all(is.na(failed_rows$verdict)))
  expect_true(all(is.na(failed_rows$credible)))
  # the interval stays visible; only the verdict is withheld
  expect_false(any(is.na(failed_rows$credible_raw)))
  expect_equal(failed_rows$estimate, jt_pick(decisions, outcome_key = "sdo_dom")$estimate)
  expect_equal(withheld$verdict[withheld$outcome_key != "sdo_dom"],
               decisions$verdict[decisions$outcome_key != "sdo_dom"])
})

# --- J12: the joint provenance row -------------------------------------------

test_that("J12 the joint provenance row carries the stored gate", {
  provenance <- av$supplement_software$provenance
  # twelve separate rows (four primary, eight sweep; no Student refit was
  # triggered) plus the one joint row
  separate <- provenance[provenance$role %in% c("primary", "sweep"), ]
  expect_equal(nrow(separate), 12L)
  expect_setequal(unique(separate$outcome), jt_keys)
  joint_row <- provenance[provenance$role == "multivariate", ]
  expect_equal(nrow(joint_row), 1L)
  expect_identical(joint_row$outcome, "multivariate")
  expect_identical(joint_row$label, jt_prod$zm_fit_label(jt_cfg, "multivariate"))
  expect_identical(joint_row$gate_status, "ok")
  expect_equal(joint_row$nobs, nrow(jt_data))
  expect_equal(joint_row$chains, jt_cfg$regression$chains)
  expect_identical(joint_row$formula, attr(jt_fit, "formula_text"))
  expect_identical(bv$supplement_software$provenance$gate_status[bv$supplement_software$provenance$role == "multivariate"],
                   "not_interpretable")
})

# --- J13: the RQ2 report target ----------------------------------------------

test_that("J13 the RQ2 report target holds the three packages in their three views", {
  rq3 <- av$report_joint
  expect_true(rq3$gate_passed)
  # 5 motives x (three ASC components, overall ASC, SDO-D)
  expect_equal(nrow(rq3$direction), 20L)
  # 5 motives x (one overall, three within-ASC, three component-SDO-D contrasts)
  expect_equal(nrow(rq3$differences), 30L)
  expect_equal(nrow(rq3$residuals), 7L)
  # the motives by key, in the codebook's order
  expect_identical(unique(rq3$direction$motive),
                   c("zm_security", "zm_arousal", "zm_power", "zm_prestige", "zm_achievement"))
  credible <- rq3$direction[grepl("^credible ", rq3$direction$classification), ]
  expect_identical(unique(rq3$direction$motive_key),
                   c("zm_security", "zm_arousal", "zm_power", "zm_prestige", "zm_achievement"))
  expect_identical(paste(rq3$credible_directions$motive_key, rq3$credible_directions$outcome),
                   paste(credible$motive_key, credible$outcome))
  expect_identical(rq3$credible_directions$sign,
                   unname(ifelse(credible$lower > 0, "positive", "negative")))
  # a credible strength difference names the outcome with the stronger association
  stronger <- rq3$differences[!grepl("^unresolved", rq3$differences$classification), ]
  expect_identical(rq3$credible_differences$stronger,
                   unname(ifelse(stronger$lower > 0, "first", "second")))
  expect_true(all(c("first", "second") %in% rq3$credible_differences$stronger))
  expect_identical(rq3$all_residuals_positive, all(rq3$residuals$lower > 0))
  expect_false(bv$report_joint$gate_passed)
  expect_equal(nrow(bv$report_joint$credible_differences), 0L)
})
