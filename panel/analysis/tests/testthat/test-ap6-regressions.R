# R/ap6_regressions.R: brms regressions (AP6), prior sweep, validity gate, predictive-check draws, provenance

local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) {
    parent <- dirname(dir)
    if (identical(parent, dir)) stop("project root not found from ", getwd())
    dir <- parent
  }
  for (f in c("config.R", "ap3_imputation_validity.R", "ap6_regressions.R")) {
    source(file.path(dir, "R", f), local = FALSE)
  }
})

root <- zm_root()
plan_yaml <- yaml::read_yaml(file.path(root, "config", "analysis_plan.yaml"))
analysis_plan <- zm_config(profile = "smoke", path = file.path(root, "config", "analysis_plan.yaml"))
# small fits for the tests: 2 chains, 500 warmup + 500 post-warmup draws (the
# smallest seed-deterministic fit that passes the R-hat gate without a retry);
# the ESS target is lowered so that the gate (which applies in every
# profile) is reachable with 1,000 draws, while every other threshold of the
# gate stays configured
analysis_plan$regression$chains <- 2L
analysis_plan$regression$cores <- 2L
analysis_plan$regression$warmup <- 500L
analysis_plan$regression$iter_per_chain <- 500L
analysis_plan$regression$ess_target <- 100
reg <- analysis_plan$regression
gate_cfg <- reg$validity_gate
primary_sd <- analysis_plan$priors$slope_sd_primary
sweep <- analysis_plan$priors$slope_sd_sweep
outcomes <- reg$outcomes
motives <- reg$motives
student_cfg <- analysis_plan$sensitivity$student_t

# Stan executables of the tests go to the scratch directory, not the project
withr::local_options(list(cmdstanr_write_stan_file_dir = file.path(tempdir(), "zm_stan_tests")), .local_envir = teardown_env())
dir.create(file.path(tempdir(), "zm_stan_tests"), showWarnings = FALSE, recursive = TRUE)

cmdstan_ok <- tryCatch({
  zm_setup()
  nzchar(cmdstanr::cmdstan_version())
}, error = function(e) FALSE)

# 200-row standardised analysis frame with the exact right-hand side of the config
make_frame <- function(n = 200, seed = 42) {
  set.seed(seed)
  d <- data.frame(
    zm_security_z = rnorm(n), zm_achievement_z = rnorm(n), zm_power_z = rnorm(n),
    zm_prestige_z = rnorm(n), zm_arousal_z = rnorm(n), age_z = rnorm(n),
    gender = factor(sample(c("male", "female"), n, replace = TRUE), levels = c("male", "female")),
    income_z = rnorm(n)
  )
  female <- as.numeric(d$gender == "female")
  z <- function(x) as.numeric(scale(x))
  d$asc_agg_z <- z(0.30 * d$zm_power_z - 0.20 * d$zm_achievement_z + 0.25 * female + rnorm(n, sd = 0.9))
  d$asc_sub_z <- z(-0.20 * d$zm_arousal_z - 0.15 * d$zm_achievement_z + rnorm(n, sd = 0.95))
  d$asc_conv_z <- z(0.20 * d$zm_security_z - 0.20 * d$zm_arousal_z + 0.15 * d$age_z + rnorm(n, sd = 0.9))
  d$sdo_dom_z <- z(0.25 * d$zm_power_z + 0.15 * d$zm_prestige_z + 0.35 * female + rnorm(n, sd = 0.9))
  tibble::as_tibble(d)
}
d <- make_frame()

# The model set of the pipeline, restricted to the authoritarian-aggression
# regression for the sampled fits below.
model_set <- define_regression_model_set(analysis_plan)
aggression_set <- model_set
aggression_set$formulas <- model_set$formulas["authoritarian_aggression"]
aggression_formula <- aggression_set$formulas[["authoritarian_aggression"]]

test_that("the sweep is 0.10 / 0.20 / 0.40 with labels and no SD 2", {
  expect_equal(sort(sweep), c(0.1, 0.2, 0.4))
  expect_false(any(dplyr::near(sweep, 2)))
  expect_false(any(sweep > 1))
  expect_true(primary_sd %in% sweep)
  expect_setequal(names(analysis_plan$priors$sweep_labels), zm_sweep_key(sweep))
  expect_equal(ap6_sweep_label(sweep, analysis_plan), unname(unlist(analysis_plan$priors$sweep_labels[zm_sweep_key(sweep)])))
  expect_equal(ap6_sweep_label(primary_sd, analysis_plan), "primary")
  # a sweep without labels is refused where the plan is loaded (zm_config()),
  # so ap6_sweep_label() is a plain reader (test-config.R)
})

test_that("the model set builds <outcome>_z ~ predictors from the config", {
  f <- aggression_formula
  expect_s3_class(f, "formula")
  expect_equal(deparse1(f), deparse1(stats::as.formula(paste("asc_agg_z ~", reg$predictors))))
  expect_equal(all.vars(f)[1], "asc_agg_z")
  expect_true(all(paste0(motives, "_z") %in% all.vars(f)))
  expect_true("gender" %in% all.vars(f))
  expect_equal(ap6_formula_text(f), deparse1(f))
  expect_equal(ap6_focal_resp(f), "")
  expect_equal(ap6_focal_pattern(""), "^(b|bsp)_")
})

test_that("ap6_clean_terms() strips b_, response prefixes and _z, and classifies terms", {
  vars <- c("b_Intercept", "b_zm_security_z", "b_age_z", "b_genderfemale", "b_income_z", "sigma", "nu")
  ct <- ap6_clean_terms(vars, reg)
  expect_equal(ct$term, c("Intercept", "zm_security", "age", "genderfemale", "income", "sigma", "nu"))
  expect_equal(ct$term_type, c("intercept", "predictor", "covariate", "covariate", "covariate", "sigma", "auxiliary"))
  mv <- c("b_ascaggz_Intercept", "b_ascaggz_zm_security_z", "b_ascaggz_age_z", "b_ascaggz_income_z",
          "b_ascaggz_genderfemale", "sigma_ascaggz", "b_zmsecurityz_Intercept", "sigma_zmsecurityz")
  cm <- ap6_clean_terms(mv, reg, resp = "ascaggz")
  expect_equal(cm$term[1:6], c("Intercept", "zm_security", "age", "income", "genderfemale", "sigma"))
  expect_equal(cm$term_type[1:6], c("intercept", "predictor", "covariate", "covariate", "covariate", "sigma"))
  expect_equal(cm$term_type[7:8], c("other", "other"))
})

test_that("ap6_ci_probs(), ap6_reg_with_seed() and ap6_gate_settings() behave", {
  expect_equal(ap6_ci_probs(reg$ci_level), c((1 - reg$ci_level) / 2, 1 - (1 - reg$ci_level) / 2))
  set.seed(99); before <- runif(1)
  set.seed(99); a <- ap6_reg_with_seed(5, runif(1)); after <- runif(1)
  expect_equal(before, after)
  expect_equal(a, ap6_reg_with_seed(5, runif(1)))
  g <- ap6_gate_settings(reg)
  expect_equal(g$ess_target, reg$ess_target)
  expect_equal(g$rhat_max, gate_cfg$rhat_max)
  expect_equal(g$divergences_max, gate_cfg$divergences_max)
  expect_equal(g$treedepth_hits_max, gate_cfg$treedepth_hits_max)
  expect_equal(g$bfmi_min, gate_cfg$bfmi_min)
  expect_equal(g$adapt_delta_retry, gate_cfg$adapt_delta_retry)
  expect_equal(g$max_treedepth_retry, gate_cfg$max_treedepth_retry)
  # An incomplete gate block is refused where the plan is loaded (zm_config()),
  # so ap6_gate_settings() is a plain reader (test-config.R).
  # BFMI: a constant energy trace has no information; an alternating one has BFMI 4 * n / (n - 1) ... > 0
  nuts <- data.frame(Chain = rep(1:2, each = 4), Iteration = rep(1:4, 2), Parameter = "energy__",
                     Value = c(1, 2, 1, 2, 5, 5, 5, 5))
  b <- ap6_bfmi(nuts)
  expect_length(b, 2)
  expect_equal(b[1], sum(diff(c(1, 2, 1, 2))^2) / sum((c(1, 2, 1, 2) - 1.5)^2))
  expect_true(is.na(b[2]))
})

test_that("nu is fixed by analysis_plan$sensitivity$student_t$nu_fixed", {
  expect_equal(student_cfg$nu_fixed, 4)
  # the matched scale prior: half-normal(1/sqrt(2)) on the scale induces half-normal(1) on the residual SD at nu = 4
  expect_equal(student_cfg$sigma_scale_prior_sd * sqrt(student_cfg$nu_fixed / (student_cfg$nu_fixed - 2)),
               analysis_plan$priors$sigma_sd)
})

# ---- one real fit (smoke profile, 2 chains, 500 + 500) -----------------------------------------
skip_if_not(cmdstan_ok, "CmdStan not available")

raw_fit <- fit_primary_regressions(d, aggression_set, analysis_plan)$authoritarian_aggression
gate_one <- function(fit, analysis_plan) {
  apply_validity_gate(list(authoritarian_aggression = fit), analysis_plan)$authoritarian_aggression
}
fit <- gate_one(raw_fit, analysis_plan)
# The same seed gives different draws on different platforms, so the fit may
# pass the gate at once or after a retry; the retry log names the warm-up used.
retried <- nrow(attr(fit, "retry_log")) > 0L
warmup_expected <- if (retried) utils::tail(attr(fit, "retry_log")$warmup, 1L) else reg$warmup

test_that("the gated primary fit carries the configured sampling settings and the gate attributes", {
  expect_s3_class(fit, "brmsfit")
  expect_equal(fit$backend, "cmdstanr")
  expect_equal(fit$fit@sim$chains, reg$chains)
  expect_equal(fit$fit@sim$warmup, warmup_expected)
  expect_equal(fit$fit@sim$iter, warmup_expected + reg$iter_per_chain)
  expect_equal(attr(fit, "iter_used"), reg$iter_per_chain)
  expect_equal(attr(fit, "warmup_used"), warmup_expected)
  expect_equal(attr(fit, "outcome"), "authoritarian_aggression")
  expect_equal(attr(fit, "slope_sd"), primary_sd)
  expect_equal(attr(fit, "family_name"), "gaussian")
  expect_equal(attr(fit, "sample_prior"), "no")
  expect_equal(attr(fit, "ci_level"), reg$ci_level)
  expect_true(is.na(attr(fit, "nu_fixed")))
  expect_equal(attr(fit, "gate_status"), if (retried) "retried_ok" else "ok")
  expect_equal(attr(fit, "n_obs"), nrow(d))
  expect_named(attr(fit, "retry_log"), c("step", "reason", "warmup", "iter_per_chain", "adapt_delta", "max_treedepth", "ok"))
  expect_equal(nrow(attr(fit, "ess_log")), 1)
  expect_true(attr(fit, "ess_log")$ok)
  expect_equal(deparse1(stats::formula(fit)$formula), deparse1(aggression_formula))
  expect_equal(attr(fit, "formula_text"), deparse1(aggression_formula))
  expect_equal(ap6_focal_resp(fit), "")
  expect_setequal(ap6_focal_b_vars(fit), grep("^b_", posterior::variables(fit), value = TRUE))
  expect_equal(ap6_sigma_var(fit), "sigma")
  expect_type(ap6_ess_check(fit, analysis_plan)$ok, "logical")
  expect_equal(ap6_ess_check(fit, analysis_plan)$target, reg$ess_target)
  # the primary width and the fixed gender width stand in the prior as literals
  priors <- as.data.frame(fit$prior)
  expect_equal(priors$prior[priors$class == "b" & priors$coef == ""],
               paste0("normal(0, ", primary_sd, ")"))
  expect_equal(priors$prior[priors$coef == "genderfemale"],
               paste0("normal(0, ", analysis_plan$priors$gender_sd, ")"))
})

test_that("ap6_fit_diagnostics() reports the full validity gate: ESS, R-hat, divergences, tree depth, BFMI, MCSE", {
  diag <- ap6_fit_diagnostics(fit, analysis_plan)
  expect_named(diag, c("ok", "ess_ok", "rhat_ok", "divergences_ok", "treedepth_ok", "bfmi_ok",
                       "ess_bulk_min", "ess_tail_min", "rhat_max", "n_divergent", "n_treedepth_hits",
                       "bfmi_min", "mcse_median_max", "mcse_q_max", "ess_target", "rhat_limit",
                       "divergences_max", "treedepth_hits_max", "bfmi_limit", "max_treedepth_used", "gate_note"))
  expect_equal(nrow(diag), 1)
  expect_identical(diag, attr(fit, "diagnostics"))
  expect_equal(diag$ess_target, reg$ess_target)
  expect_equal(diag$rhat_limit, gate_cfg$rhat_max)
  expect_equal(diag$divergences_max, gate_cfg$divergences_max)
  expect_equal(diag$treedepth_hits_max, gate_cfg$treedepth_hits_max)
  expect_equal(diag$bfmi_limit, gate_cfg$bfmi_min)
  expect_equal(diag$n_divergent, 0L)
  expect_type(diag$n_treedepth_hits, "integer")
  expect_equal(diag$n_treedepth_hits, 0L)
  expect_true(is.finite(diag$bfmi_min) && diag$bfmi_min > 0)
  expect_true(is.finite(diag$mcse_median_max) && diag$mcse_median_max > 0)
  expect_true(is.finite(diag$mcse_q_max) && diag$mcse_q_max > 0)
  expect_true(is.finite(diag$rhat_max))
  expect_true(diag$ess_bulk_min >= reg$ess_target && diag$ess_tail_min >= reg$ess_target)
  expect_true(diag$ok)
  expect_true(all(unlist(diag[, c("ess_ok", "rhat_ok", "divergences_ok", "treedepth_ok", "bfmi_ok")])))
  expect_equal(diag$gate_note, "ok")
  expect_equal(diag$max_treedepth_used, ap6_max_treedepth(fit))
  expect_true(is.finite(ap6_adapt_delta(fit)))
  # the MCSE maxima are the maxima over the b_ parameters
  draws <- posterior::subset_draws(posterior::as_draws_array(fit), variable = ap6_focal_b_vars(fit))
  probs <- ap6_ci_probs(reg$ci_level)
  s <- posterior::summarise_draws(draws, mcse_median = posterior::mcse_median,
                                  ~posterior::mcse_quantile(.x, probs = probs))
  expect_equal(diag$mcse_median_max, max(s$mcse_median))
  expect_equal(diag$mcse_q_max, max(unlist(s[, grep("^mcse_q", names(s))])))
  # a stricter gate fails on the same fit and says why
  strict <- analysis_plan
  strict$regression$ess_target <- 1e6
  strict$regression$validity_gate$bfmi_min <- 10
  ds <- ap6_fit_diagnostics(fit, strict)
  expect_false(ds$ok)
  expect_false(ds$ess_ok)
  expect_false(ds$bfmi_ok)
  expect_true(ds$rhat_ok && ds$divergences_ok && ds$treedepth_ok)
  expect_match(ds$gate_note, "ESS shortfall")
  expect_match(ds$gate_note, "BFMI")
})

test_that("failure rule step 1: an ESS shortfall doubles the iterations in every profile", {
  cfg_loop <- analysis_plan
  cfg_loop$regression$ess_target <- 1e12  # deterministic: force exactly one allowed doubling
  cfg_loop$regression$validity_gate$max_ess_doublings <- 1L
  expect_equal(cfg_loop$profile_name, "smoke")
  fit_loop <- gate_one(raw_fit, cfg_loop)
  expect_equal(attr(fit_loop, "iter_used"), 2L * reg$iter_per_chain)
  expect_equal(fit_loop$fit@sim$iter, reg$warmup + 2L * reg$iter_per_chain)
  log <- attr(fit_loop, "ess_log")
  expect_equal(nrow(log), 2)
  expect_equal(log$iter_per_chain, c(reg$iter_per_chain, 2L * reg$iter_per_chain))
  expect_false(log$ok[1])
  expect_false(log$ok[2])
  retry <- attr(fit_loop, "retry_log")
  expect_equal(retry$step, 1L)                              # one ESS doubling; the sampler retry (step 2) is not triggered
  expect_match(retry$reason, "ESS shortfall")
  expect_equal(retry$iter_per_chain, 2L * reg$iter_per_chain)
  expect_equal(retry$warmup, reg$warmup)
  expect_equal(attr(fit_loop, "gate_status"), "not_interpretable")
  expect_false(attr(fit_loop, "diagnostics")$ok)
  expect_match(attr(fit_loop, "diagnostics")$gate_note, "ESS shortfall")
})

test_that("failure rule step 2: a tree-depth failure triggers one retry with doubled warmup and the retry control", {
  # fixture: the fit is sampled again at max_treedepth 1, so every transition is a hit
  saturated <- do.call(stats::update, c(list(object = raw_fit, recompile = FALSE),
                                        ap6_sampling_args(reg, reg$warmup, reg$iter_per_chain,
                                                          list(max_treedepth = 1))))
  expect_equal(ap6_max_treedepth(saturated), 1L)
  expect_gt(ap6_fit_diagnostics(saturated, analysis_plan)$n_treedepth_hits, 0L)
  cfg_retry <- analysis_plan
  cfg_retry$regression$validity_gate$max_ess_doublings <- 0L
  fit_retry <- gate_one(saturated, cfg_retry)
  retry <- attr(fit_retry, "retry_log")
  expect_equal(nrow(retry), 1)
  expect_equal(retry$step, 2L)
  expect_match(retry$reason, "tree depth")
  expect_equal(retry$warmup, 2L * reg$warmup)
  expect_equal(retry$iter_per_chain, reg$iter_per_chain)
  expect_equal(retry$adapt_delta, gate_cfg$adapt_delta_retry)
  expect_equal(retry$max_treedepth, as.integer(gate_cfg$max_treedepth_retry))
  expect_equal(attr(fit_retry, "warmup_used"), 2L * reg$warmup)
  expect_equal(fit_retry$fit@sim$warmup, 2L * reg$warmup)
  expect_equal(ap6_max_treedepth(fit_retry), as.integer(gate_cfg$max_treedepth_retry))
  expect_equal(ap6_adapt_delta(fit_retry), gate_cfg$adapt_delta_retry)
  diag <- attr(fit_retry, "diagnostics")
  expect_equal(diag$max_treedepth_used, as.integer(gate_cfg$max_treedepth_retry))
  expect_equal(diag$n_treedepth_hits, 0L)
  expect_true(diag$treedepth_ok)
  expect_true(retry$ok)
  expect_equal(attr(fit_retry, "gate_status"), "retried_ok")
  expect_equal(attr(fit_retry, "provenance")$gate_status, "retried_ok")
})

test_that("ap6_pp_check_data() returns observed and replicated values in long form", {
  pp <- ap6_pp_check_data(fit, n_draws = 20)
  expect_named(pp, c("outcome", "type", "draw", "obs", "value"))
  expect_equal(sum(pp$type == "y"), nrow(d))
  expect_equal(sum(pp$type == "y_rep"), 20 * nrow(d))
  expect_equal(pp$value[pp$type == "y"], d$asc_agg_z)
  expect_equal(sort(unique(pp$draw[pp$type == "y_rep"])), 1:20)
  expect_true(all(is.na(pp$draw[pp$type == "y"])))
  expect_equal(pp$value, ap6_pp_check_data(fit, n_draws = 20)$value)   # reproducible
})

test_that("ap6_fit_provenance() records formula, priors, Stan hash and versions, also as an attribute", {
  prov <- ap6_fit_provenance(fit)
  expect_s3_class(prov, "tbl_df")
  expect_equal(nrow(prov), 1)
  expect_named(prov, c("outcome", "slope_sd", "family", "formula", "nobs", "priors",
                       "stan_code_hash", "hash_algorithm", "chains", "warmup", "iter_per_chain", "seed",
                       "gate_status", "brms_version", "cmdstanr_version", "cmdstan_version"))
  expect_equal(prov$outcome, "authoritarian_aggression")
  expect_equal(prov$slope_sd, primary_sd)
  expect_equal(prov$family, "gaussian")
  expect_equal(prov$formula, deparse1(aggression_formula))
  expect_equal(prov$nobs, nrow(d))
  expect_match(prov$priors, paste0("normal(0, ", primary_sd, ")"), fixed = TRUE)
  expect_match(prov$priors, paste0("normal(0, ", analysis_plan$priors$gender_sd, ")"), fixed = TRUE)
  expect_match(prov$priors, "{metric_slopes}", fixed = TRUE)
  expect_match(prov$priors, "sigma", fixed = TRUE)
  expect_equal(prov$stan_code_hash, digest::digest(brms::stancode(fit), algo = "sha256", serialize = FALSE))
  expect_equal(prov$hash_algorithm, "sha256")
  expect_equal(prov$chains, reg$chains)
  expect_equal(prov$warmup, warmup_expected)
  expect_equal(prov$iter_per_chain, reg$iter_per_chain)
  expect_equal(prov$seed, reg$seed)
  expect_equal(prov$gate_status, attr(fit, "gate_status"))
  expect_equal(prov$brms_version, as.character(packageVersion("brms")))
  expect_equal(prov$cmdstanr_version, as.character(packageVersion("cmdstanr")))
  expect_equal(prov$cmdstan_version, cmdstanr::cmdstan_version())
  expect_identical(attr(fit, "provenance"), prov)
})
