# R/ap6_regressions.R and R/ap10_inference.R: degenerate model samples (one
# respondent in a gender level), unavailable sampler diagnostics, and empty
# coefficient tables on their way into the AP10 tables. Every case must
# end in a clear error naming the function and the condition, or in a
# well-defined empty result — never in a silent number.

local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) {
    parent <- dirname(dir)
    if (identical(parent, dir)) stop("project root not found from ", getwd())
    dir <- parent
  }
  for (f in c("config.R", "ap3_imputation_validity.R", "ap6_regressions.R", "ap10_inference.R")) {
    source(file.path(dir, "R", f), local = FALSE)
  }
})

root <- zm_root()
analysis_plan <- zm_config(profile = "smoke", path = file.path(root, "config", "analysis_plan.yaml"))
# tiny fits: 2 chains, 300 warmup + 300 post-warmup draws, ESS target lowered
# so that the gate is reachable with 600 draws; every other threshold stays
analysis_plan$regression$chains <- 2L
analysis_plan$regression$cores <- 2L
analysis_plan$regression$warmup <- 300L
analysis_plan$regression$iter_per_chain <- 300L
analysis_plan$regression$ess_target <- 100
reg <- analysis_plan$regression
outcomes <- as.character(reg$outcomes)

# Stan executables of the tests go to the scratch directory, not the project
withr::local_options(list(cmdstanr_write_stan_file_dir = file.path(tempdir(), "zm_stan_edge_tests")),
                     .local_envir = teardown_env())
dir.create(file.path(tempdir(), "zm_stan_edge_tests"), showWarnings = FALSE, recursive = TRUE)

cmdstan_ok <- tryCatch({
  zm_setup()
  nzchar(cmdstanr::cmdstan_version())
}, error = function(e) FALSE)

# a small standardised model sample with the exact right-hand side of the config
make_frame <- function(n = 60, seed = 7, gender = NULL) {
  set.seed(seed)
  d <- data.frame(
    zm_security_z = stats::rnorm(n), zm_achievement_z = stats::rnorm(n), zm_power_z = stats::rnorm(n),
    zm_prestige_z = stats::rnorm(n), zm_arousal_z = stats::rnorm(n), age_z = stats::rnorm(n),
    gender = if (is.null(gender)) {
      factor(sample(c("male", "female"), n, replace = TRUE), levels = c("male", "female"))
    } else {
      gender
    },
    income_z = stats::rnorm(n)
  )
  for (o in outcomes) {
    d[[paste0(o, "_z")]] <- as.numeric(scale(0.3 * d$zm_power_z + stats::rnorm(n)))
  }
  attr(d, "z_params") <- tibble::tibble(var = outcomes, z_col = paste0(outcomes, "_z"), mean = 3, sd = 0.8)
  d
}
# a legitimate but degenerate sample: exactly one respondent in the female group
d_one <- make_frame(gender = factor(c("female", rep("male", 59)), levels = c("male", "female")))
edge_set <- define_regression_model_set(analysis_plan)
edge_set$formulas <- edge_set$formulas["authoritarian_aggression"]
plain_formula <- edge_set$formulas[["authoritarian_aggression"]]

# one fit through the pipeline's fitting verb and validity gate (no ESS
# doubling), reused by every test that needs a sampled model
fit_cache <- new.env(parent = emptyenv())
edge_fit <- function() {
  if (is.null(fit_cache$fit)) {
    gate_plan <- analysis_plan
    gate_plan$regression$validity_gate$max_ess_doublings <- 0L
    fits <- fit_primary_regressions(d_one, edge_set, analysis_plan)
    fit_cache$fit <- apply_validity_gate(fits, gate_plan)$authoritarian_aggression
  }
  fit_cache$fit
}

# ---- AP6: degenerate model samples ---------------------------------------------------------

test_that("one respondent in a gender level is degenerate but allowed", {
  expect_equal(sum(d_one$gender == "female"), 1L)
  pr <- as.data.frame(ap6_build_gender_contrast_priors(plain_formula, d_one, analysis_plan$priors$gender_sd))
  expect_identical(pr$coef, "genderfemale")
  expect_equal(pr$prior, paste0("normal(0, ", analysis_plan$priors$gender_sd, ")"))
  expect_equal(pr$tag, "gender_contrasts")
  # the design matrix keeps the single respondent: one 1 in the contrast column
  mm <- stats::model.matrix(stats::delete.response(stats::terms(plain_formula, data = d_one)), data = d_one)
  expect_equal(sum(mm[, "genderfemale"]), 1)
})

test_that("an empty list of motives or outcomes in the plan is refused when the plan is loaded", {
  # The predictor side of the regressions is built from the motives
  # (zm_config()), so an empty motive list must stop the load, as an empty
  # outcome list does.
  for (field in c("motives", "outcomes")) {
    root <- withr::local_tempdir(); dir.create(file.path(root, "config"))
    yaml_path <- file.path(root, "config", "analysis_plan.yaml")
    lines <- readLines(file.path(zm_root(), "config", "analysis_plan.yaml"), warn = FALSE)
    lines <- sub(paste0("^  ", field, ": \\[.*$"), paste0("  ", field, ": []"), lines)
    writeLines(lines, yaml_path)
    expect_error(zm_config(profile = "full", path = yaml_path), paste0("regression$", field, "' is empty"),
                 fixed = TRUE, info = field)
  }
})

test_that("degenerate sampler diagnostics are NA, never a number", {
  no_energy <- data.frame(Parameter = character(0), Chain = integer(0),
                          Iteration = integer(0), Value = numeric(0))
  expect_true(is.na(ap6_bfmi(no_energy)))
  constant <- data.frame(Parameter = "energy__", Chain = 1L, Iteration = 1:5, Value = rep(3, 5))
  expect_true(all(is.na(ap6_bfmi(constant))))
  single <- data.frame(Parameter = "energy__", Chain = 1L, Iteration = 1L, Value = 3)
  expect_true(all(is.na(ap6_bfmi(single))))
})

# ---- one sampled fit: degenerate gender group, unavailable diagnostics, gate attributes ----

test_that("a gender level with a single respondent is fitted and reported with a wide interval", {
  skip_if_not(cmdstan_ok, "CmdStan not available")
  fit <- edge_fit()
  expect_s3_class(fit, "brmsfit")
  expect_equal(stats::nobs(fit), nrow(d_one))
  summary <- posterior::summarise_draws(
    posterior::subset_draws(posterior::as_draws_df(fit), variable = ap6_focal_b_vars(fit)),
    estimate = stats::median, sd = stats::sd)
  gender_row <- summary[summary$variable == "b_genderfemale", ]
  expect_equal(nrow(gender_row), 1)
  expect_true(is.finite(gender_row$estimate))
  # the single respondent buys little information: the posterior SD of the
  # contrast is far above that of the metric covariates
  expect_gt(gender_row$sd, max(summary$sd[summary$variable %in% c("b_age_z", "b_income_z")]))
  expect_equal(ap6_clean_terms("b_genderfemale", analysis_plan$regression)$term_type, "covariate")
})

test_that("unavailable sampler diagnostics fail the validity gate instead of passing it", {
  skip_if_not(cmdstan_ok, "CmdStan not available")
  # no sampler diagnostics at all
  fit_nod <- edge_fit()
  fit_nod$fit@sim$samples <- lapply(fit_nod$fit@sim$samples, function(s) {
    attr(s, "sampler_params") <- NULL
    s
  })
  diag_nod <- ap6_fit_diagnostics(fit_nod, analysis_plan)
  expect_false(diag_nod$ok)
  expect_true(is.na(diag_nod$n_divergent))
  expect_true(is.na(diag_nod$n_treedepth_hits))
  expect_true(is.na(diag_nod$bfmi_min))
  expect_false(diag_nod$divergences_ok)
  expect_false(diag_nod$treedepth_ok)
  expect_false(diag_nod$bfmi_ok)
  expect_match(diag_nod$gate_note, "sampler diagnostics unavailable")
})
