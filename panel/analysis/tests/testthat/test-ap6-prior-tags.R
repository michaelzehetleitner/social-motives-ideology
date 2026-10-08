# The prior tags the four AP6 fitting paths write.
#
# `ap6_run_priorsense()` selects a prior block by the `tag` brms writes into
# the Stan program (`lprior_<tag>`). Every block of
# `analysis_plan$sensitivity$powerscale$blocks` except `all_priors` must therefore exist
# as a tag on every prior set the fitting paths hand to `brms::brm()`; a block
# without its tag cannot be power-scaled at all. Part A checks all four paths
# against a stubbed `brms::brm()`; Part B fits one model for real and takes the
# tags through `prior_summary()`, the Stan code and priorsense itself.

local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) {
    parent <- dirname(dir)
    if (identical(parent, dir)) stop("project root not found from ", getwd())
    dir <- parent
  }
  for (f in c("config.R", "ap6_regressions.R")) {
    source(file.path(dir, "R", f), local = FALSE)
  }
})

pt_root <- zm_root()
pt_cfg <- zm_config(profile = "smoke",
                    path = file.path(pt_root, "config", "analysis_plan.yaml"))
pt_outcomes <- c("authoritarian_aggression", "authoritarian_submission",
                 "conventionalism", "sdo_d_dominance")
# The blocks that priorsense selects by tag; `all_priors` selects nothing.
pt_tag_blocks <- setdiff(as.character(pt_cfg$sensitivity$powerscale$blocks), "all_priors")

# The analysis frame of test-targets-regressions.R (`tr_regression_input()`):
# the saved z columns plus the fixed standardisation constants the
# prior-predictive path reads.
pt_regression_input <- function() {
  columns <- c("asc_agg_z", "asc_sub_z", "asc_conv_z", "sdo_dom_z", "zm_security_z", "zm_achievement_z",
               "zm_power_z", "zm_prestige_z", "zm_arousal_z", "age_z", "income_z")
  n <- 12L
  data <- tibble::tibble(
    respondent_id = seq_len(n),
    gender = factor(rep(c("male", "female"), length.out = n), levels = c("male", "female")))
  for (i in seq_along(columns)) data[[columns[i]]] <- ((seq_len(n) + i) %% 5) - 2
  attr(data, "z_parameters") <- tibble::tibble(
    var = sub("_z$", "", columns), source = sub("_z$", "", columns), transform = "identity",
    z_col = columns, mean = 3, sd = 1.5, n = n)
  data
}

pt_stub_fit <- function() structure(list(), class = "brmsfit")

# The posterior-predictive leaf of test-targets-regressions.R
# (`tr_posterior_leaf()`); an observed excess kurtosis above the upper bound of
# its replicated band fires the one-sided Student trigger.
pt_posterior_leaf <- function(kurtosis_observed = 0.9) {
  statistic <- function(observed, lo, hi) {
    list(statistic = "x", observed = observed,
         replicated_interval = c(`2.5%` = lo, `97.5%` = hi), replicated_median = (lo + hi) / 2,
         interval_membership = if (observed >= lo && observed <= hi) "inside" else "outside",
         posterior_predictive_p_value = 0.4)
  }
  checks <- list(
    mean = statistic(0, -0.1, 0.1), sd = statistic(1, 0.9, 1.1),
    skewness = statistic(0.05, -0.2, 0.2),
    excess_kurtosis = statistic(kurtosis_observed, -0.2, 0.2),
    min = statistic(-2.5, -3, -2), max = statistic(2.5, 2, 3))
  for (name in names(checks)) checks[[name]]$statistic <- name
  attr(checks, "slope_sd") <- 0.2
  checks
}

pt_normal <- function(sd) paste0("normal(0, ", sd, ")")

# The full assertion on one captured prior set: every registered row with its
# width and its tag, and nothing else. The compared frames carry the `set`
# label, so a failure names the path, outcome and width it came from.
pt_expect_prior_set <- function(prior, slope_sd, sigma_sd, nu = NULL, label = "") {
  rows <- as.data.frame(prior)
  actual <- data.frame(
    set = label, class = rows$class, coef = rows$coef,
    prior = rows$prior, tag = rows$tag, stringsAsFactors = FALSE)
  actual <- actual[order(actual$class, actual$coef), ]
  rownames(actual) <- NULL

  expected <- data.frame(
    set = label,
    class = c("b", "b", "Intercept", "sigma"),
    coef = c("", "genderfemale", "", ""),
    prior = c(pt_normal(slope_sd), pt_normal(pt_cfg$priors$gender_sd),
              pt_normal(pt_cfg$priors$intercept_sd), pt_normal(sigma_sd)),
    tag = c("metric_slopes", "gender_contrasts", "intercept", "sigma"),
    stringsAsFactors = FALSE)
  if (!is.null(nu)) {
    # The fixed degrees of freedom stay untagged, exactly as in
    # `fit_student_t_if_needed()`; no configured block selects them.
    expected <- rbind(expected, data.frame(
      set = label, class = "nu", coef = "", prior = paste0("constant(", nu, ")"), tag = "",
      stringsAsFactors = FALSE))
  }
  expected <- expected[order(expected$class, expected$coef), ]
  rownames(expected) <- NULL

  expect_equal(actual, expected)

  # The availability guarantee: every configured block except `all_priors` is a
  # tag priorsense can select on this fit.
  expect_equal(
    list(set = label, missing_tags = setdiff(pt_tag_blocks, rows$tag)),
    list(set = label, missing_tags = character(0)))
}

# --- Part A: the four fitting paths against a stubbed brms::brm() ------------

test_that("the primary fits tag the intercept and sigma priors of every outcome", {
  data <- pt_regression_input()
  model_set <- define_regression_model_set(pt_cfg)
  captured <- list()
  testthat::local_mocked_bindings(
    brm = function(...) {
      captured[[length(captured) + 1L]] <<- list(...)
      pt_stub_fit()
    }, .package = "brms")

  fits <- fit_primary_regressions(data, model_set, pt_cfg)

  expect_named(fits, pt_outcomes)
  expect_length(captured, 4L)
  for (i in seq_along(captured)) {
    pt_expect_prior_set(
      captured[[i]]$prior,
      slope_sd = model_set$primary_slope_sd,
      sigma_sd = pt_cfg$priors$sigma_sd,
      label = paste("primary", pt_outcomes[[i]]))
  }
})

test_that("both prior-width comparisons tag the intercept and sigma priors", {
  data <- pt_regression_input()
  model_set <- define_regression_model_set(pt_cfg)
  captured <- list()
  testthat::local_mocked_bindings(
    brm = function(...) {
      captured[[length(captured) + 1L]] <<- list(...)
      pt_stub_fit()
    }, .package = "brms")

  comparisons <- fit_prior_width_comparisons(data, model_set, pt_cfg)

  expect_named(comparisons, c("0.10", "0.40"))
  expect_length(captured, 8L)
  widths <- rep(c(0.1, 0.4), each = 4L)  # width outside, the four outcomes inside
  for (i in seq_along(captured)) {
    pt_expect_prior_set(
      captured[[i]]$prior,
      slope_sd = widths[[i]],
      sigma_sd = pt_cfg$priors$sigma_sd,
      label = paste("width", widths[[i]], pt_outcomes[[(i - 1L) %% 4L + 1L]]))
  }
})

test_that("every prior-predictive specification tags the intercept and sigma priors", {
  data <- pt_regression_input()
  model_set <- define_regression_model_set(pt_cfg)
  student <- pt_cfg$sensitivity$student_t
  captured <- list()
  testthat::local_mocked_bindings(
    brm = function(...) {
      captured[[length(captured) + 1L]] <<- list(...)
      pt_stub_fit()
    }, .package = "brms")
  testthat::local_mocked_bindings(
    posterior_predict = function(...) matrix(seq_len(4L * 12L) / 12, nrow = 4L),
    .package = "brms")

  checks <- check_prior_predictions(data, model_set, pt_cfg)

  expect_length(checks, 4L)
  # Three Gaussian widths then the Student specification, per outcome.
  expect_length(captured, 16L)
  specifications <- list(
    list(slope_sd = 0.1, sigma_sd = pt_cfg$priors$sigma_sd, nu = NULL),
    list(slope_sd = 0.2, sigma_sd = pt_cfg$priors$sigma_sd, nu = NULL),
    list(slope_sd = 0.4, sigma_sd = pt_cfg$priors$sigma_sd, nu = NULL),
    list(slope_sd = model_set$primary_slope_sd,
         sigma_sd = student$sigma_scale_prior_sd, nu = student$nu_fixed))
  for (i in seq_along(captured)) {
    specification <- specifications[[(i - 1L) %% 4L + 1L]]
    outcome <- pt_outcomes[[(i - 1L) %/% 4L + 1L]]
    expect_identical(captured[[i]]$sample_prior, "only")
    pt_expect_prior_set(
      captured[[i]]$prior,
      slope_sd = specification$slope_sd,
      sigma_sd = specification$sigma_sd,
      nu = specification$nu,
      label = paste("prior-predictive", outcome, specification$slope_sd))
  }
})

test_that("the Student-t refits tag the intercept and the Student sigma prior", {
  data <- pt_regression_input()
  model_set <- define_regression_model_set(pt_cfg)
  student <- pt_cfg$sensitivity$student_t
  primary_fits <- stats::setNames(lapply(pt_outcomes, function(o) pt_stub_fit()), pt_outcomes)
  predictive_checks <- list(
    posterior = stats::setNames(lapply(pt_outcomes, function(o) pt_posterior_leaf()), pt_outcomes))
  captured <- list()
  testthat::local_mocked_bindings(
    brm = function(...) {
      captured[[length(captured) + 1L]] <<- list(...)
      pt_stub_fit()
    },
    .package = "brms")

  fits <- fit_student_t_if_needed(data, model_set, primary_fits, predictive_checks, pt_cfg)

  expect_named(fits, pt_outcomes)
  expect_true(all(attr(fits, "heavy_tail_triggers")))
  expect_length(captured, 4L)
  for (i in seq_along(captured)) {
    pt_expect_prior_set(
      captured[[i]]$prior,
      slope_sd = model_set$primary_slope_sd,
      sigma_sd = student$sigma_scale_prior_sd,
      nu = student$nu_fixed,
      label = paste("student", pt_outcomes[[i]]))
  }
})

# --- Part B: one real fit, the Stan program and priorsense -------------------

withr::local_options(
  list(cmdstanr_write_stan_file_dir = file.path(tempdir(), "zm_stan_prior_tags")),
  .local_envir = teardown_env())
dir.create(file.path(tempdir(), "zm_stan_prior_tags"), showWarnings = FALSE, recursive = TRUE)

pt_cmdstan_ok <- tryCatch({
  zm_setup()
  nzchar(cmdstanr::cmdstan_version())
}, error = function(e) FALSE)

# The 200-row standardised frame of test-ap6-regressions.R (`make_frame()`).
pt_make_frame <- function(n = 200, seed = 42) {
  set.seed(seed)
  d <- data.frame(
    zm_security_z = rnorm(n), zm_achievement_z = rnorm(n), zm_power_z = rnorm(n),
    zm_prestige_z = rnorm(n), zm_arousal_z = rnorm(n), age_z = rnorm(n),
    gender = factor(sample(c("male", "female"), n, replace = TRUE), levels = c("male", "female")),
    income_z = rnorm(n))
  female <- as.numeric(d$gender == "female")
  z <- function(x) as.numeric(scale(x))
  d$asc_agg_z <- z(0.30 * d$zm_power_z - 0.20 * d$zm_achievement_z + 0.25 * female + rnorm(n, sd = 0.9))
  d$asc_sub_z <- z(-0.20 * d$zm_arousal_z - 0.15 * d$zm_achievement_z + rnorm(n, sd = 0.95))
  d$asc_conv_z <- z(0.20 * d$zm_security_z - 0.20 * d$zm_arousal_z + 0.15 * d$age_z + rnorm(n, sd = 0.9))
  d$sdo_dom_z <- z(0.25 * d$zm_power_z + 0.15 * d$zm_prestige_z + 0.35 * female + rnorm(n, sd = 0.9))
  tibble::as_tibble(d)
}

test_that("a real primary fit carries the intercept and sigma tags into priorsense", {
  skip_if_not(pt_cmdstan_ok, "CmdStan not available")

  cfg_small <- pt_cfg
  cfg_small$regression$chains <- 2L
  cfg_small$regression$cores <- 2L
  cfg_small$regression$warmup <- 500L
  cfg_small$regression$iter_per_chain <- 500L
  model_set <- define_regression_model_set(cfg_small)
  model_set$formulas <- model_set$formulas[1]  # one outcome is enough for the tags

  fits <- fit_primary_regressions(pt_make_frame(), model_set, cfg_small)
  fit <- fits[[1]]

  summary_rows <- as.data.frame(brms::prior_summary(fit))
  intercept <- summary_rows[summary_rows$class == "Intercept" & summary_rows$source == "user", ]
  sigma <- summary_rows[summary_rows$class == "sigma" & summary_rows$source == "user", ]
  expect_equal(nrow(intercept), 1L)
  expect_equal(nrow(sigma), 1L)
  expect_identical(intercept$tag[[1]], "intercept")
  expect_identical(sigma$tag[[1]], "sigma")

  # brms names one log-prior accumulator per tag in the Stan program; these are
  # the variables priorsense power-scales when `prior_selection` is a block.
  code <- brms::stancode(fit)
  expect_true(grepl("lprior_intercept", code, fixed = TRUE))
  expect_true(grepl("lprior_sigma", code, fixed = TRUE))

  powerscale <- pt_cfg$sensitivity$powerscale
  result <- ap6_run_priorsense(
    fit,
    blocks = powerscale$blocks,
    components = powerscale$component,
    lower_alpha = 0.99,
    upper_alpha = 1.01,
    divergence_measure = powerscale$div_measure,
    sensitivity_threshold = 0.05
  )

  expect_identical(names(result$blocks), as.character(powerscale$blocks))
  for (block in c("intercept", "sigma")) {
    selected <- result$blocks[[block]]
    expect_identical(selected$prior_selection, block)
    expect_s3_class(selected$matrix, "tbl_df")
    expect_gt(nrow(selected$matrix), 0L)
    expect_true("prior" %in% names(selected$matrix))
  }
})
