# Posterior summaries only: no model fitting or target-store changes.
local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) {
    parent <- dirname(dir)
    if (identical(parent, dir)) stop("analysis root not found")
    dir <- parent
  }
  for (f in c("config.R", "ap6_regressions.R", "ap6_interval_comparison.R")) {
    source(file.path(dir, "R", f), local = FALSE)
  }
})

test_that("coefficient MCSEs preserve chains and retain each interval endpoint separately", {
  set.seed(173)
  n <- 1600L
  chains <- 4L
  x <- array(NA_real_, dim = c(n, chains, 2L),
    dimnames = list(NULL, NULL, c("b_age_z", "b_genderdivers")))
  for (chain in seq_len(chains)) {
    # Autocorrelation makes flattening the chains an observable error; the
    # asymmetric second coefficient gives distinct lower and upper errors.
    x[, chain, 1L] <- as.numeric(stats::arima.sim(list(ar = 0.7), n = n))
    x[, chain, 2L] <- exp(as.numeric(stats::arima.sim(list(ar = 0.7), n = n)) + 0.5 * chain)
  }
  draws <- posterior::as_draws_array(x)
  for (level in c(0.80, 0.95)) {
    probs <- c((1 - level) / 2, 1 - (1 - level) / 2)
    precision <- ap6_coefficient_simulation_precision(draws, probs)
    expect_named(precision, c("variable", "ess_bulk", "ess_tail", "rhat", "mcse_median", "mcse_q_lo", "mcse_q_hi"))
    for (i in seq_len(dim(x)[3L])) {
      expected <- posterior::mcse_quantile(x[, , i], probs = probs)
      expect_equal(precision$mcse_q_lo[i], unname(expected[1L]))
      expect_equal(precision$mcse_q_hi[i], unname(expected[2L]))
      expect_equal(precision$mcse_median[i], posterior::mcse_median(x[, , i]))
    }
    expect_gt(precision$mcse_q_hi[2], precision$mcse_q_lo[2])
    flattened <- posterior::mcse_quantile(as.numeric(x[, , 2]), probs = probs)
    expect_false(isTRUE(all.equal(unname(flattened[2]), precision$mcse_q_hi[2])))
  }
})

# One contrast per non-reference gender level, so whether AP1 retained the
# divers group decides how many gender priors a regression carries
# (the AP1 side of the rule is in test-ap3-preprocessing.R).
test_that("a gender prior is set for every non-reference level", {
  contrast_priors <- function(levels_present) {
    data <- data.frame(
      y = seq_along(levels_present) + 0,
      gender = factor(levels_present, levels = unique(levels_present))
    )
    ap6_build_gender_contrast_priors(y ~ gender, data, 0.4)
  }
  two <- contrast_priors(c("female", "male"))
  three <- contrast_priors(c("female", "male", "divers"))
  expect_identical(two$coef, "gendermale")
  expect_setequal(three$coef, c("gendermale", "genderdivers"))
  expect_true(all(three$prior == "normal(0, 0.4)" & three$class == "b" & three$tag == "gender_contrasts"))
})
