# LAYER 2 tests — the facts of the S3 answer (R/report_supplement_prior_sensitivity.R).

local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) {
    parent <- dirname(dir)
    if (identical(parent, dir)) stop("project root not found from ", getwd())
    dir <- parent
  }
  for (f in c("config.R", "ap10_inference.R", "classify_importance_sampling.R", "report_supplement_prior_sensitivity.R")) {
    source(file.path(dir, "R", f), local = FALSE)
  }
  assign("prior_sensitivity_root", dir, envir = .GlobalEnv)
})

ps_plan <- zm_config(profile = "full", path = file.path(prior_sensitivity_root, "config", "analysis_plan.yaml"))
ps_codebook <- zm_codebook(ps_plan)
ps_widths <- ps_plan$priors$slope_sd_sweep

# One coefficient of the route's prior-width result: its median and interval per
# width, and the two stability flags the route records.
ps_cell <- function(outcome, motive, median, lo, hi) {
  widths <- sprintf("%.2f", ps_widths)
  excludes <- lo > 0 | hi < 0
  signs <- sign(median)
  list(
    outcome = outcome, coefficient = paste0("b_", zm_z_col(motive, ps_codebook)),
    by_width = stats::setNames(lapply(seq_along(widths), function(i) {
      list(median = median[i], interval = c(lo[i], hi[i]), interval_excludes_zero = excludes[i])
    }), widths),
    interval_exclusion_stable = length(unique(excludes)) == 1L,
    median_direction_stable = length(unique(signs[signs != 0])) <= 1L
  )
}
# The fit register of one outcome: one record per width.
ps_register <- function(outcome = "authoritarian_aggression", key = "asc_agg", valid = rep(TRUE, length(ps_widths))) {
  lapply(seq_along(ps_widths), function(i) list(
    outcome = outcome, outcome_key = key,
    role = if (ps_widths[i] == ps_plan$priors$slope_sd_primary) "primary" else "sweep",
    slope_sd = ps_widths[i], family = ps_plan$regression$family,
    fit_valid = valid[i], gate_status = if (valid[i]) "ok" else "not_interpretable"
  ))
}
ps_no_priorsense <- tibble::tibble(outcome = character(), term = character(), block = character())

test_that("a model whose every planned block errored counts as completely failed", {
  answer <- check_powerscale_outcome(tibble::tibble(
    fit_valid = TRUE, fit_gate_status = "ok",
    importance_sampling_status = "reliable",
    outcome = "asc_agg", block = "intercept", term = "(Intercept)",
    diagnosis = "error: diagnostic failed"
  ))
  expect_identical(answer$failed_models, "asc_agg")
  expect_equal(nrow(answer$failed_blocks), 0L)
  expect_false(answer$any_successful)
  expect_identical(answer$n_flagged, 0L)
})

test_that("a partly failed model lists its blocks, and flags count distinct coefficients", {
  answer <- check_powerscale_outcome(tibble::tibble(
    fit_valid = TRUE, fit_gate_status = "ok",
    importance_sampling_status = "reliable",
    outcome = c("asc_agg", "asc_agg", "asc_sub", "asc_sub"),
    block = c("metric_slopes", "intercept", "metric_slopes", "metric_slopes"),
    term = c("zm_power", "(Intercept)", "zm_power", "zm_power"),
    diagnosis = c("error: diagnostic failed", "-", "potential prior-data conflict",
                  "potential prior-data conflict")
  ))
  expect_length(answer$failed_models, 0L)
  expect_identical(answer$failed_blocks$outcome, "asc_agg")
  expect_identical(answer$failed_blocks$block, "metric_slopes")
  expect_true(answer$any_successful)
  expect_identical(answer$n_flagged, 1L)
})

test_that("the sweep table classifies stable, interval-changing and direction-changing coefficients", {
  n <- length(ps_widths)
  top <- ps_widths == max(ps_widths)
  low <- ps_widths == min(ps_widths)
  stability <- list(authoritarian_aggression = list(
    # credible positive at every width -> stable
    ps_cell("authoritarian_aggression", "zm_power", rep(0.25, n), rep(0.15, n), rep(0.35, n)),
    # positive throughout, credible only at the widest prior -> ci_changes
    ps_cell("authoritarian_aggression", "zm_prestige", rep(0.10, n), ifelse(top, 0.02, -0.02), rep(0.20, n)),
    # negative at the tightest prior, positive otherwise, never credible -> direction_changes
    ps_cell("authoritarian_aggression", "zm_security", ifelse(low, -0.05, 0.08), ifelse(low, -0.15, -0.02),
            ifelse(low, 0.05, 0.18))
  ))
  priorsense <- dplyr::bind_rows(
    tibble::tibble(outcome = "asc_agg", term = c("zm_power", "zm_prestige", "zm_security"), block = "metric_slopes",
                   prior_sens = c(0.07, 0.01, 0.03), lik_sens = c(0.13, 0.09, 0.11),
                   diagnosis = c("potential prior-data conflict", "-", "-")),
    tibble::tibble(outcome = "asc_agg", term = "zm_power", block = "all_priors", prior_sens = 0.5,
                   lik_sens = 0.5, diagnosis = "all-priors run")
  )
  st <- tabulate_prior_width_sweep(stability, ps_register(), priorsense, ps_codebook, ps_plan)
  expect_equal(st$term, c("zm_power", "zm_prestige", "zm_security"))
  expect_equal(st$stability, c("stable", "ci_changes", "direction_changes"))
  expect_equal(st$interval_exclusion_changes, c(FALSE, TRUE, FALSE))
  expect_equal(st$direction_changes, c(FALSE, FALSE, TRUE))
  # the language rule concerns the interval decision, not the direction of the median
  expect_equal(st$robust_to_prior, c(TRUE, FALSE, TRUE))
  labels <- format(ps_widths, trim = TRUE, drop0trailing = TRUE)
  expect_equal(st$credible_under[1], paste(labels, collapse = "; "))
  expect_equal(st$credible_under[2], labels[top])
  expect_equal(st$n_credible, c(n, 1L, 0L))
  expect_equal(st$interval_excludes_zero_sds[[2]], ps_widths[top])
  expect_equal(st$negative_median_sds[[3]], ps_widths[low])
  expect_equal(st[[paste0("est_", labels[low])]][3], -0.05)
  # only the block that owns the slopes is joined from power scaling
  expect_equal(st$prior_sens, c(0.07, 0.01, 0.03))
  expect_equal(st$diagnosis[1], "potential prior-data conflict")
})

test_that("the sweep table withholds the comparison when a width's fit failed or is missing", {
  n <- length(ps_widths)
  stability <- list(authoritarian_aggression = list(
    ps_cell("authoritarian_aggression", "zm_power", rep(0.2, n), rep(0.1, n), rep(0.3, n))))
  expect_true(tabulate_prior_width_sweep(stability, ps_register(), ps_no_priorsense, ps_codebook, ps_plan)$comparison_valid)
  for (failed in seq_len(n)) {
    valid <- rep(TRUE, n)
    valid[failed] <- FALSE
    out <- tabulate_prior_width_sweep(stability, ps_register(valid = valid), ps_no_priorsense, ps_codebook, ps_plan)
    expect_false(out$comparison_valid)
    expect_identical(out$stability, "not_interpretable")
    expect_true(is.na(out$credible_under))
    expect_true(is.na(out$robust_to_prior))
    expect_null(out$interval_excludes_zero_sds[[1]])
    label <- format(ps_widths[failed], trim = TRUE, drop0trailing = TRUE)
    expect_true(is.na(out[[paste0("est_", label)]]))
  }
})

test_that("the sweep counts as interpretable only when every outcome has a passing fit at every width", {
  register <- unlist(lapply(seq_along(ps_plan$regression$outcomes), function(i) {
    ps_register(outcome = paste0("outcome_", i), key = ps_plan$regression$outcomes[[i]])
  }), recursive = FALSE)
  expect_true(check_prior_width_fits(register, ps_plan))
  expect_false(check_prior_width_fits(register[-1], ps_plan))
  failed <- register
  failed[[1]]$fit_valid <- FALSE
  expect_false(check_prior_width_fits(failed, ps_plan))
  expect_false(check_prior_width_fits(list(), ps_plan))
})
