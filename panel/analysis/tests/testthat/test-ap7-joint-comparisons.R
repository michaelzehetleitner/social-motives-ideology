# R/ap7_joint_comparisons.R: the AP7 / RQ2 joint-model summaries. Every
# comparison is formed inside the paired draw, the withheld classification of
# an invalid joint fit keeps its medians and intervals, and the two shaping
# helpers refuse a posterior that does not carry exactly the declared
# parameters.
#
# No test here fits a model. The draws are artificial and test arithmetic, not
# posterior adequacy; a stub fit carries only the attributes the shared AP6
# validity gate records.

local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) {
    parent <- dirname(dir)
    if (identical(parent, dir)) stop("project root not found from ", getwd())
    dir <- parent
  }
  for (f in c("config.R", "ap6_regressions.R", "ap10_inference.R", "ap7_joint_comparisons.R")) {
    source(file.path(dir, "R", f), local = FALSE)
  }
})

aj_root <- zm_root()
aj_cfg <- zm_config(profile = "smoke", path = file.path(aj_root, "config", "analysis_plan.yaml"))
aj_level <- aj_cfg$regression$ci_level  # .95

aj_response_ids <- c(aggression = "ascaggz", submission = "ascsubz",
                     conventionalism = "ascconvz", sdo_d = "sdodomz")
# The joint posterior names a motive by its key. The fixtures below index the
# five motives by position (their designed values depend on it), so this map
# keeps a fixed order of its own; the plan's order is tested on
# ap8_joint_motive_columns().
aj_one_motive <- c(zm_achievement = "zm_achievement_z")
aj_motive_columns <- c(zm_security = "zm_security_z", zm_arousal = "zm_arousal_z",
                       zm_achievement = "zm_achievement_z", zm_power = "zm_power_z",
                       zm_prestige = "zm_prestige_z")

# --- fixtures ---------------------------------------------------------------

# A nontrivial paired fixture: mean ASC is `a`, the three components sit one
# unit apart around it, and SDO-D is the same five values in a different draw
# order. Subtracting marginal medians or marginal interval endpoints gives a
# different, wrong answer.
aj_paired_draws <- function() {
  a <- c(0, 1, 2, 10, 20)
  tibble::tibble(
    .draw = 1:5,
    b_ascaggz_zm_achievement_z = a - 1,
    b_ascsubz_zm_achievement_z = a,
    b_ascconvz_zm_achievement_z = a + 1,
    b_sdodomz_zm_achievement_z = c(1, 2, 10, 20, 0)
  )
}

aj_coefficient_draws <- function(aggression, submission, conventionalism, sdo_d) {
  tibble::tibble(
    .draw = seq_along(aggression),
    b_ascaggz_zm_achievement_z = aggression,
    b_ascsubz_zm_achievement_z = submission,
    b_ascconvz_zm_achievement_z = conventionalism,
    b_sdodomz_zm_achievement_z = sdo_d
  )
}

aj_coefficients <- function(draws = aj_paired_draws()) {
  ap8_shape_joint_coefficient_draws(draws, aj_response_ids, aj_one_motive)
}

# One column per motive and response, every cell distinguishable.
aj_five_motive_draws <- function() {
  draws <- tibble::tibble(.draw = 1:5)
  for (m in seq_along(aj_motive_columns)) {
    for (r in seq_along(aj_response_ids)) {
      draws[[paste0("b_", aj_response_ids[[r]], "_", aj_motive_columns[[m]])]] <-
        seq_len(5) / 10 + m / 100 + r / 1000
    }
  }
  draws
}

# The three within-ASC pairs carry one value per draw and the three ASC-SDO-D
# pairs another, so that the draw-wise difference of the two averages is
# exactly `within - across`.
aj_within <- c(0, 0.045, 0.09, 0.45, 0.9)
aj_across <- c(0.045, 0.09, 0.45, 0.9, 0)

aj_residual_draws <- function(within = aj_within, across = aj_across, reversed = FALSE) {
  draws <- tibble::tibble(.draw = seq_along(within))
  names_forward <- c("rescor__ascaggz__ascsubz", "rescor__ascaggz__ascconvz",
                     "rescor__ascsubz__ascconvz", "rescor__ascaggz__sdodomz",
                     "rescor__ascsubz__sdodomz", "rescor__ascconvz__sdodomz")
  names_reversed <- c("rescor__ascsubz__ascaggz", "rescor__ascconvz__ascaggz",
                      "rescor__ascconvz__ascsubz", "rescor__sdodomz__ascaggz",
                      "rescor__sdodomz__ascsubz", "rescor__sdodomz__ascconvz")
  columns <- if (reversed) names_reversed else names_forward
  values <- list(within, within, within, across, across, across)
  for (i in seq_along(columns)) draws[[columns[i]]] <- values[[i]]
  draws
}

aj_residuals <- function(...) ap8_shape_joint_residual_draws(aj_residual_draws(...), aj_response_ids)

aj_diagnostics <- function(ok = TRUE) {
  tibble::tibble(ok = ok, ess_ok = ok, rhat_ok = ok, gate_note = if (ok) "ok" else "ESS shortfall")
}

# A stub in the shape the shared AP6 gate leaves behind (see tr_fit() in
# tests/testthat/test-targets-regressions.R): only the recorded attributes.
aj_fit <- function(gate_status = "ok", ok = TRUE, gated = TRUE) {
  fit <- structure(list(), class = "brmsfit")
  if (gated) {
    attr(fit, "diagnostics") <- aj_diagnostics(ok)
    attr(fit, "gate_status") <- gate_status
  }
  fit
}

aj_error <- function(message = "the joint sampler did not start") {
  tryCatch(stop(message), error = function(e) e)
}

aj_row <- function(table, column, value) {
  row <- table[table[[column]] == value, , drop = FALSE]
  expect_equal(nrow(row), 1L)
  row
}

# --- the nontrivial paired fixture ------------------------------------------

# The strength differences of RQ2a: within
# every paired draw, the absolute coefficient of the first-named outcome minus
# that of the second-named one; for the mean ASC summary the three signed
# coefficients are averaged before the absolute value is taken.

test_that("the overall and component strength differences are formed inside the paired draw", {
  coefficients <- aj_coefficients()
  overall <- ap8_result_overall_asc_sdo_coefficient_differences(coefficients, aj_level)
  expect_equal(nrow(overall), 1L)
  expect_identical(overall$contrast, "|SDO-D| - |mean ASC|")
  expect_identical(c(overall$first_outcome, overall$second_outcome), c("sdo_d", "mean_asc"))
  # |SDO-D| - |mean ASC| = (1, 1, 8, 10, -20): every coefficient is positive
  # here, so it equals the signed difference.
  expect_equal(unname(overall$posterior_median), 1, tolerance = 1e-8)
  expect_equal(unname(overall$lower), -17.9, tolerance = 1e-8)
  expect_equal(unname(overall$upper), 9.8, tolerance = 1e-8)

  components <- extract_joint_asc_components_sdo_coefficient_differences(coefficients, aj_level)
  expect_setequal(components$contrast,
                  c("|Aggression| - |SDO-D|", "|Submission| - |SDO-D|", "|Conventionalism| - |SDO-D|"))
  # The first aggression draw is -1, so |aggression| - |SDO-D| is (0, -2, -9,
  # -11, 19) where the signed difference was (-2, -2, -9, -11, 19).
  expected <- list(
    "|Aggression| - |SDO-D|" = c(-2, -10.8, 17.1),
    "|Submission| - |SDO-D|" = c(-1, -9.8, 17.9),
    "|Conventionalism| - |SDO-D|" = c(0, -8.8, 18.9)
  )
  for (contrast in names(expected)) {
    row <- aj_row(components, "contrast", contrast)
    expect_equal(unname(c(row$posterior_median, row$lower, row$upper)), expected[[contrast]],
                 tolerance = 1e-8)
  }
})

test_that("the overall contrast is not the difference of the marginal summaries", {
  coefficients <- aj_coefficients()
  directions <- ap8_result_overall_asc_sdo_directions(coefficients, aj_level)
  mean_asc <- aj_row(directions, "outcome", "mean_asc")
  sdo_d <- aj_row(directions, "outcome", "sdo_d")
  # Subtracting the marginal medians gives 0, the marginal endpoints 0 and 0.
  expect_equal(unname(sdo_d$posterior_median - mean_asc$posterior_median), 0, tolerance = 1e-8)
  expect_equal(unname(sdo_d$lower - mean_asc$lower), 0, tolerance = 1e-8)
  expect_equal(unname(sdo_d$upper - mean_asc$upper), 0, tolerance = 1e-8)
  overall <- ap8_result_overall_asc_sdo_coefficient_differences(coefficients, aj_level)
  expect_equal(unname(overall$posterior_median), 1, tolerance = 1e-8)
})

test_that("the within-ASC strength differences carry their subtraction order in the label", {
  within <- extract_joint_within_asc_coefficient_differences(aj_coefficients(), aj_level)
  expect_setequal(within$contrast,
                  c("|Aggression| - |submission|", "|Aggression| - |conventionalism|",
                    "|Submission| - |conventionalism|"))
  # The components are one unit apart in every draw, but the first aggression
  # draw is negative: its absolute value breaks the constant offset of the
  # two contrasts with aggression, and only submission - conventionalism keeps
  # its whole interval on the median.
  expected <- list(
    "|Aggression| - |submission|" = c(-1, -1, 0.8),
    "|Aggression| - |conventionalism|" = c(-2, -2, -0.2),
    "|Submission| - |conventionalism|" = c(-1, -1, -1)
  )
  for (contrast in names(expected)) {
    row <- aj_row(within, "contrast", contrast)
    expect_equal(unname(c(row$posterior_median, row$lower, row$upper)), expected[[contrast]],
                 tolerance = 1e-8)
  }
  first <- c("|Aggression| - |submission|" = "aggression", "|Aggression| - |conventionalism|" = "aggression",
             "|Submission| - |conventionalism|" = "submission")
  expect_identical(within$first_outcome, unname(first[within$contrast]))
})

test_that("a constant signed SDO-D offset is no strength difference when the signs vary", {
  component <- c(0.1, -0.3, 0.5, -0.2, 0.05)
  coefficients <- aj_coefficients(
    aj_coefficient_draws(component, component, component, component + 0.2)
  )
  # The signed difference SDO-D - mean ASC is 0.2 in every draw; the strength
  # difference |SDO-D| - |mean ASC| is (0.2, -0.2, 0.2, -0.2, 0.2).
  overall <- ap8_result_overall_asc_sdo_coefficient_differences(coefficients, aj_level)
  expect_equal(unname(overall$posterior_median), 0.2, tolerance = 1e-8)
  expect_equal(unname(overall$lower), -0.2, tolerance = 1e-8)
  expect_equal(unname(overall$upper), 0.2, tolerance = 1e-8)
  expect_identical(overall$classification, "unresolved strength difference")

  components <- extract_joint_asc_components_sdo_coefficient_differences(coefficients, aj_level)
  expect_equal(unname(components$posterior_median), rep(-0.2, 3), tolerance = 1e-8)
  expect_identical(unique(components$classification), "unresolved strength difference")
  expect_identical(unique(components$direction_comparison), "unresolved direction comparison")

  directions <- extract_joint_asc_components_sdo_directions(coefficients, aj_level)
  expect_identical(unique(directions$classification), "unresolved in direction")
})

test_that("an interval endpoint of exactly zero leaves the direction and the difference unresolved", {
  aggression <- c(0, 0, 1, 2, 3)
  coefficients <- aj_coefficients(
    aj_coefficient_draws(aggression, aggression, aggression, rep(0, 5))
  )
  directions <- extract_joint_asc_components_sdo_directions(coefficients, aj_level)
  aggression_row <- aj_row(directions, "outcome", "aggression")
  expect_equal(unname(aggression_row$lower), 0, tolerance = 1e-8)
  expect_identical(aggression_row$classification, "unresolved in direction")

  components <- extract_joint_asc_components_sdo_coefficient_differences(coefficients, aj_level)
  difference_row <- aj_row(components, "contrast", "|Aggression| - |SDO-D|")
  expect_equal(unname(difference_row$lower), 0, tolerance = 1e-8)
  expect_identical(difference_row$classification, "unresolved strength difference")
  expect_identical(difference_row$direction_comparison, "unresolved direction comparison")
})

# --- strength and sign, separately ------------------------------------------

# Five draws close to a central value: every interval keeps the sign of its
# centre.
aj_near <- function(centre) centre + c(-0.02, -0.01, 0, 0.01, 0.02)

test_that("a stronger negative association is a positive strength difference with a sign flip", {
  # Aggression is credibly negative and larger in size than the credibly
  # positive SDO-D coefficient: the signed difference would be about -0.4,
  # the strength difference is about +0.2.
  coefficients <- aj_coefficients(aj_coefficient_draws(
    aj_near(-0.3), aj_near(0.05), aj_near(0.05), aj_near(0.1)
  ))
  row <- aj_row(extract_joint_asc_components_sdo_coefficient_differences(coefficients, aj_level),
                "contrast", "|Aggression| - |SDO-D|")
  expect_equal(unname(row$posterior_median), 0.2, tolerance = 1e-8)
  expect_gt(row$lower, 0)
  expect_identical(row$classification, "credibly stronger for the first-named outcome")
  expect_identical(row$direction_comparison, "sign flip")
})

test_that("a sign flip can occur without a strength difference, and a shared direction with one", {
  coefficients <- aj_coefficients(aj_coefficient_draws(
    aj_near(-0.2), aj_near(0.4), aj_near(0.1), aj_near(0.2)
  ))
  components <- extract_joint_asc_components_sdo_coefficient_differences(coefficients, aj_level)
  flip <- aj_row(components, "contrast", "|Aggression| - |SDO-D|")
  # |-0.2 + e| - |0.2 + e'| straddles zero: no strength difference, opposite signs
  expect_identical(flip$classification, "unresolved strength difference")
  expect_identical(flip$direction_comparison, "sign flip")
  shared <- aj_row(components, "contrast", "|Submission| - |SDO-D|")
  expect_identical(shared$classification, "credibly stronger for the first-named outcome")
  expect_identical(shared$direction_comparison, "same direction")
  weaker <- aj_row(components, "contrast", "|Conventionalism| - |SDO-D|")
  expect_identical(weaker$classification, "credibly weaker for the first-named outcome")
  expect_identical(weaker$direction_comparison, "same direction")
})

test_that("the mean ASC summary averages the signed coefficients before taking the absolute value", {
  # Aggression +0.3 and conventionalism -0.3 cancel in the signed mean, so the
  # mean ASC coefficient is about 0.1 in size; averaging absolute values
  # would give about 0.3 and reverse the comparison with SDO-D (about 0.2).
  coefficients <- aj_coefficients(aj_coefficient_draws(
    aj_near(0.3), aj_near(0.3), aj_near(-0.3), aj_near(0.2)
  ))
  overall <- ap8_result_overall_asc_sdo_coefficient_differences(coefficients, aj_level)
  expect_equal(unname(overall$posterior_median), 0.1, tolerance = 1e-8)
  expect_identical(overall$classification, "credibly stronger for the first-named outcome")
  expect_identical(overall$direction_comparison, "same direction")
})

test_that("strength differences recomputed from the posterior draws match the summaries", {
  set.seed(20261004)
  n <- 4000L
  draws <- tibble::tibble(.draw = seq_len(n))
  for (m in seq_along(aj_motive_columns)) {
    for (r in seq_along(aj_response_ids)) {
      draws[[paste0("b_", aj_response_ids[[r]], "_", aj_motive_columns[[m]])]] <-
        stats::rnorm(n, mean = c(-0.15, 0.05, 0.2, -0.02, 0.1)[[m]] * c(1, -1, 0.5, 2)[[r]], sd = 0.06)
    }
  }
  coefficients <- ap8_shape_joint_coefficient_draws(draws, aj_response_ids, aj_motive_columns)
  within <- extract_joint_within_asc_coefficient_differences(coefficients, aj_level)
  components <- extract_joint_asc_components_sdo_coefficient_differences(coefficients, aj_level)
  overall <- ap8_result_overall_asc_sdo_coefficient_differences(coefficients, aj_level)
  by_hand <- function(motive, first, second) {
    b <- function(response) draws[[paste0("b_", aj_response_ids[[response]], "_", aj_motive_columns[[motive]])]]
    value <- function(name) if (name == "mean_asc") (b("aggression") + b("submission") + b("conventionalism")) / 3 else b(name)
    x <- abs(value(first)) - abs(value(second))
    c(stats::median(x), stats::quantile(x, c(0.025, 0.975), names = FALSE))
  }
  checks <- list(
    list(within, "zm_security", "|Aggression| - |submission|", "aggression", "submission"),
    list(within, "zm_power", "|Submission| - |conventionalism|", "submission", "conventionalism"),
    list(components, "zm_achievement", "|Conventionalism| - |SDO-D|", "conventionalism", "sdo_d"),
    list(components, "zm_prestige", "|Aggression| - |SDO-D|", "aggression", "sdo_d"),
    list(overall, "zm_arousal", "|SDO-D| - |mean ASC|", "sdo_d", "mean_asc")
  )
  for (check in checks) {
    row <- check[[1]][check[[1]]$motive == check[[2]] & check[[1]]$contrast == check[[3]], ]
    expect_equal(nrow(row), 1L)
    expect_equal(unname(c(row$posterior_median, row$lower, row$upper)),
                 by_hand(check[[2]], check[[4]], check[[5]]), tolerance = 1e-12,
                 info = paste(check[[2]], check[[3]]))
  }
})

# --- residual correlations ---------------------------------------------------

test_that("the six residual pairs resolve from either brms ordering", {
  forward <- aj_residuals()
  reversed <- aj_residuals(reversed = TRUE)
  pairs <- c("aggression_submission", "aggression_conventionalism", "submission_conventionalism",
             "aggression_sdo_d", "submission_sdo_d", "conventionalism_sdo_d")
  expect_identical(names(forward), c(".draw", pairs))
  expect_identical(forward, reversed)
  expect_equal(forward$aggression_submission, aj_within)
  expect_equal(forward$conventionalism_sdo_d, aj_across)
})

test_that("a residual posterior without exactly one ordering per pair is refused", {
  missing <- aj_residual_draws()
  missing$rescor__ascaggz__ascsubz <- NULL
  expect_error(
    ap8_shape_joint_residual_draws(missing, aj_response_ids),
    "exactly one ordering of residual pair aggression / submission",
    fixed = TRUE
  )
  doubled <- aj_residual_draws()
  doubled$rescor__ascsubz__ascaggz <- aj_within
  expect_error(
    ap8_shape_joint_residual_draws(doubled, aj_response_ids),
    "exactly one ordering of residual pair aggression / submission",
    fixed = TRUE
  )
})

test_that("the overall residual difference is the draw-wise difference of the two averages", {
  residuals <- aj_residuals()
  difference <- ap8_result_overall_asc_sdo_residual_difference(residuals, aj_level)
  expect_equal(nrow(difference), 1L)
  expected <- aj_within - aj_across
  expect_equal(unname(difference$posterior_median), stats::median(expected), tolerance = 1e-8)
  expect_equal(unname(difference$posterior_median), -0.045, tolerance = 1e-8)
  # Subtracting the two marginal medians would give 0.
  expect_equal(stats::median(aj_within) - stats::median(aj_across), 0, tolerance = 1e-8)
  expect_equal(unname(c(difference$lower, difference$upper)),
               unname(stats::quantile(expected, c(0.025, 0.975))), tolerance = 1e-8)
  expect_identical(difference$classification, "unresolved ordering")
  # the two averages themselves, summarised from the same draws (RQ2b)
  expect_equal(unname(c(difference$within_median, difference$within_lower, difference$within_upper)),
               c(stats::median(aj_within), stats::quantile(aj_within, c(0.025, 0.975), names = FALSE)),
               tolerance = 1e-8)
  expect_equal(unname(c(difference$between_median, difference$between_lower, difference$between_upper)),
               c(stats::median(aj_across), stats::quantile(aj_across, c(0.025, 0.975), names = FALSE)),
               tolerance = 1e-8)
})

test_that("the two descriptive residual tables describe three pairs and classify nothing", {
  residuals <- aj_residuals()
  within <- extract_joint_within_asc_residual_correlations(residuals, aj_level)
  components <- extract_joint_asc_components_sdo_residual_correlations(residuals, aj_level)
  expect_identical(names(within), c("pair", "posterior_median", "lower", "upper"))
  expect_identical(names(components), c("pair", "posterior_median", "lower", "upper"))
  expect_false("classification" %in% names(within))
  expect_false("classification" %in% names(components))
  expect_setequal(within$pair, c("aggression_submission", "aggression_conventionalism",
                                 "submission_conventionalism"))
  expect_setequal(components$pair, c("aggression_sdo_d", "submission_sdo_d",
                                     "conventionalism_sdo_d"))
  expect_equal(unname(within$posterior_median), rep(stats::median(aj_within), 3), tolerance = 1e-8)
  expect_equal(unname(components$posterior_median), rep(stats::median(aj_across), 3),
               tolerance = 1e-8)
})

# --- coefficient shaping -----------------------------------------------------

test_that("coefficient shaping names the coefficient parameter the posterior lacks", {
  draws <- aj_paired_draws()
  draws$b_ascconvz_zm_achievement_z <- NULL
  expect_error(
    ap8_shape_joint_coefficient_draws(draws, aj_response_ids, aj_one_motive),
    "Joint posterior lacks coefficient parameters: b_ascconvz_zm_achievement_z",
    fixed = TRUE
  )
})

test_that("coefficient shaping keeps one row per paired draw and motive", {
  coefficients <- ap8_shape_joint_coefficient_draws(
    aj_five_motive_draws(), aj_response_ids, aj_motive_columns
  )
  expect_identical(names(coefficients),
                   c(".draw", "motive", "aggression", "submission", "conventionalism", "sdo_d"))
  expect_equal(nrow(coefficients), 5L * length(aj_motive_columns))
  expect_setequal(coefficients$motive, names(aj_motive_columns))
  expect_identical(sort(unique(coefficients$.draw)), 1:5)
  achievement <- coefficients[coefficients$motive == "zm_achievement", ]
  expect_equal(achievement$sdo_d, seq_len(5) / 10 + 3 / 100 + 4 / 1000)
})

test_that("the joint posterior's motives are the plan's, by key and in the codebook's order", {
  # zm_config() puts the motives into the order of codebook_scales.csv; the
  # joint posterior takes them and their columns from there.
  expect_identical(ap8_joint_motive_columns(aj_cfg),
                   c(zm_security = "zm_security_z", zm_arousal = "zm_arousal_z", zm_power = "zm_power_z",
                     zm_prestige = "zm_prestige_z", zm_achievement = "zm_achievement_z"))
  reordered <- aj_cfg
  reordered$regression$motives <- rev(reordered$regression$motives)
  expect_identical(names(ap8_joint_motive_columns(reordered)), rev(names(ap8_joint_motive_columns(aj_cfg))))
  coefficients <- ap8_shape_joint_coefficient_draws(aj_five_motive_draws(), aj_response_ids,
                                                     ap8_joint_motive_columns(aj_cfg))
  expect_identical(unique(coefficients$motive), as.character(aj_cfg$regression$motives))
  broken <- aj_cfg
  broken$regression$motives <- c(broken$regression$motives, "zm_unknown")
  expect_error(ap8_joint_motive_columns(broken), "no standardised column for the motive(s): zm_unknown",
               fixed = TRUE)
})

# --- the validity record -----------------------------------------------------

test_that("the joint validity record applies the AP6 gate rule and nothing else", {
  ok <- ap8_extract_joint_fit_validity(aj_fit("ok"))
  expect_identical(ok, list(fit_valid = TRUE, gate_status = "ok", note = NA_character_))

  retried <- ap8_extract_joint_fit_validity(aj_fit("retried_ok"))
  expect_true(retried$fit_valid)
  expect_identical(retried$gate_status, "retried_ok")

  not_ok <- ap8_extract_joint_fit_validity(aj_fit("ok", ok = FALSE))
  expect_false(not_ok$fit_valid)
  expect_identical(not_ok$note, "Fit is not interpretable.")

  not_interpretable <- ap8_extract_joint_fit_validity(aj_fit("not_interpretable", ok = FALSE))
  expect_false(not_interpretable$fit_valid)
  expect_identical(not_interpretable$gate_status, "not_interpretable")
  expect_identical(not_interpretable$note, "Fit is not interpretable.")

  ungated <- ap8_extract_joint_fit_validity(aj_fit(gated = FALSE))
  expect_true(is.na(ungated$fit_valid))
  expect_identical(ungated$gate_status, "unavailable")
  expect_identical(ungated$note, "Final validity-gate result unavailable.")

  absent <- ap8_extract_joint_fit_validity(NULL)
  expect_false(absent$fit_valid)
  expect_identical(absent$gate_status, "unavailable")
  expect_identical(absent$note, "No joint fit available.")

  failed <- ap8_extract_joint_fit_validity(aj_error())
  expect_false(failed$fit_valid)
  expect_identical(failed$gate_status, "unavailable")
  expect_identical(failed$note, "the joint sampler did not start")
})

test_that("a failed joint fit yields the same tables with no rows", {
  posterior <- extract_joint_posterior(list(joint = aj_error()))
  expect_identical(names(posterior), c("coefficients", "residual_correlations", "validity"))
  expect_identical(names(posterior$coefficients),
                   c(".draw", "motive", "aggression", "submission", "conventionalism", "sdo_d"))
  expect_identical(names(posterior$residual_correlations),
                   c(".draw", "aggression_submission", "aggression_conventionalism",
                     "submission_conventionalism", "aggression_sdo_d", "submission_sdo_d",
                     "conventionalism_sdo_d"))
  expect_equal(nrow(posterior$coefficients), 0L)
  expect_equal(nrow(posterior$residual_correlations), 0L)
  expect_false(posterior$validity$fit_valid)
  expect_identical(posterior$validity$note, "the joint sampler did not start")
  # The same typed-empty tables for a joint fit that is simply not there.
  absent <- extract_joint_posterior(list(joint = NULL))
  expect_identical(names(absent$coefficients), names(posterior$coefficients))
  expect_identical(absent$validity$note, "No joint fit available.")
})

# --- withholding the classification -----------------------------------------

aj_package <- function() {
  coefficients <- aj_coefficients()
  residuals <- aj_residuals()
  list(
    direction = ap8_result_overall_asc_sdo_directions(coefficients, aj_level),
    differences = ap8_result_overall_asc_sdo_coefficient_differences(coefficients, aj_level),
    residuals = ap8_result_overall_asc_sdo_residual_difference(residuals, aj_level)
  )
}

test_that("a valid joint fit keeps every classification and gains its validity record", {
  package <- aj_package()
  validity <- ap8_extract_joint_fit_validity(aj_fit("ok"))
  kept <- withhold_classification_when_fit_invalid(package, validity)
  expect_identical(names(kept), c("direction", "differences", "residuals", "validity"))
  expect_identical(kept[c("direction", "differences", "residuals")], package)
  expect_identical(kept$validity, validity)
})

test_that("an invalid joint fit loses every classification and keeps every number", {
  package <- aj_package()
  validity <- ap8_extract_joint_fit_validity(aj_fit("not_interpretable", ok = FALSE))
  withheld <- withhold_classification_when_fit_invalid(package, validity)
  expect_identical(names(withheld), c("direction", "differences", "residuals", "validity"))
  for (element in c("direction", "differences", "residuals")) {
    expect_true(all(is.na(withheld[[element]]$classification)), info = element)
    expect_identical(withheld[[element]]$classification,
                     rep(NA_character_, nrow(package[[element]])), info = element)
    for (quantity in c("posterior_median", "lower", "upper")) {
      expect_identical(withheld[[element]][[quantity]], package[[element]][[quantity]],
                       info = paste(element, quantity))
    }
  }
  # the comparison of the two coefficients' signs is a classification too
  expect_false(anyNA(package$differences$direction_comparison))
  expect_identical(withheld$differences$direction_comparison,
                   rep(NA_character_, nrow(package$differences)))
  expect_identical(withheld$validity, validity)
})

test_that("the descriptive residual tables pass through the withholding verb unchanged", {
  coefficients <- aj_coefficients()
  residuals <- aj_residuals()
  package <- list(
    direction = extract_joint_within_asc_directions(coefficients, aj_level),
    differences = extract_joint_within_asc_coefficient_differences(coefficients, aj_level),
    residuals = extract_joint_within_asc_residual_correlations(residuals, aj_level)
  )
  validity <- ap8_extract_joint_fit_validity(NULL)
  withheld <- withhold_classification_when_fit_invalid(package, validity)
  expect_identical(withheld$residuals, package$residuals)
  expect_false("classification" %in% names(withheld$residuals))
  expect_true(all(is.na(withheld$direction$classification)))
  expect_true(all(is.na(withheld$differences$classification)))
})

# --- the shape of every summary ---------------------------------------------

test_that("every summary keeps one row per motive and outcome, contrast or pair", {
  coefficients <- ap8_shape_joint_coefficient_draws(
    aj_five_motive_draws(), aj_response_ids, aj_motive_columns
  )
  residuals <- aj_residuals()
  motives <- length(aj_motive_columns)
  direction_columns <- c("motive", "outcome", "posterior_median", "lower", "upper",
                         "classification")
  contrast_columns <- c("motive", "contrast", "first_outcome", "second_outcome",
                        "posterior_median", "lower", "upper", "classification",
                        "direction_comparison")

  overall_directions <- ap8_result_overall_asc_sdo_directions(coefficients, aj_level)
  expect_identical(names(overall_directions), direction_columns)
  expect_equal(nrow(overall_directions), motives * 2L)
  expect_setequal(overall_directions$outcome, c("mean_asc", "sdo_d"))

  within_directions <- extract_joint_within_asc_directions(coefficients, aj_level)
  expect_identical(names(within_directions), direction_columns)
  expect_equal(nrow(within_directions), motives * 3L)

  component_directions <- extract_joint_asc_components_sdo_directions(coefficients, aj_level)
  expect_identical(names(component_directions), direction_columns)
  expect_equal(nrow(component_directions), motives * 4L)

  overall_differences <- ap8_result_overall_asc_sdo_coefficient_differences(coefficients, aj_level)
  expect_identical(names(overall_differences), contrast_columns)
  expect_equal(nrow(overall_differences), motives)

  within_differences <- extract_joint_within_asc_coefficient_differences(coefficients, aj_level)
  expect_identical(names(within_differences), contrast_columns)
  expect_equal(nrow(within_differences), motives * 3L)

  component_differences <- extract_joint_asc_components_sdo_coefficient_differences(coefficients, aj_level)
  expect_identical(names(component_differences), contrast_columns)
  expect_equal(nrow(component_differences), motives * 3L)

  overall_residual <- ap8_result_overall_asc_sdo_residual_difference(residuals, aj_level)
  expect_identical(names(overall_residual),
                   c("posterior_median", "lower", "upper", "classification",
                     "within_median", "within_lower", "within_upper",
                     "between_median", "between_lower", "between_upper"))
  expect_equal(nrow(overall_residual), 1L)

  expect_equal(nrow(extract_joint_within_asc_residual_correlations(residuals, aj_level)), 3L)
  expect_equal(nrow(extract_joint_asc_components_sdo_residual_correlations(residuals, aj_level)), 3L)
})
