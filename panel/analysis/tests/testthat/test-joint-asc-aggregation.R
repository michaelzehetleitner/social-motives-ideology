# Arithmetic and provenance checks; no fits or targets-store writes.
local({
  root <- normalizePath(getwd())
  while (!file.exists(file.path(root, "config", "analysis_plan.yaml"))) {
    parent <- dirname(root)
    if (identical(root, parent)) stop("analysis root not found")
    root <- parent
  }
  for (file in c("ap10_inference.R", "ap7_joint_comparisons.R",
                 "calculate_joint_asc_aggregation.R")) {
    source(file.path(root, "R", file), local = FALSE)
  }
})

asc_aggregate_validity <- function(ok = TRUE) {
  list(fit_valid = ok, gate_status = if (isTRUE(ok)) "ok" else "not_interpretable",
       note = if (isTRUE(ok)) NA_character_ else "Fit is not interpretable.")
}

asc_aggregate_draws <- function() {
  tibble::tibble(.draw = 1:5, motive = "zm_achievement",
                 aggression = c(-1, 0, 1, 4, 10),
                 submission = c(3, -2, 0, 1, 2),
                 conventionalism = c(0, 1, 5, -1, -2))
}

asc_aggregate_sample <- function() {
  raw <- data.frame(asc_agg = c(1, 2, 4, 3, 5),
                    asc_sub = c(2, 3, 3, 5, 6),
                    asc_conv = c(1, 1, 2, 1, 4))
  data <- tibble::tibble(respondent_id = paste0("person-", 1:5),
                         gender = factor(c("female", "male", "female", "male", "female")))
  for (key in names(raw)) data[[paste0(key, "_z")]] <- as.numeric(scale(raw[[key]]))
  for (key in c("sdo_dom", "zm_security", "zm_arousal", "zm_achievement", "zm_power",
                "zm_prestige", "age", "income")) {
    data[[paste0(key, "_z")]] <- as.numeric(scale(1:5))
  }
  attr(data, "regression_participants") <- data$respondent_id
  attr(data, "z_parameters") <- tibble::tibble(
    var = names(raw), source = names(raw), transform = "identity",
    z_col = paste0(names(raw), "_z"), mean = vapply(raw, mean, numeric(1)),
    sd = vapply(raw, stats::sd, numeric(1)), n = nrow(raw))
  fit_data <- as.data.frame(data[setdiff(names(data), "respondent_id")])
  contrasts(fit_data$gender) <- stats::contr.treatment(2)
  list(raw = raw, data = data, fit_data = fit_data)
}

test_that("unequal facet SDs give an equal raw-facet aggregate, not mean ASC", {
  coefficients <- asc_aggregate_draws()
  facet_sd <- c(conventionalism = 3, aggression = 2, submission = 1)
  result <- calculate_joint_asc_aggregation_from_draws(
    coefficients, facet_sd, 1.7, .95, asc_aggregate_validity())
  gamma <- cbind(coefficients$aggression * 2, coefficients$submission,
                 coefficients$conventionalism * 3)
  expected <- rowMeans(gamma)
  expect_equal(result$draws$aggregate$gamma_aggregate, expected, tolerance = 0)
  expect_equal(result$draws$aggregate$b_aggregate, expected / 1.7, tolerance = 0)
  expect_false(isTRUE(all.equal(expected / 1.7,
    rowMeans(coefficients[c("aggression", "submission", "conventionalism")]))))
  expect_equal(result$aggregate_coefficients$gamma_aggregate_median, median(expected))
  expect_equal(result$aggregate_coefficients$b_aggregate_lower,
               unname(quantile(expected / 1.7, .025)))
  expect_equal(result$metadata$facet_weights, c(aggression = 1/3, submission = 1/3,
                                                conventionalism = 1/3))
})

test_that("magnitude and signed contrasts are calculated inside each paired draw", {
  coefficients <- asc_aggregate_draws()
  result <- calculate_joint_asc_aggregation_from_draws(
    coefficients, c(aggression = 2, submission = 1, conventionalism = 3),
    1.7, .8, asc_aggregate_validity())
  facet <- result$draws$facets[result$draws$facets$facet == "aggression", ]
  expected_signed <- coefficients$aggression - result$draws$aggregate$b_aggregate
  expected_magnitude <- abs(coefficients$aggression) - abs(result$draws$aggregate$b_aggregate)
  expect_identical(facet$.draw, coefficients$.draw)
  expect_equal(facet$signed_difference, expected_signed, tolerance = 0)
  expect_equal(facet$attenuation, expected_magnitude, tolerance = 0)
  summary <- result$facet_comparisons[result$facet_comparisons$facet == "aggression", ]
  expect_equal(summary$attenuation_median, median(expected_magnitude))
  expect_equal(summary$attenuation_lower, unname(quantile(expected_magnitude, .1)))
  expect_equal(summary$attenuation_upper, unname(quantile(expected_magnitude, .9)))
  expect_false(isTRUE(all.equal(summary$attenuation_median,
    abs(median(coefficients$aggression)) - abs(median(result$draws$aggregate$b_aggregate)))))
  expect_true(any(result$draws$facets$attenuation < 0))
})

test_that("all five motives and three facets retain all comparisons including zeros", {
  coefficients <- do.call(rbind, lapply(c("zm_security", "zm_arousal", "zm_power", "zm_prestige", "zm_achievement"), function(motive) {
    draws <- asc_aggregate_draws()
    draws$motive <- motive
    draws[c("aggression", "submission", "conventionalism")] <- 0
    draws
  }))
  result <- calculate_joint_asc_aggregation_from_draws(
    coefficients, c(aggression = 2, submission = 1, conventionalism = 3),
    1.7, .95, asc_aggregate_validity())
  expect_equal(nrow(result$aggregate_coefficients), 5L)
  expect_equal(nrow(result$facet_comparisons), 15L)
  expect_equal(nrow(result$draws$facets), 75L)
  expect_equal(result$aggregate_coefficients$b_aggregate_median, rep(0, 5))
  expect_equal(result$facet_comparisons$attenuation_lower, rep(0, 15))
  expect_equal(result$facet_comparisons$attenuation_upper, rep(0, 15))
  expect_true(result$interpretation_allowed)
})

test_that("invalid joint validity preserves numbers and withholds interpretation", {
  args <- list(asc_aggregate_draws(), c(aggression = 2, submission = 1, conventionalism = 3), 1.7, .95)
  good <- do.call(calculate_joint_asc_aggregation_from_draws, c(args, list(asc_aggregate_validity())))
  bad <- do.call(calculate_joint_asc_aggregation_from_draws, c(args, list(asc_aggregate_validity(FALSE))))
  expect_false(bad$interpretation_allowed)
  expect_identical(bad$aggregate_coefficients, good$aggregate_coefficients)
  numeric_columns <- vapply(good$facet_comparisons, is.numeric, logical(1))
  expect_identical(bad$facet_comparisons[numeric_columns], good$facet_comparisons[numeric_columns])
  expect_false(bad$validity$fit_valid)
  expect_true(all(is.na(bad$facet_comparisons$classification)))
  expect_true(all(is.na(bad$facet_comparisons$direction_comparison)))
  missing <- calculate_joint_asc_aggregation(list(joint = NULL), NULL, .95)
  expect_equal(nrow(missing$aggregate_coefficients), 0L)
  expect_equal(nrow(missing$facet_comparisons), 0L)
  expect_identical(names(missing$facet_comparisons), names(good$facet_comparisons))
  expect_false(missing$interpretation_allowed)
  expect_true(is.na(missing$metadata$aggregate_sd))
})

test_that("same-sample saved constants reconstruct raw facet scores and empirical aggregate SD", {
  fixture <- asc_aggregate_sample()
  sample <- reconstruct_joint_asc_aggregation_sample(fixture$fit_data, fixture$data)
  expect_equal(unname(sample$raw_facets), unname(as.matrix(fixture$raw)), tolerance = 1e-14)
  expected <- rowMeans(fixture$raw)
  expect_equal(sample$aggregate_scores, expected, tolerance = 1e-14)
  expect_equal(sample$aggregate_sd, sd(expected), tolerance = 1e-14)
  expect_false(isTRUE(all.equal(sample$aggregate_sd, mean(sample$facet_sd))))
})

test_that("row, participant and scaling mismatches are rejected before aggregation", {
  fixture <- asc_aggregate_sample()
  expect_error(reconstruct_joint_asc_aggregation_sample(fixture$fit_data[5:1, ], fixture$data), "ordered model column")
  other <- fixture$data
  attr(other, "regression_participants") <- rev(other$respondent_id)
  expect_error(reconstruct_joint_asc_aggregation_sample(fixture$fit_data, other), "participant order")
  other <- fixture$data
  attr(other, "z_parameters")$n <- 6L
  expect_error(reconstruct_joint_asc_aggregation_sample(fixture$fit_data, other), "joint sample")
  other <- fixture$data
  attr(other, "z_parameters")$sd[1] <- 0
  expect_error(reconstruct_joint_asc_aggregation_sample(fixture$fit_data, other), "joint sample")
  other <- fixture$data
  attr(other, "z_parameters") <- NULL
  expect_error(reconstruct_joint_asc_aggregation_sample(fixture$fit_data, other), "incomplete")
  fit_data <- fixture$fit_data
  fit_data$gender <- factor(rev(as.character(fit_data$gender)), levels = rev(levels(fit_data$gender)))
  expect_error(reconstruct_joint_asc_aggregation_sample(fit_data, fixture$data), "gender")
  other <- fixture$data
  other$asc_agg_z <- other$asc_agg_z * 2
  fit_data <- fixture$fit_data
  fit_data$asc_agg_z <- other$asc_agg_z
  expect_error(reconstruct_joint_asc_aggregation_sample(fit_data, other), "not standardised")
})

test_that("malformed SDs, intervals and unpaired coefficient draws fail clearly", {
  calculate <- function(draws = asc_aggregate_draws(), sds = c(aggression = 2, submission = 1, conventionalism = 3), total_sd = 1.7, level = .95) {
    calculate_joint_asc_aggregation_from_draws(draws, sds, total_sd, level, asc_aggregate_validity())
  }
  expect_error(calculate(total_sd = 0), "finite and positive")
  expect_error(calculate(sds = c(aggression = 2, submission = NA, conventionalism = 3)), "finite and positive")
  expect_error(calculate(level = 1), "strictly between")
  expect_error(calculate(level = NA_real_), "strictly between")
  draws <- asc_aggregate_draws()
  draws$.draw[2] <- draws$.draw[1]
  expect_error(calculate(draws), "unique paired")
  draws <- asc_aggregate_draws()
  draws$aggression[1] <- Inf
  expect_error(calculate(draws), "finite facets")
  draws <- asc_aggregate_draws()
  other <- draws[-1, ]
  other$motive <- "zm_power"
  expect_error(calculate(rbind(draws, other)), "same posterior draw ids")
})


test_that("standardised total strength and direction are separate comparisons", {
  coefficients <- tibble::tibble(.draw = 1:20, motive = "zm_achievement",
                                 aggression = -1, submission = 2, conventionalism = 2)
  flip <- calculate_joint_asc_aggregation_from_draws(coefficients,
    c(aggression = 1, submission = 1, conventionalism = 1), 1, .95, asc_aggregate_validity())
  row <- flip$facet_comparisons[flip$facet_comparisons$facet == "aggression", ]
  expect_equal(row$attenuation_median, 0)
  expect_identical(row$classification, "unresolved strength difference")
  expect_identical(row$direction_comparison, "sign flip")
  coefficients[c("aggression", "submission", "conventionalism")] <- .5
  stronger <- calculate_joint_asc_aggregation_from_draws(coefficients,
    c(aggression = 1, submission = 1, conventionalism = 1), .7, .95, asc_aggregate_validity())
  expect_true(all(stronger$facet_comparisons$attenuation_upper < 0))
  expect_true(all(stronger$facet_comparisons$direction_comparison == "same direction"))
})

test_that("a motive is named by its key, in the order the draws or the caller give", {
  keys <- c("zm_security", "zm_arousal", "zm_power", "zm_prestige", "zm_achievement")
  coefficients <- do.call(rbind, lapply(rev(keys), function(motive) {
    draws <- asc_aggregate_draws()
    draws$motive <- motive
    draws
  }))
  sds <- c(aggression = 2, submission = 1, conventionalism = 3)
  # by default in the order the draws bring them
  result <- calculate_joint_asc_aggregation_from_draws(coefficients, sds, 1.7, .95, asc_aggregate_validity())
  expect_identical(result$aggregate_coefficients$predictor_key, rev(keys))
  expect_identical(result$aggregate_coefficients$motive, result$aggregate_coefficients$predictor_key)
  # in the order of the plan's motives when the caller gives them
  ordered <- calculate_joint_asc_aggregation_from_draws(coefficients, sds, 1.7, .95, asc_aggregate_validity(),
                                                        motives = keys)
  expect_identical(ordered$aggregate_coefficients$predictor_key, keys)
  expect_identical(unique(ordered$facet_comparisons$predictor_key), keys)
  # a draw of a motive outside the given ones stops
  expect_error(calculate_joint_asc_aggregation_from_draws(coefficients, sds, 1.7, .95, asc_aggregate_validity(),
                                                          motives = keys[-1]),
               "known motives", fixed = TRUE)
})
