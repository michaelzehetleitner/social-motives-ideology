local({
  root <- normalizePath(getwd())
  while (!file.exists(file.path(root, "config", "analysis_plan.yaml"))) {
    parent <- dirname(root)
    if (identical(root, parent)) stop("analysis root not found")
    root <- parent
  }
  for (file in c("ap10_inference.R", "ap7_joint_comparisons.R", "calculate_joint_correlation_changes.R")) {
    source(file.path(root, "R", file), local = FALSE)
  }
})

make_joint_change_arrays <- function(means, residual_correlation = 0) {
  if (is.matrix(means)) means <- array(means, c(1L, dim(means)))
  covariance <- array(0, c(dim(means)[1L], dim(means)[3L], dim(means)[3L]))
  for (draw in seq_len(dim(means)[1L])) {
    covariance[draw, , ] <- matrix(c(1, residual_correlation[1L + ((draw - 1L) %% length(residual_correlation))],
                                    residual_correlation[1L + ((draw - 1L) %% length(residual_correlation))], 1), 2)
  }
  list(expected = means, covariance = covariance)
}

test_that("empirical predictor covariance uses divisor N and retains correlated predictors", {
  # Perfectly correlated predictors and identity coefficients: each fitted
  # variance/covariance is 2/3 under three equal case weights. Adding residual
  # variance one gives correlation (2/3)/(5/3)=.4, not the N-1 result .5.
  fixture <- make_joint_change_arrays(cbind(c(-1, 0, 1), c(-1, 0, 1)))
  result <- derive_correlation_changes_from_draws(fixture$expected, fixture$covariance)
  expect_equal(result$unadjusted, .4, tolerance = 1e-14)
  expect_equal(result$residual, 0)
  expect_equal(result$difference, -.4, tolerance = 1e-14)
})

test_that("zero fitted variation leaves uncertain residual draws exactly paired", {
  fixture <- make_joint_change_arrays(array(0, c(5, 4, 2)), c(-.6, -.2, .1, .3, .8))
  result <- derive_correlation_changes_from_draws(fixture$expected, fixture$covariance, draw_ids = 11:15)
  expect_identical(result$.draw, 11:15)
  expect_equal(result$unadjusted, result$residual)
  expect_equal(result$difference, rep(0, 5))
  summary <- summarise_joint_correlation_changes(result, 4, list(fit_valid = TRUE), .8)
  expect_equal(summary$difference_lower, 0)
  expect_equal(summary$difference_upper, 0)
  expect_gt(summary$residual_upper - summary$residual_lower, 0)
})

test_that("negative associations preserve sign and the residual-minus-unadjusted order", {
  fixture <- make_joint_change_arrays(cbind(c(-1, 0, 1), c(1, 0, -1)), -.2)
  result <- derive_correlation_changes_from_draws(fixture$expected, fixture$covariance)
  expect_equal(result$unadjusted, -.52, tolerance = 1e-14)
  expect_equal(result$residual, -.2, tolerance = 1e-14)
  expect_equal(result$difference, .32, tolerance = 1e-14)
})

test_that("outcome rescaling and covariance-axis permutation preserve the estimand", {
  fixture <- make_joint_change_arrays(cbind(c(-1, 0, 1), c(-2, 1, 1)), .3)
  dimnames(fixture$expected) <- list(NULL, NULL, c("first", "second"))
  dimnames(fixture$covariance) <- list(NULL, c("first", "second"), c("first", "second"))
  original <- derive_correlation_changes_from_draws(fixture$expected, fixture$covariance)
  permuted <- derive_correlation_changes_from_draws(fixture$expected, fixture$covariance[, 2:1, 2:1, drop = FALSE])
  expect_equal(permuted, original)
  fixture$expected[, , 2] <- fixture$expected[, , 2] * 7
  fixture$covariance[1, , ] <- diag(c(1, 7)) %*% fixture$covariance[1, , ] %*% diag(c(1, 7))
  rescaled <- derive_correlation_changes_from_draws(fixture$expected, fixture$covariance)
  expect_equal(rescaled, original, tolerance = 1e-14)
})

test_that("six distinct outcome pairs are retained for every four-response draw", {
  expected <- array(0, c(3, 5, 4), dimnames = list(NULL, NULL, c("a", "b", "c", "d")))
  covariance <- array(0, c(3, 4, 4))
  for (draw in 1:3) covariance[draw, , ] <- diag(4)
  result <- derive_correlation_changes_from_draws(expected, covariance)
  expect_equal(nrow(result), 18L)
  expect_equal(unique(paste(result$outcome_1, result$outcome_2)), c("a b", "a c", "a d", "b c", "b d", "c d"))
  expect_equal(as.integer(table(result$.draw)), rep(6L, 3))
})

test_that("invalid fits keep numerical summaries without interpretations or classifications", {
  fixture <- make_joint_change_arrays(array(0, c(5, 4, 2)), c(-.6, -.2, .1, .3, .8))
  draws <- derive_correlation_changes_from_draws(fixture$expected, fixture$covariance)
  result <- summarise_joint_correlation_changes(draws, 4, list(fit_valid = FALSE))
  expect_false(result$interpretable)
  expect_true(is.finite(result$residual_median))
  expect_false(any(grepl("classif", names(result))))
  unavailable <- calculate_joint_correlation_changes(list(joint = simpleError("saved fit unavailable")))
  expect_equal(nrow(unavailable$summaries), 7L)
  expect_equal(nrow(unavailable$draws), 0L)
  expect_true(all(is.na(unavailable$summaries$difference_median)))
  expect_false(unavailable$validity$fit_valid)
  expect_match(unavailable$validity$note, "saved fit unavailable")
  expect_error(calculate_joint_correlation_changes(list(joint = NULL), interval_level = 1), "strictly between")
})

test_that("unsupported likelihoods and variance structures fail before predictions", {
  fit <- structure(list(family = list(a = list(family = "student", link = "identity"))), class = "brmsfit")
  expect_error(calculate_joint_correlation_changes(list(joint = fit)), "Gaussian")
  fit$family$a$family <- "gaussian"
  fit$formula <- list(rescor = TRUE, forms = list(a = list(pforms = list(sigma = ~x))))
  expect_error(calculate_joint_correlation_changes(list(joint = fit)), "constant residual")
  fit$formula$forms$a$pforms <- list()
  fit$ranef <- data.frame(group = "participant")
  expect_error(calculate_joint_correlation_changes(list(joint = fit)), "no multilevel")
})

test_that("malformed draws fail explicitly rather than becoming zero-valued changes", {
  fixture <- make_joint_change_arrays(cbind(c(-1, 0, 1), c(-1, 0, 1)))
  expect_error(derive_correlation_changes_from_draws(fixture$expected, array(1, c(2, 2, 2))), "matching")
  fixture$covariance[1, 1, 1] <- NA
  expect_error(derive_correlation_changes_from_draws(fixture$expected, fixture$covariance), "finite")
  fixture$covariance[1, , ] <- matrix(c(1, 2, 2, 1), 2)
  expect_error(derive_correlation_changes_from_draws(fixture$expected, fixture$covariance), "positive definite")
})


test_that("seven paired comparisons transform raw total covariance rather than average correlations", {
  ids <- c("ascaggz", "ascsubz", "ascconvz", "sdodomz")
  x <- cbind(c(-1, 0, 1), c(1, -1, 0), c(0, 1, -1), c(2, 0, -2))
  residual <- matrix(c(4, .4, .2, .5, .4, 1, .1, -.2,
                       .2, .1, 2, .3, .5, -.2, .3, 1), 4)
  expected <- array(rep(x, 2), c(3, 4, 2))
  expected <- aperm(expected, c(3, 1, 2))
  covariance <- array(NA_real_, c(2, 4, 4), dimnames = list(NULL, ids, ids))
  covariance[1, , ] <- residual
  covariance[2, , ] <- residual * 2
  dimnames(expected)[[3]] <- ids
  weights <- setNames(c(2, 1, 3, 0) / 3, ids)
  result <- derive_correlation_changes_from_draws(expected, covariance,
    draw_ids = c(14, 29), response_ids = ids, total_weights = weights)
  expect_equal(nrow(result), 14L)
  total <- result[result$outcome_1 == "asc_total", ]
  transform <- rbind(weights, c(0, 0, 0, 1))
  for (d in 1:2) {
    total_cov <- cov(x) * 2/3 + residual * d
    expect_equal(total$unadjusted[d], cov2cor(transform %*% total_cov %*% t(transform))[1, 2])
    expect_equal(total$residual[d], cov2cor(transform %*% (residual * d) %*% t(transform))[1, 2])
  }
  expect_equal(total$difference, total$residual - total$unadjusted)
  expect_identical(total$.draw, c(14, 29))
  expect_false(isTRUE(all.equal(total$residual[1], mean(cov2cor(residual)[1:3, 4]))))
  perm <- c(4, 2, 1, 3)
  reordered <- derive_correlation_changes_from_draws(expected, covariance[, perm, perm],
    draw_ids = c(14, 29), response_ids = ids, total_weights = weights[perm])
  expect_equal(reordered, result)
  invalid_weights <- weights; invalid_weights[4] <- 1
  expect_error(derive_correlation_changes_from_draws(expected, covariance,
    response_ids = ids, total_weights = invalid_weights), "Total weights")
  missing <- calculate_joint_correlation_changes(list(joint = NULL))
  expect_equal(nrow(missing$summaries), 7)
  expect_true(all(!missing$summaries$interpretable))
})
