local({
  root <- normalizePath(getwd())
  while (!file.exists(file.path(root, "config", "analysis_plan.yaml"))) {
    parent <- dirname(root)
    if (identical(root, parent)) stop("analysis root not found")
    root <- parent
  }
  for (file in c("calculate_bayesian_correlations.R", "ap3_imputation_validity.R", "report_helpers.R",
                 "report_bayesian_correlations.R")) {
    source(file.path(root, "R", file), local = FALSE)
  }
})

make_correlation_fixture <- function() {
  keys <- c("zm_security", "zm_achievement", "zm_power", "zm_prestige", "zm_arousal",
            "asc_agg", "asc_sub", "asc_conv", "sdo_dom")
  data <- as.data.frame(matrix(1, nrow(iris), length(keys), dimnames = list(NULL, keys)))
  # The documented correlationBF() example, with the other seven scores
  # constant, permits an exact numerical comparison without 36 test samplers.
  data$zm_security <- iris$Sepal.Length
  data$zm_achievement <- iris$Sepal.Width
  list(data = data, codebook = list(scales = data.frame(scale_key = keys, label = keys)))
}

test_that("Bayes factor thresholds include boundaries and preserve missing values", {
  expect_identical(
    classify_correlation_evidence(c(-Inf, -log(3), -.1, 0, .1, log(3), Inf, NA_real_)),
    c("absent", "absent", "inconclusive", "inconclusive", "inconclusive", "present", "present", NA_character_))
  expect_identical(classify_correlation_evidence(log(c(.1, 1, 10)), bf_cutoff = 10),
                   c("absent", "inconclusive", "present"))
  expect_error(classify_correlation_evidence(0, 1), "greater than one")
})

test_that("invalid score data and settings fail clearly before dependency use", {
  fixture <- make_correlation_fixture()
  expect_error(calculate_bayesian_score_correlations(fixture$data[-1], fixture$codebook),
               "Missing score columns")
  bad <- fixture$data
  bad$zm_security <- as.character(bad$zm_security)
  expect_error(calculate_bayesian_score_correlations(bad, fixture$codebook), "numeric")
  expect_error(calculate_bayesian_score_correlations(fixture$data, fixture$codebook, rscale = 0),
               "positive finite")
  expect_error(calculate_bayesian_score_correlations(fixture$data, fixture$codebook, interval_level = 1),
               "strictly between")
  expect_error(calculate_bayesian_score_correlations(fixture$data, fixture$codebook, posterior_iterations = 2),
               "at least 100")
  expect_error(calculate_bayesian_score_correlations(fixture$data, fixture$codebook, seed = .Machine$integer.max),
               "room for all pair seeds")
  config <- list(descriptives = list(bayesian_correlations = list(package = "substitute")))
  expect_error(calculate_bayesian_score_correlations(fixture$data, fixture$codebook, config),
               "must be BayesFactor")
})

test_that("correlation evidence is rendered as superscripts including a digit zero", {
  categories <- c("present", "absent", "inconclusive", NA_character_)
  expect_identical(format_correlation_marker(categories, "html"), c("<sup>†</sup>", "<sup>0</sup>", "", ""))
  expect_identical(format_correlation_marker(categories, "markdown"), c("^†^", "^0^", "", ""))
  expect_identical(format_correlation_marker(categories, "plotmath"), c('^"†"', '^"0"', "", ""))
})

test_that("large and small Bayes factors follow the report's display rule instead of becoming Inf or zero", {
  # two decimals to 9.99, whole numbers to 100, then the
  # power of ten a value exceeds, mirrored below 0.01
  expect_identical(format_correlation_bf(c(log(3), log(.1), log(42.4))), c("3.00", "0.10", "42"))
  expect_identical(format_correlation_bf(c(log(1762), log(4.5e4), 1000, -1000)),
                   c("> 1,000", "> 10,000", "> 100,000", "< 0.00001"))
  expect_identical(format_correlation_bf(NA_real_), "—")
  expect_identical(format_correlation_bf(c(Inf, -Inf)), c("> 100,000", "< 0.00001"))
})

test_that("table and heatmap show the same posterior estimates and BF markers", {
  withr::local_options(sass.cache = FALSE)
  keys <- c("zm_security", "zm_achievement", "zm_power")
  result <- list(matrix = matrix(1, 3, 3, dimnames = list(keys, keys)),
                 pairs = data.frame(var_i = c(keys[1], keys[1], keys[2]),
                                    var_j = c(keys[2], keys[3], keys[3]),
                                    median = c(.24, -.02, .06),
                                    lower = c(.15, -.09, -.02), upper = c(.33, .05, .14),
                                    log_bf10 = log(c(20, .1, 1)),
                                    evidence = c("present", "absent", "inconclusive"), status = "ok"),
                 metadata = list(bf_cutoff = 3, interval_level = .95))
  table <- paste(as.character(build_bayesian_correlation_table(result, engine = "kable")), collapse = "\n")
  expect_match(table, ".24^†^", fixed = TRUE)
  expect_match(table, "−.02^0^", fixed = TRUE)
  expect_match(table, ".06", fixed = TRUE)
  figure <- plot_bayesian_correlation_heatmap(result)
  expect_equal(figure$data$median, result$pairs$median)
  expect_identical(figure$data$text, c('".24"^"†"', '"−.02"^"0"', '".06"'))
  # Force plotmath parsing and the complete draw to catch unsupported markers.
  expect_s3_class(ggplot2::ggplotGrob(figure), "gtable")
  html <- gt::as_raw_html(build_bayesian_correlation_table(result, engine = "gt"))
  expect_match(html, "<sup[^>]*>†</sup>")
  expect_match(html, "<sup[^>]*>0</sup>")
  details <- paste(as.character(build_bayesian_correlation_details(result, engine = "kable")), collapse = "\n")
  # estimate, evidence marker and interval in one cell
  expect_match(details, ".24^†^ [.15, .33]", fixed = TRUE)
  expect_match(details, "ρ [95% CrI]", fixed = TRUE)
  # the report's display rule for Bayes factors: whole numbers from 10 to 100
  expect_match(details, "|20 ", fixed = TRUE)
  expect_false(grepl("20.00", details, fixed = TRUE))
  expect_match(details, "estimated assuming a correlation exists", fixed = TRUE)
  expect_match(details, "evidence for no correlation", fixed = TRUE)
  expect_match(details, "practical equivalence", fixed = TRUE)
})

test_that("the documented iris example matches correlationBF medium exactly", {
  skip_if_not_installed("BayesFactor")
  fixture <- make_correlation_fixture()
  set.seed(431L)
  old_seed <- .Random.seed
  result <- suppressMessages(calculate_bayesian_score_correlations(
    fixture$data, fixture$codebook, posterior_iterations = 1000L, seed = 37L))
  expect_identical(.Random.seed, old_seed)
  direct <- BayesFactor::correlationBF(iris$Sepal.Length, iris$Sepal.Width, rscale = "medium")
  expect_equal(result$pairs$log_bf10[1],
               as.numeric(BayesFactor::extractBF(direct, logbf = TRUE, onlybf = TRUE)), tolerance = 1e-12)
  expect_equal(result$pairs$empirical_r[1], cor(iris$Sepal.Length, iris$Sepal.Width))
  expect_equal(result$metadata$prior_beta, c(3, 3))
  expect_equal(result$metadata$prior_support, c(-1, 1))
  expect_identical(result$metadata$package_version, utils::packageDescription("BayesFactor")$Version)
  expect_equal(result$pairs$bf10[1] * result$pairs$bf01[1], 1)
  expect_true(is.finite(result$pairs$median_mcse[1]))
  expect_gt(result$pairs$posterior_ess[1], 0)
  expect_equal(nrow(result$pairs), 36L)
  expect_equal(sum(result$pairs$status == "ok"), 1L)
  expect_true(all(is.na(result$pairs$bf10[result$pairs$status != "ok"])))
  repeat_result <- suppressMessages(calculate_bayesian_score_correlations(
    fixture$data, fixture$codebook, posterior_iterations = 1000L, seed = 37L))
  expect_identical(result, repeat_result)
})

test_that("configuration controls computation and pair seeds survive missingness", {
  skip_if_not_installed("BayesFactor")
  fixture <- make_correlation_fixture()
  fixture$data$zm_security[1:3] <- NA_real_
  settings <- list(package = "BayesFactor", rscale = .5, interval_level = .8,
                   posterior_iterations = 100L, seed = 98L, bf_cutoff = 4)
  result <- suppressMessages(calculate_bayesian_score_correlations(
    fixture$data, fixture$codebook,
    config = list(descriptives = list(bayesian_correlations = settings))))
  expect_equal(result$pairs$n[1], 147L)
  expect_equal(result$n[1, 2], 147L)
  expect_equal(result$metadata$rscale, .5)
  expect_equal(result$metadata$prior_beta, c(2, 2))
  expect_equal(result$metadata$interval_level, .8)
  expect_equal(result$metadata$bf_cutoff, 4)
  expect_equal(result$pairs$pair_seed, 98L + 0:35)
  expect_equal(result$pairs$posterior_iterations, rep(100L, 36))
  direct <- BayesFactor::correlationBF(iris$Sepal.Length[-(1:3)], iris$Sepal.Width[-(1:3)], rscale = .5)
  expect_equal(result$pairs$log_bf10[1],
               as.numeric(BayesFactor::extractBF(direct, logbf = TRUE, onlybf = TRUE)), tolerance = 1e-12)
})

test_that("short and perfectly correlated pairs are explicit non-estimates", {
  skip_if_not_installed("BayesFactor")
  fixture <- make_correlation_fixture()
  fixture$data$zm_achievement <- 2 * fixture$data$zm_security
  fixture$data$zm_power <- NA_real_
  fixture$data$zm_power[1:2] <- c(1, 2)
  result <- calculate_bayesian_score_correlations(fixture$data, fixture$codebook)
  expect_match(result$pairs$status[1], "Perfect empirical correlation")
  expect_match(result$pairs$status[2], "Fewer than three")
  expect_true(all(is.na(result$pairs$median)))
  expect_true(all(is.na(result$pairs$log_bf10)))
  expect_true(all(is.na(result$pairs$evidence)))
})
