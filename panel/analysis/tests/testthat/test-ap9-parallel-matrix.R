local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) {
    parent <- dirname(dir)
    if (identical(parent, dir)) stop("analysis root not found")
    dir <- parent
  }
  source(file.path(dir, "R", "ap9_efa.R"), local = FALSE)
})

make_parallel_matrix_fixture <- function() {
  set.seed(419)
  data <- as.data.frame(matrix(stats::rnorm(80L * 4L), nrow = 80L))
  correlation <- matrix(.4, 4L, 4L)
  diag(correlation) <- 1
  list(
    set = list(name = "fractional", data = data, seed = 73L,
      correlation = list(ok = TRUE, value = correlation, note = NA_character_)),
    plan = list(factor_analysis = list(
      factor_number_criteria = list(
        list(key = "pca_parallel", index = "parallel_analysis", label = "PCA"),
        list(key = "common_factor_parallel", index = "common_factor_parallel", label = "PAF")),
      factor_number_methods = list(parallel_analysis = list(
        reference_datasets = 100L, percentile = 95,
        correlation_type = "polychoric", random_correlation_type = "pearson"))))
  )
}

test_that("fractional inputs use retained polychoric eigenvalues and the original N for both parallel analyses", {
  fixture <- make_parallel_matrix_fixture()
  set <- fixture$set
  before <- set$data
  expect_true(any(as.matrix(set$data) != round(as.matrix(set$data))))
  expect_gt(max(abs(cor(set$data) - set$correlation$value)), .1)
  result <- expect_message(
    apply_factor_number_methods(list(fractional = set), fixture$plan), NA
  )$fractional
  expect_identical(result$data, before)

  # Independent eigenvalue calculations, including the declared SMC diagonal.
  calculate_eigenvalues <- function(correlation, common_factors) {
    if (common_factors) diag(correlation) <- 1 - 1 / diag(solve(correlation))
    eigen(correlation, symmetric = TRUE, only.values = TRUE)$values
  }
  settings <- fixture$plan$factor_analysis$factor_number_methods$parallel_analysis
  for (method in c("pca_parallel", "common_factor_parallel")) {
    common_factors <- identical(method, "common_factor_parallel")
    observed <- result$factor_number_methods[[method]]
    expect_true(observed$ok)
    expect_equal(observed$value$eigenvalues$Real_Data,
                 calculate_eigenvalues(set$correlation$value, common_factors), tolerance = 1e-12)

    # Reproduce the Gaussian reference datasets directly from the supplied seed,
    # original participant count and configured number/percentile. No RAWPAR call
    # is used for this reference check.
    set.seed(set$seed)
    eigenvalues <- replicate(settings$reference_datasets, {
      reference <- matrix(stats::rnorm(nrow(set$data) * ncol(set$data)),
                          nrow = nrow(set$data), ncol = ncol(set$data))
      calculate_eigenvalues(stats::cor(reference), common_factors)
    })
    percentile_rank <- round(settings$percentile * settings$reference_datasets / 100)
    expected_percentiles <- apply(eigenvalues, 1L, function(x) sort(x)[percentile_rank])
    expect_equal(observed$value$eigenvalues$Mean, rowMeans(eigenvalues), tolerance = 1e-12)
    expect_equal(observed$value$eigenvalues$Percentile, expected_percentiles, tolerance = 1e-12)
  }
})

test_that("an unavailable retained matrix gives explicit unavailable parallel analyses", {
  fixture <- make_parallel_matrix_fixture()
  fixture$set$correlation <- list(ok = FALSE, value = NULL, note = "matrix estimation failed")
  result <- apply_factor_number_methods(list(fractional = fixture$set), fixture$plan)$fractional
  for (method in c("pca_parallel", "common_factor_parallel")) {
    observed <- result$factor_number_methods[[method]]
    expect_false(observed$ok)
    expect_null(observed$value)
    expect_match(observed$note, "requires an available retained polychoric matrix", fixed = TRUE)
  }
})
