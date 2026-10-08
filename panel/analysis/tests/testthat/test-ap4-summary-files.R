# AP4 — the sufficient summary statistics of the ordinal measurement models
# The outputs are the polychoric correlation matrix and item thresholds,
# written beside the pairwise-residual CSV so that a secondary analysis can
# refit any confirmatory model without the raw responses.

local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) {
    parent <- dirname(dir)
    if (identical(parent, dir)) stop("project root not found from ", getwd())
    dir <- parent
  }
  for (f in c("ap4_reliability.R", "ap4_summary_files.R")) {
    source(file.path(dir, "R", f), local = FALSE)
  }
})

# Four ordinal items on a 1..6 scale, two of them correlated by construction.
fixture_items <- function(n = 400L, seed = 11L) {
  set.seed(seed)
  latent <- stats::rnorm(n)
  cut6 <- function(z) as.integer(cut(z, breaks = c(-Inf, -1.2, -0.5, 0, 0.5, 1.2, Inf), labels = FALSE))
  data.frame(
    it1 = cut6(latent + stats::rnorm(n, sd = 0.6)),
    it2 = cut6(latent + stats::rnorm(n, sd = 0.6)),
    it3 = cut6(stats::rnorm(n)),
    it4 = cut6(stats::rnorm(n)),
    other = stats::rnorm(n)
  )
}

fixture_codebook <- function() {
  # As in the real codebook, `items` also lists fields outside every scale
  # (here `other`); `scales$item_codes` carries the measurement-model items.
  list(
    items = tibble::tibble(item_code = c("it1", "it2", "it3", "it4", "other")),
    scales = tibble::tibble(
      scale = c("a", "b"),
      item_codes = list(c("it1", "it2"), c("it3", "it4"))
    )
  )
}

fixture_cfg <- function(root) {
  list(
    root = root,
    factor_analysis = list(cfa_reporting = list(residual_data_file = "data/derived/cfa_pairwise_residuals.csv"))
  )
}

test_that("both files are written beside the residual CSV and carry the item order of the codebook", {
  root <- withr::local_tempdir()
  paths <- write_measurement_summary_files(fixture_items(), fixture_codebook(), fixture_cfg(root))
  expect_length(paths, 2L)
  expect_equal(basename(paths), c("cfa_polychoric_correlations.csv", "cfa_item_thresholds.csv"))
  expect_true(all(file.exists(paths)))
  expect_equal(normalizePath(dirname(paths[1])), normalizePath(file.path(root, "data", "derived")))

  corr <- readr::read_csv(paths[1], show_col_types = FALSE, progress = FALSE)
  expect_equal(names(corr), c("item", "it1", "it2", "it3", "it4"))
  expect_equal(corr$item, c("it1", "it2", "it3", "it4"))
  m <- as.matrix(corr[, -1])
  expect_equal(diag(m), rep(1, 4), tolerance = 1e-8)
  expect_equal(unname(m), unname(t(m)), tolerance = 1e-10)   # symmetric
  expect_true(all(abs(m) <= 1 + 1e-8))
  expect_gt(m[1, 2], m[3, 4])                        # the correlated pair is the larger one
})

test_that("the thresholds are long, finite and ordered within each item", {
  root <- withr::local_tempdir()
  paths <- write_measurement_summary_files(fixture_items(), fixture_codebook(), fixture_cfg(root))
  tau <- readr::read_csv(paths[2], show_col_types = FALSE, progress = FALSE)
  expect_equal(names(tau), c("item", "threshold", "value"))
  expect_setequal(unique(tau$item), c("it1", "it2", "it3", "it4"))
  expect_true(all(is.finite(tau$value)))             # the open ends of the scale are not shipped
  for (item in unique(tau$item)) {
    v <- tau$value[tau$item == item]
    expect_false(is.unsorted(v), info = item)        # thresholds increase within an item
    expect_lte(length(v), 5L)                        # at most k - 1 for a six-point item
  }
})

test_that("the item selection follows the codebook and refuses a degenerate item", {
  items <- fixture_items()
  # a column outside the codebook never enters the matrix
  expect_false("other" %in% names(ap4_summary_item_data(items, fixture_codebook())))
  expect_equal(names(ap4_summary_item_data(items, fixture_codebook())), c("it1", "it2", "it3", "it4"))
  # an item without variance cannot enter a polychoric matrix and says so
  flat <- items
  flat$it3 <- 3L
  expect_error(ap4_summary_item_data(flat, fixture_codebook()), "without variance")
  # fewer than two items is not a correlation matrix
  expect_error(
    ap4_summary_item_data(items, list(scales = tibble::tibble(scale = "a", item_codes = list("it1")))),
    "fewer than two numeric item columns"
  )
})

test_that("a missing residual-file setting stops instead of guessing a location", {
  root <- withr::local_tempdir()
  analysis_plan <- fixture_cfg(root)
  analysis_plan$factor_analysis$cfa_reporting$residual_data_file <- NULL
  expect_error(write_measurement_summary_files(fixture_items(), fixture_codebook(), analysis_plan),
               "residual_data_file is required")
})
