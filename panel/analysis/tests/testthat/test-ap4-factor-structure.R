# R/ap4_factor_structure.R: the configured sets, the set CFA, and the
# polychoric threshold path the SRQ1 route depends on. The exploratory route
# itself is tested in test-ap9-efa.R.

local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) {
    parent <- dirname(dir)
    if (identical(parent, dir)) stop("project root not found from ", getwd())
    dir <- parent
  }
  for (f in c("config.R", "ap3_imputation_validity.R", "ap4_cfa_reporting.R", "ap4_reliability.R", "ap4_factor_structure.R",
              "ap9_efa.R")) {
    source(file.path(dir, "R", f), local = FALSE)
  }
})

# --- fixture -----------------------------------------------------------------

fs_cfg <- function(rotation_primary = "oblimin") {
  list(factor_analysis = list(
    loading_display_cutoff = 0.40,
    efa_estimation_method = "wls",
    # the nine criteria, as analysis_plan.yaml lists them
    factor_number_criteria = list(
      list(key = "pca_parallel", index = "parallel_analysis",
           label = "Parallel analysis (principal components)"),
      list(key = "common_factor_parallel", index = "common_factor_parallel",
           label = "Parallel analysis (common factors)"),
      list(key = "comparison_data", index = "comparison_data", label = "Comparison data"),
      list(key = "original_map", index = "map", label = "Velicer's minimum average partial"),
      list(key = "classical_kaiser", index = "kaiser", label = "Kaiser criterion"),
      list(key = "empirical_kaiser", index = "empirical_kaiser",
           label = "Empirical Kaiser criterion"),
      list(key = "optimal_coordinates", index = "optimal_coordinates",
           label = "Optimal coordinates"),
      list(key = "acceleration_factor", index = "acceleration_factor",
           label = "Acceleration factor"),
      list(key = "vss_complexity_1", index = "vss",
           label = "Very simple structure (complexity one)")),
    rotation_primary = rotation_primary,
    rotation_settings = list(oblimin_gamma = 0, starting_rotations = 20L),
    sets = list(
      list(key = "blocks", label = "Three planted blocks",
           scales = c("blk_a", "blk_b", "blk_c"), expected_factors = 3,
           efa = TRUE, cfa = TRUE),
      list(key = "single", label = "Block A alone",
           scales = c("blk_a"), expected_factors = 1, efa = TRUE, cfa = FALSE)
    )
  ))
}

fs_conf <- fs_cfg()
fs_set_blocks <- ap4_factor_sets(fs_conf)[["blocks"]]
fs_set_single <- ap4_factor_sets(fs_conf)[["single"]]

# These planted blocks are named by their executable column, so the codebook the
# item-set materialisation receives maps every label onto itself.
fs_identity_codebook <- function(columns) {
  list(items = tibble::tibble(item_code = columns, source_label = columns))
}

# --- set membership ----------------------------------------------------------

test_that("ap4_factor_sets reads the sets from the configuration by their flags", {
  expect_equal(names(ap4_factor_sets(fs_conf)), c("blocks", "single"))
  expect_equal(names(ap4_factor_sets(fs_conf, "efa")), c("blocks", "single"))
  expect_equal(names(ap4_factor_sets(fs_conf, "cfa")), "blocks")
  expect_error(ap4_factor_sets(list(factor_analysis = list(sets = NULL))), "sets")
})

# --- set CFA -----------------------------------------------------------------

test_that("CFA admissibility rejects an impossible latent correlation", {
  set.seed(3L)
  eta <- stats::rnorm(300L)
  x <- as.data.frame(replicate(6L,
    findInterval(eta + stats::rnorm(300L, sd = 0.7), c(-1.2, -0.6, 0, 0.6, 1.2)) + 1L
  ))
  names(x) <- paste0("i", seq_len(6L))
  syntax <- "f1 =~ i1+i2+i3\nf2 =~ i4+i5+i6"
  # the ordered WLSMV fit converges to a latent correlation above one; the
  # registered check names it and changes nothing
  fit <- suppressWarnings(lavaan::cfa(syntax, data = x, ordered = names(x),
                                      estimator = "WLSMV", std.lv = TRUE))
  expect_true(lavaan::lavInspect(fit, "converged"))
  rho <- lavaan::standardizedSolution(fit)
  rho <- rho$est.std[rho$op == "~~" & rho$lhs == "f1" & rho$rhs == "f2"]
  expect_equal(rho, 1.013411, tolerance = 1e-5)
  check <- ap4_check_cfa_admissibility(fit)
  expect_false(check$admissible)
  expect_match(check$note, "latent covariance matrix is not positive definite", fixed = TRUE)
})

# --- polychoric threshold path ------------------------------------------------
#
# `psych::polychoric()` can fail for blocks whose items do not all reach the
# highest response category. The cause is not specific to those items: the function
# abandons global threshold estimation as soon as the items of a block do not
# all reach the highest response category, and in that fallback it applies its
# continuity correction for empty cells to a table it has already normalised, so
# the cumulative marginals pass 1, the thresholds become NaN and the pairwise
# optimisation dies. `correct = 0` keeps psych on one estimator for every block.
#
# The fixture has the shape that triggers it: a low-endorsement scale on 1..6
# whose top category only a handful of respondents use, so some items miss it
# entirely. This is what any floor-effect scale looks like, real or synthetic.

fs_low_block <- function(n = 300L, seed = 20260906L, k = 8L, shift = -1.15) {
  set.seed(seed)
  f <- stats::rnorm(n)
  out <- lapply(seq_len(k), function(i) {
    z <- 0.7 * f + sqrt(1 - 0.7^2) * stats::rnorm(n)
    pmin(pmax(round(3.5 + shift + 1.05 * z), 1), 6)
  })
  names(out) <- paste0("low_", seq_len(k))
  as.data.frame(out)
}

fs_low_cb <- function(items) {
  list(
    items = tibble::tibble(item_code = names(items),
                           item_text_de = paste("Item", names(items))),
    scales = tibble::tibble(
      scale_key = "low", label = "Floor-effect scale", item_count = ncol(items),
      item_codes = list(names(items)), order = 1L
    )
  )
}

fs_low_set <- function(expected = 1L) {
  list(key = "low", label = "Floor-effect scale", scales = "low",
       expected_factors = expected, efa = TRUE, cfa = FALSE)
}

# The floor-effect block as the SRQ1 route receives it: one materialised item
# set with its retained polychoric matrix.
fs_low_sets <- function(x = fs_low_block()) {
  frame <- x
  ap4_materialise_efa_item_sets(
    frame, fs_identity_codebook(names(x)),
    list(low = names(x)), list(low = "low"), seed = 1L) |>
    estimate_polychoric_correlations()
}

fs_low_specification <- function(sets, factors, rotation = "oblimin") {
  set <- sets$low
  count_available <- factors >= 1L && factors < ncol(set$data)
  list(set = set, set_name = "low", factors = as.integer(factors), rotation = rotation,
       types = "theoretical", expected_reference = TRUE, suggested_by = character(),
       available = isTRUE(set$correlation$ok) && count_available,
       unavailable_reason = if (!isTRUE(set$correlation$ok)) set$correlation$note
                            else if (!count_available) "The requested factor count is not estimable for this item set."
                            else NA_character_)
}

# The items of a block that never received the highest category (6) of the
# six-point response scale.
fs_items_below_top <- function(x) {
  names(x)[vapply(x, function(v) max(v, na.rm = TRUE) < 6, logical(1))]
}

test_that("the well-formed floor-effect fixture leaves some items below the top category and defeats psych's default", {
  x <- fs_low_block()
  # every item is well formed: numeric, complete, with variance, five or six
  # categories -- so none of the usual degeneracy checks would catch it
  expect_true(all(vapply(x, is.numeric, logical(1))))
  expect_equal(sum(stats::complete.cases(x)), nrow(x))
  expect_true(all(vapply(x, function(v) stats::sd(v) > 0.5, logical(1))))
  expect_true(all(vapply(x, function(v) length(unique(v)) >= 5L, logical(1))))
  # and yet some items never reach the top category, and some do
  low <- fs_items_below_top(x)
  expect_gt(length(low), 0L)
  expect_lt(length(low), ncol(x))
  # psych's own default cannot do this block at all
  expect_error(suppressMessages(suppressWarnings(
    psych::fa(x, nfactors = 1, rotate = "oblimin", fm = "wls", cor = "poly")
  )))
})

test_that("a block whose items do not all reach the top category still gets a solution", {
  x <- fs_low_block()
  # the production bodies pass psych's own diagnostics through instead of
  # suppressing them, so the fallback notice is visible in the run log
  sets <- expect_warning(fs_low_sets(x), "global set to FALSE", fixed = TRUE)
  # the estimator keeps psych on one threshold path (`correct = 0`)
  expect_true(isTRUE(sets$low$correlation$ok))
  expect_true(is.na(sets$low$correlation$note))
  expect_identical(dim(sets$low$correlation$value), c(ncol(x), ncol(x)))

  fits <- fit_efa_models(
    lapply(c("none", "oblimin"), function(rotation) {
      fs_low_specification(sets, if (identical(rotation, "none")) 1L else 2L, rotation)
    }), fs_conf)
  expect_true(all(vapply(fits, function(f) isTRUE(f$ok), logical(1))))
  expect_true(all(is.na(vapply(fits, function(f) f$note, character(1)))))
  expect_equal(nrow(fits[[1]]$loadings), ncol(x))
  expect_true(all(fits[[1]]$loadings[, 1] > 0.4))
  # every factor-number method answers on this block, none with a matrix failure
  cheap <- fs_conf
  cheap$factor_analysis$factor_number_methods <- list(
    parallel_analysis = list(reference_datasets = 5L, percentile = 95,
                             correlation_type = "polychoric",
                             random_correlation_type = "pearson"),
    comparison_data = list(population_size = 500L, samples = 10L, alpha = 0.3,
                           max_iterations = 50L, correlation_type = "pearson"),
    kaiser = list(threshold = 1),
    vss = list(maximum_factors = 8L))
  cheap$factor_analysis$efa_estimation_method <- "wls"
  counts <- attr(extract_suggested_factor_numbers(
    apply_factor_number_methods(sets, cheap)), "method_counts_by_item_set")$low
  expect_true(all(is.na(counts$note)))
  expect_false(anyNA(counts$suggested_factors))
})

test_that("the solution equals psych's own global threshold path where that path runs", {
  # Where every item reaches the top category, psych does not enter the fallback
  # and `correct` is not read; the estimator must therefore remain identical.
  x <- fs_low_block(shift = 0)
  expect_equal(fs_items_below_top(x), character(0))
  sets <- fs_low_sets(x)
  ours <- fit_efa_models(list(fs_low_specification(sets, 1L, "none")), fs_conf)[[1]]
  psychs <- suppressMessages(suppressWarnings(
    psych::fa(x, nfactors = 1, rotate = "none", fm = "wls", cor = "poly")
  ))
  expect_true(isTRUE(ours$ok))
  expect_equal(unname(ours$loadings), unname(unclass(psychs$loadings)[, , drop = FALSE]))
})

test_that("an unavailable solution keeps its item set, count and rotation on the record", {
  # The record carries the identity in fields rather than in a sentence: the
  # reason names the requested count, the record names the set, the count and
  # the rotation it was requested in.
  x <- fs_low_block(shift = 0)
  sets <- fs_low_sets(x)
  fits <- fit_efa_models(
    lapply("oblimin", function(rotation) {
      fs_low_specification(sets, ncol(x) + 1L, rotation)
    }), fs_conf)
  for (fit in fits) {
    expect_false(isTRUE(fit$ok))
    expect_null(fit$loadings)
    expect_identical(fit$set_name, "low")
    expect_identical(fit$factors, as.integer(ncol(x) + 1L))
    expect_identical(fit$note, "The requested factor count is not estimable for this item set.")
  }
  expect_identical(vapply(fits, `[[`, character(1), "rotation"),
                   "oblimin")
  # the variance extractor keeps one explicit unavailable row per requested fit
  variance <- extract_efa_explained_variance(fits)
  expect_identical(nrow(variance), 1L)
  expect_true(all(is.na(variance$factor)))
  expect_true(all(variance$note == "The requested factor count is not estimable for this item set."))
})
