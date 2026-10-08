# R/ap9_efa.R (the SRQ1 route) and the EFA tables built from it
# (R/report_supplement_measurement.R).
#
# Three fixtures, each as small as its question allows:
#   * the real configuration and codebook, for the hard-coded item-set
#     definition and for the source-label/executable-column boundary;
#   * two planted blocks of four items on 220 rows, for the estimation chain
#     (psych::fa only; the reference simulations of the factor-number methods
#     are cut to a handful of data sets, as the bodies read them from the
#     configuration);
#   * hand-written fit records, for the projections, where a real estimate
#     would only make the expected numbers harder to state.

local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) {
    parent <- dirname(dir)
    if (identical(parent, dir)) stop("project root not found from ", getwd())
    dir <- parent
  }
  for (f in c("config.R", "ap3_imputation_validity.R", "ap4_cfa_reporting.R", "ap4_reliability.R",
              "ap4_factor_structure.R", "ap9_efa.R", "ap9_pipeline.R", "report_supplement_measurement.R",
              "report_helpers.R")) {
    source(file.path(dir, "R", f), local = FALSE)
  }
  assign("efa_root", dir, envir = .GlobalEnv)
})

efa_cfg <- zm_config("full", file.path(efa_root, "config", "analysis_plan.yaml"))
efa_codebook <- zm_codebook(efa_cfg)

# One numeric column per codebook item, built from the enriched executable
# names. The frame carries no map: the codebook every consumer receives
# explicitly is the one label-to-column carrier.
efa_dummy_item_frame <- function(codebook, n = 12L) {
  columns <- unique(unlist(codebook$scales$item_codes))
  tibble::as_tibble(stats::setNames(
    lapply(seq_along(columns), function(i) as.numeric(seq_len(n) + i)), columns))
}

# --- the item-set definition against plan and codebook ------------------------

test_that("the hard-coded item mappings are the codebook's scales, item for item", {
  map <- zm_item_column_map(efa_codebook)
  sets <- define_efa_item_sets(efa_dummy_item_frame(efa_codebook), efa_cfg, efa_codebook)
  for (i in seq_len(nrow(efa_codebook$scales))) {
    key <- efa_codebook$scales$scale_key[i]
    expect_identical(sets[[key]]$item_membership$column,
                     as.character(efa_codebook$scales$item_codes[[i]]), info = key)
    expect_identical(unname(map[sets[[key]]$item_membership$item]),
                     sets[[key]]$item_membership$column, info = key)
    expect_identical(unique(sets[[key]]$item_membership$theoretical_scale), key)
  }
  # the nine individual sets are exactly the codebook's scales
  individual <- names(sets)[seq_len(nrow(efa_codebook$scales))]
  expect_setequal(individual, as.character(efa_codebook$scales$scale_key))
})

test_that("the five combined item sets are the configured factor-analytic sets", {
  sets <- define_efa_item_sets(efa_dummy_item_frame(efa_codebook), efa_cfg, efa_codebook)
  configured <- ap4_factor_sets(efa_cfg, "efa")
  expect_length(sets, nrow(efa_codebook$scales) + length(configured))
  for (key in names(configured)) {
    name <- key
    expect_true(name %in% names(sets), info = key)
    expect_setequal(unique(sets[[name]]$item_membership$theoretical_scale),
                    as.character(unlist(configured[[key]]$scales)))
    expected_items <- unlist(lapply(as.character(unlist(configured[[key]]$scales)), function(scale) {
      efa_codebook$scales$item_codes[[which(efa_codebook$scales$scale_key == scale)]]
    }), use.names = FALSE)
    expect_setequal(sets[[name]]$item_membership$column, expected_items)
  }
})

test_that("the theoretical factor counts are the configured expected counts", {
  theoretical <- ap4_theoretical_factor_numbers(efa_cfg)
  configured <- ap4_factor_sets(efa_cfg, "efa")
  expect_identical(nrow(theoretical), nrow(efa_codebook$scales) + length(configured))
  expect_true(all(as.character(theoretical$type) == "theoretical"))
  scales <- theoretical$n_factors[theoretical$item_set %in% efa_codebook$scales$scale_key]
  expect_true(all(scales == as.integer(efa_cfg$factor_analysis$single_scale_expected_factors)))
  for (key in names(configured)) {
    name <- key
    expect_identical(theoretical$n_factors[theoretical$item_set == name],
                     as.integer(configured[[key]]$expected_factors), info = key)
  }
})

# --- source labels versus executable columns ----------------------------------

test_that("the eight SDO items reach the item set as columns, in order, with their values", {
  frame <- efa_dummy_item_frame(efa_codebook)
  reversed <- as.character(efa_codebook$scales$reverse_items[[
    which(efa_codebook$scales$scale_key == "sdo_dom")]])
  set <- define_efa_item_sets(frame, efa_cfg, efa_codebook)$sdo_dom
  expect_identical(set$item_membership$item,
                   c("SDO-D_1", "SDO-D_2", "SDO-D_3", "SDO-D_4",
                     "SDO-D_5_r", "SDO-D_6_r", "SDO-D_7_r", "SDO-D_8_r"))
  expect_identical(set$item_membership$column,
                   c("SDO_D_1", "SDO_D_2", "SDO_D_3", "SDO_D_4",
                     "SDO_D_5_r", "SDO_D_6_r", "SDO_D_7_r", "SDO_D_8_r"))
  expect_identical(names(set$data), set$item_membership$column)
  for (column in set$item_membership$column) {
    expect_identical(set$data[[column]], frame[[column]], info = column)
  }
  # the reverse-keyed items are the four AP3 already reversed; the selection
  # neither reverses again nor drops the suffix that records it
  expect_identical(reversed, c("SDO_D_5_r", "SDO_D_6_r", "SDO_D_7_r", "SDO_D_8_r"))
  expect_true(all(reversed %in% set$item_membership$column))
})

# --- the estimation chain on two planted blocks ------------------------------

efa_block_frame <- function(seed = 20260915L, n = 220L) {
  set.seed(seed)
  first <- stats::rnorm(n)
  second <- stats::rnorm(n)
  categorise <- function(f, lambda = 0.8) {
    score <- lambda * f + sqrt(1 - lambda^2) * stats::rnorm(n)
    findInterval(as.numeric(scale(score)), c(-1.5, -0.75, 0, 0.75, 1.5)) + 1L
  }
  frame <- tibble::tibble(
    AA_1 = categorise(first), AA_2 = categorise(first),
    AA_3 = categorise(first), AA_4 = categorise(first),
    BB_1 = categorise(second), BB_2 = categorise(second),
    BB_3 = categorise(second), BB_4 = categorise(second)
  )
  frame
}

# The minimal codebook these planted blocks resolve through: source label to
# executable column, the same mapping the enriched codebook carries.
efa_block_codebook <- function(frame = efa_block_frame()) {
  list(items = tibble::tibble(item_code = names(frame),
                              source_label = sub("_", "-", names(frame))))
}

efa_block_sets <- function(frame = efa_block_frame()) {
  ap4_materialise_efa_item_sets(
    frame, efa_block_codebook(frame),
    item_sets = list(aa = paste0("AA-", 1:4), bb = paste0("BB-", 1:4)),
    set_definitions = list(aa = "aa", bb = "bb", both = c("aa", "bb")),
    seed = 1L
  )
}

# The bodies read every reference count from the configuration, so a cheap run
# is a configuration, not a different calculation.
efa_cheap_cfg <- function(analysis_plan = efa_cfg) {
  analysis_plan$factor_analysis$factor_number_methods$parallel_analysis$reference_datasets <- 5L
  analysis_plan$factor_analysis$factor_number_methods$comparison_data$population_size <- 500L
  analysis_plan$factor_analysis$factor_number_methods$comparison_data$samples <- 10L
  analysis_plan
}

test_that("every factor-number method reports a count for a two-block set", {
  cheap <- efa_cheap_cfg()
  results <- efa_block_sets() |>
    estimate_polychoric_correlations() |>
    apply_factor_number_methods(cheap)
  expect_true(all(vapply(results$both$factor_number_methods,
                         function(m) isTRUE(m$ok), logical(1))))
  counts <- attr(extract_suggested_factor_numbers(results),
                 "method_counts_by_item_set")$both
  expect_identical(counts$method, names(results$both$factor_number_methods))
  expect_identical(as.integer(counts$suggested_factors[counts$method == "classical_kaiser"]), 2L)
  expect_identical(as.integer(counts$suggested_factors[counts$method == "pca_parallel"]), 2L)
  expect_identical(as.integer(counts$suggested_factors[counts$method == "optimal_coordinates"]), 2L)
  expect_identical(as.integer(counts$suggested_factors[counts$method == "acceleration_factor"]), 2L)
  # the Kaiser count is EFAtools::KGC's eigenvalues-greater-than-one count of the
  # polychoric matrix; optimal coordinates and the acceleration factor keep
  # nFactors::nScree's complete result
  eigenvalues <- eigen(results$both$correlation$value, symmetric = TRUE, only.values = TRUE)$values
  expect_identical(as.integer(results$both$factor_number_methods$classical_kaiser$value$n_factors),
                   sum(eigenvalues > 1))
  for (method in c("optimal_coordinates", "acceleration_factor")) {
    scree <- results$both$factor_number_methods[[method]]$value
    expect_s3_class(scree, "nScree")
    expect_equal(scree$Analysis$Eigenvalues, eigenvalues, info = method)
  }
  # the polychoric matrix is estimated once and retained on the set
  expect_true(isTRUE(results$both$correlation$ok))
  expect_identical(dim(results$both$correlation$value), c(8L, 8L))
})

test_that("a method that fails keeps its note and does not erase the others", {
  captured <- ap4_capture_efa_result(stop("the method broke"))
  expect_false(captured$ok)
  expect_identical(captured$note, "the method broke")
  packed <- ap4_pack_suggested_factor_numbers(
    list(list(name = "aa", factor_number_methods = list(
      classical_kaiser = list(ok = TRUE, value = list(n_factors = 2L), note = NA_character_),
      empirical_kaiser = list(ok = FALSE, value = NULL, note = "the method broke"),
      original_map = list(ok = TRUE, value = list(NfactorsMAP = "not a number"), note = NA_character_)
    ))),
    suggested_count_field = c(classical_kaiser = "n_factors", empirical_kaiser = "n_factors",
                              original_map = "NfactorsMAP"),
    vss_suggested_count = function(vss) which.max(vss$cfit.1)
  )
  expect_identical(packed$n_factors, 2L)
  counts <- attr(packed, "method_counts_by_item_set")$aa
  expect_identical(counts$note[counts$method == "empirical_kaiser"], "the method broke")
  expect_identical(counts$note[counts$method == "original_map"],
                   "method did not provide one finite factor count")
  expect_true(is.na(counts$suggested_factors[counts$method == "original_map"]))
})

# --- every examined count in every requested rotation -------------------------

test_that("theoretical one and empirical two give an unrotated and one rotated fit", {
  sets <- efa_block_sets() |> estimate_polychoric_correlations()
  numbers <- tibble::tibble(
    item_set = c("both", "both"), n_factors = c(2L, 1L),
    type = factor(c("estimated", "theoretical"), levels = c("estimated", "theoretical"))
  )
  attr(numbers, "item_sets") <- sets
  attr(numbers, "method_counts_by_item_set") <- list(both = tibble::tibble(
    method = "classical_kaiser", suggested_factors = 2L, note = NA_character_))
  specifications <- add_efa_rotation_methods(
    numbers, one_factor = "none",
    multiple_factors = efa_cfg$factor_analysis$rotation_primary)
  expect_identical(
    vapply(specifications, function(s) paste(s$factors, s$rotation), character(1)),
    c("2 oblimin", "1 none"))
  expect_true(specifications[[2]]$expected_reference)
  expect_false(specifications[[1]]$expected_reference)
  expect_identical(specifications[[1]]$suggested_by, "classical_kaiser")

  fits <- fit_efa_models(specifications, efa_cfg)
  expect_true(all(vapply(fits, function(f) isTRUE(f$ok), logical(1))))
  expect_identical(ncol(fits[[1]]$loadings), 2L)
  expect_identical(ncol(fits[[2]]$loadings), 1L)
  expect_identical(rownames(fits[[1]]$loadings), names(sets$both$data))

  # a specification the correlation matrix cannot support keeps its identity
  broken <- specifications[[1]]
  broken$available <- FALSE
  broken$unavailable_reason <- "the polychoric matrix could not be estimated"
  failed <- ap4_pack_efa_solution(broken, ap4_capture_efa_fit(broken, stop("never evaluated")))
  expect_false(failed$ok)
  expect_identical(failed$note, "the polychoric matrix could not be estimated")
  expect_identical(failed$set_name, "both")
  expect_identical(failed$factors, 2L)
  expect_identical(failed$rotation, "oblimin")
  expect_null(failed$loadings)
})

test_that("every factor fit is reproducible: the plan's EFA seed, not the caller's RNG", {
  # psych::fa() rotates from n.rotations random starts and keeps the best. The
  # set's seed (the plan's factor_analysis seed) is set immediately before every
  # fit, so the twenty starts are reproducible and no outer RNG state can move
  # the result.
  sets <- efa_block_sets() |> estimate_polychoric_correlations()
  numbers <- tibble::tibble(
    item_set = "both", n_factors = 2L,
    type = factor("estimated", levels = c("estimated", "theoretical")))
  attr(numbers, "item_sets") <- sets
  attr(numbers, "method_counts_by_item_set") <- list(both = tibble::tibble(
    method = "classical_kaiser", suggested_factors = 2L, note = NA_character_))
  specifications <- add_efa_rotation_methods(
    numbers, one_factor = "none",
    multiple_factors = efa_cfg$factor_analysis$rotation_primary)
  expect_identical(vapply(specifications, function(s) s$rotation, character(1)),
                   "oblimin")

  first <- fit_efa_models(specifications, efa_cfg)
  second <- fit_efa_models(specifications, efa_cfg)
  expect_true(all(vapply(first, function(f) isTRUE(f$ok), logical(1))))
  for (i in seq_along(specifications)) {
    expect_identical(second[[i]]$loadings, first[[i]]$loadings,
                     info = specifications[[i]]$rotation)
  }

  # and two different outer RNG states give the identical loadings
  set.seed(101L)
  from_101 <- fit_efa_models(specifications, efa_cfg)
  set.seed(202L)
  from_202 <- fit_efa_models(specifications, efa_cfg)
  for (i in seq_along(specifications)) {
    expect_identical(from_101[[i]]$loadings, first[[i]]$loadings,
                     info = specifications[[i]]$rotation)
    expect_identical(from_202[[i]]$loadings, first[[i]]$loadings,
                     info = specifications[[i]]$rotation)
  }
})

test_that("VSS resets the item-set seed and preserves the caller's RNG state", {
  sets <- efa_block_sets() |> estimate_polychoric_correlations()
  cfg <- efa_cfg
  cfg$factor_analysis$factor_number_criteria <- tail(cfg$factor_analysis$factor_number_criteria, 1L)
  cfg$factor_analysis$factor_number_methods$vss$maximum_factors <- 3L
  set.seed(101L)
  state_101 <- .Random.seed
  first <- apply_factor_number_methods(sets["both"], cfg)$both$factor_number_methods$vss_complexity_1
  expect_identical(.Random.seed, state_101)
  set.seed(202L)
  state_202 <- .Random.seed
  second <- apply_factor_number_methods(sets["both"], cfg)$both$factor_number_methods$vss_complexity_1
  expect_identical(.Random.seed, state_202)
  expect_true(first$ok)
  expect_true(second$ok)
  expect_identical(first$value$cfit.1, second$value$cfit.1)
  expect_identical(first$value$vss.stats, second$value$vss.stats)
})

test_that("main EFA and VSS trial fits actually use the same gamma and starts", {
  calls <- new.env(parent = emptyenv())
  calls$gamma <- numeric()
  calls$starts <- numeric()
  calls$factors <- integer()
  calls$rotations <- character()
  suppressMessages(trace("oblimin", where = asNamespace("GPArotation"), print = FALSE,
    tracer = substitute({ assign("gamma", c(get("gamma", calls), gam), envir = calls) }, list(calls = calls))))
  on.exit(suppressMessages(untrace("oblimin", where = asNamespace("GPArotation"))), add = TRUE)
  suppressMessages(trace("faRotations", where = asNamespace("psych"), print = FALSE,
    tracer = substitute({ assign("starts", c(get("starts", calls), n.rotations), envir = calls) }, list(calls = calls))))
  on.exit(suppressMessages(untrace("faRotations", where = asNamespace("psych"))), add = TRUE)
  suppressMessages(trace("fa", where = asNamespace("psych"), print = FALSE,
    tracer = substitute({
      assign("factors", c(get("factors", calls), nfactors), envir = calls)
      assign("rotations", c(get("rotations", calls), rotate), envir = calls)
    }, list(calls = calls))))
  on.exit(suppressMessages(untrace("fa", where = asNamespace("psych"))), add = TRUE)

  sets <- efa_block_sets() |> estimate_polychoric_correlations()
  numbers <- tibble::tibble(item_set = c("both", "both"), n_factors = c(1L, 2L),
    type = factor(c("theoretical", "estimated"), levels = c("estimated", "theoretical")))
  attr(numbers, "item_sets") <- sets
  attr(numbers, "method_counts_by_item_set") <- list(both = tibble::tibble(
    method = "classical_kaiser", suggested_factors = 2L, note = NA_character_))
  specifications <- add_efa_rotation_methods(numbers, one_factor = "none",
    multiple_factors = efa_cfg$factor_analysis$rotation_primary)
  fits <- fit_efa_models(specifications, efa_cfg)
  expect_true(all(vapply(fits, function(f) isTRUE(f$ok), logical(1))))
  expect_identical(calls$rotations[calls$factors == 1L], "none")
  expect_equal(calls$starts, 20)
  expect_gt(length(calls$gamma), 0)
  expect_true(all(calls$gamma == 0))

  calls$gamma <- numeric()
  calls$starts <- numeric()
  calls$factors <- integer()
  calls$rotations <- character()
  cfg <- efa_cfg
  cfg$factor_analysis$factor_number_criteria <- tail(cfg$factor_analysis$factor_number_criteria, 1L)
  cfg$factor_analysis$factor_number_methods$vss$maximum_factors <- 3L
  result <- apply_factor_number_methods(sets["both"], cfg)$both$factor_number_methods$vss_complexity_1
  expect_true(result$ok)
  expect_equal(calls$factors, 1:3)
  expect_identical(calls$rotations, c("none", "oblimin", "oblimin"))
  expect_equal(calls$starts, c(20, 20))
  expect_gt(length(calls$gamma), 0)
  expect_true(all(calls$gamma == 0))
})

test_that("oblimin settings cannot silently diverge from the multistart implementation", {
  cfg <- efa_cfg
  expect_equal(ap4_require_efa_rotation_settings(cfg)$oblimin_gamma, 0)
  cfg$factor_analysis$rotation_settings$oblimin_gamma <- .1
  expect_error(ap4_require_efa_rotation_settings(cfg), "must be zero", fixed = TRUE)
  cfg$factor_analysis$rotation_settings$oblimin_gamma <- NULL
  expect_error(ap4_require_efa_rotation_settings(cfg), "must be zero", fixed = TRUE)
  cfg <- efa_cfg
  cfg$factor_analysis$rotation_settings$starting_rotations <- 1.5
  expect_error(ap4_require_efa_rotation_settings(cfg), "positive integer", fixed = TRUE)
})

test_that("a count the item set cannot support is specified with its reason, never capped", {
  sets <- efa_block_sets() |> estimate_polychoric_correlations()
  numbers <- tibble::tibble(
    item_set = c("aa", "aa"), n_factors = c(0L, 9L),
    type = factor("estimated", levels = c("estimated", "theoretical")))
  attr(numbers, "item_sets") <- sets
  attr(numbers, "method_counts_by_item_set") <- list(aa = tibble::tibble(
    method = character(), suggested_factors = integer(), note = character()))
  specifications <- add_efa_rotation_methods(
    numbers, one_factor = "none", multiple_factors = "oblimin")
  expect_identical(vapply(specifications, `[[`, integer(1), "factors"), c(0L, 9L))
  expect_true(all(!vapply(specifications, `[[`, logical(1), "available")))
  expect_true(all(vapply(specifications, `[[`, character(1), "unavailable_reason") ==
                    "The requested factor count is not estimable for this item set."))
})

# --- correspondence and loading clarity of one hand-written solution ----------

efa_one_factor_membership_fit <- function() {
  loadings <- matrix(
    c(0.80, 0.10,
      0.75, 0.15,
      0.70, 0.20,
      0.65, 0.05),
    nrow = 4, byrow = TRUE,
    dimnames = list(c("AA_1", "AA_2", "BB_1", "BB_2"), c("F1", "F2")))
  list(
    set = list(name = "both", item_membership = tibble::tibble(
      item = c("AA-1", "AA-2", "BB-1", "BB-2"),
      column = c("AA_1", "AA_2", "BB_1", "BB_2"),
      theoretical_scale = c("aa", "aa", "bb", "bb"))),
    set_name = "both", factors = 2L, rotation = "oblimin", ok = TRUE,
    note = NA_character_, loadings = loadings)
}

test_that("two theoretical groups estimated as one group give retention 1 and purity 1/3", {
  solution <- efa_one_factor_membership_fit()
  solution$membership <- ap4_extract_efa_membership(
    solution, ap4_assign_by_largest_absolute_loading(solution$loadings, ties = "first"))
  expect_true(all(solution$membership$assigned_factor == "F1"))
  result <- extract_efa_item_correspondence(list(solution))
  expect_equal(result$summaries$expected_pair_retention, 1)
  expect_equal(result$summaries$empirical_pair_purity, 1 / 3)
  expect_equal(result$summaries$adjusted_rand, 0)
})

test_that("loading clarity is described separately from the strongest-loading assignment", {
  solution <- efa_one_factor_membership_fit()
  solution$membership <- ap4_extract_efa_membership(
    solution, ap4_assign_by_largest_absolute_loading(solution$loadings, ties = "first"))
  clarity <- extract_efa_loading_clarity(
    list(solutions = list(solution), summaries = tibble::tibble()), efa_cfg)
  # every item is assigned to F1, but only the .40 rule decides "substantial"
  expect_identical(clarity$loading_items$assigned_factor, rep("F1", 4L))
  expect_identical(clarity$loading_items$n_loadings_at_40, rep(1L, 4L))
  expect_identical(clarity$loading_items$weak, rep(FALSE, 4L))
  expect_identical(clarity$loading_items$crossloading, rep(FALSE, 4L))
  expect_identical(clarity$loading_frequencies$n_items, 4L)
  expect_identical(clarity$loading_frequencies$n_secondary_loadings_at_40, 0L)
})

test_that("a loading exactly at the cutoff counts as substantial", {
  solution <- efa_one_factor_membership_fit()
  solution$loadings["AA_1", "F2"] <- -efa_cfg$factor_analysis$loading_display_cutoff
  solution$membership <- ap4_extract_efa_membership(
    solution, ap4_assign_by_largest_absolute_loading(solution$loadings, ties = "first"))
  clarity <- extract_efa_loading_clarity(
    list(solutions = list(solution), summaries = tibble::tibble()), efa_cfg)
  expect_identical(clarity$loading_items$n_loadings_at_40[[1]], 2L)
  expect_true(clarity$loading_items$crossloading[[1]])
  expect_identical(clarity$loading_items$n_secondary_loadings_at_40[[1]], 1L)
})

# --- hand-written fit records for the report tables ----------------------

efa_fixture_loadings <- function(k, items) {
  values <- seq(0.9, 0.3, length.out = length(items) * k)
  matrix(values, nrow = length(items), ncol = k,
         dimnames = list(items, paste0("WLS", seq_len(k))))
}

efa_fixture_fit <- function(item_set, factors, rotation, items, scales,
                            ok = TRUE, note = NA_character_, types = "estimated") {
  loadings <- if (ok) efa_fixture_loadings(factors, items) else NULL
  variance <- if (ok) {
    matrix(rep(c(2.1, 0.3, 0.3), times = factors), nrow = 3, ncol = factors,
           dimnames = list(c("SS loadings", "Proportion Var", "Cumulative Var"),
                           paste0("WLS", seq_len(factors))))
  } else NULL
  list(
    set = list(
      name = item_set,
      data = as.data.frame(stats::setNames(
        lapply(items, function(i) as.numeric(1:7)), items)),
      item_membership = tibble::tibble(
        item = sub("_", "-", items), column = items, theoretical_scale = scales),
      seed = 1L, correlation = list(ok = TRUE, value = diag(length(items)), note = NA_character_)),
    set_name = item_set, factors = as.integer(factors), rotation = rotation,
    types = types, expected_reference = "theoretical" %in% types,
    suggested_by = character(), available = TRUE, unavailable_reason = NA_character_,
    ok = ok, note = note,
    loadings = loadings,
    # an oblique multi-factor solution carries factor correlations; a
    # one-factor solution has none
    Phi = if (ok && factors > 1L) diag(1, factors) else NULL,
    Vaccounted = variance)
}

# One combined set ("both") whose theoretical count (4) is supported by no
# method: every method suggests 2 or 3. The route fits each of those
# criterion-suggested counts as well, with the primary rotation.
#
# `selected_ok = FALSE` fails the theoretical-count fit the primary record
# reads.
efa_projection_fixture <- function(selected_ok = TRUE) {
  items <- c("AA_1", "AA_2", "BB_1", "BB_2")
  scales <- c("aa", "aa", "bb", "bb")
  rotations <- "oblimin"
  fits <- unlist(lapply(c(2L, 3L, 4L), function(k) {
    lapply(rotations, function(rotation) {
      failed <- identical(rotation, "oblimin") && k == 4L && !selected_ok
      efa_fixture_fit("both", k, rotation, items, scales,
                      ok = !failed,
                      note = if (failed) "the four-factor solution did not converge" else NA_character_,
                      types = if (k == 4L) "theoretical" else "estimated")
    })
  }), recursive = FALSE)
  numbers <- tibble::tibble(
    item_set = rep("both", 3L), n_factors = c(2L, 3L, 4L),
    type = factor(c("estimated", "estimated", "theoretical"),
                  levels = c("estimated", "theoretical")))
  method_counts <- tibble::tibble(
    method = c("pca_parallel", "common_factor_parallel", "comparison_data",
               "original_map", "classical_kaiser", "empirical_kaiser",
               "optimal_coordinates", "acceleration_factor", "vss_complexity_1"),
    suggested_factors = c(2L, 2L, 3L, 2L, 2L, 2L, 3L, 2L, NA_integer_),
    note = c(rep(NA_character_, 8L), "method did not provide one finite factor count"))
  attr(numbers, "method_counts_by_item_set") <- list(both = method_counts)
  attr(numbers, "factor_number_results") <- list(both = list(classical_kaiser = list(ok = TRUE)))
  variance <- extract_efa_explained_variance(fits)
  list(fits = fits, numbers = numbers, variance = variance)
}

efa_projection_cfg <- function() {
  analysis_plan <- efa_cfg
  analysis_plan$factor_analysis$sets <- list(list(
    key = "both", label = "Both blocks", scales = c("aa", "bb"),
    expected_factors = 4L, efa = TRUE, cfa = FALSE))
  analysis_plan
}

# --- the set report table at a count no method suggested ----------------------

test_that("the report table selects the theoretical count no method suggested", {
  fixture <- efa_projection_fixture()
  analysis_plan <- efa_projection_cfg()
  records <- tabulate_efa_sets(fixture$fits, fixture$numbers,
                                fixture$variance, analysis_plan)
  record <- records$both
  expect_identical(names(records), "both")
  expect_identical(record$n_factors_used, 4L)
  expect_identical(record$rotation, "oblimin")
  expect_identical(ncol(record$loadings), 4L)
  expect_true(is.na(record$error))
  expect_identical(record$n_items, 4L)
  expect_identical(record$n, 7L)
  expect_identical(record$items$item, c("AA_1", "AA_2", "BB_1", "BB_2"))
  expect_identical(record$items$intended_scale, c("aa", "aa", "bb", "bb"))
  # no method suggested four factors, and the criteria are still all reported
  expect_identical(record$indices$index,
                   c("parallel_analysis", "common_factor_parallel", "comparison_data",
                     "map", "kaiser", "empirical_kaiser", "optimal_coordinates",
                     "acceleration_factor", "vss"))
  expect_false(any(record$indices$n_factors == 4L, na.rm = TRUE))
  expect_identical(record$indices$note[record$indices$index == "vss"],
                   "method did not provide one finite factor count")
  expect_identical(record$variance$factor, paste0("WLS", 1:4))
  expect_identical(record$variance$ss_loadings, rep(2.1, 4L))
})

test_that("the complete criteria and primary solutions stay attached to the projection", {
  fixture <- efa_projection_fixture()
  evidence <- attr(tabulate_efa_sets(fixture$fits, fixture$numbers,
                                      fixture$variance, efa_projection_cfg())$both,
                   "srq1_evidence")
  expect_identical(evidence$item_set, "both")
  # every fitted count, not only the projected one
  expect_identical(
    sort(unique(vapply(evidence$solutions, `[[`, integer(1), "factors"))), c(2L, 3L, 4L))
  expect_setequal(unique(vapply(evidence$solutions, `[[`, character(1), "rotation")),
                  "oblimin")
  expect_length(evidence$solutions, 3L)
  expect_identical(nrow(evidence$method_counts), 9L)
  expect_true(nrow(evidence$explained_variance) > 0L)
  # participant item values are not copied into the record
  expect_true(all(vapply(evidence$solutions,
                         function(s) is.null(s$set$data), logical(1))))
})

test_that("a failed theoretical-count fit becomes a typed unavailable record", {
  fixture <- efa_projection_fixture(selected_ok = FALSE)
  record <- tabulate_efa_sets(fixture$fits, fixture$numbers,
                               fixture$variance, efa_projection_cfg())$both
  expect_identical(record$n_factors_used, 4L)
  expect_identical(record$rotation, "oblimin")
  expect_null(record$loadings)
  expect_null(record$phi)
  expect_null(record$variance)
  expect_identical(record$error, "the four-factor solution did not converge")
  # the row shape is unchanged: items, counts and criteria are still there
  expect_identical(record$n_items, 4L)
  expect_identical(nrow(record$indices), 9L)
})

# --- the per-scale report table of a one-factor scale -------------------------

test_that("a one-factor scale projects the unrotated solution without rotation", {
  fit <- efa_fixture_fit("aa", 1L, "none", c("AA_1", "AA_2"), c("aa", "aa"),
                         types = "theoretical")
  fits <- list(fit)
  numbers <- tibble::tibble(item_set = "aa", n_factors = 1L,
                            type = factor("theoretical", levels = c("estimated", "theoretical")))
  attr(numbers, "method_counts_by_item_set") <- list(aa = tibble::tibble(
    method = "classical_kaiser", suggested_factors = 1L, note = NA_character_))
  attr(numbers, "factor_number_results") <- list(aa = list())
  variance <- extract_efa_explained_variance(fits)
  codebook <- list(scales = tibble::tibble(scale_key = "aa", label = "Block AA"))
  record <- tabulate_efa_scales(fits, numbers, variance, codebook, efa_cfg)$aa
  expect_identical(record$key, "aa")
  expect_identical(record$label, "Block AA")
  expect_identical(record$n_factors_used, 1L)
  expect_identical(record$rotation, "none")
  expect_identical(ncol(record$loadings), 1L)
  expect_identical(record$indices$index, "kaiser")
})

# --- the plan-driven factor-number criteria ----------------------------------

test_that("the plan registers the nine factor-number criteria in this order", {
  # the plan registers these nine criteria, keys in this order
  expect_identical(
    vapply(ap4_factor_number_criteria(efa_cfg), function(entry) as.character(entry$key),
           character(1)),
    c("pca_parallel", "common_factor_parallel", "comparison_data", "original_map",
      "classical_kaiser", "empirical_kaiser", "optimal_coordinates",
      "acceleration_factor", "vss_complexity_1"))
  # and their index names, which the reporting tables carry
  expect_identical(
    name_efa_indices(efa_cfg),
    c(pca_parallel = "parallel_analysis",
      common_factor_parallel = "common_factor_parallel",
      comparison_data = "comparison_data",
      original_map = "map",
      classical_kaiser = "kaiser",
      empirical_kaiser = "empirical_kaiser",
      optimal_coordinates = "optimal_coordinates",
      acceleration_factor = "acceleration_factor",
      vss_complexity_1 = "vss"))
})

test_that("the classical Kaiser criterion stops on a threshold other than KGC's one", {
  shifted <- efa_cfg
  shifted$factor_analysis$factor_number_methods$kaiser$threshold <- 0.7
  expect_error(apply_factor_number_methods(list(), shifted),
               "the plan's kaiser threshold must be 1", fixed = TRUE)
})

test_that("a criterion the route cannot compute stops it instead of being skipped", {
  broken <- efa_cfg
  broken$factor_analysis$factor_number_criteria <- c(
    broken$factor_analysis$factor_number_criteria,
    list(list(key = "not_a_criterion", index = "not_a_criterion", label = "Not a criterion")))
  set <- list(data = tibble::tibble(a = c(1, 2, 3), b = c(3, 2, 1)),
              correlation = list(value = diag(2)), seed = 1L)
  expect_error(
    apply_factor_number_methods(list(one = set), broken),
    "cannot compute: 'not_a_criterion'", fixed = TRUE)

  empty <- efa_cfg
  empty$factor_analysis$factor_number_criteria <- NULL
  expect_error(ap4_factor_number_criteria(empty), "states no factor_analysis$factor_number_criteria",
               fixed = TRUE)

  twice <- efa_cfg
  twice$factor_analysis$factor_number_criteria <- rep(
    efa_cfg$factor_analysis$factor_number_criteria[1], 2L)
  expect_error(ap4_factor_number_criteria(twice), "names a criterion twice", fixed = TRUE)
})
