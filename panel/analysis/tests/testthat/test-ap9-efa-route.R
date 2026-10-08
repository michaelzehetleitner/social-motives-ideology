# The SRQ1 EFA chain as the pipeline runs it.
#
# Every object below is produced by the actual `_targets.R` commands, read
# through the route reader of tests/support/targets-route.R on the synthetic
# export: real `psych::polychoric`, real `psych::fa`, real factor-number
# criteria. Nothing is hand-built and no fit is substituted.
#
# Every EFA target runs end to end, from the prepared table to the report
# tables. The rotation specifications and the correspondence measures are
# asserted here on real fits; test-ap9-efa.R keeps their small hand-written
# statements.
#
# The ONLY departure from the registered configuration is cost: the reference
# simulations of parallel analysis and comparison data are cut, exactly as
# `efa_cheap_cfg()` in test-ap9-efa.R cuts them. Item sets, theoretical counts,
# rotations, the estimation method and the .40 cutoff are the registered ones.

local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) {
    parent <- dirname(dir)
    if (identical(parent, dir)) stop("project root not found from ", getwd())
    dir <- parent
  }
  for (f in c("config.R", "io_qualtrics.R", "ap1_exclusions.R", "ap3_preprocessing.R",
              "ap3_preparation.R", "ap3_fill.R", "ap3_data_files.R", "ap3_pipeline.R",
              "ap4_factor_structure.R", "ap9_efa.R", "ap9_pipeline.R", "report_supplement_measurement.R",
              "report_helpers.R")) {
    source(file.path(dir, "R", f), local = FALSE)
  }
  source(file.path(dir, "tests", "support", "targets-route.R"), local = FALSE)
  assign("er_root", dir, envir = .GlobalEnv)
})

# The bodies read every reference count from the configuration, so a cheap run
# is a configuration, not a different calculation.
er_cfg <- local({
  analysis_plan <- zm_config("full", file.path(er_root, "config", "analysis_plan.yaml"))
  analysis_plan$factor_analysis$factor_number_methods$parallel_analysis$reference_datasets <- 5L
  analysis_plan$factor_analysis$factor_number_methods$comparison_data$population_size <- 500L
  analysis_plan$factor_analysis$factor_number_methods$comparison_data$samples <- 10L
  analysis_plan
})
er_book <- zm_codebook(er_cfg)
er_cutoff <- er_cfg$factor_analysis$loading_display_cutoff
er_primary <- as.character(er_cfg$factor_analysis$rotation_primary)

# The route, and every EFA target forced once. psych's own threshold notice and
# EFAtools' progress messages are held back here only so the run log stays
# readable; the production bodies still emit them (test-ap4-factor-structure.R
# asserts that notice in "a block whose items do not all reach the top category
# still gets a solution").
er <- local({
  raw <- read_qualtrics_export(
    file.path(er_root, "data", "synthetic", "zm_panel_synthetic.sav"))
  route <- suppressWarnings(suppressMessages(
    tr_route(er_root, tr_intake(raw, er_cfg), er_cfg, er_book)))
  names <- c("data_descriptive_reliability", "efa_item_sets",
             "efa_factor_number_results", "efa_factor_numbers",
             "efa_rotation_specifications", "efa_fits", "efa_item_assignments",
             "efa_item_correspondence", "efa_loading_clarity",
             "efa_explained_variance")
  values <- lapply(names, function(name) {
    suppressWarnings(suppressMessages(route(name)))
  })
  values <- stats::setNames(values, names)
  # The tables the S1 target builds from these results.
  results <- list(values$efa_fits, values$efa_factor_numbers,
                  values$efa_explained_variance)
  values$efa_scales_table <- do.call(tabulate_efa_scales, c(results, list(er_book, er_cfg)))
  values$efa_sets_table <- do.call(tabulate_efa_sets, c(results, list(er_cfg)))
  values$correspondence_table <- tabulate_efa_correspondence(values$efa_loading_clarity, er_cfg)
  values
})

# --- what the registration says, derived here from plan and codebook ---------

er_scale_keys <- as.character(er_book$scales$scale_key)
er_scale_columns <- function(scale) {
  as.character(er_book$scales$item_codes[[match(scale, er_scale_keys)]])
}
# A combined set is named by its configured key.
er_efa_sets <- ap4_factor_sets(er_cfg, "efa")
er_combined_keys <- names(er_efa_sets)
er_expected_factors <- c(
  stats::setNames(rep(as.integer(er_cfg$factor_analysis$single_scale_expected_factors),
                      length(er_scale_keys)), er_scale_keys),
  stats::setNames(vapply(er_efa_sets, function(s) as.integer(s$expected_factors), integer(1)),
                  er_combined_keys))
er_methods <- names(name_efa_indices(er_cfg))
er_rotations_for <- function(k) if (k == 1L) "none" else er_primary
er_fit_id <- function(x) paste(x$set_name, x$factors, x$rotation)

# The strongest loading per item, computed without the production rule.
er_argmax <- function(loadings) {
  apply(abs(loadings), 1L, function(row) which(row == max(row))[[1]])
}
# The two directional pair measures and adjusted Rand, recomputed from scratch.
er_pair_measures <- function(theoretical, assigned) {
  n <- length(theoretical)
  i <- rep(seq_len(n), each = n)
  j <- rep(seq_len(n), times = n)
  upper <- i < j
  i <- i[upper]; j <- j[upper]
  same_scale <- theoretical[i] == theoretical[j]
  same_factor <- assigned[i] == assigned[j]
  list(retention = mean(same_factor[same_scale]),
       purity = mean(same_scale[same_factor]),
       rand = mclust::adjustedRandIndex(theoretical, assigned))
}

# --- data_descriptive_reliability -> the fourteen item sets -------------------

test_that("the nine scale item sets are materialised from the route's own prepared table", {
  data <- er$data_descriptive_reliability
  sets <- er$efa_item_sets
  expect_length(sets, length(er_scale_keys) + length(er_combined_keys))
  expect_identical(names(sets)[seq_along(er_scale_keys)], er_scale_keys)
  for (key in er_scale_keys) {
    set <- sets[[key]]
    columns <- er_scale_columns(key)
    # item identity: the codebook's executable columns, in order, `_r` intact
    expect_identical(names(set$data), columns, info = key)
    expect_identical(set$item_membership$column, columns, info = key)
    expect_identical(unname(zm_item_column_map(er_book)[set$item_membership$item]),
                     columns, info = key)
    expect_identical(unique(set$item_membership$theoretical_scale), key, info = key)
    # participant identity and values: the route's own table, untouched
    expect_identical(nrow(set$data), nrow(data), info = key)
    for (column in columns) {
      expect_identical(set$data[[column]], data[[column]], info = paste(key, column))
    }
    # the seed is the plan's factor_analysis.seed
    expect_identical(set$seed, as.integer(er_cfg$factor_analysis$seed), info = key)
  }
  # the eight SDO-D items with their reversals
  expect_identical(names(sets$sdo_dom$data),
                   c("SDO_D_1", "SDO_D_2", "SDO_D_3", "SDO_D_4",
                     "SDO_D_5_r", "SDO_D_6_r", "SDO_D_7_r", "SDO_D_8_r"))
})

test_that("each combined item set is its member scales' codebook columns, block by block", {
  data <- er$data_descriptive_reliability
  sets <- er$efa_item_sets
  expect_setequal(names(sets)[-seq_along(er_scale_keys)], er_combined_keys)
  for (key in er_combined_keys) {
    set <- sets[[key]]
    expect_identical(names(set$data), set$item_membership$column, info = key)
    membership <- set$item_membership$theoretical_scale
    # the member scales arrive as contiguous blocks, each in codebook order
    blocks <- rle(membership)$values
    expect_false(anyDuplicated(blocks) > 0L, info = key)
    # the blocks are the plan's scales of that set, in the plan's order
    expect_identical(blocks, as.character(er_efa_sets[[key]]$scales), info = key)
    for (scale in blocks) {
      expect_identical(set$item_membership$column[membership == scale],
                       er_scale_columns(scale), info = paste(key, scale))
    }
    expect_identical(names(set$data),
                     unlist(lapply(blocks, er_scale_columns), use.names = FALSE), info = key)
    expect_identical(nrow(set$data), nrow(data), info = key)
    expect_identical(as.data.frame(set$data),
                     as.data.frame(data[, names(set$data), drop = FALSE]), info = key)
    expect_identical(set$seed, as.integer(er_cfg$factor_analysis$seed), info = key)
  }
  # the SDO-D block of the combined outcome set keeps its reverse-key suffixes
  expect_true(all(c("SDO_D_5_r", "SDO_D_8_r") %in% names(sets$outcomes$data)))
})

# --- polychoric record -> factor-number methods -> extracted counts -----------

test_that("every item set carries one polychoric record and one count per method", {
  results <- er$efa_factor_number_results
  counts <- attr(er$efa_factor_numbers, "method_counts_by_item_set")
  expect_identical(names(results), names(er$efa_item_sets))
  expect_identical(names(counts), names(er$efa_item_sets))
  for (name in names(results)) {
    set <- results[[name]]
    columns <- names(set$data)
    expect_true(isTRUE(set$correlation$ok), info = name)
    expect_identical(dim(set$correlation$value),
                     c(length(columns), length(columns)), info = name)
    expect_identical(rownames(set$correlation$value), columns, info = name)
    expect_identical(names(set$factor_number_methods), er_methods, info = name)
    table <- counts[[name]]
    expect_identical(table$method, er_methods, info = name)
    expect_true(is.integer(table$suggested_factors), info = name)
    # a count or an explicit note; never a silent gap
    expect_true(all(!is.na(table$suggested_factors) | nzchar(table$note)), info = name)
  }
})

test_that("the extracted rows are each set's distinct available counts, typed estimated", {
  numbers <- er$efa_factor_numbers
  counts <- attr(numbers, "method_counts_by_item_set")
  estimated <- numbers[as.character(numbers$type) == "estimated", ]
  for (name in names(counts)) {
    suggested <- counts[[name]]$suggested_factors
    expect_identical(estimated$n_factors[estimated$item_set == name],
                     unique(suggested[!is.na(suggested)]), info = name)
  }
  expect_identical(levels(numbers$type), c("estimated", "theoretical"))
})

# --- add_theoretical_factor_numbers ran on the extracted table ------------

test_that("the theoretical rows are the configured counts and the extracted attributes survive", {
  numbers <- er$efa_factor_numbers
  extracted <- extract_suggested_factor_numbers(er$efa_factor_number_results)
  theoretical <- numbers[as.character(numbers$type) == "theoretical", ]
  expect_identical(nrow(theoretical), length(er_expected_factors))
  expect_setequal(theoretical$item_set, names(er_expected_factors))
  for (name in theoretical$item_set) {
    expect_identical(theoretical$n_factors[theoretical$item_set == name],
                     unname(er_expected_factors[[name]]), info = name)
  }
  # the consumer bound, it did not rebuild: the extracted rows are still on top
  expect_identical(numbers[seq_len(nrow(extracted)), ], extracted[, names(numbers)])
  for (attribute in c("item_sets", "factor_number_results", "method_counts_by_item_set")) {
    expect_identical(attr(numbers, attribute), attr(extracted, attribute), info = attribute)
  }
})

# --- rotation specifications -> real fits -------------------------------------
# Every examined count in every requested rotation, on real fits.

test_that("the rotation specifications expand exactly as registered over every count", {
  numbers <- er$efa_factor_numbers
  distinct <- unique(data.frame(item_set = numbers$item_set, n_factors = numbers$n_factors))
  wanted <- unlist(lapply(seq_len(nrow(distinct)), function(row) {
    paste(distinct$item_set[[row]], distinct$n_factors[[row]],
          er_rotations_for(distinct$n_factors[[row]]))
  }), use.names = FALSE)
  specifications <- er$efa_rotation_specifications
  expect_identical(vapply(specifications, er_fit_id, character(1)), wanted)
  for (specification in specifications) {
    rotations <- er_rotations_for(specification$factors)
    expect_true(specification$rotation %in% rotations, info = er_fit_id(specification))
    # a specification whose count is the configured one is flagged as such
    expect_identical(specification$expected_reference,
                     identical(specification$factors,
                               unname(er_expected_factors[[specification$set_name]])) &&
                       "theoretical" %in% specification$types,
                     info = er_fit_id(specification))
  }
  # every registered rotation is actually requested somewhere
  expect_setequal(unique(vapply(specifications, `[[`, character(1), "rotation")),
                  c("none", er_primary))
})

test_that("efa_fits holds one real record per specification, with its item identity", {
  fits <- er$efa_fits
  specifications <- er$efa_rotation_specifications
  expect_length(fits, length(specifications))
  expect_identical(vapply(fits, er_fit_id, character(1)),
                   vapply(specifications, er_fit_id, character(1)))
  for (fit in fits) {
    id <- er_fit_id(fit)
    columns <- names(er$efa_item_sets[[fit$set_name]]$data)
    if (isTRUE(fit$ok)) {
      expect_identical(rownames(fit$loadings), columns, info = id)
      expect_identical(ncol(fit$loadings), fit$factors, info = id)
      expect_true(all(is.finite(fit$loadings)), info = id)
    } else {
      # an unavailable fit keeps identity and reason, and is never substituted
      expect_null(fit$loadings, info = id)
      expect_true(is.character(fit$note) && nzchar(fit$note), info = id)
      expect_true(fit$set_name %in% names(er$efa_item_sets), info = id)
    }
  }
})

# --- efa_fits -> extract_efa_item_assignments ---------------------------

test_that("each item's assigned factor is the argmax of the fit's own loading matrix", {
  primary <- Filter(function(f) f$rotation %in% c(er_primary, "none"), er$efa_fits)
  assignments <- er$efa_item_assignments
  expect_identical(vapply(assignments, er_fit_id, character(1)),
                   vapply(primary, er_fit_id, character(1)))
  for (i in seq_along(assignments)) {
    solution <- assignments[[i]]
    id <- er_fit_id(solution)
    if (!isTRUE(solution$ok)) {
      expect_null(solution$membership, info = id)
      next
    }
    loadings <- solution$loadings
    expect_identical(loadings, primary[[i]]$loadings, info = id)
    membership <- solution$membership
    expect_identical(membership$item, solution$set$item_membership$item, info = id)
    expect_identical(membership$theoretical_scale,
                     solution$set$item_membership$theoretical_scale, info = id)
    strongest <- er_argmax(loadings)
    expect_identical(membership$assigned_factor,
                     unname(colnames(loadings)[strongest]), info = id)
    expect_equal(membership$largest_abs_loading,
                 unname(apply(abs(loadings), 1L, max)), info = id)
  }
})

# --- assignments -> extract_efa_item_correspondence ---------------------------
# Retention, purity and adjusted Rand, on real fits.

test_that("retention, purity and adjusted Rand recompute from the route's memberships", {
  correspondence <- er$efa_item_correspondence
  assignments <- er$efa_item_assignments
  expect_identical(correspondence$solutions, assignments)
  summaries <- correspondence$summaries
  expect_identical(nrow(summaries), length(assignments))
  for (i in seq_along(assignments)) {
    solution <- assignments[[i]]
    id <- er_fit_id(solution)
    expect_identical(summaries$set_name[[i]], solution$set_name, info = id)
    expect_identical(summaries$factors[[i]], solution$factors, info = id)
    if (is.null(solution$membership)) next
    mine <- er_pair_measures(solution$membership$theoretical_scale,
                            solution$membership$assigned_factor)
    expect_equal(summaries$expected_pair_retention[[i]], mine$retention, info = id)
    expect_equal(summaries$empirical_pair_purity[[i]], mine$purity, info = id)
    # a single-group partition makes the index NaN; the correspondence verb
    # reads that as complete agreement, which is the one recorded normalisation
    if (is.finite(mine$rand)) {
      expect_equal(summaries$adjusted_rand[[i]], mine$rand, info = id)
    } else {
      expect_equal(summaries$adjusted_rand[[i]], 1, info = id)
    }
  }
  # the combined ASC set at its two-factor solution: a nondegenerate case
  asc <- which(summaries$set_name == "asc" & summaries$factors == 2L)
  expect_length(asc, 1L)
  expect_gt(summaries$empirical_pair_purity[[asc]], 0)
  expect_lt(summaries$empirical_pair_purity[[asc]], 1)
})

# --- correspondence -> extract_efa_loading_clarity -----------------------------

test_that("loading clarity keeps the correspondence result and applies the .40 cutoff", {
  clarity <- er$efa_loading_clarity
  correspondence <- er$efa_item_correspondence
  expect_identical(clarity$solutions, correspondence$solutions)
  expect_identical(clarity$summaries, correspondence$summaries)
  items <- clarity$loading_items
  for (solution in clarity$solutions) {
    if (is.null(solution$membership)) next
    id <- er_fit_id(solution)
    rows <- items[items$set_name == solution$set_name & items$factors == solution$factors, ]
    expect_identical(rows$item, solution$membership$item, info = id)
    substantial <- unname(rowSums(abs(solution$loadings) >= er_cutoff))
    expect_identical(rows$n_loadings_at_40, as.integer(substantial), info = id)
    expect_identical(rows$weak, substantial == 0L, info = id)
    expect_identical(rows$crossloading, substantial >= 2L, info = id)
    expect_identical(rows$n_secondary_loadings_at_40,
                     as.integer(pmax(substantial - 1L, 0L)), info = id)
    # the cutoff describes, it does not reassign
    expect_identical(rows$assigned_factor, solution$membership$assigned_factor, info = id)
  }
  frequencies <- clarity$loading_frequencies
  for (row in seq_len(nrow(frequencies))) {
    rows <- items[items$set_name == frequencies$set_name[[row]] &
                    items$factors == frequencies$factors[[row]], ]
    expect_identical(frequencies$n_items[[row]], nrow(rows))
    expect_identical(frequencies$n_weak[[row]], sum(rows$weak))
    expect_identical(frequencies$n_crossloading[[row]], sum(rows$crossloading))
  }
})

# --- efa_fits -> explained variance ------------------------------------------

test_that("explained variance carries every fit's identity", {
  fits <- er$efa_fits
  ids <- vapply(fits, er_fit_id, character(1))
  expect_false(anyDuplicated(ids) > 0L)
  variance <- er$efa_explained_variance
  for (fit in fits) {
    id <- er_fit_id(fit)
    rows <- variance[variance$set_name == fit$set_name & variance$factors == fit$factors &
                       variance$rotation == fit$rotation, ]
    if (isTRUE(fit$ok)) {
      expect_identical(nrow(rows), fit$factors, info = id)
      expect_identical(rows$factor, colnames(fit$loadings), info = id)
      expect_equal(rows$cumulative_var, cumsum(rows$proportion_var), info = id)
    } else {
      expect_identical(nrow(rows), 1L, info = id)
      expect_identical(rows$note[[1]], fit$note, info = id)
    }
  }
})

# --- the report tables and the correspondence ---------------------------------

test_that("the per-scale projection is the route's theoretical-count solution", {
  scales <- er$efa_scales_table
  expect_identical(names(scales), er_scale_keys)
  for (key in er_scale_keys) {
    record <- scales[[key]]
    count <- unname(er_expected_factors[[key]])
    rotation <- er_rotations_for(count)[[1]]
    solution <- Filter(function(f) {
      identical(f$set_name, key) && identical(f$factors, count) &&
        identical(f$rotation, rotation)
    }, er$efa_fits)
    expect_length(solution, 1L)
    expect_identical(record$n_factors_used, count, info = key)
    expect_identical(record$rotation, rotation, info = key)
    expect_identical(record$loadings, solution[[1]]$loadings, info = key)
    # item identity in the table is the codebook's, not a renaming
    expect_identical(record$items$item, er_scale_columns(key), info = key)
    expect_identical(record$items$intended_scale, rep(key, record$n_items), info = key)
    expect_identical(record$n, nrow(er$data_descriptive_reliability), info = key)
    expect_identical(record$indices$index,
                     unname(name_efa_indices(er_cfg)), info = key)
    expect_identical(record$indices$n_factors,
                     attr(er$efa_factor_numbers, "method_counts_by_item_set")[[key]]$suggested_factors,
                     info = key)
  }
})

test_that("the EFA set record keeps the complete SRQ1 evidence of its item set", {
  sets <- er$efa_sets_table
  expect_identical(names(sets), names(er_efa_sets))
  for (key in names(sets)) {
    record <- sets[[key]]
    name <- key
    count <- unname(er_expected_factors[[name]])
    expect_identical(record$n_factors_used, count, info = key)
    expect_identical(record$rotation, er_rotations_for(count)[[1]], info = key)
    expect_identical(record$items$item, names(er$efa_item_sets[[name]]$data), info = key)
    evidence <- attr(record, "srq1_evidence")
    expect_identical(evidence$item_set, name, info = key)
    # every fitted count and rotation of the set, not only the projected one
    fitted <- Filter(function(f) identical(f$set_name, name), er$efa_fits)
    expect_length(evidence$solutions, length(fitted))
    expect_identical(vapply(evidence$solutions, er_fit_id, character(1)),
                     vapply(fitted, er_fit_id, character(1)), info = key)
    expect_true(all(vapply(evidence$solutions, function(s) is.null(s$set$data), logical(1))),
                info = key)
    expect_identical(nrow(evidence$method_counts), length(er_methods), info = key)
    expect_identical(names(evidence$factor_number_results), er_methods, info = key)
    expect_identical(evidence$factor_numbers,
                     er$efa_factor_numbers[er$efa_factor_numbers$item_set == name, ], info = key)
    expect_true(nrow(evidence$explained_variance) > 0L, info = key)
  }
})

test_that("the S1 correspondence lays out every solution of the route's loading clarity", {
  correspondence <- er$correspondence_table
  clarity <- er$efa_loading_clarity
  multi <- vapply(er_cfg$factor_analysis$sets, function(set) isTRUE(set$efa) && length(unlist(set$scales)) > 1L,
                  logical(1))
  multi_keys <- vapply(er_cfg$factor_analysis$sets[multi], function(set) as.character(set$key), character(1))
  in_sets <- clarity$summaries[clarity$summaries$set_name %in% multi_keys, ]
  expect_setequal(paste(correspondence$overview$set_key, correspondence$overview$factors),
                  paste(in_sets$set_name, in_sets$factors))
  expect_identical(names(correspondence$membership),
                   paste0(correspondence$overview$set_key, ":", correspondence$overview$factors))
  # every number is the route's own
  row <- match(paste(correspondence$overview$set_key, correspondence$overview$factors),
               paste(in_sets$set_name, in_sets$factors))
  expect_equal(correspondence$overview$adjusted_rand, in_sets$adjusted_rand[row])
  expect_equal(correspondence$overview$expected_pair_retention, in_sets$expected_pair_retention[row])
  for (name in names(correspondence$membership)) {
    parts <- strsplit(name, ":", fixed = TRUE)[[1]]
    items <- clarity$loading_items[clarity$loading_items$set_name == parts[1] &
                                     clarity$loading_items$factors == as.integer(parts[2]), ]
    expect_identical(correspondence$membership[[name]]$item, items$item, info = name)
    expect_identical(correspondence$membership[[name]]$assigned_factor, items$assigned_factor, info = name)
  }
  expect_setequal(correspondence$scales$scale_key, setdiff(clarity$summaries$set_name, multi_keys))
})

# --- report boundary ---------------------------------------------------------

test_that("the report tables render on the route's own objects", {
  labels <- rh_labels()
  # A table appears only where its values differ; where
  # they do not, a sentence carries them. Each display is checked on the
  # route's own objects in whichever form the data call for.
  criteria_rows <- tabulate_factor_number_criteria(er$efa_scales_table, er$efa_sets_table, er_cfg)
  criteria <- rh_factor_number_criteria_table(criteria_rows, labels, engine = "kable")
  if (any(rh_factor_number_criteria_differs(criteria_rows))) {
    expect_match(paste(as.character(criteria), collapse = "\n"), "Scale or item set", fixed = TRUE)
  } else {
    expect_null(criteria)
  }
  expect_true(nzchar(rh_factor_number_criteria_text(criteria_rows, labels)))
  set_labels <- stats::setNames(vapply(er_cfg$factor_analysis$sets, function(set) as.character(set$label), ""),
                                vapply(er_cfg$factor_analysis$sets, function(set) as.character(set$key), ""))
  off_rows <- tabulate_efa_items_off_scale(er$correspondence_table)
  off_scale <- rh_efa_items_off_scale_table(off_rows, labels, "efa_membership.csv", engine = "kable")
  if (nrow(off_rows) > 0L) expect_true(length(as.character(off_scale)) > 0L) else expect_null(off_scale)
  expect_true(nzchar(rh_efa_items_off_scale_text(off_rows, "efa_membership.csv")))
  overview_rows <- er$correspondence_table$overview
  overview <- rh_efa_correspondence_overview_table(overview_rows, er_cutoff, criteria = criteria_rows,
                                                   membership = er$correspondence_table$membership,
                                                   labels = labels, engine = "kable")
  if (any(rh_efa_correspondence_differs(overview_rows))) {
    expect_match(paste(as.character(overview), collapse = "\n"), "Empirical-pair purity", fixed = TRUE)
  } else {
    expect_null(overview)
  }
  expect_true(nzchar(rh_efa_correspondence_text(overview_rows, criteria_rows, er$correspondence_table$membership,
                                                labels)))
  clarity <- rh_efa_scale_clarity_table(er$correspondence_table$scales, labels, er_cutoff, engine = "kable")
  if (any(!(er$correspondence_table$scales$n_weak %in% 0L))) {
    expect_true(length(as.character(clarity)) > 0L)
  } else {
    expect_null(clarity)
  }
  expect_true(nzchar(rh_efa_scale_clarity_text(er$correspondence_table$scales, labels, er_cutoff)))
  # the membership of the expected solutions, one table per item set of several scales
  multi <- Filter(function(set) isTRUE(set$efa) && length(unlist(set$scales)) > 1L, er_cfg$factor_analysis$sets)
  for (set in multi) {
    table <- rh_efa_membership_table(er$correspondence_table$membership, list(set), labels, er_cutoff,
                                     engine = "kable")
    expect_true(length(as.character(table)) > 0L, info = set$key)
  }
})
