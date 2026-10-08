# Report target of the supplement section S1 "Measurement in full".
#
# Builds the one target (_targets.R, SUPPLEMENT · S1) the section, its tables
# and the Measurement section read. The report words and formats what this
# returns.

#' S1 measurement in full
#'
#' The tables of the section, the counts its prose states, and the facts of the
#' answer the Measurement section prints: whether every scale has a reliability
#' coefficient, and whether every single-scale confirmatory model converged and
#' was admissible.
#'
#' @param item_distributions Observed item summaries saved at intake, before filling.
#' @param scale_reliability Reliability of the scales (target `scale_reliability`).
#' @param cfa_reporting_data The confirmatory results (target
#'   `confirmatory_factor_structure_reporting_data`).
#' @param efa_fits,efa_factor_numbers,efa_explained_variance,efa_loading_clarity
#'   The exploratory results of SRQ1 (targets of the same names).
#' @param summary_files,residuals_file Paths of the written summary-statistics
#'   files and of the file of all pairwise residuals.
#' @param imputation_reporting_data The record of the fill (target of the same
#'   name).
#' @param codebook Codebook from [zm_codebook()].
#' @param analysis_plan Configuration from [zm_config()].
#' @return List `item_distributions`, `reliability`, `cfa_scales`, `cfa_sets`,
#'   the combined confirmatory tables `cfa_loadings`,
#'   `cfa_factor_correlations`, `cfa_largest_residuals`, `correspondence`, the
#'   combined exploratory tables `factor_number_criteria`, `efa_variance`,
#'   `efa_items_off_scale`, `summary_files`, `residuals_file`,
#'   `filled_cells`, `counts` and `answer`.
assemble_supplement_measurement <- function(item_distributions, scale_reliability, cfa_reporting_data,
                                            efa_fits, efa_factor_numbers,
                                            efa_explained_variance,
                                            efa_loading_clarity,
                                            summary_files, residuals_file, imputation_reporting_data,
                                            codebook, analysis_plan) {
  reliability <- tabulate_scale_reliability(scale_reliability, codebook)
  cfa_scales <- tabulate_cfa_scale_models(cfa_reporting_data, codebook)
  cfa_sets <- tabulate_cfa_set_models(cfa_reporting_data, codebook, analysis_plan)
  efa_scales <- tabulate_efa_scales(efa_fits, efa_factor_numbers,
                                    efa_explained_variance, codebook, analysis_plan)
  efa_sets <- tabulate_efa_sets(efa_fits, efa_factor_numbers,
                                 efa_explained_variance, analysis_plan)
  correspondence <- tabulate_efa_correspondence(efa_loading_clarity, analysis_plan)
  answer <- check_measurement_completeness(reliability, cfa_scales, analysis_plan)
  cfa_models <- rh_cfa_reporting_models(cfa_scales, cfa_sets)
  list(
    item_distributions = item_distributions,
    reliability = reliability,
    cfa_scales = cfa_scales,
    cfa_sets = cfa_sets,
    cfa_loadings = tabulate_cfa_loadings_by_model(cfa_models, analysis_plan),
    cfa_factor_correlations = tabulate_cfa_factor_correlations(cfa_models),
    cfa_largest_residuals = tabulate_cfa_largest_residuals(
      cfa_models, analysis_plan$factor_analysis$cfa_reporting$ranked_residuals_n),
    correspondence = correspondence,
    factor_number_criteria = tabulate_factor_number_criteria(efa_scales, efa_sets, analysis_plan),
    efa_variance = tabulate_efa_set_variance(efa_explained_variance, analysis_plan),
    efa_items_off_scale = tabulate_efa_items_off_scale(correspondence),
    summary_files = basename(summary_files),
    residuals_file = basename(residuals_file),
    filled_cells = build_filled_cells_table(imputation_reporting_data),
    imputation = imputation_reporting_data,
    imputation_model_status = imputation_reporting_data$model_status,
    # Both set counts describe what the supplement displays, so both are read
    # from the displayed results: the confirmatory multi-factor models the CFA
    # sets table holds, and the factor-count criteria the set EFAs report.
    counts = list(
      scale_efas = length(efa_scales),
      sets = length(analysis_plan$factor_analysis$sets),
      cfa_sets = nrow(cfa_sets),
      count_indices = length(unique(unlist(lapply(efa_sets, function(s) {
        as.character(s$indices$index)
      }))))
    ),
    answer = answer
  )
}

#' The standardised loadings of every confirmatory model, one row per item
#'
#' One column per model family of `analysis_plan$confirmatory_models`: the
#' one-factor model of the item's own scale, then each multi-factor model that
#' holds it. A cell is empty where no model of that family holds the item.
#' The unstandardised loadings with their standard errors are in the data file
#' `analysis_plan$data_files$result_files$cfa_loadings`.
#'
#' @param cfa_models Per-model records from [rh_cfa_reporting_models()].
#' @param analysis_plan Configuration from [zm_config()].
#' @return Tibble `item`, `item_label`, `scale`, `scale_key`, one numeric
#'   column per family key, with attributes `family_labels` (family key ->
#'   heading) and
#'   `diagnostic_only` (labels of the models that did not converge to an
#'   admissible solution).
tabulate_cfa_loadings_by_model <- function(cfa_models, analysis_plan) {
  planned <- analysis_plan$confirmatory_models$models
  family_of <- stats::setNames(vapply(planned, function(m) as.character(m$family), ""),
                               vapply(planned, function(m) as.character(m$key), ""))
  label_of <- stats::setNames(vapply(planned, function(m) as.character(m$label), ""), names(family_of))
  families <- unique(unname(family_of))
  family_labels <- vapply(families, function(family) {
    models <- names(family_of)[family_of == family]
    if (identical(family, "subscale")) "One-factor model of the scale"
    else paste(label_of[models], collapse = " / ")
  }, "")
  long <- dplyr::bind_rows(lapply(cfa_models, function(entry) {
    loadings <- entry$reporting$loadings
    if (is.null(loadings) || nrow(loadings) == 0L) return(NULL)
    tibble::tibble(family = unname(family_of[entry$model]), item = as.character(loadings$item),
                   item_label = as.character(loadings$item_label),
                   factor_label = as.character(loadings$factor_label),
                   factor_key = as.character(loadings$factor),
                   loading = as.numeric(loadings$estimate_std))
  }))
  own <- long[long$family == "subscale", c("item", "item_label", "factor_label", "factor_key")]
  out <- tibble::tibble(item = own$item, item_label = own$item_label, scale = own$factor_label,
                        scale_key = own$factor_key)
  for (family in families) {
    rows <- long[long$family == family, , drop = FALSE]
    out[[family]] <- rows$loading[match(out$item, rows$item)]
  }
  not_admissible <- vapply(cfa_models, function(entry) !(isTRUE(entry$converged) && isTRUE(entry$admissible)), NA)
  attr(out, "family_labels") <- family_labels
  attr(out, "diagnostic_only") <- vapply(cfa_models[not_admissible], function(entry) entry$label, "")
  out
}

#' The factor correlations of every multi-factor confirmatory model, as pairs
#'
#' @inheritParams tabulate_cfa_loadings_by_model
#' @return Tibble `model`, `model_key`, `factor_1`, `factor_2`, `factor_1_key`,
#'   `factor_2_key`, `correlation`, `se`.
tabulate_cfa_factor_correlations <- function(cfa_models) {
  dplyr::bind_rows(lapply(cfa_models, function(entry) {
    pairs <- entry$reporting$factor_correlations
    if (is.null(pairs) || nrow(pairs) == 0L) return(NULL)
    tibble::tibble(model = entry$label, model_key = entry$model,
                   factor_1 = as.character(pairs$factor_1_label),
                   factor_2 = as.character(pairs$factor_2_label),
                   factor_1_key = as.character(pairs$factor_1), factor_2_key = as.character(pairs$factor_2),
                   correlation = as.numeric(pairs$estimate_std), se = as.numeric(pairs$se_std))
  }))
}

#' The largest residual correlations of every confirmatory model
#'
#' The `n` pairs of each model with the largest absolute standardised
#' residual (`analysis_plan$factor_analysis$cfa_reporting$ranked_residuals_n`,
#' AP4). All pairs of every model are in
#' `analysis_plan$factor_analysis$cfa_reporting$residual_data_file`.
#'
#' @inheritParams tabulate_cfa_loadings_by_model
#' @param n Number of pairs per model.
#' @return Tibble `model`, `model_key`, `rank`, `item_1`, `item_2`,
#'   `correlation_residual`, `standardised_residual`.
tabulate_cfa_largest_residuals <- function(cfa_models, n) {
  dplyr::bind_rows(lapply(cfa_models, function(entry) {
    ranked <- entry$reporting$ranked_residuals
    if (is.null(ranked) || nrow(ranked) == 0L) return(NULL)
    ranked <- utils::head(ranked, n)
    tibble::tibble(model = entry$label, model_key = entry$model, rank = as.integer(ranked$rank_abs_std_residual),
                   item_1 = as.character(ranked$item_1), item_2 = as.character(ranked$item_2),
                   correlation_residual = as.numeric(ranked$bentler_residual),
                   standardised_residual = as.numeric(ranked$std_residual))
  }))
}

#' The factor-number criteria of every exploratory analysis, side by side
#'
#' One row per single scale and per item set, one column per criterion of
#' `analysis_plan$factor_analysis$factor_number_criteria` with the number of
#' factors it suggests, beside the number the scale structure expects. A
#' criterion that failed has no number; its reason is in `note`.
#'
#' @param efa_scales,efa_sets Records from [tabulate_efa_scales()] and
#'   [tabulate_efa_sets()].
#' @param analysis_plan Configuration from [zm_config()].
#' @return Tibble `key`, `label`, `n_items`, `expected`, one integer column per
#'   criterion index, `note`.
tabulate_factor_number_criteria <- function(efa_scales, efa_sets, analysis_plan) {
  indices <- unname(name_efa_indices(analysis_plan))
  dplyr::bind_rows(lapply(c(efa_scales, efa_sets), function(record) {
    counts <- record$indices
    row <- tibble::tibble(key = as.character(record$key), label = as.character(record$label),
                          n_items = as.integer(record$n_items), expected = as.integer(record$n_factors_used))
    for (index in indices) row[[index]] <- as.integer(counts$n_factors[match(index, counts$index)])
    notes <- counts$note[!is.na(counts$note) & nzchar(counts$note)]
    row$note <- if (length(notes)) paste(unique(notes), collapse = "; ") else NA_character_
    row
  }))
}

#' The variance explained by every solution of the item sets
#'
#' Each factor's proportion of the item variance and the running total, for
#' every examined solution in the primary rotation (AP9): first the one-factor
#' solution of each scale analysed on its own,
#' then the expected solution and every alternative the criteria suggested of
#' each item set of several scales. The single-scale rows are read from the
#' same stored variance table; nothing is computed anew.
#'
#' @param efa_explained_variance Tibble from [extract_efa_explained_variance()].
#' @param analysis_plan Configuration from [zm_config()].
#' @return Tibble `set` (the set label, or the scale key of a single scale),
#'   `scale_key` (the scale of a single-scale solution, else `NA`), `factors`,
#'   `solution`, `factor`, `proportion`, `cumulative`, `note`.
tabulate_efa_set_variance <- function(efa_explained_variance, analysis_plan) {
  all_sets <- ap4_factor_sets(analysis_plan, "efa")
  sets <- all_sets[vapply(all_sets, function(set) length(unlist(set$scales)) > 1L, NA)]
  keys <- names(sets)
  labels <- vapply(sets, function(set) as.character(set$label), "")
  expected <- vapply(sets, function(set) as.integer(set$expected_factors), 1L)
  rotations <- c(as.character(analysis_plan$factor_analysis$rotation_primary), "none")
  v <- efa_explained_variance[efa_explained_variance$set_name %in% keys &
                                efa_explained_variance$rotation %in% rotations, , drop = FALSE]
  is_expected <- v$factors == unname(expected[v$set_name])
  v <- v[order(match(v$set_name, keys), !is_expected, v$factors, v$cumulative_var), , drop = FALSE]
  set_rows <- tibble::tibble(
    set = unname(labels[v$set_name]), scale_key = NA_character_, factors = as.integer(v$factors),
    solution = ifelse(v$factors == unname(expected[v$set_name]), "expected count", "suggested by the criteria"),
    factor = as.character(v$factor), proportion = as.numeric(v$proportion_var),
    cumulative = as.numeric(v$cumulative_var), note = as.character(v$note)
  )
  # A scale analysed on its own is every analysed item set that is no
  # configured set; its one solution has the configured single-scale count and
  # is unrotated.
  single_count <- as.integer(analysis_plan$factor_analysis$single_scale_expected_factors)
  s <- efa_explained_variance[!efa_explained_variance$set_name %in% names(all_sets) &
                                efa_explained_variance$factors %in% single_count &
                                efa_explained_variance$rotation %in% "none", , drop = FALSE]
  scale_rows <- tibble::tibble(
    set = as.character(s$set_name), scale_key = as.character(s$set_name), factors = as.integer(s$factors),
    solution = rep("expected count", nrow(s)), factor = as.character(s$factor),
    proportion = as.numeric(s$proportion_var), cumulative = as.numeric(s$cumulative_var),
    note = as.character(s$note)
  )
  dplyr::bind_rows(scale_rows, set_rows)
}

#' The items that leave their scale's factor in an alternative solution
#'
#' For every solution the criteria suggested beside the expected count: the
#' items assigned to a different factor than the one most items of their
#' intended scale are assigned to. Merged scales, where whole scales share a
#' factor, show in the correspondence overview instead (AP9).
#'
#' @param correspondence List from [tabulate_efa_correspondence()].
#' @return Tibble `set`, `factors`, `item`, `intended_scale`,
#'   `assigned_factor`, `scale_factor`.
tabulate_efa_items_off_scale <- function(correspondence) {
  overview <- correspondence$overview
  alternatives <- overview[overview$solution != "expected count", , drop = FALSE]
  dplyr::bind_rows(lapply(seq_len(nrow(alternatives)), function(i) {
    membership <- correspondence$membership[[paste0(alternatives$set_key[i], ":", alternatives$factors[i])]]
    if (is.null(membership) || !nrow(membership)) return(NULL)
    scale_factor <- vapply(split(membership$assigned_factor, membership$intended_scale), function(x) {
      names(sort(table(x), decreasing = TRUE))[1]
    }, "")
    off <- membership$assigned_factor != scale_factor[membership$intended_scale]
    if (!any(off)) return(NULL)
    tibble::tibble(
      set = alternatives$set_label[i], factors = alternatives$factors[i],
      item = membership$item[off], intended_scale = membership$intended_scale[off],
      assigned_factor = membership$assigned_factor[off],
      scale_factor = unname(scale_factor[membership$intended_scale[off]])
    )
  }))
}

#' The item-to-factor correspondence of every EFA solution (SRQ1)
#'
#' The accepted route assigns each item of every fitted solution to the factor
#' with its largest absolute loading, compares that grouping with the intended
#' scales (adjusted Rand index, expected-pair retention, empirical-pair purity)
#' and counts the weakly loading and the cross-loading items at the loading
#' threshold (target `efa_loading_clarity`). This lays those results out for S1:
#' one overview row per solution of the sets holding several scales, one row per
#' single scale, and the membership table of every solution of those sets.
#'
#' Each factor is named by the scales it carries: those most of whose items
#' are assigned to it. With that, the membership names for every item the
#' factor of its next-largest loading and, for a cross-loading item, every
#' other factor it loads on at the loading threshold, with the scales those
#' factors carry: the preregistration's secondary factor associations (SRQ1).
#'
#' @param efa_loading_clarity List `solutions`, `summaries`, `loading_items`,
#'   `loading_frequencies` of [extract_efa_loading_clarity()].
#' @param analysis_plan Configuration from [zm_config()]
#'   (`analysis_plan$factor_analysis$loading_display_cutoff`).
#' @return List `overview` (`set_key`, `set_label`, `factors`, `solution`,
#'   `adjusted_rand`, `expected_pair_retention`, `empirical_pair_purity`,
#'   `n_items`, `n_weak`, `n_crossloading`, `note`), `scales` (`scale_key`,
#'   `n_items`, `n_weak`, `n_crossloading`) and `membership` (a membership table
#'   per solution, named `<set key>:<factors>`, which beside the loadings and
#'   their clarity carries `item_number` (the item's place in its scale),
#'   `assigned_scales`, `next_factor`, `next_factor_scales`,
#'   `crossloading_factors` and `crossloading_scales`; several factors or
#'   scales are joined by `;`, and none is `NA`).
tabulate_efa_correspondence <- function(efa_loading_clarity, analysis_plan) {
  sets <- Filter(function(set) isTRUE(set$efa) && length(unlist(set$scales)) > 1L,
                 analysis_plan$factor_analysis$sets)
  set_keys <- vapply(sets, function(set) as.character(set$key), character(1))
  set_labels <- stats::setNames(vapply(sets, function(set) as.character(set$label), character(1)), set_keys)
  solutions <- tibble::tibble(
    set_name = vapply(efa_loading_clarity$solutions, function(s) as.character(s$set_name), character(1)),
    factors = vapply(efa_loading_clarity$solutions, function(s) as.integer(s$factors), integer(1)),
    expected = vapply(efa_loading_clarity$solutions, function(s) "theoretical" %in% s$types, logical(1))
  )
  summaries <- tibble::as_tibble(efa_loading_clarity$summaries)
  frequencies <- tibble::as_tibble(efa_loading_clarity$loading_frequencies)
  rows <- dplyr::left_join(solutions, summaries, by = c("set_name", "factors")) |>
    dplyr::left_join(frequencies, by = c("set_name", "factors"))
  in_sets <- rows[rows$set_name %in% set_keys, , drop = FALSE]
  in_sets <- in_sets[order(match(in_sets$set_name, set_keys), !in_sets$expected, in_sets$factors), , drop = FALSE]
  overview <- tibble::tibble(
    set_key = in_sets$set_name,
    set_label = unname(set_labels[in_sets$set_name]),
    factors = in_sets$factors,
    solution = ifelse(in_sets$expected, "expected count", "suggested by the criteria"),
    adjusted_rand = in_sets$adjusted_rand,
    expected_pair_retention = in_sets$expected_pair_retention,
    empirical_pair_purity = in_sets$empirical_pair_purity,
    n_items = in_sets$n_items,
    n_weak = in_sets$n_weak,
    n_crossloading = in_sets$n_crossloading,
    note = in_sets$note
  )
  single <- rows[!rows$set_name %in% set_keys, , drop = FALSE]
  scales <- tibble::tibble(scale_key = single$set_name, n_items = single$n_items,
                           n_weak = single$n_weak, n_crossloading = single$n_crossloading)
  items <- tibble::as_tibble(efa_loading_clarity$loading_items)
  threshold <- analysis_plan$factor_analysis$loading_display_cutoff
  membership <- lapply(seq_len(nrow(overview)), function(i) {
    rows <- items[items$set_name == overview$set_key[i] & items$factors == overview$factors[i], , drop = FALSE]
    secondary <- describe_efa_secondary_factors(rows, threshold)
    tibble::tibble(
      item = rows$item, intended_scale = rows$theoretical_scale, assigned_factor = rows$assigned_factor,
      largest_abs_loading = rows$largest_abs_loading, next_largest_abs_loading = rows$next_largest_abs_loading,
      n_loadings_at_40 = rows$n_loadings_at_40, weak = rows$weak, crossloading = rows$crossloading,
      exact_tie = rows$exact_tie
    ) |>
      dplyr::bind_cols(secondary)
  })
  names(membership) <- paste0(overview$set_key, ":", overview$factors)
  list(overview = overview, scales = scales, membership = membership)
}

#' The scales a solution's factors carry and the secondary factors of its items
#'
#' A factor carries the scales most of whose items are assigned to it (the
#' rule [tabulate_efa_items_off_scale()] applies). For every item: its place
#' in its scale, the scales of its own factor, the factor of its next-largest
#' absolute loading with that factor's scales, and, for an item with a second
#' loading at the threshold, every such factor with its scales.
#'
#' @param rows The loading-clarity rows of one solution (`loading_items` of
#'   [extract_efa_loading_clarity()]), carrying the named `absolute_loadings`.
#' @param threshold The loading threshold of loading clarity.
#' @return Tibble `item_number`, `assigned_scales`, `next_factor`,
#'   `next_factor_scales`, `crossloading_factors`, `crossloading_scales`, one
#'   row per item.
describe_efa_secondary_factors <- function(rows, threshold) {
  n <- nrow(rows)
  empty <- tibble::tibble(
    item_number = integer(n), assigned_scales = rep(NA_character_, n), next_factor = rep(NA_character_, n),
    next_factor_scales = rep(NA_character_, n), crossloading_factors = rep(NA_character_, n),
    crossloading_scales = rep(NA_character_, n)
  )
  if (n == 0L) return(empty)
  scale <- as.character(rows$theoretical_scale)
  empty$item_number <- as.integer(stats::ave(seq_len(n), scale, FUN = seq_along))
  modal <- vapply(split(rows$assigned_factor, factor(scale, levels = unique(scale))), function(x) {
    names(sort(table(x), decreasing = TRUE))[1]
  }, "")
  scales_of <- function(factors) {
    vapply(factors, function(f) {
      if (is.na(f)) return(NA_character_)
      carried <- names(modal)[modal == f]
      if (length(carried)) paste(carried, collapse = ";") else NA_character_
    }, character(1), USE.NAMES = FALSE)
  }
  empty$assigned_scales <- scales_of(rows$assigned_factor)
  for (i in seq_len(n)) {
    loadings <- abs(unlist(rows$absolute_loadings[[i]]))
    others <- loadings[names(loadings) != rows$assigned_factor[[i]]]
    if (length(others) == 0L || is.null(names(others))) next
    empty$next_factor[[i]] <- names(others)[which.max(others)]
    if (isTRUE(rows$crossloading[[i]])) {
      at <- names(others)[others >= threshold]
      if (length(at)) empty$crossloading_factors[[i]] <- paste(at, collapse = ";")
    }
  }
  empty$next_factor_scales <- scales_of(empty$next_factor)
  empty$crossloading_scales <- vapply(empty$crossloading_factors, function(f) {
    if (is.na(f)) return(NA_character_)
    carried <- scales_of(strsplit(f, ";", fixed = TRUE)[[1]])
    carried <- unique(unlist(strsplit(carried[!is.na(carried)], ";", fixed = TRUE)))
    if (length(carried)) paste(carried, collapse = ";") else NA_character_
  }, character(1), USE.NAMES = FALSE)
  empty
}

#' Whether every measurement result the S1 answer names exists
#'
#' A reliability coefficient for every scale of the configured item sets, and a
#' converged and admissible single-scale confirmatory model for each.
#'
#' @param reliability,cfa_scales The reliability table and the single-scale
#'   confirmatory table.
#' @param analysis_plan Configuration from [zm_config()].
#' @return List `reliability_complete`, `reliability_missing` (scale keys),
#'   `cfa_complete`, `cfa_missing` (scale keys).
check_measurement_completeness <- function(reliability, cfa_scales, analysis_plan) {
  expected_scales <- unique(as.character(unlist(lapply(
    analysis_plan$factor_analysis$sets, function(set) set$scales
  ))))
  reported_reliability <- ifelse(
    reliability$method %in% "omega", reliability$omega_t,
    reliability$alpha
  )
  reliability_ok <- expected_scales %in%
    as.character(reliability$scale_key[is.finite(reported_reliability)])
  scale_cfa <- cfa_scales[cfa_scales$model %in% expected_scales, , drop = FALSE]
  cfa_ok <- expected_scales %in% as.character(
    scale_cfa$model[scale_cfa$converged %in% TRUE & scale_cfa$admissible %in% TRUE]
  )
  list(
    reliability_complete = length(expected_scales) > 0L && all(reliability_ok),
    reliability_missing = expected_scales[!reliability_ok],
    cfa_complete = length(expected_scales) > 0L && all(cfa_ok) &&
      all(scale_cfa$converged %in% TRUE) && all(scale_cfa$admissible %in% TRUE),
    cfa_missing = expected_scales[!cfa_ok]
  )
}

# ---- the tables of the section, built from the accepted results ----------
#
# The CFA tables reshape and relabel `confirmatory_factor_structure_reporting_data`;
# nothing is refitted. An assessment that could not be made at all (the fit
# call itself failed) is `NA` in the reporting data and `FALSE` in
# `converged`/`admissible` here, as the report helpers require.
#
# The EFA records show, per scale and per set, the theoretical count in its
# primary rotation (unrotated at one factor). Every number comes from
# `efa_fits`, `efa_factor_numbers` or `efa_explained_variance`, and the complete
# evidence travels on in the
# `srq1_evidence` attribute. The item-to-factor correspondence of every
# solution is the route's own (tabulate_efa_correspondence()).

#' The reliability table of the scale reliability result
#'
#' One row per scale, joined by scale key: the point coefficients come from
#' that scale's own `coefficients`, the raw intervals and resample counts from
#' its own `bootstrap` element, and the selected method and interval from the
#' `reported` row of the same key. No bootstrap is rerun and no coefficient is
#' re-estimated.
#'
#' The `omega_lo` / `omega_hi` pair carries what the report's omega
#' interval column showed: the omega bootstrap interval when enough omega
#' resamples succeeded, and nothing when the interval or model was unavailable.
#' `alpha_lo` / `alpha_hi` always carry the raw alpha interval. Omega-model
#' failure becomes `interval_source = "alpha_fallback"`; the precise reason is
#' kept in the column `interval_reason`.
#'
#' @param scale_reliability List from [apply_reliability_fallback()].
#' @param codebook Codebook list from [zm_codebook()]; supplies the labels.
#' @return Tibble with one row per scale: scale_key, label, n_items, n,
#'   omega_t, omega_lo, omega_hi, alpha, alpha_lo, alpha_hi, method,
#'   interval_source, n_resamples, n_boot_omega, n_boot_alpha, omega_success,
#'   note, interval_reason.
tabulate_scale_reliability <- function(scale_reliability, codebook) {
  labels <- stats::setNames(codebook$scales$label, codebook$scales$scale_key)
  reported <- scale_reliability$reported
  rows <- lapply(seq_len(nrow(reported)), function(row_index) {
    reported_row <- reported[row_index, ]
    scale_key <- as.character(reported_row$scale_key)
    scale <- scale_reliability$scales[[scale_key]]
    bootstrap <- scale_reliability$bootstrap[[scale_key]]
    if (is.null(scale) || is.null(bootstrap)) {
      stop("AP4: scale '", scale_key, "' has no estimated or bootstrapped evidence of its own.")
    }
    if (!scale_key %in% names(labels)) {
      stop("AP4: scale '", scale_key, "' has no label in the codebook.")
    }
    interval <- switch(
      as.character(reported_row$interval_source),
      omega_bootstrap = list(omega = bootstrap$omega_interval, source = "omega_bootstrap"),
      unavailable_omega_bootstrap = list(omega = c(NA_real_, NA_real_), source = "unavailable_omega_bootstrap"),
      alpha_fallback_omega_model_failed = list(omega = c(NA_real_, NA_real_), source = "alpha_fallback"),
      unavailable_alpha_fallback_failed = list(omega = c(NA_real_, NA_real_),
                                               source = "unavailable_alpha_fallback_failed"),
      stop("AP4: unknown reliability interval source '", reported_row$interval_source, "'.")
    )
    tibble::tibble(
      scale_key = scale_key,
      label = unname(labels[scale_key]),
      n_items = length(scale$item_codes),
      n = nrow(scale$responses),
      omega_t = as.numeric(scale$coefficients$omega),
      omega_lo = as.numeric(interval$omega[1]),
      omega_hi = as.numeric(interval$omega[2]),
      alpha = as.numeric(scale$coefficients$alpha),
      alpha_lo = as.numeric(bootstrap$alpha_interval[1]),
      alpha_hi = as.numeric(bootstrap$alpha_interval[2]),
      method = as.character(reported_row$reported_method),
      interval_source = interval$source,
      n_resamples = as.integer(bootstrap$n_resamples),
      n_boot_omega = as.integer(bootstrap$n_boot_omega),
      n_boot_alpha = as.integer(bootstrap$n_boot_alpha),
      omega_success = as.numeric(bootstrap$omega_success),
      note = as.character(reported_row$note),
      interval_reason = as.character(reported_row$interval_source)
    )
  })
  dplyr::bind_rows(rows)
}

#' The single-scale CFA table, projected from the CFA reporting data
#'
#' @param reporting_data `confirmatory_factor_structure_reporting_data`.
#' @param codebook Codebook list from [zm_codebook()].
#' @return Tibble with one row per preregistered one-factor scale model.
tabulate_cfa_scale_models <- function(reporting_data, codebook) {
  positions <- find_cfa_model_positions(reporting_data, scales = TRUE)
  if (!length(positions)) stop("The CFA reporting data carry no scale unidimensionality model.")
  dplyr::bind_rows(lapply(positions, function(i) {
    tabulate_cfa_model_row(reporting_data$model_status[i, ],
                      reporting_data$technical_supplement[[i]], codebook)
  }))
}

#' The multifactor CFA table, projected from the CFA reporting data
#'
#' @inheritParams tabulate_cfa_scale_models
#' @param analysis_plan Configuration from [zm_config()]; supplies the set keys.
#' @return Tibble with one row per preregistered multi-factor model.
tabulate_cfa_set_models <- function(reporting_data, codebook, analysis_plan) {
  positions <- find_cfa_model_positions(reporting_data, scales = FALSE)
  if (!length(positions)) stop("The CFA reporting data carry no multi-factor model.")
  dplyr::bind_rows(lapply(positions, function(i) {
    supplement <- reporting_data$technical_supplement[[i]]
    tabulate_cfa_model_row(reporting_data$model_status[i, ], supplement, codebook,
                      set = find_cfa_model_set_key(names(supplement$fixed_definition$factors), analysis_plan))
  }))
}

#' The rows of one confirmatory family, split by the unidimensionality question
#'
#' @param reporting_data `confirmatory_factor_structure_reporting_data`.
#' @param scales `TRUE` for the nine one-factor scale models, `FALSE` for the
#'   five multi-factor set models.
#' @return Integer positions into `model_status` and `technical_supplement`.
find_cfa_model_positions <- function(reporting_data, scales) {
  question <- as.character(reporting_data$model_status$question)
  which(if (scales) question == "scale unidimensionality" else question != "scale unidimensionality")
}

#' The configured factor-analytic set a fixed multi-factor model belongs to
#'
#' @param factor_keys Factor keys of the fixed definition.
#' @param analysis_plan Configuration from [zm_config()].
#' @return The set key.
find_cfa_model_set_key <- function(factor_keys, analysis_plan) {
  sets <- analysis_plan$factor_analysis$sets
  matched <- vapply(sets, function(set) {
    setequal(as.character(unlist(set$scales)), as.character(factor_keys))
  }, logical(1))
  if (sum(matched) != 1L) {
    stop("No single configured factor-analytic set has the scales ",
         paste(factor_keys, collapse = ", "), ".")
  }
  as.character(sets[[which(matched)]]$key)
}

#' One CFA row, projected from the CFA reporting data
#'
#' @param status One row of `reporting_data$model_status`.
#' @param supplement Its matching `technical_supplement` entry.
#' @param codebook Codebook list from [zm_codebook()].
#' @param set Configured set key, or `NA_character_` for a scale model.
#' @return A one-row tibble in the CFA table's columns.
tabulate_cfa_model_row <- function(status, supplement, codebook, set = NA_character_) {
  definition <- supplement$fixed_definition
  factors <- definition$factors
  columns <- zm_item_columns(unique(unlist(factors, use.names = FALSE)),
                             zm_item_column_map(codebook))
  estimates <- supplement$available_estimates
  indices <- if (is.null(estimates)) tibble::tibble() else estimates$fit_indices
  index <- function(name) {
    if (nrow(indices) > 0L && name %in% names(indices)) as.numeric(indices[[name]][1L]) else NA_real_
  }
  notes <- unique(c(as.character(supplement$warnings),
    as.character(supplement$extraction_issues$message[
      supplement$extraction_issues$component %in% "fit_indices"])))
  notes <- notes[!is.na(notes) & nzchar(notes)]
  loadings <- tibble::tibble(
    factor = character(0), item = character(0), loading = numeric(0), se = numeric(0))
  if (!is.null(estimates) && nrow(estimates$loadings) > 0L) {
    loadings <- tibble::tibble(
      factor = as.character(estimates$loadings$factor),
      item = as.character(estimates$loadings$item),
      loading = as.numeric(estimates$loadings$estimate_std),
      se = as.numeric(estimates$loadings$se_std))
  }
  tibble::tibble(
    model = as.character(status$model),
    set = as.character(set),
    label = as.character(definition$label),
    n_items = length(columns),
    n_factors = length(factors),
    n = as.integer(status$n),
    chisq = index("chisq"), df = index("df"), pvalue = index("pvalue"),
    cfi = index("cfi"), tli = index("tli"), rmsea = index("rmsea"),
    rmsea_lo = index("rmsea_90_lower"), rmsea_hi = index("rmsea_90_upper"),
    srmr = index("srmr"),
    index_version = ap4_fit_index_version(),
    converged = isTRUE(status$converged),
    admissible = isTRUE(status$admissible),
    error = as.character(supplement$error),
    note = if (length(notes)) paste(notes, collapse = " | ") else NA_character_,
    loadings = list(loadings),
    reporting = list(collect_cfa_model_reporting(
      supplement, status$model, definition$label, columns, names(factors), codebook))
  )
}

#' The per-model reporting object, projected from the technical supplement
#'
#' @param supplement One entry of `reporting_data$technical_supplement`.
#' @param model,label Model code and publication label of that entry.
#' @param columns Executable item columns of the fixed definition.
#' @param factor_keys Factor keys of the fixed definition.
#' @param codebook Codebook list from [zm_codebook()].
#' @return The named list [ap4_cfa_reporting()] returns.
collect_cfa_model_reporting <- function(supplement, model, label, columns, factor_keys, codebook) {
  model_metadata <- list(model = as.character(model), model_label = as.character(label))
  metadata <- ap4_cfa_codebook_metadata(codebook, items = columns, factors = factor_keys)
  pair_lookup <- function(source, key, label_name) {
    out <- tibble::tibble(a = as.character(source[[1L]]), b = as.character(source[[2L]]))
    names(out) <- c(key, label_name)
    out
  }
  out <- ap4_cfa_reporting_empty(model_metadata)
  estimates <- supplement$available_estimates
  if (!is.null(estimates) && nrow(estimates$loadings) > 0L) {
    loadings <- tibble::tibble(
      factor = as.character(estimates$loadings$factor),
      item = as.character(estimates$loadings$item),
      estimate_unstd = as.numeric(estimates$loadings$estimate_unstd),
      se_unstd = as.numeric(estimates$loadings$se_unstd),
      estimate_std = as.numeric(estimates$loadings$estimate_std),
      se_std = as.numeric(estimates$loadings$se_std)
    )
    loadings <- ap4_cfa_add_label(loadings, metadata$factors, "factor", "factor_label")
    loadings <- ap4_cfa_add_label(loadings, metadata$items, "item", "item_label")
    out$loadings <- ap4_cfa_attach_model(loadings[, c(
      "factor", "factor_label", "item", "item_label",
      "estimate_unstd", "se_unstd", "estimate_std", "se_std"
    )], model_metadata)
  }
  if (!is.null(estimates) && nrow(estimates$factor_correlations) > 0L) {
    correlations <- tibble::tibble(
      factor_1 = as.character(estimates$factor_correlations$factor_1),
      factor_2 = as.character(estimates$factor_correlations$factor_2),
      estimate_unstd = as.numeric(estimates$factor_correlations$estimate_unstd),
      se_unstd = as.numeric(estimates$factor_correlations$se_unstd),
      estimate_std = as.numeric(estimates$factor_correlations$estimate_std),
      se_std = as.numeric(estimates$factor_correlations$se_std)
    )
    for (suffix in c("1", "2")) {
      key <- paste0("factor_", suffix)
      correlations <- ap4_cfa_add_label(
        correlations, pair_lookup(metadata$factors, key, paste0(key, "_label")),
        key, paste0(key, "_label")
      )
    }
    out$factor_correlations <- ap4_cfa_attach_model(correlations[, c(
      "factor_1", "factor_1_label", "factor_2", "factor_2_label",
      "estimate_unstd", "se_unstd", "estimate_std", "se_std"
    )], model_metadata)
  }
  if (!is.null(estimates) && nrow(estimates$ave) > 0L) {
    ave <- tibble::tibble(factor = as.character(estimates$ave$factor),
                          ave = as.numeric(estimates$ave$ave))
    ave <- ap4_cfa_add_label(ave, metadata$factors, "factor", "factor_label")
    out$ave <- ap4_cfa_attach_model(ave[, c("factor", "factor_label", "ave")], model_metadata)
  }
  full <- supplement$full_residual_output
  if (!is.null(full) && nrow(full) > 0L) {
    rows <- tibble::tibble(
      item_1 = as.character(full$item_1), item_2 = as.character(full$item_2),
      bentler_residual = as.numeric(full$residual_correlation),
      std_residual = as.numeric(full$standardized_residual_correlation)
    )
    rows$abs_std_residual <- abs(rows$std_residual)
    ranked <- rows[order(rows$abs_std_residual, decreasing = TRUE, na.last = TRUE), , drop = FALSE]
    ranked$rank_abs_std_residual <- seq_len(nrow(ranked))
    rows$rank_abs_std_residual <- ranked$rank_abs_std_residual[
      match(paste(rows$item_1, rows$item_2), paste(ranked$item_1, ranked$item_2))
    ]
    label_pairs <- function(x) {
      for (suffix in c("1", "2")) {
        key <- paste0("item_", suffix)
        x <- ap4_cfa_add_label(
          x, pair_lookup(metadata$items, key, paste0(key, "_label")),
          key, paste0(key, "_label")
        )
      }
      ap4_cfa_attach_model(x[, c(
        "item_1", "item_1_label", "item_2", "item_2_label",
        "bentler_residual", "std_residual", "abs_std_residual", "rank_abs_std_residual"
      )], model_metadata)
    }
    out$residuals <- label_pairs(rows)
    out$ranked_residuals <- label_pairs(ranked)
  }
  issue_rows <- list()
  extraction <- supplement$extraction_issues
  if (!is.null(extraction) && nrow(extraction) > 0L) {
    issue_rows[[length(issue_rows) + 1L]] <- tibble::tibble(
      component = as.character(extraction$component), severity = "error",
      message = as.character(extraction$message))
  }
  for (entry in list(list("local_fit", supplement$residual_issue),
                     list("fit", supplement$error))) {
    text <- as.character(entry[[2L]])
    if (length(text) == 1L && !is.na(text) && nzchar(text)) {
      issue_rows[[length(issue_rows) + 1L]] <- tibble::tibble(
        component = entry[[1L]], severity = "error", message = text)
    }
  }
  if (length(issue_rows)) {
    out$issues <- ap4_cfa_attach_model(dplyr::bind_rows(issue_rows), model_metadata)
  }
  out
}

#' The per-scale EFA result object
#'
#' One record per codebook scale, in codebook order, from the accepted
#' single-scale item sets at their theoretical (one-factor, unrotated)
#' solution.
#'
#' @param efa_fits,efa_factor_numbers,efa_explained_variance
#'   The accepted SRQ1 results.
#' @param codebook Codebook list from `zm_codebook()`.
#' @param analysis_plan Analysis configuration.
#' @return Named list of EFA records, keyed by scale key.
tabulate_efa_scales <- function(efa_fits, efa_factor_numbers,
                                 efa_explained_variance, codebook, analysis_plan) {
  scales <- codebook$scales
  if (nrow(scales) == 0L) stop("The codebook lists no scale for the per-scale EFA projection.")
  records <- lapply(seq_len(nrow(scales)), function(i) {
    build_efa_record(
      key = scales$scale_key[i], label = scales$label[i], item_set = scales$scale_key[i],
      efa_fits, efa_factor_numbers,  efa_explained_variance, analysis_plan)
  })
  stats::setNames(records, as.character(scales$scale_key))
}

#' The per-set EFA result object
#'
#' One record per configured EFA set, in the plan's order, from the accepted
#' combined item sets at their configured expected count in the primary
#' rotation.
#'
#' @param efa_fits,efa_factor_numbers,efa_explained_variance
#'   The accepted SRQ1 results.
#' @param analysis_plan Analysis configuration.
#' @return Named list of EFA records, keyed by configured set key.
tabulate_efa_sets <- function(efa_fits, efa_factor_numbers,
                               efa_explained_variance, analysis_plan) {
  sets <- ap4_factor_sets(analysis_plan, "efa")
  records <- lapply(names(sets), function(key) {
    build_efa_record(
      key = key, label = sets[[key]]$label, item_set = key,
      efa_fits, efa_factor_numbers,  efa_explained_variance, analysis_plan)
  })
  stats::setNames(records, names(sets))
}

#' One EFA result object, projected from the accepted SRQ1 results
#'
#' @param key Record key (a scale key or a configured set key).
#' @param label Record label.
#' @param item_set Accepted item-set name.
#' @param efa_fits,efa_factor_numbers,efa_explained_variance
#'   The accepted SRQ1 results.
#' @param analysis_plan Analysis configuration.
#' @return List: key, label, items, n_items, n, indices, n_factors_used,
#'   rotation, loadings, phi, variance, error, with the accepted
#'   evidence in the `srq1_evidence` attribute.
build_efa_record <- function(key, label, item_set, efa_fits, efa_factor_numbers,
                                 efa_explained_variance, analysis_plan) {
  n_factors <- zm_efa_theoretical_count(efa_factor_numbers, item_set)
  solution <- zm_efa_primary_solution(efa_fits, item_set, n_factors, analysis_plan)
  membership <- solution$set$item_membership
  available <- isTRUE(solution$ok)
  out <- list(
    key = as.character(key),
    label = as.character(label),
    items = tibble::tibble(
      item = as.character(membership$column),
      intended_scale = as.character(membership$theoretical_scale)
    ),
    n_items = nrow(membership),
    n = nrow(solution$set$data),
    indices = tabulate_efa_indices(efa_factor_numbers, item_set, analysis_plan),
    n_factors_used = n_factors,
    rotation = as.character(solution$rotation),
    loadings = if (available) solution$loadings else NULL,
    phi = if (available) solution$Phi else NULL,
    variance = if (available) tabulate_efa_variance(solution, efa_explained_variance) else NULL,
    error = if (available) NA_character_ else as.character(solution$note)
  )
  attr(out, "srq1_evidence") <- zm_efa_srq1_evidence(
    item_set, efa_fits, efa_factor_numbers,  efa_explained_variance)
  out
}

#' Factor-count index name of every accepted factor-number criterion
#'
#' The pairing is the plan's own (`factor_analysis$factor_number_criteria`:
#' `key` -> `index`), in the plan's order, so this table cannot name a
#' criterion the route does not compute. `common_factor_parallel` (PAF
#' parallel analysis) keeps its accepted name.
#'
#' @param analysis_plan Configuration from [zm_config()].
#' @return Named character vector, accepted criterion -> index name.
name_efa_indices <- function(analysis_plan) {
  criteria <- ap4_factor_number_criteria(analysis_plan)
  stats::setNames(
    vapply(criteria, function(entry) as.character(entry$index), character(1)),
    vapply(criteria, function(entry) as.character(entry$key), character(1)))
}

#' The factor-count table of one item set
#'
#' One row per accepted factor-number method under its index name, with
#' the suggested count and the method's failure note.
#'
#' @param efa_factor_numbers Tibble carrying `method_counts_by_item_set`.
#' @param item_set Accepted item-set name.
#' @param analysis_plan Configuration from [zm_config()].
#' @return Tibble `index`, `n_factors`, `note`.
tabulate_efa_indices <- function(efa_factor_numbers, item_set, analysis_plan) {
  counts <- attr(efa_factor_numbers, "method_counts_by_item_set")[[item_set]]
  if (is.null(counts)) stop("No factor-number evidence for item set '", item_set, "'.")
  index_names <- name_efa_indices(analysis_plan)
  unknown <- setdiff(as.character(counts$method), names(index_names))
  if (length(unknown)) {
    stop("Factor-number method(s) without an index name: ", paste(unknown, collapse = ", "), ".")
  }
  tibble::tibble(
    index = unname(index_names[as.character(counts$method)]),
    n_factors = as.integer(counts$suggested_factors),
    note = as.character(counts$note)
  )
}

#' The variance table of one solution
#'
#' Factor names, proportion and cumulative variance come from the accepted
#' explained-variance result; the sums of squared loadings are read from the
#' same fit's retained `Vaccounted` matrix, which the accepted result does not
#' repeat.
#'
#' @param solution The selected packed solution.
#' @param efa_explained_variance Tibble from
#'   `extract_efa_explained_variance()`.
#' @return Tibble `factor`, `ss_loadings`, `proportion_var`, `cumulative_var`,
#'   or `NULL` when no variance was reported.
tabulate_efa_variance <- function(solution, efa_explained_variance) {
  rows <- efa_explained_variance[
    efa_explained_variance$set_name == solution$set_name &
      efa_explained_variance$factors == solution$factors &
      efa_explained_variance$rotation == solution$rotation, , drop = FALSE]
  if (nrow(rows) == 0L || any(!is.na(rows$note))) return(NULL)
  variance <- as.matrix(solution$Vaccounted)
  ss_loadings <- if ("SS loadings" %in% rownames(variance)) {
    as.numeric(variance["SS loadings", ])
  } else {
    rep(NA_real_, nrow(rows))
  }
  tibble::tibble(
    factor = as.character(rows$factor),
    ss_loadings = ss_loadings,
    proportion_var = as.numeric(rows$proportion_var),
    cumulative_var = as.numeric(rows$cumulative_var)
  )
}

#' Save the CFA fit extrema used in one paragraph
summarise_cfa_reporting_fits <- function(rows) {
  valid <- rows$converged %in% TRUE & rows$admissible %in% TRUE
  extreme <- function(key, direction) {
    x <- as.numeric(rows[[key]][valid]); x <- x[is.finite(x)]
    if (!length(x)) return(NA_real_)
    if (direction == "minimum") min(x) else max(x)
  }
  list(n = nrow(rows), n_valid = sum(valid), all_valid = all(valid), any_valid = any(valid),
    cfi = extreme("cfi", "minimum"), tli = extreme("tli", "minimum"),
    rmsea = extreme("rmsea", "maximum"), srmr = extreme("srmr", "maximum"))
}

#' Save the factor-number readings and CFA groups used by the report
add_measurement_reporting_facts <- function(report, analysis_plan) {
  family <- stats::setNames(
    vapply(analysis_plan$confirmatory_models$models, function(m) as.character(m$family), character(1)),
    vapply(analysis_plan$confirmatory_models$models, function(m) as.character(m$key), character(1)))
  structure <- report$cfa_sets[family[as.character(report$cfa_sets$model)] %in% c("asc", "ums_dopl"), , drop = FALSE]
  combined <- report$cfa_sets[family[as.character(report$cfa_sets$model)] %in% c("social_motives", "auth_orientation"), , drop = FALSE]
  if (nrow(structure) + nrow(combined) != nrow(report$cfa_sets)) {
    stop("A multi-factor confirmatory model belongs to neither the subscale structures nor the combined item sets.")
  }
  attr(structure, "report_summary") <- summarise_cfa_reporting_fits(structure)
  attr(combined, "report_summary") <- summarise_cfa_reporting_fits(combined)
  attr(report$cfa_scales, "report_summary") <- summarise_cfa_reporting_fits(report$cfa_scales)
  report$cfa_structure_models <- structure
  report$cfa_combined_models <- combined
  criteria <- report$factor_number_criteria
  columns <- setdiff(names(criteria), c("key", "label", "n_items", "expected", "note"))
  set_keys <- vapply(analysis_plan$factor_analysis$sets, function(set) as.character(set$key), character(1))
  report$factor_readings <- dplyr::bind_rows(lapply(seq_len(nrow(criteria)), function(i) {
    suggested <- as.integer(unlist(criteria[i, columns])); suggested <- suggested[!is.na(suggested)]
    expected <- as.integer(criteria$expected[i]); key <- as.character(criteria$key[i])
    tibble::tibble(key = key, is_set = key %in% set_keys, label = as.character(criteria$label[i]),
      reading = if (!length(suggested)) "unavailable" else if (all(suggested == expected)) "expected"
        else if (all(suggested < expected)) "fewer" else if (all(suggested > expected)) "more" else "mixed",
      n_available = length(suggested), n_at = sum(suggested == expected),
      n_below = sum(suggested < expected), n_above = sum(suggested > expected))
  }))
  report$counts$factor_mixed <- sum(report$factor_readings$reading == "mixed")
  report$counts$expected_correspondence <- sum(report$correspondence$overview$solution == "expected count")
  report$crossloading_items <- unique(dplyr::bind_rows(lapply(report$correspondence$membership, function(m) {
    m <- tibble::as_tibble(m)
    m[m$crossloading %in% TRUE, intersect(c("intended_scale", "item_number", "crossloading_scales"), names(m)), drop = FALSE]
  })))
  report$counts$crossloading_items <- nrow(report$crossloading_items)
  report$counts$factor_criteria <- length(columns)
  report$counts$cfa_structure <- nrow(structure)
  report$counts$cfa_combined <- nrow(combined)
  report$counts$factor_scales <- sum(!report$factor_readings$is_set)
  report
}
