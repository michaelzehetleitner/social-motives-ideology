# SRQ1 — exploratory dimensionality and item correspondence.
#
# The design of the exploratory factor analysis: fourteen
# item sets (the nine scales alone and five combined groups), one polychoric
# matrix per set, nine factor-number methods reported side by side, the
# configured theoretical count beside every distinct empirical suggestion,
# oblimin as the primary rotation, and the
# unrotated one-factor solution. Nothing here selects a count: every requested
# count and rotation is fitted and retained, and a failure stays an explicit
# unavailable record rather than a missing one.
#
# The measurement definitions name items by their questionnaire source label
# (`SDO-D_1`); the study data carry the executable column (`SDO_D_1`). The
# item-set definition receives the codebook as an explicit argument, and
# `zm_item_columns()` resolves the labels through `zm_item_column_map()`. Labels
# stay for display; selection goes through the map.
#
# The per-scale and per-set EFA tables the report reads are built from these
# results in R/report_supplement_measurement.R and recompute nothing.

# --- item sets ---------------------------------------------------------------

# BEGIN GENERATED PARAMETER CARD: AP9 EFA SEED
# Automatically generated from analysis_plan.yaml
#   EFA seed: 1   (factor_analysis.seed)
# Carried by every materialised item set; each stochastic factor-number call
# starts from it, so parallel analysis, comparison data and VSS are reproducible.
# END GENERATED PARAMETER CARD: AP9 EFA SEED

# BEGIN GENERATED PARAMETER CARD: AP9 EFA ITEM SETS
# Automatically generated from analysis_plan.yaml
# Each of the nine scales alone, then the combined sets   (factor_analysis.sets, efa: true)
#   asc      = asc_agg, asc_sub, asc_conv
#   dopl     = zm_power, zm_prestige
#   ums      = zm_security, zm_achievement
#   motives  = zm_security, zm_arousal, zm_power, zm_prestige, zm_achievement
#   outcomes = asc_agg, asc_sub, asc_conv, sdo_dom
# A combined set's columns are its scales' items, scale by scale in this order.
# END GENERATED PARAMETER CARD: AP9 EFA ITEM SETS
#' Declare the nine item mappings and all fourteen SRQ1 item sets
#'
#' AP9 / SRQ1 item sets: examine each of the nine scales alone and
#' the five combined groups the plan configures — ASC, Dominance–Prestige, UMS
#' Intimacy–Achievement, all five motives, and ASC with SDO-D. The combined
#' sets carry the plan's own keys. Expected factor counts are reference values,
#' not selected answers.
#'
#' AP3 already supplies complete reverse-scored items. This definition does not
#' filter, reverse, impute, or standardise them again.
#'
#' @param data The narrow descriptive/reliability/factor-analysis input
#'   (`data_descriptive_reliability`).
#' @param analysis_plan Configuration from [zm_config()]: `analysis_plan$factor_analysis$sets`
#'   supplies the five combined sets under their configured keys, and
#'   `analysis_plan$factor_analysis$seed` the RNG seed every set carries.
#' @param codebook Codebook from [zm_codebook()]; its label-to-column map
#'   resolves the declared item labels to the executable data columns.
#' @return Named list of fourteen materialised item sets; see
#'   [ap4_materialise_efa_item_sets()].
define_efa_item_sets <- function(data, analysis_plan, codebook) {
  item_sets <- list(
    zm_security = c("UMS_int_1", "UMS_int_2", "UMS_int_3", "UMS_int_4", "UMS_int_5", "UMS_int_6"),
    zm_arousal = c("UNT_1_r", "UNT_2", "UNT_3", "UNT_4_r", "UNT_5", "UNT_6"),
    zm_power = c("DOPL_dom_1", "DOPL_dom_2", "DOPL_dom_3", "DOPL_dom_4", "DOPL_dom_5", "DOPL_dom_6"),
    zm_prestige = c("DOPL_pre_1", "DOPL_pre_2", "DOPL_pre_3", "DOPL_pre_4", "DOPL_pre_5", "DOPL_pre_6"),
    zm_achievement = c("UMS_ach_1", "UMS_ach_2", "UMS_ach_3", "UMS_ach_4", "UMS_ach_5", "UMS_ach_6"),
    asc_agg = c("ASC_aag_1", "ASC_aag_2", "ASC_aag_3_r", "ASC_aag_4_r", "ASC_aag_5_r", "ASC_aag_6"),
    asc_sub = c("ASC_asu_1", "ASC_asu_2", "ASC_asu_3_r", "ASC_asu_4", "ASC_asu_5_r", "ASC_asu_6_r"),
    asc_conv = c("ASC_con_1_r", "ASC_con_2", "ASC_con_3", "ASC_con_4", "ASC_con_5_r", "ASC_con_6_r", "ASC_con_7"),
    sdo_dom = c("SDO-D_1", "SDO-D_2", "SDO-D_3", "SDO-D_4", "SDO-D_5_r", "SDO-D_6_r", "SDO-D_7_r", "SDO-D_8_r")
  )
  # The scales in the order of the scale codebook's `order` column, whatever
  # the order written above.
  item_sets <- item_sets[zm_order_scale_keys(names(item_sets), codebook)]
  # Each scale alone, then the five combined sets under the plan's keys, their
  # scale blocks in the codebook's order (zm_config() puts every set in it).
  combined_sets <- lapply(ap4_factor_sets(analysis_plan, "efa"), function(set) as.character(set$scales))
  set_definitions <- c(
    stats::setNames(as.list(names(item_sets)), names(item_sets)),
    combined_sets
  )
  ap4_materialise_efa_item_sets(data, codebook, item_sets, set_definitions,
                                seed = analysis_plan$factor_analysis$seed)
}

# --- polychoric correlations -------------------------------------------------

#' Estimate and retain one polychoric matrix for each item set
#'
#' AP9 / SRQ1 correlations: one polychoric correlation matrix per item
#' set without continuity correction, reused for every matrix-based
#' factor-number method and every EFA solution.
#'
#' A matrix failure is recorded on its set. Parallel analysis requires that
#' matrix; comparison-data estimation remains separately runnable.
#'
#' @param efa_item_sets Named list from [define_efa_item_sets()].
#' @return The same list, each set carrying `correlation` (see
#'   [ap4_capture_correlation_result()]).
estimate_polychoric_correlations <- function(efa_item_sets) {
  efa_item_sets |>
    lapply(function(set) {
    correlation <- ap4_capture_correlation_result({
      require_valid_imputation_inputs(set$data, names(set$data))
      psych::polychoric(set$data, correct = 0)$rho
    })
    c(set, list(correlation = correlation))
  })
}

# --- factor-number criteria ---------------------------------------------------

#' The factor-number criteria of the analysis plan
#'
#' @param analysis_plan Configuration from [zm_config()];
#'   `analysis_plan$factor_analysis$factor_number_criteria` must exist.
#' @return List of criterion entries (each a list with key, index, label), in
#'   the order the plan lists them.
ap4_factor_number_criteria <- function(analysis_plan) {
  criteria <- analysis_plan$factor_analysis$factor_number_criteria
  if (!is.list(criteria) || length(criteria) == 0L) {
    stop("AP9: analysis_plan.yaml states no factor_analysis$factor_number_criteria.")
  }
  keys <- vapply(criteria, function(entry) as.character(entry$key), character(1))
  if (anyDuplicated(keys)) {
    stop("AP9: factor_number_criteria names a criterion twice: ",
         paste(unique(keys[duplicated(keys)]), collapse = ", "), ".")
  }
  criteria
}

# BEGIN GENERATED PARAMETER CARD: AP9 FACTOR CRITERIA
# Automatically generated from analysis_plan.yaml
# 9 criteria, each computed on its own; none selects a count.
#   pca_parallel           -> parallel_analysis        "Parallel analysis (principal components)"
#   common_factor_parallel -> common_factor_parallel   "Parallel analysis (common factors)"
#   comparison_data        -> comparison_data          "Comparison data"
#   original_map           -> map                      "Velicer's minimum average partial"
#   classical_kaiser       -> kaiser                   "Kaiser criterion"
#   empirical_kaiser       -> empirical_kaiser         "Empirical Kaiser criterion"
#   optimal_coordinates    -> optimal_coordinates      "Optimal coordinates"
#   acceleration_factor    -> acceleration_factor      "Acceleration factor"
#   vss_complexity_1       -> vss                      "Very simple structure (complexity one)"
# Tuning settings the plan fixes:
#   Parallel analysis (both extractions): reference datasets = 100; percentile = 95; correlations =
#     polychoric; reference-dataset correlations = pearson.
#   Comparison data: population size = 10000; samples = 500; alpha = 0.3; maximum iterations = 50;
#     correlations = pearson.
#   Classical Kaiser criterion: eigenvalue threshold = 1.
#   Very simple structure: complexity = 1 (fixed by the criterion key vss_complexity_1); maximum
#     factors = 8.
# END GENERATED PARAMETER CARD: AP9 FACTOR CRITERIA

#' Apply every planned factor-number criterion independently
#'
#' AP9 / SRQ1 factor-number criteria: the criteria
#' `factor_analysis$factor_number_criteria` names, in the plan's order, each
#' applied on its own. Every suggestion is recorded; none is selected by
#' majority. A criterion the plan names but this verb cannot compute stops the
#' route rather than being skipped.
#'
#' PAF parallel analysis uses squared multiple correlations on the diagonal.
#' The classical Kaiser criterion is EFAtools::KGC on the polychoric matrix
#' (eigenvalues greater than 1); optimal coordinates and the acceleration
#' factor are nFactors::nScree on its eigenvalues, where the Kaiser rule is the
#' condition nScree applies to both.
#'
#' Each stochastic call starts at the set's seed. No method yields a confidence
#' interval for a factor count: PA/CD simulation variation belongs to the
#' reference procedure, not to participant-sampling uncertainty, and no
#' participant bootstrap is added.
#'
#' @param efa_item_sets Named list from [estimate_polychoric_correlations()].
#' @param analysis_plan Configuration from [zm_config()]
#'   (`analysis_plan$factor_analysis$factor_number_criteria`, `factor_number_methods`,
#'   `efa_estimation_method`, `rotation_primary`).
#' @return The same list, each set carrying `factor_number_methods`: one
#'   captured result per planned criterion, in the plan's order.
apply_factor_number_methods <- function(efa_item_sets, analysis_plan) {
  methods <- analysis_plan$factor_analysis$factor_number_methods
  criteria <- ap4_factor_number_criteria(analysis_plan)
  criterion_keys <- vapply(criteria, function(entry) as.character(entry$key), character(1))
  rotation_settings <- if ("vss_complexity_1" %in% criterion_keys) {
    ap4_require_efa_rotation_settings(analysis_plan)
  } else NULL
  if ("classical_kaiser" %in% criterion_keys &&
      !identical(as.numeric(methods$kaiser$threshold), 1)) {
    stop("AP9: EFAtools::KGC counts eigenvalues greater than 1; the plan's kaiser threshold must be 1.")
  }
  efa_item_sets |>
    lapply(function(set) {
    x <- set$data
    polychoric_corr_matrix <- set$correlation$value
    n <- nrow(x)
    p <- ncol(x)
    require_parallel_analysis_matrix <- function() {
      if (!isTRUE(set$correlation$ok) || is.null(polychoric_corr_matrix)) {
        stop("Parallel analysis requires an available retained polychoric matrix for item set '",
             set$name, "'.")
      }
      polychoric_corr_matrix
    }
    eigenvalues <- function() {
      eigen(polychoric_corr_matrix, symmetric = TRUE, only.values = TRUE)$values
    }
    compute <- function(key) {
      note <- describe_unavailable_imputation_inputs(set$data, names(set$data))
      if (!is.null(note)) return(list(ok = FALSE, value = NULL, note = note))
      switch(key,
      pca_parallel = ap4_capture_efa_result(ap4_with_efa_seed(set$seed,
        EFA.dimensions::RAWPAR(
          require_parallel_analysis_matrix(), Ncases = n,
          randtype = "generated", extraction = "PCA", Ndatasets = methods$parallel_analysis$reference_datasets,
          percentile = methods$parallel_analysis$percentile,
          corkind = methods$parallel_analysis$correlation_type,
          corkindRAND = methods$parallel_analysis$random_correlation_type,
          eval_plot = FALSE, verbose = FALSE
        )
      )),
      common_factor_parallel = ap4_capture_efa_result(ap4_with_efa_seed(set$seed,
        EFA.dimensions::RAWPAR(
          require_parallel_analysis_matrix(), Ncases = n,
          randtype = "generated", extraction = "PAF", Ndatasets = methods$parallel_analysis$reference_datasets,
          percentile = methods$parallel_analysis$percentile,
          corkind = methods$parallel_analysis$correlation_type,
          corkindRAND = methods$parallel_analysis$random_correlation_type,
          eval_plot = FALSE, verbose = FALSE
        )
      )),
      comparison_data = ap4_capture_efa_result(ap4_with_efa_seed(set$seed,
        EFAtools::CD(
          x,
          N_pop = methods$comparison_data$population_size,
          N_samples = methods$comparison_data$samples,
          alpha = methods$comparison_data$alpha,
          cor_method = methods$comparison_data$correlation_type,
          max_iter = methods$comparison_data$max_iterations
        )
      )),
      original_map = ap4_capture_efa_result(
        EFA.dimensions::MAP(polychoric_corr_matrix, Ncases = n, verbose = FALSE)
      ),
      classical_kaiser = ap4_capture_efa_result(
        EFAtools::KGC(polychoric_corr_matrix, eigen_type = "PCA")
      ),
      empirical_kaiser = ap4_capture_efa_result(
        EFAtools::EKC(polychoric_corr_matrix, N = n)
      ),
      # nScree's default aparallel is 1 for every eigenvalue: the Kaiser rule
      # that both scree criteria must satisfy as well.
      optimal_coordinates = ap4_capture_efa_result(
        nFactors::nScree(x = eigenvalues(), model = "components")
      ),
      acceleration_factor = ap4_capture_efa_result(
        nFactors::nScree(x = eigenvalues(), model = "components")
      ),
      vss_complexity_1 = ap4_capture_efa_result(ap4_with_efa_seed(set$seed,
        psych::vss(
          polychoric_corr_matrix, n = min(methods$vss$maximum_factors, p - 1L), n.obs = n,
          rotate = analysis_plan$factor_analysis$rotation_primary,
          fm = analysis_plan$factor_analysis$efa_estimation_method,
          gam = rotation_settings$oblimin_gamma,
          n.rotations = rotation_settings$starting_rotations, plot = FALSE
        )
      )),
      stop("AP9: the plan names a factor-number criterion this route cannot compute: '",
           key, "'.")
    )
    }
    keys <- vapply(criteria, function(entry) as.character(entry$key), character(1))
    factor_number_methods <- stats::setNames(lapply(keys, compute), keys)
    c(set, list(factor_number_methods = factor_number_methods))
  })
}

#' Extract each method's suggested count from its documented result field
#'
#' AP9 / SRQ1 factor-number criteria: extract each method's suggestion
#' without discarding its complete result object or its failure note. Every
#' distinct available empirical count is retained; none is selected by majority.
#'
#' The field mapping and the VSS rule are the only extraction decisions and
#' stand here. The compact table drives the later specifications; its technical
#' metadata retains every full method result, every extracted count and every
#' failure note for reporting.
#'
#' @param efa_factor_number_results Named list from
#'   [apply_factor_number_methods()].
#' @return Tibble `item_set`, `n_factors`, `type`, with the attributes
#'   `item_sets`, `factor_number_results` and `method_counts_by_item_set`.
extract_suggested_factor_numbers <- function(efa_factor_number_results) {
  suggested_count_field <- list(
    pca_parallel = "NfactorsPA", common_factor_parallel = "NfactorsPA",
    comparison_data = "n_factors", original_map = "NfactorsMAP",
    classical_kaiser = "n_factors", empirical_kaiser = "n_factors",
    optimal_coordinates = c("Components", "noc"),
    acceleration_factor = c("Components", "naf")
  )
  # VSS: the first maximum of complexity-one fit is the suggested count.
  vss_suggested_count <- function(vss) which.max(vss$cfit.1)
  ap4_pack_suggested_factor_numbers(
    efa_factor_number_results, suggested_count_field, vss_suggested_count
  )
}

# BEGIN GENERATED PARAMETER CARD: AP9 EFA EXPECTED COUNTS
# Automatically generated from analysis_plan.yaml
# Theoretical factor counts, reported beside the estimates; none is selected.
#   Each scale alone: 1   (factor_analysis.single_scale_expected_factors)
#   asc      = 3   (factor_analysis.sets.expected_factors)
#   dopl     = 2   (factor_analysis.sets.expected_factors)
#   ums      = 2   (factor_analysis.sets.expected_factors)
#   motives  = 5   (factor_analysis.sets.expected_factors)
#   outcomes = 4   (factor_analysis.sets.expected_factors)
# END GENERATED PARAMETER CARD: AP9 EFA EXPECTED COUNTS
#' Add one configured theoretical row per item set beside the empirical estimates
#'
#' AP9 / SRQ1 factor-count set. The theoretical row remains present
#' even when no method supports that count; an equal estimated count remains a
#' separate row.
#'
#' @param estimated_counts Tibble from [extract_suggested_factor_numbers()].
#' @param analysis_plan Configuration from [zm_config()].
#' @return The same tibble with the theoretical rows bound underneath.
add_theoretical_factor_numbers <- function(estimated_counts, analysis_plan) {
  theoretical_counts <- ap4_theoretical_factor_numbers(analysis_plan)
  dplyr::bind_rows(estimated_counts, theoretical_counts)
}

# --- rotation specifications and fits ----------------------------------------

#' Add the requested rotation methods and method provenance to each factor count
#'
#' AP9 / SRQ1 factor-count and rotation specifications: for every item
#' set, examine the expected factor count and every distinct nonmissing count
#' any method suggests. Oblimin is primary.
#' One-factor specifications are unrotated.
#'
#' Counts such as zero, or values too large for the item set, stay specified
#' with an unavailable reason; none is silently replaced, capped, or promoted
#' to a selected fit.
#'
#' @param efa_factor_numbers Tibble from [add_theoretical_factor_numbers()].
#' @param one_factor Rotation of a one-factor specification.
#' @param multiple_factors Rotations of a multi-factor specification, primary
#'   first.
#' @return Unnamed list of specifications; see
#'   [ap4_expand_efa_rotation_specifications()].
add_efa_rotation_methods <- function(
    efa_factor_numbers, one_factor, multiple_factors) {
  efa_factor_numbers |>
    dplyr::distinct(item_set, n_factors) |>
    ap4_expand_efa_rotation_specifications(
      factor_number_evidence = efa_factor_numbers,
      one_factor = one_factor,
      multiple_factors = multiple_factors
    )
}

# BEGIN GENERATED PARAMETER CARD: AP9 EFA ESTIMATION
# Automatically generated from analysis_plan.yaml
#   Estimation: wls on the item set's polychoric matrix   (factor_analysis.efa_estimation_method)
#   Primary rotation: oblimin   (factor_analysis.rotation_primary)
#   Oblimin gamma: 0   (factor_analysis.rotation_settings.oblimin_gamma)
#   Starting rotations: 20   (factor_analysis.rotation_settings.starting_rotations)
#   The same rotation settings apply to VSS trial solutions.
#   A one-factor solution is unrotated.
# END GENERATED PARAMETER CARD: AP9 EFA ESTIMATION
#' Fit every requested factor count and rotation
#'
#' AP9 / SRQ1 EFA estimation: WLS EFA on the one retained polychoric
#' matrix. Oblimin is primary; a
#' one-factor solution is unrotated.
#'
#' Fit and correlation failures produce named unavailable records with their
#' notes. Requested counts, method labels and expected-reference flags remain
#' present. No unavailable specification is substituted for.
#'
#' Every factor fit starts from the plan's EFA seed, the seed its item set
#' carries, so `psych::fa()`'s twenty random rotation starts are reproducible.
#'
#' @param efa_rotation_specifications List from [add_efa_rotation_methods()].
#' @param analysis_plan Configuration from [zm_config()]
#'   (`analysis_plan$factor_analysis$efa_estimation_method`).
#' @return List of packed solutions; see [ap4_pack_efa_solution()].
fit_efa_models <- function(efa_rotation_specifications, analysis_plan) {
  rotation_settings <- ap4_require_efa_rotation_settings(analysis_plan)
  efa_rotation_specifications |>
    lapply(function(specification) {
    polychoric_corr_matrix <- specification$set$correlation$value
    result <- ap4_capture_efa_fit(
      specification,
      # The shared settings fix the starts and oblimin gamma. psych's
      # multistart routine drops gam, so the settings guard verifies the
      # installed GPArotation default matches the prespecified zero.
      # The set's seed is reset before each fit; the caller's state is restored.
      ap4_with_efa_seed(specification$set$seed,
        psych::fa(
          r = polychoric_corr_matrix,
          nfactors = specification$factors,
          n.obs = nrow(specification$set$data),
          fm = analysis_plan$factor_analysis$efa_estimation_method,
          rotate = specification$rotation,
          gam = rotation_settings$oblimin_gamma,
          n.rotations = rotation_settings$starting_rotations
        )
      )
    )
    ap4_pack_efa_solution(specification, result)
  })
}

# --- results -----------------------------------------------------------------

#' Assign items in the primary and one-factor solutions by their strongest loading
#'
#' AP9 / SRQ1 item correspondence: each item is assigned to the factor
#' with its largest absolute unrounded pattern loading in each primary
#' solution, independently of the .40 loading-description cutoff.
#'
#' Exact ties retain the first loading column and are flagged.
#'
#' @param efa_fits List from [fit_efa_models()].
#' @param analysis_plan Configuration from [zm_config()]
#'   (`analysis_plan$factor_analysis$rotation_primary`).
#' @return List of primary solution records, each carrying `membership`
#'   (`NULL` for an unavailable solution).
extract_efa_item_assignments <- function(efa_fits, analysis_plan) {
  primary_rotations <- c(analysis_plan$factor_analysis$rotation_primary, "none")
  primary_fits <- ap4_select_primary_efa_solutions(
    efa_fits, primary_rotations
  )
  assignments <- primary_fits$available |>
    lapply(function(one_fit) {
      strongest_factor <- ap4_assign_by_largest_absolute_loading(
        one_fit$loadings, ties = "first"
      )
      ap4_extract_efa_membership(one_fit, strongest_factor)
    })
  ap4_complete_efa_item_assignments(primary_fits, assignments)
}

#' Calculate both directional pair measures and adjusted Rand agreement
#'
#' AP9 / SRQ1 correspondence measures: expected-pair retention,
#' empirical-pair purity and adjusted Rand agreement for each primary solution.
#' Undefined pair denominators remain unavailable. There is no pass/fail rule.
#'
#' Retention conditions on theoretical same-scale pairs; purity conditions on
#' empirically co-assigned pairs. The two directions are kept separate.
#'
#' @param item_assignments List from [extract_efa_item_assignments()].
#' @return List `solutions` (the input) and `summaries` (one row per solution).
extract_efa_item_correspondence <- function(item_assignments) {
  available_assignments <- ap4_available_efa_memberships(item_assignments)
  correspondence <- available_assignments |>
    lapply(function(one_fit) {
      item_pairs <- ap4_enumerate_membership_pairs(one_fit$membership)
      expected_pair_retention <- item_pairs |>
        dplyr::filter(same_theoretical_subscale) |>
        dplyr::summarise(proportion = mean(same_estimated_factor))
      empirical_pair_purity <- item_pairs |>
        dplyr::filter(same_estimated_factor) |>
        dplyr::summarise(proportion = mean(same_theoretical_subscale))
      adjusted_rand <- mclust::adjustedRandIndex(
        one_fit$membership$theoretical_scale,
        one_fit$membership$assigned_factor
      )
      list(
        expected_pair_retention = expected_pair_retention,
        empirical_pair_purity = empirical_pair_purity,
        adjusted_rand = adjusted_rand
      )
  })
  ap4_complete_efa_correspondence(item_assignments, correspondence)
}

# BEGIN GENERATED PARAMETER CARD: AP9 LOADING CLARITY
# Automatically generated from analysis_plan.yaml
#   Substantial loading: absolute value at least 0.4   (factor_analysis.loading_display_cutoff)
#   Descriptive only; the item assignment uses the strongest loading, not this cutoff.
# END GENERATED PARAMETER CARD: AP9 LOADING CLARITY
#' Describe loading clarity at the configured cutoff
#'
#' AP9 / SRQ1 loading description: unrounded absolute pattern loadings
#' at or above .40 describe weak items, cross-loadings and secondary-loading
#' frequency. This descriptive cutoff does not determine factor assignment.
#'
#' Loadings exactly at the configured cutoff count. Item assignments and
#' correspondence measures remain unchanged, and no item-change verdict is
#' made.
#'
#' @param correspondence List from [extract_efa_item_correspondence()].
#' @param analysis_plan Configuration from [zm_config()]
#'   (`analysis_plan$factor_analysis$loading_display_cutoff`).
#' @return The correspondence list with `loading_items` and
#'   `loading_frequencies` appended.
extract_efa_loading_clarity <- function(correspondence, analysis_plan) {
  loading_matrices <- ap4_available_efa_loading_matrices(correspondence$solutions)
  clarity <- loading_matrices |>
    lapply(function(loadings) {
    n_substantial_loadings <- rowSums(abs(loadings) >= analysis_plan$factor_analysis$loading_display_cutoff)
    list(
      n_substantial_loadings = n_substantial_loadings,
      weak = n_substantial_loadings == 0L,
      crossloading = n_substantial_loadings >= 2L,
      n_secondary_substantial_loadings = pmax(
        n_substantial_loadings - 1L, 0L
      )
    )
  })
  ap4_complete_efa_loading_clarity(correspondence, clarity)
}

#' Report per-factor and cumulative variance accounted for in every fitted rotation
#'
#' AP9 / SRQ1 explained variance: for every examined solution, each
#' factor's proportion of variance and the cumulative variance, read from the
#' fitted EFA variance-accounted table. Variance is descriptive for every
#' successfully examined rotation and count; it is not a factor-retention rule.
#'
#' The technical extractor retains unavailable rows and their reason as well.
#'
#' @param efa_fits List from [fit_efa_models()].
#' @return Long tibble `set_name`, `factors`, `rotation`, `factor`,
#'   `proportion_var`, `cumulative_var`, `note`.
extract_efa_explained_variance <- function(efa_fits) {
  efa_fits |>
    lapply(function(one_fit) ap4_extract_efa_variance(one_fit)) |>
    dplyr::bind_rows()
}

#' Report the loadings of every fitted solution, one row per item and factor
#'
#' The complete pattern matrices behind the supplement's membership tables
#' (AP9): every item set, every examined factor count and every rotation. A
#' solution that could not be fitted has no loadings and no rows; the
#' supplement names it with its reason.
#'
#' @param efa_fits List from [fit_efa_models()].
#' @return Long tibble `item_set`, `factors`, `rotation`, `item`, `column`,
#'   `intended_scale`, `factor`, `loading`.
extract_efa_loadings <- function(efa_fits) {
  efa_fits |>
    lapply(function(one_fit) {
      if (!isTRUE(one_fit$ok) || is.null(one_fit$loadings)) return(NULL)
      loadings <- unclass(one_fit$loadings)
      membership <- one_fit$set$item_membership[
        match(rownames(loadings), one_fit$set$item_membership$column), ]
      tibble::tibble(
        item_set = one_fit$set_name, factors = as.integer(one_fit$factors),
        rotation = one_fit$rotation,
        item = rep(membership$item, ncol(loadings)),
        column = rep(membership$column, ncol(loadings)),
        intended_scale = rep(membership$theoretical_scale, ncol(loadings)),
        factor = rep(colnames(loadings), each = nrow(loadings)),
        loading = as.numeric(loadings)
      )
    }) |>
    dplyr::bind_rows()
}

# --- technical helpers -------------------------------------------------------

#' Materialise the item-set compositions into ordered item columns
#'
#' Expands each scale composition into ordered item columns —
#' resolving the source labels to executable columns through the
#' codebook's explicit label-to-column map — item-to-scale membership, and the
#' EFA seed. Respondents with unresolved AP3 values leave only item sets
#' requiring them; item membership and scoring stay fixed.
#'
#' @param data Input table with the executable item columns.
#' @param codebook Codebook from [zm_codebook()].
#' @param item_sets Named list of item source labels per scale.
#' @param set_definitions Named list of scale keys per item set.
#' @param seed EFA seed shared by every set.
#' @return Named list: `name`, `data`, `item_membership` (`item`, `column`,
#'   `theoretical_scale`) and `seed`.
ap4_materialise_efa_item_sets <- function(data, codebook, item_sets, set_definitions, seed) {
  result <- lapply(names(set_definitions), function(name) {
    scales <- set_definitions[[name]]
    items <- unname(unlist(item_sets[scales], use.names = FALSE))
    columns <- zm_item_columns(items, zm_item_column_map(codebook))
    theoretical_scale <- rep(scales, lengths(item_sets[scales]))
    sample <- exclude_rows_with_failed_imputations(data, columns)
    list(
      name = name,
      data = copy_imputation_status(dplyr::select(sample, dplyr::all_of(columns)), sample),
      item_membership = tibble::tibble(
        item = items, column = columns, theoretical_scale = theoretical_scale
      ),
      seed = as.integer(seed)
    )
  })
  stats::setNames(result, names(set_definitions))
}

#' Evaluate one requested correlation calculation
#'
#' Returns either its matrix or its error note, so a failed matrix remains
#' explicit while the raw-data criteria can still run.
#'
#' @param expression The correlation calculation.
#' @return List `ok`, `value`, `note`.
ap4_capture_correlation_result <- function(expression) {
  result <- tryCatch(force(expression), error = identity)
  if (inherits(result, "error")) {
    list(ok = FALSE, value = NULL, note = conditionMessage(result))
  } else {
    list(ok = TRUE, value = result, note = NA_character_)
  }
}

#' Evaluate one requested factor-number or solution operation
#'
#' Returns either its value or its error note, so one unavailable method does
#' not erase the other requested results.
#'
#' @param expression The operation.
#' @return List `ok`, `value`, `note`.
ap4_capture_efa_result <- function(expression) {
  result <- tryCatch(force(expression), error = identity)
  if (inherits(result, "error")) {
    list(ok = FALSE, value = NULL, note = conditionMessage(result))
  } else {
    list(ok = TRUE, value = result, note = NA_character_)
  }
}

#' The availability boundary of one EFA fit
#'
#' The supplied fit expression is not evaluated when specification creation
#' already marked the item-set/count unavailable; otherwise this delegates to
#' [ap4_capture_efa_result()].
#'
#' @param specification One rotation specification.
#' @param expression The fit call.
#' @return List `ok`, `value`, `note`.
ap4_capture_efa_fit <- function(specification, expression) {
  if (!isTRUE(specification$available)) {
    return(list(
      ok = FALSE,
      value = NULL,
      note = specification$unavailable_reason
    ))
  }
  ap4_capture_efa_result(expression)
}

#' Evaluate one method at the item set's seed and restore the caller's RNG state
#'
#' Serves the two parallel analyses and the comparison-data method.
#'
#' @param seed Seed of the item set.
#' @param expression The method call.
#' @return The value of `expression`.
ap4_with_efa_seed <- function(seed, expression) {
  had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = globalenv()) else NULL
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = globalenv())
    } else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      rm(".Random.seed", envir = globalenv())
    }
  }, add = TRUE)
  set.seed(seed)
  force(expression)
}

#' Require the same fixed rotation settings for EFA and VSS trial solutions
#'
#' psych 2.6.5 forwards `gam` for a single rotation but its multistart
#' `faRotations()` drops it. Preserve that procedure and support gamma zero
#' only, checking the actual GPArotation default rather than silently relying
#' on it. No substitute rotation is introduced.
#'
#' @param analysis_plan Configuration from [zm_config()].
#' @return The validated `factor_analysis$rotation_settings` list.
ap4_require_efa_rotation_settings <- function(analysis_plan) {
  settings <- analysis_plan$factor_analysis$rotation_settings
  gamma <- settings$oblimin_gamma
  starts <- settings$starting_rotations
  if (!is.numeric(gamma) || length(gamma) != 1L || !is.finite(gamma) || gamma != 0) {
    stop("AP9: the prespecified oblimin gamma must be zero; psych's multistart routine does not forward gam.")
  }
  if (!is.numeric(starts) || length(starts) != 1L || !is.finite(starts) ||
      starts < 1 || starts != as.integer(starts)) {
    stop("AP9: starting_rotations must be one positive integer.")
  }
  if (starts > 1 && !identical(as.numeric(formals(GPArotation::oblimin)$gam), as.numeric(gamma))) {
    stop("AP9: GPArotation's default oblimin gamma does not match the prespecified zero; psych's multistart routine drops gam.")
  }
  settings
}

#' Apply the field mapping and the VSS rule to each stored method object
#'
#' Anything that is not one finite count becomes an unavailable suggestion with
#' a note. The distinct estimated rows are returned while item sets, complete
#' method results, and every per-method count or failure note are retained as
#' technical attributes.
#'
#' @param efa_factor_number_results Named list of sets with
#'   `factor_number_methods`.
#' @param suggested_count_field Named list, method -> result field (a path of
#'   names for a nested field).
#' @param vss_suggested_count Function extracting the VSS count.
#' @return Tibble `item_set`, `n_factors`, `type` with the technical attributes.
ap4_pack_suggested_factor_numbers <- function(
    efa_factor_number_results, suggested_count_field, vss_suggested_count) {
  extract_method_count <- function(method, result) {
    if (!isTRUE(result$ok)) {
      return(tibble::tibble(
        method = method, suggested_factors = NA_integer_, note = result$note
      ))
    }
    count <- tryCatch({
      if (method == "vss_complexity_1") {
        vss_suggested_count(result$value)
      } else if (method %in% names(suggested_count_field)) {
        result$value[[suggested_count_field[[method]]]]
      } else {
        stop("Unknown factor-number method: ", method)
      }
    }, error = identity)
    valid <- !inherits(count, "error") && length(count) == 1L &&
      is.numeric(count) && !is.na(count) && is.finite(count)
    note <- if (inherits(count, "error")) conditionMessage(count) else result$note
    if (!valid && (length(note) != 1L || is.na(note) || !nzchar(note))) {
      note <- "method did not provide one finite factor count"
    }
    tibble::tibble(
      method = method,
      suggested_factors = if (valid) as.integer(count) else NA_integer_,
      note = if (valid) NA_character_ else as.character(note)
    )
  }
  method_counts <- lapply(efa_factor_number_results, function(set) {
    names(set$factor_number_methods) |>
      lapply(function(method) {
        extract_method_count(method, set$factor_number_methods[[method]])
      }) |>
      dplyr::bind_rows()
  })
  rows <- lapply(seq_along(efa_factor_number_results), function(i) {
    set <- efa_factor_number_results[[i]]
    method_counts[[i]] |>
      dplyr::filter(!is.na(suggested_factors)) |>
      dplyr::transmute(
        item_set = set$name,
        n_factors = as.integer(suggested_factors),
        type = factor("estimated", levels = c("estimated", "theoretical"))
      ) |>
      dplyr::distinct()
  })
  estimated <- dplyr::bind_rows(rows)
  if (!nrow(estimated)) {
    estimated <- tibble::tibble(
      item_set = character(), n_factors = integer(),
      type = factor(character(), levels = c("estimated", "theoretical"))
    )
  }
  set_names <- vapply(efa_factor_number_results, `[[`, character(1), "name")
  item_sets <- lapply(efa_factor_number_results, function(set) {
    set$factor_number_methods <- NULL
    set
  })
  attr(estimated, "item_sets") <- stats::setNames(item_sets, set_names)
  attr(estimated, "factor_number_results") <- stats::setNames(lapply(
    efa_factor_number_results, `[[`, "factor_number_methods"
  ), set_names)
  attr(estimated, "method_counts_by_item_set") <- stats::setNames(
    method_counts, set_names
  )
  estimated
}

#' One typed theoretical row for each configured scale and combined set
#'
#' A combined set is named by its configured key; no name is translated.
#'
#' @param analysis_plan Configuration from [zm_config()]
#'   (`analysis_plan$factor_analysis$sets` and `single_scale_expected_factors`).
#' @return Tibble `item_set`, `n_factors`, `type`.
ap4_theoretical_factor_numbers <- function(analysis_plan) {
  configured_sets <- analysis_plan$factor_analysis$sets
  keys <- vapply(configured_sets, `[[`, character(1), "key")
  individual_scales <- unique(unlist(lapply(configured_sets, `[[`, "scales"), use.names = FALSE))
  theoretical <- tibble::tibble(
    item_set = c(individual_scales, keys),
    n_factors = as.integer(c(
      rep(analysis_plan$factor_analysis$single_scale_expected_factors, length(individual_scales)),
      vapply(configured_sets, function(set) set$expected_factors, numeric(1))
    )),
    type = factor("theoretical", levels = c("estimated", "theoretical"))
  )
  theoretical
}

#' Expand each distinct item-set/count row over the requested rotation choices
#'
#' The original typed factor-number evidence, method provenance, item data and
#' pre-recorded availability are carried into every resulting specification.
#'
#' @param factor_counts Distinct `item_set`/`n_factors` rows.
#' @param factor_number_evidence The typed factor-number tibble with its
#'   technical attributes.
#' @param one_factor,multiple_factors Rotations per count size.
#' @return Unnamed list of specifications.
ap4_expand_efa_rotation_specifications <- function(
    factor_counts, factor_number_evidence, one_factor, multiple_factors) {
  efa_rotation_specifications <- unlist(lapply(seq_len(nrow(factor_counts)), function(row) {
    item_set <- factor_counts$item_set[[row]]
    n_factors <- factor_counts$n_factors[[row]]
    item_sets <- attr(factor_number_evidence, "item_sets")
    method_counts_by_item_set <- attr(
      factor_number_evidence, "method_counts_by_item_set"
    )
    evidence <- factor_number_evidence |>
      dplyr::filter(
        .data$item_set == .env$item_set,
        .data$n_factors == .env$n_factors
      )
    methods <- method_counts_by_item_set[[item_set]] |>
      dplyr::filter(suggested_factors == .env$n_factors) |>
      dplyr::pull(method)
    set <- item_sets[[item_set]]
    correlation_available <- isTRUE(set$correlation$ok)
    count_available <- n_factors >= 1L && n_factors < ncol(set$data)
    unavailable_reason <- if (!correlation_available) {
      set$correlation$note
    } else if (!count_available) {
      "The requested factor count is not estimable for this item set."
    } else {
      NA_character_
    }
    types <- unique(as.character(evidence$type))
    rotations <- if (n_factors == 1L) one_factor else multiple_factors
    lapply(rotations, function(rotation) list(
      set = set,
      set_name = item_set,
      factors = as.integer(n_factors),
      rotation = rotation,
      types = types,
      expected_reference = "theoretical" %in% types,
      suggested_by = methods,
      available = correlation_available && count_available,
      unavailable_reason = unavailable_reason
    ))
  }), recursive = FALSE)
  unname(efa_rotation_specifications)
}

#' Pack one fitted EFA solution with its specification identity
#'
#' Preserves every specification's identity and provenance; validates and
#' orders finite unrounded pattern loadings to the declared items; and extracts
#' factor correlations and the variance-accounted table. Malformed fit output
#' becomes an unavailable record.
#'
#' @param specification The rotation specification.
#' @param result The captured fit.
#' @return List with the specification fields plus `ok`, `note`, `loadings`,
#'   `Phi`, `Vaccounted`.
ap4_pack_efa_solution <- function(specification, result) {
  base <- list(
    set = specification$set,
    set_name = specification$set_name,
    factors = specification$factors,
    rotation = specification$rotation,
    types = specification$types,
    expected_reference = specification$expected_reference,
    suggested_by = specification$suggested_by,
    available = specification$available,
    unavailable_reason = specification$unavailable_reason,
    ok = result$ok,
    note = result$note
  )
  if (!result$ok) {
    return(c(base, list(loadings = NULL, Phi = NULL, Vaccounted = NULL)))
  }
  fit <- result$value
  extracted <- tryCatch({
    loadings <- as.matrix(unclass(fit$loadings))
    items <- specification$set$item_membership$column
    if (is.null(rownames(loadings)) && nrow(loadings) == length(items)) {
      rownames(loadings) <- items
    }
    if (is.null(rownames(loadings)) || anyDuplicated(rownames(loadings)) ||
        !all(items %in% rownames(loadings))) {
      stop("EFA loadings do not contain every declared item exactly once.")
    }
    loadings <- loadings[items, , drop = FALSE]
    if (!length(loadings) || any(!is.finite(loadings))) {
      stop("EFA loadings must be a nonempty finite matrix.")
    }
    list(
      loadings = loadings,
      Phi = if (is.null(fit$Phi)) NULL else as.matrix(fit$Phi),
      Vaccounted = if (is.null(fit$Vaccounted)) NULL else as.matrix(fit$Vaccounted)
    )
  }, error = identity)
  if (inherits(extracted, "error")) {
    base$ok <- FALSE
    base$note <- conditionMessage(extracted)
    return(c(base, list(loadings = NULL, Phi = NULL, Vaccounted = NULL)))
  }
  c(base, list(
    loadings = extracted$loadings,
    Phi = extracted$Phi,
    Vaccounted = extracted$Vaccounted
  ))
}

#' Retain the oblimin multi-factor and unrotated one-factor records
#'
#' Keeps them in their original order, then exposes only available loading
#' matrices for the substantive assignment rule while retaining failed records
#' for completion.
#'
#' @param efa_fits List from [fit_efa_models()].
#' @param primary_rotations The primary rotation plus `"none"`.
#' @return List `all`, `available`, `original_names`.
ap4_select_primary_efa_solutions <- function(efa_fits, primary_rotations) {
  primary <- Filter(function(solution) {
    solution$rotation %in% primary_rotations
  }, efa_fits)
  original_names <- names(primary)
  names(primary) <- sprintf("solution_%03d", seq_along(primary))
  available <- primary[vapply(primary, function(solution) {
    isTRUE(solution$ok) && !is.null(solution$loadings)
  }, logical(1))]
  list(all = primary, available = available, original_names = original_names)
}

#' Assign every loading row to the column with its largest absolute value
#'
#' The first-column convention applies only to exact ties.
#'
#' @param loadings Loading matrix.
#' @param ties `max.col()` tie method.
#' @return Integer column index per row.
ap4_assign_by_largest_absolute_loading <- function(loadings, ties) {
  max.col(abs(loadings), ties.method = ties)
}

#' Build membership rows from an item-ordered loading matrix
#'
#' Retains every absolute and secondary loading and flags exact ties.
#'
#' @param solution One packed solution.
#' @param strongest_factor Strongest-factor column indices.
#' @return Tibble of membership rows.
ap4_extract_efa_membership <- function(solution, strongest_factor) {
  loadings <- solution$loadings
  theoretical <- solution$set$item_membership
  if (is.null(colnames(loadings))) colnames(loadings) <- paste0("F", seq_len(ncol(loadings)))
  absolute <- abs(loadings)
  assigned_index <- as.integer(strongest_factor)
  if (length(assigned_index) != nrow(absolute) ||
      any(assigned_index < 1L | assigned_index > ncol(absolute))) {
    stop("Strongest-factor indices do not match the loading matrix.")
  }
  largest <- absolute[cbind(seq_len(nrow(absolute)), assigned_index)]
  next_largest <- vapply(seq_len(nrow(absolute)), function(row) {
    remaining <- absolute[row, -assigned_index[[row]], drop = TRUE]
    if (!length(remaining)) NA_real_ else max(remaining)
  }, numeric(1))
  secondary <- lapply(seq_len(nrow(absolute)), function(row) {
    absolute[row, -assigned_index[[row]], drop = TRUE]
  })
  tibble::tibble(
    item = theoretical$item,
    theoretical_scale = theoretical$theoretical_scale,
    assigned_factor = colnames(absolute)[assigned_index],
    largest_abs_loading = largest,
    next_largest_abs_loading = next_largest,
    exact_tie = rowSums(absolute == largest) > 1L,
    absolute_loadings = lapply(seq_len(nrow(absolute)), function(row) absolute[row, ]),
    secondary_loadings = secondary
  )
}

#' Attach the calculated membership tables to their primary solution records
#'
#' Unavailable primary solutions are restored with `NULL` membership, and the
#' original list order and names are preserved.
#'
#' @param primary_solutions List from [ap4_select_primary_efa_solutions()].
#' @param assignments Named list of membership tables.
#' @return List of primary solution records carrying `membership`.
ap4_complete_efa_item_assignments <- function(primary_solutions, assignments) {
  completed <- lapply(names(primary_solutions$all), function(key) {
    solution <- primary_solutions$all[[key]]
    solution$membership <- assignments[[key]]
    solution
  })
  if (is.null(primary_solutions$original_names)) {
    names(completed) <- NULL
  } else {
    names(completed) <- primary_solutions$original_names
  }
  completed
}

#' Expose only primary solution records with calculated memberships
#'
#' The completion helper later restores every unavailable record in its
#' original position.
#'
#' @param assigned_solutions List from [extract_efa_item_assignments()].
#' @return Named list under stable technical keys.
ap4_available_efa_memberships <- function(assigned_solutions) {
  keys <- sprintf("solution_%03d", seq_along(assigned_solutions))
  available <- !vapply(assigned_solutions, function(solution) {
    is.null(solution$membership)
  }, logical(1))
  result <- assigned_solutions[available]
  names(result) <- keys[available]
  result
}

#' Enumerate every unordered item pair once
#'
#' Records whether its members share a theoretical scale and an assigned
#' factor.
#'
#' @param membership One membership table.
#' @return Tibble `item_i`, `item_j`, `same_theoretical_subscale`,
#'   `same_estimated_factor`.
ap4_enumerate_membership_pairs <- function(membership) {
  if (nrow(membership) < 2L) {
    return(tibble::tibble(
      item_i = character(), item_j = character(),
      same_theoretical_subscale = logical(), same_estimated_factor = logical()
    ))
  }
  pairs <- utils::combn(seq_len(nrow(membership)), 2L)
  tibble::tibble(
    item_i = membership$item[pairs[1, ]],
    item_j = membership$item[pairs[2, ]],
    same_theoretical_subscale =
      membership$theoretical_scale[pairs[1, ]] == membership$theoretical_scale[pairs[2, ]],
    same_estimated_factor =
      membership$assigned_factor[pairs[1, ]] == membership$assigned_factor[pairs[2, ]]
  )
}

#' Assemble the correspondence result
#'
#' Converts undefined empty-set means to unavailable, normalises the degenerate
#' identical-partition adjusted Rand result to one, restores failed solution
#' rows, and assembles the solutions-plus-summaries result.
#'
#' @param assigned_solutions List of primary solutions with memberships.
#' @param correspondence Named list of per-solution measures.
#' @return List `solutions` and `summaries`.
ap4_complete_efa_correspondence <- function(
    assigned_solutions, correspondence) {
  keys <- sprintf("solution_%03d", seq_along(assigned_solutions))
  summaries <- lapply(seq_along(assigned_solutions), function(i) {
    solution <- assigned_solutions[[i]]
    result <- correspondence[[keys[[i]]]]
    if (is.null(result)) {
      return(tibble::tibble(
        set_name = solution$set_name, factors = solution$factors,
        expected_pair_retention = NA_real_, empirical_pair_purity = NA_real_,
        adjusted_rand = NA_real_, note = solution$note
      ))
    }
    retention <- result$expected_pair_retention$proportion[[1]]
    purity <- result$empirical_pair_purity$proportion[[1]]
    if (is.nan(retention)) retention <- NA_real_
    if (is.nan(purity)) purity <- NA_real_
    adjusted_rand <- result$adjusted_rand
    membership <- solution$membership
    if (!(length(adjusted_rand) == 1L && is.finite(adjusted_rand))) {
      same_partition <- identical(
        outer(as.character(membership$theoretical_scale),
              as.character(membership$theoretical_scale), `==`),
        outer(as.character(membership$assigned_factor),
              as.character(membership$assigned_factor), `==`)
      )
      adjusted_rand <- if (
        length(adjusted_rand) == 1L && is.nan(adjusted_rand) && same_partition
      ) 1 else NA_real_
    }
    tibble::tibble(
      set_name = solution$set_name, factors = solution$factors,
      expected_pair_retention = as.numeric(retention),
      empirical_pair_purity = as.numeric(purity),
      adjusted_rand = as.numeric(adjusted_rand), note = NA_character_
    )
  })
  list(solutions = assigned_solutions, summaries = dplyr::bind_rows(summaries))
}

#' Expose item-ordered loading matrices only for solutions with memberships
#'
#' @param solutions The primary solution records.
#' @return Named list of loading matrices under stable keys.
ap4_available_efa_loading_matrices <- function(solutions) {
  keys <- sprintf("solution_%03d", seq_along(solutions))
  available <- vapply(solutions, function(solution) {
    !is.null(solution$membership) && !is.null(solution$loadings)
  }, logical(1))
  matrices <- lapply(solutions[available], `[[`, "loadings")
  names(matrices) <- keys[available]
  matrices
}

#' Attach the loading-clarity results to item memberships
#'
#' Supplies the typed empty item table when all primary solutions failed,
#' calculates per-solution frequencies, and appends both tables without
#' changing the correspondence results.
#'
#' @param correspondence List from [extract_efa_item_correspondence()].
#' @param clarity Named list of per-solution clarity results.
#' @return The correspondence list plus `loading_items` and
#'   `loading_frequencies`.
ap4_complete_efa_loading_clarity <- function(correspondence, clarity) {
  keys <- sprintf("solution_%03d", seq_along(correspondence$solutions))
  item_rows <- lapply(seq_along(correspondence$solutions), function(i) {
    solution <- correspondence$solutions[[i]]
    result <- clarity[[keys[[i]]]]
    if (is.null(result)) return(NULL)
    membership <- solution$membership
    membership$n_loadings_at_40 <- as.integer(result$n_substantial_loadings)
    membership$weak <- as.logical(result$weak)
    membership$crossloading <- as.logical(result$crossloading)
    membership$n_secondary_loadings_at_40 <- as.integer(
      result$n_secondary_substantial_loadings
    )
    dplyr::mutate(
      membership, set_name = solution$set_name, factors = solution$factors
    )
  })
  available <- Filter(Negate(is.null), item_rows)
  items <- if (length(available)) {
    dplyr::bind_rows(available)
  } else {
    tibble::tibble(
      item = character(), theoretical_scale = character(), assigned_factor = character(),
      largest_abs_loading = numeric(), next_largest_abs_loading = numeric(),
      exact_tie = logical(), absolute_loadings = list(), secondary_loadings = list(),
      n_loadings_at_40 = integer(), weak = logical(), crossloading = logical(),
      n_secondary_loadings_at_40 = integer(), set_name = character(), factors = integer()
    )
  }
  frequencies <- items |>
    dplyr::group_by(set_name, factors) |>
    dplyr::summarise(
      n_items = dplyr::n(),
      n_weak = sum(weak),
      n_crossloading = sum(crossloading),
      n_secondary_loadings_at_40 = sum(n_secondary_loadings_at_40),
      .groups = "drop"
    )
  c(correspondence, list(loading_items = items, loading_frequencies = frequencies))
}

#' Extract the variance-accounted rows of one solution
#'
#' Always returns the complete reporting columns. An available psych
#' `Vaccounted` matrix yields one row per factor; unavailable solutions and
#' fits without `Vaccounted` yield one explicit unavailable row with their
#' distinct reason. A malformed supplied variance table stops with an error.
#'
#' @param solution One packed solution.
#' @return Tibble `set_name`, `factors`, `rotation`, `factor`,
#'   `proportion_var`, `cumulative_var`, `note`.
ap4_extract_efa_variance <- function(solution) {
  unavailable <- function(note) {
    tibble::tibble(
      set_name = solution$set_name,
      factors = solution$factors,
      rotation = solution$rotation,
      factor = NA_character_,
      proportion_var = NA_real_,
      cumulative_var = NA_real_,
      note = note
    )
  }
  if (!isTRUE(solution$ok)) {
    note <- solution$note
    if (is.null(note) || is.na(note) || !nzchar(note)) note <- "EFA solution unavailable."
    return(unavailable(note))
  }
  if (is.null(solution$Vaccounted)) {
    return(unavailable("EFA fit did not provide Vaccounted."))
  }
  variance <- as.matrix(solution$Vaccounted)
  if (is.null(rownames(variance)) || !"Proportion Var" %in% rownames(variance)) {
    stop("EFA variance table lacks Proportion Var.")
  }
  factor_names <- colnames(variance)
  if (is.null(factor_names)) factor_names <- paste0("F", seq_len(ncol(variance)))
  proportion_var <- as.numeric(variance["Proportion Var", ])
  cumulative_var <- if ("Cumulative Var" %in% rownames(variance)) {
    as.numeric(variance["Cumulative Var", ])
  } else {
    cumsum(proportion_var)
  }
  tibble::tibble(
    set_name = solution$set_name,
    factors = solution$factors,
    rotation = solution$rotation,
    factor = factor_names,
    proportion_var = proportion_var,
    cumulative_var = cumulative_var,
    note = NA_character_
  )
}
