# AP4 reliability (descriptive only)
# Implements AP4: internal consistency is McDonald's omega total from psych::omega on
# polychoric correlations, with Cronbach's alpha as the fallback when the omega factor
# model does not fit.
# The exploratory factor analysis is in R/ap9_efa.R and the confirmatory models
# are in R/ap4_factor_structure.R; this file keeps the reliability calculations
# and the fit-index version sentence the CFA tables print.
# Input items have already been reversed and filled by AP3, so every scale is
# estimated on the participants with complete inputs for that scale. No item is selected or removed on the
# basis of these results.
#
# Technical note on the omega interval: psych 2.6.5 cannot bootstrap omega with
# poly = TRUE (its internal bootstrap loop fails), so the interval is a percentile
# bootstrap computed here: rows are resampled, the polychoric matrix is re-estimated,
# a one-factor minres solution is fitted and omega_t = 1 - sum(uniquenesses) / sum(R),
# which is exactly psych::omega's omega_t formula for one factor. Cronbach's alpha is
# bootstrapped on the same resamples from the raw covariance matrix.
# Omega interval rule (`analysis_plan$reliability$omega_interval_min_success`): the
# omega interval is reported only when at least that share of the resamples yields an
# omega; otherwise the omega point estimate is shown without an interval.
# Alpha is reported separately with its own bootstrap interval.

# --- polychoric correlations: why every call passes correct = 0 --------------
#
# `psych::polychoric()` (and `psych::fa(cor = "poly")`, which calls it) estimates
# each item's thresholds once from that item's own marginal and then estimates
# every pair's correlation with those thresholds held fixed — the standard
# two-step estimator, psych's `global = TRUE` path. It abandons that path by
# itself, without being asked, as soon as the items of a block do not all reach
# the same highest response category:
#
#     xmax <- apply(x, 2, max); if (min(xmax) != max(xmax)) global <- FALSE
#
# In the `global = FALSE` path psych re-estimates the thresholds inside each
# pair's contingency table, and there its continuity correction for empty cells
# is applied to a table it has already normalised to sum to one: every empty
# cell gains `correct / n`, so the corrected table sums to more than one, the
# cumulative marginals pass 1 and `qnorm()` returns NaN. `optimize()` then fails
# with "missing value where TRUE/FALSE needed" for the affected pairs,
# `mcmapply()` hands back a list instead of a matrix, and psych returns that
# list with a warning instead of stopping — after which the caller dies inside
# `stats::cor()` with a message that names neither the block nor the cause.
#
# `correct = 0` switches that correction off, which is what psych's own warning
# recommends. It is not a change of estimator: for complete-case data a pair's
# marginals ARE the two items' marginals, so with no correction the per-pair
# thresholds equal the global ones and the two paths agree to ~1e-11 (verified
# on every AP4 set). Where all items reach the top category psych stays on the
# global path, where `correct` is never read, so the value is inert there.
# Passing it therefore restores one estimator for every block instead of
# leaving some blocks on a mis-normalised variant of it.
ap4_polychoric_correct <- 0


#' Cronbach's alpha from a raw covariance matrix
#'
#' Alpha is undefined for a single item, for a single case and for items
#' without any variance; each of those returns `NaN` from the formula, which
#' would travel into the reliability table as if it were an estimate, so they
#' stop instead. Inside the bootstrap the stop is caught and counts the
#' resample as failed, exactly as the `NaN` did.
#'
#' @param x Data frame or matrix of item scores.
#' @return Raw alpha (numeric scalar).
ap4_alpha_raw <- function(x) {
  k <- ncol(x)
  if (k < 2) {
    stop("ap4_alpha_raw(): Cronbach's alpha needs at least two items, got ", k, ".")
  }
  if (nrow(x) < 2) {
    stop("ap4_alpha_raw(): Cronbach's alpha needs at least two cases, got ", nrow(x), ".")
  }
  C <- stats::cov(x)
  total <- sum(C)
  if (!is.finite(total) || total == 0) {
    stop(
      "ap4_alpha_raw(): the items have no variance (total covariance ", format(total),
      "); Cronbach's alpha is undefined."
    )
  }
  k / (k - 1) * (1 - sum(diag(C)) / total)
}

#' Omega total of a one-factor model on a correlation matrix
#'
#' Reproduces psych::omega's omega_t for `nfactors = 1`: a minres one-factor
#' solution, then `1 - sum(uniquenesses) / sum(R)`.
#'
#' @param R Correlation matrix (polychoric).
#' @param n_obs Number of observations behind `R`.
#' @return Omega total (numeric scalar).
ap4_omega_t <- function(R, n_obs) {
  f <- psych::fa(R, nfactors = 1, fm = "minres", n.obs = n_obs, warnings = FALSE)
  omega <- 1 - sum(f$uniquenesses) / sum(R)
  if (!is.finite(omega) || omega <= 0 || omega > 1) stop("omega estimate is outside (0, 1]")
  omega
}

# The AP4 reliability result ----------------------------------------------------
# One progressive object: the point coefficients of the nine registered item
# sets, then the participant-bootstrap intervals, then the registered fallback
# rule. The item sets are fixed here, by their source labels; the codebook's
# label-to-column map resolves them to the data columns.

#' Polychoric matrix passed to `psych::omega`
#'
#' Rejects a flat registered item and returns the finite polychoric matrix. It
#' never drops an item: an estimate over fewer items would describe a different
#' scale and must use the alpha fallback instead.
#'
#' @param responses Data frame with one column per registered item.
#' @return Polychoric correlation matrix.
ap4_prepare_omega_input <- function(responses) {
  spread <- vapply(responses, stats::sd, numeric(1))
  flat <- names(responses)[!is.finite(spread) | spread == 0]
  if (length(flat))
    stop("omega cannot use item(s) without variance: ",
      paste(flat, collapse = ", "))
  R <- suppressWarnings(psych::polychoric(
    responses, correct = ap4_polychoric_correct)$rho)
  if (!is.matrix(R) || any(!is.finite(R)) ||
      !identical(dim(R), c(ncol(responses), ncol(responses))))
    stop("The polychoric matrix is not finite for every registered item.")
  R
}

#' Evaluate one reliability package call and keep its exact error
#'
#' Suppresses that call's console chatter and returns either the fit or its
#' message. It does not decide the scientific fallback.
#'
#' @param expression One package call, evaluated here.
#' @return List `ok`, `value`, `error`.
ap4_capture_reliability_failure <- function(expression) {
  tryCatch({
    utils::capture.output(value <- force(expression))
    list(ok = TRUE, value = value, error = NA_character_)
  }, error = function(error) {
    list(ok = FALSE, value = NULL, error = conditionMessage(error))
  })
}

#' Omega total and raw alpha of one scale, with their failure notes
#'
#' Rejects an unusable omega or one that omitted a registered item. If
#' `psych::alpha` fails, the equivalent raw-covariance formula is attempted;
#' failure of both leaves alpha explicitly unavailable.
#'
#' @param omega_fit Captured `psych::omega` result.
#' @param alpha_fit Captured `psych::alpha` result.
#' @param responses Data frame with one column per registered item.
#' @return List `omega`, `alpha`, `note`.
ap4_extract_reliability_coefficients <- function(
    omega_fit, alpha_fit, responses) {
  omega <- if (omega_fit$ok)
    as.numeric(omega_fit$value$omega.tot)[1L] else NA_real_
  dropped <- if (omega_fit$ok)
    setdiff(names(responses), rownames(omega_fit$value$schmid$sl)) else character(0)
  omega_ok <- is.finite(omega) && omega > 0 && omega <= 1 && !length(dropped)
  omega_note <- if (!omega_fit$ok) omega_fit$error else if (length(dropped))
    paste("psych::omega dropped:", paste(dropped, collapse = ", ")) else if (!omega_ok)
    paste("psych::omega returned an unusable value:", format(omega)) else NA_character_
  if (!omega_ok) omega <- NA_real_
  alpha <- if (alpha_fit$ok)
    as.numeric(alpha_fit$value$total$raw_alpha)[1L] else NA_real_
  alpha_note <- NA_character_
  if (!is.finite(alpha)) {
    raw_alpha <- tryCatch(ap4_alpha_raw(responses), error = identity)
    if (inherits(raw_alpha, "error") || length(raw_alpha) != 1L ||
        !is.finite(raw_alpha)) {
      alpha <- NA_real_
      alpha_note <- if (inherits(raw_alpha, "error"))
        paste("alpha unavailable:", conditionMessage(raw_alpha)) else
        paste("alpha unavailable: raw formula returned", format(raw_alpha))
    } else {
      alpha <- as.numeric(raw_alpha)[1L]
      alpha_note <- "psych::alpha unavailable; raw-covariance alpha used"
    }
  }
  notes <- stats::na.omit(c(omega_note, alpha_note))
  list(omega = omega, alpha = alpha,
    note = if (length(notes)) paste(notes, collapse = "; ") else NA_character_)
}

#' Participant-bootstrap draws of omega total and alpha for one scale
#'
#' Creates the configured resample indices, retains a failed draw as `NA` and
#' returns the coefficient matrix; it chooses no interval and no fallback.
#'
#' @param responses Data frame with one column per registered item.
#' @param n_resamples Number of participant resamples.
#' @param seed Seed set before drawing the resamples.
#' @param resample_unit Resampling unit; participants.
#' @param replace Whether participants are drawn with replacement.
#' @param recompute Which coefficients each resample recomputes.
#' @param omega_available Whether the point omega model fitted at all.
#' @return Numeric matrix with rows `omega`, `alpha` and `n_resamples` columns.
ap4_participant_bootstrap_draws <- function(
    responses, n_resamples, seed, resample_unit, replace,
    recompute, omega_available) {
  n <- nrow(responses)
  set.seed(seed)
  draws <- vapply(seq_len(n_resamples), function(i) {
    rows <- sample.int(n, n, replace = replace)
    sampled <- responses[rows, , drop = FALSE]
    alpha <- if ("alpha" %in% recompute) tryCatch(
      ap4_alpha_raw(sampled), error = function(error) NA_real_) else NA_real_
    omega <- if (omega_available && "omega" %in% recompute) tryCatch(
      ap4_omega_t(ap4_prepare_omega_input(sampled), n),
      error = function(error) NA_real_) else NA_real_
    c(omega = omega, alpha = alpha)
  }, numeric(2))
  matrix(draws, nrow = 2, dimnames = list(c("omega", "alpha"), NULL))
}

#' Percentile interval over the draws of one coefficient that succeeded
#'
#' An entirely failed coefficient yields an unavailable interval.
#'
#' @param draws Numeric vector of draws of one coefficient.
#' @param probabilities The two percentile limits.
#' @return Numeric vector of length two.
ap4_finite_percentile_interval <- function(draws, probabilities) {
  draws <- draws[is.finite(draws)]
  if (!length(draws)) return(c(NA_real_, NA_real_))
  unname(stats::quantile(draws, probabilities))
}

#' AP4 — point reliability coefficients for the nine registered scales
#'
#' McDonald's omega total via `psych::omega` and Cronbach's alpha via
#' `psych::alpha`, preserving every registered item. Respondents with unresolved
#' AP3 values are excluded only from scales requiring them. Reliability values
#' are descriptive and cause no exclusions.
#'
#' @param data The descriptive and reliability input
#'   (`data_descriptive_reliability`).
#' @param codebook Codebook from [zm_codebook()]; its label-to-column map
#'   resolves the registered item labels to the executable data columns.
#' @return List `item_sets`, `scales`, `n_participants`, `bootstrap` and
#'   `reported`; the last two are filled by the two verbs below.
estimate_reliability_coefficients <- function(data, codebook) {
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
  scales <- Map(function(scale, item_codes) {
    columns <- zm_item_columns(item_codes, zm_item_column_map(codebook))
    sample <- exclude_rows_with_failed_imputations(data, columns)
    responses <- sample[columns]
    note <- describe_unavailable_imputation_inputs(sample, columns)
    if (!is.null(note)) return(list(scale_key = scale, item_codes = item_codes,
      responses = responses, coefficients = list(omega = NA_real_, alpha = NA_real_, note = note),
      unavailable_imputation = TRUE))
    omega_fit <- ap4_capture_reliability_failure(
      psych::omega(
        ap4_prepare_omega_input(responses),
        nfactors = 1, poly = FALSE, n.obs = nrow(responses),
        n.iter = 1, plot = FALSE, flip = FALSE))
    alpha_fit <- ap4_capture_reliability_failure(
      psych::alpha(responses, check.keys = FALSE, warnings = FALSE))
    coefficients <- ap4_extract_reliability_coefficients(
      omega_fit, alpha_fit, responses)
    list(scale_key = scale, item_codes = item_codes,
      responses = responses, coefficients = coefficients)
  }, names(item_sets), item_sets)
  list(item_sets = item_sets, scales = scales,
    n_participants = nrow(data), bootstrap = NULL, reported = NULL)
}

# BEGIN GENERATED PARAMETER CARD: AP4 RELIABILITY BOOTSTRAP SEED
# Automatically generated from analysis_plan.yaml
#   Bootstrap seed: 1   (reliability.bootstrap_seed)
# Set before each scale's participant resampling; every scale starts from this
# same seed, so scales with the same number of participants draw the same
# participant index sets.
# END GENERATED PARAMETER CARD: AP4 RELIABILITY BOOTSTRAP SEED

# BEGIN GENERATED PARAMETER CARD: AP4 RELIABILITY INTERVALS
# Automatically generated from analysis_plan.yaml
#   Participant resamples per scale: 1000   (profiles.full.reliability_bootstrap_n)
#   Percentile interval level: 0.95   (reliability.interval_level)
#   Quantiles 0.025 and 0.975 of the finite resample coefficients
# END GENERATED PARAMETER CARD: AP4 RELIABILITY INTERVALS
#' AP4 — participant-bootstrap reliability intervals
#'
#' Participants are resampled with replacement within each scale. Each resample
#' recomputes alpha and, when the point omega model fitted, the same one-factor
#' omega total. The interval is a 95% nonparametric percentile interval;
#' `omega_success` is the share of configured resamples that yielded a finite
#' omega. The intervals are descriptive and change no analysis input.
#'
#' Every scale's bootstrap starts from the same seed, `reliability$bootstrap_seed`
#' of the plan, so scales with the same number of participants draw the same
#' participant index sets.
#'
#' @param reliability The progressive object from
#'   [estimate_reliability_coefficients()].
#' @param analysis_plan Configuration from [zm_config()]
#'   (`analysis_plan$reliability$bootstrap_n`, `analysis_plan$reliability$bootstrap_seed`,
#'   `analysis_plan$reliability$interval_level`).
#' @return `reliability` with its `bootstrap` element filled.
add_bootstrap_intervals <- function(reliability, analysis_plan) {
  reliability$bootstrap <- calculate_reliability_bootstrap(reliability, analysis_plan)
  reliability
}

#' The compact bootstrap intervals and success counts, without response matrices
calculate_reliability_bootstrap <- function(reliability, analysis_plan) {
  n_resamples <- analysis_plan$reliability$bootstrap_n
  bootstrap_seed <- as.integer(analysis_plan$reliability$bootstrap_seed)
  interval_probabilities <- c((1 - analysis_plan$reliability$interval_level) / 2,
    1 - (1 - analysis_plan$reliability$interval_level) / 2)
  reliability$scales |>
    lapply(function(scale) {
    omega_available <- is.finite(scale$coefficients$omega)
    draws <- if (isTRUE(scale$unavailable_imputation)) {
      matrix(NA_real_, nrow = 2L, ncol = n_resamples, dimnames = list(c("omega", "alpha"), NULL))
    } else ap4_participant_bootstrap_draws(
      scale$responses, n_resamples = n_resamples, seed = bootstrap_seed,
      resample_unit = "participant", replace = TRUE,
      recompute = c("omega", "alpha"),
      omega_available = omega_available)
    list(
      omega_interval = ap4_finite_percentile_interval(
        draws["omega", ], interval_probabilities),
      alpha_interval = ap4_finite_percentile_interval(
        draws["alpha", ], interval_probabilities),
      omega_success = if (omega_available)
        mean(is.finite(draws["omega", ])) else NA_real_,
      n_boot_omega = sum(is.finite(draws["omega", ])),
      n_boot_alpha = sum(is.finite(draws["alpha", ])),
      n_resamples = n_resamples)
  })
}

# BEGIN GENERATED PARAMETER CARD: AP4 RELIABILITY FALLBACK
# Automatically generated from analysis_plan.yaml
#   Minimum share of resamples with a finite omega: 0.8   (reliability.omega_interval_min_success)
#   At or above it omega is reported with its interval; below it, omega without an interval.
#   Alpha is reported separately with its own bootstrap interval.
# END GENERATED PARAMETER CARD: AP4 RELIABILITY FALLBACK
#' AP4 — apply the registered reliability fallback rule
#'
#' The reported point coefficient and interval are chosen only after
#' estimation: the omega model failed reports alpha and its interval; otherwise
#' an omega bootstrap success at or above the configured share reports omega
#' and its interval; otherwise the omega point estimate is reported without
#' an interval. Alpha retains its own interval. A branch that needs
#' alpha without a finite alpha and a finite alpha interval reports the
#' registered reliability result as unavailable and retains the failure note.
#'
#' @param reliability The progressive object after
#'   [add_bootstrap_intervals()].
#' @param analysis_plan Configuration from [zm_config()]
#'   (`analysis_plan$reliability$omega_interval_min_success`).
#' @return `reliability` with its `reported` tibble filled.
apply_reliability_fallback <- function(reliability, analysis_plan) {
  minimum_omega_success <- analysis_plan$reliability$omega_interval_min_success  # .80
  rows <- Map(function(scale, bootstrap) {
    omega <- scale$coefficients$omega
    alpha <- scale$coefficients$alpha
    alpha_fallback_available <- is.finite(alpha) &&
      all(is.finite(bootstrap$alpha_interval))
    result_note <- scale$coefficients$note
    if (!is.finite(omega) && !alpha_fallback_available) {
      reported_method <- "unavailable"
      estimate <- NA_real_
      interval <- c(NA_real_, NA_real_)
      interval_source <- "unavailable_alpha_fallback_failed"
      result_note <- paste(stats::na.omit(c(result_note,
        "omega failed and a complete alpha fallback was unavailable")),
        collapse = "; ")
    } else if (!is.finite(omega)) {
      reported_method <- "alpha"
      estimate <- alpha
      interval <- bootstrap$alpha_interval
      interval_source <- "alpha_fallback_omega_model_failed"
    } else if (isTRUE(bootstrap$omega_success >= minimum_omega_success) &&
               all(is.finite(bootstrap$omega_interval))) {
      reported_method <- "omega"
      estimate <- omega
      interval <- bootstrap$omega_interval
      interval_source <- "omega_bootstrap"
    } else {
      reported_method <- "omega"
      estimate <- omega
      interval <- c(NA_real_, NA_real_)
      interval_source <- "unavailable_omega_bootstrap"
    }
    tibble::tibble(
      scale_key = scale$scale_key, n_items = length(scale$item_codes),
      n = nrow(scale$responses), omega_t = omega, alpha = alpha,
      reported_method = reported_method, estimate = estimate,
      interval_lower = interval[1], interval_upper = interval[2],
      interval_source = interval_source,
      n_resamples = bootstrap$n_resamples,
      n_boot_omega = bootstrap$n_boot_omega,
      n_boot_alpha = bootstrap$n_boot_alpha,
      omega_success = bootstrap$omega_success,
      note = if (!is.na(result_note) && nzchar(result_note))
        result_note else NA_character_)
  }, reliability$scales, reliability$bootstrap)
  reliability$reported <- dplyr::bind_rows(rows)
  reliability
}

#' The one sentence that names the version of every reported fit index
#'
#' Every table that prints CFA fit indices carries this string, so a reader
#' never has to infer which of lavaan's three versions of a scaled index is on
#' the page.
#'
#' @return Character scalar.
ap4_fit_index_version <- function() {
  paste0(
    "robust (Brosseau-Liard & Savalei correction) for CFI, TLI, RMSEA; ",
    "scaled chi-square"
  )
}
