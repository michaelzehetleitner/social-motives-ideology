# Pairwise Bayesian Pearson correlations. These are separate bivariate
# analyses, not draws from a joint nine-variable correlation matrix.
#
# Public package API and prior definition verified against the author's source:
# https://github.com/richarddmorey/BayesFactor/blob/master/pkg/BayesFactor/R/correlationBF.R
# The posterior uses the package's independent-candidate Metropolis sampler;
# Bayes factors come directly from correlationBF(), not posterior-density fits.

#' Classify a pairwise correlation Bayes factor on its log scale
#'
#' BF10 compares a nonzero correlation with the point null rho = 0. Equality
#' belongs to the evidence category at both thresholds. Log input preserves
#' classifications when an ordinary Bayes factor overflows or underflows.
classify_correlation_evidence <- function(log_bf10, bf_cutoff = 3) {
  if (length(bf_cutoff) != 1L || !is.finite(bf_cutoff) || bf_cutoff <= 1) {
    stop("bf_cutoff must be a finite number greater than one.", call. = FALSE)
  }
  evidence <- rep("inconclusive", length(log_bf10))
  evidence[!is.na(log_bf10) & log_bf10 >= log(bf_cutoff)] <- "present"
  evidence[!is.na(log_bf10) & log_bf10 <= log(1 / bf_cutoff)] <- "absent"
  evidence[is.na(log_bf10)] <- NA_character_
  evidence
}

#' Calculate the nine score correlations under the accepted stretched-beta prior
#'
#' The medium prior is Beta(3,3) shifted and scaled to [-1,1], i.e. rscale=1/3.
#' Each pair uses its own complete observations. Posterior summaries condition
#' on the continuous alternative and are not model-averaged with a point mass
#' at zero. The BF classification is independent of whether a CrI includes zero.
#'
#' @param data Prepared descriptive data with the nine raw scale-score columns.
#' @param codebook A codebook with scales$scale_key and scales$label.
#' @param config Optional configuration; descriptives$bayesian_correlations
#'   supplies settings unless the corresponding argument is explicitly passed.
#' @param rscale Positive numeric prior scale passed to correlationBF().
#' @param posterior_iterations Number of retained package posterior samples.
#' @param seed Integer base seed; pair k uses seed+k-1. The caller's RNG state
#'   and RNG kind are restored on exit.
#' @param interval_level Equal-tailed posterior credible interval probability.
#' @param bf_cutoff BF10 threshold; the reciprocal is the threshold for absence.
#' @return A list with a posterior-median matrix, a separately named empirical
#'   Pearson matrix, pairwise n, labels, complete pair summaries and metadata.
calculate_bayesian_score_correlations <- function(
    data, codebook, config = NULL, rscale = 1 / 3,
    posterior_iterations = 10000L, seed = 20261002L,
    interval_level = .95, bf_cutoff = 3) {
  settings <- config$descriptives$bayesian_correlations
  if (!is.null(settings)) {
    if (!identical(settings$package, "BayesFactor")) {
      stop("The configured Bayesian correlation package must be BayesFactor.", call. = FALSE)
    }
    if (missing(rscale) && !is.null(settings$rscale)) rscale <- settings$rscale
    if (missing(posterior_iterations) && !is.null(settings$posterior_iterations)) {
      posterior_iterations <- settings$posterior_iterations
    }
    if (missing(seed) && !is.null(settings$seed)) seed <- settings$seed
    if (missing(interval_level) && !is.null(settings$interval_level)) interval_level <- settings$interval_level
    if (missing(bf_cutoff) && !is.null(settings$bf_cutoff)) bf_cutoff <- settings$bf_cutoff
  }
  # The nine scale scores, in the codebook's order.
  score_keys <- as.character(codebook$scales$scale_key)
  missing_keys <- setdiff(score_keys, names(data))
  if (length(missing_keys)) {
    stop("Missing score columns: ", paste(missing_keys, collapse = ", "), call. = FALSE)
  }
  if (!all(vapply(data[score_keys], is.numeric, logical(1)))) {
    stop("Every score column must be numeric.", call. = FALSE)
  }
  if (length(rscale) != 1L || !is.finite(rscale) || rscale <= 0) {
    stop("rscale must be a positive finite number.", call. = FALSE)
  }
  if (length(interval_level) != 1L || !is.finite(interval_level) ||
      interval_level <= 0 || interval_level >= 1) {
    stop("interval_level must lie strictly between zero and one.", call. = FALSE)
  }
  if (length(posterior_iterations) != 1L || !is.finite(posterior_iterations) ||
      posterior_iterations < 100L || posterior_iterations != as.integer(posterior_iterations)) {
    stop("posterior_iterations must be an integer of at least 100.", call. = FALSE)
  }
  pair_indices <- utils::combn(seq_along(score_keys), 2L)
  if (length(seed) != 1L || !is.finite(seed) || seed < 0 ||
      seed > .Machine$integer.max - ncol(pair_indices) || seed != as.integer(seed)) {
    stop("seed must be a nonnegative integer leaving room for all pair seeds.", call. = FALSE)
  }
  classify_correlation_evidence(numeric(), bf_cutoff)
  if (!requireNamespace("BayesFactor", quietly = TRUE)) {
    stop("Package 'BayesFactor' is required for the accepted pairwise correlation method.",
         call. = FALSE)
  }
  old_kind <- RNGkind()
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  on.exit({
    do.call(RNGkind, as.list(old_kind))
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  scores <- as.matrix(data[score_keys])
  if (any(is.infinite(scores))) {
    stop("Score columns contain infinite values; correct these before analysis.", call. = FALSE)
  }
  dim_names <- list(score_keys, score_keys)
  posterior_matrix <- matrix(NA_real_, length(score_keys), length(score_keys), dimnames = dim_names)
  empirical_matrix <- posterior_matrix
  pairwise_n <- crossprod(!is.na(scores))
  dimnames(pairwise_n) <- dim_names
  estimable_score <- vapply(seq_along(score_keys), function(i) {
    observed <- scores[!is.na(scores[, i]), i]
    length(observed) >= 3L && stats::sd(observed) > 0
  }, logical(1))
  diag(posterior_matrix) <- diag(empirical_matrix) <- ifelse(estimable_score, 1, NA_real_)
  interval_probs <- c((1 - interval_level) / 2, .5, 1 - (1 - interval_level) / 2)
  pairs <- lapply(seq_len(ncol(pair_indices)), function(k) {
    i <- pair_indices[1L, k]
    j <- pair_indices[2L, k]
    complete <- stats::complete.cases(scores[, c(i, j), drop = FALSE])
    x <- scores[complete, i]
    y <- scores[complete, j]
    n <- length(x)
    pair <- data.frame(
      var_i = score_keys[i], var_j = score_keys[j], n = as.integer(n),
      empirical_r = NA_real_, median = NA_real_, lower = NA_real_, upper = NA_real_,
      log_bf10 = NA_real_, bf10 = NA_real_, bf01 = NA_real_,
      evidence = NA_character_, pair_seed = as.integer(seed + k - 1L),
      posterior_iterations = as.integer(posterior_iterations), posterior_ess = NA_real_,
      median_mcse = NA_real_,
      status = "ok", stringsAsFactors = FALSE
    )
    if (n < 3L) {
      pair$status <- "Fewer than three complete observations"
      return(pair)
    }
    if (stats::sd(x) == 0 || stats::sd(y) == 0) {
      pair$status <- "At least one score is constant within this pair"
      return(pair)
    }
    pair$empirical_r <- stats::cor(x, y)
    # A perfect correlation can compute as 1 minus a rounding error.
    if (abs(pair$empirical_r) >= 1 - sqrt(.Machine$double.eps)) {
      pair$status <- "Perfect empirical correlation; continuous-model calculation not estimated"
      return(pair)
    }
    # Unexpected package errors stop the target rather than silently converting
    # computational failures into unmarked evidence categories.
    bf <- BayesFactor::correlationBF(x = x, y = y, rscale = rscale)
    log_bf <- as.numeric(BayesFactor::extractBF(bf, logbf = TRUE, onlybf = TRUE))
    if (length(log_bf) != 1L || is.na(log_bf)) {
      stop("Invalid Bayes factor for ", score_keys[i], " and ", score_keys[j], call. = FALSE)
    }
    pair$log_bf10 <- log_bf
    pair$bf10 <- exp(log_bf)
    pair$bf01 <- exp(-log_bf)
    pair$evidence <- classify_correlation_evidence(log_bf, bf_cutoff)
    set.seed(pair$pair_seed)
    samples <- BayesFactor::posterior(bf, iterations = posterior_iterations, progress = FALSE)
    samples_matrix <- as.matrix(samples)
    if (!"rho" %in% colnames(samples_matrix)) {
      stop("BayesFactor posterior has no named rho column.", call. = FALSE)
    }
    rho <- as.numeric(samples_matrix[, "rho"])
    if (length(rho) != posterior_iterations || any(!is.finite(rho)) || any(abs(rho) > 1)) {
      stop("Invalid posterior draws for ", score_keys[i], " and ", score_keys[j], call. = FALSE)
    }
    summary <- stats::quantile(rho, interval_probs, names = FALSE)
    pair$lower <- summary[1L]
    pair$median <- summary[2L]
    pair$upper <- summary[3L]
    pair$posterior_ess <- unname(coda::effectiveSize(rho))
    pair$median_mcse <- unname(posterior::mcse_quantile(rho, probs = .5))
    pair
  })
  pairs <- do.call(rbind, pairs)
  for (k in seq_len(nrow(pairs))) {
    i <- match(pairs$var_i[k], score_keys)
    j <- match(pairs$var_j[k], score_keys)
    posterior_matrix[i, j] <- posterior_matrix[j, i] <- pairs$median[k]
    empirical_matrix[i, j] <- empirical_matrix[j, i] <- pairs$empirical_r[k]
  }
  score_labels <- stats::setNames(codebook$scales$label, codebook$scales$scale_key)
  labels <- unname(score_labels[score_keys])
  labels[is.na(labels)] <- score_keys[is.na(labels)]
  list(
    matrix = posterior_matrix, empirical_matrix = empirical_matrix,
    n = pairwise_n, labels = labels, method = "Bayesian Pearson correlation (pairwise)",
    pairs = pairs,
    metadata = list(
      package = "BayesFactor", package_version = utils::packageDescription("BayesFactor")$Version,
      rscale = rscale, prior_beta = rep(1 / rscale, 2L), prior_support = c(-1, 1),
      bf_cutoff = bf_cutoff, interval_level = interval_level,
      posterior_iterations = as.integer(posterior_iterations), seed = as.integer(seed),
      posterior_summary = "Median and equal-tailed interval conditional on the continuous alternative",
      posterior_method = "BayesFactor::posterior; independent-candidate Metropolis sampling",
      rng_kind = RNGkind(), missingness = "pairwise complete observations"
    )
  )
}
