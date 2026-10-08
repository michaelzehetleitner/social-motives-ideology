# AP8 / RQ3 — the bagged Gaussian-copula network of the nine standardised scores.
#
# The design of the network analysis: one nine-node
# Gaussian-copula graph (five motives, four authoritarian-orientation facets,
# no covariates), B participant bootstrap samples drawn once and kept, one
# easybgm/BDgraph fit per sample with a single same-data retry, the bagged
# posterior inclusion probabilities and model-averaged weights averaged over
# the successful fits before any Bayes-factor conversion, and the registered
# decision rule applied to those bagged Bayes factors. Beside the bag stand
# three descriptive comparisons: one full-sample fit, the same fit at each
# swept edge prior, and the successful-resample PIP distribution per edge.
#
# AP3 hands over `data_network`: the nine already standardised node columns,
# complete, in the configured order. Nothing here selects participants,
# standardises again, or rounds; BDgraph performs its own rank-based normal
# score transform inside each fit.
#
# A failed fit stays a failed attempt with its identity, seed and message. It
# never becomes an absent edge: when fewer than `min_success_rate` of the
# attempted fits succeed, the bag is computationally infeasible and the edge
# table is empty rather than partial, so no downstream verb needs an
# availability guard and none of them can read a failure as evidence of
# conditional independence.
#
# The network tables of the report are built from these results in
# R/report_results_network.R and R/report_supplement_network_detail.R. They
# recompute nothing.

# --- the model and its participant resamples ---------------------------------

# BEGIN GENERATED PARAMETER CARD: AP8 NETWORK MODEL
# Automatically generated from analysis_plan.yaml
# Nodes (9, in the order of codebook_scales.csv)   (network.nodes)
#   zm_security, zm_arousal, zm_power, zm_prestige, zm_achievement, asc_agg, asc_sub, asc_conv,
#   sdo_dom
# Estimation
#   Package: BDgraph   (network.package)
#   Sweeps per fit: 50000   (network.iter)
#   Continuity indicator per node: 0   (network.not_cont; all nodes continuous)
# Priors
#   Edge inclusion prior: 0.5   (network.g_prior)
#   Degrees of freedom: 3   (network.df_prior)
#   Swept edge priors: 0.25, 0.5, 0.75   (network.g_prior_sweep; descriptive only)
# Edge decision rule
#   Present when the Bayes factor is at least 10   (network.bf_include)
#   Absent when it is at most 0.1   (network.bf_exclude)
#   Inconclusive in between.
# END GENERATED PARAMETER CARD: AP8 NETWORK MODEL
#' Declare the nine-node Gaussian-copula network
#'
#' AP8 / RQ3 network: the graph contains the five motives and the four
#' authoritarian-orientation facets, with no demographic covariates. AP3
#' supplies complete, already standardised node scores; BDgraph performs its
#' rank-based normal-score transform within each fit.
#'
#' This definition fixes the scientific node set and its order, and the per-fit
#' model settings. It neither filters nor restandardises the AP3 input, whose
#' nine canonical z columns (`analysis_plan$network$nodes`) it names. The resampling
#' settings are added afterwards by [define_network_bagging()]; both verbs
#' fill the same `settings` list, which the later verbs read.
#'
#' @param data The prepared network input (`data_network`): the nine canonical
#'   z columns of the configured nodes, complete, with its `z_parameters`.
#' @param analysis_plan Configuration from [zm_config()]; the model half of
#'   `analysis_plan$network` travels with the model as its per-fit settings.
#' @return List `data` (the nine node columns in the configured order), `nodes`
#'   and `settings` (model half).
define_network_model <- function(data, analysis_plan) {
  nodes <- analysis_plan$network$nodes
  require_valid_imputation_inputs(data, nodes)
  list(
    data = dplyr::select(data, dplyr::all_of(nodes)),
    nodes = nodes,
    settings = list(
      package = analysis_plan$network$package,              # BDgraph
      iter = analysis_plan$network$iter,                    # 50000
      g_prior = analysis_plan$network$g_prior,              # 0.5
      df_prior = analysis_plan$network$df_prior,            # 3
      not_cont = analysis_plan$network$not_cont,            # 0: all nodes continuous
      bf_include = analysis_plan$network$bf_include,        # 10
      bf_exclude = analysis_plan$network$bf_exclude,        # 0.1
      g_prior_sweep = analysis_plan$network$g_prior_sweep   # 0.25, 0.5, 0.75
    )
  )
}

# BEGIN GENERATED PARAMETER CARD: AP8 NETWORK BAGGING
# Automatically generated from analysis_plan.yaml
#   Bootstrap samples: 600   (network.B)
#   Seed of bootstrap b: 39976 + b   (network.seed_base)
#   Retry seed offset: 100000   (network.retry_seed_offset; one same-data retry per failed fit)
#   Minimum success rate: 0.95   (network.min_success_rate; below it the bag is computationally infeasible)
# END GENERATED PARAMETER CARD: AP8 NETWORK BAGGING
#' Declare the participant bagging of that network
#'
#' AP8 / RQ3 bagging: `B` participant bootstrap samples, seeded from
#' `seed_base`, one same-data retry at `retry_seed_offset`, and the
#' `min_success_rate` feasibility trigger. These settings say how often the
#' model of [define_network_model()] is refitted, never what it is.
#'
#' @param network_settings List from [define_network_model()].
#' @param analysis_plan Configuration from [zm_config()].
#' @return The same list with the bagging settings added to `settings`.
define_network_bagging <- function(network_settings, analysis_plan) {
  network_settings$settings <- c(
    network_settings$settings,
    list(
      B = analysis_plan$network$B,                                  # 600 bootstrap samples
      seed_base = analysis_plan$network$seed_base,                  # 39976; seed_b = seed_base + b
      retry_seed_offset = analysis_plan$network$retry_seed_offset,  # 100000
      min_success_rate = analysis_plan$network$min_success_rate     # 0.95
    )
  )
  network_settings
}

#' Draw every participant bootstrap once and retain its rows
#'
#' AP8 / RQ3 resampling: draw `analysis_plan$network$B` bootstrap samples of
#' participants, each of size N with replacement; bootstrap b uses seed
#' `analysis_plan$network$seed_base + b`. A later model retry receives this same stored
#' data, so the resample is never redrawn.
#'
#' @param network_settings List from [define_network_model()].
#' @return List of `B` samples, each `id`, `seed` and `data`.
resample_network_participants <- function(network_settings) {
  data <- network_settings$data
  settings <- network_settings$settings
  B <- settings$B  # 600 bootstrap samples
  N <- nrow(data)
  lapply(seq_len(B), function(b) {
    seed <- settings$seed_base + b  # 39976 + b
    rows <- ap7_with_network_seed(
      seed,
      sample.int(N, size = N, replace = TRUE)
    )
    list(id = b, seed = seed, data = data[rows, , drop = FALSE])
  })
}

#' Fit the graph to each stored sample, with one same-data retry
#'
#' AP8 / RQ3 per-fit specification and failure handling: fit easybgm
#' with BDgraph to every stored bootstrap sample. A fit or extraction error is
#' retried once on the same sample at seed `seed_base + b + retry_seed_offset`.
#' The retry confines the RNG state, checks resample degeneracy, reuses the
#' identical data and treats extraction errors as fit failures.
#'
#' @param network_bootstrap_samples List from
#'   [resample_network_participants()].
#' @param network_settings List from [define_network_model()].
#' @return List of fit records: `id`, `succeeded`, `edges` (or `NULL`), `seed`
#'   of the attempt that produced them, and the `attempts` log.
fit_network_bootstraps <- function(network_bootstrap_samples, network_settings) {
  settings <- network_settings$settings
  network_bootstrap_samples |>
    lapply(function(sample) {
    result <- ap7_capture_network_attempts(
      data = sample$data,
      seed = sample$seed,
      retry_seed_offset = settings$retry_seed_offset,  # 100000
      estimate = function(sample_data) {
        raw_fit <- easybgm::easybgm(
          data = sample_data,
          type = "mixed",                        # Gaussian copula
          package = settings$package,            # BDgraph
          iter = settings$iter,                  # 50000
          g.prior = settings$g_prior,            # 0.5
          df.prior = settings$df_prior,          # 3
          not_cont = rep(settings$not_cont, ncol(sample_data)),  # all zeros
          save = FALSE,
          centrality = FALSE,
          progress = FALSE,
          verbose = FALSE
        )
        ap7_extract_network_edges(raw_fit, network_settings$nodes)
      }
    )
    c(list(id = sample$id), result)
  })
}

#' Apply the feasibility rule to all attempted fits
#'
#' AP8 / RQ3 failure handling: bagged results are reported only when
#' at least `analysis_plan$network$min_success_rate` of the `analysis_plan$network$B` bootstrap
#' fits succeed. Otherwise the bagged network is computationally infeasible,
#' without affecting any other analysis. Ordinary and retry successes both
#' count; failed fits change neither data nor model.
#'
#' @param fits List of fit records from [fit_network_bootstraps()].
#' @param network_settings List from [define_network_model()].
#' @return List `attempted_B`, `n_success`, `success_rate`, `min_success_rate`,
#'   `feasible` and `status`.
assess_network_bootstrap_success <- function(fits, network_settings) {
  settings <- network_settings$settings
  n_success <- sum(vapply(fits, function(fit) isTRUE(fit$succeeded), logical(1)))
  attempted_B <- settings$B  # 600
  success_rate <- n_success / attempted_B
  feasible <- success_rate >= settings$min_success_rate  # 0.95
  list(
    attempted_B = attempted_B,
    n_success = n_success,
    success_rate = success_rate,
    min_success_rate = settings$min_success_rate,
    feasible = feasible,
    status = if (feasible) "feasible" else "computationally infeasible"
  )
}

# --- the bagged posterior and the registered decision rule -------------------

#' Average PIPs and weights over the successful fits, before any BF conversion
#'
#' AP8 / RQ3 bagged posterior: for every edge, average the
#' full-precision posterior inclusion probabilities and the model-averaged edge
#' weights across the successful fits. The averaged PIP is converted to a Bayes
#' factor only afterwards.
#'
#' The one availability branch of the route sits here, where the bag is
#' produced: an infeasible bag yields the typed empty edge table, so every
#' downstream verb receives an edge table and needs no guard. Failed attempts
#' enter neither mean.
#'
#' @param network_bootstrap_fits List `fits` and `assessment`.
#' @param network_settings List from [define_network_model()].
#' @return Tibble `node_i`, `node_j`, `pip`, `weight`, `n_success`, `g_prior`;
#'   empty (see [ap7_no_bagged_network_edges()]) when the bag is infeasible.
extract_network_bagged_estimates <- function(network_bootstrap_fits, network_settings) {
  # Infeasible bag (fewer than 95% successful fits): no bagged estimates exist.
  # The assessment record travels beside the empty edge table in bagged_network.
  if (!network_bootstrap_fits$assessment$feasible) return(ap7_no_bagged_network_edges())
  fits <- network_bootstrap_fits$fits
  successful <- ap7_bind_successful_network_edges(fits)
  g_prior <- network_settings$settings$g_prior  # 0.5
  successful |>
    dplyr::group_by(node_i, node_j) |>
    dplyr::summarise(
      pip = mean(pip),
      weight = mean(weight),
      n_success = dplyr::n_distinct(bootstrap_id),
      .groups = "drop"
    ) |>
    dplyr::mutate(g_prior = g_prior)
}

#' Convert each full-precision PIP to its edge-inclusion Bayes factor
#'
#' AP8 / RQ3 Bayes-factor definition: the edge Bayes factor is the
#' posterior inclusion odds divided by the edge-prior odds, each row using the
#' prior that generated its fit. A PIP of zero maps to a Bayes factor of zero,
#' a PIP of one to infinity, and an unavailable PIP stays unavailable; nothing
#' is clamped or rounded for the decision.
#'
#' @param edges Edge table carrying `pip` and `g_prior`.
#' @return The same table with `bf`.
extract_network_bayes_factors <- function(edges) {
  edges |>
    dplyr::mutate(
      bf = (pip / (1 - pip)) / (g_prior / (1 - g_prior))
    )
}

#' Apply the inclusive Bayes-factor thresholds
#'
#' AP8 / RQ3 decision rule: an edge is present at BF at least
#' `analysis_plan$network$bf_include`, absent at BF at most `analysis_plan$network$bf_exclude`, and
#' inconclusive otherwise. Edge weights are interpreted only for present edges,
#' so the raw model-averaged weight remains beside the masked interpretive one.
#' A missing fit stays missing rather than becoming absent or inconclusive.
#'
#' @param edges Edge table carrying `bf` and `weight`.
#' @param network_settings List from [define_network_model()].
#' @return The same table with `decision` and `weight_interpretable`.
extract_network_edge_decisions <- function(edges, network_settings) {
  settings <- network_settings$settings
  edges |>
    dplyr::mutate(
      decision = dplyr::case_when(
        is.na(bf) ~ NA_character_,
        bf >= settings$bf_include ~ "present",       # BF >= 10
        bf <= settings$bf_exclude ~ "absent",        # BF <= 0.1
        TRUE ~ "inconclusive"
      ),
      weight_interpretable = dplyr::if_else(
        decision == "present", weight, NA_real_
      )
    )
}

#' The node sets the RQ3 questions are cut on
#'
#' RQ3a and RQ3b name the autonomy motives (power, prestige, achievement), the
#' four orientation facets and the security and arousal motives. The route
#' selects the subsets with these sets, and the report names them from here.
#' Each is a set: whoever lists its members puts them in the codebook's order
#' ([zm_order_scale_keys()]).
#'
#' @return List `autonomy`, `orientation`, `security_arousal` of canonical z
#'   columns.
define_network_question_node_sets <- function() {
  list(
    autonomy = c("zm_power_z", "zm_prestige_z", "zm_achievement_z"),
    orientation = c("asc_agg_z", "asc_sub_z", "asc_conv_z", "sdo_dom_z"),
    security_arousal = c("zm_security_z", "zm_arousal_z")
  )
}

#' The twelve autonomy-to-orientation edges (the autonomy rows of RQ3a)
#'
#' RQ3a asks which associations bridge the social-motive and
#' authoritarian-orientation sets; these are the bridges of achievement, power
#' and prestige with the four orientation facets while the remaining seven
#' network variables are held constant. This is a subset of the one fitted
#' graph, not a refit; every edge status is retained, and an infeasible bag
#' yields an empty selection whose reason stands in `network_bagged$assessment`.
#'
#' @param edges Bagged edge table with decisions.
#' @return The rows of the twelve edges.
extract_network_autonomy_orientation_edges <- function(edges) {
  nodes <- define_network_question_node_sets()
  autonomy <- nodes$autonomy
  orientation <- nodes$orientation
  edges |>
    dplyr::filter(
      (node_i %in% autonomy & node_j %in% orientation) |
        (node_j %in% autonomy & node_i %in% orientation)
    )
}

#' The six autonomy-to-security-or-arousal edges (RQ3b)
#'
#' RQ3b asks whether power, prestige and achievement are positively associated
#' with the arousal motive and negatively with the security motive when the
#' remaining network variables are held constant. The subset conditions on the
#' other seven nodes and makes no mediation, path-screening or causal claim.
#'
#' @param edges Bagged edge table with decisions.
#' @return The rows of the six edges.
extract_network_autonomy_security_arousal_edges <- function(edges) {
  nodes <- define_network_question_node_sets()
  autonomy <- nodes$autonomy
  security_arousal <- nodes$security_arousal
  edges |>
    dplyr::filter(
      (node_i %in% autonomy & node_j %in% security_arousal) |
        (node_j %in% autonomy & node_i %in% security_arousal)
    )
}

# --- the descriptive full-sample comparisons ---------------------------------

#' Fit the descriptive full-sample comparator
#'
#' AP8 / RQ3 full-sample comparison: one descriptive full-sample
#' network under the same per-fit settings, at seed `analysis_plan$network$seed_base`
#' with one same-data retry. Failure leaves all 36 edge quantities unavailable;
#' a failed comparator is never read as an absent network.
#'
#' @param network_settings List from [define_network_model()].
#' @return Edge tibble (see [ap7_network_fit_result_edges()]).
fit_full_sample_network <- function(network_settings) {
  settings <- network_settings$settings
  result <- ap7_capture_network_attempts(
    data = network_settings$data,
    seed = settings$seed_base,  # 39976
    retry_seed_offset = settings$retry_seed_offset,  # 100000
    estimate = function(sample_data) {
      raw_fit <- easybgm::easybgm(
        data = sample_data,
        type = "mixed",
        package = settings$package,
        iter = settings$iter,
        g.prior = settings$g_prior,
        df.prior = settings$df_prior,
        not_cont = rep(settings$not_cont, ncol(sample_data)),
        save = FALSE,
        centrality = FALSE,
        progress = FALSE,
        verbose = FALSE
      )
      ap7_extract_network_edges(raw_fit, network_settings$nodes)
    }
  )
  ap7_network_fit_result_edges(result, network_settings$nodes, settings$g_prior)
}

#' Fit the two new full-sample prior comparisons and reuse the registered one
#'
#' AP8 / RQ3 edge-prior sensitivity: refit the full-sample network at
#' the edge priors of `analysis_plan$network$g_prior_sweep`, reusing the already fitted
#' full-sample result at the preregistered `analysis_plan$network$g_prior`. The other
#' settings, data, seeds and the retry rule stay fixed. The sweep is a
#' descriptive full-sample analysis and cannot overwrite the bagged
#' classification at the preregistered prior.
#'
#' @param network_settings List from [define_network_model()].
#' @param network_full_sample The fitted result of [fit_full_sample_network()].
#' @return Row-bound edge tibble over the swept priors (see
#'   [ap7_bind_network_prior_results()]).
fit_network_prior_comparisons <- function(network_settings, network_full_sample) {
  settings <- network_settings$settings
  priors <- settings$g_prior_sweep  # 0.25, 0.5, 0.75
  results <- priors |>
    lapply(function(prior) {
    if (prior == settings$g_prior) return(network_full_sample)
    result <- ap7_capture_network_attempts(
      data = network_settings$data,
      seed = settings$seed_base,
      retry_seed_offset = settings$retry_seed_offset,
      estimate = function(sample_data) {
        raw_fit <- easybgm::easybgm(
          data = sample_data,
          type = "mixed",
          package = settings$package,
          iter = settings$iter,
          g.prior = prior,
          df.prior = settings$df_prior,
          not_cont = rep(settings$not_cont, ncol(sample_data)),
          save = FALSE,
          centrality = FALSE,
          progress = FALSE,
          verbose = FALSE
        )
        ap7_extract_network_edges(raw_fit, network_settings$nodes)
      }
    )
    ap7_network_fit_result_edges(result, network_settings$nodes, prior)
  })
  ap7_bind_network_prior_results(results, priors)
}

#' Assess every edge across all three full-sample priors
#'
#' AP8 / RQ3 edge-prior sensitivity: a full-sample edge classification
#' is stable only when all three fitted priors are available and yield the same
#' classification. A missing fit leaves stability unavailable; differing
#' classifications are named. The complete estimate table is preserved and the
#' bagged results are untouched.
#'
#' @param comparisons Edge table over the swept priors, with decisions.
#' @return List `estimates`, `stability`, `changed_edges`, `attempts_by_prior`.
extract_network_prior_sensitivity <- function(comparisons) {
  stability <- comparisons |>
    dplyr::group_by(node_i, node_j) |>
    dplyr::summarise(
      stable = dplyr::case_when(
        any(is.na(decision)) ~ NA,
        dplyr::n_distinct(decision) == 1L ~ TRUE,
        TRUE ~ FALSE
      ),
      .groups = "drop"
    )
  changed_edges <- dplyr::filter(stability, stable %in% FALSE)
  list(
    estimates = comparisons,
    stability = stability,
    changed_edges = changed_edges,
    attempts_by_prior = attr(comparisons, "attempts_by_prior")
  )
}

#' Compare the same edges between the bagged and the full-sample network
#'
#' AP8 / RQ3 full-sample comparison: compare all edge PIPs, Bayes
#' factors, weights and decisions between the bagged and the full-sample
#' network. The bagged decisions remain primary and the comparison is
#' descriptive. An unavailable decision on either side stays unavailable in
#' `changed` and is never treated as absent; an infeasible bag leaves every
#' bagged column unavailable and carries its assessment.
#'
#' @param network_bagged List `edges` and `assessment`.
#' @param network_full_sample Full-sample edge table with decisions.
#' @return List `edges` (the wide comparison with `changed`) and `assessment`.
extract_network_bagged_full_comparison <- function(network_bagged, network_full_sample) {
  comparison <- ap7_pair_bagged_full_edge_results(
    network_bagged$edges, network_full_sample
  ) |>
    dplyr::mutate(
      changed = dplyr::case_when(
        is.na(decision_bagged) | is.na(decision_full) ~ NA,
        TRUE ~ decision_bagged != decision_full
      )
    )
  list(edges = comparison, assessment = network_bagged$assessment)
}

#' Describe the successful-resample PIPs and the shares meeting each threshold
#'
#' AP8 / RQ3 resampling comparison: for every edge, retain the
#' successful-resample PIP distribution and report the shares meeting the
#' present and the absent threshold. Failed resamples leave the denominator,
#' whose successful count is reported. No percentile compression replaces the
#' per-resample PIPs, and this descriptive comparison cannot change the bagged
#' edge classifications.
#'
#' @param comparison List from [extract_network_bagged_full_comparison()].
#' @param network_bootstrap_fits List `fits` and `assessment`.
#' @param network_settings List from [define_network_model()].
#' @return `comparison` with `stability` (see [ap7_store_resampling_stability()]).
extract_network_resampling_stability <- function(
    comparison, network_bootstrap_fits, network_settings) {
  settings <- network_settings$settings
  distributions <- ap7_bind_successful_network_edges(network_bootstrap_fits$fits) |>
    dplyr::mutate(
      bf = (pip / (1 - pip)) / (settings$g_prior / (1 - settings$g_prior))
    )
  stability <- distributions |>
    dplyr::group_by(node_i, node_j) |>
    dplyr::summarise(
      share_above_include = mean(bf >= settings$bf_include),
      share_below_exclude = mean(bf <= settings$bf_exclude),
      n_success = dplyr::n_distinct(bootstrap_id),
      .groups = "drop"
    )
  ap7_store_resampling_stability(comparison, distributions, stability, settings)
}

# --- technical helpers -------------------------------------------------------

#' Evaluate one expression under a seed and restore the caller's RNG state
#'
#' Serves the participant resampling and the fit-attempt capture. It chooses no
#' seed and no model setting.
#'
#' @param seed Integer seed.
#' @param expression Expression to evaluate under it.
#' @return The value of `expression`.
ap7_with_network_seed <- function(seed, expression) {
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

#' Capture the attempts of one network fit, with one same-data retry
#'
#' Serves the bootstrap, full-sample and prior-sensitivity fits. It catches fit
#' and extraction errors alike, retries once at seed `seed + retry_seed_offset`
#' on the identical supplied data, and returns the attempt log. A zero-variance
#' column created by resampling is routed through the same failure rule.
#'
#' @param data The rows to fit.
#' @param seed Seed of the first attempt.
#' @param retry_seed_offset Offset added for the single retry.
#' @param estimate Function of the data returning the edge table.
#' @return List `succeeded`, `edges`, `seed`, `attempts`.
ap7_capture_network_attempts <- function(data, seed, retry_seed_offset, estimate) {
  attempt_seeds <- as.integer(c(seed, seed + retry_seed_offset))
  attempts <- tibble::tibble(
    attempt = integer(), seed = integer(), status = character(), message = character()
  )
  for (attempt in seq_along(attempt_seeds)) {
    attempt_seed <- attempt_seeds[[attempt]]
    result <- tryCatch(
      ap7_with_network_seed(attempt_seed, {
        constant <- names(data)[vapply(
          data, function(value) isTRUE(stats::sd(value) == 0), logical(1)
        )]
        if (length(constant)) {
          stop("Resample has zero-variance node columns: ", paste(constant, collapse = ", "))
        }
        estimate(data)
      }),
      error = identity
    )
    if (!inherits(result, "error")) {
      attempts <- tibble::add_row(
        attempts, attempt = attempt, seed = attempt_seed,
        status = "ok", message = NA_character_
      )
      return(list(
        succeeded = TRUE, edges = result, seed = attempt_seed, attempts = attempts
      ))
    }
    attempts <- tibble::add_row(
      attempts, attempt = attempt, seed = attempt_seed,
      status = "error", message = conditionMessage(result)
    )
  }
  list(
    succeeded = FALSE, edges = NULL,
    seed = attempt_seeds[[2]], attempts = attempts
  )
}

#' Read one easybgm fit at full precision
#'
#' Serves every easybgm fit callback. It reconstructs the BDgraph posterior
#' when easybgm has dropped it, calls `BDgraph::plinks(round = 10)`, reads the
#' unrounded model-averaged weights from `fit$parameters`, and extracts all
#' upper-triangle pairs in the node order it is given. It never uses the
#' rounded `fit$inc_probs` or a summary string.
#'
#' @param fit An easybgm fit.
#' @param nodes Character vector of node columns in their configured order.
#' @return Tibble `node_i`, `node_j`, `weight`, `pip`, `n_sweeps`.
ap7_extract_network_edges <- function(fit, nodes) {
  object <- fit$packagefit
  if (is.null(object)) {
    required <- c("sample_graphs", "graph_weights", "structure")
    missing <- required[vapply(required, function(name) is.null(fit[[name]]), logical(1))]
    if (length(missing)) {
      stop("Cannot reconstruct the unrounded BDgraph posterior: ",
           paste(missing, collapse = ", "))
    }
    object <- list(
      sample_graphs = fit$sample_graphs,
      graph_weights = fit$graph_weights,
      last_graph = as.matrix(fit$structure)
    )
    class(object) <- "bdgraph"
  }
  inclusion <- as.matrix(BDgraph::plinks(object, round = 10))
  inclusion <- inclusion + t(inclusion)
  weights <- as.matrix(fit$parameters)
  for (matrix in list(inclusion = inclusion, weights = weights)) {
    if (!identical(dim(matrix), c(length(nodes), length(nodes))) ||
        !all(nodes %in% rownames(matrix)) || !all(nodes %in% colnames(matrix))) {
      stop("Network posterior matrices must contain all nine nodes by name.")
    }
  }
  inclusion <- inclusion[nodes, nodes, drop = FALSE]
  weights <- weights[nodes, nodes, drop = FALSE]
  pair_index <- which(upper.tri(inclusion), arr.ind = TRUE)
  pip <- inclusion[pair_index]
  weight <- weights[pair_index]
  if (any(!is.finite(pip)) || any(pip < 0 | pip > 1)) {
    stop("Network posterior inclusion probabilities must be finite values in [0, 1].")
  }
  if (any(!is.finite(weight))) {
    stop("Network posterior edge weights must be finite.")
  }
  total <- sum(as.numeric(object$graph_weights))
  n_sweeps <- if (
    is.finite(total) && total >= 1 &&
      isTRUE(all.equal(total, round(total), tolerance = 1e-9))
  ) as.integer(round(total)) else NA_integer_
  tibble::tibble(
    node_i = nodes[pair_index[, "row"]],
    node_j = nodes[pair_index[, "col"]],
    weight = as.numeric(weight),
    pip = as.numeric(pip),
    n_sweeps = n_sweeps
  )
}

#' The typed empty edge table an infeasible bag produces
#'
#' Serves the bagged estimates. Bayes factors, decisions, question selections
#' and the full-sample comparison run on it without an availability guard;
#' `network_bagged$assessment` records why no edge exists.
#'
#' @return Zero-row tibble `node_i`, `node_j`, `pip`, `weight`, `n_success`,
#'   `g_prior`.
ap7_no_bagged_network_edges <- function() {
  tibble::tibble(
    node_i = character(), node_j = character(),
    pip = numeric(), weight = numeric(),
    n_success = integer(), g_prior = numeric()
  )
}

#' The typed empty record table of the successful network fits
#'
#' Empty results keep their columns: binding successful records alone would
#' leave a run without a single success as a zero-column tibble, on which the
#' resampling summary fails for want of `pip`. This is the explicit empty
#' schema [ap7_bind_successful_network_edges()] binds against, so a run with
#' no successful fit yields the same columns as one with many.
#'
#' @return Zero-row tibble with the columns of one successful fit record:
#'   `node_i`, `node_j`, `weight`, `pip`, `n_sweeps`, `bootstrap_id`,
#'   `fit_seed`.
ap7_no_successful_network_edges <- function() {
  tibble::tibble(
    node_i = character(), node_j = character(),
    weight = numeric(), pip = numeric(), n_sweeps = integer(),
    bootstrap_id = integer(), fit_seed = integer()
  )
}

#' Row-bind the successful fit records with their identity
#'
#' Serves the posterior averaging and the resampling stability. It binds only
#' successful ordinary or retry records — against the explicit empty schema of
#' [ap7_no_successful_network_edges()], so zero successes keep the columns —
#' and adds the bootstrap identifier and the successful fit seed. It computes
#' no posterior summary and no decision. Failed attempts stay in their own fit
#' records; their absence here is never evidence that an edge is absent.
#'
#' @param fits List of fit records from [fit_network_bootstraps()].
#' @return Tibble of the successful per-resample edge rows.
ap7_bind_successful_network_edges <- function(fits) {
  rows <- lapply(fits, function(fit) {
    if (!isTRUE(fit$succeeded)) return(NULL)
    dplyr::mutate(
      fit$edges,
      bootstrap_id = fit$id,
      fit_seed = fit$seed
    )
  })
  dplyr::bind_rows(c(list(ap7_no_successful_network_edges()), rows))
}

#' Pack one full-sample fit result into its edge table
#'
#' Serves the full-sample and the prior-sensitivity fits. It preserves the
#' successful edges, or produces all ordered pairs with unavailable posterior
#' values after both attempts failed, and attaches the fit seed and the attempt
#' record. Missing computation is never recoded as absence.
#'
#' @param result List from [ap7_capture_network_attempts()].
#' @param nodes Character vector of node columns.
#' @param g_prior The edge prior this fit used.
#' @return Edge tibble with attributes `fit_succeeded`, `fit_seed`, `attempts`.
ap7_network_fit_result_edges <- function(result, nodes, g_prior) {
  if (result$succeeded) {
    edges <- result$edges
  } else {
    pairs <- utils::combn(nodes, 2L)
    edges <- tibble::tibble(
      node_i = pairs[1, ],
      node_j = pairs[2, ],
      weight = NA_real_,
      pip = NA_real_,
      n_sweeps = NA_integer_
    )
  }
  edges$g_prior <- g_prior
  attr(edges, "fit_succeeded") <- result$succeeded
  attr(edges, "fit_seed") <- result$seed
  attr(edges, "attempts") <- result$attempts
  edges
}

#' Row-bind the three prior-sweep edge tables with their fit metadata
#'
#' Serves the edge-prior sensitivity fitting. It retains each prior's success,
#' seed and attempt record and selects no prior, computes no Bayes factor,
#' classification or stability.
#'
#' @param results List of edge tables from [ap7_network_fit_result_edges()].
#' @param priors The swept priors, in their configured order.
#' @return The bound table; attributes `attempts_by_prior`,
#'   `succeeded_by_prior`, `seed_by_prior`.
ap7_bind_network_prior_results <- function(results, priors) {
  comparisons <- dplyr::bind_rows(results)
  attr(comparisons, "attempts_by_prior") <- stats::setNames(
    lapply(results, attr, which = "attempts"), priors
  )
  attr(comparisons, "succeeded_by_prior") <- stats::setNames(
    vapply(results, function(result) isTRUE(attr(result, "fit_succeeded")), logical(1)),
    priors
  )
  attr(comparisons, "seed_by_prior") <- stats::setNames(
    vapply(results, function(result) {
      seed <- attr(result, "fit_seed")
      if (is.null(seed)) NA_integer_ else as.integer(seed)
    }, integer(1)),
    priors
  )
  comparisons
}

#' Pair the bagged and the full-sample estimate of every edge
#'
#' Full-joins the same undirected node pairs and keeps PIP, Bayes factor, raw
#' weight, interpretable weight and decision under `_bagged` / `_full` labels,
#' together with the successful-resample count. A value missing on one side
#' stays unavailable. The result is the wide edge table the changed-decision
#' rule of [extract_network_bagged_full_comparison()] reads. Both sides come
#' from the same node order, so the join key is the pair as both tables
#' already write it. Nothing is recomputed: an infeasible bag contributes no
#' row and its columns stay unavailable.
#'
#' @param bagged_edges Bagged edge table with decisions (possibly empty).
#' @param full_sample_edges Full-sample edge table with decisions.
#' @return Tibble `node_i`, `node_j`, the `_bagged` and `_full` columns, and
#'   `n_success`.
ap7_pair_bagged_full_edge_results <- function(bagged_edges, full_sample_edges) {
  bagged <- tibble::tibble(
    node_i = as.character(bagged_edges$node_i),
    node_j = as.character(bagged_edges$node_j),
    pip_bagged = as.numeric(bagged_edges$pip),
    bf_bagged = as.numeric(bagged_edges$bf),
    weight_bagged = as.numeric(bagged_edges$weight),
    weight_interpretable_bagged = as.numeric(bagged_edges$weight_interpretable),
    decision_bagged = as.character(bagged_edges$decision),
    n_success = as.integer(bagged_edges$n_success)
  )
  full <- tibble::tibble(
    node_i = as.character(full_sample_edges$node_i),
    node_j = as.character(full_sample_edges$node_j),
    pip_full = as.numeric(full_sample_edges$pip),
    bf_full = as.numeric(full_sample_edges$bf),
    weight_full = as.numeric(full_sample_edges$weight),
    weight_interpretable_full = as.numeric(full_sample_edges$weight_interpretable),
    decision_full = as.character(full_sample_edges$decision)
  )
  dplyr::full_join(bagged, full, by = c("node_i", "node_j"))
}

#' Store the resampling stability beside the comparison it describes
#'
#' Attaches the stability to the comparison, retaining the full
#' successful-resample distributions, the supplied edge summaries, the
#' successful sample count and the configured Bayes-factor and prior
#' thresholds, and records the equivalent PIP thresholds as
#' `BF * prior odds / (1 + BF * prior odds)`. It adds the per-edge 5th, 50th
#' and 95th percentiles of the retained successful-resample PIPs, which
#' `stats::quantile()` computes at its default type 7, taken from the retained
#' distribution and from nothing else. No classification is made and no
#' resampling is performed.
#'
#' @param comparison List from [extract_network_bagged_full_comparison()].
#' @param distributions The successful-resample rows, with their Bayes factors.
#' @param stability The per-edge shares meeting each threshold and the
#'   successful counts, from [extract_network_resampling_stability()].
#' @param settings `analysis_plan$network`.
#' @return `comparison` with `stability`: `edges` (the supplied summaries and
#'   the three quantiles), `distributions`, `n_success`, `thresholds`.
ap7_store_resampling_stability <- function(comparison, distributions, stability, settings) {
  prior_odds <- settings$g_prior / (1 - settings$g_prior)
  pip_threshold <- function(bf) bf * prior_odds / (1 + bf * prior_odds)
  quantiles <- distributions |>
    dplyr::group_by(node_i, node_j) |>
    dplyr::summarise(
      pip_resample_q05 = stats::quantile(pip, 0.05, names = FALSE),
      pip_resample_q50 = stats::quantile(pip, 0.50, names = FALSE),
      pip_resample_q95 = stats::quantile(pip, 0.95, names = FALSE),
      .groups = "drop"
    )
  comparison$stability <- list(
    edges = dplyr::left_join(stability, quantiles, by = c("node_i", "node_j")),
    distributions = distributions,
    n_success = dplyr::n_distinct(distributions$bootstrap_id),
    thresholds = tibble::tibble(
      bf_include = settings$bf_include,
      bf_exclude = settings$bf_exclude,
      pip_include = pip_threshold(settings$bf_include),
      pip_exclude = pip_threshold(settings$bf_exclude),
      g_prior = settings$g_prior
    )
  )
  comparison
}
