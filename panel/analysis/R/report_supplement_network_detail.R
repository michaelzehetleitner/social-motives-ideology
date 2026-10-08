# Report target of the supplement section S5 "The network beyond the main
# result".
#
# Builds the one target (_targets.R, SUPPLEMENT · S5) the section and its
# tables and figures read. The report words and formats what this returns.

#' S5 network detail
#'
#' The bagged network against the full-data fit, the stability of the edge
#' decisions over the resamples, and the edge-prior sweep; the counts the
#' section states; and the facts of the answer the network section prints.
#'
#' @param network_resampling_comparison,network_prior_sensitivity,network_bagged,network_full_sample,network_settings
#'   The accepted network results (targets of the same names).
#' @param network_fits,network_matrices The per-resample fits and the bagged
#'   edge matrices (target `report_network`).
#' @param codebook Codebook from [zm_codebook()].
#' @param analysis_plan Configuration from [zm_config()].
#' @return List `comparison`, `prior_sweep`, `fits`, `matrices`, `matrices_full`,
#'   `n_opposite` (edges called present by one fit and absent by the other) and
#'   `answer`.
assemble_supplement_network_detail <- function(network_resampling_comparison, network_prior_sensitivity,
                                               network_bagged, network_full_sample, network_settings,
                                               network_fits, network_matrices, codebook, analysis_plan) {
  comparison <- tabulate_network_comparison(network_resampling_comparison, codebook, analysis_plan)
  full <- tabulate_full_sample_network(network_full_sample, network_settings, codebook, analysis_plan)
  transitions <- comparison$transitions
  transition <- function(from, to) {
    if (!(from %in% rownames(transitions)) || !(to %in% colnames(transitions))) {
      stop("The edge-decision comparison has no transition from '", from, "' to '", to, "'.")
    }
    as.integer(transitions[from, to])
  }
  list(
    comparison = comparison,
    prior_sweep = tabulate_network_prior_sweep(network_prior_sensitivity, network_bagged,
                                                network_settings, codebook, analysis_plan),
    fits = network_fits,
    matrices = network_matrices,
    matrices_full = ap6_network_full_matrix(full, analysis_plan$network$node_keys),
    n_opposite = transition("present", "absent") + transition("absent", "present"),
    answer = check_network_detail(comparison)
  )
}

#' Whether the bagged and full-data decisions agree, and how stable they are
#'
#' @param comparison The edge-decision comparison of [tabulate_network_comparison()].
#' @return List `comparison_available`, `n_edges`, `n_changed`,
#'   `all_disagreements_inconclusive`, `n_conclusive`, `unstable_edges` (the
#'   conclusive edges whose resample stability is unavailable: `node_i`,
#'   `node_j`) and `n_majority` (conclusive decisions that crossed their
#'   matching threshold in most resamples).
check_network_detail <- function(comparison) {
  transitions <- comparison$transitions
  total <- as.integer(comparison$n_edges)
  changed <- as.integer(comparison$n_changed)
  comparison_available <- length(total) == 1L && length(changed) == 1L &&
    is.finite(total) && is.finite(changed) && !is.null(transitions) &&
    all(c("inconclusive") %in% rownames(transitions)) &&
    all(c("inconclusive") %in% colnames(transitions))
  edges <- tibble::as_tibble(comparison$edges)
  conclusive <- edges$decision_bagged %in% c("present", "absent")
  matching_share <- ifelse(
    edges$decision_bagged %in% "present", edges$share_above_include,
    edges$share_below_exclude
  )
  stability_available <- is.finite(matching_share)
  list(
    comparison_available = comparison_available,
    n_edges = total,
    n_changed = changed,
    n_agreed = total - changed,
    all_disagreements_inconclusive = comparison_available && changed ==
      sum(transitions[, "inconclusive"], na.rm = TRUE) - transitions["inconclusive", "inconclusive"],
    n_conclusive = sum(conclusive),
    unstable_edges = tibble::tibble(
      node_i = edges$node_i[conclusive & !stability_available],
      node_j = edges$node_j[conclusive & !stability_available]
    ),
    n_majority = sum(matching_share[conclusive] > 1 / 2, na.rm = TRUE)
  )
}

# ---- the tables of the section, built from the accepted results ----------
#
# The accepted RQ3 results carry the numbers; these tables rename, retype,
# reorder and join them, and write only values of the result or configured
# constants of `analysis_plan$network`. The accepted nodes are the canonical
# standardised columns (`zm_security_z`); the tables name the scale keys
# (`zm_security`), mapped through the codebook with `zm_key_of_z_col()`. An
# infeasible bag arrives as the typed empty edge table and stays empty.

#' The full-sample edge table with its fit attributes
#'
#' The descriptive full-sample fit under the column names the report helpers
#' read. `feasible` states whether the fit itself succeeded: after two failed
#' attempts the accepted route keeps all edge quantities unavailable instead
#' of stopping, and this attribute says so.
#'
#' @param network_full_sample Full-sample edge table with decisions.
#' @param network_settings List from [define_network_model()].
#' @param codebook Codebook from [zm_codebook()].
#' @param analysis_plan Configuration from [zm_config()].
#' @return Tibble `node_i`, `node_j`, `weight`, `pip`, `bf`, `decision`;
#'   attributes `seed`, `n`, `nodes`, `iter`, `n_sweeps`, `g_prior`,
#'   `bf_include`, `bf_exclude`, `feasible`, `attempts`.
tabulate_full_sample_network <- function(network_full_sample, network_settings, codebook, analysis_plan) {
  net <- analysis_plan$network
  nodes <- as.character(net$node_keys)
  out <- tibble::tibble(
    node_i = find_network_scale_keys(network_full_sample$node_i, codebook),
    node_j = find_network_scale_keys(network_full_sample$node_j, codebook),
    weight = as.numeric(network_full_sample$weight),
    pip = as.numeric(network_full_sample$pip),
    bf = as.numeric(network_full_sample$bf),
    decision = as.character(network_full_sample$decision)
  )
  out <- ap6_network_canonical(out, nodes)
  attr(out, "seed") <- as.integer(attr(network_full_sample, "fit_seed"))
  attr(out, "n") <- nrow(network_settings$data)
  attr(out, "nodes") <- nodes
  attr(out, "iter") <- as.integer(net$iter)
  attr(out, "n_sweeps") <- ap6_network_n_sweeps(network_full_sample)
  attr(out, "g_prior") <- as.numeric(net$g_prior)
  attr(out, "bf_include") <- net$bf_include
  attr(out, "bf_exclude") <- net$bf_exclude
  attr(out, "feasible") <- isTRUE(attr(network_full_sample, "fit_succeeded"))
  attr(out, "attempts") <- attr(network_full_sample, "attempts")
  out
}

#' The bagged-against-full comparison of the network
#'
#' The wide comparison of the accepted route, joined with the per-edge shares
#' and the successful-resample PIP quantiles its stability store retains, under
#' the column names the report helpers read and in the configured node order. The transition
#' label, the contingency table and the changed-edge ordering restate the
#' decisions the comparison already carries; where either decision is
#' unavailable, so are `changed` and `transition`.
#'
#' @param network_resampling_comparison List `edges`, `assessment`,
#'   `stability` from [extract_network_resampling_stability()].
#' @param codebook Codebook from [zm_codebook()].
#' @param analysis_plan Configuration from [zm_config()].
#' @return List `edges`, `transitions`, `changed_edges`, `thresholds`,
#'   `feasible`, `n_fits`, `n_success_fits`, `n_edges`, `n_changed`.
tabulate_network_comparison <- function(network_resampling_comparison, codebook, analysis_plan) {
  net <- analysis_plan$network
  nodes <- as.character(net$node_keys)
  levels <- c("present", "inconclusive", "absent")
  comparison <- network_resampling_comparison
  stability <- comparison$stability
  assessment <- comparison$assessment

  edges <- tibble::tibble(
    node_i = find_network_scale_keys(comparison$edges$node_i, codebook),
    node_j = find_network_scale_keys(comparison$edges$node_j, codebook),
    pip_full = as.numeric(comparison$edges$pip_full),
    bf_full = as.numeric(comparison$edges$bf_full),
    decision_full = as.character(comparison$edges$decision_full),
    pip_bagged = as.numeric(comparison$edges$pip_bagged),
    bf_bagged = as.numeric(comparison$edges$bf_bagged),
    decision_bagged = as.character(comparison$edges$decision_bagged),
    changed = as.logical(comparison$edges$changed),
    weight_full = as.numeric(comparison$edges$weight_full),
    weight_bagged = as.numeric(comparison$edges$weight_bagged),
    n_success = as.integer(comparison$edges$n_success)
  )
  resamples <- tibble::tibble(
    node_i = find_network_scale_keys(stability$edges$node_i, codebook),
    node_j = find_network_scale_keys(stability$edges$node_j, codebook),
    share_above_include = as.numeric(stability$edges$share_above_include),
    share_below_exclude = as.numeric(stability$edges$share_below_exclude),
    pip_resample_q05 = as.numeric(stability$edges$pip_resample_q05),
    pip_resample_q50 = as.numeric(stability$edges$pip_resample_q50),
    pip_resample_q95 = as.numeric(stability$edges$pip_resample_q95)
  )
  edges <- dplyr::left_join(edges, resamples, by = c("node_i", "node_j"))
  edges$transition <- ifelse(
    is.na(edges$changed), NA_character_,
    paste(edges$decision_full, "->", edges$decision_bagged)
  )
  edges <- ap6_network_canonical(edges, nodes)
  edges <- edges[, c(
    "node_i", "node_j", "pip_full", "bf_full", "decision_full",
    "pip_bagged", "bf_bagged", "decision_bagged",
    "share_above_include", "share_below_exclude",
    "pip_resample_q05", "pip_resample_q50", "pip_resample_q95",
    "changed", "transition", "weight_full", "weight_bagged", "n_success"
  ), drop = FALSE]

  transitions <- table(
    full = factor(edges$decision_full, levels = levels),
    bagged = factor(edges$decision_bagged, levels = levels)
  )
  changed_edges <- edges[!is.na(edges$changed) & edges$changed, , drop = FALSE]
  if (nrow(changed_edges) > 0) {
    changed_edges <- changed_edges[
      order(-abs(changed_edges$pip_full - changed_edges$pip_bagged)), , drop = FALSE]
  }
  list(
    edges = edges,
    transitions = transitions,
    changed_edges = changed_edges,
    thresholds = tibble::tibble(
      bf_include = stability$thresholds$bf_include,
      bf_exclude = stability$thresholds$bf_exclude,
      pip_include = stability$thresholds$pip_include,
      pip_exclude = stability$thresholds$pip_exclude,
      g_prior = as.numeric(stability$thresholds$g_prior),
      n_sweeps = ap6_network_n_sweeps(stability$distributions)
    ),
    feasible = isTRUE(assessment$feasible),
    n_fits = as.integer(assessment$attempted_B),
    n_success_fits = as.integer(assessment$n_success),
    n_edges = nrow(edges),
    n_changed = nrow(changed_edges)
  )
}

#' The edge-prior sweep table with its stability attributes
#'
#' The full-sample estimates at every swept prior under the column
#' names, carrying the stability the accepted route assessed and the bagged
#' classification each edge keeps. An unavailable fit leaves `stable`
#' unavailable rather than calling the edge unstable, and the unstable edges
#' are the ones the accepted route names as changed.
#'
#' @param network_prior_sensitivity List `estimates`, `stability`,
#'   `changed_edges`, `attempts_by_prior`.
#' @param network_bagged List `edges` and `assessment`.
#' @param network_settings List from [define_network_model()].
#' @param codebook Codebook from [zm_codebook()].
#' @param analysis_plan Configuration from [zm_config()].
#' @return Tibble `node_i`, `node_j`, `prior`, `pip`, `bf`, `decision`,
#'   `stable`, `decision_prereg`; attributes `priors`, `g_prior`, `bf_include`,
#'   `bf_exclude`, `n`, `nodes`, `n_sweeps`, `n_edges`, `n_stable`,
#'   `n_unstable`, `unstable_edges`, `seeds`, `feasible`.
tabulate_network_prior_sweep <- function(network_prior_sensitivity, network_bagged,
                                          network_settings, codebook, analysis_plan) {
  net <- analysis_plan$network
  nodes <- as.character(net$node_keys)
  priors <- ap6_network_prior_sweep_values(analysis_plan)
  estimates <- network_prior_sensitivity$estimates

  long <- tibble::tibble(
    node_i = find_network_scale_keys(estimates$node_i, codebook),
    node_j = find_network_scale_keys(estimates$node_j, codebook),
    prior = as.numeric(estimates$g_prior),
    pip = as.numeric(estimates$pip),
    bf = as.numeric(estimates$bf),
    decision = as.character(estimates$decision)
  )
  stability <- tibble::tibble(
    node_i = find_network_scale_keys(network_prior_sensitivity$stability$node_i, codebook),
    node_j = find_network_scale_keys(network_prior_sensitivity$stability$node_j, codebook),
    stable = as.logical(network_prior_sensitivity$stability$stable)
  )
  prereg <- tibble::tibble(
    node_i = find_network_scale_keys(network_bagged$edges$node_i, codebook),
    node_j = find_network_scale_keys(network_bagged$edges$node_j, codebook),
    decision_prereg = as.character(network_bagged$edges$decision)
  )
  out <- long |>
    dplyr::left_join(stability, by = c("node_i", "node_j")) |>
    dplyr::left_join(prereg, by = c("node_i", "node_j"))
  out <- ap6_network_canonical(out, nodes)
  order_nodes <- ap6_network_node_order(c(out$node_i, out$node_j), nodes)
  out <- out[order(
    ap6_network_rank(out$node_i, order_nodes),
    ap6_network_rank(out$node_j, order_nodes),
    match(out$prior, priors)
  ), , drop = FALSE]

  # the edges whose classification moves, named and listed in the order of the
  # configured nodes (the codebook's)
  unstable <- ap6_network_canonical(dplyr::left_join(
    tibble::tibble(
      node_i = find_network_scale_keys(network_prior_sensitivity$changed_edges$node_i, codebook),
      node_j = find_network_scale_keys(network_prior_sensitivity$changed_edges$node_j, codebook)
    ),
    prereg, by = c("node_i", "node_j")
  ), nodes)
  edge_first <- !duplicated(paste(out$node_i, out$node_j))
  attr(out, "priors") <- priors
  attr(out, "g_prior") <- as.numeric(net$g_prior)
  attr(out, "bf_include") <- net$bf_include
  attr(out, "bf_exclude") <- net$bf_exclude
  attr(out, "n") <- nrow(network_settings$data)
  attr(out, "nodes") <- nodes
  attr(out, "n_sweeps") <- ap6_network_n_sweeps(estimates)
  attr(out, "n_edges") <- sum(edge_first)
  attr(out, "n_stable") <- sum(out$stable[edge_first] %in% TRUE)
  attr(out, "n_unstable") <- nrow(unstable)
  attr(out, "unstable_edges") <- unstable
  attr(out, "seeds") <- tibble::tibble(
    prior = priors,
    seed = as.integer(attr(estimates, "seed_by_prior")[as.character(priors)])
  )
  attr(out, "feasible") <- isTRUE(network_bagged$assessment$feasible)
  out
}

#' Save comparison agreement and the resolution of every prior-sweep cell
add_network_detail_reporting_facts <- function(report) {
  report$boxplots <- calculate_network_boxplot_data(report$fits, report$comparison)
  report$answer$n_agreed <- report$answer$n_edges - report$answer$n_changed
  p <- report$prior_sweep
  report$n_prior_unstable <- nrow(attr(p, "unstable_edges"))
  sweeps <- attr(p, "n_sweeps")
  if (is.null(sweeps)) sweeps <- NA_integer_
  prior <- attr(p, "g_prior")
  if (is.null(prior)) prior <- p$prior[!is.na(p$prior)][1]
  attr(p, "bf_bounds") <- calculate_network_bf_resolution(sweeps, prior)
  attr(p, "bf_bounds_by_prior") <- stats::setNames(lapply(sort(unique(p$prior[!is.na(p$prior)])),
    function(value) calculate_network_bf_resolution(sweeps, value)),
    as.character(sort(unique(p$prior[!is.na(p$prior)]))))
  report$prior_sweep <- p
  thresholds <- rh_network_thresholds(report$comparison)
  if (is.data.frame(thresholds) && nrow(thresholds)) {
    attr(report$comparison, "bf_bounds_full") <- calculate_network_bf_resolution(thresholds$n_sweeps, thresholds$g_prior)
    n_resamples <- report$comparison$n_success_fits
    attr(report$comparison, "bf_bounds_bagged") <- calculate_network_bf_resolution(
      thresholds$n_sweeps, thresholds$g_prior, n_resamples)
  }
  report
}
