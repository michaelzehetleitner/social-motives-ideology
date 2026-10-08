# Report target of the Results section "Conditional dependencies (RQ3)".
#
# Builds the one target (_targets.R, REPORTING · RESULTS · CONDITIONAL
# DEPENDENCIES) the section, its figure and its table read. The report words
# and formats what this returns.

#' RQ3 network
#'
#' The bagged network with its edge decisions, its matrices for the figure, the
#' RQ3 decisions per research-question subset, and the counts and present edges
#' the section states.
#'
#' @param network_bagged,network_bootstrap_fits,network_questions The accepted
#'   network results (targets of the same names).
#' @param codebook Codebook from [zm_codebook()].
#' @param analysis_plan Configuration from [zm_config()].
#' @return List `fits` (one row per resample and edge), `bagged` (one row per
#'   edge, with the run's attributes), `matrices`, `decisions`, `counts` (edges
#'   present, inconclusive and absent) and `present_edges` (by descending
#'   absolute bagged weight).
assemble_report_network <- function(network_bagged, network_bootstrap_fits, network_questions, codebook,
                                    analysis_plan) {
  fits <- tabulate_network_resample_fits(network_bootstrap_fits, codebook)
  bagged <- tabulate_bagged_network(network_bagged, fits, codebook, analysis_plan)
  present_edges <- bagged[bagged$decision == "present", , drop = FALSE]
  list(
    fits = fits,
    bagged = bagged,
    matrices = ap6_network_matrix(bagged, analysis_plan$network$node_keys),
    decisions = tabulate_network_questions(network_questions, bagged, codebook),
    counts = rh_network_counts(bagged),
    present_edges = present_edges[order(-abs(present_edges$weight_bagged)), , drop = FALSE]
  )
}

#' The edges of the three RQ3 edge sets, in the rows of the report's bag
#'
#' The accepted route selects the whole bagged graph (RQ3), the
#' autonomy-to-orientation edges (the autonomy rows of RQ3a) and the
#' autonomy-to-security/arousal edges (RQ3b) (target `network_questions`).
#' This picks the same edges out of the report's bagged table, so they carry
#' its scale keys and node order, counts
#' the decisions per question, and names the node sets the route cuts them on
#' ([define_network_question_node_sets()]).
#'
#' @param network_questions List `conditional_structure`, `autonomy_orientation`,
#'   `autonomy_security_arousal` (edge tables on the canonical z columns).
#' @param bagged The report's bagged table of [tabulate_bagged_network()].
#' @param codebook Codebook from [zm_codebook()].
#' @return List `counts`, `subset_counts` (`subset`, `decision`, `n`), `conditional_structure`,
#'   `autonomy_orientation`, `autonomy_security_arousal`, `feasible`, `autonomy_nodes` and `security_arousal_nodes`.
tabulate_network_questions <- function(network_questions, bagged, codebook) {
  pair <- function(i, j) paste(pmin(i, j), pmax(i, j))
  bag_pairs <- pair(bagged$node_i, bagged$node_j)
  edges_of <- function(question) {
    keys <- pair(find_network_scale_keys(question$node_i, codebook),
                 find_network_scale_keys(question$node_j, codebook))
    bagged[bag_pairs %in% keys, , drop = FALSE]
  }
  conditional_structure <- edges_of(network_questions$conditional_structure)
  autonomy_orientation <- edges_of(network_questions$autonomy_orientation)
  autonomy_security_arousal <- edges_of(network_questions$autonomy_security_arousal)
  node_sets <- define_network_question_node_sets()
  levels <- c("present", "absent", "inconclusive")
  count <- function(x) {
    out <- tibble::tibble(
      decision = levels,
      n = unname(vapply(levels, function(l) sum(!is.na(x) & x == l), integer(1)))
    )
    if (anyNA(x)) out <- tibble::add_row(out, decision = NA_character_, n = sum(is.na(x)))
    out
  }
  list(
    counts = count(conditional_structure$decision),
    subset_counts = dplyr::bind_rows(
      dplyr::mutate(count(conditional_structure$decision), subset = "conditional_structure", .before = 1),
      dplyr::mutate(count(autonomy_orientation$decision), subset = "autonomy_orientation", .before = 1),
      dplyr::mutate(count(autonomy_security_arousal$decision), subset = "autonomy_security_arousal", .before = 1)
    ),
    conditional_structure = conditional_structure,
    autonomy_orientation = autonomy_orientation,
    autonomy_security_arousal = autonomy_security_arousal,
    feasible = attr(bagged, "feasible"),
    # the two node sets by scale key, in the codebook's order
    autonomy_nodes = zm_order_scale_keys(find_network_scale_keys(node_sets$autonomy, codebook), codebook),
    security_arousal_nodes = zm_order_scale_keys(find_network_scale_keys(node_sets$security_arousal, codebook),
                                                    codebook)
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

#' The scale keys of standardised node columns
#'
#' @param nodes Character vector of node names as the accepted route writes
#'   them (canonical z columns).
#' @param codebook Codebook from [zm_codebook()].
#' @return Character vector of scale keys.
find_network_scale_keys <- function(nodes, codebook) {
  as.character(zm_key_of_z_col(as.character(nodes), codebook))
}

#' The per-resample edge rows of the bootstrap fits
#'
#' One row per edge of every successful fit and one unavailable row per failed
#' fit, with the seed of the attempt that produced them and its status
#' (`"ok"`, `"retry_ok"`, `"failed"`). The retry information
#' is read from each fit's own attempt log.
#'
#' @param network_bootstrap_fits List `fits` and `assessment`.
#' @param codebook Codebook from [zm_codebook()].
#' @return Tibble `b`, `seed`, `node_i`, `node_j`, `weight`, `pip`, `n_sweeps`,
#'   `status`.
tabulate_network_resample_fits <- function(network_bootstrap_fits, codebook) {
  empty <- tibble::tibble(
    b = integer(), seed = integer(), node_i = character(), node_j = character(),
    weight = numeric(), pip = numeric(), n_sweeps = integer(), status = character()
  )
  rows <- lapply(network_bootstrap_fits$fits, function(fit) {
    if (!isTRUE(fit$succeeded)) {
      return(tibble::tibble(
        b = as.integer(fit$id), seed = as.integer(fit$seed),
        node_i = NA_character_, node_j = NA_character_,
        weight = NA_real_, pip = NA_real_, n_sweeps = NA_integer_, status = "failed"
      ))
    }
    attempts <- fit$attempts
    retried <- any(attempts$status == "ok" & attempts$attempt > 1L)
    tibble::tibble(
      b = as.integer(fit$id), seed = as.integer(fit$seed),
      node_i = find_network_scale_keys(fit$edges$node_i, codebook),
      node_j = find_network_scale_keys(fit$edges$node_j, codebook),
      weight = as.numeric(fit$edges$weight), pip = as.numeric(fit$edges$pip),
      n_sweeps = as.integer(fit$edges$n_sweeps),
      status = if (retried) "retry_ok" else "ok"
    )
  })
  dplyr::bind_rows(c(list(empty), rows))
}

#' The bagged edge table with its feasibility and settings attributes
#'
#' The bagged estimates under the column names the report helpers read, in the configured node
#' order, with the assessment of the run and the configured thresholds as
#' attributes. The retained-sweep count comes from the per-resample rows, which
#' carry the resolution of the chain that produced them.
#'
#' @param network_bagged List `edges` and `assessment`.
#' @param network_fits The per-resample rows of
#'   [tabulate_network_resample_fits()].
#' @param codebook Codebook from [zm_codebook()].
#' @param analysis_plan Configuration from [zm_config()].
#' @return Tibble `node_i`, `node_j`, `n_success`, `pip_bagged`, `bf_bagged`,
#'   `weight_bagged`, `decision`; attributes `feasible`, `n_fits`,
#'   `n_success_fits`, `success_rate`, `min_success_rate`, `bf_include`,
#'   `bf_exclude`, `g_prior`, `n_sweeps`.
tabulate_bagged_network <- function(network_bagged, network_fits, codebook, analysis_plan) {
  net <- analysis_plan$network
  edges <- network_bagged$edges
  assessment <- network_bagged$assessment
  out <- tibble::tibble(
    node_i = find_network_scale_keys(edges$node_i, codebook),
    node_j = find_network_scale_keys(edges$node_j, codebook),
    n_success = as.integer(edges$n_success),
    pip_bagged = as.numeric(edges$pip),
    bf_bagged = as.numeric(edges$bf),
    weight_bagged = as.numeric(edges$weight),
    decision = as.character(edges$decision)
  )
  out <- ap6_network_canonical(out, as.character(net$node_keys))
  attr(out, "feasible") <- isTRUE(assessment$feasible)
  attr(out, "n_fits") <- as.integer(assessment$attempted_B)
  attr(out, "n_success_fits") <- as.integer(assessment$n_success)
  attr(out, "success_rate") <- as.numeric(assessment$success_rate)
  attr(out, "min_success_rate") <- assessment$min_success_rate
  attr(out, "bf_include") <- net$bf_include
  attr(out, "bf_exclude") <- net$bf_exclude
  attr(out, "g_prior") <- as.numeric(net$g_prior)
  attr(out, "n_sweeps") <- ap6_network_n_sweeps(network_fits)
  out
}

#' Save the blocks, counts and cross-analysis comparisons the network prose reads
add_network_reporting_facts <- function(report, credible_associations, joint_report, analysis_plan) {
  bagged <- report$bagged
  bounds <- calculate_network_bf_resolution(attr(bagged, "n_sweeps"), attr(bagged, "g_prior"),
                                            attr(bagged, "n_success_fits"))
  attr(bagged, "bf_bounds") <- bounds
  pair <- function(a, b) paste(pmin(a, b), pmax(a, b))
  select <- function(a, b) {
    edges <- bagged[(bagged$node_i %in% a & bagged$node_j %in% b) |
      (bagged$node_i %in% b & bagged$node_j %in% a), , drop = FALSE]
    attr(edges, "bf_bounds") <- bounds
    attr(edges, "bf_precision") <- calculate_network_bf_precision(
      bagged, report$fits, pair(edges$node_i, edges$node_j))
    attr(edges, "decision_counts") <- vapply(c("present", "absent", "inconclusive"),
      function(d) sum(edges$decision %in% d), integer(1))
    attr(edges, "n_edges") <- nrow(edges)
    edges
  }
  motives <- intersect(as.character(analysis_plan$network$node_keys), as.character(analysis_plan$regression$motives))
  facets <- as.character(analysis_plan$regression$outcomes)
  autonomy <- report$decisions$autonomy_nodes
  security <- report$decisions$security_arousal_nodes
  report$blocks <- list(motive_facet = select(motives, facets),
    autonomy_orientation = select(autonomy, facets),
    autonomy_security_arousal = select(autonomy, security),
    asc_sdo = select(setdiff(facets, "sdo_dom"), "sdo_dom"))
  rows <- lapply(seq_len(nrow(credible_associations)), function(i) {
    if (!isTRUE(credible_associations$interpretable[i])) return(NULL)
    predictors <- as.character(credible_associations$predictor[[i]])
    if (!length(predictors)) return(NULL)
    tibble::tibble(motive = predictors, facet = as.character(credible_associations$outcome_key[i]))
  })
  credible <- dplyr::bind_rows(c(list(tibble::tibble(motive = character(), facet = character())), rows))
  credible <- credible[order(match(credible$motive, as.character(analysis_plan$regression$motives)),
    match(credible$facet, facets)), , drop = FALSE]
  credible$decision <- bagged$decision[match(pair(credible$motive, credible$facet), pair(bagged$node_i, bagged$node_j))]
  report$credible_pairs <- credible
  report$autonomy_credible <- credible[credible$motive %in% autonomy, , drop = FALSE]
  edges <- report$blocks$autonomy_orientation
  report$autonomy_present_only <- edges[edges$decision %in% "present" &
    !pair(edges$node_i, edges$node_j) %in% pair(credible$motive, credible$facet), , drop = FALSE]
  report$screened_off <- credible[credible$decision %in% "absent", , drop = FALSE]
  report$n_autonomy_credible <- nrow(report$autonomy_credible)
  report$n_autonomy_present_only <- nrow(report$autonomy_present_only)
  report$autonomy_credible_counts <- vapply(c("present", "absent", "inconclusive", NA_character_),
    function(decision) sum(report$autonomy_credible$decision %in% decision), integer(1))
  report$residual_comparison <- calculate_network_residual_comparison(report$blocks$asc_sdo, joint_report$residuals)
  report$counts_total <- sum(report$counts)
  report$n_motive_facet_edges <- length(analysis_plan$regression$motives) * length(analysis_plan$regression$outcomes)
  succeeded <- attr(bagged, "n_success_fits")
  attempted <- attr(bagged, "n_fits")
  report$sampling_share <- if (length(succeeded) == 1L && length(attempted) == 1L &&
    is.finite(attempted) && attempted > 0) succeeded / attempted else NA_real_
  report$all_fits_succeeded <- length(c(succeeded, attempted)) == 2L &&
    all(is.finite(c(succeeded, attempted))) && succeeded == attempted
  report$bagged <- bagged
  report
}

#' Save the comparison of ASC-SDO edges with their joint-model residual correlations
calculate_network_residual_comparison <- function(edges, residual) {
  pair <- rh_network_pair_key
  facet_key <- c(aggression = "asc_agg", submission = "asc_sub", conventionalism = "asc_conv", sdo_d = "sdo_dom")
  residual <- residual[grepl("^(aggression|submission|conventionalism)_sdo_d$", residual$pair), , drop = FALSE]
  first <- unname(facet_key[sub("_(submission|conventionalism|sdo_d)$", "", residual$pair)])
  second <- unname(facet_key[sub("^(aggression|submission|conventionalism)_", "", residual$pair)])
  decision <- edges$decision[match(pair(first, second), pair(edges$node_i, edges$node_j))]
  residual_sign <- ifelse(residual$lower > 0, "above zero", ifelse(residual$upper < 0, "below zero", "including zero"))
  differs <- (residual_sign != "including zero" & decision %in% "absent") |
    (residual_sign == "including zero" & decision %in% "present")
  list(first = first, second = second, decision = decision,
    residual_sign = residual_sign, differs = differs, any_differs = any(differs, na.rm = TRUE))
}
