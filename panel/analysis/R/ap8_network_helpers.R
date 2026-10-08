# Helpers of the RQ3 network (AP8)
#
# The network route itself — the bootstrap resamples, the easybgm fits, the
# bagging and the edge decisions — is R/ap8_network.R. This file holds what the
# route, the report tables and the report figures share: the conversion of an
# inclusion probability into a Bayes factor under the prior edge-inclusion
# probability, the bound a Bayes factor at an inclusion probability of exactly
# one or zero can resolve with a given number of retained sweeps, and the edge
# tables in the configured node order and as matrices for the figures.
#
# Every numeric setting is read from analysis_plan$network (config/analysis_plan.yaml).

#' Validate a prior edge-inclusion probability
#'
#' The conversions between inclusion probability and Bayes factor divide by
#' the prior odds `prior / (1 - prior)`, which is defined only for a single
#' number strictly inside (0, 1).
#'
#' @param prior The value to check, normally `analysis_plan$network$g_prior`.
#' @param arg Name used in the error message.
#' @return `prior` as a double, invisibly usable as a value; stops otherwise
#'   with an error of class `ap6_network_prior_error`.
ap6_network_check_prior <- function(prior, arg = "prior") {
  describe <- function(x) {
    if (is.null(x)) return("NULL")
    if (length(x) != 1L) return(paste0("a ", class(x)[1], " of length ", length(x)))
    tryCatch(paste0(format(x), " (", class(x)[1], ")"), error = function(e) "an unprintable value")
  }
  ok <- is.numeric(prior) && length(prior) == 1L && !is.na(prior) &&
    is.finite(prior) && prior > 0 && prior < 1
  if (!ok) {
    stop(errorCondition(
      paste0(
        "`", arg, "` (the prior edge-inclusion probability, analysis_plan$network$g_prior) ",
        "must be a single number strictly between 0 and 1; got ", describe(prior), "."
      ),
      class = "ap6_network_prior_error"
    ))
  }
  as.numeric(prior)
}

#' Inclusion Bayes factor from an inclusion probability
#'
#' Posterior odds divided by prior odds,
#' `[pip / (1 - pip)] / [prior / (1 - prior)]` — the formula easybgm itself
#' uses for `fit$inc_BF`, where `prior` is the fit's `edge.prior` (the
#' `g.prior` it was given, i.e. `analysis_plan$network$g_prior`). The prior odds cancel
#' only at `prior = 0.5`, so every call site passes the configured prior;
#' the argument is required for that reason. `pip = 1` gives `Inf`, `pip = 0` gives
#' `0`, `NA` stays `NA`.
#'
#' @param pip Numeric vector of inclusion probabilities in [0, 1].
#' @param prior Prior edge-inclusion probability; a single number strictly
#'   between 0 and 1 (`analysis_plan$network$g_prior`). Required, without a default,
#'   so that a call site cannot silently fall back to 0.5 and reintroduce the
#'   uncorrected formula at a swept prior.
#' @return Numeric vector of Bayes factors.
ap6_network_bf <- function(pip, prior) {
  prior <- ap6_network_check_prior(prior)
  prior_odds <- prior / (1 - prior)
  out <- rep(NA_real_, length(pip))
  ok <- !is.na(pip)
  out[ok & pip >= 1] <- Inf
  out[ok & pip <= 0] <- 0
  mid <- ok & pip > 0 & pip < 1
  out[mid] <- (pip[mid] / (1 - pip[mid])) / prior_odds
  out
}

#' The Bayes factors a chain of `n_sweeps` retained sweeps can resolve
#'
#' An inclusion probability of exactly one means only that every retained
#' sweep included the edge; the finest departure from certainty the chain
#' could have shown is one sweep, i.e. `1 - 1 / n_sweeps`. The Bayes factor at
#' that probability is therefore the largest the chain can distinguish from
#' infinity, and its mirror at `1 / n_sweeps` the smallest it can distinguish
#' from zero. Reports print these as bounds where the Bayes factor is `Inf` or
#' `0`, rather than a certainty no finite sampler can deliver.
#'
#' @param n_sweeps Retained sweeps of the chain (`iter - burnin`); `NA` or
#'   fewer than two yields `NA` bounds.
#' @param prior Prior edge-inclusion probability; see [ap6_network_bf()].
#' @return List `upper`, `lower` (numeric Bayes factors) and `n_sweeps`
#'   (integer or `NA_integer_`).
ap6_network_bf_bounds <- function(n_sweeps, prior) {
  prior <- ap6_network_check_prior(prior)
  n <- suppressWarnings(as.numeric(n_sweeps))
  n <- if (length(n) == 0) NA_real_ else n[1]
  if (!is.finite(n) || n < 2) {
    return(list(upper = NA_real_, lower = NA_real_, n_sweeps = NA_integer_))
  }
  list(
    upper = ap6_network_bf(1 - 1 / n, prior),
    lower = ap6_network_bf(1 / n, prior),
    n_sweeps = as.integer(n)
  )
}

#' The single retained-sweep count of a per-fit edge table
#'
#' Every fit of one run is drawn from the same preregistered `iter`, so the
#' bag has one resolution; a table carrying several (or none) yields `NA` and
#' the displays name no bound.
#'
#' @param x Table with an `n_sweeps` column, or `NULL`.
#' @return Integer scalar or `NA_integer_`.
ap6_network_n_sweeps <- function(x) {
  v <- if (is.null(x)) NULL else x[["n_sweeps"]]
  if (is.null(v)) return(NA_integer_)
  v <- unique(as.integer(v[!is.na(v)]))
  if (length(v) == 1L) v else NA_integer_
}

#' Node rank for ordering edges
#'
#' Position in `analysis_plan$network$node_keys` for known nodes; unknown
#' nodes follow in alphabetical order.
#'
#' @param x Character vector of node names.
#' @param nodes Character vector of the configured node names (without `_z`).
#' @return Integer vector of ranks.
ap6_network_rank <- function(x, nodes) {
  known <- match(x, nodes)
  unknown <- sort(unique(x[is.na(known)]))
  ifelse(is.na(known), length(nodes) + match(x, unknown), known)
}

#' Weight and adjacency matrices of the bagged network
#'
#' Symmetric p x p matrices in the order of `nodes` for plotting and for the
#' RQ3 subsets. Edge weights are interpreted only for present edges, so a
#' masked weight matrix is returned alongside the full one.
#'
#' @param bagged The bagged edge table of the RQ3 route (`tabulate_bagged_network()`).
#' @param nodes Character vector of node names in the desired order, spelled
#'   as in `bagged$node_i` and `bagged$node_j`. Typically
#'   `analysis_plan$network$node_keys`.
#' @return List: `weight` (bagged weights, 0 where no edge row or NA),
#'   `adjacency` (1 for present edges, else 0), `weight_present`
#'   (`weight * adjacency`), `pip` (bagged inclusion probabilities, NA off
#'   the reported edges and on the diagonal), `bf` (bagged Bayes factors),
#'   `decision` (character matrix), `nodes` (the node order), and attribute
#'   `feasible` copied from `bagged`.
ap6_network_matrix <- function(bagged, nodes) {
  nodes <- as.character(nodes)
  p <- length(nodes)
  dn <- list(nodes, nodes)
  weight <- matrix(0, p, p, dimnames = dn)
  adjacency <- matrix(0L, p, p, dimnames = dn)
  pip <- matrix(NA_real_, p, p, dimnames = dn)
  bf <- matrix(NA_real_, p, p, dimnames = dn)
  decision <- matrix(NA_character_, p, p, dimnames = dn)

  i <- match(bagged$node_i, nodes)
  j <- match(bagged$node_j, nodes)
  if (any(i == j)) stop("bagged contains self-loops.")
  if (anyDuplicated(cbind(pmin(i, j), pmax(i, j)))) stop("bagged contains duplicate edges.")
  w <- ifelse(is.na(bagged$weight_bagged), 0, bagged$weight_bagged)
  a <- as.integer(!is.na(bagged$decision) & bagged$decision == "present")
  for (k in seq_along(i)) {
    weight[i[k], j[k]] <- weight[j[k], i[k]] <- w[k]
    adjacency[i[k], j[k]] <- adjacency[j[k], i[k]] <- a[k]
    pip[i[k], j[k]] <- pip[j[k], i[k]] <- bagged$pip_bagged[k]
    bf[i[k], j[k]] <- bf[j[k], i[k]] <- bagged$bf_bagged[k]
    decision[i[k], j[k]] <- decision[j[k], i[k]] <- bagged$decision[k]
  }
  out <- list(
    weight = weight,
    adjacency = adjacency,
    weight_present = weight * adjacency,
    pip = pip,
    bf = bf,
    decision = decision,
    nodes = nodes
  )
  attr(out, "feasible") <- attr(bagged, "feasible")
  out
}

# ---- full-data fit and comparison with the bagged network -------------------

#' Canonical edge orientation and order
#'
#' Orients every edge so that `node_i` precedes `node_j` in the configured
#' node order and sorts the rows by that order, so that edge tables from
#' different sources (full fit, bag, per-fit rows) join on `(node_i, node_j)`.
#'
#' @param edges Data frame with character columns `node_i`, `node_j`.
#' @param nodes Character vector of node keys (`analysis_plan$network$node_keys`).
#' @return `edges` as a tibble, reoriented and ordered; stops on self-loops.
ap6_network_canonical <- function(edges, nodes) {
  nodes <- as.character(nodes)
  edges <- tibble::as_tibble(edges)
  if (nrow(edges) == 0) return(edges)
  node_i <- as.character(edges$node_i)
  node_j <- as.character(edges$node_j)
  if (any(node_i == node_j, na.rm = TRUE)) stop("Edge table contains self-loops.")
  nodes <- ap6_network_node_order(c(node_i, node_j), nodes)
  swap <- ap6_network_rank(node_i, nodes) > ap6_network_rank(node_j, nodes)
  edges$node_i <- ifelse(swap, node_j, node_i)
  edges$node_j <- ifelse(swap, node_i, node_j)
  ord <- order(ap6_network_rank(edges$node_i, nodes), ap6_network_rank(edges$node_j, nodes))
  edges[ord, , drop = FALSE]
}

#' Node order extended by the unknown nodes of an edge table
#'
#' `ap6_network_rank()` ranks unknown nodes alphabetically within the vector
#' it is given, so two columns ranked separately can disagree. This returns
#' the configured order followed by every other node name (alphabetical), so
#' both edge columns are ranked against one list.
#'
#' @param x Character vector of node names occurring in an edge table.
#' @param nodes Character vector of the configured node keys.
#' @return Character vector: `nodes`, then the remaining names sorted.
ap6_network_node_order <- function(x, nodes) {
  nodes <- as.character(nodes)
  x <- as.character(x)
  c(nodes, sort(setdiff(unique(x[!is.na(x)]), nodes)))
}

#' Matrices of the full-data network (same layout as the bagged matrices)
#'
#' Renames the full-fit edge columns to the bagged names and delegates to
#' [ap6_network_matrix()], so the report can draw both networks with one
#' layout function.
#'
#' @param full The full-data edge table (`tabulate_full_sample_network()`).
#' @param nodes Character vector of node names in the desired order.
#' @return See [ap6_network_matrix()]; attribute `feasible` is `TRUE`.
ap6_network_full_matrix <- function(full, nodes) {
  b <- tibble::tibble(
    node_i = full$node_i, node_j = full$node_j, n_success = 1L,
    pip_bagged = full$pip, bf_bagged = full$bf, weight_bagged = full$weight, decision = full$decision
  )
  attr(b, "feasible") <- TRUE
  ap6_network_matrix(b, nodes)
}

#' The configured edge-prior sweep, in the order the plan lists it
#'
#' Reads `analysis_plan$network$g_prior_sweep`, whose elements zm_config() has checked to
#' be priors. The order of the plan is the order of the sweep, so the tables
#' and the figure follow the plan rather than a sorted value.
#'
#' @param analysis_plan Configuration from `zm_config()`; uses `analysis_plan$network$g_prior_sweep`.
#' @return Numeric vector of prior edge-inclusion probabilities in the
#'   configured order.
ap6_network_prior_sweep_values <- function(analysis_plan) {
  as.numeric(unlist(analysis_plan$network$g_prior_sweep))
}

