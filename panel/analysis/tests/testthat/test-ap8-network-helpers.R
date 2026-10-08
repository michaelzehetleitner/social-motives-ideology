# R/ap8_network_helpers.R: the helpers of the RQ3 network (Bayes factors, their bounds, the edge tables and matrices)

local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) {
    parent <- dirname(dir)
    if (identical(parent, dir)) stop("project root not found from ", getwd())
    dir <- parent
  }
  for (f in c("config.R", "ap8_network_helpers.R")) {
    source(file.path(dir, "R", f), local = FALSE)
  }
})

root <- zm_root()
analysis_plan <- zm_config(profile = "smoke", path = file.path(root, "config", "analysis_plan.yaml"))
net <- analysis_plan$network
nodes_all <- net$node_keys

# `n_sweeps` carries the retained-sweep count of the fit that produced the row,
# so the bound for an inclusion probability of exactly one or zero is read from
# the chain.

test_that("ap6_network_matrix() gives symmetric p x p matrices in the configured node order", {
  p <- length(nodes_all)
  pairs <- utils::combn(nodes_all, 2)
  set.seed(11)
  n_edges <- ncol(pairs)
  pip <- runif(n_edges)
  # the nodes come in the codebook's order: security first, arousal second
  expect_identical(nodes_all[1:2], c("zm_security", "zm_arousal"))
  pip[1] <- 0.99                                 # zm_security-zm_arousal present
  absent_edge <- which((pairs[1, ] == "zm_security" & pairs[2, ] == "zm_achievement") |
                         (pairs[1, ] == "zm_achievement" & pairs[2, ] == "zm_security"))
  pip[absent_edge] <- 0.01                       # zm_security-zm_achievement absent
  bagged <- tibble::tibble(
    node_i = pairs[1, ], node_j = pairs[2, ],
    n_success = 12L, pip_bagged = pip, bf_bagged = pip / (1 - pip),
    weight_bagged = round(rnorm(n_edges, 0, 0.2), 3),
    decision = ifelse(pip / (1 - pip) >= net$bf_include, "present",
                      ifelse(pip / (1 - pip) <= net$bf_exclude, "absent", "inconclusive"))
  )
  attr(bagged, "feasible") <- TRUE
  m <- ap6_network_matrix(bagged, net$node_keys)
  expect_identical(m$nodes, nodes_all)
  for (name in c("weight", "adjacency", "weight_present", "pip", "bf")) {
    expect_equal(dim(m[[name]]), c(p, p))
    expect_identical(dimnames(m[[name]]), list(nodes_all, nodes_all))
    expect_true(isSymmetric(unname(m[[name]])))
  }
  expect_equal(dim(m$decision), c(p, p))
  expect_true(identical(m$decision, t(m$decision)))
  expect_equal(unname(diag(m$weight)), rep(0, p))
  expect_equal(unname(diag(m$adjacency)), rep(0L, p))
  expect_true(all(is.na(diag(m$pip))))
  expect_equal(sum(m$adjacency) / 2, sum(bagged$decision == "present"))
  expect_equal(m$adjacency["zm_security", "zm_arousal"], 1L)
  expect_equal(m$adjacency["zm_security", "zm_achievement"], 0L)
  expect_equal(m$weight["zm_security", "zm_arousal"], bagged$weight_bagged[1])
  expect_equal(m$weight["zm_arousal", "zm_security"], bagged$weight_bagged[1])
  expect_equal(m$weight_present["zm_security", "zm_achievement"], 0)
  expect_equal(m$weight_present, m$weight * m$adjacency)
  expect_equal(m$pip["zm_security", "zm_achievement"], 0.01)
  expect_true(attr(m, "feasible"))
  expect_identical(ap6_network_matrix(bagged, nodes_all)$weight, m$weight)
})

# ---- Bayes-factor bounds, retained sweeps, canonical edge order, full-data matrix ----

# Fabricated full-data fit for three edges a-b, a-c, b-c:
# a-b inconclusive (BF 4), a-c absent (BF ~0.02), b-c present (BF 19)
fabricated_full <- function() {
  pip <- c(0.80, 0.02, 0.95)
  tibble::tibble(
    node_i = c("a", "a", "b"), node_j = c("b", "c", "c"),
    weight = c(0.25, 0.00, 0.30), pip = pip, bf = pip / (1 - pip),
    decision = c("inconclusive", "absent", "present")
  )
}

test_that("ap6_network_bf_bounds() gives the Bayes factors one sweep in the chain can resolve", {
  b <- ap6_network_bf_bounds(750L, 0.5)
  expect_equal(b$n_sweeps, 750L)
  expect_equal(b$upper, ap6_network_bf(1 - 1 / 750, 0.5))
  expect_equal(b$lower, ap6_network_bf(1 / 750, 0.5))
  expect_equal(b$upper, 749)
  expect_true(is.finite(b$upper) && is.finite(b$lower) && b$lower > 0)
  # the prior odds move the bound with the prior
  expect_equal(ap6_network_bf_bounds(750L, 0.25)$upper, 749 * 3)
  # an unknown or degenerate sweep count yields no bound rather than a wrong one
  for (bad in list(NA_integer_, 1L, 0L, NULL, NA_real_)) {
    bb <- ap6_network_bf_bounds(bad, 0.5)
    expect_true(is.na(bb$upper) && is.na(bb$lower) && is.na(bb$n_sweeps))
  }
  expect_error(ap6_network_bf_bounds(750L, 1), class = "ap6_network_prior_error")
})

test_that("ap6_network_n_sweeps() reads one resolution or none", {
  expect_equal(ap6_network_n_sweeps(tibble::tibble(n_sweeps = c(750L, 750L))), 750L)
  expect_true(is.na(ap6_network_n_sweeps(tibble::tibble(n_sweeps = c(750L, 500L)))))
  expect_true(is.na(ap6_network_n_sweeps(tibble::tibble(n_sweeps = NA_integer_))))
  expect_true(is.na(ap6_network_n_sweeps(tibble::tibble(x = 1))))
  expect_true(is.na(ap6_network_n_sweeps(NULL)))
})

test_that("ap6_network_canonical() orients by the configured node order and sorts", {
  e <- tibble::tibble(node_i = c("asc_agg", "zm_security", "zm_power"), node_j = c("zm_power", "asc_agg", "zm_security"), v = 1:3)
  out <- ap6_network_canonical(e, net$node_keys)
  expect_equal(paste(out$node_i, out$node_j), c("zm_security zm_power", "zm_security asc_agg", "zm_power asc_agg"))
  expect_equal(out$v, c(3L, 2L, 1L))
  expect_error(ap6_network_canonical(tibble::tibble(node_i = "a", node_j = "a"), net$node_keys), "self-loops")
  expect_equal(nrow(ap6_network_canonical(e[0, ], net$node_keys)), 0)
})

test_that("ap6_network_full_matrix() lays the full fit out like the bagged matrices", {
  full <- fabricated_full()
  m <- ap6_network_full_matrix(full, c("a", "b", "c"))
  expect_identical(m$nodes, c("a", "b", "c"))
  expect_equal(m$pip["a", "b"], 0.80)
  expect_equal(m$pip["b", "a"], 0.80)
  expect_equal(m$adjacency["b", "c"], 1L)
  expect_equal(m$adjacency["a", "b"], 0L)
  expect_equal(m$weight["b", "c"], 0.30)
  expect_equal(m$weight_present["a", "b"], 0)
  expect_equal(m$decision["a", "c"], "absent")
  expect_true(attr(m, "feasible"))
})

