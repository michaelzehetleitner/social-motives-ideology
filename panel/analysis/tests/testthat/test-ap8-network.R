# R/ap8_network.R (the RQ3 network route) and the report tables built from it,
# on fabricated fit records with hand-computed cells: feasibility (570 of 600
# successful fits pass, 569 fail; zero successes keep the schema), the
# successful-resample PIP distribution, node identity and the infeasible bag.
# No network is fitted here: every fit record is fabricated, so easybgm and
# BDgraph are never called.

local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) {
    parent <- dirname(dir)
    if (identical(parent, dir)) stop("project root not found from ", getwd())
    dir <- parent
  }
  for (f in c("config.R", "ap8_network_helpers.R", "ap8_network.R", "report_results_network.R", "report_supplement_network_detail.R", "report_supplement_software.R", "report_results_associations.R",
              "ap10_inference.R", "report_helpers.R")) {
    source(file.path(dir, "R", f), local = FALSE)
  }
  assign("ap7_network_test_root", dir, envir = .GlobalEnv)
})

nw_root <- ap7_network_test_root
nw_cfg <- zm_config(profile = "smoke", path = file.path(nw_root, "config", "analysis_plan.yaml"))
nw_codebook <- zm_codebook(nw_cfg)
nw_net <- nw_cfg$network

# The first three configured nodes, as the route names them: the
# canonical standardised columns. Their scale keys come from the codebook.
nw_nodes <- as.character(nw_net$nodes)[1:3]
nw_keys <- as.character(nw_net$node_keys)[1:3]

# Four bootstrap attempts on three edges: three successful ones with the PIPs
# the assertions below are computed from, and one failed one whose absence must
# leave the denominator at three.
nw_pips <- list(
  c(0.95, 0.05, 0.50),
  c(0.90, 0.10, 0.40),
  c(1.00, 0.06, 0.60)
)
nw_weights <- list(
  c(0.30, 0.01, 0.10),
  c(0.20, -0.02, 0.00),
  c(0.40, 0.04, 0.20)
)

nw_edge_table <- function(pip, weight) {
  tibble::tibble(
    node_i = c(nw_nodes[1], nw_nodes[1], nw_nodes[2]),
    node_j = c(nw_nodes[2], nw_nodes[3], nw_nodes[3]),
    weight = weight,
    pip = pip,
    n_sweeps = 2500L
  )
}

nw_model <- function(min_success_rate = 0.5, B = 4L) {
  settings <- nw_net
  settings$B <- B
  settings$min_success_rate <- min_success_rate
  list(
    data = as.data.frame(stats::setNames(rep(list(as.numeric(1:5)), 3), nw_nodes)),
    nodes = nw_nodes,
    settings = settings
  )
}

nw_ok_fit <- function(id, pip, weight, attempts_made = 1L) {
  seed <- as.integer(nw_net$seed_base + id + (attempts_made - 1L) * nw_net$retry_seed_offset)
  made <- seq_len(attempts_made)
  failed <- attempts_made - 1L
  attempts <- tibble::tibble(
    attempt = made,
    seed = as.integer(nw_net$seed_base + id + (made - 1L) * nw_net$retry_seed_offset),
    status = c(rep("error", failed), "ok"),
    message = c(rep("fit failed", failed), NA_character_)
  )
  list(id = as.integer(id), succeeded = TRUE, edges = nw_edge_table(pip, weight),
       seed = seed, attempts = attempts)
}

nw_failed_fit <- function(id) {
  seeds <- as.integer(nw_net$seed_base + id + c(0L, nw_net$retry_seed_offset))
  list(
    id = as.integer(id), succeeded = FALSE, edges = NULL, seed = seeds[2],
    attempts = tibble::tibble(attempt = 1:2, seed = seeds, status = c("error", "error"),
                              message = c("fit failed", "fit failed again"))
  )
}

# Three successes (the second one on its retry) and one failure.
nw_fits <- function() {
  list(
    nw_ok_fit(1L, nw_pips[[1]], nw_weights[[1]]),
    nw_ok_fit(2L, nw_pips[[2]], nw_weights[[2]], attempts_made = 2L),
    nw_ok_fit(3L, nw_pips[[3]], nw_weights[[3]]),
    nw_failed_fit(4L)
  )
}

nw_bootstrap_fits <- function(model = nw_model(), fits = nw_fits()) {
  list(fits = fits, assessment = assess_network_bootstrap_success(fits, model))
}

nw_bagged <- function(model = nw_model(), bootstrap_fits = nw_bootstrap_fits(model)) {
  edges <- bootstrap_fits |>
    extract_network_bagged_estimates(model) |>
    extract_network_bayes_factors() |>
    extract_network_edge_decisions(model)
  list(edges = edges, assessment = bootstrap_fits$assessment)
}

# The descriptive full-sample comparator: inconclusive, absent, present, so
# that two of the three edges change their decision against the bag.
nw_full_sample <- function(model = nw_model()) {
  edges <- tibble::tibble(
    node_i = c(nw_nodes[1], nw_nodes[1], nw_nodes[2]),
    node_j = c(nw_nodes[2], nw_nodes[3], nw_nodes[3]),
    weight = c(0.25, 0.00, 0.30),
    pip = c(0.80, 0.02, 0.95),
    n_sweeps = 2500L,
    g_prior = as.numeric(nw_net$g_prior)
  )
  attr(edges, "fit_succeeded") <- TRUE
  attr(edges, "fit_seed") <- as.integer(nw_net$seed_base)
  attr(edges, "attempts") <- tibble::tibble(attempt = 1L, seed = as.integer(nw_net$seed_base),
                                            status = "ok", message = NA_character_)
  edges |>
    extract_network_bayes_factors() |>
    extract_network_edge_decisions(model)
}

nw_comparison <- function(model = nw_model(), bootstrap_fits = nw_bootstrap_fits(model)) {
  nw_bagged(model, bootstrap_fits) |>
    extract_network_bagged_full_comparison(nw_full_sample(model)) |>
    extract_network_resampling_stability(bootstrap_fits, model)
}

kable_text <- function(tab) paste(as.character(tab), collapse = "\n")

# ---- feasibility -------------------------------------------------------------

test_that("the registered success rate decides feasibility at 570 of 600 and refuses 569", {
  model <- nw_model(min_success_rate = nw_net$min_success_rate, B = 600L)
  expect_equal(model$settings$min_success_rate, 0.95)
  assess <- function(n_success) {
    fits <- c(
      lapply(seq_len(n_success), function(id) nw_ok_fit(id, nw_pips[[1]], nw_weights[[1]])),
      lapply(seq_len(600L - n_success), function(id) nw_failed_fit(600L + id))
    )
    assess_network_bootstrap_success(fits, model)
  }
  passed <- assess(570L)
  expect_true(passed$feasible)
  expect_identical(passed$status, "feasible")
  expect_equal(passed$attempted_B, 600)
  expect_equal(passed$n_success, 570)
  expect_equal(passed$success_rate, 0.95)
  failed <- assess(569L)
  expect_false(failed$feasible)
  expect_identical(failed$status, "computationally infeasible")
  expect_equal(failed$n_success, 569)
})

test_that("zero successes give an explicit failure status and schema-preserving tables", {
  model <- nw_model(min_success_rate = nw_net$min_success_rate, B = 4L)
  fits <- lapply(1:4, nw_failed_fit)
  bootstrap_fits <- list(fits = fits,
                         assessment = assess_network_bootstrap_success(fits, model))
  expect_false(bootstrap_fits$assessment$feasible)
  expect_identical(bootstrap_fits$assessment$status, "computationally infeasible")
  expect_equal(bootstrap_fits$assessment$n_success, 0)

  # no successful record to bind: the bound table keeps its columns
  bound <- ap7_bind_successful_network_edges(fits)
  expect_identical(names(bound), names(ap7_no_successful_network_edges()))
  expect_equal(nrow(bound), 0L)

  bagged <- nw_bagged(model, bootstrap_fits)
  expect_equal(nrow(bagged$edges), 0L)
  expect_true(all(c("pip", "bf", "decision", "weight_interpretable") %in% names(bagged$edges)))

  comparison <- bagged |>
    extract_network_bagged_full_comparison(nw_full_sample(model)) |>
    extract_network_resampling_stability(bootstrap_fits, model)
  expect_equal(nrow(comparison$edges), 3L)
  expect_true(all(is.na(comparison$edges$decision_bagged)))
  expect_true(all(is.na(comparison$edges$changed)))
  expect_equal(comparison$stability$n_success, 0L)
  expect_equal(nrow(comparison$stability$edges), 0L)
  expect_true(all(c("share_above_include", "share_below_exclude",
                    "pip_resample_q05", "pip_resample_q50", "pip_resample_q95") %in%
                    names(comparison$stability$edges)))

  network_table <- tabulate_network_comparison(comparison, nw_codebook, nw_cfg)
  expect_equal(nrow(network_table$edges), 3L)
  expect_true(all(is.na(network_table$edges$pip_resample_q50)))
  expect_equal(sum(network_table$transitions), 0)
  expect_equal(network_table$n_changed, 0L)
  expect_false(network_table$feasible)

  # Carry the actual AP8 all-failed output through the projection into every
  # comparison report helper. It remains computationally infeasible: no
  # unavailable bagged decision is displayed as an absent or inconclusive edge,
  # nor as an edge whose decision did not change.
  changed <- rh_network_changed_edges_table(network_table, engine = "kable")
  changed_text <- kable_text(changed)
  expect_match(changed_text, "Bagged classifications unavailable", fixed = TRUE)
  expect_false(grepl("No edge changes its decision", changed_text, fixed = TRUE))
  note <- rh_network_resample_note(nw_cfg, network_table$n_success_fits, network_table$n_fits,
                                   network_table$feasible)
  expect_match(note, "0 of 4 bootstrap resample fits succeeded", fixed = TRUE)
  expect_match(note, "bagged estimates and classifications are unavailable", fixed = TRUE)
  expect_false(grepl("present → absent|absent → present|inconclusive →", changed_text))
})

# ---- the bag, its distribution and its node identity --------------------------

test_that("the bag averages the successful PIPs and classifies the bagged Bayes factors", {
  bagged <- nw_bagged()
  edges <- bagged$edges
  expect_equal(nrow(edges), 3L)
  key <- paste(edges$node_i, edges$node_j)
  expect_equal(edges$pip[key == paste(nw_nodes[1], nw_nodes[2])], 0.95)
  expect_equal(edges$pip[key == paste(nw_nodes[1], nw_nodes[3])], 0.07)
  expect_equal(edges$pip[key == paste(nw_nodes[2], nw_nodes[3])], 0.50)
  expect_equal(edges$n_success, rep(3L, 3))
  expect_equal(edges$bf, (edges$pip / (1 - edges$pip)) /
                 (nw_net$g_prior / (1 - nw_net$g_prior)))
  decision <- stats::setNames(edges$decision, key)
  expect_identical(unname(decision[paste(nw_nodes[1], nw_nodes[2])]), "present")
  expect_identical(unname(decision[paste(nw_nodes[1], nw_nodes[3])]), "absent")
  expect_identical(unname(decision[paste(nw_nodes[2], nw_nodes[3])]), "inconclusive")
  # the interpretive weight is masked off the present edges
  expect_equal(sum(!is.na(edges$weight_interpretable)), 1L)
})

test_that("the resampling stability uses type-7 quantiles and the successful denominator", {
  comparison <- nw_comparison()
  stability <- comparison$stability
  expect_equal(stability$n_success, 3L)
  edges <- stability$edges
  key <- paste(edges$node_i, edges$node_j)
  pick <- function(column, i, j) edges[[column]][key == paste(nw_nodes[i], nw_nodes[j])]

  # the failed fourth resample enters neither share nor quantile
  expect_equal(edges$n_success, rep(3L, 3))
  expect_equal(pick("share_above_include", 1, 2), 2 / 3)
  expect_equal(pick("share_below_exclude", 1, 3), 2 / 3)
  expect_equal(pick("share_above_include", 2, 3), 0)

  for (spec in list(list(1, 2, c(0.95, 0.90, 1.00)), list(1, 3, c(0.05, 0.10, 0.06)),
                    list(2, 3, c(0.50, 0.40, 0.60)))) {
    values <- spec[[3]]
    expect_equal(pick("pip_resample_q05", spec[[1]], spec[[2]]),
                 stats::quantile(values, 0.05, names = FALSE, type = 7))
    expect_equal(pick("pip_resample_q50", spec[[1]], spec[[2]]),
                 stats::quantile(values, 0.50, names = FALSE, type = 7))
    expect_equal(pick("pip_resample_q95", spec[[1]], spec[[2]]),
                 stats::quantile(values, 0.95, names = FALSE, type = 7))
  }
  expect_equal(pick("pip_resample_q05", 2, 3), 0.41)
  expect_equal(pick("pip_resample_q95", 2, 3), 0.59)

  # the full successful distribution is retained, not only its summary
  distributions <- stability$distributions
  expect_equal(nrow(distributions), 9L)
  expect_setequal(unique(distributions$bootstrap_id), 1:3)
  expect_setequal(
    distributions$pip[paste(distributions$node_i, distributions$node_j) ==
                        paste(nw_nodes[2], nw_nodes[3])],
    c(0.50, 0.40, 0.60))
  expect_equal(stability$thresholds$pip_include, nw_net$bf_include / (1 + nw_net$bf_include))
  expect_equal(stability$thresholds$pip_exclude, nw_net$bf_exclude / (1 + nw_net$bf_exclude))
})

test_that("the RQ3 subsets select edges of the one fitted graph by node name", {
  nine <- as.character(nw_net$nodes)
  edges <- tibble::tibble(
    node_i = c("zm_achievement_z", "zm_power_z", "zm_security_z", "asc_sub_z"),
    node_j = c("asc_sub_z", "zm_security_z", "zm_arousal_z", "asc_conv_z")
  )
  expect_true(all(unlist(edges) %in% nine))
  expect_equal(nrow(extract_network_autonomy_orientation_edges(edges)), 1L)
  expect_equal(nrow(extract_network_autonomy_security_arousal_edges(edges)), 1L)
})

# ---- the report tables -------------------------------------------------------

test_that("the per-resample rows keep identity, seed, retry status and node keys", {
  fits <- tabulate_network_resample_fits(nw_bootstrap_fits(), nw_codebook)
  expect_identical(names(fits),
                   c("b", "seed", "node_i", "node_j", "weight", "pip", "n_sweeps", "status"))
  expect_equal(nrow(fits), 10L)                       # 3 successes x 3 edges + 1 failed row
  expect_equal(sort(unique(fits$b)), 1:4)
  expect_identical(fits$status[fits$b == 1L], rep("ok", 3))
  expect_identical(fits$status[fits$b == 2L], rep("retry_ok", 3))
  expect_identical(fits$status[fits$b == 4L], "failed")
  expect_equal(fits$seed[fits$b == 2L][1],
               as.integer(nw_net$seed_base + 2L + nw_net$retry_seed_offset))
  expect_true(all(is.na(fits$pip[fits$b == 4L])))
  # node identity comes from the codebook, never from stripping a suffix
  expect_setequal(unique(stats::na.omit(c(fits$node_i, fits$node_j))), nw_keys)
})

test_that("the bagged table carries every report column and attribute", {
  bagged <- nw_bagged()
  fits <- tabulate_network_resample_fits(nw_bootstrap_fits(), nw_codebook)
  network_table <- tabulate_bagged_network(bagged, fits, nw_codebook, nw_cfg)
  expect_identical(names(network_table), c("node_i", "node_j", "n_success", "pip_bagged",
                                    "bf_bagged", "weight_bagged", "decision"))
  expect_type(network_table$node_i, "character")
  expect_type(network_table$n_success, "integer")
  expect_setequal(c(network_table$node_i, network_table$node_j), nw_keys)
  # the configured node order, not the alphabetical grouping order of the bag
  expect_equal(paste(network_table$node_i, network_table$node_j),
               c(paste(nw_keys[1], nw_keys[2]), paste(nw_keys[1], nw_keys[3]),
                 paste(nw_keys[2], nw_keys[3])))
  expect_equal(network_table$pip_bagged, c(0.95, 0.07, 0.50))
  expect_identical(network_table$decision, c("present", "absent", "inconclusive"))
  expect_true(attr(network_table, "feasible"))
  expect_equal(attr(network_table, "n_fits"), 4L)
  expect_equal(attr(network_table, "n_success_fits"), 3L)
  expect_equal(attr(network_table, "success_rate"), 0.75)
  expect_equal(attr(network_table, "min_success_rate"), 0.5)
  expect_equal(attr(network_table, "bf_include"), nw_net$bf_include)
  expect_equal(attr(network_table, "bf_exclude"), nw_net$bf_exclude)
  expect_equal(attr(network_table, "g_prior"), as.numeric(nw_net$g_prior))
  expect_equal(attr(network_table, "n_sweeps"), 2500L)
})

test_that("the full-sample table keeps its columns and fit attributes", {
  model <- nw_model()
  network_table <- tabulate_full_sample_network(nw_full_sample(model), model, nw_codebook, nw_cfg)
  expect_identical(names(network_table), c("node_i", "node_j", "weight", "pip", "bf", "decision"))
  expect_setequal(c(network_table$node_i, network_table$node_j), nw_keys)
  expect_equal(network_table$pip, c(0.80, 0.02, 0.95))
  expect_identical(network_table$decision, c("inconclusive", "absent", "present"))
  expect_equal(attr(network_table, "seed"), as.integer(nw_net$seed_base))
  expect_equal(attr(network_table, "n"), 5L)
  expect_identical(attr(network_table, "nodes"), as.character(nw_net$node_keys))
  expect_equal(attr(network_table, "n_sweeps"), 2500L)
  expect_true(attr(network_table, "feasible"))
  expect_equal(nrow(attr(network_table, "attempts")), 1L)
  # and it lays out like the bagged matrices
  matrices <- ap6_network_full_matrix(network_table, nw_cfg$network$node_keys)
  expect_identical(matrices$nodes, as.character(nw_cfg$network$node_keys))
  expect_equal(matrices$adjacency[nw_keys[2], nw_keys[3]], 1L)
})

test_that("the comparison joins the shares and quantiles into edges and changed edges", {
  network_table <- tabulate_network_comparison(nw_comparison(), nw_codebook, nw_cfg)
  expect_setequal(names(network_table), c("edges", "transitions", "changed_edges", "thresholds",
                                   "feasible", "n_fits", "n_success_fits", "n_edges", "n_changed"))
  edges <- network_table$edges
  expect_identical(
    names(edges),
    c("node_i", "node_j", "pip_full", "bf_full", "decision_full", "pip_bagged", "bf_bagged",
      "decision_bagged", "share_above_include", "share_below_exclude", "pip_resample_q05",
      "pip_resample_q50", "pip_resample_q95", "changed", "transition", "weight_full",
      "weight_bagged", "n_success"))
  expect_equal(paste(edges$node_i, edges$node_j),
               c(paste(nw_keys[1], nw_keys[2]), paste(nw_keys[1], nw_keys[3]),
                 paste(nw_keys[2], nw_keys[3])))
  expect_equal(edges$share_above_include, c(2 / 3, 0, 0))
  expect_equal(edges$share_below_exclude, c(0, 2 / 3, 0))
  expect_equal(edges$pip_resample_q05, c(0.905, 0.051, 0.41))
  expect_equal(edges$pip_resample_q50, c(0.95, 0.06, 0.50))
  expect_equal(edges$pip_resample_q95, c(0.995, 0.096, 0.59))
  expect_equal(edges$changed, c(TRUE, FALSE, TRUE))
  expect_identical(edges$transition,
                   c("inconclusive -> present", "absent -> absent", "present -> inconclusive"))
  expect_equal(edges$n_success, rep(3L, 3))
  expect_equal(as.integer(network_table$transitions["inconclusive", "present"]), 1L)
  expect_equal(sum(network_table$transitions), 3)

  changed <- network_table$changed_edges
  expect_equal(nrow(changed), 2L)
  # largest |PIP difference| first: |.95 - .50| > |.80 - .95|
  expect_equal(paste(changed$node_i, changed$node_j),
               c(paste(nw_keys[2], nw_keys[3]), paste(nw_keys[1], nw_keys[2])))
  expect_equal(changed$pip_resample_q05, c(0.41, 0.905))
  expect_equal(changed$pip_resample_q95, c(0.59, 0.995))
  expect_equal(network_table$thresholds$pip_include, nw_net$bf_include / (1 + nw_net$bf_include))
  expect_equal(network_table$thresholds$n_sweeps, 2500L)
  expect_equal(network_table$n_fits, 4L)
  expect_equal(network_table$n_success_fits, 3L)
  expect_true(network_table$feasible)
})

test_that("the report renders the changed edges of the projection", {
  network_table <- tabulate_network_comparison(nw_comparison(), nw_codebook, nw_cfg)
  labels <- rh_labels()

  changed <- rh_network_changed_edges_table(network_table, labels, engine = "kable")
  changed_text <- kable_text(changed)
  expect_false(grepl("No edge changes its decision", changed_text, fixed = TRUE))
  expect_true(grepl(rh_network_edge_label(nw_keys[2], nw_keys[3], labels), changed_text, fixed = TRUE))
  # the transition with a real arrow, the networks named in the report's words
  expect_true(grepl("present → inconclusive", changed_text, fixed = TRUE))
  expect_true(grepl("Decision: full-data → bagged", changed_text, fixed = TRUE))
  expect_true(grepl(rh_fmt_ci(0.41, 0.59, 3, bounded = TRUE), changed_text, fixed = TRUE))
})

test_that("the prior sweep keeps every prior, its stability and the bagged decision", {
  model <- nw_model()
  priors <- ap6_network_prior_sweep_values(nw_cfg)
  results <- lapply(priors, function(prior) {
    edges <- tibble::tibble(
      node_i = c(nw_nodes[1], nw_nodes[1], nw_nodes[2]),
      node_j = c(nw_nodes[2], nw_nodes[3], nw_nodes[3]),
      weight = c(0.30, 0.01, 0.10),
      pip = if (prior == 0.75) c(0.99, 0.02, 0.995) else c(0.99, 0.02, 0.50),
      n_sweeps = 2500L
    )
    ap7_network_fit_result_edges(
      list(succeeded = TRUE, edges = edges, seed = as.integer(nw_net$seed_base),
           attempts = tibble::tibble(attempt = 1L, seed = as.integer(nw_net$seed_base),
                                     status = "ok", message = NA_character_)),
      nw_nodes, prior)
  })
  sensitivity <- ap7_bind_network_prior_results(results, priors) |>
    extract_network_bayes_factors() |>
    extract_network_edge_decisions(model) |>
    extract_network_prior_sensitivity()
  network_table <- tabulate_network_prior_sweep(sensitivity, nw_bagged(), model, nw_codebook, nw_cfg)

  expect_identical(names(network_table), c("node_i", "node_j", "prior", "pip", "bf", "decision",
                                    "stable", "decision_prereg"))
  expect_equal(nrow(network_table), 3L * length(priors))
  expect_setequal(c(network_table$node_i, network_table$node_j), nw_keys)
  expect_equal(network_table$prior[1:length(priors)], priors)
  expect_identical(attr(network_table, "priors"), priors)
  expect_equal(attr(network_table, "n_edges"), 3L)
  expect_equal(attr(network_table, "n"), 5L)
  expect_equal(attr(network_table, "n_sweeps"), 2500L)
  expect_equal(attr(network_table, "seeds")$seed, rep(as.integer(nw_net$seed_base), length(priors)))
  expect_true(attr(network_table, "feasible"))
  # the third edge is classified differently at the highest prior
  unstable <- attr(network_table, "unstable_edges")
  expect_equal(nrow(unstable), 1L)
  expect_equal(paste(unstable$node_i, unstable$node_j), paste(nw_keys[2], nw_keys[3]))
  expect_identical(unstable$decision_prereg, "inconclusive")
  expect_equal(attr(network_table, "n_stable"), 2L)
  expect_equal(attr(network_table, "n_unstable"), 1L)
})

# ---- the infeasible bag ------------------------------------------------------

test_that("an infeasible bag stays unavailable through matrices and the RQ3 questions", {
  model <- nw_model(min_success_rate = nw_net$min_success_rate, B = 4L)
  bootstrap_fits <- nw_bootstrap_fits(model)             # 3 of 4 succeed: below 95%
  expect_false(bootstrap_fits$assessment$feasible)
  bagged <- nw_bagged(model, bootstrap_fits)
  expect_equal(nrow(bagged$edges), 0L)

  network_table <- tabulate_bagged_network(
    bagged, tabulate_network_resample_fits(bootstrap_fits, nw_codebook), nw_codebook, nw_cfg)
  # no fabricated aggregate: the table is empty, typed, and says it is infeasible
  expect_equal(nrow(network_table), 0L)
  expect_type(network_table$node_i, "character")
  expect_type(network_table$pip_bagged, "double")
  expect_false(attr(network_table, "feasible"))
  expect_equal(attr(network_table, "n_success_fits"), 3L)
  expect_equal(attr(network_table, "success_rate"), 0.75)

  matrices <- ap6_network_matrix(network_table, nw_cfg$network$node_keys)
  expect_identical(matrices$nodes, as.character(nw_cfg$network$node_keys))
  expect_true(all(is.na(matrices$decision)))
  expect_true(all(is.na(matrices$pip)))
  expect_true(all(matrices$adjacency == 0L))
  expect_false(attr(matrices, "feasible"))

  questions <- list(
    conditional_structure = bagged$edges,
    autonomy_orientation = extract_network_autonomy_orientation_edges(bagged$edges),
    autonomy_security_arousal = extract_network_autonomy_security_arousal_edges(bagged$edges)
  )
  expect_equal(vapply(questions, nrow, integer(1)), c(conditional_structure = 0L,
                                                      autonomy_orientation = 0L,
                                                      autonomy_security_arousal = 0L))

  network <- tabulate_network_questions(questions, network_table, nw_codebook)
  expect_setequal(names(network), c("counts", "subset_counts", "conditional_structure", "autonomy_orientation", "autonomy_security_arousal",
                                    "feasible", "autonomy_nodes", "security_arousal_nodes"))
  expect_equal(sum(network$counts$n), 0L)
  expect_equal(nrow(network$conditional_structure), 0L)
  expect_equal(nrow(network$autonomy_orientation), 0L)
  expect_equal(nrow(network$autonomy_security_arousal), 0L)
  expect_false(network$feasible)
  expect_identical(network$autonomy_nodes, c("zm_power", "zm_prestige", "zm_achievement"))
  expect_identical(network$security_arousal_nodes,
                   setdiff(as.character(nw_cfg$regression$motives), network$autonomy_nodes))
})

test_that("tabulate_network_questions() runs on the feasible bag and keeps its subsets", {
  bagged <- nw_bagged()
  network_table <- tabulate_bagged_network(
    bagged, tabulate_network_resample_fits(nw_bootstrap_fits(), nw_codebook), nw_codebook, nw_cfg)
  questions <- list(
    conditional_structure = bagged$edges,
    autonomy_orientation = extract_network_autonomy_orientation_edges(bagged$edges),
    autonomy_security_arousal = extract_network_autonomy_security_arousal_edges(bagged$edges)
  )
  network <- tabulate_network_questions(questions, network_table, nw_codebook)
  expect_equal(sum(network$counts$n), 3L)
  expect_identical(network$counts$decision, c("present", "absent", "inconclusive"))
  expect_equal(network$counts$n, c(1L, 1L, 1L))
  expect_identical(unique(network$subset_counts$subset), c("conditional_structure", "autonomy_orientation", "autonomy_security_arousal"))
  expect_equal(nrow(network$conditional_structure), 3L)
  expect_true(network$feasible)
  # the projection's keys are the ones the subsets are cut on
  expect_setequal(c(network$conditional_structure$node_i, network$conditional_structure$node_j), nw_keys)
})

test_that("tabulate_network_questions() counts the edges of the three RQ3 edge sets in the report's rows", {
  keys <- zm_key_of_z_col(as.character(nw_cfg$network$nodes), nw_codebook)
  pairs <- utils::combn(keys, 2)
  bagged <- tibble::tibble(
    node_i = pairs[1, ], node_j = pairs[2, ], n_success = 12L,
    pip_bagged = NA_real_, bf_bagged = NA_real_, weight_bagged = 0.1, decision = NA_character_
  )
  set.seed(3)
  bagged$decision <- sample(c("present", "absent", "inconclusive"), nrow(bagged), replace = TRUE)
  bagged$decision[bagged$node_i == "zm_power" & bagged$node_j == "sdo_dom"] <- "present"
  bagged$decision[1] <- NA
  attr(bagged, "feasible") <- TRUE
  # the route's questions, on the canonical z columns of the same edges
  edges <- tibble::tibble(node_i = zm_z_col(bagged$node_i, nw_codebook),
                          node_j = zm_z_col(bagged$node_j, nw_codebook), decision = bagged$decision)
  questions <- list(
    conditional_structure = edges,
    autonomy_orientation = extract_network_autonomy_orientation_edges(edges),
    autonomy_security_arousal = extract_network_autonomy_security_arousal_edges(edges)
  )
  nd <- tabulate_network_questions(questions, bagged, nw_codebook)
  expect_named(nd, c("counts", "subset_counts", "conditional_structure", "autonomy_orientation", "autonomy_security_arousal", "feasible",
                     "autonomy_nodes", "security_arousal_nodes"))
  # both node sets in the codebook's order
  expect_equal(nd$autonomy_nodes, c("zm_power", "zm_prestige", "zm_achievement"))
  expect_equal(nd$security_arousal_nodes, c("zm_security", "zm_arousal"))
  # the whole graph (RQ3) is every edge of the nine nodes, in the report's rows
  expect_identical(nd$conditional_structure, bagged)
  expect_equal(nrow(nd$autonomy_orientation), 3 * 4)
  expect_equal(nrow(nd$autonomy_security_arousal), 3 * 2)
  expect_equal(nd$autonomy_orientation$decision[nd$autonomy_orientation$node_i == "zm_power" & nd$autonomy_orientation$node_j == "sdo_dom"], "present")
  # NA decisions are counted in their own row
  expect_true(any(is.na(nd$counts$decision)))
  expect_equal(sum(nd$counts$n), nrow(bagged))
  expect_equal(sum(nd$subset_counts$n[nd$subset_counts$subset == "autonomy_orientation"]), nrow(nd$autonomy_orientation))
  expect_equal(sum(nd$subset_counts$n[nd$subset_counts$subset == "autonomy_security_arousal"]), nrow(nd$autonomy_security_arousal))
  expect_true(nd$feasible)
})
