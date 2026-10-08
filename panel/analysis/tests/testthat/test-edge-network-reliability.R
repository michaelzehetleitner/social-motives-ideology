# EDGE-CASE tests — empty and degenerate inputs for R/ap8_network_helpers.R,
# R/ap4_reliability.R and the SRQ1 route of R/ap9_efa.R.
#
# What a real export can produce and what each function must then do: either
# stop with a message that names the function and the condition, or return a
# well-defined empty result. Never a silent NaN / Inf and never an obscure
# error from inside easybgm, psych, lavaan or stats::cor.
# Every test here is deterministic and fits no graphical model: the degenerate
# resamples are refused before easybgm is called.

local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) {
    parent <- dirname(dir)
    if (identical(parent, dir)) stop("project root not found from ", getwd())
    dir <- parent
  }
  # The SRQ1 route and its per-scale report table need the EFA verbs and the
  # table builders; the network edge rule needs ap8_network_helpers.R and ap8_network.R.
  for (f in c("config.R", "io_qualtrics.R", "ap4_reliability.R", "ap9_efa.R",
              "ap9_pipeline.R", "report_supplement_measurement.R", "ap8_network_helpers.R",
              "ap8_network.R")) {
    source(file.path(dir, "R", f), local = FALSE)
  }
})

root <- zm_root()
analysis_plan <- zm_config(profile = "smoke", path = file.path(root, "config", "analysis_plan.yaml"))
codebook <- zm_codebook(analysis_plan)
net <- analysis_plan$network
nodes_all <- net$node_keys

# ---- fixtures -----------------------------------------------------------------

raw <- read_qualtrics_export(file.path(root, "data", "synthetic", "zm_panel_synthetic.sav"))
kept <- raw[raw$survey_status == analysis_plan$exclusions$survey_status_complete & raw$demo_gender %in% c(1, 2), ]
lo <- analysis_plan$scales$response_min
hi <- analysis_plan$scales$response_max
for (code in unique(unlist(codebook$scales$reverse_items))) {
  kept[[code]] <- (lo + hi) - kept[[code]]
}
for (i in seq_len(nrow(codebook$scales))) {
  kept[[codebook$scales$scale_key[i]]] <- rowMeans(as.matrix(kept[codebook$scales$item_codes[[i]]]))
}
# A small slice keeps psych/lavaan fast; the edge cases do not depend on n. The
# export realises the scenario simulation_truth.yaml declares, which may blank a
# few item cells (`trouble`); every degenerate input below is built by hand, so
# the slice is taken from the rows complete on all nine scale scores and a
# planted blank never shows up as an unintended missing value in a fixture.
complete_scored <- kept[stats::complete.cases(kept[codebook$scales$scale_key]), , drop = FALSE]
small <- complete_scored[seq_len(150), , drop = FALSE]
scale_keys <- codebook$scales$scale_key
key1 <- "zm_security"
items1 <- codebook$scales$item_codes[[which(scale_keys == key1)]]

one_scale <- function(key) {
  cb <- codebook
  cb$scales <- cb$scales[cb$scales$scale_key == key, , drop = FALSE]
  cb
}

# The preregistered edge rule as the RQ3 route applies it: present at BF >= bf_include,
# absent at BF <= bf_exclude, inconclusive between.
decide_edges <- function(bf, net) {
  extract_network_edge_decisions(tibble::tibble(bf = bf, weight = 0), list(settings = net))$decision
}

# A full-data fit over the first three configured nodes, for the comparison.
fabricated_full <- function() {
  pip <- c(0.99, 0.50, 0.01)
  tibble::tibble(
    node_i = nodes_all[c(1, 1, 2)], node_j = nodes_all[c(2, 3, 3)],
    weight = c(0.20, 0.10, 0.00), pip = pip, bf = ap6_network_bf(pip, net$g_prior),
    decision = decide_edges(ap6_network_bf(pip, net$g_prior), net)
  )
}

# ================================================================================
# AP8 — network on empty and degenerate node data
# ================================================================================

test_that("ap6_network_matrix() turns an empty bag into empty matrices, not into an error", {
  bagged <- tibble::tibble(node_i = character(), node_j = character(), n_success = integer(),
                           pip_bagged = numeric(), bf_bagged = numeric(), weight_bagged = numeric(),
                           decision = character())
  attr(bagged, "feasible") <- FALSE
  m <- ap6_network_matrix(bagged, net$node_keys)
  p <- length(nodes_all)
  expect_identical(m$nodes, nodes_all)
  expect_equal(dim(m$weight), c(p, p))
  expect_true(all(m$weight == 0))
  expect_true(all(m$adjacency == 0L))
  expect_true(all(m$weight_present == 0))
  expect_true(all(is.na(m$pip)))
  expect_true(all(is.na(m$bf)))
  expect_true(all(is.na(m$decision)))
  expect_false(attr(m, "feasible"))
})

test_that("the bagged Bayes factor and the decision rule pass empty and NA input through", {
  expect_equal(ap6_network_bf(numeric(0), net$g_prior), numeric(0))
  expect_true(is.na(ap6_network_bf(NA_real_, net$g_prior)))
  expect_equal(decide_edges(numeric(0), net), character(0))
  expect_true(is.na(decide_edges(NA_real_, net)))
  expect_equal(nrow(ap6_network_canonical(fabricated_full()[0, ], net$nodes)), 0)
  expect_identical(ap6_network_node_order(character(0), net$node_keys), nodes_all)
})

# ================================================================================
# AP4/AP9 — reliability and EFA on degenerate scales
# ================================================================================

test_that("ap4_alpha_raw() refuses one item, one case and items without variance", {
  x <- as.data.frame(small[, items1, drop = FALSE])
  expect_true(is.finite(ap4_alpha_raw(x)))                      # unchanged on valid data
  expect_error(ap4_alpha_raw(x[, 1, drop = FALSE]), "needs at least two items, got 1")
  expect_error(ap4_alpha_raw(x[1, , drop = FALSE]), "needs at least two cases, got 1")
  const <- x
  const[] <- 3
  expect_error(ap4_alpha_raw(const), "the items have no variance")
  expect_error(ap4_alpha_raw(const), "alpha is undefined")
})

test_that("the SRQ1 route records the criterion failure and invents no factor for too few cases", {
  # Fewer rows than items: psych drops the items left without variance, so the
  # matrix does not describe the declared item set. ap4_pack_efa_solution()
  # refuses that solution, every criterion keeps a row with an NA count and its
  # note, and the set record carries the reason with no loading solution.
  book <- one_scale(key1)
  frame <- small[1:3, items1, drop = FALSE]
  # The planted items are named by their executable column: the codebook the
  # materialisation receives maps every label onto itself.
  identity_book <- list(items = tibble::tibble(item_code = items1, source_label = items1))
  sets <- estimate_polychoric_correlations(ap4_materialise_efa_item_sets(
    frame, identity_book,
    stats::setNames(list(items1), key1), stats::setNames(list(key1), key1), seed = 1L))
  expect_lt(ncol(sets[[key1]]$correlation$value), length(items1))

  # every matrix-based method fails on that matrix; none of them yields a count
  failed <- lapply(sets, function(set) {
    c(set, list(factor_number_methods = lapply(
      stats::setNames(nm = names(name_efa_indices(analysis_plan))),
      function(method) list(ok = FALSE, value = NULL, note = "the criterion could not be computed"))))
  })
  estimated <- extract_suggested_factor_numbers(failed)
  expect_equal(nrow(estimated), 0L)
  numbers <- dplyr::bind_rows(estimated, tibble::tibble(
    item_set = key1, n_factors = 1L,
    type = factor("theoretical", levels = c("estimated", "theoretical"))))
  for (a in c("item_sets", "method_counts_by_item_set", "factor_number_results")) {
    attr(numbers, a) <- attr(estimated, a)
  }

  specifications <- add_efa_rotation_methods(
    numbers, one_factor = "none", multiple_factors = "oblimin")
  fits <- fit_efa_models(specifications, analysis_plan)
  variance <- extract_efa_explained_variance(fits)
  r <- tabulate_efa_scales(fits, numbers, variance, book, analysis_plan)

  expect_equal(names(r), key1)
  e <- r[[key1]]
  expect_equal(e$key, key1)
  expect_equal(e$n_items, length(items1))
  expect_equal(e$n, 3L)
  expect_equal(e$n_factors_used, 1L)
  expect_true(nzchar(e$error))
  expect_match(e$error, "every declared item", fixed = TRUE)
  expect_equal(as.character(e$indices$index), unname(name_efa_indices(analysis_plan)))
  expect_true(all(is.na(e$indices$n_factors)))
  expect_true(all(nzchar(e$indices$note)))
  expect_null(e$loadings)
  expect_null(e$variance)
})
