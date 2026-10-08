# LAYER 3 tests — network-comparison displays of R/report_helpers.R
# (rh_network_pip_scatter, rh_network_stability_strips, rh_network_side_by_side,
# rh_network_changed_edges_table) on fabricated
# comparison objects (tabulate_network_comparison()) with hand-computed cells

local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) {
    parent <- dirname(dir)
    if (identical(parent, dir)) stop("project root not found from ", getwd())
    dir <- parent
  }
  for (f in c("config.R", "ap8_network_helpers.R", "ap8_network.R", "report_helpers.R")) {
    source(file.path(dir, "R", f), local = FALSE)
  }
  source(file.path(dir, "tests", "support", "network-display-fixtures.R"), local = FALSE)
  assign("report_network_test_root", dir, envir = .GlobalEnv)
})

root <- report_network_test_root
analysis_plan <- zm_config(profile = "smoke", path = file.path(root, "config", "analysis_plan.yaml"))
net <- analysis_plan$network
labels <- rh_labels(zm_codebook(analysis_plan))
nodes_all <- as.character(net$node_keys)

# ---- kable helpers (as in test-report.R) ------------------------------------

kable_cells <- function(tab) {
  lines <- as.character(tab)
  lines <- lines[startsWith(lines, "|")]
  cells <- lapply(lines, function(l) trimws(strsplit(l, "|", fixed = TRUE)[[1]][-1]))
  header <- cells[[1]]
  rows <- lapply(cells[-(1:2)], function(r) stats::setNames(r, header))
  list(header = header, rows = rows)
}
kable_text <- function(tab) paste(as.character(tab), collapse = "\n")
kable_header <- function(tab) kable_cells(tab)$header
kable_nrow <- function(tab) length(kable_cells(tab)$rows)
kable_cell <- function(row, column) {
  idx <- grep(column, names(row))
  if (length(idx) != 1) stop("column '", column, "' matches ", length(idx), " headers")
  unname(row[[idx]])
}
kable_rows <- function(tab, where) {
  rows <- kable_cells(tab)$rows
  Filter(function(r) all(vapply(names(where), function(col) identical(kable_cell(r, col), unname(where[[col]])), logical(1))), rows)
}
kable_row <- function(tab, where) {
  hit <- kable_rows(tab, where)
  if (length(hit) != 1) stop(length(hit), " rows match ", paste(names(where), where, sep = " = ", collapse = ", "))
  hit[[1]]
}

# ---- fixtures ----------------------------------------------------------------

# Three resamples on three edges of three nodes, the ones the frozen comparison
# fixture of tests/support was built on; bagged decisions present / absent /
# inconclusive
n1 <- "zm_security"
n2 <- "zm_arousal"
n3 <- "zm_achievement"
fab_per_fit <- function() {
  tibble::tibble(
    b = rep(1:3, each = 3),
    seed = net$seed_base + rep(1:3, each = 3),
    node_i = rep(c(n1, n1, n2), 3),
    node_j = rep(c(n2, n3, n3), 3),
    weight = c(0.30, 0.01, 0.10, 0.20, -0.02, 0.00, 0.40, 0.04, 0.20),
    pip = c(0.95, 0.05, 0.50, 0.90, 0.10, 0.40, 1.00, 0.06, 0.60),
    status = "ok"
  )
}
# Full fit: n1-n2 inconclusive (BF 4), n1-n3 absent, n2-n3 present (BF 19)
fab_full <- function() {
  pip <- c(0.80, 0.02, 0.95)
  tibble::tibble(
    node_i = c(n1, n1, n2), node_j = c(n2, n3, n3),
    weight = c(0.25, 0.00, 0.30), pip = pip, bf = pip / (1 - pip),
    decision = c("inconclusive", "absent", "present")
  )
}
per_fit <- fab_per_fit()
full <- fab_full()
cmp <- network_fixture_network_compare   # built from these inputs; see tests/support

# Comparison with nothing changed and an infeasible bag
per_fit_bad <- dplyr::bind_rows(
  per_fit[per_fit$b != 3, ],
  tibble::tibble(
    b = 3L, seed = net$seed_base + 3L + net$retry_seed_offset, node_i = NA_character_, node_j = NA_character_,
    weight = NA_real_, pip = NA_real_, status = "failed"
  )
)
cmp_bad <- network_fixture_network_compare_infeasible

# The preregistered edge rule as the RQ3 route applies it: present at BF >= bf_include,
# absent at BF <= bf_exclude, inconclusive between.
decide_edges <- function(bf, net) {
  extract_network_edge_decisions(tibble::tibble(bf = bf, weight = 0), list(settings = net))$decision
}

edge_lab <- function(a, b) paste(rh_label(a, labels), "–", rh_label(b, labels))

# ---- shared preparation -------------------------------------------------------

test_that("rh_network_compare_edges() and rh_network_thresholds() read the comparison object", {
  e <- rh_network_compare_edges(cmp, labels)
  expect_equal(e$edge, c(edge_lab(n1, n2), edge_lab(n1, n3), edge_lab(n2, n3)))
  expect_equal(e$status, c("Decision changed", "Same decision", "Decision changed"))
  expect_true(all(rh_network_compare_edges(cmp_bad, labels)$status == "Not decided"))
  thr <- rh_network_thresholds(cmp)
  expect_equal(thr$bf_include, net$bf_include)
  expect_equal(thr$pip_include, net$bf_include / (1 + net$bf_include))
  expect_equal(thr$pip_exclude, net$bf_exclude / (1 + net$bf_exclude))
})

# ---- figures ------------------------------------------------------------------

test_that("rh_network_pip_scatter() is a ggplot on the comparison edges with threshold lines and labels", {
  p <- rh_network_pip_scatter(cmp, labels)
  expect_s3_class(p, "ggplot")
  expect_equal(nrow(p$data), 3)
  expect_true(all(c("pip_full", "pip_bagged", "status") %in% names(p$data)))
  expect_match(p$labels$x, "full-data network", fixed = TRUE)
  expect_match(p$labels$y, "bagged network", fixed = TRUE)
  # the thresholds are stated in the text before the figure; the plot has no
  # caption
  expect_null(p$labels$caption)
  layer_classes <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomAbline" %in% layer_classes)
  expect_true("GeomVline" %in% layer_classes)
  expect_true("GeomHline" %in% layer_classes)
  expect_true("GeomText" %in% layer_classes)
  text_layer <- p$layers[[which(layer_classes == "GeomText")[1]]]
  expect_equal(nrow(text_layer$data), 2)                       # the two changed edges are labelled
  expect_setequal(text_layer$data$edge, c(edge_lab(n1, n2), edge_lab(n2, n3)))
  vline <- p$layers[[which(layer_classes == "GeomVline")[1]]]
  expect_equal(sort(vline$data$xintercept), sort(c(cmp$thresholds$pip_exclude, cmp$thresholds$pip_include)))
  # no changed edges: no text layer, still a ggplot
  p_bad <- rh_network_pip_scatter(cmp_bad, labels)
  expect_s3_class(p_bad, "ggplot")
  expect_false("GeomText" %in% vapply(p_bad$layers, function(l) class(l$geom)[1], character(1)))
})

test_that("rh_network_stability_strips() orders edges by bagged PIP and marks the full-data PIP", {
  p <- rh_network_stability_strips(per_fit, cmp, labels)
  expect_s3_class(p, "ggplot")
  layer_classes <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomBoxplot" %in% layer_classes)
  expect_true("GeomPoint" %in% layer_classes)
  expect_true("GeomVline" %in% layer_classes)
  box <- p$layers[[which(layer_classes == "GeomBoxplot")[1]]]
  expect_equal(nrow(box$data), 9)                               # 3 resamples x 3 edges
  expect_s3_class(box$data$edge, "factor")
  # ordered by bagged PIP: n1-n3 (.07) < n2-n3 (.50) < n1-n2 (.95)
  expect_equal(levels(box$data$edge), c(edge_lab(n1, n3), edge_lab(n2, n3), edge_lab(n1, n2)))
  point_layers <- p$layers[layer_classes == "GeomPoint"]
  full_layer <- Filter(function(l) identical(rlang::as_label(l$mapping$x), "pip_full"), point_layers)
  expect_length(full_layer, 1)
  expect_equal(full_layer[[1]]$data$pip_full, c(0.80, 0.02, 0.95))
  expect_match(p$labels$x, "per resample", fixed = TRUE)
  # failed resamples and reversed orientation are tolerated
  flipped <- per_fit_bad
  flipped$node_i <- per_fit_bad$node_j
  flipped$node_j <- per_fit_bad$node_i
  p2 <- rh_network_stability_strips(flipped, cmp_bad, labels)
  box2 <- p2$layers[[which(vapply(p2$layers, function(l) class(l$geom)[1], character(1)) == "GeomBoxplot")[1]]]
  expect_equal(nrow(box2$data), 6)
})

test_that("rh_network_side_by_side() draws both networks on one layout, as the Results draw the network", {
  pairs <- utils::combn(nodes_all, 2)
  n_edges <- ncol(pairs)
  set.seed(5)
  pip_b <- runif(n_edges)
  pip_b[1] <- 0.99
  pip_b[2] <- 0.01
  bagged_all <- tibble::tibble(
    node_i = pairs[1, ], node_j = pairs[2, ], n_success = 12L, pip_bagged = pip_b,
    bf_bagged = ap6_network_bf(pip_b, net$g_prior), weight_bagged = round(rnorm(n_edges, 0, 0.2), 3),
    decision = decide_edges(ap6_network_bf(pip_b, net$g_prior), net)
  )
  attr(bagged_all, "feasible") <- TRUE
  pip_f <- pip_b
  pip_f[1] <- 0.5                                              # present in the bag, inconclusive in the full fit
  full_all <- tibble::tibble(
    node_i = pairs[1, ], node_j = pairs[2, ], weight = bagged_all$weight_bagged, pip = pip_f,
    bf = ap6_network_bf(pip_f, net$g_prior), decision = decide_edges(ap6_network_bf(pip_f, net$g_prior), net)
  )
  m_bag <- ap6_network_matrix(bagged_all, net$node_keys)
  m_full <- ap6_network_full_matrix(full_all, net$node_keys)
  groups <- list(Motive = analysis_plan$regression$motives, Outcome = analysis_plan$regression$outcomes)
  p <- rh_network_side_by_side(m_full, m_bag, labels, groups)
  expect_s3_class(p, "ggplot")
  layer_classes <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomSegment" %in% layer_classes)
  seg <- p$layers[[which(layer_classes == "GeomSegment")[1]]]$data
  expect_setequal(levels(seg$panel), c("Full-data network", "Bagged network"))
  # the encoding of the network figure of the Results:
  # colour and line type by the sign of a present edge, a thin dotted line for
  # an inconclusive one
  expect_true(all(seg$sign %in% c("Positive", "Negative", "Inconclusive")))
  expect_false(any(seg$decision == "absent"))
  full_rows <- seg[seg$panel == "Full-data network", ]
  bag_rows <- seg[seg$panel == "Bagged network", ]
  expect_equal(sum(full_rows$decision == "present"), sum(full_all$decision == "present"))
  expect_equal(sum(bag_rows$decision == "present"), sum(bagged_all$decision == "present"))
  first_full <- full_rows[full_rows$from == nodes_all[1] & full_rows$to == nodes_all[2], ]
  first_bag <- bag_rows[bag_rows$from == nodes_all[1] & bag_rows$to == nodes_all[2], ]
  expect_equal(first_full$sign, "Inconclusive")
  expect_equal(first_bag$sign, if (bagged_all$weight_bagged[1] >= 0) "Positive" else "Negative")
  expect_equal(first_bag$width, max(abs(bagged_all$weight_bagged[1]), 0.02))
  expect_true(all(full_rows$width[full_rows$sign == "Inconclusive"] == 0.02))
  scales <- vapply(p$scales$scales, function(s) paste(s$aesthetics, collapse = ","), "")
  colour <- p$scales$scales[[which(grepl("colour", scales))[1]]]
  expect_identical(unname(colour$palette(3)), unname(c("#0072B2", "#D55E00", "grey60")))
  # same node positions in both panels
  pts <- p$layers[[which(layer_classes == "GeomPoint")[1]]]$data
  a <- pts[pts$panel == "Full-data network", c("node", "x", "y")]
  b <- pts[pts$panel == "Bagged network", c("node", "x", "y")]
  expect_equal(a, b)
  expect_equal(nrow(a), length(nodes_all))
  expect_true(all(pts$group[pts$node %in% analysis_plan$regression$outcomes] == "Authoritarian facet"))
  # the single-network plot still works on the shared layout
  p1 <- rh_network_plot(m_bag, labels = labels, groups = groups)
  expect_s3_class(p1, "ggplot")
  lay <- rh_network_layout(m_bag, labels = labels, groups = groups)
  expect_equal(nrow(lay$pos), length(nodes_all))
  expect_equal(nrow(lay$edges), sum(bagged_all$decision != "absent"))
})

# ---- tables -------------------------------------------------------------------

test_that("rh_network_changed_edges_table() lists changed edges with both PIPs, BFs and the shares", {
  expect_s3_class(rh_network_changed_edges_table(cmp, labels, engine = "gt"), "gt_tbl")
  tab <- rh_network_changed_edges_table(cmp, labels, engine = "kable")
  # the heads name the two networks in the report's words
  expect_equal(
    kable_header(tab),
    c("Edge", "Decision: full-data → bagged", "Inclusion probability, full-data network",
      "Bayes factor, full-data network", "Inclusion probability, bagged network", "BF_bagged",
      paste0("Share of resamples with Bayes factor ≥ ", rh_fmt_bf(net$bf_include)),
      paste0("Share of resamples with Bayes factor ≤ ", rh_fmt_bf(net$bf_exclude)),
      "Middle 90% of the resamples' inclusion probabilities")
  )
  expect_equal(kable_nrow(tab), 2)
  rows <- kable_cells(tab)$rows
  above <- paste0("^Share of resamples with Bayes factor ≥ ", rh_fmt_bf(net$bf_include), "$")
  below <- paste0("^Share of resamples with Bayes factor ≤ ", rh_fmt_bf(net$bf_exclude), "$")
  middle <- "^Middle 90% of the resamples' inclusion probabilities$"
  expect_equal(kable_cell(rows[[1]], "^Edge$"), edge_lab(n2, n3))      # largest PIP difference first
  expect_equal(kable_cell(rows[[1]], "^Decision: full-data → bagged$"), "present → inconclusive")
  expect_equal(kable_cell(rows[[1]], "^Inclusion probability, full-data network$"), ".950")
  # the report's display rule for Bayes factors: whole numbers from 10 to 100
  expect_equal(kable_cell(rows[[1]], "^Bayes factor, full-data network$"), "19")
  expect_equal(kable_cell(rows[[1]], "^Inclusion probability, bagged network$"), ".500")
  expect_equal(kable_cell(rows[[1]], "^BF_bagged$"), "1.00")
  expect_equal(kable_cell(rows[[1]], above), ".00")
  expect_equal(kable_cell(rows[[1]], middle), "[.410, .590]")
  expect_equal(kable_cell(rows[[2]], "^Edge$"), edge_lab(n1, n2))
  expect_equal(kable_cell(rows[[2]], "^Decision: full-data → bagged$"), "inconclusive → present")
  expect_equal(kable_cell(rows[[2]], "^Inclusion probability, full-data network$"), ".800")
  expect_equal(kable_cell(rows[[2]], "^Bayes factor, full-data network$"), "4.00")
  expect_equal(kable_cell(rows[[2]], "^Inclusion probability, bagged network$"), ".950")
  expect_equal(kable_cell(rows[[2]], "^BF_bagged$"), "19")
  expect_equal(kable_cell(rows[[2]], above), ".67")
  expect_equal(kable_cell(rows[[2]], below), ".00")
  expect_equal(kable_cell(rows[[2]], middle), "[.905, .995]")
  expect_match(kable_text(tab), rh_fmt(net$bf_include / (1 + net$bf_include), 3, bounded = TRUE), fixed = TRUE)
  # an infeasible bag: one row saying so, never "no edge changes"
  tab_bad <- rh_network_changed_edges_table(cmp_bad, labels, engine = "kable")
  expect_equal(kable_nrow(tab_bad), 1)
  expect_equal(kable_cell(kable_cells(tab_bad)$rows[[1]], "^Edge$"), "Bagged classifications unavailable")
  expect_equal(kable_cell(kable_cells(tab_bad)$rows[[1]], "^Inclusion probability, full-data network$"), "—")
})

test_that("network comparison helpers type no threshold of the decision rule", {
  helpers <- readLines(file.path(root, "R", "report_helpers.R"), warn = FALSE)
  start <- grep("^rh_network_compare_edges <- function", helpers)
  end <- grep("^rh_network_changed_edges_table <- function", helpers)
  block <- helpers[start:length(helpers)]
  code <- block[!grepl("^\\s*#", block)]
  expect_true(length(start) == 1 && length(end) == 1 && end > start)
  expect_false(any(grepl("\\b10\\b|0\\.1\\b|10 ?/ ?11", code)))
})
