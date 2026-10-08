# The AP8 network route through the actual `_targets.R` commands (the target
# reader of tests/support/target-reader.R) on the synthetic export, from
# `data_network` to `report_network` and the report helpers that read the
# report tables.
#
# The ONLY substitution is the sampler: `easybgm::easybgm()` is replaced by a
# stand-in, so no network is fitted here. The stand-in returns the fields
# `ap7_extract_network_edges()` rebuilds a "bdgraph" object from, so the REAL
# `BDgraph::plinks()` computes the inclusion probabilities and the real retry,
# feasibility, bagging, decision, projection and report code runs. The
# feasibility boundary (570 of 600 successful fits pass, 569 fail) and the
# zero-success bag are tested in test-ap8-network.R; the all-failed route is
# covered once here.

local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) {
    parent <- dirname(dir)
    if (identical(parent, dir)) stop("project root not found from ", getwd())
    dir <- parent
  }
  for (f in c("config.R", "io_qualtrics.R", "ap1_exclusions.R", "ap3_preprocessing.R",
              "ap3_preparation.R", "ap3_fill.R", "ap3_data_files.R", "ap3_pipeline.R",
              "ap8_network_helpers.R", "ap8_network.R", "report_results_network.R", "report_supplement_network_detail.R", "report_supplement_software.R", "report_results_associations.R", "ap10_inference.R",
              "report_helpers.R")) {
    source(file.path(dir, "R", f), local = FALSE)
  }
  source(file.path(dir, "tests", "support", "targets-route.R"), local = FALSE)
  assign("nwr_root", dir, envir = .GlobalEnv)
})

nwr_cfg <- zm_config(profile = "smoke", path = file.path(nwr_root, "config", "analysis_plan.yaml"))
nwr_book <- zm_codebook(nwr_cfg)
nwr_net <- nwr_cfg$network
nwr_nodes <- as.character(nwr_net$nodes)
nwr_keys <- as.character(nwr_net$node_keys)
nwr_labels <- rh_labels(nwr_book)
nwr_text <- function(x) paste(as.character(x), collapse = "\n")
# body rows of a kable display (the header and its rule are the first two)
nwr_rows <- function(x) {
  lines <- grep("^\\|", strsplit(nwr_text(x), "\n")[[1]], value = TRUE)
  if (length(lines) <= 2L) character(0) else lines[-(1:2)]
}

# ---- the stand-in sampler ---------------------------------------------------
# 36 upper-triangle pairs in the order BDgraph::plinks() encodes them and
# ap7_extract_network_edges() reads them out (column-major over the nine
# configured nodes).
nwr_pair <- which(upper.tri(matrix(0, 9L, 9L)), arr.ind = TRUE)
nwr_m <- seq_len(36L)
nwr_i <- nwr_nodes[nwr_pair[, "row"]]
nwr_j <- nwr_nodes[nwr_pair[, "col"]]
nwr_key <- paste(nwr_i, nwr_j)

# Designed posterior: every fit reports 2^14 - 1 = 16383 retained sweeps, and
# an edge's inclusion count drives plinks() through a bit-plane encoding (one
# sample graph per bit, weight 2^b, so the weights sum to exactly n_sweeps).
nwr_sweeps <- 16383L
nwr_bits <- 0:13
nwr_class <- ((nwr_m - 1L) %% 3L) + 1L                  # present / absent / inconclusive
nwr_base <- c(15564L, 819L, 8192L)[nwr_class]           # pip ~ .95 / .05 / .50
# The full-sample fit (and the prior sweep built on it) lifts two edges across
# a class boundary, so `changed` and `changed_edges` are exercised.
nwr_full_base <- replace(nwr_base, c(2L, 6L), 15564L)

nwr_counts <- function(kind, id = 0L, prior = nwr_net$g_prior) {
  if (identical(kind, "bootstrap")) {
    # small deterministic wobble per resample, so the stability quantiles are
    # non-degenerate while no per-resample decision changes
    return(nwr_base + ((id * 7L + nwr_m * 13L) %% 41L - 20L) * 20L)
  }
  k <- nwr_full_base + ((nwr_m * 11L) %% 21L - 10L) * 5L
  if (identical(kind, "prior")) k <- k + if (prior < nwr_net$g_prior) -100L else 100L
  k
}
nwr_pip <- function(kind, id = 0L, prior = nwr_net$g_prior) {
  round(nwr_counts(kind, id, prior) / nwr_sweeps, 10)   # plinks(round = 10)
}
nwr_weight <- function(kind, id = 0L) {
  w <- 0.4 - nwr_m / 50
  if (identical(kind, "bootstrap")) w <- w + ((id * 3L + nwr_m) %% 7L - 3L) / 200
  w
}
nwr_bf <- function(pip, g = nwr_net$g_prior) (pip / (1 - pip)) / (g / (1 - g))
nwr_decide <- function(bf) {
  ifelse(is.na(bf), NA_character_,
         ifelse(bf >= nwr_net$bf_include, "present",
                ifelse(bf <= nwr_net$bf_exclude, "absent", "inconclusive")))
}

nwr_fit_object <- function(k, w) {
  dn <- list(nwr_nodes, nwr_nodes)
  graph <- matrix(0L, 9L, 9L, dimnames = dn)
  graph[nwr_pair] <- as.integer(k > nwr_sweeps / 2)
  parameters <- matrix(0, 9L, 9L, dimnames = dn)
  parameters[nwr_pair] <- w
  list(
    sample_graphs = vapply(nwr_bits, function(b) {
      paste(ifelse(bitwAnd(k, bitwShiftL(1L, b)) != 0L, "1", "0"), collapse = "")
    }, ""),
    graph_weights = as.numeric(bitwShiftL(1L, nwr_bits)),
    structure = graph + t(graph),
    parameters = parameters + t(parameters)
  )
}

# The fitting verbs call the sampler inside ap7_with_network_seed(), so the
# attempt is identified by the RNG state the route set for it.
nwr_seeds <- local({
  s <- as.integer(nwr_net$seed_base + c(0L, seq_len(nwr_net$B)))
  s <- c(s, s + as.integer(nwr_net$retry_seed_offset))
  e <- new.env(parent = emptyenv())
  for (seed in s) {
    ap7_with_network_seed(seed, assign(
      paste(get(".Random.seed", envir = globalenv()), collapse = ","), seed, envir = e))
  }
  e
})
nwr_log <- new.env(parent = emptyenv())
nwr_sampler <- function(fails) {
  nwr_log$calls <- list()
  function(data, ...) {
    dots <- list(...)
    seed <- get0(paste(get(".Random.seed", envir = globalenv()), collapse = ","),
                 envir = nwr_seeds, ifnotfound = NA_integer_)
    raw <- as.integer(seed - nwr_net$seed_base)
    attempt <- if (isTRUE(raw >= nwr_net$retry_seed_offset)) 2L else 1L
    id <- if (attempt == 2L) raw - as.integer(nwr_net$retry_seed_offset) else raw
    nwr_log$calls <- c(nwr_log$calls, list(c(
      list(seed = seed, id = id, attempt = attempt, data = data), dots)))
    if (fails(id, attempt)) stop("stand-in sampler refused ", id, "/", attempt)
    kind <- if (id > 0L) "bootstrap" else if (dots$g.prior == nwr_net$g_prior) "full" else "prior"
    nwr_fit_object(nwr_counts(kind, id, dots$g.prior), nwr_weight(kind, id))
  }
}
# Resample 2 fails once and succeeds on the retry; resample 3 fails twice.
nwr_fail_two <- function(id, attempt) (id == 2L && attempt == 1L) || id == 3L
nwr_fail_all <- function(id, attempt) TRUE

# ---- the route --------------------------------------------------------------
nwr_env <- new.env(parent = globalenv())
sys.source(file.path(nwr_root, "tests", "support", "target-reader.R"), nwr_env)
nwr_env$source_pipeline_functions(nwr_root)
# The preparation half is fitted by nobody and identical for both runs, so it
# is evaluated once and injected into each run's fresh target store.
nwr_prepared <- local({
  raw <- read_qualtrics_export(file.path(nwr_root, "data", "synthetic", "zm_panel_synthetic.sav"))
  reader <- nwr_env$make_target_reader(nwr_root, c(
    list(analysis_inputs = nwr_env$make_analysis_inputs(tr_intake(raw, nwr_cfg), nwr_cfg, nwr_book),
         analysis_plan = nwr_cfg, codebook = nwr_book),
    nwr_env$make_fill_stubs()))
  reader$read("data_network")
  # Cross-analysis summaries are outside this network route. Typed empty saved
  # summaries let the real network report run without fitting regression models.
  assign("report_credible_associations", tibble::tibble(
    interpretable = logical(), predictor = list(), outcome_key = character()),
    envir = reader$values)
  assign("report_joint", list(residuals = tibble::tibble(
    pair = character(), lower = numeric(), upper = numeric())), envir = reader$values)
  as.list(reader$values)
})
nwr_targets <- c(
  "data_network", "network_settings", "network_bootstrap_samples", "network_bootstrap_fits",
  "network_full_sample_fit", "network_prior_comparison_fits", "network_edge_evidence",
  "network_bagged", "network_full_sample", "network_questions", "network_prior_sensitivity",
  "network_resampling_comparison", "report_network", "supplement_network_detail")
nwr_run <- function(fails) {
  testthat::local_mocked_bindings(easybgm = nwr_sampler(fails), .package = "easybgm")
  reader <- nwr_env$make_target_reader(nwr_root, nwr_prepared)
  for (name in nwr_targets) reader$read(name)
  list(read = reader$read, calls = nwr_log$calls)
}
# The full-sample table the S5 target builds its matrix from.
nwr_full <- function(read) {
  nwr_env$tabulate_full_sample_network(read("network_full_sample"), read("network_settings"),
                                 nwr_book, nwr_cfg)
}
nwr_ok <- nwr_run(nwr_fail_two)
nwr_none <- nwr_run(nwr_fail_all)

nwr_field <- function(calls, name) vapply(calls, function(call) call[[name]], numeric(1))
nwr_ids <- nwr_field(nwr_ok$calls, "id")

# Independent expectation of the bag: the mean over the 59 successful resamples.
nwr_ok_ids <- setdiff(seq_len(as.integer(nwr_net$B)), 3L)
nwr_boot_pip <- vapply(nwr_ok_ids, function(b) nwr_pip("bootstrap", b), numeric(36L))
nwr_boot_weight <- vapply(nwr_ok_ids, function(b) nwr_weight("bootstrap", b), numeric(36L))
nwr_bag_pip <- rowMeans(nwr_boot_pip)
# route order of the bagged table: group_by(node_i, node_j) sorts it
nwr_bag_order <- order(nwr_i, nwr_j)
# the report tables reorder to the configured node order (ap6_network_canonical)
nwr_canon <- order(nwr_pair[, "row"], nwr_pair[, "col"])

# ---- data_network -> define_network_model() -----------------------------
test_that("the model takes the nine configured node columns of data_network in order", {
  data_network <- nwr_ok$read("data_network")
  model <- nwr_ok$read("network_settings")
  expect_identical(names(model$data), nwr_nodes)
  expect_identical(model$nodes, nwr_nodes)
  expect_equal(model$data, dplyr::select(data_network, dplyr::all_of(nwr_nodes)))
  expect_equal(nrow(model$data), nrow(data_network))
  expect_true(all(vapply(model$data, is.numeric, logical(1))))
  expect_false(anyNA(model$data))
  # the two definition verbs fill one settings list; it carries every field the
  # later verbs read, at the configured values
  expect_equal(model$settings[order(names(model$settings))],
               nwr_net[intersect(names(nwr_net), names(model$settings))][
                 order(intersect(names(nwr_net), names(model$settings)))])
  expect_setequal(names(model$settings),
                  c("package", "iter", "g_prior", "df_prior", "not_cont",
                    "bf_include", "bf_exclude", "g_prior_sweep",
                    "B", "seed_base", "retry_seed_offset", "min_success_rate"))
  expect_equal(model$settings$B, 60L)
  expect_equal(model$settings$iter, 3000L)
})

# ---- the model -> resample_network_participants() -----------------------
test_that("the resamples carry id, seed_base + b and the participants of that draw", {
  model <- nwr_ok$read("network_settings")
  data_network <- nwr_ok$read("data_network")
  samples <- nwr_ok$read("network_bootstrap_samples")
  n <- nrow(model$data)
  expect_length(samples, 60L)
  expect_equal(vapply(samples, function(s) s$id, numeric(1)), as.numeric(seq_len(60L)))
  expect_equal(vapply(samples, function(s) s$seed, numeric(1)),
               as.numeric(nwr_net$seed_base + seq_len(60L)))
  for (b in c(1L, 30L, 60L)) {
    set.seed(nwr_net$seed_base + b)
    rows <- sample.int(n, size = n, replace = TRUE)
    expect_equal(nrow(samples[[b]]$data), n)
    expect_equal(samples[[b]]$data, model$data[rows, , drop = FALSE])
    # participant identity: the drawn rows are rows of the AP3 hand-off
    expect_equal(samples[[b]]$data$zm_security_z, data_network$zm_security_z[rows])
    expect_equal(samples[[b]]$data$sdo_dom_z, data_network$sdo_dom_z[rows])
  }
})

# ---- the resamples -> the fits -> the feasibility assessment -----------------
test_that("every attempt receives its own resample under the configured settings", {
  samples <- nwr_ok$read("network_bootstrap_samples")
  calls <- nwr_ok$calls
  expect_equal(length(calls), 65L)          # 58 x 1 + 2 x 2 + full sample + two priors
  boot <- calls[nwr_ids > 0L]
  expect_equal(length(boot), 62L)
  expect_equal(sort(unique(nwr_field(boot, "id"))), as.numeric(seq_len(60L)))
  for (b in c(1L, 30L, 60L)) {
    call <- boot[[which(nwr_field(boot, "id") == b)[[1]]]]
    expect_equal(call$data, samples[[b]]$data)
    expect_equal(call$seed, samples[[b]]$seed)
  }
  call <- boot[[1]]
  expect_identical(call$type, "mixed")
  expect_identical(call$package, nwr_net$package)
  expect_equal(call$iter, nwr_net$iter)
  expect_equal(call$g.prior, nwr_net$g_prior)
  expect_equal(call$df.prior, nwr_net$df_prior)
  expect_equal(call$not_cont, rep(nwr_net$not_cont, 9L))
  expect_false(call$save); expect_false(call$centrality)
  expect_false(call$progress); expect_false(call$verbose)
})

test_that("the attempt log, the retry seed and the assessment follow the two failures", {
  fits <- nwr_ok$read("network_bootstrap_fits")
  retry <- as.integer(nwr_net$retry_seed_offset)
  expect_length(fits$fits, 60L)
  normal <- fits$fits[[7L]]
  expect_true(normal$succeeded)
  expect_equal(normal$attempts$attempt, 1L)
  expect_equal(normal$attempts$status, "ok")
  expect_equal(normal$seed, nwr_net$seed_base + 7L)
  retried <- fits$fits[[2L]]
  expect_true(retried$succeeded)
  expect_equal(retried$attempts$attempt, 1:2)
  expect_equal(retried$attempts$status, c("error", "ok"))
  expect_equal(retried$attempts$seed, as.integer(nwr_net$seed_base + 2L + c(0L, retry)))
  expect_equal(retried$seed, as.integer(nwr_net$seed_base + 2L + retry))
  failed <- fits$fits[[3L]]
  expect_false(failed$succeeded)
  expect_null(failed$edges)
  expect_equal(failed$attempts$status, c("error", "error"))
  expect_equal(failed$seed, as.integer(nwr_net$seed_base + 3L + retry))
  expect_equal(fits$assessment$attempted_B, 60L)
  expect_equal(fits$assessment$n_success, 59L)
  expect_equal(fits$assessment$success_rate, 59 / 60)
  expect_true(fits$assessment$feasible)
  expect_equal(fits$assessment$status, "feasible")
})

test_that("a successful fit is read back as the 36 canonical pairs of the fitted posterior", {
  edges <- nwr_ok$read("network_bootstrap_fits")$fits[[1L]]$edges
  expect_equal(nrow(edges), 36L)
  expect_identical(edges$node_i, nwr_i)
  expect_identical(edges$node_j, nwr_j)
  expect_equal(edges$pip, nwr_pip("bootstrap", 1L))
  expect_equal(edges$weight, nwr_weight("bootstrap", 1L))
  expect_equal(edges$n_sweeps, rep(nwr_sweeps, 36L))
  # the retry of resample 2 reuses the same data, so it reads the same design
  expect_equal(nwr_ok$read("network_bootstrap_fits")$fits[[2L]]$edges$pip,
               nwr_pip("bootstrap", 2L))
})

# ---- the full-sample fit and the prior comparisons ---------------------------
test_that("the full-sample fit and the prior sweep refit the model data, reusing .50", {
  model <- nwr_ok$read("network_settings")
  full <- nwr_ok$read("network_full_sample_fit")
  comparisons <- nwr_ok$read("network_prior_comparison_fits")
  whole <- nwr_ok$calls[nwr_ids == 0L]
  expect_equal(length(whole), 3L)                     # .50 fitted once, .25 and .75 refitted
  expect_equal(sort(nwr_field(whole, "g.prior")), c(0.25, 0.5, 0.75))
  expect_equal(nwr_field(whole, "seed"), rep(as.numeric(nwr_net$seed_base), 3L))
  for (call in whole) expect_equal(call$data, model$data)
  expect_true(attr(full, "fit_succeeded"))
  expect_equal(attr(full, "fit_seed"), as.integer(nwr_net$seed_base))
  expect_equal(full$pip, nwr_pip("full"))
  expect_equal(unique(full$g_prior), nwr_net$g_prior)
  expect_equal(nrow(comparisons), 108L)
  priors <- as.character(nwr_net$g_prior_sweep)
  expect_equal(attr(comparisons, "succeeded_by_prior"), stats::setNames(rep(TRUE, 3L), priors))
  expect_equal(attr(comparisons, "seed_by_prior"),
               stats::setNames(rep(as.integer(nwr_net$seed_base), 3L), priors))
  expect_named(attr(comparisons, "attempts_by_prior"), priors)
  expect_equal(attr(comparisons, "attempts_by_prior")[["0.5"]], attr(full, "attempts"))
  # the reused entry is the full-sample table itself, the other two the refits
  expect_equal(comparisons$pip[comparisons$g_prior == 0.5], nwr_pip("full"))
  expect_equal(comparisons$pip[comparisons$g_prior == 0.25], nwr_pip("prior", prior = 0.25))
  expect_equal(comparisons$pip[comparisons$g_prior == 0.75], nwr_pip("prior", prior = 0.75))
})

# ---- assessment -> bag -> Bayes factors -> decisions -------------------------
test_that("the bag averages the 59 successful resamples and classifies the bagged odds", {
  bagged <- nwr_ok$read("network_bagged")
  edges <- bagged$edges
  expect_equal(nrow(edges), 36L)
  expect_identical(edges$node_i, nwr_i[nwr_bag_order])
  expect_identical(edges$node_j, nwr_j[nwr_bag_order])
  expect_equal(edges$n_success, rep(59L, 36L))
  expect_equal(edges$pip, nwr_bag_pip[nwr_bag_order])
  expect_equal(edges$weight, rowMeans(nwr_boot_weight)[nwr_bag_order])
  expect_equal(edges$g_prior, rep(nwr_net$g_prior, 36L))
  expect_equal(edges$bf, nwr_bf(nwr_bag_pip)[nwr_bag_order])
  expect_equal(edges$decision, nwr_decide(nwr_bf(nwr_bag_pip))[nwr_bag_order])
  expect_equal(as.integer(table(edges$decision)[c("present", "absent", "inconclusive")]),
               c(12L, 12L, 12L))
  # weights are interpreted only where the edge is present
  expect_equal(is.na(edges$weight_interpretable), edges$decision != "present")
  expect_equal(edges$weight_interpretable[edges$decision == "present"],
               edges$weight[edges$decision == "present"])
  expect_equal(bagged$assessment, nwr_ok$read("network_bootstrap_fits")$assessment)
  expect_equal(nwr_ok$read("network_edge_evidence")$bf, edges$bf)
})

# ---- the decisions -> the two research-question selections -------------------
test_that("the autonomy rows of RQ3a and the RQ3b selection carry twelve and six decided edges", {
  questions <- nwr_ok$read("network_questions")
  bagged <- nwr_ok$read("network_bagged")$edges
  expect_equal(questions$conditional_structure, bagged)
  expect_equal(nrow(questions$autonomy_orientation), 12L)
  expect_equal(nrow(questions$autonomy_security_arousal), 6L)
  for (part in c("autonomy_orientation", "autonomy_security_arousal")) {
    sub <- questions[[part]]
    expect_true(all(c("decision", "pip", "bf", "weight_interpretable") %in% names(sub)))
    expect_false(anyNA(sub$decision))
    expect_equal(sub, bagged[paste(bagged$node_i, bagged$node_j) %in%
                               paste(sub$node_i, sub$node_j), ])
  }
  expect_setequal(
    unique(c(questions$autonomy_security_arousal$node_i,
             questions$autonomy_security_arousal$node_j)),
    c("zm_achievement_z", "zm_power_z", "zm_prestige_z", "zm_security_z", "zm_arousal_z"))
})

# ---- comparison, resampling stability, prior sensitivity ---------------------
test_that("the bagged-against-full comparison and the resampling stability use the route", {
  comparison <- nwr_ok$read("network_resampling_comparison")
  edges <- comparison$edges
  full_pip <- nwr_pip("full")
  expect_equal(nrow(edges), 36L)
  match_e <- match(nwr_key, paste(edges$node_i, edges$node_j))
  expect_false(anyNA(match_e))
  expect_equal(edges$pip_full[match_e], full_pip)
  expect_equal(edges$pip_bagged[match_e], nwr_bag_pip)
  expect_equal(edges$decision_full[match_e], nwr_decide(nwr_bf(full_pip)))
  expect_equal(edges$changed[match_e],
               nwr_decide(nwr_bf(full_pip)) != nwr_decide(nwr_bf(nwr_bag_pip)))
  expect_equal(which(edges$changed[match_e]), c(2L, 6L))   # the two designed flips
  stability <- comparison$stability
  expect_equal(stability$n_success, 59L)
  expect_equal(nrow(stability$distributions), 59L * 36L)
  match_s <- match(nwr_key, paste(stability$edges$node_i, stability$edges$node_j))
  expect_equal(stability$edges$n_success[match_s], rep(59L, 36L))
  expect_equal(stability$edges$share_above_include[match_s],
               rowMeans(nwr_bf(nwr_boot_pip) >= nwr_net$bf_include))
  expect_equal(stability$edges$share_below_exclude[match_s],
               rowMeans(nwr_bf(nwr_boot_pip) <= nwr_net$bf_exclude))
  for (q in c("05", "50", "95")) {
    expect_equal(
      stability$edges[[paste0("pip_resample_q", q)]][match_s],
      apply(nwr_boot_pip, 1L, stats::quantile, as.numeric(q) / 100, names = FALSE, type = 7))
  }
  expect_gt(min(stability$edges$pip_resample_q95 - stability$edges$pip_resample_q05), 0)
  expect_equal(stability$thresholds$g_prior, nwr_net$g_prior)
})

test_that("the prior sensitivity names the edges whose decision moves with the edge prior", {
  sensitivity <- nwr_ok$read("network_prior_sensitivity")
  swept <- vapply(nwr_net$g_prior_sweep, function(p) {
    nwr_decide(nwr_bf(nwr_pip(if (p == nwr_net$g_prior) "full" else "prior", prior = p), p))
  }, character(36L))
  stable <- apply(swept, 1L, function(x) length(unique(x)) == 1L)
  match_p <- match(nwr_key, paste(sensitivity$stability$node_i, sensitivity$stability$node_j))
  expect_equal(sensitivity$stability$stable[match_p], stable)
  expect_equal(sum(stable), 11L)
  expect_equal(nrow(sensitivity$changed_edges), 25L)
  # the first present edge is present at .25 and .50 and inconclusive at .75
  expect_equal(swept[1L, ], c("present", "present", "inconclusive"))
  expect_true(paste(nwr_i[1L], nwr_j[1L]) %in%
                paste(sensitivity$changed_edges$node_i, sensitivity$changed_edges$node_j))
  expect_equal(nrow(sensitivity$estimates), 108L)
  expect_named(sensitivity$attempts_by_prior, as.character(nwr_net$g_prior_sweep))
})

# ---- the report tables: the fits, bag and full sample -----------------------
test_that("the per-resample rows keep identity, retry status and codebook keys", {
  fits <- nwr_ok$read("report_network")$fits
  retry <- as.integer(nwr_net$retry_seed_offset)
  expect_identical(names(fits), c("b", "seed", "node_i", "node_j", "weight", "pip",
                                  "n_sweeps", "status"))
  expect_equal(nrow(fits), 59L * 36L + 1L)
  expect_equal(sort(unique(fits$b)), seq_len(60L))
  expect_equal(unique(fits$status[fits$b == 1L]), "ok")
  expect_equal(unique(fits$status[fits$b == 2L]), "retry_ok")
  expect_equal(unique(fits$seed[fits$b == 2L]), as.integer(nwr_net$seed_base + 2L + retry))
  failed <- fits[fits$b == 3L, ]
  expect_equal(nrow(failed), 1L)
  expect_equal(failed$status, "failed")
  expect_true(all(is.na(c(failed$node_i, failed$node_j, failed$pip, failed$weight))))
  first <- fits[fits$b == 1L, ]
  expect_identical(first$node_i, nwr_keys[nwr_pair[, "row"]])
  expect_identical(first$node_j, nwr_keys[nwr_pair[, "col"]])
  expect_equal(first$pip, nwr_pip("bootstrap", 1L))
  expect_equal(unique(fits$n_sweeps[!is.na(fits$n_sweeps)]), nwr_sweeps)
})

test_that("the bag and the full sample restate the route with its settings", {
  bagged <- nwr_ok$read("report_network")$bagged
  route <- nwr_ok$read("network_bagged")$edges
  expect_identical(names(bagged), c("node_i", "node_j", "n_success", "pip_bagged",
                                    "bf_bagged", "weight_bagged", "decision"))
  expect_equal(nrow(bagged), 36L)
  expect_equal(bagged$node_i, nwr_keys[nwr_pair[nwr_canon, "row"]])   # configured order
  expect_equal(bagged$node_j, nwr_keys[nwr_pair[nwr_canon, "col"]])
  expect_equal(bagged$pip_bagged, nwr_bag_pip[nwr_canon])
  expect_equal(bagged$bf_bagged, nwr_bf(nwr_bag_pip)[nwr_canon])
  expect_equal(bagged$decision, nwr_decide(nwr_bf(nwr_bag_pip))[nwr_canon])
  expect_equal(bagged$weight_bagged, rowMeans(nwr_boot_weight)[nwr_canon])
  expect_true(attr(bagged, "feasible"))
  expect_equal(attr(bagged, "n_fits"), 60L)
  expect_equal(attr(bagged, "n_success_fits"), 59L)
  expect_equal(attr(bagged, "success_rate"), 59 / 60)
  expect_equal(attr(bagged, "n_sweeps"), nwr_sweeps)
  expect_equal(attr(bagged, "g_prior"), nwr_net$g_prior)
  expect_setequal(paste(route$node_i, route$node_j),
                  paste(zm_z_col(bagged$node_i, nwr_book), zm_z_col(bagged$node_j, nwr_book)))

  full <- nwr_full(nwr_ok$read)
  expect_identical(names(full), c("node_i", "node_j", "weight", "pip", "bf", "decision"))
  expect_equal(full$pip, nwr_pip("full")[nwr_canon])
  expect_equal(full$decision, nwr_decide(nwr_bf(nwr_pip("full")))[nwr_canon])
  expect_equal(attr(full, "seed"), as.integer(nwr_net$seed_base))
  expect_equal(attr(full, "n"), nrow(nwr_ok$read("network_settings")$data))
  expect_equal(attr(full, "nodes"), nwr_keys)
  expect_equal(attr(full, "n_sweeps"), nwr_sweeps)
  expect_equal(attr(full, "iter"), 3000L)
  expect_true(attr(full, "feasible"))
  expect_equal(attr(full, "attempts")$status, "ok")
})

# ---- comparison, sweep, matrices, AP8 counts ---------------------------------
test_that("the comparison, matrices and AP8 counts project the route unchanged", {
  compare <- nwr_ok$read("supplement_network_detail")$comparison
  expect_equal(compare$n_edges, 36L)
  expect_equal(compare$n_changed, 2L)
  expect_true(compare$feasible)
  expect_equal(c(compare$n_fits, compare$n_success_fits), c(60L, 59L))
  expect_equal(sum(compare$transitions), 36L)
  expect_equal(compare$thresholds$n_sweeps, nwr_sweeps)
  # ordered by |PIP full - PIP bagged|: the absent-to-present flip comes first
  expect_equal(abs(compare$changed_edges$pip_full - compare$changed_edges$pip_bagged),
               sort(abs(nwr_pip("full") - nwr_bag_pip)[c(2L, 6L)], decreasing = TRUE))

  matrices <- nwr_ok$read("report_network")$matrices
  expect_identical(matrices$nodes, nwr_keys)
  expect_equal(matrices$pip, t(matrices$pip))
  expect_equal(matrices$weight, t(matrices$weight))
  bagged <- nwr_ok$read("report_network")$bagged
  expect_equal(matrices$pip[cbind(bagged$node_i, bagged$node_j)], nwr_bag_pip[nwr_canon])
  expect_equal(matrices$weight[cbind(bagged$node_i, bagged$node_j)],
               rowMeans(nwr_boot_weight)[nwr_canon])
  expect_equal(matrices$adjacency[cbind(bagged$node_i, bagged$node_j)],
               as.integer(bagged$decision == "present"))
  full_matrices <- nwr_ok$read("supplement_network_detail")$matrices_full
  full <- nwr_full(nwr_ok$read)
  expect_equal(full_matrices$pip[cbind(full$node_i, full$node_j)], nwr_pip("full")[nwr_canon])
  expect_equal(full_matrices$decision[cbind(full$node_i, full$node_j)], full$decision)

  sweep <- nwr_ok$read("supplement_network_detail")$prior_sweep
  expect_identical(names(sweep), c("node_i", "node_j", "prior", "pip", "bf", "decision",
                                   "stable", "decision_prereg"))
  expect_equal(nrow(sweep), 108L)
  expect_equal(attr(sweep, "priors"), nwr_net$g_prior_sweep)
  expect_equal(attr(sweep, "n_edges"), 36L)
  expect_equal(attr(sweep, "n_stable"), 11L)
  expect_equal(attr(sweep, "n_unstable"), 25L)
  expect_equal(attr(sweep, "n_sweeps"), nwr_sweeps)
  expect_equal(attr(sweep, "seeds")$seed, rep(as.integer(nwr_net$seed_base), 3L))
  expect_true(attr(sweep, "feasible"))
  expect_setequal(sweep$decision_prereg[!duplicated(paste(sweep$node_i, sweep$node_j))],
                  bagged$decision)

  network <- nwr_ok$read("report_network")$decisions
  expect_true(network$feasible)
  expect_equal(nrow(network$conditional_structure), 36L)
  expect_equal(nrow(network$autonomy_orientation), 12L)
  expect_equal(nrow(network$autonomy_security_arousal), 6L)
  expect_equal(network$counts$n, c(12L, 12L, 12L))       # present, absent, inconclusive
  expect_equal(sum(network$subset_counts$n), 36L + 12L + 6L)
  # the autonomy motives in the codebook's order: power, prestige, achievement
  expect_identical(network$autonomy_nodes, c("zm_power", "zm_prestige", "zm_achievement"))
})

# ---- the all-failed route ---------------------------------------------------
test_that("a run in which no fit succeeds stays empty and typed through every projection", {
  assessment <- nwr_none$read("network_bootstrap_fits")$assessment
  expect_equal(length(nwr_none$calls), 126L)             # every attempt of every fit
  expect_equal(assessment$n_success, 0L)
  expect_false(assessment$feasible)
  expect_equal(assessment$status, "computationally infeasible")
  bagged <- nwr_none$read("network_bagged")$edges
  expect_equal(nrow(bagged), 0L)
  expect_true(all(c("node_i", "node_j", "pip", "weight", "n_success", "g_prior", "bf",
                    "decision", "weight_interpretable") %in% names(bagged)))
  expect_equal(nrow(nwr_none$read("network_questions")$autonomy_orientation), 0L)
  full <- nwr_none$read("network_full_sample_fit")
  expect_false(attr(full, "fit_succeeded"))
  expect_equal(nrow(full), 36L)
  expect_true(all(is.na(full$pip)))
  expect_true(all(is.na(nwr_none$read("network_prior_sensitivity")$stability$stable)))
  report_fits <- nwr_none$read("report_network")$fits
  expect_equal(nrow(report_fits), 60L)
  expect_equal(unique(report_fits$status), "failed")
  report_bag <- nwr_none$read("report_network")$bagged
  expect_equal(nrow(report_bag), 0L)
  expect_false(attr(report_bag, "feasible"))
  expect_equal(attr(report_bag, "n_success_fits"), 0L)
  expect_true(all(is.na(nwr_none$read("report_network")$matrices$decision)))
  expect_equal(sum(nwr_none$read("report_network")$matrices$adjacency), 0L)
  unfitted <- nwr_full(nwr_none$read)
  # an unavailable Bayes factor stays an unavailable decision, never "absent"
  expect_true(all(is.na(unfitted$pip)) && all(is.na(unfitted$bf)))
  expect_true(all(is.na(unfitted$decision)))
  expect_false(attr(unfitted, "feasible"))
  compare <- nwr_none$read("supplement_network_detail")$comparison
  expect_false(compare$feasible)
  expect_equal(compare$n_changed, 0L)
  expect_true(all(is.na(compare$edges$pip_resample_q50)))
  sweep <- nwr_none$read("supplement_network_detail")$prior_sweep
  expect_equal(nrow(sweep), 108L)
  expect_true(all(is.na(sweep$stable)))
  network <- nwr_none$read("report_network")$decisions
  expect_false(network$feasible)
  expect_equal(sum(network$counts$n), 0L)
})

# ---- the report helpers on the objects the reports read --------------

nwr_helpers <- function(read, analysis_plan = nwr_cfg) {
  bagged <- read("report_network")$bagged
  compare <- read("supplement_network_detail")$comparison
  list(
    counts = rh_network_counts(bagged),
    bagged_note = rh_network_bagged_note(analysis_plan, attr(bagged, "n_success_fits")),
    pip_scatter = rh_network_pip_scatter(compare, nwr_labels),
    compare_edges = rh_network_compare_edges(compare, nwr_labels),
    thresholds = rh_network_thresholds(compare),
    strips = rh_network_stability_strips(read("report_network")$fits, compare, nwr_labels),
    estimation = rh_network_estimation_text(analysis_plan, attr(bagged, "n_sweeps"),
                                            attr(bagged, "n_success_fits")),
    bf = rh_network_bf(bagged$bf_bagged, attr(bagged, "n_sweeps"), attr(bagged, "g_prior")),
    bound_note = rh_network_bf_bound_note(bagged$bf_bagged, attr(bagged, "n_sweeps"),
                                          attr(bagged, "g_prior")))
}

test_that("every network report helper reads the route's objects", {
  shown <- nwr_helpers(nwr_ok$read)
  bagged <- nwr_ok$read("report_network")$bagged
  expect_equal(shown$counts, c(present = 12L, inconclusive = 12L, absent = 12L))
  expect_equal(as.integer(shown$counts[c("present", "inconclusive", "absent")]),
               as.integer(table(bagged$decision)[c("present", "inconclusive", "absent")]))
  expect_match(shown$estimation, rh_fmt_n(59L), fixed = TRUE)
  expect_match(shown$bagged_note, rh_fmt_n(59L), fixed = TRUE)
  # The note reports the averaging denominator, not the attempted count:
  # 59 successful fits of 60 resamples, in the note itself and in the note of
  # the network tables of the Results.
  mismatch <- paste0("averaged over the ", rh_fmt_n(59L), " successful fits of the ",
                     rh_fmt_n(60L), " bootstrap resamples")
  expect_match(shown$bagged_note, mismatch, fixed = TRUE)
  resample_note <- rh_network_resample_note(nwr_cfg, attr(bagged, "n_success_fits"), attr(bagged, "n_fits"),
                                            attr(bagged, "feasible"))
  expect_match(resample_note, mismatch, fixed = TRUE)
  expect_length(shown$bf, 36L)
  expect_false(any(grepl("Inf", shown$bf, fixed = TRUE)))
  expect_equal(shown$bound_note, "")                 # no PIP is exactly zero or one
  expect_equal(nrow(shown$compare_edges), 36L)
  expect_equal(sum(shown$compare_edges$status == "Decision changed"), 2L)
  expect_equal(shown$thresholds$n_sweeps, nwr_sweeps)
  expect_equal(shown$thresholds$pip_include, nwr_net$bf_include / (nwr_net$bf_include + 1))
  expect_s3_class(shown$pip_scatter, "ggplot")
  expect_s3_class(shown$strips, "ggplot")
})

test_that("the same helpers show the unavailable state of the all-failed route", {
  shown <- nwr_helpers(nwr_none$read)
  expect_equal(shown$counts, c(present = 0L, inconclusive = 0L, absent = 0L))
  expect_match(shown$bagged_note, "0 of 60 bootstrap resample fits succeeded", fixed = TRUE)
  expect_match(shown$bagged_note, "bagged estimates and classifications are unavailable", fixed = TRUE)
  expect_false(grepl("averaged over the 0", shown$bagged_note, fixed = TRUE))
  expect_equal(nrow(shown$compare_edges), 36L)
  expect_true(all(shown$compare_edges$status == "Not decided"))
  expect_length(shown$bf, 0L)
  expect_equal(shown$bound_note, "")
  expect_s3_class(shown$pip_scatter, "ggplot")
  expect_s3_class(shown$strips, "ggplot")
  expect_true(is.na(shown$thresholds$n_sweeps))
})

# The three network matrices of the main text show every edge: motives by
# facets, motives among themselves, facets among themselves.
nwr_matrices <- function(bagged) {
  motives <- intersect(nwr_keys, as.character(nwr_cfg$regression$motives))
  facets <- as.character(nwr_cfg$regression$outcomes)
  list(
    rh_network_matrix_table(bagged, motives, facets, nwr_labels, engine = "gt"),
    rh_network_matrix_table(bagged, motives[-1], motives[-length(motives)], nwr_labels,
                            lower_triangle = TRUE, order = motives, engine = "gt"),
    rh_network_matrix_table(bagged, facets[-1], facets[-length(facets)], nwr_labels,
                            lower_triangle = TRUE, order = facets, engine = "gt")
  )
}
nwr_cells <- function(tables) {
  cells <- unlist(lapply(tables, function(tab) unlist(tab[["_data"]][grep("^column_", names(tab[["_data"]]))])))
  cells[nzchar(cells)]
}

test_that("the network matrices report every bagged edge once, with its classification and Bayes factor", {
  bagged <- nwr_ok$read("report_network")$bagged
  cells <- nwr_cells(nwr_matrices(bagged))
  expect_equal(length(cells), 36L)
  shown_bf <- sub("^.*BF<sub>bagged</sub> ", "", sub("<br>RQ1.*$", "", cells))
  expected_bf <- rh_network_bf(bagged$bf_bagged, attr(bagged, "n_sweeps"), attr(bagged, "g_prior"),
                               attr(bagged, "n_success_fits"))
  expect_setequal(shown_bf, expected_bf)
  decisions <- sub("^[*]*([a-z]+).*$", "\\1", cells)
  expect_equal(as.integer(table(factor(decisions, c("present", "absent", "inconclusive")))),
               as.integer(table(factor(bagged$decision, c("present", "absent", "inconclusive")))))
})

test_that("the network matrices and the changed-edge table identify an unavailable bag", {
  compare <- nwr_none$read("supplement_network_detail")$comparison
  bagged <- nwr_none$read("report_network")$bagged
  # an infeasible bag arrives as the typed empty edge table: every one of the
  # 36 pairs is shown as not classified, with no number
  cells <- nwr_cells(nwr_matrices(bagged))
  expect_equal(length(cells), 36L)
  expect_true(all(cells == "not classified<br>BF<sub>bagged</sub> —"))
  expect_match(rh_network_resample_note(nwr_cfg, attr(bagged, "n_success_fits"), attr(bagged, "n_fits"),
                                        attr(bagged, "feasible")),
               "bagged estimates and classifications are unavailable", fixed = TRUE)
  changed <- rh_network_changed_edges_table(compare, nwr_labels, engine = "kable")
  expect_match(nwr_text(changed), "Bagged classifications unavailable", fixed = TRUE)
  expect_false(grepl("No edge changes its decision", nwr_text(changed), fixed = TRUE))
})
