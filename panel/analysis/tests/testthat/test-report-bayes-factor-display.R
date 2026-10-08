# The report's one display rule for Bayes factors:
# two decimals from 0.01 to 9.99, whole numbers from 10 to 100, then only the
# power of ten a value exceeds, mirrored below 0.01, and never a power of ten
# beyond what the estimator can resolve. The network tables add a note on the
# precision of the large bagged Bayes factors, computed from the resamples.
# No model is fitted; the inputs are hand-built.

local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) {
    parent <- dirname(dir)
    if (identical(parent, dir)) stop("project root not found from ", getwd())
    dir <- parent
  }
  for (f in c("config.R", "ap8_network_helpers.R", "report_helpers.R", "report_bayesian_correlations.R")) {
    source(file.path(dir, "R", f), local = FALSE)
  }
})

test_that("Bayes factors take two decimals, then whole numbers, then powers of ten, mirrored below 0.01", {
  values <- c(0.0099, 0.01, 0.0612, 1, 9.994, 9.996, 10, 26.7, 99.6, 100, 100.4, 1045, 9999, 10001, 2e5)
  expect_identical(
    rh_fmt_bf(values),
    c("< 0.01", "0.01", "0.06", "1.00", "9.99", "10", "10", "27", "100", "100", "> 100", "> 1,000",
      "> 1,000", "> 10,000", "> 100,000")
  )
  expect_identical(rh_fmt_bf(c(0.004, 0.0004, 0.00004, 0.000004, 0)),
                   c("< 0.01", "< 0.001", "< 0.0001", "< 0.00001", "< 0.00001"))
  expect_identical(rh_fmt_bf(c(NA, Inf)), c("—", "> 100,000"))
  expect_identical(rh_bf_display_rule()$rungs, c(100, 1000, 10000, 100000))
})

test_that("no power of ten is shown beyond what the estimator can resolve", {
  # one fit of 1,500 retained steps at prior .50 resolves up to 1,499
  expect_identical(rh_fmt_bf(Inf, upper = 1499, lower = 1 / 1499), "> 1,000")
  expect_identical(rh_fmt_bf(0, upper = 1499, lower = 1 / 1499), "< 0.001")
  # the same fit at prior .75 resolves up to 499.67 and down to 0.000222
  expect_identical(rh_fmt_bf(c(Inf, 0), upper = 499.67, lower = 0.000222), c("> 100", "< 0.001"))
  # a finite value never exceeds the resolution, and its own rung is true
  expect_identical(rh_fmt_bf(1045, upper = 1499), "> 1,000")
  # an unknown resolution leaves an infinite or zero value without a rung
  expect_identical(rh_fmt_bf(c(Inf, 0, 1045, 2), upper = NA, lower = NA), c("—", "—", "> 1,000", "2.00"))
  # a resolution below the first power of ten names no rung at all
  expect_identical(rh_fmt_bf(Inf, upper = 50, lower = 0.02), "—")
})

test_that("network Bayes factors resolve by the sampled networks: one fit, or resamples times steps", {
  expect_identical(rh_network_bf(Inf, 1500L, 0.5), "> 1,000")
  # the bagged network averages 60 fits: 90,000 sampled networks
  expect_identical(rh_network_bf(c(Inf, 0, 1045.51, 84.39, 0.0612), 1500L, 0.5, 60L),
                   c("> 10,000", "< 0.0001", "> 1,000", "84", "0.06"))
  # the preregistered run: 600 resamples of 25,000 retained steps
  expect_identical(rh_network_bf(Inf, 25000L, 0.5, 600L), "> 100,000")
  expect_identical(rh_network_bf(c(Inf, 0), 1500L, 0.75), c("> 100", "< 0.001"))
  expect_identical(rh_network_bf(c(Inf, 0, 2), NA_integer_, 0.5), c("—", "—", "2.00"))
})

test_that("the correlation Bayes factors follow the same rule from their log values", {
  expect_identical(format_correlation_bf(c(log(3), log(.1), log(1762.2), log(4.4e5), 1000, -1000)),
                   c("3.00", "0.10", "> 1,000", "> 100,000", "> 100,000", "< 0.00001"))
  expect_identical(format_correlation_bf(c(NA_real_, Inf, -Inf)), c("—", "> 100,000", "< 0.00001"))
})

test_that("the note names the rule and the sampled networks only when a power of ten is shown", {
  expect_identical(rh_network_bf_bound_note(c(84, 0.06, 2), 1500L, 0.5, 60L), "")
  note <- rh_network_bf_bound_note(c(Inf, 2), 1500L, 0.5, 60L)
  expect_match(note, "above 100 is shown as the power of ten it exceeds", fixed = TRUE)
  expect_match(note, "below 0.01 as the power of ten it falls below", fixed = TRUE)
  expect_match(note, "90,000 sampled networks", fixed = TRUE)
  expect_match(note, "60 resamples of 1,500 sampler steps", fixed = TRUE)
  single <- rh_network_bf_bound_note(0, 1500L, 0.5)
  expect_match(single, "1,500 sampled networks can resolve.", fixed = TRUE)
  expect_false(grepl("resamples", single, fixed = TRUE))
  expect_match(rh_network_bf_bound_note(Inf, NA_integer_, 0.5), "shown as a dash", fixed = TRUE)
})

# --- the precision of the bagged Bayes factors -------------------------------

# Three edges: A–B leaves the edge out in some resamples (finite Bayes factor
# above 100), A–C is in every sampled network of every resample, A–D has a
# Bayes factor below 100.
bfd_bagged <- function(feasible = TRUE) {
  # A–B: bagged probability .99917, Bayes factor about 1,200 (shown as
  # > 1,000); two standard errors below, about 400.
  pips <- list(ab = c(rep(1, 59), 0.95), ac = rep(1, 60), ad = rep(c(0.9, 0.8), 30))
  out <- tibble::tibble(
    node_i = c("a", "a", "a"), node_j = c("b", "c", "d"),
    pip_bagged = vapply(pips, mean, 0, USE.NAMES = FALSE),
    weight_bagged = c(0.2, 0.3, 0.1),
    decision = c("present", "present", "inconclusive")
  )
  out$bf_bagged <- ap6_network_bf(out$pip_bagged, 0.5)
  attr(out, "feasible") <- feasible
  attr(out, "n_sweeps") <- 1500L
  attr(out, "n_success_fits") <- 60L
  attr(out, "n_fits") <- 60L
  attr(out, "g_prior") <- 0.5
  fits <- dplyr::bind_rows(lapply(names(pips), function(key) tibble::tibble(
    b = seq_len(60), node_i = "a", node_j = substr(key, 2, 2), pip = pips[[key]], status = "ok"
  )))
  # one resample orients the edge the other way round
  fits$node_i[fits$b == 1 & fits$node_j == "b"] <- "b"
  fits$node_j[fits$b == 1 & fits$node_i == "b"] <- "a"
  list(bagged = out, fits = fits, pips = pips)
}

test_that("the precision note gives the two-standard-error range of the shown value, from the resamples", {
  x <- bfd_bagged()
  labels <- c(a = "A", b = "B", c = "C", d = "D")
  note <- rh_network_bf_precision_note(x$bagged, x$fits, rh_network_pair_key(x$bagged$node_i, x$bagged$node_j),
                                        labels)
  # recomputed by hand: SE = SD of the per-resample probabilities / sqrt(60)
  se <- stats::sd(x$pips$ab) / sqrt(60)
  low <- ap6_network_bf(mean(x$pips$ab) - 2 * se, 0.5)
  expect_match(note, paste0("With this run's 60 resamples, Bayes factors above 100 are imprecise: the value shown as ",
                            rh_network_bf(x$bagged$bf_bagged[1], 1500L, 0.5, 60L), " for A – B could lie anywhere from about ",
                            rh_fmt_n(signif(low, 2)), " upward"), fixed = TRUE)
  expect_match(note, "The edge shown as > 10,000 was in every network sampled in all 60 resamples (90,000 in all)",
               fixed = TRUE)
  expect_false(grepl("A – D", note, fixed = TRUE))
  # only the edges of the table count
  expect_identical(rh_network_bf_precision_note(x$bagged, x$fits, "a d", labels), "")
  # no bag, no note
  y <- bfd_bagged(feasible = FALSE)
  expect_identical(rh_network_bf_precision_note(y$bagged, y$fits, "a b", labels), "")
})

test_that("a power-of-ten rung stays text in the HTML tables, never a Markdown block quote", {
  withr::local_options(sass.cache = FALSE)
  correlations <- list(
    pairs = data.frame(var_i = "zm_security", var_j = "zm_achievement", median = .3, lower = .2, upper = .4,
                       log_bf10 = log(2e5), evidence = "present", status = "ok", posterior_ess = 1000,
                       median_mcse = .001),
    metadata = list(bf_cutoff = 3, interval_level = .95))
  html <- as.character(gt::as_raw_html(build_bayesian_correlation_details(correlations, engine = "gt")))
  expect_false(grepl("<blockquote", html, fixed = TRUE))
  expect_match(html, "&gt; 100,000", fixed = TRUE)
  x <- bfd_bagged()
  matrix_html <- as.character(gt::as_raw_html(rh_network_matrix_table(
    x$bagged, rows = c("b", "c", "d"), columns = "a", labels = c(a = "A", b = "B", c = "C", d = "D"), engine = "gt")))
  expect_false(grepl("<blockquote", matrix_html, fixed = TRUE))
  expect_match(matrix_html, "&gt; 10,000", fixed = TRUE)
})
