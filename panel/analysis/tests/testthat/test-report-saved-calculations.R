# Saved report facts retain the existing statistical transforms, including
# incomplete fits, configured response ranges, exact bin boundaries and NA.
local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) {
    parent <- dirname(dir)
    if (identical(parent, dir)) stop("project root not found from ", getwd())
    dir <- parent
  }
  for (f in c("config.R", "ap8_network_helpers.R", "report_helpers.R")) {
    source(file.path(dir, "R", f), local = FALSE)
  }
  assign("report_saved_test_root", dir, envir = .GlobalEnv)
})

saved_plot_coordinates <- function(plot, layer, columns) {
  rows <- as.data.frame(ggplot2::ggplot_build(plot)$data[[layer]])[columns]
  rows <- rows[do.call(order, rows), , drop = FALSE]
  rownames(rows) <- NULL
  rows
}

test_that("saved histogram bins preserve exact boundaries, missing data and configured ranges", {
  for (case in list(
    list(values = c(seq(-2, 2, length.out = 21), -2, 0, 2, NA_real_), range = c(-2, 2), width = NA_real_),
    list(values = c(0, 2, 2, 4, 6, NA_real_), range = c(0, 6), width = 2)
  )) {
    bins <- calculate_histogram_data(case$values, case$range, case$width)
    expect_identical(attr(bins, "response_range"), case$range)
    expect_equal(sum(bins$count), sum(!is.na(case$values)))
    original <- rh_plot_histogram(case$values, case$range, case$width)
    saved <- plot_saved_histogram_data(bins)
    columns <- c("xmin", "xmax", "ymin", "ymax", "fill", "colour", "linewidth")
    expect_equal(saved_plot_coordinates(saved, 1, columns),
                 saved_plot_coordinates(original, 1, columns))
    expect_equal(ggplot2::ggplot_build(saved)$layout$panel_params[[1]]$x.range,
                 ggplot2::ggplot_build(original)$layout$panel_params[[1]]$x.range)
  }
  expect_null(calculate_histogram_data(c(NA_real_, NA_real_), c(1, 6)))
  expect_s3_class(plot_saved_histogram_data(NULL), "ggplot")
  expect_error(calculate_histogram_data(c(1, Inf), c(1, 6)), "finite numeric")
  expect_error(calculate_histogram_data(c(1, 7), c(1, 6)), "outside")
})

test_that("saved predictive densities support one remaining outcome and unavailable fits", {
  one <- tibble::tibble(outcome = "asc_agg", type = rep(c("y", "y_rep"), each = 12),
    draw = rep(c(NA_integer_, 1L), each = 12), obs = rep(1:12, 2), value = seq(-1, 1, length.out = 24))
  for (draws in list(one, dplyr::bind_rows(one, transform(one, outcome = "sdo_agg", value = value + 1)))) {
    saved <- calculate_predictive_density_data(draws)
    expect_setequal(saved$outcome, draws$outcome)
    original <- rh_pp_plot(draws)
    candidate <- rh_pp_plot(NULL, density_data = saved)
    for (layer in 1:2) {
      expect_equal(saved_plot_coordinates(candidate, layer, c("PANEL", "x", "y")),
                   saved_plot_coordinates(original, layer, c("PANEL", "x", "y")), tolerance = 1e-14)
    }
  }
  missing <- one[FALSE, ]
  attr(missing, "unavailable_predictions") <- data.frame(outcome = "asc_agg", note = "Fit unavailable")
  expect_equal(nrow(calculate_predictive_density_data(missing)), 0L)
  expect_s3_class(rh_pp_plot(NULL, density_data = calculate_predictive_density_data(missing)), "ggplot")
})

test_that("saved network boxes omit failed fits and preserve resample summaries", {
  edges <- tibble::tibble(node_i = c("a", "a"), node_j = c("b", "c"),
                         pip_bagged = c(.7, .2), pip_full = c(.6, .1))
  fits <- tibble::tibble(node_i = rep(c("a", "a"), each = 5), node_j = rep(c("b", "c"), each = 5),
                        status = rep(c("ok", "retry_ok", "ok", "ok", "failed"), 2),
                        pip = c(.1, .7, .8, .9, 1, 0, .1, .2, .8, 1))
  boxes <- calculate_network_boxplot_data(fits, list(edges = edges))
  expect_equal(boxes$xmiddle[boxes$key == rh_network_pair_key("a", "b")], .75)
  expect_equal(boxes$xmiddle[boxes$key == rh_network_pair_key("a", "c")], .15)
  expect_setequal(boxes$key, rh_network_pair_key(edges$node_i, edges$node_j))
  fits$status[] <- "failed"
  expect_equal(nrow(calculate_network_boxplot_data(fits, list(edges = edges))), 0L)
})

test_that("the synthetic Methods target does not read generating files for empirical data", {
  pipeline <- tail(parse(file.path(report_saved_test_root, "_targets.R")), 1L)[[1L]]
  definitions <- as.list(pipeline)[-1L]
  target <- Filter(function(expr) is.call(expr) && identical(expr[[1]], as.name("tar_target")) &&
    identical(expr[[2]], as.name("report_synthetic_methods")), definitions)
  commands <- lapply(target, function(expr) expr[[3L]])
  expect_length(commands, 1L)
  fixture <- new.env(parent = baseenv())
  fixture$data_source_used <- "empirical"
  fixture$read_export_generating_values <- function(...) stop("generating values were read")
  fixture$read_generated_latent_scores <- function(...) stop("latent scores were read")
  fixture$build_synthetic_methods_reporting_data <- function(...) stop("synthetic calculations were run")
  expect_null(eval(commands[[1]], fixture))
})
