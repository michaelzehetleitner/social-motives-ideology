# Reader-facing display contracts: grouping, interpretable summaries, monochrome
# cues and histogram/data alignment. No model fitting is needed.
local({
  root <- normalizePath(getwd())
  while (!file.exists(file.path(root, "config", "analysis_plan.yaml"))) {
    parent <- dirname(root)
    if (identical(root, parent)) stop("analysis root not found")
    root <- parent
  }
  source(file.path(root, "R", "config.R"), local = FALSE)
  source(file.path(root, "R", "report_helpers.R"), local = FALSE)
  assign("display_layout_plan", zm_config("smoke", file.path(root, "config", "analysis_plan.yaml")),
         envir = .GlobalEnv)
})

test_that("consecutive grouping labels preserve later separate groups and missing entries", {
  expect_identical(rh_blank_repeated(c("A", "A", "B", "B", "A", NA_character_)),
                   c("A", "", "B", "", "A", NA_character_))
  expect_identical(rh_blank_repeated(character()), character())
})

test_that("posterior summaries retain every estimate and identify invalid fits in the note", {
  cells <- tibble::tibble(
    outcome_key = c("asc_agg", "asc_agg", "asc_sub"),
    predictor = c("zm_power", "zm_prestige", "zm_power"),
    estimate = c(.20, -.10, .01), interval_lower = c(.10, -.20, -.09),
    interval_upper = c(.30, -.01, .11), prior_robust = c(TRUE, FALSE, NA),
    fit_valid = c(TRUE, TRUE, FALSE)
  )
  labels <- rh_labels(zm_codebook(display_layout_plan))
  tab <- rh_prediction_cells_table(cells, labels = labels, analysis_plan = display_layout_plan, engine = "gt")
  # estimate and interval in one cell
  expect_identical(names(tab[["_data"]]), c("outcome", "motive", "estimate"))
  # the codebook's labels, a capital at the start of a cell
  expect_identical(tab[["_data"]]$outcome, c("ASC aggression", "", "ASC submission"))
  expect_identical(tab[["_data"]]$motive, c("Power", "Prestige", "Power"))
  # bold: the estimate, not its interval, where the interval excludes zero in a
  # fit that passed its diagnostics
  expect_identical(tab[["_data"]]$estimate, c("**0.20** [0.10, 0.30]", "**−0.10** [−0.20, −0.01]", "0.01 [−0.09, 0.11]"))
  note <- paste(unlist(tab[["_source_notes"]]), collapse = " ")
  expect_match(note, "Bold estimate: its interval excludes zero.", fixed = TRUE)
  # inside the note's sentences, the inline form
  expect_match(note, "The classification depends on the prior for prestige → ASC aggression.", fixed = TRUE)
  expect_match(note, "Not interpretable because sampling diagnostics did not pass: ASC submission", fixed = TRUE)
  expect_match(note, "prior comparison is unavailable", fixed = TRUE)
})

test_that("estimate-and-interval cells line up at the decimal point in HTML", {
  cells <- c("**0.24** [0.13, 0.35]", "−0.02 [−0.12, 0.09]", "—")
  out <- rh_align_interval_cells(cells)
  expect_identical(out[3], "—")
  # the same boxes in every cell, so each kind of number lines up
  widths <- regmatches(out[1:2], gregexpr("width:[0-9.]+ch;text-align:[a-z]+", out[1:2]))
  expect_identical(widths[[1]], widths[[2]])
  expect_length(widths[[1]], 6L)
  # the text is the plain estimate and interval, and only the estimate is bold
  expect_identical(gsub("<[^>]+>", "", out[1:2]), c("0.24 [0.13, 0.35]", "−0.02 [−0.12, 0.09]"))
  expect_match(out[1], "<strong>0</strong></span><span[^>]*><strong>\\.24</strong></span> \\[")
  expect_false(grepl("<strong>", sub("^.*\\[", "", out[1])))
  # a superscript after the estimate or the interval, and another number of decimals
  raised <- rh_align_interval_cells(c(".23<sup>†</sup> [.16, .30]", ".05 [−.004, .11]<sup>*</sup>"))
  expect_identical(gsub("<[^>]+>", "", raised), c(".23† [.16, .30]", ".05 [−.004, .11]*"))
  # a number with more decimals widens only its own decimal box
  wide <- rh_align_interval_cells(c(".01 [−.09, .07]", "−.1188 [−.2069, −.0001]", ".13 [.04, .21]"))
  common <- regmatches(wide, gregexpr("min-width:[0-9.]+ch", wide))
  expect_identical(common[[1]], common[[2]])
  expect_identical(common[[1]], common[[3]])
  expect_identical(unique(common[[1]]), "min-width:2.6ch")
  # a range without an estimate lines up the same way
  ranges <- rh_align_interval_cells(c("[−3.98, −2.57]", "[2.55, 4.05]"))
  expect_identical(gsub("<[^>]+>", "", ranges), c("[−3.98, −2.57]", "[2.55, 4.05]"))
  range_widths <- regmatches(ranges, gregexpr("width:[0-9.]+ch", ranges))
  expect_identical(range_widths[[1]], range_widths[[2]])
  expect_length(range_widths[[1]], 4L)
  # rh_table aligns such a column in HTML
  html <- as.character(gt::as_raw_html(rh_table(data.frame(x = cells[1:2]), engine = "gt"), inline_css = FALSE))
  expect_match(html, "display:inline-block", fixed = TRUE)
})

test_that("APA tables use three horizontal rules and no body or vertical rules", {
  tab <- rh_table(data.frame(label = "A", estimate = .25), engine = "gt")
  options <- tab[["_options"]]
  value <- function(name) options$value[[match(name, options$parameter)]]
  expect_identical(value("table_body_hlines_style"), "none")
  expect_identical(value("table_body_vlines_style"), "none")
  expect_identical(value("column_labels_border_top_style"), "solid")
  expect_identical(value("column_labels_border_bottom_style"), "solid")
  expect_identical(value("table_body_border_top_style"), "none")
  expect_identical(value("table_body_border_bottom_style"), "solid")
  expect_identical(value("row_striping_include_table_body"), FALSE)
})

test_that("histograms align with score rows and embed their own images", {
  old_options <- options(sass.cache = FALSE)
  on.exit(options(old_options), add = TRUE)
  tab <- rh_table(data.frame(scale = c("A", "B"), histogram = ""), engine = "gt")
  scores <- data.frame(a = c(1, 1, 2, 3), b = c(4, 5, 6, 6))
  old <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  on.exit(knitr::opts_knit$set(rmarkdown.pandoc.to = old), add = TRUE)
  knitr::opts_knit$set(rmarkdown.pandoc.to = "html")
  shown <- rh_add_histograms(tab, scores, c("a", "b"))
  html <- gt::as_raw_html(shown)
  expect_match(html, "data:image/png;base64,", fixed = TRUE)
  expect_error(rh_add_histograms(tab, scores, c("a")), "must match table rows")
  expect_error(rh_add_histograms(tab, scores, c("a", "missing")), "available score columns")
  knitr::opts_knit$set(rmarkdown.pandoc.to = "docx")
  expect_identical(rh_add_histograms(tab, scores, c("a", "b")), tab)
})

test_that("a single coefficient series is black and network node groups have shape cues", {
  scale <- rh_prior_scale(.2, .2, display_layout_plan)
  expect_identical(unname(scale$colours), "#000000")
  expect_identical(unname(scale$linetypes), "solid")
  nodes <- c("zm_power", "zm_prestige", "asc_agg")
  weights <- matrix(c(0, .2, -.2, .2, 0, .1, -.2, .1, 0), 3,
                    dimnames = list(nodes, nodes))
  decisions <- matrix("present", 3, 3, dimnames = list(nodes, nodes))
  plot <- rh_network_plot(list(nodes = nodes, weight = weights, decision = decisions),
                         groups = list(Motive = nodes[1:2], Outcome = nodes[3]))
  built <- ggplot2::ggplot_build(plot)
  expect_length(unique(built$data[[1]]$linetype), 2L)
  expect_length(unique(built$data[[2]]$shape), 2L)
  expect_setequal(plot$layers[[2]]$data$group, c("Motive", "Authoritarian facet"))
})
