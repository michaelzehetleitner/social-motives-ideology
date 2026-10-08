# Focused checks for the final report coverage additions; no model fitting.
local({
  root <- normalizePath(getwd())
  while (!file.exists(file.path(root, "config", "analysis_plan.yaml"))) {
    parent <- dirname(root)
    if (identical(root, parent)) stop("analysis root not found")
    root <- parent
  }
  for (file in c("config.R", "io_qualtrics.R", "ap8_network_helpers.R", "report_helpers.R")) {
    source(file.path(root, "R", file), local = FALSE)
  }
  assign("final_coverage_plan", zm_config("smoke", file.path(root, "config", "analysis_plan.yaml")),
         envir = .GlobalEnv)
})

test_that("the report labels every scale from the codebook only, raw and standardised scores alike", {
  codebook <- zm_codebook(final_coverage_plan)
  original <- codebook
  labels <- rh_labels(codebook)
  expect_identical(codebook, original)
  keys <- as.character(codebook$scales$scale_key)
  # each scale's `short_label`, its first letter capitalised for a cell, a
  # head or the start of a sentence; the codebook keeps its own case
  expect_identical(unname(labels[keys]), rh_capitalise_first_letter(codebook$scales$short_label))
  expect_identical(unname(labels[as.character(codebook$scales$z_col)]), unname(labels[keys]))
  expect_identical(unname(labels[c("zm_security", "zm_arousal", "zm_power", "zm_prestige", "zm_achievement")]),
                   c("Security", "Arousal", "Power", "Prestige", "Achievement"))
  expect_identical(rh_label_inline(c("zm_security", "zm_arousal", "zm_power", "asc_conv", "asc_agg", "sdo_dom"), labels),
                   c("security", "arousal", "power", "conventionalism", "ASC aggression", "SDO-D"))
  # the scales come first, in the codebook's order
  expect_identical(names(labels)[seq_along(keys)], keys)
  # the codebook is the only source of scale labels: without it a scale shows its key
  expect_false(any(c(keys, as.character(codebook$scales$z_col)) %in% names(rh_labels())))
  expect_identical(rh_label("zm_security", rh_labels()), "zm_security")
  # a changed codebook label is the report's label
  changed <- codebook
  changed$scales$short_label[changed$scales$scale_key == "zm_power"] <- "dominance motive"
  expect_identical(rh_label("zm_power", rh_labels(changed)), "Dominance motive")
  expect_identical(rh_label_inline("zm_power_z", rh_labels(changed)), "dominance motive")
  # every name the codebook gives a scale (label, short label, subscale), as
  # written there and as a cell or a sentence starts it, reaches the report
  # code and the report sources through the codebook: their code writes none
  # as a string
  root <- final_coverage_plan$root
  sources <- c(list.files(file.path(root, "R"), pattern = "^report_.*[.]R$", full.names = TRUE),
               file.path(root, "report", "results_draft.qmd"))
  scale_names <- unique(as.character(unlist(codebook$scales[c("label", "short_label", "subscale")])))
  scale_names <- unique(c(scale_names, rh_capitalise_first_letter(scale_names)))
  expect_true(all(c("Security", "security", "SDO-D", "Dominance", "Social neophilia") %in% scale_names))
  literals <- c(paste0('"', scale_names, '"'), paste0("'", scale_names, "'"))
  for (path in sources) {
    lines <- readLines(path, warn = FALSE)
    code <- paste(lines[!grepl("^\\s*#", lines)], collapse = "\n")
    for (literal in literals) {
      expect_false(grepl(literal, code, fixed = TRUE), info = paste(basename(path), literal))
    }
  }
})

test_that("the capitalisation helpers change only the first letter of an ordinary word", {
  expect_identical(rh_capitalise_first_letter(c("security", "ASC aggression", "", NA)),
                   c("Security", "ASC aggression", "", NA))
  expect_identical(rh_lowercase_first_letter(c("Security", "All social motive scales", "ASC", "SDO-D",
                                               "UMS-6 (intimacy and achievement)")),
                   c("security", "all social motive scales", "ASC", "SDO-D", "UMS-6 (intimacy and achievement)"))
})

test_that("partial and infeasible bags keep their actual denominators without repetitive complete-fit notes", {
  expect_identical(rh_network_resample_note(final_coverage_plan, 60L, 60L), "")
  partial <- rh_network_resample_note(final_coverage_plan, 59L, 60L)
  expect_match(partial, "averaged over the 59 successful fits of the 60 bootstrap resamples", fixed = TRUE)
  unavailable <- rh_network_resample_note(final_coverage_plan, 56L, 60L)
  expect_match(unavailable, "56 of 60", fixed = TRUE)
  expect_match(unavailable, "unavailable", fixed = TRUE)
  expect_match(unavailable, "95%", fixed = TRUE)
  expect_false(grepl("averaged", unavailable, fixed = TRUE))
})

test_that("every bagged edge shows its resolvable Bayes factor, and an invalid bag classifies nothing", {
  # The three network matrices show every bagged edge; the guarantees are
  # checked on the matrix builder.
  bagged <- tibble::tibble(node_i = c("zm_power", "zm_power", "zm_arousal"),
                           node_j = c("asc_agg", "asc_sub", "asc_conv"),
                           pip_bagged = c(1, 0, .5), bf_bagged = c(Inf, 0, 3), weight_bagged = c(.2, 0, .05),
                           decision = c("present", "absent", "inconclusive"))
  attr(bagged, "feasible") <- TRUE
  attr(bagged, "n_sweeps") <- 1000L
  attr(bagged, "n_success_fits") <- 60L
  attr(bagged, "g_prior") <- .25
  shown <- rh_network_matrix_table(bagged, rows = c("zm_arousal", "zm_power"), columns = c("asc_agg", "asc_sub", "asc_conv"),
                                   labels = rh_labels(zm_codebook(final_coverage_plan)), engine = "gt")
  cells <- shown[["_data"]]
  bf <- rh_network_bf(c(Inf, 0, 3), 1000L, .25, 60L)
  expect_true(startsWith(bf[1], "> "))
  expect_true(startsWith(bf[2], "< "))
  expect_identical(cells$column_1[2], paste0("**present, .20**<br>BF<sub>bagged</sub> ", bf[1]))
  expect_identical(cells$column_2[2], paste0("absent<br>BF<sub>bagged</sub> ", bf[2]))
  expect_identical(cells$column_3[1], paste0("inconclusive<br>BF<sub>bagged</sub> ", bf[3]))
  expect_match(cells$node[2], "Power", fixed = TRUE)
  # Deliberately retain stale-looking diagnostic values in an invalid object:
  # the display must honour the bag validity flag, not classify those numbers.
  attr(bagged, "feasible") <- FALSE
  invalid <- rh_network_matrix_table(bagged, rows = c("zm_arousal", "zm_power"), columns = c("asc_agg", "asc_sub", "asc_conv"),
                                     labels = rh_labels(zm_codebook(final_coverage_plan)), engine = "gt")[["_data"]]
  filled <- unlist(invalid[c("column_1", "column_2", "column_3")])
  filled <- filled[nzchar(filled)]
  expect_true(all(filled == "not classified<br>BF<sub>bagged</sub> —"))
})

test_that("model-check tables name refit roles while preserving their saved quantities", {
  summary <- tibble::tibble(
    outcome = c("asc_agg", "asc_agg"), family = c("gaussian", "student"),
    slope_sd = c(.2, .2), role = c("primary", "student_refit"),
    n_draws = c(4000L, 4000L), p95_abs_prediction = c(1.9, 2.1),
    q5 = c(1.6, 1.7), q50 = c(3.0, 3.0), q95 = c(4.4, 4.3),
    share_below_min = c(.015, .02), share_above_max = c(.005, .01), share_outside_range = c(.02, .03)
  )
  original <- summary
  shown <- rh_prior_pred_table(summary, analysis_plan = final_coverage_plan, engine = "gt")
  expect_identical(summary, original)
  # the model in words: the prior by its name and SD, the refit by its likelihood
  primary <- rh_sweep_label(.2, final_coverage_plan)
  expect_identical(shown[["_data"]]$model,
                   paste0(toupper(substr(primary, 1, 1)), substring(primary, 2), " prior (SD 0.20)",
                          c("", ", Student-t")))
  expect_identical(shown[["_data"]]$p95_abs, rh_fmt(c(1.9, 2.1)))
  # every configured percentile, the 50th included (Methods promises it)
  expect_identical(shown[["_data"]]$q50, rh_fmt(c(3.0, 3.0)))
  expect_identical(shown[["_data"]]$below, c("1.5%", "2.0%"))
  expect_identical(shown[["_data"]]$above, c("0.5%", "1.0%"))
  r2 <- tibble::tibble(outcome = "asc_agg", slope_sd = .2, r2_median = .2,
                       r2_lo = .1, r2_hi = .3, role = "student_refit", family = "student",
                       r2_definition = "var(mu) / (var(mu) + 2 * sigma^2)")
  shown_r2 <- rh_r2_table(r2, analysis_plan = final_coverage_plan, engine = "gt")
  # estimate and interval in one cell
  expect_identical(shown_r2[["_data"]]$r2, ".20 [.10, .30]")
  # one row keeps its columns; a column with one value in several rows leaves the table
  expect_true("family" %in% names(shown_r2[["_data"]]))
  two <- dplyr::bind_rows(r2, transform(r2, outcome = "asc_sub"))
  shown_two <- rh_r2_table(two, analysis_plan = final_coverage_plan, engine = "gt")
  expect_false(any(c("family", "slope_sd") %in% names(shown_two[["_data"]])))
  expect_match(paste(unlist(shown_r2[["_source_notes"]]), collapse = " "),
               "var(mu) / (var(mu) + 2 * sigma^2)", fixed = TRUE)
})
