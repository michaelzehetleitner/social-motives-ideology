local({
  root <- normalizePath(getwd())
  while (!file.exists(file.path(root, "config", "analysis_plan.yaml"))) {
    parent <- dirname(root)
    if (identical(root, parent)) stop("analysis root not found")
    root <- parent
  }
  for (file in c("config.R", "ap8_network_helpers.R", "report_helpers.R", "report_supplement_completeness.R")) {
    source(file.path(root, "R", file), local = FALSE)
  }
  assign("supplement_completeness_plan", zm_config("smoke", file.path(root, "config", "analysis_plan.yaml")),
         envir = .GlobalEnv)
})

test_that("saved diagnostics retain exact values and exclude unfitted Student-t placeholders", {
  diagnostic <- tibble::tibble(ess_bulk_min = 3456.789, ess_tail_min = 2345.678,
    rhat_max = 1.00234, n_divergent = 0L, n_treedepth_hits = 0L, bfmi_min = .87654,
    mcse_median_max = .001234, mcse_q_max = .003456, ok = TRUE)
  joint <- structure(list(), diagnostics = diagnostic, gate_status = "retried_ok")
  student <- diagnostic
  student$ess_bulk_min <- 100.25
  student$ok <- FALSE
  records <- list(
    list(role = "student_refit", fit_available = TRUE, outcome_key = "asc_agg",
         diagnostics = student, gate_status = "not_interpretable"),
    list(role = "student_refit", fit_available = FALSE, outcome_key = "asc_sub",
         diagnostics = NULL, gate_status = "unavailable"),
    list(role = "primary", fit_available = TRUE, outcome_key = "asc_conv", diagnostics = diagnostic)
  )
  rows <- collect_saved_additional_diagnostics(records, list(joint = joint))
  expect_equal(nrow(rows), 2L)
  for (field in setdiff(names(diagnostic), "ok")) {
    expect_equal(rows[[field]], c(diagnostic[[field]], student[[field]]))
  }
  expect_identical(attr(rows, "student_not_fitted"), "asc_sub")
  # one slim table for every fitted model: the joint
  # model and the refits beside the fits of RQ1, a column only where values differ
  rq1 <- tibble::tibble(outcome = "asc_agg", slope_sd = .2, role = "primary", family = "gaussian",
                        n_obs = 699, iter_used = 1000, ess_bulk_min = 5000, ess_tail_min = 2500,
                        rhat_max = 1.002, n_divergent = 0L, n_treedepth_hits = 0L, bfmi_min = 1.04,
                        ok = TRUE, gate_status = "ok")
  labels <- rh_labels(zm_codebook(supplement_completeness_plan))
  table <- rh_diagnostics_table(rq1, supplement_completeness_plan, labels, additional = rows, engine = "gt")
  shown <- table[["_data"]]
  expect_identical(shown$model[2:3], c("ASC aggression, refit allowing heavier tails (Student-t)",
                                       "Joint regression of the four facets (RQ2)"))
  expect_identical(shown$gate, c("passed", "not passed", "passed after one retry"))
  # BFMI can exceed one and keeps its leading zero
  expect_identical(shown$bfmi_min, c("1.04", "0.88", "0.88"))
  expect_false(any(c("n_obs", "iter_used", "n_divergent", "n_treedepth_hits") %in% names(shown)))
  expect_match(rh_diagnostics_constant_text(rq1, supplement_completeness_plan, labels, rows),
               "no divergent transition and no transition at the maximum tree depth", fixed = TRUE)
  missing <- collect_saved_additional_diagnostics(list(), list(joint = NULL))
  expect_true(all(is.na(missing$ess_bulk_min)))
  expect_identical(rh_diagnostics_rows(rq1, supplement_completeness_plan, labels, missing)$gate[2], "not recorded")
})

test_that("the joint model of the five motives joins the gate table before the joint model of RQ2", {
  diagnostic <- tibble::tibble(ess_bulk_min = 3626, ess_tail_min = 2593, rhat_max = 1.002, n_divergent = 0L,
    n_treedepth_hits = 0L, bfmi_min = .97, mcse_median_max = .001, mcse_q_max = .005, ok = TRUE)
  joint <- structure(list(), diagnostics = diagnostic, gate_status = "ok")
  motives <- structure(list(), diagnostics = diagnostic, gate_status = "ok")
  rows <- collect_saved_additional_diagnostics(list(), list(joint = joint), list(motives = motives))
  expect_identical(rows$kind, c("joint", "motives"))
  expect_identical(rows$gate_passed, c(TRUE, TRUE))
  rq1 <- tibble::tibble(outcome = "asc_agg", slope_sd = .2, role = "primary", family = "gaussian",
                        n_obs = 699, iter_used = 1000, ess_bulk_min = 5000, ess_tail_min = 2500,
                        rhat_max = 1.002, n_divergent = 0L, n_treedepth_hits = 0L, bfmi_min = 1.04,
                        ok = TRUE, gate_status = "ok")
  labels <- rh_labels(zm_codebook(supplement_completeness_plan))
  table <- rh_diagnostics_table(rq1, supplement_completeness_plan, labels, additional = rows, engine = "gt")
  expect_identical(table[["_data"]]$model[2:3], c("Joint model of the five motives (RQ1)",
                                                  "Joint regression of the four facets (RQ2)"))
  note <- paste(unlist(table[["_source_notes"]]), collapse = " ")
  expect_match(note, "the residual correlations, from which the partial correlations come, are included", fixed = TRUE)
  # without the motive model the note says nothing about it
  without <- rh_diagnostics_table(rq1, supplement_completeness_plan, labels,
                                  additional = rows[rows$kind %in% "joint", ], engine = "gt")
  expect_false(grepl("five motives", paste(unlist(without[["_source_notes"]]), collapse = " "), fixed = TRUE))
})

test_that("Student-t R2 uses only saved summaries and the correct residual variance", {
  r2 <- tibble::tibble(
    outcome_key = c("asc_agg", "asc_agg", "asc_sub", "asc_sub"),
    role = c("primary", "student_refit", "primary", "student_refit"),
    family = c("gaussian", "student", "gaussian", "student"), fit_available = c(TRUE, TRUE, TRUE, FALSE),
    r2_median = c(.30, .25, .10, NA), r2_lo = c(.20, .15, .05, NA), r2_hi = c(.40, .35, .15, NA),
    nu_fixed = c(NA, 4, NA, 4)
  )
  # one refit: a sentence, not a one-row table
  expect_null(build_saved_student_r2_table(r2, supplement_completeness_plan, engine = "gt"))
  one <- describe_saved_student_r2(r2, rh_labels())
  expect_match(one, ".25 [.15, .35]", fixed = TRUE)
  expect_match(one, ".30 [.20, .40]", fixed = TRUE)
  expect_match(one, "with ν fixed at 4", fixed = TRUE)
  # two refits: the table, with plain heads
  r2$fit_available[4] <- TRUE
  r2$r2_median[4] <- .08
  r2$r2_lo[4] <- .03
  r2$r2_hi[4] <- .13
  table <- build_saved_student_r2_table(r2, supplement_completeness_plan, engine = "gt")
  expect_equal(nrow(table[["_data"]]), 2L)
  expect_identical(table[["_data"]]$gaussian[1], ".30 [.20, .40]")
  expect_identical(table[["_data"]]$student[1], ".25 [.15, .35]")
  heads <- unlist(lapply(table[["_boxhead"]]$column_label, as.character))
  expect_true(any(grepl("Normal model", heads, fixed = TRUE)))
  expect_true(any(grepl("Model allowing heavier tails", heads, fixed = TRUE)))
  note <- paste(unlist(table[["_source_notes"]]), collapse = " ")
  expect_match(note, "[ν / (ν − 2)]σ²", fixed = TRUE)
  expect_match(note, "scale parameter", fixed = TRUE)
  expect_match(note, "variance multiplier is 2.00", fixed = TRUE)
  r2$nu_fixed[c(2, 4)] <- 6
  changed <- build_saved_student_r2_table(r2, supplement_completeness_plan, engine = "gt")
  expect_match(paste(unlist(changed[["_source_notes"]]), collapse = " "), "variance multiplier is 1.50", fixed = TRUE)
  r2$fit_available[r2$role == "student_refit"] <- FALSE
  expect_null(build_saved_student_r2_table(r2, supplement_completeness_plan, engine = "gt"))
  expect_match(describe_saved_student_r2(r2, rh_labels()), "No refit allowing heavier tails was fitted", fixed = TRUE)
})

test_that("the complete network prior-sweep display retains each stored row and prior-specific BF bound", {
  sweep <- tibble::tibble(
    node_i = rep("zm_power", 3), node_j = rep("zm_prestige", 3), prior = c(.25, .50, .75),
    pip = c(1, .8, 0), bf = c(Inf, 4, 0), decision = c("present", "inconclusive", "absent"),
    stable = FALSE, decision_prereg = "inconclusive"
  )
  attr(sweep, "n_sweeps") <- 1000L
  table <- build_saved_network_prior_sweep_table(sweep, rh_labels(zm_codebook(supplement_completeness_plan)),
                                                 engine = "gt")
  shown <- table[["_data"]]
  # one row per edge, one column per prior: each cell
  # the decision of the full-data network with its Bayes factor at that prior
  expect_equal(nrow(shown), 1L)
  expect_identical(c(shown$prior_1, shown$prior_2, shown$prior_3),
                   c(paste0("present (", rh_network_bf(Inf, 1000L, .25), ")"), "inconclusive (4.00)",
                     paste0("absent (", rh_network_bf(0, 1000L, .75), ")")))
  heads <- unlist(lapply(table[["_boxhead"]]$column_label, as.character))
  expect_true(any(grepl("Prior inclusion probability .25", heads, fixed = TRUE)))
  # the bagged decision is not repeated: the network tables of the Results hold it
  expect_false(any(c("bagged", "stable") %in% names(shown)))
  expect_false(any(grepl("zm_|asc_|sdo_", shown$edge)))
})
