# Presentation of the approved paired-draw joint extensions; no model fitting.
local({
  root <- normalizePath(getwd())
  while (!file.exists(file.path(root, "config", "analysis_plan.yaml"))) {
    parent <- dirname(root)
    if (identical(parent, root)) stop("analysis root not found")
    root <- parent
  }
  source(file.path(root, "R", "config.R"), local = FALSE)
  source(file.path(root, "R", "report_helpers.R"), local = FALSE)
  source(file.path(root, "R", "report_joint_additions.R"), local = FALSE)
  # the report's labels: every scale from the codebook
  assign("joint_addition_labels",
         rh_labels(zm_codebook(zm_config("smoke", file.path(root, "config", "analysis_plan.yaml")))),
         envir = .GlobalEnv)
})

joint_addition_plan <- list(regression = list(ci_level = .95))
make_correlation_presentation_fixture <- function() {
  pairs <- utils::combn(c("asc_agg", "asc_sub", "asc_conv", "sdo_dom"), 2)
  rows <- data.frame(outcome_1 = pairs[1, ], outcome_2 = pairs[2, ])
  rows$unadjusted_median <- seq(.2, .7, .1)
  rows$unadjusted_lower <- rows$unadjusted_median - .1
  rows$unadjusted_upper <- rows$unadjusted_median + .1
  rows$residual_median <- c(.3, .2, .15, .4, .7, .4)
  rows$residual_lower <- rows$residual_median - .1
  rows$residual_upper <- rows$residual_median + .1
  rows$difference_median <- c(.1, -.1, -.25, -.1, .1, -.3)
  rows$difference_lower <- c(.02, -.2, -.4, -.199, -.02, -.4)
  rows$difference_upper <- c(.18, 0, -.1, -.001, .22, -.2)
  total <- rows[6, ]
  total$outcome_1 <- "asc_total"
  total$outcome_2 <- "sdo_dom"
  rows <- rbind(rows, total)
  rows$n <- 123L
  rows$interpretable <- TRUE
  list(summaries = rows, validity = list(fit_valid = TRUE, gate_status = "ok", note = ""),
       metadata = list(interval_level = .95))
}
make_aggregation_presentation_fixture <- function() {
  predictors <- c("zm_security", "zm_arousal", "zm_achievement", "zm_power", "zm_prestige")
  aggregate <- data.frame(predictor_key = predictors,
    b_aggregate_median = c(.2, -.2, 0, .1, .2),
    b_aggregate_lower = c(.1, -.3, -.1, -.02, .0004),
    b_aggregate_upper = c(.3, -.1, .1, .2, .4),
    gamma_aggregate_median = c(.1, -.1, 0, .05, .1),
    gamma_aggregate_lower = c(.05, -.15, -.05, -.01, .0002),
    gamma_aggregate_upper = c(.15, -.05, .05, .1, .2))
  comparisons <- data.frame(predictor_key = rep(predictors, each = 3),
    outcome_key = rep(c("asc_agg", "asc_sub", "asc_conv"), 5),
    gamma_facet_median = rep(c(-.4, .1, -.02), 5),
    gamma_facet_lower = rep(c(-.6, .01, -.1), 5),
    gamma_facet_upper = rep(c(-.2, .2, .06), 5),
    attenuation_median = rep(c(.3, -.1, .0006), 5),
    attenuation_lower = rep(c(.1, -.3, .00004), 5),
    attenuation_upper = rep(c(.5, -.01, .001), 5))
  for (suffix in c("median", "lower", "upper")) {
    comparisons[[paste0("b_facet_", suffix)]] <- comparisons[[paste0("gamma_facet_", suffix)]]
    comparisons[[paste0("b_aggregate_", suffix)]] <- rep(aggregate[[paste0("b_aggregate_", suffix)]], each = 3)
  }
  comparisons$direction_comparison <- rep(c("sign flip", "same direction", "unresolved direction comparison"), 5)
  list(aggregate_coefficients = aggregate, facet_comparisons = comparisons,
       validity = list(fit_valid = TRUE, gate_status = "ok", note = ""),
       interpretation_allowed = TRUE, metadata = list(interval_level = .95))
}

test_that("paired correlation table preserves all pairs, interval direction and precision", {
  fixture <- make_correlation_presentation_fixture()
  tab <- build_joint_correlation_changes_table(fixture, labels = joint_addition_labels,
                                               analysis_plan = joint_addition_plan, engine = "gt")
  rows <- tab[["_data"]]
  # the comparator is the zero-order correlation from the joint model
  expect_identical(names(rows), c("pair", "zero_order", "residual", "difference"))
  expect_identical(as.character(tab[["_boxhead"]]$column_label[[2]]), "Zero-order (from the joint model)")
  expect_equal(nrow(rows), 7L)
  expect_identical(rows$pair[7], "ASC total – SDO-D")
  expect_identical(rows$pair[4], "ASC submission – Conventionalism")
  # bold estimate: its interval excludes zero, and the joint fit passed its gate
  expect_identical(rows$difference[4], "**−.100** [−.199, −.001]")
  expect_identical(rows$zero_order[1], "**.20** [.10, .30]")
  text <- describe_joint_correlation_changes(fixture, joint_addition_labels)
  # inside a sentence each pair is named in the inline form
  expect_match(text, "It was lower, the interval lying below zero, for ASC aggression – SDO-D; ASC submission – conventionalism", fixed = TRUE)
  expect_match(text, "The interval of their difference included zero for ASC aggression – conventionalism", fixed = TRUE)
  expect_match(text, "higher than the zero-order correlation from the joint model", fixed = TRUE)
  expect_false(grepl("unadjusted|minus|vanish|absent|stronger|weaker", text))
  expect_match(paste(unlist(tab[["_source_notes"]]), collapse = " "), "95% CrI", fixed = TRUE)
})

test_that("near-zero bounds stay visibly nonzero and unavailable estimates are explicit", {
  fixture <- make_correlation_presentation_fixture()
  fixture$summaries$difference_upper[4] <- -.00004
  fixture$summaries$residual_median[2] <- NA_real_
  tab <- build_joint_correlation_changes_table(fixture, analysis_plan = joint_addition_plan, engine = "gt")
  expect_identical(tab[["_data"]]$difference[4], "**−.10000** [−.19900, −.00004]")
  expect_identical(tab[["_data"]]$residual[2], "—")
  fixture$summaries$difference_median[1] <- -.00004
  fixture$summaries$difference_lower[1] <- -.199
  fixture$summaries$difference_upper[1] <- .00004
  expect_identical(format_joint_addition_interval(fixture$summaries, "difference", digits = 3L)[1],
                   "0.000 [−0.199, 0.000]")
  # a zero-spanning bound that rounds to zero keeps its sign
  fixture$summaries$difference_median[1] <- .02
  fixture$summaries$difference_lower[1] <- -.004
  fixture$summaries$difference_upper[1] <- .05
  expect_identical(format_joint_addition_interval(fixture$summaries, "difference", bounded = TRUE)[1],
                   ".02 [−.00, .05]")
  fixture$metadata$interval_level <- .9
  expect_identical(label_joint_addition_interval(fixture, joint_addition_plan), "90% CrI")
  fixture$metadata$interval_level <- NULL
  expect_identical(label_joint_addition_interval(fixture, joint_addition_plan), "95% CrI")
})

test_that("aggregate tables show five aggregate and fifteen paired facet summaries", {
  fixture <- make_aggregation_presentation_fixture()
  aggregate <- build_joint_asc_aggregate_table(fixture, labels = joint_addition_labels,
                                               analysis_plan = joint_addition_plan, engine = "gt")
  facets <- build_joint_asc_attenuation_table(fixture, labels = joint_addition_labels,
                                              analysis_plan = joint_addition_plan, engine = "gt")
  expect_identical(names(aggregate[["_data"]]), c("predictor", "standardised"))
  expect_equal(nrow(aggregate[["_data"]]), 5L)
  expect_identical(aggregate[["_data"]]$standardised[5], "**0.2000** [0.0004, 0.4000]")
  expect_identical(names(facets[["_data"]]), c("predictor", "facet", "facet_coefficient", "total_coefficient", "attenuation", "direction"))
  expect_equal(nrow(facets[["_data"]]), 15L)
  expect_identical(facets[["_data"]]$predictor[1:4], c("Security", "", "", "Arousal"))
  expect_identical(facets[["_data"]]$facet_coefficient[1], "−0.40 [−0.60, −0.20]")
  expect_identical(facets[["_data"]]$attenuation[3], "0.00060 [0.00004, 0.00100]")
  labels <- c(aggregate[["_boxhead"]]$column_label, facets[["_boxhead"]]$column_label)
  expect_false(any(grepl("_|Verdict|^N$", unlist(labels))))
  note <- paste(unlist(facets[["_source_notes"]]), collapse = " ")
  expect_match(gsub("[*]", "", note), "respective outcome SD per predictor SD", fixed = TRUE)
  expect_match(note, "within each draw", fixed = TRUE)
  text <- describe_joint_asc_aggregation(fixture, joint_addition_labels)
  # the RQ2c total is the "ASC total"
  expect_match(text, "five wholly below zero (larger ASC-total magnitude)", fixed = TRUE)
  expect_match(text, "The interval of the ASC-total coefficient included zero for achievement, power", fixed = TRUE)
  expect_false(grepl("aggregate", text, fixed = TRUE))
  expect_false(grepl("absence|absent|vanish|sign reversal", text))
  expect_match(text, "opposite credible directions (a sign flip) in five", fixed = TRUE)
})

test_that("an invalid joint gate withholds interpretation and retains diagnostic numbers", {
  correlations <- make_correlation_presentation_fixture()
  aggregation <- make_aggregation_presentation_fixture()
  correlations$validity$fit_valid <- FALSE
  aggregation$validity$fit_valid <- FALSE
  expect_false(check_joint_addition_interpretability(correlations, correlations$summaries))
  expect_match(describe_joint_correlation_changes(correlations), "interpretation is withheld", fixed = TRUE)
  expect_match(describe_joint_asc_aggregation(aggregation), "interpretation is withheld", fixed = TRUE)
  for (tab in list(
    build_joint_correlation_changes_table(correlations, analysis_plan = joint_addition_plan, engine = "gt"),
    build_joint_asc_aggregate_table(aggregation, analysis_plan = joint_addition_plan, engine = "gt"),
    build_joint_asc_attenuation_table(aggregation, analysis_plan = joint_addition_plan, engine = "gt")
  )) {
    expect_gt(nrow(tab[["_data"]]), 0L)
    expect_match(paste(unlist(tab[["_source_notes"]]), collapse = " "), "Diagnostic estimates only", fixed = TRUE)
  }
  correlations$validity$fit_valid <- TRUE
  correlations$summaries$interpretable[1] <- FALSE
  expect_match(describe_joint_correlation_changes(correlations), "interpretation is withheld", fixed = TRUE)
  aggregation$validity$fit_valid <- TRUE
  aggregation$interpretation_allowed <- FALSE
  expect_match(describe_joint_asc_aggregation(aggregation), "interpretation is withheld", fixed = TRUE)
})
