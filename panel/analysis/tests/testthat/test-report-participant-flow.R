# Focused tests for the data-driven participant-flow figure.

local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) {
    parent <- dirname(dir)
    if (identical(parent, dir)) stop("project root not found from ", getwd())
    dir <- parent
  }
  for (f in c("config.R", "report_helpers.R", "report_participant_flow.R")) {
    source(file.path(dir, "R", f), local = FALSE)
  }
})

flow_log <- tibble::tibble(
  criterion = c("not_consenting", "incomplete", "age_outside_range", "attention_1_wrong", "attention_2_wrong"),
  n_before = c(750L, 744L, 730L, 726L, 714L),
  n_excluded = c(6L, 14L, 4L, 12L, 8L),
  n_after = c(744L, 730L, 726L, 714L, 706L)
)

test_that("participant-flow counts reconcile and retain every exclusion reason", {
  flow <- rh_participant_flow_data(flow_log, 776L, 26L)

  expect_equal(flow$started, 776L)
  expect_equal(flow$quota_full, 26L)
  expect_equal(flow$admitted, 750L)
  expect_equal(flow$excluded, 44L)
  expect_equal(flow$analysed, 706L)
  expect_equal(flow$reasons$n, flow_log$n_excluded)
  expect_equal(flow$reasons$label[1], "No consent")
  expect_equal(flow$reasons$label[5], "Attention check 2 failed")
})

test_that("participant-flow plot is generated from the log rather than typed counts", {
  plot <- rh_participant_flow_plot(flow_log, 776L, 26L)

  expect_s3_class(plot, "ggplot")
  labels <- plot$layers[[2L]]$data$label
  expect_true(any(grepl("776", labels, fixed = TRUE)))
  expect_true(any(grepl("Quota cell full: 26", labels, fixed = TRUE)))
  expect_true(any(grepl("750", labels, fixed = TRUE)))
  expect_true(any(grepl("44", labels, fixed = TRUE)))
  expect_true(any(grepl("706", labels, fixed = TRUE)))
  expect_true(any(grepl("No consent: 6", labels, fixed = TRUE)))
})

# The AP3 fill appends its drop to the participant flow (report_exclusion_steps()).
drop_log <- function(n_dropped) {
  n_before <- flow_log$n_after[nrow(flow_log)]
  dplyr::bind_rows(
    flow_log,
    tibble::tibble(
      criterion = "dropped_unfillable_gaps",
      n_before = n_before,
      n_excluded = as.integer(n_dropped),
      n_after = n_before - as.integer(n_dropped)
    )
  )
}

test_that("the drop for unfillable gaps is a step of the flow whether or not it removed anyone", {
  none <- rh_participant_flow_data(drop_log(0L), 776L, 26L)
  expect_equal(none$analysed, 706L)
  expect_equal(none$reasons$label[6], "More than two items missing within any scale, or at least two of age, household size and income band")
  expect_equal(none$reasons$n[6], 0L)
  labels_none <- rh_participant_flow_plot(drop_log(0L), 776L, 26L)$layers[[2L]]$data$label
  expect_true(any(grepl("More than two items missing within any scale, or at least two of age, household size and income band: 0", labels_none, fixed = TRUE)))

  planted <- rh_participant_flow_data(drop_log(3L), 776L, 26L)
  expect_equal(planted$analysed, 703L)
  expect_equal(planted$excluded, 47L)
  expect_equal(planted$reasons$n[6], 3L)
  labels_planted <- rh_participant_flow_plot(drop_log(3L), 776L, 26L)$layers[[2L]]$data$label
  expect_true(any(grepl("More than two items missing within any scale, or at least two of age, household size and income band: 3", labels_planted, fixed = TRUE)))
  expect_true(any(grepl("703", labels_planted, fixed = TRUE)))
})

test_that("participant flow refuses an inconsistent log", {
  bad <- flow_log
  bad$n_after[nrow(bad)] <- bad$n_after[nrow(bad)] - 1L
  expect_error(rh_participant_flow_data(bad, 776L, 26L), "admitted minus exclusions")
  expect_error(rh_participant_flow_data(flow_log, 776L, 25L), "started minus quota-full")
})

test_that("the exclusion labels use the preregistration's wording with the configured numbers", {
  # The exclusion labels use the preregistration's words (AP1 and AP3); the
  # minimum age and the minimum number of gender-diverse responses come from
  # the configuration.
  plan <- list(exclusions = list(min_age = 18, max_age = 69, gender_divers_min_n = 30))
  codes <- c("age_outside_range", "gender_divers", "dropped_unfillable_gaps")
  expect_equal(
    rh_criterion_label(codes, plan),
    c("Age below 18 or above 69", "Gender-diverse responses below the minimum of 30",
      "More than two items missing within any scale, or at least two of age, household size and income band")
  )
  # without the configuration the rules are named without their numbers
  expect_false(any(grepl("[0-9]", rh_criterion_label(codes))))
  flow <- rh_participant_flow_data(drop_log(0L), 776L, 26L, plan)
  expect_true("Age below 18 or above 69" %in% flow$reasons$label)
})
