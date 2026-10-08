# Focused tests for the unfilled participation-percentage reporting slot.

local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) {
    parent <- dirname(dir)
    if (identical(parent, dir)) stop("project root not found from ", getwd())
    dir <- parent
  }
  for (f in c("config.R", "report_helpers.R", "report_participation.R")) {
    source(file.path(dir, "R", f), local = FALSE)
  }
})

test_that("without the number of invitations the helper stops; the report prints the preregistration's sentence", {
  expect_error(
    rh_participation_text(750L, list(recruitment = list(invited_n = NULL))),
    "number of invited panel members"
  )
})

test_that("a later denominator fills the same sentence", {
  text <- rh_participation_text(750L, list(recruitment = list(invited_n = 1000L)))
  # APA sets the percent sign closed up.
  expect_match(text, "75.0%", fixed = TRUE)
  expect_match(text, "750 recorded responses from 1,000 invitations", fixed = TRUE)
  expect_error(
    rh_participation_text(1001L, list(recruitment = list(invited_n = 1000L))),
    "recorded responses <= invited"
  )
})


test_that("saved participation facts retain the exact sentence and unavailable denominator", {
  plan <- list(recruitment = list(invited_n = 1000L))
  facts <- calculate_participation_reporting_data(750L, plan)
  expect_identical(facts, list(available = TRUE, n_started = 750, invited = 1000, proportion = .75))
  expect_identical(rh_participation_text(750L, plan, participation = facts),
    "The participation percentage was 75.0% (750 recorded responses from 1,000 invitations).")
  expect_identical(calculate_participation_reporting_data(750L, list(recruitment = list(invited_n = NULL))),
                   list(available = FALSE))
  expect_identical(calculate_participation_reporting_data(750L, list(recruitment = list(invited_n = NA_integer_))),
                   list(available = FALSE))
})
