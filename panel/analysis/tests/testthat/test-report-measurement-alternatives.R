local({
  root <- normalizePath(getwd())
  while (!file.exists(file.path(root, "config", "analysis_plan.yaml"))) {
    parent <- dirname(root)
    if (identical(root, parent)) stop("analysis root not found")
    root <- parent
  }
  source(file.path(root, "R", "report_measurement_alternatives.R"), local = FALSE)
})

make_alternative_correspondence_fixture <- function() {
  data.frame(
    set_key = c("asc", "asc", "asc", "motives", "motives", "outcomes", "outcomes"),
    factors = c(3, 1, 2, 5, 1, 4, 2),
    solution = c("expected count", "suggested by the criteria", "suggested by the criteria",
                 "expected count", "suggested by the criteria", "expected count", "suggested by the criteria"),
    expected_pair_retention = 1,
    empirical_pair_purity = c(1, .30, .55, 1, .17, 1, .47),
    n_items = c(19, 19, 19, 30, 30, 27, 27),
    n_weak = c(0, 4, 0, 0, 16, 0, 5),
    note = NA_character_
  )
}

test_that("pure merging describes solutions, not unique items", {
  fixture <- make_alternative_correspondence_fixture()
  result <- describe_alternative_factor_correspondence(fixture)
  expect_match(result, "All four lower-factor solutions merged whole scales without splitting their items.", fixed = TRUE)
  # the weakly loading items are described per solution by describe_loading_clarity(),
  # so this sentence does not count them
  expect_false(grepl("loading counts|weakly loading", result))
  expect_false(grepl("pass|fail|accept|reject|validated", result))
})

test_that("no alternatives and unavailable inputs are different results", {
  fixture <- make_alternative_correspondence_fixture()
  expect_identical(describe_alternative_factor_correspondence(fixture[fixture$solution == "expected count", ]),
                   "No alternative factor-count solution was recorded.")
  expect_identical(describe_alternative_factor_correspondence(fixture[FALSE, ]),
                   "Alternative-solution correspondence was unavailable.")
  expect_identical(describe_alternative_factor_correspondence(NULL),
                   "Alternative-solution correspondence was unavailable.")
})

test_that("splitting is not called whole-scale merging or lower dimensionality", {
  fixture <- make_alternative_correspondence_fixture()[1:2, ]
  fixture$factors[2] <- 4
  fixture$expected_pair_retention[2] <- .80
  fixture$empirical_pair_purity[2] <- 1
  fixture$n_weak[2] <- 0
  result <- describe_alternative_factor_correspondence(fixture)
  expect_match(result, "The higher-factor solution split scales without merging items from different scales.", fixed = TRUE)
  expect_false(grepl("lower-factor|merged whole", result))
})

test_that("mixed correspondence distinguishes merging splitting and their combination", {
  fixture <- make_alternative_correspondence_fixture()
  fixture$expected_pair_retention[c(3, 5)] <- c(.8, .6)
  fixture$empirical_pair_purity[3] <- 1
  fixture$empirical_pair_purity[7] <- 1
  result <- describe_alternative_factor_correspondence(fixture)
  expect_match(result, "one merged whole scales", fixed = TRUE)
  expect_match(result, "one split scales", fixed = TRUE)
  expect_match(result, "one combined splitting and merging", fixed = TRUE)
  expect_match(result, "one retained the theoretical item grouping", fixed = TRUE)
})

test_that("missing expected counts and correspondence are not read as agreement", {
  fixture <- make_alternative_correspondence_fixture()
  fixture <- fixture[-4, ]
  fixture$empirical_pair_purity[2] <- NA_real_
  result <- describe_alternative_factor_correspondence(fixture)
  expect_match(result, "alternative factor-count solutions", fixed = TRUE)
  expect_match(result, "two had unavailable or diagnostic-only correspondence", fixed = TRUE)
  expect_false(grepl("All four lower-factor", result))
})

test_that("explicit unsuccessful and diagnostic statuses override numerical values", {
  fixture <- make_alternative_correspondence_fixture()
  fixture$status <- "ok"
  fixture$status[2] <- "diagnostic-only"
  fixture$success <- TRUE
  fixture$success[3] <- FALSE
  fixture$note[5] <- "fit unavailable"
  fixture$diagnostic_only <- FALSE
  fixture$diagnostic_only[7] <- TRUE
  result <- describe_alternative_factor_correspondence(fixture)
  expect_match(result, "All four lower-factor solutions had unavailable or diagnostic-only correspondence.", fixed = TRUE)
  expect_false(grepl("merged whole|split scales", result))
})

test_that("missing weak-loading counts do not change the grouping sentence", {
  fixture <- make_alternative_correspondence_fixture()
  fixture$n_weak[c(2, 5)] <- c(NA, 31)
  result <- describe_alternative_factor_correspondence(fixture)
  expect_identical(result, "All four lower-factor solutions merged whole scales without splitting their items.")
})
