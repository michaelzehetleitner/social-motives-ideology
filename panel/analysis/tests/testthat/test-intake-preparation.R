# These tests use only the repository synthetic responses and mock every fill.
local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) dir <- dirname(dir)
  for (file in list.files(file.path(dir, "R"), pattern = "[.]R$", full.names = TRUE))
    source(file, local = FALSE)
})

test_that("the observed demographic side file preserves margins and breaks row linkage", {
  observed <- tibble::tibble(age = c(NA, 25:43), income = seq(500, 2400, 100),
    education = factor(rep(c("A", "B"), 10)), east_west = factor(rep(c("east", "west"), 10)),
    Duration__in_seconds_ = seq(100, 2000, 100))
  result <- withr::with_seed(91, build_deidentified_demographics(observed))
  expect_identical(names(result), c("age", "income", "education", "east_west", "duration"))
  for (i in seq_along(result)) {
    expect_equal(sort(result[[i]], na.last = TRUE), sort(observed[[i]], na.last = TRUE))
    expect_false(identical(result[[i]], observed[[i]]))
  }
  expect_false(identical(match(result$income, observed$income),
                         match(result$duration, observed$Duration__in_seconds_)))
  expect_equal(sum(is.na(result$age)), 1L)
})

test_that("intake runs existing fills on raw values and bands only afterwards", {
  cfg <- zm_config(profile = "full")
  book <- zm_codebook(cfg)
  raw <- read_qualtrics_export(file.path(cfg$root, "data/synthetic/zm_panel_synthetic.sav"))
  raw <- raw[raw$survey_status == "complete" & !is.na(raw$demo_gender) & raw$demo_gender != 3, ]
  needed <- c(unique(unlist(book$scales$item_codes)), "demo_age", "demo_hh_members", "demo_income_hh_net")
  raw <- raw[stats::complete.cases(raw[needed]), ][1:8, ]
  raw$demo_age[1] <- NA_real_
  item <- book$scales$item_codes[[1]][1]
  raw[[item]][2] <- NA_real_
  raw$respondent_id <- seq_len(nrow(raw))
  observed_age <- raw$demo_age
  calls <- character()
  env <- new.env(parent = environment(prepare_analysis_at_intake))
  env$impute_items <- function(data, codebook, analysis_plan) {
    force(data)
    calls <<- c(calls, "items")
    expect_false("age_band" %in% names(data))
    data[[item]][2] <- 3.25
    attr(data, "imputed_cells") <- ap3_fill_empty_cells()
    attr(data, "imputation_model_status") <- create_empty_imputation_model_status()
    data
  }
  env$impute_age <- function(data, codebook, analysis_plan, predictors) {
    force(data)
    calls <<- c(calls, "age")
    expect_true(all(codebook$scales$scale_key %in% predictors))
    expect_true(is.na(data$demo_age[1]))
    data$demo_age[1] <- 38.2
    data
  }
  env$impute_household_size <- function(data, codebook, analysis_plan, predictors) {
    force(data)
    calls <<- c(calls, "household_size")
    expect_equal(data$demo_age[1], 38.2)
    expect_true("demo_age" %in% predictors)
    data
  }
  env$impute_income_band <- function(data, codebook, analysis_plan, predictors) {
    force(data)
    calls <<- c(calls, "income")
    expect_true("demo_hh_members" %in% predictors)
    expect_false("age_band" %in% names(data))
    data
  }
  prepare <- prepare_analysis_at_intake
  environment(prepare) <- env
  result <- prepare(raw, book, cfg)
  expect_identical(calls, c("items", "age", "household_size", "income"))
  expect_equal(result$observed$age, observed_age)
  expect_equal(result$data$demo_age[1], 38.2)
  expect_equal(result$data$age_band_midpoint[1], 37)
  expect_equal(result$data[[item]][2], 3.25)
  expect_true(all(book$scales$scale_key %in% names(result$data)))
  expect_equal(nrow(result$imputation$excluded_participants), 0L)
})


test_that("the registered divers threshold runs once before AP3 exclusions", {
  cfg <- zm_config(profile = "full"); book <- zm_codebook(cfg)
  raw <- read_qualtrics_export(file.path(cfg$root, "data/synthetic/zm_panel_synthetic.sav"))
  items <- unique(unlist(book$scales$item_codes))
  required <- c(items, "demo_age", "demo_hh_members", "demo_income_hh_net")
  raw <- raw[raw$survey_status == "complete" & stats::complete.cases(raw[required]), ][1:40, ]
  raw$respondent_id <- seq_len(nrow(raw))
  raw$demo_gender <- c(rep(3, 30), rep(1, 10))
  raw[1, book$scales$item_codes[[1]][1:3]] <- NA_real_
  eligible <- apply_study_exclusions(raw, cfg)
  expect_equal(sum(eligible$data$demo_gender == 3), 30L)
  expect_true(tail(eligible$log$divers_kept, 1))
  prepared <- prepare_analysis_at_intake(raw, book, cfg)
  shared <- ap3_scientific_use_data(prepared$data, cfg, book)
  expect_equal(sum(shared$demo_gender == 3), 29L)
  expect_equal(nrow(shared), 39L)
  expect_identical(shared$respondent_id, prepared$data$respondent_id)
  expect_equal(nrow(prepared$imputation$excluded_participants), 1L)
})
