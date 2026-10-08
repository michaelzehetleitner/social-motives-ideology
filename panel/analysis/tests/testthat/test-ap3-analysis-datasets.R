# The common study carries the two analysis populations. These checks cover the
# selectors of the three analysis inputs and the guard of the known-gender
# population; no sampler runs here.
local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) {
    parent <- dirname(dir)
    if (identical(parent, dir)) stop("project root not found from ", getwd())
    dir <- parent
  }
  for (f in c("config.R", "io_qualtrics.R", "ap1_exclusions.R",
              "ap3_preprocessing.R", "ap3_imputation_validity.R", "ap3_preparation.R")) {
    source(file.path(dir, "R", f), local = FALSE)
  }
})

dataset_root <- zm_root()
dataset_cfg <- zm_config("full", file.path(dataset_root, "config", "analysis_plan.yaml"))
dataset_codebook <- zm_codebook(dataset_cfg)
dataset_items <- unique(unlist(dataset_codebook$scales$item_codes, use.names = FALSE))

# The hand-controlled export records one household of 10 persons. The registered
# household-size codes stop at analysis_plan$income$hh_size_top_value, so that value is
# outside the set check_covariate_input_codes() guards; the income rule caps at
# the same value, so reading the code as the top code leaves every derived
# income untouched.
dataset_registered_codes <- function(data) {
  data$demo_hh_members <- pmin(data$demo_hh_members, dataset_cfg$income$hh_size_top_value)
  data
}

# The frame chain of the pipeline's preparation route, without the imputation:
# reverse scoring, the gender coding, the reference the retained population
# chooses, the preregistered covariates, the scale scores.
dataset_study <- function() {
  readRDS(file.path(dataset_root, "tests", "fixtures", "hand-controlled-input.rds")) |>
    dataset_registered_codes() |>
    reverse_items(dataset_codebook, dataset_cfg) |>
    prepare_gender(dataset_cfg) |>
    derive_east_west(dataset_cfg) |>
    set_gender_reference() |>
    derive_covariates(dataset_cfg, dataset_codebook) |>
    average_items_into_subscales(dataset_codebook)
}

# Standardisation and the three selectors receive the already imputed study. The
# fixture is deliberately allowed to contain gaps, so give these checks a finite
# stand-in without invoking any fill model.
dataset_filled_study <- function() {
  study <- dataset_study()
  sources <- unique(c(
    dataset_codebook$scales$scale_key,
    "age", "income"
  ))
  for (source in sources) {
    x <- study[[source]]
    study[[source]][is.na(x)] <- mean(x, na.rm = TRUE)
  }
  add_age_and_income_bands(study, dataset_cfg)
}

test_that("selectors only choose columns and restore canonical model names", {
  common <- dataset_filled_study() |>
    standardise_all(dataset_codebook, dataset_cfg) |>
    standardise_known_gender(dataset_codebook, dataset_cfg)
  descriptive <- select_descriptive_reliability_input(common, dataset_codebook)
  network <- select_network_input(common, dataset_codebook, dataset_cfg)
  regressions <- common |>
    drop_rows_without_gender() |>
    select_regression_input(dataset_codebook, dataset_cfg)
  network_spec <- ap3_model_spec(dataset_cfg, dataset_codebook)$network
  regression_vars <- ap3_regression_z_variables(dataset_cfg, dataset_codebook)

  # The descriptive, reliability and factor-analysis input is the item level plus
  # the raw scale scores and the reported covariates; neither physical z family
  # belongs to it, and the rows without a gender stay.
  expect_setequal(names(descriptive), c(
    "respondent_id", dataset_items, dataset_codebook$scales$scale_key,
    "gender", "quota_group"))
  expect_equal(nrow(descriptive), nrow(common))
  expect_false(any(c(
    zm_z_col(network_spec$z_vars, dataset_codebook, population = "all"),
    zm_z_col(regression_vars, dataset_codebook, population = "known_gender")
  ) %in% names(descriptive)))

  expect_identical(names(network), c("respondent_id", network_spec$vars))
  expect_identical(attr(network, "z_parameters", exact = TRUE)$z_col,
                   zm_z_col(attr(common, "z_parameters_all", exact = TRUE)$var, dataset_codebook))
  expect_true(all(vapply(network[network_spec$vars], function(x) all(is.finite(x)), logical(1))))

  canonical_regression <- zm_z_col(regression_vars, dataset_codebook)
  expect_identical(names(regressions), c("respondent_id", "gender", canonical_regression))
  expect_false(anyNA(regressions$gender))
  expect_identical(attr(regressions, "regression_participants", exact = TRUE),
                   regressions$respondent_id)
  expect_identical(attr(regressions, "regression_n_input", exact = TRUE), nrow(common))
  # Gender is never imputed, so the rows without it leave the regressions here
  # and are named, not silently dropped.
  expect_identical(attr(regressions, "omitted_gender", exact = TRUE)$respondent_id,
                   common$respondent_id[is.na(common$gender)])
  expect_identical(attr(regressions, "z_parameters", exact = TRUE)$z_col,
                   zm_z_col(attr(common, "z_parameters_known_gender", exact = TRUE)$var,
                            dataset_codebook))
  expect_true(all(vapply(regressions[canonical_regression], function(x) all(is.finite(x)), logical(1))))
})

test_that("the known-gender population is guarded where it is built, not where it is selected", {
  study <- dataset_filled_study()
  study$gender[] <- NA
  expect_error(standardise_known_gender(study, dataset_codebook, dataset_cfg),
               "Fewer than two known-gender rows.", fixed = TRUE)
  # Standardisation precedes selection, so no unguarded frame reaches the
  # selector. It therefore decides nothing itself: it records which respondents
  # the gender rule removed and leaves the population it was handed.
  emptied <- drop_rows_without_gender(study)
  expect_equal(nrow(emptied), 0L)
  expect_identical(attr(emptied, "omitted_gender", exact = TRUE)$respondent_id,
                   study$respondent_id)
  expect_identical(attr(emptied, "regression_n_input", exact = TRUE), nrow(study))
})
