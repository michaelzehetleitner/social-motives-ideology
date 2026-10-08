# The result contracts of the preparation targets: the full prepared study,
# the three selection-only analysis inputs, the reporting objects, and the
# tables the data files are written from.
# No sampler runs here: the four imputation verbs are the deterministic stubs
# of tests/support/target-reader.R.

local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) {
    parent <- dirname(dir)
    if (identical(parent, dir)) stop("project root not found from ", getwd())
    dir <- parent
  }
  for (f in c("config.R", "io_qualtrics.R", "ap1_exclusions.R", "ap3_preprocessing.R",
              "ap3_imputation_validity.R", "ap3_preparation.R", "ap3_fill.R", "ap3_data_files.R", "ap3_pipeline.R")) {
    source(file.path(dir, "R", f), local = FALSE)
  }
  source(file.path(dir, "tests", "support", "targets-route.R"), local = FALSE)
})

pc_root <- zm_root()
pc_cfg <- zm_config("full", file.path(pc_root, "config", "analysis_plan.yaml"))
pc_book <- zm_codebook(pc_cfg)
pc_items <- unique(unlist(pc_book$scales$item_codes, use.names = FALSE))
pc_raw <- read_qualtrics_export(file.path(pc_root, "data", "synthetic", "zm_panel_synthetic.sav"))
pc_route <- tr_route(pc_root, tr_intake(pc_raw, pc_cfg), pc_cfg, pc_book)

# ---- The full study, the selection-only inputs and the file tables ----------

test_that("study_analysis_ready is the all-retained full study, not a selection", {
  prepared <- pc_route("study_analysis_ready")
  records <- pc_route("imputation_reporting_data")
  expect_identical(prepared$respondent_id, records$participants$respondent_id)
  expect_identical(prepared$known_gender, records$participants$known_gender)
  expect_true(all(c("demo_gender", "pol_symp_spd", "quota_group", "age_band", "age_band_midpoint",
                    "income_band", "income_band_value", "income_band_value_log", pc_items,
                    pc_book$scales$scale_key) %in% names(prepared)))
  expect_false(any(c("demo_age", "demo_hh_members", "demo_income_hh_net", "income", "age",
                     "education", "east_west", "Kommentarfeld") %in% names(prepared)))
  all <- attr(prepared, "z_parameters_all"); known <- attr(prepared, "z_parameters_known_gender")
  expect_true(all(all$z_col %in% names(prepared)))
  expect_true(all(known$z_col %in% names(prepared)))
  expect_length(intersect(all$z_col, known$z_col), 0L)
})
test_that("the scientific-use file preserves the prepared values in configured order", {
  prepared <- pc_route("study_analysis_ready")
  exported <- ap3_scientific_use_data(prepared, pc_cfg, pc_book)
  expect_identical(names(exported), ap3_allowlist_expand(pc_cfg$data_files$scientific_use_file$keep, pc_book))
  expect_identical(exported$respondent_id, prepared$respondent_id)
  expect_length(intersect(attr(prepared, "z_parameters_all")$z_col, names(exported)), 0L)
  expect_error(ap3_scientific_use_data(dplyr::select(prepared, -age_band_midpoint), pc_cfg, pc_book), "undefined columns selected")
})
test_that("data_descriptive_reliability is exactly the narrow descriptive tibble", {
  narrow <- pc_route("data_descriptive_reliability")
  expect_identical(
    names(narrow),
    c("respondent_id", pc_items, pc_book$scales$scale_key, "gender", "quota_group")
  )
  # The descriptive-only political fields stay outside this selector; AP5 gets
  # them from the full study instead.
  expect_length(grep("^pol_", names(narrow)), 0L)
  expect_length(grep("_z($|_)", names(narrow)), 0L)
  expect_identical(narrow$respondent_id, pc_route("study_analysis_ready")$respondent_id)
})


test_that("the shared values contain the recorded item fills", {
  prepared <- pc_route("study_analysis_ready")
  shared <- ap3_scientific_use_data(prepared, pc_cfg, pc_book)
  cells <- pc_route("imputation_reporting_data")$imputed_cells
  expect_gt(nrow(cells), 0L)
  for (item in pc_items) expect_identical(shared[[item]], prepared[[item]], info = item)
  for (i in which(cells$kind == "item")) {
    row <- match(cells$respondent_id[i], shared$respondent_id)
    expect_equal(shared[[cells$variable[i]]][row], cells$value[i])
  }
})
test_that("the two fill records keep their columns and types", {
  reporting <- pc_route("imputation_reporting_data")
  cells <- build_filled_cells_table(reporting)
  dropped <- build_dropped_respondents_table(reporting, pc_cfg)
  expect_identical(names(cells),
                   c("respondent_id", "variable", "kind", "model", "value", "lower", "upper", "n_fit_rows"))
  expect_true(is.integer(cells$respondent_id))
  expect_true(is.integer(cells$n_fit_rows))
  for (column in c("variable", "kind", "model")) expect_true(is.character(cells[[column]]), info = column)
  for (column in c("value", "lower", "upper")) expect_true(is.double(cells[[column]]), info = column)
  expect_identical(names(dropped), c("respondent_id", "reason"))
  expect_true(is.character(dropped$reason))
  # Both are tables of the one reporting object.
  expect_equal(nrow(cells), nrow(reporting$imputed_cells))
  expect_equal(nrow(dropped), nrow(reporting$excluded_participants))
})

test_that("the two reasons of the exclusion rule are named separately and together", {
  reporting <- list(excluded_participants = tibble::tibble(
    respondent_id = c(1L, 2L, 3L),
    too_many_items_missing = c(TRUE, FALSE, TRUE),
    scales_over_threshold = c("asc_agg", NA_character_, "asc_agg, sdo_dom"),
    too_many_demographics_missing = c(FALSE, TRUE, TRUE),
    n_demographics_missing = c(0L, 2L, 3L)))
  expect_identical(
    build_dropped_respondents_table(reporting, pc_cfg)$reason,
    c("more than 2 missing items in asc_agg",
      "2 of 3 demographics missing",
      "more than 2 missing items in asc_agg, sdo_dom; 3 of 3 demographics missing")
  )
})

test_that("the exclusion record keeps each reason's detail in its own field", {
  scale_one <- pc_book$scales$scale_key[[1]]
  items_one <- pc_book$scales$item_codes[[1]]
  data <- tibble::tibble(respondent_id = 1:3, demo_age = c(40, NA, 40),
                         demo_hh_members = c(2, NA, 2), demo_income_hh_net = c(5, 5, 5))
  for (item in pc_items) data[[item]] <- 3
  for (item in items_one[1:3]) data[[item]][[1]] <- NA_real_
  reported <- exclude_participants_with_high_missingness(data, pc_book, pc_cfg)$reporting_data
  expect_identical(names(reported),
                   c("respondent_id", "too_many_items_missing", "scales_over_threshold",
                     "too_many_demographics_missing", "n_demographics_missing"))
  expect_identical(reported$respondent_id, 1:2)
  expect_identical(reported$scales_over_threshold, c(scale_one, NA_character_))
  expect_identical(reported$n_demographics_missing, c(0L, 2L))
})

# ---- The gender reference follows the exclusion -----------------------------

test_that("the gender reference is the largest group of the retained population", {
  # Three men and two women enter; the missingness rule removes two of the men,
  # so the retained majority is female. The reference follows the retained
  # population, as set_gender_reference() runs after the exclusion.
  scale <- pc_book$scales$item_codes[[1]]
  n <- 5L
  data <- tibble::tibble(respondent_id = seq_len(n), demo_gender = c(1, 1, 1, 2, 2),
                         demo_age = 40, demo_hh_members = 2, demo_income_hh_net = 5)
  for (item in pc_items) data[[item]] <- 3
  # The two excluded men miss three items of one scale; nobody else misses any.
  for (item in scale[1:3]) data[[item]][2:3] <- NA_real_
  coded <- prepare_gender(data, pc_cfg)
  expect_identical(levels(coded$gender), c("male", "female"))
  expect_true(all(coded$known_gender))
  eligible <- exclude_participants_with_high_missingness(coded, pc_book, pc_cfg)
  expect_identical(eligible$data$respondent_id, c(1L, 4L, 5L))
  expect_identical(levels(set_gender_reference(eligible$data)$gender), c("female", "male"))
  # Choosing it before the exclusion would have kept male, so the placement of
  # the verb is what decides the contrast.
  expect_identical(levels(set_gender_reference(coded)$gender), c("male", "female"))
})

# ---- Each standardisation population uses its own constants -----------------

test_that("the two standardisation populations use their own constants", {
  # Four participants with the scores 1, 2, 3, 4; the third has no gender. The
  # all-row mean is 2.5 and the known-gender mean 7/3, and each selector must
  # carry the constants of its own population.
  values <- c(1, 2, 3, 4)
  data <- tibble::tibble(respondent_id = 1:4, demo_gender = c(1, 1, NA, 2))
  for (key in pc_book$scales$scale_key) data[[key]] <- values
  data$age <- values
  data$age_band_midpoint <- values
  data$income <- exp(values)
  data$income_band_value_log <- values
  data <- prepare_gender(data, pc_cfg)
  data <- standardise_all(data, pc_book, pc_cfg)
  data <- standardise_known_gender(data, pc_book, pc_cfg)

  all_rows <- attr(data, "z_parameters_all", exact = TRUE)
  known <- attr(data, "z_parameters_known_gender", exact = TRUE)
  expect_true(all(all_rows$mean == 2.5))
  expect_true(all(all_rows$n == 4L))
  expect_true(all(abs(known$mean - 7 / 3) < 1e-12))
  expect_true(all(known$n == 3L))
  # The SDs are sqrt(5/3) over all four rows and sqrt(7/3) over the three with a
  # gender, and every z column is (x - mean) / sd in its own population.
  expect_true(all(abs(all_rows$sd - sqrt(5 / 3)) < 1e-12))
  expect_true(all(abs(known$sd - sqrt(7 / 3)) < 1e-12))
  for (column in all_rows$z_col) {
    expect_equal(data[[column]], (values - 2.5) / sqrt(5 / 3), info = column)
  }
  for (column in known$z_col) {
    expect_equal(data[[column]][-3], (values[-3] - 7 / 3) / sqrt(7 / 3), info = column)
  }
  # Age is standardised from its band midpoint and income from the logarithm of
  # its band representative, in both records' own terms.
  expect_identical(known$source[known$var == "age"], "age_band_midpoint")
  expect_identical(known$source[known$var == "income"], "income_band_value_log")
  expect_identical(known$transform[known$var == "income"], "log")
  # Unknown-gender rows keep NA in the known-gender family and a value in the
  # all-row family.
  expect_true(all(is.na(as.matrix(data[3, known$z_col]))))
  expect_false(any(is.na(as.matrix(data[3, all_rows$z_col]))))

  network <- select_network_input(data, pc_book, pc_cfg)
  expect_identical(attr(network, "z_parameters", exact = TRUE)$mean, all_rows$mean)
  # The network keeps every retained row; only the regressions drop the
  # missing-gender participant.
  expect_equal(nrow(network), 4L)
  regression <- select_regression_input(drop_rows_without_gender(data), pc_book, pc_cfg)
  expect_equal(nrow(regression), 3L)
  expect_identical(attr(regression, "omitted_gender", exact = TRUE)$respondent_id, 3L)
  expect_true(all(abs(attr(regression, "z_parameters", exact = TRUE)$mean - 7 / 3) < 1e-12))
  expect_false(any(is.na(as.matrix(dplyr::select(regression, dplyr::ends_with("_z"))))))
})
