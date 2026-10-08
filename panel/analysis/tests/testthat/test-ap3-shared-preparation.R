# The shared branch selects and standardises once, then provides its views.
# The input is the hand-built export of tests/fixtures/hand-controlled-input.rds.
shared_root <- normalizePath(getwd())
while (!file.exists(file.path(shared_root, "config", "analysis_plan.yaml"))) shared_root <- dirname(shared_root)
shared <- new.env(parent = globalenv())
sys.source(file.path(shared_root, "tests", "support", "target-reader.R"), shared)
shared$source_pipeline_functions(shared_root)
shared_cfg <- shared$zm_config("full", file.path(shared_root, "config", "analysis_plan.yaml"))
shared_book <- shared$zm_codebook(shared_cfg)
# The hand-controlled export records one household of 10 persons. The registered
# household-size codes stop at analysis_plan$income$hh_size_top_value, so that value lies
# outside the set check_covariate_input_codes() guards; the income rule caps at
# the same value, so reading the code as the top code leaves every derived
# income untouched.
shared_input <- readRDS(file.path(shared_root, "tests", "fixtures", "hand-controlled-input.rds"))
shared_input$demo_hh_members <- pmin(shared_input$demo_hh_members,
                                     shared_cfg$income$hh_size_top_value)
# The AP3 imputation happens once during intake preparation.
# Its four fits are the subject of test-ap3-fill.R; here
# they are pass-throughs, so that this file tests the shared preparation and
# never starts a sampler.
shared_reader <- function(input = shared_input, analysis_plan = shared_cfg) {
  shared$make_target_reader(shared_root, c(
    list(analysis_inputs = shared$make_analysis_inputs(input, analysis_plan, shared_book),
         analysis_plan = analysis_plan, codebook = shared_book),
    shared$make_fill_stubs()
  ))
}

test_that("common preparation scores once and records the whole-study reference", {
  input <- shared_input
  input$unneeded_extra_column <- "synthetic extra"
  reader <- shared_reader(input)
  study <- reader$read("study_analysis_ready")
  reporting <- reader$read("preparation_reporting_data")
  expect_false("unneeded_extra_column" %in% names(study))
  expect_true(all(shared_book$scales$scale_key %in% names(study)))
  expect_identical(reporting$gender_reference, "female")
  # The hand-controlled input plants a divers respondent in every sixth row,
  # five of them among the rows the six preregistered criteria keep. AP1
  # excludes that group as smaller than analysis_plan$exclusions$gender_divers_min_n, so
  # 28 respondents remain, of whom 27 gave a gender (row 19 of the input is
  # blank) and the planted arithmetic is left in the first five rows.
  observed_gender <- reporting$gender_counts$category != "missing"
  expect_equal(sum(reporting$gender_counts$n[observed_gender]), 27)
  expect_equal(reporting$gender_counts$n[!observed_gender], 1)
  # Every retained respondent's quota cell is recorded as exported or computed.
  expect_equal(nrow(study), 28)
  expect_equal(reporting$quota_group_computed, 3L)
  expect_identical(reporting$reversed_items, unique(unlist(shared_book$scales$reverse_items)))
  expect_true(is.na(study$gender[16]))
  expect_equal(reader$intake$data$income[1:5], c(250, 625/2, 875/3, 1125/8, 1375/8))
  expect_identical(as.character(study$income_band[1:5]), rep("<500", 5))
  expect_equal(study$income_band_value_log[1:5], log(study$income_band_value[1:5]))
  expect_equal(study$age_band_midpoint,
    unname(shared$calculate_age_band_midpoints(shared_cfg$age_bands)[as.character(study$age_band)]))
})

test_that("the analysis targets prepare common scores and their family views once", {
  score_calls <- list(); z_calls <- list()
  average_original <- shared$average_items_into_subscales
  all_original <- shared$standardise_all
  known_gender_original <- shared$standardise_known_gender
  rlang::local_bindings(
    average_items_into_subscales = function(data, codebook) {
      score_calls[[length(score_calls) + 1L]] <<- as.character(data$respondent_id)
      average_original(data, codebook)
    },
    standardise_all = function(data, codebook, analysis_plan) {
      standardised <- all_original(data, codebook, analysis_plan)
      z_calls[[length(z_calls) + 1L]] <<- list(
        population = "all", ids = as.character(data$respondent_id),
        z_parameters = attr(standardised, "z_parameters_all", exact = TRUE)
      )
      standardised
    },
    standardise_known_gender = function(data, codebook, analysis_plan) {
      standardised <- known_gender_original(data, codebook, analysis_plan)
      z_calls[[length(z_calls) + 1L]] <<- list(
        population = "known_gender",
        ids = as.character(data$respondent_id[!is.na(data$gender)]),
        z_parameters = attr(standardised, "z_parameters_known_gender", exact = TRUE)
      )
      standardised
    },
    .env = shared
  )
  reader <- shared_reader()
  study <- reader$read("study_analysis_ready")
  descriptive <- reader$read("data_descriptive_reliability")
  network <- reader$read("data_network")
  prepared <- reader$read("data_regressions")
  regression_reporting <- reader$read("regression_input_reporting_data")
  expected_ids <- as.character(study$respondent_id[!is.na(study$gender)])
  all_ids <- as.character(study$respondent_id)
  expect_length(score_calls, 1L)
  expect_length(z_calls, 2L)
  expect_identical(score_calls, list(all_ids))
  expect_identical(vapply(z_calls, `[[`, "", "population"), c("all", "known_gender"))
  expect_identical(z_calls[[1]]$ids, all_ids)
  expect_identical(z_calls[[2]]$ids, expected_ids)
  expect_setequal(z_calls[[2]]$z_parameters$var,
                  c(shared_cfg$regression$outcomes, shared_cfg$regression$motives, "age", "income"))
  expect_identical(as.character(regression_reporting$participants), expected_ids)
  expect_identical(regression_reporting$n_input, nrow(study))
  expect_identical(
    as.character(regression_reporting$omitted_gender$respondent_id),
    as.character(study$respondent_id[is.na(study$gender)])
  )
  expect_identical(levels(prepared$gender)[1], "female")
  physical_parameters <- z_calls[[2]]$z_parameters
  expect_identical(
    regression_reporting$z_parameters[names(regression_reporting$z_parameters) != "z_col"],
    physical_parameters[names(physical_parameters) != "z_col"]
  )
  expect_identical(
    regression_reporting$z_parameters$z_col,
    shared$zm_z_col(regression_reporting$z_parameters$var, shared_book)
  )
  expect_true(all(shared$zm_z_col(shared_cfg$regression$outcomes, shared_book) %in% names(prepared)))
  item_columns <- unique(unlist(shared_book$scales$item_codes))
  model_spec <- shared$ap3_model_spec(shared_cfg, shared_book)
  network_raw_sources <- model_spec$network$sources
  regression_sources <- unique(unlist(lapply(
    model_spec[shared_cfg$regression$outcomes], `[[`, "sources"
  )))
  regression_raw_sources <- intersect(regression_sources, shared$ap3_z_columns(shared_cfg, shared_book)$var)
  expect_true(all(item_columns %in% names(descriptive)))
  expect_true(all(shared_book$scales$scale_key %in% names(descriptive)))
  expect_false(any(item_columns %in% names(network)))
  expect_false(any(item_columns %in% names(prepared)))
  expect_false(any(network_raw_sources %in% names(network)))
  expect_false(any(regression_raw_sources %in% names(prepared)))
})

test_that("the network's z record follows the configured node order", {
  analysis_plan <- shared_cfg
  analysis_plan$regression$covariates <- c("income_z", "gender", "age_z")
  reader <- shared_reader(analysis_plan = analysis_plan)
  # The nodes are standardised and selected in analysis_plan$network$nodes, the
  # order the network figures and tables use, and reordering the regression
  # formula does not touch it.
  network <- attr(reader$read("data_network"), "z_parameters", exact = TRUE)
  expect_identical(shared$zm_z_col(network$var, shared_book),
                   as.character(unlist(analysis_plan$network$nodes)))
})

# ---- the imputation between the eligible population and the prepared frame ----
# No test in this suite runs tar_make(), so the wiring of the imputation is
# checked at the level of the target commands, with the fixture reader above.
# The fitting verbs are replaced by stubs that write a fixed value and record it:
# what is checked here is that a gap of the intake reaches the imputation, that
# the value it writes reaches every downstream table and the filled-cells file,
# and that a respondent the imputation must not reach leaves the study through
# the participant flow. The fits themselves are the subject of test-ap3-fill.R.

flow <- new.env(parent = globalenv())
for (f in c("report_helpers.R", "report_participant_flow.R", "report_results_sample.R")) {
  sys.source(file.path(shared_root, "R", f), flow)
}

# `predictors` is the fourth argument of the three demographic verbs and absent
# from impute_items(); one stub serves both signatures. The item verb is the
# first of the four, so it also initialises the typed empty record, as the
# production verb does.
fill_column_stub <- function(columns, value, kind, model, initialise = FALSE) {
  function(data, codebook, analysis_plan, predictors = NULL) {
    if (initialise) attr(data, "imputed_cells") <- shared$ap3_fill_empty_cells()
    for (column in columns) {
      gaps <- which(is.na(data[[column]]))
      if (length(gaps) == 0L) next
      data[[column]][gaps] <- value
      attr(data, "imputed_cells") <- dplyr::bind_rows(
        attr(data, "imputed_cells", exact = TRUE),
        tibble::tibble(
          respondent_id = data$respondent_id[gaps], variable = column, kind = kind,
          model = model, value = value, lower = value, upper = value,
          n_fit_rows = nrow(data)))
    }
    data
  }
}

test_that("a planted gap is filled through the split and a planted drop leaves the study", {
  book <- shared_book
  items_of <- function(key) book$scales$item_codes[[match(key, book$scales$scale_key)]]
  input <- shared_input
  # a respondent the imputation must not reach: three missing items in one scale
  dropped_id <- input$respondent_id[2]
  for (code in items_of("asc_agg")[1:3]) input[[code]][2] <- NA_real_
  # two gaps the imputation closes: one item and one age of another respondent
  filled_id <- input$respondent_id[3]
  gap_item <- items_of("zm_security")[1]
  input[[gap_item]][3] <- NA_real_
  input$demo_age[3] <- NA_real_

  reader <- shared$make_target_reader(shared_root, c(
    list(analysis_inputs = shared$make_analysis_inputs(input, shared_cfg, shared_book),
         analysis_plan = shared_cfg, codebook = shared_book),
    shared$make_fill_stubs(),
    list(impute_items = fill_column_stub(unlist(book$scales$item_codes), 3.5, "item",
                                             "zm_security", initialise = TRUE),
         impute_age = fill_column_stub("demo_age", 41, "demographic", "demo_age"))
  ))

  unfilled <- shared$apply_study_exclusions(input, shared_cfg)$data
  filled <- reader$intake$data
  imputation_record <- reader$read("imputation_reporting_data")
  dropped <- shared$build_dropped_respondents_table(imputation_record, shared_cfg)
  cells <- shared$build_filled_cells_table(imputation_record)

  # the drop happens in the exclusion target the imputation reads, not before it
  expect_equal(nrow(unfilled), 28L)
  expect_true(dropped_id %in% unfilled$respondent_id)
  expect_equal(nrow(filled), 27L)
  expect_identical(dropped$respondent_id, dropped_id)
  expect_match(dropped$reason, "more than 2 missing items in asc_agg", fixed = TRUE)
  expect_false(dropped_id %in% filled$respondent_id)
  # the planted gaps are gaps before the imputation and values after it
  expect_true(is.na(unfilled[[gap_item]][match(filled_id, unfilled$respondent_id)]))
  expect_true(is.na(unfilled$demo_age[match(filled_id, unfilled$respondent_id)]))
  expect_equal(filled[[gap_item]][match(filled_id, filled$respondent_id)], 3.5)
  expect_equal(filled$demo_age[match(filled_id, filled$respondent_id)], 41)
  # every imputed cell is recorded once, with the row the imputation wrote
  expect_equal(names(cells), names(shared$ap3_fill_empty_cells()))
  planted <- cells[cells$respondent_id == filled_id, , drop = FALSE]
  expect_true(gap_item %in% planted$variable)
  expect_true("demo_age" %in% planted$variable)
  expect_equal(planted$value[planted$variable == gap_item], 3.5)
  expect_equal(planted$value[planted$variable == "demo_age"], 41)
  expect_equal(anyDuplicated(paste(cells$respondent_id, cells$variable)), 0L)

  # the value reaches the prepared frame every model reads
  prepared <- reader$read("study_analysis_ready")
  expect_equal(prepared[[gap_item]][match(filled_id, prepared$respondent_id)], 3.5)
  expect_equal(prepared$age_band_midpoint[match(filled_id, prepared$respondent_id)], 42)
  expect_false(anyNA(prepared[[gap_item]]))
  expect_false(anyNA(prepared$zm_security))
  # the unfilled analysis table keeps the gap after removing the dropped respondent
  unfilled_analysis <- reader$intake$observed
  expect_equal(nrow(unfilled_analysis), 27L)
  expect_identical(unfilled_analysis$respondent_id, filled$respondent_id)
  expect_false(dropped_id %in% unfilled_analysis$respondent_id)
  expect_true(is.na(unfilled_analysis[[gap_item]][match(filled_id, unfilled_analysis$respondent_id)]))
  expect_true(is.na(unfilled_analysis$age[match(filled_id, unfilled_analysis$respondent_id)]))

  # the drop is a documented step of the participant flow, not a silent loss
  flow_value <- flow$report_exclusion_steps(reader$read("preparation_reporting_data"), imputation_record)
  log <- flow_value$steps
  step <- log[log$criterion == "dropped_unfillable_gaps", , drop = FALSE]
  expect_equal(nrow(step), 1L)
  expect_identical(step$step, max(log$step))
  expect_equal(step$n_before, 28L)
  expect_equal(step$n_excluded, 1L)
  expect_equal(step$n_after, 27L)
  counts <- flow$rh_participant_flow_data(log, flow_value$n_started, flow_value$n_quota_full)
  expect_equal(counts$analysed, 27L)
  expect_equal(counts$admitted - counts$excluded, counts$analysed)
  expect_identical(counts$reasons$label[counts$reasons$criterion == "dropped_unfillable_gaps"],
                   "More than two items missing within any scale, or at least two of age, household size and income band")

  # and both records are written as their own files
  target_dir <- withr::local_tempdir()
  cells_csv <- readr::read_csv(
    shared$write_filled_cells_file(cells, shared_cfg, path = file.path(target_dir, "filled_cells.csv")),
    show_col_types = FALSE, progress = FALSE)
  expect_equal(names(cells_csv), names(cells))
  expect_equal(nrow(cells_csv), nrow(cells))
  expect_true(any(cells_csv$respondent_id == filled_id & cells_csv$variable == gap_item))
  dropped_csv <- readr::read_csv(
    shared$write_dropped_respondents_file(dropped, shared_cfg,
                                        path = file.path(target_dir, "dropped_respondents.csv")),
    show_col_types = FALSE, progress = FALSE)
  expect_equal(dropped_csv$respondent_id, dropped$respondent_id)
  expect_match(dropped_csv$reason, "missing items", fixed = TRUE)
})

test_that("too few known-gender respondents fail at the standardisation that builds their population", {
  reader <- shared_reader()
  source_study <- reader$read("study_analysis_ready")
  for (n in 0:1) {
    study <- source_study[seq_len(n), ]
    expect_error(shared$standardise_known_gender(study, shared_book, shared_cfg),
                 "Fewer than two known-gender rows.", fixed = TRUE)
  }
})
