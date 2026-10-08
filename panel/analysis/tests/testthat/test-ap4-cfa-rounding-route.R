# The CFA rounding, on a connected route whose imputation stand-in writes
# FRACTIONAL item values.
#
# Every producer and consumer is the real one, chained through the production
# `_targets.R` commands (tests/support/target-reader.R). Nothing is
# fitted: the AP3 fill is make_fill_stubs() with an `item_placeholder` that
# plants fractional values instead of the integer column median,
# fit_cfa_models() is held back at the lavaan call, and the reliability
# estimator at ap4_capture_reliability_failure() as test-ap4-reliability.R does.
#
# The rule under test: imputed item cells stay fractional everywhere — common
# table, scale scores, reliability, EFA, network, regressions — and ONLY the
# CFA input copy (`data_cfa_input`) rounds them, to the digit with every half
# going UP: floor(x + 0.5), not R's half-to-even round().

local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) {
    parent <- dirname(dir)
    if (identical(parent, dir)) stop("project root not found from ", getwd())
    dir <- parent
  }
  for (f in c("config.R", "io_qualtrics.R", "ap1_exclusions.R", "ap3_preprocessing.R",
              "ap3_preparation.R", "ap3_fill.R", "ap3_data_files.R", "ap3_pipeline.R",
              "ap4_reliability.R", "ap4_factor_structure.R", "ap9_efa.R", "ap4_cfa_input.R")) {
    source(file.path(dir, "R", f), local = FALSE)
  }
  source(file.path(dir, "tests", "support", "targets-route.R"), local = FALSE)
})

crr_root <- zm_root()
crr_cfg <- zm_config("full", file.path(crr_root, "config", "analysis_plan.yaml"))
# Two bootstrap resamples instead of the profile's thousand: this test reads
# `scale_reliability` for the responses the estimator is handed, not for an
# interval. Nothing else in the route reads this number.
crr_cfg$reliability$bootstrap_n <- 2L
crr_book <- zm_codebook(crr_cfg)
crr_items <- unique(unlist(crr_book$scales$item_codes, use.names = FALSE))
crr_raw <- read_qualtrics_export(file.path(crr_root, "data", "synthetic", "zm_panel_synthetic.sav"))
crr_source_intake <- tr_intake(crr_raw, crr_cfg)

# ---- the planted fractional fills -------------------------------------------
# Five planned values, cycled over the item columns in codebook order. They are
# absolute values, not median offsets: the median's parity is not guaranteed,
# and the half-up rule has to be exercised on the three halves R's round() would
# treat differently — 2.5 -> 3 and 4.5 -> 5 (where round() would fall to the even
# integer below) and 3.5 -> 4 — beside two ordinary fractions. All five lie
# inside the items' 1..6 range.
crr_planned <- c(2.5, 3.5, 4.5, 3.3, 2.7)
crr_value_for <- function(column) {
  crr_planned[[(match(column, crr_items) - 1L) %% length(crr_planned) + 1L]]
}
crr_placeholder <- function(observed_values, column_name) crr_value_for(column_name)

# The synthetic export's own item gaps survive in only two item columns after
# the excessive-missingness exclusion (five cells), which cannot show both
# half-integer directions. One gap per planned value is therefore PLANTED in
# the intake frame — the test's own input, not production — by blanking one
# item cell of five participants whose item rows are otherwise complete. One
# cell each, in five different scales, so the drop rule (more than two missing
# items in a scale) removes nobody and five scale scores are affected. The
# columns are chosen from the codebook, one per planned value, so that every
# planned value is planted whatever order the codebook gives the scales: for
# each value the first item, not reverse-keyed, of a scale not yet used that
# the placeholder fills with it.
crr_planted_columns <- local({
  scale_of <- unlist(lapply(seq_len(nrow(crr_book$scales)), function(i) {
    stats::setNames(rep(crr_book$scales$scale_key[[i]], length(crr_book$scales$item_codes[[i]])),
                    crr_book$scales$item_codes[[i]])
  }))
  reversed <- unlist(crr_book$scales$reverse_items)
  picked <- character()
  for (value in crr_planned) {
    candidates <- crr_items[vapply(crr_items, crr_value_for, numeric(1)) == value &
                              !crr_items %in% reversed & !scale_of[crr_items] %in% scale_of[picked]]
    picked <- c(picked, candidates[[1]])
  }
  picked
})
crr_planted_ids <- local({
  env <- new.env(parent = globalenv())
  sys.source(file.path(crr_root, "tests", "support", "target-reader.R"), env)
  env$source_pipeline_functions(crr_root)
  reader <- env$make_target_reader(crr_root, c(
    list(analysis_inputs = env$make_analysis_inputs(crr_source_intake, crr_cfg, crr_book),
         analysis_plan = crr_cfg, codebook = crr_book),
    env$make_fill_stubs()))
  retained <- as.character(reader$intake$data$respondent_id)
  complete <- as.character(crr_source_intake$respondent_id)[
    rowSums(is.na(crr_source_intake[crr_items])) == 0L]
  utils::head(intersect(complete, retained), length(crr_planted_columns))
})
crr_intake <- local({
  data <- crr_source_intake
  rows <- match(crr_planted_ids, as.character(data$respondent_id))
  for (i in seq_along(crr_planted_columns)) data[[crr_planted_columns[i]]][rows[i]] <- NA_real_
  # `[[<-` drops the frame-level attributes the route needs (intake receipt,
  # value labels, the intake_verified flag); they are restored unchanged.
  attributes(data) <- attributes(crr_source_intake)
  data
})
# The planted cells, with the value the placeholder writes and its rounding.
crr_plan <- data.frame(
  respondent_id = crr_planted_ids, variable = crr_planted_columns,
  value = vapply(crr_planted_columns, crr_value_for, numeric(1), USE.NAMES = FALSE),
  stringsAsFactors = FALSE)
crr_plan$rounded <- floor(crr_plan$value + 0.5)  # half-up, not round()

# ---- the route --------------------------------------------------------------
crr_env <- new.env(parent = globalenv())
sys.source(file.path(crr_root, "tests", "support", "target-reader.R"), crr_env)
crr_env$source_pipeline_functions(crr_root)
crr_reader <- crr_env$make_target_reader(crr_root, c(
  list(analysis_inputs = crr_env$make_analysis_inputs(crr_intake, crr_cfg, crr_book),
       analysis_plan = crr_cfg, codebook = crr_book),
  crr_env$make_fill_stubs(item_placeholder = crr_placeholder)))
crr_route <- crr_reader$read
# The route's own environment, where the production verbs an interception must
# rebind live.
crr_prod <- parent.env(crr_reader$values)

crr_common <- crr_route("data_descriptive_reliability")
crr_record <- crr_env$build_filled_cells_table(crr_route("imputation_reporting_data"))
crr_cells <- crr_record[crr_record$kind == "item", , drop = FALSE]
crr_rows <- match(as.integer(crr_cells$respondent_id), as.integer(crr_common$respondent_id))
crr_plan_rows <- match(as.integer(crr_plan$respondent_id), as.integer(crr_common$respondent_id))
# The scale each planted item belongs to, for the scale-score checks.
crr_plan$scale <- vapply(crr_plan$variable, function(column) {
  crr_book$scales$scale_key[[which(vapply(crr_book$scales$item_codes,
                                          function(codes) column %in% codes, logical(1)))[1]]]
}, character(1), USE.NAMES = FALSE)

# ---- (1) the record carries the planted fractional values -------------------

test_that("the route's fill record carries the planted fractional item values", {
  expect_false(anyNA(crr_plan_rows))
  expect_identical(length(crr_planted_ids), length(crr_planted_columns))
  # Every planted cell is in the record exactly once, with its planned value.
  for (i in seq_len(nrow(crr_plan))) {
    hit <- which(as.character(crr_cells$respondent_id) == crr_plan$respondent_id[i] &
                   as.character(crr_cells$variable) == crr_plan$variable[i])
    expect_length(hit, 1L)
    expect_identical(as.numeric(crr_cells$value[hit]), crr_plan$value[i])
  }
  # Every recorded item cell — planted or the export's own — carries the
  # placeholder's value for its column, and no recorded value is an integer.
  expect_false(anyNA(crr_rows))
  expect_identical(as.numeric(crr_cells$value),
                   vapply(as.character(crr_cells$variable), crr_value_for,
                          numeric(1), USE.NAMES = FALSE))
  expect_true(all(as.numeric(crr_cells$value) %% 1 != 0))
  # The three halves R's round() would split, and an ordinary fraction, are
  # exercised.
  exercised <- unique(as.numeric(crr_cells$value))
  halves <- exercised[exercised %% 1 == 0.5]
  expect_setequal(halves, c(2.5, 3.5, 4.5))
  expect_true(any(round(halves) < halves))   # round() would send 2.5 and 4.5 down
  expect_true(any(exercised %% 1 != 0 & exercised %% 1 != 0.5))
  # every half goes UP under the rule this route tests
  expect_true(all(floor(halves + 0.5) > halves))
})

# ---- (2) the common tables keep the fractional values -----------------------

test_that("the common table and the prepared study keep the fractions", {
  prepared <- crr_route("study_analysis_ready")
  for (i in seq_len(nrow(crr_cells))) {
    column <- as.character(crr_cells$variable)[i]
    value <- as.numeric(crr_cells$value[i])
    expect_identical(crr_common[[column]][crr_rows[i]], value)
    expect_identical(prepared[[column]][
      match(as.integer(crr_cells$respondent_id[i]), as.integer(prepared$respondent_id))], value)
  }
})

# ---- (3) the CFA copy is the common table with exactly those cells rounded --

test_that("data_cfa_input rounds the recorded item cells half-up and nothing else", {
  cfa <- crr_route("data_cfa_input")
  expected <- crr_common
  for (column in unique(as.character(crr_cells$variable))) {
    hit <- crr_rows[as.character(crr_cells$variable) == column]
    expected[[column]][hit] <- floor(expected[[column]][hit] + 0.5)
  }
  expect_identical(cfa, expected)
  expect_identical(attributes(cfa), attributes(crr_common))
  expect_identical(cfa$respondent_id, crr_common$respondent_id)
  # A fractional fill exists in this run, so the two tables must differ.
  expect_false(identical(cfa, crr_common))
  # Per planted cell, the half-up result, stated explicitly.
  for (i in seq_len(nrow(crr_plan))) {
    expect_identical(cfa[[crr_plan$variable[i]]][crr_plan_rows[i]], crr_plan$rounded[i])
    expect_identical(crr_common[[crr_plan$variable[i]]][crr_plan_rows[i]], crr_plan$value[i])
  }
  # 2.5 -> 3, 3.5 -> 4, 4.5 -> 5: every half goes up.
  halves <- crr_plan[crr_plan$value %% 1 == 0.5, , drop = FALSE]
  expect_setequal(halves$value, c(2.5, 3.5, 4.5))
  expect_identical(halves$rounded, halves$value + 0.5)
  expect_true(all(halves$rounded > halves$value))
})

# ---- (4) all fourteen CFA models receive the rounded values -----------------

test_that("the one confirmatory target is fitted on the rounded copy, not on the common table", {
  cfa <- crr_route("data_cfa_input")
  cfa_columns_of <- get("ap4_cfa_item_columns", crr_prod)
  syntax_of <- get("ap4_build_lavaan_factor_syntax", crr_prod)
  capture_fit <- get("ap4_capture_cfa_fit", crr_prod)
  received <- list()
  # The estimator boundary only: the real definition verbs and the real column
  # resolution run, the lavaan call does not.
  rlang::local_bindings(fit_cfa_models = function(cfa_model_set, codebook, analysis_plan) {
    cfa_model_set$fits <- lapply(names(cfa_model_set$models), function(model_name) {
      executable <- cfa_columns_of(cfa_model_set$models[[model_name]], codebook)
      received[[model_name]] <<- list(respondent_id = cfa_model_set$data$respondent_id,
                                      items = cfa_model_set$data[executable$columns],
                                      columns = executable$columns)
      fit_record <- capture_fit(stop("cfa rounding route: estimator deliberately not run"))
      fit_record$syntax <- syntax_of(executable$factors)
      fit_record$columns <- executable$columns
      fit_record
    })
    names(cfa_model_set$fits) <- names(cfa_model_set$models)
    cfa_model_set
  }, .env = crr_prod)
  assessed <- crr_route("cfa_models")

  expect_identical(length(assessed$models), 14L)
  expect_length(received, 14L)
  touched <- 0L
  for (model_name in names(received)) {
    got <- received[[model_name]]
    expect_identical(got$items, cfa[got$columns])
    expect_identical(got$respondent_id, cfa$respondent_id)
    # Wherever a planted column is in this model, the model saw the ROUNDED
    # value and the common table's fractional value is demonstrably not it.
    for (i in seq_len(nrow(crr_plan))) {
      if (!crr_plan$variable[i] %in% got$columns) next
      touched <- touched + 1L
      expect_identical(got$items[[crr_plan$variable[i]]][crr_plan_rows[i]], crr_plan$rounded[i])
      expect_false(identical(got$items[[crr_plan$variable[i]]][crr_plan_rows[i]],
                             crr_common[[crr_plan$variable[i]]][crr_plan_rows[i]]))
    }
  }
  expect_gt(touched, 0L)
})

# ---- (5) the non-CFA consumers keep the fractional values ------------------

test_that("EFA, the reliability estimator and the scale scores keep the fractions", {
  # EFA: each set is a column selection of the common table, so its rows are
  # the common table's rows in the same order and carry the same values.
  sets <- crr_route("efa_item_sets")
  seen_efa <- 0L
  for (set in sets) {
    expect_identical(nrow(set$data), nrow(crr_common))
    for (i in seq_len(nrow(crr_plan))) {
      if (!crr_plan$variable[i] %in% names(set$data)) next
      seen_efa <- seen_efa + 1L
      expect_identical(set$data[[crr_plan$variable[i]]][crr_plan_rows[i]], crr_plan$value[i])
    }
  }
  expect_gt(seen_efa, 0L)

  # Reliability: the responses the estimator is handed, recorded at the real
  # selection verb while the coefficient calls themselves are held back.
  original_coefficients <- get("estimate_reliability_coefficients", crr_prod)
  responses <- list()
  rlang::local_bindings(
    ap4_capture_reliability_failure = function(expression) {
      list(ok = FALSE, value = NULL, error = "cfa rounding route: estimator deliberately not run")
    },
    estimate_reliability_coefficients = function(data, codebook) {
      reliability <- original_coefficients(data, codebook)
      for (scale in reliability$scales) responses[[scale$scale_key]] <<- scale$responses
      reliability
    }, .env = crr_prod)
  crr_route("scale_reliability")
  seen_reliability <- 0L
  for (i in seq_len(nrow(crr_plan))) {
    items <- responses[[crr_plan$scale[i]]]
    expect_false(is.null(items))
    expect_true(crr_plan$variable[i] %in% names(items))
    seen_reliability <- seen_reliability + 1L
    expect_identical(items[[crr_plan$variable[i]]][crr_plan_rows[i]], crr_plan$value[i])
  }
  expect_identical(seen_reliability, nrow(crr_plan))

  # Scale scores: the participant's score is the mean over the scale's items
  # INCLUDING the fractional value, not the rounded one.
  for (i in seq_len(nrow(crr_plan))) {
    codes <- crr_book$scales$item_codes[[
      which(crr_book$scales$scale_key == crr_plan$scale[i])[1]]]
    values <- unlist(crr_common[crr_plan_rows[i], codes], use.names = FALSE)
    expect_identical(crr_common[[crr_plan$scale[i]]][crr_plan_rows[i]], mean(values))
    rounded_values <- values
    rounded_values[match(crr_plan$variable[i], codes)] <- crr_plan$rounded[i]
    expect_false(isTRUE(all.equal(mean(values), mean(rounded_values))))
  }
})

# ---- (6) the network and regression z columns derive from the fractions -----

test_that("the standardised columns of data_network and data_regressions are the fractional ones", {
  for (name in c("data_network", "data_regressions")) {
    data <- crr_route(name)
    parameters <- attr(data, "z_parameters", exact = TRUE)
    present <- crr_plan[as.integer(crr_plan$respondent_id) %in% as.integer(data$respondent_id), ,
                        drop = FALSE]
    expect_gt(nrow(present), 0L)
    checked <- 0L
    for (i in seq_len(nrow(present))) {
      row <- which(parameters$var == present$scale[i])
      if (length(row) != 1L) next            # not a standardised variable of this model
      z_col <- parameters$z_col[row]
      score <- crr_common[[present$scale[i]]][
        match(as.integer(present$respondent_id[i]), as.integer(crr_common$respondent_id))]
      expect_equal(data[[z_col]][match(as.integer(present$respondent_id[i]),
                                       as.integer(data$respondent_id))],
                   (score - parameters$mean[row]) / parameters$sd[row])
      checked <- checked + 1L
    }
    expect_gt(checked, 0L)
  }
})
