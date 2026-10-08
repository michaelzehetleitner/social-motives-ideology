local({
  root <- normalizePath(getwd())
  while (!file.exists(file.path(root, "config", "analysis_plan.yaml"))) root <- dirname(root)
  current <- new.env(parent = globalenv())
  for (file in list.files(file.path(root, "R"), pattern = "[.]R$", full.names = TRUE)) sys.source(file, current)
  assign("demographic_cases_env", current, envir = globalenv())
})
demographic_cases_plan <- demographic_cases_env$zm_config("smoke")
demographic_cases_book <- demographic_cases_env$zm_codebook(demographic_cases_plan)

create_demographic_case_example <- function(variable, observed_gap = TRUE, recipient_gap = TRUE) {
  data <- tibble::tibble(respondent_id = seq_len(30L))
  for (item in unlist(demographic_cases_book$scales$item_codes)) data[[item]] <- rep(1:6, 5L)
  data$demo_age <- seq(20, 49)
  data$demo_hh_members <- rep(1:6, 5L)
  data$demo_income_hh_net <- rep(1:10, 3L)
  data$gender <- factor(rep(c("male", "female"), 15L))
  data$known_gender <- TRUE
  item <- demographic_cases_book$scales$item_codes[[1]][1]
  gap_rows <- c(if (observed_gap) 1L, if (recipient_gap) 3L)
  data[[item]][gap_rows] <- NA_real_
  data[[variable]][c(2L, if (recipient_gap) 3L)] <- NA_real_
  data <- demographic_cases_env$record_imputation_model_status(data,
    list(status = "failed", diagnostics = list(note = "Unavailable item fill"), history = tibble::tibble()),
    demographic_cases_book$scales$scale_key[1], "item", demographic_cases_book$scales$item_codes[[1]], length(gap_rows))
  attr(data, "imputed_cells") <- demographic_cases_env$ap3_fill_empty_cells()
  demographic_cases_env$identify_unavailable_imputation_variables(data, demographic_cases_book)
}

create_valid_demographic_diagnostics <- function() {
  list(ok = TRUE, ess_ok = TRUE, sampler_retry_needed = FALSE, note = "ok",
    parameter_count = 10L, ess_bulk_min = 3000, ess_tail_min = 3000,
    rhat_max = 1, n_divergent = 0L, n_treedepth_hits = 0L, bfmi_min = .5)
}

test_that("demographic training and prediction select only their required complete cases", {
  data <- create_demographic_case_example("demo_age")
  predictors <- demographic_cases_env$select_demographic_imputation_predictors(
    data, demographic_cases_book, demographic_cases_plan)
  raw_specification <- demographic_cases_env$ap3_age_imputation_specification(
    data, demographic_cases_book, demographic_cases_plan, predictors$demo_age)
  specification <- demographic_cases_env$prepare_demographic_imputation_cases(raw_specification)
  expect_identical(specification$formula, raw_specification$formula)
  expect_equal(specification$n_missing, 2L)
  expect_equal(specification$n_training, 27L)
  expect_equal(specification$n_eligible, 1L)
  expect_identical(specification$respondent_id, 2L)
  expect_false(anyNA(specification$observed_values))
  expect_false(anyNA(specification$participants_with_missing_value))
  expect_equal(specification$observed_values$demo_age, data$demo_age[4:30])
  expect_identical(specification$blocked_predictions$respondent_id, 3L)
  expect_identical(specification$blocked_predictions$variable, "demo_age")
  expect_identical(specification$blocked_predictions$missing_predictors,
    demographic_cases_book$scales$scale_key[1])
  expect_identical(levels(specification$observed_values$gender), levels(data$gender))
})

test_that("available demographic recipients are filled while blocked recipients remain explicit", {
  variables <- c("demo_age", "demo_hh_members", "demo_income_hh_net")
  fit_functions <- c("impute_age", "impute_household_size", "impute_income_band")
  expected_fills <- c(44.5, 2, 4)
  calls <- list()
  local_mocked_bindings(brm = function(formula, data, family, prior, ...) {
    calls[[length(calls) + 1L]] <<- list(formula = formula, data = data,
      family = family, prior = prior, sampling = list(...))
    list(formula = formula, data = data)
  }, posterior_epred = function(object, newdata, ...) {
    value <- if (all.vars(object$formula)[1] == "demo_age") 44.5 else 2.5
    matrix(value, nrow = 4L, ncol = nrow(newdata))
  }, posterior_predict = function(object, newdata, ...) {
    value <- if (all.vars(object$formula)[1] == "demo_income_hh_net") 4 else 45
    matrix(value, nrow = 4L, ncol = nrow(newdata))
  }, .package = "brms")
  local_mocked_bindings(nobs = function(object, ...) nrow(object$data), .package = "stats")
  rlang::local_bindings(extract_imputation_diagnostics = function(...) create_valid_demographic_diagnostics(),
    .env = demographic_cases_env)
  for (i in seq_along(variables)) {
    variable <- variables[i]
    data <- create_demographic_case_example(variable)
    predictors <- demographic_cases_env$select_demographic_imputation_predictors(
      data, demographic_cases_book, demographic_cases_plan)
    filled <- demographic_cases_env[[fit_functions[i]]](
      data, demographic_cases_book, demographic_cases_plan, predictors[[variable]])
    expect_equal(filled[[variable]][2], expected_fills[i])
    expect_true(is.na(filled[[variable]][3]))
    expect_equal(filled[[variable]][-c(2L, 3L)], data[[variable]][-c(2L, 3L)])
    call <- calls[[i]]
    expect_identical(all.vars(call$formula)[-1L], predictors[[variable]])
    expect_equal(nrow(call$data), 27L)
    expect_false(anyNA(call$data))
    expect_equal(call$data$demo_age, data$demo_age[4:30])
    if (variable == "demo_income_hh_net") expect_equal(call$data$demo_hh_members, data$demo_hh_members[4:30])
    expect_equal(call$sampling$chains, demographic_cases_plan$regression$chains)
    expect_equal(call$sampling$warmup, demographic_cases_plan$regression$warmup)
    expect_equal(call$sampling$iter,
      demographic_cases_plan$regression$warmup + demographic_cases_plan$regression$iter_per_chain)
    status <- attr(filled, "imputation_model_status")
    row <- status[status$model == variable, ]
    expect_identical(row$status, "ok")
    expect_equal(row$n_missing, 2L)
    expect_equal(row$n_eligible, 1L)
    expect_equal(row$n_blocked, 1L)
    expect_equal(row$n_training, 27L)
    expect_equal(row$attempts, 1L)
    expect_identical(attr(filled, "blocked_imputation_predictions")$respondent_id, 3L)
    expect_error(demographic_cases_env$check_demographic_imputations_complete(filled), NA)
    report <- demographic_cases_env$build_imputation_reporting_data(filled, tibble::tibble())
    report_row <- report$model_status[report$model_status$model == variable, ]
    expect_equal(report_row$n_filled, 1L)
    expect_equal(report_row$n_unfilled, 1L)
    expect_equal(report$imputed_cells$n_fit_rows, 27L)
    expect_identical(report$imputed_cells$respondent_id, 2L)
    expect_identical(report$unfilled_cells$respondent_id[report$unfilled_cells$variable == variable], 3L)
    expect_true(demographic_cases_env$has_failed_imputation(report))
    expect_match(demographic_cases_env$describe_imputation_processing(report), "individual prediction was unavailable")
    scored <- demographic_cases_env$average_items_into_subscales(filled, demographic_cases_book)
    scored <- demographic_cases_env$identify_unavailable_imputation_variables(scored, demographic_cases_book)
    retained <- demographic_cases_env$exclude_rows_with_failed_imputations(scored, variable)
    expect_identical(retained$respondent_id, setdiff(1:30, 3L))
    expect_identical(attr(retained, "blocked_imputation_predictions"), attr(scored, "blocked_imputation_predictions"))
    html <- demographic_cases_env$render_imputation_processing_html(report,
      demographic_cases_env$tabulate_imputation_analysis_availability(report, demographic_cases_book, demographic_cases_plan))
    expect_match(html, "Unfilled individual predictions")
    expect_match(html, "unresolved AP3 values")
    expect_false(grepl("imputation failure", html, fixed = TRUE))
    expect_match(html, "n_eligible")
    expect_match(html, "n_filled")
    expect_match(html, "n_unfilled")
  }
})

test_that("the original missing-gender rule survives demographic case selection", {
  data <- create_demographic_case_example("demo_age")
  data$gender[1] <- NA; data$known_gender[1] <- FALSE
  predictors <- demographic_cases_env$select_demographic_imputation_predictors(
    data, demographic_cases_book, demographic_cases_plan)
  expect_true(all(vapply(predictors, function(x) !"gender" %in% x, logical(1))))
  specification <- demographic_cases_env$prepare_demographic_imputation_cases(
    demographic_cases_env$ap3_age_imputation_specification(
      data, demographic_cases_book, demographic_cases_plan, predictors$demo_age))
  expect_false("gender" %in% names(specification$observed_values))
  expect_identical(specification$respondent_id, 2L)
})

test_that("demographic fits are skipped when training cases or eligible recipients are absent", {
  local_mocked_bindings(brm = function(...) stop("A skipped fit is called"), .package = "brms")
  for (no_training in c(FALSE, TRUE)) {
    data <- create_demographic_case_example("demo_age")
    if (no_training) data$demo_age[] <- NA_real_ else data$demo_age[2] <- 35
    predictors <- demographic_cases_env$select_demographic_imputation_predictors(
      data, demographic_cases_book, demographic_cases_plan)
    result <- demographic_cases_env$impute_age(
      data, demographic_cases_book, demographic_cases_plan, predictors$demo_age)
    row <- attr(result, "imputation_model_status")
    row <- row[row$model == "demo_age", ]
    expect_identical(row$status, "blocked")
    expect_equal(row$attempts, 0L)
    expect_equal(row$n_eligible, 0L)
    expect_equal(row$n_eligible + row$n_blocked, row$n_missing)
    expect_match(row$note, if (no_training) "no observed-outcome respondents" else "no recipients")
    expect_identical(result$demo_age, data$demo_age)
    report <- demographic_cases_env$build_imputation_reporting_data(result, tibble::tibble())
    expect_equal(sum(report$unfilled_cells$variable == "demo_age"), sum(is.na(data$demo_age)))
    expect_equal(sum(report$blocked_predictions$variable == "demo_age"), sum(is.na(data$demo_age)))
  }
})


test_that("successful partial fills explain only their recorded blocked cells", {
  data <- create_demographic_case_example("demo_age")
  predictors <- demographic_cases_env$select_demographic_imputation_predictors(
    data, demographic_cases_book, demographic_cases_plan)
  specification <- demographic_cases_env$prepare_demographic_imputation_cases(
    demographic_cases_env$ap3_age_imputation_specification(
      data, demographic_cases_book, demographic_cases_plan, predictors$demo_age))
  data <- demographic_cases_env$record_blocked_imputation_predictions(data, specification)
  data$demo_age[2] <- 44
  expect_false(demographic_cases_env$has_unexplained_imputation_cells(data, "demo_age"))
  data$demo_age[4] <- NA_real_
  expect_true(demographic_cases_env$has_unexplained_imputation_cells(data, "demo_age"))
  expect_error(demographic_cases_env$check_demographic_imputations_complete(data),
    "demographic imputation left")
})

test_that("a failed demographic fit and blocked recipients count each unresolved cell once", {
  data <- create_demographic_case_example("demo_age")
  predictors <- demographic_cases_env$select_demographic_imputation_predictors(
    data, demographic_cases_book, demographic_cases_plan)
  local_mocked_bindings(brm = function(...) stop("Unavailable age fit"), .package = "brms")
  result <- demographic_cases_env$impute_age(
    data, demographic_cases_book, demographic_cases_plan, predictors$demo_age)
  report <- demographic_cases_env$build_imputation_reporting_data(result, tibble::tibble())
  ages <- report$unfilled_cells[report$unfilled_cells$variable == "demo_age", ]
  expect_identical(sort(ages$respondent_id), c(2L, 3L))
  expect_false(anyDuplicated(ages$respondent_id) > 0L)
  row <- report$model_status[report$model_status$model == "demo_age", ]
  expect_identical(row$status, "failed")
  expect_equal(row$n_missing, 2L)
  expect_equal(row$n_eligible, 1L)
  expect_equal(row$n_blocked, 1L)
  expect_equal(row$n_filled, 0L)
  expect_equal(row$n_unfilled, 2L)
  expect_identical(report$blocked_predictions$respondent_id, 3L)
})
