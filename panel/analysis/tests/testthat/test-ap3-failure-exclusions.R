# AP3 failure exclusions: actual retained cases, constants and model calls.
local({
  root <- normalizePath(getwd())
  while (!file.exists(file.path(root, "config", "analysis_plan.yaml"))) root <- dirname(root)
  current <- new.env(parent = globalenv())
  for (file in list.files(file.path(root, "R"), pattern = "[.]R$", full.names = TRUE)) sys.source(file, current)
  assign("failure_env", current, envir = globalenv())
})
failure_plan <- failure_env$zm_config("smoke")
failure_book <- failure_env$zm_codebook(failure_plan)

prepare_failed_imputation_example <- function(variable, rows = 1L, missing_gender = integer()) {
  set.seed(23)
  data <- tibble::tibble(respondent_id = 1:30)
  for (item in unlist(failure_book$scales$item_codes)) data[[item]] <- sample(1:6, 30, TRUE)
  data$demo_age <- sample(20:70, 30, TRUE)
  data$gender <- factor(rep(c("male", "female"), 15))
  data$gender[missing_gender] <- NA
  data[[variable]][rows] <- NA_real_
  data <- failure_env$record_imputation_model_status(data,
    list(status = "failed", diagnostics = list(note = "controlled failure"), history = tibble::tibble()),
    variable, if (variable == "demo_age") "demographic" else "item", variable, length(rows))
  attr(data, "imputed_cells") <- failure_env$ap3_fill_empty_cells()
  data <- failure_env$identify_unavailable_imputation_variables(data, failure_book)
  reporting <- failure_env$build_imputation_reporting_data(data, tibble::tibble())
  data <- failure_env$average_items_into_subscales(data, failure_book)
  data$age <- data$demo_age
  data$income <- seq(250, 4000, length.out = nrow(data))
  data <- failure_env$add_age_and_income_bands(data, failure_plan)
  raw <- data
  data <- failure_env$standardise_all(data, failure_book, failure_plan)
  data <- failure_env$standardise_known_gender(data, failure_book, failure_plan)
  regressions <- failure_env$select_regression_input(
    failure_env$drop_rows_without_gender(data), failure_book, failure_plan)
  list(raw = raw, prepared = data, regressions = regressions, reporting = reporting)
}

test_that("a failed outcome fill changes only samples requiring that outcome", {
  item <- failure_book$scales$item_codes[[match("asc_agg", failure_book$scales$scale_key)]][1]
  example <- prepare_failed_imputation_example(item)
  models <- failure_env$define_regression_model_set(failure_plan)
  for (name in names(models$formulas)) {
    formula <- models$formulas[[name]]
    sample <- failure_env$select_regression_data_for_model(example$regressions, all.vars(formula))
    ids <- if (name == "authoritarian_aggression") 2:30 else 1:30
    expect_identical(sample$respondent_id, ids)
    expect_false(anyNA(sample))
    constants <- attr(sample, "z_parameters")
    expect_true(all(constants$n == length(ids)))
    for (i in seq_len(nrow(constants))) {
      x <- example$raw[[constants$source[i]]][ids]
      expect_equal(constants$mean[i], mean(x))
      expect_equal(constants$sd[i], stats::sd(x))
      expect_equal(sample[[constants$z_col[i]]], as.numeric(scale(x)))
    }
  }
  joint <- failure_env$define_joint_regression_model(example$regressions, models, failure_plan)
  motives <- failure_env$define_motive_model(example$regressions, models, failure_plan)
  expect_identical(joint$data$respondent_id, 2:30)
  expect_identical(motives$data$respondent_id, 1:30)
  network <- failure_env$select_network_input(example$prepared, failure_book, failure_plan)
  expect_identical(network$respondent_id, 2:30)
  expect_true(all(attr(network, "z_parameters")$n == 29L))
  for (column in setdiff(names(network), "respondent_id")) {
    expect_equal(mean(network[[column]]), 0, tolerance = 1e-12)
    expect_equal(stats::sd(network[[column]]), 1)
  }
  availability <- failure_env$tabulate_imputation_analysis_availability(example$reporting, failure_book, failure_plan)
  regression_rows <- availability[grepl("Adjusted regression", availability$analysis), ]
  expect_equal(sort(regression_rows$n_eligible), c(29L, 30L, 30L, 30L))
  expect_equal(sum(regression_rows$n_excluded), 1L)
  sizes <- failure_env$report_analysis_sample_sizes(list(n_analysis = 30L),
    failure_env$build_regression_input_reporting_data(example$regressions), network)
  expect_equal(unname(sizes$n_regressions_by_outcome), c(29L, 30L, 30L, 30L))
})

test_that("every sensitivity fit reads the primary model's cases and constants", {
  item <- failure_book$scales$item_codes[[match("asc_agg", failure_book$scales$scale_key)]][1]
  example <- prepare_failed_imputation_example(item)
  models <- failure_env$define_regression_model_set(failure_plan)
  captured <- list()
  local_mocked_bindings(brm = function(formula, data, ...) {
    captured[[length(captured) + 1L]] <<- list(formula = formula, data = data)
    structure(list(formula = formula, data = data), class = "brmsfit")
  }, posterior_predict = function(object, ...) {
    matrix(rep(object$data[[all.vars(object$formula)[1]]], each = 4L), nrow = 4L)
  }, .package = "brms")
  rlang::local_bindings(ap6_student_tail_trigger = function(...) TRUE, .env = failure_env)
  primary <- failure_env$fit_primary_regressions(example$regressions, models, failure_plan)
  failure_env$fit_prior_width_comparisons(example$regressions, models, failure_plan)
  prior <- failure_env$check_prior_predictions(example$regressions, models, failure_plan)
  failure_env$fit_student_t_if_needed(example$regressions, models, primary,
    list(posterior = stats::setNames(rep(list(list()), 4L), names(primary))), failure_plan)
  expect_length(captured, 32L)
  for (call in captured) {
    name <- names(models$formulas)[vapply(models$formulas, function(f) identical(f, call$formula), logical(1))]
    expect_length(name, 1L)
    expect_identical(call$data, primary[[name]]$data)
  }
  for (outcome in prior) for (leaf in outcome) {
    raw <- example$raw[[attr(primary[[leaf$outcome]]$data, "z_parameters")$source[
      match(leaf$response_column, attr(primary[[leaf$outcome]]$data, "z_parameters")$z_col)]]]
    ids <- primary[[leaf$outcome]]$data$respondent_id
    expect_equal(leaf$raw_scale_predictions, matrix(rep(raw[ids], each = 4L), nrow = 4L))
  }
})

test_that("failed motive fills retain observed scale descriptions and pairwise correlations", {
  item <- failure_book$scales$item_codes[[1]][1]
  example <- prepare_failed_imputation_example(item)
  distributions <- failure_env$describe_scale_distributions(example$raw, failure_book)
  expect_equal(distributions$n, c(29L, rep(30L, 8L)))
  correlations <- failure_env$describe_scale_correlations(example$raw, failure_book, failure_plan)
  expect_true(all(correlations$n[1, ] == 29L))
  expect_true(all(correlations$n[-1, -1] == 30L))
  bayesian <- failure_env$calculate_bayesian_score_correlations(example$raw, failure_book,
    failure_plan, posterior_iterations = 100L)
  expect_true(all(bayesian$pairs$n[bayesian$pairs$var_i == failure_book$scales$scale_key[1]] == 29L))
  expect_true(all(bayesian$pairs$n[bayesian$pairs$var_i != failure_book$scales$scale_key[1]] == 30L))
  expect_true(all(bayesian$pairs$status == "ok"))
})

test_that("excluding every affected case leaves unrelated regressions available", {
  item <- failure_book$scales$item_codes[[match("asc_agg", failure_book$scales$scale_key)]][1]
  example <- prepare_failed_imputation_example(item, 1:30)
  models <- failure_env$define_regression_model_set(failure_plan)
  affected <- failure_env$select_regression_data_for_model(example$regressions,
    all.vars(models$formulas$authoritarian_aggression))
  expect_equal(nrow(affected), 0L)
  expect_error(failure_env$require_valid_imputation_inputs(affected, names(affected)), class = "imputation_unavailable")
  remaining <- failure_env$select_regression_data_for_model(example$regressions,
    all.vars(models$formulas$conventionalism))
  expect_identical(remaining$respondent_id, 1:30)
  expect_false(anyNA(remaining))
})

test_that("confirmatory models retain their own complete respondents", {
  item <- failure_book$scales$item_codes[[1]][1]
  example <- prepare_failed_imputation_example(item)
  captured <- list()
  local_mocked_bindings(cfa = function(model, data, ...) {
    captured[[length(captured) + 1L]] <<- data
    structure(list(), class = "lavaan")
  }, .package = "lavaan")
  models <- example$raw |>
    failure_env$define_subscale_models(failure_plan, failure_book) |>
    failure_env$define_ums_dopl_models(failure_plan, failure_book) |>
    failure_env$define_social_motives_model(failure_plan, failure_book) |>
    failure_env$define_asc_model(failure_plan, failure_book) |>
    failure_env$define_auth_orientation_model(failure_plan, failure_book) |>
    failure_env$fit_cfa_models(failure_book, failure_plan)
  expect_length(captured, 14L)
  for (i in seq_along(captured)) {
    columns <- names(captured[[i]])
    ids <- if (item %in% columns) 2:30 else 1:30
    expect_equal(lapply(captured[[i]], identity), lapply(example$raw[ids, columns], identity))
    expect_false(anyNA(captured[[i]]))
    excluded <- models$fits[[i]]$excluded_imputation
    if (item %in% columns) expect_identical(excluded$respondent_id, 1L) else expect_null(excluded)
    expect_identical(models$fits[[i]]$n, as.integer(length(ids)))
  }
  # This test isolates case-count propagation; numerical evidence extraction is
  # tested separately. The actual AP3 exclusions and CFA input calls run above.
  rlang::local_bindings(
    ap4_extract_cfa_evidence = function(fit, model, label) list(
      fit_indices = tibble::tibble(), loadings = tibble::tibble(),
      factor_correlations = tibble::tibble(), ave = tibble::tibble(),
      issues = tibble::tibble(component = character(), message = character())),
    ap4_extract_and_rank_cfa_residuals = function(...) list(
      all_pairs = tibble::tibble(), ranked_by_absolute_standardized = tibble::tibble(),
      issue = NA_character_),
    .env = failure_env)
  models$assessment <- lapply(models$fits, function(fit) list(
    converged = TRUE, admissible = TRUE, note = NA_character_))
  reporting <- failure_env$build_cfa_reporting_data(models, failure_plan)
  expected <- vapply(models$fits, `[[`, integer(1), "n")
  expect_setequal(unique(expected), c(29L, 30L))
  expect_identical(reporting$model_status$n,
                   unname(expected[reporting$model_status$model]))
  scales <- failure_env$tabulate_cfa_scale_models(reporting, failure_book)
  sets <- failure_env$tabulate_cfa_set_models(reporting, failure_book, failure_plan)
  for (table in list(scales, sets)) {
    expect_identical(table$n, unname(expected[table$model]))
  }
})

test_that("the report states individual sample sizes after failures inside or outside the regression population", {
  lines <- readLines(file.path(failure_plan$root, "report", "results_draft.qmd"), warn = FALSE)
  start <- grep("^model_n <- function", lines)[1L]
  end <- grep("^# Participation [(]M4[)]", lines)[1L]
  code <- parse(text = lines[start:(end - 1L)])
  item <- failure_book$scales$item_codes[[match("asc_agg", failure_book$scales$scale_key)]][1]
  for (outside in c(FALSE, TRUE)) {
    example <- prepare_failed_imputation_example(if (outside) "demo_age" else item,
      missing_gender = if (outside) 1L else integer())
    network <- failure_env$select_network_input(example$prepared, failure_book, failure_plan)
    sizes <- failure_env$report_analysis_sample_sizes(list(n_analysis = 30L),
      failure_env$build_regression_input_reporting_data(example$regressions), network)
    flow <- failure_env$add_sample_reporting_facts(
      list(n_started = 30L, steps = tibble::tibble(criterion = character(), n_excluded = integer())),
      example$reporting, failure_plan)
    expect_true(flow$imputation$failed)
    environment <- list2env(list(report_sizes_value = sizes, report_flow_value = flow,
      n_analysis = 30L), parent = failure_env)
    eval(code, environment)
    expect_match(environment$sample_n_text, "excluded separately")
    if (outside) {
      expect_match(environment$sample_n_text, "each \\*N\\* = 29")
      expect_equal(environment$model_n(c("asc_agg", "asc_sub", "network")), c(29L, 29L, 30L))
    } else {
      expect_match(environment$sample_n_text, "\\*N\\* = 30")
      expect_equal(environment$model_n(c("asc_agg", "asc_sub", "network")), c(29L, 30L, 29L))
    }
    expect_false(grepl("samples were :", environment$sample_n_text, fixed = TRUE))
  }
})

test_that("CFA records keep a zero count when AP3 excludes every required respondent", {
  item <- failure_book$scales$item_codes[[1]][1]
  example <- prepare_failed_imputation_example(item, rows = 1:30)
  local_mocked_bindings(cfa = function(...) structure(list(), class = "lavaan"),
                        .package = "lavaan")
  models <- example$raw |>
    failure_env$define_subscale_models(failure_plan, failure_book) |>
    failure_env$define_ums_dopl_models(failure_plan, failure_book) |>
    failure_env$define_social_motives_model(failure_plan, failure_book) |>
    failure_env$define_asc_model(failure_plan, failure_book) |>
    failure_env$define_auth_orientation_model(failure_plan, failure_book) |>
    failure_env$fit_cfa_models(failure_book, failure_plan)
  for (record in models$fits) {
    affected <- item %in% record$columns
    expect_identical(record$n, if (affected) 0L else 30L)
    if (affected) {
      expect_false(record$succeeded)
      expect_null(record$fit)
      expect_match(record$error, "no respondents remain", fixed = TRUE)
      expect_identical(failure_env$extract_cfa_participant_count(record)$n, 0L)
    }
  }
})
