local({
  root <- normalizePath(getwd())
  while (!file.exists(file.path(root, "config", "analysis_plan.yaml"))) root <- dirname(root)
  current <- new.env(parent = globalenv())
  for (file in list.files(file.path(root, "R"), pattern = "[.]R$", full.names = TRUE)) {
    sys.source(file, current)
  }
  sys.source(file.path(root, "tests", "support", "fake-brmsfit.R"), current)
  assign("iv_env", current, envir = globalenv())
})
iv_plan <- iv_env$zm_config("smoke")
iv_book <- iv_env$zm_codebook(iv_plan)

iv_diagnostic <- function(ess = TRUE, sampler = TRUE, bfmi = TRUE) {
  list(ok = ess && sampler && bfmi, ess_ok = ess, sampler_retry_needed = !sampler,
    ess_bulk_min = if (ess) 12000 else 20, ess_tail_min = if (ess) 11000 else 15,
    rhat_max = if (sampler) 1 else 1.2, n_divergent = if (sampler) 0L else 1L,
    n_treedepth_hits = 0L, bfmi_min = if (bfmi) .5 else .1,
    parameter_count = 10L, note = if (ess && sampler && bfmi) "ok" else "diagnostic failure")
}

test_that("imputation diagnostics include thresholds, scales and individual effects", {
  iv_env$fb_register_methods()
  variables <- c("b_Intercept[1]", "b_Intercept[2]", "sd_item__Intercept",
    "r_respondent_id[1,Intercept]", "sigma", "lp__", "lprior")
  fit <- iv_env$fb_fake_brmsfit(y ~ x, data.frame(y = 1:10, x = 1:10))
  fit$fake_draws <- iv_env$fb_draws_array(variables, rep(0, length(variables)),
    rep(1, length(variables)), 4L, 1000L, 123L)
  rlang::local_bindings(ap6_max_treedepth = function(fit) 10L, .env = iv_env)
  diagnosed <- iv_env$extract_imputation_diagnostics(fit, iv_plan)
  expect_true(diagnosed$ok)
  expect_equal(diagnosed$parameter_count, 5L)
  expect_setequal(diagnosed$parameters$variable, variables[1:5])
  # A badly mixing individual effect must fail even when population effects mix.
  fit$fake_draws[, 2L, "r_respondent_id[1,Intercept]"] <-
    fit$fake_draws[, 2L, "r_respondent_id[1,Intercept]"] + 5
  expect_false(iv_env$extract_imputation_diagnostics(fit, iv_plan)$ok)
  # Fixed ordinal discrimination is recorded by brmsterms as disc = 1.
  fit$formula <- brms::bf(y ~ x, family = brms::cumulative("logit"))
  fit$fake_draws <- iv_env$fb_draws_array(c(variables, "disc"), c(rep(0, length(variables)), 1),
    c(rep(1, length(variables)), 0), 4L, 1000L, 123L)
  ordinal <- iv_env$extract_imputation_diagnostics(fit, iv_plan)
  expect_true(ordinal$ok)
  expect_false("disc" %in% ordinal$parameters$variable)
})

test_that("AP3 follows ESS doublings then one sampler retry without model changes", {
  queue <- list(iv_diagnostic(FALSE, FALSE), iv_diagnostic(FALSE, FALSE),
    iv_diagnostic(TRUE, FALSE), iv_diagnostic())
  count <- 0L; calls <- list()
  rlang::local_bindings(extract_imputation_diagnostics = function(fit, analysis_plan) {
    count <<- count + 1L; queue[[count]]
  }, .env = iv_env)
  local_mocked_bindings(update = function(object, ...) {
    calls[[length(calls) + 1L]] <<- list(...); object
  }, .package = "stats")
  sampling <- iv_env$ap3_imputation_sampling(iv_plan)
  original <- list(formula = "unchanged model", predictors = "unchanged predictors", prior = "unchanged priors")
  result <- iv_env$gate_imputation_fit(original, sampling, iv_plan)
  expect_equal(result$status, "retried_ok")
  expect_identical(result$fit, original)
  expect_length(calls, 3L)
  expect_equal(vapply(calls, `[[`, numeric(1), "warmup"), rep(sampling$warmup, 3L) * c(1, 1, 2))
  expect_equal(vapply(calls, function(call) call$iter - call$warmup, numeric(1)),
    (sampling$iter - sampling$warmup) * c(2, 4, 4))
  expect_null(calls[[1]]$control)
  expect_equal(calls[[3]]$control, list(adapt_delta = .99, max_treedepth = 15L))
  expect_true(all(vapply(calls, function(call) isTRUE(call$recompile == FALSE), logical(1))))
  expect_false(any(c("formula", "data", "family", "prior") %in% names(calls[[3]])))
  expect_equal(nrow(result$history), 4L)
})

test_that("exhausted fits, fit errors and missing diagnostics never expose a predictive fit", {
  rlang::local_bindings(extract_imputation_diagnostics = function(fit, analysis_plan)
    iv_diagnostic(FALSE), .env = iv_env)
  updates <- 0L
  local_mocked_bindings(update = function(object, ...) {
    updates <<- updates + 1L; object
  }, .package = "stats")
  result <- iv_env$gate_imputation_fit(list(), iv_env$ap3_imputation_sampling(iv_plan), iv_plan)
  expect_equal(updates, 3L)
  expect_equal(result$status, "failed")
  expect_null(result$fit)
  error <- iv_env$gate_imputation_fit(stop("initial fit failed"),
    iv_env$ap3_imputation_sampling(iv_plan), iv_plan)
  expect_null(error$fit)
  expect_equal(error$diagnostics$note, "initial fit failed")
  expect_equal(nrow(error$history), 1L)
  rlang::local_bindings(extract_imputation_diagnostics = function(...) stop("missing diagnostics"),
    .env = iv_env)
  unavailable <- iv_env$gate_imputation_fit(list(), iv_env$ap3_imputation_sampling(iv_plan), iv_plan)
  expect_null(unavailable$fit)
  expect_match(unavailable$diagnostics$note, "diagnostics unavailable")
})

iv_frame <- function() {
  set.seed(23)
  data <- tibble::tibble(respondent_id = 1:30)
  for (item in unlist(iv_book$scales$item_codes)) data[[item]] <- sample(1:6, 30, TRUE)
  data$demo_age <- sample(seq.int(min(iv_plan$age_bands$first_years), iv_plan$age_bands$last_year), 30, TRUE)
  data$demo_hh_members <- sample(1:8, 30, TRUE)
  data$demo_income_hh_net <- sample(1:13, 30, TRUE)
  data$gender <- factor(rep(c("male", "female"), 15))
  data$known_gender <- TRUE
  data
}

test_that("failed item fills leave cells missing while eligible demographic fits are attempted", {
  data <- iv_frame()
  item <- iv_book$scales$item_codes[[1]][1]
  data[[item]][1] <- NA_real_
  data$demo_age[2] <- NA_real_
  original <- data
  calls <- list()
  local_mocked_bindings(brm = function(formula, data, ...) {
    calls[[length(calls) + 1L]] <<- list(formula = formula, data = data)
    stop("simulated fit failure")
  }, posterior_epred = function(...) stop("invalid fit reached prediction extraction"),
    posterior_predict = function(...) stop("invalid fit reached predictive interval"), .package = "brms")
  imputed <- iv_env$impute_items(data, iv_book, iv_plan)
  expect_identical(imputed[[item]], original[[item]])
  expect_equal(nrow(attr(imputed, "imputed_cells")), 0L)
  expect_equal(attr(imputed, "imputation_model_status")$status, "failed")
  predictors <- iv_env$select_demographic_imputation_predictors(data, iv_book, iv_plan)
  imputed <- iv_env$impute_age(imputed, iv_book, iv_plan, predictors$demo_age)
  expect_true(is.na(imputed$demo_age[2]))
  expect_equal(attr(imputed, "imputation_model_status")$status, c("failed", "failed"))
  expect_length(calls, 2L)
  expect_equal(nrow(calls[[2]]$data), 28L)
  expect_false(anyNA(calls[[2]]$data))
  expect_equal(nrow(imputed), nrow(original))
  reporting <- iv_env$build_imputation_reporting_data(imputed, tibble::tibble())
  expect_equal(reporting$n_unfilled, 2L)
  expect_match(iv_env$describe_imputation_processing(reporting), "remain missing")
  expect_false(grepl("No value had to be filled", iv_env$describe_imputation_processing(reporting)))
})

test_that("failed demographics do not suppress complete scale/network input", {
  data <- iv_frame(); data$demo_age[1] <- NA_real_
  result <- list(status = "failed", diagnostics = iv_diagnostic(FALSE), history = tibble::tibble())
  data <- iv_env$record_imputation_model_status(data, result, "demo_age", "demographic", "demo_age", 1L)
  data <- iv_env$identify_unavailable_imputation_variables(data, iv_book)
  data <- iv_env$average_items_into_subscales(data, iv_book)
  data$age <- data$demo_age; data$income <- seq(250, 4000, length.out = nrow(data))
  data <- iv_env$add_age_and_income_bands(data, iv_plan)
  prepared <- iv_env$standardise_all(data, iv_book, iv_plan)
  prepared <- iv_env$standardise_known_gender(prepared, iv_book, iv_plan)
  age_column <- prepared[[iv_env$zm_z_col("age", iv_book, "known_gender")]]
  expect_true(is.na(age_column[1]))
  expect_true(all(is.finite(age_column[-1])))
  expect_equal(mean(age_column[-1]), 0, tolerance = 1e-12)
  expect_equal(stats::sd(age_column[-1]), 1)
  network <- iv_env$select_network_input(prepared, iv_book, iv_plan)
  expect_equal(nrow(iv_env$define_network_model(network, iv_plan)$data), nrow(data))
  regressions <- iv_env$drop_rows_without_gender(prepared)
  regressions <- iv_env$select_regression_input(regressions, iv_book, iv_plan)
  expect_null(iv_env$describe_unavailable_imputation_inputs(regressions, "age_z"))
  expect_identical(regressions$respondent_id, 2:30)
  expect_equal(iv_env$build_regression_input_reporting_data(regressions)$n_used, 29L)
  expect_identical(attr(regressions, "excluded_imputation")$respondent_id, 1L)
})

test_that("failure status reports analysis-specific exclusions and replaces stale results", {
  data <- iv_frame(); data$demo_age[1] <- NA_real_
  data <- iv_env$record_imputation_model_status(data,
    list(status = "failed", diagnostics = iv_diagnostic(FALSE), history = tibble::tibble()),
    "demo_age", "demographic", "demo_age", 1L)
  data <- iv_env$identify_unavailable_imputation_variables(data, iv_book)
  attr(data, "imputed_cells") <- iv_env$ap3_fill_empty_cells()
  reporting <- iv_env$build_imputation_reporting_data(data, tibble::tibble())
  config <- iv_plan; config$root <- tempfile("imputation-report-")
  dir.create(file.path(config$root, "report"), recursive = TRUE)
  old_path <- file.path(config$root, "report", "results_draft.html")
  writeLines("old successful report", old_path)
  path <- iv_env$write_imputation_processing_report(reporting, iv_book, config)
  status <- paste(readLines(path), collapse = "\n")
  expect_match(status, "unresolved AP3 values")
  expect_match(status, "Available after AP3 exclusions")
  expect_match(status, "Not withheld by AP3")
  expect_match(status, "Respondents with unresolved values were excluded")
  expect_identical(readLines(path), readLines(old_path))
  expect_false(grepl("old successful report|No value had to be filled", status))
})

test_that("failed item fills retain complete cases separately for each measurement analysis", {
  data <- iv_frame()
  scale <- iv_book$scales$scale_key[1]
  items <- iv_book$scales$item_codes[[1]]
  data[[items[1]]][1] <- NA_real_
  data <- iv_env$record_imputation_model_status(data,
    list(status = "failed", diagnostics = iv_diagnostic(FALSE), history = tibble::tibble()),
    scale, "item", items, 1L)
  data <- iv_env$identify_unavailable_imputation_variables(data, iv_book)
  omega_calls <- 0L; alpha_calls <- 0L; correlation_calls <- 0L
  local_mocked_bindings(
    omega = function(m, ...) {
      expect_false(anyNA(m)); omega_calls <<- omega_calls + 1L
      loadings <- matrix(1, nrow = ncol(m), ncol = 1L, dimnames = list(names(m), "g"))
      list(omega.tot = .8, schmid = list(sl = loadings))
    },
    alpha = function(x, ...) {
      expect_false(anyNA(x)); alpha_calls <<- alpha_calls + 1L
      list(total = list(raw_alpha = .7))
    },
    polychoric = function(x, ...) {
      expect_false(anyNA(x)); correlation_calls <<- correlation_calls + 1L
      list(rho = diag(ncol(x)))
    }, .package = "psych")
  reliability <- iv_env$estimate_reliability_coefficients(data, iv_book)
  expect_equal(omega_calls, 9L)
  expect_equal(alpha_calls, 9L)
  expect_equal(reliability$scales[[scale]]$coefficients$alpha, .7)
  expect_equal(nrow(reliability$scales[[scale]]$responses), 29L)
  expect_true(all(vapply(reliability$scales[setdiff(names(reliability$scales), scale)],
    function(one) nrow(one$responses) == 30L, logical(1))))
  correlation_calls <- 0L
  sets <- iv_env$define_efa_item_sets(data, iv_plan, iv_book)
  correlations <- iv_env$estimate_polychoric_correlations(sets)
  affected <- vapply(sets, function(set) any(items %in% names(set$data)), logical(1))
  expect_true(any(affected))
  expect_equal(correlation_calls, length(sets))
  expect_true(all(vapply(sets[affected], function(set) nrow(set$data) == 29L, logical(1))))
  expect_true(all(vapply(sets[!affected], function(set) nrow(set$data) == 30L, logical(1))))
  expect_true(all(vapply(correlations, function(set) set$correlation$ok, logical(1))))
})

test_that("completed data without fill metadata do not acquire an invented successful validity gate", {
  reporting <- list(imputed_cells = tibble::tibble(value = 3), model_status = NULL,
    n_unfilled = 0L, unavailable_variables = character())
  description <- iv_env$describe_imputation_processing(reporting)
  expect_match(description, "metadata are unavailable")
  expect_false(grepl("passed|No value had to be filled", description))
})

test_that("failed cells outside the known-gender population do not block its regressions", {
  for (kind in c("demographic", "item")) {
    data <- iv_frame()
    raw <- if (kind == "demographic") "demo_age" else iv_book$scales$item_codes[[1]][1]
    variables <- if (kind == "demographic") raw else iv_book$scales$item_codes[[1]]
    data[[raw]][1] <- NA_real_
    data$gender[1] <- NA; data$known_gender[1] <- FALSE
    attr(data, "imputed_cells") <- iv_env$ap3_fill_empty_cells()
    data <- iv_env$record_imputation_model_status(data,
      list(status = "failed", diagnostics = iv_diagnostic(FALSE), history = tibble::tibble()),
      raw, kind, variables, 1L)
    data <- iv_env$identify_unavailable_imputation_variables(data, iv_book)
    reporting <- iv_env$build_imputation_reporting_data(data, tibble::tibble())
    data <- iv_env$average_items_into_subscales(data, iv_book)
    data$age <- data$demo_age; data$income <- seq(250, 4000, length.out = nrow(data))
    data <- iv_env$add_age_and_income_bands(data, iv_plan)
    data <- iv_env$standardise_all(data, iv_book, iv_plan)
    data <- iv_env$standardise_known_gender(data, iv_book, iv_plan)
    regressions <- iv_env$drop_rows_without_gender(data)
    regressions <- iv_env$select_regression_input(regressions, iv_book, iv_plan)
    expect_null(iv_env$describe_unavailable_imputation_inputs(regressions, names(regressions)))
    expect_false(anyNA(regressions))
    expect_equal(iv_env$build_regression_input_reporting_data(regressions)$n_used, 29L)
    expect_true(all(is.finite(attr(regressions, "z_parameters")$sd)))
    availability <- iv_env$tabulate_imputation_analysis_availability(reporting, iv_book, iv_plan)
    expect_true(all(availability$status[grepl("Adjusted regression|Joint regression", availability$analysis)] ==
      "Not withheld by AP3"))
    if (kind == "item") {
      network <- iv_env$select_network_input(data, iv_book, iv_plan)
      expect_equal(nrow(iv_env$define_network_model(network, iv_plan)$data), 29L)
    }
  }
})


test_that("the smoke project selects reduced AP3 and AP6 sampling unless overridden", {
  withr::local_envvar(c(TAR_PROJECT = "smoke", ZM_PROFILE = NA_character_))
  plan <- iv_env$zm_config()
  expect_identical(plan$profile_name, "smoke")
  sampling <- iv_env$ap3_imputation_sampling(plan)
  expect_equal(sampling$warmup, 500L)
  expect_equal(sampling$iter - sampling$warmup, 1000L)
  expect_equal(iv_env$ap6_gate_settings(plan$regression)$ess_target, 2000L)
  files <- list(analysis_plan = file.path(iv_plan$root, "config", "analysis_plan.yaml"))
  expect_identical(iv_env$read_and_check_analysis_plan(files)$profile_name, "smoke")
  withr::local_envvar(c(ZM_PROFILE = "full"))
  expect_identical(iv_env$zm_config()$profile_name, "full")
  # Intake receives the pipeline's profile explicitly even if a worker has a different environment.
  expect_identical(iv_env$read_and_check_analysis_plan(files, profile = "smoke")$profile_name, "smoke")
  sampling_full <- iv_env$ap3_imputation_sampling(iv_env$zm_config("full"))
  expect_equal(sampling_full$warmup, 2000L)
  expect_equal(sampling_full$iter - sampling_full$warmup, 6000L)
})

test_that("smoke imputation retries stay on smoke warmup and draw counts", {
  rlang::local_bindings(extract_imputation_diagnostics = function(fit, analysis_plan)
    iv_diagnostic(FALSE, FALSE), .env = iv_env)
  calls <- list()
  local_mocked_bindings(update = function(object, ...) {
    calls[[length(calls) + 1L]] <<- list(...); object
  }, .package = "stats")
  result <- iv_env$gate_imputation_fit(list(), iv_env$ap3_imputation_sampling(iv_plan), iv_plan)
  expect_equal(result$history$warmup, c(500, 500, 500, 500, 1000))
  expect_equal(result$history$iter_per_chain, c(1000, 2000, 4000, 8000, 8000))
  expect_equal(result$status, "failed")
  expect_equal(calls[[4L]]$control, list(adapt_delta = .99, max_treedepth = 15L))
})
