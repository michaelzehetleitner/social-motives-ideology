local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "R", "classify_importance_sampling.R"))) dir <- dirname(dir)
  for (file in c("config.R", "ap6_regressions.R", "ap10_inference.R", "classify_importance_sampling.R",
                 "report_supplement_prior_sensitivity.R", "report_helpers.R")) {
    source(file.path(dir, "R", file), local = FALSE)
  }
  assign("power_gate_root", dir, envir = .GlobalEnv)
})

test_that("reliability uses draw-dependent limits and does not trust missing diagnostics", {
  expect_equal(calculate_importance_sampling_threshold(100), 0.5)
  expect_equal(calculate_importance_sampling_threshold(4000), 0.7)
  expect_identical(classify_importance_sampling(0.49, 100), "reliable")
  expect_identical(classify_importance_sampling(0.5, 100), "unreliable")
  expect_identical(classify_importance_sampling(0.69, 4000), "reliable")
  expect_identical(classify_importance_sampling(0.7, 4000), "unreliable")
  expect_identical(classify_importance_sampling(Inf, 4000), "unavailable")
  expect_identical(classify_importance_sampling(NA_real_, 4000), "unavailable")
  expect_identical(classify_importance_sampling(0.1, NULL), "unavailable")
})

test_that("unreliable checks cannot give a reassuring or conflict summary", {
  ps <- tibble::tibble(outcome = c("a", "b", "c"), block = "all_priors",
    fit_valid = TRUE, fit_gate_status = "ok",
    term = "x", diagnosis = "potential prior-data conflict",
    importance_sampling_status = c("reliable", "unreliable", "unavailable"))
  answer <- check_powerscale_outcome(ps)
  expect_identical(answer$n_flagged, 1L)
  expect_true(answer$any_successful)
  expect_equal(nrow(answer$unreliable_blocks), 1)
  expect_equal(nrow(answer$unavailable_blocks), 1)
  expect_false(check_powerscale_outcome(ps[-1, ])$any_successful)
  expect_identical(check_powerscale_outcome(ps[-1, ])$n_flagged, 0L)
})

test_that("power scaling needs both original-fit validity and reliable importance sampling", {
  cases <- tibble::tibble(
    fit_valid = c(TRUE, TRUE, FALSE, NA, TRUE, TRUE, TRUE),
    fit_gate_status = c("ok", "retried_ok", "not_interpretable", "unavailable", "not_interpretable", "ok", "ok"),
    importance_sampling_status = c("reliable", "reliable", "reliable", "reliable", "reliable", "unreliable", "unavailable")
  )
  expect_identical(do.call(can_interpret_power_scaling, cases), c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE))
})

power_gate_plan <- zm_config("full", file.path(power_gate_root, "config", "analysis_plan.yaml"))
power_gate_codebook <- zm_codebook(power_gate_plan)
power_gate_fits <- stats::setNames(lapply(seq_len(4), function(i) {
  fit <- list()
  if (i < 4L) {
    attr(fit, "gate_status") <- c("ok", "retried_ok", "not_interpretable")[i]
    attr(fit, "diagnostics") <- list(ok = i < 3L)
  }
  fit
}), ap6_regression_outcome_identity(power_gate_plan)$outcome)
power_gate_raw <- list(
  settings = list(components = c("prior", "likelihood"), lower_alpha = .99,
    upper_alpha = 1.01, div_measure = "cjs_dist", sensitivity_threshold = .05),
  priorsense_version = "test",
  blocks = stats::setNames(lapply(seq_len(3), function(i) list(
    block = c("metric_slopes", "intercept", "sigma")[i], prior_selection = "test",
    matrix = tibble::tibble(variable = c("b_zm_security_z", "bayes_R2"), prior = c(.09, .08),
      likelihood = c(.08, .07), diagnosis = "potential prior-data conflict"),
    sensitivity = .09, pareto_k_max = c(.4, .7, NA)[i], importance_sampling_draws = 4000L
  )), c("metric_slopes", "intercept", "sigma"))
)
power_gate_result <- local({
  rlang::local_bindings(ap6_run_priorsense = function(...) power_gate_raw,
    .env = environment(extract_regression_prior_sensitivity))
  extract_regression_prior_sensitivity(power_gate_fits, power_gate_plan)
})
power_gate_table <- tabulate_power_scaling(power_gate_result, power_gate_plan)

test_that("the producer and reporting retain failed-fit numbers but suppress interpretation", {
  expect_equal(vapply(power_gate_result, `[[`, logical(1), "fit_valid"),
    stats::setNames(c(TRUE, TRUE, FALSE, NA), names(power_gate_result)))
  for (i in seq_along(power_gate_result)) {
    summary <- power_gate_result[[i]]
    expect_identical(summary$sensitivity, power_gate_raw)
    interpretable <- vapply(summary$blocks, `[[`, logical(1), "interpretable")
    expect_identical(unname(interpretable), if (i < 3L) c(TRUE, FALSE, FALSE) else rep(FALSE, 3))
  }
  expect_equal(sum(power_gate_table$interpretable), 4L)
  expect_true(all(is.finite(power_gate_table$prior_sens)))
  expect_identical(unique(power_gate_table$diagnosis_raw), "potential prior-data conflict")
  expect_true(all(grepl("^not interpretable:", power_gate_table$diagnosis[!power_gate_table$interpretable])))
  answer <- check_powerscale_outcome(power_gate_table)
  expect_identical(answer$failed_fit_models, "asc_conv")
  expect_identical(answer$unavailable_fit_models, "sdo_dom")
  expect_equal(answer$n_flagged, 4L)
  expect_true(answer$any_successful)
  failed <- power_gate_table[!power_gate_table$fit_valid %in% TRUE, ]
  answer <- check_powerscale_outcome(failed)
  expect_identical(answer$n_flagged, 0L)
  expect_false(answer$any_successful)
  old <- power_gate_result
  for (i in seq_along(old)) {
    old[[i]]$fit_valid <- NULL
    old[[i]]$fit_gate_status <- NULL
  }
  expect_false(any(tabulate_power_scaling(old, power_gate_plan)$interpretable))
})

test_that("the displayed diagnoses cannot imply conflict or reassurance for a failed fit", {
  failed <- power_gate_table[power_gate_table$fit_valid %in% FALSE, ]
  rendered <- paste(as.character(rh_priorsense_table(failed,
    labels = rh_labels(power_gate_codebook), result_file = "power_scaling.csv", engine = "kable")), collapse = "\n")
  expect_match(rendered, "Original fit validity", fixed = TRUE)
  expect_match(rendered, "not interpretable: original fit failed validity gate", fixed = TRUE)
  expect_false(grepl("potential prior-data conflict", rendered, fixed = TRUE))
  expect_match(rendered, ".09", fixed = TRUE)
  expect_match(rendered, "Sensitivity values from failed fits are retained as diagnostics.", fixed = TRUE)
})

test_that("the report describes failed-fit sensitivity as uninterpretable", {
  failed <- power_gate_table[power_gate_table$fit_valid %in% FALSE, ]
  env <- new.env(parent = environment())
  env$s3_value <- add_prior_reporting_facts(list(
    priorsense = failed, answer = check_powerscale_outcome(failed),
    coefficients_primary = tibble::tibble(term_type = character()),
    powerscale = list(n_flagged = 0L, prior_max = NA_real_, lik_max = NA_real_)), power_gate_plan)
  expect_identical(env$s3_value$answer$n_unreliable_blocks, 1L)
  expect_identical(env$s3_value$answer$n_unavailable_blocks, 1L)
  expect_identical(env$s3_value$answer$n_unreliable, 2L)
  env$labels <- rh_labels(power_gate_codebook)
  env$analysis_plan <- power_gate_plan
  env$powerscale <- env$s3_value$powerscale
  env$join_and <- function(x) paste(x, collapse = " and ")
  env$sec_link <- identity
  lines <- readLines(file.path(power_gate_root, "report", "results_draft.qmd"), warn = FALSE)
  evaluate <- function(name) {
    start <- which(lines == paste0(name, " <- local({"))
    end <- which(seq_along(lines) > start & lines == "})")[1]
    eval(parse(text = lines[start:end]), env)
  }
  main <- evaluate("power_scaling_text")
  expect_match(main, "failed the validity gate; their power-scaling diagnoses are not interpreted", fixed = TRUE)
  expect_match(main, "No reliable power-scaling diagnosis was available", fixed = TRUE)
  expect_false(grepl("flagged no coefficient", main, fixed = TRUE))
  expect_false(grepl("possible prior–data conflict", main, fixed = TRUE))
  supplement <- evaluate("suppl_power_scaling_text")
  expect_match(supplement, "provided no interpretable sensitivity estimates", fixed = TRUE)
  expect_match(supplement, "Diagnostic values are in", fixed = TRUE)
})
