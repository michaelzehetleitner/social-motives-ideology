# The prior-recovery project (_targets_prior_recovery.R): its two entries in
# _targets.yaml, its targets, its one branching target, and the pipeline verbs
# its data sets and fits run through. Nothing is fitted.

local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) {
    parent <- dirname(dir)
    if (identical(parent, dir)) stop("project root not found from ", getwd())
    dir <- parent
  }
  source(file.path(dir, "R", "config.R"), local = FALSE)
})

recovery_project_root <- zm_root()

# The script is evaluated in a fresh environment, as tar_make() does, so the
# functions it sources land there.
recovery_env <- new.env(parent = globalenv())
recovery_manifest <- withr::with_dir(
  recovery_project_root,
  withr::with_envvar(
    c(TAR_PROJECT = "prior_recovery_smoke"),
    withCallingHandlers(
      targets::tar_manifest(script = "_targets_prior_recovery.R", callr_function = NULL,
                            envir = recovery_env),
      warning = function(w) {
        if (grepl("built under R version|unter R Version", conditionMessage(w))) {
          invokeRestart("muffleWarning")
        }
      }
    )
  )
)

recovery_command <- function(name) {
  command <- recovery_manifest$command[recovery_manifest$name == name]
  if (length(command) != 1L) stop("target '", name, "' not found exactly once in the manifest")
  gsub("\\s+", " ", command)
}

# The calls of a function body in the order they are written.
called_functions <- function(fun) {
  data <- utils::getParseData(parse(text = deparse(fun), keep.source = TRUE))
  data$text[data$token == "SYMBOL_FUNCTION_CALL"]
}

test_that("_targets.yaml runs the recovery in two projects with stores of their own", {
  projects <- yaml::read_yaml(file.path(recovery_project_root, "_targets.yaml"))
  expect_identical(projects$main, list(script = "_targets.R", store = "_targets"))
  expect_identical(projects$smoke, list(script = "_targets.R", store = "_targets_smoke"))
  expect_identical(projects$prior_recovery,
                   list(script = "_targets_prior_recovery.R", store = "_targets_prior_recovery"))
  expect_identical(projects$prior_recovery_smoke,
                   list(script = "_targets_prior_recovery.R", store = "_targets_prior_recovery_smoke"))
  stores <- vapply(projects, `[[`, character(1), "store")
  expect_equal(anyDuplicated(stores), 0L)
})

test_that("the recovery project lists its targets and branches once per task", {
  expect_setequal(recovery_manifest$name, c(
    "recovery_input_files", "recovery_plan", "recovery_codebook", "recovery_truth", "recovery_model_set",
    "recovery_scenario_coefficients", "recovery_tasks", "recovery_coefficient_rows", "recovery_scored_rows",
    "recovery_summary", "recovery_summary_file", "recovery_coefficients_file", "recovery_figure_file"
  ))
  patterns <- stats::setNames(recovery_manifest$pattern, recovery_manifest$name)
  expect_identical(unname(patterns["recovery_coefficient_rows"]), "map(recovery_tasks)")
  expect_true(all(is.na(patterns[names(patterns) != "recovery_coefficient_rows"])))
  # one branch: simulate, fit, summarise; the manifest deparses the chain as
  # nested calls, the last verb outermost
  branch <- recovery_command("recovery_coefficient_rows")
  positions <- vapply(c("summarise_recovery_fit(", "fit_recovery_regressions(", "simulate_recovery_dataset("),
                      function(call) regexpr(call, branch, fixed = TRUE), integer(1))
  expect_true(all(positions > 0L))
  expect_false(is.unsorted(positions))
  # the inputs are file dependencies every reader names
  for (reader in c("recovery_plan", "recovery_codebook", "recovery_truth")) {
    expect_match(recovery_command(reader), "recovery_input_files", fixed = TRUE, info = reader)
  }
})

test_that("no recovery target shares its name with a function", {
  functions <- names(Filter(isTRUE, eapply(recovery_env, is.function)))
  expect_true(all(c("fit_recovery_regressions", "fit_primary_regressions") %in% functions))
  expect_length(intersect(recovery_manifest$name, functions), 0L)
})

test_that("a simulated data set is prepared by the verbs that build data_regressions", {
  calls <- called_functions(recovery_env$simulate_recovery_dataset)
  route <- c("draw_recovery_scores", "add_age_and_income_bands", "set_gender_reference",
             "standardise_known_gender", "drop_rows_without_gender", "select_regression_input")
  expect_identical(calls, rev(route))
})

test_that("the recovery fits run through the pipeline's fitting and gate verbs and call brms nowhere", {
  fit_all <- recovery_env$fit_recovery_regressions
  fit_one <- recovery_env$fit_one_recovery_regression
  calls_all <- called_functions(fit_all)
  calls_one <- called_functions(fit_one)
  expect_equal(sum(calls_all == "fit_one_recovery_regression"), 1L)
  expect_equal(sum(calls_all == "extract_regression_coefficient_summaries"), 1L)
  for (verb in c("fit_primary_regressions", "fit_prior_width_comparisons")) {
    expect_equal(sum(calls_one == verb), 1L, info = verb)
  }
  expect_equal(sum(calls_one == "apply_validity_gate"), 2L)
  for (fun in list(fit_all, fit_one)) {
    expect_false(any(grepl("brms::", deparse(fun), fixed = TRUE)))
    expect_false(any(grepl("brm(", deparse(fun), fixed = TRUE)))
  }
})

test_that("only the sampler of one fit is caught; the branch's other steps let errors through", {
  # the fitting and gating of one regression is the one caught expression
  expect_equal(sum(called_functions(recovery_env$fit_one_recovery_regression) ==
                     "capture_recovery_sampler_error"), 1L)
  catchers <- c("tryCatch", "try", "withCallingHandlers", "capture_recovery_sampler_error")
  for (name in c("simulate_recovery_dataset", "fit_recovery_regressions", "summarise_recovery_fit")) {
    expect_identical(intersect(called_functions(recovery_env[[name]]), catchers), character(0), info = name)
  }
})
