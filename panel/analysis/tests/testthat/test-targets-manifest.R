# _targets.R: the pipeline map lists the contract targets (intake gate,
# preparation and imputation, data files, supplements, joint estimation), the
# intake gate precedes every analysis step, the preparation and regression
# routes carry no literal but a role key, the report reads only targets that
# exist, the input, codebook and card files are file dependencies, and no
# target shares its name with a pipeline function

local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) {
    parent <- dirname(dir)
    if (identical(parent, dir)) stop("project root not found from ", getwd())
    dir <- parent
  }
  source(file.path(dir, "R", "config.R"), local = FALSE)
})

root <- zm_root()

# The script is evaluated in a fresh environment (as tar_make() does in its own
# session): the pipeline functions land there, so the name check below sees
# exactly the functions targets sees.
pipeline_env <- new.env(parent = globalenv())
manifest <- withr::with_dir(
  root,
  withr::with_envvar(
    c(ZM_PROFILE = "smoke", ZM_DATA = "synthetic"),
    withCallingHandlers(
      targets::tar_manifest(callr_function = NULL, envir = pipeline_env),
      warning = function(w) {
        # library() notes about packages built under a newer R than the running
        # one are not pipeline warnings; everything else stays visible
        if (grepl("built under R version|unter R Version", conditionMessage(w))) {
          invokeRestart("muffleWarning")
        }
      }
    )
  )
)

# the command as one line: tar_manifest() deparses long commands over several lines
command_of <- function(name) {
  cmd <- manifest$command[manifest$name == name]
  if (length(cmd) != 1) stop("target '", name, "' not found exactly once in the manifest")
  gsub("\\s+", " ", cmd)
}
# the command as it was deparsed, parseable again (command_of() folds the
# newlines of a multi-statement target body into single spaces)
expression_of <- function(name) {
  cmd <- manifest$command[manifest$name == name]
  if (length(cmd) != 1) stop("target '", name, "' not found exactly once in the manifest")
  parse(text = cmd)[[1]]
}

# The AP3 imputation, in the order it runs: the gender reference is fixed on the
# retained population before gender can serve as a predictor, then the items,
# then the three demographics, each using the ones imputed before it.
ap3_fill_stages <- c("set_gender_reference", "impute_items", "impute_age",
                     "impute_household_size", "impute_income_band")

# The functions of a piped chain in the order they run. `a |> f() |> g()` is
# the call g(f(a)), so the chain is read from the innermost call outwards.
call_chain <- function(expr) {
  # A target whose command is a `{` block states one chain; read that statement.
  while (is.call(expr) && identical(expr[[1]], as.name("{"))) expr <- expr[[length(expr)]]
  names <- character()
  # `bundle$element` is the chain's input, not a verb of the chain.
  while (is.call(expr) && length(expr) >= 2L && !identical(expr[[1]], as.name("$"))) {
    names <- c(as.character(expr[[1]]), names)
    expr <- expr[[2]]
  }
  names
}

test_that("tar_manifest() lists the contract targets", {
  expect_true(is.data.frame(manifest))
  expect_true(all(c("name", "command") %in% names(manifest)))
  expect_equal(anyDuplicated(manifest$name), 0)
  for (t in c(
    "pipeline_config", "analysis_plan_file", "parameter_card_file", "raw_path", "supplement_model_checks",
    "regression_prediction_decisions", "bayesian_score_correlations",
    "report_network", "report_inputs_file", "results_draft",
    # intake gate and the prepared analysis inputs
    "codebook_files", "intake_approval_file",
    "analysis_input_files", "analysis_input_paths", "analysis_inputs",
    "study_analysis_ready", "data_descriptive_reliability", "data_network", "data_regressions",
    # AP3 imputation: the split preparation, its reporting objects and the
    # files of its two records
    "imputation_demographic_predictors", "imputation_reporting_data", "preparation_reporting_data",
    "regression_input_reporting_data",
    "filled_cells_file", "dropped_respondents_file",
    "demographics_file", "scientific_use_file", "data_files_summary",
    "supplement_sampling_diagnostics", "supplement_explained_variance", "supplement_prior_sensitivity",
    "supplement_software",
    # AP4/AP9 factor-analytic sets (analysis_plan$factor_analysis$sets)
    "confirmatory_factor_structure_residuals_file", "supplement_measurement",
    "supplement_network_detail",
    # The joint estimation for RQ2 (multivariate model, correlated residuals)
    # and its report target.
    "joint_regression_fit", "joint_correlation_changes", "joint_asc_aggregation",
    "report_joint"
  )) {
    expect_true(t %in% manifest$name, info = t)
  }
})

test_that("the Bayesian correlations use the shared descriptive sample and current plan", {
  cmd <- command_of("bayesian_score_correlations")
  expect_match(cmd, "calculate_bayesian_score_correlations", fixed = TRUE)
  expect_match(cmd, "data_descriptive_reliability", fixed = TRUE)
  expect_match(cmd, "analysis_inputs$codebook", fixed = TRUE)
  expect_match(cmd, "analysis_plan", fixed = TRUE)
  expect_false(grepl("data_regressions|joint_regression_fit", cmd))
})

test_that("the intake gate precedes every analysis step", {
  # One file target selects every tracked input; the checked bundle reads it.
  expect_match(command_of("analysis_input_files"), "check_run_spec(run_spec)", fixed = TRUE)
  expect_match(command_of("analysis_input_files"), "select_analysis_input_files(run$data_source, root, analysis_plan$meta$codebook_dir)",
               fixed = TRUE)
  # targets returns the file target's paths unnamed; the named vector every
  # reader indexes is the projection `analysis_input_paths`.
  expect_match(command_of("analysis_input_paths"), "zm_name_input_files(analysis_input_files)", fixed = TRUE)
  # The plan is read for the profile the pipeline runs under.
  for (step in c("read_and_check_analysis_plan(analysis_input_paths, profile = pipeline_config$profile_name)",
                 "read_and_check_codebook(analysis_input_paths, analysis_plan)",
                 "check_intake_approval(analysis_input_paths, source, analysis_plan)",
                 "read_clean_study_data(analysis_input_paths)",
                 "add_col_types(", "add_factor_levels(", "check_study_data_matches_approval(")) {
    expect_match(command_of("analysis_inputs"), step, fixed = TRUE)
  }
  # The reader names the file target itself: the paths stay equal when only a
  # file's content changes, so without it a changed plan would not be re-read.
  expect_match(command_of("analysis_inputs"), "\\banalysis_input_files\\b", perl = TRUE)
  # The path and approval targets read those two targets, never the intake again.
  expect_match(command_of("raw_path"), 'analysis_input_paths[["data"]]', fixed = TRUE)
  expect_match(command_of("intake_approval_file"), 'analysis_input_paths[["receipt"]]', fixed = TRUE)
  expect_false(any(grepl("zm_read_clean_intake|zm_clean_intake_paths", manifest$command)))
  expect_false(any(grepl("read_qualtrics_export|ap3_intake_apply|ap3_intake_write_proposal", manifest$command)))
  expect_match(command_of("study_analysis_ready"),
    "restore_prepared_analysis_data(analysis_inputs$data, analysis_inputs$preparation, analysis_inputs$config)", fixed = TRUE)
  for (name in c("study_analysis_ready", "data_descriptive_reliability", "data_network", "data_regressions")) {
    read <- targets::tar_deps_raw(expression_of(name))
    expect_false(any(c("analysis_plan", "codebook") %in% read), info = name)
  }
  expect_match(command_of("data_descriptive_reliability"),
               "select_descriptive_reliability_input(study_analysis_ready, analysis_inputs$codebook)", fixed = TRUE)
  expect_match(command_of("data_network"),
               "select_network_input(study_analysis_ready, analysis_inputs$codebook, analysis_inputs$config)", fixed = TRUE)
  expect_match(command_of("data_regressions"),
               "select_regression_input(drop_rows_without_gender(study_analysis_ready), analysis_inputs$codebook, analysis_inputs$config)",
               fixed = TRUE)
})

test_that("targets consume the saved AP1/AP3 preparation and its reporting records", {
  # Scientific fitting/order is exercised by the intake fixtures. Targets
  # consume the completed values and records, then standardise each population.
  expect_match(command_of("imputation_demographic_predictors"),
    "analysis_inputs$preparation$imputation_demographic_predictors", fixed = TRUE)
  expect_match(command_of("imputation_reporting_data"), "analysis_inputs$preparation$imputation", fixed = TRUE)
  expect_match(command_of("preparation_reporting_data"),
    "build_preparation_reporting_data(study_analysis_ready, analysis_inputs$preparation$exclusions, imputation_reporting_data)", fixed = TRUE)
  expect_match(command_of("regression_input_reporting_data"), "build_regression_input_reporting_data(data_regressions)", fixed = TRUE)
  expect_match(command_of("filled_cells_file"),
    "write_filled_cells_file(build_filled_cells_table(imputation_reporting_data), analysis_plan)", fixed = TRUE)
  expect_match(command_of("dropped_respondents_file"),
    "write_dropped_respondents_file(build_dropped_respondents_table(imputation_reporting_data, analysis_inputs$config), analysis_plan)", fixed = TRUE)
  expect_identical(call_chain(expression_of("study_analysis_ready")),
    c("restore_prepared_analysis_data", "standardise_all", "standardise_known_gender"))
  expect_match(command_of("report_participant_flow"), "report_exclusion_steps(preparation_reporting_data, imputation_reporting_data)", fixed = TRUE)
  expect_identical(call_chain(expression_of("report_participant_flow")),
    c("report_exclusion_steps", "add_sample_reporting_facts"))
  for (verb in c("apply_study_exclusions", "impute_items", "impute_age", "impute_household_size", "impute_income_band"))
    expect_false(any(grepl(paste0(verb, "("), manifest$command, fixed = TRUE)), info = verb)
  expect_match(command_of("analysis_inputs"), "read_prepared_demographics(approval)", fixed = TRUE)
  expect_match(command_of("analysis_inputs"), 'readRDS(analysis_input_paths[["preparation"]])', fixed = TRUE)
})
# The preparation and regression routes name verbs and targets, never a
# threshold, formula or prose string. The only literals a command of these
# routes may carry are the role keys of the plan.
preparation_route <- c("imputation_demographic_predictors", "study_analysis_ready",
                       "imputation_reporting_data", "preparation_reporting_data",
                       "regression_input_reporting_data",
                       "data_descriptive_reliability", "data_network", "data_regressions")
regression_route <- c("regression_model_set", "regression_primary_fits",
                      "regression_prior_width_fits", "regression_predictive_checks",
                      "regression_student_t_robustness", "regression_fit_register",
                      "regression_coefficient_summaries", "regression_r2_summaries",
                      "regression_prior_width_sensitivity", "regression_prior_sensitivity",
                      "regression_likelihood_robustness",
                      "report_primary_coefficients", "report_joint", "supplement_software")
allowed_literals <- c("primary", "sweep", "student", "student_refit", "multivariate")

string_literals <- function(expr) {
  if (is.character(expr)) return(as.character(expr))
  if (!is.call(expr) && !is.pairlist(expr)) return(character())
  unlist(lapply(as.list(expr), string_literals), use.names = FALSE)
}

test_that("the preparation and regression routes carry no string literal but a role key", {
  for (name in c(preparation_route, regression_route)) {
    literals <- unique(string_literals(expression_of(name)))
    expect_true(all(literals %in% allowed_literals),
                info = paste(name, ":", paste(setdiff(literals, allowed_literals), collapse = ", ")))
  }
})

test_that("the prepared inputs feed the regressions and the data files", {
  expect_match(command_of("regression_primary_fits"), "data_regressions", fixed = TRUE)
  expect_match(command_of("scientific_use_file"), 'analysis_input_paths[["data"]]', fixed = TRUE)
  expect_match(command_of("demographics_file"), 'analysis_input_paths[["demographics"]]', fixed = TRUE)
  for (dependency in c("scientific_use_file", "demographics_file", "filled_cells_file", "dropped_respondents_file"))
    expect_match(command_of("data_files_summary"), dependency, fixed = TRUE)
})
test_that("primary fitting explicitly receives the configured supported family", {
  define_fun <- get("define_regression_model_set", envir = pipeline_env)
  analysis_plan <- zm_config(profile = "full")
  codebook <- zm_codebook(analysis_plan)
  model_set <- define_fun(analysis_plan)
  formula <- model_set$formulas[[match("asc_agg", model_set$outcome_keys)]]
  expect_identical(paste(deparse(formula, width.cutoff = 500L), collapse = ""),
                   paste0(zm_z_col("asc_agg", codebook), " ~ ", analysis_plan$regression$predictors))
  expect_identical(environment(formula), globalenv())
  # The configured family itself is checked once, where the configuration is
  # loaded (zm_config()).
  expect_identical(model_set$family$family, analysis_plan$regression$family)
})

test_that("the S4 comparison of the two likelihoods reads the pipeline's pairing", {
  expect_match(command_of("supplement_model_checks"), "regression_likelihood_robustness", fixed = TRUE)
  s4_source <- paste(readLines(file.path(root, "R", "report_supplement_model_checks.R"), warn = FALSE),
                     collapse = "\n")
  expect_match(s4_source, "tabulate_likelihood_comparison(regression_likelihood_robustness", fixed = TRUE)
})

test_that("the joint model's report targets read the gated fit and the three packages", {
  # RQ2 reads the three packages, never the fit directly
  expect_match(command_of("report_joint"),
               "assemble_report_joint(joint_within_asc, joint_asc_components_sdo, joint_correlation_changes, analysis_plan)",
               fixed = TRUE)
  # the joint fit has a provenance row of its own
  expect_match(command_of("supplement_software"), "joint_regression_fit", fixed = TRUE)
  # the report states the commit of the code that produced it (M1, T10), from the receipt
  expect_match(command_of("report_provenance"),
               "assemble_report_provenance(calculation_receipt, data_source_used)", fixed = TRUE)
})

test_that("the report-side evidence and companion-file targets are wired", {
  # The deviations register, the companion file of the shared data set and the
  # measurement summary-statistics files.
  for (t in c("filled_cells_file", "deviations_register", "scientific_use_readme", "ap4_summary_files")) {
    expect_true(t %in% manifest$name, info = t)
  }
  expect_match(command_of("deviations_register"), "zm_deviations_table()", fixed = TRUE)
  expect_match(command_of("scientific_use_readme"),
               "zm_scientific_use_readme(data_files_summary, analysis_plan)", fixed = TRUE)
  expect_match(command_of("ap4_summary_files"),
               "write_measurement_summary_files(data_descriptive_reliability, codebook, analysis_plan)", fixed = TRUE)
  expect_match(command_of("data_files_summary"),
               "zm_data_files_summary(scientific_use_file, demographics_file,",
               fixed = TRUE)
})

test_that("the AP4/AP9 factor-analytic sets run on the reversed items and keep the per-scale targets", {
  # the per-scale reliability, CFA and EFA targets run beside the sets
  for (t in c("scale_reliability", "confirmatory_factor_structure_reporting_data", "efa_fits")) {
    expect_true(t %in% manifest$name, info = t)
  }
  # The S1 target builds the EFA tables from the SRQ1 route; that wiring is
  # asserted in test-targets-efa.R. The set CFAs are fixed model definitions
  # fitted on the rounded CFA input; test-targets-cfa.R carries the CFA wiring
  # assertions.
  expect_match(command_of("supplement_measurement"), "confirmatory_factor_structure_reporting_data",
               fixed = TRUE)
  # the correspondence is the route's loading-clarity result, laid out for S1
  expect_match(command_of("supplement_measurement"), "efa_loading_clarity", fixed = TRUE)
})

# The wiring of the network route — its producers and its report targets — is
# asserted in test-targets-network.R, with that slice's other contracts.

test_that("the report target tracks the R files the report sources", {
  find <- get("find_report_source_files", envir = pipeline_env)
  files <- withr::with_dir(root, find("report/results_draft.qmd"))
  qmd <- readLines(file.path(root, "report", "results_draft.qmd"), warn = FALSE)
  sourced <- sum(grepl('source(file.path(analysis_root, "R", "', qmd, fixed = TRUE))
  expect_gt(length(files), 0L)
  expect_identical(length(files), length(unique(files)))
  expect_lte(length(files), sourced)
  expect_true("R/report_helpers.R" %in% files)
  expect_true(all(file.exists(file.path(root, files))))
})

test_that("the report bundle covers every report input and the renderer depends on it", {
  qmd <- paste(readLines(file.path(root, "report", "results_draft.qmd"), warn = FALSE), collapse = "\n")
  calls <- regmatches(qmd, gregexpr("read_report_input\\(\\s*[A-Za-z0-9_]+", qmd))[[1]]
  read <- unique(sub("^read_report_input\\(\\s*", "", calls))
  expect_length(read, 34L)
  bundle <- expression_of("report_inputs_file")
  inputs <- as.list(bundle[[2]])[-1L]
  expect_setequal(names(inputs), read)
  for (name in names(inputs)) expect_identical(inputs[[name]], as.name(name), info = name)
  dependencies <- codetools::findGlobals(eval(call("function", pairlist(), expression_of("results_draft"))), merge = FALSE)$variables
  expect_setequal(intersect(dependencies, manifest$name), "report_inputs_file")
  expect_true(
    all(read %in% manifest$name),
    info = paste("read by the report but not in the manifest:", paste(setdiff(read, manifest$name), collapse = ", "))
  )
  for (t in c(
    "report_network", "supplement_model_checks", "supplement_sampling_diagnostics", "supplement_software",
    "supplement_network_detail",
    "supplement_measurement"
  )) {
    expect_true(t %in% read, info = t)
  }
  # S1 reads its tables, counts and answer from its own target
  s1 <- command_of("supplement_measurement")
  expect_match(s1, "assemble_supplement_measurement(analysis_inputs$preparation$observed_item_distributions, scale_reliability", fixed = TRUE)
  for (input in c("efa_fits", "ap4_summary_files", "confirmatory_factor_structure_residuals_file")) {
    expect_match(s1, input, fixed = TRUE)
  }
  expect_match(command_of("results_draft"), "results_draft.qmd", fixed = TRUE)
})

test_that("the Student-t refit and AP10 targets are wired as the plan states", {
  # The refit and its tail trigger are wired in test-targets-regressions.R; the
  # AP10 consumers read the report tables.
  expect_match(paste(deparse(body(get("assemble_supplement_model_checks", envir = pipeline_env))), collapse = "\n"),
               "student_refit", fixed = TRUE)
  # Table 3 is decided on the coefficient summaries of the four primary
  # regressions and on nothing else.
  # `a |> f(b)` is the call `f(a, b)`, which is how tar_manifest() deparses it.
  expect_equal(
    command_of("regression_prediction_decisions"),
    paste("{ extract_regression_prediction_decisions(regression_coefficient_summaries,",
          "analysis_inputs$config) }")
  )
  expect_equal(call_chain(expression_of("regression_prediction_decisions")),
               "extract_regression_prediction_decisions")
  for (joint in c("joint_", "joint_within_asc", "asc_components", "overall_asc")) {
    expect_false(grepl(joint, command_of("regression_prediction_decisions"), fixed = TRUE), info = joint)
  }
  expect_match(command_of("supplement_prior_sensitivity"),
               paste("assemble_supplement_prior_sensitivity(regression_coefficient_summaries,",
                     "regression_fit_register, regression_prior_sensitivity, regression_prior_width_sensitivity,",
                     "data_regressions, codebook, analysis_plan)"),
               fixed = TRUE)
})

test_that("the joint additions use paired joint draws and the matching regression sample", {
  for (name in c("joint_correlation_changes", "joint_asc_aggregation")) {
    command <- command_of(name)
    expect_match(command, "joint_regression_fit", fixed = TRUE)
    expect_match(command, "interval_level = analysis_plan$regression$ci_level", fixed = TRUE)
    expect_false(grepl("bayesian_score_correlations", command, fixed = TRUE))
    expect_false(grepl("regression_primary_fits", command, fixed = TRUE))
  }
  expect_match(command_of("joint_correlation_changes"),
               "calculate_joint_correlation_changes(", fixed = TRUE)
  expect_match(command_of("joint_asc_aggregation"),
               "calculate_joint_asc_aggregation(", fixed = TRUE)
  expect_match(command_of("joint_asc_aggregation"), "data_regressions", fixed = TRUE)
  # the motives and their order come from the configuration
  expect_match(command_of("joint_asc_aggregation"), "analysis_plan = analysis_plan", fixed = TRUE)
  qmd <- paste(readLines(file.path(root, "report", "results_draft.qmd"), warn = FALSE), collapse = "\n")
  for (name in c("joint_correlation_changes", "joint_asc_aggregation")) {
    expect_match(qmd, paste0("read_report_input(", name, ","), fixed = TRUE)
  }
})

test_that("no target shares its name with a pipeline function", {
  funs <- names(Filter(isTRUE, eapply(pipeline_env, is.function)))
  expect_gt(length(funs), 0)
  for (f in c("fit_primary_regressions", "extract_joint_posterior", "zm_config")) {
    expect_true(f %in% funs, info = f)
  }
  clash <- intersect(manifest$name, funs)
  expect_true(length(clash) == 0, info = paste("target names that are also functions:", paste(clash, collapse = ", ")))
})


test_that("all three codebook CSVs are explicit file dependencies", {
  # The three CSVs are tracked by the one file target of the intake, and the
  # `codebook_files` target lists exactly those three elements of it.
  cmd <- command_of("codebook_files")
  for (name in c("codebook_items", "codebook_scales", "codebook_factors")) {
    expect_match(cmd, paste0('"', name, '"'), fixed = TRUE)
  }
  expect_match(cmd, "analysis_input_paths", fixed = TRUE)
  expect_match(command_of("codebook"), "analysis_inputs$codebook", fixed = TRUE)
  script <- readLines(file.path(root, "_targets.R"), warn = FALSE)
  expect_match(paste(script, collapse = "\n"), '}, format = "file"),\n  tar_target(codebook,', fixed = TRUE)
})

 test_that("a codebook CSV edit invalidates its consumer", {
  tmp <- withr::local_tempdir()
  csvs <- file.path(tmp, c("codebook_items.csv", "codebook_scales.csv", "codebook_factors.csv"))
  for (p in csvs) writeLines("value\n1", p)
  script <- file.path(tmp, "pipeline.R")
  store <- file.path(tmp, "store")
  writeLines(c(
    "library(targets)",
    paste0("files <- ", paste(capture.output(dput(csvs)), collapse = "")),
    'list(tar_target(codebook_files, files, format = "file"),',
    'tar_target(codebook, lapply(codebook_files, readLines)))'
  ), script)
  targets::tar_make(script = script, store = store, callr_function = NULL, reporter = "silent", use_crew = FALSE)
  expect_length(targets::tar_outdated(script = script, store = store, callr_function = NULL), 0)
  for (p in csvs) {
    writeLines("value\n2", p)
    outdated <- targets::tar_outdated(script = script, store = store, callr_function = NULL)
    expect_true(all(c("codebook_files", "codebook") %in% outdated), info = basename(p))
    targets::tar_make(script = script, store = store, callr_function = NULL, reporter = "silent", use_crew = FALSE)
  }
})

test_that("synthetic exports read summaries and receipt without fitting models", {
  expect_false("synthetic_report" %in% manifest$name)
  expect_true("synthetic_review_files" %in% manifest$name)
  exports <- command_of("synthetic_review_files")
  for (dependency in c("regression_coefficient_summaries", "regression_prior_width_sensitivity",
                       "regression_prediction_decisions", "synthetic_report_values",
                       "report_primary_coefficients", "calculation_receipt",
                       "synthetic_recovery_check_file")) {
    expect_match(exports, dependency, fixed = TRUE)
  }
  expect_match(exports, "write_synthetic_review_exports", fixed = TRUE)
  expect_false(grepl("tar_make|ap6_fit|brm\\(", exports))
  expect_match(command_of("synthetic_recovery_check_file"), "check_synthetic_recovery.R", fixed = TRUE)
})

test_that("the calculation receipt records the input and source files after their dependencies", {
  receipt <- command_of("calculation_receipt")
  for (dependency in c("analysis_input_files", "analysis_input_paths", "data", "demographics", "preparation", "receipt", "codebook_files", "calculation_source_files")) {
    expect_match(receipt, dependency, fixed = TRUE)
  }
  expect_match(receipt, "record_calculation_receipt", fixed = TRUE)
  expect_false(grepl("tar_read|tar_meta|tar_outdated", receipt))
})

test_that("the analysis plan and comment-only card are explicit file dependencies", {
  expect_match(command_of("analysis_plan_file"), "analysis_plan.yaml", fixed = TRUE)
  expect_match(command_of("parameter_card_file"), "analysis_plan_file", fixed = TRUE)
  expect_match(command_of("parameter_card_file"), "pc_update_parameter_cards", fixed = TRUE)
  expect_match(command_of("calculation_source_files"), "parameter_card_file", fixed = TRUE)
  expect_false(grepl("pc_update_parameter_cards", command_of("calculation_source_files"), fixed = TRUE))
})
