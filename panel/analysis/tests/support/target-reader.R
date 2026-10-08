# Evaluates the production target commands of `_targets.R` on a given input,
# with the four fitting verbs of the AP3 imputation replaced by stubs. The route
# tests use it to run the real preparation and result chains without a sampler.

source_pipeline_functions <- function(root) {
  env <- environment(source_pipeline_functions)
  files <- basename(list.files(file.path(root, "R"), pattern = "[.]R$", full.names = TRUE))
  for (file in files) sys.source(file.path(root, "R", file), envir = env)
  invisible(files)
}

# The four fitting verbs of the AP3 imputation, as stubs that record their name
# and write a deterministic placeholder — the median of the column's observed
# values — into the cells the production verb fits a model for. The
# imputation's four brms fits are the subject of tests/testthat/test-ap3-fill.R,
# so a reader built with these stubs runs the whole preparation chain without a
# sampler. The stubs
# keep the production rule about WHICH cells are written (items for every row,
# the three demographics for every retained row with a missing value, gender
# known or not — the production verbs have no known-gender restriction and their
# postcondition requires complete demographics), because everything
# downstream — the model samples, the joint sample, the brms call design —
# depends on the frame being complete, not on the imputation's numbers.
# `exclude_participants_with_high_missingness()` fits nothing and always
# stays live, so the rows the exclusion rule removes are the real ones.
# `item_placeholder`, when given, replaces the median for the item cells only:
# a function of the column's observed values and the column name, returning
# the one value written into every gap of that column. It lets a route test
# plant fractional item fills, which the median of integer responses never is.
make_fill_stubs <- function(record = NULL, item_placeholder = NULL) {
  env <- environment(source_pipeline_functions)
  median_of <- function(x) {
    observed <- x[!is.na(x)]
    if (length(observed) == 0L) stop("make_fill_stubs(): no observed value to take a placeholder from.")
    stats::median(observed)
  }
  write_cells <- function(data, columns, kind, model) {
    for (column in intersect(columns, names(data))) {
      gaps <- which(is.na(data[[column]]))
      if (length(gaps) == 0L) next
      value <- if (identical(kind, "item") && is.function(item_placeholder)) {
        item_placeholder(data[[column]][!is.na(data[[column]])], column)
      } else {
        median_of(data[[column]])
      }
      data[[column]][gaps] <- value
      attr(data, "imputed_cells") <- dplyr::bind_rows(
        attr(data, "imputed_cells", exact = TRUE),
        tibble::tibble(
          respondent_id = data$respondent_id[gaps],
          variable = column, kind = kind, model = model,
          value = as.numeric(value), lower = as.numeric(value), upper = as.numeric(value),
          n_fit_rows = sum(!is.na(data[[column]]))
        ))
    }
    data
  }
  items <- function(data, codebook, analysis_plan) {
    # The frame is forced before the name is recorded, so that the recorded
    # order is the order in which the verbs of the pipe finish, innermost
    # first, rather than the order in which R enters their calls.
    force(data)
    if (is.function(record)) record("impute_items")
    # The producer's typed empty record, as the production verb initialises it.
    attr(data, "imputed_cells") <- env$ap3_fill_empty_cells()
    write_cells(data, unique(unlist(codebook$scales$item_codes)), "item", "stub")
  }
  demographic <- function(name, column) {
    function(data, codebook, analysis_plan, predictors) {
      force(data)
      if (is.function(record)) record(name)
      write_cells(data, column, "demographic", column)
    }
  }
  list(
    impute_items = items,
    impute_age = demographic("impute_age", "demo_age"),
    impute_household_size = demographic("impute_household_size", "demo_hh_members"),
    impute_income_band = demographic("impute_income_band", "demo_income_hh_net")
  )
}

# A checked pre-preparation fixture; make_target_reader() prepares it once
# using the production intake functions with the explicit fill stubs below.
make_analysis_inputs <- function(data, analysis_plan, codebook, source = "synthetic") {
  structure(list(data = data, config = analysis_plan, codebook = codebook, source = source),
            class = "unprepared_intake_fixture")
}

# Build the real one-time preparation with only its expensive fill boundaries
# replaced, then expose exactly the same saved-value bundle that targets reads.
prepare_fixture_analysis_inputs <- function(fixture, stubs) {
  env <- list2env(stubs, parent = environment(make_target_reader))
  prepare <- prepare_analysis_at_intake
  environment(prepare) <- env
  prepared <- prepare(fixture$data, fixture$codebook, fixture$config)
  results <- build_intake_preparation_results(prepared, fixture$codebook, fixture$config)
  data <- ap3_scientific_use_data(prepared$data, fixture$config, fixture$codebook)
  demographics <- build_deidentified_demographics(prepared$observed)
  paths <- file.path(tempfile("prepared-intake-fixture-"), c("synthetic.csv", "synthetic_demographics.csv", "synthetic_preparation.rds"))
  dir.create(dirname(paths[1]))
  ap3_write_data_file(data, paths[1]); ap3_write_data_file(demographics, paths[2]); saveRDS(results, paths[3])
  list(inputs = list(data = data, demographics = demographics, preparation = results,
         config = fixture$config, codebook = fixture$codebook, source = fixture$source),
       prepared = prepared, paths = stats::setNames(paths, c("data", "demographics", "preparation")))
}

# This deliberately supports only commands in this repository's target list.
# Inputs are injected, dynamic statistical targets are never requested, and
# only the named estimator/file boundaries below may run in this test session.
# The fitting targets at the end of the list are allowed so that connected
# interface tests can run the actual target commands with a stand-in sampler.
make_target_reader <- function(root, inputs, commands = NULL) {
  if (is.null(commands)) {
    expressions <- parse(file.path(root, "_targets.R"))
    calls <- as.list(expressions[[length(expressions)]])[-1L]
    calls <- Filter(function(x) is.call(x) && identical(x[[1]], as.name("tar_target")), calls)
    commands <- stats::setNames(lapply(calls, `[[`, 3L), vapply(calls, function(x) as.character(x[[2]]), ""))
  }
  prepared_fixture <- NULL
  if (inherits(inputs$analysis_inputs, "unprepared_intake_fixture")) {
    stubs <- make_fill_stubs()
    for (name in intersect(names(inputs), names(stubs)))
      stubs[[name]] <- inputs[[tail(which(names(inputs) == name), 1)]]
    prepared_fixture <- prepare_fixture_analysis_inputs(inputs$analysis_inputs, stubs)
    inputs$analysis_inputs <- prepared_fixture$inputs
    inputs$analysis_input_paths <- prepared_fixture$paths
  }
  if (is.null(inputs$reproduction_mode)) inputs$reproduction_mode <- "full"
  values <- list2env(inputs, parent = environment(make_target_reader))
  allowed <- c("imputation_demographic_predictors", "study_analysis_ready",
               "imputation_reporting_data", "preparation_reporting_data",
               "regression_input_reporting_data", "data_descriptive_reliability",
               "data_network", "data_regressions",
               "report_participant_flow", "report_sample_sizes",
               "scientific_use_file", "demographics_file", "filled_cells_file", "dropped_respondents_file",
               "reliability_point_estimates", "reliability_bootstrap_results", "scale_reliability",
               "data_cfa_input", "cfa_models",
               "confirmatory_factor_structure_reporting_data",
               "confirmatory_factor_structure_residuals_file",
               "efa_item_sets", "efa_factor_number_results", "efa_factor_numbers",
               "efa_rotation_specifications", "efa_fits",
               "efa_explained_variance",
               "sample_composition",
               "scale_score_distributions", "scale_score_correlations",
               "efa_item_assignments", "efa_item_correspondence", "efa_loading_clarity",
               "data_files_summary", "scientific_use_readme",
               "deviations_register",
               # Fitting, gate and result targets of the regression and network
               # routes. The reader still fits nothing itself:
               # a test that reads one of these must first replace the sampler
               # (`brms::brm()`, `stats::update()`, the easybgm callback) with a
               # stand-in, otherwise the real target command starts a real fit.
               "regression_model_set", "regression_primary_fits", "regression_prior_width_fits",
               "regression_predictive_checks", "regression_student_t_robustness",
               "regression_fit_register", "regression_coefficient_summaries",
               "regression_r2_summaries", "regression_prior_width_sensitivity",
               "regression_prior_sensitivity", "regression_likelihood_robustness",
               "report_primary_coefficients", "supplement_explained_variance",
               "supplement_sampling_diagnostics",
               "supplement_model_checks",
               "supplement_prior_sensitivity",
               "regression_prediction_decisions", "joint_regression_model", "joint_regression_fit",
               "joint_posterior", "joint_correlation_changes", "joint_asc_aggregation", "joint_within_asc", "joint_asc_components_sdo",
               "report_joint", "supplement_software",
               "network_settings", "network_bootstrap_samples", "network_bootstrap_fits",
               "network_full_sample_fit", "network_prior_comparison_fits",
               "network_edge_evidence", "network_bagged", "network_full_sample",
               "network_questions", "network_prior_sensitivity", "network_resampling_comparison",
               "report_network", "supplement_network_detail")
  read <- function(name) {
    if (exists(name, values, inherits = FALSE)) return(get(name, values))
    if (!name %in% names(commands)) stop("No pipeline target: ", name)
    if (!name %in% allowed) stop("Target outside the reader's allowlist: ", name)
    command <- commands[[name]]
    # Only free variable reads are dependencies: all.vars() would also report
    # the name after `$`.
    dependencies <- intersect(codetools::findGlobals(
      eval(call("function", pairlist(), command)), merge = FALSE)$variables, names(commands))
    for (dependency in dependencies) read(dependency)
    value <- eval(command, values)
    assign(name, value, values)
    value
  }
  list(read = read, values = values, intake = if (is.null(prepared_fixture)) NULL else prepared_fixture$prepared)
}
