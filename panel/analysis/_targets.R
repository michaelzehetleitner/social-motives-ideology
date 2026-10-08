# Pipeline map: the preregistered analysis as a `targets` DAG, one
# route per preregistration section (AP1 ... AP11), plus the inputs, the intake
# gate, the two data files and the rendered results report. Read it for the verbs
# of a route and their order; the values are on the cards in R/.
# Run from panel/analysis (see README.md):
#   ZM_PROFILE=smoke Rscript -e 'targets::tar_make(callr_function = NULL)'   # minutes
#   ZM_PROFILE=full  Rscript -e 'targets::tar_make(callr_function = NULL)'   # preregistered settings
# ZM_PROFILE = smoke | full, ZM_DATA = synthetic | real.

library(targets)
library(tarchetypes)

tar_source("R")

zm_setup()
analysis_plan <- zm_config()

# ---- options ----------------------------------------------------------------

# Two local crew controllers: `light` for the cheap targets, `heavy` for the fits.
resources <- zm_pipeline_resources(analysis_plan)
controller_light <- crew::crew_controller_local(name = "light", workers = resources$light_workers)
controller_heavy <- crew::crew_controller_local(name = "heavy", workers = resources$heavy_workers)

tar_option_set(
  packages = c("tibble", "dplyr", "tidyr", "purrr", "stringr", "brms", "posterior", "yaml", "readr"),
  format = "rds",
  controller = crew::crew_controller_group(controller_light, controller_heavy),
  memory = "transient",
  garbage_collection = TRUE,
  error = "continue" # AP3 failures withhold dependent analyses while other branches continue.
)

heavy <- tar_resources(crew = tar_resources_crew(controller = "heavy"))
data_source <- zm_data_source()
reproduction_mode <- read_reproduction_mode()
# The run specification the input targets check once, and the analysis root the
# selected input paths are built under.
run_spec <- list(data_source = data_source)
root <- analysis_plan$root

# ---- pipeline ---------------------------------------------------------------

list(
  if (identical(reproduction_mode, "models")) tar_target(
    resampling_input_file, resampling_results_path(analysis_plan),
    format = "file", deployment = "main"),
  # ---- DATA · Intake ----
  # INTAKE: selected source -> checked prepared analysis inputs.
  # The file target tracks the plan, codebooks, both prepared CSVs,
  # the saved preparation results and their receipt.
  tar_target(
    analysis_input_files,
    {
      run <- check_run_spec(run_spec)
      select_analysis_input_files(run$data_source, root, analysis_plan$meta$codebook_dir)
    },
    format = "file", deployment = "main"
  ),
  # targets returns a file target's paths unnamed; the readers index them by name.
  tar_target(analysis_input_paths, zm_name_input_files(analysis_input_files),
             deployment = "main"),
  tar_target(
    analysis_inputs,
    {
      # Named so that a changed plan, codebook or data file re-reads the inputs:
      # analysis_input_paths keeps the same paths when only a file's content changes.
      analysis_input_files
      # analysis_input_files already established this source invariant
      source <- run_spec$data_source
      analysis_plan <- read_and_check_analysis_plan(analysis_input_paths, profile = pipeline_config$profile_name)
      codebook <- read_and_check_codebook(analysis_input_paths, analysis_plan)
      approval <- check_intake_approval(analysis_input_paths, source, analysis_plan)
      data <- read_clean_study_data(analysis_input_paths) |>
        add_col_types(approval$files$data$schema) |>
        add_factor_levels(approval$files$data$schema) |>
        check_study_data_matches_approval(approval, analysis_plan)
      list(
        data = data, demographics = read_prepared_demographics(approval),
        preparation = readRDS(analysis_input_paths[["preparation"]]),
        config = analysis_plan, codebook = codebook,
        source = source, intake_approval = approval
      )
    },
    deployment = "main"
  ),
  # Inputs: the configuration, the data source and their files
  tar_target(pipeline_config, analysis_plan),
  tar_target(data_source_used, data_source),
  tar_target(analysis_plan_file, file.path(analysis_plan$root, "config", "analysis_plan.yaml"),
             format = "file", deployment = "main"),
  tar_target(parameter_card_file, {
    analysis_plan_file
    pc_update_parameter_cards(root = analysis_plan$root)
    file.path(analysis_plan$root, "R", c("ap1_exclusions.R", "ap3_data_files.R", "ap3_fill.R",
                               "ap3_preparation.R", "ap3_preprocessing.R",
                               "ap4_cfa_reporting.R", "ap9_efa.R", "ap4_factor_structure.R",
                               "ap4_reliability.R", "ap5_descriptives.R", "ap6_regressions.R",
                               "ap10_inference.R", "ap8_network.R", "ap7_joint_comparisons.R"))
  }, format = "file", deployment = "main"),
  tar_target(raw_path, analysis_input_paths[["data"]]),
  tar_target(intake_approval_file, analysis_input_paths[["receipt"]]),
  tar_target(codebook_files, {
    unname(analysis_input_paths[c("codebook_items", "codebook_scales", "codebook_factors")])
  }, format = "file"),
  tar_target(codebook, analysis_inputs$codebook),
  # AP1/AP3 exclusions and fills are completed once at intake. The saved
  # scientific-use values contain no linked exact demographic inputs.
  tar_target(imputation_demographic_predictors,
             analysis_inputs$preparation$imputation_demographic_predictors),
  tar_target(study_analysis_ready, {
    analysis_inputs$data |>
      restore_prepared_analysis_data(analysis_inputs$preparation, analysis_inputs$config) |>
      standardise_all(analysis_inputs$codebook, analysis_inputs$config) |>
      standardise_known_gender(analysis_inputs$codebook, analysis_inputs$config)
  }),
  # AP3: one prepared dataset for each family of analyses; selection only
  tar_target(data_descriptive_reliability, {
    study_analysis_ready |>
      select_descriptive_reliability_input(analysis_inputs$codebook)
  }),
  tar_target(data_network, {
    study_analysis_ready |>
      select_network_input(analysis_inputs$codebook, analysis_inputs$config)
  }),
  tar_target(data_regressions, {
    study_analysis_ready |>
      drop_rows_without_gender() |>
      select_regression_input(analysis_inputs$codebook, analysis_inputs$config)
  }),
  # AP3: the preparation facts, assembled after the empirical objects exist
  tar_target(imputation_reporting_data, {
    analysis_inputs$preparation$imputation
  }),
  # Independent of fitted results: an AP3 failure still produces its current report.
  tar_target(imputation_processing_report, {
    write_imputation_processing_report(imputation_reporting_data, analysis_inputs$codebook,
      analysis_inputs$config)
  }, format = "file", deployment = "main"),
  tar_target(preparation_reporting_data, {
    study_analysis_ready |>
      build_preparation_reporting_data(analysis_inputs$preparation$exclusions, imputation_reporting_data)
  }),
  tar_target(regression_input_reporting_data, {
    data_regressions |>
      build_regression_input_reporting_data()
  }),
  # ---- DATA · Data files ----
  # The two verified intake CSVs are inputs; no unfilled/raw copy is recreated.
  tar_target(scientific_use_file, analysis_input_paths[["data"]],
             format = "file", deployment = "main"),
  tar_target(demographics_file, analysis_input_paths[["demographics"]],
             format = "file", deployment = "main"),
  tar_target(filled_cells_file, {
    imputation_reporting_data |>
      build_filled_cells_table() |>
      write_filled_cells_file(analysis_plan)
  }, format = "file", deployment = "main"),
  tar_target(dropped_respondents_file, {
    imputation_reporting_data |>
      build_dropped_respondents_table(analysis_inputs$config) |>
      write_dropped_respondents_file(analysis_plan)
  }, format = "file", deployment = "main"),
  tar_target(data_files_summary,
             zm_data_files_summary(scientific_use_file, demographics_file,
                                   filled_cells_file, dropped_respondents_file, analysis_plan)),
  # AP3: the companion text file of the shared file
  tar_target(scientific_use_readme, zm_scientific_use_readme(data_files_summary, analysis_plan),
             format = "file", deployment = "main"),

  # ---- DESCRIPTION · Descriptives ----
  # AP5: descriptives
  tar_target(sample_composition, {
    describe_observed_sample_composition(analysis_inputs$demographics, study_analysis_ready,
                                         analysis_inputs$config)
  }),
  tar_target(scale_score_distributions, {
    data_descriptive_reliability |>
      describe_scale_distributions(analysis_inputs$codebook) |>
      add_scale_histogram_data(data_descriptive_reliability,
        response_range = c(analysis_plan$scales$response_min, analysis_plan$scales$response_max))
  }),
  tar_target(scale_score_correlations, {
    data_descriptive_reliability |>
      describe_scale_correlations(
        analysis_inputs$codebook,
        analysis_inputs$config
      )
  }),
  # Pairwise Bayesian correlations of the scale scores; the empirical Pearson
  # correlations remain a separate descriptive result.
  tar_target(bayesian_score_correlations, {
    calculate_bayesian_score_correlations(
      data_descriptive_reliability,
      analysis_inputs$codebook,
      analysis_plan
    )
  }, deployment = "main"),

  # ---- DESCRIPTION · Reliability ----
  # AP4: reliability and scale structure (descriptive)
  # The point coefficients of the nine registered item sets, then their
  # participant-bootstrap intervals, then the registered fallback rule.
  tar_target(reliability_point_estimates,
    estimate_reliability_coefficients(data_descriptive_reliability, analysis_inputs$codebook)),
  tar_target(reliability_bootstrap_results, {
    if (identical(reproduction_mode, "models")) {
      read_saved_resampling_results(resampling_input_file, "reliability",
                                    reliability_point_estimates, analysis_inputs$config)
    } else calculate_reliability_bootstrap(reliability_point_estimates, analysis_inputs$config)
  }),
  tar_target(scale_reliability, {
    result <- reliability_point_estimates
    result$bootstrap <- reliability_bootstrap_results
    apply_reliability_fallback(result, analysis_inputs$config)
  }),

  # ---- ANALYSES · CFA ----
  # AP4: the CFA-only input; the rounding of the imputed item cells
  # The one scientific difference between the CFA input and the common
  # descriptive/reliability table. Reliability, EFA and the descriptives keep
  # reading the common table; only the confirmatory models read this copy.
  tar_target(data_cfa_input, round_imputed_items_for_cfa(
    data_descriptive_reliability, build_filled_cells_table(imputation_reporting_data),
    analysis_inputs$codebook)),
  # AP4: the fourteen preregistered confirmatory models, in five families
  tar_target(cfa_models, {
    data_cfa_input |>
      define_subscale_models(analysis_inputs$config, analysis_inputs$codebook) |>
      define_ums_dopl_models(analysis_inputs$config, analysis_inputs$codebook) |>
      define_social_motives_model(analysis_inputs$config, analysis_inputs$codebook) |>
      define_asc_model(analysis_inputs$config, analysis_inputs$codebook) |>
      define_auth_orientation_model(analysis_inputs$config, analysis_inputs$codebook) |>
      fit_cfa_models(analysis_inputs$codebook, analysis_inputs$config) |>
      assess_cfa_models()
  }),
  # AP4: the reporting boundary of the confirmatory factor structure
  tar_target(confirmatory_factor_structure_reporting_data, {
    build_cfa_reporting_data(cfa_models, analysis_inputs$config)
  }),
  # AP4: the file of all pairwise residuals, with scale and item labels
  tar_target(
    confirmatory_factor_structure_residuals_file,
    write_full_cfa_residual_output(
      tabulate_cfa_scale_models(confirmatory_factor_structure_reporting_data, analysis_inputs$codebook),
      tabulate_cfa_set_models(confirmatory_factor_structure_reporting_data, analysis_inputs$codebook,
                               analysis_inputs$config),
      analysis_plan
    ),
    format = "file", deployment = "main"
  ),
  # AP4: polychoric correlations and item thresholds as summary files
  tar_target(
    ap4_summary_files,
    write_measurement_summary_files(data_descriptive_reliability, codebook, analysis_plan),
    format = "file", deployment = "main"
  ),

  # ---- ANALYSES · Regressions ----
  # AP6: the four preregistered regressions, one model set, one gate
  tar_target(regression_model_set, {
    define_regression_model_set(analysis_inputs$config)
  }),
  tar_target(regression_primary_fits, {
    fits <- fit_primary_regressions(
      data_regressions,
      regression_model_set,
      analysis_inputs$config
    )
    apply_validity_gate(fits, analysis_inputs$config)
  }, resources = heavy),
  tar_target(regression_prior_width_fits, {
    fits <- fit_prior_width_comparisons(
      data_regressions,
      regression_model_set,
      analysis_inputs$config
    )
    apply_validity_gate(fits, analysis_inputs$config)
  }, resources = heavy),
  # AP6: the registered prior- and posterior-predictive checks
  tar_target(regression_predictive_checks, {
    prior_checks <- check_prior_predictions(
      data_regressions,
      regression_model_set,
      analysis_inputs$config
    )
    posterior_checks <- check_posterior_predictions(
      regression_primary_fits,
      analysis_inputs$config
    )
    list(prior = prior_checks, posterior = posterior_checks)
  }, resources = heavy),
  # AP6: the descriptive Student-t refit the one-sided trigger decides
  tar_target(regression_student_t_robustness, {
    fits <- fit_student_t_if_needed(
      data_regressions,
      regression_model_set,
      regression_primary_fits,
      regression_predictive_checks,
      analysis_inputs$config
    )
    apply_validity_gate(fits, analysis_inputs$config)
  }, resources = heavy),
  # AP6: one record per specified model or variant, with its final gate
  tar_target(regression_fit_register, {
    extract_regression_result_records(
      regression_primary_fits,
      regression_prior_width_fits,
      regression_student_t_robustness,
      analysis_inputs$config
    )
  }),

  # ---- ANALYSES · RQ1 coefficient differences and partial correlations ----
  # AP6 Coefficient Differences: per regression, the motive predicted positive
  # minus the motive predicted negative, in every draw of the gated fits
  tar_target(rq1_coefficient_differences, {
    calculate_coefficient_differences(regression_primary_fits, analysis_inputs$config)
  }),
  # AP6 Partial Correlations of the Motives: one joint model of the five motive
  # scores on age, gender and income with correlated residuals, gated with its
  # residual correlations, then converted in every draw
  tar_target(motive_model, {
    define_motive_model(data_regressions, regression_model_set, analysis_inputs$config)
  }),
  tar_target(motive_model_fit, {
    fit <- fit_motive_model(motive_model, analysis_inputs$config)
    apply_validity_gate(list(motives = fit), analysis_inputs$config)
  }, resources = heavy),
  tar_target(rq1_motive_partial_correlations, {
    calculate_motive_partial_correlations(motive_model_fit$motives, motive_model, analysis_inputs$config)
  }),

  # ---- ANALYSES · Joint model ----
  # AP7: the one correlated-residual joint regression of the four outcomes
  tar_target(joint_regression_model, {
    define_joint_regression_model(
      data_regressions,
      regression_model_set,
      analysis_inputs$config
    )
  }),
  tar_target(joint_regression_fit, {
    fit <- fit_joint_regression(
      joint_regression_model,
      analysis_inputs$config
    )
    apply_validity_gate(list(joint = fit), analysis_inputs$config)
  }, resources = heavy),

  # ---- ANALYSES · Network ----
  # AP8: RQ3 conditional-dependence structure and robustness
  # The nine-node Gaussian-copula graph on the prepared network input,
  # B participant bootstraps drawn once and fitted with one same-data retry
  # each, one descriptive full-sample fit and its refits at the swept edge
  # priors. Nothing here selects participants or standardises again.
  tar_target(network_settings, {
    data_network |>
      define_network_model(analysis_inputs$config) |>
      define_network_bagging(analysis_inputs$config)
  }),
  tar_target(network_bootstrap_samples, {
    resample_network_participants(network_settings)
  }),
  tar_target(network_bootstrap_fits, {
    if (identical(reproduction_mode, "models")) {
      read_saved_resampling_results(resampling_input_file, "network", network_settings, analysis_inputs$config)
    } else {
      fits <- fit_network_bootstraps(network_bootstrap_samples, network_settings)
      list(
        fits = fits,
        assessment = assess_network_bootstrap_success(fits, network_settings)
      )
    }
  }),
  tar_target(network_full_sample_fit, {
    fit_full_sample_network(network_settings)
  }),
  tar_target(network_prior_comparison_fits, {
    fit_network_prior_comparisons(network_settings, network_full_sample_fit)
  }),

  # ---- ANALYSES · SRQ1 · EFA ----
  # SRQ1: exploratory dimensionality and item correspondence
  # The nine scales alone and five combined groups; one polychoric matrix per
  # set; nine factor-number methods reported side by side; every distinct
  # suggested count and the configured theoretical count fitted in oblimin,
  # with one factor unrotated. No criterion selects a count.
  tar_target(efa_item_sets, {
    define_efa_item_sets(data_descriptive_reliability, analysis_inputs$config,
                             analysis_inputs$codebook)
  }),
  tar_target(efa_factor_number_results, {
    efa_item_sets |>
      estimate_polychoric_correlations() |>
      apply_factor_number_methods(analysis_inputs$config)
  }),
  tar_target(efa_factor_numbers, {
    efa_factor_number_results |>
      extract_suggested_factor_numbers() |>
      add_theoretical_factor_numbers(analysis_inputs$config)
  }),
  tar_target(efa_rotation_specifications, {
    efa_factor_numbers |>
      add_efa_rotation_methods(
        one_factor = "none",
        multiple_factors = analysis_inputs$config$factor_analysis$rotation_primary
      )
  }),
  tar_target(efa_fits, {
    efa_rotation_specifications |>
      fit_efa_models(analysis_inputs$config)
  }),

  # ---- RESULTS · Regressions ----
  # AP6 results: coefficients, explained variance, the sensitivities
  tar_target(regression_coefficient_summaries, {
    extract_regression_coefficient_summaries(
      regression_primary_fits,
      regression_prior_width_fits,
      regression_student_t_robustness,
      analysis_inputs$config
    )
  }),
  tar_target(regression_r2_summaries, {
    extract_regression_r2_summaries(
      regression_primary_fits,
      regression_prior_width_fits,
      regression_student_t_robustness,
      analysis_inputs$config
    )
  }),
  tar_target(regression_prior_width_sensitivity, {
    extract_regression_prior_width_sensitivity(
      regression_primary_fits,
      regression_prior_width_fits,
      analysis_inputs$config
    )
  }),
  tar_target(regression_prior_sensitivity, {
    extract_regression_prior_sensitivity(
      regression_primary_fits,
      analysis_inputs$config
    )
  }),
  tar_target(regression_likelihood_robustness, {
    extract_regression_likelihood_robustness(
      regression_coefficient_summaries,
      regression_r2_summaries
    )
  }),
  # AP10: the Table 3 decisions, from the four primary regressions alone
  tar_target(regression_prediction_decisions, {
    regression_coefficient_summaries |>
      extract_regression_prediction_decisions(analysis_inputs$config)
  }),

  # ---- RESULTS · Joint model ----
  # AP7 results: the joint posterior and the three RQ2 comparison packages
  tar_target(joint_posterior, {
    extract_joint_posterior(joint_regression_fit, analysis_inputs$config)
  }),
  # Supplementary comparison of the joint fit's residual and model-implied
  # unadjusted correlations; both keep the same draw identity.
  tar_target(joint_correlation_changes, {
    calculate_joint_correlation_changes(
      joint_regression_fit,
      data_regressions = data_regressions,
      interval_level = analysis_plan$regression$ci_level
    ) |> add_joint_reporting_facts()
  }),
  # Equal-weight raw-facet total, compared with facets in standardised units.
  tar_target(joint_asc_aggregation, {
    calculate_joint_asc_aggregation(
      joint_regression_fit,
      data_regressions,
      interval_level = analysis_plan$regression$ci_level,
      analysis_plan = analysis_plan
    ) |> add_joint_reporting_facts()
  }),
  tar_target(joint_within_asc, {
    direction <- extract_joint_within_asc_directions(joint_posterior$coefficients,
      interval_level = analysis_inputs$config$regression$ci_level)
    differences <- extract_joint_within_asc_coefficient_differences(joint_posterior$coefficients,
      interval_level = analysis_inputs$config$regression$ci_level)
    residuals <- extract_joint_within_asc_residual_correlations(joint_posterior$residual_correlations,
      interval_level = analysis_inputs$config$regression$ci_level)
    withhold_classification_when_fit_invalid(
      list(direction = direction, differences = differences, residuals = residuals),
      joint_posterior$validity)
  }),
  tar_target(joint_asc_components_sdo, {
    direction <- extract_joint_asc_components_sdo_directions(joint_posterior$coefficients,
      interval_level = analysis_inputs$config$regression$ci_level)
    differences <- extract_joint_asc_components_sdo_coefficient_differences(joint_posterior$coefficients,
      interval_level = analysis_inputs$config$regression$ci_level)
    residuals <- extract_joint_asc_components_sdo_residual_correlations(joint_posterior$residual_correlations,
      interval_level = analysis_inputs$config$regression$ci_level)
    withhold_classification_when_fit_invalid(
      list(direction = direction, differences = differences, residuals = residuals),
      joint_posterior$validity)
  }),

  # ---- RESULTS · Network ----
  # The bagged posterior is averaged before any Bayes-factor conversion; the
  # registered thresholds then classify the bagged Bayes factors. An infeasible
  # bag carries its assessment beside an empty edge table.
  tar_target(network_edge_evidence, {
    network_bootstrap_fits |>
      extract_network_bagged_estimates(network_settings) |>
      extract_network_bayes_factors()
  }),
  tar_target(network_bagged, {
    edges <- network_edge_evidence |>
      extract_network_edge_decisions(network_settings)
    list(edges = edges, assessment = network_bootstrap_fits$assessment)
  }),
  tar_target(network_full_sample, {
    network_full_sample_fit |>
      extract_network_bayes_factors() |>
      extract_network_edge_decisions(network_settings)
  }),
  tar_target(network_questions, {
    list(
      conditional_structure = network_bagged$edges,
      autonomy_orientation = extract_network_autonomy_orientation_edges(network_bagged$edges),
      autonomy_security_arousal = extract_network_autonomy_security_arousal_edges(network_bagged$edges)
    )
  }),
  tar_target(network_prior_sensitivity, {
    network_prior_comparison_fits |>
      extract_network_bayes_factors() |>
      extract_network_edge_decisions(network_settings) |>
      extract_network_prior_sensitivity()
  }),
  tar_target(network_resampling_comparison, {
    network_bagged |>
      extract_network_bagged_full_comparison(network_full_sample) |>
      extract_network_resampling_stability(network_bootstrap_fits, network_settings)
  }),

  # ---- RESULTS · SRQ1 · EFA ----
  tar_target(efa_item_assignments, {
    efa_fits |>
      extract_efa_item_assignments(analysis_inputs$config)
  }),
  tar_target(efa_item_correspondence, {
    efa_item_assignments |>
      extract_efa_item_correspondence()
  }),
  tar_target(efa_loading_clarity, {
    efa_item_correspondence |>
      extract_efa_loading_clarity(analysis_inputs$config)
  }),
  tar_target(efa_explained_variance, {
    extract_efa_explained_variance(efa_fits)
  }),

  # ---- REPORTING · Receipt ----
  # Receipt: the sources every calculated result was produced from
  tar_target(calculation_source_files, {
    parameter_card_file
    c(
      file.path(analysis_plan$root, "_targets.R"),
      list.files(file.path(analysis_plan$root, "R"), pattern = "\\.R$", full.names = TRUE),
      file.path(analysis_plan$root, "config", "analysis_plan.yaml"),
      file.path(analysis_plan$root, "renv.lock")
    )
  }, format = "file"),
  tar_target(calculation_receipt, {
    # Path targets stay unchanged when a file's contents change at the same path.
    analysis_input_files
    calculation_source_files |>
      record_calculation_receipt(c(analysis_input_paths[c("data", "demographics", "preparation", "receipt")],
                                  codebook_files), analysis_plan) |>
      add_reused_resampling_provenance(network_bootstrap_fits, reliability_bootstrap_results)
  }),

  # ---- REPORTING · Sample ----
  # Every number the Sample section, its figure and its table print; the report
  # words and formats them (R/report_results_sample.R).
  tar_target(report_participant_flow, {
    preparation_reporting_data |>
      report_exclusion_steps(imputation_reporting_data) |>
      add_sample_reporting_facts(imputation_reporting_data, analysis_plan)
  }),
  tar_target(report_sample_sizes, {
    report_participant_flow |>
      report_analysis_sample_sizes(regression_input_reporting_data, data_network)
  }),
  tar_target(report_sample_characteristics, {
    sample_composition |>
      report_sample_characteristic_rows()
  }),
  tar_target(report_political_sample, {
    analysis_inputs$preparation$political_sample
  }),

  tar_target(report_bayesian_correlations,
             build_correlation_reporting_data(bayesian_score_correlations)),

  # ---- REPORTING · Measurement ----
  tar_target(report_reliability_table, {
    scale_reliability |>
      report_reliability_by_scale()
  }),
  tar_target(report_reliability_counts, {
    report_reliability_table |>
      report_count_reliability_substitutions()
  }),

  # ---- REPORTING · RQ1 ----
  tar_target(report_association_table, {
    regression_prediction_decisions |>
      report_prediction_cells(report_sample_sizes, analysis_inputs$config) |>
      report_prior_width_robustness(
        regression_prior_width_sensitivity,
        regression_fit_register,
        analysis_inputs$codebook
      )
  }),
  tar_target(report_association_counts, {
    report_association_table |>
      report_count_prediction_verdicts() |>
      add_association_reporting_facts(report_association_table, rq1_coefficient_differences,
                                       rq1_motive_partial_correlations, analysis_plan)
  }),
  tar_target(report_credible_associations, {
    report_association_table |>
      report_credible_cells_by_outcome()
  }),
  tar_target(report_prior_width_changes, {
    report_association_table |>
      report_cells_changing_with_prior_width(regression_prior_width_sensitivity, analysis_inputs$codebook)
  }),
  # The coefficients of the four primary fits, for the coefficient figure and
  # the likelihood comparison.
  tar_target(report_primary_coefficients, {
    regression_coefficient_summaries |>
      tabulate_regression_coefficients(regression_fit_register, analysis_plan,
                                        roles = c("primary", "sweep")) |>
      zm_primary_coefs()
  }),

  # ---- REPORTING · RQ2 ----
  # RQ2: the three comparisons of the joint model in their three views
  # (R/report_results_joint.R).
  tar_target(report_joint, {
    assemble_report_joint(joint_within_asc, joint_asc_components_sdo, joint_correlation_changes, analysis_plan)
  }),

  # ---- REPORTING · RQ3 Network ----
  # The network: its edge decisions, its matrices and the edges of the three
  # RQ3 edge sets (R/report_results_network.R).
  tar_target(report_network, {
    network_bagged |>
      assemble_report_network(network_bootstrap_fits, network_questions, codebook, analysis_plan) |>
      add_network_reporting_facts(report_credible_associations, report_joint, analysis_plan)
  }),

  # ---- REPORTING · Deviations ----
  # AP11 (Deviations Policy): the deviations register of the preregistration
  tar_target(deviations_register, zm_deviations_table()),

  # ---- APPENDIX · Sampling diagnostics ----
  # One row per primary and sweep fit, and the fits that failed the gate with
  # their failed diagnostics (R/report_supplement_sampling_diagnostics.R). The
  # appendix's other tables come from the S1 and S3 targets.
  tar_target(supplement_sampling_diagnostics, {
    regression_fit_register |>
      assemble_supplement_sampling_diagnostics(analysis_plan) |>
      add_sampling_reporting_facts(regression_fit_register, joint_regression_fit, motive_model_fit)
  }),

  # ---- SUPPLEMENT · S1 Measurement ----
  # The tables, counts and answer facts of the section, which the Measurement
  # section reads as well (R/report_supplement_measurement.R).
  tar_target(supplement_measurement, {
    analysis_inputs$preparation$observed_item_distributions |>
      assemble_supplement_measurement(
        scale_reliability, confirmatory_factor_structure_reporting_data,
        efa_fits, efa_factor_numbers, efa_explained_variance,
        efa_loading_clarity, ap4_summary_files, confirmatory_factor_structure_residuals_file, imputation_reporting_data,
        codebook, analysis_plan
      ) |>
      add_measurement_reporting_facts(analysis_plan)
  }),

  # ---- SUPPLEMENT · S3 Prior sensitivity ----
  # The prior-width sweep, the power-scaling diagnostics, their counts and the
  # answer facts (R/report_supplement_prior_sensitivity.R).
  tar_target(supplement_prior_sensitivity, {
    regression_coefficient_summaries |>
      assemble_supplement_prior_sensitivity(regression_fit_register, regression_prior_sensitivity,
                                            regression_prior_width_sensitivity, data_regressions,
                                            codebook, analysis_plan) |>
      add_prior_reporting_facts(analysis_plan)
  }),

  # ---- SUPPLEMENT · S4 Model checks ----
  # The predictive checks, the heavy-tail flags, the Student-t refits, the
  # likelihood comparison, their counts and the answer facts
  # (R/report_supplement_model_checks.R).
  tar_target(supplement_model_checks, {
    regression_predictive_checks |>
      assemble_supplement_model_checks(regression_fit_register, regression_coefficient_summaries,
                                       report_primary_coefficients, regression_likelihood_robustness,
                                       analysis_plan) |>
      add_model_check_reporting_facts(regression_r2_summaries)
  }),

  # ---- SUPPLEMENT · S5 Network detail ----
  # The bagged against the full-data network, the resample stability, the
  # edge-prior sweep, their counts and the answer facts
  # (R/report_supplement_network_detail.R).
  tar_target(supplement_network_detail, {
    network_resampling_comparison |>
      assemble_supplement_network_detail(
        network_prior_sensitivity, network_bagged, network_full_sample, network_settings,
        report_network$fits, report_network$matrices, codebook, analysis_plan
      ) |>
      add_network_detail_reporting_facts()
  }),

  # ---- SUPPLEMENT · S6 Explained variance ----
  # The R-squared of the primary fits and the answer facts
  # (R/report_supplement_explained_variance.R).
  tar_target(supplement_explained_variance, {
    regression_r2_summaries |>
      assemble_supplement_explained_variance(analysis_plan)
  }),

  # ---- SUPPLEMENT · Software ----
  # The provenance of every fit and the package versions recorded with the fits
  # (R/report_supplement_software.R).
  tar_target(supplement_software, {
    regression_fit_register |>
      assemble_supplement_software(joint_regression_fit, analysis_plan)
  }),

  # ---- SUPPLEMENT · Provenance ----
  # The commit, the fingerprints and the run of this build, from the receipt;
  # the report states them in its Software section (M1 of the preregistration).
  tar_target(report_provenance, {
    calculation_receipt |>
      assemble_report_provenance(data_source_used)
  }),

  # ---- SUPPLEMENT · Data files ----
  # SUPPLEMENT · DATA FILES · the complete numbers behind the compact tables
  # One CSV each, at analysis_plan$data_files$result_files (AP4, AP6).
  tar_target(cfa_loadings_file,
             write_result_file(confirmatory_factor_structure_reporting_data$loadings, analysis_plan, "cfa_loadings"),
             format = "file", deployment = "main"),
  tar_target(efa_loadings_file,
             write_result_file(extract_efa_loadings(efa_fits), analysis_plan, "efa_loadings"),
             format = "file", deployment = "main"),
  tar_target(efa_membership_file,
             write_result_file(efa_loading_clarity$loading_items, analysis_plan, "efa_membership"),
             format = "file", deployment = "main"),
  tar_target(prior_power_scaling_file,
             write_result_file(supplement_prior_sensitivity$priorsense, analysis_plan, "prior_power_scaling"),
             format = "file", deployment = "main"),

  tar_target(posterior_predictive_checks_file,
             write_result_file(supplement_model_checks$posterior_predictive, analysis_plan,
                               "posterior_predictive_checks"),
             format = "file", deployment = "main"),

  # ---- REPORT · Results draft ----
  tar_target(report_inputs_file, write_report_inputs(list(
    imputation_reporting_data = imputation_reporting_data,
    pipeline_config = pipeline_config,
    data_source_used = data_source_used,
    scale_score_distributions = scale_score_distributions,
    report_bayesian_correlations = report_bayesian_correlations,
    report_primary_coefficients = report_primary_coefficients,
    report_joint = report_joint,
    joint_correlation_changes = joint_correlation_changes,
    joint_asc_aggregation = joint_asc_aggregation,
    deviations_register = deviations_register,
    supplement_software = supplement_software,
    report_provenance = report_provenance,
    report_network = report_network,
    supplement_measurement = supplement_measurement,
    supplement_prior_sensitivity = supplement_prior_sensitivity,
    supplement_model_checks = supplement_model_checks,
    supplement_network_detail = supplement_network_detail,
    supplement_sampling_diagnostics = supplement_sampling_diagnostics,
    regression_r2_summaries = regression_r2_summaries,
    supplement_explained_variance = supplement_explained_variance,
    report_participant_flow = report_participant_flow,
    report_sample_sizes = report_sample_sizes,
    report_sample_characteristics = report_sample_characteristics,
    report_political_sample = report_political_sample,
    report_reliability_table = report_reliability_table,
    report_reliability_counts = report_reliability_counts,
    report_association_table = report_association_table,
    rq1_coefficient_differences = rq1_coefficient_differences,
    rq1_motive_partial_correlations = rq1_motive_partial_correlations,
    report_association_counts = report_association_counts,
    report_credible_associations = report_credible_associations,
    report_prior_width_changes = report_prior_width_changes,
    codebook = codebook,
    report_synthetic_methods = report_synthetic_methods
  ), calculation_receipt, analysis_plan), format = "file", deployment = "main"),

  # Beside the sourced R files, the report's Quarto configuration and caption
  # style are tracked, so a change to them renders the report again, and so are
  # the generating file and the latent scores its Methods describe.
  tar_quarto(results_draft, "report/results_draft.qmd",
             execute_params = { report_inputs_file; list() },
             extra_files = c(find_report_source_files("report/results_draft.qmd"),
                             "report/_quarto.yml", "report/apa-captions.css", "report/apa-captions.html",
                             # the preregistration: the report quotes its sentences at render time
                             "../preregistration/preregistration.qmd"),
             deployment = "main"),

  tar_target(report_synthetic_methods, {
    if (identical(data_source_used, "synthetic")) {
      build_synthetic_methods_reporting_data(
        read_export_generating_values(simulation_truth_file),
        read_generated_latent_scores(synthetic_latent_scores_file), codebook, analysis_plan)
    } else NULL
  }),

  # ---- FILES · Resampling results ----
  if (identical(reproduction_mode, "full")) tar_target(resampling_results_file,
    write_resampling_results(network_bootstrap_fits, network_settings,
      reliability_bootstrap_results, reliability_point_estimates, analysis_plan, calculation_receipt),
    format = "file", deployment = "main"),

  # ---- FILES · Synthetic review results ----
  # Small summaries and the calculation receipt accompany the synthetic data.
  # The full model fits remain in the targets store.
  if (identical(data_source, "synthetic")) list(
    tar_target(simulation_truth_file, file.path(analysis_plan$root, "config", "simulation_truth.yaml"),
               format = "file", deployment = "main"),
    tar_target(synthetic_latent_scores_file,
               file.path(analysis_plan$root, "data", "synthetic", "zm_panel_synthetic_latent.csv"),
               format = "file", deployment = "main"),
    tar_target(synthetic_report_values, {
      simulation_truth_file |>
        read_export_generating_values() |>
        assemble_synthetic_report_values(
          read_generated_latent_scores(synthetic_latent_scores_file),
          report_participant_flow, report_sample_sizes,
          imputation_reporting_data, report_reliability_counts, efa_loading_clarity,
          confirmatory_factor_structure_reporting_data, supplement_model_checks,
          regression_fit_register, joint_posterior, network_bootstrap_fits, codebook, analysis_plan
        )
    }, deployment = "main"),
    tar_target(synthetic_recovery_check_file,
               file.path(analysis_plan$root, "scripts", "check_synthetic_recovery.R"),
               format = "file", deployment = "main"),
    tar_target(synthetic_review_files, {
      synthetic_recovery_check_file
      comparison <- compare_saved_synthetic_coefficients(
        report_primary_coefficients, read_export_generating_values(simulation_truth_file),
        read_generated_latent_scores(synthetic_latent_scores_file), analysis_plan
      )
      write_synthetic_review_exports(
        data_source_used, regression_coefficient_summaries, regression_prior_width_sensitivity,
        regression_prediction_decisions, synthetic_report_values, comparison,
        calculation_receipt, analysis_plan,
        variable_dictionary = codebook$scales[, c("scale_key", "label", "short_label", "z_col")],
        generating_values = read_export_generating_values(simulation_truth_file)
      )
    }, format = "file", deployment = "main")
  )
)
