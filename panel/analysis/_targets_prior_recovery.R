# Prior recovery (preregistration AP6, Justification of Prior Choice): the outcome-blind simulation behind the
# choice of the slope prior, as its own targets project. Every simulated data
# set passes through the preparation, fitting and validity-gate verbs of the
# main pipeline's AP6 route; the functions are in R/prior_recovery.R, the
# settings in analysis_plan$prior_recovery.
# Run from panel/analysis (see README.md):
#   TAR_PROJECT=prior_recovery_smoke Rscript -e 'targets::tar_make()'   # smoke_replicates per scenario
#   TAR_PROJECT=prior_recovery       Rscript -e 'targets::tar_make()'   # replicates per scenario

library(targets)

tar_source("R")

zm_setup()
analysis_plan <- read_recovery_plan()
recovery_run <- select_recovery_run(analysis_plan, Sys.getenv("TAR_PROJECT"))
root <- analysis_plan$root

# One local crew controller: as many workers as fit beside each other when
# every fit runs its chains on their own cores.
controller_recovery <- crew::crew_controller_local(
  name = "recovery", workers = zm_pipeline_resources(analysis_plan)$heavy_workers
)

tar_option_set(
  packages = c("tibble", "dplyr", "tidyr", "purrr", "stringr", "brms", "posterior", "yaml", "readr"),
  format = "rds",
  controller = controller_recovery,
  memory = "transient",
  garbage_collection = TRUE,
  error = "continue"
)

list(
  # ---- Inputs ----
  # The analysis plan, the simulation truth and the three codebooks.
  tar_target(recovery_input_files,
             select_recovery_input_files(root, analysis_plan$meta$codebook_dir),
             format = "file", deployment = "main"),
  tar_target(recovery_plan, {
    # Each reader names the file target, so that a changed file is read again.
    recovery_input_files
    read_recovery_plan(file.path(root, "config", "analysis_plan.yaml"))
  }, deployment = "main"),
  tar_target(recovery_codebook, {
    recovery_input_files
    zm_codebook(recovery_plan)
  }, deployment = "main"),
  tar_target(recovery_truth, {
    recovery_input_files
    zm_truth(file.path(root, "config", "simulation_truth.yaml"))
  }, deployment = "main"),
  tar_target(recovery_model_set, define_recovery_model_set(recovery_plan), deployment = "main"),

  # ---- Simulation design ----
  tar_target(recovery_scenario_coefficients,
             build_recovery_scenario_coefficients(recovery_truth, recovery_plan),
             deployment = "main"),
  tar_target(recovery_tasks,
             build_recovery_tasks(recovery_plan, recovery_run$replicates),
             deployment = "main"),

  # ---- One branch per scenario and replicate ----
  # Simulate, prepare as data_regressions, fit and gate every regression at
  # every slope SD, keep the motive coefficients. A sampler error of one fit
  # becomes that fit's rows; any other error stops the branch, so that the next
  # tar_make() runs it again.
  tar_target(recovery_coefficient_rows, {
    recovery_tasks |>
      simulate_recovery_dataset(recovery_scenario_coefficients, recovery_truth, recovery_plan,
                                recovery_codebook) |>
      fit_recovery_regressions(recovery_model_set, recovery_plan) |>
      summarise_recovery_fit(recovery_tasks, recovery_scenario_coefficients, recovery_plan)
  }, pattern = map(recovery_tasks)),

  # ---- Summary ----
  tar_target(recovery_scored_rows, score_recovery_rows(recovery_coefficient_rows), deployment = "main"),
  tar_target(recovery_summary, summarise_recovery_rows(recovery_scored_rows, recovery_plan),
             deployment = "main"),

  # ---- Files ----
  tar_target(recovery_summary_file,
             write_recovery_table(recovery_summary, recovery_run$paths[["summary"]]),
             format = "file", deployment = "main"),
  tar_target(recovery_coefficients_file,
             write_recovery_table(recovery_scored_rows, recovery_run$paths[["coefficients"]]),
             format = "file", deployment = "main"),
  tar_target(recovery_figure_file,
             write_recovery_figure(recovery_summary, recovery_run$paths[["figure"]]),
             format = "file", deployment = "main")
)
