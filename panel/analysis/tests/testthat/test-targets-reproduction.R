# The resampling boundary changes only where slow results are obtained; the
# ordinary model commands and full-run calculations retain their existing route.
local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) dir <- dirname(dir)
  source(file.path(dir, "R", "config.R"), local = FALSE)
  assign("reproduction_targets_root", dir, envir = .GlobalEnv)
})

reproduction_manifest <- function(mode) withr::with_dir(reproduction_targets_root,
  withr::with_envvar(c(ZM_PROFILE = "smoke", ZM_DATA = "synthetic", ZM_REPRODUCE = mode),
    targets::tar_manifest(callr_function = NULL, envir = new.env(parent = globalenv()))))

reproduction_command <- function(manifest, name) {
  text <- manifest$command[manifest$name == name]
  stopifnot(length(text) == 1L)
  parse(text = text)[[1]]
}

test_that("the two resampling modes keep ordinary model targets identical", {
  full <- reproduction_manifest("full")
  models <- reproduction_manifest("models")
  expect_true("resampling_results_file" %in% full$name)
  expect_false("resampling_input_file" %in% full$name)
  expect_true("resampling_input_file" %in% models$name)
  expect_false("resampling_results_file" %in% models$name)
  for (name in c("regression_primary_fits", "regression_prior_width_fits",
                 "joint_regression_fit", "network_full_sample_fit", "network_prior_comparison_fits",
                 "reliability_point_estimates", "cfa_models", "efa_fits")) {
    expect_identical(reproduction_command(models, name), reproduction_command(full, name), info = name)
  }
  export <- reproduction_command(full, "resampling_results_file")
  expect_identical(export[[1]], as.name("write_resampling_results"))
  expect_setequal(all.vars(export), c("network_bootstrap_fits", "network_settings",
    "reliability_bootstrap_results", "reliability_point_estimates", "analysis_plan", "calculation_receipt"))
})

test_that("models mode uses checked saved results without invoking either resampling fit", {
  manifest <- reproduction_manifest("models")
  calls <- list()
  inputs <- list(reproduction_mode = "models", resampling_input_file = "saved.rds",
    network_settings = list(data = "network input"), reliability_point_estimates = list(scales = "reliability input"),
    analysis_inputs = list(config = list(profile_name = "smoke")),
    read_saved_resampling_results = function(path, family, input, analysis_plan) {
      calls[[family]] <<- list(path = path, input = input, plan = analysis_plan)
      structure(list(result = family), resampling_provenance = list(original_commit = family))
    },
    fit_network_bootstraps = function(...) stop("Network resampling must not run"),
    calculate_reliability_bootstrap = function(...) stop("Reliability resampling must not run"))
  env <- list2env(inputs, parent = globalenv())
  network <- eval(reproduction_command(manifest, "network_bootstrap_fits"), env)
  reliability <- eval(reproduction_command(manifest, "reliability_bootstrap_results"), env)
  expect_identical(network$result, "network")
  expect_identical(reliability$result, "reliability")
  expect_identical(calls$network$input, inputs$network_settings)
  expect_identical(calls$reliability$input, inputs$reliability_point_estimates)
  for (call in calls) {
    expect_identical(call$path, "saved.rds")
    expect_identical(call$plan, inputs$analysis_inputs$config)
  }
  expect_identical(attr(network, "resampling_provenance")$original_commit, "network")
  expect_identical(attr(reliability, "resampling_provenance")$original_commit, "reliability")
})
