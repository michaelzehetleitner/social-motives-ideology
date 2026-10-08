# _targets.R: the descriptive and reliability family. The AP5 and AP4 results
# are the runnable producers, and the report tables are built downstream of
# them.

local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) {
    parent <- dirname(dir)
    if (identical(parent, dir)) stop("project root not found from ", getwd())
    dir <- parent
  }
  source(file.path(dir, "R", "config.R"), local = FALSE)
})

dr_root <- zm_root()

dr_manifest <- withr::with_dir(
  dr_root,
  withr::with_envvar(
    c(ZM_PROFILE = "smoke", ZM_DATA = "synthetic"),
    withCallingHandlers(
      targets::tar_manifest(callr_function = NULL, envir = new.env(parent = globalenv())),
      warning = function(w) {
        if (grepl("built under R version|unter R Version", conditionMessage(w))) {
          invokeRestart("muffleWarning")
        }
      }
    )
  )
)

# the command as one line: tar_manifest() deparses long commands over several lines
dr_command <- function(name) {
  command <- dr_manifest$command[dr_manifest$name == name]
  if (length(command) != 1) stop("target '", name, "' not found exactly once in the manifest")
  gsub("\\s+", " ", command)
}

test_that("the three descriptive results read their prepared input directly", {
  expect_match(dr_command("sample_composition"),
               "describe_observed_sample_composition(analysis_inputs$demographics, study_analysis_ready, analysis_inputs$config)",
               fixed = TRUE)
  expect_match(dr_command("scale_score_distributions"),
               "describe_scale_distributions(data_descriptive_reliability, analysis_inputs$codebook)",
               fixed = TRUE)
  expect_match(dr_command("scale_score_correlations"),
               "describe_scale_correlations(data_descriptive_reliability, analysis_inputs$codebook, analysis_inputs$config)",
               fixed = TRUE)
})

test_that("reliability applies the same fallback to point estimates and bootstrap intervals", {
  expect_match(dr_command("reliability_point_estimates"),
    "estimate_reliability_coefficients(data_descriptive_reliability, analysis_inputs$codebook)", fixed = TRUE)
  expect_match(dr_command("reliability_bootstrap_results"),
    "calculate_reliability_bootstrap(reliability_point_estimates, analysis_inputs$config)", fixed = TRUE)
  expect_match(dr_command("scale_reliability"), "result <- reliability_point_estimates", fixed = TRUE)
  expect_match(dr_command("scale_reliability"), "result$bootstrap <- reliability_bootstrap_results", fixed = TRUE)
  expect_match(dr_command("scale_reliability"), "apply_reliability_fallback(result, analysis_inputs$config)", fixed = TRUE)
})

test_that("the S1 target reads the reliability result", {
  expect_match(dr_command("supplement_measurement"), "scale_reliability", fixed = TRUE)
})

