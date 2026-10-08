# The NETWORK section of _targets.R: the eleven RQ3 producers, the report
# targets that read them, and the model definition.

local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) {
    parent <- dirname(dir)
    if (identical(parent, dir)) stop("project root not found from ", getwd())
    dir <- parent
  }
  for (f in c("config.R", "ap8_network.R")) {
    source(file.path(dir, "R", f), local = FALSE)
  }
})

network_targets_root <- zm_root()

network_targets_manifest <- withr::with_dir(
  network_targets_root,
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

network_command_of <- function(name) {
  command <- network_targets_manifest$command[network_targets_manifest$name == name]
  if (length(command) != 1) stop("target '", name, "' not found exactly once in the manifest")
  gsub("\\s+", " ", command)
}

network_producers <- c(
  "network_settings", "network_bootstrap_samples", "network_bootstrap_fits",
  "network_full_sample_fit", "network_prior_comparison_fits",
  "network_edge_evidence", "network_bagged", "network_full_sample",
  "network_questions", "network_prior_sensitivity", "network_resampling_comparison")

test_that("the eleven RQ3 producers are in the pipeline map", {
  for (target in network_producers) {
    expect_true(target %in% network_targets_manifest$name, info = target)
  }
  expect_length(network_producers, 11L)
})

test_that("each RQ3 producer runs its operation on its input", {
  # `targets` deparses the pipeline's `|>` steps as ordinary nested calls
  expect_identical(network_command_of("network_settings"),
                   paste("{ define_network_bagging(define_network_model(data_network,",
                         "analysis_inputs$config), analysis_inputs$config) }"))
  expect_identical(network_command_of("network_bootstrap_samples"),
                   "{ resample_network_participants(network_settings) }")
  bootstrap <- parse(text = network_targets_manifest$command[
    network_targets_manifest$name == "network_bootstrap_fits"])[[1]]
  expect_identical(bootstrap[[2]][[4]], parse(text = paste(
    "{ fits <- fit_network_bootstraps(network_bootstrap_samples, network_settings);",
    "list(fits = fits, assessment = assess_network_bootstrap_success(fits, network_settings)) }"))[[1]])
  expect_identical(network_command_of("network_full_sample_fit"),
                   "{ fit_full_sample_network(network_settings) }")
  expect_identical(network_command_of("network_prior_comparison_fits"),
                   "{ fit_network_prior_comparisons(network_settings, network_full_sample_fit) }")
  expect_identical(
    network_command_of("network_edge_evidence"),
    paste("{ extract_network_bayes_factors(extract_network_bagged_estimates(network_bootstrap_fits,",
          "network_settings)) }"))
  expect_identical(
    network_command_of("network_bagged"),
    paste("{ edges <- extract_network_edge_decisions(network_edge_evidence, network_settings)",
          "list(edges = edges, assessment = network_bootstrap_fits$assessment) }"))
  expect_identical(
    network_command_of("network_full_sample"),
    paste("{ extract_network_edge_decisions(extract_network_bayes_factors(network_full_sample_fit),",
          "network_settings) }"))
  expect_identical(
    network_command_of("network_questions"),
    paste("{ list(conditional_structure = network_bagged$edges, autonomy_orientation =",
          "extract_network_autonomy_orientation_edges(network_bagged$edges), autonomy_security_arousal =",
          "extract_network_autonomy_security_arousal_edges(network_bagged$edges)) }"))
  expect_identical(
    network_command_of("network_prior_sensitivity"),
    paste0("{ extract_network_prior_sensitivity(extract_network_edge_decisions(",
           "extract_network_bayes_factors(network_prior_comparison_fits), network_settings)) }"))
  expect_identical(
    network_command_of("network_resampling_comparison"),
    paste("{ extract_network_resampling_stability(extract_network_bagged_full_comparison(network_bagged,",
          "network_full_sample), network_bootstrap_fits, network_settings) }"))
})

test_that("the network report targets read the route's results", {
  expect_identical(network_command_of("report_network"),
                   "{ add_network_reporting_facts(assemble_report_network(network_bagged, network_bootstrap_fits, network_questions, codebook, analysis_plan), report_credible_associations, report_joint, analysis_plan) }")
  # S5 builds the full-data, comparison and edge-prior tables from the route's
  # results, and takes the fits and bagged matrices of the RQ3 target
  expect_match(network_command_of("supplement_network_detail"),
               "assemble_supplement_network_detail(network_resampling_comparison, network_prior_sensitivity,",
               fixed = TRUE)
  expect_match(network_command_of("supplement_network_detail"),
               "report_network$fits, report_network$matrices", fixed = TRUE)
})

test_that("the model definition names the configured nodes and their order", {
  analysis_plan <- zm_config(profile = "smoke",
                   path = file.path(network_targets_root, "config", "analysis_plan.yaml"))
  # the nodes are read from the plan, never typed in the body; the AP8 NETWORK
  # MODEL card above the verb prints them
  expect_true(grepl("analysis_plan$network$nodes", paste(deparse(body(define_network_model)),
                                               collapse = " "), fixed = TRUE))
  model <- define_network_model(
    as.data.frame(stats::setNames(rep(list(1:3), length(analysis_plan$network$nodes)),
                                  as.character(analysis_plan$network$nodes))),
    analysis_plan)
  expect_identical(model$nodes, as.character(analysis_plan$network$nodes))
  # the model half carries the per-fit settings; the bagging half adds the rest
  expect_setequal(names(model$settings),
                  c("package", "iter", "g_prior", "df_prior", "not_cont",
                    "bf_include", "bf_exclude", "g_prior_sweep"))
  bagged <- define_network_bagging(model, analysis_plan)
  expect_identical(bagged$data, model$data)
  expect_identical(bagged$nodes, model$nodes)
  expect_setequal(setdiff(names(bagged$settings), names(model$settings)),
                  c("B", "seed_base", "retry_seed_offset", "min_success_rate"))
  for (field in names(bagged$settings)) {
    expect_equal(bagged$settings[[field]], analysis_plan$network[[field]], info = field)
  }
  # every setting the route reads is a configured value, checked by
  # zm_config() before any target runs
  for (field in c("B", "seed_base", "retry_seed_offset", "min_success_rate", "bf_include",
                  "bf_exclude", "g_prior", "g_prior_sweep", "package", "iter",
                  "df_prior", "not_cont")) {
    expect_false(is.null(analysis_plan$network[[field]]), info = field)
  }
})

