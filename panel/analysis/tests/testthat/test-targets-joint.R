# The AP7 / RQ2 joint-model slice: how the one correlated-residual regression
# of the four outcomes, its shared validity gate, its posterior and the three
# RQ2 comparison packages are wired into the target graph.
#
# No test here fits a model: the fitting verb is exercised against a stubbed
# `brms::brm()` and the gate against a stub that carries no draws.

local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) {
    parent <- dirname(dir)
    if (identical(parent, dir)) stop("project root not found from ", getwd())
    dir <- parent
  }
  for (f in c("config.R", "ap6_regressions.R", "ap6_pipeline.R", "report_supplement_prior_sensitivity.R", "report_supplement_model_checks.R", "report_supplement_sampling_diagnostics.R", "report_supplement_explained_variance.R", "report_supplement_software.R", "report_results_associations.R", "ap10_inference.R",
              "report_results_network.R", "report_supplement_network_detail.R", "ap7_joint_comparisons.R")) {
    source(file.path(dir, "R", f), local = FALSE)
  }
})

tj_root <- normalizePath(getwd())
while (!file.exists(file.path(tj_root, "config", "analysis_plan.yaml"))) tj_root <- dirname(tj_root)
tj_cfg <- zm_config()

tj_manifest <- withr::with_envvar(
  c(ZM_PROFILE = "smoke", ZM_DATA = "synthetic"),
  withr::with_dir(tj_root, targets::tar_manifest(callr_function = NULL))
)
tj_command <- function(name) {
  command <- tj_manifest$command[tj_manifest$name == name]
  if (length(command) != 1L) stop("target '", name, "' not found exactly once in the manifest")
  gsub("\\s+", " ", command)
}

tj_targets <- c("joint_regression_model", "joint_regression_fit", "joint_posterior",
                "joint_within_asc", "joint_asc_components_sdo")

tj_level <- "interval_level = analysis_inputs$config$regression$ci_level"

# The regression input of tests/testthat/test-targets-regressions.R, copied so
# that this file stands on its own: saved z columns, a known-gender factor and
# the standardisation constants the four primary fits already read.
tj_regression_input <- function() {
  columns <- c("asc_agg_z", "asc_sub_z", "asc_conv_z", "sdo_dom_z", "zm_security_z", "zm_achievement_z",
               "zm_power_z", "zm_prestige_z", "zm_arousal_z", "age_z", "income_z")
  n <- 12L
  data <- tibble::tibble(
    respondent_id = seq_len(n),
    gender = factor(rep(c("male", "female"), length.out = n), levels = c("male", "female")))
  for (i in seq_along(columns)) data[[columns[i]]] <- ((seq_len(n) + i) %% 5) - 2
  attr(data, "z_parameters") <- tibble::tibble(
    var = sub("_z$", "", columns), source = sub("_z$", "", columns), transform = "identity",
    z_col = columns, mean = 3, sd = 1.5, n = n)
  data
}

tj_error <- function(message = "the joint sampler did not start") {
  tryCatch(stop(message), error = function(e) e)
}

# --- the wiring --------------------------------------------------------------

test_that("the joint model and its fit read the same inputs as the four primary fits", {
  model <- tj_command("joint_regression_model")
  expect_match(model, "define_joint_regression_model(", fixed = TRUE)
  expect_match(model, "data_regressions,", fixed = TRUE)
  expect_match(model, "regression_model_set,", fixed = TRUE)

  fit <- tj_command("joint_regression_fit")
  expect_match(fit, "fit_joint_regression(", fixed = TRUE)
  expect_match(fit, "joint_regression_model,", fixed = TRUE)
  expect_match(fit, "apply_validity_gate(list(joint = fit), analysis_inputs$config)",
               fixed = TRUE)
})

test_that("the joint posterior is extracted once, from the gated fit", {
  # the configuration names the motives and their order (the codebook's)
  expect_match(tj_command("joint_posterior"),
               "extract_joint_posterior(joint_regression_fit, analysis_inputs$config)", fixed = TRUE)
})

test_that("each RQ2 package calls its three result verbs and ends in the withholding verb", {
  packages <- list(
    joint_within_asc = c("extract_joint_within_asc_directions(",
                   "extract_joint_within_asc_coefficient_differences(",
                   "extract_joint_within_asc_residual_correlations("),
    joint_asc_components_sdo = c("extract_joint_asc_components_sdo_directions(",
                              "extract_joint_asc_components_sdo_coefficient_differences(",
                              "extract_joint_asc_components_sdo_residual_correlations(")
  )
  for (name in names(packages)) {
    command <- tj_command(name)
    for (verb in packages[[name]]) expect_match(command, verb, fixed = TRUE, info = name)
    expect_match(command, "joint_posterior$coefficients", fixed = TRUE, info = name)
    expect_match(command, "joint_posterior$residual_correlations", fixed = TRUE, info = name)
    # The configured interval level reaches every one of the three verbs.
    expect_equal(lengths(regmatches(command, gregexpr(tj_level, command, fixed = TRUE))), 3L,
                 info = name)
    expect_match(command, "withhold_classification_when_fit_invalid(", fixed = TRUE,
                 info = name)
    expect_match(command, "joint_posterior$validity", fixed = TRUE, info = name)
  }
})

test_that("no joint target calls brms itself", {
  for (name in setdiff(tj_targets, "joint_regression_fit")) {
    expect_false(grepl("brms::", tj_command(name), fixed = TRUE), info = name)
  }
  # The decisive package call lives in the fitting verb, not in the target body.
  expect_false(grepl("brms::", tj_command("joint_regression_fit"), fixed = TRUE))
})

# --- the model definition ----------------------------------------------------

test_that("the joint response ids are the response columns of the four primary formulas", {
  data <- tj_regression_input()
  model_set <- define_regression_model_set(tj_cfg)
  joint_model <- define_joint_regression_model(data, model_set, tj_cfg)
  derived <- vapply(model_set$formulas, function(formula) gsub("[^[:alnum:]]", "", all.vars(formula)[[1]]),
                    character(1))
  expect_identical(joint_model$response_ids, derived)
  expect_identical(unname(as.character(brms::brmsterms(joint_model$formula)$responses)),
                   unname(derived))
  expect_true(joint_model$formula$rescor)
  expect_identical(joint_model$data, data)
})

test_that("the joint priors repeat the primary priors per response and add the rescor prior", {
  data <- tj_regression_input()
  model_set <- define_regression_model_set(tj_cfg)
  joint_model <- define_joint_regression_model(data, model_set, tj_cfg)
  priors <- as.data.frame(joint_model$priors)
  metric_predictors <- setdiff(all.vars(model_set$formulas[[1]])[-1], "gender")
  expect_equal(length(metric_predictors), 7L)
  gender_contrasts <- grep("^gender", colnames(stats::model.matrix(model_set$formulas[[1]], data)),
                           value = TRUE)
  expect_identical(gender_contrasts, "genderfemale")

  for (response in joint_model$response_ids) {
    slopes <- priors[priors$class == "b" & priors$resp == response, ]
    metric <- slopes[slopes$coef %in% metric_predictors, ]
    expect_setequal(metric$coef, metric_predictors)
    expect_identical(unique(metric$prior), "normal(0, 0.2)", info = response)
    gender <- slopes[slopes$coef %in% gender_contrasts, ]
    expect_setequal(gender$coef, gender_contrasts)
    expect_identical(unique(gender$prior), "normal(0, 0.4)", info = response)
    expect_equal(nrow(slopes), length(metric_predictors) + length(gender_contrasts),
                 info = response)
    intercept <- priors[priors$class == "Intercept" & priors$resp == response, ]
    expect_equal(nrow(intercept), 1L, info = response)
    expect_identical(intercept$prior, "normal(0, 0.2)", info = response)
    sigma <- priors[priors$class == "sigma" & priors$resp == response, ]
    expect_equal(nrow(sigma), 1L, info = response)
    expect_identical(sigma$prior, "normal(0, 1)", info = response)
  }
  rescor <- priors[priors$class == "rescor", ]
  expect_equal(nrow(rescor), 1L)
  expect_identical(rescor$prior, "lkj(2)")
  expect_equal(nrow(priors), 4L * (7L + 1L + 1L + 1L) + 1L)
})

# The fixed normal(0, .40) gender prior repeats per observed non-reference
# contrast and per response.
test_that("the joint gender-contrast priors cover every observed contrast of every outcome with three retained levels", {
  data <- tj_regression_input()
  # The retained gender factor with three observed levels, reference first as
  # the production reference step leaves it: 7 female, 4 male, 1 divers.
  data$gender <- factor(rep(c("female", "male", "divers"), times = c(7L, 4L, 1L)),
                        levels = c("female", "male", "divers"))
  expect_identical(levels(data$gender)[[1]], "female")
  expect_equal(length(unique(as.character(data$gender))), 3L)

  model_set <- define_regression_model_set(tj_cfg)
  joint <- define_joint_regression_model(data, model_set, tj_cfg)
  gender_sd <- tj_cfg$priors$gender_sd
  gender_prior <- paste0("normal(0, ", gender_sd, ")")
  metric_predictors <- setdiff(all.vars(model_set$formulas[[1]])[-1], "gender")
  expect_equal(length(metric_predictors), 7L)

  # --- the helper on its own -------------------------------------------------
  helper <- as.data.frame(
    ap6_build_joint_gender_contrast_priors(joint$formula, data, joint$response_ids, gender_sd))
  contrasts <- c("gendermale", "genderdivers")
  # The contrasts are read off the model matrix, so the reference level carries
  # no row of its own.
  expect_identical(
    grep("^gender", colnames(stats::model.matrix(model_set$formulas[[1]], data)), value = TRUE),
    contrasts)
  expect_equal(nrow(helper), 4L * 2L)
  expect_setequal(unique(helper$coef), contrasts)
  expect_false("genderfemale" %in% helper$coef)
  # This helper never reaches a metric predictor.
  expect_false(any(helper$coef %in% metric_predictors))
  for (response in joint$response_ids) {
    for (contrast in contrasts) {
      where <- paste(response, contrast)
      row <- helper[helper$resp == response & helper$coef == contrast, ]
      expect_equal(nrow(row), 1L, info = where)
      expect_identical(row$class, "b", info = where)
      expect_identical(row$prior, gender_prior, info = where)
      expect_identical(row$tag, "gender_contrasts", info = where)
    }
  }

  # --- the complete prior set of the three-level fixture ---------------------
  priors <- as.data.frame(joint$priors)
  slope_prior <- paste0("normal(0, ", tj_cfg$priors$slope_sd_primary, ")")
  for (response in joint$response_ids) {
    per_response <- priors[priors$resp == response, ]
    metric <- per_response[per_response$class == "b" & per_response$tag == "metric_slopes", ]
    expect_equal(nrow(metric), 7L, info = response)
    expect_setequal(metric$coef, metric_predictors)
    expect_identical(unique(metric$prior), slope_prior, info = response)
    gender <- per_response[per_response$class == "b" & per_response$tag == "gender_contrasts", ]
    expect_equal(nrow(gender), 2L, info = response)
    expect_setequal(gender$coef, contrasts)
    expect_identical(unique(gender$prior), gender_prior, info = response)
    expect_equal(sum(per_response$class == "Intercept"), 1L, info = response)
    expect_equal(sum(per_response$class == "sigma"), 1L, info = response)
    # No coefficient is given a prior twice within one response and class.
    expect_equal(anyDuplicated(per_response$coef[per_response$class == "b"]), 0L, info = response)
  }
  rescor <- priors[priors$class == "rescor", ]
  expect_equal(nrow(rescor), 1L)
  expect_identical(rescor$prior, tj_cfg$priors$rescor)
  expect_equal(nrow(priors), 4L * (7L + 2L + 1L + 1L) + 1L)

  # --- the two-level fixture keeps its single contrast ------------------------
  # The helper adapts to the observed levels; it does not hard-code two of them.
  two_level <- tj_regression_input()
  two_joint <- define_joint_regression_model(two_level, model_set, tj_cfg)
  two_helper <- as.data.frame(ap6_build_joint_gender_contrast_priors(
    two_joint$formula, two_level, two_joint$response_ids, gender_sd))
  expect_equal(nrow(two_helper), 4L)
  expect_setequal(unique(two_helper$coef), "genderfemale")
  for (response in two_joint$response_ids) {
    expect_equal(sum(two_helper$resp == response), 1L, info = response)
  }
})

# --- the fit -----------------------------------------------------------------

test_that("the joint fit is one brms call with the AP6 sampling settings", {
  data <- tj_regression_input()
  model_set <- define_regression_model_set(tj_cfg)
  joint_model <- define_joint_regression_model(data, model_set, tj_cfg)
  captured <- list()
  testthat::local_mocked_bindings(
    brm = function(...) {
      captured[[length(captured) + 1L]] <<- list(...)
      structure(list(), class = "brmsfit")
    }, .package = "brms")
  fit <- fit_joint_regression(joint_model, tj_cfg)
  expect_s3_class(fit, "brmsfit")
  expect_length(captured, 1L)
  call <- captured[[1]]
  expect_identical(call$formula, joint_model$formula)
  expect_identical(call$data, joint_model$data)
  expect_identical(call$prior, joint_model$priors)
  expect_identical(call$thin, 1)
  expect_identical(call$sample_prior, "no")
  expect_equal(call$seed, tj_cfg$regression$seed)
  expect_equal(call$chains, tj_cfg$regression$chains)
  expect_equal(call$warmup, tj_cfg$regression$warmup)
  expect_equal(call$iter, tj_cfg$regression$warmup + tj_cfg$regression$iter_per_chain)
  expect_identical(call$backend, tj_cfg$regression$backend)
})

test_that("the joint fit boundary captures only the fit call", {
  failure <- ap6_capture_joint_fit(stop("sampler failed"))
  expect_s3_class(failure, "error")
  expect_identical(conditionMessage(failure), "sampler failed")
})

# --- the shared validity gate ------------------------------------------------

test_that("the shared gate keeps the one-entry joint collection", {
  failed <- tj_error()
  gated <- apply_validity_gate(list(joint = failed), tj_cfg)
  expect_named(gated, "joint")
  expect_identical(gated$joint, failed)
  absent <- apply_validity_gate(list(joint = NULL), tj_cfg)
  expect_named(absent, "joint")
  expect_null(absent$joint)
})

test_that("the shared gate covers all four equations of the joint formula", {
  data <- tj_regression_input()
  model_set <- define_regression_model_set(tj_cfg)
  joint_model <- define_joint_regression_model(data, model_set, tj_cfg)
  expect_identical(ap6_gate_resp(joint_model$formula), unname(joint_model$response_ids))
})

# --- the report targets of the joint model -----------------------------------

test_that("the RQ2 report target reads the three packages", {
  expect_match(tj_command("report_joint"),
               "assemble_report_joint(joint_within_asc, joint_asc_components_sdo, joint_correlation_changes, analysis_plan)",
               fixed = TRUE)
  expect_match(tj_command("supplement_software"),
               "assemble_supplement_software(regression_fit_register, joint_regression_fit, analysis_plan)",
               fixed = TRUE)
})

# --- the diagnostics and provenance tables ------------------------------------

# The register fixtures of tests/testthat/test-targets-regressions.R, copied so
# that this file stands on its own: a stub fit carrying exactly the attributes
# the shared validity gate records.
tj_diagnostics <- function(ok = TRUE) {
  tibble::tibble(
    ok = ok, ess_ok = ok, rhat_ok = ok, divergences_ok = TRUE, treedepth_ok = TRUE,
    bfmi_ok = TRUE, ess_bulk_min = if (ok) 12000 else 400, ess_tail_min = if (ok) 11000 else 300,
    rhat_max = if (ok) 1.001 else 1.2, n_divergent = 0L, n_treedepth_hits = 0L,
    bfmi_min = 0.9, mcse_median_max = 0.002, mcse_q_max = 0.004,
    ess_target = 10000, rhat_limit = 1.01, divergences_max = 0, treedepth_hits_max = 0,
    bfmi_limit = 0.2, max_treedepth_used = 10L,
    gate_note = if (ok) "ok" else "ESS shortfall"
  )
}

tj_provenance <- function(gate_status) {
  tibble::tibble(
    outcome = "joint", slope_sd = NA_real_, family = "gaussian",
    formula = "asc_agg_z ~ zm_security_z", nobs = 600L, priors = "b: normal(0, 0.2)",
    stan_code_hash = "0f", hash_algorithm = "sha256", chains = 4L, warmup = 500L,
    iter_per_chain = 1000L, seed = 20260905L, gate_status = gate_status,
    brms_version = "2.22.0", cmdstanr_version = "0.8.1", cmdstan_version = "2.36.0"
  )
}

tj_gated_fit <- function(gate_status = "ok", ok = TRUE, provenance = TRUE) {
  fit <- structure(list(), class = "brmsfit")
  attr(fit, "diagnostics") <- tj_diagnostics(ok)
  attr(fit, "gate_status") <- gate_status
  attr(fit, "family_name") <- "gaussian"
  attr(fit, "iter_used") <- 1000L
  attr(fit, "warmup_used") <- 500L
  attr(fit, "n_obs") <- 600L
  attr(fit, "slope_sd") <- tj_cfg$priors$slope_sd_primary
  if (provenance) attr(fit, "provenance") <- tj_provenance(gate_status)
  fit
}

test_that("the joint provenance row names the model, not the collection key", {
  row <- tabulate_joint_provenance(list(joint = tj_gated_fit()), tj_cfg)
  expect_equal(nrow(row), 1L)
  expect_identical(row$outcome, "multivariate")
  expect_identical(row$role, "multivariate")
  expect_identical(row$label, zm_fit_label(tj_cfg, "multivariate"))
  expect_equal(row$slope_sd, tj_cfg$priors$slope_sd_primary)
  expect_identical(row$stan_code_hash, "0f")
  # The tags come first, as in the single-outcome provenance projection.
  expect_identical(names(row)[1:2], c("role", "label"))
})

test_that("the joint provenance row is absent without a fit and stops without provenance", {
  expect_null(tabulate_joint_provenance(list(joint = tj_error()), tj_cfg))
  expect_null(tabulate_joint_provenance(list(joint = NULL), tj_cfg))
  expect_error(
    tabulate_joint_provenance(list(joint = tj_gated_fit(provenance = FALSE)), tj_cfg),
    "captured at the fit boundary"
  )
})


test_that("the seven-pair calculation receives the regression data for scaling", {
  expect_match(tj_command("joint_correlation_changes"), "data_regressions = data_regressions", fixed = TRUE)
})
