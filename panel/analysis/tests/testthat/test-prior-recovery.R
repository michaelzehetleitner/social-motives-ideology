# R/prior_recovery.R without a sampler: the recovery settings, the task seeds,
# the scenario coefficients, the simulated regression input, the rows of one
# task, which errors become rows (a sampler error of one fit) and which stop the
# task (every other), the scoring and the summary the preregistration reads.

local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) {
    parent <- dirname(dir)
    if (identical(parent, dir)) stop("project root not found from ", getwd())
    dir <- parent
  }
  for (f in c("config.R", "ap3_preprocessing.R", "ap3_preparation.R", "ap3_imputation_validity.R",
              "ap6_regressions.R", "ap10_inference.R", "prior_recovery.R")) {
    source(file.path(dir, "R", f), local = FALSE)
  }
})

recovery_root <- zm_root()
recovery_plan <- read_recovery_plan(file.path(recovery_root, "config", "analysis_plan.yaml"))
recovery_codebook <- zm_codebook(recovery_plan)
recovery_truth <- zm_truth(file.path(recovery_root, "config", "simulation_truth.yaml"))
recovery_coefficients <- build_recovery_scenario_coefficients(recovery_truth, recovery_plan)
recovery_motives <- as.character(recovery_plan$regression$motives)
recovery_outcomes <- as.character(recovery_plan$regression$outcomes)
recovery_scenarios <- as.character(unlist(recovery_plan$prior_recovery$effect_scenarios))
recovery_slope_sds <- as.numeric(unlist(recovery_plan$prior_recovery$slope_sds))

test_that("the recovery sampler settings replace the regression's sampler and nothing else", {
  full <- zm_config(profile = "full", path = file.path(recovery_root, "config", "analysis_plan.yaml"))
  sampling <- full$prior_recovery$sampling
  regression <- recovery_plan$regression
  expect_identical(regression$chains, as.integer(sampling$chains))
  expect_identical(regression$cores, as.integer(sampling$chains))
  expect_identical(regression$warmup, as.integer(sampling$warmup))
  expect_identical(regression$iter_per_chain, as.integer(sampling$iter_per_chain))
  expect_equal(regression$ess_target, sampling$ess_target)
  sampler <- c("chains", "cores", "warmup", "iter_per_chain", "ess_target")
  expect_identical(regression[setdiff(names(regression), sampler)], full$regression[setdiff(names(full$regression), sampler)])
  expect_identical(recovery_plan$priors, full$priors)
  expect_identical(recovery_plan$profile_name, "full")
})

test_that("the recovery block is refused when a setting is unusable", {
  broken <- function(edit) {
    plan <- recovery_plan
    edit(plan)
  }
  expect_error(check_recovery_settings(broken(function(p) { p$prior_recovery <- NULL; p })),
               "prior_recovery is missing")
  expect_error(check_recovery_settings(broken(function(p) { p$prior_recovery$slope_sds <- list(0.1, 0.3); p })),
               "slope_sds must be values of priors$slope_sd_sweep", fixed = TRUE)
  expect_error(check_recovery_settings(broken(function(p) { p$prior_recovery$slope_sds <- list(0.1, 0.4); p })),
               "the primary width among them")
  expect_error(check_recovery_settings(broken(function(p) {
    p$prior_recovery$effect_scenarios <- list("zero", "close_anchor_x"); p
  })), "close_anchor_x")
  expect_error(check_recovery_settings(broken(function(p) { p$prior_recovery$sampling$warmup <- 0; p })),
               "sampling$warmup", fixed = TRUE)
  expect_error(check_recovery_settings(broken(function(p) { p$prior_recovery$smoke_replicates <- 1.5; p })),
               "smoke_replicates")
  expect_invisible(check_recovery_settings(recovery_plan))
})

test_that("a replicate keeps its data seed whatever the number of replicates of the run", {
  smoke <- build_recovery_tasks(recovery_plan, 2L)
  full <- build_recovery_tasks(recovery_plan, recovery_plan$prior_recovery$replicates)
  expect_named(smoke, c("scenario", "scenario_index", "replicate", "data_seed"))
  expect_equal(nrow(smoke), 2L * length(recovery_scenarios))
  expect_equal(nrow(full), recovery_plan$prior_recovery$replicates * length(recovery_scenarios))
  expect_identical(smoke$scenario, rep(recovery_scenarios, each = 2L))
  expect_identical(smoke$data_seed[smoke$scenario == "close_anchor_0.37"], c(20263906L, 20263907L))
  # the seed is keyed by the scenario's place in the plan and the replicate, not by the task's row
  in_full <- dplyr::inner_join(smoke, full, by = c("scenario", "replicate"), suffix = c("_smoke", "_full"))
  expect_equal(nrow(in_full), nrow(smoke))
  expect_identical(in_full$data_seed_smoke, in_full$data_seed_full)
  expect_equal(anyDuplicated(full$data_seed), 0L)
  expect_identical(full$data_seed,
                   as.integer(recovery_plan$regression$seed) + 1000L * match(full$scenario, recovery_scenarios) +
                     full$replicate)
})

test_that("every scenario sets the motive coefficients it names and keeps the covariates of the truth", {
  expect_named(recovery_coefficients, recovery_scenarios)
  working <- lapply(recovery_truth$true_beta, unlist)
  for (scenario in recovery_scenarios) {
    expect_named(recovery_coefficients[[scenario]], recovery_outcomes)
    for (outcome in recovery_outcomes) {
      beta <- recovery_coefficients[[scenario]][[outcome]]
      expect_named(beta, c(recovery_motives, "age", "male", "income"))
      expect_equal(beta[c("age", "male", "income")], working[[outcome]][c("age", "male", "income")],
                   info = paste(scenario, outcome))
    }
  }
  zero <- unlist(lapply(recovery_coefficients$zero, function(b) b[recovery_motives]))
  expect_true(all(zero == 0))
  for (outcome in recovery_outcomes) {
    expect_equal(recovery_coefficients$working_range[[outcome]][recovery_motives],
                 working[[outcome]][recovery_motives])
    signs <- vapply(recovery_motives, function(motive) {
      sign <- recovery_plan$predictions$table[[outcome]][[motive]]
      if (identical(sign, "+")) 1 else if (identical(sign, "-")) -1 else 0
    }, numeric(1))
    expect_equal(recovery_coefficients$close_anchor_0.37[[outcome]][recovery_motives], 0.37 * signs)
  }
  # the most strongly correlated pair of the truth matrix, opposite signs, the
  # mean absolute non-zero working-range coefficient as magnitude
  nonzero <- unlist(lapply(working[recovery_outcomes], function(b) b[recovery_motives][b[recovery_motives] != 0]))
  for (outcome in recovery_outcomes) {
    beta <- recovery_coefficients$opposing_signs_among_correlated_motives[[outcome]][recovery_motives]
    expect_equal(unname(beta[c("zm_power", "zm_prestige")]), c(1, -1) * mean(abs(nonzero)))
    expect_true(all(beta[setdiff(recovery_motives, c("zm_power", "zm_prestige"))] == 0))
  }
  expect_equal(mean(abs(nonzero)), 0.182)
})

test_that("a simulated data set has the shape of data_regressions, complete and standardised", {
  tasks <- build_recovery_tasks(recovery_plan, 2L)
  task <- tasks[tasks$scenario == "working_range" & tasks$replicate == 1L, ]
  data <- simulate_recovery_dataset(task, recovery_coefficients, recovery_truth, recovery_plan, recovery_codebook)
  z_columns <- zm_z_col(ap3_regression_z_variables(recovery_plan, recovery_codebook), recovery_codebook)
  expect_identical(names(data), c("respondent_id", "gender", z_columns))
  model_set <- define_recovery_model_set(recovery_plan)
  expect_true(all(unique(unlist(lapply(model_set$formulas, all.vars))) %in% names(data)))
  expect_equal(nrow(data), recovery_plan$prior_recovery$n_per_replicate)
  expect_false(anyNA(data))
  for (column in z_columns) {
    expect_equal(mean(data[[column]]), 0, tolerance = 1e-12, info = column)
    expect_equal(stats::sd(data[[column]]), 1, tolerance = 1e-12, info = column)
  }
  expect_s3_class(data$gender, "factor")
  expect_identical(levels(data$gender)[1], names(which.max(table(data$gender))))
  expect_identical(attr(data, "z_parameters")$z_col, z_columns)
  # deterministic under the task's data seed, and a different seed draws a different data set
  expect_identical(simulate_recovery_dataset(task, recovery_coefficients, recovery_truth, recovery_plan,
                                             recovery_codebook), data)
  other <- simulate_recovery_dataset(tasks[tasks$scenario == "working_range" & tasks$replicate == 2L, ],
                                     recovery_coefficients, recovery_truth, recovery_plan, recovery_codebook)
  expect_false(isTRUE(all.equal(other$asc_agg_z, data$asc_agg_z)))
})

test_that("the recovery model set fits the configured prior widths", {
  model_set <- define_recovery_model_set(recovery_plan)
  expect_equal(model_set$metric_slope_priors, recovery_slope_sds)
  expect_equal(model_set$primary_slope_sd, recovery_plan$priors$slope_sd_primary)
  expect_identical(model_set$formulas, define_regression_model_set(recovery_plan)$formulas)
})

# A coefficient table in the shape fit_recovery_regressions() returns, every
# cell with the interval (estimate - 0.1, estimate + 0.1).
made_up_coefficients <- function(estimate = 0.05) {
  cells <- tidyr::expand_grid(outcome_key = recovery_outcomes, slope_sd = recovery_slope_sds,
                              term = recovery_motives)
  cells$estimate <- estimate
  cells$q_lo <- estimate - 0.1
  cells$q_hi <- estimate + 0.1
  cells$fit_valid <- TRUE
  cells$gate_status <- "ok"
  cells$sampler_error <- NA_character_
  cells
}

test_that("the rows of one task carry every cell and the sampler error of a fit", {
  task <- build_recovery_tasks(recovery_plan, 2L)[3L, ]
  coefficients <- made_up_coefficients()
  # a fit whose sampling stopped with an error
  sampler_failed <- coefficients$outcome_key == "sdo_dom" & coefficients$slope_sd == 0.4
  coefficients$estimate[sampler_failed] <- NA_real_
  coefficients$q_lo[sampler_failed] <- NA_real_
  coefficients$q_hi[sampler_failed] <- NA_real_
  coefficients$fit_valid[sampler_failed] <- FALSE
  coefficients$gate_status[sampler_failed] <- "sampler_error"
  coefficients$sampler_error[sampler_failed] <- "No chains finished successfully."
  # a fit that failed the gate
  gate_failed <- coefficients$outcome_key == "asc_sub" & coefficients$slope_sd == 0.1
  coefficients$fit_valid[gate_failed] <- FALSE
  coefficients$gate_status[gate_failed] <- "not_interpretable"
  rows <- summarise_recovery_fit(coefficients, task, recovery_coefficients, recovery_plan)
  expect_named(rows, c("outcome", "slope_sd", "term", "true", "estimate", "lo", "hi", "fit_valid",
                       "gate_status", "sampler_error", "scenario", "replicate", "data_seed"))
  expect_equal(nrow(rows), length(recovery_outcomes) * length(recovery_slope_sds) * length(recovery_motives))
  expect_true(all(rows$scenario == task$scenario & rows$replicate == task$replicate &
                    rows$data_seed == task$data_seed))
  truth <- recovery_coefficients[[task$scenario]]
  expect_equal(rows$true, vapply(seq_len(nrow(rows)), function(i) truth[[rows$outcome[i]]][[rows$term[i]]],
                                 numeric(1)))
  errored <- rows$outcome == "sdo_dom" & rows$slope_sd == 0.4
  expect_true(all(is.na(rows$estimate[errored])))
  expect_true(all(!rows$fit_valid[errored] & rows$gate_status[errored] == "sampler_error"))
  expect_true(all(rows$sampler_error[errored] == "No chains finished successfully."))
  not_interpretable <- rows$outcome == "asc_sub" & rows$slope_sd == 0.1
  expect_true(all(!rows$fit_valid[not_interpretable] &
                    rows$gate_status[not_interpretable] == "not_interpretable"))
  expect_true(all(is.na(rows$sampler_error[!errored])))
  expect_true(all(rows$fit_valid[!errored & !not_interpretable]))
  expect_equal(rows$lo[!errored], rows$estimate[!errored] - 0.1)

  # a cell without a row is no outcome of a fit: the task stops
  without_fit <- coefficients[!(coefficients$outcome_key == "asc_agg" & coefficients$slope_sd == 0.2), ]
  expect_error(summarise_recovery_fit(without_fit, task, recovery_coefficients, recovery_plan),
               "no coefficient for asc_agg at slope SD 0.2")
})

# A sampler failure as brms's fitting step meets it: brms:::fit_model() takes
# its cmdstanr route, and the sampler of the stand-in model stops.
fail_in_sampler <- function(message = "No chains finished successfully.") {
  sampler_step <- utils::getFromNamespace("fit_model", "brms")
  sampler_step(
    model = list(sample = function(...) stop(message)),
    backend = "cmdstanr", sdata = list(), algorithm = "sampling", iter = 2, warmup = 1, thin = 1,
    chains = 1, cores = 1, threads = brms::threading(), opencl = brms::opencl(), init = NULL,
    exclude = NULL, seed = 1, control = list(), silent = 2, future = FALSE
  )
}

# Stand-ins for the setup and the AP6 fitting, gate and result verbs, bound in
# the global environment until `frame` ends. Every fit is a `brmsfit` shell
# naming its outcome and slope SD; `fail(outcome, slope_sd)` picks the fits whose
# sampler stops. The result verb gives every motive of a fit the interval
# (-0.05, 0.15) around 0.05, and an unavailable row for a fit without a result.
local_recovery_verbs <- function(fail = function(outcome, slope_sd) FALSE,
                                 gate = function(fits, analysis_plan) fits,
                                 result = NULL, frame = parent.frame()) {
  shell <- function(outcome, slope_sd) {
    if (fail(outcome, slope_sd)) fail_in_sampler()
    structure(list(outcome = outcome, slope_sd = slope_sd), class = "brmsfit")
  }
  outcome_keys <- define_recovery_model_set(recovery_plan)$outcome_keys
  rows_of <- function(fits, role, slope_sd) {
    dplyr::bind_rows(lapply(names(fits), function(outcome) {
      if (!inherits(fits[[outcome]], "brmsfit")) {
        return(tibble::tibble(outcome_key = outcome_keys[[outcome]], role = role, slope_sd = slope_sd,
                              term_type = NA_character_))
      }
      tibble::tibble(outcome_key = outcome_keys[[outcome]], role = role, slope_sd = slope_sd,
                     term = recovery_motives, term_type = "predictor", estimate = 0.05, q_lo = -0.05,
                     q_hi = 0.15, fit_valid = TRUE, gate_status = "ok")
    }))
  }
  if (is.null(result)) {
    result <- function(primary_fits, comparison_fits, student_fits, analysis_plan) {
      dplyr::bind_rows(
        rows_of(primary_fits, "primary", analysis_plan$priors$slope_sd_primary),
        lapply(names(comparison_fits), function(width) rows_of(comparison_fits[[width]], "sweep", as.numeric(width)))
      )
    }
  }
  rlang::local_bindings(
    zm_setup = function(...) invisible(NULL),
    fit_primary_regressions = function(data, model_set, analysis_plan) {
      lapply(stats::setNames(nm = names(model_set$formulas)), shell, slope_sd = model_set$primary_slope_sd)
    },
    fit_prior_width_comparisons = function(data, model_set, analysis_plan) {
      widths <- model_set$metric_slope_priors[model_set$metric_slope_priors != model_set$primary_slope_sd]
      stats::setNames(lapply(widths, function(width) {
        lapply(stats::setNames(nm = names(model_set$formulas)), shell, slope_sd = width)
      }), sprintf("%.2f", widths))
    },
    apply_validity_gate = gate,
    extract_regression_coefficient_summaries = result,
    .env = globalenv(),
    .frame = frame
  )
}

test_that("only an error of brms's sampling step is returned as a sampler error", {
  caught <- capture_recovery_sampler_error(fail_in_sampler("All chains failed."))
  expect_s3_class(caught, "recovery_sampler_error")
  expect_identical(conditionMessage(caught), "All chains failed.")
  expect_identical(capture_recovery_sampler_error(1 + 1), 2)
  expect_error(capture_recovery_sampler_error(stop("gate code failed")), "gate code failed")
  # brms's own checks before sampling are not the sampler
  checked <- tryCatch(
    capture_recovery_sampler_error(brms::brm(y ~ x, data = data.frame(y = c(0, 1)), backend = "cmdstanr")),
    error = identity
  )
  expect_s3_class(checked, "error")
  expect_false(inherits(checked, "recovery_sampler_error"))
})

test_that("a sampler error of one fit becomes that fit's rows, and the summary counts its cells", {
  local_recovery_verbs(fail = function(outcome, slope_sd) outcome == "sdo_d_dominance" && slope_sd == 0.4)
  tasks <- build_recovery_tasks(recovery_plan, 1L)
  task <- tasks[tasks$scenario == "working_range", ]
  # the command of the branch target
  rows <- task |>
    simulate_recovery_dataset(recovery_coefficients, recovery_truth, recovery_plan, recovery_codebook) |>
    fit_recovery_regressions(define_recovery_model_set(recovery_plan), recovery_plan) |>
    summarise_recovery_fit(task, recovery_coefficients, recovery_plan)
  errored <- rows$outcome == "sdo_dom" & rows$slope_sd == 0.4
  expect_equal(sum(errored), length(recovery_motives))
  expect_true(all(rows$sampler_error[errored] == "No chains finished successfully."))
  expect_true(all(rows$gate_status[errored] == "sampler_error" & !rows$fit_valid[errored] &
                    is.na(rows$estimate[errored])))
  # the other fits of the data set keep their coefficients
  expect_true(all(rows$fit_valid[!errored] & is.na(rows$sampler_error[!errored])))
  expect_equal(rows$estimate[!errored], rep(0.05, sum(!errored)))

  summary <- summarise_recovery_rows(score_recovery_rows(rows), recovery_plan)
  all_rows <- summary[summary$true_effect == "all", ]
  wide <- all_rows[all_rows$slope_sd == 0.4, ]
  expect_equal(wide$n_cells_error, length(recovery_motives))
  expect_equal(wide$n_cells_valid, wide$n_cells - length(recovery_motives))
  expect_equal(wide$share_fits_valid, 3 / 4)
  expect_true(all(all_rows$n_cells_error[all_rows$slope_sd != 0.4] == 0L))
})

# The rows of one task from a data set the stand-in verbs never read.
run_recovery_branch <- function() {
  task <- build_recovery_tasks(recovery_plan, 1L)[1L, ]
  tibble::tibble() |>
    fit_recovery_regressions(define_recovery_model_set(recovery_plan), recovery_plan) |>
    summarise_recovery_fit(task, recovery_coefficients, recovery_plan)
}

test_that("an error of the gate after sampling stops the task instead of becoming rows", {
  local_recovery_verbs(gate = function(fits, analysis_plan) stop("gate code failed"))
  expect_error(run_recovery_branch(), "gate code failed")
})

test_that("an error of the result extraction stops the task instead of becoming rows", {
  local_recovery_verbs(result = function(...) stop("extraction failed"))
  expect_error(run_recovery_branch(), "extraction failed")
})

test_that("a fit the verbs return without a result stops the task", {
  local_recovery_verbs()
  rlang::local_bindings(
    fit_primary_regressions = function(data, model_set, analysis_plan) {
      unavailable <- structure(list(message = "inputs unavailable", call = NULL),
                               class = c("imputation_unavailable", "error", "condition"))
      stats::setNames(list(unavailable), names(model_set$formulas))
    },
    .env = globalenv()
  )
  expect_error(run_recovery_branch(), "has no result: inputs unavailable")
})

test_that("an error before the sampler stops the task and leaves no Stan files behind", {
  local_recovery_verbs()
  output_dir_before <- getOption("cmdstanr_output_dir")
  seen_output_dir <- NULL
  rlang::local_bindings(
    fit_primary_regressions = function(...) {
      seen_output_dir <<- getOption("cmdstanr_output_dir")
      writeLines("{}", file.path(tempdir(), "standata-recovery-test.json"))
      stop("prior code failed")
    },
    .env = globalenv()
  )
  expect_error(run_recovery_branch(), "prior code failed")
  expect_false(is.null(seen_output_dir))
  expect_false(dir.exists(seen_output_dir))
  expect_false(file.exists(file.path(tempdir(), "standata-recovery-test.json")))
  expect_identical(getOption("cmdstanr_output_dir"), output_dir_before)
})

test_that("the scoring and the summary state the rates the preregistration reads", {
  tasks <- build_recovery_tasks(recovery_plan, 2L)
  rows <- dplyr::bind_rows(lapply(seq_len(nrow(tasks)), function(k) {
    summarise_recovery_fit(made_up_coefficients(0.15), tasks[k, ], recovery_coefficients, recovery_plan)
  }))
  scored <- score_recovery_rows(rows)
  # an interval (0.05, 0.25) is credible and positive: it covers 0.15 and 0.2,
  # confirms a positive truth, is a directional error at a negative or zero truth
  expect_true(all(scored$credible))
  expect_equal(scored$covered, scored$true >= 0.05 & scored$true <= 0.25)
  expect_equal(scored$confirmed, scored$true > 0)
  expect_equal(scored$directional_error, scored$true <= 0)
  expect_equal(scored$error, 0.15 - scored$true)

  summary <- summarise_recovery_rows(scored, recovery_plan)
  expect_named(summary, c("scenario", "slope_sd", "true_effect", "n_cells", "n_cells_valid", "n_replicates",
                          "share_fits_valid", "n_cells_error", "coverage_95", "directional_error_rate",
                          "confirmed_rate", "bias", "n_per_replicate", "chains", "iter_per_chain", "warmup",
                          "ess_target", "ci_level"))
  expect_true(all(summary$n_cells_error == 0L))
  all_rows <- summary[summary$true_effect == "all", ]
  expect_equal(nrow(all_rows), length(recovery_scenarios) * length(recovery_slope_sds))
  expect_true(all(all_rows$n_cells == 2L * length(recovery_outcomes) * length(recovery_motives)))
  expect_true(all(summary$n_replicates == 2L))
  zero <- all_rows[all_rows$scenario == "zero", ]
  expect_true(all(zero$directional_error_rate == 1 & is.na(zero$confirmed_rate) & zero$bias == 0.15))
  anchor <- summary[summary$scenario == "close_anchor_0.37" & summary$true_effect != "all", ]
  expect_setequal(unique(anchor$true_effect), c("-0.37", "0.37"))
  expect_true(all(anchor$coverage_95[anchor$true_effect == "0.37"] == 0))
  expect_true(all(anchor$confirmed_rate[anchor$true_effect == "0.37"] == 1))
  expect_true(all(summary$n_per_replicate == recovery_plan$prior_recovery$n_per_replicate))
  expect_true(all(summary$chains == recovery_plan$regression$chains &
                    summary$ess_target == recovery_plan$regression$ess_target))
  # the rates rest on the fits that passed the gate
  scored$fit_valid[scored$slope_sd == 0.1 & scored$outcome == "asc_agg"] <- FALSE
  gated <- summarise_recovery_rows(scored, recovery_plan)
  narrow <- gated[gated$true_effect == "all" & gated$slope_sd == 0.1, ]
  expect_true(all(narrow$n_cells_valid == narrow$n_cells * 3 / 4))
  expect_equal(narrow$share_fits_valid, rep(3 / 4, nrow(narrow)))
})

test_that("a smoke run writes files of its own, never the files of the full run", {
  full <- select_recovery_run(recovery_plan, "prior_recovery")
  smoke <- select_recovery_run(recovery_plan, "prior_recovery_smoke")
  expect_identical(full$replicates, as.integer(recovery_plan$prior_recovery$replicates))
  expect_identical(smoke$replicates, as.integer(recovery_plan$prior_recovery$smoke_replicates))
  expect_named(full$paths, c("summary", "figure", "coefficients"))
  expect_length(intersect(full$paths, smoke$paths), 0L)
  expect_identical(unname(basename(full$paths)),
                   c("prior_recovery_summary.csv", "prior_recovery_summary.png", "prior_recovery_coefficients.csv"))
  expect_true(all(startsWith(basename(smoke$paths), "prior_recovery_smoke_")))
  expect_error(select_recovery_run(recovery_plan, "smoke"), "TAR_PROJECT=prior_recovery")
})
