# config/analysis_plan.yaml: internal consistency of the analysis-plan values
# (prediction table, labelled prior sweep, joint model prior, Student-t with
# fixed nu and matched scale prior, validity gate with its failure rule,
# power-scaling blocks, the missing-value fill, data-file allowlists and intake
# lists, simulation study, factor-analytic sets, network nodes, regression
# formula)

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
plan_path <- file.path(root, "config", "analysis_plan.yaml")
plan_yaml <- yaml::read_yaml(plan_path)
analysis_plan <- zm_config(profile = "full", path = plan_path)
codebook <- zm_codebook(analysis_plan)

sd_key <- function(x) format(as.numeric(x), trim = TRUE, drop0trailing = TRUE, scientific = FALSE)

test_that("the prediction table has 16 signed and 4 ± cells over the configured outcomes and motives", {
  tab <- plan_yaml$predictions$table
  outcomes <- as.character(plan_yaml$regression$outcomes)
  motives <- as.character(plan_yaml$regression$motives)
  expect_setequal(names(tab), outcomes)
  signs <- unlist(lapply(tab, function(cells) {
    expect_setequal(names(cells), motives)
    vapply(cells, function(v) if (is.null(v)) "" else as.character(v), character(1))
  }))
  expect_length(signs, length(outcomes) * length(motives))
  expect_true(all(signs %in% c("+", "-", "±", "")))
  expect_equal(sum(signs %in% c("+", "-")), 16)
  expect_equal(sum(signs == "±"), 4)
  expect_equal(sum(signs == ""), 0)
  # the four ± cells are the prestige cells
  expect_true(all(vapply(tab, function(cells) identical(cells$zm_prestige, "±"), logical(1))))
})

test_that("the prior sweep is 0.10 / 0.20 / 0.40 with labels and contains the primary SD", {
  pr <- plan_yaml$priors
  primary <- pr$slope_sd_primary
  sweep <- as.numeric(unlist(pr$slope_sd_sweep))
  expect_true(is.numeric(primary) && length(primary) == 1)
  expect_true(primary %in% sweep)
  expect_equal(anyDuplicated(sweep), 0)
  expect_true(all(sweep > 0))
  expect_length(sweep, 3)
  expect_true(analysis_plan$priors$slope_sd_primary %in% analysis_plan$priors$slope_sd_sweep)
  # one label per SD of the sweep, keyed by the SD without trailing zeros; the primary SD is labelled "primary"
  labels <- pr$sweep_labels
  expect_true(is.list(labels))
  expect_setequal(names(labels), sd_key(sweep))
  expect_true(all(vapply(labels, function(l) is.character(l) && nzchar(l), logical(1))))
  expect_identical(labels[[sd_key(primary)]], "primary")
  expect_true(is.character(pr$sweep_block) && nzchar(pr$sweep_block))
  for (nm in c("gender_sd", "intercept_sd", "sigma_sd")) {
    expect_true(is.numeric(pr[[nm]]) && pr[[nm]] > 0, info = nm)
  }
})

test_that("the joint RQ2 model is declared with the LKJ(2) prior on the residual correlations", {
  # AP7: the four regressions are fitted jointly, with prior(lkj(2), class = rescor)
  expect_true(isTRUE(plan_yaml$regression$multivariate$fitted))
  expect_true(isTRUE(analysis_plan$regression$multivariate$fitted))
  expect_identical(plan_yaml$priors$rescor, "lkj(2)")
  expect_identical(analysis_plan$priors$rescor, "lkj(2)")
  # the joint model shares the formulas and priors of the four fits: no second
  # right-hand side, no second prior width
  expect_null(plan_yaml$regression$multivariate$predictors)
  expect_null(plan_yaml$regression$multivariate$slope_sd)
})

test_that("the Student-t refit is a one-sided tail trigger with nu fixed at 4 and a matched scale prior", {
  st <- plan_yaml$sensitivity$student_t
  expect_false(is.null(st))
  expect_identical(st$trigger, "one_sided_tails")
  expect_identical(analysis_plan$sensitivity$student_t$trigger, "one_sided_tails")
  stats <- as.character(unlist(st$trigger_stats))
  expect_gt(length(stats), 0)
  expect_true(all(stats %in% c("kurtosis", "min", "max")), info = paste(stats, collapse = ", "))
  expect_equal(anyDuplicated(stats), 0)
  expect_identical(as.numeric(st$nu_fixed), 4)
  expect_identical(as.numeric(analysis_plan[["sensitivity"]][["student_t"]][["nu_fixed"]]), 4)
  # half-normal scale SD 1/sqrt(2): induced residual-SD prior equals the Gaussian half-normal(0, sigma_sd)
  expect_equal(st$sigma_scale_prior_sd, plan_yaml$priors$sigma_sd / sqrt(2), tolerance = 1e-12)
  expect_identical(st$role, "descriptive")
})

test_that("the validity gate carries every threshold and the four-step failure rule", {
  gate <- plan_yaml$regression$validity_gate
  expect_false(is.null(gate))
  for (nm in c("ess_bulk_min", "ess_tail_min", "rhat_max", "divergences_max", "treedepth_hits_max", "bfmi_min",
               "adapt_delta_retry", "max_treedepth_retry")) {
    expect_true(is.numeric(gate[[nm]]) && length(gate[[nm]]) == 1, info = nm)
  }
  expect_equal(gate$rhat_max, plan_yaml$regression$rhat_max)
  expect_equal(gate$ess_bulk_min, plan_yaml$profiles$full$regression_ess_target)
  expect_equal(gate$divergences_max, 0)
  expect_equal(gate$treedepth_hits_max, 0)
  expect_true(gate$bfmi_min > 0 && gate$bfmi_min < 1)
  steps <- as.character(unlist(gate$on_failure))
  expect_length(steps, 4)
  expect_match(steps[1], "ESS")
  expect_match(steps[2], "adapt_delta")
  expect_match(steps[4], "not interpretable")
  expect_true(gate$adapt_delta_retry > 0.9 && gate$adapt_delta_retry < 1)
  expect_true(gate$max_treedepth_retry > 10)
  r2 <- plan_yaml$regression$r2
  expect_true(is.character(r2$definition) && grepl("var\\(mu\\)", r2$definition))
  expect_identical(plan_yaml$regression$point_estimate, "median")
})

test_that("the power-scaling check lists the prior blocks with the labelled all-priors run", {
  ps <- plan_yaml$sensitivity$powerscale
  blocks <- as.character(unlist(ps$blocks))
  expect_setequal(blocks, c("metric_slopes", "gender_contrasts", "intercept", "sigma", "all_priors"))
  expect_equal(anyDuplicated(blocks), 0)
  expect_setequal(as.character(unlist(ps$component)), c("prior", "likelihood"))
  expect_true(ps$lower_alpha < 1 && ps$upper_alpha > 1)
  expect_true(is.character(ps$div_measure) && nzchar(ps$div_measure))
  expect_true(is.numeric(ps$sensitivity_threshold) && ps$sensitivity_threshold > 0)
  expect_match(ps$inspected, "R2")
})

test_that("the missing-data block carries the fill", {
  md <- plan_yaml$missing_data
  # the fill is the whole missing-data rule
  expect_true(is.list(md$fill))
  expect_identical(md$fill$stage, "before_scale_scores")
  expect_true(isTRUE(md$standardise_within_model))
  expect_identical(plan_yaml$income$hh_size_top_value, 8L)
  expect_match(plan_yaml$free_text_comment$handling, "internal")
})

test_that("the plan carries no ROPE block at all", {
  # AP9 closes the question: "No region of practical equivalence is applied,
  # because no substantively justified smallest effect of interest has been
  # established." A config block calling the decision pending invites a
  # post-hoc ROPE, so the block must be absent rather than set to FALSE.
  expect_null(plan_yaml$sensitivity$rope)
  expect_null(analysis_plan$sensitivity$rope)
  src <- readLines(file.path(root, "config", "analysis_plan.yaml"), warn = FALSE)
  expect_false(any(grepl("^\\s{2}rope:", src)))
})

test_that("the data-file block carries the intake lists and the two-file disclosure boundary", {
  df <- plan_yaml$data_files
  expect_false(is.null(df))
  intake <- df$intake
  expect_true(isTRUE(intake$require_manual_approval))
  for (nm in c("proposal_file", "approval_file")) {
    expect_true(is.character(intake[[nm]]) && grepl("\\.yaml$", intake[[nm]]), info = nm)
  }
  expect_false(identical(intake$proposal_file, intake$approval_file))
  for (nm in c("delete_always", "delete_if_present", "delete_after_billing")) {
    lst <- as.character(unlist(intake[[nm]]))
    expect_gt(length(lst), 0)
    expect_equal(anyDuplicated(lst), 0, info = nm)
  }
  all_delete <- as.character(unlist(intake[c("delete_always", "delete_if_present", "delete_after_billing")]))
  expect_equal(anyDuplicated(all_delete), 0)
  for (nm in c("ResponseId", "StartDate", "Finished", "survey_status")) {
    expect_true(nm %in% as.character(unlist(intake$delete_always)), info = nm)
  }
  for (nm in c("IPAddress", "LocationLatitude", "LocationLongitude", "RecipientEmail")) {
    expect_true(nm %in% as.character(unlist(intake$delete_if_present)), info = nm)
  }
  expect_identical(as.character(unlist(intake$delete_after_billing)), "bilendi_id")
  expect_true(is.character(intake$respondent_id) && nzchar(intake$respondent_id))
  sci <- as.character(unlist(df$scientific_use_file$keep))
  expect_true(all(c("respondent_id", "items", "scale_scores", "demo_gender", "quota_group",
                    "pol_symp", "pol_party_vote") %in% sci))
  expect_true(all(c("age_band", "age_band_midpoint", "income_band", "income_band_value") %in% sci))
  expect_setequal(as.character(unlist(df$scientific_use_file$analysis_value_columns)),
                  c("age_band_midpoint", "income_band_value"))
  dropped <- as.character(unlist(df$scientific_use_file$drop))
  expect_true(all(c("demo_age", "income", "demo_edu_school", "east_west",
                    "Kommentarfeld", "demo_bundesland", "pol_party_vote_801_TEXT",
                    "demo_income_hh_net", "demo_hh_members", "Duration__in_seconds_",
                    "pol_vote_would", "pol_left_right") %in% dropped))
  expect_length(intersect(dropped, sci), 0)
  expect_false(any(all_delete %in% sci))
  expect_identical(df$demographics_file$path, "data/derived/deidentified_demographics.csv")
  expect_identical(df$comments_file$path, "data/private/comments.csv")
  # The smallest divers group the analysis and the shared file keep.
  expect_identical(plan_yaml$exclusions$gender_divers_min_n, 30L)
})

test_that("the prior-recovery simulation is declared", {
  sim <- plan_yaml$prior_recovery
  expect_true(is.numeric(sim$n_per_replicate) && sim$n_per_replicate > 0)
  expect_true(is.numeric(sim$replicates) && sim$replicates > 0)
  expect_true(is.numeric(sim$smoke_replicates) && sim$smoke_replicates < sim$replicates)
  expect_setequal(names(sim$sampling), c("chains", "warmup", "iter_per_chain", "ess_target"))
  expect_setequal(as.numeric(unlist(sim$slope_sds)), as.numeric(unlist(plan_yaml$priors$slope_sd_sweep)))
  scenarios <- as.character(unlist(sim$effect_scenarios))
  expect_true(all(c("zero", "working_range") %in% scenarios))
  expect_true(any(startsWith(scenarios, "close_anchor_")))
  anchor <- as.numeric(sub("^close_anchor_", "", scenarios[startsWith(scenarios, "close_anchor_")]))
  expect_true(all(is.finite(anchor) & anchor > 0))
  expect_setequal(
    as.character(unlist(sim$outcomes_recorded)),
    c("coverage_95", "directional_error_rate", "confirmed_rate_by_true_effect", "bias")
  )
})

test_that("the factor-analytic sets name codebook scales, one factor per intended scale, with a display cutoff and an index list", {
  fa <- plan_yaml$factor_analysis
  expect_false(is.null(fa))
  cutoff <- fa$loading_display_cutoff
  expect_true(is.numeric(cutoff) && length(cutoff) == 1)
  expect_true(cutoff > 0 && cutoff < 1)
  expect_equal(analysis_plan$factor_analysis$loading_display_cutoff, cutoff)
  reporting <- fa$cfa_reporting
  expect_equal(reporting$ranked_residuals_n, 3)
  expect_equal(
    analysis_plan$factor_analysis$cfa_reporting$residual_data_file,
    "data/derived/cfa_pairwise_residuals.csv"
  )
  indices <- vapply(fa$factor_number_criteria, function(x) as.character(x$index), character(1))
  expect_length(indices, 9L)
  expect_equal(anyDuplicated(indices), 0)
  expect_true(all(nzchar(indices)))
  # Both Kaiser rules are reported: the classical eigenvalue-greater-than-one
  # rule and the empirical criterion preferred by the simulation literature.
  expect_true(all(c("kaiser", "empirical_kaiser") %in% indices))
  expect_identical(fa$rotation_primary, "oblimin")
  expect_equal(fa$rotation_settings$oblimin_gamma, 0)
  expect_equal(fa$rotation_settings$starting_rotations, 20)
  expect_equal(analysis_plan$factor_analysis$rotation_settings, fa$rotation_settings)
  expect_equal(analysis_plan$factor_analysis$rotation_primary, fa$rotation_primary)
  fa_text <- paste(readLines(file.path(root, "R", "ap4_factor_structure.R"), warn = FALSE), collapse = "\n")
  efa_text <- paste(readLines(file.path(root, "R", "ap9_efa.R"), warn = FALSE), collapse = "\n")
  expect_match(efa_text, "empirical_kaiser", fixed = TRUE)
  expect_match(efa_text, "EFAtools::EKC", fixed = TRUE)
  # The primary rotation is not hard-coded in the files
  # that fit exploratory solutions.
  rel_text <- paste(readLines(file.path(root, "R", "ap4_reliability.R"), warn = FALSE), collapse = "\n")
  for (rot in fa$rotation_primary) {
    for (nm in c("ap4_factor_structure.R", "ap4_reliability.R", "ap9_efa.R")) {
      txt <- switch(nm, ap4_factor_structure.R = fa_text, ap4_reliability.R = rel_text, efa_text)
      expect_false(grepl(paste0('rotate = "', rot, '"'), txt, fixed = TRUE),
                   info = paste(nm, rot))
    }
  }
  # The EFA route takes its rotations from the specification and from the
  # plan; neither the fitting verb nor the specification verb carries a default
  # rotation that a call site could inherit.
  source(file.path(root, "R", "ap9_efa.R"), local = TRUE)
  expect_match(efa_text, "rotate = specification$rotation", fixed = TRUE)
  for (argument in c("one_factor", "multiple_factors")) {
    expect_true(argument %in% names(formals(add_efa_rotation_methods)), info = argument)
    expect_true(identical(formals(add_efa_rotation_methods)[[argument]], quote(expr = )),
                info = argument)
  }
  # the very-simple-structure criterion rotates its trial solutions too, so it
  # reads the plan's primary rotation rather than a value written in the code
  expect_match(efa_text, "rotate = analysis_plan$factor_analysis$rotation_primary", fixed = TRUE)
  sets <- fa$sets
  expect_true(is.list(sets) && length(sets) > 0)
  keys <- vapply(sets, function(s) as.character(s$key), character(1))
  expect_equal(anyDuplicated(keys), 0)
  scale_keys <- codebook$scales$scale_key
  # every set carries a label once the plan is loaded: the plan's own, or for
  # the subscales of one instrument the instrument and its subscales from
  # codebook_scales.csv
  loaded_labels <- vapply(analysis_plan$factor_analysis$sets, function(s) as.character(s$label), character(1))
  expect_true(all(nzchar(loaded_labels)))
  expect_identical(stats::setNames(loaded_labels, vapply(analysis_plan$factor_analysis$sets, function(s) s$key, "")),
                   c(asc = "ASC", dopl = "DoPL-6 (dominance and prestige)", ums = "UMS-6 (intimacy and achievement)",
                     motives = "All social motive scales", outcomes = "ASC and SDO-D"))
  for (s in sets) {
    info <- s$key
    set_scales <- as.character(unlist(s$scales))
    expect_gt(length(set_scales), 0)
    expect_equal(anyDuplicated(set_scales), 0, info = info)
    expect_true(all(set_scales %in% scale_keys), info = paste(info, paste(setdiff(set_scales, scale_keys), collapse = ", ")))
    # the confirmatory model of a set is one factor per intended scale
    expect_equal(as.integer(s$expected_factors), length(set_scales), info = info)
    expect_true(is.logical(s$efa) && !is.na(s$efa), info = info)
    expect_true(is.logical(s$cfa) && !is.na(s$cfa), info = info)
  }
  efa_keys <- keys[vapply(sets, function(s) isTRUE(s$efa), logical(1))]
  cfa_keys <- keys[vapply(sets, function(s) isTRUE(s$cfa), logical(1))]
  expect_gt(length(efa_keys), 0)
  expect_gt(length(cfa_keys), 0)
  expect_true(all(cfa_keys %in% efa_keys))
  # every scale of the codebook is covered by at least one set
  covered <- unique(unlist(lapply(sets, function(s) as.character(unlist(s$scales)))))
  expect_setequal(covered, scale_keys)
})

test_that("the network nodes are the nine codebook scale keys, mapped to their z columns", {
  node_keys <- as.character(plan_yaml$network$nodes)
  keys <- codebook$scales$scale_key
  expect_length(keys, 9)
  expect_length(node_keys, 9)
  expect_equal(anyDuplicated(node_keys), 0)
  expect_false(any(endsWith(node_keys, "_z")))
  expect_setequal(node_keys, keys)
  expect_setequal(
    node_keys,
    c(as.character(plan_yaml$regression$outcomes), as.character(plan_yaml$regression$motives))
  )
  # the loader puts the nodes into the codebook's order and reads the
  # standardised column of each node from the codebook
  expect_identical(as.character(analysis_plan$network$node_keys), as.character(keys))
  expect_identical(as.character(analysis_plan$network$nodes), zm_z_col(keys, codebook))
})

test_that("the regression formula is built from the motives in codebook order and the covariates", {
  reg <- plan_yaml$regression
  # the plan names the covariates by key; the loader maps every metric one to
  # its standardised column and leaves the gender factor as it is
  expect_setequal(as.character(reg$covariates), c("age", "gender", "income"))
  expect_identical(as.character(analysis_plan$regression$covariate_keys), as.character(reg$covariates))
  terms <- trimws(strsplit(analysis_plan$regression$predictors, "+", fixed = TRUE)[[1]])
  motives <- zm_order_scale_keys(reg$motives, codebook)
  expect_identical(terms, c(zm_z_col(motives, codebook), as.character(analysis_plan$regression$covariates)))
  expect_identical(analysis_plan$regression$predictors,
                   "zm_security_z + zm_arousal_z + zm_power_z + zm_prestige_z + zm_achievement_z + age_z + gender + income_z")
})

test_that("the plan's lists of scales are written in the order of codebook_scales.csv", {
  # zm_config() orders every list whatever the plan writes; the plan is kept
  # in that order too, so that it reads as the analyses run.
  in_codebook_order <- function(keys) identical(as.character(unlist(keys)), zm_order_scale_keys(keys, codebook))
  expect_true(in_codebook_order(plan_yaml$regression$outcomes))
  expect_true(in_codebook_order(plan_yaml$regression$motives))
  expect_true(in_codebook_order(plan_yaml$network$nodes))
  for (set in plan_yaml$factor_analysis$sets) expect_true(in_codebook_order(set$scales), info = set$key)
  for (model in plan_yaml$confirmatory_models$models) {
    expect_true(in_codebook_order(unlist(model$factors)), info = model$key)
  }
  expect_true(in_codebook_order(names(plan_yaml$predictions$table)))
  for (outcome in names(plan_yaml$predictions$table)) {
    expect_true(in_codebook_order(names(plan_yaml$predictions$table[[outcome]])), info = outcome)
  }
})
