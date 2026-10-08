# Generated parameter cards mirror the canonical YAML without changing runtime analysis_plan use.

local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) {
    parent <- dirname(dir)
    if (identical(parent, dir)) stop("project root not found from ", getwd())
    dir <- parent
  }
  assign("parameter_card_root", dir, envir = globalenv())
  source(file.path(dir, "R", "parameter_cards.R"), local = FALSE)
})

# R/ap6_regressions.R carries one generated block above each construction.
ap6_card_stub <- c(
  "# BEGIN GENERATED PARAMETER CARD: AP6 FORMULA", "# stale",
  "# END GENERATED PARAMETER CARD: AP6 FORMULA",
  "# BEGIN GENERATED PARAMETER CARD: AP6 FAMILY", "# stale",
  "# END GENERATED PARAMETER CARD: AP6 FAMILY",
  "# BEGIN GENERATED PARAMETER CARD: AP6 PRIORS", "# stale",
  "# END GENERATED PARAMETER CARD: AP6 PRIORS",
  "# BEGIN GENERATED PARAMETER CARD: AP6 SAMPLING", "# stale",
  "# END GENERATED PARAMETER CARD: AP6 SAMPLING",
  "# BEGIN GENERATED PARAMETER CARD: AP6 INITIAL CONVERGENCE", "# stale",
  "# END GENERATED PARAMETER CARD: AP6 INITIAL CONVERGENCE",
  "# BEGIN GENERATED PARAMETER CARD: AP6 RETRY", "# stale",
  "# END GENERATED PARAMETER CARD: AP6 RETRY",
  "# BEGIN GENERATED PARAMETER CARD: AP6 FINAL CONVERGENCE", "# stale",
  "# END GENERATED PARAMETER CARD: AP6 FINAL CONVERGENCE"
)

# The card generator reads the scale codebook the way the pipeline does
# (meta$codebook_dir, i.e. ../preregistration beside the analysis root), so a
# temporary root gets the same two-directory layout.
pc_temp_root <- function(env = parent.frame()) {
  base <- withr::local_tempdir(.local_envir = env)
  root <- file.path(base, "analysis")
  dir.create(root)
  dir.create(file.path(base, "preregistration"))
  for (book in "codebook_scales.csv") {
    file.copy(file.path(parameter_card_root, "..", "preregistration", book),
              file.path(base, "preregistration", book))
  }
  root
}

# R/ap3_fill.R carries one card above each of the five fill verbs: the route
# card above the drop rule and one model card above each fit.
ap3_card_markers <- c("AP3 FILL ROUTE", "AP3 FILL ITEMS", "AP3 FILL AGE",
                      "AP3 FILL HOUSEHOLD SIZE", "AP3 FILL INCOME BAND")
ap3_card_stub <- unlist(lapply(ap3_card_markers, function(marker) {
  c(paste0("# BEGIN GENERATED PARAMETER CARD: ", marker), "# stale",
    paste0("# END GENERATED PARAMETER CARD: ", marker))
}))

# R/ap4_factor_structure.R carries one card above each of the five definers.
ap4_card_markers <- c("AP4 CFA SUBSCALE", "AP4 CFA UMS DOPL", "AP4 CFA SOCIAL MOTIVES",
                      "AP4 CFA ASC", "AP4 CFA AUTH ORIENTATION", "AP4 CFA ESTIMATION")
ap4_card_stub <- unlist(lapply(ap4_card_markers, function(marker) {
  c(paste0("# BEGIN GENERATED PARAMETER CARD: ", marker), "# stale",
    paste0("# END GENERATED PARAMETER CARD: ", marker))
}))

# R/ap9_efa.R carries the seed and item-set cards above the item-set definer,
# and one card each above the factor-number, theoretical-count, fitting and
# loading-clarity verbs.
efa_card_markers <- c("AP9 EFA SEED", "AP9 EFA ITEM SETS", "AP9 FACTOR CRITERIA",
                      "AP9 EFA EXPECTED COUNTS", "AP9 EFA ESTIMATION", "AP9 LOADING CLARITY")
efa_card_stub <- unlist(lapply(efa_card_markers, function(marker) {
  c(paste0("# BEGIN GENERATED PARAMETER CARD: ", marker), "# stale",
    paste0("# END GENERATED PARAMETER CARD: ", marker))
}))

# R/ap6_regressions.R also carries the interval-level, prior-predictive and
# joint-prior cards; R/ap7_joint_comparisons.R carries one card above the
# ap8_result_* family.
ap6_interval_card_markers <- c("AP6 INTERVAL LEVEL PRIOR WIDTH", "AP6 PRIOR PREDICTIVE SUMMARY",
                               "AP6 INTERVAL LEVEL POSTERIOR CHECK",
                               "AP6 INTERVAL LEVEL COEFFICIENTS", "AP6 INTERVAL LEVEL R2",
                               "AP7 JOINT RESIDUAL PRIOR", "AP6 PRIOR SENSITIVITY")
ap6_card_stub <- c(ap6_card_stub, unlist(lapply(ap6_interval_card_markers, function(marker) {
  c(paste0("# BEGIN GENERATED PARAMETER CARD: ", marker), "# stale",
    paste0("# END GENERATED PARAMETER CARD: ", marker))
})))

ap8_card_stub <- c(
  "# BEGIN GENERATED PARAMETER CARD: AP7 INTERVAL LEVEL", "# stale",
  "# END GENERATED PARAMETER CARD: AP7 INTERVAL LEVEL")

# R/ap8_network.R carries one card above each of the two definition verbs.
network_card_stub <- c(
  "# BEGIN GENERATED PARAMETER CARD: AP8 NETWORK MODEL", "# stale",
  "# END GENERATED PARAMETER CARD: AP8 NETWORK MODEL",
  "# BEGIN GENERATED PARAMETER CARD: AP8 NETWORK BAGGING", "# stale",
  "# END GENERATED PARAMETER CARD: AP8 NETWORK BAGGING")

# R/ap4_reliability.R carries the seed and interval cards above the bootstrap
# verb and one card above the fallback rule.
reliability_card_stub <- unlist(lapply(c("AP4 RELIABILITY BOOTSTRAP SEED", "AP4 RELIABILITY INTERVALS",
                                         "AP4 RELIABILITY FALLBACK"), function(marker) {
  c(paste0("# BEGIN GENERATED PARAMETER CARD: ", marker), "# stale",
    paste0("# END GENERATED PARAMETER CARD: ", marker))
}))

# The further card files: AP3 preparation, CFA estimation and reporting, the
# descriptives, the prior sensitivity and the Table 3 prediction signs.
further_card_markers <- list(
  "ap3_preprocessing.R" = "AP3 REVERSAL",
  "ap3_preparation.R" = c("AP3 EAST WEST", "AP3 COVARIATES", "AP3 STANDARDISATION",
                          "AP3 NETWORK INPUT", "AP3 REGRESSION INPUT"),
  "ap3_data_files.R" = "AP3 SCIENTIFIC USE FILE",
  "ap4_cfa_reporting.R" = "AP4 CFA RESIDUALS",
  "ap5_descriptives.R" = c("AP5 SAMPLE COMPOSITION", "AP5 SCALE CORRELATIONS"),
  "ap10_inference.R" = "RQ1 PREDICTION SIGNS"
)
pc_write_further_card_stubs <- function(root) {
  for (file in names(further_card_markers)) {
    writeLines(unlist(lapply(further_card_markers[[file]], function(marker) {
      c(paste0("# BEGIN GENERATED PARAMETER CARD: ", marker), "# stale",
        paste0("# END GENERATED PARAMETER CARD: ", marker))
    })), file.path(root, "R", file))
  }
}

test_that("the checked-in AP1 parameter card matches the canonical YAML", {
  expect_false(pc_update_parameter_cards(parameter_card_root, check = TRUE))
})

test_that("a changed YAML value makes a copied AP1 card stale until it is updated", {
  root <- pc_temp_root()
  dir.create(file.path(root, "config"))
  dir.create(file.path(root, "R"))
  file.copy(
    file.path(parameter_card_root, "config", "analysis_plan.yaml"),
    file.path(root, "config", "analysis_plan.yaml")
  )
  writeLines(
    c(
      "before",
      "# BEGIN GENERATED PARAMETER CARD: AP1",
      "# stale",
      "# END GENERATED PARAMETER CARD: AP1",
      "after"
    ),
    file.path(root, "R", "ap1_exclusions.R")
  )
  writeLines(ap6_card_stub, file.path(root, "R", "ap6_regressions.R"))
  writeLines(ap3_card_stub, file.path(root, "R", "ap3_fill.R"))
  writeLines(ap4_card_stub, file.path(root, "R", "ap4_factor_structure.R"))
  writeLines(efa_card_stub, file.path(root, "R", "ap9_efa.R"))
  writeLines(reliability_card_stub, file.path(root, "R", "ap4_reliability.R"))
  writeLines(network_card_stub, file.path(root, "R", "ap8_network.R"))
  writeLines(ap8_card_stub, file.path(root, "R", "ap7_joint_comparisons.R"))
  pc_write_further_card_stubs(root)

  expect_true(pc_update_parameter_cards(root))
  expect_false(pc_update_parameter_cards(root, check = TRUE))
  first <- readLines(file.path(root, "R", "ap1_exclusions.R"), warn = FALSE)
  expect_true(any(grepl("#   Minimum age: 18 years", first, fixed = TRUE)))
  expect_true(any(grepl("#   Maximum age: 69 years", first, fixed = TRUE)))
  # The card carries no hash of the plan: staleness is a difference in the
  # printed values, as it is for the other cards.
  expect_false(any(grepl("SHA-256", first, fixed = TRUE)))
  expect_false(any(grepl("Source section", first, fixed = TRUE)))

  yaml_path <- file.path(root, "config", "analysis_plan.yaml")
  lines <- readLines(yaml_path, warn = FALSE)
  writeLines(sub("^  min_age: 18", "  min_age: 19", lines), yaml_path)
  expect_error(pc_update_parameter_cards(root, check = TRUE), "card is stale")
  expect_true(pc_update_parameter_cards(root))
  second <- readLines(file.path(root, "R", "ap1_exclusions.R"), warn = FALSE)
  expect_true(any(grepl("#   Minimum age: 19 years", second, fixed = TRUE)))
  expect_false(identical(first, second))
  expect_false(pc_update_parameter_cards(root, check = TRUE))
  lines <- readLines(yaml_path, warn = FALSE)
  writeLines(sub("^  max_age: 69", "  max_age: 68", lines), yaml_path)
  expect_error(pc_update_parameter_cards(root, check = TRUE), "card is stale")
  expect_true(pc_update_parameter_cards(root))
  third <- readLines(file.path(root, "R", "ap1_exclusions.R"), warn = FALSE)
  expect_true(any(grepl("#   Maximum age: 68 years", third, fixed = TRUE)))
  expect_false(pc_update_parameter_cards(root, check = TRUE))
  # a comment-only change to the plan leaves every printed value, so the card
  # is current: nothing in it depends on the file's bytes
  write("# comment-only witness", yaml_path, append = TRUE)
  expect_false(pc_update_parameter_cards(root, check = TRUE))
})

test_that("the AP6 cards state the configured regression, its family and its priors", {
  card <- pc_ap6_formula_card(file.path(parameter_card_root, "config", "analysis_plan.yaml"))
  # every outcome by its standardised column, one regression each, no labels
  # (keys only; the codebook maps a key to its construct)
  expect_true(any(grepl("asc_agg_z | asc_sub_z | asc_conv_z | sdo_dom_z", card, fixed = TRUE)))
  for (label in c("Authoritarian aggression", "Authoritarian submission", "Conventionalism",
                  "SDO dominance")) {
    expect_false(any(grepl(label, card, fixed = TRUE)), info = label)
  }
  # the shared right-hand side as zm_config() builds it: the motives in the
  # order of codebook_scales.csv, then the covariates in the plan's order
  expect_true(any(grepl("zm_security_z + zm_arousal_z + zm_power_z + zm_prestige_z + zm_achievement_z + age_z + gender + income_z",
                        paste(sub("^#[[:space:]]*", "", card), collapse = " "), fixed = TRUE)))
  expect_false(any(grepl("Intimacy", card, fixed = TRUE)))
  expect_true(any(grepl("# One regression per outcome:", card, fixed = TRUE)))
  expect_true(any(grepl("# The same predictors in every regression:", card, fixed = TRUE)))
  expect_false(any(grepl("Family", card, fixed = TRUE)))
  expect_false(any(grepl("NA", card, fixed = TRUE)))

  # the family card states the residual distribution of the primary fits, and
  # only that: the Student-t refit is a different verb on a different route
  family <- pc_ap6_family_card(file.path(parameter_card_root, "config", "analysis_plan.yaml"))
  expect_identical(family, c(
    "# BEGIN GENERATED PARAMETER CARD: AP6 FAMILY",
    "# Automatically generated from analysis_plan.yaml",
    "# Residuals: gaussian.",
    "# END GENERATED PARAMETER CARD: AP6 FAMILY"
  ))

  priors <- pc_ap6_priors_card(file.path(parameter_card_root, "config", "analysis_plan.yaml"))
  text <- paste(priors, collapse = "\n")
  # the sweep with its labels, the fixed prior SDs, the Student-t constants
  # (the residual family is on its own card, above its own verb)
  expect_false(any(grepl("# Residuals: gaussian.", priors, fixed = TRUE)))
  expect_true(any(grepl("# Slopes of the metric predictors: normal(0, 0.2).", priors, fixed = TRUE)))
  expect_true(grepl("Sweep of the slope width: 0.1 (skeptical) | 0.4 (permissive).", text, fixed = TRUE))
  expect_false(grepl("0.2 (primary)", text, fixed = TRUE))
  expect_lt(grep("# Residual SD:", priors, fixed = TRUE), grep("Sweep of the slope width", priors, fixed = TRUE))
  expect_true(any(grepl("# Gender contrasts: normal(0, 0.4).", priors, fixed = TRUE)))
  expect_true(any(grepl("# Centred intercept: normal(0, 0.2).", priors, fixed = TRUE)))
  expect_true(any(grepl("# Residual SD: half-normal(0, 1).", priors, fixed = TRUE)))
  expect_true(any(grepl("# Robustness refit with Student-t residuals: nu fixed at 4; scale half-normal(0, 0.7071).",
                        priors, fixed = TRUE)))
  # the derivation of that scale is not a plan value and is not on the card
  expect_false(grepl("implied residual SD prior", text, fixed = TRUE))
  # nothing technical: no sentence about Stan data or compiled programs
  expect_false(grepl("Stan", text, fixed = TRUE))
  expect_false(any(grepl("NA", priors, fixed = TRUE)))
})

test_that("AP6 cards regenerate semantic YAML values without touching executable text", {
  root <- pc_temp_root()
  dir.create(file.path(root, "config")); dir.create(file.path(root, "R"))
  file.copy(file.path(parameter_card_root, "config", "analysis_plan.yaml"), file.path(root, "config", "analysis_plan.yaml"))
  writeLines(c("# BEGIN GENERATED PARAMETER CARD: AP1", "# stale", "# END GENERATED PARAMETER CARD: AP1"), file.path(root, "R", "ap1_exclusions.R"))
  writeLines(c("before", ap6_card_stub, "formula <- ap6_formula(outcome, analysis_plan)"), file.path(root, "R", "ap6_regressions.R"))
  writeLines(ap3_card_stub, file.path(root, "R", "ap3_fill.R"))
  writeLines(ap4_card_stub, file.path(root, "R", "ap4_factor_structure.R"))
  writeLines(efa_card_stub, file.path(root, "R", "ap9_efa.R"))
  writeLines(reliability_card_stub, file.path(root, "R", "ap4_reliability.R"))
  writeLines(network_card_stub, file.path(root, "R", "ap8_network.R"))
  writeLines(ap8_card_stub, file.path(root, "R", "ap7_joint_comparisons.R"))
  pc_write_further_card_stubs(root)
  expect_true(pc_update_parameter_cards(root))
  ap6 <- file.path(root, "R", "ap6_regressions.R")
  first <- readLines(ap6, warn = FALSE)
  expect_true(any(grepl("sdo_dom_z", first, fixed = TRUE)))
  expect_true(any(grepl("# Slopes of the metric predictors: normal(0, 0.2).", first, fixed = TRUE)))
  expect_true(any(grepl("formula <- ap6_formula(outcome, analysis_plan)", first, fixed = TRUE)))
  first_time <- file.info(ap6)$mtime
  Sys.sleep(1.1); expect_false(pc_update_parameter_cards(root)); expect_identical(file.info(ap6)$mtime, first_time)
  yaml_path <- file.path(root, "config", "analysis_plan.yaml")
  lines <- readLines(yaml_path, warn = FALSE)
  # the scale is renamed everywhere the plan names it (the cards check every
  # scale they list against the codebook)
  lines <- gsub("sdo_dom", "sdo_dom_alt", lines, fixed = TRUE)
  # the outcome's standardised column comes from the codebook, so the renamed
  # scale is renamed there too
  book_path <- file.path(dirname(root), "preregistration", "codebook_scales.csv")
  book <- readLines(book_path, warn = FALSE)
  book <- sub("^sdo_dom,sdo_dom_z,", "sdo_dom_alt,sdo_dom_alt_z,", book)
  writeLines(book, book_path)
  lines <- sub("slope_sd_sweep: [0.10, 0.20, 0.40]", "slope_sd_sweep: [0.10, 0.30, 0.40]", lines, fixed = TRUE)
  lines <- sub("\"0.2\": \"primary\"", "\"0.3\": \"changed primary\"", lines, fixed = TRUE)
  lines <- sub("gender_sd: 0.40", "gender_sd: 0.5", lines, fixed = TRUE)
  writeLines(lines, yaml_path)
  expect_error(pc_update_parameter_cards(root, check = TRUE), "card is stale")
  expect_true(pc_update_parameter_cards(root))
  second <- readLines(ap6, warn = FALSE)
  # the outcome line follows the renamed standardised column of the codebook
  expect_true(any(grepl("sdo_dom_alt_z", second, fixed = TRUE)))
  expect_true(any(grepl("0.3 (changed primary)", second, fixed = TRUE)))
  expect_true(any(grepl("Gender contrasts: normal(0, 0.5)", second, fixed = TRUE)))
  expect_true(any(grepl("formula <- ap6_formula(outcome, analysis_plan)", second, fixed = TRUE)))
})

test_that("AP6 terms render short configured formulas without placeholder rows", {
  root <- pc_temp_root(); dir.create(file.path(root, "config")); dir.create(file.path(root, "R"))
  file.copy(file.path(parameter_card_root, "config", "analysis_plan.yaml"), file.path(root, "config", "analysis_plan.yaml"))
  lines <- readLines(file.path(root, "config", "analysis_plan.yaml"), warn = FALSE)
  lines <- sub("^  motives: \\[.*$", "  motives: [zm_power]", lines)
  lines <- sub("^  covariates: \\[age, gender, income\\]$", "  covariates: [age, gender]", lines)
  writeLines(lines, file.path(root, "config", "analysis_plan.yaml"))
  card <- pc_ap6_formula_card(file.path(root, "config", "analysis_plan.yaml"))
  expect_true(any(grepl("# The same predictors in every regression:", card, fixed = TRUE)))
  expect_true("#   zm_power_z + age_z + gender" %in% card)
  expect_false(any(grepl("NA", card, fixed = TRUE)))
})

test_that("every card that lists scales prints them in the order of codebook_scales.csv", {
  # The plan writes its lists in some order; the cards print the codebook's.
  # Here the plan lists are reversed: the cards do not change.
  plan <- file.path(parameter_card_root, "config", "analysis_plan.yaml")
  root <- pc_temp_root(); dir.create(file.path(root, "config"))
  reversed <- yaml::read_yaml(plan)
  reversed$regression$outcomes <- rev(reversed$regression$outcomes)
  reversed$regression$motives <- rev(reversed$regression$motives)
  reversed$network$nodes <- rev(reversed$network$nodes)
  reversed$factor_analysis$sets <- lapply(reversed$factor_analysis$sets, function(set) {
    set$scales <- rev(set$scales)
    set
  })
  reversed$confirmatory_models$models <- lapply(rev(reversed$confirmatory_models$models), function(model) {
    model$factors <- rev(model$factors)
    model
  })
  reversed$predictions$table <- lapply(rev(reversed$predictions$table), rev)
  reversed_path <- file.path(root, "config", "analysis_plan.yaml")
  yaml::write_yaml(reversed, reversed_path)
  for (card in c("pc_ap6_formula_card", "pc_ap4_efa_item_sets_card", "pc_ap7_network_model_card",
                 "pc_ap3_standardisation_card", "pc_ap3_network_input_card", "pc_ap3_regression_input_card",
                 "pc_ap7_prediction_signs_card")) {
    expect_identical(get(card)(reversed_path), get(card)(plan), info = card)
  }
  for (family in c("subscale", "ums_dopl", "social_motives", "asc", "auth_orientation")) {
    expect_identical(pc_ap4_cfa_card(reversed_path, family), pc_ap4_cfa_card(plan, family), info = family)
  }
  # and that order is the codebook's `order` column
  order_keys <- pc_order_scale_keys(c("sdo_dom", "zm_achievement", "zm_security", "asc_agg", "zm_power", "zm_arousal",
                                      "asc_conv", "zm_prestige", "asc_sub"), plan, yaml::read_yaml(plan))
  expect_identical(order_keys, c("zm_security", "zm_arousal", "zm_power", "zm_prestige", "zm_achievement",
                                 "asc_agg", "asc_sub", "asc_conv", "sdo_dom"))
})

test_that("partial participant builds refresh the card for YAML values and comments", {
  root <- pc_temp_root()
  dir.create(file.path(root, "config"))
  dir.create(file.path(root, "R"))
  file.copy(file.path(parameter_card_root, "config", "analysis_plan.yaml"),
            file.path(root, "config", "analysis_plan.yaml"))
  file.copy(file.path(parameter_card_root, "R", "parameter_cards.R"),
            file.path(root, "R", "parameter_cards.R"))
  writeLines(c(
    "# BEGIN GENERATED PARAMETER CARD: AP1", "# stale",
    "# END GENERATED PARAMETER CARD: AP1"
  ), file.path(root, "R", "ap1_exclusions.R"))
  writeLines(ap6_card_stub, file.path(root, "R", "ap6_regressions.R"))
  writeLines(ap3_card_stub, file.path(root, "R", "ap3_fill.R"))
  writeLines(ap4_card_stub, file.path(root, "R", "ap4_factor_structure.R"))
  writeLines(efa_card_stub, file.path(root, "R", "ap9_efa.R"))
  writeLines(reliability_card_stub, file.path(root, "R", "ap4_reliability.R"))
  writeLines(network_card_stub, file.path(root, "R", "ap8_network.R"))
  writeLines(ap8_card_stub, file.path(root, "R", "ap7_joint_comparisons.R"))
  pc_write_further_card_stubs(root)

  script <- file.path(root, "_targets.R")
  store <- file.path(root, "_targets")
  root_literal <- paste(capture.output(dput(root)), collapse = "")
  production <- parse(file.path(parameter_card_root, "_targets.R"))
  calls <- as.list(production[[length(production)]])[-1L]
  names <- vapply(calls, function(x) {
    if (is.call(x) && identical(x[[1]], as.name("tar_target"))) as.character(x[[2]]) else NA_character_
  }, "")
  target_text <- function(name) paste(deparse(calls[[match(name, names)]]), collapse = "\n")
  writeLines(c(
    "library(targets)",
    paste0("root <- ", root_literal),
    "source(file.path(root, 'R', 'parameter_cards.R'))",
    "analysis_plan <- list(root = root)",
    "list(",
    paste0(target_text("analysis_plan_file"), ","),
    paste0(target_text("parameter_card_file"), ","),
    "  tar_target(participant, { parameter_card_file; lapply(parameter_card_file, readLines, warn = FALSE) })",
    ")"
  ), script)
  make_participant <- function() targets::tar_make(
    names = "participant", script = script, store = store,
    callr_function = NULL, reporter = "silent"
  )
  read_participant <- function() targets::tar_read_raw(
    "participant", store = store
  )

  make_participant()
  card <- file.path(root, "R", "ap1_exclusions.R")
  first <- read_participant()
  expect_true(any(grepl("#   Minimum age: 18 years", first[[1]], fixed = TRUE)))
  first_time <- file.info(card)$mtime
  Sys.sleep(1.1)
  make_participant()
  expect_identical(file.info(card)$mtime, first_time)

  yaml_path <- file.path(root, "config", "analysis_plan.yaml")
  yaml_lines <- readLines(yaml_path, warn = FALSE)
  yaml_lines <- sub("^  min_age: 18", "  min_age: 19", yaml_lines)
  writeLines(yaml_lines, yaml_path, useBytes = TRUE)
  make_participant()
  second <- read_participant()
  expect_true(any(grepl("#   Minimum age: 19 years", second[[1]], fixed = TRUE)))
  expect_false(identical(first, second))
  yaml_lines <- sub("gender_sd: 0.40", "gender_sd: 0.5", yaml_lines, fixed = TRUE)
  writeLines(yaml_lines, yaml_path, useBytes = TRUE)
  make_participant()
  ap6_changed <- read_participant()
  expect_true(any(grepl("Gender contrasts: normal(0, 0.5)", unlist(ap6_changed), fixed = TRUE)))
  expect_true(any(grepl("#   Minimum age: 19 years", ap6_changed[[1]], fixed = TRUE)))

  # A comment-only change to the plan re-runs the card target through the file
  # dependency, and leaves every printed value: the cards state values, not the
  # bytes of the file they are generated from.
  write("# comment-only dependency witness", yaml_path, append = TRUE)
  make_participant()
  third <- read_participant()
  expect_identical(ap6_changed, third)
})

test_that("the AP6 convergence cards state each check's plan values", {
  plan <- file.path(parameter_card_root, "config", "analysis_plan.yaml")
  initial <- pc_ap6_initial_convergence_card(plan)
  retry <- pc_ap6_retry_card(plan)
  final <- pc_ap6_final_convergence_card(plan)
  ess_lines <- c(
    "# Effective sample size required of every coefficient: 10000 bulk and 10000 tail.",
    "# Profile smoke instead: 2000 bulk and 2000 tail.",
    "# Profile presentation instead: 200 bulk and 200 tail."
  )
  diagnostic_lines <- c(
    "# R-hat at most 1.01.",
    "# Divergent transitions at most 0.",
    "# Transitions at the maximum tree depth at most 0.",
    "# BFMI of every chain at least 0.2.",
    "# Monte Carlo SE reported for medians and the 95% interval endpoints."
  )
  expect_true(all(ess_lines %in% initial))
  expect_true(all(ess_lines %in% retry))
  expect_true(all(ess_lines %in% final))
  expect_false(any(diagnostic_lines %in% initial))
  expect_true(all(diagnostic_lines %in% retry))
  expect_true(all(diagnostic_lines %in% final))
  expect_true("# ESS retries allowed after this check: at most 3 doublings." %in% initial)
  expect_false(any(grepl("adapt_delta", initial, fixed = TRUE)))
  expect_false(any(grepl("adapt_delta", final, fixed = TRUE)))
})

test_that("the AP6 RETRY card states what a fit that falls short is refitted with", {
  card <- pc_ap6_retry_card(file.path(parameter_card_root, "config", "analysis_plan.yaml"))
  expect_identical(card[1:2], c("# BEGIN GENERATED PARAMETER CARD: AP6 RETRY",
                                "# Automatically generated from analysis_plan.yaml"))
  expect_true("# On a shortfall: posterior draws per chain doubled, at most 3 times." %in% card)
  expect_true(paste0("# On R-hat, divergences or tree-depth hits: one refit with doubled warmup, ",
                     "adapt_delta 0.99, max_treedepth 15.") %in% card)
  expect_identical(tail(card, 1L), "# END GENERATED PARAMETER CARD: AP6 RETRY")
})

test_that("each AP6 card sits above the body whose values it states", {
  lines <- readLines(file.path(parameter_card_root, "R", "ap6_regressions.R"), warn = FALSE)
  bodies <- c(
    "AP6 FORMULA" = "define_regression_model_set <- function",
    "AP6 FAMILY" = "define_regression_model_set <- function",
    "AP6 PRIORS" = "define_regression_model_set <- function",
    "AP6 SAMPLING" = "fit_primary_regressions <- function",
    "AP6 INITIAL CONVERGENCE" = "apply_validity_gate <- function",
    "AP6 RETRY" = "ap6_refit_if_needed <- function",
    "AP6 FINAL CONVERGENCE" = "ap6_check_final_convergence <- function"
  )
  # The lines of every generated card, so that cards stacked above one body
  # count as part of its header.
  card_lines <- integer(0)
  for (begin in grep("^# BEGIN GENERATED PARAMETER CARD: ", lines)) {
    card_end <- grep("^# END GENERATED PARAMETER CARD: ", lines)
    card_lines <- c(card_lines, seq.int(begin, min(card_end[card_end > begin])))
  }
  for (marker in names(bodies)) {
    end <- grep(paste0("^# END GENERATED PARAMETER CARD: ", marker, "$"), lines)
    body <- grep(paste0("^", bodies[[marker]]), lines)
    expect_equal(length(end), 1L, info = marker)
    expect_equal(length(body), 1L, info = marker)
    expect_true(body > end, info = marker)
    between <- if (body == end + 1L) integer() else seq.int(end + 1L, body - 1L)
    between <- setdiff(between, card_lines)
    expect_true(all(grepl("^#'", lines[between])), info = marker)
  }
})

test_that("the AP6 SAMPLING card states the settings that determine the draws", {
  card <- pc_ap6_sampling_card(file.path(parameter_card_root, "config", "analysis_plan.yaml"))
  text <- paste(card, collapse = "\n")
  expect_identical(card[1:2], c("# BEGIN GENERATED PARAMETER CARD: AP6 SAMPLING",
                                "# Automatically generated from analysis_plan.yaml"))
  expect_true(any(grepl("# Chains: 4.", card, fixed = TRUE)))
  expect_true(any(grepl("# Warmup draws per chain: 2000.", card, fixed = TRUE)))
  expect_true(any(grepl("# Posterior draws per chain: 6000.", card, fixed = TRUE)))
  expect_true(any(grepl("# Seed: 20260905.", card, fixed = TRUE)))
  expect_true(any(grepl("# Backend: cmdstanr.", card, fixed = TRUE)))
  expect_true(any(grepl("# Sampler control: the sampler's defaults.", card, fixed = TRUE)))
  # This verb fits once. The retry and its control are the next card down.
  expect_false(grepl("adapt_delta", text, fixed = TRUE))
  expect_false(grepl("max_treedepth", text, fixed = TRUE))
  # a profile that changes the draws is named, so a smoke run is not read as
  # the preregistered one
  expect_true(grepl("Profile smoke instead: warmup draws per chain 500, posterior draws per chain 1000.",
                    text, fixed = TRUE))
  expect_false(grepl("Profile full", text, fixed = TRUE))
  # nothing technical: no directories, no worker settings
  expect_false(grepl("cores", text, fixed = TRUE))
  expect_false(any(grepl("NA", card, fixed = TRUE)))
})

test_that("a changed sampler setting makes the AP6 SAMPLING card stale", {
  root <- pc_temp_root()
  dir.create(file.path(root, "config")); dir.create(file.path(root, "R"))
  yaml_path <- file.path(root, "config", "analysis_plan.yaml")
  file.copy(file.path(parameter_card_root, "config", "analysis_plan.yaml"), yaml_path)
  writeLines(c("# BEGIN GENERATED PARAMETER CARD: AP1", "# stale",
               "# END GENERATED PARAMETER CARD: AP1"), file.path(root, "R", "ap1_exclusions.R"))
  writeLines(ap6_card_stub, file.path(root, "R", "ap6_regressions.R"))
  writeLines(ap3_card_stub, file.path(root, "R", "ap3_fill.R"))
  writeLines(ap4_card_stub, file.path(root, "R", "ap4_factor_structure.R"))
  writeLines(efa_card_stub, file.path(root, "R", "ap9_efa.R"))
  writeLines(reliability_card_stub, file.path(root, "R", "ap4_reliability.R"))
  writeLines(network_card_stub, file.path(root, "R", "ap8_network.R"))
  writeLines(ap8_card_stub, file.path(root, "R", "ap7_joint_comparisons.R"))
  pc_write_further_card_stubs(root)
  expect_true(pc_update_parameter_cards(root))
  expect_false(pc_update_parameter_cards(root, check = TRUE))

  lines <- readLines(yaml_path, warn = FALSE)
  lines <- sub("^  seed: 20260905", "  seed: 20260906", lines)
  lines <- sub("^    regression_warmup: 2000", "    regression_warmup: 2500", lines)
  lines <- sub("^    regression_ess_target: 2000", "    regression_ess_target: 2100", lines)
  lines <- sub("^    rhat_max: 1.01", "    rhat_max: 1.02", lines)
  lines <- sub("^  ci_level: 0.95", "  ci_level: 0.9", lines)
  writeLines(lines, yaml_path)
  expect_error(pc_update_parameter_cards(root, check = TRUE), "card is stale")
  expect_true(pc_update_parameter_cards(root))
  card <- readLines(file.path(root, "R", "ap6_regressions.R"), warn = FALSE)
  expect_true(any(grepl("# Seed: 20260906.", card, fixed = TRUE)))
  expect_true(any(grepl("# Warmup draws per chain: 2500.", card, fixed = TRUE)))
  expect_true(any(grepl("# Profile smoke instead: 2100 bulk and 2100 tail.", card, fixed = TRUE)))
  expect_true(any(grepl("# R-hat at most 1.02.", card, fixed = TRUE)))
  expect_true(any(grepl("# Monte Carlo SE reported for medians and the 90% interval endpoints.",
                        card, fixed = TRUE)))
})

test_that("the route card of the fill states the drop rule, the order and gender", {
  card <- pc_ap3_fill_route_card(file.path(parameter_card_root, "config", "analysis_plan.yaml"))
  text <- paste(card, collapse = "\n")
  # a card line may wrap, so the statements are read from the unwrapped text
  flat <- paste(sub("^#[[:space:]]*", "", card), collapse = " ")
  expect_identical(card[1:2], c("# BEGIN GENERATED PARAMETER CARD: AP3 FILL ROUTE",
                                "# Automatically generated from analysis_plan.yaml"))
  expect_false(any(grepl("Source section", card, fixed = TRUE)))
  # the rule of the route and the order of the five verbs
  expect_true(grepl("one fill before the scale scores; one value per empty cell", flat, fixed = TRUE))
  expect_false(grepl("stage:", text, fixed = TRUE))
  expect_true(grepl("Order: the drop, then the items of every scale with a gap, then demo_age, demo_hh_members, demo_income_hh_net.",
                    flat, fixed = TRUE))
  # the drop thresholds as configured
  expect_true(grepl("more than 2 missing items in one scale, or at least 2 of the three demographics missing.",
                    flat, fixed = TRUE))
  # the gender rule
  expect_true(grepl("Gender: never filled; rows without gender are omitted from the four regressions and the joint model only.",
                    flat, fixed = TRUE))
  expect_false(grepl("Shared scientific-use file", text, fixed = TRUE))
  # nothing that belongs to the card of one fit
  expect_false(grepl("~", text, fixed = TRUE))
  expect_false(grepl("Family", text, fixed = TRUE))
  expect_false(grepl("Priors", text, fixed = TRUE))
  expect_false(any(grepl("NA", card, fixed = TRUE)))
  line_of <- function(pattern) grep(pattern, card, fixed = TRUE)[1]
  expect_equal(order(vapply(c("# Rule:", "# Dropped from all analyses instead of filled:",
                              "# Order:", "# Gender:"), line_of, numeric(1))), 1:4)
})

test_that("each fill model card states its formula, family, priors, value and interval", {
  plan <- file.path(parameter_card_root, "config", "analysis_plan.yaml")
  cards <- list(
    items = pc_ap3_fill_model_card(plan, "items", "AP3 FILL ITEMS"),
    demo_age = pc_ap3_fill_model_card(plan, "demo_age", "AP3 FILL AGE"),
    demo_hh_members = pc_ap3_fill_model_card(plan, "demo_hh_members", "AP3 FILL HOUSEHOLD SIZE"),
    demo_income_hh_net = pc_ap3_fill_model_card(plan, "demo_income_hh_net", "AP3 FILL INCOME BAND")
  )
  markers <- c(items = "AP3 FILL ITEMS", demo_age = "AP3 FILL AGE",
               demo_hh_members = "AP3 FILL HOUSEHOLD SIZE",
               demo_income_hh_net = "AP3 FILL INCOME BAND")
  for (model in names(cards)) {
    card <- cards[[model]]
    expect_identical(card[1:2], c(paste0("# BEGIN GENERATED PARAMETER CARD: ", markers[[model]]),
                                  "# Automatically generated from analysis_plan.yaml"),
                     info = model)
    expect_identical(card[length(card)],
                     paste0("# END GENERATED PARAMETER CARD: ", markers[[model]]), info = model)
    # the lines pair with the lines of the body: formula, family, priors, value, interval
    line_of <- function(pattern) grep(pattern, card, fixed = TRUE)[1]
    expect_equal(order(vapply(c("# Formula:", "# Family:", "# Priors:", "# Value per cell:",
                                "# Interval per filled cell:"), line_of, numeric(1))), 1:5,
                 info = model)
    expect_true(any(grepl("# Interval per filled cell: 95% posterior predictive.", card, fixed = TRUE)),
                info = model)
    expect_false(grepl("report only", paste(card, collapse = "\n"), fixed = TRUE), info = model)
    expect_false(any(grepl("Source section", card, fixed = TRUE)), info = model)
    expect_false(any(grepl("NA", card, fixed = TRUE)), info = model)
    # no route rule on a model card
    expect_false(any(grepl("Dropped from all analyses", card, fixed = TRUE)), info = model)
    expect_false(any(grepl("Gender:", card, fixed = TRUE)), info = model)
  }

  items <- paste(cards$items, collapse = "\n")
  expect_true(any(grepl("# One fit per scale with at least one gap.", cards$items, fixed = TRUE)))
  expect_true(any(grepl("#   answer ~ 1 + (1 | item) + (1 | respondent_id)", cards$items, fixed = TRUE)))
  expect_true(grepl("# Family: cumulative(logit).", items, fixed = TRUE))
  expect_true(grepl("# Priors: intercept normal(0, 1.5); sd normal(0, 1).", items, fixed = TRUE))
  expect_true(grepl("# Value per cell: posterior median of the expected answer (fractional allowed).",
                    items, fixed = TRUE))

  age <- paste(cards$demo_age, collapse = "\n")
  expect_true(grepl("#   demo_age ~ zm_security + zm_arousal + zm_power + zm_prestige + zm_achievement + asc_agg +\n#     asc_sub + asc_conv + sdo_dom + gender (only when observed for every retained participant)",
                    age, fixed = TRUE))
  expect_true(grepl("# Family: gaussian.", age, fixed = TRUE))
  expect_true(grepl("# Priors: intercept normal(45, 20); b normal(0, 5); sigma normal(0, 15).",
                    age, fixed = TRUE))
  expect_true(grepl("# Value per cell: posterior median of the expected value.", age, fixed = TRUE))

  household <- paste(cards$demo_hh_members, collapse = "\n")
  expect_true(grepl("#   demo_hh_members ~ zm_security", household, fixed = TRUE))
  expect_true(grepl("gender (only when observed for every retained participant) +\n#     demo_age\n",
                    household, fixed = TRUE))
  expect_true(grepl("# Family: poisson.", household, fixed = TRUE))
  expect_true(grepl("# Priors: intercept normal(0, 1.5); b normal(0, 0.5).", household, fixed = TRUE))
  expect_true(grepl("# Value per cell: posterior median of the expected count, rounded, kept within 1..8.",
                    household, fixed = TRUE))

  income <- paste(cards$demo_income_hh_net, collapse = "\n")
  expect_true(grepl("#   demo_income_hh_net ~ zm_security", income, fixed = TRUE))
  # the formula wraps across card lines, so compare it without the comment
  # prefixes and the line breaks
  expect_true(grepl(paste("gender (only when observed for every retained participant) +",
                          "demo_age + demo_hh_members"),
                    gsub("[[:space:]]*#[[:space:]]*", " ", income), fixed = TRUE))
  expect_true(grepl("# Family: cumulative(logit).", income, fixed = TRUE))
  expect_true(grepl("# Value per cell: median band of the posterior predictive draws.", income, fixed = TRUE))
})

test_that("the demographic fill cards state the preregistered gender rule", {
  plan <- file.path(parameter_card_root, "config", "analysis_plan.yaml")
  # select_demographic_imputation_predictors() uses gender only when it is
  # observed for every retained participant; all three cards say so, and a plan
  # formula that drops the condition stops the generator instead of printing an
  # unconditional predictor.
  for (model in c("demo_age", "demo_hh_members", "demo_income_hh_net")) {
    card <- paste(pc_ap3_fill_model_card(plan, model, "AP3 FILL AGE"), collapse = " ")
    squeezed <- gsub("[[:space:]]*#[[:space:]]*", " ", card)
    expect_true(grepl("gender (only when observed for every retained participant)",
                      squeezed, fixed = TRUE), info = model)
  }
  root <- pc_temp_root()
  dir.create(file.path(root, "config"))
  lines <- readLines(plan, warn = FALSE)
  writeLines(gsub(" (only when observed for every retained participant)", "", lines, fixed = TRUE),
             file.path(root, "config", "analysis_plan.yaml"))
  expect_error(
    pc_ap3_fill_model_card(file.path(root, "config", "analysis_plan.yaml"),
                           "demo_age", "AP3 FILL AGE"),
    "names gender without", fixed = TRUE)
})

test_that("each fill card sits directly above the verb it states", {
  lines <- readLines(file.path(parameter_card_root, "R", "ap3_fill.R"), warn = FALSE)
  verbs <- c("AP3 FILL ROUTE" = "exclude_participants_with_high_missingness <- function",
             "AP3 FILL ITEMS" = "impute_items <- function",
             "AP3 FILL AGE" = "impute_age <- function",
             "AP3 FILL HOUSEHOLD SIZE" = "impute_household_size <- function",
             "AP3 FILL INCOME BAND" = "impute_income_band <- function")
  for (marker in names(verbs)) {
    end <- grep(paste0("^# END GENERATED PARAMETER CARD: ", marker, "$"), lines)
    verb <- grep(paste0("^", verbs[[marker]]), lines)
    expect_length(end, 1L)
    expect_length(verb, 1L)
    # only the function's own roxygen block stands between the card and the verb
    between <- lines[seq.int(end + 1L, verb - 1L)]
    expect_true(all(grepl("^#'", between)), info = marker)
    expect_true(verb > end, info = marker)
  }
})

test_that("changed fill values make the fill cards stale until they are regenerated", {
  root <- pc_temp_root()
  dir.create(file.path(root, "config")); dir.create(file.path(root, "R"))
  yaml_path <- file.path(root, "config", "analysis_plan.yaml")
  file.copy(file.path(parameter_card_root, "config", "analysis_plan.yaml"), yaml_path)
  writeLines(c("# BEGIN GENERATED PARAMETER CARD: AP1", "# stale",
               "# END GENERATED PARAMETER CARD: AP1"), file.path(root, "R", "ap1_exclusions.R"))
  writeLines(ap6_card_stub, file.path(root, "R", "ap6_regressions.R"))
  writeLines(c("before", ap3_card_stub, "impute_items <- function(data, codebook, analysis_plan) data"),
             file.path(root, "R", "ap3_fill.R"))
  writeLines(ap4_card_stub, file.path(root, "R", "ap4_factor_structure.R"))
  writeLines(efa_card_stub, file.path(root, "R", "ap9_efa.R"))
  writeLines(reliability_card_stub, file.path(root, "R", "ap4_reliability.R"))
  writeLines(network_card_stub, file.path(root, "R", "ap8_network.R"))
  writeLines(ap8_card_stub, file.path(root, "R", "ap7_joint_comparisons.R"))
  pc_write_further_card_stubs(root)

  expect_true(pc_update_parameter_cards(root))
  # regenerating a current card changes nothing
  expect_false(pc_update_parameter_cards(root))
  expect_false(pc_update_parameter_cards(root, check = TRUE))
  fill_file <- file.path(root, "R", "ap3_fill.R")
  first <- readLines(fill_file, warn = FALSE)
  expect_true(any(grepl("more than 2 missing items in one scale", first, fixed = TRUE)))
  expect_true(any(grepl("impute_items <- function(data, codebook, analysis_plan) data", first, fixed = TRUE)))

  lines <- readLines(yaml_path, warn = FALSE)
  lines <- sub("      items_per_scale_more_than: 2", "      items_per_scale_more_than: 1", lines, fixed = TRUE)
  lines <- sub('        family: poisson', '        family: negbinomial', lines, fixed = TRUE)
  writeLines(lines, yaml_path)
  expect_error(pc_update_parameter_cards(root, check = TRUE), "card is stale")
  expect_true(pc_update_parameter_cards(root))
  second <- readLines(fill_file, warn = FALSE)
  expect_true(any(grepl("more than 1 missing items in one scale", second, fixed = TRUE)))
  expect_true(any(grepl("Family: negbinomial.", second, fixed = TRUE)))
  expect_true(any(grepl("impute_items <- function(data, codebook, analysis_plan) data", second, fixed = TRUE)))
})

test_that("the AP4 CFA cards list the planned models of their family", {
  plan <- file.path(parameter_card_root, "config", "analysis_plan.yaml")

  subscale <- pc_ap4_cfa_card(plan, "subscale")
  expect_identical(subscale[1L], "# BEGIN GENERATED PARAMETER CARD: AP4 CFA SUBSCALE")
  expect_identical(subscale[2L], "# Automatically generated from analysis_plan.yaml")
  # nine one-factor models, each factor its own registered scale
  expect_length(grep("^#   ", subscale), 9L)
  expect_true(any(grepl('#   sdo_dom = sdo_dom', subscale, fixed = TRUE)))
  expect_true(any(grepl('"Social dominance orientation: dominance"', subscale, fixed = TRUE)))
  # the one-factor models in the codebook's order, each labelled with its
  # scale's `label` in codebook_scales.csv
  scales <- utils::read.csv(file.path(parameter_card_root, "..", "preregistration", "codebook_scales.csv"),
                            colClasses = "character")
  scales <- scales[order(as.integer(scales$order)), ]
  expect_identical(sub("^#   ([a-z_]+) = .*$", "\\1", grep("^#   ", subscale, value = TRUE)), scales$scale_key)
  for (i in seq_len(nrow(scales))) {
    expect_true(any(grepl(paste0('"', scales$label[i], '"'), subscale, fixed = TRUE)), info = scales$scale_key[i])
  }
  # the items are not typed onto the card; the codebook owns them
  expect_false(any(grepl("SDO-D_1", subscale, fixed = TRUE)))
  expect_identical(
    subscale[length(subscale) - 1L],
    "# Items per factor: the codebook's item set of that scale, in codebook order.")

  expect_true(any(grepl(
    '#   ums_achievement_intimacy = zm_security + zm_achievement   "UMS-6 (intimacy and achievement)"',
    pc_ap4_cfa_card(plan, "ums_dopl"), fixed = TRUE)))
  expect_true(any(grepl(
    "#   dopl_dominance_prestige = zm_power + zm_prestige",
    pc_ap4_cfa_card(plan, "ums_dopl"), fixed = TRUE)))
  expect_true(any(grepl(
    "#   social_motives_five_factor = zm_security + zm_arousal + zm_power + zm_prestige + zm_achievement",
    pc_ap4_cfa_card(plan, "social_motives"), fixed = TRUE)))
  expect_true(any(grepl(
    "#   asc_three_factor = asc_agg + asc_sub + asc_conv",
    pc_ap4_cfa_card(plan, "asc"), fixed = TRUE)))
  expect_true(any(grepl(
    "#   authoritarian_orientation_four_factor = asc_agg + asc_sub + asc_conv + sdo_dom",
    pc_ap4_cfa_card(plan, "auth_orientation"), fixed = TRUE)))

  expect_error(pc_ap4_cfa_card(plan, "not_a_family"), "names no family")
})

test_that("the checked-in AP4 CFA cards are the ones the generator writes", {
  # the same assertion the AP1 card carries: no hand-written card survives
  expect_false(pc_update_parameter_cards(parameter_card_root, check = TRUE))
  source_lines <- readLines(file.path(parameter_card_root, "R", "ap4_factor_structure.R"),
                            warn = FALSE)
  for (marker in ap4_card_markers) {
    expect_length(grep(paste0("^# BEGIN GENERATED PARAMETER CARD: ", marker, "$"), source_lines), 1L)
    expect_length(grep(paste0("^# END GENERATED PARAMETER CARD: ", marker, "$"), source_lines), 1L)
  }
})

test_that("the AP9 factor-criteria card lists the planned criteria in the plan's order", {
  plan <- file.path(parameter_card_root, "config", "analysis_plan.yaml")
  card <- pc_ap4_factor_criteria_card(plan)
  expect_identical(card[1L], "# BEGIN GENERATED PARAMETER CARD: AP9 FACTOR CRITERIA")
  expect_identical(card[3L], "# 9 criteria, each computed on its own; none selects a count.")
  criteria <- yaml::read_yaml(plan)$factor_analysis$factor_number_criteria
  # the criteria block: one line per criterion, directly beneath the count line
  expect_length(grep("^#   ", card[seq_len(3L + length(criteria))]), length(criteria))
  for (i in seq_along(criteria)) {
    line <- card[3L + i]
    expect_match(line, as.character(criteria[[i]]$key), fixed = TRUE)
    expect_match(line, as.character(criteria[[i]]$index), fixed = TRUE)
    expect_match(line, as.character(criteria[[i]]$label), fixed = TRUE)
  }
  # the common-factor parallel analysis the route adds is on the card
  expect_true(any(grepl("common_factor_parallel", card, fixed = TRUE)))
})

test_that("the AP9 factor-criteria card states the tuning settings the plan fixes", {
  plan <- file.path(parameter_card_root, "config", "analysis_plan.yaml")
  card <- pc_ap4_factor_criteria_card(plan)
  flat <- paste(card, collapse = " ")
  squeezed <- gsub("[[:space:]]*#[[:space:]]*", " ", flat)
  methods <- yaml::read_yaml(plan)$factor_analysis$factor_number_methods
  expect_true(any(grepl("^# Tuning settings the plan fixes:$", card)))
  # the settings come after the criteria, and every planned value is printed
  expect_gt(grep("^# Tuning settings the plan fixes:$", card), 3L + 9L)
  expect_true(grepl(paste0("reference datasets = ", methods$parallel_analysis$reference_datasets),
                    squeezed, fixed = TRUE))
  expect_true(grepl(paste0("percentile = ", methods$parallel_analysis$percentile),
                    squeezed, fixed = TRUE))
  expect_true(grepl(paste0("correlations = ", methods$parallel_analysis$correlation_type),
                    squeezed, fixed = TRUE))
  expect_true(grepl(paste0("reference-dataset correlations = ",
                           methods$parallel_analysis$random_correlation_type),
                    squeezed, fixed = TRUE))
  expect_true(grepl(paste0("population size = ", methods$comparison_data$population_size),
                    squeezed, fixed = TRUE))
  expect_true(grepl(paste0("samples = ", methods$comparison_data$samples),
                    squeezed, fixed = TRUE))
  expect_true(grepl("alpha = 0.3", squeezed, fixed = TRUE))
  expect_true(grepl(paste0("maximum iterations = ", methods$comparison_data$max_iterations),
                    squeezed, fixed = TRUE))
  expect_true(grepl(paste0("maximum factors = ", methods$vss$maximum_factors),
                    squeezed, fixed = TRUE))
  expect_true(grepl(paste0("eigenvalue threshold = ", methods$kaiser$threshold),
                    squeezed, fixed = TRUE))
  # the VSS complexity is not a plan field: the criterion key fixes it
  expect_true(grepl("complexity = 1 (fixed by the criterion key vss_complexity_1)",
                    squeezed, fixed = TRUE))
  # a missing tuning value stops the generator rather than printing a gap
  root <- pc_temp_root()
  dir.create(file.path(root, "config"))
  lines <- readLines(plan, warn = FALSE)
  writeLines(sub("^      percentile: 95.*$", "", lines),
             file.path(root, "config", "analysis_plan.yaml"))
  expect_error(pc_ap4_factor_criteria_card(file.path(root, "config", "analysis_plan.yaml")),
               "parallel_analysis$percentile is missing", fixed = TRUE)
})

test_that("the AP8 network model card states the nodes, the estimation and the decision rule", {
  plan <- file.path(parameter_card_root, "config", "analysis_plan.yaml")
  card <- pc_ap7_network_model_card(plan)
  values <- yaml::read_yaml(plan)$network
  expect_identical(card[1L], "# BEGIN GENERATED PARAMETER CARD: AP8 NETWORK MODEL")
  expect_identical(card[length(card)], "# END GENERATED PARAMETER CARD: AP8 NETWORK MODEL")
  flat <- paste(card, collapse = " ")
  # the nodes come from the plan, in the plan's order
  expect_true(grepl(paste(values$nodes, collapse = ", "),
                    paste(sub("^#[[:space:]]*", "", card), collapse = " "), fixed = TRUE))
  expect_true(grepl(paste0("Nodes (", length(values$nodes), ","), flat, fixed = TRUE))
  expect_true(grepl(paste0("Package: ", values$package), flat, fixed = TRUE))
  expect_true(grepl(paste0("Sweeps per fit: ", values$iter), flat, fixed = TRUE))
  expect_true(grepl(paste0("Continuity indicator per node: ", values$not_cont), flat, fixed = TRUE))
  expect_true(grepl(paste0("Edge inclusion prior: ", values$g_prior), flat, fixed = TRUE))
  expect_true(grepl(paste0("Degrees of freedom: ", values$df_prior), flat, fixed = TRUE))
  expect_true(grepl(paste0("Swept edge priors: ", paste(values$g_prior_sweep, collapse = ", ")),
                    flat, fixed = TRUE))
  expect_true(grepl(paste0("at least ", values$bf_include), flat, fixed = TRUE))
  expect_true(grepl(paste0("at most ", values$bf_exclude), flat, fixed = TRUE))
  # the bagging settings belong to the other card
  expect_false(grepl("Bootstrap samples", flat, fixed = TRUE))
  # a missing value stops the generator rather than printing a gap
  root <- pc_temp_root()
  dir.create(file.path(root, "config"))
  lines <- readLines(plan, warn = FALSE)
  writeLines(sub("^  df_prior: 3.*$", "", lines), file.path(root, "config", "analysis_plan.yaml"))
  expect_error(pc_ap7_network_model_card(file.path(root, "config", "analysis_plan.yaml")),
               "network.df_prior is missing", fixed = TRUE)
})

test_that("the AP8 network bagging card states B, the seeds and the feasibility trigger", {
  plan <- file.path(parameter_card_root, "config", "analysis_plan.yaml")
  card <- pc_ap7_network_bagging_card(plan)
  values <- yaml::read_yaml(plan)$network
  expect_identical(card[1L], "# BEGIN GENERATED PARAMETER CARD: AP8 NETWORK BAGGING")
  flat <- paste(card, collapse = " ")
  expect_true(grepl(paste0("Bootstrap samples: ", values$B), flat, fixed = TRUE))
  expect_true(grepl(paste0("Seed of bootstrap b: ", values$seed_base, " + b"), flat, fixed = TRUE))
  expect_true(grepl(paste0("Retry seed offset: ", values$retry_seed_offset), flat, fixed = TRUE))
  expect_true(grepl(paste0("Minimum success rate: ", values$min_success_rate), flat, fixed = TRUE))
  root <- pc_temp_root()
  dir.create(file.path(root, "config"))
  lines <- readLines(plan, warn = FALSE)
  writeLines(sub("^  B: 600.*$", "", lines), file.path(root, "config", "analysis_plan.yaml"))
  expect_error(pc_ap7_network_bagging_card(file.path(root, "config", "analysis_plan.yaml")),
               "network.B is missing", fixed = TRUE)
})

test_that("the checked-in AP8 network cards are what their generators produce", {
  source_lines <- readLines(file.path(parameter_card_root, "R", "ap8_network.R"), warn = FALSE)
  plan <- file.path(parameter_card_root, "config", "analysis_plan.yaml")
  for (card in list(pc_ap7_network_model_card(plan), pc_ap7_network_bagging_card(plan))) {
    start <- which(source_lines == card[1L])
    expect_length(start, 1L)
    expect_identical(source_lines[seq.int(start, start + length(card) - 1L)], card)
  }
})

test_that("the AP6 interval-level cards all print the configured credible mass", {
  plan <- file.path(parameter_card_root, "config", "analysis_plan.yaml")
  level <- yaml::read_yaml(plan)$regression$ci_level
  expect_equal(level, 0.95)
  markers <- c("AP6 INTERVAL LEVEL PRIOR WIDTH", "AP6 INTERVAL LEVEL POSTERIOR CHECK",
               "AP6 INTERVAL LEVEL COEFFICIENTS", "AP6 INTERVAL LEVEL R2")
  source_lines <- readLines(file.path(parameter_card_root, "R", "ap6_regressions.R"), warn = FALSE)
  for (marker in markers) {
    card <- pc_interval_level_card(plan, marker, "witness")
    expect_identical(card[1L], paste0("# BEGIN GENERATED PARAMETER CARD: ", marker))
    flat <- paste(card, collapse = " ")
    expect_true(grepl(paste0("Interval level: ", level), flat, fixed = TRUE), info = marker)
    expect_true(grepl("quantiles 0.025 and 0.975", flat, fixed = TRUE), info = marker)
    # the checked-in block is that generator's output
    start <- which(source_lines == card[1L])
    expect_length(start, 1L)
    checked_in <- source_lines[seq.int(start, which(source_lines ==
      paste0("# END GENERATED PARAMETER CARD: ", marker)))]
    expect_identical(checked_in[c(1L, 2L, 4L, 5L, length(checked_in))],
                     card[c(1L, 2L, 4L, 5L, length(card))], info = marker)
  }
  # a missing level stops the generator rather than printing a gap
  root <- pc_temp_root()
  dir.create(file.path(root, "config"))
  lines <- readLines(plan, warn = FALSE)
  writeLines(sub("^  ci_level: 0.95$", "", lines), file.path(root, "config", "analysis_plan.yaml"))
  expect_error(pc_interval_level_card(file.path(root, "config", "analysis_plan.yaml"), "M", "w"),
               "regression.ci_level is missing", fixed = TRUE)
})

test_that("the AP6 prior-predictive card prints the configured summary quantities", {
  plan <- file.path(parameter_card_root, "config", "analysis_plan.yaml")
  analysis_plan <- yaml::read_yaml(plan)
  card <- pc_ap6_prior_predictive_card(plan)
  flat <- paste(card, collapse = " ")
  # the summary quantities are plan values
  expect_equal(analysis_plan$regression$prior_predictive$absolute_summary_percentile, 0.95)
  expect_equal(analysis_plan$regression$prior_predictive$raw_scale_quantiles, c(0.05, 0.50, 0.95))
  expect_true(grepl("absolute standardised prediction: 0.95", flat, fixed = TRUE))
  expect_true(grepl("response scale: 0.05, 0.5, 0.95", flat, fixed = TRUE))
  expect_true(grepl(paste0(analysis_plan$scales$response_min, " to ", analysis_plan$scales$response_max),
                    flat, fixed = TRUE))
  root <- pc_temp_root()
  dir.create(file.path(root, "config"))
  lines <- readLines(plan, warn = FALSE)
  writeLines(sub("^    absolute_summary_percentile: 0.95.*$", "", lines),
             file.path(root, "config", "analysis_plan.yaml"))
  expect_error(pc_ap6_prior_predictive_card(file.path(root, "config", "analysis_plan.yaml")),
               "regression.prior_predictive.absolute_summary_percentile is missing", fixed = TRUE)
})

test_that("the AP6 joint card prints the configured residual-correlation prior", {
  plan <- file.path(parameter_card_root, "config", "analysis_plan.yaml")
  card <- pc_ap6_joint_rescor_card(plan)
  rescor <- yaml::read_yaml(plan)$priors$rescor
  expect_equal(rescor, "lkj(2)")
  expect_true(grepl(paste0("Residual-correlation prior: ", rescor),
                    paste(card, collapse = " "), fixed = TRUE))
  root <- pc_temp_root()
  dir.create(file.path(root, "config"))
  lines <- readLines(plan, warn = FALSE)
  writeLines(sub("^  rescor: .*$", "", lines), file.path(root, "config", "analysis_plan.yaml"))
  expect_error(pc_ap6_joint_rescor_card(file.path(root, "config", "analysis_plan.yaml")),
               "priors.rescor is missing", fixed = TRUE)
})

test_that("the AP7 card states the interval level", {
  plan <- file.path(parameter_card_root, "config", "analysis_plan.yaml")
  analysis_plan <- yaml::read_yaml(plan)
  card <- pc_ap8_intervals_card(plan)
  flat <- paste(card, collapse = " ")
  expect_true(grepl(paste0("Interval level: ", analysis_plan$regression$ci_level), flat, fixed = TRUE))
  # the checked-in block is the generator's output
  source_lines <- readLines(file.path(parameter_card_root, "R", "ap7_joint_comparisons.R"),
                            warn = FALSE)
  start <- which(source_lines == card[1L])
  expect_length(start, 1L)
  expect_identical(source_lines[seq.int(start, start + length(card) - 1L)], card)
})

test_that("the four AP9 EFA cards state the sets, counts, estimation and cutoff", {
  plan <- file.path(parameter_card_root, "config", "analysis_plan.yaml")
  fa <- yaml::read_yaml(plan)$factor_analysis
  item_sets <- pc_ap4_efa_item_sets_card(plan)
  for (set in Filter(function(x) isTRUE(x$efa), fa$sets)) {
    expect_true(any(grepl(paste0(set$key, " += ", paste(set$scales, collapse = ", "), "$"),
                          item_sets)), info = set$key)
  }
  counts <- paste(pc_ap4_efa_expected_counts_card(plan), collapse = "\n")
  expect_true(grepl(paste0("Each scale alone: ", fa$single_scale_expected_factors), counts,
                    fixed = TRUE))
  for (set in fa$sets) {
    expect_match(counts, paste0(set$key, " += ", set$expected_factors, " "), info = set$key)
  }
  estimation <- paste(pc_ap4_efa_estimation_card(plan), collapse = " ")
  expect_true(grepl(paste0("Estimation: ", fa$efa_estimation_method), estimation, fixed = TRUE))
  expect_true(grepl(paste0("Primary rotation: ", fa$rotation_primary), estimation, fixed = TRUE))
  expect_true(grepl(paste0("Oblimin gamma: ", fa$rotation_settings$oblimin_gamma), estimation, fixed = TRUE))
  expect_true(grepl(paste0("Starting rotations: ", fa$rotation_settings$starting_rotations), estimation, fixed = TRUE))
  expect_true(grepl("same rotation settings apply to VSS", estimation, fixed = TRUE))
  clarity <- paste(pc_ap4_loading_clarity_card(plan), collapse = " ")
  expect_true(grepl("absolute value at least 0.4", clarity, fixed = TRUE))
  # the checked-in blocks are the generators' output
  source_lines <- readLines(file.path(parameter_card_root, "R", "ap9_efa.R"), warn = FALSE)
  for (card in list(pc_ap4_efa_item_sets_card(plan), pc_ap4_efa_expected_counts_card(plan),
                    pc_ap4_efa_estimation_card(plan), pc_ap4_loading_clarity_card(plan))) {
    start <- which(source_lines == card[1L])
    expect_length(start, 1L)
    expect_identical(source_lines[seq.int(start, start + length(card) - 1L)], card)
  }
  # a missing value stops the generator rather than printing a gap
  root <- pc_temp_root()
  dir.create(file.path(root, "config"))
  lines <- readLines(plan, warn = FALSE)
  writeLines(sub("^  loading_display_cutoff: .*$", "", lines),
             file.path(root, "config", "analysis_plan.yaml"))
  expect_error(pc_ap4_loading_clarity_card(file.path(root, "config", "analysis_plan.yaml")),
               "factor_analysis.loading_display_cutoff is missing", fixed = TRUE)
})

test_that("the AP3, reliability, CFA, AP5, prior-sensitivity and Table 3 cards print their plan values", {
  plan <- file.path(parameter_card_root, "config", "analysis_plan.yaml")
  analysis_plan <- yaml::read_yaml(plan)
  flat <- function(card) paste(card, collapse = " ")
  expect_match(flat(pc_ap3_reversal_card(plan)),
               paste0("Response range: ", analysis_plan$scales$response_min, " to ", analysis_plan$scales$response_max), fixed = TRUE)
  expect_match(flat(pc_ap3_reversal_card(plan)), "becomes 7 - x", fixed = TRUE)
  covariates <- flat(pc_ap3_covariates_card(plan))
  expect_match(covariates, "13 = 12500", fixed = TRUE)
  expect_match(covariates, paste0("Household size top value: ", analysis_plan$income$hh_size_top_value), fixed = TRUE)
  region <- flat(pc_ap3_east_west_card(plan))
  expect_match(region, paste0("East: ", paste(analysis_plan$east_west$east, collapse = ", ")), fixed = TRUE)
  expect_match(region, paste0("West: ", paste(analysis_plan$east_west$west, collapse = ", ")), fixed = TRUE)
  expect_match(covariates, "18–24 = 21; 25–29 = 27;", fixed = TRUE)
  expect_match(covariates, "65–69 = 67", fixed = TRUE)
  expect_match(covariates, "Limits: 357, 500, 630, 800, 1050, 1400, 2000, 2800, 12500", fixed = TRUE)
  expect_match(covariates, "<500 = 422; 500–<630 = 561;", fixed = TRUE)
  expect_match(covariates, "2800+ = 5916", fixed = TRUE)
  expect_match(flat(pc_ap3_regression_input_card(plan)),
               paste0("Motives: ", paste(analysis_plan$regression$motives, collapse = ", ")), fixed = TRUE)
  expect_match(paste(sub("^#[[:space:]]*", "", pc_ap3_network_input_card(plan)), collapse = " "),
               paste(analysis_plan$network$nodes, collapse = ", "), fixed = TRUE)
  scientific_use <- flat(pc_ap3_scientific_use_card(plan))
  expect_match(scientific_use, "demo_gender, age_band, age_band_midpoint,", fixed = TRUE)
  expect_match(scientific_use, "income_band, income_band_value,", fixed = TRUE)
  expect_match(scientific_use, "Shared exactly as the analyses hold them: age_band_midpoint, income_band_value", fixed = TRUE)
  expect_match(scientific_use, "Not shared: demo_age, income, demo_edu_school, east_west", fixed = TRUE)
  expect_match(flat(pc_ap4_reliability_intervals_card(plan)),
               paste0("resamples per scale: ", analysis_plan$profiles$full$reliability_bootstrap_n), fixed = TRUE)
  expect_match(flat(pc_ap4_reliability_fallback_card(plan)), "finite omega: 0.8", fixed = TRUE)
  expect_match(flat(pc_ap4_cfa_estimation_card(plan)), "Estimator: WLSMV", fixed = TRUE)
  expect_match(flat(pc_ap4_cfa_residuals_card(plan)), "the 3 largest", fixed = TRUE)
  expect_match(flat(pc_ap5_composition_card(plan)),
               paste(analysis_plan$descriptives$demographics, collapse = ", "), fixed = TRUE)
  expect_match(flat(pc_ap5_correlations_card(plan)), "Coefficient: pearson", fixed = TRUE)
  expect_match(flat(pc_ap6_prior_sensitivity_card(plan)), "alphas: 0.99 and 1.01", fixed = TRUE)
  signs <- pc_ap7_prediction_signs_card(plan)
  expect_true(any(grepl("^#  +zm_security +zm_arousal +zm_power +zm_prestige +zm_achievement$", signs)))
  expect_true(any(grepl("^#   asc_conv  \\+ +- +\\+ +± +-$", signs)))
  expect_true(any(grepl("^#   asc_sub   \\+ +- +- +± +-$", signs)))
  expect_identical(sub("^#   ([a-z_]+) .*$", "\\1", signs[grepl("^#   [a-z]", signs)]),
                   c("asc_agg", "asc_sub", "asc_conv", "sdo_dom"))

  # every checked-in block is its generator's output
  registry <- list(
    "ap3_preprocessing.R" = list(pc_ap3_reversal_card(plan)),
    "ap3_preparation.R" = list(pc_ap3_east_west_card(plan), pc_ap3_covariates_card(plan), pc_ap3_standardisation_card(plan),
                               pc_ap3_network_input_card(plan), pc_ap3_regression_input_card(plan)),
    "ap3_data_files.R" = list(pc_ap3_scientific_use_card(plan)),
    "ap4_reliability.R" = list(pc_ap4_reliability_intervals_card(plan),
                               pc_ap4_reliability_fallback_card(plan)),
    "ap4_factor_structure.R" = list(pc_ap4_cfa_estimation_card(plan)),
    "ap4_cfa_reporting.R" = list(pc_ap4_cfa_residuals_card(plan)),
    "ap5_descriptives.R" = list(pc_ap5_composition_card(plan),
                                pc_ap5_correlations_card(plan)),
    "ap6_regressions.R" = list(pc_ap6_prior_sensitivity_card(plan)),
    "ap10_inference.R" = list(pc_ap7_prediction_signs_card(plan))
  )
  for (file in names(registry)) {
    source_lines <- readLines(file.path(parameter_card_root, "R", file), warn = FALSE)
    for (card in registry[[file]]) {
      start <- which(source_lines == card[1L])
      expect_length(start, 1L)
      expect_identical(source_lines[seq.int(start, start + length(card) - 1L)], card, info = card[1L])
    }
  }

  # a missing value stops the generator rather than printing a gap
  root <- pc_temp_root()
  dir.create(file.path(root, "config"))
  lines <- readLines(plan, warn = FALSE)
  writeLines(sub("^  omega_interval_min_success: .*$", "", lines),
             file.path(root, "config", "analysis_plan.yaml"))
  expect_error(pc_ap4_reliability_fallback_card(file.path(root, "config", "analysis_plan.yaml")),
               "reliability.omega_interval_min_success is missing", fixed = TRUE)
})
