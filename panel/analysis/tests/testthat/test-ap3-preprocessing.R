# R/ap3_preprocessing.R and R/ap3_preparation.R: reverse keying, scale means,
# the gender coding and its reference, the covariate derivations (log income,
# free text retained), standardisation within the model sample, and the
# preparation targets on the synthetic export

local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) {
    parent <- dirname(dir)
    if (identical(parent, dir)) stop("project root not found from ", getwd())
    dir <- parent
  }
  # simulate_testdata.R only for zm_truth_scenario(): the export realises one
  # declared scenario, and what the export-level tests expect is read from it.
  for (f in c("config.R", "io_qualtrics.R", "ap1_exclusions.R", "ap3_preprocessing.R", "ap3_preparation.R",
              "ap3_data_files.R", "simulate_testdata.R")) {
    source(file.path(dir, "R", f), local = FALSE)
  }
  source(file.path(dir, "tests", "support", "targets-route.R"), local = FALSE)
})

root <- zm_root()
analysis_plan <- zm_config(profile = "full", path = file.path(root, "config", "analysis_plan.yaml"))
gender_codes <- c(male = 1, female = 2, divers = 3)  # codebook value set gender_1_3
codebook <- zm_codebook(analysis_plan)
sav_path <- file.path(root, "data", "synthetic", "zm_panel_synthetic.sav")
all_items <- unique(unlist(codebook$scales$item_codes))

# ---- what the export scenario plants ------------------------------------------
# data/synthetic/ is generated under the scenario simulation_truth.yaml declares
# in `scenarios: export:`. `clean` plants nothing; `trouble` blanks a few cells
# among the AP1-kept rows, and each blanked cell is declared with the analysis
# variable it makes missing (`affects`): one blanked item makes its whole scale
# score missing, because AP3 scores a scale as a row mean with na.rm = FALSE.
# `distinct_rows` guarantees one respondent per cell, so counts over different
# variables add.
truth <- zm_truth_scenario(zm_truth(file.path(root, "config", "simulation_truth.yaml")))
latent <- readr::read_csv(
  file.path(root, "data", "synthetic", "zm_panel_synthetic_latent.csv"),
  show_col_types = FALSE, progress = FALSE
)
blanked_cells <- if (is.null(truth$missingness)) list() else truth$missingness$cells
planted_missing <- function(field, values) {
  sum(vapply(blanked_cells, function(cell) {
    if (as.character(cell[[field]]) %in% values) as.integer(cell$n_missing) else 0L
  }, integer(1)))
}

# Demographic / scalometer columns for n rows, deterministic.
demo_fixture <- function(n, gender = NULL) {
  if (is.null(gender)) gender <- rep_len(c(1, 2), n)
  tibble::tibble(
    demo_age = 20 + (seq_len(n) * 7) %% 50,
    demo_gender = gender,
    demo_income_hh_net = 1 + (seq_len(n) * 5) %% 13,
    demo_hh_members = 1 + (seq_len(n) * 3) %% 8,
    demo_bundesland = 1 + (seq_len(n) * 11) %% 16,
    demo_edu_school = rep_len(c(1, 2, 3, 4, 5, 9), n),
    pol_symp_spd = rep_len(c(2, -1, 0, 3), n),
    pol_symp_cdu_csu = rep_len(c(-2, 1, 0, 2), n),
    pol_symp_greens = rep_len(c(1, -3, 0, -1), n),
    pol_symp_fdp = rep_len(c(-1, 0, 0, 1), n),
    pol_symp_afd = rep_len(c(-3, 2, 0, -3), n),
    pol_symp_linke = rep_len(c(0, -2, 0, 1), n),
    quota_group = rep_len(c("left_leaning", "conservative_leaning", "mixed", "mixed"), n),
    Kommentarfeld = rep_len(c("", "ein Kommentar"), n)
  )
}

# Three hand-built rows over all 57 items: row 1 all 1, row 2 all 4, row 3 a
# hand-set pattern on zm_arousal and sdo_dom (3 elsewhere).
items_fixture <- function() {
  fx <- tibble::as_tibble(stats::setNames(
    lapply(all_items, function(code) c(1, 4, 3)), all_items
  ))
  fx$UNT_1_r[3] <- 2; fx$UNT_2[3] <- 5; fx$UNT_3[3] <- 3
  fx$UNT_4_r[3] <- 6; fx$UNT_5[3] <- 4; fx$UNT_6[3] <- 1
  fx$SDO_D_1[3] <- 2; fx$SDO_D_2[3] <- 3; fx$SDO_D_3[3] <- 1; fx$SDO_D_4[3] <- 4
  fx$SDO_D_5_r[3] <- 6; fx$SDO_D_6_r[3] <- 5; fx$SDO_D_7_r[3] <- 2; fx$SDO_D_8_r[3] <- 1
  fx
}

test_that("reverse keying is 7 - x on the flagged items only and is marked by an attribute", {
  fx <- items_fixture()
  rev <- reverse_items(fx, codebook, analysis_plan)
  reversed <- unique(unlist(codebook$scales$reverse_items))
  expect_setequal(attr(rev, "reversed_items"), reversed)
  expect_equal(length(reversed), 15)
  expect_true(all(c("UNT_1_r", "UNT_4_r", "SDO_D_5_r", "ASC_con_1_r", "ASC_aag_3_r", "ASC_asu_6_r") %in% reversed))
  for (code in reversed) expect_equal(rev[[code]], 7 - fx[[code]], info = code)
  for (code in setdiff(all_items, reversed)) expect_equal(rev[[code]], fx[[code]], info = code)
  # hand values, row 3
  expect_equal(rev$UNT_1_r[3], 5)
  expect_equal(rev$UNT_4_r[3], 1)
  expect_equal(rev$SDO_D_5_r[3], 1)
  expect_equal(rev$SDO_D_8_r[3], 6)
  expect_equal(rev$UNT_2[3], 5)
  # rows 1 and 2
  expect_equal(rev$UNT_1_r[1:2], c(6, 3))
  expect_equal(rev$UMS_ach_1[1:2], c(1, 4))
})

test_that("scale scores are hand-computable row means after reversal", {
  fx <- items_fixture()
  sc <- average_items_into_subscales(reverse_items(fx, codebook, analysis_plan), codebook)
  expect_true(all(codebook$scales$scale_key %in% names(sc)))
  # row 1: all items 1 -> reversed items 6
  expect_equal(sc$zm_arousal[1], (6 + 1 + 1 + 6 + 1 + 1) / 6)
  expect_equal(sc$sdo_dom[1], (4 * 1 + 4 * 6) / 8)
  expect_equal(sc$asc_agg[1], (1 + 1 + 6 + 6 + 6 + 1) / 6)
  expect_equal(sc$asc_conv[1], (6 + 1 + 1 + 1 + 6 + 6 + 1) / 7)
  expect_equal(sc$zm_achievement[1], 1)
  # row 2: all items 4 -> reversed items 3
  expect_equal(sc$zm_arousal[2], (3 + 4 + 4 + 3 + 4 + 4) / 6)
  expect_equal(sc$sdo_dom[2], (4 * 4 + 4 * 3) / 8)
  expect_equal(sc$zm_power[2], 4)
  # row 3: hand pattern -> zm_arousal (5+5+3+1+4+1)/6, sdo_dom (2+3+1+4 + 1+2+5+6)/8
  expect_equal(sc$zm_arousal[3], 19 / 6)
  expect_equal(sc$sdo_dom[3], 3)
  expect_equal(sc$zm_security[3], 3)
  # attribute survives, a missing item gives a missing score
  expect_setequal(attr(sc, "reversed_items"), unique(unlist(codebook$scales$reverse_items)))
  fx_na <- items_fixture()
  fx_na$UMS_int_2[2] <- NA
  sc_na <- average_items_into_subscales(reverse_items(fx_na, codebook, analysis_plan), codebook)
  expect_true(is.na(sc_na$zm_security[2]))
  expect_false(is.na(sc_na$zm_achievement[2]))
})

test_that("scale averaging preserves rows, items and missing scores", {
  fx <- reverse_items(items_fixture(), codebook, analysis_plan)
  fx$UMS_int_2[2] <- NA_real_
  scored <- average_items_into_subscales(fx, codebook)
  # the averaging adds the scale columns and touches no item answer
  expect_identical(scored[names(fx)], fx)
  expect_equal(nrow(scored), nrow(fx))
  expect_setequal(setdiff(names(scored), names(fx)), codebook$scales$scale_key)
  expect_true(is.na(scored$zm_security[2]))
  expect_false(is.na(scored$zm_achievement[2]))
})

test_that("income (net household income per household member): exact kept, banded log for the model", {
  d <- demo_fixture(5)
  d$demo_income_hh_net <- c(13, 1, 6, NA, 9)
  d$demo_hh_members <- c(8, 1, 4, 2, 3)
  cv <- derive_covariates(d, analysis_plan, codebook)
  # raw: band representative over the capped household size
  expect_equal(cv$income[1], 12500 / 8)
  expect_equal(cv$income[2], 250)
  expect_equal(cv$income[3], 1750 / 4)
  expect_true(is.na(cv$income[4]))
  expect_equal(cv$income[5], 3500 / 3)
  expect_equal(cv$income[1], analysis_plan$income$band_representative[["13"]] / min(8, analysis_plan$income$hh_size_top_value))
  # the band per household member next to the exact column, its representative logged
  expect_identical(as.character(cv$income_band), c("1400–<2000", "<500", "<500", NA, "1050–<1400"))
  expect_equal(cv$income_band_value_log, log(cv$income_band_value))
  expect_true(is.na(cv$income_band_value_log[4]))
  # household size 8 is the top value: band 9 there -> 3500/8 = 437.5
  d2 <- demo_fixture(1)
  d2$demo_income_hh_net <- 9
  d2$demo_hh_members <- 8
  expect_equal(derive_covariates(d2, analysis_plan, codebook)$income, 437.5)
})

test_that("an income band, a household size or a federal state outside the registered codes stops the pipeline", {
  d <- demo_fixture(5)
  d$demo_income_hh_net[2] <- 14
  expect_error(derive_covariates(d, analysis_plan, codebook),
               "Covariate input contains an unrecognised code.", fixed = TRUE)
  d2 <- demo_fixture(5)
  d2$demo_hh_members[3] <- 0
  expect_error(derive_covariates(d2, analysis_plan, codebook),
               "Covariate input contains an unrecognised code.", fixed = TRUE)
  d3 <- demo_fixture(5)
  d3$demo_hh_members[3] <- analysis_plan$income$hh_size_top_value + 1
  expect_error(derive_covariates(d3, analysis_plan, codebook),
               "Covariate input contains an unrecognised code.", fixed = TRUE)
  d4 <- demo_fixture(5)
  d4$demo_bundesland[1] <- 17
  expect_error(derive_east_west(d4, analysis_plan),
               "Covariate input contains an unrecognised federal-state code.", fixed = TRUE)
  # a missing band or household size stays missing, it does not stop
  d5 <- demo_fixture(5)
  d5$demo_income_hh_net[2] <- NA
  d5$demo_hh_members[3] <- NA
  expect_null(check_covariate_input_codes(d5, analysis_plan))
  cv <- derive_covariates(d5, analysis_plan, codebook)
  expect_true(is.na(cv$income[2]) && is.na(cv$income[3]))
})

test_that("gender is coded male/female/divers, in code order, with a marker for the answered rows", {
  d <- demo_fixture(9, gender = c(2, 2, 2, 2, 2, 1, 1, 1, 3))
  g <- prepare_gender(d, analysis_plan)
  expect_s3_class(g$gender, "factor")
  expect_false(is.ordered(g$gender))
  # the coding step keeps the codes' own order; which level is the reference is
  # a property of the retained population and is decided in set_gender_reference()
  expect_equal(levels(g$gender), c("male", "female", "divers"))
  expect_equal(as.character(g$gender), c(rep("female", 5), rep("male", 3), "divers"))
  expect_true(all(g$known_gender))
  # only the levels present in the data become levels
  d2 <- demo_fixture(5, gender = c(1, 1, 1, 2, 2))
  expect_equal(levels(prepare_gender(d2, analysis_plan)$gender), c("male", "female"))
  # an unanswered gender stays missing and is marked as unobserved; gender is
  # never imputed, so this marker is what the later steps select on
  d_na <- demo_fixture(3, gender = c(1, 2, NA))
  g_na <- prepare_gender(d_na, analysis_plan)
  expect_true(is.na(g_na$gender[3]))
  expect_equal(g_na$known_gender, c(TRUE, TRUE, FALSE))
})

test_that("the largest group of the population given becomes the treatment reference", {
  d <- prepare_gender(demo_fixture(9, gender = c(2, 2, 2, 2, 2, 1, 1, 1, 3)), analysis_plan)
  ref <- set_gender_reference(d)
  expect_equal(levels(ref$gender), c("female", "male", "divers"))
  expect_equal(as.character(ref$gender), as.character(d$gender))
  # male reference when males are the largest group
  d2 <- prepare_gender(demo_fixture(5, gender = c(1, 1, 1, 2, 2)), analysis_plan)
  expect_equal(levels(set_gender_reference(d2)$gender), c("male", "female"))
  # a tie follows the code order
  d3 <- prepare_gender(demo_fixture(4, gender = c(2, 2, 1, 1)), analysis_plan)
  expect_equal(levels(set_gender_reference(d3)$gender)[1], "male")
})

test_that("east/west follows analysis_plan$east_west with Berlin as east", {
  d <- demo_fixture(4)
  d$demo_bundesland <- c(11, 9, 14, 1) # Berlin, Bayern, Sachsen, Schleswig-Holstein
  cv <- derive_east_west(d, analysis_plan)
  expect_equal(as.character(cv$east_west), c("east", "west", "east", "west"))
  expect_equal(levels(cv$east_west), c("west", "east"))
  # an unmapped state code stops the pipeline
  d$demo_bundesland[1] <- 17
  expect_error(derive_east_west(d, analysis_plan),
               "Covariate input contains an unrecognised federal-state code.", fixed = TRUE)
})

test_that("education is an ordered factor with 9 (still at school) as its own last level", {
  d <- demo_fixture(6)
  d$demo_edu_school <- c(1, 2, 3, 4, 5, 9)
  cv <- derive_covariates(d, analysis_plan, codebook)
  expect_true(is.ordered(cv$education))
  expect_equal(nlevels(cv$education), 6)
  expect_equal(as.integer(cv$education), 1:6)
  expect_match(levels(cv$education)[6], "Sch")
  expect_match(levels(cv$education)[5], "Abitur")
  # a code outside the value set stops the pipeline
  d$demo_edu_school[1] <- 7
  expect_error(derive_covariates(d, analysis_plan, codebook),
               "An observed education code is not in the registered value set.", fixed = TRUE)
  # a missing code stays missing
  d2 <- demo_fixture(6)
  d2$demo_edu_school <- c(1, 2, 3, 4, 5, NA)
  expect_true(is.na(derive_covariates(d2, analysis_plan, codebook)$education[6]))
})

test_that("the free-text comment survives derive_covariates() (internal file only)", {
  d <- demo_fixture(4)
  cv <- derive_covariates(d, analysis_plan, codebook)
  expect_true("Kommentarfeld" %in% names(cv))
  expect_equal(cv$Kommentarfeld, d$Kommentarfeld)
  # Another handling of the comment is refused where the plan is loaded
  # (zm_config(); test-config.R).
  expect_match(analysis_plan$free_text_comment$handling, "^retained in the internal")
})

# The M4 rule alone: every exported cell blanked, so the covariate step
# computes each quota group from the party scalometers.
m4_rule <- function(d) {
  d$quota_group <- ""
  as.character(ap3_complete_missing_quota_groups(d)$quota_group)
}

test_that("quota_group takes the export where it is filled and the M4 rule where it is blank", {
  d <- demo_fixture(8)
  # rows 1..4 pattern: (left liked only), (conservative liked only), (nothing), (both)
  expect_equal(
    m4_rule(d)[1:4],
    c("left_leaning", "conservative_leaning", "mixed", "mixed")
  )
  cv <- derive_covariates(d, analysis_plan, codebook)
  expect_s3_class(cv$quota_group, "factor")
  expect_equal(levels(cv$quota_group), c("left_leaning", "conservative_leaning", "mixed"))
  expect_equal(attr(cv, "quota_group_mismatch"), 0L)
  expect_equal(attr(cv, "quota_group_computed"), 0L)
  expect_equal(cv$quota_group_source, rep("export", 8))
  expect_equal(cv$age, d$demo_age)
})

test_that("a filled cell that disagrees with the M4 rule is kept and counted, never overridden", {
  d <- demo_fixture(8)
  d$quota_group[1] <- "mixed"                      # rule says left_leaning
  cv <- derive_covariates(d, analysis_plan, codebook)
  expect_equal(as.character(cv$quota_group[1]), "mixed")     # the export stands
  expect_equal(as.character(m4_rule(d)[1]), "left_leaning")
  expect_equal(attr(cv, "quota_group_mismatch"), 1L)
  expect_equal(attr(cv, "quota_group_computed"), 0L)
  expect_equal(cv$quota_group_source[1], "export")
})

test_that("blank export cells are computed and counted, and NA where the scalometers do not decide", {
  # the first completes of fieldwork: the flow wrote no cell for them
  d <- demo_fixture(8)
  d$quota_group[c(1, 2, 3)] <- ""
  d$quota_group[5] <- NA_character_                # a missing value counts as blank too
  cv <- derive_covariates(d, analysis_plan, codebook)
  expect_equal(attr(cv, "quota_group_computed"), 4L)
  expect_equal(attr(cv, "quota_group_mismatch"), 0L)
  expect_equal(cv$quota_group_source, c("computed", "computed", "computed", "export",
                                        "computed", "export", "export", "export"))
  # the computed cells equal the rule, the filled ones the export
  expect_equal(as.character(cv$quota_group[1:3]), m4_rule(d)[1:3])
  expect_equal(as.character(cv$quota_group[c(4, 6, 7, 8)]), d$quota_group[c(4, 6, 7, 8)])

  # a blank cell whose scalometers do not decide stays missing
  d$pol_symp_spd[3] <- NA
  expect_true(is.na(m4_rule(d)[3]))
  cv_na <- derive_covariates(d, analysis_plan, codebook)
  expect_true(is.na(cv_na$quota_group[3]))
  expect_equal(cv_na$quota_group_source[3], "computed")

  # the synthetic export has every cell filled, so nothing is computed there
  raw <- read_qualtrics_export(sav_path)
  expect_true(all(nzchar(raw$quota_group[raw$survey_status == analysis_plan$exclusions$survey_status_complete])))
  exported <- raw$quota_group
  computed <- m4_rule(raw)
  filled <- nzchar(exported) & !is.na(computed)
  expect_equal(computed[filled], exported[filled])
})

test_that("the codebook names a z column for every scale and covariate", {
  # The unstandardised column and its standardised one stand side by side in
  # the codebook, and nothing downstream invents a name from a suffix.
  expect_false(anyNA(codebook$scales$z_col))
  expect_true(all(nzchar(codebook$scales$z_col)))
  expect_false(anyNA(codebook$covariates$z_col))
  expect_true(all(nzchar(codebook$covariates$z_col)))

  spec <- ap3_z_columns(analysis_plan, codebook)
  expect_equal(spec$z_col, zm_z_col(spec$var, codebook))

  # the columns of the four regressions and of the network are among them
  formula_columns <- trimws(strsplit(analysis_plan$regression$predictors, "+", fixed = TRUE)[[1]])
  expect_true(all(setdiff(formula_columns, "gender") %in% spec$z_col))
  expect_true(all(unlist(analysis_plan$network$nodes) %in% spec$z_col))
  expect_true(all(zm_z_col(unlist(analysis_plan$regression$outcomes), codebook) %in% spec$z_col))
})

test_that("ap3_z_columns() maps the model column names and the banded covariate sources", {
  spec <- ap3_z_columns(analysis_plan, codebook)
  expect_equal(names(spec), c("var", "source", "transform", "z_col", "role"))
  expect_equal(nrow(spec), 11)
  expect_equal(spec$role, c(rep("outcome", 4), rep("predictor", 5), "covariate", "covariate"))
  expect_equal(spec$z_col[spec$var == "income"], "income_z")
  expect_equal(spec$source[spec$var == "income"], "income_band_value_log")
  expect_equal(spec$transform[spec$var == "income"], "log")
  expect_equal(spec$z_col[spec$var == "age"], "age_z")
  expect_equal(spec$source[spec$var == "age"], "age_band_midpoint")
  expect_true(all(spec$transform[spec$var != "income"] == "identity"))
  scales <- !spec$var %in% c("age", "income")
  expect_true(all(spec$source[scales] == spec$var[scales]))
  expect_true(all(unlist(analysis_plan$network$nodes) %in% spec$z_col))
})

test_that("age falls into its five-year band and enters as the band midpoint", {
  bands <- add_age_and_income_bands(
    tibble::tibble(age = c(18, 24, 24.6, 25, 29, 30, 64, 65, 69, NA), income = 1000),
    analysis_plan)
  expect_identical(as.character(bands$age_band),
    c("18–24", "18–24", "18–24", "25–29", "25–29", "30–34", "60–64", "65–69", "65–69", NA))
  expect_equal(bands$age_band_midpoint, c(21, 21, 21, 27, 27, 32, 62, 67, 67, NA))
  expect_true(is.ordered(bands$age_band))
  expect_identical(levels(bands$age_band), analysis_plan$age_bands$labels)
  expect_equal(unname(calculate_age_band_midpoints(analysis_plan$age_bands)),
               c(21, 27, 32, 37, 42, 47, 52, 57, 62, 67))
  # a filled age outside the surveyed range belongs to the nearest band
  outside <- add_age_and_income_bands(tibble::tibble(age = c(17.4, 70.2), income = 1000), analysis_plan)
  expect_identical(as.character(outside$age_band), c("18–24", "65–69"))
})

test_that("income per household member falls into its right-open band and enters as the geometric mean of the limits", {
  income <- c(31.25, 499.99, 500, 629.99, 630, 1049.99, 1050, 2799.99, 2800, 12500, NA)
  bands <- add_age_and_income_bands(tibble::tibble(age = 40, income = income), analysis_plan)
  expect_identical(as.character(bands$income_band),
    c("<500", "<500", "500–<630", "500–<630", "630–<800", "800–<1050", "1050–<1400",
      "2000–<2800", "2800+", "2800+", NA))
  limits <- analysis_plan$income_bands_per_member$limits
  representatives <- calculate_income_band_representatives(analysis_plan$income_bands_per_member)
  expect_equal(unname(representatives), sqrt(limits[-length(limits)] * limits[-1]))
  expect_equal(unname(representatives[c("<500", "2800+")]), c(sqrt(357 * 500), sqrt(2800 * 12500)))
  expect_equal(round(unname(representatives)), c(422, 561, 710, 917, 1212, 1673, 2366, 5916))
  expect_equal(bands$income_band_value, unname(representatives[as.character(bands$income_band)]))
  expect_equal(bands$income_band_value_log, log(bands$income_band_value))
})

test_that("ap3_model_spec() lists one model per outcome and the network with their columns", {
  models <- ap3_model_spec(analysis_plan, codebook)
  expect_equal(names(models), c(unlist(analysis_plan$regression$outcomes), "network"))
  m <- models$asc_agg
  expect_equal(m$kind, "regression")
  expect_equal(m$outcome, "asc_agg")
  expect_equal(m$vars, c("asc_agg_z", paste0(unlist(analysis_plan$regression$motives), "_z"), "age_z", "gender", "income_z"))
  expect_setequal(m$sources, c("asc_agg", unlist(analysis_plan$regression$motives), "age", "gender", "income"))
  expect_setequal(m$z_vars, c("asc_agg", unlist(analysis_plan$regression$motives), "age", "income"))
  expect_false("asc_sub" %in% m$sources)
  net <- models$network
  expect_equal(net$kind, "network")
  expect_equal(net$vars, unlist(analysis_plan$network$nodes))
  expect_equal(net$sources, zm_key_of_z_col(unlist(analysis_plan$network$nodes), codebook))
  expect_false("age" %in% net$sources)
})

test_that("the preparation targets compose AP1 and AP3 on the synthetic export", {
  raw <- read_qualtrics_export(sav_path)
  input <- tr_intake(raw, analysis_plan)
  env <- new.env(parent = globalenv())
  sys.source(file.path(root, "tests", "support", "target-reader.R"), env)
  env$source_pipeline_functions(root)
  reader <- env$make_target_reader(root, list(
    analysis_inputs = env$make_analysis_inputs(input, analysis_plan, codebook),
    analysis_plan = analysis_plan, codebook = codebook))
  # The one-time preparation calculates these scores before reducing the
  # linked frame to scientific-use values and observed demographic margins.
  d <- reader$intake$data
  exclusion_log <- reader$intake$exclusions$log
  # 700 = 776 - 26 quota-full exits - 44 exclusions - the 6 divers respondents,
  # a group below analysis_plan$exclusions$gender_divers_min_n and therefore
  # excluded at step 6.
  expect_equal(nrow(d), 700L)
  expect_equal(exclusion_log$n_excluded, c(6L, 14L, 4L, 12L, 8L, 6L))
  # The two AP3 quota-cell counts travel with the table.
  expect_equal(attr(d, "quota_group_computed"), 0L)
  expect_equal(attr(d, "quota_group_mismatch"), 0L)
  # the analysis data are unstandardised: scores, covariates, items, free text
  expect_false(any(endsWith(names(d), "_z")))
  expect_true(all(codebook$scales$scale_key %in% names(d)))
  expect_true(all(c(
    "age", "age_band", "age_band_midpoint", "gender", "known_gender", "income", "income_band",
    "income_band_value", "income_band_value_log",
    "east_west", "education", "quota_group", "quota_group_source", "respondent_id", "Kommentarfeld"
  ) %in% names(d)))
  # two levels: prepare_gender() builds the factor from the levels present
  # in the data, and AP1 has excluded the export's divers group of six as smaller
  # than analysis_plan$exclusions$gender_divers_min_n (test-ap1-exclusions.R covers that
  # rule from both sides). A scenario may also blank a gender cell, which hides
  # whichever gender that respondent gave; na.rm: a blank is a missing answer,
  # not a divers answer, so such a row stays and its gender is NA. The levels
  # follow the treatment reference chosen on the retained intake population.
  expect_equal(levels(d$gender), c("female", "male"))
  expect_equal(d$known_gender, !is.na(d$gender))
  expect_equal(as.integer(truth$covariates$gender_divers_n), 6L)
  expect_lt(as.integer(truth$covariates$gender_divers_n), analysis_plan$exclusions$gender_divers_min_n)
  # Intake assigns random respondent numbers; use the fixture's explicit row
  # correspondence to inspect the planted latent genders, never numeric IDs.
  blanked_gender_ids <- raw$ResponseId[match(d$respondent_id[is.na(d$gender)], input$respondent_id)]
  expect_equal(length(blanked_gender_ids), planted_missing("affects", "gender"))
  divers_blanked <- sum(latent$gender[match(blanked_gender_ids, latent$ResponseId)] ==
                          gender_codes[["divers"]])
  expect_equal(divers_blanked, 0L)
  expect_equal(sum(d$gender == "divers", na.rm = TRUE), 0L)
  expect_equal(attr(d, "quota_group_mismatch"), 0L)
  expect_equal(attr(d, "quota_group_computed"), 0L)
  expect_equal(unique(d$quota_group_source), "export")
  expect_false(anyNA(d$quota_group))
  expect_true(all(d$sdo_dom >= analysis_plan$scales$response_min & d$sdo_dom <= analysis_plan$scales$response_max))

})

# The point of the AP1 minimum is what reaches the models: a retained group is a
# third gender level, a dropped one is not. The synthetic export's group is below
# the minimum, so both branches are shown here on a fixture rather than by
# regenerating the test bed. The prior side is in test-ap6-coefficient-summaries.R.
test_that("the AP1 minimum decides how many levels the model gender factor has", {
  minimum <- analysis_plan$exclusions$gender_divers_min_n
  divers_code <- gender_codes[["divers"]]
  gender_levels <- function(n_divers, n = 40L) {
    fx <- tibble::tibble(
      ResponseId = sprintf("G_%02d", seq_len(n)),
      Status = "0",
      consent_check = 1,
      Finished = 1L,
      Progress = 100L,
      survey_status = "complete",
      demo_age = 20 + seq_len(n),
      attentioncheck_1 = 3,
      attentioncheck_2 = 5,
      demo_gender = c(rep(divers_code, n_divers), rep_len(c(1, 2), n - n_divers))
    )
    kept <- apply_study_exclusions(fx, analysis_plan)$data
    levels(prepare_gender(kept, analysis_plan)$gender)
  }
  # Below the minimum the group leaves AP1, so gender is a two-level factor.
  expect_identical(gender_levels(minimum - 1L), c("male", "female"))
  # At the minimum it stays and is a third level of the model factor.
  expect_identical(gender_levels(minimum), c("male", "female", "divers"))
})
