# R/ap1_exclusions.R: sequential exclusion log on the synthetic export, hand-built fixtures

local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) {
    parent <- dirname(dir)
    if (identical(parent, dir)) stop("project root not found from ", getwd())
    dir <- parent
  }
  # simulate_testdata.R only for zm_truth_scenario(): the export realises one
  # declared scenario, and the expectations below are read from it.
  for (f in c("config.R", "io_qualtrics.R", "ap1_exclusions.R", "simulate_testdata.R")) {
    source(file.path(dir, "R", f), local = FALSE)
  }
})

root <- zm_root()
analysis_plan <- zm_config(profile = "full", path = file.path(root, "config", "analysis_plan.yaml"))
gender_codes <- c(male = 1, female = 2, divers = 3)  # codebook value set gender_1_3
# The truth resolved to the scenario simulation_truth.yaml declares in
# `scenarios: export:` -- the one data/synthetic/ is generated from. Under
# `clean` nothing is planted; under `trouble` the expectations derive
# from the planted faults.
truth <- zm_truth_scenario(zm_truth(file.path(root, "config", "simulation_truth.yaml")))
raw <- read_qualtrics_export(file.path(root, "data", "synthetic", "zm_panel_synthetic.sav"))
# The latent file carries the truth behind a blanked cell (the fault is in the
# export, not in the truth), so a blanked gender can still be attributed.
latent <- readr::read_csv(
  file.path(root, "data", "synthetic", "zm_panel_synthetic_latent.csv"),
  show_col_types = FALSE, progress = FALSE
)

# Cells the export scenario blanks among the AP1-kept rows: `column` is the
# export column, `affects` the analysis variable the blank makes missing,
# `n_missing` the number of respondents. `distinct_rows` guarantees one
# respondent per cell, so counts over different columns add. Empty under `clean`.
blanked_cells <- if (is.null(truth$missingness)) list() else truth$missingness$cells
planted_missing <- function(field, values) {
  sum(vapply(blanked_cells, function(cell) {
    if (as.character(cell[[field]]) %in% values) as.integer(cell$n_missing) else 0L
  }, integer(1)))
}

# A minimal raw-like fixture: every column AP1 reads, one row per case.
# R_10 is a quota-full exit: the survey ended it after the scalometers, so it
# looks finished, carries an adult age and has no attention-check answer at all.
ap1_fixture <- function() {
  tibble::tibble(
    ResponseId = sprintf("R_%02d", 1:10),
    Status = "0",
    consent_check = c(2, 1, 1, 1, 1, 1, 1, 1, 1, 1),
    Finished = c(1L, 0L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L),
    Progress = c(100L, 40L, 100L, 100L, 100L, 100L, 100L, 100L, 100L, 100L),
    survey_status = c("consent_refused", "", "complete", "attention_failed", "attention_failed",
                      "complete", "complete", "complete", "complete", "quota_full"),
    demo_age = c(NA, 30, 17, 40, 41, 50, 52, 33, 61, 45),
    attentioncheck_1 = c(NA, 3, 3, 4, 3, 3, 3, 3, 3, NA),
    attentioncheck_2 = c(NA, NA, 5, NA, 2, 5, 5, 5, 5, NA),
    demo_gender = c(NA, NA, 2, NA, NA, 3, 1, 2, 2, NA)
  )
}

test_that("sequential exclusion log on the synthetic export equals simulation_truth.yaml", {
  res <- apply_study_exclusions(raw, analysis_plan)
  log <- res$log

  expect_equal(nrow(log), 6)
  expect_equal(log$step, 1:6)
  expect_equal(
    log$criterion,
    c("not_consenting", "incomplete", "age_outside_range",
      "attention_1_wrong", "attention_2_wrong", "gender_divers")
  )
  # the quota-full exits leave before the criteria and are counted beside them
  expect_equal(res$n_started, truth$n_total)
  expect_equal(res$n_quota_full, as.integer(truth$exclusions$quota_full))
  expect_equal(log$n_before[1], truth$n_total - truth$exclusions$quota_full)

  # Step 6 is the divers rule. The generator forces the declared number of
  # divers respondents, but a scenario may blank a gender cell, and a blank
  # hides whichever gender that respondent gave; a blanked gender is a missing
  # answer, not a divers answer, so it neither counts towards the group nor
  # leaves with it. The visible count is therefore the declared number minus
  # the blanked cells that hit a divers respondent; which respondents were hit
  # is read off the export, their true gender off the latent file.
  divers_code <- gender_codes[["divers"]]
  expect_equal(as.integer(truth$covariates$gender_divers_n), 6L)
  blanked_gender_ids <- res$data$respondent_id[is.na(res$data$demo_gender)]
  expect_equal(length(blanked_gender_ids), planted_missing("affects", "gender"))
  divers_blanked <- sum(latent$gender[match(blanked_gender_ids, latent$ResponseId)] == divers_code)
  n_divers_expected <- as.integer(truth$covariates$gender_divers_n) - divers_blanked
  # The export's divers group is smaller than the configured minimum, so step 6
  # excludes it: this export exercises the excluding branch of the rule.
  expect_lt(n_divers_expected, analysis_plan$exclusions$gender_divers_min_n)

  expect_equal(
    log$n_excluded,
    as.integer(c(
      truth$exclusions$consent_refused, truth$exclusions$incomplete, truth$exclusions$age_under_18,
      truth$exclusions$attention_1_failed, truth$exclusions$attention_2_failed, n_divers_expected
    ))
  )
  expect_equal(log$n_excluded, c(6L, 14L, 4L, 12L, 8L, 6L))
  # truth$n_kept counts the sample the preregistered criteria leave; the
  # divers rule then removes the small group on top of it.
  expect_equal(log$n_after[5], truth$n_kept)
  expect_equal(log$n_after[6], truth$n_kept - n_divers_expected)
  expect_equal(log$n_after[6], 700L)
  expect_equal(nrow(res$data), 700L)

  # sequential bookkeeping
  expect_equal(log$n_before - log$n_excluded, log$n_after)
  expect_equal(log$n_after[-6], log$n_before[-1])

  expect_equal(log$n_divers[6], n_divers_expected)
  expect_false(log$divers_kept[6])
  expect_match(log$note[6], paste0("n_divers = ", n_divers_expected))
  expect_match(log$note[6], "fewer than the minimum of 30, so they are excluded")
  # the group is gone from the data, and the blanked gender cell stays: na.rm,
  # a blank is a missing answer, not a divers answer
  expect_equal(sum(res$data$demo_gender == divers_code, na.rm = TRUE), 0L)
  expect_equal(sum(is.na(res$data$demo_gender)), planted_missing("affects", "gender"))

  # the quota-full exits are gone from the data
  expect_equal(sum(raw$survey_status == analysis_plan$exclusions$survey_status_quota_full), 26L)
  expect_false(any(res$data$survey_status == analysis_plan$exclusions$survey_status_quota_full))

  # The flow-status assertion is on the last exclusion criterion.
  expect_match(log$note[5], "survey_status == 'complete'")
  expect_equal(log$n_after[5], 706L)
  expect_match(log$note[5], sprintf("all %d remaining rows", log$n_after[5]))
})

test_that("quota-full exits leave before the criteria and are counted beside them", {
  # the quota branch ends the survey before the questionnaire, so those rows
  # have no attention answer; they leave before the criteria, not as check-1
  # failures
  fx <- ap1_fixture()
  res <- apply_study_exclusions(fx, analysis_plan)
  expect_equal(res$n_started, 10L)
  expect_equal(res$n_quota_full, 1L)
  expect_equal(res$log$n_before[1], 9L)
  expect_true(is.na(fx$attentioncheck_1[10]))            # it would fail there
  expect_equal(res$log$n_excluded[4], 1L)                # but only R_04 is counted there
  expect_false("R_10" %in% res$data$respondent_id)

  # the status value is read from the configuration, not hard-coded
  cfg_other <- analysis_plan
  cfg_other$exclusions$survey_status_quota_full <- "cell_closed"
  fx2 <- fx
  fx2$survey_status[10] <- "cell_closed"
  expect_equal(apply_study_exclusions(fx2, cfg_other)$n_quota_full, 1L)
  # An empty or absent status is refused where the plan is loaded (zm_config()),
  # so the condition is a plain comparison (test-config.R).
})

test_that("kept data carry respondent_id = ResponseId and only complete, valid rows", {
  res <- apply_study_exclusions(raw, analysis_plan)
  d <- res$data
  expect_equal(names(d)[1], "respondent_id")
  expect_equal(d$respondent_id, d$ResponseId)
  expect_equal(anyDuplicated(d$respondent_id), 0)
  expect_true(all(d$survey_status == analysis_plan$exclusions$survey_status_complete))
  expect_true(all(d$Finished == 1L))
  expect_true(all(d$Progress == 100L))
  expect_true(all(d$consent_check == analysis_plan$exclusions$consent_required_value))
  # AP1 criterion 3 excludes ages BELOW the minimum, so a row whose age the
  # export scenario blanks is kept: the rule is checked on the answered ages,
  # and the blanks are counted against the scenario rather than ignored.
  expect_true(all(d$demo_age >= analysis_plan$exclusions$min_age, na.rm = TRUE))
  expect_true(all(d$demo_age <= analysis_plan$exclusions$max_age, na.rm = TRUE))
  expect_equal(sum(is.na(d$demo_age)), planted_missing("affects", "age"))
  expect_true(all(d$attentioncheck_1 == analysis_plan$exclusions$attention_check_1_correct))
  expect_true(all(d$attentioncheck_2 == analysis_plan$exclusions$attention_check_2_correct))
  # a blanked gender is no gender code; the answered ones must all be codes and
  # the blanks must be exactly the ones the scenario plants
  expect_true(all(d$demo_gender[!is.na(d$demo_gender)] %in% unname(gender_codes)))
  expect_equal(sum(is.na(d$demo_gender)), planted_missing("affects", "gender"))
  # the export's divers group is below the minimum, so no divers respondent is
  # left here (the rule itself is tested below, from both sides)
  expect_equal(sum(d$demo_gender == gender_codes[["divers"]], na.rm = TRUE), 0L)
  item_cols <- grep("^(UMS_|DOPL_|UNT_|ASC_|SDO_D_)", names(d), value = TRUE)
  expect_equal(length(item_cols), 57)
  # every item cell of a kept row is answered except the ones the scenario
  # blanks (none under `clean`), and each blanked column carries exactly the
  # declared number of them
  expect_equal(sum(is.na(d[item_cols])), planted_missing("column", item_cols))
  for (col in item_cols) {
    expect_equal(sum(is.na(d[[col]])), planted_missing("column", col), info = col)
  }
})

test_that("each row of the fixture is attributed to the first criterion it meets", {
  fx <- ap1_fixture()
  res <- apply_study_exclusions(fx, analysis_plan)
  log <- res$log
  # R_10 quota-full exit leaves first; then R_01 consent (finished-looking),
  # R_02 break-off, R_03 underage, R_04 check 1, R_05 check 2,
  # R_06 divers (a group of one, below the minimum, so step 6 excludes it),
  # R_07..R_09 kept
  expect_equal(res$n_quota_full, 1L)
  expect_equal(log$n_excluded, c(1L, 1L, 1L, 1L, 1L, 1L))
  expect_equal(log$n_after, c(8L, 7L, 6L, 5L, 4L, 3L))
  expect_equal(res$data$respondent_id, c("R_07", "R_08", "R_09"))
  expect_equal(log$n_divers[6], 1L)
  expect_false(log$divers_kept[6])
})

test_that("a consent refusal or attention failure exported as finished is not counted as incomplete", {
  fx <- ap1_fixture()
  # Each condition on its own, one fixture row at a time: the row that meets
  # only that condition is counted under it, whatever its export disposition.
  attributed <- function(i) {
    log <- apply_study_exclusions(fx[i, , drop = FALSE], analysis_plan)$log
    log$criterion[log$n_excluded > 0L]
  }
  expect_equal(attributed(1L), "not_consenting")
  expect_equal(attributed(2L), "incomplete")
  expect_equal(attributed(3L), "age_outside_range")
  expect_equal(attributed(4L), "attention_1_wrong")
  expect_equal(attributed(5L), "attention_2_wrong")
  # The consent refusal is exported as finished, so criterion 2 never sees it;
  # it leaves at its own criterion instead.
  expect_equal(fx$Finished[1L], 1L)
  expect_equal(fx$Progress[1L], 100L)
  # and on the whole fixture each of them is counted exactly once
  expect_equal(apply_study_exclusions(fx, analysis_plan)$log$n_excluded[1:5], rep(1L, 5))
})

test_that("only known ages outside the inclusive age range are excluded and missing age remains", {
  # A blank age is a missing answer, not an age below the minimum.
  ages <- ap1_fixture()[rep(7L, 6L), ]
  ages$ResponseId <- sprintf("A_%d", 1:6)
  ages$demo_age <- c(NA_real_, 17, 18, 19, 69, 70)
  out_ages <- apply_study_exclusions(ages, analysis_plan)
  expect_equal(out_ages$log$n_excluded[3], 2L)
  expect_equal(out_ages$data$respondent_id, c("A_1", "A_3", "A_4", "A_5"))
  # The upper bound is read from the configuration and checked before attention.
  custom <- analysis_plan
  custom$exclusions$max_age <- 68L
  expect_false("A_5" %in% apply_study_exclusions(ages, custom)$data$respondent_id)
  ages$attentioncheck_1[6] <- 4L
  precedence <- apply_study_exclusions(ages, analysis_plan)$log
  expect_equal(precedence$n_excluded[precedence$criterion == "age_outside_range"], 2L)
  expect_equal(precedence$n_excluded[precedence$criterion == "attention_1_wrong"], 0L)
  fx <- ap1_fixture()
  fx$demo_age[7L] <- NA_real_
  out <- apply_study_exclusions(fx, analysis_plan)
  expect_true("R_07" %in% out$data$respondent_id)
  expect_true(is.na(out$data$demo_age[out$data$respondent_id == "R_07"]))
  expect_equal(out$log$n_excluded[out$log$criterion == "age_outside_range"], 1L)
})

test_that("verified cleaned intake needs only respondent numbers and still applies AP1", {
  raw <- ap1_fixture()
  expected <- apply_study_exclusions(raw, analysis_plan)
  clean <- raw[, setdiff(names(raw), c("ResponseId", "Status"))]
  clean$respondent_id <- seq_len(nrow(clean))
  attr(clean, "intake_verified") <- TRUE
  attr(clean, "intake_deferred") <- c("Finished", "Progress", "survey_status")
  out <- apply_study_exclusions(clean, analysis_plan)
  expect_equal(out$data$respondent_id, 7:9)
  expect_equal(out$log, expected$log)
  expect_false(any(c("ResponseId", "Status", "Finished", "Progress", "survey_status") %in% names(out$data)))
  expect_setequal(ap1_required_columns(TRUE), c("respondent_id", setdiff(ap1_required_columns(), c("ResponseId", "Status"))))
})

test_that("a divers group below the minimum is excluded and one at the minimum is kept", {
  # The rule from both sides on the same frame, so that only the group size
  # differs: twenty-nine divers respondents leave, thirty reach the minimum and
  # stay.
  minimum <- analysis_plan$exclusions$gender_divers_min_n
  expect_equal(minimum, 30)
  divers_code <- gender_codes[["divers"]]
  frame <- function(n_divers, n = 60L) {
    tibble::tibble(
      ResponseId = sprintf("D_%02d", seq_len(n)),
      Status = "0",
      consent_check = 1,
      Finished = 1L,
      Progress = 100L,
      survey_status = "complete",
      demo_age = rep(21:69, length.out = n),
      attentioncheck_1 = 3,
      attentioncheck_2 = 5,
      demo_gender = c(rep(divers_code, n_divers), rep_len(c(1, 2), n - n_divers))
    )
  }
  log_columns <- c("step", "criterion", "n_before", "n_excluded", "n_after",
                   "n_divers", "divers_kept", "note")

  # twenty-nine: the group leaves, and the step logs a real exclusion
  below <- frame(29L)
  res_below <- apply_study_exclusions(below, analysis_plan)
  log_below <- res_below$log
  expect_identical(names(log_below), log_columns)
  expect_equal(log_below$criterion[6], "gender_divers")
  expect_equal(log_below$n_divers[6], 29L)
  expect_false(log_below$divers_kept[6])
  expect_equal(log_below$n_before[6], 60L)
  expect_equal(log_below$n_excluded[6], 29L)
  expect_equal(log_below$n_after[6], 31L)
  expect_equal(nrow(res_below$data), 31L)
  expect_equal(sum(res_below$data$demo_gender == divers_code), 0L)
  expect_equal(res_below$data$respondent_id, sprintf("D_%02d", 30:60))
  expect_match(log_below$note[6], "n_divers = 29")
  expect_match(log_below$note[6], "fewer than the minimum of 30, so they are excluded")

  # thirty: nobody leaves, and the step logs the size and the retention
  at <- frame(30L)
  res_at <- apply_study_exclusions(at, analysis_plan)
  log_at <- res_at$log
  expect_identical(names(log_at), log_columns)
  expect_equal(log_at$n_divers[6], 30L)
  expect_true(log_at$divers_kept[6])
  expect_equal(log_at$n_before[6], 60L)
  expect_equal(log_at$n_excluded[6], 0L)
  expect_equal(log_at$n_after[6], 60L)
  expect_equal(nrow(res_at$data), 60L)
  expect_equal(sum(res_at$data$demo_gender == divers_code), 30L)
  expect_match(log_at$note[6], "n_divers = 30")
  expect_match(log_at$note[6], "the minimum of 30 is reached, so they are retained")

  # the threshold is the configured value, not a number in the code
  cfg_low <- analysis_plan
  cfg_low$exclusions$gender_divers_min_n <- 29
  res_low <- apply_study_exclusions(below, cfg_low)
  expect_true(res_low$log$divers_kept[6])
  expect_equal(res_low$log$n_excluded[6], 0L)
  expect_equal(nrow(res_low$data), 60L)

  # a blanked gender is a missing answer, not a divers answer: it does not
  # count towards the group, and that row is not one of the excluded
  blanked <- frame(30L)
  blanked$demo_gender[1] <- NA
  res_blanked <- apply_study_exclusions(blanked, analysis_plan)
  expect_equal(res_blanked$log$n_divers[6], 29L)
  expect_false(res_blanked$log$divers_kept[6])
  expect_equal(res_blanked$log$n_excluded[6], 29L)
  expect_true("D_01" %in% res_blanked$data$respondent_id)
})

test_that("a remaining row with a foreign flow status stops the pipeline after the last criterion", {
  fx <- ap1_fixture()
  # an unforeseen disposition has no criterion
  fx$survey_status[7] <- "screened_out"
  fx$survey_status[8] <- ""
  expect_error(apply_study_exclusions(fx, analysis_plan), "screened_out")
  expect_error(apply_study_exclusions(fx, analysis_plan), "<empty>")
  expect_error(apply_study_exclusions(fx, analysis_plan), "after the exclusion criteria")
})

test_that("preview or test rows stop the pipeline", {
  fx <- ap1_fixture()
  fx$Status[3] <- "1"
  expect_error(apply_study_exclusions(fx, analysis_plan), "preview/test")
  fx <- ap1_fixture()
  fx$Status[3] <- "Survey Test"
  expect_error(apply_study_exclusions(fx, analysis_plan), "preview/test")
})
