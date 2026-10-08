# AP1 participant exclusions
#
# Completion is determined from Finished and Progress, not survey_status:
# Qualtrics records end-of-survey branch exits as finished.

# BEGIN GENERATED PARAMETER CARD: AP1
# Automatically generated from analysis_plan.yaml
# Consent
#   Required response code: 1
# Age
#   Minimum age: 18 years
#   Maximum age: 69 years
# Attention checks
#   First correct response code: 3
#   Second correct response code: 5
# Survey-flow statuses
#   Completed response: "complete"
#   Full quota cell: "quota_full"
# Gender "divers"
#   Minimum group size for retention: 30
# Test and preview responses
#   Removed upstream: yes
# END GENERATED PARAMETER CARD: AP1

#' Raw columns the AP1 exclusions read
#'
#' The intake deletion (AP3, `analysis_plan$data_files$intake`) lists several of
#' these among the columns to delete (ResponseId, Status, Finished,
#' Progress, survey_status). `ap3_intake_apply()` therefore defers their
#' deletion (attribute `intake_deferred`) and [apply_study_exclusions()]
#' removes them from its output once the exclusions are logged.
#'
#' @param intake_verified Whether the input passed the cleaned-intake receipt check.
#' @return Character vector of column names.
ap1_required_columns <- function(intake_verified = FALSE) {
  columns <- c(
    "ResponseId", "Status", "consent_check", "Finished", "Progress", "survey_status",
    "demo_age", "attentioncheck_1", "attentioncheck_2", "demo_gender"
  )
  if (isTRUE(intake_verified)) {
    columns <- c("respondent_id", setdiff(columns, c("ResponseId", "Status")))
  }
  columns
}

#' Assert that no preview or test responses are present (M7)
#'
#' Test data were deleted before live data collection (M7). Defensively, any
#' row whose Qualtrics `Status` label contains "Preview" or "Test" stops the
#' pipeline.
#'
#' @param raw Tibble from [read_qualtrics_export()].
#' @return `raw`, invisibly.
ap1_assert_no_test_rows <- function(raw) {
  label <- zm_status_label(raw$Status)
  bad <- grepl("Preview|Test", label, ignore.case = TRUE)
  if (any(bad, na.rm = TRUE)) {
    stop(
      "AP1: ", sum(bad, na.rm = TRUE), " preview/test response(s) present (Status: ",
      paste(unique(label[bad]), collapse = ", "),
      "). Test data must be deleted upstream (M7)."
    )
  }
  invisible(raw)
}

#' The exclusion conditions, named once
#'
#' Gives the configuration values the criteria read short, stable names. It
#' does not evaluate rows or remove anyone.
#'
#' Criterion 2 reads no plan value: completion is `Finished == 1` and
#' `Progress == 100` unconditionally. `Finished` and `Progress` are Qualtrics
#' export fields (0/1 flag and percentage), not analysis decisions.
#' The flow status is deliberately not read by that condition: Qualtrics exports
#' branch exits (consent refused, quota cell full, attention failed) with
#' `Finished = 1` and `Progress = 100`; the quota exits leave before the
#' criteria, and the others belong to criteria 1, 4 and 5.
#'
#' The age bounds exclude only a *known* age outside them; a missing age stays for
#' the AP3 numeric-missingness rule. `quota_full_status` is the flow status of
#' the quota branch, which ends the survey after the party scalometers and
#' before the questionnaire (M4); a missing status is not a quota-full exit.
#'
#' @param analysis_plan Configuration from [zm_config()].
#' @return Named list of the plan values the conditions read.
get_exclusion_conditions <- function(analysis_plan) {
  list(
    consent_required_value = analysis_plan$exclusions$consent_required_value,
    minimum_age = analysis_plan$exclusions$min_age,
    maximum_age = analysis_plan$exclusions$max_age,
    quota_full_status = analysis_plan$exclusions$survey_status_quota_full,
    attention_check_1_correct = analysis_plan$exclusions$attention_check_1_correct,
    attention_check_2_correct = analysis_plan$exclusions$attention_check_2_correct,
    complete_status = analysis_plan$exclusions$survey_status_complete,
    gender_divers_min_n = analysis_plan$exclusions$gender_divers_min_n
  )
}

#' Apply the exclusion flags in preregistration order
#'
#' Preserves sequential first-matching attribution: each flag is applied only to
#' rows that survived the earlier flags, so every excluded row is counted under
#' the first criterion it meets.
#'
#' @param data Tibble of respondents.
#' @param exclusion_flags Tibble of one logical column per criterion, in order.
#' @return `list(data, log)`; `log` has `step`, `criterion`, `n_before`,
#'   `n_excluded`, `n_after`.
apply_exclusion_flags_in_order <- function(data, exclusion_flags) {
  rows <- vector("list", ncol(exclusion_flags))
  remaining <- seq_len(nrow(data))
  for (i in seq_along(exclusion_flags)) {
    excluded <- exclusion_flags[[i]][remaining]
    n_before <- length(remaining)
    rows[[i]] <- tibble::tibble(
      step = i, criterion = names(exclusion_flags)[i],
      n_before = n_before, n_excluded = sum(excluded),
      n_after = n_before - sum(excluded))
    remaining <- remaining[!excluded]
  }
  list(data = data[remaining, , drop = FALSE], log = dplyr::bind_rows(rows))
}

#' AP1 postcondition — only the completion disposition may remain
#'
#' This is not a second intake check. After the quota-full exits have left and
#' the preregistered exclusion conditions have removed their rows — consent
#' refusals (step 1), break-offs (step 2) and attention failures (steps 4, 5) —
#' it stops only if an export disposition escaped every criterion, and lists
#' the offending statuses with their counts.
#'
#' @param data Tibble with column `survey_status` (after the conditions).
#' @param complete_status `analysis_plan$exclusions$survey_status_complete`.
#' @return `data`, invisibly.
check_only_complete_status_remains <- function(data, complete_status) {
  status <- as.character(data$survey_status)
  status[is.na(status)] <- ""
  bad <- status != complete_status
  if (any(bad)) {
    tab <- table(status[bad])
    shown <- ifelse(nzchar(names(tab)), names(tab), "<empty>")
    stop(
      "AP1: after the exclusion criteria, ", sum(bad), " remaining row(s) have survey_status != '",
      complete_status, "': ",
      paste(sprintf("'%s' (n = %d)", shown, as.integer(tab)), collapse = ", "),
      ". The preregistered criteria do not cover this disposition; inspect the export."
    )
  }
  invisible(data)
}

#' Record the postcondition and the conditional gender "divers" step
#'
#' The conditions leave a log of counts only. This adds the reporting the
#' AP1 case flow carries beside them: the flow-status assertion on the last
#' condition's row, and a last row for the conditional divers rule with the
#' size of the group, whether it was retained, and why.
#'
#' The divers group stays only when it reaches
#' `analysis_plan$exclusions$gender_divers_min_n`. An empty group is not a group the rule
#' can exclude.
#'
#' @param log Log from [apply_exclusion_flags_in_order()].
#' @param n_divers Size of the divers group after the conditions.
#' @param divers_kept Whether the rule retained it.
#' @param conditions Values from [get_exclusion_conditions()]; the two notes
#'   quote the configured minimum and completion status.
#' @return The log with the AP1 case-flow columns and the divers row.
append_divers_reporting_row <- function(log, n_divers, divers_kept, conditions) {
  last <- nrow(log)
  minimum <- conditions$gender_divers_min_n
  log$n_divers <- NA_integer_
  log$divers_kept <- NA
  log$note <- NA_character_
  log$note[last] <- sprintf(
    "asserted after step %d: all %d remaining rows have survey_status == '%s'",
    log$step[last], log$n_after[last], conditions$complete_status
  )
  n_before <- log$n_after[last]
  n_excluded <- if (divers_kept) 0L else as.integer(n_divers)
  divers_row <- tibble::tibble(
    step = as.integer(log$step[last] + 1L),
    criterion = "gender_divers",
    n_before = as.integer(n_before),
    n_excluded = n_excluded,
    n_after = as.integer(n_before - n_excluded),
    n_divers = as.integer(n_divers),
    divers_kept = divers_kept,
    note = if (n_divers == 0L) {
      "n_divers = 0: no respondents in this group"
    } else if (divers_kept) {
      sprintf(
        "n_divers = %d: the minimum of %d is reached, so they are retained",
        n_divers, minimum
      )
    } else {
      sprintf(
        "n_divers = %d: fewer than the minimum of %d, so they are excluded",
        n_divers, minimum
      )
    }
  )
  dplyr::bind_rows(log, divers_row)
}

#' The respondent number AP1 reports, and the test-row assertion it still needs
#'
#' A verified cleaned intake (attribute `intake_verified`) supplies
#' `respondent_id` without `ResponseId` or `Status`: the absence of preview and
#' test rows was verified before those technical fields were deleted at intake.
#' A raw export is asserted free of them here (M7).
#'
#' @param data Tibble from [read_qualtrics_export()] or the cleaned intake.
#' @return `data` with a `respondent_id` column.
ap1_prepare_exclusion_input <- function(data) {
  intake_verified <- isTRUE(attr(data, "intake_verified", exact = TRUE))
  data$respondent_id <- if ("respondent_id" %in% names(data)) data$respondent_id else data$ResponseId
  if (!intake_verified) ap1_assert_no_test_rows(data)
  data
}

#' The AP1 output columns: respondent number first, deferred deletions gone
#'
#' The intake deletion (AP3, `analysis_plan$data_files$intake`) lists several columns AP1
#' reads. `ap3_intake_apply()` therefore defers their deletion (attribute
#' `intake_deferred`) and AP1 removes them once the exclusions are logged.
#'
#' @param data Kept rows.
#' @param deferred Value of the input's `intake_deferred` attribute.
#' @return Tibble with `respondent_id` first and the deferred columns removed.
ap1_finish_exclusion_output <- function(data, deferred) {
  data <- tibble::as_tibble(data)
  data <- dplyr::relocate(data, "respondent_id")
  if (length(deferred) > 0) {
    data <- data[, setdiff(names(data), deferred), drop = FALSE]
  }
  data
}

#' AP1 — apply the participant exclusions sequentially
#'
#' Respondents whose political-leaning quota cell was already full were turned
#' away by the survey before the questionnaire (M4). They were never admitted,
#' so they leave first and are counted, not attributed to a criterion.
#'
#' The preregistered criteria then run in preregistration order as sequential
#' filters, so that every excluded row is counted under the first criterion it
#' meets: (1) not consenting, (2) incomplete (`Finished != 1` or
#' `Progress < 100`), (3) age below the minimum or above the maximum,
#' (4) first attention check wrong, (5) second attention check wrong,
#' (6) gender "divers" in a group
#' below `analysis_plan$exclusions$gender_divers_min_n`. Step 6 is conditional:
#' when the group reaches the minimum nobody is dropped and the step only
#' records its size, and when it does not those rows leave the analysis
#' ([append_divers_reporting_row()]). Preview and test rows are
#' asserted absent first (M7, [ap1_prepare_exclusion_input()]). After step 5
#' every remaining row is asserted to carry the completion flow status
#' ([check_only_complete_status_remains()]); the assertion is recorded in the
#' `note` column of the step-5 log row.
#'
#' When `raw` comes from `ap3_intake_apply()` it already carries the
#' running-number `respondent_id` (kept as is) and the attribute
#' `intake_deferred`, the approved-for-deletion columns AP1 still needed;
#' those columns are removed from the returned data.
#' A verified cleaned intake (`intake_verified = TRUE` attribute) instead
#' supplies `respondent_id` without `ResponseId` or `Status`: absence of test
#' rows was verified before those technical fields were deleted at intake.
#'
#' @param raw Tibble from [read_qualtrics_export()], optionally after
#'   `ap3_intake_apply()`.
#' @param analysis_plan Configuration from [zm_config()].
#' @return `list(data, log, n_started, n_quota_full)`: `data` is the kept
#'   tibble with `respondent_id` as first column (the intake running number
#'   when present, else `ResponseId`); `log` is a tibble with one row per
#'   criterion: `step`, `criterion`, `n_before`, `n_excluded`, `n_after`, plus
#'   `n_divers` and `divers_kept` (filled in the last row) and `note` (the
#'   flow-status assertion on step 5, and on step 6 the size of the divers
#'   group with whether it was retained or excluded and why); `n_started` is
#'   the number of responses in the input and `n_quota_full` the number turned
#'   away at a full quota cell.
apply_study_exclusions <- function(raw, analysis_plan) {
  data <- ap1_prepare_exclusion_input(raw)
  deferred <- attr(raw, "intake_deferred", exact = TRUE)
  conditions <- get_exclusion_conditions(analysis_plan)
  n_started <- nrow(data)
  turned_away <- !is.na(data$survey_status) & data$survey_status == conditions$quota_full_status
  data <- data[!turned_away, , drop = FALSE]
  exclusion_flags <- data |>
    dplyr::transmute(
      not_consenting = is.na(consent_check) |
        consent_check != conditions$consent_required_value,
      incomplete = is.na(Finished) | Finished != 1 |
        is.na(Progress) | Progress < 100,
      age_outside_range = !is.na(demo_age) &
        (demo_age < conditions$minimum_age | demo_age > conditions$maximum_age),
      attention_1_wrong = is.na(attentioncheck_1) |
        attentioncheck_1 != conditions$attention_check_1_correct,
      attention_2_wrong = is.na(attentioncheck_2) |
        attentioncheck_2 != conditions$attention_check_2_correct
    )
  result <- apply_exclusion_flags_in_order(data, exclusion_flags)
  check_only_complete_status_remains(
    result$data, conditions$complete_status)
  n_divers <- sum(result$data$demo_gender == 3, na.rm = TRUE)
  divers_kept <- n_divers == 0L ||
    n_divers >= conditions$gender_divers_min_n
  if (!divers_kept) result$data <- dplyr::filter(
    result$data, is.na(.data$demo_gender) | .data$demo_gender != 3)
  result$log <- append_divers_reporting_row(
    result$log, n_divers, divers_kept, conditions)
  result$data <- ap1_finish_exclusion_output(result$data, deferred)
  result$n_started <- n_started
  result$n_quota_full <- sum(turned_away)
  result
}
