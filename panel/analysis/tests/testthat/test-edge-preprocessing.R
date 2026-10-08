# Empty and degenerate inputs of the intake/preprocessing area:
# R/io_qualtrics.R (reading an export with no data row), R/ap1_exclusions.R
# (exclusions on zero rows, everyone excluded), R/ap3_preprocessing.R (reverse
# keying and scale scores on zero rows, gender with one level or none,
# standardisation with sd = 0 / all missing / a single value, a model variable
# without a standardisation entry),
# R/ap3_preparation.R (the covariate derivations and the quota completion on
# zero rows, and on an export with no quota cell at all),
# R/ap3_data_files.R (allowlist expansion with an empty codebook subset, intake
# approval mismatches), R/config.R (codebook subsets feeding the above).
#
# Every case must end in a clear stop() naming the function and the condition,
# or in a well-defined empty result — never a silent wrong number and never an
# obscure error from deep inside a package.

local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) {
    parent <- dirname(dir)
    if (identical(parent, dir)) stop("project root not found from ", getwd())
    dir <- parent
  }
  for (f in c("config.R", "io_qualtrics.R", "ap1_exclusions.R",
              "ap3_preprocessing.R", "ap3_preparation.R", "ap3_data_files.R", "ap3_pipeline.R")) {
    source(file.path(dir, "R", f), local = FALSE)
  }
  source(file.path(dir, "tests", "support", "targets-route.R"), local = FALSE)
})

root <- zm_root()
analysis_plan <- zm_config(profile = "full", path = file.path(root, "config", "analysis_plan.yaml"))
codebook <- zm_codebook(analysis_plan)
all_items <- unique(unlist(codebook$scales$item_codes))
sav_path <- file.path(root, "data", "synthetic", "zm_panel_synthetic.sav")

# Raw columns AP1 reads, n valid rows (all kept by every criterion).
ap1_fixture <- function(n) {
  tibble::tibble(
    ResponseId = sprintf("R_%03d", seq_len(n)),
    Status = rep("0", n),
    consent_check = rep(1, n),
    Finished = rep(1L, n),
    Progress = rep(100L, n),
    survey_status = rep("complete", n),
    demo_age = 20 + seq_len(n),
    attentioncheck_1 = rep(3, n),
    attentioncheck_2 = rep(5, n),
    demo_gender = rep_len(c(1, 2), n)
  )
}

# Demographic / scalometer columns AP3 reads.
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

# All 57 item columns, n rows, values cycling inside the response bounds
# (column j carries the six valid responses rotated by j).
items_fixture <- function(n) {
  vals <- c(1, 4, 3, 6, 2, 5)
  tibble::as_tibble(stats::setNames(
    lapply(seq_along(all_items), function(j) rep_len(vals[(((j - 1) + 0:5) %% 6) + 1], n)),
    all_items
  ))
}

# ---------------------------------------------------------------- io_qualtrics

test_that("an export without a data row is refused", {
  skip_if_not(file.exists(sav_path), "synthetic export not generated")
  raw <- read_qualtrics_export(sav_path)
  template <- file.path(root, "data", "synthetic", "template", "ZM-ASC-panel_random-data_2026-09-05.sav")
  empty <- withr::local_tempfile(fileext = ".sav")
  write_qualtrics_sav(raw[0, ], empty, template)
  expect_error(read_qualtrics_export(empty), "no data row", fixed = TRUE)
  expect_error(read_qualtrics_export(empty), "read_qualtrics_export", fixed = TRUE)
  # one data row is enough; the guard fires on emptiness only
  one <- withr::local_tempfile(fileext = ".sav")
  write_qualtrics_sav(raw[1, ], one, template)
  back <- read_qualtrics_export(one)
  expect_equal(dim(back), c(1L, ncol(raw)))
  expect_equal(back$ResponseId, raw$ResponseId[1])
})

# ------------------------------------------------------------- ap1_exclusions

test_that("AP1 on zero rows gives a well-defined empty result, not an error", {
  res <- apply_study_exclusions(ap1_fixture(0), analysis_plan)
  expect_equal(nrow(res$data), 0L)
  expect_true("respondent_id" %in% names(res$data))
  expect_equal(names(res$data)[1], "respondent_id")
  expect_equal(res$n_started, 0L)
  expect_equal(res$n_quota_full, 0L)
  log <- res$log
  expect_equal(nrow(log), 6L)
  expect_equal(log$step, 1:6)
  expect_equal(log$n_before, rep(0L, 6))
  expect_equal(log$n_excluded, rep(0L, 6))
  expect_equal(log$n_after, rep(0L, 6))
  expect_equal(log$n_divers[6], 0L)
  # An empty sample has no divers group, so there is nobody the rule could
  # exclude: the step reports the group as kept and changes nothing.
  expect_true(log$divers_kept[6])
  expect_match(log$note[5], "all 0 remaining rows")
  expect_match(log$note[6], "n_divers = 0")
  expect_match(log$note[6], "no respondents in this group")
  # the assertions accept an empty sample
  expect_silent(ap1_assert_no_test_rows(ap1_fixture(0)))
  expect_silent(check_only_complete_status_remains(ap1_fixture(0), analysis_plan$exclusions$survey_status_complete))
})

test_that("AP1 attributes every row to the first criterion when the whole export fails one", {
  fx <- ap1_fixture(7)
  fx$consent_check <- 2
  res <- apply_study_exclusions(fx, analysis_plan)
  expect_equal(nrow(res$data), 0L)
  expect_equal(res$log$n_excluded, c(7L, 0L, 0L, 0L, 0L, 0L))
  expect_equal(res$log$n_after, rep(0L, 6))
  expect_equal(res$log$n_divers[6], 0L)
})

test_that("AP1 keeps a single respondent and a single-respondent divers group is excluded", {
  fx <- ap1_fixture(1)
  res <- apply_study_exclusions(fx, analysis_plan)
  expect_equal(nrow(res$data), 1L)
  expect_equal(res$log$n_after[6], 1L)
  fx3 <- ap1_fixture(3)
  fx3$demo_gender <- c(1, 2, 3)
  res3 <- apply_study_exclusions(fx3, analysis_plan)
  # a divers group of one is far below the minimum, so it is counted and dropped
  expect_equal(res3$log$n_divers[6], 1L)
  expect_false(res3$log$divers_kept[6])
  expect_equal(res3$log$n_excluded[6], 1L)
  expect_equal(nrow(res3$data), 2L)
  expect_equal(sum(res3$data$demo_gender == 3), 0L)
  expect_match(res3$log$note[6], "n_divers = 1")
  expect_match(res3$log$note[6], "so they are excluded")
})

# ----------------------------------------------- ap3 items, scores, covariates

test_that("reverse keying and scale scores on zero rows give empty columns, not an error", {
  fx <- items_fixture(0)
  rev <- reverse_items(fx, codebook, analysis_plan)
  expect_equal(nrow(rev), 0L)
  expect_setequal(attr(rev, "reversed_items"), unique(unlist(codebook$scales$reverse_items)))
  sc <- average_items_into_subscales(rev, codebook)
  expect_equal(nrow(sc), 0L)
  expect_true(all(codebook$scales$scale_key %in% names(sc)))
  expect_true(all(vapply(sc[codebook$scales$scale_key], is.numeric, logical(1))))
})

test_that("an item column that is entirely NA makes its scale score entirely NA", {
  fx <- items_fixture(5)
  fx$UMS_int_1 <- NA_real_
  sc <- average_items_into_subscales(reverse_items(fx, codebook, analysis_plan), codebook)
  expect_true(all(is.na(sc$zm_security)))
  expect_false(any(is.na(sc$zm_achievement)))
})

test_that("a one-item scale scores as the item itself", {
  cb <- codebook
  cb$scales <- codebook$scales[codebook$scales$scale_key == "zm_security", , drop = FALSE]
  cb$scales$item_codes <- list(cb$scales$item_codes[[1]][1])
  cb$scales$reverse_items <- list(character(0))
  cb$scales$item_count <- 1L
  cb$items$reverse_keyed <- FALSE
  code <- cb$scales$item_codes[[1]]
  fx <- items_fixture(4)
  sc <- average_items_into_subscales(reverse_items(fx, cb, analysis_plan), cb)
  expect_equal(sc$zm_security, fx[[code]])
  expect_equal(nrow(average_items_into_subscales(reverse_items(items_fixture(0), cb, analysis_plan), cb)), 0L)
})

test_that("gender: one level, one respondent in a level, and no value at all", {
  gender_of <- function(codes) prepare_gender(data.frame(demo_gender = codes), analysis_plan)$gender
  # all female: a single-level factor, no error
  f1 <- gender_of(rep(2, 6))
  expect_s3_class(f1, "factor")
  expect_equal(levels(f1), "female")
  expect_equal(as.integer(table(f1)), 6L)
  # one respondent in the second level
  f2 <- gender_of(c(1, 1, 1, 1, 2))
  expect_equal(levels(f2), c("male", "female"))
  expect_equal(as.integer(table(f2)), c(4L, 1L))
  # one divers respondent among males and females: the levels keep code order;
  # the reference is chosen later, on the retained population
  f3 <- gender_of(c(2, 2, 1, 3))
  expect_equal(levels(f3), c("male", "female", "divers"))
  expect_equal(as.integer(table(f3)), c(1L, 2L, 1L))
  # no answered gender: no level, and every row marked as unobserved
  none <- prepare_gender(data.frame(demo_gender = rep(NA_real_, 4)), analysis_plan)
  expect_length(levels(none$gender), 0L)
  expect_false(any(none$known_gender))
  expect_length(gender_of(numeric(0)), 0L)
})

test_that("the covariate derivations accept zero rows", {
  expect_length(ap3_order_education_categories(numeric(0), codebook), 0L)
  # an empty study gives empty columns of the right type, not an error: the
  # region factor keeps both registered levels and the quota counts are zero
  cv0 <- derive_covariates(demo_fixture(0), analysis_plan, codebook)
  expect_equal(nrow(cv0), 0L)
  expect_length(cv0$income, 0L)
  expect_length(cv0$age_band, 0L)
  expect_length(cv0$income_band_value_log, 0L)
  expect_equal(levels(derive_east_west(demo_fixture(0), analysis_plan)$east_west), c("west", "east"))
  expect_equal(levels(cv0$quota_group), c("left_leaning", "conservative_leaning", "mixed"))
  expect_equal(attr(cv0, "quota_group_mismatch"), 0L)
  expect_equal(attr(cv0, "quota_group_computed"), 0L)
  # gender is coded before the derivations; a single-level gender goes through
  cv <- derive_covariates(
    prepare_gender(demo_fixture(4, gender = rep(2, 4)), analysis_plan), analysis_plan, codebook)
  expect_equal(levels(cv$gender), "female")
  expect_true(all(cv$known_gender))
  expect_equal(attr(cv, "quota_group_mismatch"), 0L)
  expect_equal(attr(cv, "quota_group_computed"), 0L)
})

test_that("an export with no quota cell at all is computed throughout, an unusable one stays missing", {
  # the extreme of the fieldwork blanks: the flow wrote no cell for any row
  d <- demo_fixture(6)
  d$quota_group <- ""
  cv <- derive_covariates(d, analysis_plan, codebook)
  expect_equal(attr(cv, "quota_group_computed"), 6L)
  expect_equal(attr(cv, "quota_group_mismatch"), 0L)
  expect_equal(unique(cv$quota_group_source), "computed")
  # rows 1..4 of the fixture: left liked only, conservative liked only, nothing, both
  expect_equal(as.character(cv$quota_group[1:4]),
               c("left_leaning", "conservative_leaning", "mixed", "mixed"))

  # every scalometer missing on a blank row: no cell can be decided, none is invented
  d_blank <- demo_fixture(6)
  d_blank$quota_group <- ""
  for (col in c("pol_symp_spd", "pol_symp_linke", "pol_symp_greens", "pol_symp_afd", "pol_symp_cdu_csu", "pol_symp_fdp")) d_blank[[col]] <- NA_real_
  cv_blank <- derive_covariates(d_blank, analysis_plan, codebook)
  expect_true(all(is.na(cv_blank$quota_group)))
  expect_equal(unique(cv_blank$quota_group_source), "computed")
  expect_equal(attr(cv_blank, "quota_group_computed"), 6L)
  expect_equal(attr(cv_blank, "quota_group_mismatch"), 0L)

  # a filled cell survives even when its own scalometers are gone: no override
  d_mix <- demo_fixture(6)
  for (col in c("pol_symp_spd", "pol_symp_linke", "pol_symp_greens", "pol_symp_afd", "pol_symp_cdu_csu", "pol_symp_fdp")) d_mix[[col]][1] <- NA_real_
  cv_mix <- derive_covariates(d_mix, analysis_plan, codebook)
  expect_equal(as.character(cv_mix$quota_group[1]), d_mix$quota_group[1])
  expect_equal(cv_mix$quota_group_source[1], "export")
  expect_equal(attr(cv_mix, "quota_group_mismatch"), 0L)
  expect_equal(attr(cv_mix, "quota_group_computed"), 0L)

  # a filled cell outside the registered set is refused, never silently turned into NA
  d_bad <- demo_fixture(6)
  d_bad$quota_group[2] <- "quota_left_leaning"
  expect_error(derive_covariates(d_bad, analysis_plan, codebook),
               "An exported quota group is not one of the three registered groups.", fixed = TRUE)
})

# ---------------------------------------------- standardisation specification

test_that("a model variable without a standardisation entry is named before any sample is built", {
  cfg_bad <- analysis_plan
  cfg_bad$network$nodes <- c(unlist(analysis_plan$network$nodes), "nope_z")
  expect_error(ap3_model_spec(cfg_bad, codebook), "network node(s) without a standardisation entry: nope_z", fixed = TRUE)
})

# -------------------------------------------------------- allowlists and files

test_that("an allowlist keyword over an empty codebook subset stops instead of writing no column", {
  cb <- codebook
  cb$scales <- codebook$scales[0, , drop = FALSE]
  expect_error(ap3_allowlist_expand("items", cb), "'items' expands to no column", fixed = TRUE)
  expect_error(ap3_allowlist_expand("scale_scores", cb), "'scale_scores' expands to no column", fixed = TRUE)
  expect_error(ap3_allowlist_expand(c("respondent_id", "items"), cb), "ap3_allowlist_expand", fixed = TRUE)
  d <- tibble::tibble(respondent_id = 1:3, demo_age = c(20, 30, 40))
  expect_error(ap3_allowlist_apply(d, "items", cb), "expands to no column", fixed = TRUE)
  expect_equal(ap3_allowlist_expand(character(0), codebook), character(0))
  # the valid expansion is unchanged
  expect_length(ap3_allowlist_expand("items", codebook), 57L)
  expect_equal(ap3_allowlist_expand("scale_scores", codebook), codebook$scales$scale_key)
  expect_length(ap3_allowlist_expand("pol_symp", codebook), 6L)
})

test_that("the two data files accept zero rows and keep their full column set", {
  skip_if_not(file.exists(sav_path), "synthetic export not generated")
  raw <- read_qualtrics_export(sav_path)
  built <- tr_route(root, tr_intake(raw, analysis_plan), analysis_plan, codebook)("study_analysis_ready")
  sci <- ap3_scientific_use_data(built[0, , drop = FALSE], analysis_plan, codebook)
  expect_equal(nrow(sci), 0L)
  expect_equal(names(sci), ap3_allowlist_expand(analysis_plan$data_files$scientific_use_file$keep, codebook))
  demographics <- build_deidentified_demographics(tibble::tibble(
    age = numeric(), income = numeric(), education = factor(),
    east_west = factor(), Duration__in_seconds_ = numeric()))
  expect_equal(nrow(demographics), 0L)
  expect_identical(names(demographics), c("age", "income", "education", "east_west", "duration"))
  for (data in list(sci, demographics)) {
    path <- withr::local_tempfile(fileext = ".csv")
    ap3_write_data_file(data, path)
    lines <- readLines(path)
    expect_equal(length(lines), 1L)
    expect_equal(lines[1], paste(names(data), collapse = ","))
  }
})

# ------------------------------------------------------------ intake approval

test_that("the intake approval must match the raw columns exactly", {
  raw <- ap1_fixture(3)
  dir <- withr::local_tempdir()
  write_approval <- function(proposal, file, data_source = "synthetic") {
    path <- file.path(dir, file)
    ap3_intake_write_proposal(proposal, analysis_plan, path = path, data_source = data_source)
    path
  }
  proposal <- ap3_intake_proposal(raw, analysis_plan)
  # the untouched proposal is a valid approval
  ok <- ap3_intake_check_approval(raw, analysis_plan, data_source = "synthetic",
                                  path = write_approval(proposal, "ok.yaml"))
  expect_equal(ok$column, names(raw))
  expect_true(all(ok$decision %in% c("keep", "delete")))

  # a column that the export does not have
  path_extra <- write_approval(proposal, "extra.yaml")
  y <- yaml::read_yaml(path_extra)
  y$columns <- c(y$columns, list(list(
    column = "some_extra_col", decision = "delete", reason = "keep", present = TRUE
  )))
  writeLines(yaml::as.yaml(y), path_extra)
  expect_error(
    ap3_intake_check_approval(raw, analysis_plan, data_source = "synthetic", path = path_extra),
    "Approved columns absent from the export: some_extra_col", fixed = TRUE
  )

  # a raw column without a decision
  path_missing <- write_approval(proposal[proposal$column != "demo_age", ], "missing.yaml")
  expect_error(
    ap3_intake_check_approval(raw, analysis_plan, data_source = "synthetic", path = path_missing),
    "Raw columns without a decision: demo_age", fixed = TRUE
  )

  # the same column twice
  path_dup <- write_approval(proposal[c(1, seq_len(nrow(proposal))), ], "dup.yaml")
  expect_error(
    ap3_intake_check_approval(raw, analysis_plan, data_source = "synthetic", path = path_dup),
    "lists column(s) more than once", fixed = TRUE
  )

  # a decision that is neither keep nor delete
  bad <- proposal
  bad$decision[bad$column == "demo_age"] <- "maybe"
  expect_error(
    ap3_intake_check_approval(raw, analysis_plan, data_source = "synthetic", path = write_approval(bad, "bad.yaml")),
    "decision must be keep or delete for demo_age", fixed = TRUE
  )

  # an approval written for the other export
  expect_error(
    ap3_intake_check_approval(raw, analysis_plan, data_source = "real",
                              path = write_approval(proposal, "src.yaml")),
    "approved for data_source 'synthetic' but this run uses 'real'", fixed = TRUE
  )

  # an approval file without any entry
  path_none <- file.path(dir, "none.yaml")
  writeLines(yaml::as.yaml(list(data_source = "synthetic", columns = list())), path_none)
  expect_error(ap3_intake_check_approval(raw, analysis_plan, data_source = "synthetic", path = path_none),
               "has no `columns` list", fixed = TRUE)

  # no approval at all
  expect_error(ap3_intake_check_approval(raw, analysis_plan, path = file.path(dir, "absent.yaml")),
               "intake approval missing", fixed = TRUE)
})

test_that("candidates that are absent from the export may stay listed and are ignored", {
  raw <- ap1_fixture(3)
  dir <- withr::local_tempdir()
  proposal <- ap3_intake_proposal(raw, analysis_plan)
  # the proposal already carries the configured candidates that this export lacks
  expect_true(any(!proposal$present))
  expect_true("IPAddress" %in% proposal$column[!proposal$present])
  path <- file.path(dir, "approved.yaml")
  ap3_intake_write_proposal(proposal, analysis_plan, path = path, data_source = "synthetic")
  approved <- ap3_intake_check_approval(raw, analysis_plan, data_source = "synthetic", path = path)
  expect_equal(nrow(approved), ncol(raw))
  expect_equal(approved$column, names(raw))
})

test_that("applying the intake deletion to zero rows gives an empty but well-formed table", {
  raw <- ap1_fixture(0)
  proposal <- ap3_intake_proposal(raw, analysis_plan)
  approved <- proposal[proposal$present, c("column", "decision")]
  out <- ap3_intake_apply(raw, approved, analysis_plan)
  expect_equal(nrow(out), 0L)
  expect_equal(names(out)[1], "respondent_id")
  expect_type(out$respondent_id, "integer")
  # AP1's columns are deferred, not deleted yet
  expect_setequal(attr(out, "intake_deferred"),
                  intersect(ap1_required_columns(), attr(out, "intake_deleted")))
  expect_true(all(attr(out, "intake_deferred") %in% names(out)))
  # and AP1 runs on it, giving the empty log
  res <- apply_study_exclusions(out, analysis_plan)
  expect_equal(nrow(res$data), 0L)
  expect_equal(res$log$n_after, rep(0L, 6))
})
