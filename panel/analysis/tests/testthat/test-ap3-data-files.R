# R/ap3_data_files.R: intake proposal / approval / deletion with
# the deferred AP1 columns, the scientific-use allowlist, the
# committed approval of the synthetic export

local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) {
    parent <- dirname(dir)
    if (identical(parent, dir)) stop("project root not found from ", getwd())
    dir <- parent
  }
  for (f in c("config.R", "io_qualtrics.R", "ap1_exclusions.R", "ap3_preprocessing.R", "ap3_fill.R",
              "ap3_pipeline.R", "ap3_data_files.R")) {
    source(file.path(dir, "R", f), local = FALSE)
  }
  source(file.path(dir, "tests", "support", "targets-route.R"), local = FALSE)
})

root <- zm_root()
analysis_plan <- zm_config(profile = "full", path = file.path(root, "config", "analysis_plan.yaml"))
codebook <- zm_codebook(analysis_plan)
raw <- read_qualtrics_export(file.path(root, "data", "synthetic", "zm_panel_synthetic.sav"))
lists <- ap3_intake_lists(analysis_plan)
all_items <- unique(unlist(codebook$scales$item_codes))

# A configuration whose intake files live in a temporary project root.
cfg_tmp <- function() {
  c2 <- analysis_plan
  c2$root <- withr::local_tempdir(.local_envir = parent.frame())
  c2
}
write_approval <- function(rows, path, data_source = "synthetic") {
  body <- list(data_source = data_source, columns = rows)
  if (is.null(data_source)) body$data_source <- NULL
  writeLines(yaml::as.yaml(body), path)
  path
}
approval_rows <- function(proposal) {
  lapply(seq_len(nrow(proposal)), function(i) {
    list(column = proposal$column[i], decision = proposal$decision[i], reason = proposal$reason[i],
         present = isTRUE(proposal$present[i]))
  })
}

# ---- intake proposal ----------------------------------------------------------

test_that("the intake proposal lists every raw column once with its list as reason and flags unknown columns", {
  prop <- ap3_intake_proposal(raw, analysis_plan)
  expect_equal(names(prop), c("column", "reason", "decision", "present", "in_schema", "note"))
  expect_true(all(names(raw) %in% prop$column))
  expect_equal(anyDuplicated(prop$column), 0)
  expect_equal(prop$column[prop$present][seq_along(names(raw))], names(raw))
  expect_true(all(prop$reason %in% c("delete_always", "delete_if_present", "delete_after_billing", "keep")))
  expect_equal(prop$decision, ifelse(prop$reason == "keep", "keep", "delete"))
  present <- prop[prop$present, ]
  expect_equal(nrow(present), ncol(raw))
  expect_true(all(present$in_schema))
  expect_setequal(present$column[present$reason == "delete_always"], lists$delete_always)
  expect_equal(present$column[present$reason == "delete_after_billing"], "bilendi_id")
  expect_equal(sum(present$reason == "delete_if_present"), 0)
  expect_true(all(c(all_items, "Kommentarfeld", "demo_age", "consent_check") %in% present$column[present$reason == "keep"]))
  # candidates absent from this export are listed as not present
  absent <- prop[!prop$present, ]
  expect_setequal(absent$column, lists$delete_if_present)
  expect_true(all(absent$reason == "delete_if_present"))
  expect_true(all(grepl("absent from this export", absent$note)))
  # an IP column present in the export is proposed for deletion; an unknown column too, with a note
  raw2 <- raw
  raw2$IPAddress <- "127.0.0.1"
  raw2$panel_extra <- 1
  prop2 <- ap3_intake_proposal(raw2, analysis_plan)
  ip <- prop2[prop2$column == "IPAddress", ]
  expect_true(ip$present)
  expect_equal(ip$reason, "delete_if_present")
  expect_equal(ip$decision, "delete")
  extra <- prop2[prop2$column == "panel_extra", ]
  expect_equal(extra$reason, "delete_if_present")
  expect_equal(extra$decision, "delete")
  expect_false(extra$in_schema)
  expect_match(extra$note, "not in the export schema")
  expect_true(is.na(ip$note))
})

test_that("the proposal is written as YAML with a comment header and reads back", {
  c2 <- cfg_tmp()
  prop <- ap3_intake_proposal(raw, c2)
  path <- ap3_intake_write_proposal(prop, c2, data_source = "synthetic")
  expect_equal(path, file.path(c2$root, c2$data_files$intake$proposal_file))
  expect_true(file.exists(path))
  lines <- readLines(path)
  expect_true(startsWith(lines[1], "#"))
  expect_true(any(grepl(c2$data_files$intake$approval_file, lines, fixed = TRUE)))
  back <- ap3_intake_read_approval(path)
  expect_equal(back$column, prop$column)
  expect_equal(back$decision, prop$decision)
  expect_equal(back$reason, prop$reason)
  expect_equal(back$present, prop$present)
  expect_equal(attr(back, "data_source"), "synthetic")
  y <- yaml::read_yaml(path)
  expect_equal(y$n_raw_columns, ncol(raw))
})

# ---- intake approval ----------------------------------------------------------

test_that("the approval check stops without the file and accepts a complete approval", {
  c2 <- cfg_tmp()
  expect_error(ap3_intake_check_approval(raw, c2), "intake approval missing: copy intake/intake_proposal.yaml to intake/intake_approved.yaml, edit the decisions, rerun")
  prop <- ap3_intake_proposal(raw, c2)
  path <- file.path(c2$root, c2$data_files$intake$approval_file)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write_approval(approval_rows(prop), path)
  appr <- ap3_intake_check_approval(raw, c2, data_source = "synthetic")
  expect_equal(names(appr), c("column", "decision", "reason", "note"))
  expect_equal(appr$column, names(raw))
  expect_true(all(appr$decision %in% c("keep", "delete")))
  expect_equal(sum(appr$decision == "delete"), length(lists$delete_always) + 1)
  expect_equal(attr(appr, "data_source"), "synthetic")
  # a manual approval that lists only the raw columns (no absent candidates) also passes
  write_approval(approval_rows(prop[prop$present, ]), path)
  expect_equal(nrow(ap3_intake_check_approval(raw, c2)), ncol(raw))
  # the approval of one export is refused for another data source
  expect_error(ap3_intake_check_approval(raw, c2, data_source = "real"), "data_source 'synthetic'")
  # an approval without data_source is accepted for any source
  write_approval(approval_rows(prop), path, data_source = NULL)
  expect_equal(nrow(ap3_intake_check_approval(raw, c2, data_source = "real")), ncol(raw))
  c3 <- c2
  c3$data_files$intake$require_manual_approval <- FALSE
  expect_error(ap3_intake_check_approval(raw, c3), "require_manual_approval")
})

test_that("the approval check refuses incomplete, duplicated or malformed decisions", {
  c2 <- cfg_tmp()
  prop <- ap3_intake_proposal(raw, c2)
  rows <- approval_rows(prop[prop$present, ])
  path <- file.path(c2$root, c2$data_files$intake$approval_file)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  # a raw column without a decision
  write_approval(rows[-5], path)
  expect_error(ap3_intake_check_approval(raw, c2), names(raw)[5])
  # a column listed twice
  write_approval(c(rows, rows[3]), path)
  expect_error(ap3_intake_check_approval(raw, c2), "more than once")
  # an unknown decision
  bad <- rows
  bad[[10]]$decision <- "maybe"
  write_approval(bad, path)
  expect_error(ap3_intake_check_approval(raw, c2), "keep or delete")
  # an approved column that the export does not have
  extra <- c(rows, list(list(column = "ghost", decision = "delete", reason = "delete_if_present", present = TRUE)))
  write_approval(extra, path)
  expect_error(ap3_intake_check_approval(raw, c2), "ghost")
  # a column set from a different export
  write_approval(rows[1:50], path)
  expect_error(ap3_intake_check_approval(raw, c2), "does not match the raw columns")
  writeLines("data_source: synthetic\n", path)
  expect_error(ap3_intake_check_approval(raw, c2), "columns")
})

test_that("the committed approval of the synthetic export matches its raw columns and deletes bilendi_id", {
  appr <- ap3_intake_check_approval(raw, analysis_plan, data_source = "synthetic")
  expect_equal(appr$column, names(raw))
  expect_equal(appr$decision[appr$column == "bilendi_id"], "delete")
  expect_true(all(appr$decision[appr$column %in% lists$delete_always] == "delete"))
  expect_true(all(appr$decision[appr$column %in% c(all_items, "Kommentarfeld", "demo_age")] == "keep"))
  expect_error(ap3_intake_check_approval(raw, analysis_plan, data_source = "real"), "data_source")
  lines <- readLines(file.path(root, analysis_plan$data_files$intake$approval_file))
  expect_true(any(grepl("synthetic", lines[startsWith(lines, "#")], ignore.case = TRUE)))
})

# ---- intake deletion ----------------------------------------------------------

test_that("ap3_intake_apply() removes the approved columns, defers the AP1 columns and numbers the respondents", {
  appr <- ap3_intake_check_approval(raw, analysis_plan, data_source = "synthetic")
  ri <- ap3_intake_apply(raw, appr, analysis_plan)
  deleted <- appr$column[appr$decision == "delete"]
  deferred <- intersect(deleted, ap1_required_columns())
  expect_setequal(attr(ri, "intake_deleted"), deleted)
  expect_setequal(attr(ri, "intake_deferred"), deferred)
  expect_setequal(deferred, c("ResponseId", "Status", "Finished", "Progress", "survey_status"))
  expect_equal(names(ri)[1], "respondent_id")
  expect_setequal(ri$respondent_id, seq_len(nrow(raw)))
  expect_equal(nrow(ri), nrow(raw))
  expect_equal(ncol(ri), ncol(raw) - length(deleted) + length(deferred) + 1)
  expect_false("bilendi_id" %in% names(ri))
  expect_false("StartDate" %in% names(ri))
  expect_true(all(deferred %in% names(ri)))
  expect_true(all(c(all_items, "Kommentarfeld", "demo_age", "consent_check") %in% names(ri)))
  expect_equal(names(attr(ri, "labels")), names(ri)[-1])
  # AP1 runs on the intake output, keeps the running number and removes the deferred columns
  res <- apply_study_exclusions(ri, analysis_plan)
  # 700 = 776 - 26 quota-full exits - 44 exclusions - the divers group of six,
  # which is below analysis_plan$exclusions$gender_divers_min_n and therefore
  # leaves at step 6
  expect_equal(nrow(res$data), 700L)
  expect_equal(res$log$n_excluded, c(6L, 14L, 4L, 12L, 8L, 6L))
  expect_false(any(deleted %in% names(res$data)))
  expect_equal(names(res$data)[1], "respondent_id")
  expect_true(is.integer(res$data$respondent_id))
  expect_true(all(res$data$respondent_id %in% seq_len(nrow(raw))))
  expect_equal(anyDuplicated(res$data$respondent_id), 0)
  # the rows kept are the same rows as without the intake
  ref <- apply_study_exclusions(raw, analysis_plan)$data
  expect_equal(res$data$respondent_id, ri$respondent_id[match(ref$respondent_id, raw$ResponseId)])
  # the whole preprocessing runs on the intake output and no deleted column reaches the analysis data
  attr(ri, "intake_verified") <- TRUE
  built <- tr_route(root, ri, analysis_plan, codebook)("study_analysis_ready")
  expect_false(any(deleted %in% names(built)))
  expect_equal(nrow(built), 700L)
})

test_that("bilendi_id stays only when the approval says keep; an export with respondent_id is refused", {
  appr <- ap3_intake_check_approval(raw, analysis_plan, data_source = "synthetic")
  appr$decision[appr$column == "bilendi_id"] <- "keep"
  ri <- ap3_intake_apply(raw, appr, analysis_plan)
  expect_true("bilendi_id" %in% names(ri))
  expect_equal(ri$bilendi_id, raw$bilendi_id)
  raw2 <- tibble::add_column(raw, respondent_id = 1)
  appr2 <- rbind(appr, tibble::tibble(column = "respondent_id", decision = "keep", reason = "keep", note = NA_character_))
  expect_error(ap3_intake_apply(raw2, appr2, analysis_plan), "respondent_id")
})

test_that("the filled export retains eligible respondents and records their filled gaps", {
  intake <- tr_intake(raw, analysis_plan)
  eligible <- apply_study_exclusions(intake, analysis_plan)$data
  demographics <- c("demo_age", "demo_hh_members", "demo_income_hh_net")
  ids <- head(eligible$respondent_id[stats::complete.cases(eligible[c(all_items, demographics)])], 3L)
  rows <- match(ids, intake$respondent_id); items <- codebook$scales$item_codes[[1L]]
  intake[rows[1L], items[1:3]] <- NA_real_
  intake[rows[2L], c("demo_hh_members", "demo_income_hh_net")] <- NA_real_
  intake[rows[3L], c(items[1L], "demo_income_hh_net")] <- NA_real_
  read <- tr_route(root, intake, analysis_plan, codebook)
  retained <- read("study_analysis_ready")$respondent_id
  expect_false(any(ids[1:2] %in% retained)); expect_true(ids[3L] %in% retained)
  exported <- readr::read_csv(read("scientific_use_file"), show_col_types = FALSE)
  expect_equal(exported$respondent_id, retained)
  row <- match(ids[3L], exported$respondent_id)
  expect_false(is.na(exported[[items[1L]]][row]))
  expect_false(is.na(exported$income_band_value[row]))
  cells <- read("imputation_reporting_data")$imputed_cells
  expect_true(all(c(items[1L], "demo_income_hh_net") %in% cells$variable[cells$respondent_id == ids[3L]]))
})
# ---- allowlists -----------------------------------------------------------------

# The analysis table the pipeline builds before the fill: the shared file is
# written from it, and the internal file carries the same allowlist.
# AP1 keeps a divers group of at least analysis_plan$exclusions$gender_divers_min_n; the
# export's own group of six is smaller (that rule is tested in
# test-ap1-exclusions.R). The export the analysis table is built from here
# carries a group at the minimum, so both files hold divers respondents.
gender_codes <- c(male = 1, female = 2, divers = 3)  # codebook value set gender_1_3
divers_code <- gender_codes[["divers"]]
raw_divers <- raw
raw_divers$demo_gender[!is.na(raw_divers$demo_gender) &
                         raw_divers$demo_gender == divers_code] <- gender_codes[["female"]]
promotable <- which(raw_divers$survey_status == analysis_plan$exclusions$survey_status_complete &
                      raw_divers$demo_age >= analysis_plan$exclusions$min_age &
                      raw_divers$demo_age <= analysis_plan$exclusions$max_age &
                      raw_divers$demo_gender %in% unname(gender_codes[c("male", "female")]))
raw_divers$demo_gender[promotable[seq_len(analysis_plan$exclusions$gender_divers_min_n)]] <- divers_code
analysis <- tr_route(root, tr_intake(raw_divers, analysis_plan), analysis_plan, codebook)("study_analysis_ready")
n_divers_analysis <- sum(analysis$demo_gender %in% divers_code)
stopifnot(n_divers_analysis == analysis_plan$exclusions$gender_divers_min_n)

test_that("allowlist tokens expand to the codebook items, the nine scale keys and the six scalometers", {
  expect_length(ap3_allowlist_expand("items", codebook), 57)
  expect_setequal(ap3_allowlist_expand("items", codebook), all_items)
  expect_equal(ap3_allowlist_expand("scale_scores", codebook), codebook$scales$scale_key)
  expect_length(ap3_allowlist_expand("pol_symp", codebook), 6)
  expect_setequal(ap3_allowlist_expand("pol_symp", codebook), c(
    "pol_symp_spd", "pol_symp_cdu_csu", "pol_symp_greens", "pol_symp_fdp", "pol_symp_afd", "pol_symp_linke"
  ))
  expect_equal(ap3_allowlist_expand(c("respondent_id", "demo_age"), codebook), c("respondent_id", "demo_age"))
  expanded <- ap3_allowlist_expand(analysis_plan$data_files$scientific_use_file$keep, codebook)
  expect_equal(anyDuplicated(expanded), 0)
  expect_equal(length(expanded), 57 + 9 + 6 + length(analysis_plan$data_files$scientific_use_file$keep) - 3)
})

test_that("the observed side file preserves only the five independent margins", {
  observed <- tibble::tibble(age = c(22, NA, 44), income = c(500, 1000, NA),
    education = factor(c("A", "B", "A")), east_west = factor(c("east", "west", NA)),
    Duration__in_seconds_ = c(100, 200, 300))
  side <- build_deidentified_demographics(observed)
  expect_identical(names(side), c("age", "income", "education", "east_west", "duration"))
  for (i in seq_along(side)) expect_identical(sort(side[[i]], na.last = TRUE), sort(observed[[i]], na.last = TRUE))
})
test_that("the scientific-use file holds exactly its allowlist for every respondent of the analysis table", {
  sci <- ap3_scientific_use_data(analysis, analysis_plan, codebook)
  expected <- ap3_allowlist_expand(analysis_plan$data_files$scientific_use_file$keep, codebook)
  expect_equal(names(sci), expected)
  expect_length(setdiff(names(sci), expected), 0)
  expect_equal(nrow(sci), nrow(analysis))
  expect_equal(sum(sci$demo_gender %in% divers_code), n_divers_analysis)
  expect_false("Kommentarfeld" %in% names(sci))
  drop <- unlist(analysis_plan$data_files$scientific_use_file$drop)
  expect_false(any(drop %in% names(sci)))
  expect_false(any(c("income_z", "bilendi_id", "ResponseId") %in% names(sci)))
  # The demographics are gender and the two analysis bands, each band as its
  # label and as the value the analyses use.
  expect_true(all(c("demo_gender", "age_band", "age_band_midpoint", "income_band",
                    "income_band_value") %in% names(sci)))
  expect_identical(sci$age_band_midpoint, analysis$age_band_midpoint)
  expect_identical(sci$income_band_value, analysis$income_band_value)
  expect_true(any(!is.na(sci$age_band)) && any(!is.na(sci$income_band)))
  # every shared column equals the analysis data
  for (nm in names(sci)) expect_identical(sci[[nm]], analysis[[nm]], info = nm)
  expect_equal(sci$respondent_id, analysis$respondent_id)
  cfg_values <- analysis_plan
  cfg_values$data_files$scientific_use_file$analysis_value_columns <- "income"
  expect_error(ap3_scientific_use_data(analysis, cfg_values, codebook), "analysis_value_columns")
  cfg_clash <- analysis_plan
  cfg_clash$data_files$scientific_use_file$keep <- c(analysis_plan$data_files$scientific_use_file$keep, "Kommentarfeld")
  expect_error(ap3_scientific_use_data(analysis, cfg_clash, codebook), "keep and drop")
})

test_that("the filled scientific-use writer preserves its schema and values", {
  dir <- withr::local_tempdir()
  path <- write_scientific_use_file(analysis, analysis_plan, codebook, path = file.path(dir, "scientific_use.csv"))
  sci <- readr::read_csv(path, show_col_types = FALSE)
  expect_identical(names(sci), names(ap3_scientific_use_data(analysis, analysis_plan, codebook)))
  expect_equal(nrow(sci), nrow(analysis))
  expect_equal(sci$age_band_midpoint, analysis$age_band_midpoint)
  expect_equal(sci$income_band, as.character(analysis$income_band))
  expect_identical(formals(write_scientific_use_file)$path, quote(ap3_data_file_path(analysis_plan, "scientific_use_file")))
})
# ---- the files of the AP3 fill --------------------------------------------------

# The filled scientific-use file retains the same item and band values used
# for analysis. Here one item and one demographic fill are planted by hand.
fill_frames <- local({
  unfilled <- analysis
  for (column in c("demo_age", "demo_hh_members", "demo_income_hh_net", "age", "income"))
    unfilled[[column]] <- rep(NA_real_, nrow(unfilled))
  item <- all_items[1]
  gap_row <- which(!is.na(unfilled[[item]]))[1]
  unfilled[[item]][gap_row] <- NA_real_
  unfilled$demo_age[gap_row] <- NA_real_
  unfilled$demo_hh_members[gap_row] <- NA_real_
  unfilled$demo_income_hh_net[gap_row] <- NA_real_
  filled <- unfilled
  filled[[item]][gap_row] <- 3.4                 # fractional, as an item fill may be
  filled$demo_age[gap_row] <- 41
  filled$demo_hh_members[gap_row] <- 2
  filled$demo_income_hh_net[gap_row] <- 7
  # the analysis bands follow the exact values, as derive_covariates() forms them
  unfilled$age[gap_row] <- NA_real_
  unfilled$income[gap_row] <- NA_real_
  unfilled <- add_age_and_income_bands(unfilled, analysis_plan)
  filled$age[gap_row] <- 41
  filled$income[gap_row] <- analysis_plan$income$band_representative[["7"]] / 2
  filled <- add_age_and_income_bands(filled, analysis_plan)
  list(filled = filled, unfilled = unfilled, item = item, row = gap_row,
       id = unfilled$respondent_id[gap_row])
})
fill_cells <- tibble::tibble(
  respondent_id = rep(fill_frames$id, 2L),
  variable = c(fill_frames$item, "demo_age"),
  kind = c("item", "demographic"),
  model = c("zm_achievement", "demo_age"),
  value = c(3.4, 41), lower = c(2, 24), upper = c(5, 63),
  n_fit_rows = c(400L, 300L)
)
fill_dropped <- tibble::tibble(respondent_id = c(7L, 9L),
                               reason = c("more than 2 missing items in asc_agg",
                                          "2 of 3 demographics missing"))

test_that("every data file of a run is written under data/derived at its configured path", {
  c2 <- analysis_plan
  c2$root <- withr::local_tempdir()
  written <- c(
    scientific_use_file = write_scientific_use_file(fill_frames$filled, c2, codebook),
    filled_cells_file = write_filled_cells_file(fill_cells, c2),
    dropped_respondents_file = write_dropped_respondents_file(fill_dropped, c2)
  )
  for (entry in names(written)) {
    expect_equal(written[[entry]], ap3_data_file_path(c2, entry), info = entry)
    expect_true(file.exists(written[[entry]]), info = entry)
    expect_equal(dirname(written[[entry]]), file.path(c2$root, "data", "derived"), info = entry)
    expect_true(nzchar(analysis_plan$data_files[[entry]]$path), info = entry)
  }
  expect_equal(basename(unname(written)),
               c("scientific_use.csv",
                 "filled_cells.csv", "dropped_respondents.csv"))
  # a file whose path the plan does not name is not written to a guessed one
  c3 <- c2
  c3$data_files$filled_cells_file$path <- NULL
  expect_error(write_filled_cells_file(fill_cells, c3), "filled_cells_file\\$path")
})

test_that("the scientific-use writer exports the filled item and its derived bands", {
  c2 <- analysis_plan; c2$root <- withr::local_tempdir()
  shared <- readr::read_csv(write_scientific_use_file(fill_frames$filled, c2, codebook), show_col_types = FALSE)
  row <- match(fill_frames$id, shared$respondent_id)
  expect_equal(shared[[fill_frames$item]][row], 3.4)
  expect_equal(shared$age_band_midpoint[row], 42)
  expect_false(is.na(shared$income_band_value[row]))
  expect_false(any(c("demo_age", "demo_hh_members", "demo_income_hh_net") %in% names(shared)))
})
test_that("the two fill records are written with the columns their tables carry", {
  c2 <- analysis_plan
  c2$root <- withr::local_tempdir()
  cells <- readr::read_csv(write_filled_cells_file(fill_cells, c2), show_col_types = FALSE, progress = FALSE)
  expect_equal(names(cells), names(ap3_fill_empty_cells()))
  expect_equal(names(cells), c("respondent_id", "variable", "kind", "model", "value",
                               "lower", "upper", "n_fit_rows"))
  expect_equal(nrow(cells), nrow(fill_cells))
  expect_equal(cells$variable, fill_cells$variable)
  expect_equal(cells$value, fill_cells$value)
  expect_setequal(cells$kind, c("item", "demographic"))
  dropped <- readr::read_csv(write_dropped_respondents_file(fill_dropped, c2), show_col_types = FALSE, progress = FALSE)
  expect_equal(names(dropped), c("respondent_id", "reason"))
  expect_equal(dropped$respondent_id, fill_dropped$respondent_id)
  expect_equal(dropped$reason, fill_dropped$reason)
  # a run that imputed nothing and excluded nobody still writes both files, with
  # their header and no row: the writers read the projections the pipeline hands
  # them, not the records the imputation attached
  nothing_imputed <- build_imputation_reporting_data(
    structure(analysis, imputed_cells = ap3_fill_empty_cells()),
    tibble::tibble(respondent_id = integer(0), too_many_items_missing = logical(0),
                   scales_over_threshold = character(0),
                   too_many_demographics_missing = logical(0),
                   n_demographics_missing = integer(0))
  )
  empty_cells <- readr::read_csv(
    write_filled_cells_file(build_filled_cells_table(nothing_imputed), c2),
    show_col_types = FALSE, progress = FALSE)
  expect_equal(names(empty_cells), names(ap3_fill_empty_cells()))
  expect_equal(nrow(empty_cells), 0L)
  empty_dropped <- readr::read_csv(
    write_dropped_respondents_file(build_dropped_respondents_table(nothing_imputed, c2), c2),
    show_col_types = FALSE, progress = FALSE)
  expect_equal(nrow(empty_dropped), 0L)
})
