# Start with a temporary Qualtrics export and no prepared files. Exercise the
# production preparation command, then rebuild production targets in fresh R
# processes before and after simulating MZ's deletion of the temporary export.
# Complete synthetic cases keep this boundary test fast; fill-model behaviour
# and planted gaps are covered by the AP3 tests.

make_first_run_fixture <- function(source_root, destination) {
  analysis <- file.path(destination, "panel", "analysis")
  dir.create(file.path(analysis, "data", "synthetic"), recursive = TRUE)
  for (name in c("R", "config", "report", "scripts", "_targets.R", "renv.lock")) {
    if (!file.copy(file.path(source_root, name), analysis, recursive = TRUE))
      stop("Could not copy first-run test source: ", name)
  }
  prereg <- file.path(destination, "panel", "preregistration")
  dir.create(prereg, recursive = TRUE)
  inputs <- c(Sys.glob(file.path(source_root, "../preregistration/codebook_*.csv")),
              file.path(source_root, "../preregistration/preregistration.qmd"))
  if (!all(file.copy(inputs, prereg))) stop("Could not copy the codebook/preregistration inputs.")
  analysis
}

run_first_run_command <- function(analysis, command, args, log_name) {
  log <- file.path(analysis, log_name)
  status <- withr::with_dir(analysis, system2(command, args, stdout = log, stderr = log))
  if (!identical(status, 0L)) {
    stop(paste(c(paste("First-run test command failed:", command, paste(args, collapse = " ")),
                 tail(readLines(log, warn = FALSE), 35L)), collapse = "\n"), call. = FALSE)
  }
  invisible(log)
}

read_first_run_targets <- function(analysis, store, output) {
  # This is a separate process for EACH run. No object from preparation or the
  # first run is available to the restarted pipeline, even in memory.
  script <- file.path(analysis, "read-first-run-targets.R")
  writeLines(c(
    "library(targets)",
    paste0("store <- ", deparse(store)),
    'wanted <- c("data_regressions", "data_network", "scale_score_distributions",',
    '            "sample_composition", "report_participant_flow",',
    '            "report_sample_characteristics", "report_political_sample",',
    '            "data_cfa_input", "imputation_reporting_data", "preparation_reporting_data")',
    'tar_make(names = tidyselect::all_of(wanted), store = store,',
    '         callr_function = NULL, reporter = "silent", use_crew = FALSE)',
    'meta <- tar_meta(store = store, fields = "error")',
    'if (any(!is.na(meta$error))) stop(paste(meta$error[!is.na(meta$error)], collapse = "\\n"))',
    'values <- setNames(lapply(wanted, tar_read_raw, store = store), wanted)',
    paste0("saveRDS(values, ", deparse(output), ")")
  ), script)
  run_first_run_command(analysis, file.path(R.home("bin"), "Rscript"),
                        shQuote(script), paste0(basename(store), ".log"))
  readRDS(output)
}

test_that("first intake and a fresh restart need only the surviving prepared inputs", {
  skip_if_not_installed("crew")
  skip_if_not_installed("targets")
  source_root <- normalizePath(if (nzchar(Sys.getenv("ZM_PROJECT_ROOT")))
    Sys.getenv("ZM_PROJECT_ROOT") else getwd())
  while (!file.exists(file.path(source_root, "config", "analysis_plan.yaml"))) {
    parent <- dirname(source_root)
    if (identical(parent, source_root)) stop("Analysis root not found.")
    source_root <- parent
  }
  functions <- new.env(parent = globalenv())
  for (file in list.files(file.path(source_root, "R"), "[.]R$", full.names = TRUE))
    sys.source(file, envir = functions)
  cfg <- functions$zm_config(profile = "smoke", path = file.path(source_root, "config", "analysis_plan.yaml"))
  book <- functions$zm_codebook(cfg)
  raw <- functions$read_qualtrics_export(file.path(source_root, "data/synthetic/zm_panel_synthetic.sav"))
  required <- c(unique(unlist(book$scales$item_codes)), "demo_age", "demo_hh_members", "demo_income_hh_net",
                "demo_edu_school", "demo_bundesland", "Duration__in_seconds_")
  eligible <- raw$survey_status == "complete" & !is.na(raw$demo_gender) & raw$demo_gender != 3 &
    stats::complete.cases(raw[required]) & raw$demo_age >= 18 & raw$demo_age <= 69 &
    raw$attentioncheck_1 == cfg$exclusions$attention_check_1_correct &
    raw$attentioncheck_2 == cfg$exclusions$attention_check_2_correct
  raw <- raw[which(eligible)[seq_len(24L)], ]
  expect_equal(nrow(raw), 24L)
  # One known exclusion proves the observed/scientific outputs use the retained
  # population, rather than simply serialising all imported rows.
  raw$attentioncheck_1[24L] <- 999
  raw$Kommentarfeld <- ""
  raw$Kommentarfeld[1:2] <- c("Synthetic retained comment one", "Synthetic retained comment two")
  # These observed columns are never filled. Plant missing entries to check
  # their preservation independently of the fully observed model inputs.
  raw$demo_edu_school[1] <- NA_real_
  raw$demo_bundesland[2] <- NA_real_
  raw$Duration__in_seconds_[3] <- NA_real_
  retained <- raw[1:23, ]

  directory <- withr::local_tempdir()
  analysis <- make_first_run_fixture(source_root, directory)
  input <- file.path(analysis, "data/synthetic/first_run.sav")
  functions$write_qualtrics_sav(raw, input,
    file.path(source_root, "data/synthetic/template/ZM-ASC-panel_random-data_2026-09-05.sav"))
  expect_identical(list.files(file.path(analysis, "data"), recursive = TRUE), "synthetic/first_run.sav")
  expect_false(dir.exists(file.path(analysis, "_targets")))

  withr::local_envvar(c(ZM_PROJECT_ROOT = analysis, ZM_PROFILE = "smoke", ZM_DATA = "synthetic"))
  cfg <- functions$zm_config(profile = "smoke", path = file.path(analysis, "config", "analysis_plan.yaml"))
  approval <- file.path(analysis, "approved.yaml")
  functions$ap3_intake_write_proposal(functions$ap3_intake_proposal(raw, cfg), cfg,
                                     approval, data_source = "synthetic")
  rscript <- file.path(R.home("bin"), "Rscript")
  run_first_run_command(analysis, rscript,
    c("scripts/prepare_intake.R", "--source=synthetic", paste0("--input=", shQuote(input)),
      paste0("--approval=", shQuote(approval))), "prepare.log")
  paths <- functions$zm_clean_intake_paths("synthetic", cfg)
  expect_true(all(file.exists(paths)))
  expect_true(file.exists(input))
  scientific <- readr::read_csv(paths[["data"]], show_col_types = FALSE)
  demographics <- readr::read_csv(paths[["demographics"]], show_col_types = FALSE)
  expect_equal(nrow(scientific), 23L)
  expect_equal(nrow(demographics), 23L)
  expect_identical(names(demographics), c("age", "income", "education", "east_west", "duration"))
  forbidden <- c("demo_age", "demo_income_hh_net", "demo_hh_members", "demo_bundesland",
                 "demo_edu_school", "east_west", "income", "Duration__in_seconds_", "Kommentarfeld")
  expect_length(intersect(names(scientific), forbidden), 0L)

  receipt <- yaml::read_yaml(paths[["receipt"]])
  comments_path <- receipt$comments_file
  comments <- readr::read_csv(comments_path, show_col_types = FALSE)
  expect_identical(comments$comment, retained$Kommentarfeld[1:2])
  expect_equal(comments$respondent_id, scientific$respondent_id[1:2])
  comment_hash <- functions$zm_intake_hash(comments_path)

  # Expected values come from the retained source responses, rather than from
  # another intake output. A consistently wrong export cannot pass twice.
  income <- c(250, 625, 875, 1125, 1375, 1750, 2250, 2750, 3500, 4500, 6250, 8750, 12500)[
    retained$demo_income_hh_net] / pmin(retained$demo_hh_members, 8)
  categories <- book$factors[book$factors$set_id == "school_education_1_9", ]
  observed <- list(age = as.numeric(retained$demo_age), income = income,
    education = categories$label_de[match(retained$demo_edu_school, categories$value_corr)],
    east_west = ifelse(is.na(retained$demo_bundesland), NA_character_,
      ifelse(retained$demo_bundesland %in% 11:16, "east", "west")),
    duration = as.numeric(retained$Duration__in_seconds_))
  for (name in names(observed))
    expect_equal(sort(demographics[[name]], na.last = TRUE), sort(observed[[name]], na.last = TRUE))
  item_columns <- unique(unlist(book$scales$item_codes))
  scored <- as.data.frame(lapply(retained[item_columns], as.numeric))
  reversed <- book$items$item_code[book$items$reverse_keyed]
  for (item in intersect(reversed, item_columns)) scored[[item]] <- 7 - scored[[item]]
  expect_equal(lapply(scientific[item_columns], as.numeric), as.list(scored))
  for (i in seq_len(nrow(book$scales))) {
    key <- book$scales$scale_key[i]
    expect_equal(scientific[[key]], rowMeans(scored[book$scales$item_codes[[i]]]))
  }
  expect_equal(scientific$demo_gender, as.numeric(retained$demo_gender))
  politics <- c(grep("^pol_symp_", names(raw), value = TRUE), "pol_party_vote")
  for (name in politics) expect_equal(scientific[[name]], as.numeric(retained[[name]]))
  expect_equal(scientific$age_band_midpoint,
    c(21, 27, 32, 37, 42, 47, 52, 57, 62, 67)[findInterval(retained$demo_age,
      c(18, 25, 30, 35, 40, 45, 50, 55, 60, 65))])
  income_limits <- c(357, 500, 630, 800, 1050, 1400, 2000, 2800, 12500)
  expect_equal(scientific$income_band_value,
    sqrt(head(income_limits, -1) * tail(income_limits, -1))[
      findInterval(income, income_limits[2:8]) + 1L])

  # Removed deletion flags must fail without touching the original or comments.
  original_hash <- functions$zm_intake_hash(input)
  for (flag in c("--finalise", "--delete-source", "--comments-reviewed")) {
    status <- withr::with_dir(analysis, system2(rscript,
      c("scripts/prepare_intake.R", "--source=synthetic", paste0("--input=", shQuote(input)), flag),
      stdout = "unsupported-argument.log", stderr = "unsupported-argument.log"))
    expect_gt(status, 0L)
    expect_identical(functions$zm_intake_hash(input), original_hash)
    expect_identical(functions$zm_intake_hash(comments_path), comment_hash)
  }

  first_store <- file.path(analysis, "_targets_first")
  first_output <- file.path(directory, "first-values.rds")
  first <- read_first_run_targets(analysis, first_store, first_output)
  expect_equal(first$report_participant_flow$n_analysis, 23L)
  # Check both independent source expectations and the fresh-store repeat.
  scales <- first$scale_score_distributions
  expect_equal(scales$mean,
    unname(vapply(scientific[scales$scale_key], mean, numeric(1))))
  numeric_rows <- first$sample_composition$numeric
  for (variable in c("demo_age", "income")) {
    row <- numeric_rows[numeric_rows$variable == variable, ]
    values <- observed[[if (variable == "demo_age") "age" else "income"]]
    expect_equal(row$mean, mean(values, na.rm = TRUE))
    expect_equal(row$n, sum(!is.na(values)))
  }

  before <- vapply(paths, functions$zm_intake_hash, character(1))
  # This simulates a later manual deletion of a temporary synthetic export;
  # the preparation command itself never removes study data.
  unlink(input)
  expect_false(file.exists(input))
  expect_identical(vapply(paths, functions$zm_intake_hash, character(1)), before)
  expect_identical(functions$zm_intake_hash(comments_path), comment_hash)

  # Move, rather than destroy, the private comments outside the analysis input
  # directory. The fresh analysis must use only the public input bundle.
  private_copy <- file.path(directory, "retained-private-comments")
  expect_true(file.rename(dirname(comments_path), private_copy))
  retained_comments <- file.path(private_copy, basename(comments_path))
  expect_identical(functions$zm_intake_hash(retained_comments), comment_hash)
  unlink(approval)
  unlink(first_store, recursive = TRUE)
  unlink(first_output)
  expect_false(dir.exists(first_store))
  expect_false(file.exists(first_output))
  expect_setequal(list.files(file.path(analysis, "data"), recursive = TRUE),
    file.path("intake", basename(paths)))
  second_store <- file.path(analysis, "_targets_restart")
  expect_false(dir.exists(second_store))
  second <- read_first_run_targets(analysis, second_store, file.path(directory, "restart-values.rds"))
  expect_equal(second, first)
  expect_true(all(file.exists(paths)))
  expect_identical(functions$zm_intake_hash(retained_comments), comment_hash)
})
