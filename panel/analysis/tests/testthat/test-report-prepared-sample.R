# Prepared report consumers use univariate observed margins and saved results;
# participant links are neither needed nor reconstructed.
local({
  root <- normalizePath(getwd())
  while (!file.exists(file.path(root, "config", "analysis_plan.yaml"))) root <- dirname(root)
  for (file in c("config.R", "ap5_descriptives.R", "report_results_sample.R", "report_imputation.R", "report_helpers.R")) {
    source(file.path(root, "R", file), local = FALSE)
  }
  assign("prepared_sample_test_root", root, envir = .GlobalEnv)
})

test_that("independent demographic permutations leave every sample statistic unchanged", {
  margins <- data.frame(age = c(20, 40, 60, NA_real_), income = c(500, 1000, NA_real_, 4000),
    education = c("first", "second", "second", NA_character_), east_west = c("East", "West", "West", "East"),
    duration = c(100, 200, 300, 400))
  analysis <- data.frame(gender = c("female", "male", NA, "male"), quota_group = c("left", "right", "right", "left"))
  plan <- list(descriptives = list(demographics = c("demo_age", "demo_gender", "demo_edu_school", "east_west", "income", "quota_group")))
  first <- describe_observed_sample_composition(margins, analysis, plan)
  shuffled <- margins
  shuffled$age <- margins$age[c(3, 1, 4, 2)]
  shuffled$income <- margins$income[c(2, 4, 1, 3)]
  shuffled$education <- margins$education[c(4, 3, 2, 1)]
  shuffled$east_west <- margins$east_west[c(2, 1, 4, 3)]
  shuffled$duration <- margins$duration[c(4, 1, 3, 2)]
  second <- describe_observed_sample_composition(shuffled, analysis, plan)
  expect_equal(second, first, tolerance = 1e-14)
  expect_equal(first$numeric$mean, c(40, 5500 / 3))
  expect_equal(first$numeric$n, c(3L, 3L))
  expect_equal(report_sample_characteristic_rows(second), report_sample_characteristic_rows(first), tolerance = 1e-14)
})

test_that("saved imputation facts preserve successful, failed and absent-model wording", {
  cases <- list(
    list(imputed_cells = data.frame(variable = character()), n_unfilled = 0L, model_status = NULL),
    list(imputed_cells = data.frame(variable = "item"), n_unfilled = 0L, model_status = data.frame(status = "ok")),
    list(imputed_cells = data.frame(variable = "item"), n_unfilled = 1L, model_status = data.frame(status = "failed")),
    list(imputed_cells = data.frame(variable = "item"), n_unfilled = 0L, model_status = NULL))
  expected <- c(
    "No value had to be filled.",
    "1 missing value was filled using imputation models that passed the AP3 validity gate. The completed values are treated as fixed in subsequent analyses.",
    "1 missing value was filled. 1 value remains missing because a required imputation fit or an individual prediction was unavailable. Respondents with unresolved values were excluded from analyses requiring those values.",
    "1 missing value was filled. Model validity metadata are unavailable; AP3 imputation processing needs to be refreshed.")
  for (i in seq_along(cases)) {
    facts <- summarise_imputation_processing(cases[[i]])
    expect_identical(describe_imputation_processing(cases[[i]], facts = facts), expected[[i]])
    expect_identical(describe_imputation_processing(cases[[i]]), expected[[i]])
  }
})


test_that("saved sample histogram bins render without retaining linked numeric rows", {
  old_directory <- getwd()
  setwd(tempdir())
  on.exit(setwd(old_directory), add = TRUE)
  format <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  on.exit(knitr::opts_knit$set(rmarkdown.pandoc.to = format), add = TRUE)
  knitr::opts_knit$set(rmarkdown.pandoc.to = "html")
  rows <- data.frame(characteristic = c("Party choice", "Party sympathy", "Left–right placement"),
                     group = c("The Greens", "The Left", ""),
                     value = c("2 (50.0%)", "0.00 (2.45)", "5.00 (7.07)"), histogram = "")
  bins <- list(sympathy = calculate_histogram_data(c(-3, 0, 3), c(-3, 3), 1),
               placement = calculate_histogram_data(c(0, 10, NA_real_), c(0, 10)))
  shown <- rh_add_histograms(rh_table(rows, engine = "gt"), NULL, c("sympathy", "placement"),
    rows = 2:3, response_range = list(c(-3, 3), c(0, 10)), bin_width = c(1, NA), histogram_data = bins)
  expect_equal(shown[["_data"]], tibble::as_tibble(rows))
  html <- as.character(gt::as_raw_html(shown))
  expect_match(html, "The Greens", fixed = TRUE)
  expect_match(html, "The Left", fixed = TRUE)
  expect_match(html, "data:image/png;base64,", fixed = TRUE)
})


test_that("missing school education is counted in the live table note without changing denominators", {
  root <- prepared_sample_test_root
  qmd <- readLines(file.path(root, "report", "results_draft.qmd"), warn = FALSE)
  start <- which(qmd == "#| label: tbl-sample")
  end <- which(seq_along(qmd) > start & qmd == "```")[[1L]]
  chunk <- parse(text = qmd[seq.int(start + 1L, end - 1L)])
  codebook <- list(factors = utils::read.csv(file.path(root, "..", "preregistration", "codebook_factors.csv"),
                                            stringsAsFactors = FALSE))
  format <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  on.exit(knitr::opts_knit$set(rmarkdown.pandoc.to = format), add = TRUE)
  knitr::opts_knit$set(rmarkdown.pandoc.to = "latex")
  for (missing in c(0L, 1L, 3L)) {
    n <- 2L + missing
    education <- factor(c("Schule beendet ohne Abschluss", "Bin noch Schüler/in", rep(NA_character_, missing)),
                        levels = c("Schule beendet ohne Abschluss", "Bin noch Schüler/in"))
    composition <- list(
      categorical = ap5_count_categories(education, "demo_edu_school", "School education"),
      numeric = tibble::tibble(label = character(), n = integer(), mean = numeric(), sd = numeric()))
    saved <- report_sample_characteristic_rows(composition)
    expect_equal(attr(saved, "n_missing_education"), missing)
    expect_equal(sum(saved$n), n)
    data <- data.frame(respondent_id = seq_len(n), age = 20 + seq_len(n), income = 100 * seq_len(n),
      pol_vote_would = 1, pol_party_vote = 1, pol_left_right = 5)
    for (party in c("spd", "cdu_csu", "greens", "fdp", "afd", "linke")) data[[paste0("pol_symp_", party)]] <- 0
    politics <- describe_sample_politics_for_report(data, data, codebook, list(exclusions = list(min_age = 18, max_age = 69)))
    politics$scores <- NULL
    env <- new.env(parent = globalenv())
    env$report_sample_rows_value <- saved
    env$report_political_sample_value <- politics
    env$prereg <- character()
    env$rh_prereg_passage <- function(...) "Exact ages and incomes use observed responses."
    expect_no_error(eval(chunk, envir = env))
    shown <- subset(env$sample_rows_value, characteristic == "School education")
    expect_identical(shown$group, c("Schule beendet ohne Abschluss", "Bin noch Schüler/in"))
    expect_false(any(shown$group %in% c("(missing)", "Missing")))
    expect_equal(shown$n, c(1L, 1L))
    expect_equal(shown$pct, rep(100 / n, 2))
    expect_equal(sum(shown$n) + env$n_missing_education, n)
    note <- paste(as.character(env$sample_table), collapse = "\n")
    expect_match(note, paste0("School education was missing for ", missing,
      if (missing == 1L) " respondent." else " respondents."), fixed = TRUE)
  }
})
