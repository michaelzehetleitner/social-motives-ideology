# R/ap5_descriptives.R: demographics and quota composition, scale summaries,
# pairwise correlations (AP5)

local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) {
    parent <- dirname(dir)
    if (identical(parent, dir)) stop("project root not found from ", getwd())
    dir <- parent
  }
  for (f in c("config.R", "ap3_imputation_validity.R", "io_qualtrics.R", "ap5_descriptives.R", "report_helpers.R")) {
    source(file.path(dir, "R", f), local = FALSE)
  }
})

root <- zm_root()
analysis_plan <- zm_config(profile = "smoke", path = file.path(root, "config", "analysis_plan.yaml"))
codebook <- zm_codebook(analysis_plan)
raw <- read_qualtrics_export(file.path(root, "data", "synthetic", "zm_panel_synthetic.sav"))
kept <- raw[raw$survey_status == analysis_plan$exclusions$survey_status_complete & raw$demo_gender %in% c(1, 2), ]
scale_keys <- codebook$scales$scale_key
lo <- analysis_plan$scales$response_min
hi <- analysis_plan$scales$response_max

# Raw-column fixture: reversed items and row-mean scores, demographics as exported.
manual_scores <- function(data) {
  for (code in unique(unlist(codebook$scales$reverse_items))) {
    data[[code]] <- (lo + hi) - data[[code]]
  }
  for (i in seq_len(nrow(codebook$scales))) {
    data[[scale_keys[i]]] <- rowMeans(as.matrix(data[codebook$scales$item_codes[[i]]]))
  }
  data
}
raw_fixture <- manual_scores(kept)

pct_sums <- function(tbl) as.vector(tapply(tbl$pct, tbl$variable, sum))

# ---- helpers ------------------------------------------------------------------

test_that("ap5_summarise_numeric() drops missing values and reports the five summaries", {
  t <- ap5_summarise_numeric(c(1, 2, 3, NA), "v")
  expect_equal(t$n, 3L)
  expect_equal(t$mean, 2)
  expect_equal(t$sd, 1)
  expect_equal(t$median, 2)
  expect_equal(t$min, 1)
  expect_equal(t$max, 3)
})

# ================================================================================
# The AP5 results and the tables the report reads.
# ================================================================================

# Six respondents with values chosen so that every count, share and summary
# below can be read off by hand. The sixth respondent has no gender, no
# East/West and no income, so the categorical tables must carry a "(missing)"
# row and the numeric tables must describe the observed values only.
composition_input <- tibble::tibble(
  respondent_id = 1:6,
  age = c(20, 30, 40, 50, 60, 70),
  gender = factor(c("female", "male", "female", "male", "female", NA),
                  levels = c("female", "male")),
  education = factor(c(1, 1, 1, 2, 2, 3), levels = 1:3,
                     labels = c("Hauptschule", "Realschule", "Abitur"), ordered = TRUE),
  east_west = factor(c("west", "west", "west", "west", "east", NA),
                     levels = c("west", "east")),
  income = c(1000, 1200, 1400, 1600, 1800, NA),
  quota_group = factor(rep(c("left_leaning", "mixed", "conservative_leaning"), 2),
                       levels = c("left_leaning", "mixed", "conservative_leaning"))
)

test_that("describe_sample_composition() describes known values in the configured order", {
  composition <- describe_sample_composition(composition_input, analysis_plan)
  expect_named(composition, c("categorical", "numeric"))

  categorical <- composition$categorical
  expect_equal(unique(categorical$variable),
               setdiff(analysis_plan$descriptives$demographics, c("demo_age", "income")))
  gender <- categorical[categorical$variable == "demo_gender", ]
  expect_equal(gender$level, c("female", "male", "(missing)"))
  expect_equal(gender$n, c(3L, 2L, 1L))
  expect_equal(gender$pct, c(50, 100 / 3, 100 / 6))
  expect_equal(unique(gender$label), "Gender")
  # a declared but unobserved level keeps its row with a zero count
  education <- categorical[categorical$variable == "demo_edu_school", ]
  expect_equal(education$level, c("Hauptschule", "Realschule", "Abitur"))
  expect_equal(education$n, c(3L, 2L, 1L))
  expect_equal(unname(pct_sums(categorical)), rep(100, 4), tolerance = 1e-10)

  numeric <- composition$numeric
  expect_equal(numeric$variable, c("demo_age", "income"))
  expect_equal(numeric$label, c("Age (years)", "Net household income per person"))
  expect_equal(numeric$n, c(6L, 5L))
  expect_equal(numeric$mean, c(45, 1400))
  expect_equal(numeric$median, c(45, 1400))
  expect_equal(numeric$min, c(20, 1000))
  expect_equal(numeric$max, c(70, 1800))
})

test_that("the sample composition has the columns and scalar types the report reads", {
  composition <- describe_sample_composition(composition_input, analysis_plan)
  expect_named(composition, c("categorical", "numeric"))
  expect_identical(names(composition$categorical), c("variable", "label", "level", "n", "pct"))
  expect_identical(names(composition$numeric),
                   c("variable", "label", "n", "mean", "sd", "median", "min", "max"))
  expect_type(composition$categorical$level, "character")
  expect_type(composition$categorical$n, "integer")
  expect_type(composition$categorical$pct, "double")
  expect_type(composition$numeric$n, "integer")
  for (column in c("mean", "sd", "median", "min", "max")) {
    expect_type(composition$numeric[[column]], "double")
  }
  expect_true(all(composition$categorical$pct >= 0 & composition$categorical$pct <= 100))
  expect_true("(missing)" %in% composition$categorical$level)
})

# Nine scale scores with hand-computable summaries: every scale is the same
# five observations shifted by its position, so the mean and the range follow.
distribution_input <- local({
  data <- tibble::tibble(respondent_id = 1:5)
  for (index in seq_along(scale_keys)) {
    data[[scale_keys[index]]] <- c(1, 2, 3, 4, 5) + (index - 1) * 0.1
  }
  data
})

test_that("describe_scale_distributions() describes every registered scale score", {
  distributions <- describe_scale_distributions(distribution_input, codebook)
  expect_equal(distributions$scale_key, scale_keys)
  expect_equal(distributions$label, codebook$scales$label)
  expect_identical(names(distributions),
                   c("scale_key", "label", "n", "mean", "sd", "median", "min", "max",
                     "skew", "kurtosis"))
  expect_equal(distributions$n, rep(5L, length(scale_keys)))
  expect_equal(distributions$mean, 3 + (seq_along(scale_keys) - 1) * 0.1)
  expect_equal(distributions$median, 3 + (seq_along(scale_keys) - 1) * 0.1)
  expect_equal(distributions$min, 1 + (seq_along(scale_keys) - 1) * 0.1)
  expect_equal(distributions$max, 5 + (seq_along(scale_keys) - 1) * 0.1)
  expect_equal(distributions$sd, rep(stats::sd(1:5), length(scale_keys)))
  expect_equal(distributions$skew, rep(0, length(scale_keys)))
})

test_that("the scale-score distributions carry the columns and types the report reads", {
  distributions <- describe_scale_distributions(distribution_input, codebook)
  expect_setequal(names(distributions),
                  c("scale_key", "label", "mean", "sd", "min", "max", "skew", "kurtosis",
                    "n", "median"))
  expect_type(distributions$n, "integer")
  expect_type(distributions$scale_key, "character")
  expect_type(distributions$label, "character")
  for (column in c("mean", "sd", "min", "max", "skew", "kurtosis", "median")) {
    expect_type(distributions[[column]], "double")
  }
})

test_that("the hard-coded correlation columns are the registered scales in motive-outcome order", {
  # A design assertion: describe_scale_correlations() fixes the nine
  # columns, so the codebook and that fixed list may not drift apart.
  columns <- colnames(describe_scale_correlations(distribution_input, codebook, analysis_plan)$matrix)
  expect_setequal(columns, codebook$scales$scale_key)
  expect_identical(columns[1:5], analysis_plan$regression$motives)
  expect_identical(columns[6:9], analysis_plan$regression$outcomes)
})

test_that("describe_scale_correlations() returns symmetric matrices and its labels", {
  correlations <- describe_scale_correlations(raw_fixture, codebook, analysis_plan)
  expect_named(correlations, c("matrix", "n", "labels", "method"))
  columns <- colnames(correlations$matrix)
  expect_true(isSymmetric(unname(correlations$matrix)))
  expect_equal(unname(diag(correlations$matrix)), rep(1, 9))
  expect_identical(dimnames(correlations$n), dimnames(correlations$matrix))
  expect_true(isSymmetric(unname(correlations$n)))
  expect_equal(correlations$method, analysis_plan$descriptives$correlations)
  expect_equal(correlations$labels,
               unname(codebook$scales$label[match(columns, codebook$scales$scale_key)]))
})
