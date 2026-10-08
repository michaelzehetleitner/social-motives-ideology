# Every preregistered threshold at its exact boundary.
#
# The rules of AP1 and AP10 are all weak or strict inequalities
# ("at least 30", "at most 1/10", "excludes zero"). A suite that only tests
# values in the interior cannot detect a boundary operator that is off by one.
# Every threshold named in the preregistration is pinned here from both sides.

local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) {
    parent <- dirname(dir)
    if (identical(parent, dir)) stop("project root not found from ", getwd())
    dir <- parent
  }
  for (f in c("config.R", "io_qualtrics.R", "ap1_exclusions.R", "ap3_preprocessing.R",
              "ap3_data_files.R", "ap8_network_helpers.R", "ap8_network.R", "ap10_inference.R")) {
    source(file.path(dir, "R", f), local = FALSE)
  }
  source(file.path(dir, "tests", "support", "targets-route.R"), local = FALSE)
  assign("boundary_root", dir, envir = .GlobalEnv)
})

cfg_b <- zm_config(profile = "full", path = file.path(boundary_root, "config", "analysis_plan.yaml"))

# The preregistered edge rule as the RQ3 route applies it: present at BF >= bf_include,
# absent at BF <= bf_exclude, inconclusive between.
decide_edges <- function(bf, net) {
  extract_network_edge_decisions(tibble::tibble(bf = bf, weight = 0), list(settings = net))$decision
}

# ---- AP10: credible when the central 95% credible interval excludes zero ----

test_that("an interval whose endpoint is exactly zero does not exclude zero", {
  # AP10 (prereg): credible when the interval EXCLUDES zero. An interval that
  # touches zero contains it, so it is not credible. This is the case that a
  # `>=` implementation would get wrong and no other test covers. The Table 3
  # decisions on such an interval are tested in test-ap10-prediction-decisions.R
  # ("the endpoint exactly at zero").
  expect_false(ap7_credible(0, 0.5))       # lower bound exactly at zero
  expect_false(ap7_credible(-0.5, 0))      # upper bound exactly at zero
  expect_false(ap7_credible(0, 0))         # degenerate interval at zero
  # the neighbouring values on either side decide the other way
  expect_true(ap7_credible(1e-12, 0.5))
  expect_true(ap7_credible(-0.5, -1e-12))
  # and an interval straddling zero is never credible
  expect_false(ap7_credible(-0.1, 0.1))
  expect_equal(
    ap7_credible(c(0, -0.5, 1e-12, -1e-9), c(0.5, 0, 0.5, -1e-12)),
    c(FALSE, FALSE, TRUE, TRUE)
  )
})

# ---- AP10: edge present at BF >= 10, absent at BF <= 1/10 ---------------------

test_that("the network decision is inclusive at both Bayes-factor thresholds", {
  net <- cfg_b$network
  expect_equal(net$bf_include, 10)
  expect_equal(net$bf_exclude, 0.1)
  # exactly at a threshold the prereg's weak inequality decides
  expect_equal(decide_edges(net$bf_include, net), "present")
  expect_equal(decide_edges(net$bf_exclude, net), "absent")
  # just inside the interval it does not
  expect_equal(
    decide_edges(c(net$bf_include - 1e-9, net$bf_exclude + 1e-9), net),
    c("inconclusive", "inconclusive")
  )
  expect_equal(
    decide_edges(c(1e6, 1e-6, 1, NA), net),
    c("present", "absent", "inconclusive", NA_character_)
  )
})

# ---- AP1: the inclusive age range; the smallest divers group retained --------

test_that("AP1 retains both age bounds and excludes their immediate neighbours", {
  raw <- read_qualtrics_export(file.path(boundary_root, "data", "synthetic", "zm_panel_synthetic.sav"))
  keepers <- which(raw$survey_status == "complete" & raw$demo_gender %in% 1:2)
  expect_gte(length(keepers), 4)
  raw$demo_age[keepers[1]] <- cfg_b$exclusions$min_age       # exactly 18
  raw$demo_age[keepers[2]] <- cfg_b$exclusions$min_age - 1L  # 17
  raw$demo_age[keepers[3]] <- cfg_b$exclusions$max_age       # exactly 69
  raw$demo_age[keepers[4]] <- cfg_b$exclusions$max_age + 1L  # 70
  res <- apply_study_exclusions(raw, cfg_b)
  expect_true(raw$ResponseId[keepers[1]] %in% res$data$respondent_id)
  expect_false(raw$ResponseId[keepers[2]] %in% res$data$respondent_id)
  expect_true(raw$ResponseId[keepers[3]] %in% res$data$respondent_id)
  expect_false(raw$ResponseId[keepers[4]] %in% res$data$respondent_id)
})

test_that("AP1 keeps a divers group exactly at the minimum and drops the size below it", {
  # The threshold from both sides on the synthetic export: a group of
  # gender_divers_min_n stays, a group one smaller leaves, in the analysis and
  # in the shared scientific-use file.
  raw <- read_qualtrics_export(file.path(boundary_root, "data", "synthetic", "zm_panel_synthetic.sav"))
  codebook_b <- zm_codebook(cfg_b)
  min_n <- cfg_b$exclusions$gender_divers_min_n
  expect_equal(min_n, 30)
  complete <- raw$survey_status == "complete" & raw$demo_age >= cfg_b$exclusions$min_age &
    raw$demo_age <= cfg_b$exclusions$max_age
  # the export already carries a few divers respondents; start from none so the
  # fixture can set the group size exactly
  raw$demo_gender[complete & raw$demo_gender == 3L] <- 2L
  valid <- which(complete & raw$demo_gender %in% 1:2)
  make <- function(n_divers) {
    d <- raw
    d$demo_gender[valid[seq_len(n_divers)]] <- 3L
    apply_study_exclusions(d, cfg_b)
  }
  none <- make(0L)
  one <- make(1L)
  below <- make(min_n - 1L)
  at <- make(min_n)
  # at the minimum: the group stays and nobody is excluded for it
  expect_equal(at$log$n_excluded[nrow(at$log)], 0L)
  expect_true(at$log$divers_kept[nrow(at$log)])
  expect_equal(at$log$n_divers[nrow(at$log)], min_n)
  # one below it: the whole group is excluded and counted as excluded
  expect_equal(below$log$n_excluded[nrow(below$log)], min_n - 1L)
  expect_false(below$log$divers_kept[nrow(below$log)])
  expect_equal(below$log$n_divers[nrow(below$log)], min_n - 1L)
  # a group of one: excluded as well
  expect_equal(one$log$n_excluded[nrow(one$log)], 1L)
  expect_false(one$log$divers_kept[nrow(one$log)])
  expect_equal(one$log$n_divers[nrow(one$log)], 1L)
  # %in%: counts the divers rows. The export scenario may blank a gender cell,
  # and a blanked gender is a missing answer, not a divers answer -- with `==` it
  # would turn the whole count into NA.
  expect_equal(sum(at$data$demo_gender %in% 3L), min_n)   # the whole group kept
  expect_equal(sum(below$data$demo_gender %in% 3L), 0L)   # the whole group gone
  expect_equal(sum(one$data$demo_gender %in% 3L), 0L)
  # the sample shrinks by exactly the excluded group, and not at the minimum
  expect_equal(nrow(at$data), nrow(none$data))
  expect_equal(nrow(below$data), nrow(none$data) - (min_n - 1L))
  expect_equal(nrow(one$data), nrow(none$data) - 1L)

  # the shared scientific-use file holds the respondents of the analysis table
  for (n_divers in c(min_n - 1L, min_n)) {
    d <- raw
    d$demo_gender[valid[seq_len(n_divers)]] <- 3L
    built <- tr_route(boundary_root, tr_intake(d, cfg_b), cfg_b, codebook_b)("study_analysis_ready")
    sci <- ap3_scientific_use_data(built, cfg_b, codebook_b)
    expect_equal(sci$respondent_id, built$respondent_id, info = as.character(n_divers))
    expect_equal(sum(sci$demo_gender %in% 3L), if (n_divers >= min_n) n_divers else 0L,
                 info = as.character(n_divers))
  }
})
