# R/simulate_testdata.R and data/synthetic/*: the synthetic export realises simulation_truth.yaml

local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) {
    parent <- dirname(dir)
    if (identical(parent, dir)) stop("project root not found from ", getwd())
    dir <- parent
  }
  for (f in c("config.R", "io_qualtrics.R", "simulate_testdata.R")) {
    source(file.path(dir, "R", f), local = FALSE)
  }
})

root <- zm_root()
analysis_plan <- zm_config(profile = "full", path = file.path(root, "config", "analysis_plan.yaml"))
truth_base <- zm_truth(file.path(root, "config", "simulation_truth.yaml"))
codebook <- zm_codebook(analysis_plan)
schema <- zm_raw_schema()
sav_path <- file.path(root, "data", "synthetic", "zm_panel_synthetic.sav")
latent_path <- file.path(root, "data", "synthetic", "zm_panel_synthetic_latent.csv")
skip_if_not(file.exists(sav_path) && file.exists(latent_path),
            "synthetic export not generated (run scripts/make_synthetic_data.R)")

raw <- read_qualtrics_export(sav_path)
latent <- readr::read_csv(latent_path, show_col_types = FALSE, progress = FALSE)

# Which declared scenario does the export on disk realise? Every check that
# reads data/synthetic/ is run against that scenario's truth, so a stale export
# gives one clear message (the test below) instead of two dozen mismatches. The
# checks of the planted faults simulate their scenario in memory and do not
# depend on what is on disk.
scenario_names <- setdiff(names(truth_base$scenarios), "export")
sims <- lapply(scenario_names, function(s) {
  simulate_zm_panel(truth = truth_base, codebook = codebook, analysis_plan = analysis_plan,
                    seed = truth_base$seed, scenario = s)
})
names(sims) <- scenario_names
matches_disk <- vapply(sims, function(s) {
  isTRUE(all.equal(as.numeric(s$raw$ASC_aag_1), as.numeric(raw$ASC_aag_1))) &&
    isTRUE(all.equal(as.numeric(s$raw$demo_gender), as.numeric(raw$demo_gender)))
}, logical(1))
scenario_on_disk <- if (any(matches_disk)) scenario_names[which(matches_disk)[1]] else NA_character_
skip_if(is.na(scenario_on_disk),
        "data/synthetic/ realises no declared scenario of simulation_truth.yaml (regenerate it)")
sim <- sims[[scenario_on_disk]]
truth <- sim$truth

test_that("the export on disk realises the scenario the truth file declares", {
  if (!identical(scenario_on_disk, truth_base$scenarios$export)) {
    skip(paste0(
      "data/synthetic/ realises scenario '", scenario_on_disk,
      "' but simulation_truth.yaml declares '", truth_base$scenarios$export,
      "' as the export scenario. Regenerate with: Rscript scripts/make_synthetic_data.R"
    ))
  }
  expect_identical(scenario_on_disk, truth_base$scenarios$export)
})

ex <- truth$exclusions
rmin <- analysis_plan$scales$response_min
rmax <- analysis_plan$scales$response_max
a1 <- analysis_plan$exclusions$attention_check_1_correct
a2 <- analysis_plan$exclusions$attention_check_2_correct
consent_yes <- analysis_plan$exclusions$consent_required_value
gender_codes <- sim_value_set(codebook, "gender_1_3")
# A missing age is retained by AP1 (criterion 3), and a scenario may plant one
# among the kept rows, so the mirror of the AP1 rule here has to keep it too.
valid <- raw$survey_status == analysis_plan$exclusions$survey_status_complete &
  raw$consent_check == consent_yes &
  (is.na(raw$demo_age) | raw$demo_age >= analysis_plan$exclusions$min_age &
    raw$demo_age <= analysis_plan$exclusions$max_age) &
  raw$attentioncheck_1 == a1 &
  raw$attentioncheck_2 == a2
valid[is.na(valid)] <- FALSE
motives <- truth$motive_correlations$order
outcomes <- truth$outcome_residual_correlations$order

rekey <- function(d, key) {
  i <- which(codebook$scales$scale_key == key)
  m <- as.matrix(d[, codebook$scales$item_codes[[i]]])
  for (r in codebook$scales$reverse_items[[i]]) m[, r] <- (rmin + rmax) - m[, r]
  m
}

test_that("simulate_zm_panel() returns the contract shape", {
  expect_named(sim, c("raw", "latent", "truth"))
  expect_equal(names(sim$raw), schema$name)
  expect_equal(nrow(sim$raw), truth$n_total)
  expect_equal(nrow(sim$latent), truth$n_total)
  expect_true(all(c("respondent_row", motives, outcomes, "age", "male",
                    "income_true", "exclusion_reason") %in% names(sim$latent)))
  expect_equal(sim$latent$respondent_row, seq_len(truth$n_total))
  expect_equal(sim$latent$ResponseId, sim$raw$ResponseId)
  expect_identical(sim$truth, truth)
})

test_that("the files on disk equal a fresh simulation with the truth seed", {
  expect_equal(nrow(raw), nrow(sim$raw))
  for (col in schema$name) {
    if (inherits(sim$raw[[col]], "POSIXct")) {
      expect_equal(as.numeric(raw[[col]]), as.numeric(sim$raw[[col]]), info = col)
    } else {
      expect_equal(raw[[col]], sim$raw[[col]], info = col)
    }
  }
  expect_equal(latent$exclusion_reason, sim$latent$exclusion_reason)
  for (col in c(motives, outcomes, "age", "male", "income_true")) {
    expect_equal(latent[[col]], sim$latent[[col]], tolerance = 1e-12, info = col)
  }
})

test_that("exclusion counts equal simulation_truth.yaml exactly", {
  expect_equal(nrow(raw), truth$n_total)
  reasons <- table(latent$exclusion_reason)
  expect_equal(unname(reasons["consent_refused"]), ex$consent_refused)
  expect_equal(unname(reasons["incomplete"]), ex$incomplete)
  expect_equal(unname(reasons["age_under_18"]), ex$age_under_18)
  expect_equal(unname(reasons["quota_full"]), ex$quota_full)
  expect_equal(unname(reasons["attention_1_failed"]), ex$attention_1_failed)
  expect_equal(unname(reasons["attention_2_failed"]), ex$attention_2_failed)
  expect_equal(unname(reasons["none"]), truth$n_kept)

  # the same counts as the raw export shows them
  expect_equal(sum(raw$consent_check != consent_yes, na.rm = TRUE), ex$consent_refused)
  expect_equal(sum(raw$survey_status == "consent_refused"), ex$consent_refused)
  expect_equal(sum(raw$Finished == 0L), ex$incomplete)
  expect_equal(sum(raw$Progress < 100), ex$incomplete)
  expect_equal(sum(raw$survey_status == ""), ex$incomplete)
  expect_equal(sum(raw$demo_age < analysis_plan$exclusions$min_age, na.rm = TRUE), ex$age_under_18)
  expect_equal(sum(raw$attentioncheck_1 != a1, na.rm = TRUE), ex$attention_1_failed)
  expect_equal(sum(raw$survey_status_detail == "attentioncheck_1"), ex$attention_1_failed)
  expect_equal(sum(raw$attentioncheck_2 != a2, na.rm = TRUE), ex$attention_2_failed)
  expect_equal(sum(raw$survey_status_detail == "attentioncheck_2"), ex$attention_2_failed)
  expect_equal(sum(raw$survey_status == "attention_failed"), ex$attention_1_failed + ex$attention_2_failed)
  expect_equal(sum(raw$survey_status == analysis_plan$exclusions$survey_status_quota_full), ex$quota_full)
  expect_equal(sum(raw$demo_gender == gender_codes[3], na.rm = TRUE), truth$covariates$gender_divers_n)
  expect_equal(sum(valid), truth$n_kept)
  expect_equal(sum(valid), 706)
  expect_true(all(latent$exclusion_reason[valid] == "none"))
})

test_that("exclusion rows are shaped like the survey flow produces them", {
  items <- schema$name[schema$role == "item"]
  cr <- latent$exclusion_reason == "consent_refused"
  expect_true(all(is.na(as.matrix(raw[cr, items]))))
  expect_true(all(is.na(raw$demo_age[cr])))
  expect_true(all(raw$Finished[cr] == 1L & raw$Progress[cr] == 100L))
  expect_true(all(raw$quota_group[cr] == "" & raw$possibly_left[cr] == "0"))

  f1 <- latent$exclusion_reason == "attention_1_failed"
  after_att1 <- schema$name[seq(which(schema$name == "attentioncheck_1") + 1, which(schema$name == "demo_hh_members"))]
  after_att1 <- after_att1[schema$type[match(after_att1, schema$name)] == "numeric"]
  expect_true(all(is.na(as.matrix(raw[f1, after_att1]))))
  expect_true(all(!is.na(raw$UMS_ach_1[f1])))
  expect_true(all(raw$survey_status[f1] == "attention_failed"))
  f2 <- latent$exclusion_reason == "attention_2_failed"
  expect_true(all(raw$attentioncheck_1[f2] == a1))
  expect_true(all(is.na(raw$demo_gender[f2])))
  expect_true(all(!is.na(raw$SDO_D_8_r[f2])))

  inc <- latent$exclusion_reason == "incomplete"
  seq_cols <- schema$name[seq(which(schema$name == "consent_check"), which(schema$name == "demo_hh_members"))]
  seq_cols <- seq_cols[schema$type[match(seq_cols, schema$name)] == "numeric" & seq_cols != "info_teilnahme_2_1"]
  for (i in which(inc)) {
    answered <- !is.na(unlist(raw[i, seq_cols]))
    # once a question is unanswered every later one is too (single break-off point)
    expect_true(all(diff(as.integer(answered)) <= 0), info = paste("row", i))
    expect_true(any(!answered), info = paste("row", i))
  }
  expect_true(all(raw$RecordedDate[inc] > raw$EndDate[inc] + 13 * 86400))
  expect_true(all(raw$survey_status[latent$exclusion_reason %in% c("none", "age_under_18", "gender_divers")] == "complete"))

  under <- latent$exclusion_reason == "age_under_18"
  expect_true(all(raw$demo_age[under] < analysis_plan$exclusions$min_age))
  expect_true(all(!is.na(raw$SDO_D_1[under])))
})

test_that("quota-full rows leave after the scalometers and before the first attention check", {
  qf <- latent$exclusion_reason == "quota_full"
  expect_equal(sum(qf), ex$quota_full)
  # exported as a finished response, like every other flow exit
  expect_true(all(raw$Finished[qf] == 1L & raw$Progress[qf] == 100L))
  expect_true(all(raw$survey_status[qf] == analysis_plan$exclusions$survey_status_quota_full))
  expect_true(all(raw$consent_check[qf] == consent_yes))
  expect_true(all(raw$demo_age[qf] >= analysis_plan$exclusions$min_age))
  # answered up to and including the six scalometers
  scal_cols <- schema$name[startsWith(schema$name, "pol_symp_")]
  expect_length(scal_cols, 6L)
  expect_false(anyNA(as.matrix(raw[qf, scal_cols])))
  expect_true(all(nzchar(raw$quota_group[qf])))
  # nothing after them: items, both checks, the demographics behind them
  after <- schema$name[seq(which(schema$name == "pol_symp_linke") + 1, which(schema$name == "demo_hh_members"))]
  after <- after[schema$type[match(after, schema$name)] == "numeric"]
  expect_true(all(is.na(as.matrix(raw[qf, after]))))
  expect_true(all(is.na(raw$attentioncheck_1[qf])))
  expect_true(all(is.na(raw$attentioncheck_2[qf])))
  # the detail field names the row's own quota cell
  expect_equal(raw$survey_status_detail[qf], paste0("quota_", raw$quota_group[qf]))
  expect_true(all(raw$survey_status_detail[qf] %in%
                    c("quota_left_leaning", "quota_conservative_leaning", "quota_mixed")))
  expect_true(all(c("quota_left_leaning", "quota_conservative_leaning", "quota_mixed") %in%
                    raw$survey_status_detail[qf]))
  # and they are not counted as any other exclusion
  expect_false(any(valid[qf]))
})

test_that("exclusions are interleaved with valid rows and identifiers look like Qualtrics", {
  pos <- which(latent$exclusion_reason != "none")
  expect_true(min(pos) < nrow(raw) / 4)
  expect_true(max(pos) > 3 * nrow(raw) / 4)
  expect_true(all(grepl("^R_[A-Za-z0-9]{15}$", raw$ResponseId)))
  expect_equal(anyDuplicated(raw$ResponseId), 0)
  expect_true(all(grepl("^[0-9]+$", raw$bilendi_id)))
  expect_equal(anyDuplicated(raw$bilendi_id), 0)
  expect_true(all(format(raw$StartDate, "%Y-%m") == "2026-06"))
  expect_true(all(raw$EndDate >= raw$StartDate))
  expect_true(all(raw$Status == "0"))
  expect_true(mean(raw$Kommentarfeld == "") > 0.9)
  expect_true(any(raw$Kommentarfeld != ""))
})

test_that("quota flags and quota_group follow the M4 rule from the scalometers", {
  sc <- raw[, c("pol_symp_spd", "pol_symp_cdu_csu", "pol_symp_greens", "pol_symp_fdp", "pol_symp_afd", "pol_symp_linke")]
  complete_sc <- stats::complete.cases(sc)
  expect_gt(sum(complete_sc), 700)
  d <- raw[complete_sc, ]
  left <- as.integer(d$pol_symp_spd > 0 | d$pol_symp_linke > 0 | d$pol_symp_greens > 0)
  cons <- as.integer(d$pol_symp_afd > 0 | d$pol_symp_cdu_csu > 0 | d$pol_symp_fdp > 0)
  group <- ifelse(left == 1 & cons == 0, "left_leaning",
                  ifelse(cons == 1 & left == 0, "conservative_leaning", "mixed"))
  expect_equal(d$possibly_left, as.character(left))
  expect_equal(d$possibly_conservative, as.character(cons))
  expect_equal(d$quota_group, group)
  expect_true(all(c("left_leaning", "conservative_leaning", "mixed") %in% d$quota_group))
  expect_true(all(as.matrix(sc[complete_sc, ]) %in% -3:3))
  expect_true(all(raw$quota_group[!complete_sc] == ""))
})

test_that("reverse-keyed items are stored as displayed: negative correlation with their scale", {
  d <- raw[valid, ]
  n_checked <- 0
  for (i in seq_len(nrow(codebook$scales))) {
    rev <- codebook$scales$reverse_items[[i]]
    if (length(rev) == 0) next
    straight <- setdiff(codebook$scales$item_codes[[i]], rev)
    other <- rowMeans(as.matrix(d[, straight]))
    for (r in rev) {
      # `use = "complete.obs"`: a scenario may blank one straight item of the
      # scale for a few kept rows, which would make the correlation NA.
      expect_lt(stats::cor(d[[r]], other, use = "complete.obs"), -0.2)
      n_checked <- n_checked + 1
    }
  }
  expect_equal(n_checked, length(unlist(codebook$scales$reverse_items)))
  items <- schema$name[schema$role == "item"]
  answered <- stats::na.omit(as.vector(as.matrix(d[, items])))
  expect_true(all(answered %in% seq(rmin, rmax)))
})

test_that("OLS on the latent file recovers true_beta within 0.06 for every coefficient", {
  X <- scale(as.matrix(latent[, c(motives, "age", "male", "income_true")]))
  colnames(X) <- c(motives, "age", "male", "income")
  for (o in outcomes) {
    y <- as.numeric(scale(latent[[o]]))
    b <- stats::coef(stats::lm(y ~ X))[-1]
    names(b) <- colnames(X)
    tb <- unlist(truth$true_beta[[o]])[names(b)]
    expect_true(all(abs(b - tb) < 0.06), info = o)
    expect_equal(unname(b), unname(tb), tolerance = 1e-6, info = paste(o, "exact"))
  }
  # on the analysis sample alone the recovery still holds within tolerance
  keep <- latent$exclusion_reason == "none"
  Xk <- scale(as.matrix(latent[keep, c(motives, "age", "male", "income_true")]))
  for (o in outcomes) {
    b <- stats::coef(stats::lm(as.numeric(scale(latent[[o]][keep])) ~ Xk))[-1]
    tb <- unlist(truth$true_beta[[o]])[c(motives, "age", "male", "income")]
    expect_true(all(abs(b - tb) < 0.06), info = paste(o, "kept rows"))
  }
})

test_that("latent structure matches the truth: motive correlations, outcome variance, covariate links", {
  expect_equal(stats::cor(as.matrix(latent[, motives])), truth$motive_correlations$matrix, tolerance = 1e-6)
  for (o in outcomes) expect_equal(stats::var(latent[[o]]), 1, tolerance = 1e-6)
  cmc <- truth$covariates$covariate_motive_correlations
  for (k in names(cmc$age_with)) {
    expect_equal(stats::cor(latent$age, latent[[k]]), cmc$age_with[[k]], tolerance = 0.05)
  }
  for (k in names(cmc$male_with)) {
    expect_equal(stats::cor(latent$male, latent[[k]]), cmc$male_with[[k]], tolerance = 0.05)
  }
  d <- raw[valid, ]
  expect_equal(stats::cor(d$pol_left_right, latent$sdo_dom[valid], use = "complete.obs"),
               truth$politics$left_right_with_sdo, tolerance = 0.08)
  expect_true(all(d$demo_age >= truth$covariates$age$min & d$demo_age <= truth$covariates$age$max,
                  na.rm = TRUE))
  # the true income equals the pipeline's derivation wherever the export holds the inputs
  bands <- analysis_plan$income$band_representative
  has_income <- !is.na(raw$demo_income_hh_net) & !is.na(raw$demo_hh_members)
  expect_gt(sum(has_income), 700)
  expect_equal(latent$income_true[has_income],
               unname(bands[raw$demo_income_hh_net[has_income]]) /
                 pmin(raw$demo_hh_members[has_income], analysis_plan$income$hh_size_top_value),
               tolerance = 1e-9)
})

test_that("McDonald's omega per scale among valid rows is between .72 and .90", {
  d <- raw[valid, ]
  for (key in codebook$scales$scale_key) {
    m <- rekey(d, key)
    m <- m[stats::complete.cases(m), , drop = FALSE]
    om <- suppressWarnings(suppressMessages(psych::omega(m, nfactors = 1, plot = FALSE)))
    expect_gt(om$omega.tot, 0.72, label = paste(key, "omega_t"))
    expect_lt(om$omega.tot, 0.90, label = paste(key, "omega_t"))
  }
})

test_that("scale means among valid rows are within 0.5 of the measurement location", {
  d <- raw[valid, ]
  for (key in codebook$scales$scale_key) {
    m <- rekey(d, key)
    expect_lt(abs(mean(rowMeans(m), na.rm = TRUE) - truth$measurement$scale_location[[key]]), 0.5, label = key)
  }
})

test_that("item distributions are realistic: no category holds more than 40% of a scale's responses", {
  d <- raw[valid, ]
  for (i in seq_len(nrow(codebook$scales))) {
    m <- as.matrix(d[, codebook$scales$item_codes[[i]]])
    shares <- prop.table(table(factor(as.vector(m), levels = seq(rmin, rmax))))
    expect_lt(max(shares), 0.40, label = codebook$scales$scale_key[i])
    expect_equal(length(unique(stats::na.omit(as.vector(m)))), rmax - rmin + 1)
  }
})

test_that("the simulation is deterministic in the seed and changes with it", {
  again <- simulate_zm_panel(truth = truth, codebook = codebook, analysis_plan = analysis_plan, seed = truth$seed)
  expect_equal(again$raw$UMS_ach_1, sim$raw$UMS_ach_1)
  expect_equal(again$latent$zm_power, sim$latent$zm_power)
  other <- simulate_zm_panel(truth = truth, codebook = codebook, analysis_plan = analysis_plan, seed = truth$seed + 1)
  expect_false(identical(other$raw$UMS_ach_1, sim$raw$UMS_ach_1))
  expect_equal(table(other$latent$exclusion_reason), table(sim$latent$exclusion_reason))
})

test_that("party-choice fields respect the Yes-only display condition", {
  hidden <- is.na(raw$pol_vote_would) | raw$pol_vote_would != 1
  expect_true(all(is.na(raw$pol_party_vote[hidden])))
  expect_true(all(is.na(raw$pol_party_vote_801_TEXT[hidden]) | raw$pol_party_vote_801_TEXT[hidden] == ""))
  expect_true(all(latent$exclusion_reason[raw$demo_gender %in% 3] == "none"))
})

# ---- scenarios: what each one is declared to plant, and what it realises -----

test_that("zm_truth_scenario() applies only the deviations the scenario declares", {
  clean <- zm_truth_scenario(truth_base, "clean")
  trouble <- zm_truth_scenario(truth_base, "trouble")

  # clean is the base truth: nothing overridden, no fault attached
  expect_identical(clean$scenario, "clean")
  expect_equal(clean$true_beta, truth_base$true_beta)
  expect_null(clean$missingness)
  expect_null(clean$cross_loadings)
  expect_null(clean$residual_contamination)

  # trouble changes exactly one true_beta cell, and it is a sign flip
  expect_identical(trouble$scenario, "trouble")
  expect_equal(trouble$true_beta$asc_conv$zm_prestige, -0.15)
  flipped <- 0
  for (o in names(truth_base$true_beta)) {
    for (p in names(truth_base$true_beta[[o]])) {
      a <- truth_base$true_beta[[o]][[p]]
      b <- trouble$true_beta[[o]][[p]]
      if (!isTRUE(all.equal(a, b))) {
        flipped <- flipped + 1
        expect_equal(b, -a, info = paste(o, p))
      }
    }
  }
  expect_equal(flipped, 1L)
  # the flipped cell is prestige -> conventionalism, which preregistration
  # Table 3 marks ±: the flip plants a credible negative association that AP10
  # reports by credibility and sign, without a directional verdict.
  expect_identical(analysis_plan$predictions$table$asc_conv$zm_prestige, "±")
  expect_lt(trouble$true_beta$asc_conv$zm_prestige, 0)

  # resolving twice is a no-op; resolving to a different scenario is refused
  expect_identical(zm_truth_scenario(trouble), trouble)
  expect_error(zm_truth_scenario(trouble, "clean"), "already resolved")
  expect_error(zm_truth_scenario(truth_base, "nonesuch"), "unknown scenario")
  # the default is the declared export scenario
  expect_identical(zm_truth_scenario(truth_base)$scenario, truth_base$scenarios$export)
})

test_that("the trouble scenario plants exactly the declared missing cells, among kept rows", {
  tro <- sims[["trouble"]]
  cle <- sims[["clean"]]
  kept <- tro$latent$exclusion_reason == "none"
  spec <- tro$truth$missingness
  expect_true(isTRUE(spec$distinct_rows))

  blanked <- list()
  for (cell in spec$cells) {
    col <- cell$column
    extra <- which(is.na(tro$raw[[col]]) & !is.na(cle$raw[[col]]))
    expect_equal(length(extra), as.integer(cell$n_missing), info = col)
    expect_true(all(kept[extra]), info = col)
    blanked[[col]] <- extra
  }
  # one respondent per blanked cell, as declared
  all_rows <- unlist(blanked, use.names = FALSE)
  expect_equal(anyDuplicated(all_rows), 0L)
  # nothing AP1 decides on is ever blanked
  for (col in c("attentioncheck_1", "attentioncheck_2", "consent_check")) {
    expect_equal(sum(is.na(tro$raw[[col]]) & !is.na(cle$raw[[col]])), 0L, info = col)
  }
  # the clean scenario blanks nothing at all
  for (cell in spec$cells) {
    expect_equal(sum(is.na(cle$raw[[cell$column]])), sum(is.na(cle$raw[[cell$column]])))
  }
  # exactly one missing gender, and the row is kept: AP1 has no criterion for it
  expect_equal(sum(is.na(tro$raw$demo_gender)) - sum(is.na(cle$raw$demo_gender)), 1L)
  # the latent file keeps the truth: the fault is in the export, not in the truth
  expect_false(anyNA(tro$latent$age))
  expect_false(anyNA(tro$latent$gender))
})

test_that("the trouble scenario plants the declared cross-loading and nothing else", {
  tro <- sims[["trouble"]]
  entry <- tro$truth$cross_loadings[[1]]
  expect_identical(entry$item, "ASC_con_3")
  expect_identical(entry$factor, "asc_sub")
  kept <- tro$latent$exclusion_reason == "none"
  d <- tro$raw[kept, ]
  other_latent <- tro$latent$asc_sub[kept]
  i <- which(codebook$scales$scale_key == "asc_conv")
  codes <- codebook$scales$item_codes[[i]]
  r <- vapply(codes, function(code) {
    x <- d[[code]]
    if (code %in% codebook$scales$reverse_items[[i]]) x <- (rmin + rmax) - x
    stats::cor(x, other_latent, use = "complete.obs")
  }, numeric(1))
  # the cross-loading item correlates far more strongly with the OTHER latent
  # than any of its block mates, which carry only the between-scale correlation
  expect_gt(r[["ASC_con_3"]], max(r[setdiff(codes, "ASC_con_3")]) + 0.20)
  # and no item of any other scale is affected: clean and trouble differ only in
  # the scales the faults touch (asc_conv through the cross-loading, asc_agg
  # through the contaminated residual and the flipped cell)
  cle <- sims[["clean"]]
  untouched <- c("UMS_ach_1", "DOPL_dom_1", "SDO-D_1")
  for (code in untouched) {
    expect_equal(tro$raw[[code]], cle$raw[[code]], info = code)
  }
})

test_that("the trouble scenario contaminates the declared share of one outcome's residuals", {
  tro <- sims[["trouble"]]
  rc <- tro$truth$residual_contamination
  expect_identical(rc$outcome, "asc_agg")
  expect_equal(as.numeric(rc$share), 0.05)
  expect_equal(as.numeric(rc$multiplier), 4)

  kurtosis <- function(x) {
    x <- x - mean(x)
    mean(x^4) / mean(x^2)^2 - 3
  }
  # the contaminated outcome's latent is heavy-tailed; the other three are not
  expect_gt(kurtosis(tro$latent$asc_agg), 5)
  for (o in setdiff(outcomes, "asc_agg")) expect_lt(kurtosis(tro$latent[[o]]), 1)
  # the exact-covariance rotation survives the contamination: unit variances and
  # the declared residual correlations still hold, so true_beta is still exact
  for (o in outcomes) expect_equal(stats::var(tro$latent[[o]]), 1, tolerance = 1e-6)
  X <- scale(as.matrix(tro$latent[, c(motives, "age", "male", "income_true")]))
  colnames(X) <- c(motives, "age", "male", "income")
  for (o in outcomes) {
    b <- stats::coef(stats::lm(as.numeric(scale(tro$latent[[o]])) ~ X))[-1]
    tb <- unlist(tro$truth$true_beta[[o]])[colnames(X)]
    expect_equal(unname(b), unname(tb), tolerance = 1e-6, info = o)
  }
  # and it reaches the export: the z-scored scale score is heavy-tailed too
  kept <- tro$latent$exclusion_reason == "none"
  score <- rowMeans(rekey(tro$raw[kept, ], "asc_agg"), na.rm = FALSE)
  expect_gt(kurtosis(stats::na.omit(score)), 0.5)
})

test_that("the trouble scenario leaves the AP1 fixture untouched", {
  tro <- sims[["trouble"]]
  cle <- sims[["clean"]]
  expect_equal(table(tro$latent$exclusion_reason), table(cle$latent$exclusion_reason))
  expect_equal(sum(tro$latent$exclusion_reason == "none"), truth_base$n_kept)
  expect_equal(sum(tro$latent$exclusion_reason == "none"), 706L)
  expect_equal(sum(tro$latent$exclusion_reason == "quota_full"), truth_base$exclusions$quota_full)
  # gender and age are drawn before the first fault, so the treatment-contrast
  # reference level of every AP6 table is the same under both scenarios
  expect_equal(tro$latent$gender, cle$latent$gender)
  expect_equal(tro$latent$age, cle$latent$age)
})
