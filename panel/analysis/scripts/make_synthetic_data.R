# Regenerate data/synthetic/* from config/simulation_truth.yaml
#
# Run from anywhere inside the project:
#   /opt/homebrew/bin/Rscript scripts/make_synthetic_data.R
# Writes the synthetic Qualtrics export as a SAV file (the format the real export
# arrives in, with the column attributes of the random-data template export in
# data/synthetic/template/), the latent ground-truth scores per respondent, and
# a README with the row counts.
# Every number in the README is computed from the generated data.
#
# Arguments (both optional):
#   --scenario=clean|trouble  which scenario of config/simulation_truth.yaml to
#                             realise; the default is `scenarios.export` in that
#                             file, i.e. `trouble` — the export the pipeline runs
#                             on. `clean` reproduces the base truth exactly.
#   --out=DIR                 write the three files into DIR instead of
#                             data/synthetic/ (a relative path is taken from the
#                             project root). For trying a scenario out without
#                             disturbing the pipeline's input.
#
# The prior-recovery simulation (R/prior_recovery.R) is NOT affected by
# --scenario: it reads the base `true_beta` of the truth file, so the M3 numbers
# in the preregistration stay anchored to the clean truth.

local({
  script_dir <- tryCatch(
    dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))),
    error = function(e) getwd()
  )
  if (is.na(script_dir) || !nzchar(script_dir)) script_dir <- getwd()
  start <- if (file.exists(file.path(script_dir, "..", "config", "analysis_plan.yaml"))) {
    file.path(script_dir, "..")
  } else {
    getwd()
  }
  source(file.path(start, "R", "config.R"))
  root <- zm_root(start)
  source(file.path(root, "R", "io_qualtrics.R"))
  source(file.path(root, "R", "simulate_testdata.R"))

  args <- commandArgs(trailingOnly = TRUE)
  arg <- function(name, default) {
    hit <- grep(paste0("^--", name, "="), args, value = TRUE)
    if (length(hit) == 0) return(default)
    sub(paste0("^--", name, "="), "", hit[1])
  }
  unknown <- setdiff(args, grep("^--(scenario|out)=", args, value = TRUE))
  if (length(unknown) > 0) stop("Unknown argument(s): ", paste(unknown, collapse = ", "),
                                ". Use --scenario=clean|trouble and/or --out=DIR.")

  analysis_plan <- zm_config(path = file.path(root, "config", "analysis_plan.yaml"))
  truth <- zm_truth(file.path(root, "config", "simulation_truth.yaml"))
  codebook <- zm_codebook(analysis_plan)
  scenario <- arg("scenario", truth$scenarios$export)
  overrides <- truth$scenarios[[scenario]]$true_beta_overrides
  truth <- zm_truth_scenario(truth, scenario)
  sim <- simulate_zm_panel(truth = truth, codebook = codebook, analysis_plan = analysis_plan, seed = truth$seed)
  truth <- sim$truth

  out <- arg("out", "")
  out_dir <- if (!nzchar(out)) {
    file.path(root, "data", "synthetic")
  } else if (grepl("^(/|~)", out)) {
    out
  } else {
    file.path(root, out)
  }
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  sav_path <- file.path(out_dir, "zm_panel_synthetic.sav")
  latent_path <- file.path(out_dir, "zm_panel_synthetic_latent.csv")
  template <- file.path(root, "data", "synthetic", "template", "ZM-ASC-panel_random-data_2026-09-05.sav")
  write_qualtrics_sav(sim$raw, sav_path, template)
  readr::write_csv(sim$latent, latent_path, na = "", progress = FALSE)

  # round trip check: the file must read back as the schema expects
  back <- assert_raw_schema(read_qualtrics_export(sav_path))
  stopifnot(nrow(back) == truth$n_total, identical(names(back), zm_raw_schema()$name))

  raw <- sim$raw
  reason_counts <- table(sim$latent$exclusion_reason)
  status_counts <- table(ifelse(raw$survey_status == "", "(empty: incomplete)", raw$survey_status))
  quota_counts <- table(raw$quota_group[raw$survey_status == analysis_plan$exclusions$survey_status_complete])
  # AP1 semantics, not "no missing value anywhere": a missing age is retained
  # (criterion 3), a missing gender is no criterion at all, and a scenario may
  # plant either among the kept rows.
  n_valid <- sum(
    raw$survey_status == analysis_plan$exclusions$survey_status_complete &
      raw$consent_check == analysis_plan$exclusions$consent_required_value &
      (is.na(raw$demo_age) | (raw$demo_age >= analysis_plan$exclusions$min_age &
        raw$demo_age <= analysis_plan$exclusions$max_age)) &
      raw$attentioncheck_1 == analysis_plan$exclusions$attention_check_1_correct &
      raw$attentioncheck_2 == analysis_plan$exclusions$attention_check_2_correct,
    na.rm = TRUE
  )
  planted <- if (is.null(truth$missingness)) NULL else {
    vapply(truth$missingness$cells, function(c) {
      sprintf("| `%s` | %d | %s |", c$column, as.integer(c$n_missing), c$affects)
    }, "")
  }
  fmt_table <- function(tb) paste0("| ", names(tb), " | ", as.integer(tb), " |", collapse = "\n")

  readme <- c(
    "# Synthetic raw export",
    "",
    "Generated test data for the ZM panel analysis pipeline. Not real responses.",
    "Produced by `scripts/make_synthetic_data.R` from `config/simulation_truth.yaml`",
    "(ground truth) with `R/simulate_testdata.R`; the export layout follows",
    "`R/io_qualtrics.R::zm_raw_schema()` (97 columns), and the file carries the",
    "column attributes of the Qualtrics random-data export in `template/`.",
    "",
    "## Files",
    "",
    "- `zm_panel_synthetic.sav` — the raw export as a Qualtrics SAV file",
    sprintf("  (%d rows, %d columns). Read it with `read_qualtrics_export()`.", nrow(raw), ncol(raw)),
    "- `zm_panel_synthetic_latent.csv` — one row per respondent (same order as",
    "  the export, `respondent_row` = row number, `ResponseId` for joining):",
    "  the nine latent scores the items were generated from, `age`, `male`,",
    "  `gender`, `income_true`, and `exclusion_reason`.",
    "",
    "## How it was generated",
    "",
    sprintf("- Seed: `%d` (from `simulation_truth.yaml`); regenerate with", truth$seed),
    "  `/opt/homebrew/bin/Rscript scripts/make_synthetic_data.R` from `panel/analysis`.",
    "- Latent motives follow the declared motive correlations; outcome latents",
    "  are `X beta + residual` with the declared partial coefficients and residual",
    "  correlations (variance 1 per outcome). Sample moments are exact: residuals",
    "  are orthogonal to the predictors, so an OLS of each outcome latent on the",
    "  standardised predictors returns `true_beta` exactly.",
    sprintf("- Items: loading %.2f on the scale's latent, location and spread per scale,", truth$measurement$loading),
    sprintf("  rounded and clamped to %d..%d; reverse-keyed items stored as displayed", analysis_plan$scales$response_min, analysis_plan$scales$response_max),
    "  (`(min + max) - x`), like the live survey.",
    "- Party scalometers, `possibly_left` / `possibly_conservative` and",
    "  `quota_group` follow the M4 rule (liked = scalometer > 0).",
    "- Exclusion rows are generated in exact counts and shaped like the survey",
    "  flow produces them; they are interleaved with valid rows by a seeded shuffle.",
    "",
    "## Scenario",
    "",
    sprintf("This export realises the **`%s`** scenario of `simulation_truth.yaml`", truth$scenario),
    sprintf("(regenerate it with `--scenario=%s`). The truth file declares it as:", truth$scenario),
    "",
    paste0("> ", trimws(truth$scenario_description %||% "(no description)")),
    "",
    if (is.null(overrides) && is.null(truth$missingness) && is.null(truth$cross_loadings) &&
        is.null(truth$residual_contamination)) {
      "No fault is planted in this scenario."
    } else {
      "Planted faults:"
    },
    "",
    unlist(lapply(names(overrides), function(o) {
      vapply(names(overrides[[o]]), function(p) sprintf(
        "- `true_beta` override: `%s ~ %s` = %s (preregistration Table 3 predicts `%s`).",
        o, p, format(as.numeric(overrides[[o]][[p]])),
        {
          v <- analysis_plan$predictions$table[[o]][[p]]
          if (is.null(v) || !nzchar(v)) "(no prediction)" else v
        }
      ), "")
    })),
    if (!is.null(truth$residual_contamination)) sprintf(
      "- Contaminated residual: %.0f%% of the `%s` residual draws multiplied by %s.",
      100 * as.numeric(truth$residual_contamination$share),
      truth$residual_contamination$outcome, truth$residual_contamination$multiplier
    ),
    if (!is.null(truth$cross_loadings)) vapply(truth$cross_loadings, function(e) sprintf(
      "- Cross-loading: `%s` also loads %.2f on the `%s` latent.", e$item, as.numeric(e$loading), e$factor
    ), ""),
    if (!is.null(truth$missingness)) c(
      "- Blanked cells, applied to the finished export among the rows AP1 keeps",
      "  (so AP1 excludes none of them and AP3's missing-data rule has to act):",
      "",
      "| column | blanked | model variable it makes missing |", "|---|---|---|",
      planted
    ),
    "",
    "## Row counts",
    "",
    sprintf("Total rows: %d. Valid complete rows (complete, consented, age %d–%d, both",
            nrow(raw), analysis_plan$exclusions$min_age, analysis_plan$exclusions$max_age),
    sprintf("attention checks correct, including divers): %d.", n_valid),
    "",
    "Exclusion reason (from the latent file):",
    "",
    "| reason | n |", "|---|---|",
    fmt_table(reason_counts),
    "",
    "`survey_status` in the export:",
    "",
    "| survey_status | n |", "|---|---|",
    fmt_table(status_counts),
    "",
    "Quota group among rows with `survey_status = complete`:",
    "",
    "| quota_group | n |", "|---|---|",
    fmt_table(quota_counts),
    ""
  )
  writeLines(readme, file.path(out_dir, "README.md"))
  message("Wrote ", sav_path, ", ", latent_path, " and README.md (", nrow(raw),
          " rows, scenario '", truth$scenario, "').")
})
