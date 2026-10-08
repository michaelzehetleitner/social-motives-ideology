# The intake across the targets store. Every other intake test
# calls the reading functions directly or injects `analysis_inputs`; this one
# runs the production `_targets.R` itself, in a throwaway copy with a throwaway
# store, so the store's round-trip is part of the evidence: a `format = "file"`
# target hands its paths back unnamed, and the readers index them by name.

local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) {
    parent <- dirname(dir)
    if (identical(parent, dir)) stop("project root not found from ", getwd())
    dir <- parent
  }
  for (file in list.files(file.path(dir, "R"), pattern = "[.]R$", full.names = TRUE)) source(file, local = FALSE)
})

intake_store_root <- zm_root()

# A copy of the analysis project that the pipeline may write into: `_targets.R`,
# the pipeline functions, the configuration, the cleaned intake and its receipt,
# the synthetic export, the source lockfile, the report sources the Quarto targets
# require to exist (report/),
# and the three codebooks in the sibling directory `analysis_plan$meta$codebook_dir` points
# at. Nothing is written back to the repository.
intake_store_copy <- function(root = intake_store_root) {
  tmp <- withr::local_tempdir(.local_envir = parent.frame())
  analysis <- file.path(tmp, "panel", "analysis")
  dir.create(file.path(analysis, "data"), recursive = TRUE)
  file.copy(file.path(root, "_targets.R"), analysis)
  file.copy(file.path(root, "renv.lock"), analysis)
  for (dir in c("R", "config", "report")) {
    file.copy(file.path(root, dir), analysis, recursive = TRUE)
  }
  for (dir in "synthetic") {
    file.copy(file.path(root, "data", dir), file.path(analysis, "data"), recursive = TRUE)
  }
  codebook_dir <- zm_config(profile = "smoke")$meta$codebook_dir
  target <- file.path(analysis, codebook_dir)
  dir.create(target, recursive = TRUE, showWarnings = FALSE)
  file.copy(Sys.glob(file.path(root, codebook_dir, "codebook_*.csv")), target)
  # Fresh prepared input from complete synthetic rows: no sampler runs here.
  cfg <- zm_config(profile = "full", path = file.path(analysis, "config/analysis_plan.yaml"))
  book <- zm_codebook(cfg)
  raw <- read_qualtrics_export(file.path(analysis, "data/synthetic/zm_panel_synthetic.sav"))
  required <- c(unique(unlist(book$scales$item_codes)), "demo_age", "demo_hh_members", "demo_income_hh_net")
  raw <- raw[raw$survey_status == "complete" & raw$demo_gender %in% 1:2 & stats::complete.cases(raw[required]), ][1:20, ]
  input <- file.path(analysis, "fixture.sav")
  write_qualtrics_sav(raw, input, file.path(analysis, "data/synthetic/template/ZM-ASC-panel_random-data_2026-09-05.sav"))
  approval <- file.path(analysis, "approved.yaml")
  ap3_intake_write_proposal(ap3_intake_proposal(raw, cfg), cfg, approval, "synthetic")
  zm_prepare_clean_intake(input, cfg, "synthetic", approval = approval)
  normalizePath(analysis, winslash = "/", mustWork = TRUE)
}

test_that("the intake targets read the tracked paths by name through the store", {
  skip_if_not_installed("crew")
  analysis <- intake_store_copy()
  store <- file.path(analysis, "_targets_store")
  read <- function(name) targets::tar_read_raw(name, store = store)

  withr::with_dir(analysis, {
    withr::with_envvar(c(ZM_PROFILE = "smoke", ZM_DATA = "synthetic"), {
      targets::tar_make(
        names = c("analysis_inputs", "raw_path", "intake_approval_file",
                  "codebook_files", "codebook"),
        store = store, callr_function = NULL, reporter = "silent", use_crew = FALSE
      )
    })
  })

  # What the store gives back: the file target's paths without their names —
  # the behaviour the named projection exists for.
  files <- read("analysis_input_files")
  expect_type(files, "character")
  expect_length(files, 8L)
  expect_null(names(files))

  paths <- read("analysis_input_paths")
  expect_identical(names(paths), zm_analysis_input_names())
  expect_identical(names(paths), c("analysis_plan", "codebook_items", "codebook_scales",
                                   "codebook_factors", "data", "demographics", "preparation", "receipt"))
  expect_identical(unname(paths), files)

  # The bundle the readers built from the named paths.
  inputs <- read("analysis_inputs")
  expect_identical(nrow(inputs$data), 20L)
  expect_equal(nrow(inputs$demographics), 20L)
  expect_equal(inputs$preparation$exclusions$n_started, 20L)
  expect_identical(inputs$source, "synthetic")

  # The path targets name the same files the bundle was read from.
  expect_identical(read("raw_path"), unname(paths[["data"]]))
  expect_match(read("raw_path"), "data/intake/synthetic\\.csv$")
  expect_identical(read("intake_approval_file"), unname(paths[["receipt"]]))
  expect_match(read("intake_approval_file"), "data/intake/synthetic\\.yaml$")
  expect_identical(read("codebook_files"),
                   unname(paths[c("codebook_items", "codebook_scales",
                                  "codebook_factors")]))
  expect_identical(nrow(read("codebook")$items), nrow(inputs$codebook$items))

  meta <- targets::tar_meta(store = store, fields = "error")
  errored <- meta$name[!is.na(meta$error)]
  expect_identical(errored, character(0))

  # Covariate mappings are part of the tracked YAML, so a change at the same
  # path must refresh the enriched codebook even though its CSVs did not change.
  plan_path <- paths[["analysis_plan"]]
  plan <- yaml::read_yaml(plan_path)
  plan$covariates$age$z_col_known_gender <- "age_for_regressions"
  yaml::write_yaml(plan, plan_path)
  withr::with_dir(analysis, {
    withr::with_envvar(c(ZM_PROFILE = "smoke", ZM_DATA = "synthetic"), {
      targets::tar_make(names = "codebook", store = store, callr_function = NULL,
                        reporter = "silent", use_crew = FALSE)
    })
  })
  refreshed <- read("codebook")$covariates
  expect_identical(refreshed$z_col_known_gender[refreshed$covariate == "age"],
                   "age_for_regressions")
})

test_that("the calculation receipt refreshes when data or intake approval changes at the same path", {
  skip_if_not_installed("crew")
  analysis <- intake_store_copy()
  store <- file.path(analysis, "_targets_store")
  # Receipt provenance also consumes completed resampling results. Replace only
  # those expensive boundaries here; the receipt command and file dependencies
  # remain the production ones whose store invalidation this test verifies.
  writeLines(c(
    'pipeline <- source("_targets.R", local = TRUE)$value',
    'lapply(pipeline, function(target) {',
    '  if (inherits(target, "tar_target") && target$name %in% c("network_bootstrap_fits", "reliability_bootstrap_results"))',
    '    targets::tar_target_raw(target$name, quote(list()), deployment = "main") else target',
    '})'), file.path(analysis, "_targets_receipt_test.R"))
  build_receipt <- function() {
    withr::with_dir(analysis, {
      withr::with_envvar(c(ZM_PROFILE = "smoke", ZM_DATA = "synthetic"), {
        targets::tar_make(names = "calculation_receipt", store = store, script = "_targets_receipt_test.R",
                          callr_function = NULL, reporter = "silent", use_crew = FALSE)
      })
    })
    targets::tar_read_raw("calculation_receipt", store = store)
  }
  receipt <- build_receipt()
  paths <- targets::tar_read_raw("analysis_input_paths", store = store)
  original_paths <- paths
  for (input in c("data", "demographics", "preparation", "receipt")) {
    path <- paths[[input]]
    relative <- substring(path, nchar(analysis) + 2L)
    original_hash <- receipt$input_files$sha256[receipt$input_files$path == relative]
    expect_length(original_hash, 1L)
    # The receipt target only fingerprints these inputs; a trailing newline
    # changes their bytes without changing their paths or parsed contents.
    cat("\n", file = path, append = TRUE)
    receipt <- build_receipt()
    current_hash <- receipt$input_files$sha256[receipt$input_files$path == relative]
    expect_false(identical(current_hash, original_hash), info = input)
    expect_identical(current_hash, digest::digest(file = path, algo = "sha256"), info = input)
    paths <- targets::tar_read_raw("analysis_input_paths", store = store)
    expect_identical(paths, original_paths)
  }
})
