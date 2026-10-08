local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) {
    parent <- dirname(dir)
    if (identical(parent, dir)) stop("project root not found from ", getwd())
    dir <- parent
  }
  source(file.path(dir, "R", "calculation_receipt.R"), local = FALSE)
})

test_that("the receipt records the configuration and relative SHA256 fingerprints", {
  withr::local_envvar(c(ZM_SOURCE_COMMIT = NA, ZM_SOURCE_DIRTY = NA))
  base <- withr::local_tempdir()
  root <- file.path(base, "analysis space")
  dir.create(root)
  dir.create(file.path(root, "R"))
  input <- file.path(base, "input data.csv")
  source_file <- file.path(root, "R", "calculation.R")
  writeBin(charToRaw("abc"), input)
  writeLines("result <- 1", source_file)
  analysis_plan <- list(root = root, profile_name = "full", network = list(B = 200L))
  before <- Sys.time()
  receipt <- record_calculation_receipt("R/calculation.R", c(input, "../input data.csv"),
                                        analysis_plan)
  after <- Sys.time()

  expect_identical(receipt$cfg, analysis_plan)
  expect_identical(receipt$hash_algorithm, "sha256")
  expect_identical(receipt$input_files$path, "../input data.csv")
  expect_identical(receipt$source_files$path, "R/calculation.R")
  expect_identical(receipt$input_files$sha256,
                   "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")
  expect_match(receipt$source_files$sha256, "^[a-f0-9]{64}$")
  expect_identical(receipt$r_version, R.version.string)
  built <- as.POSIXct(receipt$built_at, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  expect_gte(as.numeric(built), floor(as.numeric(before)))
  expect_lte(as.numeric(built), as.numeric(after))
  expect_true(is.na(receipt$git_head))
  expect_true(is.na(receipt$git_dirty))

  original_hash <- receipt$source_files$sha256
  writeLines("result <- 2", source_file)
  changed <- record_calculation_receipt(source_file, input, analysis_plan)
  expect_false(identical(changed$source_files$sha256, original_hash))
})

test_that("the receipt records the Git HEAD and the dirty state", {
  # Container provenance must never conceal changes in an ordinary checkout.
  withr::local_envvar(c(ZM_SOURCE_COMMIT = strrep("a", 40), ZM_SOURCE_DIRTY = "false"))
  skip_if(!nzchar(Sys.which("git")), "Git is not available")
  root <- withr::local_tempdir(pattern = "receipt git ")
  git <- function(...) {
    args <- c("-C", root, ...)
    result <- suppressWarnings(system2("git", vapply(args, shQuote, character(1)),
                                       stdout = TRUE, stderr = FALSE))
    expect_null(attr(result, "status"))
    unname(result)
  }
  git("init", "--quiet")
  path <- file.path(root, "calculation.R")
  writeLines("x <- 1", path)
  git("add", "calculation.R")
  git("-c", "user.name=Receipt test", "-c", "user.email=receipt-test@example.invalid",
      "-c", "commit.gpgsign=false", "commit", "--quiet", "-m", "Test fixture")
  analysis_plan <- list(root = root, profile_name = "smoke")
  clean <- record_calculation_receipt(path, character(), analysis_plan)
  expect_identical(clean$git_head, git("rev-parse", "HEAD"))
  expect_false(clean$git_dirty)
  expect_identical(clean$input_files,
                   data.frame(path = character(), sha256 = character()))

  writeLines("x <- 2", path)
  dirty <- record_calculation_receipt(path, character(), analysis_plan)
  expect_true(dirty$git_dirty)
  expect_identical(dirty$git_head, clean$git_head)
  expect_named(dirty, c("cfg", "input_files", "source_files", "hash_algorithm",
                                "git_head", "git_dirty", "built_at", "r_version"))
})

# A source snapshot without Git needs an explicit commit and dirty state.
test_that("container provenance fills missing Git metadata without changing fingerprints", {
  root <- withr::local_tempdir()
  path <- file.path(root, "calculation.R")
  writeLines("x <- 1", path)
  plan <- list(root = root, profile_name = "full")
  withr::local_envvar(c(ZM_SOURCE_COMMIT = strrep("b", 40), ZM_SOURCE_DIRTY = "false"))
  clean <- record_calculation_receipt(path, character(), plan)
  expect_identical(clean$git_head, strrep("b", 40))
  expect_false(clean$git_dirty)
  expect_identical(clean$source_files$sha256, digest::digest(file = path, algo = "sha256"))
  Sys.setenv(ZM_SOURCE_DIRTY = "true")
  dirty <- record_calculation_receipt(path, character(), plan)
  expect_true(dirty$git_dirty)
  expect_identical(dirty$source_files, clean$source_files)
  for (values in list(c("", "false"), c("abc1234", "false"),
                     c(strrep("b", 40), ""), c(strrep("b", 40), "unknown"))) {
    Sys.setenv(ZM_SOURCE_COMMIT = values[1], ZM_SOURCE_DIRTY = values[2])
    expect_error(record_calculation_receipt(path, character(), plan), "Without Git")
  }
})

# ---- the provenance the report states (M1: the commit of the code) ----------

local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) dir <- dirname(dir)
  for (f in c("config.R", "report_supplement_software.R", "report_helpers.R")) {
    source(file.path(dir, "R", f), local = FALSE)
  }
})

provenance_receipt <- function(git_head = strrep("ab12", 10), git_dirty = FALSE) {
  list(
    cfg = list(profile_name = "full"),
    input_files = data.frame(
      path = c("data/clean/zm_panel_clean.csv", "intake/intake_approved.yaml",
               "../preregistration/codebook_items.csv"),
      sha256 = c(strrep("d", 64), strrep("e", 64), strrep("f", 64)),
      stringsAsFactors = FALSE
    ),
    source_files = data.frame(
      path = c("_targets.R", "config/analysis_plan.yaml", "renv.lock"),
      sha256 = c(strrep("1", 64), strrep("2", 64), strrep("3", 64)),
      stringsAsFactors = FALSE
    ),
    hash_algorithm = "sha256", git_head = git_head, git_dirty = git_dirty,
    built_at = "2026-10-05T14:03:11Z", r_version = "R version 4.5.1 (2025-06-13)"
  )
}

test_that("the provenance names the lockfile, the plan and the data file from the receipt", {
  provenance <- assemble_report_provenance(provenance_receipt(), "synthetic")
  expect_identical(provenance$git_head, strrep("ab12", 10))
  expect_false(provenance$git_dirty)
  expect_identical(provenance$profile, "full")
  expect_identical(provenance$data_source, "synthetic")
  expect_identical(provenance$files$role, c("lockfile", "analysis plan", "input data"))
  expect_identical(provenance$files$path,
                   c("renv.lock", "config/analysis_plan.yaml", "data/clean/zm_panel_clean.csv"))
  expect_identical(provenance$files$sha256, c(strrep("3", 64), strrep("2", 64), strrep("d", 64)))
  # a receipt without a lockfile leaves that row unrecorded instead of guessing
  bare <- provenance_receipt()
  bare$source_files <- bare$source_files[bare$source_files$path != "renv.lock", ]
  expect_true(is.na(assemble_report_provenance(bare, "real")$files$sha256[[1L]]))
})

test_that("the provenance paragraph states the commit, whether the code changed, the data source and the fingerprints", {
  text <- rh_provenance_text(assemble_report_provenance(provenance_receipt(), "synthetic"))
  expect_match(text, paste0("at commit ab12ab1 (", strrep("ab12", 10), "), unchanged relative to that commit, on the synthetic preregistration data."), fixed = TRUE)
  expect_match(text, "The lockfile `renv.lock`, the analysis plan `config/analysis_plan.yaml` and the input data `data/clean/zm_panel_clean.csv` are identified by the first twelve characters of their SHA-256 fingerprints, 333333333333, 222222222222 and dddddddddddd.", fixed = TRUE)
  expect_false(grepl("profile|working tree|UTC|R version", text))
  dirty <- rh_provenance_text(assemble_report_provenance(provenance_receipt(git_dirty = TRUE), "real"))
  expect_match(dirty, "with changes to the analysis code that are not in that commit, on the empirical survey data", fixed = TRUE)
  none <- rh_provenance_text(assemble_report_provenance(provenance_receipt(git_head = NA_character_, git_dirty = NA), "real"))
  expect_match(none, "at a commit that was not recorded on the empirical survey data", fixed = TRUE)
})


test_that("report provenance retains the original receipts for reused results", {
  receipt <- provenance_receipt()
  receipt$reproduction_mode <- "models"
  receipt$reused_resampling <- list(
    network = list(calculation_receipt = provenance_receipt(git_head = strrep("b", 40))),
    reliability = list(calculation_receipt = provenance_receipt(git_head = strrep("c", 40))))
  report <- assemble_report_provenance(receipt, "synthetic")
  expect_identical(report$git_head, receipt$git_head)
  expect_identical(report$reproduction_mode, "models")
  expect_identical(report$reused_resampling, receipt$reused_resampling)
})
