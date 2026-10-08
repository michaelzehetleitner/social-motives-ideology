# Execute the CLI entry point with file I/O and fitting replaced at its boundary.
# The real runtime setup must run before preparation, but never for a proposal.
local({
  root <- normalizePath(getwd())
  while (!file.exists(file.path(root, "config", "analysis_plan.yaml"))) root <- dirname(root)
  assign("intake_runtime_root", root, envir = globalenv())
})

test_that("standalone preparation applies the existing reader safeguard before intake", {
  skip_if_not_installed("cmdstanr")
  skip_if_not_installed("data.table")
  runtime <- new.env(parent = globalenv())
  sys.source(file.path(intake_runtime_root, "R/config.R"), runtime)
  local_mocked_bindings(set_cmdstan_path = function(...) invisible(NULL), .package = "cmdstanr")
  withr::local_envvar(c(R_DATATABLE_NUM_THREADS = "2", RENV_PROJECT = "", QUARTO_R = Sys.getenv("QUARTO_R")))
  withr::local_options(mc.cores = getOption("mc.cores"))
  original_threads <- data.table::getDTthreads()
  withr::defer(data.table::setDTthreads(original_threads))
  calls <- character()
  env <- new.env(parent = baseenv())
  env$commandArgs <- function(...) c("--source=synthetic", "--input=unused.sav")
  env$list.files <- function(...) character()
  env$zm_config <- function() list(root = tempdir(), data_files = list(intake = list(approval_file = "approval.yaml")))
  env$zm_clean_intake_source <- identity
  env$zm_setup <- function() {
    calls <<- c(calls, "setup")
    runtime$zm_setup(cmdstan_path = tempdir(), cores = 1L)
  }
  env$zm_prepare_clean_intake <- function(...) {
    expect_identical(calls, "setup")
    expect_identical(Sys.getenv("R_DATATABLE_NUM_THREADS"), "1")
    expect_identical(data.table::getDTthreads(), 1L)
    calls <<- c(calls, "prepare")
    "prepared-placeholder.csv"
  }
  capture.output(sys.source(file.path(intake_runtime_root, "scripts/prepare_intake.R"), env))
  expect_identical(calls, c("setup", "prepare"))
})

test_that("column-only proposals do not initialise sampler runtime", {
  env <- new.env(parent = baseenv())
  env$commandArgs <- function(...) c("--source=synthetic", "--input=unused.sav", "--propose")
  env$list.files <- function(...) character()
  env$file.exists <- function(...) FALSE
  env$zm_config <- function() list(root = tempdir(), data_files = list(intake = list(proposal_file = "proposal.yaml")))
  env$zm_clean_intake_source <- identity
  env$zm_setup <- function(...) stop("Proposal unexpectedly initialised sampler runtime.")
  env$zm_prepare_clean_intake <- function(...) stop("Proposal unexpectedly prepared data.")
  env$read_qualtrics_export <- function(...) data.frame()
  env$assert_raw_schema <- function(...) invisible(TRUE)
  env$ap3_intake_proposal <- function(...) "column-decisions"
  written <- FALSE
  env$ap3_intake_write_proposal <- function(proposal, ...) {
    expect_identical(proposal, "column-decisions")
    written <<- TRUE
  }
  capture.output(sys.source(file.path(intake_runtime_root, "scripts/prepare_intake.R"), env))
  expect_true(written)
})
