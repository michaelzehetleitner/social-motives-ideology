local({
  root <- normalizePath(getwd())
  while (!file.exists(file.path(root, "config/analysis_plan.yaml"))) {
    parent <- dirname(root)
    if (identical(parent, root)) stop("Analysis root not found")
    root <- parent
  }
  for (file in c("config.R", "ap4_reliability.R", "reproduction.R"))
    source(file.path(root, "R", file), local = FALSE)
  assign("reproduction_test_root", root, envir = .GlobalEnv)
})

make_resampling_fixture <- function() {
  root <- withr::local_tempdir(.local_envir = parent.frame())
  dir.create(file.path(root, "R"))
  files <- c("ap8_network.R", "ap8_network_helpers.R", "ap4_reliability.R")
  file.copy(file.path(reproduction_test_root, "R", files), file.path(root, "R"))
  file.copy(file.path(reproduction_test_root, "renv.lock"), file.path(root, "renv.lock"))
  cfg <- list(root = root, profile_name = "full", reliability = list(bootstrap_n = 3L,
    bootstrap_seed = 1L, interval_level = .95, omega_interval_min_success = .8))
  settings <- list(data = data.frame(a = c(1, 2, 4), b = c(3, 1, 5)), nodes = c("a", "b"),
    settings = list(B = 3L, iter = 100L, g_prior = .5, seed_base = 10L, min_success_rate = .95))
  attempt <- function(retry = FALSE) data.frame(attempt = seq_len(if (retry) 2 else 1),
    seed = seq_len(if (retry) 2 else 1) + 10L, status = if (retry) c("error", "ok") else "ok",
    message = if (retry) c("temporary error", NA_character_) else NA_character_)
  edge <- function(pip) data.frame(node_i = "a", node_j = "b", weight = .2,
    pip = pip, n_sweeps = 100L)
  network <- list(fits = list(
    list(id = 1L, succeeded = TRUE, edges = edge(0), seed = 11L, attempts = attempt()),
    list(id = 2L, succeeded = TRUE, edges = edge(1), seed = 12L, attempts = attempt(TRUE)),
    list(id = 3L, succeeded = FALSE, edges = NULL, seed = 13L, attempts = attempt(TRUE))),
    assessment = list(attempted_B = 3L, n_success = 2L, success_rate = 2/3,
      min_success_rate = .95, feasible = FALSE, status = "computationally infeasible"))
  points <- list(scales = list(a = list(responses = matrix(c(1, 2, 4, 5), ncol = 2),
    item_codes = c("i1", "i2"), coefficients = list(omega = .7), unavailable_imputation = FALSE)))
  reliability <- list(a = list(omega_interval = c(NA_real_, NA_real_),
    alpha_interval = c(.6, .8), omega_success = 1/3, n_boot_omega = 1L,
    n_boot_alpha = 3L, n_resamples = 3L))
  list(cfg = cfg, settings = settings, network = network, points = points,
    reliability = reliability, receipt = list(git_head = "original-fit-commit", built_at = "original-fit-time"))
}

test_that("compact resampling round trips preserve failures, edge extremes and original provenance", {
  withr::local_envvar(c(ZM_REPRODUCE = "full"))
  x <- make_resampling_fixture()
  # Extra implementation objects must never enter the public payload.
  x$network$fits[[1]]$fit <- list(model_frame = matrix(123, 5, 5))
  x$reliability$a$draws <- matrix(123, 5, 5)
  path <- write_resampling_results(x$network, x$settings, x$reliability, x$points, x$cfg, x$receipt)
  bundle <- readRDS(path)
  expect_identical(names(bundle$network$results$fits[[1]]), c("id", "succeeded", "edges", "seed", "attempts"))
  expect_false("draws" %in% names(bundle$reliability$results$a))
  expect_null(bundle$network$results$fits[[3]]$edges)
  expect_false(bundle$network$results$assessment$feasible)
  network <- read_saved_resampling_results(path, "network", x$settings, x$cfg)
  reliability <- read_saved_resampling_results(path, "reliability", x$points, x$cfg)
  expect_equal(vapply(network$fits[1:2], function(fit) fit$edges$pip, numeric(1)), c(0, 1))
  expect_equal(nrow(network$fits[[2]]$attempts), 2L)
  expect_equal(reliability$a$omega_interval, c(NA_real_, NA_real_))
  expect_identical(attr(network, "resampling_provenance")$calculation_receipt, x$receipt)
  withr::local_envvar(c(ZM_REPRODUCE = "models"))
  receipt <- add_reused_resampling_provenance(list(git_head = "new-model-commit"), network, reliability)
  expect_identical(receipt$git_head, "new-model-commit")
  expect_identical(receipt$reproduction_mode, "models")
  expect_identical(receipt$reused_resampling$network$calculation_receipt$git_head, "original-fit-commit")
  expect_error(write_resampling_results(network, x$settings, reliability, x$points, x$cfg, receipt), "freshly")
})

test_that("reused families reject changed model inputs, settings, profile, sources and dependencies", {
  withr::local_envvar(c(ZM_REPRODUCE = "full"))
  x <- make_resampling_fixture()
  path <- write_resampling_results(x$network, x$settings, x$reliability, x$points, x$cfg, x$receipt)
  changed <- x$settings; changed$data$a[1] <- 7
  expect_error(read_saved_resampling_results(path, "network", changed, x$cfg), "do not match")
  changed <- x$settings; changed$settings$g_prior <- .25
  expect_error(read_saved_resampling_results(path, "network", changed, x$cfg), "do not match")
  cfg <- x$cfg; cfg$profile_name <- "smoke"
  expect_error(read_saved_resampling_results(path, "network", x$settings, cfg), "do not match")
  changed <- x$points; changed$scales$a$responses[1, 1] <- 7
  expect_error(read_saved_resampling_results(path, "reliability", changed, x$cfg), "do not match")
  cfg <- x$cfg; cfg$reliability$bootstrap_n <- 1000L
  expect_error(read_saved_resampling_results(path, "reliability", x$points, cfg), "do not match")
  # A regression edit has no bearing on either reused family.
  writeLines("changed regression", file.path(x$cfg$root, "R/ap6_regressions.R"))
  expect_silent(read_saved_resampling_results(path, "network", x$settings, x$cfg))
  cat("\n# changed network source", file = file.path(x$cfg$root, "R/ap8_network.R"), append = TRUE)
  expect_error(read_saved_resampling_results(path, "network", x$settings, x$cfg), "do not match")
  expect_silent(read_saved_resampling_results(path, "reliability", x$points, x$cfg))
  lock <- jsonlite::read_json(file.path(x$cfg$root, "renv.lock"), simplifyVector = FALSE)
  lock$Packages$psych$Version <- "changed"
  jsonlite::write_json(lock, file.path(x$cfg$root, "renv.lock"), auto_unbox = TRUE)
  expect_error(read_saved_resampling_results(path, "reliability", x$points, x$cfg), "do not match")
  bundle <- readRDS(path); bundle$network$results$assessment$n_success <- 3L; saveRDS(bundle, path)
  expect_error(read_saved_resampling_results(path, "network", x$settings, x$cfg), "content check")
})

test_that("the reliability split keeps the existing fallback input without recomputation", {
  x <- make_resampling_fixture()
  calculate <- function(reliability, analysis_plan) x$reliability
  env <- new.env(parent = environment(add_bootstrap_intervals))
  env$calculate_reliability_bootstrap <- calculate
  wrapper <- add_bootstrap_intervals; environment(wrapper) <- env
  result <- wrapper(x$points, x$cfg)
  expect_identical(result$scales, x$points$scales)
  expect_identical(result$bootstrap, x$reliability)
})

test_that("saved reliability summaries retain finite intervals and unavailable resamples", {
  x <- make_resampling_fixture()
  env <- new.env(parent = environment(calculate_reliability_bootstrap))
  env$ap4_participant_bootstrap_draws <- function(...) matrix(
    c(.1, .2, NA_real_, .5, .9, .8), nrow = 2, dimnames = list(c("omega", "alpha"), NULL))
  calculate <- calculate_reliability_bootstrap; environment(calculate) <- env
  result <- calculate(x$points, x$cfg)$a
  expect_equal(result$omega_interval, c(.12, .88))
  expect_equal(result$alpha_interval, c(.215, .785))
  expect_equal(result$omega_success, 2/3)
  expect_equal(result$n_boot_omega, 2L)
  expect_equal(result$n_boot_alpha, 3L)
  x$points$scales$a$unavailable_imputation <- TRUE
  missing <- calculate(x$points, x$cfg)$a
  expect_true(all(is.na(missing$omega_interval)))
  expect_equal(missing$n_boot_omega, 0L)
})

test_that("execution validates reuse before rerunning models and invalidates only its dedicated store", {
  withr::local_envvar(c(ZM_PROFILE = NA_character_))
  root <- withr::local_tempdir()
  dir.create(file.path(root, "scripts"))
  writeLines("", file.path(root, "_targets.R"))
  writeLines("", file.path(root, "scripts/render_saved_report.R"))
  calls <- list()
  env <- new.env(parent = environment(execute_reproduction_step))
  env$system2 <- function(command, args) {
    calls[[length(calls) + 1L]] <<- list(args = args, mode = Sys.getenv("ZM_REPRODUCE"),
      profile = Sys.getenv("ZM_PROFILE"), project = Sys.getenv("TAR_PROJECT"))
    0L
  }
  execute <- execute_reproduction_step; environment(execute) <- env
  execute(build_reproduction_steps("report")[[1L]], root)
  expect_length(calls[[1L]]$args, 1L)
  expect_match(calls[[1L]]$args, "render_saved_report.R", fixed = TRUE)
  execute(build_reproduction_steps("models")[[1L]], root)
  call <- calls[[2L]]
  expect_identical(call$mode, "models")
  expect_identical(call$profile, "full")
  expect_identical(call$project, "main")
  code <- call$args[[2L]]
  expect_match(code, "tar_invalidate", fixed = TRUE)
  expect_match(code, "_targets_reproduce_models", fixed = TRUE)
  expect_match(code, "network_bootstrap_fits", fixed = TRUE)
  expect_match(code, "reliability_bootstrap_results", fixed = TRUE)
  expect_equal(lengths(regmatches(code, gregexpr("tar_make", code, fixed = TRUE))), 2L)
  expect_true(regexpr("network_bootstrap_fits", code, fixed = TRUE) <
                max(gregexpr("tar_make", code, fixed = TRUE)[[1L]]))
  withr::with_envvar(c(ZM_PROFILE = "smoke"),
    execute(build_reproduction_steps("models")[[1L]], root))
  expect_identical(calls[[3L]]$profile, "smoke")
  writeLines("", file.path(root, "_targets_prior_recovery.R"))
  withr::with_envvar(c(ZM_PROFILE = "smoke"),
    execute(build_reproduction_steps("full")[[2L]], root))
  expect_identical(calls[[4L]]$project, "prior_recovery_smoke")
  expect_identical(calls[[4L]]$profile, "smoke")
  withr::with_envvar(c(ZM_PROFILE = "unknown"),
    expect_error(execute(build_reproduction_steps("models")[[1L]], root), "ZM_PROFILE"))
  env$system2 <- function(...) 1L
  expect_error(execute(build_reproduction_steps("models")[[1L]], root), "Reproduction failed")
})

test_that("three command levels select exact layers and preserve ordinary stores", {
  report <- build_reproduction_steps("report")
  expect_length(report, 1L)
  expect_identical(report[[1]]$script, "scripts/render_saved_report.R")
  expect_identical(report[[1]]$kind, "report")
  models <- build_reproduction_steps("models")
  expect_length(models, 1L)
  expect_identical(models[[1]]$mode, "models")
  expect_identical(models[[1]]$preflight, c("network_bootstrap_fits", "reliability_bootstrap_results"))
  full <- build_reproduction_steps("full")
  expect_identical(vapply(full, `[[`, character(1), "script"), c("_targets.R", "_targets_prior_recovery.R"))
  expect_identical(full[[2]]$project, "prior_recovery")
  stores <- c(models[[1]]$store, vapply(full, `[[`, character(1), "store"))
  expect_identical(stores, c("_targets_reproduce_models", "_targets_reproduce_full", "_targets_reproduce_prior_recovery"))
  expect_error(build_reproduction_steps("quick"), "Choose one")
  expect_error(read_reproduction_mode("report"), "render_saved|saved reports")
  calls <- list(); runner <- function(step, root) { calls[[length(calls) + 1L]] <<- step }
  root <- withr::local_tempdir()
  run_reproduction("report", root, runner)
  expect_identical(calls, report)
  expect_error(run_reproduction("models", root, runner), "missing")
  dir.create(file.path(root, "data/derived"), recursive = TRUE)
  file.create(file.path(root, "data/derived/resampling_results.rds"))
  calls <- list(); run_reproduction("models", root, runner); run_reproduction("models", root, runner)
  expect_length(calls, 2L)
  calls <- list(); run_reproduction("full", root, runner)
  expect_identical(calls, full)
})
