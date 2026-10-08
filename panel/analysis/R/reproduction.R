# Three execution levels. Only the two expensive resampling families cross
# the saved-results boundary; ordinary model fits are never cached here.

read_reproduction_mode <- function(value = Sys.getenv("ZM_REPRODUCE", "full")) {
  if (length(value) != 1L || is.na(value) || !value %in% c("full", "models"))
    stop("ZM_REPRODUCE must be 'full' or 'models'. Use reproduce.R report for saved reports.")
  value
}

resampling_results_path <- function(analysis_plan) {
  file.path(analysis_plan$root, "data", "derived", "resampling_results.rds")
}

# The model input itself binds all upstream preparation, codebook mappings and
# missing-data selections. Source and dependency checks below concern only the
# reused family, so editing regression code or report wording does not invalidate it.
build_resampling_fingerprint <- function(family, input, analysis_plan) {
  if (identical(family, "network")) {
    values <- list(data = as.matrix(input$data), nodes = input$nodes)
    settings <- input$settings
    sources <- c("ap8_network.R", "ap8_network_helpers.R")
    packages <- c("BDgraph", "easybgm", "Matrix", "dplyr", "tibble")
  } else if (identical(family, "reliability")) {
    values <- lapply(input$scales, function(scale) list(
      responses = as.matrix(scale$responses), item_codes = scale$item_codes,
      omega_available = is.finite(scale$coefficients$omega),
      unavailable_imputation = isTRUE(scale$unavailable_imputation)))
    settings <- analysis_plan$reliability
    sources <- "ap4_reliability.R"
    packages <- c("psych", "GPArotation", "mnormt", "dplyr", "tibble")
  } else stop("Unknown resampling family: ", family)
  source_paths <- file.path(analysis_plan$root, "R", sources)
  lock <- jsonlite::read_json(file.path(analysis_plan$root, "renv.lock"), simplifyVector = FALSE)
  dependencies <- stats::setNames(lapply(packages, function(package) {
    entry <- lock$Packages[[package]]
    if (is.null(entry)) stop("Dependency missing from renv.lock: ", package)
    list(installed = as.character(utils::packageVersion(package)), locked = entry)
  }), packages)
  list(family = family, profile = analysis_plan$profile_name,
    input_sha256 = digest::digest(values, algo = "sha256"),
    settings_sha256 = digest::digest(settings, algo = "sha256"),
    source_sha256 = stats::setNames(vapply(source_paths, function(path)
      digest::digest(file = path, algo = "sha256"), character(1)), sources),
    dependencies = dependencies, r_version = as.character(getRversion()))
}

# Explicit construction prevents a fitted object, response matrix, or posterior
# draw array from entering a public bundle through an extra list element.
compact_network_resampling <- function(value) {
  fits <- lapply(value$fits, function(fit) {
    edges <- if (is.null(fit$edges)) NULL else tibble::tibble(
      node_i = as.character(fit$edges$node_i), node_j = as.character(fit$edges$node_j),
      weight = as.numeric(fit$edges$weight), pip = as.numeric(fit$edges$pip),
      n_sweeps = as.integer(fit$edges$n_sweeps))
    attempts <- tibble::tibble(attempt = as.integer(fit$attempts$attempt),
      seed = as.integer(fit$attempts$seed), status = as.character(fit$attempts$status),
      message = as.character(fit$attempts$message))
    list(id = as.integer(fit$id), succeeded = isTRUE(fit$succeeded), edges = edges,
      seed = as.integer(fit$seed), attempts = attempts)
  })
  x <- value$assessment
  list(fits = fits, assessment = list(attempted_B = as.integer(x$attempted_B),
    n_success = as.integer(x$n_success), success_rate = as.numeric(x$success_rate),
    min_success_rate = as.numeric(x$min_success_rate), feasible = isTRUE(x$feasible),
    status = as.character(x$status)))
}

compact_reliability_resampling <- function(value) {
  lapply(value, function(x) list(omega_interval = as.numeric(x$omega_interval),
    alpha_interval = as.numeric(x$alpha_interval), omega_success = as.numeric(x$omega_success),
    n_boot_omega = as.integer(x$n_boot_omega), n_boot_alpha = as.integer(x$n_boot_alpha),
    n_resamples = as.integer(x$n_resamples)))
}

write_resampling_results <- function(network, network_settings, reliability, reliability_points,
                                     analysis_plan, calculation_receipt,
                                     path = resampling_results_path(analysis_plan)) {
  if (read_reproduction_mode() != "full" ||
      !is.null(attr(network, "resampling_provenance", exact = TRUE)) ||
      !is.null(attr(reliability, "resampling_provenance", exact = TRUE)))
    stop("Only freshly calculated resampling results may replace the saved bundle.")
  if (!is.null(calculation_receipt$cfg$root)) calculation_receipt$cfg$root <- "."
  pack <- function(family, result, input) list(
    fingerprint = build_resampling_fingerprint(family, input, analysis_plan),
    results = result, results_sha256 = digest::digest(result, algo = "sha256"),
    calculation_receipt = calculation_receipt)
  bundle <- list(format = "zm-resampling-results-v1",
    network = pack("network", compact_network_resampling(network), network_settings),
    reliability = pack("reliability", compact_reliability_resampling(reliability), reliability_points))
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(".resampling-", tmpdir = dirname(path))
  on.exit(unlink(temporary), add = TRUE)
  saveRDS(bundle, temporary, version = 3)
  if (!identical(readRDS(temporary), bundle)) stop("Resampling bundle read-back failed.")
  if (!file.rename(temporary, path)) stop("Could not finish writing resampling results.")
  path
}

read_saved_resampling_results <- function(path, family, input, analysis_plan) {
  unavailable <- function(reason) stop("Saved ", family, " resampling results ", reason,
    ". Run 'Rscript scripts/reproduce.R full' to regenerate them.", call. = FALSE)
  if (!file.exists(path)) unavailable("are missing")
  bundle <- readRDS(path)
  if (!identical(bundle$format, "zm-resampling-results-v1") || is.null(bundle[[family]]))
    unavailable("have an unsupported format")
  saved <- bundle[[family]]
  if (!identical(saved$results_sha256, digest::digest(saved$results, algo = "sha256")))
    unavailable("failed their content check")
  expected <- build_resampling_fingerprint(family, input, analysis_plan)
  if (!identical(saved$fingerprint, expected))
    unavailable("do not match the current inputs, settings, source or dependencies")
  result <- saved$results
  attr(result, "resampling_provenance") <- list(family = family,
    fingerprint = saved$fingerprint, results_sha256 = saved$results_sha256,
    calculation_receipt = saved$calculation_receipt)
  result
}

add_reused_resampling_provenance <- function(receipt, network, reliability) {
  reused <- list(network = attr(network, "resampling_provenance", exact = TRUE),
                 reliability = attr(reliability, "resampling_provenance", exact = TRUE))
  receipt$reproduction_mode <- read_reproduction_mode()
  receipt$reused_resampling <- Filter(Negate(is.null), reused)
  receipt
}

build_reproduction_steps <- function(mode) {
  if (length(mode) != 1L || is.na(mode) || !mode %in% c("report", "models", "full"))
    stop("Choose one reproduction level: report, models, or full.")
  if (mode == "report") return(list(list(kind = "report", script = "scripts/render_saved_report.R")))
  steps <- list(list(kind = "targets", script = "_targets.R",
    store = paste0("_targets_reproduce_", mode), mode = mode, project = "main",
    preflight = if (mode == "models") c("network_bootstrap_fits", "reliability_bootstrap_results") else character()))
  if (mode == "full") steps <- c(steps, list(list(kind = "targets",
    script = "_targets_prior_recovery.R", store = "_targets_reproduce_prior_recovery",
    mode = "full", project = "prior_recovery", preflight = character())))
  steps
}

# Each invocation reruns its chosen layer in reserved reproduction stores.
# Invalidating those stores retains files and leaves every ordinary/user store alone.
execute_reproduction_step <- function(step, root) {
  rscript <- file.path(R.home("bin"), "Rscript")
  if (!file.exists(file.path(root, step$script))) stop("Missing reproduction script: ", step$script)
  if (step$kind == "report") {
    status <- withr::with_dir(root, system2(rscript, shQuote(step$script)))
  } else {
    profile <- Sys.getenv("ZM_PROFILE", "full")
    if (!nzchar(profile)) profile <- "full"
    if (!profile %in% c("full", "smoke")) stop("ZM_PROFILE must be 'full' or 'smoke'.")
    project <- if (step$project == "prior_recovery" && profile == "smoke")
      "prior_recovery_smoke" else step$project
    literal <- function(x) paste(deparse(x), collapse = "")
    make <- paste0("targets::tar_make(script = ", literal(step$script),
      ", store = ", literal(step$store), ", callr_function = NULL")
    check <- paste0("errors <- targets::tar_meta(fields = error, store = ", literal(step$store),
      "); if (any(!is.na(errors$error))) stop(paste(errors$error[!is.na(errors$error)], collapse = '\\n'))")
    code <- c("source('R/config.R'); zm_setup()",
      paste0("if (file.exists(", literal(file.path(step$store, "meta", "meta")),
        ")) targets::tar_invalidate(tidyselect::everything(), store = ", literal(step$store), ")"))
    if (length(step$preflight)) code <- c(code,
      paste0(make, ", names = tidyselect::all_of(", literal(step$preflight), "))"), check)
    code <- c(code, paste0(make, ")"), check)
    status <- withr::with_envvar(c(ZM_REPRODUCE = step$mode, ZM_PROFILE = profile, TAR_PROJECT = project),
      withr::with_dir(root, system2(rscript, c("-e", shQuote(paste(code, collapse = "; "))))))
  }
  if (!identical(status, 0L)) stop("Reproduction failed in ", step$script, "; inspect the preceding diagnostics.")
  invisible(NULL)
}

run_reproduction <- function(mode, root = zm_root(), runner = execute_reproduction_step) {
  steps <- build_reproduction_steps(mode)
  if (identical(mode, "models") && !file.exists(file.path(root, "data/derived/resampling_results.rds")))
    stop("Saved resampling results are missing. Run 'Rscript scripts/reproduce.R full' first.")
  for (step in steps) runner(step, root)
  invisible(steps)
}
