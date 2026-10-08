# Configuration: analysis plan, codebook, simulation truth, runtime setup
#
# Everything numeric that the pipeline uses is read from config/analysis_plan.yaml
# (analysis decisions) or config/simulation_truth.yaml (ground truth of the
# synthetic data). This file only loads, validates and lightly reshapes those
# files; it never introduces a decision of its own.

#' Locate the project root
#'
#' Walks up from `start` until a directory containing
#' `config/analysis_plan.yaml` is found. The environment variable
#' `ZM_PROJECT_ROOT` overrides the search.
#'
#' @param start Directory to start from (default: working directory).
#' @return Absolute path of the project root (character scalar).
zm_root <- function(start = getwd()) {
  env_root <- Sys.getenv("ZM_PROJECT_ROOT", "")
  if (nzchar(env_root)) {
    return(normalizePath(env_root, mustWork = TRUE))
  }
  dir <- normalizePath(start, mustWork = TRUE)
  repeat {
    if (file.exists(file.path(dir, "config", "analysis_plan.yaml"))) {
      return(dir)
    }
    parent <- dirname(dir)
    if (identical(parent, dir)) {
      stop(
        "Project root not found: no ancestor of '", start,
        "' contains config/analysis_plan.yaml (set ZM_PROJECT_ROOT to override)."
      )
    }
    dir <- parent
  }
}

#' Normalise Qualtrics variable names
#'
#' Replaces every character that is not a letter, digit or underscore by
#' `_`. This maps the CSV names to the SAV names (`SDO-D_1` -> `SDO_D_1`,
#' `Duration (in seconds)` -> `Duration__in_seconds_`) and the codebook item
#' codes to the same canonical form.
#'
#' @param x Character vector of names.
#' @return Character vector of normalised names.
zm_normalise_names <- function(x) {
  gsub("[^A-Za-z0-9_]", "_", x)
}

#' Executable item columns for the item labels of the codebook
#'
#' The codebook and the measurement definitions name items by their
#' source label (`SDO-D_1`); the study data carry the executable column name
#' (`SDO_D_1`). One explicit mapping, taken from the enriched codebook, resolves
#' labels to columns. An unknown label or two labels resolving to one column stop.
#'
#' @param labels Character vector of item source labels.
#' @param item_columns Named character vector, source label -> executable column,
#'   as `zm_item_column_map()` returns it.
#' @return Character vector of executable column names, in the order of `labels`.
zm_item_columns <- function(labels, item_columns) {
  unknown <- setdiff(labels, names(item_columns))
  if (length(unknown)) stop("Item label(s) without an executable column: ", paste(unknown, collapse = ", "), ".")
  columns <- unname(item_columns[labels])
  if (anyDuplicated(columns)) stop("Item labels resolve to the same column: ", paste(columns[duplicated(columns)], collapse = ", "), ".")
  columns
}

#' The label-to-column map of the enriched codebook
#' @param codebook Codebook from [zm_codebook()].
#' @return Named character vector, `source_label` -> `item_code`.
zm_item_column_map <- function(codebook) {
  map <- stats::setNames(codebook$items$item_code, codebook$items$source_label)
  if (anyDuplicated(names(map)) || anyDuplicated(map)) stop("The codebook's item labels and columns must both be unique.")
  map
}

#' Key of a slope SD in `priors$sweep_labels`
#'
#' The width formatted without trailing zeros (`0.10` -> `"0.1"`), which is how
#' the plan keys the labels of the prior sweep.
#'
#' @param sd Numeric vector of slope SDs.
#' @return Character vector of keys.
zm_sweep_key <- function(sd) {
  vapply(as.numeric(sd), function(s) format(s, trim = TRUE, drop0trailing = TRUE, scientific = FALSE),
         character(1))
}

#' Fetch a nested field or stop with a clear message
#'
#' @param analysis_plan List read from a YAML file.
#' @param path Character vector of nested names, e.g. `c("regression", "chains")`.
#' @param file Name of the file (for the error message).
#' @return The value at `path`.
zm_require_field <- function(analysis_plan, path, file) {
  value <- analysis_plan
  for (key in path) {
    if (!is.list(value) || is.null(value[[key]])) {
      stop(
        "Required field '", paste(path, collapse = "$"), "' is missing in ", file, "."
      )
    }
    value <- value[[key]]
  }
  value
}

#' Detect a usable local core count
#'
#' `parallel::detectCores()` can return `NA` in containers and restricted
#' sessions. This helper validates the result and falls back to one core.
#' The environment variable `ZM_CORES` caps the count: every core runs one R
#' process, so a machine with little memory per core sets it lower.
#'
#' @param reserve Number of cores to leave unused.
#' @return Positive integer core count after reserving `reserve`, at most `ZM_CORES`.
zm_detect_cores <- function(reserve = 2L) {
  detected <- suppressWarnings(parallel::detectCores())
  if (length(detected) != 1L || !is.finite(detected) || detected < 1) detected <- 1L
  cores <- max(1L, as.integer(detected) - as.integer(reserve))
  cap <- Sys.getenv("ZM_CORES", "")
  if (nzchar(cap)) {
    cap <- suppressWarnings(as.integer(cap))
    if (is.na(cap) || cap < 1L) stop("ZM_CORES must be a positive whole number.")
    cores <- min(cores, cap)
  }
  cores
}

#' Runtime setup: cmdstan path and parallel cores
#'
#' @param cmdstan_path Directory of the CmdStan installation. Default: env var
#'   `ZM_CMDSTAN_PATH`, else `~/.cmdstan/cmdstan-2.36.0`.
#' @param cores Value for `options(mc.cores)`. Default: all cores minus two,
#'   at least one.
#' @return `TRUE`, invisibly.
zm_setup <- function(cmdstan_path = Sys.getenv("ZM_CMDSTAN_PATH", "~/.cmdstan/cmdstan-2.36.0"),
                     cores = zm_detect_cores()) {
  cmdstan_path <- path.expand(cmdstan_path)
  if (requireNamespace("cmdstanr", quietly = TRUE)) {
    if (dir.exists(cmdstan_path)) {
      cmdstanr::set_cmdstan_path(cmdstan_path)
    } else {
      stop("CmdStan directory not found: ", cmdstan_path)
    }
  } else {
    stop("Package cmdstanr is not installed; brms fits will not run.")
  }
  if (!nzchar(Sys.getenv("QUARTO_R", ""))) {
    Sys.setenv(QUARTO_R = file.path(R.home("bin"), "R"))
  }
  # When the project renv is active in this session, hand its library to child
  # R processes (Quarto/knitr for the report, crew workers): a child started in
  # report/ has no .Rprofile and would otherwise fall back to the user library.
  if (nzchar(Sys.getenv("RENV_PROJECT", ""))) {
    lib <- .libPaths()[1]
    Sys.setenv(R_LIBS = lib, R_LIBS_USER = lib, R_LIBS_SITE = lib)
  }
  options(mc.cores = as.integer(cores))
  # cmdstanr reads the chain CSVs with data.table::fread. Its OpenMP threads
  # can abort in libomp inside crew workers; one reader thread avoids OpenMP
  # there entirely. The environment variable reaches every crew worker started
  # after this call (data.table reads it when it loads there), so fits that do
  # not call zm_setup() themselves, such as the AP3 fill, read single-threaded too.
  Sys.setenv(R_DATATABLE_NUM_THREADS = "1")
  data.table::setDTthreads(1L)
  invisible(TRUE)
}

#' Read the analysis-plan configuration
#'
#' Reads `config/analysis_plan.yaml`, validates the fields the pipeline relies
#' on and applies the run profile (`full` or `smoke`) to the expensive
#' settings. Adds `profile_name` and `root` (project root path). Every list of
#' scales comes back in the order of the scale codebook's `order` column
#' ([zm_order_scale_lists_technical()]), and the column names and the predictor
#' side of the regressions are read from the codebook
#' ([zm_derive_codebook_names_technical()]).
#'
#' Human-readable values are recorded in `config/analysis_plan.yaml`: `income`
#' lists the survey bands and their euro representatives; `age_bands` and
#' `income_bands_per_member` the bands age and income per household member enter
#' the analyses in; `gender` specifies
#' the contrasts; `regression`, `priors` and `missing_data`
#' describe the models. Inspect `analysis_plan$income$band_representative` after loading
#' to see the numeric mapping actually used. This function validates and loads
#' those settings; it does not define the scientific values itself.
#'
#' @param profile Profile name; `ZM_PROFILE` overrides the smoke project default.
#' @param path Path to the YAML file (default: under the project root).
#' @return Named list (the configuration).
select_run_profile <- function() {
  profile <- Sys.getenv("ZM_PROFILE", "")
  if (nzchar(profile)) return(profile)
  if (identical(Sys.getenv("TAR_PROJECT", ""), "smoke")) "smoke" else "full"
}

zm_config <- function(profile = select_run_profile(),
                      path = file.path(zm_root(), "config", "analysis_plan.yaml")) {
  if (is.null(profile) || is.na(profile) || !nzchar(profile)) profile <- "full"
  analysis_plan <- yaml::read_yaml(path)
  file <- basename(path)

  zm_require_config_fields_technical(analysis_plan, file)
  settings <- zm_select_profile_technical(analysis_plan, profile, file)
  analysis_plan <- zm_apply_profile_technical(analysis_plan, settings, profile)
  analysis_plan <- zm_normalise_income_bands_technical(analysis_plan, file)
  analysis_plan <- zm_normalise_covariate_bands_technical(analysis_plan, file)
  zm_require_primary_prior_in_sweep_technical(analysis_plan, file)
  zm_require_primary_family_technical(analysis_plan)

  analysis_plan$root <- dirname(dirname(normalizePath(path, mustWork = TRUE)))
  codebook <- zm_codebook(analysis_plan)
  analysis_plan <- zm_order_scale_lists_technical(analysis_plan, codebook, file)
  zm_require_prediction_table_technical(analysis_plan, file)
  analysis_plan <- zm_label_scale_sets_technical(analysis_plan, codebook, file)
  analysis_plan <- zm_derive_codebook_names_technical(analysis_plan, file, codebook)
  analysis_plan
}

#' Read and validate the single configuration used by preparation
#'
#' The tracked analysis plan is materialised through [zm_config()], so that the
#' typed, profile-applied and derived fields every downstream preparation verb
#' receives are the ones the rest of the project uses. The profile is the one
#' `ZM_PROFILE` names, exactly as the pipeline preamble's `zm_config()` call.
#' The configuration is a field of `analysis_inputs`, not a global object.
#'
#' @param files Paths from [select_analysis_input_files()].
#' @return The configuration list.
read_and_check_analysis_plan <- function(files, profile = select_run_profile()) {
  analysis_plan <- zm_config(profile = profile, path = files[["analysis_plan"]])
  check_analysis_plan(analysis_plan)
  analysis_plan
}

#' Stop unless the analysis plan's registered value sets are usable
#'
#' A pure stop/go check: it reads no participant data, computes nothing and
#' returns nothing. It establishes that the registered East and West
#' federal-state code sets are disjoint, the plan invariant AP3's East/West
#' derivation depends on ([ap3_check_east_west_disjoint()]).
#'
#' @param analysis_plan Configuration from [zm_config()].
#' @return `TRUE`, invisibly.
check_analysis_plan <- function(analysis_plan) {
  ap3_check_east_west_disjoint(analysis_plan)
  invisible(TRUE)
}

#' Resources of one pipeline run
#'
#' The core arithmetic of the two crew controllers: the cheap targets get one
#' worker per usable core up to twelve, the brms fits get as many workers as
#' fit beside each other when every fit uses `analysis_plan$regression$cores` cores.
#'
#' @param analysis_plan Configuration from [zm_config()].
#' @param reserve Cores left unused (see [zm_detect_cores()]).
#' @return `list(cores, light_workers, heavy_workers)`.
zm_pipeline_resources <- function(analysis_plan, reserve = 2L) {
  cores <- zm_detect_cores(reserve = reserve)
  list(
    cores = cores,
    light_workers = min(12L, cores),
    heavy_workers = max(1L, floor(cores / as.integer(analysis_plan$regression$cores)))
  )
}

# Configuration support ---------------------------------------------------------
# `_technical` checks or prepares the configuration the implementation accepts.
# It does not endorse the scientific values in the plan.

zm_require_config_fields_technical <- function(analysis_plan, file) {
  required <- list(
    c("meta", "codebook_dir"),
    c("profiles"),
    c("exclusions", "consent_required_value"),
    c("exclusions", "min_age"),
    c("exclusions", "max_age"),
    c("exclusions", "attention_check_1_correct"),
    c("exclusions", "attention_check_2_correct"),
    c("exclusions", "survey_status_complete"),
    c("exclusions", "survey_status_quota_full"),
    # exclusions$gender_divers_min_n is the smallest divers group AP1 keeps.
    c("exclusions", "gender_divers_min_n"),
    c("scales", "response_min"),
    c("scales", "response_max"),
    c("income", "band_representative"),
    c("income", "hh_size_top_value"),
    c("age_bands", "first_years"),
    c("age_bands", "last_year"),
    c("age_bands", "labels"),
    c("age_bands", "representative"),
    c("income_bands_per_member", "limits"),
    c("income_bands_per_member", "labels"),
    c("income_bands_per_member", "representative"),
    c("east_west", "east"),
    c("east_west", "west"),
    c("free_text_comment", "handling"),
    c("gender", "contrasts"),
    c("missing_data", "standardise_within_model"),
    c("missing_data", "fill"),
    c("regression", "family"),
    c("regression", "fit_roles"),
    c("regression", "validity_gate"),
    c("regression", "outcomes"),
    c("regression", "motives"),
    c("regression", "covariates"),
    c("regression", "chains"),
    c("regression", "cores"),
    c("regression", "ess_target"),
    c("regression", "rhat_max"),
    c("regression", "ci_level"),
    c("regression", "seed"),
    c("priors", "slope_sd_primary"),
    c("priors", "slope_sd_sweep"),
    c("priors", "sweep_labels"),
    c("priors", "gender_sd"),
    c("priors", "intercept_sd"),
    c("priors", "sigma_sd"),
    c("predictions", "table"),
    c("network", "nodes"),
    c("network", "iter"),
    c("network", "B"),
    c("network", "g_prior"),
    c("network", "g_prior_sweep"),
    c("network", "df_prior"),
    c("network", "seed_base"),
    c("network", "retry_seed_offset"),
    c("network", "min_success_rate"),
    c("network", "bf_include"),
    c("network", "bf_exclude")
  )
  for (p in required) zm_require_field(analysis_plan, p, file)
  # One regression per outcome, and the motives build the predictor side of
  # each (zm_derive_codebook_names_technical()), so neither list may be empty.
  for (field in c("outcomes", "motives")) {
    if (length(unlist(analysis_plan$regression[[field]])) == 0L) {
      stop("Required field 'regression$", field, "' is empty in ", file, ".")
    }
  }
  # One predicate per plan value the implementation relies on. Each names its
  # field, so that no function downstream repeats the check.
  positive_number <- function(x) is.numeric(x) && length(x) == 1L && !is.na(x) && is.finite(x) && x > 0
  whole_number <- function(x) {
    is.numeric(x) && length(x) == 1L && !is.na(x) && is.finite(x) && x >= 0 && x == round(x)
  }
  probability <- function(x) is.numeric(x) && length(x) == 1L && !is.na(x) && is.finite(x) && x > 0 && x < 1
  non_empty_string <- function(x) {
    length(x) == 1L && !is.na(x) && nzchar(trimws(as.character(x)))
  }
  for (field in c("min_age", "max_age")) {
    if (!whole_number(analysis_plan$exclusions[[field]])) {
      stop("Field 'exclusions$", field, "' must be a single non-negative whole number in ", file, ".")
    }
  }
  if (analysis_plan$exclusions$max_age < analysis_plan$exclusions$min_age) {
    stop("Field 'exclusions$max_age' must be at least exclusions$min_age in ", file, ".")
  }
  if (!non_empty_string(analysis_plan$exclusions$survey_status_quota_full)) {
    stop("Field 'exclusions$survey_status_quota_full' must name the flow status of a quota-full exit in ", file, ".")
  }
  if (!whole_number(analysis_plan$exclusions$gender_divers_min_n) || analysis_plan$exclusions$gender_divers_min_n <= 0) {
    stop("Field 'exclusions$gender_divers_min_n' must be a single positive whole number in ", file, ".")
  }
  if (!identical(analysis_plan$gender$contrasts, "treatment")) {
    stop("Field 'gender$contrasts' must be 'treatment' in ", file, "; no other gender contrast is implemented.")
  }
  if (!grepl("^retained in the internal", as.character(analysis_plan$free_text_comment$handling))) {
    stop("Field 'free_text_comment$handling' must be the implemented handling ('retained in the internal analysis file ...') in ", file, ".")
  }
  if (!isTRUE(analysis_plan$missing_data$standardise_within_model)) {
    stop("Field 'missing_data$standardise_within_model' must be true in ", file, "; only standardisation within each model's sample is implemented.")
  }
  # The validity gate: the block and every threshold the gate reads.
  gate <- analysis_plan$regression$validity_gate
  for (field in c("rhat_max", "divergences_max", "treedepth_hits_max", "bfmi_min",
                  "max_ess_doublings", "adapt_delta_retry", "max_treedepth_retry")) {
    value <- gate[[field]]
    if (!is.numeric(value) || length(value) != 1L || is.na(value)) {
      stop("Field 'regression$validity_gate$", field, "' must be a single number in ", file, ".")
    }
  }
  if (!is.numeric(analysis_plan$regression$ess_target) || length(analysis_plan$regression$ess_target) != 1L ||
      is.na(analysis_plan$regression$ess_target)) {
    stop("Field 'regression$ess_target' must be a single number in ", file, ".")
  }
  # Every fit outside the outcome x prior grid carries a role and the label the
  # plan gives that role.
  for (role in c("student_refit", "multivariate")) {
    if (!non_empty_string(analysis_plan$regression$fit_roles[[role]])) {
      stop("Field 'regression$fit_roles$", role, "' must name the label of that fit in ", file, ".")
    }
  }
  # Every prior width the regressions read, checked once here, so that no
  # fitting function repeats the check.
  sweep <- as.list(analysis_plan$priors$slope_sd_sweep)
  widths <- c(
    list("priors$slope_sd_primary" = analysis_plan$priors$slope_sd_primary),
    stats::setNames(sweep, paste0("priors$slope_sd_sweep[", seq_along(sweep), "]")),
    list("priors$gender_sd" = analysis_plan$priors$gender_sd),
    list("priors$intercept_sd" = analysis_plan$priors$intercept_sd),
    list("priors$sigma_sd" = analysis_plan$priors$sigma_sd),
    list("sensitivity$student_t$sigma_scale_prior_sd" = analysis_plan$sensitivity$student_t$sigma_scale_prior_sd)
  )
  for (field in names(widths)) {
    width <- widths[[field]]
    if (!is.numeric(width) || length(width) != 1L || is.na(width) || width <= 0) {
      stop("Field '", field, "' must be a single positive number in ", file, ".")
    }
  }
  # Every width of the sweep carries a label, keyed as the sweep value prints.
  labels <- analysis_plan$priors$sweep_labels
  for (key in zm_sweep_key(unlist(analysis_plan$priors$slope_sd_sweep))) {
    if (!non_empty_string(labels[[key]])) {
      stop("Field 'priors$sweep_labels' has no label for the swept width ", key, " in ", file, ".")
    }
  }
  # The edge-inclusion prior and its sweep: the Bayes-factor conversion divides
  # by the prior odds, which is defined only strictly inside (0, 1).
  if (!probability(analysis_plan$network$g_prior)) {
    stop("Field 'network$g_prior' must be a single number strictly between 0 and 1 in ", file, ".")
  }
  g_sweep <- as.list(analysis_plan$network$g_prior_sweep)
  if (length(g_sweep) == 0L) {
    stop("Field 'network$g_prior_sweep' must name at least one prior in ", file, ".")
  }
  for (k in seq_along(g_sweep)) {
    if (!probability(g_sweep[[k]])) {
      stop("Field 'network$g_prior_sweep[[", k, "]]' must be a single number strictly between 0 and 1 in ", file, ".")
    }
  }
  # The missing-value fill: the drop thresholds, the reported interval, the
  # prior widths of the four fill models and the three demographics it fills.
  fill <- analysis_plan$missing_data$fill
  for (field in c("items_per_scale_more_than", "demographics_missing_at_least")) {
    if (!whole_number(fill$drop[[field]])) {
      stop("Field 'missing_data$fill$drop$", field, "' must be a non-negative whole number in ", file, ".")
    }
  }
  if (!probability(fill$interval)) {
    stop("Field 'missing_data$fill$interval' must be a single number strictly between 0 and 1 in ", file, ".")
  }
  fill_models <- c(list(items = fill$items), fill$demographics[as.character(unlist(fill$demographics$order))])
  names(fill_models) <- c("items", paste0("demographics$", as.character(unlist(fill$demographics$order))))
  for (model in names(fill_models)) {
    model_priors <- fill_models[[model]]$priors
    for (field in intersect(c("intercept_sd", "sd_sd", "b_sd", "sigma_sd"), names(model_priors))) {
      if (!positive_number(model_priors[[field]])) {
        stop("Field 'missing_data$fill$", model, "$priors$", field, "' must be a single positive number in ", file, ".")
      }
    }
  }
  fill_demographics <- as.character(unlist(fill$demographics$order))
  if (length(fill_demographics) != 3L || anyNA(fill_demographics) || !all(nzchar(fill_demographics))) {
    stop("Field 'missing_data$fill$demographics$order' must name the three demographic columns in ", file, ".")
  }
  nu_fixed <- analysis_plan$sensitivity$student_t$nu_fixed
  if (!is.numeric(nu_fixed) || length(nu_fixed) != 1L || is.na(nu_fixed) || nu_fixed <= 2) {
    stop("Field 'sensitivity$student_t$nu_fixed' must be a single number greater than 2 in ", file, ".")
  }
  invisible(NULL)
}

#' Every list of scales of the plan, in the order of the scale codebook
#'
#' The plan names scales in several lists: the regression outcomes and motives,
#' the network nodes, the scales of every factor-analytic set, the factors of
#' every confirmatory model and the cells of the prediction table. Each of them
#' is a set. Its order is the `order` column of `codebook_scales.csv`
#' ([zm_order_scale_keys()]), whatever order the plan writes it in, and the
#' one-factor confirmatory models follow the order of their scales. A list that
#' names a scale the codebook does not know, or one scale twice, stops the load.
#'
#' @param analysis_plan Configuration after the profile has been applied, with `root`.
#' @param codebook Codebook from [zm_codebook()].
#' @param file Name of the plan file (for the error messages).
#' @return `analysis_plan` with every list of scales in codebook order.
zm_order_scale_lists_technical <- function(analysis_plan, codebook, file) {
  scale_keys <- as.character(codebook$scales$scale_key)
  in_order <- function(keys, field) {
    keys <- as.character(unlist(keys))
    absent <- setdiff(keys, scale_keys)
    if (length(absent) > 0L) {
      stop(field, " in ", file, " names scale(s) absent from codebook_scales.csv: ",
           paste(absent, collapse = ", "), ".")
    }
    if (anyDuplicated(keys)) {
      stop(field, " in ", file, " names the scale(s) ", paste(unique(keys[duplicated(keys)]), collapse = ", "),
           " twice.")
    }
    zm_order_scale_keys(keys, codebook)
  }
  # A list keyed by scale (the prediction table, a model's factors) keeps every
  # entry and takes the codebook's order of its scales.
  entries_in_order <- function(x, scales, field) {
    x[match(in_order(scales, field), as.character(unlist(scales)))]
  }

  analysis_plan$regression$outcomes <- in_order(analysis_plan$regression$outcomes, "regression$outcomes")
  analysis_plan$regression$motives <- in_order(analysis_plan$regression$motives, "regression$motives")
  analysis_plan$network$nodes <- in_order(analysis_plan$network$nodes, "network$nodes")

  analysis_plan$factor_analysis$sets <- lapply(analysis_plan$factor_analysis$sets, function(set) {
    set$scales <- in_order(set$scales, paste0("factor_analysis$sets (", set$key, ")"))
    set
  })

  models <- analysis_plan$confirmatory_models$models
  models <- lapply(models, function(model) {
    field <- paste0("confirmatory_models$models (", model$key, ")")
    model$factors <- entries_in_order(model$factors, unlist(model$factors), field)
    model
  })
  # Within a family, the models in the order of their first scale; the
  # families keep the plan's order.
  family <- vapply(models, function(model) as.character(model$family), character(1))
  first_scale <- vapply(models, function(model) match(as.character(unlist(model$factors))[1], scale_keys),
                        integer(1))
  analysis_plan$confirmatory_models$models <- models[order(match(family, unique(family)), first_scale)]

  table <- analysis_plan$predictions$table
  table <- entries_in_order(table, names(table), "predictions$table")
  analysis_plan$predictions$table <- lapply(table, function(cells) {
    entries_in_order(cells, names(cells), "predictions$table")
  })
  analysis_plan
}

#' The prediction table lists every motive for every outcome with a known sign
#'
#' One row per regression outcome, each listing every motive exactly once with
#' the sign `"+"`, `"-"`, `"±"` or none. Runs after
#' [zm_order_scale_lists_technical()], which has checked that every key is a
#' scale of the codebook.
#'
#' @param analysis_plan Configuration after [zm_order_scale_lists_technical()].
#' @param file Name of the plan file (for the error messages).
#' @return `NULL`, invisibly; stops otherwise.
zm_require_prediction_table_technical <- function(analysis_plan, file) {
  table <- analysis_plan$predictions$table
  outcomes <- as.character(unlist(analysis_plan$regression$outcomes))
  motives <- as.character(unlist(analysis_plan$regression$motives))
  if (!setequal(names(table), outcomes) || anyDuplicated(names(table))) {
    stop("Field 'predictions$table' must have one row per outcome of regression$outcomes in ", file, ".")
  }
  for (outcome in names(table)) {
    cells <- table[[outcome]]
    if (!setequal(names(cells), motives) || anyDuplicated(names(cells))) {
      stop("Field 'predictions$table' row '", outcome,
           "' must list every motive of regression$motives exactly once in ", file, ".")
    }
    signs <- vapply(cells, function(sign) if (is.null(sign) || is.na(sign)) "" else as.character(sign),
                    character(1))
    unknown <- setdiff(signs, c("+", "-", "±", ""))
    if (length(unknown) > 0L) {
      stop("Field 'predictions$table' row '", outcome, "' contains unknown signs: ",
           paste(unknown, collapse = ", "), " in ", file, ".")
    }
  }
  invisible(NULL)
}

#' The labels of the factor-analytic sets and confirmatory models
#'
#' A label that names scales comes from the scale codebook
#' ([zm_label_scale_set()]): a one-factor confirmatory model is labelled by its
#' scale's `label` and may not carry a label of its own in the plan, and a set
#' or model of the subscales of one instrument that the plan leaves unlabelled
#' gets the instrument and its subscales. Every other set and model keeps the
#' label the plan gives it.
#'
#' @param analysis_plan Configuration after [zm_order_scale_lists_technical()].
#' @param codebook Codebook from [zm_codebook()].
#' @param file Name of the plan file (for the error messages).
#' @return `analysis_plan` with a `label` on every set and model.
zm_label_scale_sets_technical <- function(analysis_plan, codebook, file) {
  label_of <- function(entry, scales, field) {
    scales <- as.character(unlist(scales))
    if (length(scales) == 1L && !is.null(entry$label)) {
      stop(field, " in ", file, " is one scale and carries a label; its label is the scale's ",
           "`label` in codebook_scales.csv.")
    }
    if (!is.null(entry$label)) return(entry)
    label <- zm_label_scale_set(scales, codebook)
    if (is.na(label)) {
      stop(field, " in ", file, " needs a label: its scales belong to more than one instrument.")
    }
    entry$label <- label
    entry
  }
  analysis_plan$factor_analysis$sets <- lapply(analysis_plan$factor_analysis$sets, function(set) {
    label_of(set, set$scales, paste0("factor_analysis$sets (", set$key, ")"))
  })
  analysis_plan$confirmatory_models$models <- lapply(analysis_plan$confirmatory_models$models, function(model) {
    label_of(model, model$factors, paste0("confirmatory_models$models (", model$key, ")"))
  })
  analysis_plan
}

#' The label of a set of scales, read from the scale codebook
#'
#' One scale is labelled by its `label`. Several subscales of one instrument
#' are labelled by the instrument, the `scale` column without its citation,
#' and their `subscale` names in lower case and in codebook order:
#' "UMS-6 (intimacy and achievement)". Scales of several instruments have no
#' such label.
#'
#' @param keys Scale keys.
#' @param codebook Codebook from [zm_codebook()].
#' @return Character scalar; `NA` for scales of more than one instrument.
zm_label_scale_set <- function(keys, codebook) {
  keys <- zm_order_scale_keys(keys, codebook)
  rows <- codebook$scales[match(keys, codebook$scales$scale_key), , drop = FALSE]
  if (nrow(rows) == 1L) return(as.character(rows$label))
  instrument <- unique(trimws(sub("\\s*\\([^()]*\\)\\s*$", "", as.character(rows$scale))))
  if (length(instrument) != 1L) return(NA_character_)
  subscales <- tolower(as.character(rows$subscale))
  paste0(instrument, " (", paste(subscales[-length(subscales)], collapse = ", "), " and ",
         subscales[length(subscales)], ")")
}

#' Names of variables, taken from the codebook once
#'
#' The plan names scales and covariates by their keys; the codebook holds the
#' standardised column beside every key. This step reads the codebook once, at
#' load, and writes the column names the pipeline works with into `analysis_plan`, so
#' that no function downstream pastes or strips a `_z` suffix:
#'
#' - `regression$covariates`: the standardised column of every metric covariate
#'   the plan lists, with `gender` (a factor) kept as it is.
#' - `regression$predictors`: the predictor side of every regression, the
#'   standardised motives in the codebook's order, then the covariates in the
#'   plan's order (`"zm_security_z + ... + age_z + gender + income_z"`).
#' - `regression$covariate_keys`, `regression$term_keys`: the keys behind those
#'   columns, for the tables that report a term by its key.
#' - `standardisation$covariates_z`: the metric covariates of the codebook.
#' - `network$nodes`: the standardised column of every node the plan lists;
#'   `network$node_keys` the keys, in the same order.
#'
#' It runs after [zm_order_scale_lists_technical()], which has put the scale
#' lists into the codebook's order and checked that the codebook knows them.
#'
#' @param analysis_plan Configuration after the profile has been applied, with `root`.
#' @param file Name of the plan file (for the error messages).
#' @param codebook Codebook from [zm_codebook()].
#' @return `analysis_plan` with the derived name fields.
zm_derive_codebook_names_technical <- function(analysis_plan, file, codebook = zm_codebook(analysis_plan)) {
  scale_keys <- zm_order_scale_keys(unique(c(as.character(unlist(analysis_plan$regression$outcomes)),
                                             as.character(unlist(analysis_plan$regression$motives)))),
                                    codebook)

  metric <- as.character(codebook$covariates$covariate)
  covariate_keys <- as.character(unlist(analysis_plan$regression$covariates))
  unknown <- setdiff(covariate_keys, c(metric, "gender"))
  if (length(unknown) > 0) {
    stop("regression$covariates in ", file, " name(s) neither a numeric covariate in the YAML ",
         "nor the gender factor: ", paste(unknown, collapse = ", "), ".")
  }
  covariates <- covariate_keys
  is_metric <- covariate_keys %in% metric
  covariates[is_metric] <- zm_z_col(covariate_keys[is_metric], codebook)

  node_keys <- as.character(unlist(analysis_plan$network$nodes))
  if (length(node_keys) == 0) stop("network$nodes in ", file, " is empty.")
  nodes <- zm_z_col(node_keys, codebook)

  analysis_plan$regression$covariates <- covariates
  analysis_plan$regression$covariate_keys <- covariate_keys
  # The motives in the codebook's order, then the covariates in the plan's own
  # order (they are not scales).
  analysis_plan$regression$predictors <- paste(
    c(zm_z_col(zm_order_scale_keys(analysis_plan$regression$motives, codebook), codebook), covariates),
    collapse = " + "
  )
  # Every model column that carries a key: the standardised outcomes, motives
  # and metric covariates. Used where a fit's parameter names are reported.
  model_keys <- c(scale_keys, metric)
  analysis_plan$regression$term_keys <- stats::setNames(model_keys, zm_z_col(model_keys, codebook))
  analysis_plan$standardisation$covariates_z <- metric
  analysis_plan$network$nodes <- nodes
  analysis_plan$network$node_keys <- node_keys
  analysis_plan
}

zm_select_profile_technical <- function(analysis_plan, profile, file) {
  profiles <- analysis_plan$profiles
  if (is.null(profiles[[profile]])) {
    stop(
      "Unknown profile '", profile, "' in ", file, "; available: ",
      paste(names(profiles), collapse = ", "), "."
    )
  }
  prof <- profiles[[profile]]
  profile_fields <- c(
    "reliability_bootstrap_n",
    "regression_iter_per_chain", "regression_warmup",
    "regression_ess_target", "network_B", "network_iter"
  )
  missing_prof <- setdiff(profile_fields, names(prof))
  if (length(missing_prof) > 0) {
    stop(
      "Profile '", profile, "' in ", file, " lacks: ",
      paste(missing_prof, collapse = ", "), "."
    )
  }
  prof
}

zm_apply_profile_technical <- function(analysis_plan, prof, profile) {
  analysis_plan$reliability$bootstrap_n <- prof$reliability_bootstrap_n
  analysis_plan$regression$iter_per_chain <- prof$regression_iter_per_chain
  analysis_plan$regression$warmup <- prof$regression_warmup
  analysis_plan$regression$ess_target <- prof$regression_ess_target
  analysis_plan$network$B <- prof$network_B
  analysis_plan$network$iter <- prof$network_iter
  analysis_plan$profile_name <- profile
  analysis_plan
}

zm_normalise_income_bands_technical <- function(analysis_plan, file) {
  # The income bands are keyed 1..13 in the YAML; a named numeric vector in
  # band order makes `band_representative[demo_income_hh_net]` safe.
  bands <- analysis_plan$income$band_representative
  band_keys <- as.integer(names(bands))
  if (anyNA(band_keys) || !identical(sort(band_keys), seq_along(bands))) {
    stop("income$band_representative in ", file, " must be keyed 1..", length(bands), ".")
  }
  bands <- unlist(bands)[order(band_keys)]
  analysis_plan$income$band_representative <- stats::setNames(as.numeric(bands), sort(band_keys))
  analysis_plan
}

zm_normalise_covariate_bands_technical <- function(analysis_plan, file) {
  # The age bands and the income bands per household member as plain vectors:
  # strictly increasing whole-year first years ending no later than the last
  # year, strictly increasing positive euro limits, one label per band, and a
  # representative rule the implementation knows.
  age <- analysis_plan$age_bands
  first_years <- suppressWarnings(as.numeric(unlist(age$first_years)))
  last_year <- suppressWarnings(as.numeric(age$last_year))
  if (length(first_years) == 0L || anyNA(first_years) || any(first_years != round(first_years)) ||
      any(diff(first_years) <= 0) || length(last_year) != 1L || is.na(last_year) ||
      last_year != round(last_year) || last_year < max(first_years)) {
    stop("age_bands in ", file, " needs strictly increasing whole first_years and a whole last_year no earlier than the last of them.")
  }
  age_labels <- as.character(unlist(age$labels))
  if (length(age_labels) != length(first_years) || anyNA(age_labels) || any(!nzchar(age_labels)) ||
      anyDuplicated(age_labels)) {
    stop("age_bands$labels in ", file, " must name every band once.")
  }
  if (!identical(age$representative, "midpoint")) {
    stop("age_bands$representative in ", file, " must be 'midpoint'; no other representative is implemented.")
  }
  analysis_plan$age_bands$first_years <- first_years
  analysis_plan$age_bands$last_year <- last_year
  analysis_plan$age_bands$labels <- age_labels
  income <- analysis_plan$income_bands_per_member
  limits <- suppressWarnings(as.numeric(unlist(income$limits)))
  if (length(limits) < 2L || anyNA(limits) || any(limits <= 0) || any(diff(limits) <= 0)) {
    stop("income_bands_per_member$limits in ", file, " must be at least two strictly increasing positive numbers.")
  }
  income_labels <- as.character(unlist(income$labels))
  if (length(income_labels) != length(limits) - 1L || anyNA(income_labels) ||
      any(!nzchar(income_labels)) || anyDuplicated(income_labels)) {
    stop("income_bands_per_member$labels in ", file, " must name every band once (one fewer than the limits).")
  }
  if (!identical(income$representative, "geometric_mean")) {
    stop("income_bands_per_member$representative in ", file, " must be 'geometric_mean'; no other representative is implemented.")
  }
  analysis_plan$income_bands_per_member$limits <- limits
  analysis_plan$income_bands_per_member$labels <- income_labels
  analysis_plan
}

zm_require_primary_prior_in_sweep_technical <- function(analysis_plan, file) {
  if (!(analysis_plan$priors$slope_sd_primary %in% analysis_plan$priors$slope_sd_sweep)) {
    stop("priors$slope_sd_primary must be one of priors$slope_sd_sweep in ", file, ".")
  }

  invisible(NULL)
}

#' Require the supported primary regression likelihood
#'
#' The primary and prior-width comparison fits use the configured Gaussian
#' likelihood. Student-t fits have their own declared robustness path. Check
#' this again at the pipeline fit call so a changed in-memory configuration
#' cannot describe one primary family while another is fitted.
zm_require_primary_family_technical <- function(analysis_plan) {
  if (!identical(analysis_plan$regression$family, "gaussian")) {
    stop("Only the Gaussian primary regression is implemented (analysis_plan$regression$family must be 'gaussian'); Student-t is the separate robustness refit.")
  }
  invisible(NULL)
}

#' Read the three codebook tables and the YAML covariate specification
#'
#' Item codes are normalised (`-` -> `_`). The scale table gets two
#' list-columns: `item_codes` (character vector per scale) and
#' `reverse_items` (character vector, possibly empty), parsed from the
#' `;`-separated fields of `codebook_scales.csv`. The YAML `covariates` section
#' names the metric covariates the same way the scale table names the scales:
#' the unstandardised column (`covariate`) beside its standardised one
#' (`z_col`), with the covariate's plain-language `label`.
#'
#' The `order` column of `codebook_scales.csv` is the one place the order of
#' the scales is defined: the scale table comes back sorted by it, whatever the
#' order of the rows in the file. It must give every scale a whole number, no
#' two the same, and together the ranks 1 to the number of scales.
#'
#' @param analysis_plan Configuration from [zm_config()].
#' @return `list(items = tibble, scales = tibble, factors = tibble,
#'   covariates = tibble)`; `scales` in the order of its `order` column.
zm_codebook <- function(analysis_plan) {
  dir <- file.path(analysis_plan$root, analysis_plan$meta$codebook_dir)
  read <- function(name) {
    path <- file.path(dir, name)
    if (!file.exists(path)) stop("Codebook file not found: ", path)
    readr::read_csv(path, show_col_types = FALSE, progress = FALSE,
                    col_types = readr::cols(.default = readr::col_character()))
  }
  items <- read("codebook_items.csv")
  scales <- read("codebook_scales.csv")
  factors <- read("codebook_factors.csv")
  covariates <- read_covariate_specification(analysis_plan)

  split_codes <- function(x) {
    lapply(x, function(s) {
      if (is.na(s) || !nzchar(trimws(s))) return(character(0))
      zm_normalise_names(trimws(strsplit(s, ";", fixed = TRUE)[[1]]))
    })
  }

  items$source_label <- items$item_code
  items$item_code <- zm_normalise_names(items$item_code)
  items$reverse_keyed <- tolower(items$reverse_keyed) == "yes"
  items$n_categories <- as.integer(items$n_categories)

  scales$item_count <- as.integer(scales$item_count)
  scales$order <- zm_check_scale_order_technical(scales)
  scales <- scales[order(scales$order), , drop = FALSE]
  scales$item_codes <- split_codes(scales$item_codes)
  scales$reverse_items <- split_codes(scales$reverse_keyed_items)

  bad <- scales$scale_key[lengths(scales$item_codes) != scales$item_count]
  if (length(bad) > 0) {
    stop("codebook_scales.csv: item_count disagrees with item_codes for ", paste(bad, collapse = ", "))
  }
  unknown <- setdiff(unlist(scales$item_codes), items$item_code)
  if (length(unknown) > 0) {
    stop("codebook_scales.csv lists items absent from codebook_items.csv: ", paste(unknown, collapse = ", "))
  }

  factors$order <- as.integer(factors$order)
  factors$value_corr <- as.numeric(factors$value_corr)

  for (field in c("z_col_all", "z_col_known_gender")) {
    if (!field %in% names(scales) || !field %in% names(covariates)) {
      stop("The scale codebook and YAML covariates must both name ", field, ".")
    }
    columns <- c(scales[[field]], covariates[[field]])
    if (anyNA(columns) || any(!nzchar(trimws(columns))) || anyDuplicated(columns)) {
      stop("The codebook field ", field, " must contain unique non-empty column names.")
    }
  }
  list(items = items, scales = scales, factors = factors, covariates = covariates)
}

#' Read numeric covariate labels and column names from the analysis-plan YAML
#'
#' @param analysis_plan List read from `analysis_plan.yaml`.
#' @return Tibble with one row per numeric covariate and its column mappings.
read_covariate_specification <- function(analysis_plan) {
  specification <- analysis_plan$covariates
  keys <- names(specification)
  if (!is.list(specification) || !length(specification) || is.null(keys) ||
      anyNA(keys) || any(!nzchar(keys)) || anyDuplicated(keys)) {
    stop("The YAML covariates section must be a named list of numeric covariates.")
  }
  fields <- c("z_col", "label", "z_col_all", "z_col_known_gender")
  rows <- lapply(keys, function(key) {
    entry <- specification[[key]]
    values <- vapply(fields, function(field) {
      value <- entry[[field]]
      if (!is.character(value) || length(value) != 1L || is.na(value) ||
          !nzchar(trimws(value))) {
        stop("YAML covariates$", key, "$", field, " must be one nonempty string.")
      }
      value
    }, character(1))
    tibble::as_tibble(c(list(covariate = key), as.list(values)))
  })
  dplyr::bind_rows(rows)
}

#' Check the `order` column of the scale codebook
#'
#' Every scale needs a whole number, no two scales the same one, and together
#' they are the ranks 1 to the number of scales, so that the order of the
#' scales is complete and has no gaps.
#'
#' @param scales The scale table as read from `codebook_scales.csv`.
#' @return Integer vector, the `order` of every row.
zm_check_scale_order_technical <- function(scales) {
  if (!"order" %in% names(scales)) stop("codebook_scales.csv lacks the column 'order'.")
  value <- suppressWarnings(as.numeric(scales$order))
  whole <- !is.na(value) & is.finite(value) & value == round(value)
  if (!all(whole)) {
    stop("codebook_scales.csv: 'order' must be a whole number for every scale; it is not for ",
         paste(scales$scale_key[!whole], collapse = ", "), ".")
  }
  value <- as.integer(value)
  if (anyDuplicated(value)) {
    stop("codebook_scales.csv: 'order' must differ between scales; ",
         paste(unique(value[duplicated(value)]), collapse = ", "), " is given twice.")
  }
  if (!identical(sort(value), seq_len(nrow(scales)))) {
    stop("codebook_scales.csv: 'order' must number the ", nrow(scales), " scales 1 to ", nrow(scales),
         "; it gives ", paste(sort(value), collapse = ", "), ".")
  }
  value
}

#' Scale keys in the order of the scale codebook
#'
#' The `order` column of `codebook_scales.csv` is the only place an order of
#' the scales is defined. Every other list of scale keys, in the plan or in the
#' code, is a set; this puts such a set into the codebook's order.
#'
#' @param keys Character vector of scale keys, each at most once.
#' @param codebook Codebook from [zm_codebook()].
#' @return `keys` in the codebook's order.
zm_order_scale_keys <- function(keys, codebook) {
  keys <- as.character(unlist(keys))
  ordered <- as.character(codebook$scales$scale_key)
  unknown <- setdiff(keys, ordered)
  if (length(unknown) > 0L) {
    stop("Not a scale of codebook_scales.csv: ", paste(unknown, collapse = ", "), ".")
  }
  if (anyDuplicated(keys)) {
    stop("A list of scales names ", paste(unique(keys[duplicated(keys)]), collapse = ", "), " twice.")
  }
  ordered[ordered %in% keys]
}

#' Read and cross-check the three analysis codebooks
#'
#' The three tracked codebook files are materialised through [zm_codebook()], so
#' that the split code lists, typed counts and cross-references the analysis
#' works from are the ones the rest of the project uses. Reading the CSV tables
#' alone cannot provide those shapes. The codebook is a field of
#' `analysis_inputs`, so every displayed preparation verb names its source.
#'
#' @param files Paths from [select_analysis_input_files()].
#' @param analysis_plan Configuration from [read_and_check_analysis_plan()].
#' @return `list(items, scales, factors, covariates)`.
read_and_check_codebook <- function(files, analysis_plan) {
  codebook <- zm_codebook(analysis_plan)
  tracked <- unname(files[c("codebook_items", "codebook_scales", "codebook_factors")])
  read <- file.path(analysis_plan$root, analysis_plan$meta$codebook_dir,
                    c("codebook_items.csv", "codebook_scales.csv", "codebook_factors.csv"))
  if (!identical(tracked, read)) {
    stop("The tracked codebook paths are not the three files zm_codebook() reads.")
  }
  check_codebook(codebook, analysis_plan)
  codebook
}

#' Stop unless the codebook's registered value sets are usable
#'
#' A pure stop/go check: it reads no participant data, computes nothing and
#' returns nothing. For school education it establishes one nonempty registered
#' value set with unique ordered codes and labels before participant data are
#' classified ([ap3_check_education_value_set()]).
#'
#' @param codebook Codebook from [zm_codebook()].
#' @param analysis_plan Configuration from [zm_config()]; part of the checked pair's
#'   signature, so that a later plan-dependent codebook rule has it at hand.
#' @return `TRUE`, invisibly.
check_codebook <- function(codebook, analysis_plan) {
  ap3_check_education_value_set(codebook)
  invisible(TRUE)
}

#' Standardised column of a scale or a covariate, as the codebook names it
#'
#' The codebook holds both names of every standardised variable side by side:
#' `scale_key`/`z_col` in `codebook_scales.csv`, and the YAML `covariates`
#' section. This is the only place a z column name is looked
#' up; no function derives one by appending a suffix to a key.
#'
#' @param keys Character vector of scale keys or covariate names.
#' @param codebook Codebook from [zm_codebook()].
#' @param population NULL for model-interface names; "all" or "known_gender"
#'   for the corresponding columns in the common analysis dataframe.
#' @return Character vector of standardised column names, in the order of `keys`.
zm_z_col <- function(keys, codebook, population = NULL) {
  field <- if (is.null(population)) "z_col" else switch(
    match.arg(population, c("all", "known_gender")),
    all = "z_col_all", known_gender = "z_col_known_gender"
  )
  pairs <- c(
    stats::setNames(as.character(codebook$scales[[field]]), as.character(codebook$scales$scale_key)),
    stats::setNames(as.character(codebook$covariates[[field]]), as.character(codebook$covariates$covariate))
  )
  z <- unname(pairs[as.character(keys)])
  if (anyNA(z)) {
    stop("The codebook has no standardised column for: ",
         paste(as.character(keys)[is.na(z)], collapse = ", "), ".")
  }
  z
}

#' Key of a standardised column, as the codebook names it
#'
#' The inverse of [zm_z_col()], and the only place a key is recovered from a
#' standardised column name: no function strips a suffix. A name the codebook
#' does not list as a standardised column is returned unchanged, so a mixed
#' vector of model terms (a gender contrast, an intercept) passes through.
#'
#' @param z_cols Character vector of column names.
#' @param codebook Codebook from [zm_codebook()].
#' @return Character vector of keys, in the order of `z_cols`.
zm_key_of_z_col <- function(z_cols, codebook) {
  pairs <- c(
    stats::setNames(as.character(codebook$scales$scale_key), as.character(codebook$scales$z_col)),
    stats::setNames(as.character(codebook$covariates$covariate), as.character(codebook$covariates$z_col))
  )
  x <- as.character(z_cols)
  key <- unname(pairs[x])
  ifelse(is.na(key), x, key)
}

#' Read the simulation ground truth
#'
#' Correlation blocks are returned as matrices with dimnames taken from their
#' `order` fields. Checks that `n_kept` equals `n_total` minus the exclusions
#' and that the correlation matrices are symmetric.
#'
#' The result is the BASE truth (the `clean` scenario); the deviations of a
#' scenario are applied by [zm_truth_scenario()], which the generator calls and
#' the prior-recovery simulation (R/prior_recovery.R) does not.
#'
#' @param path Path to `config/simulation_truth.yaml`.
#' @return Named list (the truth).
zm_truth <- function(path = file.path(zm_root(), "config", "simulation_truth.yaml")) {
  truth <- yaml::read_yaml(path)
  file <- basename(path)
  for (p in list(
    "seed", "n_total", "n_kept", "exclusions", "motive_correlations", "covariates",
    "true_beta", "outcome_residual_correlations", "measurement", "politics",
    "scenarios"
  )) {
    zm_require_field(truth, p, file)
  }
  if (is.null(truth$scenarios$export) || is.null(truth$scenarios[[truth$scenarios$export]])) {
    stop("scenarios$export in ", file, " must name a declared scenario.")
  }

  as_corr <- function(block, what) {
    m <- do.call(rbind, lapply(block$matrix, as.numeric))
    dimnames(m) <- list(block$order, block$order)
    if (!isTRUE(all.equal(m, t(m)))) stop(what, " in ", file, " is not symmetric.")
    if (any(diag(m) != 1)) stop(what, " in ", file, " must have unit diagonal.")
    m
  }
  truth$motive_correlations$matrix <- as_corr(truth$motive_correlations, "motive_correlations")
  truth$outcome_residual_correlations$matrix <- as_corr(
    truth$outcome_residual_correlations, "outcome_residual_correlations"
  )

  n_excluded <- sum(unlist(truth$exclusions))
  if (truth$n_kept != truth$n_total - n_excluded) {
    stop(
      "n_kept (", truth$n_kept, ") != n_total (", truth$n_total, ") - exclusions (",
      n_excluded, ") in ", file, "."
    )
  }
  truth
}
