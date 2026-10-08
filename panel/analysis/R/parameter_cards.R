# Comment-only parameter cards for human review
#
# config/analysis_plan.yaml remains the canonical source and zm_config() remains
# the runtime reader. These helpers only refresh marked comment blocks in the R
# files where a reviewer needs the corresponding values.

pc_find_root <- function(start = getwd()) {
  current <- normalizePath(start, mustWork = TRUE)
  repeat {
    if (file.exists(file.path(current, "config", "analysis_plan.yaml")) &&
        file.exists(file.path(current, "R", "ap1_exclusions.R"))) {
      return(current)
    }
    parent <- dirname(current)
    if (identical(parent, current)) stop("Could not find the panel analysis root from ", start, ".")
    current <- parent
  }
}

pc_yes_no <- function(value, field) {
  if (!is.logical(value) || length(value) != 1L || is.na(value)) {
    stop("Parameter-card field ", field, " must be true or false.")
  }
  if (value) "yes" else "no"
}

pc_scalar <- function(value, field) {
  if (is.null(value)) return("none")
  if (length(value) != 1L || is.na(value)) {
    stop("Parameter-card field ", field, " must be a single nonmissing value or null.")
  }
  as.character(value)
}

pc_require <- function(values, field) {
  if (is.null(values[[field]])) stop("Parameter-card source field exclusions.", field, " is missing.")
  values[[field]]
}

#' A configured number as a card reads it
#'
#' @param x The value from the YAML.
#' @param digits Significant digits, or `NULL` for the value as configured.
#' @return A character scalar without trailing zeros or scientific notation.
pc_number <- function(x, digits = NULL) {
  value <- as.numeric(x)
  if (!is.null(digits)) value <- signif(value, digits)
  format(value, trim = TRUE, drop0trailing = TRUE, scientific = FALSE)
}

#' Separator-suffixed pieces of an enumeration
#'
#' @param items Character vector.
#' @param separator Separator that stays at the end of the preceding piece.
#' @return `items` with `separator` appended to all but the last.
pc_pieces <- function(items, separator) {
  paste0(items, ifelse(seq_along(items) < length(items), separator, ""))
}

#' Fill pieces into comment lines
#'
#' @param pieces Character vector; a piece is never split across lines.
#' @param initial Comment prefix of the first line.
#' @param prefix Comment prefix of every continuation line.
#' @param width Character width a line may reach.
#' @return Character vector of comment lines.
pc_fill <- function(pieces, initial, prefix, width) {
  lines <- character()
  current <- ""
  start <- initial
  for (piece in pieces) {
    candidate <- if (nzchar(current)) paste(current, piece) else piece
    if (nzchar(current) && nchar(paste0(start, candidate)) > width) {
      lines <- c(lines, paste0(start, current))
      start <- prefix
      current <- piece
    } else {
      current <- candidate
    }
  }
  c(lines, paste0(start, current))
}

pc_ap1_card <- function(config_path) {
  analysis_plan <- yaml::read_yaml(config_path)
  values <- analysis_plan$exclusions
  if (!is.list(values)) stop("Parameter-card source section exclusions is missing.")

  consent <- pc_require(values, "consent_required_value")
  minimum_age <- pc_require(values, "min_age")
  maximum_age <- pc_require(values, "max_age")
  attention_1 <- pc_require(values, "attention_check_1_correct")
  attention_2 <- pc_require(values, "attention_check_2_correct")
  complete_status <- pc_require(values, "survey_status_complete")
  quota_full_status <- pc_require(values, "survey_status_quota_full")
  test_rows_removed <- pc_require(values, "test_rows_removed_upstream")
  divers_minimum <- pc_require(values, "gender_divers_min_n")

  c(
    "# BEGIN GENERATED PARAMETER CARD: AP1",
    "# Automatically generated from analysis_plan.yaml",
    "# Consent",
    paste0("#   Required response code: ", pc_scalar(consent, "consent_required_value")),
    "# Age",
    paste0("#   Minimum age: ", pc_scalar(minimum_age, "min_age"), " years"),
    paste0("#   Maximum age: ", pc_scalar(maximum_age, "max_age"), " years"),
    "# Attention checks",
    paste0("#   First correct response code: ", pc_scalar(attention_1, "attention_check_1_correct")),
    paste0("#   Second correct response code: ", pc_scalar(attention_2, "attention_check_2_correct")),
    "# Survey-flow statuses",
    paste0("#   Completed response: ", encodeString(pc_scalar(complete_status, "survey_status_complete"), quote = "\"")),
    paste0("#   Full quota cell: ", encodeString(pc_scalar(quota_full_status, "survey_status_quota_full"), quote = "\"")),
    "# Gender \"divers\"",
    paste0("#   Minimum group size for retention: ", pc_scalar(divers_minimum, "gender_divers_min_n")),
    "# Test and preview responses",
    paste0("#   Removed upstream: ", pc_yes_no(test_rows_removed, "test_rows_removed_upstream")),
    "# END GENERATED PARAMETER CARD: AP1"
  )
}

#' A codebook table the cards read
#'
#' @param name File name in `meta$codebook_dir` (`"codebook_scales.csv"`).
#' @param config_path Path of `analysis_plan.yaml`.
#' @param analysis_plan The parsed plan (for `meta$codebook_dir`).
#' @return Data frame of character columns.
pc_read_codebook <- function(name, config_path, analysis_plan) {
  root <- dirname(dirname(normalizePath(config_path, mustWork = TRUE)))
  path <- file.path(root, analysis_plan$meta$codebook_dir, name)
  if (!file.exists(path)) stop("Parameter-card source codebook not found: ", path)
  utils::read.csv(path, colClasses = "character", check.names = FALSE)
}

#' The scale codebook as the cards read it, in the order of its `order` column
#'
#' The `order` column of `codebook_scales.csv` is the one order of the scales;
#' every card that lists scales prints them in it, whatever order the plan
#' writes a list in.
#'
#' @param config_path Path of `analysis_plan.yaml`.
#' @param analysis_plan The parsed plan (for `meta$codebook_dir`).
#' @return The scale table, sorted by `order`.
pc_scale_codebook <- function(config_path, analysis_plan) {
  scales <- pc_read_codebook("codebook_scales.csv", config_path, analysis_plan)
  rank <- suppressWarnings(as.numeric(scales$order))
  if (anyNA(rank) || anyDuplicated(rank)) {
    stop("codebook_scales.csv: 'order' must give every scale a number of its own.")
  }
  scales[order(rank), , drop = FALSE]
}

#' Scale keys in the order of the scale codebook
#'
#' @param keys Scale keys (a set).
#' @param config_path Path of `analysis_plan.yaml`.
#' @param analysis_plan The parsed plan (for `meta$codebook_dir`).
#' @return `keys` in the codebook's order.
pc_order_scale_keys <- function(keys, config_path, analysis_plan) {
  ordered <- as.character(pc_scale_codebook(config_path, analysis_plan)$scale_key)
  keys <- as.character(unlist(keys))
  unknown <- setdiff(keys, ordered)
  if (length(unknown) > 0L) {
    stop("codebook_scales.csv has no scale: ", paste(unknown, collapse = ", "), ".")
  }
  ordered[ordered %in% keys]
}

#' Standardised column of a scale, read from the scale codebook
#'
#' The card prints the column names the pipeline fits, and those come from
#' `codebook_scales.csv` (`scale_key`/`z_col`), never from a pasted suffix.
#'
#' @param keys Scale keys.
#' @param config_path Path of `analysis_plan.yaml`.
#' @param analysis_plan The parsed plan (for `meta$codebook_dir`).
#' @return Character vector of standardised column names.
pc_z_col <- function(keys, config_path, analysis_plan) {
  scales <- pc_read_codebook("codebook_scales.csv", config_path, analysis_plan)
  z <- scales$z_col[match(as.character(keys), scales$scale_key)]
  if (anyNA(z)) {
    stop("codebook_scales.csv has no z_col for: ", paste(keys[is.na(z)], collapse = ", "), ".")
  }
  z
}

#' The regression columns of the configured covariates
#'
#' A metric covariate by its standardised column in the YAML `covariates` section,
#' the gender factor as it is, in the plan's order, as [zm_config()] maps them.
#'
#' @param keys Covariate keys of `regression$covariates`.
#' @param analysis_plan The parsed analysis-plan YAML.
#' @return Character vector of column names.
pc_covariate_columns <- function(keys, analysis_plan) {
  keys <- as.character(unlist(keys))
  columns <- vapply(analysis_plan$covariates, function(entry) entry$z_col, character(1))
  z <- unname(columns[keys])
  unknown <- is.na(z) & keys != "gender"
  if (any(unknown)) {
    stop("The YAML covariates section has no covariate: ", paste(keys[unknown], collapse = ", "), ".")
  }
  ifelse(is.na(z), keys, z)
}

#' The label of a set of scales, as the plan and the codebook give it
#'
#' The card form of [zm_label_scale_set()]: the plan's own label when it gives
#' one, else, from `codebook_scales.csv`, the scale's `label` for one scale and
#' the instrument with its subscales ("UMS-6 (intimacy and achievement)") for
#' several subscales of one instrument.
#'
#' @param entry The set or model entry of the plan.
#' @param keys Its scale keys.
#' @param config_path Path of `analysis_plan.yaml`.
#' @param analysis_plan The parsed plan (for `meta$codebook_dir`).
#' @return Character scalar.
pc_label_scale_set <- function(entry, keys, config_path, analysis_plan) {
  if (!is.null(entry$label)) return(pc_scalar(entry$label, "label"))
  scales <- pc_scale_codebook(config_path, analysis_plan)
  rows <- scales[scales$scale_key %in% as.character(unlist(keys)), , drop = FALSE]
  if (nrow(rows) == 1L) return(as.character(rows$label))
  instrument <- unique(trimws(sub("\\s*\\([^()]*\\)\\s*$", "", rows$scale)))
  if (length(instrument) != 1L) {
    stop("Parameter-card source entry ", entry$key, " needs a label: its scales belong to more than one instrument.")
  }
  subscales <- tolower(rows$subscale)
  paste0(instrument, " (", paste(subscales[-length(subscales)], collapse = ", "), " and ",
         subscales[length(subscales)], ")")
}

pc_ap6_formula_card <- function(config_path) {
  analysis_plan <- yaml::read_yaml(config_path)
  reg <- analysis_plan$regression
  needed <- c("outcomes", "motives", "covariates")
  if (any(vapply(needed, function(x) is.null(reg[[x]]), logical(1)))) stop("Parameter-card source section regression is incomplete.")
  # The standardised outcome columns are named by the scale codebook, as they
  # are in the code the card sits above; outcomes and motives in its order.
  outcomes <- pc_z_col(pc_order_scale_keys(reg$outcomes, config_path, analysis_plan), config_path, analysis_plan)
  # The predictor side as zm_config() builds it: the standardised motives,
  # then the covariates in the plan's order.
  terms <- c(pc_z_col(pc_order_scale_keys(reg$motives, config_path, analysis_plan), config_path, analysis_plan),
             pc_covariate_columns(reg$covariates, analysis_plan))
  c(
    "# BEGIN GENERATED PARAMETER CARD: AP6 FORMULA",
    "# Automatically generated from analysis_plan.yaml",
    "# One regression per outcome:",
    pc_fill(pc_pieces(outcomes, " |"), "#   ", "#   ", 100),
    "# The same predictors in every regression:",
    pc_fill(pc_pieces(terms, " +"), "#   ", "#   ", 100),
    "# END GENERATED PARAMETER CARD: AP6 FORMULA"
  )
}

#' The residual family of the primary fits, as its card prints it
#'
#' One value line: the family the plan states for the regressions. The
#' Student-t refit is a different verb on a different route; its residual
#' priors stand on the priors card.
#'
#' @param config_path Path of `analysis_plan.yaml`.
#' @return Character vector of comment lines.
pc_ap6_family_card <- function(config_path) {
  analysis_plan <- yaml::read_yaml(config_path)
  family <- pc_scalar(analysis_plan$regression$family, "regression$family")
  c(
    "# BEGIN GENERATED PARAMETER CARD: AP6 FAMILY",
    "# Automatically generated from analysis_plan.yaml",
    paste0("# Residuals: ", family, "."),
    "# END GENERATED PARAMETER CARD: AP6 FAMILY"
  )
}

pc_ap6_priors_card <- function(config_path) {
  analysis_plan <- yaml::read_yaml(config_path)
  priors <- analysis_plan$priors
  student <- analysis_plan$sensitivity$student_t
  prior_needed <- c("slope_sd_primary", "slope_sd_sweep", "sweep_labels", "gender_sd", "intercept_sd", "sigma_sd")
  if (any(vapply(prior_needed, function(x) is.null(priors[[x]]), logical(1))) ||
      is.null(student$nu_fixed) || is.null(student$sigma_scale_prior_sd)) stop("Parameter-card source sections priors or sensitivity$student_t are incomplete.")
  # The sweep line names the other widths; the primary width has its own line.
  sweep <- vapply(setdiff(as.numeric(priors$slope_sd_sweep), as.numeric(priors$slope_sd_primary)), function(x) {
    key <- pc_number(x)
    if (is.null(priors$sweep_labels[[key]])) stop("Parameter-card source priors$sweep_labels lacks ", key, ".")
    paste0(key, " (", pc_scalar(priors$sweep_labels[[key]], "priors$sweep_labels"), ")")
  }, character(1))
  gender_sd <- pc_number(priors$gender_sd)
  intercept_sd <- pc_number(priors$intercept_sd)
  sigma_sd <- pc_number(priors$sigma_sd)
  nu <- as.numeric(student$nu_fixed)
  scale_sd <- as.numeric(student$sigma_scale_prior_sd)
  student_line <- paste0("# Robustness refit with Student-t residuals: nu fixed at ", pc_number(nu),
                         "; scale half-normal(0, ", pc_number(scale_sd, digits = 4), ").")
  sweep_line <- paste0("Sweep of the slope width: ", paste(pc_pieces(sweep, " |"), collapse = " "), ".")
  c(
    "# BEGIN GENERATED PARAMETER CARD: AP6 PRIORS",
    "# Automatically generated from analysis_plan.yaml",
    paste0("# Slopes of the metric predictors: normal(0, ", pc_number(priors$slope_sd_primary), ")."),
    paste0("# Gender contrasts: normal(0, ", gender_sd, ")."),
    paste0("# Centred intercept: normal(0, ", intercept_sd, ")."),
    paste0("# Residual SD: half-normal(0, ", sigma_sd, ")."),
    pc_fill(strsplit(sweep_line, "[[:space:]]+")[[1]], "# ", "#   ", 100),
    student_line,
    "# END GENERATED PARAMETER CARD: AP6 PRIORS"
  )
}

#' The sampler settings of the fit, as the AP6 SAMPLING card prints them
#'
#' The settings that determine the posterior draws: chains, warmup and
#' post-warmup draws per chain, seed, backend and the sampler control of the
#' first attempt. The profile values are the preregistered `full` ones; a
#' profile that changes one of them is named in one further line.
#'
#' @param config_path Path of `analysis_plan.yaml`.
#' @return Character vector of comment lines.
pc_ap6_sampling_card <- function(config_path) {
  analysis_plan <- yaml::read_yaml(config_path)
  regression <- analysis_plan$regression
  profiles <- analysis_plan$profiles
  needed <- c("chains", "seed", "backend")
  if (any(vapply(needed, function(x) is.null(regression[[x]]), logical(1))) || is.null(profiles$full)) {
    stop("Parameter-card source sections regression or profiles are incomplete.")
  }
  # The preregistered profile supplies warmup and post-warmup draws.
  full <- profiles$full
  sampler_fields <- c(regression_warmup = "warmup draws per chain",
                      regression_iter_per_chain = "posterior draws per chain")
  if (any(vapply(names(sampler_fields), function(x) is.null(full[[x]]), logical(1)))) {
    stop("Parameter-card source section profiles$full lacks the sampler draws.")
  }
  # This verb fits once, on the sampler's own defaults. The retry and its
  # control belong to ap6_refit_if_needed(); see the AP6 RETRY card.
  control_line <- "# Sampler control: the sampler's defaults."
  # A profile that changes one of these values is named, so that a run under it
  # is not read as the preregistered one.
  changed <- setdiff(names(profiles), "full")
  changed <- changed[vapply(changed, function(name) {
    any(vapply(names(sampler_fields), function(field) {
      !identical(as.numeric(profiles[[name]][[field]]), as.numeric(full[[field]]))
    }, logical(1)))
  }, logical(1))]
  profile_lines <- vapply(changed, function(name) {
    values <- vapply(names(sampler_fields), function(field) {
      paste0(sampler_fields[[field]], " ", pc_number(profiles[[name]][[field]]))
    }, character(1))
    paste0("# Profile ", name, " instead: ", paste(values, collapse = ", "), ".")
  }, character(1), USE.NAMES = FALSE)
  c(
    "# BEGIN GENERATED PARAMETER CARD: AP6 SAMPLING",
    "# Automatically generated from analysis_plan.yaml",
    paste0("# Chains: ", pc_number(regression$chains), "."),
    paste0("# Warmup draws per chain: ", pc_number(full$regression_warmup), "."),
    paste0("# Posterior draws per chain: ", pc_number(full$regression_iter_per_chain), "."),
    paste0("# Seed: ", pc_number(regression$seed), "."),
    paste0("# Backend: ", pc_scalar(regression$backend, "regression$backend"), "."),
    control_line,
    profile_lines,
    "# END GENERATED PARAMETER CARD: AP6 SAMPLING"
  )
}

#' Profile-specific effective sample size lines for AP6 convergence cards
#'
#' @param profiles The `profiles` block of `analysis_plan.yaml`.
#' @return Character vector of comment lines, with `full` first.
pc_ap6_ess_lines <- function(profiles) {
  if (is.null(profiles$full)) {
    stop("Parameter-card source section profiles$full is missing.")
  }
  target <- function(name) {
    pc_number(pc_require(profiles[[name]], "regression_ess_target"))
  }
  full <- target("full")
  changed <- setdiff(names(profiles), "full")
  changed <- changed[vapply(changed, function(name) !identical(target(name), full), logical(1))]
  c(
    paste0("# Effective sample size required of every coefficient: ",
           full, " bulk and ", full, " tail."),
    vapply(changed, function(name) {
      value <- target(name)
      paste0("# Profile ", name, " instead: ", value, " bulk and ", value, " tail.")
    }, character(1), USE.NAMES = FALSE)
  )
}

#' Validity-gate threshold lines for AP6 convergence cards
#'
#' @param regression The `regression` block of `analysis_plan.yaml`.
#' @return Character vector of comment lines.
pc_ap6_diagnostic_lines <- function(regression) {
  gate <- regression$validity_gate
  c(
    paste0("# R-hat at most ", pc_number(pc_require(gate, "rhat_max")), "."),
    paste0("# Divergent transitions at most ",
           pc_number(pc_require(gate, "divergences_max")), "."),
    paste0("# Transitions at the maximum tree depth at most ",
           pc_number(pc_require(gate, "treedepth_hits_max")), "."),
    paste0("# BFMI of every chain at least ", pc_number(pc_require(gate, "bfmi_min")), "."),
    paste0("# Monte Carlo SE reported for medians and the ",
           pc_number(100 * as.numeric(pc_require(regression, "ci_level"))),
           "% interval endpoints.")
  )
}

#' The initial convergence card, above the first check of a fitted model
#'
#' @param config_path Path of `analysis_plan.yaml`.
#' @return Character vector of comment lines.
pc_ap6_initial_convergence_card <- function(config_path) {
  analysis_plan <- yaml::read_yaml(config_path)
  gate <- analysis_plan$regression$validity_gate
  if (is.null(gate) || is.null(analysis_plan$profiles)) {
    stop("Parameter-card source sections regression$validity_gate or profiles are incomplete.")
  }
  c(
    "# BEGIN GENERATED PARAMETER CARD: AP6 INITIAL CONVERGENCE",
    "# Automatically generated from analysis_plan.yaml",
    pc_ap6_ess_lines(analysis_plan$profiles),
    paste0("# ESS retries allowed after this check: at most ",
           pc_number(pc_require(gate, "max_ess_doublings")), " doublings."),
    "# END GENERATED PARAMETER CARD: AP6 INITIAL CONVERGENCE"
  )
}

#' The retry card, above the verb that refits a fit that did not pass
#'
#' `fit_primary_regressions()` fits once. This card states what `ap6_refit_if_needed()`
#' does when that fit falls short: the effective sample size it requires, how
#' often it may double the draws, and the sampler control of the one refit that
#' follows a sampler diagnostic failure.
#'
#' @param config_path Path of `analysis_plan.yaml`.
#' @return Character vector of comment lines.
pc_ap6_retry_card <- function(config_path) {
  analysis_plan <- yaml::read_yaml(config_path)
  regression <- analysis_plan$regression
  gate <- regression$validity_gate
  profiles <- analysis_plan$profiles
  if (is.null(gate) || is.null(profiles$full)) {
    stop("Parameter-card source sections regression$validity_gate or profiles are incomplete.")
  }
  c(
    "# BEGIN GENERATED PARAMETER CARD: AP6 RETRY",
    "# Automatically generated from analysis_plan.yaml",
    pc_ap6_ess_lines(profiles),
    paste0("# On a shortfall: posterior draws per chain doubled, at most ",
           pc_number(pc_require(gate, "max_ess_doublings")), " times."),
    pc_ap6_diagnostic_lines(regression),
    paste0("# On R-hat, divergences or tree-depth hits: one refit with doubled warmup, adapt_delta ",
           pc_number(pc_require(gate, "adapt_delta_retry")), ", max_treedepth ",
           pc_number(pc_require(gate, "max_treedepth_retry")), "."),
    "# END GENERATED PARAMETER CARD: AP6 RETRY"
  )
}

#' The final convergence card, above the gate on the returned fit
#'
#' @param config_path Path of `analysis_plan.yaml`.
#' @return Character vector of comment lines.
pc_ap6_final_convergence_card <- function(config_path) {
  analysis_plan <- yaml::read_yaml(config_path)
  gate <- analysis_plan$regression$validity_gate
  if (is.null(gate) || is.null(analysis_plan$profiles)) {
    stop("Parameter-card source sections regression$validity_gate or profiles are incomplete.")
  }
  c(
    "# BEGIN GENERATED PARAMETER CARD: AP6 FINAL CONVERGENCE",
    "# Automatically generated from analysis_plan.yaml",
    pc_ap6_ess_lines(analysis_plan$profiles),
    pc_ap6_diagnostic_lines(analysis_plan$regression),
    "# END GENERATED PARAMETER CARD: AP6 FINAL CONVERGENCE"
  )
}

# The condition the plan's demographic fill formulas and the route card state
# beside gender, because select_demographic_imputation_predictors() only
# uses gender when it is observed for every retained participant.
PC_AP3_GENDER_CONDITION <- "(only when observed for every retained participant)"

#' Priors of one fill model as a card reads them
#'
#' The plan states the widths; the card states the distributions the fill
#' builds from them, in the order the coefficient classes are listed here.
#'
#' @param priors Named list of prior widths from the YAML.
#' @param field Field path for the error message.
#' @return `"intercept normal(0, 1.5); sd normal(0, 1)"`.
pc_ap3_fill_priors <- function(priors, field) {
  if (!is.list(priors) || length(priors) == 0L) {
    stop("Parameter-card source field ", field, " is missing.")
  }
  widths <- c(intercept = "intercept_sd", sd = "sd_sd", b = "b_sd", sigma = "sigma_sd")
  present <- names(widths)[vapply(widths, function(key) !is.null(priors[[key]]), logical(1))]
  if (length(present) == 0L) stop("Parameter-card source field ", field, " states no prior width.")
  centre <- function(class) {
    if (identical(class, "intercept") && !is.null(priors$intercept_mean)) {
      pc_number(pc_scalar(priors$intercept_mean, field))
    } else {
      "0"
    }
  }
  paste(vapply(present, function(class) {
    paste0(class, " normal(", centre(class), ", ",
           pc_number(pc_scalar(priors[[widths[[class]]]], field)), ")")
  }, character(1)), collapse = "; ")
}

#' The route card of the fill, above the drop rule
#'
#' The rules that hold for the whole route rather than for one fit: which
#' respondents are dropped instead of filled, in which order the five verbs
#' run, and the rule for gender.
#'
#' @param config_path Path of `analysis_plan.yaml`.
#' @return Character vector of comment lines.
pc_ap3_fill_route_card <- function(config_path) {
  analysis_plan <- yaml::read_yaml(config_path)
  fill <- analysis_plan$missing_data$fill
  if (!is.list(fill)) stop("Parameter-card source section missing_data$fill is missing.")
  needed <- c("rule", "gender")
  if (any(vapply(needed, function(x) is.null(fill[[x]]), logical(1)))) {
    stop("Parameter-card source section missing_data$fill is incomplete.")
  }
  drop <- fill$drop
  if (is.null(drop$items_per_scale_more_than) || is.null(drop$demographics_missing_at_least)) {
    stop("Parameter-card source section missing_data$fill$drop is incomplete.")
  }
  order <- as.character(unlist(fill$demographics$order))
  words <- function(x) strsplit(x, "[[:space:]]+")[[1]]
  c(
    "# BEGIN GENERATED PARAMETER CARD: AP3 FILL ROUTE",
    "# Automatically generated from analysis_plan.yaml",
    pc_fill(words(paste0("Rule: ", pc_scalar(fill$rule, "missing_data$fill$rule"), ".")),
            "# ", "#   ", 100),
    pc_fill(words(paste0(
      "Dropped from all analyses instead of filled: more than ",
      pc_scalar(drop$items_per_scale_more_than, "missing_data$fill$drop$items_per_scale_more_than"),
      " missing items in one scale, or at least ",
      pc_scalar(drop$demographics_missing_at_least, "missing_data$fill$drop$demographics_missing_at_least"),
      " of the three demographics missing."
    )), "# ", "#   ", 100),
    pc_fill(words(paste0("Order: the drop, then the items of every scale with a gap, then ",
                         paste(order, collapse = ", "), ".")),
            "# ", "#   ", 100),
    pc_fill(words(paste0("Gender: ", pc_scalar(fill$gender, "missing_data$fill$gender"), ".")),
            "# ", "#   ", 100),
    "# END GENERATED PARAMETER CARD: AP3 FILL ROUTE"
  )
}

#' The card of one fill model, above its body
#'
#' The lines pair with the lines of the body beneath: the formula it fits, the
#' family, the priors built from the plan's widths, the value written into each
#' empty cell and the interval reported beside it.
#'
#' @param config_path Path of `analysis_plan.yaml`.
#' @param model `"items"` or the column name of one demographic fill.
#' @param marker Name of the generated block, as the file delimits it.
#' @return Character vector of comment lines.
pc_ap3_fill_model_card <- function(config_path, model, marker) {
  analysis_plan <- yaml::read_yaml(config_path)
  fill <- analysis_plan$missing_data$fill
  if (!is.list(fill) || is.null(fill$interval)) {
    stop("Parameter-card source section missing_data$fill is incomplete.")
  }
  items <- identical(model, "items")
  spec <- if (items) fill$items else fill$demographics[[model]]
  field <- if (items) "missing_data$fill$items" else paste0("missing_data$fill$demographics$", model)
  needed <- c("formula", "family", "value")
  if (!is.list(spec) || any(vapply(needed, function(x) is.null(spec[[x]]), logical(1)))) {
    stop("Parameter-card source section ", field, " is incomplete.")
  }
  words <- function(x) strsplit(x, "[[:space:]]+")[[1]]
  # Gender is a predictor of all three demographic fills only when it is
  # observed for every retained participant; otherwise
  # select_demographic_imputation_predictors() omits it from all three. A
  # formula that names gender without that condition would make the card claim
  # an unconditional predictor the code does not fit, so it stops here.
  formula_text <- pc_scalar(spec$formula, field)
  if (!items && grepl("gender", formula_text, fixed = TRUE) &&
      !grepl(PC_AP3_GENDER_CONDITION, formula_text, fixed = TRUE)) {
    stop("Parameter-card source field ", field, "$formula names gender without ",
         "\"", PC_AP3_GENDER_CONDITION, "\".")
  }
  # The item fill runs once per scale that has a gap; a demographic fill runs once.
  scope <- if (items) {
    paste0("# One fit per ", pc_scalar(fill$items$per, "missing_data$fill$items$per"),
           " with at least one gap.")
  } else {
    character()
  }
  c(
    paste0("# BEGIN GENERATED PARAMETER CARD: ", marker),
    "# Automatically generated from analysis_plan.yaml",
    scope,
    "# Formula:",
    pc_fill(words(formula_text), "#   ", "#     ", 100),
    paste0("# Family: ", pc_scalar(spec$family, field), "."),
    if (items) paste0("# Response categories: ", pc_number(pc_field(pc_section(analysis_plan, "scales"), "response_min", "scales")),
                      " to ", pc_number(pc_field(pc_section(analysis_plan, "scales"), "response_max", "scales")),
                      "   (scales.response_min, scales.response_max)."),
    pc_fill(words(paste0("Priors: ", pc_ap3_fill_priors(spec$priors, paste0(field, "$priors")), ".")),
            "# ", "#   ", 100),
    pc_fill(words(paste0("Value per cell: ", pc_scalar(spec$value, field), ".")), "# ", "#   ", 100),
    paste0("# Interval per filled cell: ",
           pc_number(100 * as.numeric(pc_scalar(fill$interval, "missing_data$fill$interval"))),
           "% posterior predictive."),
    paste0("# END GENERATED PARAMETER CARD: ", marker)
  )
}

#' The confirmatory models of one family, as their card prints them
#'
#' One line per model: its key, the registered scales its factors are, and its
#' publication label. The items are not printed — a factor's items are the
#' codebook's item set of that scale, and the card says so in one source line
#' instead of repeating a list the codebook owns. Factors and models are in the
#' order of the scale codebook, and a label the plan does not give is the
#' codebook's ([pc_label_scale_set()]), as [zm_config()] reads them.
#'
#' @param config_path Path of `analysis_plan.yaml`.
#' @param family One of the families `confirmatory_models$models` names.
#' @return Character vector of comment lines.
pc_ap4_cfa_card <- function(config_path, family) {
  analysis_plan <- yaml::read_yaml(config_path)
  planned <- analysis_plan$confirmatory_models$models
  if (!is.list(planned) || length(planned) == 0L) {
    stop("Parameter-card source section confirmatory_models$models is missing.")
  }
  planned <- Filter(function(entry) identical(as.character(entry$family), family), planned)
  if (length(planned) == 0L) {
    stop("Parameter-card source section confirmatory_models names no family ", family, ".")
  }
  scales_of <- lapply(planned, function(entry) {
    scales <- vapply(entry$factors, function(scale)
      pc_scalar(scale, "confirmatory_models$models$factors"), character(1), USE.NAMES = FALSE)
    pc_order_scale_keys(scales, config_path, analysis_plan)
  })
  ordered_keys <- as.character(pc_scale_codebook(config_path, analysis_plan)$scale_key)
  in_order <- order(vapply(scales_of, function(scales) match(scales[1], ordered_keys), integer(1)))
  planned <- planned[in_order]
  scales_of <- scales_of[in_order]
  definitions <- vapply(seq_along(planned), function(i) {
    paste0(pc_scalar(planned[[i]]$key, "confirmatory_models$models$key"), " = ",
           paste(scales_of[[i]], collapse = " + "))
  }, character(1))
  labels <- vapply(seq_along(planned), function(i) encodeString(
    pc_label_scale_set(planned[[i]], scales_of[[i]], config_path, analysis_plan), quote = "\""), character(1))
  marker <- pc_ap4_cfa_marker(family)
  c(
    paste0("# BEGIN GENERATED PARAMETER CARD: ", marker),
    "# Automatically generated from analysis_plan.yaml",
    paste0("#   ", formatC(definitions, width = -max(nchar(definitions))), "   ", labels),
    "# Items per factor: the codebook's item set of that scale, in codebook order.",
    paste0("# END GENERATED PARAMETER CARD: ", marker)
  )
}

#' The block name of the card of one confirmatory family
#'
#' @param family Family key of `confirmatory_models$models`.
#' @return The marker the file delimits the block with.
pc_ap4_cfa_marker <- function(family) {
  paste("AP4 CFA", toupper(gsub("_", " ", family, fixed = TRUE)))
}

#' One tuning setting as a card line reads it
#'
#' @param values The plan list the setting lives in.
#' @param key Its name in that list.
#' @param field Path of the list, for the error message.
#' @param label How the card names the setting.
#' @return A character scalar `label = value`.
pc_ap4_method_setting <- function(values, key, field, label) {
  if (is.null(values[[key]])) {
    stop("Parameter-card source field ", field, "$", key, " is missing.")
  }
  value <- pc_scalar(values[[key]], paste0(field, "$", key))
  if (!is.na(suppressWarnings(as.numeric(value)))) value <- pc_number(value)
  paste0(label, " = ", value)
}

#' The factor-number criteria as their card prints them
#'
#' One line per planned criterion, in the plan's order: the key the route
#' dispatches on, the index name its results carry, and the heading the report
#' prints for it. Beneath them, the tuning settings the plan fixes for the four
#' criteria that have any: the two parallel analyses, comparison data, the
#' classical Kaiser criterion and very simple structure. The VSS complexity is not a separate plan field — the
#' criterion key `vss_complexity_1` fixes it — so the card reads it off that key.
#'
#' @param config_path Path of `analysis_plan.yaml`.
#' @return Character vector of comment lines.
pc_ap4_factor_criteria_card <- function(config_path) {
  analysis_plan <- yaml::read_yaml(config_path)
  criteria <- analysis_plan$factor_analysis$factor_number_criteria
  if (!is.list(criteria) || length(criteria) == 0L) {
    stop("Parameter-card source section factor_analysis$factor_number_criteria is missing.")
  }
  methods <- analysis_plan$factor_analysis$factor_number_methods
  if (!is.list(methods)) {
    stop("Parameter-card source section factor_analysis$factor_number_methods is missing.")
  }
  methods_field <- "factor_analysis$factor_number_methods"
  parallel_field <- paste0(methods_field, "$parallel_analysis")
  comparison_field <- paste0(methods_field, "$comparison_data")
  vss_field <- paste0(methods_field, "$vss")
  parallel_settings <- c(
    pc_ap4_method_setting(methods$parallel_analysis, "reference_datasets",
                          parallel_field, "reference datasets"),
    pc_ap4_method_setting(methods$parallel_analysis, "percentile",
                          parallel_field, "percentile"),
    pc_ap4_method_setting(methods$parallel_analysis, "correlation_type",
                          parallel_field, "correlations"),
    pc_ap4_method_setting(methods$parallel_analysis, "random_correlation_type",
                          parallel_field, "reference-dataset correlations")
  )
  comparison_settings <- c(
    pc_ap4_method_setting(methods$comparison_data, "population_size",
                          comparison_field, "population size"),
    pc_ap4_method_setting(methods$comparison_data, "samples",
                          comparison_field, "samples"),
    pc_ap4_method_setting(methods$comparison_data, "alpha", comparison_field, "alpha"),
    pc_ap4_method_setting(methods$comparison_data, "max_iterations",
                          comparison_field, "maximum iterations"),
    pc_ap4_method_setting(methods$comparison_data, "correlation_type",
                          comparison_field, "correlations")
  )
  vss_keys <- vapply(criteria, function(entry)
    pc_scalar(entry$key, paste0(methods_field, "$key")), character(1))
  vss_key <- grep("^vss_complexity_", vss_keys, value = TRUE)
  if (length(vss_key) != 1L) {
    stop("Parameter-card source section factor_analysis$factor_number_criteria ",
         "names no single vss_complexity_* criterion.")
  }
  kaiser_settings <- pc_ap4_method_setting(methods$kaiser, "threshold",
                                           paste0(methods_field, "$kaiser"),
                                           "eigenvalue threshold")
  vss_settings <- c(
    paste0("complexity = ", sub("^vss_complexity_", "", vss_key),
           " (fixed by the criterion key ", vss_key, ")"),
    pc_ap4_method_setting(methods$vss, "maximum_factors", vss_field, "maximum factors")
  )
  field <- "factor_analysis$factor_number_criteria"
  keys <- vapply(criteria, function(entry) pc_scalar(entry$key, field), character(1))
  indices <- vapply(criteria, function(entry) pc_scalar(entry$index, field), character(1))
  labels <- vapply(criteria, function(entry)
    encodeString(pc_scalar(entry$label, field), quote = "\""), character(1))
  definitions <- paste0(formatC(keys, width = -max(nchar(keys))), " -> ",
                        formatC(indices, width = -max(nchar(indices))))
  words <- function(x) strsplit(x, "[[:space:]]+")[[1]]
  setting_lines <- function(heading, settings) {
    pc_fill(words(paste0(heading, ": ", paste(settings, collapse = "; "), ".")),
            "#   ", "#     ", 100)
  }
  c(
    "# BEGIN GENERATED PARAMETER CARD: AP9 FACTOR CRITERIA",
    "# Automatically generated from analysis_plan.yaml",
    paste0("# ", length(criteria), " criteria, each computed on its own; none selects a count."),
    paste0("#   ", definitions, "   ", labels),
    "# Tuning settings the plan fixes:",
    setting_lines("Parallel analysis (both extractions)", parallel_settings),
    setting_lines("Comparison data", comparison_settings),
    setting_lines("Classical Kaiser criterion", kaiser_settings),
    setting_lines("Very simple structure", vss_settings),
    "# END GENERATED PARAMETER CARD: AP9 FACTOR CRITERIA"
  )
}

#' The reliability bootstrap seed as its card prints it
#'
#' The plan fixes the RNG seed the participant bootstrap sets before each
#' scale's resampling. Every scale starts from this same seed.
#'
#' @param config_path Path of `analysis_plan.yaml`.
#' @return Character vector of comment lines.
pc_ap4_reliability_seed_card <- function(config_path) {
  analysis_plan <- yaml::read_yaml(config_path)
  values <- analysis_plan$reliability
  if (!is.list(values)) stop("Parameter-card source section reliability is missing.")
  if (is.null(values$bootstrap_seed)) {
    stop("Parameter-card source field reliability.bootstrap_seed is missing.")
  }
  c(
    "# BEGIN GENERATED PARAMETER CARD: AP4 RELIABILITY BOOTSTRAP SEED",
    "# Automatically generated from analysis_plan.yaml",
    paste0("#   Bootstrap seed: ", pc_number(values$bootstrap_seed),
           "   (reliability.bootstrap_seed)"),
    "# Set before each scale's participant resampling; every scale starts from this",
    "# same seed, so scales with the same number of participants draw the same",
    "# participant index sets.",
    "# END GENERATED PARAMETER CARD: AP4 RELIABILITY BOOTSTRAP SEED"
  )
}

#' The EFA item-set seed as its card prints it
#'
#' The plan fixes the RNG seed every materialised EFA item set carries; each
#' stochastic factor-number call starts from it.
#'
#' @param config_path Path of `analysis_plan.yaml`.
#' @return Character vector of comment lines.
pc_ap4_efa_seed_card <- function(config_path) {
  analysis_plan <- yaml::read_yaml(config_path)
  values <- analysis_plan$factor_analysis
  if (!is.list(values)) stop("Parameter-card source section factor_analysis is missing.")
  if (is.null(values$seed)) {
    stop("Parameter-card source field factor_analysis.seed is missing.")
  }
  c(
    "# BEGIN GENERATED PARAMETER CARD: AP9 EFA SEED",
    "# Automatically generated from analysis_plan.yaml",
    paste0("#   EFA seed: ", pc_number(values$seed), "   (factor_analysis.seed)"),
    "# Carried by every materialised item set; each stochastic factor-number call",
    "# starts from it, so parallel analysis, comparison data and VSS are reproducible.",
    "# END GENERATED PARAMETER CARD: AP9 EFA SEED"
  )
}

#' Section of the plan a card reads, or an error
#'
#' @param analysis_plan The parsed plan.
#' @param name Top-level section name.
#' @return The section as a list.
pc_section <- function(analysis_plan, name) {
  values <- analysis_plan[[name]]
  if (!is.list(values)) stop("Parameter-card source section ", name, " is missing.")
  values
}

#' A required plan field, or an error
#'
#' @param values The section.
#' @param field Field name.
#' @param section Section name for the message.
#' @return The field value.
pc_field <- function(values, field, section) {
  if (is.null(values[[field]])) {
    stop("Parameter-card source field ", section, ".", field, " is missing.")
  }
  values[[field]]
}

#' The combined EFA item sets as their card prints them
#'
#' One line per configured EFA set: its key and its member scales in the order
#' the set's columns are taken, the order of the scale codebook.
#'
#' @param config_path Path of `analysis_plan.yaml`.
#' @return Character vector of comment lines.
pc_ap4_efa_item_sets_card <- function(config_path) {
  analysis_plan <- yaml::read_yaml(config_path)
  sets <- pc_field(pc_section(analysis_plan, "factor_analysis"), "sets", "factor_analysis")
  sets <- Filter(function(set) isTRUE(set$efa), sets)
  keys <- vapply(sets, function(set) pc_scalar(set$key, "factor_analysis.sets.key"), character(1))
  scales <- vapply(sets, function(set) {
    paste(pc_order_scale_keys(set$scales, config_path, analysis_plan), collapse = ", ")
  }, character(1))
  c(
    "# BEGIN GENERATED PARAMETER CARD: AP9 EFA ITEM SETS",
    "# Automatically generated from analysis_plan.yaml",
    "# Each of the nine scales alone, then the combined sets   (factor_analysis.sets, efa: true)",
    paste0("#   ", formatC(keys, width = -max(nchar(keys))), " = ", scales),
    "# A combined set's columns are its scales' items, scale by scale in this order.",
    "# END GENERATED PARAMETER CARD: AP9 EFA ITEM SETS"
  )
}

#' The theoretical factor counts as their card prints them
#'
#' @param config_path Path of `analysis_plan.yaml`.
#' @return Character vector of comment lines.
pc_ap4_efa_expected_counts_card <- function(config_path) {
  analysis_plan <- yaml::read_yaml(config_path)
  values <- pc_section(analysis_plan, "factor_analysis")
  sets <- pc_field(values, "sets", "factor_analysis")
  single <- pc_field(values, "single_scale_expected_factors", "factor_analysis")
  keys <- vapply(sets, function(set) pc_scalar(set$key, "factor_analysis.sets.key"), character(1))
  counts <- vapply(sets, function(set)
    pc_number(pc_scalar(set$expected_factors, "factor_analysis.sets.expected_factors")),
    character(1))
  c(
    "# BEGIN GENERATED PARAMETER CARD: AP9 EFA EXPECTED COUNTS",
    "# Automatically generated from analysis_plan.yaml",
    "# Theoretical factor counts, reported beside the estimates; none is selected.",
    paste0("#   Each scale alone: ", pc_number(single),
           "   (factor_analysis.single_scale_expected_factors)"),
    paste0("#   ", formatC(keys, width = -max(nchar(keys))), " = ", counts,
           "   (factor_analysis.sets.expected_factors)"),
    "# END GENERATED PARAMETER CARD: AP9 EFA EXPECTED COUNTS"
  )
}

#' The EFA estimation and rotations as their card prints them
#'
#' @param config_path Path of `analysis_plan.yaml`.
#' @return Character vector of comment lines.
pc_ap4_efa_estimation_card <- function(config_path) {
  analysis_plan <- yaml::read_yaml(config_path)
  values <- pc_section(analysis_plan, "factor_analysis")
  f <- function(field) pc_field(values, field, "factor_analysis")
  settings <- f("rotation_settings")
  c(
    "# BEGIN GENERATED PARAMETER CARD: AP9 EFA ESTIMATION",
    "# Automatically generated from analysis_plan.yaml",
    paste0("#   Estimation: ", pc_scalar(f("efa_estimation_method"), "factor_analysis.efa_estimation_method"),
           " on the item set's polychoric matrix   (factor_analysis.efa_estimation_method)"),
    paste0("#   Primary rotation: ", pc_scalar(f("rotation_primary"), "factor_analysis.rotation_primary"),
           "   (factor_analysis.rotation_primary)"),
    paste0("#   Oblimin gamma: ", pc_number(pc_field(settings, "oblimin_gamma", "factor_analysis.rotation_settings")),
           "   (factor_analysis.rotation_settings.oblimin_gamma)"),
    paste0("#   Starting rotations: ", pc_number(pc_field(settings, "starting_rotations", "factor_analysis.rotation_settings")),
           "   (factor_analysis.rotation_settings.starting_rotations)"),
    "#   The same rotation settings apply to VSS trial solutions.",
    "#   A one-factor solution is unrotated.",
    "# END GENERATED PARAMETER CARD: AP9 EFA ESTIMATION"
  )
}

#' The loading-clarity cutoff as its card prints it
#'
#' @param config_path Path of `analysis_plan.yaml`.
#' @return Character vector of comment lines.
pc_ap4_loading_clarity_card <- function(config_path) {
  analysis_plan <- yaml::read_yaml(config_path)
  cutoff <- pc_field(pc_section(analysis_plan, "factor_analysis"), "loading_display_cutoff", "factor_analysis")
  c(
    "# BEGIN GENERATED PARAMETER CARD: AP9 LOADING CLARITY",
    "# Automatically generated from analysis_plan.yaml",
    paste0("#   Substantial loading: absolute value at least ", pc_number(cutoff),
           "   (factor_analysis.loading_display_cutoff)"),
    "#   Descriptive only; the item assignment uses the strongest loading, not this cutoff.",
    "# END GENERATED PARAMETER CARD: AP9 LOADING CLARITY"
  )
}

#' The network model as its card prints it
#'
#' @param config_path Path of `analysis_plan.yaml`.
#' @return Character vector of comment lines.
pc_ap7_network_model_card <- function(config_path) {
  analysis_plan <- yaml::read_yaml(config_path)
  values <- pc_section(analysis_plan, "network")
  f <- function(field) pc_field(values, field, "network")
  nodes <- pc_order_scale_keys(f("nodes"), config_path, analysis_plan)
  sweep <- vapply(f("g_prior_sweep"), pc_number, character(1))
  c(
    "# BEGIN GENERATED PARAMETER CARD: AP8 NETWORK MODEL",
    "# Automatically generated from analysis_plan.yaml",
    paste0("# Nodes (", length(nodes), ", in the order of codebook_scales.csv)   (network.nodes)"),
    pc_fill(pc_pieces(nodes, ","), "#   ", "#   ", 100),
    "# Estimation",
    paste0("#   Package: ", pc_scalar(f("package"), "network.package"), "   (network.package)"),
    paste0("#   Sweeps per fit: ", pc_number(f("iter")), "   (network.iter)"),
    paste0("#   Continuity indicator per node: ", pc_number(f("not_cont")),
           "   (network.not_cont; all nodes continuous)"),
    "# Priors",
    paste0("#   Edge inclusion prior: ", pc_number(f("g_prior")), "   (network.g_prior)"),
    paste0("#   Degrees of freedom: ", pc_number(f("df_prior")), "   (network.df_prior)"),
    paste0("#   Swept edge priors: ", paste(sweep, collapse = ", "),
           "   (network.g_prior_sweep; descriptive only)"),
    "# Edge decision rule",
    paste0("#   Present when the Bayes factor is at least ", pc_number(f("bf_include")),
           "   (network.bf_include)"),
    paste0("#   Absent when it is at most ", pc_number(f("bf_exclude")),
           "   (network.bf_exclude)"),
    "#   Inconclusive in between.",
    "# END GENERATED PARAMETER CARD: AP8 NETWORK MODEL"
  )
}

#' The network bagging as its card prints it
#'
#' @param config_path Path of `analysis_plan.yaml`.
#' @return Character vector of comment lines.
pc_ap7_network_bagging_card <- function(config_path) {
  analysis_plan <- yaml::read_yaml(config_path)
  values <- pc_section(analysis_plan, "network")
  f <- function(field) pc_field(values, field, "network")
  c(
    "# BEGIN GENERATED PARAMETER CARD: AP8 NETWORK BAGGING",
    "# Automatically generated from analysis_plan.yaml",
    paste0("#   Bootstrap samples: ", pc_number(f("B")), "   (network.B)"),
    paste0("#   Seed of bootstrap b: ", pc_number(f("seed_base")),
           " + b   (network.seed_base)"),
    paste0("#   Retry seed offset: ", pc_number(f("retry_seed_offset")),
           "   (network.retry_seed_offset; one same-data retry per failed fit)"),
    paste0("#   Minimum success rate: ", pc_number(f("min_success_rate")),
           "   (network.min_success_rate; below it the bag is computationally infeasible)"),
    "# END GENERATED PARAMETER CARD: AP8 NETWORK BAGGING"
  )
}

#' The reported interval level as a card prints it
#'
#' @param config_path Path of `analysis_plan.yaml`.
#' @param marker Card marker.
#' @param what One line naming what the interval is reported for.
#' @return Character vector of comment lines.
pc_interval_level_card <- function(config_path, marker, what) {
  analysis_plan <- yaml::read_yaml(config_path)
  level <- pc_field(pc_section(analysis_plan, "regression"), "ci_level", "regression")
  tail_mass <- (1 - as.numeric(level)) / 2
  c(
    paste0("# BEGIN GENERATED PARAMETER CARD: ", marker),
    "# Automatically generated from analysis_plan.yaml",
    paste0("# ", what),
    paste0("#   Interval level: ", pc_number(level), "   (regression.ci_level)"),
    paste0("#   Central, equal-tailed: quantiles ", pc_number(tail_mass), " and ",
           pc_number(1 - tail_mass)),
    paste0("# END GENERATED PARAMETER CARD: ", marker)
  )
}

#' The prior-predictive summary quantities as their card prints them
#'
#' @param config_path Path of `analysis_plan.yaml`.
#' @return Character vector of comment lines.
pc_ap6_prior_predictive_card <- function(config_path) {
  analysis_plan <- yaml::read_yaml(config_path)
  regression <- pc_section(analysis_plan, "regression")
  values <- regression$prior_predictive
  if (!is.list(values)) {
    stop("Parameter-card source section regression.prior_predictive is missing.")
  }
  f <- function(field) pc_field(values, field, "regression.prior_predictive")
  scales <- pc_section(analysis_plan, "scales")
  quantiles <- vapply(f("raw_scale_quantiles"), pc_number, character(1))
  c(
    "# BEGIN GENERATED PARAMETER CARD: AP6 PRIOR PREDICTIVE SUMMARY",
    "# Automatically generated from analysis_plan.yaml",
    "# Descriptive summaries of the prior-only draws",
    paste0("#   Percentile of the absolute standardised prediction: ",
           pc_number(f("absolute_summary_percentile")),
           "   (regression.prior_predictive.absolute_summary_percentile)"),
    paste0("#   Quantiles reported on the response scale: ", paste(quantiles, collapse = ", "),
           "   (regression.prior_predictive.raw_scale_quantiles)"),
    "# Response range the raw-scale draws are compared against",
    paste0("#   ", pc_number(pc_field(scales, "response_min", "scales")), " to ",
           pc_number(pc_field(scales, "response_max", "scales")),
           "   (scales.response_min, scales.response_max)"),
    "# END GENERATED PARAMETER CARD: AP6 PRIOR PREDICTIVE SUMMARY"
  )
}

#' The joint model's residual-correlation prior as its card prints it
#'
#' @param config_path Path of `analysis_plan.yaml`.
#' @return Character vector of comment lines.
pc_ap6_joint_rescor_card <- function(config_path) {
  analysis_plan <- yaml::read_yaml(config_path)
  rescor <- pc_field(pc_section(analysis_plan, "priors"), "rescor", "priors")
  c(
    "# BEGIN GENERATED PARAMETER CARD: AP7 JOINT RESIDUAL PRIOR",
    "# Automatically generated from analysis_plan.yaml",
    paste0("#   Residual-correlation prior: ", pc_scalar(rescor, "priors.rescor"),
           "   (priors.rescor; brms class rescor)"),
    "# The four outcome equations are fitted with correlated residuals; the other",
    "# priors of the joint fit are the ones on the AP6 PRIORS card.",
    "# END GENERATED PARAMETER CARD: AP7 JOINT RESIDUAL PRIOR"
  )
}

#' The AP7 interval level as its card prints it
#'
#' @param config_path Path of `analysis_plan.yaml`.
#' @return Character vector of comment lines.
pc_ap8_intervals_card <- function(config_path) {
  analysis_plan <- yaml::read_yaml(config_path)
  level <- pc_field(pc_section(analysis_plan, "regression"), "ci_level", "regression")
  tail_mass <- (1 - as.numeric(level)) / 2
  c(
    "# BEGIN GENERATED PARAMETER CARD: AP7 INTERVAL LEVEL",
    "# Automatically generated from analysis_plan.yaml",
    "# Every ap8_result_* verb below receives this level as its interval_level argument.",
    paste0("#   Interval level: ", pc_number(level), "   (regression.ci_level)"),
    paste0("#   Central, equal-tailed: quantiles ", pc_number(tail_mass), " and ",
           pc_number(1 - tail_mass)),
    "# END GENERATED PARAMETER CARD: AP7 INTERVAL LEVEL"
  )
}

#' Card lines that open and close a marked block
#'
#' @param marker Card marker.
#' @param body Comment lines between the two markers.
#' @return Character vector of comment lines.
pc_card <- function(marker, body) {
  c(paste0("# BEGIN GENERATED PARAMETER CARD: ", marker),
    "# Automatically generated from analysis_plan.yaml",
    body,
    paste0("# END GENERATED PARAMETER CARD: ", marker))
}

#' A plan list as a card prints it
#'
#' @param x A YAML sequence.
#' @return Comma-separated character scalar.
pc_list <- function(x) {
  paste(vapply(unlist(x, use.names = FALSE), function(v) pc_number_or_text(v), character(1)),
        collapse = ", ")
}

#' A plan value as text, numbers without trailing zeros
#'
#' @param x A scalar.
#' @return Character scalar.
pc_number_or_text <- function(x) {
  if (is.numeric(x)) pc_number(x) else as.character(x)
}

#' The response range and the reversal rule as their card prints them
#'
#' @param config_path Path of `analysis_plan.yaml`.
#' @return Character vector of comment lines.
pc_ap3_reversal_card <- function(config_path) {
  analysis_plan <- yaml::read_yaml(config_path)
  scales <- pc_section(analysis_plan, "scales")
  lo <- as.numeric(pc_field(scales, "response_min", "scales"))
  hi <- as.numeric(pc_field(scales, "response_max", "scales"))
  pc_card("AP3 REVERSAL", c(
    paste0("#   Response range: ", pc_number(lo), " to ", pc_number(hi),
           "   (scales.response_min, scales.response_max)"),
    paste0("#   A reverse-keyed item x becomes ", pc_number(lo + hi), " - x;",
           " the reverse-keyed items are the codebook's reverse_items.")
  ))
}

#' The derived covariates as their card prints them
#'
#' @param config_path Path of `analysis_plan.yaml`.
#' @return Character vector of comment lines.
pc_ap3_covariates_card <- function(config_path) {
  analysis_plan <- yaml::read_yaml(config_path)
  income <- pc_section(analysis_plan, "income")
  bands <- pc_field(income, "band_representative", "income")
  top <- pc_field(income, "hh_size_top_value", "income")
  age_bands <- pc_section(analysis_plan, "age_bands")
  first_years <- as.numeric(unlist(pc_field(age_bands, "first_years", "age_bands")))
  last_years <- c(first_years[-1L] - 1, as.numeric(pc_field(age_bands, "last_year", "age_bands")))
  age_labels <- as.character(unlist(pc_field(age_bands, "labels", "age_bands")))
  member_bands <- pc_section(analysis_plan, "income_bands_per_member")
  limits <- as.numeric(unlist(pc_field(member_bands, "limits", "income_bands_per_member")))
  member_labels <- as.character(unlist(pc_field(member_bands, "labels", "income_bands_per_member")))
  representatives <- sqrt(utils::head(limits, -1L) * utils::tail(limits, -1L))
  pc_card("AP3 COVARIATES", c(
    paste0("# Income per household member: band representative / min(household size, ",
           pc_number(top), ")"),
    pc_fill(pc_pieces(paste0(names(bands), " = ", vapply(bands, pc_number, character(1))), ";"),
            "#   Band representatives, euro per month: ", "#     ", 100),
    "#     (income.band_representative)",
    paste0("#   Household size top value: ", pc_number(top), "   (income.hh_size_top_value)"),
    "# Age and income per household member enter the analyses in bands, formed after the fill",
    paste0("#   Age bands, completed years, each as its ",
           pc_scalar(pc_field(age_bands, "representative", "age_bands"), "age_bands.representative"),
           "   (age_bands)"),
    pc_fill(pc_pieces(paste0(age_labels, " = ", vapply((first_years + last_years) / 2, pc_number, character(1))), ";"),
            "#     ", "#     ", 100),
    paste0("#   Income bands per household member, euro per month, right-open, each as the ",
           pc_scalar(pc_field(member_bands, "representative", "income_bands_per_member"),
                     "income_bands_per_member.representative"),
           " of its limits"),
    paste0("#     Limits: ", pc_list(limits), "   (income_bands_per_member.limits)"),
    pc_fill(pc_pieces(paste0(member_labels, " = ", vapply(round(representatives), pc_number, character(1))), ";"),
            "#     Representatives, rounded to whole euro: ", "#       ", 100)
  ))
}

#' The East/West split of the federal states as its card prints it
#'
#' @param config_path Path of `analysis_plan.yaml`.
#' @return Character vector of comment lines.
pc_ap3_east_west_card <- function(config_path) {
  analysis_plan <- yaml::read_yaml(config_path)
  region <- pc_section(analysis_plan, "east_west")
  pc_card("AP3 EAST WEST", c(
    "# East/West from the federal-state code",
    paste0("#   East: ", pc_list(pc_field(region, "east", "east_west")), "   (east_west.east)"),
    paste0("#   West: ", pc_list(pc_field(region, "west", "east_west")), "   (east_west.west)")
  ))
}

#' The two standardisation populations as their card prints them
#'
#' @param config_path Path of `analysis_plan.yaml`.
#' @return Character vector of comment lines.
pc_ap3_standardisation_card <- function(config_path) {
  analysis_plan <- yaml::read_yaml(config_path)
  regression <- pc_section(analysis_plan, "regression")
  nodes <- pc_order_scale_keys(pc_field(pc_section(analysis_plan, "network"), "nodes", "network"),
                               config_path, analysis_plan)
  pc_card("AP3 STANDARDISATION", c(
    "# All retained rows: the network nodes",
    pc_fill(pc_pieces(nodes, ","), "#   ", "#   ", 100),
    "#   (network.nodes)",
    "# Rows with a known gender: the regression variables",
    paste0("#   Covariates: ", pc_list(pc_field(regression, "covariates", "regression")),
           "; the metric ones are standardised from their bands: age as the band midpoint,",
           " income as the log of the band representative"),
    paste0("#   Within each model's own sample: ",
           pc_yes_no(pc_field(pc_section(analysis_plan, "missing_data"), "standardise_within_model", "missing_data"),
                     "missing_data.standardise_within_model"),
           "   (missing_data.standardise_within_model)")
  ))
}

#' The network input columns as their card prints them
#'
#' @param config_path Path of `analysis_plan.yaml`.
#' @return Character vector of comment lines.
pc_ap3_network_input_card <- function(config_path) {
  analysis_plan <- yaml::read_yaml(config_path)
  nodes <- pc_order_scale_keys(pc_field(pc_section(analysis_plan, "network"), "nodes", "network"),
                               config_path, analysis_plan)
  pc_card("AP3 NETWORK INPUT", c(
    paste0("# The standardised nodes, in the order of codebook_scales.csv   (network.nodes)"),
    pc_fill(pc_pieces(nodes, ","), "#   ", "#   ", 100)
  ))
}

#' The regression input columns as their card prints them
#'
#' @param config_path Path of `analysis_plan.yaml`.
#' @return Character vector of comment lines.
pc_ap3_regression_input_card <- function(config_path) {
  analysis_plan <- yaml::read_yaml(config_path)
  regression <- pc_section(analysis_plan, "regression")
  f <- function(field) pc_list(pc_field(regression, field, "regression"))
  # outcomes and motives are scales: in the order of the scale codebook
  scales <- function(field) {
    pc_list(pc_order_scale_keys(pc_field(regression, field, "regression"), config_path, analysis_plan))
  }
  pc_card("AP3 REGRESSION INPUT", c(
    paste0("#   Outcomes: ", scales("outcomes"), "   (regression.outcomes)"),
    paste0("#   Motives: ", scales("motives"), "   (regression.motives)"),
    paste0("#   Covariates: ", f("covariates"), "   (regression.covariates)")
  ))
}

#' The disclosure decisions of the scientific-use file as their card prints them
#'
#' @param config_path Path of `analysis_plan.yaml`.
#' @return Character vector of comment lines.
pc_ap3_scientific_use_card <- function(config_path) {
  analysis_plan <- yaml::read_yaml(config_path)
  suf <- pc_section(pc_section(analysis_plan, "data_files"), "scientific_use_file")
  section <- "data_files.scientific_use_file"
  f <- function(field) pc_field(suf, field, section)
  pc_card("AP3 SCIENTIFIC USE FILE", c(
    pc_fill(pc_pieces(as.character(unlist(f("keep"))), ","), "#   Shared: ", "#     ", 100),
    paste0("#     (", section, ".keep)"),
    paste0("#   Shared exactly as the analyses hold them: ", pc_list(f("analysis_value_columns")),
           "   (", section, ".analysis_value_columns)"),
    pc_fill(pc_pieces(as.character(unlist(f("drop"))), ","), "#   Not shared: ", "#     ", 100),
    paste0("#     (", section, ".drop)")
  ))
}

#' The reliability bootstrap intervals as their card prints them
#'
#' The number of resamples is the preregistered `full` profile's; the profile
#' replaces `reliability.bootstrap_n` at load.
#'
#' @param config_path Path of `analysis_plan.yaml`.
#' @return Character vector of comment lines.
pc_ap4_reliability_intervals_card <- function(config_path) {
  analysis_plan <- yaml::read_yaml(config_path)
  reliability <- pc_section(analysis_plan, "reliability")
  full <- pc_section(pc_section(analysis_plan, "profiles"), "full")
  level <- as.numeric(pc_field(reliability, "interval_level", "reliability"))
  tail_mass <- (1 - level) / 2
  pc_card("AP4 RELIABILITY INTERVALS", c(
    paste0("#   Participant resamples per scale: ",
           pc_number(pc_field(full, "reliability_bootstrap_n", "profiles.full")),
           "   (profiles.full.reliability_bootstrap_n)"),
    paste0("#   Percentile interval level: ", pc_number(level), "   (reliability.interval_level)"),
    paste0("#   Quantiles ", pc_number(tail_mass), " and ", pc_number(1 - tail_mass),
           " of the finite resample coefficients")
  ))
}

#' The reliability fallback trigger as its card prints it
#'
#' @param config_path Path of `analysis_plan.yaml`.
#' @return Character vector of comment lines.
pc_ap4_reliability_fallback_card <- function(config_path) {
  analysis_plan <- yaml::read_yaml(config_path)
  share <- pc_field(pc_section(analysis_plan, "reliability"), "omega_interval_min_success", "reliability")
  pc_card("AP4 RELIABILITY FALLBACK", c(
    paste0("#   Minimum share of resamples with a finite omega: ", pc_number(share),
           "   (reliability.omega_interval_min_success)"),
    "#   At or above it omega is reported with its interval; below it, omega without an interval.",
    "#   Alpha is reported separately with its own bootstrap interval."
  ))
}

#' The confirmatory estimation settings as their card prints them
#'
#' @param config_path Path of `analysis_plan.yaml`.
#' @return Character vector of comment lines.
pc_ap4_cfa_estimation_card <- function(config_path) {
  analysis_plan <- yaml::read_yaml(config_path)
  settings <- pc_section(pc_section(analysis_plan, "factor_analysis"), "cfa_settings")
  section <- "factor_analysis.cfa_settings"
  f <- function(field) pc_field(settings, field, section)
  pc_card("AP4 CFA ESTIMATION", c(
    paste0("#   Estimator: ", pc_scalar(f("estimator"), section), "   (", section, ".estimator)"),
    paste0("#   Items treated as ordered: ", pc_yes_no(f("ordered"), paste0(section, ".ordered")),
           "   (", section, ".ordered)"),
    paste0("#   Latent variances fixed to 1: ", pc_yes_no(f("std_lv"), paste0(section, ".std_lv")),
           "   (", section, ".std_lv)"),
    paste0("#   Factors orthogonal: ", pc_yes_no(f("orthogonal"), paste0(section, ".orthogonal")),
           "   (", section, ".orthogonal)")
  ))
}

#' The ranked residual count as its card prints it
#'
#' @param config_path Path of `analysis_plan.yaml`.
#' @return Character vector of comment lines.
pc_ap4_cfa_residuals_card <- function(config_path) {
  analysis_plan <- yaml::read_yaml(config_path)
  reporting <- pc_section(pc_section(analysis_plan, "factor_analysis"), "cfa_reporting")
  n <- pc_field(reporting, "ranked_residuals_n", "factor_analysis.cfa_reporting")
  pc_card("AP4 CFA RESIDUALS", c(
    paste0("#   Residual correlations shown per model: the ", pc_number(n),
           " largest in absolute standardised value   (factor_analysis.cfa_reporting.ranked_residuals_n)"),
    "#   Descriptive; no fit cutoff and no pass/fail."
  ))
}

#' The composition variables as their card prints them
#'
#' @param config_path Path of `analysis_plan.yaml`.
#' @return Character vector of comment lines.
pc_ap5_composition_card <- function(config_path) {
  analysis_plan <- yaml::read_yaml(config_path)
  demographics <- pc_field(pc_section(analysis_plan, "descriptives"), "demographics", "descriptives")
  pc_card("AP5 SAMPLE COMPOSITION", c(
    "# Described variables, in this order   (descriptives.demographics)",
    pc_fill(pc_pieces(as.character(unlist(demographics)), ","), "#   ", "#   ", 100)
  ))
}

#' The scale-correlation coefficient as its card prints it
#'
#' @param config_path Path of `analysis_plan.yaml`.
#' @return Character vector of comment lines.
pc_ap5_correlations_card <- function(config_path) {
  analysis_plan <- yaml::read_yaml(config_path)
  method <- pc_field(pc_section(analysis_plan, "descriptives"), "correlations", "descriptives")
  pc_card("AP5 SCALE CORRELATIONS", c(
    paste0("#   Coefficient: ", pc_scalar(method, "descriptives.correlations"),
           "   (descriptives.correlations; pairwise-complete observations)")
  ))
}

#' The power-scaling sensitivity settings as their card prints them
#'
#' @param config_path Path of `analysis_plan.yaml`.
#' @return Character vector of comment lines.
pc_ap6_prior_sensitivity_card <- function(config_path) {
  analysis_plan <- yaml::read_yaml(config_path)
  section <- "sensitivity.powerscale"
  values <- pc_section(pc_section(analysis_plan, "sensitivity"), "powerscale")
  f <- function(field) pc_field(values, field, section)
  pc_card("AP6 PRIOR SENSITIVITY", c(
    paste0("#   Prior blocks: ", pc_list(f("blocks")), "   (", section, ".blocks)"),
    paste0("#   Components scaled: ", pc_list(f("component")), "   (", section, ".component)"),
    paste0("#   Power-scaling alphas: ", pc_number(f("lower_alpha")), " and ", pc_number(f("upper_alpha")),
           "   (", section, ".lower_alpha, upper_alpha)"),
    paste0("#   Divergence measure: ", pc_scalar(f("div_measure"), section), "   (", section, ".div_measure)"),
    paste0("#   Sensitivity flagged at or above: ", pc_number(f("sensitivity_threshold")),
           "   (", section, ".sensitivity_threshold)"),
    "#   Descriptive; no AP10 classification changes."
  ))
}

#' The Table 3 prediction signs as their card prints them
#'
#' One row per outcome and one column per motive, in the order of the scale
#' codebook; ± permits either direction, and a dot marks an unspecified cell.
#'
#' @param config_path Path of `analysis_plan.yaml`.
#' @return Character vector of comment lines.
pc_ap7_prediction_signs_card <- function(config_path) {
  analysis_plan <- yaml::read_yaml(config_path)
  table <- pc_field(pc_section(analysis_plan, "predictions"), "table", "predictions")
  motives <- pc_order_scale_keys(names(table[[1]]), config_path, analysis_plan)
  outcomes <- pc_order_scale_keys(names(table), config_path, analysis_plan)
  width <- max(nchar(c(outcomes, "")))
  cell <- function(outcome, motive) {
    sign <- table[[outcome]][[motive]]
    if (is.null(sign)) stop("Parameter-card source field predictions.table.", outcome, " lacks ", motive, ".")
    if (identical(sign, "")) "." else as.character(sign)
  }
  row <- function(label, cells) {
    paste0("#   ", formatC(label, width = -width), "  ",
           paste(formatC(cells, width = -max(nchar(motives))), collapse = " "))
  }
  pc_card("RQ1 PREDICTION SIGNS", c(
    "# Predicted sign of each motive's slope   (predictions.table; ± = either direction; . = unspecified)",
    sub("[[:space:]]+$", "", row("", motives)),
    vapply(outcomes, function(outcome)
      sub("[[:space:]]+$", "", row(outcome, vapply(motives, function(m) cell(outcome, m), character(1)))),
      character(1), USE.NAMES = FALSE)
  ))
}

pc_replace_block <- function(lines, replacement, start_marker, end_marker, path) {
  start <- which(trimws(lines) == start_marker)
  end <- which(trimws(lines) == end_marker)
  if (length(start) != 1L || length(end) != 1L || start >= end) {
    stop("Expected exactly one valid parameter-card block in ", path, ".")
  }
  before <- if (start > 1L) lines[seq_len(start - 1L)] else character()
  after <- if (end < length(lines)) lines[seq.int(end + 1L, length(lines))] else character()
  indent <- sub("^([[:space:]]*).*", "\\1", lines[start])
  c(before, paste0(indent, replacement), after)
}

pc_update_parameter_cards <- function(root = pc_find_root(), check = FALSE) {
  config_path <- file.path(root, "config", "analysis_plan.yaml")
  # One file can carry several marked blocks; they are replaced in this order.
  cards <- list(
    list(file = "ap1_exclusions.R", marker = "AP1", lines = pc_ap1_card(config_path)),
    list(file = "ap3_fill.R", marker = "AP3 FILL ROUTE",
         lines = pc_ap3_fill_route_card(config_path)),
    list(file = "ap3_fill.R", marker = "AP3 FILL ITEMS",
         lines = pc_ap3_fill_model_card(config_path, "items", "AP3 FILL ITEMS")),
    list(file = "ap3_fill.R", marker = "AP3 FILL AGE",
         lines = pc_ap3_fill_model_card(config_path, "demo_age", "AP3 FILL AGE")),
    list(file = "ap3_fill.R", marker = "AP3 FILL HOUSEHOLD SIZE",
         lines = pc_ap3_fill_model_card(config_path, "demo_hh_members", "AP3 FILL HOUSEHOLD SIZE")),
    list(file = "ap3_fill.R", marker = "AP3 FILL INCOME BAND",
         lines = pc_ap3_fill_model_card(config_path, "demo_income_hh_net", "AP3 FILL INCOME BAND")),
    list(file = "ap4_factor_structure.R", marker = pc_ap4_cfa_marker("subscale"),
         lines = pc_ap4_cfa_card(config_path, "subscale")),
    list(file = "ap4_factor_structure.R", marker = pc_ap4_cfa_marker("ums_dopl"),
         lines = pc_ap4_cfa_card(config_path, "ums_dopl")),
    list(file = "ap4_factor_structure.R", marker = pc_ap4_cfa_marker("social_motives"),
         lines = pc_ap4_cfa_card(config_path, "social_motives")),
    list(file = "ap4_factor_structure.R", marker = pc_ap4_cfa_marker("asc"),
         lines = pc_ap4_cfa_card(config_path, "asc")),
    list(file = "ap4_factor_structure.R", marker = pc_ap4_cfa_marker("auth_orientation"),
         lines = pc_ap4_cfa_card(config_path, "auth_orientation")),
    list(file = "ap9_efa.R", marker = "AP9 EFA SEED",
         lines = pc_ap4_efa_seed_card(config_path)),
    list(file = "ap9_efa.R", marker = "AP9 EFA ITEM SETS",
         lines = pc_ap4_efa_item_sets_card(config_path)),
    list(file = "ap9_efa.R", marker = "AP9 FACTOR CRITERIA",
         lines = pc_ap4_factor_criteria_card(config_path)),
    list(file = "ap9_efa.R", marker = "AP9 EFA EXPECTED COUNTS",
         lines = pc_ap4_efa_expected_counts_card(config_path)),
    list(file = "ap9_efa.R", marker = "AP9 EFA ESTIMATION",
         lines = pc_ap4_efa_estimation_card(config_path)),
    list(file = "ap9_efa.R", marker = "AP9 LOADING CLARITY",
         lines = pc_ap4_loading_clarity_card(config_path)),
    list(file = "ap4_reliability.R", marker = "AP4 RELIABILITY BOOTSTRAP SEED",
         lines = pc_ap4_reliability_seed_card(config_path)),
    list(file = "ap6_regressions.R", marker = "AP6 INTERVAL LEVEL PRIOR WIDTH",
         lines = pc_interval_level_card(config_path, "AP6 INTERVAL LEVEL PRIOR WIDTH",
                                        "Coefficient stability across the three registered prior widths")),
    list(file = "ap6_regressions.R", marker = "AP6 PRIOR PREDICTIVE SUMMARY",
         lines = pc_ap6_prior_predictive_card(config_path)),
    list(file = "ap6_regressions.R", marker = "AP6 INTERVAL LEVEL POSTERIOR CHECK",
         lines = pc_interval_level_card(config_path, "AP6 INTERVAL LEVEL POSTERIOR CHECK",
                                        "Posterior-predictive replication summaries")),
    list(file = "ap6_regressions.R", marker = "AP6 INTERVAL LEVEL COEFFICIENTS",
         lines = pc_interval_level_card(config_path, "AP6 INTERVAL LEVEL COEFFICIENTS",
                                        "Regression coefficients and residual scale")),
    list(file = "ap6_regressions.R", marker = "AP6 INTERVAL LEVEL R2",
         lines = pc_interval_level_card(config_path, "AP6 INTERVAL LEVEL R2",
                                        "Explained variance (Bayesian R2)")),
    list(file = "ap6_regressions.R", marker = "AP7 JOINT RESIDUAL PRIOR",
         lines = pc_ap6_joint_rescor_card(config_path)),
    list(file = "ap7_joint_comparisons.R", marker = "AP7 INTERVAL LEVEL",
         lines = pc_ap8_intervals_card(config_path)),
    list(file = "ap8_network.R", marker = "AP8 NETWORK MODEL",
         lines = pc_ap7_network_model_card(config_path)),
    list(file = "ap8_network.R", marker = "AP8 NETWORK BAGGING",
         lines = pc_ap7_network_bagging_card(config_path)),
    list(file = "ap3_preprocessing.R", marker = "AP3 REVERSAL",
         lines = pc_ap3_reversal_card(config_path)),
    list(file = "ap3_preparation.R", marker = "AP3 EAST WEST",
         lines = pc_ap3_east_west_card(config_path)),
    list(file = "ap3_preparation.R", marker = "AP3 COVARIATES",
         lines = pc_ap3_covariates_card(config_path)),
    list(file = "ap3_preparation.R", marker = "AP3 STANDARDISATION",
         lines = pc_ap3_standardisation_card(config_path)),
    list(file = "ap3_preparation.R", marker = "AP3 NETWORK INPUT",
         lines = pc_ap3_network_input_card(config_path)),
    list(file = "ap3_preparation.R", marker = "AP3 REGRESSION INPUT",
         lines = pc_ap3_regression_input_card(config_path)),
    list(file = "ap3_data_files.R", marker = "AP3 SCIENTIFIC USE FILE",
         lines = pc_ap3_scientific_use_card(config_path)),
    list(file = "ap4_reliability.R", marker = "AP4 RELIABILITY INTERVALS",
         lines = pc_ap4_reliability_intervals_card(config_path)),
    list(file = "ap4_reliability.R", marker = "AP4 RELIABILITY FALLBACK",
         lines = pc_ap4_reliability_fallback_card(config_path)),
    list(file = "ap4_factor_structure.R", marker = "AP4 CFA ESTIMATION",
         lines = pc_ap4_cfa_estimation_card(config_path)),
    list(file = "ap4_cfa_reporting.R", marker = "AP4 CFA RESIDUALS",
         lines = pc_ap4_cfa_residuals_card(config_path)),
    list(file = "ap5_descriptives.R", marker = "AP5 SAMPLE COMPOSITION",
         lines = pc_ap5_composition_card(config_path)),
    list(file = "ap5_descriptives.R", marker = "AP5 SCALE CORRELATIONS",
         lines = pc_ap5_correlations_card(config_path)),
    list(file = "ap6_regressions.R", marker = "AP6 PRIOR SENSITIVITY",
         lines = pc_ap6_prior_sensitivity_card(config_path)),
    list(file = "ap10_inference.R", marker = "RQ1 PREDICTION SIGNS",
         lines = pc_ap7_prediction_signs_card(config_path)),
    list(file = "ap6_regressions.R", marker = "AP6 FORMULA", lines = pc_ap6_formula_card(config_path)),
    list(file = "ap6_regressions.R", marker = "AP6 FAMILY", lines = pc_ap6_family_card(config_path)),
    list(file = "ap6_regressions.R", marker = "AP6 PRIORS", lines = pc_ap6_priors_card(config_path)),
    list(file = "ap6_regressions.R", marker = "AP6 SAMPLING", lines = pc_ap6_sampling_card(config_path)),
    list(file = "ap6_regressions.R", marker = "AP6 INITIAL CONVERGENCE",
         lines = pc_ap6_initial_convergence_card(config_path)),
    list(file = "ap6_regressions.R", marker = "AP6 RETRY", lines = pc_ap6_retry_card(config_path)),
    list(file = "ap6_regressions.R", marker = "AP6 FINAL CONVERGENCE",
         lines = pc_ap6_final_convergence_card(config_path))
  )
  replacements <- list()
  for (card in cards) {
    path <- file.path(root, "R", card$file)
    if (is.null(replacements[[path]])) {
      current <- readLines(path, warn = FALSE)
      replacements[[path]] <- list(current = current, expected = current)
    }
    replacements[[path]]$expected <- pc_replace_block(
      replacements[[path]]$expected, card$lines,
      paste0("# BEGIN GENERATED PARAMETER CARD: ", card$marker),
      paste0("# END GENERATED PARAMETER CARD: ", card$marker), path
    )
  }
  changed <- any(vapply(replacements, function(x) !identical(x$current, x$expected), logical(1)))
  if (!changed) return(invisible(FALSE))
  if (isTRUE(check)) stop("Parameter card is stale. Run Rscript scripts/update_parameter_cards.R from panel/analysis.")
  for (path in names(replacements)) {
    replacement <- replacements[[path]]
    if (!identical(replacement$current, replacement$expected)) writeLines(replacement$expected, path, useBytes = TRUE)
  }
  invisible(TRUE)
}
