# Development-only check of saved estimates against synthetic generating values.
# Run from panel/analysis: Rscript scripts/check_synthetic_recovery.R
# Reads saved targets; fits nothing and does not write the production targets store.
# Output is CSV on stdout. Redirect it to a development file if needed.
# Latent generating coefficients and noisy scale-score estimands need not coincide.

#' Set the primary estimates beside their generating coefficients
#'
#' Metric predictors compare directly: the generating coefficient and the
#' estimate are both in outcome standard deviations per predictor standard
#' deviation. The generator applies the gender coefficient to the standardised
#' 0/1 indicator for men, so the file states it per standard deviation of that
#' indicator, whereas the regressions estimate the difference between men and
#' women. The gender coefficient is therefore divided by the indicator's
#' standard deviation in the generated data, and a contrast against men
#' (`genderfemale`) is compared with its sign reversed. An estimate of a fit
#' that failed its sampling checks is not shown. The comparison stays
#' approximate: the generating coefficients describe latent scores, the
#' estimates scale scores.
#'
#' @param coefficients Coefficients of the primary fits (target
#'   `report_primary_coefficients`): `outcome`, `term`, `term_type`,
#'   `estimate`, `fit_valid` and the two quantile columns of the interval
#'   (`q2.5`, `q97.5` at a 95% level).
#' @param generating Tibble from [tabulate_generating_coefficients()].
#' @param male_sd Standard deviation of the 0/1 indicator for men in the
#'   generated data.
#' @return Tibble, one row per generating coefficient: `outcome`, `predictor`,
#'   `term` (the fitted term, `NA` without one), `generating` (as the file
#'   states it), `generating_compared` (on the scale of the estimate),
#'   `estimate`, `lower`, `upper`, `ci_level` (percent), `excludes_zero`,
#'   `contains_generating`, `fit_valid`.
compare_estimates_with_generating_values <- function(coefficients, generating, male_sd) {
  b <- tibble::as_tibble(coefficients)
  b <- b[b$term_type %in% c("predictor", "covariate"), , drop = FALSE]
  q_cols <- grep("^q[0-9.]+$", names(b), value = TRUE)
  if (length(q_cols) != 2L) {
    stop("compare_estimates_with_generating_values(): the coefficients need two interval columns q<lower>, q<upper>.")
  }
  probs <- as.numeric(sub("^q", "", q_cols))
  q_cols <- q_cols[order(probs)]
  term <- as.character(b$term)
  key <- ifelse(term %in% c("gendermale", "genderfemale"), "male", term)
  hit <- match(paste(generating$outcome, generating$predictor), paste(b$outcome, key))
  flip <- term[hit] %in% "genderfemale"
  valid <- b$fit_valid[hit] %in% TRUE
  pick <- function(column) ifelse(valid, b[[column]][hit], NA_real_)
  estimate <- pick("estimate")
  lo <- pick(q_cols[1])
  hi <- pick(q_cols[2])
  lower <- ifelse(flip, -hi, lo)
  upper <- ifelse(flip, -lo, hi)
  compared <- ifelse(generating$predictor == "male", generating$generating / male_sd, generating$generating)
  tibble::tibble(
    outcome = generating$outcome,
    predictor = generating$predictor,
    term = term[hit],
    generating = generating$generating,
    generating_compared = compared,
    estimate = ifelse(flip, -estimate, estimate),
    lower = lower,
    upper = upper,
    ci_level = diff(sort(probs)),
    excludes_zero = lower > 0 | upper < 0,
    contains_generating = compared >= lower & compared <= upper,
    fit_valid = b$fit_valid[hit]
  )
}

#' Word how closely the estimates match the generating coefficients
#'
#' Three facts in turn: whether each nonzero coefficient's interval excluded
#' zero on the side of its generating value, whether each zero coefficient's
#' interval included zero, and how many intervals contained the generating
#' value. Exceptions are named with their estimate.
#'
#' @param comparison Tibble from [compare_estimates_with_generating_values()].
#' @param labels Labels from [rh_labels()].
#' @param motives Motive keys.
#' @return Character scalar.
describe_generating_agreement <- function(comparison, labels, motives) {
  available <- !is.na(comparison$estimate)
  nonzero <- comparison$generating != 0
  same_side <- comparison$excludes_zero %in% TRUE & sign(comparison$estimate) == sign(comparison$generating)
  name_cells <- function(rows) {
    paste0(name_predictors_in_text(rows$predictor, labels), " for ", name_scales_in_text(rows$outcome, labels),
           ", ", format_estimate_with_interval(rows$estimate, rows$lower, rows$upper))
  }
  # "All 13 ... had ...", "Of the 19 ..., 18 had ...; the exception was ...".
  count_sentence <- function(selected, hits, singular, plural, what) {
    n <- sum(selected & available)
    k <- sum(selected & available & hits)
    if (n == 0L) return("")
    misses <- comparison[selected & available & !hits, , drop = FALSE]
    if (n == 1L) {
      return(paste0("The ", singular, if (k == 1L) " had " else " did not have ", what,
                    if (k == 0L) paste0(": ", name_cells(misses)) else "", "."))
    }
    if (k == n) {
      return(paste0(if (n == 2L) "Both " else paste("All", rh_fmt_n(n), ""), plural, " had ", what, "."))
    }
    paste0("Of the ", rh_fmt_count(n), " ", plural, ", ", rh_fmt_count(k), " had ", what,
           "; the exception", if (nrow(misses) > 1L) "s were " else " was ", join_words_with_and(name_cells(misses)), ".")
  }
  effect <- count_sentence(nonzero, same_side, "nonzero generating coefficient", "nonzero generating coefficients",
                           "a CrI that excluded zero on the side of the generating value")
  zero <- count_sentence(!nonzero, !comparison$excludes_zero %in% TRUE, "coefficient generated as zero",
                         "coefficients generated as zero", "a CrI that included zero")
  contains <- comparison$contains_generating %in% TRUE & available
  n <- sum(available)
  k <- sum(contains)
  motive_n <- sum(comparison$predictor %in% motives & available)
  motive_note <- if (k < n && motive_n > 1L && all(contains[comparison$predictor %in% motives & available])) {
    paste0(", including ", if (motive_n == 2L) "both" else paste("all", rh_fmt_n(motive_n)), " motive coefficients")
  } else ""
  contained <- if (n == 0L) "" else if (n == 1L) {
    paste0("The CrI ", if (k == 1L) "contained" else "did not contain", " the generating value.")
  } else if (k == n) {
    paste0("The CrI contained the generating value for all ", rh_fmt_n(n), " coefficients.")
  } else {
    paste0("The CrI contained the generating value for ", rh_fmt_n(k), " of the ", rh_fmt_n(n), " coefficients",
           motive_note, ".")
  }
  missing <- if (any(!available)) {
    paste0(rh_fmt_count(sum(!available), capitalize = TRUE), " coefficient",
           if (sum(!available) > 1L) "s have" else " has", " no estimate, because the fit did not pass its sampling checks.")
  } else ""
  sentences <- c(effect, zero, contained, missing)
  paste(sentences[nzchar(sentences)], collapse = " ")
}

#' Word how the gender coefficients are put on the scale of the estimates
#'
#' @param comparison Tibble from [compare_estimates_with_generating_values()].
#' @param male_sd Standard deviation of the indicator for men in the generated
#'   data.
#' @param labels Labels from [rh_labels()].
#' @return Character scalar.
describe_gender_conversion <- function(comparison, male_sd, labels) {
  rows <- comparison[comparison$predictor == "male" & comparison$generating != 0, , drop = FALSE]
  if (nrow(rows) == 0L) {
    return("All gender coefficients of the generating file are zero.")
  }
  paste0(
    "The generating file states the gender coefficients per standard deviation of a 0/1 indicator for men: ",
    join_words_with_and(paste0(rh_fmt(rows$generating), " for ", name_scales_in_text(rows$outcome, labels))),
    ". Divided by that indicator's standard deviation in the generated data, ", rh_fmt(male_sd),
    ", they give the difference between men and women that the regressions estimate, ",
    join_words_with_and(rh_fmt(rows$generating_compared)), "; the table shows these values."
  )
}

#' Word how income is compared
#'
#' @param comparison Tibble from [compare_estimates_with_generating_values()].
#' @return Character scalar.
describe_income_comparison <- function(comparison) {
  rows <- comparison[comparison$predictor == "income", , drop = FALSE]
  paste0(
    "The regressions use the logarithm of the income band representative per household member, the generating file the exact income itself",
    if (nrow(rows) > 0L && all(rows$generating == 0)) "; its generating coefficients are zero for every outcome" else "",
    "."
  )
}

#' The display table of the generating coefficients beside the estimates
#'
#' One row per predictor; for each outcome, its generating coefficient (on the
#' scale of the estimate) and the estimate with its interval in one cell.
#'
#' @param comparison Tibble from [compare_estimates_with_generating_values()].
#' @param labels Labels from [rh_labels()].
#' @param analysis_plan Configuration (`regression$outcomes`, `regression$motives`).
#' @param engine See [rh_use_gt()].
#' @return A `gt_tbl` (with an outcome spanner per column pair) or a kable.
tabulate_generating_comparison <- function(comparison, labels, analysis_plan, engine = "auto") {
  outcomes <- as.character(analysis_plan$regression$outcomes)
  predictors <- c(as.character(analysis_plan$regression$motives), "age", "male", "income")
  predictors <- c(predictors, setdiff(unique(comparison$predictor), predictors))
  row_labels <- rh_label(predictors, labels)
  row_labels[predictors == "male"] <- "Gender (men − women)"
  df <- tibble::tibble(predictor = row_labels)
  col_labels <- c(predictor = "Predictor")
  ci_label <- rh_ci_label(comparison$ci_level[!is.na(comparison$ci_level)][1])
  for (outcome in outcomes) {
    rows <- comparison[comparison$outcome == outcome, , drop = FALSE]
    hit <- match(predictors, rows$predictor)
    gen <- paste0("generating_", outcome)
    est <- paste0("estimate_", outcome)
    df[[gen]] <- rh_fmt(rows$generating_compared[hit])
    df[[est]] <- format_estimate_with_interval(rows$estimate[hit], rows$lower[hit], rows$upper[hit])
    col_labels[[gen]] <- "Generating"
    col_labels[[est]] <- paste0("Estimate [", ci_label, "]")
  }
  tab <- rh_table(df, col_labels = col_labels, engine = engine)
  if (inherits(tab, "gt_tbl")) {
    for (outcome in outcomes) {
      tab <- gt::tab_spanner(tab, label = rh_label(outcome, labels),
                             columns = dplyr::all_of(paste0(c("generating_", "estimate_"), outcome)))
    }
  }
  tab
}


run_synthetic_recovery_check <- function() {
  for (f in c("config.R", "simulate_testdata.R", "report_helpers.R", "report_synthetic_data.R")) {
    source(file.path("R", f), local = .GlobalEnv)
  }
  store <- targets::tar_config_get("store")
  plan <- targets::tar_read(pipeline_config, store = store)
  truth <- read_export_generating_values(file.path("config", "simulation_truth.yaml"))
  latent <- read_generated_latent_scores(file.path("data", "synthetic", "zm_panel_synthetic_latent.csv"))
  coefficients <- targets::tar_read(report_primary_coefficients, store = store)
  comparison <- compare_estimates_with_generating_values(
    coefficients, order_generating_rows(tabulate_generating_coefficients(truth), plan), stats::sd(latent$male)
  )
  utils::write.csv(comparison, stdout(), row.names = FALSE)
  invisible(comparison)
}

if (sys.nframe() == 0L) run_synthetic_recovery_check()
