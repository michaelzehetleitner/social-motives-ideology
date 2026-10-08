# LAYER 3 — report helpers: table and figure builders for report/results.qmd
#
# Pure presentation code. Every function takes pipeline objects (targets) and
# returns a ggplot, a gt table (kable fallback) or a formatted string; no
# analysis decision is made here and no numeric constant of the analysis plan
# appears here (thresholds for flags are passed in through `analysis_plan`).
#
# Conventions (APA-like): posterior medians and 95 % credible intervals with
# two decimals; bounded indices (correlations, reliabilities, fit indices,
# probabilities) without a leading zero; counts as integers; missing as "—".

# ---- formatting -------------------------------------------------------------

#' Format numbers APA-style
#'
#' @param x Numeric vector.
#' @param digits Number of decimals (default 2, coefficients).
#' @param bounded Logical; `TRUE` for indices that cannot exceed 1 in absolute
#'   value (correlations, reliabilities, fit indices, probabilities): the
#'   leading zero is dropped (".85", "-.12").
#' @return Character vector; a negative number carries a true minus sign
#'   (U+2212) rather than a hyphen-minus, and `NA`, `NaN` and infinite values
#'   become "—".
rh_fmt <- function(x, digits = 2, bounded = FALSE) {
  out <- formatC(x, format = "f", digits = digits, big.mark = ",")
  out <- trimws(out)
  if (isTRUE(bounded)) {
    out <- sub("^(-?)0\\.", "\\1.", out)
  }
  # APA 7 and the DGPs Richtlinien set a minus sign, not a hyphen. The
  # substitution runs after the bounded rule so that "-0.04" has
  # already become "-.04".
  out <- sub("^-", "−", out)
  # "Inf" is not a quantity this design reports: where a bound exists it is
  # printed instead (rh_network_bf()), and elsewhere an em dash says that the
  # value is not available — a scale with no observed case has no minimum, not
  # an infinite one.
  out[is.infinite(x)] <- "—"
  out[is.na(x)] <- "—"
  out
}

#' A zero-centred normal or half-normal prior in brms notation
#'
#' The prose of both reports states the priors numerically, and no number of a
#' specification is typed into a document: the mean is zero
#' by definition of these priors and the SD comes from the configuration.
#'
#' @param sd Prior standard deviation.
#' @param half Logical; a half-normal (the residual-SD priors).
#' @return Character scalar such as `"normal(0, 0.20)"`.
rh_normal_prior <- function(sd, half = FALSE) {
  paste0(if (isTRUE(half)) "half-normal(" else "normal(", 0, ", ", rh_fmt(sd), ")")
}

#' Count as a word below ten, as a numeral from ten on
#'
#' APA's Numbers and Statistics Guide writes counts below ten as words unless
#' they are a statistic, a member of a series with larger numbers, or a
#' measurement. The counting call sites of both reports ("reported for the nine
#' scales", "eight were confirmed") use this helper; `rh_fmt_n()` stays the
#' formatter for statistics and sample sizes.
#'
#' @param x Numeric vector of counts.
#' @param capitalize Logical; capitalise the first letter of the word form, for
#'   a count that begins a sentence.
#' @return Character vector.
rh_fmt_count <- function(x, capitalize = FALSE) {
  words <- c("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
  n <- suppressWarnings(as.numeric(x))
  out <- rh_fmt_n(n)
  word_case <- !is.na(n) & n == round(n) & n >= 0 & n <= 9
  out[word_case] <- words[n[word_case] + 1L]
  if (isTRUE(capitalize)) {
    out <- paste0(toupper(substring(out, 1, 1)), substring(out, 2))
  }
  out
}

#' Format a credible interval as "[lo, hi]"
#'
#' @param lo,hi Numeric vectors, interval bounds.
#' @param digits Number of decimals.
#' @param bounded See [rh_fmt()].
#' @return Character vector.
rh_fmt_ci <- function(lo, hi, digits = 2, bounded = FALSE) {
  out <- paste0("[", rh_fmt(lo, digits, bounded), ", ", rh_fmt(hi, digits, bounded), "]")
  out[is.na(lo) | is.na(hi)] <- "—"
  out
}

#' An estimate and its interval in one cell, "0.40 [0.33, 0.47]"
#'
#' Estimate and uncertainty stand in one cell everywhere. A missing estimate is an em dash; a missing interval leaves the
#' estimate alone.
#'
#' @param est,lo,hi Numeric vectors.
#' @param digits,bounded See [rh_fmt()].
#' @param bold Logical vector; `TRUE` sets the estimate, not its interval, in
#'   Markdown bold.
#' @return Character vector.
rh_fmt_est_ci <- function(est, lo, hi, digits = 2, bounded = FALSE, bold = FALSE) {
  shown <- rh_fmt(est, digits, bounded)
  shown <- ifelse(rep_len(bold %in% TRUE, length(shown)) & !is.na(est), paste0("**", shown, "**"), shown)
  out <- paste(shown, rh_fmt_ci(lo, hi, digits, bounded))
  out[!is.na(est) & (is.na(lo) | is.na(hi))] <- shown[!is.na(est) & (is.na(lo) | is.na(hi))]
  out[is.na(est)] <- "—"
  out
}

#' The pattern of a cell holding an estimate and its interval, or a range
#'
#' "0.24 [0.13, 0.35]", the estimate possibly in Markdown bold and followed by
#' a superscript, the interval possibly followed by one; or a range alone,
#' "[0.13, 0.35]".
#'
#' @return A Perl regular expression; its groups are the bold marks, the
#'   whole and the decimal part of each number and the two superscripts (empty
#'   for a range alone).
rh_interval_cell_pattern <- function() {
  number <- "((?:−|-)?[0-9]*)((?:\\.[0-9]+)?)"
  raised <- "((?:<sup>[^<]*</sup>)?)"
  paste0("^(?:(\\*\\*)?", number, "(\\*\\*)?", raised, " )?\\[", number, ", ", number, "\\]", raised, "$")
}

#' Line up the decimal points of estimate-and-interval cells in HTML
#'
#' Each number of a cell is split at its decimal point into two boxes: the
#' whole part right-aligned, as wide as the widest of its kind in the column,
#' and the decimal part left-aligned, as wide as most of its kind; a number
#' with more decimals widens only its own box. The estimates, the lower and
#' the upper bounds then each line up at the decimal point, whatever their
#' sign. A range without an estimate lines up the same way. A bold estimate
#' stays bold; cells of another form pass unchanged.
#'
#' @param x Character vector, the cells of one column.
#' @return Character vector of HTML.
rh_align_interval_cells <- function(x) {
  x <- as.character(x)
  parts <- regmatches(x, regexec(rh_interval_cell_pattern(), x, perl = TRUE))
  ok <- lengths(parts) == 11L
  if (!any(ok)) return(x)
  m <- do.call(rbind, parts[ok])[, -1, drop = FALSE]
  # widths in digits (ch): a minus sign is a little wider than a digit, a
  # decimal point about half as wide, a superscript sign about two thirds
  whole <- function(s) nchar(s) + 0.2 * grepl("^(−|-)", s)
  decimals <- function(s) ifelse(nzchar(s), nchar(s) - 0.4, 0)
  raised <- function(s) 0.7 * nchar(gsub("<[^>]+>", "", s))
  box <- function(s, width, align, common = FALSE) {
    if (max(width) == 0) return(s)
    if (common) {
      counts <- table(width)
      width <- as.numeric(names(counts)[which.max(counts)])
    }
    sprintf("<span style=\"display:inline-block;%s:%.1fch;text-align:%s\">%s</span>",
            if (common) "min-width" else "width", ceiling(10 * max(width)) / 10, align, s)
  }
  number <- function(whole_part, decimal_part, bold = FALSE) {
    strong <- function(s) ifelse(bold & nzchar(s), paste0("<strong>", s, "</strong>"), s)
    paste0(box(strong(whole_part), whole(whole_part), "right"),
           box(strong(decimal_part), decimals(decimal_part), "left", common = TRUE))
  }
  bold <- nzchar(m[, 1]) & nzchar(m[, 4])
  estimate <- nzchar(m[, 2]) | nzchar(m[, 3])
  x[ok] <- paste0(
    "<span style=\"white-space:nowrap;font-variant-numeric:tabular-nums\">",
    ifelse(estimate, paste0(number(m[, 2], m[, 3], bold), box(m[, 5], raised(m[, 5]), "left"), " "), ""),
    "[", number(m[, 6], m[, 7]), ", ", number(m[, 8], m[, 9]), "]",
    box(m[, 10], raised(m[, 10]), "left"), "</span>"
  )
  x
}

#' An estimate and its standard error in one cell, ".40 (.04)"
#'
#' The standard error takes the leading-zero rule of its estimate: the
#' standard error of a correlation cannot exceed one either.
#'
#' @param est,se Numeric vectors.
#' @param digits,bounded See [rh_fmt()].
#' @return Character vector.
rh_fmt_est_se <- function(est, se, digits = 2, bounded = FALSE) {
  out <- paste0(rh_fmt(est, digits, bounded), " (", rh_fmt(se, digits, bounded), ")")
  out[!is.na(est) & is.na(se)] <- rh_fmt(est, digits, bounded)[!is.na(est) & is.na(se)]
  out[is.na(est)] <- "—"
  out
}

#' A p value as APA prints it: three decimals without the leading zero, "< .001" below
#'
#' @param p Numeric vector of probabilities.
#' @return Character vector.
rh_fmt_p <- function(p) {
  p <- as.numeric(p)
  out <- rh_fmt(p, 3, bounded = TRUE)
  out[!is.na(p) & p < 0.001] <- "< .001"
  out
}

#' The columns of a display table that hold one value in every row
#'
#' A column with the same value in every row leaves
#' the table and is stated once in the prose; the decision is the data's, made
#' at render time, so a report whose values differ keeps the column.
#'
#' A table of one row keeps its columns: one value shows no constant column.
#'
#' @param df Data frame of display values.
#' @param columns Column names to examine.
#' @return Named character vector: column -> its single value (as text).
rh_constant_columns <- function(df, columns = names(df)) {
  columns <- intersect(columns, names(df))
  if (nrow(df) < 2L) return(stats::setNames(character(), character()))
  constant <- vapply(columns, function(nm) {
    values <- as.character(df[[nm]])
    values[is.na(values)] <- "\r"
    length(unique(values)) == 1L
  }, logical(1))
  kept <- columns[constant]
  stats::setNames(vapply(kept, function(nm) as.character(df[[nm]][[1]]), character(1)), kept)
}

#' The report's display rule for Bayes factors
#'
#' One rule for every Bayes factor of the report:
#' two decimals from 0.01 to 9.99, whole numbers from 10 to 100, and beyond
#' that only the power of ten a value exceeds ("> 100", "> 1,000",
#' "> 10,000", "> 100,000"), mirrored below 0.01 ("< 0.01", "< 0.001", ...).
#' The constants are stated here once; [rh_fmt_bf()] applies them and the
#' table notes read them from here.
#'
#' @return List `whole_from` (the value from which whole numbers are shown)
#'   and `rungs` (the powers of ten shown above the whole numbers; their
#'   reciprocals are the rungs below two decimals).
rh_bf_display_rule <- function() {
  list(whole_from = 10, rungs = 10^(2:5))
}

#' The power of ten a Bayes factor is shown at, or `NA` for a value shown as a number
#'
#' A rung is a true statement about the value ("> 1,000" for 1,045) and is
#' never beyond what the estimator can resolve: `upper` is the largest Bayes
#' factor it can distinguish from infinity and `lower` the smallest it can
#' distinguish from zero. An exact Bayes factor has `upper = Inf` and
#' `lower = 0`; `NA` marks an unknown resolution, which leaves an infinite or
#' zero value without a rung.
#'
#' @param bf,upper,lower Numeric scalars.
#' @return Numeric scalar: the rung (positive above, below one for the lower
#'   rungs), `NA` when the value is shown as a number, or `NaN` when no rung
#'   is resolvable.
rh_bf_rung <- function(bf, upper = Inf, lower = 0) {
  rule <- rh_bf_display_rule()
  if (is.na(bf)) return(NA_real_)
  if (bf > rule$rungs[[1]]) {
    limit <- if (is.na(upper)) (if (is.finite(bf)) Inf else -Inf) else upper
    usable <- rule$rungs[rule$rungs < bf & rule$rungs <= limit]
    return(if (length(usable)) max(usable) else NaN)
  }
  if (bf < 1 / rule$rungs[1]) {
    limit <- if (is.na(lower)) (if (bf > 0) 0 else Inf) else lower
    usable <- (1 / rule$rungs)[1 / rule$rungs > bf & 1 / rule$rungs >= limit]
    return(if (length(usable)) min(usable) else NaN)
  }
  NA_real_
}

#' Format Bayes factors by the report's display rule
#'
#' See [rh_bf_display_rule()] for the rule and [rh_bf_rung()] for the rungs
#' beyond 100 and below 0.01. A value whose rung the estimator cannot resolve
#' is printed as an em dash, as is a missing value.
#'
#' @param bf Numeric vector of Bayes factors (`Inf` and `0` allowed).
#' @param upper,lower The resolution of the estimator, recycled along `bf`;
#'   see [rh_bf_rung()].
#' @return Character vector.
rh_fmt_bf <- function(bf, upper = Inf, lower = 0) {
  rule <- rh_bf_display_rule()
  bf <- as.numeric(bf)
  upper <- rep_len(as.numeric(upper), length(bf))
  lower <- rep_len(as.numeric(lower), length(bf))
  vapply(seq_along(bf), function(i) {
    x <- bf[[i]]
    if (is.na(x)) return("—")
    rung <- rh_bf_rung(x, upper[[i]], lower[[i]])
    if (is.nan(rung)) return("—")
    if (!is.na(rung) && rung > 1) return(paste0("> ", rh_fmt_n(rung)))
    if (!is.na(rung)) return(paste0("< ", formatC(rung, format = "f", digits = round(-log10(rung)))))
    if (round(x, 2) >= rule$whole_from) return(rh_fmt(x, 0))
    rh_fmt(x, 2)
  }, character(1))
}

#' Format dates as day, English month name and year
#'
#' Independent of the machine's locale, which `format(x, "%B")` is not.
#'
#' @param x Dates, or strings `as.Date()` reads.
#' @return Character vector such as "19 June 2026".
rh_fmt_date <- function(x) {
  x <- as.Date(x)
  paste(as.integer(format(x, "%d")), month.name[as.integer(format(x, "%m"))], format(x, "%Y"))
}

#' Format counts as integers with a thousands separator
#'
#' @param x Numeric vector of counts.
#' @return Character vector.
rh_fmt_n <- function(x) {
  out <- formatC(round(as.numeric(x)), format = "d", big.mark = ",")
  out <- trimws(out)
  out[is.na(x)] <- "—"
  out
}

#' Format a proportion (0–1) or percentage (0–100) as a percentage string
#'
#' The call site states the unit of `x`; it is never inferred from the values.
#'
#' @param x Numeric vector.
#' @param digits Number of decimals.
#' @param scale Unit of `x`: `"proportion"` (0–1, multiplied by 100) or
#'   `"percent"` (0–100, shown as it is). Has no default and must be given.
#' @return Character vector like "12.3%". APA sets the percent sign closed up;
#'   the DGPs typography would put a space there, so a German
#'   manuscript would differ.
rh_fmt_pct <- function(x, digits = 1, scale) {
  scale <- match.arg(scale, c("proportion", "percent"))
  x <- as.numeric(x)
  if (scale == "proportion") x <- 100 * x
  out <- paste0(trimws(formatC(x, format = "f", digits = digits)), "%")
  out <- sub("^-", "−", out)
  out[!is.finite(x)] <- "—"
  out
}

#' Credible-interval columns of a coefficient-shaped table and their level
#'
#' AP6 names the interval columns by their quantile (`q2.5`, `q97.5` at
#' `analysis_plan$regression$ci_level = 0.95`, `q5`, `q95` at 0.90), so the bounds and
#' the level are read from the names rather than typed into a label.
#'
#' @param df Data frame with two columns named `q<lower>` and `q<upper>`.
#' @return `list(lo, hi, level)`: the two column names (lower first) and the
#'   interval level in percent (e.g. 95).
rh_ci_cols <- function(df) {
  q_cols <- grep("^q[0-9.]+$", names(df), value = TRUE)
  probs <- as.numeric(sub("^q", "", q_cols))
  q_cols <- q_cols[order(probs)]
  list(lo = q_cols[1], hi = q_cols[2], level = diff(sort(probs)))
}

#' Interval label with its level ("95% CrI")
#'
#' @param level Level in percent.
#' @param what Noun of the label (`"CrI"`, `"posterior predictive interval"`).
#' @return Character scalar.
rh_ci_label <- function(level, what = "CrI") {
  paste0(rh_fmt(level, 0), "% ", what)
}

#' Configured interval level in percent
#'
#' @param analysis_plan Configuration from `zm_config()`.
#' @return `100 * analysis_plan$regression$ci_level`.
rh_cfg_ci_level <- function(analysis_plan) {
  100 * as.numeric(analysis_plan[["regression"]][["ci_level"]])
}

#' Display text of the fixed Student-t degrees of freedom and the matched scale prior
#'
#' The robustness refit fixes ν at `analysis_plan$sensitivity$student_t$nu_fixed` (a
#' constant in the Stan program, not a prior) and puts a half-normal with SD
#' `analysis_plan$sensitivity$student_t$sigma_scale_prior_sd` on the scale σ.
#'
#' @param st The `student_t` block of the configuration
#'   (`analysis_plan$sensitivity$student_t`).
#' @param nu_fixed Optional ν as recorded on the refit rows (`nu_fixed`
#'   column of the coefficient table); overrides the configured value.
#' @return Character scalar such as
#'   `"ν fixed at 4 (constant, no tail parameter); scale σ ~ half-normal(0, 0.71)"`.
rh_nu_fixed_text <- function(st, nu_fixed = NULL) {
  nu <- if (!is.null(nu_fixed) && length(nu_fixed) > 0 && !all(is.na(nu_fixed))) {
    unique(stats::na.omit(as.numeric(nu_fixed)))
  } else {
    st[["nu_fixed"]]
  }
  paste0(
    "ν fixed at ", paste(rh_fmt_n(nu), collapse = "/"), " (constant, no tail parameter)",
    "; scale σ ~ half-normal(0, ", rh_fmt(st[["sigma_scale_prior_sd"]]), ")"
  )
}

#' Sweep label of a slope prior SD from the configuration
#'
#' The keys of `analysis_plan$priors$sweep_labels` are the SD formatted without
#' trailing zeros (`"0.1"`, `"0.2"`, `"0.4"`).
#'
#' @param sd Numeric vector of slope prior SDs.
#' @param analysis_plan Configuration from `zm_config()`, or `NULL`.
#' @return Character vector of labels (`"primary"`, ...); `NA` where the
#'   configuration has no label for that SD.
rh_sweep_label <- function(sd, analysis_plan = NULL) {
  sd <- as.numeric(sd)
  labels <- analysis_plan[["priors"]][["sweep_labels"]]
  vapply(sd, function(s) {
    if (is.na(s)) return(NA_character_)
    key <- format(s, trim = TRUE, drop0trailing = TRUE, scientific = FALSE)
    v <- labels[[key]]
    if (is.null(v)) NA_character_ else as.character(v)
  }, character(1))
}

#' Slope prior SD with its sweep label ("0.10 (skeptical)")
#'
#' @param sd Numeric vector of slope prior SDs.
#' @param analysis_plan Configuration from `zm_config()`, or `NULL`.
#' @param digits Decimals of the SD.
#' @return Character vector; the bare formatted SD where no label exists.
rh_sweep_sd_label <- function(sd, analysis_plan = NULL, digits = 2) {
  lab <- rh_sweep_label(sd, analysis_plan)
  base <- rh_fmt(sd, digits)
  ifelse(is.na(lab), base, paste0(base, " (", lab, ")"))
}

#' Capitalise the first letter of a display text
#'
#' The one display rule for a label that starts a table cell, a column head, a
#' figure axis or legend entry, or a sentence: its first letter is a capital.
#' The codebook keeps its own case ("security"); [rh_label_inline()] gives the
#' form inside a sentence.
#'
#' @param x Character vector.
#' @return `x` with the first letter of each element in upper case.
rh_capitalise_first_letter <- function(x) {
  x <- as.character(x)
  ifelse(is.na(x) | !nzchar(x), x, paste0(toupper(substr(x, 1L, 1L)), substring(x, 2L)))
}

#' Human-readable labels for scale keys and model terms
#'
#' The label of each of the nine scales is its `short_label` in
#' `codebook_scales.csv` and comes from nowhere else: without the codebook a
#' scale has no label and is shown by its key. The labels are the display form
#' that starts a cell or a sentence ([rh_capitalise_first_letter()]); inside a
#' sentence a scale is named with [rh_label_inline()]. The scales come first,
#' in the codebook's order. Covariate terms and model constants get fixed
#' English labels.
#'
#' @param codebook Optional list from `zm_codebook()`.
#' @return Named character vector (key -> label).
rh_labels <- function(codebook = NULL) {
  scales <- character()
  if (!is.null(codebook)) {
    cb <- codebook$scales
    scales <- stats::setNames(rh_capitalise_first_letter(cb$short_label), as.character(cb$scale_key))
  }
  # The reported intercept is the expected outcome at the reference gender with
  # every metric predictor at its mean — not the centred intercept the
  # preregistered prior constrains, which is a different parameter. The label
  # says which one the row is.
  intercept_label <- "Intercept (at the reference gender, metric predictors at their means)"
  terms <- c(
    age = "Age band", age_z = "Age band", income = "Income band per household member",
    income_z = "Income band per household member",
    genderfemale = "Gender: female", gendermale = "Gender: male", genderdivers = "Gender: divers",
    Intercept = intercept_label, intercept = intercept_label, sigma = "Residual SD (σ)",
    male = "Gender: male", bayes_R2 = "Bayesian R²", R2 = "Bayesian R²"
  )
  labels <- c(scales, terms)
  # A standardised column carries the label of its key; zm_key_of_z_col() is the
  # codebook lookup, so no caller has to strip a suffix from a column name.
  if (!is.null(codebook)) {
    z_cols <- c(as.character(codebook$scales$z_col), as.character(codebook$covariates$z_col))
    keys <- zm_key_of_z_col(z_cols, codebook)
    known <- keys %in% names(labels)
    labels[z_cols[known]] <- unname(labels[keys[known]])
  }
  labels
}

#' Map keys to labels, keeping unknown keys as they are
#'
#' @param x Character vector of keys (the label table also names the
#'   standardised columns of the codebook).
#' @param labels Named character vector from [rh_labels()].
#' @return Character vector of labels.
rh_label <- function(x, labels = rh_labels()) {
  x <- as.character(x)
  out <- unname(labels[x])
  out[is.na(out)] <- x[is.na(out)]
  out
}

#' Labels for use inside a sentence
#'
#' A label that begins with an ordinary word starts lower case inside a
#' sentence ("conventionalism", "security"); a label that begins with an
#' abbreviation keeps it ("ASC aggression", "SDO-D").
#'
#' @inheritParams rh_label
#' @return Character vector.
rh_label_inline <- function(x, labels = rh_labels()) {
  rh_lowercase_first_letter(rh_label(x, labels))
}

#' A display text as it stands inside a sentence
#'
#' The counterpart of [rh_capitalise_first_letter()]: a text that opens with
#' an ordinary word starts lower case ("all social motive scales"); one that
#' opens with an abbreviation keeps it ("ASC", "SDO-D", "UMS-6 (...)").
#'
#' @param x Character vector.
#' @return Character vector.
rh_lowercase_first_letter <- function(x) {
  out <- as.character(x)
  ordinary <- !is.na(out) & grepl("^[A-Z][a-z]", out)
  out[ordinary] <- paste0(tolower(substr(out[ordinary], 1, 1)), substring(out[ordinary], 2))
  out
}

# ---- tables -----------------------------------------------------------------

#' Decide whether to build a gt table or a kable
#'
#' @param engine `"auto"` (gt for HTML and Word output, kable for the other
#'   output formats), `"gt"` or `"kable"`.
#' @return Logical, `TRUE` for gt.
rh_use_gt <- function(engine = c("auto", "gt", "kable")) {
  engine <- match.arg(engine)
  if (engine == "gt") return(TRUE)
  if (engine == "kable") return(FALSE)
  to <- knitr::pandoc_to()
  if (is.null(to)) return(TRUE)
  to %in% c("html", "html4", "html5", "docx")
}

#' Format the columns of a data frame for display
#'
#' Numeric columns become strings via [rh_fmt()] (integer-valued columns via
#' [rh_fmt_n()]), logicals become "yes"/"no", factors become character, and
#' `NA` becomes "—".
#'
#' @param df Data frame.
#' @param digits Decimals for non-integer numeric columns.
#' @param bounded Character vector of column names formatted without a leading
#'   zero.
#' @return Data frame of character columns.
rh_format_df <- function(df, digits = 2, bounded = NULL) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  for (nm in names(df)) {
    col <- df[[nm]]
    if (is.list(col) && !is.data.frame(col)) {
      col <- vapply(col, function(v) paste(format(v), collapse = "; "), character(1))
    }
    if (is.logical(col)) {
      col <- ifelse(is.na(col), "—", ifelse(col, "yes", "no"))
    } else if (is.numeric(col)) {
      finite <- col[is.finite(col)]
      if (length(finite) > 0 && all(finite == round(finite))) {
        col <- rh_fmt_n(col)
      } else {
        col <- rh_fmt(col, digits = digits, bounded = nm %in% bounded)
      }
    }
    col <- as.character(col)
    col[is.na(col)] <- "—"
    df[[nm]] <- col
  }
  df
}

#' Source note of a display table
#'
#' Every table of the reports names where its numbers come from: the name of
#' the target that holds them, or the analysis plan for a table built from the
#' configuration alone. The sentence is composed here so that no call site has
#' to type it.
#'
#' @param target Character vector of target names; `NA` for a table built from
#'   the configuration alone ("Source: analysis plan."); `NULL` adds no source
#'   sentence.
#' @param source_note Optional further note, appended after the source
#'   sentence.
#' @return Character scalar, or `NULL` when neither part is present.
#' Is this render the published one?
#'
#' `report: published: true` in the analysis plan marks a render meant for
#' publication: the pipeline target names then leave the table notes, which are
#' provenance for the reviewer and noise for the reader. Both
#' report preambles call [rh_set_report_published()] once from `analysis_plan`; the
#' default is the review render, which keeps the names.
#'
#' @return Logical scalar.
rh_report_published <- function() {
  isTRUE(getOption("zm.report_published", FALSE))
}

#' Set the published-render flag from the configuration
#'
#' @param analysis_plan Configuration from `zm_config()` (`analysis_plan$report$published`).
#' @return The flag, invisibly.
rh_set_report_published <- function(analysis_plan) {
  flag <- isTRUE(analysis_plan[["report"]][["published"]])
  options(zm.report_published = flag)
  invisible(flag)
}

#' Italicise the statistical symbols of a label or note
#'
#' APA sets statistical symbols in italics and abbreviations such as CFI, RMSEA
#' or WLSMV in roman. Applied to the column labels and source notes of the gt
#' path of [rh_table()], so that one helper change italicises
#' every table rather than sixty call sites typing markdown.
#'
#' @param x Character vector.
#' @return Character vector with markdown emphasis around the symbols.
rh_md_stats <- function(x) {
  symbols <- c("Mdn", "SD", "df", "N", "M", "b", "r", "p", "k", "t", "z", "F")
  out <- as.character(x)
  for (s in symbols) {
    # a letter inside an HTML tag pair (a superscript note mark such as
    # <sup>b</sup>) is a mark, not a statistic
    out <- gsub(paste0("(?<![\\w*>])", s, "(?![\\w*<])"), paste0("*", s, "*"), out, perl = TRUE)
  }
  out <- gsub("BF_bagged", "BF<sub>bagged</sub>", out, fixed = TRUE)
  out
}

rh_source_note <- function(target = NULL, source_note = NULL) {
  src <- NULL
  if (rh_report_published()) target <- NULL
  if (!is.null(target)) {
    tg <- as.character(target)
    named <- tg[!is.na(tg) & nzchar(tg)]
    src <- if (length(named) == 0) {
      "Source: analysis plan."
    } else {
      paste0(
        "Source: target", if (length(named) > 1) "s" else "", " ",
        paste0("`", named, "`", collapse = ", "), "."
      )
    }
  }
  parts <- c(src, source_note)
  parts <- parts[!is.na(parts) & nzchar(parts)]
  if (length(parts) == 0) return(NULL)
  paste(parts, collapse = " ")
}

#' Show a grouping label only at the beginning of each consecutive block
#' @param x Character vector in display order.
#' @return Character vector with repeated adjacent labels replaced by blanks.
rh_blank_repeated <- function(x) {
  out <- as.character(x)
  if (length(out) < 2L) return(out)
  same <- !is.na(out[-1L]) & !is.na(out[-length(out)]) &
    out[-1L] == out[-length(out)]
  out[c(FALSE, same)] <- ""
  out
}

#' Build a display table (gt, with kable fallback)
#'
#' @param df Data frame (already reduced to the display columns).
#' @param col_labels Named character vector column name -> display label.
#' @param digits Decimals for numeric columns.
#' @param bounded Column names formatted without a leading zero.
#' @param engine See [rh_use_gt()].
#' @param source_note Optional note under the table.
#' @param caption Optional caption (used only when the chunk sets none).
#' @param target Name(s) of the target the numbers come from, or `NA` for a
#'   table built from the configuration alone; see [rh_source_note()]. The
#'   sentence is prepended to `source_note`.
#' @param markdown Column names whose cells are Markdown (e.g. bold).
#' @return A `gt_tbl` or a `knitr_kable`.
rh_table <- function(df, col_labels = NULL, digits = 2, bounded = NULL,
                     engine = "auto", source_note = NULL, caption = NULL,
                     target = NULL, markdown = NULL) {
  source_note <- rh_source_note(target, source_note)
  out <- rh_format_df(df, digits = digits, bounded = bounded)
  labels <- names(out)
  if (!is.null(col_labels)) {
    hit <- names(col_labels)[names(col_labels) %in% labels]
    labels[match(hit, names(out))] <- unname(col_labels[hit])
  }
  if (rh_use_gt(engine)) {
    # Estimate-and-interval columns: Markdown for a bold estimate and, in
    # HTML, the decimal points lined up
    aligned <- names(out)[vapply(out, function(x) any(grepl(rh_interval_cell_pattern(), x, perl = TRUE)), logical(1))]
    markdown <- union(markdown, aligned)
    tab <- gt::gt(out)
    # Statistical symbols are italic in APA; gt::md() renders the markdown that
    # rh_md_stats() puts around them.
    md_labels <- lapply(rh_md_stats(labels), gt::md)
    tab <- gt::cols_label(tab, .list = stats::setNames(md_labels, names(out)))
    if (length(markdown)) tab <- gt::fmt_markdown(tab, columns = dplyr::all_of(markdown))
    to <- knitr::pandoc_to()
    if (!(is.null(to) || to %in% c("html", "html4", "html5"))) aligned <- character(0)
    if (length(aligned)) {
      tab <- gt::fmt(tab, columns = dplyr::all_of(aligned), fns = list(html = rh_align_interval_cells))
    }
    tab <- gt::tab_options(
      tab,
      table.font.names = c("Times New Roman", "Times", "serif"),
      table.font.size = gt::px(13),
      data_row.padding = gt::px(3),
      column_labels.font.weight = "normal",
      table.border.top.style = "none",
      table.border.bottom.style = "none",
      column_labels.border.top.style = "solid",
      column_labels.border.top.width = gt::px(1),
      column_labels.border.top.color = "black",
      column_labels.border.bottom.style = "solid",
      column_labels.border.bottom.width = gt::px(1),
      column_labels.border.bottom.color = "black",
      table_body.border.top.style = "none",
      table_body.border.bottom.style = "solid",
      table_body.border.bottom.width = gt::px(1),
      table_body.border.bottom.color = "black",
      table_body.hlines.style = "none",
      table_body.vlines.style = "none",
      column_labels.vlines.style = "none",
      row.striping.include_table_body = FALSE,
      source_notes.border.bottom.style = "none"
    )
    tab <- gt::cols_align(tab, align = "left", columns = dplyr::everything())
    numeric_columns <- names(out)[vapply(out, function(x) {
      values <- trimws(as.character(x))
      values <- values[!is.na(values) & !values %in% c("", "—", "n/a")]
      length(values) > 0L && all(grepl("^[−+<>≤≥0-9.,% /()\\[\\]*-]+$", values))
    }, logical(1))]
    # aligned columns stay flush left: a number with more decimals then
    # lengthens its interval, not shifts its estimate
    numeric_columns <- setdiff(numeric_columns, aligned)
    if (length(numeric_columns)) {
      tab <- gt::cols_align(tab, align = "right", columns = dplyr::all_of(numeric_columns))
    }
    # Every table note opens with "Note." as APA requires; the kable path below
    # does the same.
    if (!is.null(source_note)) {
      tab <- gt::tab_source_note(tab, gt::md(paste0("*Note.* ", rh_md_stats(source_note))))
    }
    if (!is.null(caption)) tab <- gt::tab_caption(tab, caption)
    return(tab)
  }
  tab <- knitr::kable(out, col.names = labels, caption = caption, align = "l")
  if (!is.null(source_note)) tab <- c(tab, "", paste0("*Note.* ", source_note))
  tab
}

#' Plot one numeric response distribution for a table histogram
#'
#' Continuous values retain twenty equal-width bins. A supplied category interval
#' uses exact response counts with narrow bars and regular whitespace.
#' @param scores Numeric responses; missing responses are omitted.
#' @param response_range Lower and upper response values.
#' @param bin_width Category interval, or NA for continuous histogram bins.
#' @return A ggplot using the same styling as the descriptive scale histograms.
rh_plot_histogram <- function(scores, response_range, bin_width = NA_real_) {
  if (!is.numeric(scores) || any(is.infinite(scores))) stop("Histogram scores must be finite numeric responses or missing.")
  observed <- scores[!is.na(scores)]
  if (!length(observed)) {
    return(ggplot2::ggplot() + ggplot2::annotate("text", x = 0, y = 0,
      label = "No observed responses", size = 2.5) + ggplot2::theme_void())
  }
  if (length(response_range) != 2L || any(!is.finite(response_range)) ||
      response_range[1] >= response_range[2]) {
    stop("Histogram response ranges require two finite, increasing limits.")
  }
  if (any(observed < response_range[1] | observed > response_range[2])) {
    stop("Histogram responses fall outside the supplied response range.")
  }
  if (length(bin_width) != 1L || (!is.na(bin_width) && (!is.finite(bin_width) || bin_width <= 0))) {
    stop("Histogram bin widths must be positive and finite, or missing for continuous bins.")
  }
  discrete <- !is.na(bin_width)
  if (discrete) {
    positions <- (observed - response_range[1]) / bin_width
    intervals <- diff(response_range) / bin_width
    if (any(abs(positions - round(positions)) > 1e-8) || abs(intervals - round(intervals)) > 1e-8) {
      stop("Discrete histogram responses and limits must lie on their declared category intervals.")
    }
    centres <- seq(response_range[1], response_range[2], by = bin_width)
    distribution <- data.frame(score = centres,
      count = tabulate(as.integer(round(positions)) + 1L, nbins = length(centres)))
    plot <- ggplot2::ggplot(distribution, ggplot2::aes(x = score, y = count)) +
      ggplot2::geom_col(width = .4 * bin_width, fill = "grey35", colour = NA)
    limits <- response_range + c(-1, 1) * bin_width / 2
  } else {
    breaks <- seq(response_range[1], response_range[2], length.out = 21L)
    plot <- ggplot2::ggplot(data.frame(score = scores), ggplot2::aes(x = score)) +
      ggplot2::geom_histogram(breaks = breaks, closed = "left", fill = "grey35",
                            colour = "white", linewidth = 0.1, na.rm = TRUE)
    limits <- range(breaks)
  }
  plot +
    ggplot2::scale_x_continuous(limits = limits, expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = ggplot2::margin(1, 1, 1, 1))
}

#' Save bin boundaries and counts using the report's existing histogram settings
calculate_histogram_data <- function(scores, response_range, bin_width = NA_real_) {
  plot <- rh_plot_histogram(scores, response_range, bin_width)
  if (!any(!is.na(scores))) return(NULL)
  rows <- ggplot2::ggplot_build(plot)$data[[1L]]
  if (!"count" %in% names(rows)) rows$count <- rows$y
  out <- rows[c("xmin", "xmax", "count")]
  attr(out, "response_range") <- response_range
  attr(out, "bin_width") <- bin_width
  out
}

#' Save the bin counts and boundaries of the existing continuous scale histograms
add_scale_histogram_data <- function(summary, scores, response_range = c(1, 6)) {
  attr(summary, "histograms") <- stats::setNames(lapply(summary$scale_key, function(key) {
    calculate_histogram_data(scores[[key]], response_range)
  }), summary$scale_key)
  summary
}

#' Draw saved histogram bins with the report's unchanged styling
plot_saved_histogram_data <- function(rows) {
  if (is.null(rows)) return(ggplot2::ggplot() + ggplot2::annotate("text", x = 0, y = 0,
    label = "No observed responses", size = 2.5) + ggplot2::theme_void())
  limits <- attr(rows, "response_range", exact = TRUE)
  bin_width <- attr(rows, "bin_width", exact = TRUE)
  discrete <- !is.na(bin_width)
  if (discrete) limits <- limits + c(-1, 1) * bin_width / 2
  ggplot2::ggplot(rows) +
    ggplot2::geom_rect(ggplot2::aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = count),
      fill = "grey35", colour = if (discrete) NA else "white",
      linewidth = if (discrete) 0.5 else 0.1) +
    ggplot2::scale_x_continuous(limits = limits, expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = ggplot2::margin(1, 1, 1, 1))
}

#' Embed small dark-grey histograms in selected rows of an HTML gt table
#'
#' The ordered keys correspond to the selected table rows. Other rows retain
#' their original cells. Existing scale tables keep their common response
#' range, continuous bins and image dimensions.
#' @param tab A table from [rh_table()], with an empty histogram column.
#' @param scores Respondent-level numeric values.
#' @param scale_keys Ordered score column names, one for each selected row.
#' @param column Name of the placeholder column.
#' @param response_range Lower and upper limits, or a list of ranges by key.
#' @param rows Selected row indices; defaults to all table rows.
#' @param bin_width One bin width, or widths by key; NA uses continuous bins.
#' @return The table with embedded PNGs when it is an HTML gt table.
rh_add_histograms <- function(tab, scores, scale_keys, column = "histogram",
                              response_range = c(1, 6), rows = seq_len(nrow(tab[["_data"]])),
                              bin_width = NA_real_, histogram_data = NULL) {
  format <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  html <- is.null(format) || format %in% c("html", "html4", "html5")
  if (!inherits(tab, "gt_tbl") || !html) return(tab)
  if (length(scale_keys) != length(rows) || anyNA(rows) || anyDuplicated(rows) ||
      any(rows != as.integer(rows) | rows < 1L | rows > nrow(tab[["_data"]])) ||
      !all(scale_keys %in% if (is.null(histogram_data)) names(scores) else names(histogram_data))) {
    stop("Histogram keys must match table rows and available score columns.")
  }
  if (!column %in% names(tab[["_data"]])) stop("Histogram column is absent from the table.")
  if (!length(scale_keys)) return(tab)
  ranges <- if (is.list(response_range)) response_range else rep(list(response_range), length(scale_keys))
  if (length(ranges) != length(scale_keys)) stop("Histogram response ranges must match selected keys.")
  if (length(bin_width) == 1L) bin_width <- rep(bin_width, length(scale_keys))
  if (length(bin_width) != length(scale_keys)) stop("Histogram bin widths must match selected keys.")
  plots <- lapply(seq_along(scale_keys), function(i) {
    if (is.null(histogram_data)) rh_plot_histogram(scores[[scale_keys[i]]], ranges[[i]], bin_width[[i]]) else
      plot_saved_histogram_data(histogram_data[[scale_keys[i]]])
  })
  # Both sample and scale tables use the identical gt image construction:
  # 30px high, 3:1 aspect ratio, with no additional width or fitting styles.
  images <- vapply(plots, function(p) as.character(gt::ggplot_image(p, height = 30, aspect_ratio = 3)),
                    character(1))
  # gt transforms selected cells in table order, even when keys arrive otherwise.
  images <- images[order(rows)]
  gt::text_transform(tab, locations = gt::cells_body(columns = dplyr::all_of(column), rows = sort(rows)),
                      fn = function(x) images)
}

#' Item response distributions of one group of scales
#'
#' The response counts of the items of one group (the motive items, or the
#' items of authoritarian orientation), grouped by scale as the report names
#' it. The number of responses and of missing answers stand in the note: an
#' item with missing answers carries a superscript and is named there with its
#' count.
#'
#' @param item_distributions Tibble from [rh_item_distribution_table()]
#'   (`Scale`, `Item`, `N`, `Missing`, one column per response category).
#' @param codebook Codebook from [zm_codebook()] (the items of each scale key).
#' @param scales Scale keys of the group, in display order.
#' @param labels Named labels from [rh_labels()].
#' @inheritParams rh_table
#' @return Display table.
rh_item_distribution_display <- function(item_distributions, codebook, scales, labels = rh_labels(),
                                         engine = "auto", target = NULL) {
  d <- tibble::as_tibble(item_distributions)
  item_scale <- unlist(lapply(seq_len(nrow(codebook$scales)), function(i) {
    stats::setNames(rep(codebook$scales$scale_key[[i]], length(codebook$scales$item_codes[[i]])),
                    codebook$scales$item_codes[[i]])
  }))
  d$scale_key <- unname(item_scale[d$Item])
  d <- d[d$scale_key %in% scales, , drop = FALSE]
  d <- d[order(match(d$scale_key, scales)), , drop = FALSE]
  missing <- as.numeric(d$Missing)
  responses <- grep("^Response ", names(d), value = TRUE)
  out <- tibble::tibble(
    scale = rh_blank_repeated(rh_label(d$scale_key, labels)),
    item = paste0(d$Item, ifelse(missing > 0, "<sup>a</sup>", ""))
  )
  for (r in responses) out[[r]] <- d[[r]]
  total <- as.numeric(d$N) + missing
  note <- paste0(
    "Count (percentage) of each response option, in the original coding of the item, after the exclusions. ",
    "Percentages are among the observed answers to the item; options nobody chose are kept with a count of zero.",
    if (length(unique(total)) == 1L) paste0(" Each item was presented to ", rh_fmt_n(total[[1]]), " respondents",
                                            if (any(missing > 0)) "." else ", all of whom answered it.") else "",
    if (any(missing > 0)) paste0(
      " <sup>a</sup> Item with missing answers, filled before scoring: ",
      paste0(d$Item[missing > 0], " (", rh_fmt_n(missing[missing > 0]), " missing)", collapse = ", "), "."
    ) else ""
  )
  rh_table(out, col_labels = c(scale = "Scale", item = "Item"), engine = engine, target = target,
           markdown = "item", source_note = note)
}

#' Plain-language labels of the AP1 criteria and the AP3 drop
#'
#' The wording is the preregistration's (AP1, AP3). With the configuration,
#' the age bounds and the minimum number of gender-diverse responses are
#' named; without it, the labels name the rules without their numbers.
#'
#' @param x Character vector of criterion codes.
#' @param analysis_plan Configuration from `zm_config()`, or `NULL`.
#' @return Character vector.
rh_criterion_label <- function(x, analysis_plan = NULL) {
  min_age <- analysis_plan[["exclusions"]][["min_age"]]
  max_age <- analysis_plan[["exclusions"]][["max_age"]]
  divers_min <- analysis_plan[["exclusions"]][["gender_divers_min_n"]]
  map <- c(
    not_consenting = "No consent", incomplete = "Incomplete response",
    age_outside_range = if (is.null(min_age) || is.null(max_age)) {
      "Age below the minimum or above the maximum age"
    } else {
      paste0("Age below ", rh_fmt_n(min_age), " or above ", rh_fmt_n(max_age))
    },
    attention_1_wrong = "Attention check 1 failed",
    attention_2_wrong = "Attention check 2 failed",
    gender_divers = if (is.null(divers_min)) {
      "Gender-diverse responses below the minimum number"
    } else {
      paste0("Gender-diverse responses below the minimum of ", rh_fmt_n(divers_min))
    },
    dropped_unfillable_gaps = "More than two items missing within any scale, or at least two of age, household size and income band"
  )
  unname(map[as.character(x)])
}

#' AP4 reliability table (omega with alpha, bootstrap success, notes)
#'
#' Shows the separate stored `n_boot_omega` and `n_boot_alpha` counts against
#' the configured number, the reported method — alpha rows are labelled as the
#' fallback — and the stored `note` (e.g. why the omega model did not fit).
#'
#' @param rel Tibble from `ap4_reliability()`.
#' @param labels Named labels from [rh_labels()].
#' @param engine See [rh_use_gt()].
#' @param target Name(s) of the target the numbers come from; see [rh_source_note()].
#' @param analysis_plan Configuration (`analysis_plan$reliability$bootstrap_n`, the configured
#'   number of resamples under the run profile).
#' @return Display table.
rh_reliability_table <- function(rel, labels = rh_labels(), engine = "auto", analysis_plan, target = NULL) {
  # Every coefficient and both intervals stand in the reliability table of the
  # Results; this table lists only the scales whose reliability needed the
  # fallback or lost bootstrap resamples, which the prose otherwise states in
  # one sentence (a table only where values differ).
  rel <- rel[rh_reliability_detail_rows(rel, analysis_plan), , drop = FALSE]
  if (nrow(rel) == 0L) return(NULL)
  method <- as.character(rel$method)
  method_label <- dplyr::case_when(
    method == "omega" ~ "ω total",
    method == "alpha" ~ "α (ω model did not fit)",
    TRUE ~ method
  )
  n_planned <- analysis_plan$reliability$bootstrap_n
  bootstrap_count <- function(column) {
    paste(rh_fmt_n(rel[[column]]), "of", rh_fmt_n(n_planned))
  }
  # A note concerns single scales, so it is a superscript letter on the scale
  # and a sentence of the table note; identical notes share their letter.
  note <- as.character(rel$note)
  note <- ifelse(is.na(note), "", trimws(note))
  distinct <- unique(note[nzchar(note)])
  mark <- ifelse(nzchar(note), letters[match(note, distinct)], "")
  source <- as.character(rel$interval_source)
  source_label <- dplyr::case_when(
    source == "omega_bootstrap" ~ "bootstrap of ω",
    source == "alpha_fallback" & method == "alpha" ~ "α interval (ω model did not fit)",
    source == "unavailable_omega_bootstrap" ~ "ω interval unavailable",
    source == "unavailable_alpha_fallback_failed" ~ "unavailable",
    TRUE ~ source
  )
  min_success <- analysis_plan[["reliability"]][["omega_interval_min_success"]]
  omega_count <- ifelse(method == "alpha", "not attempted", bootstrap_count("n_boot_omega"))
  df <- tibble::tibble(
    scale = paste0(rh_label(rel$scale_key, labels), ifelse(nzchar(mark), paste0("<sup>", mark, "</sup>"), "")),
    omega = rh_fmt_est_ci(rel$omega_t, rel$omega_lo, rel$omega_hi, 2, bounded = TRUE),
    alpha = rh_fmt_est_ci(rel$alpha, rel$alpha_lo, rel$alpha_hi, 2, bounded = TRUE),
    method = method_label,
    interval_source = source_label,
    n_boot_omega = omega_count,
    n_boot_alpha = bootstrap_count("n_boot_alpha")
  )
  specific <- if (length(distinct)) paste0(
    " ", paste0("<sup>", letters[seq_along(distinct)], "</sup> ", sub("([^.])$", "\\1.", distinct), collapse = " ")
  ) else ""
  rh_table(
    df,
    col_labels = c(scale = "Scale", omega = "ω [95% CI]", alpha = "α [95% CI]",
                   method = "Reported coefficient", interval_source = "Interval",
                   n_boot_omega = "Successful ω resamples", n_boot_alpha = "Successful α resamples"),
    engine = engine, target = target, markdown = "scale",
    source_note = paste0(
      "ω: McDonald's omega total; α: Cronbach's alpha. CI: 95% percentile bootstrap confidence interval. ",
      "The interval of ω is its percentile bootstrap interval when at least ",
      rh_fmt_pct(min_success, 0, scale = "proportion"), " of its resamples succeed; otherwise ω is shown without an interval. ",
      "α is shown with its own bootstrap interval.",
      specific
    )
  )
}

#' The scales whose reliability needed the fallback or lost resamples
#'
#' @param rel,analysis_plan See [rh_reliability_table()].
#' @return Logical vector, one element per scale.
rh_reliability_detail_rows <- function(rel, analysis_plan) {
  n_planned <- as.numeric(analysis_plan$reliability$bootstrap_n)
  !(as.character(rel$method) %in% "omega" & as.character(rel$interval_source) %in% "omega_bootstrap" &
      as.numeric(rel$n_boot_omega) %in% n_planned & as.numeric(rel$n_boot_alpha) %in% n_planned &
      (is.na(rel$note) | !nzchar(as.character(rel$note))))
}

#' The reliability detail in one sentence, for the scales without a fallback
#'
#' @param rel,analysis_plan See [rh_reliability_table()].
#' @param labels Named labels from [rh_labels()].
#' @param table_ref Reference to the table of the other scales.
#' @return One or two sentences.
rh_reliability_detail_text <- function(rel, analysis_plan, labels = rh_labels(), table_ref = "") {
  detail <- rh_reliability_detail_rows(rel, analysis_plan)
  n_planned <- analysis_plan$reliability$bootstrap_n
  standard <- paste0(
    "ω total is reported with the percentile bootstrap interval of ω, and all ", rh_fmt_n(n_planned),
    " bootstrap resamples of ω and of α succeeded"
  )
  if (!any(detail)) {
    every <- if (nrow(rel) == 1L) "For the one scale" else paste0("For all ", rh_fmt_count(nrow(rel)), " scales")
    return(paste0(every, ", ", standard, "."))
  }
  paste0(
    if (any(!detail)) paste0(
      "For ", rh_fmt_count(sum(!detail)), " of the ", rh_fmt_count(nrow(rel)), " scales, ", standard, ". "
    ) else "",
    table_ref, " lists ",
    if (sum(detail) == 1L) "the scale" else paste("the", rh_fmt_count(sum(detail)), "scales"),
    " whose reliability needed the fallback or lost resamples: ",
    paste(rh_label_inline(rel$scale_key[detail], labels), collapse = ", "), "."
  )
}

#' The fit-index version sentence carried by a CFA table
#'
#' The version of every printed index is a property of the numbers, not of the
#' display, so it travels in the `index_version` column that [ap4_cfa()] and
#' [ap4_cfa_sets()] write and is printed here rather than restated. A table
#' without the column says so instead of having a
#' version asserted on its behalf.
#'
#' @param cfa A CFA table from `ap4_cfa()` or `ap4_cfa_sets()`.
#' @return One sentence naming the version of the fit indices.
rh_cfa_index_version_note <- function(cfa) {
  v <- unique(as.character(cfa[["index_version"]]))
  v <- v[!is.na(v) & nzchar(v)]
  if (length(v) == 0) {
    return("Fit-index version: not recorded with these numbers.")
  }
  paste0("Fit-index version: ", paste(v, collapse = "; "), ".")
}

#' The estimation messages of the confirmatory models, one row per model
#'
#' Every estimation warning is kept in the technical supplement. The fit
#' table marks a model with a message by a superscript and glosses it in plain
#' words; the supplement reproduces the message itself from here.
#'
#' @param cfa_scales,cfa_sets Tables from [tabulate_cfa_scale_models()] and
#'   [tabulate_cfa_set_models()].
#' @param labels Named labels from [rh_labels()].
#' @return Tibble `model` (key), `label` (display name), `message` (whitespace
#'   collapsed), `kind` (`"vcov"` for lavaan's near-singular covariance matrix of
#'   the estimates, else `"other"`); zero rows when no model has a message.
rh_cfa_estimation_messages <- function(cfa_scales, cfa_sets, labels = rh_labels()) {
  one <- function(cfa, label) {
    if (is.null(cfa) || nrow(cfa) == 0L) return(NULL)
    messages <- vapply(seq_len(nrow(cfa)), function(i) {
      parts <- c(as.character(cfa$error)[i], as.character(cfa$note)[i])
      parts <- unique(trimws(gsub("\\s+", " ", parts[!is.na(parts) & nzchar(parts)])))
      paste(parts, collapse = " ")
    }, character(1))
    tibble::tibble(model = as.character(cfa$model), label = label, message = messages)
  }
  out <- dplyr::bind_rows(
    one(cfa_scales, paste0(rh_label(cfa_scales$model, labels), " (one factor)")),
    one(cfa_sets, as.character(cfa_sets$label))
  )
  out <- out[nzchar(out$message), , drop = FALSE]
  out$kind <- ifelse(grepl("lav_model_vcov|not appear to be positive definite", out$message), "vcov", "other")
  out
}

#' The fit of every confirmatory model, in one table
#'
#' Three blocks: the one-factor model of each scale, the subscale structures
#' proposed in the development papers, and the models of the item sets that
#' combine instruments (reported under SRQ1). A column with the same value in
#' every row (sample size, convergence, admissibility) leaves the table for
#' the sentence of [rh_cfa_fit_constant_text()]; a model with an estimation
#' message carries a superscript letter and a plain-language note, and the
#' message itself is in the supplement ([rh_cfa_estimation_messages()]).
#'
#' @param cfa_scales Table from [tabulate_cfa_scale_models()].
#' @param structure_models,combined_models Rows of [tabulate_cfa_set_models()]:
#'   the subscale structures and the combined item sets.
#' @param labels Named labels from [rh_labels()].
#' @param messages_link Reference to the section that reproduces the messages.
#' @inheritParams rh_table
#' @return Display table.
rh_cfa_fit_table <- function(cfa_scales, structure_models, combined_models, labels = rh_labels(),
                             messages_link = "the supplement", engine = "auto", target = NULL) {
  block_of <- function(models, block, name) {
    out <- tibble::as_tibble(models[setdiff(names(models), c("reporting", "loadings"))])
    out$block <- rep(block, nrow(out))
    out$name <- name
    out
  }
  rows <- dplyr::bind_rows(
    block_of(cfa_scales, "One factor per scale", rh_label(cfa_scales$model, labels)),
    block_of(structure_models, "Subscale structure of the development papers", as.character(structure_models$label)),
    block_of(combined_models, "Item sets combining instruments (SRQ1)", as.character(combined_models$label))
  )
  messages <- rh_cfa_estimation_messages(cfa_scales, dplyr::bind_rows(structure_models, combined_models), labels)
  kinds <- unique(messages$kind)
  letters_of <- stats::setNames(letters[seq_along(kinds)], kinds)
  mark <- unname(letters_of[messages$kind[match(rows$model, messages$model)]])
  mark[is.na(mark)] <- ""
  ok <- rows$converged %in% TRUE & rows$admissible %in% TRUE
  df <- tibble::tibble(
    block = rh_blank_repeated(rows$block),
    model = paste0(rows$name, ifelse(nzchar(mark), paste0("<sup>", mark, "</sup>"), "")),
    n_factors = rh_fmt_n(rows$n_factors),
    n_items = rh_fmt_n(rows$n_items),
    n = rh_fmt_n(rows$n),
    chisq = rh_fmt(rows$chisq, 2),
    df = rh_fmt_n(rows$df),
    p = rh_fmt_p(rows$pvalue),
    cfi = rh_fmt(rows$cfi, 3, bounded = TRUE),
    # TLI can exceed one, so it keeps its leading zero (APA).
    tli = rh_fmt(rows$tli, 3),
    rmsea = rh_fmt_est_ci(rows$rmsea, rows$rmsea_lo, rows$rmsea_hi, 3, bounded = TRUE),
    srmr = rh_fmt(rows$srmr, 3, bounded = TRUE),
    converged = ifelse(rows$converged %in% TRUE, "yes", "no"),
    admissible = ifelse(rows$admissible %in% TRUE, "yes", "no")
  )
  constant <- rh_constant_columns(df, c("n", "converged", "admissible"))
  df <- df[setdiff(names(df), names(constant))]
  gloss <- c(
    vcov = paste0(
      "the estimation software found the covariance matrix of the parameter estimates nearly singular, that is, ",
      "some estimates were almost perfectly dependent on each other in this sample, so their standard errors may ",
      "be unreliable; it suggests checking whether the model is identified"
    ),
    other = "the estimation software issued a message"
  )
  specific <- vapply(kinds, function(kind) {
    flagged <- rows$model %in% messages$model[messages$kind == kind]
    text <- gloss[[kind]]
    paste0(
      "<sup>", letters_of[[kind]], "</sup> ", toupper(substr(text, 1, 1)), substring(text, 2),
      ". ", if (all(ok[flagged])) {
        if (sum(flagged) == 1L) "The model converged to an admissible solution. " else
          "The models converged to admissible solutions. "
      } else "",
      "The message is reproduced in ", messages_link, "."
    )
  }, character(1))
  rh_table(
    df,
    col_labels = c(block = "Model type", model = "Model", n_factors = "Factors", n_items = "Items", n = "n",
                   chisq = "χ²", df = "df", p = "p", cfi = "CFI", tli = "TLI",
                   rmsea = "RMSEA [90% confidence interval]", srmr = "SRMR",
                   converged = "Converged", admissible = "Admissible")[names(df)],
    engine = engine, target = target, markdown = "model",
    source_note = paste(c(
      paste(
        "CFI: comparative fit index; TLI: Tucker–Lewis index; RMSEA: root mean square error of approximation;",
        "SRMR: standardised root mean square residual.",
        rh_cfa_index_version_note(rows)
      ),
      unname(specific)
    ), collapse = " ")
  )
}

#' The columns of the confirmatory fit table that hold one value, as a sentence
#'
#' @param cfa_scales,structure_models,combined_models See [rh_cfa_fit_table()].
#' @return One sentence, or `""` when no such column exists.
rh_cfa_fit_constant_text <- function(cfa_scales, structure_models, combined_models) {
  rows <- dplyr::bind_rows(
    tibble::as_tibble(cfa_scales[c("model", "n", "converged", "admissible")]),
    tibble::as_tibble(structure_models[c("model", "n", "converged", "admissible")]),
    tibble::as_tibble(combined_models[c("model", "n", "converged", "admissible")])
  )
  k <- nrow(rows)
  same_n <- k > 0L && all(is.finite(rows$n)) && length(unique(rows$n)) == 1L
  all_ok <- all(rows$converged %in% TRUE) && all(rows$admissible %in% TRUE)
  fitted <- if (same_n) paste0(" were fitted to the answers of ", rh_fmt_n(rows$n[[1]]), " respondents") else ""
  if (all_ok && same_n) {
    paste0("All ", rh_fmt_count(k), " models", fitted, " and converged to admissible solutions.")
  } else if (all_ok) {
    paste0("All ", rh_fmt_count(k), " models converged to admissible solutions.")
  } else if (same_n) {
    paste0("All ", rh_fmt_count(k), " models", fitted, ".")
  } else ""
}

# ---- AP4/AP9 factor-analytic sets -------------------------------------------

#' Display label of a factor-count index
#'
#' Keys are the index names the factor-number results carry — the `index` of
#' every entry of `analysis_plan$factor_analysis$factor_number_criteria`, as
#' [name_efa_indices()] projects them. The labels are that plan
#' section's own `label` values, kept here because presentation receives no
#' configuration; test-report.R asserts the two agree criterion for criterion.
#' Every emitted criterion needs a label here — an unlabelled one renders as an
#' empty column header or an em-dash row.
#'
#' @param x Character vector of index keys.
#' @return Character vector of display labels ("n/a" for `NA`).
rh_factor_index_label <- function(x) {
  map <- c(
    parallel_analysis = "Parallel analysis (principal components)",
    common_factor_parallel = "Parallel analysis (common factors)",
    comparison_data = "Comparison data",
    map = "Velicer's minimum average partial",
    kaiser = "Kaiser criterion",
    empirical_kaiser = "Empirical Kaiser criterion",
    optimal_coordinates = "Optimal coordinates",
    acceleration_factor = "Acceleration factor",
    vss = "Very simple structure (complexity one)"
  )
  x <- as.character(x)
  out <- unname(map[x])
  out[is.na(x)] <- "n/a"
  out
}

#' Print a display table or figure inside a cross-reference div of any kind
#'
#' For a document that carries two independently numbered float series. The results draft numbers its
#' manuscript floats `tbl-`/`fig-` and its supplement floats with the custom
#' cross-reference kinds declared in its front matter (`suppltbl-`,
#' `supplfig-`), so a supplement table printed from a loop needs the same div
#' wrapper with a different prefix.
#'
#' @param x A `gt_tbl`, a `knitr_kable`, or `NULL` (printed as nothing).
#' @param id Cross-reference id without the prefix, or `NULL` for an
#'   unnumbered float.
#' @param caption Caption of the float; required for numbering.
#' @param prefix Cross-reference kind, without the hyphen: `"tbl"`, `"fig"`,
#'   or a custom kind such as `"suppltbl"`.
#' @return `NULL`, invisibly (called for the side effect).
rh_float_show <- function(x, id = NULL, caption = NULL, prefix = "tbl") {
  if (is.null(x)) return(invisible(NULL))
  out <- tryCatch(knitr::knit_print(x), error = function(e) NULL)
  if (is.null(out)) out <- as.character(x)
  body <- paste(as.character(out), collapse = "\n")
  numbered <- !is.null(id) && !is.null(caption) && nzchar(id) && nzchar(caption)
  if (numbered) {
    cat("\n\n::: {#", prefix, "-", id, "}\n", body, "\n\n", caption, "\n:::\n\n", sep = "")
  } else {
    cat("\n\n", body, "\n\n", sep = "")
  }
  invisible(NULL)
}

#' The label of one exploratory solution in words
#'
#' "3 factors (one per scale)" for the expected
#' solution, "2 factors (suggested by 1 of 9 criteria)" for a solution the
#' factor-number criteria suggested.
#'
#' @param factors Number of factors.
#' @param expected Logical: the expected solution.
#' @param n_suggesting,n_criteria Criteria suggesting this number, and all criteria.
#' @return Character vector.
rh_efa_solution_label <- function(factors, expected, n_suggesting = NA_integer_, n_criteria = NA_integer_) {
  count <- paste(rh_fmt_n(factors), ifelse(factors == 1, "factor", "factors"))
  ifelse(expected %in% TRUE, paste0(count, " (one per scale)"),
         ifelse(is.na(n_suggesting) | is.na(n_criteria), paste0(count, " (suggested by the criteria)"),
                paste0(count, " (suggested by ", rh_fmt_n(n_suggesting), " of ", rh_fmt_n(n_criteria), " criteria)")))
}

#' How many factor-number criteria suggested a solution's number of factors
#'
#' @param criteria Tibble from [tabulate_factor_number_criteria()].
#' @param keys,factors Set keys and numbers of factors, recycled together.
#' @return List `n` (integer vector) and `of` (the number of criteria).
rh_efa_criteria_counts <- function(criteria, keys, factors) {
  fixed <- c("key", "label", "n_items", "expected", "note")
  indices <- setdiff(names(criteria), fixed)
  n <- vapply(seq_along(keys), function(i) {
    row <- criteria[criteria$key %in% keys[[i]], , drop = FALSE]
    if (nrow(row) != 1L) return(NA_integer_)
    as.integer(sum(unlist(row[1, indices]) %in% factors[[i]]))
  }, integer(1))
  list(n = n, of = length(indices))
}

#' The scales that share a factor in one solution, in words
#'
#' A factor carries the scales most of whose items it holds
#' ([tabulate_efa_correspondence()]); a factor carrying several scales is
#' named by them.
#'
#' @param membership One membership table of [tabulate_efa_correspondence()].
#' @param labels Named labels from [rh_labels()].
#' @return Character scalar such as "ASC submission and conventionalism", "" when none.
rh_efa_shared_factors <- function(membership, labels = rh_labels()) {
  if (is.null(membership) || nrow(membership) == 0L || !"assigned_scales" %in% names(membership)) return("")
  shared <- unique(membership$assigned_scales[!is.na(membership$assigned_scales) & grepl(";", membership$assigned_scales)])
  if (length(shared) == 0L) return("")
  groups <- rh_efa_factor_scales_label(shared, rep(NA_character_, length(shared)), labels, inline = TRUE)
  # a table cell opens with a capital letter
  groups <- paste0(toupper(substr(groups, 1, 1)), substring(groups, 2))
  paste(groups, collapse = "; ")
}

#' The solutions whose item grouping differs from the intended scales
#'
#' A solution corresponds perfectly when its adjusted Rand index,
#' expected-pair retention and empirical-pair purity are all one; those rows
#' become the sentence of [rh_efa_correspondence_text()] (rows at the
#' reference value become a sentence).
#'
#' @param overview The `overview` of [tabulate_efa_correspondence()].
#' @return Logical vector, `TRUE` for a solution that does not correspond perfectly.
rh_efa_correspondence_differs <- function(overview) {
  measures <- c("adjusted_rand", "expected_pair_retention", "empirical_pair_purity")
  !Reduce(`&`, lapply(measures, function(m) !is.na(overview[[m]]) & abs(overview[[m]] - 1) < 1e-9))
}

#' The display rows of the correspondence table and the values it states in prose
#'
#' Shared by [rh_efa_correspondence_overview_table()] and
#' [rh_efa_correspondence_text()], so that every measure that leaves the table
#' for holding one value in every row is the one the sentence states.
#'
#' @inheritParams rh_efa_correspondence_overview_table
#' @return List `df` (display rows, constant measures removed) and `constant`
#'   (named character vector: measure -> its one value), or `NULL` when every
#'   solution corresponds perfectly.
rh_efa_correspondence_display <- function(overview, criteria = NULL, membership = list(), labels = rh_labels()) {
  o <- tibble::as_tibble(overview)
  o <- o[rh_efa_correspondence_differs(o), , drop = FALSE]
  if (nrow(o) == 0L) return(NULL)
  counts <- if (is.null(criteria)) list(n = rep(NA_integer_, nrow(o)), of = NA_integer_) else
    rh_efa_criteria_counts(criteria, o$set_key, o$factors)
  df <- tibble::tibble(
    set = rh_blank_repeated(o$set_label),
    solution = rh_efa_solution_label(o$factors, o$solution == "expected count", counts$n, counts$of),
    adjusted_rand = rh_fmt(o$adjusted_rand, 2, bounded = TRUE),
    retention = rh_fmt(o$expected_pair_retention, 2, bounded = TRUE),
    purity = rh_fmt(o$empirical_pair_purity, 2, bounded = TRUE),
    shared = vapply(paste0(o$set_key, ":", o$factors), function(k) rh_efa_shared_factors(membership[[k]], labels), ""),
    weak = paste(rh_fmt_n(o$n_weak), "of", rh_fmt_n(o$n_items)),
    crossloading = rh_fmt_n(o$n_crossloading)
  )
  if (!any(nzchar(df$shared))) df$shared <- NULL
  constant <- rh_constant_columns(df, c("adjusted_rand", "retention", "purity", "weak", "crossloading"))
  list(df = df[setdiff(names(df), names(constant))], constant = constant)
}

#' SRQ1 correspondence of the solutions whose grouping differs from the scales
#'
#' The three preregistered measures of how the estimated grouping of the items
#' matches their intended scales, which scales share a factor, and the
#' loading-clarity counts, for every solution that does not reproduce the
#' intended grouping. A measure with one value in every shown row leaves the
#' table for the sentence of [rh_efa_correspondence_text()].
#'
#' @param overview The `overview` of [tabulate_efa_correspondence()].
#' @param threshold The loading threshold of loading clarity.
#' @param criteria Tibble from [tabulate_factor_number_criteria()], for the
#'   number of criteria behind each solution.
#' @param membership The `membership` of [tabulate_efa_correspondence()].
#' @param labels Named labels from [rh_labels()].
#' @param engine See [rh_use_gt()].
#' @param target Name(s) of the target the numbers come from; see [rh_source_note()].
#' @return Display table, or `NULL` when every solution corresponds perfectly.
rh_efa_correspondence_overview_table <- function(overview, threshold, criteria = NULL, membership = list(),
                                                 labels = rh_labels(), engine = "auto", target = NULL) {
  display <- rh_efa_correspondence_display(overview, criteria, membership, labels)
  if (is.null(display)) return(NULL)
  df <- display$df
  at <- rh_fmt(threshold, 2, bounded = TRUE)
  rh_table(
    df,
    col_labels = c(set = "Item set", solution = "Solution", adjusted_rand = "Adjusted Rand index",
                   retention = "Expected-pair retention", purity = "Empirical-pair purity",
                   shared = "Scales sharing a factor", weak = "Weakly loading items",
                   crossloading = "Cross-loading items")[names(df)],
    engine = engine, target = target,
    source_note = paste0(
      "Adjusted Rand index: agreement of the estimated grouping of the items with the intended scales ",
      "(1: identical, 0: chance level). Expected-pair retention: share of the item pairs of one scale that share ",
      "a factor; below 1, a scale was split. Empirical-pair purity: share of the item pairs on one factor that ",
      "belong to one scale; below 1, scales were merged. Weakly loading: no absolute loading of at least ", at,
      ". Cross-loading: two or more absolute loadings of at least ", at, "."
    )
  )
}

#' The correspondence of the solutions the table leaves out, in one or two sentences
#'
#' @inheritParams rh_efa_correspondence_overview_table
#' @param table_ref Reference to the table of the other solutions.
#' @return Sentences.
rh_efa_correspondence_text <- function(overview, criteria = NULL, membership = list(), labels = rh_labels(),
                                       table_ref = "the table") {
  o <- tibble::as_tibble(overview)
  differs <- rh_efa_correspondence_differs(o)
  perfect <- o[!differs, , drop = FALSE]
  shown <- o[differs, , drop = FALSE]
  join <- function(x) if (length(x) <= 1L) paste(x, collapse = "") else
    paste0(paste(x[-length(x)], collapse = ", "), " and ", x[length(x)])
  first <- if (nrow(perfect) == 0L) "" else {
    expected <- perfect$solution == "expected count"
    n_sets <- length(unique(o$set_key))
    where <- c(
      if (any(expected)) {
        if (sum(expected) == n_sets && n_sets > 1L) paste0("the expected solution of each of the ", rh_fmt_count(n_sets), " item sets")
        else paste0("the expected solution", if (sum(expected) > 1L) "s" else "", " of ", join(perfect$set_label[expected]))
      },
      if (any(!expected)) paste0(
        "the ", join(paste0(rh_fmt_n(perfect$factors[!expected]), "-factor solution of ", perfect$set_label[!expected]))
      )
    )
    paste0(
      "In ", join(where), ", every factor held the items of exactly one scale ",
      "(adjusted Rand index, expected-pair retention and empirical-pair purity all 1.00)."
    )
  }
  if (nrow(shown) == 0L) return(first)
  # the measures that left the table for holding one value in every row
  constant <- rh_efa_correspondence_display(overview, criteria, membership, labels)$constant
  stated <- c(
    if ("retention" %in% names(constant)) {
      if (constant[["retention"]] == "1.00") "none of them split a scale (expected-pair retention 1.00)" else
        paste0("expected-pair retention was ", constant[["retention"]], " in each")
    },
    if ("adjusted_rand" %in% names(constant)) paste0("the adjusted Rand index was ", constant[["adjusted_rand"]], " in each"),
    if ("purity" %in% names(constant)) paste0("empirical-pair purity was ", constant[["purity"]], " in each"),
    if ("weak" %in% names(constant)) paste0(constant[["weak"]], " items loaded weakly in each"),
    if ("crossloading" %in% names(constant)) {
      if (constant[["crossloading"]] == "0") "no item cross-loaded in them" else
        paste0(constant[["crossloading"]], " items cross-loaded in each")
    }
  )
  second <- paste0(
    table_ref, " gives the ", if (nrow(shown) == 1L) "solution" else paste(rh_fmt_count(nrow(shown)), "solutions"),
    " whose grouping differed", if (length(stated)) paste0("; ", join(stated)) else "", "."
  )
  paste(c(first[nzchar(first)], second), collapse = " ")
}

#' SRQ1 loading clarity of the scales analysed on their own
#'
#' With one scale the correspondence is complete by construction; what remains
#' to describe is how clearly its items load. Only the scales with a weakly
#' loading item are listed; the sentence of [rh_efa_scale_clarity_text()]
#' carries the others.
#'
#' @param scales The `scales` of [tabulate_efa_correspondence()].
#' @param labels Named labels from [rh_labels()].
#' @param threshold The loading threshold of loading clarity.
#' @param engine See [rh_use_gt()].
#' @param target Name(s) of the target the numbers come from; see [rh_source_note()].
#' @return Display table, or `NULL` when no item loads weakly.
rh_efa_scale_clarity_table <- function(scales, labels = rh_labels(), threshold, engine = "auto", target = NULL) {
  s <- tibble::as_tibble(scales)
  s <- s[!(s$n_weak %in% 0L), , drop = FALSE]
  if (nrow(s) == 0L) return(NULL)
  at <- rh_fmt(threshold, 2, bounded = TRUE)
  rh_table(
    tibble::tibble(scale = rh_label(s$scale_key, labels),
                   weak = paste(rh_fmt_n(s$n_weak), "of", rh_fmt_n(s$n_items))),
    col_labels = c(scale = "Scale", weak = "Weakly loading items"),
    engine = engine, target = target,
    source_note = paste0("One-factor solution of each scale analysed on its own. Weakly loading: no absolute loading ",
                         "of at least ", at, "; with one factor no item can cross-load.")
  )
}

#' The loading clarity of the scales analysed on their own, in one sentence
#'
#' @inheritParams rh_efa_scale_clarity_table
#' @param table_ref Reference to the table of the scales with weakly loading items.
#' @return One sentence.
rh_efa_scale_clarity_text <- function(scales, labels = rh_labels(), threshold, table_ref = "the table") {
  s <- tibble::as_tibble(scales)
  at <- rh_fmt(threshold, 2, bounded = TRUE)
  weak <- !(s$n_weak %in% 0L)
  if (!any(weak)) {
    return(paste0(
      "In the one-factor solution of each of the ", rh_fmt_count(nrow(s)), " scales analysed on its own, every ",
      "item loaded at least ", at, "; with one factor, no item can cross-load."
    ))
  }
  paste0(
    table_ref, " lists the ", if (sum(weak) == 1L) "scale" else paste(rh_fmt_count(sum(weak)), "scales"),
    " analysed on its own with an item loading below ", at,
    if (any(!weak)) paste0("; in the other ", rh_fmt_count(sum(!weak)), ", every item loaded at least ", at) else "",
    "."
  )
}

#' The scales a factor carries, in words
#'
#' A factor is named by the scales most of whose items it carries
#' ([tabulate_efa_correspondence()] records them as `;`-joined keys); a factor
#' that carries no scale's items is named by its own name.
#'
#' @param scales Character vector of `;`-joined scale keys, or `NA`.
#' @param factors Character vector of factor names, used where `scales` is `NA`.
#' @param labels Named labels from [rh_labels()].
#' @param inline Logical; labels for use inside a sentence ([rh_label_inline()]).
#' @return Character vector such as `"ASC submission and Conventionalism"`.
rh_efa_factor_scales_label <- function(scales, factors, labels = rh_labels(), inline = FALSE) {
  vapply(seq_along(scales), function(i) {
    keys <- if (is.na(scales[[i]]) || !nzchar(scales[[i]])) character() else strsplit(scales[[i]], ";", fixed = TRUE)[[1]]
    if (length(keys) == 0L) return(if (is.na(factors[[i]])) "—" else paste("factor", factors[[i]]))
    named <- if (inline) rh_label_inline(keys, labels) else rh_label(keys, labels)
    if (length(named) == 1L) named else paste0(paste(named[-length(named)], collapse = ", "), " and ", named[length(named)])
  }, character(1))
}

#' SRQ1 item membership of the expected solutions, one row per item
#'
#' The expected solutions of the item sets of one group (the motive item sets,
#' or the item sets of authoritarian orientation) side by side: for each solution the item's largest absolute
#' loading and its next-largest one. Item sets whose items do not overlap share
#' a pair of columns (UMS-6 and DoPL-6). Every factor is named after the scale
#' whose items it carries; an item marked "c" has its largest loading on the
#' factor of another scale, an item marked "a" cross-loads (two or more
#' loadings of at least the threshold, the secondary factor named in the note)
#' and an item marked "b" loads weakly (no loading of at least the threshold).
#'
#' @param membership The `membership` list of [tabulate_efa_correspondence()].
#' @param sets The configured item sets of the group (`analysis_plan$factor_analysis$sets`
#'   entries), in display order.
#' @param labels Named labels from [rh_labels()].
#' @param threshold The loading threshold of loading clarity.
#' @param scale_order Scale keys in display order.
#' @param item_codes The item codes of the codebook: an item the factor
#'   analyses name with other separators ("SDO-D_1") is shown under its code
#'   ("SDO_D_1"), as the other tables show it.
#' @param engine See [rh_use_gt()].
#' @param target Name(s) of the target the numbers come from; see [rh_source_note()].
#' @return Display table, or `NULL` when no solution of the group is available.
rh_efa_membership_table <- function(membership, sets, labels = rh_labels(), threshold, scale_order = NULL,
                                    item_codes = NULL, engine = "auto", target = NULL) {
  at <- rh_fmt(threshold, 2, bounded = TRUE)
  solutions <- lapply(sets, function(set) {
    m <- membership[[paste0(set$key, ":", set$expected_factors)]]
    if (is.null(m) || nrow(m) == 0L) return(NULL)
    list(key = as.character(set$key), label = sub(" \\(.*\\)$", "", as.character(set$label)),
         factors = as.integer(set$expected_factors), rows = tibble::as_tibble(m))
  })
  solutions <- Filter(Negate(is.null), solutions)
  if (length(solutions) == 0L) return(NULL)
  # Item sets whose items do not overlap share one pair of columns.
  columns <- list()
  for (sol in solutions) {
    placed <- FALSE
    for (k in seq_along(columns)) {
      used <- unlist(lapply(columns[[k]], function(x) x$rows$item))
      if (!any(sol$rows$item %in% used) && all(vapply(columns[[k]], function(x) x$factors == sol$factors, logical(1)))) {
        columns[[k]] <- c(columns[[k]], list(sol))
        placed <- TRUE
        break
      }
    }
    if (!placed) columns[[length(columns) + 1L]] <- list(sol)
  }
  items <- unique(unlist(lapply(solutions, function(x) x$rows$item)))
  scale_of <- unlist(lapply(solutions, function(x) stats::setNames(as.character(x$rows$intended_scale), x$rows$item)))
  scale_of <- scale_of[!duplicated(names(scale_of))]
  scale_keys <- unname(scale_of[items])
  order <- order(match(scale_keys, if (is.null(scale_order)) unique(scale_keys) else scale_order), seq_along(items))
  items <- items[order]
  scale_keys <- scale_keys[order]
  canonical <- function(x) gsub("[^a-z0-9]", "", tolower(x))
  shown_items <- if (is.null(item_codes)) items else {
    hit <- item_codes[match(canonical(items), canonical(item_codes))]
    ifelse(is.na(hit), items, hit)
  }
  shown_of <- stats::setNames(shown_items, items)
  df <- tibble::tibble(scale = rh_blank_repeated(rh_label(scale_keys, labels)), item = shown_items)
  heads <- c(scale = "Scale", item = "Item")
  notes <- list(a = tibble::tibble(item = character(), solution = character(), detail = character()),
                b = tibble::tibble(item = character(), solution = character(), detail = character()),
                c = tibble::tibble(item = character(), solution = character(), detail = character()))
  for (k in seq_along(columns)) {
    group <- columns[[k]]
    largest <- next_largest <- rep("—", length(items))
    for (sol in group) {
      m <- sol$rows
      i <- match(m$item, items)
      own_factor <- vapply(split(m$assigned_factor, m$intended_scale), function(x) names(sort(table(x), decreasing = TRUE))[1], "")
      off <- m$assigned_factor != own_factor[m$intended_scale]
      mark <- ifelse(m$crossloading %in% TRUE, "a", ifelse(m$weak %in% TRUE, "b", ""))
      mark <- paste0(mark, ifelse(off, "c", ""))
      largest[i] <- paste0(rh_fmt(m$largest_abs_loading, 2, bounded = TRUE),
                           ifelse(nzchar(mark), paste0("<sup>", gsub("(.)(?=.)", "\\1,", mark, perl = TRUE), "</sup>"), ""))
      next_largest[i] <- rh_fmt(m$next_largest_abs_loading, 2, bounded = TRUE)
      solution_name <- paste0(sol$label, ", ", rh_fmt_n(sol$factors), " factors")
      cross <- m[m$crossloading %in% TRUE, , drop = FALSE]
      if (nrow(cross)) notes$a <- dplyr::bind_rows(notes$a, tibble::tibble(
        item = unname(shown_of[cross$item]), solution = solution_name,
        detail = paste0("its second loading lies on the factor of ",
                        rh_efa_factor_scales_label(cross$crossloading_scales, cross$crossloading_factors, labels,
                                                   inline = TRUE))
      ))
      weak <- m[m$weak %in% TRUE, , drop = FALSE]
      if (nrow(weak)) notes$b <- dplyr::bind_rows(notes$b, tibble::tibble(
        item = unname(shown_of[weak$item]), solution = solution_name, detail = ""
      ))
      if (any(off)) notes$c <- dplyr::bind_rows(notes$c, tibble::tibble(
        item = unname(shown_of[m$item[off]]), solution = solution_name,
        detail = paste0("on the factor of ", rh_efa_factor_scales_label(m$assigned_scales[off], m$assigned_factor[off],
                                                                         labels, inline = TRUE))
      ))
    }
    name <- paste0(paste(vapply(group, function(x) x$label, ""), collapse = " or "), ", ",
                   rh_fmt_n(group[[1]]$factors), " factors")
    df[[paste0("largest_", k)]] <- largest
    df[[paste0("next_", k)]] <- next_largest
    heads[paste0("largest_", k)] <- paste0(name, ": largest loading")
    heads[paste0("next_", k)] <- paste0(name, ": next-largest loading")
  }
  # one clause per item and detail, naming the solutions it holds in
  clauses <- function(n) {
    if (nrow(n) == 0L) return(character())
    keys <- unique(n[c("item", "detail")])
    vapply(seq_len(nrow(keys)), function(i) {
      solutions <- n$solution[n$item == keys$item[[i]] & n$detail == keys$detail[[i]]]
      where <- if (length(solutions) == length(columns) && length(columns) > 1L) "in both solutions" else
        paste0("in ", paste(solutions, collapse = " and "))
      paste0(keys$item[[i]], " ", where, if (nzchar(keys$detail[[i]])) paste0(", ", keys$detail[[i]]) else "")
    }, character(1))
  }
  join <- function(x) paste(x, collapse = "; ")
  specific <- c(
    if (nrow(notes$a)) paste0("<sup>a</sup> Cross-loading, with two or more absolute loadings of at least ", at, ": ",
                              join(clauses(notes$a)), "."),
    if (nrow(notes$b)) paste0("<sup>b</sup> Weakly loading, with no absolute loading of at least ", at, ": ",
                              join(clauses(notes$b)), "."),
    if (nrow(notes$c)) paste0("<sup>c</sup> Largest loading on the factor of another scale: ", join(clauses(notes$c)), ".")
  )
  rh_table(
    df, col_labels = heads, engine = engine, target = target, markdown = grep("^largest_", names(df), value = TRUE),
    source_note = paste(c(
      paste0(
        "Absolute pattern loadings in the expected solution of each item set, one factor per scale. ",
        if (nrow(notes$c) == 0L) "Every item had its largest loading on the factor of its own scale. " else "",
        "An em dash marks an item outside that item set."
      ),
      specific
    ), collapse = " ")
  )
}

#' Publication-facing CFA reporting models
#'
#' @param cfa_scales,cfa_sets Tibbles from [ap4_cfa()] and
#'   [ap4_cfa_sets()].
#' @return A list with one model/label/item-count/reporting record per fitted
#'   CFA, in report order.
rh_cfa_reporting_models <- function(cfa_scales, cfa_sets = NULL) {
  rows <- list()
  add <- function(x) {
    if (is.null(x)) return()
    # `ap4_cfa()` carries no `label` column and `ap4_cfa_sets()` does; a model
    # that did not converge has no reporting loadings to take the label from.
    set_label <- x[["label"]]
    for (i in seq_len(nrow(x))) {
      reporting <- x$reporting[[i]]
      label <- if (nrow(reporting$loadings) > 0L) {
        as.character(reporting$loadings$model_label[1L])
      } else if (!is.null(set_label)) {
        as.character(set_label[i])
      } else {
        as.character(x$model[i])
      }
      rows[[length(rows) + 1L]] <<- list(
        model = as.character(x$model[i]),
        label = label,
        n_items = as.integer(x$n_items[i]),
        converged = x$converged[i],
        admissible = x$admissible[i],
        note = x$note[i],
        error = x$error[i],
        reporting = reporting
      )
    }
  }
  add(cfa_scales)
  add(cfa_sets)
  rows
}

#' Standardised loadings of the confirmatory models, one row per item
#'
#' The loadings of one group of items (the motive items, or the items of
#' authoritarian orientation), grouped by scale: one column for the one-factor model of the item's
#' scale and one for each multi-factor model that holds items of the group. An
#' em dash marks a scale that no model of that column holds.
#'
#' @param loadings Tibble from [tabulate_cfa_loadings_by_model()].
#' @param result_file Display path of the unstandardised-loadings data file.
#' @param scales Scale keys of the items to show, in display order; `NULL`
#'   shows every item.
#' @param labels Named labels from [rh_labels()]; the scales are named as the
#'   report names them.
#' @inheritParams rh_table
#' @return Display table.
rh_cfa_loadings_by_model_table <- function(loadings, result_file, scales = NULL, labels = rh_labels(),
                                           engine = "auto", target = NULL) {
  family_labels <- attr(loadings, "family_labels")
  families <- names(family_labels)
  rows <- tibble::as_tibble(loadings)
  key <- if ("scale_key" %in% names(rows)) rows$scale_key else rows$scale
  if (!is.null(scales)) {
    keep <- key %in% scales
    rows <- rows[keep, , drop = FALSE]
    key <- key[keep]
    order <- order(match(key, scales), seq_along(key))
    rows <- rows[order, , drop = FALSE]
    key <- key[order]
  }
  scale_name <- if ("scale_key" %in% names(rows)) rh_label(key, labels) else rows$scale
  out <- tibble::tibble(scale = rh_blank_repeated(scale_name), item = rows$item)
  shown <- families[vapply(families, function(family) any(!is.na(rows[[family]])), logical(1))]
  for (family in shown) out[[family]] <- rh_fmt(rows[[family]], 2, bounded = TRUE)
  diagnostic <- attr(loadings, "diagnostic_only")
  rh_table(
    out,
    col_labels = c(scale = "Scale", item = "Item", family_labels[shown]),
    engine = engine, target = target,
    source_note = paste0(
      "Completely standardised loadings. An em dash marks a scale that the model of that column does not hold. ",
      "The unstandardised loadings and the standard errors of both metrics are in ", result_file, ".",
      if (length(diagnostic)) paste0(" Diagnostic output only, the model did not reach an admissible solution: ",
                                     paste(diagnostic, collapse = "; "), ".") else ""
    )
  )
}

#' The latent correlations of one group of scales, one estimate per pair
#'
#' Each pair is taken from the confirmatory model with the most factors that
#' holds both scales (the five-factor model of the motives, the four-factor
#' model of SDO-D with ASC); the smaller models that hold the same pair are
#' compared with it by [rh_cfa_factor_correlation_agreement_text()].
#'
#' @param correlations Tibble from [tabulate_cfa_factor_correlations()].
#' @param scales Scale keys of the group, in display order.
#' @return Tibble `factor_1_key`, `factor_2_key`, `model`, `model_key`,
#'   `correlation`, `se`, `others` (a list of the other models' rows).
rh_cfa_factor_correlation_pairs <- function(correlations, scales) {
  cor <- tibble::as_tibble(correlations)
  if (nrow(cor) == 0L) return(cor)
  size <- vapply(split(c(cor$factor_1_key, cor$factor_2_key), c(cor$model_key, cor$model_key)),
                 function(x) length(unique(x)), integer(1))
  cor$n_factors <- unname(size[cor$model_key])
  cor$pair <- rh_network_pair_key(cor$factor_1_key, cor$factor_2_key)
  cor <- cor[cor$factor_1_key %in% scales & cor$factor_2_key %in% scales, , drop = FALSE]
  dplyr::bind_rows(lapply(split(cor, cor$pair), function(rows) {
    rows <- rows[order(-rows$n_factors), , drop = FALSE]
    first <- rows[1, , drop = FALSE]
    first$others <- list(rows[-1, , drop = FALSE])
    first
  }))
}

#' Latent correlations of one group of scales as a lower-triangle matrix
#'
#' One cell per pair: the latent correlation with its standard error in
#' parentheses (estimate and uncertainty in one cell).
#'
#' @param correlations Tibble from [tabulate_cfa_factor_correlations()].
#' @param scales Scale keys of the group, in display order.
#' @param labels Named labels from [rh_labels()].
#' @param row_title Head of the row-label column.
#' @inheritParams rh_table
#' @return Display table, or `NULL` when no model holds two scales of the group.
rh_cfa_factor_correlation_matrix_table <- function(correlations, scales, labels = rh_labels(), row_title = "Scale",
                                                   engine = "auto", target = NULL) {
  pairs <- rh_cfa_factor_correlation_pairs(correlations, scales)
  if (nrow(pairs) == 0L) return(NULL)
  present <- scales[scales %in% c(pairs$factor_1_key, pairs$factor_2_key)]
  cell <- function(row, column) {
    if (match(column, present) >= match(row, present)) return("")
    hit <- pairs[pairs$pair == rh_network_pair_key(row, column), , drop = FALSE]
    if (nrow(hit) == 0L) return("—")
    rh_fmt_est_se(hit$correlation, hit$se, 2, bounded = TRUE)
  }
  rows <- present[-1]
  columns <- present[-length(present)]
  df <- tibble::tibble(scale = rh_label(rows, labels))
  keys <- paste0("column_", seq_along(columns))
  for (k in seq_along(columns)) df[[keys[k]]] <- vapply(rows, cell, "", column = columns[[k]], USE.NAMES = FALSE)
  models <- unique(pairs$model)
  rh_table(
    df, col_labels = c(scale = row_title, stats::setNames(rh_label(columns, labels), keys)),
    engine = engine, target = target,
    source_note = paste0(
      "Latent correlation (standard error) from the confirmatory model ",
      if (length(models) == 1L) paste0("“", models, "”") else
        paste0("with the most factors that holds both scales (", paste0("“", models, "”", collapse = ", "), ")"),
      "."
    )
  )
}

#' Whether the smaller confirmatory models give the same latent correlations
#'
#' The two-factor models of UMS-6 and DoPL-6 and the three-factor ASC model
#' estimate pairs that the larger models estimate too. Where every such value
#' equals the value shown at two decimals, one sentence says so; otherwise the
#' sentence names each pair that differs (identical values become a
#' sentence).
#'
#' @param correlations Tibble from [tabulate_cfa_factor_correlations()].
#' @param groups List of scale-key vectors, one per matrix.
#' @param labels Named labels from [rh_labels()].
#' @return One sentence, or `""` when no smaller model repeats a pair.
rh_cfa_factor_correlation_agreement_text <- function(correlations, groups, labels = rh_labels()) {
  pairs <- dplyr::bind_rows(lapply(groups, function(scales) rh_cfa_factor_correlation_pairs(correlations, scales)))
  if (nrow(pairs) == 0L) return("")
  others <- dplyr::bind_rows(lapply(seq_len(nrow(pairs)), function(i) {
    o <- pairs$others[[i]]
    if (is.null(o) || nrow(o) == 0L) return(NULL)
    tibble::tibble(pair = pairs$pair[[i]], factor_1_key = pairs$factor_1_key[[i]], factor_2_key = pairs$factor_2_key[[i]],
                   shown = pairs$correlation[[i]], model = o$model, value = o$correlation)
  }))
  if (nrow(others) == 0L) return("")
  smaller <- unique(others$model)
  same <- rh_fmt(others$value, 2, bounded = TRUE) == rh_fmt(others$shown, 2, bounded = TRUE)
  quoted <- function(x) paste0("“", x, "”")
  join <- function(x) if (length(x) <= 1L) x else paste0(paste(x[-length(x)], collapse = ", "), " and ", x[length(x)])
  if (all(same)) {
    return(paste0(
      "The smaller ", if (length(smaller) == 1L) "model " else "models ", join(quoted(smaller)),
      if (length(smaller) == 1L) " gives" else " give",
      " the same latent correlations for the pairs they share, to two decimals."
    ))
  }
  differ <- others[!same, , drop = FALSE]
  paste0(
    "For the pairs they share, the smaller models ", join(quoted(smaller)), " give the same latent correlations ",
    "to two decimals except ",
    join(paste0(rh_network_edge_label(differ$factor_1_key, differ$factor_2_key, labels), " in ", quoted(differ$model),
                " (", rh_fmt(differ$value, 2, bounded = TRUE), " against ", rh_fmt(differ$shown, 2, bounded = TRUE), ")")),
    "."
  )
}

#' The largest residual correlations of every confirmatory model
#'
#' Each model's pairs in the order of their absolute standardised residual
#' (the largest `factor_analysis$cfa_reporting$ranked_residuals_n`); the model
#' name stands once per block.
#'
#' @param residuals Tibble from [tabulate_cfa_largest_residuals()].
#' @param residual_file Display path of the file with all residual pairs.
#' @param labels Named labels from [rh_labels()]; the one-factor models are
#'   named by their scale as the report names it.
#' @inheritParams rh_table
#' @return Display table.
rh_cfa_largest_residuals_table <- function(residuals, residual_file, labels = rh_labels(), engine = "auto",
                                           target = NULL) {
  model <- as.character(residuals$model)
  if ("model_key" %in% names(residuals)) {
    single <- residuals$model_key %in% names(labels)
    model[single] <- paste0(rh_label(residuals$model_key[single], labels), " (one factor)")
  }
  rh_table(
    tibble::tibble(
      model = rh_blank_repeated(model),
      item_1 = residuals$item_1, item_2 = residuals$item_2,
      # A correlation residual is bounded, so no leading zero;
      # the standardised residual is a z statistic and keeps its leading zero.
      correlation_residual = rh_fmt(residuals$correlation_residual, 2, bounded = TRUE),
      standardised_residual = rh_fmt(residuals$standardised_residual, 2)
    ),
    col_labels = c(model = "Model", item_1 = "Item 1", item_2 = "Item 2",
                   correlation_residual = "Correlation residual",
                   standardised_residual = "Standardised residual"),
    engine = engine, target = target,
    source_note = paste0(
      "The pairs of each model in the order of their absolute standardised residual. All pairs of every model: ",
      residual_file, "."
    )
  )
}

#' The scales and item sets on which the factor-number criteria disagree
#'
#' A row whose every available criterion suggests the expected number of
#' factors, without a failed criterion, becomes the sentence of
#' [rh_factor_number_criteria_text()] (identical rows become a sentence).
#'
#' @param criteria Tibble from [tabulate_factor_number_criteria()].
#' @return Logical vector, `TRUE` for a row the table shows.
rh_factor_number_criteria_differs <- function(criteria) {
  fixed <- c("key", "label", "n_items", "expected", "note")
  indices <- setdiff(names(criteria), fixed)
  vapply(seq_len(nrow(criteria)), function(i) {
    suggested <- as.integer(unlist(criteria[i, indices]))
    anyNA(suggested) || any(suggested != as.integer(criteria$expected[[i]])) ||
      (!is.na(criteria$note[[i]]) && nzchar(criteria$note[[i]]))
  }, logical(1))
}

#' Factor-number criteria of the scales and item sets on which they disagree
#'
#' The nine preregistered criteria side by side, beside the number of factors
#' the scale structure expects, for every scale or item set on which at least
#' one criterion suggested another number or failed. A single scale is named
#' as the report names it.
#'
#' @param criteria Tibble from [tabulate_factor_number_criteria()].
#' @param labels Named labels from [rh_labels()].
#' @inheritParams rh_table
#' @return Display table, or `NULL` when every criterion suggests the expected number everywhere.
rh_factor_number_criteria_table <- function(criteria, labels = rh_labels(), engine = "auto", target = NULL) {
  fixed <- c("key", "label", "n_items", "expected", "note")
  indices <- setdiff(names(criteria), fixed)
  criteria <- criteria[rh_factor_number_criteria_differs(criteria), , drop = FALSE]
  if (nrow(criteria) == 0L) return(NULL)
  single <- criteria$key %in% names(labels)
  name <- as.character(criteria$label)
  name[single] <- rh_label(criteria$key[single], labels)
  out <- tibble::tibble(label = name, n_items = rh_fmt_n(criteria$n_items),
                        expected = rh_fmt_n(criteria$expected))
  for (index in indices) out[[index]] <- rh_fmt_n(criteria[[index]])
  failed <- !is.na(criteria$note) & nzchar(criteria$note)
  if (any(failed)) out$note <- ifelse(failed, criteria$note, "")
  rh_table(
    out,
    col_labels = c(label = "Scale or item set", n_items = "Items", expected = "Expected",
                   stats::setNames(rh_factor_index_label(indices), indices), note = "Failed criteria")[names(out)],
    engine = engine, target = target,
    source_note = "Expected: number of factors in the intended scale structure, one per scale."
  )
}

#' The scales and item sets on which every criterion agrees, in one sentence
#'
#' @inheritParams rh_factor_number_criteria_table
#' @param table_ref Reference to the table of the other rows.
#' @return One or two sentences.
rh_factor_number_criteria_text <- function(criteria, labels = rh_labels(), table_ref = "the table") {
  fixed <- c("key", "label", "n_items", "expected", "note")
  n_criteria <- length(setdiff(names(criteria), fixed))
  differs <- rh_factor_number_criteria_differs(criteria)
  single <- criteria$key %in% names(labels)
  join <- function(x) if (length(x) <= 1L) paste(x, collapse = "") else
    paste0(paste(x[-length(x)], collapse = ", "), " and ", x[length(x)])
  agree <- c(
    if (any(single & !differs)) {
      if (all(!differs[single])) {
        if (sum(single) == 1L) "the one scale analysed on its own"
        else paste0("the ", rh_fmt_count(sum(single)), " scales analysed on their own")
      }
      else join(rh_label_inline(criteria$key[single & !differs], labels))
    },
    if (any(!single & !differs)) paste0("the item set", if (sum(!single & !differs) > 1L) "s " else " ",
                                        join(criteria$label[!single & !differs]))
  )
  first <- if (length(agree)) paste0(
    "All ", rh_fmt_count(n_criteria), " criteria suggested the expected number of factors for ", join(agree), "."
  ) else ""
  second <- if (any(differs)) paste0(
    table_ref, " gives the number each criterion suggested for ",
    if (sum(differs) == 1L) "the scale or item set" else paste("the", rh_fmt_count(sum(differs)), "scales or item sets"),
    " on which they disagreed."
  ) else ""
  paste(c(first, second)[nzchar(c(first, second))], collapse = " ")
}

#' Variance explained by every examined solution, one row per solution
#'
#' Each factor is named after the scale whose items it carries where the
#' solution matches the scales one to one, the factors then listed in the
#' codebook's order of their scales; otherwise the factors are numbered in the
#' order of the variance they explain, with the scales each carries. The single-scale solutions carry their scale key in
#' `scale_key`.
#'
#' @param variance Tibble from [tabulate_efa_set_variance()].
#' @param membership The `membership` of [tabulate_efa_correspondence()], for
#'   the scales each factor of an item set carries.
#' @param sets The configured item sets (`analysis_plan$factor_analysis$sets`),
#'   for the set keys behind the labels.
#' @param criteria Tibble from [tabulate_factor_number_criteria()], for the
#'   number of criteria behind each solution.
#' @param labels Named labels from [rh_labels()].
#' @inheritParams rh_table
#' @return Display table.
rh_efa_set_variance_table <- function(variance, membership = list(), sets = list(), criteria = NULL,
                                      labels = rh_labels(), engine = "auto", target = NULL) {
  v <- tibble::as_tibble(variance)
  single <- if ("scale_key" %in% names(v)) !is.na(v$scale_key) else rep(FALSE, nrow(v))
  set_key_of <- stats::setNames(vapply(sets, function(s) as.character(s$key), ""),
                                vapply(sets, function(s) as.character(s$label), ""))
  v$key <- ifelse(single, v$scale_key, unname(set_key_of[as.character(v$set)]))
  v$name <- ifelse(single, rh_label(v$scale_key, labels), as.character(v$set))
  v$solution_id <- paste(v$name, v$factors, v$solution, sep = "\r")
  ids <- unique(v$solution_id)
  rows <- lapply(ids, function(id) {
    r <- v[v$solution_id == id, , drop = FALSE]
    r <- r[order(-r$proportion), , drop = FALSE]
    k <- r$factors[[1]]
    carried <- if (single[match(id, v$solution_id)]) {
      rep(r$scale_key[[1]], nrow(r))
    } else {
      m <- membership[[paste0(r$key[[1]], ":", k)]]
      vapply(r$factor, function(f) {
        if (is.null(m) || !"assigned_scales" %in% names(m)) return(NA_character_)
        hit <- unique(m$assigned_scales[m$assigned_factor == f & !is.na(m$assigned_scales)])
        if (length(hit) == 1L) hit else NA_character_
      }, "")
    }
    one_to_one <- !anyNA(carried) && !any(grepl(";", carried)) && !anyDuplicated(carried)
    # Factors that are the set's scales one to one are listed as the set lists
    # its scales, in the codebook's order; numbered factors keep the order of
    # the variance they explain.
    set_match <- Filter(function(s) identical(as.character(s$key), as.character(r$key[[1]])), sets)
    set_scales <- if (length(set_match)) as.character(unlist(set_match[[1]]$scales)) else character()
    if (one_to_one && k > 1L && length(set_scales) && all(carried %in% set_scales)) {
      in_order <- order(match(carried, set_scales))
      r <- r[in_order, , drop = FALSE]
      carried <- carried[in_order]
    }
    names_shown <- if (one_to_one) rh_label(carried, labels) else {
      paste0("Factor ", seq_len(nrow(r)),
             ifelse(is.na(carried), "", paste0(" (", rh_efa_factor_scales_label(carried, r$factor, labels, inline = TRUE), ")")))
    }
    tibble::tibble(
      name = r$name[[1]], key = r$key[[1]], factors = k, expected = r$solution[[1]] == "expected count",
      by_factor = if (k == 1L) "—" else paste0(names_shown, " ", rh_fmt(r$proportion, 2, bounded = TRUE), collapse = "; "),
      total = rh_fmt(sum(r$proportion), 2, bounded = TRUE)
    )
  })
  df <- dplyr::bind_rows(rows)
  counts <- if (is.null(criteria)) list(n = rep(NA_integer_, nrow(df)), of = NA_integer_) else
    rh_efa_criteria_counts(criteria, df$key, df$factors)
  out <- tibble::tibble(
    set = rh_blank_repeated(df$name),
    solution = rh_efa_solution_label(df$factors, df$expected, counts$n, counts$of),
    by_factor = df$by_factor,
    total = df$total
  )
  rh_table(
    out,
    col_labels = c(set = "Scale or item set", solution = "Solution", by_factor = "Proportion of variance by factor",
                   total = "Total"),
    engine = engine, target = target,
    source_note = paste(
      "Proportion of the item variance each factor accounts for, in the primary rotation, factors in the order",
      "of the variance they explain; an em dash marks a solution of one factor, whose share is the total. A",
      "factor is named after the scale whose items it carries where the solution holds one factor per scale;",
      "otherwise the factors are numbered, with the scales each carries."
    )
  )
}

#' Items that leave their scale's factor in an alternative solution
#'
#' @param items Tibble from [tabulate_efa_items_off_scale()].
#' @param labels Named labels from [rh_labels()].
#' @param result_file Display path of the complete membership data file.
#' @inheritParams rh_table
#' @return Display table, or `NULL` when no item leaves its scale's factor.
rh_efa_items_off_scale_table <- function(items, labels = rh_labels(), result_file, engine = "auto", target = NULL) {
  if (is.null(items) || !nrow(items)) return(NULL)
  note <- paste0(
    "In each solution the criteria suggested beside the expected one: the items assigned to a different ",
    "factor than most items of their intended scale. Complete membership of every solution is in ",
    result_file, "."
  )
  rh_table(
    tibble::tibble(set = items$set, factors = rh_fmt_n(items$factors), item = items$item,
                   scale = rh_label(items$intended_scale, labels), assigned = items$assigned_factor,
                   scale_factor = items$scale_factor),
    col_labels = c(set = "Item set", factors = "Factors", item = "Item", scale = "Intended scale",
                   assigned = "Assigned factor", scale_factor = "Factor of most of its scale"),
    engine = engine, target = target, source_note = note
  )
}

#' The items that leave their scale's factor, in one sentence
#'
#' @param items Tibble from [tabulate_efa_items_off_scale()].
#' @param result_file Display path of the complete membership data file.
#' @param table_ref Reference to the table of those items.
#' @return One sentence.
rh_efa_items_off_scale_text <- function(items, result_file, table_ref = "the table") {
  if (is.null(items) || !nrow(items)) {
    return(paste0(
      "In the solutions the criteria suggested beside the expected one, no item was assigned to a different factor ",
      "than most items of its scale; scales merged onto one factor show in the correspondence above. The complete ",
      "membership of every solution is in ", result_file, "."
    ))
  }
  paste0(
    table_ref, " lists the ", if (nrow(items) == 1L) "item" else paste(rh_fmt_n(nrow(items)), "items"),
    " assigned to a different factor than most items of ", if (nrow(items) == 1L) "its" else "their",
    " scale in the solutions the criteria suggested beside the expected one."
  )
}

#' Display form of the validity-gate status of a fit
#'
#' @param x Character vector with values `ok`, `retried_ok`,
#'   `not_interpretable` (attribute `gate_status` of a gated fit).
#' @return Character vector of display labels. An unmapped status is passed
#'   through, so a new status is visible instead of blank, and a missing one
#'   shows an em dash.
rh_gate_status_label <- function(x) {
  map <- c(
    ok = "ok", retried_ok = "ok after one retry (doubled warmup)",
    not_interpretable = "not interpretable (no classification by the preregistered rule)"
  )
  x <- as.character(x)
  out <- unname(map[x])
  out[is.na(out)] <- x[is.na(out)]
  out[is.na(x)] <- "—"
  out
}

#' Prior-sensitivity table (descriptive)
#'
#' One row per motive coefficient, one column per prior of the sweep, each cell
#' the posterior median with its credible interval, bold where the interval
#' excludes zero (the prior-width table without its constant columns; power
#' scaling as a sentence). The priors are named as the
#' report names them ("narrower", "preregistered", "wider") with their SD.
#'
#' @param sensitivity Tibble from `tabulate_prior_width_sweep()`.
#' @param labels Named labels from [rh_labels()].
#' @param engine See [rh_use_gt()].
#' @param target Name(s) of the target the numbers come from; see [rh_source_note()].
#' @param analysis_plan Configuration; supplies the interval level
#'   (`analysis_plan$regression$ci_level`) and the names of the priors.
#' @return Display table.
rh_sensitivity_table <- function(sensitivity, labels = rh_labels(), engine = "auto", analysis_plan, target = NULL) {
  s <- tibble::as_tibble(sensitivity)
  ci_what <- rh_ci_label(rh_cfg_ci_level(analysis_plan))
  df <- tibble::tibble(
    outcome = rh_blank_repeated(rh_label(s$outcome, labels)),
    term = rh_label(s$term, labels)
  )
  col_labels <- c(outcome = "Outcome", term = "Motive")
  sds <- sub("^est_", "", grep("^est_", names(s), value = TRUE))
  sds <- sds[order(as.numeric(sds))]
  for (sd in sds) {
    nm <- paste0("sd_", sd)
    lo <- s[[paste0("lo_", sd)]]
    hi <- s[[paste0("hi_", sd)]]
    excludes <- !is.na(lo) & !is.na(hi) & (lo > 0 | hi < 0)
    df[[nm]] <- rh_fmt_est_ci(s[[paste0("est_", sd)]], lo, hi, bold = excludes)
    sweep_name <- rh_sweep_label(as.numeric(sd), analysis_plan)
    col_labels[nm] <- paste0(
      if (is.na(sweep_name)) "Prior" else paste0(toupper(substr(sweep_name, 1, 1)), substring(sweep_name, 2), " prior"),
      " (SD ", rh_fmt(as.numeric(sd), 2), ")"
    )
  }
  invalid <- !(s$comparison_valid %in% TRUE)
  rh_table(
    df, col_labels = col_labels, engine = engine, target = target,
    markdown = grep("^sd_", names(df), value = TRUE),
    source_note = paste0(
      "Posterior median [", ci_what, "] of each motive coefficient under the normal prior of the stated standard ",
      "deviation on the standardised slopes; bold estimate: its interval excludes zero.",
      if (any(invalid)) " An invalid or missing fit withholds the comparison of its coefficients across the priors." else ""
    )
  )
}

#' Pretty column names ("slope_sd_0.2" -> "slope sd 0.2")
#'
#' Quantile columns are labelled by their own probability (`q2.5` -> "2.5%"),
#' interval columns as "CrI"; no interval level is typed here.
#'
#' @param x Character vector of column names.
#' @return Character vector.
rh_pretty_names <- function(x) {
  out <- gsub("_", " ", x)
  out <- sub("^estimate", "Median", out)
  out <- sub("^ci ", "CrI ", out)
  out <- sub("^q([0-9.]+)", "\\1%", out)
  out
}

#' priorsense power-scaling table by prior block
#'
#' One row per prior block x inspected quantity (`ap6_priorsense()`): the
#' block whose prior was power-scaled (`block`; the all-priors run is
#' labelled as such), both sensitivity axes, the priorsense diagnosis, the
#' largest Pareto k of the importance-sampling step when recorded, and the
#' settings status per row (`explicit`, or `error_no_fallback` when the call
#' failed for that block and no substitute was computed).
#'
#' @param ps The power-scaling entries to list: those flagged with a diagnosis
#'   (`priorsense_flagged` of S3); without one the function returns `NULL`.
#' @param result_file Display path of the data file with every entry.
#' @param labels Named labels from [rh_labels()].
#' @param engine See [rh_use_gt()].
#' @param target Name(s) of the target the numbers come from; see [rh_source_note()].
#' @return Display table.
rh_priorsense_table <- function(ps, labels = rh_labels(), result_file, engine = "auto", target = NULL) {
  file_note <- paste0("Entries with a sensitivity diagnosis, failed/unavailable original-fit validity or unreliable/unavailable importance sampling are listed; all entries are in ",
                      result_file, ".")
  # No flagged entry: the prose says so in one sentence.
  if (!nrow(ps)) return(NULL)
  df <- tibble::as_tibble(ps)
  if (!"importance_sampling_status" %in% names(df)) df$importance_sampling_status <- "unavailable"
  if (!"pareto_k_threshold" %in% names(df)) df$pareto_k_threshold <- NA_real_
  if (!"fit_valid" %in% names(df)) df$fit_valid <- NA
  if (!"fit_gate_status" %in% names(df)) df$fit_gate_status <- "unavailable"
  df$diagnosis <- describe_power_scaling_diagnosis(df$diagnosis, df$fit_valid, df$fit_gate_status,
                                                 df$importance_sampling_status)
  df$fit_gate_status <- dplyr::recode(as.character(df$fit_gate_status),
    ok = "passed", retried_ok = "passed after retry", not_interpretable = "failed",
    .default = "unavailable", .missing = "unavailable")
  df$outcome <- rh_label(df$outcome, labels)
  df$term <- rh_label(df$term, labels)
  block_order <- unique(as.character(df$block))
  df$block <- ifelse(df$block == "all_priors", "all priors (labelled all-priors run)", as.character(df$block))
  df <- df[order(match(as.character(ps$block), block_order)), , drop = FALSE]
  keep <- c("outcome", "block", "term", "term_type", "slope_sd", "prior_selection",
            "prior_sens", "lik_sens", "diagnosis", "fit_gate_status", "pareto_k_max", "pareto_k_threshold", "importance_sampling_status", "settings_status")
  rh_table(
    df[, keep, drop = FALSE],
    col_labels = c(
      outcome = "Outcome", block = "Prior block scaled", term = "Quantity", term_type = "Term type",
      slope_sd = "Slope prior SD", prior_selection = "Priors scaled",
      prior_sens = "Prior sensitivity", lik_sens = "Likelihood sensitivity",
      diagnosis = "Diagnosis", fit_gate_status = "Original fit validity", pareto_k_max = "Pareto k (max)", pareto_k_threshold = "Pareto k limit",
      importance_sampling_status = "Sensitivity check reliability", settings_status = "Settings status"
    ),
    digits = 3, engine = engine, target = target,
    source_note = paste(
      "One row per prior block and inspected quantity: each block scales only its own prior tag;",
      "the all-priors run scales every prior at once and is labelled. An error is recorded in the row",
      "of its block and never replaced by another block's result.",
      "Diagnoses are interpreted only when the original fit passed its validity gate and importance sampling is reliable.",
      "Sensitivity values from failed fits are retained as diagnostics.", file_note
    )
  )
}

#' Bayes R² table
#'
#' One row per facet: the model-based Bayesian R² with its credible interval in
#' one cell. Columns with one value in every row (the prior, the role, the
#' likelihood) leave the table; the caption or the prose names them.
#'
#' @param r2 Tibble from `ap6_bayes_r2()` bound over fits.
#' @param labels Named labels from [rh_labels()].
#' @param engine See [rh_use_gt()].
#' @param target Name(s) of the target the numbers come from; see [rh_source_note()].
#' @param analysis_plan Configuration; supplies the interval level
#'   (`analysis_plan$regression$ci_level`) for the column label.
#' @return Display table.
rh_r2_table <- function(r2, labels = rh_labels(), engine = "auto", analysis_plan, target = NULL) {
  input <- tibble::as_tibble(r2)
  ci <- rh_ci_label(rh_cfg_ci_level(analysis_plan))
  df <- tibble::tibble(
    outcome = rh_label(input$outcome, labels),
    slope_sd = rh_sweep_sd_label(input$slope_sd, analysis_plan),
    family = ifelse(input$family %in% "gaussian", "normal", ifelse(input$family %in% "student", "Student-t", input$family)),
    r2 = rh_fmt_est_ci(input$r2_median, input$r2_lo, input$r2_hi, bounded = TRUE)
  )
  df <- df[setdiff(names(df), names(rh_constant_columns(df, c("slope_sd", "family"))))]
  # The per-fit formula the target itself records (Gaussian and Student-t differ).
  definition <- unique(stats::na.omit(as.character(input$r2_definition)))
  note <- paste0("Posterior median [", ci, "] of the model-based Bayesian R²: ",
                 paste(definition, collapse = " | "), ".")
  rh_table(
    df,
    col_labels = c(outcome = "Outcome", slope_sd = "Slope prior SD", family = "Residual distribution",
                   r2 = paste0("Bayesian R² [", ci, "]"))[names(df)],
    engine = engine, target = target, source_note = note
  )
}

#' Posterior-predictive statistics table
#'
#' The band is the central posterior-predictive interval at
#' `analysis_plan$regression$ci_level`, the level `ap6_pp_stats()` reuses (no separate
#' band level is configured). The last column is the one-sided posterior
#' predictive p-value P(replicated ≥ observed) that `ap6_pp_stats()` computes
#' with the same definition for every statistic; the note states its
#' direction, because for `min` a small value means a lighter (more bounded)
#' lower tail, not a heavier one. The mean and the standard deviation of a
#' standardised outcome are reproduced by construction and leave the table;
#' an observed value outside its band is set in bold.
#'
#' @param pp_stats Tibble from `ap6_pp_stats()` bound over outcomes.
#' @param labels Named labels from [rh_labels()].
#' @param engine See [rh_use_gt()].
#' @param target Name(s) of the target the numbers come from; see [rh_source_note()].
#' @param analysis_plan Configuration (`analysis_plan$regression$ci_level` for the band label).
#' @return Display table.
rh_pp_stats_table <- function(pp_stats, labels = rh_labels(), engine = "auto", analysis_plan, target = NULL) {
  level <- rh_cfg_ci_level(analysis_plan)
  pp <- tibble::as_tibble(pp_stats)
  pp <- pp[!pp$stat %in% c("mean", "sd"), , drop = FALSE]
  words <- c(min = "minimum", max = "maximum", skew = "skewness", kurtosis = "excess kurtosis")
  outside <- !is.na(pp$observed) & !is.na(pp$rep_lo) & !is.na(pp$rep_hi) &
    (pp$observed < pp$rep_lo | pp$observed > pp$rep_hi)
  observed <- rh_fmt(pp$observed)
  df <- tibble::tibble(
    outcome = rh_blank_repeated(rh_label(pp$outcome, labels)),
    stat = ifelse(pp$stat %in% names(words), unname(words[pp$stat]), pp$stat),
    observed = ifelse(outside, paste0("**", observed, "**"), observed),
    rep_band = rh_fmt_ci(pp$rep_lo, pp$rep_hi),
    p_value = rh_fmt_p(pp$p_value)
  )
  if ("note" %in% names(pp) && any(!is.na(pp$note) & nzchar(pp$note))) df$note <- pp$note
  note <- paste0(
    "Range: the central ", rh_fmt(level, 0), "% interval of the statistic over the data sets simulated from the ",
    "fitted model; bold: the observed value lies outside it. P(replicated ≥ observed): share of the simulated ",
    "data sets whose statistic is at or above the observed value; for the minimum, a value near zero indicates a ",
    "lighter (more bounded) lower tail."
  )
  rh_table(
    df,
    col_labels = c(outcome = "Outcome", stat = "Statistic", observed = "Observed",
                   rep_band = paste0("Range of ", rh_fmt(level, 0), "% of the simulated data sets"),
                   p_value = "P(replicated ≥ observed)", note = "Unavailable comparison"),
    engine = engine, target = target, markdown = "observed",
    source_note = note
  )
}

#' Prior-predictive summary table
#'
#' One row per facet and model (the three priors of the normal model and the
#' Student-t model): the percentiles of the answers simulated from the priors
#' alone on the response scale, the 95th percentile of the absolute
#' standardised prediction, and the shares below and above the response range.
#' Columns with one value in every row (the number of draws and of
#' observations, the response range) leave the table for the sentence of
#' [rh_prior_pred_constant_text()].
#'
#' @param summary Tibble from `ap6_prior_predictive()$summary` bound over outcomes.
#' @param labels Named labels from [rh_labels()].
#' @param engine See [rh_use_gt()].
#' @param target Name(s) of the target the numbers come from; see [rh_source_note()].
#' @return Display table.
rh_prior_pred_table <- function(summary, labels = rh_labels(), engine = "auto", analysis_plan, target = NULL) {
  df <- tibble::as_tibble(summary)
  ord <- order(
    match(df$outcome, as.character(analysis_plan$regression$outcomes)),
    as.character(df$family) != "gaussian",
    as.numeric(df$slope_sd)
  )
  df <- df[ord, , drop = FALSE]
  prior_name <- rh_sweep_label(df$slope_sd, analysis_plan)
  model <- paste0(
    ifelse(is.na(prior_name), "", paste0(toupper(substr(prior_name, 1, 1)), substring(prior_name, 2), " prior")),
    " (SD ", rh_fmt(df$slope_sd, 2), ")",
    ifelse(df$family %in% "student", ", Student-t", "")
  )
  q <- grep("^q[0-9.]+$", names(df), value = TRUE)
  q <- q[order(as.numeric(sub("^q", "", q)))]
  out <- tibble::tibble(outcome = rh_blank_repeated(rh_label(df$outcome, labels)), model = model)
  heads <- c(outcome = "Outcome", model = "Model")
  # the sample columns stand in the table only where they differ between rows;
  # otherwise rh_prior_pred_constant_text() states them
  varies <- function(column) column %in% names(df) && length(unique(df[[column]])) > 1L
  if (varies("n_draws") || varies("n_obs")) {
    out$n_draws <- rh_fmt_n(df$n_draws)
    out$n_obs <- rh_fmt_n(df$n_obs)
    heads[c("n_draws", "n_obs")] <- c("Draws from the priors", "Respondents")
  }
  if (varies("range_min") || varies("range_max")) {
    out$range <- paste0(rh_fmt_n(df$range_min), "–", rh_fmt_n(df$range_max))
    heads["range"] <- "Response range"
  }
  for (col in q) {
    out[[col]] <- rh_fmt(df[[col]], 2)
    heads[col] <- paste0(sub("^q", "", col), "th percentile")
  }
  out$p95_abs <- rh_fmt(df$p95_abs_prediction, 2)
  heads["p95_abs"] <- "95th percentile of |z|"
  out$below <- rh_fmt_pct(df$share_below_min, 1, scale = "proportion")
  out$above <- rh_fmt_pct(df$share_above_max, 1, scale = "proportion")
  heads["below"] <- "Below the response range"
  heads["above"] <- "Above the response range"
  out$outside <- rh_fmt_pct(df$share_outside_range, 1, scale = "proportion")
  heads["outside"] <- "Outside the response range"
  if ("note" %in% names(df) && any(!is.na(df$note) & nzchar(df$note))) {
    out$note <- df$note
    heads["note"] <- "Unavailable check"
  }
  rh_table(
    out, col_labels = heads, engine = engine, target = target,
    source_note = paste(
      "Percentiles of the answers simulated from the priors alone, on the response scale: the standardised",
      "predictions back-transformed with the fixed mean and standard deviation of the facet in the model's sample",
      "(never standardised again). |z|: absolute standardised prediction. Shares: percentages of all simulated",
      "answers (draws × respondents) below the lowest or above the highest response option."
    )
  )
}

#' The constant columns of the prior-predictive table, as a sentence
#'
#' @param summary Tibble from `ap6_prior_predictive()$summary` bound over outcomes.
#' @return One sentence.
rh_prior_pred_constant_text <- function(summary) {
  df <- tibble::as_tibble(summary)
  one <- function(x) !is.null(x) && length(unique(x)) == 1L
  parts <- c(
    if (one(df$n_draws) && one(df$n_obs)) paste0(
      "each of the ", rh_fmt_n(nrow(df)), " rows summarises ", rh_fmt_n(df$n_draws[[1]]), " draws from the priors for the ",
      rh_fmt_n(df$n_obs[[1]]), " respondents of the model's sample"
    ),
    if (one(df$range_min) && one(df$range_max)) paste0(
      "the response options run from ", rh_fmt_n(df$range_min[[1]]), " to ", rh_fmt_n(df$range_max[[1]])
    )
  )
  if (length(parts) == 0L) return("")
  out <- paste(parts, collapse = "; ")
  paste0(toupper(substr(out, 1, 1)), substring(out, 2), ".")
}

#' Network Bayes factors on the report's display rule, at the resolution of the run
#'
#' A network Bayes factor is posterior over prior inclusion odds; it is
#' infinite exactly when the inclusion probability is one, i.e. when every
#' sampled network included the edge, and zero when none did. The report's
#' display rule ([rh_fmt_bf()]) shows values beyond 100 only as the power of
#' ten they exceed, and never a power of ten beyond what the estimator can
#' resolve: one sampled network without the edge, out of the `n_sweeps`
#' retained sampler steps of one fit, or out of `n_sweeps * n_resamples` for
#' the bagged network, which averages its resamples ([ap6_network_bf_bounds()]).
#' Where the resolution is unknown an infinite or zero value prints as a dash,
#' so a missing bound is visible rather than invented.
#'
#' @param bf Numeric vector of Bayes factors.
#' @param n_sweeps Retained sampler steps of one fit (integer or `NA`).
#' @param prior Prior edge-inclusion probability the Bayes factors were formed
#'   at.
#' @param n_resamples Number of fits averaged: 1 for a single fit, the number
#'   of successful resamples for the bagged network.
#' @return Character vector.
rh_network_bf <- function(bf, n_sweeps, prior, n_resamples = 1L, bounds = NULL) {
  if (is.null(bounds)) bounds <- calculate_network_bf_resolution(n_sweeps, prior, n_resamples)
  rh_fmt_bf(bf, upper = bounds$upper, lower = bounds$lower)
}

#' The sentence that explains the powers of ten of network Bayes factors
#'
#' Added to a table note only when a value beyond the whole numbers or below
#' two decimals is shown, so a table without one carries no sentence about it.
#'
#' @param bf Numeric vector(s) of the Bayes factors shown in the table.
#' @param n_sweeps Retained sampler steps of one fit.
#' @param prior Prior edge-inclusion probability.
#' @param n_resamples Number of fits averaged, as in [rh_network_bf()].
#' @return One sentence, or `""` when no such value is shown.
rh_network_bf_bound_note <- function(bf, n_sweeps, prior, n_resamples = 1L, bounds = NULL) {
  rule <- rh_bf_display_rule()
  bf <- as.numeric(unlist(bf))
  beyond <- !is.na(bf) & (bf > rule$rungs[[1]] | bf < 1 / rule$rungs[[1]])
  if (!any(beyond)) return("")
  if (is.null(bounds)) bounds <- calculate_network_bf_resolution(n_sweeps, prior, n_resamples)
  ladder <- paste0(
    "A Bayes factor above ", rh_fmt_n(rule$rungs[[1]]), " is shown as the power of ten it exceeds, ",
    "one below ", rh_fmt_bf(1 / rule$rungs[[1]]), " as the power of ten it falls below"
  )
  if (!is.finite(bounds$upper) || !is.finite(bounds$lower)) {
    return(paste0(
      ladder, "; the number of sampled networks is not recorded for this table, so an inclusion ",
      "probability of exactly one or zero is shown as a dash."
    ))
  }
  paste0(
    ladder, ", at most to the limit that ", rh_fmt_n(bounds$n_sweeps), " sampled networks can resolve",
    if (isTRUE(as.numeric(n_resamples) > 1)) paste0(
      " (", rh_fmt_n(n_resamples), " resamples of ", rh_fmt_n(n_sweeps), " sampler steps)"
    ) else "",
    "."
  )
}

#' A probability as a small exact fraction
#'
#' The two bagged decision thresholds are exact small fractions of the retained
#' posterior odds (10/11 and 1/11 at the preregistered edge prior of .50), and
#' the note states them that way beside the odds ratio.
#'
#' @param p Numeric scalar in \[0, 1\].
#' @param max_den Largest denominator tried.
#' @return `"10/11"`, or `NA_character_` when no such fraction exists.
rh_ratio_label <- function(p, max_den = 100L) {
  if (length(p) != 1L || !is.finite(p)) return(NA_character_)
  for (q in seq_len(max_den)) {
    n <- round(p * q)
    if (abs(n / q - p) < 1e-9) return(paste0(n, "/", q))
  }
  NA_character_
}

#' What the bagged edge weight and the bagged inclusion Bayes factor are
#'
#' The weight is a model-averaged partial correlation attenuated toward zero by
#' the inclusion probability, not a plain partial correlation. The decision
#' statistic is the preregistered bagged inclusion Bayes factor (BF_bagged):
#' the posterior odds of inclusion over the prior odds, computed from the
#' inclusion probability averaged over the resamples. The thresholds are
#' stated as inclusion probabilities as well, together with the prior odds and
#' the number of resamples defining the bag.
#'
#' @param analysis_plan Configuration from `zm_config()` (`analysis_plan$network`).
#' @param n_resamples Number of successful bootstrap fits the bag averaged
#'   over (`attr(bagged, "n_success_fits")`); `NULL` states the configured
#'   `analysis_plan$network$B` as the number of resamples without a success count.
#' @param n_attempted Number of bootstrap resamples attempted
#'   (`attr(bagged, "n_fits")`); `NULL` takes `analysis_plan$network$B`.
#' @return One paragraph of sentences.
rh_network_bagged_note <- function(analysis_plan, n_resamples = NULL, n_attempted = NULL) {
  net <- analysis_plan[["network"]]
  note <- paste0(
    "Weight: model-averaged partial correlation, including zero when an edge is absent. ",
    "BF_bagged: posterior inclusion odds divided by prior inclusion odds, computed from the ",
    "resample-averaged inclusion probability. Present at BF_bagged ≥ ", rh_fmt(net$bf_include, 0),
    "; absent at BF_bagged ≤ ", rh_fmt(net$bf_exclude, 2), "; inconclusive otherwise."
  )
  counts <- rh_network_resample_note(analysis_plan, n_resamples, n_attempted)
  paste(c(note, if (nzchar(counts)) counts), collapse = " ")
}

#' Explain a partial or unavailable bag using its actual fit counts
rh_network_resample_note <- function(analysis_plan = NULL, n_success = NULL,
                                     n_attempted = NULL, feasible = NULL) {
  if (is.null(n_attempted)) n_attempted <- analysis_plan$network$B
  known <- function(x) length(x) == 1L && !is.na(x) && is.finite(x)
  if (!known(n_success) || !known(n_attempted)) {
    return(if (identical(feasible, FALSE)) "Bagged estimates and classifications are unavailable." else "")
  }
  minimum <- analysis_plan$network$min_success_rate
  if (is.null(feasible)) {
    feasible <- n_success > 0L && n_attempted > 0L &&
      (!known(minimum) || n_success / n_attempted >= minimum)
  }
  if (!isTRUE(feasible)) {
    return(paste0(
      rh_fmt_n(n_success), " of ", rh_fmt_n(n_attempted),
      " bootstrap resample fits succeeded; bagged estimates and classifications are unavailable.",
      if (known(minimum)) paste0(" At least ", rh_fmt_pct(minimum, 0, scale = "proportion"),
                                " successful fits are required.") else ""
    ))
  }
  if (n_success == n_attempted) return("")
  paste0("Bagged quantities were averaged over the ", rh_fmt_n(n_success),
         " successful fits of the ", rh_fmt_n(n_attempted), " bootstrap resamples.")
}

#' Counts of edge decisions
#'
#' @param bagged The bagged edge table (`tabulate_bagged_network()`).
#' @return Named integer vector (present, inconclusive, absent).
rh_network_counts <- function(bagged) {
  lv <- c("present", "inconclusive", "absent")
  tab <- table(factor(bagged$decision, levels = lv))
  stats::setNames(as.integer(tab), lv)
}

#' Edge rows of one research-question subset of the RQ3 decisions
#'
#' @param network List from `tabulate_network_questions()`.
#' @param which `"conditional_structure"`, `"autonomy_orientation"` or `"autonomy_security_arousal"`.
#' @return Tibble of edge rows.
rh_network_subset_edges <- function(network, which = c("conditional_structure", "autonomy_orientation", "autonomy_security_arousal")) {
  which <- match.arg(which)
  network[[which]]
}

#' Edge label "A – B" from the two node columns
#'
#' @param node_i,node_j Character vectors of node keys.
#' @param labels Named labels from [rh_labels()].
#' @param inline `TRUE` for an edge named inside a sentence: both scales in
#'   their inline form ([rh_label_inline()]).
#' @return Character vector.
rh_network_edge_label <- function(node_i, node_j, labels = rh_labels(), inline = FALSE) {
  name <- if (inline) rh_label_inline else rh_label
  paste(name(node_i, labels), "–", name(node_j, labels))
}

#' The key of an undirected edge, the same in either node order
#'
#' @param node_i,node_j Character vectors of node keys.
#' @return Character vector.
rh_network_pair_key <- function(node_i, node_j) {
  paste(pmin(as.character(node_i), as.character(node_j)), pmax(as.character(node_i), as.character(node_j)))
}

#' RQ3: one block of network edges as a matrix of their classifications
#'
#' All 36 edges of the bagged network stand in three small matrices: the
#' motives by the facets (RQ3a), the motives among themselves (whose
#' autonomy-to-security/arousal cells answer RQ3b) and the facets among
#' themselves (whose ASC–SDO-D cells answer RQ3c). Each cell holds the edge's
#' classification, the signed edge weight (partial correlation) of a present
#' edge, and its bagged Bayes factor on the
#' report's display rule at the resolution of the run's resamples
#' ([rh_network_bf()]). A block of one node set shows each pair once, below
#' the diagonal. An infeasible bag classifies nothing and shows no number.
#'
#' @param bagged The report's bagged edge table (target `report_network`,
#'   element `bagged`) with its attributes `feasible`, `n_sweeps`,
#'   `n_success_fits` and `g_prior`.
#' @param rows,columns Node keys of the rows and the columns.
#' @param labels Named labels from [rh_labels()].
#' @param row_title Head of the row-label column.
#' @param lower_triangle Logical; `TRUE` for a block of one node set, whose
#'   cells are shown where the column comes before the row in `order`.
#' @param order Node keys in the order of a one-set block.
#' @param row_marks Named character vector: row key -> superscript mark.
#' @param cell_marks Pair keys ([rh_network_pair_key()]) whose cells carry
#'   `cell_mark`.
#' @param cell_mark Superscript mark of `cell_marks`.
#' @param cell_lines Optional `function(row, column)` returning a further line
#'   of the cell (`""` for none).
#' @param note The table note.
#' @param engine,target See [rh_table()].
#' @return Display table.
rh_network_matrix_table <- function(bagged, rows, columns, labels = rh_labels(), row_title = "",
                                    lower_triangle = FALSE, order = NULL,
                                    row_marks = character(), cell_marks = character(), cell_mark = "",
                                    cell_lines = NULL, note = NULL, engine = "auto", target = NULL) {
  feasible <- !identical(attr(bagged, "feasible"), FALSE)
  n_sweeps <- attr(bagged, "n_sweeps")
  n_resamples <- attr(bagged, "n_success_fits")
  prior <- attr(bagged, "g_prior")
  keys <- rh_network_pair_key(bagged$node_i, bagged$node_j)
  superscript <- function(mark) if (length(mark) == 1L && !is.na(mark) && nzchar(mark)) paste0("<sup>", mark, "</sup>") else ""
  cell <- function(row, column) {
    if (lower_triangle && !(match(column, order) < match(row, order))) return("")
    # an infeasible bag arrives as the typed empty edge table: its pairs are
    # shown as not classified rather than left blank
    i <- match(rh_network_pair_key(row, column), keys)
    decision <- if (feasible && !is.na(i)) as.character(bagged$decision[[i]]) else NA_character_
    first <- if (is.na(decision)) {
      "not classified"
    } else if (decision == "present") {
      paste0("**present, ", rh_fmt(bagged$weight_bagged[[i]], 2, bounded = TRUE), "**")
    } else {
      decision
    }
    if (rh_network_pair_key(row, column) %in% cell_marks) first <- paste0(first, superscript(cell_mark))
    bf <- if (feasible && !is.na(i)) rh_network_bf(bagged$bf_bagged[[i]], n_sweeps, prior, n_resamples, bounds = attr(bagged, "bf_bounds")) else "—"
    lines <- c(first, paste0("BF<sub>bagged</sub> ", bf), if (is.null(cell_lines)) "" else cell_lines(row, column))
    paste(lines[nzchar(lines)], collapse = "<br>")
  }
  df <- tibble::tibble(node = paste0(rh_label(rows, labels),
                                     vapply(rows, function(r) superscript(unname(row_marks[r])), "")))
  column_keys <- paste0("column_", seq_along(columns))
  for (k in seq_along(columns)) {
    df[[column_keys[k]]] <- vapply(rows, cell, "", column = columns[[k]], USE.NAMES = FALSE)
  }
  rh_table(
    df, col_labels = c(node = row_title, stats::setNames(rh_label(columns, labels), column_keys)),
    engine = engine, target = target, markdown = names(df), source_note = note
  )
}

#' How precise the large bagged Bayes factors of this run are
#'
#' The note of a network table explains, computed
#' from the resamples, how imprecise the Bayes factors above the whole numbers
#' are in this run. The standard error of a bagged inclusion probability is
#' the standard deviation of the per-resample inclusion probabilities divided
#' by the square root of the number of resamples; two standard errors on
#' either side, converted into Bayes factors, give the range the shown value
#' could lie in. The sentence names the edge whose range reaches furthest
#' below its shown power of ten. An edge in every sampled network of every
#' resample has no spread to measure; a second sentence says what its value
#' means.
#'
#' @param bagged The report's bagged edge table, with its attributes.
#' @param fits The per-resample edge rows (target `report_network`, element
#'   `fits`).
#' @param pairs Pair keys ([rh_network_pair_key()]) of the edges the table shows.
#' @param labels Named labels from [rh_labels()].
#' @return Up to two sentences, or `""`.
rh_network_bf_precision_note <- function(bagged, fits, pairs, labels = rh_labels(), precision = NULL) {
  rule <- rh_bf_display_rule()
  if (is.null(precision)) precision <- calculate_network_bf_precision(bagged, fits, pairs)
  if (!precision$available) return("")
  prior <- attr(bagged, "g_prior")
  n_sweeps <- attr(bagged, "n_sweeps")
  n_se <- precision$n_se
  n_resamples <- precision$n_resamples
  bounds <- precision$bounds
  shown <- precision$shown
  low <- precision$low
  high <- precision$high
  imprecise <- precision$imprecise
  sentences <- character()
  if (any(imprecise)) {
    j <- precision$worst
    sentences <- paste0(
      "With this run's ", rh_fmt_n(n_resamples), " resamples, Bayes factors above ", rh_fmt_n(rule$rungs[[1]]),
      " are imprecise: the value shown as ",
      rh_network_bf(shown$bf_bagged[[j]], n_sweeps, prior, n_resamples, bounds = bounds), " for ",
      rh_network_edge_label(shown$node_i[[j]], shown$node_j[[j]], labels, inline = TRUE),
      " could lie anywhere from about ", rh_fmt_n(signif(low[[j]], 2)),
      if (is.infinite(high[[j]])) " upward" else paste0(" to about ", rh_fmt_n(signif(high[[j]], 2))),
      " (", rh_fmt_count(n_se), " standard errors of its bagged inclusion probability, from the spread across ",
      "the resamples)."
    )
  }
  n_always <- precision$n_always
  if (n_always > 0L) {
    sentences <- c(sentences, paste0(
      if (n_always == 1L) "The edge shown as " else "The edges shown as ",
      rh_network_bf(Inf, n_sweeps, prior, n_resamples, bounds = bounds),
      if (n_always == 1L) " was" else " were",
      " in every network sampled in all ", rh_fmt_n(n_resamples), " resamples (", rh_fmt_n(bounds$n_sweeps),
      " in all), so ", if (n_always == 1L) "its Bayes factor lies" else "their Bayes factors lie",
      " beyond what this run can resolve."
    ))
  }
  paste(sentences, collapse = " ")
}

#' Comparison edges with display columns
#'
#' Shared preparation for the network-comparison displays: the `edges`
#' element of the network comparison (`tabulate_network_comparison()`) with an edge label and a status label
#' (`"Decision changed"`, `"Same decision"`, `"Not decided"` when either
#' network has no decision).
#'
#' @param compare The network comparison (`tabulate_network_comparison()`).
#' @param labels Named labels from [rh_labels()].
#' @return Tibble.
rh_network_compare_edges <- function(compare, labels = rh_labels()) {
  e <- tibble::as_tibble(compare$edges)
  e$edge <- rh_network_edge_label(e$node_i, e$node_j, labels)
  e$status <- ifelse(is.na(e$changed), "Not decided", ifelse(e$changed, "Decision changed", "Same decision"))
  e
}

#' Implied PIP thresholds of a comparison object
#'
#' @param compare The network comparison (`tabulate_network_comparison()`) (element `thresholds`).
#' @return One-row tibble `bf_include`, `bf_exclude`, `pip_include`, `pip_exclude`.
rh_network_thresholds <- function(compare) {
  needed <- c("bf_include", "bf_exclude", "pip_include", "pip_exclude")
  thr <- tibble::as_tibble(compare$thresholds)
  out <- thr[1, needed]
  # `g_prior` and `n_sweeps` fix the Bayes factors the chain can resolve; a
  # comparison built before they were recorded yields NA, and the displays then
  # name no bound rather than inventing one.
  out$g_prior <- if ("g_prior" %in% names(thr)) thr$g_prior[1] else NA_real_
  out$n_sweeps <- if ("n_sweeps" %in% names(thr)) thr$n_sweeps[1] else NA_integer_
  out
}

#' Full-data PIP against bagged PIP, one point per edge
#'
#' The 45° line marks agreement; dashed lines are the inclusion-probability
#' thresholds implied by the Bayes-factor thresholds of the decision rule
#' (read from `compare$thresholds`). Edges whose decision changes between the
#' two networks are labelled.
#'
#' @param compare The network comparison (`tabulate_network_comparison()`).
#' @param labels Named labels from [rh_labels()].
#' @return A ggplot.
rh_network_pip_scatter <- function(compare, labels = rh_labels()) {
  e <- rh_network_compare_edges(compare, labels)
  thr <- rh_network_thresholds(compare)
  status_levels <- c("Same decision", "Decision changed", "Not decided")
  e$status <- factor(e$status, levels = status_levels)
  changed <- e[!is.na(e$changed) & e$changed, , drop = FALSE]
  gg <- ggplot2::ggplot(e, ggplot2::aes(x = pip_full, y = pip_bagged)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, colour = "grey70") +
    ggplot2::geom_vline(xintercept = c(thr$pip_exclude, thr$pip_include), linetype = "dashed", colour = "grey50") +
    ggplot2::geom_hline(yintercept = c(thr$pip_exclude, thr$pip_include), linetype = "dashed", colour = "grey50") +
    ggplot2::geom_point(ggplot2::aes(colour = status, shape = status), size = 2.4)
  if (nrow(changed) > 0) {
    # labels of points on the right open to the left, so none is cut at the edge
    changed$label_hjust <- ifelse(changed$pip_full > 0.6, 1.08, -0.08)
    # neighbouring labels on one side alternate above and below their point
    changed$label_vjust <- -0.5
    for (side in unique(changed$label_hjust)) {
      on_side <- which(changed$label_hjust == side)
      ranked <- on_side[order(changed$pip_bagged[on_side])]
      changed$label_vjust[ranked] <- rep_len(c(1.5, -0.5), length(ranked))
    }
    gg <- gg + ggplot2::geom_text(
      data = changed, ggplot2::aes(label = edge, hjust = label_hjust, vjust = label_vjust), size = 2.6,
      colour = "#D55E00"
    )
  }
  gg +
    ggplot2::scale_colour_manual(
      values = c("Same decision" = "#0072B2", "Decision changed" = "#D55E00", "Not decided" = "grey60"),
      drop = TRUE, name = NULL
    ) +
    ggplot2::scale_shape_manual(
      values = c("Same decision" = 16, "Decision changed" = 17, "Not decided" = 1), drop = TRUE, name = NULL
    ) +
    # The thresholds the dashed lines mark are stated in the text before the
    # figure: a caption inside the plot was cut at the page edge.
    ggplot2::coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
    ggplot2::labs(
      x = "Inclusion probability, full-data network",
      y = "Inclusion probability, bagged network"
    ) +
    rh_theme() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_line())
}

#' Resample PIP distribution per edge (stability strips)
#'
#' One row per edge, ordered by the bagged PIP: the distribution of the
#' per-resample inclusion probabilities as a boxplot with jittered points,
#' the bagged PIP (mean) as a bar and the single full-data PIP as a diamond.
#' Dashed lines are the implied PIP thresholds. Failed resamples are omitted.
#'
#' @param per_fit_edges The per-resample edge rows (`tabulate_network_resample_fits()`).
#' @param compare The network comparison (`tabulate_network_comparison()`).
#' @param labels Named labels from [rh_labels()].
#' @return A ggplot.
rh_network_stability_strips <- function(per_fit_edges, compare, labels = rh_labels(), boxplot_data = NULL) {
  e <- rh_network_compare_edges(compare, labels)
  thr <- rh_network_thresholds(compare)
  pf <- tibble::as_tibble(per_fit_edges)
  pf <- pf[pf$status %in% c("ok", "retry_ok") & !is.na(pf$node_i) & !is.na(pf$node_j), , drop = FALSE]
  # orient the per-fit rows like the comparison edges (either direction may occur)
  key <- paste(e$node_i, e$node_j)
  fwd <- paste(pf$node_i, pf$node_j)
  rev <- paste(pf$node_j, pf$node_i)
  pf$key <- ifelse(fwd %in% key, fwd, ifelse(rev %in% key, rev, NA_character_))
  pf <- pf[!is.na(pf$key), , drop = FALSE]
  pf$edge <- e$edge[match(pf$key, key)]

  ord <- order(e$pip_bagged, e$pip_full, na.last = FALSE)
  edge_levels <- e$edge[ord]
  e$edge <- factor(e$edge, levels = edge_levels)
  pf$edge <- factor(pf$edge, levels = edge_levels)

  gg <- ggplot2::ggplot() +
    ggplot2::geom_vline(xintercept = c(thr$pip_exclude, thr$pip_include), linetype = "dashed", colour = "grey50")
  if (nrow(pf) > 0) {
    boxes <- if (is.null(boxplot_data)) {
      ggplot2::geom_boxplot(
        data = pf, ggplot2::aes(x = pip, y = edge), outlier.shape = NA,
        fill = "grey95", colour = "grey60", width = 0.6
      )
    } else {
      rows <- boxplot_data
      rows$edge <- factor(e$edge[match(rows$key, rh_network_pair_key(e$node_i, e$node_j))], levels = edge_levels)
      ggplot2::geom_boxplot(data = rows,
        ggplot2::aes(xmin = xmin, xlower = xlower, xmiddle = xmiddle, xupper = xupper, xmax = xmax, y = edge),
        stat = "identity", orientation = "y", outlier.shape = NA,
        fill = "grey95", colour = "grey60", width = 0.6)
    }
    gg <- gg + boxes +
      ggplot2::geom_jitter(
        data = pf, ggplot2::aes(x = pip, y = edge), height = 0.15, width = 0,
        alpha = 0.25, size = 0.6, colour = "#0072B2"
      )
  }
  gg +
    ggplot2::geom_point(
      data = e, ggplot2::aes(x = pip_bagged, y = edge, shape = "Bagged (mean over resamples)"),
      size = 3, colour = "#0072B2"
    ) +
    ggplot2::geom_point(
      data = e, ggplot2::aes(x = pip_full, y = edge, shape = "Single fit on all rows"),
      size = 2.6, colour = "#D55E00"
    ) +
    ggplot2::scale_shape_manual(
      values = c("Bagged (mean over resamples)" = 124, "Single fit on all rows" = 18), name = NULL) +
    ggplot2::scale_x_continuous(limits = c(0, 1)) +
    ggplot2::labs(x = "Posterior inclusion probability per resample", y = NULL) +
    rh_theme() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_line(colour = "grey92"))
}

#' Edges whose decision differs between the full-data fit and the bag
#'
#' Ordered as in `compare$changed_edges` (largest PIP difference first),
#' with both PIPs, both Bayes factors, the per-resample shares above the
#' include / below the exclude threshold and the 5–95 % range of the resample
#' PIPs. A one-row placeholder is shown when no edge changes.
#'
#' @param compare The network comparison (`tabulate_network_comparison()`).
#' @param labels Named labels from [rh_labels()].
#' @param engine See [rh_use_gt()].
#' @param target Name(s) of the target the numbers come from; see [rh_source_note()].
#' @return Display table.
rh_network_changed_edges_table <- function(compare, labels = rh_labels(), engine = "auto", target = NULL) {
  ce <- tibble::as_tibble(compare$changed_edges)
  thr <- rh_network_thresholds(compare)
  # The bagged network averages its successful resamples, so its Bayes factors
  # resolve finer than one fit's (rh_network_bf()).
  n_resamples <- compare$n_success_fits
  # The heads name the two networks in the report's words: the full-data
  # network, fitted once to the whole sample, and the bagged network.
  col_labels <- c(
    edge = "Edge", transition = "Decision: full-data → bagged",
    pip_full = "Inclusion probability, full-data network", bf_full = "Bayes factor, full-data network",
    pip_bagged = "Inclusion probability, bagged network",
    bf_bagged = "BF_bagged",
    share_above_include = paste0("Share of resamples with Bayes factor ≥ ", rh_fmt_bf(thr$bf_include)),
    share_below_exclude = paste0("Share of resamples with Bayes factor ≤ ", rh_fmt_bf(thr$bf_exclude)),
    resample_range = "Middle 90% of the resamples' inclusion probabilities"
  )
  if (nrow(ce) == 0) {
    df <- tibble::tibble(
      edge = if (identical(compare$feasible, FALSE)) "Bagged classifications unavailable" else
        "No edge changes its decision",
      transition = NA_character_, pip_full = NA_character_,
      bf_full = NA_character_, pip_bagged = NA_character_, bf_bagged = NA_character_,
      share_above_include = NA_character_, share_below_exclude = NA_character_, resample_range = NA_character_
    )
  } else {
    df <- tibble::tibble(
      edge = rh_network_edge_label(ce$node_i, ce$node_j, labels),
      transition = gsub(" -> ", " → ", ce$transition, fixed = TRUE),
      pip_full = rh_fmt(ce$pip_full, 3, bounded = TRUE),
      bf_full = rh_network_bf(ce$bf_full, thr$n_sweeps, thr$g_prior, bounds = attr(compare, "bf_bounds_full")),
      pip_bagged = rh_fmt(ce$pip_bagged, 3, bounded = TRUE),
      bf_bagged = rh_network_bf(ce$bf_bagged, thr$n_sweeps, thr$g_prior, n_resamples, bounds = attr(compare, "bf_bounds_bagged")),
      share_above_include = rh_fmt(ce$share_above_include, 2, bounded = TRUE),
      share_below_exclude = rh_fmt(ce$share_below_exclude, 2, bounded = TRUE),
      resample_range = rh_fmt_ci(ce$pip_resample_q05, ce$pip_resample_q95, 3, bounded = TRUE)
    )
  }
  rh_table(
    df,
    col_labels = col_labels,
    engine = engine, target = target,
    source_note = paste0(
      "Full-data network: the network fitted once to the whole sample. BF_bagged: bagged Bayes factor. ",
      "Shares: the share of the successful resamples whose own inclusion probability reaches ",
      rh_fmt(thr$pip_include, 3, bounded = TRUE), " (Bayes factor ", rh_fmt_bf(thr$bf_include),
      ", present) or falls to ", rh_fmt(thr$pip_exclude, 3, bounded = TRUE), " (Bayes factor ",
      rh_fmt_bf(thr$bf_exclude), ", absent).",
      local({
        notes <- c(rh_network_bf_bound_note(ce$bf_full, thr$n_sweeps, thr$g_prior, bounds = attr(compare, "bf_bounds_full")),
                   rh_network_bf_bound_note(ce$bf_bagged, thr$n_sweeps, thr$g_prior, n_resamples, bounds = attr(compare, "bf_bounds_bagged")))
        notes <- unique(notes[nzchar(notes)])
        if (length(notes)) paste0(" ", paste(notes, collapse = " ")) else ""
      })
    )
  )
}

#' Display label of a prior edge-inclusion probability
#'
#' A probability, so without the leading zero (".25", ".50", ".75").
#'
#' @param p Numeric vector of prior edge-inclusion probabilities.
#' @return Character vector.
rh_network_prior_label <- function(p) {
  rh_fmt(p, 2, bounded = TRUE)
}

#' The cells the AP3 fill wrote, one row each
#'
#' Renders the table of [build_filled_cells_table()]: one row per empty
#' cell the fill wrote, with the model it came from, the value written and the
#' posterior predictive interval reported beside it. The interval level is the
#' configured one (`analysis_plan$missing_data$fill$interval`); the table computes
#' nothing, it displays what the fill recorded. An item cell shows its item code
#' and the item model of its scale; a demographic cell shows the variable's name
#' and the regression that filled it.
#'
#' A run without a gap fills no cell. The table then carries one row saying so,
#' in the manner of [rh_deviations_table()]: an empty record is a statement
#' about this run, not an omission.
#'
#' @param filled_cells Tibble from the `filled_cells` target
#'   (`respondent_id`, `variable`, `kind`, `model`, `value`, `lower`, `upper`).
#' @param analysis_plan Configuration from `zm_config()` (the interval level of the note).
#' @param labels Named labels from [rh_labels()].
#' @param engine See [rh_use_gt()].
#' @param target Name(s) of the target the numbers come from; see [rh_source_note()].
#' @return Display table.
rh_filled_cells_table <- function(filled_cells, analysis_plan, labels = rh_labels(), engine = "auto", target = NULL) {
  cells <- tibble::as_tibble(filled_cells)
  level <- rh_fmt_pct(analysis_plan$missing_data$fill$interval, 0, scale = "proportion")
  demographic_names <- c(demo_age = "Age", demo_hh_members = "Household members",
                         demo_income_hh_net = "Household income band")
  demographic_models <- c(demo_age = "Age regression", demo_hh_members = "Household-size regression",
                          demo_income_hh_net = "Income-band regression")
  name_or_key <- function(map, x) ifelse(x %in% names(map), unname(map[x]), x)
  note <- paste0(
    "One row per filled cell. For an item the filled value is the ",
    "posterior median of the expected answer, unrounded; for age the posterior median of the expected value; ",
    "for household size that median rounded to whole persons within 1–8, exact halves to the nearest even ",
    "integer; for income the median posterior predictive band. The interval is the ", level,
    " posterior predictive interval of the cell (AP3)."
  )
  if (nrow(cells) == 0L) {
    df <- tibble::tibble(
      respondent = "—", variable = "No cell was filled.", model = "—", value = "—"
    )
  } else {
    variable <- as.character(cells$variable)
    model <- as.character(cells$model)
    is_item <- as.character(cells$kind) == "item"
    # Decimals by kind: an item value is a fractional
    # expected answer whose predictive interval runs over whole answers, age
    # takes one decimal as in the sample table, and an income band is a
    # category code.
    value_digits <- ifelse(is_item, 2L, ifelse(variable %in% "demo_age", 1L, 0L))
    interval_digits <- ifelse(is_item, 0L, value_digits)
    value <- as.numeric(cells$value)
    lower <- as.numeric(cells$lower)
    upper <- as.numeric(cells$upper)
    df <- tibble::tibble(
      respondent = as.character(cells$respondent_id),
      variable = ifelse(is_item, variable, name_or_key(demographic_names, variable)),
      model = ifelse(is_item, paste(rh_label(model, labels), "item model"),
                     name_or_key(demographic_models, model)),
      value = vapply(seq_along(value), function(i) {
        paste(rh_fmt(value[[i]], value_digits[[i]]), rh_fmt_ci(lower[[i]], upper[[i]], interval_digits[[i]]))
      }, character(1))
    )
  }
  rh_table(
    df,
    col_labels = c(respondent = "Respondent", variable = "Variable", model = "Filled by",
                   value = paste0("Filled value [", level, " predictive interval]")),
    engine = engine, target = target, source_note = note
  )
}

#' Deviations from the preregistration (AP11, Deviations Policy)
#'
#' The register the preregistration promises ("Any departure from the
#' preregistration is documented in a deviations section of the final
#' write-up"). It exists before the first deviation rather than after it, and
#' renders "None to date" while the target has no row.
#'
#' @param deviations Zero-row or filled tibble from `zm_deviations_table()`
#'   (target `deviations_register`).
#' @param engine See [rh_use_gt()].
#' @param target Name(s) of the target the numbers come from; see [rh_source_note()].
#' @return Display table.
rh_deviations_table <- function(deviations, engine = "auto", target = NULL) {
  d <- tibble::as_tibble(deviations)
  if (nrow(d) == 0L) {
    d <- tibble::tibble(
      date = "—", preregistered = "—",
      deviation = "None to date", reason = "—"
    )
  }
  rh_table(
    d,
    col_labels = c(date = "Date", preregistered = "Preregistered", deviation = "Deviation",
                   reason = "Reason"),
    engine = engine, target = target
  )
}

#' The gender coding the regressions used, in one sentence
#'
#' The levels of the gender factor the regressions read, with their counts, and
#' the reference group (the first level), the largest group
#' ([set_gender_reference()]). It stands in the Methods list of what this
#' run settles.
#'
#' @param gender Gender column of the model data (factor).
#' @return One sentence.
rh_gender_coding_text <- function(gender = NULL, counts = NULL) {
  if (is.null(counts)) {
    g <- as.factor(gender)
    counts <- stats::setNames(as.integer(table(g)[levels(g)]), levels(g))
  }
  parts <- paste0(names(counts), " (", rh_fmt_n(counts), ifelse(counts == 1L, " respondent", " respondents"), ")")
  joined <- if (length(parts) <= 1L) parts else {
    paste0(paste(parts[-length(parts)], collapse = ", "), " and ", parts[length(parts)])
  }
  paste0(
    "Gender entered every regression of RQ1 and RQ2 as a factor with the levels ", joined,
    "; the reference group is ", names(counts)[1], ", the largest group (AP3, AP6)."
  )
}

#' The software paragraph of the Methods
#'
#' JARS Tables 1 and 7 ask the report to name the software and its version. The
#' versions of the three packages that produced the fits are the ones recorded
#' with the fits; the others are read from the active library, or passed in
#' where a result records its own (`recorded`).
#'
#' @param fitted_versions Named character vector `brms`, `cmdstanr`, `CmdStan`
#'   (target `supplement_software`); `NA` where no version is recorded.
#' @param packages Further packages to name, in the order they appear.
#' @param recorded Named character vector of package versions recorded with a
#'   result, named last.
#' @return One paragraph.
rh_software_text <- function(fitted_versions = c(brms = NA_character_, cmdstanr = NA_character_,
                                                 CmdStan = NA_character_),
                             packages = c("lavaan", "psych", "easybgm", "BDgraph", "priorsense"),
                             recorded = character(0)) {
  named <- function(label, version) {
    paste0(label, " ", if (is.na(version)) "(version not recorded)" else version)
  }
  join <- function(x) {
    if (length(x) <= 1L) return(paste(x, collapse = ""))
    paste0(paste(x[-length(x)], collapse = ", "), " and ", x[length(x)])
  }
  installed <- vapply(packages, function(p) {
    v <- tryCatch(as.character(utils::packageVersion(p)), error = function(e) NA_character_)
    named(p, v)
  }, character(1))
  others <- c(unname(installed), unname(mapply(named, names(recorded), recorded)))
  paste0(
    "All analyses ran in R ", R.version$major, ".", R.version$minor, ". ",
    "The regressions were fitted with ", named("brms", fitted_versions[["brms"]]), " through ",
    named("cmdstanr", fitted_versions[["cmdstanr"]]), " and ", named("CmdStan", fitted_versions[["CmdStan"]]),
    if (length(others)) paste0("; the other analyses used ", join(others)) else "",
    "."
  )
}

#' The provenance paragraph of the Methods
#'
#' Names the commit of the code that produced the report, the state of the
#' working tree, the fingerprints of the lockfile, the analysis plan and the
#' input data file, and the run (profile, data source, build time, R version).
#'
#' @param provenance The `report_provenance` target
#'   ([assemble_report_provenance()]).
#' @param digits Characters of each SHA-256 fingerprint to print.
#' @return One paragraph.
rh_provenance_text <- function(provenance, digits = 12L) {
  head <- provenance$git_head
  commit <- if (is.na(head) || !nzchar(head)) {
    "at a commit that was not recorded"
  } else {
    paste0("at commit ", substr(head, 1L, 7L), " (", head, ")")
  }
  changed <- if (is.na(provenance$git_dirty)) {
    ""
  } else if (isTRUE(provenance$git_dirty)) {
    ", with changes to the analysis code that are not in that commit,"
  } else {
    ", unchanged relative to that commit,"
  }
  source <- switch(provenance$data_source,
                   real = "the empirical survey data",
                   synthetic = "the synthetic preregistration data",
                   paste0("the data source '", provenance$data_source, "'"))
  files <- provenance$files
  named <- paste0("the ", files$role, " `", files$path, "`")
  named <- paste0(paste(named[-length(named)], collapse = ", "), " and ", named[length(named)])
  named <- paste0(toupper(substr(named, 1L, 1L)), substr(named, 2L, nchar(named)))
  prints <- ifelse(is.na(files$sha256), "not recorded", substr(files$sha256, 1L, digits))
  prints <- paste0(paste(prints[-length(prints)], collapse = ", "), " and ", prints[length(prints)])
  paste0(
    "The results were produced by the analysis code ", commit, changed, " on ", source, ". ",
    named, " are identified by the first ", tolower(english_number(digits)),
    " characters of their SHA-256 fingerprints, ", prints, "."
  )
}

english_number <- function(n) {
  words <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten",
             "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen")
  if (n >= 1 && n <= length(words)) words[n] else as.character(n)
}

#' The estimation paragraph of the conditional dependencies
#'
#' Reports the network's sampler settings, its bag and its resample count.
#'
#' @param analysis_plan Configuration from `zm_config()`.
#' @param n_sweeps Retained sweeps per fit, read off a chain; `NA` omits the
#'   clause rather than inventing it.
#' @param n_success Number of resamples whose fit succeeded, or `NULL`.
#' @return One paragraph.
rh_network_estimation_text <- function(analysis_plan, n_sweeps = NA, n_success = NULL) {
  net <- analysis_plan$network
  sweeps <- if (is.na(n_sweeps)) {
    " The number of retained sweeps is read off each chain rather than assumed from the iteration count."
  } else {
    paste0(" Of those, ", rh_fmt_n(n_sweeps), " sweeps were retained after burn-in and carry the ",
           "inclusion probabilities.")
  }
  success <- if (is.null(n_success)) "" else paste0(
    " ", rh_fmt_n(n_success), " of the ", rh_fmt_n(net$B), " resamples yielded a usable fit; the ",
    "network is treated as computationally feasible only above a success rate of ",
    rh_fmt_pct(net$min_success_rate, 0, scale = "proportion"), "."
  )
  paste0(
    "The graphical model was estimated by reversible-jump MCMC over graphs with ",
    rh_fmt_n(net$iter), " iterations per fit.", sweeps,
    " The reported quantities are bagged: the network sample was resampled ", rh_fmt_n(net$B),
    " times with replacement, each resample received its own fit, and the edge quantities are averaged ",
    "over the successful fits.", success
  )
}

#' Read-only comparison of the cached calculations with the current pipeline
#'
#' Runs dependency checking in a separate R process under the cached run's
#' profile and data-source settings. It does not build targets or refit models.
#' The time limit also bounds startup failures in restricted environments.
#'
#' @param root,store Analysis root and targets store, absolute paths.
#' @param analysis_plan,data_source Cached configuration and data-source label.
#' @param names Targets whose upstream ancestors are checked, or `NULL` for the
#'   whole pipeline. The report passes its own target: what it reads is its
#'   ancestors, and a sibling that a running build has not reached yet is no
#'   evidence that the numbers of the report are stale.
#' @return Character vector of outdated target names.
rh_outdated_targets <- function(root, store, analysis_plan, data_source, names = NULL) {
  if (!file.exists(file.path(root, "_targets.R")) || !dir.exists(store)) {
    stop("The analysis script or cached results store is unavailable.")
  }
  if (!analysis_plan$profile_name %in% c("smoke", "full") || !data_source %in% c("synthetic", "real")) {
    stop("The cached run profile or data source could not be identified.")
  }
  callr::r(
    function(root, store, profile, source, selected) {
      setwd(root)
      Sys.setenv(ZM_PROFILE = profile, ZM_DATA = source, ZM_PROJECT_ROOT = root)
      script <- file.path(root, "_targets.R")
      # The pipeline script is evaluated in a fresh environment: the arguments
      # of this function would otherwise be globals of the pipeline, and a
      # character `names` there outdates every target that calls names().
      pipeline_env <- new.env(parent = globalenv())
      if (is.null(selected)) {
        targets::tar_outdated(script = script, store = store, callr_function = NULL, reporter = "silent",
                              envir = pipeline_env)
      } else {
        targets::tar_outdated(names = tidyselect::all_of(selected), script = script, store = store,
                              callr_function = NULL, reporter = "silent", envir = pipeline_env)
      }
    },
    args = list(root = root, store = store, profile = analysis_plan$profile_name, source = data_source,
                selected = names),
    # Use the already active project library without running startup profiles
    # (and therefore without invoking renv activation or bootstrap actions).
    libpath = .libPaths(), user_profile = FALSE, system_profile = FALSE, timeout = 30
  )
}

#' Freshness status of stored calculations, without running the pipeline
#'
#' Report rendering targets are tracked separately from calculation targets.
#' Simulation summaries are tracked calculation inputs. A failed check is unknown,
#' never evidence that the cached results are current.
#'
#' @param root Analysis root.
#' @param store Cached targets store.
#' @param analysis_plan,data_source Cached configuration and data-source label.
#' @param checker Read-only dependency checker; injectable for focused tests.
#' @param names Passed to the checker ([rh_outdated_targets()]) when given.
#' @return List with state (outdated/current/unavailable), target names and reason.
rh_report_freshness <- function(root, store, analysis_plan, data_source, checker = rh_outdated_targets,
                                names = NULL) {
  root <- normalizePath(root, mustWork = TRUE)
  store <- normalizePath(store, mustWork = FALSE)
  checked_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  out <- tryCatch(
    if (is.null(names)) checker(root, store, analysis_plan, data_source) else
      checker(root, store, analysis_plan, data_source, names = names),
    error = function(e) e
  )
  if (inherits(out, "error") || !is.character(out) || anyNA(out)) {
    return(list(
      state = "unavailable", outdated = character(), other_outdated = character(), checked_at = checked_at,
      reason = if (inherits(out, "error")) conditionMessage(out) else "The checker returned an unusable result."
    ))
  }
  other <- grepl("^results_draft(_|$)", out)
  outdated <- unique(out[!other])
  list(
    state = if (length(outdated)) "outdated" else "current",
    outdated = outdated, other_outdated = unique(out[other]), checked_at = checked_at, reason = NULL
  )
}

#' Whether the stored results were current when the report was rendered, in words
#'
#' The sentence of the Methods list of what this run settles; it names no
#' target.
#'
#' @param freshness Output of [rh_report_freshness()].
#' @return Two sentences.
rh_freshness_text <- function(freshness) {
  n <- length(freshness$outdated)
  status <- switch(
    freshness$state,
    outdated = paste0(
      "When this report was rendered, ", rh_fmt_count(n), if (n == 1L) " stored result" else " stored results",
      " it reads ", if (n == 1L) "was" else "were", " out of date: the code or the input behind ",
      if (n == 1L) "it" else "them", " had changed since ", if (n == 1L) "it was" else "they were", " computed."
    ),
    current = paste0(
      "When this report was rendered, every stored result it reads was current: no code or input behind ",
      "them had changed since they were computed."
    ),
    paste0(
      "Whether the stored results were current could not be checked when this report was rendered, so they ",
      "must be treated as unchecked."
    )
  )
  paste0(status, " Rendering does not recompute results.")
}

#' Sampling diagnostics of every fitted regression, in one slim table
#'
#' One row per fit: the twelve normal fits of RQ1 (four facets under three
#' priors), the refits allowing heavier tails, and the joint model of RQ2. The
#' table keeps the diagnostics whose values differ between fits
#' (the minimum bulk and tail effective sample sizes, the largest R̂, the
#' smallest BFMI) and every other column only when some fit departs from the
#' rest; [rh_diagnostics_constant_text()] states the constant ones.
#'
#' @param coefs Tibble of `ap6_fit_diagnostics()` rows of the RQ1 fits (tagged
#'   with outcome, slope_sd, role, family, n_obs, iter_used, gate_status).
#' @param analysis_plan Configuration (`analysis_plan$regression$ess_target`,
#'   `analysis_plan$regression$validity_gate`, the outcomes for the row order).
#' @param labels Named labels from [rh_labels()].
#' @param additional Rows of [collect_saved_additional_diagnostics()] (the
#'   joint model and the refits allowing heavier tails), or `NULL`.
#' @param engine See [rh_use_gt()].
#' @param target Name(s) of the target the numbers come from; see [rh_source_note()].
#' @return Display table with one row per fit.
rh_diagnostics_table <- function(coefs, analysis_plan, labels = rh_labels(), additional = NULL, engine = "auto",
                                 target = NULL) {
  rows <- rh_diagnostics_rows(coefs, analysis_plan, labels, additional)
  gate <- analysis_plan[["regression"]][["validity_gate"]]
  ess_target <- as.numeric(analysis_plan$regression$ess_target)
  df <- tibble::tibble(
    model = rows$model,
    n_obs = rh_fmt_n(rows$n_obs),
    iter_used = rh_fmt_n(rows$iter_used),
    ess_bulk_min = rh_fmt_n(rows$ess_bulk_min),
    ess_tail_min = rh_fmt_n(rows$ess_tail_min),
    rhat_max = rh_fmt(rows$rhat_max, 3),
    # BFMI can exceed one, so it keeps its leading zero (APA).
    bfmi_min = rh_fmt(rows$bfmi_min, 2),
    n_divergent = rh_fmt_n(rows$n_divergent),
    n_treedepth_hits = rh_fmt_n(rows$n_treedepth_hits),
    gate = rows$gate
  )
  constant <- names(rh_constant_columns(df, c("n_divergent", "n_treedepth_hits", "gate")))
  # Sample size and iterations are recorded for the fits of RQ1 only; where
  # the recorded values agree, the sentence states them.
  for (nm in c("n_obs", "iter_used")) {
    known <- rows[[nm]][!is.na(rows[[nm]])]
    if (length(unique(known)) <= 1L) constant <- c(constant, nm)
  }
  df <- df[setdiff(names(df), constant)]
  rh_table(
    df,
    col_labels = c(model = "Fit", n_obs = "n", iter_used = "Post-warm-up iterations per chain",
                   ess_bulk_min = "Smallest bulk ESS", ess_tail_min = "Smallest tail ESS",
                   rhat_max = "Largest R̂", bfmi_min = "Smallest BFMI", n_divergent = "Divergent transitions",
                   n_treedepth_hits = "Transitions at the maximum tree depth", gate = "Validity gate")[names(df)],
    engine = engine, target = target,
    source_note = paste0(
      "Over the population-level coefficients of each fit: ESS, effective sample size, the number of independent ",
      "draws the chains are worth, in the bulk and in the tails of the posterior; R̂, the agreement between the ",
      "chains, complete at one; BFMI, the energy diagnostic of the sampler, smallest over the chains. ",
      "The validity gate of this run requires an ESS of at least ", rh_fmt_n(ess_target),
      ", R̂ of at most ", rh_fmt(as.numeric(gate[["rhat_max"]]), 3),
      ", no divergent transition",
      if (!is.null(gate[["treedepth_hits_max"]])) {
        if (as.numeric(gate[["treedepth_hits_max"]]) == 0) ", no transition at the maximum tree depth" else
          paste0(", at most ", rh_fmt_n(gate[["treedepth_hits_max"]]), " transitions at the maximum tree depth")
      } else "",
      if (!is.null(gate[["bfmi_min"]])) paste0(" and a BFMI of at least ", rh_fmt(gate[["bfmi_min"]], 2),
                                              " in every chain") else "",
      ".",
      if (!is.null(additional) && "kind" %in% names(additional) && any(additional$kind %in% "motives")) {
        " In the joint model of the five motives, the residual correlations, from which the partial correlations come, are included."
      } else ""
    )
  )
}

#' The fits of the diagnostics table in one shape
#'
#' @inheritParams rh_diagnostics_table
#' @return Tibble `model`, `n_obs`, `iter_used`, `ess_bulk_min`,
#'   `ess_tail_min`, `rhat_max`, `bfmi_min`, `n_divergent`,
#'   `n_treedepth_hits`, `gate`, `passed`.
rh_diagnostics_rows <- function(coefs, analysis_plan, labels = rh_labels(), additional = NULL) {
  b <- tibble::as_tibble(coefs)
  if (!"ess_bulk_min" %in% names(b)) {
    stop("rh_diagnostics_table() needs the fit-level diagnostics of ap6_fit_diagnostics() ",
         "(column `ess_bulk_min`); the gate minima are not recomputed here.")
  }
  b <- b[order(match(as.character(b$outcome), as.character(analysis_plan$regression$outcomes)),
               as.numeric(b$slope_sd), as.character(b$family)), , drop = FALSE]
  prior <- rh_sweep_label(b$slope_sd, analysis_plan)
  gate_label <- function(status, passed) {
    ifelse(passed %in% TRUE & status %in% c("ok", "retried_ok"),
           ifelse(status %in% "retried_ok", "passed after one retry", "passed"),
           ifelse(is.na(passed), "not recorded", "not passed"))
  }
  main <- tibble::tibble(
    model = paste0(rh_label(b$outcome, labels), ", ",
                   ifelse(is.na(prior), "", paste0(prior, " prior ")), "(SD ", rh_fmt(b$slope_sd, 2), ")",
                   ifelse(as.character(b$family) %in% "student", ", Student-t", "")),
    n_obs = as.numeric(b$n_obs), iter_used = as.numeric(b$iter_used),
    ess_bulk_min = as.numeric(b$ess_bulk_min), ess_tail_min = as.numeric(b$ess_tail_min),
    rhat_max = as.numeric(b$rhat_max), bfmi_min = as.numeric(b$bfmi_min),
    n_divergent = as.numeric(b$n_divergent), n_treedepth_hits = as.numeric(b$n_treedepth_hits),
    gate = gate_label(as.character(b$gate_status), as.logical(b$ok)),
    passed = as.logical(b$ok) %in% TRUE & as.character(b$gate_status) %in% c("ok", "retried_ok")
  )
  if (is.null(additional) || nrow(additional) == 0L) return(main)
  a <- tibble::as_tibble(additional)
  kind <- if ("kind" %in% names(a)) as.character(a$kind) else ifelse(is.na(a$outcome), "joint", "student_refit")
  extra <- tibble::tibble(
    model = ifelse(kind %in% "joint", "Joint regression of the four facets (RQ2)",
                   ifelse(kind %in% "motives", "Joint model of the five motives (RQ1)",
                          paste0(rh_label(a$outcome, labels), ", refit allowing heavier tails (Student-t)"))),
    n_obs = if ("n_obs" %in% names(a)) as.numeric(a$n_obs) else NA_real_,
    iter_used = if ("iter_used" %in% names(a)) as.numeric(a$iter_used) else NA_real_,
    ess_bulk_min = as.numeric(a$ess_bulk_min), ess_tail_min = as.numeric(a$ess_tail_min),
    rhat_max = as.numeric(a$rhat_max), bfmi_min = as.numeric(a$bfmi_min),
    n_divergent = as.numeric(a$n_divergent), n_treedepth_hits = as.numeric(a$n_treedepth_hits),
    gate = gate_label(as.character(a$gate_status), as.logical(a$gate_passed)),
    passed = as.logical(a$gate_passed) %in% TRUE & as.character(a$gate_status) %in% c("ok", "retried_ok")
  )
  # the refits after the fits of RQ1, then the joint model of the five
  # motives, the joint model of RQ2 last
  dplyr::bind_rows(main, extra[kind %in% "student_refit", , drop = FALSE],
                   extra[kind %in% "motives", , drop = FALSE], extra[kind %in% "joint", , drop = FALSE])
}

#' The diagnostics every fit shares, as a sentence
#'
#' @inheritParams rh_diagnostics_table
#' @return One sentence.
rh_diagnostics_constant_text <- function(coefs, analysis_plan, labels = rh_labels(), additional = NULL) {
  rows <- rh_diagnostics_rows(coefs, analysis_plan, labels, additional)
  k <- nrow(rows)
  same <- function(x) length(unique(x[!is.na(x)])) == 1L
  known_n <- rows$n_obs[!is.na(rows$n_obs)]
  known_iter <- rows$iter_used[!is.na(rows$iter_used)]
  parts <- c(
    if (all(rows$n_divergent %in% 0) && all(rows$n_treedepth_hits %in% 0))
      "had no divergent transition and no transition at the maximum tree depth",
    if (all(rows$passed)) "passed every check of the validity gate"
  )
  sample_part <- if (length(known_n) && same(known_n) && length(known_iter) && same(known_iter)) paste0(
    "; the ", rh_fmt_count(length(known_n)), " normal fits of RQ1 used ", rh_fmt_n(known_n[[1]]), " cases and ",
    rh_fmt_n(known_iter[[1]]), " post-warm-up iterations per chain"
  ) else ""
  if (length(parts) == 0L) return("")
  paste0("All ", rh_fmt_count(k), " fits ", paste(parts, collapse = " and "), sample_part, ".")
}

#' The outcomes a coefficient table has an estimated fit for
#'
#' A coefficient table of the accepted regression route carries one row per
#' outcome even when no fit exists: an untriggered Student-t refit leaves a
#' placeholder row with `fit_available = FALSE`, no term and no estimate. The
#' presence of a row therefore says nothing about whether a model ran, and a
#' display that asks "did this fit run?" reads the recorded availability
#' instead. A table without the column carries estimated rows only.
#'
#' @param coefs Coefficient table (e.g. the target `ap6_student_coefs`), or
#'   `NULL`.
#' @return Character vector of the outcomes whose fit exists (possibly empty).
rh_fitted_outcomes <- function(coefs) {
  if (is.null(coefs)) return(character(0))
  df <- tibble::as_tibble(coefs)
  if (nrow(df) == 0L || !"outcome" %in% names(df)) return(character(0))
  available <- if ("fit_available" %in% names(df)) {
    as.logical(df$fit_available) %in% TRUE
  } else {
    rep(TRUE, nrow(df))
  }
  unique(as.character(df$outcome[available]))
}

#' Which facets met the rule for heavier-than-normal tails, in one sentence
#'
#' For every facet that met the preregistered rule for
#' heavier-than-normal tails, the sentence names the statistics outside their
#' simulated range in the heavy-tail direction (excess kurtosis or maximum
#' above, minimum below), with their values, and whether the refit allowing
#' heavier tails ran; which refits ran is read from the refits themselves
#' ([rh_fitted_outcomes()]).
#'
#' @param tails Tibble `outcome`, `heavy_tails` (the S4 target's `tails`).
#' @param pp_stats Tibble from `ap6_pp_stats()` bound over outcomes.
#' @param student_coefs Coefficient table of the Student-t refits.
#' @param analysis_plan Configuration (the statistics of the rule).
#' @param labels Named labels from [rh_labels()].
#' @return One or two sentences.
rh_tail_text <- function(tails, pp_stats, student_coefs, analysis_plan, labels = rh_labels()) {
  stats <- as.character(unlist(analysis_plan[["sensitivity"]][["student_t"]][["trigger_stats"]]))
  if (length(stats) == 0L) stop("analysis_plan$sensitivity$student_t$trigger_stats is empty.")
  words <- c(kurtosis = "excess kurtosis", max = "maximum", min = "minimum", skew = "skewness")
  t <- tibble::as_tibble(tails)
  pp <- tibble::as_tibble(pp_stats)
  join <- function(x) if (length(x) <= 1L) paste(x, collapse = "") else
    paste0(paste(x[-length(x)], collapse = ", "), " and ", x[length(x)])
  rule <- "the preregistered rule for heavier-than-normal tails"
  met <- t$outcome[t$heavy_tails %in% TRUE]
  refitted <- rh_fitted_outcomes(student_coefs)
  unavailable <- t[is.na(t$heavy_tails), , drop = FALSE]
  unavailable_text <- if (nrow(unavailable)) {
    notes <- if ("note" %in% names(unavailable)) unique(as.character(unavailable$note)) else character()
    notes <- notes[!is.na(notes) & nzchar(notes)]
    paste0("The rule could not be evaluated for ", join(rh_label_inline(unavailable$outcome, labels)), ".",
           if (length(notes)) paste0(" ", paste(notes, collapse = "; ")) else "")
  } else ""
  if (length(met) == 0L) {
    if (nrow(unavailable) == nrow(t)) return(unavailable_text)
    return(trimws(paste0("No ", if (nrow(unavailable)) "assessed " else "", "facet met ", rule,
                        ", so no model was refitted allowing heavier tails. ", unavailable_text)))
  }
  detail <- vapply(met, function(outcome) {
    rows <- pp[pp$outcome %in% outcome & pp$stat %in% stats, , drop = FALSE]
    heavy <- (rows$stat %in% c("kurtosis", "max") & rows$observed > rows$rep_hi) |
      (rows$stat %in% "min" & rows$observed < rows$rep_lo)
    rows <- rows[heavy %in% TRUE, , drop = FALSE]
    if (nrow(rows) == 0L) return("")
    paste0(
      join(paste0("its ", ifelse(rows$stat %in% names(words), unname(words[rows$stat]), rows$stat), " (",
                  rh_fmt(rows$observed), "; simulated range ", rh_fmt_ci(rows$rep_lo, rows$rep_hi), ")")),
      " lay outside the range of ", rh_fmt(rh_cfg_ci_level(analysis_plan), 0),
      "% of the data sets simulated from the normal model"
    )
  }, character(1))
  lead <- if (length(met) == 1L) {
    paste0(if (nrow(unavailable)) "" else "Only ", rh_label_inline(met, labels), " met ", rule)
  } else {
    rh_capitalise_first_letter(paste0(join(rh_label_inline(met, labels)), " met ", rule))
  }
  first <- if (all(!nzchar(detail))) paste0(lead, ".") else if (length(met) == 1L) {
    paste0(lead, ": ", detail, ".")
  } else {
    paste0(lead, ": ", join(paste0("for ", rh_label_inline(met, labels), ", ", detail)[nzchar(detail)]), ".")
  }
  not_refitted <- setdiff(met, refitted)
  second <- if (length(not_refitted) == 0L) {
    paste0("The refit allowing heavier tails (Student-t errors) therefore ran for ",
           join(rh_label_inline(met, labels)), if (length(met) < nrow(t)) " only" else "", ".")
  } else {
    paste0("The refit allowing heavier tails did not complete for ", join(rh_label_inline(not_refitted, labels)), ".")
  }
  trimws(paste(first, second, unavailable_text))
}

#' Robustness table: primary Gaussian fit against the Student-t refit
#'
#' Only the coefficients of the facets that were refitted; each estimate with
#' its credible interval in one cell, bold where the interval excludes zero.
#' The fixed degrees of freedom are reported from the refit rows (the comparison's
#' `nu_fixed`) when present, else from
#' `analysis_plan$sensitivity$student_t$nu_fixed`, with the matched scale prior
#' ([rh_nu_fixed_text()]). The table displays the comparison's values and
#' classifies nothing itself.
#'
#' @param comparison The comparison of the S4 target
#'   ([tabulate_likelihood_comparison()]).
#' @param labels Named labels from [rh_labels()].
#' @param engine See [rh_use_gt()].
#' @param analysis_plan Configuration (the matched scale prior of the refit).
#' @param target Name(s) of the target the numbers come from; see [rh_source_note()].
#' @return Display table, or `NULL` when no refit ran.
rh_robustness_table <- function(comparison, labels = rh_labels(), engine = "auto", analysis_plan, target = NULL) {
  j <- tibble::as_tibble(comparison)
  j <- j[j$fitted_student %in% TRUE, , drop = FALSE]
  if (nrow(j) == 0L) return(NULL)
  cell <- function(est, lo, hi, fitted, credible) {
    ifelse(fitted %in% TRUE, rh_fmt_est_ci(est, lo, hi, bold = credible %in% TRUE), "not fitted")
  }
  df <- tibble::tibble(
    outcome = rh_blank_repeated(rh_label(j$outcome, labels)),
    term = rh_label(j$term, labels),
    gaussian = cell(j$estimate_gaussian, j$lo_gaussian, j$hi_gaussian, j$fitted_gaussian, j$credible_gaussian),
    student = cell(j$estimate_student, j$lo_student, j$hi_student, j$fitted_student, j$credible_student)
  )
  if (length(unique(j$outcome)) == 1L) df$outcome <- NULL
  nu_rows <- attr(comparison, "nu_fixed")
  nu_text <- rh_nu_fixed_text(analysis_plan[["sensitivity"]][["student_t"]], nu_fixed = nu_rows)
  ci_what <- rh_ci_label(100 * attr(comparison, "ci_level"))
  uninterpretable <- is.na(j$credible_gaussian) | is.na(j$credible_student)
  note <- paste0(
    "Posterior median [", ci_what, "]; bold estimate: its interval excludes zero. Student-t degrees of freedom: ", nu_text, ".",
    if (any(uninterpretable)) " A coefficient whose sampling checks did not pass is shown without classification." else ""
  )
  rh_table(
    df,
    col_labels = c(outcome = "Outcome", term = "Coefficient", gaussian = "Normal model",
                   student = "Model allowing heavier tails (Student-t)")[names(df)],
    engine = engine, target = target, markdown = c("gaussian", "student"),
    source_note = note
  )
}

# ---- figures ----------------------------------------------------------------

#' Shared ggplot theme for the report
#'
#' @param base_size Base font size.
#' @return A ggplot2 theme.
rh_theme <- function(base_size = 11) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      legend.position = "bottom",
      strip.text = ggplot2::element_text(face = "bold"),
      plot.title.position = "plot"
    )
}

#' Colours and shapes for the slope-prior sweep (fixed order by prior SD)
#'
#' @param sds Numeric vector of slope prior SDs.
#' @param primary The primary SD (gets the darkest colour and a filled circle).
#' @param analysis_plan Configuration; its `priors$sweep_labels` name the SDs
#'   ("0.20 (primary)", "0.10 (skeptical)").
#' @return List with `levels` (display labels in SD order), `colours`,
#'   `shapes` (both named by the display labels) and `label` (function SD -> label).
rh_prior_scale <- function(sds, primary, analysis_plan) {
  sds <- sort(unique(as.numeric(sds)))
  lab <- function(sd) paste0(rh_fmt(sd), " (", rh_sweep_label(sd, analysis_plan), ")")
  levels <- lab(sds)
  palette <- c("#E69F00", "#0072B2", "#009E73", "#CC79A7", "#56B4E9", "#D55E00", "#F0E442", "#000000")
  palette[match(primary, sds, nomatch = 1L)] <- "#000000"
  if (length(sds) == 1L) palette[1] <- "#000000"
  shapes <- c(15, 16, 17, 18, 0, 1, 2, 5)
  k <- length(sds)
  linetypes <- rep(c("dashed", "solid", "dotdash", "dotted"), length.out = k)
  linetypes[sds == primary | k == 1L] <- "solid"
  if (k == 1L) shapes[1] <- 16
  list(
    levels = levels,
    colours = stats::setNames(palette[seq_len(k)], levels),
    shapes = stats::setNames(shapes[seq_len(k)], levels),
    linetypes = stats::setNames(linetypes, levels),
    label = lab
  )
}

#' Coefficient plot: posterior medians and CrIs under every slope prior
#'
#' One panel per outcome (all outcomes when `outcome` is `NULL`); the slope
#' priors are distinguished by colour and shape in fixed SD order, the primary
#' prior is marked in the legend. All terms share the outcome-SD axis, but
#' their units differ — metric terms per predictor SD, gender contrasts as a
#' group difference against the reference level — and the axis label says so.
#' The interval columns and their level are read from the quantile names
#' ([rh_ci_cols()]).
#'
#' @param coefs Coefficient rows of every fit, with `outcome`, `slope_sd`,
#'   `term`, `term_type`, `estimate` and the interval columns named by their quantile.
#' @param outcome Outcome key to show, or `NULL` for all (facets).
#' @param analysis_plan Configuration (`analysis_plan$regression$motives`, `analysis_plan$priors$slope_sd_primary`).
#' @param labels Named labels from [rh_labels()].
#' @param terms Which term types to show.
#' @return A ggplot.
rh_coef_plot <- function(coefs, outcome = NULL, analysis_plan, labels = rh_labels(),
                         terms = c("predictor", "covariate")) {
  b <- tibble::as_tibble(coefs)
  ci <- rh_ci_cols(b)
  b$ci_lo <- b[[ci$lo]]
  b$ci_hi <- b[[ci$hi]]
  if (!is.null(outcome)) b <- b[b$outcome %in% outcome, , drop = FALSE]
  notes <- if ("note" %in% names(b)) unique(as.character(b$note)) else character()
  notes <- notes[!is.na(notes) & nzchar(notes)]
  b <- b[b$term_type %in% terms, , drop = FALSE]
  if (nrow(b) == 0) {
    return(rh_na_plot(if (length(notes)) paste(notes, collapse = "; ") else "No coefficients are available."))
  }
  # ap6_clean_terms() already reports every term by its key.
  keys <- as.character(b$term)
  display <- unique(c(analysis_plan$regression$motives, setdiff(unique(keys), analysis_plan$regression$motives)))
  b$term_label <- factor(rh_label(keys, labels), levels = rev(unique(rh_label(display, labels))))
  sc <- rh_prior_scale(b$slope_sd, analysis_plan$priors$slope_sd_primary, analysis_plan)
  b$prior <- factor(sc$label(b$slope_sd), levels = sc$levels)
  b$outcome_label <- factor(rh_label(b$outcome, labels), levels = rh_label(analysis_plan$regression$outcomes, labels))
  pd <- ggplot2::position_dodge(width = 0.6)
  p <- ggplot2::ggplot(b, ggplot2::aes(x = estimate, y = term_label, colour = prior, shape = prior)) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey55", linewidth = 0.4) +
    ggplot2::geom_errorbar(ggplot2::aes(xmin = ci_lo, xmax = ci_hi, linetype = prior), width = 0, position = pd, linewidth = 0.5) +
    ggplot2::geom_point(position = pd, size = 2.2) +
    ggplot2::scale_colour_manual(values = sc$colours, name = "Slope prior SD") +
    ggplot2::scale_shape_manual(values = sc$shapes, name = "Slope prior SD") +
    ggplot2::scale_linetype_manual(values = sc$linetypes, name = "Slope prior SD") +
    ggplot2::labs(
      x = paste0(
        "Posterior median and ", rh_ci_label(ci$level), ", outcome-SD units\n",
        "(metric terms: per predictor SD; gender: difference vs the reference group, treatment coding)"
      ),
      y = NULL
    ) +
    rh_theme()
  if (is.null(outcome) || length(outcome) > 1) {
    p <- p + ggplot2::facet_wrap(~outcome_label, ncol = 2)
  } else {
    p <- p + ggplot2::labs(title = rh_label(outcome, labels))
  }
  if (length(sc$levels) == 1L) p <- p + ggplot2::theme(legend.position = "none")
  p
}

#' Prior against posterior for every motive coefficient of the primary fits
#'
#' JARS Table 8 asks for a plot of the prior against the posterior wherever
#' informative priors are used. The three-part display (prior, likelihood proxy,
#' posterior) needs the posterior under a flat prior, and no such fit exists
#' here: a flat-prior model is not preregistered and would not be fitted after
#' the data were seen. So this draws two parts and says the third is missing and
#' why.
#'
#' The posterior is drawn as the normal density with the coefficient's posterior
#' median and posterior standard deviation, both of which the coefficient table
#' records; the posterior draws themselves do not travel with that table.
#'
#' @param coefs_primary Coefficient table of the primary fits (target
#'   `supplement_prior_sensitivity`).
#' @param analysis_plan Configuration from `zm_config()` (the primary slope prior SD).
#' @param labels Named labels from [rh_labels()].
#' @return A ggplot.
rh_prior_posterior_plot <- function(coefs_primary, analysis_plan, labels = rh_labels(), density_data = NULL) {
  d <- tibble::as_tibble(coefs_primary)
  d <- d[d$term_type %in% "predictor", , drop = FALSE]
  if (nrow(d) == 0) {
    return(rh_na_plot("No motive coefficient of a primary fit is available for the prior-posterior plot."))
  }
  df <- if (is.null(density_data)) calculate_prior_posterior_density_data(coefs_primary, analysis_plan) else density_data
  df$outcome <- rh_label(df$outcome, labels)
  df$term <- rh_label(df$term, labels)
  df$source <- factor(df$source, levels = c("Prior", "Posterior"))
  # the motive rows and the facet columns in the configured order, which is the
  # codebook's
  ordered_levels <- function(x, keys) {
    known <- rh_label(as.character(keys), labels)
    unique(c(known[known %in% x], sort(setdiff(unique(x), known))))
  }
  df$term <- factor(df$term, levels = ordered_levels(df$term, analysis_plan$regression$motives))
  df$outcome <- factor(df$outcome, levels = ordered_levels(df$outcome, analysis_plan$regression$outcomes))
  ggplot2::ggplot(df, ggplot2::aes(x = .data$x, y = .data$density, colour = .data$source,
                                   linetype = .data$source)) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey80") +
    ggplot2::geom_line(linewidth = 0.6) +
    ggplot2::facet_grid(term ~ outcome, scales = "free_y") +
    ggplot2::scale_colour_manual(values = c(Prior = "grey45", Posterior = "#0072B2"), name = NULL) +
    ggplot2::scale_linetype_manual(values = c(Prior = "dashed", Posterior = "solid"), name = NULL) +
    # What the two curves are is stated in the text before the figure; a
    # caption inside the plot was cut at the page edge.
    ggplot2::labs(x = "Coefficient (outcome SD per predictor SD)", y = NULL) +
    rh_theme() +
    ggplot2::theme(axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank())
}

#' A plot that says "n/a"
#'
#' A figure cannot fall back to a table row, so a missing display returns an
#' empty panel carrying the reason.
#'
#' @param note The reason, shown in the panel.
#' @return A ggplot.
rh_na_plot <- function(note) {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 0, label = paste0("n/a — ", note), size = 3.5, colour = "grey30") +
    ggplot2::labs(x = NULL, y = NULL) +
    rh_theme() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    )
}

#' Normalise a posterior-predictive draws table to a fixed shape
#'
#' Reads the long table of `ap6_pp_check_data()` (columns `outcome`, `type`
#' — `"y"` for the observed rows, `"y_rep"` for the replications —, `draw`,
#' `obs`, `value`).
#'
#' @param pp Tibble from `ap6_pp_check_data()`.
#' @return Tibble with columns `outcome` (if present), `draw`, `value`,
#'   `observed` (logical).
rh_pp_normalise <- function(pp) {
  nm <- names(pp)
  value <- as.numeric(pp$value)
  draw <- pp$draw
  observed <- as.character(pp$type) == "y"
  if (!any(observed)) stop("No observed rows found in the posterior-predictive draws table.")
  draw_id <- as.integer(factor(draw))
  draw_id[observed] <- 0L
  out <- tibble::tibble(draw = draw_id, value = value, observed = observed)
  if ("outcome" %in% nm) out$outcome <- pp$outcome
  out
}

#' Posterior-predictive density overlay
#'
#' Replicated data sets as thin lines, observed data as a thick line; one
#' panel per outcome when several are present.
#'
#' @param pp_draws Tibble from `ap6_pp_check_data()` bound over outcomes.
#' @param outcome Optional outcome key(s) to show.
#' @param labels Named labels from [rh_labels()].
#' @return A ggplot.
rh_pp_plot <- function(pp_draws, outcome = NULL, labels = rh_labels(), density_data = NULL) {
  unavailable <- if (is.null(density_data)) attr(pp_draws, "unavailable_predictions", exact = TRUE) else
    attr(density_data, "unavailable_predictions", exact = TRUE)
  if (is.null(density_data)) {
    if (!is.null(outcome)) pp_draws <- pp_draws[pp_draws$outcome %in% outcome, , drop = FALSE]
    if (!is.null(outcome) && !is.null(unavailable)) unavailable <- unavailable[unavailable$outcome %in% outcome, , drop = FALSE]
    if (nrow(pp_draws) == 0L && !is.null(unavailable) && nrow(unavailable)) {
      return(rh_na_plot(paste(unique(unavailable$note), collapse = "; ")))
    }
    d <- rh_pp_normalise(pp_draws)
    d$series <- ifelse(d$observed, "Observed", "Replicated")
    d$group <- ifelse(d$observed, "observed", paste0("rep_", d$draw))
  } else {
    d <- density_data
    if (!is.null(outcome)) d <- d[d$outcome %in% outcome, , drop = FALSE]
    if (!is.null(outcome) && !is.null(unavailable)) unavailable <- unavailable[unavailable$outcome %in% outcome, , drop = FALSE]
    if (nrow(d) == 0L && !is.null(unavailable) && nrow(unavailable)) {
      return(rh_na_plot(paste(unique(unavailable$note), collapse = "; ")))
    }
  }
  if (nrow(d) == 0) stop("No posterior-predictive draws to plot.")
  facet <- "outcome" %in% names(d) && length(unique(d$outcome)) > 1
  if ("outcome" %in% names(d)) d$outcome_label <- rh_label(d$outcome, labels)
  saved_density <- !is.null(density_data)
  replicated_mapping <- if (saved_density) ggplot2::aes(x = value, y = density, group = group, colour = series) else
    ggplot2::aes(x = value, group = group, colour = series)
  observed_mapping <- if (saved_density) ggplot2::aes(x = value, y = density, colour = series) else
    ggplot2::aes(x = value, colour = series)
  p <- ggplot2::ggplot() +
    ggplot2::geom_density(
      data = d[!d$observed, , drop = FALSE], replicated_mapping,
      stat = if (saved_density) "identity" else "density", linewidth = 0.25, alpha = 0.6
    ) +
    ggplot2::geom_density(
      data = d[d$observed, , drop = FALSE],
      observed_mapping,
      stat = if (saved_density) "identity" else "density", linewidth = 1.1
    ) +
    ggplot2::scale_colour_manual(values = c(Observed = "#000000", Replicated = "#56B4E9"), name = NULL) +
    ggplot2::labs(x = "Standardised outcome", y = "Density") +
    rh_theme() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_line(colour = "grey92"))
  if (facet) p <- p + ggplot2::facet_wrap(~outcome_label, ncol = 2, scales = "free_y")
  p
}

#' Node positions and drawable edges of a network on a circle
#'
#' Shared layout for [rh_network_plot()] and [rh_network_side_by_side()]:
#' nodes on a circle in the given order, edges with their weight and
#' decision; absent edges (and, when `show_inconclusive` is `FALSE`,
#' inconclusive edges) are dropped.
#'
#' @param matrices List from `ap6_network_matrix()` (or `ap6_network_full_matrix()`).
#' @param nodes Node order (default `matrices$nodes`).
#' @param labels Named labels from [rh_labels()].
#' @param groups Optional named list of node groups for the node fill.
#' @param show_inconclusive Keep inconclusive edges.
#' @return List `pos` (node, x, y, label, group) and `edges` (i, j, from, to,
#'   weight, decision, x, y, xend, yend, sign, width).
rh_network_layout <- function(matrices, nodes = NULL, labels = rh_labels(), groups = NULL,
                              show_inconclusive = TRUE) {
  if (is.null(nodes)) nodes <- matrices$nodes
  nodes <- as.character(nodes)
  p <- length(nodes)
  angle <- pi / 2 - 2 * pi * (seq_len(p) - 1) / p
  pos <- tibble::tibble(node = nodes, x = cos(angle), y = sin(angle), label = rh_label(nodes, labels))
  pos$group <- "Node"
  if (!is.null(groups)) {
    for (g in names(groups)) pos$group[pos$node %in% as.character(groups[[g]])] <- g
  }
  pos$group[pos$group %in% c("Outcome", "Facet")] <- "Authoritarian facet"
  w <- matrices$weight
  dec <- matrices$decision
  edges <- expand.grid(i = seq_len(p), j = seq_len(p))
  edges <- edges[edges$i < edges$j, , drop = FALSE]
  edges$from <- nodes[edges$i]
  edges$to <- nodes[edges$j]
  edges$weight <- w[cbind(match(edges$from, rownames(w)), match(edges$to, colnames(w)))]
  edges$decision <- dec[cbind(match(edges$from, rownames(dec)), match(edges$to, colnames(dec)))]
  edges <- edges[!is.na(edges$decision) & edges$decision != "absent", , drop = FALSE]
  if (!show_inconclusive) edges <- edges[edges$decision == "present", , drop = FALSE]
  edges$x <- pos$x[edges$i]
  edges$y <- pos$y[edges$i]
  edges$xend <- pos$x[edges$j]
  edges$yend <- pos$y[edges$j]
  edges$sign <- ifelse(edges$decision != "present", "Inconclusive", ifelse(edges$weight >= 0, "Positive", "Negative"))
  edges$width <- ifelse(edges$decision == "present", pmax(abs(edges$weight), 0.02), 0.02)
  list(pos = tibble::as_tibble(pos), edges = tibble::as_tibble(edges))
}

#' Network figure: present edges in colour, inconclusive edges dashed
#'
#' Nodes on a circle in the configured order; edge width is proportional to
#' the bagged weight, colour to its sign (blue positive, vermilion negative).
#' Absent edges are not drawn.
#'
#' @param matrices List from `ap6_network_matrix()`.
#' @param nodes Node order (default `matrices$nodes`).
#' @param labels Named labels from [rh_labels()].
#' @param groups Optional named list of node groups for the node fill, e.g.
#'   `list(Motive = predictors, Outcome = outcomes)`.
#' @param show_inconclusive Draw inconclusive edges as dashed grey lines.
#' @return A ggplot.
rh_network_plot <- function(matrices, nodes = NULL, labels = rh_labels(), groups = NULL,
                            show_inconclusive = TRUE) {
  layout <- rh_network_layout(matrices, nodes, labels, groups, show_inconclusive)
  pos <- layout$pos
  edges <- layout$edges

  gg <- ggplot2::ggplot()
  if (nrow(edges) > 0) {
    gg <- gg + ggplot2::geom_segment(
      data = edges,
      ggplot2::aes(x = x, y = y, xend = xend, yend = yend, colour = sign, linewidth = width, linetype = sign),
      lineend = "round"
    )
  }
  gg +
    ggplot2::geom_point(data = pos, ggplot2::aes(x = x, y = y, fill = group, shape = group), size = 14, colour = "grey30") +
    ggplot2::geom_text(data = pos, ggplot2::aes(x = x, y = y, label = label), size = 2.6, lineheight = 0.85) +
    ggplot2::scale_colour_manual(
      values = c(Positive = "#0072B2", Negative = "#D55E00", Inconclusive = "grey60"), name = "Edge"
    ) +
    ggplot2::scale_linetype_manual(
      values = c(Positive = "solid", Negative = "longdash", Inconclusive = "dotted"), name = "Edge"
    ) +
    ggplot2::scale_linewidth_continuous(range = c(0.3, 3), guide = "none") +
    ggplot2::scale_fill_manual(values = c(Node = "grey92", Motive = "#DCEBF5", "Authoritarian facet" = "#FBE3CC"), name = NULL) +
    ggplot2::scale_shape_manual(values = c(Node = 21, Motive = 21, "Authoritarian facet" = 22), name = NULL) +
    ggplot2::coord_equal(xlim = c(-1.35, 1.35), ylim = c(-1.35, 1.35)) +
    ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(legend.position = "bottom")
}

#' Full-data and bagged network side by side (same layout)
#'
#' Two panels with identical node positions, drawn as the network figure of the
#' Results is drawn ([rh_network_plot()]): a present
#' edge in colour by its sign (blue positive, vermilion negative) with its
#' width following the absolute edge weight, an inconclusive edge as a thin
#' dotted grey line; absent edges are not drawn.
#'
#' @param matrices_full List from `ap6_network_full_matrix()`.
#' @param matrices_bagged List from `ap6_network_matrix()`.
#' @param labels Named labels from [rh_labels()].
#' @param groups Optional named list of node groups for the node fill.
#' @param nodes Node order (default `matrices_bagged$nodes`), used for both panels.
#' @param show_inconclusive Draw inconclusive edges as dotted grey lines.
#' @return A ggplot with two facets.
rh_network_side_by_side <- function(matrices_full, matrices_bagged, labels = rh_labels(), groups = NULL,
                                    nodes = NULL, show_inconclusive = TRUE) {
  if (is.null(nodes)) nodes <- matrices_bagged$nodes
  panels <- c("Full-data network", "Bagged network")
  lay_full <- rh_network_layout(matrices_full, nodes, labels, groups, show_inconclusive)
  lay_bag <- rh_network_layout(matrices_bagged, nodes, labels, groups, show_inconclusive)
  lay_full$pos$panel <- panels[1]
  lay_full$edges$panel <- rep(panels[1], nrow(lay_full$edges))
  lay_bag$pos$panel <- panels[2]
  lay_bag$edges$panel <- rep(panels[2], nrow(lay_bag$edges))
  pos <- dplyr::bind_rows(lay_full$pos, lay_bag$pos)
  edges <- dplyr::bind_rows(lay_full$edges, lay_bag$edges)
  pos$panel <- factor(pos$panel, levels = panels)
  gg <- ggplot2::ggplot()
  if (nrow(edges) > 0) {
    edges$panel <- factor(edges$panel, levels = panels)
    gg <- gg + ggplot2::geom_segment(
      data = edges,
      ggplot2::aes(x = x, y = y, xend = xend, yend = yend, colour = sign, linewidth = width, linetype = sign),
      lineend = "round"
    )
  }
  gg +
    ggplot2::geom_point(data = pos, ggplot2::aes(x = x, y = y, fill = group, shape = group), size = 11, colour = "grey30") +
    ggplot2::geom_text(data = pos, ggplot2::aes(x = x, y = y, label = label), size = 2.2, lineheight = 0.85) +
    ggplot2::scale_colour_manual(
      values = c(Positive = "#0072B2", Negative = "#D55E00", Inconclusive = "grey60"), name = "Edge"
    ) +
    ggplot2::scale_linetype_manual(
      values = c(Positive = "solid", Negative = "longdash", Inconclusive = "dotted"), name = "Edge"
    ) +
    ggplot2::scale_linewidth_continuous(range = c(0.3, 3), guide = "none") +
    ggplot2::scale_fill_manual(values = c(Node = "grey92", Motive = "#DCEBF5", "Authoritarian facet" = "#FBE3CC"), name = NULL) +
    ggplot2::scale_shape_manual(values = c(Node = 21, Motive = 21, "Authoritarian facet" = 22), name = NULL) +
    ggplot2::coord_equal(xlim = c(-1.35, 1.35), ylim = c(-1.35, 1.35)) +
    ggplot2::facet_wrap(~panel, nrow = 1) +
    ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(legend.position = "bottom", strip.text = ggplot2::element_text(face = "bold"))
}

#' Adjusted associations: the main table of the Results section
#'
#' Formats the report target `report_association_table`: one row per
#' preregistered cell with its model sample size, posterior median and
#' interval, the preregistered sign, the verdict, whether the classification is
#' the same under every prior width. Every value comes from the target;
#' nothing is computed here.
#'
#' @param cells Tibble `report_association_table`.
#' @param labels Named labels from [rh_labels()].
#' @param analysis_plan Configuration; supplies the interval level for the label.
#' @param engine See [rh_use_gt()].
#' @param target Name(s) of the target the numbers come from; see [rh_source_note()].
#' @return Display table.
rh_prediction_cells_table <- function(cells, labels = rh_labels(), analysis_plan,
                                      engine = "auto", target = "report_association_table") {
  ci_label <- rh_ci_label(rh_cfg_ci_level(analysis_plan))
  outcome_labels <- rh_label(cells$outcome_key, labels)
  predictor_labels <- rh_label(cells$predictor, labels)
  # Estimate and interval in one cell; the head names
  # the quantity in words, as the preregistration does, and the note says what
  # it means for two people.
  df <- tibble::tibble(
    outcome = rh_blank_repeated(outcome_labels),
    motive = predictor_labels,
    estimate = rh_fmt_est_ci(cells$estimate, cells$interval_lower, cells$interval_upper)
  )
  credible <- cells$fit_valid %in% TRUE & !is.na(cells$interval_lower) & !is.na(cells$interval_upper) &
    (cells$interval_lower > 0 | cells$interval_upper < 0)
  df$estimate <- rh_fmt_est_ci(cells$estimate, cells$interval_lower, cells$interval_upper, bold = credible)
  note <- paste0(
    "Posterior median: the expected difference in the facet, in standard deviations, between two people who ",
    "differ by one standard deviation in this motive and are alike in the other motives, age, gender and ",
    "income. ", ci_label, ": central credible interval. Bold estimate: its interval excludes zero."
  )
  robust <- cells$prior_robust
  n_priors <- length(analysis_plan[["priors"]][["slope_sd_sweep"]])
  if (length(robust) && all(robust %in% TRUE)) {
    note <- paste0(note, " Every classification is the same under all ", rh_fmt_count(n_priors), " priors.")
  } else if (any(robust %in% FALSE)) {
    changed <- which(robust %in% FALSE)
    note <- paste0(note, " The classification depends on the prior for ",
                   paste(paste(rh_label_inline(cells$predictor[changed], labels), "→",
                               rh_label_inline(cells$outcome_key[changed], labels)), collapse = "; "), ".")
  }
  invalid <- which(!cells$fit_valid %in% TRUE)
  if (length(invalid)) {
    note <- paste0(note, " Not interpretable because sampling diagnostics did not pass: ",
                   paste(unique(rh_label_inline(cells$outcome_key[invalid], labels)), collapse = ", "), ".")
  }
  if (anyNA(robust)) {
    note <- paste(note, "The prior comparison is unavailable for one or more coefficients.")
  }
  rh_table(
    df, col_labels = c(outcome = "Outcome", motive = "Predictor",
                       estimate = paste0("Posterior median [", ci_label, "]")),
    engine = engine, target = target, source_note = note
  )
}

#' Save the Bayes-factor resolution for one network or a bag of networks
calculate_network_bf_resolution <- function(n_sweeps, prior, n_resamples = 1L) {
  networks <- as.numeric(n_sweeps) * as.numeric(n_resamples)
  tryCatch(ap6_network_bf_bounds(networks, prior), error = function(e) {
    list(upper = NA_real_, lower = NA_real_, n_sweeps = NA_integer_)
  })
}

#' Save the precision facts used in the note of one network edge display
calculate_network_bf_precision <- function(bagged, fits, pairs) {
  n_se <- 2
  if (identical(attr(bagged, "feasible"), FALSE) || is.null(fits) || !nrow(fits)) {
    return(list(available = FALSE))
  }
  prior <- attr(bagged, "g_prior")
  n_sweeps <- attr(bagged, "n_sweeps")
  ok <- fits[fits$status %in% c("ok", "retry_ok") & !is.na(fits$pip), , drop = FALSE]
  n_resamples <- length(unique(ok$b))
  if (n_resamples < 2L) return(list(available = FALSE))
  key_fit <- rh_network_pair_key(ok$node_i, ok$node_j)
  shown <- bagged[rh_network_pair_key(bagged$node_i, bagged$node_j) %in% pairs &
    !is.na(bagged$bf_bagged) & bagged$bf_bagged > rh_bf_display_rule()$rungs[[1]], , drop = FALSE]
  if (!nrow(shown)) return(list(available = FALSE))
  bounds <- ap6_network_bf_bounds(as.numeric(n_sweeps) * n_resamples, prior)
  se <- vapply(seq_len(nrow(shown)), function(i) {
    p <- ok$pip[key_fit == rh_network_pair_key(shown$node_i[[i]], shown$node_j[[i]])]
    if (length(p) < 2L) NA_real_ else stats::sd(p) / sqrt(length(p))
  }, numeric(1))
  rung <- vapply(shown$bf_bagged, rh_bf_rung, numeric(1), upper = bounds$upper, lower = NA_real_)
  low <- ap6_network_bf(pmax(shown$pip_bagged - n_se * se, 0), prior)
  high_pip <- shown$pip_bagged + n_se * se
  high <- ifelse(!is.na(high_pip) & high_pip >= 1, Inf, ap6_network_bf(pmin(high_pip, 1), prior))
  imprecise <- !is.na(se) & se > 0 & is.finite(rung) & low < rung
  j <- if (any(imprecise)) which(imprecise)[which.min((low / rung)[imprecise])] else NA_integer_
  always <- shown$pip_bagged >= 1 & !is.na(se) & se == 0
  list(available = TRUE, n_se = n_se, n_resamples = n_resamples, bounds = bounds,
    shown = shown, low = low, high = high, imprecise = imprecise, worst = j,
    n_always = sum(always))
}


#' Save the normal-density curves used in the prior/posterior display
calculate_prior_posterior_density_data <- function(coefs_primary, analysis_plan) {
  d <- tibble::as_tibble(coefs_primary)
  d <- d[d$term_type %in% "predictor", , drop = FALSE]
  if (!nrow(d)) return(data.frame())
  prior_sd <- as.numeric(analysis_plan$priors$slope_sd_primary)
  limit <- max(3 * prior_sd, max(abs(d$estimate) + 3 * d$sd, na.rm = TRUE))
  grid <- seq(-limit, limit, length.out = 401)
  one <- function(i, source, mean, sd) data.frame(
    outcome = d$outcome[i], term = d$term[i], x = grid,
    density = stats::dnorm(grid, mean = mean, sd = sd), source = source,
    stringsAsFactors = FALSE)
  do.call(rbind, c(lapply(seq_len(nrow(d)), function(i) one(i, "Prior", 0, prior_sd)),
    lapply(seq_len(nrow(d)), function(i) one(i, "Posterior", d$estimate[i], d$sd[i]))))
}


#' Read the labelled appendix and supplement headings used by report links
read_report_section_labels <- function(path) {
  qmd <- readLines(path, warn = FALSE)
  # review markup in a heading (~~old~~ *new*) does not change its label
  qmd <- gsub("~~[^~]*~~ ?", "", qmd)
  qmd <- gsub("(^#+ )[*]([^*]*)[*]", "\\1\\2", qmd)
  hits <- regmatches(qmd, regexec("^#{2,3} ([AS][0-9]+([.][0-9]+)?) .*[{]#(sec-[a-z0-9-]+)", qmd))
  hits <- Filter(length, hits)
  stats::setNames(vapply(hits, `[[`, "", 2L), vapply(hits, `[[`, "", 4L))
}

#' Save the existing posterior-predictive density curves, with their draw groups
calculate_predictive_density_data <- function(draws) {
  unavailable <- attr(draws, "unavailable_predictions", exact = TRUE)
  if (!nrow(draws) && !is.null(unavailable) && nrow(unavailable)) {
    out <- data.frame()
    attr(out, "unavailable_predictions") <- unavailable
    return(out)
  }
  keys <- unique(as.character(draws$outcome))
  plot <- rh_pp_plot(draws, labels = stats::setNames(keys, keys))
  built <- ggplot2::ggplot_build(plot)
  panels <- built$layout$layout
  dplyr::bind_rows(lapply(seq_along(built$data), function(layer) {
    rows <- built$data[[layer]]
    tibble::tibble(value = rows$x, density = rows$density,
      observed = layer == 2L, series = if (layer == 2L) "Observed" else "Replicated",
      group = as.character(rows$group),
      outcome = if ("outcome_label" %in% names(panels)) {
        as.character(panels$outcome_label[match(rows$PANEL, panels$PANEL)])
      } else rep(keys[[1L]], nrow(rows)))
  }))
}

#' Save the existing network resample boxplot summaries before rendering
calculate_network_boxplot_data <- function(per_fit_edges, compare) {
  e <- compare$edges
  pf <- tibble::as_tibble(per_fit_edges)
  pf <- pf[pf$status %in% c("ok", "retry_ok") & !is.na(pf$node_i) & !is.na(pf$node_j), , drop = FALSE]
  key <- rh_network_pair_key(e$node_i, e$node_j)
  pf$key <- rh_network_pair_key(pf$node_i, pf$node_j)
  pf <- pf[pf$key %in% key, , drop = FALSE]
  if (!nrow(pf)) return(data.frame())
  levels <- key[order(e$pip_bagged, e$pip_full, na.last = FALSE)]
  pf$edge <- factor(pf$key, levels = levels)
  plot <- ggplot2::ggplot(pf, ggplot2::aes(x = pip, y = edge)) +
    ggplot2::geom_boxplot(outlier.shape = NA, width = 0.6)
  rows <- ggplot2::ggplot_build(plot)$data[[1L]]
  rows$key <- levels[as.integer(rows$y)]
  rows[c("key", "xmin", "xlower", "xmiddle", "xupper", "xmax")]
}
