# R/ap4_reliability.R (AP4): the registered reliability fallback rule, the
# reliability table built from it, and the item sets the estimator selects.

local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) {
    parent <- dirname(dir)
    if (identical(parent, dir)) stop("project root not found from ", getwd())
    dir <- parent
  }
  for (f in c("config.R", "ap3_imputation_validity.R", "ap4_cfa_reporting.R", "ap4_reliability.R", "ap3_preparation.R",
              "ap9_pipeline.R", "report_supplement_measurement.R", "report_results_measurement.R", "report_helpers.R")) {
    source(file.path(dir, "R", f), local = FALSE)
  }
})

root <- zm_root()
analysis_plan <- zm_config(profile = "smoke", path = file.path(root, "config", "analysis_plan.yaml"))
codebook <- zm_codebook(analysis_plan)
scale_keys <- codebook$scales$scale_key
items_of <- function(key) codebook$scales$item_codes[[which(scale_keys == key)]]

# ================================================================================
# The registered fallback rule and the reliability table; every row's evidence
# comes from its own scale.
# ================================================================================

# The nested object is built directly, so the four fallback branches can be
# exercised without running an estimator. Every number below is distinct, so a
# row that took another scale's interval, count or sample size is visible.
reliability_responses <- function(scale_key, n) {
  items <- items_of(scale_key)
  responses <- as.data.frame(matrix(1, nrow = n, ncol = length(items)))
  names(responses) <- items
  responses
}

reliability_object <- function(cases) {
  keys <- vapply(cases, function(case) case$scale_key, character(1))
  scales <- stats::setNames(lapply(cases, function(case) list(
    scale_key = case$scale_key,
    item_codes = items_of(case$scale_key),
    responses = reliability_responses(case$scale_key, case$n),
    coefficients = list(omega = case$omega, alpha = case$alpha, note = case$note)
  )), keys)
  bootstrap <- stats::setNames(lapply(cases, function(case) list(
    omega_interval = case$omega_interval,
    alpha_interval = case$alpha_interval,
    omega_success = case$omega_success,
    n_boot_omega = case$n_boot_omega,
    n_boot_alpha = case$n_boot_alpha,
    n_resamples = case$n_resamples
  )), keys)
  list(item_sets = stats::setNames(lapply(keys, items_of), keys),
       scales = scales, n_participants = max(vapply(cases, function(case) case$n, numeric(1))),
       bootstrap = bootstrap, reported = NULL)
}

reliability_cases <- list(
  # 1. the omega model failed
  list(scale_key = "zm_achievement", n = 700, omega = NA_real_, alpha = 0.81,
       note = "psych::omega returned an unusable value: NA",
       omega_interval = c(NA_real_, NA_real_), alpha_interval = c(0.76, 0.86),
       omega_success = NA_real_, n_boot_omega = 0L, n_boot_alpha = 990L, n_resamples = 1000L),
  # 2. omega bootstrap success exactly at the configured threshold
  list(scale_key = "zm_security", n = 698, omega = 0.90, alpha = 0.88, note = NA_character_,
       omega_interval = c(0.85, 0.94), alpha_interval = c(0.83, 0.92),
       omega_success = 0.80, n_boot_omega = 800L, n_boot_alpha = 1000L, n_resamples = 1000L),
  # 3. low omega success, omega point retained without an interval
  list(scale_key = "zm_power", n = 699, omega = 0.70, alpha = 0.65, note = NA_character_,
       omega_interval = c(0.60, 0.78), alpha_interval = c(0.58, 0.72),
       omega_success = 0.50, n_boot_omega = 500L, n_boot_alpha = 1000L, n_resamples = 1000L),
  # 4. neither a finite alpha nor a finite alpha interval
  list(scale_key = "zm_prestige", n = 697, omega = NA_real_, alpha = NA_real_,
       note = "alpha unavailable: raw formula returned NaN",
       omega_interval = c(NA_real_, NA_real_), alpha_interval = c(NA_real_, NA_real_),
       omega_success = NA_real_, n_boot_omega = 0L, n_boot_alpha = 0L, n_resamples = 1000L)
)

reliability_result <- apply_reliability_fallback(reliability_object(reliability_cases), analysis_plan)
reliability_table <- tabulate_scale_reliability(reliability_result, codebook)
table_row <- function(scale_key) reliability_table[reliability_table$scale_key == scale_key, ]

test_that("the registered fallback rule chooses the reported coefficient of every branch", {
  reported <- reliability_result$reported
  expect_equal(reported$scale_key, c("zm_achievement", "zm_security", "zm_power", "zm_prestige"))
  expect_equal(reported$reported_method, c("alpha", "omega", "omega", "unavailable"))
  expect_equal(reported$interval_source,
               c("alpha_fallback_omega_model_failed", "omega_bootstrap",
                 "unavailable_omega_bootstrap", "unavailable_alpha_fallback_failed"))
  expect_equal(reported$estimate, c(0.81, 0.90, 0.70, NA_real_))
  expect_equal(reported$interval_lower, c(0.76, 0.85, NA_real_, NA_real_))
  expect_equal(reported$interval_upper, c(0.86, 0.94, NA_real_, NA_real_))
  # the threshold itself is a success, not a fallback
  expect_equal(analysis_plan$reliability$omega_interval_min_success, 0.80)
  expect_match(reported$note[4], "complete alpha fallback was unavailable")
})

test_that("the reliability table carries all four interval columns and counts", {
  expect_identical(names(reliability_table),
                   c("scale_key", "label", "n_items", "n", "omega_t", "omega_lo", "omega_hi",
                     "alpha", "alpha_lo", "alpha_hi", "method", "interval_source",
                     "n_resamples", "n_boot_omega", "n_boot_alpha", "omega_success", "note",
                     "interval_reason"))
  # omega-model failure: the omega interval stays unavailable, alpha's is kept
  failed <- table_row("zm_achievement")
  expect_true(is.na(failed$omega_t) && is.na(failed$omega_lo) && is.na(failed$omega_hi))
  expect_equal(c(failed$alpha, failed$alpha_lo, failed$alpha_hi), c(0.81, 0.76, 0.86))
  expect_equal(failed$method, "alpha")
  expect_equal(failed$interval_source, "alpha_fallback")
  expect_equal(failed$interval_reason, "alpha_fallback_omega_model_failed")
  expect_equal(c(failed$n_resamples, failed$n_boot_omega, failed$n_boot_alpha), c(1000L, 0L, 990L))
  expect_true(is.na(failed$omega_success))

  # omega success at the threshold: the omega bootstrap interval is reported
  reported <- table_row("zm_security")
  expect_equal(c(reported$omega_t, reported$omega_lo, reported$omega_hi), c(0.90, 0.85, 0.94))
  expect_equal(c(reported$alpha_lo, reported$alpha_hi), c(0.83, 0.92))
  expect_equal(reported$interval_source, "omega_bootstrap")
  expect_equal(reported$interval_reason, "omega_bootstrap")
  expect_equal(reported$omega_success, 0.80)

  # Low omega success: keep the omega point, omit its interval, and retain
  # alpha's own interval independently.
  substituted <- table_row("zm_power")
  expect_equal(substituted$omega_t, 0.70)
  expect_true(all(is.na(c(substituted$omega_lo, substituted$omega_hi))))
  expect_equal(c(substituted$alpha_lo, substituted$alpha_hi), c(0.58, 0.72))
  expect_equal(substituted$method, "omega")
  expect_equal(substituted$interval_source, "unavailable_omega_bootstrap")
  expect_equal(substituted$interval_reason, "unavailable_omega_bootstrap")
  expect_equal(c(substituted$n_boot_omega, substituted$n_boot_alpha), c(500L, 1000L))

  # no usable alpha fallback: every coefficient and interval is unavailable
  unavailable <- table_row("zm_prestige")
  expect_true(all(is.na(c(unavailable$omega_t, unavailable$omega_lo, unavailable$omega_hi,
                          unavailable$alpha, unavailable$alpha_lo, unavailable$alpha_hi))))
  expect_equal(unavailable$method, "unavailable")
  expect_equal(unavailable$interval_source, "unavailable_alpha_fallback_failed")
  expect_match(unavailable$note, "complete alpha fallback was unavailable")
})

test_that("every table row takes its points, intervals and sizes from its own scale", {
  # The failed scale may not inherit the successful one's bootstrap evidence,
  # its item count or its sample size.
  expect_equal(reliability_table$n, c(700L, 698L, 699L, 697L))
  expect_equal(reliability_table$n_items,
               vapply(reliability_table$scale_key, function(key) length(items_of(key)), integer(1),
                      USE.NAMES = FALSE))
  expect_equal(reliability_table$label,
               codebook$scales$label[match(reliability_table$scale_key, codebook$scales$scale_key)])
  failed <- table_row("zm_achievement")
  succeeded <- table_row("zm_security")
  expect_false(isTRUE(all.equal(failed$alpha_lo, succeeded$alpha_lo)))
  expect_true(is.na(failed$omega_lo))
  # the same object with the two scales in the reverse order gives the same rows
  reversed <- tabulate_scale_reliability(
    apply_reliability_fallback(reliability_object(rev(reliability_cases)), analysis_plan), codebook)
  expect_equal(reversed[match(reliability_table$scale_key, reversed$scale_key), ],
               reliability_table, ignore_attr = TRUE)
})

test_that("an unknown interval source stops instead of being projected", {
  broken <- reliability_result
  broken$reported$interval_source[1] <- "some_other_rule"
  expect_error(tabulate_scale_reliability(broken, codebook), "unknown reliability interval source")
})

test_that("omega survives an unavailable interval independently of alpha", {
  for (success in c(0.799, 0, NA_real_)) {
    case <- reliability_cases[[3]]
    case$omega_success <- success
    case$alpha <- NA_real_
    case$alpha_interval <- c(NA_real_, NA_real_)
    result <- apply_reliability_fallback(reliability_object(list(case)), analysis_plan)
    row <- result$reported
    expect_identical(row$reported_method, "omega")
    expect_equal(row$estimate, case$omega)
    expect_true(all(is.na(c(row$interval_lower, row$interval_upper))))
    expect_identical(row$interval_source, "unavailable_omega_bootstrap")
    table <- tabulate_scale_reliability(result, codebook)
    expect_equal(table$omega_t, case$omega)
    expect_true(all(is.na(c(table$omega_lo, table$omega_hi, table$alpha_lo, table$alpha_hi))))
  }
  case <- reliability_cases[[2]]
  case$omega_interval <- c(NA_real_, NA_real_)
  row <- apply_reliability_fallback(reliability_object(list(case)), analysis_plan)$reported
  expect_identical(row$reported_method, "omega")
  expect_equal(row$estimate, case$omega)
  expect_identical(row$interval_source, "unavailable_omega_bootstrap")
})

test_that("rh_reliability_table() renders all four reliability cases", {
  rendered <- paste(as.character(
    rh_reliability_table(reliability_table, engine = "kable", analysis_plan = analysis_plan)), collapse = "\n")
  configured <- rh_fmt_n(analysis_plan$reliability$bootstrap_n)
  # Every scale of the fixture needed the fallback or lost resamples, so every
  # one is listed: the detail table lists only such scales.
  # omega-model failure: no omega, no omega interval, alpha and its interval
  expect_true(grepl("α (ω model did not fit)", rendered, fixed = TRUE))
  expect_true(grepl("α interval (ω model did not fit)", rendered, fixed = TRUE))
  expect_true(grepl(".81 [.76, .86]", rendered, fixed = TRUE))
  expect_true(grepl("not attempted", rendered, fixed = TRUE))
  # omega bootstrap at the threshold, which lost resamples
  expect_true(grepl(".90 [.85, .94]", rendered, fixed = TRUE))
  expect_true(grepl("bootstrap of ω", rendered, fixed = TRUE))
  expect_true(grepl(paste("800", "of", configured), rendered, fixed = TRUE))
  # Omega stays visible without an interval; alpha keeps its own interval.
  expect_true(grepl("[.58, .72]", rendered, fixed = TRUE))
  expect_true(grepl("ω interval unavailable", rendered, fixed = TRUE))
  # the unavailable row renders as an em dash rather than a number
  expect_true(grepl("—", rendered, fixed = TRUE))
  expect_true(grepl("complete alpha fallback was unavailable", rendered, fixed = TRUE))
  expect_match(rendered, paste0("at least ",
    rh_fmt_pct(analysis_plan$reliability$omega_interval_min_success, 0, scale = "proportion"),
    " of its resamples succeed; otherwise ω is shown without an interval"), fixed = TRUE)
  expect_match(rendered, "95% percentile bootstrap confidence interval", fixed = TRUE)
  expect_match(rendered, "α is shown with its own bootstrap interval.", fixed = TRUE)
  expect_match(rendered, ".70", fixed = TRUE)
  expect_false(grepl(".70 [.58, .72]", rendered, fixed = TRUE))
  expect_false(grepl("analysis_plan$", rendered, fixed = TRUE))
})

# ================================================================================
# The item labels of the measurement definitions and the executable columns of
# the study are one explicit mapping.
# ================================================================================

test_that("the manuscript reliability table retains omega without pairing it with alpha's interval", {
  lines <- readLines(file.path(root, "report", "results_draft.qmd"), warn = FALSE)
  env <- new.env(parent = environment())
  env$report_reliability_value <- report_reliability_by_scale(reliability_result)
  env$report_reliability_counts_value <- report_count_reliability_substitutions(env$report_reliability_value)
  env$analysis_plan <- analysis_plan
  env$labels <- rh_labels(codebook)
  env$reliability_ci_pct <- "95%"
  env$rh_table <- function(data, ..., source_note) list(data = data, source_note = source_note)
  start <- which(lines == "# Measurement")
  end <- grep("^fmt_p <-", lines)[1] - 1L
  eval(parse(text = lines[start:end]), env)
  start <- which(lines == "#| label: tbl-reliability")
  end <- which(seq_along(lines) > start & lines == "```")[1] - 1L
  table <- eval(parse(text = lines[start:end]), env)
  expect_equal(table$data$omega, c("—", ".90 [.85, .94]", ".70", "—"))
  expect_equal(table$data$alpha, c(".81 [.76, .86]", ".88 [.83, .92]", ".65 [.58, .72]", "—"))
  expect_equal(table$data$interval[3], "ω interval unavailable")
  expect_match(table$source_note, "80%", fixed = TRUE)
  expect_match(table$source_note, "α retains its own interval.", fixed = TRUE)
  expect_match(env$omega_interval_text, "ω is reported without a bootstrap confidence interval", fixed = TRUE)
})

# The narrow descriptive and reliability input of twenty respondents, built from
# the codebook's executable column names. Every item column carries a different
# pattern, so an item that arrived in the wrong column is visible.
reliability_selection_input <- function() {
  item_codes <- unique(unlist(codebook$scales$item_codes))
  data <- tibble::tibble(respondent_id = seq_len(20))
  for (index in seq_along(item_codes)) {
    data[[item_codes[index]]] <- ((seq_len(20) + index) %% 6) + 1
  }
  for (index in seq_along(scale_keys)) {
    data[[scale_keys[index]]] <- rowMeans(
      as.matrix(data[codebook$scales$item_codes[[index]]]))
  }
  data$age <- 20 + seq_len(20)
  data$gender <- factor(rep(c("female", "male"), 10), levels = c("female", "male"))
  data$education <- factor(rep(1:2, 10), levels = 1:2, labels = c("a", "b"), ordered = TRUE)
  data$east_west <- factor(rep(c("west", "east"), 10), levels = c("west", "east"))
  data$income <- 1000 + seq_len(20)
  data$quota_group <- factor(rep("mixed", 20), levels = "mixed")
  select_descriptive_reliability_input(data, codebook)
}

# The item sets and the selected responses of
# estimate_reliability_coefficients(), without running an estimator:
# ap4_capture_reliability_failure() receives its package call unevaluated, and
# this replacement never forces it.
reliability_selection <- local({
  input <- reliability_selection_input()
  rlang::local_bindings(
    ap4_capture_reliability_failure = function(expression) {
      list(ok = FALSE, value = NULL, error = "test: estimator deliberately not run")
    },
    .env = environment(estimate_reliability_coefficients))
  list(input = input, result = estimate_reliability_coefficients(input, codebook))
})

test_that("the reliability item sets are the registered item codes of every scale", {
  # A design assertion: the estimator fixes the nine item sets by their source
  # labels, so the codebook and that fixed list may not drift apart.
  sets <- reliability_selection$result$item_sets
  expect_identical(names(sets), codebook$scales$scale_key)
  for (key in codebook$scales$scale_key) {
    expect_identical(zm_item_columns(sets[[key]], zm_item_column_map(codebook)),
                     items_of(key))
  }
})

test_that("all eight SDO items reach the estimator with their identity and values", {
  input <- reliability_selection$input
  responses <- reliability_selection$result$scales$sdo_dom$responses
  sdo_columns <- items_of("sdo_dom")
  expect_identical(names(responses), sdo_columns)
  expect_length(sdo_columns, 8L)
  # the reverse-keyed suffix belongs to both the label and the column
  expect_identical(sdo_columns[5:8], c("SDO_D_5_r", "SDO_D_6_r", "SDO_D_7_r", "SDO_D_8_r"))
  for (column in sdo_columns) {
    expect_identical(responses[[column]], input[[column]])
  }
  # the values carried are the already reversed ones the input holds
  expect_identical(responses$SDO_D_5_r, input$SDO_D_5_r)
  expect_false(identical(responses$SDO_D_1, responses$SDO_D_5_r))
})
