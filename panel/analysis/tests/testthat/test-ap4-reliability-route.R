# The reliability route of _targets.R executed end to end. The three verbs of
# the `scale_reliability` command run as the pipeline states them, and the
# reliability table the S1 target builds from their result
# (tabulate_scale_reliability()), through the
# target reader on the synthetic export: real psych point estimates, a real
# participant bootstrap, the real fallback rule, the real reliability table.
#
# One scale's failed omega model changes that scale's row alone. The
# hand-built branch cases of the fallback rule stay in test-ap4-reliability.R:
# this file adds the wired chain, it does not repeat their failure states.

local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) {
    parent <- dirname(dir)
    if (identical(parent, dir)) stop("project root not found from ", getwd())
    dir <- parent
  }
  for (f in c("config.R", "io_qualtrics.R", "ap1_exclusions.R", "ap3_preprocessing.R",
              "ap3_preparation.R", "ap3_fill.R", "ap3_data_files.R", "ap3_pipeline.R",
              "ap4_reliability.R", "ap9_pipeline.R", "report_supplement_measurement.R", "report_helpers.R")) {
    source(file.path(dir, "R", f), local = FALSE)
  }
  source(file.path(dir, "tests", "support", "targets-route.R"), local = FALSE)
})

rr_root <- zm_root()
rr_cfg <- zm_config("full", file.path(rr_root, "config", "analysis_plan.yaml"))
# The only configuration change: six participant resamples instead of the
# profile's thousand, so the real bootstrap finishes in seconds. The interval
# level and the omega success minimum stay the production ones, because the
# fallback rule is what this file tests.
rr_cfg$reliability$bootstrap_n <- 6L
rr_book <- zm_codebook(rr_cfg)
rr_raw <- read_qualtrics_export(file.path(rr_root, "data", "synthetic", "zm_panel_synthetic.sav"))
rr_keys <- rr_book$scales$scale_key
# The codebook's `item_codes` are the executable columns; the fixed item sets
# inside estimate_reliability_coefficients() are the source labels, which
# zm_item_columns() resolves to exactly these columns.
rr_columns <- function(key) rr_book$scales$item_codes[[match(key, rr_keys)]]
rr_reverse <- function(key) rr_book$scales$reverse_items[[match(key, rr_keys)]]
rr_probabilities <- c((1 - rr_cfg$reliability$interval_level) / 2,
                      1 - (1 - rr_cfg$reliability$interval_level) / 2)

# A fresh reader per run: make_target_reader() caches a target once it is read, so
# the failure run needs its own reader and its own production environment.
rr_route <- function() tr_route(rr_root, tr_intake(rr_raw, rr_cfg), rr_cfg, rr_book)
rr_quiet <- function(expr) suppressWarnings(suppressMessages(expr))

rr_ok_route <- rr_route()
rr_input <- rr_ok_route("data_descriptive_reliability")
rr_ok <- rr_quiet(rr_ok_route("scale_reliability"))
rr_ok_table <- tabulate_scale_reliability(rr_ok, rr_book)

# The one stand-in of this file: ap4_capture_reliability_failure() receives its
# package call unevaluated, so refusing to force the `psych::omega` call of the
# scale whose response columns are asc_sub's makes exactly that one omega model
# fail. Alpha, and every other scale's omega, run for real; no coefficient and
# no interval is fabricated. The rebinding happens in the environment the reader
# evaluates the target commands in.
rr_fail <- local({
  route <- rr_route()
  production <- parent.env(environment(route)$values)
  original <- get("ap4_capture_reliability_failure", envir = production)
  asc_sub_columns <- rr_columns("asc_sub")
  rlang::local_bindings(
    ap4_capture_reliability_failure = function(expression) {
      call_expression <- substitute(expression)
      head <- if (is.call(call_expression))
        paste(deparse(call_expression[[1L]]), collapse = "") else ""
      caller <- parent.frame()
      responses <- if (exists("responses", caller, inherits = FALSE))
        get("responses", caller, inherits = FALSE) else NULL
      if (grepl("omega", head, fixed = TRUE) &&
          !is.null(responses) && identical(names(responses), asc_sub_columns)) {
        return(list(ok = FALSE, value = NULL,
                    error = "test stand-in: psych::omega refused for asc_sub"))
      }
      original(expression)
    },
    .env = production)
  reliability <- rr_quiet(route("scale_reliability"))
  list(reliability = reliability, table = tabulate_scale_reliability(reliability, rr_book))
})
rr_fail_reported <- rr_fail$reliability$reported
rr_row <- function(table, key) table[table$scale_key == key, ]

# ---- data_descriptive_reliability -> estimate_reliability_coefficients

test_that("every scale reaches the estimator with the route's own item columns and values", {
  expect_identical(names(rr_ok$item_sets), rr_keys)
  expect_identical(names(rr_ok$scales), rr_keys)
  expect_identical(rr_ok$n_participants, nrow(rr_input))
  expect_true("respondent_id" %in% names(rr_input))
  for (key in rr_keys) {
    scale <- rr_ok$scales[[key]]
    columns <- rr_columns(key)
    expect_identical(scale$scale_key, key)
    # the estimator's fixed item sets resolve to the codebook's columns
    expect_identical(zm_item_columns(scale$item_codes, zm_item_column_map(rr_book)), columns)
    expect_identical(names(scale$responses), columns)
    expect_identical(nrow(scale$responses), nrow(rr_input))
    # the same participants in the same order, value for value
    for (column in columns) expect_identical(scale$responses[[column]], rr_input[[column]])
  }
  # the reverse-keyed suffix belongs to the column, not only to the source label
  expect_identical(rr_columns("sdo_dom")[5:8],
                   c("SDO_D_5_r", "SDO_D_6_r", "SDO_D_7_r", "SDO_D_8_r"))
  for (key in rr_keys) {
    expect_identical(grep("_r$", rr_columns(key), value = TRUE), rr_reverse(key))
  }
  expect_identical(unique(rr_ok$reported$n), nrow(rr_input))
})

# ---- estimate_reliability_coefficients -> add_bootstrap_intervals

test_that("the bootstrap entries are the estimator's scales, in its order, at the configured n", {
  expect_identical(names(rr_ok$bootstrap), names(rr_ok$scales))
  for (key in rr_keys) {
    bootstrap <- rr_ok$bootstrap[[key]]
    expect_identical(bootstrap$n_resamples, rr_cfg$reliability$bootstrap_n)
    expect_type(bootstrap$n_boot_omega, "integer")
    expect_type(bootstrap$n_boot_alpha, "integer")
    expect_lte(bootstrap$n_boot_omega, rr_cfg$reliability$bootstrap_n)
    expect_lte(bootstrap$n_boot_alpha, rr_cfg$reliability$bootstrap_n)
    # a percentile interval need not bracket the point estimate, but it is
    # finite and ordered whenever the coefficient was estimated at all
    expect_true(all(is.finite(bootstrap$omega_interval)))
    expect_true(all(is.finite(bootstrap$alpha_interval)))
    expect_lte(bootstrap$omega_interval[1], bootstrap$omega_interval[2])
    expect_lte(bootstrap$alpha_interval[1], bootstrap$alpha_interval[2])
    expect_equal(bootstrap$omega_success,
                 bootstrap$n_boot_omega / rr_cfg$reliability$bootstrap_n)
  }
})

test_that("one scale's interval is exactly the production draws of that scale's own responses", {
  # add_bootstrap_intervals() sets the plan's reliability.bootstrap_seed
  # per scale, so the draws are reproducible: recomputing them here must give
  # the identical interval.
  scale <- rr_ok$scales$zm_achievement
  draws <- rr_quiet(ap4_participant_bootstrap_draws(
    scale$responses, n_resamples = rr_cfg$reliability$bootstrap_n,
    seed = as.integer(rr_cfg$reliability$bootstrap_seed),
    resample_unit = "participant", replace = TRUE,
    recompute = c("omega", "alpha"), omega_available = TRUE))
  expect_identical(rr_ok$bootstrap$zm_achievement$omega_interval,
                   ap4_finite_percentile_interval(draws["omega", ], rr_probabilities))
  expect_identical(rr_ok$bootstrap$zm_achievement$alpha_interval,
                   ap4_finite_percentile_interval(draws["alpha", ], rr_probabilities))
  expect_identical(rr_ok$bootstrap$zm_achievement$n_boot_omega, sum(is.finite(draws["omega", ])))
  # and it is not another scale's interval
  expect_false(isTRUE(all.equal(rr_ok$bootstrap$zm_achievement$omega_interval,
                                rr_ok$bootstrap$zm_security$omega_interval)))
})

# ---- add_bootstrap_intervals -> apply_reliability_fallback

test_that("the fallback rule on the real bootstrap reports omega for every scale", {
  reported <- rr_ok$reported
  expect_identical(reported$scale_key, rr_keys)
  expect_true(all(reported$reported_method == "omega"))
  expect_true(all(reported$interval_source == "omega_bootstrap"))
  expect_equal(reported$estimate, reported$omega_t)
  expect_true(all(reported$omega_success >= rr_cfg$reliability$omega_interval_min_success))
  expect_true(all(is.na(reported$note)))
})

test_that("the failure of asc_sub changes asc_sub only", {
  expect_identical(rr_fail_reported$scale_key, rr_keys)
  expect_identical(rr_fail_reported$reported_method,
                   ifelse(rr_keys == "asc_sub", "alpha", "omega"))
  failed <- rr_row(rr_fail_reported, "asc_sub")
  expect_true(is.na(failed$omega_t))
  expect_identical(failed$interval_source, "alpha_fallback_omega_model_failed")
  expect_identical(failed$estimate, failed$alpha)
  expect_identical(failed$n_boot_omega, 0L)
  expect_true(is.na(failed$omega_success))
  expect_match(failed$note, "test stand-in: psych::omega refused for asc_sub", fixed = TRUE)
  # the reported interval is asc_sub's OWN alpha bootstrap interval — the same
  # alpha draws the all-success run produced for asc_sub
  own_alpha <- rr_ok$bootstrap$asc_sub$alpha_interval
  expect_identical(rr_fail$reliability$bootstrap$asc_sub$alpha_interval, own_alpha)
  expect_identical(c(failed$interval_lower, failed$interval_upper), own_alpha)
  expect_identical(failed$n_boot_alpha, rr_ok$bootstrap$asc_sub$n_boot_alpha)
  # no neighbour's evidence attaches to it: asc_conv is the next scale in
  # codebook order and its whole row is untouched
  expect_false(isTRUE(all.equal(c(failed$interval_lower, failed$interval_upper),
                                unname(unlist(rr_row(rr_ok$reported, "asc_conv")[
                                  c("interval_lower", "interval_upper")])))))
  expect_equal(rr_row(rr_fail_reported, "asc_conv"), rr_row(rr_ok$reported, "asc_conv"))
  # and neither does any other scale's
  expect_equal(rr_fail_reported[rr_fail_reported$scale_key != "asc_sub", ],
               rr_ok$reported[rr_ok$reported$scale_key != "asc_sub", ])
})

# ---- apply_reliability_fallback -> tabulate_scale_reliability
#      and the report helper on the real reliability table

test_that("the reliability table projects the route's object scale by scale", {
  expect_identical(names(rr_ok_table),
                   c("scale_key", "label", "n_items", "n", "omega_t", "omega_lo", "omega_hi",
                     "alpha", "alpha_lo", "alpha_hi", "method", "interval_source",
                     "n_resamples", "n_boot_omega", "n_boot_alpha", "omega_success", "note",
                     "interval_reason"))
  expect_identical(rr_ok_table$scale_key, rr_keys)
  expect_identical(rr_ok_table$label, rr_book$scales$label)
  expect_identical(rr_ok_table$n, rep(nrow(rr_input), length(rr_keys)))
  expect_identical(rr_ok_table$n_items,
                   vapply(rr_keys, function(key) length(rr_columns(key)), integer(1),
                          USE.NAMES = FALSE))
  expect_true(all(rr_ok_table$interval_source == "omega_bootstrap"))
  expect_true(all(rr_ok_table$interval_reason == "omega_bootstrap"))
  expect_identical(rr_ok_table$n_resamples,
                   rep(as.integer(rr_cfg$reliability$bootstrap_n), length(rr_keys)))
  for (key in rr_keys) {
    row <- rr_row(rr_ok_table, key)
    bootstrap <- rr_ok$bootstrap[[key]]
    expect_identical(c(row$omega_lo, row$omega_hi), bootstrap$omega_interval)
    expect_identical(c(row$alpha_lo, row$alpha_hi), bootstrap$alpha_interval)
    expect_identical(row$omega_t, rr_ok$scales[[key]]$coefficients$omega)
    expect_identical(row$alpha, rr_ok$scales[[key]]$coefficients$alpha)
  }
})

test_that("the reliability table of the failure run carries the fallback in asc_sub's row alone", {
  failed <- rr_row(rr_fail$table, "asc_sub")
  expect_true(all(is.na(c(failed$omega_t, failed$omega_lo, failed$omega_hi))))
  expect_identical(c(failed$alpha, failed$alpha_lo, failed$alpha_hi),
                   c(rr_ok$scales$asc_sub$coefficients$alpha, rr_ok$bootstrap$asc_sub$alpha_interval))
  expect_identical(failed$method, "alpha")
  expect_identical(failed$interval_source, "alpha_fallback")
  expect_identical(failed$interval_reason, "alpha_fallback_omega_model_failed")
  expect_identical(failed$n_boot_omega, 0L)
  expect_identical(failed$n_items, length(rr_columns("asc_sub")))
  expect_identical(failed$n, nrow(rr_input))
  expect_equal(rr_fail$table[rr_fail$table$scale_key != "asc_sub", ],
               rr_ok_table[rr_ok_table$scale_key != "asc_sub", ])
})

test_that("rh_reliability_table() renders both runs of the route's own table", {
  configured <- rh_fmt_n(rr_cfg$reliability$bootstrap_n)
  # The detail table lists only the scales that needed
  # the fallback or lost resamples; a run without one prints a sentence instead.
  expect_null(rh_reliability_table(rr_ok_table, engine = "kable", analysis_plan = rr_cfg))
  ok_text <- rh_reliability_detail_text(rr_ok_table, rr_cfg)
  expect_match(ok_text, paste0("all ", configured, " bootstrap resamples of ω and of α succeeded"), fixed = TRUE)
  rendered_fail <- paste(as.character(
    rh_reliability_table(rr_fail$table, engine = "kable", analysis_plan = rr_cfg)), collapse = "\n")
  expect_true(grepl("α (ω model did not fit)", rendered_fail, fixed = TRUE))
  expect_true(grepl("α interval (ω model did not fit)", rendered_fail, fixed = TRUE))
  expect_true(grepl("not attempted", rendered_fail, fixed = TRUE))
  # the scales without a fallback stay out of the table and in the sentence
  expect_false(grepl("bootstrap of ω", rendered_fail, fixed = TRUE))
  fail_text <- rh_reliability_detail_text(rr_fail$table, rr_cfg, table_ref = "Table X")
  expect_match(fail_text, "Table X lists the scale whose reliability needed the fallback", fixed = TRUE)
})
