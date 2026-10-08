# The confirmatory factor structure: the CFA-only rounding of the imputed item
# cells, the fourteen fixed models against the codebook, the reporting boundary,
# the two report tables and their wiring in the main map.

local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) {
    parent <- dirname(dir)
    if (identical(parent, dir)) stop("project root not found from ", getwd())
    dir <- parent
  }
  for (f in c("config.R", "io_qualtrics.R", "ap4_cfa_reporting.R", "ap4_reliability.R",
              "ap4_factor_structure.R", "ap4_cfa_input.R", "ap9_pipeline.R", "report_supplement_measurement.R",
              "report_helpers.R")) {
    source(file.path(dir, "R", f), local = FALSE)
  }
})

cfa_root <- zm_root()
cfa_cfg <- zm_config(profile = "smoke", path = file.path(cfa_root, "config", "analysis_plan.yaml"))
cfa_codebook <- zm_codebook(cfa_cfg)
cfa_map <- zm_item_column_map(cfa_codebook)
cfa_items <- unique(unlist(cfa_codebook$scales$item_codes))

# The CFA input as AP3 hands it over: respondent id and the reversed item
# columns. Every verb that selects item columns receives the codebook, and with
# it the label-to-column map, explicitly.
cfa_input <- local({
  raw <- read_qualtrics_export(file.path(cfa_root, "data", "synthetic", "zm_panel_synthetic.sav"))
  kept <- raw[raw$survey_status == cfa_cfg$exclusions$survey_status_complete &
                raw$demo_gender %in% c(1, 2), ]
  lo <- cfa_cfg$scales$response_min
  hi <- cfa_cfg$scales$response_max
  for (code in unique(unlist(cfa_codebook$scales$reverse_items))) kept[[code]] <- (lo + hi) - kept[[code]]
  x <- kept[, cfa_items]
  x <- x[stats::complete.cases(x), ]
  x$respondent_id <- seq_len(nrow(x))
  x <- tibble::as_tibble(x[, c("respondent_id", cfa_items)])
  x$demo_age <- 40
  x
})

cfa_empty_filled_cells <- tibble::tibble(
  respondent_id = integer(0), variable = character(0), kind = character(0),
  model = character(0), value = numeric(0), lower = numeric(0), upper = numeric(0),
  n_fit_rows = integer(0)
)

# The five definers, in the order the pipeline pipes them, and then the two
# verbs the target applies to the whole set.
cfa_define_models <- function(data, analysis_plan = cfa_cfg, codebook = cfa_codebook) {
  data |>
    define_subscale_models(analysis_plan, codebook) |>
    define_ums_dopl_models(analysis_plan, codebook) |>
    define_social_motives_model(analysis_plan, codebook) |>
    define_asc_model(analysis_plan, codebook) |>
    define_auth_orientation_model(analysis_plan, codebook)
}

cfa_run_models <- function(data, analysis_plan = cfa_cfg, codebook = cfa_codebook) {
  cfa_define_models(data, analysis_plan, codebook) |>
    fit_cfa_models(codebook, analysis_plan) |> assess_cfa_models()
}

cfa_reporting_of <- function(cfa_models, analysis_plan = cfa_cfg) {
  build_cfa_reporting_data(cfa_models, analysis_plan)
}

# The columns every projected CFA row carries.
cfa_row_columns <- c(
  "model", "set", "label", "n_items", "n_factors", "n", "df", "chisq", "pvalue",
  "cfi", "tli", "rmsea", "rmsea_lo", "rmsea_hi", "srmr", "index_version",
  "converged", "admissible", "error", "note", "reporting"
)

# ---- The CFA-only rounding ---------------------------------------------------

test_that("the CFA copy rounds exactly the recorded item fills and nothing else", {
  item <- cfa_items[1]
  observed_item <- cfa_items[2]
  common <- cfa_input
  common[[item]][1] <- 3.7
  common[[observed_item]][1] <- 4
  common$demo_age[1] <- 42.5
  before <- common
  filled <- tibble::tibble(
    respondent_id = c(1L, 1L), variable = c(item, "demo_age"),
    kind = c("item", "demographic"), model = c("item_model", "age_model"),
    value = c(3.7, 42.5), lower = NA_real_, upper = NA_real_, n_fit_rows = 10L
  )

  rounded <- round_imputed_items_for_cfa(common, filled, cfa_codebook)

  # exactly the recorded item cell moves to its nearest integer
  expect_identical(rounded[[item]][1], 4)
  expect_identical(rounded[[observed_item]][1], 4)
  expect_identical(rounded$demo_age[1], 42.5)
  # every other cell of that column, and every other column, is untouched
  expect_identical(rounded[[item]][-1], common[[item]][-1])
  for (column in setdiff(names(common), item)) {
    expect_identical(rounded[[column]], common[[column]], info = column)
  }
  # the common table itself, which the reliability and EFA inputs read, is
  # unchanged as an object, attributes included
  expect_identical(common, before)
  expect_identical(attributes(rounded), attributes(common))
})

test_that("halves go up and no other rule is applied", {
  item <- cfa_items[1]
  common <- cfa_input
  common[[item]][1:3] <- c(2.5, 3.5, 4.5)
  filled <- tibble::tibble(
    respondent_id = 1:3, variable = item, kind = "item", model = "item_model",
    value = c(2.5, 3.5, 4.5), lower = NA_real_, upper = NA_real_, n_fit_rows = 10L
  )
  # Round to the digit, halves UP. R's round() would give c(2, 4, 4) here; the
  # half-up rule gives c(3, 4, 5).
  expect_identical(
    round_imputed_items_for_cfa(common, filled, cfa_codebook)[[item]][1:3], c(3, 4, 5))
})

test_that("on complete observed data the rounding copy changes nothing", {
  rounded <- round_imputed_items_for_cfa(cfa_input, cfa_empty_filled_cells, cfa_codebook)
  expect_identical(rounded, cfa_input)
  expect_identical(names(cfa_empty_filled_cells), c(
    "respondent_id", "variable", "kind", "model", "value", "lower", "upper", "n_fit_rows"))
})

test_that("a recorded cell outside the CFA item columns or respondent set stops", {
  filled <- tibble::tibble(
    respondent_id = 1L, variable = "not_an_item", kind = "item", model = "m",
    value = 1, lower = NA_real_, upper = NA_real_, n_fit_rows = 1L)
  expect_error(round_imputed_items_for_cfa(cfa_input, filled, cfa_codebook),
               "outside the CFA item columns: not_an_item")
  filled$variable <- cfa_items[1]
  filled$respondent_id <- 10L + max(cfa_input$respondent_id)
  expect_error(round_imputed_items_for_cfa(cfa_input, filled, cfa_codebook),
               "respondents absent from the CFA input")
})

# ---- Labels versus executable columns ----------------------------------------

test_that("the SDO definition resolves to its eight executable columns, in order", {
  definition <- cfa_define_models(cfa_input)$models$sdo_dom
  labels <- c("SDO-D_1", "SDO-D_2", "SDO-D_3", "SDO-D_4",
              "SDO-D_5_r", "SDO-D_6_r", "SDO-D_7_r", "SDO-D_8_r")
  expect_identical(definition$factors$sdo_dom, labels)

  executable <- ap4_cfa_item_columns(definition, cfa_codebook)
  expected <- c("SDO_D_1", "SDO_D_2", "SDO_D_3", "SDO_D_4",
                "SDO_D_5_r", "SDO_D_6_r", "SDO_D_7_r", "SDO_D_8_r")
  expect_identical(executable$columns, expected)
  expect_identical(executable$factors$sdo_dom, expected)
  # the codebook's own item set for that scale, in its own order
  expect_identical(expected,
    as.character(cfa_codebook$scales$item_codes[[match("sdo_dom", cfa_codebook$scales$scale_key)]]))
  # the values are the ones the table carries; nothing is re-scored here
  fitted <- cfa_input[executable$columns]
  expect_identical(names(fitted), expected)
  for (column in expected) expect_identical(fitted[[column]], cfa_input[[column]], info = column)
  # the reversal state travels with the label: the four `_r` items are exactly
  # the reverse-keyed items of the scale, and their values are the reversed ones
  reverse <- as.character(
    cfa_codebook$scales$reverse_items[[match("sdo_dom", cfa_codebook$scales$scale_key)]])
  expect_identical(expected[grepl("_r$", expected)], reverse)
  raw <- read_qualtrics_export(file.path(cfa_root, "data", "synthetic", "zm_panel_synthetic.sav"))
  kept <- raw[raw$survey_status == cfa_cfg$exclusions$survey_status_complete &
                raw$demo_gender %in% c(1, 2), ]
  kept <- kept[stats::complete.cases(kept[, cfa_items]), ]
  span <- cfa_cfg$scales$response_min + cfa_cfg$scales$response_max
  for (column in reverse) {
    expect_equal(fitted[[column]], span - kept[[column]], info = column)
  }
})

# ---- The fixed definitions against the codebook and the plan ----------------

test_that("every one-factor definition is the codebook's scale, item for item", {
  models <- cfa_define_models(cfa_input)$models
  models <- models[names(models) %in% as.character(cfa_codebook$scales$scale_key)]
  expect_setequal(names(models), as.character(cfa_codebook$scales$scale_key))
  for (key in names(models)) {
    row <- match(key, cfa_codebook$scales$scale_key)
    definition <- models[[key]]
    expect_identical(names(definition$factors), key, info = key)
    expect_identical(zm_item_columns(definition$factors[[key]], cfa_map),
                     as.character(cfa_codebook$scales$item_codes[[row]]), info = key)
    expect_identical(definition$label, as.character(cfa_codebook$scales$label[row]), info = key)
  }
})

test_that("every multi-factor definition is a configured set, factor for factor", {
  all_models <- cfa_define_models(cfa_input)$models
  multi <- all_models[!names(all_models) %in% as.character(cfa_codebook$scales$scale_key)]
  keys <- vapply(cfa_cfg$factor_analysis$sets, function(set) as.character(set$key), character(1))
  # every multi-factor model is exactly the scales of one configured set
  definitions <- stats::setNames(names(multi), vapply(multi, function(model) {
    find_cfa_model_set_key(names(model$factors), cfa_cfg)
  }, character(1)))
  expect_setequal(names(definitions), keys)
  for (key in names(definitions)) {
    set <- cfa_cfg$factor_analysis$sets[[match(key, keys)]]
    factors <- multi[[definitions[[key]]]]$factors
    expect_identical(names(factors),
                     intersect(as.character(cfa_codebook$scales$scale_key), as.character(unlist(set$scales))),
                     info = key)
    expect_equal(length(factors), as.integer(set$expected_factors), info = key)
    for (scale in names(factors)) {
      row <- match(scale, cfa_codebook$scales$scale_key)
      expect_identical(zm_item_columns(factors[[scale]], cfa_map),
                       as.character(cfa_codebook$scales$item_codes[[row]]),
                       info = paste(key, scale))
    }
    # the set key the report table reports is resolved from those factors
    expect_identical(find_cfa_model_set_key(names(factors), cfa_cfg), key)
  }
})

test_that("the fit settings are the plan's, unchanged for every model", {
  settings <- cfa_cfg$factor_analysis$cfa_settings
  expect_identical(settings$estimator, "WLSMV")
  expect_true(settings$ordered)
  expect_true(settings$std_lv)
  expect_false(settings$orthogonal)
})

test_that("each model carries its registered reporting question, in model order", {
  expect_identical(
    unlist(cfa_define_models(cfa_input)$questions, use.names = TRUE),
    # the one-factor models in the codebook's order of their scales
    c(zm_security = "scale unidimensionality", zm_arousal = "scale unidimensionality",
      zm_power = "scale unidimensionality", zm_prestige = "scale unidimensionality",
      zm_achievement = "scale unidimensionality", asc_agg = "scale unidimensionality",
      asc_sub = "scale unidimensionality", asc_conv = "scale unidimensionality",
      sdo_dom = "scale unidimensionality",
      ums_achievement_intimacy = "ums", dopl_dominance_prestige = "dopl",
      social_motives_five_factor = "social motives", asc_three_factor = "asc",
      authoritarian_orientation_four_factor = "authoritarian orientation"))
})

# ---- The fourteen models, their reporting and the report tables --------

cfa_assessed <- cfa_run_models(cfa_input)
cfa_report <- cfa_reporting_of(cfa_assessed)
cfa_scales_tbl <- tabulate_cfa_scale_models(cfa_report, cfa_codebook)
cfa_sets_tbl <- tabulate_cfa_set_models(cfa_report, cfa_codebook, cfa_cfg)

test_that("the five definers supply fourteen assessed models and one status row each", {
  expect_equal(length(cfa_assessed$models), 14L)
  expect_equal(nrow(cfa_report$model_status), 14L)
  expect_true(all(cfa_report$model_status$converged))
  expect_true(all(cfa_report$model_status$admissible))
  expect_equal(nrow(cfa_scales_tbl), 9L)
  expect_equal(nrow(cfa_sets_tbl), 5L)
  expect_true(all(cfa_row_columns %in% names(cfa_scales_tbl)))
  expect_true(all(cfa_row_columns %in% names(cfa_sets_tbl)))
  # the scale rows carry no set; every set row names its configured set
  expect_true(all(is.na(cfa_scales_tbl$set)))
  expect_identical(cfa_sets_tbl$set, c("ums", "dopl", "motives", "asc", "outcomes"))
  expect_identical(cfa_scales_tbl$n_factors, rep(1L, 9L))
  expect_identical(cfa_sets_tbl$n_factors, c(2L, 2L, 5L, 3L, 4L))
  expect_true(all(cfa_scales_tbl$index_version == ap4_fit_index_version()))
})

test_that("the technical supplement shows the syntax that was actually fitted", {
  syntax <- cfa_report$technical_supplement[[9L]]$syntax
  expect_identical(cfa_report$model_status$model[9L], "sdo_dom")
  expect_identical(syntax, paste0(
    "sdo_dom =~ SDO_D_1 + SDO_D_2 + SDO_D_3 + SDO_D_4 + ",
    "SDO_D_5_r + SDO_D_6_r + SDO_D_7_r + SDO_D_8_r"))
  # the source labels are gone from the fitted syntax, so the supplement cannot
  # show a model lavaan never saw
  expect_false(grepl("SDO-D_1", syntax, fixed = TRUE))
})

test_that("the reporting list-column keeps the publication-facing tables", {
  reporting <- cfa_sets_tbl$reporting[[5L]]
  expect_identical(names(reporting), c("loadings", "factor_correlations", "residuals",
                                       "ranked_residuals", "ave", "issues"))
  expect_identical(names(reporting$loadings), c(
    "model", "model_label", "factor", "factor_label", "item", "item_label",
    "estimate_unstd", "se_unstd", "estimate_std", "se_std"))
  expect_identical(names(reporting$residuals), c(
    "model", "model_label", "item_1", "item_1_label", "item_2", "item_2_label",
    "bentler_residual", "std_residual", "abs_std_residual", "rank_abs_std_residual"))
  expect_equal(nrow(reporting$loadings), 27L)
  expect_equal(nrow(reporting$factor_correlations), 6L)
  expect_equal(nrow(reporting$ave), 4L)
  # the ranking is by the absolute standardised residual, and the full output
  # is the same set of pairs in the model's own order
  expect_identical(reporting$ranked_residuals$rank_abs_std_residual,
                   seq_len(nrow(reporting$ranked_residuals)))
  expect_setequal(paste(reporting$residuals$item_1, reporting$residuals$item_2),
                  paste(reporting$ranked_residuals$item_1, reporting$ranked_residuals$item_2))
  expect_false(is.unsorted(rev(reporting$ranked_residuals$abs_std_residual)))
  # the projection re-labels already extracted evidence; it estimates nothing
  supplement <- cfa_report$technical_supplement[[14L]]
  expect_identical(reporting$loadings$estimate_unstd,
                   as.numeric(supplement$available_estimates$loadings$estimate_unstd))
  expect_identical(reporting$residuals$bentler_residual,
                   as.numeric(supplement$full_residual_output$residual_correlation))
})

# ---- One planned model unavailable -------------------------------------------

test_that("a failed fit stays a row with every table column, its text and placeholders", {
  attempts <- 0L
  failing <- "lavaan refused this deliberately broken fit"
  # The ninth call is the SDO-D scale model, the fourteenth the four-factor
  # authoritarian-orientation set: one unavailable model in each table.
  rlang::local_bindings(
    ap4_capture_cfa_fit = function(expression) {
      attempts <<- attempts + 1L
      if (attempts %in% c(9L, 14L)) {
        return(list(succeeded = FALSE, fit = NULL, warnings = character(0), error = failing))
      }
      list(succeeded = TRUE, fit = suppressWarnings(force(expression)),
           warnings = character(0), error = NA_character_)
    },
    .env = environment(fit_cfa_models)
  )
  assessed <- cfa_run_models(cfa_input)
  report <- cfa_reporting_of(assessed)
  scales <- tabulate_cfa_scale_models(report, cfa_codebook)
  sets <- tabulate_cfa_set_models(report, cfa_codebook, cfa_cfg)

  expect_equal(nrow(scales), 9L)
  expect_equal(nrow(sets), 5L)
  expect_true(all(cfa_row_columns %in% names(scales)))
  expect_true(all(cfa_row_columns %in% names(sets)))

  for (case in list(list(table = scales, failed = 9L, ok = 1L),
                    list(table = sets, failed = 5L, ok = 1L))) {
    row <- case$table[case$failed, ]
    good <- case$table[case$ok, ]
    expect_identical(row$error, failing)
    expect_identical(row$note, failing)
    expect_false(row$converged)
    expect_false(row$admissible)
    for (index in c("chisq", "df", "pvalue", "cfi", "tli", "rmsea",
                    "rmsea_lo", "rmsea_hi", "srmr")) {
      expect_true(is.na(row[[index]]), info = index)
    }
    expect_identical(row$index_version, ap4_fit_index_version())
    # placeholder list-columns keep the schema instead of becoming NULL
    expect_equal(nrow(row$loadings[[1L]]), 0L)
    expect_identical(names(row$loadings[[1L]]), names(good$loadings[[1L]]))
    placeholder <- row$reporting[[1L]]
    expect_identical(names(placeholder), names(good$reporting[[1L]]))
    for (component in c("loadings", "factor_correlations", "residuals", "ranked_residuals", "ave")) {
      expect_equal(nrow(placeholder[[component]]), 0L, info = component)
      expect_identical(names(placeholder[[component]]), names(good$reporting[[1L]][[component]]))
    }
    expect_identical(placeholder$issues$component, "fit")
    expect_identical(placeholder$issues$message, failing)
    # and the model that did fit is still a full row
    expect_true(good$converged)
    expect_gt(nrow(good$reporting[[1L]]$loadings), 0L)
  }

  # the residual file remains a path with the established CSV schema
  path <- withr::local_tempfile(fileext = ".csv")
  file_cfg <- cfa_cfg
  file_cfg$factor_analysis$cfa_reporting$residual_data_file <- path
  written <- write_full_cfa_residual_output(scales, sets, file_cfg)
  expect_true(file.exists(written))
  header <- strsplit(readLines(written, n = 1L), ",", fixed = TRUE)[[1]]
  expect_identical(header, c(
    "model", "model_label", "item_1", "item_1_label", "item_2", "item_2_label",
    "bentler_residual", "std_residual", "abs_std_residual", "rank_abs_std_residual"))
  residuals <- readr::read_csv(written, show_col_types = FALSE, progress = FALSE)
  expect_false(any(residuals$model %in% c("sdo_dom", "authoritarian_orientation_four_factor")))
  expect_true("zm_achievement" %in% residuals$model)

  # the real report helpers render the one fit table with the failed rows
  # present: one table, the message as a specific note on the model and
  # reproduced in full in the supplement
  fit_table <- rh_cfa_fit_table(scales, sets[0, ], sets, rh_labels(), engine = "kable")
  expect_type(fit_table, "character")
  fit_text <- paste(fit_table, collapse = "\n")
  expect_match(fit_text, ap4_fit_index_version(), fixed = TRUE)
  expect_match(fit_text, "The estimation software issued a message", fixed = TRUE)
  expect_match(fit_text, "<sup>a</sup>", fixed = TRUE)
  messages <- rh_cfa_estimation_messages(scales, sets, rh_labels())
  expect_true(all(grepl(failing, messages$message, fixed = TRUE)))
  expect_setequal(messages$model, c(scales$model[9L], sets$model[5L]))
  entries <- rh_cfa_reporting_models(scales, sets)
  expect_length(entries, 14L)
  expect_false(isTRUE(entries[[9L]]$converged))
  expect_true(isTRUE(entries[[1L]]$converged) && isTRUE(entries[[1L]]$admissible))
})

# ---- Wiring: the main map ----------------------------------------------------

cfa_manifest <- withr::with_dir(
  cfa_root,
  withr::with_envvar(
    c(ZM_PROFILE = "smoke", ZM_DATA = "synthetic"),
    withCallingHandlers(
      targets::tar_manifest(callr_function = NULL, envir = new.env(parent = globalenv())),
      warning = function(w) {
        if (grepl("built under R version|unter R Version", conditionMessage(w))) {
          invokeRestart("muffleWarning")
        }
      }
    )
  )
)
cfa_command_of <- function(name) {
  command <- cfa_manifest$command[cfa_manifest$name == name]
  if (length(command) != 1L) stop("target '", name, "' not found exactly once in the manifest")
  gsub("\\s+", " ", command)
}

test_that("the one confirmatory target pipes the five definers over the rounded CFA input", {
  expect_identical(cfa_command_of("data_cfa_input"),
                   paste("round_imputed_items_for_cfa(data_descriptive_reliability,",
                         "build_filled_cells_table(imputation_reporting_data), analysis_inputs$codebook)"))
  definitions <- c("define_subscale_models", "define_ums_dopl_models",
                   "define_social_motives_model", "define_asc_model",
                   "define_auth_orientation_model")
  command <- cfa_command_of("cfa_models")
  expect_match(command, "data_cfa_input", fixed = TRUE)
  for (definition in definitions) expect_match(command, definition, fixed = TRUE)
  # the order in which they are piped is the row order of the reporting tables;
  # that order is asserted on the produced models themselves, above.
  expect_match(command, "fit_cfa_models", fixed = TRUE)
  expect_match(command, "assess_cfa_models", fixed = TRUE)
  expect_false(grepl("data_descriptive_reliability", command, fixed = TRUE))
  expect_identical(cfa_command_of("confirmatory_factor_structure_reporting_data"),
                   "{ build_cfa_reporting_data(cfa_models, analysis_inputs$config) }")
})

test_that("the residual file holds every pair of the reporting data, with labels and ranks", {
  expect_identical(
    cfa_command_of("confirmatory_factor_structure_residuals_file"),
    paste("write_full_cfa_residual_output(tabulate_cfa_scale_models(confirmatory_factor_structure_reporting_data,",
          "analysis_inputs$codebook), tabulate_cfa_set_models(confirmatory_factor_structure_reporting_data,",
          "analysis_inputs$codebook, analysis_inputs$config), analysis_plan)"))

  file_cfg <- cfa_cfg
  file_cfg$factor_analysis$cfa_reporting$residual_data_file <- withr::local_tempfile(fileext = ".csv")
  written <- write_full_cfa_residual_output(
    tabulate_cfa_scale_models(cfa_report, cfa_codebook),
    tabulate_cfa_set_models(cfa_report, cfa_codebook, cfa_cfg), file_cfg)
  residuals <- readr::read_csv(written, show_col_types = FALSE, progress = FALSE)
  expect_identical(
    names(residuals),
    c("model", "model_label", "item_1", "item_1_label", "item_2", "item_2_label",
      "bentler_residual", "std_residual", "abs_std_residual", "rank_abs_std_residual"))
  expect_identical(nrow(residuals), nrow(cfa_report$full_residual_output))
})

test_that("reliability and the exploratory factor analysis read the common table, not the rounded CFA input", {
  for (name in c("scale_reliability", "efa_item_sets", "ap4_summary_files")) {
    expect_false(grepl("data_cfa_input", cfa_command_of(name), fixed = TRUE), info = name)
  }
})

