# R/config.R: analysis plan, profiles, codebook, simulation truth

local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) {
    parent <- dirname(dir)
    if (identical(parent, dir)) stop("project root not found from ", getwd())
    dir <- parent
  }
  source(file.path(dir, "R", "config.R"), local = FALSE)
})

root <- zm_root()
plan_path <- file.path(root, "config", "analysis_plan.yaml")
truth_path <- file.path(root, "config", "simulation_truth.yaml")
plan_yaml <- yaml::read_yaml(plan_path)

test_that("zm_root() finds the directory that holds config/analysis_plan.yaml", {
  expect_true(file.exists(file.path(root, "config", "analysis_plan.yaml")))
  expect_true(file.exists(file.path(root, "R", "config.R")))
})

test_that("zm_normalise_names() maps CSV names onto SAV names and codebook codes", {
  expect_equal(zm_normalise_names("SDO-D_5_r"), "SDO_D_5_r")
  expect_equal(zm_normalise_names("Duration (in seconds)"), "Duration__in_seconds_")
  expect_equal(zm_normalise_names("UMS_ach_1"), "UMS_ach_1")
  expect_equal(zm_normalise_names(zm_normalise_names("SDO-D_1")), "SDO_D_1")
})

test_that("zm_item_column_map() resolves the codebook's source labels to the executable columns", {
  cb <- zm_codebook(zm_config(profile = "full", path = plan_path))
  map <- zm_item_column_map(cb)
  sdo_labels <- paste0("SDO-D_", c(1:4, paste0(5:8, "_r")))
  expect_true(all(sdo_labels %in% names(map)))
  expect_equal(unname(map[sdo_labels]), paste0("SDO_D_", c(1:4, paste0(5:8, "_r"))))
  expect_equal(zm_item_columns(sdo_labels, map), paste0("SDO_D_", c(1:4, paste0(5:8, "_r"))))
  expect_true(all(unlist(cb$scales$item_codes) %in% unname(map)))
})

test_that("zm_item_columns() refuses an unknown label and a duplicate resolution", {
  map <- c("SDO-D_1" = "SDO_D_1", "SDO-D_2" = "SDO_D_2")
  expect_equal(zm_item_columns(c("SDO-D_2", "SDO-D_1"), map), c("SDO_D_2", "SDO_D_1"))
  expect_error(zm_item_columns(c("SDO-D_1", "SDO_D_9"), map),
               "Item label\\(s\\) without an executable column: SDO_D_9")
  expect_error(zm_item_columns(c("SDO-D_1", "SDO-D_1"), map),
               "Item labels resolve to the same column: SDO_D_1")
  expect_error(zm_item_column_map(list(items = list(
    item_code = c("SDO_D_1", "SDO_D_1"), source_label = c("SDO-D_1", "SDO-D_2")))),
    "must both be unique")
})

test_that("every measurement column the map resolves to exists in the synthetic intake header", {
  cb <- zm_codebook(zm_config(profile = "full", path = plan_path))
  map <- zm_item_column_map(cb)
  scale_labels <- names(map)[map %in% unlist(cb$scales$item_codes)]
  header <- scan(file.path(root, "data", "intake", "synthetic.csv"), what = "",
                 sep = ",", nlines = 1, quiet = TRUE)
  expect_equal(length(scale_labels), length(unlist(cb$scales$item_codes)))
  expect_true(all(zm_item_columns(scale_labels, map) %in% header))
  expect_true(all(paste0("SDO_D_", c(1:4, paste0(5:8, "_r"))) %in% header))
})

test_that("zm_config() applies the profile overrides from the YAML", {
  for (prof in names(plan_yaml$profiles)) {
    analysis_plan <- zm_config(profile = prof, path = plan_path)
    p <- plan_yaml$profiles[[prof]]
    expect_equal(analysis_plan$profile_name, prof)
    expect_equal(analysis_plan$regression$iter_per_chain, p$regression_iter_per_chain)
    expect_equal(analysis_plan$regression$warmup, p$regression_warmup)
    expect_equal(analysis_plan$network$B, p$network_B)
    expect_equal(analysis_plan$network$iter, p$network_iter)
    expect_equal(analysis_plan$root, root)
  }
  full <- zm_config(profile = "full", path = plan_path)
  smoke <- zm_config(profile = "smoke", path = plan_path)
  expect_false(identical(full$network$B, smoke$network$B))
})

test_that("zm_config() respects the ZM_PROFILE environment variable", {
  withr::with_envvar(c(ZM_PROFILE = "smoke"), {
    expect_equal(zm_config(path = plan_path)$profile_name, "smoke")
  })
  withr::with_envvar(c(ZM_PROFILE = ""), {
    expect_equal(zm_config(path = plan_path)$profile_name, "full")
  })
})

test_that("zm_config() rejects unknown profiles and missing required fields", {
  expect_error(zm_config(profile = "nope", path = plan_path), "Unknown profile")

  broken <- plan_yaml
  broken$regression$chains <- NULL
  tmp <- withr::local_tempfile(fileext = ".yaml")
  yaml::write_yaml(broken, tmp)
  expect_error(zm_config(profile = "full", path = tmp), "regression\\$chains")

  broken2 <- plan_yaml
  broken2$profiles$smoke$network_B <- NULL
  tmp2 <- withr::local_tempfile(fileext = ".yaml")
  yaml::write_yaml(broken2, tmp2)
  expect_error(zm_config(profile = "smoke", path = tmp2), "network_B")

  # The gender prior is a required key.
  broken4 <- plan_yaml
  broken4$priors$gender_sd <- NULL
  tmp4 <- withr::local_tempfile(fileext = ".yaml")
  yaml::write_yaml(broken4, tmp4)
  expect_error(zm_config(profile = "full", path = tmp4), "priors\\$gender_sd")
})

test_that("the prediction table lists every motive for every outcome with a known sign", {
  table <- zm_config(profile = "full", path = plan_path)$predictions$table
  signs <- unlist(table)
  expect_equal(length(signs), 20L)
  expect_equal(sum(signs %in% c("+", "-")), 16L)
  expect_equal(sum(signs == "±"), 4L)
  load_with <- function(change) {
    broken <- plan_yaml
    broken$predictions$table <- change(broken$predictions$table)
    # written beside the real plan, so that meta$codebook_dir still resolves
    path <- withr::local_tempfile(tmpdir = dirname(plan_path), fileext = ".yaml",
                                  .local_envir = parent.frame())
    yaml::write_yaml(broken, path)
    zm_config(profile = "full", path = path)
  }
  expect_error(load_with(function(t) { t$asc_conv$zm_power <- "x"; t }), "unknown signs: x")
  expect_error(load_with(function(t) { t$asc_conv$zm_power <- NULL; t }),
               "row 'asc_conv' must list every motive")
  expect_error(load_with(function(t) { t$sdo_dom <- NULL; t }), "one row per outcome")
})

test_that("every prior width of the plan is checked once, when the plan is loaded", {
  # A width that is not a single positive number, and degrees of freedom at or
  # below 2, stop the load with a message naming the field.
  bad_width <- list(-1, 0, c(0.1, 0.2), "0.2")
  fields <- list(
    c("priors", "slope_sd_primary"), c("priors", "gender_sd"), c("priors", "intercept_sd"),
    c("priors", "sigma_sd"), c("sensitivity", "student_t", "sigma_scale_prior_sd")
  )
  for (field in fields) {
    for (value in bad_width) {
      broken <- plan_yaml
      broken[[field]] <- value
      path <- withr::local_tempfile(fileext = ".yaml")
      yaml::write_yaml(broken, path)
      label <- paste(field, collapse = "$")
      expect_error(zm_config(profile = "full", path = path),
                   gsub("$", "\\$", label, fixed = TRUE), info = label)
      expect_error(zm_config(profile = "full", path = path), "single positive number", info = label)
    }
  }
  # an absent Student-t scale width is not a required key of its own, so the
  # width check is what names it
  broken_absent <- plan_yaml
  broken_absent$sensitivity$student_t$sigma_scale_prior_sd <- NULL
  path <- withr::local_tempfile(fileext = ".yaml")
  yaml::write_yaml(broken_absent, path)
  expect_error(zm_config(profile = "full", path = path), "sigma_scale_prior_sd")

  # every element of the sweep, not only the primary width
  broken_sweep <- plan_yaml
  broken_sweep$priors$slope_sd_sweep[[2]] <- -0.2
  path <- withr::local_tempfile(fileext = ".yaml")
  yaml::write_yaml(broken_sweep, path)
  expect_error(zm_config(profile = "full", path = path), "priors\\$slope_sd_sweep\\[2\\]")

  # the Student-t degrees of freedom: a single number greater than 2
  for (value in list(NULL, 2, 1.5, c(4, 5), "4")) {
    broken_nu <- plan_yaml
    broken_nu$sensitivity$student_t$nu_fixed <- value
    path <- withr::local_tempfile(fileext = ".yaml")
    yaml::write_yaml(broken_nu, path)
    expect_error(zm_config(profile = "full", path = path), "student_t\\$nu_fixed")
    expect_error(zm_config(profile = "full", path = path), "greater than 2")
  }
})

test_that("every plan value the implementation relies on is checked once, at load", {
  # A broken plan stops the load with a message naming the field.
  broken_stops <- function(edit, pattern) {
    broken <- edit(plan_yaml)
    path <- withr::local_tempfile(fileext = ".yaml")
    yaml::write_yaml(broken, path)
    expect_error(zm_config(profile = "full", path = path), pattern)
  }
  # AP1: the flow status of a quota-full exit
  broken_stops(function(x) { x$exclusions$survey_status_quota_full <- ""; x },
               "exclusions\\$survey_status_quota_full")
  # AP1: the smallest divers group the analysis keeps, a positive whole number
  for (value in list(0, -1, 2.5, c(10, 11), "11")) {
    broken_stops(function(x) { x$exclusions$gender_divers_min_n <- value; x },
                 "exclusions\\$gender_divers_min_n")
    broken_stops(function(x) { x$exclusions$gender_divers_min_n <- value; x },
                 "single positive whole number")
  }
  # and it is a required key: an absent one stops the load naming the field
  broken_stops(function(x) { x$exclusions$gender_divers_min_n <- NULL; x },
               "gender_divers_min_n")
  # AP3: the gender contrasts, the free-text comment, the standardisation scope
  broken_stops(function(x) { x$gender$contrasts <- "sum"; x }, "gender\\$contrasts")
  broken_stops(function(x) { x$free_text_comment$handling <- "deleted on import"; x },
               "free_text_comment\\$handling")
  broken_stops(function(x) { x$missing_data$standardise_within_model <- FALSE; x },
               "missing_data\\$standardise_within_model")
  # AP6: the validity gate, its ESS target and the labels of the prior sweep
  broken_stops(function(x) { x$regression$validity_gate <- NULL; x },
               "regression\\$validity_gate")
  for (field in c("rhat_max", "divergences_max", "treedepth_hits_max", "bfmi_min",
                  "max_ess_doublings", "adapt_delta_retry", "max_treedepth_retry")) {
    broken_stops(function(x) { x$regression$validity_gate[[field]] <- NULL; x },
                 paste0("validity_gate\\$", field))
  }
  broken_stops(function(x) { x$regression$ess_target <- "many"; x }, "regression\\$ess_target")
  broken_stops(function(x) { x$priors$sweep_labels <- NULL; x }, "priors\\$sweep_labels")
  broken_stops(function(x) { x$priors$sweep_labels[["0.4"]] <- NULL; x },
               "priors\\$sweep_labels' has no label for the swept width 0.4")
  # AP6: the label of every fit outside the outcome x prior grid
  for (role in c("student_refit", "multivariate")) {
    broken_stops(function(x) { x$regression$fit_roles[[role]] <- NULL; x },
                 paste0("fit_roles\\$", role))
  }
  # AP8 network: the edge-inclusion prior and every value of its sweep
  for (bad in list("0.5", 0, 1, -0.1, c(0.4, 0.6))) {
    broken_stops(function(x) { x$network$g_prior <- bad; x }, "network\\$g_prior")
  }
  broken_stops(function(x) { x$network$g_prior_sweep <- list(0.25, "x", 0.75); x },
               "network\\$g_prior_sweep\\[\\[2\\]\\]")
  broken_stops(function(x) { x$network$g_prior_sweep <- list(); x }, "network\\$g_prior_sweep")
  # AP3 fill: the drop thresholds, the reported interval, the prior widths and
  # the three demographics
  for (field in c("items_per_scale_more_than", "demographics_missing_at_least")) {
    for (bad in list(-1, 1.5, "two")) {
      broken_stops(function(x) { x$missing_data$fill$drop[[field]] <- bad; x },
                   paste0("fill\\$drop\\$", field))
    }
  }
  for (bad in list(0, 1, 1.5, "0.95")) {
    broken_stops(function(x) { x$missing_data$fill$interval <- bad; x }, "fill\\$interval")
  }
  broken_stops(function(x) { x$missing_data$fill$items$priors$sd_sd <- 0; x },
               "fill\\$items\\$priors\\$sd_sd")
  broken_stops(function(x) { x$missing_data$fill$demographics$demo_age$priors$sigma_sd <- -1; x },
               "fill\\$demographics\\$demo_age\\$priors\\$sigma_sd")
  broken_stops(function(x) { x$missing_data$fill$demographics$order <- c("demo_age", "demo_hh_members"); x },
               "fill\\$demographics\\$order")
})

test_that("the loader reads the variable names of the codebook, once", {
  # The plan lists keys; the scale codebook and YAML covariates section name
  # the standardised column of each, and zm_config() does that lookup.
  analysis_plan <- zm_config(profile = "full", path = plan_path)
  cb <- zm_codebook(analysis_plan)
  # the nodes as a set of the plan, in the codebook's order
  expect_setequal(as.character(analysis_plan$network$node_keys), as.character(plan_yaml$network$nodes))
  expect_identical(as.character(analysis_plan$network$node_keys), as.character(cb$scales$scale_key))
  expect_identical(analysis_plan$network$nodes, zm_z_col(analysis_plan$network$node_keys, cb))
  expect_identical(analysis_plan$regression$covariate_keys, as.character(plan_yaml$regression$covariates))
  expect_identical(analysis_plan$regression$covariates, c("age_z", "gender", "income_z"))
  expect_identical(analysis_plan$standardisation$covariates_z, as.character(cb$covariates$covariate))
  expect_identical(unname(analysis_plan$regression$term_keys[c("asc_agg_z", "zm_power_z", "income_z")]),
                   c("asc_agg", "zm_power", "income"))
  # a scale the codebook does not know stops the load
  broken <- plan_yaml
  broken$regression$motives <- c(as.character(broken$regression$motives), "zm_unknown")
  # written beside the real plan, so that meta$codebook_dir still resolves
  path <- withr::local_tempfile(tmpdir = dirname(plan_path), fileext = ".yaml")
  yaml::write_yaml(broken, path)
  expect_error(zm_config(profile = "full", path = path), "absent from codebook_scales.csv")
})

test_that("zm_key_of_z_col() inverts zm_z_col() and leaves other names alone", {
  analysis_plan <- zm_config(profile = "full", path = plan_path)
  cb <- zm_codebook(analysis_plan)
  expect_identical(zm_key_of_z_col(cb$scales$z_col, cb), as.character(cb$scales$scale_key))
  expect_identical(zm_key_of_z_col(cb$covariates$z_col, cb), as.character(cb$covariates$covariate))
  expect_identical(zm_key_of_z_col(c("gendermale", "Intercept"), cb), c("gendermale", "Intercept"))
})

test_that("zm_sweep_key() keys a swept width as the plan writes it", {
  expect_identical(zm_sweep_key(c(0.10, 0.20, 0.40)), c("0.1", "0.2", "0.4"))
})

test_that("zm_pipeline_resources() splits the cores between the two controllers", {
  analysis_plan <- zm_config(profile = "full", path = plan_path)
  r <- zm_pipeline_resources(analysis_plan, reserve = 2L)
  expect_identical(r$cores, zm_detect_cores(reserve = 2L))
  expect_identical(r$light_workers, min(12L, r$cores))
  expect_equal(r$heavy_workers, max(1L, floor(r$cores / analysis_plan$regression$cores)))
  expect_gte(r$heavy_workers, 1)
})

test_that("ZM_CORES caps the detected cores and refuses a non-positive value", {
  uncapped <- withr::with_envvar(c(ZM_CORES = NA), zm_detect_cores(reserve = 0L))
  expect_identical(withr::with_envvar(c(ZM_CORES = "1"), zm_detect_cores(reserve = 0L)), 1L)
  expect_identical(withr::with_envvar(c(ZM_CORES = as.character(uncapped + 5L)), zm_detect_cores(reserve = 0L)), uncapped)
  expect_error(withr::with_envvar(c(ZM_CORES = "0"), zm_detect_cores()), "ZM_CORES")
  expect_error(withr::with_envvar(c(ZM_CORES = "many"), zm_detect_cores()), "ZM_CORES")
})

test_that("zm_config() turns the income bands into a named numeric vector in band order", {
  analysis_plan <- zm_config(profile = "full", path = plan_path)
  bands <- analysis_plan$income$band_representative
  expect_type(bands, "double")
  expect_equal(length(bands), length(plan_yaml$income$band_representative))
  expect_equal(names(bands), as.character(seq_along(bands)))
  expect_equal(unname(bands), unlist(plan_yaml$income$band_representative)[names(bands)] |> unname())
  expect_true(all(diff(bands) > 0))
})

test_that("zm_config() reads the age bands and the income bands per household member", {
  analysis_plan <- zm_config(profile = "full", path = plan_path)
  age <- analysis_plan$age_bands
  expect_identical(age$first_years, c(18, seq(25, 65, by = 5)))
  expect_identical(age$last_year, 69)
  expect_identical(age$labels[c(1, 2, 10)], c("18–24", "25–29", "65–69"))
  expect_identical(age$representative, "midpoint")
  income <- analysis_plan$income_bands_per_member
  expect_identical(income$limits, c(357, 500, 630, 800, 1050, 1400, 2000, 2800, 12500))
  expect_length(income$labels, length(income$limits) - 1L)
  expect_identical(income$labels[c(1, 2, 8)], c("<500", "500–<630", "2800+"))
  expect_identical(income$representative, "geometric_mean")
})

test_that("unusable age or income bands stop configuration loading", {
  expect_bands_error <- function(edit, pattern) {
    plan <- edit(yaml::read_yaml(plan_path))
    expect_error(zm_normalise_covariate_bands_technical(plan, "analysis_plan.yaml"), pattern)
  }
  expect_bands_error(function(p) { p$age_bands$first_years <- c(18, 30, 25); p }, "age_bands")
  expect_bands_error(function(p) { p$age_bands$last_year <- 60; p }, "age_bands")
  expect_bands_error(function(p) { p$age_bands$labels <- p$age_bands$labels[-1]; p }, "age_bands\\$labels")
  expect_bands_error(function(p) { p$age_bands$representative <- "mean"; p }, "age_bands\\$representative")
  expect_bands_error(function(p) { p$income_bands_per_member$limits <- c(500, 357); p }, "limits")
  expect_bands_error(function(p) { p$income_bands_per_member$labels <- "all"; p }, "income_bands_per_member\\$labels")
  expect_bands_error(function(p) { p$income_bands_per_member$representative <- "midpoint"; p },
                     "income_bands_per_member\\$representative")
})

test_that("the declared primary family must match the supported Gaussian analysis", {
  expect_identical(zm_config(path = plan_path)$regression$family, "gaussian")
  for (family in list(NULL, "student", "binomial")) {
    changed <- plan_yaml
    changed$regression$family <- family
    path <- withr::local_tempfile(fileext = ".yaml")
    yaml::write_yaml(changed, path)
    expect_error(zm_config(path = path), "regression\\$family")
  }
})

test_that("zm_codebook() returns normalised item codes and list-columns per scale", {
  analysis_plan <- zm_config(profile = "full", path = plan_path)
  cb <- zm_codebook(analysis_plan)
  expect_named(cb, c("items", "scales", "factors", "covariates"))
  scales_csv <- readr::read_csv(file.path(root, plan_yaml$meta$codebook_dir, "codebook_scales.csv"),
                                show_col_types = FALSE, progress = FALSE)
  expect_equal(nrow(cb$scales), nrow(scales_csv))
  expect_type(cb$scales$item_codes, "list")
  expect_type(cb$scales$reverse_items, "list")
  expect_equal(lengths(cb$scales$item_codes), cb$scales$item_count)
  all_codes <- unlist(cb$scales$item_codes)
  expect_false(any(grepl("-", all_codes, fixed = TRUE)))
  expect_false(any(grepl("-", cb$items$item_code, fixed = TRUE)))
  expect_true(all(all_codes %in% cb$items$item_code))
  for (i in seq_len(nrow(cb$scales))) {
    expect_true(all(cb$scales$reverse_items[[i]] %in% cb$scales$item_codes[[i]]))
  }
  sdo <- cb$scales[cb$scales$scale_key == "sdo_dom", ]
  expect_true("SDO_D_5_r" %in% sdo$reverse_items[[1]])
  expect_true("SDO_D_1" %in% sdo$item_codes[[1]])
  ach <- cb$scales[cb$scales$scale_key == "zm_achievement", ]
  expect_length(ach$reverse_items[[1]], 0)
  # reverse flags agree between the item table and the scale table
  rev_from_items <- cb$items$item_code[cb$items$reverse_keyed]
  expect_setequal(unlist(cb$scales$reverse_items), rev_from_items)
})

# A copy of the three codebooks whose scale table carries the `order` values
# given, and a copy of the plan (changed by `edit_plan`) beside it, so that the
# loader runs against an order of the test's choosing. The rows of the scale
# table stay where they are in the file.
codebook_order_root <- function(order_values, edit_plan = identity, env = parent.frame()) {
  base <- withr::local_tempdir(.local_envir = env)
  dir.create(file.path(base, "config"))
  dir.create(file.path(base, "books"))
  books <- file.path(root, plan_yaml$meta$codebook_dir)
  for (name in c("codebook_items.csv", "codebook_scales.csv", "codebook_factors.csv")) {
    file.copy(file.path(books, name), file.path(base, "books", name))
  }
  scales_path <- file.path(base, "books", "codebook_scales.csv")
  scales <- utils::read.csv(scales_path, colClasses = "character", check.names = FALSE)
  scales$order <- as.character(order_values(scales$scale_key))
  utils::write.csv(scales, scales_path, row.names = FALSE, na = "")
  plan <- edit_plan(plan_yaml)
  plan$meta$codebook_dir <- "books"
  path <- file.path(base, "config", "analysis_plan.yaml")
  yaml::write_yaml(plan, path)
  path
}
# `order` values that rank the scales as `keys` lists them
rank_as <- function(keys) function(scale_keys) match(scale_keys, keys)

test_that("numeric covariate columns come from YAML without a covariate CSV", {
  change_columns <- function(plan) {
    plan$covariates$age$z_col <- "age_for_model"
    plan$covariates$age$z_col_all <- "age_for_all"
    plan$covariates$age$z_col_known_gender <- "age_for_regressions"
    plan
  }
  path <- codebook_order_root(function(keys) seq_along(keys), change_columns)
  analysis_plan <- zm_config(profile = "full", path = path)
  codebook <- zm_codebook(analysis_plan)
  expect_false(file.exists(file.path(dirname(dirname(path)), "books", "codebook_covariates.csv")))
  expect_identical(analysis_plan$regression$covariates, c("age_for_model", "gender", "income_z"))
  expect_identical(zm_z_col("age", codebook, "all"), "age_for_all")
  expect_identical(zm_z_col("age", codebook, "known_gender"), "age_for_regressions")
  expect_identical(zm_key_of_z_col("age_for_model", codebook), "age")
  expect_identical(codebook$covariates$label, c("Age band", "Income band per household member"))
})

test_that("missing or unusable YAML covariate mappings stop configuration loading", {
  edits <- list(
    function(plan) { plan$covariates <- NULL; plan },
    function(plan) { plan$covariates$age$z_col_known_gender <- NULL; plan },
    function(plan) { plan$covariates$income$z_col <- ""; plan },
    function(plan) { plan$covariates$income$z_col_all <- plan$covariates$age$z_col_all; plan }
  )
  messages <- c("YAML covariates section", "YAML covariates$age$z_col_known_gender",
                "YAML covariates$income$z_col", "unique non-empty column names")
  for (i in seq_along(edits)) {
    path <- codebook_order_root(function(keys) seq_along(keys), edits[[i]])
    expect_error(zm_config(profile = "full", path = path), messages[[i]], fixed = TRUE)
  }
})

codebook_order <- c("zm_security", "zm_arousal", "zm_power", "zm_prestige", "zm_achievement",
                    "asc_agg", "asc_sub", "asc_conv", "sdo_dom")

test_that("zm_codebook() returns the scales in the order of the codebook's `order` column", {
  # The codebook of the study: security, arousal, power, prestige, achievement,
  # then the four facets, although the file lists achievement first.
  analysis_plan <- zm_config(profile = "full", path = plan_path)
  cb <- zm_codebook(analysis_plan)
  expect_identical(cb$scales$scale_key, codebook_order)
  expect_identical(cb$scales$order, seq_len(9L))
  scales_csv <- utils::read.csv(file.path(root, plan_yaml$meta$codebook_dir, "codebook_scales.csv"),
                                colClasses = "character")
  expect_false(identical(scales_csv$scale_key, codebook_order))
  # Another `order` gives another order, with every column of a scale moving
  # with it.
  other <- c("sdo_dom", "zm_achievement", "asc_conv", "zm_prestige", "asc_sub", "zm_power", "asc_agg", "zm_arousal", "zm_security")
  cb_other <- zm_codebook(zm_config(profile = "full", path = codebook_order_root(rank_as(other))))
  expect_identical(cb_other$scales$scale_key, other)
  expect_identical(cb_other$scales$z_col, paste0(other, "_z"))
  expect_identical(cb_other$scales$label[cb_other$scales$scale_key == "zm_security"],
                   cb$scales$label[cb$scales$scale_key == "zm_security"])
  expect_identical(zm_order_scale_keys(c("asc_agg", "zm_security", "sdo_dom"), cb_other),
                   c("sdo_dom", "asc_agg", "zm_security"))
})

test_that("zm_codebook() refuses an `order` column that is not one whole rank per scale, 1 to 9", {
  broken <- list(
    "a whole number" = function(keys) c(1.5, 2:9),
    "a whole number" = function(keys) c(NA, 2:9),
    "differ between scales" = function(keys) c(1:8, 8),
    "1 to 9" = function(keys) c(1:8, 10)
  )
  for (i in seq_along(broken)) {
    path <- codebook_order_root(broken[[i]])
    expect_error(zm_config(profile = "full", path = path), names(broken)[i], fixed = TRUE,
                 info = names(broken)[i])
  }
})

test_that("zm_config() puts every list of scales into the codebook's order, whatever the plan's order", {
  scramble <- function(plan) {
    plan$regression$outcomes <- rev(plan$regression$outcomes)
    plan$regression$motives <- c("zm_achievement", "zm_prestige", "zm_security", "zm_power", "zm_arousal")
    plan$network$nodes <- rev(plan$network$nodes)
    plan$factor_analysis$sets <- lapply(plan$factor_analysis$sets, function(set) {
      set$scales <- rev(set$scales)
      set
    })
    plan$confirmatory_models$models <- lapply(rev(plan$confirmatory_models$models), function(model) {
      model$factors <- rev(model$factors)
      model
    })
    plan$predictions$table <- lapply(rev(plan$predictions$table), rev)
    plan
  }
  check_lists <- function(analysis_plan, order_keys) {
    motives <- order_keys[order_keys %in% plan_yaml$regression$motives]
    outcomes <- order_keys[order_keys %in% plan_yaml$regression$outcomes]
    expect_identical(as.character(analysis_plan$regression$motives), motives)
    expect_identical(as.character(analysis_plan$regression$outcomes), outcomes)
    expect_identical(as.character(analysis_plan$network$node_keys), order_keys)
    expect_identical(as.character(analysis_plan$network$nodes), paste0(order_keys, "_z"))
    for (set in analysis_plan$factor_analysis$sets) {
      scales <- as.character(unlist(set$scales))
      expect_identical(scales, order_keys[order_keys %in% scales], info = set$key)
    }
    models <- analysis_plan$confirmatory_models$models
    for (model in models) {
      scales <- as.character(unlist(model$factors))
      expect_identical(scales, order_keys[order_keys %in% scales], info = model$key)
    }
    single <- vapply(models, function(model) length(model$factors) == 1L, logical(1))
    expect_identical(vapply(models[single], function(model) as.character(model$key), ""), order_keys)
    expect_identical(names(analysis_plan$predictions$table), outcomes)
    for (outcome in outcomes) expect_identical(names(analysis_plan$predictions$table[[outcome]]), motives)
    # the predictor side: the standardised motives in that order, then the
    # covariates in the plan's own order
    expect_identical(analysis_plan$regression$predictors,
                     paste(c(paste0(motives, "_z"), "age_z", "gender", "income_z"), collapse = " + "))
    expect_identical(unname(analysis_plan$regression$term_keys[seq_along(order_keys)]), order_keys)
  }
  # the study's codebook order, from a plan that writes every list otherwise
  ordered <- zm_config(profile = "full", path = codebook_order_root(rank_as(codebook_order), scramble))
  check_lists(ordered, codebook_order)
  expect_identical(ordered$regression$predictors,
                   "zm_security_z + zm_arousal_z + zm_power_z + zm_prestige_z + zm_achievement_z + age_z + gender + income_z")
  # the predictions themselves are unchanged by the order
  for (outcome in names(plan_yaml$predictions$table)) {
    for (motive in names(plan_yaml$predictions$table[[outcome]])) {
      expect_identical(ordered$predictions$table[[outcome]][[motive]], plan_yaml$predictions$table[[outcome]][[motive]])
    }
  }
  # another codebook order reorders every list, and the predictor string with it
  other <- c("zm_achievement", "zm_prestige", "zm_power", "zm_arousal", "zm_security", "sdo_dom", "asc_conv", "asc_sub", "asc_agg")
  check_lists(zm_config(profile = "full", path = codebook_order_root(rank_as(other), scramble)), other)
  # a list naming one scale twice stops the load
  twice <- function(plan) {
    plan$regression$motives <- c(plan$regression$motives, "zm_power")
    plan
  }
  expect_error(zm_config(profile = "full", path = codebook_order_root(rank_as(codebook_order), twice)),
               "regression$motives in analysis_plan.yaml names the scale(s) zm_power twice", fixed = TRUE)
})

test_that("the labels of the one-scale models and of the instrument sets come from the codebook", {
  analysis_plan <- zm_config(profile = "full", path = plan_path)
  cb <- zm_codebook(analysis_plan)
  models <- analysis_plan$confirmatory_models$models
  for (model in Filter(function(m) length(m$factors) == 1L, models)) {
    key <- as.character(unlist(model$factors))
    expect_identical(model$label, cb$scales$label[cb$scales$scale_key == key], info = key)
  }
  expect_identical(zm_label_scale_set(c("zm_achievement", "zm_security"), cb), "UMS-6 (intimacy and achievement)")
  expect_identical(zm_label_scale_set(c("zm_prestige", "zm_power"), cb), "DoPL-6 (dominance and prestige)")
  expect_identical(zm_label_scale_set("zm_arousal", cb), cb$scales$label[cb$scales$scale_key == "zm_arousal"])
  expect_true(is.na(zm_label_scale_set(c("zm_power", "sdo_dom"), cb)))
  # a one-scale model may not carry a label of its own
  labelled <- function(plan) {
    plan$confirmatory_models$models[[1]]$label <- "Intimacy"
    plan
  }
  expect_error(zm_config(profile = "full", path = codebook_order_root(rank_as(codebook_order), labelled)),
               "is one scale and carries a label", fixed = TRUE)
})

test_that("zm_codebook() loads the covariate table with model and population-specific columns", {
  analysis_plan <- zm_config(profile = "full", path = plan_path)
  cb <- zm_codebook(analysis_plan)
  expect_named(cb$covariates, c("covariate", "z_col", "label", "z_col_all", "z_col_known_gender"))
  expect_setequal(cb$covariates$covariate, as.character(unlist(analysis_plan$standardisation$covariates_z)))
  expect_false(anyNA(cb$covariates$z_col))
  expect_true(all(nzchar(cb$covariates$label)))
})

test_that("zm_z_col() reads a standardised column for every scale and covariate, and refuses an unknown key", {
  analysis_plan <- zm_config(profile = "full", path = plan_path)
  cb <- zm_codebook(analysis_plan)
  expect_equal(zm_z_col(cb$scales$scale_key, cb), cb$scales$z_col)
  expect_equal(zm_z_col(cb$covariates$covariate, cb), cb$covariates$z_col)
  expect_error(zm_z_col("not_a_variable", cb), "no standardised column for: not_a_variable")
})

test_that("zm_truth() returns correlation matrices with dimnames and consistent counts", {
  truth <- zm_truth(truth_path)
  m <- truth$motive_correlations$matrix
  expect_true(is.matrix(m))
  expect_equal(rownames(m), truth$motive_correlations$order)
  expect_equal(colnames(m), truth$motive_correlations$order)
  expect_equal(m, t(m))
  expect_equal(unname(diag(m)), rep(1, nrow(m)))
  r <- truth$outcome_residual_correlations$matrix
  expect_equal(rownames(r), truth$outcome_residual_correlations$order)
  expect_equal(r, t(r))
  expect_equal(truth$n_kept, truth$n_total - sum(unlist(truth$exclusions)))
  expect_true(is.numeric(truth$seed))
  expect_setequal(names(truth$true_beta), truth$outcome_residual_correlations$order)
})

test_that("zm_truth() rejects inconsistent exclusion counts", {
  broken <- yaml::read_yaml(truth_path)
  broken$n_kept <- broken$n_kept - 1
  tmp <- withr::local_tempfile(fileext = ".yaml")
  yaml::write_yaml(broken, tmp)
  expect_error(zm_truth(tmp), "n_kept")
})

test_that("zm_setup() returns TRUE invisibly and sets mc.cores", {
  old <- getOption("mc.cores")
  on.exit(options(mc.cores = old), add = TRUE)
  res <- withVisible(zm_setup(cores = 1L))
  expect_true(res$value)
  expect_false(res$visible)
  expect_equal(getOption("mc.cores"), 1L)
})


test_that("common-data column names come from the population-specific codebook fields", {
  cb <- zm_codebook(zm_config())
  expect_identical(zm_z_col(cb$scales$scale_key, cb, "all"), cb$scales$z_col_all)
  expect_identical(zm_z_col(cb$covariates$covariate, cb, "known_gender"), cb$covariates$z_col_known_gender)
  cb$scales$z_col_all[1] <- "explicit_all_sample_column"
  expect_identical(zm_z_col(cb$scales$scale_key[1], cb, "all"), "explicit_all_sample_column")
  expect_error(zm_z_col("not_a_variable", cb, "known_gender"), "no standardised column")
})


test_that("the exclusion age bounds are required, whole and ordered", {
  for (value in list(NULL, NA_real_, Inf, -1, 69.5, c(68, 69), 17)) {
    broken <- plan_yaml
    broken$exclusions$max_age <- value
    tmp <- withr::local_tempfile(fileext = ".yaml")
    yaml::write_yaml(broken, tmp)
    expect_error(zm_config(profile = "full", path = tmp), "exclusions\\$max_age")
  }
})
