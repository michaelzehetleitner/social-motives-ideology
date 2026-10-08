# R/ap3_fill.R: the one missing-value imputation before the scale
# scores. The fast tests check the excessive-missingness exclusion, the
# pass-through without gaps, the predictor rule and the shape of the
# imputed-cell record without fitting anything. The fitting tests run the whole
# chain once on a small planted frame with the cmdstanr backend and few draws:
# engineering validation, not publication inference.

local({
  root <- normalizePath(getwd())
  while (!file.exists(file.path(root, "config", "analysis_plan.yaml"))) root <- dirname(root)
  for (f in list.files(file.path(root, "R"), pattern = "\\.R$", full.names = TRUE)) source(f, local = FALSE)
})

root <- zm_root()
analysis_plan <- zm_config(profile = "smoke", path = file.path(root, "config", "analysis_plan.yaml"))
# tiny fits: 2 chains, 250 warmup + 250 post-warmup draws; every rule stays
analysis_plan$regression$chains <- 2L
analysis_plan$regression$cores <- 2L
analysis_plan$regression$warmup <- 250L
analysis_plan$regression$iter_per_chain <- 250L
# Engineering sampling limits for this small integration fixture; production
# thresholds and the ordered retry/failure rules are tested separately.
analysis_plan$regression$ess_target <- 40
analysis_plan$regression$validity_gate$rhat_max <- 1.10
cb <- zm_codebook(analysis_plan)
item_codes <- unlist(cb$scales$item_codes)
scale_items <- function(key) cb$scales$item_codes[[which(cb$scales$scale_key == key)]]
cells_columns <- c("respondent_id", "variable", "kind", "model", "value", "lower",
                   "upper", "n_fit_rows")

# A complete wide frame after the gender coding: 60 respondents, the nine scales
# with their real item codes, answers 1..6, two respondents without a gender,
# the three demographics.
make_fill_frame <- function(n = 60, seed = 11, gender_missing = c(5L, 40L)) {
  set.seed(seed)
  d <- tibble::tibble(respondent_id = seq_len(n))
  for (code in item_codes) d[[code]] <- sample(seq(analysis_plan$scales$response_min, analysis_plan$scales$response_max), n, replace = TRUE)
  d$demo_gender <- sample(c(1, 2), n, replace = TRUE)
  d$demo_gender[intersect(gender_missing, seq_len(n))] <- NA
  d$demo_age <- as.numeric(sample(18:80, n, replace = TRUE))
  d$demo_hh_members <- as.numeric(sample(1:8, n, replace = TRUE))
  d$demo_income_hh_net <- as.numeric(sample(1:13, n, replace = TRUE))
  prepare_gender(d, analysis_plan)
}

test_that("the exclusion removes only the respondents the imputation must not reach", {
  d <- make_fill_frame()
  agg <- scale_items("asc_agg")
  intim <- scale_items("zm_security")
  # one respondent with three missing items in one scale: excluded
  d[[agg[1]]][1] <- NA
  d[[agg[2]]][1] <- NA
  d[[agg[3]]][1] <- NA
  # one with two in each of two scales: the rule counts per scale, so retained
  d[[agg[1]]][2] <- NA
  d[[agg[2]]][2] <- NA
  d[[intim[1]]][2] <- NA
  d[[intim[2]]][2] <- NA
  # two of the three demographics missing: excluded; one missing: retained
  d$demo_age[3] <- NA
  d$demo_hh_members[3] <- NA
  d$demo_income_hh_net[4] <- NA

  excluded <- exclude_participants_with_high_missingness(d, cb, analysis_plan)
  kept <- excluded$data
  reported <- excluded$reporting_data

  expect_setequal(reported$respondent_id, c(1, 3))
  expect_named(reported, c("respondent_id", "too_many_items_missing", "scales_over_threshold",
                           "too_many_demographics_missing", "n_demographics_missing"))
  # each reason keeps its own detail beside its flag
  expect_identical(reported$scales_over_threshold[reported$respondent_id == 3], NA_character_)
  expect_identical(reported$n_demographics_missing[reported$respondent_id == 3], 2L)
  expect_true(nzchar(reported$scales_over_threshold[reported$respondent_id == 1]))
  expect_identical(reported$n_demographics_missing[reported$respondent_id == 1], 0L)
  expect_equal(nrow(kept), nrow(d) - 2L)
  expect_false(any(c(1, 3) %in% kept$respondent_id))
  # retained: the two-per-scale respondent, the one-demographic respondent, and
  # the respondents without gender (gender is never a reason to exclude)
  expect_true(all(c(2, 4, 5, 40) %in% kept$respondent_id))
  # the record names the threshold each excluded respondent exceeded, and only that one
  expect_true(reported$too_many_items_missing[reported$respondent_id == 1])
  expect_false(reported$too_many_demographics_missing[reported$respondent_id == 1])
  expect_true(reported$too_many_demographics_missing[reported$respondent_id == 3])
  expect_false(reported$too_many_items_missing[reported$respondent_id == 3])
  # the frame carries no imputed cell yet
  expect_null(attr(kept, "imputed_cells", exact = TRUE))
})

test_that("gender is a predictor of all three demographic models or of none", {
  # gender is never imputed, so a single missing gender would cost every model
  # the rows it cannot predict: the plan drops gender instead of the rows
  every_gender_known <- select_demographic_imputation_predictors(
    make_fill_frame(gender_missing = integer(0)), cb, analysis_plan)
  expect_equal(every_gender_known$demo_age, c(cb$scales$scale_key, "gender"))
  expect_equal(every_gender_known$demo_hh_members,
               c(cb$scales$scale_key, "gender", "demo_age"))
  expect_equal(every_gender_known$demo_income_hh_net,
               c(cb$scales$scale_key, "gender", "demo_age", "demo_hh_members"))

  some_gender_missing <- select_demographic_imputation_predictors(
    make_fill_frame(), cb, analysis_plan)
  expect_false(any(vapply(some_gender_missing, function(p) "gender" %in% p, logical(1))))
  # each model still adds the demographics imputed before it
  expect_equal(some_gender_missing$demo_age, cb$scales$scale_key)
  expect_equal(some_gender_missing$demo_hh_members, c(cb$scales$scale_key, "demo_age"))
  expect_equal(some_gender_missing$demo_income_hh_net,
               c(cb$scales$scale_key, "demo_age", "demo_hh_members"))
})

test_that("a scale or a demographic without a gap is passed through unfitted", {
  d <- make_fill_frame()
  predictors <- select_demographic_imputation_predictors(d, cb, analysis_plan)
  # any fit attempt on a complete frame is a failure, so brms::brm() must not run
  local_mocked_bindings(brm = function(...) stop("no fit expected"), .package = "brms")

  imputed <- impute_items(d, cb, analysis_plan)
  # the item verb always leaves its record behind; nothing else about the frame changes
  expect_identical(attr(imputed, "imputation_model_status"), create_empty_imputation_model_status())
  expect_length(attr(imputed, "unavailable_imputation_variables"), 0L)
  attr(imputed, "imputed_cells") <- NULL
  attr(imputed, "imputation_model_status") <- NULL
  attr(imputed, "unavailable_imputation_variables") <- NULL
  attr(imputed, "unavailable_imputation_variables_known_gender") <- NULL
  expect_identical(imputed, d)
  expect_identical(impute_age(d, cb, analysis_plan, predictors$demo_age), d)
  expect_identical(impute_household_size(d, cb, analysis_plan, predictors$demo_hh_members), d)
  expect_identical(impute_income_band(d, cb, analysis_plan, predictors$demo_income_hh_net), d)
})

test_that("a run that imputes nothing still hands on the typed empty record", {
  d <- make_fill_frame()
  local_mocked_bindings(brm = function(...) stop("no fit expected"), .package = "brms")

  cells <- attr(impute_items(d, cb, analysis_plan), "imputed_cells", exact = TRUE)
  expect_named(cells, cells_columns)
  expect_equal(nrow(cells), 0L)
  expect_identical(cells, ap3_fill_empty_cells())
})

test_that("the imputation records are empty with their columns before anything is imputed", {
  d <- make_fill_frame()
  cells <- ap3_fill_empty_cells()
  expect_named(cells, cells_columns)
  expect_equal(nrow(cells), 0L)
  reported <- exclude_participants_with_high_missingness(d, cb, analysis_plan)$reporting_data
  expect_named(reported, c("respondent_id", "too_many_items_missing", "scales_over_threshold",
                           "too_many_demographics_missing", "n_demographics_missing"))
  expect_equal(nrow(reported), 0L)
})

test_that("demographic imputation scoring uses the items' existing direction", {
  d <- make_fill_frame(n = 4)
  key <- cb$scales$scale_key[[1]]
  codes <- cb$scales$item_codes[[1]]
  d[codes] <- lapply(seq_along(codes), function(i) rep(as.numeric(i), nrow(d)))
  items_before <- d[codes]
  persons <- average_items_into_subscales(d, cb)
  expect_equal(persons[[key]], rep(mean(seq_along(codes)), nrow(d)))
  expect_identical(d[codes], items_before)
})

test_that("the counted gaps and the answered cells are the ones the item imputation reads", {
  d <- make_fill_frame()
  agg <- scale_items("asc_agg")
  d[[agg[1]]][1] <- NA
  d[[agg[2]]][1] <- NA
  d[[agg[1]]][2] <- NA

  specifications <- ap3_item_imputation_specifications(d, cb, analysis_plan)
  # a scale without a gap gets no specification, so it is never fitted
  expect_equal(vapply(specifications, function(s) s$scale, character(1)), "asc_agg")
  gaps <- specifications[[1]]$missing_item_cells
  expect_equal(nrow(gaps), 3L)
  expect_equal(sum(gaps$respondent_id == 1), 2L)
  expect_equal(sum(gaps$respondent_id == 2), 1L)
  expect_setequal(gaps$item[gaps$respondent_id == 1], agg[1:2])

  categories <- seq(analysis_plan$scales$response_min, analysis_plan$scales$response_max)
  answered <- ap3_observed_item_answers(d, specifications[[1]], categories)
  expect_named(answered, c("respondent_id", "item", "answer"))
  # one row per answered cell of the scale, the three gaps left out
  expect_equal(nrow(answered), length(agg) * nrow(d) - 3L)
  expect_false(anyNA(answered$answer))
  expect_true(is.ordered(answered$answer))
  expect_equal(levels(answered$answer), as.character(categories))
  expect_setequal(as.character(unique(answered$item)), agg)
  expect_equal(as.character(answered$answer[answered$respondent_id == 1 & answered$item == agg[3]]),
               as.character(d[[agg[3]]][1]))
})

test_that("the write-back reaches the named cells and the record keeps the imputation order", {
  d <- make_fill_frame()
  agg <- scale_items("asc_agg")
  d[[agg[1]]][1] <- NA
  d[[agg[2]]][3] <- NA
  d$demo_age[2] <- NA

  item_cells <- data.frame(respondent_id = c(1L, 3L), item = c(agg[1], agg[2]))
  written <- ap3_add_item_imputations(d, item_cells, c(2.5, 4))
  expect_equal(written[[agg[1]]][1], 2.5)
  expect_equal(written[[agg[2]]][3], 4)
  expect_equal(written[[agg[1]]][-1], d[[agg[1]]][-1])
  # one column name serves every cell of a demographic imputation
  plan_probabilities <- c((1 - analysis_plan$missing_data$fill$interval) / 2,
                          1 - (1 - analysis_plan$missing_data$fill$interval) / 2)
  age_specification <- list(
    variable = "demo_age", respondent_id = d$respondent_id[is.na(d$demo_age)],
    probabilities = plan_probabilities,
    participants_with_missing_value = data.frame(demo_age = NA_real_))
  ages <- ap3_add_demographic_imputations(d, age_specification, 41)
  expect_equal(ages$demo_age[2], 41)
  expect_false(anyNA(ages$demo_age))

  # The reported bounds are the 2.5% and 97.5% quantiles of the fit's posterior
  # predictive draws; the mocked draws stand in for them so that no fit is run.
  local_mocked_bindings(
    posterior_predict = function(object, newdata, ...) {
      matrix(as.numeric(1:4), nrow = 4L, ncol = nrow(newdata))
    },
    .package = "brms")
  bounds <- unname(stats::quantile(1:4, probs = plan_probabilities))
  item_fit <- stats::lm(value ~ 1, data.frame(value = seq_len(355)))
  age_fit <- stats::lm(value ~ 1, data.frame(value = seq_len(58)))

  recorded <- ap3_add_item_imputation_reporting(written, item_fit, item_cells, c(2.5, 4),
                                                plan_probabilities, "asc_agg")
  cells <- attr(recorded, "imputed_cells", exact = TRUE)
  expect_named(cells, cells_columns)
  expect_equal(cells$value, c(2.5, 4))
  expect_equal(cells$kind, c("item", "item"))
  # the model column names the scale whose fit served the cells, as the
  # demographic rows name their variable
  expect_equal(cells$model, rep("asc_agg", 2L))
  expect_equal(cells$lower, rep(bounds[1], 2L))
  expect_equal(cells$upper, rep(bounds[2], 2L))
  expect_equal(cells$n_fit_rows, c(355L, 355L))

  again <- ap3_add_demographic_imputation_reporting(recorded, age_fit, age_specification, 41)
  recorded_cells <- attr(again, "imputed_cells", exact = TRUE)
  expect_equal(recorded_cells$variable, c(agg[1], agg[2], "demo_age"))
  expect_equal(recorded_cells$kind, c("item", "item", "demographic"))
  expect_equal(recorded_cells$n_fit_rows, c(355L, 355L, 58L))
})

test_that("the reported bounds follow the plan's interval, for items and demographics alike", {
  d <- make_fill_frame()
  agg <- scale_items("asc_agg")
  d[[agg[1]]][1] <- NA
  d$demo_age[2] <- NA
  narrow <- analysis_plan
  narrow$missing_data$fill$interval <- 0.50
  predictors <- select_demographic_imputation_predictors(d, cb, narrow)

  # Items: the specification computes the bounds, the reporting helper uses them.
  item_specification <- ap3_item_imputation_specifications(d, cb, narrow)[[1]]
  expect_equal(item_specification$probabilities, c(.25, .75))
  # Demographics: the same plan value reaches all three specifications.
  for (specification in list(
      ap3_age_imputation_specification(d, cb, narrow, predictors$demo_age),
      ap3_household_imputation_specification(d, cb, narrow, predictors$demo_hh_members),
      ap3_income_imputation_specification(d, cb, narrow, predictors$demo_income_hh_net))) {
    expect_equal(specification$probabilities, c(.25, .75))
  }

  local_mocked_bindings(
    posterior_predict = function(object, newdata, ...) {
      matrix(as.numeric(1:4), nrow = 4L, ncol = nrow(newdata))
    },
    .package = "brms")
  fit <- stats::lm(value ~ 1, data.frame(value = seq_len(12)))
  cells <- data.frame(respondent_id = 1L, item = agg[1])
  narrow_bounds <- unname(stats::quantile(1:4, probs = c(.25, .75)))
  wide_bounds <- unname(stats::quantile(1:4, probs = c(.025, .975)))
  expect_false(isTRUE(all.equal(narrow_bounds, wide_bounds)))

  recorded <- attr(ap3_add_item_imputation_reporting(
    d, fit, cells, 3, item_specification$probabilities, item_specification$scale),
    "imputed_cells", exact = TRUE)
  expect_equal(unname(c(recorded$lower, recorded$upper)), narrow_bounds)

  age_specification <- list(
    variable = "demo_age", respondent_id = d$respondent_id[is.na(d$demo_age)],
    probabilities = c(.25, .75),
    participants_with_missing_value = data.frame(demo_age = NA_real_))
  demographic <- attr(ap3_add_demographic_imputation_reporting(
    d, fit, age_specification, 41), "imputed_cells", exact = TRUE)
  expect_equal(unname(c(demographic$lower, demographic$upper)), narrow_bounds)
})

test_that("a respondent without an answered item of a scale never reaches the item imputation", {
  d <- make_fill_frame()
  for (code in scale_items("zm_power")) d[[code]][7] <- NA
  excluded <- exclude_participants_with_high_missingness(d, cb, analysis_plan)
  reported <- excluded$reporting_data
  expect_false(7 %in% excluded$data$respondent_id)
  expect_true(reported$too_many_items_missing[reported$respondent_id == 7])
  # with that respondent gone, no scale has a gap left to fit
  expect_length(ap3_item_imputation_specifications(excluded$data, cb, analysis_plan), 0L)
})

# Fitting tests ---------------------------------------------------------------
# The whole chain runs once; the tests below read its result.

cmdstan_ok <- tryCatch({
  zm_setup()
  nzchar(cmdstanr::cmdstan_version())
}, error = function(e) FALSE)

stan_dir <- file.path(tempdir(), "zm_stan_fill_tests")
dir.create(stan_dir, showWarnings = FALSE, recursive = TRUE)
withr::local_options(list(cmdstanr_write_stan_file_dir = stan_dir), .local_envir = teardown_env())

planted <- local({
  d <- make_fill_frame()
  agg <- scale_items("asc_agg")
  intim <- scale_items("zm_security")
  sdo <- scale_items("sdo_dom")
  conv <- scale_items("asc_conv")
  d[[agg[1]]][1] <- NA               # one gap in asc_agg
  d[[agg[2]]][5] <- NA               # a respondent without gender, same scale
  d[[intim[1]]][2] <- NA             # two gaps of one respondent in one scale
  d[[intim[2]]][2] <- NA
  d[[sdo[3]]][3] <- NA               # two respondents missing the same item
  d[[sdo[3]]][4] <- NA
  d$demo_age[6] <- NA
  d$demo_age[5] <- NA                # no gender: imputed like every other row
  d$demo_hh_members[7] <- NA
  d$demo_income_hh_net[8] <- NA
  d[[conv[1]]][9] <- NA              # excluded: three missing items in one scale
  d[[conv[2]]][9] <- NA
  d[[conv[3]]][9] <- NA
  d$demo_age[10] <- NA               # excluded: two demographics missing
  d$demo_hh_members[10] <- NA
  d
})
excluded <- exclude_participants_with_high_missingness(planted, cb, analysis_plan)
# the reference group is a property of the retained population, so it is chosen
# after the exclusion and before gender could serve as a predictor
kept <- set_gender_reference(excluded$data)
predictors <- select_demographic_imputation_predictors(kept, cb, analysis_plan)

imputed <- if (cmdstan_ok) {
  kept |>
    impute_items(cb, analysis_plan) |>
    impute_age(cb, analysis_plan, predictors$demo_age) |>
    impute_household_size(cb, analysis_plan, predictors$demo_hh_members) |>
    impute_income_band(cb, analysis_plan, predictors$demo_income_hh_net)
} else {
  NULL
}
cells <- if (cmdstan_ok) attr(imputed, "imputed_cells", exact = TRUE) else NULL

test_that("every planted item gap is imputed inside the response range, with its interval", {
  skip_if_not(cmdstan_ok, "CmdStan not available")
  items <- cells[cells$kind == "item", ]
  expect_equal(nrow(items), 6L)
  expect_true(all(items$value >= analysis_plan$scales$response_min & items$value <= analysis_plan$scales$response_max))
  # the imputed value may be fractional; the reported bounds are quantiles of
  # the posterior predictive draws, so they stay inside the response scale and
  # bracket the value they are reported beside
  expect_true(all(items$lower >= analysis_plan$scales$response_min & items$upper <= analysis_plan$scales$response_max))
  expect_true(all(items$lower <= items$value & items$value <= items$upper))
  expect_true(all(items$n_fit_rows > 0))
  # no item column of the retained respondents is missing
  expect_false(anyNA(imputed[unlist(cb$scales$item_codes)]))
})

test_that("one fit per scale serves every gap of that scale", {
  skip_if_not(cmdstan_ok, "CmdStan not available")
  items <- cells[cells$kind == "item", ]
  # which scale served a cell is read from the model column, from the item as
  # the codebook registers it, and from the fit's row count
  scale_of <- function(item) {
    cb$scales$scale_key[vapply(cb$scales$item_codes, function(codes) item %in% codes, logical(1))]
  }
  # every item row names the scale whose fit served it
  expect_equal(items$model, unname(vapply(items$variable, scale_of, character(1))))
  expect_setequal(unique(items$model), c("asc_agg", "zm_security", "sdo_dom"))
  expect_setequal(unname(vapply(items$variable, scale_of, character(1))),
                  c("asc_agg", "zm_security", "sdo_dom"))
  # a respondent with two gaps in one scale gets both imputed by that scale's fit
  intim <- items[items$variable %in% scale_items("zm_security"), ]
  expect_equal(nrow(intim), 2L)
  expect_equal(intim$respondent_id, c(2, 2))
  expect_setequal(intim$variable, scale_items("zm_security")[1:2])
  expect_equal(length(unique(intim$n_fit_rows)), 1L)
  # two respondents missing the same item are imputed by the one sdo_dom fit
  sdo <- items[items$variable %in% scale_items("sdo_dom"), ]
  expect_setequal(sdo$respondent_id, c(3, 4))
  expect_equal(unique(sdo$variable), scale_items("sdo_dom")[3])
  expect_equal(length(unique(sdo$n_fit_rows)), 1L)
  # every answered cell of the scale enters its fit
  expect_equal(unique(sdo$n_fit_rows),
               sum(!is.na(as.matrix(kept[scale_items("sdo_dom")]))))
})

test_that("age, household size and income band are imputed in order on every retained gap", {
  skip_if_not(cmdstan_ok, "CmdStan not available")
  demographic <- cells[cells$kind == "demographic", ]
  expect_equal(demographic$variable,
               c("demo_age", "demo_age", "demo_hh_members", "demo_income_hh_net"))
  expect_equal(demographic$model, demographic$variable)

  age <- demographic[demographic$variable == "demo_age", ]
  # respondent 5 has no gender: the demographic imputation reaches that row too
  expect_equal(age$respondent_id, c(5, 6))
  expect_true(all(is.finite(age$value) & age$value > 0))
  expect_true(all(age$lower <= age$value & age$value <= age$upper))
  expect_equal(unique(age$n_fit_rows), sum(!is.na(kept$demo_age)))

  # the household fit runs after the age imputation: no retained row still lacks
  # an age, and respondent 6 (the imputed age) is one of its rows
  expect_false(anyNA(imputed$demo_age))
  hh <- demographic[demographic$variable == "demo_hh_members", ]
  expect_equal(hh$respondent_id, 7)
  expect_equal(hh$value, round(hh$value))
  expect_true(hh$value >= 1 && hh$value <= 8)
  expect_equal(hh$n_fit_rows, sum(!is.na(kept$demo_hh_members)))
  expect_true(6 %in% imputed$respondent_id[!is.na(imputed$demo_hh_members)])

  income <- demographic[demographic$variable == "demo_income_hh_net", ]
  expect_equal(income$respondent_id, 8)
  expect_equal(income$value, round(income$value))
  expect_true(income$value >= 1 && income$value <= 13)
  expect_equal(income$n_fit_rows, sum(!is.na(kept$demo_income_hh_net)))
  # the last demographic verb establishes that the three are complete
  expect_false(anyNA(imputed[c("demo_age", "demo_hh_members", "demo_income_hh_net")]))
})

test_that("a respondent without gender is imputed like any other and keeps the missing gender", {
  skip_if_not(cmdstan_ok, "CmdStan not available")
  expect_true(is.na(imputed$gender[imputed$respondent_id == 5]))
  expect_false(imputed$known_gender[imputed$respondent_id == 5])
  expect_false("gender" %in% cells$variable)
  # both the item and the demographic imputation reach that row
  expect_false(is.na(imputed[[scale_items("asc_agg")[2]]][imputed$respondent_id == 5]))
  expect_false(is.na(imputed$demo_age[imputed$respondent_id == 5]))
  expect_true(5 %in% cells$respondent_id[cells$kind == "item"])
  expect_true(5 %in% cells$respondent_id[cells$kind == "demographic"])
})

test_that("the record has one row per imputed cell and the excluded respondents stay out", {
  skip_if_not(cmdstan_ok, "CmdStan not available")
  expect_equal(nrow(cells), 10L)
  expect_equal(nrow(dplyr::distinct(cells[c("respondent_id", "variable")])), 10L)
  expect_named(cells, cells_columns)
  expect_false(anyNA(cells$n_fit_rows))
  expect_setequal(excluded$reporting_data$respondent_id, c(9, 10))
  expect_false(any(c(9, 10) %in% cells$respondent_id))
  expect_false(any(c(9, 10) %in% imputed$respondent_id))
  # the reporting boundary hands on exactly these two records, unchanged
  reporting <- build_imputation_reporting_data(imputed, excluded$reporting_data)
  expect_identical(reporting$imputed_cells, tibble::as_tibble(cells))
  expect_identical(reporting$excluded_participants,
                   tibble::as_tibble(excluded$reporting_data))
})

# ---- drift guard: what is fitted is what the card prints ---------------------
# The item model's formula is written out literally in R/ap3_fill.R; the three
# demographic formulas are assembled from the predictor sets, so they are read from the specification each verb
# builds. The same four formulas stand in config/analysis_plan.yaml, where the
# four fill model cards read them. A change to one of the two must not pass
# silently, and neither must a change to a prior width.

fill_source_calls <- function(path) {
  found <- list()
  walk <- function(node) {
    if (is.call(node)) {
      callee <- node[[1]]
      if (identical(callee, quote(brm)) || identical(callee, quote(brms::brm))) {
        arguments <- as.list(node)[-1]
        argument_names <- if (is.null(names(arguments))) rep("", length(arguments)) else names(arguments)
        positional <- which(!nzchar(argument_names))
        found[[length(found) + 1L]] <<- list(
          formula = if (length(positional)) arguments[[positional[[1]]]] else arguments$formula,
          prior = node$prior)
      }
    } else if (!is.pairlist(node)) {
      return(invisible(NULL))
    }
    parts <- as.list(node)
    for (i in seq_along(parts)) {
      # an empty argument (`x[, 1]`, a formal without a default) is the empty
      # symbol, which cannot be passed on
      if (identical(parts[[i]], quote(expr = ))) next
      walk(parts[[i]])
    }
    invisible(NULL)
  }
  for (expression in as.list(parse(path, keep.source = FALSE))) walk(expression)
  found
}

fill_flat <- function(x) {
  gsub("[[:space:]]+", " ", trimws(paste(deparse(x, width.cutoff = 500L), collapse = " ")))
}

# The prior statements of one brm() call, as its fill card writes them:
# `<class> <distribution>`, in the order the call lists them. `priors` is the
# plan object the fitting verb reads, bound here so that the strings the source
# builds can be evaluated without fitting anything.
fill_source_priors <- function(call, priors) {
  classes <- c(Intercept = "intercept", sd = "sd", b = "b", sigma = "sigma")
  rows <- as.list(call$prior)[-1]
  paste(vapply(rows, function(row) {
    stopifnot(identical(row[[1]], quote(brms::set_prior)))
    class <- eval(row$class)
    paste(classes[[class]], eval(row[[2]], list(priors = priors)))
  }, character(1)), collapse = "; ")
}

test_that("the fitted fill formulas are the formulas of the configuration", {
  fill <- analysis_plan$missing_data$fill
  configured <- c(
    fill$items$formula,
    vapply(as.character(fill$demographics$order),
           function(name) fill$demographics[[name]]$formula, character(1))
  )
  configured <- gsub("[[:space:]]+", " ", trimws(as.character(configured)))
  # The plan's demographic formulas state the preregistered condition on gender
  # (it is a predictor only when observed for every retained participant). The
  # fitted formula is the brms one, so the condition is checked here and then
  # removed before the terms are compared.
  gender_condition <- " (only when observed for every retained participant)"
  demographic <- configured[-1L]
  expect_true(all(grepl(gender_condition, demographic, fixed = TRUE)))
  configured <- gsub(gender_condition, "", configured, fixed = TRUE)

  calls <- fill_source_calls(file.path(root, "R", "ap3_fill.R"))
  expect_length(calls, length(configured))
  # every gender known, so the demographic models carry their gender term
  complete <- make_fill_frame(gender_missing = integer(0))
  sets <- select_demographic_imputation_predictors(complete, cb, analysis_plan)
  fitted <- c(
    fill_flat(calls[[1]]$formula),
    fill_flat(ap3_age_imputation_specification(
      complete, cb, analysis_plan, sets$demo_age)$formula),
    fill_flat(ap3_household_imputation_specification(
      complete, cb, analysis_plan, sets$demo_hh_members)$formula),
    fill_flat(ap3_income_imputation_specification(
      complete, cb, analysis_plan, sets$demo_income_hh_net)$formula)
  )
  expect_identical(unname(fitted), unname(configured))
})

test_that("the fitted fill priors are the widths of the configuration, as the card prints them", {
  fill <- analysis_plan$missing_data$fill
  specs <- c(list(fill$items), lapply(as.character(fill$demographics$order),
                                      function(name) fill$demographics[[name]]))
  fields <- c("missing_data$fill$items$priors",
              paste0("missing_data$fill$demographics$", fill$demographics$order, "$priors"))
  calls <- fill_source_calls(file.path(root, "R", "ap3_fill.R"))
  expect_length(calls, length(specs))
  for (i in seq_along(specs)) {
    expect_identical(
      fill_source_priors(calls[[i]], specs[[i]]$priors),
      pc_ap3_fill_priors(specs[[i]]$priors, fields[[i]]),
      info = fields[[i]]
    )
  }
})
