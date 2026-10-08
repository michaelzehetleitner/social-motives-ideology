# AP3 data preprocessing
# Implements the part of AP3 (preregistration.qmd, "AP3 — Data preprocessing")
# that precedes the imputation. Reverse-keyed items are recoded as
# (min + max) - x on the response scale (7 - x on 1..6); prepare_gender()
# produces the analysis gender factor and its observed-value marker, and
# set_gender_reference() makes the largest group of the retained population
# the reference; each scale score is the participant's mean over the scale's
# items (average_items_into_subscales() in R/ap3_preparation.R). The plan and
# codebook invariants behind the East/West and education derivations are
# checked here without participant data.
#
# The file also holds the standardisation specification (ap3_z_columns(),
# ap3_model_spec()), which the standardisation verbs of R/ap3_preparation.R read.
#
# Depends on R/config.R (zm_*) and R/ap3_preparation.R
# (average_items_into_subscales()). Every numeric decision comes from
# analysis_plan (config/analysis_plan.yaml); item lists and reverse keys from
# zm_codebook().

# BEGIN GENERATED PARAMETER CARD: AP3 REVERSAL
# Automatically generated from analysis_plan.yaml
#   Response range: 1 to 6   (scales.response_min, scales.response_max)
#   A reverse-keyed item x becomes 7 - x; the reverse-keyed items are the codebook's reverse_items.
# END GENERATED PARAMETER CARD: AP3 REVERSAL
#' AP3 — reverse-score the reverse-keyed items
#'
#' Every item code listed in `codebook$scales$reverse_items` is recoded as
#' `(response_min + response_max) - x` with the bounds from `analysis_plan$scales`
#' (7 - x on the 1..6 scale). Every item value must lie within the response
#' bounds; a value outside them stops the pipeline. The result carries the
#' attribute `reversed_items` (the recoded item codes), which prevents a
#' second reversal.
#'
#' @param data Tibble with the item columns (normalised names).
#' @param codebook Codebook from [zm_codebook()].
#' @param analysis_plan Configuration from [zm_config()].
#' @return `data` with the reverse-keyed items recoded and attribute
#'   `reversed_items`.
reverse_items <- function(data, codebook, analysis_plan) {
  lo <- analysis_plan$scales$response_min
  hi <- analysis_plan$scales$response_max
  reversed_items <- unique(unlist(codebook$scales$reverse_items))
  for (item in reversed_items) {
    data[[item]] <- (lo + hi) - data[[item]]
  }
  attr(data, "reversed_items") <- reversed_items
  data
}

# Map recorded categories first; the study sample chooses its reference next.
#' AP3 — the analysis gender factor and its observed-value marker
#'
#' Codes 1 = male, 2 = female, 3 = divers; only the levels present in the data
#' become levels of the factor. `known_gender` marks the rows whose gender was
#' observed; gender itself is never imputed. Choosing the treatment reference is
#' a separate step ([set_gender_reference()]), because the largest group is
#' a property of the retained analysis population, not of this coding.
#'
#' Human-readable coding: `panel/preregistration/codebook_factors.csv`, value
#' set `gender_1_3`; model choices: `config/analysis_plan.yaml`, `gender`.
#'
#' @param data Frame with the numeric column `demo_gender`.
#' @param analysis_plan Configuration from [zm_config()].
#' @return `data` with `gender` and `known_gender`.
prepare_gender <- function(data, analysis_plan) {
  codes <- c(male = 1, female = 2, divers = 3)
  present <- codes[codes %in% data$demo_gender]
  data$gender <- factor(data$demo_gender,
    levels = unname(present), labels = names(present))
  data$known_gender <- !is.na(data$gender)
  data
}

#' AP3 — the largest group of the retained population becomes the reference
#'
#' The treatment reference is the most frequent gender among the participants
#' the analysis retains, so the excessive-missingness exclusion must precede it:
#' the largest group can change when participants leave. Ties follow the code
#' order. The reference is fixed before gender can serve as an imputation
#' predictor.
#'
#' @param data Frame after [prepare_gender()] and the exclusion.
#' @return `data` with `gender` relevelled to its reference.
set_gender_reference <- function(data) {
  counts <- table(data$gender)
  reference <- names(counts)[which.max(counts)]
  data$gender <- stats::relevel(data$gender, ref = reference)
  data
}

#' The midpoint of every age band
#'
#' A band runs from its first year to the year before the next band's first
#' year; the last band ends at `last_year`. Its midpoint is
#' (first year + last year) / 2 (`representative: midpoint`).
#'
#' @param age_bands `analysis_plan$age_bands` after [zm_config()].
#' @return Numeric vector, one midpoint per band, named by the band labels.
calculate_age_band_midpoints <- function(age_bands) {
  first_years <- as.numeric(age_bands$first_years)
  last_years <- c(first_years[-1L] - 1, as.numeric(age_bands$last_year))
  stats::setNames((first_years + last_years) / 2, age_bands$labels)
}

#' The representative of every income band per household member
#'
#' Each band lies between two neighbouring `limits`; its representative is the
#' geometric mean of the two (`representative: geometric_mean`), so the outer
#' limits enter the lowest and the highest representative.
#'
#' @param income_bands `analysis_plan$income_bands_per_member` after [zm_config()].
#' @return Numeric vector, one representative per band, named by the band labels.
calculate_income_band_representatives <- function(income_bands) {
  limits <- as.numeric(income_bands$limits)
  stats::setNames(sqrt(utils::head(limits, -1L) * utils::tail(limits, -1L)),
    income_bands$labels)
}

#' AP3 — age and income per household member in their analysis bands
#'
#' Runs after the fill, on exact `age` and exact `income`. Age falls into the
#' band whose first year it has reached (an age below the first band's first
#' year into the first band); income into the right-open band whose lower
#' limit it has reached (every value below the second limit into the lowest
#' band). Each band enters the analyses as its representative:
#' [calculate_age_band_midpoints()] and
#' [calculate_income_band_representatives()]. A missing value has no band.
#'
#' @param data Frame with `age` and `income`.
#' @param analysis_plan Configuration from [zm_config()].
#' @return `data` with the ordered factors `age_band` and `income_band`, the
#'   numeric `age_band_midpoint` and `income_band_value`, and
#'   `income_band_value_log`, the natural logarithm of `income_band_value`.
add_age_and_income_bands <- function(data, analysis_plan) {
  age_bands <- analysis_plan$age_bands
  age_band_index <- pmax(findInterval(data$age, age_bands$first_years), 1L)
  data$age_band <- factor(age_bands$labels[age_band_index],
    levels = age_bands$labels, ordered = TRUE)
  data$age_band_midpoint <- unname(calculate_age_band_midpoints(age_bands)[age_band_index])
  income_bands <- analysis_plan$income_bands_per_member
  inner_limits <- utils::head(utils::tail(income_bands$limits, -1L), -1L)
  income_band_index <- findInterval(data$income, inner_limits) + 1L
  data$income_band <- factor(income_bands$labels[income_band_index],
    levels = income_bands$labels, ordered = TRUE)
  data$income_band_value <- unname(
    calculate_income_band_representatives(income_bands)[income_band_index])
  data$income_band_value_log <- log(data$income_band_value)
  data
}

#' The registered East and West federal-state code sets are disjoint
#'
#' The plan invariant behind the East/West derivation of
#' [derive_covariates()], checked without participant
#' data so that [check_analysis_plan()] can establish it at the input boundary.
#'
#' @param analysis_plan Configuration from [zm_config()].
#' @return `analysis_plan`, invisibly.
ap3_check_east_west_disjoint <- function(analysis_plan) {
  east <- unlist(analysis_plan$east_west$east)
  west <- unlist(analysis_plan$east_west$west)
  if (length(intersect(east, west)) > 0) {
    stop("AP3: analysis_plan$east_west lists a state as both east and west: ", paste(intersect(east, west), collapse = ", "))
  }
  invisible(analysis_plan)
}

#' The registered school-education value set, in its declared order
#'
#' The codebook invariant behind [ap3_order_education_categories()], checked
#' without participant data so that [check_codebook()] can establish it at the
#' input boundary: one nonempty registered value set for `demo_edu_school`, with
#' unique codes, unique order numbers and a label for every code.
#'
#' @param codebook Codebook from [zm_codebook()].
#' @return The value set's rows, ordered by `order`.
ap3_check_education_value_set <- function(codebook) {
  set_rows <- NULL
  if (!is.null(codebook)) {
    set_id <- codebook$items$value_set_id[codebook$items$item_code == "demo_edu_school"]
    set_id <- set_id[!is.na(set_id)]
    if (length(set_id) == 1) {
      set_rows <- codebook$factors[codebook$factors$set_id == set_id, , drop = FALSE]
      set_rows <- set_rows[order(set_rows$order), , drop = FALSE]
    }
  }
  if (is.null(set_rows) || nrow(set_rows) == 0) {
    stop("AP3: the codebook has no value set for demo_edu_school.")
  }
  if (anyNA(set_rows$value_corr) || anyDuplicated(set_rows$value_corr) ||
      anyNA(set_rows$order) || anyDuplicated(set_rows$order)) {
    stop("AP3: the demo_edu_school value set has missing or duplicated codes or order numbers.")
  }
  if (anyNA(set_rows$label_de) || any(!nzchar(trimws(set_rows$label_de)))) {
    stop("AP3: the demo_edu_school value set has a code without a label.")
  }
  set_rows
}

#' Which variables are z-standardised, from which column, under which name
#'
#' Every standardised column is named by the codebook ([zm_z_col()]): the
#' outcomes and motives of `analysis_plan$regression` by `codebook_scales.csv`
#' (`scale_key`/`z_col`), the metric covariates of
#' `analysis_plan$standardisation$covariates_z` by the YAML `covariates` section
#' (`covariate`/`z_col`), so that the column names agree with
#' `analysis_plan$regression$predictors`. `source` is the column actually standardised:
#' the variable itself for the outcomes and motives; for the covariates their
#' band value ([add_age_and_income_bands()]), `age_band_midpoint` for `age` and
#' `income_band_value_log`, the log of the band representative, for `income`
#' (`transform` records it).
#'
#' @param analysis_plan Configuration from [zm_config()].
#' @param codebook Codebook from [zm_codebook()].
#' @return Tibble with columns `var`, `source`, `transform`
#'   (`identity` / `log`), `z_col`, `role` (outcome / predictor /
#'   covariate).
ap3_z_columns <- function(analysis_plan, codebook) {
  rows <- list()
  add <- function(var, z_col, role, source = var, transform = "identity") {
    rows[[length(rows) + 1]] <<- tibble::tibble(
      var = var, source = source, transform = transform, z_col = z_col, role = role
    )
  }
  for (v in unlist(analysis_plan$regression$outcomes)) add(v, zm_z_col(v, codebook), "outcome")
  for (v in unlist(analysis_plan$regression$motives)) add(v, zm_z_col(v, codebook), "predictor")
  # The two metric covariates of analysis_plan$standardisation$covariates_z under the
  # z column names the YAML covariates section gives them, each from its band
  # value; income from the log of its band representative.
  covariate_sources <- c(age = "age_band_midpoint", income = "income_band_value_log")
  covariate_transforms <- c(age = "identity", income = "log")
  for (v in unlist(analysis_plan$standardisation$covariates_z)) {
    z_col <- zm_z_col(v, codebook)
    if (v %in% names(covariate_sources)) {
      add(v, z_col, "covariate", source = covariate_sources[[v]], transform = covariate_transforms[[v]])
    } else {
      add(v, z_col, "covariate")
    }
  }
  spec <- dplyr::bind_rows(rows)
  if (anyDuplicated(spec$z_col) > 0) {
    stop("AP3: duplicated z column names: ", paste(spec$z_col[duplicated(spec$z_col)], collapse = ", "))
  }
  spec
}

#' The column each variable is standardised from
#'
#' @param variables Variable names (`var` of [ap3_z_columns()]).
#' @param analysis_plan Configuration from [zm_config()].
#' @param codebook Codebook from [zm_codebook()].
#' @return Character vector of source columns, named by `variables`; a
#'   variable without a standardisation entry is its own source.
select_z_source_columns <- function(variables, analysis_plan, codebook) {
  spec <- ap3_z_columns(analysis_plan, codebook)
  sources <- spec$source[match(variables, spec$var)]
  stats::setNames(ifelse(is.na(sources), variables, sources), variables)
}

#' The variables of each regression and of the network
#'
#' One entry per regression outcome (`analysis_plan$regression$outcomes`) and one
#' `network` entry. A regression's model columns are the outcome's z
#' column, the motives' z columns and `analysis_plan$regression$covariates`
#' (`age_z`, `gender`, `income_z`); its source columns are the
#' unstandardised variables behind them (`asc_agg`, `zm_security`, …, `age`,
#' `gender`, `income`); its
#' `z_vars` are the variables standardised within its sample. The
#' network's model columns are `analysis_plan$network$nodes`.
#'
#' @param analysis_plan Configuration from [zm_config()].
#' @param codebook Codebook from [zm_codebook()]; it names the z columns.
#' @return Named list; each element `list(model, kind, outcome, outcome_z,
#'   vars, sources, z_vars)` with `kind` `"regression"` or `"network"`;
#'   `outcome_z` is the outcome's standardised column, the one the regression
#'   formula is built on (`NA` for the network).
ap3_model_spec <- function(analysis_plan, codebook) {
  spec <- ap3_z_columns(analysis_plan, codebook)
  z_of <- function(v) {
    z <- spec$z_col[match(v, spec$var)]
    if (anyNA(z)) stop("AP3: no z column for variable(s): ", paste(v[is.na(z)], collapse = ", "))
    z
  }
  motives <- unlist(analysis_plan$regression$motives)
  covariates <- unlist(analysis_plan$regression$covariates)
  cov_idx <- match(covariates, spec$z_col)
  cov_sources <- ifelse(is.na(cov_idx), covariates, spec$var[cov_idx])
  cov_z_vars <- spec$var[cov_idx[!is.na(cov_idx)]]

  models <- list()
  for (o in unlist(analysis_plan$regression$outcomes)) {
    models[[o]] <- list(
      model = o, kind = "regression", outcome = o, outcome_z = z_of(o),
      vars = c(z_of(o), z_of(motives), covariates),
      sources = c(o, motives, cov_sources),
      z_vars = c(o, motives, cov_z_vars)
    )
  }
  nodes <- unlist(analysis_plan$network$nodes)
  node_idx <- match(nodes, spec$z_col)
  if (anyNA(node_idx)) {
    stop("AP3: network node(s) without a standardisation entry: ", paste(nodes[is.na(node_idx)], collapse = ", "))
  }
  models[["network"]] <- list(
    model = "network", kind = "network", outcome = NA_character_, outcome_z = NA_character_,
    vars = nodes, sources = spec$var[node_idx], z_vars = spec$var[node_idx]
  )
  models
}
