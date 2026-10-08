# AP5 descriptive statistics
# Implements AP5: demographics (age, gender, school education, East/West, income —
# net household income per household member) and the quota-group composition as
# counts and percentages or as mean/sd/median/min/max; subscale means with sd,
# range, skew and kurtosis; pairwise Pearson correlations of the nine scale means
# on the raw 1..6 metric (not the z scores). Nothing here enters a model.
#
# The functions read the derived covariate columns that AP3 adds (`age`, `gender`,
# `education`, `east_west`, `income`, `quota_group`).

#' Numeric summary of one variable
#'
#' @param x Numeric vector.
#' @param variable Variable name for the output.
#' @param label Readable label for the output.
#' @return One-row tibble variable, label, n, mean, sd, median, min, max
#'   (missing values dropped; `n` counts the non-missing values).
ap5_summarise_numeric <- function(x, variable, label = variable) {
  x <- as.numeric(x)
  x <- x[!is.na(x)]
  tibble::tibble(
    variable = variable,
    label = label,
    n = length(x),
    mean = if (length(x) > 0) mean(x) else NA_real_,
    sd = if (length(x) > 1) stats::sd(x) else NA_real_,
    median = if (length(x) > 0) stats::median(x) else NA_real_,
    min = if (length(x) > 0) min(x) else NA_real_,
    max = if (length(x) > 0) max(x) else NA_real_
  )
}

#' Count the categories of one descriptive variable
#'
#' Preserves a declared factor-level order, counts every observed category and
#' adds one explicit missing-value category when needed. It chooses neither the
#' variables nor their labels.
#'
#' @param values Vector (factor, character or numeric codes).
#' @param variable Variable name for the output.
#' @param label Readable label for the output.
#' @return Tibble `variable`, `label`, `level`, `n`, `pct`; `pct` sums to 100
#'   over the rows of the variable.
ap5_count_categories <- function(values, variable, label) {
  if (is.factor(values)) {
    levels_to_report <- levels(values)
    values <- as.character(values)
  } else {
    values <- as.character(values)
    values[!is.na(values) & !nzchar(values)] <- NA_character_
    levels_to_report <- sort(unique(values[!is.na(values)]))
  }
  counts <- as.integer(table(factor(values, levels = levels_to_report)))
  if (anyNA(values)) {
    levels_to_report <- c(levels_to_report, "(missing)")
    counts <- c(counts, sum(is.na(values)))
  }
  tibble::tibble(
    variable = variable,
    label = label,
    level = levels_to_report,
    n = counts,
    pct = 100 * counts / sum(counts)
  )
}

# BEGIN GENERATED PARAMETER CARD: AP5 SAMPLE COMPOSITION
# Automatically generated from analysis_plan.yaml
# Described variables, in this order   (descriptives.demographics)
#   demo_age, demo_gender, demo_edu_school, east_west, income, quota_group
# END GENERATED PARAMETER CARD: AP5 SAMPLE COMPOSITION
#' AP5 — sample composition
#'
#' Describe the six preregistered demographic and quota variables on the
#' all-retained study, including the participants with a missing gender.
#' Categorical results retain every category and a missing-value row as n and
#' %; numeric results are n, mean, SD, median, minimum and maximum among the
#' observed values.
#'
#' @param data The descriptive and reliability input
#'   (`data_descriptive_reliability`).
#' @param config Configuration from [zm_config()];
#'   `config$descriptives$demographics` names the variables.
#' @return List `categorical` and `numeric`.
describe_sample_composition <- function(data, config) {
  variables <- list(
    demo_age = list(variable = "demo_age", label = "Age (years)", kind = "numeric", values = data$age),
    demo_gender = list(variable = "demo_gender", label = "Gender", kind = "categorical", values = data$gender),
    demo_edu_school = list(variable = "demo_edu_school", label = "School education", kind = "categorical", values = data$education),
    east_west = list(variable = "east_west", label = "East/West", kind = "categorical", values = data$east_west),
    income = list(variable = "income", label = "Net household income per person", kind = "numeric", values = data$income),
    quota_group = list(variable = "quota_group", label = "Political-leaning quota group", kind = "categorical", values = data$quota_group)
  )
  variables <- variables[config$descriptives$demographics]
  categorical <- variables |>
    lapply(function(variable) {
    if (variable$kind != "categorical") return(NULL)
    ap5_count_categories(variable$values, variable$variable, variable$label)
  }) |>
    dplyr::bind_rows()
  numeric <- variables |>
    lapply(function(variable) {
    if (variable$kind != "numeric") return(NULL)
    ap5_summarise_numeric(variable$values, variable$variable, variable$label)
  }) |>
    dplyr::bind_rows()
  list(categorical = categorical, numeric = numeric)
}

#' AP5 — distributions of all registered raw scale scores
#'
#' Describe every codebook-registered scale score before any model uses
#' z-standardised values. The scores are the AP3 item means on the
#' unstandardised 1-6 response metric; `psych::describe(type = 3)` supplies the
#' skewness and excess kurtosis.
#'
#' @param data The descriptive and reliability input
#'   (`data_descriptive_reliability`).
#' @param codebook Codebook list from [zm_codebook()].
#' @return Tibble `scale_key`, `label`, `n`, `mean`, `sd`, `median`, `min`,
#'   `max`, `skew`, `kurtosis`.
describe_scale_distributions <- function(data, codebook) {
  scale_columns <- codebook$scales$scale_key
  scale_labels <- codebook$scales$label
  scores <- as.data.frame(data[scale_columns])
  described <- suppressWarnings(psych::describe(
    scores,
    skew = TRUE,
    type = 3
  ))
  tibble::tibble(
    scale_key = scale_columns,
    label = scale_labels,
    n = as.integer(described$n),
    mean = as.numeric(described$mean),
    sd = as.numeric(described$sd),
    median = as.numeric(described$median),
    min = as.numeric(described$min),
    max = as.numeric(described$max),
    skew = as.numeric(described$skew),
    kurtosis = as.numeric(described$kurtosis)
  )
}

# BEGIN GENERATED PARAMETER CARD: AP5 SCALE CORRELATIONS
# Automatically generated from analysis_plan.yaml
#   Coefficient: pearson   (descriptives.correlations; pairwise-complete observations)
# END GENERATED PARAMETER CARD: AP5 SCALE CORRELATIONS
#' AP5 — pairwise associations among the nine raw scale scores
#'
#' Estimate the preregistered Pearson matrix and its matching pairwise
#' sample-size matrix, rows and columns in the order of the scale codebook (the
#' five motive scores precede the four authoritarian-orientation outcome
#' scores); pairwise-complete observations determine both each correlation and
#' its reported N.
#'
#' @param data The descriptive and reliability input
#'   (`data_descriptive_reliability`).
#' @param codebook Codebook list from [zm_codebook()].
#' @param config Configuration from [zm_config()];
#'   `config$descriptives$correlations` names the method.
#' @return List `matrix`, `n`, `labels`, `method`.
describe_scale_correlations <- function(data, codebook, config) {
  # The nine scale scores, in the codebook's order.
  scale_columns <- as.character(codebook$scales$scale_key)
  correlation_method <- config$descriptives$correlations
  scale_labels <- stats::setNames(codebook$scales$label, codebook$scales$scale_key)
  raw_scale_scores <- as.matrix(data[scale_columns])
  storage.mode(raw_scale_scores) <- "double"
  correlation_matrix <- stats::cor(
    raw_scale_scores,
    method = correlation_method,
    use = "pairwise.complete.obs"
  )
  pairwise_n <- crossprod(!is.na(raw_scale_scores))
  dimnames(pairwise_n) <- dimnames(correlation_matrix)
  list(
    matrix = correlation_matrix,
    n = pairwise_n,
    labels = unname(scale_labels[scale_columns]),
    method = correlation_method
  )
}


#' Observed sample margins from the unlinked side file and unfilled categories
#'
#' Every statistic is univariate. The independently shuffled demographic columns
#' are never joined by row or participant number to one another or to responses.
describe_observed_sample_composition <- function(demographics, data, config) {
  describe_sample_composition(list(age = demographics$age, income = demographics$income,
    education = demographics$education, east_west = demographics$east_west,
    gender = data$gender, quota_group = data$quota_group), config)
}
