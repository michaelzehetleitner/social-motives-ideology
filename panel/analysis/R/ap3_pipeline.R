# Saved intake files and their companion explanation.

#' Dimensions of the two data files and the two imputation records
zm_data_files_summary <- function(scientific_path, demographics_path,
                                  filled_cells_path, dropped_respondents_path, analysis_plan) {
  one <- function(label, path) {
    data <- readr::read_csv(path, col_types = readr::cols(.default = readr::col_character()),
                           show_col_types = FALSE, progress = FALSE)
    tibble::tibble(file = label, path = path, n_rows = nrow(data), n_cols = ncol(data),
                   columns = paste(names(data), collapse = "; "))
  }
  dplyr::bind_rows(one("scientific-use file", scientific_path),
    one("deidentified demographics", demographics_path),
    one("filled cells", filled_cells_path), one("dropped respondents", dropped_respondents_path))
}

#' Explain the two prepared scientific-use files and the retained cell-fit records
zm_scientific_use_readme <- function(summary, analysis_plan,
                                     path = file.path(analysis_plan$root, "data", "derived", "scientific_use.README.txt")) {
  row <- summary[summary$file == "scientific-use file", , drop = FALSE]
  if (nrow(row) != 1L) stop("The data-files summary must contain one scientific-use file.")
  lines <- c(
    "Scientific-use files of the ZM panel study (ZM-ASC-SDO)",
    paste0("Filled scientific-use data: ", basename(row$path), "."),
    "Items and scale scores contain the registered AP3 fills; gender is not imputed.",
    "Age and income per household member are shared as their analysis bands and representatives.",
    "The six party-sympathy ratings and vote intention are included.",
    "The filled-cells record identifies each filled cell and its model, value and predictive interval.",
    "The dropped-respondents record and saved preparation results preserve exclusions and model checks.",
    "The deidentified-demographics file has observed age, income per household member, school education,",
    "East/West and duration. Its columns are independently shuffled, with missing values preserved and",
    "no participant numbers. Its rows must not be joined to each other or to the response data.",
    "Comments are held separately in an unpublished file after identifying text is masked.",
    paste0("Rows in the scientific-use file: ", row$n_rows, "; columns: ", row$n_cols, "."))
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, path)
  path
}

#' Prepare the accepted analysis data once, while raw demographics are available
#'
#' Reuses the AP1/AP3 operations in their registered order. No fitted object or
#' raw model frame is returned: imputation cells and numerical checks are the
#' existing reporting summaries attached by the fit functions.
prepare_analysis_at_intake <- function(data, codebook, analysis_plan) {
  eligible <- apply_study_exclusions(data, analysis_plan)
  scoring <- eligible$data |>
    reverse_items(codebook, analysis_plan) |>
    prepare_gender(analysis_plan) |>
    derive_east_west(analysis_plan)
  retained <- exclude_participants_with_high_missingness(scoring, codebook, analysis_plan)
  observed <- retained$data |>
    set_gender_reference() |>
    derive_covariates(analysis_plan, codebook)
  predictors <- select_demographic_imputation_predictors(retained$data, codebook, analysis_plan)
  filled <- retained$data |>
    set_gender_reference() |>
    impute_items(codebook, analysis_plan) |>
    impute_age(codebook, analysis_plan, predictors$demo_age) |>
    impute_household_size(codebook, analysis_plan, predictors$demo_hh_members) |>
    impute_income_band(codebook, analysis_plan, predictors$demo_income_hh_net)
  imputation <- build_imputation_reporting_data(filled, retained$reporting_data)
  filled <- filled |>
    derive_covariates(analysis_plan, codebook) |>
    average_items_into_subscales(codebook)
  list(data = filled, observed = observed,
       exclusions = eligible[c("log", "n_started", "n_quota_full")],
       imputation = imputation, imputation_demographic_predictors = predictors,
       observed_item_distributions = rh_item_distribution_table(eligible$data, codebook, analysis_plan))
}

#' The five observed demographic columns, shuffled independently without IDs
#'
#' Missing entries remain missing. Each permutation draws from the operating-system
#' entropy source independently of the analysis RNG, so neither the original
#' row order nor a shared seed or column permutation is retained.
build_deidentified_demographics <- function(observed) {
  columns <- c(age = "age", income = "income", education = "education",
               east_west = "east_west", duration = "Duration__in_seconds_")
  if (!all(columns %in% names(observed))) stop("Observed demographic columns are incomplete.")
  result <- lapply(columns, function(column) {
    values <- observed[[column]]
    values[randomise_intake_rows(length(values))]
  })
  tibble::as_tibble(result)
}

#' Restore only the saved analysis metadata to the filled scientific-use values
#'
#' These attributes carry failure status and the fixed gender reference, never
#' the linked raw demographics. Standardisation is computed later from the
#' same filled scales and demographic-band values as before intake reduction.
restore_prepared_analysis_data <- function(data, preparation, analysis_plan) {
  reference <- preparation$gender_levels
  data$gender <- factor(data$demo_gender,
    levels = unname(c(male = 1, female = 2, divers = 3)[reference]), labels = reference)
  data$known_gender <- !is.na(data$gender)
  data$income_band_value_log <- log(data$income_band_value)
  for (name in names(preparation$analysis_attributes))
    attr(data, name) <- preparation$analysis_attributes[[name]]
  data
}


#' Save the existing preparation and observed report summaries without model frames
build_intake_preparation_results <- function(prepared, codebook, analysis_plan) {
  politics <- describe_sample_politics_for_report(prepared$observed, prepared$observed, codebook, analysis_plan)
  politics$histogram_data <- stats::setNames(lapply(seq_len(nrow(politics$histogram_specs)), function(i) {
    specification <- politics$histogram_specs[i, ]
    calculate_histogram_data(politics$scores[[specification$variable]],
      c(specification$response_min, specification$response_max), specification$bin_width)
  }), politics$histogram_specs$variable)
  politics$scores <- NULL
  attribute_names <- c("reversed_items", "quota_group_computed", "quota_group_mismatch",
    "imputation_model_status", "blocked_imputation_predictions",
    "unavailable_imputation_variables", "unavailable_imputation_variables_known_gender")
  list(exclusions = prepared$exclusions, imputation = prepared$imputation,
    imputation_demographic_predictors = prepared$imputation_demographic_predictors,
    gender_levels = levels(prepared$data$gender),
    analysis_attributes = stats::setNames(lapply(attribute_names,
      function(name) attr(prepared$data, name, exact = TRUE)), attribute_names),
    observed_item_distributions = prepared$observed_item_distributions,
    political_sample = politics)
}
