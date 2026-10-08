# One-time AP1/AP3 preparation and read-only inputs for the analysis pipeline.
# Only prepare_intake.R opens the original export. It writes the filled
# scientific-use data, independently shuffled observed demographics, existing
# preparation results and a verification receipt. Raw model frames are not saved.

zm_clean_intake_source <- function(source) {
  if (length(source) != 1L || is.na(source) || !source %in% c("synthetic", "real"))
    stop("Intake source must be 'synthetic' or 'real'.")
  source
}

zm_clean_intake_paths <- function(source, analysis_plan) {
  source <- zm_clean_intake_source(source)
  base <- file.path(analysis_plan$root, "data", "intake")
  c(data = file.path(base, paste0(source, ".csv")),
    demographics = file.path(base, paste0(source, "_demographics.csv")),
    preparation = file.path(base, paste0(source, "_preparation.rds")),
    receipt = file.path(base, paste0(source, ".yaml")))
}

zm_intake_hash <- function(path) digest::digest(file = path, algo = "sha256")
zm_intake_defer <- function() setdiff(ap1_required_columns(), c("ResponseId", "Status"))
zm_intake_forbidden <- function(analysis_plan) {
  unique(c("ResponseId", "Status", "StartDate", "EndDate", "RecordedDate", "bilendi_id",
    setdiff(unlist(ap3_intake_lists(analysis_plan), use.names = FALSE), zm_intake_defer())))
}

# Required only while the one-time intake is holding the original in memory.
zm_intake_required <- function(analysis_plan) {
  schema <- zm_raw_schema()
  unique(c(ap1_required_columns(intake_verified = TRUE), schema$name[schema$role == "item"],
    ap3_pol_symp_columns(), "demo_income_hh_net", "demo_hh_members", "demo_edu_school",
    "demo_bundesland", "Duration__in_seconds_", "Kommentarfeld", "quota_group",
    "pol_vote_would", "pol_party_vote", "pol_left_right"))
}

zm_intake_check_decisions <- function(approved, analysis_plan) {
  if (!all(c("column", "decision", "reason", "note") %in% names(approved)) ||
      anyNA(approved$column) || any(!nzchar(approved$column)) || anyDuplicated(approved$column) ||
      any(!approved$decision %in% c("keep", "delete")))
    stop("Intake receipt contains invalid or duplicated column decisions.")
  if (any(tolower(approved$column[approved$decision == "keep"]) %in%
      tolower(zm_intake_forbidden(analysis_plan))))
    stop("Intake approval keeps an identifier required to be removed before preparation.")
  retained <- c("respondent_id", approved$column[approved$decision == "keep" |
                                                approved$column %in% zm_intake_defer()])
  if ("respondent_id" %in% approved$column || !all(zm_intake_required(analysis_plan) %in% retained))
    stop("Intake approval removes required analysis, data-file or screening columns.")
  invisible(TRUE)
}

zm_intake_schema <- function(data) {
  lapply(names(data), function(nm) {
    x <- data[[nm]]
    entry <- list(name = nm, type = if (is.factor(x)) "character" else if (is.integer(x)) "integer" else if (is.numeric(x)) "numeric" else "character")
    if (is.factor(x)) {
      entry$levels <- levels(x)
      entry$ordered <- is.ordered(x)
    }
    entry
  })
}

zm_intake_write_receipt <- function(value, path) {
  tmp <- tempfile(".intake-receipt-", tmpdir = dirname(path))
  on.exit(unlink(tmp), add = TRUE)
  yaml::write_yaml(value, tmp)
  if (!file.rename(tmp, path)) stop("Could not finish writing the intake receipt.")
  invisible(path)
}

#' Check the receipt and all prepared inputs before reading any analysis data
check_clean_intake_receipt <- function(receipt_path, data_path, source, analysis_plan, require_ready = TRUE) {
  source <- zm_clean_intake_source(source)
  if (!file.exists(receipt_path)) stop("Prepared intake missing; run scripts/prepare_intake.R first.")
  receipt <- yaml::read_yaml(receipt_path)
  if (!identical(receipt$format, "zm-prepared-intake-v2") || !identical(receipt$data_source, source))
    stop("Intake format or source does not match: prepare the accepted two-file intake before targets.")
  if (identical(analysis_plan$profile_name, "full") && !identical(receipt$profile, "full"))
    stop("Full analysis requires intake prepared with the full imputation profile.")
  if (require_ready && !identical(receipt$state, "ready"))
    stop("Intake is not ready: preparation checks must finish before analysis.")
  if (!all(vapply(receipt$checks[c("raw_schema", "no_test_rows", "unique_response_ids", "read_back")],
                  isTRUE, logical(1))) || length(receipt$checks) < 4L)
    stop("Intake receipt does not confirm the preparation checks.")
  if (!is.character(receipt$input_sha256) || length(receipt$input_sha256) != 1L ||
      !grepl("^[a-f0-9]{64}$", receipt$input_sha256)) stop("Invalid original-file hash.")
  expected <- c("data", "demographics", "preparation")
  if (!identical(names(receipt$files), expected)) stop("Intake file record is incomplete.")
  paths <- vapply(receipt$files, function(entry) file.path(dirname(receipt_path), entry$file), character(1))
  if (!identical(normalizePath(paths[["data"]], mustWork = FALSE), normalizePath(data_path, mustWork = FALSE)))
    stop("Intake data path does not match its receipt.")
  for (name in expected) {
    entry <- receipt$files[[name]]
    if (!identical(entry$file, basename(entry$file)) || !file.exists(paths[[name]]) ||
        !identical(entry$sha256, zm_intake_hash(paths[[name]])))
      stop("Intake hash verification failed for ", name, ".")
  }
  approved <- ap3_intake_read_approval(receipt_path)
  approved <- approved[, c("column", "decision", "reason", "note"), drop = FALSE]
  zm_intake_check_decisions(approved, analysis_plan)
  schema_names <- vapply(receipt$files$data$schema, `[[`, character(1), "name")
  expected_columns <- ap3_allowlist_expand(analysis_plan$data_files$scientific_use_file$keep, zm_codebook(analysis_plan))
  if (!identical(schema_names, expected_columns)) stop("Prepared scientific-use schema does not match the configured columns.")
  receipt$column_decisions <- approved
  receipt$paths <- paths
  receipt
}

check_intake_approval <- function(files, source, analysis_plan) {
  check_clean_intake_receipt(files[["receipt"]], files[["data"]], source, analysis_plan)
}

read_clean_study_data <- function(files) {
  suppressWarnings(readr::read_csv(files[["data"]],
    col_types = readr::cols(.default = readr::col_character()), na = "",
    name_repair = "minimal", show_col_types = FALSE, progress = FALSE))
}
add_col_types <- function(data, schema) {
  for (entry in schema) {
    name <- entry$name; type <- entry$type
    if (type == "character") data[[name]][is.na(data[[name]])] <- ""
    if (type %in% c("integer", "numeric")) {
      value <- suppressWarnings(as.numeric(data[[name]]))
      invalid <- is.na(value) | !is.finite(value) |
        (type == "integer" & value != floor(value))
      if (any(!is.na(data[[name]]) & invalid))
        stop("Clean intake CSV contains an invalid numeric value.")
      data[[name]] <- if (type == "integer") as.integer(value) else value
    }
  }
  data
}


add_factor_levels <- function(data, schema) {
  for (entry in schema) {
    if (is.null(entry$levels)) next
    data[[entry$name]] <- factor(
      data[[entry$name]], levels = entry$levels, ordered = isTRUE(entry$ordered))
  }
  data
}


check_study_data_matches_approval <- function(data, approval, analysis_plan) {
  record <- approval$files$data
  expected <- vapply(record$schema, `[[`, character(1), "name")
  allowed <- ap3_allowlist_expand(analysis_plan$data_files$scientific_use_file$keep, zm_codebook(analysis_plan))
  if (!identical(names(data), expected) || !identical(expected, allowed) ||
      nrow(data) != record$n_rows || nrow(data) < 1L)
    stop("Prepared scientific-use data do not match the approved schema or dimensions.")
  if (anyNA(data$respondent_id) || anyDuplicated(data$respondent_id) ||
      any(data$respondent_id < 1L | data$respondent_id > approval$n_input_rows))
    stop("Prepared participant numbers must be unique numbers assigned at intake.")
  data
}

read_prepared_demographics <- function(approval) {
  record <- approval$files$demographics
  data <- read_clean_study_data(c(data = approval$paths[["demographics"]])) |>
    add_col_types(record$schema) |> add_factor_levels(record$schema)
  if (!identical(names(data), c("age", "income", "education", "east_west", "duration")) ||
      nrow(data) != approval$files$data$n_rows)
    stop("Deidentified demographics must contain five unlinked columns for the retained sample.")
  data
}

zm_validate_clean_intake <- function(data, receipt, analysis_plan, source, require_ready = TRUE) {
  approval <- check_clean_intake_receipt(receipt, data, source, analysis_plan, require_ready)
  out <- read_clean_study_data(c(data = data)) |>
    add_col_types(approval$files$data$schema) |>
    add_factor_levels(approval$files$data$schema) |>
    check_study_data_matches_approval(approval, analysis_plan)
  read_prepared_demographics(approval)
  preparation <- readRDS(approval$paths[["preparation"]])
  if (!all(c("exclusions", "imputation", "gender_levels", "analysis_attributes",
             "observed_item_distributions", "political_sample") %in% names(preparation)))
    stop("Saved preparation results are incomplete.")
  out
}

#' Prepare and verify both data files and the existing AP1/AP3 results
#'
#' Preparation retains the original and private comments. MZ reviews the files,
#' masks identifying text in comments and controls all study-data deletion.
#' Comments are never an input to the registered analyses.
zm_prepare_clean_intake <- function(input, analysis_plan, source,
                                   approval = file.path(analysis_plan$root, analysis_plan$data_files$intake$approval_file),
                                   out_dir = NULL) {
  source <- zm_clean_intake_source(source)
  if (source == "real" && !identical(analysis_plan$profile_name, "full"))
    stop("Real intake requires the full preregistered profile.")
  if (length(input) != 1L || is.na(input) || !file.exists(input) || dir.exists(input) || nzchar(Sys.readlink(input)))
    stop("Intake requires an explicit regular original export file.")
  paths <- zm_clean_intake_paths(source, analysis_plan)
  if (!is.null(out_dir)) {
    if (source == "real") stop("Real intake is written to data/intake/ only.")
    paths <- stats::setNames(file.path(out_dir, basename(paths)), names(paths))
  }
  if (any(file.exists(paths))) stop("Prepared intake already exists; refusing to overwrite.")
  input_hash <- zm_intake_hash(input)
  raw <- read_qualtrics_export(input)
  assert_raw_schema(raw)
  ap1_assert_no_test_rows(raw)
  if (anyNA(raw$ResponseId) || any(!nzchar(trimws(as.character(raw$ResponseId)))) || anyDuplicated(raw$ResponseId))
    stop("Intake original response IDs must be nonmissing, nonblank and unique.")
  approved <- ap3_intake_check_approval(raw, analysis_plan, data_source = source, path = approval)
  if (!identical(attr(approved, "data_source", exact = TRUE), source)) stop("Approval must explicitly name the data source.")
  zm_intake_check_decisions(approved, analysis_plan)
  clean <- ap3_intake_apply(raw, approved, analysis_plan, defer = zm_intake_defer())
  attr(clean, "intake_verified") <- TRUE
  codebook <- zm_codebook(analysis_plan)
  prepared <- prepare_analysis_at_intake(clean, codebook, analysis_plan)
  scientific <- ap3_scientific_use_data(prepared$data, analysis_plan, codebook)
  demographics <- build_deidentified_demographics(prepared$observed)
  results <- build_intake_preparation_results(prepared, codebook, analysis_plan)
  comments <- tibble::tibble(respondent_id = prepared$observed$respondent_id,
                            comment = prepared$observed$Kommentarfeld)
  comments <- comments[!is.na(comments$comment) & nzchar(trimws(comments$comment)), ]
  comments_path <- file.path(analysis_plan$root, "data", "private", paste0(source, "_comments.csv"))
  if (!is.null(out_dir)) comments_path <- file.path(out_dir, paste0(source, "_comments.csv"))
  if (file.exists(comments_path)) stop("Private comments already exist; refusing to overwrite.")
  dir.create(dirname(paths[["data"]]), recursive = TRUE, showWarnings = FALSE)
  lock <- paste0(paths[["data"]], ".preparing")
  if (!dir.create(lock, showWarnings = FALSE)) stop("Another intake preparation or unfinished lock exists.")
  on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  if (any(file.exists(paths))) stop("Prepared intake already exists; refusing to overwrite.")
  ap3_write_data_file(scientific, paths[["data"]])
  ap3_write_data_file(demographics, paths[["demographics"]])
  ap3_write_data_file(comments, comments_path)
  saveRDS(results, paths[["preparation"]], version = 3)
  record <- function(name, data = NULL) {
    value <- list(file = basename(paths[[name]]), sha256 = zm_intake_hash(paths[[name]]))
    if (!is.null(data)) { value$n_rows <- nrow(data); value$schema <- zm_intake_schema(data) }
    value
  }
  receipt <- list(format = "zm-prepared-intake-v2", data_source = source,
    profile = analysis_plan$profile_name, state = "prepared",
    prepared_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    input_sha256 = input_hash, n_input_rows = nrow(raw),
    checks = list(raw_schema = TRUE, no_test_rows = TRUE, unique_response_ids = TRUE, read_back = TRUE),
    files = list(data = record("data", scientific), demographics = record("demographics", demographics),
                 preparation = record("preparation")), comments_file = comments_path,
    columns = lapply(seq_len(nrow(approved)), function(i) as.list(approved[i, ])))
  zm_intake_write_receipt(receipt, paths[["receipt"]])
  readback <- zm_validate_clean_intake(paths[["data"]], paths[["receipt"]], analysis_plan, source, FALSE)
  loaded_demographics <- read_prepared_demographics(check_clean_intake_receipt(
    paths[["receipt"]], paths[["data"]], source, analysis_plan, FALSE))
  loaded_comments <- read_clean_study_data(c(data = comments_path)) |>
    add_col_types(zm_intake_schema(comments))
  if (!isTRUE(all.equal(as.data.frame(comments), as.data.frame(loaded_comments), check.attributes = FALSE)) ||
      !isTRUE(all.equal(as.data.frame(scientific), as.data.frame(readback), check.attributes = FALSE)) ||
      !isTRUE(all.equal(as.data.frame(demographics), as.data.frame(loaded_demographics), check.attributes = FALSE)) ||
      !identical(readRDS(paths[["preparation"]]), results))
    stop("Intake read-back values differ; original retained.")
  if (!identical(zm_intake_hash(input), input_hash)) stop("Original changed during preparation; it was retained.")
  receipt$state <- "ready"
  zm_intake_write_receipt(receipt, paths[["receipt"]])
  invisible(paths)
}
