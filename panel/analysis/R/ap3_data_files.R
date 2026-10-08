# One-time intake decisions, the filled scientific-use allowlist and AP3 records.
# Linked raw demographics exist only during preparation. The independently
# shuffled observed demographics and masked private comments are written by
# io_clean_intake.R before the verified original is removed.

#' The configured intake deletion lists
#'
#' @param analysis_plan Configuration from [zm_config()].
#' @return `list(delete_always, delete_if_present, delete_after_billing)` of
#'   character vectors (normalised column names).
ap3_intake_lists <- function(analysis_plan) {
  intake <- analysis_plan$data_files$intake
  if (is.null(intake)) stop("AP3: analysis_plan$data_files$intake is missing from the analysis plan.")
  lists <- list(
    delete_always = zm_normalise_names(as.character(unlist(intake$delete_always))),
    delete_if_present = zm_normalise_names(as.character(unlist(intake$delete_if_present))),
    delete_after_billing = zm_normalise_names(as.character(unlist(intake$delete_after_billing)))
  )
  all <- unlist(lists, use.names = FALSE)
  if (anyDuplicated(all) > 0) {
    stop("AP3: a column appears in more than one intake deletion list: ", paste(unique(all[duplicated(all)]), collapse = ", "))
  }
  lists
}

#' AP3 — propose the intake deletion for a raw export
#'
#' One row per raw column (export order) plus one row per configured
#' deletion candidate absent from the export (`present = FALSE`). `reason`
#' is the deletion list the column belongs to, or `keep`; a raw column that
#' is neither listed nor part of the export schema ([zm_raw_schema()]) is
#' proposed as `delete_if_present` with a note, so that an unexpected
#' identifying column (IP, geolocation, contact fields) cannot slip through
#' unnoticed. `decision` is the proposed decision (`delete` for the three
#' deletion reasons, else `keep`).
#'
#' @param raw Tibble from [read_qualtrics_export()].
#' @param analysis_plan Configuration from [zm_config()].
#' @return Tibble with columns `column`, `reason` (`delete_always` /
#'   `delete_if_present` / `delete_after_billing` / `keep`), `decision`
#'   (`delete` / `keep`), `present`, `in_schema`, `note`.
ap3_intake_proposal <- function(raw, analysis_plan) {
  cols <- names(raw)
  lists <- ap3_intake_lists(analysis_plan)
  schema <- zm_raw_schema()$name
  reason_of <- function(col) {
    if (col %in% lists$delete_always) return("delete_always")
    if (col %in% lists$delete_if_present) return("delete_if_present")
    if (col %in% lists$delete_after_billing) return("delete_after_billing")
    if (!col %in% schema) return("delete_if_present")
    "keep"
  }
  rows <- lapply(cols, function(col) {
    in_schema <- col %in% schema
    listed <- col %in% unlist(lists, use.names = FALSE)
    tibble::tibble(
      column = col,
      reason = reason_of(col),
      present = TRUE,
      in_schema = in_schema,
      note = if (!in_schema && !listed) {
        "not in the export schema (zm_raw_schema) and not on any list: proposed for deletion; change to keep only after review"
      } else {
        NA_character_
      }
    )
  })
  absent <- setdiff(unlist(lists, use.names = FALSE), cols)
  rows <- c(rows, lapply(absent, function(col) {
    tibble::tibble(
      column = col, reason = reason_of(col), present = FALSE,
      in_schema = col %in% schema, note = "listed in analysis_plan$data_files$intake but absent from this export"
    )
  }))
  out <- dplyr::bind_rows(rows)
  out$decision <- ifelse(out$reason == "keep", "keep", "delete")
  out[, c("column", "reason", "decision", "present", "in_schema", "note"), drop = FALSE]
}

#' Write the intake proposal as YAML
#'
#' The file carries a comment header explaining the approval step and a
#' `columns` list (one entry per row of the proposal). `yaml::as.yaml`
#' writes the body; the header is prepended as comments.
#'
#' @param proposal Tibble from [ap3_intake_proposal()].
#' @param analysis_plan Configuration from [zm_config()].
#' @param path Output path (default `analysis_plan$data_files$intake$proposal_file`
#'   under the project root).
#' @param data_source `"synthetic"` or `"real"`, recorded in the file so
#'   that an approval written for one export cannot be applied to another.
#' @return `path`, invisibly.
ap3_intake_write_proposal <- function(proposal, analysis_plan,
                                      path = file.path(analysis_plan$root, analysis_plan$data_files$intake$proposal_file),
                                      data_source = NA_character_) {
  approval_file <- analysis_plan$data_files$intake$approval_file
  header <- c(
    "# intake_proposal.yaml — written by scripts/prepare_intake.R --propose (ap3_intake_write_proposal); do not edit.",
    "# Step 1 of AP3 (analysis_plan$data_files$intake): the columns of the raw export with the",
    "# proposed decision. To approve: copy this file to",
    paste0("#   ", approval_file),
    "# set `decision` of every column to keep or delete (the column set must equal",
    "# the raw columns, every column exactly once), keep `data_source`, rerun",
    "# scripts/prepare_intake.R without --propose.",
    "# reason: delete_always / delete_if_present / delete_after_billing / keep.",
    paste0("# ", nrow(proposal), " columns listed; generated ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), ".")
  )
  body <- list(
    data_source = if (is.na(data_source)) NULL else data_source,
    n_raw_columns = sum(proposal$present),
    columns = lapply(seq_len(nrow(proposal)), function(i) {
      list(
        column = proposal$column[i],
        decision = proposal$decision[i],
        reason = proposal$reason[i],
        present = isTRUE(proposal$present[i]),
        in_schema = isTRUE(proposal$in_schema[i]),
        note = if (is.na(proposal$note[i])) NULL else proposal$note[i]
      )
    })
  )
  body <- body[!vapply(body, is.null, logical(1))]
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  writeLines(c(header, "", yaml::as.yaml(body)), path)
  invisible(path)
}

#' Read an intake approval (or proposal) file
#'
#' @param path Path of the YAML file.
#' @return Tibble `column`, `decision`, `reason`, `present`, `note` with
#'   attribute `data_source` (character or `NA`).
ap3_intake_read_approval <- function(path) {
  y <- yaml::read_yaml(path)
  cols <- y$columns
  if (is.null(cols) || length(cols) == 0) {
    stop("AP3: ", path, " has no `columns` list.")
  }
  field <- function(entry, name, default = NA) {
    v <- entry[[name]]
    if (is.null(v) || length(v) == 0) default else v
  }
  out <- dplyr::bind_rows(lapply(cols, function(entry) {
    tibble::tibble(
      column = as.character(field(entry, "column", NA_character_)),
      decision = as.character(field(entry, "decision", NA_character_)),
      reason = as.character(field(entry, "reason", NA_character_)),
      present = as.logical(field(entry, "present", NA)),
      note = as.character(field(entry, "note", NA_character_))
    )
  }))
  attr(out, "data_source") <- if (is.null(y$data_source)) NA_character_ else as.character(y$data_source)
  out
}

#' AP3 — check the manual intake approval against the raw export
#'
#' Stops with a clear message unless the approval file exists, lists every
#' raw column exactly once with decision `keep` or `delete`, and its column
#' set equals the raw columns (candidates absent from the export may be
#' listed with `present: no`; they are ignored). When the file records a
#' `data_source` and `data_source` is given, both must agree.
#'
#' @param raw Tibble from [read_qualtrics_export()].
#' @param analysis_plan Configuration from [zm_config()].
#' @param data_source Optional `"synthetic"` / `"real"` of this run.
#' @param path Approval file (default `analysis_plan$data_files$intake$approval_file`
#'   under the project root).
#' @return Tibble of the approved decisions for the raw columns: `column`,
#'   `decision`, `reason`, `note` (raw column order).
ap3_intake_check_approval <- function(raw, analysis_plan, data_source = NULL,
                                      path = file.path(analysis_plan$root, analysis_plan$data_files$intake$approval_file)) {
  intake <- analysis_plan$data_files$intake
  if (!isTRUE(intake$require_manual_approval)) {
    stop("AP3: analysis_plan$data_files$intake$require_manual_approval must be true; the pipeline does not delete columns without the approval file.")
  }
  if (!file.exists(path)) {
    stop(
      "intake approval missing: copy ", intake$proposal_file, " to ", intake$approval_file,
      ", edit the decisions, rerun (looked for ", path, ")."
    )
  }
  approved <- ap3_intake_read_approval(path)
  file_source <- attr(approved, "data_source", exact = TRUE)
  if (!is.null(data_source) && !is.na(file_source) && !identical(file_source, data_source)) {
    stop(
      "AP3: ", path, " was approved for data_source '", file_source,
      "' but this run uses '", data_source, "'; write and approve a new intake proposal for this export."
    )
  }
  if (anyNA(approved$column) || any(!nzchar(approved$column))) {
    stop("AP3: ", path, " has an entry without a column name.")
  }
  dups <- unique(approved$column[duplicated(approved$column)])
  if (length(dups) > 0) {
    stop("AP3: ", path, " lists column(s) more than once: ", paste(dups, collapse = ", "))
  }
  bad <- approved$column[!approved$decision %in% c("keep", "delete")]
  if (length(bad) > 0) {
    stop("AP3: ", path, ": decision must be keep or delete for ", paste(bad, collapse = ", "))
  }
  listed_absent <- approved$column[approved$present %in% FALSE]
  raw_cols <- names(raw)
  not_approved <- setdiff(raw_cols, approved$column)
  extra <- setdiff(setdiff(approved$column, raw_cols), listed_absent)
  if (length(not_approved) > 0 || length(extra) > 0) {
    stop(
      "AP3: ", path, " does not match the raw columns.",
      if (length(not_approved) > 0) paste0(" Raw columns without a decision: ", paste(not_approved, collapse = ", "), ".") else "",
      if (length(extra) > 0) paste0(" Approved columns absent from the export: ", paste(extra, collapse = ", "), ".") else "",
      " Write a new proposal for this export and approve it."
    )
  }
  out <- approved[match(raw_cols, approved$column), c("column", "decision", "reason", "note"), drop = FALSE]
  attr(out, "data_source") <- file_source
  out
}

#' AP3 — apply the approved intake deletion
#'
#' Removes every column whose approved decision is `delete` and adds
#' `respondent_id`, a participant number assigned in random order (first column;
#' `analysis_plan$data_files$intake$respondent_id`). Columns that AP1 still reads for
#' the exclusions (`defer`, default [ap1_required_columns()]) are kept for
#' now and listed in the attribute `intake_deferred`;
#' [apply_study_exclusions()] removes them from its output. The
#' attribute `intake_deleted` lists every approved deletion.
#'
#' @param raw Tibble from [read_qualtrics_export()].
#' @param approved Tibble from [ap3_intake_check_approval()].
#' @param analysis_plan Configuration from [zm_config()].
#' @param defer Character vector of columns whose deletion is deferred until
#'   after AP1.
#' @return `raw` minus the deleted columns, plus `respondent_id`.
ap3_intake_apply <- function(raw, approved, analysis_plan, defer = ap1_required_columns()) {
  raw_cols <- names(raw)
  delete <- approved$column[approved$decision == "delete" & approved$column %in% raw_cols]
  deferred <- intersect(delete, defer)
  drop_now <- setdiff(delete, deferred)
  out <- raw[, setdiff(raw_cols, drop_now), drop = FALSE]
  out <- tibble::add_column(tibble::as_tibble(out), respondent_id = randomise_intake_rows(nrow(raw)), .before = 1)
  labels <- attr(raw, "labels", exact = TRUE)
  if (!is.null(labels)) attr(out, "labels") <- labels[intersect(names(out), names(labels))]
  attr(out, "intake_deleted") <- delete
  attr(out, "intake_deferred") <- deferred
  out
}

#' The six party scalometer columns of the export schema
#'
#' @return Character vector (`pol_symp_*` in schema order).
ap3_pol_symp_columns <- function() {
  schema <- zm_raw_schema()
  schema$name[startsWith(schema$name, "pol_symp_")]
}

#' Expand an allowlist to column names
#'
#' `items` expands to the codebook item codes (57), `scale_scores` to the
#' nine scale keys, `pol_symp` to the six scalometers; every other entry is
#' a column name. A group keyword that expands to nothing (an empty codebook
#' subset) stops the pipeline: it would otherwise silently produce a data
#' file without the columns it is named for.
#'
#' @param keep Character vector (an allowlist from `analysis_plan$data_files`).
#' @param codebook Codebook from [zm_codebook()].
#' @return Character vector of column names, in allowlist order, unique.
ap3_allowlist_expand <- function(keep, codebook) {
  keep <- as.character(unlist(keep))
  group <- function(k, cols) {
    cols <- as.character(cols)
    if (length(cols) == 0) {
      stop(
        "AP3: ap3_allowlist_expand(): the allowlist entry '", k,
        "' expands to no column — the codebook subset it names is empty."
      )
    }
    cols
  }
  out <- unlist(lapply(keep, function(k) {
    switch(
      k,
      items = group(k, unique(unlist(codebook$scales$item_codes))),
      scale_scores = group(k, codebook$scales$scale_key),
      pol_symp = group(k, ap3_pol_symp_columns()),
      k
    )
  }))
  if (is.null(out)) out <- character(0)
  unique(out)
}

#' Restrict the analysis data to an allowlist
#'
#' @param analysis_data Tibble (AP1 sample with items, scores, covariates).
#' @param keep Allowlist (before expansion).
#' @param codebook Codebook from [zm_codebook()].
#' @return Tibble with exactly the expanded allowlist columns, in that order.
ap3_allowlist_apply <- function(analysis_data, keep, codebook) {
  cols <- ap3_allowlist_expand(keep, codebook)
  tibble::as_tibble(as.data.frame(analysis_data)[, cols, drop = FALSE])
}

# BEGIN GENERATED PARAMETER CARD: AP3 SCIENTIFIC USE FILE
# Automatically generated from analysis_plan.yaml
#   Shared: respondent_id, items, scale_scores, demo_gender, age_band, age_band_midpoint,
#     income_band, income_band_value, quota_group, attentioncheck_1, attentioncheck_2, pol_symp,
#     pol_party_vote
#     (data_files.scientific_use_file.keep)
#   Shared exactly as the analyses hold them: age_band_midpoint, income_band_value   (data_files.scientific_use_file.analysis_value_columns)
#   Not shared: demo_age, income, demo_edu_school, east_west, demo_bundesland, demo_income_hh_net,
#     demo_hh_members, Duration__in_seconds_, Kommentarfeld, pol_party_vote_801_TEXT,
#     pol_vote_would, pol_left_right
#     (data_files.scientific_use_file.drop)
# END GENERATED PARAMETER CARD: AP3 SCIENTIFIC USE FILE
#' AP3 — the scientific-use file (allowlist `analysis_plan$data_files$scientific_use_file$keep`)
#'
#' The demographic columns are gender, the age band and the income band per
#' household member, each band as its label and its analysis value; the
#' analysis values (`analysis_value_columns`) are shared exactly as the analysis
#' data hold them. The `drop` list, expanded like `keep`, is asserted absent.
#'
#' @param analysis_data The filled analysis table prepared at intake.
#' @param analysis_plan Configuration from [zm_config()].
#' @param codebook Codebook from [zm_codebook()].
#' @return Tibble restricted to the allowlist.
ap3_scientific_use_data <- function(analysis_data, analysis_plan, codebook = zm_codebook(analysis_plan)) {
  suf <- analysis_plan$data_files$scientific_use_file
  if (is.null(suf$keep)) stop("AP3: analysis_plan$data_files$scientific_use_file$keep is missing from the analysis plan.")
  cols <- ap3_allowlist_expand(suf$keep, codebook)
  analysis_value_columns <- as.character(unlist(suf$analysis_value_columns))
  if (length(analysis_value_columns) == 0L || !all(analysis_value_columns %in% cols)) {
    stop("AP3: analysis_plan$data_files$scientific_use_file$analysis_value_columns must name shared columns.")
  }
  drop <- ap3_allowlist_expand(suf$drop, codebook)
  clash <- intersect(drop, cols)
  if (length(clash) > 0) {
    stop("AP3: analysis_plan$data_files$scientific_use_file lists column(s) both in keep and drop: ", paste(clash, collapse = ", "))
  }
  out <- ap3_allowlist_apply(analysis_data, suf$keep, codebook)
  present_drop <- intersect(drop, names(out))
  if (length(present_drop) > 0) {
    stop("AP3: scientific-use file would contain dropped column(s): ", paste(present_drop, collapse = ", "))
  }
  for (column in analysis_value_columns) {
    if (!isTRUE(all.equal(out[[column]], analysis_data[[column]], check.attributes = FALSE))) {
      stop(
        "AP3: scientific-use file: ", column,
        " differs from the analysis data; the shared file must carry the analysis value."
      )
    }
  }
  out
}

#' Write a data file as CSV
#'
#' @param data Tibble.
#' @param path Output path; the directory is created.
#' @return `path`.
ap3_write_data_file <- function(data, path) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  readr::write_csv(data, path, na = "", progress = FALSE)
  path
}

#' The configured path of a data file
#'
#' Every file the pipeline writes under `data/derived` names its location in
#' `analysis_plan$data_files$<entry>$path`, relative to the project root, so that the
#' analysis plan and not the code decides where a file goes.
#'
#' @param analysis_plan Configuration from [zm_config()].
#' @param entry Name of the `analysis_plan$data_files` entry (`scientific_use_file`, `filled_cells_file`,
#'   `dropped_respondents_file`).
#' @return Absolute path (character scalar).
ap3_data_file_path <- function(analysis_plan, entry) {
  path <- analysis_plan$data_files[[entry]]$path
  if (is.null(path) || !is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    stop("AP3: analysis_plan$data_files$", entry, "$path is missing from the analysis plan.")
  }
  file.path(analysis_plan$root, path)
}

#' AP3 — write the scientific-use file
#'
#' The shared file contains the retained, filled analysis
#' table after the registered item and demographic fills.
#'
#' @param analysis_data Filled, retained analysis table from intake.
#' @param analysis_plan Configuration from [zm_config()].
#' @param codebook Codebook from [zm_codebook()].
#' @param path Output path (default `analysis_plan$data_files$scientific_use_file$path`
#'   under the project root; git-ignored).
#' @return The path (character scalar).
write_scientific_use_file <- function(analysis_data, analysis_plan, codebook = zm_codebook(analysis_plan),
                                    path = ap3_data_file_path(analysis_plan, "scientific_use_file")) {
  ap3_write_data_file(ap3_scientific_use_data(analysis_data, analysis_plan, codebook), path)
}

#' AP3 — the filled cells as a table, from the imputation record
#'
#' @param imputation_reporting_data From [build_imputation_reporting_data()].
#' @return Tibble `respondent_id` (integer), `variable`, `kind`, `model`
#'   (character), `value`, `lower`, `upper` (double), `n_fit_rows` (integer);
#'   the same eight columns when nothing was imputed.
build_filled_cells_table <- function(imputation_reporting_data) {
  cells <- imputation_reporting_data$imputed_cells
  tibble::tibble(
    respondent_id = as.integer(cells$respondent_id),
    variable = as.character(cells$variable),
    kind = as.character(cells$kind),
    model = as.character(cells$model),
    value = as.numeric(cells$value),
    lower = as.numeric(cells$lower),
    upper = as.numeric(cells$upper),
    n_fit_rows = as.integer(cells$n_fit_rows)
  )
}

#' AP3 — the dropped respondents as a table, from the imputation record
#'
#' The reason names the threshold(s) the participant exceeded; a participant
#' over both is reported with both, joined by "; ". The item reason names the
#' configured maximum and the scales the participant exceeded, the demographic
#' reason the count of the three demographics that are missing.
#'
#' @param imputation_reporting_data From [build_imputation_reporting_data()].
#' @param analysis_plan Configuration from [zm_config()]; supplies the stated maximum.
#' @return Tibble `respondent_id`, `reason`.
build_dropped_respondents_table <- function(imputation_reporting_data, analysis_plan) {
  excluded <- imputation_reporting_data$excluded_participants
  max_items <- analysis_plan$missing_data$fill$drop$items_per_scale_more_than
  item_reason <- ifelse(excluded$too_many_items_missing,
                        paste0("more than ", max_items, " missing items in ",
                               excluded$scales_over_threshold), NA_character_)
  demographic_reason <- ifelse(excluded$too_many_demographics_missing,
                               paste0(excluded$n_demographics_missing,
                                      " of 3 demographics missing"), NA_character_)
  reason <- ifelse(
    is.na(item_reason), demographic_reason,
    ifelse(is.na(demographic_reason), item_reason,
           paste(item_reason, demographic_reason, sep = "; "))
  )
  tibble::tibble(respondent_id = excluded$respondent_id, reason = as.character(reason))
}

#' AP3 — write the filled cells
#'
#' One row per cell the imputation wrote, as [build_filled_cells_table()]
#' builds it: the
#' respondent, the variable, the model that produced the value, the value and
#' its posterior predictive interval.
#'
#' @param filled_cells Tibble from [build_filled_cells_table()].
#' @param analysis_plan Configuration from [zm_config()].
#' @param path Output path (default `analysis_plan$data_files$filled_cells_file$path`
#'   under the project root; git-ignored).
#' @return The path (character scalar).
write_filled_cells_file <- function(filled_cells, analysis_plan,
                                  path = ap3_data_file_path(analysis_plan, "filled_cells_file")) {
  ap3_write_data_file(tibble::as_tibble(filled_cells), path)
}

#' AP3 — write the dropped respondents
#'
#' One row per respondent the drop rule removed from all analyses instead of
#' filling, with the reason, as [build_dropped_respondents_table()] builds it.
#'
#' @param dropped_respondents Tibble from [build_dropped_respondents_table()].
#' @param analysis_plan Configuration from [zm_config()].
#' @param path Output path (default
#'   `analysis_plan$data_files$dropped_respondents_file$path` under the project root;
#'   git-ignored).
#' @return The path (character scalar).
write_dropped_respondents_file <- function(dropped_respondents, analysis_plan,
                                         path = ap3_data_file_path(analysis_plan, "dropped_respondents_file")) {
  ap3_write_data_file(tibble::as_tibble(dropped_respondents), path)
}


#' Random row order independent of the public analysis seed
#'
#' Operating-system entropy is consumed directly; neither keys nor a reversible
#' seed/permutation record is retained. Used separately for participant numbers
#' and for each unlinked demographic column.
randomise_intake_rows <- function(n) {
  connection <- file("/dev/urandom", "rb", raw = TRUE)
  on.exit(close(connection))
  bytes <- readBin(connection, what = "raw", n = 8L * n)
  if (length(bytes) != 8L * n) stop("Could not obtain independent intake randomness.")
  keys <- vapply(seq_len(n), function(i) {
    paste0(as.character(bytes[seq.int(8L * (i - 1L) + 1L, 8L * i)]), collapse = "")
  }, character(1))
  if (anyDuplicated(keys)) stop("Intake randomness collided; rerun preparation.")
  order(keys)
}
