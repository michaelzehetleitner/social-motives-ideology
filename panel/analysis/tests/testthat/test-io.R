# R/io_qualtrics.R: raw schema, reading SAV exports, writing the SAV layout

local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) {
    parent <- dirname(dir)
    if (identical(parent, dir)) stop("project root not found from ", getwd())
    dir <- parent
  }
  for (f in c("config.R", "io_qualtrics.R", "simulate_testdata.R")) {
    source(file.path(dir, "R", f), local = FALSE)
  }
})

root <- zm_root()
analysis_plan <- zm_config(profile = "full", path = file.path(root, "config", "analysis_plan.yaml"))
codebook <- zm_codebook(analysis_plan)
schema <- zm_raw_schema()
sav_path <- file.path(root, "data", "synthetic", "zm_panel_synthetic.sav")
template_path <- file.path(root, "data", "synthetic", "template", "ZM-ASC-panel_random-data_2026-09-05.sav")

test_that("zm_raw_schema() describes 97 columns with valid roles and types", {
  expect_equal(nrow(schema), 97)
  expect_equal(anyDuplicated(schema$name), 0)
  expect_equal(anyDuplicated(schema$qualtrics_name), 0)
  expect_true(all(schema$role %in% c("meta", "flow", "consent", "age", "politics", "item", "attention", "demo", "comment")))
  expect_true(all(schema$type %in% c("datetime", "character", "integer", "numeric")))
  expect_equal(schema$name, zm_normalise_names(schema$qualtrics_name))
  expect_equal(sum(!schema$required), 2)
  expect_setequal(schema$name[!schema$required], c("Last_Seen_Flow_Element_ID", "Last_Seen_Question_IDs"))
  expect_true(all(grepl("ImportId", schema$import_id, fixed = TRUE)))
  expect_false(any(is.na(schema$label)))
})

test_that("schema item columns are exactly the codebook scale items", {
  expect_setequal(schema$name[schema$role == "item"], unlist(codebook$scales$item_codes))
  expect_equal(sum(schema$role == "item"), sum(codebook$scales$item_count))
  expect_true(all(schema$type[schema$role %in% c("item", "attention", "age", "demo")] == "numeric"))
  expect_true(all(schema$type[schema$role == "flow"] == "character"))
})

test_that("read_qualtrics_export() reads the synthetic SAV export with schema names and types", {
  skip_if_not(file.exists(sav_path), "synthetic export not generated (run scripts/make_synthetic_data.R)")
  d <- read_qualtrics_export(sav_path)
  expect_s3_class(d, "tbl_df")
  expect_equal(names(d), schema$name)
  expect_equal(ncol(d), 97)
  expect_true(all(vapply(d[schema$name[schema$role == "item"]], is.numeric, logical(1))))
  expect_true(all(vapply(d[c("attentioncheck_1", "attentioncheck_2", "demo_age", "pol_symp_afd", "demo_gender")], is.numeric, logical(1))))
  expect_type(d$Finished, "integer")
  expect_type(d$Progress, "integer")
  expect_true(all(d$Finished %in% c(0L, 1L)))
  expect_type(d$survey_status, "character")
  expect_type(d$quota_group, "character")
  expect_type(d$bilendi_id, "character")
  expect_type(d$ResponseId, "character")
  expect_type(d$Status, "character")
  for (col in c("StartDate", "EndDate", "RecordedDate")) {
    expect_s3_class(d[[col]], "POSIXct")
    expect_equal(attr(d[[col]], "tzone"), "Europe/Berlin")
    expect_false(anyNA(d[[col]]))
  }
  labs <- attr(d, "labels")
  expect_type(labs, "list")
  expect_equal(names(labs), schema$name)
  # the question texts are the template export's; the SAV keeps line breaks the CSV header flattens
  expect_equal(labs, attr(read_qualtrics_export(template_path), "labels"))
  expect_equal(gsub("\\s+", " ", labs$SDO_D_1), gsub("\\s+", " ", schema$label[schema$name == "SDO_D_1"]))
  expect_silent(assert_raw_schema(d))
})

test_that("the synthetic export and the random-data template give the same 97 normalised names", {
  skip_if_not(file.exists(sav_path), "synthetic export not generated")
  d_synthetic <- read_qualtrics_export(sav_path)
  d_template <- read_qualtrics_export(template_path)
  expect_equal(names(d_template), names(d_synthetic))
  expect_equal(names(d_template), schema$name)
  expect_length(names(d_template), 97)
  expect_true(all(vapply(d_template[schema$name[schema$role == "item"]], is.numeric, logical(1))))
  expect_type(d_template$Finished, "integer")
  expect_s3_class(d_template$StartDate, "POSIXct")
  expect_type(d_template$survey_status, "character")
  expect_silent(assert_raw_schema(d_template))
  expect_true(all(zm_status_label(d_template$Status) %in% c("IP Address", "Survey Preview", "Survey Test")))
})

test_that("assert_raw_schema() stops on missing required columns and non-numeric items", {
  skip_if_not(file.exists(sav_path), "synthetic export not generated")
  d <- read_qualtrics_export(sav_path)
  expect_error(assert_raw_schema(d[, setdiff(names(d), "UMS_ach_1")]), "UMS_ach_1")
  expect_error(assert_raw_schema(d[, setdiff(names(d), c("survey_status", "demo_age"))]), "survey_status")
  bad <- d
  bad$SDO_D_3 <- as.character(bad$SDO_D_3)
  expect_error(assert_raw_schema(bad), "SDO_D_3")
  # an export without the two optional Last_Seen_* columns is valid
  live_layout <- d[, schema$name[schema$required]]
  expect_silent(assert_raw_schema(live_layout))
  expect_identical(assert_raw_schema(d), d)
})

test_that("write -> read round trip preserves values, NA and types, and carries the template labels", {
  skip_if_not(file.exists(sav_path), "synthetic export not generated")
  d <- read_qualtrics_export(sav_path)
  sub <- d[1:40, ]
  tmp <- withr::local_tempfile(fileext = ".sav")
  write_qualtrics_sav(sub, tmp, template_path)
  back <- read_qualtrics_export(tmp)
  expect_equal(attr(back, "labels"), attr(read_qualtrics_export(template_path), "labels"))
  expect_equal(names(back), names(sub))
  for (col in names(sub)) {
    if (inherits(sub[[col]], "POSIXct")) {
      expect_equal(as.numeric(back[[col]]), as.numeric(sub[[col]]), info = col)
    } else {
      expect_equal(back[[col]], sub[[col]], info = col)
    }
  }
  # an NA numeric cell stays NA and an empty text cell stays empty
  expect_identical(is.na(back$pol_left_right), is.na(sub$pol_left_right))
  expect_identical(back$survey_status == "", sub$survey_status == "")
})

test_that("coercion parses numeric text and maps Status codes and labels", {
  x <- tibble::tibble(Finished = c("1", "0", "1", ""), Progress = c("100", "40", "100", "7"),
                      Status = c("0", "1", "IP Address", "2"), UMS_ach_1 = c("5", "", "3", "NA"))
  out <- zm_coerce_export(x, labels = as.list(setNames(names(x), names(x))))
  expect_equal(out$Finished, c(1L, 0L, 1L, NA))
  expect_equal(out$Progress, c(100L, 40L, 100L, 7L))
  expect_equal(out$UMS_ach_1, c(5, NA, 3, NA))
  expect_equal(zm_status_label(out$Status), c("IP Address", "Survey Preview", "IP Address", "Survey Test"))
  lab <- haven::labelled(c(0, 2), labels = c("IP Address" = 0, "Survey Test" = 2))
  expect_equal(zm_status_label(lab), c("IP Address", "Survey Test"))
})

test_that("read_qualtrics_export() rejects unsupported formats", {
  expect_error(read_qualtrics_export(file.path(root, "does_not_exist.csv")), "not found")
  tmp <- withr::local_tempfile(fileext = ".txt")
  writeLines("a,b", tmp)
  expect_error(read_qualtrics_export(tmp), "Unsupported")
})

test_that("assert_raw_schema() refuses an export whose forced items did not parse", {
  raw <- read_qualtrics_export(file.path(zm_root(), "data", "synthetic", "zm_panel_synthetic.sav"))
  expect_silent(assert_raw_schema(raw))

  # a choice-text export: the reader coerces the labels to NA, so the column is
  # numeric and empty rather than character (that is why the type check misses it)
  text_export <- raw
  for (v in c("SDO_D_1", "SDO_D_2", "ASC_aag_1")) text_export[[v]] <- NA_real_
  expect_true(all(vapply(text_export[c("SDO_D_1", "SDO_D_2", "ASC_aag_1")], is.numeric, logical(1))))
  expect_error(assert_raw_schema(text_export), "no value at all in 3 forced column\\(s\\)")
  expect_error(assert_raw_schema(text_export), "choice text rather than recoded numbers")

  # an optional numeric column may legitimately be empty
  optional_empty <- raw
  optional_empty$pol_left_right <- NA_real_
  expect_silent(assert_raw_schema(optional_empty))
})
