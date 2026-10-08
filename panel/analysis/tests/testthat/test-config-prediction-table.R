# config/analysis_plan.yaml predictions.table is Table 3 of the
# preregistration: the test reads the pipe table above the caption labelled
# #tbl-predicted-signs in preregistration.qmd and compares it with the
# configuration cell by cell, so that a change to either one fails here.

local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) {
    parent <- dirname(dir)
    if (identical(parent, dir)) stop("project root not found from ", getwd())
    dir <- parent
  }
  source(file.path(dir, "R", "config.R"), local = FALSE)
})

prediction_plan <- zm_config(profile = "full", path = file.path(zm_root(), "config", "analysis_plan.yaml"))
# The preregistration sits in the directory the codebooks are read from.
prereg_path <- file.path(prediction_plan$root, prediction_plan$meta$codebook_dir, "preregistration.qmd")

# The labels of Table 3 and the configuration keys they stand for.
prereg_motive_keys <- c("Security motive" = "zm_security", "Arousal motive" = "zm_arousal", "Power" = "zm_power",
                        "Prestige" = "zm_prestige", "Achievement" = "zm_achievement")
prereg_outcome_keys <- c("Conventionalism" = "asc_conv", "Authoritarian submission" = "asc_sub",
                         "Authoritarian aggression" = "asc_agg", "SDO-D" = "sdo_dom")

# Table 3 as one row per cell: `motive` and `outcome` as Table 3 labels them,
# their configuration keys, and the sign written as the configuration writes
# it ("−" becomes "-"). A motive label loses a parenthesised suffix such as
# "(need for security)"; whitespace around cells and labels is ignored.
read_predicted_signs_table <- function(lines) {
  caption <- grep("^\\s*:.*\\{#tbl-predicted-signs[ }]", lines)
  if (length(caption) != 1L) stop("Expected one Table 3 caption, found ", length(caption), ".")
  above <- rev(trimws(lines[seq_len(caption - 1L)]))
  last <- which(nzchar(above))[1]
  if (is.na(last) || !startsWith(above[last], "|")) stop("No pipe table above the Table 3 caption.")
  above <- above[last:length(above)]
  table_lines <- rev(above[seq_len(match(FALSE, startsWith(above, "|"), nomatch = length(above) + 1L) - 1L)])
  cells <- lapply(table_lines, function(line) {
    inner <- sub("^\\|", "", sub("\\|$", "", line))
    trimws(strsplit(inner, "|", fixed = TRUE)[[1]])
  })
  header <- cells[[1]]
  body <- cells[-(1:2)]  # the second line is the alignment row
  outcomes <- header[-1]
  do.call(rbind, lapply(body, function(row) {
    motive <- trimws(sub("\\s*\\([^)]*\\)\\s*$", "", row[1]))
    sign <- gsub("−", "-", row[-1], fixed = TRUE)
    data.frame(motive = motive, outcome = outcomes,
               motive_key = unname(prereg_motive_keys[motive]),
               outcome_key = unname(prereg_outcome_keys[outcomes]),
               sign = sign, stringsAsFactors = FALSE)
  }))
}

# The cells in which Table 3 and the configured table differ, each named as
# "motive -> outcome (keys): Table 3 'x', configuration 'y'"; empty when they agree.
differing_prediction_cells <- function(prereg_signs, configured) {
  configured_sign <- function(outcome_key, motive_key) {
    value <- configured[[outcome_key]][[motive_key]]
    if (is.null(value)) "(missing)" else as.character(value)
  }
  out <- character()
  unlabelled <- is.na(prereg_signs$motive_key) | is.na(prereg_signs$outcome_key)
  if (any(unlabelled)) {
    out <- c(out, paste0("label without a configuration key: ",
                         unique(c(prereg_signs$motive[is.na(prereg_signs$motive_key)],
                                  prereg_signs$outcome[is.na(prereg_signs$outcome_key)]))))
  }
  prereg_signs <- prereg_signs[!unlabelled, , drop = FALSE]
  for (i in seq_len(nrow(prereg_signs))) {
    in_config <- configured_sign(prereg_signs$outcome_key[i], prereg_signs$motive_key[i])
    if (!identical(in_config, prereg_signs$sign[i])) {
      out <- c(out, paste0(prereg_signs$motive[i], " -> ", prereg_signs$outcome[i], " (", prereg_signs$motive_key[i], ", ",
                           prereg_signs$outcome_key[i], "): Table 3 '", prereg_signs$sign[i], "', configuration '",
                           in_config, "'"))
    }
  }
  # a configured cell that Table 3 does not have
  for (outcome_key in names(configured)) {
    for (motive_key in names(configured[[outcome_key]])) {
      if (!any(prereg_signs$outcome_key == outcome_key & prereg_signs$motive_key == motive_key)) {
        out <- c(out, paste0(motive_key, " -> ", outcome_key, ": configured, but not in Table 3"))
      }
    }
  }
  out
}

test_that("the configured prediction table equals Table 3 of the preregistration cell by cell", {
  expect_true(file.exists(prereg_path), info = prereg_path)
  prereg_signs <- read_predicted_signs_table(readLines(prereg_path, warn = FALSE, encoding = "UTF-8"))
  # five motives by four facets, every label known, every key a configured one
  expect_equal(nrow(prereg_signs), 20L)
  expect_setequal(unique(prereg_signs$motive_key), as.character(prediction_plan$regression$motives))
  expect_setequal(unique(prereg_signs$outcome_key), as.character(prediction_plan$regression$outcomes))
  differ <- differing_prediction_cells(prereg_signs, prediction_plan$predictions$table)
  expect(length(differ) == 0L,
         paste0("The configured predictions.table differs from Table 3 of the preregistration in ",
                length(differ), " cell(s):\n", paste(differ, collapse = "\n")))
})

test_that("the Table 3 reader ignores whitespace and the motive suffix, and names a differing cell", {
  lines <- c(
    "Text before the table.",
    "",
    "|  Social motive | Conventionalism |Authoritarian submission| Authoritarian aggression | SDO-D |",
    "|---|:---:|:---:|:---:|:---:|",
    "| Power   | + | − | + | + |",
    "|Prestige|±|±|±|±|",
    "| Achievement | − | − | − | − |",
    "| Arousal motive | − | − | − | − |",
    "|  Security motive  (need for security)  | + | + | + | + |",
    "",
    "  : Predicted signs of adjusted associations. {#tbl-predicted-signs tbl-colwidths=\"20,22,23,23,12\"}  ",
    "",
    "- A note below the table."
  )
  prereg_signs <- read_predicted_signs_table(lines)
  expect_equal(nrow(prereg_signs), 20L)
  expect_false(anyNA(prereg_signs$motive_key))
  expect_false(anyNA(prereg_signs$outcome_key))
  expect_identical(prereg_signs$sign[prereg_signs$motive == "Power" & prereg_signs$outcome == "Authoritarian submission"], "-")
  expect_identical(prereg_signs$sign[prereg_signs$motive == "Security motive" & prereg_signs$outcome == "SDO-D"], "+")
  expect_identical(unique(prereg_signs$sign[prereg_signs$motive == "Prestige"]), "±")
  expect_identical(differing_prediction_cells(prereg_signs, prediction_plan$predictions$table), character())
  # one changed cell in the configuration is named with both values
  changed <- prediction_plan$predictions$table
  changed$asc_sub$zm_power <- "+"
  expect_identical(differing_prediction_cells(prereg_signs, changed),
                   "Power -> Authoritarian submission (zm_power, asc_sub): Table 3 '-', configuration '+'")
  # a label Table 3 uses but the configuration has no key for is named as well
  renamed <- lines
  renamed[7] <- "| Accomplishment | − | − | − | − |"
  expect_match(differing_prediction_cells(read_predicted_signs_table(renamed), prediction_plan$predictions$table),
               "label without a configuration key: Accomplishment", fixed = TRUE, all = FALSE)
})
