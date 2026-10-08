# The link between the results report and the preregistration: the report
# holds no copy of a preregistration sentence, only keys, and every key
# resolves against the current preregistration draft through
# rh_prereg_passage(): repetitions are printed from one
# source. A changed preregistration fails here until the report's keys
# follow; a reworded sentence flows into the report at the next render.

local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) dir <- dirname(dir)
  assign("prl_root", dir, envir = .GlobalEnv)
  source(file.path(dir, "R", "report_preregistration.R"), local = FALSE)
})

prl_qmd <- paste(readLines(file.path(prl_root, "report", "results_draft.qmd"), warn = FALSE), collapse = "\n")
prl_prereg <- rh_read_preregistration(file.path(prl_root, "..", "preregistration", "preregistration.qmd"))

# every rh_prereg_passage(prereg, "from"[, "to"]) call of the report, with its keys
prl_calls <- regmatches(prl_qmd, gregexpr('rh_prereg_passage\\(prereg,\\s*"[^"]+"(,\\s*"[^"]+")?', prl_qmd, perl = TRUE))[[1]]
prl_keys <- lapply(prl_calls, function(call) {
  keys <- regmatches(call, gregexpr('"[^"]+"', call, perl = TRUE))[[1]]
  keys <- substr(keys, 2L, nchar(keys) - 1L)
  list(from = keys[[1]], to = if (length(keys) > 1L) keys[[2]] else NULL)
})

test_that("the report quotes the preregistration through keys only", {
  expect_false(grepl("]{.prereg}", prl_qmd, fixed = TRUE))
  expect_gt(length(prl_keys), 2)
  questions <- regmatches(prl_qmd, gregexpr("__Research question\\.__ [^\n]*", prl_qmd, perl = TRUE))[[1]]
  expect_true(all(grepl("rh_prereg_question(prereg,", questions, fixed = TRUE)),
              info = paste("question typed into the report:", paste(questions[!grepl("rh_prereg_question", questions, fixed = TRUE)], collapse = " | ")))
})

test_that("every key of the report resolves against the current preregistration", {
  for (k in prl_keys) {
    passage <- tryCatch(rh_prereg_passage(prl_prereg, k$from, k$to, span = FALSE),
                        error = function(e) structure(conditionMessage(e), class = "prereg_error"))
    expect_false(inherits(passage, "prereg_error"), info = as.character(passage))
    if (!inherits(passage, "prereg_error")) {
      expect_true(startsWith(passage, k$from) || startsWith(tolower(passage), tolower(k$from)), info = k$from)
      if (!is.null(k$to)) expect_true(endsWith(passage, k$to), info = k$to)
    }
  }
})

test_that("every section and question the report copies by heading or label exists", {
  sections <- regmatches(prl_qmd, gregexpr('rh_prereg_under_heading\\(prereg,\\s*"[^"]+",\\s*"[^"]+"', prl_qmd, perl = TRUE))[[1]]
  expect_gt(length(sections), 3)
  for (call in sections) {
    k <- regmatches(call, gregexpr('"[^"]+"', call, perl = TRUE))[[1]]; k <- substr(k, 2L, nchar(k) - 1L)
    out <- tryCatch(rh_prereg_under_heading(prl_prereg, k[[1]], k[[2]]), error = function(e) conditionMessage(e))
    expect_true(startsWith(out, "["), info = paste(k, collapse = " > "))
  }
  questions <- regmatches(prl_qmd, gregexpr('rh_prereg_question\\(prereg,\\s*"[^"]+"', prl_qmd, perl = TRUE))[[1]]
  expect_gt(length(questions), 8)
  for (call in questions) {
    q <- sub('.*"([^"]+)"$', "\\1", call)
    out <- tryCatch(rh_prereg_question(prl_prereg, q), error = function(e) conditionMessage(e))
    expect_true(startsWith(out, "["), info = q)
  }
})
