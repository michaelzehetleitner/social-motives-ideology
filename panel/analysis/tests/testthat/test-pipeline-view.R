# The three-look page of the committed pipeline is built from the code itself
# (scripts/generate_pipeline_view.R): every function the route calls has its
# Look 2, and every function those call has its Look 3.

local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) {
    parent <- dirname(dir)
    if (identical(parent, dir)) stop("project root not found from ", getwd())
    dir <- parent
  }
  assign("view_root", dir, envir = .GlobalEnv)
  view_env <- new.env()
  sys.source(file.path(dir, "scripts", "generate_pipeline_view.R"), envir = view_env)
  assign("view_env", view_env, envir = .GlobalEnv)
})

view <- view_env$build_pipeline_view(view_root, "abc1234", "https://example.org")

test_that("every section of _targets.R and every heading of the report is a box of Look 1", {
  targets_sections <- grep("^\\s+# ---- ", readLines(file.path(view_root, "_targets.R")), value = TRUE)
  lines <- vapply(view$target_sections, `[[`, "", "line")
  expect_true(all(targets_sections %in% lines))
  expect_true(all(c("# ---- REPORT CODE · Appendix ----", "# ---- REPORT CODE · Electronic Supplement ----") %in% lines))
  expect_equal(anyDuplicated(vapply(view$target_sections, `[[`, "", "id")), 0L)
  # the navigation follows the headings: a short name under its group
  groups <- unlist(lapply(view$navigation, `[[`, "group"))
  expect_identical(groups, c("DATA", "DESCRIPTION", "ANALYSES", "RESULTS", "REPORTING", "APPENDIX",
                             "SUPPLEMENT", "REPORT", "FILES", "REPORT CODE"))
})

test_that("every R/ function the route calls has a Look 2, and every function it calls a Look 3", {
  expect_true(all(c("restore_prepared_analysis_data", "assemble_supplement_measurement", "rh_cfa_loadings_by_model_table") %in%
                    names(view$panes)))
  expect_identical(names(view$pane_calls), names(view$panes))
  expect_identical(names(view$helper_calls), names(view$helpers))
  expect_true("tabulate_cfa_loadings_by_model" %in% names(view$helpers))
  pane <- view$panes$restore_prepared_analysis_data
  expect_match(pane$owner, "^R/ap3_pipeline.R, line [0-9]+$")
  expect_match(pane$code, "restore_prepared_analysis_data <- function(data, preparation, analysis_plan)", fixed = TRUE)
})

test_that("the page says what each look shows", {
  template <- paste(readLines(file.path(view_root, "review", "three-look-template.html"),
                              warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  html <- view_env$render_pipeline_view(view, template)
  expect_match(html, "<b>LOOK 2 · the function</b>", fixed = TRUE)
  expect_match(html, "<b>LOOK 3 · a function it calls</b>", fixed = TRUE)
  expect_match(html, "Saved source file · called function · commit abc1234", fixed = TRUE)
  expect_match(html, "Calls no other function of R/.", fixed = TRUE)
})

test_that("a call inside a Look 3 function opens the called function in Look 3", {
  one <- view
  one$helpers <- view$helpers["zm_require_config_fields_technical"]
  expect_match(view_env$render_helpers(one), 'data-contract=\\"zm_require_field\\"', fixed = TRUE)
})

# A committed copy of the analysis root, with the preregistration folder beside
# it, in a Git repository of its own.
make_view_checkout <- function() {
  repo <- tempfile("view-checkout-")
  root <- file.path(repo, "panel", "analysis")
  dir.create(file.path(root, "review"), recursive = TRUE)
  dir.create(file.path(repo, "panel", "preregistration"))
  writeLines("preregistration", file.path(repo, "panel", "preregistration", "preregistration.qmd"))
  file.copy(file.path(view_root, c("R", "report")), root, recursive = TRUE)
  file.copy(file.path(view_root, "_targets.R"), root)
  file.copy(file.path(view_root, "review", "three-look-template.html"), file.path(root, "review"))
  git <- function(...) system2("git", c("-C", shQuote(repo), "-c", "user.name=test", "-c", "user.email=test@example.invalid", ...),
                               stdout = FALSE, stderr = FALSE)
  git("init", "-q"); git("add", "-A"); git("commit", "-q", "-m", "checkout")
  root
}

test_that("the page links its commit only to a repository it is given", {
  without <- view_env$render_page_metadata(list(source_commit = "abc1234", source_url = ""))
  expect_match(without$page_badge, "^<span class=\"page-badge\">")
  expect_no_match(without$page_badge, "href", fixed = TRUE)
  with <- view_env$render_page_metadata(list(source_commit = "abc1234", source_url = "https://github.com/o/r/tree/abc/panel/analysis"))
  expect_match(with$page_badge, 'href="https://github.com/o/r/tree/abc/panel/analysis"', fixed = TRUE)
  # no repository address in the code: GitHub appears only as the <owner>/<name> pattern
  expect_false(any(grepl("github[.]com/[A-Za-z0-9]", readLines(file.path(view_root, "scripts", "generate_pipeline_view.R")))))
})

test_that("the Code Browser shows committed source, or says it is a preview", {
  skip_if(Sys.which("git") == "", "git not available")
  root <- make_view_checkout()
  clean <- view_env$describe_view_source(root, "https://github.com/owner/name", FALSE)
  expect_match(clean$commit, "^[0-9a-f]{7}$")
  expect_match(clean$commit_url, "^https://github.com/owner/name/tree/[0-9a-f]{40}/panel/analysis$")
  expect_identical(view_env$describe_view_source(root, "", FALSE)$commit_url, "")
  expect_error(view_env$describe_view_source(root, "https://example.org/owner/name", FALSE), "GitHub")
  writeLines("changed", file.path(root, "..", "preregistration", "preregistration.qmd"))
  expect_error(view_env$describe_view_source(root, "", FALSE), "committed source only")
  preview <- view_env$describe_view_source(root, "https://github.com/owner/name", TRUE)
  expect_match(preview$commit, "preparation preview", fixed = TRUE)
  expect_identical(preview$commit_url, "")
})

test_that("the Code Browser is written with the list of its sections for the publication links", {
  skip_if(Sys.which("git") == "", "git not available")
  root <- make_view_checkout()
  page <- view_env$generate_pipeline_view(root, file.path(tempfile("browser-"), "code-browser.html"))
  sections <- jsonlite::read_json(paste0(page, ".sections.json"))
  expect_identical(vapply(sections, `[[`, "", "id"), vapply(view$target_sections, `[[`, "", "id"))
  expect_true(all(nzchar(vapply(sections, `[[`, "", "title"))))
  html <- paste(readLines(page, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_match(html, "<title>Code Browser — Social Motives and Ideology", fixed = TRUE)
})
