# The preregistration reader (R/report_preregistration.R): what counts as text
# of the preregistration, how a passage is located, and when the render stops.

local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) dir <- dirname(dir)
  source(file.path(dir, "R", "report_preregistration.R"), local = FALSE)
})

fixture <- c(
  "---", "title: \"Fixture\"", "---", "",
  "# Research Questions", "", "## Summary", "",
  "**RQ1:** Which motives matter, conditional on the others? Four regressions answer this.", "",
  "- **RQ1: Which motives matter, conditional on the others?**", "",
  "## RQ1 — Motives {#rq1}", "", "### Psychological Question", "",
  "RQ1: Which motives matter, conditional on the others?", "",
  "# Analysis Plan", "", "## AP10 — Inference Criteria", "",
  "For the regression models, a predictor counts as credible if its interval under the prior (normal(0, 0.20); AP6) excludes zero. An interval that includes zero is not evidence of absence.",
  "", "~~The sweep never overturns a classification.~~ The sweep is descriptive.", "",
  "<!-- an agent note -->", "",
  "::: {.callout-important title=\"TODO — question\"}", "Is this rule final?", ":::", "",
  "*A proposed sentence that is not accepted.*", "",
  "## AP7 — Joint Regression", "",
  "A positive difference indicates a stronger association with the first-named outcome.", "",
  "## AP7b — Conditioning", "",
  "A positive difference indicates a more positive correlation after conditioning.", "",
  "See [AP5](#ap5) and the [highlighted rule]{.highlight-audit} here."
)
path <- withr::local_tempfile(fileext = ".qmd")
writeLines(fixture, path)
prereg <- rh_read_preregistration(path)

test_that("struck text, comments and callouts are not text of the preregistration", {
  expect_false(any(grepl("never overturns", prereg$text, fixed = TRUE)))
  expect_false(any(grepl("agent note", prereg$text, fixed = TRUE)))
  expect_false(any(grepl("Is this rule final", prereg$text, fixed = TRUE)))
  expect_true(any(prereg$text == "The sweep is descriptive."))
  expect_true(any(prereg$text == "See AP5 and the highlighted rule here."))
})

test_that("a passage is located by its first words and ends with its sentence", {
  out <- rh_prereg_passage(prereg, "For the regression models, a predictor", span = FALSE)
  expect_equal(as.character(out), "For the regression models, a predictor counts as credible if its interval under the prior (normal(0, 0.20); AP6) excludes zero.")
  expect_equal(attr(out, "section"), "AP10")
  two <- rh_prereg_passage(prereg, "For the regression models, a predictor", "is not evidence of absence.", span = FALSE)
  expect_true(endsWith(as.character(two), "is not evidence of absence."))
  expect_equal(as.character(rh_prereg_passage(prereg, "The sweep is descriptive")), "[The sweep is descriptive.]{.prereg}")
  expect_equal(rh_prereg_section(prereg, "The sweep is descriptive"), "AP10")
})

test_that("a question that stands twice with the same wording is one passage; a lowercased key keeps its case", {
  q <- rh_prereg_passage(prereg, "Which motives matter", span = FALSE)
  expect_equal(as.character(q), "Which motives matter, conditional on the others?")
  frag <- rh_prereg_passage(prereg, "a predictor counts as credible", "excludes zero", span = FALSE)
  expect_equal(as.character(frag), "a predictor counts as credible if its interval under the prior (normal(0, 0.20); AP6) excludes zero")
  lower <- rh_prereg_passage(prereg, "the sweep is descriptive", span = FALSE)
  expect_equal(as.character(lower), "the sweep is descriptive.")
})

test_that("the render stops on a missing key, a proposal, or differing wordings", {
  expect_error(rh_prereg_passage(prereg, "No such sentence"), "no passage starting")
  expect_error(rh_prereg_passage(prereg, "A proposed sentence that is"), "italic proposal")
  expect_error(rh_prereg_passage(prereg, "A positive difference indicates a"), "differently worded")
  expect_equal(as.character(rh_prereg_passage(prereg, "A positive difference indicates a", "the first-named outcome.", span = FALSE)),
               "A positive difference indicates a stronger association with the first-named outcome.")
})

test_that("a whole section is copied by its heading, and a question by its label", {
  sec <- c("---", "title: x", "---", "", "## RQ2 — Joint {#rq2}", "", "### Psychological Questions", "",
           "RQ2a: Which facet differs?", "", "RQ2b: Which pairs remain?", "",
           "### What Results Answer the Questions?", "",
           "A difference is credible when its interval [central 95%] excludes zero.", "",
           "~~An old sentence.~~ The second paragraph.", "",
           "*A proposed paragraph.*", "",
           "| a | b |", "|---|---|", "| 1 | 2 |", "", ": A table caption {#tbl-x}", "",
           "### Theoretical Predictions", "", "Not part of the copy.", "",
           "## SRQ1 — Items", "", "### Psychological Question", "", "How distinct are the items?")
  path <- withr::local_tempfile(fileext = ".qmd"); writeLines(sec, path)
  pr <- rh_read_preregistration(path)
  out <- rh_prereg_under_heading(pr, "RQ2", "What Results Answer the Questions?")
  expect_equal(out, paste0("[A difference is credible when its interval \\[central 95%\\] excludes zero.]{.prereg}\n\n",
                           "[The second paragraph.]{.prereg} (RQ2)"))
  expect_error(rh_prereg_under_heading(pr, "RQ2", "No Such Heading"), "no heading")
  expect_equal(rh_prereg_question(pr, "RQ2b"), "[Which pairs remain?]{.prereg}")
  expect_equal(rh_prereg_question(pr, "SRQ1"), "[How distinct are the items?]{.prereg}")
})
