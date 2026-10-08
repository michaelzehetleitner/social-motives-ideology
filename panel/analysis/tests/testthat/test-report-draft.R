# LAYER 3 tests — report/results_draft.qmd: the manuscript Results section and
# the electronic supplement that accompanies it, in one file with the boundary
# marked in the document. The draft is prose-led and reader-facing, so these
# tests check that it reads only what it shows, that it keeps the vocabulary of
# the implementation out of the text a reader sees, that both parts number
# their tables and figures, and that the synthetic-data paragraph is present.
# The technical report is tested in test-report.R.

local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) {
    parent <- dirname(dir)
    if (identical(parent, dir)) stop("project root not found from ", getwd())
    dir <- parent
  }
  for (f in c("config.R", "ap10_inference.R", "report_helpers.R", "report_bayesian_correlations.R", "report_joint_additions.R",
              "report_supplement_model_checks.R", "report_supplement_prior_sensitivity.R",
              "report_supplement_network_detail.R", "report_supplement_sampling_diagnostics.R", "report_results_network.R",
              "report_supplement_explained_variance.R",
              "report_supplement_measurement.R",
              "ap4_factor_structure.R", "ap9_efa.R", "ap9_pipeline.R")) {
    source(file.path(dir, "R", f), local = FALSE)
  }
  assign("draft_test_root", dir, envir = .GlobalEnv)
})

root <- draft_test_root
# The report labels every scale from the codebook (rh_labels(codebook_value)).
draft_labels <- rh_labels(zm_codebook(zm_config("smoke", file.path(root, "config", "analysis_plan.yaml"))))
draft_path <- file.path(root, "report", "results_draft.qmd")
draft_lines <- readLines(draft_path, warn = FALSE)
draft_text <- paste(draft_lines, collapse = "\n")

# The document is one file in three parts: the Results section of the
# manuscript, the printed appendix (top-level heading "Appendix") and the
# electronic supplement (top-level heading "Electronic Supplement"). Both
# later parts are unnumbered; their sections carry the labels A1 ... and S1 ...
appx_start <- grep("^# Appendix( [{][^}]*[}])?$", draft_lines)[1]
suppl_start <- grep("^# Electronic Supplement( [{][^}]*[}])?$", draft_lines)[1]
main_lines <- if (is.na(appx_start)) draft_lines else draft_lines[seq_len(appx_start - 1)]
appx_lines <- if (is.na(appx_start) || is.na(suppl_start)) character(0) else
  draft_lines[seq(appx_start, suppl_start - 1)]
suppl_lines <- if (is.na(suppl_start)) character(0) else draft_lines[seq(suppl_start, length(draft_lines))]
main_text <- paste(main_lines, collapse = "\n")
appx_text <- paste(appx_lines, collapse = "\n")
suppl_text <- paste(suppl_lines, collapse = "\n")

forbidden_words <- c("target", "profile", "smoke", "store")
# The package that runs the pipeline may be named: these phrases are the only
# places where "target" may appear in the prose. "restored" is the plain verb
# of the renv sentence, not the results store.
allowed_package_phrases <- c("the targets package", "can be restored")

# Markdown lines of the qmd outside the code chunks and the YAML front matter:
# the text a reader of the rendered document sees, captions of the supplement
# floats included.
draft_prose <- function(lines) {
  fm <- which(lines == "---")
  if (length(fm) >= 2) lines <- lines[-(fm[1]:fm[2])]
  in_chunk <- FALSE
  keep <- logical(length(lines))
  for (i in seq_along(lines)) {
    if (startsWith(lines[i], "```")) {
      in_chunk <- !in_chunk
      next
    }
    keep[i] <- !in_chunk
  }
  lines[keep]
}

# The R chunks as list(label, options, body, start, end), including the line span
# needed to place a chunk inside a cross-reference div.
draft_chunks <- function(lines) {
  starts <- grep("^```\\{r", lines)
  fences <- grep("^```", lines)
  out <- list()
  for (s in starts) {
    e <- fences[fences > s][1]
    block <- if (is.na(e)) character(0) else lines[seq(s + 1, length.out = max(0, e - s - 1))]
    opt_lines <- block[cumsum(!startsWith(block, "#|")) == 0]
    body <- block[seq_along(block) > length(opt_lines)]
    opts <- list()
    for (o in opt_lines) {
      kv <- sub("^#\\|\\s*", "", o)
      key <- sub(":.*$", "", kv)
      value <- trimws(sub("^[^:]*:\\s*", "", kv))
      opts[[key]] <- gsub('^"|"$', "", value)
    }
    label <- if (is.null(opts[["label"]])) NA_character_ else opts[["label"]]
    out[[length(out) + 1]] <- list(label = label, options = opts, body = body, start = s, end = e)
  }
  out
}

# The cross-reference divs of the supplement: `::: {#suppltbl-<id>}` … `:::`,
# the caption their last non-empty line.
suppl_divs <- function(lines) {
  open <- grep("^::: \\{#(suppltbl|supplfig|appxtbl|appxfig)-", lines)
  out <- list()
  for (o in open) {
    after <- which(trimws(lines) == ":::" & seq_along(lines) > o)
    close <- if (length(after) == 0) NA_integer_ else after[1]
    inner <- if (is.na(close)) character(0) else lines[seq(o + 1, length.out = max(0, close - o - 1))]
    nonempty <- inner[nzchar(trimws(inner))]
    out[[length(out) + 1]] <- list(
      id = sub("^::: \\{#([^} ]+)\\}.*$", "\\1", lines[o]),
      open = o, close = close, inner = inner,
      caption = if (length(nonempty) == 0) "" else trimws(nonempty[length(nonempty)])
    )
  }
  out
}

# Double-quoted string literals of a chunk body: the captions an `output: asis`
# loop writes are not chunk options, so they are checked here.
chunk_strings <- function(body) {
  text <- paste(body, collapse = "\n")
  unlist(regmatches(text, gregexpr('"[^"]*"', text)))
}

test_that("the results draft exists, is wired into the project and is a Results section", {
  expect_true(file.exists(draft_path))
  expect_match(draft_text, 'title: "Preregistration Analysis Pipeline"', fixed = TRUE)
  expect_match(draft_text, "Methods, Synthetic-Data Results and Reporting Checks", fixed = TRUE)
  targets_text <- paste(readLines(file.path(root, "_targets.R"), warn = FALSE), collapse = "\n")
  expect_match(targets_text, 'tar_quarto(results_draft, "report/results_draft.qmd"', fixed = TRUE)
})

test_that("the HTML template applies APA table and figure title layout centrally", {
  quarto <- paste(readLines(file.path(root, "report", "_quarto.yml"), warn = FALSE), collapse = "\n")
  css_path <- file.path(root, "report", "apa-captions.css")
  js_path <- file.path(root, "report", "apa-captions.js")
  include_path <- file.path(root, "report", "apa-captions.html")
  expect_match(quarto, "css: apa-captions.css", fixed = TRUE)
  expect_match(quarto, "include-after-body: apa-captions.html", fixed = TRUE)
  expect_true(all(file.exists(c(css_path, js_path, include_path))))

  css <- paste(readLines(css_path, warn = FALSE), collapse = "\n")
  js <- paste(readLines(js_path, warn = FALSE), collapse = "\n")
  include <- paste(readLines(include_path, warn = FALSE), collapse = "\n")
  expect_match(include, '<script src="apa-captions.js"></script>', fixed = TRUE)
  expect_match(css, ".apa-caption-number", fixed = TRUE)
  expect_match(css, "font-weight: 700", fixed = TRUE)
  expect_match(css, ".apa-caption-title", fixed = TRUE)
  expect_match(css, "font-style: italic", fixed = TRUE)
  expect_match(css, "text-align: left !important", fixed = TRUE)
  expect_match(css, "width: 100%", fixed = TRUE)
  expect_match(js, 'match[2].replace(/\\.\\s*$/, "")', fixed = TRUE)
  expect_match(js, "apaTitleCase", fixed = TRUE)
  expect_match(js, 'figure.insertBefore(caption, figure.firstElementChild)', fixed = TRUE)
  expect_match(js, 'Table|Figure', fixed = TRUE)
  expect_match(js, '[AS]?', fixed = TRUE)
})

test_that("the report separates methods, results and synthetic-data discussion", {
  headings <- sub(" [{]#.*$", "", sub("^#+\\s+", "", grep("^#\\s", draft_prose(main_lines), value = TRUE)), fixed = FALSE)
  # the reference list of the citations follows the Discussion
  expect_equal(headings, c("Introduction", "Methods", "Results", "Discussion", "References"))
  expect_match(draft_text, "number-sections: true", fixed = TRUE)
  result_start <- grep("^# Results", main_lines)
  discussion_start <- grep("^# Discussion", main_lines)
  result_lines <- main_lines[seq(result_start, discussion_start - 1L)]
  # the headings are compared without emphasis marks
  result_headings <- gsub("[*]", "", sub(" [{]#.*$", "", sub("^##\\s+", "", grep("^##\\s", draft_prose(result_lines), value = TRUE))))
  expect_equal(result_headings, c(
    "Sample", "Measurement", "Descriptive Statistics",
    "Bayesian Multiple Regressions (RQ1)", "Bayesian Joint Regression (RQ2)",
    "Bayesian Network Analysis (RQ3)", "Dimensionality and Item Correspondence (SRQ1)",
    "Deviations From the Preregistration"
  ))
  expect_false(is.na(appx_start))
  expect_false(is.na(suppl_start))
  expect_lt(appx_start, suppl_start)
  expect_false(any(grepl("^##\\s+S[0-9]", main_lines)))
})

test_that("the electronic supplement follows the Results section with its sections in order", {
  # exactly one further top-level heading, and it is the last one
  h1 <- sub(" [{][^}]*[}]$", "", sub("^#\\s+", "", grep("^#\\s", draft_prose(draft_lines), value = TRUE)))
  expect_equal(h1[length(h1)], "Electronic Supplement")
  expect_equal(sum(h1 == "Electronic Supplement"), 1L)
  s_headings <- gsub("[*]", "", sub(" [{]#.*$", "", sub("^##\\s+", "", grep("^##\\s", draft_prose(suppl_lines), value = TRUE))))
  # The supplement holds the statistical checks, ordered by research question
  # like the Results; the joint regression's
  # sampling diagnostics share the one diagnostics table of S3, so RQ2 has no
  # section of its own.
  expect_equal(
    s_headings,
    c(
      "S1 Sample", "S2 Measurement", "S3 Bayesian Multiple Regressions (RQ1)",
      "S4 Bayesian Network Analysis (RQ3)", "S5 Exploratory Factor Analyses (SRQ1)"
    )
  )
  a_headings <- gsub("[*]", "", sub(" [{]#.*$", "", sub("^##\\s+", "", grep("^##\\s", draft_prose(appx_lines), value = TRUE))))
  # The appendix holds the results about the constructs, in the same order;
  # the RQ2a tables stand in the Results.
  expect_equal(
    a_headings,
    c("A1 Measurement", "A2 Bayesian Multiple Regressions (RQ1)",
      "A3 Bayesian Network Analysis (RQ3)", "A4 Exploratory Factor Analyses (SRQ1)")
  )
  # every section opens with its question: the research question where the
  # section belongs to one, else the question its displays answer
  sections <- split(suppl_lines, cumsum(grepl("^## ", suppl_lines)))[-1]
  sections <- c(split(appx_lines, cumsum(grepl("^## ", appx_lines)))[-1], sections)
  for (lines in sections) {
    opening <- paste(utils::head(lines[nzchar(trimws(lines))], 2)[2], collapse = " ")
    expected <- if (grepl("[(]S?RQ[0-9]", lines[1])) "^__Research question[.]__ " else "^__Question[.]__ "
    expect_match(opening, expected, info = lines[1])
  }
})

test_that("the appendix opens by stating the boundary to the manuscript part and to the supplement", {
  prose <- draft_prose(appx_lines)
  filled <- which(nzchar(trimws(prose)))
  first <- filled[filled > 1L][1]
  blank <- which(!nzchar(trimws(prose)))
  last <- min(blank[blank > first]) - 1L
  para <- paste(prose[seq(first, last)], collapse = " ")
  expect_match(para, "The appendix follows the order of the Results", fixed = TRUE)
  expect_match(para, "are in the electronic supplement", fixed = TRUE)
  expect_match(para, "files that accompany the analysis code hold the complete numbers", fixed = TRUE)
})

test_that("the report keeps computed supplement summaries at their relevant location", {
  for (name in c("s1_answer_text", "prior_width_text", "power_scaling_text", "predictive_check_text$prior",
                 "predictive_check_text$posterior", "heavier_tail_text", "s5_answer_text", "s6_answer_text")) {
    expect_match(main_text, paste0("`r ", name, "`"), fixed = TRUE, info = name)
  }
  expect_match(main_text, "`r diagnostics_answer_text`", fixed = TRUE)
  expect_match(main_text, "@appxtbl-r2", fixed = TRUE)
})

test_that("measurement distributions are reported factually and manual interpretation remains explicit", {
  # the pins are checked on the prose without emphasis marks
  prose <- gsub("[*]", "", gsub("\\s+", " ", paste(draft_prose(main_lines), collapse = " ")))
  expect_false(grepl("depart from normality", prose, fixed = TRUE))
  expect_false(grepl("which is why they were fitted", prose, fixed = TRUE))
  expect_match(prose, "## Dimensionality and Item Correspondence (SRQ1) {#sec-dimensionality}", fixed = TRUE)
  # the question is read from the preregistration at render time (one source)
  expect_match(prose, 'rh_prereg_question(prereg, "SRQ1")', fixed = TRUE)
  # social neophilia: one clause in the CFA section, the limitation in the Discussion
  expect_match(prose, "is the only scale without subscales", fixed = TRUE)
  expect_match(prose, "no separate psychometric validation was conducted", fixed = TRUE)
  expect_match(prose, "which does not validate it", fixed = TRUE)
})

test_that("reports the exploratory solutions compactly and names the files with the complete numbers", {
  expect_match(gsub("[*]", "", suppl_text), "## S5 Exploratory Factor Analyses (SRQ1)", fixed = TRUE)
  expect_match(suppl_text, "Reliability and dimensionality answer different questions", fixed = TRUE)
  expect_match(appx_text, "rh_factor_number_criteria_table(s1_value$factor_number_criteria", fixed = TRUE)
  expect_match(appx_text, "rh_efa_correspondence_overview_table(s1_value$correspondence$overview", fixed = TRUE)
  for (key in c("efa_loadings", "efa_membership", "cfa_loadings", "prior_power_scaling")) {
    expect_match(suppl_text, paste0("analysis_plan$data_files$result_files$", key), fixed = TRUE, info = key)
  }
  plan <- yaml::read_yaml(file.path(root, "config", "analysis_plan.yaml"))
  set_keys <- vapply(plan$factor_analysis$sets, function(set) as.character(set$key), character(1))
  expect_equal(set_keys, c("asc", "dopl", "ums", "motives", "outcomes"))
})

test_that("the introduction states the goal, the research questions and the synthetic-data paragraph", {
  # the pins are checked on the prose without emphasis marks
  prose <- gsub("[*]", "", gsub("\\s+", " ", paste(draft_prose(main_lines), collapse = " ")))
  expect_match(prose, "verifies that the preregistered analysis pipeline works", fixed = TRUE)
  # the research questions at top level, without sub-letters
  for (rq in c("- RQ1 —", "- RQ2 —", "- RQ3 —", "- SRQ1 —")) expect_match(prose, rq, fixed = TRUE, info = rq)
  expect_false(grepl("- RQ2a", prose, fixed = TRUE))
  # what this report is and what the manuscript keeps
  expect_match(prose, "This report presents results from synthetic preregistration data. It will be the basis for the report in the final manuscript.", fixed = TRUE)
  expect_match(prose, "the present Methods and Discussion are shaped to the synthetic-data approach; the manuscript will not contain them.", fixed = TRUE)
  # the data source is read from its target
  expect_match(draft_text, "data_source <- read_report_input(data_source_used", fixed = TRUE)
  expect_match(prose, "The synthetic preregistration data are artificial survey responses", fixed = TRUE)
  expect_match(prose, "Problems were planted in the data to test the preregistered rules for handling such cases:", fixed = TRUE)
  expect_match(prose, "respondents for every exclusion criterion except an age above `r analysis_plan$exclusions$max_age`", fixed = TRUE)
  expect_match(prose, "missing answers, a respondent without a gender entry, an item that also loads on a second scale, and a facet with heavy-tailed residuals.", fixed = TRUE)
  expect_false(grepl("../synthetic-report.html", draft_text, fixed = TRUE))
})

test_that("the results draft carries no implementation vocabulary in its prose, its captions or its supplement", {
  prose <- gsub("\\s+", " ", paste(draft_prose(draft_lines), collapse = "\n"))
  # Each allowed phrase has to occur, so that an exemption cannot outlive the
  # text it was made for; the words are then checked in what remains.
  for (phrase in allowed_package_phrases) {
    expect_true(grepl(phrase, prose, fixed = TRUE), info = paste("allowed phrase missing:", phrase))
    prose <- gsub(phrase, "", prose, fixed = TRUE)
  }
  for (word in forbidden_words) {
    expect_false(
      grepl(word, prose, ignore.case = TRUE),
      info = paste("forbidden word in the prose:", word)
    )
  }
  # the same words must not reach the reader through a chunk caption either
  caps <- unlist(lapply(draft_chunks(draft_lines), function(ch) {
    unlist(ch$options[c("tbl-cap", "fig-cap")])
  }))
  for (word in forbidden_words) {
    expect_false(any(grepl(word, caps, ignore.case = TRUE)), info = paste("forbidden word in a caption:", word))
  }
  # nor through a caption an `output: asis` loop of the supplement writes
  loops <- Filter(function(ch) identical(ch$options[["output"]], "asis"), draft_chunks(suppl_lines))
  expect_gte(length(loops), 1)
  loop_strings <- unlist(lapply(loops, function(ch) chunk_strings(ch$body)))
  for (word in forbidden_words) {
    expect_false(
      any(grepl(word, loop_strings, ignore.case = TRUE)),
      info = paste("forbidden word in a looped caption:", word)
    )
  }
})

test_that("the appendix and supplement displays appear only in their own part", {
  # The appendix holds the results about the constructs, the supplement the
  # statistical checks.
  appendix_only <- c(
    "rh_cfa_loadings_by_model_table", "rh_cfa_factor_correlation_matrix_table", "rh_r2_table",
    "rh_network_stability_strips", "rh_network_side_by_side",
    "rh_factor_number_criteria_table", "rh_efa_correspondence_overview_table", "rh_efa_scale_clarity_table",
    "rh_efa_membership_table", "rh_efa_items_off_scale_table"
  )
  supplement_only <- c(
    "rh_filled_cells_table", "rh_item_distribution_display", "rh_reliability_table", "rh_cfa_fit_table",
    "rh_cfa_largest_residuals_table",
    "rh_sensitivity_table", "rh_priorsense_table", "rh_prior_posterior_plot",
    "rh_prior_pred_table", "rh_pp_stats_table", "rh_pp_plot", "rh_robustness_table",
    "rh_diagnostics_table",
    "rh_network_pip_scatter", "rh_network_changed_edges_table",
    "rh_efa_set_variance_table"
  )
  for (helper in c(appendix_only, supplement_only)) {
    expect_true(exists(helper, mode = "function"), info = helper)
    expect_false(grepl(paste0(helper, "("), main_text, fixed = TRUE),
                 info = paste("appendix or supplement display used in the manuscript part:", helper))
  }
  for (helper in appendix_only) {
    expect_true(grepl(paste0(helper, "("), appx_text, fixed = TRUE), info = paste("appendix display never used:", helper))
    expect_false(grepl(paste0(helper, "("), suppl_text, fixed = TRUE), info = paste("appendix display in the supplement:", helper))
  }
  for (helper in supplement_only) {
    expect_true(grepl(paste0(helper, "("), suppl_text, fixed = TRUE), info = paste("supplement display never used:", helper))
    expect_false(grepl(paste0(helper, "("), appx_text, fixed = TRUE), info = paste("supplement display in the appendix:", helper))
  }
})

test_that("the supplement retains explained variance", {
  expect_match(suppl_text, "rh_efa_set_variance_table(s1_value$efa_variance", fixed = TRUE)
})

test_that("the results draft loads only saved inputs that it displays", {
  calls <- regmatches(draft_text, gregexpr("read_report_input\\(\\s*[A-Za-z0-9_]+", draft_text))[[1]]
  declared <- unique(sub("^read_report_input\\(\\s*", "", calls))
  expect_gt(length(declared), 0)
  # every read goes through the resolved location, so both execution directories work
  reads <- regmatches(draft_text, gregexpr("read_report_input\\([^)]*\\)", draft_text))[[1]]
  expect_true(all(grepl(", report_inputs", reads, fixed = TRUE)))
  # every target of the pipeline map is available to the draft
  manifest <- withr::with_dir(
    root,
    withr::with_envvar(
      c(ZM_PROFILE = "smoke", ZM_DATA = "synthetic"),
      withCallingHandlers(
        targets::tar_manifest(callr_function = NULL, envir = new.env(parent = globalenv())),
        warning = function(w) {
          if (grepl("built under R version|unter R Version", conditionMessage(w))) {
            invokeRestart("muffleWarning")
          }
        }
      )
    )
  )
  expect_true(
    all(declared %in% manifest$name),
    info = paste("read but not in the map:", paste(setdiff(declared, manifest$name), collapse = ", "))
  )
  # nothing is loaded that the document never uses: the value each read is
  # assigned to has to appear again outside its own assignment — in a display,
  # in the prose, or in a quantity derived from it for the prose
  chunks <- draft_chunks(draft_lines)
  load_chunk <- Filter(function(ch) identical(ch$label, "load-results"), chunks)[[1]]
  assign_lines <- grep("^[A-Za-z0-9_.]+ <- read_report_input\\(", load_chunk$body, value = TRUE)
  assigned <- sub(" <- read_report_input\\(.*$", "", assign_lines)
  expect_gte(length(assigned), 15)
  rest <- paste(setdiff(draft_lines, assign_lines), collapse = "\n")
  for (v in assigned) {
    expect_true(
      grepl(paste0("\\b", v, "\\b"), rest),
      info = paste("loaded but never used:", v)
    )
  }
  # `codebook` is read inline for the labels, and `labels` is used throughout
  expect_match(draft_text, "labels <- rh_labels(codebook_value)", fixed = TRUE)
})

test_that("every table and figure of the manuscript part is numbered and captioned", {
  chunks <- draft_chunks(main_lines)
  # The AP3 processing-status loader reads metadata for the manuscript.
  emitting <- Filter(function(ch) !ch$label %in% c("load-results", "load-imputation-processing"), chunks)
  expect_gte(length(emitting), 7)
  for (ch in emitting) {
    expect_true(grepl("^(tbl|fig)-", ch$label), info = paste("label:", ch$label))
    kind <- sub("-.*$", "", ch$label)
    cap <- ch$options[[paste0(kind, "-cap")]]
    expect_true(!is.null(cap) && nzchar(cap), info = paste("caption missing:", ch$label))
  }
  # every table chunk emits a table with an APA-like note: either it calls
  # rh_table() with a note of its own, or it calls a named rh_*_table() builder
  # that composes the note itself. Associations, RQ3,
  # exploratory and deviations tables use the second form.
  for (ch in Filter(function(ch) startsWith(ch$label, "tbl-"), emitting)) {
    body <- paste(ch$body, collapse = "\n")
    builder <- regmatches(body, regexpr("(?:rh_|build_)[a-z0-9_]*table\\(", body, perl = TRUE))
    expect_gt(length(builder), 0)
    if (identical(builder, "rh_table(")) {
      expect_match(body, "source_note = ", fixed = TRUE)
    } else if (!identical(builder, "rh_deviations_table(")) {
      fun <- get(sub("\\($", "", builder), mode = "function")
      if (grepl("build_joint_strength_comparison_table", paste(deparse(fun), collapse = "\n"), fixed = TRUE)) {
        fun <- build_joint_strength_comparison_table
      }
      expect_match(paste(deparse(fun), collapse = "\n"), "source_note", fixed = TRUE,
                   info = builder)
    }
  }
})

# Whether the text before a float refers to it: literally, or through an inline
# sentence the load chunk computes (`r x`) whose definition names the float.
introduces_float <- function(before, id) {
  if (grepl(paste0("@", id), before, fixed = TRUE)) return(TRUE)
  inline <- regmatches(before, gregexpr("`r [A-Za-z_][A-Za-z0-9_.]*", before))[[1]]
  used <- unique(sub("^`r ", "", inline))
  if (length(used) == 0L) return(FALSE)
  chunk <- Filter(function(ch) identical(ch$label, "load-results"), draft_chunks(draft_lines))[[1]]
  any(vapply(as.list(parse(text = chunk$body)), function(x) {
    is.call(x) && identical(x[[1]], as.name("<-")) && is.name(x[[2]]) && as.character(x[[2]]) %in% used &&
      grepl(paste0("@", id), paste(deparse(x), collapse = "\n"), fixed = TRUE)
  }, logical(1)))
}

test_that("every table and figure of the appendix and the supplement is numbered in its own series and captioned", {
  for (part in list(list(lines = appx_lines, tbl = "appxtbl", fig = "appxfig", min = 6L),
                    list(lines = suppl_lines, tbl = "suppltbl", fig = "supplfig", min = 12L))) {
    divs <- suppl_divs(part$lines)
    expect_gte(length(divs), part$min)
    for (d in divs) {
      expect_match(d$id, paste0("^(", part$tbl, "|", part$fig, ")-[a-z0-9-]+$"))
      expect_false(is.na(d$close), info = paste("unclosed div:", d$id))
      expect_true(nzchar(d$caption), info = paste("caption missing:", d$id))
      expect_match(d$caption, "\\.$")
      # the caption is the last paragraph of the div, not a fence or an option
      expect_false(startsWith(d$caption, "```"), info = d$id)
      expect_false(startsWith(d$caption, "#|"), info = d$id)
      # one sentence before the float names the question it answers and
      # refers to it
      section_start <- max(grep("^#{2,3} ", part$lines[seq_len(d$open)]))
      before <- paste(part$lines[seq(section_start, d$open)], collapse = "\n")
      expect_true(introduces_float(before, d$id), info = paste("float not introduced:", d$id))
    }
    # both series are present
    kinds <- sub("-.*$", "", vapply(divs, function(d) d$id, character(1)))
    expect_true(all(c(part$tbl, part$fig) %in% kinds))
    # every chunk sits inside such a div, or writes its own numbered float
    # (rh_float_show(), which prints nothing when the display has no rows)
    for (ch in draft_chunks(part$lines)) {
      inside <- any(vapply(
        divs,
        function(d) !is.na(d$close) && ch$start > d$open && ch$end < d$close,
        logical(1)
      ))
      loop <- identical(ch$options[["output"]], "asis")
      expect_true(inside || loop, info = paste("float not numbered:", ch$label))
      if (loop) {
        body <- paste(ch$body, collapse = "\n")
        expect_match(body, paste0('prefix = "', part$tbl, '"'), fixed = TRUE, info = ch$label)
        expect_match(body, "caption = ", fixed = TRUE, info = ch$label)
        expect_match(body, 'id = (paste0[(]|")', info = ch$label)
      }
    }
  }
  # the four series are declared in the front matter, so they number independently
  for (key in c("appxtbl", "appxfig", "suppltbl", "supplfig")) {
    expect_match(draft_text, paste("key:", key), fixed = TRUE, info = key)
  }
  expect_match(draft_text, 'reference-prefix: "Table S"', fixed = TRUE)
  expect_match(draft_text, 'reference-prefix: "Figure S"', fixed = TRUE)
  expect_match(draft_text, 'reference-prefix: "Table A"', fixed = TRUE)
  expect_match(draft_text, 'reference-prefix: "Figure A"', fixed = TRUE)
})

test_that("no float identifier of the results draft is used twice and every reference resolves", {
  chunk_labels <- vapply(draft_chunks(draft_lines), function(ch) ch$label, character(1))
  div_ids <- vapply(suppl_divs(c(appx_lines, suppl_lines)), function(d) d$id, character(1))
  # the floats rh_float_show() writes, and the membership floats of the
  # appendix loop, one per group of scales
  shown <- regmatches(draft_text, gregexpr('id = "[a-z0-9-]+", prefix = "[a-z]+"', draft_text))[[1]]
  div_ids <- c(div_ids, sub('id = "([a-z0-9-]+)", prefix = "([a-z]+)"', "\\2-\\1", shown),
               paste0("appxtbl-efa-membership-", c("motives", "orientation")))
  heading_ids <- sub(".*[{]#([^}]+)[}].*", "\\1", grep("^#+ .*[{]#sec-", draft_lines, value = TRUE))
  ids <- c(chunk_labels, div_ids, heading_ids)
  expect_equal(anyDuplicated(ids), 0)
  prose <- paste(draft_prose(draft_lines), collapse = "\n")
  refs <- unique(sub(
    "^@", "",
    regmatches(prose, gregexpr("@(suppltbl|supplfig|appxtbl|appxfig|tbl|fig|sec)-[a-z0-9-]+", prose))[[1]]
  ))
  expect_gt(length(refs), 0)
  expect_true(any(startsWith(refs, "suppltbl-")))
  expect_true(
    all(refs %in% ids),
    info = paste("dangling reference:", paste(setdiff(refs, ids), collapse = ", "))
  )
  # Quarto numbers no section of the appendix or the supplement, so no
  # cross-reference may point at one; those sections are reached by sec_link()
  unnumbered <- sub(".*[{]#(sec-[a-z0-9-]+)[ }].*", "\\1",
                    grep("^#+ .*[{]#sec-[a-z0-9-]+ [.]unnumbered[}]", draft_lines, value = TRUE))
  expect_gt(length(unnumbered), 0)
  expect_length(intersect(refs, unnumbered), 0)
  # the manuscript part points at the supplement, in words and by number
  expect_match(main_text, "electronic supplement", fixed = TRUE)
  expect_true(any(startsWith(
    unique(sub("^@", "", regmatches(main_text, gregexpr("@suppltbl-[a-z0-9-]+", main_text))[[1]])),
    "suppltbl-"
  )))
})

test_that("appendix and supplement sections carry consecutive labels and every link to them resolves", {
  # Sections of the appendix and the supplement are numbered A1, A2 ... and
  # S1, S2 ..., with subsections S2.1 ...; Quarto does
  # not number them, so the labels are typed and checked here.
  labels_of <- function(lines, level) {
    hits <- grep(paste0("^", strrep("#", level), " "), draft_prose(lines), value = TRUE)
    sub("^#+ ([AS][0-9]+([.][0-9]+)?) .*$", "\\1", hits)
  }
  for (part in list(list(lines = appx_lines, letter = "A"), list(lines = suppl_lines, letter = "S"))) {
    headings <- grep("^#{2,4} ", draft_prose(part$lines), value = TRUE)
    # every heading below the part's own is unnumbered
    expect_true(all(grepl("[.]unnumbered", headings)), info = part$letter)
    top <- labels_of(part$lines, 2L)
    expect_equal(top, paste0(part$letter, seq_along(top)))
    # subsections run from 1 under each section
    sub_labels <- labels_of(part$lines, 3L)
    if (length(sub_labels) > 0L) {
      parent <- sub("[.][0-9]+$", "", sub_labels)
      for (sec in unique(parent)) {
        expect_equal(sub_labels[parent == sec], paste0(sec, ".", seq_len(sum(parent == sec))), info = sec)
      }
    }
  }
  # the links the prose builds point at labelled sections
  heading_label <- regmatches(draft_lines, regexec("^#{2,3} ([AS][0-9]+([.][0-9]+)?) .*[{]#(sec-[a-z0-9-]+)", draft_lines))
  labelled_ids <- vapply(Filter(length, heading_label), `[[`, "", 4L)
  linked <- unique(sub('.*sec_link[(]"([a-z0-9-]+)"[)].*', "\\1",
                       unlist(regmatches(draft_text, gregexpr('sec_link[(]"[a-z0-9-]+"[)]', draft_text)))))
  expect_gt(length(linked), 0)
  expect_true(all(linked %in% labelled_ids), info = paste("unlabelled:", paste(setdiff(linked, labelled_ids), collapse = ", ")))
})

test_that("the load chunk sources every file the prose lookups need", {
  # The document brings its own functions in: what the test file sources is not
  # what the render has. The chunk's own source() calls are evaluated in an
  # environment that cannot see the globals of this test file, and the lookup
  # the prose performs has to work there.
  chunk <- Filter(function(ch) identical(ch$label, "load-results"), draft_chunks(draft_lines))[[1]]
  source_lines <- grep("^source\\(file\\.path\\(analysis_root", chunk$body, value = TRUE)
  expect_gt(length(source_lines), 5)
  files <- sub('.*"R", "([^"]+)".*', "\\1", source_lines)
  # parent = as.environment(2) is the first attached package: packages stay
  # reachable, the global environment of this test file does not.
  env <- new.env(parent = as.environment(2L))
  for (f in files) sys.source(file.path(root, "R", f), envir = env)
  # the Methods sentence on the gender coding names the reference group
  expect_true(exists("rh_gender_coding_text", envir = env, inherits = FALSE))
  gender_sentence <- eval(quote(rh_gender_coding_text(factor(c("female", "male", "female")))), envir = env)
  expect_match(gender_sentence, "the reference group is female, the largest group", fixed = TRUE)

  # Same failure mode for the resolution of the network Bayes factors: without
  # ap8_network_helpers.R sourced, rh_network_bf()/rh_network_bf_bound_note() fall
  # into their tryCatch's degraded branch instead of computing what the run
  # can resolve.
  expect_true(exists("rh_network_bf", envir = env, inherits = FALSE))
  bf <- c(Inf, 2, 0)
  n_sweeps <- 750L
  g_prior <- eval(quote(zm_config()$network$g_prior), envir = env)
  call_env <- list2env(list(bf = bf, n_sweeps = n_sweeps, g_prior = g_prior), parent = env)
  bf_out <- eval(quote(rh_network_bf(bf, n_sweeps, g_prior)), envir = call_env)
  bound_note <- eval(quote(rh_network_bf_bound_note(bf, n_sweeps, g_prior)), envir = call_env)

  # An environment without ap8_network_helpers.R sourced reproduces the degraded
  # branch: the tryCatch around the missing ap6_network_bf_bounds() call
  # fails, so no bound is printed and the note names the missing sweep count.
  degraded_env <- new.env(parent = as.environment(2L))
  degraded_files <- setdiff(files, "ap8_network_helpers.R")
  for (f in degraded_files) sys.source(file.path(root, "R", f), envir = degraded_env)
  degraded_call_env <- list2env(list(bf = bf, n_sweeps = n_sweeps, g_prior = g_prior), parent = degraded_env)
  bf_degraded <- eval(quote(rh_network_bf(bf, n_sweeps, g_prior)), envir = degraded_call_env)
  bound_note_degraded <- eval(quote(rh_network_bf_bound_note(bf, n_sweeps, g_prior)), envir = degraded_call_env)

  # the report's display rule: 749 is the largest Bayes factor 750 steps
  # resolve, so an infinite one shows as "> 100"; the smallest, 1/749, as "< 0.01"
  expect_identical(bf_out, c("> 100", "2.00", "< 0.01"))
  expect_false(any(grepl("^> ", bf_degraded)))
  expect_false(any(grepl("^< ", bf_degraded)))
  expect_match(bound_note, "750 sampled networks can resolve", fixed = TRUE)
  expect_match(bound_note_degraded, "is not recorded for this table, so an inclusion probability", fixed = TRUE)
})

test_that("all results-draft R chunks parse", {
  extracted <- tempfile(fileext = ".R")
  on.exit(unlink(extracted), add = TRUE)
  knitr::purl(draft_path, output = extracted, documentation = 0, quiet = TRUE)
  expect_silent(parse(extracted))
})

test_that("reported empirical results use computed numbers", {
  results_start <- grep("^# Results", draft_lines)
  discussion_start <- grep("^# Discussion", draft_lines)
  text <- paste(draft_prose(draft_lines[seq(results_start, discussion_start - 1L)]), collapse = "\n")
  text <- gsub("`r [^`]*`", "", text)
  # sentences copied from the preregistration carry its fixed thresholds
  text <- gsub("\\[[^]]*\\]\\{\\.prereg\\}", "", text, perl = TRUE)
  text <- gsub("\\$[^$]*\\$", "", text)
  text <- gsub("(?m)^\\s*[0-9]+\\.\\s", "", text, perl = TRUE)
  text <- gsub("@(suppltbl|supplfig|tbl|fig)-[a-z0-9-]+", "", text)
  text <- gsub("(?m)^#+\\s+S[0-9]+\\s", "", text, perl = TRUE)
  typed <- regmatches(text, gregexpr("(?<![A-Za-z0-9_^])[0-9]+(?:[.,][0-9]+)?", text, perl = TRUE))[[1]]
  expect_true(length(typed) == 0, info = paste("typed numbers:", paste(typed, collapse = ", ")))
})

# Exercise the actual prose-producing functions with contrary results, without
# reading fitted models or rendering a report from the persistent cache.
draft_prose_function <- function(name, envir) {
  chunk <- Filter(function(ch) identical(ch$label, "load-results"), draft_chunks(draft_lines))[[1]]
  expressions <- parse(text = chunk$body)
  selected <- Filter(function(x) {
    is.call(x) && identical(x[[1]], as.name("<-")) && identical(x[[2]], as.name(name))
  }, as.list(expressions))
  stopifnot(length(selected) == 1L)
  eval(selected[[1]], envir = envir)
  get(name, envir = envir)
}

test_that("the network and the joint model state the sample the fill leaves them", {
  prose <- paste(draft_prose(draft_lines), collapse = "\n")
  expect_match(draft_text, "descriptive statistics and the network use the analysis sample", fixed = TRUE)
  # the joint model's method is RQ2's Statistical Method, copied from the preregistration
  expect_match(draft_text, 'rh_prereg_under_heading(prereg, "RQ2", "Statistical Method")', fixed = TRUE)
  expect_match(draft_text, "every regression of RQ1 and RQ2 uses *N* = ", fixed = TRUE)
  expect_match(draft_text, "rh_fmt_n(joint_n)", fixed = TRUE)
  expect_match(draft_text, "joint_n <- report_sizes_value$n_regressions", fixed = TRUE)
})

test_that("the report states the social-neophilia scale in one clause of the confirmatory factor analyses", {
  expect_match(gsub("\\s+", " ", main_text), "social neophilia, developed for this study, is the only scale without subscales", fixed = TRUE)
})

test_that("the confirmatory fit table and its prose describe every model in one table", {
  # One table of three blocks holds every confirmatory model, a statistical
  # check of the supplement.
  hit <- Filter(function(d) identical(d$id, "suppltbl-cfa-fit"), suppl_divs(suppl_lines))
  expect_equal(length(hit), 1L)
  expect_equal(hit[[1]]$caption, "Fit of every confirmatory model.")
  prose <- gsub("\\s+", " ", paste(draft_prose(suppl_lines), collapse = " "))
  expect_match(prose, "@suppltbl-cfa-fit answers how well each confirmatory model describes the responses to its items",
               fixed = TRUE)
  expect_match(prose, "the subscale structures of the development papers", fixed = TRUE)
  # the columns with one value in every row are stated in the prose beside it
  expect_match(prose, "`r cfa_fit_constant_text`", fixed = TRUE)
  expect_match(draft_text, "cfa_fit_constant_text <- rh_cfa_fit_constant_text(", fixed = TRUE)
  # the counts of the prose come from the S1 target
  expect_match(draft_text, "n_count_indices <- s1_value$counts$count_indices", fixed = TRUE)
  # the subscale structures of the development papers in Measurement, the two
  # models of the combined item sets with SRQ1
  main_prose <- paste(draft_prose(main_lines), collapse = "\n")
  expect_match(main_prose, 'cfa_summary_text(cfa_structure_models, "models of the subscale structure")', fixed = TRUE)
  expect_match(main_prose, 'cfa_summary_text(cfa_combined_models, "confirmatory models of these item sets")', fixed = TRUE)
})

test_that("supplement answers report failed or unavailable inputs instead of silently passing them", {
  base_env <- function() {
    env <- new.env(parent = globalenv())
    env$labels <- draft_labels
    env$join_and <- function(x) {
      if (length(x) <= 1L) return(paste(x, collapse = ""))
      paste0(paste(x[-length(x)], collapse = ", "), " and ", x[length(x)])
    }
    env$ci_pct <- "95%"
    # the link to an unnumbered supplement section (sec_link() reads the headings)
    env$sec_link <- function(id) paste0("[Section S0](#", id, ")")
    env
  }

  env <- base_env()
  env$s1_value <- list(answer = list(
    reliability_complete = FALSE, reliability_missing = "s2",
    cfa_complete = FALSE, cfa_missing = "s2"
  ))
  s1 <- draft_prose_function("s1_answer_text", env)
  expect_match(s1, "s2", fixed = TRUE)
  expect_match(s1, "unavailable, nonconvergent, or inadmissible", fixed = TRUE)
  env$s1_value$answer$reliability_complete <- TRUE
  env$s1_value$answer$cfa_complete <- TRUE
  expect_identical(draft_prose_function("s1_answer_text", env), "")

  env <- base_env()
  env$s3_value <- list(answer = list(
    failed_models = "asc_agg",
    failed_blocks = tibble::tibble(outcome = character(), block = character()),
    unreliable_blocks = tibble::tibble(), unavailable_blocks = tibble::tibble(), n_unreliable = 0L, n_unreliable_blocks = 0L, n_unavailable_blocks = 0L,
    any_successful = FALSE, n_flagged = 0L
  ))
  s3 <- draft_prose_function("power_scaling_text", env)
  expect_match(s3, "Power-scaling failed", fixed = TRUE)
  expect_match(s3, "every prior block of the ASC aggression model", fixed = TRUE)
  expect_match(s3, "No reliable power-scaling diagnosis was available", fixed = TRUE)
  env$s3_value$answer <- list(failed_models = character(),
                              failed_blocks = tibble::tibble(outcome = character(), block = character()),
                              unreliable_blocks = tibble::tibble(), unavailable_blocks = tibble::tibble(), n_unreliable = 0L, n_unreliable_blocks = 0L, n_unavailable_blocks = 0L,
                              any_successful = TRUE, n_flagged = 0L)
  expect_match(draft_prose_function("power_scaling_text", env),
               "slightly more or less influential (power scaling) flagged no coefficient", fixed = TRUE)

  env <- base_env()
  env$s4_value <- list(answer = check_model_check_outcomes(
    prior_predictive = tibble::tibble(
      outcome = "asc_agg", share_below_min = NA_real_, share_above_max = NA_real_
    ),
    posterior_predictive = tibble::tibble(observed = NA_real_, rep_lo = NA_real_, rep_hi = NA_real_),
    tails = tibble::tibble(outcome = "asc_agg", heavy_tails = NA),
    student_coefs = NULL
  ))
  s4 <- paste(c(unlist(draft_prose_function("predictive_check_text", env)), draft_prose_function("heavier_tail_text", env)), collapse = " ")
  expect_match(s4, "The prior predictive check of the observed response scales was unavailable", fixed = TRUE)
  expect_match(s4, "The posterior predictive comparisons were unavailable", fixed = TRUE)
  expect_match(s4, "heavier-tail diagnostic was unavailable", fixed = TRUE)

  # A placeholder row of an untriggered refit is not a refit: the S4 answer
  # reads the recorded fit availability, as the tail table does.
  env <- base_env()
  env$s4_value <- list(answer = check_model_check_outcomes(
    prior_predictive = tibble::tibble(
      outcome = c("asc_agg", "asc_sub"), share_below_min = 0, share_above_max = 0
    ),
    posterior_predictive = tibble::tibble(
      outcome = c("asc_agg", "asc_sub"), stat = "kurtosis",
      observed = c(1, 1), rep_lo = c(0, 0), rep_hi = c(2, 2)
    ),
    tails = tibble::tibble(outcome = c("asc_agg", "asc_sub"), heavy_tails = c(TRUE, TRUE)),
    student_coefs = tibble::tibble(
      outcome = c("asc_agg", "asc_sub"), term = c("zm_power", NA_character_),
      fit_available = c(TRUE, FALSE), note = c(NA_character_, "not triggered")
    )
  ))
  s4_placeholder <- draft_prose_function("heavier_tail_text", env)
  expect_match(s4_placeholder, "criterion was met without a completed Student-t refit for ", fixed = TRUE)
  expect_match(s4_placeholder, rh_label_inline("asc_sub", draft_labels), fixed = TRUE)
  # the same rule for the count the S4 sentence quotes
  s4_source <- paste(readLines(file.path(root, "R", "report_supplement_model_checks.R"), warn = FALSE),
                     collapse = "\n")
  expect_match(s4_source, "n_student_outcomes = length(rh_fitted_outcomes(student_coefs))", fixed = TRUE)

  env <- base_env()
  transitions <- matrix(1L, nrow = 1L, ncol = 1L,
                        dimnames = list("inconclusive", "inconclusive"))
  env$s5_value <- list(answer = check_network_detail(list(
    transitions = transitions, n_edges = 1L, n_changed = 0L,
    edges = tibble::tibble(
      node_i = "zm_power", node_j = "zm_prestige", decision_bagged = "present",
      share_above_include = NA_real_, share_below_exclude = NA_real_
    )
  )))
  # the agreement sentence, shared with the supplement's network section, comes first
  draft_prose_function("network_agreement_text", env)
  s5 <- draft_prose_function("s5_answer_text", env)
  expect_match(s5, "Resample stability was unavailable", fixed = TRUE)
  expect_match(s5, "Resample stability was unavailable for power – prestige", fixed = TRUE)

  env <- base_env()
  env$analysis_plan <- list(priors = list(sweep_labels = list("0.2" = "primary")))
  diagnostics <- tibble::tibble(
    outcome = "asc_agg", slope_sd = .2, family = "gaussian",
    ess_ok = TRUE, rhat_ok = TRUE, divergences_ok = TRUE,
    treedepth_ok = TRUE, bfmi_ok = FALSE,
    ok = FALSE, gate_status = "not_interpretable"
  )
  env$diagnostics_value <- c(list(diagnostics = diagnostics), check_sampling_diagnostics(diagnostics))
  diagnostics <- draft_prose_function("diagnostics_answer_text", env)
  expect_match(diagnostics, "did not pass", fixed = TRUE)
  expect_match(diagnostics, "Failed diagnostics by fit: ASC aggression (", fixed = TRUE)
  expect_match(diagnostics, "BFMI", fixed = TRUE)
  expect_match(diagnostics, "Every diagnostic not named for a fit passed there", fixed = TRUE)

  env <- base_env()
  env$s6_value <- check_explained_variance(
    tibble::tibble(outcome = "asc_agg", role = "primary", r2_median = .2), "asc_agg"
  )
  s6 <- draft_prose_function("s6_answer_text", env)
  expect_match(s6, "explained the least variance", fixed = TRUE)
  expect_false(grepl("Monte Carlo standard errors of zero", s6, fixed = TRUE))


})

test_that("failed primary fits are unavailable rather than negative findings", {
  env <- new.env(parent = globalenv())
  # the facts come from report_credible_cells_by_outcome(): per facet, whether
  # its model is interpretable, and its credible cells
  env$report_credible_value <- tibble::tibble(
    outcome_key = c("asc_agg", "asc_sub"), interpretable = c(FALSE, TRUE),
    predictor = list(character(), character()), observed_sign = list(character(), character())
  )
  env$labels <- draft_labels
  draft_prose_function("join_and", env)
  text <- draft_prose_function("credible_text", env)
  expect_match(text("asc_agg"), "could not be interpreted", fixed = TRUE)
  expect_false(grepl("not credibly associated", text("asc_agg"), fixed = TRUE))
  expect_match(text("asc_sub"), "not credibly associated", fixed = TRUE)
  expect_match(text("sdo_dom"), "could not be interpreted", fixed = TRUE)
  # rh_prediction_cells_table() owns the "Not interpretable" cell, while the
  # main prose retains the withholding sentence.
  expect_match(paste(deparse(rh_prediction_cells_table), collapse = "\n"),
               'Not interpretable because sampling diagnostics did not pass', fixed = TRUE)
  expect_match(main_text, "rh_prediction_cells_table(", fixed = TRUE)
  expect_match(main_text, "Classification was withheld", fixed = TRUE)
})

test_that("the prior-sweep narrative changes when credibility changes", {
  # Presentation consumes AP10's decisions; it does not reclassify endpoints.
  text <- function(n_changes, interpretable = TRUE) {
    env <- new.env(parent = globalenv())
    env$sweep <- list(n_cells = 2L, n_interval_changes = n_changes)
    env$sweep_interpretable <- interpretable
    draft_prose_function("sweep_credibility_text", env)
  }
  expect_match(text(0L), "no cell", fixed = TRUE)
  expect_match(text(1L), "1 of the 2 cells changed", fixed = TRUE)
  expect_match(text(1L, interpretable = FALSE), "cannot be interpreted", fixed = TRUE)
  expect_false(grepl("cells changed", text(1L, interpretable = FALSE), fixed = TRUE))
  expect_match(suppl_text, "`r sweep_credibility_text`", fixed = TRUE)
})

test_that("partial prior comparisons do not describe unassessed cells as robust", {
  env <- new.env(parent = globalenv())
  # counts from report_count_prediction_verdicts(): three cells, two comparable, one changed
  env$n_cells <- 3L
  env$n_prior_comparable <- 2L
  env$n_prior_dependent <- 1L
  env$n_prior_widths <- 3L
  env$report_association_counts_value <- list(n_prior_unchanged = 1L)
  text <- draft_prose_function("prior_robust_text", env)
  expect_match(text, "assessed for two of the three cells", fixed = TRUE)
  expect_match(text, "one retained", fixed = TRUE)
  expect_match(text, "one changed", fixed = TRUE)
  expect_match(text, "remaining cells could not be compared", fixed = TRUE)
  expect_false(grepl("Every classification|every classification|remaining classifications are the same", text))
})

test_that("the prior-sweep prose names the widths for credibility and direction changes", {
  env <- new.env(parent = globalenv())
  env$analysis_plan <- zm_config("smoke",
    file.path(draft_test_root, "config", "analysis_plan.yaml"))
  env$labels <- draft_labels
  draft_prose_function("join_and", env)
  draft_prose_function("width_phrase", env)
  draft_prose_function("cell_phrase", env)
  # the changed cells as report_cells_changing_with_prior_width() returns them
  changes <- tibble::tibble(
    outcome_key = "asc_agg", predictor = "zm_power",
    interval_exclusion_stable = FALSE, median_direction_stable = FALSE,
    widths_excluding_zero = list(c("0.20", "0.40")), widths_including_zero = list("0.10"),
    widths_positive_median = list(c("0.20", "0.40")), widths_negative_median = list("0.10")
  )
  env$sweep_detail_interpretable <- TRUE
  env$credibility_changes <- changes
  env$direction_changes <- changes
  credibility <- draft_prose_function("sweep_credibility_detail_text", env)
  # the cell named inside the sentence: both labels in their inline form
  expect_match(credibility, "For power → ASC aggression, the interval", fixed = TRUE)
  expect_match(credibility, "excluded zero under the primary and permissive priors", fixed = TRUE)
  expect_match(credibility, "included zero under the skeptical prior", fixed = TRUE)
  direction <- draft_prose_function("sweep_direction_detail_text", env)
  expect_match(direction, "positive under the primary and permissive priors", fixed = TRUE)
  expect_match(direction, "negative under the skeptical prior", fixed = TRUE)
  # the report names the three priors as AP6 does: skeptical, primary, permissive
  expect_match(draft_text, "sweep_credibility_detail_text", fixed = TRUE)
  expect_match(draft_text, "sweep_direction_detail_text", fixed = TRUE)
})

test_that("measurement prose handles unavailable and inadmissible results", {
  env <- new.env(parent = globalenv())
  expect_false(grepl("acceptable internal consistency", main_text, fixed = TRUE))
  expect_false(grepl("fitted the data well", main_text, fixed = TRUE))
  env$s1_value <- list(cfa_sets = tibble::tibble(
    model = c("failed", "inadmissible"),
    converged = c(FALSE, TRUE), admissible = c(FALSE, FALSE)
  ))
  fit_text <- draft_prose_function("cfa_fit_text", env)
  expect_match(fit_text("failed"), "did not converge", fixed = TRUE)
  expect_match(fit_text("inadmissible"), "not admissible", fixed = TRUE)
  expect_match(fit_text("missing"), "unavailable", fixed = TRUE)
})

test_that("model sample sizes come from each model instead of the overall sample", {
  env <- new.env(parent = globalenv())
  # report_analysis_sample_sizes(): one regression sample, one network sample
  env$report_sizes_value <- list(n_regressions = 697L, n_network = 699L)
  n <- draft_prose_function("model_n", env)
  expect_equal(n(c("asc_sub", "asc_agg", "network")), c(697L, 697L, 699L))
  env$report_sizes_value$n_regressions_by_outcome <- c(asc_agg = 697L, asc_sub = 698L)
  expect_equal(n(c("asc_sub", "asc_agg", "network")), c(698L, 697L, 699L))
  expect_match(draft_text, "report_sizes_value <- read_report_input(report_sample_sizes", fixed = TRUE)
  # The prose states the sizes once.
  expect_false(grepl("model_n = model_n", main_text, fixed = TRUE))
  expect_match(main_text, "sample_n_text", fixed = TRUE)
})

test_that("participation, sample accounting and original item distributions are displayed", {
  expect_match(main_text, "`r participation_text`", fixed = TRUE)
  expect_match(draft_text, 'rh_prereg_passage(prereg, "Participation rate is unknown")', fixed = TRUE)
  for (count in c("n_started", "n_quota_full", "n_admitted", "n_excluded", "n_analysis")) {
    expect_match(main_text, paste0("`r rh_fmt_n(", count, ")`"), fixed = TRUE)
  }
  expect_match(main_text, "`r exclusion_text`", fixed = TRUE)
  expect_match(draft_text, "rh_criterion_label(report_flow_value$steps$criterion, analysis_plan)", fixed = TRUE)
  expect_match(draft_text, "s1_value <- read_report_input(supplement_measurement", fixed = TRUE)
  expect_match(suppl_text, "s1_value$item_distributions", fixed = TRUE)
  expect_match(suppl_text, "Response options selected by nobody are retained\nwith a count of zero", fixed = TRUE)
  expect_match(suppl_text, "rh_item_distribution_display(s1_value$item_distributions", fixed = TRUE)
})

test_that("network prior sensitivity is attributed to the unbagged refits", {
  prose <- gsub("\\s+", " ", suppl_text)
  expect_match(prose, "across the unbagged full-data refits", fixed = TRUE)
  expect_match(prose, "Stability of the bagged classifications across edge priors is not evaluated", fixed = TRUE)
  expect_false(grepl("robust to the edge prior", prose, fixed = TRUE))
})

test_that("sweep prose requires a valid fit for every planned outcome and prior width", {
  env <- new.env(parent = globalenv())
  env$analysis_plan <- list(regression = list(outcomes = c("asc_agg", "asc_sub"), family = "gaussian"),
                  priors = list(slope_sd_sweep = c(.1, .2, .4)))
  grid <- expand.grid(outcome = env$analysis_plan$regression$outcomes, slope_sd = env$analysis_plan$priors$slope_sd_sweep,
                      stringsAsFactors = FALSE)
  valid <- lapply(seq_len(nrow(grid)), function(i) list(
    outcome_key = grid$outcome[i], slope_sd = grid$slope_sd[i], role = "sweep", family = "gaussian",
    fit_valid = TRUE, gate_status = "ok"))
  passed <- function(register) check_prior_width_fits(register, env$analysis_plan)
  expect_true(passed(valid))
  failed <- valid
  failed[[1]]$fit_valid <- FALSE
  failed[[1]]$gate_status <- "not_interpretable"
  expect_false(passed(failed))
  expect_false(passed(valid[-1]))
  expect_match(draft_text, "provided no interpretable sensitivity estimates", fixed = TRUE)
})

# --- the item-set membership floats of the supplement ----------------------

test_that("the appendix prints the item membership of the expected solutions, one table per group of scales", {
  plan <- zm_config("smoke", file.path(root, "config", "analysis_plan.yaml"))
  chunk <- Filter(function(ch) identical(ch$label, "appx-efa-membership"), draft_chunks(draft_lines))[[1]]
  multi <- Filter(function(set) isTRUE(set$efa) && length(unlist(set$scales)) > 1L, plan$factor_analysis$sets)
  membership <- lapply(multi, function(set) {
    scales <- unlist(set$scales)
    tibble::tibble(
      item = paste0(scales, "_1"), intended_scale = scales, assigned_factor = paste0("F", seq_along(scales)),
      assigned_scales = scales, largest_abs_loading = 0.6, next_largest_abs_loading = 0.1,
      weak = FALSE, crossloading = FALSE, crossloading_scales = NA_character_, crossloading_factors = NA_character_
    )
  })
  names(membership) <- vapply(multi, function(set) paste0(set$key, ":", set$expected_factors), "")
  motives <- as.character(plan$regression$motives)
  orientation <- as.character(plan$regression$outcomes)
  env <- new.env(parent = globalenv())
  env$membership_groups <- list(
    motives = Filter(function(set) all(unlist(set$scales) %in% motives), multi),
    orientation = Filter(function(set) all(unlist(set$scales) %in% orientation), multi)
  )
  env$motive_scale_keys <- motives
  env$orientation_scale_keys <- orientation
  env$loading_cutoff <- plan$factor_analysis$loading_display_cutoff
  env$labels <- draft_labels
  env$codebook_value <- list(scales = list(item_codes = list()))
  env$s1_value <- list(correspondence = list(membership = membership))
  out <- paste(capture.output(eval(parse(text = paste(chunk$body, collapse = "\n")), envir = env)),
               collapse = "\n")
  for (group in c("motives", "orientation")) {
    expect_match(out, paste0("::: {#appxtbl-efa-membership-", group, "}"), fixed = TRUE, info = group)
  }
  # every set of several scales has its column pair
  for (set in multi) {
    expect_match(out, sub(" \\(.*\\)$", "", set$label), fixed = TRUE, info = set$key)
  }
  expect_match(out, "factors: largest loading", fixed = TRUE)
  # the alternative solutions have no float of their own; their items that
  # leave their scale's factor are one table after the loop
  expect_false(grepl("suggested by the criteria", out, fixed = TRUE))
  expect_match(appx_text, 'id = "efa-items-off-scale", prefix = "appxtbl"', fixed = TRUE)
})


test_that("CFA summaries use adverse-direction indices only for admissible converged models", {
  env <- new.env(parent = globalenv())
  summarise <- draft_prose_function("cfa_summary_text", env)
  rows <- tibble::tibble(converged = c(TRUE, TRUE, FALSE), admissible = c(TRUE, TRUE, FALSE),
                        cfi = c(.99, .94, .01), tli = c(.98, .92, .01),
                        rmsea = c(.03, .08, .99), srmr = c(.02, .07, .99))
  attr(rows, "report_summary") <- summarise_cfa_reporting_fits(rows)
  text <- summarise(rows, "one-factor models")
  expect_match(text, "Two of the three", fixed = TRUE)
  expect_match(text, "minimum CFI = .940", fixed = TRUE)
  # TLI can exceed one, so it keeps its leading zero (APA)
  expect_match(text, "minimum TLI = 0.920", fixed = TRUE)
  expect_match(text, "maximum RMSEA = .080", fixed = TRUE)
  expect_match(text, "maximum SRMR = .070", fixed = TRUE)
  rows$converged <- FALSE
  attr(rows, "report_summary") <- summarise_cfa_reporting_fits(rows)
  expect_false(grepl("minimum CFI", summarise(rows, "models"), fixed = TRUE))
})

test_that("diagnostic prose reports the observed extrema without invented pass claims", {
  env <- new.env(parent = globalenv())
  summarise <- draft_prose_function("diagnostic_extrema_text", env)
  rows <- tibble::tibble(rhat_max = c(1.001, 1.021), ess_bulk_min = c(850, 450),
                        ess_tail_min = c(900, 500), bfmi_min = c(.7, .2),
                        n_divergent = c(0, 2), n_treedepth_hits = c(1, 0))
  attr(rows, "report_extrema") <- summarise_sampling_diagnostic_extrema(rows)
  text <- summarise(rows)
  for (term in c("maximum R̂ was 1.021", "minimum bulk ESS was 450", "minimum tail ESS was 500",
                 "minimum BFMI was 0.20", "divergent transitions in one fit was 2", "maximum tree depth 1")) {
    expect_match(text, term, fixed = TRUE)
  }
  expect_equal(summarise(rows[FALSE, ]), "")
})


test_that("the Bayesian correlation report uses posterior summaries consistently", {
  expect_match(draft_text, "bayesian_correlations_value <- read_report_input(report_bayesian_correlations", fixed = TRUE)
  expect_false(grepl("read_report_input(scale_score_correlations", draft_text, fixed = TRUE))
  expect_match(main_text, "plot_bayesian_correlation_heatmap(bayesian_correlations_value", fixed = TRUE)
  expect_match(suppl_text, "build_bayesian_correlation_details(bayesian_correlations_value", fixed = TRUE)
  expect_match(suppl_text, "@suppltbl-bayesian-correlations", fixed = TRUE)
  expect_match(main_text, "Credible intervals and Bayes factors for every pair are reported with the\nprecision of the sampling (for details see Supplement).", fixed = TRUE)
  expect_false(grepl("zero-order r =", draft_text, fixed = TRUE))
})

test_that("Bayesian correlation prose separates estimates, evidence and Monte Carlo precision", {
  env <- new.env(parent = globalenv())
  summarise <- draft_prose_function("correlation_results_text", env)
  # the Monte Carlo precision is stated in the note of the correlation table
  sampling <- describe_correlation_sampling
  prior <- draft_prose_function("correlation_prior_text", env)
  value <- list(pairs = data.frame(
    median = c(.24, -.02, .06, NA_real_), evidence = c("present", "absent", "inconclusive", NA),
    lower = c(.10, -.08, -.02, NA_real_), upper = c(.38, .04, .14, NA_real_),
    status = c("ok", "ok", "ok", "unavailable"),
    posterior_ess = c(800, 700, 900, NA_real_), median_mcse = c(.001, .003, .002, NA_real_)
  ))
  summary <- summarise(build_correlation_reporting_data(value))
  expect_match(summary, "ranging from ρ = −.02 to ρ = .24", fixed = TRUE)
  # Only an interval excluding zero determines the reported direction;
  # a positive or negative median alone does not.
  expect_match(summary, "Of the three correlations, 1 were credibly positive and 0 credibly negative", fixed = TRUE)
  expect_match(summary, "evidence for a correlation for one pair", fixed = TRUE)
  expect_match(summary, "evidence for no correlation for one pair", fixed = TRUE)
  expect_match(summary, "for one pair the evidence was inconclusive", fixed = TRUE)
  expect_match(summary, "One correlation was unavailable", fixed = TRUE)
  value$pairs$upper[2] <- -.001
  expect_match(summarise(build_correlation_reporting_data(value)), "1 were credibly positive and 1 credibly negative", fixed = TRUE)
  diagnostics <- sampling(value)
  expect_match(diagnostics, "smallest effective sample size of the posterior draws was 700", fixed = TRUE)
  expect_match(diagnostics, "0.0030", fixed = TRUE)
  value$pairs$status <- "unavailable"
  expect_match(summarise(build_correlation_reporting_data(value)), "were unavailable", fixed = TRUE)
  expect_match(sampling(value), "were unavailable", fixed = TRUE)
  text <- prior(list(rscale = 1/3, prior_beta = c(3,3), prior_support = c(-1,1)))
  expect_match(text, "Beta(3, 3)", fixed = TRUE)
  expect_match(text, "one third (the standard medium setting)", fixed = TRUE)
})

test_that("the joint additions follow the seven-pair and standardised RQ2 specifications", {
  expect_match(draft_text, 'joint_correlation_changes_value <- read_report_input(joint_correlation_changes', fixed = TRUE)
  expect_match(draft_text, 'joint_asc_aggregation_value <- read_report_input(joint_asc_aggregation', fixed = TRUE)
  expect_match(draft_text, 'report_joint_additions.R', fixed = TRUE)
  # RQ2c of the preregistration covers the ASC total
  expect_match(gsub("[*]", "", draft_text), '### ASC Total Versus Its Facets (RQ2c) {#sec-asc-aggregation}', fixed = TRUE)
  # RQ2a: compact strength comparisons; the sign flips stand in the text
  expect_match(main_text, "@tbl-rq2a-comparisons", fixed = TRUE)
  expect_match(main_text, "#| label: tbl-rq2a-comparisons", fixed = TRUE)
  # RQ2b: the comparator is the zero-order correlation from the joint model
  expect_match(paste(deparse(build_joint_correlation_changes_table), collapse = "\n"),
               "Zero-order (from the joint model)", fixed = TRUE)
  expect_match(draft_text, '@tbl-asc-aggregate', fixed = TRUE)
  expect_match(draft_text, '@tbl-asc-attenuation', fixed = TRUE)
  expect_match(draft_text, 'build_joint_correlation_changes_table(joint_correlation_changes_value', fixed = TRUE)
})

# --- the computed answers of RQ2a, RQ3b and RQ3c ------------------------------

test_that("the RQ2a sentences name the motive and both facets, strength and signs apart, without a subtraction", {
  env <- new.env(parent = globalenv())
  env$labels <- draft_labels
  env$outcome_keys <- c("asc_agg", "asc_sub", "asc_conv", "sdo_dom")
  for (name in c("join_and", "rq2_facet_key", "rq2_facet", "rq2a_pair_text", "rq2a_comparison_text")) {
    draft_prose_function(name, env)
  }
  row <- function(motive, first, second, classification, direction) tibble::tibble(
    motive_key = motive, contrast = paste0("|", first, "| - |", second, "|"), first_outcome = first,
    second_outcome = second, posterior_median = 0, lower = 0, upper = 0,
    classification = classification, direction_comparison = direction)
  env$rq2_value <- list(
    gate_passed = TRUE,
    differences = dplyr::bind_rows(
      row("zm_achievement", "aggression", "sdo_d", "credibly stronger for the first-named outcome", "unresolved direction comparison"),
      row("zm_power", "aggression", "sdo_d", "unresolved strength difference", "same direction"),
      row("zm_prestige", "conventionalism", "sdo_d", "unresolved strength difference", "sign flip"),
      row("zm_power", "conventionalism", "sdo_d", "credibly weaker for the first-named outcome", "unresolved direction comparison")
    ),
    credible_directions = tibble::tibble(
      motive_key = c("zm_power", "zm_power", "zm_prestige", "zm_prestige", "zm_achievement"),
      outcome = c("aggression", "sdo_d", "conventionalism", "sdo_d", "aggression"),
      sign = c("positive", "positive", "negative", "positive", "negative"))
  )
  text <- env$rq2a_comparison_text("components")
  expect_match(text, "Between ASC aggression and SDO-D, the association was credibly stronger with ASC aggression for achievement, and the difference in strength was unresolved for power; power was credibly positively associated with both.", fixed = TRUE)
  expect_match(text, "Between conventionalism and SDO-D, the association was credibly stronger with SDO-D for power, and the difference in strength was unresolved for prestige; no motive had credible coefficients of the same or of opposite signs for both.", fixed = TRUE)
  # The result-summary sign-flip statement is gated by a credible strength difference.
  flip <- env$rq2_value$differences$motive_key == "zm_prestige"
  env$rq2_value$differences$lower[flip] <- 0.1
  env$rq2_value$differences$upper[flip] <- 0.2
  env$rq2_value$differences$classification[flip] <- "credibly stronger for the first-named outcome"
  expect_match(env$rq2a_comparison_text("components"),
               "prestige was credibly negatively associated with conventionalism but positively with SDO-D, a sign flip", fixed = TRUE)
  expect_false(grepl("minus", text, fixed = TRUE))
  # an invalid joint fit classifies nothing, and the sentence says so
  env$rq2_value$gate_passed <- FALSE
  expect_match(env$rq2a_comparison_text("components"), "did not pass its validity gate", fixed = TRUE)
})

test_that("the RQ3b answer gives the sign of each present edge against the direction RQ3b asks about", {
  base <- function(weight) {
    env <- new.env(parent = globalenv())
    env$labels <- draft_labels
    draft_prose_function("join_and", env)
    draft_prose_function("rq3b_asked_sign", env)
    env$network_decisions_value <- list(feasible = TRUE)
    # the node sets as report_network gives them, in the codebook's order
    env$autonomy_nodes <- c("zm_power", "zm_prestige", "zm_achievement")
    env$security_nodes <- c("zm_security", "zm_arousal")
    env$rq3b_edges <- tibble::tibble(
      node_i = c("zm_security", "zm_arousal"), node_j = c("zm_achievement", "zm_power"),
      decision = c(if (is.na(weight)) "inconclusive" else "present", "inconclusive"),
      weight_bagged = c(if (is.na(weight)) 0.1 else weight, 0.02))
    draft_prose_function("rq3b_sign_text", env)
  }
  positive <- base(0.25)
  expect_match(positive, "The present edge between achievement and security was positive (edge weight .25), the opposite of the negative association with security that RQ3b asks about.", fixed = TRUE)
  expect_match(positive, "No edge of power, prestige and achievement with arousal was present.", fixed = TRUE)
  expect_match(base(-0.25), "was negative (edge weight −.25), the direction RQ3b asks about.", fixed = TRUE)
  expect_identical(base(NA), "None of these edges was present, so no sign answers RQ3b in this run.")
})

test_that("the RQ3c answer names only ASC–SDO-D pairs whose edge differs from their RQ2b residual correlation", {
  env <- new.env(parent = globalenv())
  env$labels <- draft_labels
  for (name in c("join_and", "pair_key", "block_pairs")) draft_prose_function(name, env)
  env$network_decisions_value <- list(feasible = TRUE)
  env$rq2_value <- list(gate_passed = TRUE, residuals = tibble::tibble(
    pair = c("aggression_submission", "submission_sdo_d", "conventionalism_sdo_d"),
    posterior_median = c(0.29, 0.11, 0.01), lower = c(0.21, 0.03, -0.06), upper = c(0.35, 0.18, 0.08)))
  env$rq3c_edges <- tibble::tibble(node_i = c("asc_sub", "asc_sub", "asc_conv"),
                                   node_j = c("asc_agg", "sdo_dom", "sdo_dom"),
                                   decision = c("absent", "absent", "absent"))
  env$network_value <- list(residual_comparison = calculate_network_residual_comparison(
    env$rq3c_edges, env$rq2_value$residuals))
  text <- draft_prose_function("rq3c_rq2b_text", env)
  # the edge named at the start of the sentence
  expect_match(text, paste(rh_capitalise_first_letter(rh_network_edge_label("asc_sub", "sdo_dom", env$labels, inline = TRUE)),
                           "has a residual correlation whose interval lies above zero in RQ2b but an absent edge here"),
               fixed = TRUE)
  expect_false(grepl(rh_label("asc_conv", env$labels), text, fixed = TRUE))
  expect_false(grepl(rh_label("asc_agg", env$labels), text, fixed = TRUE))
  expect_match(text, "hold different variables constant", fixed = TRUE)
})

test_that("the only italics are N and p and the Results hold the three network tables", {
  # the only italics outside code are the statistical symbols N and p; the
  # reference list sets its titles in italics as APA asks
  prose <- gsub("\\s+", " ", paste(draft_prose(draft_lines), collapse = " "))
  plain <- sub("# References .*?(?=# Appendix)", "", gsub("`[^`]*`", "", prose), perl = TRUE)
  italics <- regmatches(plain, gregexpr("(?<![*\\w])[*](?![*\\s])[^*]+?[*](?![*\\w])", plain, perl = TRUE))[[1]]
  expect_setequal(unique(italics), c("*N*", "*p*"))
  # the three network tables present the bridge comparisons
  for (id in c("tbl-network-motive-facet", "tbl-network-motives", "tbl-network-facets")) {
    expect_match(main_text, paste0("#| label: ", id), fixed = TRUE, info = id)
  }
})
