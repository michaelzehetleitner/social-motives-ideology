# Generate the Code Browser: the page of the saved pipeline in three looks.
#
# Builds the page from the code as it stands, so it cannot drift from it:
# - Look 1 is `_targets.R` followed by the R chunks of the report, one box per
#   section comment of `_targets.R` and per heading of the report.
# - Look 2 is every function of `R/` that Look 1 calls: its roxygen card and
#   its body, as committed.
# - Look 3 is every function of `R/` those call in turn, at any depth.
# The page is the template review/three-look-template.html, filled by
# render_pipeline_view().
#
# Run from panel/analysis after committing:
#   Rscript scripts/generate_pipeline_view.R
# Writes report/code-browser.html (git-ignored, like every page there) and,
# next to it, code-browser.html.sections.json: the id and title of every Look 1
# section, which scripts/link_publication_sections.py of the publication reads.
# Source links point to the repository named by PUBLICATION_REPOSITORY_URL;
# without it the page names the commit without a link.

#' The functions defined in the R/ files, with their card and body
#'
#' @param files Paths of the R files.
#' @return Named list, one entry per top-level function: `file`, `line`,
#'   `title` (first roxygen line), `code` (roxygen and definition) and `calls`
#'   (every symbol the body names).
index_pipeline_functions <- function(files) {
  out <- list()
  for (file in files) {
    lines <- readLines(file, warn = FALSE, encoding = "UTF-8")
    expressions <- parse(file, keep.source = TRUE, encoding = "UTF-8")
    references <- attr(expressions, "srcref")
    for (i in seq_along(expressions)) {
      x <- expressions[[i]]
      if (!(is.call(x) && as.character(x[[1]]) %in% c("<-", "=") && is.name(x[[2]]) &&
            is.call(x[[3]]) && identical(x[[3]][[1]], as.name("function")))) next
      from <- references[[i]][1]
      to <- references[[i]][3]
      start <- from
      while (start > 1 && grepl("^#'", lines[start - 1])) start <- start - 1
      roxygen <- if (start < from) lines[start:(from - 1)] else character(0)
      name <- as.character(x[[2]])
      out[[name]] <- list(
        file = file.path("R", basename(file)), line = from,
        title = if (length(roxygen)) sub("^#'\\s*", "", roxygen[1]) else "",
        code = paste(lines[start:to], collapse = "\n"),
        calls = unique(all.names(x[[3]]))
      )
    }
  }
  out
}

#' Look 1: `_targets.R`, then the R chunks of the report under its headings
#'
#' A main heading of the report, or a section heading of its supplement,
#' becomes a section comment in the form `_targets.R` uses, so both parts are
#' navigated alike; subsection headings stay out of the navigation. The heading
#' keeps its words: Quarto's attribute block and emphasis marks are dropped.
#'
#' @param targets_path,report_path Paths of `_targets.R` and the report source.
#' @return Character vector of lines.
read_pipeline_route <- function(targets_path, report_path) {
  route <- readLines(targets_path, warn = FALSE, encoding = "UTF-8")
  report <- readLines(report_path, warn = FALSE, encoding = "UTF-8")
  out <- c(route, "", "# ---- REPORT CODE · Setup: results_draft.qmd reads the targets ----")
  in_chunk <- FALSE
  for (line in report) {
    if (grepl("^```\\{r", line)) {
      in_chunk <- TRUE
      next
    }
    if (in_chunk && grepl("^```\\s*$", line)) {
      in_chunk <- FALSE
      out <- c(out, "")
      next
    }
    if (in_chunk) {
      out <- c(out, line)
    } else if (grepl("^# |^## (S[0-9]|Software)", line)) {
      heading <- gsub("*", "", sub("\\s*\\{[^}]*\\}\\s*$", "", sub("^#+\\s+", "", line)), fixed = TRUE)
      out <- c(out, paste0("# ---- REPORT CODE · ", heading, " ----"))
    }
  }
  out
}

#' The sections of Look 1: one box and one navigation entry each
#'
#' The section comments of `_targets.R` are indented inside its target list;
#' the report headings are the `# ---- REPORT CODE ·` lines [read_pipeline_route()]
#' writes. Other comments, in `_targets.R`'s preamble or inside a report chunk,
#' stay code.
#'
#' @param lines Look 1 lines from [read_pipeline_route()].
#' @return List of sections: `id`, `line`, `box_title`, `group`, `label`.
find_route_sections <- function(lines) {
  pattern <- "^\\s*# ---- (.+?) -*\\s*$"
  hits <- which(grepl(pattern, lines) & grepl("^(\\s+# ---- |# ---- REPORT CODE · )", lines))
  titles <- sub(" -+$", "", sub(pattern, "\\1", lines[hits]))
  groups <- sub("^([^:·]+?)\\s*[:·].*$", "\\1", titles)
  labels <- trimws(sub("^[^:·]+?\\s*[:·]\\s*", "", titles))
  labels[labels == titles] <- ""
  ids <- make.unique(paste0("route-", gsub("(^-|-$)", "", gsub("[^a-z0-9]+", "-", tolower(titles)))), sep = "-")
  lapply(seq_along(hits), function(i) list(
    id = ids[i], line = lines[hits[i]], box_title = titles[i],
    group = trimws(groups[i]), label = if (nzchar(labels[i])) labels[i] else trimws(groups[i])
  ))
}

#' Build the page specification from the saved code
#'
#' @param root The analysis root (panel/analysis).
#' @param commit Short commit hash the page states.
#' @param commit_url URL of that commit.
#' @return List: `page` (the commit and its URL), `navigation`,
#'   `target_sections` and `target_code` (Look 1), `pane_calls` and `panes`
#'   (Look 2), `helper_calls` and `helpers` (Look 3).
build_pipeline_view <- function(root, commit, commit_url) {
  functions <- index_pipeline_functions(list.files(file.path(root, "R"), pattern = "[.]R$", full.names = TRUE))
  route <- read_pipeline_route(file.path(root, "_targets.R"), file.path(root, "report", "results_draft.qmd"))
  sections <- find_route_sections(route)
  called <- unique(unlist(regmatches(route, gregexpr("[A-Za-z_.][A-Za-z0-9_.]*(?=\\s*\\()", route, perl = TRUE))))
  pane_names <- intersect(called, names(functions))
  helper_names <- character(0)
  frontier <- pane_names
  while (length(frontier)) {
    reached <- setdiff(intersect(unlist(lapply(functions[frontier], `[[`, "calls")), names(functions)), helper_names)
    helper_names <- c(helper_names, reached)
    frontier <- reached
  }
  where <- function(f) paste0(functions[[f]]$file, ", line ", functions[[f]]$line)
  navigation <- list()
  previous <- ""
  for (s in sections) {
    item <- list(id = paste0("nav-", s$id), label = s$label, anchor = s$id, disabled = FALSE, title = s$box_title)
    if (!identical(s$group, previous)) item$group <- s$group
    previous <- s$group
    navigation[[length(navigation) + 1L]] <- item
  }
  list(
    page = list(source_commit = commit, source_url = commit_url),
    navigation = navigation,
    target_sections = lapply(sections, function(s) list(id = s$id, line = s$line, box_title = s$box_title)),
    target_code = paste(route, collapse = "\n"),
    pane_calls = stats::setNames(pane_names, pane_names),
    helper_calls = stats::setNames(helper_names, helper_names),
    panes = stats::setNames(lapply(pane_names, function(f) list(
      owner = where(f), title = functions[[f]]$title,
      authority = paste0("Saved code: ", where(f), ". Calls to other functions of R/ open them in Look 3."),
      code = functions[[f]]$code
    )), pane_names),
    helpers = stats::setNames(lapply(helper_names, function(f) list(
      name = f, owner = where(f), badge = "SAVED CODE", text = functions[[f]]$title,
      code = functions[[f]]$code
    )), helper_names)
  )
}

#' The page's fixed texts: the commit labels and the heading of each look
#'
#' Each entry fills the template placeholder of its name in capitals, so
#' `look3_file_kind` fills `@@LOOK3_FILE_KIND@@`. `look3_empty` is what Look 3
#' shows before a call is selected, for a function with and without calls of
#' its own.
#'
#' @param page `page` of the specification.
#' @return Named list of HTML strings.
render_page_metadata <- function(page) {
  commit <- html_escape(page$source_commit)
  url <- html_escape(page$source_url)
  list(
    side_note = paste0("SAVED CODE REVIEW<br>COMMIT ", commit),
    page_badge = if (nzchar(url)) {
      paste0('<a class="page-badge" href="', url, '" rel="noopener">SAVED CODE — COMMIT ', commit, "</a>")
    } else {
      paste0('<span class="page-badge">SAVED CODE — COMMIT ', commit, "</span>")
    },
    target_file_kind = paste0("Saved source file · commit ", commit),
    target_file_meta = paste0("Pipeline review<br>commit ", commit),
    pane_file_kind = paste0("Saved source file · selected function · commit ", commit),
    box_file_kind = paste0("SAVED SOURCE FILE · COMMIT ", commit),
    authority_label = "SAVED CODE · the function's documentation, then its body as committed",
    page_title = paste0("Code Browser — Social Motives and Ideology · ", commit),
    route_note = paste0("<code>_targets.R</code> as committed, then the R chunks of the report under its ",
                        "headings. Underlined calls open the function in Look 2; calls inside it open ",
                        "Look 3."),
    look1_head = paste0("<b>LOOK 1 · empirical object and order</b>",
                        "<span>What is produced, from what, and in what order?</span>",
                        '<span class="legend"><u>underlined call</u> = open the function in Look 2</span>'),
    look2_head = paste0("<b>LOOK 2 · the function</b>",
                        "<span>Its documentation, then its body as committed.</span>",
                        '<span class="legend"><u>underlined call</u> = open it in Look 3</span>'),
    look3_head = paste0("<b>LOOK 3 · a function it calls</b>",
                        "<span>The selected call: that function's documentation, then its body.</span>",
                        '<span class="legend"><u>underlined call</u> = open it in Look 3</span>'),
    look3_file_kind = paste0("Saved source file · called function · commit ", commit),
    look3_empty = paste0(
      "{calls:{badge:", js_string("NO CALL SELECTED"),
      ",text:", js_string("Select an underlined call in Look 2 to open that function here."), "},",
      "none:{badge:", js_string("NO CALLS"),
      ",text:", js_string("Calls no other function of R/."), "}}"
    )
  )
}

#' The navigation: one button per Look 1 section, under its group heading
#'
#' @param navigation `navigation` of the specification.
#' @return HTML.
render_navigation <- function(navigation) {
  paste(vapply(navigation, function(item) {
    group <- if (is.null(item$group)) "" else paste0(
      '<div class="section-nav-group">', html_escape(item$group), "</div>")
    attributes <- c(paste0('id="', html_escape(item$id), '"'), 'type="button"')
    if (!is.null(item$anchor)) {
      attributes <- c(attributes,
                      paste0('data-section="', html_escape(item$anchor), '"'))
    }
    if (isTRUE(item$disabled)) attributes <- c(attributes, "disabled")
    if (!is.null(item$title)) {
      attributes <- c(attributes, paste0('title="', html_escape(item$title), '"'))
    }
    paste0(group, "<button ", paste(attributes, collapse = " "), ">",
           html_escape(item$label), "</button>")
  }, character(1)), collapse = "")
}

#' Look 1 as a JavaScript array of lines; calls of Look 2 functions are buttons
#'
#' @param specification The page specification.
#' @return JavaScript array literal.
render_target_lines <- function(specification) {
  section_lines <- setNames(
    vapply(specification$target_sections, `[[`, character(1), "id"),
    vapply(specification$target_sections, `[[`, character(1), "line")
  )
  section_metadata <- setNames(specification$target_sections,
    vapply(specification$target_sections, `[[`, character(1), "line"))
  lines <- strsplit(specification$target_code, "\n", fixed = TRUE)[[1]]
  rendered <- vapply(lines, function(line) {
    if (!nzchar(line)) return('{"html":"","blank":true}')
    if (line %in% names(section_lines)) {
      value <- paste0('<span id="', section_lines[[line]],
                      '-section" class="comment">', html_escape(line), "</span>")
    } else {
      value <- link_calls(line, specification$pane_calls,
                          css_class = "target-ref", data_attribute = "pane")
    }
    metadata <- section_metadata[[line]]
    box <- if (is.null(metadata$box_title)) "" else paste0(
      ',"boxTitle":', js_string(metadata$box_title),
      ',"boxId":', js_string(metadata$id))
    paste0('{"html":', js_string(value), box, "}")
  }, character(1))
  paste0("[", paste(rendered, collapse = ","), "]")
}

#' Look 2 as a JavaScript object; calls of Look 3 functions are buttons
#'
#' @param specification The page specification.
#' @return JavaScript object literal.
render_panes <- function(specification) {
  entries <- Map(function(key, pane) {
    code <- strsplit(pane$code, "\n", fixed = TRUE)[[1]]
    body <- link_calls(code, specification$helper_calls, css_class = "call", data_attribute = "contract")
    paste0(js_string(key), ":{",
           "owner:", js_string(pane$owner), ",",
           "title:", js_string(pane$title), ",",
           "authority:", js_string(pane$authority), ",",
           "body:[", paste(vapply(body, js_string, character(1)), collapse = ","), "]}")
  }, names(specification$panes), specification$panes)
  paste0("{", paste(entries, collapse = ","), "}")
}

#' Look 3 as a JavaScript object; calls of other Look 3 functions are buttons
#'
#' @param specification The page specification.
#' @return JavaScript object literal.
render_helpers <- function(specification) {
  entries <- Map(function(key, helper) {
    code <- strsplit(helper$code, "\n", fixed = TRUE)[[1]]
    lines <- link_calls(code, specification$helper_calls, css_class = "call", data_attribute = "contract")
    paste0(js_string(key), ":{",
           "name:", js_string(helper$name), ",",
           "owner:", js_string(helper$owner), ",",
           "badge:", js_string(helper$badge), ",",
           "text:", js_string(helper$text), ",",
           "code:[", paste(vapply(lines, js_string, character(1)), collapse = ","), "]}")
  }, names(specification$helpers), specification$helpers)
  paste0("{", paste(entries, collapse = ","), "}")
}

#' Escape lines of code and turn the calls of `call_map` into buttons
#'
#' @param text Lines of code.
#' @param call_map Named character vector: call -> key of the function it opens.
#' @param css_class,data_attribute Class and data attribute of the buttons.
#' @return HTML.
link_calls <- function(text, call_map, css_class, data_attribute) {
  rendered <- html_escape(text)
  calls <- names(call_map)[order(nchar(names(call_map)), decreasing = TRUE)]
  for (call in calls) {
    pattern <- paste0("(?<![A-Za-z0-9_.])", regex_escape(call),
                      "(?=\\s*\\()")
    replacement <- paste0('<button class="', css_class, '" data-',
                          data_attribute, '="', html_escape(call_map[[call]]),
                          '">', html_escape(call), "</button>")
    rendered <- gsub(pattern, replacement, rendered, perl = TRUE)
  }
  rendered
}

#' Escape text for HTML
#'
#' @param value Text.
#' @return Escaped text.
html_escape <- function(value) {
  value <- enc2utf8(as.character(value))
  value <- gsub("&", "&amp;", value, fixed = TRUE)
  value <- gsub("<", "&lt;", value, fixed = TRUE)
  value <- gsub(">", "&gt;", value, fixed = TRUE)
  value <- gsub('"', "&quot;", value, fixed = TRUE)
  gsub("'", "&#39;", value, fixed = TRUE)
}

#' Quote text as a JavaScript string literal
#'
#' @param value Text.
#' @return The literal, in double quotes.
js_string <- function(value) {
  value <- enc2utf8(as.character(value))
  value <- gsub("\\", "\\\\", value, fixed = TRUE)
  value <- gsub('"', '\\"', value, fixed = TRUE)
  value <- gsub("\r", "\\r", value, fixed = TRUE)
  value <- gsub("\n", "\\n", value, fixed = TRUE)
  value <- gsub("\t", "\\t", value, fixed = TRUE)
  paste0('"', value, '"')
}

#' Count the occurrences of a fixed string
#'
#' @param text,pattern The text and the string to count in it.
#' @return Integer.
fixed_count <- function(text, pattern) {
  if (!nzchar(pattern)) return(0L)
  lengths(strsplit(text, pattern, fixed = TRUE)) - 1L
}

#' Escape the metacharacters of a regular expression
#'
#' @param value Text.
#' @return Escaped text.
regex_escape <- function(value) {
  gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", value)
}

#' Fill each placeholder of the template, which must occur exactly once
#'
#' @param template The template as one string.
#' @param replacements Named list: placeholder -> HTML.
#' @return The filled page.
fill_page_template <- function(template, replacements) {
  for (placeholder in names(replacements)) {
    if (fixed_count(template, placeholder) != 1L) {
      stop("Template must contain `", placeholder, "` exactly once.")
    }
    template <- sub(placeholder, replacements[[placeholder]], template, fixed = TRUE)
  }
  if (grepl("@@[A-Z0-9_]+@@", template, perl = TRUE)) stop("Template placeholder left unfilled.")
  template
}

#' Render the page specification into the template
#'
#' @param specification Page specification from [build_pipeline_view()].
#' @param template The template review/three-look-template.html as one string.
#' @return The page as HTML.
render_pipeline_view <- function(specification, template) {
  page <- render_page_metadata(specification$page)
  fill_page_template(template, c(
    stats::setNames(page, paste0("@@", toupper(names(page)), "@@")),
    list(
      "@@NAVIGATION@@" = render_navigation(specification$navigation),
      "@@TARGET_LINES@@" = render_target_lines(specification),
      "@@PANES@@" = render_panes(specification),
      "@@HELPERS@@" = render_helpers(specification)
    )
  ))
}

#' The commit the page shows, and the link to it
#'
#' The page shows committed source only: the analysis root and the
#' preregistration next to it must have no uncommitted or untracked changes,
#' unless `allow_uncommitted` labels the page a preparation preview.
#'
#' @param root The analysis root (panel/analysis), inside a Git checkout.
#' @param repository_url Public GitHub address of the repository, or "" for
#'   no link.
#' @param allow_uncommitted Whether uncommitted changes give a labelled
#'   preview instead of an error.
#' @return List: `commit` (the label the page shows) and `commit_url` ("" without a link).
describe_view_source <- function(root, repository_url, allow_uncommitted) {
  git <- function(args) suppressWarnings(system2("git", c("-C", shQuote(root), args), stdout = TRUE, stderr = FALSE))
  revision <- git(c("rev-parse", "--verify", "HEAD"))
  if (length(revision) != 1L || !is.null(attr(revision, "status"))) stop("The Code Browser needs a Git checkout with a commit.")
  changes <- git(c("status", "--porcelain", "--untracked-files=normal", "--", ".", "../preregistration"))
  clean <- !length(changes)
  if (!clean && !isTRUE(allow_uncommitted)) {
    stop("The Code Browser shows committed source only; commit first, or pass allow_uncommitted = TRUE for a preparation preview.")
  }
  if (nzchar(repository_url) && !grepl("^https://github[.]com/[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+/?$", repository_url)) {
    stop("repository_url must be the address of a GitHub repository, https://github.com/<owner>/<name>.")
  }
  short <- substr(revision, 1L, 7L)
  list(
    commit = if (clean) short else paste(short, "with uncommitted changes (preparation preview)"),
    commit_url = if (clean && nzchar(repository_url)) {
      paste0(sub("/$", "", repository_url), "/tree/", revision, "/panel/analysis")
    } else ""
  )
}

#' Write the page of the saved pipeline and the list of its sections
#'
#' @param root The analysis root (panel/analysis).
#' @param output_path Path of the page to write; the section list is written
#'   to the same path with `.sections.json` appended.
#' @param repository_url Public GitHub address the source links point to.
#' @param allow_uncommitted Whether uncommitted changes give a labelled preview.
#' @return Normalised path of the written page.
generate_pipeline_view <- function(root = ".", output_path = file.path(root, "report", "code-browser.html"),
                                   repository_url = Sys.getenv("PUBLICATION_REPOSITORY_URL", ""),
                                   allow_uncommitted = FALSE) {
  source <- describe_view_source(root, repository_url, allow_uncommitted)
  template <- paste(readLines(file.path(root, "review", "three-look-template.html"), warn = FALSE, encoding = "UTF-8"),
                    collapse = "\n")
  specification <- build_pipeline_view(root, source$commit, source$commit_url)
  html <- render_pipeline_view(specification, template)
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  writeLines(enc2utf8(html), output_path, useBytes = TRUE)
  jsonlite::write_json(lapply(specification$target_sections, function(x) list(id = x$id, title = x$box_title)),
                       paste0(output_path, ".sections.json"), auto_unbox = TRUE, pretty = TRUE)
  normalizePath(output_path)
}

if (sys.nframe() == 0L) {
  cat(generate_pipeline_view(), "\n")
}
