# The preregistration as the one source of every sentence the report repeats
# from it. The report holds only keys: the first words of a
# passage and, where the passage is longer or shorter than one sentence, its
# last words. The text is read from the preregistration file when the report
# renders, so a reworded preregistration reaches the report at the next render,
# and a key that does not match stops the render and names itself.

#' Read the preregistration for quotation
#'
#' Struck passages (`~~…~~`), HTML comments and review callouts are not text
#' of the preregistration and are dropped. Spans and links are unwrapped to
#' their text, emphasis markers are removed. A paragraph set entirely in
#' italics is a proposal not yet accepted and is marked as such,
#' so that [rh_prereg_passage()] refuses to quote it.
#'
#' @param path The preregistration source (`.qmd`).
#' @return A data frame with one row per paragraph: `text`, `section` (the
#'   label of the section the paragraph stands in, "AP10", "M4", "RQ1", or the
#'   top-level heading where a section carries no label), `proposal`.
rh_read_preregistration <- function(path) {
  text <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  text <- sub("(?s)^---\n.*?\n---\n", "", text, perl = TRUE)
  text <- gsub("(?s)<!--.*?-->", "", text, perl = TRUE)
  text <- gsub("(?s)~~.*?~~", "", text, perl = TRUE)
  text <- gsub("(?s)\n:::+[ ]*\\{[^}\n]*callout[^}\n]*\\}.*?\n:::+[ ]*(?=\n|$)", "\n", text, perl = TRUE)
  text <- gsub("(?s)\n```.*?\n```[^\n]*", "\n", text, perl = TRUE)   # code chunks
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  lines <- lines[!grepl("^\\s*\\|", lines) & !grepl("^: ", lines) & !grepl("^:::", lines)]  # tables, captions, divs
  section <- NA_character_
  subsection <- NA_character_
  top <- NA_character_
  rows <- list()
  buffer <- character()
  flush <- function() {
    if (length(buffer) == 0L) return(invisible())
    para <- paste(buffer, collapse = " ")
    buffer <<- character()
    para <- rh_prereg_unwrap(para)
    if (!nzchar(trimws(para))) return(invisible())
    proposal <- grepl("^\\*[^*].*[^*]\\*$", trimws(para), perl = TRUE)
    para <- gsub("(?<!\\*)\\*(?!\\*)", "", para, perl = TRUE)   # single emphasis markers
    para <- gsub("**", "", para, fixed = TRUE)                    # bold markers
    para <- gsub("[ \t]+", " ", trimws(para))
    rows[[length(rows) + 1L]] <<- data.frame(
      text = para, section = if (is.na(section)) top else section,
      subsection = subsection, proposal = proposal, stringsAsFactors = FALSE
    )
  }
  for (line in lines) {
    if (grepl("^#{1,6} ", line)) {
      flush()
      heading <- sub("\\s*\\{[^}]*\\}\\s*$", "", sub("^#+\\s+", "", line))
      if (grepl("^### ", line)) {
        subsection <- trimws(gsub("[*~]", "", heading))
        next
      }
      if (grepl("^#### ", line)) next
      subsection <- NA_character_
      if (grepl("^# ", line)) {
        top <- heading
        section <- NA_character_
      } else if (grepl("^## ", line)) {
        id <- regmatches(heading, regexpr("^(T[0-9]+|A[0-9]+|M[0-9]+|AP[0-9]+|O[0-9]+|S?RQ[0-9]+)(?=\\b)", heading, perl = TRUE))
        section <- if (length(id) == 1L) id else NA_character_
      }
      next
    }
    if (!nzchar(trimws(line))) { flush(); next }
    buffer <- c(buffer, line)
  }
  flush()
  do.call(rbind, rows)
}

rh_regex_escape <- function(x) gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x, perl = TRUE)

# Spans `[text]{.class}` and links `[text](url)` become their text; repeated
# for nesting.
rh_prereg_unwrap <- function(x) {
  repeat {
    y <- gsub("\\[([^][]*)\\]\\{[^}]*\\}", "\\1", x, perl = TRUE)
    y <- gsub("\\[([^][]*)\\]\\([^)]*\\)", "\\1", y, perl = TRUE)
    if (identical(y, x)) return(y)
    x <- y
  }
}

#' Quote a passage of the preregistration by its first words
#'
#' @param prereg The data frame from [rh_read_preregistration()].
#' @param from The first words of the passage, verbatim.
#' @param to The last words of the passage, verbatim; `NULL` ends the passage
#'   at the end of the sentence `from` begins (a full stop, question or
#'   exclamation mark followed by a capital, a bracket, a quotation mark or the
#'   end of the paragraph).
#' @param span Wrap the text as `[…]{.prereg}` (the class is invisible in the
#'   render and marks quoted preregistration text in the source).
#' @return The passage as one string. The render stops with the key when the
#'   passage is not found, is found with differing wording in several places,
#'   or stands only in a paragraph that is still an italic proposal.
rh_prereg_passage <- function(prereg, from, to = NULL, span = TRUE) {
  stopifnot(is.data.frame(prereg), is.character(from), length(from) == 1L, nzchar(from))
  found <- character()
  sections <- character()
  proposals <- logical()
  for (i in seq_len(nrow(prereg))) {
    para <- prereg$text[[i]]
    # the first letter of a key may differ in case from the preregistration (a
    # sentence quoted mid-sentence); the passage then takes the key's case
    first <- substr(from, 1L, 1L)
    from_pattern <- paste0("[", toupper(first), tolower(first), "]", rh_regex_escape(substr(from, 2L, nchar(from))))
    starts <- gregexpr(from_pattern, para, perl = TRUE)[[1]]
    if (starts[[1]] < 0L) next
    for (start in starts) {
      rest <- substr(para, start, nchar(para))
      rest <- paste0(first, substr(rest, 2L, nchar(rest)))
      if (is.null(to)) {
        end_in_rest <- regexpr("[.?!](?=\\s+[[:upper:]\u201c\"(\\[]|\\s*$)", rest, perl = TRUE)
        passage <- if (end_in_rest > 0L) substr(rest, 1L, end_in_rest) else rest
      } else {
        to_at <- regexpr(to, rest, fixed = TRUE)
        if (to_at < 0L) next
        passage <- substr(rest, 1L, to_at + nchar(to) - 1L)
      }
      found <- c(found, trimws(passage))
      sections <- c(sections, prereg$section[[i]])
      proposals <- c(proposals, prereg$proposal[[i]])
    }
  }
  key <- if (is.null(to)) paste0("\"", from, "\"") else paste0("\"", from, "\" … \"", to, "\"")
  if (length(found) == 0L) {
    stop("The preregistration has no passage starting ", key, "; the report's key must follow the preregistration.", call. = FALSE)
  }
  accepted <- found[!proposals]
  if (length(accepted) == 0L) {
    stop("The passage starting ", key, " stands only in an italic proposal of the preregistration, not in its accepted text.", call. = FALSE)
  }
  distinct <- unique(accepted)
  if (length(distinct) > 1L) {
    stop("The key ", key, " matches differently worded passages of the preregistration:\n  ",
         paste(substr(distinct, 1L, 160L), collapse = "\n  "), call. = FALSE)
  }
  text <- distinct[[1]]
  attr(text, "section") <- sections[!proposals][[1]]
  if (span) paste0("[", rh_prereg_escape(text), "]{.prereg}") else text
}

#' The section label of a quoted passage ("AP10", "M4", "RQ1")
#' @inheritParams rh_prereg_passage
rh_prereg_section <- function(prereg, from, to = NULL) {
  attr(rh_prereg_passage(prereg, from, to, span = FALSE), "section")
}

#' Copy a whole section of the preregistration by its heading
#'
#' Returns every accepted paragraph under a third-level heading of a
#' preregistration section, exactly as it stands. Struck text, tables and their captions, code, comments and review boxes are left
#' out, and so is a paragraph set entirely in italics, which is a proposal not
#' yet accepted. Within a paragraph the newest wording stands.
#'
#' @param prereg The data frame from [rh_read_preregistration()].
#' @param section The section label, "RQ1", "AP7".
#' @param heading The third-level heading, as printed, without markup.
#' @param label The label printed after the last paragraph, "(RQ1)"; `NULL` for none.
#' @param proposal Set every paragraph in italics, the markup of a proposal shown for review.
#' @return The paragraphs, separated by blank lines, each wrapped as `[…]{.prereg}`.
rh_prereg_under_heading <- function(prereg, section, heading, label = paste0("(", section, ")"), proposal = FALSE) {
  rows <- prereg[prereg$section %in% section & prereg$subsection %in% heading, , drop = FALSE]
  if (nrow(rows) == 0L) {
    stop("The preregistration has no heading \"", heading, "\" in section ", section,
         "; the report's reference must follow the preregistration.", call. = FALSE)
  }
  paras <- rows$text[!rows$proposal]
  # a parenthetical reference to a preregistration table, "(@tbl-…)", has no
  # target in the report and goes
  paras <- gsub("\\s*\\(@tbl-[A-Za-z0-9-]+\\)", "", paras, perl = TRUE)
  if (length(paras) == 0L) {
    stop("Everything under \"", heading, "\" in ", section, " is an italic proposal not yet accepted.", call. = FALSE)
  }
  paras <- paste0("[", rh_prereg_escape(paras), "]{.prereg}")
  if (proposal) paras <- paste0("*", paras, "*")
  if (!is.null(label)) paras[length(paras)] <- paste(paras[length(paras)], label)
  paste(paras, collapse = "\n\n")
}

#' One research question, copied from its section's "Psychological Question(s)"
#'
#' @param question "RQ1", "RQ2a", "SRQ1".
#' @return The question without its label, wrapped as `[…]{.prereg}`.
rh_prereg_question <- function(prereg, question) {
  section <- sub("[a-z]$", "", question)
  rows <- prereg[prereg$section %in% section & prereg$subsection %in% c("Psychological Question", "Psychological Questions") &
                   !prereg$proposal, , drop = FALSE]
  if (nrow(rows) == 0L) stop("The preregistration has no psychological question in section ", section, ".", call. = FALSE)
  prefix <- paste0(question, ":")
  hit <- rows$text[startsWith(rows$text, prefix)]
  if (length(hit) == 0L && nrow(rows) == 1L && !grepl("^S?RQ[0-9]", rows$text)) hit <- rows$text
  if (length(hit) != 1L) stop("The preregistration has no single question ", question, ".", call. = FALSE)
  paste0("[", rh_prereg_escape(trimws(sub(paste0("^", prefix), "", hit))), "]{.prereg}")
}

# Square brackets inside a copied passage ("[central 95% credible interval]")
# would close the span early; they are escaped for Pandoc.
rh_prereg_escape <- function(x) gsub("([][])", "\\\\\\1", x, perl = TRUE)
