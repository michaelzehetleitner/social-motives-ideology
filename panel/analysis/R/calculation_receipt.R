#' Record the calculation receipt of a build
#'
#' SHA-256 fingerprints of the source and input files, the configuration, the
#' git revision and whether the tree was dirty, the R version and the build time.
#'
#' @param source_files,input_files Character vectors of existing files. Relative
#'   paths are resolved against analysis_plan$root; receipt paths are relative to that root.
#' @param analysis_plan Configuration used by the calculations, including root.
#' @return List `cfg`, `input_files`, `source_files`, `hash_algorithm`, `git_head`,
#'   `git_dirty`, `built_at`, `r_version`.
record_calculation_receipt <- function(source_files, input_files, analysis_plan) {
  root <- normalizePath(analysis_plan$root, winslash = "/", mustWork = TRUE)

  relative_path <- function(path) {
    from <- strsplit(root, "/", fixed = TRUE)[[1L]]
    to <- strsplit(path, "/", fixed = TRUE)[[1L]]
    common <- 0L
    while (common < min(length(from), length(to)) &&
           identical(from[common + 1L], to[common + 1L])) {
      common <- common + 1L
    }
    paste(c(rep("..", length(from) - common),
            to[seq.int(common + 1L, length(to))]), collapse = "/")
  }

  file_hashes <- function(files, argument) {
    paths <- path.expand(files)
    absolute <- grepl("^(/|[A-Za-z]:[/\\\\])", paths)
    paths[!absolute] <- file.path(root, paths[!absolute])
    paths <- unique(normalizePath(paths, winslash = "/", mustWork = TRUE))
    data.frame(
      path = vapply(paths, relative_path, character(1), USE.NAMES = FALSE),
      sha256 = vapply(paths, function(path) digest::digest(file = path, algo = "sha256"),
                      character(1), USE.NAMES = FALSE),
      stringsAsFactors = FALSE
    )
  }

  git_read <- function(args) {
    if (!nzchar(Sys.which("git"))) return(NULL)
    value <- tryCatch(
      suppressWarnings(system2("git", c("-C", shQuote(root), args),
                               stdout = TRUE, stderr = FALSE)),
      error = function(e) NULL
    )
    if (is.null(value) || !is.null(attr(value, "status"))) NULL else value
  }
  head <- git_read(c("rev-parse", "HEAD"))
  status <- git_read(c("status", "--porcelain", "--untracked-files=normal"))
  dirty <- if (is.null(status)) NA else length(status) > 0L
  # Publication containers omit .git. Their builder verifies the checkout and
  # supplies both values explicitly; a local Git checkout remains authoritative.
  if (length(head) != 1L) {
    source_commit <- Sys.getenv("ZM_SOURCE_COMMIT", "")
    source_dirty <- Sys.getenv("ZM_SOURCE_DIRTY", "")
    if (nzchar(source_commit) || nzchar(source_dirty)) {
      if (!grepl("^([a-f0-9]{40}|[a-f0-9]{64})$", source_commit) ||
          !source_dirty %in% c("true", "false")) {
        stop("Without Git, ZM_SOURCE_COMMIT must be a full commit hash and ZM_SOURCE_DIRTY must be 'true' or 'false'.")
      }
      head <- source_commit
      dirty <- identical(source_dirty, "true")
    }
  }
  list(
    cfg = analysis_plan,
    input_files = file_hashes(input_files, "input_files"),
    source_files = file_hashes(source_files, "source_files"),
    hash_algorithm = "sha256",
    git_head = if (length(head) == 1L) unname(head) else NA_character_,
    git_dirty = dirty,
    built_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    r_version = R.version.string
  )
}
