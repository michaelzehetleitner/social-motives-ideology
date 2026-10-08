# Quarto runs the preregistration's R chunks from this folder. With the R
# version the analysis lockfile records, load that project's renv library
# (panel/analysis/renv.lock), so the document renders with the locked package
# versions; any other R keeps its own library.
local({
  analysis <- normalizePath(file.path("..", "analysis"), mustWork = FALSE)
  lockfile <- file.path(analysis, "renv.lock")
  if (!file.exists(file.path(analysis, "renv", "activate.R")) || !file.exists(lockfile)) return(invisible())
  header <- readLines(lockfile, n = 5L, warn = FALSE)
  locked <- regmatches(header, regexpr("[0-9]+\\.[0-9]+\\.[0-9]+", header))
  running <- paste(R.version$major, R.version$minor, sep = ".")
  if (length(locked) && identical(locked[[1]], running)) {
    owd <- setwd(analysis)
    on.exit(setwd(owd))
    source("renv/activate.R")
  }
})
