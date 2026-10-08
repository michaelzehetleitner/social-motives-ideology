# R/ap10_inference.R: which cells of the prediction table carry a directional
# prediction

local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) {
    parent <- dirname(dir)
    if (identical(parent, dir)) stop("project root not found from ", getwd())
    dir <- parent
  }
  for (f in c("config.R", "ap10_inference.R")) {
    source(file.path(dir, "R", f), local = FALSE)
  }
})

test_that("the directional predictions are the cells marked + or -", {
  expect_identical(ap7_is_directional(c("+", "-", "\u00b1", "", NA)),
                   c(TRUE, TRUE, FALSE, FALSE, FALSE))
})

