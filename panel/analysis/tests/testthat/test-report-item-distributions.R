local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) {
    parent <- dirname(dir)
    if (identical(parent, dir)) stop("project root not found from ", getwd())
    dir <- parent
  }
  source(file.path(dir, "R", "report_item_distributions.R"), local = FALSE)
})

test_that("item distributions expose empty categories and use observed denominators", {
  cb <- list(scales = tibble::tibble(label = "Example scale", item_codes = list(c("a", "b"))))
  analysis_plan <- list(scales = list(response_min = 1, response_max = 6))
  items <- data.frame(a = c(1, 1, 3, NA), b = c(6, 6, 6, 6))
  tab <- rh_item_distribution_table(items, cb, analysis_plan)
  expect_equal(tab$N, c(3L, 4L))
  expect_equal(tab$Missing, c(1L, 0L))
  expect_equal(tab$`Response 1`, c("2 (66.7%)", "0 (0.0%)"))
  expect_equal(tab$`Response 2`, c("0 (0.0%)", "0 (0.0%)"))
  expect_equal(tab$`Response 3`, c("1 (33.3%)", "0 (0.0%)"))
  expect_equal(tab$`Response 6`, c("0 (0.0%)", "4 (100.0%)"))
})

test_that("item distributions need integer response bounds from the configuration", {
  cb <- list(scales = tibble::tibble(label = "Example scale", item_codes = list("a")))
  expect_error(
    rh_item_distribution_table(data.frame(a = c(1, 2)), cb, list(scales = list(response_min = 1))),
    "integer response bounds"
  )
})
