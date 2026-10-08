# The supplement's complete-number files: the writer, the long table of every
# exploratory solution's loadings, and the targets that write them.

local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) {
    parent <- dirname(dir)
    if (identical(parent, dir)) stop("project root not found from ", getwd())
    dir <- parent
  }
  for (f in c("config.R", "result_outputs.R", "ap9_efa.R")) {
    source(file.path(dir, "R", f), local = FALSE)
  }
  assign("result_files_root", dir, envir = .GlobalEnv)
})

result_plan <- zm_config("smoke", file.path(result_files_root, "config", "analysis_plan.yaml"))

test_that("the plan names one file for each complete-number table", {
  expect_setequal(
    names(result_plan$data_files$result_files),
    c("cfa_loadings", "efa_loadings", "efa_membership", "prior_power_scaling",
      "posterior_predictive_checks"))
})

test_that("write_result_file() writes the table to the configured path and needs the key", {
  plan <- result_plan
  plan$data_files$result_files$cfa_loadings <- withr::local_tempfile(fileext = ".csv")
  table <- tibble::tibble(model = c("a", "b"), loading = c(0.5, NA))
  written <- write_result_file(table, plan, "cfa_loadings")
  expect_true(file.exists(written))
  back <- readr::read_csv(written, show_col_types = FALSE, progress = FALSE)
  expect_identical(names(back), c("model", "loading"))
  expect_equal(back$loading, c(0.5, NA))
  expect_error(write_result_file(table, plan, "not_a_key"), "result_files\\$not_a_key")
})

test_that("extract_efa_loadings() gives every item and factor of every fitted solution", {
  membership <- tibble::tibble(item = c("i1", "i2", "i3"), column = c("c1", "c2", "c3"),
                               theoretical_scale = c("s1", "s1", "s2"))
  loadings <- matrix(c(0.7, 0.6, 0.1, 0.05, 0.1, 0.8), nrow = 3,
                     dimnames = list(c("c1", "c2", "c3"), c("WLS1", "WLS2")))
  fits <- list(
    list(set_name = "set", factors = 2L, rotation = "oblimin", ok = TRUE,
         loadings = loadings, set = list(item_membership = membership)),
    list(set_name = "set", factors = 3L, rotation = "oblimin", ok = FALSE,
         loadings = NULL, set = list(item_membership = membership))
  )
  out <- extract_efa_loadings(fits)
  expect_identical(names(out), c("item_set", "factors", "rotation", "item", "column",
                                 "intended_scale", "factor", "loading"))
  expect_equal(nrow(out), 6L)
  expect_true(all(out$factors == 2L))
  expect_equal(out$loading[out$item == "i3" & out$factor == "WLS2"], 0.8)
  expect_identical(unique(out$intended_scale[out$item %in% c("i1", "i2")]), "s1")
})

test_that("each complete-number file has its own pipeline target", {
  expressions <- parse(file.path(result_files_root, "_targets.R"))
  text <- paste(vapply(expressions, function(e) paste(deparse(e), collapse = " "), ""), collapse = " ")
  for (key in names(result_plan$data_files$result_files)) {
    expect_match(text, paste0('"', key, '"'), fixed = TRUE, info = key)
  }
})
