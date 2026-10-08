local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) dir <- dirname(dir)
  source(file.path(dir, "R", "ap6_regressions.R"), local = FALSE)
})

test_that("missing diagnostics cannot appear to be zero failures or acceptable BFMI", {
  nuts <- expand.grid(Iteration = 1:4, Chain = 1:2,
                      Parameter = c("energy__", "divergent__", "treedepth__"), stringsAsFactors = FALSE)
  nuts$Value <- ifelse(nuts$Parameter == "energy__", rep(c(1, 3, 2, 4), 6),
                       ifelse(nuts$Parameter == "treedepth__", 4, 0))
  ok <- ap6_sampler_summary(nuts, 10, 2)
  expect_equal(ok$n_divergent, 0)
  expect_equal(ok$n_treedepth_hits, 0)
  expect_true(is.finite(ok$bfmi_min))
  fields <- c(energy__ = "bfmi_min", divergent__ = "n_divergent", treedepth__ = "n_treedepth_hits")
  for (parameter in names(fields)) {
    missing <- nuts
    missing$Value[which(missing$Parameter == parameter)[1]] <- NA_real_
    expect_true(is.na(ap6_sampler_summary(missing, 10, 2)[[fields[[parameter]]]]))
    absent <- nuts[!(nuts$Parameter == parameter & nuts$Chain == 2), ]
    expect_true(is.na(ap6_sampler_summary(absent, 10, 2)[[fields[[parameter]]]]))
  }
  flat <- nuts
  flat$Value[flat$Parameter == "energy__" & flat$Chain == 2] <- 1
  expect_true(is.na(ap6_sampler_summary(flat, 10, 2)$bfmi_min))
})
