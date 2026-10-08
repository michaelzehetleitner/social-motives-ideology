# SRQ1 loading clarity in the report: the
# synthetic data never reach a second loading of .40, so the cross-loading
# branch, its count and the scale of the factor a cross-loading item leans
# towards, is exercised here on a constructed loading matrix. The matrix runs
# through the AP9 route functions (item assignment, correspondence, loading
# clarity), the S1 tabulation and the report's sentence and membership table.
# No model is fitted.

local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) {
    parent <- dirname(dir)
    if (identical(parent, dir)) stop("project root not found from ", getwd())
    dir <- parent
  }
  for (f in c("config.R", "ap4_factor_structure.R", "ap9_efa.R", "report_supplement_measurement.R",
              "report_helpers.R", "report_measurement_alternatives.R")) {
    source(file.path(dir, "R", f), local = FALSE)
  }
  assign("lc_root", dir, envir = .GlobalEnv)
})

lc_plan <- zm_config(profile = "smoke", path = file.path(lc_root, "config", "analysis_plan.yaml"))
lc_cutoff <- lc_plan$factor_analysis$loading_display_cutoff
lc_labels <- c(aa = "Alpha scale", bb = "Beta scale", cc = "Gamma scale")
lc_plan$factor_analysis$sets <- list(list(
  key = "abc", label = "Three scales", scales = c("aa", "bb", "cc"),
  expected_factors = 3L, efa = TRUE, cfa = FALSE
))

# Three scales of three items, one factor each. BB_2 loads .62 on its own
# factor and .45 on the factor of the Gamma scale: two loadings at the cutoff,
# one cross-loading item. CC_3 loads on no factor at the cutoff: one weakly
# loading item. Every other second loading stays below .20.
lc_solution <- function(loadings) {
  items <- rownames(loadings)
  list(
    set = list(name = "abc", item_membership = tibble::tibble(
      item = sub("_", "-", items), column = items,
      theoretical_scale = rep(c("aa", "bb", "cc"), each = 3L))),
    set_name = "abc", factors = 3L, rotation = "oblimin", ok = TRUE,
    types = "theoretical", note = NA_character_, loadings = loadings
  )
}
lc_loadings <- function(cross = 0.45) {
  matrix(
    c(0.70, 0.05, 0.02,
      0.68, 0.10, 0.04,
      0.66, 0.03, 0.15,
      0.04, 0.72, 0.06,
      0.02, 0.62, cross,
      0.11, 0.69, 0.05,
      0.06, 0.01, 0.71,
      0.03, 0.12, 0.64,
      0.18, 0.09, 0.33),
    nrow = 9, byrow = TRUE,
    dimnames = list(c("AA_1", "AA_2", "AA_3", "BB_1", "BB_2", "BB_3", "CC_1", "CC_2", "CC_3"),
                    c("F1", "F2", "F3"))
  )
}
lc_correspondence <- function(cross = 0.45) {
  solution <- lc_solution(lc_loadings(cross))
  solution$membership <- ap4_extract_efa_membership(
    solution, ap4_assign_by_largest_absolute_loading(solution$loadings, ties = "first"))
  clarity <- extract_efa_loading_clarity(extract_efa_item_correspondence(list(solution)), lc_plan)
  tabulate_efa_correspondence(clarity, lc_plan)
}

test_that("a cross-loading item is counted and named with the scale of the factor it leans towards", {
  correspondence <- lc_correspondence()
  expect_identical(correspondence$overview$n_crossloading, 1L)
  expect_identical(correspondence$overview$n_weak, 1L)
  m <- correspondence$membership[["abc:3"]]
  row <- m[m$item == "BB-2", ]
  expect_true(row$crossloading)
  expect_identical(row$item_number, 2L)
  expect_identical(row$assigned_scales, "bb")
  expect_identical(row$crossloading_factors, "F3")
  expect_identical(row$crossloading_scales, "cc")
  expect_true(all(is.na(m$crossloading_scales[m$item != "BB-2"])))

  text <- describe_loading_clarity(correspondence, lc_labels, lc_cutoff)
  expect_match(text, "Three scales, three factors (expected): 1 of the 9 items loaded weakly, all of them gamma scale items",
               fixed = TRUE)
  # the count with the second loading and the scale of the factor it lies on
  expect_match(text, "one item cross-loaded: beta scale item 2, with a second loading of .45 on the factor of gamma scale",
               fixed = TRUE)
  # with a cross-loading item, no near miss is named in its place
  expect_false(grepl("largest second loading", text, fixed = TRUE))

  # the membership table marks the item and names its secondary factor in a
  # specific note, not in a column filled in one row
  shown <- rh_efa_membership_table(correspondence$membership, lc_plan$factor_analysis$sets, lc_labels, lc_cutoff,
                                   engine = "gt")
  cells <- shown[["_data"]]
  expect_false(any(c("secondary", "clarity") %in% names(cells)))
  expect_match(cells$largest_1[cells$item == "BB-2"], "<sup>a</sup>", fixed = TRUE)
  expect_false(any(grepl("<sup>a", cells$largest_1[cells$item != "BB-2"], fixed = TRUE)))
  expect_match(cells$largest_1[cells$item == "CC-3"], "<sup>b</sup>", fixed = TRUE)
  note <- paste(unlist(shown[["_source_notes"]]), collapse = " ")
  expect_match(note, "Cross-loading, with two or more absolute loadings of at least .40: BB-2", fixed = TRUE)
  expect_match(note, "its second loading lies on the factor of gamma scale", fixed = TRUE)
  expect_match(note, "Weakly loading, with no absolute loading of at least .40: CC-3", fixed = TRUE)
})

test_that("without a cross-loading item the largest second loading is named instead, and no secondary column shows", {
  correspondence <- lc_correspondence(cross = 0.38)
  expect_identical(correspondence$overview$n_crossloading, 0L)
  text <- describe_loading_clarity(correspondence, lc_labels, lc_cutoff)
  expect_match(text, "none cross-loaded; the largest second loading was .38 (beta scale item 2, on the factor of gamma scale)",
               fixed = TRUE)
  shown <- rh_efa_membership_table(correspondence$membership, lc_plan$factor_analysis$sets, lc_labels, lc_cutoff,
                                   engine = "gt")
  expect_false(any(grepl("<sup>a", shown[["_data"]]$largest_1, fixed = TRUE)))
  expect_false(grepl("Cross-loading", paste(unlist(shown[["_source_notes"]]), collapse = " "), fixed = TRUE))
})

test_that("a loading exactly at the cutoff counts as a second loading", {
  correspondence <- lc_correspondence(cross = lc_cutoff)
  expect_identical(correspondence$overview$n_crossloading, 1L)
  expect_match(describe_loading_clarity(correspondence, lc_labels, lc_cutoff), "one item cross-loaded", fixed = TRUE)
})

test_that("the scales analysed on their own get one clause", {
  correspondence <- lc_correspondence()
  correspondence$scales <- tibble::tibble(scale_key = c("aa", "bb"), n_items = 3L, n_weak = c(0L, 1L),
                                          n_crossloading = 0L)
  text <- describe_loading_clarity(correspondence, lc_labels, lc_cutoff)
  expect_match(text, "In the one-factor solutions of the scales analysed on their own, 1 of the 3 beta scale items loaded weakly",
               fixed = TRUE)
  correspondence$scales$n_weak <- 0L
  expect_match(describe_loading_clarity(correspondence, lc_labels, lc_cutoff),
               "In the one-factor solution of each of the two scales analysed on its own, no item loaded weakly",
               fixed = TRUE)
})
