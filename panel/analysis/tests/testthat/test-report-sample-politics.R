# Conditional political descriptions use the retained descriptive population;
# small known-answer inputs verify denominators, missing answers and histograms.
local({
  root <- normalizePath(getwd())
  while (!file.exists(file.path(root, "config", "analysis_plan.yaml"))) {
    parent <- dirname(root)
    if (identical(root, parent)) stop("analysis root not found")
    root <- parent
  }
  for (file in c("ap5_descriptives.R", "report_results_sample.R", "report_helpers.R")) {
    source(file.path(root, "R", file), local = FALSE)
  }
  assign("political_test_codebook", list(factors = utils::read.csv(
    file.path(root, "..", "preregistration", "codebook_factors.csv"),
    stringsAsFactors = FALSE)), envir = .GlobalEnv)
})

make_political_sample_fixture <- function() {
  data <- data.frame(respondent_id = 1:5, age = c(20, 30, 40, 50, 60),
    income = c(100, 200, 600, 800, 9999), pol_vote_would = c(1, 2, NA, 1, 1),
    pol_party_vote = c(1, NA, NA, NA, 801), pol_left_right = c(0, NA, 10, NA, 5))
  for (variable in c("pol_symp_spd", "pol_symp_cdu_csu", "pol_symp_greens",
                     "pol_symp_fdp", "pol_symp_afd", "pol_symp_linke")) {
    data[[variable]] <- c(-3, -1, 1, 3, 3)
  }
  list(data = data, descriptive = data[c(4, 1, 3, 2), c("respondent_id", "age", "income")],
       config = list(exclusions = list(min_age = 18, max_age = 69)))
}

describe_political_fixture <- function(fixture = make_political_sample_fixture()) {
  describe_sample_politics_for_report(fixture$data, fixture$descriptive,
                                     political_test_codebook, fixture$config)
}

test_that("political rows retain sample order and the full percentage denominator", {
  fixture <- make_political_sample_fixture()
  result <- describe_political_fixture(fixture)
  expect_identical(result$sample_size, 4L)
  expect_equal(result$scores$age, c(50, 20, 40, 30))
  expect_equal(result$scores$income, c(800, 100, 600, 200))
  vote <- subset(result$sample_rows, characteristic == "Voting intention")
  expect_identical(vote$group, c("Yes", "No", "Missing"))
  expect_equal(vote$n, c(2, 1, 1))
  expect_equal(vote$pct, c(50, 25, 25))
  party <- subset(result$sample_rows, characteristic == "Party choice")
  expect_false("Not asked" %in% party$group)
  expect_identical(party$group, c("CDU/CSU", "SPD", "FDP", "The Greens", "The Left", "AfD",
                                "Sahra Wagenknecht Alliance (BSW)", "Other party", "Invalid vote", "Missing"))
  expect_equal(party$n[c(1, 10)], c(1, 1))
  expect_equal(party$pct[c(1, 10)], c(25, 25))
  expect_equal(sum(party$pct), 50)
  expect_equal(result$party_question_counts$n, c(2, 2, 1))
  placement <- subset(result$numeric_rows, variable == "pol_left_right")
  expect_equal(placement$n, 2)
  expect_equal(placement$missing, 2)
  expect_equal(placement$mean, 5)
  expect_equal(placement$sd, sqrt(50))
  expect_identical(names(result$sample_rows), c("characteristic", "group", "n", "pct", "mean", "sd"))
  expect_equal(result$histogram_specs$bin_width, c(NA, NA, rep(1, 6), NA))
  expect_equal(result$histogram_specs$response_min, c(18, 100, rep(-3, 6), 0))
  expect_equal(result$histogram_specs$response_max, c(69, 800, rep(3, 6), 10))
  expect_identical(names(result$scores), result$histogram_specs$variable)
})

test_that("unavailable optional numeric answers remain numeric summaries", {
  fixture <- make_political_sample_fixture()
  fixture$data$pol_left_right <- NA_real_
  result <- describe_political_fixture(fixture)
  placement <- subset(result$sample_rows, characteristic == "Left–right placement")
  expect_equal(placement$n, 0)
  expect_true(is.na(placement$pct) && is.na(placement$mean) && is.na(placement$sd))
  expect_equal(subset(result$numeric_rows, variable == "pol_left_right")$missing, 4)
  plot <- rh_plot_histogram(result$scores$pol_left_right, c(0, 10))
  expect_identical(ggplot2::ggplot_build(plot)$data[[1]]$label, "No observed responses")
})

test_that("political descriptions reject different populations and invalid routing", {
  fixture <- make_political_sample_fixture()
  fixture$descriptive$respondent_id[1] <- 99L
  expect_error(describe_political_fixture(fixture), "absent from the prepared study")
  fixture <- make_political_sample_fixture()
  fixture$data$respondent_id[2] <- fixture$data$respondent_id[1]
  expect_error(describe_political_fixture(fixture), "unique, observed respondent ids")
  fixture <- make_political_sample_fixture()
  fixture$descriptive$income[1] <- 999
  expect_error(describe_political_fixture(fixture), "match the descriptive income")
  fixture <- make_political_sample_fixture()
  fixture$data$pol_vote_would[1] <- 99
  expect_error(describe_political_fixture(fixture), "outside their codebook")
  fixture <- make_political_sample_fixture()
  fixture$data$pol_party_vote[2] <- 1
  expect_error(describe_political_fixture(fixture), "was not asked")
})

test_that("seven narrow sympathy bars preserve exact counts and regular whitespace", {
  counts <- c(1, 4, 0, 6, 3, 0, 2)
  values <- rep(-3:3, counts)
  plot <- rh_plot_histogram(values, c(-3, 3), bin_width = 1)
  bars <- ggplot2::ggplot_build(plot)$data[[1]]
  expect_equal(plot$data$count, counts)
  expect_equal(bars$y, counts)
  expect_equal(bars$x, -3:3)
  expect_equal(diff(bars$x), rep(1, 6))
  expect_equal(bars$xmax - bars$xmin, rep(.4, 7))
  expect_true(all(bars$xmax - bars$xmin < 1))
  expect_equal(bars$xmin[-1] - bars$xmax[-7], rep(.6, 6))
  expect_true(all(is.na(bars$colour)))
  expect_equal(sum(bars$y), length(values))
  expect_equal(nrow(bars), 7L)
  expect_identical(unique(bars$fill), "grey35")
  expect_error(rh_plot_histogram(c(-3, .5), c(-3, 3), 1), "declared category intervals")
  expect_error(rh_plot_histogram(c(-4, 0), c(-3, 3), 1), "outside")
})

test_that("continuous histograms retain the existing scale-table construction", {
  plot <- rh_plot_histogram(c(1, 2, 3, 6), c(1, 6))
  bars <- ggplot2::ggplot_build(plot)$data[[1]]
  expect_equal(nrow(bars), 20L)
  expect_equal(sum(bars$count), 4)
  expect_identical(unique(bars$colour), "white")
  expect_equal(plot$theme$plot.margin, ggplot2::margin(1, 1, 1, 1))
})

test_that("selected histograms preserve categorical cells and the scale-table image size", {
  old_directory <- getwd()
  setwd(tempdir())
  on.exit(setwd(old_directory), add = TRUE)
  old_options <- options(sass.cache = FALSE)
  on.exit(options(old_options), add = TRUE)
  format <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  on.exit(knitr::opts_knit$set(rmarkdown.pandoc.to = format), add = TRUE)
  knitr::opts_knit$set(rmarkdown.pandoc.to = "html")
  tab <- rh_table(data.frame(label = c("category", "rating", "category", "placement"),
                            histogram = c("keep first", "", "keep third", "")), engine = "gt")
  scores <- data.frame(rating = c(-3, -3, 0, 3), placement = c(0, 4, 6, 10))
  shown <- rh_add_histograms(tab, scores, c("placement", "rating"), rows = c(4L, 2L),
    response_range = list(c(0, 10), c(-3, 3)), bin_width = c(NA, 1))
  html <- as.character(gt::as_raw_html(shown))
  expect_match(html, "keep first", fixed = TRUE)
  expect_match(html, "keep third", fixed = TRUE)
  document <- xml2::read_html(html)
  images <- xml2::xml_find_all(document, ".//img")
  expect_length(images, 2L)
  expect_identical(xml2::xml_attr(images, "style"), rep("height:30px;", 2))
  expect_true(all(is.na(xml2::xml_attr(images, "width"))))
  expect_false(grepl("object-fit", html, fixed = TRUE))
  expected_rating <- as.character(gt::ggplot_image(
    rh_plot_histogram(scores$rating, c(-3, 3), 1), height = 30, aspect_ratio = 3))
  expected_src <- xml2::xml_attr(xml2::xml_find_first(xml2::read_html(expected_rating), ".//img"), "src")
  expect_identical(xml2::xml_attr(images[[1]], "src"), expected_src)
  for (image in images) {
    encoded <- sub("^data:image/png;base64,", "", xml2::xml_attr(image, "src"))
    bytes <- base64enc::base64decode(encoded)
    expect_identical(bytes[1:8], as.raw(c(137, 80, 78, 71, 13, 10, 26, 10)))
    # PNG's IHDR stores width and height as big-endian four-byte integers.
    dimensions <- readBin(bytes[17:24], integer(), n = 2, size = 4, endian = "big")
    expect_equal(dimensions, c(1500L, 500L))
  }
  expect_error(rh_add_histograms(tab, scores, c("rating", "placement"), rows = c(2L, 2L)), "must match table rows")
  expect_error(rh_add_histograms(tab, scores, "rating", rows = 5L), "must match table rows")
  expect_error(rh_add_histograms(tab, scores, c("rating", "placement"), rows = c(2L, 4L),
                               response_range = list(c(-3, 3))), "ranges must match")
  knitr::opts_knit$set(rmarkdown.pandoc.to = "docx")
  expect_identical(rh_add_histograms(tab, scores, "rating", rows = 2L), tab)
})
