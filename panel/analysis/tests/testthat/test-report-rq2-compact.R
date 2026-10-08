# Compact RQ2 presentation preserves all comparisons; the text names every sign flip (AP7).
local({
  root <- normalizePath(getwd())
  while (!file.exists(file.path(root, "config", "analysis_plan.yaml"))) root <- dirname(root)
  source(file.path(root, "R", "config.R"), local = FALSE)
  source(file.path(root, "R", "report_helpers.R"), local = FALSE)
  source(file.path(root, "R", "report_joint_additions.R"), local = FALSE)
  assign("compact_rq2_root", root, envir = .GlobalEnv)
})

# The loaded plan: motives and facets in the order of codebook_scales.csv, and
# the report's labels, every scale's from the codebook.
compact_rq2_plan <- zm_config("smoke", file.path(compact_rq2_root, "config", "analysis_plan.yaml"))
compact_rq2_labels <- rh_labels(zm_codebook(compact_rq2_plan))
make_compact_rq2_fixture <- function() {
  pairs <- utils::combn(c("aggression", "submission", "conventionalism", "sdo_d"), 2)
  rows <- expand.grid(pair = seq_len(6), motive_key = compact_rq2_plan$regression$motives,
                      stringsAsFactors = FALSE)
  rows$first_outcome <- pairs[1, rows$pair]
  rows$second_outcome <- pairs[2, rows$pair]
  rows$posterior_median <- .2
  rows$lower <- .1
  rows$upper <- .3
  rows$direction_comparison <- "same direction"
  # Security: positive with aggression and conventionalism, negative with
  # submission and SDO-D, so four of its six pairs are sign flips
  rows$direction_comparison[c(1, 3, 4, 6)] <- "sign flip"
  rows$lower[2] <- -.1
  # a sign flip whose strength difference is unresolved
  rows$lower[3] <- 0
  # a sign flip with a negative credible strength difference
  rows$posterior_median[4] <- -.2
  rows$lower[4] <- -.3
  rows$upper[4] <- -.1
  direction <- data.frame(
    motive_key = rep(compact_rq2_plan$regression$motives, each = 4),
    outcome = rep(c("aggression", "submission", "conventionalism", "sdo_d"), 5),
    posterior_median = rep(c(.2, -.2, .2, -.2), 5), lower = rep(c(.1, -.3, .1, -.3), 5),
    upper = rep(c(.3, -.1, .3, -.1), 5))
  list(differences = rows, direction = direction, gate_passed = TRUE)
}

test_that("RQ2a shows all six comparisons and five motives, without a direction marker", {
  fixture <- make_compact_rq2_fixture()
  table <- build_joint_facet_strength_table(fixture, compact_rq2_labels, compact_rq2_plan, engine = "gt")
  data <- table[["_data"]]
  expect_identical(dim(data), c(6L, 6L))
  # the motive columns in the codebook's order
  expect_identical(names(data), c("comparison", "zm_security", "zm_arousal", "zm_power", "zm_prestige", "zm_achievement"))
  # the facet pairs in the codebook's order of the facets
  expect_identical(data$comparison, c("ASC aggression − ASC submission", "ASC aggression − Conventionalism",
                                      "ASC aggression − SDO-D", "ASC submission − Conventionalism",
                                      "ASC submission − SDO-D", "Conventionalism − SDO-D"))
  expect_identical(data$comparison[1], "ASC aggression − ASC submission")
  expect_identical(data$comparison[6], "Conventionalism − SDO-D")
  # the cells hold the strength differences only; the direction is in the text
  expect_false(any(grepl("<sup>", unlist(data), fixed = TRUE)))
  # coefficient differences keep their leading zero
  expect_match(data$zm_security[2], "[−0.10, 0.30]", fixed = TRUE)
  fixture$gate_passed <- FALSE
  withheld <- build_joint_facet_strength_table(fixture, compact_rq2_labels, compact_rq2_plan, engine = "gt")
  expect_false(any(grepl("<sup>", unlist(withheld[["_data"]]), fixed = TRUE)))
})

test_that("RQ2c preserves facet-minus-total order, without a direction marker", {
  rows <- expand.grid(outcome_key = c("asc_agg", "asc_sub", "asc_conv"),
                      predictor_key = compact_rq2_plan$regression$motives, stringsAsFactors = FALSE)
  rows$attenuation_median <- .1
  rows$attenuation_lower <- .02
  rows$attenuation_upper <- .2
  rows$direction_comparison <- "sign flip"
  rows$attenuation_lower[2] <- -.1
  fixture <- list(facet_comparisons = rows, validity = list(fit_valid = TRUE), interpretation_allowed = TRUE)
  table <- build_joint_total_strength_table(fixture, compact_rq2_labels, compact_rq2_plan, engine = "gt")
  expect_identical(dim(table[["_data"]]), c(3L, 6L))
  expect_identical(table[["_data"]]$comparison, c("ASC aggression − ASC total", "ASC submission − ASC total", "Conventionalism − ASC total"))
  expect_false(any(grepl("<sup>", unlist(table[["_data"]]), fixed = TRUE)))
  fixture$interpretation_allowed <- FALSE
  table <- build_joint_total_strength_table(fixture, compact_rq2_labels, compact_rq2_plan, engine = "gt")
  expect_false(any(grepl("<sup>", unlist(table[["_data"]]), fixed = TRUE)))
})

test_that("RQ2a narrative names every sign flip, whatever the strength difference", {
  fixture <- make_compact_rq2_fixture()
  text <- summarise_joint_facet_strength(fixture, compact_rq2_labels, compact_rq2_plan)
  expect_match(text, "Security was positively associated with ASC aggression and negatively with ASC submission (a sign flip).", fixed = TRUE)
  # the strength difference of this pair is unresolved
  expect_match(text, "Security was positively associated with ASC aggression and negatively with SDO-D (a sign flip).", fixed = TRUE)
  expect_match(text, "Security was negatively associated with ASC submission and positively with conventionalism (a sign flip).", fixed = TRUE)
  expect_false(grepl("ASC aggression and positively with conventionalism", text, fixed = TRUE))
})

test_that("the compact RQ2 tables and sentences take the motive and facet order from the codebook", {
  fixture <- make_compact_rq2_fixture()
  # whatever order the plan list or the rows bring, the codebook's order shows
  scrambled <- compact_rq2_plan
  scrambled$regression$motives <- rev(scrambled$regression$motives)
  scrambled$regression$outcomes <- rev(scrambled$regression$outcomes)
  expect_identical(order_joint_facet_keys(compact_rq2_plan),
                   c(aggression = "asc_agg", submission = "asc_sub", conventionalism = "asc_conv", sdo_d = "sdo_dom"))
  table <- build_joint_facet_strength_table(fixture, compact_rq2_labels, compact_rq2_plan, engine = "gt")
  heads <- vapply(table[["_boxhead"]]$column_label, as.character, "")
  expect_identical(heads[-1], c("Security", "Arousal", "Power", "Prestige", "Achievement"))
  # a plan whose lists run the other way puts the columns the other way: the
  # order is the plan's, which zm_config() takes from the codebook
  reversed <- build_joint_facet_strength_table(fixture, compact_rq2_labels, scrambled, engine = "gt")
  expect_identical(names(reversed[["_data"]])[-1], rev(names(table[["_data"]])[-1]))
  # the sentences name the motives in that order
  text <- summarise_joint_facet_strength(fixture, compact_rq2_labels, compact_rq2_plan)
  starts <- vapply(c("Security", "Arousal", "Power", "Prestige", "Achievement"),
                   function(label) regexpr(label, text, fixed = TRUE)[[1]], integer(1))
  expect_true(all(starts > 0))
  expect_false(is.unsorted(starts))
})
