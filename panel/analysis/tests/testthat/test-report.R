# LAYER 3 tests — R/report_helpers.R: the display helpers of the reports on
# hand-built fixtures, checked through their kable fallback engine. The
# manuscript Results draft is tested in test-report-draft.R.

local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) {
    parent <- dirname(dir)
    if (identical(parent, dir)) stop("project root not found from ", getwd())
    dir <- parent
  }
  for (f in c("config.R", "ap9_efa.R", "ap4_factor_structure.R", "ap9_pipeline.R", "report_supplement_measurement.R", "ap10_inference.R",
              "ap8_network_helpers.R", "ap8_network.R", "report_results_network.R", "report_helpers.R",
              "report_supplement_model_checks.R", "classify_importance_sampling.R")) {
    source(file.path(dir, "R", f), local = FALSE)
  }
  source(file.path(dir, "tests", "support", "likelihood-pairs.R"), local = FALSE)
  # the network display helpers are tested in test-report-network.R; only their
  # presence in the qmd is asserted here
  assign("report_test_root", dir, envir = .GlobalEnv)
})

root <- report_test_root
plan_path <- file.path(root, "config", "analysis_plan.yaml")
analysis_plan <- zm_config(profile = "smoke", path = plan_path)
codebook <- zm_codebook(analysis_plan)
# the report's labels: every scale from the codebook
labels <- rh_labels(codebook)
outcomes <- as.character(analysis_plan$regression$outcomes)

# ---- helpers for the tests --------------------------------------------------

# A kable pipe table as header and body rows; every row is a character vector
# of trimmed cells named by the header
kable_cells <- function(tab) {
  lines <- as.character(tab)
  lines <- lines[startsWith(lines, "|")]
  cells <- lapply(lines, function(l) trimws(strsplit(l, "|", fixed = TRUE)[[1]][-1]))
  header <- cells[[1]]
  rows <- lapply(cells[-(1:2)], function(r) stats::setNames(r, header))
  list(header = header, rows = rows)
}
kable_text <- function(tab) paste(as.character(tab), collapse = "\n")
kable_header <- function(tab) kable_cells(tab)$header
kable_nrow <- function(tab) length(kable_cells(tab)$rows)

# One cell of a row: the column whose header matches the regex `column`
kable_cell <- function(row, column) {
  idx <- grep(column, names(row))
  if (length(idx) != 1) stop("column '", column, "' matches ", length(idx), " headers")
  unname(row[[idx]])
}

# The body row(s) whose cells equal the values of `where` (names are header
# regexes, values the expected cell content)
kable_rows <- function(tab, where) {
  rows <- kable_cells(tab)$rows
  Filter(function(r) all(vapply(names(where), function(col) identical(kable_cell(r, col), unname(where[[col]])), logical(1))), rows)
}
kable_row <- function(tab, where) {
  hit <- kable_rows(tab, where)
  if (length(hit) != 1) stop(length(hit), " rows match ", paste(names(where), where, sep = " = ", collapse = ", "))
  hit[[1]]
}

# ---- the qmd ----------------------------------------------------------------

test_that("rh_source_note() names the target, the analysis plan, or nothing", {
  expect_null(rh_source_note(NULL, NULL))
  expect_equal(rh_source_note("report_network"),
               "Source: target `report_network`.")
  expect_equal(rh_source_note(c("ap6_tails", "ap6_pp_stats_tbl")),
               "Source: targets `ap6_tails`, `ap6_pp_stats_tbl`.")
  expect_equal(rh_source_note(NA), "Source: analysis plan.")
  expect_equal(rh_source_note("ap1_log", "Rule as implemented."),
               "Source: target `ap1_log`. Rule as implemented.")
  expect_equal(rh_source_note(NULL, "Rule as implemented."), "Rule as implemented.")
  # the note reaches the table through rh_table()
  tab <- rh_table(data.frame(a = 1), engine = "kable", target = "ap1_log")
  expect_match(kable_text(tab), "Source: target `ap1_log`.", fixed = TRUE)
  plan <- rh_table(data.frame(a = 1), engine = "kable", target = NA)
  expect_match(kable_text(plan), "Source: analysis plan.", fixed = TRUE)
})

# ---- display helpers on fixtures --------------------------------------------

test_that("the network tables show each edge of a block once, with present weights and bagged Bayes factors", {
  nodes <- c("zm_security", "zm_arousal", "zm_power")
  bagged <- tibble::tibble(node_i = c("zm_security", "zm_security", "zm_arousal"), node_j = c("zm_arousal", "zm_power", "zm_power"),
                           pip_bagged = c(1, 0.05, 0.6), bf_bagged = c(Inf, 0.0526, 1.5),
                           weight_bagged = c(0.214, -0.01, 0.05), decision = c("present", "absent", "inconclusive"))
  attr(bagged, "feasible") <- TRUE
  attr(bagged, "n_sweeps") <- 1500L
  attr(bagged, "n_success_fits") <- 60L
  attr(bagged, "g_prior") <- 0.5
  tab <- rh_network_matrix_table(bagged, rows = nodes[-1], columns = nodes[-3], labels = labels,
                                 row_title = "Motive", lower_triangle = TRUE, order = nodes,
                                 cell_marks = rh_network_pair_key("zm_power", "zm_arousal"), cell_mark = "a",
                                 cell_lines = function(row, column) if (row == "zm_power") "RQ1 +" else "",
                                 note = "Note text.", engine = "kable")
  expect_identical(kable_header(tab), c("Motive", rh_label(nodes[-3], labels)))
  first <- kable_cells(tab)$rows[[1]]
  expect_identical(unname(first[2]), "**present, .21**<br>BF<sub>bagged</sub> > 10,000")
  expect_identical(unname(first[3]), "")
  second <- kable_cells(tab)$rows[[2]]
  expect_identical(unname(second[2]), "absent<br>BF<sub>bagged</sub> 0.05<br>RQ1 +")
  expect_identical(unname(second[3]), "inconclusive<sup>a</sup><br>BF<sub>bagged</sub> 1.50<br>RQ1 +")
  expect_match(kable_text(tab), "Note text.", fixed = TRUE)
  # an infeasible bag classifies nothing and shows no number
  attr(bagged, "feasible") <- FALSE
  none <- rh_network_matrix_table(bagged, rows = nodes[-1], columns = nodes[-3], labels = labels,
                                  lower_triangle = TRUE, order = nodes, engine = "kable")
  expect_identical(unname(kable_cells(none)$rows[[1]][2]), "not classified<br>BF<sub>bagged</sub> —")
})

test_that("priorsense display exposes both perturbation axes, the block per row and the labelled all-priors run", {
  ps <- tibble::tibble(
    outcome = "asc_agg", block = "metric_slopes", term = "zm_power", term_type = "predictor",
    slope_sd = 0.2, prior_selection = "slopes",
    prior_sens = 0.12, lik_sens = 0.34, diagnosis = "ok",
    pareto_k_max = 0.3, settings_status = "explicit"
  )
  tab <- kable_text(rh_priorsense_table(ps, result_file = "prior_power_scaling.csv", engine = "kable"))
  expect_match(tab, "Prior sensitivity", fixed = TRUE)
  expect_match(tab, "Likelihood sensitivity", fixed = TRUE)
  expect_match(tab, "Priors scaled", fixed = TRUE)
  expect_match(tab, "slopes", fixed = TRUE)
  # one row per block x quantity: the block column, Pareto k, the all-priors label, R2 as a quantity
  blocks <- tibble::tibble(
    outcome = "asc_agg",
    block = c("metric_slopes", "gender_contrasts", "intercept", "sigma", "all_priors", "all_priors"),
    term = c("zm_power", "zm_power", "zm_power", "zm_power", "zm_power", "bayes_R2"), term_type = "predictor",
    slope_sd = 0.2, prior_selection = c("metric_slopes", "gender_contrasts", "intercept", "sigma", "all", "all"),
    prior_sens = c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06), lik_sens = 0.1, diagnosis = "-",
    pareto_k_max = c(0.3, 0.4, NA, 0.2, 0.5, 0.5), settings_status = c(rep("explicit", 5), "error_no_fallback")
  )
  tb <- rh_priorsense_table(blocks, labels, "prior_power_scaling.csv", engine = "kable")
  header <- kable_header(tb)
  for (h in c("Prior block scaled", "Quantity", "Pareto k (max)", "Settings status")) expect_true(h %in% header, info = h)
  expect_equal(kable_nrow(tb), 6)
  all_rows <- kable_rows(tb, c("^Prior block scaled$" = "all priors (labelled all-priors run)"))
  expect_length(all_rows, 2)
  expect_true(any(vapply(all_rows, function(r) kable_cell(r, "^Quantity$") == rh_label("bayes_R2", labels), logical(1))))
  expect_equal(kable_cell(kable_row(tb, c("^Prior block scaled$" = "intercept")), "^Pareto k \\(max\\)$"), "—")
  expect_match(kable_text(tb), "never replaced by another block", fixed = TRUE)
})

test_that("rh_gender_coding_text() states the gender levels as used, their counts and the reference group", {
  # The Methods list of what this run settles carries the gender coding.
  gender <- factor(c("female", "female", "male"), levels = c("female", "male"))
  text <- rh_gender_coding_text(gender)
  expect_match(text, "with the levels female (2 respondents) and male (1 respondent)", fixed = TRUE)
  expect_match(text, "the reference group is female, the largest group (AP3, AP6).", fixed = TRUE)
  three <- factor(c("male", "female", "diverse"), levels = c("male", "female", "diverse"))
  expect_match(rh_gender_coding_text(three),
               "male (1 respondent), female (1 respondent) and diverse (1 respondent); the reference group is male",
               fixed = TRUE)
})

test_that("rh_nu_fixed_text() states the fixed nu of the Student-t refit and its scale prior", {
  st <- analysis_plan$sensitivity$student_t
  expect_equal(rh_nu_fixed_text(st), paste0("ν fixed at ", rh_fmt_n(st$nu_fixed), " (constant, no tail parameter); scale σ ~ half-normal(0, ", rh_fmt(st$sigma_scale_prior_sd), ")"))
  expect_match(rh_nu_fixed_text(st, nu_fixed = c(4, 4, NA)), "^ν fixed at 4 ")
})

# ---- the fill: the two records and the section that renders them -------------

# The shape of the `filled_cells` target (ap3_filled_cells()): one row per cell
# the fill wrote, one item cell and one demographic cell.
planted_filled_cells <- tibble::tibble(
  respondent_id = c(41L, 87L),
  variable = c("zm_power_3", "demo_hh_members"),
  kind = c("item", "demographic"),
  model = c("zm_power", "demo_hh_members"),
  value = c(4.28, 3),
  lower = c(2, 2),
  upper = c(6, 5),
  n_fit_rows = c(4194L, 698L)
)
# The shape of the `dropped_respondents` target (ap3_dropped_respondents()).
planted_dropped <- tibble::tibble(
  respondent_id = c(12L, 55L),
  reason = c("more than 2 missing items in asc_conv", "2 of 3 demographics missing")
)

test_that("rh_item_distribution_display() shows one group of scales, marks items with missing answers and names them in the note", {
  codebook_fixture <- list(scales = tibble::tibble(scale_key = c("zm_power", "zm_prestige"),
                                                   item_codes = list(c("dom_1", "dom_2"), "prest_1")))
  dist <- tibble::tibble(
    Item = c("dom_1", "dom_2", "prest_1"), N = c(700L, 698L, 700L), Missing = c(0L, 2L, 0L),
    `Response 1` = c("3 (0.4%)", "0 (0.0%)", "1 (0.1%)"), `Response 2` = c("697 (99.6%)", "698 (100.0%)", "699 (99.9%)")
  )
  tab <- rh_item_distribution_display(dist, codebook_fixture, "zm_power", labels, engine = "kable")
  expect_identical(kable_header(tab), c("Scale", "Item", "Response 1", "Response 2"))
  expect_equal(kable_nrow(tab), 2L)
  expect_equal(kable_cell(kable_cells(tab)$rows[[1]], "^Scale$"), rh_label("zm_power", labels))
  expect_equal(kable_cell(kable_cells(tab)$rows[[2]], "^Item$"), "dom_2<sup>a</sup>")
  # an option nobody chose keeps its zero count
  expect_equal(kable_cell(kable_cells(tab)$rows[[2]], "^Response 1$"), "0 (0.0%)")
  note <- kable_text(tab)
  expect_match(note, "options nobody chose are kept with a count of zero", fixed = TRUE)
  expect_match(note, "Each item was presented to 700 respondents.", fixed = TRUE)
  expect_match(note, "<sup>a</sup> Item with missing answers, filled before scoring: dom_2 (2 missing).", fixed = TRUE)
  complete <- rh_item_distribution_display(dist, codebook_fixture, "zm_prestige", labels, engine = "kable")
  expect_match(kable_text(complete), "Each item was presented to 700 respondents, all of whom answered it.", fixed = TRUE)
})

test_that("rh_filled_cells_table() shows every filled cell with its variable, model, value and interval", {
  tab <- rh_filled_cells_table(planted_filled_cells, analysis_plan = analysis_plan, labels = labels, engine = "kable")
  # the value and its interval share one cell
  expect_equal(
    kable_header(tab),
    c("Respondent", "Variable", "Filled by", "Filled value [95% predictive interval]")
  )
  # an item cell names its item code and the item model of its scale; the item
  # value keeps two decimals, its interval runs over whole answers
  item <- kable_row(tab, c("^Variable$" = "zm_power_3"))
  expect_equal(kable_cell(item, "^Respondent$"), "41")
  expect_equal(kable_cell(item, "^Filled by$"), paste(rh_label("zm_power", labels), "item model"))
  expect_equal(kable_cell(item, "^Filled value"), paste(rh_fmt(4.28), rh_fmt_ci(2, 6, 0)))
  # a demographic cell names the variable and the regression that filled it
  demo <- kable_row(tab, c("^Variable$" = "Household members"))
  expect_equal(kable_cell(demo, "^Filled by$"), "Household-size regression")
  expect_equal(kable_cell(demo, "^Filled value"), paste(rh_fmt(3, 0), rh_fmt_ci(2, 5, 0)))
  # the value is displayed, never recomputed from the interval
  expect_match(kable_text(tab), "posterior median of the expected answer, unrounded", fixed = TRUE)
  expect_match(kable_text(tab), "for income the median posterior predictive band", fixed = TRUE)
})

test_that("rh_filled_cells_table() says that no cell was filled instead of showing an empty table", {
  empty <- planted_filled_cells[0, ]
  tab <- rh_filled_cells_table(empty, analysis_plan = analysis_plan, labels = labels, engine = "kable")
  expect_equal(kable_nrow(tab), 1L)
  expect_match(kable_text(tab), "No cell was filled.", fixed = TRUE)
})

test_that("sweep labels come from analysis_plan$priors$sweep_labels", {
  for (sd in analysis_plan$priors$slope_sd_sweep) {
    key <- format(sd, trim = TRUE, drop0trailing = TRUE, scientific = FALSE)
    expect_equal(rh_sweep_label(sd, analysis_plan), analysis_plan$priors$sweep_labels[[key]])
    expect_equal(rh_sweep_sd_label(sd, analysis_plan), paste0(rh_fmt(sd), " (", analysis_plan$priors$sweep_labels[[key]], ")"))
  }
  expect_equal(rh_sweep_label(analysis_plan$priors$slope_sd_primary, analysis_plan), "primary")
  expect_true(is.na(rh_sweep_label(2, analysis_plan)))
  expect_equal(rh_sweep_sd_label(2, analysis_plan), rh_fmt(2))
  expect_equal(rh_sweep_sd_label(0.2, NULL), rh_fmt(0.2))
  expect_equal(rh_sweep_sd_label(NA_real_, analysis_plan), "—")
})

test_that("rh_reliability_table() lists only the scales with a fallback or lost resamples, against the configured resamples", {
  configured_n <- analysis_plan$reliability$bootstrap_n
  rel <- tibble::tibble(
    scale_key = c("zm_power", "sdo_dom", "zm_prestige"), label = c("Dominance", "SDO-D", "Prestige"),
    n_items = c(6L, 8L, 6L), n = 700L,
    omega_t = c(0.81, NA, 0.84), omega_lo = c(0.77, NA, 0.80), omega_hi = c(0.85, NA, 0.88),
    alpha = c(0.79, 0.74, 0.83), alpha_lo = c(0.75, 0.70, 0.79), alpha_hi = c(0.83, 0.78, 0.87),
    method = c("omega", "alpha", "omega"), n_boot = configured_n,
    n_boot_omega = c(12L, 0L, configured_n), n_boot_alpha = configured_n,
    interval_source = c("omega_bootstrap", "alpha_fallback", "omega_bootstrap"),
    note = c(NA_character_, "psych::omega failed: the factor model did not fit", NA_character_)
  )
  dom <- rh_label("zm_power", labels)
  sdo <- rh_label("sdo_dom", labels)
  # the note of a scale is a superscript letter and a sentence of the table note
  sdo_shown <- paste0(sdo, "<sup>a</sup>")
  # the scale with every resample and the bootstrap interval of omega is not
  # listed: a table only where values differ
  expect_identical(rh_reliability_detail_rows(rel, analysis_plan), c(TRUE, TRUE, FALSE))
  tab <- rh_reliability_table(rel, labels, engine = "kable", analysis_plan = analysis_plan)
  expect_equal(kable_nrow(tab), 2L)
  expect_identical(vapply(kable_cells(tab)$rows, function(r) kable_cell(r, "^Scale$"), ""), c(dom, sdo_shown))
  expect_true(all(c("ω [95% CI]", "α [95% CI]", "Reported coefficient", "Interval",
                    "Successful ω resamples", "Successful α resamples") %in% kable_header(tab)))
  configured <- rh_fmt_n(configured_n)
  dom_row <- kable_row(tab, c("^Scale$" = dom))
  sdo_row <- kable_row(tab, c("^Scale$" = sdo_shown))
  expect_equal(kable_cell(dom_row, "^Successful ω"), paste("12 of", configured))
  expect_equal(kable_cell(dom_row, "^Successful α"), paste(configured, "of", configured))
  expect_equal(kable_cell(sdo_row, "^Successful ω"), "not attempted")
  expect_equal(kable_cell(sdo_row, "^Successful α"), paste(configured, "of", configured))
  expect_equal(kable_cell(dom_row, "^Reported coefficient$"), "ω total")
  expect_equal(kable_cell(sdo_row, "^Reported coefficient$"), "α (ω model did not fit)")
  # each coefficient and its interval share one cell
  expect_equal(kable_cell(sdo_row, "^ω \\["), "—")
  expect_equal(kable_cell(dom_row, "^ω \\["), rh_fmt_est_ci(0.81, 0.77, 0.85, 2, bounded = TRUE))
  expect_equal(kable_cell(dom_row, "^α \\["), rh_fmt_est_ci(0.79, 0.75, 0.83, 2, bounded = TRUE))
  expect_false("Note" %in% kable_header(tab))
  expect_match(kable_text(tab), "<sup>a</sup> psych::omega failed: the factor model did not fit.", fixed = TRUE)
  # The omega interval is unavailable below the bootstrap success threshold.
  expect_equal(kable_cell(dom_row, "^Interval$"), "bootstrap of ω")
  with_source <- rel
  with_source$interval_source <- c("unavailable_omega_bootstrap", "alpha_fallback", "omega_bootstrap")
  with_source$omega_lo[1] <- with_source$omega_hi[1] <- NA_real_
  ts <- rh_reliability_table(with_source, labels, engine = "kable", analysis_plan = analysis_plan)
  expect_equal(kable_cell(kable_row(ts, c("^Scale$" = dom)), "^Interval$"), "ω interval unavailable")
  expect_equal(kable_cell(kable_row(ts, c("^Scale$" = dom)), "^ω \\["), rh_fmt(0.81, 2, bounded = TRUE))
  expect_equal(kable_cell(kable_row(ts, c("^Scale$" = sdo_shown)), "^Interval$"), "α interval (ω model did not fit)")
  expect_match(kable_text(ts), rh_fmt_pct(analysis_plan$reliability$omega_interval_min_success, 0, scale = "proportion"), fixed = TRUE)
  expect_match(kable_text(ts), "otherwise ω is shown without an interval", fixed = TRUE)
  # identical notes share their letter
  same_note <- rel
  same_note$note <- c("lost resamples", "lost resamples", NA_character_)
  shared <- rh_reliability_table(same_note, labels, engine = "kable", analysis_plan = analysis_plan)
  expect_identical(vapply(kable_cells(shared)$rows, function(r) kable_cell(r, "^Scale$"), ""),
                   paste0(c(dom, sdo), "<sup>a</sup>"))
  expect_false(grepl("<sup>b</sup>", kable_text(shared), fixed = TRUE))
  # the prose names the listed scales and states the standard case of the others
  text <- rh_reliability_detail_text(rel, analysis_plan, labels, table_ref = "Table S1")
  expect_match(text, paste0("For one of the three scales, ω total is reported with the percentile bootstrap interval of ω, and all ",
                            configured, " bootstrap resamples of ω and of α succeeded."), fixed = TRUE)
  # the scales named inside the sentence, in their inline form
  expect_match(text, paste0("Table S1 lists the two scales whose reliability needed the fallback or lost resamples: ",
                            rh_lowercase_first_letter(dom), ", ", rh_lowercase_first_letter(sdo), "."), fixed = TRUE)
  # every scale standard: no table, one sentence
  standard <- rel[3, ]
  expect_null(rh_reliability_table(standard, labels, engine = "kable", analysis_plan = analysis_plan))
  expect_equal(rh_reliability_detail_text(standard, analysis_plan, labels, table_ref = "Table S1"),
               paste0("For the one scale, ω total is reported with the percentile bootstrap interval of ω, and all ",
                      configured, " bootstrap resamples of ω and of α succeeded."))
})

# ---- AP4/AP9 factor-analytic sets (analysis_plan$factor_analysis) ---------------------

# One EFA set result in the shape of ap4_efa_set(): two planted blocks, the
# last item of the first block cross-loading on the second factor.
efa_set_fixture <- function() {
  items <- c("dom_1", "dom_2", "dom_3", "prest_1", "prest_2", "prest_3")
  L <- matrix(
    c(0.72, 0.10,
      0.68, 0.05,
      0.55, 0.46,
      0.02, 0.71,
      0.08, 0.66,
      -0.11, 0.74),
    nrow = 6, byrow = TRUE,
    dimnames = list(items, c("F1", "F2"))
  )
  list(
    key = "dopl", label = "DoPL-6 (dominance and prestige)", n_items = 6L,
    items = tibble::tibble(item = items, intended_scale = rep(c("zm_power", "zm_prestige"), each = 3)),
    indices = tibble::tibble(
      index = c("parallel_analysis", "comparison_data", "map", "vss"),
      n_factors = c(2L, 3L, 2L, NA_integer_),
      note = c(NA_character_, NA_character_, NA_character_, "vss failed: singular matrix")
    ),
    n_factors_used = 2L, rotation = "oblimin", loadings = L,
    phi = matrix(c(1, 0.31, 0.31, 1), nrow = 2),
    variance = tibble::tibble(
      factor = c("F1", "F2"),
      ss_loadings = c(1.44, 1.62),
      proportion_var = c(0.24, 0.27),
      cumulative_var = c(0.24, 0.51)
    ),
    error = NA_character_
  )
}

test_that("every factor-count criterion the pipeline emits has a display label", {
  # The accepted route emits one row per method of name_efa_indices();
  # a criterion without a label here renders as an "NA" column header in
  # tbl-efa and as an em-dash row in every *-efa-counts-* table.
  emitted <- unname(name_efa_indices(analysis_plan))
  expect_true("common_factor_parallel" %in% emitted)
  for (idx in emitted) {
    label <- rh_factor_index_label(idx)
    expect_false(is.na(label), info = idx)
    expect_true(nzchar(label), info = idx)
    expect_false(identical(label, idx), info = idx)
    expect_false(identical(label, "n/a"), info = idx)
  }
  # the display labels are the plan's own labels, criterion for criterion, so
  # the report cannot print a heading the plan does not state
  for (entry in analysis_plan$factor_analysis$factor_number_criteria) {
    expect_identical(rh_factor_index_label(as.character(entry$index)),
                     as.character(entry$label), info = as.character(entry$key))
  }
  # the two parallel analyses are told apart
  expect_false(identical(rh_factor_index_label("parallel_analysis"),
                         rh_factor_index_label("common_factor_parallel")))
  # and the criteria table carries the label of every criterion
  set <- efa_set_fixture()
  set$indices <- tibble::tibble(
    index = emitted, n_factors = rep(1L, length(emitted)), note = NA_character_
  )
  header <- kable_header(rh_factor_number_criteria_table(
    tabulate_factor_number_criteria(list(), list(dopl = set), analysis_plan), engine = "kable"))
  for (idx in emitted) expect_true(rh_factor_index_label(idx) %in% header, info = idx)
})

test_that("the criteria table gives every scale and set its suggested counts and failure notes", {
  set <- efa_set_fixture()
  scale <- list(key = "zm_arousal", label = "Social neophilia", n_items = 6L, n_factors_used = 1L,
                indices = tibble::tibble(index = "parallel_analysis", n_factors = 1L, note = NA_character_))
  criteria <- tabulate_factor_number_criteria(list(zm_arousal = scale), list(dopl = set), analysis_plan)
  expect_identical(criteria$label, c("Social neophilia", "DoPL-6 (dominance and prestige)"))
  expect_identical(criteria$expected, c(1L, 2L))
  expect_identical(criteria$comparison_data, c(NA_integer_, 3L))
  expect_identical(criteria$note, c(NA_character_, "vss failed: singular matrix"))
  shown <- rh_factor_number_criteria_table(criteria, labels, engine = "kable")
  row <- kable_row(shown, c("^Scale or item set$" = "DoPL-6 (dominance and prestige)"))
  expect_equal(kable_cell(row, "^Comparison data$"), "3")
  expect_equal(kable_cell(row, "^Failed criteria$"), "vss failed: singular matrix")
  expect_match(kable_text(shown), "Expected: number of factors in the intended scale structure", fixed = TRUE)
  # a scale on which every criterion suggests the expected count leaves the
  # table for the sentence
  indices <- setdiff(names(criteria), c("key", "label", "n_items", "expected", "note"))
  agreeing <- criteria
  agreeing[1, indices] <- as.list(rep(1L, length(indices)))
  expect_identical(rh_factor_number_criteria_differs(agreeing), c(FALSE, TRUE))
  kept <- rh_factor_number_criteria_table(agreeing, labels, engine = "kable")
  expect_equal(kable_nrow(kept), 1L)
  # the report's labels tell a scale from an item set
  text <- rh_factor_number_criteria_text(agreeing, labels, table_ref = "Table S2")
  expect_match(text, paste0("All ", rh_fmt_count(length(indices)), " criteria suggested the expected number of factors ",
                            "for the one scale analysed on its own."), fixed = TRUE)
  expect_match(text, "Table S2 gives the number each criterion suggested for the scale or item set on which they disagreed.",
               fixed = TRUE)
  agreeing$note <- NA_character_
  agreeing[2, indices] <- as.list(rep(2L, length(indices)))
  expect_null(rh_factor_number_criteria_table(agreeing, engine = "kable"))
})

test_that("the variance table gives every examined solution, the single scales first, then each set expected count first", {
  variance <- tibble::tibble(
    set_name = c("dopl", "dopl", "dopl", "zm_power"),
    factors = c(1L, 2L, 2L, 1L),
    rotation = c("none", "oblimin", "oblimin", "none"),
    factor = c("WLS1", "WLS2", "WLS1", "WLS1"),
    proportion_var = c(0.30, 0.22, 0.25, 0.4), cumulative_var = c(0.30, 0.47, 0.25, 0.4),
    note = NA_character_)
  plan <- analysis_plan
  plan$factor_analysis$sets <- list(list(key = "dopl", label = "DoPL", scales = c("zm_power", "zm_prestige"),
                                         expected_factors = 2L, efa = TRUE, cfa = FALSE))
  out <- tabulate_efa_set_variance(variance, plan)
  # the single scale zm_power, read from the same stored table (AP9)
  expect_identical(out$scale_key, c("zm_power", NA, NA, NA))
  expect_identical(out$factors, c(1L, 2L, 2L, 1L))
  expect_identical(out$factor, c("WLS1", "WLS1", "WLS2", "WLS1"))
  expect_identical(out$solution, c("expected count", "expected count", "expected count", "suggested by the criteria"))
  shown <- rh_efa_set_variance_table(out, sets = plan$factor_analysis$sets, labels = labels, engine = "kable")
  expect_identical(kable_header(shown), c("Scale or item set", "Solution", "Proportion of variance by factor", "Total"))
  rows <- kable_cells(shown)$rows
  expect_identical(kable_cell(rows[[1]], "^Scale or item set$"), rh_label("zm_power", labels))
  expect_identical(kable_cell(rows[[1]], "^Solution$"), "1 factor (one per scale)")
  expect_identical(kable_cell(rows[[1]], "^Total$"), rh_fmt(0.4, 2, bounded = TRUE))
  # the expected two-factor solution: one cell with each factor's share, the
  # factors numbered where no membership names their scales
  expect_identical(kable_cell(rows[[2]], "^Solution$"), "2 factors (one per scale)")
  expect_identical(kable_cell(rows[[2]], "^Proportion of variance by factor$"),
                   paste0("Factor 1 ", rh_fmt(0.25, 2, bounded = TRUE), "; Factor 2 ", rh_fmt(0.22, 2, bounded = TRUE)))
  expect_identical(kable_cell(rows[[2]], "^Total$"), rh_fmt(0.47, 2, bounded = TRUE))
  expect_identical(kable_cell(rows[[3]], "^Solution$"), "1 factor (suggested by the criteria)")
  expect_identical(kable_cell(rows[[3]], "^Proportion of variance by factor$"), "—")
})

test_that("the off-scale table lists the items that leave the factor most of their scale shares", {
  membership <- tibble::tibble(item = c("a1", "a2", "a3", "b1", "b2"),
                               intended_scale = c("zm_power", "zm_power", "zm_power", "zm_prestige", "zm_prestige"),
                               assigned_factor = c("WLS1", "WLS1", "WLS2", "WLS2", "WLS2"))
  correspondence <- list(
    overview = tibble::tibble(set_key = c("dopl", "dopl"), set_label = "DoPL", factors = c(2L, 3L),
                              solution = c("expected count", "suggested by the criteria")),
    membership = list(`dopl:2` = membership, `dopl:3` = membership))
  out <- tabulate_efa_items_off_scale(correspondence)
  expect_identical(out$item, "a3")
  expect_identical(out$factors, 3L)
  expect_identical(out$scale_factor, "WLS1")
  shown <- rh_efa_items_off_scale_table(out, labels, "efa_membership.csv", engine = "kable")
  expect_match(kable_text(shown), "efa_membership.csv", fixed = TRUE)
  expect_match(rh_efa_items_off_scale_text(out, "efa_membership.csv", table_ref = "Table A4"),
               paste("Table A4 lists the item assigned to a different factor than most items of its scale",
                     "in the solutions the criteria suggested beside the expected one."), fixed = TRUE)
  # no such item: no table, one sentence naming the complete data file
  expect_null(rh_efa_items_off_scale_table(out[0, ], labels, "efa_membership.csv", engine = "kable"))
  none <- rh_efa_items_off_scale_text(out[0, ], "efa_membership.csv", table_ref = "Table A4")
  expect_match(none, "no item was assigned to a different factor than most items of its scale", fixed = TRUE)
  expect_match(none, "efa_membership.csv", fixed = TRUE)
})

test_that("rh_efa_correspondence_overview_table() lists the solutions whose grouping differs; constant measures become the sentence", {
  cutoff <- analysis_plan$factor_analysis$loading_display_cutoff
  overview <- tibble::tibble(
    set_key = "motives", set_label = "All five motives", factors = c(5L, 1L, 2L),
    solution = c("expected count", "suggested by the criteria", "suggested by the criteria"),
    adjusted_rand = c(1, 0, 0.4), expected_pair_retention = c(1, 1, 1), empirical_pair_purity = c(1, 0.172414, 0.5),
    n_items = 30L, n_weak = c(0L, 16L, 3L), n_crossloading = c(0L, 0L, 0L), note = NA_character_
  )
  tab <- rh_efa_correspondence_overview_table(overview, cutoff, engine = "kable")
  # the perfect expected solution leaves the table; retention and cross-loading
  # hold one value in both shown rows and leave it too
  expect_equal(kable_nrow(tab), 2L)
  expect_equal(kable_header(tab), c("Item set", "Solution", "Adjusted Rand index", "Empirical-pair purity",
                                    "Weakly loading items"))
  one <- kable_row(tab, c("^Solution$" = "1 factor (suggested by the criteria)"))
  expect_equal(kable_cell(one, "^Adjusted Rand index$"), rh_fmt(0, 2, bounded = TRUE))
  expect_equal(kable_cell(one, "^Empirical-pair purity$"), rh_fmt(0.172414, 2, bounded = TRUE))
  expect_equal(kable_cell(one, "^Weakly loading items$"), "16 of 30")
  expect_match(kable_text(tab), "below 1, scales were merged", fixed = TRUE)
  text <- rh_efa_correspondence_text(overview, table_ref = "Table A8")
  expect_match(text, "In the expected solution of All five motives, every factor held the items of exactly one scale",
               fixed = TRUE)
  expect_match(text, paste("Table A8 gives the two solutions whose grouping differed; none of them split a scale",
                           "(expected-pair retention 1.00) and no item cross-loaded in them."), fixed = TRUE)
  # a measure that leaves the table is the one the sentence states
  same_purity <- overview
  same_purity$empirical_pair_purity <- c(1, 0.5, 0.5)
  expect_false("Empirical-pair purity" %in% kable_header(rh_efa_correspondence_overview_table(same_purity, cutoff, engine = "kable")))
  expect_match(rh_efa_correspondence_text(same_purity, table_ref = "Table A8"), "empirical-pair purity was .50 in each",
               fixed = TRUE)
  # one differing solution keeps every measure in the table
  single <- overview[1:2, ]
  expect_true(all(c("Expected-pair retention", "Cross-loading items") %in%
                    kable_header(rh_efa_correspondence_overview_table(single, cutoff, engine = "kable"))))
  expect_match(rh_efa_correspondence_text(single, table_ref = "Table A8"), "Table A8 gives the solution whose grouping differed.",
               fixed = TRUE)
  # every solution corresponds perfectly: no table
  expect_null(rh_efa_correspondence_overview_table(overview[1, ], cutoff, engine = "kable"))
})

test_that("rh_efa_membership_table() shows each item's two largest loadings and marks its loading clarity", {
  cutoff <- analysis_plan$factor_analysis$loading_display_cutoff
  membership <- list(`dopl:2` = tibble::tibble(
    item = c("dom_1", "dom_3", "prest_1", "prest_2", "prest_3"),
    intended_scale = c("zm_power", "zm_power", "zm_prestige", "zm_prestige", "zm_prestige"),
    assigned_factor = c("F1", "F1", "F2", "F2", "F2"),
    assigned_scales = c("zm_power", "zm_power", "zm_prestige", "zm_prestige", "zm_prestige"),
    largest_abs_loading = c(0.72, 0.55, 0.30, 0.66, 0.60), next_largest_abs_loading = c(0.10, 0.46, 0.02, 0.05, 0.04),
    weak = c(FALSE, FALSE, TRUE, FALSE, FALSE), crossloading = c(FALSE, TRUE, FALSE, FALSE, FALSE),
    crossloading_scales = c(NA, "zm_prestige", NA, NA, NA), crossloading_factors = c(NA, "F2", NA, NA, NA)
  ))
  sets <- list(list(key = "dopl", label = "DoPL-6 (dominance and prestige)", expected_factors = 2L))
  tab <- rh_efa_membership_table(membership, sets, labels, cutoff, engine = "kable")
  expect_equal(kable_header(tab), c("Scale", "Item", "DoPL-6, 2 factors: largest loading",
                                    "DoPL-6, 2 factors: next-largest loading"))
  row <- kable_row(tab, c("^Item$" = "dom_3"))
  # the scale stands once, on its first item
  expect_equal(kable_cell(kable_row(tab, c("^Item$" = "dom_1")), "^Scale$"), rh_label("zm_power", labels))
  expect_equal(kable_cell(row, "^Scale$"), "")
  expect_equal(kable_cell(row, ": largest loading$"), paste0(rh_fmt(0.55, 2, bounded = TRUE), "<sup>a</sup>"))
  expect_equal(kable_cell(row, "next-largest loading$"), rh_fmt(0.46, 2, bounded = TRUE))
  expect_equal(kable_cell(kable_row(tab, c("^Item$" = "prest_1")), ": largest loading$"),
               paste0(rh_fmt(0.30, 2, bounded = TRUE), "<sup>b</sup>"))
  expect_equal(kable_cell(kable_row(tab, c("^Item$" = "dom_1")), ": largest loading$"), rh_fmt(0.72, 2, bounded = TRUE))
  # the cross-loading item's note names its secondary scale
  expect_match(kable_text(tab), paste0("dom_3 in DoPL-6, 2 factors, its second loading lies on the factor of ",
                                       rh_label_inline("zm_prestige", labels), "."), fixed = TRUE)
  expect_match(kable_text(tab), "Every item had its largest loading on the factor of its own scale.", fixed = TRUE)
  # an item on the factor of another scale is marked and named
  moved <- membership
  moved$`dopl:2`$assigned_factor[4] <- "F1"
  moved$`dopl:2`$assigned_scales[4] <- "zm_power"
  moved_tab <- rh_efa_membership_table(moved, sets, labels, cutoff, engine = "kable")
  expect_match(kable_cell(kable_row(moved_tab, c("^Item$" = "prest_2")), ": largest loading$"), "<sup>c</sup>", fixed = TRUE)
  expect_match(kable_text(moved_tab), paste0("prest_2 in DoPL-6, 2 factors, on the factor of ", rh_label_inline("zm_power", labels)),
               fixed = TRUE)
  # no solution available: no table
  expect_null(rh_efa_membership_table(list(), sets, labels, cutoff, engine = "kable"))
})

test_that("rh_cfa_fit_table() shows every confirmatory model in three blocks; constant columns become the sentence", {
  fit_row <- function(model, label, n_factors, n_items, note = NA_character_) {
    tibble::tibble(
      model = model, set = NA_character_, label = label, n_items = n_items, n_factors = n_factors, n = 700L,
      chisq = 512.3, df = 395, pvalue = 0.0004, cfi = 0.972, tli = 1.004,
      rmsea = 0.021, rmsea_lo = 0.015, rmsea_hi = 0.027, srmr = 0.048,
      index_version = "robust (Brosseau-Liard & Savalei correction) for CFI, TLI, RMSEA; scaled chi-square",
      converged = TRUE, admissible = TRUE, error = NA_character_, note = note
    )
  }
  scales <- dplyr::bind_rows(fit_row("zm_power", "Dominance", 1L, 6L), fit_row("zm_prestige", "Prestige", 1L, 6L))
  structure <- fit_row("dopl", "DoPL-6 subscales", 2L, 12L)
  combined <- fit_row("motives", "All social motive scales", 5L, 30L, note = paste(
    "lavaan->lav_model_vcov(): The variance-covariance matrix of the estimated parameters (vcov)",
    "does not appear to be positive definite!"))
  tab <- rh_cfa_fit_table(scales, structure, combined, labels, messages_link = "Section S2", engine = "kable")
  header <- kable_header(tab)
  expect_true(all(c("Model type", "Model", "Factors", "Items", "χ²", "df", "p", "CFI", "TLI",
                    "RMSEA [90% confidence interval]", "SRMR") %in% header))
  # the sample size, convergence and admissibility hold one value in every row
  expect_false(any(c("n", "Converged", "Admissible") %in% header))
  expect_equal(rh_cfa_fit_constant_text(scales, structure, combined),
               "All four models were fitted to the answers of 700 respondents and converged to admissible solutions.")
  dom <- kable_row(tab, c("^Model$" = rh_label("zm_power", labels)))
  expect_equal(kable_cell(dom, "^Model type$"), "One factor per scale")
  expect_equal(kable_cell(dom, "^CFI$"), rh_fmt(0.972, 3, bounded = TRUE))
  # TLI can exceed one and keeps its leading zero; p follows APA
  expect_equal(kable_cell(dom, "^TLI$"), "1.004")
  expect_equal(kable_cell(dom, "^p$"), "< .001")
  expect_equal(kable_cell(dom, "^RMSEA"), rh_fmt_est_ci(0.021, 0.015, 0.027, 3, bounded = TRUE))
  structure_row <- kable_row(tab, c("^Model$" = "DoPL-6 subscales"))
  expect_equal(kable_cell(structure_row, "^Model type$"), "Subscale structure of the development papers")
  # a model with an estimation message carries a letter and a plain-language
  # note; the message itself is reproduced in the supplement
  motives <- kable_row(tab, c("^Model$" = "All social motive scales<sup>a</sup>"))
  expect_equal(kable_cell(motives, "^Model type$"), "Item sets combining instruments (SRQ1)")
  expect_match(kable_text(tab), "<sup>a</sup> The estimation software found the covariance matrix", fixed = TRUE)
  expect_match(kable_text(tab), "The model converged to an admissible solution. The message is reproduced in Section S2.",
               fixed = TRUE)
  expect_match(kable_text(tab), "Fit-index version: robust (Brosseau-Liard & Savalei correction)", fixed = TRUE)
  messages <- rh_cfa_estimation_messages(scales, dplyr::bind_rows(structure, combined), labels)
  expect_equal(messages$model, "motives")
  expect_equal(messages$kind, "vcov")
  # an inadmissible model keeps the column and changes the sentence
  bad <- combined
  bad$admissible <- FALSE
  bad_tab <- rh_cfa_fit_table(scales, structure, bad, labels, engine = "kable")
  expect_true("Admissible" %in% kable_header(bad_tab))
  expect_equal(kable_cell(kable_row(bad_tab, c("^Model$" = "All social motive scales<sup>a</sup>")), "^Admissible$"), "no")
  expect_equal(rh_cfa_fit_constant_text(scales, structure, bad),
               "All four models were fitted to the answers of 700 respondents.")
  # without a recorded version the note says so
  no_version <- rh_cfa_fit_table(scales[setdiff(names(scales), "index_version")],
                                 structure[setdiff(names(structure), "index_version")],
                                 combined[setdiff(names(combined), "index_version")], labels, engine = "kable")
  expect_match(kable_text(no_version), "not recorded", fixed = TRUE)
})

cfa_reporting_fixture <- function() {
  list(
    loadings = tibble::tibble(
      model = "blocks", model_label = "Two blocks",
      factor = c("a", "a", "b"), factor_label = c("Block A", "Block A", "Block B"),
      item = c("a1", "a2", "b1"), item_label = c("Item A one", "Item A two", "Item B one"),
      estimate_unstd = c(1.1, 0.8, 1.2), se_unstd = c(0.1, 0.1, 0.2),
      estimate_std = c(0.7, 0.6, 0.8), se_std = c(0.05, 0.06, 0.04)
    ),
    factor_correlations = tibble::tibble(
      model = "blocks", model_label = "Two blocks",
      factor_1 = "a", factor_1_label = "Block A", factor_2 = "b", factor_2_label = "Block B",
      estimate_unstd = 0.3, se_unstd = 0.08, estimate_std = 0.4, se_std = 0.07
    ),
    residuals = tibble::tibble(
      model = "blocks", model_label = "Two blocks",
      item_1 = c("a2", "b1", "b1"), item_1_label = c("Item A two", "Item B one", "Item B one"),
      item_2 = c("a1", "a1", "a2"), item_2_label = c("Item A one", "Item A one", "Item A two"),
      bentler_residual = c(0.1, -0.2, 0.3), std_residual = c(0.5, -1.5, 2.5),
      abs_std_residual = c(0.5, 1.5, 2.5), rank_abs_std_residual = c(3L, 2L, 1L)
    ),
    ranked_residuals = tibble::tibble(
      model = "blocks", model_label = "Two blocks",
      item_1 = c("b1", "b1", "a2"), item_1_label = c("Item B one", "Item B one", "Item A two"),
      item_2 = c("a2", "a1", "a1"), item_2_label = c("Item A two", "Item A one", "Item A one"),
      bentler_residual = c(0.3, -0.2, 0.1), std_residual = c(2.5, -1.5, 0.5),
      abs_std_residual = c(2.5, 1.5, 0.5), rank_abs_std_residual = 1:3
    ),
    htmt = tibble::tibble(
      model = "blocks", model_label = "Two blocks",
      factor_1 = "a", factor_1_label = "Block A", factor_2 = "b", factor_2_label = "Block B", htmt = 0.42
    ),
    issues = tibble::tibble()
  )
}

test_that("the combined CFA tables put every model side by side and keep the largest residuals", {
  # Two one-factor models (blocks a and b) and one two-factor model of both,
  # in the plan's model families.
  blocks <- cfa_reporting_fixture()
  one_factor <- function(key, factor) {
    reporting <- blocks
    keep <- blocks$loadings$factor == factor
    reporting$loadings <- blocks$loadings[keep, ]
    reporting$loadings$model <- key
    reporting$loadings$model_label <- if (key == "a") "Block A" else "Block B"
    reporting$ranked_residuals <- blocks$ranked_residuals[0, ]
    reporting$loadings$estimate_std <- reporting$loadings$estimate_std + 0.05
    reporting$factor_correlations <- blocks$factor_correlations[0, ]
    reporting
  }
  model_rows <- tibble::tibble(
    model = c("a", "b", "blocks"), label = c("Block A", "Block B", "Two blocks"), n_items = c(2L, 1L, 3L),
    converged = TRUE, admissible = c(TRUE, TRUE, FALSE), note = NA_character_, error = NA_character_,
    reporting = list(one_factor("a", "a"), one_factor("b", "b"), blocks))
  models <- rh_cfa_reporting_models(model_rows)
  plan <- list(
    confirmatory_models = list(models = list(
      list(key = "a", label = "Block A", family = "subscale"),
      list(key = "b", label = "Block B", family = "subscale"),
      list(key = "blocks", label = "Two blocks", family = "pair"))),
    factor_analysis = list(cfa_reporting = list(ranked_residuals_n = 2L)))

  loadings <- tabulate_cfa_loadings_by_model(models, plan)
  expect_identical(names(loadings), c("item", "item_label", "scale", "scale_key", "subscale", "pair"))
  expect_identical(loadings$scale_key, c("a", "a", "b"))
  expect_equal(loadings$subscale, c(0.75, 0.65, 0.85))
  expect_equal(loadings$pair, c(0.7, 0.6, 0.8))
  expect_identical(attr(loadings, "family_labels"),
                   c(subscale = "One-factor model of the scale", pair = "Two blocks"))
  expect_identical(attr(loadings, "diagnostic_only"), "Two blocks")
  shown <- rh_cfa_loadings_by_model_table(loadings, "cfa_loadings.csv", engine = "kable")
  expect_identical(kable_header(shown), c("Scale", "Item", "One-factor model of the scale", "Two blocks"))
  expect_match(kable_text(shown), "cfa_loadings.csv", fixed = TRUE)
  expect_match(kable_text(shown), "Diagnostic output only", fixed = TRUE)
  # one table per group of scales: the rows of the other scales leave it
  only_b <- rh_cfa_loadings_by_model_table(loadings, "cfa_loadings.csv", scales = "b", engine = "kable")
  expect_equal(kable_nrow(only_b), 1L)
  expect_equal(kable_cell(kable_cells(only_b)$rows[[1]], "^Two blocks$"), rh_fmt(0.8, 2, bounded = TRUE))

  # the latent correlations: one matrix per group of scales, the estimate and
  # its standard error in one cell
  correlations <- tabulate_cfa_factor_correlations(models)
  expect_equal(nrow(correlations), 1L)
  expect_equal(correlations$correlation, 0.4)
  expect_identical(c(correlations$factor_1_key, correlations$factor_2_key), c("a", "b"))
  matrix <- rh_cfa_factor_correlation_matrix_table(correlations, c("a", "b"), labels, engine = "kable")
  expect_identical(kable_header(matrix), c("Scale", rh_label("a", labels)))
  expect_equal(kable_cell(kable_row(matrix, c("^Scale$" = rh_label("b", labels))), paste0("^", rh_label("a", labels), "$")),
               rh_fmt_est_se(0.4, 0.07, 2, bounded = TRUE))
  expect_match(kable_text(matrix), "Latent correlation (standard error) from the confirmatory model “Two blocks”.",
               fixed = TRUE)
  expect_null(rh_cfa_factor_correlation_matrix_table(correlations, c("a", "c"), labels, engine = "kable"))
  # a smaller model that repeats a pair is compared with the matrix in one sentence
  expect_equal(rh_cfa_factor_correlation_agreement_text(correlations, list(c("a", "b")), labels), "")
  smaller <- function(value) dplyr::bind_rows(correlations, tibble::tibble(
    model = "Small", model_key = "small", factor_1 = "Block A", factor_2 = "Block B",
    factor_1_key = "a", factor_2_key = "b", correlation = value, se = 0.08))
  expect_equal(rh_cfa_factor_correlation_agreement_text(smaller(0.402), list(c("a", "b")), labels),
               "The smaller model “Small” gives the same latent correlations for the pairs they share, to two decimals.")
  expect_match(rh_cfa_factor_correlation_agreement_text(smaller(0.41), list(c("a", "b")), labels),
               paste0("in “Small” (", rh_fmt(0.41, 2, bounded = TRUE), " against ", rh_fmt(0.4, 2, bounded = TRUE), ")."),
               fixed = TRUE)

  residuals <- tabulate_cfa_largest_residuals(models, 2L)
  expect_equal(nrow(residuals[residuals$model == "Two blocks", ]), 2L)
  expect_equal(residuals$standardised_residual[residuals$model == "Two blocks"], c(2.5, -1.5))
  expect_identical(unique(residuals$model_key), "blocks")
  shown <- rh_cfa_largest_residuals_table(residuals, "cfa_pairwise_residuals.csv", engine = "kable")
  expect_match(kable_text(shown), "cfa_pairwise_residuals.csv", fixed = TRUE)
  first <- kable_cells(shown)$rows[[1]]
  expect_equal(kable_cell(first, "^Model$"), "Two blocks")
  # a correlation residual is bounded (no leading zero); a standardised residual keeps it
  expect_equal(kable_cell(first, "^Correlation residual$"), rh_fmt(0.3, 2, bounded = TRUE))
  expect_equal(kable_cell(first, "^Standardised residual$"), rh_fmt(2.5, 2))
})

# One coefficient row with the fit-level diagnostic fields
coef_fixture_row <- function(outcome, term, term_type, ess_bulk, ess_tail, rhat, n_divergent, fit_valid,
                             role = "primary", family = "gaussian", slope_sd = analysis_plan$priors$slope_sd_primary,
                             iter_used = 1000L, nu_fixed = NA_real_, gate_status = if (isTRUE(fit_valid)) "ok" else "not_interpretable") {
  tibble::tibble(
    outcome = outcome, slope_sd = slope_sd, role = role, term = term, term_type = term_type,
    estimate = 0.1, sd = 0.05, q2.5 = 0.0, q97.5 = 0.2, p_positive = 0.97, p_negative = 0.03,
    ess_bulk = ess_bulk, ess_tail = ess_tail, rhat = rhat,
    fit_valid = fit_valid, ess_ok = NA, rhat_ok = NA, divergences_ok = n_divergent == 0,
    ess_target = analysis_plan$regression$ess_target, rhat_limit = analysis_plan$regression$rhat_max, n_divergent = n_divergent,
    n_obs = 700L, iter_used = iter_used, family = family, nu_fixed = nu_fixed, gate_status = gate_status
  )
}

# One diagnostics row in the shape of ap6_fit_diagnostics() tagged by the pipeline (target ap6_diagnostics_tbl)
diag_fixture_row <- function(outcome, slope_sd, role, family, ess_bulk_min, ess_tail_min, rhat_max, n_divergent,
                             n_treedepth_hits, bfmi_min, ok, gate_status, gate_note = "ok", n_obs = 700L, iter_used = 1000L) {
  tibble::tibble(
    outcome = outcome, slope_sd = slope_sd, role = role, label = rh_sweep_label(slope_sd, analysis_plan), family = family,
    n_obs = n_obs, iter_used = iter_used, gate_status = gate_status,
    ok = ok, ess_ok = NA, rhat_ok = NA, divergences_ok = n_divergent == 0, treedepth_ok = n_treedepth_hits == 0, bfmi_ok = NA,
    ess_bulk_min = ess_bulk_min, ess_tail_min = ess_tail_min, rhat_max = rhat_max, n_divergent = n_divergent,
    n_treedepth_hits = n_treedepth_hits, bfmi_min = bfmi_min, mcse_median_max = 0.0012, mcse_q_max = 0.0034,
    ess_target = analysis_plan$regression$ess_target, rhat_limit = analysis_plan$regression$validity_gate$rhat_max,
    divergences_max = analysis_plan$regression$validity_gate$divergences_max,
    treedepth_hits_max = analysis_plan$regression$validity_gate$treedepth_hits_max,
    bfmi_limit = analysis_plan$regression$validity_gate$bfmi_min, gate_note = gate_note
  )
}

test_that("rh_diagnostics_table() gives one slim row per fit: ESS, R-hat, BFMI, divergences, tree depth, gate", {
  ess_hi <- analysis_plan$regression$ess_target + 500
  ess_lo <- analysis_plan$regression$ess_target - 500
  gate <- analysis_plan$regression$validity_gate
  diag <- dplyr::bind_rows(
    diag_fixture_row("asc_agg", analysis_plan$priors$slope_sd_primary, "primary", "gaussian", ess_hi, ess_hi + 100, gate$rhat_max - 0.006,
                     0L, 0L, gate$bfmi_min + 0.5, TRUE, "ok"),
    diag_fixture_row("asc_agg", 0.1, "sweep", "gaussian", ess_hi, ess_hi, gate$rhat_max - 0.005,
                     0L, 0L, gate$bfmi_min + 0.4, TRUE, "retried_ok", gate_note = "retry: 2 divergent transitions on the first attempt"),
    diag_fixture_row("sdo_dom", analysis_plan$priors$slope_sd_primary, "primary", "gaussian", ess_lo, ess_lo, gate$rhat_max + 0.02,
                     3L, 7L, gate$bfmi_min - 0.05, FALSE, "not_interpretable", gate_note = "R-hat 1.03 > 1.01; 3 divergent transitions > 0"),
    diag_fixture_row("asc_agg", analysis_plan$priors$slope_sd_primary, "student_refit", "student", ess_hi, ess_hi, gate$rhat_max - 0.006,
                     0L, 0L, gate$bfmi_min + 0.5, TRUE, "ok", iter_used = 2000L)
  )
  tab <- rh_diagnostics_table(diag, analysis_plan, labels, engine = "kable")
  header <- kable_header(tab)
  # one slim row per fit; the sample size, equal in every fit, leaves the table
  expect_identical(header, c("Fit", "Post-warm-up iterations per chain", "Smallest bulk ESS", "Smallest tail ESS",
                             "Largest R̂", "Smallest BFMI", "Divergent transitions",
                             "Transitions at the maximum tree depth", "Validity gate"))
  # Monte Carlo standard errors are not reported.
  expect_false(any(grepl("MCSE", header, fixed = TRUE)))
  expect_equal(kable_nrow(tab), 4)
  primary <- paste0(rh_label("asc_agg", labels), ", primary prior (SD ", rh_fmt(analysis_plan$priors$slope_sd_primary, 2), ")")
  ok <- kable_row(tab, c("^Fit$" = primary))
  retried <- kable_row(tab, c("^Fit$" = paste0(rh_label("asc_agg", labels), ", ", rh_sweep_label(0.1, analysis_plan),
                                               " prior (SD ", rh_fmt(0.1, 2), ")")))
  bad <- kable_row(tab, c("^Fit$" = paste0(rh_label("sdo_dom", labels), ", primary prior (SD ",
                                           rh_fmt(analysis_plan$priors$slope_sd_primary, 2), ")")))
  st <- kable_row(tab, c("^Fit$" = paste0(primary, ", Student-t")))
  expect_equal(kable_cell(ok, "^Smallest bulk ESS$"), rh_fmt_n(ess_hi))
  expect_equal(kable_cell(ok, "^Smallest tail ESS$"), rh_fmt_n(ess_hi + 100))
  expect_equal(kable_cell(ok, "^Largest R"), rh_fmt(gate$rhat_max - 0.006, 3))
  # BFMI can exceed one, so it keeps its leading zero
  expect_equal(kable_cell(ok, "^Smallest BFMI$"), rh_fmt(gate$bfmi_min + 0.5, 2))
  expect_equal(kable_cell(ok, "^Divergent transitions$"), "0")
  expect_equal(kable_cell(ok, "^Validity gate$"), "passed")
  expect_equal(kable_cell(retried, "^Validity gate$"), "passed after one retry")
  expect_equal(kable_cell(bad, "^Validity gate$"), "not passed")
  expect_equal(kable_cell(bad, "^Divergent transitions$"), "3")
  expect_equal(kable_cell(bad, "^Transitions at the maximum tree depth$"), "7")
  expect_equal(kable_cell(st, "^Post-warm-up iterations per chain$"), rh_fmt_n(2000))
  expect_equal(kable_cell(ok, "^Post-warm-up iterations per chain$"), rh_fmt_n(1000))
  # a Student-t diagnostics of NULL (no refit ran) binds to nothing and changes nothing
  same <- rh_diagnostics_table(dplyr::bind_rows(diag, NULL), analysis_plan, labels, engine = "kable")
  expect_equal(kable_nrow(same), 4)
  # the note explains each diagnostic in words and states the gate of this run
  note <- kable_text(tab)
  expect_match(note, "Over the population-level coefficients of each fit", fixed = TRUE)
  expect_match(note, paste0("an ESS of at least ", rh_fmt_n(analysis_plan$regression$ess_target)), fixed = TRUE)
  expect_match(note, paste0("R̂ of at most ", rh_fmt(gate$rhat_max, 3)), fixed = TRUE)
  expect_match(note, paste0("a BFMI of at least ", rh_fmt(gate$bfmi_min, 2), " in every chain"), fixed = TRUE)
  expect_false(grepl("MCSE", note, fixed = TRUE))
  # the joint model and the Student-t refits join as further rows, the joint model last
  additional <- tibble::tibble(
    outcome = c("asc_agg", NA), ess_bulk_min = ess_hi, ess_tail_min = ess_hi, rhat_max = gate$rhat_max - 0.006,
    bfmi_min = gate$bfmi_min + 0.5, n_divergent = 0L, n_treedepth_hits = 0L, gate_status = "ok", gate_passed = TRUE
  )
  rows <- rh_diagnostics_rows(diag, analysis_plan, labels, additional)
  expect_identical(utils::tail(rows$model, 2),
                   c(paste0(rh_label("asc_agg", labels), ", refit allowing heavier tails (Student-t)"),
                     "Joint regression of the four facets (RQ2)"))
  # values every fit shares leave the table for the sentence
  clean <- diag[c(1, 2, 1), ]
  clean$outcome[3] <- "sdo_dom"
  clean_tab <- rh_diagnostics_table(clean, analysis_plan, labels, engine = "kable")
  expect_false(any(c("n", "Post-warm-up iterations per chain", "Divergent transitions",
                     "Transitions at the maximum tree depth") %in% kable_header(clean_tab)))
  expect_true("Validity gate" %in% kable_header(clean_tab))
  expect_equal(
    rh_diagnostics_constant_text(clean, analysis_plan, labels),
    paste0("All three fits had no divergent transition and no transition at the maximum tree depth and passed every ",
           "check of the validity gate; the three normal fits of RQ1 used 700 cases and 1,000 post-warm-up ",
           "iterations per chain.")
  )
  expect_equal(rh_diagnostics_constant_text(diag, analysis_plan, labels), "")
  # the fit-level route needs the aggregated gate columns; it never recomputes them
  expect_error(
    rh_diagnostics_table(diag[, setdiff(names(diag), "ess_bulk_min")], analysis_plan, labels, engine = "kable"),
    "ess_bulk_min"
  )
})

test_that("rh_tail_text() names the facets that met the heavy-tail rule, the statistics outside their range and the refits", {
  # The heavy-tail check is a sentence: its statistics are in the posterior
  # predictive table.
  stats <- as.character(unlist(analysis_plan$sensitivity$student_t$trigger_stats))
  flags <- rep(c(FALSE, TRUE), length.out = length(outcomes))
  tails <- tibble::tibble(outcome = outcomes, heavy_tails = flags)
  pp_stats <- purrr::map_dfr(outcomes, function(o) {
    tibble::tibble(
      outcome = o, stat = c("mean", "sd", "min", "max", "kurtosis", "skew"),
      observed = c(0, 1, -2.5, 3.9, 0.9, 0.1),
      rep_lo = c(-0.1, 0.9, -3.4, 2.9, -0.3, -0.2),
      rep_hi = c(0.1, 1.1, -2.7, 3.5, 0.4, 0.2)
    )
  })
  met <- outcomes[flags]
  student <- tibble::tibble(outcome = met, term = "zm_power", estimate = 0.1, fit_available = TRUE)
  text <- rh_tail_text(tails, pp_stats, student, analysis_plan, labels)
  expect_match(text, paste0(paste(rh_label(met, labels), collapse = " and "),
                            " met the preregistered rule for heavier-than-normal tails: "), fixed = TRUE)
  if ("kurtosis" %in% stats) {
    expect_match(text, paste0("excess kurtosis (", rh_fmt(0.9), "; simulated range ", rh_fmt_ci(-0.3, 0.4), ")"), fixed = TRUE)
  }
  if ("max" %in% stats) {
    expect_match(text, paste0("maximum (", rh_fmt(3.9), "; simulated range ", rh_fmt_ci(2.9, 3.5), ")"), fixed = TRUE)
  }
  # the minimum lies inside its range, the mean is no trigger statistic
  expect_false(grepl("minimum", text, fixed = TRUE))
  expect_false(grepl("mean", text, fixed = TRUE))
  ci_level <- rh_fmt(100 * analysis_plan$regression$ci_level, 0)
  expect_match(text, paste0("lay outside the range of ", ci_level, "% of the data sets simulated from the normal model"),
               fixed = TRUE)
  expect_match(text, paste0("The refit allowing heavier tails (Student-t errors) therefore ran for ",
                            paste(rh_label_inline(met, labels), collapse = " and "), " only."), fixed = TRUE)
  # one facet
  one <- tails
  one$heavy_tails <- outcomes == outcomes[2]
  one_text <- rh_tail_text(one, pp_stats, student[student$outcome == outcomes[2], ], analysis_plan, labels)
  expect_match(one_text, paste0("Only ", rh_label(outcomes[2], labels), " met the preregistered rule"), fixed = TRUE)
  # the refit target carries a placeholder row per untriggered outcome
  # (fit_available FALSE, no term): a row that stands for a fit that never ran
  # does not count as a refit
  placeholders <- tibble::tibble(
    outcome = outcomes[!flags], term = NA_character_,
    estimate = NA_real_, fit_available = FALSE, note = "not triggered"
  )
  expect_identical(rh_tail_text(tails, pp_stats, dplyr::bind_rows(student, placeholders), analysis_plan, labels), text)
  expect_equal(rh_fitted_outcomes(dplyr::bind_rows(student, placeholders)), met)
  expect_equal(rh_fitted_outcomes(placeholders), character(0))
  expect_equal(rh_fitted_outcomes(student[0, ]), character(0))
  # a refit that did not complete is named
  expect_match(rh_tail_text(tails, pp_stats, student[0, ], analysis_plan, labels),
               paste0("The refit allowing heavier tails did not complete for ",
                      paste(rh_label_inline(met, labels), collapse = " and "), "."), fixed = TRUE)
  # no facet met the rule
  calm <- tails
  calm$heavy_tails <- FALSE
  expect_equal(rh_tail_text(calm, pp_stats, student[0, ], analysis_plan, labels),
               "No facet met the preregistered rule for heavier-than-normal tails, so no model was refitted allowing heavier tails.")
  # an empty configured trigger-statistic list is a configuration error, not a default
  no_stats <- analysis_plan
  no_stats$sensitivity$student_t$trigger_stats <- list()
  expect_error(rh_tail_text(tails, pp_stats, student, no_stats, labels), "trigger_stats")
})

test_that("rh_robustness_table() keeps only the refitted coefficients and states the fixed nu with its scale prior", {
  g <- tibble::tibble(
    outcome = c("asc_agg", "sdo_dom"), slope_sd = analysis_plan$priors$slope_sd_primary, role = "primary",
    term = "zm_power", term_type = "predictor",
    estimate = c(0.20, 0.02), q2.5 = c(0.10, -0.08), q97.5 = c(0.30, 0.12), family = "gaussian",
    fit_valid = TRUE, gate_status = "ok"
  )
  s <- tibble::tibble(
    outcome = "asc_agg", slope_sd = analysis_plan$priors$slope_sd_primary, role = "student_refit",
    term = "zm_power", term_type = "predictor",
    estimate = 0.19, q2.5 = 0.09, q97.5 = 0.29, family = "student", nu_fixed = 4,
    fit_valid = TRUE, gate_status = "ok"
  )
  sdo <- rh_label("sdo_dom", labels)
  robustness <- function(g, s) {
    rh_robustness_table(tabulate_likelihood_comparison(likelihood_pairs(g, s), g, s, analysis_plan),
                        labels, engine = "kable", analysis_plan = analysis_plan)
  }
  tab <- robustness(g, s)
  # only the refitted facet keeps its rows; with one
  # refitted facet the outcome column leaves the table
  expect_equal(kable_nrow(tab), 1)
  expect_identical(kable_header(tab), c("Coefficient", "Normal model", "Model allowing heavier tails (Student-t)"))
  row <- kable_cells(tab)$rows[[1]]
  expect_equal(kable_cell(row, "^Coefficient$"), rh_label("zm_power", labels))
  # each estimate with its interval in one cell, bold where the interval excludes zero
  expect_equal(kable_cell(row, "^Normal model$"), rh_fmt_est_ci(0.20, 0.10, 0.30, bold = TRUE))
  expect_equal(kable_cell(row, "^Model allowing"), rh_fmt_est_ci(0.19, 0.09, 0.29, bold = TRUE))
  note <- kable_text(tab)
  expect_match(note, "bold estimate: its interval excludes zero", fixed = TRUE)
  st <- analysis_plan$sensitivity$student_t
  # the fixed nu of the refit rows and the configured matched scale prior are stated; no prior on nu
  expect_match(note, paste0("ν fixed at ", rh_fmt_n(st$nu_fixed), " (constant, no tail parameter)"), fixed = TRUE)
  expect_match(note, paste0("half-normal(0, ", rh_fmt(st$sigma_scale_prior_sd), ")"), fixed = TRUE)
  expect_false(grepl("ν ~|gamma\\(|truncated", note))
  # a refit table recording another fixed value wins over the configuration
  s7 <- s
  s7$nu_fixed <- 7
  expect_match(kable_text(robustness(g, s7)), "ν fixed at 7 ", fixed = TRUE)
  # two refitted facets: the outcome column names them; an interval with zero is not bold
  s_sdo <- s
  s_sdo$outcome <- "sdo_dom"
  s_sdo$estimate <- 0.03
  s_sdo$q2.5 <- -0.07
  s_sdo$q97.5 <- 0.13
  two <- robustness(g, dplyr::bind_rows(s, s_sdo))
  expect_equal(kable_nrow(two), 2)
  expect_true("Outcome" %in% kable_header(two))
  sdo_row <- kable_row(two, c("^Outcome$" = sdo))
  expect_equal(kable_cell(sdo_row, "^Model allowing"), rh_fmt_est_ci(0.03, -0.07, 0.13))
  expect_equal(kable_cell(sdo_row, "^Normal model$"), rh_fmt_est_ci(0.02, -0.08, 0.12))
  # no refit at all (NULL, or the zero-row table of the untriggered branch): no table
  for (none in list(NULL, s[0, ])) expect_null(robustness(g, none))
})

test_that("robustness displays render analysis decisions without classifying the numerical intervals again", {
  g <- tibble::tibble(
    outcome = "asc_agg", term = "zm_power", term_type = "predictor",
    estimate = .2, q2.5 = .1, q97.5 = .3, fit_valid = TRUE, gate_status = "ok"
  )
  s <- g
  s$nu_fixed <- 4
  s$gate_status <- "not_interpretable"
  comparison <- tabulate_likelihood_comparison(likelihood_pairs(g, s), g, s, analysis_plan)
  tab <- rh_robustness_table(comparison, labels, engine = "kable", analysis_plan = analysis_plan)
  row <- kable_cells(tab)$rows[[1]]
  expect_equal(kable_cell(row, "^Normal model$"), rh_fmt_est_ci(.2, .1, .3, bold = TRUE))
  # the refit whose sampling checks did not pass is shown, without classification
  expect_equal(kable_cell(row, "^Model allowing"), rh_fmt_est_ci(.2, .1, .3))
  expect_match(kable_text(tab), "shown without classification", fixed = TRUE)
  # The renderer follows the supplied decision field, even if numerical
  # columns are changed afterwards. Scientific classification belongs to AP10.
  comparison$lo_gaussian <- -.1
  comparison$hi_gaussian <- .1
  rendered <- rh_robustness_table(comparison, labels, engine = "kable", analysis_plan = analysis_plan)
  expect_equal(kable_cell(kable_cells(rendered)$rows[[1]], "^Normal model$"), rh_fmt_est_ci(.2, -.1, .1, bold = TRUE))
})

test_that("the network subset edges follow the preregistration numbering", {
  nodes <- zm_key_of_z_col(as.character(analysis_plan$network$nodes), codebook)
  pairs <- utils::combn(nodes, 2)
  bagged <- tibble::tibble(
    node_i = pairs[1, ], node_j = pairs[2, ], n_success = 12L,
    pip_bagged = 0.5, bf_bagged = 1.0, weight_bagged = 0.1,
    decision = rep(c("present", "absent", "inconclusive"), length.out = ncol(pairs))
  )
  attr(bagged, "feasible") <- TRUE
  edges <- tibble::tibble(node_i = zm_z_col(bagged$node_i, codebook), node_j = zm_z_col(bagged$node_j, codebook),
                          decision = bagged$decision)
  network <- tabulate_network_questions(list(
    conditional_structure = edges,
    autonomy_orientation = extract_network_autonomy_orientation_edges(edges),
    autonomy_security_arousal = extract_network_autonomy_security_arousal_edges(edges)
  ), bagged, codebook)

  expect_equal(rh_network_subset_edges(network, "conditional_structure"), network$conditional_structure)
  expect_equal(nrow(rh_network_subset_edges(network, "conditional_structure")), nrow(bagged))
  expect_equal(rh_network_subset_edges(network, "autonomy_orientation"), network$autonomy_orientation)
  expect_equal(nrow(rh_network_subset_edges(network, "autonomy_orientation")), length(network$autonomy_nodes) * length(outcomes))
  expect_equal(rh_network_subset_edges(network, "autonomy_security_arousal"), network$autonomy_security_arousal)
  expect_equal(nrow(rh_network_subset_edges(network, "autonomy_security_arousal")),
               length(network$autonomy_nodes) * length(network$security_arousal_nodes))
})

test_that("freshness checking distinguishes stale calculations from rendering and fails closed", {
  seen <- NULL
  check <- function(root, store, analysis_plan, data_source) {
    seen <<- list(root = root, store = store, profile = analysis_plan$profile_name, data_source = data_source)
    c("ap4_reliability_tbl", "results_draft", "results_draft_file", "simulation_study_tbl")
  }
  status <- rh_report_freshness(root, file.path(root, "_targets"), analysis_plan, "synthetic", checker = check)
  expect_equal(status$state, "outdated")
  expect_equal(status$outdated, c("ap4_reliability_tbl", "simulation_study_tbl"))
  expect_length(status$other_outdated, 2L)
  expect_equal(seen$profile, analysis_plan$profile_name)
  expect_equal(seen$data_source, "synthetic")
  expect_equal(rh_freshness_text(status), paste(
    "When this report was rendered, two stored results it reads were out of date: the code or the input behind",
    "them had changed since they were computed. Rendering does not recompute results."))
  expect_false(grepl("ap4_reliability_tbl", rh_freshness_text(status), fixed = TRUE))
  one <- status
  one$outdated <- "ap4_reliability_tbl"
  expect_match(rh_freshness_text(one), "one stored result it reads was out of date: the code or the input behind it",
               fixed = TRUE)

  report_only <- rh_report_freshness(root, file.path(root, "_targets"), analysis_plan, "synthetic",
    checker = function(...) c("results_draft", "results_draft_file"))
  expect_equal(report_only$state, "current")
  expect_equal(rh_freshness_text(report_only), paste(
    "When this report was rendered, every stored result it reads was current: no code or input behind them had",
    "changed since they were computed. Rendering does not recompute results."))
  # the report asks about its own inputs only: the names reach the checker
  asked <- NULL
  scoped <- rh_report_freshness(root, file.path(root, "_targets"), analysis_plan, "synthetic",
    checker = function(root, store, analysis_plan, data_source, names) {
      asked <<- names
      character()
    }, names = "results_draft")
  expect_equal(asked, "results_draft")
  expect_equal(scoped$state, "current")

  simulation_only <- rh_report_freshness(root, file.path(root, "_targets"), analysis_plan, "synthetic",
    checker = function(...) c("simulation_study_file", "simulation_study_tbl", "supplement_design_precision"))
  expect_equal(simulation_only$state, "outdated")
  expect_length(simulation_only$outdated, 3L)
  expect_length(simulation_only$other_outdated, 0L)

  failed <- rh_report_freshness(root, file.path(root, "_targets"), analysis_plan, "synthetic",
    checker = function(...) stop("dependency checker unavailable"))
  expect_equal(failed$state, "unavailable")
  expect_match(rh_freshness_text(failed), "could not be checked", fixed = TRUE)
  expect_match(rh_freshness_text(failed), "unchecked", fixed = TRUE)
  malformed <- rh_report_freshness(root, file.path(root, "_targets"), analysis_plan, "synthetic",
    checker = function(...) NA_character_)
  expect_equal(malformed$state, "unavailable")
})

# ---- display helpers: units, labels and levels read from the data, not typed ----

test_that("rh_prior_pred_table() gives the percentiles of the simulated answers per model and keeps sub-percent shares visible", {
  primary <- analysis_plan$priors$slope_sd_primary
  model_label <- function(sd, student = FALSE) {
    name <- rh_sweep_label(sd, analysis_plan)
    paste0(toupper(substr(name, 1, 1)), substring(name, 2), " prior (SD ", rh_fmt(sd, 2), ")",
           if (student) ", Student-t" else "")
  }
  pp <- tibble::tibble(
    outcome = c("asc_agg", "sdo_dom"), slope_sd = primary, n_draws = 4000L, n_obs = 700L,
    mean_z = 0.0004, sd_z = 1.17, mean = 2.99, sd = 0.88, q5 = c(1.6, 1.5), q50 = c(3.0, 2.9), q95 = c(4.39, 4.2),
    p95_abs_prediction = c(2.0, 2.1),
    share_below_min = c(0.0192, 0.0311), share_above_max = c(0.0043, 0.0013), share_outside_range = c(0.0235, 0.0324),
    family = "gaussian",
    range_min = analysis_plan$scales$response_min, range_max = analysis_plan$scales$response_max, z_mean = 2.99, z_sd = 0.76
  )
  tab <- rh_prior_pred_table(pp, labels, analysis_plan = analysis_plan, engine = "kable")
  header <- kable_header(tab)
  expect_identical(header[c(1:5, 7:8)], c("Outcome", "Model", "5th percentile", "50th percentile", "95th percentile",
                                          "Below the response range", "Above the response range"))
  expect_match(header[6], "^95th percentile of ")
  agg <- kable_row(tab, c("^Outcome$" = rh_label("asc_agg", labels)))
  sdo <- kable_row(tab, c("^Outcome$" = rh_label("sdo_dom", labels)))
  expect_equal(kable_cell(agg, "^Model$"), model_label(primary))
  expect_equal(kable_cell(agg, "^50th percentile$"), rh_fmt(3.0, 2))
  expect_equal(kable_cell(sdo, "^95th percentile$"), rh_fmt(4.2, 2))
  expect_equal(kable_cell(agg, "^Above the response range$"), rh_fmt_pct(0.0043, 1, scale = "proportion"))
  expect_equal(kable_cell(sdo, "^Above the response range$"), rh_fmt_pct(0.0013, 1, scale = "proportion"))
  expect_equal(kable_cell(sdo, "^Below the response range$"), rh_fmt_pct(0.0311, 1, scale = "proportion"))
  expect_equal(rh_fmt_pct(0.0013, 1, scale = "proportion"), "0.1%")                     # a sub-percent share is not shown as zero; APA sets the sign closed up
  expect_match(kable_text(tab), "Shares: percentages of all simulated", fixed = TRUE)
  expect_match(kable_text(tab), "never standardised again", fixed = TRUE)
  # the draws, the sample and the response range hold one value in every row: one sentence
  expect_equal(rh_prior_pred_constant_text(pp), paste0(
    "Each of the 2 rows summarises 4,000 draws from the priors for the 700 respondents of the model's sample; ",
    "the response options run from ", rh_fmt_n(analysis_plan$scales$response_min), " to ",
    rh_fmt_n(analysis_plan$scales$response_max), "."))
  # a sample that differs between rows keeps its columns in the table
  varied <- pp
  varied$n_obs <- c(700L, 698L)
  tv <- rh_prior_pred_table(varied, labels, analysis_plan = analysis_plan, engine = "kable")
  expect_true(all(c("Draws from the priors", "Respondents") %in% kable_header(tv)))
  expect_equal(kable_cell(kable_row(tv, c("^Outcome$" = rh_label("sdo_dom", labels))), "^Respondents$"), "698")
  expect_false(grepl("draws from the priors", rh_prior_pred_constant_text(varied), fixed = TRUE))
  # rows per outcome x family x slope SD (the grid of the sweep and the Student-t family), labelled and ordered
  grid_row <- function(outcome, family, slope_sd) {
    tibble::tibble(outcome = outcome, family = family, slope_sd = slope_sd, q5 = 1.5, q50 = 3, q95 = 4.5,
                   p95_abs_prediction = 2, share_below_min = 0.01, share_above_max = 0.02)
  }
  grid <- dplyr::bind_rows(
    grid_row("sdo_dom", "student", 0.2),
    grid_row("asc_agg", "gaussian", c(0.4, 0.1, 0.2)),
    grid_row("asc_agg", "student", 0.2)
  )
  tg <- rh_prior_pred_table(grid, labels, analysis_plan = analysis_plan, engine = "kable")
  rows <- kable_cells(tg)$rows
  expect_equal(vapply(rows, function(r) kable_cell(r, "^Outcome$"), character(1)),
               c(rh_label("asc_agg", labels), "", "", "", rh_label("sdo_dom", labels)))
  expect_equal(vapply(rows, function(r) kable_cell(r, "^Model$"), character(1)),
               c(model_label(0.1), model_label(0.2), model_label(0.4), model_label(0.2, TRUE), model_label(0.2, TRUE)))
  # no raw snake_case column survives as an "underscore to space" header
  raw <- c("slope sd", "n draws", "n obs", "mean z", "sd z", "mean", "sd", "q5", "q95", "5%", "95%",
           "share below min", "share above max", "share outside range", "range min", "range max", "z mean", "z sd")
  expect_length(intersect(c(header, kable_header(tg)), raw), 0)
})

test_that("rh_sensitivity_table() gives one median [CrI] column per prior width, bold where the interval excludes zero", {
  s <- tibble::tibble(
    outcome = "asc_agg", term = c("zm_security", "zm_power"), term_type = "predictor",
    est_0.1 = c(-0.04, 0.16), est_0.2 = c(-0.05, 0.17), est_0.4 = c(-0.05, 0.18),
    lo_0.1 = c(-0.11, 0.08), lo_0.2 = c(-0.12, 0.09), lo_0.4 = c(-0.13, 0.10),
    hi_0.1 = c(0.03, 0.24), hi_0.2 = c(0.02, 0.25), hi_0.4 = c(0.02, 0.26),
    credible_under = c("", "0.1; 0.2; 0.4"), n_sds = 3L, n_credible = c(0L, 3L), stability = "stable",
    comparison_valid = TRUE,
    prior_sens = c(0.0062, 0.0262), lik_sens = c(0.0772, 0.0956), diagnosis = "-"
  )
  head_of <- function(sd) {
    name <- rh_sweep_label(sd, analysis_plan)
    paste0(toupper(substr(name, 1, 1)), substring(name, 2), " prior (SD ", rh_fmt(sd, 2), ")")
  }
  tab <- rh_sensitivity_table(s, labels, engine = "kable", analysis_plan = analysis_plan)
  # the prior widths in increasing order, named by their configured sweep labels
  expect_identical(kable_header(tab), c("Outcome", "Motive", head_of(0.1), head_of(0.2), head_of(0.4)))
  # an SD without a configured label keeps a bare head
  s_extra <- s
  s_extra$est_2 <- 0; s_extra$lo_2 <- -0.1; s_extra$hi_2 <- 0.1
  expect_true(paste0("Prior (SD ", rh_fmt(2, 2), ")") %in%
                kable_header(rh_sensitivity_table(s_extra, labels, engine = "kable", analysis_plan = analysis_plan)))
  intim <- kable_row(tab, c("^Motive$" = rh_label("zm_security", labels)))
  dom <- kable_row(tab, c("^Motive$" = rh_label("zm_power", labels)))
  # one cell per width: the median and its interval, bold where the interval excludes zero
  expect_equal(kable_cell(dom, "^Primary prior"), rh_fmt_est_ci(0.17, 0.09, 0.25, bold = TRUE))
  expect_equal(kable_cell(intim, "^Primary prior"), rh_fmt_est_ci(-0.05, -0.12, 0.02))
  ci <- paste0(rh_fmt(100 * analysis_plan$regression$ci_level, 0), "% CrI")
  note <- kable_text(tab)
  expect_match(note, paste0("Posterior median [", ci, "] of each motive coefficient"), fixed = TRUE)
  expect_match(note, "bold estimate: its interval excludes zero", fixed = TRUE)
  expect_false(grepl("withholds the comparison", note, fixed = TRUE))
  unavailable <- s[1, ]
  unavailable$comparison_valid <- FALSE
  unavailable_tab <- rh_sensitivity_table(unavailable, labels, engine = "kable", analysis_plan = analysis_plan)
  expect_match(kable_text(unavailable_tab), "An invalid or missing fit withholds the comparison", fixed = TRUE)
})

test_that("rh_pp_stats_table() names the statistics in words, bolds an observed value outside its range and prints p in APA style", {
  pp <- tibble::tibble(
    outcome = "sdo_dom", slope_sd = analysis_plan$priors$slope_sd_primary, stat = c("mean", "sd", "min", "skew", "max"),
    observed = c(0, 1, -2.23, 0.27, 2.9), rep_median = c(0, 1, -3.08, 0.00, 3.1),
    rep_lo = c(-0.1, 0.9, -3.94, -0.17, 2.6), rep_hi = c(0.1, 1.1, -2.53, 0.17, 3.7),
    p_value = c(0.5, 0.5, 0.00025, 0.00175, 0.62)
  )
  level <- rh_fmt(100 * analysis_plan$regression$ci_level, 0)
  tab <- rh_pp_stats_table(pp, labels, engine = "kable", analysis_plan = analysis_plan)
  expect_identical(kable_header(tab), c("Outcome", "Statistic", "Observed",
                                        paste0("Range of ", level, "% of the simulated data sets"),
                                        "P(replicated ≥ observed)"))
  # the mean and SD of a standardised outcome are fixed by construction and leave the table
  expect_equal(kable_nrow(tab), 3)
  min_row <- kable_row(tab, c("^Statistic$" = "minimum"))
  skew_row <- kable_row(tab, c("^Statistic$" = "skewness"))
  max_row <- kable_row(tab, c("^Statistic$" = "maximum"))
  expect_equal(kable_cell(min_row, "^Observed$"), paste0("**", rh_fmt(-2.23), "**"))
  expect_equal(kable_cell(skew_row, "^Observed$"), paste0("**", rh_fmt(0.27), "**"))
  expect_equal(kable_cell(max_row, "^Observed$"), rh_fmt(2.9))
  expect_equal(kable_cell(skew_row, "^Range"), rh_fmt_ci(-0.17, 0.17))
  expect_equal(kable_cell(min_row, "^P\\(replicated"), "< .001")
  expect_equal(kable_cell(skew_row, "^P\\(replicated"), ".002")
  expect_equal(kable_cell(max_row, "^P\\(replicated"), ".620")
  note <- kable_text(tab)
  expect_match(note, "share of the simulated data sets whose statistic is at or above the observed value", fixed = TRUE)
  expect_match(note, "lighter (more bounded) lower tail", fixed = TRUE)
  expect_match(note, paste0("the central ", level, "% interval of the statistic"), fixed = TRUE)
  expect_match(note, "bold: the observed value lies outside it", fixed = TRUE)
})

test_that("interval labels follow the quantile columns of the pipeline tables instead of a typed 95", {
  dec <- tibble::tibble(
    outcome = "asc_agg", predictor = "zm_power", predicted_sign = "+", estimate = 0.2, q5 = 0.1, q95 = 0.3,
    credible_raw = TRUE, diagnostics_ok = TRUE, credible = TRUE, observed_sign = "+", verdict = "confirmed",
    ess_bulk_min = 10500, ess_tail_min = 10400, rhat_max = 1.001, n_divergent = 0L,
    n_treedepth_hits = 0L, gate_status = "ok"
  )
  expect_equal(rh_ci_cols(dec), list(lo = "q5", hi = "q95", level = 90))
  expect_equal(rh_ci_cols(tibble::tibble(q97.5 = 1, q2.5 = 0)), list(lo = "q2.5", hi = "q97.5", level = 95))
  expect_equal(rh_ci_label(95), "95% CrI")
  expect_equal(rh_cfg_ci_level(analysis_plan), 100 * analysis_plan$regression$ci_level)
  r2 <- tibble::tibble(outcome = "asc_agg", slope_sd = 0.2, role = "primary", family = "gaussian",
                       r2_median = 0.3, r2_lo = 0.25, r2_hi = 0.35,
                       r2_definition = "var(mu) / (var(mu) + sigma^2)")
  r2_tab <- rh_r2_table(r2, labels, engine = "kable", analysis_plan = analysis_plan)
  r2_head <- paste0("Bayesian R² [", rh_fmt(100 * analysis_plan$regression$ci_level, 0), "% CrI]")
  expect_true(r2_head %in% kable_header(r2_tab))
  # the R2 definition is the per-fit formula the target itself records, never typed
  expect_match(kable_text(r2_tab), "var(mu) / (var(mu) + sigma^2)", fixed = TRUE)
  r2_row <- kable_row(r2_tab, c("^Outcome$" = rh_label("asc_agg", labels)))
  expect_equal(kable_cell(r2_row, "^Slope prior SD$"), rh_sweep_sd_label(0.2, analysis_plan))
  # the estimate and its interval share one cell
  expect_equal(kable_cell(r2_row, "^Bayesian R"), rh_fmt_est_ci(0.3, 0.25, 0.35, bounded = TRUE))
  r2_def <- r2
  r2_def$r2_definition <- "var(mu) / (var(mu) + 2 * sigma^2)"
  r2_def$family <- "student"
  tdef <- rh_r2_table(r2_def, labels, engine = "kable", analysis_plan = analysis_plan)
  expect_match(kable_text(tdef), "var(mu) / (var(mu) + 2 * sigma^2)", fixed = TRUE)
  expect_equal(kable_cell(kable_cells(tdef)$rows[[1]], "^Residual distribution$"), "Student-t")
  # rows that share the prior and the residual distribution: those columns leave the table
  two <- dplyr::bind_rows(r2, transform(r2, outcome = "sdo_dom"))
  expect_identical(kable_header(rh_r2_table(two, labels, engine = "kable", analysis_plan = analysis_plan)),
                   c("Outcome", r2_head))
  expect_equal(rh_pretty_names(c("q2.5", "q97.5", "q5", "ci_lo", "slope_sd")), c("2.5%", "97.5%", "5%", "CrI lo", "slope sd"))
})

test_that("rh_coef_plot() labels the axis by coefficient class and reads the level from the columns", {
  coefs <- dplyr::bind_rows(
    coef_fixture_row("asc_agg", "zm_power", "predictor", 5000, 5000, 1.0, 0L, TRUE),
    coef_fixture_row("asc_agg", "gendermale", "covariate", 5000, 5000, 1.0, 0L, TRUE)
  )
  p <- rh_coef_plot(coefs, analysis_plan = analysis_plan, labels = labels)
  expect_s3_class(p, "ggplot")
  expect_match(p$labels$x, "95% CrI", fixed = TRUE)
  expect_match(p$labels$x, "outcome-SD units", fixed = TRUE)
  expect_match(p$labels$x, "metric terms: per predictor SD", fixed = TRUE)
  expect_match(p$labels$x, "gender: difference vs the reference group", fixed = TRUE)
  expect_false(grepl("Standardised partial coefficient", p$labels$x, fixed = TRUE))
  expect_true(all(c("ci_lo", "ci_hi") %in% names(p$data)))
  # the legend names the sweep arms by their configured labels
  sweep_rows <- dplyr::bind_rows(lapply(analysis_plan$priors$slope_sd_sweep, function(sd) {
    coef_fixture_row("asc_agg", "zm_power", "predictor", 5000, 5000, 1.0, 0L, TRUE, slope_sd = sd,
                     role = if (sd == analysis_plan$priors$slope_sd_primary) "primary" else "sweep")
  }))
  ps <- rh_coef_plot(sweep_rows, analysis_plan = analysis_plan, labels = labels)
  expect_equal(levels(ps$data$prior), rh_sweep_sd_label(sort(analysis_plan$priors$slope_sd_sweep), analysis_plan))
  sc <- rh_prior_scale(c(0.1, 0.2, 0.4), 0.2, analysis_plan)
  expect_equal(sc$levels, rh_sweep_sd_label(c(0.1, 0.2, 0.4), analysis_plan))
})

test_that("the software sentence names R, the fitting chain and the other packages", {
  text <- rh_software_text(c(brms = "2.23.0", cmdstanr = "0.9.0", CmdStan = "2.36.0"),
                           packages = character(0), recorded = c(BayesFactor = "0.9.12-4.8"))
  expect_match(text, "^All analyses ran in R [0-9]+[.][0-9]+[.][0-9]+[.] ")
  expect_match(text, paste0("The regressions were fitted with brms 2.23.0 through cmdstanr 0.9.0 and ",
                            "CmdStan 2.36.0; the other analyses used BayesFactor 0.9.12-4.8."), fixed = TRUE)
  expect_match(rh_software_text(c(brms = NA, cmdstanr = "0.9.0", CmdStan = "2.36.0"), packages = character(0)),
               "brms (version not recorded) through cmdstanr 0.9.0 and CmdStan 2.36.0.", fixed = TRUE)
})


test_that("the prior-posterior figure lists the motives and the facets in the codebook's order", {
  # rows of the coefficient table in an order of their own: the figure's facets
  # still follow the configured motives and outcomes, which zm_config() takes
  # from the `order` column of codebook_scales.csv
  motives <- as.character(analysis_plan$regression$motives)
  coefs <- tidyr::expand_grid(outcome = rev(outcomes), term = rev(motives))
  coefs$term_type <- "predictor"
  coefs$estimate <- 0.1
  coefs$sd <- 0.05
  p <- rh_prior_posterior_plot(coefs, analysis_plan, labels)
  expect_identical(levels(p$data$term), rh_label(motives, labels))
  expect_identical(levels(p$data$outcome), rh_label(outcomes, labels))
  expect_identical(levels(p$data$term), c("Security", "Arousal", "Power", "Prestige", "Achievement"))
})
