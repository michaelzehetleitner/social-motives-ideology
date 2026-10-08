# The edge-prior sensitivity analysis of the network:
# `ap6_network_prior_sweep_values()` (the sweep list of the plan), the edge
# rule at a fixed inclusion probability across the swept priors, and the
# wiring of the sweep into the S5 target `supplement_network_detail` and the
# report.

local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "config", "analysis_plan.yaml"))) {
    parent <- dirname(dir)
    if (identical(parent, dir)) stop("project root not found from ", getwd())
    dir <- parent
  }
  for (f in c("config.R", "ap8_network_helpers.R", "ap8_network.R")) {
    source(file.path(dir, "R", f), local = FALSE)
  }
  assign("prior_sweep_test_root", dir, envir = .GlobalEnv)
})

root <- prior_sweep_test_root
analysis_plan <- zm_config(profile = "smoke", path = file.path(root, "config", "analysis_plan.yaml"))
net <- analysis_plan$network
plan_sweep <- as.numeric(unlist(net$g_prior_sweep))

# --- fixtures -----------------------------------------------------------------

# an inclusion probability just above the include threshold at the preregistered
# prior: present at 0.25 and 0.5, inconclusive at 0.75
include_odds <- net$bf_include * net$g_prior / (1 - net$g_prior)
pip_near_include <- include_odds / (1 + include_odds) + 0.005

# --- the sweep list of the plan ------------------------------------------------

test_that("ap6_network_prior_sweep_values() reads the configured sweep in its order", {
  expect_equal(ap6_network_prior_sweep_values(analysis_plan), c(0.25, 0.5, 0.75))
  expect_equal(ap6_network_prior_sweep_values(analysis_plan), plan_sweep)
  # the preregistered prior is one of the swept values, and the sweep leaves it alone
  expect_true(analysis_plan$network$g_prior %in% ap6_network_prior_sweep_values(analysis_plan))
  expect_equal(analysis_plan$network$g_prior, 0.5)

  with_sweep <- function(v) {
    c2 <- analysis_plan
    c2$network$g_prior_sweep <- v
    c2
  }
  # the order of the plan is kept, not replaced by a sorted order
  expect_equal(ap6_network_prior_sweep_values(with_sweep(list(0.75, 0.25, 0.5))), c(0.75, 0.25, 0.5))
  # a one-element sweep is a sweep
  expect_equal(ap6_network_prior_sweep_values(with_sweep(list(0.25))), 0.25)
  expect_equal(ap6_network_prior_sweep_values(with_sweep(0.4)), 0.4)
  # an atomic vector is read as well as a YAML list
  expect_equal(ap6_network_prior_sweep_values(with_sweep(c(0.2, 0.8))), c(0.2, 0.8))
})

# A malformed sweep — an absent or empty list, or an element that is not a
# prior — is refused where the plan is loaded (zm_config(); test-config.R), so
# ap6_network_prior_sweep_values() is a plain reader of the checked list.

# --- the rule: thresholds fixed, Bayes factor moving ---------------------------

# The preregistered edge rule as the RQ3 route applies it: present at BF >= bf_include,
# absent at BF <= bf_exclude, inconclusive between.
decide_edges <- function(bf, net) {
  extract_network_edge_decisions(tibble::tibble(bf = bf, weight = 0), list(settings = net))$decision
}

test_that("at a fixed inclusion probability the Bayes factor falls as the prior rises", {
  priors <- c(0.25, 0.5, 0.75)
  bf <- vapply(priors, function(p) ap6_network_bf(0.9, p), numeric(1))
  expect_equal(bf, (0.9 / 0.1) / (priors / (1 - priors)))
  expect_true(all(diff(bf) < 0))
  # and so an edge near the include threshold changes decision although the
  # thresholds themselves never move
  dec <- decide_edges(vapply(priors, function(p) ap6_network_bf(pip_near_include, p), numeric(1)), net)
  expect_equal(dec, c("present", "present", "inconclusive"))
})

# --- the wiring ----------------------------------------------------------------

test_that("the edge-prior sweep is wired into the pipeline map and the report", {
  manifest <- withr::with_dir(
    root,
    withr::with_envvar(
      c(ZM_PROFILE = "smoke", ZM_DATA = "synthetic"),
      withCallingHandlers(
        targets::tar_manifest(callr_function = NULL, envir = new.env(parent = globalenv())),
        warning = function(w) {
          if (grepl("built under R version|unter R Version", conditionMessage(w))) {
            invokeRestart("muffleWarning")
          }
        }
      )
    )
  )
  expect_true("supplement_network_detail" %in% manifest$name)
  cmd <- gsub("\\s+", " ", manifest$command[manifest$name == "supplement_network_detail"])
  # the S5 target reads the sweep and builds its table
  expect_match(cmd, "network_prior_sensitivity", fixed = TRUE)
  s5_source <- paste(readLines(file.path(root, "R", "report_supplement_network_detail.R"), warn = FALSE),
                     collapse = "\n")
  expect_match(s5_source, "tabulate_network_prior_sweep(network_prior_sensitivity", fixed = TRUE)

  text <- paste(readLines(file.path(root, "report", "results_draft.qmd"), warn = FALSE), collapse = "\n")
  expect_match(text, "read_report_input(supplement_network_detail, report_inputs)", fixed = TRUE)
  expect_match(text, "s5_value$prior_sweep", fixed = TRUE)
  # the manuscript draft states the sweep in words, without a display of its own
  draft_flat <- gsub("\\s+", " ", text)
  expect_match(draft_flat, "edge_prior_moving_text", fixed = TRUE)
  expect_match(draft_flat, "unbagged full-data refits", fixed = TRUE)
  expect_match(draft_flat, "Stability of the bagged classifications across edge priors is not evaluated", fixed = TRUE)
  expect_match(draft_flat, "the reported classifications are the bagged ones at", fixed = TRUE)
})
