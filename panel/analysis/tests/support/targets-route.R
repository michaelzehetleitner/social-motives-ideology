# The preparation route of _targets.R, for tests that need the frames the
# pipeline builds rather than a composition of their own.
#
# The target commands are read from _targets.R itself (make_target_reader() in
# tests/support/target-reader.R), so a test can never keep running a route the
# pipeline has left behind. The four fitting verbs of the AP3 fill are
# deterministic stubs (make_fill_stubs()): no test in this repository fits a
# model to build a frame.
#
# The input is the frame after the intake (ap3_intake_apply()): one row per
# respondent with `respondent_id` and the columns zm_intake_required() names.

tr_route <- function(root, intake_data, analysis_plan, codebook) {
  env <- new.env(parent = globalenv())
  sys.source(file.path(root, "tests", "support", "target-reader.R"), env)
  env$source_pipeline_functions(root)
  reader <- env$make_target_reader(root, c(
    list(analysis_inputs = env$make_analysis_inputs(intake_data, analysis_plan, codebook),
         analysis_plan = analysis_plan, codebook = codebook),
    env$make_fill_stubs()
  ))
  reader$read
}

# The synthetic Qualtrics export after the committed intake decisions, which is
# what the pipeline's `analysis_inputs$data` field holds.
tr_intake <- function(raw, analysis_plan) {
  out <- ap3_intake_apply(raw, ap3_intake_check_approval(raw, analysis_plan, data_source = "synthetic"), analysis_plan)
  # check_study_data_matches_approval() marks the frame the pipeline reads as
  # verified; AP1 then skips the test-row assertion the intake already made.
  attr(out, "intake_verified") <- TRUE
  out
}
