# Present already-saved fit and network summaries. These helpers do not fit
# models, inspect posterior draws, or recalculate diagnostic classifications.

#' Collect the recorded joint, motive-model and available Student-t fit diagnostics
#' @param regression_fit_register Saved regression result records.
#' @param joint_regression_fit Saved list containing the joint brmsfit.
#' @param motive_model_fit Saved list containing the joint model of the five
#'   motive scores (`motives`, target `motive_model_fit`), or `NULL`.
#' @return Numeric diagnostic rows with recorded gate outcomes; `kind` names
#'   the model: `"joint"` (RQ2), `"motives"` (RQ1) or `"student_refit"`.
#'   Untriggered Student-t outcomes are an attribute, never fabricated
#'   diagnostic rows.
collect_saved_additional_diagnostics <- function(regression_fit_register, joint_regression_fit,
                                                 motive_model_fit = NULL) {
  fields <- c("ess_bulk_min", "ess_tail_min", "rhat_max", "n_divergent",
              "n_treedepth_hits", "bfmi_min", "mcse_median_max", "mcse_q_max")
  one <- function(model, outcome, diagnostics, gate_status, kind) {
    row <- tibble::tibble(model = model, outcome = outcome, kind = kind,
                          gate_status = if (length(gate_status)) as.character(gate_status)[1] else NA_character_)
    for (field in fields) {
      row[[field]] <- if (!is.null(diagnostics) && field %in% names(diagnostics) && nrow(diagnostics)) {
        as.numeric(diagnostics[[field]][1])
      } else NA_real_
    }
    row$gate_passed <- if (!is.null(diagnostics) && "ok" %in% names(diagnostics) && nrow(diagnostics)) {
      as.logical(diagnostics$ok[1])
    } else NA
    row
  }
  joint <- joint_regression_fit$joint
  joint_row <- one("Joint Gaussian regression", NA_character_, attr(joint, "diagnostics", exact = TRUE),
                    attr(joint, "gate_status", exact = TRUE), "joint")
  motives <- motive_model_fit$motives
  motive_rows <- if (!is.null(motive_model_fit)) list(
    one("Joint model of the five motives", NA_character_, attr(motives, "diagnostics", exact = TRUE),
        attr(motives, "gate_status", exact = TRUE), "motives")
  )
  records <- Filter(function(x) identical(x$role, "student_refit"), regression_fit_register)
  available <- Filter(function(x) isTRUE(x$fit_available), records)
  student_rows <- lapply(available, function(x) {
    one("Student-t refit", x$outcome_key, x$diagnostics, x$gate_status, "student_refit")
  })
  out <- dplyr::bind_rows(c(list(joint_row), motive_rows, student_rows))
  attr(out, "student_not_fitted") <- vapply(Filter(function(x) !isTRUE(x$fit_available), records),
                                             function(x) x$outcome_key, character(1))
  out
}

#' The R² of the refits allowing heavier tails beside the normal model
#'
#' @param r2_summaries Saved regression_r2_summaries target.
#' @return Tibble `outcome_key`, `student` and `gaussian` (rows of the summary
#'   with `r2_median`, `r2_lo`, `r2_hi`), `nu_fixed`; one row per fitted refit.
collect_saved_student_r2 <- function(r2_summaries) {
  student <- r2_summaries[r2_summaries$role %in% "student_refit" & r2_summaries$fit_available %in% TRUE, , drop = FALSE]
  primary <- r2_summaries[r2_summaries$role %in% "primary" & r2_summaries$family %in% "gaussian", , drop = FALSE]
  primary <- primary[match(student$outcome_key, primary$outcome_key), , drop = FALSE]
  list(student = student, gaussian = primary)
}

#' Display saved Student-t R² beside the matching primary Gaussian summary
#'
#' A table only where it helps: with one refit the
#' sentence of [describe_saved_student_r2()] carries the two values.
#'
#' @param r2_summaries Saved regression_r2_summaries target.
#' @param analysis_plan Saved analysis configuration, for the interval level.
#' @param labels Display labels from [rh_labels()].
#' @param engine Table engine for [rh_table()].
#' @return Display table, or `NULL` with fewer than two refits.
build_saved_student_r2_table <- function(r2_summaries, analysis_plan, labels = rh_labels(), engine = "auto") {
  pair <- collect_saved_student_r2(r2_summaries)
  student <- pair$student
  primary <- pair$gaussian
  if (nrow(student) < 2L) return(NULL)
  text <- function(x) rh_fmt_est_ci(x$r2_median, x$r2_lo, x$r2_hi, bounded = TRUE)
  table <- tibble::tibble(outcome = rh_label(student$outcome_key, labels),
                          gaussian = text(primary), student = text(student))
  interval <- rh_ci_label(rh_cfg_ci_level(analysis_plan))
  definition <- "R² = Var(μ) / [Var(μ) + residual variance], where μ is the fitted conditional mean."
  scale <- "Residual variance is σ² for the normal model and [ν / (ν − 2)]σ² for the Student-t model; Student-t σ is a scale parameter, not the residual SD."
  finite_nu <- sort(unique(student$nu_fixed[is.finite(student$nu_fixed) & student$nu_fixed > 2]))
  factor <- if (length(finite_nu)) paste0(" The degrees of freedom ν are fixed at ", paste(rh_fmt_n(finite_nu), collapse = ", "),
    ", so the Student-t variance multiplier is ", paste(rh_fmt(finite_nu / (finite_nu - 2), 2), collapse = ", "), ".") else ""
  rh_table(table, col_labels = c(outcome = "Outcome", gaussian = paste0("Normal model [", interval, "]"),
                                  student = paste0("Model allowing heavier tails (Student-t) [", interval, "]")),
             engine = engine, source_note = paste0("Posterior median [", interval, "] of the Bayesian R². ", definition, " ",
                                                   scale, factor))
}

#' The R² of a single refit allowing heavier tails, in one sentence
#'
#' @inheritParams build_saved_student_r2_table
#' @param table_ref Reference to the table used when there are several refits.
#' @return One sentence.
describe_saved_student_r2 <- function(r2_summaries, labels = rh_labels(), table_ref = "the table") {
  pair <- collect_saved_student_r2(r2_summaries)
  student <- pair$student
  primary <- pair$gaussian
  if (nrow(student) == 0L) return("No refit allowing heavier tails was fitted, so no explained variance is compared.")
  if (nrow(student) > 1L) {
    return(paste0(table_ref, " sets the explained variance of each refit allowing heavier tails beside that of the normal model."))
  }
  paste0(
    "For ", rh_label_inline(student$outcome_key, labels), ", the refit allowing heavier tails explained a share of ",
    rh_fmt_est_ci(student$r2_median, student$r2_lo, student$r2_hi, bounded = TRUE), " of the variance (Bayesian R², ",
    "posterior median with its credible interval), the normal model ",
    rh_fmt_est_ci(primary$r2_median, primary$r2_lo, primary$r2_hi, bounded = TRUE),
    "; the residual variance of the Student-t model is its scale σ² times ν / (ν − 2), with ν fixed at ",
    rh_fmt_n(student$nu_fixed), "."
  )
}

#' Display every edge of the edge-prior sweep, one row per edge
#'
#' The full-data network, the network fitted once to the whole sample, refitted
#' at every prior inclusion probability of the sweep (AP8: the Bayes factor and
#' the decision of every edge at each prior); one column per prior, each cell
#' the decision with its inclusion Bayes factor. The bagged network's
#' decisions are in the network tables of the Results.
#'
#' @param prior_sweep Saved supplement_network_detail$prior_sweep.
#' @param labels Display labels from [rh_labels()].
#' @param engine Table engine for [rh_table()].
build_saved_network_prior_sweep_table <- function(prior_sweep, labels = rh_labels(), engine = "auto") {
  p <- prior_sweep
  sweeps <- attr(p, "n_sweeps")
  if (is.null(sweeps)) sweeps <- NA_integer_
  priors <- sort(unique(p$prior[!is.na(p$prior)]))
  key <- paste(p$node_i, p$node_j)
  edges <- unique(key)
  first <- match(edges, key)
  table <- tibble::tibble(edge = rh_network_edge_label(p$node_i[first], p$node_j[first], labels))
  heads <- c(edge = "Edge")
  for (k in seq_along(priors)) {
    rows <- p[p$prior == priors[[k]], , drop = FALSE]
    m <- match(edges, paste(rows$node_i, rows$node_j))
    decision <- ifelse(is.na(rows$decision[m]), "not recorded", gsub("_", " ", rows$decision[m]))
    bf <- vapply(m, function(i) if (is.na(i)) "—" else rh_network_bf(rows$bf[[i]], sweeps, rows$prior[[i]],
      bounds = attr(p, "bf_bounds_by_prior")[[as.character(rows$prior[[i]])]]), character(1))
    col <- paste0("prior_", k)
    table[[col]] <- paste0(decision, " (", bf, ")")
    heads[col] <- paste0("Prior inclusion probability ", rh_network_prior_label(priors[[k]]))
  }
  # One fit per cell: the resolution of its Bayes factor is one fit's retained
  # sampler steps at the cell's prior (rh_network_bf()); the ladder sentence is
  # the same at every prior.
  prior <- attr(p, "g_prior")
  if (is.null(prior)) prior <- p$prior[!is.na(p$prior)][1]
  ladder <- rh_network_bf_bound_note(p$bf, sweeps, prior, bounds = attr(p, "bf_bounds"))
  note <- paste0(
    "Each cell: the decision of the full-data network, the network fitted once to the whole sample, at that ",
    "prior probability that an edge exists, with its inclusion Bayes factor in parentheses. The bagged network's ",
    "decisions at the preregistered ", rh_network_prior_label(prior), " are in the three network tables of the Results.",
    if (nzchar(ladder)) paste0(" ", ladder) else ""
  )
  rh_table(table, col_labels = heads, engine = engine, source_note = note)
}
