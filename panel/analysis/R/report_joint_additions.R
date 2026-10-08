# Presentation of approved supplementary quantities derived from paired draws
# of the existing joint regression. Calculations live in their own targets.

#' Join labels as a list: "A", "A and B", "A, B and C"
join_labels_with_and <- function(x) {
  if (length(x) <= 1L) return(paste(x, collapse = ""))
  paste0(paste(x[-length(x)], collapse = ", "), " and ", x[length(x)])
}

#' Require the recorded joint-model gate before interpreting a derived quantity
check_joint_addition_interpretability <- function(result, rows = NULL) {
  if (is.null(rows) && !is.null(result$report_fit_interpretable)) return(result$report_fit_interpretable)
  if (!is.null(rows) && identical(rows, result$summaries) && !is.null(result$report_interpretable)) {
    return(result$report_interpretable)
  }
  if (!is.null(rows) && !is.null(result$report_row_gates)) {
    for (key in names(result$report_row_gates)) {
      if (identical(rows, result[[key]])) return(result$report_row_gates[[key]])
    }
  }
  gate <- isTRUE(result$validity$fit_valid)
  if (!is.null(result$interpretation_allowed)) gate <- gate && isTRUE(result$interpretation_allowed)
  if (!is.null(rows) && "interpretable" %in% names(rows)) {
    gate <- gate && nrow(rows) > 0L && all(rows$interpretable %in% TRUE)
  }
  gate
}

#' Format a posterior median and its central interval without a decision symbol
#'
#' With `gate` `TRUE`, the joint fit passed its validity gate, and a median
#' whose interval excludes zero is set in Markdown bold.
format_joint_addition_interval <- function(rows, prefix, bounded = FALSE, digits = 2L, gate = FALSE) {
  fields <- paste0(prefix, c("_median", "_lower", "_upper"))
  missing <- setdiff(fields, names(rows))
  if (length(missing)) stop("Missing paired-posterior summaries: ", paste(missing, collapse = ", "))
  median <- rows[[fields[1]]]
  lower <- rows[[fields[2]]]
  upper <- rows[[fields[3]]]
  text <- vapply(seq_along(median), function(i) {
    values <- c(median[[i]], lower[[i]], upper[[i]])
    if (any(!is.finite(values))) return("—")
    # Extra decimals are needed only when a wholly positive/negative interval
    # would acquire a zero endpoint, contradicting its prose interpretation.
    precision <- digits
    directional <- values[[2]] > 0 || values[[3]] < 0
    nearest_endpoint <- min(abs(values[2:3]))
    if (directional && round(nearest_endpoint, precision) == 0) {
      precision <- max(precision, ceiling(-log10(nearest_endpoint)))
    }
    # A small median in a zero-spanning interval may round to zero normally;
    # a bound keeps its sign, so "−.00" shows on which side of zero it lies.
    if (round(values[[1]], precision) == 0) values[[1]] <- 0
    shown <- rh_fmt(values[[1]], precision, bounded = bounded)
    if (isTRUE(gate) && directional) shown <- paste0("**", shown, "**")
    paste(shown, rh_fmt_ci(values[[2]], values[[3]], precision, bounded = bounded))
  }, character(1))
  text
}

#' Label intervals by the recorded probability, with the report configuration fallback
label_joint_addition_interval <- function(result, analysis_plan) {
  level <- result$metadata$interval_level
  if (is.null(level)) return(rh_ci_label(rh_cfg_ci_level(analysis_plan)))
  rh_ci_label(100 * level)
}

#' Keep invalid numerical results visible as diagnostic output only
note_joint_addition_validity <- function(result, rows = NULL) {
  if (check_joint_addition_interpretability(result, rows)) return("")
  note <- result$validity$note
  reason <- if (identical(result$validity$gate_status, "unavailable") &&
                length(note) == 1L && !is.na(note) && nzchar(note)) paste0(" ", note) else ""
  if (identical(result$validity$gate_status, "unavailable")) {
    return(paste0(" The joint regression is unavailable; interpretation is withheld.", reason))
  }
  " Diagnostic estimates only: the joint fit did not pass its recorded validity gate; interpretation is withheld."
}

#' Compare the paired zero-order and residual outcome correlations
#'
#' One row per facet pair: the zero-order correlation the joint model implies
#' (named "zero-order (from the joint model)"), the residual correlation of RQ2b and their paired difference.
#' @param result The `joint_correlation_changes` target.
#' @param labels Named labels from [rh_labels()].
#' @param analysis_plan Configuration; supplies the interval label.
#' @param engine See [rh_use_gt()].
#' @return Display table.
build_joint_correlation_changes_table <- function(result, labels = rh_labels(), analysis_plan,
                                                  engine = "auto") {
  labels <- c(asc_total = "ASC total", labels)
  rows <- result$summaries
  gate <- check_joint_addition_interpretability(result, rows)
  display <- tibble::tibble(
    pair = paste(rh_label(rows$outcome_1, labels), "–", rh_label(rows$outcome_2, labels)),
    zero_order = format_joint_addition_interval(rows, "unadjusted", bounded = TRUE, gate = gate),
    residual = format_joint_addition_interval(rows, "residual", bounded = TRUE, gate = gate),
    # A difference of two correlations is written like a correlation residual:
    # two decimals, no leading zero; a bound near zero gains decimals as needed.
    difference = format_joint_addition_interval(rows, "difference", bounded = TRUE, gate = gate)
  )
  rh_table(
    display, engine = engine,
    col_labels = c(pair = "Outcome pair", zero_order = "Zero-order (from the joint model)",
                   residual = "Residual", difference = "Residual − zero-order"),
    source_note = paste0(
      "Posterior median [", label_joint_addition_interval(result, analysis_plan), "]. ",
      "Zero-order correlations from the joint model average over the empirical predictor distribution in the ",
      "joint-model sample. Differences use paired draws from that model; their signs describe signed correlation change. ",
      "Bold estimate: its interval excludes zero.",
      note_joint_addition_validity(result, rows)
    )
  )
}

#' Summarise correlation differences without confusing signed and absolute changes
describe_joint_correlation_changes <- function(result, labels = rh_labels()) {
  labels <- c(asc_total = "ASC total", labels)
  rows <- result$summaries
  if (!check_joint_addition_interpretability(result, rows)) {
    return("The paired correlation differences are diagnostic output only; interpretation is withheld because the joint fit did not pass its recorded validity gate.")
  }
  interval_available <- is.finite(rows$difference_lower) & is.finite(rows$difference_upper)
  positive <- interval_available & rows$difference_lower > 0
  negative <- interval_available & rows$difference_upper < 0
  unresolved <- interval_available & !positive & !negative
  label_pairs <- function(selected) paste(
    paste(rh_label_inline(rows$outcome_1[selected], labels), "–", rh_label_inline(rows$outcome_2[selected], labels)),
    collapse = "; "
  )
  zero_order <- "the zero-order correlation from the joint model"
  sentences <- c(
    if (any(positive)) paste0("The residual correlation was higher than ", zero_order,
                              ", the interval of their difference lying above zero, for ", label_pairs(positive), "."),
    if (any(negative)) paste0(
      if (any(positive)) "It was lower, the interval lying below zero, for " else paste0(
        "The residual correlation was lower than ", zero_order, ", the interval of their difference lying below zero, for "
      ),
      label_pairs(negative), "."
    ),
    if (any(unresolved)) paste0("The interval of their difference included zero for ", label_pairs(unresolved), "."),
    if (any(!interval_available)) "A difference interval was unavailable for one or more pairs."
  )
  if (length(sentences) == 0L) return("The paired correlation-difference intervals were unavailable.")
  paste(sentences, collapse = " ")
}

#' Standardised ASC-total coefficients
build_joint_asc_aggregate_table <- function(result, labels = rh_labels(), analysis_plan,
                                           engine = "auto") {
  rows <- result$aggregate_coefficients
  display <- tibble::tibble(
    predictor = rh_label(rows$predictor_key, labels),
    standardised = format_joint_addition_interval(rows, "b_aggregate", gate = check_joint_addition_interpretability(result))
  )
  rh_table(
    display, engine = engine,
    col_labels = c(predictor = "Predictor", standardised = "Standardised coefficient"),
    source_note = paste0(
      "Posterior median [", label_joint_addition_interval(result, analysis_plan), "]. ",
      "Standardised coefficients use ASC-total SD per predictor SD. Bold estimate: its interval excludes zero.",
      note_joint_addition_validity(result)
    )
  )
}

#' Standardised signed coefficients, paired strength changes, and direction comparisons
build_joint_asc_attenuation_table <- function(result, labels = rh_labels(), analysis_plan,
                                             engine = "auto") {
  rows <- result$facet_comparisons
  display <- tibble::tibble(
    predictor = rh_blank_repeated(rh_label(rows$predictor_key, labels)),
    facet = rh_label(rows$outcome_key, labels),
    facet_coefficient = format_joint_addition_interval(rows, "b_facet"),
    total_coefficient = format_joint_addition_interval(rows, "b_aggregate"),
    attenuation = format_joint_addition_interval(rows, "attenuation"),
    direction = ifelse(is.na(rows$direction_comparison), "Not interpreted", rows$direction_comparison)
  )
  rh_table(
    display, engine = engine,
    col_labels = c(predictor = "Predictor", facet = "Authoritarian facet", facet_coefficient = "Facet coefficient",
                   total_coefficient = "ASC-total coefficient", attenuation = "Strength difference",
                   direction = "Direction comparison"),
    source_note = paste0(
      "Posterior median [", label_joint_addition_interval(result, analysis_plan), "]. ",
      "Coefficients use their respective outcome SD per predictor SD. ",
      "Strength difference compares the absolute standardised facet and ASC-total coefficients within each draw; positive values denote the smaller ASC-total magnitude. Matching credible signs indicate the same direction; opposite credible signs indicate a sign flip; otherwise the direction comparison is unresolved.",
      note_joint_addition_validity(result)
    )
  )
}

#' Describe ASC-total coefficient and attenuation intervals only when the joint gate passed
describe_joint_asc_aggregation <- function(result, labels = rh_labels()) {
  if (!check_joint_addition_interpretability(result)) {
    return("The ASC-total estimates are diagnostic output only; interpretation is withheld because the joint fit did not pass its recorded validity gate.")
  }
  coefficients <- result$aggregate_coefficients
  comparisons <- result$facet_comparisons
  positive <- is.finite(coefficients$b_aggregate_lower) & coefficients$b_aggregate_lower > 0
  negative <- is.finite(coefficients$b_aggregate_upper) & coefficients$b_aggregate_upper < 0
  finite <- is.finite(coefficients$b_aggregate_lower) & is.finite(coefficients$b_aggregate_upper)
  unresolved <- finite & !positive & !negative
  motives <- function(selected) paste(rh_label_inline(coefficients$predictor_key[selected], labels), collapse = ", ")
  pieces <- c(
    if (any(positive)) paste0("The interval of the ASC-total coefficient lay above zero for ", motives(positive), "."),
    if (any(negative)) paste0("The interval of the ASC-total coefficient lay below zero for ", motives(negative), "."),
    if (any(unresolved)) paste0("The interval of the ASC-total coefficient included zero for ", motives(unresolved), "."),
    if (any(!finite)) "An ASC-total coefficient interval was unavailable for one or more motives."
  )
  comparable <- is.finite(comparisons$attenuation_lower) & is.finite(comparisons$attenuation_upper)
  smaller <- comparable & comparisons$attenuation_lower > 0
  larger <- comparable & comparisons$attenuation_upper < 0
  difference_unresolved <- comparable & !smaller & !larger
  pieces <- c(pieces, paste0(
    "Of the ", rh_fmt_count(sum(comparable)), " available paired magnitude comparisons, ",
    rh_fmt_count(sum(smaller)), " had intervals wholly above zero (smaller ASC-total magnitude), ",
    rh_fmt_count(sum(larger)), " wholly below zero (larger ASC-total magnitude), and ",
    rh_fmt_count(sum(difference_unresolved)), " spanning zero."
  ))
  if (any(!comparable)) pieces <- c(pieces, "A magnitude-difference interval was unavailable for one or more comparisons.")
  directions <- comparisons$direction_comparison
  pieces <- c(pieces, paste0(
    "The facet and total coefficients had the same credible direction in ",
    rh_fmt_count(sum(directions %in% "same direction")), " comparisons, opposite credible directions (a sign flip) in ",
    rh_fmt_count(sum(directions %in% "sign flip")), ", and an unresolved direction comparison in ",
    rh_fmt_count(sum(directions %in% "unresolved direction comparison")), "."
  ))
  paste(pieces, collapse = " ")
}

#' The joint model's four facets by scale key, in the codebook's order
#'
#' The joint model names its outcomes aggression, submission, conventionalism
#' and sdo_d; this pairs each name with its scale key and lists them in the
#' order of the configured outcomes, which [zm_config()] takes from the `order`
#' column of codebook_scales.csv.
#'
#' @param analysis_plan Configuration (`regression$outcomes`).
#' @return Named character vector, joint-model name -> scale key.
order_joint_facet_keys <- function(analysis_plan) {
  keys <- c(aggression = "asc_agg", submission = "asc_sub", conventionalism = "asc_conv", sdo_d = "sdo_dom")
  keys[order(match(keys, as.character(analysis_plan$regression$outcomes)))]
}

#' Compact RQ2 strength comparisons: comparison rows and motive columns
#' The motive columns follow the configured motives, in the codebook's order.
build_joint_strength_comparison_table <- function(rows, comparisons, analysis_plan,
                                                  labels = rh_labels(), engine = "auto",
                                                  interpretation_allowed = TRUE, target = NULL) {
  motives <- as.character(analysis_plan$regression$motives)
  display <- data.frame(comparison = comparisons$label, stringsAsFactors = FALSE)
  for (motive in motives) {
    cells <- vapply(seq_len(nrow(comparisons)), function(i) {
      row <- rows[rows$predictor_key == motive & rows$first_outcome == comparisons$first[i] &
                    rows$second_outcome == comparisons$second[i], , drop = FALSE]
      if (nrow(row) != 1L) return("—")
      # a difference of coefficients can exceed one: leading zero (APA)
      format_joint_addition_interval(row, "difference", gate = interpretation_allowed)
    }, character(1))
    display[[motive]] <- cells
  }
  rh_table(display, engine = engine, target = target, markdown = motives,
    col_labels = c(comparison = "Facet comparison", stats::setNames(rh_label(motives, labels), motives)),
    source_note = paste0("Posterior median [", rh_ci_label(rh_cfg_ci_level(analysis_plan)), "]. ",
      "Difference in absolute standardised coefficients: first-named outcome minus second-named outcome. ",
      "Positive values indicate a stronger association with the first outcome; negative values indicate a stronger association with the second. ",
      "Bold estimate: its interval excludes zero. ",
      "All available comparisons are shown.",
      if (!isTRUE(interpretation_allowed)) " Diagnostic estimates only: interpretation is withheld because the joint fit did not pass its validity gate." else ""))
}

#' RQ2a: all six facet comparisons in one matrix
build_joint_facet_strength_table <- function(result, labels = rh_labels(), analysis_plan,
                                             engine = "auto") {
  rows <- result$differences
  rows$predictor_key <- rows$motive_key
  for (statistic in c("median", "lower", "upper")) {
    rows[[paste0("difference_", statistic)]] <- rows[[if (statistic == "median") "posterior_median" else statistic]]
  }
  # the six pairs of facets, in the codebook's order of the facets
  keys <- order_joint_facet_keys(analysis_plan)
  pairs <- utils::combn(names(keys), 2L)
  comparisons <- data.frame(first = pairs[1L, ], second = pairs[2L, ])
  comparisons$label <- paste(rh_label(unname(keys[comparisons$first]), labels), "−",
                             rh_label(unname(keys[comparisons$second]), labels))
  build_joint_strength_comparison_table(rows, comparisons, analysis_plan, labels, engine,
                                        result$gate_passed, target = "report_joint")
}

#' RQ2c: three facet-minus-total comparisons in one matrix
build_joint_total_strength_table <- function(result, labels = rh_labels(), analysis_plan,
                                             engine = "auto") {
  rows <- result$facet_comparisons
  rows$first_outcome <- rows$outcome_key
  rows$second_outcome <- "asc_total"
  for (statistic in c("median", "lower", "upper")) {
    rows[[paste0("difference_", statistic)]] <- rows[[paste0("attenuation_", statistic)]]
  }
  # the facets of the ASC total, in the codebook's order of the facets
  facets <- intersect(as.character(analysis_plan$regression$outcomes), unique(as.character(rows$outcome_key)))
  comparisons <- data.frame(first = facets, second = "asc_total")
  comparisons$label <- paste(rh_label(comparisons$first, labels), "− ASC total")
  build_joint_strength_comparison_table(rows, comparisons, analysis_plan, labels, engine,
                                        check_joint_addition_interpretability(result),
                                        target = "joint_asc_aggregation")
}

#' Concise RQ2c result sentences grounded in paired strength and signed coefficients
#' The motives, and the facets within a sentence, in the codebook's order.
summarise_joint_total_strength <- function(result, labels = rh_labels(), analysis_plan) {
  if (!check_joint_addition_interpretability(result)) return(note_joint_addition_validity(result))
  rows <- result$facet_comparisons
  rows <- rows[order(match(rows$predictor_key, as.character(analysis_plan$regression$motives)),
                     match(rows$outcome_key, as.character(analysis_plan$regression$outcomes))), , drop = FALSE]
  sentences <- vapply(unique(rows$predictor_key), function(motive) {
    selected <- rows[rows$predictor_key == motive, , drop = FALSE]
    stronger <- selected$attenuation_lower > 0
    weaker <- selected$attenuation_upper < 0
    pieces <- c(
      if (any(stronger)) paste0("more strongly associated with ", join_labels_with_and(rh_label_inline(selected$outcome_key[stronger], labels)), " than with the ASC total"),
      if (any(weaker)) paste0("more strongly associated with the ASC total than with ", join_labels_with_and(rh_label_inline(selected$outcome_key[weaker], labels))))
    sentence <- if (length(pieces)) paste0(rh_label(motive, labels), " was ", paste(pieces, collapse = "; it was "), ".") else
      paste0("For ", rh_label_inline(motive, labels), ", all three facet–total strength differences remained unresolved.")
    # direction (AP7): a sign flip is named whatever the strength difference
    flips <- selected[selected$direction_comparison %in% "sign flip", , drop = FALSE]
    if (nrow(flips)) sentence <- paste(sentence, paste(vapply(seq_len(nrow(flips)), function(i) {
      row <- flips[i, ]
      paste0(rh_label(motive, labels), " was ", if (row$b_facet_lower > 0) "positively" else "negatively",
        " associated with ", rh_label_inline(row$outcome_key, labels), " and ",
        if (row$b_aggregate_lower > 0) "positively" else "negatively", " with the ASC total (a sign flip).")
    }, character(1)), collapse = " "))
    sentence
  }, character(1))
  paste(sentences, collapse = " ")
}

#' RQ2a sentences: credible strength differences, then the sign flips (AP7)
summarise_joint_facet_strength <- function(result, labels = rh_labels(), analysis_plan) {
  if (!isTRUE(result$gate_passed)) return("Interpretation is withheld because the joint fit did not pass its validity gate.")
  keys <- order_joint_facet_keys(analysis_plan)
  rows <- result$differences
  # the pairs of facets in the codebook's order of the facets
  rows <- rows[order(match(rows$first_outcome, names(keys)), match(rows$second_outcome, names(keys))), , drop = FALSE]
  sentences <- vapply(as.character(analysis_plan$regression$motives), function(motive) {
    selected <- rows[rows$motive_key == motive & (rows$lower > 0 | rows$upper < 0), , drop = FALSE]
    sentence <- if (!nrow(selected)) paste0("For ", rh_label_inline(motive, labels), ", all six facet strength differences remained unresolved.") else {
      stronger <- ifelse(selected$lower > 0, selected$first_outcome, selected$second_outcome)
      weaker <- ifelse(selected$lower > 0, selected$second_outcome, selected$first_outcome)
      comparisons <- vapply(names(keys)[names(keys) %in% stronger], function(outcome) {
        paste0("with ", rh_label_inline(unname(keys[outcome]), labels), " than with ",
          join_labels_with_and(rh_label_inline(unname(keys[names(keys) %in% weaker[stronger == outcome]]), labels)))
      }, character(1))
      joined <- if (length(comparisons) <= 1L) comparisons else
        paste0(paste(comparisons[-length(comparisons)], collapse = ", "), ", and ", comparisons[length(comparisons)])
      paste0(rh_label(motive, labels), " was more strongly associated ", joined, ".")
    }
    # direction (AP7): a sign flip is named whatever the strength difference
    flips <- rows[rows$motive_key == motive & rows$direction_comparison %in% "sign flip", , drop = FALSE]
    if (nrow(flips)) {
      signs <- result$direction[result$direction$motive_key == motive, , drop = FALSE]
      sign_word <- function(facet) if (signs$lower[signs$outcome == facet][1] > 0) "positively" else "negatively"
      sentence <- paste(sentence, paste(vapply(seq_len(nrow(flips)), function(i) {
        paste0(rh_label(motive, labels), " was ", sign_word(flips$first_outcome[i]), " associated with ",
               rh_label_inline(unname(keys[flips$first_outcome[i]]), labels), " and ", sign_word(flips$second_outcome[i]),
               " with ", rh_label_inline(unname(keys[flips$second_outcome[i]]), labels), " (a sign flip).")
      }, character(1)), collapse = " "))
    }
    sentence
  }, character(1))
  paste(sentences, collapse = " ")
}

#' Record joint-result interpretability and the residual summaries before rendering
add_joint_reporting_facts <- function(result) {
  rows <- result$summaries
  result$report_interpretable <- check_joint_addition_interpretability(result, rows)
  result$report_fit_interpretable <- check_joint_addition_interpretability(result)
  keys <- intersect(c("summaries", "aggregate_coefficients", "facet_comparisons"), names(result))
  result$report_row_gates <- stats::setNames(lapply(keys, function(key) {
    check_joint_addition_interpretability(result, result[[key]])
  }), keys)
  if (all(c("residual_median", "residual_lower", "residual_upper") %in% names(rows))) {
    credible <- is.finite(rows$residual_lower) & is.finite(rows$residual_upper) &
      (rows$residual_lower > 0 | rows$residual_upper < 0)
    total <- rows$outcome_1 %in% "asc_total" | rows$outcome_2 %in% "asc_total"
    with_sdo <- !total & (rows$outcome_1 %in% "sdo_dom" | rows$outcome_2 %in% "sdo_dom")
    within <- !total & !with_sdo
    result$report_residuals <- list(credible = credible, total = total,
      with_sdo = with_sdo, within = within, n = nrow(rows), n_credible = sum(credible),
      all_credible = all(credible), within_range = if (any(within)) range(rows$residual_median[within]) else c(NA_real_, NA_real_),
      with_sdo_range = if (any(with_sdo)) range(rows$residual_median[with_sdo]) else c(NA_real_, NA_real_))
  }
  result
}
