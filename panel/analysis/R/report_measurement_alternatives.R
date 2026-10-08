# Compact descriptive SRQ1 answer from the saved correspondence overview.
# No factor-count choice, fit, item reassignment or acceptance cutoff is added.

#' Describe grouping in the alternative factor-count solutions
#'
#' @param overview The correspondence overview returned by
#'   tabulate_efa_correspondence(), including the expected-count rows.
#' @return One sentence, without a table reference. Unavailable or
#'   diagnostic-only results are counted separately, never read as agreement.
#'   How clearly the items load in each solution is described by
#'   [describe_loading_clarity()].
describe_alternative_factor_correspondence <- function(overview) {
  required <- c("set_key", "factors", "solution", "expected_pair_retention",
                "empirical_pair_purity")
  if (!is.data.frame(overview) || !nrow(overview) ||
      !all(required %in% names(overview))) {
    return("Alternative-solution correspondence was unavailable.")
  }
  expected <- !is.na(overview$solution) & overview$solution == "expected count"
  alternatives <- overview[!expected, , drop = FALSE]
  if (!nrow(alternatives)) return("No alternative factor-count solution was recorded.")

  count <- function(n) {
    words <- c("zero", "one", "two", "three", "four", "five", "six", "seven",
               "eight", "nine", "ten", "eleven", "twelve")
    if (n <= 12L) words[n + 1L] else as.character(n)
  }
  expected_counts <- vapply(as.character(alternatives$set_key), function(key) {
    values <- overview$factors[expected & !is.na(overview$set_key) & overview$set_key == key]
    values <- unique(values[is.finite(values) & values > 0 & values == floor(values)])
    if (length(values) == 1L) values else NA_real_
  }, numeric(1))
  factors <- alternatives$factors
  known_counts <- is.finite(factors) & factors > 0 & factors == floor(factors) &
    is.finite(expected_counts)
  lower <- all(known_counts) && all(factors < expected_counts)
  higher <- all(known_counts) && all(factors > expected_counts)
  description <- if (lower) "lower-factor" else if (higher) "higher-factor" else "alternative factor-count"

  retained <- alternatives$expected_pair_retention
  purity <- alternatives$empirical_pair_purity
  available <- known_counts & !is.na(alternatives$solution) &
    is.finite(retained) & retained >= 0 & retained <= 1 &
    is.finite(purity) & purity >= 0 & purity <= 1
  # These fields are optional in the overview. If supplied, respect their
  # recorded status even when stale numerical summaries are also present.
  for (field in intersect(c("success", "fit_valid", "converged", "admissible", "interpretable"), names(alternatives))) {
    available <- available & alternatives[[field]] %in% TRUE
  }
  if ("diagnostic_only" %in% names(alternatives)) {
    available <- available & alternatives$diagnostic_only %in% FALSE
  }
  if ("status" %in% names(alternatives)) {
    status <- tolower(trimws(as.character(alternatives$status)))
    available <- available & status %in% c("ok", "success", "successful", "available", "converged", "admissible", "valid")
  }
  if ("note" %in% names(alternatives)) {
    diagnostic_note <- !is.na(alternatives$note) & grepl(
      "unavailable|fail|non.?converg|not converg|inadmiss|diagnostic.only|error",
      as.character(alternatives$note), ignore.case = TRUE
    )
    available <- available & !diagnostic_note
  }

  grouping <- rep("unavailable", nrow(alternatives))
  grouping[available & retained == 1 & purity < 1] <- "merged"
  grouping[available & retained < 1 & purity == 1] <- "split"
  grouping[available & retained < 1 & purity < 1] <- "both"
  grouping[available & retained == 1 & purity == 1] <- "unchanged"
  descriptions <- c(
    merged = "merged whole scales without splitting their items",
    split = "split scales without merging items from different scales",
    both = "combined splitting and merging",
    unchanged = "retained the theoretical item grouping",
    unavailable = "had unavailable or diagnostic-only correspondence"
  )
  totals <- table(factor(grouping, levels = names(descriptions)))
  present <- names(totals)[totals > 0L]
  n <- nrow(alternatives)
  first <- if (length(present) == 1L) {
    paste0(if (n == 1L) "The " else paste0("All ", count(n), " "), description,
           if (n == 1L) " solution " else " solutions ", descriptions[[present]], ".")
  } else {
    paste0("Among the ", count(n), " ", description, " solutions, ",
           paste(paste(vapply(as.integer(totals[present]), count, character(1)),
                       unname(descriptions[present])), collapse = "; "), ".")
  }
  first
}

#' Describe the loading clarity of every examined solution
#'
#' SRQ1 counts the items with two or more absolute loadings at the threshold
#' (cross-loading) and reports "their frequency and secondary factor
#' associations", together with the items with no such loading (weakly
#' loading). For every examined solution this names the weakly loading items
#' by scale and each cross-loading item with the scales of the factor it also
#' loads on; where no item cross-loads in a solution with several factors, it
#' names the largest second loading instead, the item closest to
#' cross-loading. Solutions with several factors also name the scales that
#' share a factor. The nine scales analysed on their own get one clause.
#'
#' @param correspondence The correspondence of [tabulate_efa_correspondence()]:
#'   `overview`, `scales` and `membership` (with the secondary-factor columns).
#' @param labels Named labels from [rh_labels()].
#' @param threshold The loading threshold of loading clarity.
#' @return One paragraph.
describe_loading_clarity <- function(correspondence, labels, threshold) {
  overview <- correspondence$overview
  scales <- correspondence$scales
  membership <- correspondence$membership
  at <- rh_fmt(threshold, 2, bounded = TRUE)
  inline <- function(keys) rh_label_inline(keys, labels)
  join <- function(x) {
    if (length(x) <= 1L) return(paste(x, collapse = ""))
    paste0(paste(x[-length(x)], collapse = ", "), " and ", x[length(x)])
  }
  scale_list <- function(joined) join(inline(strsplit(joined, ";", fixed = TRUE)[[1]]))
  factor_of <- function(joined, factor) {
    ifelse(is.na(joined), paste("factor", factor), vapply(joined, function(j) {
      if (is.na(j)) "" else paste("the factor of", scale_list(j))
    }, "", USE.NAMES = FALSE))
  }
  item_name <- function(scale, number) paste(inline(scale), "item", number)
  capitalise <- function(x) paste0(toupper(substr(x, 1, 1)), substring(x, 2))
  sentences <- paste0("An item loads weakly when none of its absolute loadings reaches ", at,
                      ", and it cross-loads when two or more do.")

  if (is.data.frame(scales) && nrow(scales) > 0L) {
    weak <- scales$n_weak
    sentences <- c(sentences, if (anyNA(weak)) {
      "Loading clarity was unavailable for at least one scale analysed on its own."
    } else if (all(weak == 0)) {
      paste0("In the one-factor solution of each of the ", rh_fmt_count(nrow(scales)),
             " scales analysed on its own, no item loaded weakly; with one factor, no item can cross-load.")
    } else {
      w <- scales[scales$n_weak > 0, , drop = FALSE]
      paste0("In the one-factor solutions of the scales analysed on their own, ",
             join(paste0(rh_fmt_n(w$n_weak), " of the ", rh_fmt_n(w$n_items), " ", inline(w$scale_key), " items")),
             " loaded weakly; with one factor, no item can cross-load.")
    })
  }

  solution_sentence <- function(row, m) {
    k <- as.integer(row$factors)
    head <- paste0(row$set_label, ", ", rh_fmt_count(k), if (k == 1L) " factor" else " factors",
                   if (identical(row$solution, "expected count")) " (expected)" else "")
    if (is.null(m) || nrow(m) == 0L) return(paste0(head, ": the loadings were unavailable."))
    parts <- character()
    if (k > 1L && "assigned_scales" %in% names(m)) {
      shared <- unique(m$assigned_scales[!is.na(m$assigned_scales) & grepl(";", m$assigned_scales)])
      if (length(shared)) {
        others <- if (length(shared) > 1L) {
          paste0(", as did ", vapply(shared[-1], scale_list, "", USE.NAMES = FALSE), collapse = "")
        } else ""
        parts <- c(parts, paste0(scale_list(shared[[1]]), " shared a factor", others))
      }
    }
    weak <- m[m$weak %in% TRUE, , drop = FALSE]
    cross <- m[m$crossloading %in% TRUE, , drop = FALSE]
    if (nrow(weak) == 0L && (k == 1L || nrow(cross) == 0L)) {
      parts <- c(parts, if (k == 1L) "no item loaded weakly" else "no item loaded weakly or cross-loaded")
    } else {
      if (nrow(weak) == 0L) {
        parts <- c(parts, "no item loaded weakly")
      } else {
        by_scale <- table(factor(weak$intended_scale, levels = unique(m$intended_scale)))
        by_scale <- by_scale[by_scale > 0L]
        parts <- c(parts, paste0(
          rh_fmt_n(nrow(weak)), " of the ", rh_fmt_n(nrow(m)), " items loaded weakly",
          if (length(by_scale) == 1L) paste0(", all of them ", inline(names(by_scale)), " items") else
            paste0(": ", join(paste(rh_fmt_n(as.integer(by_scale)), inline(names(by_scale)))), " items")
        ))
      }
      if (k > 1L) {
        # each cross-loading item with its second loading and the scales of the
        # factor it lies on: the frequency and the secondary factor association
        # the preregistration asks for (SRQ1)
        parts <- c(parts, if (nrow(cross) == 0L) "none cross-loaded" else paste0(
          rh_fmt_count(nrow(cross)), if (nrow(cross) == 1L) " item cross-loaded: " else " items cross-loaded: ",
          join(paste0(item_name(cross$intended_scale, cross$item_number), ", with a second loading of ",
                      rh_fmt(cross$next_largest_abs_loading, 2, bounded = TRUE), " on ",
                      factor_of(cross$crossloading_scales, cross$crossloading_factors)))
        ))
      }
    }
    if (k > 1L && nrow(cross) == 0L && "next_factor" %in% names(m)) {
      second <- m$next_largest_abs_loading
      if (any(is.finite(second))) {
        j <- which.max(replace(second, !is.finite(second), -Inf))
        parts <- c(parts, paste0(
          "the largest second loading was ", rh_fmt(second[[j]], 2, bounded = TRUE), " (",
          item_name(m$intended_scale[[j]], m$item_number[[j]]), ", on ",
          factor_of(m$next_factor_scales[[j]], m$next_factor[[j]]), ")"
        ))
      }
    }
    paste0(head, ": ", paste(parts, collapse = "; "), ".")
  }

  if (is.data.frame(overview) && nrow(overview) > 0L) {
    for (i in seq_len(nrow(overview))) {
      row <- overview[i, , drop = FALSE]
      sentences <- c(sentences, solution_sentence(row, membership[[paste0(row$set_key, ":", row$factors)]]))
    }
  }
  paste(sentences, collapse = " ")
}
