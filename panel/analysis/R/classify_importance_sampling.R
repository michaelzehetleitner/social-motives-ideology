# PSIS reliability rule: https://mc-stan.org/loo/reference/pareto-k-diagnostic.html
# This labels a sensitivity approximation, never the primary model fit.
calculate_importance_sampling_threshold <- function(n_draws) {
  if (length(n_draws) != 1L || !is.finite(n_draws) || n_draws <= 1) return(NA_real_)
  min(0.7, 1 - 1 / log10(n_draws))
}

classify_importance_sampling <- function(pareto_k, n_draws) {
  threshold <- calculate_importance_sampling_threshold(n_draws)
  if (length(pareto_k) != 1L || !is.finite(pareto_k) || !is.finite(threshold)) return("unavailable")
  if (pareto_k < threshold) "reliable" else "unreliable"
}

# Both the original fit and the power-scaling approximation must be reliable.
# Missing validity metadata never implies a passing fit.
can_interpret_power_scaling <- function(fit_valid, fit_gate_status, importance_sampling_status) {
  fit_valid %in% TRUE & fit_gate_status %in% c("ok", "retried_ok") &
    importance_sampling_status %in% "reliable"
}

describe_power_scaling_diagnosis <- function(diagnosis, fit_valid, fit_gate_status, importance_sampling_status) {
  n <- length(diagnosis)
  fit_valid <- rep_len(fit_valid, n)
  fit_gate_status <- rep_len(fit_gate_status, n)
  importance_sampling_status <- rep_len(importance_sampling_status, n)
  dplyr::case_when(
    is.na(fit_valid) | is.na(fit_gate_status) | fit_gate_status %in% "unavailable" ~ "not interpretable: original fit validity unavailable",
    !fit_valid %in% TRUE | !fit_gate_status %in% c("ok", "retried_ok") ~ "not interpretable: original fit failed validity gate",
    importance_sampling_status %in% "unreliable" ~ "not interpretable: importance sampling unreliable",
    !importance_sampling_status %in% "reliable" ~ "not interpretable: importance sampling unavailable",
    TRUE ~ as.character(diagnosis)
  )
}
