# Network comparison fixtures: frozen text snapshots in the shape the network
# report tables read (tabulate_network_comparison()), built once from
# hand-made inputs: three resamples on three edges (bagged present / absent /
# inconclusive, the full fit inconclusive / absent / present), and the same
# bag with one failed resample.

network_fixture_network_compare <- list(edges = structure(list(node_i = c("zm_security", "zm_security",
"zm_arousal"), node_j = c("zm_arousal", "zm_achievement", "zm_achievement"),
    pip_full = c(0.80000000000000004, 0.02, 0.94999999999999996
    ), bf_full = c(4.0000000000000009, 0.020408163265306124, 
    18.999999999999982), decision_full = c("inconclusive", "absent", 
    "present"), pip_bagged = c(0.94999999999999996, 0.070000000000000007, 
    0.5), bf_bagged = c(18.999999999999982, 0.075268817204301092, 
    1), decision_bagged = c("present", "absent", "inconclusive"
    ), share_above_include = c(0.66666666666666663, 0, 0), share_below_exclude = c(0, 
    0.66666666666666663, 0), pip_resample_q05 = c(0.90500000000000003, 
    0.051000000000000004, 0.41000000000000003), pip_resample_q50 = c(0.94999999999999996, 
    0.059999999999999998, 0.5), pip_resample_q95 = c(0.995, 0.096000000000000002, 
    0.58999999999999997), changed = c(TRUE, FALSE, TRUE), transition = c("inconclusive -> present", 
    "absent -> absent", "present -> inconclusive"), weight_full = c(0.25, 
    0, 0.29999999999999999), weight_bagged = c(0.29999999999999999, 
    0.01, 0.10000000000000001), n_success = c(3L, 3L, 3L)), row.names = c(NA, 
-3L), class = c("tbl_df", "tbl", "data.frame")), transitions = structure(c(0L, 
1L, 0L, 1L, 0L, 0L, 0L, 0L, 1L), dim = c(3L, 3L), dimnames = list(
    full = c("present", "inconclusive", "absent"), bagged = c("present", 
    "inconclusive", "absent")), class = "table"), changed_edges = structure(list(
    node_i = c("zm_arousal", "zm_security"), node_j = c("zm_achievement",
    "zm_arousal"), pip_full = c(0.94999999999999996, 0.80000000000000004
    ), bf_full = c(18.999999999999982, 4.0000000000000009), decision_full = c("present", 
    "inconclusive"), pip_bagged = c(0.5, 0.94999999999999996), 
    bf_bagged = c(1, 18.999999999999982), decision_bagged = c("inconclusive", 
    "present"), share_above_include = c(0, 0.66666666666666663
    ), share_below_exclude = c(0, 0), pip_resample_q05 = c(0.41000000000000003, 
    0.90500000000000003), pip_resample_q50 = c(0.5, 0.94999999999999996
    ), pip_resample_q95 = c(0.58999999999999997, 0.995), changed = c(TRUE, 
    TRUE), transition = c("present -> inconclusive", "inconclusive -> present"
    ), weight_full = c(0.29999999999999999, 0.25), weight_bagged = c(0.10000000000000001, 
    0.29999999999999999), n_success = c(3L, 3L)), row.names = c(NA, 
-2L), class = c("tbl_df", "tbl", "data.frame")), thresholds = structure(list(
    bf_include = 10L, bf_exclude = 0.10000000000000001, pip_include = 0.90909090909090906, 
    pip_exclude = 0.090909090909090912, g_prior = 0.5, n_sweeps = NA_integer_), row.names = c(NA, 
-1L), class = c("tbl_df", "tbl", "data.frame")), feasible = TRUE, 
    n_fits = 3L, n_success_fits = 3L, n_edges = 3L, n_changed = 2L)

network_fixture_network_compare_infeasible <- list(edges = structure(list(node_i = c("zm_security", "zm_security",
"zm_arousal"), node_j = c("zm_arousal", "zm_achievement", "zm_achievement"),
    pip_full = c(0.80000000000000004, 0.02, 0.94999999999999996
    ), bf_full = c(4.0000000000000009, 0.020408163265306124, 
    18.999999999999982), decision_full = c("inconclusive", "absent", 
    "present"), pip_bagged = c(0.92500000000000004, 0.075000000000000011, 
    0.45000000000000001), bf_bagged = c(12.333333333333341, 0.081081081081081086, 
    0.81818181818181812), decision_bagged = c(NA_character_, 
    NA_character_, NA_character_), share_above_include = c(0.5, 
    0, 0), share_below_exclude = c(0, 0.5, 0), pip_resample_q05 = c(0.90250000000000008, 
    0.052500000000000005, 0.40500000000000003), pip_resample_q50 = c(0.92500000000000004, 
    0.075000000000000011, 0.45000000000000001), pip_resample_q95 = c(0.94750000000000001, 
    0.097500000000000003, 0.495), changed = c(NA, NA, NA), transition = c(NA_character_, 
    NA_character_, NA_character_), weight_full = c(0.25, 0, 0.29999999999999999
    ), weight_bagged = c(0.25, -0.0050000000000000001, 0.050000000000000003
    ), n_success = c(2L, 2L, 2L)), row.names = c(NA, -3L), class = c("tbl_df", 
"tbl", "data.frame")), transitions = structure(c(0L, 0L, 0L, 
0L, 0L, 0L, 0L, 0L, 0L), dim = c(3L, 3L), dimnames = list(full = c("present", 
"inconclusive", "absent"), bagged = c("present", "inconclusive", 
"absent")), class = "table"), changed_edges = structure(list(
    node_i = character(0), node_j = character(0), pip_full = numeric(0), 
    bf_full = numeric(0), decision_full = character(0), pip_bagged = numeric(0), 
    bf_bagged = numeric(0), decision_bagged = character(0), share_above_include = numeric(0), 
    share_below_exclude = numeric(0), pip_resample_q05 = numeric(0), 
    pip_resample_q50 = numeric(0), pip_resample_q95 = numeric(0), 
    changed = logical(0), transition = character(0), weight_full = numeric(0), 
    weight_bagged = numeric(0), n_success = integer(0)), row.names = c(NA, 
0L), class = c("tbl_df", "tbl", "data.frame")), thresholds = structure(list(
    bf_include = 10L, bf_exclude = 0.10000000000000001, pip_include = 0.90909090909090906, 
    pip_exclude = 0.090909090909090912, g_prior = 0.5, n_sweeps = NA_integer_), row.names = c(NA, 
-1L), class = c("tbl_df", "tbl", "data.frame")), feasible = FALSE, 
    n_fits = 3L, n_success_fits = 2L, n_edges = 3L, n_changed = 0L)
