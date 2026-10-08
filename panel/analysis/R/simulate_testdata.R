# Synthetic raw export generated from config/simulation_truth.yaml
#
# simulate_zm_panel() turns the declared ground truth (latent correlations,
# true partial coefficients, measurement model, exclusion counts) into a
# tibble that has the exact shape of the Qualtrics export (97 columns of
# zm_raw_schema()). Everything the pipeline should recover is stated in the
# truth file; this file only realises it. Sample moments are made exact where
# the truth states a number (latent correlations, partial coefficients,
# residual correlations), so that recovery tests are deterministic instead
# of subject to sampling error. Realism parameters that the truth file does
# not state (party sympathy means, vote shares, Bundesland shares, comment
# texts, timing) live in small helper tables below and never enter any test
# of the analysis code.

#' Draw a matrix with exact sample covariance
#'
#' Standard-normal draws are centred, whitened with the Cholesky factor of
#' their own sample covariance and then rotated into `R`, so that
#' `cov(result)` equals `R` exactly and every column has mean 0.
#'
#' @param n Number of rows.
#' @param R Target covariance (correlation) matrix with dimnames.
#' @return Numeric matrix `n x ncol(R)` with `colnames(R)`.
sim_exact_mvn <- function(n, R) {
  sim_exact_cov(matrix(stats::rnorm(n * ncol(R)), n, ncol(R)), R)
}

#' Rotate a matrix so that its sample covariance equals `R` exactly
#'
#' @param M Numeric matrix (rows = cases).
#' @param R Target covariance matrix with dimnames.
#' @return Matrix with the same rows, mean 0 per column, `cov() == R`.
sim_exact_cov <- function(M, R) {
  M <- scale(M, center = TRUE, scale = FALSE)
  S <- crossprod(M) / (nrow(M) - 1)
  W <- M %*% solve(chol(S))
  out <- W %*% chol(R)
  colnames(out) <- colnames(R)
  out
}

#' Resolve one scenario of the simulation truth
#'
#' The truth file declares one base truth (the `clean` scenario: everything
#' outside the `scenarios:` block) and, per scenario, the deviations from it.
#' This function returns the truth *as that scenario generates it*: the base
#' truth with the scenario's `true_beta` cells overridden and its fault blocks
#' (`missingness`, `cross_loadings`, `residual_contamination`) attached at the
#' top level, where [simulate_zm_panel()] reads them. `clean` attaches none, so
#' the clean export is exactly the base truth.
#'
#' `truth$scenarios$export` names the scenario that generates the export in
#' `data/synthetic/`, i.e. the default here and in
#' `scripts/make_synthetic_data.R`. The prior-recovery simulation
#' (`R/prior_recovery.R`) does **not** call this function: it reads the base
#' `true_beta`, so the AP6 numbers (Justification of Prior Choice) stay
#' anchored to the clean truth whatever the export scenario is.
#'
#' @param truth Ground truth from [zm_truth()], unresolved.
#' @param scenario Scenario name; `NULL` takes `truth$scenarios$export`.
#' @return `truth` with `scenario`, `scenario_description` and the scenario's
#'   fault blocks set; resolving an already resolved truth returns it
#'   unchanged.
zm_truth_scenario <- function(truth, scenario = NULL) {
  if (is.null(truth$scenarios)) {
    stop("zm_truth_scenario(): simulation_truth.yaml carries no `scenarios:` block.")
  }
  # `[[` and not `$`: before the field is set, `truth$scenario` partial-matches
  # `truth$scenarios` and the guard would fire on every unresolved truth.
  resolved <- truth[["scenario"]]
  if (!is.null(resolved)) {
    if (is.null(scenario) || identical(as.character(scenario), resolved)) return(truth)
    stop("zm_truth_scenario(): the truth is already resolved to scenario '", resolved,
         "'; resolve '", scenario, "' from the unresolved truth instead.")
  }
  sc <- truth$scenarios
  known <- setdiff(names(sc), "export")
  if (is.null(scenario)) scenario <- sc$export
  scenario <- as.character(scenario)
  if (length(scenario) != 1L || is.na(scenario) || !scenario %in% known) {
    stop("zm_truth_scenario(): unknown scenario '", paste(scenario, collapse = ", "),
         "'; simulation_truth.yaml declares: ", paste(known, collapse = ", "), ".")
  }
  def <- sc[[scenario]]
  ov <- def$true_beta_overrides
  for (o in names(ov)) {
    if (is.null(truth$true_beta[[o]])) {
      stop("zm_truth_scenario(): scenario '", scenario, "' overrides an outcome absent from true_beta: ", o, ".")
    }
    for (p in names(ov[[o]])) {
      if (is.null(truth$true_beta[[o]][[p]])) {
        stop("zm_truth_scenario(): scenario '", scenario, "' overrides an absent cell: ", o, " ~ ", p, ".")
      }
      truth$true_beta[[o]][[p]] <- as.numeric(ov[[o]][[p]])
    }
  }
  truth$missingness <- def$missingness
  truth$cross_loadings <- def$cross_loadings
  truth$residual_contamination <- def$residual_contamination
  truth$verdicts <- def$verdicts
  truth$scenario <- scenario
  truth$scenario_description <- def$description
  truth
}

#' Evaluate an expression under an explicit seed, restoring the RNG afterwards
#'
#' The fault draws of a scenario (which residuals are contaminated, which cells
#' are blanked) must not shift the main random stream: with them the `clean`
#' and `trouble` exports would differ in every later draw and no longer be
#' comparable. Each fault therefore draws under its own declared seed offset
#' and hands the stream back untouched.
#'
#' @param seed Integer seed.
#' @param expr Expression to evaluate.
#' @return The value of `expr`.
sim_with_seed <- function(seed, expr) {
  had <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  old <- if (had) get(".Random.seed", envir = globalenv(), inherits = FALSE) else NULL
  on.exit({
    if (had) assign(".Random.seed", old, envir = globalenv())
    else suppressWarnings(rm(".Random.seed", envir = globalenv()))
  }, add = TRUE)
  set.seed(as.integer(seed))
  force(expr)
}

# The order in which the generator draws the item noise, scale by scale. It is
# the generator's own and stays fixed, so that the synthetic export on disk is
# reproduced from its seed; it is not an order of the scales for the analyses
# or the reports, which take theirs from the `order` column of
# codebook_scales.csv.
SIM_SCALE_DRAW_ORDER <- c("zm_achievement", "zm_security", "zm_power", "zm_prestige", "zm_arousal",
                          "asc_agg", "asc_sub", "asc_conv", "sdo_dom")

#' Items of every scale from the latent scores (the measurement model)
#'
#' The measurement model of the panel export ([simulate_zm_panel()]). Each item is
#' `loading * latent + sqrt(1 - loading^2) * noise`, shifted by the scale's
#' location, scaled by the spread, rounded and clamped to the response range;
#' a reverse-keyed item is then stored as displayed (`(min + max) - x`), like
#' the live survey. `truth$cross_loadings` adds a second loading on another
#' latent for the named item and rescales that item to unit variance, so it
#' keeps the spread of its block and consumes exactly the same one random draw
#' as every other item.
#'
#' @param latent Numeric matrix of latent scores, one column per scale key.
#' @param codebook Codebook from [zm_codebook()].
#' @param truth Resolved ground truth (`measurement`, `cross_loadings`).
#' @param analysis_plan Configuration from [zm_config()] (response range).
#' @return Named list, one numeric vector per item code, in the generator's
#'   draw order (`SIM_SCALE_DRAW_ORDER`).
sim_items_from_latent <- function(latent, codebook, truth, analysis_plan) {
  n <- nrow(latent)
  rmin <- analysis_plan$scales$response_min
  rmax <- analysis_plan$scales$response_max
  ms <- truth$measurement
  lam <- ms$loading
  loc <- ms$scale_location
  spread <- ms$spread
  missing_loc <- setdiff(codebook$scales$scale_key, names(loc))
  if (length(missing_loc) > 0) stop("measurement$scale_location lacks: ", paste(missing_loc, collapse = ", "))
  cl <- stats::setNames(vector("list", 0), character(0))
  for (entry in truth$cross_loadings) {
    if (!entry$item %in% unlist(codebook$scales$item_codes)) {
      stop("cross_loadings names an item absent from the codebook: ", entry$item)
    }
    if (!entry$factor %in% colnames(latent)) {
      stop("cross_loadings names a latent absent from the truth: ", entry$factor)
    }
    cl[[entry$item]] <- entry
  }
  items <- list()
  # The item noise is drawn scale by scale in the generator's own order, so
  # that the synthetic export stays reproducible from its seed whatever order
  # the codebook gives the scales.
  if (!setequal(SIM_SCALE_DRAW_ORDER, codebook$scales$scale_key) ||
      length(SIM_SCALE_DRAW_ORDER) != nrow(codebook$scales)) {
    stop("SIM_SCALE_DRAW_ORDER must list exactly the scales of the codebook.")
  }
  for (s in match(SIM_SCALE_DRAW_ORDER, codebook$scales$scale_key)) {
    key <- codebook$scales$scale_key[s]
    rev_items <- codebook$scales$reverse_items[[s]]
    for (code in codebook$scales$item_codes[[s]]) {
      x_std <- lam * latent[, key] + sqrt(1 - lam^2) * stats::rnorm(n)
      if (!is.null(cl[[code]])) {
        other <- cl[[code]]$factor
        cross <- as.numeric(cl[[code]]$loading)
        rho <- stats::cor(latent[, key], latent[, other])
        v <- 1 + cross^2 + 2 * lam * cross * rho
        x_std <- (x_std + cross * latent[, other]) / sqrt(v)
      }
      x <- round(loc[[key]] + spread * x_std)
      x <- pmin(pmax(x, rmin), rmax)
      if (code %in% rev_items && isTRUE(ms$reverse_keyed_stored_as_displayed)) {
        x <- (rmin + rmax) - x
      }
      items[[code]] <- as.numeric(x)
    }
  }
  items
}

#' Random alphanumeric strings
#'
#' @param n Number of strings.
#' @param width Characters per string.
#' @return Character vector.
sim_random_string <- function(n, width) {
  pool <- c(0:9, letters, LETTERS)
  vapply(seq_len(n), function(i) paste(sample(pool, width, replace = TRUE), collapse = ""), "")
}

#' Party table used for scalometers and vote intention
#'
#' Realism parameters (not in the truth file): `mean` and `slope` describe
#' the scalometer as `mean + slope * left_right` on the -3..3 scale before
#' rounding; `position` is the party's location on the left-right latent;
#' `share` an approximate vote share; `vote_code` the code in
#' `pol_party_vote`; `block` the quota block (M4).
#'
#' @return Tibble, one row per party (six scalometer parties) plus the three
#'   non-party vote options (`column` NA).
sim_party_table <- function() {
  tibble::tribble(
    ~party,     ~column,            ~mean, ~slope, ~position, ~share, ~vote_code, ~block,
    "spd",      "pol_symp_spd",     -0.3,  -0.6,   -0.5,      0.16,   4,          "left",
    "cdu_csu",  "pol_symp_cdu_csu", -0.2,   0.7,    0.8,      0.29,   1,          "conservative",
    "greens",   "pol_symp_greens",  -0.6,  -0.9,   -1.0,      0.12,   6,          "left",
    "fdp",      "pol_symp_fdp",     -0.9,   0.6,    0.7,      0.04,   5,          "conservative",
    "afd",      "pol_symp_afd",     -1.5,   1.2,    1.8,      0.21,   322,        "conservative",
    "linke",    "pol_symp_linke",   -0.9,  -1.0,   -1.5,      0.09,   7,          "left",
    "bsw",      NA,                 NA,     NA,     0.3,       0.05,   392,        NA,
    "other",    NA,                 NA,     NA,     0.0,       0.03,   801,        NA,
    "invalid",  NA,                 NA,     NA,     0.0,       0.01,   802,        NA
  )
}

#' Approximate population shares of the 16 Bundesländer (codes 1..16)
#'
#' @return Numeric vector of length 16 summing to 1.
sim_bundesland_shares <- function() {
  shares <- c(3.5, 2.3, 9.6, 0.8, 21.6, 7.6, 4.9, 13.4, 15.9, 1.2, 4.5, 3.1, 1.9, 4.8, 2.6, 2.5)
  shares / sum(shares)
}

#' Values of a codebook value set
#'
#' @param codebook Codebook from [zm_codebook()].
#' @param set_id Value-set id in `codebook_factors.csv`.
#' @return Numeric vector of the coded values, in codebook order.
sim_value_set <- function(codebook, set_id) {
  f <- codebook$factors
  v <- f$value_corr[f$set_id == set_id]
  if (length(v) == 0) stop("Value set not found in codebook_factors.csv: ", set_id)
  v[order(f$order[f$set_id == set_id])]
}

#' Simulate the ZM panel raw export
#'
#' Generates `truth$n_total` respondents. Latent motives follow
#' `truth$motive_correlations`; age and gender carry the small covariate-
#' motive correlations; outcome latents are `X %*% beta + residual` with the
#' residual correlation of the truth file, scaled so that every outcome
#' latent has variance 1 (residuals are made exactly orthogonal to the
#' predictors, so the latent-level partial coefficients equal `true_beta`
#' exactly in the sample). Items follow the measurement model (loading,
#' location, spread, rounded and clamped to the response range);
#' reverse-keyed items are stored as displayed (`(min + max) - x`).
#' Exclusion rows are generated in the exact counts of `truth$exclusions`,
#' interleaved by a seeded shuffle, and shaped like the survey flow produces
#' them (consent refusal, quota-full exit, attention failure, break-off).
#' A quota-full row is exported as finished (`Finished = 1`, `Progress = 100`)
#' with `survey_status = analysis_plan$exclusions$survey_status_quota_full` and
#' `survey_status_detail` naming its own quota cell, and carries age and the
#' six scalometers but nothing after them: the quota branch sits between the
#' scalometers and the first attention check.
#'
#' The scenario ([zm_truth_scenario()]) adds up to four faults, each declared
#' in the truth file and each a no-op under `clean`: a `true_beta` cell of the
#' opposite sign, a contaminated residual draw for one outcome, a cross-loading
#' on one item, and a small set of blanked cells applied to the finished export
#' outside the break-off logic. The blanking is the last thing that happens and
#' draws from its own seed.
#'
#' @param truth Ground truth from [zm_truth()]; resolved through
#'   [zm_truth_scenario()] if it is not resolved already.
#' @param codebook Codebook from [zm_codebook()].
#' @param analysis_plan Configuration from [zm_config()] (response range, attention
#'   check keys, income bands).
#' @param seed Random seed (default `truth$seed`).
#' @param scenario Scenario name; `NULL` takes `truth$scenarios$export`.
#' @return `list(raw = tibble (97 export columns, one row per respondent),
#'   latent = tibble(respondent_row, ResponseId, nine latent scores, age,
#'   male, gender, income_true, exclusion_reason), truth)`; `truth` is the
#'   resolved truth, so `truth$scenario` names the scenario that generated it.
simulate_zm_panel <- function(truth = zm_truth(), codebook = zm_codebook(analysis_plan),
                              analysis_plan = zm_config(), seed = truth$seed, scenario = NULL) {
  truth <- zm_truth_scenario(truth, scenario)
  set.seed(seed)
  n <- truth$n_total
  ex <- truth$exclusions
  schema <- zm_raw_schema()
  rmin <- analysis_plan$scales$response_min
  rmax <- analysis_plan$scales$response_max

  # ---- 1. exclusion reasons, interleaved by a seeded shuffle -----------------
  reason <- sample(c(
    rep("consent_refused", ex$consent_refused),
    rep("incomplete", ex$incomplete),
    rep("age_under_18", ex$age_under_18),
    rep("quota_full", ex$quota_full),
    rep("attention_1_failed", ex$attention_1_failed),
    rep("attention_2_failed", ex$attention_2_failed),
    rep("gender_divers", truth$covariates$gender_divers_n),
    rep("none", truth$n_kept - truth$covariates$gender_divers_n)
  ))

  # ---- 2. latent motives and covariate latents -------------------------------
  motives <- truth$motive_correlations$order
  cmc <- truth$covariates$covariate_motive_correlations
  lat_names <- c(motives, "age_lat", "male_lat")
  R <- diag(length(lat_names))
  dimnames(R) <- list(lat_names, lat_names)
  R[motives, motives] <- truth$motive_correlations$matrix
  for (k in names(cmc$age_with)) {
    R["age_lat", k] <- R[k, "age_lat"] <- cmc$age_with[[k]]
  }
  # A median split attenuates a latent correlation by 2 * dnorm(0) ~ 0.80;
  # inflate so that the point-biserial correlation of the 0/1 indicator
  # matches the declared value.
  for (k in names(cmc$male_with)) {
    R["male_lat", k] <- R[k, "male_lat"] <- cmc$male_with[[k]] / (2 * stats::dnorm(0))
  }
  Z <- sim_exact_mvn(n, R)

  # ---- 3. covariates ---------------------------------------------------------
  cv <- truth$covariates
  age <- round(cv$age$mean + cv$age$sd * Z[, "age_lat"])
  age <- pmin(pmax(age, cv$age$min), cv$age$max)
  is_under <- reason == "age_under_18"
  age[is_under] <- sample(
    c(analysis_plan$exclusions$min_age - 2, analysis_plan$exclusions$min_age - 1), sum(is_under), replace = TRUE
  )

  gender_codes <- sim_value_set(codebook, "gender_1_3")   # 1 male, 2 female, 3 divers
  p_male <- cv$gender_probs$male / (cv$gender_probs$male + cv$gender_probs$female)
  male <- as.integer(Z[, "male_lat"] > stats::quantile(Z[, "male_lat"], 1 - p_male))
  gender <- ifelse(male == 1L, gender_codes[1], gender_codes[2])
  is_divers <- reason == "gender_divers"
  reason[is_divers] <- "none"  # valid participants, not an exclusion
  gender[is_divers] <- gender_codes[3]
  male[is_divers] <- 0L

  income_band <- sample(seq_along(cv$income_band_probs), n, replace = TRUE, prob = cv$income_band_probs)
  hh_members <- sample(seq_along(cv$hh_size_probs), n, replace = TRUE, prob = cv$hh_size_probs)
  band_rep <- analysis_plan$income$band_representative
  income_true <- unname(band_rep[income_band]) / pmin(hh_members, analysis_plan$income$hh_size_top_value)

  edu_levels <- as.integer(names(cv$education_probs))
  education <- sample(edu_levels, n, replace = TRUE, prob = unlist(cv$education_probs))
  bundesland_codes <- sim_value_set(codebook, "bundesland_1_16")
  bundesland <- sample(bundesland_codes, n, replace = TRUE, prob = sim_bundesland_shares())

  # ---- 4. outcome latents ----------------------------------------------------
  X <- cbind(Z[, motives, drop = FALSE], age = age, male = male, income = income_true)
  Xs <- scale(X)
  outcomes <- truth$outcome_residual_correlations$order
  pred_order <- c(motives, "age", "male", "income")
  B <- vapply(outcomes, function(o) {
    b <- unlist(truth$true_beta[[o]])
    missing_b <- setdiff(pred_order, names(b))
    if (length(missing_b) > 0) stop("true_beta$", o, " lacks: ", paste(missing_b, collapse = ", "))
    b[pred_order]
  }, numeric(length(pred_order)))
  XB <- Xs %*% B
  var_xb <- apply(XB, 2, stats::var)
  if (any(var_xb >= 1)) {
    stop("true_beta implies explained variance >= 1 for: ", paste(outcomes[var_xb >= 1], collapse = ", "))
  }
  # Residual draw. The contaminated-residual fault (truth$residual_contamination)
  # inflates a share of the standard-normal draws of ONE outcome before the
  # exact-covariance rotation. sim_exact_cov() rotates with triangular factors,
  # so the first column of the residual matrix stays a scalar multiple of its
  # own draw: contaminating the first outcome of
  # outcome_residual_correlations$order gives that outcome a heavy-tailed
  # residual while the declared residual correlations, the orthogonality to the
  # predictors and the exact recovery of true_beta all survive.
  rc <- truth$residual_contamination
  R_e <- truth$outcome_residual_correlations$matrix
  E_draw <- matrix(stats::rnorm(n * ncol(R_e)), n, ncol(R_e))
  if (!is.null(rc)) {
    j <- match(rc$outcome, colnames(R_e))
    if (is.na(j)) stop("residual_contamination$outcome is not an outcome: ", rc$outcome)
    if (j != 1L) {
      stop("residual_contamination$outcome must be the first entry of ",
           "outcome_residual_correlations$order; the exact-covariance rotation localises ",
           "the contaminated tail in the first column only.")
    }
    n_cont <- round(as.numeric(rc$share) * n)
    # Own seed, RNG state restored: the mask must not shift the main stream, so
    # that clean and trouble differ only through the declared faults.
    hit <- sim_with_seed(seed + as.integer(rc$seed_offset), sample.int(n, n_cont))
    E_draw[hit, j] <- E_draw[hit, j] * as.numeric(rc$multiplier)
  }
  E <- sim_exact_cov(E_draw, R_e)
  E <- stats::lm.fit(cbind(1, Xs), E)$residuals          # orthogonal to every predictor
  E <- sim_exact_cov(E, truth$outcome_residual_correlations$matrix)
  E <- sweep(E, 2, sqrt(1 - var_xb), `*`)               # residual variance = 1 - var(X beta)
  Y <- XB + E
  colnames(Y) <- outcomes
  latent <- cbind(Z[, motives, drop = FALSE], Y)

  # ---- 5. items --------------------------------------------------------------
  items <- sim_items_from_latent(latent, codebook, truth, analysis_plan)

  # ---- 6. attention checks ---------------------------------------------------
  a1 <- analysis_plan$exclusions$attention_check_1_correct
  a2 <- analysis_plan$exclusions$attention_check_2_correct
  att1 <- rep(a1, n)
  f1 <- reason == "attention_1_failed"
  att1[f1] <- sample(setdiff(seq(rmin, rmax), a1), sum(f1), replace = TRUE)
  att2 <- rep(a2, n)
  f2 <- reason == "attention_2_failed"
  att2[f2] <- sample(setdiff(seq(rmin, rmax), a2), sum(f2), replace = TRUE)

  # ---- 7. politics: scalometers, quota flags (M4), descriptive items ---------
  pol <- truth$politics
  r_lr <- pol$left_right_with_sdo
  left_right <- r_lr * Y[, "sdo_dom"] + sqrt(1 - r_lr^2) * stats::rnorm(n)
  parties <- sim_party_table()
  sym_range <- range(sim_value_set(codebook, "party_sympathy_minus3_3"))
  scal <- list()
  for (i in which(!is.na(parties$column))) {
    noise_sd <- sqrt(pol$scalometer_sd^2 - parties$slope[i]^2)
    v <- round(parties$mean[i] + parties$slope[i] * left_right + stats::rnorm(n, 0, noise_sd))
    scal[[parties$column[i]]] <- as.numeric(pmin(pmax(v, sym_range[1]), sym_range[2]))
  }
  liked <- function(cols) Reduce(`|`, lapply(cols, function(cn) scal[[cn]] > 0))
  left_cols <- parties$column[!is.na(parties$block) & parties$block == "left"]
  cons_cols <- parties$column[!is.na(parties$block) & parties$block == "conservative"]
  possibly_left <- as.integer(liked(left_cols))
  possibly_cons <- as.integer(liked(cons_cols))
  quota_group <- ifelse(
    possibly_left == 1L & possibly_cons == 0L, "left_leaning",
    ifelse(possibly_cons == 1L & possibly_left == 0L, "conservative_leaning", "mixed")
  )

  lr_vals <- sim_value_set(codebook, "left_right_0_10")
  lr_mid <- mean(range(lr_vals))
  pol_left_right <- round(lr_mid + (diff(range(lr_vals)) / 5) * left_right)
  pol_left_right <- as.numeric(pmin(pmax(pol_left_right, min(lr_vals)), max(lr_vals)))
  pol_left_right[stats::runif(n) < 0.05] <- NA          # optional item

  yes_no <- sim_value_set(codebook, "yes_no_1_2")
  pol_vote_would <- ifelse(stats::runif(n) < 0.86, yes_no[1], yes_no[2])
  pol_vote_would[stats::runif(n) < 0.03] <- NA            # optional item

  vote_codes <- sim_value_set(codebook, "gles_party_vote")
  if (!all(parties$vote_code %in% vote_codes)) stop("sim_party_table() vote codes not in codebook.")
  vote_w <- sapply(seq_len(nrow(parties)), function(i) {
    parties$share[i] * exp(-((left_right - parties$position[i])^2) / 2)
  })
  pol_party_vote <- vapply(seq_len(n), function(i) {
    sample(parties$vote_code, 1, prob = vote_w[i, ])
  }, numeric(1))
  other_code <- parties$vote_code[parties$party == "other"]
  other_names <- c("Volt", "Freie Wähler", "Tierschutzpartei", "Die PARTEI", "Piraten")
  pol_party_vote_text <- ifelse(
    pol_party_vote == other_code, sample(other_names, n, replace = TRUE), ""
  )

  # The party question is displayed only when voting intention is Yes.
  no_party_question <- is.na(pol_vote_would) | pol_vote_would != yes_no[1]
  pol_party_vote[no_party_question] <- NA_real_
  pol_party_vote_text[no_party_question] <- ""

  # ---- 8. survey flow, timing, identifiers -----------------------------------
  is_consent_refused <- reason == "consent_refused"
  is_incomplete <- reason == "incomplete"
  is_quota_full <- reason == "quota_full"
  consent_vals <- sim_value_set(codebook, "consent_1_2")
  consent_yes <- analysis_plan$exclusions$consent_required_value
  consent_no <- setdiff(consent_vals, consent_yes)[1]
  consent <- ifelse(is_consent_refused, consent_no, consent_yes)

  # question sequence in the export (= survey order); break-offs cut it
  seq_cols <- schema$name[seq(which(schema$name == "consent_check"), which(schema$name == "Kommentarfeld"))]
  pos <- function(col) match(col, seq_cols)
  K <- length(seq_cols)
  last_answered <- rep(K, n)
  last_answered[is_consent_refused] <- pos("consent_check")
  # The quota branch sits after the six scalometers and before the first
  # attention check: those rows carry age and scalometers, nothing later.
  last_answered[is_quota_full] <- pos("pol_symp_linke")
  last_answered[f1] <- pos("attentioncheck_1")
  last_answered[f2] <- pos("attentioncheck_2")
  # A break-off must leave at least one substantive question unanswered (the
  # truth file: "items missing after a random break-off point"), so the draw
  # stops one before the last demographic question rather than on it.
  last_answered[is_incomplete] <- sample(
    seq(pos("demo_age"), pos("demo_hh_members") - 1L), sum(is_incomplete), replace = TRUE
  )

  survey_status <- rep(analysis_plan$exclusions$survey_status_complete, n)
  survey_status_detail <- rep(analysis_plan$exclusions$survey_status_complete, n)
  survey_status[is_consent_refused] <- "consent_refused"
  survey_status_detail[is_consent_refused] <- "consent_refused"
  survey_status[is_quota_full] <- analysis_plan$exclusions$survey_status_quota_full
  survey_status_detail[is_quota_full] <- paste0("quota_", quota_group[is_quota_full])
  survey_status[f1 | f2] <- "attention_failed"
  survey_status_detail[f1] <- "attentioncheck_1"
  survey_status_detail[f2] <- "attentioncheck_2"
  survey_status[is_incomplete] <- ""
  survey_status_detail[is_incomplete] <- ""

  finished <- ifelse(is_incomplete, 0L, 1L)
  progress <- ifelse(is_incomplete, pmin(floor(100 * last_answered / K), 99), 100L)

  # embedded-data defaults before the scalometer block: flags "0", group ""
  reached_scalometers <- last_answered >= pos("pol_symp_linke")
  possibly_left_chr <- ifelse(reached_scalometers, as.character(possibly_left), "0")
  possibly_cons_chr <- ifelse(reached_scalometers, as.character(possibly_cons), "0")
  quota_group_chr <- ifelse(reached_scalometers, quota_group, "")

  tz <- "Europe/Berlin"
  field_start <- as.POSIXct("2026-06-19 12:00:00", tz = tz)
  start <- field_start + stats::runif(n, 0, 5 * 86400)
  duration_full <- round(stats::rlnorm(n, log(720), 0.35))
  duration <- round(duration_full * last_answered / K)
  duration[is_consent_refused] <- round(stats::runif(sum(is_consent_refused), 20, 120))
  end <- start + duration
  recorded <- end + sample(0:1, n, replace = TRUE)
  recorded[is_incomplete] <- end[is_incomplete] + 14 * 86400     # PartialData = +2 weeks

  # Bilendi codes of real respondents are numeric strings (codebook note);
  # Qualtrics response ids are "R_" + 15 alphanumerics.
  repeat {
    response_id <- paste0("R_", sim_random_string(n, 15))
    bilendi_id <- sprintf("%d%09d", sample(1:9, n, replace = TRUE), as.integer(floor(stats::runif(n) * 1e9)))
    if (!anyDuplicated(bilendi_id) && !anyDuplicated(response_id)) break
  }

  comments <- c("Danke!", "Interessante Umfrage.", "-", "Nein, alles gut.", "Viel Erfolg mit der Studie",
                "Manche Fragen waren schwer zu beantworten.", "ok")
  kommentar <- ifelse(stats::runif(n) < 0.04, sample(comments, n, replace = TRUE), "")
  info_click <- ifelse(stats::runif(n) < 0.15, 1, NA_real_)

  # ---- 9. assemble the export ------------------------------------------------
  cols <- list(
    StartDate = start, EndDate = end, Status = rep("0", n), Progress = as.integer(progress),
    Duration__in_seconds_ = as.numeric(duration), Finished = as.integer(finished),
    RecordedDate = recorded, ResponseId = response_id,
    DistributionChannel = rep("anonymous", n), UserLanguage = rep("DE", n),
    Q_BallotBoxStuffing = rep("", n), Last_Seen_Flow_Element_ID = rep("", n),
    Last_Seen_Question_IDs = rep("", n),
    info_teilnahme_2_1 = info_click, consent_check = as.numeric(consent), demo_age = as.numeric(age)
  )
  cols <- c(cols, scal, items)
  cols$attentioncheck_1 <- as.numeric(att1)
  cols$attentioncheck_2 <- as.numeric(att2)
  cols$pol_vote_would <- as.numeric(pol_vote_would)
  cols$pol_party_vote <- as.numeric(pol_party_vote)
  cols$pol_party_vote_801_TEXT <- pol_party_vote_text
  cols$pol_left_right <- pol_left_right
  cols$demo_gender <- as.numeric(gender)
  cols$demo_edu_school <- as.numeric(education)
  cols$demo_bundesland <- as.numeric(bundesland)
  cols$demo_income_hh_net <- as.numeric(income_band)
  cols$demo_hh_members <- as.numeric(hh_members)
  cols$Kommentarfeld <- kommentar
  cols$bilendi_id <- bilendi_id
  cols$survey_status <- survey_status
  cols$survey_status_detail <- survey_status_detail
  cols$possibly_left <- possibly_left_chr
  cols$possibly_conservative <- possibly_cons_chr
  cols$quota_group <- quota_group_chr

  missing_cols <- setdiff(schema$name, names(cols))
  if (length(missing_cols) > 0) stop("simulate_zm_panel(): columns not generated: ", paste(missing_cols, collapse = ", "))
  raw <- tibble::as_tibble(cols[schema$name])

  # blank everything after the last answered question (break-off / flow exit)
  for (j in seq_along(seq_cols)) {
    hit <- last_answered < j
    if (!any(hit)) next
    col <- seq_cols[j]
    raw[[col]][hit] <- if (is.character(raw[[col]])) "" else NA
  }

  # export order: by start time (exclusions stay interleaved)
  ord <- order(raw$StartDate)
  raw <- raw[ord, ]

  # ---- 10. planted missingness (scenario fault; no-op under `clean`) ---------
  # Applied last, to the finished export, among the rows AP1 keeps and outside
  # the break-off logic: a blanked cell must be a fault of the export, not a
  # flow exit, so that AP1 keeps the row and AP3 has to deal with the missing
  # value. Attention checks and the flow columns are never blanked, because
  # AP1 excludes a row whose attention check is missing.
  ms_spec <- truth$missingness
  if (!is.null(ms_spec)) {
    kept_rows <- which(reason[ord] == "none")
    forbidden <- c("attentioncheck_1", "attentioncheck_2", "consent_check", "Finished",
                   "Progress", "survey_status", "survey_status_detail")
    columns <- vapply(ms_spec$cells, function(c) as.character(c$column), "")
    counts <- vapply(ms_spec$cells, function(c) as.integer(c$n_missing), integer(1))
    bad <- intersect(columns, forbidden)
    if (length(bad) > 0) {
      stop("missingness$cells may not blank an AP1 criterion or flow column: ", paste(bad, collapse = ", "))
    }
    absent <- setdiff(columns, names(raw))
    if (length(absent) > 0) stop("missingness$cells names column(s) absent from the export: ", paste(absent, collapse = ", "))
    if (sum(counts) > length(kept_rows)) stop("missingness$cells asks for more blanked rows than the export keeps.")
    picked <- sim_with_seed(
      seed + as.integer(ms_spec$seed_offset),
      if (isTRUE(ms_spec$distinct_rows)) {
        split(sample(kept_rows, sum(counts)), rep(seq_along(counts), counts))
      } else {
        lapply(counts, function(k) sample(kept_rows, k))
      }
    )
    for (i in seq_along(columns)) {
      raw[[columns[i]]][picked[[i]]] <- NA
    }
  }

  attr(raw, "labels") <- stats::setNames(as.list(schema$label), schema$name)

  latent_tbl <- tibble::as_tibble(latent[ord, , drop = FALSE])
  latent_tbl <- tibble::add_column(
    latent_tbl,
    respondent_row = seq_len(n), ResponseId = raw$ResponseId, .before = 1
  )
  latent_tbl$age <- age[ord]
  latent_tbl$male <- male[ord]
  latent_tbl$gender <- gender[ord]
  latent_tbl$income_true <- income_true[ord]
  latent_tbl$exclusion_reason <- reason[ord]

  list(raw = raw, latent = latent_tbl, truth = truth)
}
