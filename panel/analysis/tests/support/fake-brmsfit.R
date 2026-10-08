# A stand-in brms fit with real posterior draws.
#
# The route tests (test-ap6-regression-route.R, test-ap7-joint-route.R) mock
# `brms::brm()` to return this stand-in, so the AP6 regression route runs end
# to end on *real* draws without a sampler: the validity gate, the result
# verbs, the projections and the AP10 verbs all read posterior draws, effective
# sample sizes, R-hat, sampler diagnostics and posterior predictions. A
# fabricated `structure(list(), class = "brmsfit")` shell
# (tests/testthat/test-targets-regressions.R) cannot answer any of that.
#
# `fb_fake_brmsfit()` therefore builds an object of class
# `c("zm_fake_brmsfit", "brmsfit")` that carries a real
# `posterior::draws_array` of deterministic pseudo-draws and answers the
# generics the route calls. Nothing about it is a mock of the consumer side:
# every production verb runs unchanged on these draws.
#
# The draws are deterministic by construction. Each chain holds the SAME
# normal quantile grid around the declared per-term mean, in a chain-specific
# seeded permutation. Two consequences the scenarios rely on:
#   * between-chain variance is zero, so R-hat is ~1 and never fails the gate
#     by accident (the scenarios steer the gate through the ESS check alone);
#   * the permuted grid behaves like an independent sample, so bulk/tail ESS
#     is about the number of draws. 4 x 1000 draws clear the smoke ESS target
#     of 2000; 4 x 60 draws miss it.
# The pooled draws are symmetric about the declared mean, so the posterior
# median of a term is exactly that mean.

# ---- deterministic randomness ----------------------------------------------

# Evaluate `expr` under `seed` and restore the caller's RNG state.
fb_with_seed <- function(seed, expr) {
  had <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  old <- if (had) get(".Random.seed", envir = globalenv(), inherits = FALSE) else NULL
  on.exit({
    if (had) {
      assign(".Random.seed", old, envir = globalenv())
    } else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      rm(".Random.seed", envir = globalenv())
    }
  }, add = TRUE)
  set.seed(seed)
  expr
}

# Standardised noise of the declared shape, `n` values, unit-ish scale.
#   normal  N(0, 1)              — the neutral shape
#   light   uniform, excess kurtosis -1.2, tails far shorter than normal
#   heavy   t(3), excess kurtosis far above normal, very long tails
fb_noise <- function(kind, n, seed) {
  fb_with_seed(seed, switch(
    kind,
    normal = stats::rnorm(n),
    light = stats::runif(n, -sqrt(3), sqrt(3)),
    heavy = stats::rt(n, df = 3),
    stop("fb_noise(): unknown noise shape '", kind, "'.")
  ))
}

# The visiting order of a sorted quantile grid within one chain.
#
# The grid positions are dealt alternately into the chain's first and second
# half and shuffled inside each half. R-hat and bulk/tail ESS are computed on
# SPLIT chains, so each half must look like the whole: the alternating deal
# gives both halves the same spread and nearly the same mean, which keeps the
# between-(half-)chain variance — and therefore R-hat — at about 1, while the
# shuffle inside each half keeps the sequence free of autocorrelation.
fb_order <- function(n, seed) {
  positions <- seq_len(n)
  first <- positions[seq(1L, n, by = 2L)]
  second <- positions[seq(2L, n, by = 2L)]
  fb_with_seed(seed, c(first[sample.int(length(first))], second[sample.int(length(second))]))
}

# ---- the stand-in fit -------------------------------------------------------

#' The design matrix column names a formula and data produce
fb_design_columns <- function(formula, data) {
  colnames(stats::model.matrix(formula, data = data))
}

#' brms parameter name of a design-matrix column
fb_b_name <- function(column) {
  paste0("b_", ifelse(column == "(Intercept)", "Intercept",
                      gsub("[^[:alnum:]_]", "", column)))
}

#' The slope prior SD of a brms prior object (class "b", no coefficient)
#'
#' The fitting verbs state the metric-slope prior as `normal(0, <sd>)` on
#' class `b` without a coefficient; the gender contrasts carry their own rows.
fb_slope_sd_of <- function(prior) {
  if (is.null(prior)) return(NA_real_)
  rows <- as.data.frame(prior)
  rows <- rows[rows$class == "b" & !nzchar(rows$coef), , drop = FALSE]
  if (nrow(rows) != 1L) return(NA_real_)
  as.numeric(sub("^normal\\(0, *([0-9.]+)\\)$", "\\1", rows$prior[[1]]))
}

#' The deterministic draws array of the declared variables
#'
#' Shared by [fb_fake_brmsfit()] and [fb_fake_mv_brmsfit()], so that the
#' univariate and the multivariate stand-in build their draws by the very same
#' rule and the same per-variable seeds.
#'
#' @param variables Parameter names, in the order the array carries them.
#' @param means,scales Per-variable centre and posterior SD, same order.
#' @param chains,draws_per_chain Shape of the array.
#' @param seed Seed of the deterministic construction.
#' @return A `posterior::draws_array`.
fb_draws_array <- function(variables, means, scales, chains, draws_per_chain, seed) {
  n <- as.integer(draws_per_chain)
  chains <- as.integer(chains)
  grid <- stats::qnorm((seq_len(n) - 0.5) / n)
  array <- array(NA_real_, dim = c(n, chains, length(variables)),
                 dimnames = list(iteration = NULL, chain = NULL, variable = variables))
  for (v in seq_along(variables)) {
    for (chain in seq_len(chains)) {
      array[, chain, v] <- means[[v]] + scales[[v]] * grid[fb_order(n, seed + 1000L * v + chain)]
    }
  }
  posterior::as_draws_array(array)
}

#' A stand-in `brmsfit` with deterministic posterior draws
#'
#' A plain `formula` (or `brmsformula`) yields the univariate stand-in; an
#' `mvbrmsformula` — the joint RQ2 model of
#' `define_joint_regression_model()` — yields the multivariate stand-in of
#' [fb_fake_mv_brmsfit()], whose parameters carry brms's response prefixes.
#'
#' @param formula The model formula (a plain `formula` or an `mvbrmsformula`).
#' @param data The regression table the route passed to the fitting verb.
#' @param family `"gaussian"` or `"student"` (or a `brmsfamily`); ignored for
#'   the multivariate stand-in, whose families live in the response formulas.
#' @param slope_means Named numeric vector over the `b_...` parameter names;
#'   the posterior median of each term.
#' @param sigma Posterior mean of the residual scale; for the multivariate
#'   stand-in one value or a vector named by `sigma_<resp>`.
#' @param chains,draws_per_chain Shape of the returned draws.
#' @param seed Seed of the deterministic draw construction.
#' @param outcome,role Identity carried for the recorder and for `update()`.
#' @param noise Shape of the `posterior_predict()` noise (see [fb_noise()]).
#' @param update_draws Post-warmup draws per chain that `update()` returns.
#' @param recorder Environment collecting `brm()` and `update()` calls.
#' @param draw_scale Posterior SD of every coefficient.
#' @param rescor_means Multivariate only: named numeric vector over the six
#'   `rescor__<r1>__<r2>` parameters.
#' @param rescor_scale Multivariate only: posterior SD of every residual
#'   correlation.
#' @return An object of class `c("zm_fake_brmsfit", "brmsfit")`.
fb_fake_brmsfit <- function(formula, data, family = "gaussian", slope_means = NULL,
                            sigma = 1, chains = 4L, draws_per_chain = 1000L, seed = 1L,
                            outcome = NA_character_, role = NA_character_,
                            noise = "normal", update_draws = draws_per_chain,
                            recorder = NULL, draw_scale = 0.05, nu = 4,
                            rescor_means = NULL, rescor_scale = 0.02) {
  if (inherits(formula, "mvbrmsformula")) {
    return(fb_fake_mv_brmsfit(
      formula = formula, data = data, slope_means = slope_means, sigma = sigma,
      rescor_means = rescor_means, chains = chains, draws_per_chain = draws_per_chain,
      seed = seed, outcome = outcome, role = role, noise = noise,
      update_draws = update_draws, recorder = recorder, draw_scale = draw_scale,
      rescor_scale = rescor_scale))
  }
  family_name <- if (inherits(family, "brmsfamily") || is.list(family)) {
    as.character(family$family)
  } else {
    as.character(family)
  }
  columns <- fb_design_columns(formula, data)
  b_vars <- fb_b_name(columns)
  if (is.null(slope_means)) slope_means <- stats::setNames(rep(0, length(b_vars)), b_vars)
  missing <- setdiff(b_vars, names(slope_means))
  if (length(missing) > 0L) {
    stop("fb_fake_brmsfit(): no declared mean for ", paste(missing, collapse = ", "), ".")
  }
  variables <- c(b_vars, "sigma", if (identical(family_name, "student")) "nu")
  means <- c(slope_means[b_vars], sigma = sigma,
             if (identical(family_name, "student")) c(nu = nu))
  scales <- c(rep(draw_scale, length(b_vars)), sigma * 0.02,
              if (identical(family_name, "student")) 0)
  n <- as.integer(draws_per_chain)
  chains <- as.integer(chains)
  fit <- list(
    formula = brms::bf(formula),
    data = as.data.frame(data),
    family = brms::brmsfamily(family_name),
    fake_draws = fb_draws_array(variables, means, scales, chains, n, seed),
    fb = list(
      formula_raw = formula, family_name = family_name, slope_means = slope_means,
      sigma = sigma, chains = chains, draws_per_chain = n, seed = seed,
      outcome = outcome, role = role, noise = noise, update_draws = update_draws,
      recorder = recorder, draw_scale = draw_scale, nu = nu,
      design_columns = columns, b_vars = b_vars, multivariate = FALSE
    )
  )
  structure(fit, class = c("zm_fake_brmsfit", "brmsfit"))
}

# ---- the multivariate stand-in (the joint RQ2 fit) -------------------------
#
# The joint route needs a fit whose parameters carry brms's response prefixes:
# `ap6_gate_resp()` reads all four responses off the `mvbrmsformula`,
# `ap6_focal_b_vars()` collects `b_<resp>_<term>` for every one of them, and
# `extract_joint_posterior()` resolves `b_<resp>_<motive>` and
# `rescor__<r1>__<r2>` by name. The real `mvbrmsformula` therefore stays on the
# fit, so that `brms::brmsterms()` and `stats::family(fit, resp = )` read the
# responses and their families from the production object itself.

#' brms response names of an `mvbrmsformula`, in formula order
fb_mv_responses <- function(formula) {
  unname(as.character(brms::brmsterms(formula)$responses))
}

#' The design-matrix columns of every response formula of an `mvbrmsformula`
fb_mv_design_columns <- function(formula, data) {
  out <- lapply(formula$forms, function(form) fb_design_columns(form$formula, data))
  names(out) <- fb_mv_responses(formula)
  out
}

#' brms parameter names of one response's population-level coefficients
fb_mv_b_names <- function(resp, columns) {
  paste0("b_", resp, "_", ifelse(columns == "(Intercept)", "Intercept",
                                 gsub("[^[:alnum:]_]", "", columns)))
}

#' The six residual-correlation parameter names, in brms's pair order
#'
#' brms enumerates the unordered response pairs as `utils::combn()` does, and
#' names each `rescor__<earlier response>__<later response>`; this is the
#' ordering the stand-in reproduces.
fb_mv_rescor_names <- function(responses) {
  pairs <- utils::combn(length(responses), 2L)
  paste0("rescor__", responses[pairs[1L, ]], "__", responses[pairs[2L, ]])
}

#' A stand-in `brmsfit` of a multivariate model with correlated residuals
#'
#' @inheritParams fb_fake_brmsfit
#' @return An object of class `c("zm_fake_brmsfit", "brmsfit")`.
fb_fake_mv_brmsfit <- function(formula, data, slope_means, sigma = 1, rescor_means = NULL,
                               chains = 4L, draws_per_chain = 1000L, seed = 1L,
                               outcome = NA_character_, role = NA_character_,
                               noise = "normal", update_draws = draws_per_chain,
                               recorder = NULL, draw_scale = 0.05, rescor_scale = 0.02) {
  responses <- fb_mv_responses(formula)
  columns <- fb_mv_design_columns(formula, data)
  b_vars <- unlist(lapply(responses, function(r) fb_mv_b_names(r, columns[[r]])),
                   use.names = FALSE)
  sigma_vars <- paste0("sigma_", responses)
  rescor_vars <- fb_mv_rescor_names(responses)
  if (is.null(slope_means)) slope_means <- stats::setNames(rep(0, length(b_vars)), b_vars)
  if (is.null(rescor_means)) rescor_means <- stats::setNames(rep(0, length(rescor_vars)), rescor_vars)
  missing <- c(setdiff(b_vars, names(slope_means)), setdiff(rescor_vars, names(rescor_means)))
  if (length(missing) > 0L) {
    stop("fb_fake_mv_brmsfit(): no declared mean for ", paste(missing, collapse = ", "), ".")
  }
  sigma_values <- if (length(sigma) == 1L) {
    stats::setNames(rep(as.numeric(sigma), length(sigma_vars)), sigma_vars)
  } else {
    sigma[sigma_vars]
  }
  if (anyNA(sigma_values)) {
    stop("fb_fake_mv_brmsfit(): no declared residual scale for every response.")
  }
  if (any(abs(rescor_means[rescor_vars]) + 4 * rescor_scale >= 1)) {
    stop("fb_fake_mv_brmsfit(): the declared residual correlations must stay inside (-1, 1).")
  }
  variables <- c(b_vars, sigma_vars, rescor_vars)
  means <- c(slope_means[b_vars], sigma_values[sigma_vars], rescor_means[rescor_vars])
  scales <- c(rep(draw_scale, length(b_vars)), sigma_values[sigma_vars] * 0.02,
              rep(rescor_scale, length(rescor_vars)))
  n <- as.integer(draws_per_chain)
  chains <- as.integer(chains)
  fit <- list(
    # the production `mvbrmsformula` itself: brms reads the responses, their
    # families and the rescor flag off it
    formula = formula,
    data = as.data.frame(data),
    family = NULL,
    fake_draws = fb_draws_array(variables, means, scales, chains, n, seed),
    fb = list(
      formula_raw = formula, family_name = "gaussian", slope_means = slope_means,
      sigma = sigma_values, chains = chains, draws_per_chain = n, seed = seed,
      outcome = outcome, role = role, noise = noise, update_draws = update_draws,
      recorder = recorder, draw_scale = draw_scale, nu = NA_real_,
      design_columns = columns, b_vars = b_vars, multivariate = TRUE,
      responses = responses, sigma_vars = sigma_vars, rescor_vars = rescor_vars,
      rescor_means = rescor_means, rescor_scale = rescor_scale
    )
  )
  structure(fit, class = c("zm_fake_brmsfit", "brmsfit"))
}

# ---- the generics the AP6 route calls on a fit ------------------------------

fb_draw_index <- function(object, draw_ids = NULL, ndraws = NULL) {
  total <- posterior::ndraws(object$fake_draws)
  if (!is.null(draw_ids)) return(as.integer(draw_ids))
  if (!is.null(ndraws)) {
    ndraws <- min(as.integer(ndraws), total)
    return(unique(as.integer(round(seq(1, total, length.out = ndraws)))))
  }
  seq_len(total)
}

fb_as_draws_matrix <- function(object) {
  posterior::as_draws_matrix(object$fake_draws)
}

fb_posterior_epred <- function(object, draw_ids = NULL, ndraws = NULL, ...) {
  # No consumer of the joint route predicts from the joint fit: the gate reads
  # draws and sampler diagnostics, and the predictive checks run on the four
  # univariate fits. A caller that does would need brms's draws x obs x
  # response array, which is deliberately not fabricated here.
  if (isTRUE(object$fb$multivariate)) {
    stop("fb_posterior_epred(): the multivariate stand-in carries no predictive draws; ",
         "implement the draws x observations x response array if a consumer needs them.")
  }
  index <- fb_draw_index(object, draw_ids, ndraws)
  design <- stats::model.matrix(object$fb$formula_raw, data = object$data)
  draws <- fb_as_draws_matrix(object)[index, object$fb$b_vars, drop = FALSE]
  out <- as.matrix(draws) %*% t(design)
  dimnames(out) <- NULL
  out
}

fb_posterior_predict <- function(object, draw_ids = NULL, ndraws = NULL, ...) {
  index <- fb_draw_index(object, draw_ids, ndraws)
  mu <- fb_posterior_epred(object, draw_ids = index)
  sigma <- as.numeric(fb_as_draws_matrix(object)[index, "sigma"])
  noise <- matrix(fb_noise(object$fb$noise, length(mu), object$fb$seed + 7919L),
                  nrow = nrow(mu), ncol = ncol(mu))
  mu + sigma * noise
}

fb_nuts_params <- function(object, ...) {
  draws <- object$fake_draws
  chains <- posterior::nchains(draws)
  iterations <- posterior::niterations(draws)
  n <- chains * iterations
  grid <- expand.grid(Iteration = seq_len(iterations), Chain = seq_len(chains))
  energy <- fb_noise("normal", n, object$fb$seed + 104729L) * 10 + 500
  # Divergences and tree-depth hits are absent, energies vary finitely: the
  # sampler half of the gate passes, so the scenarios steer it through ESS.
  rbind(
    data.frame(Chain = grid$Chain, Iteration = grid$Iteration,
               Parameter = "divergent__", Value = 0),
    data.frame(Chain = grid$Chain, Iteration = grid$Iteration,
               Parameter = "treedepth__", Value = 5),
    data.frame(Chain = grid$Chain, Iteration = grid$Iteration,
               Parameter = "energy__", Value = energy)
  )[, c("Chain", "Iteration", "Parameter", "Value")]
}

fb_update <- function(object, ...) {
  args <- list(...)
  spec <- object$fb
  if (!is.null(spec$recorder)) {
    spec$recorder$update_calls[[length(spec$recorder$update_calls) + 1L]] <- list(
      outcome = spec$outcome, role = spec$role, chains = args$chains,
      iter = args$iter, warmup = args$warmup, thin = args$thin,
      control = args$control, recompile = args$recompile
    )
  }
  fb_fake_brmsfit(
    formula = spec$formula_raw, data = object$data, family = spec$family_name,
    slope_means = spec$slope_means, sigma = spec$sigma, chains = spec$chains,
    draws_per_chain = spec$update_draws,
    seed = spec$seed + 31L * length(spec$recorder$update_calls),
    outcome = spec$outcome, role = spec$role, noise = spec$noise,
    update_draws = spec$update_draws, recorder = spec$recorder,
    draw_scale = spec$draw_scale, nu = spec$nu,
    # ignored by the univariate stand-in; the joint fit keeps its six residual
    # correlations across the doublings of the ESS remedy
    rescor_means = spec$rescor_means,
    rescor_scale = if (is.null(spec$rescor_scale)) 0.02 else spec$rescor_scale
  )
}

fb_bayes_R2 <- function(object, ...) {
  mu <- fb_posterior_epred(object)
  sigma <- as.numeric(fb_as_draws_matrix(object)[, "sigma"])
  r2 <- apply(mu, 1, stats::var) / (apply(mu, 1, stats::var) + sigma^2)
  matrix(c(mean(r2), stats::sd(r2), unname(stats::quantile(r2, c(.025, .975)))),
         nrow = 1, dimnames = list("R2", c("Estimate", "Est.Error", "Q2.5", "Q97.5")))
}

#' Register the stand-in's S3 methods with the generics' own namespaces
#'
#' The draw, prediction and sampler generics live in posterior, rstantools and
#' bayesplot; brms only re-exports them. A method for a class that inherits
#' from `brmsfit` must therefore be registered with the defining namespace,
#' otherwise brms's own `*.brmsfit` methods are dispatched and reach for Stan
#' internals the stand-in does not have.
fb_register_methods <- function() {
  register <- function(generic, package, method) {
    registerS3method(generic, "zm_fake_brmsfit", method, envir = asNamespace(package))
  }
  register("as_draws_array", "posterior", function(x, ...) x$fake_draws)
  register("as_draws_df", "posterior", function(x, ...) posterior::as_draws_df(x$fake_draws))
  register("as_draws_matrix", "posterior", function(x, ...) fb_as_draws_matrix(x))
  register("as_draws", "posterior", function(x, ...) x$fake_draws)
  register("variables", "posterior", function(x, ...) posterior::variables(x$fake_draws))
  register("nchains", "posterior", function(x, ...) posterior::nchains(x$fake_draws))
  register("ndraws", "posterior", function(x, ...) posterior::ndraws(x$fake_draws))
  register("posterior_epred", "rstantools", fb_posterior_epred)
  register("posterior_predict", "rstantools", fb_posterior_predict)
  register("bayes_R2", "rstantools", fb_bayes_R2)
  register("nuts_params", "bayesplot", fb_nuts_params)
  register("nobs", "stats", function(object, ...) nrow(object$data))
  register("update", "stats", fb_update)
  invisible(TRUE)
}

# ---- stand-ins for the helpers that read Stan internals ---------------------

#' The provenance row [ap6_fit_provenance()] would capture from a real fit
#'
#' The production helper reads `brms::stancode()`, `brms::prior_summary()`,
#' `fit$fit@sim` and the CmdStan version — all Stan internals a stand-in fit
#' does not have. The shape is the one `tr_provenance()` in
#' tests/testthat/test-targets-regressions.R freezes; the identity fields come
#' from the attributes [ap6_record_fit()] has already put on the fit.
fb_fit_provenance <- function(fit) {
  tibble::tibble(
    outcome = attr(fit, "outcome"),
    slope_sd = as.numeric(attr(fit, "slope_sd")),
    family = as.character(fit$fb$family_name),
    formula = as.character(attr(fit, "formula_text")),
    nobs = as.integer(nrow(fit$data)),
    priors = "stand-in fit: prior_summary() needs a compiled model",
    stan_code_hash = "stand-in", hash_algorithm = "sha256",
    chains = as.integer(posterior::nchains(fit$fake_draws)),
    warmup = as.integer(attr(fit, "warmup_used")),
    iter_per_chain = as.integer(attr(fit, "iter_used")),
    seed = as.integer(attr(fit, "seed")),
    gate_status = as.character(attr(fit, "gate_status")),
    brms_version = as.character(utils::packageVersion("brms")),
    cmdstanr_version = "stand-in", cmdstan_version = "stand-in"
  )
}

#' The power-scaling result [ap6_run_priorsense()] would return
#'
#' priorsense needs a compiled Stan model: it re-weights the posterior by
#' power-scaling the log prior and log likelihood, which a stand-in fit cannot
#' supply. This is the fixture shape of `tr_prior_sensitivity()` in
#' tests/testthat/test-targets-regressions.R, one block per configured prior
#' block; the real `ap6_summarise_powerscale_diagnostics()` then runs on it.
fb_priorsense <- function(fit, blocks, components, lower_alpha,
                          upper_alpha, divergence_measure, sensitivity_threshold) {
  variables <- grep("^b_", posterior::variables(fit$fake_draws), value = TRUE)
  block_names <- as.character(unlist(blocks))
  results <- lapply(block_names, function(block) {
    list(
      block = block,
      prior_selection = if (block == "all_priors") "all" else block,
      matrix = tibble::tibble(
        variable = c(variables[[2L]], "bayes_R2"),
        prior = c(0.02, 0.06), likelihood = c(0.01, 0.03),
        diagnosis = c("-", "prior-data conflict")
      ),
      sensitivity = 0.06, pareto_k_max = 0.4,
      importance_sampling_draws = posterior::ndraws(fit$fake_draws),
      importance_sampling_status = "reliable"
    )
  })
  names(results) <- block_names
  list(
    variables = variables,
    settings = list(components = components, lower_alpha = lower_alpha,
                    upper_alpha = upper_alpha, div_measure = divergence_measure,
                    sensitivity_threshold = sensitivity_threshold),
    priorsense_version = "stand-in", blocks = results
  )
}
