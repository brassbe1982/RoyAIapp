# -----------------------------------------------------------------------------
# fct_simulate.R -- pure (non-reactive) simulation helpers for the simulator.
# Everything here is plain R and unit-tested; the Shiny module only wires
# inputs to sim_params() and renders what simulate_scenario() returns.
# -----------------------------------------------------------------------------

#' Copula families offered in the app
#' @keywords internal
app_families <- function() {
  c("Gaussian" = "gaussian", "Student-t" = "t", "Clayton" = "clayton",
    "Gumbel" = "gumbel", "Frank" = "frank")
}

#' Human-readable regime labels
#' @keywords internal
regime_labels <- function(J) {
  if (J == 2) c("Human-only", "AI-adopter") else c("Human-only", "AI-augmented", "AI-dominant")
}

#' Build a validated simulator parameter set
#'
#' Maps the economically named app inputs onto the copula Roy model of
#' RoyAI (Model C, ordered single index):
#'   Y_j = b_j0 + b_j1 x1 + U_j,  S = s iff kappa_s < Z'gamma + V <= kappa_{s+1},
#'   Z = (1, x1, z1, z2), (F_j(U_j), Phi(V)) joined by a copula with Kendall tau_j.
#'
#' @param J 2 (binary AI adoption) or 3 (human-only / AI-augmented / AI-dominant)
#' @param family copula family (see app_families())
#' @param tau Kendall tau for each regime (length J)
#' @param nu t-copula degrees of freedom
#' @param premium AI productivity premium: intercept gap b_10 - b_00
#' @param complementarity AI-skill complementarity: slope gap b_11 - b_01
#' @param skill_return return to skill in human-only work, b_01
#' @param sigma outcome error sds (length J)
#' @param cost adoption cost: minus the selection intercept
#' @param iv_strength coefficient on each excluded instrument (z1, z2)
#' @param kappa2 upper cutpoint for J = 3 (AI-dominant threshold)
#' @param n sample size
#' @param seed RNG seed
#' @return list of class "royai_sim_params"
#' @keywords internal
sim_params <- function(J = 2, family = "gaussian", tau = c(-0.3, 0.3), nu = 5,
                       premium = 0.5, complementarity = 0.2, skill_return = 0.3,
                       sigma = c(0.8, 1, 1.2), cost = -0.2, iv_strength = 0.7,
                       kappa2 = 1.2, n = 3000, seed = 1) {
  stopifnot(J %in% 2:3, family %in% app_families(), n >= 100)
  tau <- rep_len(tau, J); sigma <- rep_len(sigma, J)
  if (any(abs(tau) >= 0.95)) stop("|tau| must be below 0.95")
  if (any(sigma <= 0)) stop("sigma must be positive")
  b0 <- c(0.5, skill_return)
  b1 <- b0 + c(premium, complementarity)
  beta <- if (J == 2) list(b0, b1) else list(b0, b1, b0 + 1.6 * c(premium, complementarity))
  structure(list(J = J, family = family, tau = tau, nu = if (family == "t") nu else NULL,
                 beta = beta, gamma = c(-cost, 0.3, iv_strength, iv_strength),
                 sigma = sigma, kappa = if (J == 3) kappa2 else NULL,
                 n = as.integer(n), seed = seed),
            class = "royai_sim_params")
}

#' Simulate one scenario and compute its population treatment parameters
#'
#' @param p a sim_params() object
#' @return list(params, data, truth, sample, mte, shares)
#' @keywords internal
simulate_scenario <- function(p) {
  stopifnot(inherits(p, "royai_sim_params"))
  set.seed(p$seed)
  dat <- RoyAI::roy_copula_sim(p$n, beta = p$beta, gamma = p$gamma, sigma = p$sigma,
                               tau = p$tau, family = p$family, nu = p$nu, kappa = p$kappa)
  cov <- dat[, c("x1", "z1", "z2")]
  lab <- regime_labels(p$J)
  dat$regime <- factor(lab[dat$s + 1], levels = lab)
  dat$d <- as.integer(dat$s > 0)
  # Propensity of any AI use, P(Z) = P(Z'gamma + V > 0)
  dat$pscore <- stats::pnorm(drop(cbind(1, as.matrix(cov)) %*% p$gamma))
  dat$gain <- dat[[paste0("y", p$J - 1)]] - dat$y0

  contrasts <- if (p$J == 2) list(c(1, 0)) else list(c(1, 0), c(2, 0), c(2, 1))
  truth <- do.call(rbind, lapply(contrasts, function(cc) {
    tr <- RoyAI::roy_te_truth(p$beta, p$gamma, p$sigma, p$tau, p$family, nu = p$nu,
                              kappa = p$kappa, covariates = cov,
                              instrument = if (p$J == 2) "z2" else NULL,
                              to = cc[1], from = cc[2])
    data.frame(contrast = sprintf("%s vs %s", lab[cc[1] + 1], lab[cc[2] + 1]),
               parameter = names(tr), value = unname(tr), stringsAsFactors = FALSE)
  }))
  truth$parameter <- relabel_parameters(truth$parameter, lab)

  list(params = p, data = dat, truth = truth, sample = sample_effects(dat, p$J),
       mte = true_mte(p, colMeans(cbind(1, dat$x1))),
       shares = as.data.frame(table(regime = dat$regime) / nrow(dat),
                              responseName = "share"))
}

# TT_s0 / TT_s1 / TT_s2 -> "TT | Human-only" etc.
relabel_parameters <- function(x, lab) {
  for (k in seq_along(lab)) x[x == paste0("TT_s", k - 1)] <- paste0("TT | ", lab[k])
  x
}

#' Sample analogues from the simulated potential outcomes
#'
#' Because the simulator keeps both potential outcomes, the sample ATE, ATT
#' and ATU are known exactly; the naive comparison of observed means then
#' decomposes into ATT + selection bias.
#' @keywords internal
sample_effects <- function(dat, J) {
  top <- paste0("y", J - 1)
  g <- dat[[top]] - dat$y0
  adopt <- dat$s == J - 1; human <- dat$s == 0
  naive <- mean(dat$y[adopt]) - mean(dat$y[human])
  att <- mean(g[adopt])
  data.frame(
    quantity = c("Naive difference in observed means", "Sample ATE", "Sample ATT",
                 "Sample ATU", "Selection bias (naive - ATT)",
                 "Share with positive gain", "Share sorting on gain (Roy-consistent)"),
    value = c(naive, mean(g), att, mean(g[human]), naive - att, mean(g > 0),
              mean((g > 0) == (dat$s == J - 1))),
    stringsAsFactors = FALSE)
}

#' True marginal treatment effect curve along unobserved resistance u_S
#'
#' MTE(x, u) = x'(beta_to - beta_from) + m_to(1 - u) - m_from(1 - u),
#' m_j(w) = E[U_j | W = w] from the copula (RoyAI::copula_cond_mean).
#' @keywords internal
true_mte <- function(p, xbar, u = seq(0.02, 0.98, by = 0.02)) {
  lab <- regime_labels(p$J)
  m <- lapply(seq_len(p$J), function(j)
    RoyAI::copula_cond_mean(1 - u, RoyAI::copula_par(p$family, p$tau[j], p$nu), p$sigma[j]))
  pairs <- if (p$J == 2) list(c(1, 0)) else list(c(1, 0), c(2, 0))
  do.call(rbind, lapply(pairs, function(cc) {
    data.frame(u = u,
               mte = sum(xbar * (p$beta[[cc[1] + 1]] - p$beta[[cc[2] + 1]])) +
                 m[[cc[1] + 1]] - m[[cc[2] + 1]],
               contrast = sprintf("%s vs %s", lab[cc[1] + 1], lab[cc[2] + 1]))
  }))
}
