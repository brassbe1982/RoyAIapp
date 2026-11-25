# -----------------------------------------------------------------------------
# fct_modelDE.R -- pure logic for the Model D (continuous AI-use intensity) and
# Model E (dynamic selection with learning) labs. Testable without Shiny.
# -----------------------------------------------------------------------------

#' Default Model D lab parameters
#' @keywords internal
modelD_params <- function(n = 2000, seed = 1, family = "gaussian", tau = 0.4,
                          b1 = 0.8, b2 = -0.4, alpha_a = 0.5, iv = 0.8, cost = 0.6) {
  list(n = n, seed = seed, family = family, tau = tau, beta_a = c(b1, b2),
       alpha = c(log(0.6), alpha_a), gamma = c(cost, 0.3, iv, 0.6))
}

#' Simulate Model D data and fit it with RoyAI::roy_continuous()
#'
#' @return list(data, fit, coef (estimate, se, truth), effects (estimate vs truth),
#'   multistart, share_users, naive)
#' @keywords internal
run_modelD <- function(p) {
  set.seed(p$seed)
  nu <- if (p$family == "t") 5 else NULL
  d <- RoyAI::roy_continuous_sim(p$n, beta_a = p$beta_a, alpha = p$alpha, gamma = p$gamma,
                                 tau = p$tau, family = p$family, nu = 5)
  fit <- suppressWarnings(RoyAI::roy_continuous(y ~ x1, a ~ x1 + z1 + z2, d, family = p$family,
                                                fix_nu = nu))
  b <- stats::coef(fit); se <- sqrt(pmax(diag(stats::vcov(fit)), 0))
  truth <- c(1, 0.5, p$beta_a, p$gamma, p$alpha, p$tau)
  keep <- seq_along(truth)
  coefs <- data.frame(parameter = names(b)[keep], truth = truth, estimate = unname(b[keep]),
                      se = unname(se[keep]), stringsAsFactors = FALSE)
  grid <- seq(0, 0.95, by = 0.05); A <- c(0.2, 0.5, 0.8)
  est <- RoyAI::roy_continuous_effects(fit, a = A, grid = grid)
  tr <- RoyAI::roy_continuous_truth(beta_a = p$beta_a, alpha = p$alpha, gamma = p$gamma,
                                    tau = p$tau, family = p$family, nu = 5,
                                    covariates = d[, c("x1", "z1", "z2")], a = A, grid = grid)
  est$truth <- tr$truth
  ols <- stats::coef(stats::lm(y ~ x1 + a + I(a^2), d))
  naive <- data.frame(a = grid, naive = mean(ols[1] + ols[2] * d$x1) + ols[3] * grid + ols[4] * grid^2)
  list(data = d, fit = fit, coef = coefs, effects = est, naive = naive,
       multistart = fit$multistart, share_users = mean(d$a > 0), regular = fit$regular)
}

#' Dose-response plot: truth, ML estimate with 95 percent band, naive OLS
#' @keywords internal
plot_dose_response <- function(res) {
  dr <- res$effects[res$effects$estimand == "dose_response", ]
  ggplot2::ggplot(dr, ggplot2::aes(a)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se),
                         fill = "#E69F00", alpha = 0.2) +
    ggplot2::geom_line(ggplot2::aes(y = truth, colour = "True E[Y(a)]"), linewidth = 1.1) +
    ggplot2::geom_line(ggplot2::aes(y = estimate, colour = "Copula ML"), linewidth = 1, linetype = "22") +
    ggplot2::geom_line(data = res$naive, ggplot2::aes(a, naive, colour = "Naive OLS"), linewidth = 0.8) +
    ggplot2::scale_colour_manual(values = c("True E[Y(a)]" = "#0072B2", "Copula ML" = "#E69F00",
                                            "Naive OLS" = "#D55E00")) +
    ggplot2::labs(x = "AI-use intensity a", y = "Average outcome E[Y(a)]") +
    theme_royai()
}

#' Effects-at-intensity plot (TT, ATE, TUT): estimate with CI vs truth
#' @keywords internal
plot_intensity_effects <- function(res) {
  e <- res$effects[res$effects$estimand != "dose_response", ]
  e$estimand <- factor(e$estimand, c("TT", "ATE", "TUT"),
                       c("TT: +0.1 for workers at a", "ATE: +0.1 for everyone", "TUT: non-users moved to a"))
  ggplot2::ggplot(e, ggplot2::aes(factor(a), estimate)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.3) +
    ggplot2::geom_pointrange(ggplot2::aes(ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se,
                                          colour = "Estimate (95% CI)")) +
    ggplot2::geom_point(ggplot2::aes(y = truth, colour = "Truth"), shape = 4, size = 3, stroke = 1.2) +
    ggplot2::facet_wrap(~estimand, scales = "free_y") +
    ggplot2::scale_colour_manual(values = c("Estimate (95% CI)" = "#E69F00", "Truth" = "#0072B2")) +
    ggplot2::labs(x = "Intensity a", y = "Effect on outcome") + theme_royai(12)
}

#' Default Model E lab parameters
#' @keywords internal
modelE_params <- function(N = 600, T = 8, seed = 1, psi = 0.05, sd_k = 0.25, sigma_e = 0.3,
                          mu_k = 0, cost = 0.1, lambda = 0.1, beta = 0.95, method = "ccp") {
  list(N = N, T = T, seed = seed, psi = psi, sd_k = sd_k, sigma_e = sigma_e, mu_k = mu_k,
       cost = c(cost, 0.1), lambda = lambda, beta = beta, method = method)
}

#' Simulate a Model E panel and fit it with RoyAI::roy_dynamic()
#' @keywords internal
run_modelE <- function(p) {
  set.seed(p$seed)
  d <- RoyAI::roy_dynamic_sim(p$N, p$T, mu_k = p$mu_k, sd_k = p$sd_k, sigma_e = p$sigma_e,
                              psi = p$psi, cost = p$cost, lambda = p$lambda, beta = p$beta)
  fit <- suppressWarnings(RoyAI::roy_dynamic(d, cost = ~ x, beta = p$beta, method = p$method))
  b <- stats::coef(fit); se <- sqrt(pmax(diag(stats::vcov(fit)), 0))
  truth <- c(1, 0.3, p$mu_k, p$sd_k, p$sigma_e, p$psi, p$cost, p$lambda)
  coefs <- data.frame(parameter = names(b), truth = truth, estimate = unname(b), se = unname(se),
                      stringsAsFactors = FALSE)
  eff <- RoyAI::roy_dynamic_effects(fit)
  eff$ATT_true <- vapply(seq_len(p$T), function(t) mean(d$kappa[d$t == t & d$s == 1]), numeric(1))
  eff$ATE_true <- vapply(seq_len(p$T), function(t) mean(d$kappa[d$t == t]), numeric(1))
  list(data = d, fit = fit, coef = coefs, effects = eff)
}

#' Adoption path: observed, fitted dynamic model and myopic counterfactual
#' @keywords internal
plot_adoption_path <- function(res) {
  e <- res$effects
  long <- rbind(data.frame(t = e$t, value = e$share_ai, series = "Observed AI share"),
                data.frame(t = e$t, value = e$p_dynamic, series = "Fitted, forward-looking"),
                data.frame(t = e$t, value = e$p_myopic, series = "Myopic worker (beta = 0)"))
  ggplot2::ggplot(long, ggplot2::aes(t, value, colour = series)) +
    ggplot2::geom_line(linewidth = 1) + ggplot2::geom_point(size = 2) +
    ggplot2::scale_colour_manual(values = stats::setNames(series_colours[c(7, 2, 1)],
      c("Observed AI share", "Fitted, forward-looking", "Myopic worker (beta = 0)"))) +
    ggplot2::scale_y_continuous(labels = function(x) paste0(round(100 * x), "%"), limits = c(0, 1)) +
    ggplot2::labs(x = "Period", y = "Share using AI") + theme_royai()
}

#' Gains over time: ATE/ATT/ATU from beliefs, truth and the naive gap
#' @keywords internal
plot_dynamic_gains <- function(res) {
  e <- res$effects
  long <- rbind(data.frame(t = e$t, value = e$ATT, series = "ATT (model)"),
                data.frame(t = e$t, value = e$ATT_true, series = "ATT (true kappa)"),
                data.frame(t = e$t, value = e$ATU, series = "ATU (model)"),
                data.frame(t = e$t, value = e$ATE, series = "ATE (model)"),
                data.frame(t = e$t, value = e$naive_gap, series = "Naive earnings gap"))
  ggplot2::ggplot(long, ggplot2::aes(t, value, colour = series, linetype = series)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.3) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::scale_colour_manual(values = stats::setNames(series_colours[c(2, 2, 1, 3, 5)],
      c("ATT (model)", "ATT (true kappa)", "ATU (model)", "ATE (model)", "Naive earnings gap"))) +
    ggplot2::scale_linetype_manual(values = stats::setNames(c("solid", "22", "solid", "solid", "dotted"),
      c("ATT (model)", "ATT (true kappa)", "ATU (model)", "ATE (model)", "Naive earnings gap"))) +
    ggplot2::labs(x = "Period", y = "Gain from AI (log points)") + theme_royai()
}
