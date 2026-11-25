# -----------------------------------------------------------------------------
# fct_plots.R -- ggplot2 builders (pure functions, testable without Shiny).
# -----------------------------------------------------------------------------

#' Okabe-Ito colours for regimes and series
#' @keywords internal
royai_colours <- function() {
  c("Human-only" = "#0072B2", "AI-adopter" = "#E69F00", "AI-augmented" = "#E69F00",
    "AI-dominant" = "#CC79A7")
}
series_colours <- c("#0072B2", "#E69F00", "#009E73", "#CC79A7", "#D55E00", "#56B4E9", "#000000")

#' Shared ggplot theme
#' @keywords internal
theme_royai <- function(base_size = 13) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   legend.position = "top", legend.title = ggplot2::element_blank(),
                   plot.title.position = "plot",
                   strip.text = ggplot2::element_text(face = "bold", hjust = 0))
}

#' MTE curve(s): true (simulator) and optionally estimated
#' @keywords internal
plot_mte <- function(mte, truth = NULL, estimated = NULL) {
  g <- ggplot2::ggplot() +
    ggplot2::geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.3)
  if (!is.null(estimated)) {
    g <- g + ggplot2::geom_ribbon(data = estimated, ggplot2::aes(u, ymin = lower, ymax = upper),
                                  fill = "#E69F00", alpha = 0.18) +
      ggplot2::geom_line(data = estimated, ggplot2::aes(u, mte, colour = "Estimated MTE"),
                         linewidth = 1, linetype = "22")
  }
  if (!is.null(mte)) {
    g <- g + ggplot2::geom_line(data = mte, ggplot2::aes(u, mte, colour = contrast), linewidth = 1.1)
  }
  if (!is.null(truth)) {
    g <- g + ggplot2::geom_hline(data = truth, ggplot2::aes(yintercept = value, linetype = parameter),
                                 colour = "grey35", linewidth = 0.5)
  }
  g + ggplot2::scale_colour_manual(values = series_colours) +
    ggplot2::labs(x = expression("Unobserved resistance to AI adoption  " * u[S]),
                  y = "Marginal treatment effect (log earnings)", linetype = NULL) +
    theme_royai()
}

#' Densities of potential and observed earnings by regime
#' @keywords internal
plot_earnings <- function(dat, J, view = c("potential", "observed")) {
  view <- match.arg(view)
  lab <- regime_labels(J)
  if (view == "potential") {
    long <- do.call(rbind, lapply(seq_len(J), function(j)
      data.frame(y = dat[[paste0("y", j - 1)]], series = paste("Potential:", lab[j]))))
    long$series <- factor(long$series, levels = paste("Potential:", lab))
    cols <- stats::setNames(unname(royai_colours()[lab]), levels(long$series))
    ttl <- "Potential earnings for the whole population"
  } else {
    long <- data.frame(y = dat$y, series = dat$regime)
    cols <- royai_colours()[lab]
    ttl <- "Observed earnings, by chosen regime"
  }
  ggplot2::ggplot(long, ggplot2::aes(y, fill = series, colour = series)) +
    ggplot2::geom_density(alpha = 0.25, linewidth = 0.8) +
    ggplot2::scale_fill_manual(values = cols) + ggplot2::scale_colour_manual(values = cols) +
    ggplot2::labs(x = "Log earnings", y = "Density", subtitle = ttl) + theme_royai()
}

#' Sorting on gains: individual gain against the propensity score
#' @keywords internal
plot_selection <- function(dat, J, max_points = 2500) {
  if (nrow(dat) > max_points) dat <- dat[sample.int(nrow(dat), max_points), ]
  lab <- regime_labels(J)
  ggplot2::ggplot(dat, ggplot2::aes(pscore, gain, colour = regime)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.3) +
    ggplot2::geom_point(alpha = 0.45, size = 1.1) +
    ggplot2::geom_smooth(method = "loess", formula = y ~ x, se = FALSE, linewidth = 1) +
    ggplot2::scale_colour_manual(values = royai_colours()[lab]) +
    ggplot2::labs(x = "Propensity to use AI, P(Z)",
                  y = sprintf("Gain: Y(%s) - Y(Human)", lab[J])) +
    theme_royai()
}

#' Estimated treatment effects across fitted families (forest-style)
#' @keywords internal
plot_family_effects <- function(tab) {
  long <- do.call(rbind, lapply(c("ATE", "ATT", "ATU"), function(p)
    data.frame(family = family_label(tab$family), parameter = p, estimate = tab[[p]],
               se = tab[[paste0(p, "_se")]])))
  long <- long[is.finite(long$estimate), ]
  long$family <- factor(long$family, levels = rev(unique(family_label(tab$family))))
  ggplot2::ggplot(long, ggplot2::aes(estimate, family, colour = parameter)) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey60", linewidth = 0.3) +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = estimate - 1.96 * se, xmax = estimate + 1.96 * se),
                            height = 0.2, position = ggplot2::position_dodge(width = 0.5), na.rm = TRUE) +
    ggplot2::geom_point(size = 2.4, position = ggplot2::position_dodge(width = 0.5)) +
    ggplot2::scale_colour_manual(values = series_colours) +
    ggplot2::labs(x = "Estimate (95% CI)", y = NULL) + theme_royai()
}

#' Monte Carlo bias or RMSE across sample sizes
#' @keywords internal
plot_mc <- function(mc, effect = "ATT", measure = c("rmse", "bias", "coverage"), tau = NULL) {
  measure <- match.arg(measure)
  d <- mc[mc$parameter == effect & mc$design == "main", ]
  if (!is.null(tau)) d <- d[d$tau %in% tau, ]
  d <- d[is.finite(d[[measure]]), ]
  d$family <- family_label(d$family)
  d$tau_lab <- sprintf("tau = %.1f", d$tau)
  d$value <- d[[measure]]
  g <- ggplot2::ggplot(d, ggplot2::aes(n, value, colour = estimator)) +
    ggplot2::geom_line(linewidth = 0.9) + ggplot2::geom_point(size = 1.8) +
    ggplot2::scale_x_log10(breaks = sort(unique(d$n)),
                           labels = function(x) ifelse(x >= 1000, paste0(x / 1000, "k"), x),
                           expand = ggplot2::expansion(mult = 0.12)) +
    ggplot2::facet_grid(tau_lab ~ family) +
    ggplot2::theme(panel.spacing.x = ggplot2::unit(1.2, "lines")) +
    ggplot2::scale_colour_manual(values = series_colours) +
    ggplot2::labs(x = "Sample size (log scale)", y = toupper(measure)) + theme_royai(12)
  if (measure == "coverage") g <- g + ggplot2::geom_hline(yintercept = 0.95, linetype = "22", colour = "grey40")
  if (measure == "bias") g <- g + ggplot2::geom_hline(yintercept = 0, colour = "grey60")
  g
}

#' Paper Figure 1 (Monte Carlo MTE) from its stored data
#' @keywords internal
plot_mc_mte <- function(fd) {
  ggplot2::ggplot(fd, ggplot2::aes(u, mean, colour = series, fill = series)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lo, ymax = hi), alpha = 0.15, colour = NA) +
    ggplot2::geom_line(linewidth = 1) + ggplot2::facet_wrap(~panel) +
    ggplot2::scale_colour_manual(values = series_colours) +
    ggplot2::scale_fill_manual(values = series_colours) +
    ggplot2::labs(x = expression(u[S]), y = "MTE") + theme_royai(12)
}

#' PIAAC application: effects by copula family and specification
#' @keywords internal
plot_piaac_families <- function(cmp) {
  long <- do.call(rbind, lapply(c("ATE", "ATT", "ATU"), function(p)
    data.frame(spec = cmp$spec, family = family_label(cmp$family), parameter = p,
               estimate = cmp[[p]],
               se = if (paste0(p, "_se") %in% names(cmp)) cmp[[paste0(p, "_se")]] else NA_real_,
               weight = cmp$akaike_w)))
  long$spec <- factor(long$spec, levels = c("base", "occ2"),
                      labels = c("Base: country effects (n = 17,045)",
                                 "occ2: + 2-digit ISCO effects (n = 8,366)"))
  ggplot2::ggplot(long, ggplot2::aes(estimate, family, colour = parameter)) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey60", linewidth = 0.3) +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = estimate - 1.96 * se, xmax = estimate + 1.96 * se),
                            height = 0.2, position = ggplot2::position_dodge(width = 0.5), na.rm = TRUE) +
    ggplot2::geom_point(size = 2.4, position = ggplot2::position_dodge(width = 0.5)) +
    ggplot2::facet_wrap(~spec) + ggplot2::scale_colour_manual(values = series_colours) +
    ggplot2::labs(x = "Effect of AI-ready work on log hourly earnings", y = NULL) + theme_royai(12)
}

utils::globalVariables(c("u", "lower", "upper", "contrast", "value", "parameter", "series",
                         "pscore", "gain", "regime", "estimate", "family", "estimator",
                         "lo", "hi", "y", "n", "share", "se", "a", "naive", "truth", "t"))
