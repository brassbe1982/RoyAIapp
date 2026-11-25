# -----------------------------------------------------------------------------
# fct_estimate.R -- estimation helpers wrapping RoyAI::roy().
# -----------------------------------------------------------------------------

#' Bundled PIAAC-shaped example data
#'
#' Runs RoyAI::piaac_example() (synthetic data with the PIAAC Cycle 2 public
#' use file layout) through RoyAI::piaac_onet_prep(). The adoption indicator
#' is the package's proxy (AI-exposed occupation x high ICT use at work).
#' ICT use at work is the excluded selection variable in the suggested
#' specification because it enters the proxy; with the real PUF the project
#' uses ICT use at home (ICTHOMEC2) instead.
#' @keywords internal
example_piaac_data <- function(n = 3000, seed = 1) {
  ex <- RoyAI::piaac_example(n = n, seed = seed)
  d <- suppressWarnings(RoyAI::piaac_onet_prep(ex$piaac, ex$exposure))
  d$country <- factor(d$country)
  d[stats::complete.cases(d[, c("log_wage", "skill_lit", "age", "schooling", "ict_work", "d")]), ]
}

#' Default specifications for each data source
#' @keywords internal
default_spec <- function(source) {
  switch(source,
    simulator = list(y = "y", d = "s", x = "x1", z = c("z1", "z2")),
    piaac = list(y = "log_wage", d = "d",
                 x = c("skill_lit", "schooling", "age", "age2", "female", "country"),
                 z = "ict_work"),
    list(y = NULL, d = NULL, x = NULL, z = NULL))
}

#' Check a user specification against the data before fitting
#' @return character vector of problems (empty if fine)
#' @keywords internal
check_spec <- function(data, y, d, x, z) {
  msg <- character()
  if (!length(y) || !y %in% names(data)) return("Choose an outcome variable.")
  if (!length(d) || !d %in% names(data)) return("Choose a regime (selection) variable.")
  if (!is.numeric(data[[y]])) msg <- c(msg, "The outcome must be numeric.")
  lv <- sort(unique(stats::na.omit(data[[d]])))
  if (!(identical(as.numeric(lv), c(0, 1)) || identical(as.numeric(lv), c(0, 1, 2))))
    msg <- c(msg, "The regime variable must be coded 0/1 (Model A) or 0/1/2 (Model B).")
  if (y %in% c(x, z) || d %in% c(x, z)) msg <- c(msg, "Outcome or regime also appears as a regressor.")
  if (!length(z)) msg <- c(msg, paste("Note: no excluded variable (exclusion restriction) is selected,",
                                      "so identification rests on functional form only."))
  if (length(intersect(x, z))) msg <- c(msg, "Excluded variables must not also be outcome regressors.")
  msg
}

#' Build outcome and selection formulas
#' @keywords internal
build_formulas <- function(y, d, x, z) {
  rhs_x <- if (length(x)) paste(sprintf("`%s`", x), collapse = " + ") else "1"
  list(outcome = stats::as.formula(sprintf("`%s` ~ %s", y, rhs_x)),
       selection = stats::as.formula(sprintf("`%s` ~ %s", d,
                                             paste(c(rhs_x, sprintf("`%s`", z)), collapse = " + "))))
}

#' Fit Model A for one or more dependence families, or Model B (two-step)
#'
#' @param data data.frame
#' @param y,d,x,z variable names: outcome, regime, outcome regressors, exclusions
#' @param families dependence families (Model A, method "ml")
#' @param method "ml" or "twostep"
#' @param correction Model B correction ("dmf" or "lee")
#' @param progress optional function(i, n, label) called before each fit
#' @return list(model, fits (named), errors (named chr), table, formulas)
#' @keywords internal
fit_families <- function(data, y, d, x, z, families = "gaussian", method = "ml",
                         correction = "dmf", progress = NULL) {
  f <- build_formulas(y, d, x, z)
  J <- length(unique(stats::na.omit(data[[d]])))
  model <- if (J == 2) "A" else "B"
  if (model == "B" || method == "twostep") families <- if (model == "B") correction else "gaussian"
  fits <- list(); errs <- character()
  for (i in seq_along(families)) {
    fam <- families[i]
    if (is.function(progress)) progress(i, length(families), fam)
    fit <- tryCatch({
      if (model == "A") {
        ft <- RoyAI::roy(f$outcome, f$selection, data = data, method = method, dependence = fam)
        # Guard: copula likelihoods can spike at a boundary (|tau| -> 1), where the
        # Hessian is not positive definite. If multistart kept such a point, refit
        # from the default start and keep that run when it is a regular maximum.
        if (method == "ml" && fam != "gaussian" && !is_regular(ft)) {
          alt <- tryCatch(RoyAI::roy(f$outcome, f$selection, data = data, method = "ml",
                                     dependence = fam, multistart = FALSE), error = function(e) NULL)
          if (!is.null(alt) && is_regular(alt)) {
            alt$app_note <- sprintf(paste("multistart kept an irregular maximum (logLik %.1f, Hessian not",
                                          "positive definite); replaced by the regular default-start maximum"),
                                    as.numeric(stats::logLik(ft)))
            ft <- alt
          }
        }
        ft
      } else {
        RoyAI::roy(f$outcome, f$selection, data = data, method = "twostep", correction = fam)
      }
    }, error = function(e) e)
    if (inherits(fit, "error")) errs[fam] <- conditionMessage(fit) else fits[[fam]] <- fit
  }
  list(model = model, method = method, fits = fits, errors = errs, formulas = f,
       table = compare_fits(fits, model))
}

#' Comparison table: information criteria, dependence, treatment effects
#' @keywords internal
compare_fits <- function(fits, model = "A") {
  if (!length(fits)) return(data.frame())
  rows <- lapply(names(fits), function(nm) {
    f <- fits[[nm]]
    eff <- tryCatch(RoyAI::roy_effects(f), error = function(e) NULL)
    ll <- tryCatch(as.numeric(stats::logLik(f)), error = function(e) NA_real_)
    k <- f$npar %||% length(f$par)
    pick <- function(pat) if (is.null(eff)) NA_real_ else {
      i <- grep(pat, eff$population, fixed = TRUE)[1]; if (is.na(i)) NA_real_ else eff$estimate[i]
    }
    pick_se <- function(pat) if (is.null(eff)) NA_real_ else {
      i <- grep(pat, eff$population, fixed = TRUE)[1]; if (is.na(i)) NA_real_ else eff$se[i]
    }
    data.frame(
      family = nm, method = f$method, converged = isTRUE(f$converged),
      logLik = ll, npar = k,
      AIC = if (is.finite(ll)) -2 * ll + 2 * k else NA_real_,
      BIC = if (is.finite(ll)) -2 * ll + log(f$spec$n) * k else NA_real_,
      dependence = dependence_label(f),
      ATE = pick("ATE"), ATE_se = pick_se("ATE"),
      ATT = pick("ATT"), ATT_se = pick_se("ATT"),
      ATU = pick("ATU"), ATU_se = pick_se("ATU"),
      start = multistart_choice(f), regular = is_regular(f),
      note = f$app_note %||% NA_character_, stringsAsFactors = FALSE)
  })
  tab <- do.call(rbind, rows)
  if (model == "A" && any(is.finite(tab$AIC))) {
    # rank regular maxima first; Akaike weights only among regular maxima
    tab <- tab[order(!tab$regular, tab$AIC), ]
    ok <- tab$regular & is.finite(tab$AIC)
    tab$akaike_weight <- NA_real_
    if (any(ok)) {
      dA <- tab$AIC[ok] - min(tab$AIC[ok])
      tab$akaike_weight[ok] <- exp(-dA / 2) / sum(exp(-dA / 2))
    }
  }
  rownames(tab) <- NULL
  tab
}

`%||%` <- function(a, b) if (is.null(a)) b else a

#' Is a fit a regular interior maximum?
#'
#' TRUE unless the optimizer's Hessian (of the negative log-likelihood) has a
#' non-positive eigenvalue or the largest score exceeds `tol`. Two-step and
#' Model B fits (no optim object) count as regular.
#' @keywords internal
is_regular <- function(fit, tol = 1) {
  o <- fit$optim
  if (is.null(o) || is.null(o$hessian)) return(isTRUE(fit$converged) || is.null(fit$converged))
  ev <- tryCatch(eigen(o$hessian, symmetric = TRUE, only.values = TRUE)$values, error = function(e) NA)
  isTRUE(fit$converged) && all(is.finite(ev)) && min(ev) > 0 &&
    (is.null(o$gradient) || max(abs(o$gradient)) < tol)
}

#' Dependence parameters of a fit as a compact label
#' @keywords internal
dependence_label <- function(fit) {
  p <- fit$par
  i <- grep("^(rho|tau|nu)[0-9]$", names(p))
  if (!length(i)) return(NA_character_)
  paste(sprintf("%s=%.2f", names(p)[i], p[i]), collapse = " ")
}

#' Which multistart run was kept (Model A ML, non-Gaussian families)
#' @keywords internal
multistart_choice <- function(fit) {
  ms <- fit$multistart
  if (is.null(ms) || !"chosen" %in% names(ms)) return(NA_character_)
  as.character(ms$start[ms$chosen][1])
}

#' Coefficient table for any roy fit
#' @keywords internal
coef_table <- function(fit) {
  se <- sqrt(pmax(diag(fit$vcov), 0))
  data.frame(parameter = names(fit$par), estimate = unname(fit$par),
             se = unname(se[seq_along(fit$par)]),
             z = unname(fit$par / se[seq_along(fit$par)]), stringsAsFactors = FALSE)
}

#' Estimated MTE for a Model A fit (NULL when not available)
#' @keywords internal
estimated_mte <- function(fit, se = TRUE) {
  if (!inherits(fit, "roy_fit_A")) return(NULL)
  tryCatch(RoyAI::roy_mte(fit, u = seq(0.02, 0.98, by = 0.02), se = se),
           error = function(e) NULL)
}
