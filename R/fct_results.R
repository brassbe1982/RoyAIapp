# -----------------------------------------------------------------------------
# fct_results.R -- locate and read the stored Monte Carlo and PIAAC results.
# By default the copies bundled in inst/extdata/results are used. Set
#   options(royai.results_dir = "<dir>")
# to a folder with the same layout (montecarlo/, piaac/) to read live output.
# -----------------------------------------------------------------------------

#' Directory holding stored results
#' @keywords internal
results_dir <- function() {
  d <- getOption("royai.results_dir")
  if (!is.null(d) && dir.exists(d)) return(normalizePath(d))
  system.file("extdata", "results", package = "RoyAIapp", mustWork = TRUE)
}

#' Read one stored results file (NULL if absent)
#' @keywords internal
read_result <- function(group, file) {
  path <- file.path(results_dir(), group, file)
  if (!file.exists(path)) return(NULL)
  if (grepl("\\.csv$", path)) utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  else readLines(path, warn = FALSE)
}

#' Catalogue of Monte Carlo tables shown in the app
#' @keywords internal
mc_tables <- function() {
  c("Table 1. FIML parameters (bias, RMSE, coverage)" = "table1_fiml_parameters.csv",
    "Table 2. Treatment effects by estimator" = "table2_treatment_effects.csv",
    "Table 3. Copula misspecification" = "table3_misspecification.csv",
    "Table 4. Copula selection by AIC/BIC" = "table4_copula_selection.csv",
    "Table 5. Three regimes (J = 3)" = "table5_three_regimes.csv",
    "Weak instrument design" = "table_weak_instrument.csv",
    "Model D (continuous intensity): recovery" = "mc_modelD_summary.csv",
    "Model E (dynamic, learning): recovery" = "mc_modelE_summary.csv")
}

#' Pretty family labels for stored results
#' @keywords internal
family_label <- function(x) {
  lk <- c(gaussian = "Gaussian", t = "Student-t", clayton = "Clayton", gumbel = "Gumbel", frank = "Frank")
  out <- unname(lk[tolower(x)]); out[is.na(out)] <- x[is.na(out)]; out
}
