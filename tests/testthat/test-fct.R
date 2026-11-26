test_that("simulator truth matches sample analogues in a large sample", {
  for (fam in c("gaussian", "t", "clayton", "gumbel", "frank")) {
    s <- simulate_scenario(sim_params(family = fam, n = 40000, seed = 3))
    tr <- s$truth; smp <- s$sample
    # absolute differences (sampling error at n = 40,000 is about 0.01)
    expect_lt(abs(tr$value[tr$parameter == "ATE"] - smp$value[2]), 0.04, label = fam)
    expect_lt(abs(tr$value[tr$parameter == "ATT"] - smp$value[3]), 0.04, label = fam)
    expect_lt(abs(tr$value[tr$parameter == "ATU"] - smp$value[4]), 0.04, label = fam)
  }
})

test_that("MTE integrates to the ATE (binary, Gaussian)", {
  p <- sim_params(family = "gaussian", n = 20000, seed = 2)
  s <- simulate_scenario(p)
  u <- seq(0.0005, 0.9995, by = 0.001)
  m <- true_mte(p, colMeans(cbind(1, s$data$x1)), u = u)
  expect_equal(mean(m$mte), s$truth$value[s$truth$parameter == "ATE"], tolerance = 0.02)
})

test_that("three-regime scenario has three regimes and pairwise truths", {
  s <- simulate_scenario(sim_params(J = 3, tau = c(-0.3, 0.2, 0.4), family = "frank", n = 3000))
  expect_setequal(levels(s$data$regime), regime_labels(3))
  expect_length(unique(s$truth$contrast), 3)
  expect_true(all(c("u", "mte", "contrast") %in% names(s$mte)))
})

test_that("invalid parameters are rejected", {
  expect_error(sim_params(tau = c(0.97, 0)))
  expect_error(sim_params(family = "joe"))
})

test_that("check_spec flags problems and passes a valid spec", {
  d <- simulate_scenario(sim_params(n = 500))$data
  expect_length(check_spec(d, "y", "s", "x1", c("z1", "z2")), 0)
  expect_match(check_spec(d, "y", "x1", "x1", "z1"), "0/1", all = FALSE)
  expect_match(check_spec(d, "y", "s", "x1", character()), "^Note:", all = FALSE)
})

test_that("fit_families compares families and recovers the truth roughly", {
  s <- simulate_scenario(sim_params(family = "frank", tau = c(-0.4, 0.4), n = 3000, seed = 5))
  r <- fit_families(s$data, "y", "s", "x1", c("z1", "z2"), families = c("gaussian", "frank"))
  expect_equal(r$model, "A")
  expect_setequal(r$table$family, c("gaussian", "frank"))
  expect_equal(sum(r$table$akaike_weight), 1)
  ate <- s$truth$value[s$truth$parameter == "ATE"]
  expect_equal(r$table$ATE[r$table$family == "frank"], ate, tolerance = 0.15)
  expect_s3_class(estimated_mte(r$fits$frank, se = FALSE), "data.frame")
  expect_true(all(c("parameter", "estimate", "se") %in% names(coef_table(r$fits$frank))))
})

test_that("two-step and Model B paths work", {
  s <- simulate_scenario(sim_params(n = 2000))
  r <- fit_families(s$data, "y", "s", "x1", c("z1", "z2"), method = "twostep")
  expect_equal(nrow(r$table), 1)
  s3 <- simulate_scenario(sim_params(J = 3, tau = c(-0.3, 0.2, 0.4), n = 3000))
  rb <- fit_families(s3$data, "y", "s", "x1", c("z1", "z2"))
  expect_equal(rb$model, "B"); expect_length(rb$fits, 1)
})

test_that("PIAAC-shaped example fits with the default specification", {
  d <- example_piaac_data()
  sp <- default_spec("piaac")
  expect_length(grep("^Note", check_spec(d, sp$y, sp$d, sp$x, sp$z), invert = TRUE, value = TRUE), 0)
  r <- fit_families(d, sp$y, sp$d, sp$x, sp$z, families = "gaussian")
  expect_true(is.finite(r$table$ATT))
})

test_that("stored results are bundled and plots build", {
  for (f in mc_tables()) expect_false(is.null(read_result("montecarlo", f)), label = f)
  mc <- read_result("montecarlo", "mc_treatment_effects_all.csv")
  expect_s3_class(plot_mc(mc, "ATT", "rmse", c(0.3, 0.6)), "ggplot")
  cmp <- read_result("piaac", "copula_family_comparison_pv1.csv")
  expect_s3_class(plot_piaac_families(cmp), "ggplot")
  expect_s3_class(plot_mc_mte(read_result("montecarlo", "figure1_mte_data.csv")), "ggplot")
})

test_that("degenerate boundary maxima are replaced or excluded from the AIC ranking", {
  # Clayton DGP with strong selection on gains: the Frank multistart run from the
  # flipped tau climbs to tau1 -> 1 (non-PD Hessian) with a spuriously high logLik.
  s <- simulate_scenario(sim_params(family = "clayton", tau = c(-0.3, 0.7), n = 3000, seed = 1))
  # the package warns that it skipped the irregular top run; that warning is the expected behaviour
  expect_warning(
    r <- fit_families(s$data, "y", "s", "x1", c("z1", "z2"), families = c("frank", "clayton")),
    "irregular boundary point"
  )
  expect_true(all(r$table$regular))
  expect_true(is_regular(r$fits$frank))
  expect_lt(abs(coef(r$fits$frank)[["tau1"]]), 0.95)
  expect_equal(r$table$family[1], "clayton")   # true family wins once the spike is removed
})

test_that("Model D lab simulates, fits and compares with the truth", {
  r <- run_modelD(modelD_params(n = 1200, seed = 2))
  expect_true(r$regular)
  expect_equal(nrow(r$coef), 11)
  expect_true(all(c("estimate", "truth", "se") %in% names(r$effects)))
  tt <- r$effects[r$effects$estimand == "TT", ]
  expect_true(all(abs(tt$estimate - tt$truth) < 4 * tt$se + 1e-3))
  expect_s3_class(plot_dose_response(r), "ggplot")
  expect_s3_class(plot_intensity_effects(r), "ggplot")
})

test_that("Model E lab simulates, fits by CCP and builds its plots", {
  r <- run_modelE(modelE_params(N = 300, T = 5, seed = 3))
  expect_equal(nrow(r$effects), 5)
  expect_lt(abs(r$coef$estimate[r$coef$parameter == "psi"] - 0.05), 0.03)
  expect_s3_class(plot_adoption_path(r), "ggplot")
  expect_s3_class(plot_dynamic_gains(r), "ggplot")
})
