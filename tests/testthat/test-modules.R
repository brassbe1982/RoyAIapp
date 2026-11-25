test_that("simulator module responds to inputs", {
  shiny::testServer(mod_simulator_server, {
    session$setInputs(J = "2", family = "gaussian", tau0 = -0.3, tau1 = 0.3, tau2 = 0.4, nu = 5,
                      premium = 0.5, complementarity = 0.2, skill_return = 0.3,
                      sigma0 = 0.8, sigma1 = 1, cost = -0.2, iv = 0.7, kappa2 = 1.2,
                      n = 2000, seed = 1, go = 1)
    a <- scen()$truth$value[scen()$truth$parameter == "ATT"]
    # stronger positive selection on gains raises the ATT
    session$setInputs(tau1 = 0.7, go = 2)
    b <- scen()$truth$value[scen()$truth$parameter == "ATT"]
    expect_gt(b, a)
    # switching family and regimes re-simulates
    session$setInputs(family = "clayton", J = "3", go = 3)
    expect_equal(nlevels(scen()$data$regime), 3)
    expect_match(output$boxes$html, "ATT")
  })
})

test_that("estimation module fits simulator data and the PIAAC example", {
  sim <- shiny::reactiveVal(simulate_scenario(sim_params(n = 2000, seed = 4)))
  shiny::testServer(mod_estimate_server, args = list(sim = sim), {
    session$setInputs(source = "simulator", sep = ",", method = "ml",
                      families = c("gaussian", "gumbel"))
    session$setInputs(y = "y", d = "s", x = "x1", z = c("z1", "z2"), fit = 1)
    expect_equal(nrow(result()$table), 2)
    session$setInputs(pick = "gumbel")
    expect_s3_class(mte_est(), "data.frame")
    session$setInputs(source = "piaac")
    session$setInputs(y = "log_wage", d = "d", x = c("skill_lit", "schooling", "female"),
                      z = "ict_work", method = "twostep", fit = 2)
    expect_equal(result()$method, "twostep")
  })
})

test_that("Model D and E lab modules run on default inputs", {
  shiny::testServer(mod_modelD_server, {
    session$setInputs(n = 800, family = "gaussian", tau = 0.4, b1 = 0.8, b2 = -0.4, alpha_a = 0.5,
                      cost = 0.6, iv = 0.8, seed = 1, go = 1)
    expect_true(is.list(res()))
    expect_match(output$status$html, "AI users")
  })
  shiny::testServer(mod_modelE_server, {
    session$setInputs(N = 200, T = 4, psi = 0.05, sd_k = 0.25, sigma_e = 0.3, mu_k = 0, cost = 0.1,
                      beta = 0.95, method = "ccp", seed = 1, go = 1)
    expect_equal(nrow(res()$effects), 4)
    expect_match(output$status$html, "option value")
  })
})
