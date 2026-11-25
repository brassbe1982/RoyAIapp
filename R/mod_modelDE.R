# -----------------------------------------------------------------------------
# mod_modelDE.R -- "Intensity (Model D)" and "Dynamics (Model E)" labs.
# Each lab simulates from the model, estimates it with RoyAI and compares the
# estimates with the truth.
# -----------------------------------------------------------------------------

de_coef_table <- function(df) {
  num <- c("truth", "estimate", "se")
  DT::formatRound(DT::datatable(df, rownames = FALSE, fillContainer = FALSE,
                                options = list(dom = "t", pageLength = 50)), num, 3)
}

mod_modelD_ui <- function(id) {
  ns <- shiny::NS(id)
  sidebar <- bslib::sidebar(
    width = 320, open = "always",
    shiny::numericInput(ns("n"), "Workers", 2000, min = 300, max = 20000, step = 500),
    shiny::selectInput(ns("family"), "Copula family", app_families()),
    shiny::sliderInput(ns("tau"), "Kendall tau (outcome shock, pull to AI)", -0.8, 0.8, 0.4, 0.05),
    shiny::sliderInput(ns("b1"), "Return to intensity (linear)", -0.5, 1.5, 0.8, 0.05),
    shiny::sliderInput(ns("b2"), "Curvature (negative = diminishing returns)", -1, 0.5, -0.4, 0.05),
    shiny::sliderInput(ns("alpha_a"), "Dispersion rises with intensity", -0.5, 1, 0.5, 0.05),
    shiny::sliderInput(ns("cost"), "Selection intercept (lower = more non-users)", -0.5, 1.5, 0.6, 0.05),
    shiny::sliderInput(ns("iv"), "Instrument strength (z1)", 0.1, 1.5, 0.8, 0.05),
    shiny::numericInput(ns("seed"), "Random seed", 1, min = 1, step = 1),
    shiny::actionButton(ns("go"), "Simulate and estimate", icon = shiny::icon("play"),
                        class = "btn-primary w-100"),
    shiny::helpText("Fits RoyAI::roy_continuous() by maximum likelihood (multistart for non-Gaussian families).")
  )
  bslib::layout_sidebar(
    sidebar = sidebar, fillable = FALSE,
    shiny::uiOutput(ns("status")),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(full_screen = TRUE, bslib::card_header("Dose-response E[Y(a)]"),
                  shiny::plotOutput(ns("dose"), height = 340)),
      bslib::card(full_screen = TRUE, bslib::card_header("Effects at intensity a"),
                  shiny::plotOutput(ns("eff"), height = 340))),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(bslib::card_header("Parameters: truth vs estimate"), DT::DTOutput(ns("coef"))),
      bslib::card(bslib::card_header("Multistart runs"), DT::DTOutput(ns("ms")),
                  shiny::p(class = "small text-muted",
                           "A run is kept only if it is a regular maximum: positive definite Hessian,",
                           "small score and |tau| <= 0.98.")))
  )
}

mod_modelD_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    res <- shiny::eventReactive(input$go, {
      p <- modelD_params(n = input$n, seed = input$seed, family = input$family, tau = input$tau,
                         b1 = input$b1, b2 = input$b2, alpha_a = input$alpha_a,
                         iv = input$iv, cost = input$cost)
      shiny::withProgress(message = "Estimating Model D", value = 0.3,
                          tryCatch(run_modelD(p), error = function(e) e))
    }, ignoreNULL = FALSE)
    ok <- shiny::reactive({
      r <- res()
      if (inherits(r, "error")) shiny::validate(paste("Estimation failed:", conditionMessage(r)))
      r
    })
    output$status <- shiny::renderUI({
      r <- ok()
      cls <- if (r$regular) "alert alert-success small" else "alert alert-warning small"
      shiny::div(class = cls, sprintf(
        "n = %d, %.0f%% AI users, log-likelihood %.1f, %s maximum. The naive OLS curve ignores that workers who gain more choose higher intensity.",
        nrow(r$data), 100 * r$share_users, r$fit$loglik, if (r$regular) "regular" else "irregular"))
    })
    output$dose <- shiny::renderPlot(plot_dose_response(ok()), res = 96)
    output$eff <- shiny::renderPlot(plot_intensity_effects(ok()), res = 96)
    output$coef <- DT::renderDT(de_coef_table(ok()$coef))
    output$ms <- DT::renderDT(DT::formatRound(DT::datatable(ok()$multistart, rownames = FALSE,
                                                            options = list(dom = "t")), c("loglik", "max_score"), 3))
    res
  })
}

mod_modelE_ui <- function(id) {
  ns <- shiny::NS(id)
  sidebar <- bslib::sidebar(
    width = 320, open = "always",
    shiny::numericInput(ns("N"), "Workers", 600, min = 100, max = 5000, step = 100),
    shiny::sliderInput(ns("T"), "Periods", 3, 12, 8, 1),
    shiny::sliderInput(ns("psi"), "Learning by doing (psi)", 0, 0.2, 0.05, 0.01),
    shiny::sliderInput(ns("sd_k"), "Uncertainty about complementarity (sd of kappa)", 0.05, 0.6, 0.25, 0.01),
    shiny::sliderInput(ns("sigma_e"), "Signal noise in AI earnings", 0.05, 0.8, 0.3, 0.01),
    shiny::sliderInput(ns("mu_k"), "Mean initial complementarity", -0.4, 0.4, 0, 0.01),
    shiny::sliderInput(ns("cost"), "Cost of AI use", -0.2, 0.4, 0.1, 0.01),
    shiny::sliderInput(ns("beta"), "Discount factor (fixed in estimation)", 0, 0.99, 0.95, 0.01),
    shiny::radioButtons(ns("method"), "Estimator", inline = TRUE,
                        c("CCP (fast)" = "ccp", "NFXP (full ML)" = "nfxp")),
    shiny::numericInput(ns("seed"), "Random seed", 1, min = 1, step = 1),
    shiny::actionButton(ns("go"), "Simulate and estimate", icon = shiny::icon("play"),
                        class = "btn-primary w-100"),
    shiny::helpText("Fits RoyAI::roy_dynamic(). NFXP solves the model inside the likelihood and takes longer.")
  )
  bslib::layout_sidebar(
    sidebar = sidebar, fillable = FALSE,
    shiny::uiOutput(ns("status")),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(full_screen = TRUE, bslib::card_header("Adoption over time"),
                  shiny::plotOutput(ns("path"), height = 340)),
      bslib::card(full_screen = TRUE, bslib::card_header("Gains from AI over time"),
                  shiny::plotOutput(ns("gains"), height = 340))),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(bslib::card_header("Parameters: truth vs estimate"), DT::DTOutput(ns("coef"))),
      bslib::card(bslib::card_header("Dynamic effects by period"), DT::DTOutput(ns("eff"))))
  )
}

mod_modelE_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    res <- shiny::eventReactive(input$go, {
      p <- modelE_params(N = input$N, T = input$T, seed = input$seed, psi = input$psi,
                         sd_k = input$sd_k, sigma_e = input$sigma_e, mu_k = input$mu_k,
                         cost = input$cost, beta = input$beta, method = input$method)
      shiny::withProgress(message = "Estimating Model E", value = 0.3,
                          tryCatch(run_modelE(p), error = function(e) e))
    }, ignoreNULL = FALSE)
    ok <- shiny::reactive({
      r <- res()
      if (inherits(r, "error")) shiny::validate(paste("Estimation failed:", conditionMessage(r)))
      r
    })
    output$status <- shiny::renderUI({
      e <- ok()$effects
      shiny::div(class = "alert alert-info small", sprintf(
        "In period 1, %.0f%% of adopters expect a negative current net gain: they adopt for the option value of learning and the skills acquired by doing. A myopic worker would adopt at %.0f%% instead of %.0f%%.",
        100 * e$option_value_adopters[1], 100 * e$p_myopic[1], 100 * e$p_dynamic[1]))
    })
    output$path <- shiny::renderPlot(plot_adoption_path(ok()), res = 96)
    output$gains <- shiny::renderPlot(plot_dynamic_gains(ok()), res = 96)
    output$coef <- DT::renderDT(de_coef_table(ok()$coef))
    output$eff <- DT::renderDT({
      e <- ok()$effects[, c("t", "share_ai", "ATE", "ATT", "ATU", "naive_gap", "option_value_adopters")]
      DT::formatRound(DT::datatable(e, rownames = FALSE, options = list(dom = "t", pageLength = 20)),
                      names(e)[-1], 3)
    })
    res
  })
}
