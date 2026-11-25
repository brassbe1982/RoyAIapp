# -----------------------------------------------------------------------------
# mod_simulator.R -- interactive Roy simulator.
# Returns a reactive with the current simulated scenario, so the estimation
# module can fit models to the simulated sample and compare with the truth.
# -----------------------------------------------------------------------------

mod_simulator_ui <- function(id) {
  ns <- shiny::NS(id)
  tip <- function(label, text) {
    shiny::span(label, bslib::tooltip(shiny::icon("circle-info", class = "text-muted small"), text))
  }
  sidebar <- bslib::sidebar(
    width = 330, open = "always",
    bslib::accordion(
      id = ns("acc"), open = c("Model", "Dependence (copula)"), multiple = TRUE,
      bslib::accordion_panel(
        "Model", icon = shiny::icon("diagram-project"),
        shiny::radioButtons(ns("J"), NULL, inline = TRUE,
                            c("Binary: adopt AI or not" = 2,
                              "Three regimes" = 3)),
        shiny::numericInput(ns("n"), tip("Sample size", "Workers simulated."), 3000,
                            min = 200, max = 50000, step = 500),
        shiny::numericInput(ns("seed"), "Random seed", 1, min = 1, step = 1)
      ),
      bslib::accordion_panel(
        "Dependence (copula)", icon = shiny::icon("link"),
        shiny::selectInput(ns("family"), "Copula family", app_families()),
        shiny::sliderInput(ns("tau0"), tip("Kendall tau (U0, V)",
          "Dependence between human-only earnings shocks and the taste for AI. Negative = people with high human-only earnings resist AI (comparative advantage)."),
          -0.9, 0.9, -0.3, 0.05),
        shiny::sliderInput(ns("tau1"), tip("Kendall tau (U1, V)",
          "Dependence between AI-regime earnings shocks and the taste for AI. Positive = selection on gains."),
          -0.9, 0.9, 0.3, 0.05),
        shiny::conditionalPanel(
          sprintf("input['%s'] == '3'", ns("J")),
          shiny::sliderInput(ns("tau2"), "Kendall tau (U2, V)", -0.9, 0.9, 0.45, 0.05)),
        shiny::conditionalPanel(
          sprintf("input['%s'] == 't'", ns("family")),
          shiny::sliderInput(ns("nu"), "t-copula degrees of freedom", 2.5, 30, 5, 0.5))
      ),
      bslib::accordion_panel(
        "Human-AI complementarity", icon = shiny::icon("handshake"),
        shiny::sliderInput(ns("premium"), tip("AI productivity premium",
          "Intercept gap between AI-augmented and human-only earnings (log points)."),
          -1, 1.5, 0.5, 0.05),
        shiny::sliderInput(ns("complementarity"), tip("AI-skill complementarity",
          "Extra return to skill (x1) when working with AI. Positive = AI complements skill; negative = AI substitutes for it."),
          -0.6, 0.8, 0.2, 0.05),
        shiny::sliderInput(ns("skill_return"), "Return to skill, human-only", 0, 0.8, 0.3, 0.05),
        shiny::sliderInput(ns("sigma0"), "Earnings dispersion, human-only", 0.3, 2, 0.8, 0.05),
        shiny::sliderInput(ns("sigma1"), "Earnings dispersion, AI regime", 0.3, 2, 1, 0.05)
      ),
      bslib::accordion_panel(
        "Selection", icon = shiny::icon("filter"),
        shiny::sliderInput(ns("cost"), tip("Adoption cost",
          "Shifts the selection index down: higher cost, fewer adopters."), -1.5, 1.5, -0.2, 0.05),
        shiny::sliderInput(ns("iv"), tip("Instrument strength",
          "Coefficient of the two excluded cost shifters z1, z2 (e.g. AI access). Near 0 = weak instrument."),
          0, 1.5, 0.7, 0.05),
        shiny::conditionalPanel(
          sprintf("input['%s'] == '3'", ns("J")),
          shiny::sliderInput(ns("kappa2"), "AI-dominant threshold", 0.3, 3, 1.2, 0.1))
      )
    ),
    shiny::actionButton(ns("go"), "Simulate", icon = shiny::icon("play"), class = "btn-primary w-100"),
    shiny::helpText("Parameters update the results when you press Simulate.")
  )

  bslib::layout_sidebar(
    sidebar = sidebar, fillable = FALSE,
    shiny::uiOutput(ns("boxes")),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Marginal treatment effect",
                           bslib::tooltip(shiny::icon("circle-info"),
                             "The gain from AI for the worker at resistance u_S, at average covariates. Low u_S = keenest adopters. Horizontal lines: ATE / ATT / ATU.")),
        shiny::plotOutput(ns("mte"), height = 360)),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Why naive comparisons mislead"),
        DT::DTOutput(ns("sample")),
        bslib::card_footer(shiny::textOutput(ns("decomp"), inline = TRUE)))
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::navset_card_underline(
        full_screen = TRUE, title = "Earnings distributions",
        bslib::nav_panel("Potential", shiny::plotOutput(ns("earn_pot"), height = 330)),
        bslib::nav_panel("Observed", shiny::plotOutput(ns("earn_obs"), height = 330))),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Sorting on gains"),
        shiny::plotOutput(ns("selection"), height = 330))
    ),
    bslib::card(
      bslib::card_header("Population treatment parameters",
                         shiny::downloadButton(ns("dl"), "Download sample (CSV)",
                                               class = "btn-sm btn-outline-secondary float-end")),
      DT::DTOutput(ns("truth")))
  )
}

mod_simulator_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    params <- shiny::eventReactive(input$go, {
      J <- as.integer(input$J)
      sim_params(J = J, family = input$family,
                 tau = c(input$tau0, input$tau1, input$tau2)[seq_len(J)], nu = input$nu,
                 premium = input$premium, complementarity = input$complementarity,
                 skill_return = input$skill_return,
                 sigma = c(input$sigma0, input$sigma1, input$sigma1 * 1.2)[seq_len(J)],
                 cost = input$cost, iv_strength = input$iv, kappa2 = input$kappa2,
                 n = input$n, seed = input$seed)
    }, ignoreNULL = FALSE)

    scen <- shiny::reactive({
      p <- params()
      shiny::validate(shiny::need(p$n <= 50000, "Please keep the sample size at or below 50,000."))
      shiny::withProgress(message = "Simulating", value = 0.5, simulate_scenario(p))
    })

    output$boxes <- shiny::renderUI({
      s <- scen(); tr <- s$truth; main <- tr$contrast == tr$contrast[1]
      val <- function(p) { v <- tr$value[main & tr$parameter == p]; if (length(v)) sprintf("%.3f", v) else "n/a" }
      lab <- regime_labels(s$params$J)
      share_ai <- 1 - s$shares$share[1]
      att_name <- if (s$params$J == 2) "ATT" else paste0("TT | ", lab[2])
      atu_name <- if (s$params$J == 2) "ATU" else paste0("TT | ", lab[1])
      bslib::layout_column_wrap(
        width = 1 / 5, fill = FALSE,
        bslib::value_box("Use AI", sprintf("%.1f%%", 100 * share_ai),
                         shiny::p(paste(sprintf("%s %.0f%%", s$shares$regime, 100 * s$shares$share), collapse = " / "),
                                  class = "small"),
                         showcase = shiny::icon("robot"), theme = "primary"),
        bslib::value_box("ATE", val("ATE"), shiny::p("Average gain, everyone", class = "small"),
                         showcase = shiny::icon("users")),
        bslib::value_box("ATT", val(att_name), shiny::p("Gain for those who use AI", class = "small"),
                         showcase = shiny::icon("user-check")),
        bslib::value_box("ATU", val(atu_name), shiny::p("Gain for those who don't", class = "small"),
                         showcase = shiny::icon("user-xmark")),
        bslib::value_box("LATE", val("LATE"),
                         shiny::p(if (s$params$J == 2) "Compliers, z2: 0 to 1" else "Binary model only", class = "small"),
                         showcase = shiny::icon("arrows-left-right"))
      )
    })

    output$mte <- shiny::renderPlot({
      s <- scen(); tr <- s$truth
      ref <- tr[tr$contrast == tr$contrast[1] & tr$parameter %in% c("ATE", "ATT", "ATU"), ]
      plot_mte(s$mte, truth = ref)
    }, res = 96)

    output$sample <- DT::renderDT({
      x <- scen()$sample
      DT::formatRound(DT::datatable(fillContainer = FALSE, x, rownames = FALSE, colnames = c("", "Value"),
                                    options = list(dom = "t", ordering = FALSE)), 2, 3)
    })
    output$decomp <- shiny::renderText({
      x <- scen()$sample$value
      sprintf("Naive gap %.3f = ATT %.3f + selection bias %.3f (sample values, %s vs %s).",
              x[1], x[3], x[5], utils::tail(regime_labels(scen()$params$J), 1), "Human-only")
    })
    output$earn_pot <- shiny::renderPlot(plot_earnings(scen()$data, scen()$params$J, "potential"), res = 96)
    output$earn_obs <- shiny::renderPlot(plot_earnings(scen()$data, scen()$params$J, "observed"), res = 96)
    output$selection <- shiny::renderPlot(plot_selection(scen()$data, scen()$params$J), res = 96)
    output$truth <- DT::renderDT({
      DT::formatRound(DT::datatable(fillContainer = FALSE, scen()$truth, rownames = FALSE,
                                    options = list(dom = "t", pageLength = 20, ordering = FALSE)),
                      "value", 4)
    })
    output$dl <- shiny::downloadHandler(
      filename = function() sprintf("royai_sim_%s_seed%s.csv", scen()$params$family, scen()$params$seed),
      content = function(file) utils::write.csv(scen()$data, file, row.names = FALSE))

    scen
  })
}
