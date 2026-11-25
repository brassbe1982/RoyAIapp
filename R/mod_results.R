# -----------------------------------------------------------------------------
# mod_results.R -- stored Monte Carlo study and PIAAC copula application.
# -----------------------------------------------------------------------------

mod_results_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::navset_card_underline(
    id = ns("tabs"), full_screen = TRUE,
    bslib::nav_panel(
      "Monte Carlo: performance", value = "mc",
      bslib::layout_columns(
        col_widths = c(3, 3, 6),
        shiny::selectInput(ns("mc_effect"), "Treatment parameter", c("ATE", "ATT", "ATU", "LATE"), "ATT"),
        shiny::selectInput(ns("mc_measure"), "Measure", c("RMSE" = "rmse", "Bias" = "bias", "Coverage" = "coverage")),
        shiny::checkboxGroupInput(ns("mc_tau"), "Kendall tau in the DGP", c("0" = 0, "0.3" = 0.3, "0.6" = 0.6),
                                  selected = c(0.3, 0.6), inline = TRUE)),
      shiny::plotOutput(ns("mc_plot"), height = 460),
      shiny::p(class = "small text-muted",
               "Main design: binary copula Roy model, (tau0, tau1) = (-tau, +tau), 100 replications per cell above n = 2,000.",
               "OLS ignores selection; Two-step is Heckman; FIML is the copula likelihood of the true family.")),
    bslib::nav_panel(
      "Monte Carlo: tables", value = "mc_tables",
      shiny::selectInput(ns("mc_table"), NULL, mc_tables(), width = "420px"),
      DT::DTOutput(ns("mc_tab"))),
    bslib::nav_panel(
      "Monte Carlo: MTE figure", value = "mc_fig",
      shiny::plotOutput(ns("mc_mte"), height = 420),
      shiny::p(class = "small text-muted", "Paper 1, Figure 1: mean estimated MTE across replications with 2.5-97.5% bands.")),
    bslib::nav_panel(
      "PIAAC: copula families", value = "piaac",
      shiny::div(class = "alert alert-info small",
        shiny::strong("Read with care. "),
        "PIAAC Cycle 2, six countries (CHL, CZE, FRA, ISR, LVA, POL). Adoption is a proxy",
        "(AI-exposed occupation x high ICT use at work); ICT use at home is the exclusion restriction.",
        "The ATT is fairly stable across well-fitting families; ATE and ATU are not."),
      shiny::plotOutput(ns("piaac_plot"), height = 380),
      DT::DTOutput(ns("piaac_tab"))),
    bslib::nav_panel(
      "PIAAC: multistart", value = "ms",
      shiny::p(class = "small text-muted",
               "occ2 specification refitted from several starting values per family. The first Frank fit",
               "was a local maximum; the best maxima are below."),
      DT::DTOutput(ns("ms_best")), shiny::hr(), DT::DTOutput(ns("ms_all"))),
    bslib::nav_panel(
      "PIAAC: first run log", value = "log",
      shiny::verbatimTextOutput(ns("piaac_txt")))
  )
}

mod_results_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    mc <- shiny::reactive(read_result("montecarlo", "mc_treatment_effects_all.csv"))
    fmt <- function(x, digits = 3) {
      shiny::validate(shiny::need(!is.null(x), "Results file not found."))
      num <- names(x)[vapply(x, is.double, TRUE)]
      DT::formatRound(DT::datatable(fillContainer = FALSE, x, rownames = FALSE,
                                    options = list(scrollX = TRUE, pageLength = 15)), num, digits)
    }

    output$mc_plot <- shiny::renderPlot({
      shiny::validate(shiny::need(!is.null(mc()), "Monte Carlo results not found."),
                      shiny::need(length(input$mc_tau) > 0, "Pick at least one tau."))
      plot_mc(mc(), input$mc_effect, input$mc_measure, as.numeric(input$mc_tau))
    }, res = 96)
    output$mc_tab <- DT::renderDT(fmt(read_result("montecarlo", input$mc_table)))
    output$mc_mte <- shiny::renderPlot({
      fd <- read_result("montecarlo", "figure1_mte_data.csv")
      shiny::validate(shiny::need(!is.null(fd), "Figure data not found."))
      plot_mc_mte(fd)
    }, res = 96)

    cmp <- shiny::reactive(read_result("piaac", "copula_family_comparison_pv1.csv"))
    output$piaac_plot <- shiny::renderPlot({
      shiny::validate(shiny::need(!is.null(cmp()), "PIAAC results not found."))
      plot_piaac_families(cmp())
    }, res = 96)
    output$piaac_tab <- DT::renderDT(fmt(cmp()))
    output$ms_best <- DT::renderDT(fmt(read_result("piaac", "multistart_best.csv")))
    output$ms_all <- DT::renderDT(fmt(read_result("piaac", "multistart_all.csv")))
    output$piaac_txt <- shiny::renderText({
      x <- read_result("piaac", "piaac_roy_results.txt")
      shiny::validate(shiny::need(!is.null(x), "Results log not found."))
      paste(x, collapse = "\n")
    })
  })
}
