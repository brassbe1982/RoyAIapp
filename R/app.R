# -----------------------------------------------------------------------------
# app.R -- theme, UI shell, server, and the exported launcher.
# -----------------------------------------------------------------------------

royai_theme <- function() {
  bslib::bs_theme(
    version = 5, primary = "#1F4E79", secondary = "#5C6B7A", success = "#009E73",
    info = "#0072B2", warning = "#E69F00",
    base_font = bslib::font_collection("Inter", "system-ui", "-apple-system", "Segoe UI", "Roboto", "sans-serif"),
    "border-radius" = "0.5rem"
  )
}

about_ui <- function() {
  bslib::layout_columns(
    col_widths = c(7, 5),
    bslib::card(
      bslib::card_header("The model"),
      shiny::markdown(c(
        "**RoyAI** studies how workers sort between human-only and AI-based production when",
        "the gains from AI differ across people and are partly unobserved (a generalized Roy model).",
        "",
        "* Potential earnings: Y_j = X'beta_j + U_j for regime j (human-only, AI-augmented, AI-dominant).",
        "* Selection: AI is used when Z'gamma + V > 0 (ordered thresholds for three regimes).",
        "* Dependence: each pair (U_j, V) is joined by a copula (Gaussian, Student-t, Clayton, Gumbel or Frank),",
        "  parameterised by Kendall's tau, so tail behaviour of selection on gains is not forced to be Gaussian.",
        "",
        "**Simulator.** Set complementarity, dependence and selection; see how the ATE, ATT, ATU, LATE and MTE",
        "move, and how much a naive comparison of AI users and non-users is biased.",
        "",
        "**Estimation.** Fit `RoyAI::roy()` by full maximum likelihood for several copula families",
        "(multistarted) or by two-step, then compare AIC, effects and MTE. Model B (three regimes) uses MNL with",
        "the Dubin-McFadden correction.",
        "",
        "**Intensity (Model D).** AI use as a matter of degree: a censored intensity in [0, 1) with a copula",
        "between the outcome shock and the pull towards AI. Fits `RoyAI::roy_continuous()` and compares the",
        "dose-response and effects at a given intensity with the truth and with naive OLS.",
        "",
        "**Dynamics (Model E).** Workers learn their complementarity with AI from AI earnings (Kalman updating)",
        "and acquire it by doing. Fits `RoyAI::roy_dynamic()` by CCP or nested fixed point and shows the option",
        "value of trying AI and the bias of static comparisons.",
        "",
        "**Stored results.** The Paper 1 Monte Carlo study, the Model D and E Monte Carlo checks and the PIAAC Cycle 2 application."))),
    shiny::div(
    bslib::card(
      bslib::card_header("Reference publication"),
      shiny::markdown(c(
        "Niankara, I., Aljallad, Y., Soussi, O., Maghrabi, K., Alkhyeli, M. and Alghool, S. (2025).",
        "*The Impact of Artificial Intelligence on Fresh Graduate Labor Demand: A Theoretical and",
        "Empirical Analysis with Industry and Firm Heterogeneity.* 2025 Global Congress on Emerging",
        "Technologies (GCET), Lyon, France, 2-5 December 2025. IEEE.",
        "[doi:10.1109/GCET68529.2025.11450624](https://doi.org/10.1109/GCET68529.2025.11450624)",
        "",
        "Open version at [BRASS Digital Lab](https://brassbe1982-bdlweb.share.connect.posit.cloud/research/ai-graduate-labor-demand-agentic.html).",
        "",
        "Software: Ibrahim Niankara (author, maintainer). Funded by BRASS Digital Lab.",
        "`citation(\"RoyAI\")` and `citation(\"RoyAIapp\")` give BibTeX entries."))),
    bslib::card(
      bslib::card_header("Session"),
      shiny::tableOutput("about_versions"),
      shiny::markdown(c(
        "Stored results are bundled with the app. To read live output instead, set",
        "`options(royai.results_dir = \"<folder with montecarlo/ and piaac/>\")` before launching."))))
  )
}

app_ui <- function(request) {
  bslib::page_navbar(
    title = shiny::span(shiny::icon("scale-balanced"), "RoyAI"),
    window_title = "RoyAI: Human-AI Roy models",
    id = "nav", theme = royai_theme(), fillable = FALSE, bg = "#1F4E79", inverse = TRUE,
    header = shiny::tags$head(
      shiny::tags$link(rel = "stylesheet", href = "royai-www/styles.css")),
    bslib::nav_panel("Simulator", value = "sim", icon = shiny::icon("flask"), mod_simulator_ui("sim")),
    bslib::nav_panel("Estimation", value = "est", icon = shiny::icon("calculator"), mod_estimate_ui("est")),
    bslib::nav_panel("Intensity (Model D)", value = "modelD", icon = shiny::icon("sliders"), mod_modelD_ui("modelD")),
    bslib::nav_panel("Dynamics (Model E)", value = "modelE", icon = shiny::icon("chart-line"), mod_modelE_ui("modelE")),
    bslib::nav_panel("Stored results", value = "res", icon = shiny::icon("table"), mod_results_ui("res")),
    bslib::nav_panel("About", value = "about", icon = shiny::icon("circle-info"), about_ui()),
    bslib::nav_spacer(),
    bslib::nav_item(shiny::span(class = "navbar-text small", "Generalized Roy models of human-AI complementarity"))
  )
}

app_server <- function(input, output, session) {
  sim <- mod_simulator_server("sim")
  mod_estimate_server("est", sim)
  mod_modelD_server("modelD")
  mod_modelE_server("modelE")
  mod_results_server("res")
  output$about_versions <- shiny::renderTable(data.frame(
    Package = c("RoyAI", "RoyAIapp", "shiny", "bslib", "R"),
    Version = c(as.character(utils::packageVersion("RoyAI")), as.character(utils::packageVersion("RoyAIapp")),
                as.character(utils::packageVersion("shiny")), as.character(utils::packageVersion("bslib")),
                paste(R.version$major, R.version$minor, sep = "."))))
}

#' Launch the RoyAI interactive research platform
#'
#' @param ... passed to [shiny::shinyApp()] options, e.g. `port`, `launch.browser`.
#' @return a shiny.appobj (printed objects launch the app)
#' @export
#' @examples
#' if (interactive()) run_royai_app()
run_royai_app <- function(...) {
  shiny::addResourcePath("royai-www", system.file("app", "www", package = "RoyAIapp"))
  shiny::shinyApp(ui = app_ui, server = app_server, options = list(...))
}
