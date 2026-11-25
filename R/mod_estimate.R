# -----------------------------------------------------------------------------
# mod_estimate.R -- fit roy() to simulated, example or uploaded data and
# compare dependence (copula) families.
# -----------------------------------------------------------------------------

mod_estimate_ui <- function(id) {
  ns <- shiny::NS(id)
  sidebar <- bslib::sidebar(
    width = 340, open = "always",
    bslib::accordion(
      open = c("Data", "Specification", "Estimator"), multiple = TRUE,
      bslib::accordion_panel(
        "Data", icon = shiny::icon("database"),
        shiny::radioButtons(ns("source"), NULL,
                            c("Simulator sample" = "simulator",
                              "PIAAC-shaped example (synthetic)" = "piaac",
                              "Upload CSV" = "upload")),
        shiny::conditionalPanel(
          sprintf("input['%s'] == 'upload'", ns("source")),
          shiny::fileInput(ns("file"), NULL, accept = c(".csv", "text/csv"),
                           placeholder = "CSV with a 0/1 or 0/1/2 regime"),
          shiny::radioButtons(ns("sep"), "Separator", inline = TRUE,
                              c("Comma" = ",", "Semicolon" = ";", "Tab" = "\t"))),
        shiny::uiOutput(ns("data_note"))
      ),
      bslib::accordion_panel(
        "Specification", icon = shiny::icon("sliders"),
        shiny::selectInput(ns("y"), "Outcome (log earnings)", NULL),
        shiny::selectInput(ns("d"), "Regime: 0/1 = Model A, 0/1/2 = Model B", NULL),
        shiny::selectizeInput(ns("x"), "Outcome and selection regressors", NULL, multiple = TRUE),
        shiny::selectizeInput(ns("z"), "Excluded from outcome (instruments)", NULL, multiple = TRUE)
      ),
      bslib::accordion_panel(
        "Estimator", icon = shiny::icon("gears"),
        shiny::radioButtons(ns("method"), NULL, inline = TRUE,
                            c("Full ML (copula)" = "ml", "Two-step" = "twostep")),
        shiny::conditionalPanel(
          sprintf("input['%s'] == 'ml'", ns("method")),
          shiny::checkboxGroupInput(ns("families"), "Dependence families to compare",
                                    app_families(), selected = c("gaussian", "frank", "gumbel")),
          shiny::helpText("Non-Gaussian families are multistarted (independence, two-step tau,",
                          "flipped tau) and the best maximum is kept.")),
        shiny::helpText("Model B (three regimes) uses MNL + Dubin-McFadden two-step.")
      )
    ),
    shiny::actionButton(ns("fit"), "Estimate", icon = shiny::icon("calculator"),
                        class = "btn-primary w-100"),
    shiny::uiOutput(ns("spec_msgs"))
  )

  bslib::layout_sidebar(
    sidebar = sidebar, fillable = FALSE,
    bslib::navset_card_underline(
      id = ns("tabs"), full_screen = TRUE, title = "Results",
      bslib::nav_panel(
        "Family comparison", value = "compare",
        shiny::uiOutput(ns("compare_note")),
        DT::DTOutput(ns("compare")),
        shiny::plotOutput(ns("compare_plot"), height = 300)),
      bslib::nav_panel(
        "Selected fit", value = "fit",
        bslib::layout_columns(
          col_widths = c(4, 8),
          shiny::selectInput(ns("pick"), "Fit to inspect", NULL),
          shiny::uiOutput(ns("fit_note"))),
        bslib::layout_columns(
          col_widths = c(6, 6),
          bslib::card(bslib::card_header("Coefficients"), DT::DTOutput(ns("coefs"))),
          bslib::card(bslib::card_header("Treatment effects"), DT::DTOutput(ns("effects")),
                      bslib::card_header("Multistart runs"), DT::DTOutput(ns("ms"))))),
      bslib::nav_panel(
        "MTE", value = "mte",
        shiny::plotOutput(ns("mte"), height = 380),
        shiny::uiOutput(ns("mte_note"))),
      bslib::nav_panel(
        "Diagnostics", value = "diag",
        shiny::verbatimTextOutput(ns("diag"))),
      bslib::nav_panel(
        "Data preview", value = "data",
        DT::DTOutput(ns("preview"))),
      bslib::nav_spacer(),
      bslib::nav_item(shiny::downloadButton(ns("dl_tab"), "Comparison CSV",
                                            class = "btn-sm btn-outline-secondary")),
      bslib::nav_item(shiny::downloadButton(ns("dl_fit"), "Fits (.rds)",
                                            class = "btn-sm btn-outline-secondary"))
    )
  )
}

mod_estimate_server <- function(id, sim) {
  shiny::moduleServer(id, function(input, output, session) {

    piaac <- shiny::reactive(example_piaac_data())

    uploaded <- shiny::reactive({
      shiny::req(input$file)
      tryCatch(utils::read.csv(input$file$datapath, sep = input$sep, stringsAsFactors = TRUE,
                               na.strings = c("", "NA", ".n", ".v", ".")),
               error = function(e) shiny::validate(paste("Could not read the file:", conditionMessage(e))))
    })

    dat <- shiny::reactive({
      switch(input$source,
             simulator = {
               d <- sim()$data
               d[, c("y", "s", "d", "x1", "z1", "z2", grep("^y[0-9]$", names(d), value = TRUE))]
             },
             piaac = piaac(),
             upload = uploaded())
    })

    output$data_note <- shiny::renderUI({
      d <- dat()
      txt <- switch(input$source,
        simulator = sprintf("Current simulator sample: %s family, n = %s. True effects are on the Simulator tab.",
                            family_label(sim()$params$family), format(nrow(d), big.mark = ",")),
        piaac = paste("Synthetic data in the PIAAC Cycle 2 layout, prepared by piaac_onet_prep().",
                      "Adoption is the project's proxy: AI-exposed occupation x high ICT use at work.",
                      "Not real PIAAC results; see the Stored results tab for those."),
        upload = sprintf("%s rows, %s columns.", format(nrow(d), big.mark = ","), ncol(d)))
      shiny::div(class = "small text-muted mt-2", txt)
    })

    # refresh variable pickers when the data change, with source defaults
    shiny::observeEvent(list(dat(), input$source), {
      d <- dat(); nm <- names(d); num <- nm[vapply(d, is.numeric, TRUE)]
      reg <- nm[vapply(d, function(v) {
        u <- unique(stats::na.omit(v)); length(u) %in% 2:3 && all(u %in% 0:2)
      }, TRUE)]
      def <- default_spec(input$source)
      shiny::updateSelectInput(session, "y", choices = num,
                               selected = def$y %||% setdiff(num, reg)[1])
      shiny::updateSelectInput(session, "d", choices = reg, selected = def$d %||% reg[1])
      shiny::updateSelectizeInput(session, "x", choices = nm, selected = def$x)
      shiny::updateSelectizeInput(session, "z", choices = nm, selected = def$z)
    })

    problems <- shiny::reactive(check_spec(dat(), input$y, input$d, input$x, input$z))
    output$spec_msgs <- shiny::renderUI({
      p <- problems(); if (!length(p)) return(NULL)
      shiny::div(class = "alert alert-warning small mt-3 mb-0", lapply(p, shiny::p, class = "mb-1"))
    })

    result <- shiny::eventReactive(input$fit, {
      hard <- grep("^Note:", problems(), value = TRUE, invert = TRUE)
      shiny::validate(shiny::need(!length(hard), paste(hard, collapse = " ")))
      fams <- if (input$method == "ml") input$families else "gaussian"
      shiny::validate(shiny::need(length(fams) > 0, "Tick at least one dependence family."))
      d <- dat()
      keep <- unique(c(input$y, input$d, input$x, input$z))
      d <- d[stats::complete.cases(d[, keep, drop = FALSE]), , drop = FALSE]
      shiny::withProgress(message = "Fitting", value = 0, {
        fit_families(d, input$y, input$d, input$x, input$z, families = fams,
                     method = input$method,
                     progress = function(i, n, lab)
                       shiny::setProgress((i - 1) / n, detail = sprintf("%s (%d of %d)", family_label(lab), i, n)))
      })
    })

    shiny::observeEvent(result(), {
      r <- result()
      if (!nrow(r$table)) return()
      shiny::updateSelectInput(session, "pick", choices = stats::setNames(r$table$family, family_label(r$table$family)),
                               selected = r$table$family[1])
      if (length(r$errors))
        shiny::showNotification(paste("Failed:", paste(names(r$errors), r$errors, sep = ": ", collapse = "; ")),
                                type = "warning", duration = 10)
    })

    output$compare_note <- shiny::renderUI({
      if (input$fit == 0) return(shiny::div(class = "text-muted p-3",
        "Choose data and a specification, then press Estimate."))
      r <- result()
      shiny::validate(shiny::need(length(r$fits) > 0, paste(
        "No model could be fitted.", paste(names(r$errors), r$errors, sep = ": ", collapse = "; "))))
      best <- r$table$family[1]
      msg <- if (r$model == "A" && r$method == "ml") {
        txt <- sprintf("Model A, full ML, n = %s. Best regular maximum by AIC: %s (Akaike weight %.2f).",
                       format(r$fits[[best]]$spec$n, big.mark = ","), family_label(best), r$table$akaike_weight[1])
        bad <- r$table$family[!r$table$regular]
        if (length(bad)) txt <- paste(txt, sprintf(paste(
          "Irregular maximum (Hessian not positive definite or large score), excluded from the ranking: %s."),
          paste(family_label(bad), collapse = ", ")))
        fixed <- r$table$family[!is.na(r$table$note)]
        if (length(fixed)) txt <- paste(txt, sprintf(paste(
          "%s: the multistart run kept a degenerate boundary maximum, so the regular default-start",
          "maximum is shown (see the note column)."), paste(family_label(fixed), collapse = ", ")))
        txt
      } else sprintf("Model %s, two-step, n = %s.", r$model, format(r$fits[[1]]$spec$n, big.mark = ","))
      shiny::p(class = "mt-2", msg)
    })

    fmt_tab <- function(x, digits = 3) {
      num <- names(x)[vapply(x, is.double, TRUE)]
      DT::formatRound(DT::datatable(fillContainer = FALSE, x, rownames = FALSE, options = list(dom = "t", scrollX = TRUE,
                                                                        pageLength = 50)), num, digits)
    }
    output$compare <- DT::renderDT(fmt_tab(result()$table))
    output$compare_plot <- shiny::renderPlot({
      tab <- result()$table
      shiny::req(nrow(tab) > 0, any(is.finite(tab$ATE)))
      plot_family_effects(tab)
    }, res = 96)

    picked <- shiny::reactive({
      r <- result(); shiny::req(input$pick %in% names(r$fits)); r$fits[[input$pick]]
    })
    output$fit_note <- shiny::renderUI({
      f <- picked()
      shiny::p(class = "small text-muted mt-4",
               sprintf("%s | method %s | converged: %s | log-likelihood %.2f",
                       paste(class(f)[1]), f$method, isTRUE(f$converged),
                       tryCatch(as.numeric(stats::logLik(f)), error = function(e) NA)))
    })
    output$coefs <- DT::renderDT(fmt_tab(coef_table(picked()), 4))
    output$effects <- DT::renderDT(fmt_tab(RoyAI::roy_effects(picked()), 4))
    output$ms <- DT::renderDT({
      ms <- picked()$multistart
      if (is.null(ms)) ms <- data.frame(note = "Not multistarted (Gaussian, two-step or Model B).")
      fmt_tab(as.data.frame(ms), 3)
    })

    mte_est <- shiny::reactive(estimated_mte(picked()))
    output$mte <- shiny::renderPlot({
      m <- mte_est()
      shiny::validate(shiny::need(!is.null(m), "The MTE is available for Model A fits."))
      truth <- if (input$source == "simulator" && sim()$params$J == 2) sim()$mte else NULL
      plot_mte(truth, estimated = m)
    }, res = 96)
    output$mte_note <- shiny::renderUI({
      if (input$source == "simulator" && sim()$params$J == 2)
        shiny::p(class = "small text-muted", "Solid line: true MTE of the simulator; dashed with band: estimate and 95% pointwise CI.")
      else shiny::p(class = "small text-muted", "Estimate with 95% pointwise CI, at mean covariates.")
    })
    output$diag <- shiny::renderPrint(print(RoyAI::roy_diagnostics(picked())))
    output$preview <- DT::renderDT(DT::datatable(fillContainer = FALSE, utils::head(dat(), 500), rownames = FALSE,
                                                 options = list(scrollX = TRUE, pageLength = 10)))

    output$dl_tab <- shiny::downloadHandler(
      filename = function() "royai_family_comparison.csv",
      content = function(file) utils::write.csv(result()$table, file, row.names = FALSE))
    output$dl_fit <- shiny::downloadHandler(
      filename = function() "royai_fits.rds",
      content = function(file) saveRDS(result()$fits, file))

    result
  })
}
