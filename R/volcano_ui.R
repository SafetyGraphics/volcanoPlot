#' Volcano Plot Module - UI
#'
#' Modularized user interface for AE Volcano plot
#'
#' @param id module id
#'
#' @return returns shiny module UI
#'
#' @import shiny
#' @importFrom DT DTOutput
#'
#' @export
#'

volcano_ui <- function(id) {
    ns <- NS(id)

    ## Show sidebar
    sidebar <- sidebarPanel(
        # Calculation type input
        selectizeInput(
            ns("statistic"),
            label = "Measure of Association",
            choices = c(
                "Risk Ratio",
                "Risk Difference"
            )
        ),

        # Stratification input
        selectizeInput(
            ns("stratification_col"),
            label = "System Organ Glass / Preferred Term",
            choices = c()
        )
    )

    # show main panel with plots, data tables
    main <- mainPanel(
        plotOutput(
            ns("volcano_plot"),
            height = "650px",
            hover = hoverOpts(ns("plot_hover"), delay = 50),
            brush = brushOpts(ns("plot_brush"), resetOnNew = FALSE)
        ),
        tags$small(wellPanel(htmlOutput(ns("footnote")))),
        tabsetPanel(
            id = ns("table_wrap"), type = "tabs",
            tabPanel(
                "Comparisons",
                div(
                    h5(htmlOutput(ns("comparison_info"))),
                    DTOutput(ns("comparison_listing"))
                )
            ),
            tabPanel(
                "Adverse Events",
                div(
                    h5(htmlOutput(ns("ae_info"))),
                    DTOutput(ns("ae_listing"))
                )
            )
        )
    )

    ## bring components together as complete ui
    ui <- fluidPage(
        sidebarLayout(
            sidebar,
            main,
            position = c("right"),
            fluid = TRUE
        )
    )

    return(ui)
}
