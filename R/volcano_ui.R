#' Volcano Plot Module - UI
#'
#' @param id module id
#'
#' @return returns shiny module UI
#'
#' @import shiny
#' @importFrom plotly plotlyOutput
#'
#' @export
#'

volcano_ui <- function(id) {
    ns <- NS(id)
    sidebar <- sidebarPanel(
        selectInput(
            ns("calculation_type"),
            label="Measure of Association", 
            choices = c(
                "Risk Ratio",
                "Risk Difference"
            )
            ),
            selectInput(
              ns("stratification_values"),
              label="System Organ Glass / Preferred Term",
              choices = c("AEBODSYS", "AEDECOD")
            )
    )

    main <- mainPanel(plotlyOutput(ns("volcanoPlot"), height = "650px"))
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

