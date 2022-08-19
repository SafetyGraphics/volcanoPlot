#' Volcano Plot Module - UI
#'
#' @param id module id
#'
#' @return returns shiny module UI
#'
#' @import shiny
#' @importFrom plotly plotlyOutput
#' @importFrom DT DTOutput
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
        )
    )

    main <- mainPanel(
        plotlyOutput(ns("volcanoPlot"), height = "650px"),
        h3(textOutput(ns("click"))),
        DTOutput(ns("aeListing"))
    )
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

