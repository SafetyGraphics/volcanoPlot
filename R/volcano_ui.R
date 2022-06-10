
#' Safety Outlier Explorer Module - UI
#'
#' @param id module id
#'
#' @return returns shiny module UI
#'
#' @import shiny
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
                "RR", 
                "RD",
                "Risk Ratio",
                "Risk Difference", 
                "Hazard Ratio"
            )
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

