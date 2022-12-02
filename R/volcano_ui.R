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
    
    ## Show sidebar 
    sidebar <- sidebarPanel(
        
        # Calculation type input
        selectInput(
            ns("calculation_type"),
            label="Measure of Association", 
            choices = c(
                "Risk Ratio",
                "Risk Difference"
            )
            ),
        
        # Stratification input
        selectInput(
          ns("stratification_values"),
          label="System Organ Glass / Preferred Term",
          choices = c("AEBODSYS", "AEDECOD")
        )
    )

    # show main panel with plots, data tables
    main <- mainPanel(
        plotOutput(
            ns("volcanoPlot"), 
            height = "650px", 
            click = ns("plot_click"),
            brush = brushOpts(ns("plot_brush"),resetOnNew = TRUE)
        ),
        h3(textOutput(ns("info"))),
        DTOutput(ns("aeListing"))
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

