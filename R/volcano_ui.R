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
            ns("calculation_type"),
            label="Measure of Association", 
            choices = c(
                "Risk Ratio",
                "Risk Difference"
            )
            ),
        
        # Stratification input
        selectizeInput(
            ns("stratification_values"),
            label="System Organ Glass / Preferred Term",
            choices = c()
        )
    )

    # show main panel with plots, data tables
    main <- mainPanel(
        plotOutput(
            ns("volcanoPlot"), 
            height = "650px", 
            hover = hoverOpts(ns("plot_hover"),delay=50),
            brush = brushOpts(ns("plot_brush"),resetOnNew = FALSE)
        ),
        tags$small(wellPanel(htmlOutput(ns("footnote")))),
        
        tabsetPanel(id=ns("tableWrap"), type = "tabs",
            tabPanel("Comparisons", 
                div(
                    h5(htmlOutput(ns("infoComp"))),
                    DTOutput(ns("compListing")))
                ),
            tabPanel("Adverse Events", 
                div(
                    h5(htmlOutput(ns("infoAE"))),
                    DTOutput(ns("aeListing"))
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

