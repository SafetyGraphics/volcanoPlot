#' Volcano Plot Module - Server
#'
#' @param input module input
#' @param output module output
#' @param session module session
#' @param params parameters object with `data` and `settings` options.
#'
#' @return returns shiny module Server function
#'
#' @import shiny
#' @importFrom plotly renderPlotly
#'
#' @export

volcano_server <- function(input, output, session, params) {
    ns <- session$ns
    print('starting server')
    # create a custom mapping for stats calculation
    mapping<-reactive({
        print(params()$settings)
        list(
            stratification_col=params()$settings$aes$bodsys_col,
            group_col=params()$settings$dm$treatment_col, 
            reference_group=params()$settings$dm$treatment_values$group1,
            comparison_group=params()$settings$dm$treatment_values$group2,
            id_col=params()$settings$dm$id_col
        )  
    })
    print(mapping)

    # calculate the stats
    stats<-reactive({
        print("getting stats")
        getStats(
            dfAE=params()$data$aes, 
            dfDemog=params()$data$dm,
            settings=mapping(),
            stat=input$calculation_type
        )
    })
        
    # draw the chart
    output$volcanoPlot <- renderPlotly({
        volcanoPlot(
            stats()
        )
    })
}
