
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
#'
#' @export

volcano_server <- function(input, output, session, params) {
    ns <- session$ns
    print('starting server')
    print(params)
    # create a custom mapping for stats calculation
    mapping<-reactive({
        list(
            stratification_col=params()$settings$aes$bodsys_col,
            group_col=params()$settings$dm$trt_col, 
            reference_group=params()$settings$dm[['treatment_values--group1']],
            comparison_group=params()$settings$dm[['treatment_values--group2']],
            id_col=params()$settings$dm$id_col
        )  
    })
    print(mapping)

    # calculate the stats
    stats<-reactive({
        print("getting stats")
        getStats(
            params()$data$aes, 
            params()$data$dm,
            mapping()
        )
    })
        
    # draw the chart
    output$volcanoPlot <- renderPlotly({volcanoPlot(stats())})
}
