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
#' @importFrom plotly renderPlotly event_data
#' @importFrom DT renderDT
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

    groups <- reactive({
        c(
            mapping()$comparison_group, 
            mapping()$reference_group
        )
    })

    # draw the chart
    output$volcanoPlot <- renderPlotly({
        volcanoPlot(
            stats(), 
            GroupLabels = groups()
        )
    })

    stats_sub <- reactive({
        plotly_d <- event_data("plotly_click")
        if (is.null(plotly_d)) {
            NULL
        }else{
            stats()[plotly_d$pointNumber+1,]
        }
    })

    ae_sub <- reactive({
        all<-params()$data$aes
        # subset to selected strata
        sub1<-all[all[params()$settings$aes$bodsys_col]==stats_sub()$strata,]

        # subset to selected groups
        #dm <- params()$data$dm
        #ids <- dm[dm[params()$settings$dm$treatment_col] %in% groups(),]%>%
        #    pull(params()$settings$dm$id_col)
        #print(ids)
        #sub2<-sub1[sub1[params()$settings$dm$id_col,] %in% ids,]

        sub1
    })

    output$click <- renderText({
        if (is.null(stats_sub())) {
            "Click events appear here (double-click to clear)" }
        else {
            paste("Showing details for",stats_sub()$eventN_total, "AEs in",stats_sub()$strata)
        }
    })

    # draw AE listing
    output$aeListing <- renderDT(ae_sub())
}
