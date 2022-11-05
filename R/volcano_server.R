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
    
    ## create a custom mapping for stats calculation
    mapping<-reactive({
        print(params()$settings)
        list(
            stratification_col=input$stratification_values, 
            group_col=params()$settings$dm$treatment_col, 
            reference_group=params()$settings$dm$treatment_values$group1,
            comparison_group=params()$settings$dm$treatment_values$group2,
            id_col=params()$settings$dm$id_col
        )  
    })
    print(mapping)

    ## calculate the stats
    stats<-reactive({
        print("getting stats")
        getStats(
            dfAE=params()$data$aes, 
            dfDemog=params()$data$dm,
            settings=mapping(),
            stat=input$calculation_type
        )
    })

    # groups <- reactive({
    #     c(
    #         mapping()$comparison_group, 
    #         mapping()$reference_group
    #     )
    # })

    ## Output plots
    output$volcanoPlot <- renderUI({
        plots <- volcanoPlot(
            stats() 
            # GroupLabels = groups()
        )
        
        tagList(plots)
    })

    ## Take a clicked point from the plot
    stats_sub <- reactive({
        plotly_d <- event_data("plotly_click")
        
        if (is.null(plotly_d)) {
            NULL
        }else{
          #change pointNumber depending on the curveNumber
            if (plotly_d[1] == 0 ){
              stats()[plotly_d$pointNumber+4,]
            }else if (plotly_d[1] == 1 ){
              stats()[plotly_d$pointNumber+1,]
            }
        }
    })

    ## create subset of data to show when a point is click
    ae_sub <- reactive({
        print(paste0('print stats_sub()$strata: ', stats_sub()$strata))
        print(paste0("print params()$data$aes: ", params()$data$aes))
        
        all<-params()$data$aes
        # subset to selected strata
        sub1<-all[all[params()$settings$aes$bodsys_col]==stats_sub()$strata,]
        
        # print("print sub1:")
        # print(sub1)
        # print("print groups():")
        # print(groups())
 
        
        # # subset to selected groups
        # dm <- params()$data$dm
        # 
        # print("print params()$data$dm")
        # print(params()$data$dm)
        # print("print params()$data$id_col")
        # print(params()$settings$dm$id_col)
        # print("print dm[dm[params()$settings$dm$treatment_col] == groups(),]:")
        # print( dm[dm[params()$settings$dm$treatment_col] == groups(),] )
        # 
        # # Jeremy's code  
        # # ids <- dm[dm[params()$settings$dm$treatment_col] %in% groups(),]%>%
        # ids <- dm[dm[params()$settings$dm$treatment_col] == groups(),]%>%
        #    pull(params()$settings$dm$id_col)
        # print("print ids object:")
        # print(ids)
        # 
        # # Jeremy's code
        # # sub2<-sub1[sub1[params()$settings$dm$id_col,] %in% ids,]
        # 
        # #examples for subsetting
        # # data[data$x1 %in% vec, ] 
        # # filter(data, x1 %in% vec) 
        # # sub2<-sub1[sub1[params()$settings$dm$id_col] %in% c(ids),]
        # sub2<-filter(sub1, params()$settings$dm$id_col %in% c(ids))
        # print("print sub2:")
        # print(sub2)
        # 
        # # sub1
        # sub2
        
        sub1
    })
    
    ## Output text instructing how to click and get table
    output$click <- renderText({
        if (is.null(stats_sub())) {
            "Click events appear here (double-click to clear)" }
        else {
            paste("Showing details for AEs in",stats_sub()$strata)
        }
    })

    # Output table of data from the clicked point
    output$aeListing <- renderDT(ae_sub())
}
