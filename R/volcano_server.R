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
#' @importFrom purrr map
#' @export

volcano_server <- function(input, output, session, params) {
    ns <- session$ns
    cat('starting server')
    
    ## create a custom mapping for stats calculation
    mapping<-reactive({
        print(params()$settings)
        dm<-params()$data$dm
        reference_group <- params()$settings$dm$treatment_values$group1
        all_groups <- unique(dm[[params()$settings$dm$treatment_col]])
        comparison_groups <- all_groups[all_groups != reference_group]
        
        mapping <- list(
            stratification_col=input$stratification_values, 
            group_col=params()$settings$dm$treatment_col, 
            reference_group=reference_group,
            comparison_group=comparison_groups,
            id_col=params()$settings$dm$id_col
        )  
        print(mapping)
        return(mapping)
    })


    ## calculate the stats
    stats<-reactive({
        cat("getting stats")
        
        stats <- mapping()$comparison_group %>% map(function(comp_group){
            comp_mapping <- mapping()
            comp_mapping$comparison_group <- comp_group
            stats <- getStats(
                dfAE=params()$data$aes, 
                dfDemog=params()$data$dm,
                settings=comp_mapping,
                stat=input$calculation_type 
            )
            return(stats)
        })%>% 
        bind_rows

        print(stats)
    })

    ## Output plots
    output$volcanoPlot <- renderUI({
        plots <- volcanoPlot(
            stats() 
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
        cat('print stats_sub()$strata: ')
        print(stats_sub()$strata)
        cat('print params()$data$aes: ')
        print(params()$data$aes)
        
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
