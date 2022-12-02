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

        return(mapping)
    })


    ## calculate the stats
    stats<-reactive({
        cat("getting stats")
        
        stats <- mapping()$comparison_group %>% 
            map(function(comp_group){
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
    })

    ## Output plots
    output$volcanoPlot <- renderPlot({
        volcanoPlot(
            data=stats(), 
            highlights=list(col=mapping()$stratification_col, vals=selected_strata())
        )
    })

    stat_data <- reactive({
        req(input$plot_click)
        brushedPoints(stats(), input$plot_brush)
    })

    selected_strata <- reactive({
        req(stat_data())
        unique(stat_data()$strata)
    })

    sub_aes <- reactive({
        req(input$plot_click)
        raw_aes <- params()$data$aes
        sub_aes <- raw_aes %>% filter(.data[[mapping()$stratification_col]] %in% selected_strata())
    })

    output$info <- renderText({
        if( nrow(sub_aes()) > 0 ){
            paste("Showing", nrow(sub_aes()),"AEs for:" ,paste(selected_strata(),collapse=" / "))
        }else{
            paste("Brush chart to see AE listing")
        }
    })

    output$aeListing <- renderDT({
        req(selected_strata())
        raw_aes <- params()$data$aes
        sub_aes <- raw_aes %>% filter(.data[[mapping()$stratification_col]] %in% selected_strata())

        return(sub_aes)
    })
}
