#' Volcano Plot Module - Server
#'
#' Modularized server for AE volcano plot. 
#' 
#' @param input module input
#' @param output module output
#' @param session module session
#' @param params parameters object with `data` and `settings` options.
#'
#' @return returns shiny module Server function
#'
#' @import shiny
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
    
    strata_cols <- reactive({
        c(
            params()$settings$aes$bodsys_col,
            params()$settings$aes$term_col
        )
    })

    observe({
        updateSelectizeInput(
            session,
            inputId = 'stratification_values',
            choices = strata_cols(),
            selected = strata_cols()[[1]]
        )   
    })

    # calculate the stats
    stats<-reactive({
        req(mapping())
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
            highlights=selected_strata()
        )
    })

    # ############################################
    # # Reactives for interactive brushing/hover
    # ############################################

    #hover data
    hover_data <- reactive({
        nearPoints(
            stats(), 
            input$plot_hover,
            xvar="estimate",
            yvar="logp"
            )
    })

    #selected strata
    selected_strata <- reactive({
        req(stats)
        unique(brushedPoints(
            stats(), 
            input$plot_brush,
            xvar="estimate",
            yvar="logp"
        )$strata)
    })

    #filtered ae data
    sub_aes <- reactive({
        req(selected_strata())
        raw_aes <- params()$data$aes
        sub_aes <- raw_aes %>% filter(.data[[mapping()$stratification_col]] %in% selected_strata())
    })

    #filtered comparison data
    sub_stat <- reactive({
        req(selected_strata())
        stats()[stats()$strata %in% selected_strata(),]
    })

    ##########################
    # Linked table + Header    
    #########################
    output$footnote <- renderUI({
        if( nrow(hover_data()) > 0 ){
            HTML(paste(hover_data()$tooltip,collapse="<hr>"))
        }else{
            'Hover to see point details'
        }
    })

    output$infoComp <- renderUI({
        if( length(selected_strata()) == 1 ){
            HTML(paste(nrow(sub_stat()),"comparisons from <strong>", selected_strata(),"</strong>"))
        }else if(length(selected_strata() > 1)){
            HTML(paste(nrow(sub_stat()), "comparisons from <strong>", length(selected_strata()),"groups </strong>"))
        }else{
            paste("Brush to see listings")
        }
    })

    output$compListing <- renderDT({
        req(sub_stat())
        sub_stat() %>% select(-.data$tooltip)
    })

    output$infoAE <- renderUI({
        if( length(selected_strata()) == 1 ){
            HTML(paste(nrow(sub_aes()),"AEs from <strong>", selected_strata(),"</strong>"))
        }else if(length(selected_strata() > 1)){
            HTML(paste(nrow(sub_aes()), "AEs from <strong>", length(selected_strata()),"groups </strong>"))
        }else{
            paste("Brush to see listings")
        }
    })

    output$aeListing <- renderDT({
        req(sub_aes())
        sub_aes()
    })
    
}
