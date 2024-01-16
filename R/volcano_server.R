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

    observe({
        updateSelectizeInput(
            session,
            inputId = "stratification_col",
            choices = c(
                'Body System / Organ Class' = params()$aes$mapping$bodsys_col,
                'Preferred Term' = params()$aes$mapping$term_col
            ),
            selected = params()$aes$mapping$bodsys_col
        )
    })

    # Capture [ dm ] mapping and data.
    dm <- reactive({
        req(input$stratification_col)

        mapping <- params()$settings$dm

        # Remove missing values of treatment group.
        data <- params()$data$dm %>%
            filter(
                .data[[ mapping$treatment_col ]] %>% is.na == FALSE,
                .data[[ mapping$treatment_col ]] != ''
            )

        return(
            list(
                mapping = mapping %>%
                    add_treatment_groups(
                        data,
                        input$stratification_col
                    ),
                data = data
            )
        )
    })

    # Capture [ ae ] mapping and data.
    ae <- reactive({
        mapping <- params()$settings$ae

        # Remove invalid subject IDs.
        data <- params()$data$ae %>%
            filter(
                .data[[ mapping$id_col ]] %in% dm()$data[[ dm()$mapping$id_col ]]
            )

        return(
            list(
                mapping = mapping,
                data = data
            )
        )
    })

    # Calculate statistics.
    stats <- reactive({
        req(
            dm(),
            ae(),
            input$statistic
        )

        stats <- dm()$mapping$comparison_group %>%
            map(function(comp_group) {
                comp_mapping <- dm()$mapping
                comp_mapping$comparison_group <- comp_group

                stats <- get_stats(
                    dm = dm()$data,
                    ae = ae()$data,
                    settings = comp_mapping,
                    stat = input$statistic
                )

                return(stats)
            }) %>%
            bind_rows()

        return(stats)
    })

    # selected strata
    selected_strata <- reactive({
        req(stats())

        stats() %>%
            brushedPoints(
                input$plot_brush,
                xvar = "estimate",
                yvar = "logp"
            )$strata %>%
            unique()
    })

    ## Output plots
    output$volcano_plot <- renderPlot({
        req(stats())

        volcano_plot(
            data = stats(),
            highlights = selected_strata()
        )
    })

    # ############################################
    # # Reactives for interactive brushing/hover
    # ############################################

    # hover data
    hover_data <- reactive({
        nearPoints(
            stats(),
            input$plot_hover,
            xvar = "estimate",
            yvar = "logp"
        )
    })

    # filtered ae data
    sub_aes <- reactive({
        req(selected_strata())
        raw_aes <- params()$data$aes
        sub_aes <- raw_aes %>% filter(.data[[mapping()$stratification_col]] %in% selected_strata())
    })

    # filtered comparison data
    sub_stat <- reactive({
        req(selected_strata())
        stats()[stats()$strata %in% selected_strata(), ]
    })

    ##########################
    # Linked table + Header
    #########################
    output$footnote <- renderUI({
        if (nrow(hover_data()) > 0) {
            HTML(paste(hover_data()$tooltip, collapse = "<hr>"))
        } else {
            "Hover to see point details"
        }
    })

    output$comparison_info <- renderUI({
        if (length(selected_strata()) == 1) {
            HTML(paste(nrow(sub_stat()), "comparisons from <strong>", selected_strata(), "</strong>"))
        } else if (length(selected_strata() > 1)) {
            HTML(paste(nrow(sub_stat()), "comparisons from <strong>", length(selected_strata()), "groups </strong>"))
        } else {
            paste("Brush to see listings")
        }
    })

    output$comparison_listing <- renderDT({
        req(sub_stat())
        sub_stat() %>% select(-.data$tooltip)
    })

    output$ae_info <- renderUI({
        if (length(selected_strata()) == 1) {
            HTML(paste(nrow(sub_aes()), "AEs from <strong>", selected_strata(), "</strong>"))
        } else if (length(selected_strata() > 1)) {
            HTML(paste(nrow(sub_aes()), "AEs from <strong>", length(selected_strata()), "groups </strong>"))
        } else {
            paste("Brush to see listings")
        }
    })

    output$ae_listing <- renderDT({
        req(sub_aes())
        sub_aes()
    })
}
