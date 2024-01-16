#' Volcano Plot App
#'
#' Initializes a stand-alone volcano plot shiny application.
#'
#' @param dm `data.frame` Subject-level data with one row per subject.
#' @param ae `data.frame` Event-level data with one row per adverse event.
#' @param settings `list` safetyGraphics settings
#' @param runNow `logical` run app immediately?
#'
#' @return Initializes Shiny app. No return value.
#'
#' @import shiny
#'
#' @export

volcano_app <- function(
    dm = safetyData::adam_adsl,
    ae = safetyData::adam_adae,
    settings = NULL,
    runNow = TRUE
) {
    ## create default settings when settings is not defined by default
    if (is.null(settings)) {
        settings <- list(
            dm = list(
                id_col = "USUBJID",
                treatment_col = "ARM",
                treatment_values = list(
                    group1 = "Placebo"
                )
            ),
            aes = list(
                id_col = "USUBJID",
                bodsys_col = "AEBODSYS",
                term_col = "AEDECOD"
            )
        )
    }

    ## create object containing data and setting to pass to server
    params <- reactive({
        list(
            data = list(
                dm = dm,
                aes = ae
            ),
            settings = settings
        )
    })

    ## Create app with ui and server
    app <- shinyApp(
        ui = volcano_ui("volcano_plot"),
        server = function(input, output, session) {
            callModule(
                volcano_server,
                "volcano_plot",
                params
            )
        }
    )

    if (runNow) {
        runApp(app)
    } else {
        app
    }
}
