#' Volcano App
#'
#' @param dfAE AE Data
#' @param dfDemog demog data
#' @param settings safetyGraphics settings
#'
#' @import shiny
#'
#' @export

volcanoApp <- function(dfAE=safetyData::adam_adae, dfDemog = safetyData::adam_adsl, settings=NULL,runNow=TRUE){
    if(is.null(settings)){
        settings<-list(
            aes=list(id_col="USUBJID", bodsys_col="AEBODSYS", term_col = 'AEDECOD'),
            dm=list(id_col="USUBJID", treatment_col="ARM",  "treatment_values"=list(group1="Placebo", "group2" = "Xanomeline High Dose"))
        )
    }

    params <- reactive({
        list(
            data=list(aes=dfAE, dm=dfDemog),
            settings=settings
        )
    })

    app <- shinyApp(
        ui =  volcano_ui("vp"),
        server = function(input,output,session){
            callModule(volcano_server, "vp", params)
        }
    )

    if(runNow)
        runApp(app)
    else
    app
}
