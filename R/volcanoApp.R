

#' Volcano App
#'
#' @param dfAE AE Data
#' @param dfDemog demog data
#' @param settings safetyGraphics settings
#'
#' @import shiny
#'
#' @export

VolcanoApp <- function(dfAE=safetyData::adam_adae, dfDemog = safetyData::adam_adsl, settings=NULL,runNow=TRUE){
    if(is.null(settings)){
        settings<-list(
            aes=list(id_col="USUBJID", bodsys_col="AEBODSYS"),
            dm=list(id_col="USUBJID", trt_col="ARM",  "treatment_values--group1"="Placebo", "treatment_values--group2" = "Xanomeline High Dose")
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
