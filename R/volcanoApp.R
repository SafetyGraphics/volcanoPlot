#' Volcano App
#'
#' Initializes stand-alone volcano plot shiny application. 
#' 
#' @param dfAE AE Data
#' @param dfDemog demog data
#' @param settings safetyGraphics settings
#' @param runNow run app immediately? 
#' 
#' @return Initializes Shiny app. No return value. 
#' 
#' @import shiny
#'
#' @export

volcanoApp <- function(dfAE=safetyData::adam_adae, dfDemog = safetyData::adam_adsl, settings=NULL,runNow=TRUE){
    
    ## create default settings when settings is not defined by default
    if(is.null(settings)){      
        settings<-list(
            aes=list(id_col="USUBJID", bodsys_col="AEBODSYS", term_col = 'AEDECOD'),
            dm=list(id_col="USUBJID", treatment_col="ARM",  "treatment_values"=list(group1="Placebo"))
        )
    }
    
    ## create object containing data and setting to pass to server
    params <- reactive({
        list(
            data=list(aes=dfAE, dm=dfDemog),
            settings=settings
        )
    })
    
    ## Create app with ui and server
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
