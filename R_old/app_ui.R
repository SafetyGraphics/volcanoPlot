#' The application User-Interface
#'
#' @import shiny

app_ui <- function(){
    fluidPage(
        tags$head(tags$style(HTML("hr {border-top: 1px solid #999999;}"))), # horizontal line option
        fluidRow(align="center",h1("Volcano Plot")),
        useShinyjs(),
        column(
            width=3,
            wellPanel(fileInput("file_ae_test", label = "Import dataset (.csv)", accept = ".csv")),
            conditionalPanel(
                condition = "output.fileUploaded",
                wellPanel(
                    fluidRow(
                        align="center",
                        #column(width=6,selectInput("summary_by", "Summary By", choices = c("Patients", "Events"), width="75%")),
                        selectInput("review_by", "Review By", choices = c("PT", "SOC"), width="75%"),
                        #column(width=6,selectInput("review_by", "Review By", choices = c("PT", "SOC", "Other"), width="75%"), uiOutput("review_by_please_specify_UI")),
                        #selectInput("period", "Period",choices = c("Treatment emergent", "AE during entire study", "Other"), width="75%"),	 
                        selectInput("period", "Period",choices = c("Treatment emergent", "AE during entire study"), width="75%"),
                        uiOutput("period_please_specify_UI"),
                        selectInput("ae_filter", "Adverse Event filter(s)", choices=c("Serious","Drug-related","Mild","Moderate","Severe"), selected=NULL, multiple=TRUE, width="75%"),
                        fluidRow(
                            column(width=6,uiOutput("comparison_group_UI")),
                            column(width=6,uiOutput("reference_group_UI"))
                        ),
                        actionBttn("obtain", "Obtain Statistics",color = "primary",style = "bordered",block=TRUE),
                        hr(),
                        selectInput(
                            "calculation.type", 
                            "Measure of Association", 
                            choices = c(
                                "Rate Ratio", 
                                "Rate Difference",
                                "Risk Ratio",
                                "Risk Difference", 
                                "Hazard Ratio"
                            ),
                        width="75%"),
                        numericInput("X_ref", "X-axis Reference Lines", value = 0,width="75%"),
                        selectInput("pvalue_label", "P-value Transformation",choices = c("None", "-log10"), selected="-log10",width="75%")
                    )
                )
            )
        ),
        column(
            width=8,
            plotlyOutput("volcano_plot", height = "650px"), 
            fluidRow(style = "font-size: 12px; white-space: pre",htmlOutput("footnote_UI")),
            DT::dataTableOutput("volcano_plot_drill")
        )
    )
}


