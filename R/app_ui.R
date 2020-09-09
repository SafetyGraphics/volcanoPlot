#' The application User-Interface
#'
#' @import shiny

app_ui <- function(){
  fluidPage(
    tags$head(tags$style(HTML("hr {border-top: 1px solid #999999;}"))), # horizontal line option
    fluidRow(align="center",h1("Volcano Plot")),
    useShinyjs(),
    column(width=4,
           wellPanel(
             fluidRow(
               column(width=1),
               column(width=4,radioGroupButtons("data_type","Type: (Doesn't work yet)",choices=c("SDTM","ADaM"), selected="SDTM", status="primary", size="normal")),
               column(width=6,fileInput("file_ae_test", label = "Import dataset (.csv)", accept = ".csv"))
             )),
           conditionalPanel(condition = "output.fileUploaded",
                            wellPanel(
                              fluidRow(align="center",
                                       fluidRow(
                                         column(width=6,actionBttn("obtain", "Obtain Statistics",color = "primary",style = "bordered",block=TRUE)),
                                         column(width=6,actionBttn("reset", "Reset Options",color = "primary",style = "bordered",block=TRUE))
                                       ), hr(),
                                       fluidRow(
                                         #column(width=6,selectInput("summary_by", "Summary By", choices = c("Patients", "Events"), width="75%")),
                                         column(width=6,selectInput("review_by", "Review By", choices = c("PT", "SOC"), width="75%"))
                                         #column(width=6,selectInput("review_by", "Review By", choices = c("PT", "SOC", "Other"), width="75%"), uiOutput("review_by_please_specify_UI")),
                                       ),
                                       fluidRow(
                                         column(width=6,selectInput("period", "Period",choices = c("Treatment emergent", "AE during entire study", "Other"), width="75%"),	 uiOutput("period_please_specify_UI")),
                                         column(width=6,selectInput("ae_filter", "Adverse Event filter(s)", choices=c("Serious","Drug-related","Mild","Moderate","Severe"), selected=NULL, multiple=TRUE, width="75%")
                                         ),hr(),
                                         fluidRow(
                                           column(width=6,uiOutput("treatment1_UI"),  uiOutput("treatment1_label_UI")),
                                           column(width=6,uiOutput("treatment2_UI"),  uiOutput("treatment2_label_UI"))
                                         ),hr(),
                                         fluidRow(
                                             selectInput("calculation.type", "Measure of Association", choices = c("Rate Ratio", "Risk Ratio", "Hazard Ratio", "Risk Difference", "Rate Difference"), width="75%")
                                             
                                           #selectInput("subgroup_var", "Subgroup Variable", choices = c("No Subgroup Variable", "SITE", "SEX", "RACE"),selected = "No Subgroup Variable", width="75%"), uiOutput("subgroup_vals_UI")
                                         ),br(),hr(),
                                         fluidRow(
                                           column(width=6,numericInput("X_ref", "X-axis Reference Lines", value = 0,width="75%") ),
                                           column(width=6,selectInput("pvalue_label", "P-value Transformation",choices = c("None", "-log10"), selected="-log10",width="75%"))

                                         )
                                       ))
                            )
           )),
    column(width=8,
           uiOutput("volcano_plot_UI"),

           br(),
           div(htmlOutput("footnote_UI"),style = "font-size: 12px; white-space: pre"),
           htmlOutput("test"),
           br(),
           DT::dataTableOutput("volcano_plot_drill")
    )
  )
}


