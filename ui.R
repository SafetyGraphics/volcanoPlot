################################################################################
# ui.R
# This R Script only defines the Web layout for the Shiny App.
################################################################################
# test#
ui <- fluidPage(
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
                                  column(width=4,uiOutput("obtain_UI")),
                                  column(width=4,uiOutput("update_UI")),
                                  column(width=4,uiOutput("reset_UI"))
                                ), hr(),                
                                fluidRow(
                                  column(width=4,selectInput("summary_by", "Summary By", choices = c("Patients", "Events"))),
                                  column(width=4,selectInput("review_by", "Review By", choices = c("PT", "SOC", "Other")),uiOutput("review_by_please_specify_UI")),
                                  column(width=4,selectInput("period", "Period",choices = c("Treatment emergent", "AE during entire study", "Other")),	 uiOutput("period_please_specify_UI")),
                                ),
                                fluidRow(
                                  column(width=6,  checkboxInput("ser", "Serious adverse event", value = FALSE)), 
                                  column(width=6,checkboxInput("drug", "Drug-related adverse events", value = FALSE))
                                ),hr(),
                                fluidRow(
                                  column(width=6,uiOutput("treatment1_UI"),  uiOutput("treatment1_label_UI"),    ),
                                  column(width=6,uiOutput("treatment2_UI"),  uiOutput("treatment2_label_UI")   )
                                ),hr(),
                                fluidRow(
                                  selectInput("test", "Measure of Association", choices = c("Rate Ratio", "Risk Ratio", "Hazard Ratio", "Risk Difference", "Rate Difference"), width="75%"),
                                  selectInput("subgroup_var", "Subgroup Variable", choices = c("No Subgroup Variable", "SITE", "SEX", "RACE"),selected = "No Subgroup Variable", width="75%"), uiOutput("subgroup_vals_UI")
                                ),br(),hr(),
                                fluidRow(
                                  column(width=6,numericInput("X_ref", "X-axis Reference Lines", value = 0,width="75%") ),
                                  column(width=6,numericInput("Y_ref", "Y-axis Reference Line", value = 0.05,width="75%"))
                                ),
                                fluidRow(
                                  column(width=6,selectInput("pvalue_adjustment", "P-value Adjustment",choices = c("Unadjusted", "Adjusted"), selected="Unadjusted",width="75%")),
                                  column(width=6,selectInput("pvalue_label", "P-value Transformation",choices = c("None", "-log10"), selected="-log10",width="75%"))
                                ),
                                fluidRow(align="center",
                                selectInput("color_option", label="Color",choices=c("Body System or Organ Class","Severity"),selected="Body/Organ",width="50%")
                                )
                     ))
                   )
    ),
  column(width=8,
    uiOutput("volcano_plot_UI"),
    
    br(),
    div(uiOutput("footnote_UI"),style = "font-size: 12px; white-space: pre"),
    br(),
    uiOutput("volcano_plot_drill_UI")
  )
)


