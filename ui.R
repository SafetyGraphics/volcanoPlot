################################################################################
# ui.R
# This R Script only defines the Web layout for the Shiny App.
################################################################################
# test
ui <- fluidPage(
  tags$head(tags$style(HTML("hr {border-top: 1px solid #999999;}"))), # horizontal line option
  headerPanel('Volcano Plot'),
  useShinyjs(),
  sidebarPanel(width=4,align="center",
               column(width=6,uiOutput("update_UI"),uiOutput("obtain_UI")),
               column(width=6,uiOutput("reset_UI")),hr(),
               fluidRow(
                 column(width=6,radioGroupButtons("data_type","Type:",choices=c("SDTM","ADaM"), selected="SDTM", status="primary", size="normal")),
                 column(width=6,fileInput("file_ae_test", label = "Import dataset (.csv)", accept = ".csv"))
               ),  hr(),                
               fluidRow(
                 column(width=6,  uiOutput("summary_by_UI")	   ),
                 column(width=6,  uiOutput("review_by_UI"),uiOutput("review_by_please_specify_UI")	   )
               ),      hr(),   
               fluidRow(
                 column(width=6,uiOutput("period_UI"),	 uiOutput("period_please_specify_UI")),
                 column(width=6,  uiOutput("ser_UI"), uiOutput("drug_UI")    )
               ),hr(),
               fluidRow(
                 column(width=6,uiOutput("treatment1_UI"),  uiOutput("treatment1_label_UI"),    ),
                 column(width=6,   uiOutput("treatment2_UI"),  uiOutput("treatment2_label_UI")   )
               ),hr(),
               uiOutput("test_UI"),
               uiOutput("subgroup_var_UI"),
               uiOutput("subgroup_vals_title_UI"),
               uiOutput("subgroup_vals_UI"),
               wellPanel(align="center",dropdown(label="Plot Options",icon = icon("chart-bar"), width = "600px",size="lg",style="stretch",status="primary",
                                                 wellPanel(fluidRow(
                                                   column(width=4,uiOutput("X_ref_UI")),
                                                   column(width=4,uiOutput("Y_ref_UI")),
                                                   column(width=4,uiOutput("pvalue_option_UI"))
                                                 ))))
               
  ),
  mainPanel(
    uiOutput("volcano_plot_UI"),
    br(),
    div(uiOutput("footnote_UI"),
        style = "font-size: 12px; white-space: pre"),
    br(),
    uiOutput("volcano_plot_drill_UI")
  )
)


