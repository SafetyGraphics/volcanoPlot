################################################################################
# ui.R
# This R Script only defines the Web layout for the Shiny App.
################################################################################

ui <- pageWithSidebar(
  headerPanel('Volcano Plot'),
  sidebarPanel(width = 3,
    useShinyjs(),
    div(uiOutput("update_UI"), style = "display: inline-block"),
    div(style = "display: inline-block; width: 30px"),
    div(uiOutput("obtain_UI"), style = "display: inline-block"),
    div(style = "display: inline-block; width: 30px"),
    div(uiOutput("reset_UI"), style = "display: inline-block"),
    br(),
    br(),
    fileInput("file_ae_test", label = h5("Import SDTM dataset"), accept = ".csv"),                           
    br(),
    fileInput("file_statistics", label = h5("Import ADaM dataset"), accept = ".csv"),       
    br(),
    uiOutput("summary_by_UI"),	
    br(),
    uiOutput("review_by_UI"),	 
    uiOutput("review_by_please_specify_UI"),	
    br(), 
    uiOutput("period_UI"),	
    uiOutput("period_please_specify_UI"),
    br(),
    uiOutput("ser_UI"), 
    uiOutput("drug_UI"), 
    br(),
    uiOutput("test_UI"),
    br(),
    uiOutput("treatment1_UI"),
    br(),
    uiOutput("treatment2_UI"),
    br(),
    uiOutput("treatment1_label_UI"),
    br(),
    uiOutput("treatment2_label_UI"),
    br(),
    uiOutput("subgroup_var_UI"),
    uiOutput("subgroup_vals_title_UI"),
    uiOutput("subgroup_vals_UI"),
    uiOutput("X_ref_UI"),
    uiOutput("Y_ref_UI"),
    uiOutput("pvalue_option_UI")
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


