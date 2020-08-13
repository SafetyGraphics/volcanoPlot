################################################################################
# server.R
# This R Script defines the server logics for the Shiny App.
################################################################################

server <- function(input, output, session) {
  
  
  data <- reactiveValues(ae_test = NULL,
                         statistics = NULL,
                         statistics_soc = NULL)
  
  plots <- reactiveValues(volcano_plot_data = NULL,
                          volcano_plot = NULL,
                          volcano_plot_click = NULL)
  
  ### Data uploading -----------------------------------------------------------
  ae_test_data <- reactive({
    # SDTM
    file <- input$file_ae_test
    if (is.null(file)) return()
    df <- fread(input$file_ae_test$datapath)%>% as.data.frame() %>%
      mutate(AESTDT = as.Date(AESTDT), AEENDT = as.Date(AEENDT), RFSTDTC = as.Date(RFSTDTC), RFENDTC = as.Date(RFENDTC))
  })
  
  # what is statistics_data used for?
  statistics_data <- reactive({
    file <- input$file_statistics
    if (is.null(file)) return()
    df <- fread(input$file_statistics$datapath)
  })
  
  # once data is loaded, all other inputs are loaded
  output$fileUploaded <- reactive({ return(!is.null(ae_test_data())) })
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  
  
  observe({
    #only runs for SDTM
    if (!is.null(ae_test_data())) {
      data$ae_test <- ae_test_data()
    }
  })
  
  
  ### Extract all arms ---------------------------------------------------------
  ARMCD <- reactive({
    req(data$ae_test) 
    unique(data$ae_test$ARMCD) 
  })
  
  
  ### Definitions of UI outputs in the control panel and related logics --------
  output$review_by_please_specify_UI <- renderUI({
    req(data$ae_test) 
    req(input$review_by)
    if (input$review_by != "Other") return()
    textInput("review_by_please_specify", "Please specify")
  })
  
  
  output$period_please_specify_UI <- renderUI({
    req(data$ae_test) 
    req(input$period)
    if (input$period != "Other") return()
    numericInput("period_please_specify",
                 HTML("Please enter residual period (in days)"),
                 value = 30, min = 0, max = 10^5)
  })
  
  output$treatment1_UI <- renderUI({
    req(data$ae_test) 
    req(input$summary_by != "Events")
    div(style = 'height:100px; width:91%; overflow: scroll',
        checkboxGroupInput("treatment1", "Group A", 
                           choices = ARMCD(),
                           selected = ARMCD()[1],
                           inline = F)
    )
  })
  
  output$treatment2_UI <- renderUI({
    req(data$ae_test) 
    req(input$summary_by != "Events")
    div(style = 'height:100px; width:91%; overflow: scroll',
        checkboxGroupInput("treatment2", "Group B", 
                           choices = setdiff(ARMCD(), input$treatment1),
                           selected = setdiff(ARMCD(), input$treatment1)[1],
                           inline = F)
    )
  })
  
  output$treatment1_label_UI <- renderUI({
    req(data$ae_test) 
    req(input$summary_by != "Events")
    textInput("treatment1_label", "Label for Group A", value="Exposed",width="75%")
  })
  
  output$treatment2_label_UI <- renderUI({
    req(data$ae_test) 
    req(input$summary_by != "Events")
    textInput("treatment2_label", "Label for Group B",value="Unexposed",width="75%")
  })
  
  
  
  output$subgroup_vals_UI <- renderUI({
    if(input$subgroup_var == "No Subgroup Variable") return()
    req(input$summary_by != "Events")
    div(style = 'height:100px; width:91%; overflow: scroll',
        checkboxGroupInput("subgroup_vals", "Value for Subgroup", 
                           choices = sort(unique(data$ae_test[[input$subgroup_var]])),
                           inline = F)
    )
  })
  
  
  ### An essential step for obtaining statistics right after data uploading and before the generation of volcano plot ------------------------------------
  observeEvent(input$obtain, {
    req(input$treatment1)
    req(input$treatment2)
    if (input$period == "Other") {req(input$period_please_specify)}
    if (input$review_by != "SOC"){
      if (!is.null(statistics_data())) { data$statistics <- statistics_data()  } 
      else if(!is.null(statistics_data())) {data$statistics_soc <- statistics_data() }
    }
    
    if (input$summary_by == "Events") {
      data$statistics <- NULL
      shinyjs::alert(paste0("Error: ",input$test, "cannot be obtained if data is summarized by events instead of patients!"))
    }
    
    if (input$summary_by == "Patients"){
      if (input$review_by != "SOC"){
        
        withProgress(data$statistics <- GetStatistics_all(data = ae_test_data(),
                                                          ae_filter = input$ae_filter,
                                                          period = input$period,
                                                          residual =  ifelse(input$period != "Other",NULL, input$period_please_specify),
                                                          soc =   FALSE,
                                                          treatment1=   input$treatment1,
                                                          treatment2 =    input$treatment2,
                                                          test = input$test
        ),
        message = "Executing data pre-processing...",
        detail = "This step should take a while.",
        min = 0, max = 1, value = 1) 
      } else if (input$review_by == "SOC"){
        
        withProgress(data$statistics_soc <- GetStatistics_all(data = ae_test_data(),
                                                              ae_filter = input$ae_filter,
                                                              period = input$period,
                                                              residual =  ifelse(input$period != "Other",NULL, input$period_please_specify),
                                                              soc =   FALSE,
                                                              treatment1=   input$treatment1,
                                                              treatment2 =    input$treatment2,
                                                              test = input$test
        ),
        message = "Executing data pre-processing...",
        detail = "This step should take a while.",
        min = 0, max = 1, value = 1) 
        
        
        
      }
    }
    
    
    
  })
  
  
  ### Definition of 'Reset' button UI output and its consequent triggerings ----
  output$reset_UI <- renderUI({
    req(data$ae_test)
    actionBttn("reset", "Reset Options",color = "primary",style = "bordered",block=TRUE)
  })
  
  
  observeEvent(input$reset, {
    updateSelectInput(session, "summary_by", "Summary By",     choices = c("Patients", "Events"))
    updateSelectInput(session, "review_by", "Review By",    choices = c("PT", "SOC", "Other"))
    updateSelectInput(session, "period", "Period", choices = c("Treatment emergent", "AE during entire study",     "Other"))
    updateCheckboxInput(session, "ser", "Serious adverse event", value = FALSE)
    updateCheckboxInput(session, "drug", "Drug-related adverse events", value = FALSE)
    updateSelectInput(session, "test", "Measure of Association", choices = c("Rate Ratio", "Risk Ratio",  "Hazard Ratio", "Risk Difference",   "Rate Difference"))
    updateCheckboxGroupInput(session, "treatment1", "Group A", choices = ARMCD(), selected = ARMCD()[1], inline = F)
    updateCheckboxGroupInput(session, "treatment2", "Group B", choices = setdiff(ARMCD(), input$treatment1),  selected = setdiff(ARMCD(), input$treatment1)[1],                       inline = F)
    updateTextInput(session, "treatment1_label", "Label for Group A")
    updateTextInput(session, "treatment2_label", "Label for Group B")
    updateSelectInput(session, "subgroup_var", "Subgroup Variable",   choices = c("No Subgroup Variable", "SITE", "AGE", "SEX", "RACE"), selected = "No Subgroup Variable")
    updatenumericInput(session, "X_ref", "X-axis Reference Line", value = 1)
    updatenumericInput(session, "Y_ref", "Y-axis Reference Line", value = 0.05)
    updateSelectInput(session, "pvalue_option", "p-Value Option", choices = c("Unadjusted", "Adjusted"))
  })
  
  
  ### Definition of 'Obtain Statistics' button UI output -----------------------
  output$obtain_UI <- renderUI({
    req(data$ae_test)
    actionBttn("obtain", "Obtain Statistics",color = "primary",style = "bordered",block=TRUE)
  })
  
  
  ### Definition of 'Update Plot' button UI output -----------------------------
  output$update_UI <- renderUI({
    req(data$ae_test)
    if (input$review_by == "PT") {req(data$statistics)}
    if (input$review_by == "SOC") {req(data$statistics_soc)}
    actionBttn("update", "Update Plot",color = "primary",style = "bordered",block=TRUE)
  })
  
  
  
  ### Generation of volcano plot -----------------------------------------------
  observeEvent(c(input$update
                 # input$X_ref,
                 # input$Y_ref,
                 # input$pvalue_option
  ), {
    req(data$ae_test)
    if (input$review_by == "PT") {req(data$statistics)}
    if (input$review_by == "SOC") {req(data$statistics_soc)}
    req(input$treatment1_label)
    req(input$treatment2_label)
    
    if(length(input$treatment1) == 0) {
      shinyjs::alert("Please select Group A.")
      return()
    }
    
    if(length(input$treatment2) == 0) {
      shinyjs::alert("Please select Group B.")
      return()
    }
    
    
    plots$volcano_plot <- NULL
    plots$volcano_plot_data <- NULL
    withProgress(message = 'Plotting data', value = 0, {
      out <- try(
        volcano.plot(
          data = isolate(data$ae_test),
          statistics_data = isolate(data$statistics),
          statistics_soc_data = isolate(data$statistics_soc),
          ae_filter = input$ae_filter,
          period = input$period,
          residual = input$period_please_specify,
          test = input$test,
          treatment1 = input$treatment1,
          treatment2 = input$treatment2,
          subgroup_var = input$subgroup_var,
          subgroup_vals = input$subgroup_vals,
          X_ref = as.numeric(input$X_ref),
          X_label = paste(input$test, input$treatment1_label, "vs.", input$treatment2_label),
          review_by = input$review_by,
          summary_by = input$summary_by,
          pvalue_label = input$pvalue_label
        )
      )
      if (!"try-error" %in% class(p)) {
        
        tempdata <- data$ae_test %>%
          mutate_at(names(data$ae_test)[!(names(data$ae_test) %in% c("AESEQ", "AESTDY", "AEENDY",
                                                                     "AGE", "RFENDY", "SAFFN"))],
                    ~as.character(.)) %>%
          mutate_at(names(data$ae_test), ~na_if(., ""))
        
        if (input$summary_by == "Patients") {
          N1.total <- tempdata %>%
            drop_na(RFSTDTC) %>%
            filter(ARMCD %in% input$treatment1) %>%
            distinct(USUBJID) %>% tally() %>% as.numeric()
          
          N2.total <- tempdata %>%
            drop_na(RFSTDTC) %>%
            filter(ARMCD %in% input$treatment2) %>%
            distinct(USUBJID) %>% tally() %>% as.numeric()
        }
        
        # if (input$summary_by == "Events") {
        #   N1.total <- tempdata %>%
        #     filter(!is.na(AESTDT) | !is.na(AEDECOD)) %>%
        #     filter(ARMCD %in% input$treatment1) %>%
        #     tally() %>% as.numeric()
        #   N2.total <- tempdata %>%
        #     filter(!is.na(AESTDT) | !is.na(AEDECOD)) %>%
        #     filter(ARMCD %in% input$treatment2) %>%
        #     tally() %>% as.numeric()
        # }
        
        if (input$summary_by == "Patients") {
          text1 <- paste0("<- Favors ", input$treatment1_label,
                          " n=", out$N1, " (N=", N1.total, ")")
          text2 <- paste0("Favors ", input$treatment2_label,
                          " n=", out$N2, " (N=", N2.total, ") ->")
        } else {
          text1 <- paste0("<- Favors ", input$treatment1_label,
                          " (N=", N1.total, ")")
          text2 <- paste0("Favors ", input$treatment2_label,
                          " (N=", N2.total, ") ->")
        }
        
        plots$volcano_plot <- out$plot  
        plots$volcano_plot_data <- out$data
        output$volcano_plot <- renderPlotly({
          output = ggplotly(plots$volcano_plot,tooltip = c("label"), source="volcano_plot") %>%
            plotly::layout(annotations = 
                             list(x = 0, y = 0.02, text = text1, 
                                  showarrow = F, xref = 'paper', yref = 'paper', 
                                  xanchor = 'left', yanchor = 'bottom', xshift = 0, yshift = 0,
                                  font = list(size = 12, color = "blue"))) %>% 
            plotly::layout(annotations = 
                             list(x = .95, y = 0.02, text = text2, 
                                  showarrow = F, xref = 'paper', yref = 'paper', 
                                  xanchor = 'right', yanchor = 'bottom', xshift = 0, yshift = 0,
                                  font = list(size = 12, color = "blue")))
          
          output$x$layout$legend$font$size=8.5 # change legend text size
          output
          
        })
        output$volcano_plot_UI <- renderUI({
          req(plots$volcano_plot)
          plotlyOutput("volcano_plot", height = "650px") %>% withSpinner(type = 5)
        })
      } else {
        #Catch Error 
        plots$volcano_plot <- NULL
        plots$volcano_plot_data <- NULL
        shinyjs::alert("Error occurred processing the selected data.")
      } 
    })
  })
  
  
  #List the details of filtered data that matches the selected SOC/PT
  output$volcano_plot_drill <- DT::renderDataTable({
    s=event_data("plotly_click",source="volcano_plot")
    req(length(s)>0)
    
    volcano_plot_table = plots$volcano_plot$data[as.numeric(s$key),] %>% 
      inner_join(plots$volcano_plot_data) %>%
      select(USUBJID,  SITE,   AGE, SEX,   RACE,    ARM, SBJTSTAT,   AEBODSYS, AEDECOD,  AESTDT, AESTDY,  AEENDT,  AESER,  AEONGO, 
             AESEV, AEREL,     AEOUT,TRTEMFL,  STUDYFL)
    
    datatable(volcano_plot_table, extensions = 'Buttons',
              colnames = c('Subject ID',  'Site',  'Age',   'Sex',   'Race', 'Treatment',  'Subject Status',  'SOC',
                           'PT', 'Onset Date', 'Time to Onset',  'End Date','Serious AE?','Still Ongoing?','Severity',
                           'Causality','Outcome', 'Treatment Emergent?','During Entire Study?'),
              options = list(dom = 'Bfrtip', buttons = I('colvis'), pageLength = 10)
    )
  }, server = FALSE)
  
  
  
  ### Specifications on the footnote of volcano plot ---------------------------
  output$footnote_UI <- renderText({
    req(plots$volcano_plot)
    
    if (input$summary_by == "Events") {
      ft <- paste0("<b> Note: </b> <br/>",
                   "* N is the total number of events. <br/>",
                   "Dashed horizontal line represents p-value of 0.05 <br/>",
                   "Dotted horizontal line represents FDR adjsuted p-value of approximately 0.05")
    }
    
    if (input$summary_by == "Patients") {
      if (is.null(input$ae_filter)){
        ftn = "* n is the number of patients with adverse events."
      } else{
        
        ae_filter_name = tolower(input$ae_filter)
        if (length(ae_filter_name)<=1){
          ftn <- paste("* n is the number of patients with ",ae_filter_name,"adverse events.")  
        }
        else if (length(ae_filter_name)>1){
          ftn <- paste0("* n is the number of patients with ",paste0(ae_filter_name[-length(ae_filter_name)],collapse=", "), " and ", ae_filter_name[length(ae_filter_name)], " adverse events.")
        }
      }
      ft <- paste0("<b> Note: </b> <br/>",
                   "* N is the total number of treated patients. <br/>",
                   ftn, " <br/>",
                   "Dashed horizontal line represents p-value of 0.05 <br/>",
                   "Dotted horizontal line represents FDR adjsuted p-value of approximately 0.05"
      )
      
    }
    
    return(HTML(ft))
  })
  
  
  
  
  
}


