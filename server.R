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
    file <- input$file_ae_test
    if (is.null(file)) return()
    df <- fread(input$file_ae_test$datapath)
  })
  
  statistics_data <- reactive({
    file <- input$file_statistics
    if (is.null(file)) return()
    df <- fread(input$file_statistics$datapath)
  })
  
  observe({
    if (!is.null(ae_test_data())) {
      data$ae_test <- ae_test_data()
    } 
    # if (is.null(data$ae_test)){
    #   filename <- paste0(getwd(),"/ae-test.csv")
    #   if(file.exists(filename)){
    #     data$ae_test <- fread(filename)
    #   } else {
    #     shinyjs::alert(paste0("SDTM file does not exist:",filename))
    #   }
    # } 
  })


  ### Extract all arms ---------------------------------------------------------
  ARMCD <- reactive({
    req(data$ae_test) 
    unique(data$ae_test$ARMCD) 
  })


  ### Definitions of UI outputs in the control panel and related logics --------
  output$summary_by_UI <- renderUI({
    req(data$ae_test) 
    selectInput("summary_by", "Summary By",
                choices = c("Patients", "Events"), multiple = F)
  })

  output$review_by_UI <- renderUI({
    req(data$ae_test) 
    selectInput("review_by", "Review By",
                choices = c("PT", "SOC", "Other"), multiple = F)
  })

  output$review_by_please_specify_UI <- renderUI({
    req(data$ae_test) 
    req(input$review_by)
    if (input$review_by != "Other") return()
    textInput("review_by_please_specify", "Please specify")
  })
  
  output$period_UI <- renderUI({
    req(data$ae_test) 
    selectInput("period", "Period",
                choices = c("Treatment emergent", "AE during entire study", "Other"), multiple = F)
  })

  output$period_please_specify_UI <- renderUI({
    req(data$ae_test) 
    req(input$period)
    if (input$period != "Other") return()
    numericInput("period_please_specify",
                 HTML("Please enter residual period (in days)"),
                 value = 30, min = 0, max = 10^5)
  })
  
  output$ser_UI <- renderUI({
    req(data$ae_test) 
    checkboxInput("ser", "Serious adverse event", value = FALSE)  
  }) 

  output$drug_UI <- renderUI({
    req(data$ae_test) 
    checkboxInput("drug", "Drug-related adverse events", value = FALSE)
  }) 
   
  output$test_UI <- renderUI({
    req(data$ae_test) 
    selectInput("test", "Measure of Association", 
                choices = c("Rate Ratio",
                            "Risk Ratio",
                            "Hazard Ratio",
                            "Risk Difference",
                            "Rate Difference"), 
                multiple = F)
  })
  
  output$treatment1_UI <- renderUI({
    req(data$ae_test) 
    req(ARMCD())  
    if(input$summary_by == "Events" & input$test %in% c("Hazard Ratio",
                                                        "Rate Difference",
                                                        "Rate Ratio",
                                                        "Risk Difference",
                                                        "Risk Ratio")) return()
    div(style = 'height:100px; width:91%; overflow: scroll',
        checkboxGroupInput("treatment1", "Exposed Group", 
                           choices = ARMCD(),
                           selected = ARMCD()[1],
                           inline = F)
    )
  })
  
  output$treatment2_UI <- renderUI({
    req(data$ae_test) 
    req(ARMCD())   
    if(input$summary_by == "Events" & input$test %in% c("Hazard Ratio",
                                                        "Rate Difference",
                                                        "Rate Ratio",
                                                        "Risk Difference",
                                                        "Risk Ratio")) return()
    div(style = 'height:100px; width:91%; overflow: scroll',
       checkboxGroupInput("treatment2", "Unexposed Group", 
                          choices = setdiff(ARMCD(), input$treatment1),
                          selected = setdiff(ARMCD(), input$treatment1)[1],
                          inline = F)
    )
  })
  
  output$treatment1_label_UI <- renderUI({
    req(data$ae_test) 
    if(input$summary_by == "Events" & input$test %in% c("Hazard Ratio",
                                                        "Rate Difference",
                                                        "Rate Ratio",
                                                        "Risk Difference",
                                                        "Risk Ratio")) return()
    textInput("treatment1_label", "Label for Exposed Group")
  })

  output$treatment2_label_UI <- renderUI({
    req(data$ae_test) 
    if(input$summary_by == "Events" & input$test %in% c("Hazard Ratio",
                                                        "Rate Difference",
                                                        "Rate Ratio",
                                                        "Risk Difference",
                                                        "Risk Ratio")) return()
    textInput("treatment2_label", "Label for Unexposed Group")
  })
  
  subgroup_var_choices <- reactive({
    req(data$ae_test) 
    if(input$summary_by == "Events" & input$test %in% c("Hazard Ratio",
                                                        "Rate Difference",
                                                        "Rate Ratio",
                                                        "Risk Difference",
                                                        "Risk Ratio")) return()
    c("No Subgroup Variable", colnames(data$ae_test))
  })
  
  output$subgroup_var_UI <- renderUI({
    req(data$ae_test) 
    req(subgroup_var_choices()) 
    if(input$summary_by == "Events" & input$test %in% c("Hazard Ratio",
                                                        "Rate Difference",
                                                        "Rate Ratio",
                                                        "Risk Difference",
                                                        "Risk Ratio")) return()
    selectInput("subgroup_var", "Subgroup Variable", 
                # choices = subgroup_var_choices(),
                choices = c("No Subgroup Variable", "SITE", "SEX", "RACE"),
                selected = NULL, multiple = F)
  })

  output$subgroup_vals_title_UI <- renderUI({ 
    req(input$subgroup_var)
    if(input$subgroup_var == "No Subgroup Variable") return()
    if(input$summary_by == "Events" & input$test %in% c("Hazard Ratio",
                                                        "Rate Difference",
                                                        "Rate Ratio",
                                                        "Risk Difference",
                                                        "Risk Ratio")) return()
    h5(strong("Value for Subgroup"))
  })

  output$subgroup_vals_UI <- renderUI({
    req(input$subgroup_var)
    if(input$subgroup_var == "No Subgroup Variable") return()
    if(input$summary_by == "Events" & input$test %in% c("Hazard Ratio",
                                                        "Rate Difference",
                                                        "Rate Ratio",
                                                        "Risk Difference",
                                                        "Risk Ratio")) return()
    req(data$ae_test)
    div(style = 'height:200px; width:91%; overflow: scroll',
       checkboxGroupInput("subgroup_vals", "", 
                          choices = sort(unique(data$ae_test[[input$subgroup_var]])),
                          inline = F)
    )
  })
  
  output$X_ref_UI <- renderUI({
    req(data$ae_test) 
    req(input$test)
    if(input$summary_by == "Events" & input$test %in% c("Hazard Ratio",
                                                        "Rate Difference",
                                                        "Rate Ratio",
                                                        "Risk Difference",
                                                        "Risk Ratio")) return()
    if (input$test %in% c("Hazard Ratio", "Rate Ratio", "Risk Ratio")) {xref <- 1}
    if (input$test %in% c("Rate Difference", "Risk Difference")) {xref <- 0}
    textInput("X_ref", "X-axis Reference Line", value = xref)
  })

  output$Y_ref_UI <- renderUI({
    req(data$ae_test) 
    if(input$summary_by == "Events" & input$test %in% c("Hazard Ratio",
                                                        "Rate Difference",
                                                        "Rate Ratio",
                                                        "Risk Difference",
                                                        "Risk Ratio")) return()
    textInput("Y_ref", "Y-axis Reference Line", value = 0.05)
  })

  output$pvalue_option_UI <- renderUI({
    req(data$ae_test) 
    if(input$summary_by == "Events" & input$test %in% c("Hazard Ratio",
                                                        "Rate Difference",
                                                        "Rate Ratio",
                                                        "Risk Difference",
                                                        "Risk Ratio")) return()
    selectInput("pvalue_option", "p-Value Option",
                choices = c("Unadjusted", "Adjusted"), multiple = F)
  })


  ### An essential step for obtaining statistics right after data uploading and 
  ### before the generation of volcano plot ------------------------------------
  observeEvent(input$obtain, {
    req(input$review_by)
    req(input$summary_by)
    req(input$treatment1)
    req(input$treatment2)
    if (input$period == "Other") {req(input$period_please_specify)}
    if (input$review_by == "SOC") return()
    if (!is.null(statistics_data())) {
      data$statistics <- statistics_data()
    } 
    # if (is.null(data$statistics)){
    #   filename <- paste0(getwd(),"/statistics.csv")
    #   if(file.exists(filename)){
    #     data$statistics <- fread(filename)
    #   } else {
    #     shinyjs::alert(paste0("ADaM file does not exist:",filename))
    #   }
    # }
    if (is.null(ae_test_data())) {
      shinyjs::alert("Please import a SDTM dataset first!")
      data$statistics <- NULL
    } else {
      if (input$summary_by == "Events" & input$test == "Hazard Ratio") {
        data$statistics <- NULL
        shinyjs::alert(paste0("Error: Hazard Ratio cannot be obtained ",
                              "if data is summarized by events instead of patients!"))
      }
      if (input$summary_by == "Events" & input$test == "Rate Difference") {
        data$statistics <- NULL
        shinyjs::alert(paste0("Error: Rate Difference cannot be obtained ",
                              "if data is summarized by events instead of patients!"))
      }
      if (input$summary_by == "Events" & input$test == "Rate Ratio") {
        data$statistics <- NULL
        shinyjs::alert(paste0("Error: Rate Ratio cannot be obtained ",
                              "if data is summarized by events instead of patients!"))
      }
      if (input$summary_by == "Events" & input$test == "Risk Difference") {
        data$statistics <- NULL
        shinyjs::alert(paste0("Error: Risk Difference cannot be obtained ",
                              "if data is summarized by events instead of patients!"))
      }
      if (input$summary_by == "Events" & input$test == "Risk Ratio") {
        data$statistics <- NULL
        shinyjs::alert(paste0("Error: Risk Ratio cannot be obtained ",
                              "if data is summarized by events instead of patients!"))
      }
      if (input$summary_by == "Patients" & input$test == "Hazard Ratio") {
        withProgress(data$statistics <- GetStatistics_HazardR(ae_test_data(),
                                                              input$ser,
                                                              input$drug,
                                                              input$period,
                                                              ifelse(input$period != "Other",
                                                                     NULL, input$period_please_specify),
                                                              FALSE,
                                                              input$treatment1,
                                                              input$treatment2),
                     message = "Executing data pre-processing...",
                     detail = "This step should take a while.",
                     min = 0, max = 1, value = 1)
      }
      if (input$summary_by == "Patients" & input$test == "Rate Difference") {
        withProgress(data$statistics <- GetStatistics_RateD(ae_test_data(),
                                                            input$ser,
                                                            input$drug,
                                                            input$period,
                                                            ifelse(input$period != "Other",
                                                                   NULL, input$period_please_specify),
                                                            FALSE,
                                                            input$treatment1,
                                                            input$treatment2),
                     message = "Executing data pre-processing...",
                     detail = "This step should take a while.",
                     min = 0, max = 1, value = 1)
      }
      if (input$summary_by == "Patients" & input$test == "Rate Ratio") {
        withProgress(data$statistics <- GetStatistics_RateR(ae_test_data(),
                                                            input$ser,
                                                            input$drug,
                                                            input$period,
                                                            ifelse(input$period != "Other",
                                                                   NULL, input$period_please_specify),
                                                            FALSE,
                                                            input$treatment1,
                                                            input$treatment2),
                     message = "Executing data pre-processing...",
                     detail = "This step should take a while.",
                     min = 0, max = 1, value = 1)
      }
      if (input$summary_by == "Patients" & input$test == "Risk Difference") {
        withProgress(data$statistics <- GetStatistics_RiskD(ae_test_data(),
                                                            input$ser,
                                                            input$drug,
                                                            input$period,
                                                            ifelse(input$period != "Other",
                                                                   NULL, input$period_please_specify),
                                                            FALSE,
                                                            input$treatment1,
                                                            input$treatment2),
                     message = "Executing data pre-processing...",
                     detail = "This step should take a while.",
                     min = 0, max = 1, value = 1)
      }
      if (input$summary_by == "Patients" & input$test == "Risk Ratio") {
        withProgress(data$statistics <- GetStatistics_RiskR(ae_test_data(),
                                                            input$ser,
                                                            input$drug,
                                                            input$period,
                                                            ifelse(input$period != "Other",
                                                                   NULL, input$period_please_specify),
                                                            FALSE,
                                                            input$treatment1,
                                                            input$treatment2),
                     message = "Executing data pre-processing...",
                     detail = "This step should take a while.",
                     min = 0, max = 1, value = 1)
      }
    }
  })
  
  observeEvent(input$obtain, {
    req(input$review_by)
    req(input$summary_by)
    if (input$period == "Other") {req(input$period_please_specify)}
    if (input$review_by!="SOC") return()
    if (!is.null(statistics_data())) {
      data$statistics_soc <- statistics_data()
    }
    # if (is.null(data$statistics_soc)){
    #   filename <- paste0(getwd(),"/statistics-soc.csv")
    #   if(file.exists(filename)){
    #     data$statistics_soc <- fread(filename)
    #   } else {
    #     shinyjs::alert(paste0("ADaM file does not exist:",filename))
    #   }
    # }
    if (is.null(ae_test_data())) {
      shinyjs::alert("Please import a SDTM dataset first!")
      data$statistics_soc <- NULL
    } else {
      if (input$summary_by == "Events" & input$test == "Hazard Ratio") {
        data$statistics_soc <- NULL
        shinyjs::alert(paste0("Error: Hazard Ratio cannot be obtained ",
                              "if data is summarized by events instead of patients!"))
      }
      if (input$summary_by == "Events" & input$test == "Rate Difference") {
        data$statistics_soc <- NULL
        shinyjs::alert(paste0("Error: Rate Difference cannot be obtained ",
                              "if data is summarized by events instead of patients!"))
      }
      if (input$summary_by == "Events" & input$test == "Rate Ratio") {
        data$statistics_soc <- NULL
        shinyjs::alert(paste0("Error: Rate Ratio cannot be obtained ",
                              "if data is summarized by events instead of patients!"))
      }
      if (input$summary_by == "Events" & input$test == "Risk Difference") {
        data$statistics_soc <- NULL
        shinyjs::alert(paste0("Error: Risk Difference cannot be obtained ",
                              "if data is summarized by events instead of patients!"))
      }
      if (input$summary_by == "Events" & input$test == "Risk Ratio") {
        data$statistics_soc <- NULL
        shinyjs::alert(paste0("Error: Risk Ratio cannot be obtained ",
                              "if data is summarized by events instead of patients!"))
      }
      if (input$summary_by == "Patients" & input$test == "Hazard Ratio") {
        withProgress(data$statistics_soc <- GetStatistics_HazardR(ae_test_data(),
                                                                  input$ser,
                                                                  input$drug,
                                                                  input$period,
                                                                  ifelse(input$period != "Other",
                                                                         NULL, input$period_please_specify),
                                                                  TRUE,
                                                                  input$treatment1,
                                                                  input$treatment2),
                     message = "Executing data pre-processing...",
                     detail = "This step should take a while.",
                     min = 0, max = 1, value = 1)
      }
      if (input$summary_by == "Patients" & input$test == "Rate Difference") {
        withProgress(data$statistics_soc <- GetStatistics_RateD(ae_test_data(),
                                                                input$ser,
                                                                input$drug,
                                                                input$period,
                                                                ifelse(input$period != "Other",
                                                                       NULL, input$period_please_specify),
                                                                TRUE,
                                                                input$treatment1,
                                                                input$treatment2),
                     message = "Executing data pre-processing...",
                     detail = "This step should take a while.",
                     min = 0, max = 1, value = 1)
      }
      if (input$summary_by == "Patients" & input$test == "Rate Ratio") {
        withProgress(data$statistics_soc <- GetStatistics_RateR(ae_test_data(),
                                                                input$ser,
                                                                input$drug,
                                                                input$period,
                                                                ifelse(input$period != "Other",
                                                                       NULL, input$period_please_specify),
                                                                TRUE,
                                                                input$treatment1,
                                                                input$treatment2),
                     message = "Executing data pre-processing...",
                     detail = "This step should take a while.",
                     min = 0, max = 1, value = 1)
      }
      if (input$summary_by == "Patients" & input$test == "Risk Difference") {
        withProgress(data$statistics_soc <- GetStatistics_RiskD(ae_test_data(),
                                                                input$ser,
                                                                input$drug,
                                                                input$period,
                                                                ifelse(input$period != "Other",
                                                                       NULL, input$period_please_specify),
                                                                TRUE,
                                                                input$treatment1,
                                                                input$treatment2),
                     message = "Executing data pre-processing...",
                     detail = "This step should take a while.",
                     min = 0, max = 1, value = 1)
      }
      if (input$summary_by == "Patients" & input$test == "Risk Ratio") {
        withProgress(data$statistics_soc <- GetStatistics_RiskR(ae_test_data(),
                                                                input$ser,
                                                                input$drug,
                                                                input$period,
                                                                ifelse(input$period != "Other",
                                                                       NULL, input$period_please_specify),
                                                                TRUE,
                                                                input$treatment1,
                                                                input$treatment2),
                     message = "Executing data pre-processing...",
                     detail = "This step should take a while.",
                     min = 0, max = 1, value = 1)
      }
    }
  }) 
  
  
  ### Definition of 'Reset' button UI output and its consequent triggerings ----
  output$reset_UI <- renderUI({
    req(data$ae_test)
    actionButton("reset", "Reset")
  })
  
  observeEvent(input$reset, {
    updateSelectInput(session, "summary_by", "Summary By",
                      choices = c("Patients", "Events"))
    updateSelectInput(session, "review_by", "Review By",
                      choices = c("PT", "SOC", "Other"))
    updateSelectInput(session, "period", "Period",
                      choices = c("Treatment emergent",
                                  "AE during entire study",
                                  "Other"))
    updateCheckboxInput(session, "ser", "Serious adverse event", value = FALSE)
    updateCheckboxInput(session, "drug", "Drug-related adverse events", value = FALSE)
    updateSelectInput(session, "test", "Measure of Association",
                      choices = c("Rate Ratio",
                                  "Risk Ratio",
                                  "Hazard Ratio",
                                  "Risk Difference",
                                  "Rate Difference"))
    updateCheckboxGroupInput(session, "treatment1", "Exposed Group",
                             choices = ARMCD(), selected = ARMCD()[1], inline = F)
    updateCheckboxGroupInput(session, "treatment2", "Unexposed Group",
                             choices = setdiff(ARMCD(), input$treatment1),
                             selected = setdiff(ARMCD(), input$treatment1)[1],
                             inline = F)
    updateTextInput(session, "treatment1_label", "Label for Exposed Group")
    updateTextInput(session, "treatment2_label", "Label for Unexposed Group")
    updateSelectInput(session, "subgroup_var", "Subgroup Variable",
                      choices = c("No Subgroup Variable", "SITE", "AGE", "SEX", "RACE"),
                      selected = NULL)
    updateTextInput(session, "X_ref", "X-axis Reference Line", value = 1)
    updateTextInput(session, "Y_ref", "Y-axis Reference Line", value = 0.05)
    updateSelectInput(session, "pvalue_option", "p-Value Option",
                      choices = c("Unadjusted", "Adjusted"))
  })
  
  
  ### Definition of 'Obtain Statistics' button UI output -----------------------
  output$obtain_UI <- renderUI({
    req(data$ae_test)
    actionButton("obtain", "Obtain Statistics")
  })
  
  
  ### Definition of 'Update Plot' button UI output -----------------------------
  output$update_UI <- renderUI({
    req(data$ae_test)
    if (input$review_by == "PT") {req(data$statistics)}
    if (input$review_by == "SOC") {req(data$statistics_soc)}
    actionButton("update", "Update Plot")
  })

  
  ### Check the prerequisite of customized labels for exposed/unexposed group --
  observeEvent(input$update, {
    if(nchar(input$treatment1_label) == 0) {
      shinyjs::alert("Please enter label for the exposed group.")
      return()
    }

    if(nchar(input$treatment2_label) == 0) {
      shinyjs::alert("Please enter label for the unexposed group.")
      return()
    }
  })
  
  
  ### Generation of volcano plot -----------------------------------------------
  observeEvent(c(input$update
         # input$review_by,
         # input$review_by_please_specify,
         # input$period,
         # input$period_please_specify,
         # input$ser,
         # input$drug,
         # input$test,
         # input$treatment1,
         # input$treatment2,
         # input$treatment1_label,
         # input$treatment2_label,
         # input$subgroup_var,
         # input$subgroup_vals,
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
      shinyjs::alert("Please select an exposed group.")
      return()
    }
    
    if(length(input$treatment2) == 0) {
      shinyjs::alert("Please select an unexposed group.")
      return()
    }

    # if(length(input$period_please_specify)>0) {
    #   if(!numbers_only(input$period_please_specify)) {
    #     shinyjs::alert("Please enter only an integer for risk period.")
    #     return()
    #   } 
    # }
    
    if(length(input$X_ref) > 0) {
      X_ref <- as.numeric(input$X_ref)
      if(is.na(X_ref)) {
        shinyjs::alert("Please enter a numeric value for X-axis reference line.")
        return()
      }   
    }
	
    if(length(input$Y_ref) > 0) {
      Y_ref <- as.numeric(input$Y_ref)
      if(is.na(Y_ref)) {
        shinyjs::alert("Please enter a numeric value for Y-axis reference line.")
        return()
      } else {
        if (!(as.numeric(input$Y_ref) >= 0 & as.numeric(input$Y_ref) <= 1)) {
          shinyjs::alert("Please enter a valid value between 0 and 1 for Y-axis reference line.")
          return()
        }  
      }
    }

    plots$volcano_plot <- NULL
    plots$volcano_plot_data <- NULL
    plots$volcano_plot_click <- NULL
    withProgress(message = 'Plotting data', value = 0, {
      out <- try(
        volcano.plot(
        data = isolate(data$ae_test),
        statistics_data = isolate(data$statistics),
        statistics_soc_data = isolate(data$statistics_soc),
        ser = input$ser,
        drug = input$drug,
        period = input$period,
        residual = input$period_please_specify,
        test = input$test,
        Treatment1 = input$treatment1,
        Treatment2 = input$treatment2,
        treatment1_label = input$treatment1_label,
        treatment2_label = input$treatment2_label,
        subgroup_var = input$subgroup_var,
        subgroup_vals = input$subgroup_vals,
        X_ref = as.numeric(input$X_ref),
        Y_ref = as.numeric(input$Y_ref),
        X_label = paste(input$test, input$treatment1_label, "vs", input$treatment2_label),
        review_by = input$review_by,
        summary_by = input$summary_by,
        pvalue_option = input$pvalue_option)
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
          ggplotly(plots$volcano_plot,tooltip = c("label")) %>%
            plotly::layout(annotations = 
              list(x = 0, y = 0.02, text = text1, 
                   showarrow = F, xref = 'paper', yref = 'paper', 
                   xanchor = 'left', yanchor = 'bottom', xshift = 0, yshift = 0,
                   font = list(size = 15, color = "blue"))) %>% 
            plotly::layout(annotations = 
              list(x = .95, y = 0.02, text = text2, 
                   showarrow = F, xref = 'paper', yref = 'paper', 
                   xanchor = 'right', yanchor = 'bottom', xshift = 0, yshift = 0,
                   font = list(size = 15, color = "blue")))
          
        })
        output$volcano_plot_UI <- renderUI({
          req(plots$volcano_plot) 
          # req(input$treatment1_label)
          # req(input$treatment2_label)
          plotlyOutput("volcano_plot", height = "625px") %>% withSpinner(type = 5)
        })      
      } else {
        #Catch Error 
        plots$volcano_plot <- NULL
        plots$volcano_plot_data <- NULL
        shinyjs::alert("Error occurred processing the selected data.")
      } 
    })
  })

  
  ### Definitions and logics for the drilldown table from click triggering -----
  observeEvent(event_data("plotly_click"), {
    plots$volcano_plot_click <- event_data("plotly_click")
  })
  
  volcano_plot_table <- reactive({  
    req(plots$volcano_plot_click)
    plots$volcano_plot$data[as.numeric(plots$volcano_plot_click$key),] %>% 
      inner_join(plots$volcano_plot_data) %>%
      select(USUBJID, 
             SITE, 
             AGE, 
             SEX, 
             RACE, 
             ARM, 
             SBJTSTAT, 
             AEBODSYS, 
             AEDECOD, 
             AESTDT,
             AESTDY,
             AEENDT,
             AESER, 
             AEONGO, 
             AESEV, 
             AEREL, 
             AEOUT,
             TRTEMFL,
             STUDYFL)
  })  	
    	
  #List the details of filtered data that matches the selected SOC/PT
  output$volcano_plot_drill <- DT::renderDataTable({
    datatable(volcano_plot_table(), extensions = 'Buttons',
      colnames = c('Subject ID', 
                   'Site', 
                   'Age', 
                   'Sex',
                   'Race',
                   'Treatment',
                   'Subject Status',
                   'SOC',
                   'PT',
                   'Onset Date',
                   'Time to Onset', 
                   'End Date',
                   'Serious AE?',
                   'Still Ongoing?',
                   'Severity',
                   'Causality',
                   'Outcome',
                   'Treatment Emergent?',
                   'During Entire Study?'),
      options = list(dom = 'Bfrtip',
                     buttons = I('colvis'),
                     #buttons = c(I('colvis'),
                     #              'copy', 
                     #              'csv', 
                     #              'excel', 
                     #              'print', 
                     #              'pdf'),
                     pageLength = 10)
                     
                     
                     
                     )
  }, server = FALSE)
  
  output$volcano_plot_drill_UI <- renderUI({
    req(plots$volcano_plot_click)
    DT::dataTableOutput("volcano_plot_drill")
  })
  
  
  ### Specifications on the footnote of volcano plot ---------------------------
  output$footnote_UI <- renderUI({
    req(plots$volcano_plot)
    
    # if (input$summary_by == "Events") {
    #   ft <- paste0("<b> Note: </b> <br/>",
    #                "* N is the total number of events.")
    # }
    
    if (input$summary_by == "Patients") {
      
      if (input$ser == FALSE & input$drug == FALSE) {
        ftn <- "* n is the number of patients with adverse events."
      }
      if (input$ser == TRUE & input$drug == FALSE) {
        ftn <- "* n is the number of patients with serious adverse events."
      }
      if (input$ser == FALSE & input$drug == TRUE) {
        ftn <- "* n is the number of patients with drug-related adverse events."
      }
      if (input$ser == TRUE & input$drug == TRUE) {
        ftn <- "* n is the number of patients with serious adverse events and drug-related adverse events."
      }
      
      ft <- paste0("<b> Note: </b> <br/>",
                   "* N is the total number of treated patients. <br/>",
                   ftn)
    }
    
    return(HTML(ft))
  })
}


