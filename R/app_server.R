#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'
#' @import shiny
#' @noRd

app_server <- function(input, output, session) {
    print("serving")
    
    
    data <- reactiveValues(
        ae_test = NULL,
        statistics = NULL
        #statistics_soc = NULL
    )
    
    plots <- reactiveValues(
        volcano_plot_data = NULL,
        volcano_plot = NULL,
        volcano_plot_click = NULL
    )
    
    ### Data uploading -----------------------------------------------------------
    ae_test_data <- reactive({
        # SDTM
        file <- input$file_ae_test
        if (is.null(file)) return()
        df <- fread(input$file_ae_test$datapath)%>% as.data.frame() %>%
            mutate(AESTDT = as.Date(AESTDT), AEENDT = as.Date(AEENDT), RFSTDTC = as.Date(RFSTDTC), RFENDTC = as.Date(RFENDTC))
    })
    
    
    # what is statistics_data used for?
    # statistics_data <- reactive({
    #     file <- input$file_statistics
    #     if (is.null(file)) return()
    #     df <- fread(input$file_statistics$datapath)
    # })
    
    
    # once data is loaded, all other inputs are loaded
    output$fileUploaded <- reactive({ return(!is.null(ae_test_data())) })
    outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
    
    
    observe({
        #only runs for SDTM
        if (!is.null(ae_test_data())) {
            if (is.null(input$treatment2) | is.null(input$treatment1)) {return(NULL)}
            
            period=input$period; residual=input$residual; review_by=input$review_by; ae_filter=input$ae_filter; comparison_group=input$treatment2; reference_group=input$treatment1
            
            data <- ae_test_data() %>% as.data.frame() %>% #select(-c(AESTDY, AEENDY, RFENDY, AEONGO, AETERM)) %>%
            filter(ARMCD %in% c(reference_group, comparison_group)) %>%
                mutate_at(names(data)[!(names(data) %in% c("AESEQ", "AESTDY", "AEENDY","AGE", "RFENDY", "SAFFN"))], ~as.character(.)) %>%
                mutate_at(names(data), ~na_if(., "")) %>%
                mutate_at(c("AESTDT", "AEENDT", "RFSTDTC", "RFENDTC"), ~as.Date(.)) %>%
                drop_na(RFSTDTC) %>%
                mutate(STARTDT = RFSTDTC,
                       AEDECOD = ifelse(!is.na(AESTDT) & is.na(AEDECOD), "Not yet coded", AEDECOD),
                       AESTDT = ifelse(is.na(AESTDT) & !is.na(AEDECOD), RFSTDTC, AESTDT))
            
            
            # filter data for seriousness, drug-related, and severity
            if (length(ae_filter)>0){
                if ("Serious" %in% ae_filter) { data <- data %>% filter(AESER == "Y") }
                if ("Drug-related" %in% ae_filter) { data <- data %>% filter(AEREL == "Y") }
                if (sum(c("Mild","Moderate","Severe") %in% ae_filter)>0){
                    severity_filter = ae_filter[which(ae_filter%in% c("Mild","Moderate","Severe"))]
                    data = data %>% filter(AESEV %in% severity_filter)
                }
            }
            
            # filter data for ae timeframe
            if (period == "Treatment emergent") { 
                data <- data %>% filter(TRTEMFL == "Y") 
            } else if (period == "AE during entire study") { 
                data <- data %>% filter(STUDYFL == "Y") 
            }else if (period == "Other") { 
                data <- data %>% filter((AESTDT > RFSTDTC) & (AESTDT < (RFENDTC + residual)))
            }
            
            if (nrow(data) == 0) {return(NULL)}
            
            data$ae_test = data
        }
    })
    
    
    ### Extract all arms ---------------------------------------------------------
    ARMCD <- reactive({
        req(data$ae_test)
        unique(data$ae_test$ARMCD)
    })
    
    
    # output$calculation.type_ui = renderUI({
    #     #req(input$summary_by=="Patients")
    #     selectInput("calculation.type", "Measure of Association", choices = c("Rate Ratio", "Risk Ratio", "Hazard Ratio", "Risk Difference", "Rate Difference"), width="75%")
    # })
    
    
    ### Definitions of UI outputs in the control panel and related logics --------
    output$review_by_please_specify_UI <- renderUI({
        req(input$review_by == "Other")
        textInput("review_by_please_specify", "Please specify")
    })
    
    
    output$period_please_specify_UI <- renderUI({
        req(input$period == "Other")
        numericInput("period_please_specify",
                     HTML("Please enter residual period (in days)"),
                     value = 30, min = 0, max = 10^5)
    })
    
    output$treatment1_UI <- renderUI({
        #req(input$summary_by != "Events")
        div(style = 'height:100px; width:91%; overflow: scroll',
            checkboxGroupInput("treatment1", "Group A",
                               choices = ARMCD(),
                               selected = ARMCD()[1],
                               inline = F)
        )
    })
    
    output$treatment2_UI <- renderUI({
        #req(input$summary_by != "Events")
        div(style = 'height:100px; width:91%; overflow: scroll',
            checkboxGroupInput("treatment2", "Group B",
                               choices = setdiff(ARMCD(), input$treatment1),
                               selected = setdiff(ARMCD(), input$treatment1)[1],
                               inline = F)
        )
    })
    
    output$treatment1_label_UI <- renderUI({
        #req(input$summary_by != "Events")
        textInput("treatment1_label", "Label for Group A", value="Exposed",width="75%")
    })
    
    output$treatment2_label_UI <- renderUI({
        #req(input$summary_by != "Events")
        textInput("treatment2_label", "Label for Group B",value="Unexposed",width="75%")
    })
    
    
    # output$subgroup_vals_UI <- renderUI({
    #   if(input$subgroup_var == "No Subgroup Variable") return()
    #   req(input$summary_by != "Events")
    #   div(style = 'height:100px; width:91%; overflow: scroll',
    #       checkboxGroupInput("subgroup_vals", "Value for Subgroup",
    #                          choices = sort(unique(data$ae_test[[input$subgroup_var]])),
    #                          inline = F)
    #   )
    # })
    
    
    ### An essential step for obtaining statistics right after data uploading and before the generation of volcano plot ------------------------------------
    observeEvent(input$obtain, {
        if (input$period == "Other") {req(input$period_please_specify)}
        
        withProgress(data$statistics <- GetStatistics_all(data = data$ae_test,
                                                          data.mapping = list(stratification_col=ifelse(input$review_by != "SOC","AEDECOD","AEBODSYS"), #the column used to calculate points on the volcano plot (typically 'soc' or 'pt')
                                                                              group_col="ARMCD", #the column containing the comparison group data (typically treatment)
                                                                              reference_group=input$treatment2,
                                                                              comparison_group=input$treatment1,
                                                                              id_col="USUBJID"), 
                                                          calculation.type = input$calculation.type #valid options are 'rate.difference', 'rate.ratio', 'risk.ratio', 'risk.difference', 'hazard.ratio'
        ),
        message = "Executing data pre-processing...", detail = "This step should take a while.", min = 0, max = 1, value = 1)
    })
    
    
    ### Definition of 'Reset' button UI output and its consequent triggerings ----
    observeEvent(input$reset, {
        #updateSelectInput(session, "summary_by", "Summary By",     choices = c("Patients", "Events"))
        updateSelectInput(session, "review_by", "Review By",    choices = c("PT", "SOC", "Other"))
        updateSelectInput(session, "period", "Period", choices = c("Treatment emergent", "AE during entire study",     "Other"))
        selectInput("ae_filter", "Adverse Event filter(s)", choices=c("Serious","Drug-related","Mild","Moderate","Severe"), selected=NULL, multiple=TRUE, width="75%")
        updateSelectInput(session, "calculation.type", "Measure of Association", choices = c("Rate Ratio", "Risk Ratio",  "Hazard Ratio", "Risk Difference",   "Rate Difference"))
        updateCheckboxGroupInput(session, "treatment1", "Group A", choices = ARMCD(), selected = ARMCD()[1], inline = F)
        updateCheckboxGroupInput(session, "treatment2", "Group B", choices = setdiff(ARMCD(), input$treatment1),  selected = setdiff(ARMCD(), input$treatment1)[1],                       inline = F)
        updateTextInput(session, "treatment1_label", "Label for Group A")
        updateTextInput(session, "treatment2_label", "Label for Group B")
        updatenumericInput(session, "X_ref", "X-axis Reference Line", value = 1)
        updateSelectInput(session, "pvalue_option", "p-Value Option", choices = c("Unadjusted", "Adjusted"))
    })
    
    
    
    # if get statistics button, x ref, or pvalue changes, auto update plot
    update_plot = reactive({
        list(input$X_ref, input$pvalue_label,data$statistics)
    })
    
    ### Generation of volcano plot -----------------------------------------------
    observeEvent(update_plot(), {
        req(data$statistics)
        
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
                    ae_filter = input$ae_filter,
                    period = input$period,
                    residual = input$period_please_specify,
                    calculation.type = input$calculation.type,
                    treatment1 = input$treatment1,
                    treatment2 = input$treatment2,
                    X_ref = as.numeric(input$X_ref),
                    X_label = paste(input$calculation.type, input$treatment1_label, "vs.", input$treatment2_label),
                    review_by = input$review_by,
                    #summary_by = input$summary_by,
                    pvalue_label = input$pvalue_label
                )
            )
            if (!"try-error" %in% class(p)) {
                
                tempdata <- data$ae_test %>%
                    mutate_at(names(data$ae_test)[!(names(data$ae_test) %in% c("AESEQ", "AESTDY", "AEENDY","AGE", "RFENDY", "SAFFN"))],~as.character(.)) %>%
                    mutate_at(names(data$ae_test), ~na_if(., ""))
                
                #if (input$summary_by == "Patients") {
                N1.total <- tempdata %>%
                    drop_na(RFSTDTC) %>%
                    filter(ARMCD %in% input$treatment1) %>%
                    distinct(USUBJID) %>% tally() %>% as.numeric()
                
                N2.total <- tempdata %>%
                    drop_na(RFSTDTC) %>%
                    filter(ARMCD %in% input$treatment2) %>%
                    distinct(USUBJID) %>% tally() %>% as.numeric()
                #}
                
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
                
                #if (input$summary_by == "Patients") {
                text1 <- paste0("<- Favors ", input$treatment1_label,
                                " n=", out$N1, " (N=", N1.total, ")")
                text2 <- paste0("Favors ", input$treatment2_label,
                                " n=", out$N2, " (N=", N2.total, ") ->")
                #} else {
                #   text1 <- paste0("<- Favors ", input$treatment1_label,
                #                   " (N=", N1.total, ")")
                #   text2 <- paste0("Favors ", input$treatment2_label,
                #                   " (N=", N2.total, ") ->")
                # }
                
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
        
        # if (input$summary_by == "Events") {
        #   ft <- paste0("<b> Note: </b> <br/>",
        #                "* N is the total number of events. <br/>",
        #                "Dashed horizontal line represents p-value of 0.05 <br/>",
        #                "Dotted horizontal line represents FDR adjsuted p-value of approximately 0.05")
        # }
        
        #if (input$summary_by == "Patients") {
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
        
        #}
        
        return(HTML(ft))
    })
    
    
    
    
    
}


