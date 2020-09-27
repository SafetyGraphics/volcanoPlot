#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'
#' @import shiny
#' @noRd

app_server <- function(input, output, session) {
    print("serving")
    
    
    ### Data uploading -----------------------------------------------------------
    ae_test_data <- reactive({
        req(input$file_ae_test)
        ae_test_data = fread(input$file_ae_test$datapath) %>% as.data.frame()
    })
    
    
    # once data is loaded, all other inputs are loaded
    output$fileUploaded <- reactive({ return(!is.null(ae_test_data())) })
    outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
    
    
    ### Extract all arms ---------------------------------------------------------
    ARMCD <- reactive({ unique(ae_test_data()$ARMCD) })
    
    
    ### Definitions of UI outputs in the control panel and related logics --------
    # output$review_by_please_specify_UI <- renderUI({
    #     req(input$review_by == "Other")
    #     textInput("review_by_please_specify", "Please specify")
    # })
    
    
    # output$period_please_specify_UI <- renderUI({
    #     req(input$period == "Other")
    #     numericInput("period_please_specify",
    #                  HTML("Please enter residual period (in days)"),
    #                  value = 30, min = 0, max = 10^5)
    # })
    
    
    output$comparison_group_UI <- renderUI({
        #req(input$summary_by != "Events")
        selectizeInput("comparison_group", "Comparison Group",choices = setdiff(ARMCD(), input$reference_group), selected = setdiff(ARMCD(), input$reference_group)[1], multiple=TRUE)
    })
    
    output$reference_group_UI <- renderUI({
        #req(input$summary_by != "Events")
        selectizeInput("reference_group", "Reference Group", choices = ARMCD(), selected = ARMCD()[1], multiple=TRUE)
    })
    
    
    observe({
        req(input$file_ae_test)
        req(!is.null(input$reference_group))
        if(length(input$reference_group) == 0) {
            shinyjs::alert("Please select Group A.")
            return()
        }
        
        req(!is.null(input$comparison_group))
        if(length(input$comparison_group) == 0) {
            shinyjs::alert("Please select Group B.")
            return()
        }
    })
    
    
    filtered_data = reactive({
        period=input$period; residual=input$residual; review_by=input$review_by; ae_filter=input$ae_filter; comparison_group=input$comparison_group; reference_group=input$reference_group
        ae_test_data = ae_test_data(); 
        
        filtered_data <- ae_test_data %>% as.data.frame() %>% #select(-c(AESTDY, AEENDY, RFENDY, AEONGO, AETERM)) %>%
            drop_na(RFSTDTC) %>%
            filter(ARMCD %in% c(reference_group, comparison_group)) %>%
            mutate_at(names(ae_test_data)[!(names(ae_test_data) %in% c("AESTDY", "AEENDY", "RFENDY"))], ~as.character(.)) %>%
            mutate_at(names(ae_test_data), ~na_if(., "")) %>%
            mutate(STARTDT = as.Date(RFSTDTC),
                   AEDECOD = ifelse(!is.na(AESTDT) & is.na(AEDECOD), "Not yet coded", as.character(AEDECOD)),
                   AESTDT = ifelse(is.na(AESTDT) & !is.na(AEDECOD), as.character(RFSTDTC), as.character(AESTDT))) %>%
            mutate_at(c("AESTDT", "AEENDT", "RFENDTC"), ~as.Date(.))
        
        
        # filter data for seriousness, drug-related, and severity
        if (length(ae_filter)>0){
            if ("Serious" %in% ae_filter) { filtered_data <- filtered_data %>% filter(AESER == "Y") }
            if ("Drug-related" %in% ae_filter) { filtered_data <- filtered_data %>% filter(AEREL == "Y") }
            if (sum(c("Mild","Moderate","Severe") %in% ae_filter)>0){
                severity_filter = ae_filter[which(ae_filter%in% c("Mild","Moderate","Severe"))]
                filtered_data = filtered_data %>% filter(AESEV %in% severity_filter)
            }
        }
        
        # filter data for ae timeframe
        if (period == "Treatment emergent") {
            filtered_data <- filtered_data %>% filter(TRTEMFL == "Y")
        } else if (period == "AE during entire study") {
            filtered_data <- filtered_data %>% filter(STUDYFL == "Y")
        }else if (period == "Other") {
            filtered_data <- filtered_data %>% filter((AESTDT > RFSTDTC) & (AESTDT < (RFENDTC + residual)))
        }
        
        filtered_data = as.data.frame(filtered_data)
    })
    
    
    data <- reactiveValues( statistics = NULL )
    
    
    
    ### An essential step for obtaining statistics right after data uploading and before the generation of volcano plot ------------------------------------
    observeEvent(input$obtain, {
        #if (input$period == "Other") {req(input$period_please_specify)}
        withProgress(data$statistics <- GetStatistics_all(filtered_data = filtered_data(),
                                                          data.mapping = list(stratification_col=ifelse(input$review_by != "SOC","AEDECOD","AEBODSYS"), #the column used to calculate points on the volcano plot (typically 'soc' or 'pt')
                                                                              group_col="ARMCD", #the column containing the comparison group data (typically treatment)
                                                                              reference_group=input$reference_group,
                                                                              comparison_group=input$comparison_group,
                                                                              id_col="USUBJID"),
                                                          review_by=input$review_by
        ),
        message = "Executing data pre-processing...", detail = "This step should take a while.", min = 0, max = 1, value = 1)
    })
    

    
    # # if get statistics button, x ref, or pvalue changes, auto update plot
    update_plot = reactive({
        list(input$X_ref, input$pvalue_label,data$statistics,input$calculation.type)#, #nput$reference_group_label, input$comparison_group_label)
    })
    
    ### Generation of volcano plot -----------------------------------------------
    observeEvent(update_plot(), {
        req(nrow(data$statistics)>0)
        
        output$volcano_plot = renderPlotly({
            volcano.plot(
                filtered_data = filtered_data(),
                statistics_data = data$statistics%>% filter(est.type == input$calculation.type),
                calculation.type = input$calculation.type,
                reference_group = input$reference_group,
                comparison_group = input$comparison_group,
                pvalue_label = input$pvalue_label,
                X_ref = input$X_ref
            )
        })
        
    })
    
    


    
        #List the details of filtered data that matches the selected SOC/PT
        output$volcano_plot_drill <- DT::renderDataTable({
            s=event_data("plotly_click",source="volcano_plot")
            req(length(s)>0)
            ae_test_data = ae_test_data()

            selected_stratification =  data$statistics[s[[2]]+1,"stratification_col"]
            
            ae_test_data$stratification_col = ae_test_data[,ifelse(input$review_by != "SOC","AEDECOD","AEBODSYS")]
            
            volcano_plot_table = ae_test_data %>% filter(stratification_col==selected_stratification) %>%
                select(USUBJID, ARMCD, AEBODSYS, AEDECOD,  AESTDT, AESTDY,  AEENDT,  AESER,  AEONGO,AESEV, AEREL,     AEOUT,TRTEMFL,  STUDYFL)
            
            colnames(volcano_plot_table) = c('Subject ID', 'Treatment', 'SOC',
                                             'PT', 'Onset Date', 'Time to Onset',  'End Date','Serious AE?','Still Ongoing?','Severity',
                                             'Causality','Outcome', 'Treatment Emergent?','During Entire Study?')
            
            datatable(volcano_plot_table, extensions = 'Buttons', options = list(dom = 'Bfrtip', buttons = I('colvis'), pageLength = 10),rownames= FALSE)
        })



    #     ### Specifications on the footnote of volcano plot ---------------------------
    output$footnote_UI <- renderText({
        req(nrow(data$statistics)>0)
        
        # if (input$summary_by == "Events") {
        #   ft <- paste0("<b> Note: </b> <br/>",
        #                "* N is the total number of events. <br/>",
        #                "Dashed horizontal line represents p-value of 0.05 <br/>",
        #                "Dotted horizontal line represents FDR adjsuted p-value of approximately 0.05")
        # }
        
        #if (input$summary_by == "Patients") {
        
        ftn = "* n is the number of patients with adverse events."
        
        if (!is.null(input$ae_filter)){
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
                     "Dotted horizontal line represents FDR adjsuted p-value of approximately 0.05")
        #}
        
        HTML(ft)
    })
    
    
    
}

