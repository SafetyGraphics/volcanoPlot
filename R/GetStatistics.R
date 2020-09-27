################################################################################
# GetStatistics.R
# 5 Functions: Obtain statistics for further TLG outputs.
################################################################################


GetStatistics_all  = function(filtered_data, data.mapping, review_by) {
    firstup <- function(x) {substr(x, 1, 1) <- toupper(substr(x, 1, 1)); x} # makes PT and SOC names lowercase
    
    data = filtered_data
    data$stratification_col <- data[,data.mapping$stratification_col] #the column used to calculate points on the volcano plot (typically 'soc' or 'pt')
    data$group_col <- data[,data.mapping$group_col] #the column containing the comparison group data (typically treatment)
    reference_group <- data.mapping$reference_group 
    comparison_group <- data.mapping$comparison_group  
    data$id_col <- data[,data.mapping$id_col]
    
    
    # calculates number of subjects in each stratification per treatment arm
    data.summary = data %>% 
        group_by(stratification_col, AEBODSYS, group_col) %>% 
        summarize(n=length(unique(id_col))) %>% 
        ungroup() %>%
        spread(group_col,n) %>% na.omit() %>%
        rename(COUNT1=comparison_group, COUNT2=reference_group) %>%
        mutate(N=COUNT1+COUNT2)
    if (review_by == "SOC") { data.summary = data.summary %>% select(-AEBODSYS)    }
    
    #ae.list = na.omit(unique(data.summary$stratification_col)) # eventually shorten number of ae needed to test
    ae.list = na.omit(unique(data$stratification_col))
    
    data.extend <- NULL
    for (ae in ae.list) {
        # ae=ae.list[1] # for testing
        events <- data %>% filter(stratification_col==ae) %>% mutate(CNSR = 0)

        censors <- data %>%
            filter(!(id_col %in% events$id_col)) %>%
            mutate(CNSR = 1) %>%
            group_by(id_col) %>%
            slice(1) %>%
            group_by() %>%
            {if (review_by != "SOC") mutate(.,
                                            AEDECOD = ae,
                                            AEBODSYS = unique(events$AEBODSYS)) else .} %>%
            {if (review_by == "SOC") mutate(., AEBODSYS = ae) else .} %>%
            mutate(AESTDT = NA,AEENDT = NA,
                   AESER = NA, AEONGO = NA, AESEV = NA,
                   AEREL = NA, AEOUT = NA, STUDYFL = NA, TRTEMFL = NA) %>%
            {if (review_by == "SOC") mutate(., AEDECOD = NA) else .}


        extension = bind_rows(events, censors) %>%
            group_by(id_col) %>%
            mutate(ADT = ifelse(CNSR == 1, min(RFENDTC + 30, Sys.Date()), NA)) %>%
            rowwise() %>%
            mutate(ADT = ifelse(CNSR == 0, min(AESTDT, na.rm = TRUE), ADT)) %>%
            mutate(ADT = as.Date(ADT, origin = "1970-01-01")) %>%
            ungroup() %>%
            mutate(AVAL = ADT - STARTDT + 1) %>%
            distinct()%>%
            mutate(AVAL=as.numeric(AVAL),TRT = ifelse(group_col %in% comparison_group, 1, 0))
        
        data.extend <- bind_rows(data.extend, extension)
    }
    
    data.extend = as.data.frame(data.extend)
    data.extend$"stratification_col" = data.extend[,data.mapping$stratification_col]
    
    

    result_hazard_ratio <- data.extend  %>% 
        filter(stratification_col %in% data.summary$stratification_col) %>% 
        group_by(stratification_col) %>%
        summarize(est.type = "Hazard Ratio",
               est.values = as.numeric(exp(coxph(Surv(AVAL, CNSR) ~ TRT)$coefficients)),
               p = summary(coxph(Surv(AVAL, CNSR) ~ TRT))$coefficients[5]) %>%
        ungroup() %>%
        filter(p!=Inf) %>%
        mutate(p.adj = p.adjust(p, method="fdr")) %>%
        left_join(data.summary)
    
    
    # calculates person year
    data.extend.summary <- data.extend %>%
        group_by(stratification_col, AEBODSYS) %>%
        summarize(SAVAL1 = sum((group_col %in% comparison_group) * AVAL),
                  SAVAL2 = sum((group_col %in% reference_group) * AVAL))
    
    # RATE RATIO
    result_rate_ratio2 <- data.summary %>% 
        filter(stratification_col %in% data.summary$stratification_col) %>% 
        left_join(data.extend.summary) %>%group_by_all() %>%
        mutate(est.type = "Rate Ratio",
               est.values = rateratio(a = COUNT1, b = COUNT2, PT1 = SAVAL1, PT0 = SAVAL2)$estimate,
               p = rateratio(a = COUNT1, b = COUNT2, PT1 = SAVAL1, PT0 = SAVAL2)$p.value) %>%
        ungroup() %>%
        filter(p!=Inf) %>%
        mutate(p.adj = p.adjust(p, method="fdr")) %>%
        select(-c(SAVAL1,SAVAL2)) 
    
    # RATE DIFFERENCE
    result_rate_difference <- data.summary %>% 
        filter(stratification_col %in% data.summary$stratification_col) %>% 
        left_join(data.extend.summary) %>%group_by_all() %>%
        mutate(est.type = "Rate Difference",
               est.values = ratedifference(a = COUNT1, b = COUNT2, PT1 = SAVAL1, PT0 = SAVAL2)$estimate,
               p = ratedifference(a = COUNT1, b = COUNT2, PT1 = SAVAL1, PT0 = SAVAL2)$p.value) %>%
        ungroup() %>%
        filter(p!=Inf) %>%
        mutate(p.adj = p.adjust(p, method="fdr")) %>%
        select(-c(SAVAL1,SAVAL2))
    
    
    
    N1.total <- length(unique((data %>% filter(group_col %in% comparison_group))$id_col))
    N2.total <- length(unique((data %>% filter(group_col %in% reference_group))$id_col))
    
    # RISK RATIO
    result_risk_ratio <- data.summary %>%
        group_by_all() %>%
        mutate(est.type = "Risk Ratio",
               est.values = riskratio(X = COUNT1, Y = COUNT2, m1 = N1.total, m2 = N2.total)$estimate,
               p = riskratio(X = COUNT1,  Y= COUNT2, m1 = N1.total, m2 = N2.total)$p.value) %>%
        ungroup() %>%
        filter(p!=Inf) %>%
        mutate(p.adj = p.adjust(p, method="fdr")) 
    
    
    # RISK DIFFERENCE
    result_risk_difference <- data.summary %>%group_by_all() %>%
        mutate(est.type = "Risk Difference",
               est.values = riskdifference(a = COUNT1, b = COUNT2, N1 = N1.total, N0 = N2.total)$estimate,
               p = riskdifference(a = COUNT1, b = COUNT2, N1 = N1.total, N0 = N2.total)$p.value) %>%
        ungroup() %>%
        filter(p!=Inf) %>%
        mutate(p.adj = p.adjust(p, method="fdr")) 
    
    
    statistics_data = result_hazard_ratio %>% bind_rows(result_rate_ratio) %>% bind_rows(result_rate_difference) %>% 
        bind_rows(result_risk_ratio) %>% bind_rows(result_risk_difference) %>% 
        select(stratification_col, AEBODSYS, COUNT2, COUNT1, N, est.type, est.values, p, p.adj) %>%
        mutate(Summary = paste0("\n", "SOC: ", firstup(tolower(AEBODSYS)),
                                "\n",comparison_group," n=",COUNT1,", ", reference_group," n=", COUNT2,
                                "\n", est.type, ": ", round(est.values,4),
                                "\n","p-value: ", round(p,4)," (FDR: ", round(p.adj,4),")")) %>%
        
        as.data.frame()
     

    if (review_by != "SOC"){
        statistics_data$Summary = paste0("\n","PT: ",firstup(tolower(statistics_data$stratification_col)),statistics_data$Summary)
    }

    
    return(statistics_data)
}

