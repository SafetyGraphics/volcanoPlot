################################################################################
# GetStatistics.R
# 5 Functions: Obtain statistics for further TLG outputs.
################################################################################


GetStatistics_all  = function(data, data.mapping, calculation.type) {
    
    data = as.data.frame(data)
    
    data$stratification_col <- data[,data.mapping$stratification_col] #the column used to calculate points on the volcano plot (typically 'soc' or 'pt')
    data$group_col <- data[,data.mapping$group_col] #the column containing the comparison group data (typically treatment)
    reference_group <- data.mapping$reference_group 
    comparison_group <- data.mapping$comparison_group  
    data$id_col <- data[,data.mapping$id_col]
    
    
    if (calculation.type%in% c("Risk Ratio","Risk Difference")){
        N1.total <- length(unique((data %>% filter(group_col %in% comparison_group))$id_col))
        N2.total <- length(unique((data %>% filter(group_col %in% reference_group))$id_col))
    }
    
    
    # calculates number of subjects in each stratification per treatment arm
    data.summary = data %>% 
        group_by(stratification_col, AEBODSYS, group_col) %>% 
        summarize(n=length(unique(id_col))) %>% 
        ungroup() %>%
        spread(group_col,n) %>% na.omit() %>%
        rename(COUNT1=comparison_group, COUNT2=reference_group)
    if (review_by == "SOC") { data.summary = data.summary %>% select(-AEBODSYS)    }
    
    #ae.list = na.omit(unique(data.summary$stratification_col))
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
    
    
    if (calculation.type %in% c("Rate Difference", "Rate Ratio")){
        
        data.extend.summary <- data.extend %>%
            group_by(stratification_col, AEBODSYS) %>%
            summarize(SAVAL1 = sum((group_col %in% comparison_group) * AVAL),
                      SAVAL2 = sum((group_col %in% reference_group) * AVAL))
        
        
    }
    
    
    
    
    ### Hazard Ratio (Cox's Proportional Hazards Model) ----------------------------
    if (calculation.type=="Hazard Ratio"){
        
        
        getHazardRatio <- function(time, delta, trt) {
            fit <- coxph(Surv(time, delta) ~ trt)
            S <- summary(fit)
            
            HR <- unname(exp(fit$coefficients))
            HRP <- S$coefficients[5]
            
            output <- c(HR, HRP)
            return(output)
        }
        
        result <- data.extend %>%
            mutate(AVAL = as.numeric(AVAL)) %>%
            {if (review_by != "SOC") group_by(., AEDECOD) else .} %>%
            {if (review_by == "SOC") group_by(., AEBODSYS) else .} %>%
            {if (review_by != "SOC") summarise(.,
                                               AEBODSYS = first(AEBODSYS),
                                               RESULT = paste(getHazardRatio(AVAL, CNSR, TRT),
                                                              collapse = " ")) else .} %>%
            {if (review_by == "SOC") summarise(.,
                                               RESULT = paste(getHazardRatio(AVAL, CNSR, TRT),
                                                              collapse = " ")) else .} %>%
            rowwise() %>%
            mutate(TEST = as.numeric(unlist(strsplit(RESULT, " "))[1]),
                   TESTP = as.numeric(unlist(strsplit(RESULT, " "))[2])) %>%
            select(-RESULT) %>%
            mutate(LOGPTEST = -log(TESTP))%>%
            filter(TEST!=Inf)
    }
    
    
    
    ### Rate Difference (Large Sample Approximation by CLT) ------------------------
    else if (calculation.type=="Rate Difference"){
        getRateDifference <- function(count1, count2, saval1, saval2) {
            r <- ratedifference(a = count1, b = count2, PT1 = saval1, PT0 = saval2)
            RD <- r$estimate
            RDP <- r$p.value
            
            output <- c(RD, RDP)
            return(output)
        }
        
        result <- cbind(data.summary, data.extend.summary %>% select(SAVAL1, SAVAL2)) %>%
            {if (review_by != "SOC") group_by(., AEDECOD) else .} %>%
            {if (review_by == "SOC") group_by(., AEBODSYS) else .} %>%
            {if (review_by != "SOC") summarise(.,
                                               AEBODSYS = first(AEBODSYS),
                                               RESULT = paste(getRateDifference(COUNT1, COUNT2, SAVAL1, SAVAL2),
                                                              collapse = " ")) else .} %>%
            {if (review_by == "SOC") summarise(.,
                                               RESULT = paste(getRateDifference(COUNT1, COUNT2, SAVAL1, SAVAL2),
                                                              collapse = " ")) else .} %>%
            rowwise() %>%
            mutate(TEST = as.numeric(unlist(strsplit(RESULT, " "))[1]),
                   TESTP = as.numeric(unlist(strsplit(RESULT, " "))[2])) %>%
            select(-RESULT) %>%
            mutate(LOGPTEST = -log(TESTP))%>%
            filter(TEST!=Inf)
    }
    
    
    
    ### Rate Ratio (Large Sample Approximation by CLT) -----------------------------
    else if (calculation.type=="Rate Ratio"){
        result <- data.summary %>% left_join(data.extend.summary) %>%
            mutate(est.type = "Rate Ratio",
                   est.values = rateratio(a = COUNT1, b = COUNT2, PT1 = SAVAL1, PT0 = SAVAL2)$estimate,
                   p = rateratio(a = COUNT1, b = COUNT2, PT1 = SAVAL1, PT0 = SAVAL2)$p.value) %>%
            filter(p!=Inf) %>%
            mutate(p.adj = p.adjust(p, method="fdr")) %>%
            select(-c(SAVAL1,SAVAL2))
    }
    
    
    ### Risk Difference (Large Sample Approximation by CLT) ------------------------
    else if (calculation.type=="Risk Difference"){
        getRiskDifference <- function(count1, count2) {
            r <- riskdifference(a = count1, b = count2, N1 = N1.total, N0 = N2.total)
            RD <- r$estimate
            RDP <- r$p.value
            
            output <- c(RD, RDP)
            return(output)
        }
        
        result <- data.summary %>%
            {if (review_by != "SOC") group_by(., AEDECOD) else .} %>%
            {if (review_by == "SOC") group_by(., AEBODSYS) else .} %>%
            {if (review_by != "SOC") summarise(.,
                                               AEBODSYS = first(AEBODSYS),
                                               RESULT = paste(getRiskDifference(COUNT1, COUNT2),
                                                              collapse = " ")) else .} %>%
            {if (review_by == "SOC") summarise(.,
                                               RESULT = paste(getRiskDifference(COUNT1, COUNT2),
                                                              collapse = " ")) else .} %>%
            rowwise() %>%
            mutate(TEST = as.numeric(unlist(strsplit(RESULT, " "))[1]),
                   TESTP = as.numeric(unlist(strsplit(RESULT, " "))[2])) %>%
            select(-RESULT) %>%
            mutate(LOGPTEST = -log(TESTP))%>%
            filter(TEST!=Inf)
    }
    
    
    
    ### Risk Ratio (Large Sample Approximation by CLT) -----------------------------
    else if (calculation.type=="Risk Ratio"){
        # NEED N1 AND N2
        getRiskRatio <- function(count1, count2) {
            r <- riskratio(X = count1, Y = count2, m1 = N1.total, m2 = N2.total)
            RR <- r$estimate
            RRP <- r$p.value
            
            output <- c(RR, RRP)
            return(output)
        }
        
        result <- data.summary %>%
            {if (review_by != "SOC") group_by(., AEDECOD) else .} %>%
            {if (review_by == "SOC") group_by(., AEBODSYS) else .} %>%
            {if (review_by != "SOC") summarise(.,
                                               AEBODSYS = first(AEBODSYS),
                                               RESULT = paste(getRiskRatio(COUNT1, COUNT2),
                                                              collapse = " ")) else .} %>%
            {if (review_by == "SOC") summarise(.,
                                               RESULT = paste(getRiskRatio(COUNT1, COUNT2),
                                                              collapse = " ")) else .} %>%
            rowwise() %>%
            mutate(TEST = as.numeric(unlist(strsplit(RESULT, " "))[1]),
                   TESTP = as.numeric(unlist(strsplit(RESULT, " "))[2])) %>%
            select(-RESULT) %>%
            mutate(LOGPTEST = -log(TESTP))%>%
            filter(TEST!=Inf)
    }
    
    
    return(result)
}

