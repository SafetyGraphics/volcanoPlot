################################################################################
# GetStatistics.R
# 5 Functions: Obtain statistics for further TLG outputs.
################################################################################


#GetStatistics_all  = function(data, ae_filter, period, residual, soc, comparison_group, reference_group, calculation.type) {
GetStatistics_all  = function(data, data.mapping, ae_filter, period, residual, review_by, calculation.type) {
    
    data = as.data.frame(data) # need to integrate merging data
    
    data$stratification_col <- data[,data.mapping$stratification_col] #the column used to calculate points on the volcano plot (typically 'soc' or 'pt')
    data$group_col <- data[,data.mapping$group_col] #the column containing the comparison group data (typically treatment)
    reference_group <- data.mapping$reference_group 
    comparison_group <- data.mapping$comparison_group  
    data$id_col <- data[,data.mapping$id_col]
    
  if (is.null(comparison_group) | is.null(reference_group)) {return(NULL)}
  
  TDDT <- Sys.Date() # use today's date for convenience
  
  data <- data %>%
    mutate_at(names(data)[!(names(data) %in% c("AESEQ", "AESTDY", "AEENDY","AGE", "RFENDY", "SAFFN"))], ~as.character(.)) %>%
    mutate_at(names(data), ~na_if(., "")) %>%
    mutate_at(c("AESTDT", "AEENDT", "RFSTDTC", "RFENDTC"), ~as.Date(.)) %>%
    drop_na(RFSTDTC) %>%
    mutate(STARTDT = RFSTDTC,
           AEDECOD = ifelse(!is.na(AESTDT) & is.na(AEDECOD), "Not yet coded", AEDECOD),
           AESTDT = ifelse(is.na(AESTDT) & !is.na(AEDECOD), RFSTDTC, AESTDT))
  
  
  ## DOESN'T CALCULATION OF N HAVE TO GO AFTER FILTERS???
  if (calculation.type%in% c("Risk Ratio","Risk Difference")){
    N1.total <- length(unique((data %>% filter(group_col %in% comparison_group))$id_col))
    N2.total <- length(unique((data %>% filter(group_col %in% reference_group))$id_col))
  }
  
  
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
  
  data <- data %>% filter(group_col %in% c(comparison_group, reference_group))
  
  if (nrow(data) == 0) {return(NULL)}
  
  data.extend <- NULL
  ae.list = na.omit(unique(data$stratification_col))

  
  for (ae in ae.list) {
    # ae=ae.list[1] # for testing
      
    events <- data %>% filter(stratification_col==ae) %>% mutate(CNSR = 0)
    
    if (review_by != "SOC") {aebodsys <- unique(events$AEBODSYS)}
    
    censors <- data %>%
      filter(!(id_col %in% events$id_col)) %>%
      mutate(CNSR = 1) %>%
      group_by(id_col) %>%
      summarise_all(list(first)) %>%
      {if (review_by != "SOC") mutate(.,
                                AEDECOD = ae,
                                AEBODSYS = aebodsys) else .} %>%
      {if (review_by == "SOC") mutate(., AEBODSYS = ae) else .} %>%
      mutate(AESEQ = NA, AESTDT = NA, AESTDY = NA,
             AEENDT = NA, AEENDY = NA, AETERM = NA,
             AESER = NA, AEONGO = NA, AESEV = NA,
             AEREL = NA, AEOUT = NA, STUDYFL = NA, TRTEMFL = NA) %>%
      {if (review_by == "SOC") mutate(., AEDECOD = NA) else .}
    
    
    extension = bind_rows(events, censors) %>%
        group_by(id_col) %>%
        mutate(ADT = ifelse(CNSR == 1, min(RFENDTC + 30, TDDT), NA)) %>%
        rowwise() %>%
        mutate(ADT = ifelse(CNSR == 0, min(AESTDT, na.rm = TRUE), ADT)) %>%
        mutate(ADT = as.Date(ADT, origin = "1970-01-01")) %>%
        ungroup() %>%
        mutate(AVAL = ADT - STARTDT + 1) %>%
        distinct()%>% 
        mutate(AVAL=as.numeric(AVAL),TRT = ifelse(group_col %in% comparison_group, 1, 0))
    
    
    data.extend <- bind_rows(data.extend, extension)
  }
  
  
  if (calculation.type %in% c("Rate Difference", "Rate Ratio")){
      data.extend.summary <- data.extend %>%
          #group_by(stratification_col) %>%
          {if (review_by != "SOC") group_by(., AEDECOD) else .} %>%
          {if (review_by == "SOC") group_by(., AEBODSYS) else .} %>%
          {if (review_by != "SOC") summarise(.,
                                       AEBODSYS = first(AEBODSYS),
                                       SAVAL1 = sum((group_col %in% comparison_group) * AVAL),
                                       SAVAL2 = sum((group_col %in% reference_group) * AVAL)) else .} %>%
          {if (review_by == "SOC")  summarise(.,
                                      SAVAL1 = sum((group_col %in% comparison_group) * AVAL),
                                      SAVAL2 = sum((group_col %in% reference_group) * AVAL)) else .}
      
      
  }
  
  
  if (calculation.type %in% c("Rate Difference", "Rate Ratio","Risk Difference","Risk Ratio")){
    data.summary <- data %>%
      {if (review_by != "SOC") group_by(., AEDECOD) else .} %>%
      {if (review_by == "SOC") group_by(., AEBODSYS) else .} %>%
      distinct(id_col, .keep_all = TRUE) %>%
      {if (review_by != "SOC") summarise(.,
                                   AEBODSYS = first(AEBODSYS),
                                   COUNT1 = sum(group_col %in% comparison_group),
                                   COUNT2 = sum(group_col %in% reference_group)) else .} %>%
      {if (review_by == "SOC") summarise(.,
                                  COUNT1 = sum(group_col %in% comparison_group),
                                  COUNT2 = sum(group_col %in% reference_group)) else .} %>%
      na.omit()
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
      mutate(LOGPTEST = -log(TESTP))
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
      mutate(LOGPTEST = -log(TESTP))
  }
  
  
  
  ### Rate Ratio (Large Sample Approximation by CLT) -----------------------------
  else if (calculation.type=="Rate Ratio"){
    getRateRatio <- function(count1, count2, saval1, saval2) {
      r <- rateratio(a = count1, b = count2, PT1 = saval1, PT0 = saval2)
      RR <- r$estimate
      RRP <- r$p.value
      
      output <- c(RR, RRP)
      return(output)
    }
    
    result <- cbind(data.summary, data.extend.summary %>% select(SAVAL1, SAVAL2)) %>%
      {if (review_by != "SOC") group_by(., AEDECOD) else .} %>%
      {if (review_by == "SOC") group_by(., AEBODSYS) else .} %>%
      {if (review_by != "SOC") summarise(.,
                                   AEBODSYS = first(AEBODSYS),
                                   RESULT = paste(getRateRatio(COUNT1, COUNT2, SAVAL1, SAVAL2),
                                                  collapse = " ")) else .} %>%
      {if (review_by == "SOC") summarise(.,
                                  RESULT = paste(getRateRatio(COUNT1, COUNT2, SAVAL1, SAVAL2),
                                                 collapse = " ")) else .} %>%
      rowwise() %>%
      mutate(TEST = as.numeric(unlist(strsplit(RESULT, " "))[1]),
             TESTP = as.numeric(unlist(strsplit(RESULT, " "))[2])) %>%
      select(-RESULT) %>%
      mutate(LOGPTEST = -log(TESTP)) %>%
        filter(TEST!=Inf)
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
      mutate(LOGPTEST = -log(TESTP))
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
      mutate(LOGPTEST = -log(TESTP))
  }
  
  
  return(result)
}

