################################################################################
# GetStatistics.R
# 5 Functions: Obtain statistics for further TLG outputs.
################################################################################

### Hazard Ratio (Cox's Proportional Hazards Model) ----------------------------
GetStatistics_HazardR <- function(data, ser, drug, period, residual, soc,
                                  treatment1, treatment2) {
  
  if (is.null(treatment1) | is.null(treatment2)) {return(NULL)}
  
  TDDT <- Sys.Date() # use today's date for convenience
  
  data <- data %>%
    mutate_at(names(data)[!(names(data) %in% c("AESEQ", "AESTDY", "AEENDY","AGE", "RFENDY", "SAFFN"))], ~as.character(.)) %>%
    mutate_at(names(data), ~na_if(., "")) %>%
    mutate_at(c("AESTDT", "AEENDT", "RFSTDTC", "RFENDTC"), ~as.Date(.)) %>%
    drop_na(RFSTDTC) %>%
    mutate(STARTDT = RFSTDTC,
           AEDECOD = ifelse(!is.na(AESTDT) & is.na(AEDECOD), "Not yet coded", AEDECOD),
           AESTDT = ifelse(is.na(AESTDT) & !is.na(AEDECOD), RFSTDTC, AESTDT))
  
  if (ser == TRUE) { data <- data %>% filter(AESER == "Y") }
  
  if (drug == TRUE) { data <- data %>% filter(AEREL == "Y") }
  
  if (period == "Treatment emergent") { data <- data %>% filter(TRTEMFL == "Y") }
  
  if (period == "AE during entire study") { data <- data %>% filter(STUDYFL == "Y") }
  
  if (period == "Other") { data <- data %>% filter((AESTDT > RFSTDTC) & (AESTDT < (RFENDTC + residual))) }
  
  data <- data %>% filter(ARMCD %in% c(treatment1, treatment2))
  
  if (nrow(data) == 0) {return(NULL)}
  
  data.extend <- NULL
  if (soc == FALSE) {
    ae.list <- na.omit(unique(data$AEDECOD))
  } else {
    ae.list <- na.omit(unique(data$AEBODSYS))
  }
  
  for (ae in ae.list) {
    
    events <- data %>%
      {if (soc == FALSE) filter(., AEDECOD == ae) else .} %>%
      {if (soc == TRUE) filter(., AEBODSYS == ae) else .} %>%
      mutate(CNSR = 0)
    
    if (soc == FALSE) {
      aebodsys <- unique(events$AEBODSYS)
    }
    
    censors <- data %>%
      filter(!(USUBJID %in% events$USUBJID)) %>%
      mutate(CNSR = 1) %>%
      group_by(USUBJID) %>%
      summarise_all(list(first)) %>%
      {if (soc == FALSE) mutate(.,
                                AEDECOD = ae,
                                AEBODSYS = aebodsys) else .} %>%
      {if (soc == TRUE) mutate(., AEBODSYS = ae) else .} %>%
      mutate(AESEQ = NA, AESTDT = NA, AESTDY = NA,
             AEENDT = NA, AEENDY = NA, AETERM = NA,
             AESER = NA, AEONGO = NA, AESEV = NA,
             AEREL = NA, AEOUT = NA, STUDYFL = NA, TRTEMFL = NA) %>%
      {if (soc == TRUE) mutate(., AEDECOD = NA) else .}
    
    extension <- rbind(events, censors) %>%
      group_by(USUBJID) %>%
      mutate(ADT = ifelse(CNSR == 1, min(RFENDTC + 30, TDDT), NA)) %>%
      rowwise() %>%
      mutate(ADT = ifelse(CNSR == 0, min(AESTDT, na.rm = TRUE), ADT)) %>%
      mutate(ADT = as.Date(ADT, origin = "1970-01-01")) %>%
      ungroup() %>%
      mutate(AVAL = ADT - STARTDT + 1) %>%
      distinct()
    
    data.extend <- rbind(data.extend, extension)
  }
  
  data.extend <- data.extend %>%
    mutate(TRT = ifelse(ARMCD %in% treatment1, 1, 0))
  
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
    {if (soc == FALSE) group_by(., AEDECOD) else .} %>%
    {if (soc == TRUE) group_by(., AEBODSYS) else .} %>%
    {if (soc == FALSE) summarise(.,
                                 AEBODSYS = first(AEBODSYS),
                                 RESULT = paste(getHazardRatio(AVAL, CNSR, TRT),
                                                collapse = " ")) else .} %>%
    {if (soc == TRUE) summarise(.,
                                RESULT = paste(getHazardRatio(AVAL, CNSR, TRT),
                                               collapse = " ")) else .} %>%
    rowwise() %>%
    mutate(TEST = as.numeric(unlist(strsplit(RESULT, " "))[1]),
           TESTP = as.numeric(unlist(strsplit(RESULT, " "))[2])) %>%
    select(-RESULT) %>%
    mutate(LOGPTEST = -log(TESTP))
  
  return(result)
}


### Rate Difference (Large Sample Approximation by CLT) ------------------------
GetStatistics_RateD <- function(data, ser, drug, period, residual, soc,treatment1, treatment2) {
  
  if (is.null(treatment1) | is.null(treatment2)) {return(NULL)}
  
  TDDT <- Sys.Date() # use today's date for convenience
  
  data <- data %>%
    mutate_at(names(data)[!(names(data) %in% c("AESEQ", "AESTDY", "AEENDY","AGE", "RFENDY", "SAFFN"))],~as.character(.)) %>%
    mutate_at(names(data), ~na_if(., "")) %>%
    mutate_at(c("AESTDT", "AEENDT", "RFSTDTC", "RFENDTC"), ~as.Date(.)) %>%
    drop_na(RFSTDTC) %>%
    mutate(STARTDT = RFSTDTC) %>%
    mutate(AEDECOD = ifelse(!is.na(AESTDT) & is.na(AEDECOD), "Not yet coded", AEDECOD)) %>%
    mutate(AESTDT = ifelse(is.na(AESTDT) & !is.na(AEDECOD), RFSTDTC, AESTDT))
  
  if (ser == TRUE) {
    data <- data %>% filter(AESER == "Y")
  }
  
  if (drug == TRUE) {
    data <- data %>% filter(AEREL == "Y")
  }
  
  if (period == "Treatment emergent") {
    data <- data %>% filter(TRTEMFL == "Y")
  }
  
  if (period == "AE during entire study") {
    data <- data %>% filter(STUDYFL == "Y")
  }
  
  if (period == "Other") {
    data <- data %>% filter((AESTDT > RFSTDTC) & (AESTDT < (RFENDTC + residual)))
  }
  
  data <- data %>% filter(ARMCD %in% c(treatment1, treatment2))
  
  if (nrow(data) == 0) {return(NULL)}
  
  data.extend <- NULL
  if (soc == FALSE) {
    ae.list <- na.omit(unique(data$AEDECOD))
  } else {
    ae.list <- na.omit(unique(data$AEBODSYS))
  }
  
  for (ae in ae.list) {
    
    events <- data %>%
      {if (soc == FALSE) filter(., AEDECOD == ae) else .} %>%
      {if (soc == TRUE) filter(., AEBODSYS == ae) else .} %>%
      mutate(CNSR = 0)
    
    if (soc == FALSE) {
      aebodsys <- unique(events$AEBODSYS)
    } else {}
    
    censors <- data %>%
      filter(!(USUBJID %in% events$USUBJID)) %>%
      mutate(CNSR = 1) %>%
      group_by(USUBJID) %>%
      summarise_all(list(first)) %>%
      {if (soc == FALSE) mutate(.,
                                AEDECOD = ae,
                                AEBODSYS = aebodsys) else .} %>%
      {if (soc == TRUE) mutate(., AEBODSYS = ae) else .} %>%
      mutate(AESEQ = NA, AESTDT = NA, AESTDY = NA,
             AEENDT = NA, AEENDY = NA, AETERM = NA,
             AESER = NA, AEONGO = NA, AESEV = NA,
             AEREL = NA, AEOUT = NA, STUDYFL = NA, TRTEMFL = NA) %>%
      {if (soc == TRUE) mutate(., AEDECOD = NA) else .}
    
    extension <- rbind(events, censors) %>%
      group_by(USUBJID) %>%
      mutate(ADT = ifelse(CNSR == 1, min(RFENDTC + 30, TDDT), NA)) %>%
      rowwise() %>%
      mutate(ADT = ifelse(CNSR == 0, min(AESTDT, na.rm = TRUE), ADT)) %>%
      mutate(ADT = as.Date(ADT, origin = "1970-01-01")) %>%
      ungroup() %>%
      mutate(AVAL = ADT - STARTDT + 1) %>%
      distinct()
    
    data.extend <- rbind(data.extend, extension)
  }
  
  data.extend.summary <- data.extend %>%
    mutate(AVAL = as.numeric(AVAL)) %>%
    {if (soc == FALSE) group_by(., AEDECOD) else .} %>%
    {if (soc == TRUE) group_by(., AEBODSYS) else .} %>%
    {if (soc == FALSE) summarise(.,
                                 AEBODSYS = first(AEBODSYS),
                                 SAVAL1 = sum((ARMCD %in% treatment1) * AVAL),
                                 SAVAL2 = sum((ARMCD %in% treatment2) * AVAL)) else .} %>%
    {if (soc == TRUE) summarise(.,
                                SAVAL1 = sum((ARMCD %in% treatment1) * AVAL),
                                SAVAL2 = sum((ARMCD %in% treatment2) * AVAL)) else .}
  
  data.summary <- data %>%
    {if (soc == FALSE) group_by(., AEDECOD) else .} %>%
    {if (soc == TRUE) group_by(., AEBODSYS) else .} %>%
    distinct(USUBJID, .keep_all = TRUE) %>%
    {if (soc == FALSE) summarise(.,
                                 AEBODSYS = first(AEBODSYS),
                                 COUNT1 = sum(ARMCD %in% treatment1),
                                 COUNT2 = sum(ARMCD %in% treatment2)) else .} %>%
    {if (soc == TRUE) summarise(.,
                                COUNT1 = sum(ARMCD %in% treatment1),
                                COUNT2 = sum(ARMCD %in% treatment2)) else .} %>%
    na.omit()
  
  getRateDifference <- function(count1, count2, saval1, saval2) {
    r <- ratedifference(a = count1, b = count2, PT1 = saval1, PT0 = saval2)
    RD <- r$estimate
    RDP <- r$p.value
    
    output <- c(RD, RDP)
    return(output)
  }
  
  result <- cbind(data.summary, data.extend.summary %>% select(SAVAL1, SAVAL2)) %>%
    {if (soc == FALSE) group_by(., AEDECOD) else .} %>%
    {if (soc == TRUE) group_by(., AEBODSYS) else .} %>%
    {if (soc == FALSE) summarise(.,
                                 AEBODSYS = first(AEBODSYS),
                                 RESULT = paste(getRateDifference(COUNT1, COUNT2, SAVAL1, SAVAL2),
                                                collapse = " ")) else .} %>%
    {if (soc == TRUE) summarise(.,
                                RESULT = paste(getRateDifference(COUNT1, COUNT2, SAVAL1, SAVAL2),
                                               collapse = " ")) else .} %>%
    rowwise() %>%
    mutate(TEST = as.numeric(unlist(strsplit(RESULT, " "))[1]),
           TESTP = as.numeric(unlist(strsplit(RESULT, " "))[2])) %>%
    select(-RESULT) %>%
    mutate(LOGPTEST = -log(TESTP))
  
  return(result)
}


### Rate Ratio (Large Sample Approximation by CLT) -----------------------------
GetStatistics_RateR <- function(data, ser, drug, period, residual, soc, treatment1, treatment2) {
  
  if (is.null(treatment1) | is.null(treatment2)) {return(NULL)}
  
  TDDT <- Sys.Date() # use today's date for convenience
  
  data <- data %>%
    mutate_at(names(data)[!(names(data) %in% c("AESEQ", "AESTDY", "AEENDY","AGE", "RFENDY", "SAFFN"))], ~as.character(.)) %>%
    mutate_at(names(data), ~na_if(., "")) %>%
    mutate_at(c("AESTDT", "AEENDT", "RFSTDTC", "RFENDTC"), ~as.Date(.)) %>%
    drop_na(RFSTDTC) %>%
    mutate(STARTDT = RFSTDTC) %>%
    mutate(AEDECOD = ifelse(!is.na(AESTDT) & is.na(AEDECOD), "Not yet coded", AEDECOD)) %>%
    mutate(AESTDT = ifelse(is.na(AESTDT) & !is.na(AEDECOD), RFSTDTC, AESTDT))
  
  if (ser == TRUE) {
    data <- data %>% filter(AESER == "Y")
  }
  
  if (drug == TRUE) {
    data <- data %>% filter(AEREL == "Y")
  }
  
  if (period == "Treatment emergent") {
    data <- data %>% filter(TRTEMFL == "Y")
  }
  
  if (period == "AE during entire study") {
    data <- data %>% filter(STUDYFL == "Y")
  }
  
  if (period == "Other") {
    data <- data %>% filter((AESTDT > RFSTDTC) & (AESTDT < (RFENDTC + residual)))
  }
  
  data <- data %>% filter(ARMCD %in% c(treatment1, treatment2))
  
  if (nrow(data) == 0) {return(NULL)}
  
  data.extend <- NULL
  if (soc == FALSE) {
    ae.list <- na.omit(unique(data$AEDECOD))
  } else {
    ae.list <- na.omit(unique(data$AEBODSYS))
  }
  
  for (ae in ae.list) {
    
    events <- data %>%
    {if (soc == FALSE) filter(., AEDECOD == ae) else .} %>%
    {if (soc == TRUE) filter(., AEBODSYS == ae) else .} %>%
      mutate(CNSR = 0)
    
    if (soc == FALSE) {
      aebodsys <- unique(events$AEBODSYS)
    } else {}
    
    censors <- data %>%
      filter(!(USUBJID %in% events$USUBJID)) %>%
      mutate(CNSR = 1) %>%
      group_by(USUBJID) %>%
      summarise_all(list(first)) %>%
      {if (soc == FALSE) mutate(.,
                                AEDECOD = ae,
                                AEBODSYS = aebodsys) else .} %>%
                                {if (soc == TRUE) mutate(., AEBODSYS = ae) else .} %>%
      mutate(AESEQ = NA, AESTDT = NA, AESTDY = NA,
             AEENDT = NA, AEENDY = NA, AETERM = NA,
             AESER = NA, AEONGO = NA, AESEV = NA,
             AEREL = NA, AEOUT = NA, STUDYFL = NA, TRTEMFL = NA) %>%
             {if (soc == TRUE) mutate(., AEDECOD = NA) else .}
    
    extension <- rbind(events, censors) %>%
      group_by(USUBJID) %>%
      mutate(ADT = ifelse(CNSR == 1, min(RFENDTC + 30, TDDT), NA)) %>%
      rowwise() %>%
      mutate(ADT = ifelse(CNSR == 0, min(AESTDT, na.rm = TRUE), ADT)) %>%
      mutate(ADT = as.Date(ADT, origin = "1970-01-01")) %>%
      ungroup() %>%
      mutate(AVAL = ADT - STARTDT + 1) %>%
      distinct()
    
    data.extend <- rbind(data.extend, extension)
  }
  
  data.extend.summary <- data.extend %>%
    mutate(AVAL = as.numeric(AVAL)) %>%
    {if (soc == FALSE) group_by(., AEDECOD) else .} %>%
    {if (soc == TRUE) group_by(., AEBODSYS) else .} %>%
    {if (soc == FALSE) summarise(.,
                                 AEBODSYS = first(AEBODSYS),
                                 SAVAL1 = sum((ARMCD %in% treatment1) * AVAL),
                                 SAVAL2 = sum((ARMCD %in% treatment2) * AVAL)) else .} %>%
    {if (soc == TRUE) summarise(.,
                                SAVAL1 = sum((ARMCD %in% treatment1) * AVAL),
                                SAVAL2 = sum((ARMCD %in% treatment2) * AVAL)) else .}
  
  data.summary <- data %>%
    {if (soc == FALSE) group_by(., AEDECOD) else .} %>%
    {if (soc == TRUE) group_by(., AEBODSYS) else .} %>%
    distinct(USUBJID, .keep_all = TRUE) %>%
    {if (soc == FALSE) summarise(.,
                                 AEBODSYS = first(AEBODSYS),
                                 COUNT1 = sum(ARMCD %in% treatment1),
                                 COUNT2 = sum(ARMCD %in% treatment2)) else .} %>%
    {if (soc == TRUE) summarise(.,
                                COUNT1 = sum(ARMCD %in% treatment1),
                                COUNT2 = sum(ARMCD %in% treatment2)) else .} %>%
    na.omit()
  
  getRateRatio <- function(count1, count2, saval1, saval2) {
    r <- rateratio(a = count1, b = count2, PT1 = saval1, PT0 = saval2)
    RR <- r$estimate
    RRP <- r$p.value
    
    output <- c(RR, RRP)
    return(output)
  }
  
  result <- cbind(data.summary, data.extend.summary %>% select(SAVAL1, SAVAL2)) %>%
    {if (soc == FALSE) group_by(., AEDECOD) else .} %>%
    {if (soc == TRUE) group_by(., AEBODSYS) else .} %>%
    {if (soc == FALSE) summarise(.,
                                 AEBODSYS = first(AEBODSYS),
                                 RESULT = paste(getRateRatio(COUNT1, COUNT2, SAVAL1, SAVAL2),
                                                collapse = " ")) else .} %>%
    {if (soc == TRUE) summarise(.,
                                RESULT = paste(getRateRatio(COUNT1, COUNT2, SAVAL1, SAVAL2),
                                               collapse = " ")) else .} %>%
    rowwise() %>%
    mutate(TEST = as.numeric(unlist(strsplit(RESULT, " "))[1]),
           TESTP = as.numeric(unlist(strsplit(RESULT, " "))[2])) %>%
    select(-RESULT) %>%
    mutate(LOGPTEST = -log(TESTP))
  
  return(result)
}


### Risk Difference (Large Sample Approximation by CLT) ------------------------
GetStatistics_RiskD <- function(data, ser, drug, period, residual, soc,
                                treatment1, treatment2) {
  
  if (is.null(treatment1) | is.null(treatment2)) {return(NULL)}
  
  data <- data %>%
    mutate_at(names(data)[!(names(data) %in% c("AESEQ", "AESTDY", "AEENDY",
                                               "AGE", "RFENDY", "SAFFN"))],
              ~as.character(.)) %>%
    mutate_at(names(data), ~na_if(., "")) %>%
    mutate_at(c("AESTDT", "AEENDT", "RFSTDTC", "RFENDTC"), ~as.Date(.)) %>%
    drop_na(RFSTDTC) %>%
    mutate(AEDECOD = ifelse(!is.na(AESTDT) & is.na(AEDECOD), "Not yet coded", AEDECOD)) %>%
    mutate(AESTDT = ifelse(is.na(AESTDT) & !is.na(AEDECOD), RFSTDTC, AESTDT))
  
  N1.total <- data %>%
    filter(ARMCD %in% treatment1) %>% distinct(USUBJID) %>%
    tally() %>% as.numeric()
  
  N2.total <- data %>%
    filter(ARMCD %in% treatment2) %>% distinct(USUBJID) %>%
    tally() %>% as.numeric()
  
  if (ser == TRUE) { data <- data %>% filter(AESER == "Y") }
  
  if (drug == TRUE) { data <- data %>% filter(AEREL == "Y") }
  
  if (period == "Treatment emergent") { data <- data %>% filter(TRTEMFL == "Y") }
  
  if (period == "AE during entire study") { data <- data %>% filter(STUDYFL == "Y") }
  
  if (period == "Other") {  data <- data %>% filter((AESTDT > RFSTDTC) & (AESTDT < (RFENDTC + residual))) }
  
  data <- data %>% filter(ARMCD %in% c(treatment1, treatment2))
  
  if (nrow(data) == 0) {return(NULL)}
  
  data.summary <- data %>%
    {if (soc == FALSE) group_by(., AEDECOD) else .} %>%
    {if (soc == TRUE) group_by(., AEBODSYS) else .} %>%
    distinct(USUBJID, .keep_all = TRUE) %>%
    {if (soc == FALSE) summarise(.,
                                 AEBODSYS = first(AEBODSYS),
                                 COUNT1 = sum(ARMCD %in% treatment1),
                                 COUNT2 = sum(ARMCD %in% treatment2)) else .} %>%
    {if (soc == TRUE) summarise(.,
                                COUNT1 = sum(ARMCD %in% treatment1),
                                COUNT2 = sum(ARMCD %in% treatment2)) else .}
  
  getRiskDifference <- function(count1, count2) {
    r <- riskdifference(a = count1, b = count2, N1 = N1.total, N0 = N2.total)
    RD <- r$estimate
    RDP <- r$p.value
    
    output <- c(RD, RDP)
    return(output)
  }
  
  result <- data.summary %>%
    {if (soc == FALSE) group_by(., AEDECOD) else .} %>%
    {if (soc == TRUE) group_by(., AEBODSYS) else .} %>%
    {if (soc == FALSE) summarise(.,
                                 AEBODSYS = first(AEBODSYS),
                                 RESULT = paste(getRiskDifference(COUNT1, COUNT2),
                                                collapse = " ")) else .} %>%
    {if (soc == TRUE) summarise(.,
                                RESULT = paste(getRiskDifference(COUNT1, COUNT2),
                                               collapse = " ")) else .} %>%
    rowwise() %>%
    mutate(TEST = as.numeric(unlist(strsplit(RESULT, " "))[1]),
           TESTP = as.numeric(unlist(strsplit(RESULT, " "))[2])) %>%
    select(-RESULT) %>%
    mutate(LOGPTEST = -log(TESTP))
  
  return(result)
}


### Risk Ratio (Large Sample Approximation by CLT) -----------------------------
GetStatistics_RiskR <- function(data, ser, drug, period, residual, soc,
                                treatment1, treatment2) {
  
  if (is.null(treatment1) | is.null(treatment2)) {return(NULL)}
  
  data <- data %>%
    mutate_at(names(data)[!(names(data) %in% c("AESEQ", "AESTDY", "AEENDY", "AGE", "RFENDY", "SAFFN"))], ~as.character(.)) %>%
    mutate_at(names(data), ~na_if(., "")) %>%
    mutate_at(c("AESTDT", "AEENDT", "RFSTDTC", "RFENDTC"), ~as.Date(.)) %>%
    drop_na(RFSTDTC) %>%
    mutate(AEDECOD = ifelse(!is.na(AESTDT) & is.na(AEDECOD), "Not yet coded", AEDECOD)) %>%
    mutate(AESTDT = ifelse(is.na(AESTDT) & !is.na(AEDECOD), RFSTDTC, AESTDT))
  
  N1.total <- data %>%
    filter(ARMCD %in% treatment1) %>% distinct(USUBJID) %>%
    tally() %>% as.numeric()
  
  N2.total <- data %>%
    filter(ARMCD %in% treatment2) %>% distinct(USUBJID) %>%
    tally() %>% as.numeric()
  
  if (ser == TRUE) {
    data <- data %>% filter(AESER == "Y")
  }
  
  if (drug == TRUE) {
    data <- data %>% filter(AEREL == "Y")
  }
  
  if (period == "Treatment emergent") {
    data <- data %>% filter(TRTEMFL == "Y")
  }
  
  if (period == "AE during entire study") {
    data <- data %>% filter(STUDYFL == "Y")
  }
  
  if (period == "Other") {
    data <- data %>% filter((AESTDT > RFSTDTC) & (AESTDT < (RFENDTC + residual)))
  }
  
  data <- data %>% filter(ARMCD %in% c(treatment1, treatment2))
  
  if (nrow(data) == 0) {return(NULL)}
  
  data.summary <- data %>%
    {if (soc == FALSE) group_by(., AEDECOD) else .} %>%
    {if (soc == TRUE) group_by(., AEBODSYS) else .} %>%
    distinct(USUBJID, .keep_all = TRUE) %>%
    {if (soc == FALSE) summarise(.,
                                 AEBODSYS = first(AEBODSYS),
                                 COUNT1 = sum(ARMCD %in% treatment1),
                                 COUNT2 = sum(ARMCD %in% treatment2)) else .} %>%
    {if (soc == TRUE) summarise(.,
                                COUNT1 = sum(ARMCD %in% treatment1),
                                COUNT2 = sum(ARMCD %in% treatment2)) else .}
  
  getRiskRatio <- function(count1, count2) {
    r <- riskratio(X = count1, Y = count2, m1 = N1.total, m2 = N2.total)
    RR <- r$estimate
    RRP <- r$p.value
    
    output <- c(RR, RRP)
    return(output)
  }
  
  result <- data.summary %>%
    {if (soc == FALSE) group_by(., AEDECOD) else .} %>%
    {if (soc == TRUE) group_by(., AEBODSYS) else .} %>%
    {if (soc == FALSE) summarise(.,
                                 AEBODSYS = first(AEBODSYS),
                                 RESULT = paste(getRiskRatio(COUNT1, COUNT2),
                                                collapse = " ")) else .} %>%
    {if (soc == TRUE) summarise(.,
                                RESULT = paste(getRiskRatio(COUNT1, COUNT2),
                                               collapse = " ")) else .} %>%
    rowwise() %>%
    mutate(TEST = as.numeric(unlist(strsplit(RESULT, " "))[1]),
           TESTP = as.numeric(unlist(strsplit(RESULT, " "))[2])) %>%
    select(-RESULT) %>%
    mutate(LOGPTEST = -log(TESTP))
  
  return(result)
}


