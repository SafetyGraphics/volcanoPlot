################################################################################
# volcano_plot.R
# 1 Function: Generation of volcano plot along with specifications.
################################################################################

volcano.plot <- function(data,
                         statistics_data,
                         statistics_soc_data,
                         ser,
                         drug,
                         period,
                         residual,
                         test, 
                         Treatment1,
                         Treatment2, 
                         subgroup_var,
                         subgroup_vals,
                         X_ref,
                         Y_ref,
                         X_label,
                         review_by,
                         summary_by,
                         pvalue_adjustment,
                         pvalue_label)
{
  assign("data", data, envir = .GlobalEnv)
  assign("statistics_data", statistics_data, envir = .GlobalEnv)
  assign("statistics_soc_data", statistics_soc_data, envir = .GlobalEnv)
  assign("ser", ser, envir = .GlobalEnv)
  assign("drug", drug, envir = .GlobalEnv)
  assign("period", period, envir = .GlobalEnv)
  assign("residual", residual, envir = .GlobalEnv)
  assign("test", test, envir = .GlobalEnv)
  assign("Treatment1", Treatment1, envir = .GlobalEnv)
  assign("Treatment2", Treatment2, envir = .GlobalEnv)
  assign("subgroup_var", subgroup_var, envir = .GlobalEnv)
  assign("subgroup_vals", subgroup_vals, envir = .GlobalEnv)
  assign("X_ref", X_ref, envir = .GlobalEnv)
  assign("Y_ref", Y_ref, envir = .GlobalEnv)
  assign("X_label", X_label, envir = .GlobalEnv)
  assign("review_by", review_by, envir = .GlobalEnv)
  assign("summary_by", summary_by, envir = .GlobalEnv)
  assign("pvalue_adjustment", pvalue_adjustment, envir = .GlobalEnv)
  assign("pvalue_label", pvalue_label, envir = .GlobalEnv)
  
  ### Data pre-processing & data imputation ------------------------------------
  data <- data %>%
    mutate_at(names(data)[!(names(data) %in% c("AESEQ", "AESTDY", "AEENDY",
                                               "AGE", "RFENDY", "SAFFN"))],
              ~as.character(.)) %>%
    mutate_at(names(data), ~na_if(., "")) %>%
    mutate(AEDECOD = ifelse(!is.na(AESTDT) & is.na(AEDECOD), "Not yet coded", AEDECOD)) %>%
    mutate(AESTDT = ifelse(is.na(AESTDT) & !is.na(AEDECOD), RFSTDTC, AESTDT))
  
  
  ### Filtering based on selected treatment groups -----------------------------
  data <- data %>%
    filter(ARMCD %in% c(Treatment1, Treatment2))
  
  
  ### Filtering based on subgroup variable -------------------------------------
  if (subgroup_var != "No Subgroup Variable") {
    data <- filter(data, length(subgroup_vals) == 0 | get(subgroup_var) %in% subgroup_vals) 
  }
  
  
  ### Filtering based on other options from control panel ----------------------
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
    data <- data %>% filter((as.Date(AESTDT) > as.Date(RFSTDTC)) & (as.Date(AESTDT) < (as.Date(RFENDTC) + residual)))
  }
  
  
  ### Calculation of lowercase n -----------------------------------------------
  TRT_N <- data %>%
    filter(!is.na(AEDECOD)) %>%
    group_by(ARMCD) %>%
    {if (summary_by == "Patients") filter(., !is.na(RFSTDTC)) else .} %>%
    {if (summary_by == "Patients") summarise(., N = length(unique((USUBJID)))) else .} %>%
    {if (summary_by == "Events") summarise(., N = n()) else .} 
    
  N1 <- sum(subset(TRT_N, ARMCD %in% Treatment1)$N)
  N2 <- sum(subset(TRT_N, ARMCD %in% Treatment2)$N)
  
  
  ### Data preparation for the next step of data integration -------------------
  PT_N <- data %>%
    filter(!is.na(AEDECOD) & !is.na(RFSTDTC)) %>%
    group_by(AEDECOD, ARMCD) %>%
    {if (summary_by == "Patients") summarise(., N = length(unique((USUBJID)))) else .} %>%
    {if (summary_by == "Events") summarise(., N = n()) else .}
    
  SOC_N <- data %>%
    filter(!is.na(AEDECOD) & !is.na(RFSTDTC)) %>%
    group_by(AEBODSYS, ARMCD) %>%
    {if (summary_by == "Patients") summarise(., N = length(unique((USUBJID)))) else .} %>%
    {if (summary_by == "Events") summarise(., N = n()) else .}
  
  
  ### Integration of data for the purpose of graph & listing presentation ------
  if (review_by == "SOC") {
    statistics_data <- statistics_soc_data %>%  
      inner_join(data %>% select(ARMCD, AEBODSYS) %>% unique()) %>% 
      inner_join(SOC_N) %>%
      mutate(N1 = (ARMCD %in% Treatment1) * N,
             N2 = (ARMCD %in% Treatment2) * N,
             pvalue = TESTP,
             Summary = paste0("\nSOC:", AEBODSYS,
                              "\n<", N1, ",", N2,">",
                              "\n", test, ":", TEST,
                              "\np-Value:", pvalue))
  } else {
    statistics_data <- statistics_data %>%  
      inner_join(data %>% select(ARMCD, AEBODSYS, AEDECOD) %>% unique())  %>% 
      inner_join(PT_N) %>%
      mutate(N1 = (ARMCD %in% Treatment1) * N,
             N2 = (ARMCD %in% Treatment2) * N,
             pvalue = TESTP,
             Summary = paste0("\nSOC:", AEBODSYS,
                              "\nPT:", AEDECOD, " <", N1, ",", N2, ">",
                              "\n", test, ":", TEST,
                              "\np-Value:", pvalue))
  }
  
  
  ### Construction of volcano plot ---------------------------------------------
  if (dim(statistics_data)[1] == 0) {
    p <- ggplot() + 
      annotate("text", x = 4, y = 25, size = 8, label = "No data selected for plot.") + 
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank())      
    return(list(plot = p, data = data))
  }

  key <- row.names(statistics_data)
  
  # adjusted p value
  if (pvalue_adjustment=="Unadjusted"){
    statistics_data$my_pvalue = statistics_data$pvalue
  } else{
    statistics_data$my_pvalue = p.adjust(statistics_data$pvalue, method="fdr")
  }
  
  p <- ggplot(statistics_data, aes(TEST, my_pvalue, label = Summary, fill = AEBODSYS, key = key)) + 
    geom_point(aes(size=N), pch=21, alpha=0.65) + 
    geom_hline(yintercept = Y_ref, color = 'grey30', linetype = "dotted") +
    geom_vline(xintercept = ifelse(grepl("Ratio",test),1,0), color = 'grey30', linetype = "dotted") +
    geom_vline(xintercept = ifelse(grepl("Ratio",test),1,0)+c(-X_ref,X_ref), color = 'grey30', linetype = "dotted") +
    theme_bw() +
    scale_x_continuous(X_label,expand = expansion(mult = c(0.05, 0.05)))+
    scale_size_continuous(range = c(2, 15))
  
  if (pvalue_label=="-log10"){
   p=p+ scale_y_continuous(ifelse(pvalue_adjustment=="Unadjusted","-log10(p-value)","-log10(adj. p-value)"),
                       trans = reverselog_trans(10), 
                       breaks = as.numeric(paste0("1e-",0:20)), 
                       labels = as.character(0:20), 
                       expand = expansion(mult = c(0.05, 0.05)))
  } else if (pvalue_label=="None"){
    p=p+ scale_y_continuous(ifelse(pvalue_adjustment=="Unadjusted","P-value","Adj. p-value"),
                            trans = reverselog_trans(10), 
                            breaks = c(Y_ref, 0, .000000001,.00000001,.0000001,.000001,.00001,.00001, .0001, .001, .01, .1, 1),
                            labels = as.character(c(Y_ref, 0, .000000001,.00000001,.0000001,.000001,.00001,.00001, .0001, .001, .01, .1, 1)),
                            expand = expansion(mult = c(0.05, 0.05)))
    
  }
  
  return(list(plot = p, data = data, N1 = N1, N2 = N2))
}  


