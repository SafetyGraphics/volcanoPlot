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
                         treatment1_label,
                         treatment2_label,
                         subgroup_var,
                         subgroup_vals,
                         X_ref,
                         Y_ref,
                         X_label,
                         review_by,
                         summary_by,
                         pvalue_option)
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
  assign("treatment1_label", treatment1_label, envir = .GlobalEnv)
  assign("treatment2_label", treatment2_label, envir = .GlobalEnv)
  assign("subgroup_var", subgroup_var, envir = .GlobalEnv)
  assign("subgroup_vals", subgroup_vals, envir = .GlobalEnv)
  assign("X_ref", X_ref, envir = .GlobalEnv)
  assign("Y_ref", Y_ref, envir = .GlobalEnv)
  assign("X_label", X_label, envir = .GlobalEnv)
  assign("review_by", review_by, envir = .GlobalEnv)
  assign("summary_by", summary_by, envir = .GlobalEnv)
  assign("pvalue_option", pvalue_option, envir = .GlobalEnv)
  
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
  breaks <- c(Y_ref, 0, .00001, .0001, .001, .01, .1, 1)
  key <- row.names(statistics_data)
  
  # pt_size <- statistics_data %>%
  #   mutate('N3' = N1+N2) %>%
  #   select('N3')
  pt_size <- statistics_data %>%
    mutate('N3' = N1 + N2) 
  
  if (test %in% c("Hazard Ratio", "Rate Ratio", "Risk Ratio")) {x_L <- 0; x_U <- 2 * X_ref}
  if (test == "Rate Difference") {
    x_L <- min(statistics_data$TEST, na.rm = TRUE) - 1
    x_U <- max(statistics_data$TEST, na.rm = TRUE) + 1
  }
  if (test == "Risk Difference") {x_L <- X_ref - 1; x_U <- X_ref + 1}
  
  p <- ggplot(statistics_data, aes(TEST, pvalue, label = Summary, color = AEBODSYS, key = key)) + 
    # geom_point() + 
    geom_point(size = pt_size$N3) + 
    geom_hline(aes(yintercept = Y_ref), color = 'grey30', linetype = "dotted") +
    geom_vline(aes(xintercept = X_ref), color = 'grey30', linetype = "dotted") +
    theme_bw() +
    xlim(x_L, x_U) +
    xlab(X_label)  +
    scale_y_continuous(trans = reverselog_trans(10), breaks = breaks, labels = fmt_dcimals()) +
    coord_cartesian(ylim = c(0.00001, 1))
  return(list(plot = p, data = data, N1 = N1, N2 = N2))
}  


