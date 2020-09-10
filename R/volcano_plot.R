################################################################################
# volcano_plot.R
# 1 Function: Generation of volcano plot along with specifications.
################################################################################

volcano.plot <- function(data,
                         statistics_data,
                         ae_filter,
                         period,
                         residual,
                         calculation.type, 
                         treatment1,
                         treatment2, 
                         X_ref,
                         X_label,
                         review_by,
                         pvalue_label
                         )
{

    summary_by="Patients"
    
  

  
  ### Calculation of lowercase n -----------------------------------------------
  TRT_N <- data %>%
    filter(!is.na(AEDECOD)) %>%
    group_by(ARMCD) %>%
    {if (summary_by == "Patients") filter(., !is.na(RFSTDTC)) else .} %>%
    {if (summary_by == "Patients") summarise(., N = length(unique((USUBJID)))) else .} %>%
    {if (summary_by == "Events") summarise(., N = n()) else .} 
    
  N1 <- sum(subset(TRT_N, ARMCD %in% treatment1)$N)
  N2 <- sum(subset(TRT_N, ARMCD %in% treatment2)$N)
  
  
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
    statistics_data <- statistics_data %>% group_by() %>%
      inner_join(data %>% select(ARMCD, AEBODSYS) %>% unique()) %>% 
      inner_join(SOC_N) %>%
      mutate(N1 = (ARMCD %in% treatment1) * N,
             N2 = (ARMCD %in% treatment2) * N,
             pvalue = TESTP,
             adjpvalue = p.adjust(pvalue, method="fdr"),
             Summary = paste0("\nSOC:", AEBODSYS,
                              "\n","n=",ifelse(N1==0,N2,N1),
                              "\n", calculation.type, ":", TEST,
                              "\np-Value:", round(pvalue,4)))
  } else {
    statistics_data <- statistics_data %>%  group_by() %>%
      inner_join(data %>% select(ARMCD, AEBODSYS, AEDECOD) %>% unique())  %>% 
      inner_join(PT_N) %>%
      mutate(N1 = (ARMCD %in% treatment1) * N,
             N2 = (ARMCD %in% treatment2) * N,
             pvalue = TESTP,
             adjpvalue = p.adjust(pvalue, method="fdr"),
             Summary = paste0("\n SOC:", AEBODSYS,
                              "\n PT:", AEDECOD, 
                              "\n n=",ifelse(N1==0,N2,N1),
                              "\n", calculation.type, ":", TEST,
                              "\n p-value:", round(pvalue,4)," (adj. p-value: ", round(adjpvalue,4)))
  }
  
  
  ### Construction of volcano plot ---------------------------------------------
  if (nrow(statistics_data) == 0) {
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

  
  statistics_data = statistics_data %>% filter(TEST!=Inf)
  
  key <- row.names(statistics_data)
    
  
      significant_data =  statistics_data %>% group_by() %>% filter(adjpvalue<=0.05) %>% arrange(desc(adjpvalue)) %>% slice(1)
      pvalue_adj0.05 = significant_data$pvalue

    
  
  p <- ggplot(statistics_data%>% group_by(), aes(TEST, pvalue, label = Summary, key = key)) + 
    geom_point(aes(size=N), pch=21, alpha=0.5, fill="skyblue2") + 
    geom_hline(yintercept = 0.05, color = 'grey30', linetype = "dashed") +
    geom_vline(xintercept = ifelse(grepl("Ratio",calculation.type),1,0), color = 'grey30', linetype = "dashed") +
    geom_vline(xintercept = ifelse(grepl("Ratio",calculation.type),1,0)+c(-X_ref,X_ref), color = 'grey30', linetype = "dashed") +
    theme_classic() +
    background_grid(major = "xy", minor = "none", color.major="grey92")+
    theme(legend.position = "none")+
    scale_x_continuous(X_label,expand = expansion(mult = c(0.05, 0.05)))+
    scale_size_continuous(range = c(2, 15))
  
  if (nrow(significant_data)!=0){
      p = p+    geom_hline(yintercept = pvalue_adj0.05, color = 'grey30', linetype = "dotted")
  }
  
  if (pvalue_label=="-log10"){
   p=p+ scale_y_continuous("-log10(p-value)",
                       trans = reverselog_trans(10), 
                       breaks = as.numeric(paste0("1e-",0:20)), 
                       labels = as.character(0:20), 
                       expand = expansion(mult = c(0.05, 0.05)))
  } else if (pvalue_label=="None"){
    p=p+ scale_y_continuous("P-value",
                            trans = reverselog_trans(10), 
                            breaks = c(0.05, 0, .000000001,.00000001,.0000001,.000001,.00001,.00001, .0001, .001, .01, .1, 1),
                            labels = as.character(c(0.05, 0, .000000001,.00000001,.0000001,.000001,.00001,.00001, .0001, .001, .01, .1, 1)),
                            expand = expansion(mult = c(0.05, 0.05)))
    
  }
  
  return(list(plot = p, data = data, N1 = N1, N2 = N2))
}  


