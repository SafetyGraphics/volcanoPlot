################################################################################
# volcano_plot.R
# 1 Function: Generation of volcano plot along with specifications.
################################################################################

volcano.plot <- function(filtered_data,
                         statistics_data,
                         calculation.type,
                         comparison_group,
                         reference_group,
                         pvalue_label,
                         X_ref
                         )
{
    summary_by="Patients"
    
    #if (input$summary_by == "Patients") {
    sum_subjects = filtered_data %>% filter(!is.na(RFSTDTC)) %>% group_by(USUBJID) %>% slice(1) %>% ungroup()
    N1.total = length(unique((sum_subjects %>% filter(ARMCD %in% comparison_group))$USUBJID))
    N2.total = length(unique((sum_subjects %>% filter(ARMCD %in% reference_group))$USUBJID))
#}
    
    
  ### Construction of volcano plot ---------------------------------------------
  if (nrow(statistics_data) == 0) {
    p <- ggplot() + 
      annotate("text", x = 4, y = 25, size = 8, label = "No data selected for plot.") + 
      theme_bw() +
      theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
            axis.title = element_blank(),axis.text = element_blank(),axis.ticks = element_blank())      
    return(list(plot = p))
  }

  
      significant_data =  statistics_data  %>% filter(p.adj<=0.05) %>% arrange(desc(p.adj)) %>% slice(1)
      pvalue_adj0.05 = significant_data$p


  p <- ggplot(statistics_data, aes(est.values, p)) + 
    geom_point(aes(size=N, label1=Summary), pch=21, alpha=0.5, fill="skyblue2") + 
    geom_hline(yintercept = 0.05, color = 'grey30', linetype = "dashed") +
    geom_vline(xintercept = ifelse(grepl("Ratio",calculation.type),1,0), color = 'grey30', linetype = "dashed") +
    geom_vline(xintercept = ifelse(grepl("Ratio",calculation.type),1,0)+c(-X_ref,X_ref), color = 'grey30', linetype = "dashed") +
    theme_classic() +
    background_grid(major = "xy", minor = "none", color.major="grey92")+
    theme(legend.position = "none")+
    scale_x_continuous(paste(calculation.type,"Comparison Group vs. Reference Group"),expand = expansion(mult = c(0.05, 0.05)))+
    scale_size_continuous(range = c(2, 12))
  
  if (nrow(significant_data)!=0){ p = p+geom_hline(yintercept = pvalue_adj0.05, color = 'grey30', linetype = "dotted") }
  
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
  
  ggplotly(p, source="volcano_plot") %>%
      plotly::layout(annotations =
                         list(x = 0, y = 0.02, text = paste0("<- Favors Refrence Group"," (N=", N2.total, ")"),
                              showarrow = F, xref = 'paper', yref = 'paper',
                              xanchor = 'left', yanchor = 'bottom', xshift = 0, yshift = 0,
                              font = list(size = 12, color = "blue"))) %>%
      plotly::layout(annotations =
                         list(x = .95, y = 0.02, text = paste0("Favors Comparison Group"," (N=", N1.total, ") ->"),
                              showarrow = F, xref = 'paper', yref = 'paper',
                              xanchor = 'right', yanchor = 'bottom', xshift = 0, yshift = 0,
                              font = list(size = 12, color = "blue")))

}  
