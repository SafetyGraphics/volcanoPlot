#' Create a volcano plot
#'
#' @param data A data frame from getStats() 
#' @param plotly wrap output in ggplotly? default=TRUE
#'  
#' @return a volcano plot created with ggplot or plotly
#' 
#' @examples
#' settings<-list(
#'   stratification_col="AEBODSYS",
#'   group_col="ARM",
#'   reference_group="Placebo",
#'   comparison_group="Xanomeline High Dose",
#'   id_col="USUBJID"
#' )
#' stats<-getStats(dfAE=safetyData::adam_adae, dfDemog = safetyData::adam_adsl, settings)
#' volcanoPlot(stats)
#' 
#' @import ggplot2
#' @importFrom plotly ggplotly
#' 
#' @export

volcanoPlot <- function(data, plotly = TRUE){
  
  # change fill color based on pvalue and estimate
  data$diffexp <- 'NO'
  data$diffexp[data$estimate >= 1 & data$pvalue < 0.05] <- 'UP'
  data$diffexp[data$estimate < 1 & data$pvalue < 0.05] <- 'DOWN'
  fillcolors <- c('DOWN' = 'sienna2', 'UP' = 'skyblue2', 'NO' = 'grey')

  p <- ggplot(data, aes(estimate, -log10(pvalue))) +
    geom_point(aes(size = eventN_total, fill = diffexp), pch = 21, alpha = 0.5) +
    scale_size_continuous(range = c(2, 12)) +
    scale_fill_manual(values = fillcolors) +
    geom_hline(yintercept = -log10(0.05), color = 'grey30', linetype = "dashed") +
    geom_vline(xintercept = 1, color = 'grey30', linetype = "dashed") +
    theme_classic() +
    theme(legend.position = "none") +
    scale_x_continuous("Comparison Group vs. Reference Group",
                       expand = expansion(mult = c(0.05, 0.05)))


    if (plotly) {
      return(
        ggplotly(p)%>%
          plotly::layout(annotations =
                           list(x = 0, y = 0.02, text = paste0("<- Favors Refrence Group"," (N=", data$N_ref[1], ")"),
                                showarrow = F, xref = 'paper', yref = 'paper',
                                xanchor = 'left', yanchor = 'bottom', xshift = 0, yshift = 0,
                                font = list(size = 12, color = "blue"))) %>%
          plotly::layout(annotations =
                           list(x = .95, y = 0.02, text = paste0("Favors Comparison Group"," (N=", data$N_comparison[1], ") ->"),
                                showarrow = F, xref = 'paper', yref = 'paper',
                                xanchor = 'right', yanchor = 'bottom', xshift = 0, yshift = 0,
                                font = list(size = 12, color = "blue")))
             )
    } else {
      return(p)
    }
}

#' Create a volcano plot with EnhancedVolcano
#'
#' @param data A data frame from getStats() 
#'  
#' @return a volcano plot created with ggplot
#' 
#' @examples
#' settings<-list(
#'   stratification_col="AEBODSYS",
#'   group_col="ARM",
#'   reference_group="Placebo",
#'   comparison_group="Xanomeline High Dose",
#'   id_col="USUBJID"
#' )
#' stats<-getStats(dfAE=safetyData::adam_adae, dfDemog = safetyData::adam_adsl, settings)
#' volcanoPlot(stats)
#' 
#' @import ggplot2
#' @importFrom EnhancedVolcano EnhancedVolcano
#' 
#' @export

volcanoPlotEnh <- function(data) {
    p <- EnhancedVolcano(
        toptable = data,
        lab = data$strata,
        x = 'estimate',
        y = 'pvalue',
        pointSize = data$eventN_total / 2.5,
        title = NULL,
        subtitle = NULL,
        pCutoff = 0.05,
        FCcutoff = 1,
        labSize = 1,
        xlab = 'Comparison Group vs. Reference Group'
    )
    return(p)
}