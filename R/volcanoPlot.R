#' Create a volcano plot
#'
#' @param data A data frame from getStats()
#' @param plotly wrap output in ggplotly? default=TRUE
#' @param ... Extra options to change the look of the plot. `fillcol =
#'   c('sienna2', 'skyblue2', 'grey')`: fill colors; `pcutoff = 0.05`: p value
#'   cutoff; `ecutoff = 1`: estimate cutoff, `GroupLabels = c('Comparison
#'   Group', 'Reference Group')`: custom group labels.
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

volcanoPlot <- function(data, plotly = TRUE, ...){
  
  # process options for the plot
  opts <- list(...)
  if(!('fillcol' %in% names(opts))) {
    opts$fillcol = c('sienna2', 'skyblue2', 'grey')}
  if(!('pcutoff' %in% names(opts))) {opts$pcutoff = 0.05}
  if(!('ecutoff' %in% names(opts))) {
    opts$ecutoff <- ifelse(data$stat[1]=="Risk Difference",0,1)
  }
  if(!('GroupLabels' %in% names(opts))) {
    opts$GroupLabels <- c('Comparison Group', 'Reference Group')
  }
  
  # change fill color based on pvalue and estimate
  data$diffexp <- 'NO'
  data$diffexp[data$estimate >= opts$ecutoff & data$pvalue < opts$pcutoff] <- 'UP'
  data$diffexp[data$estimate < opts$ecutoff & data$pvalue < opts$pcutoff] <- 'DOWN'
  # fillcolors <- c('DOWN' = 'sienna2', 'UP' = 'skyblue2', 'NO' = 'grey')
  fillcolors <- c('DOWN' = opts$fillcol[1], 'UP' = opts$fillcol[2], 'NO' = opts$fillcol[3])

  p <- ggplot(data, aes(estimate, -log10(pvalue))) +
    geom_point(aes(size = eventN_total, fill = diffexp, 
                   text = paste0('Group:  ', strata, '\n',
                                 'Risk Ratio: ', round(estimate, 2), '\n',
                                 'P Value: ', round(pvalue, 2), '\n',
                                 opts$GroupLabels[2], ': ', eventN_ref, '/', eventN_total, '\n',
                                 opts$GroupLabels[1], ': ', eventN_comparison, '/', eventN_total, '\n')), 
               pch = 21, alpha = 0.5) +
    scale_size_continuous(range = c(2, 12)) +
    scale_fill_manual(values = fillcolors) +
    geom_hline(yintercept = -log10(opts$pcutoff), color = 'grey30', linetype = "dashed") +
    geom_vline(xintercept = opts$ecutoff, color = 'grey30', linetype = "dashed") +
    theme_classic() +
    theme(legend.position = "none") +
    scale_x_continuous(paste0(opts$GroupLabels[1], ' vs. ', opts$GroupLabels[2]),
                       expand = expansion(mult = c(0.05, 0.05)))


    if (plotly) {
      return(
        ggplotly(p, tooltip = 'text')%>%
          plotly::layout(annotations =
                           list(x = 0, y = 0.02, text = paste0("<- Favors ", opts$GroupLabels[2]," (N=", data$N_ref[1], ")"),
                                showarrow = F, xref = 'paper', yref = 'paper',
                                xanchor = 'left', yanchor = 'bottom', xshift = 0, yshift = 0,
                                font = list(size = 12, color = "blue"))) %>%
          plotly::layout(annotations =
                           list(x = .95, y = 0.02, text = paste0("Favors ", opts$GroupLabels[1], " (N=", data$N_comparison[1], ") ->"),
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