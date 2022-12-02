#' Create a volcano plot
#'
#' @param data A data frame from getStats()
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
#'   comparison_group=c("Xanomeline High Dose","Xanomeline Low Dose"),
#'   id_col="USUBJID"
#' )
#' stats<-getStats(dfAE=safetyData::adam_adae, dfDemog = safetyData::adam_adsl, settings)
#' volcanoPlot(stats)
#'
#' @import ggplot2
#' @importFrom plotly ggplotly
#' @importFrom crosstalk SharedData bscols
#'
#' @export

volcanoPlot <- function(data, ...){
  
  print('plotting')
  print(head(data))
  print(unique(data$comp_grp))

  # process options for the plot
  opts <- list(...)
  if(!('fillcol' %in% names(opts))) {
    opts$fillcol = c('sienna2', 'skyblue2', 'grey')}
  if(!('pcutoff' %in% names(opts))) {opts$pcutoff = 0.05}
  if(!('ecutoff' %in% names(opts))) {
    opts$ecutoff <- ifelse(data$stat[1]=="Risk Difference",0,1)
  }
  # if(!('GroupLabels' %in% names(opts))) {
  #   opts$GroupLabels <- c('Comparison Group', 'Reference Group')
  # }
  
  # change fill color based on pvalue and estimate
  data$diffexp <- 'NO'
  data$diffexp[data$estimate >= opts$ecutoff & data$pvalue < opts$pcutoff] <- 'UP'
  data$diffexp[data$estimate < opts$ecutoff & data$pvalue < opts$pcutoff] <- 'DOWN'
  fillcolors <- c('DOWN' = opts$fillcol[1], 'UP' = opts$fillcol[2], 'NO' = opts$fillcol[3])
  
  p<-ggplot(data, aes(estimate, -log10(pvalue))) +
  geom_point(
    aes(
      size = eventN_total, 
      fill = diffexp#,
      # making hover text
      # text = paste0(
      #   'Group:  ', 
      #   strata, '\n', 
      #   'Risk Ratio: ', round(estimate, 2), '\n',
      #   'P Value: ', round(pvalue, 2), '\n',
      #   ref_grp, ': ', eventN_ref, '/', eventN_total, '\n',
      #   comp_grp, ': ', eventN_comparison, '/', eventN_total, '\n'
      # )
    ),
  pch = 21, 
  alpha = 0.5) +
  scale_size_continuous(range = c(2, 12)) +
  scale_fill_manual(values = fillcolors) +
  # adding cutoff lines
  geom_hline(yintercept = -log10(opts$pcutoff), color = 'grey30', linetype = "dashed") +
  geom_vline(xintercept = opts$ecutoff, color = 'grey30', linetype = "dashed") +
  # theming and labeling the plot
  theme_classic() +
  theme(legend.position = "none") +
  scale_x_continuous(
    #paste0(comp_groups[i], ' vs. ', ref_group),
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  facet_wrap(vars(comp_grp))
    
  return(p)
} 
