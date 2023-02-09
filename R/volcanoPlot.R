#' Create a volcano plot
#' 
#' Creates a paneled volcano plot showing the distribution of Adverse events. Options to highlight selected events and customize options are provided. 
#'
#' @param data A data frame from getStats()
#' @param highlights A list providing a column and values to be highlighted in the chart
#' @param ... Extra options to change the look of the plot. `fillcol =
#'   c('sienna2', 'skyblue2', 'grey')`: fill colors; `pcutoff = 0.05`: p value
#'   cutoff; `ecutoff = 1`: estimate cutoff, `GroupLabels = c('Comparison
#'   Group', 'Reference Group')`: custom group labels.
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
#'
#' @export

volcanoPlot <- function(data, highlights=c(), ...){

  # process options for the plot
  opts <- list(...)
  if(!('fillcol' %in% names(opts))) {
    opts$fillcol = c('sienna2', 'skyblue2', 'grey')}
  if(!('pcutoff' %in% names(opts))) {opts$pcutoff = 0.05}
  if(!('ecutoff' %in% names(opts))) {
    opts$ecutoff <- ifelse(data$stat[1]=="Risk Difference",0,1)
  }

  # change fill color based on pvalue and estimate
  data$diffexp <- 'NO'
  data$diffexp[data$estimate >= opts$ecutoff & data$pvalue < opts$pcutoff] <- 'UP'
  data$diffexp[data$estimate < opts$ecutoff & data$pvalue < opts$pcutoff] <- 'DOWN'
  fillcolors <- c('DOWN' = opts$fillcol[1], 'UP' = opts$fillcol[2], 'NO' = opts$fillcol[3])
  
  data$alpha <- 0.7
  data$alpha[data$strata %in% highlights] <- 1

  p<-ggplot(data, aes(.data$estimate,.data$logp)) +
  geom_point(
    aes(
      size = .data$eventN_total, 
      fill = .data$diffexp,
      alpha = alpha
    ),
  pch = 21) +
  scale_size_continuous(range = c(2, 12)) +
  scale_fill_manual(values = fillcolors) +
  
  # adding cutoff lines
  geom_hline(yintercept = -log10(opts$pcutoff), color = 'grey30', linetype = "dashed") +
  geom_vline(xintercept = opts$ecutoff, color = 'grey30', linetype = "dashed") +
  
  # theming and labeling the plot
  theme_bw() + 
  theme(legend.position = "none") +
  scale_x_continuous(
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  theme(aspect.ratio = 1) + 

  # Facet on comparison
  facet_wrap(vars(.data$comp_grp))

  return(p)
} 
