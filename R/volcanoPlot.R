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
#' @import crosstalk
#'
#' @export

volcanoPlot <- function(data, ...){
  
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
  
  # create a vector of comparison groups included in the data
  comp_groups <- unique(data$comp_grp)
  
  # separate data into a last of data frame for each comparison group
  data_list <- list()
  for (i in seq_along(comp_groups)) {
    data_list[[i]] <- filter(data, comp_grp == comp_groups[i])
  }
  
  # turn each data frame in the list to a crosstalk SharedData object linked by 'group'
  shared_data <- list()
  for (i in seq_along(comp_groups)) {
    shared_data[[i]] <- SharedData$new(data_list[[i]], 
                                       key = ~strata,
                                       group = 'group')
  }
  
  # turn each of the SharedData data frame into its own volcano plot
  shared_plots <- list()
  for (i in seq_along(shared_data)) {
    shared_plots[[i]] <-
      ggplot(shared_data[[i]], aes(estimate, -log10(pvalue))) +
      geom_point(aes(size = eventN_total, fill = diffexp,
                     # making hover text
                     text = paste0('Group:  ', strata, '\n', 
                                   'Risk Ratio: ', round(estimate, 2), '\n',
                                   'P Value: ', round(pvalue, 2), '\n',
                                   opts$GroupLabels[2], ': ', eventN_ref, '/', eventN_total, '\n',
                                   opts$GroupLabels[1], ': ', eventN_comparison, '/', eventN_total, '\n')),
                 pch = 21, alpha = 0.5) +
      scale_size_continuous(range = c(2, 12)) +
      scale_fill_manual(values = fillcolors) +
      # adding cutoff lines
      geom_hline(yintercept = -log10(opts$pcutoff), color = 'grey30', linetype = "dashed") +
      geom_vline(xintercept = opts$ecutoff, color = 'grey30', linetype = "dashed") +
      # theming and labeling the plot
      theme_classic() +
      theme(legend.position = "none") +
      scale_x_continuous(paste0(opts$GroupLabels[1], ' vs. ', opts$GroupLabels[2]),
                         expand = expansion(mult = c(0.05, 0.05)))
    
    # turn the plot to an interactive plotly
    shared_plots[[i]] <- 
      ggplotly(shared_plots[[i]], tooltip = 'text') %>%
      # add text and arrow for favority at the bottom of the plot
      plotly::layout(
        annotations =
          list(
            x = 0,
            y = 0.02,
            text = paste0("<- Favors ", opts$GroupLabels[2], " (N=", data_list[[i]]$N_ref[1], ")"),
            showarrow = F,
            xref = 'paper',
            yref = 'paper',
            xanchor = 'left',
            yanchor = 'bottom',
            xshift = 0,
            yshift = 0,
            font = list(size = 12, color = "blue")
          )
      ) %>%
      plotly::layout(
        annotations =
          list(
            x = .95,
            y = 0.02,
            text = paste0("Favors ", opts$GroupLabels[1], " (N=", data_list[[i]]$N_comparison[1], ") ->"),
            showarrow = F,
            xref = 'paper',
            yref = 'paper',
            xanchor = 'right',
            yanchor = 'bottom',
            xshift = 0,
            yshift = 0,
            font = list(size = 12, color = "blue")
          )
      )
  }
  
  # return one plot if only one comparison group
  # return multiple synchronized plot if more than one comp group
  # if (length(shared_plots) == 1) {
  #   return(shared_plots[[1]])
  # } else {
  # return(bscols(shared_plots))
  # }
  
  return(shared_plots)
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