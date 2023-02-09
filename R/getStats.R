#' Get Summary AE Statistics
#' 
#' Compares reference and comparison groups to calculate group-wise metrics and p-values for use in AE volcano plot. 
#'
#' @param settings Named list of settings (see examples below for standard list)
#' @param dfAE Adverse events dataset structured as 1 record per adverse event
#'   per subject
#' @param dfDemog Subject-level dataset
#' @param stat Statistic to calculate for AE plot. Options are risk ratio ("RR"
#'   or "Risk Ratio"), risk difference ("RD" or "Risk Difference"). Defaults to
#'   "Risk Ratio".
#'
#' @return a data frame of group-wise statistics for use in the volcano plot
#'   
#' @examples
#' settings<-list(
#'   stratification_col="AEBODSYS",
#'   group_col="ARM",
#'   reference_group="Placebo",
#'   comparison_group="Xanomeline High Dose",
#'   id_col="USUBJID"
#' )
#' getStats(dfAE=safetyData::adam_adae, dfDemog = safetyData::adam_adsl, settings)
#' 
#' @import dplyr
#' @import tidyr
#' @importFrom fmsb riskratio riskdifference
#' 
#' @export

getStats <- function(dfAE, dfDemog, settings, stat="Risk Ratio") {
    ## Prepare data
    dfDemog <- dfDemog %>% select(settings[["id_col"]], settings[["group_col"]])
    anly <- dfDemog %>% left_join(dfAE) # left join to keep all rows in dm (even if there were no AEs) 
    aeCounts <- list()
    


    # count n of comparison group
    N_comparison <- dfDemog %>%
      filter(.data[[settings$group_col]] == settings$comparison_group) %>%
      pull(.data[[settings$id_col]]) %>%
      unique() %>%
      length()
    
    # count n of reference group
    N_ref <- dfDemog %>%
      filter(.data[[settings$group_col]] == settings$reference_group) %>%
      pull(.data[[settings$id_col]]) %>%
      unique() %>%
      length()
    
    # create table of numbers for doing stats
    aeCounts <- anly %>%
      filter(.data[[settings$group_col]] %in% c(settings$comparison_group, settings$reference_group)) %>%
      group_by(.data[[settings$stratification_col]], .data[[settings$group_col]]) %>%
      # summarize(event=n())%>% do we need this too?
      summarize(event = length(unique(.data[[settings$id_col]]))) %>%
      ungroup() %>%
      stats::na.omit() %>%
      pivot_wider(
        names_from = .data[[settings$group_col]],
        values_from = "event",
        values_fill = 0
      ) %>%
      rename(
        strata = settings$stratification_col,
        eventN_comparison = settings$comparison_group,
        eventN_ref = settings$reference_group
      ) %>%
      mutate(
        eventN_total = .data$eventN_comparison + .data$eventN_ref,
        N_comparison = N_comparison,
        N_ref = N_ref,
        N_total = .data$N_comparison + .data$N_ref
      ) %>%
      arrange(-1 * .data$eventN_total)
    
    # calculate stats for each row
    if (stat %in% c("RR", "Risk Ratio")) {
      aeCounts <- aeCounts %>%
        rowwise %>%
        mutate(
          result = fmsb::riskratio(
            X = .data$eventN_comparison,
            Y = .data$eventN_ref,
            m1 = .data$N_comparison,
            m2 = .data$N_ref
          ) %>% list,
          pvalue = .data$result$`p.value`,
          estimate = .data$result$estimate,
          ref_grp = settings$reference_group,
          comp_grp = settings$comparison_group
        ) %>%
        ungroup %>%
        select(-.data$result) %>%
        mutate(stat = "Risk Ratio")
    } else if (stat %in% c("RD", "Risk Difference")) {
      aeCounts <- aeCounts%>%
        rowwise %>%
        mutate(
          result = fmsb::riskdifference(
            a = .data$eventN_comparison,
            b = .data$eventN_ref,
            N1 = .data$N_comparison,
            N0 = .data$N_ref
          ) %>% list,
          pvalue = .data$result$`p.value`,
          estimate = .data$result$estimate,
          ref_grp = settings$reference_group,
          comp_grp = settings$comparison_group
        ) %>%
        ungroup %>%
        select(-.data$result) %>%
        mutate(stat = "Risk Difference")
    } else if (TRUE) {
      message("stat not supported yet :( ")
    }
    aeCounts<-aeCounts %>% 
      mutate(logp = -log10(.data$pvalue)) %>%
      mutate(
        tooltip=paste0(
          'Group:  ', .data$strata, '<br/>', 
          'Risk Ratio: ', round(.data$estimate, 2), '<br/>',
          'P Value: ', round(.data$pvalue, 2), '<br/>',
          .data$ref_grp, ': ', .data$eventN_ref, '/', .data$eventN_total, '<br/>',
          .data$comp_grp, ': ', .data$eventN_comparison, '/', .data$eventN_total, '<br/>'
        )
      )
    ## create one table from a list of tables
    return(aeCounts)
}   
