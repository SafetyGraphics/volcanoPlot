#' Get statistics for AE data
#'
#' @param settings Named list of settings
#' @param dfAE Adverse events dataset structured as 1 record per adverse event
#'   per subject
#' @param dfDemog Subject-level dataset
#' @param stat Statistic to calculate for AE plot. Options are risk ratio ("RR"
#'   or "Risk Ratio"), risk difference ("RD" or "Risk Difference"). Defaults to
#'   "Risk Ratio".
#'
#' @return a data frame for use in the volcano plot
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
    print('stat settings')
    print(settings)
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
      na.omit %>%
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
        eventN_total = eventN_comparison + eventN_ref,
        N_comparison = N_comparison,
        N_ref = N_ref,
        N_total = N_comparison + N_ref
      ) %>%
      arrange(-1 * eventN_total)
    
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
          pvalue = result$`p.value`,
          estimate = result$estimate,
          ref_grp = settings$reference_group,
          comp_grp = settings$comparison_group
        ) %>%
        ungroup %>%
        select(-result) %>%
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
          pvalue = result$`p.value`,
          estimate = result$estimate,
          ref_grp = settings$reference_group,
          comp_grp = settings$comparison_group
        ) %>%
        ungroup %>%
        select(-result) %>%
        mutate(stat = "Risk Difference")
    } else if (TRUE) {
      message("stat not supported yet :( ")
    }
    
    ## create one table from a list of tables
    return(bind_rows(aeCounts))
}   
