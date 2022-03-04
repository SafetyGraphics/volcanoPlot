#' Get statistics for AE data
#'
#' @param data A data frame with adverse event data and placeholder rows for participants with no AEs
#' @param mapping mapping
#'  
#' @return a data frame for use in the volcano plot
#'  
#' @examples
#' mapping<-list(
#'   stratification_col="AEBODSYS",
#'   group_col="ARM", 
#'   reference_group="Placebo",
#'   comparison_group="Xanomeline High Dose",
#'   id_col="USUBJID"
#' )
#' data<-read.csv('./data/test_data.csv')
#' getStats(dfAE=safetyData::adam_ae, dfDemog = safetyData::adam_dm, mapping)
#' 
#' @import dplyr
#' @importFrom epitools rateratio
#' 
#' @export

getStats <- function(dfAE, dfDemog, mapping, stat="RR") {
    dfDemog <- dfDemog %>% select(settings[["id_col"]], settings[["group_col"]])
    anly <- dm_sub %>% left_join(data$aes) # left join to keep all rows in dm (even if there were no AEs)

    # get # of participants in each group
    N_comparison<-anly %>% 
    filter(.data[[mapping$group_col]]==mapping$comparison_group) %>% 
    pull(.data[[mapping$id_col]]) %>%
    unique() %>%
    length()

    N_ref<-anly %>% 
    filter(.data[[mapping$group_col]]==mapping$reference_group) %>% 
    pull(.data[[mapping$id_col]]) %>%
    unique() %>%
    length()

    # calculates number of subjects in each stratification per treatment arm
    eventCounts <- anly %>% 
        group_by(.data[[mapping$stratification_col]],.data[[mapping$group_col]]) %>% 
        summarize(event=n())%>%
        ungroup() %>%
        spread(.data[[mapping$group_col]],event) %>% 
        na.omit() %>%
        rename(eventN_comparison=mapping$comparison_group, eventN_ref=mapping$reference_group) %>%
        mutate(
            eventN_total=eventN_comparison+eventN_ref,
            N_comparison=N_comparison,
            N_ref=N_ref,
            N_total = N_comparison + N_ref
        ) %>%
        arrange(-1*eventN_total)

    # calculate stats for each row
    if(stat="RR"){
        eventCounts$stat = rand()
        eventCounts$pval = rand()
        # eventStats <- eventCounts %>% 
        #     mutate(stats=fmsb::riskratio(
        #         a = eventN_ref, 
        #         b = eventN_comparison, 
        #         PT1 = N_ref, 
        #         PT0 = N_comparison)
        #     )
    }else if(TRUE){
        message("stat not supported yet :( ")
    }

    return(eventCounts)
}   
