#' Get statistics for AE data
#'
#' @param data A data frame with adverse event data and placeholder rows for participants with no AEs
#' @param mapping mapping
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
#' data<-read.csv('./data/test_data.csv')
#' getStats(dfAE=safetyData::adam_ae, dfDemog = safetyData::adam_dm, mapping)
#' 
#' @import dplyr
#' @import tidyr
#' @importFrom fmsb rateratio
#' 
#' @export

getStats <- function(dfAE, dfDemog, settings, stat="RR") {
    dfDemog <- dfDemog %>% select(settings[["id_col"]], settings[["group_col"]])
    anly <- dfDemog %>% left_join(dfAE) # left join to keep all rows in dm (even if there were no AEs)

    # get # of participants in each group
    N_comparison<-dfDemog %>% 
    filter(.data[[mapping$group_col]]==mapping$comparison_group) %>% 
    pull(.data[[mapping$id_col]]) %>%
    unique() %>%
    length()

    N_ref<-dfDemog %>% 
    filter(.data[[mapping$group_col]]==mapping$reference_group) %>% 
    pull(.data[[mapping$id_col]]) %>%
    unique() %>%
    length()

    aeCounts <- anly %>% 
        filter(.data[[mapping$group_col]] %in% c(mapping$comparison_group, mapping$reference_group))%>%
        group_by(.data[[mapping$stratification_col]],.data[[mapping$group_col]]) %>% 
        summarize(event=n())%>%
        ungroup() %>%
        spread(.data[[mapping$group_col]],event) %>% 
        na.omit() %>%
        rename(
            strata=mapping$stratification_col,
            eventN_comparison=mapping$comparison_group, 
            eventN_ref=mapping$reference_group
        ) %>%
        mutate(
            eventN_total=eventN_comparison+eventN_ref,
            N_comparison=N_comparison,
            N_ref=N_ref,
            N_total = N_comparison + N_ref
        ) %>%
        arrange(-1*eventN_total)

    # calculate stats for each row
    if(stat=="RR"){
        aeCounts <- aeCounts %>% 
            # run this with purr and then pull estimates? 
            # mutate(stats=fmsb::riskratio(
            #     X=.data$eventN_comparison, 
            #     Y=.data$eventN_ref, 
            #     m1=.data$N_comarison, 
            #     m2=.data$N_ref)) %>%
            mutate(pvalue=fmsb::riskratio(
                X=.data$eventN_comparison, 
                Y=.data$eventN_ref, 
                m1=.data$N_comparison, 
                m2=.data$N_ref)$`p.value`
            )%>%
            mutate(estimate=fmsb::riskratio(
                X=.data$eventN_comparison, 
                Y=.data$eventN_ref, 
                m1=.data$N_comparison, 
                m2=.data$N_ref)$estimate
            )%>% 
            mutate(stat="RR")
    }else if(TRUE){
        message("stat not supported yet :( ")
    }

    return(aeCounts)
}   
