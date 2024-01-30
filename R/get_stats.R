#' Get Summary AE Statistics
#'
#' Compares reference and comparison groups to calculate group-wise metrics and p-values for use in AE volcano plot.
#'
#' @param dm `data.frame` Subject-level dataset with one record per subject.
#' @param ae `data.frame` Event-level datasets with one record per adverse event.
#' @param settings `list` Named list of settings (see examples below for standard list).
#' @param statistic `character` Statistic to calculate for AE plot. Options are risk ratio ("RR"
#'   or "Risk Ratio"), risk difference ("RD" or "Risk Difference"). Defaults to
#'   "Risk Ratio".
#'
#' @return a data frame of group-wise statistics for use in the volcano plot
#'
#' @examples
#' settings <- list(
#'     stratification_col = "AEBODSYS",
#'     treatment_col = "ARM",
#'     reference_group = "Placebo",
#'     comparison_group = "Xanomeline High Dose",
#'     id_col = "USUBJID"
#' )
#'
#' get_stats(
#'     dm = safetyData::adam_adsl,
#'     ae = safetyData::adam_adae,
#'     settings
#' )
#'
#' @import dplyr
#' @importFrom fmsb riskratio riskdifference
#' @importFrom tidyr pivot_wider
#'
#' @export

get_stats <- function(
    dm,
    ae,
    settings,
    statistic = "Risk Ratio"
) {
    # left join to keep all rows in dm (even if there were no AEs)
    anly <- dm %>%
        dplyr::select(
            settings$id_col,
            settings$treatment_col
        ) %>%
        dplyr::left_join(
            ae,
            settings$id_col
        )

    aeCounts <- list()

    # count n of comparison group
    N_comparison <- dm %>%
        dplyr::filter(.data[[settings$treatment_col]] == settings$comparison_group) %>%
        dplyr::pull(.data[[settings$id_col]]) %>%
        unique() %>%
        length()

    # count n of reference group
    N_ref <- dm %>%
        dplyr::filter(.data[[settings$treatment_col]] == settings$reference_group) %>%
        dplyr::pull(.data[[settings$id_col]]) %>%
        unique() %>%
        length()

    # create table of numbers for doing stats
    aeCounts <- anly %>%
        dplyr::filter(
            .data[[settings$treatment_col]] %in% c(settings$comparison_group, settings$reference_group)
        ) %>%
        dplyr::group_by(
            .data[[settings$stratification_col]],
            .data[[settings$treatment_col]]
        ) %>%
        # summarize(event=n())%>% do we need this too?
        dplyr::summarize(
            event = length(unique(.data[[settings$id_col]]))
        ) %>%
        dplyr::ungroup() %>%
        stats::na.omit() %>%
        tidyr::pivot_wider(
            names_from = .data[[settings$treatment_col]],
            values_from = "event",
            values_fill = 0
        ) %>%
        dplyr::rename(
            strata = settings$stratification_col,
            eventN_comparison = settings$comparison_group,
            eventN_ref = settings$reference_group
        ) %>%
        dplyr::mutate(
            eventN_total = .data$eventN_comparison + .data$eventN_ref,
            N_comparison = N_comparison,
            N_ref = N_ref,
            N_total = .data$N_comparison + .data$N_ref
        ) %>%
        dplyr::arrange(-1 * .data$eventN_total)

    # calculate stats for each row
    if (statistic %in% c("RR", "Risk Ratio")) {
        aeCounts <- aeCounts %>%
            dplyr::rowwise() %>%
            dplyr::mutate(
                result = {
                    sink(tempfile()) # suppress `print` output
                    result <- fmsb::riskratio(
                        X = .data$eventN_comparison,
                        Y = .data$eventN_ref,
                        m1 = .data$N_comparison,
                        m2 = .data$N_ref
                    ) %>% list()
                    sink()
                    result
                },
                pvalue = .data$result$`p.value`,
                estimate = .data$result$estimate,
                ref_grp = settings$reference_group,
                comp_grp = settings$comparison_group
            ) %>%
            dplyr::ungroup() %>%
            dplyr::select(-.data$result) %>%
            dplyr::mutate(statistic = "Risk Ratio")
    } else if (statistic %in% c("RD", "Risk Difference")) {
        aeCounts <- aeCounts %>%
            dplyr::rowwise() %>%
            dplyr::mutate(
                result = {
                    sink(tempfile()) # suppress `print` output
                    result <- fmsb::riskdifference(
                        a = .data$eventN_comparison,
                        b = .data$eventN_ref,
                        N1 = .data$N_comparison,
                        N0 = .data$N_ref
                    ) %>% list()
                    sink()
                    result
                },
                result = fmsb::riskdifference(
                    a = .data$eventN_comparison,
                    b = .data$eventN_ref,
                    N1 = .data$N_comparison,
                    N0 = .data$N_ref
                ) %>% list(),
                pvalue = .data$result$`p.value`,
                estimate = .data$result$estimate,
                ref_grp = settings$reference_group,
                comp_grp = settings$comparison_group
            ) %>%
            dplyr::ungroup() %>%
            dplyr::select(-.data$result) %>%
            dplyr::mutate(statistic = "Risk Difference")
    } else if (TRUE) {
        message("[ statistic ] not supported yet :( ")
    }

    aeCounts <- aeCounts %>%
        dplyr::mutate(
            logp = -log10(.data$pvalue),
            tooltip = paste0(
                "Group:  ", .data$strata, "<br/>",
                "Risk Ratio: ", round(.data$estimate, 2), "<br/>",
                "P Value: ", round(.data$pvalue, 2), "<br/>",
                .data$ref_grp, ": ", .data$eventN_ref, "/", .data$eventN_total, "<br/>",
                .data$comp_grp, ": ", .data$eventN_comparison, "/", .data$eventN_total, "<br/>"
            )
        )

    ## create one table from a list of tables
    return(aeCounts)
}
