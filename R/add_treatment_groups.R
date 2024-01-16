#' Customize [ dm ] mapping for stats calculation.
#'
#' @export

add_treatment_groups <- function(mapping, data, stratification_col) {
    treatment_groups <- data[[ mapping$treatment_col ]] %>%
        unique %>%
        sort

    # reference group
    mapping$reference_group <- ifelse(
        mapping$treatment_values$group1 != '' && !is.null(mapping$treatment_values$group1),
        mapping$treatment_values$group1,
        treatment_groups[1]
    )

    cli::cli_alert_info(
        'Reference group: {mapping$reference_group}'
    )

    # comparison group
    mapping$comparison_group <- ifelse(
        mapping$treatment_values$group2 != '' && !is.null(mapping$treatment_values$group2),
        mapping$treatment_values$group2,
        treatment_groups[
            treatment_groups != mapping$reference_group
        ]
    )

    cli::cli_alert_info(
        'Comparison group: {mapping$comparison_group}'
    )

    mapping$stratification_col <- stratification_col

    return(mapping)
}
