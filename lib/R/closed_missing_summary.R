#' Summarise missing data across data sets
#'
#' @description Summarise missing data across all data sets.
#'
#' @details Summarise missing data at the LSOA and Site level
#'
#' @param df.lsoa List of LSOA level data frames to summarise
#' @param df.site List of Site level data frames to summarise
#'
#' @return A data frame.
#'
#' @examples
#'
#'
#' ts.mortality <- closed_missing(df       = ed_attendances_by_mode_measure)
#'
#'
#' @references
#'
#' @export
closed_missing_summary<- function(df.lsoa  = ed_attendances_by_mode_measure,
                                  df.site  = ed_attendances_by_mode_site_measure,
                                  ...){
    ## List for results
    results <- list()
    ## Helper functions
    results$check.lsoa <- df.lsoa %>%
                          data.frame() %>%
                          filter(is.na(value)) %>%
                          dplyr::select_('group', 'town', 'lsoa', 'relative_month', 'measure', 'sub_measure') %>%
                          unique() %>%
                          group_by_('group', 'town', 'relative_month', 'measure', 'sub_measure') %>%
                          summarise(n = n())
    results$check.site <- df.site %>%
                          data.frame() %>%
                          filter(is.na(value)) %>%
                          dplyr::select_('town', 'relative_month', 'measure', 'sub_measure') %>%
                          unique()
    ## Return results
    return(results)
}
