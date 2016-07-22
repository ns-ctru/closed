#' Tidy ClosED data sets
#'
#' @description Prepare ClosED data sets for analysis
#'
#' @details The ClosED study uses time-series data with dummy indicators to
#' test for the impact of closing Emergency Departments on indicators of
#' performance.  Data come from various sources (HSCIC, ONS and Ambulance
#' Services), and is prepared (summarised by month) by Tony Stone.
#'
#' This file tidys up the provided data which are prepared as data.tables
#' (by virtue of using data.table package) converting to data frames to
#' work with dplyr (data table support wasn't available when the work
#' started).  Variable names within data frames are converted from having
#' '\code{_}' (underscore) to '\code{.}' (period) for the authors preferred style, and
#' where there is only one-submeasure for a given outcome the \code{sub.measure}
#' variable is filled in with a value the same as the \code{measure}.
#'
#'
#' @param df Data frame to be plotted.
#'
#' @return A list of ggplot2 objects.
#'
#' @examples
#'
#'
#' @references
#'
#' @export
closed_tidy <- function(df        = ed_attendances_by_mode_site_measure,
                        ...){
    ## Convert to Data frame
    df <- as.data.frame(df)
    ## Convert variable names
    names(df) <- gsub('_', '.', names(df))
    ## For select dataframes replace sub.measure
    ## ToDo - Not currently working correctly, but as there are only
    ##        a handful of instances where sub.measure is missing
    ##        for now just run on everything.
    ## data.is <- substitute(df) %>% deparse()
    ## if(data.is %in% c('unnecessary_ed_attendances_measure',
    ##                   'unnecessary_ed_attendances_site_measure',
    ##                   'emergency_admissions_measure',
    ##                   'emergency_admissions_site_measure')){
    ##     df <- ifelse(is.na(df$sub.measure), 'all', df$sub.measure)
    ## }
    df$sub.measure <- ifelse(is.na(df$sub.measure), 'all', df$sub.measure)
    ## If Ambulance times then convert time to minutes
    ## ToDo - Not working either, correct at some point
    ## if(data.is %in% c('amb_mean_time_measure',
    ##                   'amb_mean_time_site_measure')){
    ##     df$value <- (df$value / 60) %>% as.numeric()
    ## }
    ## Factor variables
    df$town <- factor(df$town)
    return(df)
}
