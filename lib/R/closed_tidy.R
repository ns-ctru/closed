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
#' Further dummy variables are derived for all of the events that are to
#' be tested.
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
    ## Remove underscores from measure and sub.measure
    df$measure     <- gsub('_', ' ', df$measure)
    df$sub.measure <- gsub('_', ' ', df$sub.measure)
    ## Factor variables
    df$town <- factor(df$town)
    #######################################################################
    ## Derive a seasonal indicator                                       ##
    #######################################################################
    ## print("Debug 5")
    ## print("LSOA")
    ## dim(df) %>% print()
    ## print("TRUST")
    ## dim(df.trust) %>% print()
    df$season <- 1
    df <- within(df,{
                      season[month(yearmonth) == 1  | month(yearmonth) == 2]  <- 1
                      season[month(yearmonth) == 3  | month(yearmonth) == 4]  <- 2
                      season[month(yearmonth) == 5  | month(yearmonth) == 6]  <- 3
                      season[month(yearmonth) == 7  | month(yearmonth) == 8]  <- 4
                      season[month(yearmonth) == 9  | month(yearmonth) == 10] <- 5
                      season[month(yearmonth) == 11 | month(yearmonth) == 12] <- 6
    })
    df$season <- factor(df$season)
    df$season <- relevel(df$season, ref = '1')
    #######################################################################
    ## Add a dummy 'step' for closure                                    ##
    #######################################################################
    df$closure  <- ifelse(df$relative.month  > 24,
                          ##  & df$town %in% c('Bishop Auckland', 'Hartlepool', 'Hemel Hempstead', 'Newark', 'Rochdale'),
                          1, 0)
    #######################################################################
    ## Add dummy for other 'steps'                                       ##
    ##                                                                   ##
    ## See list from e.l.knowles@sheffield.ac.uk at...                   ##
    ##                                                                   ##
    ## https://goo.gl/TlhfCF                                             ##
    ##                                                                   ##
    #######################################################################
    ## ## print("Debug 7")
    df <- mutate(df,
                 nhs111 = ifelse((town == 'Bishop Auckland' & relative.month >= 35) |
                                 (town == 'Southport' & relative.month >= 48) |
                                 ## ToDo - Uncomment once confirmed and revised dates available
                                 (town == 'Rochdale' & relative.month >= 48) |
                                 (town == 'Rotherham' & relative.month >= 48) |
                                 (town == 'Hartlepool' & relative.month >= 45) |
                                 (town == 'Grimsby' & relative.month >= 16),
                                 1, 0),
                 ambulance.divert = ifelse(town == 'Rochdale' & relative.month >= 17, 1, 0),
                 other.centre = ifelse((town == 'Hemel Hempstead' & relative.month >= 20) |
                                       (town == 'Southport' & relative.month >= 3) |
                                       (town == 'Rochdale' & relative.month >= 11), ##  |
                                       ## (town == 'Hartlepool' & relative.month >= 22),
                                       1, 0),
                 misc = ifelse((town == 'Hemel Hempstead' & relative.month >= 38),
                               1, 0)
                 )
    #######################################################################
    ## 2017-02-01 - Add dummy for new step in Hartlepool at month 30     ##
    ##              see email from Jon Nicholl 2017-02-01 @ 11:17 titled ##
    ##              'hartlepool ED query'                                ##
    #######################################################################
    measure <- dplyr::select(df, measure) %>% unique() %>% as.character()
    ## if(measure %in% c('ed attendances', 'unnecessary ed attendances')){
    ##     df <- mutate(df,
    ##                  other.misc = ifelse(town == 'Hartlepool' & relative.month >= 31,
    ##                                      1, 0))
    ## }
    #######################################################################
    ## Clean 'spurious' data points that are >3 x SD from mean           ##
    #######################################################################

    return(df)
}
