#' Tidy ClosED data frames to remove spurious data points
#'
#' @description Remove spurious data points from a given data frame for analysis.
#'
#' @details The ClosED study uses time-series with dummy indicators to
#' test for the impact of closing Emergency Departments on indicators of
#' performance.  The data is derived from various sources including the Health
#' and Social Care Information Commission (HSCIC), Office for National Statistics
#' (ONS) and various Ambulance Services (AS).  Unfortuantely it is of variable
#' quality and there are a number of instances where, after summarising the data
#' by month and plotting as a time-series, it is clear that there are problems
#' with the underlying data in terms of accuracy.  This function therefore cleans
#' the data set prior to analyses (or plotting) by identifying data points that
#' are more than a function of the standard deviation away from the mean across
#' time points.
#'
#' Currently spurious data points to be cleaned are internally coded.
#'
#' **NB** This function works with only one data frame at a time, there are two for
#'        each outcome (Trust and LSOA level data).  Both datasets require cleaning
#'        so remember to clean both.
#'
#' @param df Data frame to analyse clean.
#' @param indicator The performance indicator to assess.
#' @param systematic Systematic removal of spurious data points, as a multiple of the Standard Deviation within a ED site.
#'
#' @return A cleaned data frame with spurious data points removed.
#'
#'
#' @export
closed_clean_ts <- function(df              = ed_attendances_by_mode_measure,
                                 indicator       = 'ed attendances',
                                 systematic      = NA,
                                 ...){
    ## Systematic removal, default is x3 SD from the mean
    if(!is.na(systematic)){
        df <- group_by(df, town, sub.measure) %>%
              mutate(mean      = mean(value, na.rm = TRUE),
                     sd        = sd(value, na.rm = TRUE))
        ## dplyr::filter(df, town == 'Bishop Auckland' & indicator == 'any sec') %>%
        ##     dplyr::select(relative.month, value) %>% print()
        ## print(systematic)
        df <- mutate(df,
                     value = ifelse(value < (mean - systematic * sd) |
                                    value > (mean + systematic * sd),
                                    yes = NA,
                                    no  = value))
        df <- dplyr::select(df, -c(mean, sd))
    }
    ## Remove 'spurious' data points (see Results > Summary > Spurious Data)
    ## Clean the data set conditional on the Indicator and in turn Sub-indicator
    if(indicator ==  'ed attendances'){
        df <- mutate(df,
                     value = ifelse(sub.measure == 'any' &
                                    town %in% c('Bishop Auckland') &
                                    relative.month %in% c(1, 6),
                                    yes = NA,
                                    no  = value),
                     value = ifelse(sub.measure == 'any' &
                                    town %in% c('Newark', 'Southport') &
                                    relative.month %in% c(11, 23),
                                    yes = NA,
                                    no  = value)
                     )
        df <- mutate(df,
                     value = ifelse(sub.measure == 'ambulance' &
                                    town %in% c('Bishop Auckland') &
                                    relative.month %in% c(1, 6),
                                    yes = NA,
                                    no = value),
                     value = ifelse(sub.measure == 'ambulance' &
                                    town %in% c('Newark', 'Southport') &
                                    relative.month %in% c(11, 23),
                                    yes = NA,
                                    no  = value))
        df <- mutate(df,
                     value = ifelse(sub.measure == 'other' &
                                    town %in% c('Whitehaven') &
                                    relative.month %in% c(1, 6),
                                    yes = NA,
                                    no = value),
                     value = ifelse(sub.measure == 'other' &
                                    town %in% c('Newark', 'Southport') &
                                    relative.month %in% c(11, 23),
                                    yes = NA,
                                    no = value))
    }
    else if(indicator ==  'unnecessary ed attendances'){
        df <- mutate(df,
                     value = ifelse(sub.measure == 'all' &
                                    town %in% c('Bishop Auckland') &
                                    relative.month %in% c(1, 6),
                                    yes = NA,
                                    no = value),
                     ## value = ifelse(sub.measure == 'all' &
                     ##                town %in% c('Hartlepool', 'Grimsby') &
                     ##                relative.month >= 31,
                     ##                yes = NA,
                     ##                no = value),
                     ## value = ifelse(sub.measure == 'all' &
                     ##                town %in% c('Hemel Hempstead', 'Warwick') &
                     ##                relative.month %in% c(1, 6),
                     ##                yes = NA,
                     ##                no = value),
                     value = ifelse(sub.measure == 'all' &
                                    town %in% c('Newark', 'Southport') &
                                    relative.month %in% c(11, 23),
                                    yes = NA,
                                    no = value))
    }
    else if(indicator ==  'all emergency admissions'){
    ##     df <- mutate(df,
    ##                  value = ifelse(sub.measure == 'all emergency admissions' &
    ##                                 town %in% c('') &
    ##                                 relative.month %in% c(),
    ##                                 yes = NA,
    ##                                 no = value))
    }
    else if(indicator ==  'avoidable emergency admissions'){
    ##     df <- mutate(df,
    ##                  value = ifelse(sub.measure == 'any' &
    ##                                 town %in% c('') &
    ##                                 relative.month %in% c(),
    ##                                 yes = NA,
    ##                                 no = value))
    ##     df <- mutate(df,
    ##                  value = ifelse(sub.measure == 'non-specific chest pain' &
    ##                                 town %in% c('') &
    ##                                 relative.month %in% c(),
    ##                                 yes = NA,
    ##                                 no = value))
    }
    else if(indicator ==  'ed attendances admitted'){
    ##     df <- mutate(df,
    ##                  value = ifelse(sub.measure == 'all' &
    ##                                 town %in% c('') &
    ##                                 relative.month %in% c(),
    ##                                 yes = NA,
    ##                                 no = value))
    ##     df <- mutate(df,
    ##                  value = ifelse(sub.measure == 'fraction admitted' &
    ##                                 town %in% c('') &
    ##                                 relative.month %in% c(),
    ##                                 yes = NA,
    ##                                 no = value))
    ##     df <- mutate(df,
    ##                  value = ifelse(sub.measure == 'admitted' &
    ##                                 town %in% c('') &
    ##                                 relative.month %in% c(),
    ##                                 yes = NA,
    ##                                 no = value))
    }
    else if(indicator ==  'critical care stays'){
    ##     df <- mutate(df,
    ##                  value = ifelse(sub.measure == 'all' &
    ##                                 town %in% c('') &
    ##                                 relative.month %in% c(),
    ##                                 yes = NA,
    ##                                 no = value))
    ##     df <- mutate(df,
    ##                  value = ifelse(sub.measure == 'critical care' &
    ##                                 town %in% c('') &
    ##                                 relative.month %in% c(),
    ##                                 yes = NA,
    ##                                 no = value))
    ##     df <- mutate(df,
    ##                  value = ifelse(sub.measure == 'fraction critical care' &
    ##                                 town %in% c('') &
    ##                                 relative.month %in% c(),
    ##                                 yes = NA,
    ##                                 no = value))
    }
    else if(indicator ==  'length of stay'){
    ##     df <- mutate(df,
    ##                  value = ifelse(sub.measure == 'mean' &
    ##                                 town %in% c('') &
    ##                                 relative.month %in% c(),
    ##                                 yes = NA,
    ##                                 no = value))
    ##     df <- mutate(df,
    ##                  value = ifelse(sub.measure == 'median' &
    ##                                 town %in% c('') &
    ##                                 relative.month %in% c(),
    ##                                 yes = NA,
    ##                                 no = value))
    }
    else if(indicator ==  'sec case fatality 7 days'){
    ##     df <- mutate(df,
    ##                  value = ifelse(sub.measure == 'any sec' &
    ##                                 town %in% c('') &
    ##                                 relative.month %in% c(),
    ##                                 yes = NA,
    ##                                 no = value))
    ##     df <- mutate(df,
    ##                  value = ifelse(sub.measure == 'myocardial infarction' &
    ##                                 town %in% c('') &
    ##                                 relative.month %in% c(),
    ##                                 yes = NA,
    ##                                 no = value))
    ##     df <- mutate(df,
    ##                  value = ifelse(sub.measure == 'any trauma sec' &
    ##                                 town %in% c('') &
    ##                                 relative.month %in% c(),
    ##                                 yes = NA,
    ##                                 no = value))
    ##     df <- mutate(df,
    ##                  value = ifelse(sub.measure == 'stroke cva' &
    ##                                 town %in% c('') &
    ##                                 relative.month %in% c(),
    ##                                 yes = NA,
    ##                                 no = value))
    }
    else if(indicator ==  'ambulance mean times'){
        df <- mutate(df,
                     value = ifelse(sub.measure == 'call to dest' &
                                    town %in% c('Hemel Hempstead') &
                                    relative.month %in% c(26, 27, 28, 38, 39, 40, 41, 42, 43, 44, 45, 46),
                                    yes = NA,
                                    no = value),
                     value = ifelse(sub.measure == 'call to dest' &
                                    town %in% c('Rochdale', 'Rotherham') &
                                    relative.month %in% c(3, 4, 15),
                                    yes = NA,
                                    no = value))
        df <- mutate(df,
                     value = ifelse(sub.measure == 'call to scene any' &
                                    town %in% c('Hemel Hempstead', 'Warwick') &
                                    relative.month %in% c(26, 27, 28, 38, 39, 40, 41, 42, 43, 44, 45, 46),
                                    yes = NA,
                                    no = value),
                     value = ifelse(sub.measure == 'call to scene any' &
                                    town %in% c('Rochdale', 'Rotherham') &
                                    relative.month %in% c(3, 8, 9, 15),
                                    yes = NA,
                                    no = value))
        df <- mutate(df,
                     value = ifelse(sub.measure == 'call to scene conveying' &
                                    town %in% c('Hemel Hempstead', 'Warwick') &
                                    relative.month %in% c(26, 27, 28, 38, 39, 40, 41, 42, 43, 44, 45, 46),
                                    yes = NA,
                                    no = value))
        df <- mutate(df,
                     value = ifelse(sub.measure == 'scene to dest' &
                                    town %in% c('Bishop Auckland', 'Whitehaven') &
                                    relative.month %in% c(5),
                                    yes = NA,
                                    no = value))
        ## df <- mutate(df,
        ##              value = ifelse(sub.measure == 'dest to clear' &
        ##                             town %in% c('') &
        ##                             relative.month %in% c(),
        ##                             yes = NA,
        ##                             no = value))
    }
    else if(indicator ==  'ambulance green calls'){
        df <- mutate(df,
                     value = ifelse(sub.measure == 'green calls' &
                                    town %in% c('Grimsby') &
                                    relative.month %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                                    yes = NA,
                                    no = value),
                     value = ifelse(sub.measure == 'green calls' &
                                    town %in% c('Warwick') &
                                    relative.month %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25),
                                    yes = NA,
                                    no = value),
                     value = ifelse(sub.measure == 'green calls' &
                                    town %in% c('Newark') &
                                    relative.month %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                                    yes = NA,
                                    no = value),
                     value = ifelse(sub.measure == 'green calls' &
                                    town %in% c('Rotherham') &
                                    relative.month %in% c(13, 14, 15, 48),
                                    yes = NA,
                                    no = value))
        df <- mutate(df,
                     value = ifelse(sub.measure == 'not conveyed green calls' &
                                    town %in% c('Grimsby') &
                                    relative.month %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                                    yes = NA,
                                    no = value),
                     value = ifelse(sub.measure == 'not conveyed green calls' &
                                    town %in% c('Warwick') &
                                    relative.month %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25),
                                    yes = NA,
                                    no = value),
                     value = ifelse(sub.measure == 'green calls' &
                                    town %in% c('Newark') &
                                    relative.month %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                                    yes = NA,
                                    no = value),
                     value = ifelse(sub.measure == 'not conveyed green calls' &
                                    town %in% c('Rotherham') &
                                    relative.month %in% c(13, 14, 15, 48),
                                    yes = NA,
                                    no = value))
        df <- mutate(df,
                     value = ifelse(sub.measure == 'fraction not conveyed' &
                                    town %in% c('Grimsby') &
                                    relative.month %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                                    yes = NA,
                                    no = value),
                     value = ifelse(sub.measure == 'fraction not conveyed' &
                                    town %in% c('Newark') &
                                    relative.month %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25),
                                    yes = NA,
                                    no = value),
                     value = ifelse(sub.measure == 'fraction not conveyed' &
                                    town %in% c('Southport') &
                                    relative.month %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                                    yes = NA,
                                    no = value))
                     ## value = ifelse(sub.measure == 'fraction not conveyed' &
                     ##                town %in% c('Rochdale', 'Rotherham') &
                     ##                relative.month %in% c(13, 14, 15, 48),
                     ##                yes = NA,
                     ##                no = value))
    }
    else if(indicator ==  'ambulance red calls'){
        df <- mutate(df,
                     value = ifelse(sub.measure == 'hospital transfers' &
                                    town %in% c('Whitehaven') &
                                    relative.month %in% c(1, 2, 3, 4, 5, 6),
                                    yes = NA,
                                    no = value),
                     value = ifelse(sub.measure == 'hospital transfers' &
                                    town %in% c('Grimsby') &
                                    relative.month %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                                    yes = NA,
                                    no = value),
                     value = ifelse(sub.measure == 'hospital transfers' &
                                    town %in% c('Hemel Hempstead') &
                                    relative.month %in% c(1, 26, 27, 38, 39, 40, 41, 42, 43, 44, 45),
                                    yes = NA,
                                    no = value),
                     value = ifelse(sub.measure == 'hospital transfers' &
                                    town %in% c('Newark') &
                                    relative.month %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                                    yes = NA,
                                    no = value),
                     value = ifelse(sub.measure == 'hospital transfers' &
                                    town %in% c('Rotherham'),
                                    yes = NA,
                                    no = value))
        df <- mutate(df,
                     value = ifelse(sub.measure == 'total' &
                                    town %in% c('Grimsby') &
                                    relative.month %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                                    yes = NA,
                                    no = value),
                     value = ifelse(sub.measure == 'hospital transfers' &
                                    town %in% c('Hemel Hempstead') &
                                    relative.month %in% c(26, 27, 38, 39, 40, 41, 42, 43, 44, 45),
                                    yes = NA,
                                    no = value),
                     value = ifelse(sub.measure == 'total' &
                                    town %in% c('Warwick') &
                                    relative.month %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25),
                                    yes = NA,
                                    no = value),
                     value = ifelse(sub.measure == 'total' &
                                    town %in% c('Newark') &
                                    relative.month %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                                    yes = NA,
                                    no = value),
                     value = ifelse(sub.measure == 'total' &
                                    town %in% c('Rotherham') &
                                    relative.month %in% c(13, 14, 15),
                                    yes = NA,
                                    no = value))
        ## df <- mutate(df,
        ##              value = ifelse(sub.measure == 'total' &
        ##                             town %in% c('') &
        ##                             relative.month %in% c(),
        ##                             yes = NA,
        ##                             no = value))
    }
    else if(indicator ==  'ambulance all calls'){
        df <- mutate(df,
                     ## value = ifelse(sub.measure == 'total' &
                     ##                town %in% c('Bishop Auckland', 'Whitehaven') &
                     ##                relative.month %in% c(1, 2, 3, 4, 5, 6),
                     ##                yes = NA,
                     ##                no = value),
                     value = ifelse(sub.measure == 'total' &
                                    town %in% c('Grimsby') &
                                    relative.month %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                                    yes = NA,
                                    no = value),
                     value = ifelse(sub.measure == 'total' &
                                    town %in% c('Warwick') &
                                    relative.month %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25),
                                    yes = NA,
                                    no = value),
                     value = ifelse(sub.measure == 'total' &
                                    town %in% c('Hemel Hempstead') &
                                    relative.month %in% c(1),
                                    yes = NA,
                                    no = value),
                     value = ifelse(sub.measure == 'total' &
                                    town %in% c('Newark') &
                                    relative.month %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                                    yes = NA,
                                    no = value))
                     ## value = ifelse(sub.measure == 'total' &
                     ##                town %in% c('Rochdale', 'Rotherham') &
                     ##                relative.month %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                     ##                yes = NA,
                     ##                no = value))
    }
    else if(indicator ==  'hospital transfers'){
    ##     df <- mutate(df,
    ##                  value = ifelse(sub.measure == 'all stays' &
    ##                                 town %in% c('') &
    ##                                 relative.month %in% c(),
    ##                                 yes = NA,
    ##                                 no = value))
    ##     df <- mutate(df,
    ##                  value = ifelse(sub.measure == 'stays with transfer' &
    ##                                 town %in% c('') &
    ##                                 relative.month %in% c(),
    ##                                 yes = NA,
    ##                                 no = value))
    ##     df <- mutate(df,
    ##                  value = ifelse(sub.measure == 'fraction with transfer' &
    ##                                 town %in% c('') &
    ##                                 relative.month %in% c(),
    ##                                 yes = NA,
    ##                                 no = value))
    }
    ## Return the cleaned data frame
    return(df)
}
