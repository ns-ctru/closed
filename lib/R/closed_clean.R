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
#' the data set prior to analyses (or plotting).
#'
#' Currently spurious data points to be cleaned are internally coded.
#'
#' **NB** This function works with only one data frame at a time, there are two for
#'        each outcome (Trust and LSOA level data).  Both datasets require cleaning
#'        so remember to clean both.
#'
#' @param df.lsoa Data frame at the LSOA level to analyse.
#' @param df.trust Data frame at the Trust level to analyse.
#' @param indicator The performance indicator to assess.
#' @param sub.indicator The sub-measure performance indicator to assess.
#'
#' @return A cleaned data frame with spurious data points removed.
#'
#' @examples
#'
#' ## Run mortality analyses for Bishop Auckland and its matched site
#' ## producing time-series plot, step and dose models using both the
#' ## panelAR and prais package.
#'
#' ts.mortality <- closed_regress(df       = ,)
#'
#'
#' @references
#'
#' @export
closed_clean <- function(df              = ed_attendances_by_mode_measure,
                          indicator       = 'ed attendances',
                          sub.indicator   = 'any',
                          balance         = TRUE,
                          ...){
    ## Clean the data set conditional on the Indicator and in turn Sub-indicator
    if(indicator == 'ed attendances'){
        if(sub.indicator == 'any'){
            if(balance == TRUE){
                df <- mutate(df,
                             value = ifelse(sub.measure == sub.indicator &
                                            !(relative.month %in% c(1, 6, 24)),
                                            yes = NA,
                                            no  = value))
            }
            else{
                df <- mutate(df,
                             value = ifelse(town == 'Bishop Auckland' &
                                            sub.measure == sub.indicator &
                                            !(relative.month %in% c(1, 6)),
                                            yes = NA,
                                            no  = value),
                             value = ifelse(town == 'Southport' &
                                            sub.measure == sub.indicator &
                                            !(relative.month %in% c(24)),
                                            yes = NA,
                                            no  = value),
                             value = ifelse(town == 'Basingstoke' &
                                            sub.measure == sub.indicator &
                                            !(relative.month %in% c(1:9, 31-47)),
                                            yes = NA,
                                            no  = value))
            }
        }
        if(sub.indicator == 'other'){

        }
        if(sub.indicator == 'ambulance'){

        }
    }
    else if(indicator == 'unnecessary attendances'){

    }
    else if(indicator == 'all emergency admissions'){

    }
    else if(indicator == 'avoidable emergency admissions'){

    }
    else if(indicator == 'ed attendances admitted'){

    }
    else if(indicator == 'critical care stays'){

    }
    else if(indicator == 'length of stay'){

    }
    else if(indicator == 'case fatality ratio'){

    }
    else if(indicator == 'ambulance mean times'){

    }

}
