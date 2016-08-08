#' Pool Controls within or across cohorts in ClosED
#'
#' @description Pool control sites.
#'
#' @details The ClosED study uses time-series with dummy indicators to
#' test for the impact of closing Emergency Departments on indicators of
#' performance comparing case sites where Emergency Departments closed to matched
#' control sites.  This function pools control sites, either against their
#' matched controls or across all sites.
#'
#'
#' @param df Data frame to be plotted.
#' @param within.centre Whether to pool within case centres or across all.
#'
#' @examples
#'
#'
#' @references
#'
#' @export
closed_pool <- function(df              = ed_attendances_by_mode_site_measure,
                        within.centre   = TRUE,
                        ...){
    df <- mutate(df,
                 pooled.control = ifelse(site.type %in% c('matched control', 'pooled control'), 'Control', town))
    df <- mutate(df,
                 pooled.control = ifelse(pooled.control == 2, 'Bishop Auckland', pooled.control),
                 pooled.control = ifelse(pooled.control == 6, 'Harltepool', pooled.control),
                 pooled.control = ifelse(pooled.control == 7, 'Hemel Hempstead', pooled.control),
                 pooled.control = ifelse(pooled.control == 8, 'Newark', pooled.control),
                 pooled.control = ifelse(pooled.control == 9, 'Rochdale', pooled.control))
    df$pooled.control <- factor(df$pooled.control)
    ## Set reference group for pooled controls
    df$pooled.control <- relevel(df$pooled.control, ref = 'Control')
    ## Collapse the data, summing the counts within the grouping
    if(within.centre == TRUE){
        df <- group_by(df, measure, sub.measure, group, pooled.control, relative.month, yearmonth) %>%
              summarise(value = sum(value))
    }
    else{
        df <- group_by(df, measure, sub.measure, pooled.control, relative.month, yearmonth) %>%
              summarise(value = sum(value))
    }
    ## Add dummy indicators back into the data
    df <- mutate(df,
                 nhs111 = ifelse((pooled.control == 'Bishop Auckland' & relative.month >= 35) |
                                 (pooled.control == 'Hartlepool' & relative.month >= 45) |
                                 (pooled.control == 'Rochdale' & relative.month >= 48) |
                                 1, 0),
                 ambulance.divert = ifelse(pooled.control == 'Rochdale' & relative.month >= 17, 1, 0),
                 other.centre = ifelse((pooled.control == 'Hartlepool' & relative.month >= 22) |
                                       (pooled.control == 'Hemel Hempstead' & relative.month >= 20) |
                                       (pooled.control == 'Newark' & relative.month >= 3) |
                                       (pooled.control == 'Rochdale' & relative.month >= 11),
                                       1, 0),
                 closure  = ifelse(relative.month  > 24, 1, 0),
                 town = pooled.control)
    df$relative.month <- as.numeric(df$relative.month) %>% as.integer()
    ## No idea why pooled.control is removed, I think its actually needed
    ## df <- df[, -which(names(df) %in% c('pooled.control'))]
    ## Can only add a seasonal component if pooling is within centres since
    ## they do not align across centers
    if(within.centre == TRUE){
        df$season <- 1
        df <- within(df, {
                     season[month(yearmonth) == 1  | month(yearmonth) == 2]  <- 1
                     season[month(yearmonth) == 3  | month(yearmonth) == 4]  <- 2
                     season[month(yearmonth) == 5  | month(yearmonth) == 6]  <- 3
                     season[month(yearmonth) == 7  | month(yearmonth) == 8]  <- 4
                     season[month(yearmonth) == 9  | month(yearmonth) == 10] <- 5
                     season[month(yearmonth) == 11 | month(yearmonth) == 12] <- 6
                     })
    }
    ## Return the df
    return(df)
}
