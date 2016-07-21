#' Summarise ClosED data
#'
#' @description Summarise ClosED data by hospital
#'
#' @details The ClosED study uses time-series with dummy indicators to
#' test for the impact of closing Emergency Departments on indicators of
#' performance.  This short function tidies the data (ensures case of strings
#' is consistent, date/time and factor variables are correctly formatted) and
#' combines them with the dummy indicators that denote 'steps' (changes) in
#' the EDs functioning
#'
#' @param df Data frame to analyse.
#' @param df.steps Data frame containing steps for all sites.
#' @param vars The variable to summarise (default is \code{value} and shouldn't need changing).
#' @param plot.avg Whether to plot the \code{mean} (default) )or \code{median} of the difference in time to ED.
#' @param digits Number of digits to include in formatted output.
#' @param theme GGplot2 theme to use.
#' @param latex Produce results table in LaTeX format using Stargazer.
#' @param html Produce results table in HTML format using Stargazer.
#'
#' @return A list of results depending on the options specified.
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
closed_summary <- function(df          = ed_attendances_by_mode_measure,
                           ## ToDo - Switch sites to steps when all steps are available
                           df.steps    = sites,
                           vars        = value,
                           plot.avg    = 'mean',
                           digits      = 3,
                           theme       = theme_bw(),
                           latex       = FALSE,
                           html        = FALSE,
                           ...){
    #######################################################################
    ## Set up (results, formula, renaming variables)                     ##
    #######################################################################
    ## Initialise results list for returning everything
    results <- list()
    #######################################################################
    ## Combine steps with data frame                                     ##
    #######################################################################
    ## df <- merge(df,
    ##             dplyr::select(df.steps, group, town),
    ##             by     = c('group', 'town'),
    ##             all.x  = TRUE)
    ## Derive indicators for steps
    ## ToDo - Make this flexible for all steps
    df$closure <- ifelse(df$relative.month >= 24, 1, 0)
    #######################################################################
    ## Plot the distribution of Time to ED by site pre/post closure      ##
    #######################################################################
    ## Function to calculate mode
    mode <- function(x){
        ux <- unique(x)
        ux[which.max(tabulate(match(x, ux)))]
    }
    ## Collapse data, don't need repeated observations over time
    ## to.plot <- group_by(df, town, lsoa, closure) %>%
    to.plot <- group_by(df, town, lsoa) %>%
               summarise(time.to.ed.mode = mode(diff.time.to.ed),
                         time.to.ed.mean = mean(diff.time.to.ed, na.rm = TRUE),
                         time.to.ed.min  = min(diff.time.to.ed,  na.rm = TRUE),
                         time.to.ed.max  = max(diff.time.to.ed,  na.rm = TRUE),
                         time.to.ed.q25  = quantile(diff.time.to.ed, probs = (0.25), na.rm = TRUE),
                         time.to.ed.q50  = quantile(diff.time.to.ed, probs = (0.5),  na.rm = TRUE),
                         time.to.ed.q75  = quantile(diff.time.to.ed, probs = (0.75), na.rm = TRUE))
    to.plot <- dplyr::filter(to.plot, town %in% c('Bishop Auckland', 'Hemel Hempstead', 'Newark', 'Rochdale', 'Hartlepool')) %>%
        ungroup()
    if(plot.avg == 'mean'){
        results$plot.time.to.ed <- ggplot(to.plot, aes(time.to.ed.mean))
        max.diff <- max(to.plot$time.to.ed.mean) %>% round(digits = 0)
    }
    else if(plot.avg == 'median'){
        results$plot.time.to.ed <- ggplot(to.plot, aes(time.to.ed.median))
        max.diff <- max(to.plot$time.to.ed.median) %>% round(digits = 0)
    }
    results$plot.time.to.ed <- results$plot.time.to.ed + geom_histogram(bins = 20) +
                               facet_grid(town ~ .) +
                               xlab('Difference in Time to ED (minutes)') + ylab('N') +
                               ggtitle('Difference in Time to ED from LSOAs')
    ## Apply the user-specified theme
    if(!is.null(theme)){
        results$plot.time.to.ed <- results$plot.time.to.ed + theme
    }
    #######################################################################
    ## Summarise                                                         ##
    #######################################################################
    results$n.lsoa  <- dplyr::select(df, group, town, lsoa) %>%
                       unique() %>%
                       group_by(group, town) %>%
                       summarise(n.lsoa          = n())
    results$summary <- group_by(df, group, town, closure) %>%
                       summarise(time.to.ed.mean = mean(diff.time.to.ed, na.rm = TRUE),
                                 time.to.ed.sd   = sd(diff.time.to.ed,   na.rm = TRUE),
                                 time.to.ed.min  = min(diff.time.to.ed,  na.rm = TRUE),
                                 time.to.ed.max  = max(diff.time.to.ed,  na.rm = TRUE),
                                 time.to.ed.q25  = quantile(diff.time.to.ed, probs = (0.25), na.rm = TRUE),
                                 time.to.ed.q50  = quantile(diff.time.to.ed, probs = (0.5),  na.rm = TRUE),
                                 time.to.ed.q75  = quantile(diff.time.to.ed, probs = (0.75), na.rm = TRUE))
    results$summary <- merge(results$n.lsoa,
                             results$summary,
                             by  = c('group', 'town'))
    ## Removed 'Closed' from sites that don't actually closed
    ## ToDo - Need a 'smarter' way to do this
    open   <- dplyr::filter(results$summary, closure == 0)
    closed <- dplyr::filter(results$summary, closure == 1) %>%
              dplyr::filter(town %in% c('Bishop Auckland', 'Hemel Hempstead', 'Newark', 'Rochdale', 'Hartlepool'))
    results$summary <- rbind(open, closed)
    rm(open, closed)
    ## results$summary <- dplyr::filter(results$summary, ed.status == 1 & !town %in% !c('Bishop Auckland', 'Hemel Hempstead', 'Newark', 'Rochdale', 'Hartlepool'))
    results$n.lsoa <- NULL
    #######################################################################
    ## Formatted Output                                                  ##
    #######################################################################
    results$formatted <- within(results$summary, {
                                ed.status <- 'Open'
                                ed.status[closure == 1] <- 'Closed'
                                ed.status[is.na(ed.status)] <- 'Open'
                                mean.sd <- paste0(formatC(time.to.ed.mean, digits = digits, format = 'f'),
                                                  ' (',
                                                  formatC(time.to.ed.sd, digits = digits, format = 'f'),
                                                  ')')
                                range <- paste0(formatC(time.to.ed.min, digits = 1, format = 'f'),
                                                ' to ',
                                                formatC(time.to.ed.max, digits = 1, format = 'f'))
                                median.iqr <- paste0(formatC(time.to.ed.q50, digits = 1, format = 'f'),
                                                  ' (',
                                                  formatC(time.to.ed.q25, digits = 1, format = 'f'),
                                                  '-',
                                                  formatC(time.to.ed.q75, digits = 1, format = 'f'),
                                                  ')')
                                }) %>%
                         dplyr::select(group, town, closure, n.lsoa, ed.status, mean.sd, range, median.iqr)
    ## Derive groups and order variables
    results$formatted$order1 <- 1
    results$formatted$order2 <- 1
    results$formatted$order3 <- 1
    results$formatted <- within(results$formatted, {
                                order1[group == 'Bishop Auckland General Hospital']  <- 1
                                order1[group == 'Hemel Hempstead Hospital']          <- 2
                                order1[group == 'Newark Hospital']                   <- 3
                                order1[group == 'Rochdale Infirmary']                <- 4
                                order1[group == 'University Hospital of Hartlepool'] <- 5
                                order2[town  == 'Bishop Auckland'] <- 1
                                order2[town  == 'Salford']         <- 2
                                order2[town  == 'Scarborough']     <- 3
                                order2[town  == 'Whitehaven']      <- 4
                                order2[town  == 'Hemel Hepmstead'] <- 1
                                order2[town  == 'Basingstoke']     <- 2
                                order2[town  == 'Warwick']         <- 3
                                order2[town  == 'Yeovil']          <- 4
                                order2[town  == 'Newark']          <- 1
                                order2[town  == 'Carlisle']        <- 2
                                order2[town  == 'Salisbury']       <- 3
                                order2[town  == 'Southport']       <- 4
                                order2[town  == 'Rochdale']        <- 1
                                order2[town  == 'Rotherham']       <- 2
                                order2[town  == 'Scunthorpe']      <- 3
                                order2[town  == 'Wansbeck']        <- 4
                                order2[town  == 'Hartlepool']      <- 1
                                order2[town  == 'Blackburn']       <- 2
                                order2[town  == 'Grimsby']         <- 3
                                order2[town  == 'Wigan']           <- 4
                                order3[closure == 'Open']   <- 1
                                order3[closure == 'Closed'] <- 2
                                }) %>%
                         arrange(order1, order2) %>%
                         dplyr::select(town, ed.status, n.lsoa, mean.sd, range, median.iqr)
    names(results$formatted) <- c('Town', 'ED Status', 'LSOAs', 'Mean (SD)', 'Range', 'Median (IQR)')
    results$caption <- 'Distribution of Differences in the time to Emergency Departments for LSOAs in the catchment areas of affected Hospitals.'
    ## Return results
    return(results)
}
