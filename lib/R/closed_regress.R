#' Plot and Perform Regression on ClosED data
#'
#' @description Plot time-series and Perform Prais-Winsten Regression on ClosED data
#'
#' @details The ClosED study uses time-series with dummy indicators to
#' test for the impact of closing Emergency Departments on indicators of
#' performance.  This function generates time-series plots and performs
#' Prais-Winsten time-series regression analysis to account for atuo-correlation.
#'
#' @param df Data frame to analyse.
#' @param df.steps Data frame containing steps for all sites.
#' @param site The case site you wish to analyse, choose from \code{Bishop Auckland General Hospital} (default) | \code{Hemel Hempstead Hospital} | \code{Newark Hospital} | \code{Rochdale Infirmary} | \code{University Hospital of Hartlepool} | \code{All}.
#' @param controls The controls which should be included and analysed, choose from \code{matched} (default and only option if \code{group = 'All'}) | \code{pooled} | \code{none}.
#' @param indicator The performance indicator to assess.
#' @param sub.indicator The sub-measure performance indicator to assess.
#' @param covariates The covariates to include in the model.
#' @param steps List of steps (dummy variables) to include in time-series analysis.
#' @param fit.with Which package to fit Prais-Winsten regression with, options are  \code{both} (default) | \code{panelAR} | \code{prais}.
#' @param plot Generate time-series plot.
#' @param common.y Generate all plots with a common y-axis range.
#' @param theme GGplot2 theme to use (only relevant if \code{plot = TRUE}).
#' @param return.df Logical operator of whether to return the subsetted/summarised data frame (useful for subsequent development).
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
closed_regress<- function(df            = ed_attendances_by_mode_measure,
                          ## ToDo - Switch sites to steps when all steps are available
                          df.steps      = sites,
                          site          = 'Bishop Auckland General Hospital',
                          controls      = 'matched control',
                          indicator     = 'ed attendances',
                          sub.indicator = 'any',
                          covariates    = c('time.to.ed'),
                          steps         = c('closure'),
                          fit.with      = 'both',
                          plot          = TRUE,
                          common.y      = TRUE,
                          theme         = theme_bw(),
                          return.df     = FALSE,
                          latex         = FALSE,
                          html          = FALSE,
                          ...){
    #######################################################################
    ## Set up (results, formula, renaming variables)                     ##
    #######################################################################
    ## Initialise results list for returning everything
    results <- list()
    ## Convert to data frame
    df <- as.data.frame(df)
    ## Convert variable names for ease of typing within this function
    ## (ESS artefact, hitting underscore inserts '<-' so lots of underscores are
    ## tedious to type)
    names(df) <- names(df) %>%
        gsub("_", ".", x = .)
    ## Conditionally select range for y-axis, MUST do this BEFORE subsetting
    ## data so that it is common across all outcomes for the given indicator
    if(common.y == TRUE){
        ## Set the sites to include
        if(site != 'All' & controls != 'none'){
            site.town <- c('Bishop Auckland', 'Whitehaven',
                           'Hemel Hempstead', 'Warwick',
                           'Newark',          'Southport',
                           'Rochdale',        'Rotherham',
                           'Hartlepool',      'Grimsby')
            y.max <- dplyr::filter(df, measure == indicator & sub.measure == sub.indicator & town %in% site.town) %>%
                     group_by(town, yearmonth) %>%
                     summarise(n = sum(value))
        }
        else if(site == 'All' & controls == 'none'){
            site.town <- c('Bishop Auckland',
                           'Hemel Hempstead',
                           'Newark',
                           'Rochdale',
                           'Hartlepool')
            y.max <- dplyr::filter(df, measure == indicator & sub.measure == sub.indicator & town %in% site.town) %>%
                     group_by(town, yearmonth) %>%
                     summarise(n = sum(value))
        }
        else if(site == 'All' & controls != 'none'){
            y.max <- dplyr::filter(df, measure == indicator & sub.measure == sub.indicator) %>%
                     group_by(town, yearmonth) %>%
                     summarise(n = sum(value))
        }
        ## y.max <- dplyr::filter(df, measure == indicator & sub.measure == sub.indicator) %>%
        ##          group_by(town, yearmonth) %>%
        ##          summarise(n = sum(value))
        y.max <- max(y.max$n) %>%
                 round(-2)
    }
    ## Need to translate the supplied 'site' (which is compared against 'group')
    ## to the corresponding town so that
    if(site == 'Bishop Auckland General Hospital') site.town       <- 'Bishop Auckland'
    else if(site == 'Hemel Hempstead Hospital') site.town          <- 'Hemel Hempstead'
    else if(site == 'Newark Hospital') site.town                   <- 'Newark'
    else if(site == 'Rochdale Infirmary') site.town                <- 'Rochdale'
    else if(site == 'University Hospital of Hartlepool') site.town <- 'Hartlepool'
    ## Build the regerssion model, because this is site specific the panels are LSOAs
    ## and a dummy term for the EDs being compared can be included
    ## ToDo - Create formula from specified steps
    .formula <- reformulate(response   = 'value',
                            termlabels = c(covariates, steps))
    #######################################################################
    ## Filter data                                                       ##
    #######################################################################
    ## Filter based on sites to include
    if(site != 'All'){
        if(is.na(site)){
            stop('You must specify the site you wish to analyse.  See ?closed_regress for valid options\n\n')
        }
        ## print('Site?')
        df <- dplyr::filter(df, group == site)
        ## Set the panel
        panel <- 'lsoa'
    }
    else if(site == 'All'){
        ## print('All?')
        ## MUST collapse down the data to site because there are too many LSOAs for
        ## the Virtual Machine to handle (requires 80GB RAM, only have 16GB)
        df <- group_by(df, town, yearmonth, measure, sub.measure, site.type, group) %>%
              summarise(time.to.ed     = mean(time.to.ed),
                        value          = sum(value))
        ## Set the panel
        panel <- 'town'
    }
    else{
        stop('You must specify the site you wish to analyse.  See ?closed_regress for valid options\n\n')
    }
    ## ToDo - Modify to reflect the options should exclude all sub-measures
    if(!is.na(indicator) & !is.na(sub.indicator)){
        ## print('Indicator?')
        df <- dplyr::filter(df, measure == indicator & sub.measure == sub.indicator)
    }
    else{
        stop('You must specify an indicator and the sub measure you wish to analyse.  See ?closed_regress for valid options.\n\n')
    }
    ## Select intervention site and the desired controls
    if(controls == 'matched control'){
        ## print('Mathed Controls?')
        t <- c('intervention', controls)
        df <- dplyr::filter(df, site.type %in% t)
        rm(t)
    }
    else if(controls == 'pooled control'){
        ## print('Pooled controls?')
        df <- df
    }
    else if(controls == 'none'){
        case.only <- c('Bishop Auckland', 'Hemel Hempstead', 'Newark', 'Rochdale', 'Hartlepool')
        df <- dplyr::filter(df, town %in% case.only)
    }
    else{
        stop('You must specify the controls you wish to analyse.  See ?closed_regress for valid options.\n\n')
    }
    ## Combine steps with data frame
    df <- merge(df,
                df.steps,
                by     = c("group", "town"),
                all.x  = TRUE)
    ## Derive binary indicator for each step
    ## ToDo - Make this flexible for all steps
    ## ToDo - Probably don't need the if() else if()
    if(site != 'All'){
        ## Add dummy for closure
        df$closure <- 0
        df <- within(df, {
                     closure[town == site.town & yearmonth >= closure.date] <- 1
        })
        ## Select out data points that are -/+ 2 years either side of the closure date
        ## results$df <- df
        ## return(results)
        df <- mutate(df,
                     diff = as.numeric(closure.date) - as.numeric(yearmonth)) %>%
              dplyr::filter(diff < -730 | diff > 730)
    }
    else if(site == 'All'){
        ## ToDo - Dummy for sites with closures
        case.only <- c('Bishop Auckland', 'Hemel Hempstead', 'Newark', 'Rochdale', 'Hartlepool')
        df$closure <- 0
        df <- within(df, {
                     closure[town %in% case.only & yearmonth >= closure.date] <- 1
        })
        ## Select out data points that are -/+ 2 years from the lowest and highest
        ## closure dates
        df <- mutate(df,
                     first.closure = min(closure.date),
                     last.closure  = max(closure.date)) %>%
            dplyr::filter(as.numeric(first.closure) - as.numeric(yearmonth) < -730 |
                          as.numeric(last.closure)  - as.numeric(yearmonth) >  730)
    }
    ## Optionally return results
    if(return.df == TRUE){
        results$df <- df
    }
    #######################################################################
    ## Plot the data                                                     ##
    #######################################################################
    if(plot == TRUE){
        ## Conditionally set the graph title
        ## ToDo - Complete with all parameters
        ## Title
        if(indicator          == 'ed attendances')                 indicator.title     <- 'ED Attendance'
        else if(indicator     == 'mortality')                      indicator.title     <- 'Mortality'
        else if(indicator     == 'unnecessary ed attendances')     indicator.title     <- 'Unncessary ED Attendances'
        else if(indicator     == 'all emergency admissions')       indicator.title     <- 'Emergency Admissions'
        else if(indicator     == 'avoidable emergency admissions') indicator.title     <- 'Potentially Avoidable Emergency Admissions'
        if(sub.indicator      == 'any')                            sub.indicator.title <- ' - Any'
        else if(sub.indicator == 'ambulance')                      sub.indicator.title <- ' - Ambulance'
        else if(sub.indicator == 'other')                          sub.indicator.title <- ' - Other'
        else if(sub.indicator == 'unnecessary ed attendances')     sub.indicator.title <- ''
        else if(sub.indicator == 'all emergency admissions')       sub.indicator.title <- ''
        if(controls           == 'matched control')                control.title       <- 'Matched Control'
        else if(controls      == 'pooled control')                 control.title       <- 'Pooled Control'
        else if(controls      == 'all control')                    control.title       <- 'All Controls'
        ## Legends
        if(indicator          == 'ed attendances')                 y.title             <- 'ED Attendances'
        else if(indicator     == 'unnecessary ed attendances')     y.title             <- 'Unnecessary ED Attendances'
        else if(indicator     == 'all emergency admissions')       y.title             <- 'Emergency Admissions'
        else if(indicator     == 'avoidable emergency admissions') y.title             <- 'Potentially Avoidable Emergency Admissions'
        ## Vertical lines for steps
        closure.date <- dplyr::filter(df.steps, group == site) %>%
                        dplyr::select(closure.date) %>%
                        summarise(closure = mean(closure.date))
        closure.date <- closure.date[1,1] %>% as.numeric()
        ## ToDo Steps for other sites
        if(site == 'Bishop Auckland General Hospital'){

        }
        else if(site == 'Hemel Hemstead Hospital'){

        }
        else if(site == 'Newark Hospital'){

        }
        else if(site == 'Rochdale Infirmary'){

        }
        else if(site == 'University Hospital of Hartlepool'){

        }
        ## Summarise counts by ED department (town)
        results$by.town <- group_by(df, town, yearmonth) %>%
                           summarise(n = sum(value))
        ## Generate graph
        ## ToDo - Winsorise the data and break plotting when there are gaps
        ##        See https://stackoverflow.com/questions/14821064/line-break-when-no-data-in-ggplot2
        ##        for how to break lines when plotting (derive new variable for groups) once the data
        ##        has been winsorised (method currently pending, but possibly convert values <10% and
        ##        > 90% for pre-step/post-step within sites)
        results$ts.plot.events <- ggplot(data    = results$by.town,
                                         mapping = aes(x = yearmonth,
                                                       y = n,
                                                       color = town)) +
                                  geom_line() +
                                  geom_vline(xintercept = c(closure.date), linetype = 4) +
                                  ggtitle(paste0(site, " (",
                                                 control.title, ")\n",
                                                 indicator.title,
                                                 sub.indicator.title)) +
                                  ylab(paste0("Number of ",
                                              y.title)) +
                                  labs(color = 'Hospital') +
                                  scale_x_date(name              = "Date",
                                               date_breaks       = '3 month',
                                               date_labels       = '%Y-%m',
                                               date_minor_breaks = '1 month') +
                                  theme(axis.text.x = element_text(angle = 45,
                                                                   hjust = 1))
        ## Conditionally make y-axis common
        if(common.y == TRUE){
            results$ts.plot.events <- results$ts.plot.events +
                                      expand_limits(y = c(0, y.max))
        }
        ## Apply the user-specified theme
        if(!is.null(theme)){
            results$ts.plot.events <- results$ts.plot.events + theme +
                                      theme(axis.text.x = element_text(angle = 45,
                                                                       hjust = 1))

        }
    }
    ##########################################################################
    ## Perform regression using panelAR                                     ##
    ##########################################################################
    if(fit.with == 'panelAR' | fit.with == 'both'){
        ## Define time as an integer
        df$time <- as.integer(df$yearmonth)
        ## typeof(df$time) %>% print()
        ## table(df$time, useNA = "ifany") %>% print()
        ## results$.formula <- .formula
        ## results$panel <- panel
        ## results$df <- df
        ## Need to un-dplyr the data frame
        df <- as.data.frame(df)
        ## Run regression, saving all results for returning
        results$panelar <- panelAR(formula         = .formula,
                                   data            = df,
                                   timeVar         = 'time',
                                   panelVar        = panel,
                                   autoCorr        = 'ar1',
                                   ## ToDo - Make this a flexible option?
                                   panelCorrMethod = 'pcse')
        ## Extract the coefficients, tidy and add indicator
        results$coefficients      <- summary(results$panelar) %>%
                                     coef() %>%
                                     as.data.frame()
        rownames(results$coefficients) <- gsub('town', '', rownames(results$coefficients))
        if(site == 'All'){
            site <- paste0(site,
                           ' (',
                           controls,
                           ')')
        }
        results$coefficients$town <- site
        ## results$coefficients$term <- rownames(results$coefficients)
        ## results$coefficients$site <- site
        results$r2 <- results$panelar$r2
    }
    ## #######################################################################
    ## ## Perform regression using prais                                    ##
    ## #######################################################################
    ## if(fit.with == 'prais' | fit.with == 'both'){

    ## }
    #######################################################################
    ## Formatted results                                                 ##
    #######################################################################
    ## ToDo
    #######################################################################
    ## Return the results                                                ##
    #######################################################################
    return(results)
}
