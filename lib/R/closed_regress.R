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
#' @param controls The controls which should be included and analysed, choose from \code{matched} (default and only option if \code{group = 'All'}) | \code{pooled}.
#' @param indicator The performance indicator to assess.
#' @param sub.indicator The sub-measure performance indicator to assess.
#' @param steps List of steps (dummy variables) to include in time-series analysis.
#' @param fit.with Which package to fit Prais-Winsten regression with, options are  \code{both} (default) | \code{panelAR} | \code{prais}
#' @param plot Generate time-series plot
#' @param theme GGplot2 theme to use (only relevant if \code{plot = TRUE})
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
closed_regress<- function(df          = ed_attendances_by_mode_measure,
                          ## ToDo - Switch sites to steps when all steps are available
                          df.steps    = sites,
                          site        = 'Bishop Auckland General Hospital',
                          controls    = 'matched control',
                          indicator     = 'ed attendances',
                          sub.indicator = 'any',
                          steps       = c('closure'),
                          fit.with    = 'both',
                          plot        = TRUE,
                          theme       = theme_bw(),
                          latex       = FALSE,
                          html        = FALSE,
                          ...){
    #######################################################################
    ## Set up (results, formula, renaming variables)                     ##
    #######################################################################
    ## Initialise results list for returning everything
    results <- list()
    ## Build the regerssion model
    ## ToDo - Create formula from specified steps
    .formula <- reformulate(response   = 'value',
                            termlabels = c('town', 'time.to.ed', steps))
    ## Convert to data frame
    df <- as.data.frame(df)
    ## Convert variable names for ease of typing within this function
    ## (ESS artefact, hitting underscore inserts '<-' so lots of underscores are
    ## tedious to type)
    names(df) <- names(df) %>%
        gsub("_", ".", x = .)
    ## Need to translate the supplied 'site' (which is compared against 'group')
    ## to the corresponding town so that
    if(site == 'Bishop Auckland General Hospital') site.town       <- 'Bishop Auckland'
    else if(site == 'Hemel Hempstead Hospital') site.town          <- 'Hemel Hempstead'
    else if(site == 'Newark Hospital') site.town                   <- 'Newark'
    else if(site == 'Rochdale Infirmary') site.town                <- 'Rochdale'
    else if(site == 'University Hospital of Hartlepool') site.town <- 'Hartlepool'
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
    }
    else if(site == 'All'){
        ## print('All?')
        df <- df
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
        df$closure <- 0
        df <- within(df, {
            closure[town == site.town & yearmonth >= closure.date] <- 1
        })
    }
    else if(site == 'All'){
        df <- group_by(df, town) %>%
              mutate(closure = ifelse(yearmonth >= closure.date, 1, 0))
    }
    ## Combine data with steps
    ## results$df <- df
    #######################################################################
    ## Plot the data                                                     ##
    #######################################################################
    if(plot == TRUE){
        ## Conditionally set the graph title
        ## ToDo - Complete with all parameters
        ## Title
        if(indicator          == 'ed attendances')             indicator.title     <- 'ED Attendance'
        else if(indicator     == 'mortality')                  indicator.title     <- 'Mortality'
        else if(indicator     == 'unnecessary ed attendances') indicator.title     <- 'Unncessary ED Attendances'
        if(sub.indicator      == 'any')                        sub.indicator.title <- 'Any'
        else if(sub.indicator == 'ambulance')                  sub.indicator.title <- 'Ambulance'
        else if(sub.indicator == 'other')                      sub.indicator.title <- 'Other'
        else if(indicator     == 'unnecessary ed attendances') sub.indicator.title <- ''
        if(controls           == 'matched control')            control.title       <- 'Matched Control'
        else if(controls      == 'pooled control')             control.title       <- 'Pooled Control'
        ## Legends
        if(indicator          == 'ed attendances')             y.title <- 'ED Attendances'
        else if(indicator     == 'unnecessary ed attendances') y.title <- 'Unnecessary ED Attendances'
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
        results$ts.plot.events <- ggplot(data    = results$by.town,
                                         mapping = aes(x = yearmonth,
                                                       y = n,
                                                       color = town)) +
                                  geom_line() +
                                  geom_vline(xintercept = c(closure.date), linetype = 4) +
                                  ggtitle(paste0(indicator.title, " (",
                                                 sub.indicator.title, ")")) +
                                  ylab(paste0(site,
                                              " : Number of ",
                                              y.title)) +
                                  labs(color = 'Hospital') +
                                  scale_x_date(name              = "Date",
                                               date_breaks       = '3 month',
                                               date_labels       = '%Y-%m',
                                               date_minor_breaks = '1 month') +
                                  theme(axis.text.x = element_text(angle = 45,
                                                                   hjust = 1))

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
        ## Run regression, saving all results for returning
        results$panelar <- panelAR(formula         = .formula,
                                   data            = df,
                                   timeVar         = 'time',
                                   panelVar        = 'lsoa',
                                   autoCorr        = 'ar1',
                                   ## ToDo - Make this a flexible option?
                                   panelCorrMethod = 'pcse')
        ## Extract the coefficients, tidy and add indicator
        results$coefficients      <- summary(results$panelar) %>%
                                     coef() %>%
                                     as.data.frame()
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
