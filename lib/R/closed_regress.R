#' Tidy ClosED data
#'
#' @description Tidy up Closed data and combine with 'step' co-variates
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
                          theme       = theme_linedraw(),
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
    print(.formula)
    ## Convert to data frame
    df <- as.data.frame(df)
    ## Convert variable names for ease of typing within this function
    ## (ESS artefact, hitting underscore inserts '<-' so lots of underscores are
    ## tedious to type)
    names(df) <- names(df) %>%
                 gsub("_", ".", x = .)
    #######################################################################
    ## Filter data                                                       ##
    #######################################################################
    ## Filter based on sites to include
    if(!is.na(site) & site != 'All'){
        df <- dplyr::filter(df, group == site)
    }
    else if(site == 'All'){
        df <- df
    }
    else{
        stop('You must specify the site you wish to analyse.  See ?closed_regress for valid options\n\n')
    }
    ## ToDo - Modify to reflect the options should exclude all sub-measures
    if(!is.na(indicator) & !is.na(sub.indicator)){
        df <- dplyr::filter(df, measure == indicator & sub.measure == sub.indicator)
    }
    else{
        stop('You must specify an indicator and the sub measure you wish to analyse.  See ?closed_regress for valid options.\n\n')
    }
    ## Select intervention site and the desired controls
    if(controls == 'matched control' | controls == 'pooled control'){
        t <- c('intervention', controls)
        df <- dplyr::filter(df, site.type %in% t)
        rm(t)
    }
    else{
        stop('You must specify the controls you wish to analyse.  See ?closed_regress for valid options.\n\n')
    }
    ## Extract step information for specified group (not strictly necessary due to subsequent
    ## merge using all.x)
    ## ToDo - Will need modifying when other steps are available
    df.steps <- dplyr::filter(df.steps, group == site) %>%
                dplyr::select(group, town, closure, ambulance.service)
    ## Combine steps with data frame
    df <- merge(df,
                df.steps,
                by     = c("group", "town"),
                all.x  = TRUE)
    ## Combine data with steps
    results$df <- df
    #######################################################################
    ## Plot the data                                                     ##
    #######################################################################
    if(plot == TRUE){
        ## Conditionally set the graph title
        ## ToDo - Complete with all parameters
        ## Title
        if(indicator          == 'ed attendances')  indicator.title     <- 'ED Attendance'
        else if(indicator     == 'mortality')       indicator.title     <- 'Mortality'
        else if(indicator     == 'other')           indicator.title     <- 'Other'
        if(sub.indicator      == 'any')             sub.indicator.title <- 'Any'
        else if(sub.indicator == 'ambulance')       sub.indicator.title <- 'Ambulance'
        else if(sub.indicator == 'other')           sub.indicator.title <- 'Other'
        if(controls           == 'matched control') control.title       <- 'Matched Control'
        else if(controls      == 'pooled control')  control.title       <- 'Pooled Control'
        ## Legends
        if(indicator          == 'ed attendances')  y.title <- 'ED Attendances'
        ## Vertical lines for steps
        step.closure <- dplyr::filter(df.steps, group == site) %>%
                        dplyr::select(closure) %>%
                        summarise(closure = mean(closure))
        step.closure <- step.closure[1,1] %>% as.numeric()
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
                                  geom_vline(xintercept = c(step.closure), linetype = 4) +
                                  ggtitle(paste0(indicator.title, " (",
                                                 sub.indicator.title, ")")) +
                                  ylab(paste0("Number of ",
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
            results$ts.plot.events <- results$ts.plot.events + theme
        }
    }
    #######################################################################
    ## Perform regression using panelAR
    #######################################################################
    if(fit.with == 'panelAR' | fit.with == 'both'){
        ## Define time as an integer
        df$time <- as.numeric(df$yearmonth)
        ## Run regression, saving all results for returning
        results$panelar <- panelAR(formula         = .formula,
                                   data            = df,
                                   timevar         = time,
                                   panelvar        = 'lsoa',
                                   autoCorr        = 'ar1',
                                   panelCorrMethod = 'pcse')
        ## Extract the coefficients, tidy and add indicator
        coefficients      <- summary(results$panelar) %>%
                             coef()
        coefficients$term <- rownames(coefficients)
        coefficients$site <- site
    }
    #######################################################################
    ## Perform regression using prais                                    ##
    #######################################################################
    if(fit.with == 'prais' | fit.with == 'both'){

    }
    #######################################################################
    ## Formatted results                                                 ##
    #######################################################################
    ## Return the results
    return(results)
}
