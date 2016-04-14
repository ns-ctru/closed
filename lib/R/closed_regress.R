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
#' @param group The case site you wish to analyse, choose from \code{Bishop Auckland General Hospital} (default) | \code{Hemel Hempstead Hospital} | \code{Newark Hospital} | \code{Rochdale Infirmary} | \code{University Hospital of Hartlepool} | \code{All}.
#' @param controls The controls which should be included and analysed, choose from \code{matched} (default and only option if \code{group = 'All'}) | \code{pooled}.
#' @param measure The performance indicator to assess.
#' @param sub.measure The sub-measure performance indicator to assess.
#' @param steps List of steps (dummy variables) to include in time-series analysis.
#' @param fit.with Which package to fit Prais-Winsten regression with, options are  \code{both} (default) | \code{panelAR} | \code{prais}
#' @param plot Generate time-series plot
#' @param theme GGplot2 theme to use (only relevant if \code{plot = TRUE})
#' @param dose Perform dose response model based on median travel time to ED for LSOA.
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
                          site        = 'Bishop Auckland General Hospital',
                          controls    = 'matched control',
                          indicator     = 'ed attendances',
                          sub.indicator = 'any',
                          steps       = c('closure'),
                          fit.with    = 'both',
                          plot        = TRUE,
                          theme       = NULL,
                          dose        = FALSE,
                          latex       = FALSE,
                          html        = FALSE,
                          ...){
    ## Initialise results list for returning everything
    results <- list()
    ## Build the regerssion model
    ## ToDo - Create formula from specified steps
    ## .formula <- reformulate(repsonse   = 'value',
    ##                         termalbels = c('town', 'time.to.ed', steps))
    ## Convert to data frame
    df <- as.data.frame(df)
    ## Convert variable names for ease of typing within this function
    ## (ESS artefact, hitting underscore inserts '<-' so lots of underscores are
    ## tedious to type)
    names(df) <- names(df) %>%
                 gsub("_", ".", x = .)
    print("The data frame passed to this function has...")
    names(df) %>% print()
    dim(df) %>% print()
    ## Filter based on sites to include
    if(!is.na(site) & site != 'All'){
        print("Filtering site...")
        print(site)
        df <- dplyr::filter(df, group == site)
        dim(df) %>% print()
    }
    else if(site == 'All'){
        df <- df
    }
    else{
        stop('You must specify the site you wish to analyse.  See ?closed_regress for valid options\n\n')
    }
    ## ToDo - Modify to reflect the options should exclude all sub-measures
    if(!is.na(indicator) & !is.na(sub.indicator)){
        print("Filtering measures...")
        df <- dplyr::filter(df, measure == indicator & sub.measure == sub.indicator)
        dim(df) %>% print()
    }
    else{
        stop('You must specify an indicator and the sub measure you wish to analyse.  See ?closed_regress for valid options.\n\n')
    }
    ## Select intervention site and the desired controls
    if(controls == 'matched control' | controls == 'pooled control'){
        print("Filtering controls...")
        t <- c('intervention', controls)
        df <- dplyr::filter(df, site.type %in% t)
        rm(t)
        dim(df) %>% print()
    }
    else{
        stop('You must specify the controls you wish to analyse.  See ?closed_regress for valid options.\n\n')
    }
    results$df <- df
    ## Plot the data
    if(plot == TRUE){
        ## Conditionally set the graph title
        ## ToDo - Complete with all parameters
        if(indicator      == 'ed attendances')  indicator.title   <- 'Mode of ED Attendance'
        else if(indicator == 'mortality')       indicator.title   <- 'Mortality'
        else if(indicator == 'other')           indicator.title   <- 'Other'
        if(controls == 'matched control')       control.title <- 'Matched Control'
        else if(controls == 'pooled control')   control.title <- 'Pooled Control'
        ## Summarise counts by ED department (town)
        results$by.town <- group_by(df, town, yearmonth) %>%
                           summarise(n = sum(value))
        ## Generate graph
        results$ts.plot.events <- ggplot(data    = results$by.town,
                                         mapping = aes(x = yearmonth,
                                                       y = n,
                                                       color = town)) +
                                  geom_line() +
                                  ## stat_seas(start = c(2000, 1), frequency = 2) +
                                  ggtitle(paste0(indicator.title, " at ",
                                                 site, " (",
                                                 control.title, ")"))
        ## Apply the user-specified theme
        if(!is.null(theme)){

        }
    }
    ## Perform regression using panelAR
    if(fit.with == 'panelAR' | fit.with == 'both'){
        ## results$panelar <- panelAR(formula = .formula,
        ##                            data    = df)
    }
    ## Perform regression using prais
    if(fit.with == 'prais' | fit.with == 'both'){

    }
    ## Return the results
    return(results)
}
