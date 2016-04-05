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
#' @param sites List of sites to include, should be lower-case and list the case and control sites of interest.
#' @param indicator Performance indicator to assess.
#' @param steps List of steps (dummy variables) to include in time-series analysis.
#' @param fit.with Which package to fit Prais-Winsten regression with, options are
#'                 \code{panelAR} | \code{prais} | \code{both} (default)
#' @param plot Generate time-series plot
#' @param theme GGplot2 theme to use (only relevant if \code{plot = TRUE})
#' @param dose Perform dose response model based on median travel time to ED for LSOA.
#' @param latex Produce results table in LaTeX format using Stargazer.
#' @param html Produce results table in HTML format using Stargazer.
#'
#' @return A list of results depending on the options specified
#'
#' @examples
#'
#' @references
#'
#' @export
closed_regress<- function(df        = test,
                          sites     = c('Bishop Auckland', 'Hemel Hempstead'),
                          indicator = 'mortality',
                          steps     = c(closure),
                          fit.with  = 'both',
                          plot      = TRUE,
                          theme     = NULL,
                          dose      = FALSE,
                          latex     = FALSE,
                          html      = FALSE,
                          ...){
    ## Initialise results list for returning everything
    results <- list()
    ## Build the regerssion model
    .formula <- formulate()
    ## Filter the data based on the specified indicator
    ## ToDo - Modify to reflect the options should exclude all sub-measures
    if(indicator == 'mortality.rate'){
        df <- dplyr::filter(df, measure == indicator)
    }
    else{
        df <- dplyr::filter(df, sub.measure == indicator)
    }
    ## Filter based on sites to include
    df <- dplyr::filter(df, site.short %in% sites )
    ## Plot the data
    if(plot == TRUE){
        ## Conditionally set the graph title
        ## ToDo - Complete
        if(indicator      == 'mortality')  title <- 'Mortality'
        else if(indicator == 'admissions') title <- 'Admissions'
        else if(indicator == 'other')      title <- 'Other'
        title.site <- site[1]
        results$ts.plot.events <- ggplot(data    = df,
                                         mapping = aes(yearmonth,
                                                       value)) +
            geom_line() +
            stat_seas(start = c(2000, 1), frequency = 2) +
            ggtitle(paste0(title,
                           " at ",
                           title.site,
                           " and "))
        ## Apply the user-specified theme
        if(!is.null(theme)){

        }
    }
    ## Perform regression using panelAR
    if(fit.with == 'panelAR' | fit.with == 'both'){
        results$panelar <- panelAR(formula = .formula,
                                   data    = df)
    }
    ## Perform regression using prais
    if(fit.with == 'prais' | fit.with == 'both'){

    }
    ## Return the results
    return(results)
}
