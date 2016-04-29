#' Summarise ClosED data
#'
#' @description Summarise ClosED regression results
#'
#' @details The ClosED study uses time-series with dummy indicators to
#' test for the impact of closing Emergency Departments on indicators of
#' performance.  This short function tidies the results of various time-series
#' regression models and produces forest plots and summary tables of the key
#' predictor variables.
#'
#' @param df.list A list of data frame objects to summarise.
#' @param term The term(s) from the regression model that is to be summarised/plotted.
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
closed_forest <- function(df.list     = c(bishop.attendance.any.matched$coefficients,
                                          hartlepool.attendance.any.matched$coefficients,
                                          hemel.attendance.any.matched$coefficients,
                                          newark.attendance.any.matched$coefficients,
                                          rochdale.attendance.any.matched$coefficients,
                                          all.attendance.any.matched$coefficients), ## ToDo - Add all pooled and case only
                          term        = 'closure',
                          digits      = 3,
                          theme       = theme_bw(),
                          latex       = FALSE,
                          html        = FALSE,
                          ...){
    ## Inititiate list for returning results
    results <- list()
    ## Set label for x-axis based on term
    ## ToDo
    if(length(term) == 1){
        if(term == 'closure')         xaxis.label <- 'Closure'
        else if(term == 'time.to.ed') xaxis.label <- 'Time to ED'
        else if(term == 'intercept')  xaxis.label <- 'Intercept'
        xaxis.label <- paste0('Coefficient Estimate for ', xaxis.label)
    }
    else{
        xaxis.label <- 'Coefficient Estimates'
    }
    ## Convert lower case term to the names actually used
    ## ToDo - Is it worth converting these within closed_regress()
    ## term <- gsub('', 'town', term)
    ## term <- gsub('Time to ED', 'time.to.ed', term)
    ## term <- gsub('Intercept', '\\(Intercept\\)', term)
    ## term <- gsub('Closure', 'closure', term)
    ## ToDo - Workout how to take list of lists and extract coefficients internally
    #########################################################################
    ## Make data frame of coefficients                                     ##
    #########################################################################
    ## Bind everything together
    ## ToDo - Modify when All pooled and Case only are specified.
    ## ToDo - Ultimately make this flexible so that arbitrary numbers of outcomes can be grouped and plotted.
    if(length(df.list) < 6){
        stop('There should be six objects to be summarised, one for each site and one for the All v Matched Controls')
    }
    else{
        typeof(df.list) %>% print()
        typeof(df.list[[1]]) %>% print()
        df.list[[1]] %>% print()
        typeof(df.list[1]) %>% print()
        df.list[1] %>% print()
        df <- cbind(df.list[[1]],
                    df.list[[2]],
                    df.list[[3]],
                    df.list[[4]],
                    df.list[[5]],
                    df.list[[6]])
    }
    df %>% print()
    ## Return data frame for printing as kable
    results$summary <- df
    ## Return everything
    return(results)
    #########################################################################
    ## Use data frame to produce forest plot                               ##
    #########################################################################
    results$forest <- dplyr::filter(df, term %in% term)
    if(length(term) == 1){
        results$forest <- results$forest +
                          ggplot(aes(x = est,
                                     y = site))
    }
    else{
        results$forest <- results$forest +
                          ggplot(aes(x = est,
                                     y = site,
                                     color = term))
    }
    results$forest <- results$forest + geom_point() +
                      geom_errorbarh(aes(xmin = est - se,xmax = est + se),height = 0.25) +
                      geom_vline(xintercept = 0,linetype = "dashed") +
                      ylab("Closed ED") + xlab(paste0('Estimate for ', term))
    ## Apply the user-specified theme
    if(!is.null(theme)){
        results$forest <- results$forest + theme

    }
}
