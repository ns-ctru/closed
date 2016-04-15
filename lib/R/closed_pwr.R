#' Prais-Winsten Regression for ClosED
#'
#' @description
#' @description Time-series analysis using Prais-Winsten to account for autocorrelation
#'
#' @details The ClosED study investigates the impact closing Emergency Departments has
#' on a number of different metrics such as mortality, emergency admission, and ambulance
#' service performance.  To account for the autocorrelation that exists the Prais-Winsten
#' regression approach is taken (as oppossed to ARIMA or GARCH modelling approaches).
#' This function serves as a wrapper taking a given data set (or subset thereof),
#' performing the regression and summarising the results in a formatted manner whilst
#' producing time-series plots to aid visualisation and interpretation of the regression
#' model
#' 
#'
#' 
#' @param df Data frame to analyse, should include all co-variates (including step terms).
#' @param outcome Performance indicator that is being assessed, choose from \code{mortality} \code{admissions} \code{ambulance.service}.
#' @param predictors Covariates for inclusion in the regression model.
#' @param condition The specific condition or performance indicator that is being assessed.
#' @param site The ED site to assess, default is \code{all} but specific sites can be assessed by specifygin them
#' @param 
#' @return A list containing the model fit (\code{$pwr}) a formatted table of regression results
#'         (\code{$latex}, \code{$html} and/or \code{$ascii}) produced using Stargazer.
#'
#' @examples
#'
#' @references
#'
#' @export
closed_pwr<- function(df                = test,
                      outcome           = 'mortality',
                      predictors        = c('site', 'closed.step'),
                      condition         = 'all',
                      site              = 'all',
                      prais.winsten     = 'both',
                      plot              = TRUE,
                      latex             = TRUE,
                      html              = TRUE,
                      ascii             = TRUE,
                      ...){
    ## Initialise object to return results
    results <- list()
    ## Build the formula
    .formula <- reformulate(response = outcome,
                            termlabels = predictors)
    ## Produce Plot
    if(plot == TRUE){
        ## ToDo Plot using ggseasons()
    }
    ## Run Regression
    ## Format Results in LaTeX
    if(latex == TRUE){
        
    }
    ## Format Results in HTML
    if(html == TRUE){
        
    }
    ## Format Results in ASCII
    if(ascii == TRUE){
        
    }
    ## Return the results
    return(results)
}
