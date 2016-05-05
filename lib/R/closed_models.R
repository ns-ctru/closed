#' Plot and Perform Regression on ClosED data
#'
#' @description Plot time-series and Perform Prais-Winsten Regression on ClosED data
#'
#' @details The ClosED study uses time-series with dummy indicators to
#' test for the impact of closing Emergency Departments on indicators of
#' performance.  This function generates time-series plots and performs
#' Prais-Winsten time-series regression analysis to account for atuo-correlation.
#'
#' This function differs from \code{closed_regress} in that it runs one model on
#' all sites, returning the results for each model along with summary data frames
#' of all coefficients and forest plots.  Since the site to be analysed includes
#' all of them and the controls vary between model these do not need to be specified.
#' The covariates are coded internally depending on the model and are not therefore
#' specified by the user.
#'
#' @param df.lsoa Data frame at the LSOA level to analyse.
#' @param df.trust Data frame at the Trust level to analyse.
#' @param df.steps Data frame containing steps for all sites.
#' @param indicator The performance indicator to assess.
#' @param sub.indicator The sub-measure performance indicator to assess.
#' @param steps List of steps (dummy variables) to include in time-series analysis.
#' @param fit.with Which package to fit Prais-Winsten regression with, options are  \code{both} (default) | \code{panelAR} | \code{prais}.
#' @param panel.lsoa Variable that defines panels in LSOA level data (default is \code{lsoa} and shouldn't need changing).
#' @param panel.trust Variable that defines panels in Trust level data (default is \code{town} and shouldn't need changing).
#' @param time Variable defining time in both data sets (default is \code{relative.month} and shouldn't need changing).
#' @param outcome Outcome variable containing the counts (default is \code{value} and shouldn't need changing).
#' @param model1 Covariates to include in model 1.
#' @param model2 Covariates to include in model 2.
#' @param model3 Covariates to include in model 3.
#' @param model4 Covariates to include in model 4.
#' @param model5 Covariates to include in model 5.
#' @param model6 Covariates to include in model 6.
#' @param plot Generate time-series plot.
#' @param common.y Generate all plots with a common y-axis range.
#' @param theme GGplot2 theme to use (only relevant if \code{plot = TRUE}).
#' @param return.df Logical operator of whether to return the subsetted/summarised data frame (useful for subsequent development).
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
closed_models <- function(df.lsoa         = ed_attendances_by_mode_measure,
                          df.trust        = ed_attendances_by_mode_site_measure,
                          df.steps        = steps,
                          indicator       = 'ed attendances',
                          sub.indicator   = 'any',
                          steps           = c('closure'),
                          fit.with        = 'both',
                          panel.lsoa      = 'lsoa',
                          panel.trust     = 'town',
                          timevar         = 'relative.month',
                          outcome         = 'value',
                          model1          = c('closure', 'season'), ## ToDo - Add other steps when available
                          model2          = c('closure', 'season', 'site', 'site * closure'),
                          model3          = c('closure', 'season', 'site', 'site * closure'),
                          model4          = c(),
                          model5          = c(),
                          model6          = c(),
                          autocorr        = 'ar1',
                          panelcorrmethod = 'pcse',
                          plot            = TRUE,
                          common.y        = TRUE,
                          theme           = theme_bw(),
                          return.df       = FALSE,
                          html            = FALSE,
                          ...){
    #######################################################################
    ## Set up (results, formula, renaming variables)                     ##
    #######################################################################
    ## Initialise results list for returning everything
    results <- list()
    ## Convert to data frame
    df.lsoa  <- as.data.frame(df.lsoa)
    df.trust <- as.data.frame(df.trust)
    df.steps <- as.data.frame(df.steps)
    ## Convert variable names for ease of typing within this function
    ## (ESS artefact, hitting underscore inserts '<-' so lots of underscores are
    ## tedious to type)
    names(df.lsoa)  <- names(df.lsoa) %>%
                       gsub("_", ".", x = .)
    names(df.trust) <- names(df.trust) %>%
                       gsub("_", ".", x = .)
    names(df.steps) <- names(df.steps) %>%
                       gsub("_", ".", x = .)
    ## Conditionally select range for y-axis, MUST do this BEFORE subsetting
    ## data so that it is common across all outcomes for the given indicator
    if(common.y == TRUE){
        df.lsoa.max  <-  max(df.lsoa$value)
        df.trust.max <-  max(df.trust$value)
        y.max <- max(df.lsoa.max, df.trust.max) %>%
                 round(-2)
    }
    #######################################################################
    ## Derive a seasonal indicator (really this should be on the data    ##
    ## preparation side but Tony is already doing tons)                  ##
    #######################################################################
    df.lsoa$season <- 1
    df.lsoa <- within(df.lsoa,{
                      season[month(yearmonth) == 1  | month(yearmonth) == 2]  <- 1
                      season[month(yearmonth) == 3  | month(yearmonth) == 4]  <- 2
                      season[month(yearmonth) == 5  | month(yearmonth) == 6]  <- 3
                      season[month(yearmonth) == 7  | month(yearmonth) == 8]  <- 4
                      season[month(yearmonth) == 9  | month(yearmonth) == 10] <- 5
                      season[month(yearmonth) == 11 | month(yearmonth) == 12] <- 6
    })
    df.trust$season <- 1
    df.trust <- within(df.trust,{
                      season[month(yearmonth) == 1  | month(yearmonth) == 2]  <- 1
                      season[month(yearmonth) == 3  | month(yearmonth) == 4]  <- 2
                      season[month(yearmonth) == 5  | month(yearmonth) == 6]  <- 3
                      season[month(yearmonth) == 7  | month(yearmonth) == 8]  <- 4
                      season[month(yearmonth) == 9  | month(yearmonth) == 10] <- 5
                      season[month(yearmonth) == 11 | month(yearmonth) == 12] <- 6
    })
    #######################################################################
    ## Add a 'step' indicator for closure                                ##
    #######################################################################
    sites <- c('Bishop Auckland', 'Hartlepool', 'Hemel Hempstead', 'Newark', 'Rochdale')
    df.lsoa$closure  <- ifelse(df.lsoa$relative.month  >= 25, 1, 0)
    df.trust$closure <- ifelse(df.trust$relative.month >= 25, 1, 0)
    #######################################################################
    ## Labels and captions conditional on outcome                        ##
    #######################################################################
    ## ToDo - Switch to parsing the data frames name, slightly easier/greater
    ##        internal consistency
    ## indicator <- substitute(df.lsoa) %>% evaulate()
    if(indicator == 'ed attendances')                      title1 <- 'ED Attendance'
    else if(indicator == 'unnecessary ed attendances')     title1 <- 'Unnecessary ED Attendances'
    else if(indicator == 'all emergency admissions')       title1 <- 'Emergency Admissions'
    else if(indicator == 'avoidable emergency admissions') title1 <- 'Unnecessary Emergency Attendances'
    if(sub.indicator == 'any')                             title2 <- ' (Any)'
    else if(sub.indicator == 'other')                      title2 <- ' (Other)'
    else if(sub.indicator == 'ambulance')                  title2 <- ' (Other)'
    else if(sub.indicator == 'unnecessary ed attendances') title2 <- ''
    else if(sub.indicator == 'all emergency admissions')   title2 <- ''
    #######################################################################
    ## Internal functions (to save typing)                               ##
    #######################################################################
    ## closed_panelar <- function(.df       = .,
    ##                            .site     = 'Bishop Auckland',
    ##                            .formula  = formula.model1,
    ##                            .timeVar  = timevar,
    ##                            .panelVar = panel.lsoa,
    ##                            .autoCorr = autocorr,
    ##                            .panelCorrMethod = panelcorrmethod,
    ##                            ...){
    ##     ## List to return results
    ##     results.panelar <- list()
    ##     ## names(.df) %>% print()
    ##     ## head(.df) %>% print()
    ##     ## dim(.df) %>% print()
    ##     ## paste0('Formula : ', .formula) %>% print()
    ##     ## paste0('Time : ', .timeVar) %>% print()
    ##     ## paste0('Panel : ', .panelVar) %>% print()
    ##     ## paste0('Autocorrelation : ', .autoCorr) %>% print()
    ##     ## paste0('Standard Error : ', .panelCorrMethod) %>% print()
    ##     ## Run regression
    ##     t <- panelAR(data     = .df,
    ##                                      formula  = .formula,
    ##                                      timeVar  = .timeVar,
    ##                                      panelVar = .panelVar,
    ##                                      autoCorr = .autoCorr,
    ##                  panelCorrMethod = panelcorrmethod)
    ##     print(t)
    ##     summary(t) %>% print()
    ##     results.panelar$panelar <- t
    ##     ## ToDo - Can't access results.panelar$panelar directly gives error...
    ##     ##
    ##     ##        Error in switch(x$call$panelCorrMethod, none = "homoskedastic variance," :
    ##     ##          EXPR must be a length 1 vector
    ##     ##
    ##     ##        ...would be nice to resolve this, for now though return the
    ##     ##        individual components of interest
    ##     typeof(results.panelar$panelar) %>% print()
    ##     names(results.panelar$panelar) %>% print()
    ##     results.panelar$panelar %>% print()  ## <- This produces the above error
    ##     results.panelar$panelar %>% summary() ## <- As does this
    ##     results.panelar$panelar$coefficients %>% print()
    ##     results.panelar$panelar$residuals %>% print()
    ##     results.panelar$panelar$fitted.values %>% head() %>% print()
    ##     results.panelar$panelar$rank %>% print()
    ##     results.panelar$panelar$df.residual %>% print()
    ##     results.panelar$panelar$call %>% print()
    ##     results.panelar$panelar$terms %>% print()
    ##     results.panelar$panelar$panelar %>% print()
    ##     results.panelar$panelar$aliased %>% print()
    ##     results.panelar$panelar$na.action %>% print()
    ##     results.panelar$panelar$vcov %>% print()
    ##     results.panelar$panelar$r2 %>% print()
    ##     results.panelar$panelar$panelStructure %>% print()
    ##     ## Summary of model for printing along with R2 coefficient
    ##     summary(results.panelar$panelar) %>% print()
    ##     results.panelar$summary <- summary(t)
    ##     results.panelar$r2      <- t$r2
    ##     ## Extract coefficients to a data frame and add labels/terms
    ##     results.panelar$coefficients <- summary(results.panelar$panelar) %>%
    ##                                     coef() %>%
    ##                                     as.data.frame()
    ##     results.panelar$coefficients$term <- rownames(results.panelar$coefficients)
    ##     rownames(results.panelar$coefficients) <- NULL
    ##     results.panelar$coefficients$site <- .site
    ##     return(results.panelar)
    ## }
    extract_coefficients <- function(x,
                                     .site          = site,
                                     .indicator     = indicator,
                                     .sub.indicator = sub.indicator){
        coefficients <- summary(x) %>%
                                        coef() %>%
                                        as.data.frame()
        coefficients$term <- rownames(x)
        rownames(coefficients) <- NULL
        coefficients$site          <- .site
        coefficients$indicator     <- .indicator
        coefficients$sub.indicator <- .sub.indicator
        return(coefficients)

    }
    #######################################################################
    ## Model 1                                                           ##
    #######################################################################
    if(!is.null(model1)){
        ## Reformulate outcome and covariates
        formula.model1 <- reformulate(response = outcome,
                                      termlabels = model1)
        ## Subset data
        sites <- c('Bishop Auckland', 'Hartlepool', 'Hemel Hempstead', 'Newark', 'Rochdale')
        df1 <- filter(df.trust,
                      town %in% sites &
                      measure     == indicator,
                      sub.measure == sub.indicator)
        ## Generate time-series plot
        results$model1.ts.plot <- ggplot(data = df1,
                                         mapping = aes(x     = relative.month,
                                                       y     = value,
                                                       color = town)) +
            geom_line() +
            geom_vline(xintercept = 25, linetype = 4) +
            ## ToDo - Include other steps
            labs(list(title  = paste0(title1, title2),
                      x      = 'Month (Aligned)',
                      y      = 'N',
                      colour = 'Hospital'))
        ## Apply the user-specified theme
        if(!is.null(theme)){
            results$model1.ts.plot <- results$model1.ts.plot + theme
        }
        ## Perform analysis with panelAR in each site
        .df <- filter(df1,
                      town        == 'Bishop Auckland' &
                      measure     == indicator,
                      sub.measure == sub.indicator)
        results$model1.panelar.bishop <- panelAR(data     = .df,
                                                 formula  = formula.model1,
                                                 timeVar  = timevar,
                                                 panelVar = panel.trust,
                                                 autoCorr = autocorr,
                                                 panelCorrMethod = panelcorrmethod)
        ## results$model1.panelar.bishop %>% print()
        results$model1.panelar.bishop.coef <- extract_coefficients(x              = results$model1.panelar.bishop,
                                                                   .site          = 'Bishop Auckland',
                                                                   .indicator     = indicator,
                                                                   .sub.indicator = sub.indicator)
        results$model1.panelar.bishop.r2 <- results$model1.panelar.bishop$r2
        ## Summary table

    }
    #######################################################################
    ## Model 2                                                           ##
    #######################################################################
    ## if(!is.null(model2)){
    ##     ## Reformulate outcome and covariates
    ##     formula.model2 <- reformulate(response = outcome,
    ##                                   termlabels = model2)
    ##     ## Subset data
    ##     sites <- c('Bishop Auckland', 'Whitehaven',
    ##                'Hartlepool', 'Grimsby',
    ##                'Hemel Hempstead', 'Warwick',
    ##                'Newark', 'Southport',
    ##                'Rochdale', 'Rotherham')
    ##     df2 <- filter(df.trust, town %in% sites)
    ##     ## Generate time-series plot
    ##     results$model2.ts.plot <- ggplot(data = df2,
    ##                                      mapping = aes(x     = relative.month,
    ##                                                    y     = value,
    ##                                                    color = town)) +
    ##         geom_line() +
    ##         geom_vline(xintercept = 25, linetype = 4) +
    ##         ## ToDo - Include other steps
    ##         labs(list(title  = paste0(title1, title2),
    ##                   x      = 'Month (Aligned)',
    ##                   y      = 'N',
    ##                   colour = 'Hospital'))
    ##     ## Apply the user-specified theme
    ##     if(!is.null(theme)){
    ##         results$model2.ts.plot <- results$model2.ts.plot + theme
    ##     }
    ##     ## Perform analysis with panelAR in each
    ##     results$model2.panelar.bishop <- filter(df2,
    ##                                             town        == 'Bishop Auckland' &
    ##                                             measure     == indicator,
    ##                                             sub.measure == sub.indicator) %>%
    ##                                      closed_panelar(.site = 'Bishop Auckland',
    ##                                                     formula = formula.model2,
    ##                                                     .timeVar = time,
    ##                                                     .panelVar = panel.lsoa,
    ##                                                     .autoCorr = autocorr,
    ##                                                     .panelCorrMethod = panelcorrmethod)
    ##     ## Summary table

    ## }
    #######################################################################
    ## Return the results                                                ##
    #######################################################################
    return(results)
}
