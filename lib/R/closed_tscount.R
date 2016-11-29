#' Perform Regression on ClosED data
#'
#' @description Perform Negative Binomial Time Series Regression on ClosED data
#'
#' @details The ClosED study uses time-series with dummy indicators to
#' test for the impact of closing Emergency Departments on indicators of
#' performance.  This performs
#' Negative Binomial time-series regression analysis to account for atuo-correlation
#' using the \code{tscount} package.
#'
#' This function differs from \code{closed_regress} in that it runs one model on
#' all sites, returning the results for each model along with summary data frames
#' of all coefficients and forest plots.  Since the site to be analysed includes
#' all of them and the controls vary between model these do not need to be specified.
#' The covariates are coded internally depending on the model and are not therefore
#' specified by the user.
#'
#' @param df.lsoa Data frame at the LSOA level to analyse.
#' @param df.trust Data frame at the Trust level to analyse ideally cleaned of spurious datasets by \code{closed_clean()}.
#' @param indicator The performance indicator to assess.
#' @param sub.indicator The sub-measure performance indicator to assess.
#' @param steps List of steps (dummy variables) to include in time-series analysis.
#' @param panel.lsoa Variable that defines panels in LSOA level data (default is \code{lsoa} and shouldn't need changing).
#' @param panel.trust Variable that defines panels in Trust level data (default is \code{town} and shouldn't need changing).
#' @param time Variable defining time in both data sets (default is \code{relative.month} and shouldn't need changing).
#' @param outcome Outcome variable containing the counts (default is \code{value} and shouldn't need changing).
#' @param model1 Covariates to include in model 1.
#' @param model2 Covariates to include in model 2.
#' @param model3.1 Covariates to include in model 3.
#' @param model3b Covariates to include in model 8.
#' @param model4 Covariates to include in model 4.
#' @param model5 Covariates to include in model 5.
#' @param model6 Covariates to include in model 6.
#' @param model7 Covariates to include in model 7.
#' @param autocorr panelAR() option for handling auto-correlation, default is \code{ar1}.
#' @param panelcorrmethod panelAR() option for panel correction, default is \code{pcse}.
#' @param coefficients Determine which coefficients from the model are included in summary tables.  Setting to \code{closure} will return only terms that involve only the closure indicator (i.e. \code{closure} itself).  Other options include \code{town} for site specific terms (no interactions) and \code{closure.town} (the default) which includes all closure and town terms, both individually and from interactions.  Use \code{all} to get all terms returned or for closure, town and other steps use \code{all.steps}
#' @param complete.case Whether to use balanced panels.
#' @param weights option for \code{panelAR} weights (see \code{?panelAR} for options).
#' @param seq.times Logical whether to use \code{panelAR} \code{seq.times} option to ignore gaps.
#' @param rho.na.rm Logical operator passed to panelAR() for excluding panel specific autocorrelation when it can not be calculated.
#' @param return.df Logical operator of whether to return the subsetted/summarised data frame (useful for subsequent development).
#' @param return.model Logical operator of whether to return the fitted models (not currently working correctly).
#' @param return.residuals Logical oeprator of whether to return the residuals of the fitted model.
#' @param legend Logical operator of whether to include legends passed to \code{closed_ts_plot()}.
#' @param digits Number of digits to include in summary table of means/sd.
#'
#' @return A list of results depending on the options specified.
#'
#' @examples
#'
#' ## Run mortality analyses for Bishop Auckland and its matched site
#' ## producing time-series plot, step and dose models using both the
#' ## panelAR and prais package.
#'
#' ts.attendance <- closed_tscount(df       = ,)
#'
#'
#' @references
#'
#' @export
closed_tscount <- function(df.lsoa         = ed_attendances_by_mode_measure,
                          df.trust        = ed_attendances_by_mode_site_measure_clean,
                          indicator       = 'ed attendances',
                          sub.indicator   = 'any',
                          steps           = c('closure'),
                          fit.with        = 'both',
                          panel.lsoa      = 'lsoa',
                          panel.trust     = 'town',
                          timevar         = 'relative.month',
                          outcome         = 'value',
                          model1          = c('closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
                          model2          = c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
                          model3.1        = c('pooled.control * closure', 'town', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
                          model3.2        = c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
                          model4          = c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
                          model5          = c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
                          model6.1        = c('season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert', 'diff.time.to.ed'),
                          model6.2        = c('town', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert', 'diff.time.to.ed'),
                          model7          = c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert', 'diff.time.to.ed'),
                           ...){
    #######################################################################
    ## Set up (results, formula, renaming variables)                     ##
    #######################################################################
    ## Initialise results list for returning everything
    results <- list()
    ## ## print("Debug 1")
    ## Obtain the levels of town and group and site type the number of observations
    ## within each to control subsequent analyses
    ## ToDo - Think how to loop over each of these groups testing each of the models
    ##        This would make the function very general and flexible for future use.
    town.group <- filter(df.trust, measure == indicator & sub.measure == sub.indicator & !is.na(value)) %>%
                          group_by(town, group, site.type) %>%
                          summarise(n = n())
    ## ToDo - For now fill in levels of town/group that are missing, will have to have
    ##        this provided as an additional argument when generalising the function
    all.town <- data.frame(c('Bishop Auckland', 'Salford', 'Scarborough', 'Whitehaven', 'Basingstoke', 'Hemel Hempstead', 'Warwick', 'Yeovil', 'Carlisle', 'Newark', 'Salisbury', 'Southport', 'Rochdale', 'Rotherham', 'Scunthorpe', 'Wansbeck', 'Blackburn', 'Grimsby', 'Hartlepool', 'Wigan'),
                           c('Bishop Auckland General Hospital', 'Bishop Auckland General Hospital', 'Bishop Auckland General Hospital', 'Bishop Auckland General Hospital', 'Hemel Hempstead Hospital', 'Hemel Hempstead Hospital', 'Hemel Hempstead Hospital', 'Hemel Hempstead Hospital', 'Newark Hospital', 'Newark Hospital', 'Newark Hospital', 'Newark Hospital', 'Rochdale Infirmary', 'Rochdale Infirmary', 'Rochdale Infirmary', 'Rochdale Infirmary', 'University Hospital of Hartlepool', 'University Hospital of Hartlepool', 'University Hospital of Hartlepool', 'University Hospital of Hartlepool'))
    names(all.town) <- c('town', 'group')
    town.group <- merge(town.group,
                        all.town,
                        by     = c('town', 'group'),
                        all    = TRUE)
    town.group <- mutate(town.group,
                         n = ifelse(is.na(n), 0, n))
    ## print("Debug 2")
    names(df.lsoa)  <- names(df.lsoa) %>%
                       gsub("_", ".", x = .)
    names(df.trust) <- names(df.trust) %>%
                       gsub("_", ".", x = .)
    ## Convert to data frame, selecting only the specified outcome and convert
    ## town to factor so that it can be releveled as required
    ## print("Debug 3")
    df.lsoa  <- as.data.frame(df.lsoa) %>%
                dplyr::filter(measure == indicator,
                              sub.measure == sub.indicator)
    df.lsoa$town <- factor(df.lsoa$town)
    df.trust <- as.data.frame(df.trust) %>%
                dplyr::filter(measure == indicator,
                              sub.measure == sub.indicator)
    df.trust$town <- factor(df.trust$town)

}
