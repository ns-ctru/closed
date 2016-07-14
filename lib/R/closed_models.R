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
#' @param model7 Covariates to include in model 7.
#' @param model8 Covariates to include in model 8.
#' @param autocorr panelAR() option for handling auto-correlation, default is \code{ar1}.
#' @param panelcorrmethod panelAR() option for panel correction, default is \code{pcse}.
#' @param coefficients Determine which coefficients from the model are included in summary tables.  Setting to \code{closure} will return only terms that involve only the closure indicator (i.e. \code{closure} itself).  Other options include \code{town} for site specific terms (no interactions) and \code{closure.town} (the default) which includes all closure and town terms, both individually and from interactions.  Use \code{all} to get all terms returned or for closure, town and other steps use \code{all.steps}
#' @param weights option for \code{panelAR} weights (see \code{?panelAR} for options).
#' @param seq.times Logical whether to use \code{panelAR} \code{seq.times} option to ignore gaps.
#' @param rho.na.rm Logical operator passed to panelAR() for excluding panel specific autocorrelation when it can not be calculated.
#' @param plot Generate time-series plot.
#' @param common.y Generate all plots with a common y-axis range.
#' @param theme GGplot2 theme to use (only relevant if \code{plot = TRUE}).
#' @param return.df Logical operator of whether to return the subsetted/summarised data frame (useful for subsequent development).
#' @param return.model Logical operator of whether to return the fitted models (not currently working correctly).
#' @param return.residuals Logical oeprator of whether to return the residuals of the fitted model.
#' @param return.residuals.plot Logical operator of whether to return a plot of residuals from the fitted model(s).
#' @param join.line Logical operator of whether to join missing data points on plots.
#' @param legend Logical operator of whether to include legends passed to \code{closed_ts_plot()}.
#' @param rho.na.rm Logical operator passed to panelAR() for excluding panel specific autocorrelation when it can not be calculated.
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
                          model3          = c('pooled.control * closure', 'town', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
                          model4          = c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
                          model5          = c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
                          model6          = c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert', 'diff.time.to.ed'),
                          model7          = c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert', 'diff.time.to.ed'),
                          model8          = c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
                          autocorr        = 'ar1',
                          panelcorrmethod = 'pcse',
                          coefficients    = 'closure.town',
                          weights         = '',
                          seq.times       = TRUE,
                          rho.na.rm       = FALSE,
                          plot            = TRUE,
                          common.y        = TRUE,
                          theme           = theme_bw(),
                          return.df       = FALSE,
                          return.model    = TRUE,
                          return.residuals = FALSE,
                          return.residuals.plot = FALSE,
                          join.line        = TRUE,
                          legend           = FALSE,
                          ...){
    #######################################################################
    ## Set up (results, formula, renaming variables)                     ##
    #######################################################################
    ## Initialise results list for returning everything
    results <- list()
    ## 2016-05-24 - For a small number of outcomes there is no sub-indicator
    ##              and it is therefore missing.  In order to work with this
    ##              function such missing values are therefore replaced with
    ##              the main indicator which is supplied as the sub.indicator
    ##              argument
    ## which.df <- substitute(df.lsoa) %>% deparse()
    ## print("Debug 1")
    ## if(indicator == 'unnecessary ed attendances'){
    ##     df.lsoa$sub_measure  <- ifelse(is.na(df.lsoa$sub_measure), 'all', df.lsoa$sub_measure)
    ##     df.trust$sub_measure <- ifelse(is.na(df.trust$sub_measure), 'all', df.trust$sub_measure)
    ## }
    ## if(indicator == 'all emergency admissions'){
    ##     df.lsoa$sub_measure  <- ifelse(is.na(df.lsoa$sub_measure), 'all', df.lsoa$sub_measure)
    ##     df.trust$sub_measure <- ifelse(is.na(df.trust$sub_measure), 'all', df.trust$sub_measure)
    ## }
    ## Convert variable names for ease of typing within this function
    ## (ESS artefact, hitting underscore inserts '<-' so lots of underscores are
    ## tedious to type)
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
    ## df.steps <- as.data.frame(df.steps)
    ## names(df.steps) <- names(df.steps) %>%
    ##                    gsub("_", ".", x = .)
    ## Conditionally select range for y-axis, MUST do this BEFORE subsetting
    ## data so that it is common across all outcomes for the given indicator
    ## print("Debug 4")
    if(common.y == TRUE){
        ## print('Dim LSOA')
        ## dim(df.lsoa) %>% print()
        ## print('Dim Trust')
        ## dim(df.trust) %>% print()
        df.lsoa.max  <-  max(as.numeric(df.lsoa$value), na.rm = TRUE)
        df.trust.max <-  max(as.numeric(df.trust$value), na.rm = TRUE)
        y.max <- max(df.lsoa.max, df.trust.max) %>%
                 round(-2)
    }
    #######################################################################
    ## Derive a seasonal indicator (really this should be on the data    ##
    ## preparation side but Tony is already doing tons)                  ##
    #######################################################################
    ## print("Debug 5")
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
    ## Add a dummy 'step' for closure                                    ##
    #######################################################################
    ## 2016-05-24 - Post meeting with Jon, this should be 0/1 for _all_ sites not
    ##              just intervention ones
    ## print("Debug 6")
    df.lsoa$closure  <- ifelse(df.lsoa$relative.month  > 24, 1, 0)
    df.trust$closure <- ifelse(df.trust$relative.month > 24, 1, 0)
    #######################################################################
    ## Add dummy for other 'steps'                                       ##
    ##                                                                   ##
    ## See list from e.l.knowles@sheffield.ac.uk at...                   ##
    ##                                                                   ##
    ## https://goo.gl/TlhfCF                                             ##
    ##                                                                   ##
    #######################################################################
    ## print("Debug 7")
    df.lsoa <- mutate(df.lsoa,
                      nhs111 = ifelse((town == 'Bishop Auckland' & relative.month >= 35) |
                                      (town == 'Southport' & relative.month >= 48) |
                                      ## ToDo - Uncomment once confirmed and revised dates available
                                      (town == 'Rochdale' & relative.month >= 48) |
                                      (town == 'Rotherham' & relative.month >= 48) |
                                      (town == 'Hartlepool' & relative.month >= 45) |
                                      (town == 'Grimsby' & relative.month >= 16),
                                      1, 0),
                      ambulance.divert = ifelse(town == 'Rochdale' & relative.month >= 17, 1, 0),
                      other.centre = ifelse((town == 'Hemel Hempstead' & relative.month >= 20) |
                                            (town == 'Newark' & relative.month >= 3) |
                                            (town == 'Rochdale' & relative.month >= 11) |
                                            (town == 'Hartlepool' & relative.month >= 22),
                                            1, 0)
                      )
    df.trust <- mutate(df.trust,
                       nhs111 = ifelse((town == 'Bishop Auckland' & relative.month >= 35) |
                                       (town == 'Southport' & relative.month >= 48) |
                                       ## ToDo - Uncomment once confirmed and revised dates available
                                       (town == 'Rochdale' & relative.month >= 48) |
                                       (town == 'Rotherham' & relative.month >= 48) |
                                       (town == 'Hartlepool' & relative.month >= 45) |
                                       (town == 'Grimsby' & relative.month >= 16),
                                       1, 0),
                       ambulance.divert = ifelse(town == 'Rochdale' & relative.month >= 17, 1, 0),
                       other.centre = ifelse((town == 'Hemel Hempstead' & relative.month >= 20) |
                                             (town == 'Newark' & relative.month >= 3) |
                                             (town == 'Rochdale' & relative.month >= 11) |
                                             (town == 'Hartlepool' & relative.month >= 22),
                                             1, 0)
                       )
    #######################################################################
    ## Internal functions (to save typing)                               ##
    #######################################################################
    extract_coefficients <- function(x,
                                     .site          = site,
                                     .indicator     = indicator,
                                     .sub.indicator = sub.indicator){
        ## Extract coefficients from a given model
        coefficients <- summary(x) %>%
                        coef() %>%
                        as.data.frame()
        coefficients$term <- rownames(coefficients)
        rownames(coefficients) <- NULL
        coefficients$site          <- .site
        coefficients$indicator     <- .indicator
        coefficients$sub.indicator <- .sub.indicator
        coefficients$r2            <- x$r2
        return(coefficients)
    }
    combine_coefficients <- function(bishop.coef     = results$model1.panelar.bishop.coef,
                                     hartlepool.coef = results$model1.panelar.hartlepool.coef,
                                     hemel.coef      = results$model1.panelar.hemel.coef,
                                     newark.coef     = results$model1.panelar.newark.coef,
                                     rochdale.coef   = results$model1.panelar.rochdale.coef,
                                     all.coef        = NULL,
                                     return.coef     = coefficients){
        ## List of results
        coef <- list()
        ## Combine coefficients, derive CIs and derive tidy df for output
        .coef <- rbind(bishop.coef,
                       hartlepool.coef,
                       hemel.coef,
                       newark.coef,
                       rochdale.coef,
                       all.coef)
        ## Rename
        names(.coef) <- c('est', 'se', 't', 'p', 'term', 'site', 'indicator', 'sub.indicator', 'r2')
        .coef$r2 <- formatC(.coef$r2, digits = 3, format = 'f')
        ## Extract and reshape the r2
        .r2 <- dplyr::select(.coef, indicator, sub.indicator, site, r2) %>%
              unique() %>%
              melt(id = c('indicator', 'sub.indicator', 'site')) %>%
              dcast(indicator + sub.indicator + variable ~ site)
        ## Extract, format and reshape the estimates, se and p-values
        .coef$out <- paste0(formatC(.coef$est, digits = 3, format = 'f'),
                           " (",
                           formatC(.coef$se, digits = 3, format = 'f'),
                           ") p = ",
                           formatC(.coef$p, digits = 3, format = 'e'))
        .coef <- dplyr::select(.coef, indicator, sub.indicator, term, site, out) %>%
                 melt(id = c('indicator', 'sub.indicator', 'site', 'term')) %>%
                 dcast(indicator + sub.indicator + term ~ site + variable)
        ## Conditionally remove the coefficients that are not of interest
        if(return.coef == 'closure'){
            .coef <- dplyr::filter(.coef, grepl('closure', term))
        }
        else if(return.coef == 'town'){
            .coef <- dplyr::filter(.coef, grepl('town', term))
        }
        else if(return.coef == 'closure.town'){
            .coef <- dplyr::filter(.coef, grepl('closure', term) | grepl('town', term))
        }
        ## Not really necessary, but it makes the code clear
        else if(return.coef == 'all.steps'){
            .coef <- dplyr::filter(.coef, grepl('closure', term) | grepl('town', term) | grepl('nhs111', term) | grepl('ambulance.divert', term) | grepl('other.closure', term))
        }
        ## Not really necessary, but it makes the code clear
        else if(return.coef == 'all'){
            .coef <- .coef
        }
        ## Format the term label for interactions between site and town
        .coef <- within(.coef, {
                        term[term == 'townBasingstoke'] <- 'Basingstoke'
                        term[term == 'townBasingstoke:closure'] <- 'Basingstoke x Closure'
                        term[term == 'townBishop Auckland'] <- 'Bishop Auckland'
                        term[term == 'townBishop Auckland:closure'] <- 'Bishop Auckland x Closure'
                        term[term == 'pooled.controlBishop Auckland:closure'] <- 'Bishop Auckland x Closure'
                        term[term == 'townBlackburn'] <- 'Blackburn'
                        term[term == 'townBlackburn:closure'] <- 'Blackburn x Closure'
                        term[term == 'townCarlisle'] <- 'Carlisle'
                        term[term == 'townCarlisle:closure'] <- 'Carlisle x Closure'
                        term[term == 'townGrimsby'] <- 'Grimsby'
                        term[term == 'townGrimsby:closure'] <- 'Grimsby x Closure'
                        term[term == 'townHartlepool'] <- 'Hartlepool'
                        term[term == 'townHartlepool:closure'] <- 'Hartlepool x Closure'
                        term[term == 'pooled.controlHartlepool:closure'] <- 'Hartlepool x Closure'
                        term[term == 'townHemel Hempstead'] <- 'Hemel Hempstead'
                        term[term == 'townHemel Hempstead:closure'] <- 'Hemel Hempstead x Closure'
                        term[term == 'pooled.controlHemel Hempstead:closure'] <- 'Hemel Hempstead x Closure'
                        term[term == 'townNewark']  <- 'Newark'
                        term[term == 'townNewark:closure']  <- 'Newark x Closure'
                        term[term == 'pooled.controlNewark:closure'] <- 'Newark x Closure'
                        term[term == 'townRochdale']  <- 'Rochdale'
                        term[term == 'townRochdale:closure']  <- 'Rochdale x Closure'
                        term[term == 'pooled.controlRochdale:closure'] <- 'Rochdale x Closure'
                        term[term == 'townRotherham'] <- 'Rotherham'
                        term[term == 'townRotherham:closure'] <- 'Rotherham x Closure'
                        term[term == 'townSalford'] <- 'Salford'
                        term[term == 'townSalford:closure'] <- 'Salford x Closure'
                        term[term == 'townSalisbury'] <- 'Salisbury'
                        term[term == 'townSalisbury:closure'] <- 'Salisbury x Closure'
                        term[term == 'townScarborough'] <- 'Scarborough'
                        term[term == 'townScarborough:closure'] <- 'Scarborough x Closure'
                        term[term == 'townScunthorpe'] <- 'Scunthorpe'
                        term[term == 'townScunthorpe:closure'] <- 'Scunthorpe x Closure'
                        term[term == 'townSouthport'] <- 'Southport'
                        term[term == 'townSouthport:closure'] <- 'Southport x Closure'
                        term[term == 'townWansbeck'] <- 'Wansbeck'
                        term[term == 'townWansbeck:closure'] <- 'Wansbeck x Closure'
                        term[term == 'townWarwick'] <- 'Warwick'
                        term[term == 'townWarwick:closure'] <- 'Warwick x Closure'
                        term[term == 'townWhitehaven'] <- 'Whitehaven'
                        term[term == 'townWhitehaven:closure'] <- 'Whitehaven x Closure'
                        term[term == 'townWigan'] <- 'Wigan'
                        term[term == 'townWigan:closure'] <- 'Wigan x Closure'
                        term[term == 'townYeovil'] <- 'Yeovil'
                        term[term == 'townYeovil:closure'] <- 'Yeovil x Closure'
                        term[term == 'nhs111'] <- 'NHS 111'
                        term[term == 'ambulance.divert'] <- 'Ambulances Diverted'
                        term[term == 'other.centre'] <- 'Other Medical Centre'
                        term[term == 'closure'] <- 'ED Closure'
                        term[term == 'relative.month'] <- 'Time (Month)'
                        term[term == 'season'] <- 'Season'
                        term[term == 'diff.time.to.ed'] <- 'Change in Time to ED'
        })
        ## Combine with r2
        names(.coef) <- gsub("_out", "", names(.coef))
        names(.r2) <- gsub("variable", "term", names(.r2))
        coef$coef <- rbind(.coef, .r2)
        rm(.r2)
        if(is.null(all.coef)){
            names(coef$coef) <- c('Indicator', 'Subindicator', 'Term', 'Bishop Auckland', 'Hartlepool', 'Hemel Hempstead', 'Newark', 'Rochdale')
        }
        else{
            names(coef$coef) <- c('Indicator', 'Subindicator', 'Term', 'All', 'Bishop Auckland', 'Hartlepool', 'Hemel Hempstead', 'Newark', 'Rochdale')
        }
        ## Derive a caption for the table
        coef$caption <- paste0('Comparison of coefficients across sites.  Each cell contains a point estimate followed by the standard error (in brackets) and the associated p-value (in scientific format due to some values being very small).')
        return(coef)
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
                      measure     == indicator &
                      sub.measure == sub.indicator)
        ## Generate time-series plot
        results$model1.ts.plot <- closed_ts_plot(df = df1,
                                                 sites = sites,
                                                 indicator = indicator,
                                                 sub.indicator = sub.indicator,
                                                 steps = TRUE,
                                                 facet = FALSE,
                                                 tidy  = TRUE,
                                                 join  = join.line,
                                                 legend = legend)
        ##################################################
        ## Model 1 - Bishop Auckland                    ##
        ##################################################
        t <- filter(df1,
                    town        == 'Bishop Auckland')
        ## dim(t) %>% print()
        ## head(t) %>% print()
        ## table(t$value) %>% print()
        ## return(t)
        if(nrow(t) != 0){
            model1.panelar.bishop <- panelAR(data      = t,
                                             formula   = formula.model1,
                                             timeVar   = timevar,
                                             panelVar  = panel.trust,
                                             autoCorr  = autocorr,
                                             panelCorrMethod = 'pcse',
                                             seq.times = seq.times,
                                             rho.na.rm = rho.na.rm)
            results$model1.panelar.bishop.coef <- extract_coefficients(x              = model1.panelar.bishop,
                                                                       .site          = 'Bishop Auckland',
                                                                       .indicator     = indicator,
                                                                       .sub.indicator = sub.indicator)
            results$model1.panelar.bishop.r2 <- model1.panelar.bishop$r2
        }
        ##################################################
        ## Model 1 - Hartlepool                         ##
        ##################################################
        t <- filter(df1,
                    town        == 'Hartlepool')
        if(nrow(t) != 0){
            model1.panelar.hartlepool <- panelAR(data     = t,
                                                 formula  = formula.model1,
                                                 timeVar  = timevar,
                                                 panelVar = panel.trust,
                                                 autoCorr = autocorr,
                                                 panelCorrMethod = 'pcse',
                                                 seq.times = seq.times,
                                                 rho.na.rm = rho.na.rm)
            results$model1.panelar.hartlepool.coef <- extract_coefficients(x              = model1.panelar.hartlepool,
                                                                           .site          = 'Hartlepool',
                                                                           .indicator     = indicator,
                                                                           .sub.indicator = sub.indicator)
            results$model1.panelar.hartlepool.r2 <- model1.panelar.hartlepool$r2
        }
        ##################################################
        ## Model 1 - Hemel Hempstead                    ##
        ##################################################
        t <- filter(df1,
                    town        == 'Hemel Hempstead')
        if(nrow(t) >= 0){
            model1.panelar.hemel <- panelAR(data     = t,
                                            formula  = formula.model1,
                                            timeVar  = timevar,
                                            panelVar = panel.trust,
                                            autoCorr = autocorr,
                                            panelCorrMethod = 'pcse',
                                            seq.times = seq.times,
                                            rho.na.rm = rho.na.rm)
            results$model1.panelar.hemel.coef <- extract_coefficients(x              = model1.panelar.hemel,
                                                                      .site          = 'Hemel Hempstead',
                                                                      .indicator     = indicator,
                                                                      .sub.indicator = sub.indicator)
            results$model1.panelar.hemel.r2 <- model1.panelar.hemel$r2
        }
        ##################################################
        ## Model 1 - Newark                             ##
        ##################################################
        t <- filter(df1,
                    town        == 'Newark')
        if(nrow(t) >= 0){
            model1.panelar.newark <- panelAR(data     = t,
                                             formula  = formula.model1,
                                             timeVar  = timevar,
                                             panelVar = panel.trust,
                                             autoCorr = autocorr,
                                             panelCorrMethod = 'pcse',
                                             seq.times = seq.times,
                                             rho.na.rm = rho.na.rm)
            results$model1.panelar.newark.coef <- extract_coefficients(x              = model1.panelar.newark,
                                                                       .site          = 'Newark',
                                                                       .indicator     = indicator,
                                                                       .sub.indicator = sub.indicator)
            results$model1.panelar.newark.r2 <- model1.panelar.newark$r2
        }
        ##################################################
        ## Model 1 - Rochdale                           ##
        ##################################################
        t <- filter(df1,
                    town        == 'Rochdale')
        if(nrow(t) >= 0){
            model1.panelar.rochdale <- panelAR(data     = t,
                                               formula  = formula.model1,
                                               timeVar  = timevar,
                                               panelVar = panel.trust,
                                               autoCorr = autocorr,
                                               panelCorrMethod = 'pcse',
                                               seq.times = seq.times,
                                               rho.na.rm = rho.na.rm)
            results$model1.panelar.rochdale.coef <- extract_coefficients(x            = model1.panelar.rochdale,
                                                                         .site          = 'Rochdale',
                                                                         .indicator     = indicator,
                                                                         .sub.indicator = sub.indicator)
            results$model1.panelar.rochdale.r2 <- model1.panelar.rochdale$r2
        }
        ## Summary table
        results$model1.panelar <- combine_coefficients(bishop.coef     = results$model1.panelar.bishop.coef,
                                                       hartlepool.coef = results$model1.panelar.hartlepool.coef,
                                                       hemel.coef      = results$model1.panelar.hemel.coef,
                                                       newark.coef     = results$model1.panelar.newark.coef,
                                                       rochdale.coef   = results$model1.panelar.rochdale.coef)
        ## Forest plot
        results$model1.forest <- closed_forest(df.list       = list(results$model1.panelar.bishop.coef,
                                                                    results$model1.panelar.hartlepool.coef,
                                                                    results$model1.panelar.hemel.coef,
                                                                    results$model1.panelar.newark.coef,
                                                                    results$model1.panelar.rochdale.coef),
                                               plot.term     = c('closure'),
                                               facet.outcome = FALSE,
                                               title         = paste0('Model 1 : ',
                                                                      indicator,
                                                                      ' (',
                                                                      sub.indicator,
                                                                      ')'),
                                               theme         = theme_bw())
        ## Return model objects if requested
        if(return.model == TRUE){
            results$model1.panelar.bishop     <- model1.panelar.bishop
            results$model1.panelar.hartlepool <- model1.panelar.hartlepool
            results$model1.panelar.hemel      <- model1.panelar.hemel
            results$model1.panelar.newark     <- model1.panelar.newark
            results$model1.panelar.rochdale   <- model1.panelar.rochdale
        }
        if(return.df == TRUE){
            results$model1.df <- df1
        }
        if(return.residuals == TRUE){
            results$model1.panelar.residuals.bishop     <- summary(model1.panelar.bishop)$residuals
            results$model1.panelar.residuals.hartlepool <- summary(model1.panelar.hartlepool)$residuals
            results$model1.panelar.residuals.hemel      <- summary(model1.panelar.hemel)$residuals
            results$model1.panelar.residuals.newark     <- summary(model1.panelar.newark)$residuals
            results$model1.panelar.residuals.rochdale   <- summary(model1.panelar.rochdale)$residuals
        }
        if(return.residuals.plot == TRUE){
            results$model1.panelar.residuals.plot.bishop     <- summary(model1.panelar.bishop)$residuals %>% plot()
            results$model1.panelar.residuals.plot.hartlepool <- summary(model1.panelar.hartlepool)$residuals %>% plot()
            results$model1.panelar.residuals.plot.hemel      <- summary(model1.panelar.hemel)$residuals %>% plot()
            results$model1.panelar.residuals.plot.newark     <- summary(model1.panelar.newark)$residuals %>% plot()
            results$model1.panelar.residuals.plot.rochdale   <- summary(model1.panelar.rochdale)$residuals %>% plot()
        }
        ## Remove clutter
        rm(df1)
    }
    #######################################################################
    ## Model 2                                                           ##
    #######################################################################
    if(!is.null(model2)){
        ## Reformulate outcome and covariates
        formula.model2 <- reformulate(response = outcome,
                                      termlabels = model2)
        ## Subset data
        sites <- c('Bishop Auckland', 'Whitehaven',
                   'Hartlepool', 'Grimsby',
                   'Hemel Hempstead', 'Warwick',
                   'Newark', 'Southport',
                   'Rochdale', 'Rotherham')
        df2 <- filter(df.trust, town %in% sites &
                      measure     == indicator &
                      sub.measure == sub.indicator)
        ## Generate time-series plot
        df2$group <- paste0('Cohort : ', df2$group)
        results$model2.ts.plot <- closed_ts_plot(df = df2,
                                                 sites = sites,
                                                 indicator = indicator,
                                                 sub.indicator = sub.indicator,
                                                 steps = TRUE,
                                                 facet = TRUE,
                                                 tidy  = TRUE,
                                                 join  = join.line,
                                                 legend = legend)
        ## Perform analysis with panelAR in each
        ##################################################
        ## Model 2 - Bishop Auckland                    ##
        ##################################################
        df2$town <- relevel(df2$town, ref = 'Whitehaven')
        t <- filter(df2,
                    town == 'Bishop Auckland' |
                    town == 'Whitehaven')
        if(nrow(t) > 0){
            model2.panelar.bishop <- panelAR(data     = t,
                                             formula  = formula.model2,
                                             timeVar  = timevar,
                                             panelVar = panel.trust,
                                             autoCorr = autocorr,
                                             panelCorrMethod = 'pcse',
                                             seq.times = seq.times,
                                             rho.na.rm = rho.na.rm)
            results$model2.panelar.bishop.coef <- extract_coefficients(x              = model2.panelar.bishop,
                                                                       .site          = 'Bishop Auckland',
                                                                       .indicator     = indicator,
                                                                       .sub.indicator = sub.indicator)
            results$model2.panelar.bishop.r2 <- model2.panelar.bishop$r2
        }
        ##################################################
        ## Model 2 - Hartlepool                         ##
        ##################################################
        df2$town <- relevel(df2$town, ref = 'Grimsby')
        t <- filter(df2,
                    town == 'Hartlepool' |
                    town == 'Grimsby')
        if(nrow(t) > 0){
            model2.panelar.hartlepool <- panelAR(data     = t,
                                                 formula  = formula.model2,
                                                 timeVar  = timevar,
                                                 panelVar = panel.trust,
                                                 autoCorr = autocorr,
                                                 panelCorrMethod = 'pcse',
                                                 seq.times = seq.times,
                                                 rho.na.rm = rho.na.rm)
            results$model2.panelar.hartlepool.coef <- extract_coefficients(x             = model2.panelar.hartlepool,
                                                                           .site          = 'Hartlepool',
                                                                           .indicator     = indicator,
                                                                           .sub.indicator = sub.indicator)
            results$model2.panelar.hartlepool.r2 <- model2.panelar.hartlepool$r2
        }
        ##################################################
        ## Model 2 - Hemel Hempstead                    ##
        ##################################################
        df2$town <- relevel(df2$town, ref = 'Warwick')
        t <- filter(df2,
                    town == 'Hemel Hempstead' |
                    town == 'Warwick')
        if(nrow(t) > 0){
            model2.panelar.hemel <- panelAR(data     = t,
                                            formula  = formula.model2,
                                            timeVar  = timevar,
                                            panelVar = panel.trust,
                                            autoCorr = autocorr,
                                            panelCorrMethod = 'pcse',
                                            seq.times = seq.times,
                                            rho.na.rm = rho.na.rm)
            results$model2.panelar.hemel.coef <- extract_coefficients(x              = model2.panelar.hemel,
                                                                      .site          = 'Hemel Hempstead',
                                                                      .indicator     = indicator,
                                                                      .sub.indicator = sub.indicator)
            results$model2.panelar.hemel.r2 <- model2.panelar.hemel$r2
        }
        ##################################################
        ## Model 2 - Newark                             ##
        ##################################################
        df2$town <- relevel(df2$town, ref = 'Southport')
        t <- filter(df2,
                    town == 'Newark' |
                    town == 'Southport')
        if(nrow(t) > 0){
            model2.panelar.newark <- panelAR(data     = t,
                                             formula  = formula.model2,
                                             timeVar  = timevar,
                                             panelVar = panel.trust,
                                             autoCorr = autocorr,
                                             panelCorrMethod = 'pcse',
                                             seq.times = seq.times,
                                             rho.na.rm = rho.na.rm)
            results$model2.panelar.newark.coef <- extract_coefficients(x              = model2.panelar.newark,
                                                                       .site          = 'Newark',
                                                                   .indicator     = indicator,
                                                                   .sub.indicator = sub.indicator)
            results$model2.panelar.newark.r2 <- model2.panelar.newark$r2
        }
        ##################################################
        ## Model 2 - Rochdale                           ##
        ##################################################
        df2$town <- relevel(df2$town, ref = 'Rotherham')
        t <- filter(df2,
                    town == 'Rochdale' |
                    town == 'Rotherham')
        if(nrow(t) > 0){
            model2.panelar.rochdale <- panelAR(data     = t,
                                               formula  = formula.model2,
                                               timeVar  = timevar,
                                               panelVar = panel.trust,
                                               autoCorr = autocorr,
                                               panelCorrMethod = 'pcse',
                                               seq.times = seq.times,
                                               rho.na.rm = rho.na.rm)
            results$model2.panelar.rochdale.coef <- extract_coefficients(x            = model2.panelar.rochdale,
                                                                         .site          = 'Rochdale',
                                                                         .indicator     = indicator,
                                                                   .sub.indicator = sub.indicator)
            results$model2.panelar.rochdale.r2 <- model2.panelar.rochdale$r2
        }
        ## Summary table
        results$model2.panelar <- combine_coefficients(bishop.coef     = results$model2.panelar.bishop.coef,
                                                       hartlepool.coef = results$model2.panelar.hartlepool.coef,
                                                       hemel.coef      = results$model2.panelar.hemel.coef,
                                                       newark.coef     = results$model2.panelar.newark.coef,
                                                       rochdale.coef   = results$model2.panelar.rochdale.coef)
        ## Forest plot
        results$model2.forest <- closed_forest(df.list       = list(results$model2.panelar.bishop.coef,
                                                                    results$model2.panelar.hartlepool.coef,
                                                                    results$model2.panelar.hemel.coef,
                                                                    results$model2.panelar.newark.coef,
                                                                    results$model2.panelar.rochdale.coef),
                                               plot.term     = c('closure'),
                                               facet.outcome = FALSE,
                                               title         = paste0('Model 2 : ',
                                                                      indicator,
                                                                      ' (',
                                                                      sub.indicator,
                                                                      ')'),
                                               theme         = theme_bw())
        ## Return model objects if requested
        if(return.model == TRUE){
            results$model2.panelar.bishop     <- model2.panelar.bishop
            results$model2.panelar.hartlepool <- model2.panelar.hartlepool
            results$model2.panelar.hemel      <- model2.panelar.hemel
            results$model2.panelar.newark     <- model2.panelar.newark
            results$model2.panelar.rochdale   <- model2.panelar.rochdale
        }
        if(return.df == TRUE){
            results$model2.df <- df2
        }
        if(return.residuals == TRUE){
            results$model2.panelar.residuals.bishop     <- summary(model2.panelar.bishop)$residuals
            results$model2.panelar.residuals.hartlepool <- summary(model2.panelar.hartlepool)$residuals
            results$model2.panelar.residuals.hemel      <- summary(model2.panelar.hemel)$residuals
            results$model2.panelar.residuals.newark     <- summary(model2.panelar.newark)$residuals
            results$model2.panelar.residuals.rochdale   <- summary(model2.panelar.rochdale)$residuals
        }
        if(return.residuals.plot == TRUE){
            results$model2.panelar.residuals.plot.bishop     <- summary(model2.panelar.bishop)$residuals %>% plot()
            results$model2.panelar.residuals.plot.hartlepool <- summary(model2.panelar.hartlepool)$residuals %>% plot()
            results$model2.panelar.residuals.plot.hemel      <- summary(model2.panelar.hemel)$residuals %>% plot()
            results$model2.panelar.residuals.plot.newark     <- summary(model2.panelar.newark)$residuals %>% plot()
            results$model2.panelar.residuals.plot.rochdale   <- summary(model2.panelar.rochdale)$residuals %>% plot()
        }
        ## Remove clutter
        rm(df2)
    }
    #######################################################################
    ## Model 3                                                         ##
    #######################################################################
    if(!is.null(model3)){
        ## Reformulate outcome and covariates
        formula.model3 <- reformulate(response = outcome,
                                      termlabels = model3)
        ## Subset data
        df3 <- filter(df.trust,
                      measure     == indicator &
                      sub.measure == sub.indicator)
        ## Define group pooling for controls (but DON'T pool the data!)
        df3 <- mutate(df3,
                      pooled.control = ifelse(site.type %in% c('matched control', 'pooled control'), 'Control', town))
        df3 <- mutate(df3,
                      pooled.control = ifelse(pooled.control == 2, 'Bishop Auckland', pooled.control),
                      pooled.control = ifelse(pooled.control == 6, 'Harltepool', pooled.control),
                      pooled.control = ifelse(pooled.control == 7, 'Hemel Hempstead', pooled.control),
                      pooled.control = ifelse(pooled.control == 8, 'Newark', pooled.control),
                      pooled.control = ifelse(pooled.control == 9, 'Rochdale', pooled.control))
        df3$pooled.control <- factor(df3$pooled.control)
        ## Set reference group for pooled controls
        df3$pooled.control <- relevel(df3$pooled.control, ref = 'Control')
        ## Generate time-series plot
        df3$group <- paste0('Cohort : ', df3$group)
        sites <- c('Basingstoke', 'Bishop Auckland', 'Blackburn', 'Carlisle', 'Grimsby', 'Hartlepool', 'Hemel Hempstead', 'Newark', 'Rochdale', 'Rotherham', 'Salford', 'Salisbury', 'Scarborough', 'Scunthorpe', 'Southport', 'WansbeckWarwick', 'Whitehaven', 'Wigan', 'Yeovil')
        results$model3.ts.plot <- closed_ts_plot(df = df3,
                                                 sites = sites,
                                                 indicator = indicator,
                                                 sub.indicator = sub.indicator,
                                                 steps = TRUE,
                                                 facet = TRUE,
                                                 tidy  = TRUE,
                                                 join  = join.line,
                                                 legend = legend)
        ## Perform analysis with panelAR in each
        ##################################################
        ## Bishop Auckland                              ##
        ##################################################
        df3$town <- relevel(df3$town, ref = 'Whitehaven')
        model3.panelar.bishop <- filter(df3,
                                        group == 'Cohort : Bishop Auckland General Hospital') %>%
                                 panelAR(formula  = formula.model3,
                                         timeVar  = timevar,
                                         panelVar = panel.trust,
                                         autoCorr = autocorr,
                                         panelCorrMethod = 'pcse',
                                         seq.times = seq.times,
                                         rho.na.rm = rho.na.rm)
        results$model3.panelar.bishop.coef <- extract_coefficients(x              = model3.panelar.bishop,
                                                                   .site          = 'Bishop Auckland',
                                                                   .indicator     = indicator,
                                                                   .sub.indicator = sub.indicator)
        results$model3.panelar.bishop.r2 <- model3.panelar.bishop$r2
        ##################################################
        ## Hartlepool                                   ##
        ##################################################
        df3$town <- relevel(df3$town, ref = 'Grimsby')
        model3.panelar.hartlepool <- filter(df3,
                                              group == 'Cohort : University Hospital of Hartlepool') %>%
                                     panelAR(formula  = formula.model3,
                                             timeVar  = timevar,
                                             panelVar = panel.trust,
                                             autoCorr = autocorr,
                                             panelCorrMethod = 'pcse',
                                             seq.times = seq.times,
                                             rho.na.rm = rho.na.rm)
        results$model3.panelar.hartlepool.coef <- extract_coefficients(x             = model3.panelar.hartlepool,
                                                                      .site          = 'Hartlepool',
                                                                      .indicator     = indicator,
                                                                      .sub.indicator = sub.indicator)
        results$model3.panelar.hartlepool.r2 <- model3.panelar.hartlepool$r2
        ##################################################
        ## Hemel Hempstead                              ##
        ##################################################
        df3$town <- relevel(df3$town, ref = 'Warwick')
        model3.panelar.hemel <- filter(df3,
                                         group == 'Cohort : Hemel Hempstead Hospital') %>%
                                panelAR(formula  = formula.model3,
                                        timeVar  = timevar,
                                        panelVar = panel.trust,
                                        autoCorr = autocorr,
                                        panelCorrMethod = 'pcse',
                                        seq.times = seq.times,
                                        rho.na.rm = rho.na.rm)
        results$model3.panelar.hemel.coef <- extract_coefficients(x              = model3.panelar.hemel,
                                                                  .site          = 'Hemel Hempstead',
                                                                  .indicator     = indicator,
                                                                  .sub.indicator = sub.indicator)
        results$model3.panelar.hemel.r2 <- model3.panelar.hemel$r2
        ##################################################
        ## Newark                                       ##
        ##################################################
        df3$town <- relevel(df3$town, ref = 'Southport')
        model3.panelar.newark <- filter(df3,
                                          group == 'Cohort : Newark Hospital') %>%
                                 panelAR(formula  = formula.model3,
                                         timeVar  = timevar,
                                         panelVar = panel.trust,
                                         autoCorr = autocorr,
                                         panelCorrMethod = 'pcse',
                                         seq.times = seq.times,
                                         rho.na.rm = rho.na.rm)
        results$model3.panelar.newark.coef <- extract_coefficients(x              = model3.panelar.newark,
                                                                   .site          = 'Newark',
                                                                   .indicator     = indicator,
                                                                   .sub.indicator = sub.indicator)
        results$model3.panelar.newark.r2 <- model3.panelar.newark$r2
        ##################################################
        ## Rochdale                                     ##
        ##################################################
        df3$town <- relevel(df3$town, ref = 'Rotherham')
        model3.panelar.rochdale <- filter(df3,
                                            group == 'Cohort : Rochdale Infirmary') %>%
                                   panelAR(formula  = formula.model3,
                                           timeVar  = timevar,
                                           panelVar = panel.trust,
                                           autoCorr = autocorr,
                                           panelCorrMethod = 'pcse',
                                           seq.times = seq.times,
                                           rho.na.rm = rho.na.rm)
        results$model3.panelar.rochdale.coef <- extract_coefficients(x            = model3.panelar.rochdale,
                                                                   .site          = 'Rochdale',
                                                                   .indicator     = indicator,
                                                                   .sub.indicator = sub.indicator)
        results$model3.panelar.rochdale.r2 <- model3.panelar.rochdale$r2
        ## Summary table
        results$model3.panelar <- combine_coefficients(bishop.coef     = results$model3.panelar.bishop.coef,
                                                       hartlepool.coef = results$model3.panelar.hartlepool.coef,
                                                       hemel.coef      = results$model3.panelar.hemel.coef,
                                                       newark.coef     = results$model3.panelar.newark.coef,
                                                       rochdale.coef   = results$model3.panelar.rochdale.coef)
        ## Forest plot
        results$model3.forest <- closed_forest(df.list       = list(results$model3.panelar.bishop.coef,
                                                                      results$model3.panelar.hartlepool.coef,
                                                                      results$model3.panelar.hemel.coef,
                                                                      results$model3.panelar.newark.coef,
                                                                      results$model3.panelar.rochdale.coef),
                                               plot.term     = c('closure'),
                                               facet.outcome = FALSE,
                                               title         = paste0('Model 3 : ',
                                                                      indicator,
                                                                      ' (',
                                                                      sub.indicator,
                                                                      ')'),
                                               theme         = theme_bw())
        ## Return model objects if requested
        if(return.model == TRUE){
            results$model3.panelar.bishop     <- model3.panelar.bishop
            results$model3.panelar.hartlepool <- model3.panelar.hartlepool
            results$model3.panelar.hemel      <- model3.panelar.hemel
            results$model3.panelar.newark     <- model3.panelar.newark
            results$model3.panelar.rochdale   <- model3.panelar.rochdale
        }
        if(return.df == TRUE){
            results$model3.df <- df3
        }
        if(return.residuals == TRUE){
            results$model3.panelar.residuals.bishop     <- summary(model3.panelar.bishop)$residuals
            results$model3.panelar.residuals.hartlepool <- summary(model3.panelar.hartlepool)$residuals
            results$model3.panelar.residuals.hemel      <- summary(model3.panelar.hemel)$residuals
            results$model3.panelar.residuals.newark     <- summary(model3.panelar.newark)$residuals
            results$model3.panelar.residuals.rochdale   <- summary(model3.panelar.rochdale)$residuals
        }
        if(return.residuals.plot == TRUE){
            results$model3.panelar.residuals.plot.bishop     <- summary(model3.panelar.bishop)$residuals %>% plot()
            results$model3.panelar.residuals.plot.hartlepool <- summary(model3.panelar.hartlepool)$residuals %>% plot()
            results$model3.panelar.residuals.plot.hemel      <- summary(model3.panelar.hemel)$residuals %>% plot()
            results$model3.panelar.residuals.plot.newark     <- summary(model3.panelar.newark)$residuals %>% plot()
            results$model3.panelar.residuals.plot.rochdale   <- summary(model3.panelar.rochdale)$residuals %>% plot()
        }
        ## Remove clutter
        rm(df3)
    }
    #######################################################################
    ## Model 4                                                           ##
    #######################################################################
    if(!is.null(model4)){
        ## Reformulate outcome and covariates
        formula.model4 <- reformulate(response = outcome,
                                      termlabels = model4)
        ## Subset data
        sites <- c('Bishop Auckland', 'Whitehaven',
                   'Hartlepool', 'Grimsby',
                   'Hemel Hempstead', 'Warwick',
                   'Newark', 'Southport',
                   'Rochdale', 'Rotherham')
        df4 <- filter(df.trust, town %in% sites  &
                      measure     == indicator &
                      sub.measure == sub.indicator)
        ## Add in indicator of case/control status for plotting
        case <- c('Bishop Auckland',
                  'Hartlepool',
                  'Hemel Hempstead',
                  'Newark',
                  'Rochdale')
        df4$status <- ifelse(df4$town %in% case, 'Case', 'Control')
        ## Generate time-series plot (at site/town level)
        df4$group <- paste0('Cohort : ', df4$group)
        results$model4.ts.plot <- closed_ts_plot(df = df4,
                                                 sites = sites,
                                                 indicator = indicator,
                                                 sub.indicator = sub.indicator,
                                                 steps = TRUE,
                                                 facet = TRUE,
                                                 tidy  = TRUE,
                                                 join  = join.line,
                                                 legend = legend)
        ## Perform analysis with panelAR in each
        ##################################################
        ## All                                          ##
        ##################################################
        df4$town <- relevel(df4$town, ref = 'Whitehaven')
        model4.panelar <- df4 %>%
                          ## closed_missing() %>%
                          panelAR(formula  = formula.model4,
                                  timeVar  = timevar,
                                  panelVar = panel.trust,
                                  autoCorr = autocorr,
                                  panelCorrMethod = 'pcse',
                                  seq.times = seq.times,
                                  rho.na.rm = rho.na.rm)
        results$model4.panelar.coef <- extract_coefficients(x              = model4.panelar,
                                                            .site          = 'All',
                                                            .indicator     = indicator,
                                                            .sub.indicator = sub.indicator)
        results$model4.panelar.r2 <- model4.panelar$r2
        ## Summary table
        results$model4.panelar.all <- combine_coefficients(bishop.coef     = results$model2.panelar.bishop.coef,
                                                       hartlepool.coef = results$model2.panelar.hartlepool.coef,
                                                       hemel.coef      = results$model2.panelar.hemel.coef,
                                                       newark.coef     = results$model2.panelar.newark.coef,
                                                       rochdale.coef   = results$model2.panelar.rochdale.coef,
                                                       all.coef        = results$model4.panelar.coef)
        ## Forest plot
        results$model4.forest <- closed_forest(df.list       = list(results$model2.panelar.bishop.coef,
                                                                    results$model2.panelar.hartlepool.coef,
                                                                    results$model2.panelar.hemel.coef,
                                                                    results$model2.panelar.newark.coef,
                                                                    results$model2.panelar.rochdale.coef,
                                                                    results$model4.panelar.coef),
                                               plot.term     = c('closure'),
                                               facet.outcome = FALSE,
                                               title         = paste0('Model 2 & 4 : ',
                                                                      indicator,
                                                                      ' (',
                                                                      sub.indicator,
                                                                      ')'),
                                               theme         = theme_bw())
        ## Return model objects if requested
        if(return.model == TRUE){
            results$model4.panelar     <- model4.panelar
        }
        if(return.df == TRUE){
            results$model4.df <- df4
        }
        if(return.residuals == TRUE){
            results$model4.panelar.residuals     <- summary(model4.panelar)$residuals
        }
        if(return.residuals.plot == TRUE){
            results$model4.panelar.residuals.plot <- summary(model4.panelar)$residuals %>% plot()
        }
        ## Remove clutter
        rm(df4)
    }
    #######################################################################
    ## Model 5                                                           ##
    #######################################################################
    if(!is.null(model5)){
        ## Reformulate outcome and covariates
        formula.model5 <- reformulate(response = outcome,
                                      termlabels = model5)
        ## Subset data
        df5 <- filter(df.trust,
                      measure     == indicator &
                      sub.measure == sub.indicator)
        ## Add in indicator of case/control status for plotting
        case <- c('Bishop Auckland',
                  'Hartlepool',
                  'Hemel Hempstead',
                  'Newark',
                  'Rochdale')
        df5$status <- ifelse(df5$town %in% case, 'Case', 'Control')
        ## Generate time-series plot (at site/town level)
        df5$group <- paste0('Cohort : ', df5$group)
        sites <- c('Basingstoke', 'Bishop Auckland', 'Blackburn', 'Carlisle', 'Grimsby', 'Hartlepool', 'Hemel Hempstead', 'Newark', 'Rochdale', 'Rotherham', 'Salford', 'Salisbury', 'Scarborough', 'Scunthorpe', 'Southport', 'WansbeckWarwick', 'Whitehaven', 'Wigan', 'Yeovil')
        results$model5.ts.plot <- closed_ts_plot(df = df5,
                                                 sites = sites,
                                                 indicator = indicator,
                                                 sub.indicator = sub.indicator,
                                                 steps = TRUE,
                                                 facet = TRUE,
                                                 tidy  = TRUE,
                                                 join  = join.line,
                                                 legend = legend)
        ## Perform analysis with panelAR in each
        ##################################################
        ## All sites                                    ##
        ##################################################
        df5$town <- relevel(df5$town, ref = 'Whitehaven')
        model5.panelar <- df5 %>%
                              ## closed_missing() %>%
                              panelAR(formula  = formula.model5,
                                      timeVar  = timevar,
                                      panelVar = panel.trust,
                                      autoCorr = autocorr,
                                      panelCorrMethod = 'pcse',
                                      seq.times = seq.times,
                                      rho.na.rm = rho.na.rm)
        results$model5.panelar.coef <- extract_coefficients(x              = model5.panelar,
                                                            .site          = 'All',
                                                            .indicator     = indicator,
                                                            .sub.indicator = sub.indicator)
        results$model5.panelar.r2 <- model5.panelar.all$r2
        ## Summary table
        ## results$model5.panelar <- results$model5.panelar.coef
        results$model5.panelar.all <- combine_coefficients(bishop.coef     = results$model3.panelar.bishop.coef,
                                                           hartlepool.coef = results$model3.panelar.hartlepool.coef,
                                                           hemel.coef      = results$model3.panelar.hemel.coef,
                                                           newark.coef     = results$model3.panelar.newark.coef,
                                                           rochdale.coef   = results$model3.panelar.rochdale.coef,
                                                           all.coef        = results$model5.panelar.coef)
        ## Forest plot
        results$model5.forest <- closed_forest(df.list       = list(results$model3.panelar.bishop.coef,
                                                                    results$model3.panelar.hartlepool.coef,
                                                                    results$model3.panelar.hemel.coef,
                                                                    results$model3.panelar.newark.coef,
                                                                    results$model3.panelar.rochdale.coef,
                                                                    results$model5.panelar.coef),
                                               plot.term     = c('closure'),
                                               facet.outcome = FALSE,
                                               title         = paste0('Model 3 & 5  : ',
                                                                      indicator,
                                                                      ' (',
                                                                      sub.indicator,
                                                                      ')'),
                                               theme         = theme_bw())
        ## Return model objects if requested
        if(return.model == TRUE){
            results$model5.panelar     <- model5.panelar
        }
        if(return.df == TRUE){
            results$model5.df <- df5
        }
        if(return.residuals == TRUE){
            results$model5.panelar.residuals.all     <- summary(model5.panelar.all)$residuals
        }
        if(return.residuals.plot == TRUE){
            results$model2.panelar.residuals.plot <- summary(model4.panelar)$residuals %>% plot()
        }
        ## Remove clutter
        rm(df5)
    }
    #######################################################################
    ## Model 6                                                           ##
    #######################################################################
    if(!is.null(model6)){
        ## Reformulate outcome and covariates
        formula.model6 <- reformulate(response = outcome,
                                      termlabels = model6)
        ## Subset data
        sites <- c('Bishop Auckland', 'Whitehaven',
                   'Hartlepool', 'Grimsby',
                   'Hemel Hempstead', 'Warwick',
                   'Newark', 'Southport',
                   'Rochdale', 'Rotherham')
        df6 <- filter(df.lsoa, town %in% sites &
                            measure     == indicator &
                            sub.measure == sub.indicator)
        df6$group <- paste0('Cohort : ', df6$group)
        ## Add in indicator of case/control status for plotting
        case <- c('Bishop Auckland',
                  'Hartlepool',
                  'Hemel Hempstead',
                  'Newark',
                  'Rochdale')
        df6$status <- ifelse(df6$town %in% case, 'Case', 'Control')
        ## Generate time-series plot (at site/town level)
        df6.trust <- filter(df.trust, town %in% sites &
                            measure     == indicator &
                            sub.measure == sub.indicator)
        df6.trust$group <- paste0('Cohort : ', df6.trust$group)
        results$model6.ts.plot <- closed_ts_plot(df = df6.trust,
                                                 sites = sites,
                                                 indicator = indicator,
                                                 sub.indicator = sub.indicator,
                                                 steps = TRUE,
                                                 facet = TRUE,
                                                 tidy  = TRUE,
                                                 join  = join.line,
                                                 legend = legend)
        ## Perform analysis with panelAR in each
        ##################################################
        ## Model 6 - Bishop Auckland                    ##
        ##################################################
        df6$town <- relevel(df6$town, ref = 'Whitehaven')
        model6.panelar.bishop <- filter(df6,
                                        town == 'Bishop Auckland' |
                                        town == 'Whitehaven') %>%
                              panelAR(formula  = formula.model6,
                                      timeVar  = timevar,
                                      panelVar = panel.lsoa,
                                      autoCorr = autocorr,
                                      panelCorrMethod = 'pcse',
                                      seq.times = seq.times,
                                      rho.na.rm = rho.na.rm)
        results$model6.panelar.bishop.coef <- extract_coefficients(x              = model6.panelar.bishop,
                                                                .site          = 'Bishop Auckland',
                                                                .indicator     = indicator,
                                                                .sub.indicator = sub.indicator)
        results$model6.panelar.bishop.r2 <- model6.panelar.bishop
        ##################################################
        ## Model 6 - Hartlepool                         ##
        ##################################################
        df6$town <- relevel(df6$town, ref = 'Grimsby')
        model6.panelar.hartlepool <- filter(df6,
                                            town == 'Hartlepool' |
                                            town == 'Grimsby') %>%
                                     panelAR(formula  = formula.model6,
                                             timeVar  = timevar,
                                             panelVar = panel.lsoa,
                                             autoCorr = autocorr,
                                             panelCorrMethod = 'pcse',
                                             seq.times = seq.times,
                                             rho.na.rm = rho.na.rm)
        results$model6.panelar.hartlepool.coef <- extract_coefficients(x             = model6.panelar.hartlepool,
                                                                      .site          = 'Hartlepool',
                                                                      .indicator     = indicator,
                                                                      .sub.indicator = sub.indicator)
        results$model6.panelar.hartlepool.r2 <- model6.panelar.hartlepool$r2
        ##################################################
        ## Model 6 - Hemel Hempstead                    ##
        ##################################################
        df6$town <- relevel(df6$town, ref = 'Warwick')
        model6.panelar.hemel <- filter(df6,
                                       town == 'Hemel Hempstead' |
                                       town == 'Warwick') %>%
                                panelAR(formula  = formula.model6,
                                        timeVar  = timevar,
                                        panelVar = panel.lsoa,
                                        autoCorr = autocorr,
                                        panelCorrMethod = 'pcse',
                                        seq.times = seq.times,
                                        rho.na.rm = rho.na.rm)
        results$model6.panelar.hemel.coef <- extract_coefficients(x              = model6.panelar.hemel,
                                                                  .site          = 'Hemel Hempstead',
                                                                  .indicator     = indicator,
                                                                  .sub.indicator = sub.indicator)
        results$model6.panelar.hemel.r2 <- model6.panelar.hemel$r2
        ##################################################
        ## Model 6 - Newark                             ##
        ##################################################
        df6$town <- relevel(df6$town, ref = 'Southport')
        model6.panelar.newark <- filter(df6,
                                        town == 'Newark' |
                                        town == 'Southport') %>%
                                 panelAR(formula  = formula.model6,
                                         timeVar  = timevar,
                                         panelVar = panel.lsoa,
                                         autoCorr = autocorr,
                                         panelCorrMethod = 'pcse',
                                         seq.times = seq.times,
                                         rho.na.rm = rho.na.rm)
        results$model6.panelar.newark.coef <- extract_coefficients(x              = model6.panelar.newark,
                                                                   .site          = 'Newark',
                                                                   .indicator     = indicator,
                                                                   .sub.indicator = sub.indicator)
        results$model6.panelar.newark.r2 <- model6.panelar.newark$r2
        ##################################################
        ## Model 6 - Rochdale                           ##
        ##################################################
        df6$town <- relevel(df6$town, ref = 'Rotherham')
        model6.panelar.rochdale <- filter(df6,
                                          town == 'Rochdale' |
                                          town == 'Rotherham') %>%
                                   panelAR(formula  = formula.model6,
                                           timeVar  = timevar,
                                           panelVar = panel.lsoa,
                                           autoCorr = autocorr,
                                           panelCorrMethod = 'pcse',
                                           seq.times = seq.times,
                                           rho.na.rm = rho.na.rm)
        results$model6.panelar.rochdale.coef <- extract_coefficients(x            = model6.panelar.rochdale,
                                                                   .site          = 'Rochdale',
                                                                   .indicator     = indicator,
                                                                   .sub.indicator = sub.indicator)
        results$model6.panelar.rochdale.r2 <- model6.panelar.rochdale$r2
        ## Summary table
        results$model6.panelar <- results$model6.panelar.all.coef
        results$model6.panelar <- combine_coefficients(bishop.coef     = results$model6.panelar.bishop.coef,
                                                       hartlepool.coef = results$model6.panelar.hartlepool.coef,
                                                       hemel.coef      = results$model6.panelar.hemel.coef,
                                                       newark.coef     = results$model6.panelar.newark.coef,
                                                       rochdale.coef   = results$model6.panelar.rochdale.coef)
        ## ## Forest plot
        results$model6.forest.model6 <- closed_forest(df.list       = list(results$model6.panelar.bishop.coef,
                                                                           results$model6.panelar.hartlepool.coef,
                                                                           results$model6.panelar.hemel.coef,
                                                                           results$model6.panelar.newark.coef,
                                                                           results$model6.panelar.rochdale.coef),
                                               plot.term     = c('closure'),
                                               facet.outcome = FALSE,
                                               title         = paste0('Model 6 : ',
                                                                      indicator,
                                                                      ' (',
                                                                      sub.indicator,
                                                                      ')'),
                                               theme         = theme_bw())
        ## Return model objects if requested
        if(return.model == TRUE){
            results$model6.panelar.bishop     <- model6.panelar.bishop
            results$model6.panelar.hartlepool <- model6.panelar.hartlepool
            results$model6.panelar.hemel      <- model6.panelar.hemel
            results$model6.panelar.newark     <- model6.panelar.newark
            results$model6.panelar.rochdale   <- model6.panelar.rochdale
        }
        if(return.df == TRUE){
            results$model6.df <- df6
        }
        if(return.residuals == TRUE){
            results$model6.panelar.residuals.bishop     <- summary(model6.panelar.bishop)$residuals
            results$model6.panelar.residuals.hartlepool <- summary(model6.panelar.hartlepool)$residuals
            results$model6.panelar.residuals.hemel      <- summary(model6.panelar.hemel)$residuals
            results$model6.panelar.residuals.newark     <- summary(model6.panelar.newark)$residuals
            results$model6.panelar.residuals.rochdale   <- summary(model6.panelar.rochdale)$residuals
        }
        if(return.residuals.plot == TRUE){
            results$model6.panelar.residuals.plot.bishop     <- summary(model6.panelar.bishop)$residuals %>% plot()
            results$model6.panelar.residuals.plot.hartlepool <- summary(model6.panelar.hartlepool)$residuals %>% plot()
            results$model6.panelar.residuals.plot.hemel      <- summary(model6.panelar.hemel)$residuals %>% plot()
            results$model6.panelar.residuals.plot.newark     <- summary(model6.panelar.newark)$residuals %>% plot()
            results$model6.panelar.residuals.plot.rochdale   <- summary(model6.panelar.rochdale)$residuals %>% plot()
        }
        ## Remove clutter
        rm(df6)
    }
    #######################################################################
    ## Model 7                                                           ##
    #######################################################################
    if(!is.null(model7)){
        ## Reformulate outcome and covariates
        formula.model7 <- reformulate(response = outcome,
                                      termlabels = model7)
        ## Subset data
        sites <- c('Bishop Auckland', 'Whitehaven',
                   'Hartlepool', 'Grimsby',
                   'Hemel Hempstead', 'Warwick',
                   'Newark', 'Southport',
                   'Rochdale', 'Rotherham')
        df7 <- filter(df.lsoa, town %in% sites &
                      measure     == indicator &
                      sub.measure == sub.indicator)
        ## Two LSOAs overlap two EDs so derive a new unique indicator
        ## and use that for the panels
        df7$town.lsoa <- paste0(df7$town, df7$lsoa)
        ## Add in indicator of case/control status for plotting
        case <- c('Bishop Auckland',
                  'Hartlepool',
                  'Hemel Hempstead',
                  'Newark',
                  'Rochdale')
        df7$status <- ifelse(df7$town %in% case, 'Case', 'Control')
        results$df7 <- df7
        ## Generate time-series plot (at site/town level)
        df7$group <- paste0('Cohort : ', df7$group)
        results$model7.ts.plot <- closed_ts_plot(df = df7,
                                                 sites = sites,
                                                 indicator = indicator,
                                                 sub.indicator = sub.indicator,
                                                 steps = TRUE,
                                                 facet = TRUE,
                                                 tidy  = TRUE,
                                                 join  = join.line,
                                                 legend = legend)
        ## Perform analysis with panelAR in each
        ##################################################
        ## All sites                                    ##
        ##################################################
        formula.model7 <- reformulate(response = outcome,
                                       termlabels = c(model7))
        df7$town <- relevel(df7$town, ref = 'Whitehaven')
        model7.panelar <- filter(df7,
                                 measure     == indicator &
                                 sub.measure == sub.indicator) %>%
                              panelAR(formula  = formula.model7,
                                      timeVar  = timevar,
                                      panelVar = 'town.lsoa',
                                      autoCorr = autocorr,
                                      panelCorrMethod = 'pcse',
                                      seq.times = seq.times,
                                      rho.na.rm = rho.na.rm)
        results$model7.panelar.coef <- extract_coefficients(x              = model7.panelar,
                                                                .site          = 'All',
                                                                .indicator     = indicator,
                                                                .sub.indicator = sub.indicator)
        results$model7.panelar.r2 <- model7.panelar.all
        ## Summary table
        ## results$model7.panelar <- results$model7.panelar.coef
        results$model7.panelar.all <- combine_coefficients(bishop.coef     = results$model6.panelar.bishop.coef,
                                                       hartlepool.coef = results$model6.panelar.hartlepool.coef,
                                                       hemel.coef      = results$model6.panelar.hemel.coef,
                                                       newark.coef     = results$model6.panelar.newark.coef,
                                                       rochdale.coef   = results$model6.panelar.rochdale.coef,
                                                       all.coef        = results$model7.panelar.all.coef)
        ## ## Forest plot
        results$model7.forest <- closed_forest(df.list       = list(results$model6.panelar.bishop.coef,
                                                                    results$model6.panelar.hartlepool.coef,
                                                                    results$model6.panelar.hemel.coef,
                                                                    results$model6.panelar.newark.coef,
                                                                    results$model6.panelar.rochdale.coef,
                                                                    results$model7.panelar.all.coef),
                                               plot.term     = c('closure'),
                                               facet.outcome = FALSE,
                                               title         = paste0('Model 6 & Model 7 : ',
                                                                      indicator,
                                                                      ' (',
                                                                      sub.indicator,
                                                                      ')'),
                                               theme         = theme_bw())
        ## Return model objects if requested
        if(return.model == TRUE){
            results$model7.panelar     <- model7.panelar
        }
        if(return.df == TRUE){
            results$model7.df <- df7
        }
        if(return.residuals == TRUE){
            results$model7.panelar.residuals.all     <- summary(model7.panelar.all)$residuals
        }
        if(return.residuals.plot == TRUE){
            results$model7.panelar.residuals.plot <- summary(model7.panelar)$residuals %>% plot()
        }
        ## Remove clutter
        rm(df7)
    }
    #######################################################################
    ## Model 8                                                           ##
    #######################################################################
    if(!is.null(model8)){
        ## Reformulate outcome and covariates
        formula.model8 <- reformulate(response = outcome,
                                      termlabels = model8)
        ## Pool the data
        df8 <- closed_pool(df             = df.trust,
                           within.centres = TRUE)
        ## Generate Time-Series Plots
        df8$group <- paste0('Cohort : ', df8$group)
        sites <- c('Bishop Auckland', 'Hartlepool', 'Hemel Hempstead', 'Newark', 'Rochdale', 'Control')
        results$model8.ts.plot <- closed_ts_plot(df            = df8,
                                                 sites         = sites,
                                                 indicator     = indicator,
                                                 sub.indicator = sub.indicator,
                                                 steps         = TRUE,
                                                 facet         = TRUE,
                                                 tidy          = TRUE,
                                                 join          = join.line,
                                                 legend        = legend,
                                                 pool.control  = FALSE) ## NB - This is FALSE because data has already been pooled
        ## Ensure relative.month is a numeric integer
        df8$relative.month <- as.integer(df8$relative.month) %>% as.numeric()
        ##################################################
        ## Bishop Auckland                              ##
        ##################################################
        t <- filter(df8,
                    group == 'Cohort : Bishop Auckland General Hospital') %>%
            as.data.frame()
        if(nrow(t) > 0){
            ## print('Bishop')
            model8.panelar.bishop <- panelAR(data     = t,
                                             formula  = formula.model8,
                                             timeVar  = timevar,
                                             panelVar = panel.trust,
                                             autoCorr = autocorr,
                                             panelCorrMethod = 'pcse',
                                             seq.times = seq.times,
                                             rho.na.rm = rho.na.rm)
            results$model8.panelar.bishop.coef <- extract_coefficients(x              = model8.panelar.bishop,
                                                                       .site          = 'Bishop Auckland',
                                                                   .indicator     = indicator,
                                                                   .sub.indicator = sub.indicator)
            results$model8.panelar.bishop.r2 <- model8.panelar.bishop$r2
        }
        ##################################################
        ## Hartlepool                                   ##
        ##################################################
        t <- filter(df8,
                    group == 'Cohort : University Hospital of Hartlepool') %>%
            as.data.frame()
        if(nrow(t) > 0){
            ## print('Hartlepool')
            model8.panelar.hartlepool <- panelAR(data     = t,
                                                 formula  = formula.model8,
                                                 timeVar  = timevar,
                                                 panelVar = panel.trust,
                                                 autoCorr = autocorr,
                                                 panelCorrMethod = 'pcse',
                                                 seq.times = seq.times,
                                                 rho.na.rm = rho.na.rm)
            results$model8.panelar.hartlepool.coef <- extract_coefficients(x             = model8.panelar.hartlepool,
                                                                           .site          = 'Hartlepool',
                                                                           .indicator     = indicator,
                                                                           .sub.indicator = sub.indicator)
            results$model8.panelar.hartlepool.r2 <- model8.panelar.hartlepool$r2
        }
        ##################################################
        ## Hemel Hempstead                              ##
        ##################################################
        t <- filter(df8,
                    group == 'Cohort : Hemel Hempstead Hospital') %>%
             as.data.frame()
        if(nrow(t) > 0){
            ## print('Hemel')
            model8.panelar.hemel <- panelAR(data     = t,
                                            formula  = formula.model8,
                                            timeVar  = timevar,
                                            panelVar = panel.trust,
                                            autoCorr = autocorr,
                                            panelCorrMethod = 'pcse',
                                            seq.times = seq.times,
                                            rho.na.rm = rho.na.rm)
            results$model8.panelar.hemel.coef <- extract_coefficients(x              = model8.panelar.hemel,
                                                                      .site          = 'Hemel Hempstead',
                                                                      .indicator     = indicator,
                                                                      .sub.indicator = sub.indicator)
            results$model8.panelar.hemel.r2 <- model8.panelar.hemel$r2
        }
        ##################################################
        ## Newark                                       ##
        ##################################################
        t <- filter(df8,
                    group == 'Cohort : Newark Hospital') %>%
             as.data.frame()
        if(nrow(t) > 0){
            ## print('Newark')
            model8.panelar.newark <- panelAR(data     = t,
                                             formula  = formula.model8,
                                             timeVar  = timevar,
                                             panelVar = panel.trust,
                                             autoCorr = autocorr,
                                             panelCorrMethod = 'pcse',
                                             seq.times = seq.times,
                                             rho.na.rm = rho.na.rm)
            results$model8.panelar.newark.coef <- extract_coefficients(x              = model8.panelar.newark,
                                                                       .site          = 'Newark',
                                                                       .indicator     = indicator,
                                                                       .sub.indicator = sub.indicator)
            results$model8.panelar.newark.r2 <- model8.panelar.newark$r2
        }
        ##################################################
        ## Rochdale                                     ##
        ##################################################
        t <- filter(df8,
                    group == 'Cohort : Rochdale Infirmary') %>%
            as.data.frame()
        if(nrow(t) > 0){
            ## print('Rochdale')
            model8.panelar.rochdale <- panelAR(data     = t,
                                               formula  = formula.model8,
                                               timeVar  = timevar,
                                               panelVar = panel.trust,
                                               autoCorr = autocorr,
                                               panelCorrMethod = 'pcse',
                                               seq.times = seq.times,
                                               rho.na.rm = rho.na.rm)
            results$model8.panelar.rochdale.coef <- extract_coefficients(x            = model8.panelar.rochdale,
                                                                         .site          = 'Rochdale',
                                                                         .indicator     = indicator,
                                                                         .sub.indicator = sub.indicator)
            results$model8.panelar.rochdale.r2 <- model8.panelar.rochdale$r2
        }
        ##################################################
        ## All sites                                    ##
        ##################################################
        ## TODO - Maybe get this working
        ## ## Pool the data across centres
        ## df8 <- closed_pool(df             = df.trust,
        ##                    within.centres = FALSE)
        ## ## Remove 'season' from the model since that is only feasible when pooling within
        ## ## centres because months/dates do not align
        ## model8 <- gsub('season', '', model8)
        ## formula.model8 <- reformulate(response = outcome,
        ##                               termlabels = model8)
        ## model8.panelar.all <- filter(df8,
        ##                              measure     == indicator &
        ##                              sub.measure == sub.indicator) %>%
        ##                       panelAR(formula  = formula.model8,
        ##                               timeVar  = timevar,
        ##                               panelVar = panel.trust,
        ##                               autoCorr = autocorr,
        ##                               panelCorrMethod = 'pcse',
        ##                               seq.times = seq.times,
        ##                               rho.na.rm = rho.na.rm)
        ## results$model8.panelar.all.coef <- extract_coefficients(x              = model8.panelar.all,
        ##                                                         .site          = 'All',
        ##                                                         .indicator     = indicator,
        ##                                                         .sub.indicator = sub.indicator)
        ## results$model8.panelar.r2 <- model8.panelar.all
        ## Summary table
        results$model8.panelar <- combine_coefficients(bishop.coef     = results$model8.panelar.bishop.coef,
                                                       hartlepool.coef = results$model8.panelar.hartlepool.coef,
                                                       hemel.coef      = results$model8.panelar.hemel.coef,
                                                       newark.coef     = results$model8.panelar.newark.coef,
                                                       rochdale.coef   = results$model8.panelar.rochdale.coef)
                                                       ## all.coef        = results$model8.panelar.all.coef)
        ## ## Forest plot
        results$model8.forest <- closed_forest(df.list       = list(results$model8.panelar.bishop.coef,
                                                                    results$model8.panelar.hartlepool.coef,
                                                                    results$model8.panelar.hemel.coef,
                                                                    results$model8.panelar.newark.coef,
                                                                    results$model8.panelar.rochdale.coef),
                                                                    ## results$model8.panelar.all.coef),
                                               plot.term     = c('closure'),
                                               facet.outcome = FALSE,
                                               title         = paste0('Model 8 : ',
                                                                      indicator,
                                                                      ' (',
                                                                      sub.indicator,
                                                                      ')'),
                                               theme         = theme_bw())
        ## Return model objects if requested
        if(return.model == TRUE){
            results$model8.panelar.bishop     <- model8.panelar.bishop
            results$model8.panelar.hartlepool <- model8.panelar.hartlepool
            results$model8.panelar.hemel      <- model8.panelar.hemel
            results$model8.panelar.newark     <- model8.panelar.newark
            results$model8.panelar.rochdale   <- model8.panelar.rochdale
            ## results$model8.panelar.all     <- model8.panelar.all
        }
        if(return.df == TRUE){
            results$model8.df <- df8
        }
        if(return.residuals == TRUE){
            results$model8.panelar.residuals.bishop     <- summary(model8.panelar.bishop)$residuals
            results$model8.panelar.residuals.hartlepool <- summary(model8.panelar.hartlepool)$residuals
            results$model8.panelar.residuals.hemel      <- summary(model8.panelar.hemel)$residuals
            results$model8.panelar.residuals.newark     <- summary(model8.panelar.newark)$residuals
            results$model8.panelar.residuals.rochdale   <- summary(model8.panelar.rochdale)$residuals
            ## results$model8.panelar.residuals.all     <- summary(model8.panelar.all)$residuals
        }
        if(return.residuals.plot == TRUE){
            results$model8.panelar.residuals.plot.bishop     <- summary(model8.panelar.bishop)$residuals %>% plot()
            results$model8.panelar.residuals.plot.hartlepool <- summary(model8.panelar.hartlepool)$residuals %>% plot()
            results$model8.panelar.residuals.plot.hemel      <- summary(model8.panelar.hemel)$residuals %>% plot()
            results$model8.panelar.residuals.plot.newark     <- summary(model8.panelar.newark)$residuals %>% plot()
            results$model8.panelar.residuals.plot.rochdale   <- summary(model8.panelar.rochdale)$residuals %>% plot()
            ## results$model8.panelar.residuals.plot <- summary(model8.panelar)$residuals %>% plot()
        }
        ## Remove clutter
        rm(df8)
    }
    #######################################################################
    ## Return the results                                                ##
    #######################################################################
    return(results)
}
