#' Perform Regression on ClosED data
#'
#' @description Perform Prais-Winsten Regression on ClosED data
#'
#' @details The ClosED study uses time-series with dummy indicators to
#' test for the impact of closing Emergency Departments on indicators of
#' performance.  This performs
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
#' @param df.trust Data frame at the Trust level to analyse ideally cleaned of spurious datasets by \code{closed_clean()}.
#' @param indicator The performance indicator to assess.
#' @param sub.indicator The sub-measure performance indicator to assess.
#' @param panel.lsoa Variable that defines panels in LSOA level data (default is \code{lsoa} and shouldn't need changing).
#' @param panel.trust Variable that defines panels in Trust level data (default is \code{town} and shouldn't need changing).
#' @param time Variable defining time in both data sets (default is \code{relative.month} and shouldn't need changing).
#' @param outcome Outcome variable containing the counts (default is \code{value} and shouldn't need changing).
#' @param model0 Covariates to include in model 0.
#' @param model0.5 Covariates to include in model 0.5.
#' @param model1 Covariates to include in model 1.
#' @param model2 Covariates to include in model 2.
#' @param model3.1 Covariates to include in model 3.
#' @param model3.2 Covariates to include in model 8.
#' @param model4 Covariates to include in model 4.
#' @param model5 Covariates to include in model 5.
#' @param model6.1 Covariates to include in model 6.1.
#' @param model6.2 Covariates to include in model 6.2.
#' @param model7.1 Covariates to include in model 7.1.
#' @param model7.2 Covariates to include in model 7.2.
#' @param autocorr panelAR() option for handling auto-correlation, default is \code{ar1}.
#' @param panelcorrmethod panelAR() option for panel correction, default is \code{pcse}.
#' @param coefficients Determine which coefficients from the model are included in summary tables.  Setting to \code{closure} will return only terms that involve only the closure indicator (i.e. \code{closure} itself).  Other options include \code{town} for site specific terms (no interactions) and \code{closure.town} (the default) which includes all closure and town terms, both individually and from interactions.  Use \code{all} to get all terms returned or for closure, town and other steps use \code{all.steps}
#' @param complete.case Whether to use balanced panels.
#' @param weights option for \code{panelAR} weights (see \code{?panelAR} for options).
#' @param seq.times Logical whether to use \code{panelAR} \code{seq.times} option to ignore gaps.
#' @param rho.na.rm Logical operator passed to panelAR() for excluding panel specific autocorrelation when it can not be calculated.
#' @param theme GGplot2 theme to use for Forrest Plots.
#' @param return.df Logical operator of whether to return the subsetted/summarised data frame (useful for subsequent development).
#' @param return.model Logical operator of whether to return the fitted models (not currently working correctly).
#' @param return.residuals Logical oeprator of whether to return the residuals of the fitted model.
#' @param rho.na.rm Logical operator passed to panelAR() for excluding panel specific autocorrelation when it can not be calculated.
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
#' ts.mortality <- closed_regress(df       = ,)
#'
#'
#' @references
#'
#' @export
closed_models <- function(df.lsoa         = ed_attendances_by_mode_measure,
                          df.trust        = ed_attendances_by_mode_site_measure_clean,
                          indicator       = 'ed attendances',
                          sub.indicator   = 'any',
                          panel.lsoa      = 'lsoa',
                          panel.trust     = 'town',
                          timevar         = 'relative.month',
                          outcome         = 'value',
                          model0          = c('closure'),
                          model0.5        = c('closure', 'relative.month'),
                          model1          = c('closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
                          model2          = c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
                          model3.1        = c('pooled.control * closure', 'town', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
                          model3.2        = c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
                          model4          = c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
                          model5          = c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
                          model6.1        = c('season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert', 'diff.time.to.ed'),
                          model6.2        = c('town', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert', 'diff.time.to.ed'),
                          model7.1        = c('closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert', 'diff.time.to.ed'),
                          model7.2        = c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert', 'diff.time.to.ed'),
                          autocorr        = 'ar1',
                          panelcorrmethod = 'pcse',
                          coefficients    = 'closure.town',
                          complete.case   = TRUE,
                          weights         = '',
                          seq.times       = FALSE,
                          rho.na.rm       = FALSE,
                          theme           = theme_bw(),
                          return.df       = FALSE,
                          return.model    = TRUE,
                          return.residuals = FALSE,
                          digits           = 3,
                          ...){
    #######################################################################
    ## Set up (results, formula, renaming variables)                     ##
    #######################################################################
    ## Initialise results list for returning everything
    results <- list()
    ## print("Debug 1")
    ## Obtain the levels of town and group and site type the number of observations
    ## within each to control subsequent analyses
    ## ToDo - Think how to loop over each of these groups testing each of the models
    ##        This would make the function very general and flexible for future use.
    town.group <- dplyr::filter(df.trust, measure == indicator & sub.measure == sub.indicator & !is.na(value)) %>%
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
    #######################################################################
    ## Derive the mean, sd, median, iqr, min and max of events before/   ##
    ## after closure for combining into a summary table with model       ##
    ## coefficients                                                      ##
    #######################################################################
    df.trust <- mutate(df.trust,
                       before.after = ifelse(site.type == 'intervention' & relative.month >= 25, "After", "Before"))
    results$summary.df <- group_by(df.trust, town, before.after) %>%
                          summarise(n        = n(),
                                    mean     = mean(value, na.rm = TRUE),
                                    sd       = sd(value, na.rm = TRUE),
                                    min      = min(value, na.rm = TRUE),
                                    max      = max(value, na.rm = TRUE),
                                    p25      = quantile(value, probs = 0.25, na.rm = TRUE),
                                    p50      = quantile(value, probs = 0.50, na.rm = TRUE),
                                    p75      = quantile(value, probs = 0.75, na.rm = TRUE))
    results$summary.table.head <- results$summary.df
    results$summary.table.head <- mutate(results$summary.table.head,
                                         mean.sd    = paste0(formatC(mean, digits = digits, format = 'f'),
                                                             ' (',
                                                             formatC(sd, digits = digits, format = 'f'),
                                                             ')'))
    results$summary.table.head <- mutate(results$summary.table.head,
                                         median.iqr = paste0(formatC(p50, digits = 1, format = 'f'),
                                                             ' (',
                                                             formatC(p25, digits = 1, format = 'f'),
                                                             '-',
                                                             formatC(p75, digits = 1, format = 'f'),
                                                             ')'))
    results$summary.table.head <- mutate(results$summary.table.head,
                                         min.max    = paste0(formatC(min, digits = 0, format = 'f'),
                                                             '-',
                                                             formatC(max, digits = 0, format = 'f')))
    results$summary.table.head <- dplyr::select(results$summary.table.head,
                                                town, before.after, mean.sd, median.iqr, min.max, mean)
    ## Reshape the table header
    results$summary.table.head <- melt(results$summary.table.head, id.vars = c('town', 'before.after')) %>%
                                  dcast(town ~ before.after + variable)
    results$summary.table.head$Before_mean <- as.numeric(results$summary.table.head$Before_mean)
    results$summary.table.head$After_mean  <- as.numeric(results$summary.table.head$After_mean)
    results$summary.table.head <- mutate(results$summary.table.head,
                                         diff_abs = formatC(Before_mean - After_mean, digits = digits, format = 'f'),
                                         diff_perc = formatC((100 * abs(Before_mean - After_mean)) / Before_mean, digits = digits, format = 'f'))
    ## Order the data
    results$summary.table.head$order <- 0
    results$summary.table.head$order[results$summary.table.head$town == 'Bishop Auckland'] <- 1
    results$summary.table.head$order[results$summary.table.head$town == 'Whitehaven']      <- 2
    results$summary.table.head$order[results$summary.table.head$town == 'Salford']         <- 3
    results$summary.table.head$order[results$summary.table.head$town == 'Scarborough']     <- 4
    results$summary.table.head$order[results$summary.table.head$town == 'Hartlepool']      <- 5
    results$summary.table.head$order[results$summary.table.head$town == 'Grimsby']         <- 6
    results$summary.table.head$order[results$summary.table.head$town == 'Blackburn']       <- 7
    results$summary.table.head$order[results$summary.table.head$town == 'Wigan']           <- 8
    results$summary.table.head$order[results$summary.table.head$town == 'Hemel Hempstead'] <- 9
    results$summary.table.head$order[results$summary.table.head$town == 'Warwick']         <- 10
    results$summary.table.head$order[results$summary.table.head$town == 'Basingstoke']     <- 11
    results$summary.table.head$order[results$summary.table.head$town == 'Yeovil']          <- 12
    results$summary.table.head$order[results$summary.table.head$town == 'Newark']          <- 13
    results$summary.table.head$order[results$summary.table.head$town == 'Southport']       <- 14
    results$summary.table.head$order[results$summary.table.head$town == 'Carlisle']        <- 15
    results$summary.table.head$order[results$summary.table.head$town == 'Salisbury']       <- 16
    results$summary.table.head$order[results$summary.table.head$town == 'Rochdale']        <- 17
    results$summary.table.head$order[results$summary.table.head$town == 'Rotherham']       <- 18
    results$summary.table.head$order[results$summary.table.head$town == 'Scunthorpe']      <- 19
    results$summary.table.head$order[results$summary.table.head$town == 'Wansbeck']        <- 20
    results$summary.table.head <- arrange(results$summary.table.head, order)
    results$summary.table.head <- dplyr::select(results$summary.table.head,
                                                town,
                                                Before_mean.sd, Before_median.iqr, Before_min.max,
                                                After_mean.sd, After_median.iqr, After_min.max,
                                                diff_abs, diff_perc)
    ## Add in grouping to facilitate subsetting later
    results$summary.table.head$group <- NA
    results$summary.table.head$group[results$summary.table.head$town %in% c('Bishop Auckland', 'Whitehaven', 'Salford', 'Scarborough')] <- 'Bishop Auckland'
    results$summary.table.head$group[results$summary.table.head$town %in% c('Hartlepool', 'Grimsby', 'Blackburn', 'Wigan')] <- 'Hartlepool'
    results$summary.table.head$group[results$summary.table.head$town %in% c('Hemel Hempstead', 'Warwick', 'Basingstoke', 'Yeovil')] <- 'Hemel Hempstead'
    results$summary.table.head$group[results$summary.table.head$town %in% c('Newark', 'Southport', 'Carlisle', 'Salisbury')] <- 'Newark'
    results$summary.table.head$group[results$summary.table.head$town %in% c('Rochdale', 'Rotherham', 'Scunthorpe', 'Wansbeck')] <- 'Rochdale'
    ## Add indicator for primary control
    results$summary.table.head$town <- as.character(results$summary.table.head$town)
    results$summary.table.head$town[results$summary.table.head$town %in% c('Whitehaven', 'Grimsby', 'Warwick', 'Southport', 'Rotherham')] <- paste0(results$summary.table.head$town[results$summary.table.head$town %in% c('Whitehaven', 'Grimsby', 'Warwick', 'Southport', 'Rotherham')], ' (Primary)')
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
        ## print(.coef)
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
        ## Build the column names conditional on the non-null arguments
        ## Bear in mind that the reshape puts everything in alphabetical order
        ## Stub that all require
        column.names <- c('Indicator', 'Subindicator', 'Term')
        ## All column
        if(!is.null(all.coef)){
            column.names <- c(column.names, 'All')
        }
        if(!is.null(bishop.coef)){
            column.names <- c(column.names, 'Bishop Auckland')
        }
        if(!is.null(hartlepool.coef)){
            column.names <- c(column.names, 'Hartlepool')
        }
        if(!is.null(hemel.coef)){
            column.names <- c(column.names, 'Hemel Hempstead')
        }
        if(!is.null(newark.coef)){
            column.names <- c(column.names, 'Newark')
        }
        if(!is.null(rochdale.coef)){
            column.names <- c(column.names, 'Rochdale')
        }
        names(coef$coef) <- column.names
        ## Derive a caption for the table
        coef$caption <- paste0('Comparison of coefficients from Prais-Winsten Time-Series Regression across sites.  Each cell contains a point estimate followed by the standard error (in brackets) and the associated p-value (in scientific format due to some values being very small).')
        return(coef)
    }
    #######################################################################
    ## Model 0                                                           ##
    #######################################################################
    if(!is.null(model0)){
        ## print("Model 0")
        ## Reformulate outcome and covariates
        formula.model0 <- reformulate(response = outcome,
                                      termlabels = model0)
        ## Subset data
        sites <- c('Bishop Auckland', 'Hartlepool', 'Hemel Hempstead', 'Newark', 'Rochdale')
        df0 <- dplyr::filter(df.trust,
                      town %in% sites &
                      measure     == indicator &
                      sub.measure == sub.indicator)
        ##################################################
        ## Model 0 - Bishop Auckland                    ##
        ##################################################
        ## print("Bishop Auckland")
        t <- dplyr::filter(df0,
                           town        == 'Bishop Auckland')
        ## dim(t) %>% print()
        ## return(t)
        if(town.group$n[town.group$town == 'Bishop Auckland'] > 0){
            model0.panelar.bishop <- panelAR(data      = t,
                                             formula   = formula.model0,
                                             timeVar   = timevar,
                                             panelVar  = panel.trust,
                                             autoCorr  = autocorr,
                                             panelCorrMethod = 'pcse',
                                             complete.case = complete.case,
                                             seq.times = seq.times,
                                             rho.na.rm = rho.na.rm)
            results$model0.panelar.bishop.coef <- extract_coefficients(x              = model0.panelar.bishop,
                                                                       .site          = 'Bishop Auckland',
                                                                       .indicator     = indicator,
                                                                       .sub.indicator = sub.indicator)
            results$model0.panelar.bishop.r2 <- model0.panelar.bishop$r2
        }
        ##################################################
        ## Model 0 - Hartlepool                         ##
        ##################################################
        ## print("Hartlepool")
        t <- dplyr::filter(df0,
                    town        == 'Hartlepool')
        if(town.group$n[town.group$town == 'Hartlepool'] > 0){
            model0.panelar.hartlepool <- panelAR(data     = t,
                                                 formula  = formula.model0,
                                                 timeVar  = timevar,
                                                 panelVar = panel.trust,
                                                 autoCorr = autocorr,
                                                 panelCorrMethod = 'pcse',
                                                 complete.case = complete.case,
                                                 seq.times = seq.times,
                                                 rho.na.rm = rho.na.rm)
            results$model0.panelar.hartlepool.coef <- extract_coefficients(x              = model0.panelar.hartlepool,
                                                                           .site          = 'Hartlepool',
                                                                           .indicator     = indicator,
                                                                           .sub.indicator = sub.indicator)
            results$model0.panelar.hartlepool.r2 <- model0.panelar.hartlepool$r2
        }
        ##################################################
        ## Model 0 - Hemel Hempstead                    ##
        ##################################################
        ## print("Hemel Hempstead")
        t <- dplyr::filter(df0,
                    town        == 'Hemel Hempstead')
        if(town.group$n[town.group$town == 'Hemel Hempstead'] > 0){
            model0.panelar.hemel <- panelAR(data     = t,
                                            formula  = formula.model0,
                                            timeVar  = timevar,
                                            panelVar = panel.trust,
                                            autoCorr = autocorr,
                                            panelCorrMethod = 'pcse',
                                            complete.case = complete.case,
                                            seq.times = seq.times,
                                            rho.na.rm = rho.na.rm)
            results$model0.panelar.hemel.coef <- extract_coefficients(x              = model0.panelar.hemel,
                                                                      .site          = 'Hemel Hempstead',
                                                                      .indicator     = indicator,
                                                                      .sub.indicator = sub.indicator)
            results$model0.panelar.hemel.r2 <- model0.panelar.hemel$r2
        }
        ##################################################
        ## Model 0 - Newark                             ##
        ##################################################
        ## print("Newark")
        t <- dplyr::filter(df0,
                    town        == 'Newark')
        if(town.group$n[town.group$town == 'Newark'] > 0){
            model0.panelar.newark <- panelAR(data     = t,
                                             formula  = formula.model0,
                                             timeVar  = timevar,
                                             panelVar = panel.trust,
                                             autoCorr = autocorr,
                                             panelCorrMethod = 'pcse',
                                             complete.case = complete.case,
                                             seq.times = seq.times,
                                             rho.na.rm = rho.na.rm)
            results$model0.panelar.newark.coef <- extract_coefficients(x              = model0.panelar.newark,
                                                                       .site          = 'Newark',
                                                                       .indicator     = indicator,
                                                                       .sub.indicator = sub.indicator)
            results$model0.panelar.newark.r2 <- model0.panelar.newark$r2
        }
        ##################################################
        ## Model 0 - Rochdale                           ##
        ##################################################
        ## print("Rochdale")
        t <- dplyr::filter(df0,
                    town        == 'Rochdale')
        if(town.group$n[town.group$town == 'Rochdale'] > 0){
            model0.panelar.rochdale <- panelAR(data     = t,
                                               formula  = formula.model0,
                                               timeVar  = timevar,
                                               panelVar = panel.trust,
                                               autoCorr = autocorr,
                                               panelCorrMethod = 'pcse',
                                               complete.case = complete.case,
                                               seq.times = seq.times,
                                               rho.na.rm = rho.na.rm)
            results$model0.panelar.rochdale.coef <- extract_coefficients(x            = model0.panelar.rochdale,
                                                                         .site          = 'Rochdale',
                                                                         .indicator     = indicator,
                                                                         .sub.indicator = sub.indicator)
            results$model0.panelar.rochdale.r2 <- model0.panelar.rochdale$r2
        }
        ## Summary table
        results$model0.panelar.all <- combine_coefficients(bishop.coef     = results$model0.panelar.bishop.coef,
                                                           hartlepool.coef = results$model0.panelar.hartlepool.coef,
                                                           hemel.coef      = results$model0.panelar.hemel.coef,
                                                           newark.coef     = results$model0.panelar.newark.coef,
                                                           rochdale.coef   = results$model0.panelar.rochdale.coef)
        ## Forest plot
        results$model0.forest <- closed_forest(df.list       = list(results$model0.panelar.bishop.coef,
                                                                    results$model0.panelar.hartlepool.coef,
                                                                    results$model0.panelar.hemel.coef,
                                                                    results$model0.panelar.newark.coef,
                                                                    results$model0.panelar.rochdale.coef),
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
            if(exists('model0.panelar.bishop')){
                results$model0.panelar.bishop     <- model0.panelar.bishop
            }
            if(exists('model0.panelar.hartlepool')){
                results$model0.panelar.hartlepool <- model0.panelar.hartlepool
            }
            if(exists('model0.panelar.hemel')){
                results$model0.panelar.hemel      <- model0.panelar.hemel
            }
            if(exists('model0.panelar.newark')){
                results$model0.panelar.newark     <- model0.panelar.newark
            }
            if(exists('model0.panelar.rochdale')){
                results$model0.panelar.rochdale   <- model0.panelar.rochdale
            }
        }
        if(return.df == TRUE){
            results$model0.df <- df0
        }
        if(return.residuals == TRUE){
            if(exists('model0.panelar.bishop')){
                results$model0.panelar.residuals.bishop     <- summary(model0.panelar.bishop)$residuals
            }
            if(exists('model0.panelar.hartlepool')){
                results$model0.panelar.residuals.hartlepool <- summary(model0.panelar.hartlepool)$residuals
            }
            if(exists('model0.panelar.hemel')){
                results$model0.panelar.residuals.hemel      <- summary(model0.panelar.hemel)$residuals
            }
            if(exists('model0.panelar.newark')){
                results$model0.panelar.residuals.newark     <- summary(model0.panelar.newark)$residuals
            }
            if(exists('model0.panelar.rochdale')){
                results$model0.panelar.residuals.rochdale   <- summary(model0.panelar.rochdale)$residuals
            }
        }
        ## Remove clutter
        rm(df0)
    }
    #######################################################################
    ## Model 0.5                                                           ##
    #######################################################################
    if(!is.null(model0.5)){
        ## print("Model 0.5")
        ## Reformulate outcome and covariates
        formula.model0.5 <- reformulate(response = outcome,
                                      termlabels = model0.5)
        ## Subset data
        sites <- c('Bishop Auckland', 'Hartlepool', 'Hemel Hempstead', 'Newark', 'Rochdale')
        df0.5 <- dplyr::filter(df.trust,
                      town %in% sites &
                      measure     == indicator &
                      sub.measure == sub.indicator)
        ##################################################
        ## Model 0.5 - Bishop Auckland                    ##
        ##################################################
        ## print("Bishop Auckland")
        t <- dplyr::filter(df0.5,
                    town        == 'Bishop Auckland')
        ## return(t)
        if(town.group$n[town.group$town == 'Bishop Auckland'] > 0){
            model0.5.panelar.bishop <- panelAR(data      = t,
                                             formula   = formula.model0.5,
                                             timeVar   = timevar,
                                             panelVar  = panel.trust,
                                             autoCorr  = autocorr,
                                             panelCorrMethod = 'pcse',
                                             complete.case = complete.case,
                                             seq.times = seq.times,
                                             rho.na.rm = rho.na.rm)
            results$model0.5.panelar.bishop.coef <- extract_coefficients(x              = model0.5.panelar.bishop,
                                                                       .site          = 'Bishop Auckland',
                                                                       .indicator     = indicator,
                                                                       .sub.indicator = sub.indicator)
            results$model0.5.panelar.bishop.r2 <- model0.5.panelar.bishop$r2
        }
        ##################################################
        ## Model 0.5 - Hartlepool                         ##
        ##################################################
        ## print("Hartlepool")
        t <- dplyr::filter(df0.5,
                    town        == 'Hartlepool')
        if(town.group$n[town.group$town == 'Hartlepool'] > 0){
            model0.5.panelar.hartlepool <- panelAR(data     = t,
                                                 formula  = formula.model0.5,
                                                 timeVar  = timevar,
                                                 panelVar = panel.trust,
                                                 autoCorr = autocorr,
                                                 panelCorrMethod = 'pcse',
                                                 complete.case = complete.case,
                                                 seq.times = seq.times,
                                                 rho.na.rm = rho.na.rm)
            results$model0.5.panelar.hartlepool.coef <- extract_coefficients(x              = model0.5.panelar.hartlepool,
                                                                           .site          = 'Hartlepool',
                                                                           .indicator     = indicator,
                                                                           .sub.indicator = sub.indicator)
            results$model0.5.panelar.hartlepool.r2 <- model0.5.panelar.hartlepool$r2
        }
        ##################################################
        ## Model 0.5 - Hemel Hempstead                    ##
        ##################################################
        ## print("Hemel Hempstead")
        t <- dplyr::filter(df0.5,
                    town        == 'Hemel Hempstead')
        if(town.group$n[town.group$town == 'Hemel Hempstead'] > 0){
            model0.5.panelar.hemel <- panelAR(data     = t,
                                            formula  = formula.model0.5,
                                            timeVar  = timevar,
                                            panelVar = panel.trust,
                                            autoCorr = autocorr,
                                            panelCorrMethod = 'pcse',
                                            complete.case = complete.case,
                                            seq.times = seq.times,
                                            rho.na.rm = rho.na.rm)
            results$model0.5.panelar.hemel.coef <- extract_coefficients(x              = model0.5.panelar.hemel,
                                                                      .site          = 'Hemel Hempstead',
                                                                      .indicator     = indicator,
                                                                      .sub.indicator = sub.indicator)
            results$model0.5.panelar.hemel.r2 <- model0.5.panelar.hemel$r2
        }
        ##################################################
        ## Model 0.5 - Newark                             ##
        ##################################################
        ## print("Newark")
        t <- dplyr::filter(df0.5,
                    town        == 'Newark')
        if(town.group$n[town.group$town == 'Newark'] > 0){
            model0.5.panelar.newark <- panelAR(data     = t,
                                             formula  = formula.model0.5,
                                             timeVar  = timevar,
                                             panelVar = panel.trust,
                                             autoCorr = autocorr,
                                             panelCorrMethod = 'pcse',
                                             complete.case = complete.case,
                                             seq.times = seq.times,
                                             rho.na.rm = rho.na.rm)
            results$model0.5.panelar.newark.coef <- extract_coefficients(x              = model0.5.panelar.newark,
                                                                       .site          = 'Newark',
                                                                       .indicator     = indicator,
                                                                       .sub.indicator = sub.indicator)
            results$model0.5.panelar.newark.r2 <- model0.5.panelar.newark$r2
        }
        ##################################################
        ## Model 0.5 - Rochdale                           ##
        ##################################################
        ## print("Rochdale")
        t <- dplyr::filter(df0.5,
                    town        == 'Rochdale')
        if(town.group$n[town.group$town == 'Rochdale'] > 0){
            model0.5.panelar.rochdale <- panelAR(data     = t,
                                               formula  = formula.model0.5,
                                               timeVar  = timevar,
                                               panelVar = panel.trust,
                                               autoCorr = autocorr,
                                               panelCorrMethod = 'pcse',
                                               complete.case = complete.case,
                                               seq.times = seq.times,
                                               rho.na.rm = rho.na.rm)
            results$model0.5.panelar.rochdale.coef <- extract_coefficients(x            = model0.5.panelar.rochdale,
                                                                         .site          = 'Rochdale',
                                                                         .indicator     = indicator,
                                                                         .sub.indicator = sub.indicator)
            results$model0.5.panelar.rochdale.r2 <- model0.5.panelar.rochdale$r2
        }
        ## Summary table
        results$model0.5.panelar.all <- combine_coefficients(bishop.coef     = results$model0.5.panelar.bishop.coef,
                                                           hartlepool.coef = results$model0.5.panelar.hartlepool.coef,
                                                           hemel.coef      = results$model0.5.panelar.hemel.coef,
                                                           newark.coef     = results$model0.5.panelar.newark.coef,
                                                           rochdale.coef   = results$model0.5.panelar.rochdale.coef)
        ## Forest plot
        results$model0.5.forest <- closed_forest(df.list       = list(results$model0.5.panelar.bishop.coef,
                                                                    results$model0.5.panelar.hartlepool.coef,
                                                                    results$model0.5.panelar.hemel.coef,
                                                                    results$model0.5.panelar.newark.coef,
                                                                    results$model0.5.panelar.rochdale.coef),
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
            if(exists('model0.5.panelar.bishop')){
                results$model0.5.panelar.bishop     <- model0.5.panelar.bishop
            }
            if(exists('model0.5.panelar.hartlepool')){
                results$model0.5.panelar.hartlepool <- model0.5.panelar.hartlepool
            }
            if(exists('model0.5.panelar.hemel')){
                results$model0.5.panelar.hemel      <- model0.5.panelar.hemel
            }
            if(exists('model0.5.panelar.newark')){
                results$model0.5.panelar.newark     <- model0.5.panelar.newark
            }
            if(exists('model0.5.panelar.rochdale')){
                results$model0.5.panelar.rochdale   <- model0.5.panelar.rochdale
            }
        }
        if(return.df == TRUE){
            results$model0.5.df <- df0.5
        }
        if(return.residuals == TRUE){
            if(exists('model0.5.panelar.bishop')){
                results$model0.5.panelar.residuals.bishop     <- summary(model0.5.panelar.bishop)$residuals
            }
            if(exists('model0.5.panelar.hartlepool')){
                results$model0.5.panelar.residuals.hartlepool <- summary(model0.5.panelar.hartlepool)$residuals
            }
            if(exists('model0.5.panelar.hemel')){
                results$model0.5.panelar.residuals.hemel      <- summary(model0.5.panelar.hemel)$residuals
            }
            if(exists('model0.5.panelar.newark')){
                results$model0.5.panelar.residuals.newark     <- summary(model0.5.panelar.newark)$residuals
            }
            if(exists('model0.5.panelar.rochdale')){
                results$model0.5.panelar.residuals.rochdale   <- summary(model0.5.panelar.rochdale)$residuals
            }
        }
        ## Remove clutter
        rm(df0.5)
    }
    #######################################################################
    ## Model 1                                                           ##
    #######################################################################
    if(!is.null(model1)){
        ## print("Model 1")
        ## Reformulate outcome and covariates
        formula.model1 <- reformulate(response = outcome,
                                      termlabels = model1)
        ## Subset data
        sites <- c('Bishop Auckland', 'Hartlepool', 'Hemel Hempstead', 'Newark', 'Rochdale')
        df1 <- dplyr::filter(df.trust,
                      town %in% sites &
                      measure     == indicator &
                      sub.measure == sub.indicator)
        ##################################################
        ## Model 1 - Bishop Auckland                    ##
        ##################################################
        ## print("Bishop Auckland")
        t <- dplyr::filter(df1,
                    town        == 'Bishop Auckland')
        ## return(t)
        if(town.group$n[town.group$town == 'Bishop Auckland'] > 0){
            model1.panelar.bishop <- panelAR(data      = t,
                                             formula   = formula.model1,
                                             timeVar   = timevar,
                                             panelVar  = panel.trust,
                                             autoCorr  = autocorr,
                                             panelCorrMethod = 'pcse',
                                             complete.case = complete.case,
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
        ## print("Hartlepool")
        t <- dplyr::filter(df1,
                    town        == 'Hartlepool')
        if(town.group$n[town.group$town == 'Hartlepool'] > 0){
            model1.panelar.hartlepool <- panelAR(data     = t,
                                                 formula  = formula.model1,
                                                 timeVar  = timevar,
                                                 panelVar = panel.trust,
                                                 autoCorr = autocorr,
                                                 panelCorrMethod = 'pcse',
                                                 complete.case = complete.case,
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
        ## print("Hemel Hempstead")
        t <- dplyr::filter(df1,
                    town        == 'Hemel Hempstead')
        if(town.group$n[town.group$town == 'Hemel Hempstead'] > 0){
            model1.panelar.hemel <- panelAR(data     = t,
                                            formula  = formula.model1,
                                            timeVar  = timevar,
                                            panelVar = panel.trust,
                                            autoCorr = autocorr,
                                            panelCorrMethod = 'pcse',
                                            complete.case = complete.case,
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
        ## print("Newark")
        t <- dplyr::filter(df1,
                    town        == 'Newark')
        if(town.group$n[town.group$town == 'Newark'] > 0){
            model1.panelar.newark <- panelAR(data     = t,
                                             formula  = formula.model1,
                                             timeVar  = timevar,
                                             panelVar = panel.trust,
                                             autoCorr = autocorr,
                                             panelCorrMethod = 'pcse',
                                             complete.case = complete.case,
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
        ## print("Rochdale")
        t <- dplyr::filter(df1,
                    town        == 'Rochdale')
        if(town.group$n[town.group$town == 'Rochdale'] > 0){
            model1.panelar.rochdale <- panelAR(data     = t,
                                               formula  = formula.model1,
                                               timeVar  = timevar,
                                               panelVar = panel.trust,
                                               autoCorr = autocorr,
                                               panelCorrMethod = 'pcse',
                                               complete.case = complete.case,
                                               seq.times = seq.times,
                                               rho.na.rm = rho.na.rm)
            results$model1.panelar.rochdale.coef <- extract_coefficients(x            = model1.panelar.rochdale,
                                                                         .site          = 'Rochdale',
                                                                         .indicator     = indicator,
                                                                         .sub.indicator = sub.indicator)
            results$model1.panelar.rochdale.r2 <- model1.panelar.rochdale$r2
        }
        ## Summary table
        results$model1.panelar.all <- combine_coefficients(bishop.coef     = results$model1.panelar.bishop.coef,
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
            if(exists('model1.panelar.bishop')){
                results$model1.panelar.bishop     <- model1.panelar.bishop
            }
            if(exists('model1.panelar.hartlepool')){
                results$model1.panelar.hartlepool <- model1.panelar.hartlepool
            }
            if(exists('model1.panelar.hemel')){
                results$model1.panelar.hemel      <- model1.panelar.hemel
            }
            if(exists('model1.panelar.newark')){
                results$model1.panelar.newark     <- model1.panelar.newark
            }
            if(exists('model1.panelar.rochdale')){
                results$model1.panelar.rochdale   <- model1.panelar.rochdale
            }
        }
        if(return.df == TRUE){
            results$model1.df <- df1
        }
        if(return.residuals == TRUE){
            if(exists('model1.panelar.bishop')){
                results$model1.panelar.residuals.bishop     <- summary(model1.panelar.bishop)$residuals
            }
            if(exists('model1.panelar.hartlepool')){
                results$model1.panelar.residuals.hartlepool <- summary(model1.panelar.hartlepool)$residuals
            }
            if(exists('model1.panelar.hemel')){
                results$model1.panelar.residuals.hemel      <- summary(model1.panelar.hemel)$residuals
            }
            if(exists('model1.panelar.newark')){
                results$model1.panelar.residuals.newark     <- summary(model1.panelar.newark)$residuals
            }
            if(exists('model1.panelar.rochdale')){
                results$model1.panelar.residuals.rochdale   <- summary(model1.panelar.rochdale)$residuals
            }
        }
        ## Remove clutter
        rm(df1)
    }
    #######################################################################
    ## Model 2                                                           ##
    #######################################################################
    if(!is.null(model2)){
        ## print("Model 2")
        ## print(model2)
        ## Reformulate outcome and covariates
        formula.model2 <- reformulate(response = outcome,
                                      termlabels = model2)
        ## Subset data
        sites <- c('Bishop Auckland', 'Whitehaven',
                   'Hartlepool', 'Grimsby',
                   'Hemel Hempstead', 'Warwick',
                   'Newark', 'Southport',
                   'Rochdale', 'Rotherham')
        df2 <- dplyr::filter(df.trust, town %in% sites &
                      measure     == indicator &
                      sub.measure == sub.indicator)
        ## Perform analysis with panelAR in each
        ##################################################
        ## Model 2 - Bishop Auckland                    ##
        ##################################################
        ## print("Bishop Auckland")
        t <- dplyr::filter(df2,
                    town == 'Bishop Auckland' |
                    town == 'Whitehaven')
        whitehaven <- dplyr::filter(df2, town == 'Whitehaven') %>% count()
        if(town.group$n[town.group$town == 'Bishop Auckland'] > 0 & town.group$n[town.group$town == 'Whitehaven'] > 0){
            t$town <- relevel(t$town, ref = 'Whitehaven')
            model2.panelar.bishop <- panelAR(data     = t,
                                             formula  = formula.model2,
                                             timeVar  = timevar,
                                             panelVar = panel.trust,
                                             autoCorr = autocorr,
                                             panelCorrMethod = 'pcse',
                                             complete.case = complete.case,
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
        ## print("Hartlepool")
        t <- dplyr::filter(df2,
                    town == 'Hartlepool' |
                    town == 'Grimsby')
        grimsby <- dplyr::filter(df2, town == 'Grimsby') %>% count()
        if(town.group$n[town.group$town == 'Hartlepool'] > 0 & town.group$n[town.group$town == 'Grimsby'] > 0){
            t$town <- relevel(t$town, ref = 'Grimsby')
            model2.panelar.hartlepool <- panelAR(data     = t,
                                                 formula  = formula.model2,
                                                 timeVar  = timevar,
                                                 panelVar = panel.trust,
                                                 autoCorr = autocorr,
                                                 panelCorrMethod = 'pcse',
                                                 complete.case = complete.case,
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
        ## print("Hemel Hempstead")
        t <- dplyr::filter(df2,
                    town == 'Hemel Hempstead' |
                    town == 'Warwick')
        warwick <- dplyr::filter(df2, town == 'Warwick') %>% count()
        if(town.group$n[town.group$town == 'Hemel Hempstead'] > 0 & town.group$n[town.group$town == 'Warwick']  > 0){
            t$town <- relevel(t$town, ref = 'Warwick')
            model2.panelar.hemel <- panelAR(data     = t,
                                            formula  = formula.model2,
                                            timeVar  = timevar,
                                            panelVar = panel.trust,
                                            autoCorr = autocorr,
                                            panelCorrMethod = 'pcse',
                                            complete.case = complete.case,
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
        ## print("Newark")
        t <- dplyr::filter(df2,
                    town == 'Newark' |
                    town == 'Southport')
        southport <- dplyr::filter(df2, town == 'Southport') %>% count()
        if(town.group$n[town.group$town == 'Newark'] > 0 & town.group$n[town.group$town == 'Southport']  > 0){
            t$town <- relevel(t$town, ref = 'Southport')
            model2.panelar.newark <- panelAR(data     = t,
                                             formula  = formula.model2,
                                             timeVar  = timevar,
                                             panelVar = panel.trust,
                                             autoCorr = autocorr,
                                             panelCorrMethod = 'pcse',
                                             complete.case = complete.case,
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
        ## print("Rochdale")
        t <- dplyr::filter(df2,
                    town == 'Rochdale' |
                    town == 'Rotherham')
        rotherham <- dplyr::filter(df2, town == 'Rotherham') %>% count()
        if(town.group$n[town.group$town == 'Rochdale'] > 0 & town.group$n[town.group$town == 'Rotherham'] > 0){
            t$town <- relevel(t$town, ref = 'Rotherham')
            model2.panelar.rochdale <- panelAR(data     = t,
                                               formula  = formula.model2,
                                               timeVar  = timevar,
                                               panelVar = panel.trust,
                                               autoCorr = autocorr,
                                               panelCorrMethod = 'pcse',
                                               complete.case = complete.case,
                                               seq.times = seq.times,
                                               rho.na.rm = rho.na.rm)
            results$model2.panelar.rochdale.coef <- extract_coefficients(x            = model2.panelar.rochdale,
                                                                         .site          = 'Rochdale',
                                                                         .indicator     = indicator,
                                                                   .sub.indicator = sub.indicator)
            results$model2.panelar.rochdale.r2 <- model2.panelar.rochdale$r2
        }
        ## Summary table
        results$model2.panelar.all <- combine_coefficients(bishop.coef     = results$model2.panelar.bishop.coef,
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
            if(exists('model2.panelar.bishop')){
                results$model2.panelar.bishop     <- model2.panelar.bishop
            }
            if(exists('model2.panelar.hartlepool')){
                results$model2.panelar.hartlepool <- model2.panelar.hartlepool
            }
            if(exists('model2.panelar.hemel')){
                results$model2.panelar.hemel      <- model2.panelar.hemel
            }
            if(exists('model2.panelar.newark')){
                results$model2.panelar.newark     <- model2.panelar.newark
            }
            if(exists('model2.panelar.rochdale')){
                results$model2.panelar.rochdale   <- model2.panelar.rochdale
            }
        }
        if(return.df == TRUE){
            results$model2.df <- df2
        }
        if(return.residuals == TRUE){
            if(exists('model2.panelar.bishop')){
                results$model2.panelar.residuals.bishop     <- summary(model2.panelar.bishop)$residuals
            }
            if(exists('model2.panelar.hartlepool')){
                results$model2.panelar.residuals.hartlepool <- summary(model2.panelar.hartlepool)$residuals
            }
            if(exists('model2.panelar.hemel')){
                results$model2.panelar.residuals.hemel      <- summary(model2.panelar.hemel)$residuals
            }
            if(exists('model2.panelar.newark')){
                results$model2.panelar.residuals.newark     <- summary(model2.panelar.newark)$residuals
            }
            if(exists('model2.panelar.rochdale')){
                results$model2.panelar.residuals.rochdale   <- summary(model2.panelar.rochdale)$residuals
            }
        }
        ## Remove clutter
        rm(df2)
    }
    #######################################################################
    ## Model 3.1                                                         ##
    #######################################################################
    if(!is.null(model3.1)){
        ## print("Model 3.1")
        ## Reformulate outcome and covariates
        formula.model3.1 <- reformulate(response = outcome,
                                      termlabels = model3.1)
        ## Subset data
        df3 <- dplyr::filter(df.trust,
                      measure     == indicator &
                      sub.measure == sub.indicator)
        ## Generate time-series plot
        df3$group <- paste0('Cohort : ', df3$group)
        sites <- c('Basingstoke', 'Bishop Auckland', 'Blackburn', 'Carlisle',
                   'Grimsby', 'Hartlepool', 'Hemel Hempstead', 'Newark',
                   'Rochdale', 'Rotherham', 'Salford', 'Salisbury',
                   'Scarborough', 'Scunthorpe', 'Southport', 'Wansbeck',
                   'Warwick', 'Whitehaven', 'Wigan', 'Yeovil')
        ## Perform analysis with panelAR in each
        ##################################################
        ## Bishop Auckland                              ##
        ##################################################
        ## print("Bishop Auckland")
        t <- dplyr::filter(df3,
                    group == 'Cohort : Bishop Auckland General Hospital')
        if(town.group$n[town.group$town == 'Bishop Auckland'] > 0 &
           town.group$n[town.group$town == 'Salford'] > 0 &
           town.group$n[town.group$town == 'Scarborough'] > 0 &
           town.group$n[town.group$town == 'Whitehaven'] > 0){
            t$town <- relevel(t$town, ref = 'Whitehaven')
            model3.1.panelar.bishop <- panelAR(data     = t,
                                             formula  = formula.model3.1,
                                             timeVar  = timevar,
                                             panelVar = panel.trust,
                                             autoCorr = autocorr,
                                             panelCorrMethod = 'pcse',
                                             complete.case = complete.case,
                                             seq.times = seq.times,
                                             rho.na.rm = rho.na.rm)
            results$model3.1.panelar.bishop.coef <- extract_coefficients(x              = model3.1.panelar.bishop,
                                                                       .site          = 'Bishop Auckland',
                                                                       .indicator     = indicator,
                                                                       .sub.indicator = sub.indicator)
            results$model3.1.panelar.bishop.r2 <- model3.1.panelar.bishop$r2
        }
        ##################################################
        ## Hartlepool                                   ##
        ##################################################
        ## print("Hartlepool")
        t <- dplyr::filter(df3,
                    group == 'Cohort : University Hospital of Hartlepool')
        if(town.group$n[town.group$town == 'Hartlepool'] > 0 &
           town.group$n[town.group$town == 'Blackburn'] > 0 &
           town.group$n[town.group$town == 'Grimsby'] > 0 &
           town.group$n[town.group$town == 'Wigan'] > 0){
            t$town <- relevel(t$town, ref = 'Grimsby')
            model3.1.panelar.hartlepool <- panelAR(data     = t,
                                                 formula  = formula.model3.1,
                                                 timeVar  = timevar,
                                                 panelVar = panel.trust,
                                                 autoCorr = autocorr,
                                                 panelCorrMethod = 'pcse',
                                                 complete.case = complete.case,
                                                 seq.times = seq.times,
                                                 rho.na.rm = rho.na.rm)
            results$model3.1.panelar.hartlepool.coef <- extract_coefficients(x             = model3.1.panelar.hartlepool,
                                                                           .site          = 'Hartlepool',
                                                                           .indicator     = indicator,
                                                                           .sub.indicator = sub.indicator)
            results$model3.1.panelar.hartlepool.r2 <- model3.1.panelar.hartlepool$r2
        }
        ##################################################
        ## Hemel Hempstead                              ##
        ##################################################
        ## print("Hemel Hempstead")
        t <- dplyr::filter(df3,
                    group == 'Cohort : Hemel Hempstead Hospital')
        if(town.group$n[town.group$town == 'Hemel Hempstead'] > 0 &
           town.group$n[town.group$town == 'Basingstoke'] > 0 &
           town.group$n[town.group$town == 'Warwick'] > 0 &
           town.group$n[town.group$town == 'Yeovil'] > 0){
            t$town <- relevel(t$town, ref = 'Warwick')
            model3.1.panelar.hemel <- panelAR(data     = t,
                                            formula  = formula.model3.1,
                                            timeVar  = timevar,
                                            panelVar = panel.trust,
                                            autoCorr = autocorr,
                                            panelCorrMethod = 'pcse',
                                            complete.case = complete.case,
                                            seq.times = seq.times,
                                            rho.na.rm = rho.na.rm)
            results$model3.1.panelar.hemel.coef <- extract_coefficients(x              = model3.1.panelar.hemel,
                                                                      .site          = 'Hemel Hempstead',
                                                                      .indicator     = indicator,
                                                                      .sub.indicator = sub.indicator)
            results$model3.1.panelar.hemel.r2 <- model3.1.panelar.hemel$r2
        }
        ##################################################
        ## Newark                                       ##
        ##################################################
        ## print("Newark")
        t <- dplyr::filter(df3,
                    group == 'Cohort : Newark Hospital')
        if(town.group$n[town.group$town == 'Newark'] > 0 &
           town.group$n[town.group$town == 'Carlisle'] > 0 &
           town.group$n[town.group$town == 'Salisbury'] > 0 &
           town.group$n[town.group$town == 'Southport'] > 0){
            t$town <- relevel(t$town, ref = 'Southport')
            model3.1.panelar.newark <- panelAR(data     = t,
                                             formula  = formula.model3.1,
                                             timeVar  = timevar,
                                             panelVar = panel.trust,
                                             autoCorr = autocorr,
                                             panelCorrMethod = 'pcse',
                                             complete.case = complete.case,
                                             seq.times = seq.times,
                                             rho.na.rm = rho.na.rm)
            results$model3.1.panelar.newark.coef <- extract_coefficients(x              = model3.1.panelar.newark,
                                                                       .site          = 'Newark',
                                                                       .indicator     = indicator,
                                                                       .sub.indicator = sub.indicator)
            results$model3.1.panelar.newark.r2 <- model3.1.panelar.newark$r2
        }
        ##################################################
        ## Rochdale                                     ##
        ##################################################
        ## print("Rochdale")
        t <- dplyr::filter(df3,
                    group == 'Cohort : Rochdale Infirmary')
        if(town.group$n[town.group$town == 'Rochdale'] > 0 &
           town.group$n[town.group$town == 'Rotherham'] > 0 &
           town.group$n[town.group$town == 'Scunthorpe'] > 0 &
           town.group$n[town.group$town == 'Wansbeck'] > 0){
            t$town <- relevel(t$town, ref = 'Rotherham')
            model3.1.panelar.rochdale <- panelAR(data     = t,
                                               formula  = formula.model3.1,
                                               timeVar  = timevar,
                                               panelVar = panel.trust,
                                               autoCorr = autocorr,
                                               panelCorrMethod = 'pcse',
                                               complete.case = complete.case,
                                               seq.times = seq.times,
                                               rho.na.rm = rho.na.rm)
            results$model3.1.panelar.rochdale.coef <- extract_coefficients(x            = model3.1.panelar.rochdale,
                                                                         .site          = 'Rochdale',
                                                                         .indicator     = indicator,
                                                                         .sub.indicator = sub.indicator)
            results$model3.1.panelar.rochdale.r2 <- model3.1.panelar.rochdale$r2
        }
        ## Summary table
        if(!is.null(results$model3.1.panelar.bishop.coef) |
           !is.null(results$model3.1.panelar.hartlepool.coef) |
           !is.null(results$model3.1.panelar.hemel.coef) |
           !is.null(results$model3.1.panelar.newark.coef) |
           !is.null(results$model3.1.panelar.rochdale.coef)){
            results$model3.1.panelar.all <- combine_coefficients(bishop.coef     = results$model3.1.panelar.bishop.coef,
                                                                hartlepool.coef = results$model3.1.panelar.hartlepool.coef,
                                                                hemel.coef      = results$model3.1.panelar.hemel.coef,
                                                                newark.coef     = results$model3.1.panelar.newark.coef,
                                                                rochdale.coef   = results$model3.1.panelar.rochdale.coef)
            ## Forest plot
            results$model3.1.forest <- closed_forest(df.list       = list(results$model3.1.panelar.bishop.coef,
                                                                         results$model3.1.panelar.hartlepool.coef,
                                                                         results$model3.1.panelar.hemel.coef,
                                                                         results$model3.1.panelar.newark.coef,
                                                                         results$model3.1.panelar.rochdale.coef),
                                                    plot.term     = c('closure'),
                                                    facet.outcome = FALSE,
                                                    title         = paste0('Model 3 : ',
                                                                           indicator,
                                                                           ' (',
                                                                           sub.indicator,
                                                                           ')'),
                                                    theme         = theme_bw())
        }
        ## Return model objects if requested
        if(return.model == TRUE){
            if(exists('model3.1.panelar.bishop')){
                results$model3.1.panelar.bishop     <- model3.1.panelar.bishop
            }
            if(exists('model3.1.panelar.hartlepool')){
                results$model3.1.panelar.hartlepool <- model3.1.panelar.hartlepool
            }
            if(exists('model3.1.panelar.hemel')){
                results$model3.1.panelar.hemel      <- model3.1.panelar.hemel
            }
            if(exists('model3.1.panelar.newark')){
                results$model3.1.panelar.newark     <- model3.1.panelar.newark
            }
            if(exists('model3.1.panelar.rochdale')){
                results$model3.1.panelar.rochdale   <- model3.1.panelar.rochdale
            }
        }
        if(return.df == TRUE){
            results$model3.1.df <- df3
        }
        if(return.residuals == TRUE){
            if(exists('model3.1.panelar.bishop')){
                results$model3.1.panelar.residuals.bishop     <- summary(model3.1.panelar.bishop)$residuals
            }
            if(exists('model3.1.panelar.hartlepool')){
                results$model3.1.panelar.residuals.hartlepool <- summary(model3.1.panelar.hartlepool)$residuals
            }
            if(exists('model3.1.panelar.hemel')){
                results$model3.1.panelar.residuals.hemel      <- summary(model3.1.panelar.hemel)$residuals
            }
            if(exists('model3.1.panelar.newark')){
                results$model3.1.panelar.residuals.newark     <- summary(model3.1.panelar.newark)$residuals
            }
            if(exists('model3.1.panelar.rochdale')){
                results$model3.1.panelar.residuals.rochdale   <- summary(model3.1.panelar.rochdale)$residuals
            }
        }
        ## Remove clutter
        rm(df3)
    }
    #######################################################################
    ## Model 3.2                                                         ##
    #######################################################################
    if(!is.null(model3.2)){
        ## print("Model 3.2")
        ## Reformulate outcome and covariates
        formula.model3.2 <- reformulate(response = outcome,
                                      termlabels = model3.2)
        ## Pool the data
        df3.2 <- closed_pool(df             = df.trust,
                             within.centres = TRUE)
        ## Set reference group for pooled controls
        df3.2$pooled.control <- relevel(df3.2$pooled.control, ref = 'Control')
        ## Ensure relative.month is a numeric integer
        df3.2$relative.month <- as.integer(df3.2$relative.month) %>% as.numeric()
        ##################################################
        ## Bishop Auckland                              ##
        ##################################################
        ## print("Bishop Auckland")
        t <- dplyr::filter(df3.2,
                    group == 'Bishop Auckland General Hospital') %>%
            as.data.frame()
        if(town.group$n[town.group$town == 'Bishop Auckland'] > 0 &
           town.group$n[town.group$town == 'Salford'] > 0 &
           town.group$n[town.group$town == 'Scarborough'] > 0 &
           town.group$n[town.group$town == 'Whitehaven'] > 0){
            ## print('Bishop Auckland')
            model3.2.panelar.bishop <- panelAR(data     = t,
                                              formula  = formula.model3.2,
                                              timeVar  = timevar,
                                              panelVar = panel.trust,
                                              autoCorr = autocorr,
                                              panelCorrMethod = 'pcse',
                                              complete.case = complete.case,
                                              seq.times = seq.times,
                                              rho.na.rm = rho.na.rm)
            results$model3.2.panelar.bishop.coef <- extract_coefficients(x              = model3.2.panelar.bishop,
                                                                       .site          = 'Bishop Auckland',
                                                                   .indicator     = indicator,
                                                                   .sub.indicator = sub.indicator)
            results$model3.2.panelar.bishop.r2 <- model3.2.panelar.bishop$r2
        }
        ##################################################
        ## Hartlepool                                   ##
        ##################################################
        ## print("Hartlepool")
        t <- dplyr::filter(df3.2,
                    group == 'University Hospital of Hartlepool') %>%
            as.data.frame()
        if(town.group$n[town.group$town == 'Hartlepool'] > 0 &
           town.group$n[town.group$town == 'Blackburn'] > 0 &
           town.group$n[town.group$town == 'Grimsby'] > 0 &
           town.group$n[town.group$town == 'Wigan'] > 0){
            ## print('Hartlepool')
            model3.2.panelar.hartlepool <- panelAR(data     = t,
                                                  formula  = formula.model3.2,
                                                   timeVar  = timevar,
                                                   panelVar = panel.trust,
                                                   autoCorr = autocorr,
                                                   panelCorrMethod = 'pcse',
                                                  complete.case = complete.case,
                                                   seq.times = seq.times,
                                                   rho.na.rm = rho.na.rm)
            results$model3.2.panelar.hartlepool.coef <- extract_coefficients(x             = model3.2.panelar.hartlepool,
                                                                           .site          = 'Hartlepool',
                                                                           .indicator     = indicator,
                                                                           .sub.indicator = sub.indicator)
            results$model3.2.panelar.hartlepool.r2 <- model3.2.panelar.hartlepool$r2
        }
        ##################################################
        ## Hemel Hempstead                              ##
        ##################################################
        ## print("Hemel Hempstead")
        t <- dplyr::filter(df3.2,
                    group == 'Hemel Hempstead Hospital') %>%
             as.data.frame()
        if(town.group$n[town.group$town == 'Hemel Hempstead'] > 0 &
           town.group$n[town.group$town == 'Basingstoke'] > 0 &
           town.group$n[town.group$town == 'Warwick'] > 0 &
           town.group$n[town.group$town == 'Yeovil'] > 0){
            ## print('Hemel Hempstead')
            model3.2.panelar.hemel <- panelAR(data     = t,
                                            formula  = formula.model3.2,
                                            timeVar  = timevar,
                                            panelVar = panel.trust,
                                            autoCorr = autocorr,
                                            panelCorrMethod = 'pcse',
                                            complete.case = complete.case,
                                            seq.times = seq.times,
                                            rho.na.rm = rho.na.rm)
            results$model3.2.panelar.hemel.coef <- extract_coefficients(x              = model3.2.panelar.hemel,
                                                                      .site          = 'Hemel Hempstead',
                                                                      .indicator     = indicator,
                                                                      .sub.indicator = sub.indicator)
            results$model3.2.panelar.hemel.r2 <- model3.2.panelar.hemel$r2
        }
        ##################################################
        ## Newark                                       ##
        ##################################################
        ## print("Newark")
        t <- dplyr::filter(df3.2,
                    group == 'Newark Hospital') %>%
             as.data.frame()
        if(town.group$n[town.group$town == 'Newark'] > 0 &
           town.group$n[town.group$town == 'Carlisle'] > 0 &
           town.group$n[town.group$town == 'Salisbury'] > 0 &
           town.group$n[town.group$town == 'Southport'] > 0){
            ## print('Newark')
            model3.2.panelar.newark <- panelAR(data     = t,
                                             formula  = formula.model3.2,
                                             timeVar  = timevar,
                                             panelVar = panel.trust,
                                             autoCorr = autocorr,
                                             panelCorrMethod = 'pcse',
                                             complete.case = complete.case,
                                             seq.times = seq.times,
                                             rho.na.rm = rho.na.rm)
            results$model3.2.panelar.newark.coef <- extract_coefficients(x              = model3.2.panelar.newark,
                                                                       .site          = 'Newark',
                                                                       .indicator     = indicator,
                                                                       .sub.indicator = sub.indicator)
            results$model3.2.panelar.newark.r2 <- model3.2.panelar.newark$r2
        }
        ##################################################
        ## Rochdale                                     ##
        ##################################################
        ## print("Rochdale")
        t <- dplyr::filter(df3.2,
                    group == 'Rochdale Infirmary') %>%
            as.data.frame()
        if(town.group$n[town.group$town == 'Rochdale'] > 0 &
           town.group$n[town.group$town == 'Rotherham'] > 0 &
           town.group$n[town.group$town == 'Scunthorpe'] > 0 &
           town.group$n[town.group$town == 'Wansbeck'] > 0){
            ## print('Rochdale')
            model3.2.panelar.rochdale <- panelAR(data     = t,
                                               formula  = formula.model3.2,
                                               timeVar  = timevar,
                                               panelVar = panel.trust,
                                               autoCorr = autocorr,
                                               panelCorrMethod = 'pcse',
                                               complete.case = complete.case,
                                               seq.times = seq.times,
                                               rho.na.rm = rho.na.rm)
            results$model3.2.panelar.rochdale.coef <- extract_coefficients(x            = model3.2.panelar.rochdale,
                                                                         .site          = 'Rochdale',
                                                                         .indicator     = indicator,
                                                                         .sub.indicator = sub.indicator)
            results$model3.2.panelar.rochdale.r2 <- model3.2.panelar.rochdale$r2
        }
        ##################################################
        ## All sites                                    ##
        ##################################################
        ## TODO - Maybe get this working
        ## Pool the data across centres
        ## df3.2 <- closed_pool(df             = df.trust,
        ##                    within.centres = FALSE)
        ## ## Remove 'season' from the model since that is only feasible when pooling within
        ## ## centres because months/dates do not align
        ## model3.2 <- gsub('season', '', model3.2)
        ## formula.model3.2 <- reformulate(response = outcome,
        ##                               termlabels = model3.2)
        ## t <- dplyr::filter(df3.2,
        ##             measure     == indicator &
        ##             sub.measure == sub.indicator) %>%
        ##      as.data.frame()
        ## if(town.group$n[town.group$town == 'Bishop Auckland'] > 0 &
        ##    town.group$n[town.group$town == 'Salford'] > 0 &
        ##    town.group$n[town.group$town == 'Scarborough'] > 0 &
        ##    town.group$n[town.group$town == 'Whitehaven'] > 0 &
        ##    town.group$n[town.group$town == 'Hartlepool'] > 0 &
        ##    town.group$n[town.group$town == 'Blackburn'] > 0 &
        ##    town.group$n[town.group$town == 'Grimsby'] > 0 &
        ##    town.group$n[town.group$town == 'Wigan'] > 0 &
        ##    town.group$n[town.group$town == 'Hemel Hempstead'] > 0 &
        ##    town.group$n[town.group$town == 'Basingstoke'] > 0 &
        ##    town.group$n[town.group$town == 'Warwick'] > 0 &
        ##    town.group$n[town.group$town == 'Yeovil'] > 0 &
        ##    town.group$n[town.group$town == 'Newark'] > 0 &
        ##    town.group$n[town.group$town == 'Carlisle'] > 0 &
        ##    town.group$n[town.group$town == 'Salisbury'] > 0 &
        ##    town.group$n[town.group$town == 'Southport'] > 0 &
        ##    town.group$n[town.group$town == 'Rochdale'] > 0 &
        ##    town.group$n[town.group$town == 'Rotherham'] > 0 &
        ##    town.group$n[town.group$town == 'Scunthorpe'] > 0 &
        ##    town.group$n[town.group$town == 'Wansbeck'] > 0){
        ##     table(t$town) %>% print()
        ##     formula.model3.2 %>% print()
        ##     model3.2.panelar.all <- panelAR(data     = t,
        ##                                    formula  = formula.model3.2,
        ##                                    timeVar  = timevar,
        ##                                    panelVar = panel.trust,
        ##                                    autoCorr = autocorr,
        ##                                    panelCorrMethod = 'pcse',
        ##                                    seq.times = seq.times,
        ##                                    rho.na.rm = rho.na.rm)
        ##     results$model3.2.panelar.all.coef <- extract_coefficients(x              = model3.2.panelar.all,
        ##                                                              .site          = 'All',
        ##                                                              .indicator     = indicator,
        ##                                                              .sub.indicator = sub.indicator)
        ##     results$model3.2.panelar.r2 <- model3.2.panelar.all
        ## }
        ## Summary table
        if(!is.null(results$model3.2.panelar.bishop.coef) |
           !is.null(results$model3.2.panelar.hartlepool.coef) |
           !is.null(results$model3.2.panelar.hemel.coef) |
           !is.null(results$model3.2.panelar.newark.coef) |
           !is.null(results$model3.2.panelar.rochdale.coef)){
           ## !is.null(results$model3.2.panelar.all.coef)){
            results$model3.2.panelar.all <- combine_coefficients(bishop.coef     = results$model3.2.panelar.bishop.coef,
                                                                hartlepool.coef = results$model3.2.panelar.hartlepool.coef,
                                                                hemel.coef      = results$model3.2.panelar.hemel.coef,
                                                                newark.coef     = results$model3.2.panelar.newark.coef,
                                                                rochdale.coef   = results$model3.2.panelar.rochdale.coef,
                                                                all.coef        = results$model3.2.panelar.all.coef)
            ## Forest plot
            results$model3.2.forest <- closed_forest(df.list       = list(results$model3.2.panelar.bishop.coef,
                                                                         results$model3.2.panelar.hartlepool.coef,
                                                                         results$model3.2.panelar.hemel.coef,
                                                                         results$model3.2.panelar.newark.coef,
                                                                         results$model3.2.panelar.rochdale.coef,
                                                                         results$model3.2.panelar.all.coef),
                                                    plot.term     = c('closure'),
                                                    facet.outcome = FALSE,
                                                    title         = paste0('Model 8 : ',
                                                                           indicator,
                                                                           ' (',
                                                                           sub.indicator,
                                                                           ')'),
                                                    theme         = theme_bw())
        }
        ## Return model objects if requested
        if(return.model == TRUE){
            if(exists('model3.2.panelar.bishop')){
                results$model3.2.panelar.bishop     <- model3.2.panelar.bishop
            }
            if(exists('model3.2.panelar.hartlepool')){
                results$model3.2.panelar.hartlepool <- model3.2.panelar.hartlepool
            }
            if(exists('model3.2.panelar.hemel')){
                results$model3.2.panelar.hemel      <- model3.2.panelar.hemel
            }
            if(exists('model3.2.panelar.newark')){
                results$model3.2.panelar.newark     <- model3.2.panelar.newark
            }
            if(exists('model3.2.panelar.rochdale')){
                results$model3.2.panelar.rochdale   <- model3.2.panelar.rochdale
            }
        }
        if(return.df == TRUE){
            results$model3.2.df <- df3.2
        }
        if(return.residuals == TRUE){
            if(exists('model3.2.panelar.bishop')){
                results$model3.2.panelar.residuals.bishop     <- summary(model3.2.panelar.bishop)$residuals
            }
            if(exists('model3.2.panelar.hartlepool')){
                results$model3.2.panelar.residuals.hartlepool <- summary(model3.2.panelar.hartlepool)$residuals
            }
            if(exists('model3.2.panelar.hemel')){
                results$model3.2.panelar.residuals.hemel      <- summary(model3.2.panelar.hemel)$residuals
            }
            if(exists('model3.2.panelar.newark')){
                results$model3.2.panelar.residuals.newark     <- summary(model3.2.panelar.newark)$residuals
            }
            if(exists('model3.2.panelar.rochdale')){
                results$model3.2.panelar.residuals.rochdale   <- summary(model3.2.panelar.rochdale)$residuals
            }
        }
        ## Remove clutter
        rm(df3.2)
    }
    #######################################################################
    ## Model 4                                                           ##
    #######################################################################
    if(!is.null(model4)){
        ## print("Model 4")
        ## Reformulate outcome and covariates
        formula.model4 <- reformulate(response = outcome,
                                      termlabels = model4)
        ## Subset data
        sites <- c('Bishop Auckland', 'Whitehaven',
                   'Hartlepool', 'Grimsby',
                   'Hemel Hempstead', 'Warwick',
                   'Newark', 'Southport',
                   'Rochdale', 'Rotherham')
        df4 <- dplyr::filter(df.trust, town %in% sites  &
                      measure     == indicator &
                      sub.measure == sub.indicator)
        ## Add in indicator of case/control status for plotting
        case <- c('Bishop Auckland',
                  'Hartlepool',
                  'Hemel Hempstead',
                  'Newark',
                  'Rochdale')
        df4$status <- ifelse(df4$town %in% case, 'Case', 'Control')
        ## Perform analysis with panelAR in each
        ##################################################
        ## All                                          ##
        ##################################################
        t <- df4
        if(town.group$n[town.group$town == 'Bishop Auckland'] > 0 &
           town.group$n[town.group$town == 'Whitehaven'] > 0 &
           town.group$n[town.group$town == 'Hartlepool'] > 0 &
           town.group$n[town.group$town == 'Grimsby'] > 0 &
           town.group$n[town.group$town == 'Hemel Hempstead'] > 0 &
           town.group$n[town.group$town == 'Warwick'] > 0 &
           town.group$n[town.group$town == 'Newark'] > 0 &
           town.group$n[town.group$town == 'Southport'] > 0 &
           town.group$n[town.group$town == 'Rochdale'] > 0 &
           town.group$n[town.group$town == 'Rotherham'] > 0){
            t$town <- relevel(t$town, ref = 'Grimsby')
            model4.panelar <- panelAR(data     = t,
                                      formula  = formula.model4,
                                      timeVar  = timevar,
                                      panelVar = panel.trust,
                                      autoCorr = autocorr,
                                      panelCorrMethod = 'pcse',
                                      complete.case = complete.case,
                                      seq.times = seq.times,
                                      rho.na.rm = rho.na.rm)
            results$model4.panelar.all.coef <- extract_coefficients(x              = model4.panelar,
                                                                .site          = 'All',
                                                                .indicator     = indicator,
                                                                .sub.indicator = sub.indicator)
            results$model4.panelar.r2 <- model4.panelar$r2
        }
        ## Summary table
        if(!is.null(results$model2.panelar.bishop.coef) |
           !is.null(results$model2.panelar.hartlepool.coef) |
           !is.null(results$model2.panelar.hemel.coef) |
           !is.null(results$model2.panelar.newark.coef) |
           !is.null(results$model2.panelar.rochdale.coef) |
           !is.null(results$model4.panelar.all.coef)){
            results$model4.panelar.all <- combine_coefficients(bishop.coef     = results$model2.panelar.bishop.coef,
                                                               hartlepool.coef = results$model2.panelar.hartlepool.coef,
                                                               hemel.coef      = results$model2.panelar.hemel.coef,
                                                               newark.coef     = results$model2.panelar.newark.coef,
                                                               rochdale.coef   = results$model2.panelar.rochdale.coef,
                                                               all.coef        = results$model4.panelar.all.coef)
            ## Forest plot
            results$model4.forest <- closed_forest(df.list       = list(results$model2.panelar.bishop.coef,
                                                                        results$model2.panelar.hartlepool.coef,
                                                                        results$model2.panelar.hemel.coef,
                                                                        results$model2.panelar.newark.coef,
                                                                        results$model2.panelar.rochdale.coef,
                                                                        results$model4.panelar.all.coef),
                                                   plot.term     = c('closure'),
                                                   facet.outcome = FALSE,
                                                   title         = paste0('Model 2 & 4 : ',
                                                                          indicator,
                                                                          ' (',
                                                                          sub.indicator,
                                                                          ')'),
                                                   theme         = theme_bw())
        }
        ## Return model objects if requested
        if(return.model == TRUE){
            if(exists('model4.panelar')){
                results$model4.panelar     <- model4.panelar
            }
        }
        if(return.df == TRUE){
            results$model4.df <- df4
        }
        if(return.residuals == TRUE){
            if(exists('model4.panelar')){
                results$model4.panelar.residuals     <- summary(model4.panelar)$residuals
            }
        }
        ## Remove clutter
        rm(df4)
    }
    #######################################################################
    ## Model 5                                                           ##
    #######################################################################
    if(!is.null(model5)){
        ## print("Model 5")
        ## Reformulate outcome and covariates
        formula.model5 <- reformulate(response = outcome,
                                      termlabels = model5)
        ## Subset data
        df5 <- dplyr::filter(df.trust,
                      measure     == indicator &
                      sub.measure == sub.indicator)
        ## Add in indicator of case/control status for plotting
        case <- c('Bishop Auckland',
                  'Hartlepool',
                  'Hemel Hempstead',
                  'Newark',
                  'Rochdale')
        df5$status <- ifelse(df5$town %in% case, 'Case', 'Control')
        ## Perform analysis with panelAR in each
        ##################################################
        ## All sites                                    ##
        ##################################################
        t <- df5
        if(town.group$n[town.group$town == 'Bishop Auckland'] > 0 &
           town.group$n[town.group$town == 'Salford'] > 0 &
           town.group$n[town.group$town == 'Scarborough'] > 0 &
           town.group$n[town.group$town == 'Whitehaven'] > 0 &
           town.group$n[town.group$town == 'Hartlepool'] > 0 &
           town.group$n[town.group$town == 'Blackburn'] > 0 &
           town.group$n[town.group$town == 'Grimsby'] > 0 &
           town.group$n[town.group$town == 'Wigan'] > 0 &
           town.group$n[town.group$town == 'Hemel Hempstead'] > 0 &
           town.group$n[town.group$town == 'Basingstoke'] > 0 &
           town.group$n[town.group$town == 'Warwick'] > 0 &
           town.group$n[town.group$town == 'Yeovil'] > 0 &
           town.group$n[town.group$town == 'Newark'] > 0 &
           town.group$n[town.group$town == 'Carlisle'] > 0 &
           town.group$n[town.group$town == 'Salisbury'] > 0 &
           town.group$n[town.group$town == 'Southport'] > 0 &
           town.group$n[town.group$town == 'Rochdale'] > 0 &
           town.group$n[town.group$town == 'Rotherham'] > 0 &
           town.group$n[town.group$town == 'Scunthorpe'] > 0 &
           town.group$n[town.group$town == 'Wansbeck'] > 0){
            t$town <- relevel(t$town, ref = 'Grimsby')
            model5.panelar <- panelAR(data     = t,
                                      formula  = formula.model5,
                                      timeVar  = timevar,
                                      panelVar = panel.trust,
                                      autoCorr = autocorr,
                                      panelCorrMethod = 'pcse',
                                      complete.case = complete.case,
                                      seq.times = seq.times,
                                      rho.na.rm = rho.na.rm)
            results$model5.panelar.all.coef <- extract_coefficients(x              = model5.panelar,
                                                                    .site          = 'All',
                                                                    .indicator     = indicator,
                                                                    .sub.indicator = sub.indicator)
            results$model5.panelar.r2 <- model5.panelar$r2
        }
        ## Summary table
        if(!is.null(results$model3.1.panelar.bishop.coef) |
           !is.null(results$model3.1.panelar.hartlepool.coef) |
           !is.null(results$model3.1.panelar.hemel.coef) |
           !is.null(results$model3.1.panelar.newark.coef) |
           !is.null(results$model3.1.panelar.rochdale.coef) |
           !is.null(results$model5.panelar.all.coef)){
            results$model5.panelar.all <- combine_coefficients(bishop.coef     = results$model3.1.panelar.bishop.coef,
                                                               hartlepool.coef = results$model3.1.panelar.hartlepool.coef,
                                                               hemel.coef      = results$model3.1.panelar.hemel.coef,
                                                               newark.coef     = results$model3.1.panelar.newark.coef,
                                                               rochdale.coef   = results$model3.1.panelar.rochdale.coef,
                                                               all.coef        = results$model5.panelar.all.coef)
            ## Forest plot
            results$model5.forest <- closed_forest(df.list       = list(results$model3.1.panelar.bishop.coef,
                                                                        results$model3.1.panelar.hartlepool.coef,
                                                                        results$model3.1.panelar.hemel.coef,
                                                                        results$model3.1.panelar.newark.coef,
                                                                        results$model3.1.panelar.rochdale.coef,
                                                                        results$model5.panelar.all.coef),
                                                   plot.term     = c('closure'),
                                                   facet.outcome = FALSE,
                                                   title         = paste0('Model 3 & 5  : ',
                                                                          indicator,
                                                                          ' (',
                                                                          sub.indicator,
                                                                          ')'),
                                                   theme         = theme_bw())
        }
        ## Return model objects if requested
        if(return.model == TRUE){
            if(exists('model5.panelar')){
                results$model5.panelar     <- model5.panelar
            }
        }
        if(return.df == TRUE){
            results$model5.df <- df5
        }
        if(return.residuals == TRUE){
            if(exists('model5.panelar')){
                results$model5.panelar.residuals.all     <- summary(model5.panelar)$residuals
            }
        }
        ## Remove clutter
        rm(df5)
    }
    #######################################################################
    ## Model 6.1                                                         ##
    #######################################################################
    if(!is.null(model6.1)){
        ## print("Model 6.1")
        ## Reformulate outcome and covariates
        formula.model6.1 <- reformulate(response = outcome,
                                      termlabels = model6.1)
        ## Subset data
        sites <- c('Bishop Auckland',
                   'Hartlepool',
                   'Hemel Hempstead',
                   'Newark',
                   'Rochdale')
        df6.1 <- dplyr::filter(df.lsoa, town %in% sites &
                            measure     == indicator &
                            sub.measure == sub.indicator)
        df6.1$group <- paste0('Cohort : ', df6.1$group)
        ## Add in indicator of case/control status for plotting
        case <- c('Bishop Auckland',
                  'Hartlepool',
                  'Hemel Hempstead',
                  'Newark',
                  'Rochdale')
        df6.1$status <- ifelse(df6.1$town %in% case, 'Case', 'Control')
        ## Perform analysis with panelAR in each
        ##################################################
        ## Model 6.1 - Bishop Auckland                    ##
        ##################################################
        ## print("Bishop Auckland")
        t <- dplyr::filter(df6.1,
                    town == 'Bishop Auckland')
        ## Getting errors with complete.case == TRUE...
        ##
        ## Error: Unable to compute correlated SEs / PCSEs because there are no time
        ## periods in common across all units. Instead, consider setting
        ## complete.case =FALSE.
        ##
        ## ...so have opted for that for ALL LSOA analyses rather than conditionally
        ## switching.
        ## complete.case.6.1 <- complete.case
        ## if(indicator == 'case fatality ratio' & sub.indicator == 'any') complete.case.6.1 <- FALSE
        complete.case <- FALSE
        if(town.group$n[town.group$town == 'Bishop Auckland'] > 0){
            ## Remove instances where there are missing observations for LSOAs
            t <- dplyr::filter(t, !is.na(value))
            model6.1.panelar.bishop <- panelAR(data     = t,
                                             formula  = formula.model6.1,
                                             timeVar  = timevar,
                                             panelVar = panel.lsoa,
                                             autoCorr = autocorr,
                                             panelCorrMethod = 'pcse',
                                             complete.case = complete.case,
                                             seq.times = seq.times,
                                             rho.na.rm = rho.na.rm)
            results$model6.1.panelar.bishop.coef <- extract_coefficients(x              = model6.1.panelar.bishop,
                                                                       .site          = 'Bishop Auckland',
                                                                       .indicator     = indicator,
                                                                       .sub.indicator = sub.indicator)
            results$model6.1.panelar.bishop.r2 <- model6.1.panelar.bishop
        }
        ##################################################
        ## Model 6.1 - Hartlepool                       ##
        ##################################################
        ## print("Hartlepool")
        t <- dplyr::filter(df6.1,
                    town == 'Hartlepool')
        if(town.group$n[town.group$town == 'Hartlepool'] > 0){
            ## Remove instances where there are missing observations for LSOAs
            t <- dplyr::filter(t, !is.na(value))
            model6.1.panelar.hartlepool <- panelAR(data     = t,
                                                 formula  = formula.model6.1,
                                                 timeVar  = timevar,
                                                 panelVar = panel.lsoa,
                                                 autoCorr = autocorr,
                                                 panelCorrMethod = 'pcse',
                                                 complete.case = complete.case,
                                                 seq.times = seq.times,
                                                 rho.na.rm = rho.na.rm)
            results$model6.1.panelar.hartlepool.coef <- extract_coefficients(x             = model6.1.panelar.hartlepool,
                                                                           .site          = 'Hartlepool',
                                                                           .indicator     = indicator,
                                                                           .sub.indicator = sub.indicator)
            results$model6.1.panelar.hartlepool.r2 <- model6.1.panelar.hartlepool$r2
        }
        ##################################################
        ## Model 6.1 - Hemel Hempstead                  ##
        ##################################################
        ## print("Hemel Hempstead")
        t <- dplyr::filter(df6.1,
                    town == 'Hemel Hempstead')
        if(town.group$n[town.group$town == 'Hemel Hempstead'] > 0){
            ## Remove instances where there are missing observations for LSOAs
            t <- dplyr::filter(t, !is.na(value))
            model6.1.panelar.hemel <- panelAR(data     = t,
                                            formula  = formula.model6.1,
                                            timeVar  = timevar,
                                            panelVar = panel.lsoa,
                                            autoCorr = autocorr,
                                            panelCorrMethod = 'pcse',
                                            complete.case = complete.case,
                                            seq.times = seq.times,
                                            rho.na.rm = rho.na.rm)
            results$model6.1.panelar.hemel.coef <- extract_coefficients(x              = model6.1.panelar.hemel,
                                                                      .site          = 'Hemel Hempstead',
                                                                      .indicator     = indicator,
                                                                      .sub.indicator = sub.indicator)
            results$model6.1.panelar.hemel.r2 <- model6.1.panelar.hemel$r2
        }
        ##################################################
        ## Model 6.1 - Newark                           ##
        ##################################################
        ## print("Newark")
        t <- dplyr::filter(df6.1,
                    town == 'Newark')
        if(town.group$n[town.group$town == 'Newark'] > 0){
            ## Remove instances where there are missing observations for LSOAs
            t <- dplyr::filter(t, !is.na(value))
            model6.1.panelar.newark <- panelAR(data     = t,
                                             formula  = formula.model6.1,
                                             timeVar  = timevar,
                                             panelVar = panel.lsoa,
                                             autoCorr = autocorr,
                                             panelCorrMethod = 'pcse',
                                             complete.case = complete.case,
                                             seq.times = seq.times,
                                             rho.na.rm = rho.na.rm)
            results$model6.1.panelar.newark.coef <- extract_coefficients(x              = model6.1.panelar.newark,
                                                                       .site          = 'Newark',
                                                                       .indicator     = indicator,
                                                                       .sub.indicator = sub.indicator)
            results$model6.1.panelar.newark.r2 <- model6.1.panelar.newark$r2
        }
        ##################################################
        ## Model 6.1 - Rochdale                         ##
        ##################################################
        ## print("Rochdale")
        t <- dplyr::filter(df6.1,
                    town == 'Rochdale')
        if(town.group$n[town.group$town == 'Rochdale'] > 0){
            ## Remove instances where there are missing observations for LSOAs
            t <- dplyr::filter(t, !is.na(value))
            model6.1.panelar.rochdale <- panelAR(data     = t,
                                               formula  = formula.model6.1,
                                               timeVar  = timevar,
                                               panelVar = panel.lsoa,
                                               autoCorr = autocorr,
                                               panelCorrMethod = 'pcse',
                                               complete.case = complete.case,
                                               seq.times = seq.times,
                                               rho.na.rm = rho.na.rm)
            results$model6.1.panelar.rochdale.coef <- extract_coefficients(x            = model6.1.panelar.rochdale,
                                                                         .site          = 'Rochdale',
                                                                         .indicator     = indicator,
                                                                         .sub.indicator = sub.indicator)
            results$model6.1.panelar.rochdale.r2 <- model6.1.panelar.rochdale$r2
        }
        ## Summary table
        if(!is.null(results$model6.1.panelar.bishop.coef) |
           !is.null(results$model6.1.panelar.hartlepool.coef) |
           !is.null(results$model6.1.panelar.hemel.coef) |
           !is.null(results$model6.1.panelar.newark.coef) |
           !is.null(results$model6.1.panelar.rochdale.coef)){
            results$model6.1.panelar.all <- combine_coefficients(bishop.coef     = results$model6.1.panelar.bishop.coef,
                                                               hartlepool.coef = results$model6.1.panelar.hartlepool.coef,
                                                               hemel.coef      = results$model6.1.panelar.hemel.coef,
                                                               newark.coef     = results$model6.1.panelar.newark.coef,
                                                               rochdale.coef   = results$model6.1.panelar.rochdale.coef)
            ## Forest plot
            results$model6.1.forest.model6.1 <- closed_forest(df.list       = list(results$model6.1.panelar.bishop.coef,
                                                                               results$model6.1.panelar.hartlepool.coef,
                                                                               results$model6.1.panelar.hemel.coef,
                                                                               results$model6.1.panelar.newark.coef,
                                                                               results$model6.1.panelar.rochdale.coef),
                                                          plot.term     = c('closure'),
                                                          facet.outcome = FALSE,
                                                          title         = paste0('Model 6.1 : ',
                                                                                 indicator,
                                                                                 ' (',
                                                                                 sub.indicator,
                                                                                 ')'),
                                                          theme         = theme_bw())
        }
        ## Return model objects if requested
        if(return.model == TRUE){
            if(exists('model6.1.panelar.bishop')){
                results$model6.1.panelar.bishop     <- model6.1.panelar.bishop
            }
            if(exists('model6.1.panelar.hartlepool')){
                results$model6.1.panelar.hartlepool <- model6.1.panelar.hartlepool
            }
            if(exists('model6.1.panelar.hemel')){
                results$model6.1.panelar.hemel      <- model6.1.panelar.hemel
            }
            if(exists('model6.1.panelar.newark')){
                results$model6.1.panelar.newark     <- model6.1.panelar.newark
            }
            if(exists('model6.1.panelar.rochdale')){
                results$model6.1.panelar.rochdale   <- model6.1.panelar.rochdale
            }
        }
        if(return.df == TRUE){
            results$model6.1.df <- df6.1
        }
        if(return.residuals == TRUE){
            if(exists('model6.1.panelar.bishop')){
                results$model6.1.panelar.residuals.bishop     <- summary(model6.1.panelar.bishop)$residuals
            }
            if(exists('model6.1.panelar.hartlepool')){
                results$model6.1.panelar.residuals.hartlepool <- summary(model6.1.panelar.hartlepool)$residuals
            }
            if(exists('model6.1.panelar.hemel')){
                results$model6.1.panelar.residuals.hemel      <- summary(model6.1.panelar.hemel)$residuals
            }
            if(exists('model6.1.panelar.newark')){
                results$model6.1.panelar.residuals.newark     <- summary(model6.1.panelar.newark)$residuals
            }
            if(exists('model6.1.panelar.rochdale')){
                results$model6.1.panelar.residuals.rochdale   <- summary(model6.1.panelar.rochdale)$residuals
            }
        }
        ## Remove clutter
        rm(df6.1)
    }
    #######################################################################
    ## Model 6.2                                                         ##
    #######################################################################
    if(!is.null(model6.2)){
        ## print("Model 6.2")
        ## Reformulate outcome and covariates
        formula.model6.2 <- reformulate(response = outcome,
                                      termlabels = model6.2)
        ## Subset data
        sites <- c('Bishop Auckland', 'Whitehaven',
                   'Hartlepool', 'Grimsby',
                   'Hemel Hempstead', 'Warwick',
                   'Newark', 'Southport',
                   'Rochdale', 'Rotherham')
        df6.2 <- dplyr::filter(df.lsoa, town %in% sites &
                            measure     == indicator &
                            sub.measure == sub.indicator)
        df6.2$group <- paste0('Cohort : ', df6.2$group)
        ## Add in indicator of case/control status for plotting
        case <- c('Bishop Auckland',
                  'Hartlepool',
                  'Hemel Hempstead',
                  'Newark',
                  'Rochdale')
        df6.2$status <- ifelse(df6.2$town %in% case, 'Case', 'Control')
        ## Perform analysis with panelAR in each
        ##################################################
        ## Model 6.2 - Bishop Auckland                  ##
        ##################################################
        ## print("Bishop Auckland")
        t <- dplyr::filter(df6.2,
                    town == 'Bishop Auckland' |
                    town == 'Whitehaven')
        ## Getting errors with complete.case == TRUE...
        ##
        ## Error: Unable to compute correlated SEs / PCSEs because there are no time
        ## periods in common across all units. Instead, consider setting
        ## complete.case =FALSE.
        ##
        ## ...so have opted for that for ALL LSOA analyses rather than conditionally
        ## switching.
        ## complete.case.6.2 <- complete.case
        ## if(indicator == 'case fatality ratio' & sub.indicator == 'any') complete.case.6.2 <- FALSE
        complete.case <- FALSE
        if(town.group$n[town.group$town == 'Bishop Auckland'] > 0 & town.group$n[town.group$town == 'Whitehaven'] > 0 ){
            t$town <- relevel(t$town, ref = 'Whitehaven')
            ## Remove instances where there are missing observations for LSOAs
            t <- dplyr::filter(t, !is.na(value))
            model6.2.panelar.bishop <- panelAR(data     = t,
                                             formula  = formula.model6.2,
                                             timeVar  = timevar,
                                             panelVar = panel.lsoa,
                                             autoCorr = autocorr,
                                             panelCorrMethod = 'pcse',
                                             complete.case = complete.case,
                                             seq.times = seq.times,
                                             rho.na.rm = rho.na.rm)
            results$model6.2.panelar.bishop.coef <- extract_coefficients(x              = model6.2.panelar.bishop,
                                                                       .site          = 'Bishop Auckland',
                                                                       .indicator     = indicator,
                                                                       .sub.indicator = sub.indicator)
            results$model6.2.panelar.bishop.r2 <- model6.2.panelar.bishop
        }
        ##################################################
        ## Model 6.2 - Hartlepool                       ##
        ##################################################
        ## print("Hartlepool")
        t <- dplyr::filter(df6.2,
                    town == 'Hartlepool' |
                    town == 'Grimsby')
        if(town.group$n[town.group$town == 'Hartlepool'] > 0 & town.group$n[town.group$town == 'Grimsby'] > 0){
            t$town <- relevel(t$town, ref = 'Grimsby')
            ## Remove instances where there are missing observations for LSOAs
            t <- dplyr::filter(t, !is.na(value))
            model6.2.panelar.hartlepool <- panelAR(data     = t,
                                                 formula  = formula.model6.2,
                                                 timeVar  = timevar,
                                                 panelVar = panel.lsoa,
                                                 autoCorr = autocorr,
                                                 panelCorrMethod = 'pcse',
                                                 complete.case = complete.case,
                                                 seq.times = seq.times,
                                                 rho.na.rm = rho.na.rm)
            results$model6.2.panelar.hartlepool.coef <- extract_coefficients(x             = model6.2.panelar.hartlepool,
                                                                           .site          = 'Hartlepool',
                                                                           .indicator     = indicator,
                                                                           .sub.indicator = sub.indicator)
            results$model6.2.panelar.hartlepool.r2 <- model6.2.panelar.hartlepool$r2
        }
        ##################################################
        ## Model 6.2 - Hemel Hempstead                  ##
        ##################################################
        ## print("Hemel Hempstead")
        t <- dplyr::filter(df6.2,
                    town == 'Hemel Hempstead' |
                    town == 'Warwick')
        if(town.group$n[town.group$town == 'Hemel Hempstead'] > 0 & town.group$n[town.group$town == 'Warwick'] > 0){
            t$town <- relevel(t$town, ref = 'Warwick')
            ## Remove instances where there are missing observations for LSOAs
            t <- dplyr::filter(t, !is.na(value))
            model6.2.panelar.hemel <- panelAR(data     = t,
                                            formula  = formula.model6.2,
                                            timeVar  = timevar,
                                            panelVar = panel.lsoa,
                                            autoCorr = autocorr,
                                            panelCorrMethod = 'pcse',
                                            complete.case = complete.case,
                                            seq.times = seq.times,
                                            rho.na.rm = rho.na.rm)
            results$model6.2.panelar.hemel.coef <- extract_coefficients(x              = model6.2.panelar.hemel,
                                                                      .site          = 'Hemel Hempstead',
                                                                      .indicator     = indicator,
                                                                      .sub.indicator = sub.indicator)
            results$model6.2.panelar.hemel.r2 <- model6.2.panelar.hemel$r2
        }
        ##################################################
        ## Model 6.2 - Newark                           ##
        ##################################################
        ## print("Newark")
        t <- dplyr::filter(df6.2,
                    town == 'Newark' |
                    town == 'Southport')
        if(town.group$n[town.group$town == 'Newark'] > 0 & town.group$n[town.group$town == 'Southport'] > 0){
            t$town <- relevel(t$town, ref = 'Southport')
            ## Remove instances where there are missing observations for LSOAs
            t <- dplyr::filter(t, !is.na(value))
            model6.2.panelar.newark <- panelAR(data     = t,
                                             formula  = formula.model6.2,
                                             timeVar  = timevar,
                                             panelVar = panel.lsoa,
                                             autoCorr = autocorr,
                                             panelCorrMethod = 'pcse',
                                             complete.case = complete.case,
                                             seq.times = seq.times,
                                             rho.na.rm = rho.na.rm)
            results$model6.2.panelar.newark.coef <- extract_coefficients(x              = model6.2.panelar.newark,
                                                                       .site          = 'Newark',
                                                                       .indicator     = indicator,
                                                                       .sub.indicator = sub.indicator)
            results$model6.2.panelar.newark.r2 <- model6.2.panelar.newark$r2
        }
        ##################################################
        ## Model 6.2 - Rochdale                         ##
        ##################################################
        ## print("Rochdale")
        t <- dplyr::filter(df6.2,
                    town == 'Rochdale' |
                    town == 'Rotherham')
        if(town.group$n[town.group$town == 'Rochdale'] > 0 & town.group$n[town.group$town == 'Rotherham'] > 0){
            t$town <- relevel(t$town, ref = 'Rotherham')
            ## Remove instances where there are missing observations for LSOAs
            t <- dplyr::filter(t, !is.na(value))
            model6.2.panelar.rochdale <- panelAR(data     = t,
                                               formula  = formula.model6.2,
                                               timeVar  = timevar,
                                               panelVar = panel.lsoa,
                                               autoCorr = autocorr,
                                               panelCorrMethod = 'pcse',
                                               complete.case = complete.case,
                                               seq.times = seq.times,
                                               rho.na.rm = rho.na.rm)
            results$model6.2.panelar.rochdale.coef <- extract_coefficients(x            = model6.2.panelar.rochdale,
                                                                         .site          = 'Rochdale',
                                                                         .indicator     = indicator,
                                                                         .sub.indicator = sub.indicator)
            results$model6.2.panelar.rochdale.r2 <- model6.2.panelar.rochdale$r2
        }
        ## Summary table
        if(!is.null(results$model6.2.panelar.bishop.coef) |
           !is.null(results$model6.2.panelar.hartlepool.coef) |
           !is.null(results$model6.2.panelar.hemel.coef) |
           !is.null(results$model6.2.panelar.newark.coef) |
           !is.null(results$model6.2.panelar.rochdale.coef)){
            results$model6.2.panelar.all <- combine_coefficients(bishop.coef     = results$model6.2.panelar.bishop.coef,
                                                               hartlepool.coef = results$model6.2.panelar.hartlepool.coef,
                                                               hemel.coef      = results$model6.2.panelar.hemel.coef,
                                                               newark.coef     = results$model6.2.panelar.newark.coef,
                                                               rochdale.coef   = results$model6.2.panelar.rochdale.coef)
            ## Forest plot
            results$model6.2.forest.model6.2 <- closed_forest(df.list       = list(results$model6.2.panelar.bishop.coef,
                                                                               results$model6.2.panelar.hartlepool.coef,
                                                                               results$model6.2.panelar.hemel.coef,
                                                                               results$model6.2.panelar.newark.coef,
                                                                               results$model6.2.panelar.rochdale.coef),
                                                          plot.term     = c('closure'),
                                                          facet.outcome = FALSE,
                                                          title         = paste0('Model 6.2 : ',
                                                                                 indicator,
                                                                                 ' (',
                                                                                 sub.indicator,
                                                                                 ')'),
                                                          theme         = theme_bw())
        }
        ## Return model objects if requested
        if(return.model == TRUE){
            if(exists('model6.2.panelar.bishop')){
                results$model6.2.panelar.bishop     <- model6.2.panelar.bishop
            }
            if(exists('model6.2.panelar.hartlepool')){
                results$model6.2.panelar.hartlepool <- model6.2.panelar.hartlepool
            }
            if(exists('model6.2.panelar.hemel')){
                results$model6.2.panelar.hemel      <- model6.2.panelar.hemel
            }
            if(exists('model6.2.panelar.newark')){
                results$model6.2.panelar.newark     <- model6.2.panelar.newark
            }
            if(exists('model6.2.panelar.rochdale')){
                results$model6.2.panelar.rochdale   <- model6.2.panelar.rochdale
            }
        }
        if(return.df == TRUE){
            results$model6.2.df <- df6.2
        }
        if(return.residuals == TRUE){
            if(exists('model6.2.panelar.bishop')){
                results$model6.2.panelar.residuals.bishop     <- summary(model6.2.panelar.bishop)$residuals
            }
            if(exists('model6.2.panelar.hartlepool')){
                results$model6.2.panelar.residuals.hartlepool <- summary(model6.2.panelar.hartlepool)$residuals
            }
            if(exists('model6.2.panelar.hemel')){
                results$model6.2.panelar.residuals.hemel      <- summary(model6.2.panelar.hemel)$residuals
            }
            if(exists('model6.2.panelar.newark')){
                results$model6.2.panelar.residuals.newark     <- summary(model6.2.panelar.newark)$residuals
            }
            if(exists('model6.2.panelar.rochdale')){
                results$model6.2.panelar.residuals.rochdale   <- summary(model6.2.panelar.rochdale)$residuals
            }
        }
        ## Remove clutter
        rm(df6.2)
    }
    #######################################################################
    ## Model 7.1                                                         ##
    #######################################################################
    if(!is.null(model7.1)){
        ## print("Model 7.1")
        ## Reformulate outcome and covariates
        formula.model7.1 <- reformulate(response = outcome,
                                      termlabels = model7.1)
        ## Subset data
        sites <- c('Bishop Auckland',
                   'Hartlepool',
                   'Hemel Hempstead',
                   'Newark',
                   'Rochdale')
        df7.1 <- dplyr::filter(df.lsoa, town %in% sites &
                      measure     == indicator &
                      sub.measure == sub.indicator)
        ## Two LSOAs overlap two EDs so derive a new unique indicator
        ## and use that for the panels
        df7.1$town.lsoa <- paste0(df7.1$town, df7.1$lsoa)
        ## ## Add in indicator of case/control status for plotting
        ## case <- c('Bishop Auckland',
        ##           'Hartlepool',
        ##           'Hemel Hempstead',
        ##           'Newark',
        ##           'Rochdale')
        ## df7.1$status <- ifelse(df7.1$town %in% case, 'Case', 'Control')
        results$df7.1 <- df7.1
        ## Perform analysis with panelAR in each
        ##################################################
        ## All sites                                    ##
        ##################################################
        formula.model7.1 <- reformulate(response = outcome,
                                       termlabels = c(model7.1))
        t <- dplyr::filter(df7.1,
                    measure     == indicator &
                    sub.measure == sub.indicator)
        if(town.group$n[town.group$town == 'Bishop Auckland'] > 0 &
           town.group$n[town.group$town == 'Hartlepool'] > 0 &
           town.group$n[town.group$town == 'Hemel Hempstead'] > 0 &
           town.group$n[town.group$town == 'Newark'] > 0 &
           town.group$n[town.group$town == 'Rochdale'] > 0){
            ## Getting errors with complete.case == TRUE...
            ##
            ## Error: Unable to compute correlated SEs / PCSEs because there are no time
            ## periods in common across all units. Instead, consider setting
            ## complete.case =FALSE.
            ##
            ## ...so have opted for that for ALL LSOA analyses rather than conditionally
            ## switching.
            ## if(indicator == 'length of stay')      complete.case <- FALSE
            ## if(indicator == 'case fatality ratio') complete.case <- FALSE
            complete.case <- FALSE
            model7.1.panelar <- panelAR(data     = t,
                                      formula  = formula.model7.1,
                                      timeVar  = timevar,
                                      panelVar = 'town.lsoa',
                                      autoCorr = autocorr,
                                      panelCorrMethod = 'pcse',
                                      complete.case = complete.case,
                                      seq.times = seq.times,
                                      rho.na.rm = rho.na.rm)
            results$model7.1.panelar.all.coef <- extract_coefficients(x              = model7.1.panelar,
                                                                .site          = 'All',
                                                                .indicator     = indicator,
                                                                .sub.indicator = sub.indicator)
            results$model7.1.panelar.r2 <- model7.1.panelar$r2
        }
        ## Summary table
        if(!is.null(results$model6.1.panelar.bishop.coef) |
           !is.null(results$model6.1.panelar.hartlepool.coef) |
           !is.null(results$model6.1.panelar.hemel.coef) |
           !is.null(results$model6.1.panelar.newark.coef) |
           !is.null(results$model6.1.panelar.rochdale.coef) |
           !is.null(results$model7.1.panelar.all.coef)){
            results$model7.1.panelar.all <- combine_coefficients(bishop.coef     = results$model6.1.panelar.bishop.coef,
                                                               hartlepool.coef = results$model6.1.panelar.hartlepool.coef,
                                                               hemel.coef      = results$model6.1.panelar.hemel.coef,
                                                               newark.coef     = results$model6.1.panelar.newark.coef,
                                                               rochdale.coef   = results$model6.1.panelar.rochdale.coef,
                                                               all.coef        = results$model7.1.panelar.all.coef)
            ## ## Forest plot
            results$model7.1.forest <- closed_forest(df.list       = list(results$model6.1.panelar.bishop.coef,
                                                                        results$model6.1.panelar.hartlepool.coef,
                                                                        results$model6.1.panelar.hemel.coef,
                                                                        results$model6.1.panelar.newark.coef,
                                                                        results$model6.1.panelar.rochdale.coef,
                                                                        results$model7.1.panelar.all.coef),
                                                   plot.term     = c('closure'),
                                                   facet.outcome = FALSE,
                                                   title         = paste0('Model 6 & Model 7.1 : ',
                                                                          indicator,
                                                                          ' (',
                                                                          sub.indicator,
                                                                          ')'),
                                                   theme         = theme_bw())
        }
        ## Return model objects if requested
        if(return.model == TRUE){
            if(exists('model7.1.panelar')){
                results$model7.1.panelar     <- model7.1.panelar
            }
        }
        if(return.df == TRUE){
            results$model7.1.df <- df7.1
        }
        if(return.residuals == TRUE){
            if(exists('model7.1.panelar')){
                results$model7.1.panelar.residuals.all     <- summary(model7.1.panelar)$residuals
            }
        }
        ## Remove clutter
        rm(df7.1)
    }
    #######################################################################
    ## Model 7.2                                                         ##
    #######################################################################
    if(!is.null(model7.2)){
        ## print("Model 7.2")
        ## Reformulate outcome and covariates
        formula.model7.2 <- reformulate(response = outcome,
                                      termlabels = model7.2)
        ## Subset data
        sites <- c('Bishop Auckland', 'Whitehaven',
                   'Hartlepool', 'Grimsby',
                   'Hemel Hempstead', 'Warwick',
                   'Newark', 'Southport',
                   'Rochdale', 'Rotherham')
        df7.2 <- dplyr::filter(df.lsoa, town %in% sites &
                      measure     == indicator &
                      sub.measure == sub.indicator)
        ## Two LSOAs overlap two EDs so derive a new unique indicator
        ## and use that for the panels
        df7.2$town.lsoa <- paste0(df7.2$town, df7.2$lsoa)
        ## Add in indicator of case/control status for plotting
        case <- c('Bishop Auckland',
                  'Hartlepool',
                  'Hemel Hempstead',
                  'Newark',
                  'Rochdale')
        df7.2$status <- ifelse(df7.2$town %in% case, 'Case', 'Control')
        results$df7.2 <- df7.2
        ## Perform analysis with panelAR in each
        ##################################################
        ## All sites                                    ##
        ##################################################
        formula.model7.2 <- reformulate(response = outcome,
                                       termlabels = c(model7.2))
        t <- dplyr::filter(df7.2,
                    measure     == indicator &
                    sub.measure == sub.indicator)
        if(town.group$n[town.group$town == 'Bishop Auckland'] > 0 &
           town.group$n[town.group$town == 'Whitehaven'] > 0 &
           town.group$n[town.group$town == 'Hartlepool'] > 0 &
           town.group$n[town.group$town == 'Grimsby'] > 0 &
           town.group$n[town.group$town == 'Hemel Hempstead'] > 0 &
           town.group$n[town.group$town == 'Warwick'] > 0 &
           town.group$n[town.group$town == 'Newark'] > 0 &
           town.group$n[town.group$town == 'Southport'] > 0 &
           town.group$n[town.group$town == 'Rochdale'] > 0 &
           town.group$n[town.group$town == 'Rotherham'] > 0){
            t$town <- relevel(t$town, ref = 'Grimsby')
            ## Getting errors with complete.case == TRUE...
            ##
            ## Error: Unable to compute correlated SEs / PCSEs because there are no time
            ## periods in common across all units. Instead, consider setting
            ## complete.case =FALSE.
            ##
            ## ...so have opted for that for ALL LSOA analyses rather than conditionally
            ## switching.
            ## if(indicator == 'length of stay')      complete.case <- FALSE
            ## if(indicator == 'case fatality ratio') complete.case <- FALSE
            complete.case <- FALSE
            model7.2.panelar <- panelAR(data     = t,
                                      formula  = formula.model7.2,
                                      timeVar  = timevar,
                                      panelVar = 'town.lsoa',
                                      autoCorr = autocorr,
                                      panelCorrMethod = 'pcse',
                                      complete.case = complete.case,
                                      seq.times = seq.times,
                                      rho.na.rm = rho.na.rm)
            results$model7.2.panelar.all.coef <- extract_coefficients(x              = model7.2.panelar,
                                                                .site          = 'All',
                                                                .indicator     = indicator,
                                                                .sub.indicator = sub.indicator)
            results$model7.2.panelar.r2 <- model7.2.panelar$r2
        }
        ## Summary table
        if(!is.null(results$model6.1.panelar.bishop.coef) |
           !is.null(results$model6.1.panelar.hartlepool.coef) |
           !is.null(results$model6.1.panelar.hemel.coef) |
           !is.null(results$model6.1.panelar.newark.coef) |
           !is.null(results$model6.1.panelar.rochdale.coef) |
           !is.null(results$model7.2.panelar.all.coef)){
            results$model7.2.panelar.all <- combine_coefficients(bishop.coef     = results$model6.1.panelar.bishop.coef,
                                                               hartlepool.coef = results$model6.1.panelar.hartlepool.coef,
                                                               hemel.coef      = results$model6.1.panelar.hemel.coef,
                                                               newark.coef     = results$model6.1.panelar.newark.coef,
                                                               rochdale.coef   = results$model6.1.panelar.rochdale.coef,
                                                               all.coef        = results$model7.2.panelar.all.coef)
            ## ## Forest plot
            results$model7.2.forest <- closed_forest(df.list       = list(results$model6.1.panelar.bishop.coef,
                                                                        results$model6.1.panelar.hartlepool.coef,
                                                                        results$model6.1.panelar.hemel.coef,
                                                                        results$model6.1.panelar.newark.coef,
                                                                        results$model6.1.panelar.rochdale.coef,
                                                                        results$model7.2.panelar.all.coef),
                                                   plot.term     = c('closure'),
                                                   facet.outcome = FALSE,
                                                   title         = paste0('Model 6 & Model 7.2 : ',
                                                                          indicator,
                                                                          ' (',
                                                                          sub.indicator,
                                                                          ')'),
                                                   theme         = theme_bw())
        }
        ## Return model objects if requested
        if(return.model == TRUE){
            if(exists('model7.2.panelar')){
                results$model7.2.panelar     <- model7.2.panelar
            }
        }
        if(return.df == TRUE){
            results$model7.2.df <- df7.2
        }
        if(return.residuals == TRUE){
            if(exists('model7.2.panelar')){
                results$model7.2.panelar.residuals.all     <- summary(model7.2.panelar)$residuals
            }
        }
        ## Remove clutter
        rm(df7.2)
    }
    #######################################################################
    ## Produce summary tables by center using results$summary.table.head ##
    ## and the coefficients from each model                              ##
    #######################################################################
    ## Bind all model results together
    if(!is.null(model0)){
        model0.coef <- rbind(results$model0.panelar.bishop.coef,
                             results$model0.panelar.hartlepool.coef,
                             results$model0.panelar.hemel.coef,
                             results$model0.panelar.newark.coef,
                             results$model0.panelar.rochdale.coef)
        model0.coef$model <- 'Model 0'
    }
    else{
        model0.coef <- 1
    }
    if(!is.null(model0.5)){
        model0.5.coef <- rbind(results$model0.5.panelar.bishop.coef,
                               results$model0.5.panelar.hartlepool.coef,
                               results$model0.5.panelar.hemel.coef,
                               results$model0.5.panelar.newark.coef,
                               results$model0.5.panelar.rochdale.coef)
        model0.5.coef$model <- 'Model 0.5'
    }
    else{
        model0.5.coef <- 1
    }
    if(!is.null(model1)){
        model1.coef <- rbind(results$model1.panelar.bishop.coef,
                             results$model1.panelar.hartlepool.coef,
                             results$model1.panelar.hemel.coef,
                             results$model1.panelar.newark.coef,
                             results$model1.panelar.rochdale.coef)
        model1.coef$model <- 'Model 1'
    }
    else{
        model1.coef <- 1
    }
    if(!is.null(model2)){
        model2.coef <- rbind(results$model2.panelar.bishop.coef,
                             results$model2.panelar.hartlepool.coef,
                             results$model2.panelar.hemel.coef,
                             results$model2.panelar.newark.coef,
                             results$model2.panelar.rochdale.coef)
        model2.coef$model <- 'Model 2'
    }
    else{
        model2.coef <- 1
    }
    if(!is.null(model3.1)){
        model3.1.coef <- rbind(results$model3.1.panelar.bishop.coef,
                               results$model3.1.panelar.hartlepool.coef,
                               results$model3.1.panelar.hemel.coef,
                               results$model3.1.panelar.newark.coef,
                               results$model3.1.panelar.rochdale.coef)
        model3.1.coef$model <- 'Model 3.1'
    }
    else{
        model3.1.coef <- 1
    }
    if(!is.null(model3.2)){
        model3.2.coef <- rbind(results$model3.2.panelar.bishop.coef,
                               results$model3.2.panelar.hartlepool.coef,
                               results$model3.2.panelar.hemel.coef,
                               results$model3.2.panelar.newark.coef,
                               results$model3.2.panelar.rochdale.coef)
        model3.2.coef$model <- 'Model 3.2'
    }
    else{
        model3.2.coef <- 1
    }
    if(!is.null(model4)){
        model4.coef <- results$model4.panelar.all.coef
        model4.coef$model <- 'Model 4'
    }
    else{
        model4.coef <- 1
    }
    if(!is.null(model5)){
        model5.coef <- results$model5.panelar.all.coef
        model5.coef$model <- 'Model 5'
    }
    else{
        model5.coef <- 1
    }
    if(!is.null(model6.1)){
        model6.1.coef <- rbind(results$model6.1.panelar.bishop.coef,
                               results$model6.1.panelar.hartlepool.coef,
                               results$model6.1.panelar.hemel.coef,
                               results$model6.1.panelar.newark.coef,
                               results$model6.1.panelar.rochdale.coef)
        model6.1.coef$model <- 'Model 6.1'
    }
    else{
        model6.1.coef <- 1
    }
    if(!is.null(model6.2)){
        model6.2.coef <- rbind(results$model6.2.panelar.bishop.coef,
                               results$model6.2.panelar.hartlepool.coef,
                               results$model6.2.panelar.hemel.coef,
                               results$model6.2.panelar.newark.coef,
                               results$model6.2.panelar.rochdale.coef)
        model6.2.coef$model <- 'Model 6.2'
    }
    else{
        model6.2.coef <- 1
    }
    if(!is.null(model7.1)){
        model7.1.coef <- results$model7.1.panelar.all.coef
        model7.1.coef$model <- 'Model 7.1'
    }
    else{
        model7.1.coef <- 1
    }
    if(!is.null(model7.2)){
        model7.2.coef <- results$model7.2.panelar.all.coef
        model7.2.coef$model <- 'Model 7.2'
    }
    else{
        model7.2.coef <- 1
    }
    ## Some model*.coef may not have any data though as the models weren't run
    ## make those NULL so the subsequent rbind() doesn't fail
    if(length(model0.coef) == 1) model0.coef     <- NULL
    if(length(model0.5.coef) == 1) model0.5.coef <- NULL
    if(length(model1.coef) == 1) model1.coef     <- NULL
    if(length(model2.coef) == 1) model2.coef     <- NULL
    if(length(model3.1.coef) == 1) model3.1.coef <- NULL
    if(length(model3.2.coef) == 1) model3.2.coef <- NULL
    if(length(model4.coef) == 1) model4.coef     <- NULL
    if(length(model5.coef) == 1) model5.coef     <- NULL
    if(length(model6.1.coef) == 1) model6.1.coef <- NULL
    if(length(model6.2.coef) == 1) model6.2.coef <- NULL
    if(length(model7.1.coef) == 1) model7.1.coef <- NULL
    if(length(model7.2.coef) == 1) model7.2.coef <- NULL
    ## Return all coefficients across models
    if(!is.null(model0.coef)){
        results$all.model.all.coef <- model0.coef
    }
    if(!is.null(model0.5.coef)){
    results$all.model.all.coef <- rbind(results$all.model.all.coef,
                                        model0.5.coef)
    }
    if(!is.null(model1.coef)){
        results$all.model.all.coef <- rbind(results$all.model.all.coef,
                                            model1.coef)
    }
    if(!is.null(model2.coef)){
        results$all.model.all.coef <- rbind(results$all.model.all.coef,
                                            model2.coef)
    }
    if(!is.null(model3.1.coef)){
        results$all.model.all.coef <- rbind(results$all.model.all.coef,
                                            model3.1.coef)
    }
    if(!is.null(model3.2.coef)){
        results$all.model.all.coef <- rbind(results$all.model.all.coef,
                                            model3.2.coef)
    }
    if(!is.null(model4.coef)){
        results$all.model.all.coef <- rbind(results$all.model.all.coef,
                                            model4.coef)
    }
    if(!is.null(model5.coef)){
        results$all.model.all.coef <- rbind(results$all.model.all.coef,
                                            model5.coef)
    }
    if(!is.null(model6.1.coef)){
        results$all.model.all.coef <- rbind(results$all.model.all.coef,
                                            model6.1.coef)
    }
    if(!is.null(model6.2.coef)){
        results$all.model.all.coef <- rbind(results$all.model.all.coef,
                                            model6.2.coef)
    }
    if(!is.null(model7.1.coef)){
        results$all.model.all.coef <- rbind(results$all.model.all.coef,
                                            model7.1.coef)
    }
    if(!is.null(model7.2.coef)){
        results$all.model.all.coef <- rbind(results$all.model.all.coef,
                                            model7.2.coef)
    }
    ## results$all.model.all.coef <- rbind(model0.coef,
    ##                                     model0.5.coef
    ##                                     model1.coef,
    ##                                     model2.coef,
    ##                                     model3.1.coef,
    ##                                     model3.2.coef,
    ##                                     model4.coef,
    ##                                     model5.coef,
    ##                                     model6.1.coef,
    ##                                     model6.2.coef,
    ##                                     model7.1.coef,
    ##                                     model7.2.coef) %>%
    ##                               as.data.frame()
    results$all.model.all.coef <- as.data.frame(results$all.model.all.coef)
    names(results$all.model.all.coef) <- gsub('site', 'town', names(results$all.model.all.coef))
    names(results$all.model.all.coef) <- gsub('Estimate', 'est', names(results$all.model.all.coef))
    names(results$all.model.all.coef) <- gsub('Std. Error', 'stderr', names(results$all.model.all.coef))
    names(results$all.model.all.coef) <- gsub('p(>|t|)', 'p', names(results$all.model.all.coef))
    names(results$all.model.all.coef) <- gsub('Pr(>|t|)', 'p', names(results$all.model.all.coef))
    names(results$all.model.all.coef) <- gsub('t value', 't', names(results$all.model.all.coef))
    names(results$all.model.all.coef) <- gsub('indicator', 'measure', names(results$all.model.all.coef))
    names(results$all.model.all.coef) <- c('est', 'stderr', 't', 'p', 'term', 'town', 'measure', 'sub.measure', 'r2', 'model')
    ## Calculate 95% CI's
    results$all.model.all.coef$min95 <- results$all.model.all.coef$est - (1.96 * results$all.model.all.coef$stderr)
    results$all.model.all.coef$max95 <- results$all.model.all.coef$est + (1.96 * results$all.model.all.coef$stderr)
    ## Standardise this output for combining with -xtnbreg- results
    results$all.model.all.coef <- dplyr::select(results$all.model.all.coef,
                                                measure, sub.measure, town, model, term, est, stderr, p, min95, max95)
    ## Subset out the closure coefficients and derive output variable/df to append to
    ## table header which contains the means
    results$all.model.closure.coef <- dplyr::filter(results$all.model.all.coef,
                                             term == 'closure' | term == 'diff.time.to.ed')
    results$all.model.closure.coef$estimate <- paste0(formatC(results$all.model.closure.coef$est, digits = digits, format = 'f'),
                                                      ' (',
                                                      formatC(results$all.model.closure.coef$min95, digits = digits, format = 'f'),
                                                      '-',
                                                      formatC(results$all.model.closure.coef$max95, digits = digits, format = 'f'),
                                                      ') p = ',
                                                      formatC(results$all.model.closure.coef$p, digits = digits, format = 'f'))
    results$summary.table.tail <- dplyr::select(results$all.model.closure.coef,
                                                town,
                                                model,
                                                estimate)
    results$summary.table.tail$Before_mean.sd    <- NA
    results$summary.table.tail$Before_median.iqr <- results$summary.table.tail$model
    results$summary.table.tail$Before_min.max    <- NA
    results$summary.table.tail$After_mean.sd     <- NA
    results$summary.table.tail$After_median.iqr  <- NA
    results$summary.table.tail$After_min.max     <- results$summary.table.tail$estimate
    results$summary.table.tail$diff_abs          <- NA
    results$summary.table.tail$diff_perc         <- NA
    ## Order the table
    results$summary.table.tail$order1 <- 0
    results$summary.table.tail$order2 <- 0
    results$summary.table.tail <- mutate(results$summary.table.tail,
                                         order1 = ifelse(model == 'Model 0.5', 1, order1),
                                         order1 = ifelse(model == 'Model 1',   2, order1),
                                         order1 = ifelse(model == 'Model 2',   3, order1),
                                         order1 = ifelse(model == 'Model 3',   4, order1),
                                         order1 = ifelse(model == 'Model 4',   5, order1),
                                         order1 = ifelse(model == 'Model 5',   6, order1),
                                         order1 = ifelse(model == 'Model 6.1', 7, order1),
                                         order1 = ifelse(model == 'Model 6.2', 8, order1),
                                         order1 = ifelse(model == 'Model 7.1', 9, order1),
                                         order1 = ifelse(model == 'Model 7.2', 10, order1),
                                         order2 = ifelse(town == 'Bishop Auckland', 1, order2),
                                         order2 = ifelse(town == 'Hartlepool',      2, order2),
                                         order2 = ifelse(town == 'Hemel Hempstead', 3, order2),
                                         order2 = ifelse(town == 'Newark',          4, order2),
                                         order2 = ifelse(town == 'Rochdale',        5, order2)) %>%
                                   arrange(order1, order2)
    results$summary.table.tail <- dplyr::select(results$summary.table.tail,
                                                town,
                                                Before_mean.sd, Before_median.iqr, Before_min.max,
                                                After_mean.sd, After_median.iqr, After_min.max,
                                                diff_abs, diff_perc)
    results$summary.table.tail$group <- results$summary.table.tail$town
    results$summary.table <- rbind(results$summary.table.head,
                                   results$summary.table.tail)
    ## Sort out indicators
    results$summary.table$town[grep('Model', results$summary.table$Before_median.iqr)]             <- NA
    results$summary.table$Before_mean.sd[results$summary.table$Before_median.iqr == 'Model 0']               <- 'Estimated closure coefficients'
    results$summary.table$Before_min.max[results$summary.table$Before_median.iqr == 'Model 0']     <- 'Individual Case Site'
    results$summary.table$After_mean.sd[results$summary.table$Before_median.iqr == 'Model 0']      <- 'No Control'
    results$summary.table$After_median.iqr[results$summary.table$Before_median.iqr == 'Model 0']   <- 'ED Panel'
    results$summary.table$Before_mean.sd[results$summary.table$Before_median.iqr == 'Model 0.5']               <- 'Estimated closure coefficients'
    results$summary.table$Before_min.max[results$summary.table$Before_median.iqr == 'Model 0.5']     <- 'Individual Case Site'
    results$summary.table$After_mean.sd[results$summary.table$Before_median.iqr == 'Model 0.5']      <- 'No Control'
    results$summary.table$After_median.iqr[results$summary.table$Before_median.iqr == 'Model 0.5']   <- 'ED Panel'
    results$summary.table$Before_mean.sd[results$summary.table$Before_median.iqr == 'Model 1']               <- 'Estimated closure coefficients'
    results$summary.table$Before_min.max[results$summary.table$Before_median.iqr == 'Model 1']     <- 'Individual Case Site'
    results$summary.table$After_mean.sd[results$summary.table$Before_median.iqr == 'Model 1']      <- 'No Control'
    results$summary.table$After_median.iqr[results$summary.table$Before_median.iqr == 'Model 1']   <- 'ED Panel'
    results$summary.table$Before_mean.sd[results$summary.table$Before_median.iqr == 'Model 2']               <- 'Estimated closure coefficients'
    results$summary.table$Before_min.max[results$summary.table$Before_median.iqr == 'Model 2']     <- 'Individual Case Site'
    results$summary.table$After_mean.sd[results$summary.table$Before_median.iqr == 'Model 2']      <- 'Primary Control'
    results$summary.table$After_median.iqr[results$summary.table$Before_median.iqr == 'Model 2']   <- 'ED Panel'
    results$summary.table$Before_mean.sd[results$summary.table$Before_median.iqr == 'Model 3.1']               <- 'Estimated closure coefficients'
    results$summary.table$Before_min.max[results$summary.table$Before_median.iqr == 'Model 3.1']    <- 'Individual Case Site'
    results$summary.table$After_mean.sd[results$summary.table$Before_median.iqr == 'Model 3.1']    <- 'All Control'
    results$summary.table$After_median.iqr[results$summary.table$Before_median.iqr == 'Model 3.1'] <- 'ED Panel'
    results$summary.table$Before_mean.sd[results$summary.table$Before_median.iqr == 'Model 3.2']               <- 'Estimated closure coefficients'
    results$summary.table$Before_min.max[results$summary.table$Before_median.iqr == 'Model 3.2']    <- 'Individual Case Site'
    results$summary.table$After_mean.sd[results$summary.table$Before_median.iqr == 'Model 3.2']    <- 'All Controls Pooled'
    results$summary.table$After_median.iqr[results$summary.table$Before_median.iqr == 'Model 3.2'] <- 'ED Panel'
    results$summary.table$Before_mean.sd[results$summary.table$Before_median.iqr == 'Model 4']               <- 'Estimated closure coefficients'
    results$summary.table$Before_min.max[results$summary.table$Before_median.iqr == 'Model 4']     <- 'All Case Sites'
    results$summary.table$After_mean.sd[results$summary.table$Before_median.iqr == 'Model 4']      <- 'Primary Control'
    results$summary.table$After_median.iqr[results$summary.table$Before_median.iqr == 'Model 4']   <- 'ED Panel'
    results$summary.table$Before_mean.sd[results$summary.table$Before_median.iqr == 'Model 5']               <- 'Estimated closure coefficients'
    results$summary.table$Before_min.max[results$summary.table$Before_median.iqr == 'Model 5']     <- 'All Case Sites'
    results$summary.table$After_mean.sd[results$summary.table$Before_median.iqr == 'Model 5']      <- 'All Controls'
    results$summary.table$After_median.iqr[results$summary.table$Before_median.iqr == 'Model 5']   <- 'ED Panel'
    results$summary.table$Before_mean.sd[results$summary.table$Before_median.iqr == 'Model 6.1']               <- 'Estimated difference in time to ED coefficient'
    results$summary.table$Before_min.max[results$summary.table$Before_median.iqr == 'Model 6.1']    <- 'Individual Case Site'
    results$summary.table$After_mean.sd[results$summary.table$Before_median.iqr == 'Model 6.1']    <- 'None'
    results$summary.table$After_median.iqr[results$summary.table$Before_median.iqr == 'Model 6.1'] <- 'LSOA Panel'
    results$summary.table$Before_mean.sd[results$summary.table$Before_median.iqr == 'Model 6.2']               <- 'Estimated difference in time to ED coefficient'
    results$summary.table$Before_min.max[results$summary.table$Before_median.iqr == 'Model 6.2']    <- 'Individual Case Site'
    results$summary.table$After_mean.sd[results$summary.table$Before_median.iqr == 'Model 6.2']    <- 'Primary Control'
    results$summary.table$After_median.iqr[results$summary.table$Before_median.iqr == 'Model 6.2'] <- 'LSOA Panel'
    results$summary.table$Before_mean.sd[results$summary.table$Before_median.iqr == 'Model 7.1']               <- 'Estimated difference in time to ED coefficient'
    results$summary.table$Before_min.max[results$summary.table$Before_median.iqr == 'Model 7.1']     <- 'All Case Sites'
    results$summary.table$After_mean.sd[results$summary.table$Before_median.iqr == 'Model 7.1']      <- 'None'
    results$summary.table$After_median.iqr[results$summary.table$Before_median.iqr == 'Model 7.1']   <- 'LSOA Panel'
    results$summary.table$Before_mean.sd[results$summary.table$Before_median.iqr == 'Model 7.2']               <- 'Estimated difference in time to ED coefficient'
    results$summary.table$Before_min.max[results$summary.table$Before_median.iqr == 'Model 7.2']     <- 'All Case Sites'
    results$summary.table$After_mean.sd[results$summary.table$Before_median.iqr == 'Model 7.2']      <- 'All Controls'
    results$summary.table$After_median.iqr[results$summary.table$Before_median.iqr == 'Model 7.2']   <- 'LSOA Panel'
    results$summary.table$town[is.na(results$summary.table$diff_abs)] <- results$summary.table$group[is.na(results$summary.table$diff_abs)]
    ## Site specific tables
    ## Bishop Auckland
    results$summary.table.bishop <- dplyr::filter(results$summary.table,
                                           group %in% c('Bishop Auckland', 'All')) %>%
                                    dplyr::select(town, Before_mean.sd, Before_median.iqr, Before_min.max, After_mean.sd, After_median.iqr, After_min.max, diff_abs, diff_perc)
    names(results$summary.table.bishop) <- c('Town', 'Pre Mean (SD)', 'Pre Median (IQR)', 'Pre Range', 'Post Mean (SD)', 'Post Median (IQR)', 'Post Range', 'Difference', 'Percentage')
    ## Hartlepool
    results$summary.table.hartlepool <- dplyr::filter(results$summary.table,
                                                     group %in% c('Hartlepool', 'All')) %>%
                                    dplyr::select(town, Before_mean.sd, Before_median.iqr, Before_min.max, After_mean.sd, After_median.iqr, After_min.max, diff_abs, diff_perc)
    names(results$summary.table.hartlepool) <- c('Town', 'Pre Mean (SD)', 'Pre Median (IQR)', 'Pre Range', 'Post Mean (SD)', 'Post Median (IQR)', 'Post Range', 'Difference', 'Percentage')
    ## Hemel Hempstead
    results$summary.table.hemel <- dplyr::filter(results$summary.table,
                                          group %in% c('Hemel Hempstead', 'All')) %>%
                                    dplyr::select(town, Before_mean.sd, Before_median.iqr, Before_min.max, After_mean.sd, After_median.iqr, After_min.max, diff_abs, diff_perc)
    names(results$summary.table.hemel) <- c('Town', 'Pre Mean (SD)', 'Pre Median (IQR)', 'Pre Range', 'Post Mean (SD)', 'Post Median (IQR)', 'Post Range', 'Difference', 'Percentage')
    ## Newark
    results$summary.table.newark <- dplyr::filter(results$summary.table,
                                           group %in% c('Newark', 'All')) %>%
                                    dplyr::select(town, Before_mean.sd, Before_median.iqr, Before_min.max, After_mean.sd, After_median.iqr, After_min.max, diff_abs, diff_perc)
    names(results$summary.table.newark) <- c('Town', 'Pre Mean (SD)', 'Pre Median (IQR)', 'Pre Range', 'Post Mean (SD)', 'Post Median (IQR)', 'Post Range', 'Difference', 'Percentage')
    ## Rochdale
    results$summary.table.rochdale <- dplyr::filter(results$summary.table,
                                             group %in% c('Rochdale', 'All')) %>%
                                    dplyr::select(town, Before_mean.sd, Before_median.iqr, Before_min.max, After_mean.sd, After_median.iqr, After_min.max, diff_abs, diff_perc)
    names(results$summary.table.rochdale) <- c('Town', 'Pre Mean (SD)', 'Pre Median (IQR)', 'Pre Range', 'Post Mean (SD)', 'Post Median (IQR)', 'Post Range', 'Difference', 'Percentage')
    ## Finaly sort the overall table
    ## results$summary.table <- dplyr::select(results$summary.table, town, Before_mean.sd, Before_median.iqr, Before_min.max, After_mean.sd, After_median.iqr, After_min.max, diff_abs, diff_perc)
    ## names(results$summary.table) <- c('Town', 'Pre Mean (SD)', 'Pre Median (IQR)', 'Pre Range', 'Post Mean (SD)', 'Post Median (IQR)', 'Post Range', 'Difference', 'Percentage')
    #######################################################################
    ## Return the results                                                ##
    #######################################################################
    return(results)
}
