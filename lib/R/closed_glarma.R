#' Perform Regression on ClosED data
#'
#' @description Perform Negative-Binomial Time-series regression on ClosED data.
#'
#' @details The ClosED study uses time-series with dummy indicators to
#' test for the impact of closing Emergency Departments on indicators of
#' performance.  This performs
#' Negative-Binomial Time-series regression analysis to account for atuo-correlation.
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
#' @param model0 Covariates to include in model 0.
#' @param model1 Covariates to include in model 1.
#' @param model2 Covariates to include in model 2.
#' @param model3.1 Covariates to include in model 3.
#' @param model3b Covariates to include in model 8.
#' @param model4 Covariates to include in model 4.
#' @param model5 Covariates to include in model 5.
#' @param model6 Covariates to include in model 6.
#' @param model7 Covariates to include in model 7.
#' @param glarma.link Link to use for \code{glarma()} (options are \code{log}, the default as it permits negative values and \code{identity} which is constrainged to positive values).
#' @param glarma.model Model (i.e. lags in this instance) to use in the call to \code{glarma}.
#' @param glarma.distr Distribution to use in the call to \code{glarma} (options are \code{nbinom} the default to allow for over-disperssion and \code{poisson}).
#' @param return.df Logical operator of whether to return the subsetted/summarised data frame (useful for subsequent development).
#' @param return.model Logical operator of whether to return the fitted models (not currently working correctly).
#' @param return.residuals Logical oeprator of whether to return the residuals of the fitted model.
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
closed_glarma <- function(df.lsoa          = ed_attendances_by_mode_measure,
                         df.trust         = ed_attendances_by_mode_site_measure_clean,
                         indicator        = 'ed attendances',
                         sub.indicator    = 'any',
                         steps            = c('closure'),
                         panel.lsoa       = 'lsoa',
                         panel.trust      = 'town',
                         timevar          = 'relative.month',
                         outcome          = 'value',
                         model0           = NULL,
                         model1           = c('closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
                         model2           = c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
                         model3.1         = c('pooled.control * closure', 'town', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
                         model3.2         = c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
                         model4           = c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
                         model5           = c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
                         model6.1         = c('season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert', 'diff.time.to.ed'),
                         model6.2         = c('town', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert', 'diff.time.to.ed'),
                         model7           = c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert', 'diff.time.to.ed'),
                         glarma.link       = 'log',
                         glarma.model      = list(past_obs = 1),
                         glarma.distr      = 'nbinom',
                         coefficients     = c('closure.town'),
                         return.df        = FALSE,
                         return.model     = TRUE,
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
    #######################################################################
    ## Derive the mean, sd, median, iqr, min and max of events before/   ##
    ## after closure for combining into a summary table with model       ##
    ## coefficients                                                      ##
    #######################################################################
    names(df.trust) %>% print()
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
    print(results$summary.df)
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
                                         diff_abs  = Before_mean - After_mean,
                                         diff_perc = (100 * abs(Before_mean - After_mean)) / Before_mean) %>%
                                  dplyr::select(-Before_mean, -After_mean)
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
    ## Convert to data frame, selecting only the specified outcome and expand season
    ## to a series of dummy variables for inclusion
    print("Debug 3")
    df.lsoa  <- as.data.frame(df.lsoa) %>%
                dplyr::filter(measure == indicator,
                              sub.measure == sub.indicator) %>%
                dplyr::filter(!is.na(value))
    season <- model.matrix(~df.lsoa$season) %>% as.data.frame()
    names(season) <- c('intercept', 'season2', 'season3', 'season4', 'season5', 'season6')
    season <- dplyr::select(season, season2, season3, season4, season5, season6)
    df.lsoa <- cbind(df.lsoa, season)
    df.trust <- as.data.frame(df.trust) %>%
                dplyr::filter(measure == indicator,
                              sub.measure == sub.indicator) %>%
                dplyr::filter(!is.na(value))
    season <- model.matrix(~df.trust$season) %>% as.data.frame()
    names(season) <- c('intercept', 'season2', 'season3', 'season4', 'season5', 'season6')
    season <- dplyr::select(season, season2, season3, season4, season5, season6)
    df.trust <- cbind(df.trust, season)
    #######################################################################
    ## Internal functions (to save typing)                               ##
    #######################################################################
    ## extract_coefficients <- function(x,
    ##                                  .site          = site,
    ##                                  .indicator     = indicator,
    ##                                  .sub.indicator = sub.indicator){
    ##     ## Extract coefficients from a given model
    ##     coefficients <- summary(x) %>%
    ##                     coef() %>%
    ##                     as.data.frame()
    ##     coefficients$term <- rownames(coefficients)
    ##     rownames(coefficients) <- NULL
    ##     coefficients$site          <- .site
    ##     coefficients$indicator     <- .indicator
    ##     coefficients$sub.indicator <- .sub.indicator
    ##     coefficients$r2            <- x$r2
    ##     return(coefficients)
    ## }
    combine_coefficients <- function(bishop.coef     = results$model0.glarma.bishop.coef,
                                     hartlepool.coef = results$model0.glarma.hartlepool.coef,
                                     hemel.coef      = results$model0.glarma.hemel.coef,
                                     newark.coef     = results$model0.glarma.newark.coef,
                                     rochdale.coef   = results$model0.glarma.rochdale.coef,
                                     all.coef        = NULL,
                                     .indicator      = indicator,
                                     .sub.indicator  = sub.indicator){
        ## List of results
        coef <- list()
        ## Combine coefficients, SE and CIs and derive tidy df for output
        ## print("Internal Debug 1")
        bishop <- cbind(bishop.coef$est,
                        bishop.coef$se,
                        bishop.coef$ci) %>%
                  data.frame()
        bishop$coefficient <- rownames(bishop)
        bishop$town <- 'Bishop Auckland'
        ## print("Internal Debug 2")
        hartlepool <- cbind(hartlepool.coef$est,
                            hartlepool.coef$se,
                            hartlepool.coef$ci) %>%
                      data.frame()
        hartlepool$coefficient <- rownames(hartlepool)
        hartlepool$town <- 'Hartlepool'
        ## print("Internal Debug 3")
        hemel <- cbind(hemel.coef$est,
                       hemel.coef$se,
                       hemel.coef$ci) %>%
                  data.frame()
        hemel$coefficient <- rownames(hemel)
        hemel$town <- 'Hemel Hempstead'
        ## print("Internal Debug 4")
        newark <- cbind(newark.coef$est,
                        newark.coef$se,
                        newark.coef$ci) %>%
                  data.frame()
        newark$coefficient <- rownames(newark)
        newark$town <- 'Newark'
        ## print("Internal Debug 5")
        rochdale <- cbind(rochdale.coef$est,
                          rochdale.coef$se,
                          rochdale.coef$ci) %>%
                  data.frame()
        rochdale$coefficient <- rownames(rochdale)
        rochdale$town <- 'Rochdale'
        if(!is.null(all.coef)){
            ## print("Internal Debug 6")
            all <- cbind(all.coef$est,
                         all.coef$se,
                         all.coef$ci) %>%
                data.frame()
            all$coefficient <- rownames(all)
            all$town <- 'All'
            .coefficients <- rbind(bishop,
                                   hartlepool,
                                   hemel,
                                   newark,
                                   rochdale,
                                   all)
        }
        else{
            .coefficients <- rbind(bishop,
                                   hartlepool,
                                   hemel,
                                   newark,
                                   rochdale)
        }
        names(.coefficients)        <- c('est', 'se', 'lower', 'upper', 'term', 'site')
        rownames(.coefficients) <- NULL
        .coefficients$indicator     <- .indicator
        .coefficients$sub.indicator <- .sub.indicator
        return(.coefficients)
    }
    tidy_coefficients <- function(df    = results$model0.glarma.coefficients,
                                  return.coef     = coefficients,
                                  bishop.coef     = TRUE,
                                  hartlepool.coef = TRUE,
                                  hemel.coef      = TRUE,
                                  newark.coef     = TRUE,
                                  rochdale.coef   = TRUE,
                                  all.coef        = FALSE,
                                  ...){
        coef <- list()
        ## Produce a formatted table for printing, produce output and reshape
        ## print("Internal Debug 7")
        out <- dplyr::select(df, indicator, sub.indicator, term, site, est, lower, upper) %>%
            data.frame()
        out$out <- paste0(formatC(out$est, digits = 3, format = 'f'),
                          ' (',
                          formatC(out$lower, digits = 3, format = 'f'),
                          ' - ',
                          formatC(out$upper, digits = 3, format = 'f'),
                          ')')
        out <- dplyr::select(out, indicator, sub.indicator, term, site, out) %>%
               melt(id = c('indicator', 'sub.indicator', 'site', 'term')) %>%
            dcast(indicator + sub.indicator + term ~ site + variable)
        ## Conditionally remove the coefficients that are not of interest
        if(return.coef == 'closure'){
            out <- dplyr::filter(out, grepl('closure', term))
        }
        else if(return.coef == 'town'){
            out <- dplyr::filter(out, grepl('town', term))
        }
        else if(return.coef == 'closure.town'){
            out <- dplyr::filter(out, grepl('closure', term) | grepl('town', term))
        }
        ## Not really necessary, but it makes the code clear
        else if(return.coef == 'all.steps'){
            out <- dplyr::filter(out, grepl('closure', term) | grepl('town', term) | grepl('nhs111', term) | grepl('ambulance.divert', term) | grepl('other.closure', term))
        }
        ## Not really necessary, but it makes the code clear
        else if(return.coef == 'all'){
            out <- out
        }
        ## Format the term label for interactions between site and town
        out <- within(out, {
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
        ## print("Internal Debug 8")
        ## Build the column names conditional on the non-null arguments
        ## Bear in mind that the reshape puts everything in alphabetical order
        ## Stub that all require
        column.names <- c('Indicator', 'Subindicator', 'Term')
        ## All column
        if(all.coef == TRUE){
            column.names <- c(column.names, 'All')
        }
        if(bishop.coef == TRUE){
            column.names <- c(column.names, 'Bishop Auckland')
        }
        if(hartlepool.coef == TRUE){
            column.names <- c(column.names, 'Hartlepool')
        }
        if(hemel.coef == TRUE){
            column.names <- c(column.names, 'Hemel Hempstead')
        }
        if(newark.coef == TRUE){
            column.names <- c(column.names, 'Newark')
        }
        if(rochdale.coef == TRUE){
            column.names <- c(column.names, 'Rochdale')
        }
        coef$coef <- out
        names(coef$coef) <- column.names
        ## Derive a caption for the table
        coef$caption <- paste0('Comparison of coefficients from Negative-Binomial Time-Series Generalised Linear Auto-Regressive Moving Average Regression across sites.  Each cell contains a point estimate followed by the confidence interval (in brackets).')
        return(coef)
    }
    #######################################################################
    ## Model 0                                                           ##
    #######################################################################
    if(!is.null(model0)){
        print("Model 0")
        ## Subset data
        sites <- c('Bishop Auckland', 'Hartlepool', 'Hemel Hempstead', 'Newark', 'Rochdale')
        df0 <- dplyr::filter(df.trust,
                      town %in% sites &
                      measure     == indicator &
                      sub.measure == sub.indicator)
        ##################################################
        ## Model 0 - Bishop Auckland                    ##
        ##################################################
        print("Bishop Auckland")
        ts.vector  <- dplyr::filter(df0,
                             town        == 'Bishop Auckland') %>%
                      as.data.frame() %>% .[,'value']
        regressors <- dplyr::filter(df0,
                             town        == 'Bishop Auckland') %>%
                      dplyr::select(closure)
        ## return(t)
        if(town.group$n[town.group$town == 'Bishop Auckland'] > 0){
            model0.glarma.bishop <- glarma(y = ts.vector,
                                         X = regressors,
                                         type = 'NegBin')
            results$model0.glarma.bishop.coef <- se(model0.glarma.bishop)
            results$model0.glarma.bishop.coef$site          <- 'Bishop Auckland'
            results$model0.glarma.bishop.coef$indicator     <- indicator
            results$model0.glarma.bishop.coef$sub.indicator <- sub.indicator
        }
        ##################################################
        ## Model 0 - Hartlepool                         ##
        ##################################################
        ## print("Hartlepool")
        ts.vector  <- dplyr::filter(df0,
                             town        == 'Hartlepool') %>%
                      as.data.frame() %>% .[,'value']
        regressors <- dplyr::filter(df0,
                             town        == 'Hartlepool') %>%
                      dplyr::select(closure)
        if(town.group$n[town.group$town == 'Hartlepool'] > 0){
            model0.glarma.hartlepool <- glarma(ts = ts.vector,
                                             link = glarma.link,
                                             model = glarma.model,
                                             xreg  = regressors,
                                             distr = glarma.distr)
            results$model0.glarma.hartlepool.coef <- se(model0.glarma.hartlepool)
            results$model0.glarma.hartlepool.coef$site          <- 'Hartlepool'
            results$model0.glarma.hartlepool.coef$indicator     <- indicator
            results$model0.glarma.hartlepool.coef$sub.indicator <- sub.indicator
        }
        ##################################################
        ## Model 0 - Hemel Hempstead                    ##
        ##################################################
        ## print("Hemel Hempstead")
        ts.vector  <- dplyr::filter(df0,
                             town        == 'Hemel Hempstead') %>%
                      as.data.frame() %>% .[,'value']
        regressors <- dplyr::filter(df0,
                             town        == 'Hemel Hempstead') %>%
                      dplyr::select(closure)
        if(town.group$n[town.group$town == 'Hemel Hempstead'] > 0){
            model0.glarma.hemel <- glarma(ts = ts.vector,
                                        link = glarma.link,
                                        model = glarma.model,
                                        xreg  = regressors,
                                        distr = glarma.distr)
            results$model0.glarma.hemel.coef <- se(model0.glarma.hemel)
            results$model0.glarma.hemel.coef$site          <- 'Hemel Hempstead'
            results$model0.glarma.hemel.coef$indicator     <- indicator
            results$model0.glarma.hemel.coef$sub.indicator <- sub.indicator
        }
        ##################################################
        ## Model 0 - Newark                             ##
        ##################################################
        ## print("Newark")
        ts.vector  <- dplyr::filter(df0,
                             town        == 'Newark') %>%
                      as.data.frame() %>% .[,'value']
        regressors <- dplyr::filter(df0,
                             town        == 'Newark') %>%
                      dplyr::select(closure)
        if(town.group$n[town.group$town == 'Newark'] > 0){
            model0.glarma.newark <- glarma(y = ts.vector,
                                         X = regressors,
                                         type = 'NegBin')
            results$model0.glarma.newark.coef <- se(model0.glarma.newark)
            results$model0.glarma.newark.coef$site          <- 'Newark'
            results$model0.glarma.newark.coef$indicator     <- indicator
            results$model0.glarma.newark.coef$sub.indicator <- sub.indicator
        }
        ##################################################
        ## Model 0 - Rochdale                           ##
        ##################################################
        ## print("Rochdale")
        ts.vector  <- dplyr::filter(df0,
                             town        == 'Rochdale') %>%
                      as.data.frame() %>% .[,'value']
        regressors <- dplyr::filter(df0,
                             town        == 'Rochdale') %>%
                      dplyr::select(closure)
        if(town.group$n[town.group$town == 'Rochdale'] > 0){
            model0.glarma.rochdale <- glarma(ts = ts.vector,
                                           link = glarma.link,
                                           model = glarma.model,
                                           xreg  = regressors,
                                           distr = glarma.distr)
            results$model0.glarma.rochdale.coef <- se(model0.glarma.rochdale)
            results$model0.glarma.rochdale.coef$site          <- 'Rochdale'
            results$model0.glarma.rochdale.coef$indicator     <- indicator
            results$model0.glarma.rochdale.coef$sub.indicator <- sub.indicator
        }
        ## Summary table
        results$model0.glarma.coefficients <- combine_coefficients(bishop.coef     = results$model0.glarma.bishop.coef,
                                                                  hartlepool.coef = results$model0.glarma.hartlepool.coef,
                                                                  hemel.coef      = results$model0.glarma.hemel.coef,
                                                                  newark.coef     = results$model0.glarma.newark.coef,
                                                                  rochdale.coef   = results$model0.glarma.rochdale.coef,
                                                                  .indicator      = indicator,
                                                                  .sub.indicator  = sub.indicator)
        ## Tidy the output (matches that generated by closed_models())
        results$model0.glarma.coef <- tidy_coefficients(df    = results$model0.glarma.coefficients,
                                                       .coef = coefficients,
                                                       bishop.coef     = TRUE,
                                                       hartlepool.coef = TRUE,
                                                       hemel.coef      = TRUE,
                                                       newark.coef     = TRUE,
                                                       rochdale.coef   = TRUE,
                                                       all.coef        = FALSE)
        ## Extract coefficients for plotting
        ## Forest plot
        results$model0.forest <- closed_forest(df.list = list(results$model0.glarma.coefficients),
                                               plot.term     = c('closure'),
                                               facet.outcome = FALSE,
                                               title         = paste0('Model 0 : ',
                                                                      indicator,
                                                                      ' (',
                                                                      sub.indicator,
                                                                      ')'),
                                               theme         = theme_bw())
        ## Return model objects if requested
        if(return.model == TRUE){
            if(exists('model0.glarma.bishop')){
                results$model0.glarma.bishop     <- model0.glarma.bishop
            }
            if(exists('model0.glarma.hartlepool')){
                results$model0.glarma.hartlepool <- model0.glarma.hartlepool
            }
            if(exists('model0.glarma.hemel')){
                results$model0.glarma.hemel      <- model0.glarma.hemel
            }
            if(exists('model0.glarma.newark')){
                results$model0.glarma.newark     <- model0.glarma.newark
            }
            if(exists('model0.glarma.rochdale')){
                results$model0.glarma.rochdale   <- model0.glarma.rochdale
            }
        }
        if(return.df == TRUE){
            results$model0.df <- df0
        }
        if(return.residuals == TRUE){
            if(exists('model0.glarma.bishop')){
                results$model0.glarma.residuals.bishop     <- summary(model0.glarma.bishop)$residuals
            }
            if(exists('model0.glarma.hartlepool')){
                results$model0.glarma.residuals.hartlepool <- summary(model0.glarma.hartlepool)$residuals
            }
            if(exists('model0.glarma.hemel')){
                results$model0.glarma.residuals.hemel      <- summary(model0.glarma.hemel)$residuals
            }
            if(exists('model0.glarma.newark')){
                results$model0.glarma.residuals.newark     <- summary(model0.glarma.newark)$residuals
            }
            if(exists('model0.glarma.rochdale')){
                results$model0.glarma.residuals.rochdale   <- summary(model0.glarma.rochdale)$residuals
            }
        }
        ## Remove clutter
        rm(df0)
    }
    #######################################################################
    ## Model 1                                                           ##
    #######################################################################
    if(!is.null(model1)){
        ## print("Model 1")
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
        ts.vector  <- dplyr::filter(df1,
                             town        == 'Bishop Auckland') %>%
                      as.data.frame() %>% .[,'value']
        regressors <- dplyr::filter(df1,
                             town        == 'Bishop Auckland') %>%
                      dplyr::select(closure, season2, season3, season4, season5, season6, relative.month, nhs111)
        ## return(t)
        if(town.group$n[town.group$town == 'Bishop Auckland'] > 0){
            model1.glarma.bishop <- glarma(y = ts.vector,
                                         X = regressors,
                                         type = 'NegBin')
            results$model1.glarma.bishop.coef <- se(model1.glarma.bishop)
            results$model1.glarma.bishop.coef$site          <- 'Bishop Auckland'
            results$model1.glarma.bishop.coef$indicator     <- indicator
            results$model1.glarma.bishop.coef$sub.indicator <- sub.indicator
        }
        ##################################################
        ## Model 1 - Hartlepool                         ##
        ##################################################
        ## print("Hartlepool")
        ts.vector  <- dplyr::filter(df1,
                             town        == 'Hartlepool') %>%
                      as.data.frame() %>% .[,'value']
        regressors <- dplyr::filter(df1,
                             town        == 'Hartlepool') %>%
                      dplyr::select(closure, season2, season3, season4, season5, season6, relative.month, nhs111, other.centre)
        if(town.group$n[town.group$town == 'Hartlepool'] > 0){
            model1.glarma.hartlepool <- glarma(ts = ts.vector,
                                             link = glarma.link,
                                             model = glarma.model,
                                             xreg  = regressors,
                                             distr = glarma.distr)
            results$model1.glarma.hartlepool.coef <- se(model1.glarma.hartlepool)
            results$model1.glarma.hartlepool.coef$site          <- 'Hartlepool'
            results$model1.glarma.hartlepool.coef$indicator     <- indicator
            results$model1.glarma.hartlepool.coef$sub.indicator <- sub.indicator
        }
        ##################################################
        ## Model 1 - Hemel Hempstead                    ##
        ##################################################
        ## print("Hemel Hempstead")
        ts.vector  <- dplyr::filter(df1,
                             town        == 'Hemel Hempstead') %>%
                      as.data.frame() %>% .[,'value']
        regressors <- dplyr::filter(df1,
                             town        == 'Hemel Hempstead') %>%
                      dplyr::select(closure, season2, season3, season4, season5, season6, relative.month, other.centre)
        if(town.group$n[town.group$town == 'Hemel Hempstead'] > 0){
            model1.glarma.hemel <- glarma(ts = ts.vector,
                                        link = glarma.link,
                                        model = glarma.model,
                                        xreg  = regressors,
                                        distr = glarma.distr)
            results$model1.glarma.hemel.coef <- se(model1.glarma.hemel)
            results$model1.glarma.hemel.coef$site          <- 'Hemel Hempstead'
            results$model1.glarma.hemel.coef$indicator     <- indicator
            results$model1.glarma.hemel.coef$sub.indicator <- sub.indicator
        }
        ##################################################
        ## Model 1 - Newark                             ##
        ##################################################
        ## print("Newark")
        ts.vector  <- dplyr::filter(df1,
                             town        == 'Newark') %>%
                      as.data.frame() %>% .[,'value']
        regressors <- dplyr::filter(df1,
                             town        == 'Newark') %>%
                      dplyr::select(closure, season2, season3, season4, season5, season6, relative.month, other.centre)
        if(town.group$n[town.group$town == 'Newark'] > 0){
            model1.glarma.newark <- glarma(y = ts.vector,
                                         X = regressors,
                                         type = 'NegBin')
            results$model1.glarma.newark.coef <- se(model1.glarma.newark)
            results$model1.glarma.newark.coef$site          <- 'Newark'
            results$model1.glarma.newark.coef$indicator     <- indicator
            results$model1.glarma.newark.coef$sub.indicator <- sub.indicator
        }
        ##################################################
        ## Model 1 - Rochdale                           ##
        ##################################################
        ## print("Rochdale")
        ts.vector  <- dplyr::filter(df1,
                             town        == 'Rochdale') %>%
                      as.data.frame() %>% .[,'value']
        regressors <- dplyr::filter(df1,
                             town        == 'Rochdale') %>%
                      dplyr::select(closure, season2, season3, season4, season5, season6, relative.month, nhs111, other.centre, ambulance.divert)
        if(town.group$n[town.group$town == 'Rochdale'] > 0){
            model1.glarma.rochdale <- glarma(ts = ts.vector,
                                           link = glarma.link,
                                           model = glarma.model,
                                           xreg  = regressors,
                                           distr = glarma.distr)
            results$model1.glarma.rochdale.coef <- se(model1.glarma.rochdale)
            results$model1.glarma.rochdale.coef$site          <- 'Rochdale'
            results$model1.glarma.rochdale.coef$indicator     <- indicator
            results$model1.glarma.rochdale.coef$sub.indicator <- sub.indicator
        }
        ## Summary table
        results$model1.glarma.coefficients <- combine_coefficients(bishop.coef     = results$model1.glarma.bishop.coef,
                                                                  hartlepool.coef = results$model1.glarma.hartlepool.coef,
                                                                  hemel.coef      = results$model1.glarma.hemel.coef,
                                                                  newark.coef     = results$model1.glarma.newark.coef,
                                                                  rochdale.coef   = results$model1.glarma.rochdale.coef,
                                                                  .indicator      = indicator,
                                                                  .sub.indicator  = sub.indicator)
        ## Tidy the output (matches that generated by closed_models())
        results$model1.glarma.coef <- tidy_coefficients(df    = results$model1.glarma.coefficients,
                                                       .coef = coefficients,
                                                       bishop.coef     = TRUE,
                                                       hartlepool.coef = TRUE,
                                                       hemel.coef      = TRUE,
                                                       newark.coef     = TRUE,
                                                       rochdale.coef   = TRUE,
                                                       all.coef        = FALSE)
        ## Extract coefficients for plotting
        ## Forest plot
        results$model1.forest <- closed_forest(df.list = list(results$model1.glarma.coefficients),
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
            if(exists('model1.glarma.bishop')){
                results$model1.glarma.bishop     <- model1.glarma.bishop
            }
            if(exists('model1.glarma.hartlepool')){
                results$model1.glarma.hartlepool <- model1.glarma.hartlepool
            }
            if(exists('model1.glarma.hemel')){
                results$model1.glarma.hemel      <- model1.glarma.hemel
            }
            if(exists('model1.glarma.newark')){
                results$model1.glarma.newark     <- model1.glarma.newark
            }
            if(exists('model1.glarma.rochdale')){
                results$model1.glarma.rochdale   <- model1.glarma.rochdale
            }
        }
        if(return.df == TRUE){
            results$model1.df <- df1
        }
        if(return.residuals == TRUE){
            if(exists('model1.glarma.bishop')){
                results$model1.glarma.residuals.bishop     <- summary(model1.glarma.bishop)$residuals
            }
            if(exists('model1.glarma.hartlepool')){
                results$model1.glarma.residuals.hartlepool <- summary(model1.glarma.hartlepool)$residuals
            }
            if(exists('model1.glarma.hemel')){
                results$model1.glarma.residuals.hemel      <- summary(model1.glarma.hemel)$residuals
            }
            if(exists('model1.glarma.newark')){
                results$model1.glarma.residuals.newark     <- summary(model1.glarma.newark)$residuals
            }
            if(exists('model1.glarma.rochdale')){
                results$model1.glarma.residuals.rochdale   <- summary(model1.glarma.rochdale)$residuals
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
        ## Subset data
        sites <- c('Bishop Auckland', 'Whitehaven',
                   'Hartlepool', 'Grimsby',
                   'Hemel Hempstead', 'Warwick',
                   'Newark', 'Southport',
                   'Rochdale', 'Rotherham')
        df2 <- dplyr::filter(df.trust,
                      town %in% sites &
                      measure     == indicator &
                      sub.measure == sub.indicator)
        ##################################################
        ## Model 2 - Bishop Auckland                    ##
        ##################################################
        ## print("Bishop Auckland")
        ts.vector  <- dplyr::filter(df2,
                             town        %in% c('Bishop Auckland', 'Whitehaven')) %>%
                      as.data.frame() %>% .[,'value']
        df2$town_ <- ifelse(df2$town == 'Bishop Auckland', 1, 0)
        df2$town_closure <- df2$town_ * df2$closure
        regressors <- dplyr::filter(df2,
                             town        %in% c('Bishop Auckland', 'Whitehaven')) %>%
                      dplyr::select(closure, town_, town_closure, season2, season3, season4, season5, season6, relative.month, nhs111)
        ## return(t)
        if(town.group$n[town.group$town == 'Bishop Auckland'] > 0){
            model2.glarma.bishop <- glarma(y = ts.vector,
                                         X = regressors,
                                         type = 'NegBin')
            results$model2.glarma.bishop.coef <- se(model2.glarma.bishop)
            results$model2.glarma.bishop.coef$site          <- 'Bishop Auckland'
            results$model2.glarma.bishop.coef$indicator     <- indicator
            results$model2.glarma.bishop.coef$sub.indicator <- sub.indicator
        }
        ##################################################
        ## Model 2 - Hartlepool                         ##
        ##################################################
        ## print("Hartlepool")
        ts.vector  <- dplyr::filter(df2,
                             town        %in% c('Hartlepool', 'Grimsby')) %>%
                      as.data.frame() %>% .[,'value']
        df2$town_ <- ifelse(df2$town == 'Hartlepool', 1, 0)
        df2$town_closure <- df2$town_ * df2$closure
        regressors <- dplyr::filter(df2,
                             town        %in% c('Hartlepool', 'Grimsby')) %>%
                      dplyr::select(closure, town_, town_closure, season2, season3, season4, season5, season6, relative.month, nhs111, other.centre)
        if(town.group$n[town.group$town == 'Hartlepool'] > 0){
            model2.glarma.hartlepool <- glarma(ts = ts.vector,
                                             link = glarma.link,
                                             model = glarma.model,
                                             xreg  = regressors,
                                             distr = glarma.distr)
            results$model2.glarma.hartlepool.coef <- se(model2.glarma.hartlepool)
            results$model2.glarma.hartlepool.coef$site          <- 'Hartlepool'
            results$model2.glarma.hartlepool.coef$indicator     <- indicator
            results$model2.glarma.hartlepool.coef$sub.indicator <- sub.indicator
        }
        ##################################################
        ## Model 2 - Hemel Hempstead                    ##
        ##################################################
        ## print("Hemel Hempstead")
        ts.vector  <- dplyr::filter(df2,
                             town        %in% c('Hemel Hempstead', 'Warwick')) %>%
                      as.data.frame() %>% .[,'value']
        df2$town_ <- ifelse(df2$town == 'Hemel Hempstead', 1, 0)
        df2$town_closure <- df2$town_ * df2$closure
        regressors <- dplyr::filter(df2,
                             town        %in% c('Hemel Hempstead', 'Warwick')) %>%
                      dplyr::select(closure, town_, town_closure, season2, season3, season4, season5, season6, relative.month, other.centre)
        if(town.group$n[town.group$town == 'Hemel Hempstead'] > 0){
            model2.glarma.hemel <- glarma(ts = ts.vector,
                                        link = glarma.link,
                                        model = glarma.model,
                                        xreg  = regressors,
                                        distr = glarma.distr)
            results$model2.glarma.hemel.coef <- se(model2.glarma.hemel)
            results$model2.glarma.hemel.coef$site          <- 'Hemel Hempstead'
            results$model2.glarma.hemel.coef$indicator     <- indicator
            results$model2.glarma.hemel.coef$sub.indicator <- sub.indicator
        }
        ##################################################
        ## Model 2 - Newark                             ##
        ##################################################
        ## print("Newark")
        ts.vector  <- dplyr::filter(df2,
                             town        %in% c('Newark', 'Southport')) %>%
                      as.data.frame() %>% .[,'value']
        df2$town_ <- ifelse(df2$town == 'Newark', 1, 0)
        df2$town_closure <- df2$town_ * df2$closure
        regressors <- dplyr::filter(df2,
                             town        %in% c('Newark', 'Southport')) %>%
                      dplyr::select(closure, town_, town_closure, season2, season3, season4, season5, season6, relative.month, other.centre)
        if(town.group$n[town.group$town == 'Newark'] > 0){
            model2.glarma.newark <- glarma(y = ts.vector,
                                         X = regressors,
                                         type = 'NegBin')
            results$model2.glarma.newark.coef <- se(model2.glarma.newark)
            results$model2.glarma.newark.coef$site          <- 'Newark'
            results$model2.glarma.newark.coef$indicator     <- indicator
            results$model2.glarma.newark.coef$sub.indicator <- sub.indicator
        }
        ##################################################
        ## Model 2 - Rochdale                           ##
        ##################################################
        ## print("Rochdale")
        ts.vector  <- dplyr::filter(df2,
                             town        %in% c('Rochdale', 'Rotherham')) %>%
                      as.data.frame() %>% .[,'value']
        df2$town_ <- ifelse(df2$town == 'Rochdale', 1, 0)
        df2$town_closure <- df2$town_ * df2$closure
        regressors <- dplyr::filter(df2,
                             town        %in% c('Rochdale', 'Rotherham')) %>%
                      dplyr::select(closure, town_, town_closure, season2, season3, season4, season5, season6, relative.month, nhs111, other.centre, ambulance.divert)
        if(town.group$n[town.group$town == 'Rochdale'] > 0){
            model2.glarma.rochdale <- glarma(ts = ts.vector,
                                           link = glarma.link,
                                           model = glarma.model,
                                           xreg  = regressors,
                                           distr = glarma.distr)
            results$model2.glarma.rochdale.coef <- se(model2.glarma.rochdale)
            results$model2.glarma.rochdale.coef$site          <- 'Rochdale'
            results$model2.glarma.rochdale.coef$indicator     <- indicator
            results$model2.glarma.rochdale.coef$sub.indicator <- sub.indicator
        }
        ## Summary table
        results$model2.glarma.coefficients <- combine_coefficients(bishop.coef     = results$model2.glarma.bishop.coef,
                                                                  hartlepool.coef = results$model2.glarma.hartlepool.coef,
                                                                  hemel.coef      = results$model2.glarma.hemel.coef,
                                                                  newark.coef     = results$model2.glarma.newark.coef,
                                                                  rochdale.coef   = results$model2.glarma.rochdale.coef,
                                                                  .indicator      = indicator,
                                                                  .sub.indicator  = sub.indicator)
        ## Tidy the output (matches that generated by closed_models())
        results$model2.glarma.coef <- tidy_coefficients(df    = results$model2.glarma.coefficients,
                                                       .coef = coefficients,
                                                       bishop.coef     = TRUE,
                                                       hartlepool.coef = TRUE,
                                                       hemel.coef      = TRUE,
                                                       newark.coef     = TRUE,
                                                       rochdale.coef   = TRUE,
                                                       all.coef        = FALSE)
        ## Extract coefficients for plotting
        ## Forest plot
        results$model2.forest <- closed_forest(df.list = list(results$model2.glarma.coefficients),
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
            if(exists('model2.glarma.bishop')){
                results$model2.glarma.bishop     <- model2.glarma.bishop
            }
            if(exists('model2.glarma.hartlepool')){
                results$model2.glarma.hartlepool <- model2.glarma.hartlepool
            }
            if(exists('model2.glarma.hemel')){
                results$model2.glarma.hemel      <- model2.glarma.hemel
            }
            if(exists('model2.glarma.newark')){
                results$model2.glarma.newark     <- model2.glarma.newark
            }
            if(exists('model2.glarma.rochdale')){
                results$model2.glarma.rochdale   <- model2.glarma.rochdale
            }
        }
        if(return.df == TRUE){
            results$model2.df <- df2
        }
        if(return.residuals == TRUE){
            if(exists('model2.glarma.bishop')){
                results$model2.glarma.residuals.bishop     <- summary(model2.glarma.bishop)$residuals
            }
            if(exists('model2.glarma.hartlepool')){
                results$model2.glarma.residuals.hartlepool <- summary(model2.glarma.hartlepool)$residuals
            }
            if(exists('model2.glarma.hemel')){
                results$model2.glarma.residuals.hemel      <- summary(model2.glarma.hemel)$residuals
            }
            if(exists('model2.glarma.newark')){
                results$model2.glarma.residuals.newark     <- summary(model2.glarma.newark)$residuals
            }
            if(exists('model2.glarma.rochdale')){
                results$model2.glarma.residuals.rochdale   <- summary(model2.glarma.rochdale)$residuals
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
        ## Subset data
        df3.1 <- dplyr::filter(df.trust,
                      ## town %in% sites &
                      measure     == indicator &
                      sub.measure == sub.indicator)
        ##################################################
        ## Model 3.1 - Bishop Auckland                  ##
        ##################################################
        ## print("Bishop Auckland")
        ts.vector  <- dplyr::filter(df3.1,
                             town        %in% c('Bishop Auckland', 'Whitehaven', 'Salford', 'Scarborough')) %>%
                      as.data.frame() %>% .[,'value']
        df3.1$town_ <- relevel(df3.1$town, ref = 'Whitehaven')
        town <- model.matrix(~df3.1$town_) %>% as.data.frame()
        names(town) <- c('Intercept', 'basingstoke', 'bishop', 'blackburn', 'carlisle', 'grimsby', 'hartlepool', 'hemel', 'newark', 'rochdale', 'rotherham', 'salford', 'salisbury', 'scarborough', 'scunthorpe', 'southport', 'wansbeck', 'warwick', 'wigan', 'yeovil')
        regressors <- cbind(df3.1, town) %>%
                      dplyr::filter(town        %in% c('Bishop Auckland', 'Whitehaven', 'Salford', 'Scarborough')) %>%
                      dplyr::select(bishop, salford, scarborough, closure, season2, season3, season4, season5, season6, relative.month, nhs111)
        ## return(t)
        if(town.group$n[town.group$town == 'Bishop Auckland'] > 0){
            model3.1.glarma.bishop <- glarma(y = ts.vector,
                                         X = regressors,
                                         type = 'NegBin')
            results$model3.1.glarma.bishop.coef <- se(model3.1.glarma.bishop)
            results$model3.1.glarma.bishop.coef$site          <- 'Bishop Auckland'
            results$model3.1.glarma.bishop.coef$indicator     <- indicator
            results$model3.1.glarma.bishop.coef$sub.indicator <- sub.indicator
        }
        ##################################################
        ## Model 3.1 - Hartlepool                       ##
        ##################################################
        ## print("Hartlepool")
        ts.vector  <- dplyr::filter(df3.1,
                             town        %in% c('Hartlepool', 'Grimsby', 'Blackburn', 'Wigan')) %>%
                      as.data.frame() %>% .[,'value']
        df3.1$town_ <- relevel(df3.1$town, ref = 'Grimsby')
        town <- model.matrix(~df3.1$town_) %>% as.data.frame()
        names(town) <- c('Intercept', 'basingstoke', 'bishop', 'blackburn', 'carlisle', 'hartlepool', 'hemel', 'newark', 'rochdale', 'rotherham', 'salford', 'salisbury', 'scarborough', 'scunthorpe', 'southport', 'wansbeck', 'warwick', 'whitehaven', 'wigan', 'yeovil')
        regressors <- cbind(df3.1, town) %>%
                      dplyr::filter(town        %in% c('Hartlepool', 'Grimsby', 'Blackburn', 'Wigan')) %>%
                      dplyr::select(hartlepool, blackburn, wigan, closure, season2, season3, season4, season5, season6, relative.month, nhs111, other.centre)
        if(town.group$n[town.group$town == 'Hartlepool'] > 0){
            model3.1.glarma.hartlepool <- glarma(ts = ts.vector,
                                             link = glarma.link,
                                             model = glarma.model,
                                             xreg  = regressors,
                                             distr = glarma.distr)
            results$model3.1.glarma.hartlepool.coef <- se(model3.1.glarma.hartlepool)
            results$model3.1.glarma.hartlepool.coef$site          <- 'Hartlepool'
            results$model3.1.glarma.hartlepool.coef$indicator     <- indicator
            results$model3.1.glarma.hartlepool.coef$sub.indicator <- sub.indicator
        }
        ##################################################
        ## Model 3.1 - Hemel Hempstead                  ##
        ##################################################
        ## print("Hemel Hempstead")
        ts.vector  <- dplyr::filter(df3.1,
                             town        %in% c('Hemel Hempstead', 'Warwick', 'Basingstoke', 'Yeovil')) %>%
                      as.data.frame() %>% .[,'value']
        df3.1$town_ <- relevel(df3.1$town, ref = 'Warwick')
        town <- model.matrix(~df3.1$town_) %>% as.data.frame()
        names(town) <- c('Intercept', 'basingstoke', 'bishop', 'blackburn', 'carlisle', 'grimsby', 'hartlepool', 'hemel', 'newark', 'rochdale', 'rotherham', 'salford', 'salisbury', 'scarborough', 'scunthorpe', 'southport', 'wansbeck', 'whitehaven', 'wigan', 'yeovil')
        regressors <- cbind(df3.1, town) %>%
                      dplyr::filter(town        %in% c('Hemel Hempstead', 'Warwick', 'Basingstoke', 'Yeovil')) %>%
                      dplyr::select(hemel, basingstoke, yeovil, closure, season2, season3, season4, season5, season6, relative.month, other.centre)
        if(town.group$n[town.group$town == 'Hemel Hempstead'] > 0){
            model3.1.glarma.hemel <- glarma(ts = ts.vector,
                                        link = glarma.link,
                                        model = glarma.model,
                                        xreg  = regressors,
                                        distr = glarma.distr)
            results$model3.1.glarma.hemel.coef <- se(model3.1.glarma.hemel)
            results$model3.1.glarma.hemel.coef$site          <- 'Hemel Hempstead'
            results$model3.1.glarma.hemel.coef$indicator     <- indicator
            results$model3.1.glarma.hemel.coef$sub.indicator <- sub.indicator
        }
        ##################################################
        ## Model 3.1 - Newark                           ##
        ##################################################
        ## print("Newark")
        ts.vector  <- dplyr::filter(df3.1,
                             town        %in% c('Newark', 'Southport', 'Carlisle', 'Salisbury')) %>%
                      as.data.frame() %>% .[,'value']
        df3.1$town_ <- relevel(df3.1$town, ref = 'Warwick')
        town <- model.matrix(~df3.1$town_) %>% as.data.frame()
        names(town) <- c('Intercept', 'basingstoke', 'bishop', 'blackburn', 'carlisle', 'grimsby', 'hartlepool', 'hemel', 'newark', 'rochdale', 'rotherham', 'salford', 'salisbury', 'scarborough', 'scunthorpe', 'wansbeck', 'warwick', 'whitehaven', 'wigan', 'yeovil')
        regressors <- cbind(df3.1, town) %>%
                      dplyr::filter(town        %in% c('Newark', 'Southport', 'Carlisle', 'Salisbury')) %>%
                      dplyr::select(newark, carlisle, salisbury, closure, season2, season3, season4, season5, season6, relative.month, other.centre)
        if(town.group$n[town.group$town == 'Newark'] > 0){
            model3.1.glarma.newark <- glarma(y = ts.vector,
                                         X = regressors,
                                         type = 'NegBin')
            results$model3.1.glarma.newark.coef <- se(model3.1.glarma.newark)
            results$model3.1.glarma.newark.coef$site          <- 'Newark'
            results$model3.1.glarma.newark.coef$indicator     <- indicator
            results$model3.1.glarma.newark.coef$sub.indicator <- sub.indicator
        }
        ##################################################
        ## Model 3.1 - Rochdale                         ##
        ##################################################
        ## print("Rochdale")
        ts.vector  <- dplyr::filter(df3.1,
                             town        %in% c('Rochdale', 'Rotherham', 'Scunthorpe', 'Wansbeck')) %>%
                      as.data.frame() %>% .[,'value']
        df3.1$town_ <- relevel(df3.1$town, ref = 'Rotherham')
        town <- model.matrix(~df3.1$town_) %>% as.data.frame()
        names(town) <- c('Intercept', 'basingstoke', 'bishop', 'blackburn', 'carlisle', 'grimsby', 'hartlepool', 'hemel', 'newark', 'rochdale', 'salford', 'salisbury', 'scarborough', 'scunthorpe', 'southport', 'wansbeck', 'warwick', 'whitehaven', 'wigan', 'yeovil')
        regressors <- cbind(df3.1, town) %>%
                      dplyr::filter(town        %in% c('Rochdale', 'Rotherham', 'Scunthorpe', 'Wansbeck')) %>%
                      dplyr::select(rochdale, scunthorpe, wansbeck, closure, season2, season3, season4, season5, season6, relative.month, nhs111, other.centre, ambulance.divert)
        if(town.group$n[town.group$town == 'Rochdale'] > 0){
            model3.1.glarma.rochdale <- glarma(ts = ts.vector,
                                           link = glarma.link,
                                           model = glarma.model,
                                           xreg  = regressors,
                                           distr = glarma.distr)
            results$model3.1.glarma.rochdale.coef <- se(model3.1.glarma.rochdale)
            results$model3.1.glarma.rochdale.coef$site          <- 'Rochdale'
            results$model3.1.glarma.rochdale.coef$indicator     <- indicator
            results$model3.1.glarma.rochdale.coef$sub.indicator <- sub.indicator
        }
        ## Summary table
        results$model3.1.glarma.coefficients <- combine_coefficients(bishop.coef     = results$model3.1.glarma.bishop.coef,
                                                                  hartlepool.coef = results$model3.1.glarma.hartlepool.coef,
                                                                  hemel.coef      = results$model3.1.glarma.hemel.coef,
                                                                  newark.coef     = results$model3.1.glarma.newark.coef,
                                                                  rochdale.coef   = results$model3.1.glarma.rochdale.coef,
                                                                  .indicator      = indicator,
                                                                  .sub.indicator  = sub.indicator)
        ## Tidy the output (matches that generated by closed_models())
        results$model3.1.glarma.coef <- tidy_coefficients(df    = results$model3.1.glarma.coefficients,
                                                       .coef = coefficients,
                                                       bishop.coef     = TRUE,
                                                       hartlepool.coef = TRUE,
                                                       hemel.coef      = TRUE,
                                                       newark.coef     = TRUE,
                                                       rochdale.coef   = TRUE,
                                                       all.coef        = FALSE)
        ## Extract coefficients for plotting
        ## Forest plot
        results$model3.1.forest <- closed_forest(df.list = list(results$model3.1.glarma.coefficients),
                                               plot.term     = c('closure'),
                                               facet.outcome = FALSE,
                                               title         = paste0('Model 3.1 : ',
                                                                      indicator,
                                                                      ' (',
                                                                      sub.indicator,
                                                                      ')'),
                                               theme         = theme_bw())
        ## Return model objects if requested
        if(return.model == TRUE){
            if(exists('model3.1.glarma.bishop')){
                results$model3.1.glarma.bishop     <- model3.1.glarma.bishop
            }
            if(exists('model3.1.glarma.hartlepool')){
                results$model3.1.glarma.hartlepool <- model3.1.glarma.hartlepool
            }
            if(exists('model3.1.glarma.hemel')){
                results$model3.1.glarma.hemel      <- model3.1.glarma.hemel
            }
            if(exists('model3.1.glarma.newark')){
                results$model3.1.glarma.newark     <- model3.1.glarma.newark
            }
            if(exists('model3.1.glarma.rochdale')){
                results$model3.1.glarma.rochdale   <- model3.1.glarma.rochdale
            }
        }
        if(return.df == TRUE){
            results$model3.1.df <- df3.1
        }
        if(return.residuals == TRUE){
            if(exists('model3.1.glarma.bishop')){
                results$model3.1.glarma.residuals.bishop     <- summary(model3.1.glarma.bishop)$residuals
            }
            if(exists('model3.1.glarma.hartlepool')){
                results$model3.1.glarma.residuals.hartlepool <- summary(model3.1.glarma.hartlepool)$residuals
            }
            if(exists('model3.1.glarma.hemel')){
                results$model3.1.glarma.residuals.hemel      <- summary(model3.1.glarma.hemel)$residuals
            }
            if(exists('model3.1.glarma.newark')){
                results$model3.1.glarma.residuals.newark     <- summary(model3.1.glarma.newark)$residuals
            }
            if(exists('model3.1.glarma.rochdale')){
                results$model3.1.glarma.residuals.rochdale   <- summary(model3.1.glarma.rochdale)$residuals
            }
        }
        ## Remove clutter
        rm(df3.1)
    }
    #######################################################################
    ## Model 3.2                                                         ##
    #######################################################################
    if(!is.null(model3.2)){
        ## print("Model 3.2")
        ## Subset data
        df3.2 <- dplyr::filter(df.trust,
                      ## town %in% sites &
                      measure     == indicator &
                      sub.measure == sub.indicator)
        ##################################################
        ## Model 3.2 - Bishop Auckland                  ##
        ##################################################
        ## print("Bishop Auckland")
        ts.vector  <- dplyr::filter(df3.2,
                             town        %in% c('Bishop Auckland', 'Whitehaven', 'Salford', 'Scarborough')) %>%
                      as.data.frame() %>% .[,'value']
        df3.2$town_ <- ifelse(df3.2$town == 'Bishop Auckland', 1, 0)
        df3.2$town_closure <- df3.2$town_ * df3.2$closure
        regressors <- dplyr::filter(df3.2,
                             town        %in% c('Bishop Auckland', 'Whitehaven', 'Salford', 'Scarborough')) %>%
                      dplyr::select(town_, closure, town_closure, season2, season3, season4, season5, season6, relative.month, nhs111)
        ## return(t)
        if(town.group$n[town.group$town == 'Bishop Auckland'] > 0){
            model3.2.glarma.bishop <- glarma(y = ts.vector,
                                         X = regressors,
                                         type = 'NegBin')
            results$model3.2.glarma.bishop.coef <- se(model3.2.glarma.bishop)
            results$model3.2.glarma.bishop.coef$site          <- 'Bishop Auckland'
            results$model3.2.glarma.bishop.coef$indicator     <- indicator
            results$model3.2.glarma.bishop.coef$sub.indicator <- sub.indicator
        }
        ##################################################
        ## Model 3.2 - Hartlepool                       ##
        ##################################################
        ## print("Hartlepool")
        ts.vector  <- dplyr::filter(df3.2,
                             town        %in% c('Hartlepool', 'Grimsby', 'Blackburn', 'Wigan')) %>%
                      as.data.frame() %>% .[,'value']
        df3.2$town_ <- ifelse(df3.2$town == 'Hartlepool', 1, 0)
        df3.2$town_closure <- df3.2$town_ * df3.2$closure
        regressors <- dplyr::filter(df3.2,
                             town        %in% c('Hartlepool', 'Grimsby', 'Blackburn', 'Wigan')) %>%
                      dplyr::select(town_, closure, town_closure, season2, season3, season4, season5, season6, relative.month, nhs111, other.centre)
        if(town.group$n[town.group$town == 'Hartlepool'] > 0){
            model3.2.glarma.hartlepool <- glarma(ts = ts.vector,
                                             link = glarma.link,
                                             model = glarma.model,
                                             xreg  = regressors,
                                             distr = glarma.distr)
            results$model3.2.glarma.hartlepool.coef <- se(model3.2.glarma.hartlepool)
            results$model3.2.glarma.hartlepool.coef$site          <- 'Hartlepool'
            results$model3.2.glarma.hartlepool.coef$indicator     <- indicator
            results$model3.2.glarma.hartlepool.coef$sub.indicator <- sub.indicator
        }
        ##################################################
        ## Model 3.2 - Hemel Hempstead                  ##
        ##################################################
        ## print("Hemel Hempstead")
        ts.vector  <- dplyr::filter(df3.2,
                             town        %in% c('Hemel Hempstead', 'Warwick', 'Basingstoke', 'Yeovil')) %>%
                      as.data.frame() %>% .[,'value']
        df3.2$town_ <- ifelse(df3.2$town == 'Hemel Hempstead', 1, 0)
        df3.2$town_closure <- df3.2$town_ * df3.2$closure
        regressors <- dplyr::filter(df3.2,
                             town        %in% c('Hemel Hempstead', 'Warwick', 'Basingstoke', 'Yeovil')) %>%
                      dplyr::select(town_, closure, town_closure, season2, season3, season4, season5, season6, relative.month, other.centre)
        if(town.group$n[town.group$town == 'Hemel Hempstead'] > 0){
            model3.2.glarma.hemel <- glarma(ts = ts.vector,
                                        link = glarma.link,
                                        model = glarma.model,
                                        xreg  = regressors,
                                        distr = glarma.distr)
            results$model3.2.glarma.hemel.coef <- se(model3.2.glarma.hemel)
            results$model3.2.glarma.hemel.coef$site          <- 'Hemel Hempstead'
            results$model3.2.glarma.hemel.coef$indicator     <- indicator
            results$model3.2.glarma.hemel.coef$sub.indicator <- sub.indicator
        }
        ##################################################
        ## Model 3.2 - Newark                           ##
        ##################################################
        ## print("Newark")
        ts.vector  <- dplyr::filter(df3.2,
                             town        %in% c('Newark', 'Southport', 'Carlisle', 'Salisbury')) %>%
                      as.data.frame() %>% .[,'value']
        df3.2$town_ <- ifelse(df3.2$town == 'Newark', 1, 0)
        df3.2$town_closure <- df3.2$town_ * df3.2$closure
        regressors <- dplyr::filter(df3.2,
                             town        %in% c('Newark', 'Southport', 'Carlisle', 'Salisbury')) %>%
                      dplyr::select(town_, closure, town_closure, season2, season3, season4, season5, season6, relative.month, other.centre)
        if(town.group$n[town.group$town == 'Newark'] > 0){
            model3.2.glarma.newark <- glarma(y = ts.vector,
                                         X = regressors,
                                         type = 'NegBin')
            results$model3.2.glarma.newark.coef <- se(model3.2.glarma.newark)
            results$model3.2.glarma.newark.coef$site          <- 'Newark'
            results$model3.2.glarma.newark.coef$indicator     <- indicator
            results$model3.2.glarma.newark.coef$sub.indicator <- sub.indicator
        }
        ##################################################
        ## Model 3.2 - Rochdale                         ##
        ##################################################
        ## print("Rochdale")
        ts.vector  <- dplyr::filter(df3.2,
                             town        %in% c('Rochdale', 'Rotherham', 'Scunthorpe', 'Wansbeck')) %>%
                      as.data.frame() %>% .[,'value']
        df3.2$town_ <- ifelse(df3.2$town == 'Rochdale', 1, 0)
        df3.2$town_closure <- df3.2$town_ * df3.2$closure
        regressors <- dplyr::filter(df3.2,
                             town        %in% c('Rochdale', 'Rotherham', 'Scunthorpe', 'Wansbeck')) %>%
                      dplyr::select(town_, closure, town_closure, season2, season3, season4, season5, season6, relative.month, nhs111, other.centre, ambulance.divert)
        if(town.group$n[town.group$town == 'Rochdale'] > 0){
            model3.2.glarma.rochdale <- glarma(ts = ts.vector,
                                           link = glarma.link,
                                           model = glarma.model,
                                           xreg  = regressors,
                                           distr = glarma.distr)
            results$model3.2.glarma.rochdale.coef <- se(model3.2.glarma.rochdale)
            results$model3.2.glarma.rochdale.coef$site          <- 'Rochdale'
            results$model3.2.glarma.rochdale.coef$indicator     <- indicator
            results$model3.2.glarma.rochdale.coef$sub.indicator <- sub.indicator
        }
        ## Summary table
        results$model3.2.glarma.coefficients <- combine_coefficients(bishop.coef     = results$model3.2.glarma.bishop.coef,
                                                                  hartlepool.coef = results$model3.2.glarma.hartlepool.coef,
                                                                  hemel.coef      = results$model3.2.glarma.hemel.coef,
                                                                  newark.coef     = results$model3.2.glarma.newark.coef,
                                                                  rochdale.coef   = results$model3.2.glarma.rochdale.coef,
                                                                  .indicator      = indicator,
                                                                  .sub.indicator  = sub.indicator)
        ## Tidy the output (matches that generated by closed_models())
        results$model3.2.glarma.coef <- tidy_coefficients(df    = results$model3.2.glarma.coefficients,
                                                       .coef = coefficients,
                                                       bishop.coef     = TRUE,
                                                       hartlepool.coef = TRUE,
                                                       hemel.coef      = TRUE,
                                                       newark.coef     = TRUE,
                                                       rochdale.coef   = TRUE,
                                                       all.coef        = FALSE)
        ## Extract coefficients for plotting
        ## Forest plot
        results$model3.2.forest <- closed_forest(df.list = list(results$model3.2.glarma.coefficients),
                                               plot.term     = c('closure'),
                                               facet.outcome = FALSE,
                                               title         = paste0('Model 3.2 : ',
                                                                      indicator,
                                                                      ' (',
                                                                      sub.indicator,
                                                                      ')'),
                                               theme         = theme_bw())
        ## Return model objects if requested
        if(return.model == TRUE){
            if(exists('model3.2.glarma.bishop')){
                results$model3.2.glarma.bishop     <- model3.2.glarma.bishop
            }
            if(exists('model3.2.glarma.hartlepool')){
                results$model3.2.glarma.hartlepool <- model3.2.glarma.hartlepool
            }
            if(exists('model3.2.glarma.hemel')){
                results$model3.2.glarma.hemel      <- model3.2.glarma.hemel
            }
            if(exists('model3.2.glarma.newark')){
                results$model3.2.glarma.newark     <- model3.2.glarma.newark
            }
            if(exists('model3.2.glarma.rochdale')){
                results$model3.2.glarma.rochdale   <- model3.2.glarma.rochdale
            }
        }
        if(return.df == TRUE){
            results$model3.2.df <- df3.2
        }
        if(return.residuals == TRUE){
            if(exists('model3.2.glarma.bishop')){
                results$model3.2.glarma.residuals.bishop     <- summary(model3.2.glarma.bishop)$residuals
            }
            if(exists('model3.2.glarma.hartlepool')){
                results$model3.2.glarma.residuals.hartlepool <- summary(model3.2.glarma.hartlepool)$residuals
            }
            if(exists('model3.2.glarma.hemel')){
                results$model3.2.glarma.residuals.hemel      <- summary(model3.2.glarma.hemel)$residuals
            }
            if(exists('model3.2.glarma.newark')){
                results$model3.2.glarma.residuals.newark     <- summary(model3.2.glarma.newark)$residuals
            }
            if(exists('model3.2.glarma.rochdale')){
                results$model3.2.glarma.residuals.rochdale   <- summary(model3.2.glarma.rochdale)$residuals
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
        ## Subset data
        df4 <- dplyr::filter(df.trust,
                      ## town %in% sites &
                      measure     == indicator &
                      sub.measure == sub.indicator)
        ##################################################
        ## Model 4 - All                                ##
        ##################################################
        ## print("Bishop Auckland")
        ts.vector  <- dplyr::filter(df4,
                             town        %in% c('Bishop Auckland', 'Whitehaven', 'Hartlepool', 'Grimsby', 'Hemel Hempstead', 'Warwick', 'Newark', 'Southport', 'Rochdale', 'Rotherham')) %>%
            as.data.frame() %>% .[,'value']
        df4$town_ <- relevel(df4$town, ref = 'Whitehaven')
        town <- model.matrix(~df4$town_) %>% as.data.frame()
        names(town) <- c('Intercept', 'basingstoke', 'bishop', 'blackburn', 'carlisle', 'grimsby', 'hartlepool', 'hemel', 'newark', 'rochdale', 'rotherham', 'salford', 'salisbury', 'scarborough', 'scunthorpe', 'southport', 'wansbeck', 'warwick', 'wigan', 'yeovil')
        regressors <- cbind(df4, town) %>%
                      dplyr::filter(town %in% c('Bishop Auckland', 'Whitehaven', 'Hartlepool', 'Grimsby', 'Hemel Hempstead', 'Warwick', 'Newark', 'Southport', 'Rochdale', 'Rotherham')) %>%
                      dplyr::select(bishop, hartlepool, grimsby, hemel, warwick, newark, southport, rochdale, rotherham, closure, season2, season3, season4, season5, season6, relative.month, nhs111, other.centre, ambulance.divert)
        ## return(t)
        if(town.group$n[town.group$town == 'Bishop Auckland'] > 0){
            model4.glarma.all <- glarma(y = ts.vector,
                                         X = regressors,
                                         type = 'NegBin')
            results$model4.glarma.all.coef <- se(model4.glarma.all)
            results$model4.glarma.all.coef$site          <- 'All'
            results$model4.glarma.all.coef$indicator     <- indicator
            results$model4.glarma.all.coef$sub.indicator <- sub.indicator
        }
        ## Summary table
        results$model4.glarma.coefficients <- combine_coefficients(bishop.coef     = results$model2.glarma.bishop.coef,
                                                                  hartlepool.coef = results$model2.glarma.hartlepool.coef,
                                                                  hemel.coef      = results$model2.glarma.hemel.coef,
                                                                  newark.coef     = results$model2.glarma.newark.coef,
                                                                  rochdale.coef   = results$model2.glarma.rochdale.coef,
                                                                  all.coef        = results$model4.glarma.all.coef,
                                                                  .indicator      = indicator,
                                                                  .sub.indicator  = sub.indicator)
        ## Tidy the output (matches that generated by closed_models())
        results$model4.glarma.coef <- tidy_coefficients(df    = results$model4.glarma.coefficients,
                                                       .coef = coefficients,
                                                       bishop.coef     = TRUE,
                                                       hartlepool.coef = TRUE,
                                                       hemel.coef      = TRUE,
                                                       newark.coef     = TRUE,
                                                       rochdale.coef   = TRUE,
                                                       all.coef        = TRUE)
        ## Extract coefficients for plotting
        ## Forest plot
        results$model4.forest <- closed_forest(df.list = list(results$model4.glarma.coefficients),
                                               plot.term     = c('closure'),
                                               facet.outcome = FALSE,
                                               title         = paste0('Model 4 : ',
                                                                      indicator,
                                                                      ' (',
                                                                      sub.indicator,
                                                                      ')'),
                                               theme         = theme_bw())
        ## Return model objects if requested
        if(return.model == TRUE){
            if(exists('model4.glarma.all')){
                results$model4.glarma.all     <- model4.glarma.all
            }
        }
        if(return.df == TRUE){
            results$model4.df <- df4
        }
        if(return.residuals == TRUE){
            if(exists('model4.glarma.all')){
                results$model4.glarma.residuals.all     <- summary(model4.glarma.all)$residuals
            }
        }
        ## Remove clutter
        rm(df4)
    }
    #######################################################################
    ## Model 5                                                           ##
    #######################################################################
    if(!is.null(model5)){
        ## print("Model 4")
        ## Subset data
        df5 <- dplyr::filter(df.trust,
                      ## town %in% sites &
                      measure     == indicator &
                      sub.measure == sub.indicator)
        ##################################################
        ## Model 5 - All                                ##
        ##################################################
        ## print("Bishop Auckland")
        ts.vector  <- as.data.frame(df5) %>% .[,'value']
        df5$town_ <- relevel(df5$town, ref = 'Whitehaven')
        town <- model.matrix(~df5$town_) %>% as.data.frame()
        names(town) <- c('Intercept', 'basingstoke', 'bishop', 'blackburn', 'carlisle', 'grimsby', 'hartlepool', 'hemel', 'newark', 'rochdale', 'rotherham', 'salford', 'salisbury', 'scarborough', 'scunthorpe', 'southport', 'wansbeck', 'warwick', 'wigan', 'yeovil')
        town <- dplyr::select(town, -Intercept)
        regressors <- dplyr::select(df5, closure, season2, season3, season4, season5, season6, relative.month, nhs111, other.centre, ambulance.divert)
        regressors <- cbind(regressors, town)
        ## return(t)
        if(town.group$n[town.group$town == 'Bishop Auckland'] > 0){
            model5.glarma.all <- glarma(y = ts.vector,
                                         X = regressors,
                                         type = 'NegBin')
            results$model5.glarma.all.coef <- se(model5.glarma.all)
            results$model5.glarma.all.coef$site          <- 'All'
            results$model5.glarma.all.coef$indicator     <- indicator
            results$model5.glarma.all.coef$sub.indicator <- sub.indicator
        }
        ## Summary table
        results$model5.glarma.coefficients <- combine_coefficients(bishop.coef     = results$model3.1.glarma.bishop.coef,
                                                                  hartlepool.coef = results$model3.1.glarma.hartlepool.coef,
                                                                  hemel.coef      = results$model3.1.glarma.hemel.coef,
                                                                  newark.coef     = results$model3.1.glarma.newark.coef,
                                                                  rochdale.coef   = results$model3.1.glarma.rochdale.coef,
                                                                  all.coef        = results$model5.glarma.all.coef,
                                                                  .indicator      = indicator,
                                                                  .sub.indicator  = sub.indicator)
        ## Tidy the output (matches that generated by closed_models())
        results$model5.glarma.coef <- tidy_coefficients(df    = results$model5.glarma.coefficients,
                                                       .coef = coefficients,
                                                       bishop.coef     = TRUE,
                                                       hartlepool.coef = TRUE,
                                                       hemel.coef      = TRUE,
                                                       newark.coef     = TRUE,
                                                       rochdale.coef   = TRUE,
                                                       all.coef        = TRUE)
        ## Extract coefficients for plotting
        ## Forest plot
        results$model5.forest <- closed_forest(df.list = list(results$model5.glarma.coefficients),
                                               plot.term     = c('closure'),
                                               facet.outcome = FALSE,
                                               title         = paste0('Model 5 : ',
                                                                      indicator,
                                                                      ' (',
                                                                      sub.indicator,
                                                                      ')'),
                                               theme         = theme_bw())
        ## Return model objects if requested
        if(return.model == TRUE){
            if(exists('model5.glarma.all')){
                results$model5.glarma.all     <- model5.glarma.all
            }
        }
        if(return.df == TRUE){
            results$model5.df <- df5
        }
        if(return.residuals == TRUE){
            if(exists('model5.glarma.all')){
                results$model5.glarma.residuals.all     <- summary(model5.glarma.all)$residuals
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
        ## Subset data
        sites <- c('Bishop Auckland', 'Hartlepool', 'Hemel Hempstead', 'Newark', 'Rochdale')
        df6.1 <- dplyr::filter(df.lsoa,
                      town %in% sites &
                      measure     == indicator &
                      sub.measure == sub.indicator)
        ##################################################
        ## Model 6.1 - Bishop Auckland                    ##
        ##################################################
        ## print("Bishop Auckland")
        ts.vector  <- dplyr::filter(df6.1,
                             town        == 'Bishop Auckland') %>%
                      as.data.frame() %>% .[,'value']
        regressors <- dplyr::filter(df6.1,
                             town        == 'Bishop Auckland') %>%
                      dplyr::select(closure, season2, season3, season4, season5, season6, relative.month, nhs111)
        ## return(t)
        if(town.group$n[town.group$town == 'Bishop Auckland'] > 0){
            model6.1.glarma.bishop <- glarma(y = ts.vector,
                                         X = regressors,
                                         type = 'NegBin')
            results$model6.1.glarma.bishop.coef <- se(model6.1.glarma.bishop)
            results$model6.1.glarma.bishop.coef$site          <- 'Bishop Auckland'
            results$model6.1.glarma.bishop.coef$indicator     <- indicator
            results$model6.1.glarma.bishop.coef$sub.indicator <- sub.indicator
        }
        ##################################################
        ## Model 6.1 - Hartlepool                         ##
        ##################################################
        ## print("Hartlepool")
        ts.vector  <- dplyr::filter(df6.1,
                             town        == 'Hartlepool') %>%
                      as.data.frame() %>% .[,'value']
        regressors <- dplyr::filter(df6.1,
                             town        == 'Hartlepool') %>%
                      dplyr::select(closure, season2, season3, season4, season5, season6, relative.month, nhs111, other.centre)
        if(town.group$n[town.group$town == 'Hartlepool'] > 0){
            model6.1.glarma.hartlepool <- glarma(ts = ts.vector,
                                             link = glarma.link,
                                             model = glarma.model,
                                             xreg  = regressors,
                                             distr = glarma.distr)
            results$model6.1.glarma.hartlepool.coef <- se(model6.1.glarma.hartlepool)
            results$model6.1.glarma.hartlepool.coef$site          <- 'Hartlepool'
            results$model6.1.glarma.hartlepool.coef$indicator     <- indicator
            results$model6.1.glarma.hartlepool.coef$sub.indicator <- sub.indicator
        }
        ##################################################
        ## Model 6.1 - Hemel Hempstead                    ##
        ##################################################
        ## print("Hemel Hempstead")
        ts.vector  <- dplyr::filter(df6.1,
                             town        == 'Hemel Hempstead') %>%
                      as.data.frame() %>% .[,'value']
        regressors <- dplyr::filter(df6.1,
                             town        == 'Hemel Hempstead') %>%
                      dplyr::select(closure, season2, season3, season4, season5, season6, relative.month, other.centre)
        if(town.group$n[town.group$town == 'Hemel Hempstead'] > 0){
            model6.1.glarma.hemel <- glarma(ts = ts.vector,
                                        link = glarma.link,
                                        model = glarma.model,
                                        xreg  = regressors,
                                        distr = glarma.distr)
            results$model6.1.glarma.hemel.coef <- se(model6.1.glarma.hemel)
            results$model6.1.glarma.hemel.coef$site          <- 'Hemel Hempstead'
            results$model6.1.glarma.hemel.coef$indicator     <- indicator
            results$model6.1.glarma.hemel.coef$sub.indicator <- sub.indicator
        }
        ##################################################
        ## Model 6.1 - Newark                             ##
        ##################################################
        ## print("Newark")
        ts.vector  <- dplyr::filter(df6.1,
                             town        == 'Newark') %>%
                      as.data.frame() %>% .[,'value']
        regressors <- dplyr::filter(df6.1,
                             town        == 'Newark') %>%
                      dplyr::select(closure, season2, season3, season4, season5, season6, relative.month, other.centre)
        if(town.group$n[town.group$town == 'Newark'] > 0){
            model6.1.glarma.newark <- glarma(y = ts.vector,
                                         X = regressors,
                                         type = 'NegBin')
            results$model6.1.glarma.newark.coef <- se(model6.1.glarma.newark)
            results$model6.1.glarma.newark.coef$site          <- 'Newark'
            results$model6.1.glarma.newark.coef$indicator     <- indicator
            results$model6.1.glarma.newark.coef$sub.indicator <- sub.indicator
        }
        ##################################################
        ## Model 6.1 - Rochdale                           ##
        ##################################################
        ## print("Rochdale")
        ts.vector  <- dplyr::filter(df6.1,
                             town        == 'Rochdale') %>%
                      as.data.frame() %>% .[,'value']
        regressors <- dplyr::filter(df6.1,
                             town        == 'Rochdale') %>%
                      dplyr::select(closure, season2, season3, season4, season5, season6, relative.month, nhs111, other.centre, ambulance.divert)
        if(town.group$n[town.group$town == 'Rochdale'] > 0){
            model6.1.glarma.rochdale <- glarma(ts = ts.vector,
                                           link = glarma.link,
                                           model = glarma.model,
                                           xreg  = regressors,
                                           distr = glarma.distr)
            results$model6.1.glarma.rochdale.coef <- se(model6.1.glarma.rochdale)
            results$model6.1.glarma.rochdale.coef$site          <- 'Rochdale'
            results$model6.1.glarma.rochdale.coef$indicator     <- indicator
            results$model6.1.glarma.rochdale.coef$sub.indicator <- sub.indicator
        }
        ## Summary table
        results$model6.1.glarma.coefficients <- combine_coefficients(bishop.coef     = results$model6.1.glarma.bishop.coef,
                                                                  hartlepool.coef = results$model6.1.glarma.hartlepool.coef,
                                                                  hemel.coef      = results$model6.1.glarma.hemel.coef,
                                                                  newark.coef     = results$model6.1.glarma.newark.coef,
                                                                  rochdale.coef   = results$model6.1.glarma.rochdale.coef,
                                                                  .indicator      = indicator,
                                                                  .sub.indicator  = sub.indicator)
        ## Tidy the output (matches that generated by closed_models())
        results$model6.1.glarma.coef <- tidy_coefficients(df    = results$model6.1.glarma.coefficients,
                                                       .coef = coefficients,
                                                       bishop.coef     = TRUE,
                                                       hartlepool.coef = TRUE,
                                                       hemel.coef      = TRUE,
                                                       newark.coef     = TRUE,
                                                       rochdale.coef   = TRUE,
                                                       all.coef        = FALSE)
        ## Extract coefficients for plotting
        ## Forest plot
        results$model6.1.forest <- closed_forest(df.list = list(results$model6.1.glarma.coefficients),
                                               plot.term     = c('closure'),
                                               facet.outcome = FALSE,
                                               title         = paste0('Model 6.1 : ',
                                                                      indicator,
                                                                      ' (',
                                                                      sub.indicator,
                                                                      ')'),
                                               theme         = theme_bw())
        ## Return model objects if requested
        if(return.model == TRUE){
            if(exists('model6.1.glarma.bishop')){
                results$model6.1.glarma.bishop     <- model6.1.glarma.bishop
            }
            if(exists('model6.1.glarma.hartlepool')){
                results$model6.1.glarma.hartlepool <- model6.1.glarma.hartlepool
            }
            if(exists('model6.1.glarma.hemel')){
                results$model6.1.glarma.hemel      <- model6.1.glarma.hemel
            }
            if(exists('model6.1.glarma.newark')){
                results$model6.1.glarma.newark     <- model6.1.glarma.newark
            }
            if(exists('model6.1.glarma.rochdale')){
                results$model6.1.glarma.rochdale   <- model6.1.glarma.rochdale
            }
        }
        if(return.df == TRUE){
            results$model6.1.df <- df6.1
        }
        if(return.residuals == TRUE){
            if(exists('model6.1.glarma.bishop')){
                results$model6.1.glarma.residuals.bishop     <- summary(model6.1.glarma.bishop)$residuals
            }
            if(exists('model6.1.glarma.hartlepool')){
                results$model6.1.glarma.residuals.hartlepool <- summary(model6.1.glarma.hartlepool)$residuals
            }
            if(exists('model6.1.glarma.hemel')){
                results$model6.1.glarma.residuals.hemel      <- summary(model6.1.glarma.hemel)$residuals
            }
            if(exists('model6.1.glarma.newark')){
                results$model6.1.glarma.residuals.newark     <- summary(model6.1.glarma.newark)$residuals
            }
            if(exists('model6.1.glarma.rochdale')){
                results$model6.1.glarma.residuals.rochdale   <- summary(model6.1.glarma.rochdale)$residuals
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
        ## Subset data
        sites <- c('Bishop Auckland', 'Whitehaven',
                   'Hartlepool', 'Grimsby',
                   'Hemel Hempstead', 'Warwick',
                   'Newark', 'Southport',
                   'Rochdale', 'Rotherham')
        df6.2 <- dplyr::filter(df.trust,
                      town %in% sites &
                      measure     == indicator &
                      sub.measure == sub.indicator)
        ##################################################
        ## Model 6.2 - Bishop Auckland                  ##
        ##################################################
        ## print("Bishop Auckland")
        ts.vector  <- dplyr::filter(df6.2,
                             town        %in% c('Bishop Auckland', 'Whitehaven')) %>%
                      as.data.frame() %>% .[,'value']
        df6.2$town_ <- ifelse(df6.2$town == 'Bishop Auckland', 1, 0)
        df6.2$town_closure <- df6.2$town_ * df6.2$closure
        regressors <- dplyr::filter(df6.2,
                             town        %in% c('Bishop Auckland', 'Whitehaven')) %>%
                      dplyr::select(closure, town_, town_closure, season2, season3, season4, season5, season6, relative.month, nhs111)
        ## return(t)
        if(town.group$n[town.group$town == 'Bishop Auckland'] > 0){
            model6.2.glarma.bishop <- glarma(y = ts.vector,
                                         X = regressors,
                                         type = 'NegBin')
            results$model6.2.glarma.bishop.coef <- se(model6.2.glarma.bishop)
            results$model6.2.glarma.bishop.coef$site          <- 'Bishop Auckland'
            results$model6.2.glarma.bishop.coef$indicator     <- indicator
            results$model6.2.glarma.bishop.coef$sub.indicator <- sub.indicator
        }
        ##################################################
        ## Model 6.2 - Hartlepool                       ##
        ##################################################
        ## print("Hartlepool")
        ts.vector  <- dplyr::filter(df6.2,
                             town        %in% c('Hartlepool', 'Grimsby')) %>%
                      as.data.frame() %>% .[,'value']
        df6.2$town_ <- ifelse(df6.2$town == 'Hartlepool', 1, 0)
        df6.2$town_closure <- df6.2$town_ * df6.2$closure
        regressors <- dplyr::filter(df6.2,
                             town        %in% c('Hartlepool', 'Grimsby')) %>%
                      dplyr::select(closure, town_, town_closure, season2, season3, season4, season5, season6, relative.month, nhs111, other.centre)
        if(town.group$n[town.group$town == 'Hartlepool'] > 0){
            model6.2.glarma.hartlepool <- glarma(ts = ts.vector,
                                             link = glarma.link,
                                             model = glarma.model,
                                             xreg  = regressors,
                                             distr = glarma.distr)
            results$model6.2.glarma.hartlepool.coef <- se(model6.2.glarma.hartlepool)
            results$model6.2.glarma.hartlepool.coef$site          <- 'Hartlepool'
            results$model6.2.glarma.hartlepool.coef$indicator     <- indicator
            results$model6.2.glarma.hartlepool.coef$sub.indicator <- sub.indicator
        }
        ##################################################
        ## Model 6.2 - Hemel Hempstead                  ##
        ##################################################
        ## print("Hemel Hempstead")
        ts.vector  <- dplyr::filter(df6.2,
                             town        %in% c('Hemel Hempstead', 'Warwick')) %>%
                      as.data.frame() %>% .[,'value']
        df6.2$town_ <- ifelse(df6.2$town == 'Hemel Hempstead', 1, 0)
        df6.2$town_closure <- df6.2$town_ * df6.2$closure
        regressors <- dplyr::filter(df6.2,
                             town        %in% c('Hemel Hempstead', 'Warwick')) %>%
                      dplyr::select(closure, town_, town_closure, season2, season3, season4, season5, season6, relative.month, other.centre)
        if(town.group$n[town.group$town == 'Hemel Hempstead'] > 0){
            model6.2.glarma.hemel <- glarma(ts = ts.vector,
                                        link = glarma.link,
                                        model = glarma.model,
                                        xreg  = regressors,
                                        distr = glarma.distr)
            results$model6.2.glarma.hemel.coef <- se(model6.2.glarma.hemel)
            results$model6.2.glarma.hemel.coef$site          <- 'Hemel Hempstead'
            results$model6.2.glarma.hemel.coef$indicator     <- indicator
            results$model6.2.glarma.hemel.coef$sub.indicator <- sub.indicator
        }
        ##################################################
        ## Model 6.2 - Newark                           ##
        ##################################################
        ## print("Newark")
        ts.vector  <- dplyr::filter(df6.2,
                             town        %in% c('Newark', 'Southport')) %>%
                      as.data.frame() %>% .[,'value']
        df6.2$town_ <- ifelse(df6.2$town == 'Newark', 1, 0)
        df6.2$town_closure <- df6.2$town_ * df6.2$closure
        regressors <- dplyr::filter(df6.2,
                             town        %in% c('Newark', 'Southport')) %>%
                      dplyr::select(closure, town_, town_closure, season2, season3, season4, season5, season6, relative.month, other.centre)
        if(town.group$n[town.group$town == 'Newark'] > 0){
            model6.2.glarma.newark <- glarma(y = ts.vector,
                                         X = regressors,
                                         type = 'NegBin')
            results$model6.2.glarma.newark.coef <- se(model6.2.glarma.newark)
            results$model6.2.glarma.newark.coef$site          <- 'Newark'
            results$model6.2.glarma.newark.coef$indicator     <- indicator
            results$model6.2.glarma.newark.coef$sub.indicator <- sub.indicator
        }
        ##################################################
        ## Model 6.2 - Rochdale                         ##
        ##################################################
        ## print("Rochdale")
        ts.vector  <- dplyr::filter(df6.2,
                             town        %in% c('Rochdale', 'Rotherham')) %>%
                      as.data.frame() %>% .[,'value']
        df6.2$town_ <- ifelse(df6.2$town == 'Rochdale', 1, 0)
        df6.2$town_closure <- df6.2$town_ * df6.2$closure
        regressors <- dplyr::filter(df6.2,
                             town        %in% c('Rochdale', 'Rotherham')) %>%
                      dplyr::select(closure, town_, town_closure, season2, season3, season4, season5, season6, relative.month, nhs111, other.centre, ambulance.divert)
        if(town.group$n[town.group$town == 'Rochdale'] > 0){
            model6.2.glarma.rochdale <- glarma(ts = ts.vector,
                                           link = glarma.link,
                                           model = glarma.model,
                                           xreg  = regressors,
                                           distr = glarma.distr)
            results$model6.2.glarma.rochdale.coef <- se(model6.2.glarma.rochdale)
            results$model6.2.glarma.rochdale.coef$site          <- 'Rochdale'
            results$model6.2.glarma.rochdale.coef$indicator     <- indicator
            results$model6.2.glarma.rochdale.coef$sub.indicator <- sub.indicator
        }
        ## Summary table
        results$model6.2.glarma.coefficients <- combine_coefficients(bishop.coef     = results$model6.2.glarma.bishop.coef,
                                                                  hartlepool.coef = results$model6.2.glarma.hartlepool.coef,
                                                                  hemel.coef      = results$model6.2.glarma.hemel.coef,
                                                                  newark.coef     = results$model6.2.glarma.newark.coef,
                                                                  rochdale.coef   = results$model6.2.glarma.rochdale.coef,
                                                                  .indicator      = indicator,
                                                                  .sub.indicator  = sub.indicator)
        ## Tidy the output (matches that generated by closed_models())
        results$model6.2.glarma.coef <- tidy_coefficients(df    = results$model6.2.glarma.coefficients,
                                                       .coef = coefficients,
                                                       bishop.coef     = TRUE,
                                                       hartlepool.coef = TRUE,
                                                       hemel.coef      = TRUE,
                                                       newark.coef     = TRUE,
                                                       rochdale.coef   = TRUE,
                                                       all.coef        = FALSE)
        ## Extract coefficients for plotting
        ## Forest plot
        results$model6.2.forest <- closed_forest(df.list = list(results$model6.2.glarma.coefficients),
                                               plot.term     = c('closure'),
                                               facet.outcome = FALSE,
                                               title         = paste0('Model 6.2 : ',
                                                                      indicator,
                                                                      ' (',
                                                                      sub.indicator,
                                                                      ')'),
                                               theme         = theme_bw())
        ## Return model objects if requested
        if(return.model == TRUE){
            if(exists('model6.2.glarma.bishop')){
                results$model6.2.glarma.bishop     <- model6.2.glarma.bishop
            }
            if(exists('model6.2.glarma.hartlepool')){
                results$model6.2.glarma.hartlepool <- model6.2.glarma.hartlepool
            }
            if(exists('model6.2.glarma.hemel')){
                results$model6.2.glarma.hemel      <- model6.2.glarma.hemel
            }
            if(exists('model6.2.glarma.newark')){
                results$model6.2.glarma.newark     <- model6.2.glarma.newark
            }
            if(exists('model6.2.glarma.rochdale')){
                results$model6.2.glarma.rochdale   <- model6.2.glarma.rochdale
            }
        }
        if(return.df == TRUE){
            results$model6.2.df <- df6.2
        }
        if(return.residuals == TRUE){
            if(exists('model6.2.glarma.bishop')){
                results$model6.2.glarma.residuals.bishop     <- summary(model6.2.glarma.bishop)$residuals
            }
            if(exists('model6.2.glarma.hartlepool')){
                results$model6.2.glarma.residuals.hartlepool <- summary(model6.2.glarma.hartlepool)$residuals
            }
            if(exists('model6.2.glarma.hemel')){
                results$model6.2.glarma.residuals.hemel      <- summary(model6.2.glarma.hemel)$residuals
            }
            if(exists('model6.2.glarma.newark')){
                results$model6.2.glarma.residuals.newark     <- summary(model6.2.glarma.newark)$residuals
            }
            if(exists('model6.2.glarma.rochdale')){
                results$model6.2.glarma.residuals.rochdale   <- summary(model6.2.glarma.rochdale)$residuals
            }
        }
        ## Remove clutter
        rm(df6.2)
    }
    #######################################################################
    ## Model 7                                                           ##
    #######################################################################
    if(!is.null(model7)){
        ## print("Model 4")
        ## Subset data
        df7 <- dplyr::filter(df.lsoa,
                      ## town %in% sites &
                      measure     == indicator &
                      sub.measure == sub.indicator)
        ##################################################
        ## Model 7 - All                                ##
        ##################################################
        ## print("Bishop Auckland")
        ts.vector  <- as.data.frame(df7) %>% .[,'value']
        df7$town_ <- relevel(df7$town, ref = 'Whitehaven')
        town <- model.matrix(~df7$town_) %>% as.data.frame()
        names(town) <- c('Intercept', 'basingstoke', 'bishop', 'blackburn', 'carlisle', 'grimsby', 'hartlepool', 'hemel', 'newark', 'rochdale', 'rotherham', 'salford', 'salisbury', 'scarborough', 'scunthorpe', 'southport', 'wansbeck', 'warwick', 'wigan', 'yeovil')
        town <- dplyr::select(town, -Intercept)
        regressors <- dplyr::select(df7, closure, season2, season3, season4, season5, season6, relative.month, nhs111, other.centre, ambulance.divert)
        regressors <- cbind(regressors, town)
        ## return(t)
        if(town.group$n[town.group$town == 'Bishop Auckland'] > 0){
            model7.glarma.all <- glarma(y = ts.vector,
                                         X = regressors,
                                         type = 'NegBin')
            results$model7.glarma.all.coef <- se(model7.glarma.all)
            results$model7.glarma.all.coef$site          <- 'All'
            results$model7.glarma.all.coef$indicator     <- indicator
            results$model7.glarma.all.coef$sub.indicator <- sub.indicator
        }
        ## Summary table
        results$model7.glarma.coefficients <- combine_coefficients(bishop.coef     = results$model3.1.glarma.bishop.coef,
                                                                  hartlepool.coef = results$model3.1.glarma.hartlepool.coef,
                                                                  hemel.coef      = results$model3.1.glarma.hemel.coef,
                                                                  newark.coef     = results$model3.1.glarma.newark.coef,
                                                                  rochdale.coef   = results$model3.1.glarma.rochdale.coef,
                                                                  all.coef        = results$model7.glarma.all.coef,
                                                                  .indicator      = indicator,
                                                                  .sub.indicator  = sub.indicator)
        ## Tidy the output (matches that generated by closed_models())
        results$model7.glarma.coef <- tidy_coefficients(df    = results$model7.glarma.coefficients,
                                                       .coef = coefficients,
                                                       bishop.coef     = TRUE,
                                                       hartlepool.coef = TRUE,
                                                       hemel.coef      = TRUE,
                                                       newark.coef     = TRUE,
                                                       rochdale.coef   = TRUE,
                                                       all.coef        = TRUE)
        ## Extract coefficients for plotting
        ## Forest plot
        results$model7.forest <- closed_forest(df.list = list(results$model7.glarma.coefficients),
                                               plot.term     = c('closure'),
                                               facet.outcome = FALSE,
                                               title         = paste0('Model 7 : ',
                                                                      indicator,
                                                                      ' (',
                                                                      sub.indicator,
                                                                      ')'),
                                               theme         = theme_bw())
        ## Return model objects if requested
        if(return.model == TRUE){
            if(exists('model7.glarma.all')){
                results$model7.glarma.all     <- model7.glarma.all
            }
        }
        if(return.df == TRUE){
            results$model7.df <- df7
        }
        if(return.residuals == TRUE){
            if(exists('model7.glarma.all')){
                results$model7.glarma.residuals.all     <- summary(model7.glarma.all)$residuals
            }
        }
        ## Remove clutter
        rm(df7)
    }

    #######################################################################
    ## Produce summary tables by center using results$summary.table.head ##
    ## and the coefficients from each model                              ##
    #######################################################################
    ## Bind all model results together
    results$model0.glarma.coefficients$model   <- 'Model 0'
    results$model1.glarma.coefficients$model   <- 'Model 1'
    results$model2.glarma.coefficients$model   <- 'Model 2'
    results$model3.1.glarma.coefficients$model <- 'Model 3.1'
    results$model3.2.glarma.coefficients$model <- 'Model 3.2'
    results$model4.glarma.coefficients$model   <- 'Model 4'
    results$model5.glarma.coefficients$model   <- 'Model 5'
    results$model6.1.glarma.coefficients$model <- 'Model 6.1'
    results$model6.2.glarma.coefficients$model <- 'Model 6.2'
    results$model7.glarma.coefficients$model   <- 'Model 7'
    ## Remove extrenuous levels from models 4, 5 and 7 which have their correpsonding
    ## center only variables included
    model4.glarma.coefficients <- dplyr::filter(results$model4.glarma.coefficients, site != 'All')
    model5.glarma.coefficients <- dplyr::filter(results$model5.glarma.coefficients, site != 'All')
    model7.glarma.coefficients <- dplyr::filter(results$model7.glarma.coefficients, site != 'All')
    ## Some model*.coef may not have any data though as the models weren't run
    ## make those NULL so the subsequent rbind() doesn't fail
    if(length(results$model0.coefficients) == 1) results$model0.glarma.coefficients     <- NULL
    if(length(results$model1.coefficients) == 1) results$model1.glarma.coefficients     <- NULL
    if(length(results$model2.coefficients) == 1) results$model2.glarma.coefficients     <- NULL
    if(length(results$model3.1.coefficients) == 1) results$model3.1.glarma.coefficients <- NULL
    if(length(results$model3.2.coefficients) == 1) results$model3.2.glarma.coefficients <- NULL
    if(length(results$model4.coefficients) == 1) model4.glarma.coefficients             <- NULL
    if(length(results$model5.coefficients) == 1) model5.glarma.coefficients             <- NULL
    if(length(results$model6.1.coefficients) == 1) results$model6.1.glarma.coefficients <- NULL
    if(length(results$model6.2.coefficients) == 1) results$model6.2.glarma.coefficients <- NULL
    if(length(results$model7.coefficients) == 1) model7.glarma.coefficients             <- NULL
    ## Return all coefficients across models
    results$all.model.all.coef <- rbind(results$model1.glarma.coefficients,
                                        results$model2.glarma.coefficients,
                                        results$model3.1.glarma.coefficients,
                                        results$model3.2.glarma.coefficients,
                                        model4.glarma.coefficients,
                                        model5.glarma.coefficients,
                                        results$model6.1.glarma.coefficients,
                                        results$model6.2.glarma.coefficients,
                                        model7.glarma.coefficients) %>%
                                  as.data.frame()
    names(results$all.model.all.coef) <- gsub('site', 'town', names(results$all.model.all.coef))
    ## Subset out the closure coefficients and derive output variable/df to append to
    ## table header which contains the means
    results$all.model.closure.coef <- dplyr::filter(results$all.model.all.coef,
                                             term == 'closure')
    names(results$all.model.closure.coef) <- c('est', 'se', 'lower', 'upper', 'term', 'town', 'indicator', 'sub.indicator', 'model')
    ## results$all.model.closure.coef$lci <- results$all.model.closure.coef$est - (1.96 * results$all.model.closure.coef$se)
    ## results$all.model.closure.coef$uci <- results$all.model.closure.coef$est + (1.96 * results$all.model.closure.coef$se)
    results$all.model.closure.coef$estimate <- paste0(formatC(results$all.model.closure.coef$est, digits = digits, format = 'f'),
                                                      ' (',
                                                      formatC(results$all.model.closure.coef$lower, digits = digits, format = 'f'),
                                                      '-',
                                                      formatC(results$all.model.closure.coef$upper, digits = digits, format = 'f'),
                                                      ')')## ,
                                                      ## ') p = ',
                                                      ## formatC(results$all.model.closure.coef$p, digits = digits, format = 'f'))
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
    results$summary.table$town[results$summary.table$Before_median.iqr == 'Model 1']               <- 'Estimated closure coefficients'
    results$summary.table$Before_min.max[results$summary.table$Before_median.iqr == 'Model 1']     <- 'Individual Case Site'
    results$summary.table$After_mean.sd[results$summary.table$Before_median.iqr == 'Model 1']      <- 'No Control'
    results$summary.table$After_median.iqr[results$summary.table$Before_median.iqr == 'Model 1']   <- 'ED Panel'
    results$summary.table$Before_min.max[results$summary.table$Before_median.iqr == 'Model 2']     <- 'Individual Case Site'
    results$summary.table$After_mean.sd[results$summary.table$Before_median.iqr == 'Model 2']      <- 'Primary Control'
    results$summary.table$After_median.iqr[results$summary.table$Before_median.iqr == 'Model 2']   <- 'ED Panel'
    results$summary.table$Before_min.max[results$summary.table$Before_median.iqr == 'Model 3.1']    <- 'Individual Case Site'
    results$summary.table$After_mean.sd[results$summary.table$Before_median.iqr == 'Model 3.1']    <- 'All Control'
    results$summary.table$After_median.iqr[results$summary.table$Before_median.iqr == 'Model 3.1'] <- 'ED Panel'
    results$summary.table$Before_min.max[results$summary.table$Before_median.iqr == 'Model 3.2']    <- 'Individual Case Site'
    results$summary.table$After_mean.sd[results$summary.table$Before_median.iqr == 'Model 3.2']    <- 'All Controls Pooled'
    results$summary.table$After_median.iqr[results$summary.table$Before_median.iqr == 'Model 3.2'] <- 'ED Panel'
    results$summary.table$Before_min.max[results$summary.table$Before_median.iqr == 'Model 4']     <- 'All Case Sites'
    results$summary.table$After_mean.sd[results$summary.table$Before_median.iqr == 'Model 4']      <- 'Primary Control'
    results$summary.table$After_median.iqr[results$summary.table$Before_median.iqr == 'Model 4']   <- 'ED Panel'
    results$summary.table$Before_min.max[results$summary.table$Before_median.iqr == 'Model 5']     <- 'All Case Sites'
    results$summary.table$After_mean.sd[results$summary.table$Before_median.iqr == 'Model 5']      <- 'All Controls'
    results$summary.table$After_median.iqr[results$summary.table$Before_median.iqr == 'Model 5']   <- 'ED Panel'
    results$summary.table$Before_min.max[results$summary.table$Before_median.iqr == 'Model 6.1']    <- 'Individual Case Site'
    results$summary.table$After_mean.sd[results$summary.table$Before_median.iqr == 'Model 6.1']    <- 'None'
    results$summary.table$After_median.iqr[results$summary.table$Before_median.iqr == 'Model 6.1'] <- 'LSOA Panel'
    results$summary.table$Before_min.max[results$summary.table$Before_median.iqr == 'Model 6.2']    <- 'Individual Case Site'
    results$summary.table$After_mean.sd[results$summary.table$Before_median.iqr == 'Model 6.2']    <- 'Primary Control'
    results$summary.table$After_median.iqr[results$summary.table$Before_median.iqr == 'Model 6.2'] <- 'LSOA Panel'
    results$summary.table$Before_min.max[results$summary.table$Before_median.iqr == 'Model 7']     <- 'All Case Sites'
    results$summary.table$After_mean.sd[results$summary.table$Before_median.iqr == 'Model 7']      <- 'All Controls'
    results$summary.table$After_median.iqr[results$summary.table$Before_median.iqr == 'Model 7']   <- 'LSOA Panel'
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
    #######################################################################
    ## Return the results                                                ##
    #######################################################################
    return(results)
}
