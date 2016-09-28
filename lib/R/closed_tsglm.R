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
#' @param steps List of steps (dummy variables) to include in time-series analysis.
#' @param fit.with Which package to fit Prais-Winsten regression with, options are  \code{both} (default) | \code{panelAR} | \code{prais}.
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
#' @param tsglm.link Link to use for \code{tsglm()} (options are \code{log}, the default as it permits negative values and \code{identity} which is constrainged to positive values).
#' @param tsglm.model Model (i.e. lags in this instance) to use in the call to \code{tsglm}.
#' @param tsglm.distr Distribution to use in the call to \code{tsglm} (options are \code{nbinom} the default to allow for over-disperssion and \code{poisson}).
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
closed_tsglm <- function(df.lsoa          = ed_attendances_by_mode_measure,
                         df.trust         = ed_attendances_by_mode_site_measure_clean,
                         indicator        = 'ed attendances',
                         sub.indicator    = 'any',
                         steps            = c('closure'),
                         fit.with         = 'both',
                         panel.lsoa       = 'lsoa',
                         panel.trust      = 'town',
                         timevar          = 'relative.month',
                         outcome          = 'value',
                         model0           = c('closure'),
                         model1           = c('closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
                         model2           = c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
                         model3.1         = c('pooled.control * closure', 'town', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
                         model3.2         = c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
                         model4           = c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
                         model5           = c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
                         model6.1         = c('season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert', 'diff.time.to.ed'),
                         model6.2         = c('town', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert', 'diff.time.to.ed'),
                         model7           = c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert', 'diff.time.to.ed'),
                         tsglm.link       = 'log',
                         tsglm.model      = list(past_obs = 1),
                         tsglm.distr      = 'nbinom',
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
    ## Convert to data frame, selecting only the specified outcome and expand season
    ## to a series of dummy variables for inclusion
    ## print("Debug 3")
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
                                                town, before.after, mean.sd, median.iqr, min.max)
    ## Reshape the table header
    results$summary.table.head <- melt(results$summary.table.head, id.vars = c('town', 'before.after')) %>%
                                  dcast(town ~ before.after + variable)
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
                                                After_mean.sd, After_median.iqr, After_min.max)
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
    combine_coefficients <- function(bishop.coef     = results$model0.tsglm.bishop.coef,
                                     hartlepool.coef = results$model0.tsglm.hartlepool.coef,
                                     hemel.coef      = results$model0.tsglm.hemel.coef,
                                     newark.coef     = results$model0.tsglm.newark.coef,
                                     rochdale.coef   = results$model0.tsglm.rochdale.coef,
                                     .indicator      = indicator,
                                     .sub.indicator  = sub.indicator){
        ## List of results
        coef <- list()
        ## Combine coefficients, SE and CIs and derive tidy df for output
        bishop <- cbind(bishop.coef$est,
                        bishop.coef$se,
                        bishop.coef$ci) %>%
                  data.frame()
        bishop$coefficient <- rownames(bishop)
        bishop$town <- 'Bishop Auckland'
        hartlepool <- cbind(hartlepool.coef$est,
                            hartlepool.coef$se,
                            hartlepool.coef$ci) %>%
                      data.frame()
        hartlepool$coefficient <- rownames(hartlepool)
        hartlepool$town <- 'Hartlepool'
        hemel <- cbind(hemel.coef$est,
                       hemel.coef$se,
                       hemel.coef$ci) %>%
                  data.frame()
        hemel$coefficient <- rownames(hemel)
        hemel$town <- 'Hemel Hempstead'
        newark <- cbind(newark.coef$est,
                        newark.coef$se,
                        newark.coef$ci) %>%
                  data.frame()
        newark$coefficient <- rownames(newark)
        newark$town <- 'Newark'
        rochdale <- cbind(rochdale.coef$est,
                          rochdale.coef$se,
                          rochdale.coef$ci) %>%
                  data.frame()
        rochdale$coefficient <- rownames(rochdale)
        rochdale$town <- 'Rochdale'
        .coefficients <- rbind(bishop,
                               hartlepool,
                               hemel,
                               newark,
                               rochdale)
        names(.coefficients)        <- c('est', 'se', 'lower', 'upper', 'coefficient', 'town')
        rownames(.coefficients) <- NULL
        .coefficients$indicator     <- .indicator
        .coefficients$sub.indicator <- .sub.indicator
        return(.coefficients)
    }
    #######################################################################
    ## Model 0                                                           ##
    #######################################################################
    if(!is.null(model0)){
        ## print("Model 0")
        ## Subset data
        sites <- c('Bishop Auckland', 'Hartlepool', 'Hemel Hempstead', 'Newark', 'Rochdale')
        df0 <- filter(df.trust,
                      town %in% sites &
                      measure     == indicator &
                      sub.measure == sub.indicator)
        ##################################################
        ## Model 0 - Bishop Auckland                    ##
        ##################################################
        ## print("Bishop Auckland")
        ts.vector  <- filter(df0,
                             town        == 'Bishop Auckland') %>%
                      as.data.frame() %>% .[,'value']
        regressors <- filter(df0,
                             town        == 'Bishop Auckland') %>%
                      dplyr::select(closure)
        ## return(t)
        if(town.group$n[town.group$town == 'Bishop Auckland'] > 0){
            model0.tsglm.bishop <- tsglm(ts = ts.vector,
                                         link = tsglm.link,
                                         model = tsglm.model,
                                         xreg  = regressors,
                                         distr = tsglm.distr)
            results$model0.tsglm.bishop.coef <- se(model0.tsglm.bishop)
            results$model0.tsglm.rochdale.coef$site          <- 'Bishop Auckland'
            results$model0.tsglm.rochdale.coef$indicator     <- indicator
            results$model0.tsglm.rochdale.coef$sub.indicator <- sub.indicator
        }
        ##################################################
        ## Model 0 - Hartlepool                         ##
        ##################################################
        ## print("Hartlepool")
        ts.vector  <- filter(df0,
                             town        == 'Hartlepool') %>%
                      as.data.frame() %>% .[,'value']
        regressors <- filter(df0,
                             town        == 'Hartlepool') %>%
                      dplyr::select(closure)
        if(town.group$n[town.group$town == 'Hartlepool'] > 0){
            model0.tsglm.hartlepool <- tsglm(ts = ts.vector,
                                             link = tsglm.link,
                                             model = tsglm.model,
                                             xreg  = regressors,
                                             distr = tsglm.distr)
            results$model0.tsglm.hartlepool.coef <- se(model0.tsglm.hartlepool)
            results$model0.tsglm.rochdale.coef$site          <- 'Hartlepool'
            results$model0.tsglm.rochdale.coef$indicator     <- indicator
            results$model0.tsglm.rochdale.coef$sub.indicator <- sub.indicator
        }
        ##################################################
        ## Model 0 - Hemel Hempstead                    ##
        ##################################################
        ## print("Hemel Hempstead")
        ts.vector  <- filter(df0,
                             town        == 'Hemel Hempstead') %>%
                      as.data.frame() %>% .[,'value']
        regressors <- filter(df0,
                             town        == 'Hemel Hempstead') %>%
                      dplyr::select(closure)
        if(town.group$n[town.group$town == 'Hemel Hempstead'] > 0){
            model0.tsglm.hemel <- tsglm(ts = ts.vector,
                                        link = tsglm.link,
                                        model = tsglm.model,
                                        xreg  = regressors,
                                        distr = tsglm.distr)
            results$model0.tsglm.hemel.coef <- se(model0.tsglm.hemel)
            results$model0.tsglm.rochdale.coef$site          <- 'Hemel Hempstead'
            results$model0.tsglm.rochdale.coef$indicator     <- indicator
            results$model0.tsglm.rochdale.coef$sub.indicator <- sub.indicator
        }
        ##################################################
        ## Model 0 - Newark                             ##
        ##################################################
        ## print("Newark")
        ts.vector  <- filter(df0,
                             town        == 'Newark') %>%
                      as.data.frame() %>% .[,'value']
        regressors <- filter(df0,
                             town        == 'Newark') %>%
                      dplyr::select(closure)
        if(town.group$n[town.group$town == 'Newark'] > 0){
            model0.tsglm.newark <- tsglm(ts = ts.vector,
                                         link = tsglm.link,
                                         model = tsglm.model,
                                         xreg  = regressors,
                                         distr = tsglm.distr)
            results$model0.tsglm.newark.coef <- se(model0.tsglm.newark)
            results$model0.tsglm.rochdale.coef$site          <- 'Newark'
            results$model0.tsglm.rochdale.coef$indicator     <- indicator
            results$model0.tsglm.rochdale.coef$sub.indicator <- sub.indicator
        }
        ##################################################
        ## Model 0 - Rochdale                           ##
        ##################################################
        ## print("Rochdale")
        ts.vector  <- filter(df0,
                             town        == 'Rochdale') %>%
                      as.data.frame() %>% .[,'value']
        regressors <- filter(df0,
                             town        == 'Rochdale') %>%
                      dplyr::select(closure)
        if(town.group$n[town.group$town == 'Rochdale'] > 0){
            model0.tsglm.rochdale <- tsglm(ts = ts.vector,
                                           link = tsglm.link,
                                           model = tsglm.model,
                                           xreg  = regressors,
                                           distr = tsglm.distr)
            results$model0.tsglm.rochdale.coef <- se(model0.tsglm.rochdale)
            results$model0.tsglm.rochdale.coef$site          <- 'Rochdale'
            results$model0.tsglm.rochdale.coef$indicator     <- indicator
            results$model0.tsglm.rochdale.coef$sub.indicator <- sub.indicator
        }
        ## Summary table
        results$model0.tsglm.coefficients <- combine_coefficients(bishop.coef     = results$model0.tsglm.bishop.coef,
                                                                  hartlepool.coef = results$model0.tsglm.hartlepool.coef,
                                                                  hemel.coef      = results$model0.tsglm.hemel.coef,
                                                                  newark.coef     = results$model0.tsglm.newark.coef,
                                                                  rochdale.coef   = results$model0.tsglm.rochdale.coef,
                                                                  .indicator      = indicator,
                                                                  .sub.indicator  = sub.indicator)
        ## Extract coefficients for plotting
        ## ## Forest plot
        results$model0.forest <- closed_forest(df.list = list(results$model0.tsglm.coefficients),
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
            if(exists('model0.tsglm.bishop')){
                results$model0.tsglm.bishop     <- model0.tsglm.bishop
            }
            if(exists('model0.tsglm.hartlepool')){
                results$model0.tsglm.hartlepool <- model0.tsglm.hartlepool
            }
            if(exists('model0.tsglm.hemel')){
                results$model0.tsglm.hemel      <- model0.tsglm.hemel
            }
            if(exists('model0.tsglm.newark')){
                results$model0.tsglm.newark     <- model0.tsglm.newark
            }
            if(exists('model0.tsglm.rochdale')){
                results$model0.tsglm.rochdale   <- model0.tsglm.rochdale
            }
        }
        if(return.df == TRUE){
            results$model0.df <- df0
        }
        if(return.residuals == TRUE){
            if(exists('model0.tsglm.bishop')){
                results$model0.tsglm.residuals.bishop     <- summary(model0.tsglm.bishop)$residuals
            }
            if(exists('model0.tsglm.hartlepool')){
                results$model0.tsglm.residuals.hartlepool <- summary(model0.tsglm.hartlepool)$residuals
            }
            if(exists('model0.tsglm.hemel')){
                results$model0.tsglm.residuals.hemel      <- summary(model0.tsglm.hemel)$residuals
            }
            if(exists('model0.tsglm.newark')){
                results$model0.tsglm.residuals.newark     <- summary(model0.tsglm.newark)$residuals
            }
            if(exists('model0.tsglm.rochdale')){
                results$model0.tsglm.residuals.rochdale   <- summary(model0.tsglm.rochdale)$residuals
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
        df1 <- filter(df.trust,
                      town %in% sites &
                      measure     == indicator &
                      sub.measure == sub.indicator)
        ##################################################
        ## Model 1 - Bishop Auckland                    ##
        ##################################################
        ## print("Bishop Auckland")
        ts.vector  <- filter(df1,
                             town        == 'Bishop Auckland') %>%
                      as.data.frame() %>% .[,'value']
        regressors <- filter(df1,
                             town        == 'Bishop Auckland') %>%
                      dplyr::select(closure, season2, season3, season4, season5, season6, relative.month, nhs111)
        ## return(t)
        if(town.group$n[town.group$town == 'Bishop Auckland'] > 0){
            model1.tsglm.bishop <- tsglm(ts = ts.vector,
                                         link = tsglm.link,
                                         model = tsglm.model,
                                         xreg  = regressors,
                                         distr = tsglm.distr)
            results$model1.tsglm.bishop.coef <- se(model1.tsglm.bishop)
            results$model1.tsglm.rochdale.coef$site          <- 'Bishop Auckland'
            results$model1.tsglm.rochdale.coef$indicator     <- indicator
            results$model1.tsglm.rochdale.coef$sub.indicator <- sub.indicator
        }
        ##################################################
        ## Model 1 - Hartlepool                         ##
        ##################################################
        ## print("Hartlepool")
        ts.vector  <- filter(df1,
                             town        == 'Hartlepool') %>%
                      as.data.frame() %>% .[,'value']
        regressors <- filter(df1,
                             town        == 'Hartlepool') %>%
                      dplyr::select(closure, season2, season3, season4, season5, season6, relative.month, nhs111, other.centre)
        if(town.group$n[town.group$town == 'Hartlepool'] > 0){
            model1.tsglm.hartlepool <- tsglm(ts = ts.vector,
                                             link = tsglm.link,
                                             model = tsglm.model,
                                             xreg  = regressors,
                                             distr = tsglm.distr)
            results$model1.tsglm.hartlepool.coef <- se(model1.tsglm.hartlepool)
            results$model1.tsglm.rochdale.coef$site          <- 'Hartlepool'
            results$model1.tsglm.rochdale.coef$indicator     <- indicator
            results$model1.tsglm.rochdale.coef$sub.indicator <- sub.indicator
        }
        ##################################################
        ## Model 1 - Hemel Hempstead                    ##
        ##################################################
        ## print("Hemel Hempstead")
        ts.vector  <- filter(df1,
                             town        == 'Hemel Hempstead') %>%
                      as.data.frame() %>% .[,'value']
        regressors <- filter(df1,
                             town        == 'Hemel Hempstead') %>%
                      dplyr::select(closure, season2, season3, season4, season5, season6, relative.month, other.centre)
        if(town.group$n[town.group$town == 'Hemel Hempstead'] > 0){
            model1.tsglm.hemel <- tsglm(ts = ts.vector,
                                        link = tsglm.link,
                                        model = tsglm.model,
                                        xreg  = regressors,
                                        distr = tsglm.distr)
            results$model1.tsglm.hemel.coef <- se(model1.tsglm.hemel)
            results$model1.tsglm.rochdale.coef$site          <- 'Hemel Hempstead'
            results$model1.tsglm.rochdale.coef$indicator     <- indicator
            results$model1.tsglm.rochdale.coef$sub.indicator <- sub.indicator
        }
        ##################################################
        ## Model 1 - Newark                             ##
        ##################################################
        ## print("Newark")
        ts.vector  <- filter(df1,
                             town        == 'Newark') %>%
                      as.data.frame() %>% .[,'value']
        regressors <- filter(df1,
                             town        == 'Newark') %>%
                      dplyr::select(closure, season2, season3, season4, season5, season6, relative.month, other.centre)
        if(town.group$n[town.group$town == 'Newark'] > 0){
            model1.tsglm.newark <- tsglm(ts = ts.vector,
                                         link = tsglm.link,
                                         model = tsglm.model,
                                         xreg  = regressors,
                                         distr = tsglm.distr)
            results$model1.tsglm.newark.coef <- se(model1.tsglm.newark)
            results$model1.tsglm.rochdale.coef$site          <- 'Newark'
            results$model1.tsglm.rochdale.coef$indicator     <- indicator
            results$model1.tsglm.rochdale.coef$sub.indicator <- sub.indicator
        }
        ##################################################
        ## Model 1 - Rochdale                           ##
        ##################################################
        ## print("Rochdale")
        ts.vector  <- filter(df1,
                             town        == 'Rochdale') %>%
                      as.data.frame() %>% .[,'value']
        regressors <- filter(df1,
                             town        == 'Rochdale') %>%
                      dplyr::select(closure, season2, season3, season4, season5, season6, relative.month, nhs111, other.centre, ambulance.divert)
        if(town.group$n[town.group$town == 'Rochdale'] > 0){
            model1.tsglm.rochdale <- tsglm(ts = ts.vector,
                                           link = tsglm.link,
                                           model = tsglm.model,
                                           xreg  = regressors,
                                           distr = tsglm.distr)
            results$model1.tsglm.rochdale.coef <- se(model1.tsglm.rochdale)
            results$model1.tsglm.rochdale.coef$site          <- 'Rochdale'
            results$model1.tsglm.rochdale.coef$indicator     <- indicator
            results$model1.tsglm.rochdale.coef$sub.indicator <- sub.indicator
        }
        ## Summary table
        results$model1.tsglm.coefficients <- combine_coefficients(bishop.coef     = results$model1.tsglm.bishop.coef,
                                                                  hartlepool.coef = results$model1.tsglm.hartlepool.coef,
                                                                  hemel.coef      = results$model1.tsglm.hemel.coef,
                                                                  newark.coef     = results$model1.tsglm.newark.coef,
                                                                  rochdale.coef   = results$model1.tsglm.rochdale.coef,
                                                                  .indicator      = indicator,
                                                                  .sub.indicator  = sub.indicator)
        ## Extract coefficients for plotting
        ## ## Forest plot
        results$model1.forest <- closed_forest(df.list = list(results$model1.tsglm.coefficients),
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
            if(exists('model1.tsglm.bishop')){
                results$model1.tsglm.bishop     <- model1.tsglm.bishop
            }
            if(exists('model1.tsglm.hartlepool')){
                results$model1.tsglm.hartlepool <- model1.tsglm.hartlepool
            }
            if(exists('model1.tsglm.hemel')){
                results$model1.tsglm.hemel      <- model1.tsglm.hemel
            }
            if(exists('model1.tsglm.newark')){
                results$model1.tsglm.newark     <- model1.tsglm.newark
            }
            if(exists('model1.tsglm.rochdale')){
                results$model1.tsglm.rochdale   <- model1.tsglm.rochdale
            }
        }
        if(return.df == TRUE){
            results$model1.df <- df1
        }
        if(return.residuals == TRUE){
            if(exists('model1.tsglm.bishop')){
                results$model1.tsglm.residuals.bishop     <- summary(model1.tsglm.bishop)$residuals
            }
            if(exists('model1.tsglm.hartlepool')){
                results$model1.tsglm.residuals.hartlepool <- summary(model1.tsglm.hartlepool)$residuals
            }
            if(exists('model1.tsglm.hemel')){
                results$model1.tsglm.residuals.hemel      <- summary(model1.tsglm.hemel)$residuals
            }
            if(exists('model1.tsglm.newark')){
                results$model1.tsglm.residuals.newark     <- summary(model1.tsglm.newark)$residuals
            }
            if(exists('model1.tsglm.rochdale')){
                results$model1.tsglm.residuals.rochdale   <- summary(model1.tsglm.rochdale)$residuals
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
        df2 <- filter(df.trust,
                      town %in% sites &
                      measure     == indicator &
                      sub.measure == sub.indicator)
        ##################################################
        ## Model 2 - Bishop Auckland                    ##
        ##################################################
        ## print("Bishop Auckland")
        ts.vector  <- filter(df2,
                             town        %in% c('Bishop Auckland', 'Whitehaven')) %>%
                      as.data.frame() %>% .[,'value']
        df2$town_ <- ifelse(df2$town == 'Bishop Auckland', 1, 0)
        df2$town_closure <- df2$town_ * df2$closure
        regressors <- filter(df2,
                             town        %in% c('Bishop Auckland', 'Whitehaven')) %>%
                      dplyr::select(closure, town_, town_closure, season2, season3, season4, season5, season6, relative.month, nhs111)
        ## return(t)
        if(town.group$n[town.group$town == 'Bishop Auckland'] > 0){
            model2.tsglm.bishop <- tsglm(ts = ts.vector,
                                         link = tsglm.link,
                                         model = tsglm.model,
                                         xreg  = regressors,
                                         distr = tsglm.distr)
            results$model2.tsglm.bishop.coef <- se(model2.tsglm.bishop)
            results$model2.tsglm.rochdale.coef$site          <- 'Bishop Auckland'
            results$model2.tsglm.rochdale.coef$indicator     <- indicator
            results$model2.tsglm.rochdale.coef$sub.indicator <- sub.indicator
        }
        ##################################################
        ## Model 2 - Hartlepool                         ##
        ##################################################
        ## print("Hartlepool")
        ts.vector  <- filter(df2,
                             town        %in% c('Hartlepool', 'Grimsby')) %>%
                      as.data.frame() %>% .[,'value']
        df2$town_ <- ifelse(df2$town == 'Hartlepool', 1, 0)
        df2$town_closure <- df2$town_ * df2$closure
        regressors <- filter(df2,
                             town        %in% c('Hartlepool', 'Grimsby')) %>%
                      dplyr::select(closure, town_, town_closure, season2, season3, season4, season5, season6, relative.month, nhs111, other.centre)
        if(town.group$n[town.group$town == 'Hartlepool'] > 0){
            model2.tsglm.hartlepool <- tsglm(ts = ts.vector,
                                             link = tsglm.link,
                                             model = tsglm.model,
                                             xreg  = regressors,
                                             distr = tsglm.distr)
            results$model2.tsglm.hartlepool.coef <- se(model2.tsglm.hartlepool)
            results$model2.tsglm.rochdale.coef$site          <- 'Hartlepool'
            results$model2.tsglm.rochdale.coef$indicator     <- indicator
            results$model2.tsglm.rochdale.coef$sub.indicator <- sub.indicator
        }
        ##################################################
        ## Model 2 - Hemel Hempstead                    ##
        ##################################################
        ## print("Hemel Hempstead")
        ts.vector  <- filter(df2,
                             town        %in% c('Hemel Hempstead', 'Warwick')) %>%
                      as.data.frame() %>% .[,'value']
        df2$town_ <- ifelse(df2$town == 'Hemel Hempstead', 1, 0)
        df2$town_closure <- df2$town_ * df2$closure
        regressors <- filter(df2,
                             town        %in% c('Hemel Hempstead', 'Warwick')) %>%
                      dplyr::select(closure, town_, town_closure, season2, season3, season4, season5, season6, relative.month, other.centre)
        if(town.group$n[town.group$town == 'Hemel Hempstead'] > 0){
            model2.tsglm.hemel <- tsglm(ts = ts.vector,
                                        link = tsglm.link,
                                        model = tsglm.model,
                                        xreg  = regressors,
                                        distr = tsglm.distr)
            results$model2.tsglm.hemel.coef <- se(model2.tsglm.hemel)
            results$model2.tsglm.rochdale.coef$site          <- 'Hemel Hempstead'
            results$model2.tsglm.rochdale.coef$indicator     <- indicator
            results$model2.tsglm.rochdale.coef$sub.indicator <- sub.indicator
        }
        ##################################################
        ## Model 2 - Newark                             ##
        ##################################################
        ## print("Newark")
        ts.vector  <- filter(df2,
                             town        %in% c('Newark', 'Southport')) %>%
                      as.data.frame() %>% .[,'value']
        df2$town_ <- ifelse(df2$town == 'Newark', 1, 0)
        df2$town_closure <- df2$town_ * df2$closure
        regressors <- filter(df2,
                             town        %in% c('Newark', 'Southport')) %>%
                      dplyr::select(closure, town_, town_closure, season2, season3, season4, season5, season6, relative.month, other.centre)
        if(town.group$n[town.group$town == 'Newark'] > 0){
            model2.tsglm.newark <- tsglm(ts = ts.vector,
                                         link = tsglm.link,
                                         model = tsglm.model,
                                         xreg  = regressors,
                                         distr = tsglm.distr)
            results$model2.tsglm.newark.coef <- se(model2.tsglm.newark)
            results$model2.tsglm.rochdale.coef$site          <- 'Newark'
            results$model2.tsglm.rochdale.coef$indicator     <- indicator
            results$model2.tsglm.rochdale.coef$sub.indicator <- sub.indicator
        }
        ##################################################
        ## Model 2 - Rochdale                           ##
        ##################################################
        ## print("Rochdale")
        ts.vector  <- filter(df2,
                             town        %in% c('Rochdale', 'Rotherham')) %>%
                      as.data.frame() %>% .[,'value']
        df2$town_ <- ifelse(df2$town == 'Rochdale', 1, 0)
        df2$town_closure <- df2$town_ * df2$closure
        regressors <- filter(df2,
                             town        %in% c('Rochdale', 'Rotherham')) %>%
                      dplyr::select(closure, town_, town_closure, season2, season3, season4, season5, season6, relative.month, nhs111, other.centre, ambulance.divert)
        if(town.group$n[town.group$town == 'Rochdale'] > 0){
            model2.tsglm.rochdale <- tsglm(ts = ts.vector,
                                           link = tsglm.link,
                                           model = tsglm.model,
                                           xreg  = regressors,
                                           distr = tsglm.distr)
            results$model2.tsglm.rochdale.coef <- se(model2.tsglm.rochdale)
            results$model2.tsglm.rochdale.coef$site          <- 'Rochdale'
            results$model2.tsglm.rochdale.coef$indicator     <- indicator
            results$model2.tsglm.rochdale.coef$sub.indicator <- sub.indicator
        }
        ## Summary table
        results$model2.tsglm.coefficients <- combine_coefficients(bishop.coef     = results$model2.tsglm.bishop.coef,
                                                                  hartlepool.coef = results$model2.tsglm.hartlepool.coef,
                                                                  hemel.coef      = results$model2.tsglm.hemel.coef,
                                                                  newark.coef     = results$model2.tsglm.newark.coef,
                                                                  rochdale.coef   = results$model2.tsglm.rochdale.coef,
                                                                  .indicator      = indicator,
                                                                  .sub.indicator  = sub.indicator)
        ## Extract coefficients for plotting
        ## ## Forest plot
        results$model2.forest <- closed_forest(df.list = list(results$model2.tsglm.coefficients),
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
            if(exists('model2.tsglm.bishop')){
                results$model2.tsglm.bishop     <- model2.tsglm.bishop
            }
            if(exists('model2.tsglm.hartlepool')){
                results$model2.tsglm.hartlepool <- model2.tsglm.hartlepool
            }
            if(exists('model2.tsglm.hemel')){
                results$model2.tsglm.hemel      <- model2.tsglm.hemel
            }
            if(exists('model2.tsglm.newark')){
                results$model2.tsglm.newark     <- model2.tsglm.newark
            }
            if(exists('model2.tsglm.rochdale')){
                results$model2.tsglm.rochdale   <- model2.tsglm.rochdale
            }
        }
        if(return.df == TRUE){
            results$model2.df <- df2
        }
        if(return.residuals == TRUE){
            if(exists('model2.tsglm.bishop')){
                results$model2.tsglm.residuals.bishop     <- summary(model2.tsglm.bishop)$residuals
            }
            if(exists('model2.tsglm.hartlepool')){
                results$model2.tsglm.residuals.hartlepool <- summary(model2.tsglm.hartlepool)$residuals
            }
            if(exists('model2.tsglm.hemel')){
                results$model2.tsglm.residuals.hemel      <- summary(model2.tsglm.hemel)$residuals
            }
            if(exists('model2.tsglm.newark')){
                results$model2.tsglm.residuals.newark     <- summary(model2.tsglm.newark)$residuals
            }
            if(exists('model2.tsglm.rochdale')){
                results$model2.tsglm.residuals.rochdale   <- summary(model2.tsglm.rochdale)$residuals
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
        df3.1 <- filter(df.trust,
                      ## town %in% sites &
                      measure     == indicator &
                      sub.measure == sub.indicator)
        ##################################################
        ## Model 3.1 - Bishop Auckland                  ##
        ##################################################
        ## print("Bishop Auckland")
        ts.vector  <- filter(df3.1,
                             town        %in% c('Bishop Auckland', 'Whitehaven', 'Salford', 'Scarborough')) %>%
                      as.data.frame() %>% .[,'value']
        df3.1$town_ <- relevel(df3.1$town, ref = 'Whitehaven')
        town <- model.matrix(~df3.1$town_) %>% as.data.frame()
        names(town) <- c('Intercept', 'basingstoke', 'bishop', 'blackburn', 'carlisle', 'grimsby', 'hartlepool', 'hemel', 'newark', 'rochdale', 'rotherham', 'salford', 'salisbury', 'scarborough', 'scunthorpe', 'southport', 'wansbeck', 'warwick', 'wigan', 'yeovil')
        regressors <- cbind(df3.1, town) %>%
                      filter(town        %in% c('Bishop Auckland', 'Whitehaven', 'Salford', 'Scarborough')) %>%
                      dplyr::select(bishop, salford, scarborough, closure, season2, season3, season4, season5, season6, relative.month, nhs111)
        ## return(t)
        if(town.group$n[town.group$town == 'Bishop Auckland'] > 0){
            model3.1.tsglm.bishop <- tsglm(ts = ts.vector,
                                         link = tsglm.link,
                                         model = tsglm.model,
                                         xreg  = regressors,
                                         distr = tsglm.distr)
            results$model3.1.tsglm.bishop.coef <- se(model3.1.tsglm.bishop)
            results$model3.1.tsglm.rochdale.coef$site          <- 'Bishop Auckland'
            results$model3.1.tsglm.rochdale.coef$indicator     <- indicator
            results$model3.1.tsglm.rochdale.coef$sub.indicator <- sub.indicator
        }
        ##################################################
        ## Model 3.1 - Hartlepool                       ##
        ##################################################
        ## print("Hartlepool")
        ts.vector  <- filter(df3.1,
                             town        %in% c('Hartlepool', 'Grimsby', 'Blackburn', 'Wigan')) %>%
                      as.data.frame() %>% .[,'value']
        df3.1$town_ <- relevel(df3.1$town, ref = 'Grimsby')
        town <- model.matrix(~df3.1$town_) %>% as.data.frame()
        names(town) <- c('Intercept', 'basingstoke', 'bishop', 'blackburn', 'carlisle', 'hartlepool', 'hemel', 'newark', 'rochdale', 'rotherham', 'salford', 'salisbury', 'scarborough', 'scunthorpe', 'southport', 'wansbeck', 'warwick', 'whitehaven', 'wigan', 'yeovil')
        regressors <- cbind(df3.1, town) %>%
                      filter(town        %in% c('Hartlepool', 'Grimsby', 'Blackburn', 'Wigan')) %>%
                      dplyr::select(hartlepool, blackburn, wigan, closure, season2, season3, season4, season5, season6, relative.month, nhs111, other.centre)
        if(town.group$n[town.group$town == 'Hartlepool'] > 0){
            model3.1.tsglm.hartlepool <- tsglm(ts = ts.vector,
                                             link = tsglm.link,
                                             model = tsglm.model,
                                             xreg  = regressors,
                                             distr = tsglm.distr)
            results$model3.1.tsglm.hartlepool.coef <- se(model3.1.tsglm.hartlepool)
            results$model3.1.tsglm.rochdale.coef$site          <- 'Hartlepool'
            results$model3.1.tsglm.rochdale.coef$indicator     <- indicator
            results$model3.1.tsglm.rochdale.coef$sub.indicator <- sub.indicator
        }
        ##################################################
        ## Model 3.1 - Hemel Hempstead                  ##
        ##################################################
        ## print("Hemel Hempstead")
        ts.vector  <- filter(df3.1,
                             town        %in% c('Hemel Hempstead', 'Warwick', 'Basingstoke', 'Yeovil')) %>%
                      as.data.frame() %>% .[,'value']
        df3.1$town_ <- relevel(df3.1$town, ref = 'Warwick')
        town <- model.matrix(~df3.1$town_) %>% as.data.frame()
        names(town) <- c('Intercept', 'basingstoke', 'bishop', 'blackburn', 'carlisle', 'grimsby', 'hartlepool', 'hemel', 'newark', 'rochdale', 'rotherham', 'salford', 'salisbury', 'scarborough', 'scunthorpe', 'southport', 'wansbeck', 'whitehaven', 'wigan', 'yeovil')
        regressors <- cbind(df3.1, town) %>%
                      filter(town        %in% c('Hemel Hempstead', 'Warwick', 'Basingstoke', 'Yeovil')) %>%
                      dplyr::select(hemel, basingstoke, yeovil, closure, season2, season3, season4, season5, season6, relative.month, other.centre)
        if(town.group$n[town.group$town == 'Hemel Hempstead'] > 0){
            model3.1.tsglm.hemel <- tsglm(ts = ts.vector,
                                        link = tsglm.link,
                                        model = tsglm.model,
                                        xreg  = regressors,
                                        distr = tsglm.distr)
            results$model3.1.tsglm.hemel.coef <- se(model3.1.tsglm.hemel)
            results$model3.1.tsglm.rochdale.coef$site          <- 'Hemel Hempstead'
            results$model3.1.tsglm.rochdale.coef$indicator     <- indicator
            results$model3.1.tsglm.rochdale.coef$sub.indicator <- sub.indicator
        }
        ##################################################
        ## Model 3.1 - Newark                           ##
        ##################################################
        ## print("Newark")
        ts.vector  <- filter(df3.1,
                             town        %in% c('Newark', 'Southport', 'Carlisle', 'Salisbury')) %>%
                      as.data.frame() %>% .[,'value']
        df3.1$town_ <- relevel(df3.1$town, ref = 'Warwick')
        town <- model.matrix(~df3.1$town_) %>% as.data.frame()
        names(town) <- c('Intercept', 'basingstoke', 'bishop', 'blackburn', 'carlisle', 'grimsby', 'hartlepool', 'hemel', 'newark', 'rochdale', 'rotherham', 'salford', 'salisbury', 'scarborough', 'scunthorpe', 'wansbeck', 'warwick', 'whitehaven', 'wigan', 'yeovil')
        regressors <- cbind(df3.1, town) %>%
                      filter(town        %in% c('Newark', 'Southport', 'Carlisle', 'Salisbury')) %>%
                      dplyr::select(newark, carlisle, salisbury, closure, season2, season3, season4, season5, season6, relative.month, other.centre)
        if(town.group$n[town.group$town == 'Newark'] > 0){
            model3.1.tsglm.newark <- tsglm(ts = ts.vector,
                                         link = tsglm.link,
                                         model = tsglm.model,
                                         xreg  = regressors,
                                         distr = tsglm.distr)
            results$model3.1.tsglm.newark.coef <- se(model3.1.tsglm.newark)
            results$model3.1.tsglm.rochdale.coef$site          <- 'Newark'
            results$model3.1.tsglm.rochdale.coef$indicator     <- indicator
            results$model3.1.tsglm.rochdale.coef$sub.indicator <- sub.indicator
        }
        ##################################################
        ## Model 3.1 - Rochdale                         ##
        ##################################################
        ## print("Rochdale")
        ts.vector  <- filter(df3.1,
                             town        %in% c('Rochdale', 'Rotherham', 'Scunthorpe', 'Wansbeck')) %>%
                      as.data.frame() %>% .[,'value']
        df3.1$town_ <- relevel(df3.1$town, ref = 'Warwick')
        town <- model.matrix(~df3.1$town_) %>% as.data.frame()
        names(town) <- c('Intercept', 'basingstoke', 'bishop', 'blackburn', 'carlisle', 'grimsby', 'hartlepool', 'hemel', 'newark', 'rochdale', 'salford', 'salisbury', 'scarborough', 'scunthorpe', 'southport', 'wansbeck', 'warwick', 'whitehaven', 'wigan', 'yeovil')
        regressors <- cbind(df3.1, town) %>%
                      filter(town        %in% c('Rochdale', 'Rotherham', 'Scunthorpe', 'Wansbeck')) %>%
                      dplyr::select(rochdale, scunthorpe, wansbeck, closure, season2, season3, season4, season5, season6, relative.month, nhs111, other.centre, ambulance.divert)
        if(town.group$n[town.group$town == 'Rochdale'] > 0){
            model3.1.tsglm.rochdale <- tsglm(ts = ts.vector,
                                           link = tsglm.link,
                                           model = tsglm.model,
                                           xreg  = regressors,
                                           distr = tsglm.distr)
            results$model3.1.tsglm.rochdale.coef <- se(model3.1.tsglm.rochdale)
            results$model3.1.tsglm.rochdale.coef$site          <- 'Rochdale'
            results$model3.1.tsglm.rochdale.coef$indicator     <- indicator
            results$model3.1.tsglm.rochdale.coef$sub.indicator <- sub.indicator
        }
        ## Summary table
        results$model3.1.tsglm.coefficients <- combine_coefficients(bishop.coef     = results$model3.1.tsglm.bishop.coef,
                                                                  hartlepool.coef = results$model3.1.tsglm.hartlepool.coef,
                                                                  hemel.coef      = results$model3.1.tsglm.hemel.coef,
                                                                  newark.coef     = results$model3.1.tsglm.newark.coef,
                                                                  rochdale.coef   = results$model3.1.tsglm.rochdale.coef,
                                                                  .indicator      = indicator,
                                                                  .sub.indicator  = sub.indicator)
        ## Extract coefficients for plotting
        ## ## Forest plot
        results$model3.1.forest <- closed_forest(df.list = list(results$model3.1.tsglm.coefficients),
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
            if(exists('model3.1.tsglm.bishop')){
                results$model3.1.tsglm.bishop     <- model3.1.tsglm.bishop
            }
            if(exists('model3.1.tsglm.hartlepool')){
                results$model3.1.tsglm.hartlepool <- model3.1.tsglm.hartlepool
            }
            if(exists('model3.1.tsglm.hemel')){
                results$model3.1.tsglm.hemel      <- model3.1.tsglm.hemel
            }
            if(exists('model3.1.tsglm.newark')){
                results$model3.1.tsglm.newark     <- model3.1.tsglm.newark
            }
            if(exists('model3.1.tsglm.rochdale')){
                results$model3.1.tsglm.rochdale   <- model3.1.tsglm.rochdale
            }
        }
        if(return.df == TRUE){
            results$model3.1.df <- df3.1
        }
        if(return.residuals == TRUE){
            if(exists('model3.1.tsglm.bishop')){
                results$model3.1.tsglm.residuals.bishop     <- summary(model3.1.tsglm.bishop)$residuals
            }
            if(exists('model3.1.tsglm.hartlepool')){
                results$model3.1.tsglm.residuals.hartlepool <- summary(model3.1.tsglm.hartlepool)$residuals
            }
            if(exists('model3.1.tsglm.hemel')){
                results$model3.1.tsglm.residuals.hemel      <- summary(model3.1.tsglm.hemel)$residuals
            }
            if(exists('model3.1.tsglm.newark')){
                results$model3.1.tsglm.residuals.newark     <- summary(model3.1.tsglm.newark)$residuals
            }
            if(exists('model3.1.tsglm.rochdale')){
                results$model3.1.tsglm.residuals.rochdale   <- summary(model3.1.tsglm.rochdale)$residuals
            }
        }
        ## Remove clutter
        rm(df3.1)
        }

    ## #######################################################################
    ## ## Model 3.2                                                           ##
    ## #######################################################################
    ## if(!is.null(model3.2)){
    ##     ## print("Model 3.2")
    ##     ## Reformulate outcome and covariates
    ##     formula.model3.2 <- reformulate(response = outcome,
    ##                                   termlabels = model3.2)
    ##     ## Pool the data
    ##     df3.2 <- closed_pool(df             = df.trust,
    ##                        within.centres = TRUE)
    ##     ## Define group pooling for controls (but DON'T pool the data!)
    ##     ## df3.2 <- mutate(df3.2,
    ##     ##               pooled.control = ifelse(site.type %in% c('matched control', 'pooled control'), 'Control', town))
    ##     ## df3.2 <- mutate(df3.2,
    ##     ##               pooled.control = ifelse(pooled.control == 2, 'Bishop Auckland', pooled.control),
    ##     ##               pooled.control = ifelse(pooled.control == 6, 'Harltepool', pooled.control),
    ##     ##               pooled.control = ifelse(pooled.control == 7, 'Hemel Hempstead', pooled.control),
    ##     ##               pooled.control = ifelse(pooled.control == 8, 'Newark', pooled.control),
    ##     ##               pooled.control = ifelse(pooled.control == 9, 'Rochdale', pooled.control))
    ##     ## df3.2$pooled.control <- factor(df3.2$pooled.control)
    ##     ## Set reference group for pooled controls
    ##     df3.2$pooled.control <- relevel(df3.2$pooled.control, ref = 'Control')
    ##     ## Generate Time-Series Plots
    ##     df3.2$group <- paste0('Cohort : ', df3.2$group)
    ##     sites <- c('Bishop Auckland', 'Hartlepool', 'Hemel Hempstead', 'Newark', 'Rochdale', 'Control')
    ##     ## results$model3.2.ts.plot <- closed_ts_plot(df            = df3.2,
    ##     ##                                          sites         = sites,
    ##     ##                                          indicator     = indicator,
    ##     ##                                          sub.indicator = sub.indicator,
    ##     ##                                          steps         = TRUE,
    ##     ##                                          lines         = TRUE,
    ##     ##                                          xaxis.steps   = FALSE,
    ##     ##                                          facet         = TRUE,
    ##     ##                                          tidy          = TRUE,
    ##     ##                                          join          = join.line,
    ##     ##                                          legend        = legend,
    ##     ##                                          pool.control  = FALSE) ## NB - This is FALSE because data has already been pooled
    ##     ## Ensure relative.month is a numeric integer
    ##     df3.2$relative.month <- as.integer(df3.2$relative.month) %>% as.numeric()
    ##     ##################################################
    ##     ## Bishop Auckland                              ##
    ##     ##################################################
    ##     ## print("Bishop Auckland")
    ##     t <- filter(df3.2,
    ##                 group == 'Cohort : Bishop Auckland General Hospital') %>%
    ##         as.data.frame()
    ##     if(town.group$n[town.group$town == 'Bishop Auckland'] > 0 &
    ##        town.group$n[town.group$town == 'Salford'] > 0 &
    ##        town.group$n[town.group$town == 'Scarborough'] > 0 &
    ##        town.group$n[town.group$town == 'Whitehaven'] > 0){
    ##         ## print('Bishop Auckland')
    ##         model3.2.panelar.bishop <- panelAR(data     = t,
    ##                                           formula  = formula.model3.2,
    ##                                           timeVar  = timevar,
    ##                                           panelVar = panel.trust,
    ##                                           autoCorr = autocorr,
    ##                                           panelCorrMethod = 'pcse',
    ##                                           complete.case = complete.case,
    ##                                           seq.times = seq.times,
    ##                                           rho.na.rm = rho.na.rm)
    ##         results$model3.2.panelar.bishop.coef <- extract_coefficients(x              = model3.2.panelar.bishop,
    ##                                                                    .site          = 'Bishop Auckland',
    ##                                                                .indicator     = indicator,
    ##                                                                .sub.indicator = sub.indicator)
    ##         results$model3.2.panelar.bishop.r2 <- model3.2.panelar.bishop$r2
    ##     }
    ##     ##################################################
    ##     ## Hartlepool                                   ##
    ##     ##################################################
    ##     ## print("Hartlepool")
    ##     t <- filter(df3.2,
    ##                 group == 'Cohort : University Hospital of Hartlepool') %>%
    ##         as.data.frame()
    ##     if(town.group$n[town.group$town == 'Hartlepool'] > 0 &
    ##        town.group$n[town.group$town == 'Blackburn'] > 0 &
    ##        town.group$n[town.group$town == 'Grimsby'] > 0 &
    ##        town.group$n[town.group$town == 'Wigan'] > 0){
    ##         ## print('Hartlepool')
    ##         model3.2.panelar.hartlepool <- panelAR(data     = t,
    ##                                               formula  = formula.model3.2,
    ##                                                timeVar  = timevar,
    ##                                                panelVar = panel.trust,
    ##                                                autoCorr = autocorr,
    ##                                                panelCorrMethod = 'pcse',
    ##                                               complete.case = complete.case,
    ##                                                seq.times = seq.times,
    ##                                                rho.na.rm = rho.na.rm)
    ##         results$model3.2.panelar.hartlepool.coef <- extract_coefficients(x             = model3.2.panelar.hartlepool,
    ##                                                                        .site          = 'Hartlepool',
    ##                                                                        .indicator     = indicator,
    ##                                                                        .sub.indicator = sub.indicator)
    ##         results$model3.2.panelar.hartlepool.r2 <- model3.2.panelar.hartlepool$r2
    ##     }
    ##     ##################################################
    ##     ## Hemel Hempstead                              ##
    ##     ##################################################
    ##     ## print("Hemel Hempstead")
    ##     t <- filter(df3.2,
    ##                 group == 'Cohort : Hemel Hempstead Hospital') %>%
    ##          as.data.frame()
    ##     if(town.group$n[town.group$town == 'Hemel Hempstead'] > 0 &
    ##        town.group$n[town.group$town == 'Basingstoke'] > 0 &
    ##        town.group$n[town.group$town == 'Warwick'] > 0 &
    ##        town.group$n[town.group$town == 'Yeovil'] > 0){
    ##         ## print('Hemel Hempstead')
    ##         model3.2.panelar.hemel <- panelAR(data     = t,
    ##                                         formula  = formula.model3.2,
    ##                                         timeVar  = timevar,
    ##                                         panelVar = panel.trust,
    ##                                         autoCorr = autocorr,
    ##                                         panelCorrMethod = 'pcse',
    ##                                         complete.case = complete.case,
    ##                                         seq.times = seq.times,
    ##                                         rho.na.rm = rho.na.rm)
    ##         results$model3.2.panelar.hemel.coef <- extract_coefficients(x              = model3.2.panelar.hemel,
    ##                                                                   .site          = 'Hemel Hempstead',
    ##                                                                   .indicator     = indicator,
    ##                                                                   .sub.indicator = sub.indicator)
    ##         results$model3.2.panelar.hemel.r2 <- model3.2.panelar.hemel$r2
    ##     }
    ##     ##################################################
    ##     ## Newark                                       ##
    ##     ##################################################
    ##     ## print("Newark")
    ##     t <- filter(df3.2,
    ##                 group == 'Cohort : Newark Hospital') %>%
    ##          as.data.frame()
    ##     if(town.group$n[town.group$town == 'Newark'] > 0 &
    ##        town.group$n[town.group$town == 'Carlisle'] > 0 &
    ##        town.group$n[town.group$town == 'Salisbury'] > 0 &
    ##        town.group$n[town.group$town == 'Southport'] > 0){
    ##         ## print('Newark')
    ##         model3.2.panelar.newark <- panelAR(data     = t,
    ##                                          formula  = formula.model3.2,
    ##                                          timeVar  = timevar,
    ##                                          panelVar = panel.trust,
    ##                                          autoCorr = autocorr,
    ##                                          panelCorrMethod = 'pcse',
    ##                                          complete.case = complete.case,
    ##                                          seq.times = seq.times,
    ##                                          rho.na.rm = rho.na.rm)
    ##         results$model3.2.panelar.newark.coef <- extract_coefficients(x              = model3.2.panelar.newark,
    ##                                                                    .site          = 'Newark',
    ##                                                                    .indicator     = indicator,
    ##                                                                    .sub.indicator = sub.indicator)
    ##         results$model3.2.panelar.newark.r2 <- model3.2.panelar.newark$r2
    ##     }
    ##     ##################################################
    ##     ## Rochdale                                     ##
    ##     ##################################################
    ##     ## print("Rochdale")
    ##     t <- filter(df3.2,
    ##                 group == 'Cohort : Rochdale Infirmary') %>%
    ##         as.data.frame()
    ##     if(town.group$n[town.group$town == 'Rochdale'] > 0 &
    ##        town.group$n[town.group$town == 'Rotherham'] > 0 &
    ##        town.group$n[town.group$town == 'Scunthorpe'] > 0 &
    ##        town.group$n[town.group$town == 'Wansbeck'] > 0){
    ##         ## print('Rochdale')
    ##         model3.2.panelar.rochdale <- panelAR(data     = t,
    ##                                            formula  = formula.model3.2,
    ##                                            timeVar  = timevar,
    ##                                            panelVar = panel.trust,
    ##                                            autoCorr = autocorr,
    ##                                            panelCorrMethod = 'pcse',
    ##                                            complete.case = complete.case,
    ##                                            seq.times = seq.times,
    ##                                            rho.na.rm = rho.na.rm)
    ##         results$model3.2.panelar.rochdale.coef <- extract_coefficients(x            = model3.2.panelar.rochdale,
    ##                                                                      .site          = 'Rochdale',
    ##                                                                      .indicator     = indicator,
    ##                                                                      .sub.indicator = sub.indicator)
    ##         results$model3.2.panelar.rochdale.r2 <- model3.2.panelar.rochdale$r2
    ##     }
    ##     ##################################################
    ##     ## All sites                                    ##
    ##     ##################################################
    ##     ## TODO - Maybe get this working
    ##     ## Pool the data across centres
    ##     ## df3.2 <- closed_pool(df             = df.trust,
    ##     ##                    within.centres = FALSE)
    ##     ## ## Remove 'season' from the model since that is only feasible when pooling within
    ##     ## ## centres because months/dates do not align
    ##     ## model3.2 <- gsub('season', '', model3.2)
    ##     ## formula.model3.2 <- reformulate(response = outcome,
    ##     ##                               termlabels = model3.2)
    ##     ## t <- filter(df3.2,
    ##     ##             measure     == indicator &
    ##     ##             sub.measure == sub.indicator) %>%
    ##     ##      as.data.frame()
    ##     ## if(town.group$n[town.group$town == 'Bishop Auckland'] > 0 &
    ##     ##    town.group$n[town.group$town == 'Salford'] > 0 &
    ##     ##    town.group$n[town.group$town == 'Scarborough'] > 0 &
    ##     ##    town.group$n[town.group$town == 'Whitehaven'] > 0 &
    ##     ##    town.group$n[town.group$town == 'Hartlepool'] > 0 &
    ##     ##    town.group$n[town.group$town == 'Blackburn'] > 0 &
    ##     ##    town.group$n[town.group$town == 'Grimsby'] > 0 &
    ##     ##    town.group$n[town.group$town == 'Wigan'] > 0 &
    ##     ##    town.group$n[town.group$town == 'Hemel Hempstead'] > 0 &
    ##     ##    town.group$n[town.group$town == 'Basingstoke'] > 0 &
    ##     ##    town.group$n[town.group$town == 'Warwick'] > 0 &
    ##     ##    town.group$n[town.group$town == 'Yeovil'] > 0 &
    ##     ##    town.group$n[town.group$town == 'Newark'] > 0 &
    ##     ##    town.group$n[town.group$town == 'Carlisle'] > 0 &
    ##     ##    town.group$n[town.group$town == 'Salisbury'] > 0 &
    ##     ##    town.group$n[town.group$town == 'Southport'] > 0 &
    ##     ##    town.group$n[town.group$town == 'Rochdale'] > 0 &
    ##     ##    town.group$n[town.group$town == 'Rotherham'] > 0 &
    ##     ##    town.group$n[town.group$town == 'Scunthorpe'] > 0 &
    ##     ##    town.group$n[town.group$town == 'Wansbeck'] > 0){
    ##     ##     table(t$town) %>% print()
    ##     ##     formula.model3.2 %>% print()
    ##     ##     model3.2.panelar.all <- panelAR(data     = t,
    ##     ##                                    formula  = formula.model3.2,
    ##     ##                                    timeVar  = timevar,
    ##     ##                                    panelVar = panel.trust,
    ##     ##                                    autoCorr = autocorr,
    ##     ##                                    panelCorrMethod = 'pcse',
    ##     ##                                    seq.times = seq.times,
    ##     ##                                    rho.na.rm = rho.na.rm)
    ##     ##     results$model3.2.panelar.all.coef <- extract_coefficients(x              = model3.2.panelar.all,
    ##     ##                                                              .site          = 'All',
    ##     ##                                                              .indicator     = indicator,
    ##     ##                                                              .sub.indicator = sub.indicator)
    ##     ##     results$model3.2.panelar.r2 <- model3.2.panelar.all
    ##     ## }
    ##     ## Summary table
    ##     if(!is.null(results$model3.2.panelar.bishop.coef) |
    ##        !is.null(results$model3.2.panelar.hartlepool.coef) |
    ##        !is.null(results$model3.2.panelar.hemel.coef) |
    ##        !is.null(results$model3.2.panelar.newark.coef) |
    ##        !is.null(results$model3.2.panelar.rochdale.coef)){
    ##        ## !is.null(results$model3.2.panelar.all.coef)){
    ##         results$model3.2.panelar.all <- combine_coefficients(bishop.coef     = results$model3.2.panelar.bishop.coef,
    ##                                                             hartlepool.coef = results$model3.2.panelar.hartlepool.coef,
    ##                                                             hemel.coef      = results$model3.2.panelar.hemel.coef,
    ##                                                             newark.coef     = results$model3.2.panelar.newark.coef,
    ##                                                             rochdale.coef   = results$model3.2.panelar.rochdale.coef,
    ##                                                             all.coef        = results$model3.2.panelar.all.coef)
    ##         ## Forest plot
    ##         results$model3.2.forest <- closed_forest(df.list       = list(results$model3.2.panelar.bishop.coef,
    ##                                                                      results$model3.2.panelar.hartlepool.coef,
    ##                                                                      results$model3.2.panelar.hemel.coef,
    ##                                                                      results$model3.2.panelar.newark.coef,
    ##                                                                      results$model3.2.panelar.rochdale.coef,
    ##                                                                      results$model3.2.panelar.all.coef),
    ##                                                 plot.term     = c('closure'),
    ##                                                 facet.outcome = FALSE,
    ##                                                 title         = paste0('Model 8 : ',
    ##                                                                        indicator,
    ##                                                                        ' (',
    ##                                                                        sub.indicator,
    ##                                                                        ')'),
    ##                                                 theme         = theme_bw())
    ##     }
    ##     ## Return model objects if requested
    ##     if(return.model == TRUE){
    ##         if(exists('model3.2.panelar.bishop')){
    ##             results$model3.2.panelar.bishop     <- model3.2.panelar.bishop
    ##         }
    ##         if(exists('model3.2.panelar.hartlepool')){
    ##             results$model3.2.panelar.hartlepool <- model3.2.panelar.hartlepool
    ##         }
    ##         if(exists('model3.2.panelar.hemel')){
    ##             results$model3.2.panelar.hemel      <- model3.2.panelar.hemel
    ##         }
    ##         if(exists('model3.2.panelar.newark')){
    ##             results$model3.2.panelar.newark     <- model3.2.panelar.newark
    ##         }
    ##         if(exists('model3.2.panelar.rochdale')){
    ##             results$model3.2.panelar.rochdale   <- model3.2.panelar.rochdale
    ##         }
    ##     }
    ##     if(return.df == TRUE){
    ##         results$model3.2.df <- df3.2
    ##     }
    ##     if(return.residuals == TRUE){
    ##         if(exists('model3.2.panelar.bishop')){
    ##             results$model3.2.panelar.residuals.bishop     <- summary(model3.2.panelar.bishop)$residuals
    ##         }
    ##         if(exists('model3.2.panelar.hartlepool')){
    ##             results$model3.2.panelar.residuals.hartlepool <- summary(model3.2.panelar.hartlepool)$residuals
    ##         }
    ##         if(exists('model3.2.panelar.hemel')){
    ##             results$model3.2.panelar.residuals.hemel      <- summary(model3.2.panelar.hemel)$residuals
    ##         }
    ##         if(exists('model3.2.panelar.newark')){
    ##             results$model3.2.panelar.residuals.newark     <- summary(model3.2.panelar.newark)$residuals
    ##         }
    ##         if(exists('model3.2.panelar.rochdale')){
    ##             results$model3.2.panelar.residuals.rochdale   <- summary(model3.2.panelar.rochdale)$residuals
    ##         }
    ##     }
    ##     ## Remove clutter
    ##     rm(df3.2)
    ## }
    ## #######################################################################
    ## ## Model 4                                                           ##
    ## #######################################################################
    ## if(!is.null(model4)){
    ##     ## print("Model 4")
    ##     ## Reformulate outcome and covariates
    ##     formula.model4 <- reformulate(response = outcome,
    ##                                   termlabels = model4)
    ##     ## Subset data
    ##     sites <- c('Bishop Auckland', 'Whitehaven',
    ##                'Hartlepool', 'Grimsby',
    ##                'Hemel Hempstead', 'Warwick',
    ##                'Newark', 'Southport',
    ##                'Rochdale', 'Rotherham')
    ##     df4 <- filter(df.trust, town %in% sites  &
    ##                   measure     == indicator &
    ##                   sub.measure == sub.indicator)
    ##     ## Add in indicator of case/control status for plotting
    ##     case <- c('Bishop Auckland',
    ##               'Hartlepool',
    ##               'Hemel Hempstead',
    ##               'Newark',
    ##               'Rochdale')
    ##     df4$status <- ifelse(df4$town %in% case, 'Case', 'Control')
    ##     ## Generate time-series plot (at site/town level)
    ##     df4$group <- paste0('Cohort : ', df4$group)
    ##     ## results$model4.ts.plot <- closed_ts_plot(df = df4,
    ##     ##                                          sites = sites,
    ##     ##                                          indicator = indicator,
    ##     ##                                          sub.indicator = sub.indicator,
    ##     ##                                          steps = TRUE,
    ##     ##                                          lines         = TRUE,
    ##     ##                                          xaxis.steps   = FALSE,
    ##     ##                                          facet = TRUE,
    ##     ##                                          tidy  = TRUE,
    ##     ##                                          join  = join.line,
    ##     ##                                          legend = legend)
    ##     ## Perform analysis with panelAR in each
    ##     ##################################################
    ##     ## All                                          ##
    ##     ##################################################
    ##     t <- df4
    ##     if(town.group$n[town.group$town == 'Bishop Auckland'] > 0 &
    ##        town.group$n[town.group$town == 'Whitehaven'] > 0 &
    ##        town.group$n[town.group$town == 'Hartlepool'] > 0 &
    ##        town.group$n[town.group$town == 'Grimsby'] > 0 &
    ##        town.group$n[town.group$town == 'Hemel Hempstead'] > 0 &
    ##        town.group$n[town.group$town == 'Warwick'] > 0 &
    ##        town.group$n[town.group$town == 'Newark'] > 0 &
    ##        town.group$n[town.group$town == 'Southport'] > 0 &
    ##        town.group$n[town.group$town == 'Rochdale'] > 0 &
    ##        town.group$n[town.group$town == 'Rotherham'] > 0){
    ##         t$town <- relevel(t$town, ref = 'Grimsby')
    ##         model4.panelar <- panelAR(data     = t,
    ##                                   formula  = formula.model4,
    ##                                   timeVar  = timevar,
    ##                                   panelVar = panel.trust,
    ##                                   autoCorr = autocorr,
    ##                                   panelCorrMethod = 'pcse',
    ##                                   complete.case = complete.case,
    ##                                   seq.times = seq.times,
    ##                                   rho.na.rm = rho.na.rm)
    ##         results$model4.panelar.all.coef <- extract_coefficients(x              = model4.panelar,
    ##                                                             .site          = 'All',
    ##                                                             .indicator     = indicator,
    ##                                                             .sub.indicator = sub.indicator)
    ##         results$model4.panelar.r2 <- model4.panelar$r2
    ##     }
    ##     ## Summary table
    ##     if(!is.null(results$model2.panelar.bishop.coef) |
    ##        !is.null(results$model2.panelar.hartlepool.coef) |
    ##        !is.null(results$model2.panelar.hemel.coef) |
    ##        !is.null(results$model2.panelar.newark.coef) |
    ##        !is.null(results$model2.panelar.rochdale.coef) |
    ##        !is.null(results$model4.panelar.all.coef)){
    ##         results$model4.panelar.all <- combine_coefficients(bishop.coef     = results$model2.panelar.bishop.coef,
    ##                                                            hartlepool.coef = results$model2.panelar.hartlepool.coef,
    ##                                                            hemel.coef      = results$model2.panelar.hemel.coef,
    ##                                                            newark.coef     = results$model2.panelar.newark.coef,
    ##                                                            rochdale.coef   = results$model2.panelar.rochdale.coef,
    ##                                                            all.coef        = results$model4.panelar.all.coef)
    ##         ## Forest plot
    ##         results$model4.forest <- closed_forest(df.list       = list(results$model2.panelar.bishop.coef,
    ##                                                                     results$model2.panelar.hartlepool.coef,
    ##                                                                     results$model2.panelar.hemel.coef,
    ##                                                                     results$model2.panelar.newark.coef,
    ##                                                                     results$model2.panelar.rochdale.coef,
    ##                                                                     results$model4.panelar.all.coef),
    ##                                                plot.term     = c('closure'),
    ##                                                facet.outcome = FALSE,
    ##                                                title         = paste0('Model 2 & 4 : ',
    ##                                                                       indicator,
    ##                                                                       ' (',
    ##                                                                       sub.indicator,
    ##                                                                       ')'),
    ##                                                theme         = theme_bw())
    ##     }
    ##     ## Return model objects if requested
    ##     if(return.model == TRUE){
    ##         if(exists('model4.panelar')){
    ##             results$model4.panelar     <- model4.panelar
    ##         }
    ##     }
    ##     if(return.df == TRUE){
    ##         results$model4.df <- df4
    ##     }
    ##     if(return.residuals == TRUE){
    ##         if(exists('model4.panelar')){
    ##             results$model4.panelar.residuals     <- summary(model4.panelar)$residuals
    ##         }
    ##     }
    ##     ## Remove clutter
    ##     rm(df4)
    ## }
    ## #######################################################################
    ## ## Model 5                                                           ##
    ## #######################################################################
    ## if(!is.null(model5)){
    ##     ## print("Model 5")
    ##     ## Reformulate outcome and covariates
    ##     formula.model5 <- reformulate(response = outcome,
    ##                                   termlabels = model5)
    ##     ## Subset data
    ##     df5 <- filter(df.trust,
    ##                   measure     == indicator &
    ##                   sub.measure == sub.indicator)
    ##     ## Add in indicator of case/control status for plotting
    ##     case <- c('Bishop Auckland',
    ##               'Hartlepool',
    ##               'Hemel Hempstead',
    ##               'Newark',
    ##               'Rochdale')
    ##     df5$status <- ifelse(df5$town %in% case, 'Case', 'Control')
    ##     ## Generate time-series plot (at site/town level)
    ##     df5$group <- paste0('Cohort : ', df5$group)
    ##     sites <- c('Basingstoke', 'Bishop Auckland', 'Blackburn', 'Carlisle', 'Grimsby', 'Hartlepool', 'Hemel Hempstead', 'Newark', 'Rochdale', 'Rotherham', 'Salford', 'Salisbury', 'Scarborough', 'Scunthorpe', 'Southport', 'WansbeckWarwick', 'Whitehaven', 'Wigan', 'Yeovil')
    ##     ## results$model5.ts.plot <- closed_ts_plot(df = df5,
    ##     ##                                          sites = sites,
    ##     ##                                          indicator = indicator,
    ##     ##                                          sub.indicator = sub.indicator,
    ##     ##                                          steps = TRUE,
    ##     ##                                          lines         = TRUE,
    ##     ##                                          xaxis.steps   = FALSE,
    ##     ##                                          facet = TRUE,
    ##     ##                                          tidy  = TRUE,
    ##     ##                                          join  = join.line,
    ##     ##                                          legend = legend)
    ##     ## Perform analysis with panelAR in each
    ##     ##################################################
    ##     ## All sites                                    ##
    ##     ##################################################
    ##     t <- df5
    ##     if(town.group$n[town.group$town == 'Bishop Auckland'] > 0 &
    ##        town.group$n[town.group$town == 'Salford'] > 0 &
    ##        town.group$n[town.group$town == 'Scarborough'] > 0 &
    ##        town.group$n[town.group$town == 'Whitehaven'] > 0 &
    ##        town.group$n[town.group$town == 'Hartlepool'] > 0 &
    ##        town.group$n[town.group$town == 'Blackburn'] > 0 &
    ##        town.group$n[town.group$town == 'Grimsby'] > 0 &
    ##        town.group$n[town.group$town == 'Wigan'] > 0 &
    ##        town.group$n[town.group$town == 'Hemel Hempstead'] > 0 &
    ##        town.group$n[town.group$town == 'Basingstoke'] > 0 &
    ##        town.group$n[town.group$town == 'Warwick'] > 0 &
    ##        town.group$n[town.group$town == 'Yeovil'] > 0 &
    ##        town.group$n[town.group$town == 'Newark'] > 0 &
    ##        town.group$n[town.group$town == 'Carlisle'] > 0 &
    ##        town.group$n[town.group$town == 'Salisbury'] > 0 &
    ##        town.group$n[town.group$town == 'Southport'] > 0 &
    ##        town.group$n[town.group$town == 'Rochdale'] > 0 &
    ##        town.group$n[town.group$town == 'Rotherham'] > 0 &
    ##        town.group$n[town.group$town == 'Scunthorpe'] > 0 &
    ##        town.group$n[town.group$town == 'Wansbeck'] > 0){
    ##         t$town <- relevel(t$town, ref = 'Grimsby')
    ##         model5.panelar <- panelAR(data     = t,
    ##                                   formula  = formula.model5,
    ##                                   timeVar  = timevar,
    ##                                   panelVar = panel.trust,
    ##                                   autoCorr = autocorr,
    ##                                   panelCorrMethod = 'pcse',
    ##                                   complete.case = complete.case,
    ##                                   seq.times = seq.times,
    ##                                   rho.na.rm = rho.na.rm)
    ##         results$model5.panelar.all.coef <- extract_coefficients(x              = model5.panelar,
    ##                                                                 .site          = 'All',
    ##                                                                 .indicator     = indicator,
    ##                                                                 .sub.indicator = sub.indicator)
    ##         results$model5.panelar.r2 <- model5.panelar$r2
    ##     }
    ##     ## Summary table
    ##     if(!is.null(results$model3.1.panelar.bishop.coef) |
    ##        !is.null(results$model3.1.panelar.hartlepool.coef) |
    ##        !is.null(results$model3.1.panelar.hemel.coef) |
    ##        !is.null(results$model3.1.panelar.newark.coef) |
    ##        !is.null(results$model3.1.panelar.rochdale.coef) |
    ##        !is.null(results$model5.panelar.all.coef)){
    ##         results$model5.panelar.all <- combine_coefficients(bishop.coef     = results$model3.1.panelar.bishop.coef,
    ##                                                            hartlepool.coef = results$model3.1.panelar.hartlepool.coef,
    ##                                                            hemel.coef      = results$model3.1.panelar.hemel.coef,
    ##                                                            newark.coef     = results$model3.1.panelar.newark.coef,
    ##                                                            rochdale.coef   = results$model3.1.panelar.rochdale.coef,
    ##                                                            all.coef        = results$model5.panelar.all.coef)
    ##         ## Forest plot
    ##         results$model5.forest <- closed_forest(df.list       = list(results$model3.1.panelar.bishop.coef,
    ##                                                                     results$model3.1.panelar.hartlepool.coef,
    ##                                                                     results$model3.1.panelar.hemel.coef,
    ##                                                                     results$model3.1.panelar.newark.coef,
    ##                                                                     results$model3.1.panelar.rochdale.coef,
    ##                                                                     results$model5.panelar.all.coef),
    ##                                                plot.term     = c('closure'),
    ##                                                facet.outcome = FALSE,
    ##                                                title         = paste0('Model 3 & 5  : ',
    ##                                                                       indicator,
    ##                                                                       ' (',
    ##                                                                       sub.indicator,
    ##                                                                       ')'),
    ##                                                theme         = theme_bw())
    ##     }
    ##     ## Return model objects if requested
    ##     if(return.model == TRUE){
    ##         if(exists('model5.panelar')){
    ##             results$model5.panelar     <- model5.panelar
    ##         }
    ##     }
    ##     if(return.df == TRUE){
    ##         results$model5.df <- df5
    ##     }
    ##     if(return.residuals == TRUE){
    ##         if(exists('model5.panelar')){
    ##             results$model5.panelar.residuals.all     <- summary(model5.panelar)$residuals
    ##         }
    ##     }
    ##     ## Remove clutter
    ##     rm(df5)
    ## }
    ## #######################################################################
    ## ## Model 6.1                                                         ##
    ## #######################################################################
    ## if(!is.null(model6.1)){
    ##     ## print("Model 6.1")
    ##     ## Reformulate outcome and covariates
    ##     formula.model6.1 <- reformulate(response = outcome,
    ##                                   termlabels = model6.1)
    ##     ## Subset data
    ##     sites <- c('Bishop Auckland',
    ##                'Hartlepool',
    ##                'Hemel Hempstead',
    ##                'Newark',
    ##                'Rochdale')
    ##     df6.1 <- filter(df.lsoa, town %in% sites &
    ##                         measure     == indicator &
    ##                         sub.measure == sub.indicator)
    ##     df6.1$group <- paste0('Cohort : ', df6.1$group)
    ##     ## Add in indicator of case/control status for plotting
    ##     case <- c('Bishop Auckland',
    ##               'Hartlepool',
    ##               'Hemel Hempstead',
    ##               'Newark',
    ##               'Rochdale')
    ##     df6.1$status <- ifelse(df6.1$town %in% case, 'Case', 'Control')
    ##     ## Generate time-series plot (at site/town level)
    ##     df6.1.trust <- filter(df.trust, town %in% sites &
    ##                         measure     == indicator &
    ##                         sub.measure == sub.indicator)
    ##     df6.1.trust$group <- paste0('Cohort : ', df6.1.trust$group)
    ##     ## results$model6.1.ts.plot <- closed_ts_plot(df = df6.1.trust,
    ##     ##                                          sites = sites,
    ##     ##                                          indicator = indicator,
    ##     ##                                          sub.indicator = sub.indicator,
    ##     ##                                          steps = TRUE,
    ##     ##                                          lines         = TRUE,
    ##     ##                                          xaxis.steps   = FALSE,
    ##     ##                                          facet = TRUE,
    ##     ##                                          tidy  = TRUE,
    ##     ##                                          join  = join.line,
    ##     ##                                          legend = legend)
    ##     ## Perform analysis with panelAR in each
    ##     ##################################################
    ##     ## Model 6.1 - Bishop Auckland                    ##
    ##     ##################################################
    ##     ## print("Bishop Auckland")
    ##     t <- filter(df6.1,
    ##                 town == 'Bishop Auckland')
    ##     ## Getting errors with complete.case == TRUE...
    ##     ##
    ##     ## Error: Unable to compute correlated SEs / PCSEs because there are no time
    ##     ## periods in common across all units. Instead, consider setting
    ##     ## complete.case =FALSE.
    ##     ##
    ##     ## ...so have opted for that for ALL LSOA analyses rather than conditionally
    ##     ## switching.
    ##     ## complete.case.6.1 <- complete.case
    ##     ## if(indicator == 'case fatality ratio' & sub.indicator == 'any') complete.case.6.1 <- FALSE
    ##     complete.case <- FALSE
    ##     if(town.group$n[town.group$town == 'Bishop Auckland'] > 0){
    ##         ## Remove instances where there are missing observations for LSOAs
    ##         t <- filter(t, !is.na(value))
    ##         model6.1.panelar.bishop <- panelAR(data     = t,
    ##                                          formula  = formula.model6.1,
    ##                                          timeVar  = timevar,
    ##                                          panelVar = panel.lsoa,
    ##                                          autoCorr = autocorr,
    ##                                          panelCorrMethod = 'pcse',
    ##                                          complete.case = complete.case,
    ##                                          seq.times = seq.times,
    ##                                          rho.na.rm = rho.na.rm)
    ##         results$model6.1.panelar.bishop.coef <- extract_coefficients(x              = model6.1.panelar.bishop,
    ##                                                                    .site          = 'Bishop Auckland',
    ##                                                                    .indicator     = indicator,
    ##                                                                    .sub.indicator = sub.indicator)
    ##         results$model6.1.panelar.bishop.r2 <- model6.1.panelar.bishop
    ##     }
    ##     ##################################################
    ##     ## Model 6.1 - Hartlepool                       ##
    ##     ##################################################
    ##     ## print("Hartlepool")
    ##     t <- filter(df6.1,
    ##                 town == 'Hartlepool')
    ##     if(town.group$n[town.group$town == 'Hartlepool'] > 0){
    ##         ## Remove instances where there are missing observations for LSOAs
    ##         t <- filter(t, !is.na(value))
    ##         model6.1.panelar.hartlepool <- panelAR(data     = t,
    ##                                              formula  = formula.model6.1,
    ##                                              timeVar  = timevar,
    ##                                              panelVar = panel.lsoa,
    ##                                              autoCorr = autocorr,
    ##                                              panelCorrMethod = 'pcse',
    ##                                              complete.case = complete.case,
    ##                                              seq.times = seq.times,
    ##                                              rho.na.rm = rho.na.rm)
    ##         results$model6.1.panelar.hartlepool.coef <- extract_coefficients(x             = model6.1.panelar.hartlepool,
    ##                                                                        .site          = 'Hartlepool',
    ##                                                                        .indicator     = indicator,
    ##                                                                        .sub.indicator = sub.indicator)
    ##         results$model6.1.panelar.hartlepool.r2 <- model6.1.panelar.hartlepool$r2
    ##     }
    ##     ##################################################
    ##     ## Model 6.1 - Hemel Hempstead                  ##
    ##     ##################################################
    ##     ## print("Hemel Hempstead")
    ##     t <- filter(df6.1,
    ##                 town == 'Hemel Hempstead')
    ##     if(town.group$n[town.group$town == 'Hemel Hempstead'] > 0){
    ##         ## Remove instances where there are missing observations for LSOAs
    ##         t <- filter(t, !is.na(value))
    ##         model6.1.panelar.hemel <- panelAR(data     = t,
    ##                                         formula  = formula.model6.1,
    ##                                         timeVar  = timevar,
    ##                                         panelVar = panel.lsoa,
    ##                                         autoCorr = autocorr,
    ##                                         panelCorrMethod = 'pcse',
    ##                                         complete.case = complete.case,
    ##                                         seq.times = seq.times,
    ##                                         rho.na.rm = rho.na.rm)
    ##         results$model6.1.panelar.hemel.coef <- extract_coefficients(x              = model6.1.panelar.hemel,
    ##                                                                   .site          = 'Hemel Hempstead',
    ##                                                                   .indicator     = indicator,
    ##                                                                   .sub.indicator = sub.indicator)
    ##         results$model6.1.panelar.hemel.r2 <- model6.1.panelar.hemel$r2
    ##     }
    ##     ##################################################
    ##     ## Model 6.1 - Newark                           ##
    ##     ##################################################
    ##     ## print("Newark")
    ##     t <- filter(df6.1,
    ##                 town == 'Newark')
    ##     if(town.group$n[town.group$town == 'Newark'] > 0){
    ##         ## Remove instances where there are missing observations for LSOAs
    ##         t <- filter(t, !is.na(value))
    ##         model6.1.panelar.newark <- panelAR(data     = t,
    ##                                          formula  = formula.model6.1,
    ##                                          timeVar  = timevar,
    ##                                          panelVar = panel.lsoa,
    ##                                          autoCorr = autocorr,
    ##                                          panelCorrMethod = 'pcse',
    ##                                          complete.case = complete.case,
    ##                                          seq.times = seq.times,
    ##                                          rho.na.rm = rho.na.rm)
    ##         results$model6.1.panelar.newark.coef <- extract_coefficients(x              = model6.1.panelar.newark,
    ##                                                                    .site          = 'Newark',
    ##                                                                    .indicator     = indicator,
    ##                                                                    .sub.indicator = sub.indicator)
    ##         results$model6.1.panelar.newark.r2 <- model6.1.panelar.newark$r2
    ##     }
    ##     ##################################################
    ##     ## Model 6.1 - Rochdale                         ##
    ##     ##################################################
    ##     ## print("Rochdale")
    ##     t <- filter(df6.1,
    ##                 town == 'Rochdale')
    ##     if(town.group$n[town.group$town == 'Rochdale'] > 0){
    ##         ## Remove instances where there are missing observations for LSOAs
    ##         t <- filter(t, !is.na(value))
    ##         model6.1.panelar.rochdale <- panelAR(data     = t,
    ##                                            formula  = formula.model6.1,
    ##                                            timeVar  = timevar,
    ##                                            panelVar = panel.lsoa,
    ##                                            autoCorr = autocorr,
    ##                                            panelCorrMethod = 'pcse',
    ##                                            complete.case = complete.case,
    ##                                            seq.times = seq.times,
    ##                                            rho.na.rm = rho.na.rm)
    ##         results$model6.1.panelar.rochdale.coef <- extract_coefficients(x            = model6.1.panelar.rochdale,
    ##                                                                      .site          = 'Rochdale',
    ##                                                                      .indicator     = indicator,
    ##                                                                      .sub.indicator = sub.indicator)
    ##         results$model6.1.panelar.rochdale.r2 <- model6.1.panelar.rochdale$r2
    ##     }
    ##     ## Summary table
    ##     if(!is.null(results$model6.1.panelar.bishop.coef) |
    ##        !is.null(results$model6.1.panelar.hartlepool.coef) |
    ##        !is.null(results$model6.1.panelar.hemel.coef) |
    ##        !is.null(results$model6.1.panelar.newark.coef) |
    ##        !is.null(results$model6.1.panelar.rochdale.coef)){
    ##         results$model6.1.panelar.all <- combine_coefficients(bishop.coef     = results$model6.1.panelar.bishop.coef,
    ##                                                            hartlepool.coef = results$model6.1.panelar.hartlepool.coef,
    ##                                                            hemel.coef      = results$model6.1.panelar.hemel.coef,
    ##                                                            newark.coef     = results$model6.1.panelar.newark.coef,
    ##                                                            rochdale.coef   = results$model6.1.panelar.rochdale.coef)
    ##         ## Forest plot
    ##         results$model6.1.forest.model6.1 <- closed_forest(df.list       = list(results$model6.1.panelar.bishop.coef,
    ##                                                                            results$model6.1.panelar.hartlepool.coef,
    ##                                                                            results$model6.1.panelar.hemel.coef,
    ##                                                                            results$model6.1.panelar.newark.coef,
    ##                                                                            results$model6.1.panelar.rochdale.coef),
    ##                                                       plot.term     = c('closure'),
    ##                                                       facet.outcome = FALSE,
    ##                                                       title         = paste0('Model 6.1 : ',
    ##                                                                              indicator,
    ##                                                                              ' (',
    ##                                                                              sub.indicator,
    ##                                                                              ')'),
    ##                                                       theme         = theme_bw())
    ##     }
    ##     ## Return model objects if requested
    ##     if(return.model == TRUE){
    ##         if(exists('model6.1.panelar.bishop')){
    ##             results$model6.1.panelar.bishop     <- model6.1.panelar.bishop
    ##         }
    ##         if(exists('model6.1.panelar.hartlepool')){
    ##             results$model6.1.panelar.hartlepool <- model6.1.panelar.hartlepool
    ##         }
    ##         if(exists('model6.1.panelar.hemel')){
    ##             results$model6.1.panelar.hemel      <- model6.1.panelar.hemel
    ##         }
    ##         if(exists('model6.1.panelar.newark')){
    ##             results$model6.1.panelar.newark     <- model6.1.panelar.newark
    ##         }
    ##         if(exists('model6.1.panelar.rochdale')){
    ##             results$model6.1.panelar.rochdale   <- model6.1.panelar.rochdale
    ##         }
    ##     }
    ##     if(return.df == TRUE){
    ##         results$model6.1.df <- df6.1
    ##     }
    ##     if(return.residuals == TRUE){
    ##         if(exists('model6.1.panelar.bishop')){
    ##             results$model6.1.panelar.residuals.bishop     <- summary(model6.1.panelar.bishop)$residuals
    ##         }
    ##         if(exists('model6.1.panelar.hartlepool')){
    ##             results$model6.1.panelar.residuals.hartlepool <- summary(model6.1.panelar.hartlepool)$residuals
    ##         }
    ##         if(exists('model6.1.panelar.hemel')){
    ##             results$model6.1.panelar.residuals.hemel      <- summary(model6.1.panelar.hemel)$residuals
    ##         }
    ##         if(exists('model6.1.panelar.newark')){
    ##             results$model6.1.panelar.residuals.newark     <- summary(model6.1.panelar.newark)$residuals
    ##         }
    ##         if(exists('model6.1.panelar.rochdale')){
    ##             results$model6.1.panelar.residuals.rochdale   <- summary(model6.1.panelar.rochdale)$residuals
    ##         }
    ##     }
    ##     ## Remove clutter
    ##     rm(df6.1)
    ## }
    ## #######################################################################
    ## ## Model 6.2                                                         ##
    ## #######################################################################
    ## if(!is.null(model6.2)){
    ##     ## print("Model 6.2")
    ##     ## Reformulate outcome and covariates
    ##     formula.model6.2 <- reformulate(response = outcome,
    ##                                   termlabels = model6.2)
    ##     ## Subset data
    ##     sites <- c('Bishop Auckland', 'Whitehaven',
    ##                'Hartlepool', 'Grimsby',
    ##                'Hemel Hempstead', 'Warwick',
    ##                'Newark', 'Southport',
    ##                'Rochdale', 'Rotherham')
    ##     df6.2 <- filter(df.lsoa, town %in% sites &
    ##                         measure     == indicator &
    ##                         sub.measure == sub.indicator)
    ##     df6.2$group <- paste0('Cohort : ', df6.2$group)
    ##     ## Add in indicator of case/control status for plotting
    ##     case <- c('Bishop Auckland',
    ##               'Hartlepool',
    ##               'Hemel Hempstead',
    ##               'Newark',
    ##               'Rochdale')
    ##     df6.2$status <- ifelse(df6.2$town %in% case, 'Case', 'Control')
    ##     ## Generate time-series plot (at site/town level)
    ##     df6.2.trust <- filter(df.trust, town %in% sites &
    ##                         measure     == indicator &
    ##                         sub.measure == sub.indicator)
    ##     df6.2.trust$group <- paste0('Cohort : ', df6.2.trust$group)
    ##     ## results$model6.2.ts.plot <- closed_ts_plot(df = df6.2.trust,
    ##     ##                                          sites = sites,
    ##     ##                                          indicator = indicator,
    ##     ##                                          sub.indicator = sub.indicator,
    ##     ##                                          steps = TRUE,
    ##     ##                                          lines         = TRUE,
    ##     ##                                          xaxis.steps   = FALSE,
    ##     ##                                          facet = TRUE,
    ##     ##                                          tidy  = TRUE,
    ##     ##                                          join  = join.line,
    ##     ##                                          legend = legend)
    ##     ## Perform analysis with panelAR in each
    ##     ##################################################
    ##     ## Model 6.2 - Bishop Auckland                  ##
    ##     ##################################################
    ##     ## print("Bishop Auckland")
    ##     t <- filter(df6.2,
    ##                 town == 'Bishop Auckland' |
    ##                 town == 'Whitehaven')
    ##     ## Getting errors with complete.case == TRUE...
    ##     ##
    ##     ## Error: Unable to compute correlated SEs / PCSEs because there are no time
    ##     ## periods in common across all units. Instead, consider setting
    ##     ## complete.case =FALSE.
    ##     ##
    ##     ## ...so have opted for that for ALL LSOA analyses rather than conditionally
    ##     ## switching.
    ##     ## complete.case.6.2 <- complete.case
    ##     ## if(indicator == 'case fatality ratio' & sub.indicator == 'any') complete.case.6.2 <- FALSE
    ##     complete.case <- FALSE
    ##     if(town.group$n[town.group$town == 'Bishop Auckland'] > 0 & town.group$n[town.group$town == 'Whitehaven'] > 0 ){
    ##         t$town <- relevel(t$town, ref = 'Whitehaven')
    ##         ## Remove instances where there are missing observations for LSOAs
    ##         t <- filter(t, !is.na(value))
    ##         model6.2.panelar.bishop <- panelAR(data     = t,
    ##                                          formula  = formula.model6.2,
    ##                                          timeVar  = timevar,
    ##                                          panelVar = panel.lsoa,
    ##                                          autoCorr = autocorr,
    ##                                          panelCorrMethod = 'pcse',
    ##                                          complete.case = complete.case,
    ##                                          seq.times = seq.times,
    ##                                          rho.na.rm = rho.na.rm)
    ##         results$model6.2.panelar.bishop.coef <- extract_coefficients(x              = model6.2.panelar.bishop,
    ##                                                                    .site          = 'Bishop Auckland',
    ##                                                                    .indicator     = indicator,
    ##                                                                    .sub.indicator = sub.indicator)
    ##         results$model6.2.panelar.bishop.r2 <- model6.2.panelar.bishop
    ##     }
    ##     ##################################################
    ##     ## Model 6.2 - Hartlepool                       ##
    ##     ##################################################
    ##     ## print("Hartlepool")
    ##     t <- filter(df6.2,
    ##                 town == 'Hartlepool' |
    ##                 town == 'Grimsby')
    ##     if(town.group$n[town.group$town == 'Hartlepool'] > 0 & town.group$n[town.group$town == 'Grimsby'] > 0){
    ##         t$town <- relevel(t$town, ref = 'Grimsby')
    ##         ## Remove instances where there are missing observations for LSOAs
    ##         t <- filter(t, !is.na(value))
    ##         model6.2.panelar.hartlepool <- panelAR(data     = t,
    ##                                              formula  = formula.model6.2,
    ##                                              timeVar  = timevar,
    ##                                              panelVar = panel.lsoa,
    ##                                              autoCorr = autocorr,
    ##                                              panelCorrMethod = 'pcse',
    ##                                              complete.case = complete.case,
    ##                                              seq.times = seq.times,
    ##                                              rho.na.rm = rho.na.rm)
    ##         results$model6.2.panelar.hartlepool.coef <- extract_coefficients(x             = model6.2.panelar.hartlepool,
    ##                                                                        .site          = 'Hartlepool',
    ##                                                                        .indicator     = indicator,
    ##                                                                        .sub.indicator = sub.indicator)
    ##         results$model6.2.panelar.hartlepool.r2 <- model6.2.panelar.hartlepool$r2
    ##     }
    ##     ##################################################
    ##     ## Model 6.2 - Hemel Hempstead                  ##
    ##     ##################################################
    ##     ## print("Hemel Hempstead")
    ##     t <- filter(df6.2,
    ##                 town == 'Hemel Hempstead' |
    ##                 town == 'Warwick')
    ##     if(town.group$n[town.group$town == 'Hemel Hempstead'] > 0 & town.group$n[town.group$town == 'Warwick'] > 0){
    ##         t$town <- relevel(t$town, ref = 'Warwick')
    ##         ## Remove instances where there are missing observations for LSOAs
    ##         t <- filter(t, !is.na(value))
    ##         model6.2.panelar.hemel <- panelAR(data     = t,
    ##                                         formula  = formula.model6.2,
    ##                                         timeVar  = timevar,
    ##                                         panelVar = panel.lsoa,
    ##                                         autoCorr = autocorr,
    ##                                         panelCorrMethod = 'pcse',
    ##                                         complete.case = complete.case,
    ##                                         seq.times = seq.times,
    ##                                         rho.na.rm = rho.na.rm)
    ##         results$model6.2.panelar.hemel.coef <- extract_coefficients(x              = model6.2.panelar.hemel,
    ##                                                                   .site          = 'Hemel Hempstead',
    ##                                                                   .indicator     = indicator,
    ##                                                                   .sub.indicator = sub.indicator)
    ##         results$model6.2.panelar.hemel.r2 <- model6.2.panelar.hemel$r2
    ##     }
    ##     ##################################################
    ##     ## Model 6.2 - Newark                           ##
    ##     ##################################################
    ##     ## print("Newark")
    ##     t <- filter(df6.2,
    ##                 town == 'Newark' |
    ##                 town == 'Southport')
    ##     if(town.group$n[town.group$town == 'Newark'] > 0 & town.group$n[town.group$town == 'Southport'] > 0){
    ##         t$town <- relevel(t$town, ref = 'Southport')
    ##         ## Remove instances where there are missing observations for LSOAs
    ##         t <- filter(t, !is.na(value))
    ##         model6.2.panelar.newark <- panelAR(data     = t,
    ##                                          formula  = formula.model6.2,
    ##                                          timeVar  = timevar,
    ##                                          panelVar = panel.lsoa,
    ##                                          autoCorr = autocorr,
    ##                                          panelCorrMethod = 'pcse',
    ##                                          complete.case = complete.case,
    ##                                          seq.times = seq.times,
    ##                                          rho.na.rm = rho.na.rm)
    ##         results$model6.2.panelar.newark.coef <- extract_coefficients(x              = model6.2.panelar.newark,
    ##                                                                    .site          = 'Newark',
    ##                                                                    .indicator     = indicator,
    ##                                                                    .sub.indicator = sub.indicator)
    ##         results$model6.2.panelar.newark.r2 <- model6.2.panelar.newark$r2
    ##     }
    ##     ##################################################
    ##     ## Model 6.2 - Rochdale                         ##
    ##     ##################################################
    ##     ## print("Rochdale")
    ##     t <- filter(df6.2,
    ##                 town == 'Rochdale' |
    ##                 town == 'Rotherham')
    ##     if(town.group$n[town.group$town == 'Rochdale'] > 0 & town.group$n[town.group$town == 'Rotherham'] > 0){
    ##         t$town <- relevel(t$town, ref = 'Rotherham')
    ##         ## Remove instances where there are missing observations for LSOAs
    ##         t <- filter(t, !is.na(value))
    ##         model6.2.panelar.rochdale <- panelAR(data     = t,
    ##                                            formula  = formula.model6.2,
    ##                                            timeVar  = timevar,
    ##                                            panelVar = panel.lsoa,
    ##                                            autoCorr = autocorr,
    ##                                            panelCorrMethod = 'pcse',
    ##                                            complete.case = complete.case,
    ##                                            seq.times = seq.times,
    ##                                            rho.na.rm = rho.na.rm)
    ##         results$model6.2.panelar.rochdale.coef <- extract_coefficients(x            = model6.2.panelar.rochdale,
    ##                                                                      .site          = 'Rochdale',
    ##                                                                      .indicator     = indicator,
    ##                                                                      .sub.indicator = sub.indicator)
    ##         results$model6.2.panelar.rochdale.r2 <- model6.2.panelar.rochdale$r2
    ##     }
    ##     ## Summary table
    ##     if(!is.null(results$model6.2.panelar.bishop.coef) |
    ##        !is.null(results$model6.2.panelar.hartlepool.coef) |
    ##        !is.null(results$model6.2.panelar.hemel.coef) |
    ##        !is.null(results$model6.2.panelar.newark.coef) |
    ##        !is.null(results$model6.2.panelar.rochdale.coef)){
    ##         results$model6.2.panelar.all <- combine_coefficients(bishop.coef     = results$model6.2.panelar.bishop.coef,
    ##                                                            hartlepool.coef = results$model6.2.panelar.hartlepool.coef,
    ##                                                            hemel.coef      = results$model6.2.panelar.hemel.coef,
    ##                                                            newark.coef     = results$model6.2.panelar.newark.coef,
    ##                                                            rochdale.coef   = results$model6.2.panelar.rochdale.coef)
    ##         ## Forest plot
    ##         results$model6.2.forest.model6.2 <- closed_forest(df.list       = list(results$model6.2.panelar.bishop.coef,
    ##                                                                            results$model6.2.panelar.hartlepool.coef,
    ##                                                                            results$model6.2.panelar.hemel.coef,
    ##                                                                            results$model6.2.panelar.newark.coef,
    ##                                                                            results$model6.2.panelar.rochdale.coef),
    ##                                                       plot.term     = c('closure'),
    ##                                                       facet.outcome = FALSE,
    ##                                                       title         = paste0('Model 6.2 : ',
    ##                                                                              indicator,
    ##                                                                              ' (',
    ##                                                                              sub.indicator,
    ##                                                                              ')'),
    ##                                                       theme         = theme_bw())
    ##     }
    ##     ## Return model objects if requested
    ##     if(return.model == TRUE){
    ##         if(exists('model6.2.panelar.bishop')){
    ##             results$model6.2.panelar.bishop     <- model6.2.panelar.bishop
    ##         }
    ##         if(exists('model6.2.panelar.hartlepool')){
    ##             results$model6.2.panelar.hartlepool <- model6.2.panelar.hartlepool
    ##         }
    ##         if(exists('model6.2.panelar.hemel')){
    ##             results$model6.2.panelar.hemel      <- model6.2.panelar.hemel
    ##         }
    ##         if(exists('model6.2.panelar.newark')){
    ##             results$model6.2.panelar.newark     <- model6.2.panelar.newark
    ##         }
    ##         if(exists('model6.2.panelar.rochdale')){
    ##             results$model6.2.panelar.rochdale   <- model6.2.panelar.rochdale
    ##         }
    ##     }
    ##     if(return.df == TRUE){
    ##         results$model6.2.df <- df6.2
    ##     }
    ##     if(return.residuals == TRUE){
    ##         if(exists('model6.2.panelar.bishop')){
    ##             results$model6.2.panelar.residuals.bishop     <- summary(model6.2.panelar.bishop)$residuals
    ##         }
    ##         if(exists('model6.2.panelar.hartlepool')){
    ##             results$model6.2.panelar.residuals.hartlepool <- summary(model6.2.panelar.hartlepool)$residuals
    ##         }
    ##         if(exists('model6.2.panelar.hemel')){
    ##             results$model6.2.panelar.residuals.hemel      <- summary(model6.2.panelar.hemel)$residuals
    ##         }
    ##         if(exists('model6.2.panelar.newark')){
    ##             results$model6.2.panelar.residuals.newark     <- summary(model6.2.panelar.newark)$residuals
    ##         }
    ##         if(exists('model6.2.panelar.rochdale')){
    ##             results$model6.2.panelar.residuals.rochdale   <- summary(model6.2.panelar.rochdale)$residuals
    ##         }
    ##     }
    ##     ## Remove clutter
    ##     rm(df6.2)
    ## }
    ## #######################################################################
    ## ## Model 7                                                           ##
    ## #######################################################################
    ## if(!is.null(model7)){
    ##     ## print("Model 7")
    ##     ## Reformulate outcome and covariates
    ##     formula.model7 <- reformulate(response = outcome,
    ##                                   termlabels = model7)
    ##     ## Subset data
    ##     sites <- c('Bishop Auckland', 'Whitehaven',
    ##                'Hartlepool', 'Grimsby',
    ##                'Hemel Hempstead', 'Warwick',
    ##                'Newark', 'Southport',
    ##                'Rochdale', 'Rotherham')
    ##     df7 <- filter(df.lsoa, town %in% sites &
    ##                   measure     == indicator &
    ##                   sub.measure == sub.indicator)
    ##     ## Two LSOAs overlap two EDs so derive a new unique indicator
    ##     ## and use that for the panels
    ##     df7$town.lsoa <- paste0(df7$town, df7$lsoa)
    ##     ## Add in indicator of case/control status for plotting
    ##     case <- c('Bishop Auckland',
    ##               'Hartlepool',
    ##               'Hemel Hempstead',
    ##               'Newark',
    ##               'Rochdale')
    ##     df7$status <- ifelse(df7$town %in% case, 'Case', 'Control')
    ##     results$df7 <- df7
    ##     ## Generate time-series plot (at site/town level)
    ##     df7$group <- paste0('Cohort : ', df7$group)
    ##     results$model7.ts.plot <- closed_ts_plot(df = df7,
    ##                                              sites = sites,
    ##                                              indicator = indicator,
    ##                                              sub.indicator = sub.indicator,
    ##                                              steps = TRUE,
    ##                                              lines         = TRUE,
    ##                                              xaxis.steps   = FALSE,
    ##                                              facet = TRUE,
    ##                                              tidy  = TRUE,
    ##                                              join  = join.line,
    ##                                              legend = legend)
    ##     ## Perform analysis with panelAR in each
    ##     ##################################################
    ##     ## All sites                                    ##
    ##     ##################################################
    ##     formula.model7 <- reformulate(response = outcome,
    ##                                    termlabels = c(model7))
    ##     t <- filter(df7,
    ##                 measure     == indicator &
    ##                 sub.measure == sub.indicator)
    ##     if(town.group$n[town.group$town == 'Bishop Auckland'] > 0 &
    ##        town.group$n[town.group$town == 'Whitehaven'] > 0 &
    ##        town.group$n[town.group$town == 'Hartlepool'] > 0 &
    ##        town.group$n[town.group$town == 'Grimsby'] > 0 &
    ##        town.group$n[town.group$town == 'Hemel Hempstead'] > 0 &
    ##        town.group$n[town.group$town == 'Warwick'] > 0 &
    ##        town.group$n[town.group$town == 'Newark'] > 0 &
    ##        town.group$n[town.group$town == 'Southport'] > 0 &
    ##        town.group$n[town.group$town == 'Rochdale'] > 0 &
    ##        town.group$n[town.group$town == 'Rotherham'] > 0){
    ##         t$town <- relevel(t$town, ref = 'Grimsby')
    ##         ## Getting errors with complete.case == TRUE...
    ##         ##
    ##         ## Error: Unable to compute correlated SEs / PCSEs because there are no time
    ##         ## periods in common across all units. Instead, consider setting
    ##         ## complete.case =FALSE.
    ##         ##
    ##         ## ...so have opted for that for ALL LSOA analyses rather than conditionally
    ##         ## switching.
    ##         ## if(indicator == 'length of stay')      complete.case <- FALSE
    ##         ## if(indicator == 'case fatality ratio') complete.case <- FALSE
    ##         complete.case <- FALSE
    ##         model7.panelar <- panelAR(data     = t,
    ##                                   formula  = formula.model7,
    ##                                   timeVar  = timevar,
    ##                                   panelVar = 'town.lsoa',
    ##                                   autoCorr = autocorr,
    ##                                   panelCorrMethod = 'pcse',
    ##                                   complete.case = complete.case,
    ##                                   seq.times = seq.times,
    ##                                   rho.na.rm = rho.na.rm)
    ##         results$model7.panelar.all.coef <- extract_coefficients(x              = model7.panelar,
    ##                                                             .site          = 'All',
    ##                                                             .indicator     = indicator,
    ##                                                             .sub.indicator = sub.indicator)
    ##         results$model7.panelar.r2 <- model7.panelar$r2
    ##     }
    ##     ## Summary table
    ##     if(!is.null(results$model6.1.panelar.bishop.coef) |
    ##        !is.null(results$model6.1.panelar.hartlepool.coef) |
    ##        !is.null(results$model6.1.panelar.hemel.coef) |
    ##        !is.null(results$model6.1.panelar.newark.coef) |
    ##        !is.null(results$model6.1.panelar.rochdale.coef) |
    ##        !is.null(results$model7.panelar.all.coef)){
    ##         results$model7.panelar.all <- combine_coefficients(bishop.coef     = results$model6.1.panelar.bishop.coef,
    ##                                                            hartlepool.coef = results$model6.1.panelar.hartlepool.coef,
    ##                                                            hemel.coef      = results$model6.1.panelar.hemel.coef,
    ##                                                            newark.coef     = results$model6.1.panelar.newark.coef,
    ##                                                            rochdale.coef   = results$model6.1.panelar.rochdale.coef,
    ##                                                            all.coef        = results$model7.panelar.all.coef)
    ##         ## ## Forest plot
    ##         results$model7.forest <- closed_forest(df.list       = list(results$model6.1.panelar.bishop.coef,
    ##                                                                     results$model6.1.panelar.hartlepool.coef,
    ##                                                                     results$model6.1.panelar.hemel.coef,
    ##                                                                     results$model6.1.panelar.newark.coef,
    ##                                                                     results$model6.1.panelar.rochdale.coef,
    ##                                                                     results$model7.panelar.all.coef),
    ##                                                plot.term     = c('closure'),
    ##                                                facet.outcome = FALSE,
    ##                                                title         = paste0('Model 6 & Model 7 : ',
    ##                                                                       indicator,
    ##                                                                       ' (',
    ##                                                                       sub.indicator,
    ##                                                                       ')'),
    ##                                                theme         = theme_bw())
    ##     }
    ##     ## Return model objects if requested
    ##     if(return.model == TRUE){
    ##         if(exists('model7.panelar')){
    ##             results$model7.panelar     <- model7.panelar
    ##         }
    ##     }
    ##     if(return.df == TRUE){
    ##         results$model7.df <- df7
    ##     }
    ##     if(return.residuals == TRUE){
    ##         if(exists('model7.panelar')){
    ##             results$model7.panelar.residuals.all     <- summary(model7.panelar)$residuals
    ##         }
    ##     }
    ##     ## Remove clutter
    ##     rm(df7)
    ## }
    #######################################################################
    ## Produce summary tables by center using results$summary.table.head ##
    ## and the coefficients from each model                              ##
    #######################################################################
    ## Bind all model results together
    ## model1.coef <- rbind(results$model1.panelar.bishop.coef,
    ##                      results$model1.panelar.hartlepool.coef,
    ##                      results$model1.panelar.hemel.coef,
    ##                      results$model1.panelar.newark.coef,
    ##                      results$model1.panelar.rochdale.coef)
    ## model1.coef$model <- 'Model 1'
    ## model2.coef <- rbind(results$model2.panelar.bishop.coef,
    ##                      results$model2.panelar.hartlepool.coef,
    ##                      results$model2.panelar.hemel.coef,
    ##                      results$model2.panelar.newark.coef,
    ##                      results$model2.panelar.rochdale.coef)
    ## model2.coef$model <- 'Model 2'
    ## model3.1.coef <- rbind(results$model3.1.panelar.bishop.coef,
    ##                        results$model3.1.panelar.hartlepool.coef,
    ##                        results$model3.1.panelar.hemel.coef,
    ##                        results$model3.1.panelar.newark.coef,
    ##                        results$model3.1.panelar.rochdale.coef)
    ## model3.1.coef$model <- 'Model 3.1'
    ## model3.2.coef <- rbind(results$model3.2.panelar.bishop.coef,
    ##                        results$model3.2.panelar.hartlepool.coef,
    ##                        results$model3.2.panelar.hemel.coef,
    ##                        results$model3.2.panelar.newark.coef,
    ##                        results$model3.2.panelar.rochdale.coef)
    ## model3.2.coef$model <- 'Model 3.2'
    ## model4.coef <- results$model4.panelar.all.coef
    ## model4.coef$model <- 'Model 4'
    ## model5.coef <- results$model5.panelar.all.coef
    ## model5.coef$model <- 'Model 5'
    ## model6.1.coef <- rbind(results$model6.1.panelar.bishop.coef,
    ##                        results$model6.1.panelar.hartlepool.coef,
    ##                        results$model6.1.panelar.hemel.coef,
    ##                        results$model6.1.panelar.newark.coef,
    ##                        results$model6.1.panelar.rochdale.coef)
    ## model6.1.coef$model <- 'Model 6.1'
    ## model6.2.coef <- rbind(results$model6.2.panelar.bishop.coef,
    ##                        results$model6.2.panelar.hartlepool.coef,
    ##                        results$model6.2.panelar.hemel.coef,
    ##                        results$model6.2.panelar.newark.coef,
    ##                        results$model6.2.panelar.rochdale.coef)
    ## model6.2.coef$model <- 'Model 6.2'
    ## model7.coef <- results$model7.panelar.all.coef
    ## model7.coef$model <- 'Model 7'
    ## ## Some model*.coef may not have any data though as the models weren't run
    ## ## make those NULL so the subsequent rbind() doesn't fail
    ## if(length(model1.coef) == 1) model1.coef     <- NULL
    ## if(length(model2.coef) == 1) model2.coef     <- NULL
    ## if(length(model3.1.coef) == 1) model3.1.coef <- NULL
    ## if(length(model3.2.coef) == 1) model3.2.coef <- NULL
    ## if(length(model4.coef) == 1) model4.coef     <- NULL
    ## if(length(model5.coef) == 1) model5.coef     <- NULL
    ## if(length(model6.1.coef) == 1) model6.1.coef <- NULL
    ## if(length(model6.2.coef) == 1) model6.2.coef <- NULL
    ## if(length(model7.coef) == 1) model7.coef     <- NULL
    ## ## Return all coefficients across models
    ## results$all.model.all.coef <- rbind(model1.coef,
    ##                                     model2.coef,
    ##                                     model3.1.coef,
    ##                                     model3.2.coef,
    ##                                     model4.coef,
    ##                                     model5.coef,
    ##                                     model6.1.coef,
    ##                                     model6.2.coef,
    ##                                     model7.coef) %>%
    ##                               as.data.frame()
    ## names(results$all.model.all.coef) <- gsub('site', 'town', names(results$all.model.all.coef))
    ## ## Subset out the closure coefficients and derive output variable/df to append to
    ## ## table header which contains the means
    ## results$all.model.closure.coef <- filter(results$all.model.all.coef,
    ##                                          term == 'closure')
    ## names(results$all.model.closure.coef) <- c('est', 'se', 't', 'p', 'term', 'town', 'indicator', 'sub.indicator', 'r2', 'model')
    ## results$all.model.closure.coef$lci <- results$all.model.closure.coef$est - (1.96 * results$all.model.closure.coef$se)
    ## results$all.model.closure.coef$uci <- results$all.model.closure.coef$est + (1.96 * results$all.model.closure.coef$se)
    ## results$all.model.closure.coef$estimate <- paste0(formatC(results$all.model.closure.coef$est, digits = digits, format = 'f'),
    ##                                                   ' (',
    ##                                                   formatC(results$all.model.closure.coef$lci, digits = digits, format = 'f'),
    ##                                                   '-',
    ##                                                   formatC(results$all.model.closure.coef$uci, digits = digits, format = 'f'),
    ##                                                   ') p = ',
    ##                                                   formatC(results$all.model.closure.coef$p, digits = digits, format = 'f'))
    ## results$summary.table.tail <- dplyr::select(results$all.model.closure.coef,
    ##                                             town,
    ##                                             model,
    ##                                             estimate)
    ## results$summary.table.tail$Before_mean.sd    <- NA
    ## results$summary.table.tail$Before_median.iqr <- results$summary.table.tail$model
    ## results$summary.table.tail$Before_min.max    <- NA
    ## results$summary.table.tail$After_mean.sd     <- NA
    ## results$summary.table.tail$After_median.iqr  <- NA
    ## results$summary.table.tail$After_min.max     <- results$summary.table.tail$estimate
    ## results$summary.table.tail <- dplyr::select(results$summary.table.tail,
    ##                                             town,
    ##                                             Before_mean.sd, Before_median.iqr, Before_min.max,
    ##                                             After_mean.sd, After_median.iqr, After_min.max)
    ## results$summary.table.tail$group <- results$summary.table.tail$town
    ## results$summary.table <- rbind(results$summary.table.head,
    ##                                results$summary.table.tail)
    ## ## Sort out indicators
    ## results$summary.table$town[grep('Model', results$summary.table$Before_median.iqr)]             <- NA
    ## results$summary.table$town[results$summary.table$Before_median.iqr == 'Model 1']               <- 'Estimated closure coefficients'
    ## results$summary.table$Before_min.max[results$summary.table$Before_median.iqr == 'Model 1']     <- 'Individual Case Site'
    ## results$summary.table$After_mean.sd[results$summary.table$Before_median.iqr == 'Model 1']      <- 'No Control'
    ## results$summary.table$After_median.iqr[results$summary.table$Before_median.iqr == 'Model 1']   <- 'ED Panel'
    ## results$summary.table$Before_min.max[results$summary.table$Before_median.iqr == 'Model 2']     <- 'Individual Case Site'
    ## results$summary.table$After_mean.sd[results$summary.table$Before_median.iqr == 'Model 2']      <- 'Primary Control'
    ## results$summary.table$After_median.iqr[results$summary.table$Before_median.iqr == 'Model 2']   <- 'ED Panel'
    ## results$summary.table$Before_min.max[results$summary.table$Before_median.iqr == 'Model 3.1']    <- 'Individual Case Site'
    ## results$summary.table$After_mean.sd[results$summary.table$Before_median.iqr == 'Model 3.1']    <- 'All Control'
    ## results$summary.table$After_median.iqr[results$summary.table$Before_median.iqr == 'Model 3.1'] <- 'ED Panel'
    ## results$summary.table$Before_min.max[results$summary.table$Before_median.iqr == 'Model 3.2']    <- 'Individual Case Site'
    ## results$summary.table$After_mean.sd[results$summary.table$Before_median.iqr == 'Model 3.2']    <- 'All Controls Pooled'
    ## results$summary.table$After_median.iqr[results$summary.table$Before_median.iqr == 'Model 3.2'] <- 'ED Panel'
    ## results$summary.table$Before_min.max[results$summary.table$Before_median.iqr == 'Model 4']     <- 'All Case Sites'
    ## results$summary.table$After_mean.sd[results$summary.table$Before_median.iqr == 'Model 4']      <- 'Primary Control'
    ## results$summary.table$After_median.iqr[results$summary.table$Before_median.iqr == 'Model 4']   <- 'ED Panel'
    ## results$summary.table$Before_min.max[results$summary.table$Before_median.iqr == 'Model 5']     <- 'All Case Sites'
    ## results$summary.table$After_mean.sd[results$summary.table$Before_median.iqr == 'Model 5']      <- 'All Controls'
    ## results$summary.table$After_median.iqr[results$summary.table$Before_median.iqr == 'Model 5']   <- 'ED Panel'
    ## results$summary.table$Before_min.max[results$summary.table$Before_median.iqr == 'Model 6.1']    <- 'Individual Case Site'
    ## results$summary.table$After_mean.sd[results$summary.table$Before_median.iqr == 'Model 6.1']    <- 'None'
    ## results$summary.table$After_median.iqr[results$summary.table$Before_median.iqr == 'Model 6.1'] <- 'LSOA Panel'
    ## results$summary.table$Before_min.max[results$summary.table$Before_median.iqr == 'Model 6.2']    <- 'Individual Case Site'
    ## results$summary.table$After_mean.sd[results$summary.table$Before_median.iqr == 'Model 6.2']    <- 'Primary Control'
    ## results$summary.table$After_median.iqr[results$summary.table$Before_median.iqr == 'Model 6.2'] <- 'LSOA Panel'
    ## results$summary.table$Before_min.max[results$summary.table$Before_median.iqr == 'Model 7']     <- 'All Case Sites'
    ## results$summary.table$After_mean.sd[results$summary.table$Before_median.iqr == 'Model 7']      <- 'All Controls'
    ## results$summary.table$After_median.iqr[results$summary.table$Before_median.iqr == 'Model 7']   <- 'LSOA Panel'
    ## ## Site specific tables
    ## ## Bishop Auckland
    ## results$summary.table.bishop <- filter(results$summary.table,
    ##                                        group %in% c('Bishop Auckland', 'All')) %>%
    ##                                 dplyr::select(town, Before_mean.sd, Before_median.iqr, Before_min.max, After_mean.sd, After_median.iqr, After_min.max)
    ## names(results$summary.table.bishop) <- c('Town', 'Pre Mean (SD)', 'Pre Median (IQR)', 'Pre Range', 'Post Mean (SD)', 'Post Median (IQR)', 'Post Range')
    ## ## Hartlepool
    ## results$summary.table.hartlepool <- filter(results$summary.table,
    ##                                                  group %in% c('Hartlepool', 'All')) %>%
    ##                                 dplyr::select(town, Before_mean.sd, Before_median.iqr, Before_min.max, After_mean.sd, After_median.iqr, After_min.max)
    ## names(results$summary.table.hartlepool) <- c('Town', 'Pre Mean (SD)', 'Pre Median (IQR)', 'Pre Range', 'Post Mean (SD)', 'Post Median (IQR)', 'Post Range')
    ## ## Hemel Hempstead
    ## results$summary.table.hemel <- filter(results$summary.table,
    ##                                       group %in% c('Hemel Hempstead', 'All')) %>%
    ##                                 dplyr::select(town, Before_mean.sd, Before_median.iqr, Before_min.max, After_mean.sd, After_median.iqr, After_min.max)
    ## names(results$summary.table.hemel) <- c('Town', 'Pre Mean (SD)', 'Pre Median (IQR)', 'Pre Range', 'Post Mean (SD)', 'Post Median (IQR)', 'Post Range')
    ## ## Newark
    ## results$summary.table.newark <- filter(results$summary.table,
    ##                                        group %in% c('Newark', 'All')) %>%
    ##                                 dplyr::select(town, Before_mean.sd, Before_median.iqr, Before_min.max, After_mean.sd, After_median.iqr, After_min.max)
    ## names(results$summary.table.newark) <- c('Town', 'Pre Mean (SD)', 'Pre Median (IQR)', 'Pre Range', 'Post Mean (SD)', 'Post Median (IQR)', 'Post Range')
    ## ## Rochdale
    ## results$summary.table.rochdale <- filter(results$summary.table,
    ##                                          group %in% c('Rochdale', 'All')) %>%
    ##                                 dplyr::select(town, Before_mean.sd, Before_median.iqr, Before_min.max, After_mean.sd, After_median.iqr, After_min.max)
    ## names(results$summary.table.rochdale) <- c('Town', 'Pre Mean (SD)', 'Pre Median (IQR)', 'Pre Range', 'Post Mean (SD)', 'Post Median (IQR)', 'Post Range')
    #######################################################################
    ## Return the results                                                ##
    #######################################################################
    return(results)
}
