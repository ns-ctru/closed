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
#' @param autocorr panelAR() option for handling auto-correlation, default is \code{ar1}.
#' @param panelcorrmethod panelAR() option for panel correction, default is \code{pcse}.
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
                          model1          = c('closure', 'season', 'relative.month'), ## ToDo - Add other steps when available
                          model2          = c('town * closure', 'season', 'relative.month'),
                          model3          = c('town * closure', 'season', 'relative.month', 'diff.time.to.ed'),
                          model4          = c('town * closure', 'season', 'relative.month', 'diff.time.to.ed'),
                          model5          = c('town * closure', 'season', 'relative.month'),
                          model6          = c(),
                          autocorr        = 'ar1',
                          panelcorrmethod = 'pcse',
                          plot            = TRUE,
                          common.y        = TRUE,
                          theme           = theme_bw(),
                          return.model    = FALSE,
                          html            = FALSE,
                          ...){
    #######################################################################
    ## Set up (results, formula, renaming variables)                     ##
    #######################################################################
    ## Initialise results list for returning everything
    results <- list()
    ## Convert variable names for ease of typing within this function
    ## (ESS artefact, hitting underscore inserts '<-' so lots of underscores are
    ## tedious to type)
    names(df.lsoa)  <- names(df.lsoa) %>%
                       gsub("_", ".", x = .)
    names(df.trust) <- names(df.trust) %>%
                       gsub("_", ".", x = .)
    ## Convert to data frame, selecting only the specified outcome
    df.lsoa  <- as.data.frame(df.lsoa) %>%
                dplyr::filter(measure == indicator,
                              sub.measure == sub.indicator)
    df.trust <- as.data.frame(df.trust) %>%
                dplyr::filter(measure == indicator,
                              sub.measure == sub.indicator)
    ## df.steps <- as.data.frame(df.steps)
    ## names(df.steps) <- names(df.steps) %>%
    ##                    gsub("_", ".", x = .)
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
    df.lsoa$closure  <- ifelse(df.lsoa$relative.month  >= 24, 1, 0)
    df.trust$closure <- ifelse(df.trust$relative.month >= 24, 1, 0)
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
                                     all.coef        = NULL){
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
                           formatC(.coef$p, digits = 4, format = 'e'))
        .coef <- dplyr::select(.coef, indicator, sub.indicator, term, site, out) %>%
                 melt(id = c('indicator', 'sub.indicator', 'site', 'term')) %>%
                 dcast(indicator + sub.indicator + term ~ site + variable)
        ## Format the term label for interactions between site and town
        .coef <- within(.coef, {
                        term[term == 'townBishop Auckland'] <- 'Bishop Auckland'
                        term[term == 'townBishop Auckland:closure'] <- 'Bishop Auckland x Closure'
                        term[term == 'townHartlepool'] <- 'Hartlepool'
                        term[term == 'townHartlepool:closure'] <- 'Hartlepool x Closure'
                        term[term == 'townHemel Hempstead'] <- 'Hemel Hempstead'
                        term[term == 'townHemel Hempstead:closure'] <- 'Hemel Hempstead x Closure'
                        term[term == 'townNewark']  <- 'Newark'
                        term[term == 'townNewark:closure']  <- 'Newark x Closure'
                        term[term == 'townRochdale']  <- 'Rochdale'
                        term[term == 'townRochdale:closure']  <- 'Rochdale x Closure'
                       })
        ## Combine with r2
        names(.coef) <- gsub("_out", "", names(.coef))
        names(.r2) <- gsub("variable", "term", names(.r2))
        coef$coef <- rbind(.coef, .r2)
        rm(.r2)
        names(coef$coef) <- c('Indicator', 'Subindicator', 'Term', 'Bishop Auckland', 'Hartlepool', 'Hemel Hempstead', 'Newark', 'Rochdale')
        ## Derive a caption for the table
        coef$caption <- paste0('Comparison of coefficients across sites.  Each cell contains a point estimates followed by the standard error (in brackets) and the associated p-value.')
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
                      measure     == indicator,
                      sub.measure == sub.indicator)
        ## Generate time-series plot
        results$model1.ts.plot <- ggplot(data = df1,
                                         mapping = aes(x     = relative.month,
                                                       y     = value,
                                                       color = town)) +
                                  geom_line() +
                                  geom_vline(xintercept = 24, linetype = 4) +
                                  ## ToDo - Include other steps
                                  labs(list(title  = paste0(title1, title2),
                                            x      = 'Month (Aligned)',
                                            y      = 'N',
                                            colour = 'Hospital')) +
                                  geom_text_repel(data = filter(df1, relative.month == 3),
                                                  aes(relative.month,
                                                      value,
                                                      colour = town,
                                                      label  = town),
                                                  force   = 1,
                                                  nudge_x = 0,
                                                  nudge_y = 600) +
                                  theme(legend.position = 'none')
        ## Apply the user-specified theme
        if(!is.null(theme)){
            results$model1.ts.plot <- results$model1.ts.plot + theme +
                                      theme(legend.position = 'none')
        }
        ##################################################
        ## Bishop Auckland                              ##
        ##################################################
        model1.panelar.bishop <- filter(df1,
                                        town        == 'Bishop Auckland') %>%
                                 panelAR(formula  = formula.model1,
                                         timeVar  = timevar,
                                         panelVar = panel.trust,
                                         autoCorr = autocorr,
                                         panelCorrMethod = panelcorrmethod)
        results$model1.panelar.bishop.coef <- extract_coefficients(x              = model1.panelar.bishop,
                                                                   .site          = 'Bishop Auckland',
                                                                   .indicator     = indicator,
                                                                   .sub.indicator = sub.indicator)
        results$model1.panelar.bishop.r2 <- model1.panelar.bishop$r2
        ##################################################
        ## Hartlepool                                   ##
        ##################################################
        model1.panelar.hartlepool <- filter(df1,
                                            town        == 'Hartlepool') %>%
                                     panelAR(formula  = formula.model1,
                                             timeVar  = timevar,
                                             panelVar = panel.trust,
                                             autoCorr = autocorr,
                                             panelCorrMethod = panelcorrmethod)
        results$model1.panelar.hartlepool.coef <- extract_coefficients(x              = model1.panelar.hartlepool,
                                                                       .site          = 'Hartlepool',
                                                                       .indicator     = indicator,
                                                                       .sub.indicator = sub.indicator)
        results$model1.panelar.hartlepool.r2 <- model1.panelar.hartlepool$r2
        ##################################################
        ## Hemel Hempstead                              ##
        ##################################################
        model1.panelar.hemel <- filter(df1,
                                       town        == 'Hemel Hempstead') %>%
                                panelAR(formula  = formula.model1,
                                        timeVar  = timevar,
                                        panelVar = panel.trust,
                                        autoCorr = autocorr,
                                        panelCorrMethod = panelcorrmethod)
        results$model1.panelar.hemel.coef <- extract_coefficients(x              = model1.panelar.hemel,
                                                                  .site          = 'Hemel Hempstead',
                                                                  .indicator     = indicator,
                                                                  .sub.indicator = sub.indicator)
        results$model1.panelar.hemel.r2 <- model1.panelar.hemel$r2
        ##################################################
        ## Newark                              ##
        ##################################################
        model1.panelar.newark <- filter(df1,
                                        town        == 'Newark') %>%
                                  panelAR(formula  = formula.model1,
                                          timeVar  = timevar,
                                          panelVar = panel.trust,
                                          autoCorr = autocorr,
                                          panelCorrMethod = panelcorrmethod)
        results$model1.panelar.newark.coef <- extract_coefficients(x              = model1.panelar.newark,
                                                                   .site          = 'Newark',
                                                                   .indicator     = indicator,
                                                                   .sub.indicator = sub.indicator)
        results$model1.panelar.newark.r2 <- model1.panelar.newark$r2
        ##################################################
        ## Rochdale                              ##
        ##################################################
        results$model1.panelar.rochdale <- filter(df1,
                                                  town        == 'Rochdale') %>%
                                           panelAR(formula  = formula.model1,
                                                   timeVar  = timevar,
                                                   panelVar = panel.trust,
                                                   autoCorr = autocorr,
                                                   panelCorrMethod = panelcorrmethod)
        results$model1.panelar.rochdale.coef <- extract_coefficients(x            = results$model1.panelar.rochdale,
                                                                     .site          = 'Rochdale',
                                                                     .indicator     = indicator,
                                                                     .sub.indicator = sub.indicator)
        results$model1.panelar.rochdale.r2 <- results$model1.panelar.rochdale$r2
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
                      measure     == indicator,
                      sub.measure == sub.indicator)
        ## Generate time-series plot
        df2$group <- paste0('Cohort : ', df2$group)
        results$model2.ts.plot <- ggplot(data = df2,
                                         mapping = aes(x     = relative.month,
                                                       y     = value,
                                                       color = town)) +
                                  geom_line() +
                                  geom_vline(xintercept = 24, linetype = 4) +
                                  ## ToDo - Include other steps
                                  labs(list(title  = paste0(title1, title2),
                                            x      = 'Month (Aligned)',
                                            y      = 'N',
                                            colour = 'Hospital')) +
                                  facet_wrap(~ group, ncol = 1) +
                                  geom_text_repel(data = filter(df2, relative.month == 3),
                                                  aes(relative.month,
                                                      value,
                                                      colour = town,
                                                      label  = town),
                                                  force   = 1,
                                                  nudge_x = 0,
                                                  nudge_y = 600) +
                                  theme(legend.position = 'none')
        ## Apply the user-specified theme
        if(!is.null(theme)){
            results$model2.ts.plot <- results$model2.ts.plot + theme +
                                      theme(legend.position = 'none')
        }
        ## Perform analysis with panelAR in each
        ##################################################
        ## Bishop Auckland                              ##
        ##################################################
        ## ToDo - Add in the other steps when available to both the data and the formula
        ## .formula.model2 <- reformulate(response = outcome,
        ##                                termlabels = c(model2, ###))
        model2.panelar.bishop <- filter(df2,
                                        town == 'Bishop Auckland' |
                                        town == 'Whitehaven') %>%
                                 panelAR(formula  = formula.model2,
                                         timeVar  = timevar,
                                         panelVar = panel.trust,
                                         autoCorr = autocorr,
                                         panelCorrMethod = panelcorrmethod)
        results$model2.panelar.bishop.coef <- extract_coefficients(x              = model2.panelar.bishop,
                                                                   .site          = 'Bishop Auckland',
                                                                   .indicator     = indicator,
                                                                   .sub.indicator = sub.indicator)
        results$model2.panelar.bishop.r2 <- model2.panelar.bishop$r2
        ##################################################
        ## Hartlepool                                   ##
        ##################################################
        model2.panelar.hartlepool <- filter(df2,
                                            town == 'Hartlepool' |
                                            town == 'Grimsby') %>%
                                     panelAR(formula  = formula.model2,
                                             timeVar  = timevar,
                                             panelVar = panel.trust,
                                             autoCorr = autocorr,
                                             panelCorrMethod = panelcorrmethod)
        results$model2.panelar.hartlepool.coef <- extract_coefficients(x             = model2.panelar.hartlepool,
                                                                      .site          = 'Hartlepool',
                                                                      .indicator     = indicator,
                                                                      .sub.indicator = sub.indicator)
        results$model2.panelar.hartlepool.r2 <- model2.panelar.hartlepool$r2
        ##################################################
        ## Hemel Hempstead                              ##
        ##################################################
        model2.panelar.hemel <- filter(df2,
                                       town == 'Hemel Hempstead' |
                                       town == 'Warwick') %>%
                                panelAR(formula  = formula.model2,
                                        timeVar  = timevar,
                                        panelVar = panel.trust,
                                        autoCorr = autocorr,
                                        panelCorrMethod = panelcorrmethod)
        results$model2.panelar.hemel.coef <- extract_coefficients(x              = model2.panelar.hemel,
                                                                  .site          = 'Hemel Hempstead',
                                                                  .indicator     = indicator,
                                                                  .sub.indicator = sub.indicator)
        results$model2.panelar.hemel.r2 <- model2.panelar.hemel$r2
        ##################################################
        ## Newark                                       ##
        ##################################################
        model2.panelar.newark <- filter(df2,
                                        town == 'Newark' |
                                        town == 'Southport') %>%
                                 panelAR(formula  = formula.model2,
                                         timeVar  = timevar,
                                         panelVar = panel.trust,
                                         autoCorr = autocorr,
                                         panelCorrMethod = panelcorrmethod)
        results$model2.panelar.newark.coef <- extract_coefficients(x              = model2.panelar.newark,
                                                                   .site          = 'Newark',
                                                                   .indicator     = indicator,
                                                                   .sub.indicator = sub.indicator)
        results$model2.panelar.newark.r2 <- model2.panelar.newark$r2
        ##################################################
        ## Rochdale                                       ##
        ##################################################
        model2.panelar.rochdale <- filter(df2,
                                          town == 'Rochdale' |
                                          town == 'Rotherham') %>%
                                   panelAR(formula  = formula.model2,
                                           timeVar  = timevar,
                                           panelVar = panel.trust,
                                           autoCorr = autocorr,
                                           panelCorrMethod = panelcorrmethod)
        results$model2.panelar.rochdale.coef <- extract_coefficients(x            = model2.panelar.rochdale,
                                                                   .site          = 'Rochdale',
                                                                   .indicator     = indicator,
                                                                   .sub.indicator = sub.indicator)
        results$model2.panelar.rochdale.r2 <- model2.panelar.rochdale$r2
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
        ## Remove clutter
        rm(df2)
    }
    #######################################################################
    ## Model 2.5                                                           ##
    #######################################################################
    if(!is.null(model2.5)){
        ## Reformulate outcome and covariates
        formula.model2.5 <- reformulate(response = outcome,
                                      termlabels = model2.5)
        ## Subset data
        sites <- c('Bishop Auckland', 'Whitehaven',
                   'Hartlepool', 'Grimsby',
                   'Hemel Hempstead', 'Warwick',
                   'Newark', 'Southport',
                   'Rochdale', 'Rotherham')
        df2.5 <- filter(df.trust, town %in% sites &
                      measure     == indicator,
                      sub.measure == sub.indicator)
        ## Generate time-series plot
        df2.5$group <- paste0('Cohort : ', df2.5$group)
        results$model2.5.ts.plot <- ggplot(data = df2.5,
                                         mapping = aes(x     = relative.month,
                                                       y     = value,
                                                       color = town)) +
                                  geom_line() +
                                  geom_vline(xintercept = 24, linetype = 4) +
                                  ## ToDo - Include other steps
                                  labs(list(title  = paste0(title1, title2),
                                            x      = 'Month (Aligned)',
                                            y      = 'N',
                                            colour = 'Hospital')) +
                                  facet_wrap(~ group, ncol = 1) +
                                  geom_text_repel(data = filter(df2.5, relative.month == 3),
                                                  aes(relative.month,
                                                      value,
                                                      colour = town,
                                                      label  = town),
                                                  force   = 1,
                                                  nudge_x = 0,
                                                  nudge_y = 600) +
                                  theme(legend.position = 'none')
        ## Apply the user-specified theme
        if(!is.null(theme)){
            results$model2.5.ts.plot <- results$model2.5.ts.plot + theme +
                                      theme(legend.position = 'none')
        }
        ## Perform analysis with panelAR in each
        ##################################################
        ## Bishop Auckland                              ##
        ##################################################
        ## ToDo - Add in the other steps when available to both the data and the formula
        ## .formula.model2.5 <- reformulate(response = outcome,
        ##                                termlabels = c(model2.5, ###))
        model2.5.panelar.bishop <- filter(df2.5,
                                        town == 'Bishop Auckland' |
                                        town == 'Whitehaven') %>%
                                 panelAR(formula  = formula.model2.5,
                                         timeVar  = timevar,
                                         panelVar = panel.trust,
                                         autoCorr = autocorr,
                                         panelCorrMethod = panelcorrmethod)
        results$model2.5.panelar.bishop.coef <- extract_coefficients(x              = model2.5.panelar.bishop,
                                                                   .site          = 'Bishop Auckland',
                                                                   .indicator     = indicator,
                                                                   .sub.indicator = sub.indicator)
        results$model2.5.panelar.bishop.r2 <- model2.5.panelar.bishop$r2
        ##################################################
        ## Hartlepool                                   ##
        ##################################################
        model2.5.panelar.hartlepool <- filter(df2.5,
                                            town == 'Hartlepool' |
                                            town == 'Grimsby') %>%
                                     panelAR(formula  = formula.model2.5,
                                             timeVar  = timevar,
                                             panelVar = panel.trust,
                                             autoCorr = autocorr,
                                             panelCorrMethod = panelcorrmethod)
        results$model2.5.panelar.hartlepool.coef <- extract_coefficients(x             = model2.5.panelar.hartlepool,
                                                                      .site          = 'Hartlepool',
                                                                      .indicator     = indicator,
                                                                      .sub.indicator = sub.indicator)
        results$model2.5.panelar.hartlepool.r2 <- model2.5.panelar.hartlepool$r2
        ##################################################
        ## Hemel Hempstead                              ##
        ##################################################
        model2.5.panelar.hemel <- filter(df2.5,
                                       town == 'Hemel Hempstead' |
                                       town == 'Warwick') %>%
                                panelAR(formula  = formula.model2.5,
                                        timeVar  = timevar,
                                        panelVar = panel.trust,
                                        autoCorr = autocorr,
                                        panelCorrMethod = panelcorrmethod)
        results$model2.5.panelar.hemel.coef <- extract_coefficients(x              = model2.5.panelar.hemel,
                                                                  .site          = 'Hemel Hempstead',
                                                                  .indicator     = indicator,
                                                                  .sub.indicator = sub.indicator)
        results$model2.5.panelar.hemel.r2 <- model2.5.panelar.hemel$r2
        ##################################################
        ## Newark                                       ##
        ##################################################
        model2.5.panelar.newark <- filter(df2.5,
                                        town == 'Newark' |
                                        town == 'Southport') %>%
                                 panelAR(formula  = formula.model2.5,
                                         timeVar  = timevar,
                                         panelVar = panel.trust,
                                         autoCorr = autocorr,
                                         panelCorrMethod = panelcorrmethod)
        results$model2.5.panelar.newark.coef <- extract_coefficients(x              = model2.5.panelar.newark,
                                                                   .site          = 'Newark',
                                                                   .indicator     = indicator,
                                                                   .sub.indicator = sub.indicator)
        results$model2.5.panelar.newark.r2 <- model2.5.panelar.newark$r2
        ##################################################
        ## Rochdale                                       ##
        ##################################################
        model2.5.panelar.rochdale <- filter(df2.5,
                                          town == 'Rochdale' |
                                          town == 'Rotherham') %>%
                                   panelAR(formula  = formula.model2.5,
                                           timeVar  = timevar,
                                           panelVar = panel.trust,
                                           autoCorr = autocorr,
                                           panelCorrMethod = panelcorrmethod)
        results$model2.5.panelar.rochdale.coef <- extract_coefficients(x            = model2.5.panelar.rochdale,
                                                                   .site          = 'Rochdale',
                                                                   .indicator     = indicator,
                                                                   .sub.indicator = sub.indicator)
        results$model2.5.panelar.rochdale.r2 <- model2.5.panelar.rochdale$r2
        ## Summary table
        results$model2.5.panelar <- combine_coefficients(bishop.coef     = results$model2.5.panelar.bishop.coef,
                                                       hartlepool.coef = results$model2.5.panelar.hartlepool.coef,
                                                       hemel.coef      = results$model2.5.panelar.hemel.coef,
                                                       newark.coef     = results$model2.5.panelar.newark.coef,
                                                       rochdale.coef   = results$model2.5.panelar.rochdale.coef)
        ## Forest plot
        results$model2.5.forest <- closed_forest(df.list       = list(results$model2.5.panelar.bishop.coef,
                                                                    results$model2.5.panelar.hartlepool.coef,
                                                                    results$model2.5.panelar.hemel.coef,
                                                                    results$model2.5.panelar.newark.coef,
                                                                    results$model2.5.panelar.rochdale.coef),
                                               plot.term     = c('closure'),
                                               facet.outcome = FALSE,
                                               title         = paste0('Model 2.5 : ',
                                                                      indicator,
                                                                      ' (',
                                                                      sub.indicator,
                                                                      ')'),
                                               theme         = theme_bw())
        ## Return model objects if requested
        if(return.model == TRUE){
            results$model2.5.panelar.bishop     <- model2.5.panelar.bishop
            results$model2.5.panelar.hartlepool <- model2.5.panelar.hartlepool
            results$model2.5.panelar.hemel      <- model2.5.panelar.hemel
            results$model2.5.panelar.newark     <- model2.5.panelar.newark
            results$model2.5.panelar.rochdale   <- model2.5.panelar.rochdale
        }
        ## Remove clutter
        rm(df2.5)
    }
    #######################################################################
    ## Model 3                                                           ##
    #######################################################################
    if(!is.null(model3)){
        ## Reformulate outcome and covariates
        formula.model3 <- reformulate(response = outcome,
                                      termlabels = model3)
        ## Subset data
        sites <- c('Bishop Auckland', 'Whitehaven',
                   'Hartlepool', 'Grimsby',
                   'Hemel Hempstead', 'Warwick',
                   'Newark', 'Southport',
                   'Rochdale', 'Rotherham')
        df3 <- filter(df.lsoa, town %in% sites)##  &
                      ## measure     == indicator,
                      ## sub.measure == sub.indicator)
        df3.trust <- filter(df.trust, town %in% sites)##  &
                            ## measure     == indicator,
                            ## sub.measure == sub.indicator)
        ## Add in indicator of case/control status for plotting
        case <- c('Bishop Auckland',
                  'Hartlepool',
                  'Hemel Hempstead',
                  'Newark',
                  'Rochdale')
        df3$status <- ifelse(df3$town %in% case, 'Case', 'Control')
        ## Generate time-series plot (at site/town level)
        df3.trust$group <- paste0('Cohort : ', df3.trust$group)
        results$model3.ts.plot.trust <- ggplot(data = df3.trust,
                                               mapping = aes(x     = relative.month,
                                                             y     = value,
                                                             color = town)) +
                                        geom_line() +
                                        geom_vline(xintercept = 24, linetype = 4) +
                                        ## ToDo - Include other steps
                                        labs(list(title  = paste0(title1, title2),
                                                  x      = 'Month (Aligned)',
                                                  y      = 'N',
                                                  colour = 'Hospital')) +
                                        facet_wrap(~ group, ncol = 1) +
                                  geom_text_repel(data = filter(df3.trust, relative.month == 3),
                                                  aes(relative.month,
                                                      value,
                                                      colour = town,
                                                      label  = town),
                                                  force   = 1,
                                                  nudge_x = 0,
                                                  nudge_y = 600) +
                                  theme(legend.position = 'none')
        ## Generate time-series plot (at lsoa level)
        results$model3.ts.plot.lsoa <-  ggplot(data = df3,
                                               mapping = aes(x     = relative.month,
                                                             y     = value,
                                                             color = lsoa)) +
                                        geom_line() +
                                        geom_vline(xintercept = 24, linetype = 4) +
                                        ## ToDo - Include other steps
                                        labs(list(title  = paste0(title1, title2),
                                                  x      = 'Month (Aligned)',
                                                  y      = 'N')) +
                                        theme(legend.position = 'none') +
                                        facet_wrap( ~ town + status, ncol = 2)
        ## Apply the user-specified theme
        if(!is.null(theme)){
            results$model3.ts.plot.trust <- results$model3.ts.plot.trust + theme +
                                            theme(legend.position = 'none')
            results$model3.ts.plot.lsoa  <- results$model3.ts.plot.lsoa + theme + theme(legend.position = 'none')
        }
        ## Perform analysis with panelAR in each
        ##################################################
        ## Bishop Auckland                              ##
        ##################################################
        ## ToDo - Add in the other steps when available to both the data and the formula
        ## .formula.model3 <- reformulate(response = outcome,
        ##                                termlabels = c(model3, ###))
        model3.panelar.bishop <- filter(df3,
                                        town == 'Bishop Auckland' |
                                        town == 'Whitehaven') %>%
                                 panelAR(formula  = formula.model3,
                                         timeVar  = timevar,
                                         panelVar = panel.lsoa,
                                         autoCorr = autocorr,
                                         panelCorrMethod = panelcorrmethod)
        results$model3.panelar.bishop.coef <- extract_coefficients(x              = model3.panelar.bishop,
                                                                   .site          = 'Bishop Auckland',
                                                                   .indicator     = indicator,
                                                                   .sub.indicator = sub.indicator)
        results$model3.panelar.bishop.r2 <- model3.panelar.bishop$r2
        ##################################################
        ## Hartlepool                                   ##
        ##################################################
        model3.panelar.hartlepool <- filter(df3,
                                            town == 'Hartlepool' |
                                            town == 'Grimsby') %>%
                                     panelAR(formula  = formula.model3,
                                             timeVar  = timevar,
                                             panelVar = panel.lsoa,
                                             autoCorr = autocorr,
                                             panelCorrMethod = panelcorrmethod)
        results$model3.panelar.hartlepool.coef <- extract_coefficients(x             = model3.panelar.hartlepool,
                                                                      .site          = 'Hartlepool',
                                                                      .indicator     = indicator,
                                                                      .sub.indicator = sub.indicator)
        results$model3.panelar.hartlepool.r2 <- model3.panelar.hartlepool$r2
        ##################################################
        ## Hemel Hempstead                              ##
        ##################################################
        model3.panelar.hemel <- filter(df3,
                                       town == 'Hemel Hempstead' |
                                       town == 'Warwick') %>%
                                panelAR(formula  = formula.model3,
                                        timeVar  = timevar,
                                        panelVar = panel.lsoa,
                                        autoCorr = autocorr,
                                        panelCorrMethod = panelcorrmethod)
        results$model3.panelar.hemel.coef <- extract_coefficients(x              = model3.panelar.hemel,
                                                                  .site          = 'Hemel Hempstead',
                                                                  .indicator     = indicator,
                                                                  .sub.indicator = sub.indicator)
        results$model3.panelar.hemel.r2 <- model3.panelar.hemel$r2
        ##################################################
        ## Newark                                       ##
        ##################################################
        model3.panelar.newark <- filter(df3,
                                        town == 'Newark' |
                                        town == 'Southport') %>%
                                 panelAR(formula  = formula.model3,
                                         timeVar  = timevar,
                                         panelVar = panel.lsoa,
                                         autoCorr = autocorr,
                                         panelCorrMethod = panelcorrmethod)
        results$model3.panelar.newark.coef <- extract_coefficients(x              = model3.panelar.newark,
                                                                   .site          = 'Newark',
                                                                   .indicator     = indicator,
                                                                   .sub.indicator = sub.indicator)
        results$model3.panelar.newark.r2 <- model3.panelar.newark$r2
        ##################################################
        ## Rochdale                                       ##
        ##################################################
        model3.panelar.rochdale <- filter(df3,
                                          town == 'Rochdale' |
                                          town == 'Rotherham') %>%
                                   panelAR(formula  = formula.model3,
                                           timeVar  = timevar,
                                           panelVar = panel.lsoa,
                                           autoCorr = autocorr,
                                           panelCorrMethod = panelcorrmethod)
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
        ## sites <- c('Bishop Auckland', 'Whitehaven',
        ##            'Hartlepool', 'Grimsby',
        ##            'Hemel Hempstead', 'Warwick',
        ##            'Newark', 'Southport',
        ##            'Rochdale', 'Rotherham')
        ## df4 <- filter(df.lsoa, town %in% sites &
        ##               measure     == indicator,
        ##               sub.measure == sub.indicator)
        ## ToDo - Remove duplicates (based on lsoa and relative.time)
        ##
        df4.lsoa <- df.lsoa
        ## check <- dplyr::select(df4, lsoa, relative.month)
        ## df4.lsoa <- df4.lsoa[!duplicated(check),]
        ## OR   - Derive new indicator based on LSOA and Town
        df4.lsoa$town.lsoa <- paste0(df4.lsoa$town, df4$lsoa, sep = '-')
        df4.trust <- df.trust
        ## Add in indicator of case/control status for plotting
        case <- c('Bishop Auckland',
                  'Hartlepool',
                  'Hemel Hempstead',
                  'Newark',
                  'Rochdale')
        df4$status <- ifelse(df4$town %in% case, 'Case', 'Control')
        ## Generate time-series plot (at site/town level)
        df4.trust$group <- paste0('Cohort : ', df4.trust$group)
        results$model4.ts.plot.trust <- ggplot(data = df4.trust,
                                               mapping = aes(x     = relative.month,
                                                             y     = value,
                                                             color = town)) +
                                        geom_line() +
                                        geom_vline(xintercept = 24, linetype = 4) +
                                        ## ToDo - Include other steps
                                        labs(list(title  = paste0(title1, title2),
                                                  x      = 'Month (Aligned)',
                                                  y      = 'N',
                                                  colour = 'Hospital')) +
                                        facet_wrap(~ group, ncol = 1) +
                                        geom_text_repel(data = filter(df4.trust, relative.month == 3),
                                                  aes(relative.month,
                                                      value,
                                                      colour = town,
                                                      label  = town),
                                                  force   = 1,
                                                  nudge_x = 0,
                                                  nudge_y = 600) +
                                        theme(legend.position = 'none')
        ## Apply the user-specified theme
        if(!is.null(theme)){
            results$model4.ts.plot.trust <- results$model4.ts.plot.trust + theme +
                                            theme(legend.position = 'none')
        }
        ## Perform analysis with panelAR in each
        ##################################################
        ## All sites                                    ##
        ##################################################
        ## ToDo - Add in the other steps when available to both the data and the formula
        ## .formula.model4 <- reformulate(response = outcome,
        ##                                termlabels = c(model4, ###))
        ## results$df4 <- filter(df4,
        ##                       measure     == indicator,
        ##                       sub.measure == sub.indicator)
        ## model4.panelar.all <- filter(df4,
        ##                              measure     == indicator,
        ##                              sub.measure == sub.indicator) %>%
        ##                       panelAR(formula  = formula.model4,
        ##                               timeVar  = timevar,
        ##                               panelVar = 'town.lsoa',
        ##                               autoCorr = autocorr,
        ##                               panelCorrMethod = panelcorrmethod)
        ## results$model4.panelar.coef <- extract_coefficients(x              = model4.panelar.all,
        ##                                                     .site          = 'All',
        ##                                                     .indicator     = indicator,
        ##                                                     .sub.indicator = sub.indicator)
        ## results$model4.panelar.r2 <- model4.panelar.all
        ## ## Summary table
        ## results$model4.panelar <- results$model4.panelar.coef
        ## results$model4.panelar <- combine_coefficients(bishop.coef     = results$model4.panelar.bishop.coef,
        ##                                                hartlepool.coef = results$model4.panelar.hartlepool.coef,
        ##                                                hemel.coef      = results$model4.panelar.hemel.coef,
        ##                                                newark.coef     = results$model4.panelar.newark.coef,
        ##                                                rochdale.coef   = results$model4.panelar.rochdale.coef)
        ## ## Forest plot
        ## results$model4.forest <- closed_forest(df.list       = list(results$model4.panelar.bishop.coef,
        ##                                                             results$model4.panelar.hartlepool.coef,
        ##                                                             results$model4.panelar.hemel.coef,
        ##                                                             results$model4.panelar.newark.coef,
        ##                                                             results$model4.panelar.rochdale.coef),
        ##                                        plot.term     = c('closure'),
        ##                                        facet.outcome = FALSE,
        ##                                        title         = paste0('Model 4 : ',
        ##                                                               indicator,
        ##                                                               ' (',
        ##                                                               sub.indicator,
        ##                                                               ')'),
        ##                                        theme         = theme_bw())
        ## Return model objects if requested
        if(return.model == TRUE){
            results$model4.panelar.all     <- model4.panelar.all
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
        ## df5 <- filter(df.trust, town %in% sites &
        ##                     measure     == indicator,
        ##                     sub.measure == sub.indicator)
        df5 <- df.trust
        ## Add in indicator of case/control status for plotting
        case <- c('Bishop Auckland',
                  'Hartlepool',
                  'Hemel Hempstead',
                  'Newark',
                  'Rochdale')
        df5$status <- ifelse(df5$town %in% case, 'Case', 'Control')
        results$df5 <- df5
        ## Generate time-series plot (at site/town level)
        df5$group <- paste0('Cohort : ', df5$group)
        results$model5.ts.plot.trust <- ggplot(data = df5,
                                               mapping = aes(x     = relative.month,
                                                             y     = value,
                                                             color = town)) +
                                        geom_line() +
                                        geom_vline(xintercept = 24, linetype = 4) +
                                        ## ToDo - Include other steps
                                        labs(list(title  = paste0(title1, title2),
                                                  x      = 'Month (Aligned)',
                                                  y      = 'N',
                                                  colour = 'Hospital')) +
                                        facet_wrap(~ group, ncol = 1) +
                                        geom_text_repel(data = filter(df5, relative.month == 3),
                                                  aes(relative.month,
                                                      value,
                                                      colour = town,
                                                      label  = town),
                                                  force   = 1,
                                                  nudge_x = 0,
                                                  nudge_y = 600) +
                                        theme(legend.position = 'none')
        ## Apply the user-specified theme
        if(!is.null(theme)){
            results$model5.ts.plot.trust <- results$model5.ts.plot.trust + theme +
                                            theme(legend.position = 'none')
        }
        ## Perform analysis with panelAR in each
        ##################################################
        ## All sites                                    ##
        ##################################################
        ## ToDo - Add in the other steps when available to both the data and the formula
        .formula.model5 <- reformulate(response = outcome,
                                       termlabels = c(model5))
        model5.panelar.all <- filter(df5,
                                     measure     == indicator,
                                     sub.measure == sub.indicator) %>%
                              panelAR(formula  = formula.model5,
                                      timeVar  = timevar,
                                      panelVar = panel.trust,
                                      autoCorr = autocorr,
                                      panelCorrMethod = panelcorrmethod)
        results$model5.panelar.all.coef <- extract_coefficients(x              = model5.panelar.all,
                                                                .site          = 'All',
                                                                .indicator     = indicator,
                                                                .sub.indicator = sub.indicator)
        results$model5.panelar.r2 <- model5.panelar.all
        ## Summary table
        results$model5.panelar <- results$model5.panelar.all.coef
        results$model5.panelar <- combine_coefficients(bishop.coef     = results$model2.panelar.bishop.coef,
                                                       hartlepool.coef = results$model2.panelar.hartlepool.coef,
                                                       hemel.coef      = results$model2.panelar.hemel.coef,
                                                       newark.coef     = results$model2.panelar.newark.coef,
                                                       rochdale.coef   = results$model2.panelar.rochdale.coef,
                                                       all.coef        = results$model5.panelar.all.coef)
        ## ## Forest plot
        results$model5.forest <- closed_forest(df.list       = list(results$model2.panelar.bishop.coef,
                                                                    results$model2.panelar.hartlepool.coef,
                                                                    results$model2.panelar.hemel.coef,
                                                                    results$model2.panelar.newark.coef,
                                                                    results$model2.panelar.rochdale.coef,
                                                                    results$model5.panelar.all.coef),
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
            results$model5.panelar.all     <- model5.panelar.all
        }
        ## Remove clutter
        rm(df5)
    }
    #######################################################################
    ## Return the results                                                ##
    #######################################################################
    return(results)
}
