#' Call Stata to run Negative Binomial analysis
#'
#' @description Perform Prais-Winsten Regression on ClosED data
#'
#' @details The CI has requested that Negative-Binomial Time-Series
#' regression is performed in Stata, this script does that and tidies
#' the results into comparable format to the closed_models() function.
#'
#' @param df.lsoa Data frame at the LSOA level to analyse.
#' @param df.trust Data frame at the Trust level to analyse ideally cleaned of spurious datasets by \code{closed_clean()}.
#' @param indicator The performance indicator to assess.
#' @param sub.indicator The sub-measure performance indicator to assess.
#' @param return.df Logical operator of whether to return the subsetted/summarised data frame (useful for subsequent development).
#' @param return.model Logical operator of whether to return the fitted models (not currently working correctly).
#' @param return.residuals Logical oeprator of whether to return the residuals of the fitted model.
#' @param rho.na.rm Logical operator passed to panelAR() for excluding panel specific autocorrelation when it can not be calculated.
#' @param digits Number of digits to include in summary table of means/sd.
#' @param rm.unused.control Remove controls that are not being used.
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
closed_stata_negbin <- function(df.lsoa         = ed_attendances_by_mode_measure,
                                df.trust        = ed_attendances_by_mode_site_measure_clean,
                                indicator       = 'ed attendances',
                                sub.indicator   = 'any',
                                return.df       = FALSE,
                                return.model    = TRUE,
                                return.residuals = FALSE,
                                digits           = 3,
                                rm.unused.control = TRUE,
                                ...){
    ## Results
    results <- list()
    ## Write site level data to Stata's .dta and make a copy
    dplyr::filter(df.trust,
                  measure == indicator, sub.measure == sub.indicator) %>%
        write.dta(file = '~/work/closed/nihr_report/data/site.dta')
    call <- paste0('cp ~/work/closed/nihr_report/data/site.dta ~/work/closed/nihr_report/data/input_site_',
                   gsub(' ', '_', indicator),
                   '_',
                   gsub(' ', '_', sub.indicator),
                   '.dta')
    system(call)
    ## Build a call to Stata to run the do-file with the given
    ## arguments of measure(/indicator) and sub-measure(/sub-indicator)
    call <- paste0('/usr/local/stata14/stata-mp -b ~/work/closed/nihr_report/do/negbin_site.do ',
                   gsub(' ', '_', indicator),
                   ' ',
                   gsub(' ', '_', sub.indicator))
    ## Run, copy file to its own unique location
    system(call)
    ## Read the results back in, copy results to their own file
    results$site <- read_dta(file = '~/work/closed/nihr_report/data/results/stata_negbin_site.dta')
    call <- paste0('cp ~/work/closed/nihr_report/data/results/stata_negbin_site.dta ~/work/closed/nihr_report/data/results/results_site_',
                   gsub(' ', '_', indicator),
                   '_',
                   gsub(' ', '_', sub.indicator),
                   '.dta')
    system(call)
    ## Write lsoa level data to Stata's .dta and make a copy
    ## NB - NOT filtering out control sites here, that is done in the call to -xtnbreg- in Stata
    dplyr::filter(df.lsoa,
                  measure == indicator, sub.measure == sub.indicator) %>%
        write.dta(file = '~/work/closed/nihr_report/data/lsoa.dta')
    call <- paste0('cp ~/work/closed/nihr_report/data/lsoa.dta ~/work/closed/nihr_report/data/input_lsoa_',
                   gsub(' ', '_', indicator),
                   '_',
                   gsub(' ', '_', sub.indicator),
                   '.dta')
    system(call)
    ## Build a call to Stata to run the do-file with the given
    ## arguments of measure(/indicator) and sub-measure(/sub-indicator)
    call <- paste0('/usr/local/stata14/stata-mp -b ~/work/closed/nihr_report/do/negbin_lsoa.do ',
                   gsub(' ', '_', indicator),
                   ' ',
                   gsub(' ', '_', sub.indicator))
    ## Run
    system(call)
    ## Read the results back in
    results$lsoa <- read_dta(file = '~/work/closed/nihr_report/data/results/stata_negbin_lsoa.dta')
    call <- paste0('cp ~/work/closed/nihr_report/data/results/stata_negbin_lsoa.dta ~/work/closed/nihr_report/data/results/results_lsoa_',
                   gsub(' ', '_', indicator),
                   '_',
                   gsub(' ', '_', sub.indicator),
                   '.dta')
    system(call)
    ## Remove the input files
    system('rm ~/work/closed/nihr_report/data/results/site.dta ~/work/closed/nihr_report/data/results/lsoa.dta')
    ## Bind and return results
    ## print("Site...")
    ## head(results$site) %>% print()
    ## print("LSOA...")
    ## head(results$lsoa) %>% print()
    results$all.model.all.coef <- rbind(results$site, results$lsoa)
    ## Build summary table as per other regression wrapper functions
    #######################################################################
    ## Derive the mean, sd, median, iqr, min and max of events before/   ##
    ## after closure for combining into a summary table with model       ##
    ## coefficients                                                      ##
    #######################################################################
    ## print("Debug 1")
    df.trust <- dplyr::filter(df.trust,
                              measure == indicator, sub.measure == sub.indicator) %>%
                mutate(before.after = ifelse(relative.month >= 25, "After", "Before"))
    results$tmp <-  df.trust
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
    ## print("Debug 2")
    results$summary.table.head <- melt(results$summary.table.head, id.vars = c('town', 'before.after')) %>%
                                  dcast(town ~ before.after + variable)
    results$summary.table.head$Before_mean <- as.numeric(results$summary.table.head$Before_mean)
    results$summary.table.head$After_mean  <- as.numeric(results$summary.table.head$After_mean)
    results$summary.table.head <- mutate(results$summary.table.head,
                                         diff_abs = formatC(Before_mean - After_mean, digits = digits, format = 'f'),
                                         diff_perc = formatC((100 * abs(Before_mean - After_mean)) / Before_mean, digits = digits, format = 'f'))
    ## Order the data
    ## print("Debug 3")
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
    ## print("Debug 4")
    results$summary.table.head$group <- NA
    results$summary.table.head$group[results$summary.table.head$town %in% c('Bishop Auckland', 'Whitehaven', 'Salford', 'Scarborough')] <- 'Bishop Auckland'
    results$summary.table.head$group[results$summary.table.head$town %in% c('Hartlepool', 'Grimsby', 'Blackburn', 'Wigan')] <- 'Hartlepool'
    results$summary.table.head$group[results$summary.table.head$town %in% c('Hemel Hempstead', 'Warwick', 'Basingstoke', 'Yeovil')] <- 'Hemel Hempstead'
    results$summary.table.head$group[results$summary.table.head$town %in% c('Newark', 'Southport', 'Carlisle', 'Salisbury')] <- 'Newark'
    results$summary.table.head$group[results$summary.table.head$town %in% c('Rochdale', 'Rotherham', 'Scunthorpe', 'Wansbeck')] <- 'Rochdale'
    ## Add indicator for primary control...
    if(rm.unused.control == TRUE){
        results$summary.table.head <- dplyr::filter(results$summary.table.head,
                                             town %in% c('Bishop Auckland', 'Whitehaven',
                                                         'Hartlepool', 'Grimsby',
                                                         'Hemel Hempstead', 'Warwick',
                                                         'Newark', 'Southport',
                                                         'Rochdale', 'Rotherham'))
    }
    ## ...if not then we add an indicator of ' (Primary)'
    else if(rm.unused.control == FALSE){
        results$summary.table.head$town <- as.character(results$summary.table.head$town)
        results$summary.table.head$town[results$summary.table.head$town %in% c('Whitehaven', 'Grimsby', 'Warwick', 'Southport', 'Rotherham')] <- paste0(results$summary.table.head$town[results$summary.table.head$town %in% c('Whitehaven', 'Grimsby', 'Warwick', 'Southport', 'Rotherham')], ' (Primary)')
    }
    ## Build the table footer from the regression results read in from Stata
    names(results$all.model.all.coef) <- gsub('estimate', 'est', names(results$all.model.all.coef))
    ## Standardise this output for combining with panelAR()
    ## print("Debug 5")
    names(results$all.model.all.coef) <- gsub('_', '.', names(results$all.model.all.coef))
    names(results$all.model.all.coef) <- gsub('parm', 'term', names(results$all.model.all.coef))
    names(results$all.model.all.coef) <- gsub('z', 'standard', names(results$all.model.all.coef))
    results$all.model.all.coef <- dplyr::select(results$all.model.all.coef,
                                     measure, sub.measure, town, model, term, est, stderr, p, min95, max95, standard)
    ## results$all.model.all.coef <- mutate(results$all.model.all.coef,
    ##                           measure     = gsub("_", " ", measure),
    ##                           sub.measure = gsub("_", " ", sub.measure))
    ## Remove baseline terms
    results$all.model.all.coef <- dplyr::filter(results$all.model.all.coef, !is.nan(p))
    ## Get terms to have same name
    results$all.model.all.coef <- mutate(results$all.model.all.coef,
                              term = gsub('_cons', 'Intercept', term),
                              term = gsub('1.ambulance_divert', 'ambulance.divert', term),
                              term = gsub('1.nhs111', 'nhs111', term),
                              term = gsub('1.nhs111', 'nhs111', term),
                              term = gsub('1.closure', 'closure', term),
                              term = gsub('1.other_centre', 'other.centre', term),
                              term = gsub('relative_month', 'relative.month', term),
                              term = gsub('diff_time_to_ed', 'diff.time.to.ed', term),
                              term = gsub('2.season', 'season.2', term),
                              term = gsub('3.season', 'season.3', term),
                              term = gsub('4.season', 'season.4', term),
                              term = gsub('5.season', 'season.5', term),
                              term = gsub('6.season', 'season.6', term),
                              term = gsub('1.town#closure', 'townBishop Auckland:closure ', term),
                              term = gsub('1.town', 'townBishop Auckland', term),
                              term = gsub('2.town#closure', 'townBasingstoke:closure ', term),
                              term = gsub('2.town', 'townBasingstoke', term),
                              term = gsub('3.town#closure', 'townBlackburn:closure ', term),
                              term = gsub('3.town', 'townBlackburn', term),
                              term = gsub('4.town#closure', 'townCarlisle:closure ', term),
                              term = gsub('4.town', 'townCarlisle', term),
                              term = gsub('5.town#closure', 'townGrimsby:closure ', term),
                              term = gsub('5.town', 'townGrimsby', term),
                              term = gsub('6.town#closure', 'townHartlepool:closure ', term),
                              term = gsub('6.town', 'townHartlepool', term),
                              term = gsub('7.town#closure', 'townHemel Hempstead:closure ', term),
                              term = gsub('7.town', 'townHemel Hempstead', term),
                              term = gsub('8.town#closure', 'townNewark:closure ', term),
                              term = gsub('8.town', 'townNewark', term),
                              term = gsub('9.town#closure', 'townRochdale:closure ', term),
                              term = gsub('9.town', 'townRochdale', term),
                              term = gsub('10.town#closure', 'townRotherham:closure ', term),
                              term = gsub('10.town', 'townRotherham', term),
                              term = gsub('1town#closure', 'townSalford:closure ', term),
                              term = gsub('1town', 'townSalford', term),
                              term = gsub('12.town#closure', 'townSalisbury:closure ', term),
                              term = gsub('12.town', 'townSalisbury', term),
                              term = gsub('13.town#closure', 'townScarborough:closure ', term),
                              term = gsub('13.town', 'townScarborough', term),
                              term = gsub('14.town#closure', 'townScunthorpe:closure ', term),
                              term = gsub('14.town', 'townScunthorpe', term),
                              term = gsub('15.town#closure', 'townSouthport:closure ', term),
                              term = gsub('15.town', 'townSouthport', term),
                              term = gsub('16.town#closure', 'townWansbeck:closure ', term),
                              term = gsub('16.town', 'townWansbeck', term),
                              term = gsub('17.town#closure', 'townWarwick:closure ', term),
                              term = gsub('17.town', 'townWarwick', term),
                              term = gsub('18.town#closure', 'townWhitehaven:closure ', term),
                              term = gsub('18.town', 'townWhitehaven', term),
                              term = gsub('19.town#closure', 'townWigan:closure ', term),
                              term = gsub('19.town', 'townWigan', term),
                              term = gsub('20.town#closure', 'townYeovil:closure ', term),
                              term = gsub('20.town', 'townYeovil', term),
                              model = gsub('model', 'Model ', model))
    ## Subset out the coefficients of interest for the table footer
    ## print("Debug 6.1")
    ## dim(results$all.model.all.coef) %>% print()
    ## head(results$all.model.all.coef) %>% print()
    ## table(results$all.model.all.coef$term) %>% print()
    results$summary.table.tail <- dplyr::filter(results$all.model.all.coef, term == 'closure' | term == 'diff.time.to.ed') %>%
                                  dplyr::select(est, stderr, p, min95, max95, town, measure, sub.measure, model)
    ## print("Debug 6.2")
    ## dim(results$all.model.all.coef) %>% print()
    ## head(results$all.model.all.coef) %>% print()
    ## table(results$all.model.all.coef$term) %>% print()
    results$summary.table.tail$estimate <- paste0(formatC(results$summary.table.tail$est, digits = digits, format = 'f'),
                                                  ' (',
                                                  formatC(results$summary.table.tail$min95, digits = digits, format = 'f'),
                                                  '-',
                                                  formatC(results$summary.table.tail$max95, digits = digits, format = 'f'),
                                                  ' ) p = ',
                                                  formatC(results$summary.table.tail$p, digits = digits, format = 'f'))
    ## print("Debug 6.2")
    ## print(results$summary.table.tail)
    results$summary.table.tail <- dplyr::select(results$summary.table.tail,
                                                town,
                                                model,
                                                estimate) %>%
                                  mutate(model = gsub('model', 'Model ', model))
    ## Order the table
    ## print("Debug 7")
    results$summary.table.tail$order1 <- 0
    results$summary.table.tail$order2 <- 0
    results$summary.table.tail <- mutate(results$summary.table.tail,
                                         order1 = ifelse(model == 'Model 0', 1, order1),
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
    ## results$summary.table$Before_median_iqr <- gsub('model', 'Model ', results$summary.table$Before_median_iqr)
    ## print("Debug 8")
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
    ## Finally sort the overall summary table
    ## results$summary.table <- dplyr::select(results$summary.table, town, Before_mean.sd, Before_median.iqr, Before_min.max, After_mean.sd, After_median.iqr, After_min.max, diff_abs, diff_perc)
    ## names(results$summary.table) <- c('Town', 'Pre Mean (SD)', 'Pre Median (IQR)', 'Pre Range', 'Post Mean (SD)', 'Post Median (IQR)', 'Post Range', 'Difference', 'Percentage')
    #######################################################################
    ## Return the results                                                ##
    #######################################################################
    return(results)
}
