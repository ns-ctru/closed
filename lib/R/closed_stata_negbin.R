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
                                ...){
    ## Write site level data to Stata's .dta
    write.dta(df.trust,
              file = '~/work/closed/hta_report/data/site.dta')
    ## Build a call to Stata to run the do-file with the given
    ## arguments of measure(/indicator) and sub-measure(/sub-indicator)
    call <- paste0('/usr/local/stata14/stata-mp -b ~/work/closed/hta_report/do/negbin_site.do ',
                   gsub(' ', '_', indicator),
                   ' ',
                   sub.indicator)
    ## Run
    system(call)
    ## Read the results back in
    results_site <- read_dta(file = '~/work/closed/hta_report/data/results/stata_negbin_site.dta')
    ## Write lsoa level data to Stata's .dta
    write.dta(df.lsoa,
              file = '~/work/closed/hta_report/data/lsoa.dta')
    ## Build a call to Stata to run the do-file with the given
    ## arguments of measure(/indicator) and sub-measure(/sub-indicator)
    call <- paste0('/usr/local/stata14/stata-mp -b ~/work/closed/hta_report/do/negbin_lsoa.do ',
                   gsub(' ', '_', indicator),
                   ' ',
                   sub.indicator)
    ## Run
    system(call)
    ## Read the results back in
    results_lsoa <- read_dta(file = '~/work/closed/hta_report/data/results/stata_negbin_lsoa.dta')
    ## Bind and return results
    results <- rbind(results_site, results_lsoa)
    return(results)
}
