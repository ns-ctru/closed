#' Summarise ClosED data
#'
#' @description Summarise ClosED regression results
#'
#' @details The ClosED study uses time-series with dummy indicators to
#' test for the impact of closing Emergency Departments on indicators of
#' performance.  This short function tidies the results of various time-series
#' regression models and produces forest plots and summary tables of the key
#' predictor variables.
#'
#' @param df.list A list of data frame objects to summarise.
#' @param plot.term The term(s) from the regression model that is to be summarised/plotted.
#' @param facet.outcome Logical indicator of whether to facet plots by outcome for comparison.
#' @param title Title for Forest plot.
#' @param digits Number of digits to include in formatted output.
#' @param plot.ci If \code{TRUE} 95\% Confidence Intervals are plotted rather than Standard Error bars.
#' @param theme GGplot2 theme to use.
#' @param latex Produce results table in LaTeX format using Stargazer.
#' @param html Produce results table in HTML format using Stargazer.
#' @param model Indicator of whether the data to be plotted is from \code{panelAR} (use \code{prais}) or \code{tscount} (use \code{tscount}).
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
closed_forest <- function(df.list       = list(bishop.attendance.any.matched$coefficients,
                                               hartlepool.attendance.any.matched$coefficients,
                                               hemel.attendance.any.matched$coefficients,
                                               newark.attendance.any.matched$coefficients,
                                               rochdale.attendance.any.matched$coefficients),
##                                             all.attendance.any.matched$coefficients), ## ToDo - Add all pooled and case only
                          plot.term     = c('closure', 'time.to.ed'),
                          facet.outcome = FALSE,
                          title         = c('ED Attendance (Any)', 'Site v Matched Control'),
                          digits        = 3,
                          plot.ci       = TRUE,
                          theme         = theme_bw(),
                          latex         = FALSE,
                          html          = FALSE,
                          model         = 'prais'
                          ...){
    ## Inititiate list for returning results
    results <- list()
    ## Set label for x-axis based on term
    ## ToDo
    if(length(plot.term) == 1){
        if(plot.term == 'closure')         xaxis.label <- 'Closure'
        else if(plot.term == 'time.to.ed') xaxis.label <- 'Time to ED'
        else if(plot.term == 'intercept')  xaxis.label <- 'Intercept'
        xaxis.label <- paste0('Coefficient Estimate for ', xaxis.label)
    }
    else{
        xaxis.label <- 'Coefficient Estimates'
    }
    ## Convert lower case term to the names actually used
    ## ToDo - Is it worth converting these within closed_regress()
    ## term <- gsub('', 'town', term)
    ## term <- gsub('Time to ED', 'time.to.ed', term)
    ## term <- gsub('Intercept', '\\(Intercept\\)', term)
    ## term <- gsub('Closure', 'closure', term)
    ## ToDo - Workout how to take list of lists and extract coefficients internally
    #########################################################################
    ## Make data frame of coefficients                                     ##
    #########################################################################
    ## Bind everything together (works for arbitrary number of supplied data frames)
    ## ToDo - Might be more efficient/faster to use rbind_list() from dplyr
    if(model == 'prais'){
        df <- do.call(rbind, df.list)
        ## Tidy row and column names
        row.names(df) <- NULL
        ## Return data frame for printing as kable
        names(df) <- gsub('Std\\. Error', 'SE', names(df))
        names(df) <- c('Estimate', 'SE', 't value', 'P', 'term', 'site','indicator', 'sub.indicator')
        results$summary <- dplyr::select(df, site, term, Estimate, SE, P) %>%
                           arrange(site, term)
        names(df) <- c('est', 'se', 't', 'pvalue', 'Term', 'site', 'indicator', 'sub.indicator')
    }
    ## else if(model == 'tscount'){
    ##     df <- filter(df, coefficient == 'closure')
    ## }
    #########################################################################
    ## Use data frame to produce forest plot                               ##
    #########################################################################
    ## Set the distance for positioning when using multiple terms
    ## pd <- position_dodge(width = 0.4)
    if(ncol(df) == 9){
        df <- df[1:8]
    }
    df <- dplyr::filter(df, Term %in% plot.term)
    results$forest <- ggplot(df, aes(x = est,
                                     y = site,
                                     color = Term)) +
                      ## geom_point(position = pd) +
                      geom_point() +
                      geom_vline(xintercept = 0,linetype = "dashed") +
                      ylab("Closed ED") + xlab('Prais-Winsten Time-series Estimate') +
                      ggtitle(title)
    if(plot.ci == FALSE){
        results$forest <- results$forest +
                          geom_errorbarh(aes(xmin = est - se,
                                             xmax = est + se),
                                         height = 0.25)
                                     ## position = pd) +
    }
    else{
        results$forest <- results$forest +
                          geom_errorbarh(aes(xmin = est - (1.96 * se),
                                             xmax = est + (1.96 * se)),
                                         height = 0.25)
                                     ## position = pd) +
    }
    if(facet.outcome == TRUE){
        results$forest <- results$forest +
                          facet_wrap(outcome)
    }
    ## If multiple coefficients are being plotted we now jitter them
    if(length(plot.term) > 1){
        results$forest <- results$forest + geom_jitter()
    }
    ## Apply the user-specified theme
    if(!is.null(theme)){
        results$forest <- results$forest + theme

    }
    ## Return everything
    return(results)
}
