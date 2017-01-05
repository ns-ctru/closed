#' Meta Analysis and Forest plot for ClosED
#'
#' @description Meta Analysis and Forest plot for ClosED
#'
#' @details The ClosED study uses time-series with dummy indicators to
#' test for the impact of closing Emergency Departments on indicators of
#' performance.  This short function tidies the results of various time-series
#' regression models and produces forest plots and summary tables of the key
#' predictor variables.
#'
#' @param df.list A list of data frame objects to summarise.
#' @param ma.model The model whose term(s) from the regression model that are to be summarised/plotted, this uses the original model number \code{0, 1, 2, 3.1, 3.2, 4, 5, 6.1, 6.2, 7.1, 7.2}
#' @param ma.method Method for calculating pooled effect, default is \code{FE} for fixed effects.  See \code{?rma} for other options.
#' @param title Title for Forest plot.
#' @param digits Number of digits to include in formatted output.
#' @param plot.ci If \code{TRUE} 95\% Confidence Intervals are plotted rather than Standard Error bars.
#' @param plot.null.line if \code{TRUE} a vertical dashed line is plotted at the null effect (i.e. \code{x = 0})
#' @param theme GGplot2 theme to use.
#'
#'
#' @references
#'
#' @export
################################################################################################
## Notes...                                                                                   ##
## Main >> https://sakaluk.wordpress.com/2016/02/16/7-make-it-pretty-plots-for-meta-analysis/ ##
## http://chetvericov.ru/analiz-dannyx/grouped-forest-plots-using-ggplot2/                    ##
################################################################################################
closed_meta <- function(df             = mode.of.arrival.any,
                        ma.model       = 'Model 2',
                        ma.method      = 'FE',
                        title          = 'ED Attendance (Any)',
                        digits         = 3,
                        plot.ci        = TRUE,
                        plot.null.line = FALSE,
                        theme          = theme_bw(),
                        ...){
    ## Inititiate list for returning results
    results <- list()
    ## Extract Point Estimate and SE from data frame for the specificed model and
    ## given outcome.
    df <- dplyr::filter(df, model == ma.model) %>%
          dplyr::filter(term %in% c('closure', 'diff.time.to.ed')) %>%
          dplyr::select(measure, sub.measure, town, model, term, est, stderr)
    ## print('Debug 1')
    ## print(df)
    ## Calculate the meta-analysis statistics
    results$meta.est <- rma(data   = df,
                            yi     = est,
                            sei    = stderr,
                            method = ma.method)
    ## Create a matrix for summary meta-statistics
    meta.row <- matrix(c(NA, NA, 'Summary', NA, NA, results$meta.est$b[1], results$meta.est$se[1]), nrow = 1) %>%
                 as.data.frame()
    names(meta.row) <- names(df)
    ## print('Debug 2')
    ## print(meta.row)
    results$df <- rbind(df, meta.row)
    ## print('Debug 3')
    ## print(results$df)
    ## typeof(results$df$est) %>% print()
    ## typeof(results$df$stderr) %>% print()
    ## Force formats
    results$df$est    <- as.numeric(results$df$est)
    results$df$stderr <- as.numeric(results$df$stderr)
    ## Derive 95% CI's
    results$df$lci <- results$df$est - (1.96 * results$df$stderr)
    results$df$uci <- results$df$est + (1.96 * results$df$stderr)
    ## Combine Site, point estimate and CI for plotting on y-axis
    results$df$y.axis <- paste0(results$df$town,
                                ' ',
                                formatC(results$df$est, format = 'f', digits = digits),
                                ' (',
                                formatC(results$df$lci, format = 'f', digits = digits),
                                ' - ',
                                formatC(results$df$uci, format = 'f', digits = digits),
                                ')')
    ## Reverse the order of y.axis labels ('Summary' the meta needs to be
    ## at the bottom, always will be since alphabetically its last)
    results$df <- mutate(results$df,
                         y.axis = factor(y.axis),
                         y.axis = factor(y.axis, levels = rev(levels(y.axis))))
    ## Generate ggplot
    results$forest <- ggplot(results$df,
                             aes(x = est,
                                 y = y.axis,
                                 xmin = lci,
                                 xmax = uci)) +
                      geom_point() +
                      ## scale_y_continuous(trans = 'reverse') +
                      ylab('Emergency Department')
    ## Optionally and CI's
    if(plot.ci == TRUE){
        results$forest <- results$forest +
                          geom_errorbarh(aes(xmin = lci,
                                             xmax = uci),
                                         height = 0.25)
    }
    ## Optionally add null effect line
    if(plot.null.line == TRUE){
        results$forest <- results$forest +
                          geom_vline(xintercept = 0, linetype = 'dashed')
    }
    ## Conditionally label the x-axis
    if(ma.model == 'Model 2'){
        results$forest <- results$forest +
                          xlab('Estimated Coefficient for Closure of ED')
    }
    else if(ma.model == 'Model 6.1'){
        results$forest <- results$forest +
                          xlab('Estimated Coefficient for Difference in Time to ED')
    }
    ## Apply users theme
    if(!is.null(theme))
        results$forest <- results$forest + theme
    return(results)
}
