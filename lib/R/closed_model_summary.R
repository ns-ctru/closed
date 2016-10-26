#' Summary of Time-Series models for ClosED study
#'
#' @description Table of statistics for the Time-Series models in the ClosED study.
#'
#' @details The ClosED study uses time-series with dummy indicators to
#' test for the impact of closing Emergency Departments on indicators of
#' performance.  This function summarises the mean, standard deviation, median,
#' inter-quartile, minimum and maximum before and after the intervention by
#' site and if provided (which they should be!) appends the regression coefficients
#' for the intervention itself for each of the models tested.
#'
#' This function produces time-series plots in different formats.
#'
#' @param df Data frame to be plotted.
#' @param sites The sites that are to be plotted, default is for case sites.
#' @param indicator The performance indicator to assess.
#' @param sub.indicator The sub-measure performance indicator to assess.
#' @param steps Logical indicator of whether to plot vertical lines for each step.
#' @param common.y Generate all plots with a common y-axis range.
#' @param theme GGplot2 theme to use.
#' @param tidy Logical indicator of whether to remove spurious data points when plotting
#' @param join Logical indicator of whether to completely remove time points with spurious data so that lines are continuous
#' @param legend Logical indicator of whether to include a legend
#' @param lines Logical indicator of whether to include a vertical line for steps.
#' @param exclude.control Logical indicator of whether to exclude vertical step lines for pooled controls (default is \code{FALSE}, switch to \code{TRUE} when passing a pooled data set)
#' @param xaxis.steps Logical indicator of whether to add x-axis labels for steps.
#' @param fig String of the figure number to apply to the title.
#' @param repel Add repelled labels to graph (deafult \code{FALSE}).
#' @param colour Produce colour plots or not.
#'
#' @return A list of ggplot2 objects.
#'
#' @examples
#'
#'
#' @references
#'
#' @export
closed_model_summary <- function(df        = ed_attendances_by_mode_site_measure,
                           sites           = c('Basingstoke', 'Bishop Auckland', 'Blackburn', 'Carlisle', 'Grimsby', 'Hartlepool', 'Hemel Hempstead', 'Newark', 'Rochdale', 'Rotherham', 'Salford', 'Salisbury', 'Scarborough', 'Scunthorpe', 'Southport', 'Wansbeck', 'Warwick', 'Whitehaven', 'Wigan', 'Yeovil'),
                           indicator       = 'ed attendances',
                           sub.indicator   = 'any',
                           model1.results  = NULL,
                           model2.results  = NULL,
                           model3a.results  = NULL,
                           model3b.results  = NULL,
                           model4.results  = NULL,
                           model5.results  = NULL,
                           model6.results  = NULL,
                           ## model6.1.results  = NULL,
                           ## model6.2.results  = NULL,
                           model7.results  = NULL,
                           ...){
    ## Dummy for before/after
    df$closure <- ifelse(df$relative.month > 24, 'After', 'Before')
    ## Filter, group and summarise
    df.sum <- dplyr::filter(df,
                      town %in% sites &
                      measure     == indicator &
                      sub.measure == sub.indicator) %>%
               group_by(group, town, closure) %>%
                   summarise(n          = n(),
                             mean       = mean(value, na.rm = TRUE),
                             sd         = sd(value, na.rm = TRUE),
                             p25        = quantile(value, probs = 0.25, na.rm = TRUE),
                             p50        = quantile(value, probs = 0.50, na.rm = TRUE),
                             p75        = quantile(value, probs = 0.75, na.rm = TRUE),
                             min        = min(value, na.rm = TRUE),
                             max        = max(value, na.rm = TRUE))
    ## Melt and Cast
    ## gdf.sum <- gather(df.sum, key = c(group, town, closure))
    ## df.sum <- dcast(df.sum, group + town)
    ## Append models results if any are supplied
    if(!is.null(model1.results) |
       !is.null(model2.results) |
       !is.null(model3a.results) |
       !is.null(model3b.results) |
       !is.null(model4.results) |
       !is.null(model5.results) |
       !is.null(model6.results) |
       ## !is.null(model6.1.results) |
       ## !is.null(model6.2.results) |
       !is.null(model7.results)){

    }
    return(df.sum)
}
