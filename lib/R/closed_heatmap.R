#' Generate a heatmap table of results
#'
#' @description Tabulates in a heatmap the results of different models and outcomes
#'
#' @details The Closed study has a large number of indicator variables and models tested. In
#' order to aid comprehension and understanding of patterns tables of the coefficients
#' of interest from each of the models are produced by \code{closed_table()}.  These
#' still be tricky to understand though, requiring the reader to digest each number and
#' compare them across indicators/models.  The mental task of interpretation can be
#' eased to some extent by utilisng heatmaps to show the strength of effect for a given
#' indicator and model.
#'
#' @param df Data frame of all results combined
#' @param color.by Variable used to shade tiles, default is standardised coefficient \code{standard} (which for Negative Binomial is \code{z}, whilst for Prais-Winsten it is \code{t}).  You can use \code{p} to plot the significance, but it is STRONGLY advisable not to since p-values are NOT indicative of the magnitude of effect.
#' @param coef List of coefficients to be overlaid.  Default is \code{c(closure, diff.time.to.ed)} and should not need changing.
#' @param include.text Whether to overlay the actual numbers. Default is \code{c('coefficient', 'se', 'p')} other options include \code{'ci'} if the 95% Confidence Interval is also required.
#' @param colour Colour scheme to use.
#' @param digits Number of decimal places to include in numeric overlays.
#' @param final Logical indicator of whether results are for final output, this will renumber models if \code{TRUE}.
#'
#' @return A ggplot2 graph.
#'
#' @examples
#'
#' @export
## Tips...
## http://stackoverflow.com/questions/14290364/heatmap-with-values-ggplot2
closed_heatmap <- function(df           = summary.models,
                           site         = c('Bishop Auckland', 'Hartlepool', 'Hemel Hempstead', 'Newark', 'Rochdale'),
                           colour.by    = 'coefficient',
                           coef         = c('closure', 'diff.time.to.ed'),
                           include.text = c('coefficient', 'se', 'p'),
                           colour       = 'red',
                           final        = TRUE,
                           digits       = 2,
                           ...){
    ## Results
    results <- list()
    ## Combine measure and sub.measure to give overall indicator name
    df <- mutate(df,
                 indicator = paste0(measure, ' ', sub.measure))
    ## Renumber models if this is for final output
    if(final == TRUE){
        df <- mutate(df,
                     model = ifelse(model == 'Model 4',
                                    yes = 'Model 3',
                                    no  = model),
                     model = ifelse(model == 'Model 6.1',
                                    yes = 'Model 4',
                                    no  = model),
                     model = ifelse(model == 'Model 7.1',
                                    yes = 'Model 5',
                                    no  = model))
    }
    ## Build the text that is to be overlaid
    if('coefficient' %in% include.text){
        df <- mutate(df, overlay = formatC(est, digits = digits, format = 'f'))
    }
    if('coefficient' %in% include.text & 'se' %in% include.text){
        df <- mutate(df, overlay = paste0(formatC(est, digits = digits, format = 'f'),
                                          '\n SE = ',
                                          formatC(stderr, digits = digits, format = 'f')))
    }
    if('coefficient' %in% include.text & 'se' %in% include.text & 'p' %in% include.text){
        df <- mutate(df, overlay = paste0(formatC(est, digits = digits, format = 'f'),
                                          '\n SE = ',
                                          formatC(stderr, digits = digits, format = 'f'),
                                          '\n p =',
                                          formatC(p, digits = digits, format = 'f')))
    }
    else if('p' %in% include.text){
        df <- mutate(df, overlay = formatC(p, digits, format = 'f'))
    }
    ## Loop over all sites
    for(x in site){
        ## Filter data
        to.plot <- dplyr::filter(df, (town == x | town == 'All') & term %in% coef)
        ## Build graph
        if(colour.by == 'coefficient'){
            fig <- ggplot(to.plot,
                          aes(x = as.factor(model),
                              y = as.factor(indicator))) +
                   geom_tile(aes(fill = est)) +
                   geom_text(aes(fill = est, label = overlay)) +
                   ggtitle(x) + xlab('') + ylab('Indicator') +
                   scale_fill_gradient(low = 'white', high = colour) +
                   scale_x_discrete(position = 'top') +
                   theme_bw()
        }
        else if(colour.by == 'p'){
            fig <- ggplot(to.plot,
                          aes(x = as.factor(model),
                              y = as.factor(indicator))) +
                   geom_tile(aes(fill = p)) +
                   geom_text(aes(fill = p, label = overlay)) +
                   ggtitle(x) + xlab('') + ylab('Indicator') +
                   scale_fill_gradient(high = 'white', low = colour) +
                   scale_x_discrete(position = 'top') +
                   theme_bw()
            }
        ## Build results to return
        if(x == 'Bishop Auckland')      results$bishop     <- fig
        else if(x == 'Hartlepool')      results$hartlepool <- fig
        else if(x == 'Hemel Hempstead') results$hemel      <- fig
        else if(x == 'Newark')          results$newark     <- fig
        else if(x == 'Rochdale')        results$rochdale   <- fig
    }
    return(results)
}
