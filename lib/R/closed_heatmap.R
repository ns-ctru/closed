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
                 indicator = paste0(measure, ' - ', sub.measure),
                 indicator = gsub('_', ' ', indicator),
                 indicator = factor(indicator,
                                    ## ToDo - Order these and (hopefully) do away with order1/order2
                                    levels = c('hospital transfers - fraction with transfer',
                                               'hospital transfers - stays with transfer',
                                               'hospital transfers - all stays',
                                               'case fatality ratio - stroke cva',
                                               'case fatality ratio - myocardial infarction',
                                               'case fatality ratio - acute heart failure',
                                               'case fatality ratio - any',
                                               'case fatality ratio - any trauma sec',
                                               'sec case fatality 7 days - any sec'
                                               'length of stay - median',
                                               'length of stay - mean',
                                               'critical care stays - fraction critical care',
                                               'critical care stays - critical care',
                                               'critical care stays - all',
                                               'avoidable emergency admissions - non-specific chest pain',
                                               'avoidable emergency admissions - any',
                                               'all emergency admissions - all',
                                               'ed attendances admitted - fraction admitted',
                                               'ed attendances admitted - admitted',
                                               'ed attendances admitted - all',
                                               'unnecessary ed attendances - all',
                                               'ed attendances - other',
                                               'ed attendances - ambulance',
                                               'ed attendances - any',
                                               'ambulance mean times - scene to dest',
                                               'ambulance mean times - dest to clear',
                                               'ambulance mean times - call to scene conveying',
                                               'ambulance mean times - call to scene any',
                                               'ambulance mean times - call to dest',
                                               'ambulance red calls - total',
                                               'ambulance red calls - hospital transfers',
                                               'ambulance green calls - fraction not conveyed',
                                               'ambulance green calls - hospital transfers',
                                               'ambulance green calls - not conveyed green calls',
                                               'ambulance green calls - green calls',
                                               'sec deaths all 7days - any sec',
                                               'sec deaths in cips 7days - any sec',
                                               'sec deaths not in cips 7days - any sec')))
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
    ## ToDo - Convert indicator to a factor variable and order logically
    df <- mutate(df,
                 order1 = 0,
                 order1 = ifelse(measure == 'ambulance green calls',      yes = 0, no = order1),
                 order1 = ifelse(measure == 'ambulance red calls',        yes = 1, no = order1),
                 order1 = ifelse(measure == 'ambulance mean times',       yes = 2, no = order1),
                 order1 = ifelse(measure == 'ed attendances',             yes = 3, no = order1),
                 order1 = ifelse(measure == 'unnecessary ed attendances', yes = 4, no = order1),
                 order1 = ifelse(measure == 'ed attendances admitted',    yes = 5, no = order1),
                 order1 = ifelse(measure == 'all emergency admissions',   yes = 6, no = order1),
                 order1 = ifelse(measure == 'avoidable emergency admissions',   yes = 7, no = order1),
                 order1 = ifelse(measure == 'critical care stays',        yes = 8, no = order1),
                 order1 = ifelse(measure == 'length of stay',             yes = 9, no = order1),
                 order1 = ifelse(measure == 'case fatality ratio',        yes = 10, no = order1),
                 order1 = ifelse(measure == 'hospital transfers',         yes = 11, no = order1),
                 order1 = ifelse(measure == 'sec case fatality 7 days',   yes = 12, no = order1),
                 order1 = ifelse(measure == 'sec deaths all 7 days',      yes = 13, no = order1),
                 order1 = ifelse(measure == 'sec deaths in cips 7 days',  yes = 14, no = order1),
                 order1 = ifelse(measure == 'sec deaths not in cips 7 days',  yes = 15, no = order1),
                 order2 = 0,
                 order2 = ifelse(measure == 'ambulance green calls' &
                                 sub.measure == 'green calls',
                                 yes = 1, no = order2),
                 order2 = ifelse(measure == 'ambulance green calls' &
                                 sub.measure == 'not conveyed green calls',
                                 yes = 2, no = order2),
                 order2 = ifelse(measure == 'ambulance green calls' &
                                 sub.measure == 'hospital transfers',
                                 yes = 3, no = order2),
                 order2 = ifelse(measure == 'ambulance green calls' &
                                 sub.measure == 'fraction not conveyed',
                                 yes = 4, no = order2),
                 order2 = ifelse(measure == 'ambulance red calls' &
                                 sub.measure == 'hospital transfers',
                                 yes = 1, no = order2),
                 order2 = ifelse(measure == 'ambulance red calls' &
                                 sub.measure == 'total',
                                 yes = 2, no = order2),
                 order2 = ifelse(measure == 'ambulance mean times' &
                                 sub.measure == 'call to dest',
                                 yes = 1, no = order2),
                 order2 = ifelse(measure == 'ambulance mean times' &
                                 sub.measure == 'call to scene any',
                                 yes = 2, no = order2),
                 order2 = ifelse(measure == 'ambulance mean times' &
                                 sub.measure == 'call to scene conveying',
                                 yes = 3, no = order2),
                 order2 = ifelse(measure == 'ambulance mean times' &
                                 sub.measure == 'dest to clear',
                                 yes = 4, no = order2),
                 order2 = ifelse(measure == 'ambulance mean times' &
                                 sub.measure == 'scene to dest',
                                 yes = 5, no = order2),
                 order2 = ifelse(measure == 'ed attendances' &
                                 sub.measure == 'any',
                                 yes = 1, no = order2),
                 order2 = ifelse(measure == 'ed attendances' &
                                 sub.measure == 'ambulance',
                                 yes = 2, no = order2),
                 order2 = ifelse(measure == 'ed attendances' &
                                 sub.measure == 'other',
                                 yes = 3, no = order2),
                 order2 = ifelse(measure == 'unnecessary ed attendances' &
                                 sub.measure == 'all',
                                 yes = 1, no = order2),
                 order2 = ifelse(measure == 'ed attendances admitted' &
                                 sub.measure == 'all',
                                 yes = 1, no = order2),
                 order2 = ifelse(measure == 'ed attendances admitted' &
                                 sub.measure == 'admitted',
                                 yes = 2, no = order2),
                 order2 = ifelse(measure == 'ed attendances admitted' &
                                 sub.measure == 'fraction admitted',
                                 yes = 3, no = order2),
                 order2 = ifelse(measure == 'all emergency admissions' &
                                 sub.measure == 'all',
                                 yes = 1, no = order2),
                 order2 = ifelse(measure == 'avoidable emergency admissions' &
                                 sub.measure == 'any',
                                 yes = 1, no = order2),
                 order2 = ifelse(measure == 'avoidable emergency admissions' &
                                 sub.measure == 'non-specific chest pain',
                                 yes = 2, no = order2),
                 order2 = ifelse(measure == 'critical care stays' &
                                 sub.measure == 'all',
                                 yes = 1, no = order2),
                 order2 = ifelse(measure == 'critical care stays' &
                                 sub.measure == 'critical care',
                                 yes = 2, no = order2),
                 order2 = ifelse(measure == 'critical care stays' &
                                 sub.measure == 'fraction critical care',
                                 yes = 3, no = order2),
                 order2 = ifelse(measure == 'length of stay' &
                                 sub.measure == 'mean',
                                 yes = 1, no = order2),
                 order2 = ifelse(measure == 'length of stay' &
                                 sub.measure == 'median',
                                 yes = 1, no = order2),
                 order2 = ifelse(measure == 'case fatality ratio' &
                                 sub.measure == 'any',
                                 yes = 1, no = order2),
                 order2 = ifelse(measure == 'case fatality ratio' &
                                 sub.measure == 'acute heart failure',
                                 yes = 2, no = order2),
                 order2 = ifelse(measure == 'case fatality ratio' &
                                 sub.measure == 'myocardial infarction',
                                 yes = 3, no = order2),
                 order2 = ifelse(measure == 'case fatality ratio' &
                                 sub.measure == 'trauma',
                                 yes = 4, no = order2),
                 order2 = ifelse(measure == 'case fatality ratio' &
                                 sub.measure == 'stroke cva',
                                 yes = 5, no = order2),
                 order2 = ifelse(measure == 'sec case fatality 7 days' &
                                 sub.measure == 'any sec',
                                 yes = 1, no = order2),
                 order2 = ifelse(measure == 'sec case fatality 7 days' &
                                 sub.measure == 'any single sec',
                                 yes = 1, no = order2),
                 order2 = ifelse(measure == 'sec deaths all 7 days' &
                                 sub.measure == 'any sec',
                                 yes = 2, no = order2),
                 order2 = ifelse(measure == 'sec deaths in cips 7 days' &
                                 sub.measure == 'any sec',
                                 yes = 1, no = order2),
                 order2 = ifelse(measure == 'sec deaths not in cips 7 days' &
                                 sub.measure == 'any sec',
                                 yes = 1, no = order2))
    ## Alternatively turn them into factors
    df <- mutate(df,
                 indicator = factor(indicator,))
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
                   geom_text(aes(label = overlay)) +
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
                   geom_text(aes(label = overlay)) +
                   ggtitle(x) + xlab('') + ylab('Indicator') +
                   scale_fill_gradient(high = 'white', low = colour) +
                   scale_x_discrete(position = 'top') +
                   theme_bw()
            }
        ## Build results to return
        if(x == 'Bishop Auckland'){
            results$bishop      <- fig
            results$bishop.data <- to.plot
        }
        else if(x == 'Hartlepool'){
            results$hartlepool <- fig
            results$hartlepool.data <- to.plot
        }
        else if(x == 'Hemel Hempstead'){
            results$hemel      <- fig
            results$hemel.data <- to.plot
        }
        else if(x == 'Newark'){
            results$newark     <- fig
            results$newark.data <- to.plot
        }
        else if(x == 'Rochdale'){
            results$rochdale   <- fig
            results$rochdale.data <- to.plot
        }
    }
    return(results)
}
