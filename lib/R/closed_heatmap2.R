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
#'
#' @return A ggplot2 graph.
#'
#' @examples
#'
#' @export
## Tips...
## http://stackoverflow.com/questions/14290364/heatmap-with-values-ggplot2
closed_heatmap2 <- function(df           = summary.models,
                            site         = c('Bishop Auckland', 'Hartlepool', 'Hemel Hempstead', 'Newark', 'Rochdale'),
                            colour.by    = 'coefficient',
                            coef         = c('closure', 'diff.time.to.ed'),
                            include.text = c('coefficient', 'se', 'p'),
                            text.size    = 3,
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
                                    levels = c('sec deaths all 7days - any sec',
                                               'sec deaths in cips 7days - any sec',
                                               'sec deaths not in cips 7days - any sec',
                                               'hospital transfers - fraction with transfer',
                                               'hospital transfers - stays with transfer',
                                               'hospital transfers - all stays',
                                               'case fatality ratio - stroke cva',
                                               'case fatality ratio - myocardial infarction',
                                               'case fatality ratio - acute heart failure',
                                               'case fatality ratio - any',
                                               'case fatality ratio - any trauma sec',
                                               'sec case fatality 7 days - any sec',
                                               'sec case fatality 7 days - stroke cva',
                                               'sec case fatality 7 days - myocardial infarction',
                                               'sec case fatality 7 days - acute heart failure',
                                               'sec case fatality 7 days - any',
                                               'sec case fatality 7 days - any trauma sec',
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
                                               'ambulance green calls - green calls')))
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
    df <- mutate(df, overlay = '')
    if('coefficient' %in% include.text){
        df <- mutate(df, overlay = paste0(overlay,
                                          '\n',
                                          formatC(est, digits = digits, format = 'f')))
    }
    if('standard' %in% include.text){
        df <- mutate(df, overlay = paste0(overlay,
                                          '\n',
                                          formatC(standard, digits = digits, format = 'f')))
    }
    if('se' %in% include.text){
        df <- mutate(df, overlay = paste0(overlay,
                                          '\n',
                                          'SE = ',
                                          formatC(se, digits = digits, format = 'f')))
    }
    if('p' %in% include.text){
        df <- mutate(df, overlay = paste0(overlay,
                                          '\n',
                                          'p = ',
                                          formatC(p, digits = digits, format = 'f')))
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
                 order1 = ifelse(measure == 'sec deaths all 7days',       yes = 13, no = order1),
                 order1 = ifelse(measure == 'sec deaths in cips 7days',   yes = 14, no = order1),
                 order1 = ifelse(measure == 'sec deaths not in cips 7days',  yes = 15, no = order1),
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
                                 sub.measure == 'any trauma sec',
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
                 order2 = ifelse(measure == 'sec deaths all 7days' &
                                 sub.measure == 'any sec',
                                 yes = 2, no = order2),
                 order2 = ifelse(measure == 'sec deaths in cips 7days' &
                                 sub.measure == 'any sec',
                                 yes = 1, no = order2),
                 order2 = ifelse(measure == 'sec deaths not in cips 7days' &
                                 sub.measure == 'any sec',
                                 yes = 1, no = order2))
    ## Alternatively turn them into factors
    df <- mutate(df,
                 indicator = factor(indicator))
    ## Generate Site level heatmap
    to.plot <- dplyr::filter(df, model %in% c('Model 2', 'Model 3') & term == 'closure')
    results$df.site <- to.plot
    site <- ggplot(to.plot,
                  aes(x = as.factor(town),
                      y = as.factor(indicator)))
    ## By Coefficient
    if(colour.by == 'coefficient'){
        site <- site +
               geom_tile(aes(fill = est)) +
               scale_fill_gradient(low = 'white', high = colour)
        title <- 'Heatmap by Coefficient'
    }
    ## By P-value (wrong!)
    else if(colour.by == 'p'){
        site <- site +
               geom_tile(aes(fill = p)) +
               scale_fill_gradient(low = colour, high = 'white')
        title <- 'Heatmap by P-value'
    }
    ## By standardised value
    else if(colour.by == 'standard'){
        site <- site +
            geom_tile(aes(fill = standard)) +
               scale_fill_gradient(low = 'white', high = colour)
        title <- 'Heatmap by Standardised Coefficient'
    }
    results$site <- site +
                    geom_text(aes(label = overlay), size = text.size, nudge_y = 0.3) +
                    ggtitle(paste0('Site : ', title, ' (Models 2 + 3)')) + xlab('') + ylab('Indicator') +
                    scale_x_discrete(position = 'top') +
                    theme_bw()
    ## Generate LSOA level heatmap
    to.plot <- dplyr::filter(df, model %in% c('Model 4', 'Model 5') & term == 'diff.time.to.ed')
    results$df.lsoa <- to.plot
    lsoa <- ggplot(to.plot,
                  aes(x = as.factor(town),
                      y = as.factor(indicator)))
    ## By Coefficient
    if(colour.by == 'coefficient'){
        lsoa <- lsoa +
               geom_tile(aes(fill = est)) +
               scale_fill_gradient(low = 'white', high = colour)
        title <- 'Heatmap by Coefficient'
    }
    ## By P-value (wrong!)
    else if(colour.by == 'p'){
        lsoa <- lsoa +
               geom_tile(aes(fill = p)) +
               scale_fill_gradient(low = colour, high = 'white')
        title <- 'Heatmap by P-value'
    }
    ## By standardised value
    else if(colour.by == 'standard'){
        lsoa <- lsoa +
               geom_tile(aes(fill = standard)) +
               scale_fill_gradient(low = 'white', high = colour)
        title <- 'Heatmap by Standardised Coefficient'
    }
    results$lsoa <- lsoa +
                    geom_text(aes(label = overlay), size = text.size, nudge_y = 0.3) +
                    ggtitle(paste0('LSOA : ', title, ' (Models 4 + 5)')) + xlab('') + ylab('Indicator') +
                    scale_x_discrete(position = 'top') +
        theme_bw()
    ## Return results
    return(results)
}
