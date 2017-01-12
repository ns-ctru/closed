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
                        indicator      = '',
                        sub.indicator  = '',
                        digits         = 3,
                        plot.ci        = TRUE,
                        plot.null.line = FALSE,
                        theme          = theme_bw(),
                        ...){
    ## Inititiate list for returning results
    results <- list()
    #######################################################################
    ## Labels and captions conditional on outcome                        ##
    #######################################################################
    ## ToDo - Switch to parsing the data frames name, slightly easier/greater
    ##        internal consistency
    ## indicator <- substitute(df) %>% evaulate()
    ## print("Debug 1")
    if(indicator == 'ed attendances'){
        title1 <- 'Total ED Attendance'
        if(sub.indicator == 'any')            title2 <- ' (Any)'
        else if(sub.indicator == 'other')     title2 <- ' (Other)'
        else if(sub.indicator == 'ambulance') title2 <- ' (Ambulance)'
        null.line <- 1
    }
    else if(indicator == 'unnecessary ed attendances'){
        title1 <- 'Unnecessary ED Attendances'
        title2 <- ''
        null.line <- 1
    }
    else if(indicator == 'ed attendances admitted'){
        title1 <- 'ED Attendances Admitted'
        if(sub.indicator == 'all'){
            title2 <- ' (All)'
            null.line <- 1
        }
        else if(sub.indicator == 'fraction admitted'){
            title2 <- ' (Fraction)'
            null.line <- 0
        }
        else if(sub.indicator == 'admitted'){
            title2 <- ' (Absolute)'
            null.line <- 1
        }
    }
    else if(indicator == 'all emergency admissions'){
        title1 <- 'All Emergency Admissions'
        title2 <- ''
        null.line <- 1
    }
    else if(indicator == 'avoidable emergency admissions'){
        title1 <- 'Avoidable Emergency Admissions'
        if(sub.indicator == 'any')                             title2 <- ' (Any)'
        else if(sub.indicator == 'acute mental health crisis') title2 <- ' (Acute Mental Health Crisis)'
        else if(sub.indicator == 'angina')                     title2 <- ' (Angina)'
        else if(sub.indicator == 'blocked catheter')           title2 <- ' (Blocked Catheter)'
        else if(sub.indicator == 'cellulitis')                 title2 <- ' (Cellulitis)'
        else if(sub.indicator == 'copd')                       title2 <- ' (COPD)'
        else if(sub.indicator == 'dvt')                        title2 <- ' (DVT)'
        else if(sub.indicator == 'epileptic fit')              title2 <- ' (Epileptic Fit)'
        else if(sub.indicator == 'falls (75+ years)')          title2 <- ' (Falls >76yrs)'
        else if(sub.indicator == 'hypoglycaemia')              title2 <- ' (Hypoglycaemia)'
        else if(sub.indicator == 'minor head injuries')        title2 <- ' (Minor Head Injuries)'
        else if(sub.indicator == 'non-specific chest pain')    title2 <- ' (Non-Specific Chest Pain)'
        else if(sub.indicator == 'pyrexial child (<6 years)')  title2 <- ' (Pyrexial Child <6yrs)'
        else if(sub.indicator == 'urinary tract infection')    title2 <- ' (Urinary Tract Infection)'
        null.line <- 1
    }
    else if(indicator == 'all emergency admissions'){
        title1 <- 'All Emergency Admissions'
        title2 <- ' (All)'
        null.line <- 1
    }
    else if(indicator == 'length of stay'){
        title1 <- 'Length of Stay'
        if(sub.indicator == 'mean'){
            title2 <- ' (Mean)'
        }
        else if(sub.indicator == 'median'){
            title2 <- ' (Median)'
        }
        null.line <- 0
    }
    else if(indicator == 'critical care stays'){
        title1 <- 'Critical Care Stays'
        if(sub.indicator == 'all'){
            title2 <- ' (All)'
            null.line <- 1
        }
        else if(sub.indicator == 'critical care'){
            title2 <- ' (Critical Care)'
            null.line <- 1
        }
        else if(sub.indicator == 'fraction critical care'){
            title2 <- ' (Fractional Critical Care)'
            null.line <- 0
        }
    }
    else if(indicator == 'case fatality ratio' | indicator == 'sec case fatality 7 days'){
        if(indicator == 'case fatality ratio'){
            title1 <- 'Case Fatality Ratio'
            null.line <- 0
        }
        else if(indicator == 'sec case fatality 7 days'){
            title1 <- 'Case Fatalities @ 7 Days'
            null.line <- 1
        }
        if(sub.indicator == 'acute heart failure')              title2 <- ' (Acute Heart Failure)'
        else if(sub.indicator == 'anaphylaxis')                 title2 <- ' (Anaphylaxis)'
        else if(sub.indicator == 'any')                         title2 <- ' (Any)'
        else if(sub.indicator == 'any sec')                     title2 <- ' (Any SEC)'
        else if(sub.indicator == 'asphyxiation')                title2 <- ' (Asphyxiation)'
        else if(sub.indicator == 'asthma')                      title2 <- ' (Asthma)'
        else if(sub.indicator == 'cardiac arrest')              title2 <- ' (Cardiac Arrest)'
        else if(sub.indicator == 'falls')                       title2 <- ' (Falls)'
        else if(sub.indicator == 'fractured neck of femur')     title2 <- ' (Fractured Neck of Femur)'
        else if(sub.indicator == 'meningitis')                  title2 <- ' (Meningitis)'
        else if(sub.indicator == 'myocardial infarction')       title2 <- ' (Myocardial Infarction)'
        else if(sub.indicator == 'pregnancy and birth related') title2 <- ' (Pregnancy and Birth Related)'
        else if(sub.indicator == 'road traffic accident')       title2 <- ' (Road Traffic Accident)'
        else if(sub.indicator == 'ruptured aortic aneurysm')    title2 <- ' (Ruptured Aortic Aneurysm)'
        else if(sub.indicator == 'self harm')                   title2 <- ' (Self Harm)'
        else if(sub.indicator == 'septic shock')                title2 <- ' (Septic Shock)'
        else if(sub.indicator == 'serious head injury')         title2 <- ' (Serious Head Injury)'
        else if(sub.indicator == 'stroke cva')                  title2 <- ' (Stroke CVA)'
        else if(sub.indicator == 'any trauma sec')                         title2 <- ' (Any Trauma)'
    }
    else if(indicator == 'ambulance mean times'){
        title1 <- 'Ambulance Mean Times'
        if(sub.indicator == 'call to dest' | sub.indicator == 'call_to_dest'){
            title2 <- ' (Call to Destination)'
        }
        else if(sub.indicator == 'call to scene any' | sub.indicator == 'call_to_scene_any'){
            title2 <- ' (Call to Scene, Any)'
        }
        else if(sub.indicator == 'call to scene conveying' | sub.indicator == 'call_to_scene_conveying'){
            title2 <- ' (Call to Scene, Conveying)'
        }
        else if(sub.indicator == 'dest to clear' | sub.indicator == 'dest_to_clear'){
            title2 <- ' (Destination to Clear)'
        }
        else if(sub.indicator == 'scene to dest' | sub.indicator == 'scene_to_dest'){
            title2 <- ' (Scene to Destination)'
        }
        null.line <- 0
    }
    else if(indicator == 'ambulance non-conveyance'){
        title1 <- 'Ambulance Non-Conveyance'
        if(sub.indicator == 'fraction not conveyed'){
            title2 <- ' (Fraction)'
            null.line <- 0
        }
        else if(sub.indicator == 'green calls'){
            title2 <- ' (Green Calls)'
            null.line <- 1
        }
        else if(sub.indicator == 'not conveyed green calls'){
            title2 <- ' (Green Calls Not Conveyed)'
            null.line <- 1
        }
    }
    else if(indicator == 'ambulance green calls'){
        title1 <- 'Ambulance Green Calls'
        if(sub.indicator == 'fraction not conveyed'){
            title2 <- ' (Fraction)'
            null.line <- 0
        }
        else if(sub.indicator == 'green calls'){
            title2 <- ' (Green Calls)'
            null.line <- 1
        }
        else if(sub.indicator == 'hospital transfers'){
            title2 <- ' (Hospital Transfers)'
            null.line <- 1
        }
        else if(sub.indicator == 'not conveyed green calls'){
            title2 <- ' (Green Calls Not Conveyed)'
            null.line <- 1
        }
    }
    else if(indicator == 'ambulance red calls'){
        title1 <- 'Ambulance Red Calls'
        if(sub.indicator == 'hospital transfers')            title2 <- ' (Hospital Transfers)'
        else if(sub.indicator == 'total')                    title2 <- ' (Total)'
        null.line <- 1
    }
    else if(indicator == 'hospital transfers'){
        title1 <- 'Hospital Transfers'
        if(sub.indicator == 'all stays')                   title2 <- ' (All Stays)'
        else if(sub.indicator == 'fraction with transfer') title2 <- ' (Fraction of Stays with Transfers)'
        else if(sub.indicator == 'stays with transfer')    title2 <- ' (Stays with Transfers)'
        null.line <- 1
    }
    else if(indicator == 'sec case fatality 7 days'){
        title1 <- 'Case Fatality @ 7 Days'
        if(sub.indicator == 'any sec')                          title2 <- ' (Any Sec)'
        else if(sub.indicator == 'any single sec')              title2 <- ' (Any Single Sec)'
        else if(sub.indicator == 'acute heart failure')         title2 <- ' (Acute Heart Failure)'
        else if(sub.indicator == 'anaphylaxis')                 title2 <- ' (Anaphylaxis)'
        else if(sub.indicator == 'any')                         title2 <- ' (Any)'
        else if(sub.indicator == 'asphyxiation')                title2 <- ' (Asphyxiation)'
        else if(sub.indicator == 'asthma')                      title2 <- ' (Asthma)'
        else if(sub.indicator == 'cardiac arrest')              title2 <- ' (Cardiac Arrest)'
        else if(sub.indicator == 'falls')                       title2 <- ' (Falls)'
        else if(sub.indicator == 'fractured neck of femur')     title2 <- ' (Fractured Neck of Femur)'
        else if(sub.indicator == 'meningitis')                  title2 <- ' (Meningitis)'
        else if(sub.indicator == 'myocardial infarction')       title2 <- ' (Myocardial Infarction)'
        else if(sub.indicator == 'pregnancy and birth related') title2 <- ' (Pregnancy and Birth Related)'
        else if(sub.indicator == 'road traffic accident')       title2 <- ' (Road Traffic Accident)'
        else if(sub.indicator == 'ruptured aortic aneurysm')    title2 <- ' (Ruptured Aortic Aneurysm)'
        else if(sub.indicator == 'self harm')                   title2 <- ' (Self Harm)'
        else if(sub.indicator == 'septic shock')                title2 <- ' (Septic Shock)'
        else if(sub.indicator == 'serious head injury')         title2 <- ' (Serious Head Injury)'
        else if(sub.indicator == 'stroke cva')                  title2 <- ' (Stroke CVA)'
        null.line <- 1
    }
    else if(indicator == 'sec deaths all 7days' | indicator == 'sec_deaths_all_7_days'){
        title1 <- 'Deaths @ 7 Days'
        if(sub.indicator == 'any sec')                          title2 <- ' (Any Sec)'
        else if(sub.indicator == 'any sec')                     title2 <- ' (Any Single Sec)'
        else if(sub.indicator == 'acute heart failure')         title2 <- ' (Acute Heart Failure)'
        else if(sub.indicator == 'anaphylaxis')                 title2 <- ' (Anaphylaxis)'
        else if(sub.indicator == 'any')                         title2 <- ' (Any)'
        else if(sub.indicator == 'asphyxiation')                title2 <- ' (Asphyxiation)'
        else if(sub.indicator == 'asthma')                      title2 <- ' (Asthma)'
        else if(sub.indicator == 'cardiac arrest')              title2 <- ' (Cardiac Arrest)'
        else if(sub.indicator == 'falls')                       title2 <- ' (Falls)'
        else if(sub.indicator == 'fractured neck of femur')     title2 <- ' (Fractured Neck of Femur)'
        else if(sub.indicator == 'meningitis')                  title2 <- ' (Meningitis)'
        else if(sub.indicator == 'myocardial infarction')       title2 <- ' (Myocardial Infarction)'
        else if(sub.indicator == 'pregnancy and birth related') title2 <- ' (Pregnancy and Birth Related)'
        else if(sub.indicator == 'road traffic accident')       title2 <- ' (Road Traffic Accident)'
        else if(sub.indicator == 'ruptured aortic aneurysm')    title2 <- ' (Ruptured Aortic Aneurysm)'
        else if(sub.indicator == 'self harm')                   title2 <- ' (Self Harm)'
        else if(sub.indicator == 'septic shock')                title2 <- ' (Septic Shock)'
        else if(sub.indicator == 'serious head injury')         title2 <- ' (Serious Head Injury)'
        else if(sub.indicator == 'stroke cva')                  title2 <- ' (Stroke CVA)'
        null.line <- 1
    }
    else if(indicator == 'sec deaths in cips 7days' | indicator == 'sec_deaths_in_cips_7_days'){
        title1 <- 'Deaths in CIPS @ 7 Days'
        if(sub.indicator == 'any sec')                          title2 <- ' (Any Sec)'
        else if(sub.indicator == 'any single sec')              title2 <- ' (Any Single Sec)'
        else if(sub.indicator == 'acute heart failure')         title2 <- ' (Acute Heart Failure)'
        else if(sub.indicator == 'anaphylaxis')                 title2 <- ' (Anaphylaxis)'
        else if(sub.indicator == 'any')                         title2 <- ' (Any)'
        else if(sub.indicator == 'asphyxiation')                title2 <- ' (Asphyxiation)'
        else if(sub.indicator == 'asthma')                      title2 <- ' (Asthma)'
        else if(sub.indicator == 'cardiac arrest')              title2 <- ' (Cardiac Arrest)'
        else if(sub.indicator == 'falls')                       title2 <- ' (Falls)'
        else if(sub.indicator == 'fractured neck of femur')     title2 <- ' (Fractured Neck of Femur)'
        else if(sub.indicator == 'meningitis')                  title2 <- ' (Meningitis)'
        else if(sub.indicator == 'myocardial infarction')       title2 <- ' (Myocardial Infarction)'
        else if(sub.indicator == 'pregnancy and birth related') title2 <- ' (Pregnancy and Birth Related)'
        else if(sub.indicator == 'road traffic accident')       title2 <- ' (Road Traffic Accident)'
        else if(sub.indicator == 'ruptured aortic aneurysm')    title2 <- ' (Ruptured Aortic Aneurysm)'
        else if(sub.indicator == 'self harm')                   title2 <- ' (Self Harm)'
        else if(sub.indicator == 'septic shock')                title2 <- ' (Septic Shock)'
        else if(sub.indicator == 'serious head injury')         title2 <- ' (Serious Head Injury)'
        else if(sub.indicator == 'stroke cva')                  title2 <- ' (Stroke CVA)'
        null.line <- 1
    }
    else if(indicator == 'sec deaths not in cips 7days' | indicator == 'sec_deaths_not_in_cips_7_days'){
        title1 <- 'Deaths not in CIPS @ 7 Days'
        if(sub.indicator == 'any sec')                          title2 <- ' (Any Sec)'
        else if(sub.indicator == 'any single sec')              title2 <- ' (Any Single Sec)'
        else if(sub.indicator == 'acute heart failure')         title2 <- ' (Acute Heart Failure)'
        else if(sub.indicator == 'anaphylaxis')                 title2 <- ' (Anaphylaxis)'
        else if(sub.indicator == 'any')                         title2 <- ' (Any)'
        else if(sub.indicator == 'asphyxiation')                title2 <- ' (Asphyxiation)'
        else if(sub.indicator == 'asthma')                      title2 <- ' (Asthma)'
        else if(sub.indicator == 'cardiac arrest')              title2 <- ' (Cardiac Arrest)'
        else if(sub.indicator == 'falls')                       title2 <- ' (Falls)'
        else if(sub.indicator == 'fractured neck of femur')     title2 <- ' (Fractured Neck of Femur)'
        else if(sub.indicator == 'meningitis')                  title2 <- ' (Meningitis)'
        else if(sub.indicator == 'myocardial infarction')       title2 <- ' (Myocardial Infarction)'
        else if(sub.indicator == 'pregnancy and birth related') title2 <- ' (Pregnancy and Birth Related)'
        else if(sub.indicator == 'road traffic accident')       title2 <- ' (Road Traffic Accident)'
        else if(sub.indicator == 'ruptured aortic aneurysm')    title2 <- ' (Ruptured Aortic Aneurysm)'
        else if(sub.indicator == 'self harm')                   title2 <- ' (Self Harm)'
        else if(sub.indicator == 'septic shock')                title2 <- ' (Septic Shock)'
        else if(sub.indicator == 'serious head injury')         title2 <- ' (Serious Head Injury)'
        else if(sub.indicator == 'stroke cva')                  title2 <- ' (Stroke CVA)'
        null.line <- 1
    }
    else{
        title1 <- NULL
    }
    if(ma.model == 'Model 2')        title1 <- paste0('Site : ', title1)
    else if(ma.model == 'Model 6.1') title1 <- paste0('LSOA : ', title1)
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
    ## Use the metafor function for producing forest plots for comparison
    ## ToDo - Doesn't seem to work
    ## results$meta.forest <- forest.rma(results$meta.est)
    ## Obtain the list of sites used and return these so that plots can be generated
    results$meta.sites <- df[['town']]
    ## Create a matrix for summary meta-statistics
    meta.row <- matrix(c(NA, NA, ' Overall', NA, NA, results$meta.est$b[1], results$meta.est$se[1]), nrow = 1) %>%
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
    results$df <- mutate(results$df,
                         town = factor(town))
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
    ## Generate y.axis conditional on the town/overall indicator
    ## results$df <- results$df %>% mutate(y.axis = case_when(.$town == 'Bishop Auckland' ~ 6,
    ##                                                        .$town == 'Hartlepool' ~ 5,
    ##                                                        .$town == 'Hemel Hempstead' ~ 4,
    ##                                                        .$town == 'Newark' ~ 3,
    ##                                                        .$town == 'Rochdale' ~ 2,
    ##                                                        .$town == 'Overall' ~ 1))
    ## Generate ggplot
    results$forest <- ggplot(results$df,
                             aes(x = est,
                                 y = y.axis,
                                 xmin = lci,
                                 xmax = uci)) +
                      geom_point() +
                      ylab('Emergency Department')
    ## Add a title
    if(!is.null(title1)){
        results$forest <- results$forest +
                          labs(list(title = paste0(title1, title2)))
    }
    ## Optionally and CI's
    if(plot.ci == TRUE){
        results$forest <- results$forest +
                          geom_errorbarh(aes(xmin = lci,
                                             xmax = uci),
                                         height = 0.25)
    }
    ## Optionally add null effect line
    if(plot.null.line == TRUE){
        ## Conditionally set the intercept based on the measure/sub.measure
        if(indicator == 'mode')
        results$forest <- results$forest +
                          geom_vline(xintercept = null.line, linetype = 'dashed')
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
    if(!is.null(theme)){
        results$forest <- results$forest + theme
    }
    ## Add meaningful y-axis labels
    ## y.axis.label <- dplyr::select(results$df, )
    ## results$forest <- results$forest + scale_y_continuous(breaks = c(seq(1:6)),
    ##                                                       labels = c(1 = 'Overall',
    ##                                                                  2 = 'Rochdale',
    ##                                                                  3 = 'Newark',
    ##                                                                  4 = 'Hemel Hempstead'
    ##                                                                  5 = 'Hartlepool'
    ##                                                                  6 = 'Bishop Auckland'))
    return(results)
}
