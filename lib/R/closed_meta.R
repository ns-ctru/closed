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
    ########################################################################
    ## Filter the data                                                    ##
    ########################################################################
    ## Extract Point Estimate and SE from data frame for the specificed model and
    ## given outcome.
    ## 2016-01-30 - Interaction terms required to be plotted.
    df <- df %>%
          mutate(use = case_when(.$model == 'Model 0'   & .$term == 'closure' ~ TRUE,
                                 .$model == 'Model 0.5' & .$term == 'closure' ~ TRUE,
                                 .$model == 'Model 1'   & .$term == 'closure' ~ TRUE,
                                 .$model == 'Model 2'   & .$term == 'townBishop Auckland:closure' ~ TRUE,
                                 .$model == 'Model 2'   & .$term == 'townBishop Auckland#closure' ~ TRUE,
                                 .$model == 'Model 2'   & .$term == 'townHartlepool:closure' ~ TRUE,
                                 .$model == 'Model 2'   & .$term == 'townHartlepool#closure' ~ TRUE,
                                 .$model == 'Model 2'   & .$term == 'townHemel Hempstead:closure' ~ TRUE,
                                 .$model == 'Model 2'   & .$term == 'townHemel Hempstead#closure' ~ TRUE,
                                 .$model == 'Model 2'   & .$term == 'townNewark:closure' ~ TRUE,
                                 .$model == 'Model 2'   & .$term == 'townNewark#closure' ~ TRUE,
                                 .$model == 'Model 2'   & .$term == 'townRochdale:closure' ~ TRUE,
                                 .$model == 'Model 2'   & .$term == 'townRochdale#closure' ~ TRUE,
                                 .$model == 'Model 3.1' & .$term == 'closure' ~ TRUE,
                                 .$model == 'Model 3.2' & .$term == 'closure' ~ TRUE,
                                 .$model == 'Model 4'   & .$term == 'closure' ~ TRUE,
                                 .$model == 'Model 5'   & .$term == 'closure' ~ TRUE,
                                 .$model == 'Model 6.1' & .$term == 'diff.time.to.ed' ~ TRUE,
                                 .$model == 'Model 6.2' & .$term == 'diff.time.to.ed' ~ TRUE,
                                 .$model == 'Model 7.1'  & .$term == 'diff.time.to.ed' ~ TRUE,
                                 .$model == 'Model 7.2'  & .$term == 'diff.time.to.ed' ~ TRUE,
                                 .$model == 'Model 8'   & .$term == 'diff.time.to.ed#closure' ~ TRUE,
                                 .$model == 'Model 8'   & .$term == 'diff.time.to.ed:closure' ~ TRUE),
                 use = ifelse(is.na(use), FALSE, use))
    df <- dplyr::filter(df, model == ma.model & use == TRUE) %>%
          dplyr::select(measure, sub.measure, town, model, term, est, stderr)
    #######################################################################
    ## Labels and captions conditional on outcome                        ##
    #######################################################################
    ## ToDo - Switch to parsing the data frames name, slightly easier/greater
    ##        internal consistency
    ## indicator <- substitute(df) %>% evaulate()
    ## print("Debug 1")
    if(indicator == 'ed attendances'){
        title1 <- 'Total ED Attendance'
        if(sub.indicator == 'any'){
            title2 <- ' (Any)'
            ## 2017-02-08 Remove Hartlepool at Jons request (email 2017-02-08; Subject : all sites Forest plots)
            df <- dplyr::filter(df, town != 'Hartlepool')
        }
        else if(sub.indicator == 'other'){
            title2 <- ' (Other)'
            ## 2017-02-08 Remove Hartlepool at Jons request (email 2017-02-08; Subject : all sites Forest plots)
            df <- dplyr::filter(df, town != 'Hartlepool')
        }
        else if(sub.indicator == 'ambulance'){
            title2 <- ' (Ambulance)'
        }
        null.line <- 1
        ma.measure = 'PLO'
        ## 2017-02-08 Remove Hemel Hempstead at Jons request (email 2017-02-08; Subject : all sites Forest plots)
        df <- dplyr::filter(df, town != 'Hemel Hempstead')
    }
    else if(indicator == 'unnecessary ed attendances'){
        title1 <- 'Minor Attendances'
        title2 <- ''
        null.line <- 1
        ma.measure = 'PLO'
        ## 2017-02-08 Remove Hartlepool & Hemel Hempstead at Jons request
        ## (email 2017-02-08; Subject : all sites Forest plots)
        df <- dplyr::filter(df, town != 'Hartlepool') %>%
              dplyr::filter(town != 'Hemel Hempstead')
    }
    else if(indicator == 'ed attendances admitted'){
        title1 <- 'ED Attendances Admitted'
        if(sub.indicator == 'all'){
            title2 <- ' (All)'
            null.line <- 1
        ma.measure = 'PLO'
        }
        else if(sub.indicator == 'fraction admitted'){
            title2 <- ' (Fraction)'
            null.line <- 0
        ma.measure = 'GEN'
        }
        else if(sub.indicator == 'admitted'){
            title2 <- ' (Absolute)'
            null.line <- 1
        ma.measure = 'PLO'
        }
    }
    else if(indicator == 'all emergency admissions'){
        title1 <- 'All Emergency Admissions'
        title2 <- ''
        null.line <- 1
        ma.measure = 'PLO'
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
        ma.measure = 'PLO'
    }
    else if(indicator == 'all emergency admissions'){
        title1 <- 'All Emergency Admissions'
        title2 <- ' (All)'
        null.line <- 1
        ma.measure = 'PLO'
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
        ma.measure = 'GEN'
    }
    else if(indicator == 'critical care stays'){
        title1 <- 'Critical Care Stays'
        if(sub.indicator == 'all'){
            title2 <- ' (All)'
            null.line <- 1
            ma.measure = 'PLO'
        }
        else if(sub.indicator == 'critical care'){
            title2 <- ' (Critical Care)'
            null.line <- 1
            ma.measure = 'PLO'
        }
        else if(sub.indicator == 'fraction critical care'){
            title2 <- ' (Fractional Critical Care)'
            null.line <- 0
        ma.measure = 'GEN'
        }
    }
    else if(indicator == 'case fatality ratio' | indicator == 'sec case fatality 7 days'){
        if(indicator == 'case fatality ratio'){
            title1 <- 'Case Fatality Ratio'
            null.line <- 0
        ma.measure = 'GEN'
        }
        else if(indicator == 'sec case fatality 7 days'){
            title1 <- 'Case Fatalities @ 7 Days'
            null.line <- 0
        ma.measure = 'GEN'
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
        ma.measure = 'GEN'
    }
    else if(indicator == 'ambulance non-conveyance'){
        title1 <- 'Ambulance Non-Conveyance'
        if(sub.indicator == 'fraction not conveyed'){
            title2 <- ' (Fraction)'
            null.line <- 0
            ma.measure = 'GEN'
        }
        else if(sub.indicator == 'green calls'){
            title2 <- ' (Green Calls)'
            null.line <- 1
            ma.measure = 'PLO'
        }
        else if(sub.indicator == 'not conveyed green calls'){
            title2 <- ' (Green Calls Not Conveyed)'
            null.line <- 1
            ma.measure = 'PLO'
        }
    }
    else if(indicator == 'ambulance green calls'){
        title1 <- 'Ambulance Green Calls'
        if(sub.indicator == 'fraction not conveyed'){
            title2 <- ' (Fraction)'
            null.line <- 0
            ma.measure = 'GEN'
        }
        else if(sub.indicator == 'green calls'){
            title2 <- ' (Green Calls)'
            null.line <- 1
        ma.measure = 'PLO'
        }
        else if(sub.indicator == 'hospital transfers'){
            title2 <- ' (Hospital Transfers)'
            null.line <- 1
        ma.measure = 'PLO'
        }
        else if(sub.indicator == 'not conveyed green calls'){
            title2 <- ' (Green Calls Not Conveyed)'
            null.line <- 1
        ma.measure = 'PLO'
        }
    }
    else if(indicator == 'ambulance red calls'){
        title1 <- 'Ambulance Red Calls'
        if(sub.indicator == 'hospital transfers')            title2 <- ' (Hospital Transfers)'
        else if(sub.indicator == 'total')                    title2 <- ' (Total)'
        null.line <- 1
        ma.measure = 'PLO'
    }
    else if(indicator == 'ambulance all calls'){
        title1 <- 'Ambulance All Calls'
        if(sub.indicator == 'total')            title2 <- ' (Total)'
        null.line <- 1
        ma.measure = 'PLO'
    }
    else if(indicator == 'hospital transfers'){
        title1 <- 'Hospital Transfers'
        if(sub.indicator == 'all stays')                   title2 <- ' (All Stays)'
        else if(sub.indicator == 'fraction with transfer') title2 <- ' (Fraction of Stays with Transfers)'
        else if(sub.indicator == 'stays with transfer')    title2 <- ' (Stays with Transfers)'
        null.line <- 1
        ma.measure = 'PLO'
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
        ma.measure = 'PLO'
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
        ma.measure = 'PLO'
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
        ma.measure = 'PLO'
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
        ma.measure = 'PLO'
    }
    else{
        title1 <- NULL
    }
    if(ma.model == 'Model 0')        title1 <- paste0('Site (Model 0) : ', title1)
    else if(ma.model == 'Model 1')   title1 <- paste0('Site (Model 1) : ', title1)
    else if(ma.model == 'Model 2')   title1 <- paste0('Site (Model 2) : ', title1)
    else if(ma.model == 'Model 6.1') title1 <- paste0('LSOA (Model 4) : ', title1)
    else if(ma.model == 'Model 7.1') title1 <- paste0('LSOA (Model 5) : ', title1)
    else if(ma.model == 'Model 8')   title1 <- paste0('LSOA (Model 6) : ', title1)
    ## print('Debug 1')
    ## print(df)
    ## Calculate the meta-analysis statistics
    ## 2017-02-14 - More changes *where is the brick wall*???
    ##              See email from Jon Nichol 2017-02-14 @ 12:10 Re : all sites Forest plot
    ##              The consequence is that we now explicitly specify the 'measure' option
    ##              having conditionally set it above
    results$meta.est <- rma(data   = df,
                            yi     = est,
                            sei    = stderr,
                            method = ma.method)
    ## print(results$meta.est)
    ## print('Explicit GEN')
    ## rma(data = df,
    ##     yi   = est,
    ##     sei  = stderr,
    ##     method = ma.method,
    ##     measure = 'GEN') %>% print()
    ## print('Explicit OR2DL')
    ## rma(data = df,
    ##     yi   = est,
    ##     sei  = stderr,
    ##     method = ma.method,
    ##     measure = 'OR2DL') %>% print()
    ## results$meta.pval <- results$meta.est$pval
    if(null.line == 0){
        results$meta.b  <- results$meta.est$b
        results$meta.se <- results$meta.est$se
        results$meta.z  <- results$meta.est$zval
        results$meta.p  <- results$meta.est$pval
    }
    else if(null.line == 1){
        results$meta.b  <- results$meta.est$b
        results$meta.se <- results$meta.est$se
        results$meta.z  <- abs(1 - results$meta.est$b) / results$meta.est$se
        results$meta.p  <- 2 * (1 - pnorm(results$meta.z))
    }
    results$extra <- cbind(results$meta.b, results$meta.se, results$meta.z, results$meta.p) %>% as.data.frame()
    names(results$extra) <- c('estimate', 'se', 'z (correct)', 'p (correct)')
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
    results$df$lci.other <- results$df$est - (1.96 * exp(results$df$stderr))
    results$df$uci.other <- results$df$est + (1.96 * exp(results$df$stderr))
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
                         y.axis = factor(y.axis))
                         ## No longer needed now 'Overall xx (xx - xx) has leading space
                         ## y.axis = factor(y.axis, levels = rev(levels(y.axis))))
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
        results$forest <- results$forest +
                          geom_vline(xintercept = null.line, linetype = 'dashed')
    }
    ## Conditionally label the x-axis
    if(ma.model == 'Model 1'){
        results$forest <- results$forest +
                          xlab('Estimated Coefficient for Closure of ED')
    }
    if(ma.model == 'Model 2'){
        results$forest <- results$forest +
                          xlab('Estimated Coefficient for interaction between Closure ED and Town')
    }
    else if(ma.model == 'Model 6.1'){
        results$forest <- results$forest +
                          xlab('Estimated Coefficient for Difference in Time to ED')
    }
    else if(ma.model == 'Model 8'){
        results$forest <- results$forest +
                          ## xlab('Estimated Coefficient for interaction of Difference in Time to ED (Binary) x Closure')
                          xlab('Estimated Coefficient for Dichotomised Difference in Time to ED x Closure')
    }
    ## Apply users theme
    if(!is.null(theme)){
        results$forest <- results$forest + theme
    }
    return(results)
}
