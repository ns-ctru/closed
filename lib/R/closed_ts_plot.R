#' Time-Series plots for the ClosED data
#'
#' @description Time-Series plots for the ClosED data
#'
#' @details The ClosED study uses time-series with dummy indicators to
#' test for the impact of closing Emergency Departments on indicators of
#' performance.  This function generates time-series plots and performs
#' Prais-Winsten time-series regression analysis to account for atuo-correlation.
#'
#' This function produces time-series plots in different formats.
#'
#' @param df Data frame to be plotted.
#' @param sites The sites that are to be plotted, default is for case sites.
#' @param indicator The performance indicator to assess.
#' @param sub.indicator The sub-measure performance indicator to assess.
#' @param steps Logical indicator of whether to plot vertical lines for each step.
#' @param smooth.plot Logical indicator of whether to overlay smoothed lines (\code{geom_smooth()})
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
closed_ts_plot <- function(df        = ed_attendances_by_mode_site_measure,
                           sites           = c('Bishop Auckland', 'Hartlepool', 'Hemel Hempstead', 'Newark', 'Rochdale'),
                           indicator       = 'ed attendances',
                           sub.indicator   = 'any',
                           steps           = TRUE,
                           smooth.plot    = TRUE,
                           common.y        = TRUE,
                           theme           = theme_bw(),
                           facet           = FALSE,
                           tidy            = FALSE,
                           join            = FALSE,
                           legend          = FALSE,
                           lines           = FALSE,
                           exclude.control = FALSE,
                           xaxis.steps     = TRUE,
                           fig             = '',
                           repel           = FALSE,
                           colour          = TRUE,
                           ...){
    ## Initialise results for returning
    results <- list()
    ## Conditionally select range for y-axis, MUST do this BEFORE subsetting
    ## data so that it is common across all outcomes for the given indicator
    if(common.y == TRUE){
        df.max <- max(df$value, na.rm = TRUE)
        y.max  <- max(df.max, df.max) %>%
                  round(-2)
    }
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
        ylabel <- 'N'
        y.text <- -10
        y.text.steps <- 1000
    }
    else if(indicator == 'unnecessary ed attendances'){
        title1 <- 'Unnecessary ED Attendances'
        title2 <- ''
        ylabel <- 'N'
        y.text.steps <- 1000
    }
    else if(indicator == 'ed attendances admitted'){
        title1 <- 'ED Attendances Admitted'
        if(sub.indicator == 'all')                    title2 <- ' (All)'
        else if(sub.indicator == 'fraction admitted') title2 <- ' (Fraction)'
        else if(sub.indicator == 'admitted')          title2 <- ' (Absolute)'
        ylabel <- 'N'
        if(sub.indicator %in% c('all', 'admitted')){
            y.text.steps <- 200
        }
        else if(sub.indicator %in% c('fraction admitted')){
            y.text.steps <- 0.05
        }
    }
    else if(indicator == 'all emergency admissions'){
        title1 <- 'All Emergency Admissions'
        title2 <- ''
        ylabel <- 'N'
        y.text.steps <- 1000
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
        ylabel <- 'N'
        y.text.steps <- 150
    }
    else if(indicator == 'all emergency admissions'){
        title1 <- 'All Emergency Admissions'
        title2 <- ' (All)'
        ylabel <- 'N'
        y.text.steps <- 1000
    }
    else if(indicator == 'length of stay'){
        title1 <- 'Length of Stay'
        if(sub.indicator == 'mean')        title2 <- ' (Mean)'
        else if(sub.indicator == 'median') title2 <- ' (Median)'
        ylabel <- 'Length of Stay (Days)'
        y.text.steps <- 5
    }
    else if(indicator == 'critical care stays'){
        title1 <- 'Critical Care Stays'
        if(sub.indicator == 'all')                           title2 <- ' (All)'
        else if(sub.indicator == 'critical care')            title2 <- ' (Critical Care)'
        else if(sub.indicator == 'fraction critical care') title2 <- ' (Fractional Critical Care)'
        ylabel <- 'N'
        if(sub.indicator == 'all'){
            y.text.steps <- 200
        }
        if(sub.indicator == 'critical care'){
            y.text.steps <- 200
        }
        else if(sub.indicator == 'fraction critical care'){
            y.text.steps <- 0.1
        }
    }
    else if(indicator == 'case fatality ratio' | indicator == 'sec case fatality 7 days'){
        title1 <- 'Case Fatality Ratio'
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
        nudge <- 0.5
        ylabel <- 'Case Fatality Ratio'
        y.text <- -0.05
        y.text.steps <- 0.05
    }
    else if(indicator == 'ambulance mean times'){
        title1 <- 'Ambulance Mean Times'
        if(sub.indicator == 'call to dest' | sub.indicator == 'call_to_dest'){
            title2 <- ' (Call to Destination)'
            y.text.steps <- 30
        }
        else if(sub.indicator == 'call to scene any' | sub.indicator == 'call_to_scene_any'){
            title2 <- ' (Call to Scene, Any)'
            y.text.steps <- 5
        }
        else if(sub.indicator == 'call to scene conveying' | sub.indicator == 'call_to_scene_conveying'){
            title2 <- ' (Call to Scene, Conveying)'
            y.text.steps <- 5
        }
        else if(sub.indicator == 'dest to clear' | sub.indicator == 'dest_to_clear'){
            title2 <- ' (Destination to Clear)'
            y.text.steps <- 10
        }
        else if(sub.indicator == 'scene to dest' | sub.indicator == 'scene_to_dest'){
            title2 <- ' (Scene to Destination)'
            y.text.steps <- 15
        }
        nudge <- 10
        ylabel <- 'Mean Time (minutes)'
    }
    else if(indicator == 'ambulance non-conveyance'){
        title1 <- 'Ambulance Non-Conveyance'
        if(sub.indicator == 'fraction not conveyed')                 title2 <- ' (Fraction)'
        else if(sub.indicator == 'green calls')             title2 <- ' (Green Calls)'
        else if(sub.indicator == 'not conveyed green calls') title2 <- ' (Green Calls Not Conveyed)'
        nudge <- 10
        ylabel <- 'N'
        y.text.steps <- 200
    }
    else if(indicator == 'ambulance green calls'){
        title1 <- 'Ambulance Green Calls'
        if(sub.indicator == 'fraction not conveyed'){
            title2 <- ' (Fraction)'
            ylabel <- 'Proportion'
        }
        else if(sub.indicator == 'green calls'){
            title2 <- ' (Green Calls)'
            ylabel <- 'N'
        }
        else if(sub.indicator == 'hospital transfers'){
            title2 <- ' (Hospital Transfers)'
            ylabel <- 'N'
        }
        else if(sub.indicator == 'not conveyed green calls'){
            title2 <- ' (Green Calls Not Conveyed)'
            ylabel <- 'N'
        }
        nudge <- 10
        y.text.steps <- 200
    }
    else if(indicator == 'ambulance red calls'){
        title1 <- 'Ambulance Red Calls'
        if(sub.indicator == 'hospital transfers')            title2 <- ' (Hospital Transfers)'
        else if(sub.indicator == 'total')                    title2 <- ' (Total)'
        nudge <- 10
        ylabel <- 'N'
        y.text.steps <- 20
    }
    else if(indicator == 'hospital transfers'){
        title1 <- 'Hospital Transfers'
        if(sub.indicator == 'all stays')                   title2 <- ' (All Stays)'
        else if(sub.indicator == 'fraction with transfer') title2 <- ' (Fraction of Stays with Transfers)'
        else if(sub.indicator == 'stays with transfer')    title2 <- ' (Stays with Transfers)'
        nudge <- 10
        if(sub.indicator == 'all stays')                   ylabel <- 'N (Stays)'
        else if(sub.indicator == 'fraction with transfer') ylabel <- 'Proportion of Stays with Transfers)'
        else if(sub.indicator == 'stays with transfer')    ylabel <- 'N (Stays with Transfers)'
        y.text.steps <- 100
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
        nudge <- 0.5
        ylabel <- 'Case Fatality Ratio @ 7 Days'
        y.text <- -0.05
        y.text.steps <- 0.05
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
        nudge <- 0.5
        ylabel <- 'Deaths @ 7 Days'
        y.text <- -0.05
        y.text.steps <- 0.05
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
        nudge <- 0.5
        ylabel <- 'Deaths in CIPS @ 7 Days'
        y.text <- -0.05
        y.text.steps <- 0.05
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
        nudge <- 0.5
        ylabel <- 'Deaths not in CIPS @ 7 Days'
        y.text <- -0.05
        y.text.steps <- 0.05
    }
    #######################################################################
    ## Define vertical lines for steps                                   ##
    #######################################################################
    ## print("Debug 2")
    steps        <- c(24.5)
    steps.labels <- c('ED Closure')
    town         <- c('')
    variable     <- c(1)
    if('Bishop Auckland' %in% sites){
        steps <- c(steps, 35)
        steps.labels <- c(steps.labels, 'NHS111')
        town <- c(town, 'Bishop Auckland')
        variable <- c(variable, 2)
    }
    if('Grimsby' %in% sites){
        steps <- c(steps, 16)
        steps.labels <- c(steps.labels, 'NHS111')
        town <- c(town, 'Grimsby (Primary)')
        variable <- c(variable, 2)
    }
    if('Hartlepool' %in% sites){
        steps <- c(steps, 45, 22)
        steps.labels <- c(steps.labels, 'NHS111', 'Other Centre')
        town <- c(town, 'Hartlepool', 'Hartlepool')
        variable <- c(variable, 2, 3)
    }
    if('Hemel Hempstead' %in% sites){
        steps <- c(steps, 20)
        steps.labels <- c(steps.labels, 'Other Centre')
        town <- c(town, 'Hemel Hempstead')
        variable <- c(variable, 3)
    }
    if('Newark' %in% sites){
        steps <- c(steps, 3)
        steps.labels <- c(steps.labels, 'Other Centre')
        town <- c(town, 'Newark')
        variable <- c(variable, 3)
    }
    if('Rochdale' %in% sites){
        steps <- c(steps, 48, 11, 17)
        steps.labels <- c(steps.labels, 'NHS111', 'Other Centre', 'Ambulance Diversion')
        town <- c(town, 'Rochdale', 'Rochdale', 'Rochdale')
        variable <- c(variable, 2, 3, 4)
    }
    if('Rotherham' %in% sites){
        steps <- c(steps, 48)
        steps.labels <- c(steps.labels, 'NHS111')
        town <- c(town, 'Rotherham (Primary)')
        variable <- c(variable, 2)
    }
    if('Southport' %in% sites){
        steps <- c(steps, 48)
        steps.labels <- c(steps.labels, 'NHS111')
        town <- c(town, 'Southport (Primary)')
        variable <- c(variable, 2)
    }
    if('Warwick' %in% sites){
        steps <- c(steps)
        steps.labels <- c(steps.labels)
        town <- c(town)
        variable <- c(variable)
    }
    if('Whitehaven' %in% sites){
        steps <- c(steps)
        steps.labels <- c(steps.labels)
        town <- c(town)
        variable <- c(variable)
    }
    ## Bind into a dataframe
    df.steps <- data.frame(steps, steps.labels, town, variable)
    df.steps$xaxis.steps.labels <- paste0(steps.labels,
                                          " (",
                                          town,
                                          ")")
    df.steps$xaxis.steps.labels[df.steps$xaxis.steps.labels == 'ED Closure ()'] <- NA
    ## df.steps <- dplyr::filter(df.steps, steps.labels != 'ED Closure')
    df.steps$town <- factor(df.steps$town)
    df.steps$steps.labels <- factor(df.steps$steps.labels)
    df.steps$variable <- as.integer(df.steps$variable)
    df.steps$variable <- factor(df.steps$variable,
                                levels = c(1:4),
                                labels = c('ED Closure', 'NHS 111', 'Other Centre', 'Ambulance Diversion'))
    ## results$df.steps <- df.steps
    #######################################################################
    ## Identify and remove spurious data points                          ##
    #######################################################################
    ## TODO - Convert to calls to closed_clean()
    ## print("Debug 3")
    if(tidy == TRUE){
        ## Condition on the indicator and sub indicator
        if(indicator == 'ed attendances' & sub.indicator == 'any'){
            if('Bishop Auckland' %in% sites){
                df <- mutate(df,
                             value = ifelse(relative.month %in% c(1, 6) & town == 'Bishop Auckland', yes = NA, no = value))
            }
        }
        else if(indicator == 'ed attendances' & sub.indicator == 'ambulance'){
            if('Bishop Auckland' %in% sites){
                df <- mutate(df,
                             value = ifelse(relative.month %in% c(1, 6) & town == 'Bishop Auckland', yes = NA, no = value))
            }
        }
        else if(indicator == 'ed attendances' & sub.indicator == 'other'){
            if('Bishop Auckland' %in% sites){
                df <- mutate(df,
                             value = ifelse(relative.month %in% c(1, 6) & town == 'Bishop Auckland', yes = NA, no = value))
            }
        }
        else if(indicator == 'unnecessary ed attendances' & is.na(sub.indicator)){
            if('Bishop Auckland' %in% sites){
                df <- mutate(df,
                             value = ifelse(relative.month %in% c(1, 6) & town == 'Bishop Auckland', yes = NA, no = value))
            }
        }
        else if(indicator == 'ed attendances admitted' & sub.indicator == 'all'){
            if('Bishop Auckland' %in% sites){
                df <- mutate(df,
                             value = ifelse(relative.month %in% c(1, 6) & town == 'Bishop Auckland', yes = NA, no = value))
            }
        }
        else if(indicator == 'ed attendances admitted' & sub.indicator == 'admitted'){
            if('Bishop Auckland' %in% sites){
                df <- mutate(df,
                             value = ifelse(relative.month %in% c(1, 6) & town == 'Bishop Auckland', yes = NA, no = value))
            }
        }
        else if(indicator == 'ed attendances admitted' & sub.indicator == 'fraction'){
            if('Hemel Hempstead' %in% sites){
                df <- mutate(df,
                             value = ifelse(relative.month %in% c(1) & town == 'Hemel Hempstead', yes = NA, no = value))
            }
        }
        else if(indicator == 'critical care stays' & sub.indicator == 'all'){
            if('Newark' %in% sites){
                df <- mutate(df,
                             value = ifelse(relative.month %in% c(47) & town == 'Newark', yes = NA, no = value))
            }
        }
        ## Remove data points that are now missing
        if(join == TRUE){
            df <- dplyr::filter(df, !is.na(value))
        }
    }
    #######################################################################
    ## Plot!                                                             ##
    #######################################################################
    ## Subset data
    ## print("Debug 4")
    df <- dplyr::filter(df,
                  town %in% sites &
                  measure     == indicator &
                  sub.measure == sub.indicator)
    ## Add indicator of primary matched control
    df$town <- as.character(df$town)
    df$town <- ifelse(df$town %in% c('Whitehaven', 'Grimsby', 'Warwick', 'Southport', 'Rotherham'), paste0(df$town, ' (Primary)'), df$town)
    df$town <- factor(df$town)
    ## Add linetype based on the remaining centers that are being plotted
    ## print("Debug 5")
    ## df$linetype <- as.numeric(levels(df$town))
    ## print("Debug 6")
    if(colour == TRUE){
        results$plot <- ggplot(data = df,
                               mapping = aes(x     = relative.month,
                                             y     = value,
                                             colour  = town))
    }
    else{
        results$plot <- ggplot(data = df,
                               mapping = aes(x     = relative.month,
                                             y     = value,
                                             linetype = town))
    }
    ## Add smoothed line
    if(smooth.plot == TRUE){
        ## print('Are we smoothing?')
        results$plot <- results$plot + geom_point(aes(shape = town)) + geom_smooth(aes(linetype = town), method = 'loess')
    }
    ## Basic line plot
    ## results$plot <- results$plot + geom_line(linetype = linetype) +
    else{
        ## print('Are we drawing a line graph?')
        results$plot <- results$plot + geom_line()
        ## Graph and axis labels
    }
    ## Add graph and axis labels
    results$plot <- results$plot + labs(list(title  = paste0(fig,
                                                             title1,
                                                             title2),
                                             caption = paste0('Generated : ', Sys.time()),
                                             x      = 'Month (Aligned)',
                                             y      = ylabel,
                                             colour = 'Hospital Catchment Area',
                                             linetype = 'Hospital Catchment Area'))
    ## Label lines
    if(repel == TRUE){
        if(colour == TRUE){
            print('Adding colour?')
            results$plot <- results$plot + geom_text_repel(data = dplyr::filter(df, relative.month == 3),
                                                           aes(relative.month,
                                                               value,
                                                               colour = town,
                                                               linetype  = town),
                                                           force   = 1,
                                                           nudge_x = 0,
                                                           nudge_y = 0)
        }
        else{
            print('No colour?')
            results$plot <- results$plot + geom_text_repel(data = dplyr::filter(df, relative.month == 3),
                                                           aes(relative.month,
                                                               value,
                                                               label  = town),
                                                           force   = 1,
                                                           nudge_x = 0,
                                                           nudge_y = 0)
        }
    }
    ## Add vertical lines for all steps
    if(lines == TRUE){
        ## If pooled then only include the steps for Case sites
        if(exclude.control == TRUE){
            df.steps <- dplyr::filter(df.steps, town %in% c('Bishop Auckland', 'Hartlepool', 'Hemel Hempstead', 'Newark', 'Rochdale', 'ED Closure'))
        }
        if(colour == TRUE){
            results$plot <- results$plot + geom_vline(data = df.steps,
                                                      mapping = aes(xintercept = steps,
                                                                    colour     = town,
                                                                    linetype   = variable)) +
                            labs(linetype = 'Modelled Changes', colour = 'Hospital Catchment Area')
        }
        else{
            results$plot <- results$plot + geom_vline(data = df.steps,
                                                      mapping = aes(xintercept = steps,
                                                                    label      = town,
                                                                    linetype   = variable)) +
                            labs(linetype = 'Modelled Changes', colour = 'Hospital Catchment Area')

        }
    }
    ## Scale y-axis to have range 0-1 if this is case fatality ratio
    if(indicator == 'case fatality ratio'){
        results$plot <- results$plot + scale_y_continuous(limits = c(0, 0.5))
    }
    ## else{
    ##     results$plot <- results$plot + scale_y_continuous(limits = c(0, max(df$value)))
    ## }
    ## X-axis labels for events
    if(xaxis.steps == TRUE){
        results$plot <- results$plot +
                        annotate(geom   = 'text',
                                 x      = df.steps$steps,
                                 y      = y.text.steps,
                                 label  = df.steps$xaxis.steps.labels, angle = 90) +
                        geom_vline(xintercept = 24.5)
    }
    ## Facet
    if(facet == TRUE){
        results$plot <- results$plot + facet_wrap(~ group, ncol = 1)
    }
    ## Apply user specified theme to all graphs
    if(!is.null(theme)){
        results$plot <- results$plot + theme
    }
    if(legend == FALSE){
        results$plot <- results$plot +
                        theme(legend.position = 'none')
    }
    ## Return results
    return(results$plot)
}
