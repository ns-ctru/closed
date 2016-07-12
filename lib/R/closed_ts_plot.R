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
#' @param pool.control Logical indicator of whether to pool controls.
#' @param steps Logical indicator of whether to plot vertical lines for each step.
#' @param common.y Generate all plots with a common y-axis range.
#' @param theme GGplot2 theme to use.
#' @param tidy Logical indicator of whether to remove spurious data points when plotting
#' @param join Logical indicator of whether to completely remove time points with spurious data so that lines are continuous
#' @param legend Logical indicator of whether to include a legend
#' @param lines Logical indicator of whether to include a vertical line for steps.
#' @param xaxis.steps Logical indicator of whether to add x-axis labels for steps.
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
                           pool.control    = FALSE,
                           steps           = TRUE,
                           common.y        = TRUE,
                           theme           = theme_bw(),
                           facet           = FALSE,
                           tidy            = FALSE,
                           join            = FALSE,
                           legend          = FALSE,
                           lines           = TRUE,
                           xaxis.steps     = FALSE,
                           ...){
    ## Initialise results for returning
    results <- list()
    ## 2016-05-24 - For a small number of outcomes there is no sub-indicator
    ##              and it is therefore missing.  In order to work with this
    ##              function such missing values are therefore replaced with
    ##              the main indicator which is supplied as the sub.indicator
    ##              argument
    ## which.df <- substitute(df) %>% deparse()
    if(indicator == 'unnecessary_ed_attendances_site_measure'){
        df$sub.measure <- ifelse(is.na(df$sub.measure), 'all', df$sub.measure)
    }
    if(indicator == 'all emergency admissions'){
        df$sub.measure <- ifelse(is.na(df$sub.measure), 'all', df$sub.measure)
    }
    ## Convert variable names for ease of typing within this function
    ## (ESS artefact, hitting underscore inserts '<-' so lots of underscores are
    ## tedious to type)
    names(df) <- names(df) %>%
        gsub("_", ".", x = .)
    ## Convert to data frame,  and convert
    ## town to factor so that it can be releveled as required
    df <- as.data.frame(df)
    df$town <- factor(df$town)
    ## df.steps <- as.data.frame(df.steps)
    ## names(df.steps) <- names(df.steps) %>%
    ##                    gsub("_", ".", x = .)
    ## Conditionally select range for y-axis, MUST do this BEFORE subsetting
    ## data so that it is common across all outcomes for the given indicator
    if(common.y == TRUE){
        df.max <- max(df$value, na.rm = TRUE)
        y.max  <- max(df.max, df.max) %>%
                  round(-2)
    }
    #######################################################################
    ## Add a dummy 'step' for closure                                    ##
    #######################################################################
    ## 2016-05-24 - Post meeting with Jon, this should be 0/1 for _all_ sites not
    ##              just intervention ones
    df$closure <- ifelse(df$relative.month > 24, 1, 0)
    #######################################################################
    ## Add dummy for other 'steps'                                       ##
    ##                                                                   ##
    ## See list from e.l.knowles@sheffield.ac.uk at...                   ##
    ##                                                                   ##
    ## https://goo.gl/TlhfCF                                             ##
    ##                                                                   ##
    #######################################################################
    df <- mutate(df,
                       nhs111 = ifelse((town == 'Bishop Auckland' & relative.month >= 35) |
                                       (town == 'Southport' & relative.month >= 48) |
                                       ## ToDo - Uncomment once confirmed and revised dates available
                                       (town == 'Rochdale' & relative.month >= 48) |
                                       (town == 'Rotherham' & relative.month >= 48) |
                                       (town == 'Hartlepool' & relative.month >= 45) |
                                       (town == 'Grimsby' & relative.month >= 16),
                                       1, 0),
                       ambulance.divert = ifelse(town == 'Rochdale' & relative.month >= 17, 1, 0),
                       other.centre = ifelse((town == 'Hemel Hempstead' & relative.month >= 20) |
                                             (town == 'Newark' & relative.month >= 3) |
                                             (town == 'Rochdale' & relative.month >= 11) |
                                             (town == 'Hartlepool' & relative.month >= 22),
                                             1, 0)
                 )
    #######################################################################
    ## Labels and captions conditional on outcome                        ##
    #######################################################################
    ## ToDo - Switch to parsing the data frames name, slightly easier/greater
    ##        internal consistency
    ## indicator <- substitute(df) %>% evaulate()
    if(indicator == 'ed attendances'){
        title1 <- 'ED Attendance'
        if(sub.indicator == 'any')            title2 <- ' (Any)'
        else if(sub.indicator == 'other')     title2 <- ' (Other)'
        else if(sub.indicator == 'ambulance') title2 <- ' (Ambulance)'
        ylabel <- 'N'
        y.text <- -10
    }
    else if(indicator == 'unnecessary ed attendances'){
        title1 <- 'Unnecessary ED Attendances'
        title2 <- '(All)'
        ylabel <- 'N'
    }
    else if(indicator == 'ed attendances admitted'){
        title1 <- 'ED Attendances Admitted'
        if(sub.indicator == 'all')                    title2 <- ' (All)'
        else if(sub.indicator == 'fraction admitted') title2 <- ' (Fraction Admitted)'
        else if(sub.indicator == 'admitted')          title2 <- ' (Admitted)'
        ylabel <- 'N'
    }
    else if(indicator == 'all emergency admissions'){
        title1 <- 'Emergency Admissions'
        title2 <- ''
        ylabel <- 'N'
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
    }
    else if(indicator == 'all emergency admissions'){
        title1 <- 'All Emergency Admissions'
        title2 <- ' (All)'
        ylabel <- 'N'
    }
    else if(indicator == 'length of stay'){
        title1 <- 'Length of Stay'
        if(sub.indicator == 'mean')        title2 <- ' (Mean)'
        else if(sub.indicator == 'median') title2 <- ' (Median)'
        ylabel <- 'Length of Stay (Days)'
    }
    else if(indicator == 'critical care stays'){
        title1 <- 'Critical Care Stays'
        if(sub.indicator == 'all')                           title2 <- ' (All)'
        else if(sub.indicator == 'critical care')            title2 <- ' (Critical Care)'
        else if(sub.indicator == 'fraction critical care') title2 <- ' (Fractional Critical Care)'
        ylabel <- 'N'
    }
    else if(indicator == 'case fatality ratio'){
        title1 <- 'Case Fatality Ratio'
        if(sub.indicator == 'acute heart failure')              title2 <- ' (Acute Heart Failure)'
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
        ylabel <- 'Case Fatality Ratio'
        y.text <- -0.05
    }
    else if(indicator == 'ambulance mean times'){
        title1 <- 'Ambulance Mean Times'
        if(sub.indicator == 'call_to_dest')                 title2 <- ' (Call to Destination)'
        else if(sub.indicator == 'call_to_scene_any')       title2 <- ' (Call to Scene, Any)'
        else if(sub.indicator == 'call_to_scene_conveying') title2 <- ' (Call to Scene, Conveying)'
        else if(sub.indicator == 'dest_to_clear')           title2 <- ' (Destination to Clear)'
        else if(sub.indicator == 'scene_to_dest')           title2 <- ' (Scene to Destination)'
        nudge <- 10
        ylabel <- 'Mean Time (minutes)'
    }
    #######################################################################
    ## Define vertical lines for steps                                   ##
    #######################################################################
    steps        <- c(25)
    steps.labels <- c('ED Closure')
    town         <- c('ED Closure')
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
        town <- c(town, 'Grimsby')
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
        town <- c(town, 'Rotherham')
        variable <- c(variable, 2)
    }
    if('Southport' %in% sites){
        steps <- c(steps, 48)
        steps.labels <- c(steps.labels, 'NHS111')
        town <- c(town, 'Southport')
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
    ## df.steps <- filter(df.steps, steps.labels != 'ED Closure')
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
            df <- filter(df, !is.na(value))
        }
    }
    #######################################################################
    ## Plot!                                                             ##
    #######################################################################
    ## Subset data
    df <- filter(df,
                  town %in% sites &
                  measure     == indicator &
                  sub.measure == sub.indicator)
    #######################################################################
    ## Pool Controls                                                     ##
    #######################################################################
    if(pool.control == TRUE){
        ## Derive indicator
        df <- mutate(df,
                     pooled.control = ifelse(site.type %in% c('matched control', 'pooled control'), 'Control', town))
        ## Sum value by pooled.control and month
        df <- group_by(df,
                       pooled.control, relative.month) %>%
              summarise(value = sum(value))

    }
    ## Generate time-series plot
    if(pool.control == FALSE){
        results$plot <- ggplot(data = df,
                               mapping = aes(x     = relative.month,
                                             y     = value,
                                             color = town))
    }
    else if(pool.control == TRUE){
        results$plot <- ggplot(data = df,
                               mapping = aes(x     = relative.month,
                                             y     = value,
                                             color = pooled.control))
    }
            ## Basic line plot
    results$plot <- results$plot + geom_line() +
            ## Graph and axis labels
            labs(list(title  = paste0(title1, title2),
                      x      = 'Month (Aligned)',
                      y      = ylabel,
                      colour = 'Hospital')) +
            ## Label lines
            geom_text_repel(data = filter(df, relative.month == 3),
                            aes(relative.month,
                                value,
                                colour = town,
                                label  = town),
                            force   = 1,
                            nudge_x = 0,
                            nudge_y = 0)
    ## Add vertical lines for all steps
    if(lines == TRUE){
        ## If pooled then only include the steps for Case sites
        if(pool.control == TRUE){
            df.steps <- filter(df.steps,
                               town %in% c('Bishop Auckland', 'Hartlepool', 'Hemel Hempstead', 'Newark', 'Rochdale'))
        }
        results$plot <- results$plot + geom_vline(data = df.steps,
                                                  mapping = aes(xintercept = steps,
                                                                color      = town,
                                                                linetype   = variable))
    }
    ## Scale y-axis to have range 0-1 if this is case fatality ratio
    if(indicator == 'case fatality ratio'){
        results$plot <- results$plot + scale_y_continuous(limits = c(0, 1))
    }
    ## X-axis labels for events
    if(xaxis.steps == TRUE){
        results$plot <- results$plot + annotate(geom   = 'text',
                                                x      = df.steps$steps,
                                                y      = y.text,
                                                label  = steps.labels)
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
