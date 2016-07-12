#' Tidy ClosED data frames to remove spurious data points
#'
#' @description Remove spurious data points from a given data frame for analysis.
#'
#' @details The ClosED study uses time-series with dummy indicators to
#' test for the impact of closing Emergency Departments on indicators of
#' performance.  The data is derived from various sources including the Health
#' and Social Care Information Commission (HSCIC), Office for National Statistics
#' (ONS) and various Ambulance Services (AS).  Unfortuantely it is of variable
#' quality and there are a number of instances where, after summarising the data
#' by month and plotting as a time-series, it is clear that there are problems
#' with the underlying data in terms of accuracy.  This function therefore cleans
#' the data set prior to analyses (or plotting).
#'
#' Currently spurious data points to be cleaned are internally coded.
#'
#' **NB** This function works with only one data frame at a time, there are two for
#'        each outcome (Trust and LSOA level data).  Both datasets require cleaning
#'        so remember to clean both.
#'
#' @param df Data frame to analyse clean.
#' @param indicator The performance indicator to assess.
#' @param sub.indicator The sub-measure performance indicator to assess.
#' @param systematic Systematic removal of spurious data points, as a multiple of the Standard Deviation within a ED site.
#' @param balance Balance the panels after removal of spurious data points.
#'
#' @return A cleaned data frame with spurious data points removed.
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
closed_clean <- function(df              = ed_attendances_by_mode_measure,
                         indicator       = 'ed attendances',
                         sub.indicator   = 'any',
                         systematic      = 2,
                         balance         = TRUE,
                         ...){
    ## Systematic removal
    if(!is.na(systematic)){
        df <- group_by(df, town, sub.measure) %>%
              mutate(mean      = mean(value, na.rm = TRUE),
                     sd        = sd(value, na.rm = TRUE))
        df <- mutate(df,
                     value = ifelse(value < (mean - systematic * sd) |
                                    value > (mean + systematic * sd),
                                    yes = NA,
                                    no  = value))
        df <- dplyr::select(df, -c(mean, sd))
        return(df)
    }
    ## Clean the data set conditional on the Indicator and in turn Sub-indicator
    else{
        if(indicator == 'ed attendances'){
            if(sub.indicator == 'any'){
                if(balance == TRUE){
                    df <- mutate(df,
                                 value = ifelse(sub.measure == sub.indicator &
                                                relative.month %in% c(1, 6, 24),
                                                yes = NA,
                                                no  = value))
                }
                else{
                    df <- mutate(df,
                                 value = ifelse(town == 'Basingstoke' &
                                                sub.measure == sub.indicator &
                                                relative.month %in% c(1:9, 31:47),
                                                yes = NA,
                                                no  = value),
                                 value = ifelse(town == 'Bishop Auckland' &
                                                sub.measure == sub.indicator &
                                                relative.month %in% c(1, 6),
                                                yes = NA,
                                                no  = value),
                                 value = ifelse(town == 'Yeovil' &
                                                sub.measure == sub.indicator &
                                                relative.month %in% c(25),
                                                yes = NA,
                                                no  = value),
                                 )
                }
            }
            if(sub.indicator == 'ambulance'){
                if(balance == TRUE){
                    df <- mutate(df,
                                 value = ifelse(sub.measure == sub.indicator &
                                                relative.month %in% c(1, 6),
                                                yes = NA,
                                                no = value))
                }
                else{
                    df <- mutate(df,
                                 value = ifelse(town == 'Basingstoke' &
                                                sub.measure == sub.indicator &
                                                relative.month %in% c(1:9, 31:47),
                                                yes = NA,
                                                no = value),
                                 value = ifelse(town == 'Bishop Auckland' &
                                                sub.measure == sub.indicator &
                                                relative.month %in% c(1, 6),
                                                yes = NA,
                                                no = value),
                                 value = ifelse(town == 'Grimsby' &
                                                sub.measure == sub.indicator &
                                                relative.month %in% c(1:12),
                                                yes = NA,
                                                no  = value),
                                 value = ifelse(town == 'Scunthorpe' &
                                                sub.measure == sub.indicator &
                                                relative.month %in% c(1:16),
                                                yes = NA,
                                                no = value),
                                 value = ifelse(town == 'Yeovil' &
                                                sub.measure == sub.indicator &
                                                relative.month %in% c(1:16),
                                                yes = NA,
                                                no = value))
                }
            }
            if(sub.indicator == 'other'){
                if(balance == TRUE){
                    df <- mutate(df,
                                 value = ifelse(sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
                else{
                    df <- mutate(df,
                                 value = ifelse(town == 'Basingstoke' &
                                                sub.measure == sub.indicator &
                                                relative.month %in% c(1:9, 31:47),
                                                yes = NA,
                                                no = value),
                                 value = ifelse(town == 'Bishop Auckland' &
                                                sub.measure == sub.indicator &
                                                relative.month %in% c(1, 6),
                                                yes = NA,
                                                no = value),
                                 value = ifelse(town == 'Southport' &
                                                sub.measure == sub.indicator &
                                                relative.month %in% c(24),
                                                yes = NA,
                                                no = value),
                                 value = ifelse(town == 'Yeovil' &
                                                sub.measure == sub.indicator &
                                                relative.month %in% c(1:25),
                                                yes = NA,
                                                no = value))
                }
            }
        }
        else if(indicator == 'unnecessary attendances'){
            if(sub.indicator == 'unnecessary attendances'){
                if(balance == TRUE){
                    df <- mutate(df,
                                 value = ifelse(sub.measure == sub.indicator &
                                                relative.month %in% c(1, 6),
                                                yes = NA,
                                                no = value))
                }
                else{
                    df <- mutate(df,
                                 value = ifelse(town == 'Bishop Auckland' &
                                                sub.measure == sub.indicator &
                                                relative.month %in% c(1, 6),
                                                yes = NA,
                                                no = value),
                                 value = ifelse(town == 'Yeovil' &
                                                sub.measure == sub.indicator &
                                                relative.month %in% c(25),
                                                yes = NA,
                                                no = value))
                }
            }
        }
        else if(indicator == 'all emergency admissions'){
            if(sub.indicator == 'all emergency admissions'){
                if(balance == TRUE){
                    df <- mutate(df,
                                 value = ifelse(sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
                else{
                    df <- mutate(df,
                                 value = ifelse(town == 'Newark' &
                                                sub.measure == sub.indicator &
                                                relative.month %in% c(49),
                                                yes = NA,
                                                no = value))
                }
            }
        }
        else if(indicator == 'avoidable emergency admissions'){
            if(sub.indicator == 'any'){
                if(balance == TRUE){
                    df <- mutate(df,
                                 value = ifelse(sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
                else{
                    df <- mutate(df,
                                 value = ifelse(town == '' &
                                                sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
            }
            if(sub.indicator == 'acute mental health crisis'){
                if(balance == TRUE){
                    df <- mutate(df,
                                 value = ifelse(sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
                else{
                    df <- mutate(df,
                                 value = ifelse(town == '' &
                                                sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
            }
            if(sub.indicator == 'angina'){
                if(balance == TRUE){
                    df <- mutate(df,
                                 value = ifelse(sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
                else{
                    df <- mutate(df,
                                 value = ifelse(town == '' &
                                                sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
            }
            if(sub.indicator == 'blocked catheter'){
                if(balance == TRUE){
                    df <- mutate(df,
                                 value = ifelse(sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
                else{
                    df <- mutate(df,
                                 value = ifelse(town == '' &
                                                sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
            }
            if(sub.indicator == 'cellulitis'){
                if(balance == TRUE){
                    df <- mutate(df,
                                 value = ifelse(sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
                else{
                    df <- mutate(df,
                                 value = ifelse(town == '' &
                                                sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
            }
            if(sub.indicator == 'copd'){
                if(balance == TRUE){
                    df <- mutate(df,
                                 value = ifelse(sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
                else{
                    df <- mutate(df,
                                 value = ifelse(town == '' &
                                                sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
            }
            if(sub.indicator == 'dvt'){
                if(balance == TRUE){
                    df <- mutate(df,
                                 value = ifelse(sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
                else{
                    df <- mutate(df,
                                 value = ifelse(town == '' &
                                                sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
            }
            if(sub.indicator == 'epileptic fit'){
                if(balance == TRUE){
                    df <- mutate(df,
                                 value = ifelse(sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
                else{
                    df <- mutate(df,
                                 value = ifelse(town == '' &
                                                sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
            }
            if(sub.indicator == 'falls (75+ years)'){
                if(balance == TRUE){
                    df <- mutate(df,
                                 value = ifelse(sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
                else{
                    df <- mutate(df,
                                 value = ifelse(town == '' &
                                                sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
            }
            if(sub.indicator == 'hypoglycaemia'){
                if(balance == TRUE){
                    df <- mutate(df,
                                 value = ifelse(sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
                else{
                    df <- mutate(df,
                                 value = ifelse(town == '' &
                                                sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
            }
            if(sub.indicator == 'minor head injuries'){
                if(balance == TRUE){
                    df <- mutate(df,
                                 value = ifelse(sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
                else{
                    df <- mutate(df,
                                 value = ifelse(town == '' &
                                                sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
            }
            if(sub.indicator == 'non-specific chest pain'){
                if(balance == TRUE){
                    df <- mutate(df,
                                 value = ifelse(sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
                else{
                    df <- mutate(df,
                                 value = ifelse(town == '' &
                                                sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
            }
            if(sub.indicator == 'pyrexial child (<6 years)'){
                if(balance == TRUE){
                    df <- mutate(df,
                                 value = ifelse(sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
                else{
                    df <- mutate(df,
                                 value = ifelse(town == '' &
                                                sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
            }
            if(sub.indicator == 'urinary tract infection'){
                if(balance == TRUE){
                    df <- mutate(df,
                                 value = ifelse(sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
                else{
                    df <- mutate(df,
                                 value = ifelse(town == '' &
                                                sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
            }
        }
        else if(indicator == 'ed attendances admitted'){
            if(sub.indicator == 'all'){
                if(balance == TRUE){
                    df <- mutate(df,
                                 value = ifelse(sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
                else{
                    df <- mutate(df,
                                 value = ifelse(town == 'Basingstoke' &
                                                sub.measure == sub.indicator &
                                                relative.month %in% c(1:9, 31:47),
                                                yes = NA,
                                                no = value),
                                 value = ifelse(town == 'Bishop Auckland' &
                                                sub.measure == sub.indicator &
                                                relative.month %in% c(1, 6),
                                                yes = NA,
                                                no = value),
                                 value = ifelse(town == 'Southpor' &
                                                sub.measure == sub.indicator &
                                                relative.month %in% c(24),
                                                yes = NA,
                                                no = value),
                                 value = ifelse(town == 'Yeovil' &
                                                sub.measure == sub.indicator &
                                                relative.month %in% c(25),
                                                yes = NA,
                                                no = value))
                }
            }
            if(sub.indicator == 'fraction admitted'){
                if(balance == TRUE){
                    df <- mutate(df,
                                 value = ifelse(sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
                else{
                    df <- mutate(df,
                                 value = ifelse(town == 'Basingstoke' &
                                                sub.measure == sub.indicator &
                                                relative.month %in% c(1),
                                                yes = NA,
                                                no = value),
                                 value = ifelse(town == 'Hemel Hempstead' &
                                                sub.measure == sub.indicator &
                                                relative.month %in% c(1),
                                                yes = NA,
                                                no = value),
                                 value = ifelse(town == 'Warwick' &
                                                sub.measure == sub.indicator &
                                                relative.month %in% c(1),
                                                yes = NA,
                                                no = value),
                                 value = ifelse(town == 'Yeovil' &
                                                sub.measure == sub.indicator &
                                                relative.month %in% c(1),
                                                yes = NA,
                                                no = value))
                }
            }
            if(sub.indicator == 'admitted'){
                if(balance == TRUE){
                    df <- mutate(df,
                                 value = ifelse(sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
                else{
                    df <- mutate(df,
                                 value = ifelse(town == 'Basingstoke' &
                                                sub.measure == sub.indicator &
                                                relative.month %in% c(1:9, 31:47),
                                                yes = NA,
                                                no = value),
                                 value = ifelse(town == 'Bishop Auckland' &
                                                sub.measure == sub.indicator &
                                                relative.month %in% c(1, 6),
                                                yes = NA,
                                                no = value),
                                 value = ifelse(town == 'Yeovil' &
                                                sub.measure == sub.indicator &
                                                relative.month %in% c(25),
                                                yes = NA,
                                                no = value))
                }
            }
        }
        else if(indicator == 'critical care stays'){
            if(sub.indicator == 'all'){
                if(balance == TRUE){
                    df <- mutate(df,
                                 value = ifelse(sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
                else{
                    df <- mutate(df,
                                 value = ifelse(town == 'Newark' &
                                                sub.measure == sub.indicator &
                                                relative.month %in% c(47),
                                                yes = NA,
                                                no = value))
                }
            }
            if(sub.indicator == 'critical care'){
                if(balance == TRUE){
                    df <- mutate(df,
                                 value = ifelse(sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
                else{
                    df <- mutate(df,
                                 value = ifelse(town == 'Newark' &
                                                sub.measure == sub.indicator &
                                                relative.month %in% c(26),
                                                yes = NA,
                                                no = value))
                }
            }
            if(sub.indicator == 'fraction critical care'){
                if(balance == TRUE){
                    df <- mutate(df,
                                 value = ifelse(sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
                else{
                    df <- mutate(df,
                                 value = ifelse(town == 'Newark' &
                                                sub.measure == sub.indicator &
                                                relative.month %in% c(26),
                                                yes = NA,
                                                no = value))
                }
            }
        }
        else if(indicator == 'length of stay'){
            if(sub.indicator == 'mean'){
                if(balance == TRUE){
                    df <- mutate(df,
                                 value = ifelse(sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
                else{
                    df <- mutate(df,
                                 value = ifelse(town == '' &
                                                sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
            }
            if(sub.indicator == 'median'){
                if(balance == TRUE){
                    df <- mutate(df,
                                 value = ifelse(sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
                else{
                    df <- mutate(df,
                                 value = ifelse(town == '' &
                                                sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
            }
        }
        else if(indicator == 'case fatality ratio'){
            if(sub.indicator == 'any'){
                if(balance == TRUE){
                    df <- mutate(df,
                                 value = ifelse(sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
                else{
                    df <- mutate(df,
                                 value = ifelse(town == '' &
                                                sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
            }
            if(sub.indicator == 'acute heart failure'){
                if(balance == TRUE){
                    df <- mutate(df,
                                 value = ifelse(sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
                else{
                    df <- mutate(df,
                                 value = ifelse(town == '' &
                                                sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
            }
            if(sub.indicator == 'anaphylaxis'){
                if(balance == TRUE){
                    df <- mutate(df,
                                 value = ifelse(sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
                else{
                    df <- mutate(df,
                                 value = ifelse(town == '' &
                                                sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
            }
            if(sub.indicator == 'asphyxiation'){
                if(balance == TRUE){
                    df <- mutate(df,
                                 value = ifelse(sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
                else{
                    df <- mutate(df,
                                 value = ifelse(town == '' &
                                                sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
            }
            if(sub.indicator == 'asthma'){
                if(balance == TRUE){
                    df <- mutate(df,
                                 value = ifelse(sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
                else{
                    df <- mutate(df,
                                 value = ifelse(town == '' &
                                                sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
            }
            if(sub.indicator == 'falls'){
                if(balance == TRUE){
                    df <- mutate(df,
                                 value = ifelse(sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
                else{
                    df <- mutate(df,
                                 value = ifelse(town == '' &
                                                sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
            }
            if(sub.indicator == 'fractured neck of femur'){
                if(balance == TRUE){
                    df <- mutate(df,
                                 value = ifelse(sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
                else{
                    df <- mutate(df,
                                 value = ifelse(town == '' &
                                                sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
            }
            if(sub.indicator == 'meningitis'){
                if(balance == TRUE){
                    df <- mutate(df,
                                 value = ifelse(sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
                else{
                    df <- mutate(df,
                                 value = ifelse(town == '' &
                                                sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
            }
            if(sub.indicator == 'myocardial infarction'){
                if(balance == TRUE){
                    df <- mutate(df,
                                 value = ifelse(sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
                else{
                    df <- mutate(df,
                                 value = ifelse(town == '' &
                                                sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
            }
            if(sub.indicator == 'pregnancy and birth related'){
                if(balance == TRUE){
                    df <- mutate(df,
                                 value = ifelse(sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
                else{
                    df <- mutate(df,
                                 value = ifelse(town == '' &
                                                sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
            }
            if(sub.indicator == 'road traffic accident'){
                if(balance == TRUE){
                    df <- mutate(df,
                                 value = ifelse(sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
                else{
                    df <- mutate(df,
                                 value = ifelse(town == '' &
                                                sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
            }
            if(sub.indicator == 'ruptured aurtoc aneurysm'){
                if(balance == TRUE){
                    df <- mutate(df,
                                 value = ifelse(sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
                else{
                    df <- mutate(df,
                                 value = ifelse(town == '' &
                                                sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
            }
            if(sub.indicator == 'self harm'){
                if(balance == TRUE){
                    df <- mutate(df,
                                 value = ifelse(sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
                else{
                    df <- mutate(df,
                                 value = ifelse(town == '' &
                                                sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
            }
            if(sub.indicator == 'septic shock'){
                if(balance == TRUE){
                    df <- mutate(df,
                                 value = ifelse(sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
                else{
                    df <- mutate(df,
                                 value = ifelse(town == '' &
                                                sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
            }
            if(sub.indicator == 'serious head injury'){
                if(balance == TRUE){
                    df <- mutate(df,
                                 value = ifelse(sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
                else{
                    df <- mutate(df,
                                 value = ifelse(town == '' &
                                                sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
            }
            if(sub.indicator == 'stroke cva'){
                if(balance == TRUE){
                    df <- mutate(df,
                                 value = ifelse(sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
                else{
                    df <- mutate(df,
                                 value = ifelse(town == '' &
                                                sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
            }
        }
        else if(indicator == 'ambulance mean times'){
            if(sub.indicator == 'call_to_dest'){
                if(balance == TRUE){
                    df <- mutate(df,
                                 value = ifelse(sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
                else{
                    df <- mutate(df,
                                 value = ifelse(town == '' &
                                                sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
            }
            if(sub.indicator == 'call_to_scene_any'){
                if(balance == TRUE){
                    df <- mutate(df,
                                 value = ifelse(sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
                else{
                    df <- mutate(df,
                                 value = ifelse(town == '' &
                                                sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
            }
            if(sub.indicator == 'call_to_scene_conveying'){
                if(balance == TRUE){
                    df <- mutate(df,
                                 value = ifelse(sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
                else{
                    df <- mutate(df,
                                 value = ifelse(town == '' &
                                                sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
            }
            if(sub.indicator == 'dest_to_clear'){
                if(balance == TRUE){
                    df <- mutate(df,
                                 value = ifelse(sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
                else{
                    df <- mutate(df,
                                 value = ifelse(town == '' &
                                                sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
            }
            if(sub.indicator == 'scene_to_dest'){
                if(balance == TRUE){
                    df <- mutate(df,
                                 value = ifelse(sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
                else{
                    df <- mutate(df,
                                 value = ifelse(town == '' &
                                                sub.measure == sub.indicator &
                                                relative.month %in% c(),
                                                yes = NA,
                                                no = value))
                }
            }
        }
    }
    ## Return the cleaned data frame
    return(df)
}
