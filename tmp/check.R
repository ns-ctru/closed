## 2016-05-13 - Checking and getting towork model 2.5
t <- closed_models()
df2.5 <- t$df2.5
## Filter by group (which should retain the case ED and its associated controls)
bishop <- filter(df2.5, group == 'Cohort : Bishop Auckland General Hospital')
formula2.5 <- reformulate(response = 'value',
                          termlabels = c('town * closure', 'season', 'relative.month'))
test <- panelAR(formula = formula2.5,
                data    = bishop,
                timeVar = 'relative.month',
                panelVar = 'town',
                autoCorr = 'ar1',
                panelCorrMethod = 'pcse')

## 2016-05-13 - Output ED level data to Stata directory for cross-validation
sites <- c('Bishop Auckland', 'Whitehaven',
           'Hartlepool', 'Grimsby',
           'Hemel Hempstead', 'Warwick',
           'Newark', 'Southport',
           'Rochdale', 'Rotherham')
indicator <- 'ed attendances'
sub.indicator <- 'any'
ed_attendances_by_mode_site_measure %>%
    as.data.frame() %>%
    filter(town %in% sites &
       measure     == indicator,
       sub_measure == sub.indicator) %>%
    write.csv('//tsclient/hamilton/stata/data/ed_attendances_by_mode_site_measure.csv',
              row.names = FALSE)


## 2016-05-12 - Checking plots for model 4 and model 5 which are not currently correct.
t <- closed_models()
df4 <- t$df4
df5 <- t$df5
ggplot(data = df4,
       mapping = aes(x     = relative.month,
                     y     = value,
                     color = town)) +
    geom_line() +
    geom_vline(xintercept = 24, linetype = 4) +
    ## ToDo - Include other steps
    labs(list(title  = 'A test',
              x      = 'Month (Aligned)',
              y      = 'N',
              colour = 'Hospital')) +
    facet_wrap(~ group, ncol = 1) +
    theme_bw()
## Take one cohort
df.test <- filter(t$df4, group == 'Bishop Auckland General Hospital') %>%
           arrange(town, relative.month)
ggplot(data = df.test,
       mapping = aes(x     = relative.month,
                     y     = value,
                     color = town)) +
    geom_line() +
    geom_vline(xintercept = 24, linetype = 4) +
    ## ToDo - Include other steps
    labs(list(title  = 'A test',
              x      = 'Month (Aligned)',
              y      = 'N',
              colour = 'Hospital')) +
    facet_wrap(~ town, ncol = 1) +
    theme_bw() +
    geom_text_repel(data = filter(df.test, relative.month == 5),
                    aes(relative.month,
                        value,
                        color = town,
                        label = town),
                    nudge_x = -2,
                    nudge_y = 600) +
    theme(legend.position = "none")


## 2016-05-11 - Define a formula and model for testing Model 4
##              Works because closed_models() currently returns df4 but won't in the future
t <- closed_models()
formula.model4 <- reformulate(response = 'value',
                              termlabels = c('town * closure', 'season', 'relative.month', 'diff.time.to.ed'))
model4 <- panelAR(data      = t$df4,
                  formula   = formula.model4,
                  timeVar   = 'relative.month',
                  panelVar  = 'lsoa',
                  autoCorr  = 'ar1',
                  panelCorrMethod = 'pcse')
## Supposedly have duplicate observations within each panel subset and inspect
check <- as.data.frame(ed_attendances_by_mode_measure) %>%
         dplyr::filter(sub_measure == 'any') %>%
    dplyr::select(lsoa, relative_month)
duplicated
check[duplicated(check),]
## Check E01010986
as.data.frame(ed_attendances_by_mode_measure) %>%
    dplyr::filter(lsoa == 'E01010986') %>%
    dplyr::select(town, lsoa, relative_month)
## Check E01025367
as.data.frame(ed_attendances_by_mode_measure) %>%
    dplyr::filter(lsoa == 'E01025367') %>%
    dplyr::select(town, lsoa, relative_month)
## Check original data frame using only lsoa and relative_month
ed_attendances_by_mode_measure[duplicated(check),]
ed_attendances_by_mode_measure[!duplicated(check),]

## 2016-05-09 - Testing function
mode.of.arrival.any <- closed_models(df.lsoa         = ed_attendances_by_mode_measure,
                                    df.trust        = ed_attendances_by_mode_site_measure,
                                    df.steps        = steps,
                                    indicator       = 'ed attendances',
                                    sub.indicator   = 'any',
                                    steps           = c('closure'),
                                    fit.with        = 'both',
                                    panel.lsoa      = 'lsoa',
                                    panel.trust     = 'town',
                                    timevar         = 'relative.month',
                                    outcome         = 'value',
                                    model1          = c('closure', 'season', 'relative.month'), ## ToDo - Add other steps when available
                                    model2          = c('town * closure', 'season', 'relative.month'),
                                    model3          = c('town * closure', 'season', 'relative.month', 'diff.time.to.ed'),
                                    model4          = c('town * closure', 'season', 'relative.month', 'diff.time.to.ed'),
                                    model5          = NULL,
                                    autocorr        = 'ar1',
                                    panelcorrmethod = 'pcse',
                                    plot            = TRUE,
                                    common.y        = TRUE,
                                    theme           = theme_bw(),
                                    return.model    = FALSE)

## 2016-05-09 - Save a ED level data set to txt for importing to Stata locally and checking results
sites <- c('Bishop Auckland', 'Whitehaven',
           'Hartlepool', 'Grimsby',
           'Hemel Hempstead', 'Warwick',
           'Newark', 'Southport',
           'Rochdale', 'Rotherham')
indicator <- 'ed attendances'
sub.indicator <- 'any'
filter(ed_attendances_by_mode_measure, town %in% sites &
               measure     == indicator,
               sub_measure == sub.indicator) %>%
    write.csv('data-raw/ed_attendances_by_mode_measure.csv',
              row.names = FALSE)
lsoa <- filter(ed_attendances_by_mode_site_measure, town %in% sites &
               measure     == indicator,
               sub.measure == sub.indicator)

## 2016-05-05 - Checking development of closed_models() as panelAR returns error about
##              the panelCorrMethod being wrong, have tried specifying as an object and
##              explicitly with the same result, strangely though there are coefficients
##              extracted but the resulting panelAR object isn't returned.

## Load the data, convert to data frames and load the closed library.
load('d:/Rpackages/rclosed/data/ed attendances by mode measure - site - 2016-05-03 20.38.Rda')
load('d:/Rpackages/rclosed/data/ed attendances by mode measure - lsoa - 2016-05-03 19.17.Rda')
ed_attendances_by_mode_measure <- as.data.frame(ed_attendances_by_mode_measure)
ed_attendances_by_mode_site_measure <- as.data.frame(ed_attendances_by_mode_site_measure)
library(closed)
## Run the model to check its misbehaving
t <- closed_models()
t$model1.panelar.bishop$model
## Derive a bishop subset (currently being analysed within the function)
bishop <- dplyr::filter(ed_attendances_by_mode_site_measure,
                        town == 'Bishop Auckland' &
                        measure == 'ed attendances' &
                        sub_measure == 'any')
bishop$closure <- ifelse(bishop$relative_month >= 25, 1, 0)
bishop$season <- 1
bishop <- within(bishop,{
                 season[month(yearmonth) == 1  | month(yearmonth) == 2]  <- 1
                 season[month(yearmonth) == 3  | month(yearmonth) == 4]  <- 2
                 season[month(yearmonth) == 5  | month(yearmonth) == 6]  <- 3
                 season[month(yearmonth) == 7  | month(yearmonth) == 8]  <- 4
                 season[month(yearmonth) == 9  | month(yearmonth) == 10] <- 5
                 season[month(yearmonth) == 11 | month(yearmonth) == 12] <- 6
})
## Run panelAR on this
formula <- value ~ closure + season
test <- panelAR(formula = formula,
                data    = bishop,
                timeVar = 'relative_month',
                panelVar = 'town',
                autoCorr = 'ar1',
                panelCorrMethod = 'pcse')
typeof(test)
summary(test)
names(test)
