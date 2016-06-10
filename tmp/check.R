## 2016-06-10 - Checking missing function for site level data
##              Don't think this is actually needed as there are four sites who
##              are missing data for one month, although on reflection this is
##             why its needed, so that other panels in the groups are also missing
t <- data.frame(ed_attendances_by_mode_site_measure) %>%
     ## filter(town == 'Basingstoke' |
    ##        town == 'Hemel Hempstead') %>%
     filter(town %in% c('Basingstoke', 'Hemel Hempstead', 'Warwick', 'Yeovil')) %>%
     closed_missing(test, area = 'ed')

## Check the returned data frames
table(t$orig$town, t$orig$group)
table(t$wide$town, t$wide$group)
table(t$wide.replaced$town, t$wide.replaced$group)
table(t$modified$town, t$modified$group)

original <- t$orig
names(original) <- gsub("_", ".", names(original))
modified <- t$modified
original <- transform(original, is = TRUE)
modified <- transform(modified, is = TRUE)
check <- merge(original,
               modified,
               by       = c('town', 'lsoa', 'measure', 'sub.measure', 'relative.month'),
               all      = TRUE,
               suffixes = c('.orig', '.mod'))
check$is.orig <- !is.na(check$is.orig)
check$is.mod  <- !is.na(check$is.mod)


## 2016-06-08 - Testing conversion to missing when there is one site with missing observations
t <- data.frame(ed_attendances_by_mode_measure) %>%
     filter(town == 'Basingstoke' |
            town == 'Hemel Hempstead') %>%
     closed_missing(test, area = 'lsoa')
## Check the returned data frames
table(t$orig$town, t$orig$group)
table(t$wide$town, t$wide$group)
table(t$wide.replaced$town, t$wide.replaced$group)
table(t$modified$town, t$modified$group)

original <- t$orig
names(original) <- gsub("_", ".", names(original))
modified <- t$modified
original <- transform(original, is = TRUE)
modified <- transform(modified, is = TRUE)
check <- merge(original,
               modified,
               by       = c('town', 'lsoa', 'measure', 'sub.measure', 'relative.month'),
               all      = TRUE,
               suffixes = c('.orig', '.mod'))
check$is.orig <- !is.na(check$is.orig)
check$is.mod  <- !is.na(check$is.mod)

## Now check how many places there are missing values for across all data sets
check_lsoa<- function(df = ed_attendances_by_mode_measure){
    df %>%
    data.frame() %>%
    filter(is.na(value)) %>%
    dplyr::select_('town', 'lsoa', 'relative_month') %>%
    ## order('town', 'relative_month') %>%
    unique()
}
check_site<- function(df = ed_attendances_by_mode_measure){
    df %>%
    data.frame() %>%
    filter(is.na(value)) %>%
    dplyr::select_('town', 'relative_month') %>%
    ## order('town', 'relative_month') %>%
    unique()
}
## ED Attendances by Mode - LSOA
check_lsoa(df = ed_attendances_by_mode_measure)
## ED Attendances by Mode - Site
check_site(df = ed_attendances_by_mode_site_measure)
## Unnecessary Attendances - LSOA
check_lsoa(unnecessary_ed_attendances_measure)
## Unnecessary Attendances - Site
check_site(unnecessary_ed_attendances_site_measure)
## Admitted - LSOA
check_lsoa(ed_attendances_admitted_measure)
## Admitted - Site
check_site(ed_attendances_admitted_site_measure)
## Critical Care - LSOA
check_lsoa(critical_care_cips_measure)
## Critical Care - Site
check_site(critical_care_cips_site_measure)
## Length of Stay - LSOA
check_lsoa(length_of_stay_measure)
## Length of Stay - Site
check_site(length_of_stay_site_measure)

## Index what is missing across the columns of interest...
ind <- which(is.na(t[towns]))
## ...then use this index to replace them as NA
t[ind, towns] <- NA
head(towns)


## 2016-05-23 - Why aren't interaction terms returned by Model 2 and Model 3?
mod1 <- c('closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert')
mod2 <- c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert')
mod3 <- c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert')
mod4 <- c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert')
mod5 <- c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert')
mod6 <- c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert', 'diff.time.to.ed')
mod7 <- c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert', 'diff.time.to.ed')
mode.of.arrival.any <- closed_models(df.lsoa         = ed_attendances_by_mode_measure,
                                     df.trust        = ed_attendances_by_mode_site_measure,
                                     indicator       = 'ed attendances',
                                     sub.indicator   = 'any',
                                     steps           = c('closure'),
                                     fit.with        = 'both',
                                     panel.lsoa      = 'lsoa',
                                     panel.trust     = 'town',
                                     timevar         = 'relative.month',
                                     outcome         = 'value',
                                     model1          = mod1,
                                     model2          = mod2,
                                     model3          = mod3,
                                     model4          = mod4,
                                     model5          = mod5,
                                     model6          = mod6,
				     model7          = NULL,
                                     ## model7          = mod7,
                                     autocorr        = 'ar1',
                                     panelcorrmethod = 'pcse',
                                     plot            = TRUE,
                                     common.y        = TRUE,
                                     theme           = theme_bw(),
                                     return.df       = TRUE)
## Model as called with interaction term between town and closure
mod2 <- c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert')
formula.model2 <- reformulate(response   = 'value',
                              termlabels = mod2)
filter(mode.of.arrival.any$model2.df,
       town == 'Bishop Auckland' |
       town == 'Whitehaven') %>%
panelAR(#data            = to.analyse,
        formula         = formula.model2,
        timeVar         = 'relative.month',
        panelVar        = 'town',
        autoCorr        = 'ar1',
        panelCorrMethod = 'pcse')

## Derive an independent dummy variable representing the interaction
to.analyse <- filter(mode.of.arrival.any$model2.df,
                     town == 'Bishop Auckland' |
                     town == 'Whitehaven')
to.analyse$town.closure <- as.numeric(to.analyse$town) * as.numeric(to.analyse$closure)
## Redefine the model and run this
formula.model2 <- reformulate(response = 'value',
                              termlabels = c('town', 'closure', 'town.closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'))
results <- panelAR(data            = to.analyse,
                   formula         = formula.model2,
                   timeVar         = 'relative.month',
                   panelVar        = 'town',
                   autoCorr        = 'ar1',
                   panelCorrMethod = 'pcse')
results
## Cross-tabulate the closure and town.closure to show they are co-linear
table(to.analyse$closure, to.analyse$town.closure)
## List the data to show that they are co-linear
dplyr::select(to.analyse, relative.month, closure, town.closure)

## Derive an alternative version of the interaction dummy
to.analyse$town.closure.alt <- 0
to.analyse <- within(to.analyse, {
                     town.closure.alt[town == 'Whitehaven' & closure == 0] <- 0
                     town.closure.alt[town == 'Whitehaven' & closure == 1] <- 1
                     town.closure.alt[town == 'Bishop Auckland' & closure == 0] <- 2
                     town.closure.alt[town == 'Bishop Auckland' & closure == 0] <- 3
})



## 2016-05-19 - Reading in Stata results for comparison
stata <- read.csv('~/work/closed/stata/data/stata/results.csv', header = TRUE)
stata <- read.csv('//tsclient/hamilton/stata/data/stata/results.csv', header = TRUE)
stata$out <- paste0(formatC(stata$estimate, digits = 3, format = 'f'),
                    " (",
                    formatC(stata$stderr, digits = 3, format = 'f'),
                    ")",
                    " p = ",
                    formatC(stata$p, digits = 3, format = 'f'))
## Split the models
stata <- dplyr::select(stata, model, town, term, out)
stata.model1 <- filter(stata, model == 1) %>%
                dplyr::select(town, term, out) %>%
                dcast(term ~ town)
names(stata.model1) <- c('Term', 'Bishop Auckland (Stata)', 'Hartlepool (Stata)', 'Hemel Hempstead (Stata)', 'Newark (Stata)', 'Rochdale (Stata)')
stata.model2 <- filter(stata, model == 2) %>%
                dplyr::select(town, term, out) %>%
                dcast(term ~ town)
names(stata.model2) <- c('Term', 'Bishop Auckland (Stata)', 'Hartlepool (Stata)', 'Hemel Hempstead (Stata)', 'Newark (Stata)', 'Rochdale (Stata)')
stata.model3 <- filter(stata, model == 3) %>%
                dplyr::select(town, term, out) %>%
                dcast(term ~ town)
names(stata.model3) <- c('Term', 'Bishop Auckland (Stata)', 'Hartlepool (Stata)', 'Hemel Hempstead (Stata)', 'Newark (Stata)', 'Rochdale (Stata)')
stata.model4 <- filter(stata, model == 4) %>%
                dplyr::select(town, term, out) %>%
                dcast(term ~ town)
stata.model5 <- filter(stata, model == 5) %>%
                dplyr::select(town, term, out) %>%
                dcast(term ~ town)
## Reshape for merging with R models
stata.model1 <- dcast(stata.model1, term ~ town)

## 2016-05-18 - Checking derivation of other dummy variables
months <- read.csv('../stata/data/csv/ed_attendances_by_mode_site_measure.csv',
                   header = TRUE)
months <- mutate(months,
                 nhs111 = ifelse((town == 'Bishop Auckland' & relative_month >= 35) |
                                 (town == 'Southport' & relative_month >= 48) |
                                 (town == 'Rochdale' & relative_month >= 48) |
                                 (town == 'Rotherham' & relative_month >= 48) |
                                 (town == 'Hartlepool' & relative_month >= 45) |
                                 (town == 'Grimsby' & relative_month >= 16),
                                 1, 0),
                 ambulance.divert = ifelse(town == 'Rochdale' & relative_month >= 17, 1, 0),
                 other.centre = ifelse((town == 'Hemel Hempstead' & relative_month >= 20) |
                                       (town == 'Newark' & relative_month >= 3) |
                                       (town == 'Rochdale' & relative_month >= 11) |
                                       (town == 'Hartlepool' & relative_month >= 22),
                                       1, 0))
t <- closed_models(model7 = NULL, return.df = TRUE)
trust.check <- t$model2.df
dplyr::filter(trust.check, town == 'Bishop Auckland') %>% dplyr::select(yearmonth, relative.month, closure, nhs111, other.centre, ambulance.divert)
dplyr::filter(trust.check, town == 'Hemel Hempstead') %>% dplyr::select(yearmonth, relative.month, closure, nhs111, other.centre, ambulance.divert)
dplyr::filter(trust.check, town == 'Newark') %>% dplyr::select(yearmonth, relative.month, closure, nhs111, other.centre, ambulance.divert)
dplyr::filter(trust.check, town == 'Southport') %>% dplyr::select(yearmonth, relative.month, closure, nhs111, other.centre, ambulance.divert)
dplyr::filter(trust.check, town == 'Rochdale') %>% dplyr::select(yearmonth, relative.month, closure, nhs111, other.centre, ambulance.divert)
dplyr::filter(trust.check, town == 'Rotherham') %>% dplyr::select(yearmonth, relative.month, closure, nhs111, other.centre, ambulance.divert)
dplyr::filter(trust.check, town == 'Hartlepool') %>% dplyr::select(yearmonth, relative.month, closure, nhs111, other.centre, ambulance.divert)
dplyr::filter(trust.check, town == 'Grimsby') %>% dplyr::select(yearmonth, relative.month, closure, nhs111, other.centre, ambulance.divert)

## 2016-05-18 - Working out the relative month for the dates noted by Emma (see https://goo.gl/TlhfCF)
load('d:/Rpackages/rclosed/data/ed attendances by mode measure - site - 2016-05-18 15.06.Rda')
load('d:/Rpackages/rclosed/data/ed attendances by mode measure - lsoa - 2016-05-18 15.05.Rda')
months <- read.csv('../stata/data/csv/ed_attendances_by_mode_site_measure.csv',
                   header = TRUE) %>%
          dplyr::select(yearmonth, relative_month, town) %>%
          unique()
## Bishop Auckland
dplyr::filter(months, town == 'Bishop Auckland') %>%
dplyr::filter(yearmonth == '2010-08-01')
## Whitehaven (No additional steps)
## Hemel Hempstead
dplyr::filter(months, town == 'Hemel Hempstead') %>%
dplyr::filter(yearmonth == '2008-10-01')
## Warwick (No additional steps)
## Newark (No additional steps)
## Southport
dplyr::filter(months, town == 'Southport') %>%
dplyr::filter(yearmonth == '2009-06-01' |
              yearmonth == '2013-03-01')
## Rochdale
dplyr::filter(months, town == 'Rochdale') %>%
dplyr::filter(yearmonth == '2010-08-01' |
              yearmonth == '2011-04-01' |
              yearmonth == '2013-03-01')
## Rotherham
dplyr::filter(months, town == 'Rotherham') %>%
dplyr::filter(yearmonth == '2009-06-01' |
              yearmonth == '2013-03-01')
## Hartlepool
dplyr::filter(months, town == 'Hartlepool') %>%
dplyr::filter(yearmonth == '2011-05-01' |
              yearmonth == '2013-04-01')
## Grimsby
dplyr::filter(months, town == 'Grimsby') %>%
dplyr::filter(yearmonth == '2010-11-01')

## 2016-05-17 - Checking re-jigged version of closed_models() works ok
load('d:/Rpackages/rclosed/data/ed attendances by mode measure - site - 2016-05-03 20.38.Rda')
load('d:/Rpackages/rclosed/data/ed attendances by mode measure - lsoa - 2016-05-03 19.17.Rda')
t <- closed_models(model7 = NULL)

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
    filter(measure     == indicator,
           sub_measure == sub.indicator) %>%
    write.csv('//tsclient/hamilton/stata/data/csv/ed_attendances_by_mode_site_measure.csv',
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
