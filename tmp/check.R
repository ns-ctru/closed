## 2017-01-05 - Changing pooled models (Negative Binomial)
mode.of.arrival.any.alt.pooled <- closed_stata_negbin(df.lsoa         = ed_attendances_by_mode_measure_clean,
                                           df.trust        = ed_attendances_by_mode_site_measure_clean,
                                           indicator       = 'ed attendances',
                                           sub.indicator   = 'any',
                                           return.df       = FALSE,
                                           return.model    = TRUE,
                                           return.residuals = FALSE,
                                           rm.unused.control = model.opts$rm.unused.control,
                                           digits           = 3)
dplyr::filter(mode.of.arrival.any.alt.pooled, model == 'Model 4')

## 2017-01-04 - Changing pooled models (Prais-Winston)
model.opts$mod4    <- c('town', 'closure', 'season', 'relative.month')
model.opts$mod7.1 <- c('town', 'season', 'relative.month', 'diff.time.to.ed')
ambulance.mean.times.call.to.dest <- closed_models(df.lsoa         = amb_mean_times_measure_clean,
                                     df.trust         = amb_mean_times_site_measure_clean,
                                     indicator        = 'ambulance mean times',
                                     sub.indicator    = 'call to dest',
                                     panel.lsoa       = model.opts$panel.lsoa,
                                     panel.trust      = model.opts$panel.trust,
                                     timevar          = model.opts$timevar,
                                     outcome          = model.opts$outcome,
                                     model0           = model.opts$mod0,
                                     model0.5         = model.opts$mod0.5,
                                     model1           = model.opts$mod1,
                                     model2           = model.opts$mod2,
                                     model3.1         = model.opts$mod3.1,
                                     model3.2         = model.opts$mod3.2,
                                     model4           = model.opts$mod4,
                                     model5           = model.opts$mod5,
                                     model6.1         = model.opts$mod6.1,
                                     model6.2         = model.opts$mod6.2,
                                     model7.1         = model.opts$mod7.1,
                                     model7.2         = model.opts$mod7.2,
                                     autocorr         = model.opts$autocorr,
                                     panelcorrmethod  = model.opts$panelcorrmethod,
                                     coefficients     = model.opts$coefficients,
                                     seq.times        = model.opts$seq.times,
                                     complete.case    = model.opts$complete.case,
                                     rho.na.rm        = model.opts$rho.na.rm,
                                     theme            = model.opts$theme,
                                     return.df        = model.opts$return.df,
                                     return.model     = model.opts$return.model,
                                     return.residuals = model.opts$return.residuals,
                                     remove.spurious = model.opts$remove.spurious,
                                     join.line        = model.opts$join.line,
                                     rm.unused.control = model.opts$rm.unused.control,
                                     legend           = model.opts$legend)
ambulance.mean.times.call.to.dest$model4.panelar.all.coef

## 2017-01-03 - Checking black lines on geom_smooth()
png(file = '~/work/closed/tmp/check_geom_smooth.png', width = 1024, height = 768)
closed_ts_plot(df            = ed_attendances_by_mode_site_measure,
               indicator     = 'ed attendances',
               sub.indicator = 'any',
               steps         = ts.plot.opts$steps,
               theme         = theme_bw(),
               facet         = ts.plot.opts$facet,
               sites         = c('Bishop Auckland', 'Whitehaven'),
               smooth.plot   = ts.plot.opts$smooth.plot,
               legend        = ts.plot.opts$legend,
               tidy          = ts.plot.opts$tidy,
               colour        = ts.plot.opts$colour,
               lines         = ts.plot.opts$lines,
               hide.control         = ts.plot.opts$hide.control,
               xaxis.steps   = ts.plot.opts$xaxis.steps)
dev.off()

## 2017-01-03 - Checking model 4 from Stata negative binomial
mode.of.arrival.check <- closed_stata_negbin(df.lsoa         = ed_attendances_by_mode_measure_clean,
                                           df.trust        = ed_attendances_by_mode_site_measure_clean,
                                           indicator       = 'ed attendances',
                                           sub.indicator   = 'any',
                                           return.df       = FALSE,
                                           return.model    = TRUE,
                                           return.residuals = FALSE,
                                           rm.unused.control = model.opts$rm.unused.control,
                                           digits           = 3)
table(mode.of.arrival.check$xtnbreg$term, mode.of.arrival.check$xtnbreg$model)
table(mode.of.arrival.any$xtnbreg$term, mode.of.arrival.any$xtnbreg$model)

## 2016-12-22 - Checking exclusion of models with spurious data (cont.)
ambulance.mean.times.call.to.dest <- closed_models(df.lsoa         = amb_mean_times_measure_clean,
                                     df.trust         = amb_mean_times_site_measure_clean,
                                     indicator        = 'ambulance mean times',
                                     sub.indicator    = 'call to dest',
                                     panel.lsoa       = model.opts$panel.lsoa,
                                     panel.trust      = model.opts$panel.trust,
                                     timevar          = model.opts$timevar,
                                     outcome          = model.opts$outcome,
                                     model0           = model.opts$mod0,
                                     model0.5         = model.opts$mod0.5,
                                     model1           = model.opts$mod1,
                                     model2           = model.opts$mod2,
                                     model3.1         = model.opts$mod3.1,
                                     model3.2         = model.opts$mod3.2,
                                     model4           = model.opts$mod4,
                                     model5           = model.opts$mod5,
                                     model6.1         = model.opts$mod6.1,
                                     model6.2         = model.opts$mod6.2,
                                     model7.1         = model.opts$mod7.1,
                                     model7.2         = model.opts$mod7.2,
                                     autocorr         = model.opts$autocorr,
                                     panelcorrmethod  = model.opts$panelcorrmethod,
                                     coefficients     = model.opts$coefficients,
                                     seq.times        = model.opts$seq.times,
                                     complete.case    = model.opts$complete.case,
                                     rho.na.rm        = model.opts$rho.na.rm,
                                     theme            = model.opts$theme,
                                     return.df        = model.opts$return.df,
                                     return.model     = model.opts$return.model,
                                     return.residuals = model.opts$return.residuals,
                                     remove.spurious = model.opts$remove.spurious,
                                     join.line        = model.opts$join.line,
                                     rm.unused.control = model.opts$rm.unused.control,
                                     legend           = model.opts$legend)
t <- dplyr::filter(ambulance.mean.times.call.to.dest$all.model.all.coef, term %in% c('closure', 'diff.time.to.ed'))
table(t$town, t$model)
ambulance.mean.times.call.to.dest$summary.table

## Check another outcome
ambulance.non.conveyances.fraction.not.conveyed <- closed_models(df.lsoa         = amb_green_calls_measure,
                                     df.trust         = amb_green_calls_site_measure_clean,
                                     indicator        = 'ambulance green calls',
                                     sub.indicator    = 'fraction not conveyed',
                                     panel.lsoa       = model.opts$panel.lsoa,
                                     panel.trust      = model.opts$panel.trust,
                                     timevar          = model.opts$timevar,
                                     outcome          = model.opts$outcome,
                                     model0           = model.opts$mod0,
                                     model0.5         = model.opts$mod0.5,
                                     model1           = model.opts$mod1,
                                     model2           = model.opts$mod2,
                                     model3.1         = model.opts$mod3.1,
                                     model3.2         = model.opts$mod3.2,
                                     model4           = model.opts$mod4,
                                     model5           = model.opts$mod5,
                                     model6.1         = model.opts$mod6.1,
                                     model6.2         = model.opts$mod6.2,
                                     model7.1         = model.opts$mod7.1,
                                     model7.2         = model.opts$mod7.2,
                                     autocorr         = model.opts$autocorr,
                                     panelcorrmethod  = model.opts$panelcorrmethod,
                                     coefficients     = model.opts$coefficients,
                                     seq.times        = model.opts$seq.times,
                                     complete.case    = model.opts$complete.case,
                                     rho.na.rm        = model.opts$rho.na.rm,
                                     theme            = model.opts$theme,
                                     return.df        = model.opts$return.df,
                                     return.model     = model.opts$return.model,
                                     return.residuals = model.opts$return.residuals,
                                     remove.spurious = model.opts$remove.spurious,
                                     join.line        = model.opts$join.line,
                                     rm.unused.control = model.opts$rm.unused.control,
                                     legend           = model.opts$legend)
t <- dplyr::filter(ambulance.non.conveyances.fraction.not.conveyed$all.model.all.coef, term %in% c('closure', 'diff.time.to.ed'))
table(t$town, t$model)
ambulance.non.conveyances.fraction.not.conveyed$summary.table


## 2016-12-21 - Checking exclusion of models with spurious data
mode.of.arrival.any <- closed_stata_negbin(df.lsoa         = ed_attendances_by_mode_measure_clean,
                                           df.trust        = ed_attendances_by_mode_site_measure_clean,
                                           indicator       = 'ed attendances',
                                           sub.indicator   = 'any',
                                           return.df       = FALSE,
                                           return.model    = TRUE,
                                           return.residuals = FALSE,
                                           rm.unused.control = model.opts$rm.unused.control,
                                           digits           = 3)
t <- dplyr::filter(mode.of.arrival.any$xtnbreg, term %in% c('closure', 'diff.time.to.ed'))
table(t$town, t$model)
mode.of.arrival.any$summary.table

## 2016-12-20 - Investigating problematic site
dplyr::filter(amb_mean_times_measure_clean, is.na(value) & town == 'Bishop Auckland') %>% dim()
dplyr::filter(amb_mean_times_measure_clean, is.na(value) & town == 'Hartlepool') %>% dim()
dplyr::filter(amb_mean_times_measure_clean, is.na(value) & town == 'Hemel Hempstead') %>% dim()
dplyr::filter(amb_mean_times_measure_clean, is.na(value) & town == 'Newark') %>% dim()
dplyr::filter(amb_mean_times_measure_clean, is.na(value) & town == 'Rochdale') %>% dim()
## Lets try first removing all NA's from the LSOA level data
test <- dplyr::filter(amb_mean_times_measure_clean, !is.na(value))
check <- dplyr::filter(amb_mean_times_measure_clean,
                       sub.measure == 'dest to clear' &
                       town == 'Hemel Hempstead'      &
                       is.na(value))
ambulance.mean.times.dest.to.clear <- closed_models(df.lsoa         = test,
                                     df.trust         = amb_mean_times_site_measure_clean,
                                     indicator        = 'ambulance mean times',
                                     sub.indicator    = 'dest to clear',
                                     panel.lsoa       = model.opts$panel.lsoa,
                                     panel.trust      = model.opts$panel.trust,
                                     timevar          = model.opts$timevar,
                                     outcome          = model.opts$outcome,
                                     model0           = model.opts$mod0,
                                     model0.5         = model.opts$mod0.5,
                                     model1           = model.opts$mod1,
                                     model2           = model.opts$mod2,
                                     model3.1         = model.opts$mod3.1,
                                     model3.2         = model.opts$mod3.2,
                                     model4           = model.opts$mod4,
                                     model5           = model.opts$mod5,
                                     model6.1         = model.opts$mod6.1,
                                     model6.2         = model.opts$mod6.2,
                                     model7.1         = model.opts$mod7.1,
                                     model7.2         = model.opts$mod7.2,
                                     autocorr         = model.opts$autocorr,
                                     panelcorrmethod  = model.opts$panelcorrmethod,
                                     coefficients     = model.opts$coefficients,
                                     seq.times        = model.opts$seq.times,
                                     complete.case    = model.opts$complete.case,
                                     rho.na.rm        = model.opts$rho.na.rm,
                                     theme            = model.opts$theme,
                                     return.df        = model.opts$return.df,
                                     return.model     = model.opts$return.model,
                                     return.residuals = model.opts$return.residuals,
                                     join.line        = model.opts$join.line,
                                     rm.unused.control = model.opts$rm.unused.control,
                                     legend           = model.opts$legend)


## 2016-12-20 - Excluding months from LSOA level data
ed_attendances_by_mode_measure_clean <- closed_clean_revised(df      = ed_attendances_by_mode_measure,
                                                          indicator     = 'ed attendances',
                                                          systematic    = systematic.outlier)
ed_attendances_by_mode_site_measure_clean <- closed_clean_revised(df      = ed_attendances_by_mode_site_measure,
                                                          indicator     = 'ed attendances',
                                                          systematic    = systematic.outlier)
## Analyse using uncleaned data
unclean <- mode.of.arrival.any <- closed_stata_negbin(df.lsoa         = ed_attendances_by_mode_measure,
                                           df.trust        = ed_attendances_by_mode_site_measure,
                                           indicator       = 'ed attendances',
                                           sub.indicator   = 'any',
                                           return.df       = FALSE,
                                           return.model    = TRUE,
                                           return.residuals = FALSE,
                                           rm.unused.control = model.opts$rm.unused.control,
                                           digits           = 3)
clean <- mode.of.arrival.any <- closed_stata_negbin(df.lsoa         = ed_attendances_by_mode_measure_clean,
                                           df.trust        = ed_attendances_by_mode_site_measure_clean,
                                           indicator       = 'ed attendances',
                                           sub.indicator   = 'any',
                                           return.df       = FALSE,
                                           return.model    = TRUE,
                                           return.residuals = FALSE,
                                           rm.unused.control = model.opts$rm.unused.control,
                                           digits           = 3)
## Combine the two to check
check <- merge(unclean$xtnbreg,
               clean$xtnbreg,
               by       = c('measure', 'sub.measure', 'town', 'model', 'term'),
               suffixes = c('.unclean', '.clean'))
## Look at some subsets
dplyr::filter(check, term %in% c('closure', 'diff.time.to.ed')) %>%
    dplyr::select(model, town, term, est.unclean, est.clean, standard.clean, standard.unclean) %>%
        dplyr::arrange(model, town)
## Beyond the systematic removal of data points two sites had data points cleaned...
##
## Bishop Auckland/Whitehaven (due to large erroneous data points in Bishop Auckland)
## Newark/Southport (due to slightly low data points in Southport)
##
## ...unsurprisingly the biggest difference between clean and unclean analyses is observed
## in the Bishop Auckland/Whitehaven analyses at both Site and LSOA level, thus LSOA does
## need cleaning.


## 2016-12-13 Generic example of dual axis for asking on Stackoverflow how to angle top
png('~/work/closed/tmp/dual_axis_eg.png', width = 768, height = 768)
ggplot(mtcars, aes(x = wt, y = mpg, colour = mpg)) +
    geom_point() +
    scale_x_continuous(name = 'Bottom Axis',
                       sec.axis = sec_axis(trans = ~ .,
                                           name  = 'Top Axis',
                                           breaks = c(2:5),
                                           labels = c('Two Two', 'Three Three Three', 'Four Four Four Four', 'Five Five Five Five Five'))) +
    theme(axis.text.x.top = element_text(angle = 90))
dev.off()
png(filename = '~/work/closed/tmp/sec_deaths_all_7days.png', width = 1024, heigh = 768)
example <- closed_ts_plot(df            = sec_deaths_all_7days_site_measure,
               indicator     = 'sec deaths all 7days',
               sub.indicator = 'any sec',
               steps         = ts.plot.opts$steps,
               theme         = theme_bw(),
               facet         = ts.plot.opts$facet,
               sites         = c('Rochdale', 'Rotherham'),
               smooth.plot   = TRUE,
               legend        = ts.plot.opts$legend,
               tidy          = FALSE,
               colour        = ts.plot.opts$colour,
               lines         = FALSE,
               hide.control  = TRUE,
               xaxis.steps   = ts.plot.opts$xaxis.steps)
example
dev.off()
save(example, file = '~/work/closed/tmp/example.Rdata')

## 2016-12-12 Peaks/spikes for Bishop Auckland/Whitehaven  Length of Stay (Mean)
los <- dplyr::filter(length_of_stay_site_measure,
                     town %in% c('Bishop Auckland', 'Whitehaven') &
                     sub.measure == 'mean') %>%
       dplyr::select(relative.month, yearmonth, town, value) %>%
       group_by(town) %>%
       mutate(mean          = mean(value, na.rm = TRUE),
              sd            = sd(value, na.rm = TRUE),
              lower         = mean - sd,
              upper         = mean + sd,
              peak_trough   = ifelse(value >= mean, 'Peak', 'Trough'),
              month         = month(yearmonth),
              days_in_month = days_in_month(yearmonth))
table(los$month, los$peak_trough)
table(los$days_in_month, los$peak_trough)
write.table(los,
            file      = '~/work/closed/tmp/length_of_stay.csv',
            sep       = ',',
            col.names = TRUE)
dplyr::filter(length_of_stay_site_measure,
              town == 'Bishop Auckland' &
              sub.measure == 'mean',
              relative.month %in% c(16, 17, 37, 43)) %>%
       dplyr::select(relative.month, yearmonth, town, value) %>%
       group_by(town) %>%
       mutate(month         = month(yearmonth),
              days_in_month = days_in_month(yearmonth))

## 2016-12-12 Sample plots for meeting
png(filename = '~/work/closed/tmp/case_fatality_all.png', width = 1024, heigh = 768)
closed_ts_plot(df            = sec_case_fatality_7days_site_measure,
               indicator     = 'sec case fatality 7 days',
               sub.indicator = 'any',
               steps         = ts.plot.opts$steps,
               theme         = theme_bw(),
               facet         = ts.plot.opts$facet,
               sites         = c('Bishop Auckland', 'Whitehaven'),
               smooth.plot   = TRUE,
               legend        = ts.plot.opts$legend,
               tidy          = FALSE,
               colour        = ts.plot.opts$colour,
               lines         = FALSE,
               xaxis.steps   = ts.plot.opts$xaxis.steps)
dev.off()
png(filename = '~/work/closed/tmp/sec_deaths_all_7days.png', width = 1024, heigh = 768)
closed_ts_plot(df            = sec_deaths_all_7days_site_measure,
               indicator     = 'sec deaths all 7days',
               sub.indicator = 'any sec',
               steps         = ts.plot.opts$steps,
               theme         = theme_bw(),
               facet         = ts.plot.opts$facet,
               sites         = c('Rochdale', 'Rotherham'),
               smooth.plot   = TRUE,
               legend        = ts.plot.opts$legend,
               tidy          = FALSE,
               colour        = ts.plot.opts$colour,
               lines         = FALSE,
               xaxis.steps   = ts.plot.opts$xaxis.steps)
dev.off()
png(filename = '~/work/closed/tmp/sec_deaths_all_in_cips_7days.png', width = 1024, heigh = 768)
closed_ts_plot(df            = sec_deaths_in_cips_7days_site_measure_clean,
               indicator     = 'sec deaths in cips 7days',
               sub.indicator = 'any sec',
               steps         = ts.plot.opts$steps,
               theme         = theme_bw(),
               facet         = ts.plot.opts$facet,
               sites         = c('Newark', 'Southport'),
               smooth.plot   = TRUE,
               legend        = ts.plot.opts$legend,
               tidy          = FALSE,
               colour        = ts.plot.opts$colour,
               lines         = FALSE,
               xaxis.steps   = ts.plot.opts$xaxis.steps)
dev.off()


## 2016-12-08 Developing possibility of geom_point() and geom_smooth() in the closed_ts_plot() function
load('~/work/closed/tmp/ed_attendances_clean.RData')
ts.plot.opts <- list()
ts.plot.opts$steps       <- TRUE
ts.plot.opts$facet       <- FALSE
ts.plot.opts$legend      <- TRUE
ts.plot.opts$tidy        <- TRUE
ts.plot.opts$colour      <- FALSE
ts.plot.opts$lines       <- FALSE
ts.plot.opts$xaxis.steps <- TRUE
png(filename = 'mode_of_arrival_any_bishop_smooth.png', width = 1024, height = 768)
closed_ts_plot(df            = ed_attendances_by_mode_site_measure_clean,
               indicator     = 'ed attendances',
               sub.indicator = 'any',
               steps         = ts.plot.opts$steps,
               theme         = theme_bw(),
               facet         = ts.plot.opts$facet,
               sites         = c('Bishop Auckland', 'Whitehaven'),
               legend        = ts.plot.opts$legend,
               tidy          = ts.plot.opts$tidy,
               colour        = ts.plot.opts$colour,
               lines         = ts.plot.opts$lines,
               xaxis.steps   = ts.plot.opts$xaxis.steps,
               smooth.plot   = TRUE)
dev.off()
png(filename = 'mode_of_arrival_any_bishop.png', width = 1024, height = 768)
closed_ts_plot(df            = ed_attendances_by_mode_site_measure_clean,
               indicator     = 'ed attendances',
               sub.indicator = 'any',
               steps         = ts.plot.opts$steps,
               theme         = theme_bw(),
               facet         = ts.plot.opts$facet,
               sites         = c('Bishop Auckland', 'Whitehaven'),
               legend        = ts.plot.opts$legend,
               tidy          = ts.plot.opts$tidy,
               colour        = ts.plot.opts$colour,
               lines         = ts.plot.opts$lines,
               xaxis.steps   = ts.plot.opts$xaxis.steps,
               smooth.plot   = FALSE)
dev.off()
## Normal plot
save(check.ts, file = '~/work/closed/tmp/check.ts')

## 2016-12-01 Checking removal of spurious data points
## 2016-12-01 Checking another outcome
mode.of.arrival.any <- closed_stata_negbin(df.lsoa         = ed_attendances_by_mode_measure,
                                           df.trust        = ed_attendances_by_mode_site_measure_clean,
                                           indicator       = 'ed attendances',
                                           sub.indicator   = 'any',
                                           return.df       = FALSE,
                                           return.model    = TRUE,
                                           return.residuals = FALSE,
                                           digits           = 3)
mode.of.arrival.any$summary.table

## 2016-11-30 Post meeting why the hell are means not correct???
## Deaths (closed_stata_negbin())
deaths.dirty <- dplyr::filter(sec_deaths_all_7days_site_measure, sub.measure == 'any sec') %>%
                mutate(before.after = ifelse(relative.month >= 25, 'After', 'Before')) %>%
                group_by(town, before.after) %>%
                summarise(mean = mean(value, na.rm = TRUE)) %>%
                as.data.frame()
deaths.clearn <- dplyr::filter(sec_deaths_all_7days_site_measure_clean, sub.measure == 'any sec') %>%
                 mutate(before.after = ifelse(relative.month >= 25, 'After', 'Before')) %>%
                 group_by(town, before.after) %>%
                 summarise(mean = mean(value, na.rm = TRUE)) %>%
                 as.data.frame()
sec.deaths.all.7days.any.sec <- closed_stata_negbin(df.lsoa =  sec_deaths_all_7days_measure,
                                           df.trust         = sec_deaths_all_7days_site_measure_clean,
                                           indicator       = 'sec deaths all 7days',
                                           sub.indicator   = 'any sec',
                                           return.df       = FALSE,
                                           return.model    = TRUE,
                                           return.residuals = FALSE,
                                           digits           = 3)
sec.deaths.all.7days.any.sec$summary.table
## Check the data frame...
group_by(sec.deaths.all.7days.any.sec$tmp, town, before.after) %>%
    summarise(mean = mean(value, na.rm = TRUE)) %>%
    as.data.frame()
## Look at the data
t <- dplyr::filter(sec.deaths.all.7days.any.sec$tmp, town == 'Bishop Auckland' & sub.measure == 'any sec') %>%
     dplyr::select(before.after, value) %>%
     ungroup() %>%
     as.data.frame()
summary(t)
mean(t$value, na.rm = TRUE)
group_by(t, before.after) %>% summarise(mean = mean(value, na.rm = TRUE))


## Case Fatality (closed_models())
fatality.dirty <- filter(case_fatality_site_measure, sub.measure == 'any sec') %>%
                  mutate(before.after = ifelse(relative.month >= 25, 'After', 'Before')) %>%
                  group_by(to.check.fatality, town, before.after) %>%
                  summarise(mean = mean(value, na.rm = TRUE)) %>%
                  as.data.frame()
fatality.clean <- filter(case_fatality_site_measure_clean, sub.measure == 'any sec') %>%
                  mutate(before.after = ifelse(relative.month >= 25, 'After', 'Before')) %>%
                  group_by(to.check.fatality, town, before.after) %>%
                  summarise(mean = mean(value, na.rm = TRUE)) %>%
                  as.data.frame()
case.fatality.ratio.7days.any.sec <- closed_models(df.lsoa         = sec_case_fatality_7days_measure,
                                     df.trust         = sec_case_fatality_7days_site_measure,
                                     indicator        = 'sec case fatality 7 days',
                                     sub.indicator    = 'any sec',
                                     panel.lsoa       = model.opts$panel.lsoa,
                                     panel.trust      = model.opts$panel.trust,
                                     timevar          = model.opts$timevar,
                                     outcome          = model.opts$outcome,
                                     model0           = model.opts$mod0,
                                     model0.5         = model.opts$mod0.5,
                                     model1           = model.opts$mod1,
                                     model2           = model.opts$mod2,
                                     model3.1         = model.opts$mod3.1,
                                     model3.2         = model.opts$mod3.2,
                                     model4           = model.opts$mod4,
                                     model5           = model.opts$mod5,
                                     model6.1         = model.opts$mod6.1,
                                     model6.2         = model.opts$mod6.2,
                                     model7.1         = model.opts$mod7.1,
                                     model7.2         = model.opts$mod7.2,
                                     autocorr         = model.opts$autocorr,
                                     panelcorrmethod  = model.opts$panelcorrmethod,
                                     coefficients     = model.opts$coefficients,
                                     seq.times        = model.opts$seq.times,
                                     complete.case    = model.opts$complete.case,
                                     rho.na.rm        = TRUE,
                                     theme            = model.opts$theme,
                                     return.df        = model.opts$return.df,
                                     return.model     = model.opts$return.model,
                                     return.residuals = model.opts$return.residuals,
                                     join.line        = model.opts$join.line,
                                     legend           = model.opts$legend)
case.fatality.ratio.7days.any.sec$summary.table

## 2016-11-30 Back to why Model 3 isn't running for all centres
check.call.to.dest <- closed_models(df.lsoa         = amb_mean_times_measure,
                                     df.trust         = amb_mean_times_site_measure_clean,
                                     indicator        = 'ambulance mean times',
                                     sub.indicator    = 'call to dest',
                                     panel.lsoa       = model.opts$panel.lsoa,
                                     panel.trust      = model.opts$panel.trust,
                                     timevar          = model.opts$timevar,
                                     outcome          = model.opts$outcome,
                                     model0           = model.opts$mod0,
                                     model0.5         = model.opts$mod0.5,
                                     model1           = model.opts$mod1,
                                     model2           = model.opts$mod2,
                                     model3.1         = model.opts$mod3.1,
                                     model3.2         = model.opts$mod3.2,
                                     model4           = model.opts$mod4,
                                     model5           = model.opts$mod5,
                                     model6.1         = model.opts$mod6.1,
                                     model6.2         = model.opts$mod6.2,
                                     model7.1         = model.opts$mod7.1,
                                     model7.2         = model.opts$mod7.2,
                                     autocorr         = model.opts$autocorr,
                                     panelcorrmethod  = model.opts$panelcorrmethod,
                                     coefficients     = model.opts$coefficients,
                                     seq.times        = model.opts$seq.times,
                                     complete.case    = model.opts$complete.case,
                                     rho.na.rm        = model.opts$rho.na.rm,
                                     theme            = model.opts$theme,
                                     return.df        = model.opts$return.df,
                                     return.model     = model.opts$return.model,
                                     return.residuals = model.opts$return.residuals,
                                     join.line        = model.opts$join.line,
                                     legend           = model.opts$legend)
names(check.call.to.dest)

## 2016-11-30 Spurious data points in ambulance mean times call to dest?
amb_mean_times_site_measure_clean %>%
    dplyr::filter(town %in% c('Hemel Hempstead'), sub.measure == 'call to dest') %>%
    dplyr::select(yearmonth, relative.month, value, season, nhs111, closure, ambulance.divert, other.centre) %>%
    as.data.frame()
## Check the data as read in (in case I've done something dumb)
load('~/work/closed/lib/data/ambulance mean times measure - site - 2016-11-21 20.31.Rda')
amb_mean_times_site_measure %>%
    dplyr::filter(town %in% c('Hemel Hempstead') & sub.measure %in% c('call_to_dest', 'call_to_scene_any', 'call_to_scene_conveying')) %>%
    arrange(sub_measure, yearmonth) %>%
    dplyr::select(yearmonth, sub_measure, value) %>%
    as.data.frame()



## 2016-11-30 Why aren't ambulance mean times working?
amb_mean_times_site_measure_clean %>%
    dplyr::filter(town %in% c('Bishop Auckland', 'Whitehaven'), sub.measure == 'call to dest') %>%
    dplyr::select(yearmonth, relative.month, value, season, nhs111, closure, ambulance.divert, other.centre) %>%
    as.data.frame()
check.ts <- closed_ts_plot(df            = amb_mean_times_site_measure_clean,
                           indicator     = 'ambulance mean times',
                           sub.indicator = 'call to dest',
                           steps         = ts.plot.opts$steps,
                           theme         = theme_bw(),
                           facet         = ts.plot.opts$facet,
                           sites         = c('Bishop Auckland', 'Whitehaven'),
                           legend        = ts.plot.opts$legend,
                           tidy          = ts.plot.opts$tidy,
                           colour        = ts.plot.opts$colour,
                           lines         = ts.plot.opts$lines,
                           xaxis.steps   = ts.plot.opts$xaxis.steps)
check.call.to.dest <- closed_models(df.lsoa         = amb_mean_times_measure,
                                     df.trust         = amb_mean_times_site_measure_clean,
                                     indicator        = 'ambulance mean times',
                                     sub.indicator    = 'call to dest',
                                     panel.lsoa       = model.opts$panel.lsoa,
                                     panel.trust      = model.opts$panel.trust,
                                     timevar          = model.opts$timevar,
                                     outcome          = model.opts$outcome,
                                     model0           = model.opts$mod0,
                                     model0.5         = model.opts$mod0.5,
                                     model1           = model.opts$mod1,
                                     model2           = model.opts$mod2,
                                     model3.1         = model.opts$mod3.1,
                                     model3.2         = model.opts$mod3.2,
                                     model4           = model.opts$mod4,
                                     model5           = model.opts$mod5,
                                     model6.1         = model.opts$mod6.1,
                                     model6.2         = model.opts$mod6.2,
                                     model7.1         = model.opts$mod7.1,
                                     model7.2         = model.opts$mod7.2,
                                     autocorr         = model.opts$autocorr,
                                     panelcorrmethod  = model.opts$panelcorrmethod,
                                     coefficients     = model.opts$coefficients,
                                     seq.times        = model.opts$seq.times,
                                     complete.case    = model.opts$complete.case,
                                     rho.na.rm        = model.opts$rho.na.rm,
                                     theme            = model.opts$theme,
                                     return.df        = model.opts$return.df,
                                     return.model     = model.opts$return.model,
                                     return.residuals = model.opts$return.residuals,
                                     join.line        = model.opts$join.line,
                                     legend           = model.opts$legend)
save(check.ts, check.call.to.dest, file = '~/work/closed/tmp/check.call.to.dest.RData')


## 2016-11-28 Checking include t and z in model summary
critical.care.cips.critical <- closed_stata_negbin(df.lsoa =  critical_care_cips_measure,
                                           df.trust         = critical_care_cips_site_measure_clean,
                                           indicator       = 'critical care stays',
                                           sub.indicator   = 'critical care',
                                           return.df       = FALSE,
                                           return.model    = TRUE,
                                           return.residuals = FALSE,
                                           digits           = 3)
critical.care.cips.fraction <- closed_models(df.lsoa         = critical_care_cips_measure,
                                     df.trust         = critical_care_cips_site_measure_clean,
                                     indicator        = 'critical care stays',
                                     sub.indicator    = 'fraction critical care',
                                     panel.lsoa       = model.opts$panel.lsoa,
                                     panel.trust      = model.opts$panel.trust,
                                     timevar          = model.opts$timevar,
                                     outcome          = model.opts$outcome,
                                     model0           = model.opts$mod0,
                                     model0.5         = model.opts$mod0.5,
                                     model1           = model.opts$mod1,
                                     model2           = model.opts$mod2,
                                     model3.1         = model.opts$mod3.1,
                                     model3.2         = model.opts$mod3.2,
                                     model4           = model.opts$mod4,
                                     model5           = model.opts$mod5,
                                     model6.1         = model.opts$mod6.1,
                                     model6.2         = model.opts$mod6.2,
                                     model7.1         = model.opts$mod7.1,
                                     model7.2         = model.opts$mod7.2,
                                     autocorr         = model.opts$autocorr,
                                     panelcorrmethod  = model.opts$panelcorrmethod,
                                     coefficients     = model.opts$coefficients,
                                     seq.times        = model.opts$seq.times,
                                     complete.case    = model.opts$complete.case,
                                     rho.na.rm        = model.opts$rho.na.rm,
                                     theme            = model.opts$theme,
                                     return.df        = model.opts$return.df,
                                     return.model     = model.opts$return.model,
                                     return.residuals = model.opts$return.residuals,
                                     join.line        = model.opts$join.line,
                                     legend           = model.opts$legend)
summary.models <- rbind(critical.care.cips.critical$xtnbreg,
                        critical.care.cips.fraction$all.model.all.coefs)
heatmap.standard <- closed_heatmap(df           = summary.models,
                                  site         = c('Bishop Auckland', 'Hartlepool', 'Hemel Hempstead', 'Newark', 'Rochdale'),
                                  colour.by    = 'standard',
                                  coef         = c('closure', 'diff.time.to.ed'),
                                  include.text = c('standard'),
                                  colour       = 'green',
                                  final        = TRUE,
                                  digits       = 3)


## 2016-11-16 Working out missing heatmaps
load(file = '~/work/closed/hta_report/data/results.RData')
## Combine all coefficients from all models into one dataset
summary.models <- rbind(mode.of.arrival.any$xtnbreg,
                        mode.of.arrival.other$xtnbreg,
                        mode.of.arrival.ambulance$xtnbreg,
                        unnecessary.attendance$xtnbreg,
                        all.emergency.admissions.all$xtnbreg,
                        avoidable.emergency.admissions.any$xtnbreg,
                        avoidable.emergency.admissions.chest.pain$xtnbreg,
                        ed.attendances.admitted.all$xtnbreg,
                        ed.attendances.admitted.fraction.admitted$all.model.all.coef,
                        ed.attendances.admitted.admitted$xtnbreg,
                        critical.care.cips.all$xtnbreg,
                        critical.care.cips.critical$xtnbreg,
                        critical.care.cips.fraction$all.model.all.coef,
                        length.of.stay.mean$all.model.all.coef,
                        length.of.stay.median$all.model.all.coef,
                        case.fatality.ratio.any$all.model.all.coef,
                        case.fatality.ratio.acute.heart.failure$all.model.all.coef,
                        case.fatality.ratio.stroke.cva$all.model.all.coef,
                        ambulance.mean.times.call.to.dest$all.model.all.coef,
                        ambulance.mean.times.call.to.scene.any$all.model.all.coef,
                        ambulance.mean.times.call.to.scene.conveying$all.model.all.coef,
                        ambulance.mean.times.dest.to.clear$all.model.all.coef,
                        ambulance.mean.times.scene.to.dest$all.model.all.coef,
                        ambulance.non.conveyances.green.calls$xtnbreg,
                        ambulance.non.conveyances.green.calls.non.conveyed$xtnbreg,
                        ambulance.non.conveyances.fraction.not.conveyed$all.model.all.coef,
                        ambulance.red.calls.hospital.transfers$xtnbreg,
                        ambulance.red.calls.total$xtnbreg,
                        hospital.transfers.all.stays$xtnbreg,
                        hospital.transfers.stays.with.transfer$xtnbreg,
                        hospital.transfers.proportion.with.transfer$all.model.all.coef)
summary.models <- mutate(summary.models,
                 indicator = paste0(measure, ' - ', sub.measure),
                 indicator = gsub('_', ' ', indicator),
                 indicator = factor(indicator,
                                    levels = c('hospital transfers - fraction with transfer',
                                               'hospital transfers - stays with transfer',
                                               'hospital transfers - all stays',
                                               'case fatality ratio - stroke cva',
                                               'case fatality ratio - acute heart failure',
                                               'case fatality ratio - any',
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

## 2016-11-15 Ordering tables
load(file = '~/work/closed/tmp/closed.heatmap.RData')
summary.models <- mutate(summary.models,
                         indicator = paste0(measure, ' - ', sub.measure))
## Convert to factor
summary.models <-  mutate(summary.models,
                          indicator = factor(indicator,
                                             levels = c('ambulance green calls - green calls',
                                                        'ambulance green calls - not conveyed green calls',
                                                        'ambulance green calls - hospital transfers',
                                                        'ambulance green calls - fraction not conveyed',
                                                        'ambulance red calls - hospital transfers',
                                                        'ambulance red calls - total',
                                                        'ambulance mean times - call to dest',
                                                        'ambulance mean times - call to scene any',
                                                        'ambulance mean times - call to scene conveying',
                                                        'ambulance mean times - dest to clear',
                                                        'ambulance mean times - scene to dest',
                                                        'ed attendances - any',
                                                        'ed attendances - ambulance',
                                                        'ed attendances - other',
                                                        'unnecessary ed attendances - all',
                                                        'ed attendances admitted - all',
                                                        'ed attendances admitted - admitted',
                                                        'ed attendances admitted - fraction admitted',
                                                        'all emergency admissions - all',
                                                        'avoidable emergency admissions - any',
                                                        'avoidable emergency admissions - non-specific chest pain',
                                                        'critical care stays - all',
                                                        'critical care stays - critical care',
                                                        'critical care stays - fraction critical care',
                                                        'length of stay - mean',
                                                        'length of stay median',
                                                        'case fatality ratio - any',
                                                        'case fatality ratio - acure heart failure',
                                                        'case fatality ratio - stroke cva')))

## Now recode
summary.models <- mutate(summary.models,
                         indicator = recode_factor())

## 2016-11-14 Working on Heatmaps again, ggplot acll constructed 2016-11-10 works but
##            something in the preping of data within closed_heatmap() doesn't leave
##            unique data as overlaid text appears to have multiple layers.
load(file = '~/work/closed/tmp/closed.heatmap.RData')

## Working on the ggplot2 call...
dplyr::filter(summary.models, town == 'Bishop Auckland' & term == 'closure') %>%
    mutate(indicator = paste0(measure, ' ', sub.measure),
           overlay   = paste0(formatC(est, digits = 2, format = 'f'),
                              '\nSE =',
                              formatC(stderr, digits = 2, format = 'f'),
                              '\np = ',
                              formatC(p, digits = 3, format = 'f'))) %>%
    mutate(model = ifelse(model == 'Model 4',
                          yes = 'Model 3',
                          no  = model),
           model = ifelse(model == 'Model 6.1',
                          yes = 'Model 4',
                          no  = model),
           model = ifelse(model == 'Model 7.1',
                          yes = 'Model 5',
                          no  = model)) %>%
            ggplot(aes(x = as.factor(model),
                       y = as.factor(indicator))) +
            geom_tile(aes(fill = p)) +
            geom_text(aes(fill = p, label = overlay)) +
            scale_fill_gradient(high = 'white', low = 'red') +
            theme_bw() +
            ggtitle('Bishop Auckland') + xlab('Model') + ylab('Indicator') +
            scale_x_discrete(position = 'top')

## Call to closed_heatmap()
closed_heatmap(df           = summary.models,
               site         = c('Bishop Auckland', 'Hartlepool', 'Hemel Hempstead', 'Newark', 'Rochdale'),
               colour.by    = 'coefficient',
               coef         = c('closure', 'diff.time.to.ed'),
               include.text = c('coefficient', 'se', 'p'),
               colour       = 'red',
               final        = TRUE,
               digits       = 2)

## 2016-11-10 Heatmap/table of results
mode.of.arrival.any <- closed_stata_negbin(df.lsoa         = ed_attendances_by_mode_measure,
                                           df.trust        = ed_attendances_by_mode_site_measure_clean,
                                           indicator       = 'ed attendances',
                                           sub.indicator   = 'any',
                                           return.df       = FALSE,
                                           return.model    = TRUE,
                                           return.residuals = FALSE,
                                           digits           = 3)
mode.of.arrival.other <- closed_stata_negbin(df.lsoa         = ed_attendances_by_mode_measure,
                                           df.trust        = ed_attendances_by_mode_site_measure_clean,
                                           indicator       = 'ed attendances',
                                           sub.indicator   = 'other',
                                           return.df       = FALSE,
                                           return.model    = TRUE,
                                           return.residuals = FALSE,
                                           digits           = 3)
mode.of.arrival.ambulance <- closed_stata_negbin(df.lsoa         = ed_attendances_by_mode_measure,
                                           df.trust        = ed_attendances_by_mode_site_measure_clean,
                                           indicator       = 'ed attendances',
                                           sub.indicator   = 'ambulance',
                                           return.df       = FALSE,
                                           return.model    = TRUE,
                                           return.residuals = FALSE,
                                           digits           = 3)
ed.attendances.admitted.fraction.admitted <- closed_models(df.lsoa         = ed_attendances_admitted_measure,
                                     df.trust         = ed_attendances_admitted_site_measure_clean,
                                     indicator        = 'ed attendances admitted',
                                     sub.indicator    = 'fraction admitted',
                                     panel.lsoa       = model.opts$panel.lsoa,
                                     panel.trust      = model.opts$panel.trust,
                                     timevar          = model.opts$timevar,
                                     outcome          = model.opts$outcome,
                                     model0           = model.opts$mod0,
                                     model0.5         = model.opts$mod0.5,
                                     model1           = model.opts$mod1,
                                     model2           = model.opts$mod2,
                                     model3.1         = model.opts$mod3.1,
                                     model3.2         = model.opts$mod3.2,
                                     model4           = model.opts$mod4,
                                     model5           = model.opts$mod5,
                                     model6.1         = model.opts$mod6.1,
                                     model6.2         = model.opts$mod6.2,
                                     model7.1         = model.opts$mod7.1,
                                     model7.2         = model.opts$mod7.2,
                                     autocorr         = model.opts$autocorr,
                                     panelcorrmethod  = model.opts$panelcorrmethod,
                                     coefficients     = model.opts$coefficients,
                                     seq.times        = model.opts$seq.times,
                                     complete.case    = model.opts$complete.case,
                                     rho.na.rm        = model.opts$rho.na.rm,
                                     theme            = model.opts$theme,
                                     return.df        = model.opts$return.df,
                                     return.model     = model.opts$return.model,
                                     return.residuals = model.opts$return.residuals,
                                     join.line        = model.opts$join.line,
                                     legend           = model.opts$legend)
summary.models <- rbind(mode.of.arrival.any$xtnbreg,
                        mode.of.arrival.other$xtnbreg,
                        mode.of.arrival.ambulance$xtnbreg,
                        ed.attendances.admitted.fraction.admitted$all.model.all.coef) %>%
                  mutate(indicator = paste0(measure, ' ', sub.measure))
summary.models <- mutate(summary.models, indicator = paste0(measure, ' ', sub.measure))
closed.heatmap <- closed_heatmap(df           = summary.models,
                                 site         = c('Bishop Auckland', 'Hartlepool', 'Hemel Hempstead', 'Newark', 'Rochdale'),
                                 coef         = c('closure', 'diff_time_to_ed'),
                                 include.text = c('coefficient', 'se', 'p'),
                                 color        = 'red',
                                 final        = TRUE,
                                 digits       = 2)
## Working on the ggplot2 call...
dplyr::filter(summary.models, town == 'Bishop Auckland' & term == 'closure') %>%
    mutate(indicator = paste0(measure, ' ', sub.measure),
           overlay   = paste0(formatC(est, digits = 2, format = 'f'),
                              '\nSE =',
                              formatC(stderr, digits = 2, format = 'f'),
                              '\np = ',
                              formatC(p, digits = 3, format = 'f'))) %>%
    mutate(model = ifelse(model == 'Model 4',
                          yes = 'Model 3',
                          no  = model),
           model = ifelse(model == 'Model 6.1',
                          yes = 'Model 4',
                          no  = model),
           model = ifelse(model == 'Model 7.1',
                          yes = 'Model 5',
                          no  = model)) %>%
            ggplot(aes(x = as.factor(model),
                       y = as.factor(indicator))) +
            geom_tile(aes(fill = p)) +
            geom_text(aes(fill = p, label = overlay)) +
            scale_fill_gradient(high = 'white', low = 'red') +
            theme_bw() +
            ggtitle('Bishop Auckland') + xlab('Model') + ylab('Indicator') ## +
    ## scale_x_discrete(position = 'top')
save(summary.models, closed.heatmap, file = '~/work/closed/tmp/closed.heatmap.RData')

## 2016-11-10 Aligning output from closed_models() and closed_stata_negbin()
mode.of.arrival.any <- closed_stata_negbin(df.lsoa         = ed_attendances_by_mode_measure,
                                           df.trust        = ed_attendances_by_mode_site_measure_clean,
                                           indicator       = 'ed attendances',
                                           sub.indicator   = 'any',
                                           return.df       = FALSE,
                                           return.model    = TRUE,
                                           return.residuals = FALSE,
                                           digits           = 3)
ed.attendances.admitted.fraction.admitted <- closed_models(df.lsoa         = ed_attendances_admitted_measure,
                                     df.trust         = ed_attendances_admitted_site_measure_clean,
                                     indicator        = 'ed attendances admitted',
                                     sub.indicator    = 'fraction admitted',
                                     panel.lsoa       = model.opts$panel.lsoa,
                                     panel.trust      = model.opts$panel.trust,
                                     timevar          = model.opts$timevar,
                                     outcome          = model.opts$outcome,
                                     model0           = model.opts$mod0,
                                     model0.5         = model.opts$mod0.5,
                                     model1           = model.opts$mod1,
                                     model2           = model.opts$mod2,
                                     model3.1         = model.opts$mod3.1,
                                     model3.2         = model.opts$mod3.2,
                                     model4           = model.opts$mod4,
                                     model5           = model.opts$mod5,
                                     model6.1         = model.opts$mod6.1,
                                     model6.2         = model.opts$mod6.2,
                                     model7.1         = model.opts$mod7.1,
                                     model7.2         = model.opts$mod7.2,
                                     autocorr         = model.opts$autocorr,
                                     panelcorrmethod  = model.opts$panelcorrmethod,
                                     coefficients     = model.opts$coefficients,
                                     seq.times        = model.opts$seq.times,
                                     complete.case    = model.opts$complete.case,
                                     rho.na.rm        = model.opts$rho.na.rm,
                                     theme            = model.opts$theme,
                                     return.df        = model.opts$return.df,
                                     return.model     = model.opts$return.model,
                                     return.residuals = model.opts$return.residuals,
                                     join.line        = model.opts$join.line,
                                     legend           = model.opts$legend)
rbind(mode.of.arrival.any$xtnbreg,
      ed.attendances.admitted.fraction.admitted$all.model.all.coef)

## 2016-10-27 Developing closed_stata_negbin() function
library(Rstata)
options(RStata.StataPath = '/usr/local/stata/stata-mp')
load('~/work/closed/tmp/ed_attendances_clean.RData')
indicator <- 'ed attendances'
sub.indicator <- 'any'
stata(data.in = ed_attendances_by_mode_site_measure_clean,
      data.out = TRUE,
      stata.version = 14.2,
      src      = paste0("~/work/closed/hta_report/do/negbin_lsoa.do '",
                        indicator,
                        "' '",
                        sub.indicator,
                        "'"))
foreign::write.dta(ed_attendances_by_mode_site_measure_clean,
                   file = '~/work/closed/hta_report/data/site.dta')
foreign::write.dta(ed_attendances_by_mode_measure,
                   file = '~/work/closed/hta_report/data/lsoa.dta')

system("/usr/local/stata14/stata-mp -b ~/work/closed/hta_report/do/negbin_site.do 'ed_attendances' 'any'")
system("/usr/local/stata14/stata-mp -b ~/work/closed/hta_report/do/negbin_lsoa.do 'ed_attendances' 'any'")

## 2016-10-26 Testing glarma
closed_glarma(df.lsoa          = ed_attendances_by_mode_measure,
              df.trust         = ed_attendances_by_mode_site_measure_clean,
              indicator        = 'ed attendances',
              sub.indicator    = 'any',
              steps            = c('closure'),
              panel.lsoa       = 'lsoa',
              panel.trust      = 'town',
              timevar          = 'relative.month',
              outcome          = 'value',
              model0           = NULL,
              model1           = c('closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
              model2           = c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
              model3.1         = c('pooled.control * closure', 'town', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
              model3.2         = c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
              model4           = c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
              model5           = c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
              model6.1         = c('season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert', 'diff.time.to.ed'),
              model6.2         = c('town', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert', 'diff.time.to.ed'),
              model7           = c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert', 'diff.time.to.ed'),
              glarma.link       = 'log',
              glarma.model      = list(past_obs = 1),
              glarma.distr      = 'nbinom',
              coefficients     = c('closure.town'),
              return.df        = FALSE,
              return.model     = TRUE,
              return.residuals = FALSE,
              digits           = 3)

## 2016-10-19 Developing glarma
load('~/work/closed/tmp/ed_attendances_clean.RData')
## testing <- ungroup(ed_attendances_by_mode_site_measure_clean) %>%
##            filter(sub.measure == 'ambulance') %>%
##            filter(!is.na(value))
## Model 0 - Start with site only, no covariates
##
## Bishop
## ts.bishop <- filter(testing,
ts.bishop <- filter(ed_attendances_by_mode_site_measure_clean,
                    town == 'Bishop Auckland' & sub.measure == 'ambulance') %>%
             as.data.frame() %>% .[,'value']
regressors.bishop <- filter(ed_attendances_by_mode_site_measure_clean,
                            town == 'Bishop Auckland' & sub.measure == 'ambulance') %>%
                     ungroup() %>%
                     ## dplyr::select(closure) %>%
                     dplyr::select(closure, season, relative.month, nhs111) %>%
                     as.data.frame()
regressors.bishop$season <- as.numeric(regressors.bishop$season)
regressors.bishop <- as.matrix(regressors.bishop)
## regressors.bishop <- as.vector(regressors.bishop)
glarma.bishop.model1 <- glarma(y    = ts.bishop,
                              X  = regressors.bishop,
                              type = 'NegBin')

## 2016-10-19 Checking closed_tsglm() output of formatted coefficients
testing <- closed_tsglm(df.lsoa          = ed_attendances_by_mode_measure,
                        df.trust         = ed_attendances_by_mode_site_measure_clean,
                        indicator        = 'ed attendances',
                        sub.indicator    = 'any',
                        panel.lsoa       = model.opts$panel.lsoa,
                        panel.trust      = model.opts$panel.trust,
                        timevar          = model.opts$timevar,
                        outcome          = model.opts$outcome,
                        model0           = model.opts$mod0,
                        model1           = model.opts$mod1,
                        model2           = model.opts$mod2,
                        model3.1         = model.opts$mod3.1,
                        model3.2         = model.opts$mod3.2,
                        model4           = model.opts$mod4,
                        model5           = model.opts$mod5,
                        model6.1         = model.opts$mod6.1,
                        model6.2         = model.opts$mod6.2,
                        model7           = model.opts$mod7,
                        tsglm.link       = model.opts$tsglm.link,
                        tsglm.model      = model.opts$tsglm.model,
                        tsglm.distr      = model.opts$tsglm.distr,
                        return.df        = model.opts$return.df,
                        return.model     = model.opts$return.model,
                        return.residuals = model.opts$return.residuals,
                        digits           = 3)

## 2016-10-05 Code for asking on Stackexchange
sample <- dplyr::filter(ed_attendances_by_mode_site_measure_clean, town == 'Bishop Auckland' & sub.measure == 'ambulance') %>% ungroup() %>% dplyr::select(relative.month, value, closure) %>% as.data.frame()
names(sample) <- c('relative.month', 'value', 'step')
sample$town <- 1
sample$relative.month <- as.numeric(sample$relative.month) %>% as.integer()
sample <- structure(list(relative.month = 1:48, value = c(1235, 1276, 3402,
 2240, 2588, 2887, 1931, 3019, 1755, 1896, 3337, 2928, 2120, 4360,
 2163, 2965, 2918, 1409, 1674, 2433, 2665, 2316, 3112, 3189, 1690,
 1193, 1517, 1582, 1080, 1547, 1431, 1867, 1599, 1425, 1403, 1279,
 931, 1113, 1683, 1389, 1267, 1292, 1036, 1557, 1326, 1950, 1289,
 1552), step = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), town = structure(c(2L,
 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L,
 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L,
 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L), .Label = c("Basingstoke",
 "Bishop Auckland", "Blackburn", "Carlisle", "Grimsby", "Hartlepool",
 "Hemel Hempstead", "Newark", "Rochdale", "Rotherham", "Salford",
 "Salisbury", "Scarborough", "Scunthorpe", "Southport", "Wansbeck",
 "Warwick", "Whitehaven", "Wigan", "Yeovil"), class = "factor")), row.names = c(NA,
 -48L), .Names = c("relative.month", "value", "step", "town"
 ), class = "data.frame")

## Replace value with something random based on pre-post
set.seed(6549876)
sample$value <- ifelse(sample$step == 0 ,
                       rnorm(n = 24,  mean = 2472, sd = 302),
                       rnorm(n = 24, mean = 1488, sd = 297)) %>%
    round()

## Libraries
library(panelAR)
library(tscount)
## Data
dput(sample)
## Quick plot
png(file = '~/work/closed/tmp/pwr_negbin.png', width = 1024, height = 768)
ggplot(sample, aes(x = relative.month, y = value)) +
    geom_line() + geom_vline(xintercept = 24.5)
dev.off()
## Pre and post Means
group_by(sample, step) %>%
    summarise(mean = mean(value, na.rm = TRUE),
              sd   = sd(value, na.rm = TRUE))
## Simple Prais-Winsten Regression with 1-step
model <- reformulate(response   = 'value',
                     termlabels = c('step'))
pwr.1step <- panelAR(formula         = model,
                     data            = sample,
                     panelVar        = 'town',
                     timeVar         = 'relative.month',
                     autoCorr        = 'ar1',
                     panelCorrMethod = 'pcse',
                     rho.na.rm       = TRUE,
                     seq.times       = FALSE)
pwr.1step %>% summary()
## Simple Prais-Winsten Regression with 1-step and time as co-variate
model <- reformulate(response   = 'value',
                     termlabels = c('step', 'relative.month'))
pwr.1step.time <- panelAR(formula         = model,
                          data            = sample,
                          panelVar        = 'town',
                          timeVar         = 'relative.month',
                          autoCorr        = 'ar1',
                          panelCorrMethod = 'pcse',
                          rho.na.rm       = TRUE,
                          seq.times       = FALSE)
pwr.1step.time %>% summary()
## Negative Binomial Time-Series with 1-step
outcome <- sample[,'value']
covariates <- dplyr::select(sample, step)
negbin.1step <- tsglm(ts    = outcome,
                      link  = 'log',
                      model = list(past_obs = 1),
                      xreg  = covariates,
                      distr = 'nbinom')
negbin.1step %>% summary()
negbin.1step %>% coefficients() %>% exp()
## Negative Binomial Time-Series with 1-step and time
outcome <- sample[,'value']
covariates <- dplyr::select(sample, step, relative.month)
negbin.1step.time <- tsglm(ts    = outcome,
                           link  = 'log',
                           model = list(past_obs = 1),
                           xreg  = covariates,
                           distr = 'nbinom')
negbin.1step.time %>% summary()
negbin.1step.time %>% coefficients() %>% exp()
## Save for moving back
save(sample,
     pwr.1step, pwr.1step.time,
     negbin.1step, negbin.1step.time,
     file = '~/work/closed/tmp/pwr_negbin.Rdata')

## 2016-10-04 Checking closed_tsglm() after adding in code to derive the difference/percent difference
t <- closed_tsglm(df.lsoa         = emergency_admissions_measure,
                  df.trust         = emergency_admissions_site_measure_clean,
                  indicator        = 'all emergency admissions',
                  sub.indicator    = 'all')

## 2016-10-03 Testing model 0.5 in closed_models() function
closed_models(df.lsoa         = ed_attendances_by_mode_measure,
              df.trust        = ed_attendances_by_mode_site_measure_clean,
              indicator       = 'ed attendances',
              sub.indicator   = 'any',
              steps           = c('closure'),
              fit.with        = 'both',
              panel.lsoa      = 'lsoa',
              panel.trust     = 'town',
              timevar         = 'relative.month',
              outcome         = 'value',
              model0          = c('closure'),
              model0.5        = c('closure', 'relative.month'),
              model1          = c('closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
              model2          = c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
              model3.1        = c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
              model3.2        = c('pooled.control * closure', 'town', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
              model4          = c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
              model5          = c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
              model6.1        = c('season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert', 'diff.time.to.ed'),
              model6.2        = c('town', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert', 'diff.time.to.ed'),
              model7          = c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert', 'diff.time.to.ed'),
              autocorr        = 'ar1',
              panelcorrmethod = 'pcse',
              coefficients    = 'closure.town',
              complete.case   = TRUE,
              weights         = '',
              seq.times       = FALSE,
              rho.na.rm       = FALSE,
              plot            = TRUE,
              common.y        = TRUE,
              theme           = theme_bw(),
              return.df       = FALSE,
              return.model    = TRUE,
              return.residuals = FALSE,
              join.line        = TRUE,
              legend           = FALSE,
              digits           = 3)


## 2016-09-12 Testing models using tscount package (mainly tsglm() function)
## save(ed_attendances_by_mode_site_measure_clean, file = '~/work/closed/tmp/ed_attendances_clean.RData')
##
## Caveats...
##
## 1. tsglm() works with vectors/matrices, doesn't follow the same structure/syntax as other
##    regression modelling functions.
##
## 2. tsglm() does not permit missing data points in the time series, therefore have to
##    remove those observations completely
##
## 3. For some reason NHS111 isn't coming out in the subsetting?
##
## 4. The component...  model = list(past_obs = 1) ...means that autorgression is performed on
##    observation i-1
##
## 5. Hit a problem...
##
##    Error in optim(theta.old, fun, gradient, control = control, method = method,  :
##    initial value in 'vmmin' is not finite
##    Timing stopped at: 0 0 0.003
##
##    ...this is caused by one matrix having a determinent of 0 and when logs are then taken
##    the value is Infinity which isn't valid.
library(tscount)
load('~/work/closed/tmp/ed_attendances_clean.RData')
ed_attendances_by_mode_site_measure_clean <- ungroup(ed_attendances_by_mode_site_measure_clean) %>%
                                             filter(sub.measure == 'ambulance') %>%
                                             filter(!is.na(value))
## Model 1a - Start with site only, no covariates
##
## Bishop
ts.bishop <- filter(ed_attendances_by_mode_site_measure_clean,
                     town == 'Bishop Auckland') %>% as.data.frame() %>% .[,'value']
regressors.bishop <- filter(ed_attendances_by_mode_site_measure_clean,
                            town == 'Bishop Auckland') %>%
                     dplyr::select(closure, season, relative.month, nhs111)  %>% as.data.frame()
                     ## dplyr::select(closure) %>% as.data.frame()
regressors.bishop$season <- as.numeric(regressors.bishop$season)
tsglm.bishop.model1 <- tsglm(ts    = ts.bishop,
                                 link  = 'identity',
                                 model = list(past_obs = 1),
                                 xreg = regressors.bishop,
                                 distr = 'nbinom')
## Hartlepool
ts.hartlepool <- filter(ed_attendances_by_mode_site_measure_clean,
                     town == 'Hartlepool') %>% as.data.frame() %>% .[,'value']
regressors.hartlepool <- filter(ed_attendances_by_mode_site_measure_clean,
                            town == 'Hartlepool') %>%
                     dplyr::select(closure, season, relative.month, nhs111, other.centre, ambulance.divert)  %>% as.data.frame()
regressors.hartlepool$season <- as.numeric(regressors.hartlepool$season)
tsglm.hartlepool.model1 <- tsglm(ts    = ts.hartlepool,
                                 link  = 'identity',
                                 model = list(past_obs = 1),
                                 xreg = regressors.hartlepool,
                                 distr = 'nbinom')
## Hemel Hempstead
ts.hemel <- filter(ed_attendances_by_mode_site_measure_clean,
                     town == 'Hemel Hempstead') %>% as.data.frame() %>% .[,'value']
regressors.hemel <- filter(ed_attendances_by_mode_site_measure_clean,
                            town == 'Hemel Hempstead') %>%
                     dplyr::select(closure, season, relative.month, nhs111, other.centre, ambulance.divert) %>% as.data.frame()
regressors.hemel$season <- as.numeric(regressors.hemel$season)
tsglm.hemel.model1 <- tsglm(ts    = ts.hemel,
                                 link  = 'identity',
                                 model = list(past_obs = 1),
                                 xreg = regressors.hemel,
                                 distr = 'nbinom')
## Newark
ts.newark <- filter(ed_attendances_by_mode_site_measure_clean,
                     town == 'Newark') %>% as.data.frame() %>% .[,'value']
regressors.newark <- filter(ed_attendances_by_mode_site_measure_clean,
                            town == 'Newark') %>%
                     dplyr::select(closure, season, relative.month, nhs111, other.centre, ambulance.divert) %>% as.data.frame()
regressors.newark$season <- as.numeric(regressors.newark$season)
tsglm.newark.model1 <- tsglm(ts    = ts.newark,
                                 link  = 'identity',
                                 model = list(past_obs = 1),
                                 xreg = regressors.newark,
                                 distr = 'nbinom')
## Rochdale
ts.rochdale <- filter(ed_attendances_by_mode_site_measure_clean,
                     town == 'Rochdale') %>% as.data.frame() %>% .[,'value']
regressors.rochdale <- filter(ed_attendances_by_mode_site_measure_clean,
                            town == 'Rochdale') %>%
                     dplyr::select(closure, season, relative.month, nhs111, other.centre, ambulance.divert) %>% as.data.frame()
regressors.rochdale$season <- as.numeric(regressors.rochdale$season)
tsglm.rochdale.model1 <- tsglm(ts    = ts.rochdale,
                                 link  = 'identity',
                                 model = list(past_obs = 1),
                                 xreg = regressors.rochdale,
                                 distr = 'nbinom')

## Model 2 - Case and its primary matched control
## Bishop
ts.bishop <- filter(ed_attendances_by_mode_site_measure_clean,
                     town %in% c('Bishop Auckland', 'Whitehaven')) %>% as.data.frame() %>% .[,'value']
ed_attendances_by_mode_site_measure_clean$town_ <- ifelse(ed_attendances_by_mode_site_measure_clean$town == 'Bishop Auckland', yes = 1, no = 0)
ed_attendances_by_mode_site_measure_clean$town_closure <- ed_attendances_by_mode_site_measure_clean$town_ * ed_attendances_by_mode_site_measure_clean$closure
regressors.bishop <- filter(ed_attendances_by_mode_site_measure_clean,
                            town %in% c('Bishop Auckland', 'Whitehaven')) %>%
                     dplyr::select(town_, closure, town_closure, season, relative.month, nhs111, other.centre, ambulance.divert)  %>% as.data.frame()
regressors.bishop$season <- as.numeric(regressors.bishop$season)
tsglm.bishop.model2 <- tsglm(ts    = ts.bishop,
                                 link  = 'identity',
                                 model = list(past_obs = 1),
                                 xreg = regressors.bishop,
                                 distr = 'nbinom')
## Hartlepool
ts.hartlepool <- filter(ed_attendances_by_mode_site_measure_clean,
                     town %in% c('Hartlepool', 'Grimsby')) %>% as.data.frame() %>% .[,'value']
ed_attendances_by_mode_site_measure_clean$town_ <- ifelse(ed_attendances_by_mode_site_measure_clean$town == 'Hartlepool', yes = 1, no = 0)
ed_attendances_by_mode_site_measure_clean$town_closure <- ed_attendances_by_mode_site_measure_clean$town_ * ed_attendances_by_mode_site_measure_clean$closure
regressors.hartlepool <- filter(ed_attendances_by_mode_site_measure_clean,
                            town %in% c('Hartlepool', 'Grimsby')) %>%
                     dplyr::select(town_, closure, town_closure, season, relative.month, nhs111, other.centre, ambulance.divert)  %>% as.data.frame()
regressors.hartlepool$season <- as.numeric(regressors.hartlepool$season)
tsglm.hartlepool.model2 <- tsglm(ts    = ts.hartlepool,
                                 link  = 'identity',
                                 model = list(past_obs = 1),
                                 xreg = regressors.hartlepool,
                                 distr = 'nbinom')
## Hemel Hempstead
ts.hemel <- filter(ed_attendances_by_mode_site_measure_clean,
                     town %in% c('Hemel Hempstead', 'Warwick')) %>% as.data.frame() %>% .[,'value']
ed_attendances_by_mode_site_measure_clean$town_ <- ifelse(ed_attendances_by_mode_site_measure_clean$town == 'Hemel Hempstead', yes = 1, no = 0)
ed_attendances_by_mode_site_measure_clean$town_closure <- ed_attendances_by_mode_site_measure_clean$town_ * ed_attendances_by_mode_site_measure_clean$closure
regressors.hemel <- filter(ed_attendances_by_mode_site_measure_clean,
                            town %in% c('Hemel Hempstead', 'Warwick')) %>%
                     dplyr::select(town_, closure, town_closure, season, relative.month, nhs111, other.centre, ambulance.divert) %>% as.data.frame()
regressors.hemel$season <- as.numeric(regressors.hemel$season)
tsglm.hemel.model2 <- tsglm(ts    = ts.hemel,
                                 link  = 'identity',
                                 model = list(past_obs = 1),
                                 xreg = regressors.hemel,
                                 distr = 'nbinom')
## Newark
ts.newark <- filter(ed_attendances_by_mode_site_measure_clean,
                    town %in% c('Newark', 'Southport')) %>% as.data.frame() %>% .[,'value']
ed_attendances_by_mode_site_measure_clean$town_ <- ifelse(ed_attendances_by_mode_site_measure_clean$town == 'Newark', yes = 1, no = 0)
ed_attendances_by_mode_site_measure_clean$town_closure <- ed_attendances_by_mode_site_measure_clean$town_ * ed_attendances_by_mode_site_measure_clean$closure
regressors.newark <- filter(ed_attendances_by_mode_site_measure_clean,
                            town %in% c('Newark', 'Southport')) %>%
                     dplyr::select(town_, closure, town_closure, season, relative.month, nhs111, other.centre, ambulance.divert) %>% as.data.frame()
regressors.newark$season <- as.numeric(regressors.newark$season)
tsglm.newark.model2 <- tsglm(ts    = ts.newark,
                                 link  = 'identity',
                                 model = list(past_obs = 1),
                                 xreg = regressors.newark,
                                 distr = 'nbinom')
## Rochdale
ts.rochdale <- filter(ed_attendances_by_mode_site_measure_clean,
                     town %in% c('Rochdale', 'Rotherham')) %>% as.data.frame() %>% .[,'value']
ed_attendances_by_mode_site_measure_clean$town_ <- ifelse(ed_attendances_by_mode_site_measure_clean$town == 'Rochdale', yes = 1, no = 0)
ed_attendances_by_mode_site_measure_clean$town_closure <- ed_attendances_by_mode_site_measure_clean$town_ * ed_attendances_by_mode_site_measure_clean$closure
regressors.rochdale <- filter(ed_attendances_by_mode_site_measure_clean,
                            town %in% c('Rochdale', 'Rotherham')) %>%
                     dplyr::select(town_, closure, town_closure, season, relative.month, nhs111, other.centre, ambulance.divert) %>% as.data.frame()
regressors.rochdale$season <- as.numeric(regressors.rochdale$season)
tsglm.rochdale.model2 <- tsglm(ts    = ts.rochdale,
                                 link  = 'identity',
                                 model = list(past_obs = 1),
                                 xreg = regressors.rochdale,
                                 distr = 'nbinom')

## Model 3.1 - Case and all its matched controls (unpooled)
## Bishop
ts.bishop <- filter(ed_attendances_by_mode_site_measure_clean,
                     town %in% c('Bishop Auckland', 'Whitehaven', 'Salford', 'Scarborough')) %>% as.data.frame() %>% .[,'value']
ed_attendances_by_mode_site_measure_clean$town_ <- 1
ed_attendances_by_mode_site_measure_clean <- within(ed_attendances_by_mode_site_measure_clean, {
                                                    town_[town == 'Bishop Auckland'] <- 0
                                                    town_[town == 'Salford']         <- 2
                                                    town_[town == 'Scarborough']     <- 3
})
ed_attendances_by_mode_site_measure_clean$town_closure <- ed_attendances_by_mode_site_measure_clean$town_ * ed_attendances_by_mode_site_measure_clean$closure
regressors.bishop <- filter(ed_attendances_by_mode_site_measure_clean,
                            town %in% c('Bishop Auckland', 'Whitehaven', 'Salford', 'Scarborough')) %>%
                     dplyr::select(town_, closure, town_closure, season, relative.month, nhs111, other.centre, ambulance.divert)  %>% as.data.frame()
regressors.bishop$season <- as.numeric(regressors.bishop$season)
tsglm.bishop.model3.1 <- tsglm(ts    = ts.bishop,
                                 link  = 'identity',
                                 model = list(past_obs = 1),
                                 xreg = regressors.bishop,
                                 distr = 'nbinom')
## Hartlepool
ts.hartlepool <- filter(ed_attendances_by_mode_site_measure_clean,
                     town %in% c('Hartlepool', 'Grimsby', 'Blackburn', 'Wigan')) %>% as.data.frame() %>% .[,'value']
ed_attendances_by_mode_site_measure_clean$town_ <- 1
ed_attendances_by_mode_site_measure_clean <- within(ed_attendances_by_mode_site_measure_clean, {
                                                    town_[town == 'Hartlepool'] <- 0
                                                    town_[town == 'Blackburn']  <- 2
                                                    town_[town == 'Wigan']      <- 3
})
ed_attendances_by_mode_site_measure_clean$town_closure <- ed_attendances_by_mode_site_measure_clean$town_ * ed_attendances_by_mode_site_measure_clean$closure
regressors.hartlepool <- filter(ed_attendances_by_mode_site_measure_clean,
                            town %in% c('Hartlepool', 'Grimsby', 'Blackburn', 'Wigan')) %>%
                     dplyr::select(town_, closure, town_closure, season, relative.month, nhs111, other.centre, ambulance.divert)  %>% as.data.frame()
regressors.hartlepool$season <- as.numeric(regressors.hartlepool$season)
tsglm.hartlepool.model3.1 <- tsglm(ts    = ts.hartlepool,
                                 link  = 'identity',
                                 model = list(past_obs = 1),
                                 xreg = regressors.hartlepool,
                                 distr = 'nbinom')
## Hemel Hempstead
ts.hemel <- filter(ed_attendances_by_mode_site_measure_clean,
                     town %in% c('Hemel Hempstead', 'Warwick', 'Basingstoke', 'Yeovil')) %>% as.data.frame() %>% .[,'value']
ed_attendances_by_mode_site_measure_clean$town_ <- 1
ed_attendances_by_mode_site_measure_clean <- within(ed_attendances_by_mode_site_measure_clean, {
                                                    town_[town == 'Hemel Hempstead'] <- 0
                                                    town_[town == 'Basingstoke']     <- 2
                                                    town_[town == 'Yeovil']          <- 3
})
ed_attendances_by_mode_site_measure_clean$town_closure <- ed_attendances_by_mode_site_measure_clean$town_ * ed_attendances_by_mode_site_measure_clean$closure
regressors.hemel <- filter(ed_attendances_by_mode_site_measure_clean,
                            town %in% c('Hemel Hempstead', 'Warwick', 'Basingstoke', 'Yeovil')) %>%
                     dplyr::select(town_, closure, town_closure, season, relative.month, nhs111, other.centre, ambulance.divert) %>% as.data.frame()
regressors.hemel$season <- as.numeric(regressors.hemel$season)
tsglm.hemel.model3.1 <- tsglm(ts    = ts.hemel,
                                 link  = 'identity',
                                 model = list(past_obs = 1),
                                 xreg = regressors.hemel,
                                 distr = 'nbinom')
## Newark
ts.newark <- filter(ed_attendances_by_mode_site_measure_clean,
                     town %in% c('Newark', 'Southport', 'Carlisle', 'Salisbury')) %>% as.data.frame() %>% .[,'value']
ed_attendances_by_mode_site_measure_clean$town_ <- 1
ed_attendances_by_mode_site_measure_clean <- within(ed_attendances_by_mode_site_measure_clean, {
                                                    town_[town == 'Newark']    <- 0
                                                    town_[town == 'Carlisle']  <- 2
                                                    town_[town == 'Salisbury'] <- 3
})
ed_attendances_by_mode_site_measure_clean$town_closure <- ed_attendances_by_mode_site_measure_clean$town_ * ed_attendances_by_mode_site_measure_clean$closure
regressors.newark <- filter(ed_attendances_by_mode_site_measure_clean,
                            town %in% c('Newark', 'Southport', 'Carlisle', 'Salisbury')) %>%
                     dplyr::select(town_, closure, town_closure, season, relative.month, nhs111, other.centre, ambulance.divert) %>% as.data.frame()
regressors.newark$season <- as.numeric(regressors.newark$season)
tsglm.newark.model3.1 <- tsglm(ts    = ts.newark,
                                 link  = 'identity',
                                 model = list(past_obs = 1),
                                 xreg = regressors.newark,
                                 distr = 'nbinom')
## Rochdale
ts.rochdale <- filter(ed_attendances_by_mode_site_measure_clean,
                     town %in% c('Rochdale', 'Rotherham', 'Scunthorpe', 'Wansbeck')) %>% as.data.frame() %>% .[,'value']
ed_attendances_by_mode_site_measure_clean$town_ <- 1
ed_attendances_by_mode_site_measure_clean <- within(ed_attendances_by_mode_site_measure_clean, {
                                                    town_[town == 'Rochdale']   <- 0
                                                    town_[town == 'Scunthorpe'] <- 2
                                                    town_[town == 'Wansbeck']   <- 3
})
ed_attendances_by_mode_site_measure_clean$town_closure <- ed_attendances_by_mode_site_measure_clean$town_ * ed_attendances_by_mode_site_measure_clean$closure
regressors.rochdale <- filter(ed_attendances_by_mode_site_measure_clean,
                            town %in% c('Rochdale', 'Rotherham', 'Scunthorpe', 'Wansbeck')) %>%
                     dplyr::select(town_, closure, town_closure, season, relative.month, nhs111, other.centre, ambulance.divert) %>% as.data.frame()
regressors.rochdale$season <- as.numeric(regressors.rochdale$season)
tsglm.rochdale.model3.1 <- tsglm(ts    = ts.rochdale,
                                 link  = 'identity',
                                 model = list(past_obs = 1),
                                 xreg = regressors.rochdale,
                                 distr = 'nbinom')

## Model 3.2 - Case and all its matched controls (pooled)
## Bishop
ts.bishop <- filter(ed_attendances_by_mode_site_measure_clean,
                     town %in% c('Bishop Auckland', 'Whitehaven', 'Salford', 'Scarborough')) %>% as.data.frame() %>% .[,'value']
ed_attendances_by_mode_site_measure_clean$town_ <- ifelse(ed_attendances_by_mode_site_measure_clean$town == 'Bishop Auckland', yes = 1, no = 0)
ed_attendances_by_mode_site_measure_clean$town_closure <- ed_attendances_by_mode_site_measure_clean$town_ * ed_attendances_by_mode_site_measure_clean$closure
regressors.bishop <- filter(ed_attendances_by_mode_site_measure_clean,
                            town %in% c('Bishop Auckland', 'Whitehaven', 'Salford', 'Scarborough')) %>%
                     dplyr::select(town_, closure, town_closure, season, relative.month, nhs111, other.centre, ambulance.divert)  %>% as.data.frame()
regressors.bishop$season <- as.numeric(regressors.bishop$season)
tsglm.bishop.model3.2 <- tsglm(ts    = ts.bishop,
                                 link  = 'identity',
                                 model = list(past_obs = 1),
                                 xreg = regressors.bishop,
                                 distr = 'nbinom')
## Hartlepool
ts.hartlepool <- filter(ed_attendances_by_mode_site_measure_clean,
                     town %in% c('Hartlepool', 'Grimsby', 'Blackburn', 'Wigan')) %>% as.data.frame() %>% .[,'value']
ed_attendances_by_mode_site_measure_clean$town_ <- ifelse(ed_attendances_by_mode_site_measure_clean$town == 'Hartlepool', yes = 1, no = 0)
ed_attendances_by_mode_site_measure_clean$town_closure <- ed_attendances_by_mode_site_measure_clean$town_ * ed_attendances_by_mode_site_measure_clean$closure
regressors.hartlepool <- filter(ed_attendances_by_mode_site_measure_clean,
                            town %in% c('Hartlepool', 'Grimsby', 'Blackburn', 'Wigan')) %>%
                     dplyr::select(town_, closure, town_closure, season, relative.month, nhs111, other.centre, ambulance.divert)  %>% as.data.frame()
regressors.hartlepool$season <- as.numeric(regressors.hartlepool$season)
tsglm.hartlepool.model3.2 <- tsglm(ts    = ts.hartlepool,
                                 link  = 'identity',
                                 model = list(past_obs = 1),
                                 xreg = regressors.hartlepool,
                                 distr = 'nbinom')
## Hemel Hempstead
ts.hemel <- filter(ed_attendances_by_mode_site_measure_clean,
                     town %in% c('Hemel Hempstead', 'Warwick', 'Basingstoke', 'Yeovil')) %>% as.data.frame() %>% .[,'value']
ed_attendances_by_mode_site_measure_clean$town_ <- ifelse(ed_attendances_by_mode_site_measure_clean$town == 'Hemel Hempstead', yes = 1, no = 0)
ed_attendances_by_mode_site_measure_clean$town_closure <- ed_attendances_by_mode_site_measure_clean$town_ * ed_attendances_by_mode_site_measure_clean$closure
regressors.hemel <- filter(ed_attendances_by_mode_site_measure_clean,
                            town %in% c('Hemel Hempstead', 'Warwick', 'Basingstoke', 'Yeovil')) %>%
                     dplyr::select(town_, closure, town_closure, season, relative.month, nhs111, other.centre, ambulance.divert) %>% as.data.frame()
regressors.hemel$season <- as.numeric(regressors.hemel$season)
tsglm.hemel.model3.2 <- tsglm(ts    = ts.hemel,
                                 link  = 'identity',
                                 model = list(past_obs = 1),
                                 xreg = regressors.hemel,
                                 distr = 'nbinom')
## Newark
ts.newark <- filter(ed_attendances_by_mode_site_measure_clean,
                     town %in% c('Newark', 'Southport', 'Carlisle', 'Salisbury')) %>% as.data.frame() %>% .[,'value']
ed_attendances_by_mode_site_measure_clean$town_ <- ifelse(ed_attendances_by_mode_site_measure_clean$town == 'Newark', yes = 1, no = 0)
ed_attendances_by_mode_site_measure_clean$town_closure <- ed_attendances_by_mode_site_measure_clean$town_ * ed_attendances_by_mode_site_measure_clean$closure
regressors.newark <- filter(ed_attendances_by_mode_site_measure_clean,
                            town %in% c('Newark', 'Southport', 'Carlisle', 'Salisbury')) %>%
                     dplyr::select(town_, closure, town_closure, season, relative.month, nhs111, other.centre, ambulance.divert) %>% as.data.frame()
regressors.newark$season <- as.numeric(regressors.newark$season)
tsglm.newark.model3.2 <- tsglm(ts    = ts.newark,
                                 link  = 'identity',
                                 model = list(past_obs = 1),
                                 xreg = regressors.newark,
                                 distr = 'nbinom')
## Rochdale
ts.rochdale <- filter(ed_attendances_by_mode_site_measure_clean,
                     town %in% c('Rochdale', 'Rotherham', 'Scunthorpe', 'Wansbeck')) %>% as.data.frame() %>% .[,'value']
ed_attendances_by_mode_site_measure_clean$town_ <- ifelse(ed_attendances_by_mode_site_measure_clean$town == 'Rochdale', yes = 1, no = 0)
ed_attendances_by_mode_site_measure_clean$town_closure <- ed_attendances_by_mode_site_measure_clean$town_ * ed_attendances_by_mode_site_measure_clean$closure
regressors.rochdale <- filter(ed_attendances_by_mode_site_measure_clean,
                            town %in% c('Rochdale', 'Rotherham', 'Scunthorpe', 'Wansbeck')) %>%
                     dplyr::select(town_, closure, town_closure, season, relative.month, nhs111, other.centre, ambulance.divert) %>% as.data.frame()
regressors.rochdale$season <- as.numeric(regressors.rochdale$season)
tsglm.rochdale.model3.2 <- tsglm(ts    = ts.rochdale,
                                 link  = 'identity',
                                 model = list(past_obs = 1),
                                 xreg = regressors.rochdale,
                                 distr = 'nbinom')

## Model 4 - All case sites and their primary matched controls
ts.vec <- filter(ed_attendances_by_mode_site_measure_clean,
                 town %in% c('Bishop Auckland', 'Whitehaven', 'Hartlepool', 'Grimsby', 'Hemel Hempstead', 'Warwick', 'Newark', 'Southport', 'Rochdale', 'Rotherham')) %>% as.data.frame() %>% .[,'value']
ed_attendances_by_mode_site_measure_clean$town_ <- 0
ed_attendances_by_mode_site_measure_clean <- within(ed_attendances_by_mode_site_measure_clean, {
                                                    town_[town == 'Bishop Auckland'] <- 1
                                                    town_[town == 'Hartlepool'] <- 2
                                                    town_[town == 'Grimsby'] <- 3
                                                    town_[town == 'Hemel Hempstead'] <- 4
                                                    town_[town == 'Warwick'] <- 5
                                                    town_[town == 'Newark'] <- 6
                                                    town_[town == 'Southport'] <- 7
                                                    town_[town == 'Rochdale'] <- 8
                                                    town_[town == 'Rotherham'] <- 9
})
ed_attendances_by_mode_site_measure_clean$town_closure <- ed_attendances_by_mode_site_measure_clean$town_ * ed_attendances_by_mode_site_measure_clean$closure
regressors <- filter(ed_attendances_by_mode_site_measure_clean,
                     town %in% c('Bishop Auckland', 'Whitehaven', 'Hartlepool', 'Grimsby', 'Hemel Hempstead', 'Warwick', 'Newark', 'Southport', 'Rochdale', 'Rotherham')) %>%
                     dplyr::select(town_, closure, town_closure, season, relative.month, nhs111, other.centre, ambulance.divert) %>% as.data.frame()
regressors$season <- as.numeric(regressors$season)
tsglm.all.model4 <- tsglm(ts    = ts.vec,
                                 link  = 'identity',
                                 model = list(past_obs = 1),
                                 xreg = regressors,
                                 distr = 'nbinom')

## Model 5 - All Case sites and all matched controls
ts.vec <- as.data.frame(ed_attendances_by_mode_site_measure_clean) %>% .[,'value']
ed_attendances_by_mode_site_measure_clean$town_ <- 0
ed_attendances_by_mode_site_measure_clean <- within(ed_attendances_by_mode_site_measure_clean, {
                                                    town_[town == 'Basingstoke']     <- 1
                                                    town_[town == 'Bishop Auckland'] <- 2
                                                    town_[town == 'Blackburn']       <- 3
                                                    town_[town == 'Carlisle']        <- 4
                                                    town_[town == 'Grimsby']         <- 5
                                                    town_[town == 'Hartlepool']      <- 6
                                                    town_[town == 'Hemel Hempstead'] <- 7
                                                    town_[town == 'Neawrk']          <- 8
                                                    town_[town == 'Rochdale']        <- 9
                                                    town_[town == 'Rotherham']       <- 10
                                                    town_[town == 'Salford']         <- 11
                                                    town_[town == 'Salisbury']       <- 12
                                                    town_[town == 'Scarborough']     <- 13
                                                    town_[town == 'Scunthorpe']      <- 14
                                                    town_[town == 'Southport']       <- 15
                                                    town_[town == 'Wansbeck']        <- 16
                                                    town_[town == 'Warwick']         <- 17
                                                    town_[town == 'Wigan']           <- 18
                                                    town_[town == 'Yeovil']          <- 19
})
ed_attendances_by_mode_site_measure_clean$town_closure <- ed_attendances_by_mode_site_measure_clean$town_ * ed_attendances_by_mode_site_measure_clean$closure
regressors <- dplyr::select(ed_attendances_by_mode_site_measure_clean, town_, closure, town_closure, season, relative.month, nhs111, other.centre, ambulance.divert) %>% as.data.frame()
regressors$season <- as.numeric(regressors$season)
tsglm.all.model5 <- tsglm(ts    = ts.vec,
                                 link  = 'identity',
                                 model = list(past_obs = 1),
                                 xreg = regressors,
                                 distr = 'nbinom')


## Model 6.1 - LSOA level site only
##
## Bishop
ts.bishop <- filter(ed_attendances_by_mode_measure,
                     town == 'Bishop Auckland') %>% as.data.frame() %>% .[,'value']
regressors.bishop <- filter(ed_attendances_by_mode_measure,
                            town == 'Bishop Auckland') %>%
                     dplyr::select(closure, season, relative.month, nhs111, other.centre, ambulance.divert)  %>% as.data.frame()
                     ## dplyr::select(closure) %>% as.data.frame()
regressors.bishop$season <- as.numeric(regressors.bishop$season)
tsglm.bishop.model6.1 <- tsglm(ts    = ts.bishop,
                                 link  = 'identity',
                                 model = list(past_obs = 1),
                                 xreg = regressors.bishop,
                                 distr = 'nbinom')

## Hartlepool
ts.hartlepool <- filter(ed_attendances_by_mode_measure,
                     town == 'Hartlepool') %>% as.data.frame() %>% .[,'value']
regressors.hartlepool <- filter(ed_attendances_by_mode_measure,
                            town == 'Hartlepool') %>%
                     dplyr::select(closure, season, relative.month, nhs111, other.centre, ambulance.divert)  %>% as.data.frame()
regressors.hartlepool$season <- as.numeric(regressors.hartlepool$season)
tsglm.hartlepool.model6.1 <- tsglm(ts    = ts.hartlepool,
                                 link  = 'identity',
                                 model = list(past_obs = 1),
                                 xreg = regressors.hartlepool,
                                 distr = 'nbinom')

## Hemel Hempstead
ts.hemel <- filter(ed_attendances_by_mode_measure,
                     town == 'Hemel Hempstead') %>% as.data.frame() %>% .[,'value']
regressors.hemel <- filter(ed_attendances_by_mode_measure,
                            town == 'Hemel Hempstead') %>%
                     dplyr::select(closure, season, relative.month, nhs111, other.centre, ambulance.divert) %>% as.data.frame()
regressors.hemel$season <- as.numeric(regressors.hemel$season)
tsglm.hemel.model6.1 <- tsglm(ts    = ts.hemel,
                                 link  = 'identity',
                                 model = list(past_obs = 1),
                                 xreg = regressors.hemel,
                                 distr = 'nbinom')

## Newark
ts.newark <- filter(ed_attendances_by_mode_measure,
                     town == 'Newark') %>% as.data.frame() %>% .[,'value']
regressors.newark <- filter(ed_attendances_by_mode_measure,
                            town == 'Newark') %>%
                     dplyr::select(closure, season, relative.month, nhs111, other.centre, ambulance.divert) %>% as.data.frame()
regressors.newark$season <- as.numeric(regressors.newark$season)
tsglm.newark.model6.1 <- tsglm(ts    = ts.newark,
                                 link  = 'identity',
                                 model = list(past_obs = 1),
                                 xreg = regressors.newark,
                                 distr = 'nbinom')

## Rochdale
ts.rochdale <- filter(ed_attendances_by_mode_measure,
                     town == 'Rochdale') %>% as.data.frame() %>% .[,'value']
regressors.rochdale <- filter(ed_attendances_by_mode_measure,
                            town == 'Rochdale') %>%
                     dplyr::select(closure, season, relative.month, nhs111, other.centre, ambulance.divert) %>% as.data.frame()
regressors.rochdale$season <- as.numeric(regressors.rochdale$season)
tsglm.rochdale.model6.1 <- tsglm(ts    = ts.rochdale,
                                 link  = 'identity',
                                 model = list(past_obs = 1),
                                 xreg = regressors.rochdale,
                                 distr = 'nbinom')


## Model 6.2 - Case and its primary matched control
## Bishop
ts.bishop <- filter(ed_attendances_by_mode_site_measure_clean,
                     town %in% c('Bishop Auckland', 'Whitehaven')) %>% as.data.frame() %>% .[,'value']
ed_attendances_by_mode_site_measure_clean$town_ <- ifelse(ed_attendances_by_mode_site_measure_clean$town == 'Bishop Auckland', yes = 1, no = 0)
regressors.bishop <- filter(ed_attendances_by_mode_site_measure_clean,
                            town %in% c('Bishop Auckland', 'Whitehaven')) %>%
                     dplyr::select(town_, closure, season, relative.month, nhs111, other.centre, ambulance.divert)  %>% as.data.frame()
regressors.bishop$season <- as.numeric(regressors.bishop$season)
tsglm.bishop.model6.2 <- tsglm(ts    = ts.bishop,
                                 link  = 'identity',
                                 model = list(past_obs = 1),
                                 xreg = regressors.bishop,
                                 distr = 'nbinom')
## Hartlepool
ts.hartlepool <- filter(ed_attendances_by_mode_site_measure_clean,
                     town %in% c('Hartlepool', 'Grimsby')) %>% as.data.frame() %>% .[,'value']
ed_attendances_by_mode_site_measure_clean$town_ <- ifelse(ed_attendances_by_mode_site_measure_clean$town == 'Hartlepool', yes = 1, no = 0)
regressors.hartlepool <- filter(ed_attendances_by_mode_site_measure_clean,
                            town %in% c('Hartlepool', 'Grimsby')) %>%
                     dplyr::select(town_, closure, season, relative.month, nhs111, other.centre, ambulance.divert)  %>% as.data.frame()
regressors.hartlepool$season <- as.numeric(regressors.hartlepool$season)
tsglm.hartlepool.model6.2 <- tsglm(ts    = ts.hartlepool,
                                 link  = 'identity',
                                 model = list(past_obs = 1),
                                 xreg = regressors.hartlepool,
                                 distr = 'nbinom')
## Hemel Hempstead
ts.hemel <- filter(ed_attendances_by_mode_site_measure_clean,
                     town %in% c('Hemel Hempstead', 'Warwick')) %>% as.data.frame() %>% .[,'value']
ed_attendances_by_mode_site_measure_clean$town_ <- ifelse(ed_attendances_by_mode_site_measure_clean$town == 'Hemel Hempstead', yes = 1, no = 0)
regressors.hemel <- filter(ed_attendances_by_mode_site_measure_clean,
                            town %in% c('Hemel Hempstead', 'Warwick')) %>%
                     dplyr::select(town_, closure, season, relative.month, nhs111, other.centre, ambulance.divert) %>% as.data.frame()
regressors.hemel$season <- as.numeric(regressors.hemel$season)
tsglm.hemel.model6.2 <- tsglm(ts    = ts.hemel,
                                 link  = 'identity',
                                 model = list(past_obs = 1),
                                 xreg = regressors.hemel,
                                 distr = 'nbinom')
## Newark
ts.newark <- filter(ed_attendances_by_mode_site_measure_clean,
                    town %in% c('Newark', 'Southport')) %>% as.data.frame() %>% .[,'value']
ed_attendances_by_mode_site_measure_clean$town_ <- ifelse(ed_attendances_by_mode_site_measure_clean$town == 'Newark', yes = 1, no = 0)
regressors.newark <- filter(ed_attendances_by_mode_site_measure_clean,
                            town %in% c('Newark', 'Southport')) %>%
                     dplyr::select(town_, closure, season, relative.month, nhs111, other.centre, ambulance.divert) %>% as.data.frame()
regressors.newark$season <- as.numeric(regressors.newark$season)
tsglm.newark.model6.2 <- tsglm(ts    = ts.newark,
                                 link  = 'identity',
                                 model = list(past_obs = 1),
                                 xreg = regressors.newark,
                                 distr = 'nbinom')
## Rochdale
ts.rochdale <- filter(ed_attendances_by_mode_site_measure_clean,
                     town %in% c('Rochdale', 'Rotherham')) %>% as.data.frame() %>% .[,'value']
ed_attendances_by_mode_site_measure_clean$town_ <- ifelse(ed_attendances_by_mode_site_measure_clean$town == 'Rochdale', yes = 1, no = 0)
regressors.rochdale <- filter(ed_attendances_by_mode_site_measure_clean,
                            town %in% c('Rochdale', 'Rotherham')) %>%
                     dplyr::select(town_, closure, season, relative.month, nhs111, other.centre, ambulance.divert) %>% as.data.frame()
regressors.rochdale$season <- as.numeric(regressors.rochdale$season)
tsglm.rochdale.model6.2 <- tsglm(ts    = ts.rochdale,
                                 link  = 'identity',
                                 model = list(past_obs = 1),
                                 xreg = regressors.rochdale,
                                 distr = 'nbinom')



## 2016-08-18 Quick check of code for producing different plots, which are being moved out of
##            calls to closed_models()

## Model 1
load('~/work/closed/tmp/ed attendances by mode measure - site - 2016-05-24 14.26.Rda')
ed_attendances_by_mode_site_measure       <- closed_tidy(ed_attendances_by_mode_site_measure)
ed_attendances_by_mode_site_measure_clean <- closed_clean(ed_attendances_by_mode_site_measure)
## Set variables
measure <- 'ed attendances'
sub.measure <- 'any'
## Set sites to plot
sites <- c('Bishop Auckland', 'Hartlepool', 'Hemel Hempstead', 'Newark', 'Rochdale')
## Filter
to.plot <- filter(ed_attendances_by_mode_site_measure_clean,
                  town %in% sites &
                  measure == measure,
                  sub.measure == sub.measure)
## Plot
closed_ts_plot(df            = to.plot,
               sites         = sites,
               indicator     = measure,
               sub.indicator = sub.measure,
               steps         = TRUE,
               lines         = TRUE,
               xaxis.steps   = FALSE,
               join.line     = TRUE,
               legend        = FALSE,
               facet         = FALSE)

## Model 2
## Set sites to plot
sites <- c('Bishop Auckland', 'Whitehaven',
                   'Hartlepool', 'Grimsby',
                   'Hemel Hempstead', 'Warwick',
                   'Newark', 'Southport',
                   'Rochdale', 'Rotherham')
## Filter
to.plot <- filter(ed_attendances_by_mode_site_measure_clean,
                  town %in% sites &
                  measure == measure,
                  sub.measure == sub.measure)
## Plot
closed_ts_plot(df            = to.plot,
               sites         = sites,
               indicator     = measure,
               sub.indicator = sub.measure,
               steps         = TRUE,
               lines         = TRUE,
               xaxis.steps   = FALSE,
               join.line     = TRUE,
               legend        = TRUE,
               facet         = TRUE)

## Model 3.1
## Set sites to plot
sites <- c('Basingstoke', 'Bishop Auckland', 'Blackburn', 'Carlisle', 'Grimsby', 'Hartlepool', 'Hemel Hempstead', 'Newark', 'Rochdale', 'Rotherham', 'Salford', 'Salisbury', 'Scarborough', 'Scunthorpe', 'Southport', 'Wansbeck', 'Warwick', 'Whitehaven', 'Wigan', 'Yeovil')
## Filter
to.plot <- filter(ed_attendances_by_mode_site_measure_clean,
                  town %in% sites &
                  measure == measure,
                  sub.measure == sub.measure)
## Plot
closed_ts_plot(df            = to.plot,
               sites         = sites,
               indicator     = measure,
               sub.indicator = sub.measure,
               steps         = TRUE,
               lines         = TRUE,
               xaxis.steps   = FALSE,
               join.line     = TRUE,
               legend        = TRUE,
               facet         = TRUE)

## Model 3.2
## Set sites to plot
sites <- c('Bishop Auckland', 'Hartlepool', 'Hemel Hempstead', 'Newark', 'Rochdale', 'Control')
## Filter
to.plot <- closed_pool(df = ed_attendances_by_mode_site_measure,
                       within.centres = TRUE) # %>% closed_clean()
## Plot
closed_ts_plot(df            = to.plot,
               sites         = sites,
               indicator     = measure,
               sub.indicator = sub.measure,
               steps         = TRUE,
               lines         = TRUE,
               xaxis.steps   = FALSE,
               join.line     = TRUE,
               legend        = TRUE,
               facet         = TRUE)

## Model 4
sites <- c('Bishop Auckland', 'Whitehaven',
           'Hartlepool', 'Grimsby',
           'Hemel Hempstead', 'Warwick',
           'Newark', 'Southport',
           'Rochdale', 'Rotherham')
## Filter
to.plot <- filter(ed_attendances_by_mode_site_measure_clean,
                  town %in% sites &
                  measure == measure,
                  sub.measure == sub.measure)
## Plot
closed_ts_plot(df            = to.plot,
               sites         = sites,
               indicator     = measure,
               sub.indicator = sub.measure,
               steps         = TRUE,
               lines         = TRUE,
               xaxis.steps   = FALSE,
               join.line     = TRUE,
               legend        = TRUE,
               facet         = TRUE)
## Model 5
sites <- c('Basingstoke', 'Bishop Auckland', 'Blackburn', 'Carlisle', 'Grimsby', 'Hartlepool', 'Hemel Hempstead', 'Newark', 'Rochdale', 'Rotherham', 'Salford', 'Salisbury', 'Scarborough', 'Scunthorpe', 'Southport', 'Wansbeck', 'Warwick', 'Whitehaven', 'Wigan', 'Yeovil')
## Filter
to.plot <- filter(ed_attendances_by_mode_site_measure_clean,
                  town %in% sites &
                  measure == measure,
                  sub.measure == sub.measure)
## Plot
closed_ts_plot(df            = to.plot,
               sites         = sites,
               indicator     = measure,
               sub.indicator = sub.measure,
               steps         = TRUE,
               lines         = TRUE,
               xaxis.steps   = FALSE,
               join.line     = TRUE,
               legend        = TRUE,
               facet         = TRUE)

## Model 6.1
sites <- c('Bishop Auckland',
                  'Hartlepool',
                  'Hemel Hempstead',
                  'Newark',
                  'Rochdale')
## Filter
to.plot <- filter(ed_attendances_by_mode_site_measure_clean,
                  town %in% sites &
                  measure == measure,
                  sub.measure == sub.measure)
## Plot
closed_ts_plot(df            = to.plot,
               sites         = sites,
               indicator     = measure,
               sub.indicator = sub.measure,
               steps         = TRUE,
               lines         = TRUE,
               xaxis.steps   = FALSE,
               join.line     = TRUE,
               legend        = TRUE,
               facet         = TRUE)

## Model 6.2
sites <- c('Bishop Auckland', 'Whitehaven',
                   'Hartlepool', 'Grimsby',
                   'Hemel Hempstead', 'Warwick',
                   'Newark', 'Southport',
                   'Rochdale', 'Rotherham')
## Filter
to.plot <- filter(ed_attendances_by_mode_site_measure_clean,
                  town %in% sites &
                  measure == measure,
                  sub.measure == sub.measure)
## Plot
closed_ts_plot(df            = to.plot,
               sites         = sites,
               indicator     = measure,
               sub.indicator = sub.measure,
               steps         = TRUE,
               lines         = TRUE,
               xaxis.steps   = FALSE,
               join.line     = TRUE,
               legend        = TRUE,
               facet         = TRUE)


## 2016-08-17 Why have closed_ts_plots() made internal to closed_models() stopped working for models >= 3.1
##
##            Most likely something I've changed with regards to the default options of closed_ts_model()
##            but need to work out what then explicity set it within closed)

mode.of.arrival.any <- closed_models(df.lsoa          = ed_attendances_by_mode_measure,
                                     df.trust         = ed_attendances_by_mode_site_measure,
                                     indicator        = 'ed attendances',
                                     sub.indicator    = 'any',
                                     steps            = c('closure'),
                                     fit.with         = model.opts$fit.with,
                                     panel.lsoa       = model.opts$panel.lsoa,
                                     panel.trust      = model.opts$panel.trust,
                                     timevar          = model.opts$timevar,
                                     outcome          = model.opts$outcome,
                                     model1           = model.opts$mod1,
                                     model2           = model.opts$mod2,
                                     model3.1         = model.opts$mod3.1,
                                     model3.2         = model.opts$mod3.2,
                                     model4           = model.opts$mod4,
                                     model5           = model.opts$mod5,
                                     model6.1         = model.opts$mod6.1,
                                     model6.2         = model.opts$mod6.2,
                                     model7           = model.opts$mod7,
                                     autocorr         = model.opts$autocorr,
                                     panelcorrmethod  = model.opts$panelcorrmethod,
                                     coefficients     = model.opts$coefficients,
                                     seq.times        = model.opts$seq.times,
                                     rho.na.rm        = model.opts$rho.na.rm,
                                     plot             = model.opts$plot,
                                     common.y         = model.opts$common.y,
                                     theme            = model.opts$theme,
                                     ## return.df        = model.opts$return.df,
                                     return.df        = TRUE,
                                     return.model     = model.opts$return.model,
                                     return.residuals = model.opts$return.residuals,
                                     join.line        = model.opts$join.line,
                                     legend           = model.opts$legend)
mode.of.arrival.any$model1.ts.plot
mode.of.arrival.any$model2.ts.plot
mode.of.arrival.any$model3.1.ts.plot
mode.of.arrival.any$model3.2.ts.plot
mode.of.arrival.any$model4.ts.plot
mode.of.arrival.any$model5.ts.plot
mode.of.arrival.any$model6.1.ts.plot
mode.of.arrival.any$model6.2.ts.plot
mode.of.arrival.any$model7.ts.plot


## 2016-08-12 Finishing off black and white graphs
load('~/work/closed/tmp/ed attendances by mode measure - site - 2016-05-24 14.26.Rda')
ed_attendances_by_mode_site_measure <- closed_tidy(ed_attendances_by_mode_site_measure)

closed_ts_plot(df            = ed_attendances_by_mode_site_measure,
               indicator     = 'ed attendances',
               sub.indicator = 'any',
               steps         = TRUE,
               theme         = theme_bw(),
               tidy          = FALSE,
               facet         = FALSE,
               sites         = c('Bishop Auckland', 'Hartlepool', 'Hemel Hempstead', 'Newark', 'Rochdale'),
               legend        = TRUE,
               colour        = FALSE,
               lines         = FALSE,
               xaxis.steps   = TRUE)

closed_ts_plot(df            = ed_attendances_by_mode_site_measure,
               indicator     = 'ed attendances',
               sub.indicator = 'any',
               steps         = TRUE,
               theme         = theme_bw(),
               tidy          = FALSE,
               facet         = FALSE,
               sites         = c('Bishop Auckland', 'Salford', 'Scarborough', 'Whitehaven'),
               legend        = TRUE)



## 2016-08-11 Checking ambulance outcomes with new data
load('~/work/closed/lib/data/ambulance mean times measure - lsoa - 2016-08-01 17.14.Rda')
load('~/work/closed/lib/data/ambulance mean times measure - site - 2016-08-01 17.14.Rda')
amb_mean_times_measure                  <- closed_tidy(amb_mean_times_measure)
amb_mean_times_site_measure             <- closed_tidy(amb_mean_times_site_measure)

## Complains about lack of contrasts when attempting to analyse Hartlepool
check <- filter(amb_mean_times_site_measure,
                group == 'University Hospital of Hartlepool' &
                sub.measure == 'call_to_dest')

ambulance.mean.times.call.to.dest <- closed_models(df.lsoa         = amb_mean_times_measure,
                                     df.trust         = amb_mean_times_site_measure,
                                     indicator        = 'ambulance mean times',
                                     sub.indicator    = 'call_to_dest',
                                     steps            = c('closure'),
                                     fit.with         = model.opts$fit.with,
                                     panel.lsoa       = model.opts$panel.lsoa,
                                     panel.trust      = model.opts$panel.trust,
                                     timevar          = model.opts$timevar,
                                     outcome          = model.opts$outcome,
                                     model1           = model.opts$mod1,
                                     model2           = model.opts$mod2,
                                     model3.1          = model.opts$mod3.1,
                                     model3.2          = model.opts$mod3.2,
                                     model4           = model.opts$mod4,
                                     model5           = model.opts$mod5,
                                     model6.1         = model.opts$mod6.1,
                                     model6.2         = model.opts$mod6.2,
                                     model7           = model.opts$mod7,
                                     autocorr         = model.opts$autocorr,
                                     panelcorrmethod  = model.opts$panelcorrmethod,
                                     coefficients     = model.opts$coefficients,
                                     seq.times        = model.opts$seq.times,
                                     rho.na.rm        = model.opts$rho.na.rm,
                                     plot             = model.opts$plot,
                                     common.y         = model.opts$common.y,
                                     theme            = model.opts$theme,
                                     return.df        = model.opts$return.df,
                                     return.model     = model.opts$return.model,
                                     return.residuals = model.opts$return.residuals,
                                     join.line        = model.opts$join.line,
                                     legend           = model.opts$legend)

load('~/work/closed/lib/data/ambulance non-conveyance measure - lsoa - 2016-08-01 17.16.Rda')
load('~/work/closed/lib/data/ambulance non-conveyance measure - site - 2016-08-01 17.16.Rda')
amb_non_conveyance_measure              <- closed_tidy(amb_non_conveyance_measure)
amb_non_conveyance_site_measure         <- closed_tidy(amb_non_conveyance_site_measure)
ambulance.non.conveyances.green.calls <- closed_models(df.lsoa         = amb_non_conveyance_measure,
                                     df.trust         = amb_non_conveyance_site_measure,
                                     indicator        = 'ambulance non-conveyance',
                                     sub.indicator    = 'green calls',
                                     steps            = c('closure'),
                                     fit.with         = model.opts$fit.with,
                                     panel.lsoa       = model.opts$panel.lsoa,
                                     panel.trust      = model.opts$panel.trust,
                                     timevar          = model.opts$timevar,
                                     outcome          = model.opts$outcome,
                                     model1           = model.opts$mod1,
                                     model2           = model.opts$mod2,
                                     model3.1          = model.opts$mod3.1,
                                     model3.2          = model.opts$mod3.2,
                                     model4           = model.opts$mod4,
                                     model5           = model.opts$mod5,
                                     model6.1         = model.opts$mod6.1,
                                     model6.2         = model.opts$mod6.2,
                                     model7           = model.opts$mod7,
                                     autocorr         = model.opts$autocorr,
                                     panelcorrmethod  = model.opts$panelcorrmethod,
                                     coefficients     = model.opts$coefficients,
                                     seq.times        = model.opts$seq.times,
                                     rho.na.rm        = model.opts$rho.na.rm,
                                     plot             = model.opts$plot,
                                     common.y         = model.opts$common.y,
                                     theme            = model.opts$theme,
                                     return.df        = model.opts$return.df,
                                     return.model     = model.opts$return.model,
                                     return.residuals = model.opts$return.residuals,
                                     join.line        = model.opts$join.line,
                                     legend           = model.opts$legend)

load('~/work/closed/lib/data/ambulance red calls measure - lsoa - 2016-08-01 17.15.Rda')
load('~/work/closed/lib/data/ambulance red calls measure - site - 2016-08-01 17.15.Rda')
amb_red_calls_measure                   <- closed_tidy(amb_red_calls_measure)
amb_red_calls_site_measure              <- closed_tidy(amb_red_calls_site_measure)
ambulance.red.calls.hospital.transfers <- closed_models(df.lsoa         = amb_red_calls_measure,
                                     df.trust         = amb_red_calls_site_measure,
                                     indicator        = 'ambulance red calls',
                                     sub.indicator    = 'hospital transfers',
                                     steps            = c('closure'),
                                     fit.with         = model.opts$fit.with,
                                     panel.lsoa       = model.opts$panel.lsoa,
                                     panel.trust      = model.opts$panel.trust,
                                     timevar          = model.opts$timevar,
                                     outcome          = model.opts$outcome,
                                     model1           = model.opts$mod1,
                                     model2           = model.opts$mod2,
                                     model3.1          = model.opts$mod3.1,
                                     model3.2          = model.opts$mod3.2,
                                     model4           = model.opts$mod4,
                                     model5           = model.opts$mod5,
                                     model6.1         = model.opts$mod6.1,
                                     model6.2         = model.opts$mod6.2,
                                     model7           = model.opts$mod7,
                                     autocorr         = model.opts$autocorr,
                                     panelcorrmethod  = model.opts$panelcorrmethod,
                                     coefficients     = model.opts$coefficients,
                                     seq.times        = model.opts$seq.times,
                                     rho.na.rm        = model.opts$rho.na.rm,
                                     plot             = model.opts$plot,
                                     common.y         = model.opts$common.y,
                                     theme            = model.opts$theme,
                                     return.df        = model.opts$return.df,
                                     return.model     = model.opts$return.model,
                                     return.residuals = model.opts$return.residuals,
                                     join.line        = model.opts$join.line,
                                     legend           = model.opts$legend)


## 2016-08-10 Developing requested summary of before/after means, sd etc. along with adjusted
##            model coefficients

mode.of.arrival.any <- closed_models(df.lsoa          = ed_attendances_by_mode_measure,
                                     df.trust         = ed_attendances_by_mode_site_measure,
                                     indicator        = 'ed attendances',
                                     sub.indicator    = 'any',
                                     steps            = c('closure'),
                                     fit.with         = model.opts$fit.with,
                                     panel.lsoa       = model.opts$panel.lsoa,
                                     panel.trust      = model.opts$panel.trust,
                                     timevar          = model.opts$timevar,
                                     outcome          = model.opts$outcome,
                                     model1           = model.opts$mod1,
                                     model2           = model.opts$mod2,
                                     model3.1         = model.opts$mod3.1,
                                     model3.2         = model.opts$mod3.2,
                                     model4           = model.opts$mod4,
                                     model5           = model.opts$mod5,
                                     model6.1         = model.opts$mod6.1,
                                     model6.2         = model.opts$mod6.2,
                                     model7           = model.opts$mod7,
                                     autocorr         = model.opts$autocorr,
                                     panelcorrmethod  = model.opts$panelcorrmethod,
                                     coefficients     = model.opts$coefficients,
                                     seq.times        = model.opts$seq.times,
                                     rho.na.rm        = model.opts$rho.na.rm,
                                     plot             = model.opts$plot,
                                     common.y         = model.opts$common.y,
                                     theme            = model.opts$theme,
                                     ## return.df        = model.opts$return.df,
                                     return.df        = TRUE,
                                     return.model     = model.opts$return.model,
                                     return.residuals = model.opts$return.residuals,
                                     join.line        = model.opts$join.line,
                                     legend           = model.opts$legend)

## 2016-08-10 Checking why model 6.1.panelar.all isn't populated with all coefficients
mode.of.arrival.any <- closed_models(df.lsoa          = ed_attendances_by_mode_measure,
                                     df.trust         = ed_attendances_by_mode_site_measure,
                                     indicator        = 'ed attendances',
                                     sub.indicator    = 'any',
                                     steps            = c('closure'),
                                     fit.with         = model.opts$fit.with,
                                     panel.lsoa       = model.opts$panel.lsoa,
                                     panel.trust      = model.opts$panel.trust,
                                     timevar          = model.opts$timevar,
                                     outcome          = model.opts$outcome,
                                     model1           = NULL,
                                     model2           = NULL,
                                     model3.1         = NULL,
                                     model3.2         = NULL,
                                     model4           = NULL,
                                     model5           = NULL,
                                     model6.1         = model.opts$mod6.1,
                                     model6.2         = NULL,
                                     model7           = NULL,
                                     autocorr         = model.opts$autocorr,
                                     panelcorrmethod  = model.opts$panelcorrmethod,
                                     coefficients     = model.opts$coefficients,
                                     seq.times        = model.opts$seq.times,
                                     rho.na.rm        = model.opts$rho.na.rm,
                                     plot             = model.opts$plot,
                                     common.y         = model.opts$common.y,
                                     theme            = model.opts$theme,
                                     ## return.df        = model.opts$return.df,
                                     return.df        = TRUE,
                                     return.model     = model.opts$return.model,
                                     return.residuals = model.opts$return.residuals,
                                     join.line        = model.opts$join.line,
                                     legend           = model.opts$legend)

## 2016-07-22 Correcting problem in closed_ts_plot()
check.plot <- closed_ts_plot(df            = ed_attendances_by_mode_site_measure,
                             indicator     = 'ed attendances',
                             sub.indicator = 'any',
                             steps         = TRUE,
                             theme         = theme_bw(),
                             tidy          = TRUE,
                             facet         = TRUE,
                             sites         = c('Bishop Auckland', 'Whitehaven',
                                               'Hartlepool', 'Grimsby',
                                               'Hemel Hempstead', 'Warwick',
                                               'Newark', 'Southport',
                                               'Rochdale', 'Rotherham'),
                             legend        = TRUE,
                             pool.control  = FALSE)

mode.of.arrival.any <- closed_models(df.lsoa          = ed_attendances_by_mode_measure,
                                     df.trust         = ed_attendances_by_mode_site_measure,
                                     indicator        = 'ed attendances',
                                     sub.indicator    = 'any',
                                     steps            = c('closure'),
                                     fit.with         = model.opts$fit.with,
                                     panel.lsoa       = model.opts$panel.lsoa,
                                     panel.trust      = model.opts$panel.trust,
                                     timevar          = model.opts$timevar,
                                     outcome          = model.opts$outcome,
                                     model1           = model.opts$mod1,
                                     model2           = model.opts$mod2,
                                     model3a          = model.opts$mod3a,
                                     model3b          = model.opts$mod3b,
                                     model4           = model.opts$mod4,
                                     model5           = model.opts$mod5,
                                     model6           = model.opts$mod6,
                                     model6.1         = model.opts$mod6.1,
                                     model6.2         = model.opts$mod6.2,
                                     model7           = model.opts$mod7,
                                     autocorr         = model.opts$autocorr,
                                     panelcorrmethod  = model.opts$panelcorrmethod,
                                     coefficients     = model.opts$coefficients,
                                     seq.times        = model.opts$seq.times,
                                     rho.na.rm        = model.opts$rho.na.rm,
                                     plot             = model.opts$plot,
                                     common.y         = model.opts$common.y,
                                     theme            = model.opts$theme,
                                     ## return.df        = model.opts$return.df,
                                     return.df        = TRUE,
                                     return.model     = model.opts$return.model,
                                     return.residuals = model.opts$return.residuals,
                                     join.line        = model.opts$join.line,
                                     legend           = model.opts$legend)


## 2016-07-22 Testing models 6.1 and 6.2
check.models <- closed_models(df.lsoa          = ed_attendances_by_mode_measure,
                              df.trust         = ed_attendances_by_mode_site_measure,
                              indicator        = 'ed attendances',
                              sub.indicator    = 'any',
                              steps            = c('closure'),
                              fit.with         = model.opts$fit.with,
                              panel.lsoa       = model.opts$panel.lsoa,
                              panel.trust      = model.opts$panel.trust,
                              timevar          = model.opts$timevar,
                              outcome          = model.opts$outcome,
                              ## model1           = c('closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
                              ## model2           = c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
                              ## model3           = c('pooled.control * closure', 'town', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
                              ## model4           = c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
                              ## model5           = c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
                              ## model6           = c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert', 'diff.time.to.ed'),
                              model6.1         = c('season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert', 'diff.time.to.ed'),
                              model6.2         = c('town', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert', 'diff.time.to.ed'),
                              ## model7           = c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert', 'diff.time.to.ed'),
                              ## model8           = c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
                              model1           = NULL,
                              model2           = NULL,
                              model3a          = NULL,
                              model3b          = NULL,
                              model4           = NULL,
                              model5           = NULL,
                              model6           = NULL,
                              model7           = NULL,
                              ## model8           = NULL,
                              autocorr         = model.opts$autocorr,
                              panelcorrmethod  = model.opts$panelcorrmethod,
                              coefficients     = model.opts$coefficients,
                              seq.times        = model.opts$seq.times,
                              rho.na.rm        = model.opts$rho.na.rm,
                              plot             = model.opts$plot,
                              common.y         = model.opts$common.y,
                              theme            = model.opts$theme,
                              return.df        = TRUE,
                              return.model     = model.opts$return.model,
                              return.residuals = model.opts$return.residuals,
                              return.residuals.plot = FALSE,
                              join.line        = model.opts$join.line,
                              legend           = model.opts$legend)
save(check.models,
     file = '~/work/closed/tmp/check.RData')


## 2016-07-13 Function not working, lets try manually
formula.model8 <- reformulate(response = model.opts$outcome,
                      termlabels = model.opts$mod8)
## Pool the data
df8 <- closed_pool(df             = ed_attendances_by_mode_site_measure,
                   within.centres = TRUE)
df8$group <- paste0('Cohort : ', df8$group)
## Run the model
df8 <- filter(df8,
       group == 'Cohort : Bishop Auckland General Hospital') %>% as.data.frame()
panelAR(data     = df8,
        formula  = formula.model8,
        timeVar  = model.opts$timevar,
        panelVar = model.opts$panel.trust,
        autoCorr = model.opts$autocorr,
        panelCorrMethod = 'pcse',
        seq.times = model.opts$seq.times,
        rho.na.rm = model.opts$rho.na.rm)

## 2016-07-13 Testing pooling for Model 8 models (NOT CURRENTLY WORKING) and plots (ONLY CURRENTLY
##            GET CASE SITES)
check.models <- closed_models(df.lsoa          = ed_attendances_by_mode_measure,
                              df.trust         = ed_attendances_by_mode_site_measure,
                              indicator        = 'ed attendances',
                              sub.indicator    = 'any',
                              steps            = c('closure'),
                              fit.with         = model.opts$fit.with,
                              panel.lsoa       = model.opts$panel.lsoa,
                              panel.trust      = model.opts$panel.trust,
                              timevar          = model.opts$timevar,
                              outcome          = model.opts$outcome,
                              ## model1           = c('closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
                              ## model2           = c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
                              model3           = c('pooled.control * closure', 'town', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
                              ## model4           = c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
                              ## model5           = c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
                              ## model6           = c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert', 'diff.time.to.ed'),
                              ## model7           = c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert', 'diff.time.to.ed'),
                              model8           = c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
                              model1           = NULL,
                              model2           = NULL,
                              ## model3           = NULL,
                              model4           = NULL,
                              model5           = NULL,
                              model6           = NULL,
                              model7           = NULL,
                              ## model8           = NULL,
                              autocorr         = model.opts$autocorr,
                              panelcorrmethod  = model.opts$panelcorrmethod,
                              coefficients     = model.opts$coefficients,
                              seq.times        = model.opts$seq.times,
                              rho.na.rm        = model.opts$rho.na.rm,
                              plot             = model.opts$plot,
                              common.y         = model.opts$common.y,
                              theme            = model.opts$theme,
                              return.df        = TRUE,
                              return.model     = model.opts$return.model,
                              return.residuals = model.opts$return.residuals,
                              return.residuals.plot = FALSE,
                              join.line        = model.opts$join.line,
                              legend           = model.opts$legend)
save(check.models,
     file = '~/work/closed/tmp/check.RData')

## 2016-07-13 Testing pooling for plots, which is now working \o/
sites <- c('Bishop Auckland', 'Control')
check.plots.bishop <- closed_pool(ed_attendances_by_mode_site_measure, within.centre = TRUE) %>%
                      filter(group == 'Bishop Auckland General Hospital' ) %>%
                      closed_ts_plot(sites         = sites,
                              indicator     = 'ed attendances',
                              sub.indicator = 'any',
                              steps         = TRUE,
                              facet         = FALSE,
                              tidy          = TRUE,
                              join          = model.opts$join.line,
                              legend        = model.opts$legend,
                              lines         = TRUE,
                              exclude.control = TRUE)
sites <- c('Bishop Auckland', 'Hartlepool', 'Hemel Hempstead', 'Newark', 'Rochdale', 'Control')
check.plots.all <- closed_pool(ed_attendances_by_mode_site_measure, within.centre = TRUE) %>%
                   closed_ts_plot(sites         = sites,
                              indicator     = 'ed attendances',
                              sub.indicator = 'any',
                              steps         = TRUE,
                              facet         = TRUE,
                              tidy          = TRUE,
                              join          = model.opts$join.line,
                              legend        = model.opts$legend,
                              lines         = TRUE,
                              exclude.control = TRUE)
save(check.plots.bishop, check.plots.all,
     file = '~/work/closed/tmp/check.RData')

## 2016-07-13 Checking pooling funciton which has been written to replace internal code in
##            closed_models() and closed_ts_plots()
t <- as.data.frame(ed_attendances_by_mode_site_measure)
names(t) <- gsub('_', '.', names(t))
check.within <- closed_pool(df            = t,
                     within.centre = TRUE)
check.across <- closed_pool(df            = t,
                     within.centre = FALSE)
table(check.within$town, check.within$group)
table(check.across$town)

## 2016-07-13 Fixing Faceting of pooled plots
## Pool control sites
pool <- function(x                    = df.trust,
                 remove.control.steps = FALSE){
    x <- mutate(x,
                pooled.control = ifelse(site.type %in% c('matched control', 'pooled control'), 'Control', town))
    x <- mutate(x,
                pooled.control = ifelse(pooled.control == 2, 'Bishop Auckland', pooled.control),
                pooled.control = ifelse(pooled.control == 6, 'Harltepool', pooled.control),
                pooled.control = ifelse(pooled.control == 7, 'Hemel Hempstead', pooled.control),
                pooled.control = ifelse(pooled.control == 8, 'Newark', pooled.control),
                pooled.control = ifelse(pooled.control == 9, 'Rochdale', pooled.control))
    x$pooled.control <- factor(x$pooled.control)
    ## Set reference group for pooled controls
    x$pooled.control <- relevel(x$pooled.control, ref = 'Control')
    ## Optionally remove steps from controls since they are now pooled
        if(remove.control.steps == TRUE){
            control <- c('Basingstoke', 'Blackburn', 'Carlisle', 'Grimsby', 'Rotherham', 'Salford', 'Salisbury', 'Scarborough', 'Scunthorpe', 'Southport', 'WansbeckWarwick', 'Whitehaven', 'Wigan', 'Yeovil')
            x <- mutate(x,
                        nhs111 = ifelse(town %in% control, 0, nhs111),
                        other.centre = ifelse(town %in% control, 0, other.centre),
                        ambulance.divert = ifelse(town %in% control, 0, ambulance.divert))
        }
    ## Return modified data frame
    return(x)
}
t <- as.data.frame(ed_attendances_by_mode_site_measure)
names(t) <- gsub('_', '.', names(t))
## Add in dummies
t <- mutate(t,
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
                                  1, 0),
            closure = ifelse(relative.month > 24, 1, 0))
t$season <- 1
t <- within(t,{
    season[month(yearmonth) == 1  | month(yearmonth) == 2]  <- 1
    season[month(yearmonth) == 3  | month(yearmonth) == 4]  <- 2
    season[month(yearmonth) == 5  | month(yearmonth) == 6]  <- 3
    season[month(yearmonth) == 7  | month(yearmonth) == 8]  <- 4
    season[month(yearmonth) == 9  | month(yearmonth) == 10] <- 5
    season[month(yearmonth) == 11 | month(yearmonth) == 12] <- 6
})
t <- filter(t, sub.measure == 'any') %>%
     pool(x                    = ,
          remove.control.steps = TRUE)
t$group <- paste0('Cohort : ', t$group)
sites <- c('Basingstoke', 'Bishop Auckland', 'Blackburn', 'Carlisle', 'Grimsby', 'Hartlepool', 'Hemel Hempstead', 'Newark', 'Rochdale', 'Rotherham', 'Salford', 'Salisbury', 'Scarborough', 'Scunthorpe', 'Southport', 'Wansbeck', 'Warwick', 'Whitehaven', 'Wigan', 'Yeovil')
pooled.plot <- closed_ts_plot(df            = t,
                              sites         = sites,
                              indicator     = 'ed attendances',
                              sub.indicator = 'any',
                              steps         = TRUE,
                              facet         = FALSE,
                              tidy          = TRUE,
                              join          = model.opts$join.line,
                              legend        = model.opts$legend,
                              pool.control  = TRUE,
                              lines         = FALSE)
save(pooled.plot,
     file = '~/work/closed/tmp/check.RData')
rm(pool, t)

## 2016-07-12 Ambulance Mean Times - only running when there is data.
## Model didn't work, lets look at the data in isolation
## Subset out Bishop Auckland
t <- filter(as.data.frame(amb_mean_times_site_measure),
            town == 'Bishop Auckland',
            measure == 'ambulance mean times',
            sub_measure == 'call_to_dest')
names(t) <- gsub('_', '.', names(t))
## Add in dummies
t <- mutate(t,
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
                                  1, 0),
            closure = ifelse(relative.month > 24, 1, 0))
t$season <- 1
t <- within(t,{
    season[month(yearmonth) == 1  | month(yearmonth) == 2]  <- 1
    season[month(yearmonth) == 3  | month(yearmonth) == 4]  <- 2
    season[month(yearmonth) == 5  | month(yearmonth) == 6]  <- 3
    season[month(yearmonth) == 7  | month(yearmonth) == 8]  <- 4
    season[month(yearmonth) == 9  | month(yearmonth) == 10] <- 5
    season[month(yearmonth) == 11 | month(yearmonth) == 12] <- 6
})
model1 <- reformulate(response = model.opts$outcome,
                      termlabels = model.opts$mod1)
panelAR(data = t,
        formula = model1,
        timeVar          = model.opts$timevar,
        panelVar         = model.opts$panel.trust,
        autoCorr         = model.opts$autocorr,
        panelCorrMethod  = 'pcse',
        seq.times        = model.opts$seq.times,
        rho.na.rm        = model.opts$rho.na.rm)

## 2016-07-12 Testing closed_model() to accommodate incomplete ambulance data
check.models <- closed_models(df.lsoa         = amb_mean_times_measure,
                              df.trust         = amb_mean_times_site_measure,
                              indicator        = 'ambulance mean times',
                              sub.indicator    = 'call_to_dest',
                              steps            = c('closure'),
                              fit.with         = model.opts$fit.with,
                              panel.lsoa       = model.opts$panel.lsoa,
                              panel.trust      = model.opts$panel.trust,
                              timevar          = model.opts$timevar,
                              outcome          = model.opts$outcome,
                              model1           = c('closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
                              ## model2           = c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
                              ## model3           = c('pooled.control * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
                              ## model4           = c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
                              ## model5           = c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
                              ## model6           = c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert', 'diff.time.to.ed'),
                              ## model7           = c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert', 'diff.time.to.ed'),
                              ## model8           = c('pooled.control * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
                              ## model1           = NULL,
                              model2           = NULL,
                              model3           = NULL,
                              model4           = NULL,
                              model5           = NULL,
                              model6           = NULL,
                              model7           = NULL,
                              model8           = NULL,
                              autocorr         = model.opts$autocorr,
                              panelcorrmethod  = model.opts$panelcorrmethod,
                              coefficients     = model.opts$coefficients,
                              seq.times        = model.opts$seq.times,
                              rho.na.rm        = model.opts$rho.na.rm,
                              plot             = model.opts$plot,
                              common.y         = model.opts$common.y,
                              theme            = model.opts$theme,
                              return.df        = TRUE,
                              return.model     = model.opts$return.model,
                              return.residuals = model.opts$return.residuals,
                              return.residuals.plot = FALSE,
                              join.line        = model.opts$join.line,
                              legend           = model.opts$legend)
save(check.models,
     file = '~/work/closed/tmp/check.RData')


## 2016-07-12 Trouble shooting Time-Series plots in models 4-8
check.models <- closed_models(df.lsoa         = ed_attendances_by_mode_measure,
                              df.trust         = ed_attendances_by_mode_site_measure,
                              indicator        = 'ed attendances',
                              sub.indicator    = 'other',
                              steps            = c('closure'),
                              fit.with         = model.opts$fit.with,
                              panel.lsoa       = model.opts$panel.lsoa,
                              panel.trust      = model.opts$panel.trust,
                              timevar          = model.opts$timevar,
                              outcome          = model.opts$outcome,
                              model1           = c('closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
                              model2           = c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
                              model3           = c('pooled.control * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
                              model4           = c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
                              model5           = c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
                              model6           = c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert', 'diff.time.to.ed'),
                              model7           = c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert', 'diff.time.to.ed'),
                              model8           = c('pooled.control * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
                              ## model1           = NULL,
                              ## model2           = NULL,
                              ## model3           = NULL,
                              ## model4           = NULL,
                              ## model5           = NULL,
                              ## model6           = NULL,
                              ## model7           = NULL,
                              ## model8           = NULL,
                              autocorr         = model.opts$autocorr,
                              panelcorrmethod  = model.opts$panelcorrmethod,
                              coefficients     = model.opts$coefficients,
                              seq.times        = model.opts$seq.times,
                              rho.na.rm        = model.opts$rho.na.rm,
                              plot             = model.opts$plot,
                              common.y         = model.opts$common.y,
                              theme            = model.opts$theme,
                              return.df        = TRUE,
                              return.model     = model.opts$return.model,
                              return.residuals = model.opts$return.residuals,
                              return.residuals.plot = FALSE,
                              join.line        = model.opts$join.line,
                              legend           = model.opts$legend)
save(check.models,
     file = '~/work/closed/tmp/check.RData')

## 2016-07-12 Trouble shooting Time Series plots for Unnecessary ED Attendance
unnecessary.ts.plot <- closed_ts_plot(df            = unnecessary_ed_attendances_site_measure,
                                      indicator     = 'unnecessary ed attendances',
                                      sub.indicator = 'all',
                                      steps         = TRUE,
                                      theme         = theme_bw(),
                                      tidy          = FALSE,
                                      facet         = FALSE,
                                      sites         = c('Bishop Auckland', 'Hartlepool', 'Hemel Hempstead', 'Newark', 'Rochdale'),
                                      legend        = TRUE)
save(unnecessary.ts.plot,
     file = '~/work/closed/tmp/check.RData')


## 2016-07-11 Testing systematic removal of spurious data points using closed_clean()
t <- as.data.frame(ed_attendances_by_mode_site_measure)
names(t) <- gsub("_", ".", names(t))
ed.attendance.any.unbalanced <- closed_clean(df      = t,
                                       indicator     = 'ed attendances',
                                       sub.indicator = 'any',
                                       systematic    = 2,
                                       balance       = FALSE)
## Check ED Attendances Any
filter(ed.attendance.any.unbalanced, sub.measure == 'any' & is.na(value))
## Check ED Attendances Ambulance
filter(ed.attendance.any.unbalanced, sub.measure == 'ambulance' & is.na(value))
## Check ED Attendances Other
filter(ed.attendance.any.unbalanced, sub.measure == 'other' & is.na(value))

## 2016-07-11 Testing development of closed_clean() which converts to NA spurious data points in
##            either a balanced or unbalanced manner.
t <- as.data.frame(ed_attendances_by_mode_site_measure)
names(t) <- gsub("_", ".", names(t))
ed.attendance.any.unbalanced <- closed_clean(df            = t,
                                       indicator     = 'ed attendances',
                                       sub.indicator = 'any',
                                       balance       = FALSE)
filter(ed.attendance.any.unbalanced, sub.measure == 'any' & is.na(value)) %>% arrange(town, relative.month)
ed.attendance.any.balanced <- closed_clean(df            = t,
                                      indicator     = 'ed attendances',
                                      sub.indicator = 'any',
                                      balance       = TRUE)
filter(ed.attendance.any.balanced, sub.measure == 'any' & is.na(value)) %>% arrange(town, relative.month)
save(ed.attendance.any.unbalanced,
     ed.attendance.any.balanced
     file = '~/work/closed/tmp/check.RData')

## 2016-07-11 Checking closed_models() works correctly with internal function pool() that derives
##            pooling and changes the step dummys for control sites that are pooled.
check.model8 <- closed_models(df.lsoa         = ed_attendances_by_mode_measure,
                              df.trust         = ed_attendances_by_mode_site_measure,
                              indicator        = 'ed attendances',
                              sub.indicator    = 'other',
                              steps            = c('closure'),
                              fit.with         = model.opts$fit.with,
                              panel.lsoa       = model.opts$panel.lsoa,
                              panel.trust      = model.opts$panel.trust,
                              timevar          = model.opts$timevar,
                              outcome          = model.opts$outcome,
                              model1           = c('closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
                              model2           = c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
                              model3           = c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
                              model4           = c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
                              model5           = c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
                              model6           = c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert', 'diff.time.to.ed'),
                              model7           = c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert', 'diff.time.to.ed'),
                              model8           = c('pooled.control * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
                              ## model1           = NULL,
                              ## model2           = NULL,
                              ## model3           = NULL,
                              ## model4           = NULL,
                              ## model5           = NULL,
                              ## model6           = NULL,
                              ## model7           = NULL,
                              ## model8           = NULL,
                              autocorr         = model.opts$autocorr,
                              panelcorrmethod  = model.opts$panelcorrmethod,
                              coefficients     = model.opts$coefficients,
                              seq.times        = model.opts$seq.times,
                              rho.na.rm        = model.opts$rho.na.rm,
                              plot             = model.opts$plot,
                              common.y         = model.opts$common.y,
                              theme            = model.opts$theme,
                              return.df        = TRUE,
                              return.model     = model.opts$return.model,
                              return.residuals = model.opts$return.residuals,
                              return.residuals.plot = FALSE,
                              join.line        = model.opts$join.line,
                              legend           = model.opts$legend)
save(check.model8,
     file = '~/work/closed/tmp/check.RData')

## 2016-07-06 Checking Model 8 which now performs pooled within centers as well as across all.
check.model8 <- check.model8 <- closed_models(df.lsoa         = ed_attendances_by_mode_measure,
                                     df.trust         = ed_attendances_by_mode_site_measure,
                                     indicator        = 'ed attendances',
                                     sub.indicator    = 'other',
                                     steps            = c('closure'),
                                     fit.with         = model.opts$fit.with,
                                     panel.lsoa       = model.opts$panel.lsoa,
                                     panel.trust      = model.opts$panel.trust,
                                     timevar          = model.opts$timevar,
                                     outcome          = model.opts$outcome,
                                     model1           = c('closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
                                     model2           = c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
                                     model3           = c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
                                     model4           = c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
                                     model5           = c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
                                     model6           = c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert', 'diff.time.to.ed'),
                                     model7           = c('town * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert', 'diff.time.to.ed'),
                                     model8           = c('pooled.control * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
                                     ## model1           = NULL,
                                     ## model2           = NULL,
                                     ## model3           = NULL,
                                     ## model4           = NULL,
                                     ## model5           = NULL,
                                     ## model6           = NULL,
                                     ## model7           = NULL,
                                     ## model8           = NULL,
                                     autocorr         = model.opts$autocorr,
                                     panelcorrmethod  = model.opts$panelcorrmethod,
                                     coefficients     = model.opts$coefficients,
                                     seq.times        = model.opts$seq.times,
                                     rho.na.rm        = model.opts$rho.na.rm,
                                     plot             = model.opts$plot,
                                     common.y         = model.opts$common.y,
                                     theme            = model.opts$theme,
                                     return.df        = TRUE,
                                     return.model     = model.opts$return.model,
                                     return.residuals = model.opts$return.residuals,
                                     return.residuals.plot = FALSE,
                                     join.line        = model.opts$join.line,
                                     legend           = model.opts$legend)
save(check.model8,
     file = '~/work/closed/tmp/check.RData')


## 2016-07-05 Checking Model 8
check.model8 <- closed_models(df.lsoa         = ed_attendances_by_mode_measure,
                                     df.trust         = ed_attendances_by_mode_site_measure,
                                     indicator        = 'ed attendances',
                                     sub.indicator    = 'other',
                                     steps            = c('closure'),
                                     fit.with         = model.opts$fit.with,
                                     panel.lsoa       = model.opts$panel.lsoa,
                                     panel.trust      = model.opts$panel.trust,
                                     timevar          = model.opts$timevar,
                                     outcome          = model.opts$outcome,
                                     model1           = NULL,
                                     model2           = NULL,
                                     model3           = NULL,
                                     model4           = NULL,
                                     model5           = NULL,
                                     model6           = NULL,
                                     model7           = NULL,
                                     model8           = c('pooled.control * closure', 'season', 'relative.month', 'nhs111', 'other.centre', 'ambulance.divert'),
                                     autocorr         = model.opts$autocorr,
                                     panelcorrmethod  = model.opts$panelcorrmethod,
                                     coefficients     = model.opts$coefficients,
                                     seq.times        = model.opts$seq.times,
                                     rho.na.rm        = model.opts$rho.na.rm,
                                     plot             = model.opts$plot,
                                     common.y         = model.opts$common.y,
                                     theme            = model.opts$theme,
                                     return.df        = TRUE,
                                     return.model     = model.opts$return.model,
                                     return.residuals = model.opts$return.residuals,
                                     join.line        = model.opts$join.line,
                                     legend           = model.opts$legend)

## 2016-07-05 Checking tweaked closed_ts_plot() to see if case fatality ratio plots now
##           have scale 0 to 1 by default...
ts.plot.cfr <- closed_ts_plot(df            = case_fatality_site_measure,
                          indicator     = 'case fatality ratio',
                          sub.indicator = 'any',
                          steps         = TRUE,
                          theme         = theme_bw(),
                          tidy          = FALSE,
                          facet         = FALSE,
                          sites         = c('Bishop Auckland', 'Hartlepool', 'Hemel Hempstead', 'Newark', 'Rochdale'),
                          legend        = TRUE)
## ...and to check new option of labelling x-axis with steps
ts.plot.xaxis <- closed_ts_plot(df            = case_fatality_site_measure,
                          indicator     = 'case fatality ratio',
                          sub.indicator = 'any',
                          steps         = TRUE,
                          theme         = theme_bw(),
                          tidy          = FALSE,
                          facet         = FALSE,
                          sites         = c('Bishop Auckland', 'Hartlepool', 'Hemel Hempstead', 'Newark', 'Rochdale'),
                          legend        = TRUE,
                          xaxis.steps   = TRUE)
## Hmm above isn't working, perhaps because the offset for labels is -1 which will be
## way off the page when the axis is 0 to 1, lets try another
ts.plot.xaxis2 <- closed_ts_plot(df            = ed_attendances_by_mode_site_measure,
                                 indicator     = 'ed attendances',
                                 sub.indicator = 'any',
                                 steps         = TRUE,
                                 theme         = theme_bw(),
                                 tidy          = FALSE,
                                 facet         = FALSE,
                                 sites         = c('Bishop Auckland', 'Hartlepool', 'Hemel Hempstead', 'Newark', 'Rochdale'),
                                 legend        = TRUE)
save(ts.plot.cfr,
     ts.plot.xaxis,
     ts.plot.xaxis2,
     file = '~/work/closed/tmp/check.RData')


## 2016-06-28 Troubleshooting closed_ts_model() function to correctly handle
##            'all emergency admissions' (without any sub_measure)
closed_ts_plot(df = emergency_admissions_site_measure,
               indicator = 'all emergency admissions',
               sub.indicator = NA)

##2016-06-21 - Checking Case Fatality analyses
case_fatality_site_measure %>%
    as.data.frame() %>%
    filter(sub_measure == 'any')
case.fatality.ratio.any <- closed_models(df.lsoa         = case_fatality_measure,
                                     df.trust         = case_fatality_site_measure,
                                     indicator        = 'case fatality ratio',
                                     sub.indicator    = 'any',
                                     steps            = c('closure'),
                                     fit.with         = model.opts$fit.with,
                                     panel.lsoa       = model.opts$panel.lsoa,
                                     panel.trust      = model.opts$panel.trust,
                                     timevar          = model.opts$timevar,
                                     outcome          = model.opts$outcome,
                                     model1           = model.opts$mod1,
                                     model2           = model.opts$mod2,
                                     model3           = model.opts$mod3,
                                     model4           = model.opts$mod4,
                                     model5           = model.opts$mod5,
                                     model6           = model.opts$mod6,
                                     model7           = model.opts$mod7,
                                     autocorr         = model.opts$autocorr,
                                     panelcorrmethod  = model.opts$panelcorrmethod,
                                     coefficients     = model.opts$coefficients,
                                     seq.times        = model.opts$seq.times,
                                     rho.na.rm        = TRUE,
                                     plot             = model.opts$plot,
                                     common.y         = model.opts$common.y,
                                     theme            = model.opts$theme,
                                     return.df        = model.opts$return.df,
                                     return.model     = model.opts$return.model,
				     return.residuals = model.opts$return.residuals,
                                     join.line        = model.opts$join.line)
save(case.fatality.ratio.any,
     file = '~/work/closed/tmp/case_fatality_ratio_any.RData')


## 2016-06-21 - For Jon to choose whether to have continuous or discontinuous lines
png(file = '~/work/closed/tmp/ts-unfiltered.png', width = 1024, height = 768)
closed_ts_plot(indicator = 'ed attendances',
               sub.indicator = 'any',
               steps = TRUE,
               theme = theme_bw(),
               tidy  = FALSE,
               join  = FALSE,
               facet = FALSE,
               sites = c('Bishop Auckland', 'Hartlepool', 'Hemel Hempstead', 'Newark', 'Rochdale'),)
dev.off()
png(file = '~/work/closed/tmp/ts-filtered-gap.png', width = 1024, height = 768)
closed_ts_plot(indicator = 'ed attendances',
               sub.indicator = 'any',
               steps = TRUE,
               theme = theme_bw(),
               tidy  = TRUE,
               join  = FALSE,
               facet = FALSE,
               sites = c('Bishop Auckland', 'Hartlepool', 'Hemel Hempstead', 'Newark', 'Rochdale'),)
dev.off()
png(file = '~/work/closed/tmp/ts-filtered-no-gap.png', width = 1024, height = 768)
closed_ts_plot(indicator = 'ed attendances',
               sub.indicator = 'any',
               steps = TRUE,
               theme = theme_bw(),
               tidy  = TRUE,
               join  = TRUE,
               facet = FALSE,
               sites = c('Bishop Auckland', 'Hartlepool', 'Hemel Hempstead', 'Newark', 'Rochdale'),)
dev.off()


## 2016-06-20 - Checking filtering of spurious data points carrys through when called from closed-models()
load('~/work/closed/tmp/ed attendances by mode measure - site - 2016-05-24 14.26.Rda')
check.join.true <- closed_models(df.lsoa          = ed_attendances_by_mode_measure,
                                 df.trust         = ed_attendances_by_mode_site_measure,
                                 indicator        = 'ed attendances',
                                 sub.indicator    = 'any',
                                 steps            = c('closure'),
                                 fit.with         = model.opts$fit.with,
                                 panel.lsoa       = model.opts$panel.lsoa,
                                 panel.trust      = model.opts$panel.trust,
                                 timevar          = model.opts$timevar,
                                 outcome          = model.opts$outcome,
                                 model1           = model.opts$mod1,
                                 model2           = model.opts$mod2,
                                 model3           = model.opts$mod3,
                                 model4           = model.opts$mod4,
                                 model5           = model.opts$mod5,
                                 model6           = model.opts$mod6,
                                 model7           = model.opts$mod7,
                                 autocorr         = model.opts$autocorr,
                                 panelcorrmethod  = model.opts$panelcorrmethod,
                                 coefficients     = model.opts$coefficients,
                                 seq.times        = model.opts$seq.times,
                                 plot             = model.opts$plot,
                                 common.y         = model.opts$common.y,
                                 theme            = model.opts$theme,
                                 ## return.df        = model.opts$return.df,
                                 return.df        = TRUE,
                                 return.model     = model.opts$return.model,
                                 return.residuals = model.opts$return.residuals,
                                 join.line = TRUE)
check.join.false <- closed_models(df.lsoa          = ed_attendances_by_mode_measure,
                                 df.trust         = ed_attendances_by_mode_site_measure,
                                 indicator        = 'ed attendances',
                                 sub.indicator    = 'any',
                                 steps            = c('closure'),
                                 fit.with         = model.opts$fit.with,
                                 panel.lsoa       = model.opts$panel.lsoa,
                                 panel.trust      = model.opts$panel.trust,
                                 timevar          = model.opts$timevar,
                                 outcome          = model.opts$outcome,
                                 model1           = model.opts$mod1,
                                 model2           = model.opts$mod2,
                                 model3           = model.opts$mod3,
                                 model4           = model.opts$mod4,
                                 model5           = model.opts$mod5,
                                 model6           = model.opts$mod6,
                                 model7           = model.opts$mod7,
                                 autocorr         = model.opts$autocorr,
                                 panelcorrmethod  = model.opts$panelcorrmethod,
                                 coefficients     = model.opts$coefficients,
                                 seq.times        = model.opts$seq.times,
                                 plot             = model.opts$plot,
                                 common.y         = model.opts$common.y,
                                 theme            = model.opts$theme,
                                 ## return.df        = model.opts$return.df,
                                 return.df        = TRUE,
                                 return.model     = model.opts$return.model,
                                 return.residuals = model.opts$return.residuals,
                                 join.line = FALSE)
save(check.join.true,
     check.join.false,
     file = '~/work/closed/tmp/check-20160620.RData')


## 2016-06-20 - Checking filtering of spurious data points in closed_ts_plots()
load('~/work/closed/tmp/ed attendances by mode measure - site - 2016-05-24 14.26.Rda')
unfiltered <- closed_ts_plot(indicator = 'ed attendances',
                             sub.indicator = 'any',
                             steps = TRUE,
                             theme = theme_bw(),
                             tidy  = FALSE,
                             join  = FALSE,
                             facet = FALSE,
                             sites = c('Bishop Auckland', 'Hartlepool', 'Hemel Hempstead', 'Newark', 'Rochdale'))
filtered <- closed_ts_plot(indicator = 'ed attendances',
                           sub.indicator = 'any',
                           steps = TRUE,
                           theme = theme_bw(),
                           tidy  = TRUE,
                           join  = FALSE,
                           facet = FALSE,
                           sites = c('Bishop Auckland', 'Hartlepool', 'Hemel Hempstead', 'Newark', 'Rochdale'))
filtered.joined <- closed_ts_plot(indicator = 'ed attendances',
                                  sub.indicator = 'any',
                                  steps = TRUE,
                                  theme = theme_bw(),
                                  tidy  = TRUE,
                                  join  = TRUE,
                                  facet = FALSE,
                                  sites = c('Bishop Auckland', 'Hartlepool', 'Hemel Hempstead', 'Newark', 'Rochdale'))


## 2016-06-15 - Checking whether the pooled analyses are actually balanced or not
formula <- reformulate(response = 'value',
                       termlabels = c('town * closure', 'season', 'relative_month', 'nhs111', 'other.centre', 'ambulance.divert'))
df5 <- ed_attendances_by_mode_site_measure %>%
       as.data.frame() %>%
       filter(measure == 'ed attendances' &
              sub_measure == 'any')
## Add in covariates
df5$season <- 1
df5 <- within(df5,{
    season[month(yearmonth) == 1  | month(yearmonth) == 2]  <- 1
    season[month(yearmonth) == 3  | month(yearmonth) == 4]  <- 2
    season[month(yearmonth) == 5  | month(yearmonth) == 6]  <- 3
    season[month(yearmonth) == 7  | month(yearmonth) == 8]  <- 4
    season[month(yearmonth) == 9  | month(yearmonth) == 10] <- 5
    season[month(yearmonth) == 11 | month(yearmonth) == 12] <- 6
})
df5$closure  <- ifelse(df5$relative_month  > 24, 1, 0)
df5 <- mutate(df5,
              nhs111 = ifelse((town == 'Bishop Auckland' & relative_month >= 35) |
                              (town == 'Southport' & relative_month >= 48) |
                              ## ToDo - Uncomment once confirmed and revised dates available
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
                                    1, 0)
              )
## Set base level
df5$town <- factor(df5$town)
df5$town <- relevel(df5$town, ref = 'Whitehaven')
## Test the model
check <- panelAR(data = df5,
                 formula = formula,
                 timeVar = 'relative_month',
                 panelVar = 'town',
                 autoCorr = 'ar1',
                 panelCorrMethod = 'pcse')
## Is it balanced?
summary(check)$panelStructure$balanced


## 2016-06-14 - Now get complaints about colinearity in Model 3 lets investigate
formula <- reformulate(response = 'value',
                       termlabels = c('town * closure', 'season', 'relative_month', 'nhs111', 'other.centre', 'ambulance.divert'))
df3 <- ed_attendances_by_mode_site_measure %>%
       as.data.frame() %>%
       filter(measure == 'ed attendances' &
              sub_measure == 'any' &
              group == 'Bishop Auckland General Hospital')
dim(df3)
## Add in covariates
df3$season <- 1
df3 <- within(df3,{
    season[month(yearmonth) == 1  | month(yearmonth) == 2]  <- 1
    season[month(yearmonth) == 3  | month(yearmonth) == 4]  <- 2
    season[month(yearmonth) == 5  | month(yearmonth) == 6]  <- 3
    season[month(yearmonth) == 7  | month(yearmonth) == 8]  <- 4
    season[month(yearmonth) == 9  | month(yearmonth) == 10] <- 5
    season[month(yearmonth) == 11 | month(yearmonth) == 12] <- 6
})
df3$closure  <- ifelse(df3$relative_month  > 24, 1, 0)
df3 <- mutate(df3,
              nhs111 = ifelse((town == 'Bishop Auckland' & relative_month >= 35) |
                              (town == 'Southport' & relative_month >= 48) |
                              ## ToDo - Uncomment once confirmed and revised dates available
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
                                    1, 0)
              )

panelAR(data = df3,
        formula = formula,
        timeVar = 'relative_month',
        panelVar = 'town',
        autoCorr = 'ar1',
        panelCorrMethod = 'pcse')

panelAR(data = test$df3,
        formula = test$formula.model3,
        timeVar = test$timevar,
        panelVar = test$panelvar,
        autoCorr = test$autocorr,
        panelCorrMethod = test$panelcorrmethod)


## 2016-06-13 - All seems fine but now I get an error with panelAR() complaining that the defined
##              time variable (saved in timevar) isn't an integer, yet my investigations (insert
##              following after line 581 in closed_models()) shows that it is...
        ##        filter(df2,
        ##        town == 'Bishop Auckland' |
        ##        town == 'Whitehaven') %>%
        ##        names() %>% print()
        ## filter(df2,
        ##        town == 'Bishop Auckland' |
        ##        town == 'Whitehaven') %>%
        ##        closed_missing() %>% names() %>% print()
        ## filter(df2,
        ##        town == 'Bishop Auckland' |
        ##        town == 'Whitehaven') %>%
        ##     closed_missing() %>% head() %>% print()
        ## print(timevar)
        ## typeof(df2$relative.month) %>% print()


## 2016-06-13 - Converted closed_missing() to use dplyr/mutate which removes the need
##              to reshape the data and massively simplifies the function.  This has
##              thrown up an error whereby groups which have all NA return logical when
##              the variable (value) should be numeric.  It was supposedly fixed in
##              August 2015 (see https://github.com/hadley/dplyr/issues/958), but doesn't
##              seem to work within the function.  Following is some code to play around
##              with whilst ironing out code in closed_missing()
test1 <- ed_attendances_by_mode_site_measure %>%
         data.frame() %>%
         filter(town == 'Hemel Hempstead' | town == 'Whitehaven') %>%
         closed_missing()
test2 <- ed_attendances_by_mode_site_measure %>%
         data.frame() %>%
         filter(town == 'Hemel Hempstead' | town == 'Whitehaven') %>%
         group_by(relative_month) %>%
         mutate(value = ifelse(anyNA(value), NA, value))
test3 <- ed_attendances_by_mode_site_measure %>%
         data.frame() %>%
         ## filter(town == 'Hemel Hempstead' | town == 'Whitehaven') %>%
         closed_missing()



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
