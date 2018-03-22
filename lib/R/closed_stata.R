#' Write data files to Stata format
#'
#' @description Write data files to Stata format for ClosED data.
#'
#' @details Summary files by month are written to Stata files using the haven package.
#'
#' @param file R formatted data file to load
#' @param df data frame within the data file that is to be saved in Stata format
#' @param path_source Path to the directory containing the R formatted file to load
#' @param path_stata Path to the directory that Stata files are to be written
#' @param pooled Indicator of whether the data is \code{count} or \code{proportion}, required
#'               because you either \code{sum()} counts across High/Low sites or take the
#'               \code{mean()} if its proportions.
#' @param version Version of Stata to write file as.
#'
#' @export
closed_stata <- function(file         = 'ed attendances by mode measure - site - 2016-11-21 19.54.Rda',
                         df          = ed_attendances_by_mode_site_measure,
                         path_source  = '~/work/scharr/closed/lib/data/',
                         path_stata   = '~/work/scharr/closed/stata/data',
                         pooled       = "count",
                         version      = 14){
    ## Combine the path of the source file with the filename and load it
    to_read <- paste0(path_source, file)
    load(to_read)
    ## "Tidy" the data frame, this adds the dummy variables (copy and pasted
    ## from closed_tidy() function as I could not get calling it to work.
    ## df <- closed_tidy(df = df)
    names(df) <- gsub('_', '.', names(df))
    df$sub.measure <- ifelse(is.na(df$sub.measure), 'all', df$sub.measure)
    df$measure     <- gsub('_', ' ', df$measure)
    df$sub.measure <- gsub('_', ' ', df$sub.measure)
    ## Factor variables
    df$town <- factor(df$town)
    #######################################################################
    ## Derive a seasonal indicator                                       ##
    #######################################################################
    ## print("Debug 5")
    ## print("LSOA")
    ## dim(df) %>% print()
    ## print("TRUST")
    ## dim(df.trust) %>% print()
    df$season <- 1
    df <- within(df,{
                      season[month(yearmonth) == 1  | month(yearmonth) == 2]  <- 1
                      season[month(yearmonth) == 3  | month(yearmonth) == 4]  <- 2
                      season[month(yearmonth) == 5  | month(yearmonth) == 6]  <- 3
                      season[month(yearmonth) == 7  | month(yearmonth) == 8]  <- 4
                      season[month(yearmonth) == 9  | month(yearmonth) == 10] <- 5
                      season[month(yearmonth) == 11 | month(yearmonth) == 12] <- 6
    })
    df$season <- factor(df$season)
    df$season <- relevel(df$season, ref = '1')
    #######################################################################
    ## Add a dummy 'step' for closure                                    ##
    #######################################################################
    df$closure  <- ifelse(df$relative.month  > 24,
                          1, 0)
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
                                       (town == 'Southport' & relative.month >= 3) |
                                       (town == 'Rochdale' & relative.month >= 11), ##  |
                                       ## (town == 'Hartlepool' & relative.month >= 22),
                                       1, 0),
                 misc = ifelse((town == 'Hemel Hempstead' & relative.month >= 38),
                               1, 0)
                 )
    ## If thi is LSOA data then aggregate into high/low (code grabbed from
    ## closed_stata_negbin.R / closed_ts_plot_lsoa_binary.R)
    if(grepl("lsoa", file)){
        ## Derive High/Low difference in time to ED by town...
        binary <- dplyr::select(df, town, lsoa, diff.time.to.ed) %>%
                  dplyr::filter(diff.time.to.ed != 0) %>%
                  unique() %>%
                  group_by(town) %>%
                  mutate(median      = quantile(diff.time.to.ed, probs = c(0.5)),
                         binary.diff = ifelse(diff.time.to.ed < median, 'Low', 'High')) %>%
                  dplyr::select(lsoa, town, binary.diff)
        ## Merge and summarise based on type of data
        if(pooled == "proportion"){
            df <- merge(dplyr::select(df, -diff.time.to.ed),
                        binary,
                        by = c('town', 'lsoa')) %>%
                  group_by(town, relative.month, measure, sub.measure, binary.diff) %>%
                  summarise(value            = mean(value, na.rm = TRUE),
                            closure          = mean(closure),
                            nhs111           = mean(nhs111),
                            ambulance.divert = mean(ambulance.divert),
                            other.centre     = mean(other.centre),
                            season           = mean(as.numeric(season))) %>%
                  ungroup() %>%
                  mutate(season = as.factor(season),
                         relative.month = as.integer(relative.month))

        }
        else if(pooled == "countr"){
            df <- merge(dplyr::select(df, -diff.time.to.ed),
                        binary,
                        by = c('town', 'lsoa')) %>%
                  group_by(town, relative.month, measure, sub.measure, binary.diff) %>%
                  summarise(value            = sum(value, na.rm = TRUE),
                            closure          = mean(closure),
                            nhs111           = mean(nhs111),
                            ambulance.divert = mean(ambulance.divert),
                            other.centre     = mean(other.centre),
                            season           = mean(as.numeric(season))) %>%
                  ungroup() %>%
                  mutate(season = as.factor(season),
                         relative.month = as.integer(relative.month))
        }
    }
    ## Substitute the file extension from Rda to dta and combine with the output path
    ## and convert '.' in variable names back to '_' for Stata
    names(df) <- gsub("\\.", "_", names(df))
    names(df) %>% print()
    to_write <- paste0(path_stata, gsub("Rda", "dta", file))
    ## Write to Stata
    write_dta(data    = df,
              path    = to_write,
              version = version)
    ## Remove the data frame to leave a clean workspace
    rm(df)
}
