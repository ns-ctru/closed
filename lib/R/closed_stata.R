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
                         df           = ed_attendances_by_mode_site_measure,
                         path_source  = '~/work/scharr/closed/lib/data/',
                         path_stata   = '~/work/scharr/closed/stata/data',
                         pooled       = "count",
                         version      = 14){
    ## Combine the path of the source file with the filename and load it
    to_read <- paste0(path_source, file)
    load(to_read)
    ## "Tidy" the data frame, this adds the dummy variables
    df <- closed_tidy(df = df)
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
    to_write <- paste0(path_stata, gsub("Rda", "dta", file))
    ## Write to Stata
    write_dta(data    = df,
              path    = to_write,
              version = version)
    ## Remove the data frame to leave a clean workspace
    rm(df)
}
