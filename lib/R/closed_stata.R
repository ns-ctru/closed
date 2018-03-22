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
    ## If thi is LSOA data then aggregate into high/low (code grabbed from
    ## closed_stata_negbin.R / closed_ts_plot_lsoa_binary.R)
    if(grepl("lsoa", file)){
        ## Derive High/Low difference in time to ED by town...
        binary <- dplyr::select(df, town, lsoa, diff_time_to_ed) %>%
                  dplyr::filter(diff_time_to_ed != 0) %>%
                  unique() %>%
                  group_by(town) %>%
                  mutate(median      = quantile(diff_time_to_ed, probs = c(0.5)),
                         binary_diff = ifelse(diff_time_to_ed < median, 'Low', 'High')) %>%
                  dplyr::select(lsoa, town, binary_diff)
        ## Merge and summarise based on type of data
        if(pooled == "proportion"){
            df <- merge(dplyr::select(df, -diff_time_to_ed),
                        binary,
                        by = c('town', 'lsoa')) %>%
                  group_by(town, relative_month, measure, sub_measure, binary_diff) %>%
                  summarise(value            = mean(value, na.rm = TRUE),
                            closure          = mean(closure),
                            nhs111           = mean(nhs111),
                            ambulance.divert = mean(ambulance_divert),
                            other.centre     = mean(other_centre),
                            season           = mean(as.numeric(season))) %>%
                  ungroup() %>%
                  mutate(season = as.factor(season),
                         relative_month = as.integer(relative_month))

        }
        else if(pooled == "countr"){
            df <- merge(dplyr::select(df, -diff_time_to_ed),
                        binary,
                        by = c('town', 'lsoa')) %>%
                  group_by(town, relative_month, measure, sub_measure, binary_diff) %>%
                  summarise(value            = sum(value, na.rm = TRUE),
                            closure          = mean(closure),
                            nhs111           = mean(nhs111),
                            ambulance_divert = mean(ambulance_divert),
                            other_centre     = mean(other_centre),
                            season           = mean(as.numeric(season))) %>%
                  ungroup() %>%
                  mutate(season = as.factor(season),
                         relative_month = as.integer(relative_month))
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
