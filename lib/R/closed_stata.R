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
#' @param version Version of Stata to write file as.
#'
#' @export
closed_stata <- function(file         = 'ed attendances by mode measure - site - 2016-11-21 19.54.Rda',
                         df           = ed_attendances_by_mode_site_measure,
                         path_source  = '~/work/scharr/closed/lib/data/',
                         path_stata   = '~/work/scharr/closed/stata/data',
                         version      = 14){
    ## Combine the path of the source file with the filename and load it
    to_read <- paste0(path_source, file)
    load(to_read)
    ## Substitute the file extension from Rda to dta and combine with the output path
    to_write <- paste0(path_stata, gsub("Rda", "dta", file))
    ## Write to Stata
    write_dta(data    = df,
              path    = to_write,
              version = version)
}
