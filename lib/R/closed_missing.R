#' Convert observations to missing
#'
#' @description Convert observations to missing for corresponding locales.
#'
#' @details The ClosED study uses time-series analyses, but an inherent problem with
#' such data are missing observations.  Where these arise in one center (ED) data set
#' the corresponding observations in the other centres within the matched groups are
#' converted to missing by this function.
#'
#' @param df Data frame to convert.
#'
#' @return A data frame.
#'
#' @examples
#'
#'
#' ts.mortality <- closed_missing(df       = ed_attendances_by_mode_measure)
#'
#' @references
#'
#' @export
closed_missing <- function(df    = ed_attendances_by_mode_measure,
                           area  = 'lsoa',
                           ...){
    results <- list()
    ## Convert to data frame
    df  <- as.data.frame(df)
    results$orig <- df
    ## Convert underscores to periods
    names(df)  <- names(df) %>%
                  gsub("_", ".", x = .)
    ## towns <- levels(df$town)
    ## ...or...
    ## 2016-06-13 - Alternative approach using dplyr/mutate, much more efficient
    ##              and no need to differentiate between area levels, as the data
    ##              is filtered and passed to the function before hand and it
    ##              therefore reduces to grouping by month to check whether there
    ##              is any site/lsoa within that month that have missing data (via
    ##              the incredibly useful anyNA() function.
    df <- df %>%
          group_by(relative.month) %>%
          mutate(value = ifelse(anyNA(value), NA, value)) %>%
          ## transform(value = ifelse(anyNA(value), NA, value)) %>%
          ungroup() %>%
          as.data.frame()
    ## For some reason the relative.month variable isn't an interger when returned
    ## df$relative.month <- as.integer(df$relative.month)
    return(df)
}
