#' Tidy ClosED data
#'
#' @description Tidy up Closed data and combine with 'step' co-variates
#'
#' @details The ClosED study uses time-series with dummy indicators to
#' test for the impact of closing Emergency Departments on indicators of
#' performance.  This short function tidies the data (ensures case of strings
#' is consistent, date/time and factor variables are correctly formatted) and
#' combines them with the dummy indicators that denote 'steps' (changes) in
#' the EDs functioning
#'
#' @param df Data frame to tidy.
#' @param steps Data frame containing steps
#'
#' @return A merged data frame of the two specified.
#'
#' @examples
#'
#' @references
#'
#' @export
tidy <- function(df        = test,
                 steps     = steps,
                 ...){
    ## Convert all strings to lower case and underscores in
    ## variable names to periods (as per Google R style guide
    ## which I adopt since '_' is bound to '<-' under
    ## Emacs Speaks Statistics)
    names(df) <- names(df) %>%
                 gsub("_", ".", x = .)
    df <- within(df, {
        site <- tolower(site)
        measure <- tolower(measure)
        sub_measure <- tolower(sub_measure)
    })
    ## Merge the two based on centre and month
    combined <- merge(df,
                      steps,
                      by  = c("site", "yearmonth"),
                      all = TRUE)
    ## Convert names as stored to those used in the SAP add in identifiers for groups
    df <- within(df, {
                 site.short <- ""
                 site.short[site == "bishop auckland general hospital"]      <- "Bishop Auckland"
                 site.short[site == "hemel hempstead hospital"]              <- "Hemel Hempstead"
                 site.short[site == "newark hospital"]                       <- "Newark"
                 site.short[site == "rochdale infirmary"]                    <- "Rochdale"
                 site.short[site == "university hospital of hartlepool"]     <- "Hartlepool"
                 site.short[site == "basingstoke and north hampshire"]       <- "Basingstoke"
                 site.short[site == "chesterfield royal"]                    <- "Chesterfield"
                 site.short[site == "cumberland infirmary"]                  <- "Carlisle"
                 site.short[site == "diana princess of wales"]               <- "Grimsby"
                 site.short[site == "macclesfield district general"]         <- "Macclesfield"
                 site.short[site == "north devon district"]                  <- "Barnstaple"
                 site.short[site == "royal albert edward infirmary"]         <- "Wigan"
                 site.short[site == "royal blackburn"]                       <- "Blackburn"
                 site.short[site == "salisbury district"]                    <- "Salisbury"
                 site.short[site == "scarborough"]                           <- "Scarborough"
                 site.short[site == "scunthorpe general"]                    <- "Scunthorpe"
                 site.short[site == "southport and formby district general"] <- "Southport"
                 site.short[site == "warwick"]                               <- "Warwick"
                 site.short[site == "west cumberland"]                       <- "Whitehaven"
                 site.short[site == "yeovil district"]                       <- "Yeovil"
                 group <- ""
                 group[site.short == "Bishop Auckland"] <- 1
                 group[site.short == "Barnstaple"]      <- 1
                 group[site.short == "Macclesfield"]    <- 1
                 group[site.short == "Whitehaven"]      <- 1
                 group[site.short == "Hartlepool"]      <- 2
                 group[site.short == "Carlisle"]        <- 2
                 group[site.short == "Scarborough"]     <- 2
                 group[site.short == "Southport"]       <- 2
                 group[site.short == "Hemel Hempstead"] <- 3
                 group[site.short == "Basingstoke"]     <- 3
                 group[site.short == "Scunthorpe"]      <- 3
                 group[site.short == "Wigan"]           <- 3
                 group[site.short == "Newark"]          <- 4
                 group[site.short == "Salisbury"]       <- 4
                 group[site.short == "Warwick"]         <- 4
                 group[site.short == "Yeovil"]          <- 4
                 group[site.short == "Rochdale"]        <- 5
                 group[site.short == "Blackburn"]       <- 5
                 group[site.short == "Chesterfield"]    <- 5
                 group[site.short == "Grimsby"]         <- 5
                 group <- as.factor(group,
                                    levels = c(1:5),
                                    labels = c("Bishop Auckland",
                                               "Hartlepool",
                                               "Hemel Hempstead",
                                               "Newark",
                                               "Rochdale"))

    })
    ## Return the tidy and merged dataframe
    return(combined)
}
