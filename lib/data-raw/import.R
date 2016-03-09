## Filename            import.R
## Created             2016-03-08
## Author              n.shephard@sheffield.ac.uk
## Description         Import/tidies a few things required for ClosED, primarily
##                     it generates the co-varite 'steps'

## Load libraries (in theory this file is run without the package loaded)
library(dplyr)
library(lubridate)

##################################################################################
## Closure Steps                                                                ##
##################################################################################
## Data is available from 2007-01-01 through to 2014-03-01 for all sites.
## There are five 'case' sites where the ED closed a dummy indicator is required
## for each
site <- c("bishop auckland",
           "barnstaple",
           "macclesfield",
           "whitehaven",
           "hartlepool",
           "carlisle",
           "scarborough",
           "southport",
           "hemel hempstead",
           "basingstoke",
           "scunthorpe",
           "wigan",
           "newark",
           "salisbury",
           "warwick",
           "yeovil",
           "rochdale",
           "blackburn",
           "chesterfield",
           "grimsby")

## Add in the date of closure for specific sites and a dummy indicator that
## groups them
steps <- data.frame(site)
steps <- within(steps, {
                ## Closure (Step 0)
                closure                            <- ""
                closure[site == "bishop auckland"] <- "2009-10-01"
                closure[site == "hartlepool"]      <- "2011-08-01"
                closure[site == "hemel hempstead"] <- "2011-04-01"
                closure[site == "newark"]          <- "2010-08-01"
                closure[site == "rochdale"]        <- "2011-08-01"
                closure                            <- ymd(closure)
                ## NHS111 (Step 1)
                nhs111                             <- ""
                nhs111[site == "bishop auckland"]  <- "2009-10-01"
                nhs111[site == "barnstaple"]       <- "2009-08-02"
                nhs111[site == "macclesfield"]     <- "2008-12-01"
                nhs111[site == "whitehaven"]       <- "2003-04-01"
                nhs111[site == "hartlepool"]       <- "2011-08-01"
                nhs111[site == "carlisle"]         <- "2010-12-01"
                nhs111[site == "scarborough"]      <- "2010-11-01"
                nhs111[site == "southport"]        <- "2011-04-01"
                nhs111[site == "hemel hempstead"]  <- "2011-04-01"
                nhs111[site == "newark"]           <- "2010-08-01"
                nhs111[site == "salisbury"]        <- "2010-01-01"
                nhs111[site == "warwick"]          <- "2010-09-01"
                nhs111[site == "yeovil"]           <- "2010-06-01"
                nhs111[site == "rochdale"]         <- "2011-08-01"
                nhs111[site == "blackburn"]        <- "2011-03-01"
                nhs111[site == "chesterfield"]     <- "2011-04-01"
                nhs111[site == "grimsby"]          <- "2011-12-01"
                nhs111                             <- ymd(nhs111)
                ## (Step 2)
                step2                             <- ""
                step2[site == "bishop auckland"]  <- "2009-10-01"
                step2[site == "barnstaple"]       <- "2009-08-02"
                step2[site == "macclesfield"]     <- "2008-12-01"
                step2[site == "whitehaven"]       <- "2003-04-01"
                step2[site == "hartlepool"]       <- "2011-08-01"
                step2[site == "carlisle"]         <- "2010-12-01"
                step2[site == "scarborough"]      <- "2010-11-01"
                step2[site == "southport"]        <- "2011-04-01"
                step2[site == "hemel hempstead"]  <- "2011-04-01"
                step2[site == "newark"]           <- "2010-08-01"
                step2[site == "salisbury"]        <- "2010-01-01"
                step2[site == "warwick"]          <- "2010-09-01"
                step2[site == "yeovil"]           <- "2010-06-01"
                step2[site == "rochdale"]         <- "2011-08-01"
                step2[site == "blackburn"]        <- "2011-03-01"
                step2[site == "chesterfield"]     <- "2011-04-01"
                step2[site == "grimsby"]          <- "2011-12-01"
                step2                             <- ymd(step2)
                ## (Step 3)
                step3                             <- ""
                step3[site == "bishop auckland"]  <- "2009-10-01"
                step3[site == "barnstaple"]       <- "2009-08-02"
                step3[site == "macclesfield"]     <- "2008-12-01"
                step3[site == "whitehaven"]       <- "2003-04-01"
                step3[site == "hartlepool"]       <- "2011-08-01"
                step3[site == "carlisle"]         <- "2010-12-01"
                step3[site == "scarborough"]      <- "2010-11-01"
                step3[site == "southport"]        <- "2011-04-01"
                step3[site == "hemel hempstead"]  <- "2011-04-01"
                step3[site == "newark"]           <- "2010-08-01"
                step3[site == "salisbury"]        <- "2010-01-01"
                step3[site == "warwick"]          <- "2010-09-01"
                step3[site == "yeovil"]           <- "2010-06-01"
                step3[site == "rochdale"]         <- "2011-08-01"
                step3[site == "blackburn"]        <- "2011-03-01"
                step3[site == "chesterfield"]     <- "2011-04-01"
                step3[site == "grimsby"]          <- "2011-12-01"
                step3                             <- ymd(step3)
                ## (Step 4)
                step4                             <- ""
                step4[site == "bishop auckland"]  <- "2009-10-01"
                step4[site == "barnstaple"]       <- "2009-08-02"
                step4[site == "macclesfield"]     <- "2008-12-01"
                step4[site == "whitehaven"]       <- "2003-04-01"
                step4[site == "hartlepool"]       <- "2011-08-01"
                step4[site == "carlisle"]         <- "2010-12-01"
                step4[site == "scarborough"]      <- "2010-11-01"
                step4[site == "southport"]        <- "2011-04-01"
                step4[site == "hemel hempstead"]  <- "2011-04-01"
                step4[site == "newark"]           <- "2010-08-01"
                step4[site == "salisbury"]        <- "2010-01-01"
                step4[site == "warwick"]          <- "2010-09-01"
                step4[site == "yeovil"]           <- "2010-06-01"
                step4[site == "rochdale"]         <- "2011-08-01"
                step4[site == "blackburn"]        <- "2011-03-01"
                step4[site == "chesterfield"]     <- "2011-04-01"
                step4[site == "grimsby"]          <- "2011-12-01"
                step4                             <- ymd(step4)
                ## Group identifiers for subset analysis                
                group                              <- ""
                group[site == "bishop auckland"]   <- "bishop auckland"
                group[site == "barnstaple"]        <- "bishop auckland"
                group[site == "macclesfield"]      <- "bishop auckland"
                group[site == "whitehaven"]        <- "bishop auckland"
                group[site == "hartlepool"]        <- "hartlepool"
                group[site == "carlisle"]          <- "hartlepool"
                group[site == "scarborough"]       <- "hartlepool"
                group[site == "southport"]         <- "hartlepool"
                group[site == "hemel hempstead"]   <- "hemel hempstead"
                group[site == "basingstoke"]       <- "hemel hempstead"
                group[site == "scunthorpe"]        <- "hemel hempstead"
                group[site == "wigan"]             <- "hemel hempstead"
                group[site == "newark"]            <- "newark"
                group[site == "salisbury"]         <- "newark"
                group[site == "warwick"]           <- "newark"
                group[site == "yeovil"]            <- "newark"
                group[site == "rochdale"]          <- "rochdale"
                group[site == "blackburn"]         <- "rochdale"
                group[site == "chesterfield"]      <- "rochdale"
                group[site == "grimsby"]           <- "rochdale"
                ## group <- factor(group,
                ##                 levels = c("bishop auckland", "hartlepool", "hemel hempstead", "newark", "rochdale"))
})

## Load the data prepared by Tony Stone and merge the steps with it
load("d:/Rpackages/rclosed/data/example measures data.Rda")

## Save the data
save(steps,
     file = "../data/master.RData")
