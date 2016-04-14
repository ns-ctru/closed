## Filename            import.R
## Created             2016-03-08
## Author              n.shephard@sheffield.ac.uk
## Description         Import/tidies a few things required for ClosED, primarily
##                     it generates the co-varite 'steps'

## Load libraries (in theory this file is run without the package loaded)
library(dplyr)
library(lubridate)

##################################################################################
## Sites (includes closure steps)                                               ##
##################################################################################
sites <- read.csv('sites.csv')

##################################################################################
## Other steps                                                                  ##
##################################################################################

##################################################################################
## Combine Steps                                                                ##
##################################################################################

## Save the data
save(sites,
     file = '../data/master.RData')
