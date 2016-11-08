#!/bin/bash
##
## This script takes template.Rmd and uses the command line text editor sed
## and regular expressions to produce a number of files that print the results
## for the multitude of different outcomes.

## Measure     : Mode of Arrival
## Sub-Measure : Any
sed -e 's/template/mode.of.arrival.any/g' \
    -e 's/Measure/ed attendances/g' \
    -e 's/sub_measure/any/g' \
    -e 's/data_frame/ed_attendances_by_mode/g' \
    -e 's/DESCRIPTION/mode of attendance by any means/g' \
    template.Rmd > mode_of_arrival_any.Rmd

## Measure     : Mode of Arrival
## Sub-Measure : Ambulance
sed -e 's/template/mode.of.arrival.ambulance/g' \
    -e 's/Measure/ed attendances/g' \
    -e 's/sub_measure/ambulance/g' \
    -e 's/data_frame/ed_attendances_by_mode/g' \
    -e 's/DESCRIPTION/mode of attendance by any means/g' \
    template.Rmd > mode_of_arrival_any.Rmd

## Measure     : Mode of Arrival
## Sub-Measure : Other
sed -e 's/template/mode.of.arrival.other/g' \
    -e 's/Measure/ed attendances/g' \
    -e 's/sub_measure/other/g' \
    -e 's/data_frame/ed_attendances_by_mode/g' \
    -e 's/DESCRIPTION/mode of attendance by any means/g' \
    template.Rmd > mode_of_arrival_any.Rmd
