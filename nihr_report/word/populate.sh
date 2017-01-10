#!/bin/bash
##
## A short script to take 'template.Rmd' and produce five files, one for each
## case site for producing a M$-Word format document in the desired structure.

## Bishop Auckland
sed -e 's/SITE/Bishop Auckland/g' template.Rmd \
    -e 's/CONTROL/Whitehaven/g' \
    -e 's/SHUT/closed\/downgraded/g' \
    -e 's/DATE/October 2009/g' \
    -e 's/OUTCOME1/mean time from 999 call to hospital Emergency Department/g' \
    -e 's/OUTCOME2/mean time from 999 call to arrival at scene/g' \
    -e 's/OUTCOME3/mean time from scene to hospital Emergency Department/g' \
    -e 's/OUTCOME4/total Emergency Department Attendances/g' \
    -e 's/OUTCOME5/Emergency Department attendances by ambulance/g' \
    -e 's/OUTCOME6/Emergency Department attendances by other modes/g' \
    -e 's/OUTCOME7/Emergency Admissions/g' \
    -e 's/OUTCOME8/all avoidable Emergency Admissions/g' \
    -e 's/OUTCOME9/all avoidable non-sepcific chest pain Emergency Admissions/g' \
    -e 's/OUTCOME10/all avoidable deaths/g' \
    -e 's/OUTCOME11/all avoidable pre-hospital deaths/g' \
    -e 's/OUTCOME12/case fatality ratio for avoidable deaths/g' > results_bishop.Rmd

## Hartlepool
sed -e 's/SITE/Hartlepool/g' template.Rmd \
    -e 's/CONTROL/Grimsby/g' \
    -e 's/SHUT/closed\/downgraded/g' \
    -e 's/DATE/August 2011/g' \
    -e 's/OUTCOME1/mean time from 999 call to hospital Emergency Department/g' \
    -e 's/OUTCOME2/mean time from 999 call to arrival at scene/g' \
    -e 's/OUTCOME3/mean time from scene to hospital Emergency Department/g' \
    -e 's/OUTCOME4/total Emergency Department Attendances/g' \
    -e 's/OUTCOME5/Emergency Department attendances by ambulance/g' \
    -e 's/OUTCOME6/Emergency Department attendances by other modes/g' \
    -e 's/OUTCOME7/Emergency Admissions/g' \
    -e 's/OUTCOME8/all avoidable Emergency Admissions/g' \
    -e 's/OUTCOME9/all avoidable non-sepcific chest pain Emergency Admissions/g' \
    -e 's/OUTCOME10/all avoidable deaths/g' \
    -e 's/OUTCOME11/all avoidable pre-hospital deaths/g' \
    -e 's/OUTCOME12/case fatality ratio for avoidable deaths/g' > results_hartlepool.Rmd

## Hemel Hempstead
sed -e 's/SITE/Hemel Hempstead/g' template.Rmd \
    -e 's/CONTROL/Warwick/g' \
    -e 's/SHUT/closed\/downgraded/g' \
    -e 's/DATE/March 2009/g' \
    -e 's/OUTCOME1/mean time from 999 call to hospital Emergency Department/g' \
    -e 's/OUTCOME2/mean time from 999 call to arrival at scene/g' \
    -e 's/OUTCOME3/mean time from scene to hospital Emergency Department/g' \
    -e 's/OUTCOME4/total Emergency Department Attendances/g' \
    -e 's/OUTCOME5/Emergency Department attendances by ambulance/g' \
    -e 's/OUTCOME6/Emergency Department attendances by other modes/g' \
    -e 's/OUTCOME7/Emergency Admissions/g' \
    -e 's/OUTCOME8/all avoidable Emergency Admissions/g' \
    -e 's/OUTCOME9/all avoidable non-sepcific chest pain Emergency Admissions/g' \
    -e 's/OUTCOME10/all avoidable deaths/g' \
    -e 's/OUTCOME11/all avoidable pre-hospital deaths/g' \
    -e 's/OUTCOME12/case fatality ratio for avoidable deaths/g' > results_hemel.Rmd

## Newark
sed -e 's/SITE/Newark/g' template.Rmd \
    -e 's/CONTROL/Southport/g' \
    -e 's/SHUT/closed\/downgraded/g' \
    -e 's/DATE/April 2011/g' \
    -e 's/OUTCOME1/mean time from 999 call to hospital Emergency Department/g' \
    -e 's/OUTCOME2/mean time from 999 call to arrival at scene/g' \
    -e 's/OUTCOME3/mean time from scene to hospital Emergency Department/g' \
    -e 's/OUTCOME4/total Emergency Department Attendances/g' \
    -e 's/OUTCOME5/Emergency Department attendances by ambulance/g' \
    -e 's/OUTCOME6/Emergency Department attendances by other modes/g' \
    -e 's/OUTCOME7/Emergency Admissions/g' \
    -e 's/OUTCOME8/all avoidable Emergency Admissions/g' \
    -e 's/OUTCOME9/all avoidable non-sepcific chest pain Emergency Admissions/g' \
    -e 's/OUTCOME10/all avoidable deaths/g' \
    -e 's/OUTCOME11/all avoidable pre-hospital deaths/g' \
    -e 's/OUTCOME12/case fatality ratio for avoidable deaths/g' > results_newark.Rmd

## Rochdale
sed -e 's/SITE/Rochdale/g' template.Rmd \
    -e 's/CONTROL/Rotherham/g' \
    -e 's/SHUT/closed\/downgraded/g' \
    -e 's/DATE/April 2011/g' \
    -e 's/OUTCOME1/mean time from 999 call to hospital Emergency Department/g' \
    -e 's/OUTCOME2/mean time from 999 call to arrival at scene/g' \
    -e 's/OUTCOME3/mean time from scene to hospital Emergency Department/g' \
    -e 's/OUTCOME4/total Emergency Department Attendances/g' \
    -e 's/OUTCOME5/Emergency Department attendances by ambulance/g' \
    -e 's/OUTCOME6/Emergency Department attendances by other modes/g' \
    -e 's/OUTCOME7/Emergency Admissions/g' \
    -e 's/OUTCOME8/all avoidable Emergency Admissions/g' \
    -e 's/OUTCOME9/all avoidable non-sepcific chest pain Emergency Admissions/g' \
    -e 's/OUTCOME10/all avoidable deaths/g' \
    -e 's/OUTCOME11/all avoidable pre-hospital deaths/g' \
    -e 's/OUTCOME12/case fatality ratio for avoidable deaths/g' > results_rochdale.Rmd
