/* Filename     cross-validate.do                                    */
/* Created      2016-05-10                                           */
/* Author       n.shephard@sheffield.ac.uk                           */
/* Description  Repeats Prais-Winsten regression to check the results*/
/*              from R's panelAR() function/package                  */

/* Set up the system                                                 */
clear
set more off
capture log c
version 14.1

/* Create a log-file so we can compare the results                    */
log using "log/cross-validate.smcl", replace


/* Read in the data                                                   */
insheet using "data/csv/ed_attendances_by_mode_site_measure.csv", clear

/* Derive a closure dummy variable                                    */
gen closure = cond(relative_month >= 24, 1, 0, .)
/* Derive a Season indicator                                          */
gen month = substr(yearmonth, 6, 2)
gen     season = 1
replace season = 1 if(month == "01" | month == "02")
replace season = 2 if(month == "03" | month == "04")
replace season = 3 if(month == "05" | month == "06")
replace season = 4 if(month == "07" | month == "08")
replace season = 5 if(month == "09" | month == "10")
replace season = 6 if(month == "11" | month == "12")
/* Encode town (required for -tsset-)                                 */
rename town town_txt
encode town_txt, gen(town)

/* List the encoding so that these can be used to -fvset- the data.   */
label list town

/* Set the data as Time-series                                        */
tsset town relative_month

/* Set reference group for factor variables                           */
fvset base 0 closure

/* Save data so it can be restored                                    */
tempfile master
save "`master'", replace

/* Define models                                                      */
local model1   "value season relative_month i.closure"
local model2   "value season relative_month i.closure##i.town"
local model2_5 "value season relative_month i.closure##i.town"
local model5   "value season relative_month i.closure##i.town"

/* Model 1 - Bishop Auckland                                          */
prais `model1' if(town_txt == "Bishop Auckland"), vce(robust)
parmest, saving("data/stata/model1_bishop", replace) idstr("1 : Bishop Auckland")
/* Model 1 - Hartlepool                                               */
prais `model1' if(town_txt == "Hartlepool"), vce(robust)
parmest, saving("data/stata/model1_hartlepool", replace) idstr("1 : Hartlepool")
/* Model 1 - Hemel Hempstead                                          */
prais `model1' if(town_txt == "Hemel Hempstead"), vce(robust)
parmest, saving("data/stata/model1_hemel", replace) idstr("1 : Hemel Hempstead")
/* Model 1 - Newark                                                   */
prais `model1' if(town_txt == "Newark"), vce(robust)
parmest, saving("data/stata/model1_newark", replace) idstr("1 : Newark")
/* Model 1 - Rochdale                                                 */
prais `model1' if(town_txt == "Rochdale"), vce(robust)
parmest, saving("data/stata/model1_rochdale", replace) idstr("1 : Rochdale")

/* Model 2 - Bishop Auckland                                          */
fvset base 10 town
prais `model2' if(town_txt == "Bishop Auckland" | town_txt == "Whitehaven"), vce(cluster town)
parmest, saving("data/stata/model2_bishop", replace) idstr("2 : Bishop Auckland")
/* Model 2 - Hartlepool                                               */
fvset base 2 town
prais `model2' if(town_txt == "Hartlepool" | town_txt == "Grimsby"), vce(cluster town)
parmest, saving("data/stata/model2_hartlepool", replace) idstr("2 : Hartlepool")
/* Model 2 - Hemel Hempstead                                          */
fvset base 9 town
prais `model2' if(town_txt == "Hemel Hempstead" | town_txt == "Warwick"), vce(cluster town)
parmest, saving("data/stata/model2_hemel", replace) idstr("2 : Hemel Hempstead")
/* Model 2 - Newark                                                   */
fvset base 8 town
prais `model2' if(town_txt == "Newark" | town_txt == "Southport"), vce(cluster town)
parmest, saving("data/stata/model2_newark", replace) idstr("2 : Newark")
/* Model 2 - Rochdale                                                 */
fvset base 7 town
prais `model2' if(town_txt == "Rochdale" | town_txt == "Rotherham"), vce(cluster town)
parmest, saving("data/stata/model2_rochdale", replace) idstr("2 : Rochdale")


/* Model 2.5 - Bishop Auckland                                        */
fvset base 10 town
prais `model2_5' if(town_txt == "Bishop Auckland" | town_txt == "Whitehaven"), vce(cluster town)
parmest, saving("data/stata/model2_5_bishop", replace) idstr("2.5 : Bishop Auckland")
/* Model 2.5 - Hartlepool                                             */
fvset base 2 town
prais `model2_5' if(town_txt == "Hartlepool" | town_txt == "Grimsby"), vce(cluster town)
parmest, saving("data/stata/model2_5_hartlepool", replace) idstr("2.5 : Hartlepool")
/* Model 2.5 - Hemel Hempstead                                        */
fvset base 9 town
prais `model2_5' if(town_txt == "Hemel Hempstead" | town_txt == "Warwick"), vce(cluster town)
parmest, saving("data/stata/model2_5_hemel", replace) idstr("2.5 : Hemel Hempstead")
/* Model 2.5 - Newark                                                 */
fvset base 8 town
prais `model2_5' if(town_txt == "Newark" | town_txt == "Southport"), vce(cluster town)
parmest, saving("data/stata/model2_5_newark", replace) idstr("2.5 : Newark")
/* Model 2.5 - Rochdale                                               */
fvset base 7 town
prais `model2_5' if(town_txt == "Rochdale" | town_txt == "Rotherham"), vce(cluster town)
parmest, saving("data/stata/model2_5_rochdale", replace) idstr("2.5 : Rochdale")

/* Model 5 - All                                                      */
fvset base 10 town
fvset base 0 closure
prais `model5', vce(cluster town)
parmest, saving("data/stata/model5", replace) idstr("5 : All v All Controls")

/* Model 5.5 - All Cases v Matched Controls                           */
fvset base 10 town
fvset base 0 closure
prais `model5.5' if(town_txt == "Bishop Auckland" | town_txt == "Whitehaven" | ///
                    town_txt == "Hartlepool"      | town_txt == "Grimsby" |    ///
                    town_txt == "Hemel Hempstead" | town_txt == "Warwick" |    ///
                    town_txt == "Newark"          | town_txt == "Southport" |  ///
                    town_txt == "Rochdale"        | town_txt == "Rotherham"), vce(cluster town)
parmest, saving("data/stata/model5_5", replace) idstr("5.5 : All v Matched Controls")

/* Load and merge all saved results                                   */
use "data/stata/model1_bishop", clear
append using "data/stata/model1_hartlepool"
append using "data/stata/model1_hemel"
append using "data/stata/model1_newark"
append using "data/stata/model1_rochdale"

append using "data/stata/model2_bishop"
append using "data/stata/model2_hartlepool"
append using "data/stata/model2_hemel"
append using "data/stata/model2_newark"
append using "data/stata/model2_rochdale"

append using "data/stata/model2_5_bishop"
append using "data/stata/model2_5_hartlepool"
append using "data/stata/model2_5_hemel"
append using "data/stata/model2_5_newark"
append using "data/stata/model2_5_rochdale"

append using "data/stata/model5"
append using "data/stata/model5_5"

/* Remove reference/non-existent levels for which there are no        */
/* estimates                                                          */
*drop if(estimate == 0 & stderr == 0 & t == . & p == .)

/* Format results and output to CSV for reading into R and comparing  */
gen model = substr(idstr, 1, strpos(idstr, " :") - 1)
destring model, replace
gen town  = substr(idstr, strpos(idstr, ": ") + 2, .)
gen term = ""
replace term = "Season"                    if(parm == "season")
replace term = "Time"                      if(parm == "relative_month")
replace term = "Closure"                   if(parm == "1.closure")
replace term = "Intercept"                 if(parm == "_cons")
replace term = "Whitehaven"                if(parm == "10.town" & model == 2)
replace term = "Grimsby"                   if(parm == "10.town" & model == 2)
replace term = "Warwick"                   if(parm == "10.town" & model == 2)
replace term = "Bishop Auckland"           if(parm == "1.town" & model == 2)
replace term = "Grimsby"                   if(parm == "2.town" & model == 2)
replace term = "Hartlepool"                if(parm == "3.town" & model == 2)
replace term = "Hemel Hempstead"           if(parm == "4.town" & model == 2)
replace term = "Newark"                    if(parm == "5.town" & model == 2)
replace term = "Rochdale"                  if(parm == "6.town" & model == 2)
replace term = "Rotherham"                 if(parm == "7.town" & model == 2)
replace term = "Southport"                 if(parm == "8.town" & model == 2)
replace term = "Warwick"                   if(parm == "9.town" & model == 2)
replace term = "Whitehaven"                if(parm == "10.town" & model == 2)
replace term = "No Closure x Bishop Auckland" if(parm == "0bclosure#1o.town" & model > 3)
replace term = "No Closure x Grimsby"         if(parm == "0bclosure#2o.town" & model > 3)
replace term = "No Closure x Hartlepool"      if(parm == "0bclosure#3o.town" & model > 3)
replace term = "No Closure x Hemel Hempstead" if(parm == "0bclosure#4o.town" & model > 3)
replace term = "No Closure x Newark"          if(parm == "0bclosure#5o.town" & model > 3)
replace term = "No Closure x Rochdale"        if(parm == "0bclosure#6o.town" & model > 3)
replace term = "No Closure x Rotherham"       if(parm == "0bclosure#7o.town" & model > 3)
replace term = "No Closure x Southport"       if(parm == "0bclosure#8o.town" & model > 3)
replace term = "No Closure x Warwick"         if(parm == "0bclosure#9o.town" & model > 3)
replace term = "No Closure x Whitehaven"      if(parm == "0bclosure#10o.town" & model > 3)
replace term = "Closure x Bishop Auckland" if(parm == "1.closure#1.town" & model > 3)
replace term = "Closure x Grimsby"         if(parm == "1.closure#2.town" & model > 3)
replace term = "Closure x Hartlepool"      if(parm == "1.closure#3.town" & model > 3)
replace term = "Closure x Hemel Hempstead" if(parm == "1.closure#4.town" & model > 3)
replace term = "Closure x Newark"          if(parm == "1.closure#5.town" & model > 3)
replace term = "Closure x Rochdale"        if(parm == "1.closure#6.town" & model > 3)
replace term = "Closure x Rotherham"       if(parm == "1.closure#7.town" & model > 3)
replace term = "Closure x Southport"       if(parm == "1.closure#8.town" & model > 3)
replace term = "Closure x Warwick"         if(parm == "1.closure#9.town" & model > 3)
replace term = "Closure x Whitehaven"      if(parm == "1.closure#10.town" & model > 3)

/*
/* Close and convert the log file                                     */
log c
log2html "log/cross-validate.smcl", replace
rm "log/cross-validate.smcl"
