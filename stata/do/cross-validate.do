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

/* Derive a dummy for the closure step                                */
gen closure = cond(relative_month >= 25, 1, 0, .)

/* Derive dummys for other steps                                      */
gen nhs111 = 0
replace nhs111 = 1 if(town == "Bishop Auckland" & relative_month >= 35)
replace nhs111 = 1 if(town == "Southport" & relative_month >= 48)
replace nhs111 = 1 if(town == "Rochdale" & relative_month >= 48)
replace nhs111 = 1 if(town == "Rotherham" & relative_month >= 48)
replace nhs111 = 1 if(town == "Hartlepool" & relative_month >= 45)
replace nhs111 = 1 if(town == "Grimsby" & relative_month >= 16)
gen ambulance_divert = 0
replace ambulance_divert = 1 if(town == "Rochdale" & relative_month >= 17)
gen other_centre = 0
replace other_centre = 1 if(town == "Hemel Hempstead" & relative_month >= 20)
replace other_centre = 1 if(town == "Newark" & relative_month >= 3)
replace other_centre = 1 if(town == "Rochdale" & relative_month >= 11)
replace other_centre = 1 if(town == "Hartlepool" & relative_month >= 22)

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
fvset base 0 nhs111
fvset base 0 other_centre
fvset base 0 ambulance_divert

/* Save data so it can be restored                                    */
tempfile master
save "`master'", replace

/* Define models                                                      */
local model1 "value season relative_month i.closure i.nhs111 i.other_centre i.ambulance_divert"
local model2 "value season relative_month i.closure##i.town i.nhs111 i.other_centre i.ambulance_divert"
local model3 "value season relative_month i.closure##i.town i.nhs111 i.other_centre i.ambulance_divert"
local model4 "value season relative_month i.closure##i.town i.nhs111 i.other_centre i.ambulance_divert"
local model5 "value season relative_month i.closure##i.town i.nhs111 i.other_centre i.ambulance_divert"

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
fvset base 18 town
prais `model2' if(town_txt == "Bishop Auckland" | town_txt == "Whitehaven"), vce(cluster town)
parmest, saving("data/stata/model2_bishop", replace) idstr("2 : Bishop Auckland")
/* Model 2 - Hartlepool                                               */
fvset base 5 town
prais `model2' if(town_txt == "Hartlepool" | town_txt == "Grimsby"), vce(cluster town)
parmest, saving("data/stata/model2_hartlepool", replace) idstr("2 : Hartlepool")
/* Model 2 - Hemel Hempstead                                          */
fvset base 17 town
prais `model2' if(town_txt == "Hemel Hempstead" | town_txt == "Warwick"), vce(cluster town)
parmest, saving("data/stata/model2_hemel", replace) idstr("2 : Hemel Hempstead")
/* Model 2 - Newark                                                   */
fvset base 15 town
prais `model2' if(town_txt == "Newark" | town_txt == "Southport"), vce(cluster town)
parmest, saving("data/stata/model2_newark", replace) idstr("2 : Newark")
/* Model 2 - Rochdale                                                 */
fvset base 10 town
prais `model2' if(town_txt == "Rochdale" | town_txt == "Rotherham"), vce(cluster town)
parmest, saving("data/stata/model2_rochdale", replace) idstr("2 : Rochdale")


/* Model 3 - Bishop Auckland                                        */
fvset base 18 town
prais `model3' if(group == "Bishop Auckland General Hospital"), vce(cluster town)
parmest, saving("data/stata/model3_bishop", replace) idstr("3 : Bishop Auckland")
/* Model 3 - Hartlepool                                             */
fvset base 5 town
prais `model3' if(group == "University Hospital of Hartlepool"), vce(cluster town)
parmest, saving("data/stata/model3_hartlepool", replace) idstr("3 : Hartlepool")
/* Model 3 - Hemel Hempstead                                        */
fvset base 17 town
prais `model3' if(group == "Hemel Hempstead Hospital"), vce(cluster town)
parmest, saving("data/stata/model3_hemel", replace) idstr("3 : Hemel Hempstead")
/* Model 3 - Newark                                                 */
fvset base 15 town
prais `model3' if(group == "Newark Hospital"), vce(cluster town)
parmest, saving("data/stata/model3_newark", replace) idstr("3 : Newark")
/* Model 3 - Rochdale                                               */
fvset base 10 town
prais `model3' if(group == "Rochdale Infirmary"), vce(cluster town)
parmest, saving("data/stata/model3_rochdale", replace) idstr("3 : Rochdale")

/* Model 4 - All Case & One Matched                                 */
fvset base 18 town
fvset base 0 closure
prais `model4' if(town_txt == "Bishop Auckland" | town_txt == "Whitehaven" | town_txt == "Hartlepool" | town_txt == "Grimsby" | town_txt == "Hemel Hempstead" | town_txt == "Warwick" | town_txt == "Newark" | town_txt == "Southport" | town_txt == "Rochdale" | town_txt == "Rotherham"), vce(cluster town)
parmest, saving("data/stata/model4", replace) idstr("4 : All v One Controls")

/* Model 5 - All                                                      */
fvset base 18 town
fvset base 0 closure
prais `model5', vce(cluster town)
parmest, saving("data/stata/model5", replace) idstr("5 : All v All Controls")


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

append using "data/stata/model3_bishop"
append using "data/stata/model3_hartlepool"
append using "data/stata/model3_hemel"
append using "data/stata/model3_newark"
append using "data/stata/model3_rochdale"

append using "data/stata/model4"
append using "data/stata/model5"

/* Remove reference/non-existent levels for which there are no        */
/* estimates                                                          */
*drop if(estimate == 0 & stderr == 0 & t == . & p == .)

/* Format results and output to CSV for reading into R and comparing  */
gen model = substr(idstr, 1, strpos(idstr, " :") - 1)
destring model, replace
gen town  = substr(idstr, strpos(idstr, ": ") + 2, .)
gen term = ""
replace term = "Season"                    if(parm == "season")
replace term = "Time (Month)"              if(parm == "relative_month")
replace term = "ED Closure"                if(parm == "1.closure")
replace term = "NHS 111"                    if(parm == "1.nhs111")
replace term = "Other Medical Centre"      if(parm == "1.other_centre")
replace term = "Ambulances Diverted"       if(parm == "1.ambulance_divert")
replace term = "(Intercept)"               if(parm == "_cons")
replace term = "Basingstoke"               if(parm == "1.town")
replace term = "Bishop Auckland"           if(parm == "2.town")
replace term = "Blackburn"                 if(parm == "3.town")
replace term = "Carlisle"                  if(parm == "4.town")
replace term = "Grimsby"                   if(parm == "5.town")
replace term = "Hartlepool"                if(parm == "6.town")
replace term = "Hemel Hempstead"           if(parm == "7.town")
replace term = "Newark"                    if(parm == "8.town")
replace term = "Rochdale"                  if(parm == "9.town"2)
replace term = "Rotherham"                 if(parm == "10.town")
replace term = "Salford"                   if(parm == "11.town")
replace term = "Salisbury"                 if(parm == "12.town")
replace term = "Scarborough"               if(parm == "13.town")
replace term = "Scunthorpe"                if(parm == "14.town")
replace term = "Southport"                 if(parm == "15.town")
replace term = "Wansbeck"                  if(parm == "16.town")
replace term = "Warwick"                   if(parm == "17.town")
replace term = "Whitehaven"                if(parm == "18.town")
replace term = "Wigan"                     if(parm == "19.town")
replace term = "Yeovil"                    if(parm == "20.town")
replace term = "Basingstoke x Closure"     if(parm == "1.closure#1.town")
replace term = "Bishop Auckland x Closure" if(parm == "1.closure#2.town")
replace term = "Blackburn x Closure"       if(parm == "1.closure#3.town")
replace term = "Carlisle x Closure"        if(parm == "1.closure#4.town")
replace term = "Grimsby x Closure"         if(parm == "1.closure#5.town")
replace term = "Hartlepool x Closure"      if(parm == "1.closure#6.town")
replace term = "Hemel Hempstead x Closure" if(parm == "1.closure#7.town")
replace term = "Newark x Closure"          if(parm == "1.closure#8.town")
replace term = "Rochdale x Closure"        if(parm == "1.closure#9.town")
replace term = "Rotherham x Closure"       if(parm == "1.closure#10.town")
replace term = "Salford x Closure"         if(parm == "1.closure#11.town")
replace term = "Salisbury x Closure"       if(parm == "1.closure#12.town")
replace term = "Scarborough x Closure"     if(parm == "1.closure#13.town")
replace term = "Scunthorpe x Closure"      if(parm == "1.closure#14.town")
replace term = "Southport x Closure"       if(parm == "1.closure#15.town")
replace term = "Wansbeck x Closure"        if(parm == "1.closure#16.town")
replace term = "Warwick x Closure"         if(parm == "1.closure#17.town")
replace term = "Whitehaven x Closure"      if(parm == "1.closure#18.town")
replace term = "Wigan x Closure"           if(parm == "1.closure#19.town")
replace term = "Yeovil x Closure"          if(parm == "1.closure#20.town")
drop if(term == "" & estimate == 0 & t == . & p == .)

/* Write to file for reading into R and deriving comparisons          */
outsheet model town term estimate stderr t p min95 max95 using data/stata/results.csv, comma replace

/* Close and convert the log file                                     */
log c
log2html "log/cross-validate.smcl", replace
rm "log/cross-validate.smcl"
