/* File           check_negbin.do                                   */
/* Author         n.shephard@sheffield.ac.uk                        */
/* Description    Uses -xtnbreg- to cross-check negative binomial   */
/*                time-series regerssion.                           */
clear
set more off
capture log c
version 14.1
/* Conditionally set the base directory                             */
if("`c(os)'" == "Unix"){
    local base_dir "~/work/closed/nihr_report/"
}
else{
    di "This scripts will only run on the mcru-closed-hp Virtual Machine due to restrictions on data security."
    exit
}

/* Load the Site level data                                         */
use "`base_dir'/data/lsoa.dta", clear
local measure     = subinstr("`1'", "_", " ", .)
local sub_measure = subinstr("`2'", "_", " ", .)
di "`measure'"
tab measure sub_measure
keep if(measure == "`measure'" & sub_measure == "`sub_measure'")
decode town, generate(town_string)
egen _lsoa_town = concat(town lsoa)
encode _lsoa_town, generate(lsoa_town)
drop _lsoa_town
xtset lsoa_town relative_month
tempfile data
save `data', replace

log using "`base_dir'/log/lsoa_`measure'_`sub_measure'.smcl", replace

/* Set the various models, can only run models 0 to 5 as LSOA level */
/* data can not be taken off of the Virtual Machines and I don't    */
/* have Stata available on the GNU/Linux Virtual Machine            */
local iter        200
local tolerance   0.01
local nrtolerance 0.001
local ltolerance  0.001
local outcome     value
local model6_1    relative_month i.season i.nhs111 other_centre ambulance_divert diff_time_to_ed
local model7_1    relative_month i.season i.nhs111 other_centre ambulance_divert diff_time_to_ed
local sites "Bishop_Auckland Hartlepool Hemel_Hempstead Newark Rochdale"

/************************************************************************/
/* PROBLEM!!!! Some sites do not run, build local macro conditional     */
/*             on outcome being tested for now until this is solved.    */
/************************************************************************/

/* Run Analyses for within centres                                      */
foreach x of local sites{
    use "`data'", clear
    /* Model 6.1                                                        */
    di "`x'"
    local y = subinstr("`x'", "_", " ", .)
    xtnbreg `outcome' `model6_1' if(town_string == "`y'"), iterate(`iter') ltolerance(`ltolerance') nrtolerance(`nrtolerance')
    parmest, saving("`base_dir'/data/results/model6_1.dta", replace) eform label
    use "`base_dir'/data/results/model6_1.dta", clear
    gen model = "model6.1"
    gen town = "`y'"
    if("`y'" == "Bishop Auckland"){
        tempfile results
        save `results', replace
    }
    else{
        append using `results'
        save `results', replace
    }
}

/* Run Analyses for pooled centres at both Site level (model 7)       */
use `data', clear
keep if(town_string == "Bishop Auckland" | ///
        town_string == "Hartlepool"      | ///
        town_string == "Hemel Hempstead" | ///
        town_string == "Newark"          | ///
        town_string == "Rochdale")
/* 2016-12-21 Quite a few sites need excluding from models >= 2 do so here   */
if("`measure'" != "ed attendances" & "`sub_measure'" != "any"){
    drop if(town_string == "Grimsby")
}
if("`measure'" != "ed attendances" & "`sub_measure'" != "ambulance"){
    drop if(town_string == "Grimsby")
}
if("`measure'" != "unnecessary ed attendances" & "`sub_measure'" != "all"){
    drop if(town_string == "Hartlepool")
}
if("`measure'" != "ambulance mean times" & "`sub_measure'" != "call to dest"){
    drop if(town_string == "White Haven")
}
if("`measure'" != "ambulance mean times" & "`sub_measure'" != "call to dest"){
    drop if(town_string == "Grimsby")
}
if("`measure'" != "ambulance mean times" & "`sub_measure'" != "call to dest"){
    drop if(town_string == "Hemel Hempstead")
}
if("`measure'" != "ambulance mean times" & "`sub_measure'" != "call to scene any"){
    drop if(town_string == "White Haven")
}
if("`measure'" != "ambulance mean times" & "`sub_measure'" != "call to scene any"){
    drop if(town_string == "Grimsby")
    }
if("`measure'"!= "ambulance mean times" & "`sub_measure'" != "call to scene any"){
    drop if(town_string == "Hemel Hempstead")
}
if("`measure'"!= "ambulance mean times" & "`sub_measure'" != "call to scene any"){
    drop if(town_string == "Newark")
}
if("`measure'"!= "ambulance mean times" & "`sub_measure'" != "call to scene any"){
    drop if(town_string == "Hemel Hempstead")
}
if("`measure'" != "ambulance mean times" & "`sub_measure'" != "call to scene conveying"){
    drop if(town_string == "White Haven")
}
if("`measure'" != "ambulance mean times" & "`sub_measure'" != "call to scene conveying"){
    drop if(town_string == "Grimsby")
}
if("`measure'" != "ambulance mean times" & "`sub_measure'" != "call to scene conveying"){
    drop if(town_string == "Hemel Hempstead")
}
if("`measure'" != "ambulance mean times" & "`sub_measure'" != "call to scene conveying"){
    drop if(town_string == "Newark")
}
if("`measure'"!= "ambulance mean times" & "`sub_measure'" != "call to scene conveying"){
    drop if(town_string == "Rochdale")
}
if("`measure'" != "ambulance mean times" & "`sub_measure'" != "scene to dest"){
    drop if(town_string == "Grimsby")
}
if("`measure'"!= "ambulance mean times" & "`sub_measure'" != "scene to dest"){
    drop if(town_string == "Hemel Hempstead")
}
if("`measure'"!= "ambulance mean times" & "`sub_measure'" != "scene to dest"){
    drop if(town_string == "Newark")
}
if("`measure'"!= "ambulance mean times" & "`sub_measure'" != "scene to dest"){
    drop if(town_string == "Rochdale")
}
if("`measure'"!= "ambulance mean times" & "`sub_measure'" != "dest to clear"){
    drop if(town_string == "White Haven")
}
if("`measure'" != "ambulance mean times" & "`sub_measure'" != "dest to clear"){
    drop if(town_string == "Grimsby")
}
if("`measure'"!= "ambulance mean times" & "`sub_measure'" != "dest to clear"){
    drop if(town_string == "Hemel Hempstead")
}
if("`measure'"!= "ambulance mean times" & "`sub_measure'" != "dest to clear"){
    drop if(town_string == "Newark")
}
if("`measure'"!= "ambulance mean times" & "`sub_measure'" != "dest to clear"){
    drop if(town_string == "Rochdale")
}
if("`measure'" != "ambulance green calls" & "`sub_measure'" != "green calls"){
    drop if(town_string == "Grimsby")
}
if("`measure'" != "ambulance green calls" & "`sub_measure'" != "green calls"){
    drop if(town_string == "Warwick")
}
if("`measure'" != "ambulance green calls" & "`sub_measure'" != "green calls"){
    drop if(town_string == "Newark")
}
if("`measure'" != "ambulance green calls" & "`sub_measure'" != "fraction not conveyed"){
    drop if(town_string == "Grimsby")
}
if("`measure'" != "ambulance green calls" & "`sub_measure'" != "fraction not conveyed"){
    drop if(town_string == "Warwick")
}
if("`measure'" != "ambulance green calls" & "`sub_measure'" != "fraction not conveyed"){
    drop if(town_string == "Newark")
}
if("`measure'" != "ambulance red calls" & "`sub_measure'" != "hospital transfers"){
    drop if(town_string == "Grimsby")
}
if("`measure'" != "ambulance red calls" & "`sub_measure'" != "hospital transfers"){
    drop if(town_string == "Warwick")
}
if("`measure'" != "ambulance red calls" & "`sub_measure'" != "hospital transfers"){
    drop if(town_string == "Newark")
}
if("`measure'" != "ambulance red calls" & "`sub_measure'" != "total"){
    drop if(town_string == "Grimsby")
}
if("`measure'" != "ambulance red calls" & "`sub_measure'" != "total"){
    drop if(town_string == "Warwick")
}
if("`measure'" != "ambulance red calls" & "`sub_measure'" != "total"){
    drop if(town_string == "Newark")
}
/* Model 1                                                        */
xtnbreg `outcome' `model7_1', iterate(`iter') ltolerance(`ltolerance') nrtolerance(`nrtolerance')
parmest, saving("`base_dir'/data/results/model7_1.dta", replace) eform label
use "`base_dir'/data/results/model7_1.dta", clear
gen model = "model7.1"
gen town  = "All"
append using `results'
save `results', replace

/* Add in meaningful terms for the parameters which are now all       */
/* obfuscated                                                         */
gen measure     = "`measure'"
gen sub_measure = "`sub_measure'"
replace label = "(Intercept)"                 if(parm == "Constant")
replace label = "closure"                     if(parm == "1.closure")
replace label = "season2"                     if(parm == "2.season")
replace label = "season3"                     if(parm == "3.season")
replace label = "season4"                     if(parm == "4.season")
replace label = "season5"                     if(parm == "5.season")
replace label = "season6"                     if(parm == "6.season")
replace label = "townBishop Auckland"         if(parm == "2o.town")
replace label = "townHartlepool"              if(parm == "6o.town")
replace label = "townHemel Hempstead"         if(parm == "7o.town")
replace label = "townNewark"                  if(parm == "8o.town")
replace label = "townRochdale"                if(parm == "9o.town")
replace label = "townBishop Auckland:closure" if(parm == "2o.town#1o.closure")
replace label = "townHartlepool:closure"      if(parm == "6o.town#1o.closure")
replace label = "townHemel Hempstead:closure" if(parm == "7o.town#1o.closure")
replace label = "townNewark:closure"          if(parm == "8o.town#1o.closure")
replace label = "townRochdale:closure"        if(parm == "9o.town#1o.closure")

/* Save for reading into Stata                                        */
keep measure sub_measure town model parm label estimate stderr z p min95 max95
save "`base_dir'/data/results/stata_negbin_lsoa.dta", replace
save "`base_dir'/data/results/stata_negbin_lsoa_`measure'_`sub_measure'.dta", replace

log c
log2html "`base_dir'/log/lsoa_`measure'_`sub_measure'.smcl", replace
rm  "`base_dir'/log/lsoa_`measure'_`sub_measure'.smcl"
