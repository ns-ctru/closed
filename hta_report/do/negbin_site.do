/* File           check_negbin.do                                   */
/* Author         n.shephard@sheffield.ac.uk                        */
/* Description    Uses -xtnbreg- to perform negative binomial       */
/*                time-series regerssion.                           */
clear
set more off
capture log c
version 14.1
/* Conditionally set the base directory                             */
if("`c(os)'" == "Unix"){
    local base_dir "~/work/closed/hta_report/"
}
else{
    di "This scripts will only run on the mcru-closed-hp Virtual Machine due to restrictions on data security."
    exit
}

/* Load the Site level data                                         */
use "`base_dir'/data/site.dta", clear
/* Sort out measure                                                 */
local measure     = subinstr("`1'", "_", " ", .)
local sub_measure = subinstr("`2'", "_", " ", .)
keep if(measure == "`measure'" & sub_measure == "`sub_measure'")
decode town, generate(town_string)
/* replace town_string = subinstr(town_string, " ", "_", .) */
xtset town relative_month
tempfile data
save `data', replace

log using "`base_dir'/log/site_`measure'_`sub_measure'.smcl", replace

/* Set the various models, can only run models 0 to 5 as LSOA level */
/* data can not be taken off of the Virtual Machines and I don't    */
/* have Stata available on the GNU/Linux Virtual Machine            */
local iter       20
local tolerance  0.01
local nrtolerance 0.001
local ltolerance  0.001
local outcome    value
local model0     i.closure
local model1     i.closure relative_month i.season i.nhs111 i.other_centre i.ambulance_divert
local model2     i.town##i.closure relative_month i.season i.nhs111 i.other_centre i.ambulance_divert
local model4     i.closure relative_month i.season i.nhs111 other_centre ambulance_divert
local sites "Bishop_Auckland Hartlepool Hemel_Hempstead Newark Rochdale"

/************************************************************************/
/* PROBLEM!!!! Some sites do not run, build local macro conditional     */
/*             on outcome being tested for now until this is solved.    */
/************************************************************************/
if("`measure'" == "ed attendances" & "`sub_measure'" == "any")            local sites "Bishop_Auckland Hartlepool Newark Rochdale"
else if("`measure'" == "ed attendances" & "`sub_measure'" == "other")     local sites "Hartlepool Hemel_Hempstead Newark Rochdale"
else if("`measure'" == "ed attendances" & "`sub_measure'" == "ambulance") local sites "Bishop_Auckland Hartlepool Hemel_Hempstead Newark Rochdale"
else if("`measure'" == "unnecessary ed attendances" & "`sub_measure'" == "all") local sites "Bishop_Auckland Hemel_Hempstead Newark Rochdale"
else if("`measure'" == "all emergency admissions" & "`sub_measure'" == "all") local sites "Bishop_Auckland Hartlepool Hemel_Hempstead Newark Rochdale"
else if("`measure'" == "avoidable emergency admissions" & "`sub_measure'" == "any") local sites "Bishop_Auckland Hartlepool Hemel_Hempstead Newark Rochdale"
else if("`measure'" == "avoidable emergency admissions" & "`sub_measure'" == "non-specific chest pain") local sites "Bishop_Auckland Hartlepool Hemel_Hempstead Newark Rochdale"
else if("`measure'" == "ed attendances admitted" & "`sub_measure'" == "all") local sites "Bishop_Auckland Hartlepool Newark Rochdale"
else if("`measure'" == "ed attendances admitted" & "`sub_measure'" == "admitted") local sites "Bishop_Auckland Hartlepool Hemel_Hempstead Newark Rochdale"
else if("`measure'" == "critical care stays" & "`sub_measure'" == "all") local sites "Bishop_Auckland Hartlepool Hemel_Hempstead Newark Rochdale"
else if("`measure'" == "critical care stays" & "`sub_measure'" == "critical care") local sites "Bishop_Auckland Hartlepool Hemel_Hempstead Newark Rochdale"
else if("`measure'" == "ambulance green calls" & "`sub_measure'" == "green calls") local sites "Bishop_Auckland Hartlepool Hemel_Hempstead Newark Rochdale"
else if("`measure'" == "ambulance green calls" & "`sub_measure'" == "not conveyed green calls") local sites "Bishop_Auckland Hartlepool Hemel_Hempstead Newark Rochdale"
else if("`measure'" == "ambulance red calls" & "`sub_measure'" == "hospital transfers") local sites "Bishop_Auckland Hartlepool Hemel_Hempstead Newark Rochdale"
else if("`measure'" == "ambulance red calls" & "`sub_measure'" == "total") local sites "Bishop_Auckland Hartlepool Hemel_Hempstead Newark Rochdale"
else if("`measure'" == "hospital transfers" & "`sub_measure'" == "all stays") local sites "Bishop_Auckland Hartlepool Hemel_Hempstead Newark Rochdale"
else if("`measure'" == "hospital transfers" & "`sub_measure'" == "stays with transfer") local sites "Bishop_Auckland Hartlepool Hemel_Hempstead Newark Rochdale"

di "MEASURE    : `measure'"
di "SUBMEASURE : `sub_measure'"
di "SITES      : `sites'"


/* Run Analyses for within centres                                      */
/* Initially these were not converging found a thread on Stata Forums   */
/* https://goo.gl/a5wHMx                                                */
foreach x of local sites{
    use "`data'", clear
    table town_string
    local y = subinstr("`x'", "_", " ", .)
    /* Model 0                                                          */
    di "`x' : Model 0"
    xtnbreg `outcome' `model0' if(town_string == "`y'"), iterate(`iter') ltolerance(`ltolerance') nrtolerance(`nrtolerance')
    parmest, saving("`base_dir'/data/results/model0.dta", replace) eform label
    /* Model 1                                                          */
    di "`x' : Model 1"
    xtnbreg `outcome' `model1' if(town_string == "`y'"), iterate(`iter') ltolerance(`ltolerance') nrtolerance(`nrtolerance')
    parmest, saving("`base_dir'/data/results/model1.dta", replace) eform label
    /* Model 2                                                          */
    di "`y' : Model 2"
    if("`y'" == "Bishop Auckland")      local x2 = "Whitehaven"
    else if("`y'" == "Hartlepool")      local x2 = "Grimsby"
    else if("`y'" == "Hemel Hempstead") local x2 = "Warwick"
    else if("`y'" == "Newark")          local x2 = "Southport"
    else if("`y'" == "Rochdale")        local x2 = "Rotherham"
    xtnbreg `outcome' `model2' if(town_string == "`y'" | town_string == "`x2'"), iterate(`iter') ltolerance(`ltolerance') nrtolerance(`nrtolerance')
    parmest, saving("`base_dir'/data/results/model2.dta", replace) eform label
    use "`base_dir'/data/results/model0.dta", clear
    gen model = "model0"
    tempfile t
    save `t', replace
    use "`base_dir'/data/results/model1.dta", clear
    gen model = "model1"
    append using `t'
    save `t', replace
    use "`base_dir'/data/results/model2.dta", clear
    gen model = "model2"
    append using `t'
    gen town = "`y'"
    /* if("`measure'" == "ed attendances" & "`sub_measure'" == "any"){ */
    /*     if("`y'" == "Bishop Auckland"){ */
    /*         tempfile results */
    /*         save `results', replace */
    /*     } */
    /*     else{ */
    /*         append using `results' */
    /*         save `results', replace */
    /*     } */
    /* } */
    /* else if("`measure'" == "ed attendances" & "`sub_measure'" == "other"){ */
    /*     if("`y'" == "Hartlepool"){ */
    /*         tempfile results */
    /*         save `results', replace */
    /*     } */
    /*     else{ */
    /*         append using `results' */
    /*         save `results', replace */
    /*     } */
    /* } */
    save "`base_dir'/data/results/`x'", replace
}


/* Combine files from across sites                                    */
if("`measure'" == "ed attendances" & "`sub_measure'" == "any"){
    use "`base_dir'/data/results/Bishop_Auckland.dta", clear
    append using "`base_dir'/data/results/Hartlepool.dta"
    /* append using "`base_dir'/data/results/Hemel_Hempstead.dta" */
    append using "`base_dir'/data/results/Newark.dta"
    append using "`base_dir'/data/results/Rochdale.dta"
}
else if("`measure'" == "ed attendances" & "`sub_measure'" == "other"){
    /* use "`base_dir'/data/results/Bishop_Auckland.dta", clear */
    use "`base_dir'/data/results/Hartlepool.dta"
    append using "`base_dir'/data/results/Hemel_Hempstead.dta"
    append using "`base_dir'/data/results/Newark.dta"
    append using "`base_dir'/data/results/Rochdale.dta"
}
else if("`measure'" == "ed attendances" & "`sub_measure'" == "ambulance"){
    use "`base_dir'/data/results/Bishop_Auckland.dta", clear
    append using "`base_dir'/data/results/Hartlepool.dta"
    append using "`base_dir'/data/results/Hemel_Hempstead.dta"
    append using "`base_dir'/data/results/Newark.dta"
    append using "`base_dir'/data/results/Rochdale.dta"
}
else if("`measure'" == "unnecessary ed attendances" & "`sub_measure'" == "all"){
    use "`base_dir'/data/results/Bishop_Auckland.dta", clear
    append using "`base_dir'/data/results/Hartlepool.dta"
    append using "`base_dir'/data/results/Hemel_Hempstead.dta"
    append using "`base_dir'/data/results/Newark.dta"
    append using "`base_dir'/data/results/Rochdale.dta"
}
else if("`measure'" == "all emergency admissions" & "`sub_measure'" == "all"){
    use "`base_dir'/data/results/Bishop_Auckland.dta", clear
    append using "`base_dir'/data/results/Hartlepool.dta"
    append using "`base_dir'/data/results/Hemel_Hempstead.dta"
    append using "`base_dir'/data/results/Newark.dta"
    append using "`base_dir'/data/results/Rochdale.dta"
}
else if("`measure'" == "avoidable emergency admissions" & "`sub_measure'" == "any"){
    use "`base_dir'/data/results/Bishop_Auckland.dta", clear
    append using "`base_dir'/data/results/Hartlepool.dta"
    append using "`base_dir'/data/results/Hemel_Hempstead.dta"
    append using "`base_dir'/data/results/Newark.dta"
    append using "`base_dir'/data/results/Rochdale.dta"
}
else if("`measure'" == "avoidable emergency admissions" & "`sub_measure'" == "non-specific chest pain"){
    use "`base_dir'/data/results/Bishop_Auckland.dta", clear
    append using "`base_dir'/data/results/Hartlepool.dta"
    append using "`base_dir'/data/results/Hemel_Hempstead.dta"
    append using "`base_dir'/data/results/Newark.dta"
    append using "`base_dir'/data/results/Rochdale.dta"
}
else if("`measure'" == "ed attendances admitted" & "`sub_measure'" == "all"){
    use "`base_dir'/data/results/Bishop_Auckland.dta", clear
    append using "`base_dir'/data/results/Hartlepool.dta"
    /* append using "`base_dir'/data/results/Hemel_Hempstead.dta" */
    append using "`base_dir'/data/results/Newark.dta"
    append using "`base_dir'/data/results/Rochdale.dta"
}
else if("`measure'" == "ed attendances admitted" & "`sub_measure'" == "admitted"){
    use "`base_dir'/data/results/Bishop_Auckland.dta", clear
    append using "`base_dir'/data/results/Hartlepool.dta"
    append using "`base_dir'/data/results/Hemel_Hempstead.dta"
    append using "`base_dir'/data/results/Newark.dta"
    append using "`base_dir'/data/results/Rochdale.dta"
}
else if("`measure'" == "critical care stays" & "`sub_measure'" == "all"){
    use "`base_dir'/data/results/Bishop_Auckland.dta", clear
    append using "`base_dir'/data/results/Hartlepool.dta"
    append using "`base_dir'/data/results/Hemel_Hempstead.dta"
    append using "`base_dir'/data/results/Newark.dta"
    append using "`base_dir'/data/results/Rochdale.dta"
}
else if("`measure'" == "critical care stays" & "`sub_measure'" == "critical care"){
    use "`base_dir'/data/results/Bishop_Auckland.dta", clear
    append using "`base_dir'/data/results/Hartlepool.dta"
    append using "`base_dir'/data/results/Hemel_Hempstead.dta"
    append using "`base_dir'/data/results/Newark.dta"
    append using "`base_dir'/data/results/Rochdale.dta"
}
else if("`measure'" == "ambulance green calls" & "`sub_measure'" == "green calls"){
    use "`base_dir'/data/results/Bishop_Auckland.dta", clear
    append using "`base_dir'/data/results/Hartlepool.dta"
    append using "`base_dir'/data/results/Hemel_Hempstead.dta"
    append using "`base_dir'/data/results/Newark.dta"
    append using "`base_dir'/data/results/Rochdale.dta"
}
else if("`measure'" == "ambulance green calls" & "`sub_measure'" == "not conveyed green calls"){
    use "`base_dir'/data/results/Bishop_Auckland.dta", clear
    append using "`base_dir'/data/results/Hartlepool.dta"
    append using "`base_dir'/data/results/Hemel_Hempstead.dta"
    append using "`base_dir'/data/results/Newark.dta"
    append using "`base_dir'/data/results/Rochdale.dta"
}
else if("`measure'" == "ambulance red calls" & "`sub_measure'" == "hospital transfers"){
    use "`base_dir'/data/results/Bishop_Auckland.dta", clear
    append using "`base_dir'/data/results/Hartlepool.dta"
    append using "`base_dir'/data/results/Hemel_Hempstead.dta"
    append using "`base_dir'/data/results/Newark.dta"
    append using "`base_dir'/data/results/Rochdale.dta"
}
else if("`measure'" == "ambulance red calls" & "`sub_measure'" == "total"){
    use "`base_dir'/data/results/Bishop_Auckland.dta", clear
    append using "`base_dir'/data/results/Hartlepool.dta"
    append using "`base_dir'/data/results/Hemel_Hempstead.dta"
    append using "`base_dir'/data/results/Newark.dta"
    append using "`base_dir'/data/results/Rochdale.dta"
}
else if("`measure'" == "hospital transfers" & "`sub_measure'" == "all stays"){
    use "`base_dir'/data/results/Bishop_Auckland.dta", clear
    append using "`base_dir'/data/results/Hartlepool.dta"
    append using "`base_dir'/data/results/Hemel_Hempstead.dta"
    append using "`base_dir'/data/results/Newark.dta"
    append using "`base_dir'/data/results/Rochdale.dta"
}
else if("`measure'" == "hospital transfers" & "`sub_measure'" == "stays with transfer"){
    use "`base_dir'/data/results/Bishop_Auckland.dta", clear
    append using "`base_dir'/data/results/Hartlepool.dta"
    append using "`base_dir'/data/results/Hemel_Hempstead.dta"
    append using "`base_dir'/data/results/Newark.dta"
    append using "`base_dir'/data/results/Rochdale.dta"
}


/* Run Analyses for pooled centres at both Site level (model 4)       */
tempfile previous_models
save `previous_models', replace

di "All : Model 4"
use `data', clear
keep if(town_string == "Bishop Auckland" | town_string ==  "Whitehaven" | ///
        town_string == "Hartlepool"      | town_string ==  "Grimsby"    | ///
        town_string == "Hemel Hempstead" | town_string ==  "Warwick"    | ///
        town_string == "Newark"          | town_string ==  "Southport"  | ///
        town_string == "Rochdale"        | town_string ==  "Rotherham")
xtnbreg `outcome' `model4', iterate(`iter') ltolerance(`ltolerance') nrtolerance(`nrtolerance') technique(nr 10 dfp 10 bfgs 10)
parmest, saving("`base_dir'/data/results/model4.dta", replace) eform label
use "`base_dir'/data/results/model4.dta", clear
gen model = "model4"
gen town  = "All"
append using `previous_models'
di "Append done!"

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
replace town = subinstr(town, "_", " ", .)

/* Save for reading into Stata                                        */
keep measure sub_measure town model parm label estimate stderr z p min95 max95
save "`base_dir'/data/results/stata_negbin_site.dta", replace
save "`base_dir'/data/results/stata_negbin_site_`measure'_`sub_measure'.dta", replace

log c
log2html "`base_dir'log/site_`measure'_`sub_measure'.smcl", replace
rm "`base_dir'log/site_`measure'_`sub_measure'.smcl"
