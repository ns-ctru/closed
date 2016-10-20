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
    local base_dir "~/work/closed/"
}
else{
    local base_dir "n:/projects/closed/stats"
}

/* Load the data and set up panels                                  */
use "`base_dir'/stata/data/attendances_by_ambulance.dta", clear
decode town, generate(town_string)
xtset town relative_month
tempfile data
save `data', replace


/* Set the various models, can only run models 0 to 5 as LSOA level */
/* data can not be taken off of the Virtual Machines and I don't    */
/* have Stata available on the GNU/Linux Virtual Machine            */
local iter     20
local outcome  value
local model0   i.closure
local model05  i.closure relative_month
local model1   i.closure relative_month i.season i.nhs111 other_centre ambulance_divert
local model2   i.town##i.closure relative_month i.season nhs111 other_centre ambulance_divert


/* Run Analyses for Attendance by Ambulance                         */
foreach x in "Bishop Auckland" "Hartlepool" "Hemel Hempstead" "Newark" "Rochdale"{
    local t_out  = strlower(subinstr("`x'", " ", "_", 1))
    use "`data'", clear
    /* Model 0                                                          */
    xtnbreg `outcome' `model0' if(town_string == "`x'"), iterate(`iter')
    parmest, saving("`base_dir'/stata/data/attendance_model0_`out'.dta", replace) eform label
    /* Model 0.5                                                        */
    xtnbreg `outcome' `model05' if(town_string == "`x'"), iterate(`iter')
    parmest, saving("`base_dir'/stata/data/attendance_model05_`out'.dta", replace) eform label
    /* Model 1                                                          */
    xtnbreg `outcome' `model1' if(town_string == "`x'"), iterate(`iter')
    parmest, saving("`base_dir'/stata/data/attendance_model1_`out'.dta", replace) eform label
    /* Model 2                                                          */
    if("x'" == "Bishop Auckland")      local x2 = "Whitehaven"
    else if("x'" == "Hartlepool")      local x2 = "Grimsby"
    else if("x'" == "Hemel Hempstead") local x2 = "Warwick"
    else if("x'" == "Newark")          local x2 = "Southport"
    else if("x'" == "Rochdale")        local x2 = "Rotherham"
    xtnbreg `outcome' `model2' if(town_string == "`x'" | town_string == "`x2'"), iterate(`iter')
    parmest, saving("`base_dir'/stata/data/attendance_model2_`out'.dta", replace) eform label
    use "`base_dir'/stata/data/attendance_model0_`out'.dta", clear
    gen model = "Model 0"
    tempfile t
    save `t', replace
    use "`base_dir'/stata/data/attendance_model05_`out'.dta", clear
    gen model = "Model 0.5"
    append using `t'
    save `t', replace
    use "`base_dir'/stata/data/attendance_model1_`out'.dta", clear
    gen model = "Model 1"
    append using `t'
    save `t', replace
    use "`base_dir'/stata/data/attendance_model2_`out'.dta", clear
    gen model = "Model 2"
    append using `t'
    gen town = "`x'"
    if("`x'" == "Bishop Auckland"){
        tempfile results
        save `results', replace
    }
    else{
        append using `results'
        save `results', replace
    }
}
gen indicator     = "ed attendances"
gen sub_indicator = "all"
save "`base_dir'/stata/data/results_attendance.dta", replace

/* Load the data and set up panels                                  */
use "`base_dir'/stata/data/admissions.dta", clear
decode town, generate(town_string)
xtset town relative_month
tempfile data
save `data', replace

/* Run Analyses                                                     */
foreach x in "Bishop Auckland" "Hartlepool" "Hemel Hempstead" "Newark" "Rochdale"{
    local t_out  = strlower(subinstr("`x'", " ", "_", 1))
    use "`data'", clear
    /* Model 0                                                          */
    xtnbreg `outcome' `model0' if(town_string == "`x'"), iterate(`iter')
    parmest, saving("`base_dir'/stata/data/attendance_model0_`out'.dta", replace) eform label
    /* Model 0.5                                                        */
    xtnbreg `outcome' `model05' if(town_string == "`x'"), iterate(`iter')
    parmest, saving("`base_dir'/stata/data/attendance_model05_`out'.dta", replace) eform label
    /* Model 1                                                          */
    xtnbreg `outcome' `model1' if(town_string == "`x'"), iterate(`iter')
    parmest, saving("`base_dir'/stata/data/attendance_model1_`out'.dta", replace) eform label
    /* Model 2                                                          */
    if("x'" == "Bishop Auckland")      local x2 = "Whitehaven"
    else if("x'" == "Hartlepool")      local x2 = "Grimsby"
    else if("x'" == "Hemel Hempstead") local x2 = "Warwick"
    else if("x'" == "Newark")          local x2 = "Southport"
    else if("x'" == "Rochdale")        local x2 = "Rotherham"
    xtnbreg `outcome' `model2' if(town_string == "`x'" | town_string == "`x2'"), iterate(`iter')
    parmest, saving("`base_dir'/stata/data/attendance_model2_`out'.dta", replace) eform label
    use "`base_dir'/stata/data/attendance_model0_`out'.dta", clear
    gen model = "Model 0"
    tempfile t
    save `t', replace
    use "`base_dir'/stata/data/attendance_model05_`out'.dta", clear
    gen model = "Model 0.5"
    append using `t'
    save `t', replace
    use "`base_dir'/stata/data/attendance_model1_`out'.dta", clear
    gen model = "Model 1"
    append using `t'
    save `t', replace
    use "`base_dir'/stata/data/attendance_model2_`out'.dta", clear
    gen model = "Model 2"
    append using `t'
    gen town = "`x'"
    if("`x'" == "Bishop Auckland"){
        tempfile results
        save `results', replace
    }
    else{
        append using `results'
        save `results', replace
    }
}
gen indicator     = "emergency admissions"
gen sub_indicator = "all"
save "`base_dir'/stata/data/results_admissions.dta", replace
append using "`base_dir'/stata/data/results_attendance.dta"

/* Add in meaningful terms for the parameters which are now all       */
/* obfuscated                                                         */
replace label = "(Intercept)"                 if(parm == "Constant")
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

save "`base_dir'/stata/data/results.dta", replace
outsheet indicator sub_indicator town model label estimate stderr p min95 max95 using "`base_dir'/stata/data/txt/negbin_check.csv", replace comma
