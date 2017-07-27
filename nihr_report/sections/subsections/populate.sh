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
    -e 's/<!-- ###### Case Sites -->/**NB** - For Hartlepool Models with steps (i.e. 1, 2 and 3) include a step at week 31 for some undocumented event/g' \
    template.Rmd > mode_of_arrival_any.Rmd

## Measure     : Mode of Arrival
## Sub-Measure : Ambulance
sed -e 's/template/mode.of.arrival.ambulance/g' \
    -e 's/Measure/ed attendances/g' \
    -e 's/sub_measure/ambulance/g' \
    -e 's/data_frame/ed_attendances_by_mode/g' \
    -e 's/DESCRIPTION/mode of attendance by ambulance means/g' \
    -e 's/<!-- ###### Case Sites -->/**NB** - For Hartlepool Models with steps (i.e. 1, 2 and 3) include a step at week 31 for some undocumented event/g' \
    template.Rmd > mode_of_arrival_ambulance.Rmd

## Measure     : Mode of Arrival
## Sub-Measure : Other
sed -e 's/template/mode.of.arrival.other/g' \
    -e 's/Measure/ed attendances/g' \
    -e 's/sub_measure/other/g' \
    -e 's/data_frame/ed_attendances_by_mode/g' \
    -e 's/DESCRIPTION/mode of attendance by other means/g' \
    -e 's/<!-- ###### Case Sites -->/**NB** - For Hartlepool Models with steps (i.e. 1, 2 and 3) include a step at week 31 for some undocumented event/g' \
    template.Rmd > mode_of_arrival_other.Rmd

## Measure     : Unnecssary ED Attendances
## Sub-Measure : all
sed -e 's/template/unnecessary.attendance/g' \
    -e 's/Measure/unnecessary ed attendances/g' \
    -e 's/sub_measure/all/g' \
    -e 's/data_frame/unnecessary_ed_attendances/g' \
    -e 's/DESCRIPTION/unnecessary ED attendances/g' \
    -e 's/<!-- ###### Case Sites -->/**NB** - For Hartlepool Models with steps (i.e. 1, 2 and 3) include a step at week 31 for some undocumented event/g' \
    template.Rmd > unnecessary_attendance.Rmd

## Measure     : Emergency Admissions
## Sub-Measure : all
sed -e 's/template/all.emergency.admissions.all/g' \
    -e 's/Measure/all emergency admissions/g' \
    -e 's/sub_measure/all/g' \
    -e 's/data_frame/emergency_admissions/g' \
    -e 's/DESCRIPTION/all emergency admissions/g' \
    template.Rmd > all_emergency_admissions_all.Rmd

## Measure     : Avoidable Emergency Admissions
## Sub-Measure : any
sed -e 's/template/avoidable.emergency.admissions.any/g' \
    -e 's/Measure/avoidable emergency admissions/g' \
    -e 's/sub_measure/any/g' \
    -e 's/data_frame/emergency_admissions/g' \
    -e 's/DESCRIPTION/all avoidable emergency admissions/g' \
    template.Rmd > avoidable_emergency_admissions_any.Rmd

## Measure     : Avoidable Emergency Admissions
## Sub-Measure : non-specific chest pain
sed -e 's/template/avoidable.emergency.admissions.chest.pain/g' \
    -e 's/Measure/avoidable emergency admissions/g' \
    -e 's/sub_measure/non-specific chest pain/g' \
    -e 's/data_frame/emergency_admissions/g' \
    -e 's/DESCRIPTION/all avoidable emergency admissions/g' \
    template.Rmd > avoidable_emergency_admissions_non_specific_chest_pain.Rmd

## Measure     : ED Attendances Admitted
## Sub-Measure : all
sed -e 's/template/ed.attendances.admitted.all/g' \
    -e 's/Measure/ed attendances admitted/g' \
    -e 's/sub_measure/all/g' \
    -e 's/data_frame/ed_attendances_admitted/g' \
    -e 's/DESCRIPTION/all ED attendances admitted/g' \
    template.Rmd > ed_attendances_admitted_all.Rmd

## Measure     : ED Attendances Admitted
## Sub-Measure : admitted
sed -e 's/template/ed.attendances.admitted.admitted/g' \
    -e 's/Measure/ed attendances admitted/g' \
    -e 's/sub_measure/admitted/g' \
    -e 's/data_frame/ed_attendances_admitted/g' \
    -e 's/DESCRIPTION/ED attendances admitted/g' \
    template.Rmd > ed_attendances_admitted_admitted.Rmd

## Measure     : ED Attendances Admitted
## Sub-Measure : fraction admitted
sed -e 's/template/ed.attendances.admitted.fraction.admitted/g' \
    -e 's/Measure/ed attendances admitted/g' \
    -e 's/sub_measure/fraction admitted/g' \
    -e 's/data_frame/ed_attendances_admitted/g' \
    -e 's/DESCRIPTION/fraction of ED attendances admitted/g' \
    template.Rmd > ed_attendances_admitted_fraction_admitted.Rmd

## Measure     : Critical Care
## Sub-Measure : all
sed -e 's/template/critical.care.cips.all/g' \
    -e 's/Measure/critical care stays/g' \
    -e 's/sub_measure/all/g' \
    -e 's/data_frame/critical_care_cips/g' \
    -e 's/DESCRIPTION/all critical care stays/g' \
    template.Rmd > critical_care_cips_all.Rmd

## Measure     : Critical Care
## Sub-Measure : critical
sed -e 's/template/critical.care.cips.critical/g' \
    -e 's/Measure/critical care stays/g' \
    -e 's/sub_measure/critical care/g' \
    -e 's/data_frame/critical_care_cips/g' \
    -e 's/DESCRIPTION/critical critical care stays/g' \
    template.Rmd > critical_care_cips_critical.Rmd

## Measure     : Critical Care
## Sub-Measure : fraction
sed -e 's/template/critical.care.cips.fraction/g' \
    -e 's/Measure/critical care stays/g' \
    -e 's/sub_measure/fraction critical care/g' \
    -e 's/data_frame/critical_care_cips/g' \
    -e 's/DESCRIPTION/fraction of critical care stays/g' \
    template.Rmd > critical_care_cips_fraction.Rmd

## Measure     : Length of Stay
## Sub-Measure : Mean
sed -e 's/template/length.of.stay.mean/g' \
    -e 's/Measure/length of stay/g' \
    -e 's/sub_measure/mean/g' \
    -e 's/data_frame/length_of_stay/g' \
    -e 's/DESCRIPTION/mean length of stay/g' \
    -e 's/count/proportion/g' \
    template.Rmd > length_of_stay_mean.Rmd

## Measure     : Length of Stay
## Sub-Measure : Median
sed -e 's/template/length.of.stay.median/g' \
    -e 's/Measure/length of stay/g' \
    -e 's/sub_measure/median/g' \
    -e 's/data_frame/length_of_stay/g' \
    -e 's/DESCRIPTION/median length of stay/g' \
    -e 's/count/proportion/g' \
    template.Rmd > length_of_stay_median.Rmd

## Measure     : Case Fatality Ratio
## Sub-Measure : Any
sed -e 's/template/case.fatality.ratio.any/g' \
    -e 's/Measure/sec case fatality 7 days/g' \
    -e 's/sub_measure/any/g' \
    -e 's/data_frame/sec_case_fatality_7days/g' \
    -e 's/DESCRIPTION/case fatality ratio (all conditions)/g' \
    -e 's/ts\.plot\.opts$tidy/FALSE/g' \
    -e 's/count/proportion/g' \
    template.Rmd > case_fatality_ratio_any.Rmd

## Measure     : Case Fatality Ratio
## Sub-Measure : Acute Heart Failure
sed -e 's/template/case.fatality.ratio.acute.heart.failure/g' \
    -e 's/Measure/sec case fatality 7 days/g' \
    -e 's/sub_measure/acute heart failure/g' \
    -e 's/data_frame/sec_case_fatality_7days/g' \
    -e 's/DESCRIPTION/case fatality ratio (Acute Heart Failure)/g' \
    -e 's/ts\.plot\.opts$tidy/FALSE/g' \
    -e 's/count/proportion/g' \
    template.Rmd > case_fatality_ratio_acute_heart_failure.Rmd

## Measure     : Case Fatality Ratio
## Sub-Measure : Falls
sed -e 's/template/case.fatality.ratio./g' \
    -e 's/Measure/sec case fatality 7 days/g' \
    -e 's/sub_measure/falls/g' \
    -e 's/data_frame/sec_case_fatality_7days/g' \
    -e 's/DESCRIPTION/case fatality ratio (Falls < 75)/g' \
    -e 's/ts\.plot\.opts$tidy/FALSE/g' \
    -e 's/count/proportion/g' \
    template.Rmd > case_fatality_ratio_falls.Rmd

## Measure     : Case Fatality Ratio
## Sub-Measure : Myocardial Infarction
sed -e 's/template/case.fatality.ratio.myocardial.infarction/g' \
    -e 's/Measure/sec case fatality 7 days/g' \
    -e 's/sub_measure/myocardial infarction/g' \
    -e 's/data_frame/sec_case_fatality_7days/g' \
    -e 's/DESCRIPTION/case fatality ratio (Myocardial Infarction)/g' \
    -e 's/ts\.plot\.opts$tidy/FALSE/g' \
    -e 's/count/proportion/g' \
    template.Rmd > case_fatality_ratio_myocardial_infarction.Rmd

## Measure     : Case Fatality Ratio
## Sub-Measure : Any Trauma Sec
sed -e 's/template/case.fatality.ratio.any.trauma.sec/g' \
    -e 's/Measure/sec case fatality 7 days/g' \
    -e 's/sub_measure/any trauma sec/g' \
    -e 's/data_frame/sec_case_fatality_7days/g' \
    -e 's/DESCRIPTION/case fatality ratio (Any Trauma)/g' \
    -e 's/ts\.plot\.opts$tidy/FALSE/g' \
    -e 's/count/proportion/g' \
    template.Rmd > case_fatality_ratio_any_trauma_sec.Rmd

## Measure     : Case Fatality Ratio
## Sub-Measure : Serious Head Injury
# sed -e 's/template/case.fatality.ratio.serious.head.injury/g' \
#     -e 's/Measure/sec case fatality 7 days/g' \
#     -e 's/sub_measure/serious head injury/g' \
#     -e 's/data_frame/case_fatality/g' \
#     -e 's/DESCRIPTION/case fatality ratio (Serious Head Injury)/g' \
#     -e 's/ts\.plot\.opts$tidy/FALSE/g' \
#     -e 's/count/proportion/g' \
#     template.Rmd > case_fatality_ratio_serious_head_injury.Rmd

## Measure     : Case Fatality Ratio
## Sub-Measure : Road Traffic Accient
sed -e 's/template/case.fatality.ratio.road.traffic.accident/g' \
    -e 's/Measure/sec case fatality 7 days/g' \
    -e 's/sub_measure/road traffic accident/g' \
    -e 's/data_frame/sec_case_fatality_7days/g' \
    -e 's/DESCRIPTION/case fatality ratio (Road Traffic Accident)/g' \
    -e 's/ts\.plot\.opts$tidy/FALSE/g' \
    -e 's/count/proportion/g' \
    template.Rmd > case_fatality_ratio_road_traffic_accident.Rmd

## Measure     : Case Fatality Ratio
## Sub-Measure : Stroke CVA
sed -e 's/template/case.fatality.ratio.stroke.cva/g' \
    -e 's/Measure/sec case fatality 7 days/g' \
    -e 's/sub_measure/stroke cva/g' \
    -e 's/data_frame/sec_case_fatality_7days/g' \
    -e 's/DESCRIPTION/case fatality ratio (Stroke CVA)/g' \
    -e 's/ts\.plot\.opts$tidy/FALSE/g' \
    -e 's/count/proportion/g' \
    template.Rmd > case_fatality_ratio_stroke_cva.Rmd

## Measure     : Ambulance Mean Times
## Sub-Measure : call to destination
sed -e 's/template/ambulance.mean.times.call.to.dest/g' \
    -e 's/Measure/ambulance mean times/g' \
    -e 's/sub_measure/call to dest/g' \
    -e 's/data_frame/amb_mean_times/g' \
    -e 's/DESCRIPTION/mean time from call to destination/g' \
    -e 's/count/proportion/g' \
    -e "s/ma.model       = 'Model 2'/ma.model       = 'Model 1'/g" \
    -e "s/ambulance.mean.times.call.to.dest.forest.model2, echo = FALSE, cache = FALSE, results = 'markup', eval = TRUE/ambulance.mean.times.call.to.dest.forest.model2, echo = FALSE, cache = FALSE, results = 'markup', eval = FALSE/g" \
    template.Rmd > ambulance_mean_times_call_to_dest.Rmd

## Measure     : Ambulance Mean Times
## Sub-Measure : call to scene (any)
sed -e 's/template/ambulance.mean.times.call.to.scene.any/g' \
    -e 's/Measure/ambulance mean times/g' \
    -e 's/sub_measure/call to scene any/g' \
    -e 's/data_frame/amb_mean_times/g' \
    -e 's/DESCRIPTION/mean time from call to scene (any)/g' \
    -e 's/count/proportion/g' \
    template.Rmd > ambulance_mean_times_call_to_scene_any.Rmd

## Measure     : Ambulance Mean Times
## Sub-Measure : call to scene (conveying)
sed -e 's/template/ambulance.mean.times.call.to.scene.conveying/g' \
    -e 's/Measure/ambulance mean times/g' \
    -e 's/sub_measure/call to scene conveying/g' \
    -e 's/data_frame/amb_mean_times/g' \
    -e 's/DESCRIPTION/mean time from call to scene (conveying)/g' \
    -e 's/count/proportion/g' \
    template.Rmd > ambulance_mean_times_call_to_scene_conveying.Rmd

## Measure     : Ambulance Mean Times
## Sub-Measure : destination to clear
sed -e 's/template/ambulance.mean.times.dest.to.clear/g' \
    -e 's/Measure/ambulance mean times/g' \
    -e 's/sub_measure/dest to clear/g' \
    -e 's/data_frame/amb_mean_times/g' \
    -e 's/DESCRIPTION/mean time from destination to clear/g' \
    -e 's/count/proportion/g' \
    template.Rmd > ambulance_mean_times_dest_to_clear.Rmd

## Measure     : Ambulance Mean Times
## Sub-Measure : scene to destination
sed -e 's/template/ambulance.mean.times.scene.to.dest/g' \
    -e 's/Measure/ambulance mean times/g' \
    -e 's/sub_measure/scene to dest/g' \
    -e 's/data_frame/amb_mean_times/g' \
    -e 's/DESCRIPTION/mean time from scene to destination/g' \
    -e "s/r ambulance.mean.times.scene.to.dest.forest.model4, echo = FALSE, cache = FALSE, results = 'markup', eval = TRUE/r ambulance.mean.times.scene.to.dest.forest.model4, echo = FALSE, cache = FALSE, results = 'markup', eval = FALSE/g" \
    -e 's/count/proportion/g' \
    template.Rmd > ambulance_mean_times_scene_to_dest.Rmd

## Measure     : Ambulance Non Conveyances (green calls)
## Sub-Measure : green calls
sed -e 's/template/ambulance.non.conveyances.green.calls/g' \
    -e 's/Measure/ambulance green calls/g' \
    -e 's/sub_measure/green calls/g' \
    -e 's/data_frame/amb_green_calls/g' \
    -e 's/DESCRIPTION/ambulance green calls/g' \
    template.Rmd > ambulance_non_conveyances_green_calls.Rmd

## Measure     : Ambulance Non Conveyances (green calls)
## Sub-Measure : green calls not conveyed
sed -e 's/template/ambulance.non.conveyances.green.calls.non.conveyed/g' \
    -e 's/Measure/ambulance green calls/g' \
    -e 's/sub_measure/not conveyed green calls/g' \
    -e 's/data_frame/amb_green_calls/g' \
    -e 's/DESCRIPTION/non-conveyed ambulance green calls/g' \
    template.Rmd > ambulance_non_conveyances_green_calls_non_conveyed.Rmd

## Measure     : Ambulance Non Conveyances (green calls)
## Sub-Measure : hospital transfers
sed -e 's/template/ambulance.non.conveyances.green.calls/g' \
    -e 's/Measure/ambulance green calls/g' \
    -e 's/sub_measure/hospital transfers/g' \
    -e 's/data_frame/amb_green_calls/g' \
    -e 's/DESCRIPTION/ambulance green calls hospital transfers/g' \
    template.Rmd > ambulance_non_conveyances_green_calls_hospital_transfers.Rmd

## Measure     : Ambulance Non Conveyances (green calls)
## Sub-Measure : fraction not conveyed
sed -e 's/template/ambulance.non.conveyances.fraction.not.conveyed/g' \
    -e 's/Measure/ambulance green calls/g' \
    -e 's/sub_measure/fraction not conveyed/g' \
    -e 's/data_frame/amb_green_calls/g' \
    -e 's/DESCRIPTION/ambulance green calls/g' \
    template.Rmd > ambulance_non_conveyances_fraction_not_conveyed.Rmd


## Measure     : Red Calls (Hospital Transfers)
## Sub-Measure : red calls
sed -e 's/template/ambulance.red.calls.hospital.transfers/g' \
    -e 's/Measure/ambulance red calls/g' \
    -e 's/sub_measure/hospital transfers/g' \
    -e 's/data_frame/amb_red_calls/g' \
    -e 's/DESCRIPTION/ambulance red calls (hospital transfers)/g' \
    -e "s/ambulance\.red\.calls\.hospital\.transfers\.forest\.model6, echo = FALSE, cache = FALSE, results = 'markup', eval = TRUE/ambulance\.red\.calls\.hospital\.transfers\.forest\.model6, echo = FALSE, cache = FALSE, results = 'markup', eval = FALSE/g" \
    template.Rmd > ambulance_red_calls_hospital_transfers.Rmd

## Measure     : Red Calls (Hospital Transfers)
## Sub-Measure : total
sed -e 's/template/ambulance.red.calls.total/g' \
    -e 's/Measure/ambulance red calls/g' \
    -e 's/sub_measure/total/g' \
    -e 's/data_frame/amb_red_calls/g' \
    -e 's/DESCRIPTION/ambulance red calls (total)/g' \
    template.Rmd > ambulance_red_calls_total.Rmd

## Measure     : Hospital Transfers
## Sub-Measure : stays with transfers
sed -e 's/template/hospital.transfers.stays.with.transfer/g' \
    -e 's/Measure/hospital transfers/g' \
    -e 's/sub_measure/stays with transfer/g' \
    -e 's/data_frame/hospital_transfers/g' \
    -e 's/DESCRIPTION/hospital stays with transfers/g' \
    template.Rmd > hospital_transfers_stays_with_transfers.Rmd

## Measure     : Hospital Transfers
## Sub-Measure : all stays
sed -e 's/template/hospital.transfers.all.stays/g' \
    -e 's/Measure/hospital transfers/g' \
    -e 's/sub_measure/all stays/g' \
    -e 's/data_frame/hospital_transfers/g' \
    -e 's/DESCRIPTION/all hospital stays/g' \
    template.Rmd > hospital_transfers_all_stays.Rmd

## Measure     : Hospital Transfers
## Sub-Measure : fraction with transfers
sed -e 's/template/hospital.transfers.proportion.with.transfer/g' \
    -e 's/Measure/hospital transfers/g' \
    -e 's/sub_measure/fraction with transfer/g' \
    -e 's/data_frame/hospital_transfers/g' \
    -e 's/DESCRIPTION/fraction of hospital stays with transfers/g' \
    -e 's/count/proportion/g' \
    template.Rmd > hospital_transfers_proportion_with_transfers.Rmd

## Measure     : Case Fatality 7 days
## Sub-Measure : any sec
sed -e 's/template/case.fatality.ratio.7days.any.sec/g' \
    -e 's/Measure/sec case fatality 7 days/g' \
    -e 's/sub_measure/any sec/g' \
    -e 's/data_frame/sec_case_fatality_7days/g' \
    -e 's/DESCRIPTION/Case Fatality at 7 days (any sec)/g' \
    -e 's/count/proportion/g' \
    template.Rmd > case_fatality_ratio_7days_any_sec.Rmd

## Measure     : Case Fatality 7 days
## Sub-Measure : any single sec
sed -e 's/template/case.fatality.ratio.7days.any.single.sec/g' \
    -e 's/Measure/sec case fatality 7 days/g' \
    -e 's/sub_measure/any single sec/g' \
    -e 's/data_frame/sec_case_fatality_7days/g' \
    -e 's/DESCRIPTION/Case Fatality at 7 days (any sec)/g' \
    -e 's/count/proportion/g' \
    template.Rmd > case_fatality_ratio_7days_any_single_sec.Rmd

## Measure     : All Deaths 7 days
## Sub-Measure : any sec
sed -e 's/template/sec.deaths.all.7days.any.sec/g' \
    -e 's/Measure/sec deaths all 7days/g' \
    -e 's/sub_measure/any sec/g' \
    -e 's/data_frame/sec_deaths_all_7days/g' \
    -e 's/DESCRIPTION/all Deaths at 7 days (any sec)/g' \
    template.Rmd > sec_deaths_all_7days_any_sec.Rmd

## Measure     : All Deaths in CIPS 7 days
## Sub-Measure : any sec
sed -e 's/template/sec.deaths.in.cips.7days.any.sec/g' \
    -e 's/Measure/sec deaths in cips 7days/g' \
    -e 's/sub_measure/any sec/g' \
    -e 's/data_frame/sec_deaths_in_cips_7days/g' \
    -e 's/DESCRIPTION/in CIPS Deaths at 7 days (any sec)/g' \
    template.Rmd > sec_deaths_in_cips_7days_any_sec.Rmd

## Measure     : All Deaths not in CIPS 7 days
## Sub-Measure : any sec
sed -e 's/template/sec.deaths.not.in.cips.7days.any.sec/g' \
    -e 's/Measure/sec deaths not in cips 7days/g' \
    -e 's/sub_measure/any sec/g' \
    -e 's/data_frame/sec_deaths_not_in_cips_7days/g' \
    -e 's/DESCRIPTION/not in CIPS Deaths at 7 days (any sec)/g' \
    template.Rmd > sec_deaths_not_in_cips_7days_any_sec.Rmd

## Measure     : All Ambulance Calls
## Sub-Measure : Total
sed -e 's/template/ambulance.all.calls.total/g' \
    -e 's/Measure/ambulance all calls/g' \
    -e 's/sub_measure/total/g' \
    -e 's/data_frame/amb_all_calls/g' \
    -e 's/DESCRIPTION/All Ambulance Calls/g' \
    template.Rmd > ambulance_all_calls_total.Rmd
