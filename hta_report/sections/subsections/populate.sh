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
    template.Rmd > mode_of_arrival_ambulance.Rmd

## Measure     : Mode of Arrival
## Sub-Measure : Other
sed -e 's/template/mode.of.arrival.other/g' \
    -e 's/Measure/ed attendances/g' \
    -e 's/sub_measure/other/g' \
    -e 's/data_frame/ed_attendances_by_mode/g' \
    -e 's/DESCRIPTION/mode of attendance by any means/g' \
    template.Rmd > mode_of_arrival_other.Rmd

## Measure     : Unnecssary ED Attendances
## Sub-Measure : all
sed -e 's/template/unnecessary.attendance/g' \
    -e 's/Measure/unnecessary ed attendances/g' \
    -e 's/sub_measure/all/g' \
    -e 's/data_frame/unnecessary_ed_attendances/g' \
    -e 's/DESCRIPTION/unnecessary ED attendances/g' \
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
    template.Rmd > length_of_stay_mean.Rmd

## Measure     : Length of Stay
## Sub-Measure : Median
sed -e 's/template/length.of.stay.median/g' \
    -e 's/Measure/length of stay/g' \
    -e 's/sub_measure/median/g' \
    -e 's/data_frame/length_of_stay/g' \
    -e 's/DESCRIPTION/median length of stay/g' \
    template.Rmd > length_of_stay_median.Rmd

## Measure     : Case Fatality Ratio
## Sub-Measure : Any
sed -e 's/template/case.fatality.ratio.any/g' \
    -e 's/Measure/case fatality ratio/g' \
    -e 's/sub_measure/any/g' \
    -e 's/data_frame/case_fatality/g' \
    -e 's/DESCRIPTION/case fatality ratio (all conditions)/g' \
    -e 's/ts\.plot\.opts$tidy/FALSE/g' \
    template.Rmd > case_fatality_ratio_any.Rmd

## Measure     : Case Fatality Ratio
## Sub-Measure : Acute Heart Failure
sed -e 's/template/case.fatality.ratio.acute.heart.failure/g' \
    -e 's/Measure/case fatality ratio/g' \
    -e 's/sub_measure/acute heart failure/g' \
    -e 's/data_frame/case_fatality/g' \
    -e 's/DESCRIPTION/case fatality ratio (Acute Heart Failure)/g' \
    -e 's/ts\.plot\.opts$tidy/FALSE/g' \
    template.Rmd > case_fatality_ratio_acute_heart_failure.Rmd

## Measure     : Case Fatality Ratio
## Sub-Measure : Stroke CVA
sed -e 's/template/case.fatality.ratio.stroke.cva/g' \
    -e 's/Measure/case fatality ratio/g' \
    -e 's/sub_measure/stroke cva/g' \
    -e 's/data_frame/case_fatality/g' \
    -e 's/DESCRIPTION/case fatality ratio (Stroke CVA)/g' \
    -e 's/ts\.plot\.opts$tidy/FALSE/g' \
    template.Rmd > case_fatality_ratio_stroke_cva.Rmd

## Measure     : Ambulance Mean Times
## Sub-Measure : call to destination
sed -e 's/template/ambulance.mean.times.call.to.dest/g' \
    -e 's/Measure/ambulance mean times/g' \
    -e 's/sub_measure/call_to_dest/g' \
    -e 's/data_frame/ambulance_mean_times/g' \
    -e 's/DESCRIPTION/mean time from call to destination/g' \
    template.Rmd > ambulance_mean_times_call_to_dest.Rmd

## Measure     : Ambulance Mean Times
## Sub-Measure : call to scene (any)
sed -e 's/template/ambulance.mean.times.call.to.scene.any/g' \
    -e 's/Measure/ambulance mean times/g' \
    -e 's/sub_measure/call_to_scene_any/g' \
    -e 's/data_frame/ambulance_mean_times/g' \
    -e 's/DESCRIPTION/mean time from call to scene (any)/g' \
    template.Rmd > ambulance_mean_times_call_to_scene_any.Rmd

## Measure     : Ambulance Mean Times
## Sub-Measure : call to scene (conveying)
sed -e 's/template/ambulance.mean.times.call.to.scene.conveying/g' \
    -e 's/Measure/ambulance mean times/g' \
    -e 's/sub_measure/call_to_scene_conveying/g' \
    -e 's/data_frame/ambulance_mean_times/g' \
    -e 's/DESCRIPTION/mean time from call to scene (conveying)/g' \
    template.Rmd > ambulance_mean_times_call_to_scene_conveying.Rmd

## Measure     : Ambulance Mean Times
## Sub-Measure : destination to clear
sed -e 's/template/ambulance.mean.times.dest.to.clear/g' \
    -e 's/Measure/ambulance mean times/g' \
    -e 's/sub_measure/dest_to_clear/g' \
    -e 's/data_frame/ambulance_mean_times/g' \
    -e 's/DESCRIPTION/mean time from destination to clear/g' \
    template.Rmd > ambulance_mean_times_dest_to_clear.Rmd

## Measure     : Ambulance Mean Times
## Sub-Measure : scene to destination
sed -e 's/template/ambulance.mean.times.scene.to.dest/g' \
    -e 's/Measure/ambulance mean times/g' \
    -e 's/sub_measure/scene_to_dest/g' \
    -e 's/data_frame/ambulance_mean_times/g' \
    -e 's/DESCRIPTION/mean time from scene to destination/g' \
    template.Rmd > ambulance_mean_times_scene_to_dest.Rmd
