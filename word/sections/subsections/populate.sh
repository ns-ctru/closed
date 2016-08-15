#!/bin/bash
##
## Uses the template.Rmd to generate files for each outcome

## Mode of Arrival Any
sed -e 's/template/mode\.of\.arrival\.any/g' \
    -e 's/data_frame/ed_attendances_by_mode/g' \
    -e 's/Measure/ed attendances/g' \
    -e 's/sub_measure/any/g' \
    -e 's/DESCRIPTION/Mode of Arrival (Any)/g' \
    template.Rmd > mode_of_arrival_any.Rmd

## Mode of Arrival Ambulance
sed -e 's/template/mode\.of\.arrival\.ambulance/g' \
    -e 's/data_frame/ed_attendances_by_mode/g' \
    -e 's/Measure/ed attendances/g' \
    -e 's/sub_measure/ambulance/g' \
    -e 's/DESCRIPTION/Mode of Arrival (Ambulance)/g' \
    template.Rmd > mode_of_arrival_ambulance.Rmd

## Mode of Arrival Other
sed -e 's/template/mode\.of\.arrival\.other/g' \
    -e 's/data_frame/ed_attendances_by_mode/g' \
    -e 's/Measure/ed attendances/g' \
    -e 's/sub_measure/other/g' \
    -e 's/DESCRIPTION/Mode of Arrival (Other)/g' \
    template.Rmd > mode_of_arrival_other.Rmd

## Unnecessary Attendances
sed -e 's/template/unnecessary.attendance/g' \
    -e 's/data_frame/unnecessary_ed_attendances/g' \
    -e 's/Measure/unnecessary ed attendances/g' \
    -e 's/sub_measure/all/g' \
    -e 's/DESCRIPTION/Unnecessary ED Attendances/g' \
    template.Rmd > unnecessary_attendance.Rmd

## Emergency Admissions - All
sed -e 's/template/all.emergency.admissions.all/g' \
    -e 's/data_frame/emergency_admissions/g' \
    -e 's/Measure/all emergency admissions/g' \
    -e 's/sub_measure/all/g' \
    -e 's/DESCRIPTION/Emergency Admissions (All)/g' \
    template.Rmd > all_emergency_admissions_all.Rmd

## Avoidable Emergency Admissions - Any
sed -e 's/template/avoidable.emergency.admissions.any/g' \
    -e 's/data_frame/emergency_admissions/g' \
    -e 's/Measure/avoidable emergency admissions/g' \
    -e 's/sub_measure/any/g' \
    -e 's/DESCRIPTION/Avoidable Emergency Admissions (Any)/g' \
    template.Rmd > avoidable_emergency_admissions_any.Rmd

## Avoidable Emergency Admissions - Chest Pain
sed -e 's/template/avoidable.emergency.admissions.chest/g' \
    -e 's/data_frame/emergency_admissions/g' \
    -e 's/Measure/avoidable emergency admissions/g' \
    -e 's/sub_measure/chest/g' \
    -e 's/DESCRIPTION/Avoidable Emergency Admissions (Non-Specific Chest Pain)/g' \
    template.Rmd > avoidable_emergency_admissions_chest.Rmd

## Attendances Admitted - Admitted
sed -e 's/template/ed.attendances.admitted.admitted/g' \
    -e 's/data_frame/ed_attendances_admitted/g' \
    -e 's/Measure/ed attendances admitted/g' \
    -e 's/sub_measure/admitted/g' \
    -e 's/DESCRIPTION/Avoidable Emergency Admissions (Non-Specific Chest Pain)/g' \
    template.Rmd > ed_attendances_admitted_admitted.Rmd

## Attendances Admitted - Fraction Admitted
sed -e 's/template/ed.attendances.admitted.fraction.admitted/g' \
    -e 's/data_frame/ed_attendances_admitted/g' \
    -e 's/Measure/ed attendances admitted/g' \
    -e 's/sub_measure/fraction admitted/g' \
    -e 's/DESCRIPTION/Avoidable Emergency Admissions (Non-Specific Chest Pain)/g' \
    template.Rmd > ed_attendances_admitted_fraction_admitted.Rmd

## Critical Care - All
sed -e 's/template/critical.care.cips.all/g' \
    -e 's/data_frame/critical_care_cips/g' \
    -e 's/Measure/critcial care stays/g' \
    -e 's/sub_measure/all/g' \
    -e 's/DESCRIPTION/Critical Care Stays (All)/g' \
    template.Rmd > critical_care_cips_all.Rmd

## Critical Care - Critical
sed -e 's/template/critical.care.cips.critical/g' \
    -e 's/data_frame/critical_care_cips/g' \
    -e 's/Measure/critcial care stays/g' \
    -e 's/sub_measure/critical care/g' \
    -e 's/DESCRIPTION/Critical Care Stays (Critical)/g' \
    template.Rmd > critical_care_cips_critical.Rmd

## Critical Care - Fraction
sed -e 's/template/critical.care.cips.fraction/g' \
    -e 's/data_frame/critical_care_cips/g' \
    -e 's/Measure/critcial care stays/g' \
    -e 's/sub_measure/fraction critical care/g' \
    -e 's/DESCRIPTION/Critical Care Stays (Fraction Critical)/g' \
    template.Rmd > critical_care_cips_fraction.Rmd

## Length of Stay - Mean
sed -e 's/template/length.of.stay.mean/g' \
    -e 's/data_frame/length_of_stay/g' \
    -e 's/Measure/length of stay/g' \
    -e 's/sub_measure/mean/g' \
    -e 's/DESCRIPTION/Length of Stay (Mean)/g' \
    template.Rmd > length_of_stay_mean.Rmd

## Length of Stay - Median
sed -e 's/template/length.of.stay.median/g' \
    -e 's/data_frame/length_of_stay/g' \
    -e 's/Measure/length of stay/g' \
    -e 's/sub_measure/median/g' \
    -e 's/DESCRIPTION/Length of Stay (Median)/g' \
    template.Rmd > length_of_stay_median.Rmd

## Case Fatality Ratio - Any
sed -e 's/template/case.fatality.ratio.any/g' \
    -e 's/data_frame/case_fatality/g' \
    -e 's/Measure/case fatality ratio/g' \
    -e 's/sub_measure/any/g' \
    -e 's/DESCRIPTION/Case Fatality Ratio (Any)/g' \
    template.Rmd > case_fatality_ratio_any.Rmd

## Case Fatality Ratio - Acute Heart
sed -e 's/template/case.fatality.ratio.acute.heart.failure/g' \
    -e 's/data_frame/case_fatality/g' \
    -e 's/Measure/case fatality ratio/g' \
    -e 's/sub_measure/acute heart failure/g' \
    -e 's/DESCRIPTION/Case Fatality Ratio (Acute Heart Failure)/g' \
    template.Rmd > case_fatality_ratio_acute_heart_failure.Rmd


## Case Fatality Ratio - Stroke CVA
sed -e 's/template/case.fatality.ratio.stroke.cva/g' \
    -e 's/data_frame/case_fatality/g' \
    -e 's/Measure/case fatality ratio/g' \
    -e 's/sub_measure/stroke cva/g' \
    -e 's/DESCRIPTION/Case Fatality Ratio (Stroke CVA)/g' \
    template.Rmd > case_fatality_ratio_stroke_cva.Rmd

## Ambulance Mean Times - Call to Destination
sed -e 's/template/ambulance.mean.times.call.to.dest/g' \
    -e 's/data_frame/ambulance_mean_times/g' \
    -e 's/Measure/ambulance mean times/g' \
    -e 's/sub_measure/call_to_dest/g' \
    -e 's/DESCRIPTION/Ambulance Mean Times - Call to Destination/g' \
    template.Rmd > ambulance_mean_times_call_to_dest.Rmd

## Ambulance Mean Times - Call to Scene (Any)
sed -e 's/template/ambulance.mean.times.call.to.scene.any/g' \
    -e 's/data_frame/ambulance_mean_times/g' \
    -e 's/Measure/ambulance mean times/g' \
    -e 's/sub_measure/call_to_scene_any/g' \
    -e 's/DESCRIPTION/Ambulance Mean Times - Call to Scene (Any)/g' \
    template.Rmd > ambulance_mean_times_call_to_scene_any.Rmd

## Ambulance Mean Times - Call to Scene (Conveying)
sed -e 's/template/ambulance.mean.times.call.to.scene.conveying/g' \
    -e 's/data_frame/ambulance_mean_times/g' \
    -e 's/Measure/ambulance mean times/g' \
    -e 's/sub_measure/call_to_scene_conveying/g' \
    -e 's/DESCRIPTION/Ambulance Mean Times - Call to Scene (Conveying)/g' \
    template.Rmd > ambulance_mean_times_call_to_scene_conveying.Rmd

## Ambulance Mean Times - Scene to Destination
sed -e 's/template/ambulance.mean.times.scene.to.dest/g' \
    -e 's/data_frame/ambulance_mean_times/g' \
    -e 's/Measure/ambulance mean times/g' \
    -e 's/sub_measure/scene_to_dest/g' \
    -e 's/DESCRIPTION/Ambulance Mean Times - Scene to Destination/g' \
    template.Rmd > ambulance_mean_times_scene_to_dest.Rmd

## Ambulance Mean Times - Scene to Destination
sed -e 's/template/ambulance.mean.times.dest.to.clear/g' \
    -e 's/data_frame/ambulance_mean_times/g' \
    -e 's/Measure/ambulance mean times/g' \
    -e 's/sub_measure/dest_to_clear/g' \
    -e 's/DESCRIPTION/Ambulance Mean Times - Destination to Clear/g' \
    template.Rmd > ambulance_mean_times_dest_to_clear.Rmd

## Ambulance Non Conveyances - Green Calls
sed -e 's/template/ambulance.non.conveyances.green.calls/g' \
    -e 's/data_frame/amb_non_conveyance/g' \
    -e 's/Measure/ambulance non-conveyance/g' \
    -e 's/sub_measure/green calls/g' \
    -e 's/DESCRIPTION/Ambulance Non Conveyances - Green Calls/g' \
    template.Rmd > ambulance_non_conveyances_green_calls.Rmd

## Ambulance Non Conveyances - Green Calls Not Conveyed
sed -e 's/template/ambulance.non.conveyances.green.calls.non.conveyed/g' \
    -e 's/data_frame/amb_non_conveyance/g' \
    -e 's/Measure/ambulance non-conveyance/g' \
    -e 's/sub_measure/not conveyed green calls/g' \
    -e 's/DESCRIPTION/Ambulance Non Conveyances - Green Calls (Non-Conveyed)/g' \
    template.Rmd > ambulance_non_conveyances_green_calls_non_conveyed.Rmd

## Ambulance Non Conveyances - Green Calls Fraction Not Conveyed
sed -e 's/template/ambulance.non.conveyances.fraction.not.conveyed/g' \
    -e 's/data_frame/amb_non_conveyance/g' \
    -e 's/Measure/ambulance non-conveyance/g' \
    -e 's/sub_measure/fraction not conveyed/g' \
    -e 's/DESCRIPTION/Ambulance Non Conveyances - Green Calls (Fraction Non-Conveyed)/g' \
    template.Rmd > ambulance_non_conveyances_fraction_not_conveyed.Rmd

## Ambulance Red Calls - Hospital Transfers
sed -e 's/template/ambulance.red.calls.hospital.transfers/g' \
    -e 's/data_frame/amb_red_calls/g' \
    -e 's/Measure/ambulance red calls/g' \
    -e 's/sub_measure/hospital transfers/g' \
    -e 's/DESCRIPTION/Ambulance Red Calls - Hospital Transfers/g' \
    template.Rmd > ambulance_red_calls_hospital_transfers.Rmd

## Ambulance Red Calls - Hospital Transfers
sed -e 's/template/ambulance.red.calls.hospital.transfers/g' \
    -e 's/data_frame/amb_red_calls/g' \
    -e 's/Measure/ambulance red calls/g' \
    -e 's/sub_measure/hospital transfers/g' \
    -e 's/DESCRIPTION/Ambulance Red Calls - Hospital Transfers/g' \
    template.Rmd > ambulance_red_calls_hospital_transfers.Rmd

## Ambulance Red Calls - Hospital Transfers
sed -e 's/template/ambulance.red.calls.total/g' \
    -e 's/data_frame/amb_red_calls/g' \
    -e 's/Measure/ambulance red calls/g' \
    -e 's/sub_measure/total/g' \
    -e 's/DESCRIPTION/Ambulance Red Calls - Total/g' \
    template.Rmd > ambulance_red_calls_total.Rmd
