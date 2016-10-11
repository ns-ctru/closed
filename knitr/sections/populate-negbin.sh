#!/bin/bash

## Arrival by Ambulance...
sed 's/model1/model0/g' negbin_template1.Rmd > negbin_arrival_amb_model0.Rmd
sed 's/model1/model0.5/g' negbin_template1.Rmd > negbin_arrival_amb_model0.5.Rmd
cp negbin_template1.Rmd negbin_model1.Rm
sed 's/model1/model2/g' negbin_template1.Rmd > negbin_arrival_amb_model2.Rmd
sed 's/model1/model3\.1/g' negbin_template1.Rmd > negbin_arrival_amb_model3.1.Rmd
sed 's/model1/model3\.2/g' negbin_template1.Rmd > negbin_arrival_amb_model3.2.Rmd
cp negbin_template2.Rmd negbin_model4.Rmd
sed 's/model4/model5/g'    negbin_template2.Rmd > negbin_arrival_amb_model5.Rmd
sed 's/model1/model6\.1/g' negbin_template1.Rmd > negbin_arrival_amb_model6.1.Rmd
sed 's/model1/model6\.2/g' negbin_template1.Rmd > negbin_arrival_amb_model6.2.Rmd
sed 's/model4/model7/g'    negbin_template2.Rmd > negbin_arrival_amb_model7.Rmd

## Emergency Admissions
# sed 's/model1/model0/g' negbin_template1.Rmd > negbin_em_admissions_model0.Rmd
# sed 's/model1/model0.5/g' negbin_template1.Rmd > negbin_em_admissions_model0.5.Rmd
# cp negbin_template1.Rmd negbin_model1.Rm
# sed 's/model1/model2/g' negbin_template1.Rmd > negbin_em_admissions_model2.Rmd
# sed 's/model1/model3\.1/g' negbin_template1.Rmd > negbin_em_admissions_model3.1.Rmd
# sed 's/model1/model3\.2/g' negbin_template1.Rmd > negbin_em_admissions_model3.2.Rmd
# cp negbin_template2.Rmd negbin_model4.Rmd
# sed 's/model4/model5/g'    negbin_tempalte2.Rmd > negbin_em_admissions_model5.Rmd
# sed 's/model1/model6\.1/g' negbin_template1.Rmd > negbin_em_admissions_model6.1.Rmd
# sed 's/model1/model6\.2/g' negbin_template1.Rmd > negbin_em_admissions_model6.2.Rmd
# sed 's/model4/model7/g'    negbin_template2.Rmd > negbin_em_admissions_model7.Rmd
