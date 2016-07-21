# ClosED - Impact of Closing Emergency Departments

This repository contains code used in a study investigating the impact of closing Emergency Departments in the UK.  It consists primarily of [R](https://www.r-project.org/) code, most of which is bundled into a package.  No data is available due to data sharing policies of the providers.

## Data Providers

Data comes from several sources.

* Hospital Episode Statistics (HES) are provided by [Health and Social Care Information Centre (HSCIC)](https://www.hscic.gov.uk).
* Mortality data is provided by [The Office for National Statistics](https://www.ons.gov.uk/ons/index.html).
* Ambulance data is provided by relevant Ambulance Services.
* The [Department of Transport](https://www.gov.uk/government/organisations/department-for-transport/) provide information on travel times to hospital EDs.

## Data Dictionary

### Abbreviations

There are a number of abbreviations used in the following text.

| Abbreviation | Description       |
|--------------|-------------------|
| AS           | **A**mbulance **S**ervice - data providers for Ambulance conveyance times. |
| ED           | **E**mergency **D**epartment. |
| HSCIC        | [**H**ealth and **S**ocial **C**are **I**nformation **C**ommission](http://www.hscic.gov.uk/) - data providers for A&E attendance and Hospital Episodes. |
| LSOA         | [**L**ower **S**uper **O**utput **A**rea](http://www.datadictionary.nhs.uk/data_dictionary/nhs_business_definitions/l/lower_layer_super_output_area_de.asp?shownav=1) - A small geographical region rouighly standardised on population size. |
| ONS          | [**O**ffice for **N**ational **S**tatistics](https://www.ons.gov.uk/) - Mortality data providers |

**TODO** Check data sources are correct
### Files
The derived datasets (*not* available in this repository) all take the same format, there are two files for each dataset, one for ED site level data (for fitting models 1-5) and one for LSOA level data (for fitting models 6 & 7).  The nomenclature of the files is consistent and contains a date/time stamp indicating when the dataset was derived (see table below).


| File                                                               | Data Source   | Level | Description |
|--------------------------------------------------------------------|---------------|-------|-------------|
| `ed attendances by mode measure - lsoa - 2016-05-24 14.25.Rda`     | HSCIC         | LSOA  | Mode of ED Attendance |
| `ed attendances by mode measure - site - 2016-05-24 14.26.Rda`     | HSCIC         | Site  | Mode of ED Attendance |
| `unnecessary ed attendances measure - lsoa - 2016-05-24 14.26.Rda` | HSCIC         | LSOA  | Unnecessary ED Attendances |
| `unnecessary ed attendances measure - site - 2016-05-24 14.26.Rda` | HSCIC         | Site  | Unnecessary ED Attendances |
| `ed attendances admitted measure - lsoa - 2016-05-24 14.28.Rda`    | HSCIC         | LSOA  | ED Attendances that led to Admission |
| `ed attendances admitted measure - site - 2016-05-24 14.28.Rda`    | HSCIC         | Site  | ED Attendances that led to Admission |
| `emergency admissions measure - lsoa - 2016-05-24 14.30.Rda`       | HSCIC         | LSOA  | Emergency Admissions (incl. non ED Attendances) |
| `emergency admissions measure - site - 2016-05-24 14.31.Rda`       | HSCIC         | Site  | Emergency Admissions (incl. non ED Attendances) |
| `critical care stays measure - lsoa - 2016-06-10 18.17.Rda`        | HSCIC         | LSOA  | Critical Care Stays |
| `critical care stays measure - site - 2016-06-10 18.18.Rda`        | HSCIC         | Site  | Critical Care Stays |
| `length of stay measure - lsoa - 2016-05-24 14.34.Rda`             | HSCIC         | LSOA  | Length of stay for admissions |
| `length of stay measure - site - 2016-05-24 14.35.Rda`             | HSCIC         | Site  | Length of stay for admissions |
| `ambulance mean times measure - lsoa - 2016-06-17 01.21.Rda`       | AS            | LSOA  | Ambulance conveyance times |
| `ambulance mean times measure - site - 2016-06-17 01.23.Rda`       | AS            | Site  | Ambulance conveyance times |
| `case fatality measure - lsoa - 2016-07-15 21.05.Rda`              | ONS Mortality | LSOA  | Mortality rates |
| `case fatality measure - site - 2016-07-15 21.06.Rda`              | ONS Mortality | Site  | Mortality rates |
| `ambulance non-conveyance measure - lsoa - 2016-07-15 20.09.Rda`   | AS            | LSOA  | Non-conveyances |
| `ambulance non-conveyance measure - site - 2016-07-15 20.09.Rda`   | AS            | Site  | Non-conveyances |
| `ambulance red calls measure - lsoa - 2016-07-15 20.11.Rda`        | AS            | LSOA  | 'Red' Ambulance Calls |
| `ambulance red calls measure - site - 2016-07-15 20.11.Rda`        | AS            | Site  | 'Red' Ambulance Calls |

**TODO** Clarify/reocncile the above against the descriptions in the protocol, question the following...

* Are the indicated sources correct?
* Are Length of Stay for Critical Care Stays or all admissions?

### Variables

Each of the above files contains the following fields.

| Field           | Description       | Values              |
|-----------------|-------------------|---------------------|
| `town`          | Town associated with the ED. | `Basingstoke` `Bishop Auckland` `Blackburn` `Carlisle` `Grimsby` `Hartlepool` `Hemel Hempstead` `Newark` `Rochdale` `Rotherham` `Salford` `Salisbury` `Scarborough` `Scunthorpe` `Southport` `Wansbeck` `Warwick` `Whitehaven` `Wigan` `Yeovil` |
| `group`         | Indicator of grouping, control sites were selected to be matched to case sites. | `Bishop Auckland General Hospital` `Hemel Hempstead Hospital` `Newark Hospital` `Rochdale Infirmary` `University Hospital of Hartlepool` |
| `realtivemonth` | Aligned month relative to the closure of the case sites ED. | Range : `1 - 48` |
| `yearmonth`     | Actual year/month. | Range : `2007-03-01 - 2013-07-01` |
| `site_type`     | Site type, all control sites are matched, but one (`matched control`) has undergone documentary analysis to identify steps.  | `intervention` `matched control` `pooled control` |
| `measure`       | The broad indicator measure being considered | See next table for details.
| `sub_measure`   | The specific sub-indicator being considered  | See next table for details.
| `value`         | The observed value for the indicator. | Generally an integer, but in some instances a fraction (i.e. in the range `0 - 1`) |
| `lsoa`          | Lower Super Output Area (only present in LSOA data sets) | **N/A** Too many to list.


#### Measures and Sub-Measures

The following reconciles the description of outcomes given in the study Protocol and [Statistical Analysis Plan (SAP)](https://www.overleaf.com/4191342frhzys) (see pg 4 table 2) with the chosen `measure` and `sub_measure` values used.

| Measure          | Sub-measure       | Description         |
|------------------|-------------------|---------------------|
| `ed attendances` | `any`             | Attendance at ED by any mode. |
| `ed attendances` | `ambulance`       | Attendance at ED by ambulance. |
| `ed attendances` | `other`           | Attendance at ED by modes other than ambulance. |
| `unnecessary ed attendances` | `N/A` | Unnecessary ED attendances. |
| `ed attendances admitted`    | `all`               | All ED attendances. |
| `ed attendances admitted`    | `admitted`          | ED attendances that resulted in admission. |
| `ed attendances admitted`    | `fraction admitted` | Fraction of ED attendances that resulted in admission (i.e. `admitted` / `all`). |
| `all emergency admissions`   | `N/A`               | All Emergency Admissions. |
| `avoidable emergency admissions` | `any`                        | All avoidable Emergency Admissions (sum of the following 13 condition specific admissions). |
| `avoidable emergency admissions` | `acute mental health crisis` | Avoidable Acute Mental Health Crisis admissions. |
| `avoidable emergency admissions` | `angina`                     | Avoidable [Angina](https://en.wikipedia.org/wiki/Angina_pectoris) admissions. |
| `avoidable emergency admissions` | `blocked catheter`           | Avoidable Blocked Catheter admissions. |
| `avoidable emergency admissions` | `cellulitis`                 | Avoidable [Cellulitis](https://en.wikipedia.org/wiki/Cellulitis) admissions. |
| `avoidable emergency admissions` | `copd`                       | Avoidable [Chronic Obstructuve Pulmonary Diseas (COPD)](https://en.wikipedia.org/wiki/COPD) admissions. |
| `avoidable emergency admissions` | `dvt`                        | Avoidable [Deep Veing Thrombosis (DVT)](https://en.wikipedia.org/wiki/Deep_vein_thrombosis) admissions.|
| `avoidable emergency admissions` | `epileptic fit`              | Avoidable [Epileptic Fit](https://en.wikipedia.org/wiki/Epileptic_seizure) admissions. |
| `avoidable emergency admissions` | `falls (>75 years)`          | Avoidable admissions following falls in individuals >75yrs old.
| `avoidable emergency admissions` | `hypoglycaemia`              | Avoidable [Hypoglycaemia](https://en.wikipedia.org/wiki/Hypoglycemia) admissions (common in diabetics). |
| `avoidable emergency admissions` | `minor head injuries`        | Avoidable Minor Head Injury admissions. |
| `avoidable emergency admissions` | `non-specific chest pain`    | Avoidable Non-Specific Chest Pain admissions. |
| `avoidable emergency admissions` | `pyrexial child (<6 years)`  | Avoidable [Pyrexial (Fever)](https://en.wikipedia.org/wiki/Fever) admissions in children <6yrs old. |
| `avoidable emergency admissions` | `urinary tract infection`    | Avoidable [Urinary Tract Infection](https://en.wikipedia.org/wiki/Urinary_tract_infection) admissions|
| `length of stay` | `mean`   | [Mean](https://en.wikipedia.org/wiki/Mean) length of hospital episode. |
| `length of stay` | `median` | [Median](https://en.wikipedia.org/wiki/Median) length of hospital episode. |
| `critical care stays` | `all`							| All? |
| `critical care stays` | `critical care`				| Critical Care Stays. |
| `critical care stays` | `fraction critical care`		| Fraction of ? resulting in Critical Care Stays. |
| `case fatality ratio` | `any`							| Proportion of All condition specific events that resulted in death. |
| `case fatality ratio` | `acute heart failure`			| Proportion of Acute Heart Failure events that resulted in death. |
| `case fatality ratio` | `anaphylaxis`					| Proportion of Anaphlyaxis events that resulted in death. |
| `case fatality ratio` | `asphyxiation`				| Proportion of Asphyxiation events that resulted in death. |
| `case fatality ratio` | `asthma`						| Proportion of Asthma events that resulted in death. |
| `case fatality ratio` | `cardiac arrest`				| Proportion of Cardiac Arrest events that resulted in death. |
| `case fatality ratio` | `falls`						| Proportion of Falls events that resulted in death. |
| `case fatality ratio` | `fractured neck of femur`		| Proportion of Fractured Neck of Femurevents that resulted in death. |
| `case fatality ratio` | `meningitis`					| Proportion of Meningitis events that resulted in death. |
| `case fatality ratio` | `myocardial infarction`		| Proportion of Myocardial Infarction events that resulted in death. |
| `case fatality ratio` | `pregnancy and birth related` | Proportion of Pregnancy and Birth Related events that resulted in death. |
| `case fatality ratio` | `road traffic accident`		| Proportion of Road Traffic Accident events that resulted in death. |
| `case fatality ratio` | `ruptured aortic aneurysm`	| Proportion of Ruptured Aortic Aneurysms events that resulted in death. |
| `case fatality ratio` | `self harm`					| Proportion of Self Harm events that resulted in death. |
| `case fatality ratio` | `septic shock`				| Proportion of Septic Shock events that resulted in death. |
| `case fatality ratio` | `serious head injury`			| Proportion of Serious Head Injury events that resulted in death. |
| `case fatality ratio` | `stroke cva`					| Proportion of Stroke CVA events that resulted in death. |
| `ambulance mean times` | `call_to_dest`				| Time from Call to Destination ED. |
| `ambulance mean times` | `call_to_scene_any`			| Time from Call to Scene for any subsequent action. |
| `ambulance mean times` | `call_to_scene_conveying`	| Time from Call to Scene only if subsequent conveyencing to ED. |
| `ambulance mean times` | `dest_to_clear`				| Time from Destination ED? to Ambulance being clear. |
| `ambulance mean times` | `scene_to_dest`				| Time from Scene to Destination ED. |
| `ambulance non-conveyance` | `fraction not conveyed`    | Fraction of Calls not Conveyed
| `ambulance non-conveyance` | `green calls`              | Green Calls
| `ambulance non-conveyance` | `not conveyed green calls` | Green Calls not Conveyed
| `ambulance red calls` | `hospital transfers`            | Inter-Hospital Transfers
| `ambulance red calls` | `total`                         | Total Red Calls
