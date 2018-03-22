[![ORCiD](https://img.shields.io/badge/ORCiD-0000--0001--8301--6857-green.svg)](https://orcid.org/0000-0001-8301-6857)

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


### Files

The datasets derived  by Tony Stone are *not* available in this repository, nor on the network drive because of the [information governance directives](https://digital.nhs.uk/article/402/Information-Governance) from the [HSCIC](http://content.digital.nhs.uk/).  Instead all data was prepared by Tony Stone who has written an R package ([`rclosed`](https://github.com/tony-stone/rclosed)) and prepared the data on a M$-Windows [Virtual Machine](https://en.wikipedia.org/wiki/Virtual_machine).  This data was transferred via [sFTP](https://en.wikipedia.org/wiki/SSH_File_Transfer_Protocol) ([FTP](https://en.wikipedia.org/wiki/File_Transfer_Protocol) over [SSH](https://en.wikipedia.org/wiki/Secure_Shell))to a secondary Virtual Machine running [CentOS a GNU/Linux distribution](https://www.centos.org/) for subsequent analysis. A common data structure and nomenclature has been used by Tony in deriving these files and there are two files for each dataset, one for ED site level data (for fitting models 1-5) and one for LSOA level data (for fitting models 6 & 7).  The nomenclature of the files is consistent and contains a date/time stamp indicating when the dataset was derived.  The table below contains the filenames of the most recent versions (the GNU/Linux virtual machine contains older data files too as a back-up).

**ToDo 2018-03-09** Update the files with the latest versions.

| File                                                               | Data Source   | Level | Description |
|--------------------------------------------------------------------|---------------|-------|-------------|
| `ed attendances by mode measure - lsoa - 2016-11-21 19.54.Rda`     | HSCIC         | LSOA  | Mode of ED Attendance |
| `ed attendances by mode measure - site - 2016-11-21 19.54.Rda`     | HSCIC         | Site  | Mode of ED Attendance |
| `unnecessary ed attendances measure - lsoa - 2016-11-21 19.55.Rda` | HSCIC         | LSOA  | Unnecessary ED Attendances |
| `unnecessary ed attendances measure - site - 2016-11-21 19.55.Rda` | HSCIC         | Site  | Unnecessary ED Attendances |
| `ed attendances admitted measure - lsoa - 2016-11-21 19.58.Rda`    | HSCIC         | LSOA  | ED Attendances that led to Admission |
| `ed attendances admitted measure - site - 2016-11-21 19.58.Rda`    | HSCIC         | Site  | ED Attendances that led to Admission |
| `emergency admissions measure - lsoa - 2016-11-21 20.01.Rda`       | HSCIC         | LSOA  | Emergency Admissions (incl. non ED Attendances) |
| `emergency admissions measure - site - 2016-11-21 20.02.Rda`       | HSCIC         | Site  | Emergency Admissions (incl. non ED Attendances) |
| `critical care stays measure - lsoa - 2016-11-21 20.02.Rda`        | HSCIC         | LSOA  | Critical Care Stays |
| `critical care stays measure - site - 2016-11-21 20.03.Rda`        | HSCIC         | Site  | Critical Care Stays |
| `length of stay measure - lsoa - 2016-11-21 20.06.Rda`             | HSCIC         | LSOA  | Length of stay for admissions |
| `length of stay measure - site - 2016-11-21 20.07.Rda`             | HSCIC         | Site  | Length of stay for admissions |
| `ambulance mean times measure - lsoa - 2016-11-21-20.30.Rda`       | AS            | LSOA  | Ambulance conveyance times |
| `ambulance mean times measure - site - 2016-11-21-20.31.Rda`       | AS            | Site  | Ambulance conveyance times |
| `case fatality measure - lsoa - 2016-11-24 18.46.Rda`              | ONS Mortality | LSOA  | Mortality rates |
| `case fatality measure - site - 2016-11-24 18.46.Rda`              | ONS Mortality | Site  | Mortality rates |
| `ambulance non-conveyance measure - lsoa - 2016-09-02 15.25.Rda`   | AS            | LSOA  | Non-conveyances |
| `ambulance non-conveyance measure - site - 2016-09-02 15.25.Rda`   | AS            | Site  | Non-conveyances |
| `ambulance red calls measure - lsoa - 2016-11-21 20.31.Rda`        | AS            | LSOA  | 'Red' Ambulance Calls |
| `ambulance red calls measure - site - 2016-11-21 20.31.Rda`        | AS            | Site  | 'Red' Ambulance Calls |



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

## Replication and Extension of Work

Should you wish to extend this work you will at the very least have to use the GNU/Linux Virtual Machine.  This is likely to be unfamiliar to those who have only ever used M$-Windows since it requires the use of a [Command Line Interface (CLI)](Command Line Interface (CLI)).

### Pre-requisites

You will have to install a [SSH](https://en.wikipedia.org/wiki/Secure_Shell) client in order to connect to the Virtual Machine (VM).  If you are using M$-Windows there are two options, [PuTTy](https://putty.org/) or [Cygwin](https://cygwin.com/).  Both include tools for moving files back and forth from computers, although this might be easier to achieve using [WinSCP](https://winscp.net/eng/index.php).

After installing these software you will need to request a [static IP address from CiCS](http://csrs.shef.ac.uk/iprequestform.php) and provide it to the system adminstrator to allow access (as access to the VM is restricted to permitted [IP address](https://en.wikipedia.org/wiki/IP_address)) .  Once you IP address has been registered and allowed access you will have to configure the software (PuTTY/Cygwin and WinSCP) to connect.  The IP address of the VM is *not* detailed here and is instead provided to potential users via email.  Once you have the VM's IP address and your username (which is a variant on your staff login username across CiCS, usually of the format `sa_cm####`) you should be able to connect, the port through which the connection is made is the default for SSH (i.e. `22`).


### Running Analyses

Analyses are performed in the statistical progrmaming language [R](https://www.r-project.org/), although some models are run by calling [Stata](https://www.stata.com/) from R and then importing the results back into R for summarisation and presentation.

The work is done under the principles of [Reproducible Resarch](http://reproducibleresearch.net/), which in essence means that all work is written in a series of scripts and the script is re-run each time to re-create the report.  As the work is being done in [R](https://www.r-project.org/) the reports are written in [Rmarkdown](https://rmarkdown.rstudio.com/) which allows text describing the methods and results to be inter-weaved with R code that produces tables and figures.  The whole Rmarkdown script is run and currently [HTML](https://rmarkdown.rstudio.com/html_document_format.html) outputs are produced (although it is simple to produce [PDF](https://rmarkdown.rstudio.com/pdf_document_format.html) or [M$-Word](https://rmarkdown.rstudio.com/word_document_format.html) by modifying the header of the main file).

The Rmarkdown files can be found in the sub-directory `knitr`.  This top-level directory contains a number of files with the extension `.Rmd` and these are the master files for each report of the same filename but with the extension `.html`.  The main file of interest is `knitr/closed.Rmd` but sevewral other documents are also produced within this directory structure.  Of these top-level master files is quite short but calls "child" documents for inclusion as managing very long documents is cumbersome so the work is split across a number of files.  The first tier of child documents that are included is in the `knitr/sections` directory, and files here in-turn call/include files in the `knitr/sections/subsections` directory.  The nomenclature used should be self-explanatory as to what each is doing.

Ideally anyone working on this data will be conversant in [R](https://www.r-project.org/) as that is the language it is run in, ideally they will have experience with using the GNU/Linux [Bash shell](https://en.wikipedia.org/wiki/Bash_%28Unix_shell%29) as that is how they navigate and start programmes after having connected to the VM.  However, a very simple overview of how to, at a bare minimum, re-run the analyses follows...

1. SSH to the GNU/Linux Virtual Machine using PuTTY or Cygwin.
2. Change directory (`cd`) to the directory `work/scharr/closed/knitr` by typing `cd work/scharr/closed/knitr`
3. Start R by typing `R`.
4. Recreated the HTML report by typing `render("closed.Rmd")`.

This will take a while to run (perhaps an hour or so from memory) after which the file `~/work/scharr/closed/knitr/knitr.html` will have been updated.  You can then use WinSCP to copy that file from the VM back to your local computer (whether thats your computers hard-drive of the projects network drive).


### Modifying Analyses

This is a *lot* more invovled than simply re-running the analyses and will almost definitely require someone who is familiar with R to undertake.

As well as the [`rclosed`](https://github.com/tony-stone/rclosed) package written by Tony Stone for creating the time-series datasets used in these analyses and R package is created within this project under the [`lib/`](https://github.com/ns-ctru/closed/tree/master/lib) directory.  This consists of a number of [R functions](https://www.statmethods.net/management/userfunctions.html) for repeating the same task over and over on the different subsets.  This approach has been taken as it means that when for example a new model is required the code only needs changing in one place, the function, rather than 54 places if the code were written explicitly for each of the outcome measures/sub-measures.

To extend this work you should, ideally, fork this GitHub repository.  To do this you will need a GitHub account, signing up is free and since you are likely to be an academic then you can get a [free private repository](https://education.github.com/discount_requests/new) (normally private repositories are not available on free accounts, its a feature you have to pay for).  Once you have created your GitHub account and are signed in go to the top of this page and click the `Fork` button.  This makes a copy of the repository to your GitHub account.  You should then install [Git](https://www.git-scm.com/) on your computer and clone the forked copy of the `closed` project using the `Clone or download` link at the top of this page (the green button).  See [here](https://help.github.com/articles/cloning-a-repository/) for more instructions (Step 4 referes to the Git Shell you will have installed under M$-Windows).

You are now ready to start adding to or modifying the code.  It is recommended that you use [RStudio](https://www.rstudio.com/) to perform all work as it has built-in support for Git/GitHub (although you can use [Emacs](https://www.gnu.org/software/emacs/) with [ESS](https://ess.r-project.org/) as it too supports Git version control).  You can modify and change the code as desired.

You will have to upload any changes made to the R package and/or Rmarkdown files to the Virtual Machine and rebuild and install the R package on the Virtual Machine so that modifications are actually present.  This is done using the [devtools](https://www.rstudio.com/products/rpackages/devtools/), but is realitvely simple...

1. Copy your files to the VM using WinSCP in the directory `~/work/scharr/closed/`.
2. SSH to the VM.
3. Change directory (`cd`) to the directory `work/scharr/closed/lib` by typing `cd work/scharr/closed/knitr`
4. Start R by typing `R`.
5. Rebuild and install the R Closed package by typing...

    document()
	build()
	install()

6. You can now use the new/modified functions you have written.


#### populate.sh

A lot of the output is repetitive, its the same graphs/tables/summaries for different outcomes.  To simplify the process of writing and maintaining the required code for the 54 different outcomes a scripted approach has been taken whereby a master template document (`knitr/sections/subsections/template.Rmd`) has been written that contains the structure of the code.  This master document is then parsed using a Bash shell script (`knitr/sections/subsections/populate.sh`) that contains  [sed](https://en.wikipedia.org/wiki/Sed) command which replaces place holders for variable names with the acutal variable names in the datasets.  Thus if some new output is required you only have to modify the templates and re-run the `populate.sh` file and all child documents (under `sections/subsections/`) are updated to include the output and re-running `render("closed.Rmd")` on the VM produces an updated report (under `work/scharr/closed/knitr/closed.html` which you will need to copy back to your local computer).  Code is included in the master `closed.Rmd` which runs `knitr/sections/subsections/populate.sh` so "all" you need to do is modify `knitr/sections/subsections/template.Rmd` to include new output or if there is a new outcome add a new section to `knitr/sections/subsections/populate.sh` using the structure that is there and the appropriate measure/sub-measure names.

## Handover ToDo 2018-03-22

Post meeting with [Jon Nicholl](mailto:j.nicholl@sheffield.ac.uk) and [Emma Knowles](mailto:e.knowles@sheffield.ac.uk) the following actions...

- [x] Pass on my email address for future contact.
- [ ] Check LSOA files have all sites included.
- [ ] Convert Site level files and LSOA High/Low level files into Stata and move from VM to Network Drives.
- [ ] Highlight the location of existing Stata do-files.
- [ ] Extract and simplify the code for generating the time-series plots (as these are done in R it will require the Site level files in R format to be placed on the network drives too).
