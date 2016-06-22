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
log using 'tmp/cross-validate.smcl', replace


/* Read in the data                                                   */
insheet using 'lib/data-raw/ed_attendances_by_mode_measure.csv', clear

/* 2016-05-10 - Progress halted, can't run Stata                      */

/* Close and convert the log file                                     */
log c
log2html 'tmp/cross-validate.smcl', replace
