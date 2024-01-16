# timescales

This readme gives an overview of the scripts associated with the paper "Implications of timescale choice for estimated effects of exposures on rate of cognitive change among older adults".

1.	Load results.R: This script pulls in output (.Rdata files) for all cohorts and groups and is sourced at the beginning of the rest of the scripts. The major parts of the code are:
o	1. Initializations: libraries, setting up data, and extracting the list of files to be loaded. 
o	2. Loading data: In a loop over all cohorts and groups, pull in .Rdata output, and create a “df” object with info about each group.
o	3. Extracting model results: extracts model fits from all output, and compares lowest AIC, BIC, and most flexible models that converge
o	4. Creates input objects that will be used to create the predicted trajectories for each model. 

  1.1 Final models.R: This script uses the fitted model data formatted in the “Load results.R” script (which it sources), and combines it with the final models selected by coauthors in a spreadsheet to create the final set of models used to create trajectory figures for the paper. 


2.	Table 1 formatting.R: Sources the “1 Load results.R” script, and uses output from each cohort and group to create Table 1s for the paper. The major parts of the code are:
o	1. Source output file and then save html Table 1 for each cohort/group, as they were created by coauthors. 
o	2. Manual edits to Table 1 (e.g. relabeling race categories to collapse to “non-white”, fixing typos in education level/relabeling to make them comparable.
o	3. Combining and formatting table 1 overall and by race 


3.	Predicted trajectories.R: Sources the “1.1 Final models.R” script (which also sources “1 Load results.R”). The major parts of the code are:
o	1. Looping over cohort and groups in pred_input to create the predicted  datasets and predictions for each model.  
o	2. Generating plots for the manuscripts for each cohort, group, with faceting by model specification, and for final models for all cohorts/groups.

4.	Predicted differences.R: Sources the “1.1 Final models.R” script (which also sources “1 Load results.R”). All calculations here are only done on the “final models” selected by coauthors. The major parts of the code are:
o	1. Looping over cohort and groups in pred_input to create the predicted  differences in 5-year time/age bands. 
o	2. Generating plots for the manuscripts for each cohort, group.
o	3. Creating tables with the data that went in the plots.

5.	Package versions.R: Sources the “1 Load results.R” script. Saves the version information from the output for each model.

6.	Attrition tables.R: Sources the “1 Load results.R” script. Loops over the df object to pull and format data from tables on attrition.

DWS analysis code_v4_HRS.Rmd: Example of analysis script run for each cohort, with variables names changed depending on the cohort. Code creates analytic variables, runs descriptives, and then mixed effects models with all combinations of specifications of timescale and baseline age. Runs analysis stratified by race/ethnicity if requested by user.  
