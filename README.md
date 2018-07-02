# PostgreSQL
Hospital data access

Creation of programs using the PostgreSQL database and SQL codes

The following files should be used in the following sequence:  
Dataset creation files: 

| Program name | Description |
| --- | --- |
| 100_adsl_sqlpart.sql | SQL code to download the necessary data from the server |
| 100_adsl.R | R code to create data for patient level information: Demog + Visit + diseases + treatments + services |
| 02other_data.R | SQL code is added at the top of the file and then followed by R code |
| 100_adsl_updt_BasedOn_otherData.R | Ongoing <br>Creates various background characteristics variables using other data, <br>this program uses a few other files, <br>Used files:<br>lookup.csv, lookup_backchar004_003.txt and lookup_backchar004_005.txt<br>Allopathic diagnosis variable creation: 060_allopathic_diag.R |

Madhumeha specific analysis: This is an attempt to understand patients with primary diagnosis of madhumeha "M2.0", there are approximately 1400 patients in this subset.

| Program name | Description |
| --- | --- |
| 01_Primary_madhumeha.R | This program creates a dataset for Madhumeha patients M2.0 listed as Primary diagnosis. This dataset is used in Tableau display.<br>The diseases and corresponding treatments are displayed on calendar days as well as Study day view in Tableau|
| 02_Primary_madhumeha_comb_medicine | This program creates a cumulative view of diseases and corresponding treatments prescribed. There are 1026 combinations of diseases observed in the data|

| Excel files | Description |
| --- | --- |
| _unq_whichdata_touse.xlsx| This file contains information related to background characteristics and medical history,<br>>* The background history is coded by using ISO standards dictionary,<br>>* The diseases are coded using ICD10 codelist|

Analysis creation files:

| Program name | Description |
| --- | --- |
| Ongoing: | |
| 100_adsl_analysis.rmd | R Knitr file to produce a word document / HTML file |  
| 01adsl_primary_related_diseases.R | Primary and related disease/medication caretsian product creation program, use in Tableau |
| 01adsl_primary_related_diseases_subgroup.R | Primary and related disease/medication by gender, used in network graph type of analysis
| 01adsl_primary_related_diseases_seasonGender_subgroup_Network.R | Primary and related disease/medication by gender and season, used in network graph type of analysis |
| 20_cooccur.R | Co-occurence analysis for the diseases, may help in printing the network graph |
| 101_age_sparkline.Rmd | Sparkline analysis to show many graphs at one go. |
| 01_traxminer_disease.R | Event sequence analysis using R |
| 05_slopegraph.R | Edward Tufte style slopegraph to understand the trends in the data |

Documents created in RPubs based on the following programs [http://rpubs.com/mahajvi]:  

| Program name | Description |
| --- | --- |
| 100_adsl_analysis.rmd | Metabolic and RMSD disease analysis | 
| 100_adsl_analysis_survival_rmsd.Rmd | RMSD Survival analysis |
| 100_adsl_analysis_survival_met.Rmd  | Metabolic Survival analysis |
| 100_adsl_medicine.Rmd | Metabolic and RMSD Medicine analysis |

D3 network program and the corresponding files are: [https://coursephd.github.io]

Displays created in Tableau [https://public.tableau.com/profile/frlht#!/]   

| Viz name | Description |
| --- | --- |
| Based on old data ||
| 04_patient_analysis_tablaeu |Demographic information of patients:<br> 01NoOfPatients<br>02 Country<br>03 AgeBoxplotCountry<br>04 AgeBoxplotGroup<br>05 NoOfVisitsBox<br>05a NoOfDis_age<br>05b NoOfDis_agebox<br>06 NoOfDiseases<br>07 BloodGroup |
| IndividualPatientCalendar | This visual has 3 sheets (ignore sheet 3):<br> IndividualPatientCal: Individual patient visit and recorded disease<br>MonthDiseases: This provides a frequency count of diseases by month and gender, the month and year view can be opened up and details for each day can be found|
| Primary_disease_and_all_other_diseases | This provides a view of diseases and other diseases reported by patients. The dashboard contains comprehensive information about summary statistics of age by gender, boxplots and bubble plot<br>The bubble plot is shown for each disease and additional diseases experienced|
