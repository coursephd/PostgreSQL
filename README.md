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
| 01_Primary_madhumeha.R | This program creates dataset for Madhumeha patients M2.0 listed as Primary diagnosis,<br>This dataset is used in Tableau display.<br>The diseases and corresponding treatments are displayed on calendar days as well as Study day view in Tableau|

| Excel files | Description |
| --- | --- |
| _unq_whichdata_touse.xlsx| This file contains information related to background characteristics and medical history,<br>The background history is coded by using ISO standards dictionary,<br>The diseases are coded using ICD10 codelist|

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

