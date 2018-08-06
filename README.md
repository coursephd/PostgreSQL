
Gephi:https://seinecle.github.io/gephi-tutorials/  
Another network analysis approach: http://ramblings.mcpher.com/Home/excelquirks/gassites/d3-concept-browser  

Disease medicine analysis:
Summary statistics for number of prescriptions per visit, new as well as repeat
Summary statistics for number of diseases per visit, new as well as repeat

Number of prescriptions per medicine and disease combination

Combinations of treatments given at a specific visit, almost like a shift table.
When a new treatment is introduced what is the sentiment? – any sentiment analysis?

1st disease and 1st dose combination – would it give any idea?

AA0084IRH2  
QLIK

Sentiment analysis  
https://www.cse.iitb.ac.in/~pb/papers/lrec18-medical-sa.pdf

# PostgreSQL
Hospital data access

Creation of programs using the PostgreSQL database and SQL codes

The following files should be used in the following sequence:  
# Dataset creation files: 

| Program name | Description |
| --- | --- |
| 100_adsl_sqlpart.sql | SQL code to download the necessary data from the server |
| 100_adsl.R | R code to create data for patient level information: Demog + Visit + diseases + treatments + services<br>-- Treatment names are corrected, Type_med variable is an attempt to create logical treatment grouping|
| 02other_data.R | SQL code is added at the top of the file and then followed by R code |
| 100_adsl_updt_BasedOn_otherData.R | Ongoing <br>Creates various background characteristics variables using other data, <br>this program uses a few other files, <br>Used files:<br>lookup.csv, lookup_backchar004_003.txt and lookup_backchar004_005.txt<br>Allopathic diagnosis variable creation: 060_allopathic_diag.R |

| Excel files | Description |
| --- | --- |
| _unq_whichdata_touse.xlsx| This file contains information related to background characteristics and medical history,<br>- The background history is coded by using ISO standards dictionary,<br>- The diseases are coded using ICD10 codelist|

# Analysis creation files:

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
| 060_allopathic_diag.R| ICD10 coding for background diseases used in **Tableau** displays|
| 070_Diff_Visit_Studyday_analysis | Create file for differences in 2 consecutive visits Overall and by Disease, these 2 files are used in **Tableau** displays|
| 070_Diff_Visit_Studyday_Medicine_analysis| Create file for differences in 2 consecutive visits by treatment, this file is used in **Tableau** displays|

# Documents created in RPubs based on the following programs [http://rpubs.com/mahajvi]:  

| Program name | Description |
| --- | --- |
| 100_adsl_analysis.rmd | Metabolic and RMSD disease analysis | 
| 100_adsl_analysis_survival_rmsd.Rmd | RMSD Survival analysis |
| 100_adsl_analysis_survival_met.Rmd  | Metabolic Survival analysis |
| 100_adsl_medicine.Rmd | Metabolic and RMSD Medicine analysis |

# D3 network program and the corresponding files are: [https://coursephd.github.io]  
-- This tree shows progression of diseases as experienced by patients in the database.  
-- This tree shows approximately 12,500 lines of data in very short space.  
-- Some diseases are experienced more by males or by females.  
-- Some diseases have many more branches than a few others.

# Displays created in Tableau [https://public.tableau.com/profile/frlht#!/]   

| Viz name | Description |
| --- | --- |
| **Based on old data till July 2016**||
| 04_patient_analysis_tablaeu |Demographic information of patients:<br>- 01NoOfPatients<br>- 02 Country<br>- 03 AgeBoxplotCountry<br>- 04 AgeBoxplotGroup<br>- 05 NoOfVisitsBox<br>- 05a NoOfDis_age<br>- 05b NoOfDis_agebox<br>- 06 NoOfDiseases<br>- 07 BloodGroup |
| IndividualPatientCalendar | This visual has 3 sheets (ignore sheet 3):<br>- IndividualPatientCal: Individual patient visit and recorded disease<br>- MonthDiseases: This provides a frequency count of diseases by month and gender, the month and year view can be opened up and details for each day can be found|
| Primary_disease_and_all_other_diseases |- This provides a view of diseases and other diseases reported by patients. The dashboard contains comprehensive information about summary statistics of age by gender, boxplots and bubble plot<br>- The bubble plot is shown for each disease and additional diseases experienced|
| PrimDis_otherDis_ByMonth | - This display is similar to the **Primary_disease_and_all_other_diseases** display, the disease distribution over different months is displayed.<br>- This should provide a good idea about seasonal variations of diseases|
| 01RMSD_MET |Detailed RMSD and Metabolic disease analysis<br>- 01TotalPatRMSD_Metabolic<br>- 02AgeGroupByDisease<br>- 03AgeDistByDisease<br>- 08CumDisplayByDuration<br>- 08UniqueVisWindow<br>- 08UnqBoxAgeGender<br>- 08CumDisplayAgeDist<br>- 08CumDispByDurDisease<br>- 09CumDisByDurVitals<br>- 09CumStatsByDurVitals<br>- 09PatProfile|

| Viz name | Description |
| --- | --- |
| **Based on data from SQL database till Oct 2017**||
| 01SQL_Dis_Med_Ser|Detailed RMSD and Metabolic disease analysis<br>- RMSD_Met_patients (Frequency table)<br>- Visit_Duration (Boxplot)<br>- Patient_Visit_View (Patient Profile 1 row per patient)<br>- 1stDay_Met_Disease (Summary statistics of metabolic disease reporting)<br>- DisType_Diseases (Disease frequency by gender and type)<br>- MedType_DisType (Medicine by gender and type)<br>- Medicine_DisCode (Medicine by disease, gender and type)<br>- DiseaseByStudyDay (Frequency of diseases by each day and gender)<br>- DiseaseByStudyDaySeason - Grishma Ritu (Frequency of diseases by each day, gender and **Rutus**)<br>- MedByStudyDay (Medicine prescribed by each study day)<br>- SeasonDisease (Frequency of diseases by gender and **Rutu**)<br>- SeasonMedicine (Frequency of medicines by gender and **Rutu**)<br>- PatProf_season - Aamavaata (Patient Profile 1 row per patient by **Rutus**)<br>- Box_AgeMed - Box plot for Age and medicine<br>- Slopegraph_disPatients -- Number of patients per year for each disease<br>- Slopegraph_disVisit -- Number of visits per year for each disease<br>- MedicineByDay -- Medicines prescribed by day| 
| Diff_Visit_Studyday | Boxplots for difference between 2 consecutive visits<br>- The number of days between 2 consecutive visits is quite large in initial visits, the difference reduces as the visits increase|
| Diff_Visit_Studyday_ByDisease | Boxplots for difference between 2 consecutive visits for individual disease<br>- This helps understand diseases reported more than once for individual patients.|
| Allopathic_diag | These displays provide background disease information for 8000+ patients.|

# Madhumeha
## Madhumeha specific analysis: This is an attempt to understand patients with primary diagnosis of madhumeha "M2.0", there are approximately 1400 patients in this subset.

| Program name | Description |
| --- | --- |
| 01_Primary_madhumeha.R |- This program creates a dataset for Madhumeha patients M2.0 listed as Primary diagnosis. This dataset is used in Tableau display.<br>- The diseases and corresponding treatments are displayed on calendar days as well as Study day view in Tableau|
| 02_Primary_madhumeha_comb_medicine |- This program creates a cumulative view of diseases and corresponding treatments prescribed. There are 1026 combinations of diseases observed in the data|

## Madhumeha specific analysis in Tableau:

| Viz name | Description |
| --- | --- |
| **Based on data from SQL database till Oct 2017**||
| 01_Primary_madhumeha| The diseases and corresponding treatments are displayed on calendar days as well as Study day view|
| 02_Primary_madhumeha_comb_medicine|A cumulative view of diseases and corresponding treatments prescribed.<br>- There are 1026 combinations of diseases observed in the data|

# Gridhrasee
## Gridhrasee specific analysis: This is an attempt to understand patients with primary diagnosis of Gridhrasee "V2.23", there are approximately 2000 patients in this subset.

| Program name | Description |
| --- | --- |
| 01_Primary_Gridhrasee| |
