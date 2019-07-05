
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
| 070_Diff_Visit_Studyday_analysis | Create a file for differences in 2 consecutive visits Overall and by Disease, these 2 files are used in **Tableau** displays|
| 070_Diff_Visit_Studyday_Medicine_analysis| Create a file for differences in 2 consecutive visits by treatment, this file is used in **Tableau** displays|
| 080_medicine_repeat_prop.R| Create a file to understand the first occurrence of the disease, repeat occurrences of diseases as well as for the medicines|
| 080_medicine_repeat_prop_addnl_cal.R| Create a file to understand the first occurrence of the disease, repeat occurrences of diseases as well as for the medicines, this inlcudes number of times a disease and treatment is co-occurring|
| 085_dis_1st_time_refCal_NodesEdges.R| This program creates **all_met_rmsd02** dataset, this allows the "before" and "after" view for each disease for each patient, this provides a comprehensive view of background and foreground for each disease|
| 085_dis_counts_bruce_java.R| Bruce McPherson, a programmer, has provided a Java utility to create a **collapsible view**. This is implemented using R, Java and Github, this program creates the necessary Json file. This view allows combination of diseases and treatments assigned|
| 085_dis_counts_bruce_java_byperiod.R| Explanation similar to the above program, data by period "before" and "after" view is possible|
| 085_dis_counts_edges.R| This program creates a csv file used in a Circular Tableau display, the inner circle represents the diseases, the outer circle represents the medicines given, the data is visualised for each reference disease and period|
| 085_dis_counts_edges_3rdbyPeriod_circular17.R| Explanation similar to the above program|
| 086_dis_patterns_HumanBody.R| This program creates a dataset useful for display of disease trajectories on a human body tableau display|
| 086_dis_patterns_combinations.R| This program creates a dataset of disease trajectories|
| 086_dis_patterns_combinations_gender_Macro.R| This programs creates multiple files, similarity measurements for each patient by reference disease, similarity measurements for medicines, these files are used in Tableau display|
| 086_fisher.R| This program creates singificant combinations of disease trajectories|
| 01Cancer_SQL_Dis_Med_Ser.R| This program creates a dataset/csv file for basic analysis of Cancer patients, same named tableau display is also available|
| 01VrikkaRoga_SQL_Dis_Med_Ser.R| This program creates a dataset/csv file for basic analysis of Vrikka Roga patients|
| 01VrikkaRoga_Before_After.R| Analysis carried out for the before and after periods of reporting Vrikka Roga|
| 01Cancer_Before_After.R| Analysis carried out for the before and after periods of reporting Cancer|
| 080VrikkaRogaDis_Med_analysis.R| Analysis of number of diseases and number of prescribed treatments for Vrikka Roga patients|
| 102_episodic01_responder_nonresponder.R| Based on study visits, creation of the disease episodes, related / un-related diseases / Only 1 day visit|
| 105_trt_dis_unq_mult.R| Identification of prescribed treatments to a specific disease or multiple diseases|
| 107_prim_sec_diag01.R| Disease-disease relationship, consider 1 disease as a primary disease and then calculate the duration between 2 reported events, distant events may mean clinically irrelevant events, closer could mean related to each other|
| 107_prim_sec_diag01vrikka_roga.R| Disease-disease relationship as explained above carried out considering Vrikka Roga as a primary disease|
| 102_episodic01_responder_nonresponder_vrikka_roga.R| Based on study visits, creation of the disease episodes, related / un-related diseases / Only 1 day visit for Vrikka Roga patients|
| 105_trt_dis_unq_mult_vrikka_roga.R| Identification of prescribed treatments to a specific disease or multiple diseases for Vrikka Roga patients|

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
| 080_medicine_dis_repeat_prop | These displays may be inaccurate (over representing number of treatments and diseases)|
| 080_medicine_dis_repeat_prop_cumulative | These displays may be inaccurate (over representing number of treatments and diseases)|
| 080_medicine_dis_all_met_rmsd_prop | These displays provide patient level information by visit, number of medicines and diseases, 1st time reporting and repeat reporting|
| 085_dis_1st_time_refCal_NodesEdges | These displays provide display for each reference disease by period "before" and "after" , split into month and year, frequency of distinct patients by each disease and treatment|
| 085_dis_count_edges_3rd_byPeriod02try | This display provides a comprehensive view of disease and treatment combinations across "before" and "after" periods. The inner circle represents the diseases, the outer circle represents the medicines given, the data is visualised for each reference disease and period|
| 086_dis_patterns_combinations_distance | Diease trajectory similarity distance calculations are displayed as a matrix form, before and after each reference disease, Distance (0 dissimilar, 1 similar) is color coded. A lot of dissimilarity is visible before day 1 of reference disease, it becomes more homogeneous after day 1|
| DistanceMeasures | Butterfly views of the similarity distance matrix, diease distance before and after, maximum distance (similarity) before and after, Maximum distance for each patient for each reference disease, distance for each disease by gender before and after day 1|
| DistanceMeasures-Medicines| Butterfly views for medicines similar to the disease views|
| HumanBody02 | This display show disease trajectory for each reference disease on human body|

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


Additional notes:
=================================================================================    
Execute as an administrator  
Windows PowerShell
Copyright (C) Microsoft Corporation. All rights reserved.

PS C:\WINDOWS\system32> npm install -g --production windows-build-tools

> windows-build-tools@3.1.0 postinstall C:\Users\user\AppData\Roaming\npm\node_modules\windows-build-tools
> node ./dist/index.js

Downloading BuildTools_Full.exe
Downloading python-2.7.14.amd64.msi
[>                                            ] 0.0% (0 B/s)
Downloaded python-2.7.14.amd64.msi. Saved to C:\Users\user\.windows-build-tools\python-2.7.14.amd64.msi.

Starting installation...
Launched installers, now waiting for them to finish.
This will likely take some time - please be patient!

Status from the installers:
---------- Visual Studio Build Tools ----------
[05C8:2D84][2018-08-31T20:19:41]i000: MUX:  Free Disk Space before install:  SystemDrive C:\ 17357598720 bytes  AppDrive C:\ 17357598720 bytes
[2480:07BC][2018-08-31T20:19:41]i360: Creating a system restore point.

------------------- Python --------------------  
Successfully installed Python 2.7  
All done!

+ windows-build-tools@3.1.0
added 142 packages in 228.729s  


Pathfinder: Harvard tool  
http://caleydo.org/tools/

Interactive tree of life:  
http://itol.embl.de/  
Login: mahajvi, password: frlht123  

Dendroscope: http://dendroscope.org/  

All related to the network graphs:  
Gephi:https://seinecle.github.io/gephi-tutorials/  

Bruce McPherson D3 and java examples (look brilliant):     
This should help in generating the necessary output: http://ramblings.mcpher.com/Home/excelquirks/d3/anyforce  
The code from Github: https://gist.github.com/brucemcpherson/4684498  

Another network analysis approach: http://ramblings.mcpher.com/Home/excelquirks/gassites/d3-concept-browser  
http://ramblings.mcpher.com/Home/excelquirks/drivesdk/vizdependencies  
Dowloadable files: http://ramblings.mcpher.com/Home/excelquirks/downloadlist  
Files to create: http://ramblings.mcpher.com/Home/excelquirks/gassites/d3nodefocus  
http://ramblings.mcpher.com/Home/excelquirks/gassites/siterestexce  


D3 Bi-directional Sankey diagram: http://bl.ocks.org/Neilos/584b9a5d44d5fe00f779  

Tableau:  
Radial and polygon charts: http://www.analysis-consulting.com/single-post/2017/01/13/Polygon-Radial-Charts -- help in improving the current graph   
Use this post to print a circle: http://www.kenflerlage.com/2017/11/beyond-show-me-part-2-trigonometry.html  
Data layering examples: http://www.datablick.com/blog?category=Tableau  
Jim Knippenberg:  
https://public.tableau.com/profile/jim.knippenberg#!/vizhome/NetworkGraphExample_0/NetworkGraph  
https://www.dropbox.com/sh/83uslc5lshnf2po/AAA7cmGt2ICQNW7xGwgBPzcra?dl=0  

http://www.clearlyandsimply.com/clearly_and_simply/2012/12/build-network-graphs-in-tableau.html  
https://stephanefrechette.com/network-graphs-tableau/#.W2lXoWlubiw  

Timelines storyteller: https://timelinesrevisited.github.io/  
App for use: https://timelinestoryteller.com/app/  
=================================================================================    

Zipf's law: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3189502/  

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

https://flare.prefuse.org/apps/job_voyager/  


https://www.shanelynn.ie/using-pandas-dataframe-creating-editing-viewing-data-in-python/ 
It is like Data.Table package


Blog  
https://shiring.github.io/categories.html#machine_learning-ref

Co-morbidity:  
https://pdfs.semanticscholar.org/ada9/861da5deb32c0bb4133e3c05880af7b6c2fc.pdf

