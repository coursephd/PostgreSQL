# Analysis carried out using Tableau 

This document covers the details of analysis carried out on the Hospital database.  
The Source data used is the Hospital database
SQL programming and R programming are used to create analysis datasets  
These analysis datasets are then used as inputs to Tableau for generating various interactive visual displays  

## Displays created in Tableau [https://public.tableau.com/profile/frlht#!/]   

**Based on data from SQL database till Oct 2017**

1. Viz name: 01SQL_Dis_Med_Ser  

| Sheet name | Description |
| --- | --- |
| RMSD_Met_patients| Frequency table by gender and high level disease classification, there are more number of RMSD patients compared to the Metabolic, Metabolic and RMSD patients.<br>There are more number of female RMSD patients compared to males.<br>There are similar number of males and females in Metabolic disease categories| 
| Visit_Duration| Total duration of hopsital visits is calculated as the maximum date of hopsital visit - minimum date of hospital visit + 1 in days for each patient. This duration is plotted by each disease category. \\* The median duration for RMSD was more than that for other categories|   

| Viz name | Description |
| --- | --- |
| **Based on data from SQL database till Oct 2017**||
| 01SQL_Dis_Med_Ser|Detailed RMSD and Metabolic disease analysis<br>- RMSD_Met_patients (Frequency table)<br>- Visit_Duration (Boxplot)<br>- Patient_Visit_View (Patient Profile 1 row per patient)<br>- 1stDay_Met_Disease (Summary statistics of metabolic disease reporting)<br>- DisType_Diseases (Disease frequency by gender and type)<br>- MedType_DisType (Medicine by gender and type)<br>- Medicine_DisCode (Medicine by disease, gender and type)<br>- DiseaseByStudyDay (Frequency of diseases by each day and gender)<br>- DiseaseByStudyDaySeason - Grishma Ritu (Frequency of diseases by each day, gender and **Rutus**)<br>- MedByStudyDay (Medicine prescribed by each study day)<br>- SeasonDisease (Frequency of diseases by gender and **Rutu**)<br>- SeasonMedicine (Frequency of medicines by gender and **Rutu**)<br>- PatProf_season - Aamavaata (Patient Profile 1 row per patient by **Rutus**)<br>- Box_AgeMed - Box plot for Age and medicine<br>- Slopegraph_disPatients -- Number of patients per year for each disease<br>- Slopegraph_disVisit -- Number of visits per year for each disease<br>- MedicineByDay -- Medicines prescribed by day| 
