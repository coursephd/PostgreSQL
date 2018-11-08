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
| Visit_Duration| Total duration of hopsital visits is calculated as the maximum date of hopsital visit - minimum date of hospital visit + 1 in days for each patient. This duration is plotted by each disease category. <br>- The median duration for RMSD was more than that for other categories<br>Median duration by disease category for females / males:<br>Metabolic: 325.5 / 408<br>Metabolic and RMSD: 530.5 / 559.5<br>RMSD: 690 / 631|
| Patient_Visit_View| This is a listing of individual patient by disease categories. The x-axis displays study day going from day 1 to last visit for each patient. Each bar represents a single study day. The IP visits are marked in Blue and OP visits are marked in Orange colour. For each visit, what kind disease type has been reported is displayed. The tooltip provides additional information related to Total duration of visits to hospital, description of disease, medicine name, first (minimum) day on which Metaolic disease was reported, first (minimum) day on which RMSD disease was reported.<br>- Patients suffering from RMSD type of diseases have more frequent visits to the hospital than those suffering from Metabolic diseases.|
| 1stDay_Met_Disease| Summary statistics by gender for first (minimum) day on which Metaolic disease was reported.|
| DisType_Diseases| Frequency table for individual diseases by disease category and gender<br>Prameha, Madhumeha and Sthaulya are top 3 most frequently reported diseases.<br>- Prameha and Madhumeha are reported more by males than females.<br>- Vaatavyaadhi - Sandhigata Vaata, Vaatavydahi, Vaatavydahi - Gridhrasee, Sthanabhedena Shoola - Katee Shoola and Sthanabhedena Shoola - Katee Graha are top 5 reported diseases in RMSD.<br>- Vaatavyaadhi - Sandhigata Vaata, Amavaata, Vaatavyaadhi - Asthigata Vaata, Vaatavyaadhi - Vaatakantaka are reported more by females compared to males.|
