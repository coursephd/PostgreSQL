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
| MedType_DisType| Frequency table for disease categories by gender.<br> Frequency counts for each prescribed medicine is reported.<br>- The source variable reported in the database needs to be modified to seperate the medicine name, the quantiy and the manufactures name.<br>- Current frequency counts show a 1 medicine prescibed to more than 3000 patients, 2 medicines to more than 2000 patients, 19 medicines to more than 1000 patients|
| Medicine_DisCode| Frequency table for disease categories, Medicine by gender and individual disease.<br>- as explained earlier, the medicine name variable needs to be modified to get an accurate picture of prescribed medicines|
| DiseaseByStudyDay| Frequency counts of patients reporting a disease on a particular study day under each disease category.<br>- Most of the diseases have the highest frequency reported on day 1 and a steep drop in frequency counts is observed.|
|  DiseaseByStudyDay - by season| Analysis similar to DiseaseByStudyDay, the frequency counts are reported by rutus|
| MedByStudyDay| Frequency counts of prescibed medicines on a particular study day under each disease category.|
| SeasonDisease| Frequency table for individual diseases by disease category, rutu and gender.<br>- This analysis should provide insights into seasonal variations of diseases|
| SeasonMedicine| Frequency table for prescribed medicines by rutu and gender.<br>- This analysis should provide insights into seasonal variations of prescriptions of treatments|
| Box_AgeMed| Box plot representation of age for each prescribed medicine by gender.<br>- This analysis should provide insights into age groupings|
| Slopegraph_disPatients| Line chart for each disease by calendar year by gender.<br>- Count of distinct patients is plotted on y-axis, the calendar years are displayed on x-axis.<br>- The x-axis can be expanded to an individual month or week or a day to understand the number of patients at a specific time point.<br>- This provides an easy comparison on similar or dissimilar reporting of a specific disease across gender.|
| DisType_Diseases| Frequency table for individual diseases by disease category and gender<br>Prameha, Madhumeha and Sthaulya are top 3 most frequently reported diseases.<br>- Prameha and Madhumeha are reported more by males than females.<br>- Vaatavyaadhi - Sandhigata Vaata, Vaatavydahi, Vaatavydahi - Gridhrasee, Sthanabhedena Shoola - Katee Shoola and Sthanabhedena Shoola - Katee Graha are top 5 reported diseases in RMSD.<br>- Vaatavyaadhi - Sandhigata Vaata, Amavaata, Vaatavyaadhi - Asthigata Vaata, Vaatavyaadhi - Vaatakantaka are reported more by females compared to males.|
| Slopegraph_disVisit| Line chart similar to Slopegraph_disPatients. This visual shows number of distinct visits to the hospital.|
| MedicineByDay| This visual uses a derived variable for prescribed medicine. The medicines are classified into different kinds, Aristham, Asavams, Bhasmas, Arkas, Dhara, Drops, etc.<br>- Frequency counts by each day is plotted by gender.<br>- Arkas, Avagha, Bhasmas, Panchakarma are prescribed in lesser frequencies.<br>- Kashayam, Aristham, Rasaynama, Abhyanga, etc. are prescribed in higher frequencies.|

2. Viz name: Diff_Visit_Studyday  

| Sheet name | Description |
| --- | --- |
| Sheet1| These boxplots provide the different between 2 visits in terms of days.| 
