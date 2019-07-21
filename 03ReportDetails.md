# Analysis carried out using Tableau, R and Java D3 

This document covers the details of analysis carried out on the Hospital database.  
The Source data used is the Hospital database
SQL programming and R programming are used to create analysis datasets  
These analysis datasets are then used as inputs to Tableau for generating various interactive visual displays  

## Displays created in Tableau [https://public.tableau.com/profile/frlht#!/]   

**Based on data from SQL database till Oct 2017**

## 1. Viz name: 01SQL_Dis_Med_Ser  

| Sheet name | Description |
| --- | --- |
| RMSD_Met_patients| Frequency table by gender and high level disease classification, there are more number of RMSD patients compared to the Metabolic, Metabolic and RMSD patients.<br>There are more number of female RMSD patients compared to males.<br>There are similar number of males and females in Metabolic disease categories| 
| Visit_Duration| Total duration of hopsital visits is calculated as the maximum date of hopsital visit - minimum date of hospital visit + 1 in days for each patient. This duration is plotted by each disease category. <br>- The median duration for RMSD was more than that for other categories<br>Median duration by disease category for females / males:<br>Metabolic: 325.5 / 408<br>Metabolic and RMSD: 530.5 / 559.5<br>RMSD: 690 / 631|
| Patient_Visit_View| This is a listing of individual patient by disease categories. The x-axis displays study day going from day 1 to last visit for each patient. Each bar represents a single study day. The IP visits are marked in Blue and OP visits are marked in Orange colour. For each visit, what kind disease type has been reported is displayed. The tooltip provides additional information related to Total duration of visits to hospital, description of disease, medicine name, first (minimum) day on which Metaolic disease was reported, first (minimum) day on which RMSD disease was reported.<br>- Patients suffering from RMSD type of diseases have more frequent visits to the hospital than those suffering from Metabolic diseases.|
| 1stDay_Met_Disease| Summary statistics by gender for first (minimum) day on which Metaolic disease was reported.|
| DisType_Diseases| Frequency table for individual diseases by disease category and gender<br>Prameha, Madhumeha and Sthaulya are top 3 most frequently reported diseases.<br>- Prameha and Madhumeha are reported more by males than females.<br>- Vaatavyaadhi - Sandhigata Vaata, Vaatavydahi, Vaatavydahi - Gridhrasee, Sthanabhedena Shoola - Katee Shoola and Sthanabhedena Shoola - Katee Graha are top 5 reported diseases in RMSD.<br>- Vaatavyaadhi - Sandhigata Vaata, Amavaata, Vaatavyaadhi - Asthigata Vaata, Vaatavyaadhi - Vaatakantaka are reported more by females compared to males.|
| MedType_DisType| Frequency table for medicine categories by gender.<br> Frequency counts for each prescribed medicine is reported.<br>- The source variable reported in the database needs to be modified to seperate the medicine name, the quantiy and the manufactures name.<br>- Current frequency counts show a 1 medicine prescibed to more than 3000 patients, 2 medicines to more than 2000 patients, 19 medicines to more than 1000 patients|
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

## 2. Viz name: Diff_Visit_Studyday  

| Sheet name | Description |
| --- | --- |
| Sheet 1| These boxplots provide the different between 2 visits in terms of days. Patients visit hospital as and when they need to or based on the guidance given by Vaidyas.<br>- There are some patients visiting the hospital for more than 2500 days.<br>- There are some patients only coming for a single visit.<br>- 1 unique date for a patient is considered as 1 visit.<br>- Difference between 2 consecutive visits is calculated for each patient and then plotted as a boxplot.<br>- The observed median difference in 2 visits reduces as the visit number increases.<br>- The median difference starts at 447 days and reduces to 342, 293, 235, 211.5, 197, 191.5, 154, and so on.<br>- Till visit 82 there are 10 or more patients.<br><br>This would provide useful insights into patient behaviour w.r.to visiting, recurrence and follow-up with vaidyas, operational challenges.|

## 3. Viz name: Diff_Visit_Studyday_ByDisease  

| Sheet name | Description |
| --- | --- |
| Sheet1| These boxplots provide similar information to the "Diff_Visit_Studyday" visual.<br>- For same patient, find out if a disease is reported more than once on different visits. If yes, then find out the difference between 2 consecutive occurences, use that "duration" in the boxplot.<br>- Display this difference as per visits for each disease reported.<br>- Many diseases are not reported more than once for the same patient.<br><br>This analysis should provide a clinical view as well as operational view on follow-up visits for patients. In case of diseases getting cured and a new episode appearing for a patient, large gaps could be seen. If the diseases can be classified into chronic / acute, easily curable vs. difficult to cure then this analysis would provide more useful insights.|

## 4. Viz name: Allopathic_diag  

| Sheet name | Description |
| --- | --- |
| ICDFreq|The existing ACD codes are used to map to the ICD 10 dictionary.<br>- Frequency table by ICD code, high level term by gender.|
| Baseage_box| Box plot of baseline age by ICD code and gender|
| All_vis_box| Boxplot of total number Visits by ICD code and gender|
| Hospital_duration| Boxplot of hospital duration by ICD and gender|

## 5. Viz name: Diff_Visit_Studyday_ByMedicine(NotCoded)  

| Sheet name | Description |
| --- | --- |
| Sheet 1| These boxplots provide similar information to the "Diff_Visit_Studyday" visual.<br>- For same patient, find out if a prescribed medicine is reported more than once on different visits. If yes, then find out the difference between 2 consecutive occurences, use that "duration" in the boxplot.<br>- Display this difference as per visits for each prescribed medicine.<br>- Many prescribed medicines are not reported more than once for the same patient.<br><br>This analysis should provide a clinical view as well as operational view on follow-up visits for patients.|

## 6. Viz name: 01_Primary_madhumeha:  
This is an attempt to understand patients with primary diagnosis of madhumeha "M2.0", there are approximately 1400 patients in this subset.  

| Sheet name | Description |
| --- | --- |
| Dis+Med_Caldate_view| This is a listing for individual madhumeha patient.<br>- Patient ID, gender, Baseline age are displayed.<br>- Diseases and medicines are displayed one below the other under Code and description columns.<br>- The duration is displayed as per calendar days.<br>- Duration of each disease and prescribed medicine is calculated as difference between maximum date - minimum date + 1 (this may not always be accurate, as it combines 2 independent episodes into 1).<br>- In the tooltip, additional information about total duration of visits to hospital, duration of disease and duration of prescribed medicine are displayed.|
| Dis+Med_Studyday_view| This is a listing similar to the calendar view, in place of calendar dates, the study day starting from 1 to maximum study day are used.|
| Box_MedAge| This boxplot display shows the summary statistics for age of Madhumeha patients getting prescribed to different medicines.|  

This individual patient listing would help in understanding relationship between treatments, diseases, co-occurences of diseases and co-admininstritation of treatments.  

## 7. Viz name: 01__Primary_Gridhrasee:  
This is an attempt to understand patients with primary diagnosis of Gridhrasee "V2.23", there are approximately 2000 patients in this subset.  

| Sheet name | Description |
| --- | --- |
| Dis+Med_Studyday_view| This is a listing for individual Gridhrasee patient.<br>- Patient ID, gender, Baseline age are displayed.<br>- Diseases and medicines are displayed one below the other under Code and description columns.<br>- The duration is displayed as per the study day starting from 1 to maximum study day are used.<br>- Duration of each disease and prescribed medicine is calculated as difference between maximum date - minimum date + 1 (this may not always be accurate, as it combines 2 independent episodes into 1).<br>- In the tooltip, additional information about total duration of visits to hospital, duration of disease and duration of prescribed medicine are displayed.|  

## 8. Viz name: 03_typesOfassessment:  
The hospital database captures information for each and every visit. This covers the operational data and clinical data. There are more than 100 Case Report Forms (CRF) within our database with more than 500 variables. Some of the CRF pages are not used at all. These CRFs are covering the In-patient as well as Out-patient visits. This information has been classified into the following categories:

* Ayurvedic data  
* Background data  
* Diease data  
* Doctor's Notes  
* Food / Exercise  
* Hospital Visit  
* Lab report  
* Measurement  
* Treatment - IP  
* Treatment - OP  
* Treatment - Procedure  
* Treatment / Procedure  

| Sheet name | Description |
| --- | --- |
| TypesOfassessments - Visit| This listing shows data for each patient for each of the categories created above for each visit.<br>- If the data is collected for a particular visit then a vertical bar is presented, if the data is not present then a blank is presented.<br>- Disease data, Treatment data, Treatment - procedure data is captured for almost each and every visit.<br>- Ayurvedic data, Background data, measurements, Doctor's notes are not captured consistently.|
| TypesOfassessments - StudyDay| This listing is similar to earlier listing, it is presented by study day.|
| Summary - Visit| This table displays frequency counts of unique patients for each of the categories created above for each visit.<br>- A patient is counted if the data is non-missing for a specific category.|
| Summary - StudyDay| This table is similar to earlier table, it is presented by study day.|

## 9. Viz name: 080_medicine_dis_all_met_rmsd_prop:  
Dashboard 1 in this visual is explained below.  
080_medicine_dis_repeat_prop_cumulative: 

| Sheet name | Description |
| --- | --- |
| Dashboard 1| This shows individual patient data for disease and treatment for Metabolic and RMSD patients. It provides diseases and treatments per patient as either<br>"disease reported 1st time" or "repeat",<br>"treatment reported 1st time" or "repeat".<br>- It is reported by studyday (or visit) when a disease and medicine is reported in the data.|  

This provides the following information:  
(1) When a new disease is reported, usually a new treatment(s) is (are) reported  
(2) If there is only a new treatment added then it could indicate, the earlier treatment may not have worked, or it explains the nature of treatment regimen.  

## 10. Viz name: 080_medicine_dis_repeat_prop_cumulative:  
Dashboard 1 in this visual is explained below.  

| Sheet name | Description |
| --- | --- |
| Dashboard 1| This dashboard should be read in parallel to the Dashborad on **080_medicine_dis_all_met_rmsd_prop** visual.<br>- Individual patient data is presented by visit in a cumulative manner for disease and prescribed medicine. In the adjoining table % are displayed.<br>|  

## 11. Viz name: 080_medicine_dis_repeat_prop:  
Dashboard 1 in this visual is explained below.  

| Sheet name | Description |
| --- | --- |
| Dashboard 1| This shows individual patient data for disease and treatment for Metabolic and RMSD patients.<br>- The top section provides diseases and treatments per patient as either<br>"disease reported 1st time" or "repeat",<br>"treatment reported 1st time" or "repeat". <br>- The section below displays detailed data for diseases and prescribed medicines.|   

## 12. Viz name: 085_dis_1st_time_refCal_NodesEdges:  
Each of the 106 diseases (10 Metabolic and 96 RMSD) is considered as a reference disease.  
- Day 1 is calculated as the reference day 1 for individual patient for each disease.  
- Other diseases for the same patient are positioned either before or after compared to this reference disease.  
- Duration w.r.to this reference day is calculated before and after day 1. This calculation provides the background view as well as future view.  
- This referencing allows for more informative background disease as well as background medicine information.
- The duration is split into the following time points:  

| Before | After |
| --- | --- |
| Day 1 as reference|
| Before 1 month| Within 1 month|
| Before 2 months| Within 2 months|
| Before 3 to 6 months| Within 3 to 6 months|
| Before 7 to 12 months| Within 7 to 12 months|
| Before 2nd year| Within 2nd year|
| Before 3rd year| Within 3rd year|
| Before 4th year| Within 4th year|
| Before 5 year| Within 5 year|  

* 1 sheet for each reference disease is created.  
* Frequency count of diseases and prescribed medicines is displayed.  
* Prior counts are displayed in red colour and After counts are displayed in Green colour.  

This view should provide good insights into the causal relationships.  

## 13. Viz name: 085_dis_count_edges_3rd_byPeriod02try:  
Dashboard PrimaryDis_relatedDisMed (2): circular view of disease and medicine relationship.  
This view allows the following comparisons:  

* Relationship between diseases and treatments across different time points.  
* If a disease is experienced in different time windows then would the treatment options look different or would they look similar.  
* Occurrence of diseases and proximity -- do the diseases precede and / or succeed each other, etc.  

E.g. Amavaata, what all diseases were experienced and treatments given before the 1st occurrence of disease and after the 1st occurrence of  disease. How far or how close were these events are given in terms of within 1 month, within 2 months, within 3 to 6 months, 1 year, 2year etc. on both sides.  

* The inner circle displays the diseases.  
* The outer circle displays the treatments.  
* Counts of distinct medicines prescribed and distinct diseases experienced are given.  
* More frequenct counts are displayed in the table above for each of the periods.  

## 14. Viz name: DistanceMeasures  
* There are 100s of Distance measures available in mathematics and statistics. These provide the similarity / dis-similarity between objects.  
* Diseases experienced by each patient is sorted by date and only 1st instance of a disease is retained. This way a disease trajectory is created for each and every patient for each and every reference disease, before and after the occurence of the reference disease.  
* Cartesian product of patients is created for each reference disease, so that distances can be calculated.  
* The similarity measure is calculated for each disease trajectory, e.g. Jaccard distance is used as a distance measure for this display.  
* Jaccard distance closer to 0 shows dissimilarities and closer to 1 shows similarities.  
* The distances are cut into 4 categories 0 to 0.25, 0.25 to 0.5, 0.5 to 0.75 and 0.75 to 1.  

| Sheet name | Description |
| --- | --- |
| DiseaseDist |-Most of the disease trajectories have distance score between 0 and 0.25 for before and after the reference disease.<br> - This could be interpreted as dissimilar. Not many patients have similar diseases before and after the reference disease.<br>- The underlying patient population could be considered as a heterogeneous population.<br>- This should also be seen in distance measures calculated for prescribed medicines.|
| DiseaseMaxDist|- Maximum distance measure from Jaccard is considered for calculations.<br>- This way most similar patients (as per disease trajectory) are analysed.<br>- Similarity scores between 0.75 and 1 increase in "After period" for almost all the diseases when compared to the "Before period".<br>- After an onset of a reference disease similar diseases are experienced and could help in building causal relationship between diseases.|
| DistIndPatientFreq|This view shows frequency count of distinct patients for each reference disease by similarity distance categories for Before and After periods|

## 15. Viz name: DistanceMeasures-Medicines:   
* This analysis is same as "DistanceMeasures" analysis. In place of disease trajectories, prescribed treatment trajectories.  
* The similarity scores are worse than that for the disease trajectories.  
* Most of the prescribed treatments are dis-similar for both the periods.  

## 16. Viz name: HumanBody02:  
* This visual shows the disease trajectories on a human skeleton.  
* Before and after periods are shown as 2 skeletons next to each other.  
* Diseases which could be approximately assigned to a body part are displayed on the body, otherwise are displayed on the side of the body.  
* Following Frequency counts are displayed:  
  * Total number of patients  
  * Total number of patients by gender  
  * Number of patients experiencing the disease trajectory is displayed  
  * Similarity score  
  * Total number of diseases listed in the disease trajectory  
  
## 17. Viz name: Primary_disease_and_all_other_diseases (On Older data)
Dashboard 2 in this visual is explained below.  

| Sheet name | Description |
| --- | --- |
| Dashboard 2| This dashboard has 4 sections<br>1. Summary statistics for age for various diseases.<br>2. Boxplot of age for various diseases.<br>3. Number of other diseases contracted at any time while having the underlying disease.<br>4. Bubble plot using frequency counts for diseases.<br><br>The dashboard is controlled by a "PrimaryCode" of a reference disease. The corresponding data is displayed on the page.<br>- The bubble plot displays the number of distinct patients having the primary disease.<br>- Other bubbles display the diseases reported by this subset of patients at any point in time (these could be clinically related or unrelated, could have occurred before or after the occurence of reference disease).<br>- The tooltip shows min, median and max age, distict counts of patients.<br>- A small table on the left side shows number of diseases experienced.<br><br>This display provides a comprehensive view of the disease clsuters. Some diseases could be experienced a lot more than some of the other diseases. Some diseases could be experienced differently by different genders, at different age groups.| 

## 18. Viz name: 01Cancer_SQL_Dis_Med_Ser  
Basic analysis carried out to understand the Cancer patients  

| Sheet name | Description |
| --- | --- |
| Cancer_patients| Frequency table by gender.<br>There are more number of females.<br>Many of these patients may have come for palliative treatment, but there is no evidence to claim this.<br>- Additional classification into different types of cancers is not clearly done in the database.|
| Visit_Duration| Total duration of hopsital visits is calculated as the maximum date of hopsital visit - minimum date of hospital visit + 1 in days for each patient.<br>Median duration for females is more than twice for males.|
| Patient_Visit_View| This is a listing of individual patient by disease categories. The x-axis displays study day going from day 1 to last visit for each patient. Each bar represents a single study day. The IP visits are marked in Blue and OP visits are marked in Orange colour. For each visit, what kind disease type has been reported is displayed. The tooltip provides additional information related to Total duration of visits to hospital, description of disease, medicine name, first (minimum) day on which Cancer was reported.<br>Many patients with cancer have come only once.|
| DisType_Diseases| Frequency table for individual diseases by disease category and gender.<br>- Arbuda Maamsaja has been reported by 81 females vs. 6 males, is this breast cancer?|
| MedType_DisType| Frequency table for medicine categories by gender.<br> Frequency counts for each prescribed medicine is reported.<br>- Cruel Plus, Heeraka Bhasma, Kanchanara are few of the most frequently prescribed medicine|
| Medicine_DisCode|  Frequency table for disease categories, Medicine by gender and individual disease.<br>- as explained earlier, the medicine name variable needs to be modified to get an accurate picture of prescribed medicines|
| DiseaseByStudyDay| Frequency counts of patients reporting a disease on a particular study day under each disease category.<br>- Most of the diseases have the highest frequency reported on day 1 and a steep drop in frequency counts is observed.|
| DiseaseByStudyDay - by season| Analysis similar to DiseaseByStudyDay, the frequency counts are reported by rutus| 
| MedByStudyDay| Frequency counts of prescibed medicines on a particular study day under each disease category.| 
| SeasonDisease| Frequency table for individual diseases by disease category, rutu and gender.<br>- This analysis should provide insights into seasonal variations of diseases|
| SeasonMedicine| Frequency table for prescribed medicines by rutu and gender.<br>- This analysis should provide insights into seasonal variations of prescriptions of treatments|
| Box_AgeMed| Box plot representation of age for each prescribed medicine by gender.<br>- This analysis should provide insights into age groupings|
| Slopegraph_disPatients| Line chart for each disease by calendar year by gender.<br>- Count of distinct patients is plotted on y-axis, the calendar years are displayed on x-axis.<br>- The x-axis can be expanded to an individual month or week or a day to understand the number of patients at a specific time point.<br>- This provides an easy comparison on similar or dissimilar reporting of a specific disease across gender.|
| Slopegraph_disVisit| Line chart similar to Slopegraph_disPatients. This visual shows number of distinct visits to the hospital.|
| MedicineByDay| This visual is for prescribed medicine. The medicines are classified into different kinds, Aristham, Asavams, Bhasmas, Arkas, Dhara, Drops, etc.<br>- Frequency counts by each day is plotted by gender.| 

## 19. Viz name: 01VrikkaRoga_SQL_Dis_Med_Ser  
Basic analysis carried out to understand the Vrikka Roga patients 

| Sheet name | Description |
| --- | --- |
| --| The explanation for each of the visual displays is similar to the earlier display for Cancer patients, the data presented here is for Vrikka Roga patients.|

## 20. Viz name: 01VrikkaRoga_Before_After  
Analysis carried out for the before and after periods of reporting Vrikka Roga 

Vrikka Roga is considered as a reference disease.  
- Day 1 is calculated as the reference day 1 for individual patient for Vrikka Roga.  
- Other diseases for the same patient are positioned either before or after compared to this reference disease (Vrikka Roga).  
- Duration w.r.to this reference day is calculated before and after day 1. This calculation provides the background view as well as future view.  
- This referencing allows for more informative background disease as well as background medicine information.
- The duration is split into the following time points:  

| Before | After |
| --- | --- |
| Day 1 as reference|
| Before 1 month| Within 1 month|
| Before 2 months| Within 2 months|
| Before 3 to 6 months| Within 3 to 6 months|
| Before 7 to 12 months| Within 7 to 12 months|
| Before 2nd year| Within 2nd year|
| Before 3rd year| Within 3rd year|
| Before 4th year| Within 4th year|
| Before 5 year| Within 5 year|  

* 1 sheet for each reference disease (Vrikka Roga) is created.  
* Frequency count of diseases and prescribed medicines is displayed.  
* Prior counts are displayed in red colour and After counts are displayed in Green colour.  

This view should provide good insights into the causal relationships. 

## 21. Viz name: 01Cancer_Before_After  
Analysis carried out for the before and after periods of reporting Cancer  

The explanation for each of the visual displays is similar to the earlier display for Vrikka Roga patients, the data presented here is for Cancer patients.

## 22. Viz name: 080VrikkaRogaDis_Med_analysis  
Analysis of number of diseases and number of prescribed treatments for Vrikka Roga patients  

This shows individual patient data for disease and treatment for Vrikka Roga patients. It provides diseases and treatments per patient as either  
"disease reported 1st time" or "repeat" (data displayed in Sheet Disease),  
"treatment reported 1st time" or "repeat" (data displayed in Sheet Medicine).  
- It is reported by studyday (or visit) when a disease and medicine is reported in the data.  

The data is analysed in 2 forms (1) what would have happened at every visit and (2) cumulative form (till a particular visit)  
Interpretation  
(1) When a new disease is reported, usually a new treatment(s) is (are) reported  
(2) If there is only a new treatment added then it could indicate, the earlier treatment may not have worked, or it explains the nature of treatment regimen.  

| Sheet name | Description |
| --- | --- |
| Disease| "disease reported 1st time" or "repeat"|
| Medicine| "treatment reported 1st time" or "repeat"|
| Patient_1st_Repeat| Individual patient listing of disease and medicine classified as 1st or "repeat" by study day|
| Patient_Dis_Med_counts| Frequency count of disease and medicine classified as 1st or "repeat" by study day (i.e. each visit)|
| Patient_dis_med_Cumulative| Frequency count of disease and medicine classified as 1st or "repeat" by cumulative study day (i.e. till xx visit)|
| %Patient_dis_med_Cumulative| % of the cumulative classification|
| Dashboard 1| All the above data is displayed on 1 page, the patient subset applied on this page is applied to all other pages for easy navigation and review|

## 23. Viz name: 102_episodic01_responder_nonresponder
Based on study visits, creation of the disease episodes, related / un-related diseases / Only 1 day visit  

Episodic view analysis:  
* Patients come to hospital as and when there is a need either for the same disease or for different diseases. There is no fixed protocol as well as fixed visit schedule which they would need to follow. A lot of patients (more than 50%) do not visit more than 1 visit. Within a month, the overall patient proportion falls to 30%. Due to this underlying reason the response variable is not properly captured.  
* The following algorithm attempts to create an artificial response rate. Following steps create patient categories:  

1.	Create a variable to identify episodes of a disease if a disease is re-appearing after 30 days then consider that asa new episode, this duration should be specific to each disease in reality  
2.	Use the variable “eps01” for cumulative addition and get number of episodes for each disease for each patient, if a disease is non-episodic then use 9999 as the duration  
3.	This calculation should help in understanding the disease specific pseudo outcome and amount of data collected  
4.	Use 180 days duration to separate episodes as related vs. un-related as an additional layer of relationship, save this information in a variable called as “releps01”  
5.	The duration between episodes as well as between related episodes provides an insight into how close or how far the recurrence of events  
6.	Use these variables along with the overall classification of a patient to create a medical story  

| Sheet name | Description |
| --- | --- |
| Resp/NonResp/List| Individual patient listing for each disease (diseases are listed as an individual page), the response is displayed as 1 Day Visit, Responder, Non-responder.<br>* The subset created on this page is applied to all other pages |
| Resp/NonResp/RelatedEvent| This is similar to the earlier display, an additional "related episode" variable is displayed to understand the recurrence of the related events |
| Respond/NonRespond/Boxplot| Boxplot of responder vs. non-responder by individual episode, due to the current definition, the responder category would have more number days compared to the non-responder category |
| SummaryStatsEpisodeDur| Descriptive statistics for the same analysis are displayed in "Respond/NonRespond/Boxplot" display |
| BoxplotDiffBetEpisodes| This boxplot provides information about the number of days between episodes, the minimum duration between the 2 episodes is 30 days due to the algorithm used. Far apart the duration between the 2 episodes, the possibility of them being clinically independent from each other.|
| SummaryStatsDiffBetEpisodes| Descriptive statistics for the same analysis are displayed in "BoxplotDiffBetEpisodes" display |

## 24. Viz name: 105_trt_dis_unq_mult
Identification of prescribed treatments to a specific disease or multiple diseases  

* Treatments and diseases are reported per visit on a case report form. Multiple diseases and multiple treatments could get reported in a visit. Due to the database set-up, the 1 to 1 mapping of the disease and treatment may not be possible. In ayurvedic treatment mechanism, the same treatment could be administered for multiple diseases and vice versa.   
* Patients are classified as having reported only a single disease and having reported multiple diseases.  

| Sheet name | Description |
| --- | --- |
| Dashboard 2| This dashboard has 4 sections<br>1. Summary statistics for age for various diseases.<br>2. Boxplot of age for various |

## 25. Viz name: 107_prim_sec_diag01
Disease-disease relationship, consider 1 disease as a primary disease and then calculate the duration between 2 reported events, distant events may mean clinically irrelevant events, closer could mean related to each other  

| Sheet name | Description |
| --- | --- |
| Dashboard 2| This dashboard has 4 sections<br>1. Summary statistics for age for various diseases.<br>2. Boxplot of age for various |

## 26. Viz name: 107_prim_sec_diag01vrikka_roga
Disease-disease relationship as explained above carried out considering Vrikka Roga as a primary disease  

| Sheet name | Description |
| --- | --- |
| Dashboard 2| This dashboard has 4 sections<br>1. Summary statistics for age for various diseases.<br>2. Boxplot of age for various |

## 27. Viz name: 102_episodic01_responder_nonresponder_vrikka_roga
Based on study visits, creation of the disease episodes, related / un-related diseases / Only 1 day visit for Vrikka Roga patients  

| Sheet name | Description |
| --- | --- |
| Dashboard 2| This dashboard has 4 sections<br>1. Summary statistics for age for various diseases.<br>2. Boxplot of age for various |

## 28. Viz name: 105_trt_dis_unq_mult_vrikka_roga
Identification of prescribed treatments to a specific disease or multiple diseases for Vrikka Roga patients  

| Sheet name | Description |
| --- | --- |
| Dashboard 2| This dashboard has 4 sections<br>1. Summary statistics for age for various diseases.<br>2. Boxplot of age for various |


## Analysis created in RPubs [http://rpubs.com/mahajvi]:

## 1. Metabolic and RMSD disease analysis
This report has been split into 7 sections.

## Analysis created using Java D3 network: 

## 1. Collapsible tree: [https://coursephd.github.io]

* Diseases experienced by each patient is sorted by date and only 1st instance of a disease is retained. 
* For each disease trajectory the frequency counts are created and are displayed as a collapsible tree. 
* This tree shows progression of diseases as experienced by patients in the database.
* This tree shows approximately 12,500 lines of data in very short space.
* Some diseases are experienced more by males or by females.
* Some diseases have many more branches than a few others.

## 2. Collapsible network diagrams

Amavaata [https://coursephd.github.io/nodediagram/A2_0/]  
Amavaata by period [https://coursephd.github.io/nodediagram/A2_0byperiod/]

Prameha [https://coursephd.github.io/nodediagram/P5_0_Prameha/]  
Prameha by period [https://coursephd.github.io/nodediagram/P5_0_Pramehabyperiod/]

* This tree shows the relationships between the reference disease, other diseases and treatments administered.
* The links above show 2 examples using Amavaata as a reference disease.
* Explanation: 
    * Identify unique patients who have had Amavaata reported at least once. 
    * Get all the other diseases and prescribed medicines for this subset of patients.
    * The bubble display the proportional sizes.
    * The links display relationships between diseases and treatments
* If a bubble is "double clicked" then all the "unrelated data" to that bubble vanishes and only relevant data is retained on the screen. Once double clicked again the complete data is displayed again.
* "By period" link shows the time period in which either a treatment or disease has been reported providing relationship between entities.

Similar type of displays could be built for all the diseases.
