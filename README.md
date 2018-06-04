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
| 100_adsl_updt_BasedOn_otherData.R | Ongoing   -This program creates various background characteristics variables using other data, this program uses lookup.csv file and a few other files |

Analysis creation files:

| Program name | Description |
| --- | --- |
| 100_adsl_analysis.rmd | R Knitr file to produce a word document / HTML file |  
| 01adsl_primary_related_diseases.R | Primary and related disease/medication caretsian product creation program, use in Tableau |

Documents created in RPubs based on the following programs [http://rpubs.com/mahajvi]:  

| Program name | Description |
| --- | --- |
| 100_adsl_analysis.rmd | Metabolic and RMSD disease analysis | 
| 100_adsl_analysis_survival_rmsd.Rmd | RMSD Survival analysis |
| 100_adsl_analysis_survival_met.Rmd  | Metabolic Survival analysis |
| 100_adsl_medicine.Rmd | Metabolic and RMSD Medicine analysis |

D3 network program and the corresponding files are: [https://coursephd.github.io]

Displays created in Tableau [https://public.tableau.com/profile/frlht#!/]  

Not fully done:
20_cooccur.R           Co-occurence analysis for the diseases, may help in printing the network graph  
101_age_sparkline.Rmd  Sparkline analysis to show many graphs at one go.  

