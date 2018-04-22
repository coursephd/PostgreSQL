# PostgreSQL
Hospital data access

Creation of programs using the PostgreSQL database and SQL codes

The following files should be used in the following sequence:

Dataset creation files:

100_adsl_sqlpart.sql [SQL code to download the necessary data from the server]  
100_adsl.R           [R code to create data for patient level information: Demog + Visit + diseases + treatments + services]  
02other_data.R       [SQL code is added at the top of the file and then followed by R code]  


Analysis creation files:

100_adsl_analysis.rmd                  [R Knitr file to produce a word document]  
01adsl_primary_related_diseases.R      [Primary and related disease/medication caretsian product creation program, use in Tableau]
