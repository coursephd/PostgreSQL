########################################################################################
#
# 115_comoRbidity.R related programs
#
# Four files are required for the comorbidity analysis with the comoRbidity package:
#
# (1) patientData         [patient_id, patient_sex, patient_dateBirth]
# (2) diagnosisData       [patient_id, admission_id, diagnosis_code]
# (3) admissionData       [patient_id, admission_id, admissionStartDate]
# (4) indexDiseaseCodes   [Code, Agg]
#
# Create these files based on 01adsl_met_rmsd.rds data
#
# # file:///C:/Users/user/Downloads/bty315_suppl_data.pdf
#
# library(devtools)
# install_bitbucket( "ibi_group/disgenet2r" )
# Did not work but the next line workd
# install_bitbucket( "ibi_group/comoRbidity")

# install_github("aGutierrezSacristan/comoRbidity")
########################################################################################

library(data.table)
library(tidyverse)
library(readxl)
library(scales)
library(devtools)
library(comoRbidity)

all_met_rmsd02 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/01adsl_met_rmsd.rds")
all_met_rmsd02 <- all_met_rmsd02 [, Code := ifelse (Code == " " | Code == "", "** Not yet coded", Code),]
all_met_rmsd02 <- all_met_rmsd02 [, description:= ifelse (description == "" | description ==" ", "** Not yet coded", description),]
all_met_rmsd02 <- all_met_rmsd02 [, Code02 := paste(distype, ":", Code, ":", description, sep =""), ]
all_met_rmsd02 <- all_met_rmsd02 [, Med02 := paste(Type_med, ":", Coded_med, sep =""), ]

code <- unique (all_met_rmsd02 [, c("Code02", "Code", "description", "distype"),])
code <- code [, pos := str_locate(Code, "\\."),]
code <- code [, `:=` (main = substr(Code, 1, pos-1),
                      second = substr(Code, pos+1, length(Code) ) ),]

code02 <- code [ second == "0", c("main", "description", "distype"),]
#setnames(code02, "main", "maincode")
setnames(code02, "description", "maindesc")

code03 <- merge (x = code, 
                 y = code02 [, -c("distype"), ], 
                 by = c("main") )

all_met_rmsd03 <- merge(x = all_met_rmsd02,
                        y = code03 [, c("Code02", "main", "maindesc"), ],
                        by = c("Code02") )
all_met_rmsd03 <- all_met_rmsd03 [, maincode := paste(main, ":", maindesc, sep=""),]

all_met_rmsd03 <- all_met_rmsd03 [ Code != "** Not yet coded" ]
all_met_rmsd03 <- all_met_rmsd03 [ dateofbirth != "" ]

setnames(all_met_rmsd03, "mr_no", "patient_id")
setnames(all_met_rmsd03, "patient_gender", "patient_sex")
setnames(all_met_rmsd03, "dateofbirth", "patient_dateBirth")
setnames(all_met_rmsd03, "vis", "admission_id")
setnames(all_met_rmsd03, "maincode", "diagnosis_code")
setnames(all_met_rmsd03, "newdt0", "admissionStartDate")

########################################################################
# (1) patientData         [patient_id, patient_sex, patient_dateBirth]
########################################################################
a01patientData <- unique ( all_met_rmsd03 [, c("patient_id", "patient_sex", "patient_dateBirth"), ])

########################################################################
# (2) diagnosisData       [patient_id, admission_id, diagnosis_code]
########################################################################
a02diagnosisData <- unique ( all_met_rmsd03 [, c("patient_id", "admission_id", "diagnosis_code"), ])

########################################################################
# (3) admissionData       [patient_id, admission_id, admissionStartDate]
########################################################################
a03admissionData <- unique ( all_met_rmsd03 [, c("patient_id", "admission_id", "admissionStartDate"), ])

########################################################################
# (4) indexDiseaseCodes   [Code, Agg]
########################################################################
a04indexDiseaseCodes <- unique ( all_met_rmsd03 [ distype == "Metabolic", c("diagnosis_code", "distype"), ])
setnames(a04indexDiseaseCodes, "diagnosis_code", "Code")
setnames(a04indexDiseaseCodes, "distype", "Agg")

###########################
# Save the files as rdata
###########################
saveRDS (a01patientData, "D:/Hospital_data/ProgresSQL/analysis/a01patientData.rds")
saveRDS (a02diagnosisData, "D:/Hospital_data/ProgresSQL/analysis/a02diagnosisData.rds")
saveRDS (a03admissionData, "D:/Hospital_data/ProgresSQL/analysis/a03admissionData.rds")
saveRDS (a04indexDiseaseCodes, "D:/Hospital_data/ProgresSQL/analysis/a04indexDiseaseCodes.rds")

save (a01patientData, file = "D:/Hospital_data/ProgresSQL/analysis/allData.Rdata")

###############################
# Save the files as txt files
###############################
fwrite (a01patientData, "D:/Hospital_data/ProgresSQL/analysis/patientData.txt", sep = "\t")
fwrite (a02diagnosisData, "D:/Hospital_data/ProgresSQL/analysis/diagnosisData.txt", sep = "\t")
fwrite (a03admissionData, "D:/Hospital_data/ProgresSQL/analysis/admissionData.txt", sep = "\t")
fwrite (a04indexDiseaseCodes, "D:/Hospital_data/ProgresSQL/analysis/indexDiseaseCode.txt", sep = "\t")

##############################
# Execute the query function
##############################

ff <- query( databasePth = "D:/Hospital_data/ProgresSQL/analysis",
             codesPth = "D:/Hospital_data/ProgresSQL/analysis",
             admissionDataSep = "-",
             birthDataSep = "-")
ff

aggQuery <- query( databasePth = "D:/Hospital_data/ProgresSQL/analysis",
                   codesPth = "D:/Hospital_data/ProgresSQL/analysis",
                   admissionDataSep = "-",
                   birthDataSep = "-",
                   aggregatedDis = TRUE)

aggQuery

queryIntra <- query( databasePth = "D:/Hospital_data/ProgresSQL/analysis",
                     codesPth = "D:/Hospital_data/ProgresSQL/analysis",
                     admissionDataSep = "-",
                     birthDataSep = "-",
                     intraCodes = TRUE,
                     aggregatedDis = FALSE)
queryIntra

aggQueryIntra <- query( databasePth = "D:/Hospital_data/ProgresSQL/analysis",
                        codesPth = "D:/Hospital_data/ProgresSQL/analysis",
                        admissionDataSep = "-",
                        birthDataSep = "-",
                        intraCodes = TRUE,
                        aggregatedDis = TRUE)
aggQueryIntra


summaryDB(input = ff,
          maleCode = "M",
          femaleCode ="F")
## Checking the input object

load ("D:/Hospital_data/ProgresSQL/analysis/allData.RData")

populationAge ( input = ff,
                codesPth = "D:/Hospital_data/ProgresSQL/analysis",
                databasePth = "D:/Hospital_data/ProgresSQL/analysis/",
                type = "together",
                interactive = FALSE,
                verbose = TRUE)
## Checking the input object

diseasePrevalence(input = ff,
                  maleCode = "M",
                  femaleCode ="F",
                  databasePth = "D:/Hospital_data/ProgresSQL/analysis")
## Checking the input object