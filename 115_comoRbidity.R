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

########################################################################
# (1) patientData         [patient_id, patient_sex, patient_dateBirth]
########################################################################
a01patientData <- unique ( all_met_rmsd03 [Code != "** Not yet coded", c("mr_no", "patient_gender", "dateofbirth"), ])
setnames(a01patientData, "mr_no", "patient_id")
setnames(a01patientData, "patient_gender", "patient_sex")
setnames(a01patientData, "dateofbirth", "patient_dateBirth")

########################################################################
# (2) diagnosisData       [patient_id, admission_id, diagnosis_code]
########################################################################
a02diagnosisData <- unique ( all_met_rmsd03 [Code != "** Not yet coded", c("mr_no", "vis", "maincode"), ])
setnames(a02diagnosisData, "mr_no", "patient_id")
setnames(a02diagnosisData, "vis", "admission_id")
setnames(a02diagnosisData, "maincode", "diagnosis_code")

########################################################################
# (3) admissionData       [patient_id, admission_id, admissionStartDate]
########################################################################
a03admissionData <- unique ( all_met_rmsd03 [Code != "** Not yet coded", c("mr_no", "vis", "newdt0"), ])
setnames(a03admissionData, "mr_no", "patient_id")
setnames(a03admissionData, "vis", "admission_id")
setnames(a03admissionData, "newdt0", "admissionStartDate")

########################################################################
# (4) indexDiseaseCodes   [Code, Agg]
########################################################################
a04indexDiseaseCodes <- unique ( all_met_rmsd03 [, c("maincode", "distype"), ])
setnames(a04indexDiseaseCodes, "maincode", "Code")
setnames(a04indexDiseaseCodes, "distype", "Agg")

###########################
# Save the files as rdata
###########################
saveRDS (a01patientData, "D:/Hospital_data/ProgresSQL/analysis/a01patientData.rds")
saveRDS (a02diagnosisData, "D:/Hospital_data/ProgresSQL/analysis/a02diagnosisData.rds")
saveRDS (a03admissionData, "D:/Hospital_data/ProgresSQL/analysis/a03admissionData.rds")
saveRDS (a04indexDiseaseCodes, "D:/Hospital_data/ProgresSQL/analysis/a04indexDiseaseCodes.rds")


##############################
# Execute the query function
##############################

ff <- query( databasePth = databasePth,
             codesPth = diagnosticCodes,
             admissionDataSep = "-",
             birthDataSep = "-")
