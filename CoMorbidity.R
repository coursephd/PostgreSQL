library(healthcareai)

d_train <- pima_diabetes[1:700, ]

library(devtools)
#install_github("aGutierrezSacristan/comoRbidity")
install_bitbucket( "ibi_group/disgenet2r" )
install_bitbucket( "ibi_group/comoRbidity")

library(data.table)
library(tidyverse)
library(readxl)
library(stringi)
library(stringr)
library(comoRbidity)

all_met_rmsd02 <- readRDS("C:\\Users\\mahajvi1\\OneDrive - Novartis Pharma AG\\Downloads\\01adsl_met_rmsd.rds")
#all_met_rmsd02 <- fread("C:\\Users\\mahajvi1\\OneDrive - Novartis Pharma AG\\Downloads\\01adsl_met_rmsd.csv")
all_met_rmsd02 <- all_met_rmsd02 [, Code := ifelse (Code == " " | Code == "", "** Not yet coded", Code),]
all_met_rmsd02 <- all_met_rmsd02 [, description:= ifelse (description == "" | description ==" ", "** Not yet coded", description),]
all_met_rmsd02 <- all_met_rmsd02 [, Code02 := paste(distype, ":", Code, ":", description, sep =""), ]
#all_met_rmsd02 <- all_met_rmsd02 [, Med02 := paste(Type_med, ":", Coded_med, sep =""), ]

code <- unique (all_met_rmsd02 [, c("Code02", "Code", "description", "distype"),])
code <- code [, pos := ifelse(str_count(Code, "\\.") >0,str_locate(Code, "\\."), 0 ), ]
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
#setnames(all_met_rmsd03, "maincode", "diagnosis_code")
setnames(all_met_rmsd03, "Code", "diagnosis_code")
setnames(all_met_rmsd03, "newdt", "admissionStartDate")

##################################################
# If the patients need to be subsetted by disease
# Patients with unique combinations of diseases
##################################################
dis01 <- unique (all_met_rmsd03 [distype != "OTHER", c("patient_id", "diagnosis_code"), ])
dis01 <- dis01 [, var :=1, ]
dis01_t <- dcast (data = dis01,
                  patient_id ~ diagnosis_code,
                  value.var = c("var"),
                  fill = "0")

dis02 <- dis01_t [ M2.0 == 1, c("patient_id"), ] 

all_met_rmsd033 <- all_met_rmsd03 [ patient_id %in% dis02$patient_id ]
  
########################################################################
# (1) patientData         [patient_id, patient_sex, patient_dateBirth]
########################################################################
a01patientData <- unique ( all_met_rmsd03 [, c("patient_id", "patient_sex", "patient_dateBirth"), ])
#a01patientData <- unique ( all_met_rmsd033 [, c("patient_id", "patient_sex", "patient_dateBirth"), ])

########################################################################
# (2) diagnosisData       [patient_id, admission_id, diagnosis_code]
########################################################################
a02diagnosisData <- unique ( all_met_rmsd03 [, c("patient_id", "admission_id", "diagnosis_code"), ])
#a02diagnosisData <- unique ( all_met_rmsd033 [, c("patient_id", "admission_id", "diagnosis_code"), ])

########################################################################
# (3) admissionData       [patient_id, admission_id, admissionStartDate]
########################################################################
a03admissionData <- unique ( all_met_rmsd03 [, c("patient_id", "admission_id", "admissionStartDate"), ])
#a03admissionData <- unique ( all_met_rmsd033 [, c("patient_id", "admission_id", "admissionStartDate"), ])

########################################################################
# (4) indexDiseaseCodes   [Code, Agg]
########################################################################
a04indexDiseaseCodes <- unique ( all_met_rmsd03 [ , c("diagnosis_code", "distype"), ])
#a04indexDiseaseCodes <- unique ( all_met_rmsd033 [ , c("diagnosis_code", "distype"), ])

setnames(a04indexDiseaseCodes, "diagnosis_code", "Code")
setnames(a04indexDiseaseCodes, "distype", "Agg")

###########################
# Save the files as rdata
###########################
#saveRDS (a01patientData, "C:\\Users\\mahajvi1\\OneDrive - Novartis Pharma AG\\Downloads\\a01patientData.rds")
#saveRDS (a02diagnosisData, "C:\\Users\\mahajvi1\\OneDrive - Novartis Pharma AG\\Downloads\\a02diagnosisData.rds")
#saveRDS (a03admissionData, "C:\\Users\\mahajvi1\\OneDrive - Novartis Pharma AG\\Downloads\\a03admissionData.rds")
#saveRDS (a04indexDiseaseCodes, "C:\\Users\\mahajvi1\\OneDrive - Novartis Pharma AG\\Downloads\\a04indexDiseaseCodes.rds")

#save (a01patientData, file = "C:\\Users\\mahajvi1\\OneDrive - Novartis Pharma AG\\Downloads\\allData.Rdata")

###############################
# Save the files as txt files
###############################
fwrite (a01patientData, "C:\\Users\\mahajvi1\\OneDrive - Novartis Pharma AG\\Downloads\\patientData.txt", sep = "\t")
fwrite (a02diagnosisData, "C:\\Users\\mahajvi1\\OneDrive - Novartis Pharma AG\\Downloads\\diagnosisData.txt", sep = "\t")
fwrite (a03admissionData, "C:\\Users\\mahajvi1\\OneDrive - Novartis Pharma AG\\Downloads\\admissionData.txt", sep = "\t")
fwrite (a04indexDiseaseCodes [Code %in% c( "P5.0", "M2.0") ], "C:\\Users\\mahajvi1\\OneDrive - Novartis Pharma AG\\Downloads\\indexDiseaseCode.txt", sep = "\t")

# [Code %in% c( "P5.0", "A6.0") ]
#cnt <- all_met_rmsd03 [, .(n = uniqueN(patient_id)), by =.(Code02, Code)]

# "P5:Prameha", "A1:Aamadosha"

# M2:Madhumeha
# "P2:Pandu", , "A6:Amlapitta"
# "M10:Medoroga", "V1:Vaatarakta", "A2:Aamavaata"

##############################
# Execute the query function
##############################

path0 <- "C:\\Users\\mahajvi1\\OneDrive - Novartis Pharma AG\\Downloads\\"

##############
# step 1:
##############
ff <- query( databasePth = path0,
             codesPth = path0,
             admissionDataSep = "-",
             intraCodes = FALSE,
             birthDataSep = "-")
ff

#all00 <- load(paste0(path0, "/allData.RData"))

##############
# step 2:
##############
aggQuery <- query( databasePth = path0,
                   codesPth = path0,
                   admissionDataSep = "-",
                   birthDataSep = "-",
                   aggregatedDis = TRUE)
aggQuery

##############
# step 3:
##############
queryIntra <- query( databasePth = path0,
                     codesPth = path0,
                     admissionDataSep = "-",
                     birthDataSep = "-",
                     intraCodes = FALSE,
                     aggregatedDis = FALSE)
queryIntra

##############
# step 4:
##############
aggQueryIntra <- query( databasePth = path0,
                        codesPth = path0,
                        admissionDataSep = "-",
                        birthDataSep = "-",
                        intraCodes = FALSE,
                        aggregatedDis = FALSE)
aggQueryIntra

##############
# step 5:
##############
summaryDB(input = ff,
          maleCode = "M",
          femaleCode ="F")
## Checking the input object

#load ("C:\\Users\\mahajvi1\\OneDrive - Novartis Pharma AG\\Downloads\\allData.RData")

##############
# step 6:
##############
populationAge ( input = ff,
                codesPth = path0,
                databasePth = path0,
                type = "separate",
                interactive = FALSE,
                verbose = TRUE)
## Checking the input object

##############
# step 7:
##############
diseasePrevalence(input = ff,
                  maleCode = "M",
                  femaleCode ="F",
                  databasePth = path0)

##############
# step 8:
##############
diagnosticUse( codesPth = path0,
               input = ff,
               cutOff = 0,
               interactive = FALSE)
## Checking the input object

##############
# step 9:
##############
diagnosticUse( codesPth = path0,
               input = aggQueryIntra,
               cutOff = 0,
               interactive = FALSE)
## Checking the input object

##############
# step 10:
##############
comorFemale <- comorbidityAnalysis( input = ff,
                                    codesPth = path0,
                                    databasePth = path0,
                                    score = 0,
                                    correctionMethod = "fdr",
                                    correctedPval = 1,
                                    ageRange = c( 0, 100 ),
                                    sex = "F",
                                    verbose = TRUE)
comorFemale

#step10 <- extract( comorFemale)
#head(step10)


##############
# step 11:
##############
comorMale <- comorbidityAnalysis( input = ff,
                                    codesPth = path0,
                                    databasePth = path0,
                                    score = 0,
                                    correctionMethod = "fdr",
                                    correctedPval = 1,
                                    ageRange = c( 0, 100 ),
                                    sex = "M",
                                    verbose = TRUE)
comorMale

save(comorMale, file=paste0(path0, "comorMale.RData"))
save(comorFemale, file=paste0(path0, "comorFemale.RData"))

##############
# step 12:
##############

comorFemale01 <- as.data.table(comorFemale@result)

network ( input = comorFemale,
          databasePth = path0,
          layout = "layout.fruchterman.reingold",
          selectValue = "score",
          cutOff = 1, #0.05,
          npairs = 7,
          prop = 1,
          title = "Female comorbidity network",
          interactive = FALSE)

##############
# step 12a:
##############
network ( input = comorMale,
          databasePth = path0,
          layout = "layout.kamada.kawai",
          selectValue = "score",
          cutOff = 1,
          npairs = 7,
          prop = 1,
          title = "Male comorbidity network",
          interactive = FALSE)

##############
# step 13:
##############
heatmapPlot( input = comorFemale,
             selectValue = "score", #jaccard", #"score",
             npairs = 2,
             cutOff = 0,
             verbose = TRUE,
             interactive = FALSE)

heatmapPlot( input = comorMale,
             selectValue = "score", #jaccard", #"score",
             npairs = 2,
             cutOff = 0,
             verbose = TRUE,
             interactive = FALSE)

# Not working
##############
# step 14:
##############
srAnalysis <- sexRatio(female = comorFemale,
                       male = comorMale,
                       fisherTest = 0)

srAnalysis02 <- srAnalysis[as.numeric(srAnalysis$SR) <= -0.5
                         | as.numeric(srAnalysis$SR) >= 0.5,]
##############
# step 14a:
##############
heatmapSexRatio(srAnalysis,
                interactive = FALSE)

heatmapSexRatio(srAnalysis02,
                interactive = FALSE)

##############
# step 15:
##############
comorbidityDirection <- directionality( input = comorFemale,
                                        databasePth = path0,
                                        sex = "F",
                                        ageRange = c(0,100),
                                        days = 100,
                                        minPairs = 10,
                                        dataSep = "-",
                                        correctionMethod = "fdr")

##############
# step 16:
##############
heatmapDirection(comorbidityDirection,
                 fromAtoBColor = "darkgreen",
                 fromBtoAColor = "orange",
                 noDirectionColor = "grey",
                 interactive = FALSE)


##############
# step 15a:
##############
comorbidityDirectionM <- directionality( input = comorMale,
                                        databasePth = path0,
                                        sex = "M",
                                        ageRange = c(0,100),
                                        days = 100,
                                        minPairs = 10,
                                        dataSep = "-",
                                        correctionMethod = "fdr")

##############
# step 16a:
##############
heatmapDirection(comorbidityDirectionM,
                 fromAtoBColor = "darkgreen",
                 fromBtoAColor = "orange",
                 noDirectionColor = "grey",
                 interactive = FALSE)