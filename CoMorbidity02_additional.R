
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
fwrite (a04indexDiseaseCodes [Code %in% c("V2.23", "V2.63"), ], "C:\\Users\\mahajvi1\\OneDrive - Novartis Pharma AG\\Downloads\\indexDiseaseCode.txt", sep = "\t")

# Code %in% c('M10.0','M10.1','M10.2','M2.0','P5.0','P5.1','P5.2','P5.3','P5.4','S16.0')
# "P5.0", "M2.0", "V2.63", "P2.0"

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
             intraCodes = TRUE,
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
               cutOff = 5,
               interactive = FALSE)
## Checking the input object

##############
# step 10:
##############
comorFemale <- comorbidityAnalysis( input = ff,
                                    codesPth = path0,
                                    databasePth = path0,
                                    score = 0,
                                    correctionMethod = "BY", #"fdr",
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
                                  correctionMethod = "BY",  #"fdr",
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
          cutOff = 0, #0.05,
          npairs = 1, #7,
          prop = 0.2,
          title = "Female comorbidity network",
          interactive = FALSE)

##############
# step 12a:
##############
network ( input = comorMale,
          databasePth = path0,
          layout = "layout.kamada.kawai", # "layout.fruchterman.reingold", #  "layout.kamada.kawai",
          selectValue = "score",
          cutOff = 0, #1,
          npairs = 1, #7,
          prop = 0.2,
          title = "Male comorbidity network",
          interactive = FALSE)

##############
# step 13:
##############
heatmapPlot( input = comorFemale,
             selectValue = "score", #jaccard", #"score",
             npairs = 1,
             cutOff = 0,
             verbose = TRUE,
             interactive = FALSE)

heatmapPlot( input = comorMale,
             selectValue = "score", #jaccard", #"score",
             npairs = 1,
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
                                        days = 0, #100, #100,
                                        minPairs = 1, #10, #10,
                                        dataSep = "-",
                                        correctionMethod = "fdr")

summary(as.factor(comorbidityDirection$result))

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
                                         days = 0, #100,
                                         minPairs = 1, #10,
                                         dataSep = "-",
                                         correctionMethod = "fdr")

summary(as.factor(comorbidityDirectionM$result))

##############
# step 16a:
##############
heatmapDirection(comorbidityDirectionM,
                 fromAtoBColor = "darkgreen",
                 fromBtoAColor = "orange",
                 noDirectionColor = "grey",
                 interactive = FALSE)
################################################################################################################



a0chk <- unique( all_met_rmsd03 [ diagnosis_code %in% c("V2.23", "V2.63"), c("patient_id", "diagnosis_code", "patient_sex"),  ] )
a0chk <- a0chk [, nrow := .N, by = .(patient_id)]


a01chk <- all_met_rmsd03 [, .(npat = uniqueN(patient_id)), by = .(diagnosis_code, patient_sex)]


m2 <- all_met_rmsd [!Code %in% c("", " ", "M2.0") & refcode =="M2.0" , refday2 := ifelse(refday >=1, "After", "Before"), ]
m2 <- m2 [ !is.na(refday2)]
a_addmr <- unique( m2 [!Code %in% c(" ", "") & refcode =="M2.0", c("mr_no", "refcode", "Code", "distype", "patient_gender", "refday2"),])
a_addmr <- a_addmr [, numdis := uniqueN(Code), by = .(mr_no)]
a_addmr <- a_addmr [, numtype := ifelse(numdis >1, "> 1 dis", "Only 1 dis" ), ]

a_addmr02 <- a_addmr [, .( ncount = uniqueN(mr_no)), by = .(patient_gender, numtype, refcode)]
a_addmr03 <- a_addmr [numtype == "> 1 dis" & refday2 != "", .( ncount = uniqueN(mr_no)), by = .(patient_gender, numtype, refday2)]

####################################################################
# 086time_dis_patterns_combinations_gender_Macro.R
####################################################################

library(tidyverse)
library(tidytext)
#library(stringr)
library(stringi)
library(data.table)
library(stringdist)
library(scales)

# https://stackoverflow.com/questions/43706729/expand-dates-in-data-table
#dis <- fread("D:/Hospital_data/ProgresSQL/analysis/discategory.csv")
#setnames (dis, "Code", "refcode")

#all_met_rmsd <- readRDS ("D:/Hospital_data/ProgresSQL/analysis/all_met_rmsd02.rds")
all_met_rmsd <- readRDS ("C:\\Users\\mahajvi1\\OneDrive - Novartis Pharma AG\\Downloads\\all_met_rmsd02.rds")


############################################
# Find patients with only the disease 
# same as reference disease
# 1 = patients with only disease
# 99 = patients with more than 1 disease in
# a reference disease category
############################################

addmr <- unique( all_met_rmsd [!Code %in% c(" ", ""), c("mr_no", "refcode", "Code", "distype"),])
addmr <- addmr [, cnt := uniqueN(refcode), by  = .(mr_no)] 
addmr <- addmr [, dis := ifelse(refcode == Code, 1, 0),]
addmr <- addmr [, calc := ifelse(cnt == 1 & dis == 1, 1, 99),]

addmr02 <- addmr [, .(cntr = uniqueN(mr_no)), by = .(distype, refcode, Code, calc)]
addmr03 <- addmr [, .(cntot = uniqueN(mr_no)), by = .(refcode)]
addmr04 <- merge(addmr02, addmr03, by = c("refcode"))
addmr04 <- addmr04 [, perc := percent(cntr / cntot),]
addmr05 <- addmr04 [ refcode == Code]

dis <- unique( addmr05 [ distype != "OTHER", c("refcode"), ] )

addmr06 <- merge (addmr05, dis, by = c("refcode"), all.y = TRUE)

unq <- unique(addmr06 [cntot > 5, c("refcode"),])
unqdis <- unique(unq$refcode)

count <- 1

for ( dis in unqdis[1:uniqueN(unqdis)])
{ print (dis)
  print (count)
  
  a2 <- all_met_rmsd [!Code %in% c("", " ", dis) & refcode == dis]
  a2 <- a2 [, Code := paste(period, Code, sep="_"),]
  
  #a2med <- a2 [, description := paste(Type_med, Coded_med),]
  #a2med <- a2med [ order(description)]
  #a2med <- a2med [, Code := paste("M", str_pad(.N, width =4, pad="0"), sep =""), by = .(description) ]
  
  #a2all <- rbind(a2 [, c("mr_no", "studyday", "refday", "Code", "description", "refcode", "refdesc", "patient_gender")], 
  #               a2med [, c("mr_no", "studyday", "refday", "Code", "description", "refcode", "refdesc", "patient_gender")] )
  
  # Change a2 to a2all
  dis <- unique(a2[, c("mr_no", "studyday", "refday", "Code", "description", "refcode", "refdesc", "patient_gender")])
  dis <- dis [, `:=` (refday2 = ifelse(refday >=1, "After", "Before"), 
                      Code = str_replace_all (Code, " ", ""),
                      description = str_replace_all(description, " ", "")),]
  dis <- dis [ order(mr_no, studyday, Code, refcode, refdesc)]
  dis <- dis [, `:=` (alldis = uniqueN(Code), 
                      nrow = seq_len(.N),
                      nrowend = seq_len(.N) + 4,
                      totrow = .N), by = .(mr_no, refcode, refdesc)]
  dis <- dis [, `:=` (alldisbfraftr = uniqueN(Code), 
                      nrowbfraftr = seq_len(.N) ), by = .(mr_no, refcode, refdesc, refday2, patient_gender)]
  dis <- dis [, `:=` (total = uniqueN(mr_no) ), by = .(refcode, refdesc, refday2, patient_gender)]
  dis <- dis [, `:=` (allcapn = uniqueN(mr_no) ), by = .(refcode, refdesc, patient_gender)]
  
  dis02 <- dis [, .(combdis = paste(unique(Code), collapse = ",", sep = " " ),
                    combdesc = paste(unique(description), collapse = ",", sep = " " )), 
                by = .(mr_no, refcode, refdesc, refday2, patient_gender, alldis, totrow, total, allcapn)]
  
  unq01comb <- unique( dis02 [, c("mr_no", "refcode", "refdesc", "alldis", "refday2","patient_gender",
                                  "totrow",  "combdis", "combdesc", "total", "allcapn"), ])
  unq01comb <- unq01comb [, x := 1, ] 
  
  # create a copy
  unq02comb <- copy(unq01comb)
  setnames(unq02comb, "mr_no", "mr_no2")
  setnames(unq02comb, "combdis", "combdis2")
  
  unq01comb <- unq01comb [, combdis := str_replace_all(combdis, ",", "|"), ]
  
  # Merge the datasets on x to get all the combinations
  
  unq03comb <- merge(x = unq01comb, 
                     y = unq02comb [, -c("refcode", "refdesc", "totrow", "alldis", "total", "allcapn", "combdesc"), ], 
                     by = c("x", "refday2", "patient_gender"), 
                     allow.cartesian = TRUE)
  
  ########################################################
  # Using str_count function to count the common diseases
  # Create tempdis and tempdis2
  #
  # Consider mr_no as the reference patient
  # tempdis: should be lookup
  # a: common in both the strings
  # b: only present in reference patient (mr_no)
  # c: only present in other patient (mr_no2)
  # d: complete absence -- not sure how to calculate this
  ########################################################
  
  unq03comb <- unq03comb [, `:=` (tempdis = str_replace_all(combdis, ",", "|"), 
                                  tempdis2 = str_replace_all(combdis2, ",", "|")),]
  
  unq03comb <- unq03comb [, `:=` (cntdis = str_count(tempdis, "\\|") + 1, 
                                  cntdis2 = str_count(tempdis2, "\\|") + 1), ]
  
  unq03comb <- unq03comb[, `:=` (a = str_count(combdis2, tempdis)),]
  
  unq03comb <- unq03comb [, `:=` (b = cntdis - a,
                                  c = cntdis2 - a),  ]
  
  unq03comb <- unq03comb[, `:=` (a01jac = (a / (a + b + c)),
                                 a02dice = (2 * a / (2* a + b + c) ),
                                 a03CZEKANOWSKI = (2 * a / (2* a + b + c) ),
                                 a04jac3w = (3 * a / (3* a + b + c) ),
                                 a05nei_li = (2 * a /  (a + b + a + c) ),
                                 a06sokalsneath1 = (a / (a + 2 * b + 2 * c)) ),]
  unq03comb <- unq03comb [ mr_no != mr_no2]
  
  
  maxscr <- unq03comb[, .(maxscr = max(a01jac) ), by = .(mr_no, refcode, total, allcapn, totrow, alldis, refday2, patient_gender, combdis, combdesc)]
  maxscr_t <- dcast (data = maxscr, 
                     mr_no + patient_gender + refcode + totrow + alldis ~ refday2, 
                     value.var = c("maxscr", "combdis", "combdesc"))
  
  maxscr02 <- maxscr [, .(scr = uniqueN(mr_no)), by = .(refcode, total, allcapn, refday2, patient_gender, cut(maxscr,  
                                                                                                              seq(0, 1, .25), 
                                                                                                              include.lowest = TRUE,
                                                                                                              ordered_result = TRUE))]
  maxscr02_t <- dcast(data = maxscr02, 
                      refcode + patient_gender + allcapn + cut ~ refday2,
                      value.var = c("scr", "total"))
  
  maxscr03 <- maxscr [, .(scr = uniqueN(mr_no)), by = .(refcode, total, allcapn, maxscr, combdis, combdesc, refday2, patient_gender)]
  maxscr03_t <- dcast(data = maxscr03, 
                      refcode + patient_gender + allcapn + combdis + combdesc + maxscr ~ refday2,
                      value.var = c("scr", "total"))
  
  maxscr04_t <- unq03comb [, .(scr = .N), by = .(mr_no, refcode, total, allcapn, refday2, patient_gender, cut(a01jac,  
                                                                                                              seq(0, 1, .25), 
                                                                                                              include.lowest = TRUE,
                                                                                                              ordered_result = TRUE))]
  maxscr04_t <- maxscr04_t [, numrow := .N, by = . (mr_no, refcode, refday2, patient_gender)]
  
  totscr <- unq03comb [, .( rowcnt = .N), by = .(refcode, total, allcapn, refday2, patient_gender, cut(a01jac, 
                                                                                                       seq(0, 1, .25), 
                                                                                                       include.lowest = TRUE,
                                                                                                       ordered_result = TRUE) )]
  totscr02 <- unq03comb [, .(totn = .N), by = .(refcode, refday2, patient_gender, total, allcapn )]
  totscr02 <- merge (totscr, totscr02, by = c("refcode", "refday2", "patient_gender", "total", "allcapn"))
  totscr02 <- totscr02 [, perc := percent( rowcnt / totn),]
  totscr02_t <- dcast(data = totscr02,
                      refcode + patient_gender + allcapn + cut ~ refday2,
                      value.var = c("perc", "totn", "rowcnt", "total"))
  
  assign ( paste("D01maxscr_t", count, sep="") ,  maxscr_t)
  assign ( paste("D02maxscr02_t", count, sep="") ,  maxscr02_t)
  assign ( paste("D03maxscr03_t", count, sep="") ,  maxscr03_t)
  assign ( paste("D03maxscr04_t", count, sep="") ,  maxscr04_t)
  
  assign ( paste("t02totscr02_t", count, sep="") ,  totscr02_t)
  
  count = count + 1
  
}

allD01maxscr_t <- rbindlist(mget(ls(pattern = "D01maxscr_t*")), fill = TRUE)
allD02maxscr02_t <- rbindlist(mget(ls(pattern = "D02maxscr02_t*")), fill = TRUE)
allD03maxscr03_t <- rbindlist(mget(ls(pattern = "D03maxscr03_t*")), fill = TRUE)
allD03maxscr04_t <- rbindlist(mget(ls(pattern = "D03maxscr04_t*")), fill = TRUE)

allt02maxscr02_t <- rbindlist(mget(ls(pattern = "t02totscr02_t*")), fill = TRUE)

rm(list = ls( pattern='^D01maxscr_t*'))
rm(list = ls( pattern='^D02maxscr02_t*'))
rm(list = ls( pattern='^D03maxscr03_t*'))
rm(list = ls( pattern='^D03maxscr04_t*'))
rm(list = ls( pattern='^t02totscr02_t*'))

fwrite(allD01maxscr_t, "D:/Hospital_data/ProgresSQL/analysis/086time_dis_indPat_max.csv")
fwrite(allD02maxscr02_t, "D:/Hospital_data/ProgresSQL/analysis/086time_dis_refcode_max.csv")
fwrite(allD03maxscr03_t, "D:/Hospital_data/ProgresSQL/analysis/086time_dis_indtrajectory.csv")
fwrite(allD03maxscr04_t, "D:/Hospital_data/ProgresSQL/analysis/086time_dis_indPat_freqcat.csv")
fwrite(allt02maxscr02_t, "D:/Hospital_data/ProgresSQL/analysis/086time_dis_refcode_allfreq_max.csv")
############################################################################################################################################


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
fwrite (a04indexDiseaseCodes [Code %in% c('A1.0', 'M2.0') ], "C:\\Users\\mahajvi1\\OneDrive - Novartis Pharma AG\\Downloads\\indexDiseaseCode.txt", sep = "\t")

# Code %in% c('M10.0','M10.1','M10.2','M2.0','P5.0','P5.1','P5.2','P5.3','P5.4','S16.0')
# "P5.0", "M2.0", "V2.63", "P2.0"

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
             intraCodes = TRUE,
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
          prop = 0.2,
          title = "Female comorbidity network",
          interactive = FALSE)

##############
# step 12a:
##############
network ( input = comorMale,
          databasePth = path0,
          layout = "layout.kamada.kawai", # "layout.fruchterman.reingold", #  "layout.kamada.kawai",
          selectValue = "score",
          cutOff = 1,
          npairs = 7,
          prop = 0.2,
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
                                        days = 100, #100,
                                        minPairs = 10, #10,
                                        dataSep = "-",
                                        correctionMethod = "fdr")

summary(as.factor(comorbidityDirection$result))

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

summary(as.factor(comorbidityDirectionM$result))

##############
# step 16a:
##############
heatmapDirection(comorbidityDirectionM,
                 fromAtoBColor = "darkgreen",
                 fromBtoAColor = "orange",
                 noDirectionColor = "grey",
                 interactive = FALSE)