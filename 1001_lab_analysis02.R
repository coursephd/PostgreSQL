
labname <- fread("D:/Hospital_data/ProgresSQL/analysis/1001_lab_lookup.csv")
labname <- labname [, result02 := toupper( str_replace_all(source, " ", "") ),]
labname02 <- unique( labname [, c("result02", "target"),] )

lab <- fread("D:\\Hospital_data\\01_31JUL2016\\source\\Lab.csv")

lab <- lab[, `:=` (data = "lab",
                   type = substr(`Patient Id`, 1, 2) ), ]

lab2 <- lab[, c(1, 2, 4, 5, 6, 7, 8, 20, 22, 28, 35, 40, 61,
                62, 76, 124, 150, 151, 161, 162), with=FALSE ]

setnames(lab2, "Patient ID", "patient_id")
setnames(lab2, "MR No.", "mr_no")

setnames(lab2, "Sample Date", "smpdate")
setnames(lab2, "Conducted Date", "condate")
setnames(lab2, "Result Label", "result")
setnames(lab2, "With In Normal", "withinnormal")

setnames(lab2, "Test Name", "param")
setnames(lab2, "Test Value (Numeric)", "aval")
setnames(lab2, "Test Value", "avalc")

setnames(lab2, "Blood Group", "Bloodgrp")
setnames(lab2, "Age In", "AgeIn")
setnames(lab2, "First Visit Date", "fvisdate")

# Create date variables and find the difference
lab2 <- lab2[, smpdate := as.POSIXct( gsub("-", "/", smpdate), format="%d/%m/%Y") ]
lab2 <- lab2[, condate := as.POSIXct( gsub("-", "/", condate), format="%d/%m/%Y") ]
lab2 <- lab2[, fvisdate := as.POSIXct( gsub("-", "/", fvisdate), format="%d/%m/%Y") ]

# Sort the data by patient and day
lab2 <- lab2 [, visday := as.Date(smpdate) - as.Date(fvisdate) + 1] [order(mr_no, visday, result)]
lab2 <- lab2 [, result02 := toupper( str_replace_all(result, " ", "") ),]

lab22 <- lab2 [ result %in% c("Blood Sugar - Fasting",
                              "Blood Sugar - Fasting and PP",                                   
                              "Blood Sugar - Post Lunch",                                         
                              "Blood Sugar - Post Prandial",
                              "Blood Sugar - Random", 
                              "Blood Sugar Fasting - Glucometer",                                 
                              "Blood Sugar Postprandial - Glucometer",                            
                              "Blood Sugar Random - Glucometer",                                  
                              "Blood sugar -  1 1/2 hrs",                                         
                              "Blood sugar -  2  hrs",
                              "Blood sugar -  2hrs",  
                              "Blood sugar -  3 hrs", 
                              "Blood sugar -  3hrs",  
                              "Blood sugar - 1 hrs",  
                              "Blood sugar - 1/2 hrs",
                              "Blood sugar - 1hrs",   
                              "Blood sugar - 2 hrs",  
                              "Blood sugar - Fasting",
                              "Blood sugar- fasting", 
                              "Blood sugar- postparndial",                                        
                              "Blood sugar- postprandial",                                        
                              "Blood sugar-postprandial ( after giving 50 grms of glucose given)",
                              "Blood sugar-postprandial ( after giving 75grms of glucose given)",
                              "HbA1C (Glycosylated Haemoglobin)-Whole Blood",
                              "HbA1c(Glycosylated Haemoglobin)-Whole Blood",
                              "LDL - CHOLESTEROL",
                              "LDL : HDL - RATIO",
                              "MEAN BLOOD GLUCOSE            ( PAST 60 DAYS)",
                              "MEAN BLOOD GLUCOSE      ( PAST 60",
                              "RANDOM BLOOD SUGAR", 
                              "RANDOM URINE FOR CREATININE",
                              "RANDOM URINE SUGAR",
                              "Random blood sugar",
                              "Random urine sugar",
                              "TOTAL : HDL - RATIO",
                              "TOTAL : HDL-RATIO",
                              "Urine - sugar",
                              "Urine sugar - 1 1/2 hrs",  
                              "Urine sugar - 1 1/2hrs",   
                              "Urine sugar - 1 hrs",      
                              "Urine sugar - 1/2  hrs",   
                              "Urine sugar - 1/2 hrs",    
                              "Urine sugar - 1hrs",       
                              "Urine sugar - 2 hrs",      
                              "Urine sugar - 3 hrs",      
                              "Urine sugar - fasting",    
                              "Urine sugar 2 hrs",        
                              "Urine sugar- fasting",     
                              "Urine sugar- postparndial",
                              "Urine sugar- postprandial",
                              "Urine sugar-Fasting",      
                              "Urine sugar-Random",       
                              "Urine sugar-post prandial",
                              "urine sugar- post prandial",
                              "urine sugar-posLunch",
                              "VLDL - CHOLESTEROL",
                              "VLDL- CHOLESTEROL"  ), ]

lab23 <- merge (x = lab22,
               y = labname02,
               by = c("result02"),
               all.y = TRUE)

lab23 <- lab23 [ , `:=`( COUNT = .N , labid = 1:.N ),
               by = .(mr_no, patient_id, smpdate, visday, target) ]

lab23 <- lab23 [ as.numeric(aval) >= 0]
lab23 <- lab23 [ mr_no != ""]

# This version creates correct transposed data
lab23_tran <- dcast(lab23,
                   mr_no + patient_id + smpdate + fvisdate + visday +
                     AgeIn + Bloodgrp  +labid ~ target,
                   value.var = c("aval") )

saveRDS(lab23_tran, file="D:/Hospital_data/ProgresSQL/analysis/1001_lab_analysis02.Rds")
save(lab23_tran, file="D:/Hospital_data/ProgresSQL/analysis/1001_lab_analysis02.Rdata")
fwrite(lab23_tran, file="D:/Hospital_data/ProgresSQL/analysis/1001_lab_analysis02.csv")
































































=====================================================================





































install.packages("SqlRender")
install.packages("DatabaseConnectorJars")
install.packages("DatabaseConnector")
install.packages("FeatureExtraction") # Not available for R 3.6.1
devtools::install_github("ohdsi/FeatureExtraction") #,  args = "--no-multiarch")

options(buildtools.check = function(action) TRUE )

remotes::install_github("OHDSI/FeatureExtraction")

install.packages("OhdsiSharing")
install.packages("OhdsiRTools")
install.packages("BigKnn")
install.packages("PatientLevelPrediction")



drat::addRepo("OHDSI")
install.packages("FeatureExtraction", type="source") # Not available for R 3.6.1
install.packages("PatientLevelPrediction")


library(data.table)

library(pkgbuild)
rtools_path()
check_rtools()
has_rtools()
find_rtools()

library(reticulate)
use_virtualenv("r-tensorflow")
library(keras)
import("scipy")


library(reticulate)
#use_python()

#devtools::install_github("rstudio/keras")
library(keras)
install_keras()

install.packages("usethis")
usethis::edit_r_environ()

Sys.setenv('USESPECIALPYTHONVERSION'='python3.7')
devtools::install_bitbucket("Floooo/PythonInR")

clust <- code02 [main %in% c("U1", "V9", "A1", "P2", "D1", "J1", "S3", "N2")]

py01 <- py01 [ order(all_vis, row_no, pid) ]

admissions <- unique ( py01 [, c("row_no", "pid", "vis", "admtime"),])
diagnosis <- unique ( py01 [, c("row_no", "pid", "vis", "main"),])
patients <- unique ( py01 [, c("row_no", "pid", "patient_gender", "dateofbirth", "stdt_IP")])
patients <- patients [, dateofdeath := "", ]



py01 <- py01 [, `:=`(row_no = 1,
                     admid = paste(Type, vis, sep =""),
                     admtime = paste(newdt0, "00:00:00"),
                     unxdt = paste( as.numeric(as.POSIXct(newdt0, format="%Y-%m-%d")), ".0", sep="" )),]



# Subset only for madhumeha
dis <- unique( py01 [ main %in% c("M2"), c("mr_no"), ] )

py01 <- py01 [ mr_no %in% dis$mr_no ]


load(system.file("extdata", "comorbidity.RData", package="comoRbidity"))


qresult <- data.table (comor_obj@qresult)
search <- data.table (comor_obj@search)
tDiseases <- data.table (comor_obj@tDiseases)
indexDisList <- data.table (comor_obj@indexDisList)
indexDis <- data.table (comor_obj@indexDis)


ex1 <- query( databasePth      = system.file("extdata", package="comoRbidity"),
              codesPth         = system.file("extdata", package="comoRbidity"),
              birthDataSep     = "-",
              admissionDataSep = "-",
              python           = FALSE)


library(data.table)
library(tidyverse)
library(cumstats)
library(openxlsx)
library(readxl)
library(scales)

sheet01 <- read_xlsx(path ="D:/Hospital_data/ProgresSQL/analysis/Medicine_names.xlsx", sheet = "Latest3217 records")
sheet02 <- read_xlsx(path ="D:/Hospital_data/ProgresSQL/analysis/Medicine_names.xlsx", sheet = "Reword")

sheet02 <- as.data.table (unique( sheet02 ) )

sheet_all <- merge (x = sheet01, 
                    y = sheet02, 
                    by = c("ShamanaShodhanaPanchakarma"))
setnames(sheet_all, "Reworded", "ayurtype")

all_met_rmsd <- readRDS("D:/Hospital_data/ProgresSQL/analysis/01adsl_vrikka.rds")

all_met_rmsd02 <- merge(x = all_met_rmsd,
                        y = sheet_all [ , c("medicine_name", "ClassicalProprietary", "ayurtype", "MetalbasedtreatmentsRasaoushadhi")],
                        by = c("medicine_name"),
                        all.x = TRUE)

fwrite(all_met_rmsd02, "D:/Hospital_data/ProgresSQL/analysis/01adsl_vrikka.csv")
saveRDS (all_met_rmsd02, "D:/Hospital_data/ProgresSQL/analysis/01adsl_vrikka.rds")

chk <- all_met_rmsd02 [, .(n = uniqueN(mr_no)), by = .(ayurtype)]

#####################################
# Merge single or multiple diseases
#####################################
patcat <- fread("D:\\Hospital_data\\ProgresSQL\\analysis\\105_trt_dis_unq_mult01vrikka_roga.csv")
patcat2 <- unique( patcat [, c("mr_no", "discat"),])

all_met_rmsd02 <- merge (x = all_met_rmsd02,
                         y = patcat2, 
                         by = c("mr_no"), 
                         all.x = TRUE )

all_met_rmsd03 <- unique ( all_met_rmsd02 [, c("mr_no", "studyday", "Code", "description", "mindayVrikkaRoga",
                                               "ayurtype", "ClassicalProprietary", "discat", 
                                               "MetalbasedtreatmentsRasaoushadhi"),])

all_met_rmsd03 <- all_met_rmsd03 [ order(mr_no, studyday, Code)]



#all_met_rmsd02 <- fread("C://Users//mahajvi1//Downloads//01adsl_met_rmsd.csv")
all_met_rmsd02 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/01adsl_met_rmsd.rds")
all_met_rmsd02 <- all_met_rmsd02 [, Code := ifelse (Code == " " | Code == "", "** Not yet coded", Code),]
all_met_rmsd02 <- all_met_rmsd02 [, description:= ifelse (description == "" | description ==" ", "** Not yet coded", description),]
all_met_rmsd02 <- all_met_rmsd02 [, Code02 := paste(distype, ":", Code, ":", description, sep =""), ]
all_met_rmsd02 <- all_met_rmsd02 [, Med02 := paste(Type_med, ":", Coded_med, sep =""), ]

all_met_rmsd03 <- unique(all_met_rmsd02 [, c("mr_no", "studyday", "vis", "all_vis", "cdur", 
                                             "Code", "description", "Code02",
                                             "Type_med", "Coded_med", "Med02")] )

diag3 <- unique( all_met_rmsd03[, c("mr_no", "Code02", "studyday"), ] ) 

diag4 <- diag3 [, `:=`(primarycode = Code02,
                       primaryday = studyday), ]

diag3 <- diag3 [, c("mr_no", "Code02", "studyday"), ]
diag4 <- diag4 [, -c("Code02", "studyday"), ]

# set the ON clause as keys of the tables:
setkey(diag3, mr_no)
setkey(diag4, mr_no)

# perform the join
prim_diag <- merge(x = diag3,
                   y = diag4, 
                   by = c("mr_no"),
                   all=TRUE, 
                   allow.cartesian = TRUE)

prim_diag02 <- prim_diag [ primaryday <= studyday]
prim_diag02 <- prim_diag02 [, diff := studyday - primaryday,]


links <- prim_diag02 [, .(n = uniqueN(mr_no),
                          mean = round ( mean(diff, na.rm = FALSE), 0),
                          sd = round (sd(diff, na.rm = FALSE), 0),
                          median = round (median (diff, na.rm = FALSE), 0),
                          min = round (min (diff, na.rm = FALSE), 0),
                          max = round (max (diff, na.rm = FALSE), 0) ), by = .(primarycode, Code02)]

links <- links [ primarycode %like% c("Metabolic")]

points <- unique( links [, c("primarycode"),] )
points <- points [ order(primarycode)]
points <- points [, num := 1:.N, ]

fwrite(links, "D:/Hospital_data/ProgresSQL/analysis/107_flourish_links.csv")
fwrite(points, "D:/Hospital_data/ProgresSQL/analysis/107_flourish_points.csv")



fwrite(prim_diag02, "D:/Hospital_data/ProgresSQL/analysis/107_prim_sec_diag01.csv")

####################################################################
#
####################################################################


cnts <- base01_met_rmsd [ ! is.na( option_remarks), n := uniqueN(mr_no), by = .(trnvar) ]

# Merge single or multiple diseases
patcat <- fread("D:\\Hospital_data\\ProgresSQL\\analysis\\105_trt_dis_unq_mult01vrikka_roga.csv")
patcat2 <- unique( patcat [, c("mr_no", "discat"),])

cnts2 <- merge (x = cnts,
                y = patcat2, 
                by = c("mr_no"))

# Remove the sections which are not useful
cnt3 <- cnts2 [ section_id %in% (14, 13, 1, 4, 36)]


chk <- base01_met_rmsd [ ! is.na( option_remarks), .(n = uniqueN(mr_no) ), by = .(section_id, trnvar) ]

bbb <- base01_met_rmsd [ ! is.na( option_remarks) ]

hh <- unique (bbb [ trnvar == "sec011_var001_Allopathic Diagnosis", c("mr_no"), ]) #95
hh <- unique (bbb [ trnvar %like% c("sec013_var001_Investigation Reports"), c("mr_no"), ]) #177
hh <- unique (bbb [ trnvar %like% c("sec014_var001_Complaint"), c("mr_no"), ]) #223

hh <- unique (bbb [ trnvar %like% c("sec014_var002_Doctor's Notes"), c("mr_no"), ]) #59
hh <- unique (bbb [ trnvar %like% c("sec036_var001_Main Complaints"), c("mr_no"), ]) #115
hh <- unique (bbb [ trnvar %like% c("sec036_var002_Associated Symptoms"), c("mr_no"), ]) #38
hh <- unique (bbb [ trnvar %like% c("sec085_var001_Complaint"), c("mr_no"), ]) #20



library(data.table)
library(tidyverse)
library(cumstats)

#all_met_rmsd02 <- fread("C://Users//mahajvi1//Downloads//01adsl_met_rmsd.csv")
all_met_rmsd02 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/01adsl_met_rmsd.rds")
all_met_rmsd02 <- all_met_rmsd02 [, Code := ifelse (Code == " " | Code == "", "** Not yet coded", Code),]
all_met_rmsd02 <- all_met_rmsd02 [, description:= ifelse (description == "" | description ==" ", "** Not yet coded", description),]
all_met_rmsd02 <- all_met_rmsd02 [, Code02 := paste(distype, ":", Code, ":", description, sep =""), ]
all_met_rmsd02 <- all_met_rmsd02 [, Med02 := paste(Type_med, ":", Coded_med, sep =""), ]

code <- unique (all_met_rmsd02 [, c("Code02", "Code", "description"),])
code <- code [, pos := str_locate(Code, "\\."),]
code <- code [, `:=` (main = substr(Code, 1, pos-1),
                      second = substr(Code, pos+1, length(Code) ) ),]

code02 <- code [ second == "0", c("main", "description"),]
#setnames(code02, "main", "maincode")
setnames(code02, "description", "maindesc")

code03 <- merge (x = code, 
                 y = code02, 
                 by = c("main") )

chk <- unique( code03 [, c("main", "Code", "maindesc", "description"),])

library(data.table)
library(tidyverse)
library(cumstats)

#all_met_rmsd02 <- fread("C://Users//mahajvi1//Downloads//01adsl_met_rmsd.csv")
all_met_rmsd02 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/01adsl_met_rmsd.rds")
all_met_rmsd02 <- all_met_rmsd02 [, Code := ifelse (Code == " " | Code == "", "** Not yet coded", Code),]
all_met_rmsd02 <- all_met_rmsd02 [, description:= ifelse (description == "" | description ==" ", "** Not yet coded", description),]
all_met_rmsd02 <- all_met_rmsd02 [, Code02 := paste(distype, ":", Code, ":", description, sep =""), ]
all_met_rmsd02 <- all_met_rmsd02 [, Med02 := paste(Type_med, ":", Coded_med, sep =""), ]

all_met_rmsd03 <- unique(all_met_rmsd02 [, c("mr_no", "studyday", "vis", "all_vis", "cdur", 
                                             "Code", "description", "Code02",
                                             "Type_med", "Coded_med", "Med02",
                                             "duration", "duration_units", "frequency")] )

dur <- unique ( all_met_rmsd03 [, c("duration", "duration_units")])


library(tidyverse)
library(tidytext)
library(stringr)
library(stringi)
library(data.table)
library(stringdist)

# https://stackoverflow.com/questions/43706729/expand-dates-in-data-table

all_met_rmsd <- readRDS ("D:/Hospital_data/ProgresSQL/analysis/all_met_rmsd02.rds")
a2 <- all_met_rmsd [!Code %in% c("", " ", "M2.0") & refcode == "M2.0"]

a2med <- a2 [, description := paste(Type_med, Coded_med),]
a2med <- a2med [ order(description)]
a2med <- a2med [, Code := paste("M", str_pad(.N, width =4, pad="0"), sep =""), by = .(description) ]

a2all <- rbind(a2 [, c("mr_no", "studyday", "refday", "Code", "description", "refcode", "refdesc")], 
               a2med [, c("mr_no", "studyday", "refday", "Code", "description", "refcode", "refdesc")] )

dis <- unique(a2all[, c("mr_no", "studyday", "refday", "Code", "description", "refcode", "refdesc")])
dis <- dis [, `:=` (refday2 = ifelse(refday >=1, "After", "Before"), 
                    Code = str_replace_all (Code, " ", ""),
                    description = str_replace_all(description, " ", "")),]
dis <- dis [ order(mr_no, studyday, Code, refcode, refdesc)]
dis <- dis [, `:=` (alldis = uniqueN(Code), 
                    nrow = seq_len(.N),
                    nrowend = seq_len(.N) + 4,
                    totrow = .N), by = .(mr_no, refcode, refdesc)]
dis <- dis [, `:=` (alldisbfraftr = uniqueN(Code), 
                    nrowbfraftr = seq_len(.N) ), by = .(mr_no, refcode, refdesc, refday2)]

dis02 <- dis [, .(combdis = paste(unique(Code), collapse = ",", sep = " " )), 
              by = .(mr_no, refcode, refdesc, refday2, alldis, totrow)]

unq01comb <- unique( dis02 [, c("mr_no", "refcode", "refdesc", "alldis", "refday2",
                                "totrow",  "combdis"), ])
unq01comb <- unq01comb [, x := 1, ]

# create a copy
unq02comb <- copy(unq01comb)
setnames(unq02comb, "mr_no", "mr_no2")
setnames(unq02comb, "combdis", "combdis2")

unq01comb <- unq01comb [, combdis := str_replace_all(combdis, ",", "|"), ]

# Merge the datasets on x to get all the combinations

unq03comb <- merge(x = unq01comb, 
                   y = unq02comb [, -c("refcode", "refdesc", "totrow", "alldis"), ], 
                   by = c("x", "refday2"), 
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

unq03comb <- unq03comb[, a01jac := (a / (a + b + c)),]
unq03comb <- unq03comb [ mr_no != mr_no2]

distraj <- unique(unq03comb [tempdis != tempdis2 , c("tempdis", "tempdis2", "a01jac", "refday2"),])

distraj01 <- distraj [, .(distraj = .N), by = .(refday2, a01jac)]

cnt <- unq03comb [, .(cnt = uniqueN( paste(mr_no))), by = .(refday2, a01jac)]

chk022 <- melt(data = chk02,
               id.vars = 1:10,
               measure.vars = 11:20,
               variable.name = "distmethod")

library(data.table)
library(tidyverse)
library(sqldf)
library(healthcareai)

all_met_rmsd02 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/01adsl_met_rmsd.rds")
all_met_rmsd02 <- all_met_rmsd02 [, Code := ifelse (Code == " " | Code == "", "** Not yet coded", Code),]
all_met_rmsd02 <- all_met_rmsd02 [, description:= ifelse (description == "" | description ==" ", "** Not yet coded", description),]
all_met_rmsd02 <- all_met_rmsd02 [, Code02 := paste(distype, ":", Code, ":", description, sep =""), ]
all_met_rmsd02 <- all_met_rmsd02 [, Med02 := paste(Type_med, ":", Coded_med, sep =""), ]

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


chk <- unique( all_met_rmsd03 [, c("main", "maindesc", "maincode", "mr_no",
                                   "patient_gender", "RMSD", "Metabolic", "all_vis",
                                   "baseage", "cdur"),])
chk <- chk [, var :=1, ]
chk <- chk [, cdur := as.numeric(cdur),]
chk02 <- dcast(data = chk,
               mr_no + patient_gender + RMSD + Metabolic + all_vis + baseage + cdur ~ main,
               value.var = c("var"),
               fill = 0)
chk022 <- na.omit(chk02)


# Create prep_data based on the package function
chk03 <- as.data.table (prep_data(d = chk022, mr_no, outcome = M2 )  )


# Split the data into training and test sets
d <- split_train_test(d = chk03,
                      outcome = M2,
                      percent_train = .9)

# Clean and prep the training data, specifying that patient_id is an ID column,
# and tune algorithms over hyperparameter values to predict diabetes
M2_models <- machine_learn(d$train, mr_no, outcome = M2)

# Inspect model specification and performance
M2_models
summary(M2_models)
evaluate(M2_models)
plot(M2_models)
predict(M2_models)

# Make predictions (predicted probability of diabetes) on test data
predict(M2_models, d$test)

flash <- flash_models(d = chk03, mr_no, outcome = M2)

