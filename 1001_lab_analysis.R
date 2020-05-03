library(data.table)
library(tidyverse)
library(sqldf)
library(tableplot)


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
lab2 <- lab2 [ , `:=`( COUNT = .N , labid = 1:.N ),
                  by = .(mr_no, patient_id, smpdate, visday, result02) ]

lab2 <- lab2 [ as.numeric(aval) >= 0]
lab2 <- lab2 [ mr_no != ""]

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

# Create an additional variable
lab22 <- lab22 [, avalc2 := paste(avalc, Units, withinnormal, sep= " "),]

lab23 <- lab22 [ withinnormal == "No"]

# This version creates correct transposed data
lab2_tran <- dcast(lab22,
                   mr_no + patient_id + smpdate + fvisdate + visday +
                     AgeIn + Bloodgrp  +labid ~ result02,
                   value.var = c("aval", "avalc2") )

# Merge based on patient and visit
unqpat <- unique( lab23 [, c("mr_no", "patient_id"),] )

all_met_rmsd02 <- fread("D:/Hospital_data/ProgresSQL/analysis/01adsl.csv")
all_met_rmsd02 <- all_met_rmsd02 [, Code := ifelse (icd_code == " " | icd_code == "", "** Not yet coded", icd_code),]
all_met_rmsd02 <- all_met_rmsd02 [, description:= ifelse (description == "" | description ==" ", "** Not yet coded", description),]

all_met_rmsd03 <- merge (x = all_met_rmsd02,
                         y = unqpat, 
                         by = c("mr_no", "patient_id"),
                         all.y = TRUE) 


# Merge based on patient
# this data has patient with at least one abnormal lab value
# this will be merged with the disease and medicine data
# this merge will be independent of the abnormal value

unqpat02 <- unique( lab23 [, c("mr_no"),] )

all_met_rmsd04 <- merge (x = all_met_rmsd02,
                         y = unqpat02, 
                         by = c("mr_no"),
                         all.y = TRUE) 


# Vertical merge
all_met_rmsd05 <- merge (x = all_met_rmsd02,
                         y = lab23, 
                         by = c("mr_no", "patient_id"),
                         all.y = TRUE, 
                         allow.cartesian = TRUE) 

saveRDS(all_met_rmsd05, file="D:/Hospital_data/ProgresSQL/analysis/1001_lab_analysis.Rds")
save(all_met_rmsd05, file="D:/Hospital_data/ProgresSQL/analysis/1001_lab_analysis.Rdata")
fwrite(all_met_rmsd05, file="D:/Hospital_data/ProgresSQL/analysis/1001_lab_analysis.csv")

# Get an idea about diseases
unqdis <- unique(all_met_rmsd03 [, c("mr_no", "Code", "description"),])
count01 <- unqdis [, .(unqpat = uniqueN(mr_no)), by = .(Code, description)]





# Get an idea about diseases
unqdis_cart <- unique(all_met_rmsd04 [, c("mr_no", "Code", "description", "result"),])
count01_cart <- unqdis_cart [, .(unqpat = uniqueN(mr_no)), by = .(Code, description, result)]

##########################################################################
# End of program 
##########################################################################


alab <- fread("D:\\Hospital_data\\01_31JUL2016\\analysis\\aLab.csv")

alab <- alab [, -c(9, 10, 11), ]
alab02 <- melt(data = alab,
               id.vars = c(1:8), 
               value.factor =  FALSE)

alab02 <- alab02 [, variable02 := toupper( str_replace_all(variable, " ", "") ),]
alab02 <- alab02[ , `:=`( COUNT = .N , labid = 1:.N ),
              by = .(MRNo, smpdate, visday, variable02) ]

alab022 <- alab02 [ as.numeric(value) >= 0]
alab022 <- alab022 [ MRNo != ""]

alab03 <- dcast(data = alab02,
                V1 + MRNo + smpdate + fvisdate + visday + AgeIn + Bloodgrp + labid ~ variable02,
                value.var = c("value"))

alab04 <- alab03 [ MRNo != ""]  



tableplot(alab04)


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
lab2 <- lab2[, visday := as.Date(smpdate) - as.Date(fvisdate) + 1] [order(mr_no, visday, result)]

# Create a counter variable for each patient per each day
# per each parameter, check if there is more than 1 record
# Currently, timepoint is not considered in the overall calculation
# Count creates overall number of records
# IDX creates the counter

lab2 <- lab2[ , `:=`( COUNT = .N , labid = 1:.N ),
              by = .(mr_no, patient_id, smpdate, visday, result) ]


