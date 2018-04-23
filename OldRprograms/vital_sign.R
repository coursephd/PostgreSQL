##########################
## Vital signs data
##########################

library(data.table)
setwd ("C:\\Users\\Lucky\\Documents\\Hospital data\\01_31JUL2016\\source")

# Create Vital sign data from 31st July 2016 version of the data

vitals <- fread("Vitals.csv", check.names = FALSE)
vitals <- vitals[, `:=` (data = "vital",
                         type = substr(`Patient Id`, 1, 2) ), ]

vitals2 <- vitals[, c(1, 3, 4, 6, 8, 9, 10, 18, 24, 30, 35, 44, 119, 120), with=FALSE ]

setnames(vitals2, "MR No.", "MRNo")
setnames(vitals2, "Vital Date", "visdate")
setnames(vitals2, "Vital Parameter", "param")
setnames(vitals2, "Parameter Value", "aval")
setnames(vitals2, "Blood Group", "Bloodgrp")
setnames(vitals2, "Age In", "AgeIn")
setnames(vitals2, "First Visit Date", "fvisdate")

# Create date variables and find the difference
vitals2 <- vitals2[, visdate := as.POSIXct( gsub("-", "/", visdate), format="%d/%m/%Y") ]
vitals2 <- vitals2[, fvisdate := as.POSIXct( gsub("-", "/", fvisdate), format="%d/%m/%Y") ]
vitals2 <- vitals2[, Age := ifelse(AgeIn =="M", Age/12, Age), ]
vitals2 <- vitals2[, AgeIn := ifelse(AgeIn =="M", "Y", AgeIn), ]

# Sort the data by patient and day
vitals2 <- vitals2[, `:=` (visday = as.Date(visdate) - as.Date(fvisdate) + 1,
                           aval = tolower(aval) ) ,]
vitals2 <- vitals2[, vismon := round(visday /30.4375, 1),] [order(MRNo, visday, param)]


# Create a counter variable for each patient per each day
# per each parameter, check if there is more than 1 record
# Currently, timepoint is not considered in the overall calculation
# Count creates overall number of records
# IDX creates the counter

vitals2 <- vitals2[ , `:=`( COUNT = .N , vitalid = 1:.N ),
                    by = .(MRNo, visdate, visday, param) ]

# This version creates correct transposed data with more than 1 record
# Using IDX variable, uniqueness of records are maitained
vitals2_tran <- dcast(vitals2,
                      MRNo + visdate + fvisdate + visday + Age +
                      Gender + Bloodgrp + City + Country + AgeIn                         + vismon +
                      data + type + COUNT + vitalid ~ param,
                      value.var = "aval")

setnames(vitals2_tran, "Breath Holding Time (BHT)", "BHT")
setnames(vitals2_tran, "Hip Circumference", "Hip")
setnames(vitals2_tran, "Waist Circumference", "Waist")

vitals3_tran <- vitals2_tran [, `:=`(
                Weight_cal = as.numeric ( as.character ( gsub("[a-z]", " ", Weight) )),
                BMI_cal    = as.numeric ( as.character ( gsub("[a-z]", " ", BMI) )),
                Height_cal = as.numeric ( as.character ( gsub("[a-z]", " ", Height) )),
                Temp_cal   = as.numeric ( as.character ( gsub("[a-z]", " ", Temp) )),
                Hip_cal    = as.numeric ( as.character ( gsub("[a-z]", " ", Hip) )),
                Pulse_cal  = as.numeric ( as.character ( gsub("[a-z]", " ", Pulse) )),
                BMR_cal    = as.numeric ( as.character ( gsub("[a-z]", " ", BMR) )),
                Waist_cal  = as.numeric ( as.character ( gsub("[a-z]", " ", Waist) )),
                BHT_cal    = as.numeric ( as.character ( gsub("[a-z]", " ", BHT) )),
                Resp_cal   = as.numeric ( as.character ( gsub("[a-z]", " ", Resp) )),
                B.P2 = gsub("[a-z]", " ", B.P)
                                  ), ]

# Split the BP measurements into numeric values
vitals4_tran <- vitals3_tran[, c("BP1", "BP2", "TMP1", "TMP2") := tstrsplit(B.P2, "/", fixed=TRUE), ]
vitals4_tran <- vitals4_tran[, -c("Age", "Gender", "Bloodgrp","City", "Country", "AgeIn"), with = FALSE]

# create a vertical data once again
vitals5 <- melt.data.table(data = vitals4_tran,
                           id.vars =c("MRNo", "visday", "vismon", "visdate","fvisdate",
                                      "vitalid", "COUNT"),
                           measure.vars = c("Weight_cal", "BMI_cal", "Height_cal",
                                            "Temp_cal", "Hip_cal", "Pulse_cal",
                                            "BMR_cal", "Waist_cal", "Resp_cal",
                                            "BP1", "BP2","TMP1", "TMP2"),
                           variable.name = "vitalparam",
                           value.name = "vitalaval",
                           variable.factor = FALSE)
vitals5 <- vitals5[vitalaval != ""]

#write.csv(vitals4_tran, file ="C:\\Users\\VinayMahajan\\Desktop\\Misc\\Hospital data\\01_31JUL2016\\analysis\\aVitals.csv", na=" ")

#
#vitals3 <- subset(vitals2,
#                  !Parameter.Value %in% c("", "0", ".", " ", "*af*brile", "normal", "kgs", "",
#                                          "above *", "high", "*unable*", "x", "y", "*nadi*",
#                                          "*abdominal*"))

#vitals4 <- vitals3[!grepl("`.|-|`|pv|above|unable|x|y|nadi|abdominal|Lungs|*af*|*feb*|-1.4*|n|'|............|R.R*|--|low|could|*head*|*less*|*sitting*|*regular*",
#                          ignore.case = TRUE,
#                          vitals3$Parameter.Value),]

#vitals4 <- data.frame (cbind (adm = gsub("-", "/", vitals4$visdate),
#                              fdt = gsub("-", "/", vitals4$First.Visit.Date),
#                              vitals4
#))
#vitals4 <- data.frame (cbind (adm2 = as.POSIXct(vitals4$adm, , format="%d/%m/%Y"),
#                              fdt2 = as.POSIXct(vitals4$fdt, format="%d/%m/%Y"),
#                              vitals4
#))

