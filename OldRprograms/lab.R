library(data.table)
setwd ("C:\\Users\\Lucky\\Documents\\Hospital data\\01_31JUL2016\\source")

# Create Vital sign data from 31st July 2016 version of the data

lab <- fread("lab.csv", check.names = FALSE)
lab <- lab[, `:=` (data = "lab",
                   type = substr(`Patient Id`, 1, 2) ), ]

lab2 <- lab[, c(2, 4, 5, 6, 7, 8, 20, 22, 28, 35, 40, 61,
                62, 76, 124, 150, 151, 161, 162), with=FALSE ]

setnames(lab2, "MR No.", "MRNo")
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
lab2 <- lab2[, visday := as.Date(smpdate) - as.Date(fvisdate) + 1] [order(MRNo, visday, result)]

# Create a counter variable for each patient per each day
# per each parameter, check if there is more than 1 record
# Currently, timepoint is not considered in the overall calculation
# Count creates overall number of records
# IDX creates the counter

lab2 <- lab2[ , `:=`( COUNT = .N , labid = 1:.N ),
                by = .(MRNo, smpdate, visday, result) ]

# This version creates correct transposed data
lab2_tran <- dcast(lab2,
                      MRNo + smpdate + fvisdate + visday +
                      AgeIn + Bloodgrp  +labid ~ result,
                      value.var = "aval")

#write.csv(lab2_tran, file ="C:\\Users\\VinayMahajan\\Desktop\\Misc\\Hospital data\\01_31JUL2016\\analysis\\aLab.csv")


# Check for the result variable

result <- unique(tolower(lab2$result))

adist(result)
