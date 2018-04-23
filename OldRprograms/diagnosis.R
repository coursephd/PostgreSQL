#################################
# Code to generate DIAGNOSIS data
#################################

library(data.table)
setwd ("C:\\Users\\Lucky\\Documents\\Hospital data\\01_31JUL2016\\source")

diag <- fread("Diagnosis.csv", check.names = FALSE)

diag <- diag[, `:=` (data = "diag",
                     type = substr(`Patient Id`, 1, 2) ), ]

diag2 <- diag[, c(1, 4, 5, 6, 8, 9, 10, 23, 29, 34, 44, 118, 119), with=FALSE ]

setnames(diag2, "MR No.", "MRNo")
setnames(diag2, "Admission Date", "visdate")
setnames(diag2, "Blood Group", "Bloodgrp")
setnames(diag2, "Age In", "AgeIn")
setnames(diag2, "First Visit Date", "fvisdate")

#setnames(diag2, "Diagnosis Type", "diagtype")
# Create date variables and find the difference
diag2 <- diag2[, visdate := as.POSIXct( gsub("-", "/", visdate), format="%d/%m/%Y") ]
diag2 <- diag2[, fvisdate := as.POSIXct( gsub("-", "/", fvisdate), format="%d/%m/%Y") ]
diag2 <- diag2[, visday := as.Date(visdate) - as.Date(fvisdate) + 1]
diag2 <- diag2[, vismon := round(visday /30.4375, 1)]
diag2 <- diag2[, Age := ifelse(AgeIn =="M", Age/12, Age), ]
diag2 <- diag2[, AgeIn := ifelse(AgeIn =="M", "Y", AgeIn), ]

# Code ** NOT YET CODED for the missing code values
# Sort the data by patient and day
diag2 <- diag2[, Code2 := ifelse(Code =="", "ZZZ999", Code), ] [order(MRNo, visday, Code2)]

# Create number of diagnosis per patient and a counter for each diagnosis
diag2 <- diag2[ , noofdis := uniqueN(Code2), by = .(MRNo) ]

# Count number of unique diagnosis per patient per day
# Sort the data by patient and day
diag2 <- diag2[ , `:=` (IDX = 1: .N), by = .(MRNo, visdate, visday) ] [order(MRNo, visday, IDX, Code2)]

# Get the first date of the diagnosis by each code
diag2 <- diag2 [, min := min(visday), by = .(MRNo, Code2)]

# Get the maximum date of the diagnosis (end date for each patient)
# Use this data with vital sign data to get diagnosis attached to vital sign measurements
diag2 <- diag2 [, max := max(visday), by = MRNo]

setkeyv (diag2, c("MRNo", "Code2", "Description", "min", "max", "type", "IDX", "noofdis"))

diag2 <- unique(diag2)

#write.csv(diag2, file ="C:\\Users\\mahajvi1\\Desktop\\adiag.csv", na=" ")