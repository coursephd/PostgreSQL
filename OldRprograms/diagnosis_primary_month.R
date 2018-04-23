#################################
# Code to generate DIAGNOSIS data
#################################

library(data.table)
setwd ("C:\\Users\\Lucky\\Documents\\Hospital data\\01_31JUL2016\\source")

diag <- fread("Diagnosis.csv", check.names = FALSE)
diag2 <- diag[, c(1, 8, 9, 10), with=FALSE ]
setnames(diag2, "MR No.", "MRNo")
setnames(diag2, "Admission Date", "visdate")

# Code ** NOT YET CODED for the missing code values
# Sort the data by patient and day
# Create date variables and find the difference
diag2 <- diag2[, visdate := as.POSIXct( gsub("-", "/", visdate), format="%d/%m/%Y") ]
diag2 <- diag2[, Code2 := ifelse(Code =="", "ZZZ999", Code), ] [order(MRNo, Code2)]
# Only extract month part
diag2 <- diag2[, month := format(as.Date(visdate), "%m"), ]

setkeyv (diag2, c("MRNo", "Code2", "Description", "month"))
diag3 <- unique(diag2)

diag4 <- diag3[, `:=`(primarycode = Code2,
                      primarydesc = Description,
                      primarymon  = month),]

diag5 <- diag4[, c(1, 3, 5, 6), with =FALSE]
diag6 <- diag4[, c(1, 7, 8, 9), with =FALSE]

# set the ON clause as keys of the tables:
setkey(diag5,MRNo, month)
setkey(diag6,MRNo, primarymon)

# perform the join
prim_diag_mon <- merge(diag5,diag6, all=TRUE, allow.cartesian = TRUE)
