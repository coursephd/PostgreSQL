#################################
# Code to generate DIAGNOSIS data
#################################

library(data.table)
setwd ("C:\\Users\\Lucky\\Documents\\Hospital data\\01_31JUL2016\\source")

diag <- fread("Diagnosis.csv", check.names = FALSE)
diag2 <- diag[, c(1, 8, 9), with=FALSE ]
setnames(diag2, "MR No.", "MRNo")

# Code ** NOT YET CODED for the missing code values
# Sort the data by patient and day
diag2 <- diag2[, Code2 := ifelse(Code =="", "ZZZ999", Code), ] [order(MRNo, Code2)]

setkeyv (diag2, c("MRNo", "Code2", "Description"))
diag3 <- unique(diag2)

diag4 <- diag3[, `:=`(primarycode = Code2,
                      primarydesc = Description),]

diag3 <- diag3[, c(1, 3, 4), with =FALSE]
diag4 <- diag4[, c(1, 5, 6), with =FALSE]

# set the ON clause as keys of the tables:
setkey(diag3,MRNo)
setkey(diag4,MRNo)

# perform the join
prim_diag <- merge(diag3,diag4, all=TRUE, allow.cartesian = TRUE)