#################################
# Code to generate DIAGNOSIS data
#################################

library(data.table)
setwd ("C:\\Users\\Lucky\\Documents\\Hospital data\\01_31JUL2016\\source")

# Get the disease category list for MCSD and Metabolic
discat <- data.table( fread ("../analysis/discategory.csv") )

# Read the diagnosis data
diag <- fread("Diagnosis.csv", check.names = FALSE)
diag2 <- diag[, c(1, 8, 9, 10), with=FALSE ]
setnames(diag2, "MR No.", "MRNo")
setnames(diag2, "Admission Date", "visdate")

# Code ** NOT YET CODED for the missing code values
# Sort the data by patient and day
# Create date variables and find the difference
diag2 <- diag2[, visdate := as.POSIXct( gsub("-", "/", visdate), format="%d/%m/%Y") ] [order(MRNo, Code)]

subset <- merge (x = discat[, -c("Description"), with =FALSE],
                 y = diag2,
                 by = "Code")

# create a dummy variable
subset <- subset[ ,val:=1]

subset2 <- subset[, c("MRNo", "distype", "val"), with =FALSE]
subset2 <- unique(subset2)

subset3 <- dcast (data = subset2,
                  fill =0,
                  MRNo ~ distype,
                  value.var="val")

# Create an indicator variable to determine
# Both Metabolic and RMSD = 99
# Only Metabolic = 1
# Only RMSD = 2
subset3 <- subset3 [Metabolic == 1 & RMSD == 1, combine := 99]
subset3 <- subset3 [Metabolic == 1 & RMSD == 0, combine := 1]
subset3 <- subset3 [Metabolic == 0 & RMSD == 1, combine := 2]
