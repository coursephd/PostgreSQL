
######################
## Call all programs
######################

source("C:\\Users\\VinayMahajan\\Desktop\\Misc\\Hospital data\\01_31JUL2016\\prgm\\rmsd_metabolic_subset.R")
source("C:\\Users\\VinayMahajan\\Desktop\\Misc\\Hospital data\\01_31JUL2016\\prgm\\diagnosis_primary.R")
source("C:\\Users\\VinayMahajan\\Desktop\\Misc\\Hospital data\\01_31JUL2016\\prgm\\diagnosis_primary_month.R")
source("C:\\Users\\VinayMahajan\\Desktop\\Misc\\Hospital data\\01_31JUL2016\\prgm\\diagnosis.R")
source("C:\\Users\\VinayMahajan\\Desktop\\Misc\\Hospital data\\01_31JUL2016\\prgm\\vital_sign.R")
source("C:\\Users\\VinayMahajan\\Desktop\\Misc\\Hospital data\\01_31JUL2016\\prgm\\lab.R")

## Calculate all the common variables from all datasets
## merge them back onto the individual datasets

keep <- c("MRNo", "Age", "AgeIn", "Gender", "City", "Country",
          "Bloodgrp", "data", "type", "visday")

# Combine all datasets
nvisall <- data.table ( rbind (diag2 [, keep, with = FALSE],
                              vitals2[, keep, with = FALSE],
                              lab2   [, keep, with = FALSE][MRNo !=""] ) )
# Add a dummry variable
nvisall <- nvisall[, cal :=1]

# Unique patient values
n1unq <- unique ( nvisall [, c("MRNo", "Age", "AgeIn", "Gender", "City","Country"), with = FALSE] )

# Count distinct visits for each domain based on diag2, vitals2 and lab2:
n1vis <- nvisall[ , .(novis = uniqueN(visday) ), by = .(MRNo, data) ]
n2vis <- dcast(n1vis, MRNo ~ data, value.var = "novis")

# No of diseases
n1dis <- unique ( diag2 [, c("MRNo", "noofdis"), with = FALSE] )

# Blood group creation
n1blood <- unique( nvisall [ Bloodgrp != "", c("MRNo", "Bloodgrp"), with =FALSE])

# OP, IP creation
n1type <- unique (nvisall [ type != "", c("MRNo", "type", "cal"), with =FALSE])
n2type <- dcast(n1type, MRNo ~ type, value.var = "cal")

# Combine all variables into 1 dataset ADSL
adsl <- Reduce(function(...) merge(..., all = TRUE, by = "MRNo"),
                list(n1unq, n1dis, n1blood, n2type, n2vis))

setnames(adsl, "vital", "nvis_vital")
setnames(adsl, "diag", "nvis_diag")
setnames(adsl, "lab", "nvis_lab")

#n1all [is.na (n21all)] <- 0


# http://stackoverflow.com/questions/21560500/data-table-merge-based-on-date-ranges
# Version 2 of the code

## The foverlaps function requires both tables to have a start and end range,
# and the "y" table to be keyed
vitals4_tran[, tempday := visday]  ## Add a redundant day column to use as the end range
setkey(diag2, MRNo, min, max) ## Set the key for patient IDs ("y" table)

## Find the overlaps, remove the redundant lossDate2 column, and add the inPolicy column:
ans2 <- foverlaps(vitals4_tran,
                  diag2,
                  by.x=c("MRNo", "visday", "tempday"))[, `:=`(inPolicy=T, tempday=NULL)]

## Update rows where the claim was out of policy:
ans2[is.na(min), inPolicy:=F]

## Remove duplicates (such as policyNumber==123 & claimNumber==3),
##  and add policies with no claims (policyNumber==125):
setkey(ans2, MRNo,Code2,visday, min) ## order the results
setkey(ans2, MRNo, Code2) ## set the key to identify unique values
ans2 <- rbindlist(list(
  ans2, ## select only the unique values
  diag2[!.(ans2[, unique(MRNo)])] ## policies with no claims
), fill=T)

ans20 <- ans2 [, -c("Age", "AgeIn", "City", "Country", "Bloodgrp",
                    "Gender","noofdis"), with = FALSE]

ans3 <- Reduce(function(...) merge(..., all = TRUE, by = "MRNo"),
               list(ans20, adsl))

write.csv(ans3, file ="C:\\Users\\VinayMahajan\\Desktop\\Misc\\Hospital data\\01_31JUL2016\\analysis\\adiag_vital.csv", na=" ")


########################################################
# Create a vertical vesion of diag_vital
########################################################
vitals5[, tempday := visday]  ## Add a redundant day column to use as the end range
setkey(diag2, MRNo, min, max) ## Set the key for patient IDs ("y" table)

## Find the overlaps, remove the redundant lossDate2 column, and add the inPolicy column:
ans2 <- foverlaps(vitals5,
                  diag2,
                  by.x=c("MRNo", "visday", "tempday"))[, `:=`(inPolicy=T, tempday=NULL)]

## Update rows where the claim was out of policy:
ans2[is.na(min), inPolicy:=F]

## Remove duplicates (such as policyNumber==123 & claimNumber==3),
##  and add policies with no claims (policyNumber==125):
setkey(ans2, MRNo,Code2,visday, min) ## order the results
setkey(ans2, MRNo, Code2) ## set the key to identify unique values
ans2 <- rbindlist(list(
  ans2, ## select only the unique values
  diag2[!.(ans2[, unique(MRNo)])] ## policies with no claims
), fill=T)

ans20 <- ans2 [, -c("Age", "AgeIn", "City", "Country", "Bloodgrp",
                    "Gender","noofdis"), with = FALSE]

ans3 <- Reduce(function(...) merge(..., all = TRUE, by = "MRNo"),
               list(ans20, adsl))

write.csv(ans3, file ="C:\\Users\\VinayMahajan\\Desktop\\Misc\\Hospital data\\01_31JUL2016\\analysis\\adiag_vital_vertical.csv", na=" ")


########################################################
# Create primary disease data, combination of 1 disease
# considered as primary disease and display all other
# diseases
########################################################

prim_diag2 <- Reduce(function(...) merge(..., all = TRUE, by = "MRNo"),
               list(prim_diag, adsl))

write.csv(prim_diag2, file ="C:\\Users\\VinayMahajan\\Desktop\\Misc\\Hospital data\\01_31JUL2016\\analysis\\primary_diag.csv", na=" ")



########################################################
# Create primary disease data, combination of 1 disease
# considered as primary disease and display all other
# diseases
########################################################

prim_diag_mon2 <- Reduce(function(...) merge(..., all = TRUE, by = "MRNo"),
                     list(prim_diag_mon, adsl))

write.csv(prim_diag_mon2, file ="C:\\Users\\VinayMahajan\\Desktop\\Misc\\Hospital data\\01_31JUL2016\\analysis\\primary_diag_mon.csv", na=" ")


########################################################
# Create a subsetted data for RMSD and Metabolic diseases
# For analysis create a few additional variables
########################################################

# Get all the disease by month for these patients
subset4 <- Reduce(function(...) merge(..., all.x = TRUE, by = "MRNo"),
                  list(subset3, prim_diag_mon2))


subset5 <- merge (x = discat[, -c("Description"), with =FALSE],
                 y = subset4,
                 all = TRUE,
                 by.x = "Code",
                 by.y = "primarycode")

# Code non metabolic and non RMSD diseases as OTHER
subset5$distype[is.na(subset5$distype)] <- "OTHER"

count <- subset5[, .(cnt = uniqueN(MRNo),
                     frq = .N), by =.(distype, Code)]
