
######################
## Call all programs
######################

source("C:\\Users\\Lucky\\Documents\\Hospital data\\01_31JUL2016\\prgm\\rmsd_metabolic_subset.R")
source("C:\\Users\\Lucky\\Documents\\Hospital data\\01_31JUL2016\\prgm\\diagnosis_primary.R")
source("C:\\Users\\Lucky\\Documents\\Hospital data\\01_31JUL2016\\prgm\\diagnosis_primary_month.R")
source("C:\\Users\\Lucky\\Documents\\Hospital data\\01_31JUL2016\\prgm\\diagnosis.R")
source("C:\\Users\\Lucky\\Documents\\Hospital data\\01_31JUL2016\\prgm\\vital_sign.R")
source("C:\\Users\\Lucky\\Documents\\Hospital data\\01_31JUL2016\\prgm\\lab.R")

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

# Combine the diag2 dataset and discat data and
# determine diseases which are in RMSD and Metabolic categories vs. OTHER categories
subset5 <- merge (x = discat[, -c("Description"), with =FALSE],
                  y = diag2[, -c("Code"), with =FALSE],
                  all = TRUE,
                  by.x = "Code",
                  by.y = "Code2")

subset5 <- subset5 [,-c("Age", "AgeIn", "Gender", "City", "Country",
                        "Bloodgrp", "data", "type", "noofdis"), with =FALSE]

# Code non metabolic and non RMSD diseases as OTHER
subset5$distype[is.na(subset5$distype)] <- "OTHER"

# Count number of diseases and freq count for each patient
# ??????????????????????????????????????????????????????
# ???? Understand why Code2 is not coming out correctly
# ??????????????????????????????????????????????????????
subset6 <- subset5[, .(cnt = uniqueN(Code),
                     frq = .N), by =.(MRNo, distype)]

# Transpose the data and get in 1 row per patient
subset7 <- dcast(subset6,
                 MRNo ~ distype,
                 value.var = c("cnt", "frq"),
                 fill = 0)

# Combine all variables into 1 dataset ADSL
adsl <- Reduce(function(...) merge(..., all = TRUE, by = "MRNo"),
               list(n1unq, n1dis, n1blood, n2type, n2vis, subset7))

setnames(adsl, "vital", "nvis_vital")
setnames(adsl, "diag", "nvis_diag")
setnames(adsl, "lab", "nvis_lab")

########################################################
# Create a subsetted data for RMSD and Metabolic diseases
# For analysis create a few additional variables
########################################################

adsl_sub <- Reduce(function(...) merge(..., all.x = TRUE, by = "MRNo"),
                  list(subset3, adsl))

# Merge this information onto diag2 dataset
diag8 <- Reduce(function(...) merge(..., all.x = TRUE, by = "MRNo"),
                  list(adsl_sub, subset5 ))
write.csv(diag8, file ="C:\\Users\\Lucky\\Documents\\Hospital data\\01_31JUL2016\\analysis\\rmsd_met_diag.csv", na=" ")


# Merge this information onto vitals5 dataset
vitals8 <- Reduce(function(...) merge(..., all.x = TRUE, by = "MRNo"),
                list(adsl_sub, vitals5 ))
write.csv(vitals8, file ="C:\\Users\\Lucky\\Documents\\Hospital data\\01_31JUL2016\\analysis\\rmsd_met_vital.csv", na=" ")


# http://stackoverflow.com/questions/21560500/data-table-merge-based-on-date-ranges
# Version 2 of the code

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

ans3 <- Reduce(function(...) merge(..., all.y = TRUE, by = "MRNo"),
               list(ans20, adsl_sub))

write.csv(ans3, file ="C:\\Users\\Lucky\\Documents\\Hospital data\\01_31JUL2016\\analysis\\rmsd_met_vital_vertical.csv", na=" ")


########################################################
# Create primary disease data, combination of 1 disease
# considered as primary disease and display all other
# diseases
########################################################

prim_diag2 <- Reduce(function(...) merge(..., all.y = TRUE, by = "MRNo"),
                     list(prim_diag, adsl_sub))

write.csv(prim_diag2, file ="C:\\Users\\Lucky\\Documents\\Hospital data\\01_31JUL2016\\analysis\\rmsd_met_primary_diag.csv", na=" ")


########################################################
# Create primary disease data, combination of 1 disease
# considered as primary disease and display all other
# diseases
########################################################

prim_diag_mon2 <- Reduce(function(...) merge(..., all.y = TRUE, by = "MRNo"),
                         list(prim_diag_mon, adsl_sub))

write.csv(prim_diag_mon2, file ="C:\\Users\\Lucky\\Documents\\Hospital data\\01_31JUL2016\\analysis\\rmsd_met_primary_diag_mon.csv", na=" ")



# Create a lookup table to get the cumulative view of the patients
# Merge this onto individual datasets to create multiple records for patients
# Use vismon variable and create many to many join 

dur    <- c(">=1 day", ">=1 month", ">=2 months", ">=3 months", ">=6 months", 
            ">=1 year", ">=2 years", ">=3 years", ">=4 years", ">=5 years")

durlwr <- c(0,           1,            2,            3,             6,           
            12,          24,           36,            48,             60)

durupr <- c(999,         999,          999,          999,           999, 
            999,         999,           999,          999,           999)

ref <- data.table ( cbind.data.frame (durlwr, durupr, dur ) )

# http://stackoverflow.com/questions/21560500/data-table-merge-based-on-date-ranges
# Version 2 of the code

## The foverlaps function requires both tables to have a start and end range,
# and the "y" table to be keyed
diag8[, tempmon := vismon]  ## Add a redundant day column to use as the end range
setkey(ref, durlwr, durupr) ## Set the key for patient IDs ("y" table)

## Find the overlaps, remove the redundant lossDate2 column, and add the inPolicy column:
diag8rpt <- foverlaps(diag8,
                      ref,
                      by.x=c("vismon", "tempmon"))[, `:=`(inPolicy=T, tempmon=NULL)]

## Update rows where the claim was out of policy:
diag8rpt[is.na(durlwr), inPolicy:=F]

## Remove duplicates (such as policyNumber==123 & claimNumber==3),
##  and add policies with no claims (policyNumber==125):
setkey(diag8rpt, MRNo, Code, vismon, durlwr) ## order the results
setkey(diag8rpt, MRNo, Code, dur) ## set the key to identify unique values
diag8rpt <- rbindlist(list(
  diag8rpt, ## select only the unique values
  diag8[!.(diag8rpt[, unique(MRNo)])] ## policies with no claims
), fill=T)

# Count number of unique patients, with only 1 visit, 2 visits,3 visits, etc.
diag9rpt <- diag8rpt [, unqvisit := uniqueN(dur), by = .(MRNo)] [order(MRNo, durlwr)]
diag10rpt <- diag9rpt [, .(nopat = uniqueN(MRNo)), by = .(unqvisit)] [order(unqvisit)]

# Create a file for 
write.csv(diag9rpt, file ="C:\\Users\\Lucky\\Documents\\Hospital data\\01_31JUL2016\\analysis\\rmsd_met_diag_repeat.csv", na=" ")



# Cumulative view on the vital signs data

vitals8[, tempmon := vismon]  ## Add a redundant day column to use as the end range
setkey(ref, durlwr, durupr) ## Set the key for patient IDs ("y" table)

## Find the overlaps, remove the redundant lossDate2 column, and add the inPolicy column:
vitals8rpt <- foverlaps(vitals8 [vismon > 0],
                        ref,
                        by.x=c("vismon", "tempmon"))[, `:=`(inPolicy=T, tempmon=NULL)]

## Update rows where the claim was out of policy:
vitals8rpt[is.na(durlwr), inPolicy:=F]

## Remove duplicates (such as policyNumber==123 & claimNumber==3),
##  and add policies with no claims (policyNumber==125):
setkey(vitals8rpt, MRNo, vitalparam, vismon, durlwr) ## order the results
setkey(vitals8rpt, MRNo, vitalparam, dur) ## set the key to identify unique values
vitals8rpt <- rbindlist(list(
  vitals8rpt, ## select only the unique values
  vitals8[!.(vitals8rpt[, unique(MRNo)])] ## policies with no claims
), fill=T)


# Create a file for vital signs observations repeated for 
# cumulative time point
write.csv(vitals8rpt, file ="C:\\Users\\Lucky\\Documents\\Hospital data\\01_31JUL2016\\analysis\\rmsd_met_vital_vertical_repeat.csv", na=" ")


# ????????????????????????????????????????????????????
# DOES NOT WORK -- NOT ENOUGHN SPACE
# ????????????????????????????????????????????????????

# Create primary diagnosis and all other diagnosis view by year
# This gives a view about patient having many diseases simultaneously 
# along with other diseases

setkeyv (diag8rpt, c("MRNo", "Code", "Description", "dur", "durlwr"))
diag30 <- unique(diag8rpt)

diag40 <- diag30[, `:=`(primarycode = Code,
                      primarydesc = Description,
                      primarydur  = dur,
                      primarydurlwr = durlwr),]

diag50 <- diag40[, c("MRNo", "Age", "AgeIn", "Gender", "City","Country", "Bloodgrp",
                     "Code","distype", "Description", "dur", "durlwr", "durupr",
                     "Metabolic", "RMSD", "combine"), with =FALSE]

diag60 <- diag40[, c("MRNo", "primarycode", "primarydesc", 
                     "primarydur", "primarydurlwr"), with =FALSE]

# set the ON clause as keys of the tables:
setkey(diag50,MRNo, dur)
setkey(diag60,MRNo, primarydur)

# perform the join
prim_diag_dur_repeat <- data.table( merge(diag50,diag60, 
                                          all=TRUE, 
                                          allow.cartesian = TRUE) )


# Clean up some space
rm (list = ls( pattern = "lab*") )
rm (list = ls( pattern = "n1*") )


# Create a file for vital signs observations repeated for 
# cumulative time point
write.csv(prim_diag_dur, file ="C:\\Users\\Lucky\\Documents\\Hospital data\\01_31JUL2016\\analysis\\rmsd_met_primary_diag_dur_repeat.csv", na=" ")




# Find difference between 2 consecutive visits
# This may give us some idea about the data

diff <- diag8 [, c("MRNo", "visday"), with =FALSE]
setkey(diag8, MRNo, visday)
diff <- unique(diff) [order (MRNo, visday)]
diff <- diff[,diff:=c(NA,diff(visday)),by=MRNo]
summary(diff$diff)
