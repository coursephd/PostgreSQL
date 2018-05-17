
library(data.table)
library(zoo)
library(stringi)
library(stringr)
#library(openxlsx)
library(anytime)
library(collapsibleTree)
#library(quantmod)
library(htmltools)
#library(networkD3)
#library(jsonlite)
library(tidyr)

# Cumulative duration
all_met_rmsd <- readRDS("D:/Hospital_data/ProgresSQL/analysis/01adsl_met_rmsd.rds")

all_met_rmsd <- all_met_rmsd [, `:=` (baseage = min(age)), by =.(mr_no)]
all_met_rmsd <- all_met_rmsd [, `:=` (vismon = round( cdur/30.4375, digits = 0))]

# Create a lookup table to get the cumulative view of the patients
# Merge this onto individual datasets to create multiple records for patients
# Use vismon variable and create many to many join 

diag8 <- unique( all_met_rmsd [, c("mr_no", "combine", "RMSD", "Metabolic",
                                    "patient_gender", "vismon", "vis"), ])

dur    <- c(">=1 day", ">=1 month", ">=2 months", ">=3 months", ">=6 months", 
            ">=1 year", ">=2 years", ">=3 years", ">=4 years", ">=5 years")

durlwr <- c(0,           1,            2,            3,             6,           
            12,          24,           36,            48,             60)

durupr <- c(999,         999,          999,          999,           999, 
            999,         999,           999,          999,           999)

ref <- data.table ( cbind.data.frame (durlwr, durupr, dur ) )

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
setkey(diag8rpt, mr_no, vismon, durlwr) ## order the results
setkey(diag8rpt, mr_no, dur) ## set the key to identify unique values
diag8rpt <- rbindlist(list(
  diag8rpt, ## select only the unique values
  diag8[!.(diag8rpt[, unique(mr_no)])] ## policies with no claims
), fill=T)

# Count number of unique patients, with only 1 visit, 2 visits,3 visits, etc.
diag9rpt <- diag8rpt [, unqvisit := uniqueN(dur), by = .(mr_no)] [order(mr_no, durlwr)]
diag10rpt <- diag9rpt [, .(nopat = uniqueN(mr_no)), by = .(unqvisit)] [order(unqvisit)]


diag100rpt <- diag9rpt [, .(all = uniqueN(mr_no)), by = .(dur)]
diag100rmsd <- diag9rpt [RMSD == 1, .(rmsd = uniqueN(mr_no)), by = .(dur)]
diag100met <- diag9rpt [Metabolic == 1, .(Metabolic = uniqueN(mr_no)), by = .(dur)]

diag100all <- cbind(diag100met, diag100rmsd, diag100rpt)
