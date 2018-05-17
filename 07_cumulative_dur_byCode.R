library(data.table)
library(ggplot2)
library(dplyr)
library(anytime)
library(knitr)

all_met_rmsd <- readRDS("D:/Hospital_data/ProgresSQL/analysis/01adsl_met_rmsd.rds")

all_met_rmsd <- all_met_rmsd [, `:=` (baseage = min(age)), by =.(mr_no)]
all_met_rmsd <- all_met_rmsd [, `:=` (vismon = round( cdur/30.4375, digits = 0),
                                      imon = round( studyday/30.4375, digits = 0))]

ind_dis <- unique(all_met_rmsd [, c("mr_no", "distype", "Code", "Metabolic", "cdur", "description",
                                    "RMSD", "combine", "baseage", "age", "season", "imon",
                                    "Type", "studyday", "patient_gender", "newdt0", "vismon"), ])

diag8 <- ind_dis[, `:=`(istdt = min(newdt0), 
                         iendt = max(newdt0), 
                         idur = max(newdt0) - min(newdt0) + 1), by = .(mr_no, Code)]

## The foverlaps function requires both tables to have a start and end range,
# and the "y" table to be keyed
diag8[, tempmon := imon]  ## Add a redundant day column to use as the end range
setkey(diag8, mr_no)

dur    <- c(">=1 day", ">=1 month", ">=2 months", ">=3 months", ">=6 months", 
            ">=1 year", ">=2 years", ">=3 years", ">=4 years", ">=5 years")

durlwr <- c(0,           1,            2,            3,             6,           
            12,          24,           36,            48,           60)

durupr <- c(999,         999,          999,          999,           999, 
            999,         999,           999,          999,           999)

ref <- data.table ( cbind.data.frame (durlwr, durupr, dur ) )
setkey(ref, durlwr, durupr) ## Set the key for patient IDs ("y" table)

## Find the overlaps, remove the redundant lossDate2 column, and add the inPolicy column:
diag8rpt <- foverlaps(diag8,
                      ref,
                      by.x=c("imon", "tempmon"))[, `:=`(inPolicy=T, tempmon=NULL)]

## Update rows where the claim was out of policy:
diag8rpt[is.na(durlwr), inPolicy:=F]

## Remove duplicates (such as policyNumber==123 & claimNumber==3),
##  and add policies with no claims (policyNumber==125):
setkey(diag8rpt, mr_no, imon, durlwr) ## order the results
setkey(diag8rpt, mr_no, dur) ## set the key to identify unique values

diag8rpt <- rbindlist(list(
  diag8rpt, ## select only the unique values
  diag8[!.(diag8rpt[, unique(mr_no)])] ## policies with no claims
), fill=T)


diag9rpt <- diag8rpt [, unqvisit := uniqueN(dur), by = .(mr_no)] [order(mr_no, durlwr)]
diag10rpt <- diag9rpt [, .(nopat = uniqueN(mr_no)), by = .(unqvisit)] [order(unqvisit)]


diag100rpt <- diag9rpt [, .(all = uniqueN(mr_no)), by = .(distype, Code, description, durlwr, dur)]
diag100rmsd <- diag9rpt [RMSD == 1, .(rmsd = uniqueN(mr_no)), by = .(distype, Code, description, durlwr, dur)]
diag100met <- diag9rpt [Metabolic == 1, .(Metabolic = uniqueN(mr_no)), by = .(distype, Code, description, durlwr, dur)]

diag100all <- Reduce(function(...) merge(..., all.y = TRUE, by = c("distype", "Code", "description","durlwr", "dur") ),
               list(diag100met, diag100rmsd, diag100rpt))

