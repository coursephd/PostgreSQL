library(data.table)
library(ggplot2)
library(dplyr)
library(anytime)
library(knitr)
library(DT)

all_met_rmsd <- readRDS("D:/Hospital_data/ProgresSQL/analysis/01adsl_met_rmsd.rds")

all_met_rmsd <- all_met_rmsd [, `:=` (baseage = min(age)), by =.(mr_no)]
all_met_rmsd <- all_met_rmsd [, `:=` (vismon = round( cdur/30.4375, digits = 0),
                                      imon = round( studyday/30.4375, digits = 0),
                                      cdur = as.numeric(cdur),
                                      studyday =as.numeric(studyday), 
                                      Code = paste(distype, ":", Code, description, sep= " ")), ]

ind_dis <- unique(all_met_rmsd [, c("mr_no", "distype", "Code", "Metabolic", "cdur", "description",
                                    "RMSD", "combine", "baseage", "age", "season", "imon",
                                    "Type", "studyday", "patient_gender", "newdt0", "vismon"), ])

diag8 <- ind_dis[, `:=`(istdt = min(newdt0), 
                        iendt = max(newdt0), 
                        idur = as.numeric(max(newdt0) - min(newdt0) + 1)), by = .(mr_no, Code)]

rm(all_met_rmsd)
rm(ind_dis)


dur    <- c("1 day", "2nd day to 1st month", "2nd month", "3rd month", "4th to 6th month", 
            "6th Month to 1 year", "> 1 year and <= 2 years", "> 2 years and <= 3 years", 
            "> 3 years and <= 4 years", ">4 years and <= 5 years", ">5 years", "Within 1st year")

durlwr <- c(1,           2,            31,            61,             91,           
            181,       366,           732,            1098,           1464, 1830, 1)

durupr <- c(1,         30,          60,          90,           180, 
            365,       731,         1097,          1463,       1839,     9999, 365)

ref <- data.table ( cbind.data.frame (durlwr, durupr, dur ) )
setkey(ref, durlwr, durupr) ## Set the key for patient IDs ("y" table)

# The same analysis for the complete duration and then merge that onto the daig100all dataset

diag800 <- diag8[, tempmon := cdur]  ## Add a redundant day column to use as the end range
setkey(diag800, mr_no)

## Find the overlaps, remove the redundant lossDate2 column, and add the inPolicy column:
diag800rpt <- foverlaps(diag800,
                        ref,
                        by.x=c("cdur", "tempmon"))[, `:=`(inPolicy=T, tempmon=NULL)]

## Update rows where the claim was out of policy:
diag800rpt[is.na(durlwr), inPolicy:=F]

## Remove duplicates (such as policyNumber==123 & claimNumber==3),
##  and add policies with no claims (policyNumber==125):
setkey(diag800rpt, mr_no, cdur, durlwr) ## order the results
setkey(diag800rpt, mr_no, dur) ## set the key to identify unique values

diag800rpt <- rbindlist(list(
  diag800rpt, ## select only the unique values
  diag800[!.(diag800rpt[, unique(mr_no)])] ## policies with no claims
), fill=T)

diag800rpt <- diag800rpt [order(mr_no, studyday, durlwr, Code)]

diag900rpt <- diag800rpt [, .(unqvisit = uniqueN(studyday)), 
                          by = .(mr_no, RMSD, Metabolic, durlwr, durupr, dur, patient_gender, baseage, cdur)] 

diag900rpt_code <- diag800rpt [, .(unqvisit = uniqueN(studyday)), 
                          by = .(mr_no, RMSD, Metabolic, durlwr, durupr, dur, Code, patient_gender, baseage, cdur)] 


diag10000rpt <- diag900rpt [, .(Ntot = uniqueN(mr_no)), by = .(durupr, dur)]
diag10000rmsd <- diag900rpt [RMSD == 1, .(Nrmsd = uniqueN(mr_no)), by = .(durupr, dur)]
diag10000met <- diag900rpt [Metabolic == 1, .(NMetabolic = uniqueN(mr_no)), by = .(durupr, dur)]

diag10000all <- Reduce(function(...) merge(..., all.y = TRUE, by = c("durupr", "dur") ),
                       list(diag10000met, diag10000rmsd, diag10000rpt))

diag10000rpt_c <- diag900rpt_code [, .(Ndis = uniqueN(mr_no)), by = .(durupr, dur, Code)]
diag10000rmsd_c <- diag900rpt_code [RMSD == 1, .(disrmsd = uniqueN(mr_no)), by = .(durupr, dur, Code)]
diag10000met_c <- diag900rpt_code [Metabolic == 1, .(disMetabolic = uniqueN(mr_no)), by = .(durupr, dur, Code)]

diag10000all_c <- Reduce(function(...) merge(..., all.y = TRUE, by = c("durupr", "dur", "Code") ),
                       list(diag10000met_c, diag10000rmsd_c, diag10000rpt_c))


diag100001all <- Reduce(function(...) merge(..., all.y = TRUE, by = c("durupr", "dur") ),
                       list(diag10000all, diag10000all_c))

diag100001all <- diag100001all [, c(6, 1:5, 7:9),] [order(Code, durupr, dur)]


summ_stat <- function (datain, xsub, by, by1, dataout ="D8")
{
  stats_data <- datain [, .(n=uniqueN(mr_no), 
                            mean = round( mean(get(xsub), na.rm = TRUE), digits =1),
                            median= round( median(get(xsub), na.rm = TRUE), digits =2),
                            SD = round( sd(get(xsub), na.rm = TRUE), digits =2),
                            min = round( min(get(xsub), na.rm = TRUE), digits =0),
                            max = round( max(get(xsub), na.rm = TRUE), digits =0)), 
                        by = .(durupr, get(by), get(by1))]
  
  assign(dataout, stats_data, envir=.GlobalEnv)
}

# Dataset for complete duration across
# non overlapping time periods
summ_stat (datain =diag900rpt, 
           xsub = 'cdur', 
           by =c("dur"), 
           by1 =c("patient_gender"), 
           dataout ="nonovr01")

# Dataset for unique visits across
# non overlapping time periods
summ_stat (datain =diag900rpt, 
           xsub = 'unqvisit', 
           by =c("dur"), 
           by1 =c("patient_gender"), 
           dataout ="nonovr011")

summ_stat03 <- function (datain, xsub, by, by1, by2, dataout ="D8")
{
  stats_data <- datain [, .(n=uniqueN(mr_no), 
                            mean = round( mean(get(xsub), na.rm = TRUE), digits =1),
                            median= round( median(get(xsub), na.rm = TRUE), digits =2),
                            SD = round( sd(get(xsub), na.rm = TRUE), digits =2),
                            min = round( min(get(xsub), na.rm = TRUE), digits =0),
                            max = round( max(get(xsub), na.rm = TRUE), digits =0)), 
                        by = .(durupr, get(by), get(by1), get(by2))]
  
  assign(dataout, stats_data, envir=.GlobalEnv)
}

# Dataset with number of visits per disease across 
# different non overlapping time periods
summ_stat03 (datain =diag900rpt_code, 
             xsub = 'unqvisit', 
             by =c("dur"), 
             by1 =c("patient_gender"), 
             by2 =c("Code"),
             dataout ="nonovr03")

setnames(nonovr03, "get.2", "Code")
setnames(nonovr03, "get.1", "patient_gender")
setnames(nonovr03, "get", "dur")

nonovr03 <- nonovr03 [order(Code, patient_gender, durupr, dur)]


diag950rpt <- unique(diag900rpt_code[, c("Code", "durupr", "dur")] )
diag950rpt <- diag950rpt[, val :=1]

diag950rpt_t <- dcast(data = diag950rpt, 
                      Code ~ dur,
                      value.var = c("val"))
diag950rpt_t <- diag950rpt_t [, c (1, 7, 8, 9, 10, 11, 12, 13, 2, 3, 4, 5, 6),]

rm(diag900rpt)
