library(data.table)
library(ggplot2)
library(dplyr)
library(anytime)
library(knitr)

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

## The foverlaps function requires both tables to have a start and end range,
# and the "y" table to be keyed
diag8[, tempmon := idur]  ## Add a redundant day column to use as the end range
setkey(diag8, mr_no)


dur    <- c("1 day", "2nd day to 1st month", "2nd month", "3rd month", "4th to 6th month", 
            "6th Month to 1 year", "> 1 year and <= 2 years", "> 2 years and <= 3 years", 
            "> 3 years and <= 4 years", ">4 years and <= 5 years", ">5 years", "Within 1st year")

durlwr <- c(1,           2,            31,            61,             91,           
            181,       366,           732,            1098,           1464, 1830, 1)

durupr <- c(1,         30,          60,          90,           180, 
            365,       731,         1097,          1463,       1839,     9999, 365)

ref <- data.table ( cbind.data.frame (durlwr, durupr, dur ) )
setkey(ref, durlwr, durupr) ## Set the key for patient IDs ("y" table)

## Find the overlaps, remove the redundant lossDate2 column, and add the inPolicy column:
diag8rpt <- foverlaps(diag8,
                      ref,
                      by.x=c("idur", "tempmon"))[, `:=`(inPolicy=T, tempmon=NULL)]

## Update rows where the claim was out of policy:
diag8rpt[is.na(durlwr), inPolicy:=F]

## Remove duplicates (such as policyNumber==123 & claimNumber==3),
##  and add policies with no claims (policyNumber==125):
setkey(diag8rpt, mr_no, idur, durlwr) ## order the results
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
diag900rpt <- diag800rpt [, unqvisit := 1:.GRP, by = .(mr_no, durlwr)] 
diag900rpt <- diag900rpt [, unqdisvis := 1:.GRP, by = .(mr_no, durlwr, Code)] 

diag10000rpt <- diag900rpt [, .(N = uniqueN(mr_no)), by = .(durlwr, dur)]
diag10000rmsd <- diag900rpt [RMSD == 1, .(Nrmsd = uniqueN(mr_no)), by = .(durlwr, dur)]
diag10000met <- diag900rpt [Metabolic == 1, .(NMetabolic = uniqueN(mr_no)), by = .(durlwr, dur)]

diag10000all <- Reduce(function(...) merge(..., all.y = TRUE, by = c("durlwr", "dur") ),
                     list(diag10000met, diag10000rmsd, diag10000rpt))

diag10001all <- Reduce(function(...) merge(..., all.y = TRUE, by = c("durlwr", "dur") ),
                       list(diag100all, diag10000all))

diag10001all <- diag10001all [order(distype, Code, description, durlwr, dur)]


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

# Dataset with duration for each disease across
# non overlapping time periods
summ_stat03 (datain =diag9rpt, 
           xsub = 'idur', 
           by =c("dur"), 
           by1 =c("patient_gender"), 
           by2 =c("Code"),
           dataout ="nonovr02")

# Dataset with number of visits per disease across 
# different non overlapping time periods
summ_stat03 (datain =diag900rpt, 
             xsub = 'unqdisvis', 
             by =c("dur"), 
             by1 =c("patient_gender"), 
             by2 =c("Code"),
             dataout ="nonovr03")


# Dataset for complete duration across
# non overlapping time periods, by distype
summ_stat03 (datain =diag900rpt, 
           xsub = 'cdur', 
           by =c("dur"), 
           by1 =c("patient_gender"), 
           by2 =c("distype"),
           dataout ="nonovr01dis")

# Dataset for unique visits across
# non overlapping time periods, by distype
summ_stat03 (datain =diag900rpt, 
           xsub = 'unqvisit', 
           by =c("dur"), 
           by1 =c("patient_gender"),
           by2 =c("distype"),
           dataout ="nonovr011dis")

setnames(nonovr011dis, "get.2", "distype")
setnames(nonovr011dis, "get.1", "patient_gender")
setnames(nonovr011dis, "get", "duration")
nonovr011dis <- nonovr011dis [order(distype, patient_gender, durupr, duration ) ]
