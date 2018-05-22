library(data.table)
library(ggplot2)
library(dplyr)
library(anytime)
library(knitr)
library(DT)

all_met_rmsd <- readRDS("D:/Hospital_data/ProgresSQL/analysis/01adsl_met_rmsd.rds")

##########################################
# vismon = Overall duration in months
# imon = Individual visit date in months
##########################################
all_met_rmsd <- all_met_rmsd [, `:=` (baseage = min(age)), by =.(mr_no)]
overall <- all_met_rmsd [, `:=` (vismon = as.numeric(round( cdur/30.4375, digits = 0)) + 1,
                                      imon = round( studyday/30.4375, digits = 0) + 1,
                                      cdur = as.numeric(cdur),
                                      studyday =as.numeric(studyday), 
                                      Code = paste(distype, ":", Code, description, sep= " ")), ]

overall <- overall[, `:=`(istdt = min(newdt0), 
                        iendt = max(newdt0), 
                        idur = as.numeric( max(newdt0) - min(newdt0) + 1 ) ), 
                   by = .(mr_no, Code)]

overall <- overall[, `:=`(idurmonth = round(idur/30.4375 , digits = 0 ) + 1),]

dur    <- c("1st Month", "2nd month", "3rd month", "4th to 6th month", 
            "6th Month to 1 year", "> 1 year and <= 2 years", "> 2 years and <= 3 years", 
            "> 3 years and <= 4 years", "> 4 years and <= 5 years", "> 5 years")

durlwr <- c(1,           2,            3,            4,             7,           
            13,       25,           37,            49,           61)

durupr <- c(1,         2,          3,          6,           12, 
            24,       36,         48,          60,       999)

ref <- data.table ( cbind.data.frame (durlwr, durupr, dur ) )
setkey(ref, durlwr, durupr) ## Set the key for patient IDs ("y" table)

ref02 <- ref[ , list(durlwr = durlwr, durupr = durupr, dur = dur,
                         mon = seq(durlwr, durupr, by = 1) ), by = 1:nrow(ref)]

################################################################
# First merge to get the visit category for the overall duration
################################################################
overall <- merge (x = overall,
                  y = ref02 [, c("durlwr", "durupr", "mon", "dur")],
                  by.x = c("vismon"),
                  by.y = c("mon"),
                  all.x = TRUE)

############################################################
# Change the names to reflect the duration related variables
############################################################
setnames(overall, "dur", "visdur")
setnames(overall, "durlwr", "visdurlwr")
setnames(overall, "durupr", "visdurupr")

###################################################
# Merge the visit category with individual studyday
###################################################
overall <- merge (x = overall,
                  y = ref02 [, c("durlwr", "durupr", "mon", "dur")],
                  by.x = c("idurmonth"),
                  by.y = c("mon"),
                  all.x = TRUE)

############################################################
# Change the names to reflect the duration related variables
############################################################
setnames(overall, "dur", "imondur")
setnames(overall, "durlwr", "imondurlwr")
setnames(overall, "durupr", "imondurupr")


###############################################################
# Calculate Overall duration analysis with non-overlapping time
# period approach
###############################################################

overall02 <- unique(overall [, c("mr_no", "Metabolic", "cdur", "RMSD", "combine", "baseage", 
                                 "all_vis", "patient_gender", 
                                 "vismon", "visdur", "visdurlwr", "visdurupr"), ])

summ_stat <- function (datain, xsub, by, by1, by99= "visdurupr", dataout ="D8")
{
  stats_data <- datain [, .(n=uniqueN(mr_no), 
                            mean = round( mean(get(xsub), na.rm = TRUE), digits =1),
                            median= round( median(get(xsub), na.rm = TRUE), digits =2),
                            SD = round( sd(get(xsub), na.rm = TRUE), digits =2),
                            min = round( min(get(xsub), na.rm = TRUE), digits =0),
                            max = round( max(get(xsub), na.rm = TRUE), digits =0)), 
                        by = .(get(by99), get(by), get(by1))]
  
  assign(dataout, stats_data, envir=.GlobalEnv)
}

# Dataset for complete duration across
# non overlapping time periods
summ_stat (datain =overall02, 
           xsub = 'cdur', 
           by =c("visdur"), 
           by1 =c("patient_gender"), 
           dataout ="nonovr01")

summ_stat (datain =overall02 [RMSD ==1], 
           xsub = 'cdur', 
           by =c("visdur"), 
           by1 =c("patient_gender"), 
           dataout ="nonovr01rmsd")

summ_stat (datain =overall02 [Metabolic ==1], 
           xsub = 'cdur', 
           by =c("visdur"), 
           by1 =c("patient_gender"), 
           dataout ="nonovr01met")

#################################################
# Summary statistics for non-overlapping diseases
#################################################
disease <- unique(overall [, c("mr_no", "Metabolic", "cdur", "RMSD", "combine", "baseage", 
                                 "all_vis", "patient_gender", "Code",
                                 "vismon", "visdur", "visdurlwr", "visdurupr",
                                 "idur", "idurmonth","imondur", "imondurlwr", "imondurupr"), ])

summ_stat (datain =disease [RMSD ==1 & patient_gender !=""], 
           xsub = 'idur', 
           by =c("imondur"), 
           by1 =c("patient_gender"), 
           by99 = c("imondurupr"),
           dataout ="nonovr01ovr_rmsd")

summ_stat (datain =disease [Metabolic ==1 & patient_gender !=""], 
           xsub = 'idur', 
           by =c("imondur"), 
           by1 =c("patient_gender"), 
           by99 = c("imondurupr"),
           dataout ="nonovr01ovr_met")

summ_stat (datain =disease [RMSD ==1 & patient_gender !=""], 
           xsub = 'idur', 
           by =c("imondur"), 
           by1 =c("patient_gender"), 
           by99 = c("Code"),
           dataout ="nonovr01dis_rmsd")

summ_stat (datain =disease [Metabolic ==1 & patient_gender !=""], 
           xsub = 'idur', 
           by =c("imondur"), 
           by1 =c("patient_gender"), 
           by99 = c("Code"),
           dataout ="nonovr01dis_met")

# Frequency counts for Total number of patients with treatment

freq01 <- overall[patient_gender != "", .(npatall = uniqueN(mr_no)), by = .(visdurupr, visdur, patient_gender)]
freq01met <- overall[patient_gender != "" & Metabolic ==1, .(npatmet = uniqueN(mr_no)), by = .(visdurupr, visdur, patient_gender)]
freq01rmsd <- overall[patient_gender != "" & RMSD ==1, .(npatRMSD = uniqueN(mr_no)), by = .(visdurupr, visdur, patient_gender)]

freq01all <- Reduce(function(...) merge(..., all.y = TRUE, by = c("visdurupr","visdur", "patient_gender") ),
                       list(freq01, freq01met, freq01rmsd))

# Frequency counts for Total number of patients with disease

dis01 <- overall[patient_gender != "", .(ndisall = uniqueN(mr_no)), by = .(imondurupr, imondur, patient_gender)]
dis01met <- overall[patient_gender != "" & Metabolic ==1, .(ndismet = uniqueN(mr_no)), by = .(imondurupr, imondur, patient_gender)]
dis01rmsd <- overall[patient_gender != "" & RMSD ==1, .(ndisRMSD = uniqueN(mr_no)), by = .(imondurupr, imondur, patient_gender)]

dis01all <- Reduce(function(...) merge(..., all.y = TRUE, by = c("imondurupr","imondur", "patient_gender") ),
                    list(dis01, dis01met, dis01rmsd))

dis_freq01all <- Reduce(function(...) merge(..., all = TRUE, 
                                            by.x = c("imondurupr","imondur", "patient_gender"),
                                            by.y = c("visdurupr","visdur", "patient_gender")),
                   list(dis01all, freq01all))


####################################################################
# Understand of the diseases are occurring in different time periods
# If present at least once then denote it by 1
####################################################################
diag950rpt <- unique(overall[, c("Code", "imondurupr", "imondur")] )
diag950rpt <- diag950rpt[, val :="Yes"]

diag950rpt_t <- dcast(data = diag950rpt, 
                      Code ~ imondur,
                      value.var = c("val"))
diag950rpt_t <- diag950rpt_t [, c (1, 7, 8, 9, 10, 11, 2, 3, 4, 5, 6),]

