#############################################################
# 080_medicine_repeat_prop_addnl_cal.R
# Code dependent on 080_medicine_repeat_prop.R
# Output created: 080_medcine_repeat_prop_addnl.rds
#############################################################

library(data.table)
library(stringi)
library(stringr)
library(sqldf)
library(scales)

all_met_rmsd <- readRDS("D:/Hospital_data/ProgresSQL/analysis/01adsl_met_rmsd.rds")

###########################
# Data related to medicines
###########################
meds0 <- unique( all_met_rmsd [medicine_name != " ", 
                               c("mr_no", "studyday", "Coded_med", "Type_med")] )

###########################################################
# Get the minimum day (minday) for any medicine and
# Get the minimum day (minmedday) for individual medicine
###########################################################
meds0 <- meds0 [order(mr_no, studyday)]  
meds0 <- meds0 [, minday := min(studyday), by = .(mr_no)]
meds0 <- meds0 [, minmedday := min(studyday), by = .(mr_no, Type_med, Coded_med)]

###########################################################
# Get group (each day of treatment) as a grouping variable
# Get individual sequential rows within each group
###########################################################
time <- unique(all_met_rmsd [, c("mr_no", "studyday")] ) 
time <- time [order(mr_no, studyday)]
time <- time [, grpday := 1:.N, by = .(mr_no)]
time <- time [, grpmaxday := max(grpday), by = .(mr_no)]

#######################################################
# Merge the grouping variables for further calculations
# Sort the data
#######################################################
meds0 <- merge (x = meds0, y = time, by = c("mr_no", "studyday") )
meds0 <- meds0[order(mr_no, studyday, grpday)]

###############################################################################
# Sort the data to get prescription number for each medicine
# If the prescription number is > 1 then that medicine is given more than once
#
# There are 2 sequence variables: one for day and one for medicine
###############################################################################
cum01 <- meds0 [, presc := 1:.N, by = .(mr_no, Type_med, Coded_med)]
cum01 <- cum01 [order(mr_no, studyday, minday, Type_med, Coded_med, presc )]  
cum01 <- cum01 [, grpall := 1:.N, by = .(mr_no)]

####################################################################
# If the prescription = 1 and studyday = minmedday then Start
# If prescription > 1 then Old (already given and not a medicine)
# If prescription  group number is > 1 then Start
####################################################################
cum01 <- cum01 [, newold := ifelse (studyday == minmedday, "1st dose", ""), ]
cum02 <- cum01 [, newold2 := ifelse(presc > 1 & grpday > 1 & studyday > minmedday & newold != "1st dose", "Repeat", newold), by =.(mr_no)]
cum02 <- cum02 [, cat := "Medicine", ]

#############################################################################
# Duplicate the medication and see which medications are given multiple times
# This gives a cumulative view of what has been prescribed till a certain
# Visit, how many medicines are 1st time given and how many are Repeated
#############################################################################

cum03 <- cum02 [, (list( cumday = (grpday: grpmaxday) ) ), 
                by = .(mr_no, presc, Type_med, Coded_med, 
                       studyday, grpday, grpmaxday, minmedday, newold2, cat) ]

cum03 <- cum03 [, cumday2 := paste("Till visit", cumday, sep = " "), ]

###########################
# Data related to diseases
###########################
meds0 <- unique( all_met_rmsd [, c("mr_no", "Code", "studyday", "description")] )
meds0 <- data.table(meds0 [, Code := ifelse (Code == " " | Code == "", "** Not yet coded", Code),])
meds0 <- data.table(meds0 [, description:= ifelse (description == "" | description ==" ", "** Not yet coded", description),])

###########################################################
# Get the minimum day (minday) for any medicine and
# Get the minimum day (minmedday) for individual medicine
###########################################################
meds0 <- meds0 [order(mr_no, studyday, Code, description )]  
meds0 <- meds0 [, minday := min(studyday), by = .(mr_no)]
meds0 <- meds0 [, minmedday := min(studyday), by = .(mr_no, Code, description)]

#######################################################
# Merge the grouping variables for further calculations
# Sort the data
#######################################################
meds0 <- merge (x = meds0, y = time, by = c("mr_no", "studyday") )
meds0 <- meds0[order(mr_no, studyday, grpday)]

###############################################################################
# Sort the data to get prescription number for each medicine
# If the prescription number is > 1 then that medicine is given more than once
#
# There are 2 sequence variables: one for day and one for medicine
###############################################################################
cum01 <- meds0 [, presc := 1:.N, by = .(mr_no, Code, description)]
cum01 <- cum01 [order(mr_no, studyday, minday, presc )]  
cum01 <- cum01 [, grpall := 1:.N, by = .(mr_no)]

####################################################################
# If the prescription = 1 and studyday = minmedday then Start
# If prescription > 1 then Old (already given and not a medicine)
# If prescription  group number is > 1 then Start
####################################################################
cum01 <- cum01 [, newold := ifelse (studyday == minmedday, "1st time disease", ""), ]
cum02dis <- cum01 [, newold2 := ifelse(presc > 1 & grpday > 1 & studyday > minmedday & newold != "1st time dose", "Repeat", newold), by =.(mr_no)]
cum02dis <- cum02dis [, cat := "Disease", ]

setnames (cum02dis, "Code", "Type_med")
setnames (cum02dis, "description", "Coded_med")

#############################################################################
# Duplicate the medication and see which medications are given multiple times
# This gives a cumulative view of what has been prescribed till a certain
# Visit, how many medicines are 1st time given and how many are Repeated
#############################################################################
cum03dis <- cum02dis [, (list( cumday = (grpday: grpmaxday) ) ), 
                      by = .(mr_no, Type_med, presc, Coded_med, 
                             studyday, grpday, grpmaxday, minmedday, newold2, cat) ]

cum03dis <- cum03dis [, cumday2 := paste("Till visit", cumday, sep = " "), ]

########################################################
# Combine all disease and medicine information
# for individual visits as well as cumulative visit data
########################################################
cum02all <- rbind (cum02, cum02dis, fill = TRUE)
cum02all <- cum02all[, -c("newold"),]
cum03all <- rbind (cum03, cum03dis, fill = TRUE)

all_met_rmsd0 <- data.table(all_met_rmsd [, Code := ifelse (Code == " " | Code == "", "** Not yet coded", Code),])
all_met_rmsd0 <- data.table(all_met_rmsd0 [, description:= ifelse (description == "" | description ==" ", "** Not yet coded", description),])

keep <- c("mr_no", "studyday", "patient_gender", "baseage", "age", "Code", "description", 
          "Coded_med", "Type_med", "combine", "Metabolic", "RMSD", "vis", "season", "newdt0", "distype")

all_met_rmsd_unq <- unique( all_met_rmsd0 [, ..keep, ])


all_met_rmsd_unq02 <- merge(x = all_met_rmsd_unq, 
                            y = cum02 [, -c("newold", "cat"),],
                            by = c("mr_no", "studyday", "Coded_med", "Type_med"),
                            all.x = TRUE)

setnames (cum02dis, "Type_med", "Code")
setnames (cum02dis, "Coded_med", "description")

setnames (cum02dis, "presc", "prescdis")
setnames (cum02dis, "newold2", "newold2dis")
setnames (cum02dis, "grpday", "grpdaydis")

all_met_rmsd_unq03 <- merge(x = all_met_rmsd_unq02, 
                            y = cum02dis [, -c("newold", "cat", "grpall", "minday", "minmedday", "grpmaxday"),],
                            by = c("mr_no", "studyday", "Code", "description"),
                            all.x = TRUE)

all_met_rmsd_unq04 <- all_met_rmsd_unq03 [grpday > 0, `:=`(cumday = grpmaxday, 
                                                           cumday3 = max(studyday),
                                                           cumday2 = paste("Till visit", grpmaxday, sep = " ") ), 
                                          by = .(mr_no)]

#############################################################################
# Duplicate the medication and see which medications are given multiple times
# This gives a cumulative view of what has been prescribed till a certain
# Visit, how many medicines are 1st time given and how many are Repeated
#############################################################################

all_met_rmsd_unq05 <- all_met_rmsd_unq03 [grpday > 0, (list( cumday = (grpday: grpmaxday) ) ), 
                                          by = .(mr_no, presc, prescdis, Type_med, Coded_med, 
                                                 Code, description, baseage, age, combine, Metabolic, RMSD, 
                                                 studyday, grpday, grpmaxday, minmedday, newold2, newold2dis) ]

all_met_rmsd_unq05 <- all_met_rmsd_unq05 [, cumday2 := paste("Till visit", cumday, sep = " "), ]
all_met_rmsd_unq05 <- all_met_rmsd_unq05 [, cumday3 := max(studyday), by = .(mr_no, cumday2)]

#########################################
# Count number of 1st and repeat diseases
# for individual patient
#
# Count number of 1st and repeat doses
# for individual patient
#
# Transpose the 
#########################################

a0dis <- all_met_rmsd_unq04 [, .(cntdis = uniqueN( paste(Code, description, sep=" "))), 
                             by = .(mr_no, grpday, cumday, cumday2, cumday3, newold2dis)]
a0dis_t <- dcast(data = a0dis,
                 mr_no + grpday + cumday + cumday2 + cumday3 ~ newold2dis,
                 value.var = c("cntdis"),
                 fill = 0)
setnames(a0dis_t, "Repeat", "Repeatdis")

a0dose <- all_met_rmsd_unq04 [, .(cntdose = uniqueN( paste(Type_med, Coded_med, sep=" "))), 
                              by = .(mr_no, grpday, cumday, cumday2, cumday3, newold2)]
a0dose_t <- dcast(data = a0dose,
                  mr_no + grpday + cumday + cumday2 + cumday3 ~ newold2,
                  value.var = c("cntdose"),
                  fill = 0)
setnames(a0dose_t, "Repeat", "Repeatdose")

##################################################################
# Count total number of diseases and doses for individual patients
##################################################################
a0distot <- all_met_rmsd_unq05 [, .(totdis = uniqueN( paste(Code, description, sep=" "))), 
                                by = .(mr_no, cumday, cumday2, cumday3)]

a0dosetot <- all_met_rmsd_unq05 [, .(totdose = uniqueN( paste(Type_med, Coded_med, sep=" "))), 
                                 by = .(mr_no, cumday, cumday2, cumday3)]

a01small <- Reduce(function(...) merge(..., all.y = TRUE, by = c("mr_no", "grpday", "cumday", "cumday2", "cumday3") ),
                   list(a0dis_t, a0dose_t))

a01cap <- Reduce(function(...) merge(..., all.y = TRUE, by = c("mr_no", "cumday", "cumday2", "cumday3") ),
                 list(a0distot, a0dosetot))

a01all <- merge(x = a01small [, -c("cumday", "cumday2", "cumday3"),], 
                y = a01cap, 
                by.x = c("mr_no", "grpday"),
                by.y = c("mr_no", "cumday"))

a01all <- a01all [, `:=` (perc1dis = percent(`1st time disease` / totdis),
                          percrepdis = percent(`Repeatdis` / totdis), 
                          perc1dose = percent(`1st dose` / totdose),
                          percrepdose = percent(`Repeatdose` / totdose)) , ]

a02all <- merge(x = a01all, 
                y = all_met_rmsd_unq03 [, -c("cumday2", "cumday3"), ], 
                by = c("mr_no", "grpday"))

dis <- unique( a02all [, c("mr_no", "Code", "description"),])
dis <- dis [, disnum_cal := 1:.N, by =.(mr_no)]

dis00 <- unique( a02all [, c("mr_no", "Code", "description", "grpday"),])
dis00 <- dis00 [, distimes := 1:.N, by =.(mr_no, Code, description)]

med <- unique( a02all [, c("mr_no", "Type_med", "Coded_med"),])
med <- med [, dosnum_cal := 1:.N, by =.(mr_no)]

med00 <- unique( a02all [, c("mr_no", "Type_med", "Coded_med", "grpday"),])
med00 <- med00 [, dostimes := 1:.N, by =.(mr_no, Type_med, Coded_med)]

dismed <- unique( a02all [, c("mr_no", "Code", "description", "Type_med", "Coded_med", "grpday"),])
dismed <- dismed [, bothnum_cal := 1:.N, by =.(mr_no, Code, description, Type_med, Coded_med)]

a07all <- merge( x = a02all,
                 y = dis,
                 by = c("mr_no", "Code", "description"))

a07all <- merge( x = a07all,
                 y = med,
                 by = c("mr_no", "Type_med", "Coded_med"))

a07all <- merge( x = a07all,
                 y = dismed,
                 by = c("mr_no", "Code", "description", "Type_med", "Coded_med", "grpday"))

a07all <- merge( x = a07all,
                 y = dis00,
                 by = c("mr_no", "Code", "description", "grpday"))

a07all <- merge( x = a07all,
                 y = med00,
                 by = c("mr_no", "Type_med", "Coded_med", "grpday"))

a08all <- data.table(unique ( a07all) )

saveRDS (a08all, "D:/Hospital_data/ProgresSQL/analysis/080_medcine_repeat_prop_addnl.rds")
fwrite(a08all, "D:/Hospital_data/ProgresSQL/analysis/080_medcine_repeat_prop_addnl.csv")
