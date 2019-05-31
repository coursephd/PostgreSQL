library(data.table)
library(tidyverse)
library(sqldf)
library(cumstats)

#all_met_rmsd02 <- fread("C://Users//mahajvi1//Downloads//01adsl_met_rmsd.csv")
all_met_rmsd02 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/01adsl_met_rmsd.rds")
all_met_rmsd02 <- all_met_rmsd02 [, Code := ifelse (Code == " " | Code == "", "** Not yet coded", Code),]
all_met_rmsd02 <- all_met_rmsd02 [, description:= ifelse (description == "" | description ==" ", "** Not yet coded", description),]
all_met_rmsd02 <- all_met_rmsd02 [, Code02 := paste(Code, ":", description, sep =""), ]
all_met_rmsd03 <- unique(all_met_rmsd02 [, c("mr_no", "studyday", "vis", "all_vis", "cdur", "Code", "description", "Code02")] )
all_met_rmsd03 <- all_met_rmsd03 [order(mr_no, Code02, Code, description, studyday, vis)]

all_met_rmsd03 <- all_met_rmsd03 [, comdisn := .N, by = .(mr_no, Code02)]
all_met_rmsd03 <- all_met_rmsd03 [, distime := 1:.N, by = .(mr_no, Code02)]
all_met_rmsd03 <- all_met_rmsd03 [, diff := studyday - shift(studyday), by = .(mr_no, Code, description)]
all_met_rmsd03 <- all_met_rmsd03 [, diff := ifelse (distime ==1, 1, diff), ]

# For merge
pat <- unique( all_met_rmsd02 [, c("mr_no", "Metabolic", "RMSD", "patient_gender", "all_vis", "cdur")])

# If all_vis = 1 then patient level only 1 visit

all_met_rmsd04 <- all_met_rmsd03 %>%
  mutate(
    patdis = 
      case_when( all_vis == 1 ~ 100,
                 all_vis > 1 & all_vis <= 5 ~ 200,
                 all_vis > 5 & cdur <= 30 ~ 300,
                 all_vis > 5 & all_vis <= 10 & cdur > 30 & cdur <= 180 ~ 400,
                 all_vis > 10 & cdur > 30 & cdur < 180 ~ 500,
                 cdur > 180 ~ 600
                 
      )
  )

all_met_rmsd04 <- as.data.table(all_met_rmsd04)

chk <- all_met_rmsd04 [, .(cnt = uniqueN(mr_no)), by = .(patdis)]


###############################################################################
# Create a variable to identify episodes of a disease
# if a disease is re-appearing after 30 days then consider that as
# a new episode, this duration should be specific to each disease in reality
# Use the variable to eps01 for cumulative addition and get number of 
# episodes for each disease for each patient
#
# If a disease is non-episodic then use 9999 as the duration
# This calculation should help in understanding the disease specific
# pseudo outcome and amount of data collected
# 
# Use this variable along with the overall classification of a patient to 
# create a medical story
###############################################################################

all_met_rmsd04 <- all_met_rmsd04 [, eps01 := ifelse(diff > 30, 1, 0), ]
all_met_rmsd04 <- all_met_rmsd04 [, eps011 := cumsum(eps01) + 1, by = .(mr_no, Code02, Code, description)]

# Create episodic visit numbering
all_met_rmsd04 <- all_met_rmsd04 [, eps_vis := sequence(.N), by = .(mr_no, Code02, Code, description, eps011)]

# Calculate the difference between 2 episodes
all_met_rmsd04 <- all_met_rmsd04 [, diffeps := studyday - shift(studyday), by = .(mr_no, Code02, Code, description)]
all_met_rmsd04 <- all_met_rmsd04 [, diffeps := ifelse (eps011 ==1 & eps_vis ==1, 0, diffeps), ]
all_met_rmsd04_11 <- unique( all_met_rmsd04 [ eps_vis ==1, c("mr_no", "Code02", "Code", "description", "eps011", "diffeps"), ] )

all_met_rmsd04_11 <- all_met_rmsd04_11 [, releps01 := ifelse(diffeps > 180, 1, 0), ]
all_met_rmsd04_11 <- all_met_rmsd04_11 [, releps011 := cumsum(releps01) + 1, by = .(mr_no, Code02, Code, description)]

all_met_rmsd04 <- merge (x = all_met_rmsd04 [, -c("diffeps"),],
                         y = all_met_rmsd04_11,
                         by = c("mr_no", "Code02", "Code", "description", "eps011"),
                         all.x = TRUE)

# Calculate duration of invidual disease episode
all_met_rmsd04 <- all_met_rmsd04 [, `:=` (epsdur = max(studyday) - min (studyday) + 1), by = .(mr_no, Code02, Code, description, eps011, diffeps)]

epsd01 <- all_met_rmsd04 [, .(epsdur = max(studyday) - min (studyday) + 1), by = .(mr_no, Code02, Code, description, eps011, diffeps, releps011)]

# calculate the median after removing events with only 1 day
all_met_rmsd05 <- epsd01 [epsdur > 1]
all_met_rmsd05 <- all_met_rmsd05 [, `:=` (epsmedian = median(epsdur, na.rm= FALSE) ), by = .(Code02, eps011)]
all_met_rmsd05 <- all_met_rmsd05 [, pattype := ifelse(epsdur > epsmedian, "01Responder", "02Non-responder"), ]


all_met_rmsd06 <- merge (x = all_met_rmsd04,
                         y = all_met_rmsd05,
                         by = c("mr_no", "Code02", "Code", "description", "eps011", "diffeps", "releps011", "epsdur"),
                         all.x = TRUE)

all_met_rmsd06 <- merge (x = all_met_rmsd06,
                         y = pat,
                         by = c("mr_no", "all_vis", "cdur"),
                         all.x = TRUE)

all_met_rmsd06$pattype[is.na(all_met_rmsd06$pattype)] <- "00Data-for-1day"
all_met_rmsd06$epsmedian[is.na(all_met_rmsd06$epsmedian)] <- 9999

###########################################################
# Get group (each day of treatment) as a grouping variable
# Get individual sequential rows within each group
###########################################################
time <- unique(all_met_rmsd06 [, c("mr_no", "studyday")] ) 
time <- time [order(mr_no, studyday)]
time <- time [, grpday := 1:.N, by = .(mr_no)]
time <- time [, grpmaxday := max(grpday), by = .(mr_no)]

###########################
# Data related to diseases
###########################
meds0 <- unique( all_met_rmsd06 [, c("mr_no", "Code", "studyday", "description")] )
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
cum03dis <- cum03dis [, study := max(studyday), by = .(mr_no, cumday2)]


a0dis <- cum03dis [, .(cntdis = uniqueN( paste(Type_med, Coded_med, sep=" "))), 
                   by = .(mr_no, study, cumday2)]

all_met_rmsd07 <- all_met_rmsd06[, `:=`(mediandis = cummedian(diff),
                                        q3dis = cumquant(diff, p =0.75)), by = .(mr_no, Code02)]

all_met_rmsd07 <- all_met_rmsd07[ order(mr_no, studyday) ]

all_met_rmsd07 <- all_met_rmsd07[, `:=`(medianpat = cummedian(diff),
                                        q3pat = cumquant(diff, p =0.75)), by = .(mr_no)]


fwrite(all_met_rmsd06, "D:/Hospital_data/ProgresSQL/analysis/102_episodicdis01.csv")

t01 <- epsd01 [, .(n = uniqueN(mr_no),
                   mean = mean(epsdur, na.rm = FALSE),
                   sd = sd(epsdur, na.rm = FALSE),
                   median = median (epsdur, na.rm = FALSE),
                   min = min (epsdur, na.rm = FALSE),
                   max = max (epsdur, na.rm = FALSE)), by = .(Code02, eps011)]

t01_exl01 <- epsd01 [epsdur > 1, .(n = uniqueN(mr_no),
                                   mean = mean(epsdur, na.rm = FALSE),
                                   sd = sd(epsdur, na.rm = FALSE),
                                   median = median (epsdur, na.rm = FALSE),
                                   min = min (epsdur, na.rm = FALSE),
                                   max = max (epsdur, na.rm = FALSE)), by = .(Code02, eps011)]

t01_diffeps <- epsd01 [diffeps > 0, .(n = uniqueN(mr_no),
                                      mean = mean(diffeps, na.rm = FALSE),
                                      sd = sd(diffeps, na.rm = FALSE),
                                      median = median (diffeps, na.rm = FALSE),
                                      min = min (diffeps, na.rm = FALSE),
                                      max = max (diffeps, na.rm = FALSE)), by = .(Code02, eps011)]


t01resp <- all_met_rmsd05 [, .(n = uniqueN(mr_no),
                               mean = mean(epsdur, na.rm = FALSE),
                               sd = sd(epsdur, na.rm = FALSE),
                               median = median (epsdur, na.rm = FALSE),
                               min = min (epsdur, na.rm = FALSE),
                               max = max (epsdur, na.rm = FALSE)), by = .(Code02, pattype, eps011)]

t01resp_rel <- all_met_rmsd05 [, .(n = uniqueN(mr_no),
                                   mean = mean(epsdur, na.rm = FALSE),
                                   sd = sd(epsdur, na.rm = FALSE),
                                   median = median (epsdur, na.rm = FALSE),
                                   min = min (epsdur, na.rm = FALSE),
                                   max = max (epsdur, na.rm = FALSE)), by = .(Code02, pattype, releps011)]


pat00 <- all_met_rmsd06 [mr_no == "MR000059"]
patchk <- all_met_rmsd04 [, nvis011 := max( eps011 ), by = .(mr_no, Code02)]
