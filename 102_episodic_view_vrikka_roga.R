library(data.table)
library(tidyverse)
library(sqldf)

#all_met_rmsd02 <- fread("C://Users//mahajvi1//Downloads//01adsl_met_rmsd.csv")
all_met_rmsd02 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/01adsl_vrikka.rds")
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
pat <- unique( all_met_rmsd02 [, c("mr_no", "patient_gender", "all_vis", "cdur")])

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

fwrite(all_met_rmsd06, "D:/Hospital_data/ProgresSQL/analysis/102_episodicdis01vrikka.csv")
