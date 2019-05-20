library(data.table)
library(tidyverse)
library(sqldf)


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

############################################################
# If all_vis = 1 then patient level only 1 visit
# Category and explanation
#
# 100 = Drop out with no data
# 200 = Drop out with limited data
#
# 300 = Drop out with approx 1 month data
# 400 = Limited visits with 1 to 6 month data?
#
# 500 = Large visits with  1 to 6 month data?
# 600 = More than 6 months data
#
############################################################
all_met_rmsd04 <- all_met_rmsd03 %>%
  mutate(
    patdis =
      case_when( all_vis == 1 ~ 100,
                 all_vis > 1 & all_vis <= 5 ~ 200,
                 all_vis > 5 & cdur <= 30 ~ 300,
                 all_vis > 5 & all_vis <= 10 & cdur > 30 & cdur <= 180 ~ 400,
                 all_vis > 10 & cdur > 30 & cdur <= 180 ~ 500,
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
all_met_rmsd04 <- all_met_rmsd04 [, eps011 := cumsum(eps01) + 1, by = .(mr_no, Code02)]

# Create episodic visit numbering
all_met_rmsd04 <- all_met_rmsd04 [, eps_vis := sequence(.N), by = .(mr_no, Code02, eps011)]

# Calculate duration of invidual disease episode
all_met_rmsd04 <- all_met_rmsd04 [, `:=` (epsdur = max(studyday) - min (studyday) + 1), by = .(mr_no, Code02, eps011)]

pat <- all_met_rmsd04 [mr_no == "MR000059"]
patchk <- all_met_rmsd04 [, nvis011 := max( eps011 ), by = .(mr_no, Code02)]