library(data.table)
library(tidyverse)
library(cumstats)

#all_met_rmsd02 <- fread("C://Users//mahajvi1//Downloads//01adsl_met_rmsd.csv")
all_met_rmsd02 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/01adsl_vrikka.rds")
all_met_rmsd02 <- all_met_rmsd02 [, Code := ifelse (Code == " " | Code == "", "** Not yet coded", Code),]
all_met_rmsd02 <- all_met_rmsd02 [, description:= ifelse (description == "" | description ==" ", "** Not yet coded", description),]
all_met_rmsd02 <- all_met_rmsd02 [, Code02 := paste(Code, ":", description, sep =""), ]
all_met_rmsd02 <- all_met_rmsd02 [, Med02 := paste(Type_med, ":", Coded_med, sep =""), ]

all_met_rmsd03 <- unique(all_met_rmsd02 [, c("mr_no", "studyday", "vis", "all_vis", "cdur", 
                                             "Code", "description", "Code02",
                                             "Type_med", "Coded_med", "Med02")] )

patonedis <- all_met_rmsd03 [, .(n = uniqueN(Code02)), by = .(mr_no)]
patonedis <- patonedis [ n == 1]

# Patients with only single disease
all_met_rmsd04 <- all_met_rmsd03 [ mr_no %in% c(patonedis$mr_no)]
all_met_rmsd04 <- all_met_rmsd04 [, discat := "Single disease", ]

# Patients with only multiple diseases
all_met_rmsd05 <- all_met_rmsd03 [ ! mr_no %in% c(patonedis$mr_no)]
all_met_rmsd05 <- all_met_rmsd05 [, discat := "Multiple diseases", ]

# Combine the 2 datasets into one
all_met_rmsd06 <- rbind(all_met_rmsd04, all_met_rmsd05)

fwrite(all_met_rmsd06, "D:/Hospital_data/ProgresSQL/analysis/105_trt_dis_unq_mult01vrikka_roga.csv")

############################################################################################
# End of program for tableau
############################################################################################