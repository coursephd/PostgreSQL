library(data.table)
library(tidyverse)
library(cumstats)

#all_met_rmsd02 <- fread("C://Users//mahajvi1//Downloads//01adsl_met_rmsd.csv")
all_met_rmsd02 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/01adsl_met_rmsd.rds")
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

fwrite(all_met_rmsd06, "D:/Hospital_data/ProgresSQL/analysis/105_trt_dis_unq_mult01.csv")

############################################################################################
# End of program for tableau
############################################################################################

#all_met_rmsd02 <- fread("C://Users//mahajvi1//Downloads//01adsl_met_rmsd.csv")
all_met_rmsd02 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/01adsl_met_rmsd.rds")
all_met_rmsd02 <- all_met_rmsd02 [, Code := ifelse (Code == " " | Code == "", "** Not yet coded", Code),]
all_met_rmsd02 <- all_met_rmsd02 [, description:= ifelse (description == "" | description ==" ", "** Not yet coded", description),]
all_met_rmsd02 <- all_met_rmsd02 [, Code02 := paste(Code, ":", description, sep =""), ]
all_met_rmsd03 <- unique(all_met_rmsd02 [, c("mr_no", "studyday", "vis", "all_vis", "cdur", "Code", "description", "Code02")] )
all_met_rmsd03 <- all_met_rmsd03 [order(mr_no, Code02, Code, description, studyday, vis)]

dis_trt_comb_all <- unique ( all_met_rmsd02 [, c("Code02", "Code", "description", "Coded_med", "Type_med"),])
dis_trt_comb_all <- dis_trt_comb_all [, all:=1,]

patonedis <- all_met_rmsd03[, .(n = uniqueN(Code02)), by = .(mr_no)]
patonedis <- patonedis [ n == 1]


all_met_rmsd04 <- merge (x = all_met_rmsd02,
                         y = patonedis,
                         by = c("mr_no"),
                         all.y = TRUE)

dis_trt_comb <- unique ( all_met_rmsd04 [, c("Code02", "Code", "description", "Coded_med", "Type_med"),])
dis_trt_comb <- dis_trt_comb [, unq :=1,]

dis_trt <- all_met_rmsd04[, .(unqmed = uniqueN( paste(Coded_med, Type_med) )), by = .(Code02)]
dis_pat <- all_met_rmsd04[, .(unqpat = uniqueN(mr_no)), by = .(Code02)]

dis_pat_trt <- merge (x = dis_pat,
                      y = dis_trt,
                      by = c("Code02"))


dis_trt_all <- all_met_rmsd02[, .(allmed = uniqueN(paste(Coded_med, Type_med) )), by = .(Code02)]
dis_pat_all <- all_met_rmsd02[, .(allpat = uniqueN(mr_no)), by = .(Code02)]

dis_pat_trt_all <- merge (x = dis_pat_all,
                          y = dis_trt_all,
                          by = c("Code02"))

a0final <- merge (x = dis_pat_trt,
                  y = dis_pat_trt_all,
                  by = c("Code02"))


dis_med_trt <- all_met_rmsd04[, .(unqdis = uniqueN(Code02)), by = .(Type_med, Coded_med)]
dis_med_pat <- all_met_rmsd04[, .(unqpat = uniqueN(mr_no)), by = .(Type_med, Coded_med)]

dis_med_pat_trt <- merge (x = dis_med_pat,
                          y = dis_med_trt,
                          by = c("Coded_med", "Type_med"))


unqall <- merge (x = dis_trt_comb_all,
                 y = dis_trt_comb,
                 by = c("Code02", "Code", "description", "Coded_med", "Type_med"),
                 all.x = TRUE,
                 all.y = TRUE)