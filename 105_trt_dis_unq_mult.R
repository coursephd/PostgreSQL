library(data.table)
library(tidyverse)
library(cumstats)

all_met_rmsd02 <- fread("C://Users//mahajvi1//Downloads//01adsl_met_rmsd.csv")
#all_met_rmsd02 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/01adsl_met_rmsd.rds")
all_met_rmsd02 <- all_met_rmsd02 [, Code := ifelse (Code == " " | Code == "", "** Not yet coded", Code),]
all_met_rmsd02 <- all_met_rmsd02 [, description:= ifelse (description == "" | description ==" ", "** Not yet coded", description),]
all_met_rmsd02 <- all_met_rmsd02 [, Code02 := paste(Code, ":", description, sep =""), ]
all_met_rmsd03 <- unique(all_met_rmsd02 [, c("mr_no", "studyday", "vis", "all_vis", "cdur", "Code", "description", "Code02")] )
all_met_rmsd03 <- all_met_rmsd03 [order(mr_no, Code02, Code, description, studyday, vis)]

dis_trt_comb_all <- unique ( all_met_rmsd02 [, c("Code02", "Code", "description", "medicine_name"),])
dis_trt_comb_all <- dis_trt_comb_all [, all:=1,]

patonedis <- all_met_rmsd03[, .(n = uniqueN(Code02)), by = .(mr_no)]
patonedis <- patonedis [ n == 1]


all_met_rmsd04 <- merge (x = all_met_rmsd02,
                         y = patonedis,
                         by = c("mr_no"),
                         all.y = TRUE)

dis_trt_comb <- unique ( all_met_rmsd04 [, c("Code02", "Code", "description", "medicine_name"),])
dis_trt_comb <- dis_trt_comb [, unq :=1,]

dis_trt <- all_met_rmsd04[, .(unqmed = uniqueN(medicine_name)), by = .(Code02)]
dis_pat <- all_met_rmsd04[, .(unqpat = uniqueN(mr_no)), by = .(Code02)]

dis_pat_trt <- merge (x = dis_pat,
                      y = dis_trt,
                      by = c("Code02"))


dis_trt_all <- all_met_rmsd02[, .(allmed = uniqueN(medicine_name)), by = .(Code02)]
dis_pat_all <- all_met_rmsd02[, .(allpat = uniqueN(mr_no)), by = .(Code02)]

dis_pat_trt_all <- merge (x = dis_pat_all,
                      y = dis_trt_all,
                      by = c("Code02"))

a0final <- merge (x = dis_pat_trt,
                  y = dis_pat_trt_all,
                  by = c("Code02"))


dis_med_trt <- all_met_rmsd04[, .(unqdis = uniqueN(Code02)), by = .(medicine_name)]
dis_med_pat <- all_met_rmsd04[, .(unqpat = uniqueN(mr_no)), by = .(medicine_name)]

dis_med_pat_trt <- merge (x = dis_med_pat,
                      y = dis_med_trt,
                      by = c("medicine_name"))


unqall <- merge (x = dis_trt_comb_all,
                 y = dis_trt_comb,
                 by = c("Code02", "Code", "description", "medicine_name"),
                 all.x = TRUE,
                 all.y = TRUE)
