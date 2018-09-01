####################################################################
# This is used for 086_dis_count_edges_3rd_byPeriod Tableau display
####################################################################

library(data.table)
library(stringi)
library(stringr)
library(sqldf)
library(tidyr)
library(rjson)
library(jsonlite)
library(dplyr)
library(zoo)
library(tidyr)

all_met_rmsd <- readRDS ("D:/Hospital_data/ProgresSQL/analysis/all_met_rmsd02.rds")

all_met_rmsd <- readRDS ("D:/Hospital_data/ProgresSQL/analysis/01adsl_met_rmsd.rds")

dis <- unique(all_met_rmsd[!Code %in% c("", " "), c("mr_no", "studyday", "Code")])
dis <- dis [ order(mr_no, studyday, Code)]
dis <- dis [, `:=` (ndis = uniqueN(Code), 
                    nrow = seq_len(.N),
                    nrowend = seq_len(.N) + 4,
                    totrow = .N), by = .(mr_no)]

dis02 <- dis [, .(newgrp = seq(nrow, nrowend, by =1)), by = .(mr_no, Code, nrow, nrowend, ndis, totrow)]

dis03 <- dis02 [, .(combdis = paste(Code, collapse = ",", sep = " " )), 
              by = .(mr_no, newgrp, ndis, totrow)]

dis03cnt <- dis03 [, .(npat = uniqueN(mr_no)), by = .(combdis)]


