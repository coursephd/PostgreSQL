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

# https://stackoverflow.com/questions/43706729/expand-dates-in-data-table

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


# Get the unique combinations per patient
unq01comb <- unique( dis03 [, c("mr_no", "combdis"), ])
unq01comb <- unq01comb [, x := 1, ]

# create a copy
unq02comb <- copy(unq01comb)
setnames(unq02comb, "mr_no", "mr_no2")
setnames(unq02comb, "combdis", "combdis2")

# Merge the datasets on x to get all the combinations

unq03comb <- merge(x = unq01comb [ mr_no == "MR000002"], 
                   y = unq02comb, 
                   by = c("x"), 
                   allow.cartesian = TRUE)
unq03comb <- unq03comb [, num := ifelse( combdis == combdis2, 1, 0), ]


dis03cnt <- dis03 [, .(npat = uniqueN(mr_no)), by = .(combdis)]


