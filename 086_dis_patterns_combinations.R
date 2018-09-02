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

dis <- unique(all_met_rmsd[!Code %in% c("", " ") & refcode == "A2.0", c("mr_no", "studyday", "Code", "description", "refcode", "refdesc")])
dis <- dis [ order(mr_no, studyday, Code, refcode, refdesc)]
dis <- dis [, `:=` (ndis = uniqueN(Code), 
                    nrow = seq_len(.N),
                    nrowend = seq_len(.N) + 4,
                    totrow = .N), by = .(mr_no, refcode, refdesc)]

dis02 <- dis [, .(newgrp = seq(nrow, nrowend, by =1)), by = .(mr_no, Code, description, refcode, refdesc, nrow, nrowend, ndis, totrow)]

dis03 <- dis02 [, .(combdis = paste(Code, collapse = ",", sep = " " )), 
              by = .(mr_no, refcode, refdesc, newgrp, ndis, totrow)]

########################################################
# Get the unique combinations per patient
# Get the numerator -- equal n-grams
# Find the jacard distance for each patient combination
#
# Denominator: distinct n-grams for each 
# patient combination
# num / den for each patient combination, gives jacard
# distance
########################################################

unq01comb <- unique( dis03 [, c("mr_no", "refcode", "refdesc", "combdis"), ])
unq01comb <- unq01comb [, x := 1, ]

# create a copy
unq02comb <- copy(unq01comb)
setnames(unq02comb, "mr_no", "mr_no2")
setnames(unq02comb, "combdis", "combdis2")

# Merge the datasets on x to get all the combinations

unq03comb <- merge(x = unq01comb, # [ mr_no == "MR000002"], 
                   y = unq02comb [, -c("refcode", "refdesc"), ], 
                   by = c("x"), 
                   allow.cartesian = TRUE)

unq04comb <- unq03comb [ combdis == combdis2]
unq04comb <- unq04comb [, num := 1]

num01 <- unq04comb [, .(num01 = sum(num)), by = .(mr_no, mr_no2, refcode, refdesc)]
rm(unq04comb)

###################################
# Get the denominator calculations
###################################

temp01 <- unique( unq03comb [, c("mr_no", "mr_no2", "combdis", "refcode", "refdesc"), ])
temp02 <- unique( unq03comb [, c("mr_no", "mr_no2", "combdis2", "refcode", "refdesc"), ])
setnames(temp02, "combdis2", "combdis")

temp03 <- unique( rbind (temp01, temp02))

rm(temp01); rm(temp02)
rm(unq03comb)

den01 <- temp03 [, .(den01 = .N), by = .(mr_no, mr_no2, refcode, refdesc)]

rm(temp03)

jacard01 <- merge(x = num01,
                  y = den01,
                  by = c("mr_no", "mr_no2", "refdesc", "refcode"),
                  all.x = TRUE)
jacard01 <- jacard01 [, dist := (num01 / den01) * 100, ]

jacard01trn <- dcast(data = jacard01, 
                     refcode + refdesc + mr_no ~ mr_no2, 
                     value.var = c ("dist"), 
                     fill = 0)

#dis03cnt <- dis03 [, .(npat = uniqueN(mr_no)), by = .(combdis)]

unqdis <- unique( dis [, c("mr_no", "ndis", "totrow", "refcode", "refdesc"), ])
alldis <- dis02 [, .(alldis = paste(unique(Code), collapse = ",", sep = " " ),
                     alldesc = paste(unique(description), collapse = ",", sep = " " )), 
                by = .(mr_no, refcode, refdesc)]

# Combine all variables into 1 dataset ADSL
alldis <- Reduce(function(...) merge(..., all = TRUE, by = c("mr_no", "refcode", "refdesc")),
               list(unqdis, alldis, jacard01trn) )

fwrite(alldis, 
       "D:/Hospital_data/ProgresSQL/analysis/086_dis_patterns_combinations_A2.0.csv")
