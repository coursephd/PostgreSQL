####################################################################
# 086_med_patterns_combinations
####################################################################

library(tidyverse)
library(tidytext)
library(stringr)
library(stringi)
library(data.table)
library(stringdist)


all_met_rmsd <- readRDS ("D:/Hospital_data/ProgresSQL/analysis/all_met_rmsd02.rds")
a2 <- all_met_rmsd [ refcode == "A2.0" & Coded_med != " "]
a2 <- a2 [, description := paste(Type_med, Coded_med),]
a2 <- a2 [ order(description)]

a2 <- a2 [, Code := paste("M", str_pad(.N, width =4, pad="0"), sep =""), by = .(description) ]

dis <- unique(a2[!Code %in% c("", " ", "A2.0") & refcode == "A2.0", c("mr_no", "studyday", "refday", "Code", "description", "refcode", "refdesc")])
dis <- dis [, `:=` (refday2 = ifelse(refday >=1, "After", "Before"), 
                    Code = str_replace_all (Code, " ", ""),
                    description = str_replace_all(description, " ", "")),]
dis <- dis [ order(mr_no, studyday, Code, refcode, refdesc)]
dis <- dis [, `:=` (alldis = uniqueN(Code), 
                    nrow = seq_len(.N),
                    nrowend = seq_len(.N) + 4,
                    totrow = .N), by = .(mr_no, refcode, refdesc)]
dis <- dis [, `:=` (alldisbfraftr = uniqueN(Code), 
                    nrowbfraftr = seq_len(.N) ), by = .(mr_no, refcode, refdesc, refday2)]

dis02 <- dis [, .(combdis = paste(unique(Code), collapse = ",", sep = " " )), 
              by = .(mr_no, refcode, refdesc, refday2, alldis, totrow)]

unq01comb <- unique( dis02 [, c("mr_no", "refcode", "refdesc", "alldis", "refday2",
                                "totrow",  "combdis"), ])
unq01comb <- unq01comb [, x := 1, ]

# create a copy
unq02comb <- copy(unq01comb)
setnames(unq02comb, "mr_no", "mr_no2")
setnames(unq02comb, "combdis", "combdis2")

unq01comb <- unq01comb [, combdis := str_replace_all(combdis, ",", "|"), ]

# Merge the datasets on x to get all the combinations

unq03comb <- merge(x = unq01comb, 
                   y = unq02comb [, -c("refcode", "refdesc", "totrow", "alldis"), ], 
                   by = c("x", "refday2"), 
                   allow.cartesian = TRUE)

########################################################
# Using str_count function to count the common diseases
# Create tempdis and tempdis2
#
# Consider mr_no as the reference patient
# tempdis: should be lookup
# a: common in both the strings
# b: only present in reference patient (mr_no)
# c: only present in other patient (mr_no2)
# d: complete absence -- not sure how to calculate this
########################################################

unq03comb <- unq03comb [, `:=` (tempdis = str_replace_all(combdis, ",", "|"), 
                                tempdis2 = str_replace_all(combdis2, ",", "|")),]

unq03comb <- unq03comb [, `:=` (cntdis = str_count(tempdis, "\\|") + 1, 
                                cntdis2 = str_count(tempdis2, "\\|") + 1), ]

unq03comb <- unq03comb[, `:=` (a = str_count(combdis2, tempdis)),]

unq03comb <- unq03comb [, `:=` (b = cntdis - a,
                                c = cntdis2 - a),  ]

unq03comb <- unq03comb[, a01jac := (a / (a + b + c)),]

distraj <- unique(unq03comb [tempdis != tempdis2 , c("tempdis", "tempdis2", "a01jac", "refday2"),])

distraj01 <- distraj [, .(distraj = .N), by = .(refday2, a01jac)]

common <- unq03comb [, .(cmn = (.N / nrow(unq03comb)) * 100), by = .(a)]
