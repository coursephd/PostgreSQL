library(tidyverse)
library(tidytext)
#library(stringr)
library(stringi)
library(data.table)
library(stringdist)
library(scales)

all_met_rmsd <- readRDS ("D:/Hospital_data/ProgresSQL/analysis/all_met_rmsd02.rds")
a01all <- readRDS ("D:/Hospital_data/ProgresSQL/analysis/080_medicine_repeat_prop_cumulative_Rcal.rds")

a02all <- a01all [, `:=` (maxdis = max(totdis),
                          maxmed = max(totdose)), by = .(mr_no)]


a03all <- separate_rows(a01all, `discomb_1st time disease`, sep =";") 
a03all <- separate_rows(a03all, discomb_Repeat, sep =";") 
a03all <- separate_rows(a03all, `descomb_1st time disease`, sep =";") 
a03all <- separate_rows(a03all, descomb_Repeat, sep =";") 
a03all <- separate_rows(a03all, `Combine1st dose`, sep =";") 
a03all <- separate_rows(a03all, CombineRepeat, sep =";") 

a03all[is.na(a03all)] <- ""

a04all <- a03all [, `:=` (`discomb_1st time disease` = ifelse(nchar(trimws(`discomb_1st time disease`)) > 7, paste("1", `discomb_1st time disease`, sep= "|"), ""),
                          `descomb_1st time disease` = ifelse(nchar(trimws(`descomb_1st time disease`)) > 7, paste("1", `descomb_1st time disease`, sep= "|"), ""),
                          `Combine1st dose` = ifelse(nchar(trimws(`Combine1st dose`)) > 7, paste("1", `Combine1st dose`, sep= "|"), ""),
                          CombineRepeat = ifelse(nchar(trimws(CombineRepeat)) > 7, paste("Rpt", CombineRepeat, sep= "|"), ""),
                          discomb_Repeat = ifelse(nchar(trimws(discomb_Repeat)) > 7, paste("Rpt", discomb_Repeat, sep= "|"), ""),
                          descomb_Repeat = ifelse(nchar(trimws(descomb_Repeat)) > 7, paste("Rpt", descomb_Repeat, sep= "|"), "")
                          ),]

a04all <- a04all [, `:=`( discomb = paste(`discomb_1st time disease`, discomb_Repeat, sep =";"), 
                          descomb = paste(`descomb_1st time disease`, descomb_Repeat, sep =";"),
                          doscomb = paste(`Combine1st dose`, CombineRepeat, sep =";")) ,]

a04all <- a04all[, `:=` ( discomb = ifelse ( substr(discomb, 1, 1)== ";", substr(discomb, 2, nchar(discomb)), discomb),
                          descomb = ifelse ( substr(descomb, 1, 1)== ";", substr(descomb, 2, nchar(descomb)), descomb),
                          doscomb = ifelse ( substr(doscomb, 1, 1)== ";", substr(doscomb, 2, nchar(doscomb)), doscomb)
                                            ), ]

a04all <- a04all[, `:=` ( discomb = ifelse ( substr(stri_reverse(discomb), 1, 1)== ";", substr(discomb, 1, nchar(discomb)-1), discomb),
                          descomb = ifelse ( substr(stri_reverse(descomb), 1, 1)== ";", substr(descomb, 1, nchar(descomb)-1), descomb),
                          doscomb = ifelse ( substr(stri_reverse(doscomb), 1, 1)== ";", substr(doscomb, 1, nchar(doscomb)-1), doscomb)
                                            ), ]

a05all <- separate_rows(a04all, discomb, sep =";") 
a05all <- separate_rows(a05all, descomb, sep =";") 
a05all <- separate_rows(a05all, doscomb, sep =";") 

a05all [, c("disnum", "discomb01") := tstrsplit(discomb, "|", fixed=TRUE), ]
a05all [, c("desnum", "descomb01") := tstrsplit(descomb, "|", fixed=TRUE), ]
a05all [, c("dosnum", "doscomb01") := tstrsplit(doscomb, "|", fixed=TRUE), ]

a06all <- a05all [, -c("discomb", "descomb", "doscomb", "NA",
                        "CombineRepeat", "Combine1st dose", 
                       "discomb_Repeat", "descomb_Repeat", 
                       "discomb_1st time disease", "descomb_1st time disease"),]


##############################################################
# Create the number of diseases and number of times they occur
# Similarly, create number of treatments appearing for each
# disease
##############################################################

a06all <- a06all [ order(mr_no, grpday, cumday2, cumday3)]

dis <- unique( a06all [, c("mr_no", "discomb01"),])
dis <- dis [, disnum_cal := 1:.N, by =.(mr_no)]

dis00 <- unique( a06all [, c("mr_no", "discomb01", "grpday"),])
dis00 <- dis00 [, distimes := 1:.N, by =.(mr_no, discomb01)]

med <- unique( a06all [, c("mr_no", "doscomb01"),])
med <- med [, dosnum_cal := 1:.N, by =.(mr_no)]

med00 <- unique( a06all [, c("mr_no", "doscomb01", "grpday"),])
med00 <- med00 [, dostimes := 1:.N, by =.(mr_no, doscomb01)]

dismed <- unique( a06all [, c("mr_no", "discomb01", "doscomb01", "grpday"),])
dismed <- dismed [, bothnum_cal := 1:.N, by =.(mr_no, discomb01, doscomb01)]

a07all <- merge( x = a06all,
                 y = dis,
                 by = c("mr_no", "discomb01"))

a07all <- merge( x = a07all,
                 y = med,
                 by = c("mr_no", "doscomb01"))

a07all <- merge( x = a07all,
                 y = dismed,
                 by = c("mr_no", "discomb01", "doscomb01", "grpday"))

a07all <- merge( x = a07all,
                 y = dis00,
                 by = c("mr_no", "discomb01", "grpday"))

a07all <- merge( x = a07all,
                 y = med00,
                 by = c("mr_no", "doscomb01", "grpday"))

saveRDS (a07all, "D:/Hospital_data/ProgresSQL/analysis/080_medcine_repeat_prop_addnl.rds")
