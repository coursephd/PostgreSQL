####################################################################
# 086_dis_patterns_combinations_gender_Macro.R
####################################################################

library(tidyverse)
library(tidytext)
#library(stringr)
library(stringi)
library(data.table)
library(stringdist)
library(scales)

# https://stackoverflow.com/questions/43706729/expand-dates-in-data-table
dis <- fread("D:/Hospital_data/ProgresSQL/analysis/discategory.csv")
setnames (dis, "Code", "refcode")

all_met_rmsd <- readRDS ("D:/Hospital_data/ProgresSQL/analysis/all_met_rmsd02.rds")

############################################
# Find patients with only the disease 
# same as reference disease
# 1 = patients with only disease
# 99 = patients with more than 1 disease in
# a reference disease category
############################################

addmr <- unique( all_met_rmsd [!Code %in% c(" ", ""), c("mr_no", "refcode", "Code", "distype"),])
addmr <- addmr [, cnt := uniqueN(refcode), by  = .(mr_no)] 
addmr <- addmr [, dis := ifelse(refcode == Code, 1, 0),]
addmr <- addmr [, calc := ifelse(cnt == 1 & dis == 1, 1, 99),]

addmr02 <- addmr [, .(cntr = uniqueN(mr_no)), by = .(distype, refcode, Code, calc)]
addmr03 <- addmr [, .(cntot = uniqueN(mr_no)), by = .(refcode)]
addmr04 <- merge(addmr02, addmr03, by = c("refcode"))
addmr04 <- addmr04 [, perc := percent(cntr / cntot),]
addmr05 <- addmr04 [ refcode == Code]
addmr06 <- merge (addmr05, dis, by = c("refcode"), all.y = TRUE)

unq <- unique(addmr06 [cntot > 5, c("refcode"),])
unqdis <- unique(unq$refcode)


count <- 1

for ( dis in unqdis[1:uniqueN(unqdis)])
{ print (dis)
  print (count)
  
  a2 <- all_met_rmsd [!Code %in% c("", " ", dis) & refcode == dis]
  
  #a2med <- a2 [, description := paste(Type_med, Coded_med),]
  #a2med <- a2med [ order(description)]
  #a2med <- a2med [, Code := paste("M", str_pad(.N, width =4, pad="0"), sep =""), by = .(description) ]
  
  #a2all <- rbind(a2 [, c("mr_no", "studyday", "refday", "Code", "description", "refcode", "refdesc", "patient_gender")], 
  #               a2med [, c("mr_no", "studyday", "refday", "Code", "description", "refcode", "refdesc", "patient_gender")] )
  
  # Change a2 to a2all
  dis <- unique(a2[, c("mr_no", "studyday", "refday", "Code", "description", "refcode", "refdesc", "patient_gender")])
  dis <- dis [, `:=` (refday2 = ifelse(refday >=1, "After", "Before"), 
                      Code = str_replace_all (Code, " ", ""),
                      description = str_replace_all(description, " ", "")),]
  dis <- dis [ order(mr_no, studyday, Code, refcode, refdesc)]
  dis <- dis [, `:=` (alldis = uniqueN(Code), 
                      nrow = seq_len(.N),
                      nrowend = seq_len(.N) + 4,
                      totrow = .N), by = .(mr_no, refcode, refdesc)]
  dis <- dis [, `:=` (alldisbfraftr = uniqueN(Code), 
                      nrowbfraftr = seq_len(.N) ), by = .(mr_no, refcode, refdesc, refday2, patient_gender)]
  dis <- dis [, `:=` (total = uniqueN(mr_no) ), by = .(refcode, refdesc, refday2, patient_gender)]
  dis <- dis [, `:=` (allcapn = uniqueN(mr_no) ), by = .(refcode, refdesc)]
  
  dis02 <- dis [, .(combdis = paste(unique(Code), collapse = ",", sep = " " ),
                    combdesc = paste(unique(description), collapse = ",", sep = " " )), 
                by = .(mr_no, refcode, refdesc, refday2, patient_gender, alldis, totrow, total, allcapn)]
  
  unq01comb <- unique( dis02 [, c("mr_no", "refcode", "refdesc", "alldis", "refday2","patient_gender",
                                  "totrow",  "combdis", "combdesc", "total", "allcapn"), ])
  unq01comb <- unq01comb [, x := 1, ] 
  
  # create a copy
  unq02comb <- copy(unq01comb)
  setnames(unq02comb, "mr_no", "mr_no2")
  setnames(unq02comb, "combdis", "combdis2")
  
  unq01comb <- unq01comb [, combdis := str_replace_all(combdis, ",", "|"), ]
  
  # Merge the datasets on x to get all the combinations
  
  unq03comb <- merge(x = unq01comb, 
                     y = unq02comb [, -c("refcode", "refdesc", "totrow", "alldis", "total", "allcapn", "combdesc"), ], 
                     by = c("x", "refday2", "patient_gender"), 
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
  
  unq03comb <- unq03comb[, `:=` (a01jac = (a / (a + b + c)),
                                 a02dice = (2 * a / (2* a + b + c) ),
                                 a03CZEKANOWSKI = (2 * a / (2* a + b + c) ),
                                 a04jac3w = (3 * a / (3* a + b + c) ),
                                 a05nei_li = (2 * a /  (a + b + a + c) ),
                                 a06sokalsneath1 = (a / (a + 2 * b + 2 * c)) ),]
  unq03comb <- unq03comb [ mr_no != mr_no2]
  
  
  maxscr <- unq03comb[, .(maxscr = max(a01jac) ), by = .(mr_no, refcode, total, allcapn, totrow, alldis, refday2, patient_gender, combdis, combdesc)]
  maxscr_t <- dcast (data = maxscr, 
                     mr_no + patient_gender + refcode + totrow + alldis ~ refday2, 
                     value.var = c("maxscr", "combdis", "combdesc"))
  
  maxscr02 <- maxscr [, .(scr = uniqueN(mr_no)), by = .(refcode, total, allcapn, refday2, patient_gender, cut(maxscr,  
                                                                                                              seq(0, 1, .25), 
                                                                                                              include.lowest = TRUE,
                                                                                                              ordered_result = TRUE))]
  maxscr02_t <- dcast(data = maxscr02, 
                      refcode + patient_gender + allcapn + cut ~ refday2,
                      value.var = c("scr", "total"))
  
  maxscr03 <- maxscr [, .(scr = uniqueN(mr_no)), by = .(refcode, total, allcapn, maxscr, combdis, combdesc, refday2, patient_gender)]
  maxscr03_t <- dcast(data = maxscr03, 
                      refcode + patient_gender + allcapn + combdis + combdesc + maxscr ~ refday2,
                      value.var = c("scr", "total"))
  
  maxscr04_t <- unq03comb [, .(scr = .N), by = .(mr_no, refcode, total, allcapn, refday2, patient_gender, cut(a01jac,  
                                                                                                              seq(0, 1, .25), 
                                                                                                              include.lowest = TRUE,
                                                                                                              ordered_result = TRUE))]
  maxscr04_t <- maxscr04_t [, numrow := .N, by = . (mr_no, refcode, refday2, patient_gender)]
  
  totscr <- unq03comb [, .( rowcnt = .N), by = .(refcode, total, allcapn, refday2, patient_gender, cut(a01jac, 
                                                                                                       seq(0, 1, .25), 
                                                                                                       include.lowest = TRUE,
                                                                                                       ordered_result = TRUE) )]
  totscr02 <- unq03comb [, .(totn = .N), by = .(refcode, refday2, patient_gender, total, allcapn )]
  totscr02 <- merge (totscr, totscr02, by = c("refcode", "refday2", "patient_gender", "total", "allcapn"))
  totscr02 <- totscr02 [, perc := percent( rowcnt / totn),]
  totscr02_t <- dcast(data = totscr02,
                      refcode + patient_gender + allcapn + cut ~ refday2,
                      value.var = c("perc", "totn", "rowcnt", "total"))
  
  assign ( paste("D01maxscr_t", count, sep="") ,  maxscr_t)
  assign ( paste("D02maxscr02_t", count, sep="") ,  maxscr02_t)
  assign ( paste("D03maxscr03_t", count, sep="") ,  maxscr03_t)
  assign ( paste("D03maxscr04_t", count, sep="") ,  maxscr04_t)
  
  assign ( paste("t02totscr02_t", count, sep="") ,  totscr02_t)
  
  count = count + 1
  
}

allD01maxscr_t <- rbindlist(mget(ls(pattern = "D01maxscr_t*")), fill = TRUE)
allD02maxscr02_t <- rbindlist(mget(ls(pattern = "D02maxscr02_t*")), fill = TRUE)
allD03maxscr03_t <- rbindlist(mget(ls(pattern = "D03maxscr03_t*")), fill = TRUE)
allD03maxscr04_t <- rbindlist(mget(ls(pattern = "D03maxscr04_t*")), fill = TRUE)

allt02maxscr02_t <- rbindlist(mget(ls(pattern = "t02totscr02_t*")), fill = TRUE)

rm(list = ls( pattern='^D01maxscr_t*'))
rm(list = ls( pattern='^D02maxscr02_t*'))
rm(list = ls( pattern='^D03maxscr03_t*'))
rm(list = ls( pattern='^D03maxscr04_t*'))
rm(list = ls( pattern='^t02totscr02_t*'))

fwrite(allD01maxscr_t, "D:/Hospital_data/ProgresSQL/analysis/086_dis_indPat_max.csv")
fwrite(allD02maxscr02_t, "D:/Hospital_data/ProgresSQL/analysis/086_dis_refcode_max.csv")
fwrite(allD03maxscr03_t, "D:/Hospital_data/ProgresSQL/analysis/086_dis_indtrajectory.csv")
fwrite(allD03maxscr04_t, "D:/Hospital_data/ProgresSQL/analysis/086_dis_indPat_freqcat.csv")
fwrite(allt02maxscr02_t, "D:/Hospital_data/ProgresSQL/analysis/086_dis_refcode_allfreq_max.csv")

