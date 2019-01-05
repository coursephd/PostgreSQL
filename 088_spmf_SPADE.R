library(data.table)
library(tidyverse)

all_met_rmsd02 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/all_met_rmsd02.rds")
all_met_rmsd02 <- all_met_rmsd02 [, Code := ifelse (Code == " " | Code == "", "** Not yet coded", Code),]
all_met_rmsd02 <- all_met_rmsd02 [, description:= ifelse (description == "" | description ==" ", "** Not yet coded", description),]
all_met_rmsd02 <- all_met_rmsd02 [, Code02 := paste(Code, ":", description, sep =""), ]

all_met_rmsd02 <- all_met_rmsd02[, comdisn := .GRP, by = .(Code02)]

all_met_rmsd02 <- all_met_rmsd02[Code != "** Not yet coded"]

all_met_rmsd03 <- unique( all_met_rmsd02 [, c("mr_no", "refcode", "refdesc", "Code02", "comdisn",
                                              "period", "periodn", "patient_gender", "baseage"), ])

all_met_rmsd04 <- all_met_rmsd03 [ refcode == "M2.0"]

all_met_rmsd05 <- all_met_rmsd04 [, .(combdis = paste(comdisn, collapse = " ", sep = " " )), 
                                  by = .(mr_no, refcode, refdesc, period, periodn, baseage)]


# create 1 line per patient
# -1 to seperate itemset and
# -2 to seperate sequence

all_met_rmsd06 <- all_met_rmsd05 [, .(combdis02 = paste(combdis, collapse = " -1 ", sep = " " )), 
                                  by = .(mr_no, refcode, refdesc, baseage)]
all_met_rmsd06 <- all_met_rmsd06 [, combdis02 := paste(combdis02, " -1 -2", sep = ""), ]

fwrite(x = all_met_rmsd06 [, c("combdis02"),], 
       col.names = FALSE,
       file = "D:/Hospital_data/ProgresSQL/analysis_spmf/spmf_SPADE_M2.0.txt")

disnum <- unique( all_met_rmsd02 [, c("Code02", "Code", "comdisn"),])



# Layout needed for the Associatio rules:
# FPGrowth_association_rules

all_met_rmsd06_arff <- all_met_rmsd05 [, .(combdis02 = paste(combdis, collapse = " ", sep = " " )), 
                                       by = .(mr_no, refcode, refdesc, baseage)]

fwrite(x = all_met_rmsd06_arff [, c("combdis02"),], 
       col.names = FALSE,
       file = "D:/Hospital_data/ProgresSQL/analysis_spmf/spmf_ARFF_M2.0.txt")



out <- fread("D:/Hospital_data/ProgresSQL/analysis_spmf/spmf_ARFFoutput_TopKRules_M2.0.txt", 
             sep ="!",
             header = FALSE)

out2 <- out [, c("var01", "var02", "var03") := tstrsplit(V1, "#"),]
out3 <- out2 [, c("var021", "var022") := tstrsplit(var01, "==>"),]
out4 <- out3 [, `:=` (cntvar021 = str_count( trimws(var021), " ") + 1,
                      cntvar022 = str_count( trimws(var022), " ") + 1),]

out5 <- out4 [, paste0("type", 1:max(out3$cntvar021)) := tstrsplit(var021, " "),]



library(openxlsx)
library(tidyverse)
library(tidytext)
library(stringi)
library(data.table)
library(stringdist)
library(scales)
library(anytime)
library(arulesSequences)

all_met_rmsd02 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/all_met_rmsd02.rds")
all_met_rmsd02 <- all_met_rmsd02 [, Code := ifelse (Code == " " | Code == "", "** Not yet coded", Code),]
all_met_rmsd02 <- all_met_rmsd02 [, description:= ifelse (description == "" | description ==" ", "** Not yet coded", description),]
all_met_rmsd02 <- all_met_rmsd02 [, Code02 := paste(Code, ":", description, sep =""), ]

all_met_rmsd03 <- unique( all_met_rmsd02 [, c("mr_no", "refcode", "refdesc", "Code02",
                                              "period", "periodn", "patient_gender", "baseage"), ])
                          
all_met_rmsd04 <- all_met_rmsd03 [ refcode == "A2.0"]

all_met_rmsd05 <- all_met_rmsd04 [, .(combdis = str_replace_all(paste(unique(Code02), collapse = "|", sep = "" ), " ", "")), 
              by = .(mr_no, refcode, refdesc, period, periodn, baseage)]

# create 1 line per patient
# -1 to seperate itemset and
# -2 to seperate sequence

all_met_rmsd06 <- all_met_rmsd05 [, .(combdis02 = paste(combdis, collapse = " -1 ", sep = " " )), 
                                  by = .(mr_no, refcode, refdesc, baseage)]
all_met_rmsd06 <- all_met_rmsd06 [, combdis02 := paste(combdis02, " -1 -2 ", sep = ""), ]

fwrite(x = all_met_rmsd06 [, c("combdis02"),], 
       col.names = FALSE,
       file = "D:/Hospital_data/ProgresSQL/analysis/spmf_SPADE.txt")


all_met_rmsd <- readRDS("D:/Hospital_data/ProgresSQL/analysis/01adsl_met_rmsd.rds")
all_met_rmsd <- all_met_rmsd [, Code := ifelse (Code == " " | Code == "", "** Not yet coded", Code),]
all_met_rmsd <- all_met_rmsd [, description:= ifelse (description == "" | description ==" ", "** Not yet coded", description),]
all_met_rmsd <- all_met_rmsd [, discomb01:= paste(distype, Code, sep= ":"),]
all_met_rmsd <- all_met_rmsd [, descomb01:= paste(distype, description, sep= ":"),]
all_met_rmsd <- all_met_rmsd [, Type_med := ifelse (Type_med == " " | Type_med == "", "** Not yet coded", Type_med),]
all_met_rmsd <- all_met_rmsd [, Coded_med := ifelse (Coded_med == " " | Coded_med == "", "** Not yet coded", Coded_med),]
all_met_rmsd <- all_met_rmsd [, doscomb01:= paste(Type_med, Coded_med, sep= ":"),]
all_met_rmsd <- all_met_rmsd [, grpday := studyday]








unqdis <- unique(all_met_rmsd [, c("Code", "description", "discomb01", "descomb01"),])
fwrite(unqdis, "D:/Hospital_data/ProgresSQL/analysis/unqdis.csv")
       

dist <- unique( all_met_rmsd [, c("mr_no", "studyday", "Code", "description" ), ])
dist <- dist [ order(mr_no, Code, description, studyday)]
studyday_dis <- dist[ , diff := studyday - shift(studyday), by = .(mr_no, Code, description)] 


dis <- unique( all_met_rmsd [, c("mr_no", "discomb01"),])
dis <- dis [, disnum_cal := 1:.N, by =.(mr_no)]

dis00 <- unique( all_met_rmsd [, c("mr_no", "discomb01", "grpday"),])
dis00 <- dis00 [, distimes := 1:.N, by =.(mr_no, discomb01)]

med <- unique( all_met_rmsd [, c("mr_no", "doscomb01"),])
med <- med [, dosnum_cal := 1:.N, by =.(mr_no)]

med00 <- unique( all_met_rmsd [, c("mr_no", "doscomb01", "grpday"),])
med00 <- med00 [, dostimes := 1:.N, by =.(mr_no, doscomb01)]

dismed <- unique( all_met_rmsd [, c("mr_no", "discomb01", "doscomb01", "grpday"),])
dismed <- dismed [, bothnum_cal := 1:.N, by =.(mr_no, discomb01, doscomb01)]



a07all <- readRDS ("D:/Hospital_data/ProgresSQL/analysis/080_medcine_repeat_prop_addnl.rds")

a08all <- data.table(unique ( a07all) )

temp <- unique(a07all [mr_no == "MR006269", c("discomb01"),])


temp2 <- a07all [, .(max = max(disnum_cal)), by = .(mr_no, maxdis)]
temp3 <- temp2 [ max != maxdis]

dis <- unique( a06all [, c("mr_no", "discomb01", "descomb01"),])
dis <- dis [, disnum_cal := 1:.N, by =.(mr_no)]


temp <- unique(data03 [refcode == "A2.0", c("orig"), ])

all_met_rmsd <- readRDS ("D:/Hospital_data/ProgresSQL/analysis/all_met_rmsd02.rds")
addmr <- unique( all_met_rmsd [!Code %in% c(" ", ""), c("Code", "description", "distype"),])
fwrite(addmr, "D:/Hospital_data/ProgresSQL/misc/HumanBody/temp.csv")

####################################################################
# 086_dis_patterns_combinations_gender_addcalcs.R
####################################################################


# https://stackoverflow.com/questions/43706729/expand-dates-in-data-table
dis <- fread("D:/Hospital_data/ProgresSQL/analysis/discategory.csv")
setnames (dis, "Code", "refcode")

all_met_rmsd <- readRDS ("D:/Hospital_data/ProgresSQL/analysis/all_met_rmsd02.rds")

a2med <- all_met_rmsd [, description := paste(Type_med, Coded_med),]
a2med <- a2med [ order(description)]
a2med <- a2med [, Code := paste("M", str_pad(.N, width =5, pad="0"), sep =""), by = .(description) ]

a2all <- rbind(a2 [, c("mr_no", "studyday", "refday", "Code", "description", "refcode", "refdesc", "patient_gender")], 
               a2med [, c("mr_no", "studyday", "refday", "Code", "description", "refcode", "refdesc", "patient_gender")] )

# Change a2 to a2all

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
  
  a2 <- a2all [!Code %in% c("", " ") & refcode == dis]
  
  dis <- unique(a2[, c("mr_no", "studyday", "refday", "Code", "description", "refcode", "refdesc", "patient_gender")])
  dis <- dis [, `:=` (refday2 = ifelse(refday >=1, paste("After",patient_gender, sep =""),  paste("Before", patient_gender, sep="")), 
                      Code = str_replace_all (Code, " ", ""),
                      description = str_replace_all(description, " ", "")),]
  dis <- dis [ order(mr_no, studyday, Code, refcode, refdesc)]
  dis <- dis [, `:=` (alldis = uniqueN(Code), 
                      nrow = seq_len(.N),
                      nrowend = seq_len(.N) + 4,
                      totrow = .N), by = .(mr_no, refcode, refdesc)]
  dis <- dis [, `:=` (alldisbfraftr = uniqueN(Code), 
                      nrowbfraftr = seq_len(.N) ), by = .(mr_no, refcode, refdesc, refday2)]
  dis <- dis [, `:=` (total = uniqueN(mr_no) ), by = .(refcode, refdesc, refday2)]
  dis <- dis [, `:=` (allcapn = uniqueN(mr_no) ), by = .(refcode, refdesc)]
  
  dis02 <- dis [, .(combdis = paste(unique(Code), collapse = ",", sep = " " )), 
                by = .(mr_no, refcode, refdesc, refday2, alldis, totrow, total, allcapn)]
  
  unq01comb <- unique( dis02 [, c("mr_no", "refcode", "refdesc", "alldis", "refday2",
                                  "totrow",  "combdis", "total", "allcapn"), ])
  unq01comb <- unq01comb [, x := 1, ]
  
  # create a copy
  unq02comb <- copy(unq01comb)
  setnames(unq02comb, "mr_no", "mr_no2")
  setnames(unq02comb, "combdis", "combdis2")
  
  unq01comb <- unq01comb [, combdis := str_replace_all(combdis, ",", "|"), ]
  
  # Merge the datasets on x to get all the combinations
  
  unq03comb <- merge(x = unq01comb, 
                     y = unq02comb [, -c("refcode", "refdesc", "totrow", "alldis", "total", "allcapn"), ], 
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
  
  unq03comb <- unq03comb[, `:=` (a01jac = (a / (a + b + c)),
                                 a02dice = (2 * a / (2* a + b + c) ),
                                 a03CZEKANOWSKI = (2 * a / (2* a + b + c) ),
                                 a04jac3w = (3 * a / (3* a + b + c) ),
                                 a05nei_li = (2 * a /  (a + b + a + c) ),
                                 a06sokalsneath1 = (a / (a + 2 * b + 2 * c)) ),]
  unq03comb <- unq03comb [ mr_no != mr_no2]
  
  
  maxscr <- unq03comb[, .(maxscr = max(a01jac) ), by = .(mr_no, refcode, total, allcapn, totrow, alldis, refday2, combdis)]
  maxscr_t <- dcast (data = maxscr, 
                     mr_no + refcode + totrow + alldis ~ refday2, 
                     value.var = c("maxscr", "combdis"))
  
  maxscr02 <- maxscr [, .(scr = uniqueN(mr_no)), by = .(refcode, total, allcapn, refday2, cut(maxscr,  
                                                                                              seq(0, 1, .25), 
                                                                                              include.lowest = TRUE,
                                                                                              ordered_result = TRUE))]
  maxscr02_t <- dcast(data = maxscr02, 
                      refcode + allcapn + cut ~ refday2,
                      value.var = c("scr", "total"))
  
  maxscr03 <- maxscr [, .(scr = uniqueN(mr_no)), by = .(refcode, total, allcapn, maxscr, combdis, refday2)]
  maxscr03_t <- dcast(data = maxscr03, 
                      refcode + allcapn + combdis + maxscr ~ refday2,
                      value.var = c("scr", "total"))
  
  maxscr04 <- unq03comb [, .(scr = .N), by = .(mr_no, refcode, total, allcapn, refday2, cut(a01jac,  
                                                                                            seq(0, 1, .25), 
                                                                                            include.lowest = TRUE,
                                                                                            ordered_result = TRUE))]
  maxscr04 <- maxscr04 [, numrow := .N, by = . (mr_no, refcode, refday2)]
  
  totscr <- unq03comb [, .( rowcnt = .N), by = .(refcode, total, allcapn, refday2, cut(a01jac, 
                                                                                       seq(0, 1, .25), 
                                                                                       include.lowest = TRUE,
                                                                                       ordered_result = TRUE) )]
  totscr02 <- unq03comb [, .(totn = .N), by = .(refcode, refday2, total, allcapn )]
  totscr02 <- merge (totscr, totscr02, by = c("refcode", "refday2", "total", "allcapn"))
  totscr02 <- totscr02 [, perc := percent( rowcnt / totn),]
  totscr02_t <- dcast(data = totscr02,
                      refcode + allcapn + cut ~ refday2,
                      value.var = c("perc", "totn", "rowcnt", "total"))
  
  #dismaxscr02_t <- maxscr02_t
  
  assign ( paste("D01maxscr_t", count, sep="") ,  maxscr_t)
  assign ( paste("D02maxscr02_t", count, sep="") ,  maxscr02_t)
  assign ( paste("D03maxscr03_t", count, sep="") ,  maxscr03_t)
  assign ( paste("D03maxscr04", count, sep="") ,  maxscr04)
  
  assign ( paste("t02totscr02_t", count, sep="") ,  totscr02_t)
  
  count = count + 1
  
}

allD01maxscr_t <- rbindlist(mget(ls(pattern = "D01maxscr_t*")), fill = TRUE)
allD02maxscr02_t <- rbindlist(mget(ls(pattern = "D02maxscr02_t*")), fill = TRUE)
allD03maxscr03_t <- rbindlist(mget(ls(pattern = "D03maxscr03_t*")), fill = TRUE)
allD03maxscr04 <- rbindlist(mget(ls(pattern = "D03maxscr04*")), fill = TRUE)
allt02maxscr02_t <- rbindlist(mget(ls(pattern = "t02totscr02_t*")), fill = TRUE)

rm(list = ls( pattern='^D01maxscr_t*'))
rm(list = ls( pattern='^D02maxscr02_t*'))
rm(list = ls( pattern='^D03maxscr03_t*'))
rm(list = ls( pattern='^D03maxscr04*'))
rm(list = ls( pattern='^t02totscr02_t*'))



capn <- unique(all_met_rmsd [RMSD ==1, c("mr_no", "cstdt", "cendt", "cdur", "Metabolic", "RMSD"), ])


capn <- unique(all_met_rmsd [RMSD ==1, c("mr_no", "cstdt", "cendt", "cdur", "Metabolic", "RMSD", "newdt0"), ])
capn <- capn [, cnt :=1, ]
sum(capn$cnt) / 365.25

capn <- unique(all_met_rmsd [Metabolic ==1, c("mr_no", "cstdt", "cendt", "cdur", "Metabolic", "RMSD", "newdt0", "age"), ])
capn <- capn [, cnt :=1, ]
sum(capn$cnt) / 365.25

nrow( capn [age > 0])
