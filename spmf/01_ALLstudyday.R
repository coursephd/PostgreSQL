library(data.table)
library(tidyverse)

all_met_rmsd02 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/01adsl_met_rmsd.rds")
all_met_rmsd02 <- all_met_rmsd02 [, Code := ifelse (Code == " " | Code == "", "** Not yet coded", Code),]
all_met_rmsd02 <- all_met_rmsd02 [, description:= ifelse (description == "" | description ==" ", "** Not yet coded", description),]
all_met_rmsd02 <- all_met_rmsd02 [, Code02 := paste(Code, ":", description, sep =""), ]

all_met_rmsd02 <- all_met_rmsd02[, comdisn := .GRP, by = .(Code02)]

all_met_rmsd02 <- all_met_rmsd02[Code != "** Not yet coded"]

all_met_rmsd02 <- all_met_rmsd02 [ order(mr_no, studyday, Code02)]
all_met_rmsd03 <- unique( all_met_rmsd02 [, c("mr_no",  "Code02", "studyday",
                                              "comdisn", "patient_gender", "baseage"), ])

all_met_rmsd04 <- all_met_rmsd03 [ 1 == 1]

all_met_rmsd05 <- all_met_rmsd04 [, .(combdis = paste(comdisn, collapse = " ", sep = " " )), 
                                  by = .(mr_no, studyday, baseage)]


# create 1 line per patient
# -1 to seperate itemset and
# -2 to seperate sequence

all_met_rmsd06 <- all_met_rmsd05 [, .(combdis02 = paste(combdis, collapse = " -1 ", sep = " " )), 
                                  by = .(mr_no, baseage)]
all_met_rmsd06 <- all_met_rmsd06 [, combdis02 := paste(combdis02, " -1 -2", sep = ""), ]

fwrite(x = all_met_rmsd06 [, c("combdis02"),], 
       col.names = FALSE,
       file = "D:/Hospital_data/ProgresSQL/analysis_spmf/01input_SPADE_ALLstudyday.txt")

disnum <- unique( all_met_rmsd02 [, c("Code02", "Code", "comdisn"),])
disnum <- disnum [, comdisn := as.numeric(comdisn),]



# Layout needed for the Associatio rules:
# FPGrowth_association_rules

all_met_rmsd06_arff <- all_met_rmsd05 [, .(combdis02 = paste(combdis, collapse = " ", sep = " " )), 
                                       by = .(mr_no, baseage)]

fwrite(x = all_met_rmsd06_arff [, c("combdis02"),], 
       col.names = FALSE,
       file = "D:/Hospital_data/ProgresSQL/analysis_spmf/01input_ARFF_ALLstudyday.txt")



out <- fread("D:/Hospital_data/ProgresSQL/analysis_spmf/01output_SPADE_TopSeqRules_ALLstudyday.txt", 
             sep ="!",
             header = FALSE)

out <- out [, V1 := str_replace_all(V1, ",", " "),] 
out2 <- out [, c("var01", "var02", "var03") := tstrsplit(V1, "#"),]
out3 <- out2 [, c("var021", "var022") := tstrsplit(trimws(var01), "==>"),]
out4 <- out3 [, `:=` (cntvar021 = max(str_count( trimws(var021), " ")) + 1,
                      cntvar022 = max(str_count( trimws(var022), " ")) + 1 ),]

out5 <- out4 [, paste0("stt", 1:max(out4$cntvar021)) := tstrsplit(trimws(var021), " ", fixed = TRUE ),]
out6 <- out5 [, paste0("end", 1:max(out5$cntvar022)) := tstrsplit(trimws(var022), " ", fixed = TRUE),]
#out6 <- out6 [, nrow := 1:.N,]

out6_tra <- melt (data = out6, 
                  id.vars = 1:8,
                  value.factor = FALSE)

out6_tra <- out6_tra [, value := as.numeric(value), ]

out7 <- merge (x = out6_tra, 
               y = disnum,
               by.x = c("value"),
               by.y = c("comdisn") )

out7_tra <- dcast (data = out7,
                   formula = V1 + var01 + var02 + var03 + var021 + var022 + cntvar021 + cntvar022 ~ variable,
                   value.var = c("Code02"), 
                   fill = "")

out8 <- out7_tra [, newstt := do.call(paste, c(.SD, sep = " ")), .SDcols = paste0("stt", 1:max(out7_tra$cntvar021)), ]
out8 <- out8 [, newend := do.call(paste, c(.SD, sep = " ")), .SDcols = paste0("end", 1:max(out8$cntvar022)), ]

out9 <- out8 [, c("newstt", "newend", "var02", "var03"),]

fwrite(out9, "D:/Hospital_data/ProgresSQL/analysis_spmf/01output_SPADE_TopSeqRules_ALLstudyday_formatted.txt")
