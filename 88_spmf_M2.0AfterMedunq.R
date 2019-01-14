
library(data.table)
library(tidyverse)

path <- "D:/Hospital_data/ProgresSQL/analysis_spmf_InputsOutputs/M2.0/"

all_met_rmsd02 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/all_met_rmsd02.rds")
all_met_rmsd02 <- all_met_rmsd02 [! Coded_med %in% c("", " "),]
all_met_rmsd02 <- all_met_rmsd02 [, Code02 := paste(Type_med, ":", Coded_med, sep =""), ]

all_met_rmsd02 <- all_met_rmsd02[, comdisn := .GRP, by = .(Code02)]

all_met_rmsd02 <- all_met_rmsd02[Code != "** Not yet coded"]

all_met_rmsd02 <- all_met_rmsd02 [ order(mr_no, refcode, refdesc, period)]
all_met_rmsd03 <- unique( all_met_rmsd02 [, c("mr_no", "refcode", "refdesc", "Code02", "Code", "period",
                                              "comdisn", "patient_gender", "baseage"), ])

all_met_rmsd04 <- all_met_rmsd03 [ refcode == "M2.0"]
all_met_rmsd040 <- all_met_rmsd04 [ ! (Code == "M2.0" & period >= 0) ]

all_met_rmsd05 <- all_met_rmsd040 [, .(combdis = paste(comdisn, collapse = " ", sep = " " )), 
                                   by = .(mr_no, refcode, refdesc, baseage)]


# create 1 line per patient
# -1 to seperate itemset and
# -2 to seperate sequence

all_met_rmsd06 <- all_met_rmsd05 [, .(combdis02 = paste(combdis, collapse = " -1 ", sep = " " )), 
                                  by = .(mr_no, refcode, refdesc, baseage)]
all_met_rmsd06 <- all_met_rmsd06 [, combdis02 := paste(combdis02, " -1 -2", sep = ""), ]

fwrite(x = all_met_rmsd06 [, c("combdis02"),], 
       col.names = FALSE,
       file = paste(path, "SPADE_M2.0_AfterMedunq.txt", sep="") )

disnum <- unique( all_met_rmsd02 [, c("Code02", "comdisn"),])
disnum <- disnum [, comdisn := as.numeric(comdisn),]

# Layout needed for the Associatio rules:
# FPGrowth_association_rules

all_met_rmsd06_arff <- all_met_rmsd05 [, .(combdis02 = paste(combdis, collapse = " ", sep = " " )), 
                                       by = .(mr_no, refcode, refdesc, baseage)]

fwrite(x = all_met_rmsd06_arff [, c("combdis02"),], 
       col.names = FALSE,
       file = paste(path, "ARFF_M2.0_AfterMedunq.txt", sep="") )

# Execute the java program command with Apriori algorithm

Frequent itemset mining:
  
  D:\Hospital_data\ProgresSQL\analysis_spmf

java -jar spmf-V2.35-VDate18NOV2018.jar run Apriori "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\M2.0\ARFF_M2.0_AfterMedunq.txt" "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\M2.0\oARFF_M2.0_AfterMedunq_01perc.txt" 1%
java -jar spmf-V2.35-VDate18NOV2018.jar run Apriori "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\M2.0\ARFF_M2.0_AfterMedunq.txt" "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\M2.0\oARFF_M2.0_AfterMedunq_02perc.txt" 2%
java -jar spmf-V2.35-VDate18NOV2018.jar run Apriori "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\M2.0\ARFF_M2.0_AfterMedunq.txt" "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\M2.0\oARFF_M2.0_AfterMedunq_03perc.txt" 3%
java -jar spmf-V2.35-VDate18NOV2018.jar run Apriori "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\M2.0\ARFF_M2.0_AfterMedunq.txt" "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\M2.0\oARFF_M2.0_AfterMedunq_04perc.txt" 4%
java -jar spmf-V2.35-VDate18NOV2018.jar run Apriori "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\M2.0\ARFF_M2.0_AfterMedunq.txt" "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\M2.0\oARFF_M2.0_AfterMedunq_05perc.txt" 5%
java -jar spmf-V2.35-VDate18NOV2018.jar run Apriori "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\M2.0\ARFF_M2.0_AfterMedunq.txt" "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\M2.0\oARFF_M2.0_AfterMedunq_06perc.txt" 6%
java -jar spmf-V2.35-VDate18NOV2018.jar run Apriori "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\M2.0\ARFF_M2.0_AfterMedunq.txt" "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\M2.0\oARFF_M2.0_AfterMedunq_07perc.txt" 7%
java -jar spmf-V2.35-VDate18NOV2018.jar run Apriori "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\M2.0\ARFF_M2.0_AfterMedunq.txt" "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\M2.0\oARFF_M2.0_AfterMedunq_08perc.txt" 8%
java -jar spmf-V2.35-VDate18NOV2018.jar run Apriori "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\M2.0\ARFF_M2.0_AfterMedunq.txt" "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\M2.0\oARFF_M2.0_AfterMedunq_09perc.txt" 9%
java -jar spmf-V2.35-VDate18NOV2018.jar run Apriori "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\M2.0\ARFF_M2.0_AfterMedunq.txt" "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\M2.0\oARFF_M2.0_AfterMedunq_10perc.txt" 10%
java -jar spmf-V2.35-VDate18NOV2018.jar run Apriori "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\M2.0\ARFF_M2.0_AfterMedunq.txt" "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\M2.0\oARFF_M2.0_AfterMedunq_11perc.txt" 11%
java -jar spmf-V2.35-VDate18NOV2018.jar run Apriori "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\M2.0\ARFF_M2.0_AfterMedunq.txt" "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\M2.0\oARFF_M2.0_AfterMedunq_12perc.txt" 12%
java -jar spmf-V2.35-VDate18NOV2018.jar run Apriori "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\M2.0\ARFF_M2.0_AfterMedunq.txt" "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\M2.0\oARFF_M2.0_AfterMedunq_13perc.txt" 13%
java -jar spmf-V2.35-VDate18NOV2018.jar run Apriori "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\M2.0\ARFF_M2.0_AfterMedunq.txt" "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\M2.0\oARFF_M2.0_AfterMedunq_14perc.txt" 14%
java -jar spmf-V2.35-VDate18NOV2018.jar run Apriori "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\M2.0\ARFF_M2.0_AfterMedunq.txt" "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\M2.0\oARFF_M2.0_AfterMedunq_15perc.txt" 15%
java -jar spmf-V2.35-VDate18NOV2018.jar run Apriori "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\M2.0\ARFF_M2.0_AfterMedunq.txt" "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\M2.0\oARFF_M2.0_AfterMedunq_16perc.txt" 16%
java -jar spmf-V2.35-VDate18NOV2018.jar run Apriori "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\M2.0\ARFF_M2.0_AfterMedunq.txt" "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\M2.0\oARFF_M2.0_AfterMedunq_17perc.txt" 17%
java -jar spmf-V2.35-VDate18NOV2018.jar run Apriori "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\M2.0\ARFF_M2.0_AfterMedunq.txt" "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\M2.0\oARFF_M2.0_AfterMedunq_18perc.txt" 18%
java -jar spmf-V2.35-VDate18NOV2018.jar run Apriori "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\M2.0\ARFF_M2.0_AfterMedunq.txt" "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\M2.0\oARFF_M2.0_AfterMedunq_19perc.txt" 19%
java -jar spmf-V2.35-VDate18NOV2018.jar run Apriori "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\M2.0\ARFF_M2.0_AfterMedunq.txt" "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\M2.0\oARFF_M2.0_AfterMedunq_20perc.txt" 20%
java -jar spmf-V2.35-VDate18NOV2018.jar run Apriori "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\M2.0\ARFF_M2.0_AfterMedunq.txt" "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\M2.0\oARFF_M2.0_AfterMedunq_21perc.txt" 21%
java -jar spmf-V2.35-VDate18NOV2018.jar run Apriori "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\M2.0\ARFF_M2.0_AfterMedunq.txt" "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\M2.0\oARFF_M2.0_AfterMedunq_22perc.txt" 22%
java -jar spmf-V2.35-VDate18NOV2018.jar run Apriori "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\M2.0\ARFF_M2.0_AfterMedunq.txt" "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\M2.0\oARFF_M2.0_AfterMedunq_23perc.txt" 23%
java -jar spmf-V2.35-VDate18NOV2018.jar run Apriori "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\M2.0\ARFF_M2.0_AfterMedunq.txt" "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\M2.0\oARFF_M2.0_AfterMedunq_24perc.txt" 24%
java -jar spmf-V2.35-VDate18NOV2018.jar run Apriori "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\M2.0\ARFF_M2.0_AfterMedunq.txt" "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\M2.0\oARFF_M2.0_AfterMedunq_25perc.txt" 25%
java -jar spmf-V2.35-VDate18NOV2018.jar run Apriori "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\M2.0\ARFF_M2.0_AfterMedunq.txt" "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\M2.0\oARFF_M2.0_AfterMedunq_26perc.txt" 26%
java -jar spmf-V2.35-VDate18NOV2018.jar run Apriori "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\M2.0\ARFF_M2.0_AfterMedunq.txt" "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\M2.0\oARFF_M2.0_AfterMedunq_27perc.txt" 27%
java -jar spmf-V2.35-VDate18NOV2018.jar run Apriori "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\M2.0\ARFF_M2.0_AfterMedunq.txt" "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\M2.0\oARFF_M2.0_AfterMedunq_28perc.txt" 28%
java -jar spmf-V2.35-VDate18NOV2018.jar run Apriori "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\M2.0\ARFF_M2.0_AfterMedunq.txt" "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\M2.0\oARFF_M2.0_AfterMedunq_29perc.txt" 29%
java -jar spmf-V2.35-VDate18NOV2018.jar run Apriori "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\M2.0\ARFF_M2.0_AfterMedunq.txt" "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\M2.0\oARFF_M2.0_AfterMedunq_30perc.txt" 30%
java -jar spmf-V2.35-VDate18NOV2018.jar run Apriori "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\M2.0\ARFF_M2.0_AfterMedunq.txt" "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\M2.0\oARFF_M2.0_AfterMedunq_31perc.txt" 31%
java -jar spmf-V2.35-VDate18NOV2018.jar run Apriori "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\M2.0\ARFF_M2.0_AfterMedunq.txt" "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\M2.0\oARFF_M2.0_AfterMedunq_32perc.txt" 32%
java -jar spmf-V2.35-VDate18NOV2018.jar run Apriori "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\M2.0\ARFF_M2.0_AfterMedunq.txt" "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\M2.0\oARFF_M2.0_AfterMedunq_33perc.txt" 33%


# /cygdrive/d/Hospital_data/ProgresSQL/analysis_spmf_InputsOutputs/M2.0
# Combine the files into 1 file 
# gawk '{print FILENAME "#", $0}' oARFF_M2.0_AfterMedunq_*perc.txt > oARFF_Apriori_M2.0_AfterMedunq.txt

out <- fread( paste(path, "oARFF_Apriori_M2.0_AfterMedunq.txt", sep=""), 
             sep ="!",
             header = FALSE)

out2 <- out [, c("var03", "var01", "var02") := tstrsplit(V1, "#"),]
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
out9 <- out9 [ order ( var03)]

fwrite(out9, file = paste(path, "oARFF_M2.0_AfterMedunq_formatted.csv", sep="") )
