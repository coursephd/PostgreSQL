library(data.table)
library(tidyverse)
library(sqldf)

all_met_rmsd02 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/01adsl_met_rmsd.rds")
all_met_rmsd02 <- all_met_rmsd02 [, Code := ifelse (Code == " " | Code == "", "** Not yet coded", Code),]
all_met_rmsd02 <- all_met_rmsd02 [, description:= ifelse (description == "" | description ==" ", "** Not yet coded", description),]
all_met_rmsd02 <- all_met_rmsd02 [, Code02 := paste(distype, ":", Code, ":", description, sep =""), ]
all_met_rmsd02 <- all_met_rmsd02 [, Med02 := paste(Type_med, ":", Coded_med, sep =""), ]

code <- unique (all_met_rmsd02 [, c("Code02", "Code", "description", "distype"),])
code <- code [, pos := ifelse(str_count(Code, "\\.") >0,str_locate(Code, "\\."), 0 ), ]
code <- code [, `:=` (main = substr(Code, 1, pos-1),
                      second = substr(Code, pos+1, length(Code) ) ),]

code02 <- code [ second == "0", c("main", "description", "distype"),]
code <- code [ order(main)]
code02 <- code02 [, ndis := .I, by = .(main)]
setnames(code02, "description", "maindesc")

code03 <- merge (x = code,
                 y = code02 [, -c("distype"), ],
                 by = c("main") )

all_met_rmsd03 <- merge(x = all_met_rmsd02,
                        y = code03 [, c("Code02", "main", "maindesc", "ndis"), ],
                        by = c("Code02") )
all_met_rmsd03 <- all_met_rmsd03 [, maincode := paste(main, ":", maindesc, sep=""),]

py01 <- unique ( all_met_rmsd03 [, c("mr_no", "newdt0", "main", "patient_gender",
                                     "vis", "baseage", "all_vis", "Type", "ndis", "Med02"), ])


med_01 <- unique ( py01 [, c("Med02"), ] )
med_01 <- med_01 [ order(Med02)]
med_01 <- med_01 [ , nmed := .I, ]
med_01 <- med_01 [ , tmed := paste("MED_", nmed, sep=""), ]

py01 <- merge(x = py01,
              y = med_01,
              by = c("Med02"))
py01 <- py01 [, main02 := paste("DIS_", main, sep=""),]

py02 <- py01 [, .(combdis = paste(unique(main02), collapse = " ", sep = " "), 
                  combmed = paste(unique(tmed), collapse = " ", sep = " ")) , by = .(mr_no, vis, main02) ]
py02 <- py02 [, dis_med := paste(combdis, combmed, sep = " "),]
py03 <- py02 [, .(dis_med02 = paste (dis_med, collapse = " ", sep = "") ), by = .(mr_no)]

setnames(py03, "dis_med02", "comment_text")
#fwrite(py03, "D:\\Hospital_data\\ProgresSQL\\analysis\\500_deepLearn_dismed.txt")

fwrite(py03 [, c("comment_text"), ], 
       "D:\\Hospital_data\\ProgresSQL\\analysis\\500_deepLearn_dismed.txt",
       row.names = FALSE, 
       quote = FALSE)
