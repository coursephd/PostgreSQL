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
                                     "vis", "baseage", "age", "all_vis", "Type", "ndis", "Med02"), ])


med_01 <- unique ( py01 [, c("Med02"), ] )
med_01 <- med_01 [ order(Med02)]
med_01 <- med_01 [ , nmed := .I, ]
med_01 <- med_01 [ , tmed := paste("MED_", nmed, sep=""), ]

py01 <- merge(x = py01,
              y = med_01,
              by = c("Med02"))
py01 <- py01 [, main02 := paste("DIS_", main, sep=""),]


###############################################################
# Calculate median age for each disease and gender combination 
# Replace the missing age and create a 10 year freq count
###############################################################

py01 <- py01 [, median_age := median(age, na.rm=TRUE), by = .(main, patient_gender)]
py01 <- py01 [, age := ifelse(is.na(age), median_age, age), ]
py01 <- py01 [, agecut := cut(age, seq(from = 0, to = 100, by = 10)), ]

# There should be a better option
py011 <- copy(py01)
py012 <- copy(py01)

###################################################################
# Create all gender and all age -- grouping [All, 0-100]             [py011_1]
# Create gender and all age -- grouping [M, 0-100] and [F, 0-100]    [py012_1]
# Create diff gender and age grouping -- Existing data               [py013_1]
###################################################################

py011_1 <- py011 [, agecut := cut(age, seq(from = 0, to = 100, by = 100)), ]
py011_1 <- py011_1 [, patient_gender := "All", ]

py012_1 <- py012 [, agecut := cut(age, seq(from = 0, to = 100, by = 100)), ]
py013_1 <- py01

py014 <- rbind(py011_1, py012_1, py013_1)

###################################################################
# Combine the gender and age onto the diseases and medicine names
###################################################################
py014 <- py014 [, mr_no := paste(patient_gender, agecut, mr_no, sep="_"),]
py014 <- py014 [, main02 := paste(patient_gender, agecut, main02, sep="-"),]
py014 <- py014 [, tmed := paste(patient_gender, agecut, tmed, sep="-"),]

py02 <- py014 [, .(combdis = paste(unique(main02), collapse = " ", sep = " "), 
                  combmed = paste(unique(tmed), collapse = " ", sep = " ")) , by = .(mr_no, vis) ]
py02 <- py02 [, dis_med := paste(combdis, combmed, sep = " "),]
py03 <- py02 [, .(dis_med02 = paste (dis_med, collapse = " ", sep = "") ), by = .(mr_no)]

setnames(py03, "dis_med02", "comment_text")
#fwrite(py03, "D:\\Hospital_data\\ProgresSQL\\analysis\\501_deepLearn_dismed_subgrps.txt")

fwrite(py03 [, c("comment_text"), ], 
       "D:\\Hospital_data\\ProgresSQL\\analysis\\501_deepLearn_dismed_subgrps.txt",
       row.names = FALSE, 
       quote = FALSE)



rec01 <- unique(py014 [, c("main02", "main"), ])
rec01 <- merge (x = rec01, 
                y = code02, 
                by = c("main"))
rec01 <- rec01 [, meta := paste(distype, main02, maindesc, sep=":"),]

rec02 <- unique(py014 [, c("tmed", "Med02"), ])
rec02 <- rec02 [, meta := paste(tmed, Med02, sep=":"),]

rec03 <- rbind ( rec01 [, c("meta"), ], 
                 rec02 [, c("meta"), ])

fwrite(rec03, 
       "D:\\Hospital_data\\ProgresSQL\\analysis\\w2x_501_dismed_rprgm_metadata.tsv",
       row.names = FALSE, 
       quote = FALSE,
       sep = "\t")