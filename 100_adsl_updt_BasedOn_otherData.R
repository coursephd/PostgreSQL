library(Hmisc)
library(data.table)
library(stringi)
library(stringr)

sec001 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/sec001.rds")
sec001_1 <- sec001 [, (names(sec001) %in% c("mr_no", "patient_id", "subvis") | 
                         grepl("^sec", names(sec001)) ), with =FALSE]

lookup <- fread("D:/Hospital_data/ProgresSQL/analysis/lookup.csv", sep=",")
diab001_015 <- paste("\\b", lookup [sec001_var015_diab != "", c(sec001_var015_diab)], "\\b", collapse = "|", sep="")
htn001_015 <- paste("\\b", lookup [sec001_var015_htn != "", c(sec001_var015_htn)], "\\b", collapse = "|", sep="")
rmsd001_015 <- paste("\\b", lookup [sec001_var015_rmsd != "", c(sec001_var015_rmsd)], "\\b", collapse = "|", sep="")

sec001_10 <- sec001_1 [, `:=` ( temp01diab = ifelse (toupper(sec001_var008_Diabetes) %in% lookup$sec001_var008_Diabetes, 0, 1), 
                                temp01htn = ifelse (toupper(sec001_var009_Hypertension) %in% lookup$sec001_var009_Hypertension, 0, 1),
                                temp01renal = ifelse(toupper(`sec001_var011_Renal Diseases`) %in% lookup$`sec001_var011_Renal Diseases`, 0, 1),
                                t001_015_diab = str_detect(`sec001_var015_Chief Complaint with Onset & Duration`, diab001_015),
                                t001_015_htn = str_detect(`sec001_var015_Chief Complaint with Onset & Duration`, htn001_015), 
                                t001_015_rmsd = str_detect(`sec001_var015_Chief Complaint with Onset & Duration`, rmsd001_015) ), ]


sec001_10 [t001_015_diab ==TRUE, .(cnt = uniqueN(mr_no))]
sec001_10 [t001_015_htn ==TRUE, .(cnt = uniqueN(mr_no))]
sec001_10 [t001_015_rmsd ==TRUE, .(cnt = uniqueN(mr_no))]
