library(data.table)
library(stringi)
library(stringr)
library(sqldf)

all_met_rmsd <- readRDS("D:/Hospital_data/ProgresSQL/analysis/01adsl_met_rmsd.rds")

studyday_med <- unique( all_met_rmsd [, c("mr_no", "studyday", "vis", "medicine_name"), ])
studyday_med[ , diff := studyday - shift(studyday), by = .(mr_no, medicine_name)] 

fwrite(studyday_med, 
       "D:/Hospital_data/ProgresSQL/data_chk/datachk_diffvis_med.csv")

