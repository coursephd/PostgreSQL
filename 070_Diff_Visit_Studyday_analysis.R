library(data.table)
library(stringi)
library(stringr)
library(sqldf)

all_met_rmsd <- readRDS("D:/Hospital_data/ProgresSQL/analysis/01adsl_met_rmsd.rds")

studyday <- unique( all_met_rmsd [, c("mr_no", "studyday", "vis"), ])
studyday[ , diff := studyday - shift(studyday), by = mr_no] 

fwrite(studyday, 
       "D:/Hospital_data/ProgresSQL/data_chk/datachk_diffvis.csv")


studyday_dis <- unique( all_met_rmsd [, c("mr_no", "studyday", "vis", "Code", "description"), ])
studyday_dis[ , diff := studyday - shift(studyday), by = .(mr_no, Code, description)] 

fwrite(studyday_dis, 
       "D:/Hospital_data/ProgresSQL/data_chk/datachk_diffvis_dis.csv")

