library(data.table)
library(stringi)
library(stringr)
library(sqldf)

all_met_rmsd <- readRDS("D:/Hospital_data/ProgresSQL/analysis/01adsl_met_rmsd.rds")
meds <- unique( all_met_rmsd [substr(cat_id, 1, 3) != "SER", 
                              c("mr_no", "medicine_name", "studyday", "remarks", "frequency", 
                               "duration", "duration_units", "Coded_med", "Type_med", 
                               "quantity", "patient_id", "cat_id")] )

meds0 <- meds [nchar(medicine_name) > 0 & 
                 nchar(duration) > 0 & nchar(duration_units) > 0] 

meds0 <- meds0 [order(mr_no, studyday, as.numeric(cat_id) )]  
meds0 <- meds0 [, minday := min(studyday), by = .(mr_no)]

time <- unique(meds0 [, c("mr_no", "studyday")] ) 
time <- time [order(mr_no, studyday)]
time <- time [, grpday := 1:.N, by = .(mr_no)]
time <- time [, grpmaxday := max(grpday), by = .(mr_no)]

meds0 <- merge (x = meds0, y = time, by = c("mr_no", "studyday") )

cum <- meds0 [mr_no == "MR000016"][order(mr_no, as.numeric(cat_id), studyday, grpday)]
cum01 <- meds0 [, presc := 1:.N, by = .(mr_no, cat_id)]
cum01 <- cum01 [order(mr_no, studyday, minday, as.numeric(cat_id), presc )]  
cum01 <- cum01 [, grpall := 1:.N, by = .(mr_no)]

tmp <- cum01 [, c("mr_no", "studyday", "minday","cat_id", "presc", "medicine_name", "grpday", "grpmaxday", "grpall")]

cum02 <- tmp [, newold := ifelse(presc == 1 & grpday > 1, "new", "old"), by =.(mr_no)]

#############################################################################
# Duplicate the medication and see which medications are given multiple times
#############################################################################

cum03 <- cum02 [, (list( newday = (grpday: grpmaxday) ) ), 
                by = .(mr_no, cat_id, presc, medicine_name, studyday, grpday, grpmaxday, newold) ]

tmp02 <- cum03 [mr_no == "MR000016"]
