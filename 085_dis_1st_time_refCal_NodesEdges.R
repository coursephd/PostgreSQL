
library(data.table)
library(stringi)
library(stringr)
library(sqldf)

all_met_rmsd <- readRDS("D:/Hospital_data/ProgresSQL/analysis/01adsl_met_rmsd.rds")

unqdis <- unique( all_met_rmsd [, c("mr_no", "studyday", "Code", "description"),]) 
unqdis <- unqdis [!Code %in% c("" , " ")]
unqdis <- unqdis [, mindisday := min(studyday), by = .(mr_no, Code, description)]
unqdis <- unique( unqdis [, c("mr_no", "mindisday", "Code", "description"),])

setnames(unqdis, "Code", "refcode")
setnames(unqdis, "description", "refdesc")

###################################################
# Create this data with min refday for each disease
###################################################

all_met_rmsd02 <- merge(x = all_met_rmsd,
                        y = unqdis,
                        by = c("mr_no"),
                        allow.cartesian = TRUE)

all_met_rmsd02 <- all_met_rmsd02 [, c("mr_no", "Code", "description", "combine", "RMSD", "Metabolic",
                                      "newdt0", "Type_med", "Coded_med", "studyday", "mindisday",
                                      "refcode", "refdesc", "patient_gender", "age", "baseage", 
                                      "distype", "cdur"), ]

##############################################################
# Calculate reference day for each disease as before and after
# studyday and mindisday
##############################################################
all_met_rmsd02 <- all_met_rmsd02 [, refday := ifelse(studyday >= mindisday, 
                                                     studyday - mindisday + 1,
                                                     studyday - mindisday),]
all_met_rmsd02 <- all_met_rmsd02[, refmnyr := as.numeric( ceiling (refday / 30.4375) ), ]

period01 <- fread("D:/Hospital_data/ProgresSQL/analysis/lookup_1st_nodesedges.csv")
period02 <- period01[ , list(period = period, periodn = periodn,
                         refmnyr = seq(as.numeric(start), as.numeric(end)) ), by = 1:nrow(period01)]

all_met_rmsd02 <- merge (x = all_met_rmsd02,
                       y = period02 [, c("refmnyr", "period", "periodn")],
                       by = c("refmnyr"),
                       all.x = TRUE)

chk01 <- all_met_rmsd02 [, .(cnt = uniqueN(mr_no)), 
                         by = .(refcode, refdesc, period, periodn, Code, description )]
