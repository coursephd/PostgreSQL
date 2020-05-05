########################################################################
# Medicine duration 
# Medicine duration by disease
# Summary statistics should show the most frequently used medicines
# Indirect relationship builiding
# More usage stronger the relationship
# Less usage may be no relationship or rare usage
########################################################################

library(data.table)
library(tidyverse)
library(sqldf)

all_met_rmsd <- readRDS("D:/Hospital_data/ProgresSQL/analysis/01adsl_met_rmsd.rds")
all_met_rmsd <- all_met_rmsd [, Code := ifelse (Code == " " | Code == "", "** Not yet coded", Code),]
all_met_rmsd <- all_met_rmsd [, description:= ifelse (description == "" | description ==" ", "** Not yet coded", description),]
all_met_rmsd <- all_met_rmsd [, Code02 := paste(distype, ":", Code, ":", description, sep =""), ]
all_met_rmsd <- all_met_rmsd [, Med02 := paste(Type_med, ":", Coded_med, sep =""), ]


med01 <- unique( all_met_rmsd [Med02 != "NA:NA" , 
                               c("mr_no", "Med02", "Type_med", "Coded_med", "studyday",
                                 "frequency", "duration", "duration_units", "cat_id",
                                 "patient_gender"),])
med01 <- med01 [ duration > 0]
med01 <- med01 [, duration := as.numeric(duration),]
med01 <- med01 [, numdays := case_when( duration_units == "D" ~ duration, 
                                        duration_units == "W" ~ duration * 7,
                                        duration_units == "M" ~ duration * 30), ]

# Create 1 record per patient per medication with sum of durations
med011 <- med01 [, .(numdays = sum(numdays)), by =.(mr_no, Med02, Type_med, Coded_med, patient_gender)]
med02 <- med011 [, .(n=uniqueN(mr_no), 
                     mean = round( mean(numdays, na.rm = TRUE), digits =1),
                     median= round( median(numdays, na.rm = TRUE), digits =2),
                     SD = round( sd(numdays, na.rm = TRUE), digits =2),
                     min = round( min(numdays, na.rm = TRUE), digits =0),
                     max = round( max(numdays, na.rm = TRUE), digits =0),
                     sum = round( sum(numdays, na.rm = TRUE), digits =0)),
                 by = .(Type_med)]


med02_med <- med011 [, .(n=uniqueN(mr_no), 
                         mean = round( mean(numdays, na.rm = TRUE), digits =1),
                         median= round( median(numdays, na.rm = TRUE), digits =2),
                         SD = round( sd(numdays, na.rm = TRUE), digits =2),
                         min = round( min(numdays, na.rm = TRUE), digits =0),
                         max = round( max(numdays, na.rm = TRUE), digits =0),
                         sum = round( sum(numdays, na.rm = TRUE), digits =0)),
                     by = .(Type_med, Med02)]



dismed01 <- unique( all_met_rmsd [Med02 != "NA:NA" , 
                                  c("mr_no", "Med02", "Type_med", "Coded_med", "studyday",
                                    "Code02",
                                    "frequency", "duration", "duration_units", "cat_id",
                                    "patient_gender"),])
dismed01 <- dismed01 [ duration > 0]
dismed01 <- dismed01 [, duration := as.numeric(duration),]
dismed01 <- dismed01 [, numdays := case_when( duration_units == "D" ~ duration, 
                                              duration_units == "W" ~ duration * 7,
                                              duration_units == "M" ~ duration * 30), ]

# Create 1 record per patient per medication with sum of durations
dismed011 <- dismed01 [, .(numdays = sum(numdays)), by =.(mr_no, Code02, Med02, Type_med, Coded_med, patient_gender)]
dismed011 <- dismed01 [, totpatdis := uniqueN(mr_no), by =.(Code02)]
dismed011 <- dismed01 [, totpatmed := uniqueN(mr_no), by =.(Med02)]
dismed02 <- dismed011 [, .(n=uniqueN(mr_no), 
                           mean = round( mean(numdays, na.rm = TRUE), digits =1),
                           median= round( median(numdays, na.rm = TRUE), digits =2),
                           SD = round( sd(numdays, na.rm = TRUE), digits =2),
                           min = round( min(numdays, na.rm = TRUE), digits =0),
                           max = round( max(numdays, na.rm = TRUE), digits =0),
                           sum = round( sum(numdays, na.rm = TRUE), digits =0)),
                       by = .(Type_med, Code02, totpat, totpatmed)]


dismed02_med <- dismed011 [, .(n=uniqueN(mr_no), 
                               mean = round( mean(numdays, na.rm = TRUE), digits =1),
                               median= round( median(numdays, na.rm = TRUE), digits =2),
                               SD = round( sd(numdays, na.rm = TRUE), digits =2),
                               min = round( min(numdays, na.rm = TRUE), digits =0),
                               max = round( max(numdays, na.rm = TRUE), digits =0),
                               sum = round( sum(numdays, na.rm = TRUE), digits =0)),
                           by = .(Code02, totpat, Type_med, Med02, totpatmed)]

##################################################################################################
