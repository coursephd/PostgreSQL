####################################################################
# Pgm name: 117_disdur_med_single_multiple_bfr_aftr.R
# This uses data from
# (1) 105_trt_dis_unq_mult01 program
# (2) 01adsl_met_rmsd.rds
# (3) all_met_rmsd02.rds: from 085_dis_1st_time_refCal_NodesEdges.R
# (4) ayur classification
####################################################################

library(data.table)
library(tidyverse)
library(sqldf)
library(openxlsx)
library(readxl)

###################################################
# Get the data from 105_trt_dis_unq_mult01 program
###################################################
mult <- fread("D:/Hospital_data/ProgresSQL/analysis/105_trt_dis_unq_mult01.csv")
mult0 <- unique( mult [, c("mr_no", "discat"),])

#######################################################
# Update the medicine type as Prasan's classification
#######################################################
sheet01 <- read_xlsx(path ="D:/Hospital_data/ProgresSQL/analysis/Medicine_names.xlsx", sheet = "Latest3217 records")
sheet02 <- read_xlsx(path ="D:/Hospital_data/ProgresSQL/analysis/Medicine_names.xlsx", sheet = "Reword")

sheet02 <- as.data.table (unique( sheet02 ) )

sheet_all <- merge (x = sheet01, 
                    y = sheet02, 
                    by = c("ShamanaShodhanaPanchakarma"))
setnames(sheet_all, "Reworded", "ayurtype")

#############################
# Get the patient level data
#############################
all_met_rmsd02 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/01adsl_met_rmsd.rds")
all_met_rmsd02 <- all_met_rmsd02 [, Code := ifelse (Code == " " | Code == "", "** Not yet coded", Code),]
all_met_rmsd02 <- all_met_rmsd02 [, description:= ifelse (description == "" | description ==" ", "** Not yet coded", description),]
all_met_rmsd02 <- all_met_rmsd02 [, Code02 := paste(distype, ":", Code, ":", description, sep =""), ]
all_met_rmsd02 <- all_met_rmsd02 [, Med02 := paste(Type_med, ":", Coded_med, sep =""), ]

all_met_rmsd02 <- merge(x = all_met_rmsd02,
                        y = sheet_all [ , c("medicine_name", "ClassicalProprietary", "ayurtype", "MetalbasedtreatmentsRasaoushadhi")],
                        by = c("medicine_name"),
                        all.x = TRUE)

all_met_rmsd03 <- unique(all_met_rmsd02 [, c("mr_no", "studyday", "vis", "all_vis", "cdur", 
                                             "duration", "duration_units", 
                                             "ClassicalProprietary", "ayurtype", "MetalbasedtreatmentsRasaoushadhi",
                                             "Type_med", "Coded_med", "Med02", "Code02")] )

all_met_rmsd03 <- all_met_rmsd03 [, duration := as.numeric(duration),]

###############################################
# Get the duration for each prescribed medicine
###############################################
all_met_rmsd03 <- sqldf("select *,
                        case
                        When duration_units == 'D' then duration
                        When duration_units == 'M' then duration * 30
                        When duration_units == 'W' then duration * 7
                        end as durstd
                        from all_met_rmsd03"
                        , method = "name__class")

all_met_rmsd03 <- data.table(all_met_rmsd03)
all_met_rmsd03 <- merge (x = all_met_rmsd03,
                         y = mult0,
                         by = c("mr_no"))

######################################################################
# Get the before and after information, but that data does not have
# information related to the duration and duration unit
# Merge that information onto the dataset to calculate
######################################################################

ref02 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/all_met_rmsd02.rds")
ref02 <- ref02 [, Code := ifelse (Code == " " | Code == "", "** Not yet coded", Code),]
ref02 <- ref02 [, description:= ifelse (description == "" | description ==" ", "** Not yet coded", description),]
ref02 <- ref02 [, Code02 := paste(distype, ":", Code, ":", description, sep =""), ]
ref02 <- ref02 [, Med02 := paste(Type_med, ":", Coded_med, sep =""), ]

all_met_rmsd04 <- merge (x = all_met_rmsd03,
                         y = ref02,
                         by = c("mr_no", "Code02", "Med02", "Type_med", "Coded_med", "studyday", "cdur"))

fwrite(all_met_rmsd04, "D:/Hospital_data/ProgresSQL/analysis/117_disdur_med_single_multiple_bfr_aftr.csv")
