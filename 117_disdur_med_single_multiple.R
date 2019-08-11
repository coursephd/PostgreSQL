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


dur02 <- all_met_rmsd03 [, .(maxdurDisMed = sum(durstd, na.rm = TRUE),
                             unqPatDisMed = uniqueN(mr_no)), by = .(Code02, Med02)]
dur03 <- all_met_rmsd03 [, .(maxdurMed = sum(durstd, na.rm = TRUE),
                             unqPatMed = uniqueN(mr_no)), by = .(Med02)]
dur04 <- merge (x = dur02,
                y = dur03,
                by = c("Med02"))


dur02disc <- all_met_rmsd03 [, .(maxdurDisMedcat = sum(durstd, na.rm = TRUE),
                             unqPatDisMedcat = uniqueN(mr_no)), by = .(discat, Code02, Med02)]
dur03disc <- all_met_rmsd03 [, .(maxdurMedcat = sum(durstd, na.rm = TRUE),
                             unqPatMedcat = uniqueN(mr_no)), by = .(discat, Med02)]

dur04disc <- merge (x = dur02disc,
                    y = dur03disc,
                    by = c("discat", "Med02"))

dur04disc_t <- dcast(data = dur04disc,
                     Med02 + Code02 ~ discat,
                     value.var = c("maxdurDisMedcat", "unqPatDisMedcat", "maxdurMedcat", "unqPatMedcat"),
                     fill= "0")

dur05 <- merge(x = dur04,
               y = dur04disc_t,
               by = c("Code02", "Med02"))

dur02ayur <- all_met_rmsd03 [, .(maxdurDisMedcatayur = sum(durstd, na.rm = TRUE),
                                 unqPatDisMedcatayur = uniqueN(mr_no)), by = .(discat, ayurtype, Code02, Med02)]
dur03ayur <- all_met_rmsd03 [, .(maxdurMedcatayur = sum(durstd, na.rm = TRUE),
                                 unqPatMedcatayur = uniqueN(mr_no)), by = .(discat, ayurtype, Med02)]

dur04ayur <- merge (x = dur02ayur,
                    y = dur03ayur,
                    by = c("discat", "ayurtype", "Med02"))

dur04ayur_t <- dcast(data = dur04ayur,
                     ayurtype + Med02 + Code02 ~ discat,
                     value.var = c("maxdurDisMedcatayur", "unqPatDisMedcatayur", "maxdurMedcatayur", "unqPatMedcatayur"),
                     fill= "0")

fwrite(dur05, "D:/Hospital_data/ProgresSQL/analysis/117_disdur_med_singl_multiple.csv")

############################################################
# End of program
############################################################
