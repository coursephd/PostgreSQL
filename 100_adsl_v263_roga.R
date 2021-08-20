
######################################################
# Program name: 100_adsl_v263_roga.R
# Output files: 01adsl_v263_roga.csv, 01adsl_v263_roga.rds
# Create calculations using base01_ip and base01_op
# Initial date: 20-Aug-2021
######################################################

#C- Cancelled
#U - Condn. Unnecessary
#Y -Conducted
#N - Not Conducted
#P - Partially Conducted

library(data.table)
library(dplyr)
library(anytime)

# Get all the data IP, OP and Service

base01_ip <- fread("D:/Hospital_data/ProgresSQL/source/base01_ip.csv")
base01_op <- fread("D:/Hospital_data/ProgresSQL/source/base01_op.csv")
base01_ser <- fread("D:/Hospital_data/ProgresSQL/source/base01_ser.csv")
pat_diag_vis <- fread("D:/Hospital_data/ProgresSQL/source/pat_diag_vis.csv")

# Get the disease category list for MCSD and Metabolic
# This is replaced by cancer data with a subset for "Arbuda" below
#discat <- data.table( fread ("D:/Hospital_data/ProgresSQL/analysis/discategory.csv") )

# Get the medication and service list
med <- data.table( fread ("D:/Hospital_data/ProgresSQL/source/med.csv") )
ser <- data.table( fread ("D:/Hospital_data/ProgresSQL/source/services.csv") )

medall <- rbind(med, ser, fill = TRUE)
rm(med, ser)

########################################################
# Work on the services data
# get the date converted to numeric date
# get the minimum and maximum date for each visit
# get the frequency count for each type of service
########################################################

base01_ser0 <- base01_ser [,c("mr_no", "patient_id", "prescdate", "sercond_date", "cat_id", "conducted"), with =FALSE]
base01_ser0 <- base01_ser0 [, `:=` ( newdt = anydate(prescdate),
                                     serdt = anydate(sercond_date) )] [order(mr_no, newdt, patient_id)]

base01_ser01 <- base01_ser0[, .(serstdt = min(newdt), 
                                serendt = max(newdt),
                                freq = .N), by = .(mr_no, patient_id, cat_id, conducted)]

base01_ser01t <- dcast(data = base01_ser01,
                       mr_no + patient_id + cat_id + serstdt + serendt ~ conducted,
                       value.var = c("freq"),
                       fill = "")

base01_ser01t <- merge (x = base01_ser01t,
                        y = medall,
                        by.x = "cat_id",
                        by.y = "medicine_id",
                        all.x = TRUE)

base01_ser01t <- base01_ser01t [order(mr_no, serstdt, patient_id)]
base01_ser01t <- base01_ser01t [, newdt := serstdt]

l = list(IP = base01_ip, OP = base01_op)
base01_all <- rbindlist(l, idcol = "Type", use.names = TRUE, fill = TRUE)

base01_all <- base01_all [, `:=` ( newdt = anydate(prescdate) )] [order(mr_no, newdt, patient_id)]

#################################################
# create visit numbers and total number of visits
# Individual visits: merge the data on base01_all
# IP visits
# OP visits
# Total number of visits IP + OP
#################################################

vis <- unique ( rbind(base01_all [, c("mr_no", "patient_id", "newdt"), with =FALSE], 
                      base01_ser01t[, c("mr_no", "patient_id", "newdt"), with =FALSE], fill=TRUE ))

vis <- vis [, Type := substr(patient_id, 1, 2)] [order (mr_no, newdt, patient_id)]
vis <- vis [, `:=` (vis =1:.N, 
                    all_vis = max( seq_len(.N) ) ), by = .(mr_no)]
vis02 <- vis [, .(vistype =.N), by = .(mr_no, Type, all_vis)]
vis02t <- dcast(data = vis02, 
                mr_no +all_vis ~ paste("all_", tolower(Type), sep =""),
                value.var =c("vistype"),
                fill="")
vis03 <- merge (vis [, -c("all_vis")], vis02t, by = "mr_no")

#############################################
# Start and end date for each type OP and IP
# Start and end date for overall visit dates
#############################################
base01_all01 <- vis[, .(stdt = min(newdt), 
                        endt = max(newdt), 
                        dur = max(newdt) - min(newdt) + 1), by = .(mr_no, Type)]

base01_all01t <- dcast(data = base01_all01,
                       mr_no ~ Type,
                       value.var = c("stdt", "endt", "dur"),
                       fill = "")

#############################
# Start for the overall study
#############################
base01_all020 <- vis[, .(cstdt = min(newdt), 
                         cendt = max(newdt), 
                         cdur = max(newdt) - min(newdt) + 1), by = .(mr_no)]

#############################################
# Create one large dataset with all the dates
#############################################
dates_dur <- merge (x = base01_all020,
                    y = base01_all01t,
                    by = c("mr_no"),
                    all.x = TRUE)

vis03dates_dur <- merge (x = dates_dur,
                         y = vis03,
                         by = c("mr_no"),
                         all.x = TRUE)

vis03dates_dur <- vis03dates_dur [, studyday := newdt - cstdt + 1]

##################################################
# Merge the Medication information
# Merge the visit information and day calculations
# Merge this information on SERVICEs data as well
##################################################

base01_all01 <- merge (x = base01_all,
                       y = medall,
                       by.x = "cat_id",
                       by.y = "medicine_id",
                       all.x = TRUE)

base01_all011 <- merge (x = base01_all01,
                        y = vis03dates_dur [, -c("Type")],
                        by = c("mr_no", "patient_id", "newdt" ),
                        all.x = TRUE)

#################################################
# This should be moved after the VIS calculations
# Add the patient_info
#################################################
base01_ser02t <- merge (x = base01_ser01t,
                        y = vis03dates_dur,
                        by = c("mr_no", "patient_id", "newdt" ),
                        all.x = TRUE)

base01_ser02t <- merge (x = base01_ser02t,
                        y = pat_diag_vis,
                        by = c("mr_no", "patient_id"),
                        all.x = TRUE)

all <- rbind(base01_all011, base01_ser02t, fill =TRUE, use.names = TRUE)
all02 <- all [, -c("ippatient_id", "consult_id", "consultation_id" ,"patient_presc_id", 
                   "med_form_id", "op_medicine_pres_id", "doctor_id", "diagdate", 
                   "prescdate")] [order(mr_no, studyday, patient_id, newdt, vis, cat_id)]

#######################################################
# Calculations for
# Get the disease category list for Cancer patients
#######################################################

# Find patients with V2.63 and create a subset
# tolower(description) %like% "vrikka"
discat0 <- unique(all02 [icd_code == "V2.63", c("icd_code", "description", "mr_no"),] )
discat0 <- discat0 [, distype := "V263Roga",]
discat0 <- discat0 [, Code := icd_code,]
discat <- unique( discat0 [, -c("mr_no"),])

cancer <- unique(discat0 [, c("mr_no", "distype"), ])

tmpall <- merge (x = cancer,
                 y = all02,
                 by = c("mr_no"),
                 all.x = TRUE)

# create a dummy variable
tmpall <- tmpall[ ,val:=1]

subset2 <- tmpall [, c("mr_no", "distype", "val"), with =FALSE]
subset2 <- unique(subset2)

subset3 <- dcast (data = subset2,
                  fill =0,
                  mr_no ~ distype,
                  value.var="val")

all_met_rmsd <- merge (x = subset3,
                       y = all02,
                       by = "mr_no",
                       all.x = TRUE)

all_met_rmsd <- merge (x = discat,
                       y = all_met_rmsd,
                       all = TRUE,
                       by.x = c("Code", "description"),
                       by.y = c("icd_code", "description"))

all_met_rmsd$distype[is.na(all_met_rmsd$distype)] <- "OTHER"
all_met_rmsd <- all_met_rmsd [order(mr_no, studyday, patient_id, newdt, vis, cat_id)]

# Calculation of first RMSD or Metabolic disease date
minday <- all_met_rmsd[ distype != "OTHER", 
                        .(minday = min(studyday)), by =.(mr_no, distype)]
mindayt <- dcast (data = minday,
                  mr_no ~ paste("minday", distype, sep=""),
                  value.var="minday")
all_met_rmsd <- merge (all_met_rmsd, mindayt, by = "mr_no")

# Calculate the age variable for non-missing dates
all_met_rmsd <- all_met_rmsd [, `:=`( age = ifelse ( !is.na( anydate(dateofbirth)) , 
                                                     round( (anydate(newdt) - anydate(dateofbirth) + 1)/365.25, digits = 0 ), NA),
                                      newdt0 = anydate(newdt)), ]

# Add Indian rutus as new variables
# https://www.drikpanchang.com/seasons/season-tropical-timings.html?geoname-id=1277333&year=2010

rutus <- fread("D:/Hospital_data/ProgresSQL/analysis/rutus.csv")
rutus <- rutus [, `:=`(startdt = as.POSIXct( startdate, format="%d-%m-%Y"), 
                       enddt = as.POSIXct( enddate, format="%d-%m-%Y")) ]

rutus02 <- rutus[ , list(season = season, year = year,
                         newdt0 = anydate( seq(startdt, enddt, by = "day") )), by = 1:nrow(rutus)]

all_met_rmsd <- merge (x = all_met_rmsd,
                       y = rutus02 [, c("newdt0", "year", "season")],
                       by = c("newdt0"),
                       all.x = TRUE)

rm (base01_ip, base01_op, base01_ser, l)

all_met_rmsd <- all_met_rmsd [, `:=` (baseage = min(age)), by =.(mr_no)]

#?????????????????????????????????????????
# Section after this has not been executed
# as the names of the medicines are unknown
#?????????????????????????????????????????

#############################################
# Update the data by re-coded Medicine names
#############################################
lookup_medicine <- fread("D:/Hospital_data/ProgresSQL/analysis/lookup_medicine.txt", sep="|")

all_met_rmsd <- merge(x = all_met_rmsd,
                      y = lookup_medicine,
                      all.x = TRUE,
                      by.x = c("medicine_name"),
                      by.y = c("medicine_name")  )

fwrite(all_met_rmsd, "D:/Hospital_data/ProgresSQL/analysis_v263/01adsl_v263_roga.csv")
saveRDS (all_met_rmsd, "D:/Hospital_data/ProgresSQL/analysis_v263/01adsl_v263_roga.rds")

#######################################################
# Update the medicine type as Prasan's classification
#######################################################
library(openxlsx)
library(readxl)

sheet01 <- read_xlsx(path ="D:/Hospital_data/ProgresSQL/analysis/Medicine_names.xlsx", sheet = "Latest3217 records")
sheet02 <- read_xlsx(path ="D:/Hospital_data/ProgresSQL/analysis/Medicine_names.xlsx", sheet = "Reword")

sheet02 <- as.data.table (unique( sheet02 ) )

sheet_all <- merge (x = sheet01, 
                    y = sheet02, 
                    by = c("ShamanaShodhanaPanchakarma"))
setnames(sheet_all, "Reworded", "ayurtype")

all_met_rmsd <- readRDS("D:/Hospital_data/ProgresSQL/analysis_v263/01adsl_v263_roga.rds")

all_met_rmsd02 <- merge(x = all_met_rmsd,
                        y = sheet_all [ , c("medicine_name", "ClassicalProprietary", "ayurtype", "MetalbasedtreatmentsRasaoushadhi")],
                        by = c("medicine_name"),
                        all.x = TRUE)

fwrite(all_met_rmsd02, "D:/Hospital_data/ProgresSQL/analysis_v263/01adsl_v263_roga.csv")
saveRDS (all_met_rmsd02, "D:/Hospital_data/ProgresSQL/analysis_v263/01adsl_v263_roga.rds")

					  
dis_rutu <- all_met_rmsd [Code != "",  .(cnt = uniqueN(mr_no)), by = .(season, Code, description)] [order(season, -cnt, Code)]
dis_rutu_yr <- all_met_rmsd [Code != "",  .(cnt = uniqueN(mr_no)), by = .(year, season, Code, description)][order(year, season, -cnt, Code)]
dis_rutu_yr02 <- dcast(dis_rutu_yr,
                       season + Code + description ~ paste("yr", year, sep=""),
                       value.var = c("cnt"),
                       fill=" ")

#fwrite(dis_rutu, "D:/Hospital_data/ProgresSQL/analysis/dis_rutu.csv")
#fwrite(dis_rutu_yr02, "D:/Hospital_data/ProgresSQL/analysis/dis_rutu_yr.csv")
