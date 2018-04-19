
######################################################
# Create calculations using base01_ip and base01_op
######################################################

library(data.table)
library(dplyr)
library(anytime)

# Get all the data IP, OP and Service

base01_ip <- fread("D:/Hospital_data/ProgresSQL/source/base01_ip.csv")
base01_op <- fread("D:/Hospital_data/ProgresSQL/source/base01_op.csv")
base01_ser <- fread("D:/Hospital_data/ProgresSQL/source/base01_ser.csv")
pat_info <- fread("D:/Hospital_data/ProgresSQL/source/pat_info.csv")

setnames(pat_info, "mrno", "mr_no")

# Get the disease category list for MCSD and Metabolic
discat <- data.table( fread ("D:/Hospital_data/ProgresSQL/analysis/discategory.csv") )

# Get the medication and service list
med <- data.table( fread ("D:/Hospital_data/ProgresSQL/source/med.csv") )
ser <- data.table( fread ("D:/Hospital_data/ProgresSQL/source/services.csv") )

medall <- rbind(med, ser, fill = TRUE)

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

l = list(ip = base01_ip, op = base01_op)
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
                        y = vis03dates_dur [, -c("Type")],
                        by = c("mr_no", "patient_id", "newdt" ),
                        all.x = TRUE)

base01_ser02t <- merge (x = base01_ser02t,
                        y = pat_info,
                        by = c("mr_no", "patient_id", "newdt" ),
                        all.x = TRUE)

rm (base01_ip, base01_op, base01_ser)
