
##############################################################################
# Use the following SQL tables
# \copy base01_ip TO 'd:/hospital_data/ProgresSQL/source/base01_ip.csv' CSV HEADER DELIMITER ',';
# \copy base01_op TO 'd:/hospital_data/ProgresSQL/source/base01_op.csv' CSV HEADER DELIMITER ',';
# \copy base01_ser TO 'd:/hospital_data/ProgresSQL/source/base01_ser.csv' CSV HEADER DELIMITER ',';
# \copy iaim.services TO 'd:/hospital_data/ProgresSQL/source/services.csv' CSV HEADER DELIMITER ',';
# \copy iaim.services TO 'd:/hospital_data/ProgresSQL/source/med.csv' CSV HEADER DELIMITER ',';
##############################################################################


library(data.table)

# Get all the data IP, OP and Service

base01_ip <- fread("D:/Hospital_data/ProgresSQL/source/base01_ip.csv")
base01_op <- fread("D:/Hospital_data/ProgresSQL/source/base01_op.csv")
base01_ser <- fread("D:/Hospital_data/ProgresSQL/source/base01_ser.csv")

base01_all <- rbind(base01_ip, base01_op, base01_ser)
base01_all <- base01_all[order(mr_no, patient_id)]

rm (base01_ip, base01_op, base01_ser)

# Get the disease category list for MCSD and Metabolic
discat <- data.table( fread ("D:/Hospital_data/ProgresSQL/analysis/discategory.csv") )

# Get the medication and service list
med <- data.table( fread ("D:/Hospital_data/ProgresSQL/source/med.csv") )
ser <- data.table( fread ("D:/Hospital_data/ProgresSQL/source/services.csv") )

setnames(ser, "service_id", "medicine_id")
setnames(ser, "service_name", "medicine_name")

medall <- rbind(med, ser)

base01_all01 <- merge (x = base01_all,
                 y = medall,
                 by.x = "cat_id",
                 by.y = "medicine_id",
                 all.x = TRUE)

base01_all011 <- merge (x = discat[, -c("Description"), with =FALSE],
                 y = base01_all01,
                 by.x = "Code",
                 by.y = "icd_code")

# create a dummy variable
base01_all011 <- base01_all011[ ,val:=1]

subset2 <- base01_all011 [, c("mr_no", "distype", "val"), with =FALSE]
subset2 <- unique(subset2)

subset3 <- dcast (data = subset2,
                  fill =0,
                  mr_no ~ distype,
                  value.var="val")

# Create an indicator variable to determine
# Both Metabolic and RMSD = 99
# Only Metabolic = 1
# Only RMSD = 2

subset3 <- subset3 [Metabolic == 1 & RMSD == 1, combine := "Metabolic and RMSD"]
subset3 <- subset3 [Metabolic == 1 & RMSD == 0, combine := "Metabolic"]
subset3 <- subset3 [Metabolic == 0 & RMSD == 1, combine := "RMSD"]

base01_all012 <- merge (x = subset3,
                       y = base01_all01,
                       by = "mr_no",
                       all.x = TRUE)
                       

fwrite(base01_all, "D:/Hospital_data/ProgresSQL/analysis/base01_all.csv")
fwrite(base01_all012, "D:/Hospital_data/ProgresSQL/analysis/base01_met_rmsd.csv")
