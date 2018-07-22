
library(Hmisc)
library(data.table)
library(stringi)
library(stringr)
library(sqldf)

all_met_rmsd <- readRDS("D:/Hospital_data/ProgresSQL/analysis/01adsl_met_rmsd.rds")

##################################
# Subset for Metabolic RMSD data
##################################
all_met_rmsd <- readRDS("D:/Hospital_data/ProgresSQL/analysis/01adsl_met_rmsd.rds")

subpat <- unique(all_met_rmsd [, c("mr_no", "Metabolic", "RMSD", "combine", "all_vis", "patient_gender", "baseage",
                                   "city_name", "state_name", "dateofbirth", "country_name",
                                   "death_date")])

vispat <- unique(all_met_rmsd [, c("mr_no", "studyday", "patient_id", "newdt", "vis", "Type", "Code", "distype", "description",
                                   "all_ip", "all_op")])

##################################################
# Get records per visit for Treatment / Procedure
# Med start date, end date non missing and 
# name non missing
##################################################

med_ip <- unique( na.omit( all_met_rmsd, cols = c("stdt_IP") ))
med_ip <- unique( med_ip [Type == "IP", c("mr_no", "vis", "studyday", "Metabolic", "RMSD", "combine", "all_vis", "patient_gender", "baseage"), ] )
med_ip <- med_ip [, cat := "Treatment - IP"]

med_op <- unique( na.omit( all_met_rmsd, cols = c("stdt_OP") ))
med_op <- unique( med_op [Type == "OP", c("mr_no", "vis", "studyday", "Metabolic", "RMSD", "combine", "all_vis", "patient_gender", "baseage"), ] )
med_op <- med_op [, cat := "Treatment - OP"]

ser <- unique( na.omit( all_met_rmsd, cols = c("serstdt") ))
ser <- unique( ser [, c("mr_no", "vis", "studyday", "Metabolic", "RMSD", "combine", "all_vis", "patient_gender", "baseage"), ] )
ser <- ser [, cat := "Treatment - Procedure"]

dis <- unique( all_met_rmsd [Code != " " | description != " ", c("mr_no", "vis", "studyday", "Metabolic", "RMSD", "combine", "all_vis", "patient_gender", "baseage"), ] )
dis <- dis [, cat := "Disease"]

catall <- rbind(med_ip, med_op, ser, dis, fill = TRUE)

# Read the data
base01_other <- fread("D:/Hospital_data/ProgresSQL/data_chk/base10_other11.csv")
base01_other02 <- base01_other [nchar(option_remarks)> 0]

# CRF names
section_master <- fread("D:/Hospital_data/ProgresSQL/data_chk/section_master.csv")
section_master <- section_master[, c("section_id", "section_title"), with = FALSE]

base01_other02 <- merge (x = base01_other02,
                         y = section_master,
                         by = "section_id",
                         all.x = TRUE)

# variable names
section_field_options <- fread("D:/Hospital_data/ProgresSQL/data_chk/section_field_options.csv")

base01_other022 <- merge (x = base01_other02 [ option_id >= 0],
                          y = section_field_options ,
                          by = c("option_id", "field_id"), #by = c("section_id", "field_id"), 
                          all.x = TRUE)

# Keep Unique records
base01_other022 <- unique ( base01_other022 [, c("mr_no", "patient_id", "section_id",  "field_id", "option_remarks", "section_title", "display_order", "option_value"), with =FALSE] )

# Sort the data by patient and visits
base01_other022 <- base01_other022 [ order(mr_no, patient_id, section_id, field_id, display_order)]

section_field_desc <- fread("D:/Hospital_data/ProgresSQL/data_chk/section_field_desc.csv")
section_field_desc <- section_field_desc[, c("section_id", "field_id", "display_order", "field_name", "no_of_lines"), with = FALSE]

base01_other044 <- merge (x = base01_other02 [ option_id < 0],
                          y = section_field_desc,
                          by = c("section_id", "field_id"), 
                          all.x = TRUE)
base01_other044 <- unique ( base01_other044 [, c("mr_no", "patient_id", "section_id", "option_id", "field_id", "option_remarks", "section_title", "display_order", "field_name", "no_of_lines"), with =FALSE] )

# Sort the data by patient and visits
base01_other044 <- base01_other044 [ order(mr_no, patient_id, section_id, field_id, display_order)]
setnames(base01_other044, "field_name", "option_value")

base01_all <- rbind(base01_other022, base01_other044, fill =TRUE)

########################################################
# Need to consolidate variable names and combine 
# base01_other044
# base01_other022
# Create a counter variable for transposing
########################################################

# Create a counter variable for transpose
base01_other030 <- base01_all [, subvis := 1:.N, by = .(mr_no, patient_id, section_id, field_id, option_value)]


# Only keep Metabolic and RMSD patients
base01_met_rmsd <- merge (x = base01_other030,
                          y = subpat [,c("mr_no")],
                          by = c("mr_no"), 
                          all.y = TRUE)

sub <- unique( base01_met_rmsd [, c("section_id", "section_title", 
                                    "field_id", "display_order", "option_value")] ) [order(section_id, field_id, display_order, option_value)]
sub <- sub [, varnum:=seq_len(.N), by =.(section_id)]
sub <- sub [, trnvar := paste("sec", str_pad(section_id, 3, side = "left", pad = 0), 
                              "_var", str_pad(varnum, 3, side = "left", pad = 0),
                              "_", option_value, sep="" )]

base01_met_rmsd <- merge (x = base01_met_rmsd,
                          y = sub,
                          by = c("section_id", "section_title", 
                                 "field_id", "display_order", "option_value"), 
                          all.x = TRUE)

base01_met_rmsd02 <- unique( base01_met_rmsd [option_remarks != " ", c("mr_no", "option_value", "patient_id", "trnvar")] )

# Add visit information and disease information:
base01_met_rmsd02 <- merge (x = base01_met_rmsd02,
                              y = unique( vispat [, c("mr_no", "patient_id", "vis", "studyday")] ),
                              by = c("mr_no", "patient_id"), 
                              all.x = TRUE, 
                            allow.cartesian = TRUE)

# Add patient demog + visit + duration information
base01_met_rmsd02 <- merge (x = base01_met_rmsd02,
                              y = subpat,
                              by = c("mr_no"), 
                              all.x = TRUE)

base01_met_rmsd02 <- unique( base01_met_rmsd02 )

# variable names
types <- fread("D:/Hospital_data/ProgresSQL/analysis/lookup_03types.csv")

base01_met_rmsd02 <- merge (x = base01_met_rmsd02,
                            y = types [, c("trnvar", "cat")],
                            by = c("trnvar"), 
                            all.x = TRUE)

base01_met_rmsd03 <- unique( base01_met_rmsd02 [ , -c("option_value", "patient_id", "trnvar" )])

catall02 <- rbind(catall, base01_met_rmsd03, fill = TRUE)
catall02 <- catall02 [, val :=1]

fwrite(catall02, 
       "D:/Hospital_data/ProgresSQL/analysis/03_typesOfassessent.csv")
