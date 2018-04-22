#============================================================
drop table if exists patient_section_details, patient_section_values;
drop table if exists patient_section_details, patient_section_values, base10_other0;

create table patient_section_details as 
select mr_no, patient_id, section_id, section_detail_id, section_item_id, item_type  
from iaim.patient_section_details 
order by mr_no, patient_id;

create table patient_section_values as
select section_detail_id, field_id, option_id, option_remarks 
from iaim.patient_section_values
order by section_detail_id;

## Not working
/*
create table base10_other0 as
select patient_section_details.*, 
patient_section_values.field_id, patient_section_values.option_id, patient_section_values.option_remarks
from patient_section_details
full join patient_section_values on
patient_section_details.section_detail_id = patient_section_values.section_detail_id and
patient_section_details.section_item_id = patient_section_values.field_id and 
patient_section_details.section_id = patient_section_values.option_id;
*/

## Working: 
create table base10_other11 as
select a.*, 
b.field_id, b.option_id, b.option_remarks
from patient_section_details as a, patient_section_values as b where a.section_detail_id=b.section_detail_id ;

\copy base10_other11 TO 'd:/hospital_data/ProgresSQL/data_chk/base10_other11.csv' CSV HEADER DELIMITER ',';
\copy iaim.section_master TO 'd:/hospital_data/ProgresSQL/data_chk/section_master.csv' CSV HEADER DELIMITER ',';
\copy iaim.section_field_options TO 'd:/hospital_data/ProgresSQL/data_chk/section_field_options.csv' CSV HEADER DELIMITER ',';
\copy iaim.section_field_desc TO 'd:/hospital_data/ProgresSQL/data_chk/section_field_desc.csv' CSV HEADER DELIMITER ',';
\copy iaim.patient_consultation_field_values TO 'd:/hospital_data/ProgresSQL/data_chk/patient_consultation_field_values.csv' CSV HEADER DELIMITER ',';

###############################
Check the _orig dataset

create table patient_section_details_orig as 
select mr_no, patient_id, section_id, section_detail_id, section_item_id, item_type  
from iaim.patient_section_details 
order by mr_no, patient_id;

## Working: 
create table base10_other11_orig as
select a.*, 
b.field_id, b.option_id, b.option_remarks
from patient_section_details_orig as a, patient_section_values as b where a.section_detail_id=b.section_detail_id ;

\copy base10_other11_orig TO 'd:/hospital_data/ProgresSQL/data_chk/base10_other11_orig.csv' CSV HEADER DELIMITER ',';


#============================================================

library(data.table)
library(stringi)
library(stringr)

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
#base01_other022 <- unique ( base01_other02 [, c("mr_no", "patient_id", "section_id", "option_id", "field_id", "option_remarks", "section_title", "display_order", "option_value"), with =FALSE] )
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

#fwrite(base01_other030, "D:/Hospital_data/ProgresSQL/analysis/complete_other_data.csv")


##################################
# Subset for Metabolic RMSD data
##################################
all_met_rmsd <- readRDS("D:/Hospital_data/ProgresSQL/analysis/01adsl_met_rmsd.rds")

subpat <- unique(all_met_rmsd [, c("mr_no", "Metabolic", "RMSD", "combine", "all_vis",
                                   "city_name", "state_name", "dateofbirth", "country_name",
                                   "death_date")])

vispat <- unique(all_met_rmsd [, c("mr_no", "studyday", "patient_id", "newdt", "vis", "Type", "Code", "distype", "description",
                                   "all_ip", "all_op")])

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

# Transpose the data as per CRF pages
base01_met_rmsd_trn <- dcast(data = base01_met_rmsd,
                             mr_no + patient_id + subvis ~ trnvar,
                             value.var = c("option_remarks"))

# Add visit information and disease information:
base01_met_rmsd_trn <- merge (x = base01_met_rmsd_trn,
                          y = vispat,
                          by = c("mr_no", "patient_id"), 
                          all.x = TRUE)

# Add patient demog + visit + duration information
base01_met_rmsd_trn <- merge (x = base01_met_rmsd_trn,
                                y = subpat,
                                by = c("mr_no"), 
                                all.x = TRUE)

# Keep variables by section

df = base01_met_rmsd_trn[,(names(base01_met_rmsd_trn) %in% 
                             c("mr_no", "patient_id", "Metabolic", "RMSD", "combine", "subvis",
                               "city_name", "state_name", "dateofbirth", "country_name",
                               "death_date", "Type", "Code", "distype", "description",
                               "studyday", "patient_id", "newdt", "vis", "all_vis", "all_ip", "all_op")
                           | grepl("^sec004",names(base01_met_rmsd_trn)) ), with =FALSE]

sections <- unique(sub$section_id)
for (ii in sections){
  
  jj <- str_pad(ii, 3, side = "left", pad = 0)
  kk <- paste0("^sec", jj, sep="")
  print(jj)
  print(kk)
  
  fwrite(file = paste0("D:/Hospital_data/ProgresSQL/analysis/sec", 
                       jj, ".csv"),
         x = base01_met_rmsd_trn [,(names(base01_met_rmsd_trn) %in% 
                                      c("mr_no", "patient_id", "Metabolic", "RMSD", "combine", "subvis","city_name", 
                                        "state_name", "dateofbirth", "country_name", "death_date", "Type", "Code", 
                                        "distype", "description", "studyday", "patient_id", "newdt", "vis", "all_vis", 
                                        "all_ip", "all_op")  | 
                                      grepl(kk,names(base01_met_rmsd_trn)) ), with =FALSE]
  )
}


# dtable <- df[, fwrite(.SD, paste0("./output/"), Name, ".csv"), by = Name]
########################################################################################
# End of program
########################################################################################
