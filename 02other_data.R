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


########################################################
# Need to consolidate variable names and combine 
# base01_other044
# base01_other022
# Create a counter variable for transposing
########################################################

# Create a counter variable for transpose
base01_other030 <- base01_other03 [, subvis := 1:.N, by = .(mr_no, patient_id, section_id, field_id, field_name)]

# Transpose the data as per CRF pages
base01_other030t <- dcast(data = base01_other030,
                          mr_no + patient_id + subvis + section_id + section_title ~ 
                            paste("var", str_pad(field_id, 3, side = "left", pad = 0), sep=""),
                          value.var = c("option_remarks"), 
                          subset = . (section_id == 85))


chk <- base01_other02 [, .(cnt = .N, 
                           unqpat = uniqueN(mr_no)), by = .(section_id)]


# Read the data
base01orig_other <- fread("D:/Hospital_data/ProgresSQL/data_chk/base10_other11_orig.csv")
base01orig_other02 <- base01orig_other [nchar(option_remarks)> 0]

base01orig_other02 <- merge (x = base01orig_other02,
                         y = section_master,
                         by = "section_id",
                         all.x = TRUE)

base01orig_other02 <- merge (x = base01orig_other02,
                         y = section_field_options,
                         by = c("section_id", "field_id"), # by = c("option_id", "field_id")
                         all.x = TRUE)

chk_orig <- base01orig_other02 [, .(cnt = .N, unqpat = uniqueN(mr_no)), by = .(section_id)]

base01_all <- dplyr::bind_rows(list(base01_other02, base01orig_other02), .id = 'source')

chk_all0 <- base01_all [, .(cnt = .N, unqpat = uniqueN(mr_no)), by = .(section_id)]
chk_all <- base01_all [, .(cnt = .N, unqpat = uniqueN(mr_no)), by = .(source, section_id)]
chk_all2 <- dcast (data = chk_all, 
                   section_id ~ source, 
                   value.var = c("cnt", "unqpat"))
chk_all3 <- merge (x = chk_all0, y = chk_all2, by = c("section_id"))

fwrite(chk_all3, "D:/Hospital_data/ProgresSQL/analysis/all_other_data_comparison.csv")
fwrite(base01_other02, "D:/Hospital_data/ProgresSQL/analysis/all_other_data.csv")
fwrite(section_master, "D:/Hospital_data/ProgresSQL/analysis/section_master.csv")


fwrite(base01_other02, "D:/Hospital_data/ProgresSQL/analysis/all_other_data_Using_section_field_options.csv")

base01_all01 <- unique ( base01_all [, c("mr_no", "section_id", "source"), with =FALSE])
base01_all01 <- base01_all01 [, val:=1]
base01_all01t <- dcast(data =base01_all01,
                       mr_no + section_id ~ source, 
                       value.var = "val", 
                       fill =0)

section_field_desc <- fread("D:/Hospital_data/ProgresSQL/data_chk/section_field_desc.csv")
patient_consultation_field_values <- fread("D:/Hospital_data/ProgresSQL/data_chk/patient_consultation_field_values.csv")
section_field_options02 <- section_field_options[, c("option_id", "field_id", "old_option_id", 
                                                     "display_order", "option_value"), with = FALSE]

section_field_options03 <- section_field_options02 [ field_id == 67]
  
sub <- base01_other030 [patient_id == "IP002102"]

counts <- base01_other02 [, cnt :=.N, by =.(mr_no, patient_id, section_id, field_id, option_id)]
