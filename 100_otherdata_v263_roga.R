
##########################################################################################################
# This file takes Complete other data from D:/Hospital_data/ProgresSQL/analysis/complete_other_data.csv
# VrikkaRoga patients are subsetted from the above data and csv, rds files are created in
# the following folder: D:/Hospital_data/ProgresSQL/analysis_ckd
#
# Individual section wise csv / rds files are created: sec**
#
# The other CRF pages of data will not be very useful directly for analysis, but will be attempted
# with some amount of manual review
#
# Create for V2.63
# Initial date: 20-Aug-2021
#
##########################################################################################################



library(data.table)
library(tidyverse)
library(cumstats)


other <- fread("D:/Hospital_data/ProgresSQL/analysis/complete_other_data.csv")

##################################
# Subset for Metabolic RMSD data
##################################
all_met_rmsd <- readRDS("D:/Hospital_data/ProgresSQL/analysis_v263/01adsl_v263_roga.rds")

subpat <- unique(all_met_rmsd [, c("mr_no", "V263Roga", "all_vis",
                                   "city_name", "state_name", "dateofbirth", "country_name",
                                   "death_date")])

vispat <- unique(all_met_rmsd [, c("mr_no", "studyday", "patient_id", "newdt", "vis", "Type", "Code", "distype", "description",
                                   "all_ip", "all_op")])

# Only keep Vrikka Roga patients
base01_met_rmsd <- merge (x = other, #base01_other030,
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
                             c("mr_no", "patient_id", "V263Roga", "subvis",
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
  
  fwrite(file = paste0("D:/Hospital_data/ProgresSQL/analysis_v263/sec", 
                       jj, ".csv"),
         x = base01_met_rmsd_trn [,(names(base01_met_rmsd_trn) %in% 
                                      c("mr_no", "patient_id", "V263Roga", "subvis","city_name", 
                                        "state_name", "dateofbirth", "country_name", "death_date", "Type", "Code", 
                                        "distype", "description", "studyday", "patient_id", "newdt", "vis", "all_vis", 
                                        "all_ip", "all_op")  | 
                                      grepl(kk,names(base01_met_rmsd_trn)) ), with =FALSE]
  )
}


sections <- unique(sub$section_id)
for (ii in sections){
  
  jj <- str_pad(ii, 3, side = "left", pad = 0)
  kk <- paste0("^sec", jj, sep="")
  print(jj)
  print(kk)
  
  saveRDS(file = paste0("D:/Hospital_data/ProgresSQL/analysis_v263/sec", 
                        jj, ".rds"),
          object = base01_met_rmsd_trn [,(names(base01_met_rmsd_trn) %in% 
                                            c("mr_no", "patient_id", "V263Roga", "subvis","city_name", 
                                              "state_name", "dateofbirth", "country_name", "death_date", "Type", "Code", 
                                              "distype", "description", "studyday", "patient_id", "newdt", "vis", "all_vis", 
                                              "all_ip", "all_op")  | 
                                            grepl(kk,names(base01_met_rmsd_trn)) ), with =FALSE]
  )
}



library(readxl)

############################################
# Get the Drug classification information
############################################
drugs <- read_excel(path = "D:\\Hospital_data\\ProgresSQL\\analysis\\Medicine_names.xlsx-VInay.xlsx")
setnames(x=drugs, old=names(drugs), new=gsub(" ","",names(drugs)))
setnames(x=drugs, old=names(drugs), new=gsub("\r\n","",names(drugs)))


drugs2 <- unique( drugs [, c("medicine_name", "Subtype", "ClassicalProprietary", "ShamanaShodhana(Panchakarma)", "MetalbasedtreatmentsRasaoushadhi"),] )

all_met_rmsd02 <- merge (x = all_met_rmsd,
                         y = drugs2 ,
                         by = c("medicine_name"),
                         all.x = TRUE)

#####################################
# Merge single or multiple diseases
#####################################
patcat <- fread("D:\\Hospital_data\\ProgresSQL\\analysis\\105_trt_dis_unq_mult01vrikka_roga.csv")
patcat2 <- unique( patcat [, c("mr_no", "discat"),])

all_met_rmsd02 <- merge (x = all_met_rmsd02,
                         y = patcat2, 
                         by = c("mr_no"), 
                         all.x = TRUE )

base01_met_rmsd02 <- merge (x = base01_met_rmsd,
                            y = patcat2, 
                            by = c("mr_no"), 
                            all.x = TRUE )

# Write to a permenant file for temporary calculations
saveRDS (all_met_rmsd02, "D:/Hospital_data/ProgresSQL/analysis_v263/01adsl_v263_roga.rds")
saveRDS (base01_met_rmsd02, "D:/Hospital_data/ProgresSQL/analysis_v263/01adsl_v263_roga.rds")


# Single disease and cdur =1 only 1 and only visit
onedisvis <- all_met_rmsd02 [, .(n= uniqueN(mr_no)), by = .(discat, cdur)]

chk <- base01_met_rmsd02 [ ! is.na( option_remarks), .(n = uniqueN(mr_no) ), by = .(discat, trnvar) ]
chk_t <- dcast(data = chk,
               trnvar ~ discat,
               value.var = c("n"),
               fill ="")


# Create a dataset with patients and therapy type:

trt_type <- unique(all_met_rmsd02 [, c("mr_no", "studyday", "Code", "discat", "ShamanaShodhana(Panchakarma)"), ])


# dtable <- df[, fwrite(.SD, paste0("./output/"), Name, ".csv"), by = Name]
########################################################################################
# End of program
########################################################################################
