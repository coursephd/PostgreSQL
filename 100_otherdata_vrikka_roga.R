
##########################################################################################################
# This file takes Complete other data from D:/Hospital_data/ProgresSQL/analysis/complete_other_data.csv
# VrikkaRoga patients are subsetted from the above data and csv, rds files are created in
# the following folder: D:/Hospital_data/ProgresSQL/analysis_ckd
#
# Individual section wise csv / rds files are created: sec**
#
# The other CRF pages of data will not be very useful directly for analysis, but will be attempted
# with some amount of manual review
##########################################################################################################



library(data.table)
library(tidyverse)
library(cumstats)


other <- fread("D:/Hospital_data/ProgresSQL/analysis/complete_other_data.csv")

##################################
# Subset for Metabolic RMSD data
##################################
all_met_rmsd <- readRDS("D:/Hospital_data/ProgresSQL/analysis/01adsl_vrikka.rds")

subpat <- unique(all_met_rmsd [, c("mr_no", "VrikkaRoga", "all_vis",
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
                             c("mr_no", "patient_id", "VrikkaRoga", "subvis",
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
  
  fwrite(file = paste0("D:/Hospital_data/ProgresSQL/analysis_ckd/sec", 
                       jj, ".csv"),
         x = base01_met_rmsd_trn [,(names(base01_met_rmsd_trn) %in% 
                                      c("mr_no", "patient_id", "VrikkaRoga", "subvis","city_name", 
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
  
  saveRDS(file = paste0("D:/Hospital_data/ProgresSQL/analysis_ckd/sec", 
                        jj, ".rds"),
          object = base01_met_rmsd_trn [,(names(base01_met_rmsd_trn) %in% 
                                            c("mr_no", "patient_id", "VrikkaRoga", "subvis","city_name", 
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
