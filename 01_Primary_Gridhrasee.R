library(Hmisc)
library(data.table)
library(stringi)
library(stringr)
library(sqldf)

all_met_rmsd <- readRDS("D:/Hospital_data/ProgresSQL/analysis/01adsl_met_rmsd.rds")

all_met_rmsd <- all_met_rmsd [, `:=` (vismon = round( cdur/30.4375, digits = 0))]
all_met_rmsd <- all_met_rmsd [Code !=""]

madhumeha <- unique( all_met_rmsd [ Code %in% c("V2.23") & diag_type == "P", c("mr_no"), ])
all_sub <- all_met_rmsd [ mr_no %in% madhumeha$mr_no]

# Analysis for the disease duration
dis_data <- unique(all_sub [, .(disstt = min(newdt0 ), disend = max( newdt0),
                                daystt = min(studyday ), dayend = max( studyday),
                                disdur = as.numeric(max(newdt0) - min(newdt0) + 1)), 
                            by = .(distype, mr_no, patient_gender, baseage, all_vis, all_ip, all_op, cdur, cstdt,cendt, Code, description)] )

dis_data <- dis_data [, cat := "Disease"]

# Analysis for the disease duration
#                             by = .(distype, mr_no, patient_gender, baseage, all_vis, all_ip, all_op, cdur, cstdt,cendt, medicine_name)] )

med_data <- unique(all_sub [, .(disstt = min(newdt0 ), disend = max( newdt0),
                                daystt = min(studyday ), dayend = max( studyday),
                                disdur = as.numeric(max(newdt0) - min(newdt0) + 1)), 
                            by = .(distype, mr_no, patient_gender, baseage, all_vis, all_ip, all_op, cdur, cstdt,cendt, Coded_med, Type_med)] )

setnames(med_data, "Coded_med", "description")
setnames(med_data, "Type_med", "Code")
med_data <- med_data [, cat := "Medicine"]

all_data <- rbind(dis_data, med_data, fill =TRUE)

fwrite(all_data, 
       "D:/Hospital_data/ProgresSQL/analysis/Primary_gridhrasee.csv")

#####################################################################################
# End of program
#####################################################################################
