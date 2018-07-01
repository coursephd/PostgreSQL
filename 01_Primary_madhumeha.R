library(dplyr)
library(data.table)
library(fuzzyjoin)
library(stringr)
library(stringi)
library(stringdist)
library(quanteda)
library(tm)
library(tidyr)
library(sqldf)
library(ggplot2)

all_met_rmsd <- readRDS("D:/Hospital_data/ProgresSQL/analysis/01adsl_met_rmsd.rds")

all_met_rmsd <- all_met_rmsd [, `:=` (baseage = min(age)), by =.(mr_no)]
all_met_rmsd <- all_met_rmsd [, `:=` (vismon = round( cdur/30.4375, digits = 0))]
all_met_rmsd <- all_met_rmsd [Code !=""]

madhumeha <- unique( all_met_rmsd [ Code %in% c("M2.0") & diag_type == "P", c("mr_no"), ])
all_sub <- all_met_rmsd [ mr_no %in% madhumeha$mr_no]

# Analysis for the disease duration
# Analysis for the disease duration
dis_data <- unique(all_sub [, .(disstt = min(newdt0 ), disend = max( newdt0),
                                daystt = min(studyday ), dayend = max( studyday),
                                disdur = as.numeric(max(newdt0) - min(newdt0) + 1)), 
                            by = .(distype, mr_no, patient_gender, all_vis, all_ip, all_op, cdur, cstdt,cendt, Code, description)] )

dis_data <- dis_data [, cat := "Disease"]

# Analysis for the disease duration
med_data <- unique(all_sub [, .(disstt = min(newdt0 ), disend = max( newdt0),
                                daystt = min(studyday ), dayend = max( studyday),
                                disdur = as.numeric(max(newdt0) - min(newdt0) + 1)), 
                            by = .(distype, mr_no, patient_gender, all_vis, all_ip, all_op, cdur, cstdt,cendt, medicine_name)] )

setnames(med_data, "medicine_name", "description")
med_data <- med_data [, cat := "Medicine"]

all_data <- rbind(dis_data, med_data, fill =TRUE)

fwrite(all_data, 
       "D:/Hospital_data/ProgresSQL/analysis/Primary_madhumeha.csv")

#####################################################################################
# End of program
#####################################################################################


p <- ggplot(data=dis_data [mr_no == "MR000774"] ) + 
  geom_segment(aes(x=disstt, xend=disend, y=Code, yend=Code, group=mr_no), size=12)
p


setnames(dis_data, "mr_no", "id")
setnames(dis_data, "Code", "content")
setnames(dis_data, "disstt", "start")
setnames(dis_data, "disend", "end")

library(timevis)
timevis(data = dis_data,
        )

timevis(dis_data)
