library(data.table)
library(stringi)
library(stringr)
library(sqldf)

all_met_rmsd <- readRDS("D:/Hospital_data/ProgresSQL/analysis/01adsl_met_rmsd.rds")

madhumeha <- unique( all_met_rmsd [ Code %in% c("M2.0") & diag_type == "P", c("mr_no"), ])
all_sub <- all_met_rmsd [ mr_no %in% madhumeha$mr_no]

all_sub <- all_sub [, Code := str_replace_all(Code, "\\.", "_")]
all_sub_med <- unique(all_sub [patient_gender != "" & Code != "", 
         c("mr_no", "Code", "description","distype", "patient_gender", "medicine_name"), ])
all_sub_med <-  all_sub_med [, Code :=  paste("[", trimws(Code), "]", sep="") ]

cnt<- unique( all_sub [patient_gender != "" & Code != "", 
                            c("mr_no", "studyday","Code", "description","distype", "patient_gender"), ])
cnt <- cnt [, `:=` (mnth = round( studyday /30.25, digits = 0), 
                    description = paste("'", trimws(distype), ": ", trimws(description), sep = ""),
                    Code =  paste("[", trimws(Code), "]", sep="")) ]

# Sorted by the sequence of days
#cnt <- cnt [order(mr_no, studyday, Code, description, patient_gender)]
cnt <- cnt [order(mr_no, Code, description, patient_gender)]
cnt2 <- unique(cnt [, c("mr_no", "Code", "description", "patient_gender", "distype"), ])

# Combinations for each patient
# Do these calculations for first rows
cnt3 <- cnt2[, `:=` (numcomb = seq_len(.N),
                     descomb = description,
                     discomb = Code,
                     grpcomb = paste(trimws(Code), collapse = " ", sep=)),
             by = .(mr_no, patient_gender)]

cnt30 <- cnt3 [numcomb > 0, `:=` (discomb = sapply(seq_len(.N), function(x) paste(Code[seq_len(x)], collapse = ">")),
                                  descomb = sapply(seq_len(.N), function(x) paste(description[seq_len(x)], collapse = " ")) ),
               by = .(mr_no, patient_gender)]

counter <- cnt30 [, .(npat = .N), by = .(discomb, descomb, patient_gender)]

# Merge the cumulative diseases and treatments into 1 dataset
all_sub_med02 <- merge (x = all_sub_med [, -c("description")],
                        y = cnt30,
                        by = c("mr_no","distype", "patient_gender", "Code"))

fwrite(all_sub_med02, 
       "D:/Hospital_data/ProgresSQL/analysis/Primary_madhumeha_comb_medicine.csv")

counter_mrno <- all_sub_med02 [, .(npat = uniqueN(mr_no)), by = .(discomb, descomb, medicine_name,patient_gender)]

chk <- counter_mrno [ discomb == "[M2_0]"]
