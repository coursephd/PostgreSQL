
library(data.table)
library(zoo)
library(stringi)
library(stringr)
library(openxlsx)
library(anytime)

all_met_rmsd <- readRDS("D:/Hospital_data/ProgresSQL/analysis/01adsl_met_rmsd.rds")

diag2 <- all_met_rmsd [Code !="", Code2 := ifelse(Code =="", "ZZZ999", Code), ] [order(mr_no, Code2)]
diag2 <- diag2 [, description := ifelse(Code2 == "ZZZ999", "** Uncoded", description), ]

cnt<- unique( all_met_rmsd [patient_gender != "" & Code != "", 
                            c("mr_no", "studyday","Code", "description","distype", "patient_gender"), ])
cnt <- cnt [, mnth := round( studyday /30.25, digits = 0), ]
cntrow <- cnt [, .(permnth = uniqueN(Code) ), by =.(mr_no, mnth)]

###############################################################
# Order the diseases by study day and get the uniue combintions
###############################################################
cnt <- cnt [order(mr_no, studyday, Code, description, patient_gender)]
cnt2 <- unique(cnt [, c("mr_no", "Code", "description", "patient_gender", "distype"), ])
cnt2 <- cnt2 [, description := paste(distype, description, sep = ": "),]

# Combintions for each patient
cnt3 <- cnt2[, .(discomb = paste("'", trimws(Code), "'", collapse=" ", sep=""),
                 descomb = paste("'", trimws(description), "'", collapse=" ", sep=""),
                 cntdis = uniqueN(Code) ),
             by = .(mr_no, patient_gender)]

cnt3unq <- cnt3 [, .(unqdiscomb = .N), by =.(discomb, descomb, cntdis)]
cnt3unq <- cnt3unq [order (-unqdiscomb, -cntdis, discomb, descomb)]

cnt3unq_gndr <- cnt3 [, .(unqdiscomb = .N), by =.(discomb, descomb, cntdis, patient_gender)]
cnt3unq_gndrt <- dcast(cnt3unq_gndr,
                       discomb + descomb + cntdis ~ patient_gender,
                       value.var = c("unqdiscomb"),
                       fill = 0)

############################################
# This is by alphabetical order
############################################
cnt<- unique( all_met_rmsd [patient_gender != "" & Code != "", 
                            c("mr_no", "studyday","Code", "description","distype", "patient_gender"), ])
cnt <- cnt [, mnth := round( studyday /30.25, digits = 0), ]
cntrow <- cnt [, .(permnth = uniqueN(Code) ), by =.(mr_no, mnth)]

cnt <- cnt [order(mr_no, Code, description, patient_gender)]
cnt2 <- unique(cnt [, c("mr_no", "Code", "description", "patient_gender", "distype"), ])
cnt2 <- cnt2 [, description := paste(distype, description, sep = ": "),]

# Combintions for each patient
cnt3 <- cnt2[, .(discomb = paste("'", trimws(Code), "'", collapse=" ", sep=""),
                 descomb = paste("'", trimws(description), "'", collapse=" ", sep=""),
                 cntdis = uniqueN(Code) ),
             by = .(mr_no, patient_gender)]

cnt3unq <- cnt3 [, .(unqdiscomb = .N), by =.(discomb, descomb, cntdis)]

cnt3unq_gndr <- cnt3 [, .(unqdiscomb = .N), by =.(discomb, descomb, cntdis, patient_gender)]
cnt3unq_gndrt <- dcast(cnt3unq_gndr,
                       discomb + descomb + cntdis ~ patient_gender,
                       value.var = c("unqdiscomb"), 
                       fill = 0)



###############################################################
# Order the diseases by study day and get the uniue combintions
# This is other cumultive view of disease progression tree
###############################################################
cnt<- unique( all_met_rmsd [patient_gender != "" & Code != "", 
                            c("mr_no", "studyday","Code", "description","distype", "patient_gender"), ])
cnt <- cnt [, `:=` (mnth = round( studyday /30.25, digits = 0), 
                     description = paste("'", trimws(distype), ": ", trimws(description), sep = ""),
                     Code =  paste("'", trimws(Code), "'", sep="")) ]

cntrow <- cnt [, .(permnth = uniqueN(Code) ), by =.(mr_no, mnth)]

cnt <- cnt [order(mr_no, studyday, Code, description, patient_gender)]
cnt2 <- unique(cnt [, c("mr_no", "Code", "description", "patient_gender", "distype"), ])

# Combintions for each patient

cnt3 <- cnt2[, `:=` (discomb = sapply(seq_len(.N), function(x) paste(Code[seq_len(x)], collapse = " ")),
                     descomb = sapply(seq_len(.N), function(x) paste(description[seq_len(x)], collapse = " ")) ),
             by = .(mr_no, patient_gender)]

cnt3disprgs <- cnt3 [, .(npt = uniqueN(mr_no)), by = .(discomb, descomb)]

cnt3unq <- cnt3 [, .(unqdiscomb = .N), by =.(discomb, descomb, cntdis)]
cnt3unq <- cnt3unq [order (-unqdiscomb, -cntdis, discomb, descomb)]

cnt3unq_gndr <- cnt3 [, .(unqdiscomb = .N), by =.(discomb, descomb, cntdis, patient_gender)]
cnt3unq_gndrt <- dcast(cnt3unq_gndr,
                       discomb + descomb + cntdis ~ patient_gender,
                       value.var = c("unqdiscomb"),
                       fill = 0)
