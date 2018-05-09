
library(data.table)
library(zoo)
library(stringi)
library(stringr)
#library(openxlsx)
library(anytime)
library(collapsibleTree)
#library(quantmod)
library(htmltools)
library(networkD3)
library(jsonlite)
library(tidyr)

#####################
# This version for 05, not working so far
#####################

all_met_rmsd <- readRDS("D:/Hospital_data/ProgresSQL/analysis/01adsl_met_rmsd.rds")
all_met_rmsd <- all_met_rmsd [, Code := str_replace_all(Code, "\\.", "_")]
cnt<- unique( all_met_rmsd [patient_gender != "" & Code != "", 
                            c("mr_no", "studyday","Code", "description","distype", "patient_gender"), ])

cnt <- cnt [order(mr_no, studyday, Code, description, patient_gender)]
cnt2 <- unique(cnt [, c("mr_no", "Code", "description", "patient_gender", "distype"), ])

# Combinations for each patient
# Do these calculations for first rows
cnt3 <- cnt2[, `:=` (numcomb = seq_len(.N),
                     descomb = description,
                     discomb = Code,
                     grpcomb = paste(trimws(Code), collapse = " ", sep=)),
             by = .(mr_no, patient_gender)]

# Starting disease sttdis
stt <- cnt3 [ numcomb == 1, .(sttdis = discomb), by =.(mr_no, patient_gender, Code)]
cnt3disprgs <- merge(cnt3, stt [, c("mr_no", "sttdis", "patient_gender"), ], by = c("mr_no", "patient_gender"))

cnt3disprgs02 <- cnt3disprgs [, .(npt = uniqueN(mr_no)), by = .(discomb, descomb, numcomb, grpcomb, sttdis, patient_gender)]

cnt3disprgs03 <- dcast(data = cnt3disprgs02, 
                       npt + sttdis + grpcomb + patient_gender ~ paste("node", str_pad(numcomb, width=3, pad="0", side= c("left")), sep=""),
                       value.var = c("discomb"),
                       fill=" ")
tmp <- cnt3disprgs03[sttdis == "A11_0", -c("grpcomb")]



cnt3disprgs03 <- dcast(data = cnt3disprgs02, 
                       sttdis + grpcomb + patient_gender 
                       ~ paste("fieldA.children.", numcomb, ".name", sep=""),
                       value.var = c("discomb"),
                       fill=" ")

setnames(cnt3disprgs03, "sttdis", "fieldA.name")
setnames(cnt3disprgs03, "patient_gender", "fieldA.children.0.name")

tmp <- cnt3disprgs03[fieldA.name == "A11_0", -c("grpcomb")]
fwrite(tmp, "D:\\Hospital_data\\ProgresSQL\\misc\\jsfolder\\05gender_json\\sub.csv")

user@DESKTOP-13IUJT2 /cygdrive/d/Hospital_data/ProgresSQL/misc/jsfolder/05gender_json
$ csvtojson --ignoreEmpty=TRUE sub.csv > sub.json




#####################
# This version is for m / f
# Use 04dis_gender_csv folder
#####################
all_met_rmsd <- readRDS("D:/Hospital_data/ProgresSQL/analysis/01adsl_met_rmsd.rds")
all_met_rmsd <- all_met_rmsd [, Code := str_replace_all(Code, "\\.", "_")]
cnt<- unique( all_met_rmsd [patient_gender != "" & Code != "", 
                            c("mr_no", "studyday","Code", "description","distype", "patient_gender"), ])
cnt <- cnt [, `:=` (mnth = round( studyday /30.25, digits = 0), 
                    description = paste("'", trimws(distype), ": ", trimws(description), sep = ""),
                    Code =  paste("[", trimws(Code), "]", sep="")) ]
cntrow <- cnt [, .(permnth = uniqueN(Code) ), by =.(mr_no, mnth)]

cnt <- cnt [order(mr_no, studyday, Code, description, patient_gender)]
cnt2 <- unique(cnt [, c("mr_no", "Code", "description", "patient_gender", "distype"), ])

# Combinations for each patient
# Do these calculations for first rows
cnt3 <- cnt2[, `:=` (numcomb = seq_len(.N),
                     descomb = description,
                     discomb = Code,
                     grpcomb = paste(trimws(Code), collapse = " ", sep=)),
             by = .(mr_no, patient_gender)]

cnt30 <- cnt3 [numcomb > 1, `:=` (discomb = sapply(seq_len(.N), function(x) paste(Code[seq_len(x)], collapse = ".")),
                                  descomb = sapply(seq_len(.N), function(x) paste(description[seq_len(x)], collapse = " ")) ),
               by = .(mr_no, patient_gender)]

cnt31 <- rbind(cnt3 [numcomb ==1], cnt30 [numcomb > 1])

# Starting disease sttdis
stt <- cnt3 [ numcomb == 1, .(sttdis = paste(discomb, ".", patient_gender, sep="")), by =.(mr_no, patient_gender, Code)]
cnt3disprgs <- merge(cnt31, stt [, c("mr_no", "sttdis"), ], by = c("mr_no"))

cnt3disprgs <- cnt3disprgs [, .(npt = uniqueN(mr_no)), by = .(discomb, descomb, numcomb, grpcomb, sttdis, patient_gender)]

cnt3disprgs <- cnt3disprgs [order(sttdis, patient_gender, numcomb, discomb, grpcomb)]
cnt3disprgs <- cnt3disprgs [, node := 1:.N, by =.(sttdis, patient_gender, grpcomb)]

cnt3disprgs <- cnt3disprgs [, treecomb := paste(sttdis, ".", discomb, sep="")]
cnt3disprgs <- cnt3disprgs [order(sttdis, grpcomb, node)]

cnt3disprgs02 <- cnt3disprgs [numcomb > 1]

# These 2 subsets are for the CSV for D3js
sttdis <- unique(stt [, c("Code"), ])
sttdisgen <- unique(stt [ sttdis != "", c("sttdis"), ])
frow <- data.table ( treecomb = "id,value")

# Rename to the same variable
setnames(sttdis, "Code", "treecomb")
setnames(sttdisgen, "sttdis", "treecomb")

cnt3disprgs03 <- rbind(cnt3disprgs02 [, c("treecomb")], sttdis, sttdisgen) [order(treecomb)]
cnt3disprgs03 <- cnt3disprgs03[, treecomb := paste("Disease.", treecomb, ",", sep="")]

cnt3disprgs03 <- rbind(frow, cnt3disprgs03)

# No subset
fwrite(unique(cnt3disprgs03), 
       col.names = FALSE,
       quote = FALSE,
       "D:\\Hospital_data\\ProgresSQL\\misc\\jsfolder\\04dis_gender_csv\\a11_0_gender.csv")
#####################################################################################
# End of program
#####################################################################################


#####################################################
# The verson of data which is working with d3_csv file
# Use the file created by the following code in the following folder
# D:\\Hospital_data\\ProgresSQL\\misc\\jsfolder\\03dis_csv\\
# Then execute the D3js program to generate the tree
# This tree is not collapsible at this point
#####################################################
all_met_rmsd <- readRDS("D:/Hospital_data/ProgresSQL/analysis/01adsl_met_rmsd.rds")
all_met_rmsd <- all_met_rmsd [, Code := str_replace_all(Code, "\\.", "_")]
cnt<- unique( all_met_rmsd [patient_gender != "" & Code != "", 
                            c("mr_no", "studyday","Code", "description","distype", "patient_gender"), ])
cnt <- cnt [, `:=` (mnth = round( studyday /30.25, digits = 0), 
                    description = paste("'", trimws(distype), ": ", trimws(description), sep = ""),
                    Code =  paste("[", trimws(Code), "]", sep="")) ]

cntrow <- cnt [, .(permnth = uniqueN(Code) ), by =.(mr_no, mnth)]

cnt <- cnt [order(mr_no, studyday, Code, description, patient_gender)]
cnt2 <- unique(cnt [, c("mr_no", "Code", "description", "patient_gender", "distype"), ])

# Combintions for each patient

cnt3 <- cnt2[, `:=` (discomb = sapply(seq_len(.N), function(x) paste(Code[seq_len(x)], collapse = ".")),
                     descomb = sapply(seq_len(.N), function(x) paste(description[seq_len(x)], collapse = " ")),
                     numcomb = seq_len(.N),
                     grpcomb = paste(trimws(Code), collapse = " ", sep=)),
             by = .(mr_no, patient_gender)]

cnt3disprgs <- cnt3 [, .(npt = uniqueN(mr_no)), by = .(discomb, descomb, numcomb, grpcomb)]
cnt3disprgs <- cnt3disprgs [, sttdis := word (discomb, sep ="\\."), ]
cnt3disprgs <- cnt3disprgs [order(sttdis, numcomb, discomb, grpcomb)]
cnt3disprgs <- cnt3disprgs [, node := 1:.N, by =.(sttdis, grpcomb)]

cnt3disprgs <- cnt3disprgs [, treecomb := paste("Disease.", discomb, ",", sep="")]
cnt3disprgs <- cnt3disprgs [order(sttdis, grpcomb, node)]

fwrite(unique(cnt3disprgs [sttdis %in% c("[A11_0]", "[A1_0]"), c("treecomb")]), 
       col.names = FALSE,
       quote = FALSE,
       "D:\\Hospital_data\\ProgresSQL\\misc\\jsfolder\\03dis_csv\\a11_0.csv")

#####################################################################################
# End of program
#####################################################################################


devtools::install_github
devtools::install_github("timelyportfolio/networkD3@feature/d3.chart.layout")

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
all_met_rmsd <- readRDS("D:/Hospital_data/ProgresSQL/analysis/01adsl_met_rmsd.rds")
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
                     descomb = sapply(seq_len(.N), function(x) paste(description[seq_len(x)], collapse = " ")),
                     numcomb = seq_len(.N),
                     grpcomb = paste("'", trimws(Code), "'", collapse = " ", sep=)),
             by = .(mr_no, patient_gender)]

cnt3disprgs <- cnt3 [, .(npt = uniqueN(mr_no)), by = .(discomb, descomb, numcomb, grpcomb)]
cnt3disprgs <- cnt3disprgs [, sttdis := str_replace_all(word (discomb), "'", ""), ]
cnt3disprgs <- cnt3disprgs [order(sttdis, numcomb, discomb, grpcomb)]
cnt3disprgs <- cnt3disprgs [, node := 1:.N, by =.(sttdis, grpcomb)]

cnt3disprgs02 <- dcast(cnt3disprgs,
                       sttdis + grpcomb ~ paste("node", str_pad(numcomb, width=3, pad="0", side= c("left")), sep=""),
                       value.var = c("discomb"),
                       fill = " ")
                       
fwrite(cnt3disprgs, "D:\\Hospital_data\\ProgresSQL\\analysis\\node_verti.csv")
fwrite(cnt3disprgs02, "D:\\Hospital_data\\ProgresSQL\\analysis\\node_horiz.csv")

tmp <- cnt3disprgs02 [sttdis %in% c("M1.0")] #[sttdis %in% c("A11.0", "A1.0")] 
#[order(sttdis, discomb, numcomb, grpcomb)]

collapsibleTree(
  tmp,
  root = deparse(substitute(tmp)),
  hierarchy = c("sttdis", 
                "node001", "node002", "node003", "node004", "node005", "node006", "node007", "node008",
                "node009", "node010", "node011", "node012", "node013", "node014", "node015", "node016",
                "node017", "node018", "node019", "node020", "node021", "node022", "node023", "node024",
                "node025", "node026", "node027"),
  width = 1800,
  height = 1800,
  tooltip = TRUE,
  nodeSize = "leafCount",
  linkLength = 200
  )

collapsibleTreeSummary(
tmp,
root = deparse(substitute(tmp)),
hierarchy = c("sttdis", 
              "node001", "node002", "node003", "node004", "node005", "node006", "node007", "node008",
              "node009", "node010", "node011", "node012", "node013", "node014", "node015", "node016",
              "node017", "node018", "node019", "node020", "node021", "node022", "node023", "node024",
              "node025", "node026", "node027"),
width = 1800,
height = 1800,
tooltip = TRUE,
nodeSize = "leafCount",
linkLength = 200
)


tmp [1,1] <- NA

collapsibleTreeNetwork(
  tmp,
  attribute = "Title",
  fill = "Color",
  nodeSize = "leafCount",
  tooltipHtml = "tooltip",
  collapsed = FALSE
)


# Create Json file from data.table

jsondis <- toJSON(cnt3disprgs, pretty = TRUE)
