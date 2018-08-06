library(data.table)
library(stringi)
library(stringr)
library(sqldf)

##########################################
# Disease and medicine combination
# Count of patients as weight
##########################################


all_met_rmsd <- readRDS("D:/Hospital_data/ProgresSQL/analysis/01adsl_met_rmsd.rds")

gephi <- unique(all_met_rmsd [, .(Weight = uniqueN(mr_no)), by = .(Code, Coded_med)])
gephi02 <- na.omit(gephi) 
gephi02 <- gephi02 [ nchar(Code) > 0]
setnames (gephi02, "Code", "Label")

setnv <- gephi02 [, c(1, 2, 3),]
setnames(setnv, "Label", "source")
setnames(setnv, "Coded_med", "target")
setnames(setnv, "Weight", "weight")

soc01 <- unique(all_met_rmsd [Code != "" | Code != " ", c("mr_no", "Code"), ])
soc01 <- soc01 [ nchar(Code) > 0]
soc02 <- soc01 [, Code2 := Code]

soc03 <- merge(x = soc01 [, c("mr_no", "Code"),],
               y = soc02 [, c("mr_no", "Code2"),],
               by = c("mr_no"),
               allow.cartesian = TRUE)

soc04 <- soc03 [, .(Weight = uniqueN(mr_no)), by = .(Code, Code2)]

setnames(soc04, "Code", "source")
setnames(soc04, "Code2", "target")
setnames(soc04, "Weight", "weight")

setnv02 <- rbind(setnv, soc04)
setnv02 <- setnv02[, source := str_replace_all(string=source, pattern=" ", repl=""),]
setnv02 <- setnv02[, target := str_replace_all(string=target, pattern=" ", repl=""),]

fwrite(setnv02, "D:/Hospital_data/ProgresSQL/analysis/setnv01weighted.csv")
fwrite(setnv02, sep ="\t", "D:/Hospital_data/ProgresSQL/analysis/setnv01weighted.txt")

#######################################################################################

#############################
# Patient disease combination
#############################
all_met_rmsd <- readRDS("D:/Hospital_data/ProgresSQL/analysis/01adsl_met_rmsd.rds")

gephi <- unique(all_met_rmsd [, .(Weight = uniqueN(mr_no)), by = .(Code, Coded_med)])
gephi02 <- na.omit(gephi) 
gephi02 <- gephi02 [ nchar(Code) > 0]
gephi02 <- gephi02 [order(Code, Coded_med)]
gephi02 <- gephi02 [, Id := .GRP, by = .(Code)]
gephi02 <- gephi02 [, Target := .GRP, by = .(Coded_med)]
gephi02 <- gephi02 [, Type := "Directed", ]

setnames (gephi02, "Code", "Label")
nodes <- unique (gephi02 [, c("Id", "Label")])

edges <- gephi02 [, c("Id", "Target", "Type", "Weight", "Coded_med")]
setnames (edges, "Id", "Source")

fwrite(nodes, "D:/Hospital_data/ProgresSQL/analysis/gephi_nodes.csv")
fwrite(edges, "D:/Hospital_data/ProgresSQL/analysis/gephi_edges.csv")
#######################################################################################

mr320 <- all_met_rmsd [mr_no == "MR000320", c("mr_no", "studyday", "Code", "description", "Coded_med"), ]

gephi02 <- na.omit(mr320) 
gephi02 <- gephi02 [ nchar(Code) > 0]
gephi02 <- gephi02 [order(Code, Coded_med)]
gephi02 <- gephi02 [, Id := .GRP, by = .(Code)]
gephi02 <- gephi02 [, Target := .GRP, by = .(Coded_med)]
gephi02 <- gephi02 [, Type := "Directed", ]

setnames (gephi02, "Code", "Label")
nodes <- unique (gephi02 [, c("Id", "Label")])

edges <- unique(gephi02 [, c("Id", "Target", "Type", "Coded_med", "studyday")])
setnames (edges, "Id", "Source")
setnames (edges, "studyday", "Interval")

fwrite(nodes, "D:/Hospital_data/ProgresSQL/analysis/gephi_320_nodes.csv")
fwrite(edges, "D:/Hospital_data/ProgresSQL/analysis/gephi_320_edges.csv")
#######################################################################################

all_met_rmsd <- readRDS("D:/Hospital_data/ProgresSQL/analysis/01adsl_met_rmsd.rds")

mrdis <- unique (all_met_rmsd [, c("mr_no", "studyday", "newdt0","Code", "description", "patient_gender")])

cnt3 <- mrdis[, .(rpcomb = paste(trimws(Code), collapse = " ", sep=",")),
             by = .(mr_no, patient_gender)]

cnt3 <- cnt3 [, nrow :=.I]

cnt4 <- cnt3 [ nrow <= 100]

#fwrite(cnt4 [, -c("nrow"),], "D:/Hospital_data/ProgresSQL/analysis/gephi_pat_dis.csv")

fwrite(mrdis, "D:/Hospital_data/ProgresSQL/analysis/gephi_pat_dis.csv")
