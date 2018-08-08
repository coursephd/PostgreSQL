
library(data.table)
library(stringi)
library(stringr)
library(sqldf)

all_met_rmsd <- readRDS("D:/Hospital_data/ProgresSQL/analysis/01adsl_met_rmsd.rds")

gephi <- unique(all_met_rmsd [, .(Weight = uniqueN(mr_no)), by = .(Code, Coded_med)])
gephi02 <- na.omit(gephi) 
gephi02 <- gephi02 [ nchar(Code) > 0]
setnames (gephi02, "Code", "Label")

setnv <- gephi02 [, c(1, 2, 3),]
setnames(setnv, "Label", "source")
setnames(setnv, "Coded_med", "target")
setnames(setnv, "Weight", "weight")
setnv <- setnv [, `:=` (linex = 10, NodeName = target),]

setnv00 <- copy(setnv)
setnv00 <- setnv00 [, `:=` (linex = 50, NodeName = source),]

###############################################
# Unique disease and codes
# Combine the rownum and other into one data
###############################################
code <- unique(setnv [, c("source"),])
code <- code [ order(source)]
code <- code [, liney := .I, ]
setnames(code, "source", "ref") 

dis <- unique(setnv [, c("target"),])
dis <- dis [ order(target)]
dis <- dis [, liney := .I, ]
setnames(dis, "target", "ref") 

ref02 <- rbind(code, dis)

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
soc04 <- soc04 [, `:=` (linex = 50, NodeName = source),]

soc040 <- copy(soc04)
soc040 <- soc040 [, `:=` (linex = 100, NodeName = target),]

setnv_ref <- merge(x = setnv,
                   y = ref02,
                   by.x = c("target"),
                   by.y = c("ref"),
                   all.x = TRUE)

setnv00_ref <- merge(x = setnv00,
                     y = code,
                     by.x = c("source"),
                     by.y = c("ref"),
                     all.x = TRUE)

soc04_ref <- merge(x = soc04,
                   y = code,
                   by.x = c("source"),
                   by.y = c("ref"),
                   all.x = TRUE)

soc040_ref <- merge(x = soc040,
                    y = code,
                    by.x = c("target"),
                    by.y = c("ref"),
                    all.x = TRUE)

setnv02 <- rbind(setnv_ref, setnv00_ref, soc04_ref, soc040_ref)

setnv03 <- setnv02 [, `:=` (circley = liney,
                            relationship = paste(trimws(source), "->", trimws(target), sep = "") ),]

setnv03 <- setnv03 [, id := .GRP, by = .(source, target)]

fwrite(setnv03, "D:/Hospital_data/ProgresSQL/analysis/090_med_dis_rel_tableau.csv")
#######################################################################################



all_met_rmsd <- readRDS("D:/Hospital_data/ProgresSQL/analysis/01adsl_met_rmsd.rds")

gephi <- unique(all_met_rmsd [, .(Weight = uniqueN(mr_no)), by = .(Code, Coded_med)])
gephi02 <- na.omit(gephi) 
gephi02 <- gephi02 [ nchar(Code) > 0]
setnv <- gephi02 [, c(1, 2, 3),]
setnv <- setnv [order(Code)]
setnv <- setnv [, row := 1:.N, by = .(Code)]

soc01 <- unique(all_met_rmsd [Code != "" | Code != " ", c("mr_no", "Code"), ])
soc01 <- soc01 [ nchar(Code) > 0]
soc02 <- soc01 [, Code2 := Code]
soc03 <- merge(x = soc01 [, c("mr_no", "Code"),],
               y = soc02 [, c("mr_no", "Code2"),],
               by = c("mr_no"),
               allow.cartesian = TRUE)

soc04 <- soc03 [, .(Weight02 = uniqueN(mr_no)), by = .(Code, Code2)]
soc04 <- soc04 [, row := 1:.N, by = .(Code)]

setnv02 <- merge(x = setnv, 
                 y = soc04,
                 by = c("Code", "row"),
                 all = TRUE)

fwrite(setnv02, "D:/Hospital_data/ProgresSQL/analysis/090_med_dis_rel_tableau.csv")
#######################################################################################

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


sub <- setnv02 [ source %in% c("M10.0",	"M10.1",	"M10.2",	"M2.0",	"P5.0",	"P5.1",	"P5.2",	"P5.3",	"P5.4", "S16.0") | 
                   target %in% c("M10.0",	"M10.1",	"M10.2",	"M2.0",	"P5.0",	"P5.1",	"P5.2",	"P5.3",	"P5.4", "S16.0")]

fwrite(sub, "D:/Hospital_data/ProgresSQL/analysis/setnv01weighted_metabolic.csv")
fwrite(sub, sep ="\t", "D:/Hospital_data/ProgresSQL/analysis/setnv01weighted_metabolic.txt")


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
