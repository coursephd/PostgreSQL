library(data.table)
library(stringi)
library(stringr)
library(sqldf)

all_met_rmsd <- readRDS("D:/Hospital_data/ProgresSQL/analysis/01adsl_met_rmsd.rds")

gephi <- unique(all_met_rmsd [, .(Weight = uniqueN(mr_no)), by = .(Code, Coded_med)])
gephi02 <- na.omit(gephi) 
gephi02 <- gephi02 [ nchar(Code) > 0]
gephi02 <- gephi02 [order(Code, Coded_med)]
gephi02 <- gephi02 [, Id := .GRP, by = .(Code)]
gephi02 <- gephi02 [, Target := .GRP, by = .(Coded_med)]
gephi02 <- gephi02 [, Type := "Undirected", ]

nodes <- unique (gephi02 [, c("Id", "Code")])

edges <- gephi02 [, c("Id", "Target", "Type", "Weight", "Coded_med")]
setnames (edges, "Id", "Source")

fwrite(nodes, "D:/Hospital_data/ProgresSQL/analysis/gephi_nodes.csv")
fwrite(edges, "D:/Hospital_data/ProgresSQL/analysis/gephi_edges.csv")