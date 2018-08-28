####################################################################
# This is used for 085_dis_count_edges_3rd_byPeriod Tableau display
####################################################################

library(data.table)
library(stringi)
library(stringr)
library(sqldf)
library(tidyr)
library(rjson)
library(jsonlite)
library(dplyr)

#######################################################################
# These 2 are created using 085_dis_1st_time_refCal_NodeEdges.R program
#######################################################################

all_met_rmsd02 <- readRDS ("D:/Hospital_data/ProgresSQL/analysis/all_met_rmsd02.rds")
all_met_rmsd02 <- all_met_rmsd02 [, Coded_med := paste(Type_med, Coded_med, sep=":"),]

all_met_rmsd02 <- all_met_rmsd02 [, Coded_med := str_replace_all(Coded_med, "\"", ""),]

chk01 <- all_met_rmsd02 [, .(cnt = uniqueN(mr_no)), 
                         by = .(period, periodn, refcode, refdesc, Code, description, Type_med, Coded_med )]
chk01 <- chk01[Code != "" & Coded_med != ""]
chk01 <- chk01 [, `:=` (Code02 = paste(Code, ":", description, "->", Coded_med, sep =""),
                        name = paste(Code, description, sep =","),
                        key = paste(period, periodn, sep=",") ), ]

med <- unique( chk01 [!Coded_med %in% c("", " "), c("Coded_med"), ])
setnames(med, "Coded_med", "name")
dis <- unique( chk01 [!name %in% c("", " "), c("name"), ])
meddis <- rbind(med, dis)
meddis <- meddis [, nrow := .I,]

###################################################################
# Create a version of data as follows:
# Fixed nodes as relation between 
# (1) Period + Reference disease <--> other diseases
# (2) other diseases <--> Medicine
# Use Period + Periodn as a floating key
# Use the other diseases section for creating the moving nodes
###################################################################
part01 <- chk01 [, c("name", "key", "Code02", "cnt", "refdesc", "refcode", "period", "periodn"), ]

part02 <- chk01 [, c("Coded_med", "key", "Code02", "cnt", "refdesc", "refcode", "period", "periodn"), ]
setnames(part02, "Coded_med", "name")

part03 <- rbind (part01, part02)
part03 <- merge(part03, 
                meddis, 
                by = c("name"),
                all = TRUE)

part03 <- part03 [, num := .N, by =.(refdesc, name)]
part03 <- part03 [, maxnum := max(num), by =.(refdesc)]
part03 <- part03 [, pernum := (num / maxnum) * 100,]

###################################################################
# Create the Json file
#
#[{
#  "name": "addons",
#  "count": 1,
#  "key": "addons",
#  "pages": [{
#    "name": "A year in apps script and my bucket list",
#    "key": "4478459723408930641",
#    "title": "A year in apps script and my bucket list",
#    "url": "http://excelramblings.blogspot.com/2015/01/a-year-in-apps-script-and-my-bucket-list.html"
# }]
# }
#
# "name" : use name
# "count" : use num
# "key" : use name
# "pages" : 
#   "names" : use name
#   "key" : use nrow
#   "title" : use name
#   "url" : Code02
######################################################################

part04 <- part03 [, frstprt := paste('{"name" :"', name, '", "count" :', pernum, ', "key" :"', name, '",', sep ="" ), ]
part04 <- part04 [, scndprt := paste('{"name" :"', name, '", "key" :', nrow, ', "title" :"', name, '", "url" :"', Code02, '"}', sep = ""), ]

#######################################################################
# Combine the scndprt variable into 1 row per 
# period + period (which is stored in key) + refdesc + key combination
#######################################################################
part05 <- part04 [, .(scndprt02 = paste(scndprt, collapse = ",", sep = " " )), 
                  by = .(key, refcode, refdesc, frstprt)]
part05 <- part05 [ order(key, refcode, refdesc, frstprt)]
part05 <- part05 [, rowrecal := .I, by = .(key, refcode, refdesc)]
part05 <- part05 [, scndprt03 := paste('"pages": [', scndprt02, "]},", sep=""), ]

chk02 <- part05 [ refcode == "A2.0"]

fwrite(chk02 [ scndprt02 != "...", c("frstprt", "scndprt03"), ], 
       "D:/Hospital_data/ProgresSQL/analysis/085d3concept_byperiod_A2_0.json", 
       col.names = FALSE, 
       quote = FALSE, 
       sep = " ")


chk02 <- part05 [ refcode == "P5.0"]

fwrite(chk02 [ scndprt02 != "...", c("frstprt", "scndprt03"), ], 
       "D:/Hospital_data/ProgresSQL/analysis/085d3concept_byperiod_P5_0.json", 
       col.names = FALSE, 
       quote = FALSE, 
       sep = " ")

############################################################################
# End of program
############################################################################
