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

#######################################################################
# These 2 are created using 085_dis_1st_time_refCal_NodeEdges.R program
#######################################################################

all_met_rmsd02 <- readRDS ("D:/Hospital_data/ProgresSQL/analysis/all_met_rmsd02.rds")
all_met_rmsd02 <- all_met_rmsd02 [, Coded_med := paste(Type_med, Coded_med, sep=":"),]

chk01 <- all_met_rmsd02 [, .(cnt = uniqueN(mr_no)), 
                         by = .(period, periodn, refcode, refdesc, Code, description, Type_med, Coded_med )]
chk01 <- chk01[Code != "" & Coded_med != ""]
chk01 <- chk01 [, `:=` (Code02 = paste(Code, ":", description, "->", Coded_med, sep =""),
                        name = paste(period, periodn, refcode, refdesc, sep =","),
                        key = paste(Code, description, sep=","),
                        nrow = .I), ]

###################################################################
# Create a version of data as follows:
# Fixed nodes as relation between 
# (1) Period + Reference disease <--> other diseases
# (2) other diseases <--> Medicine
# Use the other diseases section for creating the moving nodes
###################################################################
part01 <- chk01 [, c("name", "key", "nrow", "Code02", "cnt", "refdesc", "refcode"), ]

part02 <- chk01 [, c("Coded_med", "key", "nrow", "Code02", "cnt", "refdesc", "refcode"), ]
setnames(part02, "Coded_med", "name")

part03 <- rbind (part01, part02)
part03 <- part03 [, num := .N, by =.(refdesc, key)]
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
# "name" : use key
# "count" : use num
# "key" : use key
# "pages" : 
#   "names" : use name
#   "key" : use nrow
#   "title" : use name
#   "url" : Code02
######################################################################

part04 <- part03 [, frstprt := paste('{"name" :"', key, '", "count" :', pernum, ', "key" :"', key, '",', sep ="" ), ]
part04 <- part04 [, scndprt := paste('{"name" :"', name, '", "key" :', nrow, ', "title" :"', name, '", "url" :"', Code02, '"}', sep = ""), ]

#######################################################################
# Combine the scndprt variable into 1 row per refdesc + key combination
#######################################################################
part05 <- part04 [, .(scndprt02 = paste(scndprt, collapse = ",", sep = " " )), 
                  by = .(refcode, refdesc, frstprt)]
part05 <- part05 [ order(refcode, refdesc, frstprt)]
part05 <- part05 [, rowrecal := .I, by = .(refcode, refdesc)]
part05 <- part05 [, scndprt03 := paste('"pages": [', scndprt02, "]},", sep=""), ]

chk02 <- part05 [ refcode == "A2.0"]

fwrite(chk02 [ scndprt02 != "...", c("frstprt", "scndprt03"), ], 
       "D:/Hospital_data/ProgresSQL/analysis/085d3concept.json", 
       col.names = FALSE, 
       quote = FALSE, 
       sep = " ")



chk02 <- chk02 [, `:=` (name = paste(period, periodn, refcode, refdesc, sep =","),
                        count = cnt,
                        key = paste(Code, description, sep=","),
                        pages = Coded_med,
                        url = Code02, 
                        title = Code02),]

chk03 <- chk02 [, c("name", "count", "key", "pages", "url", "title", "nrow"), ]

write_json(chk03, 
       "D:/Hospital_data/ProgresSQL/misc/bruce_approach/085d3concept.json")

# Validate Json using https://jsonlint.com/


fwrite(chk01, 
       "D:/Hospital_data/ProgresSQL/analysis/085_dis_count_edges_3rd_byPeriod_.csv")


chk01 <- all_met_rmsd02 [, .(cnt = uniqueN(mr_no)), 
                         by = .(refcode, refdesc, Code, description, Type_med, Coded_med )]
chk01 <- chk01[Code != "" & Coded_med != ""]
chk01 <- chk01 [, Code02 := paste(Code, ":", description, "->", Coded_med, sep =""),]

chk02 <- chk01 [ refcode == "A2.0"]

fwrite(chk02, 
       "D:/Hospital_data/ProgresSQL/analysis/085_dis_count_edges_3rd_byPeriod_A2_bruce.csv")

#########################################################################################
