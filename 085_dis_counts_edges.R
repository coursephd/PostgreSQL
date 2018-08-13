####################################################################
# This is used for 085_dis_count_edges_3rd_byPeriod Tableau display
####################################################################

library(data.table)
library(stringi)
library(stringr)
library(sqldf)
library(tidyr)

#######################################################################
# These 2 are created using 085_dis_1st_time_refCal_NodeEdges.R program
#######################################################################

all_met_rmsd02 <- readRDS ("D:/Hospital_data/ProgresSQL/analysis/all_met_rmsd02.rds")
all_met_rmsd02 <- all_met_rmsd02 [, Coded_med := paste(Type_med, Coded_med, sep=":"),]

edges <- readRDS("D:/Hospital_data/ProgresSQL/analysis/085_dis_1st_time_refCal_NodesEdges.rds")

###########################################################
# Get the unique number of reference diseases and medicines
# Create this for each of the periods before and after 1st 
# occurrence of the disease
###########################################################
unqref <- unique( edges [, c("period", "periodn")])

dismed <- unique( edges [, c("cat", "refcode", "refdesc", "Code", "description"), ])
dismed <- dismed [nchar(Code) > 0] [order(cat, refcode, refdesc, Code, description)]
dismed <- dismed [, `:=` (npoints = 1:.N, tot = .N), by = .(cat, refcode, refdesc)]
dismed <- dismed [, `:=` (radius = ifelse (cat == "Disease", 20, 40), 
                          angle = 360 / tot), ]
dismed <- dismed [, cumulative := cumsum(angle), by = .(cat, refcode, refdesc)]

################################################
# Function for the degrees and radian conversion
################################################
deg2rad <- function(deg) {(deg * pi) / (180)}

dismed <- dismed [, radian := deg2rad(cumulative),]
dismed <- dismed [, `:=` (xaxis = cos(radian)*radius,
                          yaxis = sin(radian)*radius), ]

#dismed <- dismed [, Code02 := paste(Code, ":", description), ]
#dismed <- dismed [, cnt := 0,]

########################################################
# create a complete dataset
# this is to ensure, circle is displayed all the time
# Combine with the individual period for replication
########################################################
dismed_all <- crossing(unqref, dismed)


chk01 <- all_met_rmsd02 [, .(cnt = uniqueN(mr_no)), 
                         by = .(period, periodn, refcode, refdesc, Code, description, Type_med, Coded_med )]
chk01 <- chk01[Code != "" & Coded_med != ""]
chk01 <- chk01 [, Code02 := paste(Code, ":", description, "->", Coded_med, sep =""),]

# Merge the x and y coordiantes

chk02dis <- unique(chk01 [, c("period","periodn", "refcode", "refdesc", "Code", "description", "cnt", "Code02")] )
chk02dis <- chk02dis [, cat := "Disease"]

chk02med <- unique(chk01 [, c("period","periodn", "refcode", "refdesc", "Type_med", "Coded_med", "cnt", "Code02")])
setnames (chk02med, "Type_med", "Code")       
setnames (chk02med, "Coded_med", "description")
chk02med <- chk02med [, cat := "Medicine"]

chk02all <- rbind(chk02dis, chk02med)
chk02all <- chk02all [nchar(Code) >0 & nchar(description) > 0 ]

path01 <- merge (x = chk02all,
                 y = dismed_all,
                 by = c("cat", "period","periodn", "refcode", "refdesc", "Code", "description"),
                 all = TRUE)
path01 <- path01 [, `:=` (cat = "DiseaseMedicine", Code = Code02),] 
path01 <- path01 [, c("TabCode", "TabMed") := tstrsplit(Code, "->", fixed = TRUE), ]

chk03all <- rbind(path01, dismed, fill = TRUE)

fwrite(chk03all, 
       "D:/Hospital_data/ProgresSQL/analysis/085_dis_count_edges_3rd_byPeriod.csv")

#########################################################################################


##########################################################
# This is used for 085_dis_count_edges_3rd Tableau display
##########################################################
library(data.table)
library(stringi)
library(stringr)
library(sqldf)
library(tidyr)

#######################################################################
# These 2 are created using 085_dis_1st_time_refCal_NodeEdges.R program
#######################################################################

all_met_rmsd02 <- readRDS ("D:/Hospital_data/ProgresSQL/analysis/all_met_rmsd02.rds")
all_met_rmsd02 <- all_met_rmsd02 [, Coded_med := paste(Type_med, Coded_med, sep=":"),]

edges <- readRDS("D:/Hospital_data/ProgresSQL/analysis/085_dis_1st_time_refCal_NodesEdges.rds")

###########################################################
# Get the unique number of reference diseases and medicines
###########################################################
#unqref <- unique( edges [, c("period", "periodn")])

dismed <- unique( edges [, c("cat", "refcode", "refdesc", "Code", "description"), ])
dismed <- dismed [nchar(Code) > 0] [order(cat, refcode, refdesc, Code, description)]
dismed <- dismed [, `:=` (npoints = 1:.N, tot = .N), by = .(cat, refcode, refdesc)]
dismed <- dismed [, `:=` (radius = ifelse (cat == "Disease", 20, 40), 
                          angle = 360 / tot), ]
dismed <- dismed [, cumulative := cumsum(angle), by = .(cat, refcode, refdesc)]

################################################
# Function for the degrees and radian conversion
################################################
deg2rad <- function(deg) {(deg * pi) / (180)}

dismed <- dismed [, radian := deg2rad(cumulative),]
dismed <- dismed [, `:=` (xaxis = cos(radian)*radius,
                          yaxis = sin(radian)*radius), ]

#dismed <- dismed [, Code02 := paste(Code, ":", description), ]
#dismed <- dismed [, cnt := 0,]

########################################################
# create a complete dataset
# this is to ensure, circle is displayed all the time
########################################################
#dismed_all <- crossing(unqref, dismed)


chk01 <- all_met_rmsd02 [, .(cnt = uniqueN(mr_no)), 
                         by = .(refcode, refdesc, Code, description, Type_med, Coded_med )]
chk01 <- chk01[Code != "" & Coded_med != ""]
chk01 <- chk01 [, Code02 := paste(Code, ":", description, "->", Coded_med, sep =""),]

# Merge the x and y coordiantes

chk02dis <- unique(chk01 [, c("refcode", "refdesc", "Code", "description", "cnt", "Code02")] )
chk02dis <- chk02dis [, cat := "Disease"]

chk02med <- unique(chk01 [, c("refcode", "refdesc", "Type_med", "Coded_med", "cnt", "Code02")])
setnames (chk02med, "Type_med", "Code")       
setnames (chk02med, "Coded_med", "description")
chk02med <- chk02med [, cat := "Medicine"]

chk02all <- rbind(chk02dis, chk02med)
chk02all <- chk02all [nchar(Code) >0 & nchar(description) > 0 ]

path01 <- merge (x = chk02all,
                 y = dismed,
                 by = c("cat", "refcode", "refdesc", "Code", "description"),
                 all = TRUE)
path01 <- path01 [, `:=` (cat = "DiseaseMedicine", Code = Code02),] 
path01 <- path01 [, c("TabCode", "TabMed") := tstrsplit(Code, "->", fixed = TRUE), ]

chk03all <- rbind(path01, dismed, fill = TRUE)

fwrite(chk03all, 
       "D:/Hospital_data/ProgresSQL/analysis/085_dis_count_edges_3rd.csv")
######################################################################################



library(data.table)
library(stringi)
library(stringr)
library(sqldf)
library(tidyr)

#######################################################################
# These 2 are created using 085_dis_1st_time_refCal_NodeEdges.R program
#######################################################################

all_met_rmsd02 <- readRDS ("D:/Hospital_data/ProgresSQL/analysis/all_met_rmsd02.rds")
all_met_rmsd02 <- all_met_rmsd02 [, Coded_med := paste(Type_med, Coded_med, sep=":"),]

edges <- readRDS("D:/Hospital_data/ProgresSQL/analysis/085_dis_1st_time_refCal_NodesEdges.rds")

###########################################################
# Get the unique number of reference diseases and medicines
###########################################################
#unqref <- unique( edges [, c("refcode", "refdesc")])

dismed <- unique( edges [, c("cat", "refcode", "refdesc", "Code", "description"), ])
dismed <- dismed [nchar(Code) > 0] [order(cat, refcode, refdesc, Code, description)]
dismed <- dismed [, `:=` (npoints = 1:.N, tot = .N), by = .(cat, refcode, refdesc)]
dismed <- dismed [, `:=` (radius = ifelse (cat == "Disease", 20, 40), 
                          angle = 360 / tot), ]
dismed <- dismed [, cumulative := cumsum(angle), by = .(cat, refcode, refdesc)]

################################################
# Function for the degrees and radian conversion
################################################
deg2rad <- function(deg) {(deg * pi) / (180)}

dismed <- dismed [, radian := deg2rad(cumulative),]
dismed <- dismed [, `:=` (xaxis = cos(radian)*radius,
                          yaxis = sin(radian)*radius), ]

#dismed <- dismed [, Code02 := paste(Code, ":", description), ]
#dismed <- dismed [, cnt := 0,]

########################################################
# create a complete dataset
# this is to ensure, circle is displayed all the time
########################################################
#dismed_all <- crossing(unqref, dismed)


chk01 <- all_met_rmsd02 [, .(cnt = uniqueN(mr_no)), 
                         by = .(refcode, refdesc, Code, description, Type_med, Coded_med )]
chk01 <- chk01[Code != "" & Coded_med != ""]
chk01 <- chk01 [, Code02 := paste(Code, ":", description, "->", Coded_med, sep =""),]

# Merge the x and y coordiantes

chk02dis <- unique(chk01 [, c("refcode", "refdesc", "Code", "description", "cnt", "Code02")] )
chk02dis <- chk02dis [, cat := "Disease"]

chk02med <- unique(chk01 [, c("refcode", "refdesc", "Type_med", "Coded_med", "cnt", "Code02")])
setnames (chk02med, "Type_med", "Code")       
setnames (chk02med, "Coded_med", "description")
chk02med <- chk02med [, cat := "Medicine"]

chk02all <- rbind(chk02dis, chk02med)
chk02all <- chk02all [nchar(Code) >0 & nchar(description) > 0 ]

path01 <- merge (x = chk02all,
                 y = dismed,
                 by = c("cat", "refcode", "refdesc", "Code", "description"),
                 all = TRUE)
path01 <- path01 [, `:=` (cat = "DiseaseMedicine", Code = Code02),] 
path01 <- path01 [, c("TabCode", "TabMed") := tstrsplit(Code, "->", fixed = TRUE), ]

chk03all <- rbind(path01, dismed, fill = TRUE)

fwrite(chk03all, 
       "D:/Hospital_data/ProgresSQL/analysis/085_dis_count_edges_2nd.csv")



#########################################
# Create counts of diseases and medicines
#########################################
chk01 <- edges [, .(cnt = uniqueN(mr_no)), 
                         by = .(refcode, refdesc, Code, description )]
chk01 <- chk01 [nchar(Code) > 0]

chk02 <- merge (x = dismed_all,
                y = chk01,
                by = c("refcode", "refdesc", "Code", "description"),
                all = TRUE)

fwrite(chk02, 
       "D:/Hospital_data/ProgresSQL/analysis/085_dis_count_edges.csv")



chk01med <- edges [, .(cnt = uniqueN(mr_no)), 
                            by = .(refcode, refdesc, period, periodn, Type_med, Coded_med )]
