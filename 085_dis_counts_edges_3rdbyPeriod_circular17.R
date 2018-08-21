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

####################################################
# Get unique diseases in RMSD and Metabolic
# Keep refcode from these 2 areas 107 unique values
####################################################
discat <- unique( all_met_rmsd02 [distype %in% c("RMSD", "Metabolic"), c("Code", "description"), ])

###########################################################
# Get the unique number of reference diseases and medicines
# Create this for each of the periods before and after 1st 
# occurrence of the disease
###########################################################
unqref <- unique( edges [, c("period", "periodn")])

dismed <- unique( edges [, c("cat", "refcode", "refdesc", "Code", "description"), ])
dismed <- dismed [nchar(Code) > 0] [order(cat, refcode, refdesc, Code, description)]
dismed <- dismed [, `:=` (npoints = 1:.N, tot = .N), by = .(cat, refcode, refdesc)]

########################################################
# create a complete dataset
# this is to ensure, circle is displayed all the time
# Combine with the individual period for replication
########################################################
dismed <- as.data.table(crossing(unqref, dismed))

###########################################################
# Create a radius for each period for disease and medicine
# Rather than the tableau layout being trellis, let us try
# the output with 17 * 2 = 34 circles
# 1 circle each for disease and medicine for each timepoint
###########################################################
dismed <- as.data.table(sqldf("select *, 
              case 
              When cat == 'Disease' AND period == -9 then 10
              When cat == 'Disease' AND period == -8 then 30
              When cat == 'Disease' AND period == -7 then 50
              When cat == 'Disease' AND period == -6 then 70
              When cat == 'Disease' AND period == -5 then 90
              When cat == 'Disease' AND period == -4 then 110
              When cat == 'Disease' AND period == -3 then 130
              When cat == 'Disease' AND period == -2 then 150
              When cat == 'Disease' AND period == -1 then 170
              When cat == 'Disease' AND period == 1 then 190
              When cat == 'Disease' AND period == 2 then 210
              When cat == 'Disease' AND period == 3 then 230
              When cat == 'Disease' AND period == 4 then 250
              When cat == 'Disease' AND period == 5 then 270
              When cat == 'Disease' AND period == 6 then 290
              When cat == 'Disease' AND period == 7 then 310
              When cat == 'Disease' AND period == 8 then 330
              When cat == 'Disease' AND period == 9 then 350

              When cat == 'Medicine' AND period == -9 then 20
              When cat == 'Medicine' AND period == -8 then 40
              When cat == 'Medicine' AND period == -7 then 60
              When cat == 'Medicine' AND period == -6 then 80
              When cat == 'Medicine' AND period == -5 then 100
              When cat == 'Medicine' AND period == -4 then 120
              When cat == 'Medicine' AND period == -3 then 140
              When cat == 'Medicine' AND period == -2 then 160
              When cat == 'Medicine' AND period == -1 then 180
              When cat == 'Medicine' AND period == 1 then 200
              When cat == 'Medicine' AND period == 2 then 220
              When cat == 'Medicine' AND period == 3 then 240
              When cat == 'Medicine' AND period == 4 then 260
              When cat == 'Medicine' AND period == 5 then 280
              When cat == 'Medicine' AND period == 6 then 300
              When cat == 'Medicine' AND period == 7 then 320
              When cat == 'Medicine' AND period == 8 then 340
              When cat == 'Medicine' AND period == 9 then 360
                
              end as radius
              from dismed") )

dismed <- dismed [, `:=` (angle = 360 / tot), ]
dismed <- dismed [, cumulative := cumsum(angle), by = .(cat, refcode, refdesc)]

################################################
# Function for the degrees and radian conversion
################################################
deg2rad <- function(deg) {(deg * pi) / (180)}

dismed <- dismed [, radian := deg2rad(cumulative),]
dismed_all <- dismed [, `:=` (xaxis = cos(radian)*radius,
                          yaxis = sin(radian)*radius), ]

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

chk03all <- rbind(path01, dismed_all, fill = TRUE)
chk04all <- chk03all [ refcode %in% discat$Code]

fwrite(chk04all, 
       "D:/Hospital_data/ProgresSQL/analysis/085_dis_count_edges_3rd_byPeriod_circular17.csv")

#########################################################################################
