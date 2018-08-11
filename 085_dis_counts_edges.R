library(data.table)
library(stringi)
library(stringr)
library(sqldf)
library(tidyr)

edges <- readRDS("D:/Hospital_data/ProgresSQL/analysis/085_dis_1st_time_refCal_NodesEdges.rds")

###########################################################
# Get the unique number of reference diseases and medicines
###########################################################
unqref <- unique( edges [, c("refcode", "refdesc")])

dismed <- unique( edges [, c("cat", "Code", "description"), ])
dismed <- dismed [nchar(Code) > 0] [order(cat, Code, description)]
dismed <- dismed [, `:=` (npoints = 1:.N, tot = .N), by = .(cat)]
dismed <- dismed [, `:=` (radius = ifelse (cat == "Disease", 20, 40), 
                          angle = 360 / tot), ]
dismed <- dismed [, cumulative := cumsum(angle), by = .(cat)]

################################################
# Function for the degrees and radian conversion
################################################
deg2rad <- function(deg) {(deg * pi) / (180)}

dismed <- dismed [, radian := deg2rad(cumulative),]
dismed <- dismed [, `:=` (xaxis = cos(radian)*radius,
                          yaxis = sin(radian)*radius), ]

########################################################
# create a complete dataset
# this is to ensure, circle is displayed all the time
########################################################
dismed_all <- crossing(unqref, dismed)

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
