#######################################################
# Disease trajectory on human body
# 086_dis_indtrajectory.csv
#######################################################

library(openxlsx)
library(tidyverse)
library(tidytext)
#library(stringr)
library(stringi)
library(data.table)
library(stringdist)
library(scales)

pos <- read.xlsx("D:/Hospital_data/ProgresSQL/misc/HumanBody/HumanBody02.xlsx")

traj <- fread("D:/Hospital_data/ProgresSQL/analysis/086_dis_indtrajectory.csv")
traj <- traj [, orig := combdesc,]

traj0 <- separate_rows(traj, orig, sep =",")
traj01 <- merge( x= traj0,
                 y = pos,
                 by.x = c("orig"),
                 by.y = c("Description"))

before <- copy(traj01)
before <- before [, -c("X2", "Y2"),]

after <- copy(traj01)
after <- after [, -c("X", "Y"),]

setnames (before, "scr_Before", "scr")
setnames (before, "total_Before", "tot")

setnames (after, "scr_After", "scr")
setnames (after, "total_After", "tot")
setnames (after, "X2", "X")
setnames (after, "Y2", "Y")


data03 <- rbind (before, after, fill = TRUE)
data03 <- data03 [, -c("scr_Before", "total_Before", "scr_After", "total_After"),]

data04 <- data03[scr > 0 ]

fwrite(data04, "D:/Hospital_data/ProgresSQL/analysis/086_dis_traj_humanbody.csv")
