
library(data.table)
library(stringi)
library(stringr)
library(sqldf)
library(scales)
library(tscount)
library(lubridate)

all_met_rmsd02 <- readRDS ("D:/Hospital_data/ProgresSQL/analysis/01adsl_met_rmsd.rds")
all_met_rmsd02 <- all_met_rmsd02 [, Code := ifelse (Code == " " | Code == "", "** Not yet coded", Code),]
all_met_rmsd02 <- all_met_rmsd02 [, description:= ifelse (description == "" | description ==" ", "** Not yet coded", description),]
all_met_rmsd02 <- all_met_rmsd02 [, year := year(newdt), ]

count <- all_met_rmsd02 [, .(npat = uniqueN(mr_no)), by = .(year, season, Code, description)]
count <- count [, season_num := as.numeric(substr(season, 2, 3)), ]

tsglm(count$npat, model = list(past_obs = 1), distr = "poisson")




all_met_rmsd02 <- readRDS ("D:/Hospital_data/ProgresSQL/analysis/all_met_rmsd02.rds")
all_met_rmsd02 <- all_met_rmsd02 [, Code := ifelse (Code == " " | Code == "", "** Not yet coded", Code),]
all_met_rmsd02 <- all_met_rmsd02 [, description:= ifelse (description == "" | description ==" ", "** Not yet coded", description),]
all_met_rmsd02 <- all_met_rmsd02 [, year := year(newdt0), ]

count <- all_met_rmsd02 [, .(npat = uniqueN(mr_no)), by = .(year, season, Code, description)]
count <- count [, season_num := as.numeric(substr(season, 2, 3)), ]

tsglm(count$npat, model = list(past_obs = 1), distr = "poisson")


a08all <- readRDS ("D:/Hospital_data/ProgresSQL/analysis/080_medcine_repeat_prop_addnl.rds")


all_met_rmsd02_mr <- all_met_rmsd02 [ mr_no == "MR000008"]

a08all_mr <- a08all [ mr_no == "MR046575"]

a08all_mr <- a08all [ bothnum_cal == disnum_cal & bothnum_cal == dosnum_cal & bothnum_cal >= 2]
a08all_mr02 <- a08all_mr [ substr(Code, 1, 2) != "**"]

all_data <- merge ( x = all_met_rmsd02_mr,
                    y = a08all_mr,
                    by = c("mr_no", "Code", "description", "Type_med", "Coded_med", "studyday",
                           "patient_gender", "baseage", "age", "combine", "Metabolic", "RMSD",
                           "newdt0", "distype"))