###################################################################################
# 087_epidemiology_prv_rate.R
# Calculate capn = Number of patients present in each of Indian rutus
# Calculate smalln = Number of patients present with a disease in each Indian rutu
# Calculate the %s smalln / capn -- prevalence rate for each disease for each rutu
###################################################################################

library(openxlsx)
library(tidyverse)
library(tidytext)
library(stringi)
library(data.table)
library(stringdist)
library(scales)
library(anytime)

all_met_rmsd <- readRDS("D:/Hospital_data/ProgresSQL/analysis/01adsl_met_rmsd.rds")

capn <- unique(all_met_rmsd [, c("mr_no", "cstdt", "cendt", "cdur"), ])

capn02 <- capn[ , list(cdur = cdur, mr_no = mr_no,
                       newdt0 = anydate( seq(as.numeric(cstdt), as.numeric(cendt)) ) ), 
                by = 1:nrow(capn)]

rutus <- fread("D:/Hospital_data/ProgresSQL/analysis/rutus.csv")
rutus <- rutus [, `:=`(startdt = as.POSIXct( startdate, format="%d-%m-%Y"), 
                       enddt = as.POSIXct( enddate, format="%d-%m-%Y")) ]

rutus02 <- rutus[ , list(season = season, year = year,
                         newdt0 = anydate( seq(startdt, enddt, by = "day") )), by = 1:nrow(rutus)]

capn02 <- merge (x = capn02,
                       y = rutus02 [, c("newdt0", "year", "season")],
                       by = c("newdt0"),
                       all.x = TRUE)


capn03sns <- capn02 [, .(capn = uniqueN(mr_no)), by = .(season)]
capn03sns_yr <- capn02 [, .(capn = uniqueN(mr_no)), by = .(year, season)]

rm(capn02)



smalln <- unique(all_met_rmsd [, c("mr_no", "newdt0", "Code", "description", "year", "season"), ])
smalln <- smalln [, Code := ifelse (Code == " " | Code == "", "** Not yet coded", Code),]
smalln <- smalln [, description:= ifelse (description == "" | description ==" ", "** Not yet coded", description),]

small03sns <- smalln [, .(smalln = uniqueN(mr_no)), by = .(season, Code, description)]
small03sns_yr <- smalln [, .(smalln = uniqueN(mr_no)), by = .(year, season, Code, description)]

all01sns <- merge(x = small03sns, 
                  y = capn03sns, 
                  by = c("season"))

all01sns_yr <- merge(x = small03sns_yr, 
                  y = capn03sns_yr, 
                  by = c("year", "season"))

all01sns <- all01sns[, perc := percent(smalln / capn), ]
all01sns_yr <- all01sns_yr[, perc := percent(smalln / capn), ]


add01sns <- all01sns [, .(chk01 = sum(smalln)), by = .(Code, description)]
add01sns_yr <- all01sns_yr [, .(chk01 = sum(smalln)), by = .(Code, description)]