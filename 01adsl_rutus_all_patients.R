####################################################
#
# All patients ~50,000 with rutus data
#
####################################################

library(data.table)
library(tidyverse)
library(readxl)
library(stringi)
library(stringr)
library(anytime)

adsl <- fread("D:\\Hospital_data\\ProgresSQL\\analysis\\01adsl\\01adsl.csv")
adsl <- adsl [, newdt0 := anydate(newdt), ]

# Add Indian rutus as new variables
# https://www.drikpanchang.com/seasons/season-tropical-timings.html?geoname-id=1277333&year=2010

rutus <- fread("D:/Hospital_data/ProgresSQL/analysis/rutus.csv")
rutus <- rutus [, `:=`(startdt = as.POSIXct( startdate, format="%d-%m-%Y"), 
                       enddt = as.POSIXct( enddate, format="%d-%m-%Y")) ]

rutus02 <- rutus[ , list(season = season, year = year,
                         newdt0 = anydate( seq(startdt, enddt, by = "day") )), by = 1:nrow(rutus)]

all <- merge (x = adsl,
              y = rutus02 [, c("newdt0", "year", "season")],
              by = c("newdt0"),
              all.x = TRUE)

fwrite(all, "D:\\Hospital_data\\ProgresSQL\\analysis\\01adsl\\01adsl_rutus.csv")
saveRDS(all, "D:\\Hospital_data\\ProgresSQL\\analysis\\01adsl\\01adsl_rutus.rds")
