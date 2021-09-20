library(TTR)
library(tidyquant)
library(QuantTools)
library(derivmkts)
library(quantmod)
library(openxlsx)
library(data.table)
library(tidyverse)
library(anytime)
library(simstudy)
library(zoo)
library(RCurl)
library(lubridate)
library(curl)
library(reshape)
library(ggplot2)
library(plotly)

options(scipen = 999)

library(BatchGetSymbols)

# https://stackoverflow.com/questions/3397885/how-do-you-read-multiple-txt-files-into-r

# First get the list of all txt files (including those in sub-folders)
list_of_files <- list.files(path = "D:\\My-Shares\\Intraday-data-Nifty\\", recursive = TRUE,
                            pattern = "\\NIFTY.txt$", 
                            full.names = TRUE)

# Read all the files and create a FileName column to store filenames
data <- rbindlist(sapply(list_of_files, fread, simplify = FALSE, header = FALSE),
                idcol = "FileName")

#data <- fread("D:\\My-Shares\\Intraday-data-Nifty\\2019\\2019 MAY NIFTY.txt", header = FALSE)

# Calculate the 15 in blocks for each date
# create 1 to 15 rows
# create 1 to 25 groups

data01 <- data [ V3 != "09:08"]
data01 <- data01 [, row := 1:.N, by = .(V2)]
data01 <- data01 [, row02 := ifelse(row > 15, row %% 15, row), ]
data01 <- data01 [, row02 := ifelse(row02 == 0, 15, row02), ]
data01 <- data01 [, group := ceiling (row / 15), ]
data01 <- data01 [, maxgroup := max(group), by = .(FileName, V1, V2)]

# Calculate the OHLC values for 15 min blocks
# Open = row =1 
# Close = row = 15
# Min = min (across all rows)
# Max = max (across all rows)

data01 <- data01 [, `:=` (open = ifelse(row02 == 1, V4, ""),
                          close = ifelse(row02 == 1, V7, "") ),]

data01 <- data01 [, `:=`(low = min(V6), high = max(V5)), by = .(V1, V2, group, FileName)]

data02 <- data01 [ row02 == 1, -c("V4", "V5", "V6", "V7", "V8", "V9"), ]
data02 <- data02 [, `:=` (trdate = anydate(V2),
                          open = as.numeric(open), 
                          close = as.numeric(close), 
                          low = as.numeric(low), 
                          high = as.numeric(high) ), ]

# Calculate the width of the candle and check on the height
# Calculate the nearest strike for Low and high

data02 <- data02 [, `:=` (hgt = high - low,
                          ce_strk = ifelse(high < 10000, signif(high, 2), signif(high, 3) ),
                          pe_strk = ifelse(low < 10000, signif(low, 2), signif(low, 3) ) ),  ]

data02spot <- data02

saveRDS (data02spot, "D:\\My-Shares\\Intraday-data-Nifty\\source-nifty\\nifty15mins.rds")
rm(data01)
rm(data02)

# Get the whole data for all the options:
# this file may becom very large, need to see how to cut it to smaller size

# First get the list of all txt files (including those in sub-folders)
list_of_files_opt2019 <- list.files(path = "D:/My-Shares/Intraday-data-Nifty/2019", recursive = TRUE,
                            pattern = "alloptions.txt", 
                            full.names = TRUE)

# Read all the files and create a FileName column to store filenames
data_opt2019 <- rbindlist(sapply(list_of_files_opt2019, fread, simplify = FALSE, header = FALSE, fill = TRUE),
                  idcol = "FileName")

data01_2019 <- data_opt2019 [ V3 != "09:08"]
data01_2019 <- unique( data01_2019 ) # To remove duplicate data, need to check if this is due to my programming
data01_2019 <- data01_2019[, row := 1:.N, by = .(FileName, V1, V2)]
data01_2019 <- data01_2019[, row02 := ifelse(row > 15, row %% 15, row), ]
data01_2019 <- data01_2019[, row02 := ifelse(row02 == 0, 15, row02), ]
data01_2019 <- data01_2019[, group := ceiling (row / 15), ]
data01_2019 <- data01_2019[, maxgroup := max(group), by = .(FileName, V1, V2)]

# Calculate the OHLC values for 15 min blocks
# Open = row =1 
# Close = row = 15
# Min = min (across all rows)
# Max = max (across all rows)

data01_2019 <- data01_2019[, `:=` (open = ifelse(row02 == 1, V4, ""),
                                  close = ifelse(row02 == 1, V7, "") ),]

data01_2019 <- data01_2019[, `:=`(low = min(V6), high = max(V5)), by = .(V1, V2, group, FileName)]

data02 <- data01_2019[ row02 == 1, -c("V4", "V5", "V6", "V7", "V8"), ]
data02 <- data02 [, `:=` (trdate = anydate(V2),
                          open = as.numeric(open), 
                          close = as.numeric(close), 
                          low = as.numeric(low), 
                          high = as.numeric(high) ), ]

setnames (data02, "open", "open_opt")
setnames (data02, "close", "close_opt")
setnames (data02, "low", "low_opt")
setnames (data02, "high", "high_opt")

###############################################
#
# Create expiry dates
# Merge the dates back onto the whole file
#
###############################################

file001 <- unique( data02 [ ,c ("FileName"), ])
file001 <- file001 [, c("tmp001", "tmp002", "tmp003", "tmp004", "tmp005", "tmp006") := tstrsplit(FileName, "/"), ]
file001 <- file001 [, tmp005 := str_replace(tmp005, "(Starting Week)", ""), ]
file001 <- file001 [, tmp005 := str_replace(tmp005, "-General Elections Results Week", ""), ]
file001 <- file001 [, tmp005 := str_replace(tmp005, "\\(", ""), ]
file001 <- file001 [, tmp005 := str_replace(tmp005, "\\)", ""), ]

file001 <- file001 [, c("dt01", "dt02", "dt03") := tstrsplit(tmp005, " "), ]
file001 <- file001 [, expdt := anydate( paste( substr(dt02, 1, 2), dt03, tmp004 ) ), ]

data02 <- merge (x = data02,
                 y = file001 [, c("FileName", "expdt"), ],
                 by = c("FileName"))

# Calculate the days between trading days and the expiry day
data02 <- data02 [, numdays := as.numeric(expdt - trdate + 1), ]

# Merge the Spot data based on the dates

data03 <- merge (x = data02, 
                 y = data02spot [group == 2, c("trdate", "open", "high", "low", "close", "ce_strk", "pe_strk"), ], 
                 by = c ("trdate"),
                 all.x = TRUE)

# Keep only <= 7 days to expiry in the dataset
data04 <- data03 [ numdays <= 7]
data04 <- data04 [, strlen := str_length(V1), ]
data04 <- data04 [, strikes02 := ifelse(strlen == 19, substr(V1, 13, 17), ""), ]
data04 <- data04 [, strikes02 := ifelse(strlen <= 14, parse_number(V1), strikes02), ]
data04 <- data04 [, strikes02 := as.numeric(strikes02), ]
data04 <- data04 [, callput := case_when ( strlen ==8 ~ substr(V1, 1, 2), 
                                           strlen > 8 ~ substr(V1, strlen -1, strlen),
                                           TRUE ~ "Other"), ]

# Keep +/- 300 on day 7
# Keep +/- 250 on day 6
# Keep +/- 200 on day 5

data04 <- data04 [, range := case_when( numdays == 1 ~ 100,
                                        numdays == 2 ~ 150,
                                        numdays == 3 ~ 200,
                                        numdays == 4 ~ 250,
                                        numdays >= 5 ~ 300), ]

# Keep only records which fall in ce_strk + range and pe_strk - range
data05ce <- data04 [ strikes02 <= ce_strk + range & strikes02 >= ce_strk & callput == "CE" ]
data05pe <- data04 [ strikes02 >= pe_strk - range & strikes02 <= pe_strk & callput == "PE" ]
data05cepe <- rbind(data05ce, data05pe)

data05cepe <- data05cepe [, ce_distance := strikes02 - ce_strk, ]
data05cepe <- data05cepe [, pe_distance := strikes02 - pe_strk, ]

saveRDS (data05cepe, "D:\\My-Shares\\Intraday-data-Nifty\\source-nifty\\nifty_opt2019_15mins_7days.rds")
fwrite (data05cepe, "D:\\My-Shares\\Intraday-data-Nifty\\source-nifty\\nifty_opt2019_15mins_7days.csv")
rm(data_opt2019)

data05cepe <- readRDS("nifty_opt2019_15mins_7days.rds")

 

chk <- data04 [ expdt == "2019-12-26"]

####################################
#
# Same calculations for 2021
#
####################################
list_of_files_opt2021 <- list.files(path = "D:/My-Shares/Intraday-data-Nifty/2021", recursive = TRUE,
                                    pattern = "alloptions.txt", 
                                    full.names = TRUE)

# Read all the files and create a FileName column to store filenames
data_opt2021 <- rbindlist(sapply(list_of_files_opt2021, fread, simplify = FALSE, header = FALSE, fill = TRUE),
                          idcol = "FileName")

data01_2021<- data_opt2021 [ V3 != "09:08"]
data01_2021<- data01_2021[, row := 1:.N, by = .(V2)]
data01_2021<- data01_2021[, row02 := ifelse(row > 15, row %% 15, row), ]
data01_2021<- data01_2021[, row02 := ifelse(row02 == 0, 15, row02), ]
data01_2021<- data01_2021[, group := ceiling (row / 15), ]

# Calculate the OHLC values for 15 min blocks
# Open = row =1 
# Close = row = 15
# Min = min (across all rows)
# Max = max (across all rows)

data01_2021<- data01_2021[, `:=` (open = ifelse(row02 == 1, V4, ""),
                                  close = ifelse(row02 == 1, V7, "") ),]

data01_2021<- data01_2021[, `:=`(low = min(V6), high = max(V5)), by = .(V1, V2, group)]

data02 <- data01_2021[ row02 == 1, -c("V4", "V5", "V6", "V7", "V8"), ]
data02 <- data02 [, `:=` (trdate = anydate(V2),
                          open = as.numeric(open), 
                          close = as.numeric(close), 
                          low = as.numeric(low), 
                          high = as.numeric(high) ), ]

###############################################
#
# Create expiry dates
# Merge the dates back onto the whole file
#
###############################################

file001 <- unique( data02 [ ,c ("FileName"), ])
file001 <- file001 [, c("tmp001", "tmp002", "tmp003", "tmp004", "tmp005", "tmp006") := tstrsplit(FileName, "/"), ]
file001 <- file001 [, c("dt01", "dt02", "dt03") := tstrsplit(tmp005, " "), ]
file001 <- file001 [, expdt := anydate( paste( substr(dt02, 1, 2), dt03, tmp004 ) ), ]

data02 <- merge (x = data02,
                 y = file001 [, c("FileName", "expdt"), ],
                 by = c("FileName"))

# Calculate the days between trading days and the expiry day
data02 <- data02 [, numdays := as.numeric(expdt - trdate + 1), ]

saveRDS (data02, "D:\\My-Shares\\Intraday-data-Nifty\\source-nifty\\nifty_opt2021_15mins.rds")
rm(data_opt2021)


# Get the yearly, monthly, weekly, daily values:
styrdate <- as.numeric(as.POSIXct("2019-01-01", format="%Y-%m-%d"))
endaydate <- as.numeric(as.POSIXct("2019-04-22", format="%Y-%m-%d")) # Vinay update 9th Jan 2021

url_19 <- paste('https://in.investing.com/indices/india-vix-historical-data?end_date=', endaydate, '&st_date=', styrdate, '&interval_sec=monthly&interval_sec=daily', sep=''); 
url_19_html <- read_html(url_19); 
url_19_whole  <- url_19_html  %>% html_nodes('table') %>% html_table(fill = TRUE) %>% .[[2]]; 
url_19_whole  <- as.data.table(url_19_whole)
