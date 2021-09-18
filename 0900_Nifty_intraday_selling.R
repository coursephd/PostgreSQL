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

# Calculate the OHLC values for 15 min blocks
# Open = row =1 
# Close = row = 15
# Min = min (across all rows)
# Max = max (across all rows)

data01 <- data01 [, `:=` (open = ifelse(row02 == 1, V4, ""),
                          close = ifelse(row02 == 1, V7, "") ),]

data01 <- data01 [, `:=`(low = min(V6), high = max(V5)), by = .(V1, V2, group)]

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
