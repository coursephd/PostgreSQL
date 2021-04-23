library(data.table)
library(tidyverse)
library(lubridate)
library(anytime)
library(bizdays)
library(rvest)
library(xml2)
library(openxlsx)
library(RCurl)
library(curl)

library(ggplot2)
library(ggpubr)
library(plotly)

library(TTR)
library(tidyquant)
library(QuantTools)
library(derivmkts)
library(quantmod)

options(scipen = 999)

#
# Extract only specific files 
#
# https://stackoverflow.com/questions/32870863/extract-certain-files-from-zip
# https://stackoverflow.com/questions/32870863/extract-certain-files-from-zip
# https://stackoverflow.com/questions/31146263/sys-glob-within-unzip

#########################################################
#
# Step 1:
#
# Download the files from the nse website:
#
# Example calls of the data download
# Check for the existence of the file
# if it exists then only download
#
# if (curl_fetch_memory('https://www1.nseindia.com/archives/equities/bhavcopy/pr/PR010311.zip')$status_code == 200) download.file('https://www1.nseindia.com/archives/equities/bhavcopy/pr/PR010311.zip', 'D:\\My-Shares\\source-bhavcopy\\PR010311.zip')
# if (curl_fetch_memory('https://www1.nseindia.com/archives/equities/bhavcopy/pr/PR020311.zip')$status_code == 200) download.file('https://www1.nseindia.com/archives/equities/bhavcopy/pr/PR020311.zip', 'D:\\My-Shares\\source-bhavcopy\\PR020311.zip')
# if (curl_fetch_memory('https://www1.nseindia.com/archives/equities/bhavcopy/pr/PR030311.zip')$status_code == 200) download.file('https://www1.nseindia.com/archives/equities/bhavcopy/pr/PR030311.zip', 'D:\\My-Shares\\source-bhavcopy\\PR030311.zip')
# if (curl_fetch_memory('https://www1.nseindia.com/archives/equities/bhavcopy/pr/PR040311.zip')$status_code == 200) download.file('https://www1.nseindia.com/archives/equities/bhavcopy/pr/PR040311.zip', 'D:\\My-Shares\\source-bhavcopy\\PR040311.zip')
#
# The following statement executes the actual column 
# eval(parse (text = variable name))
#
#########################################################

step001 <- data.table ( read.xlsx("D:\\My-Shares\\prgm\\0500_rakeshpujara_atmlong.xlsx", 2) )
step002 <- step001 [Date <= 40612]
step003 <- step002 [, dwn := eval(parse(text = download)),]

#########################################################
#
# Step 2:
#
# Unzip the files for the 1st time as the fno file is inside the BIG file 
#
# Check if the zip file exists then unzip
#
# if file.exists("leaflet.R") unzip
#
# Example calls
# if (file.exists('D:\\My-Shares\\source-bhavcopy\\PR010311.zip')) unzip(zipfile = 'D:\\My-Shares\\source-bhavcopy\\PR010311.zip', exdir = 'D:/My-Shares/source-fno')
# if (file.exists('D:\\My-Shares\\source-bhavcopy\\PR020311.zip')) unzip(zipfile = 'D:\\My-Shares\\source-bhavcopy\\PR020311.zip', exdir = 'D:/My-Shares/source-fno')
# if (file.exists('D:\\My-Shares\\source-bhavcopy\\PR030311.zip')) unzip(zipfile = 'D:\\My-Shares\\source-bhavcopy\\PR030311.zip', exdir = 'D:/My-Shares/source-fno')
# if (file.exists('D:\\My-Shares\\source-bhavcopy\\PR040311.zip')) unzip(zipfile = 'D:\\My-Shares\\source-bhavcopy\\PR040311.zip', exdir = 'D:/My-Shares/source-fno')
#
# The following step creates some warning messages but 
# the unzipping of the files takes place
# If this could be sorted out then try
#
#########################################################

step003 <- step003 [, eval(parse(text = unzip1)),]

#########################################################
#
# Step 3:
#
# Now unzip the fno file which was created in step 2
#
#########################################################

step004 <- step002 [, eval(parse(text = unzip_fno_csv)),]

#########################################################
#
# Step 4:
#
# Read only few lines from the CSV files into R data.table
# https://readr.tidyverse.org/reference/read_delim_chunked.html
#
# The futures data should be read into fo files
# The options data should be read into op files
#
# if (file.exists('D:\\My-Shares\\source-fno-csv\\fo01032011.csv')) fo01032011 = data.table (read_csv_chunked('D:\\My-Shares\\source-fno-csv\\fo01032011.csv', DataFrameCallback$new(f), chunk_size = 5))
# if (file.exists('D:\\My-Shares\\source-fno-csv\\op01032011.csv')) op01032011 = data.table (read_csv_chunked('D:\\My-Shares\\source-fno-csv\\op01032011.csv', DataFrameCallback$new(f), chunk_size = 5))
#
#########################################################
f <- function(x, pos) subset(x, SYMBOL %in% c("BANKNIFTY", "NIFTY") )
#fo <- data.table (read_csv_chunked("D:\\My-Shares\\source-fno-csv\\fo*.csv", DataFrameCallback$new(f), chunk_size = 5) )

step005 <- step002 [, eval(parse(text = fut_data)),]
step006 <- step002 [, eval(parse(text = opt_data)),]

#####################################################################################
#
# End of program
#
#####################################################################################
