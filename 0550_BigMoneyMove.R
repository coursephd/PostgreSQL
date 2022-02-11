###################################################################################################
#
# Bhavcopy:
#  https://www1.nseindia.com/content/historical/DERIVATIVES/2022/FEB/fo08FEB2022bhav.csv.zip
#
# % Delivery data:
#  
#  https://www1.nseindia.com/archives/equities/mto/MTO_08022022.DAT
#
# Number of trades and number of shares traded:
#  
#  https://www1.nseindia.com/content/historical/EQUITIES/2022/FEB/cm08FEB2022bhav.csv.zip
#
# Combine OI:
#  
#  https://www1.nseindia.com/archives/nsccl/mwpl/combineoi_08022022.zip
#
# FPI data:
#  
#  https://www.fpi.nsdl.co.in/web/Reports/FPI_Fortnightly_Selection.aspx
#
# BSE list of securities, the file will have to be downloaded, cannot be read from the site:
#  
#  https://www.bseindia.com/corporates/List_Scrips.html
#
# https://www1.nseindia.com/products/content/derivatives/equities/archieve_fo.htm
#
###################################################################################################
library(data.table)
library(tidyverse)
library(anytime)
library(derivmkts)
library(quantmod)
library(simstudy)
library(zoo)
library(RCurl)
library(lubridate)
library(curl)
library(tidyquant) 

options(scipen = 999)

# from today to 2 years behind
tday <- Sys.Date()
#tday <- "2022-12-31"
lastday <- "2022-01-01"
########################################################################################
# Section 3
#
# Use NSE data:
# Get the bhavcopy data for at least 5 days and pick up the latest value for 
# options premiums - in turn calculate the Implied volatility
#
# All the code for the extraction from NSE website
# It takes time to extract so re-thinking about this part
# But is needed for the Last day's Contract values for the IV calculation
#
########################################################################################

#dates <- seq ( anydate(tday), anydate(tday) - 364, by=-1)
dates <- seq ( anydate(tday), anydate(lastday), by=-1)

mon <- toupper(format(anydate(dates),"%b"))
mon_num <- format(anydate(dates),"%m")
day <- toupper(format(anydate(dates),"%d"))
yr <- toupper(format(anydate(dates),"%Y"))

monyr <- paste(yr, "/", mon, sep="")

# For bhav: 2022/FEB/cm08FEB2022bhav.csv.zip
date_char <- paste("cm", toupper(format(dates, "%d%b%Y")), "bhav.csv", sep ="")

# For other files
date_num <- paste(day, mon_num, yr, sep="")

n <- length(dates)

csvpath <- "D:/My-Shares/source-fno-csv/"

for (i in 1:n) { 
  
  url <- paste("https://www1.nseindia.com/content/historical/EQUITIES/", monyr[i], "/", date_char[i], ".zip", sep="")
  csvpath0 <- paste(csvpath, yr[i], sep="")
  zipfile0 <- paste(csvpath0, "/", date_char[i], ".zip", sep="")
  csvfile0 <- paste(csvpath0, "/", date_char[i], sep="")
  req <- curl_fetch_memory(url)
  
  print(i)
  print(csvpath0)
  print(zipfile0)
  print(csvfile0)
  print(url)
  print(req$status_code)
  
  if (req$status_code == 200) {
    download.file(url, zipfile0)
    unzip(zipfile = zipfile0, exdir = csvpath0)
  }
  
  url_coi <- paste("https://www1.nseindia.com/archives/nsccl/mwpl/combineoi_", day[i], mon_num[i], yr[i], ".zip", sep="")
  coi_data <- paste("combinecoi_", day[i], mon_num[i], yr[i], sep="")
  coizipfile0 <- paste(csvpath0, "/", coi_data, ".zip", sep="")
  coifile0 <- paste(csvpath0, "/", coi_data, ".csv", sep="")
  req_coi <- curl_fetch_memory(url_coi)
  
  print(url_coi)
  print(coi_data)
  print(coizipfile0)
  print(coifile0)
  print(req_coi$status_code)
  
  if (req_coi$status_code == 200) {
    download.file(url_coi, coizipfile0)
    unzip(zipfile = coizipfile0, exdir = csvpath0)
  }

  url_del <- paste("https://www1.nseindia.com/archives/equities/mto/MTO_", day[i], mon_num[i], yr[i], ".DAT", sep="")
  del_data <- paste("delivery", day[i], mon_num[i], yr[i], sep="")
  del_date <- paste(yr[i], mon_num[i], day[i], sep="-")
  req_del <- curl_fetch_memory(url_del)

  print(url_del)
  print(del_data)
  print(req_del$status_code)
  
  if (req$status_code == 200) {
  temp <- fread(url_del, skip =4)
  temp <- temp [V4 == "EQ"]
  temp <- temp [, trdate := anydate( del_date ), ]

  #Record Type,
  #Sr No,
  #Name of Security,
  #Quantity Traded,
  #Deliverable Quantity(gross across client level),
  #% of Deliverable Quantity to Traded Quantity
  
  setnames(temp, "V3", "ticker")
  setnames(temp, "V5", "trdqty")
  setnames(temp, "V6", "delqty")
  setnames(temp, "V7", "delperc")
  
  assign(del_data, temp)
  
  }
  del_all <<- rbindlist(mget(ls(pattern = "^delivery")), fill = TRUE, idcol = "file_del")  
}
rm(list = ls( pattern = "^delivery") )

list_of_files <- list.files(path = csvpath0, recursive = TRUE, pattern = "^cm.*csv$", full.names = TRUE)
f <- function(x, pos) subset(x, SERIES == "EQ" )
dt_chunk <- rbindlist(sapply(list_of_files, read_csv_chunked, DataFrameCallback$new(f), simplify = FALSE), fill = TRUE)

dt_chunk <- dt_chunk [, `:=`(trdate = anydate( dmy(TIMESTAMP) )),]
dt_chunk <- dt_chunk [, nshares_trade := as.numeric( round( TOTTRDQTY / TOTALTRADES, 0) ), ]
setnames(dt_chunk, "SYMBOL", "ticker")

list_of_filescoi <- list.files(path = csvpath0, recursive = TRUE, pattern = "^combineoi.*csv$", full.names = TRUE)
f2 <- function(x, pos) subset(x, 1 == 1 )
dt_chunkcoi <- rbindlist(sapply(list_of_filescoi, read_csv_chunked, DataFrameCallback$new(f2), simplify = FALSE), fill = TRUE)

setnames(dt_chunkcoi, "Scrip Name", "ScripName")
setnames(dt_chunkcoi, "NSE Symbol", "ticker")
setnames(dt_chunkcoi, "Open Interest" , "coi")

dt_chunkcoi <- dt_chunkcoi [, trdate := anydate( dmy(Date) ), ]

all02 <- Reduce(function(...) merge(..., by = c("ticker", "trdate"), all=T),
                list( del_all, dt_chunk, dt_chunkcoi [, -c("ISIN"), ]) )

all03 <- all02 [ coi != ""]

all04 <- all03 [, c("ticker", "ISIN", "trdate", "ScripName", "delperc", "coi", "OPEN", "CLOSE", "LOW", "HIGH", "nshares_trade" ), ]

# Calculate 10, 20, 30 day moving average for delperc, coi, nshares_trade
all04 <- all04 [, `:=`(delperc10sma = SMA( as.numeric(delperc), 10),
                       coi10sma = SMA(coi, 10),
                       nshares_trade10sma = SMA(nshares_trade, 10),
                       close10sma = SMA (CLOSE, 10)), by = .(ticker)]

do.call(file.remove, list(list.files(path = csvpath0, pattern = "^*.*csv$", full.names = TRUE)))
do.call(file.remove, list(list.files(path = csvpath0, pattern = "^*.*xml$", full.names = TRUE)))



#saveRDS(dt_chunk, paste(csvpath, "source/dt_chunk2022.rds", sep="") )
#fwrite(dt_chunk, paste(csvpath, "source/dt_chunk2022.csv", sep=""))
