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
# NSDL: https://www.fpi.nsdl.co.in/web/Reports/FPI_Fortnightly_Selection.aspx
# CDSL: https://www.cdslindia.com/Publications/ForeignPortInvestor.html
#
# BSE list of securities, the file will have to be downloaded, cannot be read from the site:
#  
#  https://www.bseindia.com/corporates/List_Scrips.html
#
# https://www1.nseindia.com/products/content/derivatives/equities/archieve_fo.htm
#
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
tday <- "2021-12-31"
lastday <- "2021-01-01"
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

saveRDS(all03, "D:\\My-Shares\\source-fno-csv\\2021\\all2021.rds")

all04 <- all03 [, c("ticker", "ISIN", "trdate", "ScripName", "delperc", "coi", "OPEN", "CLOSE", "LOW", "HIGH", "nshares_trade" ), ]

# Calculate 10, 20, 30 day moving average for delperc, coi, nshares_trade
all04 <- all04 [, `:=`(delperc10sma = SMA( as.numeric(delperc), 10),
                       coi10sma = SMA(coi, 10),
                       nshares_trade10sma = SMA(nshares_trade, 10),
                       close10sma = SMA (CLOSE, 10)), by = .(ticker)]

do.call(file.remove, list(list.files(path = csvpath0, pattern = "^*.*csv$", full.names = TRUE)))
do.call(file.remove, list(list.files(path = csvpath0, pattern = "^*.*xml$", full.names = TRUE)))


all2021 <- readRDS("D:\\My-Shares\\source-fno-csv\\2021\\all2021.rds")

all04 <- all2021 [, c("ticker", "ISIN", "trdate", "ScripName", "delperc", "coi", "OPEN", "CLOSE", "LOW", "HIGH", "nshares_trade" ), ]

all04 <- all04 [, totrow := .N, by = .(ticker)]
all05 <- all04 [ totrow >= 10]


all05_t <- melt(data = all05,
                id.vars = c("ticker", "ISIN", "ScripName", "trdate", "OPEN", "HIGH", "LOW", "CLOSE", "delperc", "coi", "nshares_trade"), 
                measure.vars = c("coi", "nshares_trade", "CLOSE"))

all05_t <- all05_t [, `:=`(lag01 = shift(value, n=1, type = c("lag")),
                           lag03 = shift(value, n=3, type = c("lag")),
                           lag05 = shift(value, n=5, type = c("lag")),
                           lag07 = shift(value, n=7, type = c("lag")),
                           lag10 = shift(value, n=10, type = c("lag")) ), by = .(ticker, variable)]

all06_t <- melt(data = all05_t,
                id.vars = c("ticker", "ISIN", "ScripName", "trdate", "OPEN", "HIGH", "LOW", "CLOSE", "delperc", "coi", "nshares_trade",  "variable", "value"),
                measure.vars = c("lag01", "lag03", "lag05", "lag07", "lag10"),
                variable.name = c("category"),
                value.name = c("cat_value") )

all06_t <- all06_t [, days := as.numeric( str_sub(category, 4, 5) ), ]
all06_t <- all06_t [, perchg := round( (value - cat_value) / value * 100, 2), ]
all06_t <- all06_t [, chg := value - cat_value, ]

all07_t <- dcast(data = all06_t,
                 ticker + ISIN + ScripName + trdate + OPEN + HIGH + LOW + CLOSE + delperc + coi + nshares_trade + category ~ variable,
                 value.var = c("perchg", "chg") )

all07_t <- all07_t [, logic := case_when(perchg_CLOSE >= 0 & perchg_coi >= 0 ~ "Long Buildup - Bullish",
                                         perchg_CLOSE >= 0 & perchg_coi < 0 ~ "Short covering - Bullish",
                                         perchg_CLOSE < 0 & perchg_coi >= 0 ~ "Short Buildup - Bearish",
                                         perchg_CLOSE < 0 & perchg_coi < 0 ~ "Long unwinding - Bearish"), ]

all07_t <- all07_t [, totrow := .N, by = .(ticker)]
all08_t <- all07_t [ totrow >= 10]


# Calculate 10, 20, 30 day moving average for delperc, coi, nshares_trade
all08_t <- all08_t [, `:=`(delperc10ema = EMA( as.numeric(delperc), 10),
                       coi10ema = EMA(coi, 10),
                       nshares_trade10ema = EMA(nshares_trade, 10),
                       close10ema = EMA (CLOSE, 10)), by = .(ticker)]

all08_t <- all08_t [, opportunity := case_when( nshares_trade >= nshares_trade10ema & delperc >= delperc10ema ~ "JACKPOT",
                                                nshares_trade < nshares_trade10ema & delperc >= delperc10ema ~ "^", 
                                                delperc < delperc10ema ~ "-", 
                                                TRUE ~ ""), ]

# https://in.tradingview.com/script/f32HlgbI/

all2021 <- all2021 [, totrow := .N, by = .(ticker)]
all2021 <- all2021 [ totrow >= 20 ]
all2021 <- all2021 [, avgprice := TOTTRDVAL / TOTTRDQTY, ]

# // BUYING VOLUME AND SELLING VOLUME //
# BV = iff( (high==low), 0, volume*(close-low)/(high-low))
# SV = iff( (high==low), 0, volume*(high-close)/(high-low))
# vol = iff(volume > 0, volume, 1)
# TP = BV + SV

all2021 <- all2021 [, BV := ifelse(HIGH == LOW, 0, TOTTRDQTY * (CLOSE - LOW) / (HIGH - LOW) ), ]
all2021 <- all2021 [, SV := ifelse(HIGH == LOW, 0, TOTTRDQTY * (HIGH - CLOSE) / (HIGH - LOW) ), ]
all2021 <- all2021 [, vol := ifelse(TOTTRDQTY > 0, TOTTRDQTY, 1 ), ]
all2021 <- all2021 [, TP := BV + SV, ]

# // RAW Pressure Volume Calculations
all2021 <- all2021 [, BPV := BV / TP * vol, ]
all2021 <- all2021 [, SPV := SV / TP * vol, ]
all2021 <- all2021 [, TPV := BPV + SPV, ]

# // Karthik Marar's Pressure Volume Normalized Version (XeL-MOD.)
# VN = vol / ema(vol,20)
# BPN = BV / ema(BV,20) * VN * 100
# SPN = SV / ema(SV,20) * VN * 100
# TPN = BPN + SPN

all2021 <- all2021 [, VN := vol / EMA(vol,20), by = .(ticker)]
all2021 <- all2021 [, BPN := BV / EMA(BV,20) * VN * 100, by = .(ticker)]
all2021 <- all2021 [, SPN := SV / EMA(SV,20) * VN * 100, by = .(ticker)]
all2021 <- all2021 [, TPN := BPN + SPN, by = .(ticker)]

all2021 <- all2021 [, BuyVolumePercent := 100*BV/(BV+SV), ]
all2021 <- all2021 [, SellVolumePercent := 100*SV/(BV+SV), ]
