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
library(xml2)
library(rvest)
library(openxlsx)

options(scipen = 999)

# from today to 2 years behind
tday <- Sys.Date()
#tday <- "2022-12-31"
lastday <- "2022-01-01"
lastday <- "2022-10-01"
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
date_char_fo <- paste("fo", toupper(format(dates, "%d%b%Y")), "bhav.csv", sep ="")

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

  url_fo <- paste("https://www1.nseindia.com/content/historical/DERIVATIVES/", monyr[i], "/", date_char_fo[i], ".zip", sep="")
  csvpath_fo0 <- paste(csvpath, yr[i], sep="")
  zipfile_fo0 <- paste(csvpath_fo0, "/", date_char_fo[i], ".zip", sep="")
  csvfile_fo0 <- paste(csvpath_fo0, "/", date_char_fo[i], sep="")
  req_fo <- curl_fetch_memory(url_fo)
  
  print(i)
  print(csvpath_fo0)
  print(zipfile_fo0)
  print(csvfile_fo0)
  print(url_fo)
  print(req_fo$status_code)
  
  if (req_fo$status_code == 200) {
    download.file(url_fo, zipfile_fo0)
    unzip(zipfile = zipfile_fo0, exdir = csvpath_fo0)
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
  
  if (req_del$status_code == 200) {
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
  
  url_prt <- paste("https://www1.nseindia.com/content/nsccl/fao_participant_oi_", day[i], mon_num[i], yr[i], ".csv", sep="")
  prt_data <- paste("fao_participant_oi_", day[i], mon_num[i], yr[i], sep="")
  #prtzipfile0 <- paste(csvpath0, "/", prt_data, sep="")
  prtfile0 <- paste(csvpath0, "/", prt_data, ".csv", sep="")
  req_prt <- curl_fetch_memory(url_prt)
  
  print(url_prt)
  print(prt_data)
  #print(prtzipfile0)
  print(prtfile0)
  print(req_prt$status_code)
  
  if (req_prt$status_code == 200) {
    download.file(url_prt, prtfile0)
    #unzip(zipfile = coizipfile0, exdir = csvpath0)
  }
  
}

rm(list = ls( pattern = "^delivery") )


list_of_files <- list.files(path = csvpath0, recursive = TRUE, pattern = "^cm.*csv$", full.names = TRUE)
f <- function(x, pos) subset(x, SERIES == "EQ" )
dt_chunk <- rbindlist(sapply(list_of_files, read_csv_chunked, DataFrameCallback$new(f), simplify = FALSE), fill = TRUE)

dt_chunk <- dt_chunk [, `:=`(trdate = anydate( dmy(TIMESTAMP) )),]
dt_chunk <- dt_chunk [, nshares_trade := as.numeric( round( TOTTRDQTY / TOTALTRADES, 0) ), ]
setnames(dt_chunk, "SYMBOL", "ticker")

list_of_files_fo <- list.files(path = csvpath_fo0, recursive = TRUE, pattern = "^fo.*csv$", full.names = TRUE)
f_fo <- function(x, pos) subset(x, CONTRACTS > 0 )
dt_chunk_fo <- rbindlist(sapply(list_of_files_fo, read_csv_chunked, DataFrameCallback$new(f_fo), simplify = FALSE), fill = TRUE)

dt_chunk_fo <- dt_chunk_fo [, `:=`(trdate = anydate( dmy(TIMESTAMP) )),]
setnames(dt_chunk_fo, "SYMBOL", "ticker")
dt_chunk_fo <- dt_chunk_fo [, .( s_oi = sum(OPEN_INT), 
                                 s_chgoi = sum(CHG_IN_OI)), by = .(INSTRUMENT, ticker, trdate, OPTION_TYP)]

dt_chunk_fo <- dt_chunk_fo [, `:=`(t_oi = sum(s_oi), 
                                   t_chgoi = sum(s_chgoi)), by = .(ticker, trdate)]
                            
list_of_filescoi <- list.files(path = csvpath0, recursive = TRUE, pattern = "^combineoi.*csv$", full.names = TRUE)
f2 <- function(x, pos) subset(x, 1 == 1 )
dt_chunkcoi <- rbindlist(sapply(list_of_filescoi, read_csv_chunked, DataFrameCallback$new(f2), simplify = FALSE), fill = TRUE)

setnames(dt_chunkcoi, "Scrip Name", "ScripName")
setnames(dt_chunkcoi, "NSE Symbol", "ticker")
setnames(dt_chunkcoi, "Open Interest" , "coi")

dt_chunkcoi <- dt_chunkcoi [, trdate := anydate( dmy(Date) ), ]

list_of_files_prt <- list.files(path = csvpath0, recursive = TRUE, pattern = "^fao_participant_oi.*csv$", full.names = TRUE)
#f_prt <- function(x, pos) subset(x, CONTRACTS > 0 )
dt_chunk_prt <- rbindlist(sapply(list_of_files_prt, fread, simplify = FALSE, skip =2), fill = TRUE, idcol = "file")
cols <- names(dt_chunk_prt)[3:16]
dt_chunk_prt <- dt_chunk_prt [, (cols) := lapply(.SD, as.numeric), .SDcols = cols ]
dt_chunk_prt <- dt_chunk_prt [, c("t01", "t02", "t03", "t04") := tstrsplit(file, "_"), ]
dt_chunk_prt <- dt_chunk_prt [, c("dt", "tmp") := tstrsplit(t04, "\\."), ]
dt_chunk_prt <- dt_chunk_prt [, `:=`(trdate = anydate( dmy(dt) )),]
dt_chunk_prt <- dt_chunk_prt [, -c("file", "t01", "t02", "t03", "t04", "dt", "tmp"), ]

all02 <- Reduce(function(...) merge(..., by = c("ticker", "trdate"), all=T),
                list( del_all, dt_chunk, dt_chunkcoi [, -c("ISIN"), ]) )

all03 <- all02 [ coi != ""]

saveRDS(all03, "D:\\My-Shares\\source-fno-csv\\2022\\all2022.rds")

all04 <- all03 [, c("ticker", "ISIN", "trdate", "ScripName", "delperc", "coi", "OPEN", "CLOSE", "LOW", "HIGH", "nshares_trade" ), ]

# Calculate 10, 20, 30 day moving average for delperc, coi, nshares_trade
all04 <- all04 [, `:=`(delperc10sma = SMA( as.numeric(delperc), 10),
                       coi10sma = SMA(coi, 10),
                       nshares_trade10sma = SMA(nshares_trade, 10),
                       close10sma = SMA (CLOSE, 10)), by = .(ticker)]

do.call(file.remove, list(list.files(path = csvpath0, pattern = "^*.*csv$", full.names = TRUE)))
do.call(file.remove, list(list.files(path = csvpath0, pattern = "^*.*xml$", full.names = TRUE)))

all2021_01 <- readRDS("D:\\My-Shares\\source-fno-csv\\2021\\all2021.rds")
all2022_01 <- readRDS("D:\\My-Shares\\source-fno-csv\\2022\\all2022.rds")

all2021 <- rbind(all2021_01, all2022_01 )

all04 <- all2021 [, c("ticker", "ISIN", "trdate", "ScripName", "delperc", "coi", "OPEN", "CLOSE", "LOW", "HIGH", "nshares_trade" ), ]

all04 <- all04 [, totrow := .N, by = .(ticker)]
all05 <- all04 [ totrow >= 10]


all05_t <- melt(data = all05,
                id.vars = c("ticker", "ISIN", "ScripName", "trdate", "OPEN", "HIGH", "LOW", "CLOSE", "delperc", "coi", "nshares_trade"), 
                measure.vars = c("coi", "nshares_trade", "CLOSE"))

all05_t <- as.data.table(all05_t)
all05_t <- all05_t [, `:=`(lag01 = shift(value, n=1, type = c("lag")),
                           lag03 = shift(value, n=3, type = c("lag")),
                           lag05 = shift(value, n=5, type = c("lag")),
                           lag07 = shift(value, n=7, type = c("lag")),
                           lag10 = shift(value, n=10, type = c("lag")) ), by = .(ticker, variable)]

all06_t <- melt.data.table(data = all05_t,
                           id.vars = c("ticker", "ISIN", "ScripName", "trdate", "OPEN", "HIGH", "LOW", "CLOSE", "delperc", "coi", "nshares_trade",  "variable", "value"),
                           measure.vars = c("lag01", "lag03", "lag05", "lag07", "lag10"),
                           variable.name = "category",
                           value.name = c("cat_value") )

all06_t <- as.data.table(all06_t)
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

all07_t <- all07_t [, logic_n := case_when(perchg_CLOSE >= 0 & perchg_coi >= 0 ~ 1,
                                           perchg_CLOSE >= 0 & perchg_coi < 0 ~ 1,
                                           perchg_CLOSE < 0 & perchg_coi >= 0 ~ -1,
                                           perchg_CLOSE < 0 & perchg_coi < 0 ~ -1), ]

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


all08_t <- all08_t [ trdate >= Sys.Date() - 365]

# https://www.youtube.com/watch?v=ACdCQuQJxhU

#all08_t <- all08_t [ order(SYMBOL, expdate, trdate)]
wb <- createWorkbook()
addWorksheet(wb, "Sheet 1")

writeData(wb, "Sheet 1", all08_t)

redstyle <- createStyle(fontColour = "#9c0006", bgFill = "#FFC7CE" ) 
greenstyle <- createStyle(fontColour = "#006100", bgFill = "#C6EFCE")

conditionalFormatting(wb, "Sheet 1",
                      cols = 1:25,
                      rows = 1: nrow(all08_t)+1,
                      rule = "Long Buildup - Bullish", 
                      "contains",
                      style = greenstyle)

conditionalFormatting(wb, "Sheet 1",
                      cols = 1:25,
                      rows = 1: nrow(all08_t)+1,
                      rule = "Short covering - Bullish", 
                      "contains",
                      style = greenstyle)

conditionalFormatting(wb, "Sheet 1",
                      cols = 1:25,
                      rows = 1: nrow(all08_t)+1,
                      rule = "Long unwinding - Bearish", 
                      "contains",
                      style = redstyle)

conditionalFormatting(wb, "Sheet 1",
                      cols = 1:25,
                      rows = 1: nrow(all08_t)+1,
                      rule = "Short Buildup - Bearish", 
                      "contains",
                      style = redstyle)

freezePane(wb, "Sheet 1", firstActiveRow = 2, firstActiveCol = 3)
addFilter(wb, "Sheet 1", row = 1, cols = 1:ncol(all08_t))

saveWorkbook(wb, "D:/My-Shares/analysis/0550_BigMoney_2021_2022.xlsx", TRUE)





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


#################################################################################
#
# Get the list of stock with the industry names from BSE website
# https://www.bseindia.com/corporates/List_Scrips.html
# Name the file as 001_bse.xlsx
#
# To avoid programming problems an additional comma is added to the Column row
#
#################################################################################

step001 <- fread("D:\\My-Shares\\analysis\\001_bse.csv")
setnames(x=step001, old=names(step001), new=gsub(" ","",names(step001)))

# Subset only the active companies and equity
step002 <- step001 [Status == "Active" & Instrument == "Equity" & Group != "Z" & IndustryNewName != "-"]


##########################################
#
# FII data
#
##########################################

url <- "https://www.cdslindia.com/Publications/ForeignPortInvestor.html"
pg <- read_html(url)

links <- data.table ( html_attr(html_nodes(pg, "a"), "href") )
links <- links [, nrow := .I, ]
links02 <- links [ str_detect(V1, "FortnightlySecWisePages")]
links02 <- links02 [, V10 := str_replace_all(V1, "//", "/") , ]
links02 <- links02 [, V2 := str_replace_all(V10, " ", "%20") , ]
links02 <- links02 [, V3 :=  str_trim ( paste("https://www.cdslindia.com/", str_sub(V2, 4, length(V2) ), sep = "") ), ]
links02 <- links02 [, c("t01", "t02", "t03", "t04", "t05") := tstrsplit(V10, "/"), ]
links02 <- links02 [, temp001 := paste("url_", nrow, "_html", sep=""), ]
links02 <- links02 [, temp002 := paste("url_", nrow, "_whole", sep=""), ]
links02 <- links02 [, step001 := paste(temp001, " = read_html('", V3, "');", sep=""), ]
links02 <- links02 [, step002 := paste(temp002, " = ", temp001, " %>% html_nodes('table') %>% html_table (fill = TRUE) %>% .[[1]];", sep =""), ]
links02 <- links02 [, step003 := paste(temp002, " = as.data.table(", temp002, ");", sep=""), ]
links02 <- links02 [, step033 := paste(temp002, " = ", temp002, "[, dt := '", t05, "', ];", sep=""), ]
links02 <- links02 [, step034 := paste(temp002, " = ", temp002, "[, nrow := .I, ];", sep=""), ]
links02 <- links02 [, step035 := paste(temp002, " = ", temp002, "[, drow :=", nrow, ", ];", sep=""), ]
links02 <- links02 [, step004 := paste(step001, step002, step003, step033, step034, step035, sep = " "), ]

eval(parse(text = links02 [ nrow <= 36]$step004))

fii_data <- rbindlist(mget(ls(pattern = "whole$")), fill = TRUE, idcol = "file_del")  
fii_data <- fii_data [, c("dt01", "dt02") := tstrsplit(dt, "\\."), ]
fii_data <- fii_data [, dt01 := anydate(dt01), ]

fii_data02 <- fii_data [, c("X1", "X2", "X37", "nrow", "drow", "dt01"), ]
fii_data02 <- fii_data02 [, X37num := as.numeric( str_remove_all(X37, ",") ), ]
fii_data02 <- na.omit(fii_data02)
fii_data02 <- fii_data02 [ order (dt01, nrow, drow) ]
fii_data02 <- fii_data02 [, x37lag := shift(X37num, n = 1, type = c("lag") ), by = .(X2)]
fii_data02 <- fii_data02 [, x37lag02 := shift(X37num, n = 2, type = c("lag") ), by = .(X2)]
fii_data02 <- fii_data02 [, flow_15 := X37num - x37lag, ]
fii_data02 <- fii_data02 [, flow_30 := X37num - x37lag02, ]
fii_data02 <- fii_data02 [, flow_30per := as.numeric( round( (X37num - x37lag02) / x37lag02 * 100, 2) ),  ]

fii_data03_per <- dcast(data = fii_data02,
                        nrow + X1 + X2 ~ dt01,
                        value.var =c("flow_30per"))

fii_data03_abs <- dcast(data = fii_data02,
                        nrow + X1 + X2 ~ dt01,
                        value.var =c("flow_30"))


eval(parse(text = links02 [ nrow == 50]$step004)) # runs
eval(parse(text = links02 [ nrow == 51]$step004)) # runs
eval(parse(text = links02 [ nrow == 52]$step004)) # runs
eval(parse(text = links02 [ nrow == 53]$step004)) # runs
eval(parse(text = links02 [ nrow == 54]$step004)) # runs
eval(parse(text = links02 [ nrow == 55]$step004)) # runs

