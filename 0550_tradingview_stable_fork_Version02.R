# Technical Rank ( TR ) was authored by John Murphy. 
# Technical Rank shows how a security is performing relative to its peers . 
# Multiple moving averages, rate of change and the Relative Strength Index ( RSI ) indicators are used to calculate the Technical Rank. 
# These values are mathematically manipulated with percentage factors and then summed together. 
# There are 3 parts, long term, middle term and short term. 
#
# For Long term part Moving Average with length 200 (30%) and Rate of Change with the length 125 (30%) are used, 
# For middle term part, Moving Average with length 50 (15%) and Rate of Change with the length 20 (15%) are used and 
# For short term part, PPO (5%) and RSI (5%) used.


# Technical Rank is created using the following formula and weightings:
#  Long-Term Indicators (weighting): Percent above/below the 200-day exponential moving average ( EMA ) (30% weight) and the 125-day rate-of-change ( ROC ) (30% weight).
#  Medium-Term Indicators (weighting): Percent above/below 50-day EMA (15%) and the 20-day rate-of-change (15%).
#  Short-Term Indicators (weighting): Three-day slope of percentage price oscillator histogram divided by three (5%) and the relative strength index (5%).


library(TTR)
library(openxlsx)
library(data.table)
library(tidyverse)
library(anytime)
library(zoo)
library(RCurl)
library(lubridate)
library(curl)
library(ggplot2)
library(plotly)

options(scipen = 999)

library(BatchGetSymbols)

future.seed = TRUE
options(future.rng.onMisuse="ignore")

future::plan(future::multisession, workers = floor(parallel::detectCores() ))

###########################################
#
# Part 1
# Create a mapping of the company names
# ICICI - Yahoo - NSE [FnO stocks]
#
###########################################
source("D:\\My-Shares\\prgm\\0550_tradingview_yh_icici_map.R")

###########################################
#
# Part 2
#
# Python calculations
# Get the 5 mins data for calculations
# Get 1 day data for Fibo calculations
# Fibo values will be used for the targets
#
###########################################

library(reticulate)

start_time <- Sys.time()

use_python("C:/ProgramData/Anaconda3/python.exe", required = T)

# Get the source data from yahoo finance, 
# Create a csv file to be passed into supertrend formula

py_run_file("D:/My-Shares/prgm/0550_yh_stock_part01_5min.py")

# Read the csv file created above
# Create supertrend and and again create another csv file
# Create remaining calculations as expected:

start_time <- Sys.time()

calcs <- function (infile, outdata) {
  
  #data <- fread("D:/My-Shares/analysis/0551_5min_data.csv")
  data <- fread(infile)
  
  data <- data [, nrow := 1:.N, ]
  data <- data [, x := 1, ]
  
  data02 <- data [ nrow <= 2]
  data02_t <- melt(data = data02, 
                   id.vars = c("x"), 
                   measure.vars = c( names(data02[, -c("x"), ] ) ) )
  
  data02_t <- as.data.table( data02_t)
  data02_t <- data02_t [, .(value02 = paste(value, collapse = "_", sep="") ), by = .(variable) ]
  data02_t <- data02_t [, c("ticker", "temp") := tstrsplit(value02, "_"), ]
  data02_t <- data02_t [, value02 := ifelse(value02 == "_", "Datetime", value02), ]
  data02_t <- data02_t [, value02 := str_replace(value02, " ", "_"), ]
  data02_t <- data02_t [, value02 := ifelse(variable == "nrow", "nrow", value02), ]
  data02_t <- data02_t [, step001 := paste("setnames (data,'", variable, "', '", value02, "')", sep=""), ]
  
  eval(parse(text = data02_t$step001))
  
  data03 <- data [ nrow >= 4]
  
  data03_t <- melt(data = data03,
                   id.vars = c("Datetime", "nrow"),
                   measure.vars = c( names(data03 [, -c("Datetime", "nrow"), ] ) ) )
  data03_t <- as.data.table( data03_t)
  
  data03_t <- data03_t [, c("Name", "ohlcv", "tmp") := tstrsplit(variable, "_"), ]
  
  stock_final <- dcast(data = data03_t [, -c("tmp"), ],
                       Datetime + nrow + Name ~ ohlcv, 
                       value.var = c("value") )
  
  stock_final <- as.data.table(stock_final)
  stock_final <- stock_final [, trdtme := format(Datetime, tz="Asia/Calcutta"), ]
  stock_final <- stock_final [, trdate := anydate(str_sub(trdtme, 1, 10) ), ]
  stock_final <- stock_final [ order(Name, trdtme) ]
  stock_final <- stock_final [, nrow := 1:.N, by = .(Name)]
  stock_final <- stock_final [, subrow := 1:.N, by = .(Name, trdate)]
  
  setnames(stock_final, "Open", "price.open")
  setnames(stock_final, "High", "price.high")
  setnames(stock_final, "Low", "price.low")
  setnames(stock_final, "Close", "price.close")
  setnames(stock_final, "Volume", "volume")
  setnames(stock_final, "Name", "ticker")
  
  all02 <- stock_final [, -c("NA"), ]
  all02 <- all02 [, `:=`(price.open = as.numeric(price.open), 
                         price.high = as.numeric(price.high), 
                         price.low = as.numeric(price.low), 
                         price.close = as.numeric(price.close), 
                         volume = as.numeric(volume)), ]
  all02 <- na.omit(all02)
  all02 <- all02 [ order(ticker, trdate)]
  
  assign(deparse(substitute(outdata)), all02, envir=.GlobalEnv)
  
}

calcs(infile = "D:/My-Shares/analysis/0550_data.csv", outdata = data_05min)
calcs(infile = "D:/My-Shares/analysis/0550_data02.csv", outdata = data_01day)

setnames(data_01day, "price.open", "open",)
setnames(data_01day, "price.high", "high")
setnames(data_01day, "price.low", "low")
setnames(data_01day, "price.close", "close")
setnames(data_01day, "volume", "volumed")

calcs(infile = "D:/My-Shares/analysis/0550_data02.csv", outdata = data_01day)

setnames(data_01day, "price.open", "open",)
setnames(data_01day, "price.high", "high")
setnames(data_01day, "price.low", "low")
setnames(data_01day, "price.close", "close")
setnames(data_01day, "volume", "volumed")

data_01day <- data_01day [, drow := 1:.N, by = .(ticker)]

data_01day <- data_01day [, vPP := as.numeric(round( (shift(high, n =1, type = c("lag")) + 
                                                        shift(low, n =1, type = c("lag")) + 
                                                        shift(close, n =1, type = c("lag"))  )/ 3), 2 ), ]
data_01day <- data_01day [, `:=`(phigh = shift(high, n =1, type = c("lag")), 
                                 plow = shift(low, n =1, type = c("lag")) ), by =.(ticker)]

data_01day <- data_01day [, `:=`(vR0 = vPP + (phigh - plow) * 0,
                                 vS0 = vPP - (phigh - plow) * 0,
                                 
                                 vR0236 = vPP + (phigh - plow) * 0.236,
                                 vS0236 = vPP - (phigh - plow) * 0.236,
                                 
                                 vR0382 = vPP + (phigh - plow) * 0.382,
                                 vS0382 = vPP - (phigh - plow) * 0.382,
                                 
                                 vR05 = vPP + (phigh - plow) * 0.5,
                                 vS05 = vPP - (phigh - plow) * 0.5,
                                 
                                 vR0618 = vPP + (phigh - plow) * 0.618,
                                 vS0618 = vPP - (phigh - plow) * 0.618,
                                 
                                 vR0786 = vPP + (phigh - plow) * 0.786,
                                 vS0786 = vPP - (phigh - plow) * 0.786,
                                 
                                 vR1 = vPP + (phigh - plow) * 01,
                                 vS1 = vPP - (phigh - plow) * 01,
                                 
                                 vR1272 = vPP + (phigh - plow) * 1.272,
                                 vS1272 = vPP - (phigh - plow) * 1.272,
                                 
                                 vR1414 = vPP + (phigh - plow) * 1.414,
                                 vS1414 = vPP - (phigh - plow) * 1.414,
                                 
                                 vR1618 = vPP + (phigh - plow) * 1.618,
                                 vS1618 = vPP - (phigh - plow) * 1.618,
                                 
                                 vR2618 = vPP + (phigh - plow) * 2.618,
                                 vS2618 = vPP - (phigh - plow) * 2.618), ]

all02 <- merge.data.table (x = data_05min, 
                y = data_01day [, c("ticker", "trdate", "open", "high", "low", "close", "volumed", "drow",
                                    "vR0", "vR0236", "vR0382", "vR05", "vR0618", "vR0786", "vR1", "vR1272", "vR1414", "vR1618", "vR2618",
                                    "vS0", "vS0236", "vS0382", "vS05", "vS0618", "vS0786", "vS1", "vS1272", "vS1414", "vS1618", "vS2618"), ],
                by = c("ticker", "trdate"))

all02 <- all02 [, allrow := .I, ]
all02 <- all02 [, nrow := 1:.N, by = .(ticker)]

###########################################
#
# Part 3:
# TRI components:
# Get the ranks calculations done here
#
#
###########################################

all02 <- all02 [, `:=`(price.open = as.numeric(price.open), 
                       price.high = as.numeric(price.high), 
                       price.low = as.numeric(price.low), 
                       price.close = as.numeric(price.close), 
                       volume = as.numeric(volume)), ]
all02 <- na.omit(all02)

all02 <- all02 [ order(ticker, trdate)]
all02 <- all02 [, allrow := .I, ]
all02 <- all02 [, nrow := 1:.N, by =.(ticker)]

adx_n <- 14
adx_dn = as.data.table( ADX(all02[,c("price.high","price.low","price.close"),], n = adx_n) )
adx_dn <- adx_dn [, allrow := .I, ]

roc_n <- 20
roc_dn = as.data.table( ROC(all02$price.close, n = roc_n) )
roc_dn <- roc_dn [, allrow := .I, ]
setnames(roc_dn, "V1", "roc20")

roc_n02 <- 125
roc_dn02 = as.data.table( ROC(all02$price.close, n = roc_n02) )
roc_dn02 <- roc_dn02 [, allrow := .I, ]
setnames(roc_dn02, "V1", "roc125")

#sar <- as.data.table( SAR(all02[, c("price.high","price.low") ], accel = c(0.1, 0.2) ) )
#sar <- sar [, allrow := .I, ]


all02 <- Reduce(function(...) merge(..., by = c("allrow"), all=T),  
                list( all02, roc_dn, roc_dn02, adx_dn) )

rm(roc_dn, roc_dn02, adx_dn)

# Calculate the Bollinger band, the calculations are done over all the dataset
# So the first n lines of the bollinger band calculations go wrong
# as the data from previous company is carried forward
#
# Remove the calculations for first 20 * 2 rows

# To avoid any incorrect calculations explained above, remove certain number of rows

all03 <- all02 [ nrow > roc_n02]
all03 <- all03 [, `:=`(ma200 = SMA(price.close, 200),
                       ma50  = SMA(price.close, 50),
                       ma12 = EMA(price.close, 12),
                       ma26 = EMA(price.close, 26)), by = .(ticker)]

all03 <- all03 [, `:=`(longtermma = 0.15 * 100 * (price.close - ma200) / ma200,
                       longtermroc = 0.15 * roc125,
                       midtermma = 0.125 * 100 * (price.close - ma50) / ma50,
                       midtermroc = 0.125  * roc20,
                       ppo = 100 * (ma12 - ma26) / ma26), ]

all03 <- all03 [, `:=`(ema13 = EMA(price.close, 13),
                       ema21 = EMA(price.close, 21),
                       mfi09 = MFI(price.close, volume, 9)), by = .(ticker)]

#all03 <- na.omit(all03)
all03 <- all03 [, wma21mfi09 := WMA(mfi09, 21), by = .(ticker)]
all03 <- all03 [, subrow01 := 1:.N, by = .(ticker, trdate)]
all03 <- all03 [, hlc3 := (price.high + price.low + price.close)/3,]

# Some problems with how VWAP is calculated
#all03 <- all03 [, vwap := VWAP (hlc3, volume, n = subrow01), by = .(ticker, trdate)]
all03 <- all03 [, stp001_c := hlc3 * volume, ]
all03 <- all03 [, stp002_c := cumsum(stp001_c), by = .(ticker, trdate)]
all03 <- all03 [, stp003_c := cumsum(volume), by = .(ticker, trdate)]
all03 <- all03 [, vwap := stp002_c / stp003_c,]

all03 <- all03 [, sig := EMA(ppo, 9), by = .(ticker)]
all03 <- all03 [, ppoHist := ppo - sig, ]
all03 <- all03 [, slope := (ppoHist - shift(ppoHist, n = 8, type = c("lag") ) / 8), by = .(ticker)]
#all03 <- all03 [, stPpo := 0.15 * 100 * slope, ]
all03 <- all03 [, stPpo := ifelse(slope > 1, 0.15 * 100 * slope, 0), ]
#all03 <- all03 [, stRsi := .05 * RSI(price.close, 9), by = .(ticker)]
all03 <- all03 [, stRsi := 0.1 * MFI(price.close, volume, 9), by = .(ticker)]
all03 <- all03 [, stDlp := ifelse(DIp > 20 & DIp > DIn & ADX > 15, 0.1 * DIp, 0),]

fwrite(all03, "D:/My-Shares/analysis/0550_stock_final.csv")

end_time <- Sys.time()
end_time - start_time

py_run_file("D:/My-Shares/prgm/0550_yh_stock_part02_5min_supertrend.py")

all03 <- fread("D:/My-Shares/analysis/0550_stock_final03.csv")
all03 <- all03 [, ST := ifelse(SUPERTd_20_2.7 == 1, 0.15, 0), ]

####################
#
# Added new concept
#
####################

all03 <- all03 [, `:=` ( up_st = ifelse(SUPERTd_20_2.7 == 1, 1, 0),
                         up_mfi = ifelse(mfi09 >= wma21mfi09, 1, 0),
                         up_adx = ifelse(DIp >= 25 & DIn < DIp, 1, 0),
                         up_ema = ifelse(ema13 >= ema21, 1, 0), 
                         up_vwap = ifelse(price.close >= vwap, 1, 0) ), ]

all03 <- all03 [, up_tot := up_st + up_mfi + up_adx + up_ema + up_vwap, ]

all03 <- all03 [, `:=` (grp_st = rleid(SUPERTd_20_2.7),
                        grp_mfi = rleid(up_mfi),
                        grp_adx = rleid(up_adx),
                        grp_ema = rleid(up_ema),
                        grp_vwap = rleid(up_vwap)),  by = .(ticker)]

all03 <- all03 [, rows_st := 1:.N, by = .(ticker, grp_st)]
all03 <- all03 [, rows_mfi := 1:.N, by = .(ticker, grp_mfi)]
all03 <- all03 [, rows_adx := 1:.N, by = .(ticker, grp_adx)]
all03 <- all03 [, rows_ema := 1:.N, by = .(ticker, grp_ema)]
all03 <- all03 [, rows_vwap := 1:.N, by = .(ticker, grp_vwap)]

all03 <- all03 [, trank := round(longtermma + longtermroc + midtermma + midtermroc + stPpo + stRsi + stDlp + ST, 2), ]
all03 <- all03 [, trank := as.numeric(trank) * up_tot, ]

all03 <- all03 [ order(trdate, subrow, -trank)]
all03 <- all03 [, nrank := 1:.N, by = .(trdate, subrow)]

# Count number of times the stock is in top 10 on a rolling basis of 10 days

all03 <- all03 [, top10 := ifelse(nrank <= 10, 1, 0), ]
all03 <- all03 [, cumtop10 :=runSum(top10, n = 15 ), by =.(ticker) ]
all03 <- all03 [, ticker02 := paste(ticker, ",", trank, ",", cumtop10, ",rows_st =", rows_st, ",rows_adx =", rows_adx, sep=""), ]
all03 <- all03 [, subrow02 := as.ITime (as.ITime("09:15") + subrow*5*60 ), ]
all03 <- all03 [, perchg := round( (price.high  - price.low) / price.low * 100, 2) , ]

all03_t1hr <- dcast(data = all03 [ nrank <= 20 ] ,
                    trdate + subrow + subrow02 ~ nrank,
                    value.var = c("ticker02") )

all03_t1hr <- all03_t1hr [ order(-trdate, -subrow) ]

fwrite(all03_t1hr, "D:\\My-Shares\\analysis\\rerun_5min_new.csv")

end_time <- Sys.time()
end_time - start_time

##################################################################################################
#
# Part 4
# For trade management
#
#
##################################################################################################

trial001 <- copy(all03)

#output <- trial001 [up_st == 1 & up_adx == 1 & up_mfi == 1 & up_ema == 1 & nrank <= 15]
#output <- trial001 [up_st == 1 & rows_st <= 5 & up_adx == 1 & rows_adx <= 5 & up_ema == 1 & rows_ema <=5 & nrank <= 5 & perchg <= 0.75]
output <- trial001 [up_st == 1 & rows_st <= 5 & up_adx == 1 & rows_adx <= 5 & nrank <= 5 & perchg <= 0.75]

output <- output [, subset := 1:.N, by =.(ticker, trdate)]

output02 <- output [ subset == 1]
output02 <- output02 [, c("ticker", "drow", "trdate", "subrow", "subrow02",  "nrank", "price.open", "price.high", "price.low", "price.close"), ]
output02 <- output02 [, signal := 1, ]

setnames(output02, "price.open", "entry_o")
setnames(output02, "price.high", "entry_h")
setnames(output02, "price.low", "entry_l")
setnames(output02, "price.close", "entry_c")
setnames(output02, "subrow", "entry_row")
setnames(output02, "subrow02", "entry_time")
setnames(output02, "nrank", "entry_nrank")

# Merge this data with the original data

trial002 <- merge (x = trial001, 
                   y = output02, 
                   by = c("ticker", "trdate", "drow"))

output02 <- output02 [ order(-trdate, -entry_row, entry_nrank)]

trial002 <- trial002 [ subrow >= entry_row]
trial002 <- trial002 [, c("ticker", "drow", "trdate", "subrow", "subrow02", "entry_row", "nrank", "signal",
                          "entry_o", "entry_h", "entry_l", "entry_c",
                          "price.open", "price.high", "price.low", "price.close", "volume",
                          "SUPERT_20_2.7", "SUPERTd_20_2.7", "SUPERTl_20_2.7", "SUPERTs_20_2.7",
                          "vR0", "vR0236", "vR0382", "vR05", "vR0618", "vR0786", "vR1", "vR1272", 
                          "vR1414", "vR1618", "vR2618"), ]

trial002 <- trial002 [, temp_prc := round( (entry_h * 1.02)/5, 2),  ]
trial002 <- trial002 [, nshares := round( (100000 / temp_prc) * 0.8 , 0),  ]

trial002 <- trial002 [ order(-trdate, -subrow) ]


trial002_t <- melt.data.table(data = trial002 [ subrow == entry_row ],
                              id.vars = c("ticker", "drow", "trdate", "subrow", "subrow02", "entry_row", "nrank", "signal",
                                          "entry_o", "entry_h", "entry_l", "entry_c", "temp_prc", "nshares",
                                          "price.open", "price.high", "price.low", "price.close", "volume",
                                          "SUPERT_20_2.7", "SUPERTd_20_2.7", "SUPERTl_20_2.7", "SUPERTs_20_2.7"), 
                              measure.vars = c("vR0", "vR0236", "vR0382", "vR05", "vR0618", "vR0786", "vR1", "vR1272", 
                                               "vR1414", "vR1618", "vR2618"))

trial002_t <- trial002_t [, value := as.numeric( round(value, 2 )), ]
trial002_t <- trial002_t [, dist := value - entry_h, ]
trial002_t <- trial002_t [ order (-trdate, ticker, dist)]

#######################################################
#
# Fibo series gives the targets T01, T02, T03, ...
# Supertrend values give the SL or TSL
#
# Use the Fib levels where the distance between the CMP and
# Fibo is +ve
#
# Get the ICICI company name merged onto the dataset
#
#######################################################

trial002_t02 <- trial002_t [dist > 0]
trial002_t02 <- trial002_t02 [, tgt := 1:.N, by = .(trdate, ticker)]

trial002_t02 <- merge (x = trial002_t02,
                       y = icici_fno [, c("SYMBOL02", "ShortName"), ],
                       by.x = c("ticker"),
                       by.y = c("SYMBOL02"),
                       all.x = TRUE)

trial003 <- dcast.data.table(data = trial002_t02,
                             ticker + ShortName + drow + trdate + entry_row ~ paste("t", str_pad(tgt, 2, pad="0"), sep ="_"),
                             fill =" ")

trial004 <- merge(x = trial002,
                  y = trial003,
                  by = c("ticker", "drow", "trdate", "entry_row"))

trial004 <- trial004 [ order(-trdate, -subrow, ticker)]

#######################################################
#
# Part 5
#
# Create place_order, modify_order, cancel_order, etc.
# Based on the ST, T01, T02, T03, nshares, ShortName
# variables
#
# needs some work to get this fully correct
#
# Output this to a python code (.py file)
#
# This code will have to be executed once the file is
# created
# 
#######################################################

trial005 <- trial004 [, -c("vR0", "vR0236", "vR0382", "vR05", "vR0618", "vR0786", "vR1", "vR1272", 
                           "vR1414", "vR1618", "vR2618"),  ]

trial005 <- trial005 [, step001 := 'breeze.place_order(exchange_code="NSE", product="cash", action="buy", order_type="limit", validity="day", user_remark="1st buy order",', ]
trial005 <- trial005 [, step001sell := 'breeze.place_order(exchange_code="NSE", product="cash", action="sell", order_type="limit", validity="day", ', ]
trial005 <- trial005 [, step002 := paste('stock_code="', ShortName, '", stoploss="', round(SUPERT_20_2.7, 2), '", quantity="', round(nshares/3, 0) * 3, '", price="', round(entry_h, 2), '")', sep=""), ]
trial005 <- trial005 [, step002t01 := paste('stock_code="', ShortName, '", stoploss="', round(SUPERT_20_2.7, 2), '", quantity="', round(nshares/3, 0), '", price="', round(t_01, 2), '", user_remark="Sell order T01")', sep=""), ]
trial005 <- trial005 [, step002t02 := paste('stock_code="', ShortName, '", stoploss="', round(SUPERT_20_2.7, 2), '", quantity="', round(nshares/3, 0), '", price="', round(t_02, 2), '", user_remark="Sell order T02")', sep=""), ]
trial005 <- trial005 [, step002t03 := paste('stock_code="', ShortName, '", stoploss="', round(SUPERT_20_2.7, 2), '", quantity="', round(nshares/3, 0), '", price="', round(t_03, 2), '", user_remark="Sell order T03")', sep=""), ]

trial005 <- trial005 [, step003buy := paste(step001, step002, sep=""), ]
trial005 <- trial005 [, step003sell01 := paste(step001sell, step002t01, sep=""), ]
trial005 <- trial005 [, step003sell02 := paste(step001sell, step002t02, sep=""), ]
trial005 <- trial005 [, step003sell03 := paste(step001sell, step002t03, sep=""), ]


#######################################################
#
# Part 6
#
# Creation of the python files
#
# Part 01 Login code
# Part 02 Orders code
# Part 03 Updates / modify / cancel orders code 
#         if needed as a separate file
#
#######################################################

chk <- trial005 [drow == max(drow) & entry_row == subrow ]
chk <- chk [, comments := paste("from breeze_connect import BreezeConnect\nimport http.client\nimport json\nimport pandas as pd\n\n# Order on date ", trdate, " at ", subrow02, " for company ", ticker, "\n\n", sep= "" ),]
chk <- chk [, orders := paste(comments, step003buy, "\n", step003sell01, "\n", step003sell02, "\n", step003sell03, sep=""), ]

fwrite(chk[, c("orders"), ], 
       quote = FALSE,
       sep = " ",
       col.names = FALSE,
       row.names = FALSE,
       file = "D:/My-Shares/prgm/0550_icici_02_orders.py")


py_run_file("D:/My-Shares/prgm/0550_icici_01_login.py")

py_run_file("D:/My-Shares/prgm/0550_icici_02_orders.py")


########################################################################################################
#
#
# End of program
#
#
########################################################################################################
