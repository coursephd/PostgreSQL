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

future.seed = TRUE
options(future.rng.onMisuse="ignore")

future::plan(future::multisession, workers = floor(parallel::detectCores() ))

###############################
#
# Python calculations
#
###############################

library(reticulate)

use_python("C:/ProgramData/Anaconda3", required = T)
#py_run_file("D:/My-Shares/prgm/0550_py_from_r_ranking.py")
py_run_file("D:/My-Shares/prgm/0550_py_from_r_ranking_15min.py")


stock_final <- py$stock_final
stock_final <- as.data.table(stock_final)
stock_final <- stock_final [, trdtme := anytime(Datetime, tz ="IST"), ]
stock_final <- stock_final [, trdate := anydate(Datetime), ]
stock_final <- stock_final [ order(Name, trdtme) ]
stock_final <- stock_final [, nrow := 1:.N, by = .(Name)]
stock_final <- stock_final [, subrow := 1:.N, by = .(Name, trdate)]

setnames(stock_final, "Open", "price.open")
setnames(stock_final, "High", "price.high")
setnames(stock_final, "Low", "price.low")
setnames(stock_final, "Close", "price.close")
setnames(stock_final, "Volume", "volume")
setnames(stock_final, "Name", "ticker")

all02 <- stock_final
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


all02 <- Reduce(function(...) merge(..., by = c("allrow"), all=T),  
                list( all02, roc_dn, roc_dn02, adx_dn) )

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

all03 <- all03 [, `:=`(longtermma = 0.2 * 100 * (price.close - ma200) / ma200,
                       longtermroc = 0.2 * roc125,
                       midtermma = 0.15 * 100 * (price.close - ma50) / ma50,
                       midtermroc = 0.15  * roc20,
                       ppo = 100 * (ma12 - ma26) / ma26), ]

all03 <- all03 [, sig := EMA(ppo, 9), by = .(ticker)]
all03 <- all03 [, ppoHist := ppo - sig, ]
all03 <- all03 [, slope := (ppoHist - shift(ppoHist, n = 8, type = c("lag") ) / 3), by = .(ticker)]
all03 <- all03 [, stPpo := 0.1 * 100 * slope, ]
#all03 <- all03 [, stRsi := .05 * RSI(price.close, 9), by = .(ticker)]
all03 <- all03 [, stRsi := 0.1 * MFI(price.close, volume, 9), by = .(ticker)]
all03 <- all03 [, stDlp := ifelse(DIp > 20 & DIp > DIn & ADX > 15, 0.1 * DIp, 0),]

all03 <- all03 [, trank := round(longtermma + longtermroc + midtermma + midtermroc + stPpo + stRsi + stDlp, 2), ]
all03 <- all03 [, trank := as.numeric(trank), ]

#all03 <- na.omit(all03)
all03 <- all03 [ order(trdate, subrow, -trank)]
all03 <- all03 [, nrank := 1:.N, by = .(trdate, subrow)]

# Count number of times the stock is in top 10 on a rolling basis of 10 days

all03 <- all03 [, top10 := ifelse(nrank <= 10, 1, 0), ]
all03 <- all03 [, cumtop10 :=runSum(top10, n = 15 ), by =.(ticker) ]
all03 <- all03 [, ticker02 := paste(ticker, trank, cumtop10, sep=","), ]

all03 <- all03 [, `:=`(tw = price.high - pmax(price.open, price.close),
                       bw = pmin(price.open, price.close) - price.low,
                       body = abs(price.close - price.open) ), by = .(ticker)]

all03 <- all03 [, vol_up := ifelse(price.open < price.close, volume * 0.5 * (tw + bw + 2 * body) / (tw + bw + body), volume * 0.5 * (tw + bw) / (tw + bw + body) ), ]
all03 <- all03 [, vol_dwn := ifelse(price.open >= price.close, volume * 0.5 * (tw + bw + 2 * body) / (tw + bw + body), volume * 0.5 * (tw + bw) / (tw + bw + body) ), ]
all03 <- all03 [, subrow02 := as.ITime (as.ITime("09:15") + subrow*5*60 ), ]

all03_t1hr <- dcast(data = all03 [ nrank <= 20 ] ,
                    trdate + subrow ~ nrank,
                    value.var = c("ticker02") )

all03_t1hr <- all03_t1hr [ order(-trdate, -subrow) ]
fwrite(all03_t1hr, "D:\\My-Shares\\analysis\\rerun.csv")
#############################################################################


all2021 <- stock_final
all2021 <- all2021 [, `:=`(TOTTRDQTY = volume, CLOSE = price.close, LOW = price.low, 
                   HIGH = price.high, OPEN = price.open), ]

all2021 <- all2021 [, totrow := .N, by = .(ticker)]
all2021 <- all2021 [ totrow >= 20 ]
#all2021 <- all2021 [, avgprice := TOTTRDVAL / TOTTRDQTY, ]

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

all2021_02 <- all2021 [ BuyVolumePercent > 0]
all2021_02 <- all2021_02 [ order (Datetime, -BuyVolumePercent)]

