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

###########################################################
#
# Part 1
# Create a mapping of the company names
# ICICI - Yahoo - NSE [FnO stocks]
#
###########################################################
#
# Creating conditional execution of the mapping dataset
#
###########################################################

file_crea <- file.info("D:/My-Shares/analysis/icici_fno.rds")$ctime
compdt <- as.POSIXct( paste(Sys.Date(), "09:15:00", sep = ""), tz=Sys.timezone())

if (is.na(file_crea) ) {print("File does not exist, the source code must be executed to create mapping file")
  source("D:\\My-Shares\\prgm\\0550_tradingview_yh_icici_map.R") 
  file_crea <- file.info("D:/My-Shares/analysis/icici_fno.rds")$ctime
} 

if (file_crea >= compdt ) {
  print("The mapping dataset exists for the day, need not re-execute, only extracting from the earlier version to the local area") 
  icici_fno <- readRDS("D:/My-Shares/analysis/icici_fno.rds")
} else { 
  print("The mapping dataset needs to be created, executing the source code")
  source("D:\\My-Shares\\prgm\\0550_tradingview_yh_icici_map.R") 
}

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

data_05min02 <- all02 [, `:=` (o15 = shift(price.open, n = 2, type = c("lag") ),
                                    h15 = pmax( shift(price.high, n = 2, type = c("lag") ), shift(price.high, n = 1, type = c("lag") ), price.high  ),
                                    l15 = pmin( shift(price.low, n = 2, type = c("lag") ), shift(price.low, n = 1, type = c("lag") ), price.low  ),
                                    c15 = price.close,
                                    v15 = runSum(volume, n =3) ), 
                            by = .(ticker)]

data_05min02 <- data_05min02 [, `:=` (o30 = shift(price.open, n = 5, type = c("lag") ),
                                    h30 = pmax( shift(price.high, n = 5, type = c("lag") ), shift(price.high, n = 4, type = c("lag") ), shift(price.high, n = 3, type = c("lag") ), shift(price.high, n = 2, type = c("lag") ), shift(price.high, n = 1, type = c("lag") ), price.high  ),
                                    l30 = pmin( shift(price.low, n = 5, type = c("lag") ), shift(price.low, n = 4, type = c("lag") ), shift(price.low, n = 3, type = c("lag") ), shift(price.low, n = 2, type = c("lag") ), shift(price.low, n = 1, type = c("lag") ), price.low  ),
                                    c30 = price.close,
                                    v30 = runSum(volume, n =6) ), 
                            by = .(ticker)]

data_05min02 <- data_05min02 [, `:=` (o60 = shift(price.open, n = 11, type = c("lag") ),
                                    h60 = pmax( shift(price.high, n = 11, type = c("lag") ), shift(price.high, n = 10, type = c("lag") ), shift(price.high, n = 9, type = c("lag") ), shift(price.high, n = 8, type = c("lag") ), shift(price.high, n = 7, type = c("lag") ), shift(price.high, n = 6, type = c("lag") ), shift(price.high, n = 5, type = c("lag") ), shift(price.high, n = 4, type = c("lag") ), shift(price.high, n = 3, type = c("lag") ), shift(price.high, n = 2, type = c("lag") ), shift(price.high, n = 1, type = c("lag") ), price.high  ),
                                    l60 = pmin( shift(price.low, n = 11, type = c("lag") ), shift(price.low, n = 10, type = c("lag") ), shift(price.low, n = 9, type = c("lag") ), shift(price.low, n = 8, type = c("lag") ), shift(price.low, n = 7, type = c("lag") ), shift(price.low, n = 6, type = c("lag") ), shift(price.low, n = 5, type = c("lag") ), shift(price.low, n = 4, type = c("lag") ), shift(price.low, n = 3, type = c("lag") ), shift(price.low, n = 2, type = c("lag") ), shift(price.low, n = 1, type = c("lag") ), price.low  ),
                                    c60 = price.close,
                                    v60 = runSum(volume, n =12) ), 
                            by = .(ticker)]


fwrite(data_05min02, "D:/My-Shares/analysis/0550_stock_final_mtf.csv")

py_run_file("D:/My-Shares/prgm/0550_yh_stock_part02_multi_tf_supertrend.py")
all03mtf <- fread("D:/My-Shares/analysis/0550_stock_final03.csv")


all03 <- all03mtf [, `:=` ( up_st = ifelse(SUPERTd_10_2 == 1 & SUPERTd15_10_2 == 1 & SUPERTd30_10_2 == 1 & SUPERTd60_10_2 == 1, 1, 0) ),]
all03 <- all03 [, `:=` (grp_st05 = rleid(SUPERTd_10_2),
                        grp_st15 = rleid(SUPERTd15_10_2),
                        grp_st30 = rleid(SUPERTd30_10_2),
                        grp_st60 = rleid(SUPERTd60_10_2) ),  by = .(ticker)]

all03 <- all03 [, rows_st05 := 1:.N, by = .(ticker, grp_st05)]
all03 <- all03 [, rows_st15 := 1:.N, by = .(ticker, grp_st15)]
all03 <- all03 [, rows_st30 := 1:.N, by = .(ticker, grp_st30)]
all03 <- all03 [, rows_st60 := 1:.N, by = .(ticker, grp_st60)]
all03 <- all03 [, subrow02 := as.ITime (as.ITime("09:15") + subrow*5*60 ), ]

output <- all03 [up_st == 1 & rows_st05 <= 10]
output <- output [, subset := 1:.N, by =.(ticker, trdate)]

output02 <- output [ subset == 1]
output02 <- output02 [, c("ticker", "drow", "trdate", "subrow", "subrow02", "price.open", "price.high", "price.low", "price.close"), ]
output02 <- output02 [, signal := 1, ]

setnames(output02, "price.open", "entry_o")
setnames(output02, "price.high", "entry_h")
setnames(output02, "price.low", "entry_l")
setnames(output02, "price.close", "entry_c")
setnames(output02, "subrow", "entry_row")
setnames(output02, "subrow02", "entry_time")

output02 <- output02 [ order(-trdate, -entry_row) ]

# https://rdrr.io/github/rengelke/tradr/src/R/supertrend.R
# Home / GitHub / rengelke/tradr / R/supertrend.R
# R/supertrend.R
# In rengelke/tradr:
#  Defines functions supertrend
# Documented in supertrend
#' SuperTrend
#'
#' @param HLC xts object that contains High-Low-Close prices
#' @param n period of days
#' @param f Average True Range factor
#'
#' @import xts
#' @import quantmod
#'
#' @return
#' @export
#'
#' @examples supertrend(price_data, n = 10, f = 3)
supertrend <- function(HLC, n = 10, f = 3) {
  
  atr <<- TTR::ATR(HLC, n = n)
  atr <- as.data.table(atr)
  
  upperbasic <- (HLC[, 1] + HLC[, 2])/2 + (f * atr$atr)
  upperfinal <- upperbasic
  close <- HLC[, 3]
  for (i in seq_len(nrow(upperbasic))[-1]) {
    if (isTRUE(close[i-1] < upperfinal[i-1])) {
      upperfinal[i] <- min(upperbasic[i], upperfinal[i-1], na.rm = TRUE)
    } else {
      upperfinal[i] <- upperbasic[i]
    }
  }
  
  lowerbasic <- (HLC[, 1] + HLC[, 2])/2 - (f * atr$atr)
  lowerfinal <- lowerbasic
  close <- HLC[, 3]
  for (i in seq_len(nrow(lowerbasic))[-1]) {
    if (isTRUE(close[i-1] > lowerfinal[i-1])) {
      lowerfinal[i] <- max(lowerbasic[i], lowerfinal[i-1], na.rm = TRUE)
    } else {
      lowerfinal[i] <- lowerbasic[i]
    }
  }
  
  supertrend <- cbind(upperfinal, lowerfinal)
  supertrend$supertrend <- NA
  st <- supertrend$supertrend
  
  cl_greater_upper <- close > upperfinal
  cl_greater_lower <- close > lowerfinal
  cl_less_upper <- close < upperfinal
  cl_less_lower <- close < lowerfinal
  
  for (i in seq_len(nrow(supertrend))[-1]) {
    if ( isTRUE(st[i-1] == upperfinal[i-1]) &
         isTRUE(cl_less_upper[i]) ) {
      st[i] <- upperfinal[i]
    } else if ( isTRUE(st[i-1] == upperfinal[i-1]) &
                isTRUE(cl_greater_upper[i]) ) {
      st[i] <- lowerfinal[i]
    } else if ( isTRUE(st[i-1] == lowerfinal[i-1]) &
                isTRUE(cl_greater_lower[i]) ) {
      st[i] <- lowerfinal[i]
    } else if ( isTRUE(st[i-1] == lowerfinal[i-1]) &
                isTRUE(cl_less_lower[i]) ) {
      st[i] <- upperfinal[i]
    } else if ( isTRUE(cl_less_upper[i]) ) {
      st[i] <- upperfinal[i]
    } else if ( isTRUE(cl_greater_lower[i]) ) {
      st[i] <- lowerfinal[i]
    }
  }
  st
}

super05 <- as.data.table( supertrend(data_05min02[,c("price.high","price.low","price.close"),], n = 10, f = 3) )
super05 <- super05 [, allrow := .I, ]



data(ttrc)
#atr <- ATR(ttrc[,c("High","Low","Close")], n=14)
super <- supertrend(ttrc[,c("High","Low","Close")], n=10, f = 2)
