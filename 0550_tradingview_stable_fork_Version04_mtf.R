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

start_time <- Sys.time()

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

#start_time <- Sys.time()

use_python("C:/ProgramData/Anaconda3/python.exe", required = T)

# Get the source data from yahoo finance, 
# Create a csv file to be passed into supertrend formula

py_run_file("D:/My-Shares/prgm/0550_yh_stock_part01_5min_temp.py")

# Read the csv file created above
# Create supertrend and and again create another csv file
# Create remaining calculations as expected:

#start_time <- Sys.time()

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

########################################
#
# Create 1 day data from 5 mins data
#
########################################

data_05min <- data_05min [, `:=`(minsub = min(subrow),
                                 maxsub = max(subrow), 
                                 high = max(price.high), 
                                 low = min(price.low), 
                                 volumed = sum(volume)), by = .(ticker, trdate)]

data_01dayo <- data_05min [minsub == subrow]
data_01dayc <- data_05min [maxsub == subrow]

setnames(data_01dayo, "price.open", "open")
setnames(data_01dayc, "price.close", "close")

data_01day <- merge(x = data_01dayo [, c("ticker", "trdate", "open", "high", "low", "volumed"), ],
                    y = data_01dayc [, c("ticker", "trdate", "close"), ],
                    by = c("ticker", "trdate"))

rm(data_01dayo, data_01dayc)

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

all02 <- all02 [, `:=` (a15 = ceiling(subrow / 3),
                        a30 = ceiling(subrow / 6),
                        a60 = ceiling(subrow / 12)), ]

all02 <- all02 [, a15vol := sum(volume), by = .(ticker, trdate, a15)]
all02 <- all02 [, a30vol := sum(volume), by = .(ticker, trdate, a30)]
all02 <- all02 [, a60vol := sum(volume), by = .(ticker, trdate, a60)]

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

a15 <- data_05min02 [ subrow / 3 == a15, c("ticker", "trdate", "trdtme", "subrow", "a15", "a30", "a60", "v15", "o15", "h15", "l15", "c15"), ]
a30 <- data_05min02 [ subrow / 6 == a30, c("ticker", "trdate", "trdtme", "subrow", "a15", "a30", "a60", "v30", "o30", "h30", "l30", "c30"), ]
a60 <- data_05min02 [ subrow / 12 == a60, c("ticker", "trdate", "trdtme", "subrow", "a15", "a30", "a60", "v60", "o60", "h60", "l60", "c60"), ]

setnames(a15, "o15", "price.open")
setnames(a15, "h15", "price.high")
setnames(a15, "l15", "price.low")
setnames(a15, "c15", "price.close")
setnames(a15, "v15", "volume")

setnames(a30, "o30", "price.open")
setnames(a30, "h30", "price.high")
setnames(a30, "l30", "price.low")
setnames(a30, "c30", "price.close")
setnames(a30, "v30", "volume")

setnames(a60, "o60", "price.open")
setnames(a60, "h60", "price.high")
setnames(a60, "l60", "price.low")
setnames(a60, "c60", "price.close")
setnames(a60, "v60", "volume")

fwrite(all02 [, c("ticker", "trdate", "trdtme", "subrow", "a15", "a30", "a60", "volume", "price.open", "price.high", "price.low", "price.close"), ], "D:/My-Shares/analysis/0551_5min_data_stin.csv")
fwrite(a15, "D:/My-Shares/analysis/0551_15min_data_stin.csv")
fwrite(a30, "D:/My-Shares/analysis/0551_30min_data_stin.csv")
fwrite(a60, "D:/My-Shares/analysis/0551_60min_data_stin.csv")

py_run_file("D:/My-Shares/prgm/0551_yh_stock_part02_multi_tf_supertrend_trial.py")

all03_05 <- fread("D:/My-Shares/analysis/0551_5min_data_stout.csv")
all03_15 <- fread("D:/My-Shares/analysis/0551_15min_data_stout.csv")
all03_30 <- fread("D:/My-Shares/analysis/0551_30min_data_stout.csv")
all03_60 <- fread("D:/My-Shares/analysis/0551_60min_data_stout.csv")

all03 <- Reduce(function(...) merge(..., by = c("ticker", "trdate", "trdtme", "subrow"), all=T),  
                list( all03_05 [, c(2:5, 9:17), ], 
                      all03_15 [, c(2:5, 14:17), ], 
                      all03_30 [, c(2:5, 14:17), ], 
                      all03_60 [, c(2:5, 14:17), ]) )


ll <- list(all03_05, all03_15, all03_30, all03_60)
all03 <- rbindlist(ll, idcol = "file")

all03 <- rbind(list(all03_05, all03_15, all03_30, all03_60), idcol = "file")


end_time <- Sys.time()
end_time - start_time