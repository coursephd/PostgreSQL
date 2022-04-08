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

###############################
#
# Python calculations
#
###############################

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

data_01day <- data_01day [, drow := 1:.N, by = .(ticker)]

all02 <- merge.data.table (x = data_05min, 
                y = data_01day [, c("ticker", "trdate", "open", "high", "low", "close", "volumed", "drow"), ],
                by = c("ticker", "trdate"))

all02 <- all02 [, allrow := .I, ]
all02 <- all02 [, nrow := 1:.N, by = .(ticker)]

all02 <- all02 [, vPP := as.numeric(round( (shift(high, n =1, type = c("lag")) + 
                                              shift(low, n =1, type = c("lag")) + 
                                              shift(close, n =1, type = c("lag"))  )/ 3), 2 ), ]
all02 <- all02 [, `:=`(phigh = shift(high, n =1, type = c("lag")), 
                       plow = shift(low, n =1, type = c("lag")) ), by =.(ticker)]

all02 <- all02 [, `:=`(vR0 = vPP + (phigh - plow) * 0,
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


##################################################################################################
# TRI components:
##################################################################################################



##################################################################################################
#
#
# For trade management
#
#
###################################################################################################

all03 <- fread("D:/My-Shares/analysis/0550_stock_final03.csv")
all03 <- all03 [, ST := ifelse(SUPERTd_20_2.7 == 1, 0.15, 0), ]

all03 <- all03 [, `:=` ( up_st = ifelse(SUPERTd_20_2.7 == 1, 1, 0),
                         up_mfi = ifelse(mfi09 >= wma21mfi09, 1, 0),
                         up_adx = ifelse(DIp >= 25, 1, 0),
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

all03 <- all03 [, up_tot := up_st + up_mfi + up_adx + up_ema + up_vwap, ]

all03 <- all03 [, trank := round(longtermma + longtermroc + midtermma + midtermroc + stPpo + stRsi + stDlp + ST, 2), ]
all03 <- all03 [, trank := as.numeric(trank) * up_tot, ]


#################################################
#
# New addition for ST in uptrend and adx positive
#
#################################################
#all03 <- all03 [ up_st == 1 & up_adx == 1]

all03 <- all03 [ order(trdate, subrow, -trank)]
all03 <- all03 [, nrank := 1:.N, by = .(trdate, subrow)]

# Count number of times the stock is in top 10 on a rolling basis of 10 days

all03 <- all03 [, top10 := ifelse(nrank <= 10, 1, 0), ]
all03 <- all03 [, cumtop10 :=runSum(top10, n = 15 ), by =.(ticker) ]
all03 <- all03 [, ticker02 := paste(ticker, ",", trank, ",", cumtop10, ",rows_st =", rows_st, ",rows_adx =", rows_adx, sep=""), ]
all03 <- all03 [, subrow02 := as.ITime (as.ITime("09:15") + subrow*5*60 ), ]

trial001 <- copy(all03)
#trial001 <- trial001 [ trdate == "2022-04-04"]

#output <- trial001 [up_st == 1 & up_adx == 1 & up_mfi == 1 & up_ema == 1 & nrank <= 15]
output <- trial001 [up_st == 1 & up_adx == 1 & up_ema == 1 ] #& nrank <= 15]
output <- output [, subset := 1:.N, by =.(ticker, trdate)]

output02 <- output [ subset == 1]
output02 <- output02 [, c("ticker", "trdate", "subrow", "price.open", "price.high", "price.low", "price.close"), ]
output02 <- output02 [, signal := 1, ]

setnames(output02, "price.open", "entry_o")
setnames(output02, "price.high", "entry_h")
setnames(output02, "price.low", "entry_l")
setnames(output02, "price.close", "entry_c")
setnames(output02, "subrow", "entry_row")

# Merge this data with the original data

trial002 <- merge (x = trial001, 
                   y = output02, 
                   by = c("ticker", "trdate"))

trial002 <- trial002 [ subrow >= entry_row]
trial002 <- trial002 [, c("ticker", "trdate", "subrow", "subrow02", "entry_row", "nrank", "signal",
                          "entry_o", "entry_h", "entry_l", "entry_c",
                          "price.open", "price.high", "price.low", "price.close", "volume",
                          "SUPERT_20_2.7", "SUPERTd_20_2.7", "SUPERTl_20_2.7", "SUPERTs_20_2.7"), ]

trial002 <- trial002 [, temp_prc := round( (entry_h * 1.02)/5, 2),  ]
trial002 <- trial002 [, nshares := round( (100000 / temp_prc) * 0.8 , 0),  ]

########################################################################################################
#
#
# End of program
#
#
########################################################################################################
