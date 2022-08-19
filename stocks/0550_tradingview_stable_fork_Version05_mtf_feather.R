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

# Python Modules are usually stored in /lib/site-packages in your Python folder. If you want to see 

# In
# Go to: Control Panel-->System-->Advanced-->Environment Variables.
# Under 'System Variables' click New.
# Variable name: R_HOME

# R_HOME: C:\Program Files\R\R-4.1.3

# Highlight the 'Path' variable and click Edit. Make sure that C:\Program Files\R\R-4.1.3 is somewhere in that long list of pathways.

if (! "ttr"        %in% tolower ( (.packages() ) ) ) { library(TTR)        } else { print ("The library TTR is already loaded") }
if (! "data.table" %in% tolower ( (.packages() ) ) ) { library(data.table) } else { print ("The library data.table is already loaded") }
if (! "tidyverse"  %in% tolower ( (.packages() ) ) ) { library(tidyverse)  } else { print ("The library tidyverse is already loaded") }
if (! "anytime"    %in% tolower ( (.packages() ) ) ) { library(anytime)    } else { print ("The library anytime is already loaded") }
if (! "zoo"        %in% tolower ( (.packages() ) ) ) { library(zoo)        } else { print ("The library zoo is already loaded") }
if (! "lubridate"  %in% tolower ( (.packages() ) ) ) { library(lubridate)  } else { print ("The library lubridate is already loaded") }
if (! "arrow"      %in% tolower ( (.packages() ) ) ) { library(arrow)      } else { print ("The library arrow is already loaded") }
if (! "reticulate" %in% tolower ( (.packages() ) ) ) { library(reticulate) } else { print ("The library reticulate is already loaded") }

# read_feather function from this library works but not from feather

#if (! "openxlsx" %in% tolower ( (.packages() ) ) )   { library(openxlsx) }   else { print ("The library openxlsx is already loaded") }
#if (! "RCurl" %in% tolower ( (.packages() ) ) )      { library(RCurl) }      else { print ("The library RCurl is already loaded") }
#if (! "curl" %in% tolower ( (.packages() ) ) )       { library(curl) }       else { print ("The library curl is already loaded") }
#if (! "plotly" %in% tolower ( (.packages() ) ) )     { library(plotly) }     else { print ("The library plotly is already loaded") }

options(scipen = 999)

#library(BatchGetSymbols)

#future.seed = TRUE
#options(future.rng.onMisuse="ignore")

#future::plan(future::multisession, workers = floor(parallel::detectCores() ))

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

file_crea <- file.info("D:/My-Shares/analysis/icici_fno.rds")$mtime
compdt <- as.POSIXct( paste(Sys.Date(), "09:15:00", sep = ""), tz=Sys.timezone())

if (is.na(file_crea) ) {print("File does not exist, the source code must be executed to create mapping file")
  source("D:\\My-Shares\\prgm\\0550_tradingview_yh_icici_map.R") 
  file_crea <- file.info("D:/My-Shares/analysis/icici_fno.rds")$mtime
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

#use_python("C:/ProgramData/Anaconda3/python.exe", required = T)

use_python("C:/Program Files/Python310/python.exe", required = T)

# Get the source data from yahoo finance, 
# Create a csv file to be passed into supertrend formula

py_run_file("D:/My-Shares/prgm/0550_yh_stock_part01_5min_temp.py")

pickle_data <- as.data.table (py$data)

pd <- import("pandas")
#pickle_data <- as.data.table ( pd$read_pickle("D:/My-Shares/analysis/0550_data.pkl") )
pickle_data <- pickle_data [, nrow := 1:.N, ]

dnames <- as.data.table ( names(pickle_data) )
dnames <- dnames [, nrow := 1:.N, ]
dnames <- dnames [, nrow02 := ceiling(nrow / 6), ]
dnames <- dnames [, subrow := 1:.N, by = .(nrow02)]
dnames <- dnames [, v10 := str_squish(V1), ]
dnames <- dnames [, v10 := ifelse(v10 == "Adj Close", "Adj_Close", v10), ]
dnames <- dnames [, v11 := ifelse(subrow > 1, paste(" ", v10, sep=""), v10), ]
dnames <- dnames [, c("tmp01", "tmp02") := tstrsplit(v11, " ", fill =""), ]
dnames02 <- dnames [subrow == 1, c("tmp01", "nrow02"), ]

dnames03 <- merge(x = dnames [, -c("tmp01"), ],
                  y = dnames02,
                  by = c("nrow02"))

dnames03 <- dnames03 [, tmp03 := paste(tmp01, tmp02, sep="_"),]
dnames03 <- dnames03 [, chg001 := paste("setnames (pickle_data, '", V1, "', '", tmp03, "')", sep = "") , ]

eval(parse(text = dnames03$chg001))

pickle_data_t <- melt(data = pickle_data, id.vars = c("Datetime_", "Datetime_nrow"))
data03_t <- pickle_data_t [, c("Name", "ohlcv", "tmp") := tstrsplit(variable, "_"), ]
stock_final <- dcast(data = data03_t [, -c("tmp"), ],
                     Datetime_ + Datetime_nrow + Name ~ ohlcv, 
                     value.var = c("value") )

setnames (stock_final, "Datetime_", "Datetime")
stock_final <- as.data.table(stock_final)
stock_final <- stock_final [, trdtme := format(Datetime, tz="Asia/Calcutta"), ]
stock_final <- stock_final [, trdate := anydate(str_sub(trdtme, 1, 10) ), ]
stock_final <- stock_final [ order(Name, trdtme) ]
stock_final <- stock_final [, nrow := 1:.N, by = .(Name)]
stock_final <- stock_final [, subrow := 1:.N, by = .(Name, trdate)]

setnames(stock_final, c("Open", "High", "Low", "Close", "Volume", "Name"), 
         c("price.open", "price.high", "price.low", "price.close", "volume", "ticker") )


all02 <- stock_final [, -c("NA"), ]
all02 <- all02 [, `:=`(price.open = as.numeric(price.open), 
                       price.high = as.numeric(price.high), 
                       price.low = as.numeric(price.low), 
                       price.close = as.numeric(price.close), 
                       volume = as.numeric(volume)), ]
all02 <- na.omit(all02)
all02 <- all02 [, allrow := .I, ]

adx_n <- 14
adx_dn = as.data.table( ADX(all02[,c("price.high","price.low","price.close"),], n = adx_n) )
adx_dn <- adx_dn [, allrow := .I, ]

data_05min <- all02 [ order(ticker, trdate)]

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
#all02 <- all02 [, a60vol := sum(volume), by = .(ticker, trdate, a60)]

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

#data_05min02 <- data_05min02 [, `:=` (o60 = shift(price.open, n = 11, type = c("lag") ),
#                                      h60 = pmax( shift(price.high, n = 11, type = c("lag") ), shift(price.high, n = 10, type = c("lag") ), shift(price.high, n = 9, type = c("lag") ), shift(price.high, n = 8, type = c("lag") ), shift(price.high, n = 7, type = c("lag") ), shift(price.high, n = 6, type = c("lag") ), shift(price.high, n = 5, type = c("lag") ), shift(price.high, n = 4, type = c("lag") ), shift(price.high, n = 3, type = c("lag") ), shift(price.high, n = 2, type = c("lag") ), shift(price.high, n = 1, type = c("lag") ), price.high  ),
#                                      l60 = pmin( shift(price.low, n = 11, type = c("lag") ), shift(price.low, n = 10, type = c("lag") ), shift(price.low, n = 9, type = c("lag") ), shift(price.low, n = 8, type = c("lag") ), shift(price.low, n = 7, type = c("lag") ), shift(price.low, n = 6, type = c("lag") ), shift(price.low, n = 5, type = c("lag") ), shift(price.low, n = 4, type = c("lag") ), shift(price.low, n = 3, type = c("lag") ), shift(price.low, n = 2, type = c("lag") ), shift(price.low, n = 1, type = c("lag") ), price.low  ),
#                                      c60 = price.close,
#                                      v60 = runSum(volume, n =12) ), 
#                              by = .(ticker)]

a15 <- data_05min02 [ subrow / 3 == a15, c("ticker", "trdate", "trdtme", "subrow", "a15", "a30", "a60", "v15", "o15", "h15", "l15", "c15"), ]
a30 <- data_05min02 [ subrow / 6 == a30, c("ticker", "trdate", "trdtme", "subrow", "a15", "a30", "a60", "v30", "o30", "h30", "l30", "c30"), ]
#a60 <- data_05min02 [ subrow / 12 == a60, c("ticker", "trdate", "trdtme", "subrow", "a15", "a30", "a60", "v60", "o60", "h60", "l60", "c60"), ]

setnames(a15, c("o15", "h15", "l15", "c15", "v15"), 
         c("price.open", "price.high", "price.low", "price.close", "volume") )

setnames(a30, c("o30", "h30", "l30", "c30", "v30"), 
         c("price.open", "price.high", "price.low", "price.close", "volume") )


a05 <- all02 [, c("ticker", "trdate", "trdtme", "allrow", "subrow", "a15", "a30", "a60", "volume", "price.open", "price.high", "price.low", "price.close"), ]

write_feather(a05, "D:/My-Shares/analysis/0551_5min_data_stin.feather")
write_feather(a15, "D:/My-Shares/analysis/0551_15min_data_stin.feather")
write_feather(a30, "D:/My-Shares/analysis/0551_30min_data_stin.feather")

#############################################################################
#
# As the feather library from python was not getting read correctly
# Switched from py_run_file command to system command to execute the program
#
#############################################################################

system(paste('"c:/Program Files/Python310/python.exe"', 'D:/My-Shares/prgm/0551_yh_stock_part02_multi_tf_supertrend_trial_feather02.py'))

#################################################################
#
# Part 3:
#
# Get the signal calculations done here
# ST5, St15, ST30 aligned
# DIp / Din >= 3 or Din / DIp >= 3 and ADX <= 30 or ADX <= 40
# Price compared to previous day <= 1% 
#
#################################################################

all03_05 <- as.data.table( arrow::read_feather("D:/My-Shares/analysis/0551_5min_data_stout.feather") )
all03_15 <- as.data.table( arrow::read_feather("D:/My-Shares/analysis/0551_15min_data_stout.feather") )
all03_30 <- as.data.table( arrow::read_feather("D:/My-Shares/analysis/0551_30min_data_stout.feather") )

all03 <- Reduce(function(...) merge(..., by = c("ticker", "trdtme", "subrow"), all=T),  
                list( all03_05, #[, c(1:1, 3:4, 8:14), ], 
                      all03_15 [, c(1:1, 3:4, 13:14), ], 
                      all03_30 [, c(1:1, 3:4, 13:14), ]) )

all03 <- all03 [, `:=`(st15 = na.locf(SUPERT15_10_2, na.rm = FALSE),
                       std15 = na.locf(SUPERTd15_10_2, na.rm = FALSE),
                       st30 = na.locf(SUPERT30_10_2, na.rm = FALSE),
                       std30 = na.locf(SUPERTd30_10_2, na.rm = FALSE) ), by = .(ticker)]

all03 <- all03 [, -c("SUPERT15_10_2", "SUPERTd15_10_2", "SUPERT30_10_2", "SUPERTd30_10_2"), ]

all03 <- merge(x = all03,
               y = adx_dn,
               by = c("allrow"))

all03 <- merge (x = all03,
                y = all02 [, c("ticker", "trdate", "trdtme", "allrow", "close",
                                "vR0", "vR0236", "vR0382", "vR05", "vR0618", "vR0786", "vR1", "vR1272", "vR1414", "vR1618", "vR2618",
                                "vS0", "vS0236", "vS0382", "vS05", "vS0618", "vS0786", "vS1", "vS1272", "vS1414", "vS1618", "vS2618"), ], 
                by = c("ticker", "trdate", "trdtme", "allrow"))

setnames(all03, c("SUPERT05_10_2", "SUPERTd05_10_2"), 
         c("st05", "std05") )

all03 <- all03 [, `:=`(long001 = ifelse(std05 == 1 & std15 == 1 & std30 == 1, 1, 0),
                       long002 = ifelse(DIp / DIn > 3 & ADX <= 30, 1, 0),
                       prc003 = ifelse( abs( (price.close - close)/ close) * 100 < 1, 1, 0),
                       
                       short001 = ifelse(std05 == -1 & std15 == -1 & std30 == -1, 1, 0),
                       short002 = ifelse(DIn / DIp > 3 & ADX <= 30, 1, 0)  ),]
all03 <- all03 [, subrow02 := as.ITime (as.ITime("09:15") + subrow*5*60 ), ]

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

output <- trial001 [long001 == 1 & long002 == 1 & prc003 == 1]

# See if the EMA cross over OR ST change as the primary signal
#output <- trial001 [((up_st == 1 & rows_st <= 5) |(up_ema == 1 & rows_ema <= 5)) & up_adx == 1 & rows_adx <= 5 & nrank <= 5 & perchg <= 0.75]

output <- output [, subset := 1:.N, by =.(ticker, trdate)]

output02 <- output [ subset == 1]
output02 <- output02 [, c("ticker", "trdate", "subrow", "subrow02", "price.open", "price.high", "price.low", "price.close"), ]
output02 <- output02 [, signal := 1, ]

setnames(output02, "price.open", "entry_o")
setnames(output02, "price.high", "entry_h")
setnames(output02, "price.low", "entry_l")
setnames(output02, "price.close", "entry_c")
setnames(output02, "subrow", "entry_row")
setnames(output02, "subrow02", "entry_time")

# Merge this data with the original data

trial002 <- merge (x = trial001, 
                   y = output02, 
                   by = c("ticker", "trdate"))

output02 <- output02 [ order(-trdate, -entry_row)]

trial002 <- trial002 [ subrow >= entry_row]
trial002 <- trial002 [, c("ticker", "trdate", "subrow", "subrow02", "entry_row", "signal",
                          "entry_o", "entry_h", "entry_l", "entry_c",
                          "price.open", "price.high", "price.low", "price.close", "volume",
                          "vR0", "vR0236", "vR0382", "vR05", "vR0618", "vR0786", "vR1", "vR1272", 
                          "vR1414", "vR1618", "vR2618"), ]

trial002 <- trial002 [, temp_prc := round( (entry_h * 1.002)/5, 2),  ]
trial002 <- trial002 [, nshares := round( (100000 / temp_prc) * 0.8 , 0),  ]

trial002 <- trial002 [ order(-trdate, -subrow) ]


trial002_t <- melt.data.table(data = trial002 [ subrow == entry_row ],
                              id.vars = c("ticker", "trdate", "subrow", "subrow02", "entry_row",  "signal",
                                          "entry_o", "entry_h", "entry_l", "entry_c", "temp_prc", "nshares",
                                          "price.open", "price.high", "price.low", "price.close", "volume"), 
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
                             ticker + ShortName + trdate + entry_row ~ paste("t", str_pad(tgt, 2, pad="0"), sep ="_"),
                             fill =" ")

trial004 <- merge(x = trial002,
                  y = trial003,
                  by = c("ticker", "trdate", "entry_row"))

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

#py_run_file("D:/My-Shares/prgm/0550_icici_02_orders.py")


########################################################################################################
#
#
# End of program
#
#
########################################################################################################


#
# Various attempts of pkl file
#

#all03_05 <- as.data.table( py$stock_final05 )
#all03_15 <- as.data.table( py$stock_final15 )
#all03_30 <- as.data.table( py$stock_final30 )
#all03_60 <- as.data.table( py$stock_final60 )

#py_save_object(a05, "D:/My-Shares/analysis/0551_5min_data_stin.pkl")
#py_save_object(a15, "D:/My-Shares/analysis/0551_15min_data_stin.pkl")
#py_save_object(a30, "D:/My-Shares/analysis/0551_30min_data_stin.pkl")
#py_save_object(a60, "D:/My-Shares/analysis/0551_60min_data_stin.pkl")

#pd$to_pickle(a05, "D:/My-Shares/analysis/0551_5min_data_stin.pkl")
#pd$to_pickle(a15, "D:/My-Shares/analysis/0551_15min_data_stin.pkl")
#pd$to_pickle(a30, "D:/My-Shares/analysis/0551_30min_data_stin.pkl")
#pd$to_pickle(a60, "D:/My-Shares/analysis/0551_60min_data_stin.pkl")

#py_run_file("D:/My-Shares/prgm/0551_yh_stock_part02_multi_tf_supertrend_trial_feather.py")

#all03_05 <- as.data.table ( py_load_object("D:/My-Shares/analysis/0551_5min_data_stout.pkl") )
#all03_15 <- as.data.table ( py_load_object("D:/My-Shares/analysis/0551_15min_data_stout.pkl") )
#all03_30 <- as.data.table ( py_load_object("D:/My-Shares/analysis/0551_30min_data_stout.pkl") )
#all03_60 <- as.data.table ( py_load_object("D:/My-Shares/analysis/0551_60min_data_stout.pkl") )

#all03_05 <- as.data.table ( pd$read_pickle("D:/My-Shares/analysis/0551_5min_data_stout.pkl") )
#all03_15 <- as.data.table ( pd$read_pickle("D:/My-Shares/analysis/0551_15min_data_stout.pkl") )
#all03_30 <- as.data.table ( pd$read_pickle("D:/My-Shares/analysis/0551_30min_data_stout.pkl") )
#all03_60 <- as.data.table ( pd$read_pickle("D:/My-Shares/analysis/0551_60min_data_stout.pkl") )
