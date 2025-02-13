
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
#options(digits=2)

#library(BatchGetSymbols)

#future.seed = TRUE
#options(future.rng.onMisuse="ignore")

#future::plan(future::multisession, workers = floor(parallel::detectCores() ))

start_time <- Sys.time()

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

py_run_file("D:/My-Shares/prgm/swing_trade/0600_yh_stock_part01_1d.py")

pickle_data <- as.data.table (py$data)
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

all02 <- stock_final # [, -c("NA"), ]
all02 <- all02 [, `:=`(price.open = as.numeric(price.open), 
                       price.high = as.numeric(price.high), 
                       price.low = as.numeric(price.low), 
                       price.close = as.numeric(price.close), 
                       volume = as.numeric(volume)), ]

all02 <- all02 [, ema13 := EMA (price.close, n = 13), by = .(ticker)]

all02 <- na.omit(all02)
all02 <- all02 [, allrow := .I, ]
all02 <- all02 [, nrow := 1:.N, by = .(ticker)]

all02 <- all02 [, mfi09 := MFI (ema13, volume, n = 9), by = .(ticker)]
all02 <- all02 [, mfi09avg := EMA (mfi09, n = 5), by = .(ticker)]

bb_n <- 20
bb_dn = as.data.table( BBands(all02$price.close, n = bb_n, k = 2) )
bb_dn <- bb_dn [, allrow := .I, ]

all02 <- Reduce(function(...) merge(..., by = c("allrow"), all=T),  
                list( all02, bb_dn ) )

all02 <- all02 [, filter001 := ifelse( shift(mfi09, type = c("lag"), n = 1) >= 99 & mfi09 < 99 & shift(mfi09avg, type = c("lag"), n = 1) >= 99 & mfi09avg < 99, 1, 0), by = .(ticker)]
