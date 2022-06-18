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


library(reticulate)

start_time <- Sys.time()

use_python("C:/ProgramData/Anaconda3/python.exe", required = T)
#py_run_file("D:/My-Shares/prgm/0550_py_from_r_ranking.py")

# Get the source data from yahoo finance, 
# Create a csv file to be passed into supertrend formula

py_run_file("D:/My-Shares/prgm/0551_mtf_rsi_download.py")

end_time <- Sys.time()
end_time - start_time



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
  all02 <- all02 [, allrow := .I, ]
  all02 <- all02 [, nrow := 1:.N, by =.(ticker)]
  all02 <- all02 [, mfi09 := MFI(price.close, volume, 9), by = .(ticker)]
  
  assign(deparse(substitute(outdata)), all02, envir=.GlobalEnv)
  
}

calcs(infile = "D:/My-Shares/analysis/0551_5min_data.csv", outdata = data_05min)
calcs(infile = "D:/My-Shares/analysis/0551_15min_data.csv", outdata = data_15min)
calcs(infile = "D:/My-Shares/analysis/0551_30min_data.csv", outdata = data_30min)
calcs(infile = "D:/My-Shares/analysis/0551_60min_data.csv", outdata = data_60min)


fwrite(data_05min, "D:/My-Shares/analysis/0551_5min_data_stin.csv")
fwrite(data_15min, "D:/My-Shares/analysis/0551_15min_data_stin.csv")
fwrite(data_30min, "D:/My-Shares/analysis/0551_30min_data_stin.csv")
fwrite(data_60min, "D:/My-Shares/analysis/0551_60min_data_stin.csv")


py_run_file("D:/My-Shares/prgm/0551_yh_stock_part02_multi_tf_supertrend_trial.py")

all03_05 <- fread("D:/My-Shares/analysis/0551_5min_data_stout.csv")
all03_15 <- fread("D:/My-Shares/analysis/0551_15min_data_stout.csv")
all03_30 <- fread("D:/My-Shares/analysis/0551_30min_data_stout.csv")
all03_60 <- fread("D:/My-Shares/analysis/0551_60min_data_stout.csv")

ll <- list(all03_05, all03_15, all03_30, all03_60)
all03 <- rbindlist(ll, idcol = "file")


all03 <- all03_05 [, `:=` (a15 = ceiling(subrow / 3),
                        a30 = ceiling(subrow / 6),
                        a60 = ceiling(subrow / 12)), ]

all03 <- all03 [, a15vol := sum(volume), by = .(ticker, trdate, a15)]
all03 <- all03 [, a30vol := sum(volume), by = .(ticker, trdate, a30)]
all03 <- all03 [, a60vol := sum(volume), by = .(ticker, trdate, a60)]

a15 <- all03 [ subrow / 3 == a15, c("ticker", "trdate", "trdtme", "subrow", "a15", "a30", "a60", "a15vol", "price.close")]
a30 <- all03 [ subrow / 6 == a30, c("ticker", "trdate", "trdtme", "subrow", "a15", "a30", "a60", "a30vol", "price.close")]
a60 <- all03 [ subrow / 12 == a60, c("ticker", "trdate", "trdtme", "subrow", "a15", "a30", "a60", "a60vol", "price.close")]

a15 <- a15 [, mfi15 := MFI(price.close, a15vol, 9), by = .(ticker, a15)]
a30 <- a30 [, mfi30 := MFI(price.close, a30vol, 9), by = .(ticker, a30)]
a60 <- a60 [, mfi60 := MFI(price.close, a60vol, 9), by = .(ticker, a60)]

a05 <- all03 [, c("ticker", "trdate", "trdtme", "subrow", "a15", "a30", "a60", "mfi09")]

a_mfi <- merge(x = a05,
               y = a15 [, c("ticker", "trdate", "a15", "mfi15"), ],
               by = c("ticker", "trdate", "a15"), 
               all = TRUE)

a_mfi <- merge(x = a_mfi,
               y = a30 [, c("ticker", "trdate", "a30", "mfi30"), ],
               by = c("ticker", "trdate", "a30"), 
               all = TRUE)

a_mfi <- merge(x = a_mfi,
               y = a60 [, c("ticker", "trdate", "a60", "mfi60"), ],
               by = c("ticker", "trdate", "a60"), 
               all = TRUE)
a_mfi02 <- a_mfi [ order(-trdate, -subrow02) ]
a_mfi02 <- a_mfi02 [, chk := ifelse(mfi09 > mfi15 & mfi09 > mfi30 & mfi09 > mfi60, 1, 0), ]
