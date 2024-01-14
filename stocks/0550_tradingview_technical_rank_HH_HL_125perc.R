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
#library(tidyquant)
#library(QuantTools)
#library(derivmkts)
library(quantmod)
#library(openxlsx)
library(data.table)
library(tidyverse)
library(anytime)
#library(simstudy)
library(zoo)
#library(RCurl)
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

fno <- fread("https://archives.nseindia.com/content/fo/fo_mktlots.csv")
fno <- fno [, c("SYMBOL"), ]
fno <- fno [, `:=` (nrow = .I, SYMBOL02 = paste(SYMBOL, ".NS", sep="") ), ]

# For NIFTY 500
fno <- fread("C:\\Users\\vyomv\\Downloads\\ind_nifty500list.csv")
fno <- fno [, c("Symbol"), ]
fno <- fno [, `:=` (nrow = .I, SYMBOL02 = paste(Symbol, ".NS", sep="") ), ]


a01nfity50 <-  BatchGetSymbols(
  tickers = fno$SYMBOL02, #c("^CNXAUTO", "^NSEI"), 
  first.date = Sys.Date() - 2000, #"2008-01-01", #Sys.Date() - 5000,
  last.date = Sys.Date(),
  thresh.bad.data = 0.75,
  bench.ticker = "^NSEI",
  type.return = "arit",
  freq.data = "daily",
  how.to.aggregate = "last",
  do.complete.data = FALSE,
  
  do.fill.missing.prices = TRUE,
  do.cache = TRUE,
  cache.folder = file.path(tempdir(), "BGS_Cache"),
  do.parallel = FALSE, #TRUE, # FALSE
  be.quiet = FALSE
)

all02 <- data.table(a01nfity50$df.tickers)
all02 <- all02 [, trdate := anydate(ref.date), ]

all02 <- all02 [ order(ticker, trdate)]
all02 <- all02 [, allrow := .I, ]
all02 <- all02 [, nrow := 1:.N, by =.(ticker)]


roc_n <- 20
roc_dn = as.data.table( ROC(all02$price.close, n = roc_n) )
roc_dn <- roc_dn [, allrow := .I, ]
setnames(roc_dn, "V1", "roc20")


roc_n02 <- 125
roc_dn02 = as.data.table( ROC(all02$price.close, n = roc_n02) )
roc_dn02 <- roc_dn02 [, allrow := .I, ]
setnames(roc_dn02, "V1", "roc125")

adx_n <- 14
adx_dn = as.data.table( ADX(all02[,c("price.high","price.low","price.close"),], n = adx_n) )
adx_dn <- adx_dn [, allrow := .I, ]

all02 <- Reduce(function(...) merge(..., by = c("allrow"), all=T),  
                list( all02, roc_dn, roc_dn02, adx_dn) )

# Calculate the Bollinger band, the calculations are done over all the dataset
# So the first n lines of the bollinger band calculations go wrong
# as the data from previous company is carried forward
#
# Remove the calculations for first 20 * 2 rows

# To avoid any incorrect calculations explained above, remove certain number of rows

all03 <- all02 [ nrow > roc_n02]
all03 <- all03 [, turnover := price.close * volume, ]
all03 <- all03 [, `:=`(ma200 = SMA(price.close, 200),
                       ma150  = SMA(price.close, 150),
                       ma50  = SMA(price.close, 50),
                       ma12 = EMA(price.close, 12),
                       ma26 = EMA(price.close, 26), 
                       turn26 = SMA(turnover, 26),
                       turn52 = SMA(turnover, 52),
                       turn104 = SMA(turnover, 104) ), by = .(ticker)]

# Use price close comparison
all04 <- all03 [ price.close > shift(price.close, type = c("lag"), n = 5) &
                 price.close > shift(price.close, type = c("lag"), n = 20) * 1.2 & price.close < shift(price.close, type = c("lag"), n = 20) * 1.4 &
                 price.close > shift(price.close, type = c("lag"), n = 50) * 1.2 & 
                 turn26 > turnover &
                 ma50 > ma150 &
                 ADX > 25 & DIp > 20 & DIp > DIn]

all04 <- all04  [ order(-trdate, -turnover) ]


# Use price.high

all04 <- all03 [ price.close >= shift(price.high, type = c("lag"), n = 5) &
                 price.close >= shift(price.high, type = c("lag"), n = 20) &
                 price.close >= shift(price.high, type = c("lag"), n = 50) &
                 price.close >= shift(price.high, type = c("lag"), n = 100) &
                 turn26 > turnover &
                 ma50 > ma150 &
                 ADX > 25 & DIp > 20 & DIp > DIn]

all04 <- all04  [ order(-trdate, -turnover) ]


# Use price going higher high say till 10 days ago and 
# low not broken and 
# then would have gone flat

all04 <- all03 [ shift(price.high, type = c("lag"), n = 15) < shift(price.high, type = c("lag"), n = 10) &
                 shift(price.high, type = c("lag"), n = 30) < shift(price.high, type = c("lag"), n = 10) & 
                 shift(price.high, type = c("lag"), n = 30) < shift(price.high, type = c("lag"), n = 15) &
                   
                 price.low > shift(price.low, type = c("lag"), n = 5) & 
                 price.low > shift(price.low, type = c("lag"), n = 7) &
                 price.low > shift(price.low, type = c("lag"), n = 9) & 
                   
                 shift(price.low, type = c("lag"), n = 5) > shift(price.low, type = c("lag"), n = 7) &
                 shift(price.low, type = c("lag"), n = 7) > shift(price.low, type = c("lag"), n = 9) &
                 shift(price.low, type = c("lag"), n = 9) > shift(price.low, type = c("lag"), n = 13) &
                 turn26 > turnover &
                 ma50 > ma150 &
                 price.close / shift(price.close, type = c("lag"), n = 200) > 1.25 ]

all04 <- all04  [ order(-trdate, -turnover) ]
