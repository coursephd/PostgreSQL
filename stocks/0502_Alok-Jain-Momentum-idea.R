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

# https://www1.nseindia.com/content/equities/EQUITY_L.csv           : List of all companies
# https://www1.nseindia.com/content/indices/ind_nifty500list.csv    : List of Nifty500 companies

# cntrt <- fread('https://www1.nseindia.com/content/equities/EQUITY_L.csv')
# cntrt02 <- cntrt [, `:=` (nrow = .I, SYMBOL02 = paste(SYMBOL, ".NS", sep="") ), ]
# setnames(cntrt02, "ISIN NUMBER", "ISIN")

cntrt <- fread('https://www1.nseindia.com/content/indices/ind_niftysmallcap100list.csv')
cntrt02 <- cntrt [, `:=` (nrow = .I, SYMBOL02 = paste(Symbol, ".NS", sep="") ), ]
setnames(cntrt02, "ISIN Code", "ISIN")


a01 <-  BatchGetSymbols(
  tickers = cntrt02$SYMBOL02,
  first.date = Sys.Date() - 5000,
  last.date = Sys.Date(),
  thresh.bad.data = 0.2,
  bench.ticker = "^NSEI",
  type.return = "arit",
  freq.data = "weekly",
  how.to.aggregate = "last",
  do.complete.data = FALSE,
  do.fill.missing.prices = TRUE,
  do.cache = TRUE,
  cache.folder = file.path(tempdir(), "BGS_Cache"),
  do.parallel = TRUE, # FALSE
  be.quiet = FALSE
)

a01con <- data.table(a01$df.control)
a02 <- data.table(a01$df.tickers)

#
# Extract the Nifty50 data
#

a01nfity50 <-  BatchGetSymbols(
  tickers = "^NSEI",
  first.date = Sys.Date() - 5000,
  last.date = Sys.Date(),
  thresh.bad.data = 0.75,
  bench.ticker = "^NSEI",
  type.return = "arit",
  freq.data = "weekly",
  how.to.aggregate = "last",
  do.complete.data = FALSE,
  do.fill.missing.prices = TRUE,
  do.cache = TRUE,
  cache.folder = file.path(tempdir(), "BGS_Cache"),
  do.parallel = TRUE, # FALSE
  be.quiet = FALSE
)

a02nfity50 <- data.table(a01nfity50$df.tickers)
setnames(a02nfity50, paste("NF", names(a02nfity50), sep = "_"))

# Merge the data with remaining data

a03all <- merge(x = a02,
                y = a02nfity50, 
                by.x = c("ref.date"),
                by.y = c("NF_ref.date"))

#####################################################
#
# Calculate Relative price strength
# https://corporatefinanceinstitute.com/resources/knowledge/trading-investing/relative-price-strength-rps/
#
# EMA: Daily, weekly and monthly 5, 21, and 55 periods
#
#####################################################

a03all <- a03all [, rs := price.adjusted / NF_price.adjusted, ]
a03all <- a03all [, `:=` (rs_sma55 = SMA(rs, 55), rs_ema55 = EMA(rs, 55),
                          rs_mean55 = runMean(rs, 55),
                          rs_sd55 = runSD(rs, 55),
                          nrow_tick = .N), by =.(ticker)]

a03all <- a03all [  nrow_tick >= 252 & volume > 0]

a03all <- a03all [, `:=` (prc_ema5d = EMA(price.adjusted, 5), prc_ema21d = EMA(price.adjusted, 21), prc_ema55d = EMA(price.adjusted, 55), prc_ema200d = EMA(price.adjusted, 200), 
                          prc_ema5w = EMA(price.adjusted, 5 * 4), prc_ema21w = EMA(price.adjusted, 21 * 4)
                          ),
                  by =.(ticker)]

a03all <- a03all [, jdk_rs55 := 100 + ((rs - rs_mean55)/rs_sd55) + 1, ]
a03all <- a03all [, jdk_roc55 := 100 * (shift(jdk_rs55, type = "lag", n=1) / jdk_rs55 - 1), by = .(ticker)]
a03all <- a03all [, `:=` (jdk_roc55_mean55 = runMean(jdk_roc55, 55),
                          jdk_roc55_sd55 = runSD(jdk_roc55, 55) ), by =.(ticker)]

a03all <- a03all [, jdk_momratio55 := 100 + ((jdk_roc55 - jdk_roc55_mean55)/jdk_roc55_sd55) + 1, ]

ami001 <- a03all # [, c("ticker", "ref.date", "price.open", "price.high", "price.low", "price.close", "volume", "jdk_rs55", "jdk_momratio55"), ]
ami001 <- ami001 [, ref.date := ymd(ref.date), ]
#fwrite(ami001, "D:\\My-Shares\\analysis\\0504_yahoo_amibroker_all_equity.csv")

#
# https://www.niftyindices.com/Methodology/Method_NIFTY_Equity_Indices.pdf
# Page 90 for the momentum index calculations
#
# Calculate these with true price and then calculate VWAP for 20 period
#

ami001 <- ami001 [, typical_price := (price.high + price.low + price.close)/3, ]

ami001 <- ami001 [, vwap20_typ_price := VWAP(typical_price, volume, n = 20), ]
ami001 <- ami001 [, only_close := price.close, ]
ami001 <- ami001 [, price.close := vwap20_typ_price, ] # replace the price by vwap for timebeing

ami002 <- ami001 [, perc_chg := ( price.close - shift(price.close, type = c("lag"), n = 1 ) / shift(price.close, type = c("lag"), n = 1 ) * 100),  ]
ami001 <- ami001 [, std_chg252 := runSD (perc_chg, 252), by = .(ticker)]
ami001 <- ami001 [, price_chg252 := (price.close / shift(price.close, type = c("lag"), n = 252 )) -1 , by = .(ticker)]
ami001 <- ami001 [, price_chg126 := (price.close / shift(price.close, type = c("lag"), n = 126 )) -1, by = .(ticker)]

ami001 <- ami001 [, momentum_ratio252 := price_chg252 / std_chg252, ]
ami001 <- ami001 [, momentum_ratio126 := price_chg126 / std_chg252, ]

# Calculate the mean and sd for each day across all the stocks
# This calculation should provide the yearly variations and means
ami003 <- ami001 [ !is.na(momentum_ratio252)]

ami003 <- ami003 [, overall_mean126 := mean(momentum_ratio126), by =.(ref.date) ]
ami003 <- ami003 [, overall_sd126 := sd(momentum_ratio126), by =.(ref.date) ]

ami003 <- ami003 [, overall_mean252 := mean(momentum_ratio252), by =.(ref.date) ]
ami003 <- ami003 [, overall_sd252 := sd(momentum_ratio252), by =.(ref.date) ]

# Standardized score for 6 and 12 months:
ami003 <- ami003 [, z252 := (momentum_ratio252 - overall_mean252) / overall_sd252, ]
ami003 <- ami003 [, z126 := (momentum_ratio126 - overall_mean126) / overall_sd126, ]

# Weighted average z score 50% wgt for both
ami003 <- ami003 [, z_wgt := 0.5 * z252 + 0.5 * z126, ]
ami003 <- ami003 [, z_wgt_std := ifelse(z_wgt >= 0, 1 + z_wgt, 1/(1 - z_wgt) ), ]

ami003 <- ami003 [ only_close > prc_ema200d ] # Actual stock closing price > 200 moving average
  
# Sort the data
ami003 <- ami003 [ order(ref.date, -z_wgt_std), ]
ami003 <- ami003 [, z_rank := 1:.N, by =.(ref.date)]

ami004 <- dcast(data = ami003 [ ref.date >= "2021-05-01"], # & ref.date >= "2020-09-01"],
                z_rank ~ ref.date,
                value.var = c("ticker"))

ami005 <- ami004 [z_rank <= 15]

