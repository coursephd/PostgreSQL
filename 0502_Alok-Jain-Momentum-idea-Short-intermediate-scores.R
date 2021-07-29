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
  first.date = Sys.Date() - 800,
  last.date = Sys.Date(),
  thresh.bad.data = 0.2,
  bench.ticker = "^NSEI",
  type.return = "arit",
  freq.data = "daily",
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
  first.date = Sys.Date() - 800,
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

# Calculate short term momentum based on 5 days, 10 days, 20 days, 30 days, 40 days
# Calculate intermediate term momentum based on 60 days (3 months), 120 days (6 months), 180 days (9 months), 240 days (12 months),
ami001 <- ami001 [, `:=`(std_chg5 = runSD (perc_chg, 5),
                         std_chg10 = runSD (perc_chg, 10),
                         std_chg20 = runSD (perc_chg, 20),
                         std_chg30 = runSD (perc_chg, 30),
                         std_chg40 = runSD (perc_chg, 40),
                         price_chg5 = (price.close / shift(price.close, type = c("lag"), n = 5 )) -1,
                         price_chg10 = (price.close / shift(price.close, type = c("lag"), n = 10 )) -1,
                         price_chg20 = (price.close / shift(price.close, type = c("lag"), n = 20 )) -1,
                         price_chg30 = (price.close / shift(price.close, type = c("lag"), n = 30 )) -1,
                         price_chg40 = (price.close / shift(price.close, type = c("lag"), n = 40 )) -1, 
                         
                         std_chg60 = runSD (perc_chg, 60),
                         std_chg120 = runSD (perc_chg, 120),
                         std_chg180 = runSD (perc_chg, 180),
                         std_chg240 = runSD (perc_chg, 240),
                         price_chg60 = (price.close / shift(price.close, type = c("lag"), n = 60 )) -1,
                         price_chg120 = (price.close / shift(price.close, type = c("lag"), n = 120 )) -1,
                         price_chg180 = (price.close / shift(price.close, type = c("lag"), n = 180 )) -1,
                         price_chg240 = (price.close / shift(price.close, type = c("lag"), n = 240 )) -1), by = .(ticker)]

ami001 <- ami001 [, `:=`(momentum_ratio5 = price_chg5 / std_chg5,
                         momentum_ratio10 = price_chg10 / std_chg10,
                         momentum_ratio20 = price_chg20 / std_chg20,
                         momentum_ratio30 = price_chg30 / std_chg30,
                         momentum_ratio40 = price_chg40 / std_chg40, 
                         
                         momentum_ratio60 = price_chg60 / std_chg60,
                         momentum_ratio120 = price_chg120 / std_chg120,
                         momentum_ratio180 = price_chg180 / std_chg180,
                         momentum_ratio240 = price_chg240 / std_chg240), ]

# Calculate the mean and sd for each day across all the stocks
# This calculation should provide the yearly variations and means
ami003 <- ami001 [ !is.na(momentum_ratio240)]

ami003 <- ami003 [, `:=`(overall_mean5 = mean(momentum_ratio5),
                         overall_mean10 = mean(momentum_ratio10),
                         overall_mean20 = mean(momentum_ratio20),
                         overall_mean30 = mean(momentum_ratio30),
                         overall_mean40 = mean(momentum_ratio40),
                         overall_mean60 = mean(momentum_ratio60),
                         overall_mean120 = mean(momentum_ratio120),
                         overall_mean180 = mean(momentum_ratio180),
                         overall_mean240 = mean(momentum_ratio240),
                         
                         overall_sd5 = sd(momentum_ratio5),
                         overall_sd10 = sd(momentum_ratio10),
                         overall_sd20 = sd(momentum_ratio20),
                         overall_sd30 = sd(momentum_ratio30),
                         overall_sd40 = sd(momentum_ratio40),
                         overall_sd60 = sd(momentum_ratio60),
                         overall_sd120 = sd(momentum_ratio120),
                         overall_sd180 = sd(momentum_ratio180),
                         overall_sd240 = sd(momentum_ratio240) ), by =.(ref.date) ]

# Standardized score for 6 and 12 months:
ami003 <- ami003 [, `:=` (z5 = (momentum_ratio5 - overall_mean5) / overall_sd5,
                          z10 = (momentum_ratio10 - overall_mean10) / overall_sd10,
                          z20 = (momentum_ratio20 - overall_mean20) / overall_sd20,
                          z30 = (momentum_ratio30 - overall_mean30) / overall_sd30,
                          z40 = (momentum_ratio40 - overall_mean40) / overall_sd40,
                          z60 = (momentum_ratio60 - overall_mean60) / overall_sd60,
                          z120 = (momentum_ratio120 - overall_mean120) / overall_sd120,
                          z180 = (momentum_ratio180 - overall_mean180) / overall_sd180,
                          z240 = (momentum_ratio240 - overall_mean240) / overall_sd240)]

# Weighted average z score 50% wgt for short term and long term
ami003 <- ami003 [, z_wgt_short := 0.2 * (z5 + z10 + z20 + z30 + z40), ]
ami003 <- ami003 [, z_wgt_inter := 0.25 * (z60 + z120 + z180 + z240), ]

ami003 <- ami003 [, z_wgt_std_short := ifelse(z_wgt_short >= 0, 1 + z_wgt_short, 1/(1 - z_wgt_short) ), ]
ami003 <- ami003 [, z_wgt_std_inter := ifelse(z_wgt_inter >= 0, 1 + z_wgt_inter, 1/(1 - z_wgt_inter) ), ]

# Sort the data
ami003 <- ami003 [ order(ref.date, -z_wgt_std_short, -z_wgt_std_inter), ]
ami003 <- ami003 [, z_rank := 1:.N, by =.(ref.date)]

ami004 <- dcast(data = ami003 [ ref.date >= "2021-05-01"], # & ref.date >= "2020-09-01"],
                z_rank ~ ref.date,
                value.var = c("ticker"))

ami005 <- ami004 [z_rank <= 15]