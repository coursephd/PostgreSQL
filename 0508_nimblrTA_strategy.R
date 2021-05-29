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

options(scipen = 999)

library(BatchGetSymbols)

future.seed = TRUE
options(future.rng.onMisuse="ignore")

future::plan(future::multisession, workers = floor(parallel::detectCores()/2 ))

cntrt <- fread('https://www1.nseindia.com/content/indices/ind_nifty500list.csv')
cntrt02 <- cntrt [, `:=` (nrow = .I, SYMBOL02 = paste(Symbol, ".NS", sep="") ), ]
setnames(cntrt02, "ISIN Code", "ISIN")

a01 <-  BatchGetSymbols(
  tickers = cntrt02$SYMBOL02,
  first.date = Sys.Date() - 3600,
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
  first.date = Sys.Date() - 3600,
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
                          rs_sd55 = runSD(rs, 55) ), by =.(ticker)]

a03all <- a03all [, `:=` (prc_ema5d = EMA(price.adjusted, 5), prc_ema21d = EMA(price.adjusted, 21), prc_ema55d = EMA(price.adjusted, 55), 
                          prc_ema5w = EMA(price.adjusted, 5 * 4), prc_ema21w = EMA(price.adjusted, 21 * 4), prc_ema55w = EMA(price.adjusted, 55 * 4),
                          prc_ema5m = EMA(price.adjusted, 5 * 21 ), prc_ema21m = EMA(price.adjusted, 21 * 21), prc_ema55m = EMA(price.adjusted, 55 * 21) ), 
                  by =.(ticker)]

a03all <- a03all [, jdk_rs55 := 100 + ((rs - rs_mean55)/rs_sd55) + 1, ]
a03all <- a03all [, jdk_roc55 := 100 * (shift(jdk_rs55, type = "lag", n=1) / jdk_rs55 - 1), by = .(ticker)]
a03all <- a03all [, `:=` (jdk_roc55_mean55 = runMean(jdk_roc55, 55),
                          jdk_roc55_sd55 = runSD(jdk_roc55, 55) ), by =.(ticker)]

a03all <- a03all [, jdk_momratio55 := 100 + ((jdk_roc55 - jdk_roc55_mean55)/jdk_roc55_sd55) + 1, ]

a04all <- na.omit(a03all)
a04all <- a04all [, qudrant := case_when(jdk_rs55 > 100 & jdk_momratio55 > 100 ~ 1,
                                         jdk_rs55 > 100 & jdk_momratio55 < 100 ~ 2,
                                         jdk_rs55 < 100 & jdk_momratio55 > 100 ~ 3,
                                         jdk_rs55 < 100 & jdk_momratio55 < 100 ~ 4 ), ]

#################################################################
#
# Calculate NibmlrTA
# http://nimblr.pbworks.com/w/page/124439211/NimblrTA
#
# http://nimblr.pbworks.com/w/page/125232650/Strength_Candles
# Candle height [CH]
# Body height [BH]
#
# Calculate BH/CH %: if the % is >=50% then breakout candle
#                    if the % is < 50% then Non-breakout  
# If there is a T form: very open = High, close = Low with < 5% BH (my interpretation)
# then Breakout based on Nimblr comment
#
# https://docs.google.com/document/d/15eBbWo-NhzoIGCjz29QEden_GJ9s0XXaWd7RZdNrpIg/edit
#
#################################################################
a04all <- a04all [, `:=`(ch = price.high - price.low,
                         bh = abs(price.open - price.close),
                         allrow = .I ), ]

a04all <- a04all [, bh_ch_per := round( (bh/ch) * 100, 1), ]
a04all <- a04all [, t_candle := case_when(round(price.low, 1) == round(price.close, 1) & bh_ch_per < 5 ~ "T01", 
                                          round(price.high, 1) == round(price.open, 1) & bh_ch_per < 5 ~ "T01", 
                                          TRUE ~ ""), ]
a04all <- a04all [, bh_ch_cat := case_when (bh_ch_per >=50 ~ "Breakout", 
                                            bh_ch_per < 5 & t_candle == "T01" ~ "Breakout", 
                                            TRUE ~ "Non-breakout"), ]

#############################################################
#
# Calculate CCI on 
# http://nimblr.pbworks.com/w/page/127448330/NimblrTA_Score
# CCI: Daily, weekly and monthly 8 and 34 periods
# 
#############################################################

cci_8d <- as.data.table( CCI(a04all[, c("price.high","price.low","price.close") ], n = 8) )
cci_8d <- cci_8d [, allrow := .I, ]
setnames(cci_8d, "V1", "cci_8d")

cci_8w <- as.data.table( CCI(a04all[, c("price.high","price.low","price.close") ], n = 8 * 5) )
cci_8w <- cci_8w [, allrow := .I, ]
setnames(cci_8w, "V1", "cci_8w")

cci_8m <- as.data.table( CCI(a04all[, c("price.high","price.low","price.close") ], n = 8 * 21) )
cci_8m <- cci_8m [, allrow := .I, ]
setnames(cci_8m, "V1", "cci_8m")

cci_34d <- as.data.table( CCI(a04all[, c("price.high","price.low","price.close") ], n = 34) )
cci_34d <- cci_34d [, allrow := .I, ]
setnames(cci_34d, "V1", "cci_34d")

cci_34w <- as.data.table( CCI(a04all[, c("price.high","price.low","price.close") ], n = 34 * 5) )
cci_34w <- cci_34w [, allrow := .I, ]
setnames(cci_34w, "V1", "cci_34w")

cci_34m <- as.data.table( CCI(a04all[, c("price.high","price.low","price.close") ], n = 34 * 21) )
cci_34m <- cci_34m [, allrow := .I, ]
setnames(cci_34m, "V1", "cci_34m")





a04all <- a04all [, `:=`(), by = .(ticker)]


# Non-breakout candle must be followed by 2 breakout candles
#

