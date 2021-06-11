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

future::plan(future::multisession, workers = floor(parallel::detectCores() ))

# https://www1.nseindia.com/content/equities/EQUITY_L.csv           : List of all companies
# https://www1.nseindia.com/content/indices/ind_nifty500list.csv    : List of Nifty500 companies

cntrt <- fread('https://www1.nseindia.com/content/equities/EQUITY_L.csv')
cntrt02 <- cntrt [, `:=` (nrow = .I, SYMBOL02 = paste(SYMBOL, ".NS", sep="") ), ]
setnames(cntrt02, "ISIN NUMBER", "ISIN")

a01 <-  BatchGetSymbols(
  tickers = cntrt02$SYMBOL02,
  first.date = Sys.Date() - 500,
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
  first.date = Sys.Date() - 500,
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

#  prc_ema5m = EMA(price.adjusted, 5 * 21 ), prc_ema21m = EMA(price.adjusted, 21 * 21), prc_ema55m = EMA(price.adjusted, 55 * 21)
a03all <- a03all [, `:=` (prc_ema5d = EMA(price.adjusted, 5), prc_ema21d = EMA(price.adjusted, 21), prc_ema55d = EMA(price.adjusted, 55), 
                          prc_ema5w = EMA(price.adjusted, 5 * 4), prc_ema21w = EMA(price.adjusted, 21 * 4), prc_ema55w = EMA(price.adjusted, 55 * 4) ), 
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
a04all <- a04all [ order(ticker, ref.date)]

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

#cci_34m <- as.data.table( CCI(a04all[, c("price.high","price.low","price.close") ], n = 34 * 21) )
#cci_34m <- cci_34m [, allrow := .I, ]
#setnames(cci_34m, "V1", "cci_34m")

#a05all <- Reduce(function(...) merge(..., by = c("allrow"), all=T),  
#                 list( a04all, cci_8d, cci_8w, cci_8m, cci_34d, cci_34w, cci_34m) )

a05all <- Reduce(function(...) merge(..., by = c("allrow"), all=T),  
                 list( a04all, cci_8d, cci_8w, cci_8m, cci_34d, cci_34w) )

a05all <- a05all [, nrow := .I, by =.(ticker)]

# To avoid any incorrect calculations explained above, remove certain number of rows

a05all <- a05all [ nrow > 170 ]

# Create an indicator CCI (170) > 100 and CCI (34) > 125

a05all <- a05all [, `:=`(rule001 = ifelse( cci_34w > 100 & cci_34d > 125 & (cci_34d > cci_34w) & volume > 100000, 1, 0), 
                         rule002 = ifelse( cci_34d < -100, 1, 0) ), by = .(ticker)]

# As the entry and exit are done on the next day get the next date

a05all <- a05all [, `:=` (entrydt = shift(ref.date, n = 1, type = c("lead") ),
                          exitdt = shift(ref.date, n = 1, type = c("lead") ),
                          open = shift(price.open, n = 1, type = c("lead") ),
                          close = shift(price.close, n = 1, type = c("lead") ), 
                          high = shift(price.high, n = 1, type = c("lead") ), 
                          low = shift(price.low, n = 1, type = c("lead") ), 
                          cci34w = shift(cci_34w, n = 1, type = c("lead") ),
                          cci34d = shift(cci_34d, n = 1, type = c("lead") ),
                          prcema21d = shift(prc_ema21d, n = 1, type = c("lead") ),
                          prcema55d = shift(prc_ema55d, n = 1, type = c("lead") ),
                          prcema55w = shift(prc_ema55w, n = 1, type = c("lead") ) ), by =.(ticker)]
# get those dates = 1

subs001 <- a05all [ , c("ref.date", "ticker", "rule001", "rule002", "qudrant",
                        "cci_34w", "cci_34d", "volume", 
                        "price.high", "price.open", "price.low", "price.close"), ]

subs001 <- a05all [ rule001 == 1 ]
subs002 <- a05all [ rule002 == 1 ]

subs003 <- rbind ( subs001 [, c("ticker", "entrydt", "exitdt", "rule001", "rule002", "open", "close", "low", "high", "cci34w", "cci34d", "prcema21d", "prcema55d", "qudrant"), ],
                   subs002 [, c("ticker", "entrydt", "exitdt", "rule001", "rule002", "open", "close", "low", "high", "cci34w", "cci34d", "prcema21d", "prcema55d", "qudrant"), ])

subs003 <- subs003 [ order(ticker, entrydt)]

# Check how many stocks appear on each day
subs001_chk <- subs001 [, .(n = uniqueN(ticker),
                            stocks = paste(ticker, collapse = ",", sep = "") ), by = .(ref.date, qudrant)]

# Based on the following page, the heatmap of trades is generated
# https://towardsdatascience.com/time-series-calendar-heatmaps-9f576578fcfe

subs001_chk <- subs001_chk [, weekday := as.POSIXlt(ref.date)$wday, ]
subs001_chk <- subs001_chk [, weekdayf := factor(weekday,levels=rev(1:7),labels=rev(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")),ordered=TRUE) , ]
subs001_chk <- subs001_chk [, monthf := factor(month(ref.date),levels=as.character(1:12),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE) , ]
subs001_chk <- subs001_chk [, yearmonth := factor(as.yearmon(ref.date)), ]
subs001_chk <- subs001_chk [, week := as.numeric(format(ref.date,"%W")), ]
subs001_chk <- subs001_chk [, monthweek := 1+week-min(week), by =.(yearmonth) ]

subs001_chk <- subs001_chk [, ntrademonth := sum(n), by = .(yearmonth)]

p <- ggplot(subs001_chk [ qudrant ==1 ], aes(monthweek, weekdayf, fill = n)) + 
  geom_tile(colour = "white") + 
  facet_grid(year(ref.date) ~ monthf) + 
  scale_fill_gradient(low="red", high="green") + 
  xlab("Week of Month") + ylab("") + 
  ggtitle("Time-Series Calendar Heatmap: Trades generated by CCI") + 
  labs(fill = "Number of trades in quadrant 1") 
p
