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

a01nfity50 <-  BatchGetSymbols(
  tickers = c("^NSEBANK", "^NSEI"), #c("INFY.NS"), #c("^GDAXI"), # c("^N225"), #c("^HSI"), # c("^DJI"), # c("^NSEBANK", "^NSEI"),
  first.date = "2008-01-01", #Sys.Date() - 5000,
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

allfut0001 <- data.table(a01nfity50$df.tickers)

setnames(allfut0001, "ticker", "SYMBOL")
setnames(allfut0001, "price.open", "OPENFUT")
setnames(allfut0001, "price.close", "CLOSEFUT")
setnames(allfut0001, "price.high", "HIGHFUT")
setnames(allfut0001, "price.low", "LOWFUT")
setnames(allfut0001, "ref.date", "trdate")

allfut0001 <- allfut0001 [ order(SYMBOL, trdate)]
allfut0001 <- allfut0001 [, nrow := 1:.N, by = .(SYMBOL, trdate)]
allfut0002 <- allfut0001 [ nrow == 1 ]
allfut0002 <- allfut0002 [, `:=`(rsi9_black_c = RSI(CLOSEFUT, 9),
                                 rsi9_black_o = RSI(OPENFUT, 9)), by =.(SYMBOL)]

allfut0002 <- allfut0002 [, `:=` (ema_rsi3_green_c = EMA(rsi9_black_c, n = 3),
                                  wma_rsi21_red_c = WMA(rsi9_black_c, n= 21),
                                  ema_rsi3_green_o = EMA(rsi9_black_o, n = 3),
                                  wma_rsi21_red_o = WMA(rsi9_black_o, n= 21) ), 
                by = .(SYMBOL)]

##############################################################################################################
#
# 4 conditions which have been created based on the manual observations:
#
#Above line of 50:
# === If both the green and / or red lines are inside the Pink box (i.e. above 50) then it is an uptrend.
#     ema_rsi3_green_c <= rsi9_black_c OR wma_rsi21_red_c <= rsi9_black_c AND green and red >= 50
#
# === If Green and / or red line is outside of the Pink box then it is a downtrend.
#     ema_rsi3_green_c > rsi9_black_c OR wma_rsi21_red_c > rsi9_black_c AND green and red >= 50
#
#Below line of 50:
# === If both the green and red lines are inside the water box (i.e. below 50) then it is downtrend.
#     ema_rsi3_green_c > rsi9_black_c OR wma_rsi21_red_c > rsi9_black_c AND green and red < 50
#
# === If Green and / or red line is outside of the water box then it is an uptrend OR at least the falling will completely stop.
#     ema_rsi3_green_c <= rsi9_black_c OR wma_rsi21_red_c <= rsi9_black_c AND green and red < 50
#
#
##############################################################################################################
allfut0002 <- allfut0002 [, `:=`(up001 = ifelse( (ema_rsi3_green_c <= rsi9_black_c | wma_rsi21_red_c <= rsi9_black_c) 
                                         & ema_rsi3_green_c >= 50 & wma_rsi21_red_c >= 50, 1, 0),
                                 up002 = ifelse( (ema_rsi3_green_c > rsi9_black_c | wma_rsi21_red_c > rsi9_black_c) 
                                         & ema_rsi3_green_c < 50 & wma_rsi21_red_c < 50, 1, 0),
                                 dwn001 = ifelse( (ema_rsi3_green_c > rsi9_black_c | wma_rsi21_red_c > rsi9_black_c) 
                                          & ema_rsi3_green_c >= 50 & wma_rsi21_red_c >= 50, 1, 0),
                                 dwn002 = ifelse( (ema_rsi3_green_c <= rsi9_black_c | wma_rsi21_red_c <= rsi9_black_c) 
                                          & ema_rsi3_green_c < 50 & wma_rsi21_red_c < 50, 1, 0) ),]
allfut0002 <- allfut0002 [, `:=`(all_up = up001 + up002, 
                                 all_dwn = dwn001 + dwn002), ]

allfut0002 <- allfut0002 [, red_lowest_c := ifelse(wma_rsi21_red_c < ema_rsi3_green_c & wma_rsi21_red_c < rsi9_black_c, 1, 0), ]
allfut0002 <- allfut0002 [, red_lowest_o := ifelse(wma_rsi21_red_o < ema_rsi3_green_o & wma_rsi21_red_o < rsi9_black_o, 1, 0), ]

#################################################################
#
# Create Heikin Ashi candles:
# https://towardsdatascience.com/how-to-calculate-heikin-ashi-candles-in-python-for-trading-cff7359febd7#:~:text=Heikin%20Ashi%20candles%20are%20calculated,Low%20%2B%20Close%20%2B%20High)%2F4&text=Low%3A%20the%20same%20of%20the%20actual%20candle
# 
#
# Open: (Open (previous candle) + Close (previous candle))/2
# Close: (Open + Low + Close + High)/4
# High: the same of the actual candle
# Low: the same of the actual candle
#
#################################################################

allfut0002 <- allfut0002 [, prvopen := shift(OPENFUT, type = c("lag"), n = 1), by = .(SYMBOL)]
allfut0002 <- allfut0002 [, prvcls := shift(CLOSEFUT, type = c("lag"), n = 1), by = .(SYMBOL)]
allfut0002 <- allfut0002 [, `:=`(h_open = (prvopen + prvcls)/2,
                                 h_close = (OPENFUT + CLOSEFUT + HIGHFUT + LOWFUT)/ 4 ), ]

allfut0002 <- allfut0002 [, `:=`(h_low = pmin(LOWFUT, h_open, h_close),
                                 h_high = pmax(HIGHFUT, h_open, h_close) ), ]


ggplot(data = allfut0002 [SYMBOL == "^NSEI" & trdate >= "2021-12-01"], aes(x = trdate, y = h_close)) +
  geom_candlestick(aes(open = h_open, high = h_high, low = h_low, close = h_close)) +
  labs(title = "NF candlestick Chart", y = "Closing Price", x = "") + 
  theme_tq()
