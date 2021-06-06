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

cci_34m <- as.data.table( CCI(a04all[, c("price.high","price.low","price.close") ], n = 34 * 21) )
cci_34m <- cci_34m [, allrow := .I, ]
setnames(cci_34m, "V1", "cci_34m")

a05all <- Reduce(function(...) merge(..., by = c("allrow"), all=T),  
                 list( a04all, cci_8d, cci_8w, cci_8m, cci_34d, cci_34w, cci_34m) )

a05all <- a05all [, nrow := .I, by =.(ticker)]

# To avoid any incorrect calculations explained above, remove certain number of rows

a05all <- a05all [ nrow > 34 * 21]

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

subs001 <- a05all [ , c("ref.date", "ticker", "rule001", "rule002", 
                        "cci_34w", "cci_34d", "volume", 
                        "price.high", "price.open", "price.low", "price.close"), ]

subs001 <- a05all [ rule001 == 1 ]
subs002 <- a05all [ rule002 == 1 ]

subs003 <- rbind ( subs001 [, c("ticker", "entrydt", "exitdt", "rule001", "rule002", "open", "close", "low", "high", "cci34w", "cci34d", "prcema21d", "prcema55d"), ],
                   subs002 [, c("ticker", "entrydt", "exitdt", "rule001", "rule002", "open", "close", "low", "high", "cci34w", "cci34d", "prcema21d", "prcema55d"), ])

subs003 <- subs003 [ order(ticker, entrydt)]

# Check how many stocks appear on each day
subs001_chk <- subs001 [, .(n = uniqueN(ticker),
                            stocks = paste(ticker, collapse = ",", sep = "") ), by = .(ref.date)]



# Non-breakout candle must be followed by 2 breakout candles
#




# https://financetrain.com/quantstrat-example-in-r-ema-crossover-strategy/
#
# Check the 20, 50 and 200 day confluence
# Check ema20 > ema50 > ema200 and
# ema20 and ema200 should not be different by 1%, 3%, 5%
#

conf0001 <- a04all [, `:=` (ema20 = ema(price.close, 20), 
                            ema50 = ema(price.close, 50),
                            ema200 = ema(price.close, 200),
                            smavol20 = sma(volume, 20)), by =.(ticker)]

conf0001 <- conf0001 [ smavol20 >= 100000]

conf0001 <- conf0001 [, `:=` (crule001 = ifelse( (price.close > ema20 & ema20 > ema50), 1, 0),
                              crule002 = ifelse( (price.close > ema50 & ema50 > ema200), 1, 0),
                              crule003 = ifelse( (price.close > ema20 & ema20 < ema200 * 1.01), 1, 0),
                              crule004 = ifelse( (price.close > ema20 & ema20 < ema200 * 1.03), 1, 0),
                              crule005 = ifelse( (price.close > ema20 & ema20 < ema200 * 1.05), 1, 0) ),]

conf0002 <- conf0001 [, c("allrow", "ref.date", "ticker", "price.open", "price.high", "price.low", "price.close", "volume",
                          "qudrant", "jdk_momratio55", "jdk_rs55", 
                          "ema20", "ema50", "ema200", "crule001", "crule002", "crule003", "crule004", "crule005"), ]

# Check how may rows within 1%, 3%, 5%
conf0002_1perc <- conf0002 [ crule001 == 1 & crule002 == 1 & crule003 == 1]
conf0002_3perc <- conf0002 [ crule001 == 1 & crule002 == 1 & crule004 == 1]
conf0002_5perc <- conf0002 [ crule001 == 1 & crule002 == 1 & crule005 == 1]

# Check how many stocks appear on each day
conf0002_5perc02 <- conf0002_5perc [, .(n = uniqueN(ticker),
                                        stocks = paste(ticker, collapse = ",", sep = "") ), by = .(ref.date, qudrant)]

# Based on the following page, the heatmap of trades is generated
# https://towardsdatascience.com/time-series-calendar-heatmaps-9f576578fcfe

conf0002_5perc02 <- conf0002_5perc02 [, weekday := as.POSIXlt(ref.date)$wday, ]
conf0002_5perc02 <- conf0002_5perc02 [, weekdayf := factor(weekday,levels=rev(1:7),labels=rev(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")),ordered=TRUE) , ]
conf0002_5perc02 <- conf0002_5perc02 [, monthf := factor(month(ref.date),levels=as.character(1:12),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE) , ]
conf0002_5perc02 <- conf0002_5perc02 [, yearmonth := factor(as.yearmon(ref.date)), ]
conf0002_5perc02 <- conf0002_5perc02 [, week := as.numeric(format(ref.date,"%W")), ]
conf0002_5perc02 <- conf0002_5perc02 [, monthweek := 1+week-min(week), by =.(yearmonth) ]

p <- ggplot(conf0002_5perc02 [ qudrant ==1 ], aes(monthweek, weekdayf, fill = n)) + 
  geom_tile(colour = "white") + 
  facet_grid(year(ref.date) ~ monthf) + 
  scale_fill_gradient(low="red", high="green") + 
  xlab("Week of Month") + ylab("") + 
  ggtitle("Time-Series Calendar Heatmap: Trades generated by EMA cross-over") + 
  labs(fill = "Number of trades in quadrant 1") 
p

ggplotly(p)


#############################################################
#
# Create a strategy with 52 week high
# The stock should not have a high for at least 3 months
#
#############################################################

high001 <- na.omit(a04all [, c("allrow", "ref.date", "ticker", "price.open", "price.high", "price.low", "price.close", "volume",
                               "qudrant", "jdk_momratio55", "jdk_rs55"), ] )

#######################################################
#
# Errors appear but the variable gets created  
#
# runMax function from TTR is not working well
# So had to change to roll_max function from QuantTools
#
########################################################

high001 <- high001 [, high52 := roll_max(price.high, n = 252), by = .(ticker)]

high001 <- high001 [, highdt := case_when( price.high == high52 ~ as.character(ref.date),
                                           price.high != high52 ~ "") , by =.(ticker)]

high001 <- high001 [, highdt := ifelse(highdt == "", NA, highdt), ] 

high001 <- high001 [, highdt02 := na.locf(highdt, na.rm=FALSE), by = .(ticker)]
high001 <- high001 [, highdt02 := anydate(highdt02), ]
high001 <- high001 [, t52high := as.numeric(ref.date - highdt02 + 1),]

# Compute how far or how close the current price is to the 52 week high

high001 <- high001 [, d52high := round( (high52 - price.close) / high52 * 100, 1), ]


# Subset the stocks with 5% distance and at least 3 month distance

high002 <- high001 [ d52high <= 5 & t52high >= 60 ]

# Check how many stocks appear on each day
high003 <- high002 [, .(n = uniqueN(ticker),
                        stocks = paste(ticker, collapse = ",", sep = "") ), by = .(ref.date, qudrant)]




# Get a dashboard created
# Merge the data with industry data

a04all <- merge(x = a04all,
                y = cntrt02, 
                by.x = c("ticker"),
                by.y = c("SYMBOL02"),
                all.x = TRUE)
a04all <- a04all [, adv_dec := ifelse(ret.adjusted.prices >=0, "Advance", "Decline"), ]
a04all <- a04all [, above200ema := ifelse(price.close >= ema200, "Above_200ema", "Below_200ema"), ]

setkey(a04all, Industry, ref.date)
dash001 <- a04all [, totcom := uniqueN(ticker), by = .(Industry, ref.date)]

dash001 <- dash001 [, quadcom := uniqueN(ticker), by = .(Industry, ref.date, qudrant)]

setkey(dash001, Industry, ref.date, totcom, qudrant, quadcom, adv_dec)
dash002 <- dash001 [, .(nadv_dec = uniqueN(ticker) ), by =.(Industry, ref.date, totcom, qudrant, quadcom, adv_dec)]

dash003 <- dcast(data = dash002,
                 Industry + ref.date + totcom + qudrant + quadcom ~ adv_dec, 
                 value.var = c("nadv_dec"),
                 fill = " ")
dash003[is.na(dash003)] <- " "

fwrite(dash003, "D:\\My-Shares\\analysis\\0508_plotly_daily_overview.csv")



#
# Actual Turtle strategy related calculations
# Above max 55 day entry + above 200 ema
# Use the subset with SuperTrend 20, 2 as the SL: Assumption is 20 day ATR * 2
# 
#
a04all <- merge(x = a04all,
                y = cntrt02, 
                by.x = c("ticker"),
                by.y = c("SYMBOL02"),
                all.x = TRUE)
a04all <- a04all [, adv_dec := ifelse(ret.adjusted.prices >=0, "Advance", "Decline"), ]
a04all <- a04all [, above200ema := ifelse(price.close >= ema200, "Above_200ema", "Below_200ema"), ]

a04all <- a04all [, `:=` (max20 = roll_max(price.close, 20),
                          max55 = roll_max(price.close, 55), 
                          min10 = roll_max(price.close, 10), 
                          min20 = roll_max(price.close, 20)), by =.(ticker)]

a04all <- a04all [, above55max := ifelse(price.close >= max55, "Above_55", ""), ]
a04all <- a04all [, above20max := ifelse(price.close >= max20, "Above_20", ""), ]

above55 <- a04all [ above55max == "Above_55" & above200ema == "Above_200ema" & 
                      ( (price.close > ema20) & (ema20 > ema50) & (ema50 > ema200)) ]

# Check how many stocks appear on each day
above55_02 <- above55 [, .(n = uniqueN(ticker),
                           stocks = paste(ticker, collapse = ",", sep = "") ), by = .(ref.date, qudrant)]

# Based on the following page, the heatmap of trades is generated
# https://towardsdatascience.com/time-series-calendar-heatmaps-9f576578fcfe

above55_02 <- above55_02 [, weekday := as.POSIXlt(ref.date)$wday, ]
above55_02 <- above55_02 [, weekdayf := factor(weekday,levels=rev(1:7),labels=rev(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")),ordered=TRUE) , ]
above55_02 <- above55_02 [, monthf := factor(month(ref.date),levels=as.character(1:12),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE) , ]
above55_02 <- above55_02 [, yearmonth := factor(as.yearmon(ref.date)), ]
above55_02 <- above55_02 [, week := as.numeric(format(ref.date,"%W")), ]
above55_02 <- above55_02 [, monthweek := 1+week-min(week), by =.(yearmonth) ]

p <- ggplot(above55_02 [ qudrant ==1 ], aes(monthweek, weekdayf, fill = n)) + 
  geom_tile(colour = "white") + 
  facet_grid(year(ref.date) ~ monthf) + 
  scale_fill_gradient(low="red", high="green") + 
  xlab("Week of Month") + ylab("") + 
  ggtitle("Time-Series Calendar Heatmap: Trades generated by 55 day turtle") + 
  labs(fill = "Number of trades in quadrant 1") 
p

ggplotly(p)



#
# System 1 from Turtle trades
# 20 day high
#

above20 <- a04all [ above20max == "Above_20" & above200ema == "Above_200ema" & 
                      ( (price.close > ema20) & (ema20 > ema50) & (ema50 > ema200)) ]

# Check how many stocks appear on each day
above20_02 <- above20 [, .(n = uniqueN(ticker),
                           stocks = paste(ticker, collapse = ",", sep = "") ), by = .(ref.date, qudrant)]

# Based on the following page, the heatmap of trades is generated
# https://towardsdatascience.com/time-series-calendar-heatmaps-9f576578fcfe

above20_02 <- above20_02 [, weekday := as.POSIXlt(ref.date)$wday, ]
above20_02 <- above20_02 [, weekdayf := factor(weekday,levels=rev(1:7),labels=rev(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")),ordered=TRUE) , ]
above20_02 <- above20_02 [, monthf := factor(month(ref.date),levels=as.character(1:12),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE) , ]
above20_02 <- above20_02 [, yearmonth := factor(as.yearmon(ref.date)), ]
above20_02 <- above20_02 [, week := as.numeric(format(ref.date,"%W")), ]
above20_02 <- above20_02 [, monthweek := 1+week-min(week), by =.(yearmonth) ]

p <- ggplot(above20_02 [ qudrant ==1 ], aes(monthweek, weekdayf, fill = n)) + 
  geom_tile(colour = "white") + 
  facet_grid(year(ref.date) ~ monthf) + 
  scale_fill_gradient(low="red", high="green") + 
  xlab("Week of Month") + ylab("") + 
  ggtitle("Time-Series Calendar Heatmap: Trades generated by 20 day turtle") + 
  labs(fill = "Number of trades in quadrant 1") 
p

ggplotly(p)
