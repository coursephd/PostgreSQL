library(TTR)
library(tidyquant)
library(QuantTools)
library(derivmkts)
library(quantmod)

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

# from today to 2 years behind
tday <- Sys.Date()


# Contract size automation
cntrt <- fread("https://www1.nseindia.com/content/fo/fo_mktlots.csv")
cntrt <- cntrt [, `:=` (nrow = .I, SYMBOL02 = paste(SYMBOL, ".NS", sep="") ), ]
cntrt02 <- cntrt [ nrow > 4]

cntrt02 <- cntrt02 [ SYMBOL02 != "COFORGE.NS"]
cntrt02 <- cntrt02 [ SYMBOL02 != "BAJAJ-AUTO.NS"]
cntrt02 <- cntrt02 [ SYMBOL02 != "MCDOWELL-N.NS"]
cntrt02 <- cntrt02 [ SYMBOL02 != "M&MFIN.NS"]
cntrt02 <- cntrt02 [ SYMBOL02 != "M&M.NS"]
cntrt02 <- cntrt02 [ SYMBOL02 != "L&TFH.NS"]

cntrt02 <- cntrt02 [, step001 := paste("pbr <- getSymbols('", SYMBOL02, "', ", 'src = "yahoo", from =  anydate(tday) - 365, to =  anydate(tday), auto.assign = TRUE) \n', sep=""), ]
cntrt02 <- cntrt02 [, step002 := paste( SYMBOL02, " <- as.data.table(", SYMBOL02, ") \n", sep = ""), ]
cntrt02 <- cntrt02 [, step003 := paste( SYMBOL02, " <- ", SYMBOL02, "[, nrow := .I, ] \n", sep = ""), ]
cntrt02 <- cntrt02 [, step004 := paste( SYMBOL02, " <- melt(", SYMBOL02, ", id.vars = c('index', 'nrow')) \n", sep = ""), ]


fwrite(cntrt02 [, c("step001", "step002", "step003", "step004"), ],
       "D:\\My-Shares\\prgm\\yahoooutput.R",
       quote = FALSE,
       sep = " ",
       col.names = FALSE,
       row.names = FALSE) 

source("D:\\My-Shares\\prgm\\yahoooutput.R")

all <- rbindlist(mget( ls(pattern="*NS*") ), fill = TRUE )

rm ( list = ls (pattern = "*NS*"))

all <- all [, c("symbol", "exchange", "cat") := tstrsplit(variable, "\\."), ]
all <- all [, cat := tolower(cat),]
all <- all [, index := anydate(index),]

all0 <- unique( na.omit( all) )

all02 <- dcast(data = all0,
               symbol + index + nrow ~ cat,
               value.var = c("value"), 
               fill ="")

all02 <- all02 [ order(symbol, index)]
all02 <- all02 [, allrow := .I, ]

#
# Calculate SMA, RSI_White, Money Flow index, VWAP,
# ema_rsi, 3_green, wma_rsi21_red
# Bollinger Bands
#
# Create flags for consecutive high and low days
# Sign and Streak
# Signal
#
# Calculate the Bollinger band, the calculations are done over all the dataset
# So the first n lines of the bollinger band calculations go wrong
# as the data from previous company is carried forward
#
# Simliar calculations are done for the ADX parameter
# Remove the calculations for first 14 * 2 rows

bb_n <- 20
adx_n <- 14

bb_dn = as.data.table( bbands(all02$close, n = bb_n, k = 2) )
bb_dn <- bb_dn [, allrow := .I, ]

dmi.adx <- as.data.table( ADX(all02[, c("high","low","close") ], n = adx_n) )
dmi.adx <- dmi.adx [, allrow := .I, ]

sar <- as.data.table( SAR(all02[, c("high","low") ] ) )
sar <- sar [, allrow := .I, ]

atr <- as.data.table( ATR(all02[, c("high","low","close") ], n = adx_n) )
atr <- atr [, allrow := .I, ]


all02 <- all02 [, `:=` (ema20 = EMA(adjusted, 20), ema50 = EMA(adjusted, 50), ema200 = EMA(adjusted, 200),
                        sma50 = SMA(adjusted, 50),
                        rsi9_white = RSI(adjusted, 9),
                        mfi = MFI(close, volume, 9),
                        vwap10 = VWAP(adjusted, volume, n= 10),
                        dayperc = ( (close - open) / open) * 100, # Percentage change in a day
                        Sign_prc = ifelse(close>lag(close),"up", "down") ), by = .(symbol)]

# Find the maximum percentage change and rank them
# for each day
#
# Find the next day's price behaviour, would stocks in top 10
# not perform the next day?
# Could this provide a list of shares for short sell?

all02 <- all02 [ order(index, -dayperc, symbol )]
all02 <- all02 [, rank := 1:.N, by = .(index)]

#
# Find the top 10 stocks on each day
# Assumption is that the stock should fall after the rise
# 
# Give 5 extra days for the trade after short selling
# Create additional rows after the identification of the stock for trade
#
# There is one more calculation to be taken into consideration:
# Even if a stock is in top 10 for the day, the % increase on that should be
# taken into account -- calculate the trades with thresholds of 2%, 3%, 4%, etc.
#
# If a stock is in top 10 and has increased only by 0.5% then that may not be a good 
# pick for the next day, such stocks must be eliminated from the subset

top10_1 <- all02 [ rank <= 10, c("symbol", "index", "nrow", "rank", "dayperc"), ]
top10_1 <- top10_1 [ order(index, -dayperc, symbol) ]
top10_1 <- top10_1 [, grp := .GRP, by = .(index)]
top10_1 <- top10_1 [, `:=`(grprank = rank, row00 = nrow, maxchg = dayperc) ]
top10_2 <- top10_1 [, `:=` (day01 = nrow + 1, day02 = nrow + 2, day03 = nrow + 3, day04 = nrow + 4, day05 = nrow + 5), ]

# Transpose the data:
top10_3 <- melt(data = top10_2,
                id.vars = c("symbol", "grp", "grprank", "maxchg"), 
                measure.vars = c("row00", "day01", "day02", "day03", "day04", "day05"))
top10_3 <- top10_3 [, `:=`(nrow = value, trday = as.numeric( substr(variable, 4, 5)) ), ]

# Merge the data with the other data:
top20 <- merge(x = top10_3,
               y = all02, 
               by = c("symbol", "nrow"), 
               all.x = TRUE)

top20 <- na.omit(top20)

# Now find out the low and high value on day 1 of the trade:
# As long as there is lower value than the low value of day 1 then it satisfies the
# trade as a positive trade
#
# E.g. day 1= low value = 1200, if there is a value <= 1200 on next 5 days then the trade
# will be in profit.
#
# Let us create a dataset with possible entry values: 
# (1) Low of day 1, 
# (2) High of day 1
#
# Identify: the lower values after day 1 to see the success rate at high and low value:
#
# Additionally calculations can be done by using any value between the low and high value
# and compare that against the lower value

# Entry prices
top21 <- top20 [ trday == 1, c("symbol", "index", "grp", "grprank", "maxchg", "dayperc", "high", "low"), ]

# Exit prices using trday >= 2
# Transpose the data to get the quick min and max value
#
# Calculate the min and max exit for any time between 2 and 5 days
# Calculate the same for any individual day on or after day 2
# This will provide an idea about when can the trade get over

top22 <- melt(data = top20 [trday >=2], 
              id.vars =c("symbol", "grp", "trday"), 
              measure.vars = c("high", "low"))

top22_1 <- top22 [, .(minext = min(value), maxext = max(value)), by = .(symbol, grp)]
top22_1 <- top22_1 [, trday := -99, ]
top22_2 <- top22 [, .(minext = min(value), maxext = max(value)), by = .(symbol, grp, trday)]

top22_3 <- rbind(top22_1, top22_2)

# Merge these 2 datasets:

top23 <- merge (x = top21, 
                y = top22_3, 
                by = c("symbol", "grp"))

# Calculate success:
# (1) crit01: Low > minext
# (2) crit02: High > minext, 
# (3) crit03: High > maxext

top23 <- top23 [, `:=`(crit01 = ifelse(low > minext, 1, 0), 
                       crit02 = ifelse(high > minext, 1, 0),
                       crit03 = ifelse(high > maxext, 1, 0) ), ] 

# Create a category variable to identify the % change 
top23 <- top23 [, maxchgcat := case_when( maxchg <= 0 ~ "01 Less then 0",
                                          maxchg > 0 & maxchg <= 2 ~ "02 between 0% and 2%", 
                                          maxchg > 2 & maxchg <= 5 ~ "03 between 1% and 5%", 
                                          maxchg > 5 ~ "04 > 5%"), ]

top23_1 <- melt (data = top23, 
                 id.vars = c("symbol", "index", "grp", "grprank", "maxchg", "maxchgcat", "trday", "dayperc"), 
                 measure.vars = c("crit01", "crit02", "crit03"))

# Create a variable to get the N
top23_1 <- top23_1 [, value_tot := 1, ]

# Calculate a few frequencies:
top24 <- top23_1 [, .(total = sum(value_tot), success = sum( as.numeric(value)) ), by = .(trday, maxchgcat, variable)]


# Fibonnaci series retracement code
# https://gist.github.com/drewgriffith15/e34560476a022612aa60

all02 <- all02 [, `:=` (ema_rsi3_green = EMA(rsi9_white, n = 3),
                        wma_rsi21_red = WMA(rsi9_white, n= 21), 
                        Streak_prc = sequence(rle(Sign_prc)$lengths )  ), 
                by = .(symbol)]

all02 <- all02 [, Signal_prc := case_when(lag(Sign_prc)=="up" & lag(Streak_prc)%%4==0~'short',
                                     lag(Sign_prc)=="down" & lag(Streak_prc)%%4==0~'long',
                                     TRUE~""), by = .(symbol)]

all02 <- Reduce(function(...) merge(..., by = c("allrow"), all=T),  
                list( all02, bb_dn, dmi.adx, sar, atr) )

# To avoid any incorrect calculations explained above, remove certain number of rows

all02 <- all02 [ nrow > adx_n * 2]
                   
# Create VHF - Vertical Horizontal Filter (VHF)
all02 <- all02 [, vhf_rsi9 := VHF(rsi9_white, n = 9), by = .(symbol)]
all02 <- all02 [, vhf_price := VHF(adjusted, n = 9), by = .(symbol)]

all02 <- all02 [, pivot := (high + low + close) / 3, ]
all02 <- all02 [, `:=`( R1 = (2 * pivot) - low, 
                     R2 = pivot + high - low, 
                     R3 = pivot + 2 * (high - low),
                     S1 = (2 * pivot) - high, 
                     S2 = pivot - (high - low), 
                     S3 = pivot - 2 * (high - pivot) ) , ]

#
# Calculate the range for the volume to understand the 
# contraction and expansion of volume
# Calculate sma for the range
#
# Find the range of the price - if it is contracting then there is
# a chance of expansion
#  

all02 <- all02 [, `:=`(sma_vol = SMA(volume, 50), 
                       range_prc = as.numeric(high - close) ),]
all02 <- all02 [, sma20_range_prc := SMA(range_prc, 20), ]
all02 <- all02 [, sma10_range_prc := SMA(range_prc, 10), ]

# Calculate up and down signals for
# Price - already done
# Volume
# MFI
# RSI

all02 <- all02 [, Sign_vol := ifelse(volume > lag(volume),"up", "down"), ]
all02 <- all02 [, Streak_vol := sequence(rle(Sign_vol)$lengths ), ]
all02 <- all02 [, Signal_vol := case_when(lag(Sign_vol)=="up" & lag(Streak_vol)%%4==0~'short',
                                          lag(Sign_vol)=="down" & lag(Streak_vol)%%4==0~'long',
                                          TRUE~""), by = .(symbol)]

all02 <- all02 [, Sign_range_prc := ifelse(range_prc > lag(range_prc),"up", "down"), ]
all02 <- all02 [, Streak_range_prc := sequence(rle(Sign_range_prc)$lengths ), ]
all02 <- all02 [, Signal_range_prc := case_when(lag(Sign_range_prc)=="up" & lag(Streak_range_prc)%%4==0~'short',
                                          lag(Sign_range_prc)=="down" & lag(Streak_range_prc)%%4==0~'long',
                                          TRUE~""), by = .(symbol)]


all02 <- all02 [, Sign_mfi := ifelse(mfi > lag(mfi),"up", "down"), ]
all02 <- all02 [, Streak_mfi := sequence(rle(Sign_mfi)$lengths ), ]
all02 <- all02 [, Signal_mfi := case_when(lag(Sign_mfi)=="up" & lag(Streak_mfi)%%4==0~'short',
                                                lag(Sign_mfi)=="down" & lag(Streak_mfi)%%4==0~'long',
                                                TRUE~""), by = .(symbol)]


# Create a filtering condition to see if the companies should be picked up
# Pick up the last day
# May not be the best subset but will work with this set-up for the moment

all03 <- all02 [ nrow == max(nrow)]

# Pick up the stock when 
# (1) the last value > sma50
# (2) vwap > sma50
# (3) Red >= 50
# (4) Green >= 50
# (5) Last close value > pivot
# (6) Red >= Green by say 10 points?

watch01 <- all03 [ (adjusted >= sma50) & 
                   (vwap10 >= sma50) & 
                   (ema_rsi3_green >= 50) & 
                   (wma_rsi21_red >= 50) &
                   (ema_rsi3_green >= wma_rsi21_red )]
# (adjusted >= pivot) &


watch02 <- all03 [ (ema_rsi3_green - wma_rsi21_red >= 10)]


# Additional code
df<-df%>%mutate(Sign = ifelse(Close>lag(Close),"up", "down"))%>%
  mutate(Streak=sequence(rle(Sign)$lengths))

#########################################################################
# End of program
#########################################################################




# Combine all the names of the companies into one row
cntrt03 <- cntrt02 [, .(sym = paste("'", SYMBOL02, "'", collapse = ",", sep ="") ), ]

cntrt03lst <- split(cntrt03, list(cntrt03$sym) )



########################################################################################
# Section 2
#
# For some reason this is the syntax from this quntmod package:
# Get the NIFTY and NIFTY BANK data for last 1 year
#
# Calculate the summary statistics for both the indices and Correlation coefficient
# The mean and SD will be used to create simulated data (as needed)
#
########################################################################################

pbr <- getSymbols( cntrt03lst[[1]], src = "yahoo", from =  anydate(tday) - 365, to =  anydate(tday), auto.assign = TRUE)
rm(pbr)

NSEI02 <- na.omit( as.data.table (TCS.NS) )
NSEI02 <- NSEI02 [, symbol := "TCS"]

NSEI02 <- NSEI02 [, `:=` (sma50 = SMA(TCS.NS.Adjusted, 50),
                          rsi9 = RSI(TCS.NS.Adjusted, 9),
                          vwap10 = VWAP(TCS.NS.Adjusted, TCS.NS.Volume, n= 10)), ]
NSEI02 <- NSEI02 [, `:=` (ema_rsi3 = EMA(rsi9, n = 3),
                          wma_rsi21 = WMA(rsi9, n= 21), ]


pbr <- getSymbols("ASIANPAINT.NS", src = "yahoo", from =  anydate(tday) - 365, to =  anydate(tday), auto.assign = TRUE)
rm(pbr)

NSEI02 <- na.omit( as.data.table (ASIANPAINT.NS) )
NSEI02 <- NSEI02 [, symbol := "ASIANPAINT"]

NSEI02 <- NSEI02 [, `:=` (sma50 = SMA(ASIANPAINT.NS.Adjusted, 50),
                          rsi9 = RSI(ASIANPAINT.NS.Adjusted, 9),
                          vwap10 = VWAP(ASIANPAINT.NS.Adjusted, ASIANPAINT.NS.Volume, n= 10)), ]
NSEI02 <- NSEI02 [, `:=` (ema_rsi3_green = EMA(rsi9, n = 3),
                          wma_rsi21_red = WMA(rsi9, n= 21) ), ]

pbr <- getSymbols("DIVISLAB.NS", src = "yahoo", from =  anydate(tday) - 365, to =  anydate(tday), auto.assign = TRUE)
rm(pbr)

NSEI02 <- na.omit( as.data.table (DIVISLAB.NS) )
NSEI02 <- NSEI02 [, symbol := "DIVISLAB.NS"]

NSEI02 <- NSEI02 [, `:=` (sma50 = SMA(DIVISLAB.NS.Adjusted, 50),
                          rsi9 = RSI(DIVISLAB.NS.Adjusted, 9),
                          vwap10 = VWAP(DIVISLAB.NS.Adjusted, DIVISLAB.NS.Volume, n= 10)), ]
NSEI02 <- NSEI02 [, `:=` (ema_rsi3_green = EMA(rsi9, n = 3),
                          wma_rsi21_red = WMA(rsi9, n= 21) ), ]



pbr <- getSymbols("NTPC.NS", src = "yahoo", from =  anydate(tday) - 365, to =  anydate(tday), auto.assign = TRUE)
rm(pbr)

NSEI02 <- na.omit( as.data.table (NTPC.NS) )
NSEI02 <- NSEI02 [, symbol := "NTPC"]

NSEI02 <- NSEI02 [, `:=` (sma50 = SMA(NTPC.NS.Adjusted, 50),
                          rsi9 = RSI(NTPC.NS.Adjusted, 9),
                          vwap10 = VWAP(NTPC.NS.Adjusted, NTPC.NS.Volume, n= 10)), ]
NSEI02 <- NSEI02 [, `:=` (ema_rsi3_green = EMA(rsi9, n = 3),
                          wma_rsi21_red = WMA(rsi9, n= 21) ), ]


NSEI02 <- melt(NSEI02, id.vars = c("index", "symbol"))


pbr <- getSymbols("^NSEBANK", src = "yahoo", from =  anydate(tday) - 365, to =  anydate(tday), auto.assign = TRUE)


NSEI02 <- as.data.table (NSEI)
NSEI02 <- NSEI02 [, symbol := "NIFTY50"]
NSEI02 <- melt(NSEI02, id.vars = c("index", "symbol"))

NSEBANK02 <- as.data.table (NSEBANK)
NSEBANK02 <- NSEBANK02 [, symbol := "NIFTYBANK"]
NSEBANK02 <- melt(NSEBANK02, id.vars = c("index", "symbol"))



# Calculate the date of the expiry and last 4 days to the Thursday expiry

exp01 <- unique( na.omit( opt03 [, c("trday", "TIMESTAMP", "nexpday", "EXPIRY_DT"), ] ) )
exp01 <- exp01 [ order (nexpday, -trday) ]
exp01 <- exp01 [, nrow :=1:.N, by = .(nexpday)]

# For Bank Nifty, pick up the date 4 (Monday) or 5 (Previous Friday) before the expiry
bnkexp01 <- exp01 [ nrow <= 5]
bnkexp01 <- bnkexp01 [, sttrow := max(nrow), by = .(nexpday)]

# Merge this information for BANK NIFTY data and get the values

bnkexp02 <- merge (x = bnkexp01 ,
                   y = opt03 [SYMBOL == "BANKNIFTY"],
                   by = c("trday", "TIMESTAMP", "nexpday", "EXPIRY_DT"),
                   all.x = TRUE)

bnkexp02 <- bnkexp02 [, strk := ifelse(OPENFUT < 10000, signif(OPENFUT, 2), signif(OPENFUT, 3) ), ]

# Pick up the strike on day 4 or 5 and add 1400 or 1500, 1600, 1700
bnkexp03 <- unique( na.omit( bnkexp02 [ nrow %in% c(1, 2, 3, 4, 5), c("SYMBOL", "EXPIRY_DT", "strk", "trday",  "nrow", "mrgdt", "OPENFUT"), ]) )

# Each row is a possible trade block
bnkexp03 <- bnkexp03 [, cat01 := .I, ]
bnkexp03 <- bnkexp03 [, `:=`(strk900 = strk + 900, strk1000 = strk + 1000, strk1100 = strk + 1100, strk1200 = strk + 1200, 
                             strk1300 = strk + 1300, 
                             strk1400 = strk + 1400, 
                             strk1500 = strk + 1500, 
                             strk1600 = strk + 1600, 
                             strk1700 = strk + 1700), ]
bnkexp03_t <- melt(data = bnkexp03, 
                   id.vars = c("SYMBOL", "EXPIRY_DT", "strk", "trday",  "nrow", "mrgdt", "OPENFUT", "cat01")  )
bnkexp03_t <- bnkexp03_t [, STRIKE_PR := strk, ]


# Create the trade number to use afterwards
bnkexp03_t <- bnkexp03_t [, tradenum := 1:.N, by = .(EXPIRY_DT)]
bnkexp03_t <- bnkexp03_t [, cat02 := .GRP, by = .(EXPIRY_DT, variable)]

bnkexp04 <- merge(x = bnkexp03_t [, -c("strk", "mrgdt", "nrow"),],
                  y = bnkexp02 [, -c("strk"),],
                  by = c("SYMBOL", "EXPIRY_DT", "STRIKE_PR", "trday", "OPENFUT") )

# Only keep the records whre strk and STRIKE_PR match
# value == STRIKE_PR ==> incorrect subset in the earlier code

bnkexp05 <- bnkexp04 [ toupper(mrgdt) == toupper( substr(TIMESTAMP, 4, length(TIMESTAMP) ) ) ]
bnkexp05 <- bnkexp05 [, sttdt := min(trday), by =.(EXPIRY_DT, tradenum)]

# Get the entry price for each trade
# Create combinations for entry and exit
bnkexp06 <- bnkexp05 [ sttdt == trday]

setnames (bnkexp06, "OPEN", "entryOPEN")
setnames (bnkexp06, "CLOSE", "entryCLOSE")
setnames (bnkexp06, "LOW", "entryLOW")
setnames (bnkexp06, "HIGH", "entryHIGH")
setnames (bnkexp06, "CONTRACTS", "entryCONTRACTS")

bnkexp07 <- merge(x = bnkexp06 [, c("SYMBOL", "STRIKE_PR", "EXPIRY_DT",  "tradenum", "entryOPEN", "entryCLOSE", "entryLOW", "entryHIGH", "entryCONTRACTS")],
                  y = bnkexp05,
                  by = c("SYMBOL", "STRIKE_PR", "tradenum", "EXPIRY_DT"))

# Subset bank nifty for selling value >=90 and contracts > 0
bnkexp07 <- bnkexp07 [, `:=` (minval = pmin(entryOPEN, entryCLOSE, entryLOW, entryHIGH), 
                              maxval = pmax(entryOPEN, entryCLOSE, entryLOW, entryHIGH) ), ]

bnkexp08 <- bnkexp07# [ minval >= 90 & entryCONTRACTS > 0] 

bnkexp08 <- bnkexp08 [, alltrades := .GRP, by =.(STRIKE_PR, tradenum, EXPIRY_DT)]

#setnames (bnkexp08,  "entryOPEN"     ,'b_entry_o')
#setnames (bnkexp08,  "entryCLOSE"    ,'b_entry_c')
#setnames (bnkexp08,  "entryLOW"      ,'b_entry_l')
#setnames (bnkexp08,  "entryHIGH"     ,'b_entry_h')
#setnames (bnkexp08,  "entryCONTRACTS",'b_entry_contr')

#
# Adjust the strikes upwards if the price moves against
# Finally merge the original strike and updated strike
# This data should provide a view on the movement of prices
# and how are the premiums of originally sold options behave
# 

bnkexp030 <- bnkexp03 [, `:=`(strk900 = strk, strk1000 = strk, strk1100 = strk, strk1200 = strk, 
                             strk1300 = strk, 
                             strk1400 = strk, 
                             strk1500 = strk, 
                             strk1600 = strk, 
                             strk1700 = strk), ]
bnkexp030_t <- melt(data = bnkexp030, 
                   id.vars = c("SYMBOL", "EXPIRY_DT", "strk", "trday",  "nrow", "mrgdt", "OPENFUT", "cat01")  )
bnkexp030_t <- bnkexp030_t [, STRIKE_PR := strk, ]

# Create the trade number to use afterwards
bnkexp030_t <- bnkexp030_t [, tradenum := 1:.N, by = .(EXPIRY_DT)]
bnkexp030_t <- bnkexp030_t [, cat02 := .GRP, by = .(EXPIRY_DT, variable)]

bnkexp040 <- merge(x = bnkexp030_t [, -c("strk", "mrgdt", "nrow"),],
                  y = bnkexp02 [, -c("strk"),],
                  by = c("SYMBOL", "EXPIRY_DT", "STRIKE_PR", "trday", "OPENFUT") )

bnkexp041 <- bnkexp040 [, c("SYMBOL", "EXPIRY_DT", "STRIKE_PR", "trday", "mrgdt",
                            "cat02", "cat01", "tradenum", "variable", "value",
                            "HIGH", "LOW", "CLOSE", "OPEN", "CONTRACTS"), ]

setnames (bnkexp041,  "value"         ,'strk_adj')
setnames (bnkexp041,  "OPEN"          ,'b_adj_o')
setnames (bnkexp041,  "CLOSE"         ,'b_adj_c')
setnames (bnkexp041,  "HIGH"          ,'b_adj_h')
setnames (bnkexp041,  "LOW"           ,'b_adj_l')
setnames (bnkexp041,  "CONTRACTS"     ,'b_adj_contr')

bnkexp09 <- merge(x = bnkexp08,
                  y = bnkexp041,
                  by = c("SYMBOL", "STRIKE_PR", "tradenum", "EXPIRY_DT", "trday", 
                         "cat01", "variable", "cat02", "mrgdt") )

setnames (bnkexp09,  "STRIKE_PR"     ,'b_strk')
setnames (bnkexp09,  "tradenum"      ,'b_tradenum')
setnames (bnkexp09,  "EXPIRY_DT"     ,'b_exp_dt')
setnames (bnkexp09,  "OPEN"          ,'b_exit_o')
setnames (bnkexp09,  "CLOSE"         ,'b_exit_c')
setnames (bnkexp09,  "HIGH"          ,'b_exit_h')
setnames (bnkexp09,  "LOW"           ,'b_exit_l')
setnames (bnkexp09,  "CONTRACTS"     ,'b_exit_contr')
setnames (bnkexp09,  "SYMBOL"        ,'b_SYMBOL')

bnkexp10 <- bnkexp09[, c("b_SYMBOL", "variable", "value", "trday", "mrgdt",  "alltrades", "cat01", "cat02",
                         "b_strk", "b_tradenum", "b_exp_dt", "b_entry_o", "b_entry_c", "b_entry_l", "b_entry_h", "b_entry_contr",
                         "b_exit_o", "b_exit_c", "b_exit_l", "b_exit_h", "b_exit_contr", "nrow",
                         "OPENFUT", "CLOSEFUT", "HIGHFUT", "LOWFUT",
                         "strk_adj", "b_adj_o", "b_adj_c", "b_adj_l", "b_adj_h", "b_adj_contr"),]

fwrite(bnkexp10, "D:/My-Shares/analysis/105_bnk.csv")
saveRDS (bnkexp10, "D:/My-Shares/analysis/105_bnk.rds")
