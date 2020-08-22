library(TTR)
library(data.table)
library(tidyverse)
library(anytime)
library(derivmkts)
library(quantmod)
library(simstudy)
library(zoo)
library(RCurl)
library(lubridate)
library(curl)
library(tidyquant) 
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
cntrt02 <- cntrt02 [, step004 := paste( SYMBOL02, " <- melt(", SYMBOL02, ", id.vars = c('index')) \n", sep = ""), ]


fwrite(cntrt02 [, c("step001", "step002", "step003", "step004"), ],
       "D:\\My-Shares\\prgm\\yahoooutput.R",
       quote = FALSE,
       sep = " ",
       col.names = FALSE,
       row.names = FALSE) 

source("D:\\My-Shares\\prgm\\yahoooutput.R")

all <- rbindlist(mget( ls(pattern="*NS*") ), fill = TRUE )
all <- all [, c("symbol", "exchange", "cat") := tstrsplit(variable, "\\."), ]
all <- all [, cat := tolower(cat),]
all <- all [, index := anydate(index),]

all0 <- unique( na.omit( all) )

all02 <- dcast(data = all0,
               symbol + index ~ cat,
               value.var = c("value"), 
               fill ="")

all02 <- all02 [ order(symbol, index)]
all02 <- all02 [, `:=` (sma50 = SMA(adjusted, 50),
                        rsi9_white = RSI(adjusted, 9),
                        vwap10 = VWAP(adjusted, volume, n= 10)), by = .(symbol)]
all02 <- all02 [, `:=` (ema_rsi3_green = EMA(rsi9_white, n = 3),
                        wma_rsi21_red = WMA(rsi9_white, n= 21) ), by = .(symbol)]

all02 <- all02 [, pivot := (high + low + close) / 3, ]
all02 <- all02 [, `:=`( R1 = (2 * pivot) - low, 
                     R2 = pivot + high - low, 
                     R3 = pivot + 2 * (high - low),
                     S1 = (2 * pivot) - high, 
                     S2 = pivot - (high - low), 
                     S3 = pivot - 2 * (high - pivot) ) , ]

rm ( list = ls (pattern = "*NS*"))

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
