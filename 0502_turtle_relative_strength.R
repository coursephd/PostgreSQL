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

library(BatchGetSymbols)

future.seed = TRUE
options(future.rng.onMisuse="ignore")

future::plan(future::multisession, workers = floor(parallel::detectCores()/2 ))

cntrt <- fread('https://www1.nseindia.com/content/indices/ind_nifty500list.csv')
cntrt02 <- cntrt [, `:=` (nrow = .I, SYMBOL02 = paste(Symbol, ".NS", sep="") ), ]
setnames(cntrt02, "ISIN Code", "ISIN")

a01 <-  BatchGetSymbols(
  tickers = cntrt02$SYMBOL02,
  first.date = Sys.Date() - 700,
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

a01con <- data.table(a01$df.control)
a02 <- data.table(a01$df.tickers)

a01w <-  BatchGetSymbols(
  tickers = cntrt02$SYMBOL02,
  first.date = Sys.Date() - 700,
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

a01w_con <- data.table(a01w$df.control)
a02w <- data.table(a01w$df.tickers)

cntrt02 <- cntrt02 [, grp := round(nrow/ 100), ]
name01 <- cntrt02 [, quote := paste(SYMBOL02, collapse = ";", sep= " "), by = .(grp)]
name01 <- unique ( cntrt02 [, c("quote", "grp"), ])

require("quantmod")
a03quote <- as.data.table ( getQuote("TCS.NS;INFY.NS", what = yahooQF(c("Market Capitalization", "Earnings/Share", 
                                                                        "P/E Ratio", "Book Value", "EBITDA", "52-week Range"))) )


a03quote <- getQuote("", what = yahooQF(c("Market Capitalization", "Earnings/Share", "Shares Outstanding",
                                          "P/E Ratio", "Book Value", "EBITDA", "52-week Range"))) 

a03quote <- getQuote(eval(name01[grp == 6]$quote), what = yahooQF(c("Market Capitalization", "Earnings/Share", "Shares Outstanding",
                                                                    "P/E Ratio", "Book Value", "EBITDA", "52-week Range"))) 

setDT(a03quote, keep.rownames = TRUE)[]


# from today to 2 years behind
tday <- Sys.Date()

cntrt <- fread('https://www1.nseindia.com/content/indices/ind_nifty500list.csv')
cntrt02 <- cntrt [, `:=` (nrow = .I, SYMBOL02 = paste(Symbol, ".NS", sep="") ), ]
setnames(cntrt02, "ISIN Code", "ISIN")

cntrt02 <- cntrt02 [nrow > 1] # 3MIndia company name deleted as data.table should not start with a number
cntrt02 <- cntrt02 [ SYMBOL02 != "COFORGE.NS"]
cntrt02 <- cntrt02 [ SYMBOL02 != "BAJAJ-AUTO.NS"]
cntrt02 <- cntrt02 [ SYMBOL02 != "MCDOWELL-N.NS"]
cntrt02 <- cntrt02 [ SYMBOL02 != "M&MFIN.NS"]
cntrt02 <- cntrt02 [ SYMBOL02 != "M&M.NS"]
cntrt02 <- cntrt02 [ SYMBOL02 != "L&TFH.NS"]
cntrt02 <- cntrt02 [ SYMBOL02 != "NAM-INDIA.NS"]
  
cntrt02 <- cntrt02 [, step001 := paste("pbr <- getSymbols('", SYMBOL02, "', ", 'src = "yahoo", from =  anydate(tday) - 365, to =  anydate(tday), auto.assign = TRUE); print(', nrow, ');', sep=""), ]
cntrt02 <- cntrt02 [, step002 := paste("d", ISIN, " <- as.data.table(", SYMBOL02, "); print(", nrow, ');', sep = ""), ]
cntrt02 <- cntrt02 [, step003 := paste("d", ISIN, " <- d", ISIN, "[, nrow := .I, ]; print(", nrow, ');', sep = ""), ]
cntrt02 <- cntrt02 [, step004 := paste("d", ISIN, " <- melt(d", ISIN, ", id.vars = c('index', 'nrow')); print(", nrow, ');', sep = ""), ]

eval(parse(text = cntrt02$step001))
eval(parse(text = cntrt02$step002))
eval(parse(text = cntrt02$step003))
eval(parse(text = cntrt02$step004))

all <- rbindlist(mget( ls(pattern="^d") ), fill = TRUE )

rm ( list = ls (pattern = "NS*$"))

all <- all [, c("symbol", "exchange", "cat") := tstrsplit(variable, "\\."), ]
all <- all [, cat := tolower(cat),]
all <- all [, index := anydate(index),]


pbr <- getSymbols('^NSEI', src = "yahoo", from =  anydate(tday) - 365, to =  anydate(tday), auto.assign = TRUE) 

all0 <- unique( na.omit( all) )

all02 <- dcast(data = all0,
               symbol + index + nrow ~ cat,
               value.var = c("value"), 
               fill ="")

all02 <- all02 [ order(symbol, index)]
all02 <- all02 [, allrow := .I, ]

adx_n <- 14
atr <- as.data.table( ATR(all02[, c("high","low","close") ], n = adx_n) )
atr <- atr [, allrow := .I, ]

all02 <- all02 [, `:=` (ema5 = EMA(adjusted, 5), ema13 = EMA(adjusted, 13), ema21 = EMA(adjusted, 21), 
                        ema20 = EMA(adjusted, 20), ema50 = EMA(adjusted, 50), ema200 = EMA(adjusted, 200),
                        sma50 = SMA(adjusted, 50),
                        rsi9_white = RSI(adjusted, 9),
                        mfi = MFI(close, volume, 9),
                        vwap10 = VWAP(adjusted, volume, n= 10),
                        dayperc = ( (close - open) / open) * 100, # Percentage change in a day
                        Sign_prc = ifelse(close>lag(close),"up", "down") ), by = .(symbol)]


# Max of last 20 days
# Max of last 55 days
# Min of last 10 days
# Min of last 20 days

all02 <- all02 [, `:=` (max20 = runMax(adjusted, 20),
                        max55 = runMax(adjusted, 55), 
                        min10 = runMin(adjusted, 10), 
                        min20 = runMin(adjusted, 20)), by =.(symbol)]


chk <- all02 [, c("symbol", "nrow", "open", "low", "high", "close", "max20", "max55", "min10", "min20"), ]
