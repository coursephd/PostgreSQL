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
  tickers = c("^NSEBANK", "^NSEI"),
  first.date = "2008-04-07", #Sys.Date() - 5000,
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
allfut0002 <- allfut0002 [, `:=`(ema5 = EMA(CLOSEFUT, 5), 
                                 ema9 = EMA(CLOSEFUT, 9) ), by =.(SYMBOL)]

allfut0003bnf <- allfut0002 [ SYMBOL == "^NSEI"] # "^NSEI" ] # "^NSEBANK"] # "BANKNIFTY" ]
allfut0003bnf <- allfut0003bnf [, allrow := .I, ]

sar <- as.data.table( SAR(allfut0003bnf[, c("HIGHFUT", "LOWFUT") ], accel = c(0.1, 0.2) ) )
#sar <- as.data.table( SAR(allfut0003bnf[, c("HIGHFUT", "LOWFUT") ] ) )

sar <- sar [, allrow := .I, ]

allfut0003bnf <- Reduce(function(...) merge(..., by = c("allrow"), all=T),
                        list( allfut0003bnf, sar) )


allfut0003bnf <- allfut0003bnf [, downtrend := ifelse(sar >= HIGHFUT, -1, 0), ]
allfut0003bnf <- allfut0003bnf [, uptrend := ifelse(sar <= LOWFUT, 1, 0), ]
allfut0003bnf <- allfut0003bnf [, overalltrend := uptrend + downtrend, ]

allfut0003bnf <- allfut0003bnf [ order (SYMBOL, trdate)]

# Include one more condition:
allfut0003bnf <- allfut0003bnf [, prvhigh := shift(HIGHFUT, n = 1, type =c("lag") ), ] 
allfut0003bnf <- allfut0003bnf [, prvlow := shift(LOWFUT, n = 1, type =c("lag") ), ] 

allfut0003bnf <- allfut0003bnf [, chk_lh := ifelse( sar == prvlow | sar == prvhigh, 1, 0), ]
allfut0003bnf <- allfut0003bnf [, -c("ret.adjusted.prices", "ret.closing.prices" , "price.adjusted", "volume", "ema5", "ema9"), ]

allfut0003bnf <- allfut0003bnf [, prvtrnd := shift(overalltrend, n = 1, type =c("lag") ), ]
allfut0003bnf <- allfut0003bnf [, totrow := 1:.N, ]
allfut0003bnf <- allfut0003bnf [, chkvar := ifelse(prvtrnd != overalltrend, 1, 0), ]
allfut0003bnf <- allfut0003bnf [, chkvar := ifelse(totrow == 1, 1, chkvar), ]

# Create a group variable to understand the trending period
# Create an entry and exit only at the Open of each day
# This will mean that the reversal will only be understood the next day
# Let us see how this impacts the overall calculations

chk01 <- allfut0003bnf [ chkvar ==1]
chk01 <- chk01 [, `:=`(totrow02 = shift(totrow, n = 1, type =c("lead") ),
                       group = 1:.N), ]
chk01 <- chk01 [, totrow_n := ifelse(totrow > 1, totrow + 1, totrow), ]
chk01 <- chk01 [, totrow02_n := ifelse(totrow02 > totrow_n, totrow02, totrow02), ]

chk01 <- chk01 [, durn := totrow02 - totrow + 1, ]
chk01 <- chk01 [, durn_n := totrow02_n - totrow_n + 1, ]
chk01 <- chk01 [, c("totrow", "totrow02", "durn", "group", "totrow_n", "totrow02_n", "durn_n"), ]

chk01 <- na.omit(chk01)
chk02 <- chk01 [, list (newrow = totrow, totrow02 = totrow02, durn = durn, group = group,
                        totrow = seq( totrow, totrow02)), by = 1:nrow(chk01)]

allfut0004bnf <- merge (x = allfut0003bnf, 
                        y = chk02 [, c("group", "totrow", "durn"), ], 
                        by = c("totrow"),
                        all = TRUE)

allfut0004bnf <- allfut0004bnf [, trdrow := 1:.N, by = .(group)]

# Either sell or buy at the open on first row 

a_initiate01 <- allfut0004bnf [ trdrow == 1, c("SYMBOL", "trdate", "group", "OPENFUT", "overalltrend", "durn"), ] 
a_initiate01 <- a_initiate01 [, `:=`(start = OPENFUT, startdt = trdate), ]

a_end01 <- allfut0004bnf [ trdrow == durn, c("SYMBOL", "trdate", "group", "CLOSEFUT", "overalltrend", "durn"), ] 
a_end01 <- a_end01 [, `:=`(end = CLOSEFUT, enddt = trdate), ]

a_all <- merge(x = a_initiate01,
               y = a_end01 [, c("SYMBOL", "group", "enddt", "end"), ],
               by =c("SYMBOL", "group"))

a_all <- a_all [, pnl := ifelse(overalltrend == 1, end - start, start - end), ]
a_all <- a_all [, cumpnl := cumsum(pnl), ]
a_all <- a_all [, lotsize := ifelse(SYMBOL == "^NSEI", 75, 25), ]
#a_all <- a_all [, cumrs_4lots := cumpnl * 25 * 4, ]
#a_all <- a_all [, pnlrs_4lots := pnl * 25 * 4, ]

# Let us start a capital of 4 lac and see how it can be scaled up:
# Do 2 lots in 4 lacs, and see how to increase and decrease lots
# 4 lac: 2 lots, 
# For 2 lac increase in capital increase the lots by 1.
# For 2 lac decrease in capital decrease lots by 1.

a_all <- a_all [, `:=` (capital = 400000, nlots = 2), ]

write.xlsx (a_all, "D:\\My-Shares\\analysis\\0900_Nifty_BNF_pSAR_EMA_from2008_BNF_open.xlsx")
write.xlsx (a_all, "D:\\My-Shares\\analysis\\0900_Nifty_BNF_pSAR_EMA_from2008_NF_open.xlsx")


write.xlsx (a_all, "D:\\My-Shares\\analysis\\0900_Nifty_BNF_pSAR_EMA_from2008_BNF_close.xlsx")
write.xlsx (a_all, "D:\\My-Shares\\analysis\\0900_Nifty_BNF_pSAR_EMA_from2008_NF_close.xlsx")


write.xlsx (allfut0004bnf, "D:\\My-Shares\\analysis\\0900_Nifty_BNF_pSAR_EMA_from2008_BNF_open_NF_SAR.xlsx")

#a_all <- a_all [, cumcap := ]

# accumulate function
# https://stackoverflow.com/questions/64325001/row-wise-iteration-in-a-dataframe-where-each-row-depends-on-previous-row-calcula


####################################################
#
# End of program
#
####################################################
