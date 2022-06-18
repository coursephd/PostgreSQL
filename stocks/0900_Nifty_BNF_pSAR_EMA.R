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


dt_chunk2017 <- data.table( readRDS("D:/My-Shares/source-fno-csv/source/dt_chunk2017.rds") )
dt_chunk2018 <- data.table( readRDS("D:/My-Shares/source-fno-csv/source/dt_chunk2018.rds") )
dt_chunk2019 <- data.table( readRDS("D:/My-Shares/source-fno-csv/source/dt_chunk2019.rds") )
dt_chunk2020 <- data.table( readRDS("D:/My-Shares/source-fno-csv/source/dt_chunk2020.rds") )
dt_chunk2021 <- data.table( readRDS("D:/My-Shares/source-fno-csv/source/dt_chunk2021.rds") )

# Get the Futures data

allfut0001 <- rbind (dt_chunk2017 [ OPTION_TYP == "XX"], 
                     dt_chunk2018 [ OPTION_TYP == "XX"],
                     dt_chunk2019 [ OPTION_TYP == "XX"],
                     dt_chunk2020 [ OPTION_TYP == "XX"],
                     dt_chunk2021 [ OPTION_TYP == "XX"])
allfut0001 <- allfut0001 [, c("INSTRUMENT", "SYMBOL", "OPEN", "HIGH", "LOW", "CLOSE", "expdate", "trdate"), ]

setnames(allfut0001, "INSTRUMENT", "FUT")
setnames(allfut0001, "OPEN", "OPENFUT")
setnames(allfut0001, "CLOSE", "CLOSEFUT")
setnames(allfut0001, "HIGH", "HIGHFUT")
setnames(allfut0001, "LOW", "LOWFUT")

allfut0001 <- allfut0001 [, mrgdt_exp := paste(toupper(format(expdate,"%Y")), "-", toupper(format(expdate,"%b") ), sep="") ,]
allfut0001 <- allfut0001 [ order(SYMBOL, trdate, expdate)]
allfut0001 <- allfut0001 [, nrow := 1:.N, by = .(SYMBOL, trdate)]
allfut0002 <- allfut0001 [ nrow == 1 ]
allfut0002 <- allfut0002 [, `:=`(ema5 = EMA(CLOSEFUT, 5), 
                                 ema9 = EMA(CLOSEFUT, 9) ), by =.(SYMBOL)]

allfut0003bnf <- allfut0002 [ SYMBOL == "BANKNIFTY" ]
allfut0003bnf <- allfut0003bnf [, allrow := .I, ]

sar <- as.data.table( SAR(allfut0003bnf[, c("HIGHFUT", "LOWFUT") ], accel = c(0.1, 0.2) ) )
sar <- sar [, allrow := .I, ]

allfut0003bnf <- Reduce(function(...) merge(..., by = c("allrow"), all=T),
                list( allfut0003bnf, sar) )


allfut0003bnf <- allfut0003bnf [, downtrend := ifelse(sar >= HIGHFUT, -1, 0), ]
allfut0003bnf <- allfut0003bnf [, uptrend := ifelse(sar <= LOWFUT, 1, 0), ]
allfut0003bnf <- allfut0003bnf [, overalltrend := uptrend + downtrend, ]

allfut0003bnf <- allfut0003bnf [ order (SYMBOL, trdate)]
allfut0003bnf <- allfut0003bnf [, prvtrnd := shift(overalltrend, n = 1, type =c("lag") ), ]
allfut0003bnf <- allfut0003bnf [, totrow := 1:.N, ]
allfut0003bnf <- allfut0003bnf [, chkvar := ifelse(prvtrnd != overalltrend, 1, 0), ]
allfut0003bnf <- allfut0003bnf [, chkvar := ifelse(totrow == 1, 1, chkvar), ]

# Create a group variable to understand the trending period

chk01 <- allfut0003bnf [ chkvar ==1]
chk01 <- chk01 [, `:=`(totrow02 = shift(totrow, n = 1, type =c("lead") ) -1,
                       group = 1:.N), ]
chk01 <- chk01 [, durn := totrow02 - totrow + 1, ]
chk01 <- chk01 [, c("totrow", "totrow02", "durn", "group"), ]

chk01 <- na.omit(chk01)
chk02 <- chk01 [, list (newrow = totrow, totrow02 = totrow02, durn = durn, group = group,
                        totrow = seq( totrow, totrow02)), by = 1:nrow(chk01)]

allfut0004bnf <- merge (x = allfut0003bnf, 
                        y = chk02 [, c("group", "totrow", "durn"), ], 
                        by = c("totrow"),
                        all = TRUE)

allfut0004bnf <- allfut0004bnf [, trdrow := 1:.N, by = .(group)]

# Either sell or buy at the open on first row 
# Either sell or buy at the end of the group

a_initiate01 <- allfut0004bnf [ trdrow == 1, c("SYMBOL", "trdate", "group", "OPENFUT", "overalltrend", "durn"), ] 
a_initiate01 <- a_initiate01 [, `:=`(start = OPENFUT, startdt = trdate), ]

a_end01 <- allfut0004bnf [ trdrow == durn, c("SYMBOL", "trdate", "group", "CLOSEFUT", "overalltrend", "durn"), ] 
a_end01 <- a_end01 [, `:=`(end = CLOSEFUT, enddt = trdate), ]

a_all <- merge(x = a_initiate01,
               y = a_end01 [, c("SYMBOL", "group", "enddt", "end"), ],
               by =c("SYMBOL", "group"))

a_all <- a_all [, pnl := ifelse(overalltrend == 1, end - start, start - end), ]
a_all <- a_all [, cumpnl := cumsum(pnl), ]
a_all <- a_all [, cumrs_4lots := cumpnl * 25 * 4, ]
a_all <- a_all [, pnlrs_4lots := pnl * 25 * 4, ]
a_all <- a_all [, capital := 800000, ] 
