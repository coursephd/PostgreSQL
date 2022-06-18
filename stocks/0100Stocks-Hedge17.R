library(data.table)
library(tidyverse)
library(anytime)
library(derivmkts)

options(scipen = 999)

nifty0 <- c(10551)
bnknifty0 <- c(22014) #c(21370)

nifty_dt <- anydate(c("30JUL2020") )
bnk_dt <- anydate (c("09JUL2020") )

nifty_lot <- 2
nifty_bnk <- nifty_lot * 2

dtcal <- anydate(Sys.Date() )


min_nifty <- ifelse(nifty0 < 10000, signif(nifty0, 2), signif(nifty0, 3) )
max_nifty <- min_nifty + 700

min_bnk <- ifelse(bnknifty0 < 10000, signif(bnknifty0, 2), signif(bnknifty0, 3) )
max_bnk <- min_bnk + 1500


nifty50 <- data.table(nifty = runif(20, min = min_nifty - 200, max = max_nifty) )
nifty50 <- nifty50 [order (nifty)]

nifty50 <- rbind(nifty50, nifty = nifty0, use.names = FALSE)

strikes <- data.table( strike50 = seq(from = min_nifty, to = max_nifty, by =50) )
time <- data.table( time = seq(from =1, to = 30, by =1) )

data01nifty <- crossing (nifty50, strikes, time, dtcal, nifty_dt, nifty_lot)
data01nifty <- data01nifty [, `:=` (v = 0.2, 
                                    r = 0.05, 
                                    d = 0, 
                                    timeday50 = as.numeric( (nifty_dt - dtcal + 1) ) ),]
data01nifty <- data01nifty [, timeyrs50 := timeday50 / 365.25, ]

# Calculate the premium value based on the B&S formula:
data01nifty <- data01nifty [, premium50 := round( bscall(s = nifty, k = strike50, v = v, r = r, tt = timeyrs50, d = d), 2), ]

data01nifty <- data01nifty [, `:=`( current50 = ifelse (nifty == nifty0, "current", "simulation"),
                          type50 = ifelse(nifty <= strike50, "Below", "Above") ),]

# Keep only the rows where the actual number of days are left
# time = timday50
data02nifty <- data01nifty [ time == timeday50 & type50 == "Below"  & current50 == "current"]

# Calculate the movement for both NIFTY and BANK NIFTY in either direction
# Calculate notional min and max value and compare with the days start price
# This will help in understanding profit / loss
# The underlying variability is considered varying to some degree

#data02nifty <- data02nifty [, low50 := round( bscall(s = nifty - 300, k = strike50, v = v - 0.02, r = r, tt = timeyrs50, d = d), 2),]
#data02nifty <- data02nifty [, high50 := round( bscall(s = nifty + 300, k = strike50, v = v + 0.02, r = r, tt = timeyrs50, d = d), 2),]

bnknifty <- data.table(bnk = runif(20, min = min_bnk - 4000, max = max_bnk) )
bnknifty <- bnknifty [order (bnk)]

bnknifty <- rbind(bnknifty, bnk = bnknifty0, use.names = FALSE)

# -600 is done to see what has happened behind on 1st July 2020
strikes0 <- data.table( strikebnk = seq(from = min_bnk - 4000, to = max_bnk, by =100) )
time0 <- time [ time <= 7 ]

data01bnknifty <- crossing (bnknifty, strikes0, time0, dtcal, bnk_dt, nifty_bnk)

# Pick the variability for the weekly Bank Nifty -- quite high number for calculations
data01bnknifty <- data01bnknifty [, `:=` (v = 0.4, 
                                          r = 0.05, 
                                          d = 0,
                                          timedaybnk = as.numeric( (bnk_dt - dtcal + 1) ) ),]
data01bnknifty <- data01bnknifty [, timeyrsbnk := timedaybnk / 365.25, ]

# Calculate the premium value based on the B&S formula:
data01bnknifty <- data01bnknifty [, premiumbnk := round( bscall(s = bnk, k = strikebnk, v = v, r = r, tt = timeyrsbnk, d = d), 2), ]

data01bnknifty <- data01bnknifty [, `:=`( currentbnk = ifelse (bnk == bnknifty0, "current", "simulation"),
                                    typebnk = ifelse(bnk <= strikebnk, "Below", "Above") ),]

# Keep only the rows where the actual number of days are left
# time = timdaybnk
data02bnknifty <- data01bnknifty [ time == timedaybnk & typebnk == "Below" & currentbnk == "current"]

# Calculate the movement for both NIFTY and BANK NIFTY in either direction
# Calculate notional min and max value and compare with the days start price
# This will help in understanding profit / loss
# The underlying variability is considered varying to some degree

#v - 0.1, v + 0.1
data02bnknifty <- data02bnknifty [, lowbnk := round( bscall(s = bnk - 500, k = strikebnk, v = v, r = r, tt = timeyrsbnk, d = d), 2), ]
data02bnknifty <- data02bnknifty [, highbnk := round( bscall(s = bnk + 500, k = strikebnk, v =v, r = r, tt = timeyrsbnk, d = d), 2), ]



####################################################################################
# Combine the Bank nifty and nifty50 data
# for the spot price below the strike price
# the strike price should be picked up at the highest point
# find the difference between the premium
# Calculate the Far distanced strike prices so that there is no danger
#
# Calculate the overall premium needed from the expected values
# These premiums could be different based on the actual preiums in the market
# Calculate pricing for each lot and then multiply for number of lots
#
# Bank Nifty : Nifty
# 2:1 ratio has been suggested
####################################################################################

data03all <- crossing (data02nifty [, c("dtcal", "nifty_dt", "nifty", "strike50", "timeday50", "premium50", "nifty_lot", "low50", "high50"), ],
                       data02bnknifty [, c("bnk", "strikebnk", "bnk_dt", "premiumbnk", "nifty_bnk", "lowbnk", "highbnk"), ])

data03all <- data03all [, prmdiff := abs( premium50 - premiumbnk), ]
data03all <- data03all [ order(-strike50)]
data03all <- data03all [, rank50 := .GRP, by = .(strike50)]
data03all <- data03all [ order(-strikebnk)]
data03all <- data03all [, rankbnk := .GRP, by = .(-strikebnk)]
data03all <- data03all [, `:=` (filt50 = ifelse(strike50 >= max_nifty - 200, "Far", "Close"), 
                                filtbnk = ifelse(strikebnk >= max_bnk - 200, "Far", "Close"), 
                                cost50 = premium50 * 75 * nifty_lot,
                                costlow50 = low50 * 75 * nifty_lot,
                                costhigh50 = high50 * 75 * nifty_lot,
                                costbnk = premiumbnk * 20 * nifty_bnk,
                                costlowbnk = lowbnk * 20 * nifty_bnk,
                                costhighbnk = highbnk * 20 * nifty_bnk
                                ), ]
data03all <- data03all [, totcost := cost50 + costbnk,]

data04all <- data03all [ filt50 == "Far" & filtbnk == "Far" & prmdiff >= 20, 
                         c("nifty", "bnk", "strike50", "strikebnk", "premium50", "premiumbnk",  "prmdiff",
                           "nifty_lot", "nifty_bnk", "cost50", "costbnk", "totcost"), ]
                                

###################################################################################
bscall(s = 10360,
       k = seq(from =10500, to = 11000, by =100),
       v = 0.2,
       r = 0.05,
       tt = 30/365,
       d = 0)


bscall(s = 21350,
       k = 22500, #seq(from =22000, to = 23000, by =100),
       v = 0.58,
       r = 0.05,
       tt = 2/365,
       d = 0)
