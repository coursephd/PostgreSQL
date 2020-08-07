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

options(scipen = 999)

# Read the data from the permenant dataset
out <- as.data.table( readRDS ("D:/My-Shares/analysis/bhavvopy_all.rds") )

out2 <- out [SYMBOL %in% c("NIFTY", "BANKNIFTY")]

fut <- out [INSTRUMENT %in% c("FUTIDX") & SYMBOL %in% c("NIFTY", "BANKNIFTY") & OPTION_TYP == "XX"]
opt <- out [INSTRUMENT %in% c("OPTIDX") & SYMBOL %in% c("NIFTY", "BANKNIFTY") & OPTION_TYP %in% c("CE") ]


# create a numeric date variable and expiry date
fut <- fut [, `:=` (trday = anydate(TIMESTAMP), nexpday = anydate(EXPIRY_DT)), ]
opt <- opt [, `:=` (trday = anydate(TIMESTAMP), nexpday = anydate(EXPIRY_DT)), ]

# Merge the future prices along with the options prices

opt02 <- opt [, c("INSTRUMENT", "SYMBOL", "STRIKE_PR", "trday", "nexpday", "EXPIRY_DT", "OPEN", "CLOSE", "HIGH", "LOW", "CONTRACTS", "TIMESTAMP", "OPTION_TYP"), ]
fut02 <- fut [, c("INSTRUMENT", "SYMBOL", "trday", "nexpday", "EXPIRY_DT", "OPEN", "CLOSE", "HIGH", "LOW", "SETTLE_PR", "TIMESTAMP"), ]

setnames(fut02, "INSTRUMENT", "FUT")
setnames(fut02, "OPEN", "OPENFUT")
setnames(fut02, "CLOSE", "CLOSEFUT")
setnames(fut02, "HIGH", "HIGHFUT")
setnames(fut02, "LOW", "LOWFUT")

opt02 <- opt02 [, mrgdt := substr(EXPIRY_DT, 4, length(EXPIRY_DT)), ]
fut02 <- fut02 [, mrgdt := substr(EXPIRY_DT, 4, length(EXPIRY_DT)), ]

opt03 <- merge(x = opt02,
               y = fut02 [, -c("EXPIRY_DT", "nexpday"),],
               by = c("SYMBOL", "trday", "mrgdt", "TIMESTAMP"),
               all = TRUE)

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

bnkexp03 <- unique( na.omit( bnkexp02 [ nrow %in% c(1, 2, 3, 4, 5), c("SYMBOL", "EXPIRY_DT", "strk", "nrow"), ]) )
bnkexp03 <- bnkexp03 [, `:=`(strk1300 = strk + 1300, 
                             strk1400 = strk + 1400, 
                             strk1500 = strk + 1500, 
                             strk1600 = strk + 1600, 
                             strk1700 = strk + 1700), ]
bnkexp03_t <- melt(data = bnkexp03, 
                   id.vars = c("SYMBOL", "EXPIRY_DT", "strk", "nrow")  )
setnames (bnkexp03_t, "nrow", "entryday")

bnkexp04 <- merge(x = bnkexp03_t [, -c("strk"),],
                  y = bnkexp02 [, -c("strk"),],
                  by = c("SYMBOL", "EXPIRY_DT"),
                  allow.cartesian = TRUE)

# Only keep the records whre strk and STRIKE_PR match
bnkexp05 <- bnkexp04 [ value == STRIKE_PR]

# Calculate the combinations of contracts and number of times success
# These calculations are only based on the BANK NIFTY data

bnkexp100 <- bnkexp05 [ nrow == 1]
bnkexp100 <- bnkexp100 [, ntrades := .N,]
bnkexp100 <- bnkexp100 [, pftloss := ifelse(LOW < 1, "Profit", "Loss"),]
bnkexp101 <- bnkexp100 [, .(cnt = .N), by = .(pftloss, ntrades)]

# Pick up Nifty on any of the first 5 days in the 100s range:

nifty001 <- opt03 [ SYMBOL == "NIFTY" & toupper(mrgdt) == toupper( substr(TIMESTAMP, 4, length(TIMESTAMP) ) ) ]
nifty001 <- nifty001 [, `:=` (day = format(trday, "%d"),
                              minval = pmin(OPEN, CLOSE, LOW, HIGH), 
                              maxval = pmax(OPEN, CLOSE, LOW, HIGH) ), ]
nifty001 <- nifty001 [, mnthexp := max(nexpday), by = .(mrgdt) ]
nifty002 <- nifty001 [ (minval >= 70 & minval <= 130) & 
                         mnthexp == nexpday  ]

nifty003 <- nifty002 [, c("SYMBOL", "trday", "STRIKE_PR", "OPEN", "CLOSE", "LOW", "HIGH", "mrgdt")]
nifty003 <- nifty003 [, entrydt := trday,]
setnames (nifty003, "OPEN", "OPENentry")
setnames (nifty003, "CLOSE", "CLOSEentry")
setnames (nifty003, "LOW", "LOWentry")
setnames (nifty003, "HIGH", "HIGHentry")

nifty004 <- merge(x = nifty003 [, -c("trday"), ],
                  y = nifty001,
                  by = c("SYMBOL", "STRIKE_PR", "mrgdt"),
                  allow.cartesian = TRUE)
nifty005 <- nifty004 [ entrydt <= trday & mnthexp == nexpday ]

# Calculate the number of trades by creating a group
# SYMBOL, STRIKE_PR, mrgdt, entrydt, trday, nexpday, EXPIRY_DT
nifty005 <- nifty005 [, `:=`(tradenum = 1:.N), by = .(SYMBOL, STRIKE_PR, mrgdt, entrydt, nexpday, EXPIRY_DT)]
nifty005 <- nifty005 [, `:=`(tradetot = .N, cumtrade = 1:.N), by = .(mrgdt)]
nifty005 <- nifty005 [, tradedays := as.numeric(trday - entrydt + 1),]

# 30th April 2020 example: Nifty and Bank Nifty went mad and increased over 2000 points in a week


#######################################################
# End of program
#######################################################
# For NIFTY determine, 1st or 2nd of month to pick up the NIFTY contract
# If the contract value decreases to 30 then sell with a loss and pick up the
# contract after the 15th of that month


  
saveRDS (opt03, "D:/My-Shares/analysis/0103bhavvopy_nf_bnf.rds")
fwrite(opt03, "D:/My-Shares/analysis/0103bhavvopy_nf_bnf.csv")


opt020 <- opt [, c("INSTRUMENT", "SYMBOL", "trday", "nexpday", "EXPIRY_DT", "OPEN", "CLOSE", "HIGH", "LOW", "SETTLE_PR", "CONTRACTS", "TIMESTAMP", "OPTION_TYP", "STRIKE_PR"), ]
fut020 <- fut [, c("INSTRUMENT", "SYMBOL", "trday", "nexpday", "EXPIRY_DT", "OPEN", "CLOSE", "HIGH", "LOW", "SETTLE_PR", "CONTRACTS", "TIMESTAMP", "OPTION_TYP", "STRIKE_PR"), ]

opt030 <- rbind(opt020, fut020)

saveRDS (opt030, "D:/My-Shares/analysis/0103bhavvopy_nf_bnf_vert.rds")
fwrite(opt030, "D:/My-Shares/analysis/0103bhavvopy_nf_bnf_vert.csv")

#####################################################################################
# End of program
#####################################################################################
