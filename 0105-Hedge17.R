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

# Create a numeric seq for monthly and weekly series
opt03 <- opt03 [, mnth_seq := .GRP, by = .(mrgdt)]
opt03 <- opt03 [, week_seq := .GRP, by = .(EXPIRY_DT)]

# Create a numeric seq for monthly and weekly series for each of the trading day
opt03 <- opt03 [, mnth_trd := .GRP, by = .(mrgdt)]
opt03 <- opt03 [, week_trd := .GRP, by = .(EXPIRY_DT)]


# Varsha drop the data with mrgdt mot present in fut02
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
bnkexp03 <- unique( na.omit( bnkexp02 [ nrow %in% c(1, 2, 3, 4, 5), c("SYMBOL", "EXPIRY_DT", "strk", "nrow", "mrgdt"), ]) )
bnkexp03 <- bnkexp03 [, `:=`(strk900 = strk + 900, strk1000 = strk + 1000, strk1100 = strk + 1100, strk1200 = strk + 1200, 
                             strk1300 = strk + 1300, 
                             strk1400 = strk + 1400, 
                             strk1500 = strk + 1500, 
                             strk1600 = strk + 1600, 
                             strk1700 = strk + 1700), ]
bnkexp03_t <- melt(data = bnkexp03, 
                   id.vars = c("SYMBOL", "EXPIRY_DT", "strk", "nrow", "mrgdt")  )
bnkexp03_t <- bnkexp03_t [, STRIKE_PR := strk, ]

# Create the trade number to use afterwards
bnkexp03_t <- bnkexp03_t [, tradenum := 1:.N, by = .(EXPIRY_DT)]

bnkexp04 <- merge(x = bnkexp03_t [, -c("strk", "mrgdt", "nrow"),],
                  y = bnkexp02 [, -c("strk"),],
                  by = c("SYMBOL", "EXPIRY_DT", "STRIKE_PR"),
                  allow.cartesian = TRUE)

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

bnkexp08 <- bnkexp07 [ minval >= 90 & entryCONTRACTS > 0] 

bnkexp08 <- bnkexp08 [, alltrades := .GRP, by =.(STRIKE_PR, tradenum, EXPIRY_DT)]

# Calculate strike price for each start day
# Get the OCHL data for the consistent distance
# from start to end day
# This will help in understanding adjustments
bnkexp08 <- bnkexp08 [, strk_adj := ifelse(OPENFUT < 10000, signif(OPENFUT, 2), signif(OPENFUT, 3) ),]
bnkexp08 <- bnkexp08 [, strk_adj := as.numeric(strk_adj) + as.numeric( substr(variable, 5, 10) ), ]

# Merge the original data using
# SYMBOL, trday, nexpday, STRIKE_PR

bnkadj01 <- opt03 [SYMBOL == "BANKNIFTY", c("trday", "TIMESTAMP", "nexpday", "EXPIRY_DT", "STRIKE_PR", 
                                            "HIGH", "LOW", "CLOSE", "OPEN", "CONTRACTS"), ]

setnames (bnkadj01,  "OPEN"          ,'b_adj_o')
setnames (bnkadj01,  "CLOSE"         ,'b_adj_c')
setnames (bnkadj01,  "HIGH"          ,'b_adj_h')
setnames (bnkadj01,  "LOW"           ,'b_adj_l')
setnames (bnkadj01,  "CONTRACTS"     ,'b_adj_contr')

bnkexp08 <- merge (x = bnkexp08,
                   y = bnkadj01,
                   by = c("trday", "TIMESTAMP", "nexpday", "EXPIRY_DT", "STRIKE_PR"),
                   all.x = TRUE)

setnames (bnkexp08,  "STRIKE_PR"     ,'b_strk')
setnames (bnkexp08,  "tradenum"      ,'b_tradenum')
setnames (bnkexp08,  "EXPIRY_DT"     ,'b_exp_dt')
setnames (bnkexp08,  "entryOPEN"     ,'b_entry_o')
setnames (bnkexp08,  "entryCLOSE"    ,'b_entry_c')
setnames (bnkexp08,  "entryLOW"      ,'b_entry_l')
setnames (bnkexp08,  "entryHIGH"     ,'b_entry_h')
setnames (bnkexp08,  "entryCONTRACTS",'b_entry_contr')
setnames (bnkexp08,  "OPEN"          ,'b_exit_o')
setnames (bnkexp08,  "CLOSE"         ,'b_exit_c')
setnames (bnkexp08,  "HIGH"          ,'b_exit_h')
setnames (bnkexp08,  "LOW"           ,'b_exit_l')
setnames (bnkexp08,  "CONTRACTS"     ,'b_exit_contr')
setnames (bnkexp08,  "SYMBOL"        ,'b_SYMBOL')

bnkexp09 <- bnkexp08[, c("b_SYMBOL", "variable", "value", "trday", "mrgdt",  "alltrades", 
                         "b_strk", "b_tradenum", "b_exp_dt", "b_entry_o", "b_entry_c", "b_entry_l", "b_entry_h", "b_entry_contr",
                         "b_exit_o", "b_exit_c", "b_exit_l", "b_exit_h", "b_exit_contr", "nrow",
                         "OPENFUT", "CLOSEFUT", "HIGHFUT", "LOWFUT",
                         "strk_adj", "b_adj_o", "b_adj_c", "b_adj_l", "b_adj_h", "b_adj_contr"),]

fwrite(bnkexp09, "D:/My-Shares/analysis/105_bnk.csv")
saveRDS (bnkexp09, "D:/My-Shares/analysis/105_bnk.rds")


##############################################################################
#
# 9th August 2020:
# Reworked NIFTY calculations
# Need some additional checks
#
##############################################################################
nifty001 <- opt03 [ SYMBOL == "NIFTY" ]
nifty001 <- nifty001 [, `:=` (day = as.numeric(format(trday, "%d") ),
                              mon_num = as.numeric( format(trday,"%m") ),
                              yr = toupper(format(trday,"%Y")),
                              mon_ser = as.numeric( format(nexpday,"%m") ),
                              yr_ser = toupper(format(nexpday,"%Y")),
                              minval = pmin(OPEN, CLOSE, LOW, HIGH), 
                              maxval = pmax(OPEN, CLOSE, LOW, HIGH) ), ]
nifty001 <- nifty001 [, mnthexp := max(nexpday), by = .(mrgdt) ]
nifty002 <- nifty001 [ (minval >= 70 & minval <= 110) & nexpday == mnthexp ] 

# Keep only the contracts of the same month for day <= 15
# Keep the contracts for the next month when day > 15
#
# Delete rows where the difference in mon_num and mon_ser >2 then delete such rows
# 
# ????????????????????????????????????????????????????????????????
#
# Need to add logic for the year change -Dec and Jan how to subset
#
# ????????????????????????????????????????????????????????????????

nifty002a <- nifty002 [yr == yr_ser & mon_ser - mon_num < 2]
nifty002b <- nifty002a [(day <= 15 & mon_ser == mon_num) | (day > 15 & mon_ser > mon_num)]

# Find the unique combinations and create tradenum
nifty002b <- nifty002b [, alltrades := .I,  ]
nifty002b <- nifty002b [, tradenum := 1:.N, by =.(SYMBOL, mnthexp, mrgdt) ]
nifty002b <- nifty002b [, tradedur := as.numeric(mnthexp - trday + 1), ]

#year = year
nifty002c <- nifty002b[ , list(SYMBOL = SYMBOL, STRIKE_PR = STRIKE_PR, mrgdt = mrgdt, alltrades = alltrades,
                               OPEN = OPEN, CLOSE= CLOSE, HIGH= HIGH, LOW = LOW, CONTRACTS = CONTRACTS,
                               tradenum = tradenum, tradedur = tradedur, mnthexp = mnthexp, 
                               OPENFUT = OPENFUT, CLOSEFUT = CLOSEFUT, HIGHFUT = HIGHFUT, LOWFUT = LOWFUT, 
                               trday = anydate( seq(trday, mnthexp, by = "day") )), by = 1:nrow(nifty002b)]
nifty002c <- nifty002c [, trdcumday := 1:.N, by =.(SYMBOL, mnthexp, mrgdt, tradenum)]

setnames (nifty002c, "OPEN", "entryOPEN")
setnames (nifty002c, "CLOSE", "entryCLOSE")
setnames (nifty002c, "LOW", "entryLOW")
setnames (nifty002c, "HIGH", "entryHIGH")
setnames (nifty002c, "CONTRACTS", "entryCONTRACTS")

nifty003 <- merge (x = nifty002c,
                   y = nifty001 [, c("mrgdt", "trday", "TIMESTAMP", "STRIKE_PR", "SYMBOL", "mnthexp", "OPEN", "CLOSE", "HIGH", "LOW", "CONTRACTS", "nexpday"), ] , 
                   by = c("mrgdt", "trday", "STRIKE_PR", "SYMBOL", "mnthexp"), 
                   all.x = TRUE)
nifty004 <- nifty003 [ mnthexp == nexpday]

setnames (nifty004,  "STRIKE_PR"     ,'n_strk')
setnames (nifty004,  "mnthexp"       ,'n_exp_dt')
setnames (nifty004,  "entryOPEN"     ,'n_entry_o')
setnames (nifty004,  "entryCLOSE"    ,'n_entry_c')
setnames (nifty004,  "entryHIGH"     ,'n_entry_l')
setnames (nifty004,  "entryLOW"      ,'n_entry_h')
setnames (nifty004,  "entryCONTRACTS",'n_entry_contr')
setnames (nifty004,  "tradenum"      ,'n_tradenum')
setnames (nifty004,  "OPEN"          ,'n_exit_o')
setnames (nifty004,  "CLOSE"         ,'n_exit_c')
setnames (nifty004,  "HIGH"          ,'n_exit_h')
setnames (nifty004,  "LOW"           ,'n_exit_l')
setnames (nifty004,  "CONTRACTS"     ,'n_exit_contr')


fwrite(nifty004, "D:/My-Shares/analysis/105_nifty.csv")
saveRDS (nifty004, "D:/My-Shares/analysis/105_nifty.rds")

#######################################################
# End of program
#######################################################



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

nifty003 <- nifty002 [, c("SYMBOL", "trday", "STRIKE_PR", "OPEN", "CLOSE", "LOW", "HIGH", "mrgdt", "CONTRACTS")]
nifty003 <- nifty003 [, entrydt := trday,]
setnames (nifty003, "OPEN", "OPENentry")
setnames (nifty003, "CLOSE", "CLOSEentry")
setnames (nifty003, "LOW", "LOWentry")
setnames (nifty003, "HIGH", "HIGHentry")
setnames (nifty003, "CONTRACTS", "CONTRACTSentry")

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
