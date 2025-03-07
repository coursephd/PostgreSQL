library(data.table)
library(tidyverse)
library(readxl)
library(httr)
library(dplyr)
library(scales)
library(fCertificates)

# https://www.nseindia.com/products/content/derivatives/equities/homepage_fo.htm

# Contract size automation
cntrt <- fread("https://www.nseindia.com/content/fo/fo_mktlots.csv")

# Bhavcopy automation
#bhavcopy <- fread("curl https://www.nseindia.com/content/historical/DERIVATIVES/2019/JAN/fo11JAN2019bhav.csv.zip | funzip")
temp <- tempfile()
download.file("https://www.nseindia.com/content/historical/DERIVATIVES/2019/JAN/fo11JAN2019bhav.csv.zip", temp)
bhavcopy <- fread(unzip(temp, files = "fo11JAN2019bhav.csv"))
rm(temp)

# Daily volatality automation
volality <- fread("https://www.nseindia.com/archives/nsccl/volt/FOVOLT_11012019.csv")
names(volality)[3]<-"LASTCLOSE"
names(volality)[4]<-"PRVTOLASTCLOSE"

# Daily settlement Prices
settle <- fread("https://www.nseindia.com/archives/nsccl/sett/FOSett_prce_11012019.csv")

# Securities with bans remove then in 
ban <- fread("https://www.nseindia.com/archives/fo/sec_ban/fo_secban_11012019.csv")
names(ban)[2]<-"SYMBOL"

# MWPL
#A stock comes under ban period when its open interest crosses 
# 95% of MWPL(market wide position limit)
# I.e. when the combined open interest in futures and option in 
# all the available contracts taken together crosses the prescribed limit. 

# Would it be good to keep away from any listed here? 

temp <- "https://www.nseindia.com/content/nsccl/mwpl_cli_10012019.xls"
GET(temp, write_disk(tf <- tempfile(fileext = ".xls")))
mwpl <- read_excel(tf, skip = 1)
rm(temp)
names(mwpl)[2]<-"SYMBOL"

# Exposure Limit file (csv)
# Would it be ok to keep away from any stock where there is additional limit levied

explmt <- fread("https://www.nseindia.com/archives/exp_lim/ael_11012019.csv")
names(explmt)[2]<-"SYMBOL"
names(explmt)[4]<-"ADDLMT"
explmt02 <- explmt [ ADDLMT > 0]

# Haircut for securities, may not be useful for calculations
haircut <- fread("https://www.nseindia.com/content/equities/APPSEC_COLLVAL_11012019.csv")

# Merge contract size and bhavcopy

bhavcopy02 <- merge (x = cntrt,
                     y = bhavcopy,
                     by = c("SYMBOL"))

bhavcopy02 <- merge (x = bhavcopy02,
                     y = volality,
                     by.x = c("SYMBOL"),
                     by.y = c("Symbol"))

# & INSTRUMENT == "OPTSTK"
bhavcopy03 <- bhavcopy02 [CONTRACTS > 0  & INSTRUMENT == "OPTSTK" &
                          ! (SYMBOL %in% ban$SYMBOL) &
                          ! (SYMBOL %in% mwpl$SYMBOL) &
                          ! (SYMBOL %in% explmt02$SYMBOL)]

# Count the total number of CONTRACTS
bhavcopy03 <- bhavcopy03 [, totcontract := sum(CONTRACTS), by = .(SYMBOL, INSTRUMENT, EXPIRY_DT, OPTION_TYP)]


# https://www.investopedia.com/ask/answers/06/putcallratio.asp

unq <- unique(bhavcopy03 [, c("SYMBOL", "OPTION_TYP", "totcontract", "EXPIRY_DT", "INSTRUMENT"), ] )

# Create overall Put and Call counts columns and ratios for each company
bhavcopy030 <- dcast( unq,
                     formula = SYMBOL + INSTRUMENT + EXPIRY_DT ~ OPTION_TYP,
                     value.var =c("totcontract"),
                     fill = "0")
bhavcopy030 <- bhavcopy030 [, putcallratio := PE / CE,]

bhavcopy031 <- merge (x = bhavcopy03,
                      y = bhavcopy030,
                      by = c("SYMBOL", "EXPIRY_DT", "INSTRUMENT"))

# create putcall ratio for each strike price
unqstr02 <- unique(bhavcopy03 [, c("SYMBOL", "OPTION_TYP", "totcontract", "EXPIRY_DT", "INSTRUMENT", "STRIKE_PR", "CONTRACTS"), ] )
bhavcopy0302 <- dcast( unqstr02,
                      formula = SYMBOL + INSTRUMENT + STRIKE_PR + EXPIRY_DT ~ paste("ind_", OPTION_TYP, sep=""),
                      value.var =c("CONTRACTS"),
                      fill = "0")

bhavcopy0302 <- bhavcopy0302 [, putcallratioind := ind_PE / ind_CE, ]

bhavcopy0303 <- merge (x = bhavcopy031,
                      y = bhavcopy0302,
                      by = c("SYMBOL", "EXPIRY_DT", "INSTRUMENT", "STRIKE_PR"))


# Create overall sum of open_int for each security including F&O
bhavcopy0303 <- bhavcopy0303 [, totopenint := sum(OPEN_INT), by = .(SYMBOL, EXPIRY_DT)]
bhavcopy0303 <- bhavcopy0303 [, totoptint := sum(OPEN_INT), by = .(SYMBOL, INSTRUMENT, EXPIRY_DT)]
bhavcopy0303 <- bhavcopy0303 [, totchgint := sum(CHG_IN_OI), by = .(SYMBOL, INSTRUMENT, EXPIRY_DT)]

# create quantiles based on the number of Open interest for Options
# 10th quartile should be the highest frequency companies

bhavcopy0303 <- bhavcopy0303 [, `:=` (quartile = ntile(totoptint, 10),
                                      voltquartile = ntile(`Applicable Annualised Volatility (N) = Max (F or L)`, 4),
                                      diff = STRIKE_PR - LASTCLOSE ),] 

bhavcopy0303 <- bhavcopy0303 [order(SYMBOL, INSTRUMENT, EXPIRY_DT, OPTION_TYP, diff)]
bhavcopy0303 <- bhavcopy0303 [, difford := seq_len(.N), by = .(SYMBOL, INSTRUMENT, OPTION_TYP, EXPIRY_DT)]

# A rising put-call ratio or greater than 0.7 or exceeding 1 means equity traders 
# are buying more puts than calls and indicates a bearish sentiment 
# is building in the market

# A falling put-call ratio or below .7 and approaching .5 is considered bullish 
# since it means more calls are being bought versus puts. 
# In other words, the market has a bullish sentiment. 

# Bullish signal: Price rising + Increasing Open interest
# Bullish signal: Price falling + Declining Open interest

# Bearish signal: Price rising + Declining Open interest
# Bearish signal: Price falling + Increasing Open interest

# https://traderslounge.in/nifty-openinterest-pcr/


#Now Create a view: High volume and low variability

disp01 <- unique( bhavcopy0303 [, c("SYMBOL", "quartile", "voltquartile", "totoptint", 
                                    "INSTRUMENT", "putcallratio", "EXPIRY_DT"), ])
disp01 <- disp01 [, num := seq_len(.N), by = .(INSTRUMENT, quartile, voltquartile, EXPIRY_DT)]

disp02 <- dcast(disp01,
                num + INSTRUMENT + EXPIRY_DT + putcallratio + voltquartile ~ quartile,
                value.var = c("SYMBOL") )



# Create Cumulative additions for CONTRACTS, VAL_IN_LAKH, OPEN_INT, CHG_IN_OI
# for each company, index, instrument, each day, expiry date

bhavcopy <- bhavcopy [, `:=` (cCONTRACTS = cumsum(CONTRACTS), 
                              cVAL_INLAKH = cumsum(VAL_INLAKH), 
                              cOPEN_INT = cumsum(OPEN_INT), 
                              cCHG_IN_OI = cumsum (CHG_IN_OI)), 
                      by = .(INSTRUMENT, SYMBOL, EXPIRY_DT, TIMESTAMP, OPTION_TYP)]

test <- dcast (data = bhavcopy,
               subset = . (INSTRUMENT %like% c("OPT") ),
               INSTRUMENT + SYMBOL + EXPIRY_DT + STRIKE_PR + TIMESTAMP ~ OPTION_TYP,
               value.var = c("OPEN", "HIGH", "LOW", "CLOSE", "SETTLE_PR", 
                             "CONTRACTS", "VAL_INLAKH", "OPEN_INT", "CHG_IN_OI",
                             "cCONTRACTS", "cVAL_INLAKH", "cOPEN_INT", "cCHG_IN_OI"))

test01 <- test [, putcallind :=ifelse(CONTRACTS_CE> 0, formatC( CONTRACTS_PE / CONTRACTS_CE, digits = 2, format ="f"), "" ), ]



library(RQuantLib)

BarrierOption(barrType="downin", type="put", underlying=338,
              strike=320, dividendYield=0.02, riskFreeRate=0.03,
              maturity=15/252, volatility=0.4, barrier=90)


EuropeanOption(type="put", underlying=333, strike=320, dividendYield=0, 
               riskFreeRate=0.03, maturity=0.5/12, volatility=0.4)


