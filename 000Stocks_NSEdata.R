library(data.table)
library(tidyverse)
library(readxl)
library(httr)
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
bhavcopy03 <- bhavcopy02 [CONTRACTS > 0  & 
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
unqstr <- unique(bhavcopy03 [, c("SYMBOL", "OPTION_TYP", "totcontract", "EXPIRY_DT", "INSTRUMENT", "STRIKE_PR", "CONTRACTS"), ] )
bhavcopy030 <- dcast( unqstr,
                      formula = SYMBOL + INSTRUMENT + STRIKE_PR + EXPIRY_DT ~ OPTION_TYP,
                      value.var =c("totcontract", "CONTRACTS"),
                      fill = "0")


# Create overall sum of open_int for each security including F&O
bhavcopy031 <- bhavcopy031 [, totopenint := sum(OPEN_INT), by = .(SYMBOL, EXPIRY_DT)]


# A rising put-call ratio or greater than .7 or exceeding 1 means equity traders 
# are buying more puts than calls and indicates a bearish sentiment 
# is building in the market

# A falling put-call ratio or below .7 and approaching .5 is considered bullish 
# since it means more calls are being bought versus puts. 
# In other words, the market has a bullish sentiment. 

# Count total companies >= 500
bhavcopy04 <- bhavcopy031 [ totcontract >= 500]
