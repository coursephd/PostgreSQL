library(data.table)
library(tidyverse)
library(readxl)
library(httr)
library(dplyr)
library(scales)
library(anytime)
library(derivmkts)
library(RQuantLib)

# Create all the directories and files based on the date
# One date DDMONYYYY should be put as input
# Rest of the files will be extracted based on this date

date <- c("29JAN2019")

# Only month in upcase
# Month number
# Year
mon <- toupper(format(anydate(date),"%b"))
mon_num <- format(anydate(date),"%m")
day <- toupper(format(anydate(date),"%d"))
yr <- toupper(format(anydate(date),"%Y"))

monyr <- paste(yr, "/", mon, sep="")

# For bhav: 2019/JAN/fo18JAN2019bhav.csv.zip
date_char <- paste("fo", date, "bhav.csv", sep ="")

# For other files
date_num <- paste(day, mon_num, yr, sep="")

# Contract size automation
cntrt <- fread("https://www.nseindia.com/content/fo/fo_mktlots.csv")

# Bhavcopy automation
# The folder structure is:
# "https://www.nseindia.com/content/historical/DERIVATIVES/2019/JAN/fo18JAN2019bhav.csv.zip

temp <- tempfile()
download.file( paste("https://www.nseindia.com/content/historical/DERIVATIVES/", monyr, "/", date_char, ".zip", sep=""), temp)
bhavcopy <- fread(unzip(temp, files = date_char))
rm(temp)

# Daily volatality automation
# Keep the last day stock price
# The folder structure is:
# "https://www.nseindia.com/archives/nsccl/volt/FOVOLT_18012019.csv"

volality <- fread( paste("https://www.nseindia.com/archives/nsccl/volt/FOVOLT_", date_num, ".csv", sep="") )
names(volality)[3]<-"LASTCLOSE"
names(volality)[4]<-"PRVTOLASTCLOSE"

volality <-  volality [ , c(1:4)]

# Daily settlement Prices
# Folder structure
# https://www.nseindia.com/archives/nsccl/sett/FOSett_prce_18012019.csv

settle <- fread( paste("https://www.nseindia.com/archives/nsccl/sett/FOSett_prce_", date_num, ".csv", sep="") )

# Securities with bans remove then in 
# Folder structure
# https://www.nseindia.com/archives/fo/sec_ban/fo_secban_18012019.csv

ban <- fread( paste("https://www.nseindia.com/archives/fo/sec_ban/fo_secban_", date_num, ".csv", sep="") )
names(ban)[2]<-"SYMBOL"

# MWPL
#A stock comes under ban period when its open interest crosses 
# 95% of MWPL(market wide position limit)
# I.e. when the combined open interest in futures and option in 
# all the available contracts taken together crosses the prescribed limit. 

# Would it be good to keep away from any listed here? 
# https://www.nseindia.com/content/nsccl/mwpl_cli_18012019.xls

temp <- paste("https://www.nseindia.com/content/nsccl/mwpl_cli_", date_num, ".xls", sep="")

GET(temp, write_disk(tf <- tempfile(fileext = ".xls")))
mwpl <- read_excel(tf, skip = 1)
rm(temp)
names(mwpl)[2]<-"SYMBOL"

# Exposure Limit file (csv)
# Would it be ok to keep away from any stock where there is additional limit levied
# https://www.nseindia.com/archives/exp_lim/ael_18012019.csv

explmt <- fread( paste("https://www.nseindia.com/archives/exp_lim/ael_", date_num, ".csv", sep="") )

names(explmt)[2]<-"SYMBOL"
names(explmt)[4]<-"ADDLMT"
explmt02 <- explmt [ ADDLMT > 0]

# Haircut for securities, may not be useful for calculations
# https://www.nseindia.com/content/equities/APPSEC_COLLVAL_18012019.csv

haircut <- fread( paste("https://www.nseindia.com/content/equities/APPSEC_COLLVAL_", date_num, ".csv", sep="") )

# Merge contract size and bhavcopy

bhavcopy02 <- merge (x = cntrt,
                     y = bhavcopy,
                     by = c("SYMBOL"))

bhavcopy02 <- merge (x = bhavcopy02,
                     y = volality,
                     by.x = c("SYMBOL"),
                     by.y = c("Symbol"))

# & INSTRUMENT == "OPTSTK" 
# CONTRACTS > 0  &
bhavcopy03 <- bhavcopy02 [ INSTRUMENT == "OPTSTK" &
                             ! (SYMBOL %in% ban$SYMBOL) &
                             ! (SYMBOL %in% explmt02$SYMBOL)]

# create a numeric date variable and expiry date
bhavcopy03 <- bhavcopy03[, `:=` (trday = anydate(TIMESTAMP), 
                                 nexpday = anydate(EXPIRY_DT)), ]

# Presently only kept the Jan-19 values for contract sizes
cal01 <- bhavcopy03 [, c("SYMBOL", "UNDERLYING", "INSTRUMENT", "STRIKE_PR",
                         "OPTION_TYP", "LASTCLOSE", "trday", "nexpday", "JAN-19")]

# Download file as Results.csv
# https://www.bseindia.com/corporates/Forth_Results.aspx

results <- fread("C:/Users/user/Downloads/Results.csv")

names(results)[1]<-"number"
names(results)[2]<-"SYMBOL"
names(results)[3]<-"name"
names(results)[4]<-"resdate"

# Keep only the F&O companies

results02 <- merge (x = results,
                    y = cntrt [, c("SYMBOL"),],
                    by = c("SYMBOL"),
                    all.y = TRUE)
results02 <- results02 [, resdtn := anydate(resdate),]

# File to get all stocks historic data VBA macro
# http://investexcel.net/multiple-stock-quote-downloader-for-excel/
# Execute the VBA macro on Yahoo finance and get the data for each of F&O companies

temp <- "D:/My-Shares/prgm/Multiple Stock Quote Downloader.xlsm"
yf <- data.table(read_excel(temp, sheet = "Adjusted Close Price"))
yf <- yf [, trday := anydate(Date), ]
yf_tr <- melt (yf,
               id = c("Date", "trday" ),
               variable.name = "SYMBOL02",
               value.name = "price")

yf_tr <- yf_tr [, Symbol := word(SYMBOL02, 1, sep= "\\."),]

# Sort the data for each company by day
yf_tr <- yf_tr [ order(Symbol, trday)]

# Calculate previous value for each company and 
# log (value / prv) = Daily returns

yf_tr <- yf_tr [, prv := shift(price), by = .(Symbol)]
yf_tr <- yf_tr [, dlyrtn := log( as.numeric(price) / as.numeric(prv) ) , ]

yf_tr0 <- na.omit(yf_tr, cols="dlyrtn")
# Calculate average returns and volatility for the period, for a year
# The calculated volatility is daily
# * sqrt(uniqueN(trday)

# Calculate the avg daily rice as well as moving averages

yf_tr0 <- yf_tr0 [, `:=`(avgdaily = mean(dlyrtn, na.rm = TRUE) ,
                         sddaily = sd(dlyrtn, na.rm = TRUE) ,
                         avgprice = mean(as.numeric(price), na.rm = TRUE),
                         avg50s = frollmean(as.numeric(price), 50),
                         avg100s = frollmean(as.numeric(price), 100),
                         avg200s = frollmean(as.numeric(price), 200),
                         nrow = 1:.N,
                         maxrow = .N
)
, by = .(Symbol)]

yf_tr02 <- yf_tr0 [ maxrow == nrow]


# Merge Option chain and the variability
cal02 <- merge (x = cal01,
                y = yf_tr02 [, -c("trday"), ],
                by.x = c("SYMBOL"),
                by.y = c("Symbol"))

# Merge Option chain and the variability
cal02 <- merge (x = cal02,
                y = results02,
                by = c("SYMBOL"))

# Maximum number of days to expiry
cal02 <- cal02 [, maxday :=  as.numeric(nexpday - trday), ]

# Create 1 day till each expiry date 
cal03 <- cal02 [, (list( cumday = (1: maxday) ) ),
                by = .(SYMBOL, INSTRUMENT, STRIKE_PR, OPTION_TYP, resdate, resdtn,
                       nexpday, LASTCLOSE, trday, maxday, sddaily, avgdaily,
                       avg50s, avg100s, avg200s, `JAN-19`) ]

# Calculate for x number of days
cal03 <- cal03 [, `:=` (avgxday = avgdaily * cumday, sdxday = sddaily * sqrt(cumday) ),]

# Keep contracts only till next 45 days

cal04 <- cal03 [INSTRUMENT %like% c("OPT") & OPTION_TYP == "CE"  & maxday <= 100]

# Create Confidence interval

cal04 <- cal04 [, `:=` (xup01 = avgxday + sdxday, xlw01 = avgxday - sdxday,
                        xup02 = avgxday + 2*sdxday, xlw02 = avgxday - 2*sdxday,
                        xup03 = avgxday + 3*sdxday, xlw03 = avgxday - 3*sdxday),]

cal04 <- cal04 [, `:=` (xup01p = LASTCLOSE * exp(xup01), xlw01p = LASTCLOSE * exp(xlw01),
                        xup02p = LASTCLOSE * exp(xup02), xlw02p = LASTCLOSE * exp(xlw02),
                        xup03p = LASTCLOSE * exp(xup03), xlw03p = LASTCLOSE * exp(xlw03),
                        nobs = .I,
                        x2 = paste( "dset", str_pad(.I, 6, side = "left", pad = 0), SYMBOL, sep="_")),]

dd2 <- greeks(bscall(s = cal04$LASTCLOSE, 
                     k = cal04$STRIKE_PR, 
                     v = as.numeric(cal04$sdxday), 
                     r = 0.06, 
                     tt = cal04$maxday / 365, 
                     d = 0 ), complete=FALSE, long=FALSE, initcaps=TRUE)

dd3 <- setDT( list(dd2))
dd3 <-dd3 [, nobs := ceiling(.I/8),]
dd3 <- dd3 [, indrow := 1:.N, by = .(nobs)]

dd3_tr <- dcast (data = dd3,
                 nobs ~ indrow,
                 value.var = c("V1"))

setnames(dd3_tr, "1", "premium")
setnames(dd3_tr, "2", "delta")
setnames(dd3_tr, "3", "gamma")
setnames(dd3_tr, "4", "vega")
setnames(dd3_tr, "5", "rho")
setnames(dd3_tr, "6", "theta")
setnames(dd3_tr, "7", "psi")
setnames(dd3_tr, "8", "elasticity")

# Final dataset for analysis purpose:

cal05 <- merge(x = cal04,
               y = dd3_tr,
               by = c("nobs"))

# Keep only records for where strike price >= last close
# Calculate the % difference between strike price and last close
# Keep only records with >= 15% difference 

#cal06 <- cal05 [ maxday > 6 & STRIKE_PR > LASTCLOSE]
cal06 <- cal05 [, per := as.numeric(STRIKE_PR) / as.numeric(LASTCLOSE),]
cal07 <- cal06 [ cumday == maxday & 
                   cumday >5 & 
                   per >= 1.15 & 
                   (1-delta) >= 0.9 & 
                   LASTCLOSE < avg50s & avg50s < avg100s & avg100s < avg200s]

temp <- paste("http://download.nirmalbang.com/odin/EquityCommodity/Marginfiles/FNO%20MARGIN%20", date_num, ".xls", sep="")

GET(temp, write_disk(tf <- tempfile(fileext = ".xls")))
marfile <- read_excel(tf)
rm(temp)


##############################################################################################
# Implied volatility calculation

library(RCurl)

seq(as.Date('2017-01-01'),as.Date('2017-01-31')+365,by = 1)

# from today to 2 years behind
tday <- Sys.Date()

#format(Sys.Date(), "%d%b%Y")  

dates <- seq ( tday - (2 * 365), tday, by=1)

mon <- toupper(format(anydate(dates),"%b"))
mon_num <- format(anydate(dates),"%m")
day <- toupper(format(anydate(dates),"%d"))
yr <- toupper(format(anydate(dates),"%Y"))

monyr <- paste(yr, "/", mon, sep="")

# For bhav: 2019/JAN/fo18JAN2019bhav.csv.zip
date_char <- paste("fo", toupper(format(dates, "%d%b%Y")), "bhav.csv", sep ="")

# For other files
date_num <- paste(day, mon_num, yr, sep="")

n <- length(dates)
out <- vector('list', n)
for (i in 1:n) { 
  
  url <- paste("https://www.nseindia.com/archives/nsccl/volt/FOVOLT_", date_num[i], ".csv", sep="")
  print(i)
  if (url.exists(url))
    out[[i]] <- fread( url )
  
}

out2 <- rbindlist(out)

saveRDS (out2, "D:/My-Shares/data_tickers/volatility.rds")

out2 <- readRDS("D:/My-Shares/data_tickers/volatility.rds")

out3 <- out2 
names(out3)[16]<-"volatility"
out3 <- out3 [, volatility := volatility * 100,]

out4 <- out3 [, c("Date", "Symbol", "volatility"),]
out4 <- out4 [ order(Symbol, Date)]
out4 <- out4 [, `:=` (monyr = substring(Date, 4),
                      nobs10 = ceiling(.I/10),
                      nobs20 = ceiling(.I/20),
                      nobs30 = ceiling(.I/30),
                      nobs45 = ceiling(.I/45),
                      nobs60 = ceiling(.I/60),
                      nobs90 = ceiling(.I/90)), by = .(Symbol)]

out5 <- melt(data = out4,
             id.vars = 1:4,
             variable.name = "day",
             value.name = "dayn")

out6 <- out5 [, .(min = min(volatility),
                  max = max(volatility),
                  mean = mean(volatility),
                  sd = sd(volatility)), by =.(Symbol, day, dayn)]

out6 <- out6 [, `:=` (mnsd10 = mean + sd,
                      mnsd20 = mean + 2*sd,
                      mnsd30 = mean + 3*sd,
                      mnsd01 = mean - sd,
                      mnsd02 = mean - 2*sd,
                      mnsd03 = mean - 3*sd),]

out7 <- melt(data = out6 [, -c("sd"),],
             id.vars = 1:3)
out7 <- out7 [, period := substring(day, 5),]

fwrite(out7, "D:/My-Shares/data_tickers/volatility_cone.csv")


#############################################################################################

# Simple checks on the spot price and strike 

s=c(62:80); k=80; v=0.40; r=0.08; tt=c(1:31); d=0;
data <- data.table(expand.grid(s = s, k = k, v = v, r = r, tt = tt, d = d))
data <- data [, nobs := .I,]

dd20 <- greeks(bscall(s = data$s, 
                      k = data$k, 
                      v = data$v, 
                      r = data$r, 
                      tt = data$tt / 365, 
                      d = data$d), complete=FALSE, long=FALSE, initcaps=TRUE)

dd3 <- setDT( list(dd20))
dd3 <-dd3 [, nobs := ceiling(.I/8),]
dd3 <- dd3 [, indrow := 1:.N, by = .(nobs)]

dd3_tr <- dcast (data = dd3,
                 nobs ~ indrow,
                 value.var = c("V1"))

setnames(dd3_tr, "1", "premium")
setnames(dd3_tr, "2", "delta")
setnames(dd3_tr, "3", "gamma")
setnames(dd3_tr, "4", "vega")
setnames(dd3_tr, "5", "rho")
setnames(dd3_tr, "6", "theta")
setnames(dd3_tr, "7", "psi")
setnames(dd3_tr, "8", "elasticity")


cal05 <- merge(x = data,
               y = dd3_tr,
               by = c("nobs"))

cal05 <- cal05 [, premium02 := round(premium, digits = 2)]
cal05 [order(s, -tt)]

prm_t <- dcast(data = cal05,
               s ~ tt,
               value.var = c("premium02"))
prm_t <- prm_t [, s2 := s,]
############################################################################################



# This does not work for large datasets

cal04 <- cal04 [, calls :=paste(x2, "<- setDT(EuropeanOption(type ='call', underlying =", LASTCLOSE, ", strike =", STRIKE_PR, ", dividendYield = 0, riskFreeRate = 0.07, maturity =", cumday / 365, ", volatility =", sdxday, "))", sep=""),]

# This creates mcall_shares.txt file, where the macro call is created
# follwing is one such example

outfile <- "D://My-Shares//prgm//macall_shares.R"

# SYMBOL == "ACC"

fwrite(cal04 [, c("calls"),],
       outfile,
       row.names = FALSE,
       col.names = FALSE,
       quote = FALSE,
       sep=" ")

# Execute the program
source("D://My-Shares//prgm//macall_shares.R")
agreek <- rbindlist(mget(ls(pattern = "dset*")), fill = TRUE)

rm(list = ls( pattern='^dset*'))

##############################################################################################


# calculate Call and Put prices
cal04 <- cal04 [OPTION_TYP == "CE", pricecall := list( bscall (s = LASTCLOSE,
                                               k = STRIKE_PR,
                                               v = as.numeric(sddaily) * sqrt(maxday) * 100,
                                               tt = maxday / 365,
                                               r = 0.06,
                                               d = 0) ), ]

cal04 <- cal03 [, erpcall := EuropeanOptionArrays(type = "call", 
                                            underlying = LASTCLOSE, 
                                            strike = STRIKE_PR,
                                            dividendYield = 0.01, 
                                            riskFreeRate = 0.06, 
                                            maturity = maxday / 365, 
                                            volatility = sddaily * sqrt(maxday)) , ]

temp <- cal03 [SYMBOL =="BHARTIARTL" & OPTION_TYP == "CE" & cumday ==13 ]

# Margin calculator file:
# https://www.swastika.co.in/span-margin -- need to download manually

# Daily return formula = LN (Today’s Value / Yesterday’s Value) expressed as a percentage
# Standard Deviation of daily return is equivalent of daily volatility
# Annualised returns / SQRT (365) OR
# Annualised returns / SQRT (262)
# For a pre-specified number of days x: Annualised returns / SQRT (x)


# Use the following information for the prediction
# Date = 11th August 2015                    Current date
# Number of days for expiry = 16             Expiry date OR then the date of planned exit
# Nifty current market price = 8462          Current price of underlying stock
# Daily Average Return = 0.04%               Daily average returns (need to find)
# Annualized Return = 14.8%                  Annualised returns (need to find)
# Daily SD = 0.89%                           
# Annualized SD = 17.04%


"/products/content/sec_bhavdata_full.csv"



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


