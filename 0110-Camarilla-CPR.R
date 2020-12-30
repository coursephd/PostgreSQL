#
# Need to understand the conversion of the date
# The date is UNIX date stamp
# The following URL could be automated for the previous year, previous month, previous week
# Go to Investing.com and get the futures data for bank nifty
# Example:
# url <- "https://in.investing.com/indices/bank-nifty-futures-historical-data?end_date=1577730600&st_date=1546281000&interval_sec=monthly&interval_sec=daily"
#
# R code from the following website: https://r-tastic.co.uk/post/from-messy-to-tidy/
# 2nd table in the HTML page contains the OHLC data 

library(data.table)
library(tidyverse)
library(lubridate)
library(anytime)
library(rvest)
library(xml2)

dt <- Sys.Date()

# Create the reference values for the previous periods:

# Find previous year start to end
prvyr <- as.numeric(format(as.Date( floor_date(dt, "year") - years(1) ), "%Y"))
prvyr01 <- as.Date(paste(prvyr, "01", "01", sep="-"))
prvyr02 <- as.Date(paste(prvyr, "12", "31", sep="-"))

# Find the previous month
prvmnth <- floor_date(dt, "month") - months(1)
prvmnth02 <- prvmnth + as.numeric( days_in_month(prvmnth) ) - 1

# Find previous week
prvwk <- floor_date(dt, "week", week_start = 1) - weeks(1)
prvwk02 <- prvwk + 5

# Get the yearly, monthly, weekly values:
styrdate <- as.numeric(as.POSIXct(prvyr01, format="%Y-%m-%d"))
enyrdate <- as.numeric(as.POSIXct(prvyr02, format="%Y-%m-%d"))

stmnthdate <- as.numeric(as.POSIXct(prvmnth, format="%Y-%m-%d"))
enmthdate <- as.numeric(as.POSIXct(prvmnth02, format="%Y-%m-%d"))

stwkdate <- as.numeric(as.POSIXct(prvwk, format="%Y-%m-%d"))
enwkdate <- as.numeric(as.POSIXct(prvwk02, format="%Y-%m-%d"))

url_yr <- paste("https://in.investing.com/indices/bank-nifty-futures-historical-data?end_date=", enyrdate,  
                "&st_date=", styrdate, "&interval_sec=monthly&interval_sec=daily", sep="")

url_mnth <- paste("https://in.investing.com/indices/bank-nifty-futures-historical-data?end_date=", enmthdate,
                "&st_date=", stmnthdate, "&interval_sec=monthly&interval_sec=daily", sep="")

url_wk <- paste("https://in.investing.com/indices/bank-nifty-futures-historical-data?end_date=", enwkdate,  
                "&st_date=", stwkdate, "&interval_sec=monthly&interval_sec=daily", sep="")

url_html_yr <- read_html(url_yr)
url_html_mnth <- read_html(url_mnth)
url_html_wk <- read_html(url_wk)

# extract the HTML table
whole_table_yr <- url_html_yr %>% 
  html_nodes('table') %>%
  html_table(fill = TRUE) %>%
  .[[2]]

# extract the HTML table
whole_table_mnth <- url_html_mnth %>% 
  html_nodes('table') %>%
  html_table(fill = TRUE) %>%
  .[[2]]

# extract the HTML table
whole_table_wk <- url_html_wk %>% 
  html_nodes('table') %>%
  html_table(fill = TRUE) %>%
  .[[2]]

# For each of the interval calculate the CPR and camarilla equations
# combine the data into one data
# Price column= closing value

whole_table_yr <- as.data.table(whole_table_yr)
whole_table_mnth <- as.data.table(whole_table_mnth)
whole_table_wk <- as.data.table(whole_table_wk)

whole_table_yr <- whole_table_yr [, timefrm := "Yearly", ]
whole_table_mnth <- whole_table_mnth [, timefrm := "Monthly", ]
whole_table_wk <- whole_table_wk [, timefrm := "Weekly", ]

whole <- rbind(whole_table_yr, whole_table_mnth, whole_table_wk)
whole <- whole [, `:=`(date02 = mdy(Date),
                       High = as.numeric( str_remove_all(High, ",") ), 
                       Low = as.numeric( str_remove_all(Low, ",") ) ,
                       Price = as.numeric( str_remove_all(Price, ",") ) ,
                       Open = as.numeric( str_remove_all(Open, ",") ) ),  ]

whole <- whole [ order(timefrm, date02) ]
whole <- whole [, `:=`(nrow =1:.N, 
                       tot =.N,
                       high_t = max( High ),
                       low_t = min ( Low ) ) , by = .(timefrm)]

############################################
# End of program
############################################

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

cntrt02 <- cntrt02 [, step001 := paste("pbr <- getSymbols('", SYMBOL02, "', ", 'src = "yahoo", from =  anydate(tday) - 365 * 2, to =  anydate(tday), auto.assign = TRUE) \n', sep=""), ]
cntrt02 <- cntrt02 [, step002 := paste( SYMBOL02, " <- as.data.table(", SYMBOL02, ") \n", sep = ""), ]
cntrt02 <- cntrt02 [, step003 := paste( SYMBOL02, " <- ", SYMBOL02, "[, nrow := .I, ] \n", sep = ""), ]
cntrt02 <- cntrt02 [, step004 := paste( SYMBOL02, " <- melt(", SYMBOL02, ", id.vars = c('index', 'nrow')) \n", sep = ""), ]


fwrite(cntrt02 [, c("step001", "step002", "step003", "step004"), ],
       "D:\\My-Shares\\prgm\\yahoooutput.R",
       quote = FALSE,
       sep = " ",
       col.names = FALSE,
       row.names = FALSE) 

source("D:\\My-Shares\\prgm\\yahoooutput.R")

all <- rbindlist(mget( ls(pattern="*NS*") ), fill = TRUE )

rm ( list = ls (pattern = "*NS*"))

all <- all [, c("symbol", "exchange", "cat") := tstrsplit(variable, "\\."), ]
all <- all [, cat := tolower(cat),]
all <- all [, index := anydate(index),]

all0 <- unique( na.omit( all) )

all02 <- dcast(data = all0,
               symbol + index + nrow ~ cat,
               value.var = c("value"), 
               fill ="")

all02 <- all02 [ order(symbol, index)]
all02 <- all02 [, allrow := .I, ]

#
# Calculate SMA, RSI_White, Money Flow index, VWAP,
# ema_rsi, 3_green, wma_rsi21_red
# Bollinger Bands
#
# Create flags for consecutive high and low days
# Sign and Streak
# Signal
#
# Calculate the Bollinger band, the calculations are done over all the dataset
# So the first n lines of the bollinger band calculations go wrong
# as the data from previous company is carried forward
#
# Simliar calculations are done for the ADX parameter
# Remove the calculations for first 14 * 2 rows

bb_n <- 20
adx_n <- 14

bb_dn = as.data.table( bbands(all02$close, n = bb_n, k = 2) )
bb_dn <- bb_dn [, allrow := .I, ]

dmi.adx <- as.data.table( ADX(all02[, c("high","low","close") ], n = adx_n) )
dmi.adx <- dmi.adx [, allrow := .I, ]

sar <- as.data.table( SAR(all02[, c("high","low") ] ) )
sar <- sar [, allrow := .I, ]

atr <- as.data.table( ATR(all02[, c("high","low","close") ], n = adx_n) )
atr <- atr [, allrow := .I, ]


all02 <- all02 [, `:=` (ema5 = EMA(adjusted, 5), ema13 = EMA(adjusted, 13), ema21 = EMA(adjusted, 21), 
                        ema20 = EMA(adjusted, 20), ema50 = EMA(adjusted, 50), ema200 = EMA(adjusted, 200),
                        sma50 = SMA(adjusted, 50),
                        rsi14 = RSI(adjusted, 14),
                        rsi9_white = RSI(adjusted, 9),
                        mfi = MFI(close, volume, 9),
                        vwap10 = VWAP(adjusted, volume, n= 10),
                        dayperc = ( (close - open) / open) * 100, # Percentage change in a day
                        Sign_prc = ifelse(close>lag(close),"up", "down") ), by = .(symbol)]

# Fibonnaci series retracement code
# https://gist.github.com/drewgriffith15/e34560476a022612aa60

all02 <- all02 [, `:=` (ema_rsi3_green = EMA(rsi9_white, n = 3),
                        wma_rsi21_red = WMA(rsi9_white, n= 21), 
                        Streak_prc = sequence(rle(Sign_prc)$lengths )  ), 
                by = .(symbol)]

all02 <- all02 [, Signal_prc := case_when(lag(Sign_prc)=="up" & lag(Streak_prc)%%4==0~'short',
                                          lag(Sign_prc)=="down" & lag(Streak_prc)%%4==0~'long',
                                          TRUE~""), by = .(symbol)]

all02 <- Reduce(function(...) merge(..., by = c("allrow"), all=T),  
                list( all02, bb_dn, dmi.adx, sar, atr) )


# Create a category variable to identify the % change 
all02 <- all02 [, rsi14_cat := case_when( maxchg <= 0 ~ "01 Less then 0",
                                          maxchg > 0 & maxchg <= 1 ~ "02 between 0% and 1%",
                                          maxchg > 1 & maxchg <= 3 ~ "03 between 1% and 2%",
                                          maxchg > 3 & maxchg <= 5 ~ "04 between 3% and 5%", 
                                          maxchg > 5 ~ "05 > 5%"), ]

all02 <- all02 [, close_dwn := ifelse( ema13 < ema21 & adjusted < ema13 & adjusted < ema21, 1, 0), ]
