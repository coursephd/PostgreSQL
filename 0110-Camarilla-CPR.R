##########################################################################################
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
#
##########################################################################################

library(data.table)
library(tidyverse)
library(lubridate)
library(anytime)
library(rvest)
library(xml2)
library(ggplot2)
library(plotly)

library(TTR)
library(tidyquant)
library(QuantTools)
library(derivmkts)
library(quantmod)

options(scipen = 999)

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

# Get the yearly, monthly, weekly, daily values:
styrdate <- as.numeric(as.POSIXct(prvyr01, format="%Y-%m-%d"))
enyrdate <- as.numeric(as.POSIXct(prvyr02, format="%Y-%m-%d"))

stmnthdate <- as.numeric(as.POSIXct(prvmnth, format="%Y-%m-%d"))
enmthdate <- as.numeric(as.POSIXct(prvmnth02, format="%Y-%m-%d"))

stwkdate <- as.numeric(as.POSIXct(prvwk, format="%Y-%m-%d"))
enwkdate <- as.numeric(as.POSIXct(prvwk02, format="%Y-%m-%d"))

stdaydate <- as.numeric(as.POSIXct(dt-7, format="%Y-%m-%d"))
endaydate <- as.numeric(as.POSIXct(dt, format="%Y-%m-%d"))


url_yr <- paste("https://in.investing.com/indices/bank-nifty-futures-historical-data?end_date=", enyrdate,  
                "&st_date=", styrdate, "&interval_sec=monthly&interval_sec=daily", sep="")

url_mnth <- paste("https://in.investing.com/indices/bank-nifty-futures-historical-data?end_date=", enmthdate,
                "&st_date=", stmnthdate, "&interval_sec=monthly&interval_sec=daily", sep="")

url_wk <- paste("https://in.investing.com/indices/bank-nifty-futures-historical-data?end_date=", enwkdate,  
                "&st_date=", stwkdate, "&interval_sec=monthly&interval_sec=daily", sep="")

url_day <- paste("https://in.investing.com/indices/bank-nifty-futures-historical-data?end_date=", endaydate,  
                "&st_date=", stdaydate, "&interval_sec=monthly&interval_sec=daily", sep="")


url_html_yr <- read_html(url_yr)
url_html_mnth <- read_html(url_mnth)
url_html_wk <- read_html(url_wk)
url_html_day <- read_html(url_day)

# extract the HTML table for year
whole_table_yr <- url_html_yr %>% 
  html_nodes('table') %>%
  html_table(fill = TRUE) %>%
  .[[2]]

# extract the HTML table for month
whole_table_mnth <- url_html_mnth %>% 
  html_nodes('table') %>%
  html_table(fill = TRUE) %>%
  .[[2]]

# extract the HTML table for week
whole_table_wk <- url_html_wk %>% 
  html_nodes('table') %>%
  html_table(fill = TRUE) %>%
  .[[2]]

# extract the HTML table for day
whole_table_day <- url_html_day %>% 
  html_nodes('table') %>%
  html_table(fill = TRUE) %>%
  .[[2]]

#############################################################################
#
# For each of the interval calculate the CPR and camarilla equations
# combine the data into one data
# Price column= closing value
#
# Calculate the closing value for each of the groups
# Use "last" function from data.table to get this value
#
#############################################################################

whole_table_yr <- as.data.table(whole_table_yr)
whole_table_mnth <- as.data.table(whole_table_mnth)
whole_table_wk <- as.data.table(whole_table_wk)
whole_table_day <- as.data.table(whole_table_day)

whole_table_yr <- whole_table_yr [, timefrm := "Yearly", ]
whole_table_mnth <- whole_table_mnth [, timefrm := "Monthly", ]
whole_table_wk <- whole_table_wk [, timefrm := "Weekly", ]
whole_table_day <- whole_table_day [, timefrm := paste("Daily", .I, sep=""), ]

whole <- rbind(whole_table_yr, whole_table_mnth, whole_table_wk, whole_table_day)
whole <- whole [, `:=`(date02 = mdy(Date),
                       High = as.numeric( str_remove_all(High, ",") ), 
                       Low = as.numeric( str_remove_all(Low, ",") ) ,
                       Price = as.numeric( str_remove_all(Price, ",") ) ,
                       Open = as.numeric( str_remove_all(Open, ",") ) ),  ]

whole <- whole [, close := as.numeric( last(Price) ), by = .(timefrm)]

whole <- whole [ order(timefrm, date02) ]
whole <- whole [, `:=`(nrow =1:.N, 
                       tot =.N,
                       high_t = max( High ),
                       low_t = min ( Low ) ) , by = .(timefrm)]

##########################################################################################
#
# Calculate the CPR related parameters:
# (1) Create pivot point which is the average of PP = (high_t + Low_t + close) / 3
# (2) Bottom central level: BC = (high_t + low_t) / 2
# (3) Top central level: TC = PP - BC + PP
#
# (4) CPR width: cprwidth = (ABS(TC - BC)/ PP )*100
# Below is the interpretation of the reading.
#
# CPR Width > 0.5 - Sideways or Trading Range Day,
# CPR Width > 0.75 - increases the likelihood of sideways trading behavior,
# CPR Width < 0.5 - Trending type of day,
# CPR Width < 0.25 - increases the likelihood of a trending market.
#
##########################################################################################

whole <- whole [, `:=`(pp = (high_t + low_t + close) / 3,
                       bc = (high_t + low_t) / 2 ), by = .(timefrm)]
whole <- whole [, tc := pp - bc + pp, by = .(timefrm)]
whole <- whole [, cprwidth := (abs(tc - bc)/ pp )*100, by = .(timefrm)]


#############################################################################
#
# Calculate Camarilla S1, S2, S3, S4, S5, S6 and
#                     R1, R2, R3, R4, R5, R6 values for each time frame
#
# S1 = close - (high_t - low_t) * 1.1/12
# S2 = close - (high_t - low_t) * 1.1/6
# S3 = close - (high_t - low_t) * 1.1/4
# S4 = close - (high_t - low_t) * 1.1/2
# S5 = S4 - (S3 - S4) * 1.168
# S6 = close - (R6 - close)
#
# R1 = close + (high_t - low_t) * 1.1/12
# R2 = close + (high_t - low_t) * 1.1/6
# R3 = close + (high_t - low_t) * 1.1/4
# R4 = close + (high_t - low_t) * 1.1/2
# R5 = R4 + (R4 - R3) * 1.168
# R6 = (high_t/low_t) * close
#
#############################################################################

whole <- whole [, `:=` (s1 = close - (high_t - low_t) * 1.1/12,
                        s2 = close - (high_t - low_t) * 1.1/6, 
                        s3 = close - (high_t - low_t) * 1.1/4, 
                        s4 = close - (high_t - low_t) * 1.1/2,
                        r1 = close + (high_t - low_t) * 1.1/12,
                        r2 = close + (high_t - low_t) * 1.1/6, 
                        r3 = close + (high_t - low_t) * 1.1/4, 
                        r4 = close + (high_t - low_t) * 1.1/2),  by = .(timefrm)]

whole <- whole [, `:=`(s5 = s4 - (s3 - s4) * 1.168,
                       r5 = r4 + (r4 - r3) * 1.168,
                       r6 = (high_t/low_t) * close), by =.(timefrm)]

whole <- whole [, s6 := close - (r6 - close), by =.(timefrm)]


#############################################################################
#
# Create a variable to identifty:
# (1) Daily group
# (2) Last month name
# (3) Last year value
# Use this to create unique records
#
#############################################################################
whole <- whole [, timefrm02 := case_when( timefrm == "Monthly" ~ format(as.Date(date02), "%Y-%m"),
                                          timefrm == "Yearly" ~  format(as.Date(date02), "%Y"),
                                          timefrm == "Weekly" ~  format(as.Date(date02), "%Y-%W"),
                                          TRUE ~ as.character(date02) ), ]

whole02 <- unique( whole [, c("timefrm", "timefrm02", 
                              "s1", "s2", "s3", "s4", "s5", "s6",
                              "r1", "r2", "r3", "r4", "r5", "r6",
                              "pp", "tc", "bc", "cprwidth"), ])

#############################################################################
#
# Create a varable for plotting the graph
# x and xend variables will be useful in expanding the column widths and 
# would allow to understand the possible relationships
#
# Create a group variable for the coloring of 
# (1) the CPR and 
# (2) r1 to r6
# (3) s1 to s6
#
#############################################################################

whole02 <- whole02 [, `:=`(x = .I, xend = .I + 1), ]

whole02_t <- melt(data = whole02,
                  id.vars =c("timefrm", "timefrm02", "x", "xend") )

whole02_t <- whole02_t [, grpclr := case_when( variable %in% c("tc", "bc") ~ "pink",
                                               variable %in% c("pp") ~ "blue",
                                               variable %in% c("s3", "r3") ~ "green",
                                               variable %in% c("s4", "r4") ~ "red"), ]

xmin <- min(summary(whole02_t[! timefrm %in% c("Yearly", "Weekly", "Monthly") & value > 100]$value))
xmax <- max(summary(whole02_t[! timefrm %in% c("Yearly", "Weekly", "Monthly") & value > 100]$value))

ggplot(whole02_t [! timefrm %in% c("Yearly", "Weekly", "Monthly") & value > 100], aes(x= timefrm02, y= value) ) +
  geom_segment( aes (x = x, xend = xend, y = value, yend = value, color = grpclr) ) +
  geom_label ( aes (x = x, label = variable) ) +
  scale_colour_identity() +
  scale_y_continuous( c(xmin, xmax, 100) )

#########################################################################
#
# Json file:
# (1) Go to the chart
# (2) Go to Right click and inspect
# (3) Go to Network and refresh the page
# (4) Go to XHR area: and the last Name would appear
# (5) Go to the 15Min? ..., right click Copy as cURL (bash)
# (6) This line would generate the json file as listed below
# 
# (7) Run the following code to get the XML file into R dataset
# 
######################################################################### 

library("jsonlite")

curl 'https://kite.zerodha.com/oms/instruments/historical/12680706/15minute?user_id=QI9532&oi=1&from=2020-12-03&to=2021-01-02&ciqrandom=1609604737880' \
-H 'authority: kite.zerodha.com' \
-H 'sec-ch-ua: "Google Chrome";v="87", " Not;A Brand";v="99", "Chromium";v="87"' \
-H 'user-agent: Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/87.0.4280.88 Safari/537.36' \
-H 'sec-ch-ua-mobile: ?0' \
-H 'authorization: enctoken HcU5EXua5dtUnHZHwXcYIeXwpvZh774mrvfcGql+e5Ei6Ra4EDL3y0Ka4mVNlZSt1CEhYK9lny6FwUk+7bgY2wWUIo8KzQ==' \
-H 'accept: */*' \
-H 'sec-fetch-site: same-origin' \
-H 'sec-fetch-mode: cors' \
-H 'sec-fetch-dest: empty' \
-H 'referer: https://kite.zerodha.com/static/build/chart.html?v=2.6.3' \
-H 'accept-language: en-US,en;q=0.9' \
-H 'cookie: _ga=GA1.2.1000558376.1593278039; kf_session=4pNa34VF3AjpPhtduIdwGqPCua26xZzv; user_id=QI9532; __cfduid=de78e50dea9baf3e6e3276db9e6a1ae401609603927; public_token=b7erh62oV7NL7PQlI3YTdE7iu5qOsti0; enctoken=HcU5EXua5dtUnHZHwXcYIeXwpvZh774mrvfcGql+e5Ei6Ra4EDL3y0Ka4mVNlZSt1CEhYK9lny6FwUk+7bgY2wWUIo8KzQ==' \
--compressed > banknifty.json

dd01_1 <- fromJSON ("D:\\My-Shares\\prgm/banknifty.json")
dd01_2 <- data.table(dd01_1$data$candles)

p1 <- ggplot(whole02_t [! timefrm %in% c("Yearly", "Weekly", "Monthly") & value > 100], aes(x= timefrm02, y= value) ) +
  geom_segment( aes (x = x, xend = xend, y = value, yend = value, color = grpclr) ) +
  geom_label ( aes (x = x, label = variable) )

p2 <- ggplotly(p1)
p2

fig <- whole %>% plot_ly(x = ~date02, type="ohlc",
                      open = ~Open, close = ~Price,
                      high = ~High, low = ~Low) 
fig <- fig %>% layout(title = "Basic OHLC Chart")

fig



# Plotting of values
# https://timelyportfolio.github.io/rCharts_time_series/history.html

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
