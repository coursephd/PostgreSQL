
library(data.table)
library(tidyverse)
library(lubridate)
library(anytime)
library(bizdays)
library(rvest)
library(xml2)

library(ggplot2)
library(ggpubr)
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


# Get the yearly, monthly, weekly, daily values:
styrdate <- as.numeric(as.POSIXct(prvyr01, format="%Y-%m-%d"))
endaydate <- as.numeric(as.POSIXct(dt+1, format="%Y-%m-%d")) # Vinay update 9th Jan 2021

url_all <- paste("https://in.investing.com/indices/bank-nifty-futures-historical-data?end_date=", endaydate,  
                 "&st_date=", styrdate, "&interval_sec=monthly&interval_sec=daily", sep="")

url_html_all <- read_html(url_all)

# extract the HTML table for year
whole_table_all <- url_html_all %>% 
  html_nodes('table') %>%
  html_table(fill = TRUE) %>%
  .[[2]]

whole <- as.data.table(whole_table_all)
whole <- whole [, `:=`(date02 = mdy(Date),
                       High = as.numeric( str_remove_all(High, ",") ), 
                       Low = as.numeric( str_remove_all(Low, ",") ) ,
                       Price = as.numeric( str_remove_all(Price, ",") ) ,
                       Open = as.numeric( str_remove_all(Open, ",") ) ),  ]


whole <- whole [ order (date02) ]
whole <- whole [, `:=`(prvyr = as.numeric(format(as.Date( floor_date(date02, "year") - years(1) ), "%Y")), 
                       prvyr01 = as.Date(paste(prvyr, "01", "01", sep="-")), 
                       prvmnth = floor_date(date02, "month") - months(1), 
                       prvwk = floor_date(date02, "week") - weeks(1),
                       prvday = shift(date02, type ="lag"),
                       
                       curyr = as.numeric(format(as.Date( floor_date(date02, "year") ), "%Y")), 
                       curmnth = floor_date(date02, "month"), 
                       curwk = floor_date(date02, "week"), 
                       curday = date02), ]

# do the same for current date

curdata <- melt(data = whole, 
                id.vars = c("date02", "Price", "Open", "High", "Low", "Volume", "Chg%"),
                measure.vars = c("curyr", "curmnth", "curwk", "curday"))

curdata <- curdata [, value02 := ifelse(! variable == "curyr", as.character( anydate(value) ), value), ]

# Create high, low, close values for each week, month, year

curdata <- curdata [, close := as.numeric( last(Price) ), by = .(variable, value02) ]
curdata <- curdata [, `:=`(high_t = max( High ),
                           low_t = min ( Low ) ) , by = .(variable, value02)]


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

curdata <- curdata [, `:=`(pp = (high_t + low_t + close) / 3,
                           bc = (high_t + low_t) / 2 ), by = .(variable, value02)]
curdata <- curdata [, tc := pp - bc + pp, by = .(variable, value02)]
curdata <- curdata [, cprwidth := (abs(tc - bc)/ pp )*100, by = .(variable, value02)]


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

curdata <- curdata [, `:=` (s1 = close - (high_t - low_t) * 1.1/12,
                            s2 = close - (high_t - low_t) * 1.1/6, 
                            s3 = close - (high_t - low_t) * 1.1/4, 
                            s4 = close - (high_t - low_t) * 1.1/2,
                            r1 = close + (high_t - low_t) * 1.1/12,
                            r2 = close + (high_t - low_t) * 1.1/6, 
                            r3 = close + (high_t - low_t) * 1.1/4, 
                            r4 = close + (high_t - low_t) * 1.1/2),  by = .(variable, value02)]

curdata <- curdata [, `:=`(s5 = s4 - (s3 - s4) * 1.168,
                           r5 = r4 + (r4 - r3) * 1.168,
                           r6 = (high_t/low_t) * close), by =.(variable, value02)]

curdata <- curdata [, s6 := close - (r6 - close), by =.(variable, value02)]
curdata <- curdata [, r3s3width := r3 - s3, by =.(variable, value02)]

# Create unique records for the week, month and year

unqcurdata <- unique ( curdata [variable %in% c("curyr", "curmnth", "curwk"), 
                                c("variable", "value", "value02", "s1", "s2", "s3", "s4", "s5", "s6", 
                                  "r1", "r2", "r3", "r4", "r5", "r6", 
                                  "pp", "bc", "tc", "cprwidth", "r3s3width"), ] )

#####################################################################
#
# Calculate the unique day values and stats as follows:
# 5, 20, 50 SMA for price
# 14 day SMA for cprwidth, r3s3width
# From NKstocktalk:  SRT: if possible: 
# SRT = close / sma(close, 124): 
# Interpretation: > 1.35 Exit, 0.85 - 1.35 Hold, 0.55 - 0.75 Invest
# 20 SMA Volume
#
#####################################################################
unqcurdata_day <- unique ( curdata [variable %in% c("curday") ] )

unqcurdata_day <- unqcurdata_day [, `:=` (ema_cls5 = EMA(close, 5), ema_cls20 = EMA(close, 20), ema_cls50 = EMA(close, 50), 
                                          sma_cls124 = SMA(close, 124),
                                          rsi_cls9 = RSI(close, n = 9),
                                          ema_cprwdt5 = EMA(cprwidth, 5), ema_cprwdt20 = EMA(cprwidth, 20), ema_cprwdt50 = EMA(cprwidth, 50),
                                          ema_r3s3wdt5 = EMA(r3s3width, 5), ema_r3s3wdt20 = EMA(r3s3width, 20), ema_r3s3wdt50 = EMA(r3s3width, 50), 
                                          
                                          # Currently this is a duplication, need to see how to stop this duplication
                                          curyr = as.numeric(format(as.Date( floor_date(date02, "year") ), "%Y")), 
                                          curmnth = floor_date(date02, "month"), 
                                          curwk = floor_date(date02, "week"), 
                                          curday = date02), ]

unqcurdata_day <- unqcurdata_day [, srt := close / sma_cls124,]

#####################################################################
#
# Create new variable to get the next date
# Use the bizdays library to get the necesary function for the next
# business day
#
#####################################################################
cal <- create.calendar("India", weekdays=c("saturday", "sunday"))

unqcurdata_day <- unqcurdata_day [, `:=`(nrow = 1:.N, tot = .N), ]
unqcurdata_day <- unqcurdata_day [, dt0 := case_when(nrow < tot ~ anydate( shift(date02, type = "lead") ),
                                                     nrow == tot ~ anydate( add.bizdays(date02, 1, cal)) ),]


# Use the prvdata and transpose the data


unqcurdata_t <- melt(data = unqcurdata, 
                     id.vars =c("variable", "value", "value02"), 
                     variable.name = "stat",
                     value.name = "result")

# Concatenate the time frame to the stat value
unqcurdata_t <- unqcurdata_t [, stat := paste(variable, "_", stat, sep=""), ]

# Transpose the data to horizontal format for 3 different time frames
unqcurdata_t01yr <- dcast(data = unqcurdata_t [ variable == "curyr"], 
                          value + value02 ~ stat,
                          value.var = c("result") )

unqcurdata_t01mnth <- dcast(data = unqcurdata_t [ variable == "curmnth"], 
                            value + value02 ~ stat,
                            value.var = c("result") )

unqcurdata_t01wk <- dcast(data = unqcurdata_t [ variable == "curwk"], 
                          value + value02 ~ stat,
                          value.var = c("result") )

# Merge these datasets into 1 dataset with day values
# Need 3 different merges based on 3 time frame values
#
# As these data are applicable to the next week, month and year
# create another date variable which is advanced to the next week, month and year

unqcurdata_t01yr <- unqcurdata_t01yr [, nxtyr := as.numeric(value) + 1,]
unqcurdata_t01mnth <- unqcurdata_t01mnth [, nxtmnth := anydate(value02) + months(1),]
unqcurdata_t01wk <- unqcurdata_t01wk [, nxtwk := anydate(value02) + weeks(1), ]

############################
#
# First merge for the year
#
############################
unqcurdata_day01 <- merge (x = unqcurdata_day, 
                           y = unqcurdata_t01yr [, -c("value", "value02")], 
                           by.x = c("curyr"),
                           by.y = c("nxtyr"), 
                           all = TRUE)


############################
#
# Second merge for the month
#
############################
unqcurdata_day02 <- merge (x = unqcurdata_day01, 
                           y = unqcurdata_t01mnth [, -c("value", "value02")], 
                           by.x = c("curmnth"),
                           by.y = c("nxtmnth"), 
                           all = TRUE)

############################
#
# Third merge for the week
#
############################
unqcurdata_day03 <- merge (x = unqcurdata_day02, 
                           y = unqcurdata_t01wk [, -c("value", "value02")], 
                           by.x = c("curwk"),
                           by.y = c("nxtwk"), 
                           all = TRUE)

unqcurdata_day03 <- unqcurdata_day03 [, value03 := anydate(value02), ]
############################
#
# Fourth merge for the day
#
# Remove the calculations which are done on the same day
# Merge the same calculations as applicable for the next day
# Hence remove the default values - merge the same values which are moved
# ahead by 1 day
#
# Remove variables from: unqcurdata_day03
# [17] "pp"                "bc"               
# [19] "tc"                "cprwidth"         
# [21] "s1"                "s2"               
# [23] "s3"                "s4"               
# [25] "r1"                "r2"               
# [27] "r3"                "r4"               
# [29] "s5"                "r5"               
# [31] "r6"                "s6"               
# [33] "r3s3width"         "ema_cls5"         
# [35] "ema_cls20"         "ema_cls50"        
# [37] "sma_cls124"        "rsi_cls9"         
# [39] "ema_cprwdt5"       "ema_cprwdt20"     
# [41] "ema_cprwdt50"      "ema_r3s3wdt5"     
# [43] "ema_r3s3wdt20"     "ema_r3s3wdt50"    
# [45] "curday"            "srt"              
# [47] "nrow"              "tot"        
#
# Remove variables from: unqcurdata_day
# [1] "date02"        "Price"        
# [3] "Open"          "High"         
# [5] "Low"           "Volume"       
# [7] "Chg%"          "variable"     
# [9] "value"         "value02"      
# [11] "close"         "high_t"       
# [13] "low_t"         
############################
unqcurdata_day04 <- merge (x = unqcurdata_day03 [, -c(17:48), ], 
                           # ??????????????????????
                           # Need to update for the day data
                           ########################
                           y = unqcurdata_day [, -c(1:13, 42:45), ], 
                           by.x = c("value03"),
                           by.y = c("dt0"), 
                           all = TRUE)

# Create a transpose of the data

unqcurdata_day04_t <- melt(data = unqcurdata_day04, 
                           id.vars = c(1:18, 99:100), 
                           variable.name = "category",
                           value.name = "result")

# Create a scale for the excel printing
# The scale will go from 30000 to 300050 - in a form 50 point gap

scale01 <- unqcurdata_day04_t [! category %in% 
                                  c("curmnth_cprwidth", "curmnth_r3s3width", "curwk_cprwidth", "curwk_r3s3width", "cprwidth", 
                                    "curyr_cprwidth", "curyr_r3s3width", 
                                    "r3s3width", "ema_cls5", "ema_cls20", "ema_cls50", "sma_cls124",  
                                    "rsi_cls9", "ema_cprwdt5", "ema_cprwdt20", "ema_cprwdt50", "ema_r3s3wdt5", "ema_r3s3wdt20",
                                    "ema_r3s3wdt50", "srt")]

scale01 <- scale01 [, x:=1, ]
#############################################################
#
# Keep only values beyond 5000 as it is difficult to imagine 
# Banknifty going so much
##############################################################

min <- min(scale01$result > 0, na.rm = TRUE)
max <- max(scale01$result, na.rm = TRUE)

minmax <- seq(from = min, to = max, by = 50)
minmax <- data.table(minmax)
minmax02 <- minmax [minmax > 5000]
minmax02 <- minmax02 [, `:=` (start0 = minmax, end0 = minmax + 49, x = 1), ]

# https://stackoverflow.com/questions/46339638/join-tables-based-on-multiple-ranges-in-r
scale02 <- scale01 %>%
  inner_join(minmax02 [, -c("minmax"), ]) %>%
  filter( result >= start0 & result <= end0)

scale03 <- as.data.table( na.omit(scale02) )

# Create 1 record per category per date by collapsing the different values

scale04 <- scale03 [, .(result_cat = paste(category, collapse =",", sep=" ") ) , by = .(dt0, start0, end0)]


# Transpose the dataset with 1 column per date
scale04_t <- dcast(data = scale04 [ dt0 >= dt - 45], 
                   start0 + end0 ~ dt0, 
                   value.var =c("result_cat"), 
                   fill =" ")

# Merge the data with scale02 data to get all the categories
scale04_t <- merge (x = scale04_t, 
                    y = minmax02,
                    by = c("start0", "end0"), 
                    all = TRUE)

#######################################################################################
