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

options(scipen = 999)

# from today to 2 years behind
tday <- Sys.Date()
#tday <- Sys.Date() #c("06JUL2020") #

########################################################################################
# Section 1
#
# Calculate the reference dates needed for the contracts and previous duration
#
# Contract dates from the NSE website: -- Do not change wkcday for, we will see how to
# make it dynamic
########################################################################################

wkcday <- c("09JUL2020")

contract <- as.data.table ( list ( date = anydate( seq(from = anydate(wkcday), to = anydate(wkcday) + 90, by = "day" ) ) ))
contract <- contract [, `:=`(wkday = weekdays(date),
                             mon = toupper(format(date,"%b")),
                             mon_num = as.numeric( format(date,"%m") ),
                             yr = toupper(format(date,"%Y")),
                             caldate = anydate(tday)),]

#contract <- contract [, time := .I,]
#contract <- contract [, ndycnt := 1:.N, by = .(yr, mon, wkday)]

# Get the last Thursday of the month to construct the monthly contract
cal02 <- contract [ wkday == "Thursday", .( monthly = max(date) ), by =.(mon, mon_num, yr)]

contract <- merge (x = contract,
                   y = cal02,
                   by = c("mon", "yr", "mon_num"),
                   all = TRUE)

# If the monthly contract date and the date of transaction are within 8 to 12 days of 
# each other then move to the next monthly contract for NIFTY
# Merge the monthly contract information once again:

contract <- contract [, diff := as.numeric(monthly - caldate + 1),]
contract <- contract [, mon_num := ifelse( diff <= 12, mon_num + 1, mon_num),]

contract <- merge (x = contract [, -c("monthly", "mon")],
                   y = cal02 ,
                   by = c("mon_num", "yr"),
                   all = TRUE)

# Create duration for the Weekly contracts
thursday <- contract [wkday == "Thursday", c("date", "wkday"),]
thursday <- thursday [, weekdt := date,]
thursday <- thursday [, weekdt6 := date - 6,]
thursday <- thursday [, list (weekly = date, date = anydate( seq(from = weekdt6, to = weekdt, by = "day") ) ), by = 1:nrow(thursday)]

contract <- merge (x = contract,
                   y = thursday,
                   by = c("date"),
                   all = TRUE)

# Subset the dates for caldate >= wkday
contract02 <- contract [ date >= caldate]
contract02 <- contract02 [ order(date)]
contract02 <- contract02 [, nrow :=.I,]

# Get the previous day for the caldate
# If the previous day is a Saturday or Sunday then need to go behind
# 1 or 2 days, if the previous day is a holiday for the NSE / BSE then 
# that has to be taken into account
#
# In order to avoid that confusion, go back at least 5 days from caldate
# Download the bhavcopy for at least 5 days, pick the maximum date from 
# bhavcopy for the previous day's pricing

contract03 <- contract02 [ nrow == 1, c("caldate", "wkday", "monthly", "weekly", "nrow"), ]
contract03 <- contract03 [, prvdate := caldate - 5, ]

# Transpose the data to then merge with the Options data
# This will be done based on Contract dates for NIFTY and BANK NIFTY

contract04 <- melt(data = contract03, 
                   id = c("nrow", "caldate"),
                   measure.vars = c("monthly", "weekly"),
                   value.name = "nexpday")
contract04 <- contract04 [, SYMBOL := ifelse(variable == "monthly", "NIFTY", "BANKNIFTY"),]

########################################################################################
# Section 2
#
# For some reason this is the syntax from this quntmod package:
# Get the NIFTY and NIFTY BANK data for last 1 year
#
# Calculate the summary statistics for both the indices and Correlation coefficient
# The mean and SD will be used to create simulated data (as needed)
#
########################################################################################

pbr <- getSymbols("^NSEI", src = "yahoo", from =  anydate(tday) - 365, to =  anydate(tday), auto.assign = TRUE)
pbr <- getSymbols("^NSEBANK", src = "yahoo", from =  anydate(tday) - 365, to =  anydate(tday), auto.assign = TRUE)
rm(pbr)

NSEI02 <- as.data.table (NSEI)
NSEI02 <- NSEI02 [, symbol := "NIFTY50"]
NSEI02 <- melt(NSEI02, id.vars = c("index", "symbol"))

NSEBANK02 <- as.data.table (NSEBANK)
NSEBANK02 <- NSEBANK02 [, symbol := "NIFTYBANK"]
NSEBANK02 <- melt(NSEBANK02, id.vars = c("index", "symbol"))

data01day <- rbind (NSEI02, NSEBANK02)
data01day <- na.omit(data01day)
data01day <- data01day [ order(index, symbol)]
data01day <- data01day [, prv := shift(value, type = c("lag") ), by = .(symbol, variable)]

data01stats <- data01day [, .(n=uniqueN(index), 
                          mean = round( mean(value, na.rm = TRUE), digits =1),
                          median= round( median(value, na.rm = TRUE), digits =2),
                          SD = round( sd(value, na.rm = TRUE), digits =2),
                          min = round( min(value, na.rm = TRUE), digits =1),
                          max = round( max(value, na.rm = TRUE), digits =1)), 
                      by = .(symbol, variable)]
data01stats <- data01stats [, cvperc := (SD/mean)*100,]

data02day <- dcast(data = data01day,
                   index ~ variable, 
                   value.var = c("value") )
data02day <- na.omit(data02day)
data02day <- data02day [, `:=`(numday = uniqueN(index),
                               return50 = log(NSEI.Close / shift(NSEI.Close, type = c("lag") ) ) * 100,
                               returnbnk = log(NSEBANK.Close / shift(NSEBANK.Open, type = c("lag") ) ) * 100 ),]
data02day <- data02day [, `:=`(dayvol50 = round( sd(return50, na.rm = TRUE), digits =2),
                               dayvolbnk = round( sd(returnbnk, na.rm = TRUE), digits =2),
                               daymean50 = round( mean(return50, na.rm = TRUE), digits =2),
                               daymeanbnk = round( mean(returnbnk, na.rm = TRUE), digits =2)), ]
data02day <- data02day [, `:=`(annvol50 = dayvol50 * sqrt(numday), 
                               annvolbnk = dayvolbnk * sqrt(numday),
                               annmean50 = daymean50 * sqrt(numday),
                               annmeanbnk = daymeanbnk * sqrt(numday)), ]

# Find the correlation between the NIFTY and BANK NIFTY data:
cor <- cor(data02day$NSEI.Adjusted, data02day$NSEBANK.Adjusted)

data03day <- unique (data02day [index == max(index), c("dayvol50", "dayvolbnk", "daymean50", "daymeanbnk", "index"), ] )

########################################################################################
# Section 3
#
# Use NSE data:
# Get the bhavcopy data for at least 5 days and pick up the latest value for 
# options premiums - in turn calculate the Implied volatility
#
# All the code for the extraction from NSE website
# It takes time to extract so re-thinking about this part
# But is needed for the Last day's Contract values for the IV calculation
#
########################################################################################

dates <- seq ( anydate(tday), anydate(tday) - 180, by=-1)

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

bhavcopy <- vector('list', n)
outprice <- vector('list', n)

for (i in 1:n) { 
  
  url <- paste("https://www1.nseindia.com/content/historical/DERIVATIVES/", monyr[i], "/", date_char[i], ".zip", sep="")
  req <- curl_fetch_memory(url)
  print(i)
  
  if (req$status_code == 200) {
    temp <- tempfile()
    
    download.file( url, temp)
    bhavcopy[[i]] <- as.data.table( fread(unzip(temp, files = date_char[i])) )
    rm(temp)
  }
  
  price <- paste("https://www1.nseindia.com/archives/nsccl/volt/FOVOLT_", date_num[i], ".csv", sep="")
  req02 <- curl_fetch_memory(price)
  print(i)
  if (req02$status_code == 200)
    outprice[[i]] <- as.data.table( fread( price ) )
  
}

out2 <- rbindlist(bhavcopy) [SYMBOL %in% c("NIFTY", "BANKNIFTY")]

fut <- rbindlist(bhavcopy) [INSTRUMENT %in% c("FUTIDX") & SYMBOL %in% c("NIFTY", "BANKNIFTY") & OPTION_TYP == "XX"]
opt <- rbindlist(bhavcopy) [INSTRUMENT %in% c("OPTIDX") & SYMBOL %in% c("NIFTY", "BANKNIFTY") & OPTION_TYP == "CE"]

out2price <- rbindlist(outprice) [Symbol %in% c("NIFTY", "BANKNIFTY")]

# create a numeric date variable and expiry date
fut <- fut [, `:=` (trday = anydate(TIMESTAMP), nexpday = anydate(EXPIRY_DT)), ]
opt <- opt [, `:=` (trday = anydate(TIMESTAMP), nexpday = anydate(EXPIRY_DT)), ]

rm (bhavcopy)
rm (outprice)

fut02 <- data02day [index == max(index), c("index", "NSEI.Adjusted", "NSEBANK.Adjusted"), ]

fut02 <- merge(x = data03day,
               y = fut02,
               by = c("index") )

# Create mean +/- 1.5, 2 and 3 SD from the closing of previous day

fut02 <- fut02 [, `:=`(nifty_15sdlow = round( exp( (daymean50 - 1.5 *dayvol50)/ 100) * NSEI.Adjusted, 2),
                       nifty_15sdhigh = round( exp( (daymean50 + 1.5 *dayvol50)/ 100) * NSEI.Adjusted, 2),
                       nifty_2sdlow = round( exp( (daymean50 - 2 *dayvol50)/ 100) * NSEI.Adjusted, 2),
                       nifty_2sdhigh = round( exp( (daymean50 + 2 *dayvol50)/ 100) * NSEI.Adjusted, 2),
                       nifty_3sdlow = round( exp( (daymean50 - 3 *dayvol50)/ 100) * NSEI.Adjusted, 2),
                       nifty_3sdhigh = round (exp( (daymean50 + 3 *dayvol50)/ 100) * NSEI.Adjusted, 2),
                       nsebnk_15sdlow = round( exp( (daymeanbnk - 1.5 *dayvol50)/ 100) * NSEBANK.Adjusted, 2),
                       nsebnk_15sdhigh = round( exp( (daymeanbnk + 1.5 *dayvol50)/ 100) * NSEBANK.Adjusted, 2),
                       nsebnk_2sdlow = round( exp( (daymeanbnk - 2 *dayvol50)/ 100) * NSEBANK.Adjusted, 2),
                       nsebnk_2sdhigh = round( exp( (daymeanbnk + 2 *dayvol50)/ 100) * NSEBANK.Adjusted, 2),
                       nsebnk_3sdlow = round( exp( (daymeanbnk - 3 *dayvol50)/ 100) * NSEBANK.Adjusted, 2),
                       nsebnk_3sdhigh = round (exp( (daymeanbnk + 3 *dayvol50)/ 100) * NSEBANK.Adjusted, 2), 
                       y = 1),]

#
# Create a data with the strikes above the 2SD and 3SD levels
# This will be used to pick up the strikes
# Use fut04 to merge with the OPT data

fut03 <- melt(data = fut02,
              id = c("index", "NSEI.Adjusted", "NSEBANK.Adjusted"))
fut03 <- fut03 [, STRIKE := ifelse(value < 10000, signif(value, 2), signif(value, 3) ), ]
fut03 <- fut03 [, SYMBOL := ifelse( variable %like% c("nifty"), "NIFTY", "BANKNIFTY"),]
fut04 <- fut03 [ variable %like% c("high")]
fut04 <- fut04 [, c("temp", "class") := tstrsplit(variable, "_"),]

#
# Pick up the latest IV data from the by calculating in reverse direction
# Use the contract price and strike and other known values
#
# Need to automate this
#

opt02 <- opt [trday == max(trday) ]
opt03 <- merge(x = opt02, 
               y = contract04 [, -c("variable"), ],
               by = c("SYMBOL", "nexpday"), 
               all.y = TRUE)

opt04 <- merge(x = fut04,
               y = opt03, 
               by.x = c("SYMBOL", "index"),
               by.y = c("SYMBOL", "trday"),
               allow.cartesian=TRUE)

opt05 <- opt04 [ as.numeric(STRIKE) <= as.numeric(STRIKE_PR)]
opt05 <- opt05 [, `:=`(time = as.numeric(nexpday - index -1), r = 0.05, d=0, y=1), ]
opt05 <- opt05 [, row :=.I,]
opt05 <- opt05 [, iv := round( bscallimpvol(s = ifelse(SYMBOL == "NIFTY", NSEI.Adjusted, NSEBANK.Adjusted), k = STRIKE_PR, r = r, tt = time/365, d = d, price = CLOSE), 2), by =.(row)]

opt06 <- unique( opt05 [CONTRACTS > 10, c("SYMBOL", "index", "variable", "nexpday", "iv", "time", "caldate", "y",
                                          "class", "STRIKE_PR", "CLOSE", "NSEI.Adjusted", "NSEBANK.Adjusted"),])
opt06 <- opt06 [, rank := 1:.N, by = .(SYMBOL, index, variable)]
opt06 <- opt06 [ rank <= 2]

# Transpose opt06 to merge with the dummy data

opt06_t <- dcast(data = opt06,
                 y + rank + class + caldate ~ SYMBOL,
                 value.var = c("nexpday", "iv", "STRIKE_PR"))

########################################################################################
# Section 4
#
# Createa simulated data for the NIFTY and BANK NIFTY values
# define and generate the original data set
#
# Keep the observations between the lower and upper limits
########################################################################################

def <- defData(varname = "x", dist = "normal", formula = 0, variance = 1, id = "cid")
dt <- genData(1000, def)

# add new correlate fields a0 and a1 to 'dt'
dt <- addCorData(dt, idname = "cid", 
                 mu = c( data01stats [ variable == "NSEI.Adjusted", c("mean")], 
                         data01stats [ variable == "NSEBANK.Adjusted", c("mean")]), 
                 sigma = c( data01stats [ variable == "NSEI.Adjusted", c("SD")], 
                            data01stats [ variable == "NSEBANK.Adjusted", c("SD")]), 
                 rho = cor, 
                 corstr = "cs", cnames = c("NIFTY50", "NIFTYBANK"))
dt <- dt [, y :=1,]

dt02 <- merge (x = dt,
               y = fut02,
               by = c("y"))

# Keep observations which are only between the 1.5 lower and upper SD
dt02_15 <- dt02 [ (nifty_15sdlow < NIFTY50 & NIFTY50 < nifty_15sdhigh) &
                  (nsebnk_15sdlow < NIFTYBANK & NIFTYBANK < nsebnk_15sdhigh), 
                  c("y", "NIFTY50", "NIFTYBANK", "NSEI.Adjusted", "NSEBANK.Adjusted"),]

# Keep observations which are only between the 2 lower and upper SD
dt02_2 <- dt02 [ (nifty_2sdlow < NIFTY50 & NIFTY50 < nifty_2sdhigh) &
                 (nsebnk_2sdlow < NIFTYBANK & NIFTYBANK < nsebnk_2sdhigh), 
                 c("y", "NIFTY50", "NIFTYBANK", "NSEI.Adjusted", "NSEBANK.Adjusted"),]

# Keep observations which are only between the 3 lower and upper SD
dt02_3 <- dt02 [ (nifty_2sdlow < NIFTY50 & NIFTY50 < nifty_2sdhigh) &
                 (nsebnk_2sdlow < NIFTYBANK & NIFTYBANK < nsebnk_2sdhigh), 
                 c("y", "NIFTY50", "NIFTYBANK", "NSEI.Adjusted", "NSEBANK.Adjusted"),]


########################################################################################
# Section 5
#
# Create time variable from caldate (date of transaction) to contract expiry
# Get the iv, r = 0.05 and d = 0 and calculate the premium for the combination
#
# Merge the BS formula related variables and calculate possible combinations
########################################################################################
dt03_15 <- merge (x = dt02_15,
                  y = opt06_t [ class %like% c("15s")],
                  by = c("y"),
                  allow.cartesian = TRUE)

dt03_2 <- merge (x = dt02_2,
                  y = opt06_t [ class %like% c("2s")],
                  by = c("y"),
                  allow.cartesian = TRUE)

dt03_3 <- merge (x = dt02_3,
                 y = opt06_t [ class %like% c("3s")],
                 by = c("y"),
                 allow.cartesian = TRUE)

# Combine all the datasets: 
dt04 <- rbind(dt03_15, dt03_2, dt03_3)

dt04 <- dt04 [, `:=` (t50 = as.numeric(nexpday_NIFTY - caldate + 1),
                      tbnk = as.numeric(nexpday_BANKNIFTY - caldate + 1)),]
dt04 <- dt04 [, premium50 := round( bscall(s = NIFTY50, k = STRIKE_PR_NIFTY, v = iv_NIFTY, r = 0.05, tt = t50 /365.25, d = 0), 2), ]
dt04 <- dt04 [, premiumbnk := round( bscall(s = NIFTYBANK, k = STRIKE_PR_BANKNIFTY, v = iv_BANKNIFTY, r = 0.05, tt = tbnk /365.25, d = 0), 2), ]



#shell('curl "https://www1.nseindia.com/content/historical/DERIVATIVES/2020/JUN/fo28JUN2020bhav.csv.zip" > "D:/My-Shares/prgm/fo28JUN2020bhav.csv.zip"')
#shell('curl "https://www1.nseindia.com/content/historical/DERIVATIVES/2020/JUL/fo02JUL2020bhav.csv.zip" > "D:/My-Shares/prgm/fo02JUL2020bhav.csv.zip"')
#shell('curl "https://www1.nseindia.com/content/historical/DERIVATIVES/2020/JUL/fo03JUL2020bhav.csv.zip" > "D:/My-Shares/prgm/fo03JUL2020bhav.csv.zip"')
#shell('curl "https://www1.nseindia.com/content/historical/DERIVATIVES/2020/JUL/fo04JUL2020bhav.csv.zip" > "D:/My-Shares/prgm/fo04JUL2020bhav.csv.zip"')
##############################################################################################

#fut02 <- fut [trday == max(trday) ]
#fut02 <- fut02 [nexpday == min(nexpday)]


bscall(s = 21848,
       k = 22900, #seq(from =22000, to = 23000, by =100),
       v = 0.3, #seq(from = 0.2, to =0.9, by = 0.1),
       r = 0.05,
       tt = seq(from= 6, to = 1, by = -1)/365,
       d = 0)

round( bscall(s = 10602,
       k = 11300, #seq(from =22000, to = 23000, by =100),
       v = 0.2, #seq(from = 0.2, to =0.9, by = 0.1),
       r = 0.05,
       tt = seq(from= 1, to = 30, by = 1)/365,
       d = 0), 2)

bscallimpvol(s = 11265.52,
             k = 11300, 
             r = 0.05,
             tt = 7/365, 
             d = 0,
             price = 90)


temp <- data01nifty [strike50 == 10400, premiumbnk := round( bscall(s = bnk, k = strikebnk, v = v, r = r, tt = time / 365.25, d = d), 2), ]
temp <- data01nifty [strike50 == 10400, premium50 := round( bscall(s = nifty, k = strike50, v = v, r = r, tt = time /365.25, d = d), 2), ]





data03all <- crossing (data02nifty [, c("dtcal", "nifty_dt", "nifty", "strike50", "timeday50", "premium50", "nifty_lot"), ],
                       data02bnknifty [, c("bnk", "strikebnk", "bnk_dt", "premiumbnk", "nifty_bnk"), ])

data03all <- data03all [, prmdiff := abs( premium50 - premiumbnk), ]
data03all <- data03all [ order(-strike50)]
data03all <- data03all [, rank50 := .GRP, by = .(strike50)]
data03all <- data03all [ order(-strikebnk)]
data03all <- data03all [, rankbnk := .GRP, by = .(-strikebnk)]
data03all <- data03all [, `:=` (filt50 = ifelse(strike50 >= max_nifty - 200, "Far", "Close"), 
                                filtbnk = ifelse(strikebnk >= max_bnk - 200, "Far", "Close"), 
                                cost50 = premium50 * 75 * nifty_lot,
                                costbnk = premiumbnk * 20 * nifty_bnk), ]
data03all <- data03all [, totcost := cost50 + costbnk,]


data02nifty <- data02nifty [, norow := .I,]
data03nifty <- data02nifty[ , list(norow,
                         prices = ( seq(nifty - 300, nifty + 300, by = 25) )), by = 1:nrow(data02nifty)]

data03nifty <- merge (x = data03nifty,
                      y = data02nifty,
                      by = c("norow"),
                      all = TRUE)

data03nifty <- data03nifty [, prices_prm := round( bscall(s = prices, k = strike50, v = v, r = r, tt = timeyrs50, d = d), 2),]


