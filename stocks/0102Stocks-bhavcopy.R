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

# from today to 2 years behind
tday <- Sys.Date()

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

# Create the whole data and save that in the Rdataset

out <- rbindlist(bhavcopy)
saveRDS (out, "D:/My-Shares/analysis/bhavvopy_all.rds")
fwrite(out, "D:/My-Shares/analysis/bhavvopy_all.csv")


#####################################################################################
# End of program
#####################################################################################
