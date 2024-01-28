library(TTR)
library(data.table)
library(tidyverse)
library(anytime)
library(zoo)
library(lubridate)
library(curl)
library(ggplot2)
library(plotly)

options(scipen = 999)

library(BatchGetSymbols)

future.seed = TRUE
options(future.rng.onMisuse="ignore")

future::plan(future::multisession, workers = floor(parallel::detectCores() ))

#fno <- fread("https://archives.nseindia.com/content/fo/fo_mktlots.csv")
#fno <- fno [, c("SYMBOL"), ]
#fno <- fno [, `:=` (nrow = .I, SYMBOL02 = paste(SYMBOL, ".NS", sep="") ), ]


# For NIFTY aLL

fno <- fread("C:\\My-Shares\\analysis\\EQUITY_L.csv")
#fno <- fno [, SYMBOL :=  Symbol, ]
fno <- fno [, c("SYMBOL"), ]
fno <- fno [, `:=` (nrow = .I, SYMBOL02 = paste(SYMBOL, ".NS", sep="") ), ]

# Exract all data including NIFTY 50

a01nfity50 <-  BatchGetSymbols(
  tickers = c("^NSEI", fno$SYMBOL02), #c("^CNXAUTO", "^NSEI"),
  first.date = Sys.Date() - 2000, #"2016-01-01", #  #"2008-01-01", #Sys.Date() - 5000,
  last.date = Sys.Date(),
  thresh.bad.data = 0.75,
  bench.ticker = "^NSEI",
  type.return = "arit",
  freq.data = "daily",
  how.to.aggregate = "last",
  do.complete.data = FALSE,
  do.fill.missing.prices = TRUE,
  do.cache = TRUE,
  cache.folder = file.path(tempdir(), "BGS_Cache"),
  do.parallel = FALSE, #TRUE, # FALSE
  be.quiet = FALSE
)

all02 <- data.table(a01nfity50$df.tickers)
all02 <- all02 [, trdate := anydate(ref.date), ]
all02 <- all02 [ order(ticker, trdate)]

all02 <- all02 [, allrow := .I, ]
all02 <- all02 [, nrow := 1:.N, by =.(ticker)]

#saveRDS(all02, "C:\\Users\\mahajvi1\\Downloads\\list.rds")
#all03 <- readRDS("C:\\Users\\mahajvi1\\Downloads\\list.rds")

all022 <- all02 [ ticker == "^NSEI"]
all023 <- all022 [, c("ticker", "price.adjusted", "trdate"), ]

setnames(all023, "ticker", "nifty50")
setnames(all023, "price.adjusted", "nifty50price")

all02 <- merge (x = all02 , #all03,
                y = all023,
                by = c("trdate"))

################################################################
#
# Calculate daily returns for stock and Nifty 50
#
################################################################
all02 <- all02 [, `:=`(stk_rtn = (price.close) / shift(price.close, type = c("lag"), n = 1),
                       mkt_rtn = (nifty50price) / shift(nifty50price, type = c("lag"), n = 1) ), by = .(ticker)]

################################################################
#
# Calculate Beta - regression coefficient for rolling window
# Cov (stock, market) / variance (market)
# TTR library does not allow the Cov and Var calculation at one go
# hence the calculations are split in 2 variables
# Cov / Var is calculated in the next step 
#
# Video link
# https://www.youtube.com/watch?v=vr1lQeKX8Mc&ab_channel=heyi%27maaron
#
################################################################
roll01 <- all02 [, `:=`(beta63_num = runCov(x = stk_rtn, y = mkt_rtn, n = 75),
                        beta63_den = runVar(x = mkt_rtn, n = 75),
                        
                        beta126_num = runCov(x = stk_rtn, y = mkt_rtn, n = 30),
                        beta126_den = runVar(x = mkt_rtn, n = 30),
                        
                        beta189_num = runCov(x = stk_rtn, y = mkt_rtn, n = 45),
                        beta189_den = runVar(x = mkt_rtn, n = 45),
                        
                        beta252_num = runCov(x = stk_rtn, y = mkt_rtn, n = 60),
                        beta252_den = runVar(x = mkt_rtn, n = 60) ), by = .(ticker)]


roll01 <- roll01 [, `:=` (beta63  = beta63_num / beta63_den,
                          beta126 = beta126_num / beta126_den,
                          beta189 = beta189_num / beta189_den,
                          beta252 = beta252_num / beta252_den ), ]

################################################################
#
# Now use the quarterly returns adjusted for the beta
# Provide 40% and 20% weightages across quarters
#
# Calculate RS similar to what is provided in IBD
# (1 + stock returns) / (1 + market returns) * 100
#
################################################################

roll01 <- roll01 [, `:=`(returns =
                           ( price.close / shift(price.close, type = c("lag"), n = 30) * 0.4 * (beta63_num / beta63_den) +
                               price.close / shift(price.close, type = c("lag"), n = 45) * 0.2 * (beta126_num / beta126_den) +
                               price.close / shift(price.close, type = c("lag"), n = 60) * 0.2 * (beta189_num / beta189_den) +
                               price.close / shift(price.close, type = c("lag"), n = 75) * 0.2 * (beta252_num / beta252_den) ) * 100,
                         
                         n50returns =
                           ( nifty50price / shift(nifty50price, type = c("lag"), n = 30) +
                               nifty50price / shift(nifty50price, type = c("lag"), n = 45) +
                               nifty50price / shift(nifty50price, type = c("lag"), n = 60) +
                               nifty50price / shift(nifty50price, type = c("lag"), n = 75) ) * 100, 
                         
                         beta = 0.4 * (beta63_num / beta63_den) +
                           0.2 * (beta126_num / beta126_den) +
                           0.2 * (beta189_num / beta189_den) +
                           0.2 * (beta252_num / beta252_den) ), by = .(ticker)]

roll01 <- roll01 [, rs:= round ( (1 + returns) / (1 + n50returns)* 100, 2), ]


bb_n <- 20
bb_dn = as.data.table( BBands(all02[,c("price.high","price.low","price.close"),], n = bb_n) )
bb_dn <- bb_dn [, allrow := .I, ]

kc_n <- 20
kc_dn1 = as.data.table( keltnerChannels(all02[,c("price.high","price.low","price.close"),], n = kc_n, atr = 1) )
kc_dn1 <- kc_dn1 [, allrow := .I, ]
setnames(kc_dn1, c("dn", "up"), c("dn1", "up1"))
kc_dn1 <- kc_dn1 [, -c("mavg"), ]

kc_dn15 = as.data.table( keltnerChannels(all02[,c("price.high","price.low","price.close"),], n = kc_n, atr = 1.5) )
kc_dn15 <- kc_dn15 [, allrow := .I, ]
setnames(kc_dn15, c("dn", "up"), c("dn15", "up15"))
kc_dn15 <- kc_dn15 [, -c("mavg"), ]

kc_dn2 = as.data.table( keltnerChannels(all02[,c("price.high","price.low","price.close"),], n = kc_n, atr = 2) )
kc_dn2 <- kc_dn2 [, allrow := .I, ]
setnames(kc_dn2, c("dn", "up"), c("dn2", "up2"))
kc_dn2 <- kc_dn2 [, -c("mavg"), ]


all02 <- Reduce(function(...) merge(..., by = c("allrow"), all=T),  
                list( roll01, bb_dn, kc_dn1, kc_dn15, kc_dn2) )

#####################################################################################
#
# Calculate the Bollinger band, the calculations are done over all the dataset
# So the first n lines of the bollinger band calculations go wrong
# as the data from previous company is carried forward
#
# Remove the calculations for first 20 * 2 rows

# To avoid any incorrect calculations explained above, remove certain number of rows
#
#####################################################################################

all03 <- all02 [ nrow > bb_n * 2]

#####################################################################################
#
# Calculate Waves A, B and C
# Wave A: 8 and 34
# Wave B: 8 and 89
# Wave C: 8 and 144
#
# Calculate squeeze conditions
## SQUEEZE CONDITIONS
## NoSqz = BB_lower < KC_lower_low or BB_upper > KC_upper_low //NO SQUEEZE: GREEN
## LowSqz = BB_lower >= KC_lower_low or BB_upper <= KC_upper_low //LOW COMPRESSION: BLACK
## MidSqz = BB_lower >= KC_lower_mid or BB_upper <= KC_upper_mid //MID COMPRESSION: RED
## HighSqz = BB_lower >= KC_lower_high or BB_upper <= KC_upper_high //HIGH COMPRESSION: ORANGE
#
#####################################################################################

all03 <- all03 [, `:=`(ma200 = SMA(price.close, 200),
                       ma8  = EMA(price.close, 8),
                       ma34 = EMA(price.close, 34),
                       ma89 = EMA(price.close, 89),
                       ma144 = EMA(price.close, 144),
                       z75 = (price.close - SMA(price.close, 75)) / runSD(price.close, 75) ), by = .(ticker)]

all03 <- all03 [, `:=`(amacd1 =  ma8 - ma34,
                       bmacd1 =  ma8 - ma89,
                       cmacd1 =  ma8 - ma144, 
                       z75sma = SMA(z75, 75) ), by = .(ticker)] 

all03 <- all03 [, `:=`(asignal1 = EMA(amacd1, 8),
                       bsignal1 = EMA(bmacd1, 89),
                       csignal1 = EMA(cmacd1, 144)), by = .(ticker)] 

all03 <- all03 [, `:=`(NoSqz = ifelse(dn < dn2 | up > up2, 1, 0),   
                       LowSqz = ifelse(dn >= dn2 | up <= up2, 1, 0), 
                       MidSqz = ifelse(dn >= dn15 | up <= up15, 1, 0),
                       HighSqz = ifelse(dn >= dn1 | up <= up1, 1, 0) ), ]

#####################################################################################
#
#  Z-Score - SMA Calculations                                                                  
#
#####################################################################################

all04 <- na.omit(all03, cols= "z75")
all04 <- all04 [, z75sma := SMA(z75, 75), by = .(ticker)] 
all04 <- all04 [, z_comp := ifelse(z75 >= z75sma, 1, 0), ]


all04chk <- all04 [, c("ref.date", "ticker", "price.close", "z75", "z75sma", "z_comp"), ]


#####################################################################################
#
# End of program
#
#####################################################################################



roll01 <- roll01 [ order(trdate, -rs) ]
roll01 <- roll01 [, rsrank := 1:.N, by = .(trdate)]
roll02 <- roll01 [, c("trdate", "ticker", "beta", "price.open", "price.high", "price.low", "price.close", "price.adjusted", 
                      "allrow", "nrow", "nifty50", "nifty50price", "returns", "n50returns", 
                      "rs", "rsrank"),]



##########################################################
#
# Pick up the industry name from this excel from BSE
# Complete list of companies with the sectors appear
#
# https://www.bseindia.com/corporates/List_Scrips.html
#
#
##########################################################

#################################################################################
#
# Get the list of stock with the industry names from BSE website
# https://www.bseindia.com/corporates/List_Scrips.html
# Name the file as 001_bse.xlsx
#
# To avoid programming problems an additional comma is added to the Column row
#
#################################################################################

step001 <- fread("C:\\Users\\vyomv\\Downloads\\Select.csv")
#step001 <- fread("D:\\My-Shares\\analysis\\001_bse.csv")
setnames(x=step001, old=names(step001), new=gsub(" ","",names(step001)))

# Subset only the active companies and equity
step002 <- step001 [Status == "Active" & Instrument == "Equity" & Group != "Z" & !Industry %in% c("", "Mutual Fund Scheme - ETF")]
step002 <- step002 [, ticker := paste(SecurityId, ".NS", sep=""),]





nifty500 <- fread("https://www1.nseindia.com/content/indices/ind_nifty500list.csv")
setnames(nifty500, "ISIN Code", "ISINCode")


all <- fread("https://nsearchives.nseindia.com/content/equities/EQUITY_L.csv")

nseall <- "https://nsearchives.nseindia.com/content/equities/EQUITY_L.csv"
download.file(nseall, "C:/Users/vyomv/Downloads/EQUITY_L.csv")


icici_url <- "https://directlink.icicidirect.com/NewSecurityMaster/SecurityMaster.zip"
download.file(icici_url, "C:/My-Shares/analysis/SecurityMaster.zip")
unzip(zipfile = "C:/My-Shares/analysis/SecurityMaster.zip", exdir = "C:/My-Shares/analysis")

# List of compnay names present in ICICI fno
icici_fno <- fread("C:/My-Shares/analysis/NSEScripMaster.txt")
icici_fno <- unique( icici_fno [, c("ShortName"), ])


a01nfity50 <-  BatchGetSymbols(
  tickers = "INE002A01018", #"fno$SYMBOL02, #c("^CNXAUTO", "^NSEI"),
  first.date = "2016-01-01", # Sys.Date() - 500, #"2008-01-01", #Sys.Date() - 5000,
  last.date = "2020-01-01", # Sys.Date(),
  thresh.bad.data = 0.75,
  bench.ticker = "^NSEI",
  type.return = "arit",
  freq.data = "daily",
  how.to.aggregate = "last",
  do.complete.data = FALSE,
  do.fill.missing.prices = TRUE,
  do.cache = TRUE,
  cache.folder = file.path(tempdir(), "BGS_Cache"),
  do.parallel = FALSE, #TRUE, # FALSE
  be.quiet = FALSE
)

