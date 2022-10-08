
if (! "ttr"        %in% tolower ( (.packages() ) ) ) { library(TTR)        } else { print ("The library TTR is already loaded") }
if (! "data.table" %in% tolower ( (.packages() ) ) ) { library(data.table) } else { print ("The library data.table is already loaded") }
if (! "tidyverse"  %in% tolower ( (.packages() ) ) ) { library(tidyverse)  } else { print ("The library tidyverse is already loaded") }
if (! "anytime"    %in% tolower ( (.packages() ) ) ) { library(anytime)    } else { print ("The library anytime is already loaded") }
if (! "zoo"        %in% tolower ( (.packages() ) ) ) { library(zoo)        } else { print ("The library zoo is already loaded") }
if (! "lubridate"  %in% tolower ( (.packages() ) ) ) { library(lubridate)  } else { print ("The library lubridate is already loaded") }
if (! "arrow"      %in% tolower ( (.packages() ) ) ) { library(arrow)      } else { print ("The library arrow is already loaded") }
if (! "reticulate" %in% tolower ( (.packages() ) ) ) { library(reticulate) } else { print ("The library reticulate is already loaded") }

# read_feather function from this library works but not from feather

#if (! "openxlsx" %in% tolower ( (.packages() ) ) )   { library(openxlsx) }   else { print ("The library openxlsx is already loaded") }
#if (! "RCurl" %in% tolower ( (.packages() ) ) )      { library(RCurl) }      else { print ("The library RCurl is already loaded") }
#if (! "curl" %in% tolower ( (.packages() ) ) )       { library(curl) }       else { print ("The library curl is already loaded") }
#if (! "plotly" %in% tolower ( (.packages() ) ) )     { library(plotly) }     else { print ("The library plotly is already loaded") }

options(scipen = 999)
options(digits=2)

#library(BatchGetSymbols)

#future.seed = TRUE
#options(future.rng.onMisuse="ignore")

#future::plan(future::multisession, workers = floor(parallel::detectCores() ))

start_time <- Sys.time()

###########################################################
#
# Part 1
# Create a mapping of the company names
# ICICI - Yahoo - NSE [FnO stocks]
#
###########################################################
#
# Creating conditional execution of the mapping dataset
#
###########################################################

file_crea <- file.info("D:/My-Shares/analysis/icici_fno.rds")$mtime
compdt <- as.POSIXct( paste(Sys.Date(), "09:15:00", sep = ""), tz=Sys.timezone())

if (is.na(file_crea) ) {print("File does not exist, the source code must be executed to create mapping file")
  source("D:\\My-Shares\\prgm\\0550_tradingview_yh_icici_map.R") 
  file_crea <- file.info("D:/My-Shares/analysis/icici_fno.rds")$mtime
} 

if (file_crea >= compdt ) {
  print("The mapping dataset exists for the day, need not re-execute, only extracting from the earlier version to the local area") 
  icici_fno <- readRDS("D:/My-Shares/analysis/icici_fno.rds")
} else { 
  print("The mapping dataset needs to be created, executing the source code")
  source("D:\\My-Shares\\prgm\\0550_tradingview_yh_icici_map.R") 
}

###########################################
#
# Part 2
#
# Python calculations
# Get the 5 mins data for calculations
# Get 1 day data for Fibo calculations
# Fibo values will be used for the targets
#
###########################################

#use_python("C:/ProgramData/Anaconda3/python.exe", required = T)

use_python("C:/Program Files/Python310/python.exe", required = T)

# Get the source data from yahoo finance, 
# Create a csv file to be passed into supertrend formula

py_run_file("D:/My-Shares/prgm/0550_yh_stock_part01_5min_temp.py")
#system(paste('"c:/Program Files/Python310/python.exe"', 'D:/My-Shares/prgm/0550_yh_stock_part01_5min_temp.py'))


pickle_data <- as.data.table (py$data)

pd <- import("pandas")
#pickle_data <- as.data.table ( pd$read_pickle("D:/My-Shares/analysis/0550_data.pkl") )
pickle_data <- pickle_data [, nrow := 1:.N, ]

dnames <- as.data.table ( names(pickle_data) )
dnames <- dnames [, nrow := 1:.N, ]
dnames <- dnames [, nrow02 := ceiling(nrow / 6), ]
dnames <- dnames [, subrow := 1:.N, by = .(nrow02)]
dnames <- dnames [, v10 := str_squish(V1), ]
dnames <- dnames [, v10 := ifelse(v10 == "Adj Close", "Adj_Close", v10), ]
dnames <- dnames [, v11 := ifelse(subrow > 1, paste(" ", v10, sep=""), v10), ]
dnames <- dnames [, c("tmp01", "tmp02") := tstrsplit(v11, " ", fill =""), ]
dnames02 <- dnames [subrow == 1, c("tmp01", "nrow02"), ]

dnames03 <- merge(x = dnames [, -c("tmp01"), ],
                  y = dnames02,
                  by = c("nrow02"))

dnames03 <- dnames03 [, tmp03 := paste(tmp01, tmp02, sep="_"),]
dnames03 <- dnames03 [, chg001 := paste("setnames (pickle_data, '", V1, "', '", tmp03, "')", sep = "") , ]

eval(parse(text = dnames03$chg001))

pickle_data_t <- melt(data = pickle_data, id.vars = c("Datetime_", "Datetime_nrow"))
data03_t <- pickle_data_t [, c("Name", "ohlcv", "tmp") := tstrsplit(variable, "_"), ]
stock_final <- dcast(data = data03_t [, -c("tmp"), ],
                     Datetime_ + Datetime_nrow + Name ~ ohlcv, 
                     value.var = c("value") )

setnames (stock_final, "Datetime_", "Datetime")
stock_final <- as.data.table(stock_final)
stock_final <- stock_final [, trdtme := format(Datetime, tz="Asia/Calcutta"), ]
stock_final <- stock_final [, trdate := anydate(str_sub(trdtme, 1, 10) ), ]
stock_final <- stock_final [ order(Name, trdtme) ]
stock_final <- stock_final [, nrow := 1:.N, by = .(Name)]
stock_final <- stock_final [, subrow := 1:.N, by = .(Name, trdate)]

setnames(stock_final, c("Open", "High", "Low", "Close", "Volume", "Name"), 
         c("price.open", "price.high", "price.low", "price.close", "volume", "ticker") )

all02 <- stock_final # [, -c("NA"), ]
all02 <- all02 [, `:=`(price.open = as.numeric(price.open), 
                       price.high = as.numeric(price.high), 
                       price.low = as.numeric(price.low), 
                       price.close = as.numeric(price.close), 
                       volume = as.numeric(volume)), ]
#all02 <- all02 [, mva := EMA(price.close, 20), by = .(ticker)]

#all02 <- all02 [, ovwap := VWAP(price.open, volume, n = 75),]
#all02 <- all02 [, hvwap := VWAP(price.high, volume, n = 75),]
#all02 <- all02 [, lvwap := VWAP(price.low, volume, n = 75),]
#all02 <- all02 [, cvwap := VWAP(price.close, volume, n = 75),]

all02 <- na.omit(all02)
all02 <- all02 [, allrow := .I, ]
all02 <- all02 [, nrow := 1:.N, by = .(ticker)]

bb_n <- 20
bb_dn = as.data.table( BBands(all02$price.close, n = bb_n, k = 2) )
bb_dn <- bb_dn [, allrow := .I, ]

dc_dn = as.data.table( DonchianChannel(all02 [, c("price.high", "price.low"),], n = bb_n) )
dc_dn <- dc_dn [, allrow := .I, ]


all02 <- Reduce(function(...) merge(..., by = c("allrow"), all=T),  
                list( all02, bb_dn [, -c("pctB"), ], dc_dn ) )

all02 <- all02 [, `:=`( prv.low = shift(price.low, type =c("lag"), n = 1),
                        prv.low02 = shift(price.low, type =c("lag"), n = 2),
                        prv.close = shift(price.close, type =c("lag"), n = 1),
                        prv.close02 = shift(price.close, type =c("lag"), n = 2),
                        prvdn = shift(dn, type =c("lag"), n = 1),
                        prvdn02 = shift(dn, type =c("lag"), n = 2),
                        prvup = shift(up, type =c("lag"), n = 1),
                        prvup02 = shift(up, type =c("lag"), n = 2),
                        prv.open = shift(price.open, type =c("lag"), n = 1),
                        prv.open02 = shift(price.open, type =c("lag"), n = 2),
                        prv.high = shift(price.high, type =c("lag"), n = 1),
                        prv.high02 = shift(price.high, type =c("lag"), n = 2) ), 
                by = .(ticker)]

all02 <- all02 [, bullReversal :=  ifelse(prv.low < prvdn & prv.close < prv.open & price.close > dn & price.close > price.open, 1, 0), ]
all02 <- all02 [, bullReversalConfirm := ifelse(prv.low02 < prvdn02 & prv.close02 < prv.open02 & prv.close > prvdn & prv.close > prv.open & price.close > prv.high, 1, 0), ]
all02 <- all02 [, bearReversal := ifelse(prv.high > prvup & prv.close > prv.open & price.close < up & price.close < price.open, 1, 0), ]
all02 <- all02 [, bearReversalConfirm := ifelse(prv.high02 > prvup02 & prv.close02 > prv.open02 & prv.close < prvup & prv.close < prv.open & price.close < prvdn, 1, 0), ]

###########################################
#
# Remove blank value for EMA calculations
# After calculating VOL And price Moving
# averages again remove na's
#
# Change: Calculate % change compared to 5 period
# % change: Calculate % change compared to 5 period
# Momentum: SMA 5 period on change of change
# Acceleration: Average Change of the Change over 5 period, average acceleration
# 
# Calculate these values for volume also
#
###########################################

all02 <- na.omit(all02)

all02 <- all02 [, prc5 := shift(price.close, type = c("lag"), n = 5), by = .(ticker) ]
all02 <- all02 [, abs_chg := price.close - prc5, ]
all02 <- all02 [, perc_chg := (price.close - prc5) / prc5 * 100, ]
all02 <- all02 [, mom5 := SMA (abs_chg, n = 5), by = .(ticker)]
all02 <- all02 [, acc5 := SMA (mom5, n = 5), by = .(ticker)]
all02 <- all02 [, volat := price.close / shift(price.close, type = c("lag"), n = 1), by = .(ticker)]
all02 <- all02 [, volat5 := SMA (volat, n = 5), by = .(ticker)]

##############################
#
# See if this should be done
# volume = 0 to change to 1
##############################

all02 <- all02 [, volume := ifelse(volume == 0, 2, volume), ]
all02 <- all02 [, volume := log(volume), ]
all02 <- all02 [, v5 := shift(volume, type = c("lag"), n = 5), by = .(ticker) ]
all02 <- all02 [, vabs_chg := volume - v5, ]
all02 <- all02 [, vperc_chg := (volume - v5) / v5 * 100, ]
all02 <- all02 [, vmom5 := SMA (vabs_chg, n = 5), by = .(ticker)]
all02 <- all02 [, vacc5 := SMA (vmom5, n = 5), by = .(ticker)]

all02 <- all02 [, cont5 := price.close/runSum(price.close, n = 5), by = .(ticker)]
all02 <- all02 [, cmom5 := SMA (cont5, n = 5), by = .(ticker)]
all02 <- all02 [, cacc5 := SMA (cont5, n = 5), by = .(ticker)]


####################################################
#
# Calculate Shannon entropy based on 
# https://www.tradingview.com/v/90gGxKtX/
#
# In the step above, volume is already converted to log scale
# So no need to again convert to log scale below
#
####################################################
all02 <- all02 [, cr := price.close/runSum(price.close, 12), by = .(ticker)]
#all02 <- all02 [, vr := log(volume)/sum(log(volume), 12), by = .(ticker)]
all02 <- all02 [, vr := volume/sum(volume, 12), by = .(ticker)]

all02 <- na.omit(all02)
all02 <- all02 [, vr := ifelse(vr == 0, 0.000000001, vr), ]
all02 <- all02 [, info := runSum(vr *log(vr)/log(2), 12) - runSum(cr*log(cr)/log(2), 12), by = .(ticker)]
all02 <- all02 [, hvp := runPercentRank(info, 75), by = .(ticker)]
  
###########################################
#
# Calculate t-test statistics for each of
# Calculate normal distribution CDF for each of
#
# Price % change
# Price momentum
# Price acceleration
# Price contribution
#
# Volume % change
# Volume momentum
# Volume acceleration
#
###########################################

all02 <- na.omit(all02)

all02 <- all02 [, `:=` (pc_mu = SMA(perc_chg, 75), pc_sigma = runSD(perc_chg, 75),
                        pm_mu = SMA(mom5, 75),     pm_sigma = runSD(mom5, 75),
                        pa_mu = SMA(acc5, 75),     pa_sigma = runSD(acc5, 75),
                        
                        vc_mu = SMA(vperc_chg, 75), vc_sigma = runSD(vperc_chg, 75),
                        vm_mu = SMA(vmom5, 75),     vm_sigma = runSD(vmom5, 75),
                        va_mu = SMA(vacc5, 75),     va_sigma = runSD(vacc5, 75),
                        
                        cc_mu = SMA(cont5, 75),     cc_sigma = runSD(cont5, 75),
                        cm_mu = SMA(cmom5, 75),     cm_sigma = runSD(cmom5, 75),
                        ca_mu = SMA(cacc5, 75),     ca_sigma = runSD(cacc5, 75),
                        
                        pv_mu = SMA(volat5, 75),      pv_sigma = runSD(volat5, 75),
                        
                        xbar = SMA(perc_chg, 5) ), by = .(ticker)]

all02 <- all02 [, `:=`(z_pc = pnorm(q = perc_chg, mean = pc_mu, sd = pc_sigma),
                       z_pm = pnorm(q = mom5,     mean = pm_mu, sd = pm_sigma),
                       z_pa = pnorm(q = acc5,     mean = pa_mu, sd = pa_sigma),
                       
                       z_vc = pnorm(q = vperc_chg, mean = vc_mu, sd = vc_sigma),
                       z_vm = pnorm(q = vmom5,     mean = vm_mu, sd = vm_sigma),
                       z_va = pnorm(q = vacc5,     mean = va_mu, sd = va_sigma),
                       
                       z_cc = pnorm(q = cont5,     mean = cc_mu, sd = cc_sigma),
                       z_cm = pnorm(q = cmom5,     mean = cm_mu, sd = cm_sigma),
                       z_ca = pnorm(q = cacc5,     mean = ca_mu, sd = ca_sigma),
                       z_pv = pnorm(q = volat5,    mean = pv_mu, sd = pv_sigma) ), ]

#all02$z_pc[is.na(all02$z_pc)] <- 0.0000001
#all02$z_pm[is.na(all02$z_pm)] <- 0.0000001
#all02$z_pa[is.na(all02$z_pa)] <- 0.0000001
#all02$z_vc[is.na(all02$z_vc)] <- 0.0000001
#all02$z_vm[is.na(all02$z_vm)] <- 0.0000001
#all02$z_va[is.na(all02$z_va)] <- 0.0000001

all02 <- all02[!is.na(all02$z_pc),]
all02 <- all02[!is.na(all02$z_pm),]
all02 <- all02[!is.na(all02$z_pa),]
all02 <- all02[!is.na(all02$z_vc),]
all02 <- all02[!is.na(all02$z_vm),]
all02 <- all02[!is.na(all02$z_va),]
all02 <- all02[!is.na(all02$z_cc),]
all02 <- all02[!is.na(all02$z_cm),]
all02 <- all02[!is.na(all02$z_ca),]
all02 <- all02[!is.na(all02$z_pv),]

all02 <- all02 [, `:=` (z_pcsmt3 = SMA(z_pc, 3),
                        z_pmsmt3 = SMA(z_pm, 3),
                        z_pasmt3 = SMA(z_pa, 3),
                        
                        z_vcsmt3 = SMA(z_vc, 3),
                        z_vmsmt3 = SMA(z_vm, 3),
                        z_vasmt3 = SMA(z_va, 3),
                        
                        z_ccsmt3 = SMA(z_cc, 3),
                        z_cmsmt3 = SMA(z_cm, 3),
                        z_casmt3 = SMA(z_ca, 3),
                        
                        z_pvsmt3 = SMA(z_pv, 3),
                        subrow02 = as.ITime (as.ITime("09:15") + (subrow-1)*5*60 ) ), by = .(ticker)]

trial001 <- copy(all02)

# Check only for the Price ST and MFI ST confirmation, so do not check the additional conditions 
output <- trial001 [z_pcsmt3 >= 0.9 & z_pmsmt3 >= 0.9 & z_pasmt3 >= 0.9 & 
                                      z_vmsmt3 >= 0.9 & z_vasmt3 >= 0.9 & hvp >= 0.9 &
                                      z_cmsmt3 >= 0.9 & z_casmt3 >= 0.9 &
                                      price.low > up &
                                      subrow >= 4  & subrow <= 65]

######################################################################################
#
# Tried creating the long set-up
# Check for the price %change, momentum, acceleration, and bullReversalConfirm
#
######################################################################################

output <- trial001 [z_pcsmt3 <= 0.1 & z_pmsmt3 <= 0.1 & z_pasmt3 <= 0.1 & bullReversalConfirm == 1 &
                      subrow >= 4  & subrow <= 65]

output <- output [, subset := 1:.N, by =.(ticker, trdate)]

output02 <- output [ subset == 1]
output02 <- output02 [, c("ticker", "trdate", "subrow", "subrow02", "price.open", "price.high", "price.low", "price.close", "up"), ]
output02 <- output02 [, signal := 1, ]

setnames(output02, "price.open", "entry_o")
setnames(output02, "price.high", "entry_h")
setnames(output02, "price.low", "entry_l")
setnames(output02, "price.close", "entry_c")
setnames(output02, "subrow", "entry_row")
setnames(output02, "subrow02", "entry_time")

output02long <- output02 [, ncount := uniqueN(ticker), by = .(trdate)]
output02long <- output02long [  order(-trdate, -entry_row), ]

######################################################################################
# Tried creating the short set-up

trial001 <- copy(all02)
output <- trial001 [z_pcsmt3 >= 0.8 & z_pmsmt3 >= 0.8 & z_pasmt3 >= 0.8 & bearReversalConfirm == 1 &
                      subrow >= 4 & subrow <= 65]

output <- trial001 [bearReversalConfirm == 1 &
                      subrow >= 4 & subrow <= 65]

output <- output [, subset := 1:.N, by =.(ticker, trdate)]

output02 <- output [ subset == 1]
output02 <- output02 [, c("ticker", "trdate", "subrow", "subrow02", "price.open", "price.high", "price.low", "price.close", "up"), ]
output02 <- output02 [, signal := 1, ]

setnames(output02, "price.open", "entry_o")
setnames(output02, "price.high", "entry_h")
setnames(output02, "price.low", "entry_l")
setnames(output02, "price.close", "entry_c")
setnames(output02, "subrow", "entry_row")
setnames(output02, "subrow02", "entry_time")

output02short <- output02 [, ncount := uniqueN(ticker), by = .(trdate)]
output02short <- output02short [  order(-trdate, -entry_row), ]

######################################################################################
#
######################################################################################



library(tidyquant)

p1 <- ggplot(data = all02 [ ticker == "RELIANCE.NS" & trdate == "2022-09-30"], aes(x = subrow, y = price.close)) +
  geom_candlestick(aes(open = price.open, high = price.high, low = price.low, close = price.close)) +
  labs(title = "BNF candlestick Chart", y = "Closing Price", x = "")  + 
  theme_tq()


library(gganimate)

p1.anim = p1 + transition_reveal(subrow)

anim_p1 = animate(p1.anim, fps = 10, start_pause = 2, end_pause = 5, rewind = FALSE,
                  width = 800, height = 1000)





all02 <- all02 [, t := (xbar - mu) / (sigma / sqrt(5) ), by = .(ticker)]
all02 <- all02 [, s := sqrt( runSum( (perc_chg - xbar)^2 / (5-1), n = 5) ), by = .(ticker)]
all02 <- all02 [, f_t := (0.5 + t * (t^2) + 6 ) / (2*  ((t^2) + 4)^1.5 ), ]

all02 <- all02 [, zbar := pmax( pmin( (perc_chg - xbar)/s, 5.55), -5.55), ]
all02 <- all02 [, tmp := 1 / ( 1 + (1 - (zbar/5.555)^(1/0.1186) ) ), ]
all02 <- all02 [, n_cdf := SMA(tmp, 3), by = .(ticker)]





all02 <- all02 [, t_test := pt(q = perc_chg, mean = mu, sd = sigma), ]


t_CDF(_src,_len,_avg) =>
  mu = sma(_src,_avg)
sigma = sqrt(sum(pow(_src - mu,2),_avg)/_avg)
x_bar = sma(_src,_len) //mean
s = sqrt(sum(pow(_src - x_bar,2),_len)/(_len-1))
t = (x_bar - mu)/(sigma/sqrt(_len))
F_t = iff(_len<5,
          0.5 + t/(2*sqrt(2)*sqrt(1+0.5*pow(t,2))),
          0.5 + t*(pow(t,2)+6)/(2*pow(pow(t,2)+4,1.5)))   //CFD v=2 or v=4

