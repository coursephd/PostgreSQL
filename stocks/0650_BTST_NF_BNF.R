library(TTR)
library(data.table)
library(tidyverse)
library(anytime)
library(zoo)
library(lubridate)
library(curl)
library(ggplot2)
library(plotly)

library(yfR)

options(scipen = 999)

library(BatchGetSymbols)

future.seed = TRUE
options(future.rng.onMisuse="ignore")

future::plan(future::multisession, workers = floor(parallel::detectCores() ))

a01nfity50 <-  yf_get(
  tickers = c("^NSEI", "^NSEBANK", "^BSESN"), # c("^NDX"), # c("^NSEI", "^NSEBANK", "^BSESN", "^N225", "^DJI"), # ^NSEI, ^NSEBANK, ^BSESN # fno$SYMBOL02,  
  first_date = "2002-01-01", #Sys.Date() - 6000, #"2008-01-01", #Sys.Date() - 5000,
  last_date = Sys.Date(),
  thresh_bad_data = 0.75,
  bench_ticker = "^NSEI",
  type_return = "arit",
  freq_data = "daily", #"monthly", #"weekly", #daily",
  how_to_aggregate = "last",
  do_complete_data = FALSE,
  do_cache = TRUE,
  cache_folder = file.path(tempdir(), "BGS_Cache"),
  do_parallel = FALSE, #TRUE, # FALSE
  be_quiet = FALSE
)


all02 <- data.table(a01nfity50)
all02 <- all02 [, trdate := anydate(ref_date), ]
all02 <- all02 [ order(ticker, trdate)]

all02 <- all02 [, allrow := .I, ]
all02 <- all02 [, nrow := 1:.N, by =.(ticker)]

all02 <- all02 [, prvclose := shift(price_close, type =c("lag"), n = 1), by = .(ticker)]
all02 <- all02 [, -c("price_adjusted", "ret_adjusted_prices", "cumret_adjusted_prices"), ]

#####################################################
#
# Calculate diff and diff/2
# diff - will be considered for Futures
# diff/2 - will be considered for ATM CE option
#
#####################################################

all02 <- all02 [, diff := (price_open - prvclose)/2, ]
all02 <- all02 [, diff := ifelse(is.na(diff), 0, diff), ]
all02 <- all02 [, `:=`( diffcum = cumsum(diff), 
                        prfloss = ifelse(diff >0, 1, 0), 
                        yr = year(trdate),
                        qtr = quarter(trdate),
                        mon = month(trdate) ), by = .(ticker)]

all02 <- all02 [, yrpnl := sum(diff), by = .(yr, ticker)]
all02 <- all02 [, qtrpnl := sum(diff), by = .(yr, qtr, ticker)]

ggplot(data = all02, aes (y = diffcum, x = nrow) ) +
  geom_line() +
  geom_point()

############################################
#
# Create tableua type interactive plots
#
############################################

library(GWalkR)
gwalkr(all02)


############################################
#
# Get the numbers in terms 
# % of win / loss
# Cumulative amount of money possibly earned
#
############################################

all03 <- all02 [, .(pfldays = .N), by = .(ticker, prfloss)]
all03 <- all03 [, tot := sum(pfldays), by = .(ticker)]
all03 <- all03 [, perc := pfldays / tot, ]

############################################
#
# This block provides yearly numbers
#
############################################


all04 <- all02 [, .(pfldays = .N), by = .(yr, ticker, prfloss, yrpnl)]
all04 <- all04 [, tot := sum(pfldays), by = .(yr, ticker, yrpnl)]
all04 <- all04 [, perc := pfldays / tot, ]

#####################################################
#
# Calculate diff and diff/2
# diff - will be considered for Futures
# diff/2 - will be considered for ATM CE option
#
# With 30 lac capital each for NIFTY and BANKNIFTY
# it is possible to trade 10000 and 4500 quantity
# 
# This is based on 25 NF lot size and 15 BNF lot size
#
#####################################################

#all04 <- all04 [, qnty := ifelse(ticker == "^NSEBANK", 450, 1000), ]
all04 <- all04 [, qnty := ifelse(ticker == "^NSEBANK", 4500, 10000), ]
all04 <- all04 [, totmoney := yrpnl * qnty, ]
all05 <- all04 [ prfloss == 1]

all05 <- all05 [ order (yr, ticker)]
all05 <- all05 [, cumtotmoney := cumsum(totmoney), by = .(ticker)]


############################################
#
# This block provides quarterly numbers
#
############################################

all04q <- all02 [, .(pfldays = .N), by = .(yr, qtr, ticker, prfloss, yrpnl, qtrpnl)]
all04q <- all04q [, tot := sum(pfldays), by = .(yr, qtr, ticker, yrpnl, qtrpnl)]
all04q <- all04q [, perc := pfldays / tot, ]
all04q <- all04q [, qnty := ifelse(ticker == "^NSEBANK", 4500, 10000), ]
all04q <- all04q [, totmoney := yrpnl * qnty, ]
all04q <- all04q [, qtrmoney := qtrpnl * qnty, ]
all05q <- all04q [ prfloss == 1]

all05q <- all05q [ order (yr, qtr, ticker)]
all05q <- all05q [, cumtotmoney := cumsum(totmoney), by = .(ticker)]
all05q <- all05q [, cumqtrmoney := cumsum(qtrmoney), by = .(ticker)]
