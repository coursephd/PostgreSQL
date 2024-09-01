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
  tickers = c("^NSEI", "^NSEBANK", "^BSESN"), # ^NSEI, ^NSEBANK, ^BSESN # fno$SYMBOL02,  
  first_date = Sys.Date() - 4000, #"2008-01-01", #Sys.Date() - 5000,
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
all02 <- all02 [, diff := price_open - prvclose, ]
all02 <- all02 [, diff := ifelse(is.na(diff), 0, diff), ]
all02 <- all02 [, diffcum := cumsum(diff), by = .(ticker)]
all02 <- all02 [, prfloss := ifelse(diff >0, 1, 0), by = .(ticker)]
all02 <- all02 [, yr := year(trdate), by = .(ticker)]
all02 <- all02 [, yrpnl := sum(diff), by = .(yr, ticker)]

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


all04 <- all02 [, .(pfldays = .N), by = .(yr, ticker, prfloss, yrpnl)]
all04 <- all04 [, tot := sum(pfldays), by = .(yr, ticker, yrpnl)]
all04 <- all04 [, perc := pfldays / tot, ]
all04 <- all04 [, qnty := ifelse(ticker == "^NSEBANK", 450, 1000), ]
all04 <- all04 [, totmoney := yrpnl * qnty, ]
all05 <- all04 [ prfloss == 1]

all05 <- all05 [ order (yr, ticker)]
all05 <- all05 [, cumtotmoney := cumsum(totmoney), by = .(ticker)]
