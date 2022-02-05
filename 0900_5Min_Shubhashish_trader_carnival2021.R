
library(rjson)
library(jsonlite)
library(data.table)
library(tidyverse)
library(anytime)
library(TTR)

# This should be run after the BASH command to get the json file


dd01_1 <- fromJSON ("D:\\My-Shares\\prgm/bnf15.json")
dd01_2 <- data.table(dd01_1$data$candles)

setnames(dd01_2, "V1", "trd")
setnames(dd01_2, "V2", "open")
setnames(dd01_2, "V3", "high")
setnames(dd01_2, "V4", "low")
setnames(dd01_2, "V5", "close")
setnames(dd01_2, "V6", "volume")

dd01_2 <- dd01_2 [, nrow := .I, ]

rsi <- as.data.table( RSI(as.numeric(dd01_2$close), 5) )
rsi <- rsi [, nrow := .I, ]
setnames(rsi, "V1", "rsi5")

dd01_2 <- merge(x = dd01_2,
                y = rsi,
                by =c("nrow"))
dd01_2 <- dd01_2 [, `:=`(ema05 = EMA(close, n = 5),
                         mfi05 = MFI(as.numeric(close), as.numeric(volume), n = 5),
                         prvlow = shift(low, n=1, type = c("lag") ),
                         prvcls = shift(close, n=1, type = c("lag") )), ]
dd01_2 <- dd01_2 [, abv_ema05 := ifelse(low > ema05 & close > ema05, 1, 0), ]
dd01_2 <- dd01_2 [, cls_close := ifelse(low > ema05 & close > ema05 & close < prvcls & low < prvlow, 1, 0), ]


curl 'https://kite.zerodha.com/oms/instruments/historical/260105/5minute?user_id=QI9532&oi=1&from=2021-12-07&to=2021-12-07' \
-H 'authority: kite.zerodha.com' \
-H 'sec-ch-ua: " Not A;Brand";v="99", "Chromium";v="96", "Google Chrome";v="96"' \
-H 'accept: application/json, text/plain, */*' \
-H 'authorization: enctoken i7oUKUAixPeTqBw9BGTMeog3hsmMksLgsO7ZNG5uxMWrhRFTVy+WJEdoV53vammIBIyDCKEca2HP98oG42VXzHARMzHp/HWnvgVlznxM5jDzTdEx4Iu8Zg==' \
-H 'sec-ch-ua-mobile: ?0' \
-H 'user-agent: Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/96.0.4664.45 Safari/537.36' \
-H 'sec-ch-ua-platform: "Windows"' \
-H 'sec-fetch-site: same-origin' \
-H 'sec-fetch-mode: cors' \
-H 'sec-fetch-dest: empty' \
-H 'referer: https://kite.zerodha.com/chart/ext/tvc/INDICES/NIFTY%20BANK/260105' \
-H 'accept-language: en-US,en;q=0.9' \
-H 'cookie: _ga=GA1.2.1830839033.1621932472; __utma=134287610.1830839033.1621932472.1623483094.1623483094.1; __utmz=134287610.1623483094.1.1.utmcsr=google|utmccn=(organic)|utmcmd=organic|utmctr=(not%20provided); _hjid=56092100-783a-47be-a871-f89b241b3eb4; WZRK_G=37771d2361b94e01a4b87e23f4fe4a18; mp_7b1e06d0192feeac86689b5599a4b024_mixpanel=%7B%22distinct_id%22%3A%20%225c1fc1a8428d60085fd5ff7e%22%2C%22%24device_id%22%3A%20%2217acdd25965e3-0c1e65009e9c66-6373260-100200-17acdd259668f5%22%2C%22%24initial_referrer%22%3A%20%22https%3A%2F%2Fkite.zerodha.com%2F%22%2C%22%24initial_referring_domain%22%3A%20%22kite.zerodha.com%22%2C%22%24user_id%22%3A%20%225c1fc1a8428d60085fd5ff7e%22%7D; kf_session=YQv9eHMLFw1NAvwoSsJNfYCfZAoMlw8e; user_id=QI9532; public_token=m2D2CCf20rNyK7KJCW5kYC9t34OgDprN; enctoken=i7oUKUAixPeTqBw9BGTMeog3hsmMksLgsO7ZNG5uxMWrhRFTVy+WJEdoV53vammIBIyDCKEca2HP98oG42VXzHARMzHp/HWnvgVlznxM5jDzTdEx4Iu8Zg==' \
--compressed > bnf.json


# https://stackoverflow.com/questions/48101485/curl-request-with-authorization-header-in-r
library(curl)
h <- new_handle(verbose = TRUE)
handle_setheaders(h,
#                  "authority" = "kite.zerodha.com",
#                  "sec-ch-ua" = " Not A;Brand";v="99", "Chromium";v="96", "Google Chrome";v="96"',

)
#con <- curl("https://api-fxpractice.oanda.com/v3/accounts/{account_id}/instruments?instruments=EUR_USD", handle = h)
#jsonlite::prettify(readLines(con))



#####;

library(tidyverse)
library(tidyquant)
library(data.table)

dd01_2 <- fread("D:\\My-Shares\\Intraday-data\\2018\\2018 APR BNF.txt")

setnames(dd01_2, "V1", "symbol")
setnames(dd01_2, "V2", "trd")
setnames(dd01_2, "V3", "time")
setnames(dd01_2, "V4", "open")
setnames(dd01_2, "V5", "high")
setnames(dd01_2, "V6", "low")
setnames(dd01_2, "V7", "close")
setnames(dd01_2, "V8", "volume")

dd01_2 <- dd01_2 [, nrow := .I, ]
dd01_2 <- dd01_2 [, subrow := 1:.N, by = .(trd)]

p1 <- ggplot(data = dd01_2, aes(x = subrow, y = close)) +
  geom_candlestick(aes(open = open, high = high, low = low, close = close)) +
  labs(title = "BNF candlestick Chart", y = "Closing Price", x = "") + 
  facet_wrap(~ trd, ncol = 3, scale = "free_y") + 
  theme_tq()


p1 <- ggplot(data = dd01_2, aes(x = subrow, y = close)) +
  geom_candlestick(aes(open = open, high = high, low = low, close = close)) +
  labs(title = "BNF candlestick Chart", y = "Closing Price", x = "")  + 
  theme_tq()


library(gganimate)

p1.anim = p1 + transition_reveal(trd)

anim_p1 = animate(p1.anim, fps = 10, start_pause = 2, end_pause = 5, rewind = FALSE,
                  width = 800, height = 1000)