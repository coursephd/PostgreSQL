#
# Momentum strategy as suggested by Alok Jain -- used the Nifty methodlogy to get some momentum index calculations
#
# Based on 0502_turtle_20day_50day_all_equity_display.R:
# Merge the data with only 

a03all <- readRDS("D:\\My-Shares\\analysis\\a03all.rds")

i11_small100 <- fread('https://www1.nseindia.com/content/indices/ind_niftysmallcap100list.csv')
cntrt02 <- i11_small100 [, `:=` (nrow = .I, ticker = paste(Symbol, ".NS", sep="") ), ]
setnames(cntrt02, "ISIN Code", "ISIN")

a03_nifty_small100 <- merge(x = a03all,
                            y = cntrt02, 
                            by = c("ticker"),
                            all.y = TRUE)

ami001 <- a03_nifty_small100 [, c("ticker", "ref.date", "price.open", "price.high", "price.low", "price.close", "volume", "jdk_rs55", "jdk_momratio55"), ]
ami001 <- ami001 [, ref.date := ymd(ref.date), ]
fwrite(ami001, "D:\\My-Shares\\analysis\\amibroker_nifty_small100.csv")


#
# https://www.niftyindices.com/Methodology/Method_NIFTY_Equity_Indices.pdf
# Page 90 for the momentum index calculations
#

ami001 <- ami001 [, perc_chg := ( price.close - shift(price.close, type = c("lag"), n = 1 ) / shift(price.close, type = c("lag"), n = 1 ) * 100),  ]
ami001 <- ami001 [, nrow_tick := .N, by = .(ticker) ]

ami002 <- ami001 [ !is.na(perc_chg) & nrow_tick >= 252]
ami002 <- ami002 [, std_chg252 := runSD (perc_chg, 252), by = .(ticker)]
ami002 <- ami002 [, price_chg252 := (price.close / shift(price.close, type = c("lag"), n = 252 )) -1 , by = .(ticker)]
ami002 <- ami002 [, price_chg126 := (price.close / shift(price.close, type = c("lag"), n = 126 )) -1, by = .(ticker)]

ami002 <- ami002 [, momentum_ratio252 := price_chg252 / std_chg252, ]
ami002 <- ami002 [, momentum_ratio126 := price_chg126 / std_chg252, ]

# Calculate the mean and sd for each day across all the stocks
# This calculation should provide the yearly variations and means
ami003 <- ami002 [ !is.na(momentum_ratio252)]

ami003 <- ami003 [, overall_mean126 := mean(momentum_ratio126), by =.(ref.date) ]
ami003 <- ami003 [, overall_sd126 := sd(momentum_ratio126), by =.(ref.date) ]

ami003 <- ami003 [, overall_mean252 := mean(momentum_ratio252), by =.(ref.date) ]
ami003 <- ami003 [, overall_sd252 := sd(momentum_ratio252), by =.(ref.date) ]

# Standardized score for 6 and 12 months:
ami003 <- ami003 [, z252 := (momentum_ratio252 - overall_mean252) / overall_sd252, ]
ami003 <- ami003 [, z126 := (momentum_ratio126 - overall_mean126) / overall_sd126, ]

# Weighted average z score 50% wgt for both
ami003 <- ami003 [, z_wgt := 0.5 * z252 + 0.5 * z126, ]
ami003 <- ami003 [, z_wgt_std := ifelse(z_wgt >= 0, 1 + z_wgt, 1/(1 - z_wgt) ), ]

# Sort the data
ami003 <- ami003 [ order(ref.date, -z_wgt_std), ]
ami003 <- ami003 [, z_rank := 1:.N, by =.(ref.date)]

ami004 <- dcast(data = ami003 [ ref.date <= "2021-01-01" & ref.date <= "2020-09-12"],
                z_rank ~ ref.date,
                value.var = c("ticker"))

ami005 <- ami004 [ z_rank <= 10]
