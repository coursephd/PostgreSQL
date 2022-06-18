####################################################################
#
# A program to check the BIG money move from the Mutual funds
#
# Go through the Money control site
# Get the small cap funds and see where is the money flowing
# Add the change in the quantity and number of shares company names 
#
# Could this provide us inflows and outflows
#
# This program is currently looking at only small cap
# But need to add other funds to get the overall inputs and exits
#
# A write-up about FPI:
# https://squareoff.in/tracking-fii-investment-in-indian-stock-market/
#
#####################################################################
library(TTR)
library(tidyquant)
library(QuantTools)
library(derivmkts)
library(quantmod)
library(openxlsx)
library(data.table)
library(tidyverse)
library(anytime)
library(simstudy)
library(zoo)
library(RCurl)
library(lubridate)
library(curl)
library(reshape)
library(ggplot2)
library(plotly)

options(scipen = 999)

library(BatchGetSymbols)

future.seed = TRUE
options(future.rng.onMisuse="ignore")

future::plan(future::multisession, workers = floor(parallel::detectCores()/2 ))

step001 <- data.table ( read.xlsx("D:\\My-Shares\\prgm\\0502-Mutual-Fund-Options.xlsx", 3) )
step002 <- step001 [, var13exec := str_replace_all(var12exec, "&gt;", ">"), ]
step002 <- step002 [ var05 != 25 ]
eval(parse(text = step002 [var00 == "Small"]$var13exec))

all01 <- rbindlist(mget(ls(pattern = "url_tbl")), fill = TRUE, idcol = "url_tbl")
setnames(x=all01, old=names(all01), new=gsub(" ","_", names(all01)))
setnames(x=all01, old=names(all01), new=gsub("\\(", "_", names(all01)))
setnames(x=all01, old=names(all01), new=gsub("\\)", "", names(all01)))
setnames(x=all01, old=names(all01), new=gsub("-", "", names(all01)))

#rm(list = ls( pattern = "^url") )

all01 <- unique(all01)

all01 <- all01 [`%_of_Total_Holdings` != "0.00%"]
all01 <- all01 [, Value_Mn := as.numeric(Value_Mn), ]
all01 <- all01 [, stocks := case_when( substr(Stock_Invested_in, 1, 1) == "-" ~ substr(Stock_Invested_in, 3, 200),
                                       substr(Stock_Invested_in, 1, 1) == "#" ~ substr(Stock_Invested_in, 3, 200), 
                                       TRUE ~ Stock_Invested_in), ]
all01 <- all01 [, stocks := trimws(stocks), ]
all01 <- all01 [, c("quantity", "unit") := tstrsplit(`1M_Change_in_Qty`, " "), ]
all01 <- all01 [, unit := ifelse(is.na(unit), "", unit), ]
all01 <- all01 [, quantity := ifelse(is.na(quantity), 0, quantity), ]

all01 <- all01 [, quantity02 := ifelse(unit == "k", as.numeric(quantity) /100, as.numeric(quantity)), ]
all01 <- all01 [, unit02 := ifelse(unit %in% c("k", ""), "L", unit), ]

all01 <- all01 [, quantity02 := ifelse(unit02 == "Cr", quantity02 * 100, quantity02), ]
all01 <- all01 [, unit02 := ifelse(unit02 %in% c("Cr"), "L", unit02), ]

# Create a category variable to check 0 or > 0 change in the last month
#all01 <- all01 [, chg_mnth := ifelse(quantity02> 0 , "Inflow", "No inflow"), ]
all01 <- all01 [, chg_mnth := case_when(quantity02 > 0 ~ "Inflow",
                                        quantity02 == 0 ~ "No change",
                                        quantity02 < 0 ~ "Outflow" ), ]

# Create a multiplier
all01 <- all01 [, multiply := case_when(quantity02 > 0 ~ 1,
                                        quantity02 == 0 ~ 1,
                                        quantity02 < 0 ~ -1 ), ]

# Add the value in million for each MF to get the overall number:
# Calculate % to check against the % column coming from the source data

all01 <- all01 [, tot_value := sum( Value_Mn ), by =.(url_tbl)]
all01 <- all01 [, tot_perc := round( Value_Mn / tot_value * 100, 2), ]

# Overall inflows in a particular stock
all01 <- all01 [, overall := sum(quantity02), by =.(stocks)]
#all01 <- all01 [, over_mn := sum( Value_Mn * multiply), by =.(stocks)]
# See if the multiplication by -1 should be done or not as % are coming more than 100
all01 <- all01 [, over_mn := sum( Value_Mn ), by =.(stocks)]

# Overall inflows in a particular stock in the last month
all01 <- all01 [, overall_lstmnth := sum(quantity02), by =.(stocks, chg_mnth)]
all01 <- all01 [, over_mn_lstmnth := sum( Value_Mn * multiply), by =.(stocks, chg_mnth)]

# Compare the overall inflows in % for the last month
all01 <- all01 [, perc_inflow := round(over_mn_lstmnth / over_mn * 100, 2), ]

all01 <- all01 [ order(-over_mn, -overall, -quantity02)]

all01 <- all01 [, -c("quantity", "unit"), ]
all02 <- unique( all01 [ overall >0 & quantity02 > 0, c("stocks", "Sector", "overall", "unit02", "over_mn", "over_mn_lstmnth", "perc_inflow"), ])

all02 <- all02 [order (-perc_inflow, -over_mn_lstmnth)]


# Do the same with sector
all01sec <- unique( all01 [, c("Sector", "chg_mnth", "over_mn", "Value_Mn",  
                                "over_mn_lstmnth", "chg_mnth", "multiply",
                                "quantity02", "unit02"), ] )

all01sec <- all01sec [, Sector := trimws(Sector), ]

all01sec <- all01sec [, sec_tot_value := sum(Value_Mn), by = .(Sector)]

# Overall inflows in a particular stock
all01sec <- all01sec [, sec_overall := sum(quantity02), by =.(Sector)]

# See if the multiplication by -1 should be done or not as % are coming more than 100
all01sec <- all01sec [, sec_over_mn := sum( over_mn ), by =.(Sector)]


# Overall inflows in a particular stock in the last month
all01sec <- all01sec [, sec_overall_lstmnth := sum(quantity02), by =.(Sector)]
all01sec <- all01sec [, sec_over_mn_lstmnth := sum( Value_Mn * multiply), by =.(Sector)]

# Compare the overall inflows in % for the last month
all01sec <- all01sec [, sec_perc_inflow := round(sec_over_mn_lstmnth / sec_over_mn * 100, 2), ]

all01sec <- all01sec [ order(-sec_over_mn, -sec_overall, -quantity02)]
all02sec <- unique( all01sec [ , c( "Sector",  "sec_over_mn", "sec_over_mn_lstmnth", "sec_perc_inflow"), ])

all02sec <- all02sec [order (-sec_over_mn_lstmnth, -sec_perc_inflow)]

all02sec <- all02sec [, grand_tot := sum(sec_over_mn_lstmnth),]
all02sec <- all02sec [, grand_perc := round(sec_over_mn_lstmnth / grand_tot * 100, 2), ]

# Merge the stocks and sectors to understand the moving sectors and stocks

all03 <- merge(x = all02,
               y = all02sec, 
               by = c ("Sector"))

################################################
#
# Based on the % delivery and deliery volume
# Try finding BIG money
#
# Multiply the delivery volume % to the hlc3 value 
# This much money is still in the company for each day as the 
# the stock has been taken home
#
# if (curl_fetch_memory('https://www1.nseindia.com/archives/equities/mto/MTO_01012016.DAT')$status_code == 200) {MTO_01012016 = fread('https://www1.nseindia.com/archives/equities/mto/MTO_01012016.DAT');MTO_01012016 = MTO_01012016[, date := 42370,]};
# if (curl_fetch_memory('https://www1.nseindia.com/archives/equities/mto/MTO_02012016.DAT')$status_code == 200) {MTO_02012016 = fread('https://www1.nseindia.com/archives/equities/mto/MTO_02012016.DAT');MTO_02012016 = MTO_02012016[, date := 42371,]};
#
################################################

step001 <- data.table ( read.xlsx("D:\\My-Shares\\prgm\\0500_rakeshpujara_atmlong.xlsx", 2) )
step002 <- step001 [Year4 >= 2019]
step002 <- step002 [, del09 := paste(del08, "print ( anydate(step002$Date0) );", sep =" "), ]

eval(parse(text = step002$del08))

all01_del <- rbindlist(mget(ls(pattern = "MTO")), fill = TRUE, idcol = "Delivery")
all01_del <- all01_del [, ref.date := dmy ( substr(Delivery, 5, 20) ), ]
all01_del <- all01_del [ `Name of Security` == "EQ"]

setnames(all01_del, "Sr No", "ticker")
setnames(all01_del, "Deliverable Quantity(gross across client level)", "del_vol")
setnames()

all01_del <- all01_del [, ticker := paste(ticker, ".NS", sep=""), ]

saveRDS (all01_del, "D:\\My-Shares\\analysis\\0502_delivery_volume.rds")

rm(list = ls( pattern = "^MTO") )

#
# Only execute the following line when running the program half way through with the assumption that
# the necessary data has been created earlier
# Otherwise comment out the following line
#
all01_del <- readRDS("D:\\My-Shares\\analysis\\0502_delivery_volume.rds")

all02 <- fread("D:\\My-Shares\\analysis\\0504_yahoo_amibroker_all_equity.csv")
all02 <- all02 [, ref.date02 := ymd (ref.date), ]

# Merge the datasets

all03 <- merge(x = all01_del,
               y = all02, 
               by.x = c("ticker", "ref.date"),
               by.y = c("ticker", "ref.date02"),
               all.x = TRUE)

#
# Calculate the hlc3 value: a replacement for possible VWAP value
# 
all03 <- all03 [, hlc3 := (price.high + price.low + price.close) /3, ]
all03 <- all03 [, in_out := ifelse(price.open > price.close, -1, 1), ]
all03 <- all03 [, price_del := del_vol * hlc3 * in_out, ]
all03 <- all03 [, price_intra := (volume - del_vol) * hlc3 * in_out, ]
all03 <- all03 [, nrow_tick := .N, by = .(ticker) ]

all04 <- all03 [ !is.na(price_del) & nrow_tick >= 15]
all05 <- all04 [, pric_del_cum15 := runSum(price_del, 15), by = .(ticker)]
