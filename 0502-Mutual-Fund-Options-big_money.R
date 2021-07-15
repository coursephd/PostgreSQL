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
eval(parse(text = step002$var13exec))

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
all01 <- all01 [, over_mn := sum( Value_Mn * multiply), by =.(stocks)]

# Overall inflows in a particular stock in the last month
all01 <- all01 [, overall_lstmnth := sum(quantity02), by =.(stocks, chg_mnth)]
all01 <- all01 [, over_mn_lstmnth := sum( Value_Mn * multiply), by =.(stocks, chg_mnth)]

# Compare the overall inflows in % for the last month
all01 <- all01 [, perc_inflow := round(over_mn_lstmnth / over_mn * 100, 2), ]

all01 <- all01 [ order(-over_mn, -overall, -quantity02)]

all01 <- all01 [, -c("quantity", "unit"), ]
all02 <- unique( all01 [ overall >0 & quantity02 > 0, c("stocks", "overall", "unit02", "over_mn", "over_mn_lstmnth", "perc_inflow"), ])

all02 <- all02 [order (-perc_inflow, -over_mn_lstmnth)]
