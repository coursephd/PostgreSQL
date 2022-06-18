# High low plot using tableau
# https://www.youtube.com/watch?v=aQA4KiPXDdw

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

# Read the data from the permenant dataset
out <- as.data.table( readRDS ("D:/My-Shares/analysis/bhavvopy_all.rds") )

out2 <- out [SYMBOL %in% c("NIFTY", "BANKNIFTY")]

fut <- out [INSTRUMENT %in% c("FUTIDX") & SYMBOL %in% c("NIFTY", "BANKNIFTY") & OPTION_TYP == "XX"]
opt <- out [INSTRUMENT %in% c("OPTIDX") & SYMBOL %in% c("NIFTY", "BANKNIFTY") & OPTION_TYP %in% c("CE", "PE") & CONTRACTS >= 500]


# create a numeric date variable and expiry date
fut <- fut [, `:=` (trday = anydate(TIMESTAMP), nexpday = anydate(EXPIRY_DT)), ]
opt <- opt [, `:=` (trday = anydate(TIMESTAMP), nexpday = anydate(EXPIRY_DT)), ]

# Merge the future prices along with the options prices

opt02 <- opt [, c("INSTRUMENT", "SYMBOL", "STRIKE_PR", "trday", "nexpday", "EXPIRY_DT", "OPEN", "CLOSE", "HIGH", "LOW", "CONTRACTS", "TIMESTAMP", "OPTION_TYP"), ]
fut02 <- fut [, c("INSTRUMENT", "SYMBOL", "trday", "nexpday", "EXPIRY_DT", "OPEN", "CLOSE", "HIGH", "LOW", "SETTLE_PR", "TIMESTAMP"), ]

setnames(fut02, "INSTRUMENT", "FUT")
setnames(fut02, "OPEN", "OPENFUT")
setnames(fut02, "CLOSE", "CLOSEFUT")
setnames(fut02, "HIGH", "HIGHFUT")
setnames(fut02, "LOW", "LOWFUT")

opt02 <- opt02 [, mrgdt := substr(EXPIRY_DT, 4, length(EXPIRY_DT)), ]
fut02 <- fut02 [, mrgdt := substr(EXPIRY_DT, 4, length(EXPIRY_DT)), ]

opt03 <- merge(x = opt02,
               y = fut02 [, -c("EXPIRY_DT"),],
               by = c("SYMBOL", "trday", "nexpday", "mrgdt", "TIMESTAMP"),
               all = TRUE)

saveRDS (opt03, "D:/My-Shares/analysis/0103bhavvopy_nf_bnf.rds")
fwrite(opt03, "D:/My-Shares/analysis/0103bhavvopy_nf_bnf.csv")


opt020 <- opt [, c("INSTRUMENT", "SYMBOL", "trday", "nexpday", "EXPIRY_DT", "OPEN", "CLOSE", "HIGH", "LOW", "SETTLE_PR", "CONTRACTS", "TIMESTAMP", "OPTION_TYP", "STRIKE_PR"), ]
fut020 <- fut [, c("INSTRUMENT", "SYMBOL", "trday", "nexpday", "EXPIRY_DT", "OPEN", "CLOSE", "HIGH", "LOW", "SETTLE_PR", "CONTRACTS", "TIMESTAMP", "OPTION_TYP", "STRIKE_PR"), ]

opt030 <- rbind(opt020, fut020)

saveRDS (opt030, "D:/My-Shares/analysis/0103bhavvopy_nf_bnf_vert.rds")
fwrite(opt030, "D:/My-Shares/analysis/0103bhavvopy_nf_bnf_vert.csv")

#####################################################################################
# End of program
#####################################################################################
