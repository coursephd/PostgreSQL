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

options(scipen = 999)

library(BatchGetSymbols)

future.seed = TRUE
options(future.rng.onMisuse="ignore")

future::plan(future::multisession, workers = floor(parallel::detectCores()/2 ))

#
# Get the average wgt of each bank:
#

all02_pdf <- readRDS("D:\\My-Shares\\source-index-wgt-bnfpdf\\0504_bnf_idx_wgt.rds")
all03_pdf <- all02_pdf [,  .(avgwgt = mean(Weightage)), by =.(monyr, Symbol)]
all03_pdf <- all03_pdf [ order(monyr, -avgwgt, Symbol)]
all03_pdf <- all03_pdf [, rank := 1:.N, by = (monyr)]
all03_pdf <- all03_pdf [, cumperc := cumsum(avgwgt), by =.(monyr)]
all03_pdf <- all03_pdf [, ticker := paste(Symbol, ".NS", sep=""), ]

unq <- unique(all03_pdf$ticker)

# banknifty data from yahoo
a01bnk <-  BatchGetSymbols(
  tickers = "^NSEBANK",
  first.date = "2016-01-01",
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
  do.parallel = TRUE, # FALSE
  be.quiet = FALSE
)

a02bnk <- data.table(a01bnk$df.tickers)
setnames(a02bnk, paste("BNF", names(a02bnk), sep = "_"))

#
# Get only the banks which appear in the unq dataset:
#
a01 <-  BatchGetSymbols(
  tickers = unq,
  first.date = "2016-01-01",
  last.date = Sys.Date(),
  thresh.bad.data = 0.1,
  bench.ticker = "^NSEI",
  type.return = "arit",
  freq.data = "daily",
  how.to.aggregate = "last",
  do.complete.data = FALSE,
  do.fill.missing.prices = TRUE,
  do.cache = TRUE,
  cache.folder = file.path(tempdir(), "BGS_Cache"),
  do.parallel = TRUE, # FALSE
  be.quiet = FALSE
)

a01con <- data.table(a01$df.control)
a02 <- data.table(a01$df.tickers)

bnf0001 <- a02 [, monyr := substr (ref.date, 1, 7),]

# Merge the data with remaining data

bnf0001 <- merge(x = bnf0001,
                 y = a02bnk, 
                 by.x = c("ref.date"),
                 by.y = c("BNF_ref.date"),
                 all.x = TRUE)

# Merge the data with % wgt

bnf0001 <- merge(x = bnf0001,
                 y = all03_pdf, 
                 by = c("ticker", "monyr"),
                 all.x = TRUE)

bnf0001 <- bnf0001 [, `:=` (lowhigh = price.high - price.low,
                            perchg = round( (price.close - price.open)/ price.open * 100, 1),
                            bnfperchg = (BNF_price.close - BNF_price.open)/ BNF_price.open * 100,
                            bnf_chg = BNF_price.close - BNF_price.open,
                            bnf_cat = ifelse(rank <=3, "01 Top 3 banks", "02 Other banks")) , ]

###########################################################################
#
# Calculate the number of points changed due to a company
# As every min data is not present, calculate the same once with Open
# and once with close values
#
# This could be done with low / high values as well
#
# Calculate the cumulative sum by Top 3 / remaining banks
# 
###########################################################################
bnf0001 <- bnf0001 [, `:=` (pnt_open_chg = round(avgwgt/100 * perchg/100 * BNF_price.open, 1),  
                            pnt_close_chg = round(avgwgt/100 * perchg/100 * BNF_price.close, 1) ), ]

bnf0001 <- bnf0001 [, `:=` (pnt_open_chg_bnfcat = sum(pnt_open_chg), 
                            pnt_close_chg_bnfcat = sum(pnt_close_chg)), by = .(ref.date, bnf_cat)]

bnf0001unq <- na.omit( unique( bnf0001 [ , c("monyr", "ref.date", "bnf_cat", "pnt_open_chg_bnfcat", "pnt_close_chg_bnfcat", "bnf_chg"), ] ) )

bnf0001unq02 <- dcast(data = bnf0001unq,
                      monyr + ref.date + bnf_chg ~ paste("Cat_", str_remove_all(bnf_cat, " "), sep=""), 
                      value.var =  c("pnt_open_chg_bnfcat", "pnt_close_chg_bnfcat"))  

# Create scenarios:

bnf0001unq02 <- bnf0001unq02 [, grpvar := case_when(bnf_chg >= 0 & pnt_open_chg_bnfcat_Cat_01Top3banks >= 0 & pnt_open_chg_bnfcat_Cat_02Otherbanks >= 0 ~ "01 all +ve",
                                                    bnf_chg <= 0 & pnt_open_chg_bnfcat_Cat_01Top3banks <= 0 & pnt_open_chg_bnfcat_Cat_02Otherbanks <= 0 ~ "02 all -ve",
                                                    bnf_chg >= 0 & pnt_open_chg_bnfcat_Cat_01Top3banks >= 0 & pnt_open_chg_bnfcat_Cat_02Otherbanks <  0 ~ "03 BNF/Top3 +ve, rest -ve",
                                                    bnf_chg >= 0 & pnt_open_chg_bnfcat_Cat_01Top3banks <  0 & pnt_open_chg_bnfcat_Cat_02Otherbanks >= 0 ~ "04 BNF/Rest +ve, Top3 -ve",
                                                    bnf_chg <  0 & pnt_open_chg_bnfcat_Cat_01Top3banks >= 0 & pnt_open_chg_bnfcat_Cat_02Otherbanks >= 0 ~ "05 Top3/Rest +ve, BNF -ve",
                                                    bnf_chg <  0 & pnt_open_chg_bnfcat_Cat_01Top3banks <  0 & pnt_open_chg_bnfcat_Cat_02Otherbanks >= 0 ~ "06 BNF/Top3 -ve, rest +ve", 
                                                    bnf_chg <  0 & pnt_open_chg_bnfcat_Cat_01Top3banks >= 0 & pnt_open_chg_bnfcat_Cat_02Otherbanks <  0 ~ "06 BNF/Rest -ve, Top3 +ve", ), ]


write.xlsx(bnf0001unq02, "D:\\My-Shares\\analysis\\0504_bnf_points.xlsx")
