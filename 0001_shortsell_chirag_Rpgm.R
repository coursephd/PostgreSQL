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

future::plan(future::multisession, workers = floor(parallel::detectCores() ))


step001 <- data.table ( read.xlsx("D:\\My-Shares\\Short-Sell-Chirag-Jain-Maths-teacher\\0001_shortsell_chirag.xlsx", 1) )
step001 <- step001 [, trdate := anydate ( as.Date( Date0, origin = "1899-12-30" ) ), ]

step002 <- step001 [ trdate >= "2021-10-05"]

eval(parse(text = step002$allsteps))
# eval(parse(text = step002$unzip_excel)) # This was needed to unzip files as the pathname was incorrect

# Use the Github bash and use the xlsx2csv command:
# Be at the folder: /d/My-Shares/source-fno-csv
ls -lt *.xlsx|head -n 20|grep "Oct 24"|awk '{print $9}'|tr -s "\\." " "|awk '{print "xlsx2csv", $1 ".xlsx >", $1 ".csv"}'|sh


# Run the grep commands from the excel file to create TXT files
# BankNifty-Expiries-2018-2019-2020-2021.xlsx
# Combine the txt files into a dataset

list_of_files <- list.files(path = "D:/My-Shares/Short-Sell-Chirag-Jain-Maths-teacher/analysis", recursive = TRUE,
                            pattern = "\\.txt", 
                            full.names = TRUE)

# Read all the files and create a FileName column to store filenames
DT <- rbindlist(sapply(list_of_files, fread, simplify = FALSE),
                use.names = TRUE, idcol = "FileName")

DT <- DT [, `:=` (trdate = anydate(V2), 
                  expdate = anydate(V4)),  ]

DT <- DT [, nrow := 1:.N, by =.(FileName)]
DT <- DT [, ngrp := uniqueN(trdate), by =.(FileName)]
DT <- DT [, ngrp02 := sequence(.N), by =.(FileName, trdate)]

setnames(DT, "V5", "strike" )
setnames(DT, "V6", "callput")
setnames(DT, "V7", "open")
setnames(DT, "V8", "high")
setnames(DT, "V9", "low")
setnames(DT, "V10", "close")

dt02 <- melt(data = DT, 
             measure.vars = c("open", "high", "low", "close"), 
             variable.name = "ohlc")

dt02 <- dt02 [, ohlc02 := case_when( callput == "CE" & ohlc == "high" ~ "hl", 
                                     callput == "PE" & ohlc == "low" ~ "hl",
                                     callput == "CE" & ohlc == "low" ~ "lh", 
                                     callput == "PE" & ohlc == "high" ~ "lh",
                                     TRUE ~ as.character(ohlc)) , ]
 
dt02 <- dt02 [, sumprm := sum(value), by =.(FileName, trdate, ohlc02, ngrp)]

# Find the originally sold premium
dt03 <- unique(dt02 [ nrow ==1 & ohlc02 == "close" , c("FileName", "sumprm"), ] )
setnames(dt03, "sumprm", "org_sell")

dt04 <- merge (x = dt02, 
               y = dt03, 
               by = c("FileName"))

#####################################################################
# Subtract the premium from the originally sold premium
# If the subsequent premium is less than original then in profit
#
# diff_prm > 0 is good
#####################################################################

dt04 <- dt04 [, diff_prm := org_sell - sumprm, ]
dt04 <- dt04 [, diff_prm_perc := round( diff_prm / org_sell * 100, 2), ]


dt05 <- unique (dt04 [, c("FileName", "trdate", "expdate", "ohlc02", "sumprm", "org_sell", "diff_prm", "diff_prm_perc"), ])

dt06 <- dcast(data = dt05 [!ohlc02 %in%  c("high", "low") ],
               FileName + trdate + expdate ~ ohlc02,
              value.var =c("sumprm", "org_sell", "diff_prm", "diff_prm_perc") )

dt07 <- merge(x = DT, 
              y = dt06 [, -c("org_sell_hl", "org_sell_lh", "org_sell_open"), ],
              by = c("FileName", "trdate", "expdate"))

dt07 <- dt07 [, weekdays := weekdays(trdate), ]
