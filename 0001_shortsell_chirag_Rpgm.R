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
