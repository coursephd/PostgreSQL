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

#################################################################################
#
# Get the list of stock with the industry names from BSE website
# https://www.bseindia.com/corporates/List_Scrips.html
# Name the file as 001_bse.xlsx
#
# To avoid programming problems an additional comma is added to the Column row
#
#################################################################################

step001 <- fread("D:\\My-Shares\\analysis\\001_bse.csv")
setnames(x=step001, old=names(step001), new=gsub(" ","",names(step001)))

# Subset only the active companies and equity
step002 <- step001 [Status == "Active" & Instrument == "Equity" & Group != "Z"]


#################################################################################
#
# Get the company sorted by the Large, Mid and small caps from the AMFI website
# 
# https://www.amfiindia.com/research-information/other-data/categorization-of-stocks
#
# Name the file 002_Average Market Capitalization of List Companies during Jan-June 2021
#
#################################################################################

step100 <- data.table ( read.xlsx("D:\\My-Shares\\analysis\\002_Average Market Capitalization of List Companies during Jan-June 2021.xlsx", 
                                  sheet = 1, 
                                  startRow = 2) )

step200 <- merge (x = step002 [! is.na(ISINNo)],
                  y = step100 [! is.na(ISIN)],
                  by.x =c("ISINNo"),
                  by.y =c("ISIN"),
                  all.x = TRUE,
                  all.y = TRUE)

step201 <- step200 [ ! is.na(`Categorization.as.per.SEBI.Circular.dated.Oct.6,.2017`) ]
step201 <- step201 [ ! is.na(Industry) ]


# Calculate number of companies in each industry and total amount of market size
step201 <- step201 [, numcompany := uniqueN(ISINNo), by =.(Industry)]
step201 <- step201 [, mktsize_ind := sum(`Average.of.All.Exchanges.(Rs..Cr.)`), by =.(Industry)]


step300 <- unique( step201 [, c("Industry", "numcompany", "mktsize_ind"), ])
