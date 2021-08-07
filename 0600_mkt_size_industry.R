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
library(stringi)

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

#################################################################################
#
# Get all the names of Mutual funds from AMFI website
# https://portal.amfiindia.com/DownloadSchemeData_Po.aspx?mf=0
# As the file can be directly read into a data.table there is no need to download
# the physical file reducing some effort
#
#################################################################################

mf001 <- fread("https://portal.amfiindia.com/DownloadSchemeData_Po.aspx?mf=0")
setnames(x=mf001, old=names(mf001), new=gsub(" ","",names(mf001)))

mf002 <- unique( mf001 [ SchemeType == "Open Ended", c("AMC", "SchemeName", "SchemeCategory"), ] )
mf002 <- mf002 [ substr(SchemeCategory, 1, 4) != "Debt" & 
                   SchemeCategory != "Other Scheme - FoF Domestic" &
                   substr(SchemeCategory, 1, 12 ) != "Other Scheme"]


df01 <- fread("C:\\Users\\user\\Downloads\\www.moneycontrol.com_6th_Aug_2021.csv")
df02 <- fread("C:\\Users\\user\\Downloads\\www.moneycontrol.com_6th_Aug_2021 (2).csv")
df03 <- fread("C:\\Users\\user\\Downloads\\www.moneycontrol.com_6th_Aug_2021 (3).csv")
df04 <- fread("C:\\Users\\user\\Downloads\\www.moneycontrol.com_6th_Aug_2021 (4).csv")
df05 <- fread("C:\\Users\\user\\Downloads\\www.moneycontrol.com_6th_Aug_2021 (5).csv")
df06 <- fread("C:\\Users\\user\\Downloads\\www.moneycontrol.com_6th_Aug_2021 (6).csv")
df07 <- fread("C:\\Users\\user\\Downloads\\www.moneycontrol.com_6th_Aug_2021 (7).csv")
df08 <- fread("C:\\Users\\user\\Downloads\\www.moneycontrol.com_6th_Aug_2021 (8).csv")
df09 <- fread("C:\\Users\\user\\Downloads\\www.moneycontrol.com_6th_Aug_2021 (9).csv")
df10 <- fread("C:\\Users\\user\\Downloads\\www.moneycontrol.com_6th_Aug_2021 (10).csv")

df11 <- fread("C:\\Users\\user\\Downloads\\www.moneycontrol.com_6th_Aug_2021 (11).csv")
df12 <- fread("C:\\Users\\user\\Downloads\\www.moneycontrol.com_6th_Aug_2021 (12).csv")
df13 <- fread("C:\\Users\\user\\Downloads\\www.moneycontrol.com_6th_Aug_2021 (13).csv")
df14 <- fread("C:\\Users\\user\\Downloads\\www.moneycontrol.com_6th_Aug_2021 (14).csv")
df15 <- fread("C:\\Users\\user\\Downloads\\www.moneycontrol.com_6th_Aug_2021 (15).csv")
df16 <- fread("C:\\Users\\user\\Downloads\\www.moneycontrol.com_6th_Aug_2021 (16).csv")
df17 <- fread("C:\\Users\\user\\Downloads\\www.moneycontrol.com_6th_Aug_2021 (17).csv")
df18 <- fread("C:\\Users\\user\\Downloads\\www.moneycontrol.com_6th_Aug_2021 (18).csv")
df19 <- fread("C:\\Users\\user\\Downloads\\www.moneycontrol.com_6th_Aug_2021 (19).csv")
df20 <- fread("C:\\Users\\user\\Downloads\\www.moneycontrol.com_6th_Aug_2021 (20).csv")

df21 <- fread("C:\\Users\\user\\Downloads\\www.moneycontrol.com_6th_Aug_2021 (21).csv")
df22 <- fread("C:\\Users\\user\\Downloads\\www.moneycontrol.com_6th_Aug_2021 (22).csv")
df23 <- fread("C:\\Users\\user\\Downloads\\www.moneycontrol.com_6th_Aug_2021 (23).csv")
df24 <- fread("C:\\Users\\user\\Downloads\\www.moneycontrol.com_6th_Aug_2021 (24).csv")
df25 <- fread("C:\\Users\\user\\Downloads\\www.moneycontrol.com_6th_Aug_2021 (25).csv")
df26 <- fread("C:\\Users\\user\\Downloads\\www.moneycontrol.com_6th_Aug_2021 (26).csv")
df27 <- fread("C:\\Users\\user\\Downloads\\www.moneycontrol.com_6th_Aug_2021 (27).csv")
df28 <- fread("C:\\Users\\user\\Downloads\\www.moneycontrol.com_6th_Aug_2021 (28).csv")
df29 <- fread("C:\\Users\\user\\Downloads\\www.moneycontrol.com_6th_Aug_2021 (29).csv")
df30 <- fread("C:\\Users\\user\\Downloads\\www.moneycontrol.com_6th_Aug_2021 (30).csv")

df31 <- fread("C:\\Users\\user\\Downloads\\www.moneycontrol.com_6th_Aug_2021 (31).csv")
df32 <- fread("C:\\Users\\user\\Downloads\\www.moneycontrol.com_6th_Aug_2021 (32).csv")
df33 <- fread("C:\\Users\\user\\Downloads\\www.moneycontrol.com_6th_Aug_2021 (33).csv")
df34 <- fread("C:\\Users\\user\\Downloads\\www.moneycontrol.com_6th_Aug_2021 (34).csv")
df35 <- fread("C:\\Users\\user\\Downloads\\www.moneycontrol.com_6th_Aug_2021 (35).csv")
df36 <- fread("C:\\Users\\user\\Downloads\\www.moneycontrol.com_6th_Aug_2021 (36).csv")
df37 <- fread("C:\\Users\\user\\Downloads\\www.moneycontrol.com_6th_Aug_2021 (37).csv")
df38 <- fread("C:\\Users\\user\\Downloads\\www.moneycontrol.com_6th_Aug_2021 (38).csv")
df39 <- fread("C:\\Users\\user\\Downloads\\www.moneycontrol.com_6th_Aug_2021 (39).csv")
df40 <- fread("C:\\Users\\user\\Downloads\\www.moneycontrol.com_6th_Aug_2021 (40).csv")

df41 <- fread("C:\\Users\\user\\Downloads\\www.moneycontrol.com_6th_Aug_2021 (41).csv")

all01 <- rbindlist(mget(ls(pattern = "df")), fill = TRUE, idcol = "file_df")
rm(list = ls( pattern = "^df") )

setnames(x=all01, old=names(all01), new=gsub(" ","",names(all01)))

all01 <- all01 [, nrow := 1:.N, by = .(AnchorText)]
all02 <- all01 [ nrow == 1]
all02 <- all02 [, totrow := 1:.N, ]
all02 <- all02 [, c("c1", "c2", "c3", "c4", "c5", "c6", "c7") := tstrsplit(Link, "/", fixed = TRUE), ]#str_locate_all(Link, "/"), ]
all02 <- all02 [, link00 := paste(c1, "//", c3, "/", c4, "/", c6, "/portfolio-overview/", c7, sep=""), ]
all02 <- all02 [, step001 := paste("url", totrow, sep ="" ), ]
all02 <- all02 [, step001a := paste("url_tbl", totrow, sep ="" ), ]
all02 <- all02 [, step002 := paste(step001, " = '", link00, "';", sep ="" ), ]
all02 <- all02 [, step003 := paste(step001, " = read_html(", step001, ");", sep ="" ), ]
all02 <- all02 [, step004 := paste(step001a, " = ", step001, " %>% html_nodes('table') %>% html_table (fill = TRUE) %>% .[[5]];", sep = "")]
all02 <- all02 [, step005 := paste(step001a, " = data.table(", step001a, ");", step="" ), ]
all02 <- all02 [, step006 := paste("print (", totrow, ");", sep= ""), ]
all02 <- all02 [, step007 := paste(step002, step003, step004, step005, step006, sep = " "), ]

eval(parse(text = all02 [! stri_detect_fixed(tolower(AnchorText), "debt")]$step007 ))

all02 [totrow ==8]$step007
all02 [! stri_detect_fixed(tolower(AnchorText), "debt")]$step007

url8 = 'https://www.moneycontrol.com/mutual-funds/axis-corporate-debt-fund-direct-plan/portfolio-overview/MAA718'; 
url8 = read_html(url8); 
url_tbl8 = url8 %>% html_nodes('table') %>% html_table (fill = TRUE) %>% .[[5]]; 
url_tbl8  = data.table( url_tbl8 );  
print (8);
