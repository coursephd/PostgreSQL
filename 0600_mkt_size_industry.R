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
library(Rcrawler)

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
# Get the data from Money control for the mutual funds
# (1) Get the primary link for each MF
# (2) Extract all the links from each page
# (3) Subset "https://www.moneycontrol.com/mutual-funds/nav/" specific links
# (4) Get the additional calculations for full portfolio
#
#################################################################################


if (curl_fetch_memory("https://www.moneycontrol.com/mutual-funds/amc-details/BS")$status_code == 200) chk00 <- data.table( LinkExtractor("https://www.moneycontrol.com/mutual-funds/amc-details/BS")$InternalLinks )
if (curl_fetch_memory("https://www.moneycontrol.com/mutual-funds/amc-details/AA")$status_code == 200) chk01 <- data.table( LinkExtractor("https://www.moneycontrol.com/mutual-funds/amc-details/AA")$InternalLinks )
if (curl_fetch_memory("https://www.moneycontrol.com/mutual-funds/amc-details/BO")$status_code == 200) chk02 <- data.table( LinkExtractor("https://www.moneycontrol.com/mutual-funds/amc-details/BO")$InternalLinks )
if (curl_fetch_memory("https://www.moneycontrol.com/mutual-funds/amc-details/AB")$status_code == 200) chk03 <- data.table( LinkExtractor("https://www.moneycontrol.com/mutual-funds/amc-details/AB")$InternalLinks )
if (curl_fetch_memory("https://www.moneycontrol.com/mutual-funds/amc-details/BA")$status_code == 200) chk04 <- data.table( LinkExtractor("https://www.moneycontrol.com/mutual-funds/amc-details/BA")$InternalLinks )
if (curl_fetch_memory("https://www.moneycontrol.com/mutual-funds/amc-details/CA")$status_code == 200) chk05 <- data.table( LinkExtractor("https://www.moneycontrol.com/mutual-funds/amc-details/CA")$InternalLinks )
if (curl_fetch_memory("https://www.moneycontrol.com/mutual-funds/amc-details/PA")$status_code == 200) chk06 <- data.table( LinkExtractor("https://www.moneycontrol.com/mutual-funds/amc-details/PA")$InternalLinks )
if (curl_fetch_memory("https://www.moneycontrol.com/mutual-funds/amc-details/DS")$status_code == 200) chk07 <- data.table( LinkExtractor("https://www.moneycontrol.com/mutual-funds/amc-details/DS")$InternalLinks )
if (curl_fetch_memory("https://www.moneycontrol.com/mutual-funds/amc-details/EW")$status_code == 200) chk08 <- data.table( LinkExtractor("https://www.moneycontrol.com/mutual-funds/amc-details/EW")$InternalLinks )
if (curl_fetch_memory("https://www.moneycontrol.com/mutual-funds/amc-details/PM")$status_code == 200) chk09 <- data.table( LinkExtractor("https://www.moneycontrol.com/mutual-funds/amc-details/PM")$InternalLinks )
if (curl_fetch_memory("https://www.moneycontrol.com/mutual-funds/amc-details/TE")$status_code == 200) chk10 <- data.table( LinkExtractor("https://www.moneycontrol.com/mutual-funds/amc-details/TE")$InternalLinks )
if (curl_fetch_memory("https://www.moneycontrol.com/mutual-funds/amc-details/HD")$status_code == 200) chk11 <- data.table( LinkExtractor("https://www.moneycontrol.com/mutual-funds/amc-details/HD")$InternalLinks )
if (curl_fetch_memory("https://www.moneycontrol.com/mutual-funds/amc-details/HS")$status_code == 200) chk12 <- data.table( LinkExtractor("https://www.moneycontrol.com/mutual-funds/amc-details/HS")$InternalLinks )
if (curl_fetch_memory("https://www.moneycontrol.com/mutual-funds/amc-details/PI")$status_code == 200) chk13 <- data.table( LinkExtractor("https://www.moneycontrol.com/mutual-funds/amc-details/PI")$InternalLinks )
if (curl_fetch_memory("https://www.moneycontrol.com/mutual-funds/amc-details/IB")$status_code == 200) chk14 <- data.table( LinkExtractor("https://www.moneycontrol.com/mutual-funds/amc-details/IB")$InternalLinks )
if (curl_fetch_memory("https://www.moneycontrol.com/mutual-funds/amc-details/AG")$status_code == 200) chk15 <- data.table( LinkExtractor("https://www.moneycontrol.com/mutual-funds/amc-details/AG")$InternalLinks )
if (curl_fetch_memory("https://www.moneycontrol.com/mutual-funds/amc-details/II")$status_code == 200) chk16 <- data.table( LinkExtractor("https://www.moneycontrol.com/mutual-funds/amc-details/II")$InternalLinks )
if (curl_fetch_memory("https://www.moneycontrol.com/mutual-funds/amc-details/IM")$status_code == 200) chk17 <- data.table( LinkExtractor("https://www.moneycontrol.com/mutual-funds/amc-details/IM")$InternalLinks )
if (curl_fetch_memory("https://www.moneycontrol.com/mutual-funds/amc-details/LI")$status_code == 200) chk18 <- data.table( LinkExtractor("https://www.moneycontrol.com/mutual-funds/amc-details/LI")$InternalLinks )
if (curl_fetch_memory("https://www.moneycontrol.com/mutual-funds/amc-details/JM")$status_code == 200) chk19 <- data.table( LinkExtractor("https://www.moneycontrol.com/mutual-funds/amc-details/JM")$InternalLinks )
if (curl_fetch_memory("https://www.moneycontrol.com/mutual-funds/amc-details/KM")$status_code == 200) chk20 <- data.table( LinkExtractor("https://www.moneycontrol.com/mutual-funds/amc-details/KM")$InternalLinks )
if (curl_fetch_memory("https://www.moneycontrol.com/mutual-funds/amc-details/CC")$status_code == 200) chk21 <- data.table( LinkExtractor("https://www.moneycontrol.com/mutual-funds/amc-details/CC")$InternalLinks )
if (curl_fetch_memory("https://www.moneycontrol.com/mutual-funds/amc-details/JB")$status_code == 200) chk22 <- data.table( LinkExtractor("https://www.moneycontrol.com/mutual-funds/amc-details/JB")$InternalLinks )                                                                                            
if (curl_fetch_memory("https://www.moneycontrol.com/mutual-funds/amc-details/MH")$status_code == 200) chk23 <- data.table( LinkExtractor("https://www.moneycontrol.com/mutual-funds/amc-details/MH")$InternalLinks )
if (curl_fetch_memory("https://www.moneycontrol.com/mutual-funds/amc-details/MA")$status_code == 200) chk24 <- data.table( LinkExtractor("https://www.moneycontrol.com/mutual-funds/amc-details/MA")$InternalLinks )
if (curl_fetch_memory("https://www.moneycontrol.com/mutual-funds/amc-details/MO")$status_code == 200) chk25 <- data.table( LinkExtractor("https://www.moneycontrol.com/mutual-funds/amc-details/MO")$InternalLinks )
if (curl_fetch_memory("https://www.moneycontrol.com/mutual-funds/amc-details/PP")$status_code == 200) chk26 <- data.table( LinkExtractor("https://www.moneycontrol.com/mutual-funds/amc-details/PP")$InternalLinks )
if (curl_fetch_memory("https://www.moneycontrol.com/mutual-funds/amc-details/ID")$status_code == 200) chk27 <- data.table( LinkExtractor("https://www.moneycontrol.com/mutual-funds/amc-details/ID")$InternalLinks )
if (curl_fetch_memory("https://www.moneycontrol.com/mutual-funds/amc-details/ES")$status_code == 200) chk28 <- data.table( LinkExtractor("https://www.moneycontrol.com/mutual-funds/amc-details/ES")$InternalLinks )
if (curl_fetch_memory("https://www.moneycontrol.com/mutual-funds/amc-details/QU")$status_code == 200) chk29 <- data.table( LinkExtractor("https://www.moneycontrol.com/mutual-funds/amc-details/QU")$InternalLinks )
if (curl_fetch_memory("https://www.moneycontrol.com/mutual-funds/amc-details/RC")$status_code == 200) chk30 <- data.table( LinkExtractor("https://www.moneycontrol.com/mutual-funds/amc-details/RC")$InternalLinks )
if (curl_fetch_memory("https://www.moneycontrol.com/mutual-funds/amc-details/FI")$status_code == 200) chk31 <- data.table( LinkExtractor("https://www.moneycontrol.com/mutual-funds/amc-details/FI")$InternalLinks )
if (curl_fetch_memory("https://www.moneycontrol.com/mutual-funds/amc-details/SB")$status_code == 200) chk32 <- data.table( LinkExtractor("https://www.moneycontrol.com/mutual-funds/amc-details/SB")$InternalLinks )
if (curl_fetch_memory("https://www.moneycontrol.com/mutual-funds/amc-details/SR")$status_code == 200) chk33 <- data.table( LinkExtractor("https://www.moneycontrol.com/mutual-funds/amc-details/SR")$InternalLinks )
if (curl_fetch_memory("https://www.moneycontrol.com/mutual-funds/amc-details/SM")$status_code == 200) chk34 <- data.table( LinkExtractor("https://www.moneycontrol.com/mutual-funds/amc-details/SM")$InternalLinks )
if (curl_fetch_memory("https://www.moneycontrol.com/mutual-funds/amc-details/SN")$status_code == 200) chk35 <- data.table( LinkExtractor("https://www.moneycontrol.com/mutual-funds/amc-details/SN")$InternalLinks )
if (curl_fetch_memory("https://www.moneycontrol.com/mutual-funds/amc-details/TA")$status_code == 200) chk36 <- data.table( LinkExtractor("https://www.moneycontrol.com/mutual-funds/amc-details/TA")$InternalLinks )
if (curl_fetch_memory("https://www.moneycontrol.com/mutual-funds/amc-details/UK")$status_code == 200) chk37 <- data.table( LinkExtractor("https://www.moneycontrol.com/mutual-funds/amc-details/UK")$InternalLinks )
if (curl_fetch_memory("https://www.moneycontrol.com/mutual-funds/amc-details/UT")$status_code == 200) chk38 <- data.table( LinkExtractor("https://www.moneycontrol.com/mutual-funds/amc-details/UT")$InternalLinks )
if (curl_fetch_memory("https://www.moneycontrol.com/mutual-funds/amc-details/YM")$status_code == 200) chk39 <- data.table( LinkExtractor("https://www.moneycontrol.com/mutual-funds/amc-details/YM")$InternalLinks )
if (curl_fetch_memory("https://www.moneycontrol.com/mutual-funds/amc-details/IT")$status_code == 200) chk40 <- data.table( LinkExtractor("https://www.moneycontrol.com/mutual-funds/amc-details/IT")$InternalLinks )

all_chk01 <- rbindlist(mget(ls(pattern = "chk")), fill = TRUE, idcol = "file_chk")
rm(list = ls( pattern = "^chk") )

all_chk02 <- all_chk01 [ V1 %like% c("https://www.moneycontrol.com/mutual-funds/nav/")]

all01 <- all_chk01

setnames(x=all01, old=names(all01), new=gsub(" ","",names(all01)))

all01 <- all01 [, nrow := 1:.N, by = .(AnchorText)]
all02 <- all01 [ nrow == 1]
all02 <- all02 [, totrow := 1:.N, ]
all02 <- all02 [, c("c1", "c2", "c3", "c4", "c5", "c6", "c7") := tstrsplit(Link, "/", fixed = TRUE), ]#str_locate_all(Link, "/"), ]
all02 <- all02 [, link00 := paste(c1, "//", c3, "/", c4, "/", c6, "/portfolio-overview/", c7, sep=""), ]
all02 <- all02 [, step001 := paste("url", totrow, sep ="" ), ]
all02 <- all02 [, step001a := paste("url_tbl", totrow, sep ="" ), ]
all02 <- all02 [, step001b := paste("act_tbl", totrow, sep ="" ), ]
all02 <- all02 [, step002 := paste(step001, " = '", link00, "';", sep ="" ), ]
all02 <- all02 [, step002a := paste("if (curl_fetch_memory('", link00, "')$status_code == 200) {", sep=""), ]
all02 <- all02 [, step003 := paste(step001, " = read_html(", step001, ");", sep ="" ), ]
all02 <- all02 [, step004 := paste(step001a, " = ", step001, " %>% html_nodes('table') %>% html_table (fill = TRUE);", sep = "")]
all02 <- all02 [, step005 := paste("if (length(", step001a, ") > 5) ", step001b, " = data.table(", step001a, "[[5]] );", step="" ), ]
all02 <- all02 [, step006 := paste("print (", totrow, ");}", sep= ""), ]
all02 <- all02 [, step007 := paste(step002, step002a, step003, step004, step005, step006, sep = " "), ]

eval(parse(text = all02$step007 ))

rm(list = ls( pattern = "^url") )

all01act <- rbindlist(mget(ls(pattern = "act")), fill = TRUE, idcol = "file_act")
saveRDS(all01act, "D:\\My-Shares\\analysis\\0600_mkt_size_industry.rds")
rm(list = ls( pattern = "^act") )


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
all02 <- all02 [, step001b := paste("act_tbl", totrow, sep ="" ), ]
all02 <- all02 [, step002 := paste(step001, " = '", link00, "';", sep ="" ), ]
all02 <- all02 [, step002a := paste("if (curl_fetch_memory('", link00, "')$status_code == 200) {", sep=""), ]
all02 <- all02 [, step003 := paste(step001, " = read_html(", step001, ");", sep ="" ), ]
all02 <- all02 [, step004 := paste(step001a, " = ", step001, " %>% html_nodes('table') %>% html_table (fill = TRUE);", sep = "")]
all02 <- all02 [, step005 := paste("if (length(", step001a, ") > 5) ", step001b, " = data.table(", step001a, "[[5]] );", step="" ), ]
all02 <- all02 [, step006 := paste("print (", totrow, ");}", sep= ""), ]
all02 <- all02 [, step007 := paste(step002, step002a, step003, step004, step005, step006, sep = " "), ]

eval(parse(text = all02$step007 ))

rm(list = ls( pattern = "^url") )

all01act <- rbindlist(mget(ls(pattern = "act")), fill = TRUE, idcol = "file_act")
saveRDS(all01act, "D:\\My-Shares\\analysis\\0600_mkt_size_industry.rds")
rm(list = ls( pattern = "^act") )

all01 <- all01act
all01 <- all01 [, url_tbl := file_act, ]

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

