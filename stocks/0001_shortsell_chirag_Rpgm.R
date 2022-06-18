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

step002 <- step001 [ trdate >= "2021-10-28"] #[ trdate >= "2021-10-25"]

# Data downloaded till  09-Nov-2021
# Data is available till 09-Nov-2021: after that there is missing data for 3 days

eval(parse(text = step002$allsteps))
# eval(parse(text = step002$unzip_excel)) # This was needed to unzip files as the pathname was incorrect

# Use the Github bash and use the xlsx2csv command:
# Be at the folder: /d/My-Shares/source-fno-csv
ls -lt *.xlsx|head -n 20|grep "Nov 13"|awk '{print $9}'|tr -s "\\." " "|awk '{print "xlsx2csv", $1 ".xlsx >", $1 ".csv"}'|sh

ls -lt *.xlsx|grep "Nov 13"|head -n 20||awk '{print $9}'|tr -s "\\." " "|awk '{print "xlsx2csv", $1 ".xlsx >", $1 ".csv"}'


#####################################################################################################
#
# The program is executed on 2 different time points
# 2:00 PM ATM: the data for 2:00 PM, ATM strike premium is not available.
# Closeing ATM: to understand the real nature of the data, this ATM premiums are closely represented
#
#####################################################################################################

# Run the grep commands from the excel file to create TXT files
# BankNifty-Expiries-2018-2019-2020-2021.xlsx
# Combine the txt files into a dataset

#list_of_files <- list.files(path = "D:/My-Shares/Short-Sell-Chirag-Jain-Maths-teacher/analysis_2pm", recursive = TRUE,
#                            pattern = "\\.txt", 
#                            full.names = TRUE)


list_of_files <- list.files(path = "D:/My-Shares/Short-Sell-Chirag-Jain-Maths-teacher/analysis_close", recursive = TRUE,
                            pattern = "\\.txt", 
                            full.names = TRUE)

# Read all the files and create a FileName column to store filenames
DT <- rbindlist(sapply(list_of_files, fread, simplify = FALSE),
                use.names = TRUE, idcol = "FileName")

DT <- DT [, `:=` (trdate = anydate(V2), 
                  expdate = anydate(V4)),  ]
DT <- unique(DT)

DT <- DT [, nrow := 1:.N, by =.(FileName)]
DT <- DT [, ngrp := uniqueN(trdate), by =.(FileName)]
DT <- DT [, ngrp02 := sequence(.N), by =.(FileName, trdate)]

setnames(DT, "V5", "strike" )
setnames(DT, "V6", "callput")
setnames(DT, "V7", "open")
setnames(DT, "V8", "high")
setnames(DT, "V9", "low")
setnames(DT, "V10", "close")

#################################################################
#
# Remove some unwanted strikes which come into the TXT files
# These would typically have < 2 rows
#
#################################################################
DT <- DT [, n_str_row := max(.N), by = .(FileName, trdate, strike)]

DT <- DT [ n_str_row >= 2]

# Check if there are duplicated values

DT0 <- DT [, dup := length(unique(DT)), by = .(V2, V3, V4, strike, callput, open, high, low, close)]

dt02 <- melt(data = DT, 
             measure.vars = c("open", "high", "low", "close"), 
             variable.name = "ohlc")

dt02 <- dt02 [, ohlc02 := case_when( callput == "CE" & ohlc == "high" ~ "hl", 
                                     callput == "PE" & ohlc == "low" ~ "hl",
                                     callput == "CE" & ohlc == "low" ~ "lh", 
                                     callput == "PE" & ohlc == "high" ~ "lh",
                                     TRUE ~ as.character(ohlc)) , ]
 

dt02 <- dt02 [, sumprm := sum(value), by =.(FileName, trdate, strike, ohlc02, ngrp)]

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

dt04 <- dt04 [, diff_prm := as.numeric(ifelse(nrow >2, org_sell - sumprm, "")), ]
dt04 <- dt04 [, diff_prm_perc := ifelse(nrow >2, round( diff_prm / org_sell * 100, 2), ""), ]


dt05 <- unique (dt04 [, c("FileName", "trdate", "expdate", "ohlc02", "sumprm", "org_sell", "diff_prm", "diff_prm_perc"), ])

dt06 <- dcast(data = dt05 [!ohlc02 %in%  c("high", "low") ],
               FileName + trdate + expdate ~ ohlc02,
              value.var =c("sumprm", "org_sell", "diff_prm", "diff_prm_perc") )

dt07 <- merge(x = DT, 
              y = dt06 [, -c("org_sell_hl", "org_sell_lh", "org_sell_open"), ],
              by = c("FileName", "trdate", "expdate"))

dt07 <- dt07 [, weekdays := weekdays(trdate), ]

dt08 <- dt07 [ nrow > 2 ]
dt08 <- dt08 [, nblck := .GRP, by = .(expdate)]


# Get the calculations to understand how many times, the trades can get out in profit

dt04 <- dt04 [, nblck := .GRP, by = .(expdate)]

# Check for the profitability on each day to get a better idea
# if the % reduction is >= 10 then consider that day as a success
# Count the cumulative success for each type of exit

prft001 <- unique( dt04 [ nrow > 2, c("nblck", "diff_prm_perc", "ohlc02", "trdate", "expdate"), ])
prft001 <- prft001 [, exit := ifelse( as.numeric(diff_prm_perc) >= 9, 1, 0), ]
prft001 <- prft001 [, exit_cum := cumsum(exit), by =.(nblck, ohlc02)]
prft001 <- prft001 [, max_exit_cum := max(exit_cum), by = .(nblck, ohlc02)]
prft001 <- prft001 [, tottrd := max(nblck), ]

# get the counts

prft001_cnt <- prft001 [, .(loss = uniqueN(nblck)), by =.(tottrd, ohlc02, max_exit_cum)]

prft002_cnt <- dcast(data = prft001_cnt, 
                     tottrd + max_exit_cum ~ ohlc02, 
                     value.var = c("loss"))

################################################################################################################

# https://www.youtube.com/watch?v=ACdCQuQJxhU

wb <- createWorkbook()
addWorksheet(wb, "Sheet 1")

writeData(wb, "Sheet 1", dt08)

redstyle <- createStyle(fontColour = "#9c0006", bgFill = "#FFC7CE" ) 
greenstyle <- createStyle(fontColour = "#006100", bgFill = "#C6EFCE")

conditionalFormatting(wb, "Sheet 1",
                      cols = 24:27,
                      rows = 1: nrow(dt08)+1,
                      rule = ">=0",
                      style = greenstyle)

conditionalFormatting(wb, "Sheet 1",
                      cols = 24:27,
                      rows = 1: nrow(dt08)+1,
                      rule = "<0",
                      style = redstyle)

freezePane(wb, "Sheet 1", firstActiveRow = 2, firstActiveCol = 3)
addFilter(wb, "Sheet 1", row = 1, cols = 1:ncol(dt08))

saveWorkbook(wb, "D:/My-Shares/Short-Sell-Chirag-Jain-Maths-teacher/analysis_close/0001_shortsell_chirag_RpgmOutput_closeATM.xlsx", TRUE)


#write.xlsx(dt07 [, c("expdate", "trdate", "weekdays", "strike", "callput",
#                     "open", "high", "low", "close", 
#                     "org_sell_close", 
#                     "sumprm_open", "sumprm_close", "sumprm_hl", "sumprm_lh", 
#                     "diff_prm_open", "diff_prm_close", "diff_prm_hl", "diff_prm_lh",
#                     "diff_prm_perc_open", "diff_prm_perc_close", "diff_prm_perc_hl", "diff_prm_perc_lh")], 
#           file ="D:/My-Shares/Short-Sell-Chirag-Jain-Maths-teacher/analysis_2pm/0001_shortsell_chirag_RpgmOutput.xlsx")


#write.xlsx(dt07 [, c("expdate", "trdate", "weekdays", "strike", "callput",
#                     "open", "high", "low", "close", 
#                     "org_sell_close", 
#                     "sumprm_open", "sumprm_close", "sumprm_hl", "sumprm_lh", 
#                     "diff_prm_open", "diff_prm_close", "diff_prm_hl", "diff_prm_lh",
#                     "diff_prm_perc_open", "diff_prm_perc_close", "diff_prm_perc_hl", "diff_prm_perc_lh")], 
#           file ="D:/My-Shares/Short-Sell-Chirag-Jain-Maths-teacher/analysis_close/0001_shortsell_chirag_RpgmOutput_closeATM.xlsx")

list_of_files2011 <- list.files(path = "D:\\My-Shares\\source-fno-csv\\2011", recursive = TRUE, pattern = "^op", full.names = TRUE)
f <- function(x, pos) subset(x, SYMBOL %in% c("BANKNIFTY", "NIFTY") )
dt_chunk2011 <- rbindlist(sapply(list_of_files2011, read_csv_chunked, DataFrameCallback$new(f), simplify = FALSE))

list_of_files2012 <- list.files(path = "D:\\My-Shares\\source-fno-csv\\2012", recursive = TRUE, pattern = "^op", full.names = TRUE)
f <- function(x, pos) subset(x, SYMBOL %in% c("BANKNIFTY", "NIFTY") )
dt_chunk2012 <- rbindlist(sapply(list_of_files2012, read_csv_chunked, DataFrameCallback$new(f), simplify = FALSE))

list_of_files2013 <- list.files(path = "D:\\My-Shares\\source-fno-csv\\2013", recursive = TRUE, pattern = "^op", full.names = TRUE)
f <- function(x, pos) subset(x, SYMBOL %in% c("BANKNIFTY", "NIFTY") )
dt_chunk2013 <- rbindlist(sapply(list_of_files2013, read_csv_chunked, DataFrameCallback$new(f), simplify = FALSE))

list_of_files2014 <- list.files(path = "D:\\My-Shares\\source-fno-csv\\2014", recursive = TRUE, pattern = "^op", full.names = TRUE)
f <- function(x, pos) subset(x, SYMBOL %in% c("BANKNIFTY", "NIFTY") )
dt_chunk2014 <- rbindlist(sapply(list_of_files2014, read_csv_chunked, DataFrameCallback$new(f), simplify = FALSE))

list_of_files2015 <- list.files(path = "D:\\My-Shares\\source-fno-csv\\2015", recursive = TRUE, pattern = "^op", full.names = TRUE)
f <- function(x, pos) subset(x, SYMBOL %in% c("BANKNIFTY", "NIFTY") )
dt_chunk2015 <- rbindlist(sapply(list_of_files2015, read_csv_chunked, DataFrameCallback$new(f), simplify = FALSE))

list_of_files2016 <- list.files(path = "D:\\My-Shares\\source-fno-csv\\2016", recursive = TRUE, pattern = "^op", full.names = TRUE)
f <- function(x, pos) subset(x, SYMBOL %in% c("BANKNIFTY", "NIFTY") )
dt_chunk2016 <- rbindlist(sapply(list_of_files2016, read_csv_chunked, DataFrameCallback$new(f), simplify = FALSE))

list_of_files2017 <- list.files(path = "D:\\My-Shares\\source-fno-csv\\2017", recursive = TRUE, pattern = "^op", full.names = TRUE)
f <- function(x, pos) subset(x, SYMBOL %in% c("BANKNIFTY", "NIFTY") )
dt_chunk2017 <- rbindlist(sapply(list_of_files2017, read_csv_chunked, DataFrameCallback$new(f), simplify = FALSE))

list_of_files2018 <- list.files(path = "D:\\My-Shares\\source-fno-csv\\2018", recursive = TRUE, pattern = "^op", full.names = TRUE)
f <- function(x, pos) subset(x, SYMBOL %in% c("BANKNIFTY", "NIFTY") )
dt_chunk2018 <- rbindlist(sapply(list_of_files2018, read_csv_chunked, DataFrameCallback$new(f), simplify = FALSE))

list_of_files2019 <- list.files(path = "D:\\My-Shares\\source-fno-csv\\2019", recursive = TRUE, pattern = "\\^combined|csv$", full.names = TRUE)
f <- function(x, pos) subset(x, Symbol %in% c("BANKNIFTY", "NIFTY") )
dt_chunk2019 <- rbindlist(sapply(list_of_files2019, read_csv_chunked, DataFrameCallback$new(f), simplify = FALSE))

list_of_files2020 <- list.files(path = "D:\\My-Shares\\source-fno-csv\\2020", recursive = TRUE, pattern = "\\^combined|csv$", full.names = TRUE)
f <- function(x, pos) subset(x, Symbol %in% c("BANKNIFTY", "NIFTY") )
dt_chunk2020 <- rbindlist(sapply(list_of_files2020, read_csv_chunked, DataFrameCallback$new(f), simplify = FALSE))
