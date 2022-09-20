##########
#
# Q4 2019
#  lacs profit
#
# output <- trial001 [long001a == 1 & long002a == 1 & subrow >= 3  & subrow <= 65] # & entry_c]
# 5 trades: 6.4 / -1.9, overall: 4.5 profit
##########

icici_fno <- readRDS("D:/My-Shares/analysis/icici_fno.rds")
icici_fno <- icici_fno [, file := paste("file_Q4_2019_", .I, ".feather", sep=""), ]
icici_fno <- icici_fno [, file0 := paste("file_Q4_2019_", .I, sep=""), ]
icici_fno <- icici_fno [, step001 := paste('data2 = breeze.get_historical_data(interval="5minute", from_date= "2019-10-01T07:00:00.000Z", to_date= "2019-12-31T07:00:00.000Z", exchange_code="NSE", product_type="cash", stock_code="', ShortName, '")\nstock_data = pd.DataFrame(data2["Success"])', sep=""), ]
icici_fno <- icici_fno [, step002 := paste('feather.write_dataframe(stock_data, "D:/My-Shares/analysis/icici_direct/', file, '")', sep = ""), ]
icici_fno <- icici_fno [, step900 := paste(step001, "\n", step002, sep =""),]
icici_fno <- icici_fno [, step501 := paste(file0, ' <- as.data.table( arrow::read_feather("D:/My-Shares/analysis/icici_direct/', file, '"))\n', sep =""), ]

fwrite(icici_fno[, c("step900"), ], 
       quote = FALSE,
       sep = " ",
       col.names = FALSE,
       row.names = FALSE,
       file = "D:/My-Shares/prgm/temp_py/0550_icici_99_historicdata_Q4_2019.py")

eval(parse(text = icici_fno [nrow <= 189]$step501))

allD01 <- rbindlist(mget(ls(pattern = "file_Q4_2019_*")), fill = TRUE)
rm(list = ls( pattern='^file_Q4_2019_*'))

allD02 <- allD01 [, c("datetime", "stock_code", "open", "high", "low", "close", "volume"), ]
setnames(allD02, c("datetime", "stock_code", "open", "high", "low", "close", "volume"),
         c("Datetime_", "Name", "Open", "High", "Low", "Close", "Volume") )

stock_final <- allD02
#################################################


##########
#
# Q1 2020
# 1.75 lac profit
# 1,99 and 99: -1.80 lacs
#
# output <- trial001 [long001a == 1 & long002a == 1 & subrow >= 3  & subrow <= 65] # & entry_c]
# 5 trades: 7.99 / -1.39, overall: 6.59 profit
##########

icici_fno <- readRDS("D:/My-Shares/analysis/icici_fno.rds")
icici_fno <- icici_fno [, file := paste("file_Q1_2020_", .I, ".feather", sep=""), ]
icici_fno <- icici_fno [, file0 := paste("file_Q1_2020_", .I, sep=""), ]
icici_fno <- icici_fno [, step001 := paste('data2 = breeze.get_historical_data(interval="5minute", from_date= "2020-01-01T07:00:00.000Z", to_date= "2020-03-31T07:00:00.000Z", exchange_code="NSE", product_type="cash", stock_code="', ShortName, '")\nstock_data = pd.DataFrame(data2["Success"])', sep=""), ]
icici_fno <- icici_fno [, step002 := paste('feather.write_dataframe(stock_data, "D:/My-Shares/analysis/icici_direct/', file, '")', sep = ""), ]
icici_fno <- icici_fno [, step900 := paste(step001, "\n", step002, sep =""),]
icici_fno <- icici_fno [, step501 := paste(file0, ' <- as.data.table( arrow::read_feather("D:/My-Shares/analysis/icici_direct/', file, '"))\n', sep =""), ]

fwrite(icici_fno[, c("step900"), ], 
       quote = FALSE,
       sep = " ",
       col.names = FALSE,
       row.names = FALSE,
       file = "D:/My-Shares/prgm/temp_py/0550_icici_99_historicdata_Q1_2020.py")

eval(parse(text = icici_fno$step501))

allD01 <- rbindlist(mget(ls(pattern = "file_Q1_2020_*")), fill = TRUE)
rm(list = ls( pattern='^file_Q1_2020_*'))

allD02 <- allD01 [, c("datetime", "stock_code", "open", "high", "low", "close", "volume"), ]
setnames(allD02, c("datetime", "stock_code", "open", "high", "low", "close", "volume"),
         c("Datetime_", "Name", "Open", "High", "Low", "Close", "Volume") )

stock_final <- allD02

###########################################

##########
#
# Q2 2020
# 1 lac profit
# 1,99 and 99: -2.03 lacs
#
# output <- trial001 [long001a == 1 & long002a == 1 & subrow >= 3  & subrow <= 65] # & entry_c]
# 5 trades: 7.11 / -3.49, overall: 3.62 profit
##########


icici_fno <- readRDS("D:/My-Shares/analysis/icici_fno.rds")
icici_fno <- icici_fno [, file := paste("file_Q2_2020_", .I, ".feather", sep=""), ]
icici_fno <- icici_fno [, file0 := paste("file_Q2_2020_", .I, sep=""), ]
icici_fno <- icici_fno [, step001 := paste('data2 = breeze.get_historical_data(interval="5minute", from_date= "2020-04-01T07:00:00.000Z", to_date= "2020-06-30T07:00:00.000Z", exchange_code="NSE", product_type="cash", stock_code="', ShortName, '")\nstock_data = pd.DataFrame(data2["Success"])', sep=""), ]
icici_fno <- icici_fno [, step002 := paste('feather.write_dataframe(stock_data, "D:/My-Shares/analysis/icici_direct/', file, '")', sep = ""), ]
icici_fno <- icici_fno [, step900 := paste(step001, "\n", step002, sep =""),]
icici_fno <- icici_fno [, step501 := paste(file0, ' <- as.data.table( arrow::read_feather("D:/My-Shares/analysis/icici_direct/', file, '"))\n', sep =""), ]

fwrite(icici_fno[, c("step900"), ], 
       quote = FALSE,
       sep = " ",
       col.names = FALSE,
       row.names = FALSE,
       file = "D:/My-Shares/prgm/temp_py/0550_icici_99_historicdata_Q2_2020.py")

eval(parse(text = icici_fno$step501))

allD01 <- rbindlist(mget(ls(pattern = "file_Q2_2020_*")), fill = TRUE)
rm(list = ls( pattern='^file_Q2_2020_*'))

allD02 <- allD01 [, c("datetime", "stock_code", "open", "high", "low", "close", "volume"), ]
setnames(allD02, c("datetime", "stock_code", "open", "high", "low", "close", "volume"),
         c("Datetime_", "Name", "Open", "High", "Low", "Close", "Volume") )

stock_final <- allD02
#################################################

##########
#
# Q3 2020
# 0.23 lac profit
# 1,99 and 99: -2.50 lacs
#
# output <- trial001 [long001a == 1 & long002a == 1 & subrow >= 3  & subrow <= 65] # & entry_c]
# 5 trades: 5.62 / -2.68, overall: 2.94 profit
##########

icici_fno <- readRDS("D:/My-Shares/analysis/icici_fno.rds")
icici_fno <- icici_fno [, file := paste("file_Q3_2020_", .I, ".feather", sep=""), ]
icici_fno <- icici_fno [, file0 := paste("file_Q3_2020_", .I, sep=""), ]
icici_fno <- icici_fno [, step001 := paste('data2 = breeze.get_historical_data(interval="5minute", from_date= "2020-07-01T07:00:00.000Z", to_date= "2020-09-30T07:00:00.000Z", exchange_code="NSE", product_type="cash", stock_code="', ShortName, '")\nstock_data = pd.DataFrame(data2["Success"])', sep=""), ]
icici_fno <- icici_fno [, step002 := paste('feather.write_dataframe(stock_data, "D:/My-Shares/analysis/icici_direct/', file, '")', sep = ""), ]
icici_fno <- icici_fno [, step900 := paste(step001, "\n", step002, sep =""),]
icici_fno <- icici_fno [, step501 := paste(file0, ' <- as.data.table( arrow::read_feather("D:/My-Shares/analysis/icici_direct/', file, '"))\n', sep =""), ]

fwrite(icici_fno[, c("step900"), ], 
       quote = FALSE,
       sep = " ",
       col.names = FALSE,
       row.names = FALSE,
       file = "D:/My-Shares/prgm/temp_py/0550_icici_99_historicdata_Q3_2020.py")

eval(parse(text = icici_fno$step501))

allD01 <- rbindlist(mget(ls(pattern = "file_Q3_2020_*")), fill = TRUE)
rm(list = ls( pattern='^file_Q3_2020_*'))

allD02 <- allD01 [, c("datetime", "stock_code", "open", "high", "low", "close", "volume"), ]
setnames(allD02, c("datetime", "stock_code", "open", "high", "low", "close", "volume"),
         c("Datetime_", "Name", "Open", "High", "Low", "Close", "Volume") )

stock_final <- allD02
#################################################


##########
#
# Q4 2020
# 1.56 lac profit
# 1,99 and 99: -2.89 lacs
#
# output <- trial001 [long001a == 1 & long002a == 1 & subrow >= 3  & subrow <= 65] # & entry_c]
# 5 trades: 6.86 / -2.49, overall: 4.37 profit
##########

icici_fno <- readRDS("D:/My-Shares/analysis/icici_fno.rds")
icici_fno <- icici_fno [, file := paste("file_Q4_2020_", .I, ".feather", sep=""), ]
icici_fno <- icici_fno [, file0 := paste("file_Q4_2020_", .I, sep=""), ]
icici_fno <- icici_fno [, step001 := paste('data2 = breeze.get_historical_data(interval="5minute", from_date= "2020-10-01T07:00:00.000Z", to_date= "2020-12-31T07:00:00.000Z", exchange_code="NSE", product_type="cash", stock_code="', ShortName, '")\nstock_data = pd.DataFrame(data2["Success"])', sep=""), ]
icici_fno <- icici_fno [, step002 := paste('feather.write_dataframe(stock_data, "D:/My-Shares/analysis/icici_direct/', file, '")', sep = ""), ]
icici_fno <- icici_fno [, step900 := paste(step001, "\n", step002, sep =""),]
icici_fno <- icici_fno [, step501 := paste(file0, ' <- as.data.table( arrow::read_feather("D:/My-Shares/analysis/icici_direct/', file, '"))\n', sep =""), ]

fwrite(icici_fno[, c("step900"), ], 
       quote = FALSE,
       sep = " ",
       col.names = FALSE,
       row.names = FALSE,
       file = "D:/My-Shares/prgm/temp_py/0550_icici_99_historicdata_Q4_2020.py")

eval(parse(text = icici_fno$step501))

allD01 <- rbindlist(mget(ls(pattern = "file_Q4_2020_*")), fill = TRUE)
rm(list = ls( pattern='^file_Q4_2020_*'))

allD02 <- allD01 [, c("datetime", "stock_code", "open", "high", "low", "close", "volume"), ]
setnames(allD02, c("datetime", "stock_code", "open", "high", "low", "close", "volume"),
         c("Datetime_", "Name", "Open", "High", "Low", "Close", "Volume") )

stock_final <- allD02
#################################################

##########
#
# Q1 2021
# 1 lac profit
# 1,99 and 99: -2.06 lacs
#
# output <- trial001 [long001a == 1 & long002a == 1 & subrow >= 3  & subrow <= 65] # & entry_c]
# 5 trades: 7.52 / -5.09, overall: 2.42 profit
##########

icici_fno <- readRDS("D:/My-Shares/analysis/icici_fno.rds")
icici_fno <- icici_fno [, file := paste("file_Q1_2021_", .I, ".feather", sep=""), ]
icici_fno <- icici_fno [, file0 := paste("file_Q1_2021_", .I, sep=""), ]
icici_fno <- icici_fno [, step001 := paste('data2 = breeze.get_historical_data(interval="5minute", from_date= "2021-01-01T07:00:00.000Z", to_date= "2021-03-31T07:00:00.000Z", exchange_code="NSE", product_type="cash", stock_code="', ShortName, '")\nstock_data = pd.DataFrame(data2["Success"])', sep=""), ]
icici_fno <- icici_fno [, step002 := paste('feather.write_dataframe(stock_data, "D:/My-Shares/analysis/icici_direct/', file, '")', sep = ""), ]
icici_fno <- icici_fno [, step900 := paste(step001, "\n", step002, sep =""),]
icici_fno <- icici_fno [, step501 := paste(file0, ' <- as.data.table( arrow::read_feather("D:/My-Shares/analysis/icici_direct/', file, '"))\n', sep =""), ]

fwrite(icici_fno[, c("step900"), ], 
       quote = FALSE,
       sep = " ",
       col.names = FALSE,
       row.names = FALSE,
       file = "D:/My-Shares/prgm/temp_py/0550_icici_99_historicdata_Q1_2021.py")

eval(parse(text = icici_fno$step501))

allD01 <- rbindlist(mget(ls(pattern = "file_Q1_2021_*")), fill = TRUE)
rm(list = ls( pattern='^file_Q1_2021_*'))

allD02 <- allD01 [, c("datetime", "stock_code", "open", "high", "low", "close", "volume"), ]
setnames(allD02, c("datetime", "stock_code", "open", "high", "low", "close", "volume"),
         c("Datetime_", "Name", "Open", "High", "Low", "Close", "Volume") )

stock_final <- allD02

###########################################

##########
#
# Q2 2021
# 1.82 lac profit 
# 1,99 and 99: -1.89 lacs
#
# output <- trial001 [long001a == 1 & long002a == 1 & subrow >= 3  & subrow <= 65] # & entry_c]
# 5 trades: 6.44 / -3.17, overall: 3.26 profit
##########

icici_fno <- readRDS("D:/My-Shares/analysis/icici_fno.rds")
icici_fno <- icici_fno [, file := paste("file_Q2_2021_", .I, ".feather", sep=""), ]
icici_fno <- icici_fno [, file0 := paste("file_Q2_2021_", .I, sep=""), ]
icici_fno <- icici_fno [, step001 := paste('data2 = breeze.get_historical_data(interval="5minute", from_date= "2021-04-01T07:00:00.000Z", to_date= "2021-06-30T07:00:00.000Z", exchange_code="NSE", product_type="cash", stock_code="', ShortName, '")\nstock_data = pd.DataFrame(data2["Success"])', sep=""), ]
icici_fno <- icici_fno [, step002 := paste('feather.write_dataframe(stock_data, "D:/My-Shares/analysis/icici_direct/', file, '")', sep = ""), ]
icici_fno <- icici_fno [, step900 := paste(step001, "\n", step002, sep =""),]
icici_fno <- icici_fno [, step501 := paste(file0, ' <- as.data.table( arrow::read_feather("D:/My-Shares/analysis/icici_direct/', file, '"))\n', sep =""), ]

fwrite(icici_fno[, c("step900"), ], 
       quote = FALSE,
       sep = " ",
       col.names = FALSE,
       row.names = FALSE,
       file = "D:/My-Shares/prgm/temp_py/0550_icici_99_historicdata_Q2_2021.py")

eval(parse(text = icici_fno$step501))

allD01 <- rbindlist(mget(ls(pattern = "file_Q2_2021_*")), fill = TRUE)
rm(list = ls( pattern='^file_Q2_2021_*'))

allD02 <- allD01 [, c("datetime", "stock_code", "open", "high", "low", "close", "volume"), ]
setnames(allD02, c("datetime", "stock_code", "open", "high", "low", "close", "volume"),
         c("Datetime_", "Name", "Open", "High", "Low", "Close", "Volume") )

stock_final <- allD02

###########################################

##########
#
# Q3 2021
# Regular ST: 2.03 lac profits, 1,99 and 99: -2.43 lacs
# VPT ST: 1.69 lacs overall, 3.39, -1.70 lacs
#
# output <- trial001 [long001a == 1 & long002a == 1 & subrow >= 3  & subrow <= 65] # & entry_c]
# 5 trades: 7.41 / -1.61, overall: 5.79 profit
##########

icici_fno <- readRDS("D:/My-Shares/analysis/icici_fno.rds")
icici_fno <- icici_fno [, file := paste("file_Q3_2021_", .I, ".feather", sep=""), ]
icici_fno <- icici_fno [, file0 := paste("file_Q3_2021_", .I, sep=""), ]
icici_fno <- icici_fno [, step001 := paste('data2 = breeze.get_historical_data(interval="5minute", from_date= "2021-07-01T07:00:00.000Z", to_date= "2021-09-30T07:00:00.000Z", exchange_code="NSE", product_type="cash", stock_code="', ShortName, '")\nstock_data = pd.DataFrame(data2["Success"])', sep=""), ]
icici_fno <- icici_fno [, step002 := paste('feather.write_dataframe(stock_data, "D:/My-Shares/analysis/icici_direct/', file, '")', sep = ""), ]
icici_fno <- icici_fno [, step900 := paste(step001, "\n", step002, sep =""),]
icici_fno <- icici_fno [, step501 := paste(file0, ' <- as.data.table( arrow::read_feather("D:/My-Shares/analysis/icici_direct/', file, '"))\n', sep =""), ]

fwrite(icici_fno[, c("step900"), ], 
       quote = FALSE,
       sep = " ",
       col.names = FALSE,
       row.names = FALSE,
       file = "D:/My-Shares/prgm/temp_py/0550_icici_99_historicdata_Q3_2021.py")

eval(parse(text = icici_fno$step501))

allD01 <- rbindlist(mget(ls(pattern = "file_Q3_2021_*")), fill = TRUE)
rm(list = ls( pattern='^file_Q3_2021_*'))

allD02 <- allD01 [, c("datetime", "stock_code", "open", "high", "low", "close", "volume"), ]
setnames(allD02, c("datetime", "stock_code", "open", "high", "low", "close", "volume"),
         c("Datetime_", "Name", "Open", "High", "Low", "Close", "Volume") )

stock_final <- allD02
#################################################


##########
#
# Q4 2021
# Regular ST: 2.05 lacs profit, # 1,99 and 99: -1.67 lacs
# VPT ST: 2 lacs overall, 3.7 lacs profit, -1.7 lacs (NA appearing in sl008, need to check)
#
# >2.5 ratio and DIp <= 35
# 0.75 lacs profit, # 1,99 and 99: -2.30 lacs
#
# output <- trial001 [long001a == 1 & long002a == 1 & subrow >= 3  & subrow <= 65] # & entry_c]
# 5 trades: 8.17 / -2.61, overall: 5.56 profit
##########

icici_fno <- readRDS("D:/My-Shares/analysis/icici_fno.rds")
icici_fno <- icici_fno [, file := paste("file_Q4_2021_", .I, ".feather", sep=""), ]
icici_fno <- icici_fno [, file0 := paste("file_Q4_2021_", .I, sep=""), ]
icici_fno <- icici_fno [, step001 := paste('data2 = breeze.get_historical_data(interval="5minute", from_date= "2021-10-01T07:00:00.000Z", to_date= "2021-12-31T07:00:00.000Z", exchange_code="NSE", product_type="cash", stock_code="', ShortName, '")\nstock_data = pd.DataFrame(data2["Success"])', sep=""), ]
icici_fno <- icici_fno [, step002 := paste('feather.write_dataframe(stock_data, "D:/My-Shares/analysis/icici_direct/', file, '")', sep = ""), ]
icici_fno <- icici_fno [, step900 := paste(step001, "\n", step002, sep =""),]
icici_fno <- icici_fno [, step501 := paste(file0, ' <- as.data.table( arrow::read_feather("D:/My-Shares/analysis/icici_direct/', file, '"))\n', sep =""), ]

fwrite(icici_fno[, c("step900"), ], 
       quote = FALSE,
       sep = " ",
       col.names = FALSE,
       row.names = FALSE,
       file = "D:/My-Shares/prgm/temp_py/0550_icici_99_historicdata_Q4_2021.py")

eval(parse(text = icici_fno$step501))

allD01 <- rbindlist(mget(ls(pattern = "file_Q4_2021_*")), fill = TRUE)
rm(list = ls( pattern='^file_Q4_2021_*'))

allD02 <- allD01 [, c("datetime", "stock_code", "open", "high", "low", "close", "volume"), ]
setnames(allD02, c("datetime", "stock_code", "open", "high", "low", "close", "volume"),
         c("Datetime_", "Name", "Open", "High", "Low", "Close", "Volume") )

stock_final <- allD02
#################################################


##########
#
# Q1 2022 [5 trades]
# Regular ST: 2.20 lacs profit, 1,99 and 99: -1.6 lacs
# VPT ST: 2.28 lacs overall, 3.35 lacs, -1.07 lacs
#
# >2.5 ratio and DIp <= 35
# 1.82 lacs profit, # 1,99 and 99: -1.80 lacs
#
# output <- trial001 [long001a == 1 & long002a == 1 & subrow >= 3  & subrow <= 65] # & entry_c]
# 5 trades: 6.99 / -1.8, overall: 5.19 profit
##########

icici_fno <- readRDS("D:/My-Shares/analysis/icici_fno.rds")
icici_fno <- icici_fno [, file := paste("file_Q1_2022_", .I, ".feather", sep=""), ]
icici_fno <- icici_fno [, file0 := paste("file_Q1_2022_", .I, sep=""), ]
icici_fno <- icici_fno [, step001 := paste('data2 = breeze.get_historical_data(interval="5minute", from_date= "2022-01-01T07:00:00.000Z", to_date= "2022-03-31T07:00:00.000Z", exchange_code="NSE", product_type="cash", stock_code="', ShortName, '")\nstock_data = pd.DataFrame(data2["Success"])', sep=""), ]
icici_fno <- icici_fno [, step002 := paste('feather.write_dataframe(stock_data, "D:/My-Shares/analysis/icici_direct/', file, '")', sep = ""), ]
icici_fno <- icici_fno [, step900 := paste(step001, "\n", step002, sep =""),]
icici_fno <- icici_fno [, step501 := paste(file0, ' <- as.data.table( arrow::read_feather("D:/My-Shares/analysis/icici_direct/', file, '"))\n', sep =""), ]

fwrite(icici_fno[, c("step900"), ], 
       quote = FALSE,
       sep = " ",
       col.names = FALSE,
       row.names = FALSE,
       file = "D:/My-Shares/prgm/temp_py/0550_icici_99_historicdata_Q1_2022.py")

eval(parse(text = icici_fno$step501))

allD01 <- rbindlist(mget(ls(pattern = "file_Q1_2022_*")), fill = TRUE)
rm(list = ls( pattern='^file_Q1_2022_*'))

allD02 <- allD01 [, c("datetime", "stock_code", "open", "high", "low", "close", "volume"), ]
setnames(allD02, c("datetime", "stock_code", "open", "high", "low", "close", "volume"),
         c("Datetime_", "Name", "Open", "High", "Low", "Close", "Volume") )

stock_final <- allD02

###########################################

##########
#
# Q2 2022
# >3 ratio [5 trades]
# Regular ST: 2.29 lacs profit, # 1,99 and 99: -1.5 lacs
# VPT ST: 1.31 lacs overall, 2.67 lacs, -1.36 lacs
#
# >2.5 ratio and DIp <= 35
# 2.39 lacs profit, # 1,99 and 99: -1.90 lacs
#
# 1 lac and 10 trades: >3 ratio
# 
#
# 1 lac and 10 trades: >2.5 ratio and DIp <= 35
# 
#
# output <- trial001 [long001a == 1 & long002a == 1 & subrow >= 3  & subrow <= 65] # & entry_c]
# 5 trades: 5.79 / -2.23
##########

icici_fno <- readRDS("D:/My-Shares/analysis/icici_fno.rds")
icici_fno <- icici_fno [, file := paste("file_Q2_2022_", .I, ".feather", sep=""), ]
icici_fno <- icici_fno [, file0 := paste("file_Q2_2022_", .I, sep=""), ]
icici_fno <- icici_fno [, step001 := paste('data2 = breeze.get_historical_data(interval="5minute", from_date= "2022-04-01T07:00:00.000Z", to_date= "2022-06-30T07:00:00.000Z", exchange_code="NSE", product_type="cash", stock_code="', ShortName, '")\nstock_data = pd.DataFrame(data2["Success"])', sep=""), ]
icici_fno <- icici_fno [, step002 := paste('feather.write_dataframe(stock_data, "D:/My-Shares/analysis/icici_direct/', file, '")', sep = ""), ]
icici_fno <- icici_fno [, step900 := paste(step001, "\n", step002, sep =""),]
icici_fno <- icici_fno [, step501 := paste(file0, ' <- as.data.table( arrow::read_feather("D:/My-Shares/analysis/icici_direct/', file, '"))\n', sep =""), ]

fwrite(icici_fno[, c("step900"), ], 
       quote = FALSE,
       sep = " ",
       col.names = FALSE,
       row.names = FALSE,
       file = "D:/My-Shares/prgm/temp_py/0550_icici_99_historicdata_Q2_2022.py")

eval(parse(text = icici_fno$step501))

allD01 <- rbindlist(mget(ls(pattern = "file_Q2_2022_*")), fill = TRUE)
rm(list = ls( pattern='^file_Q2_2022_*'))

allD02 <- allD01 [, c("datetime", "stock_code", "open", "high", "low", "close", "volume"), ]
setnames(allD02, c("datetime", "stock_code", "open", "high", "low", "close", "volume"),
         c("Datetime_", "Name", "Open", "High", "Low", "Close", "Volume") )

stock_final <- allD02
#################################################





setnames (stock_final, "Datetime_", "Datetime")
stock_final <- as.data.table(stock_final)
stock_final <- stock_final [, trdtme := format(Datetime, tz="Asia/Calcutta"), ]
stock_final <- stock_final [, trdate := anydate(str_sub(trdtme, 1, 10) ), ]
stock_final <- stock_final [ order(Name, trdtme) ]
stock_final <- stock_final [, nrow := 1:.N, by = .(Name)]
stock_final <- stock_final [, subrow := 1:.N, by = .(Name, trdate)]

setnames(stock_final, c("Open", "High", "Low", "Close", "Volume", "Name"), 
         c("price.open", "price.high", "price.low", "price.close", "volume", "ticker") )


all02 <- stock_final [, -c("NA"), ]
all02 <- all02 [, `:=`(price.open = as.numeric(price.open), 
                       price.high = as.numeric(price.high), 
                       price.low = as.numeric(price.low), 
                       price.close = as.numeric(price.close), 
                       volume = as.numeric(volume)), ]
all02 <- na.omit(all02)
all02 <- all02 [, allrow := .I, ]

adx_n <- 14
adx_dn = as.data.table( ADX(all02[,c("price.high","price.low","price.close"),], n = adx_n) )
adx_dn <- adx_dn [, allrow := .I, ]

data_05min <- all02 [ order(ticker, trdate)]

########################################
#
# Create 1 day data from 5 mins data
#
########################################

data_05min <- data_05min [, `:=`(minsub = min(subrow),
                                 maxsub = max(subrow), 
                                 high = max(price.high), 
                                 low = min(price.low), 
                                 volumed = sum(volume)), by = .(ticker, trdate)]

data_01dayo <- data_05min [minsub == subrow]
data_01dayc <- data_05min [maxsub == subrow]

setnames(data_01dayo, "price.open", "open")
setnames(data_01dayc, "price.close", "close")

data_01day <- merge(x = data_01dayo [, c("ticker", "trdate", "open", "high", "low", "volumed"), ],
                    y = data_01dayc [, c("ticker", "trdate", "close"), ],
                    by = c("ticker", "trdate"))

rm(data_01dayo, data_01dayc)

data_01day <- data_01day [, drow := 1:.N, by = .(ticker)]

data_01day <- data_01day [, vPP := as.numeric(round( (shift(high, n =1, type = c("lag")) + 
                                                        shift(low, n =1, type = c("lag")) + 
                                                        shift(close, n =1, type = c("lag"))  )/ 3), 2 ), ]
data_01day <- data_01day [, `:=`(phigh = shift(high, n =1, type = c("lag")), 
                                 plow = shift(low, n =1, type = c("lag")),
                                 pclose = shift(close, n =1, type =c("lag")) ), by =.(ticker)]

data_01day <- data_01day [, `:=`(vR0 = vPP + (phigh - plow) * 0,
                                 vS0 = vPP - (phigh - plow) * 0,
                                 
                                 vR0236 = vPP + (phigh - plow) * 0.236,
                                 vS0236 = vPP - (phigh - plow) * 0.236,
                                 
                                 vR0382 = vPP + (phigh - plow) * 0.382,
                                 vS0382 = vPP - (phigh - plow) * 0.382,
                                 
                                 vR05 = vPP + (phigh - plow) * 0.5,
                                 vS05 = vPP - (phigh - plow) * 0.5,
                                 
                                 vR0618 = vPP + (phigh - plow) * 0.618,
                                 vS0618 = vPP - (phigh - plow) * 0.618,
                                 
                                 vR0786 = vPP + (phigh - plow) * 0.786,
                                 vS0786 = vPP - (phigh - plow) * 0.786,
                                 
                                 vR1 = vPP + (phigh - plow) * 01,
                                 vS1 = vPP - (phigh - plow) * 01,
                                 
                                 vR1272 = vPP + (phigh - plow) * 1.272,
                                 vS1272 = vPP - (phigh - plow) * 1.272,
                                 
                                 vR1414 = vPP + (phigh - plow) * 1.414,
                                 vS1414 = vPP - (phigh - plow) * 1.414,
                                 
                                 vR1618 = vPP + (phigh - plow) * 1.618,
                                 vS1618 = vPP - (phigh - plow) * 1.618,
                                 
                                 vR2618 = vPP + (phigh - plow) * 2.618,
                                 vS2618 = vPP - (phigh - plow) * 2.618), ]

all02 <- merge.data.table (x = data_05min, 
                           y = data_01day [, c("ticker", "trdate", "open", "high", "low", "close", "volumed", "drow", "pclose",
                                               "vR0", "vR0236", "vR0382", "vR05", "vR0618", "vR0786", "vR1", "vR1272", "vR1414", "vR1618", "vR2618",
                                               "vS0", "vS0236", "vS0382", "vS05", "vS0618", "vS0786", "vS1", "vS1272", "vS1414", "vS1618", "vS2618"), ],
                           by = c("ticker", "trdate"))

all02 <- all02 [, allrow := .I, ]
all02 <- all02 [, nrow := 1:.N, by = .(ticker)]

all02 <- all02 [, `:=` (a15 = ceiling(subrow / 3),
                        a30 = ceiling(subrow / 6),
                        a60 = ceiling(subrow / 12)), ]

all02 <- all02 [, a15vol := sum(volume), by = .(ticker, trdate, a15)]
all02 <- all02 [, a30vol := sum(volume), by = .(ticker, trdate, a30)]
#all02 <- all02 [, a60vol := sum(volume), by = .(ticker, trdate, a60)]

###################
#
# Rafael Zioni:
# //https://www.tradingview.com/script/i8FO3CyP-VPT-v4/
#
###################

all02 <- all02 [, `:=`(hilow = ((price.high - price.low)*100),
                       openclose = ((price.close - price.open)*100),
                       price_spread = sd(price.high - price.low, 28)), by = .(ticker)]

all02 <- all02 [, vol := (volume / ifelse(hilow == 0, 1, hilow) ), by = .(ticker)] 
all02 <- all02 [, spreadvol := (openclose * vol), by = .(ticker) ]
all02 <- all02 [, v := cumsum(spreadvol), by = .(ticker)]
all02 <- all02 [, smooth := EMA(v, 14), by = .(ticker)]
all02 <- all02 [, v_spread := sd(v - smooth, 28), by = .(ticker)]
all02 <- all02 [, shadow := (v - smooth) / v_spread * price_spread, by = .(ticker)]
all02 <- all02 [, out := ifelse(shadow >= 0, price.high + shadow, price.low + shadow), by = .(ticker)]
all02 <- all02 [, vpt05 := EMA(out, 10), by = .(ticker)]

#############################################################
#
# Calculate MFI for 5, 15, and 30 mins
# Calculate Supertrend and check if this could work
#
#############################################################

all02 <- all02 [, `:=`(mfi09o05 = MFI(price.open, volume, 9),
                       mfi09h05 = MFI(price.high, volume, 9),
                       mfi09l05 = MFI(price.low, volume, 9),
                       mfi09c05 = MFI(price.close, volume, 9) ), by = .(ticker)]

###################

data_05min02 <- all02 [, `:=` (o15 = shift(price.open, n = 2, type = c("lag") ),
                               h15 = pmax( shift(price.high, n = 2, type = c("lag") ), shift(price.high, n = 1, type = c("lag") ), price.high  ),
                               l15 = pmin( shift(price.low, n = 2, type = c("lag") ), shift(price.low, n = 1, type = c("lag") ), price.low  ),
                               c15 = price.close,
                               v15 = runSum(volume, n =3) ), 
                       by = .(ticker)]

data_05min02 <- data_05min02 [, `:=` (o30 = shift(price.open, n = 5, type = c("lag") ),
                                      h30 = pmax( shift(price.high, n = 5, type = c("lag") ), shift(price.high, n = 4, type = c("lag") ), shift(price.high, n = 3, type = c("lag") ), shift(price.high, n = 2, type = c("lag") ), shift(price.high, n = 1, type = c("lag") ), price.high  ),
                                      l30 = pmin( shift(price.low, n = 5, type = c("lag") ), shift(price.low, n = 4, type = c("lag") ), shift(price.low, n = 3, type = c("lag") ), shift(price.low, n = 2, type = c("lag") ), shift(price.low, n = 1, type = c("lag") ), price.low  ),
                                      c30 = price.close,
                                      v30 = runSum(volume, n =6) ), 
                              by = .(ticker)]

#data_05min02 <- data_05min02 [, `:=` (o60 = shift(price.open, n = 11, type = c("lag") ),
#                                      h60 = pmax( shift(price.high, n = 11, type = c("lag") ), shift(price.high, n = 10, type = c("lag") ), shift(price.high, n = 9, type = c("lag") ), shift(price.high, n = 8, type = c("lag") ), shift(price.high, n = 7, type = c("lag") ), shift(price.high, n = 6, type = c("lag") ), shift(price.high, n = 5, type = c("lag") ), shift(price.high, n = 4, type = c("lag") ), shift(price.high, n = 3, type = c("lag") ), shift(price.high, n = 2, type = c("lag") ), shift(price.high, n = 1, type = c("lag") ), price.high  ),
#                                      l60 = pmin( shift(price.low, n = 11, type = c("lag") ), shift(price.low, n = 10, type = c("lag") ), shift(price.low, n = 9, type = c("lag") ), shift(price.low, n = 8, type = c("lag") ), shift(price.low, n = 7, type = c("lag") ), shift(price.low, n = 6, type = c("lag") ), shift(price.low, n = 5, type = c("lag") ), shift(price.low, n = 4, type = c("lag") ), shift(price.low, n = 3, type = c("lag") ), shift(price.low, n = 2, type = c("lag") ), shift(price.low, n = 1, type = c("lag") ), price.low  ),
#                                      c60 = price.close,
#                                      v60 = runSum(volume, n =12) ), 
#                              by = .(ticker)]


a15 <- data_05min02 [ subrow / 3 == a15, c("ticker", "trdate", "trdtme", "subrow", "a15", "a30", "a60", "v15", "o15", "h15", "l15", "c15"), ]
a30 <- data_05min02 [ subrow / 6 == a30, c("ticker", "trdate", "trdtme", "subrow", "a15", "a30", "a60", "v30", "o30", "h30", "l30", "c30"), ]
#a60 <- data_05min02 [ subrow / 12 == a60, c("ticker", "trdate", "trdtme", "subrow", "a15", "a30", "a60", "v60", "o60", "h60", "l60", "c60"), ]

a15 <- a15 [, `:=`(hilow = ((h15 - l15)*100),
                   openclose = ((c15 - o15)*100),
                   price_spread = sd(h15 - l15, 28)), by = .(ticker)]

a15 <- a15 [, vol := (v15 / ifelse(hilow == 0, 1, hilow) ), by = .(ticker)] 
a15 <- a15 [, spreadvol := (openclose * v15), by = .(ticker) ]
a15 <- a15 [, v := cumsum(spreadvol), by = .(ticker)]
a15 <- a15 [, smooth := EMA(v, 14), by = .(ticker)]
a15 <- a15 [, v_spread := sd(v - smooth, 28), by = .(ticker)]
a15 <- a15 [, shadow := (v - smooth) / v_spread * price_spread, by = .(ticker)]
a15 <- a15 [, out := ifelse(shadow >= 0, h15 + shadow, l15 + shadow), by = .(ticker)]
a15 <- a15 [, vpt15 := EMA(out, 10), by = .(ticker)]

a15 <- a15 [, `:=`(mfi09o15 = MFI(o15, v15, 9),
                   mfi09h15 = MFI(h15, v15, 9),
                   mfi09l15 = MFI(l15, v15, 9),
                   mfi09c15 = MFI(c15, v15, 9) ), by = .(ticker)]

a30 <- a30 [, `:=`(hilow = ((h30 - l30)*100),
                   openclose = ((c30 - o30)*100),
                   price_spread = sd(h30 - l30, 28)), by = .(ticker)]

a30 <- a30 [, vol := (v30 / ifelse(hilow == 0, 1, hilow) ), by = .(ticker)] 
a30 <- a30 [, spreadvol := (openclose * v30), by = .(ticker) ]
a30 <- a30 [, v := cumsum(spreadvol), by = .(ticker)]
a30 <- a30 [, smooth := EMA(v, 14), by = .(ticker)]
a30 <- a30 [, v_spread := sd(v - smooth, 28), by = .(ticker)]
a30 <- a30 [, shadow := (v - smooth) / v_spread * price_spread, by = .(ticker)]
a30 <- a30 [, out := ifelse(shadow >= 0, h30 + shadow, l30 + shadow), by = .(ticker)]
a30 <- a30 [, vpt30 := EMA(out, 10), by = .(ticker)]

a30 <- a30 [, `:=`(mfi09o30 = MFI(o30, v30, 9),
                   mfi09h30 = MFI(h30, v30, 9),
                   mfi09l30 = MFI(l30, v30, 9),
                   mfi09c30 = MFI(c30, v30, 9) ), by = .(ticker)]

setnames(a15, c("o15", "h15", "l15", "c15", "v15", "vpt15"), 
         c("price.open", "price.high", "price.low", "price.close", "volume", "vpt15") )

setnames(a30, c("o30", "h30", "l30", "c30", "v30", "vpt30"), 
         c("price.open", "price.high", "price.low", "price.close", "volume", "vpt30") )


a05 <- all02 [, c("ticker", "trdate", "trdtme", "allrow", "subrow", "a15", "a30", "a60", "volume", "price.open", "price.high", "price.low", "price.close", "vpt05", "mfi09o05", "mfi09h05", "mfi09l05", "mfi09c05"), ]

write_feather(a05, "D:/My-Shares/analysis/0551_5min_data_stin.feather")
write_feather(a15, "D:/My-Shares/analysis/0551_15min_data_stin.feather")
write_feather(a30, "D:/My-Shares/analysis/0551_30min_data_stin.feather")

write_feather(a05[, c("ticker", "trdate", "trdtme", "allrow", "subrow", "mfi09o05", "mfi09h05", "mfi09l05", "mfi09c05"),], "D:/My-Shares/analysis/0551_5min_data_mfi_stin.feather")
write_feather(a15[, c("ticker", "trdate", "trdtme", "subrow", "mfi09o15", "mfi09h15", "mfi09l15", "mfi09c15"),], "D:/My-Shares/analysis/0551_15min_data_mfi_stin.feather")
write_feather(a30[, c("ticker", "trdate", "trdtme", "subrow", "mfi09o30", "mfi09h30", "mfi09l30", "mfi09c30"),], "D:/My-Shares/analysis/0551_30min_data_mfi_stin.feather")

#############################################################################
#
# As the feather library from python was not getting read correctly
# Switched from py_run_file command to system command to execute the program
#
#############################################################################

system(paste('"c:/Program Files/Python310/python.exe"', 'D:/My-Shares/prgm/vpt_super/0551_yh_stock_part02_multi_tf_supertrend_trial_feather02.py'))

system(paste('"c:/Program Files/Python310/python.exe"', 'D:/My-Shares/prgm/vpt_super/0551_yh_stock_part02_multi_tf_supertrend_trial_feather02mfi.py'))

#################################################################
#
# Part 3:
#
# Get the signal calculations done here
# ST5, St15, ST30 aligned
# DIp / Din >= 3 or Din / DIp >= 3 and ADX <= 30 or ADX <= 40
# Price compared to previous day <= 1% 
#
#################################################################

all03_05 <- as.data.table( arrow::read_feather("D:/My-Shares/analysis/0551_5min_data_stout.feather") )
all03_15 <- as.data.table( arrow::read_feather("D:/My-Shares/analysis/0551_15min_data_stout.feather") )
all03_30 <- as.data.table( arrow::read_feather("D:/My-Shares/analysis/0551_30min_data_stout.feather") )

all03 <- Reduce(function(...) merge(..., by = c("ticker", "trdtme", "subrow"), all=T),  
                list( all03_05, #[, c(1:1, 3:4, 8:14), ], 
                      all03_15 [, c("ticker", "trdtme", "subrow", "SUPERT15_10_2", "SUPERTd15_10_2"), ], 
                      all03_30 [, c("ticker", "trdtme", "subrow", "SUPERT30_10_2", "SUPERTd30_10_2"), ]) )

all03 <- all03 [, `:=`(st15 = na.locf(SUPERT15_10_2, na.rm = FALSE),
                       std15 = na.locf(SUPERTd15_10_2, na.rm = FALSE),
                       st30 = na.locf(SUPERT30_10_2, na.rm = FALSE),
                       std30 = na.locf(SUPERTd30_10_2, na.rm = FALSE) ), by = .(ticker)]

all03 <- all03 [, -c("SUPERT15_10_2", "SUPERTd15_10_2", "SUPERT30_10_2", "SUPERTd30_10_2"), ]


#######################################
#
# Do the same calculations for MFI
#
#######################################

all03_05mfi <- as.data.table( arrow::read_feather("D:/My-Shares/analysis/0551_5min_data_mfi_stout.feather") )
all03_15mfi <- as.data.table( arrow::read_feather("D:/My-Shares/analysis/0551_15min_data_mfi_stout.feather") )
all03_30mfi <- as.data.table( arrow::read_feather("D:/My-Shares/analysis/0551_30min_data_mfi_stout.feather") )

all03mfi <- Reduce(function(...) merge(..., by = c("ticker", "trdtme", "subrow"), all=T),  
                list( all03_05mfi, #[, c(1:1, 3:4, 8:14), ], 
                      all03_15mfi [, c("ticker", "trdtme", "subrow", "SUPERT15_10_2", "SUPERTd15_10_2"), ], 
                      all03_30mfi [, c("ticker", "trdtme", "subrow", "SUPERT30_10_2", "SUPERTd30_10_2"), ]) )

all03mfi <- all03mfi [, `:=`(st15mfi = na.locf(SUPERT15_10_2, na.rm = FALSE),
                       std15mfi = na.locf(SUPERTd15_10_2, na.rm = FALSE),
                       st30mfi = na.locf(SUPERT30_10_2, na.rm = FALSE),
                       std30mfi = na.locf(SUPERTd30_10_2, na.rm = FALSE) ), by = .(ticker)]

all03mfi <- all03mfi [, -c("SUPERT15_10_2", "SUPERTd15_10_2", "SUPERT30_10_2", "SUPERTd30_10_2"), ]
setnames(all03mfi, c("SUPERT05_10_2", "SUPERTd05_10_2"), 
         c("st05mfi", "std05mfi") )


all03 <- merge(x = all03,
               y = adx_dn,
               by = c("allrow"))

# Merge mfi data with other data:
all03 <- merge(x = all03,
               y = all03mfi [, c("allrow", "st05mfi", "std05mfi", "st15mfi", "std15mfi", "st30mfi", "std30mfi"), ],
               by = c("allrow"))

all03 <- merge (x = all03,
                y = all02 [, c("ticker", "trdate", "trdtme", "allrow", "pclose",
                               "vR0", "vR0236", "vR0382", "vR05", "vR0618", "vR0786", "vR1", "vR1272", "vR1414", "vR1618", "vR2618",
                               "vS0", "vS0236", "vS0382", "vS05", "vS0618", "vS0786", "vS1", "vS1272", "vS1414", "vS1618", "vS2618"), ], 
                by = c("ticker", "trdate", "trdtme", "allrow"))

setnames(all03, c("SUPERT05_10_2", "SUPERTd05_10_2"), 
         c("st05", "std05") )


# long002 = ifelse(DIp / DIn > 2.5 & ADX <= 30 & DIp <= 35, 1, 0)
all03 <- all03 [, `:=`(long001 = ifelse(std05 == 1 & std15 == 1 & std30 == 1 & std05mfi == 1 & std15mfi == 1 & std30mfi == 1, 1, 0),
                       long001a = ifelse(std05 == 1 & std15 == 1 & std05mfi == 1 & std15mfi == 1, 1, 0),
                       long002 = ifelse(DIp / DIn > 3 & ADX <= 30, 1, 0),
                       long002a = ifelse(DIp / DIn > 3 & ADX >= 25, 1, 0),
                       prc003 = ifelse((((price.close - pclose)/ pclose) * 100) < 0.95, 1, 0),
                       
                       short001 = ifelse(std05 == -1 & std15 == -1 & std30 == -1, 1, 0),
                       short002 = ifelse(DIn / DIp > 3 & ADX <= 30, 1, 0),
                       
                       up = ifelse(price.close - price.open >= 0, 1, 0),
                       dn = ifelse(price.close - price.open < 0, 1, 0),
                       subrow02 = as.ITime (as.ITime("09:15") + (subrow-1)*5*60 ) ),]


##################################################################################################
#
# Part 4
# For trade management
#
#
##################################################################################################

trial001 <- copy(all03)

#output <- trial001 [long001 == 1 & long002 == 1 & prc003 == 1 & up == 1 & subrow >= 3  & subrow <= 65] # & entry_c]

# Check only for the Price ST and MFI ST confirmation, so do not check the additional conditions 
output <- trial001 [long001a == 1 & long002a == 1 & subrow >= 3  & subrow <= 65] # & entry_c]

#output <- trial001 [short001 == 1 & short002 == 1 & prc003 == 1 & dn == 1]

# See if the EMA cross over OR ST change as the primary signal
#output <- trial001 [((up_st == 1 & rows_st <= 5) |(up_ema == 1 & rows_ema <= 5)) & up_adx == 1 & rows_adx <= 5 & nrank <= 5 & perchg <= 0.75]

output <- output [, subset := 1:.N, by =.(ticker, trdate)]

output02 <- output [ subset == 1]
output02 <- output02 [, c("ticker", "trdate", "subrow", "subrow02", "price.open", "price.high", "price.low", "price.close"), ]
output02 <- output02 [, signal := 1, ]

setnames(output02, "price.open", "entry_o")
setnames(output02, "price.high", "entry_h")
setnames(output02, "price.low", "entry_l")
setnames(output02, "price.close", "entry_c")
setnames(output02, "subrow", "entry_row")
setnames(output02, "subrow02", "entry_time")

# Merge this data with the original data

trial002 <- merge (x = trial001, 
                   y = output02, 
                   by = c("ticker", "trdate"),
                   all.y = TRUE)

# Create execution row when the entry price is greater than price.high
# ??? Need to check this calculation

trial002exe <- trial002 [, exe := ifelse(subrow > entry_row & price.high > entry_h, 1, 0), ]
trial002exe <- trial002exe [exe == 1 ]
trial002exe <- trial002exe [, exe_row := min(subrow), by = .(trdate, ticker)]
trial002exe <- unique( trial002exe [, c("ticker", "trdate", "entry_row", "exe_row"), ] )

trial002exe <- trial002exe [ order(trdate, entry_row) ]
trial002exe <- trial002exe [, tradeorder := 1:.N, by = .(trdate)]
#output02 <- output02 [ tradeorder <= 5]

trial002 <- merge (x = trial002, 
                   y = trial002exe, 
                   by = c("ticker", "trdate", "entry_row"),
                   all.y = TRUE)

trial002 <- trial002 [ subrow >= exe_row ] # [ subrow >= entry_row]
trial002 <- trial002 [, c("ticker", "trdate", "subrow", "subrow02", "entry_row", "exe_row", "signal",
                          "entry_o", "entry_h", "entry_l", "entry_c", "tradeorder",
                          "price.open", "price.high", "price.low", "price.close", "volume", "st05",
                          "vR0", "vR0236", "vR0382", "vR05", "vR0618", "vR0786", "vR1", "vR1272", 
                          "vR1414", "vR1618", "vR2618"), ]

trial002 <- trial002 [, temp_prc := round( (entry_h * 1.002)/5, 2),  ]
trial002 <- trial002 [, nshares := round( (200000 / temp_prc) * 1 , 0),  ]

trial002 <- trial002 [ order(-trdate, -subrow) ]

trial002_t <- melt.data.table(data = trial002 [ subrow == exe_row ],
                              id.vars = c("ticker", "trdate", "subrow", "subrow02", "entry_row", "exe_row",  "signal",
                                          "entry_o", "entry_h", "entry_l", "entry_c", "tradeorder", "temp_prc", "nshares",
                                          "price.open", "price.high", "price.low", "price.close", "volume", "st05"), 
                              measure.vars = c("vR0", "vR0236", "vR0382", "vR05", "vR0618", "vR0786", "vR1", "vR1272", 
                                               "vR1414", "vR1618", "vR2618"))

trial002_t <- trial002_t [, value := as.numeric( round(value, 2 )), ]
trial002_t <- trial002_t [, dist := value - entry_h, ]
trial002_t <- trial002_t [ order (-trdate, tradeorder, ticker, dist)]

#######################################################
#
# Fibo series gives the targets T01, T02, T03, ...
# Supertrend values give the SL or TSL
#
# Use the Fib levels where the distance between the CMP and
# Fibo is +ve
#
# Get the ICICI company name merged onto the dataset
#
#######################################################

trial002_t02 <- trial002_t [dist > 0]
trial002_t02 <- trial002_t02 [, tgt := 1:.N, by = .(trdate, ticker)]

trial002_t02 <- merge (x = trial002_t02,
                       y = icici_fno [, c("SYMBOL02", "ShortName"), ],
                       by.x = c("ticker"),
                       by.y = c("SYMBOL02"),
                       all.x = TRUE)

trial004 <- merge(x = trial001 [, c("ticker", "trdate", "subrow", "subrow02", "price.open", "price.high", "price.low", "price.close", "st05"), ],
                  y = trial002_t02 [, c("ticker", "trdate", "entry_row", "exe_row", "entry_o", "entry_h", "entry_l", "entry_c", "tradeorder", "variable", "value", "dist", "tgt", "ShortName",  "temp_prc", "nshares"), ],
                  by = c("ticker", "trdate"),
                  allow.cartesian = TRUE)

trial004 <- trial004 [ subrow >= exe_row ]

###########################################################
#
# When the SL is hit create an indicator of 1
# When targets are hit create an indicator of 1
#
# Calculate row numbers for SL and TGTs
# If row_sl > row_tgt
#
# The SL should be hit after the target to get into profit
#
# Get the row number for SL
# Rows between the entry and SL should be the profit booking 
# These will come from either T01, T02, T03 or from st05
#
###########################################################

trial004 <- trial004 [, `:=` (dist_sl = ifelse(price.low - st05 > 0, 0, 1), 
                              dist_tgt = ifelse(price.high - value > 0, 1, 0) ),] 

trial004 <- trial004 [, cum_sl := cumsum(dist_sl), by =.(ticker, trdate)]
trial004 <- trial004 [, cum_tgt := cumsum(dist_tgt), by =.(ticker, trdate, tgt)]

tgt001 <- trial004 [ cum_tgt == 1 & tgt <= 4, c("ticker", "trdate", "subrow", "tgt", "subrow02","entry_h", "value", "st05", "dist", "nshares", "entry_row", "exe_row", "tradeorder"), ]
tgt001 <- tgt001 [, tgt_max := max(tgt), by = .(ticker, trdate)]
tgt002 <- unique( tgt001 [ tgt == tgt_max, c("ticker", "trdate", "tgt_max", "subrow"), ] )
tgt002 <- tgt002 [, tgt_row := min(subrow), by = .(ticker, trdate)]
tgt002 <- tgt002 [ tgt_row == subrow ]

tgt003 <- merge (x = tgt001,
                 y = tgt002 [, c("ticker", "trdate", "tgt_row"), ],
                 by =c("ticker", "trdate"))

tgt003 <- tgt003 [ subrow <= tgt_row]

sl001 <- trial004 [ cum_sl == 1, c("ticker", "trdate", "subrow"), ]
sl001 <- sl001 [, sl_row := min(subrow), by = .(ticker, trdate)]
sl001 <- sl001 [, -c("subrow"), ]
sl001 <- unique( sl001)

trial004 <- merge(x = trial004,
                  y = sl001,
                  by = c("ticker", "trdate") )

#trial004a <- trial004 [ subrow <= sl_row & cum_tgt == 1]

# Get the sl_row - 1 and get the st05 so that the trailing stop can be calculated

sl002 <- trial004 [ subrow == sl_row - 1 ] 
sl002 <- sl002 [, dist := st05 - entry_c, ]
sl002 <- unique( sl002 [, c("ticker", "trdate", "subrow", "subrow02", "st05", "entry_h", "value", "dist", "nshares", "entry_row", "exe_row", "tradeorder"), ] )
sl002 <- sl002 [, tgt := 99, ]

sl004 <- rbind(sl002, tgt003 [, -c("tgt_max", "tgt_row"), ])

# Fix a problem of the same target appearing multiple times
# Subset only for nexits = 1
sl004 <- sl004 [, nexits := 1:.N, by = .(trdate, ticker, tgt)]
sl005 <- sl004 [ nexits == 1 ]

# Split the shares by 3 and multiply by the dist value
sl005 <- sl005 [, pnl := dist * nshares /3 * 0.65, ]
#sl005 <- sl005 [, pnl := (value - st05) * nshares /3 * 0.65, ]

# Calculate pnl for each ticker per day and overall pnl
# Create 2 combinations:
#
# TGT 2, 3, 4: meaning T02, T03, T04 will be hit
# TGT 2, 3, 99: T02, T03 hit + last part will trailing SL using ST
# Calculate number of trades per day

sl005 <- sl005 [ order(-trdate, tradeorder, ticker, subrow, tgt)]

#sl005_234 <- sl005 [ tgt %in% c(2, 3, 4) & tradeorder <= 10]
#sl005_234 <- sl005_234 [, pnl_tick := sum(pnl), by = .(trdate, ticker)]
#sl005_234 <- sl005_234 [, pnl_date := sum(pnl), by = .(trdate)]
#sl005_234 <- sl005_234 [, ntrades := uniqueN(ticker), by = .(trdate)]

#sum(sl005_234$pnl)

#fwrite(sl005_234, "D:/My-Shares/analysis/trial004_234.csv")

#sl005_2399 <- sl005 [ tgt %in% c(2, 3, 99) & tradeorder <= 10]
#sl005_2399 <- sl005_2399 [, pnl_tick := sum(pnl), by = .(trdate, ticker)]
#sl005_2399 <- sl005_2399 [, pnl_date := sum(pnl), by = .(trdate)]
#sl005_2399 <- sl005_2399 [, ntrades := uniqueN(ticker), by = .(trdate)]
#sum(sl005_2399$pnl)

#fwrite(sl005_2399, "D:/My-Shares/analysis/trial004_2399.csv")

sl005 <- sl005 [, type := paste(tgt, collapse = ",", sep =""), by =.(trdate, ticker)]
sl005_99 <- sl005 [tgt == 99, c("trdate", "ticker", "subrow"), ]
setnames(sl005_99, "subrow", "subrow99")

sl005a <- merge (x = sl005,
                 y = sl005_99,
                 by = c("trdate", "ticker"),
                 all.x = TRUE)

sl005a <- sl005a [, subrow99 := ifelse( is.na(subrow99), 99, subrow99), ]
sl006 <- sl005a [ subrow <= subrow99] 

sl006 <- sl006 [, type := paste(tgt, collapse = ",", sep =""), by =.(trdate, ticker)]
#comb002 <- unique(sl006 [, c("trdate", "ticker", "tradeorder", "type"), ])
#comb003 <- comb002 [tradeorder <= 10, .(ncount = length(ticker) ), by =.(type)]
#comb003 <- comb003 [, tot := sum(ncount), ]

sl006 <- sl006 [, include := ifelse(type == "1,99" & tgt == 1, 0, 1), ]
sl006 <- sl006 [, pnl := ifelse(type %in% c("1, 99", "99"), nshares * dist, pnl), ]

sl007 <- sl006 [include == 1 & tgt < 99, .(pnl_tot = sum(pnl) ), by = .(trdate, type)]
sl007 <- sl007 [, pnl_day := sum(pnl_tot), by = .(trdate)]
sl007 <- sl007 [, pnl_all := cumsum(pnl_tot), ]
sum(sl007 [! type %in% c("99") ]$pnl_tot)

sl006 <- sl005a [ subrow <= subrow99] 
sl006 <- sl006 [, type := paste(tgt, collapse = ",", sep =""), by =.(trdate, ticker)]

#########################################################
#
# If all 3 targets are not hit then there is a need to 
# derive another target at 3:20, say tgt = 98
#
# When type is in (1, "1,2", "1,2,3") get the 3:20 subrow value
# and use the st05 value as the possible exit
#
# Merge this data with subrow == 74
#
#########################################################
sl006a <- sl006 [ type %in% c("1", "1,2", "1,2,3") ]
sl006a <- unique(sl006a [, -c("subrow", "subrow02", "tgt", "pnl", "st05"), ])

sl006b <- merge (x = sl006a,
                 y = trial001 [ subrow == 74, c("trdate", "ticker", "price.close", "subrow", "subrow02"), ],
                 by = c("trdate", "ticker"),
                 all.x = TRUE)

###################################################################
#
# Calculate the end of the 3:20 profit with price.close and ST05,
# but for merging the data rename the variable to ST05
#
###################################################################

setnames(sl006b, "price.close", "st05")
sl006b <- sl006b [, `:=` (dist = st05 - entry_h, tgt = 98, subrow99 = 98, pnl = 0), ]

sl006c <- rbind(sl006, sl006b)
sl006d <- sl006c [ tgt > 1]
sl006d <- sl006d [, shares_mult := case_when(type %in% c("1", "1,99", "99") ~ nshares,
                                             type %in% c("1,2", "1,2,99") & tgt == 2 ~ nshares * 0.33,
                                             type %in% c("1,2", "1,2,99") & tgt %in% c(98, 99) ~ nshares * 0.66,
                                             type %in% c("1,2,3,4,99") & tgt == 99 ~ nshares * 0,
                                             type %in% c("1,2,3", "1,2,3,4", "1,2,3,99", "1,2,3,4,99") ~ nshares * 0.33), ]

sl006d <- sl006d [, pnl := dist * shares_mult * 0.65, ]
sl006d <- sl006d [, pnl_tick := sum(pnl), by = .(trdate, ticker)]
sl006d <- sl006d [, pnl_date := sum(pnl), by = .(trdate)]
sl006d <- sl006d [, ntrades := uniqueN(ticker), by = .(trdate)]

sl007 <- sl006d [ tradeorder <= 5 ]
sl007 <- sl007 [, .(pnl_tot = sum(pnl) ), by = .(trdate, type)]
sl007 <- sl007 [, pnl_day := sum(pnl_tot), by = .(trdate)]
sl007 <- sl007 [, pnl_all := cumsum(pnl_tot), ]

sl008 <- sl007 [, .(cum_pnl = sum(pnl_tot)), by = .(type)]
sl008 <- sl008 [, grp := ifelse(cum_pnl < 0, "Loss", "Profit"), ]
sl008 <- sl008 [, tyep_pnl := sum(cum_pnl), by = .(grp)]

View(sl007)
View(sl008)