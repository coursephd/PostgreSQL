library(data.table)
library(tidyverse)
library(lubridate)
library(anytime)
library(bizdays)
library(rvest)
library(xml2)
library(openxlsx)
library(RCurl)
library(curl)

library(ggplot2)
library(ggpubr)
library(plotly)

library(TTR)
library(tidyquant)
library(QuantTools)
library(derivmkts)
library(quantmod)

options(scipen = 999)

#
# Extract only specific files 
#
# https://stackoverflow.com/questions/32870863/extract-certain-files-from-zip
# https://stackoverflow.com/questions/32870863/extract-certain-files-from-zip
# https://stackoverflow.com/questions/31146263/sys-glob-within-unzip

#########################################################
#
# Step 1:
#
# Download the files from the nse website:
#
# Example calls of the data download
# Check for the existence of the file
# if it exists then only download
#
# if (curl_fetch_memory('https://www1.nseindia.com/archives/equities/bhavcopy/pr/PR010311.zip')$status_code == 200) download.file('https://www1.nseindia.com/archives/equities/bhavcopy/pr/PR010311.zip', 'D:\\My-Shares\\source-bhavcopy\\PR010311.zip')
# if (curl_fetch_memory('https://www1.nseindia.com/archives/equities/bhavcopy/pr/PR020311.zip')$status_code == 200) download.file('https://www1.nseindia.com/archives/equities/bhavcopy/pr/PR020311.zip', 'D:\\My-Shares\\source-bhavcopy\\PR020311.zip')
# if (curl_fetch_memory('https://www1.nseindia.com/archives/equities/bhavcopy/pr/PR030311.zip')$status_code == 200) download.file('https://www1.nseindia.com/archives/equities/bhavcopy/pr/PR030311.zip', 'D:\\My-Shares\\source-bhavcopy\\PR030311.zip')
# if (curl_fetch_memory('https://www1.nseindia.com/archives/equities/bhavcopy/pr/PR040311.zip')$status_code == 200) download.file('https://www1.nseindia.com/archives/equities/bhavcopy/pr/PR040311.zip', 'D:\\My-Shares\\source-bhavcopy\\PR040311.zip')
#
# The following statement executes the actual column 
# eval(parse (text = variable name))
#
#########################################################

step001 <- data.table ( read.xlsx("D:\\My-Shares\\prgm\\0500_rakeshpujara_atmlong.xlsx", 2) )
#step002 <- step001 [Date <= 40612]
step002 <- step001 [, Date := anydate(Date), ]
step003 <- step001 [, dwn := eval(parse(text = download)),]

#########################################################
#
# Step 2:
#
# Unzip the files for the 1st time as the fno file is inside the BIG file 
#
# Check if the zip file exists then unzip
#
# if file.exists("leaflet.R") unzip
#
# Example calls
# if (file.exists('D:\\My-Shares\\source-bhavcopy\\PR010311.zip')) unzip(zipfile = 'D:\\My-Shares\\source-bhavcopy\\PR010311.zip', exdir = 'D:/My-Shares/source-fno')
# if (file.exists('D:\\My-Shares\\source-bhavcopy\\PR020311.zip')) unzip(zipfile = 'D:\\My-Shares\\source-bhavcopy\\PR020311.zip', exdir = 'D:/My-Shares/source-fno')
# if (file.exists('D:\\My-Shares\\source-bhavcopy\\PR030311.zip')) unzip(zipfile = 'D:\\My-Shares\\source-bhavcopy\\PR030311.zip', exdir = 'D:/My-Shares/source-fno')
# if (file.exists('D:\\My-Shares\\source-bhavcopy\\PR040311.zip')) unzip(zipfile = 'D:\\My-Shares\\source-bhavcopy\\PR040311.zip', exdir = 'D:/My-Shares/source-fno')
#
# The following step creates some warning messages but 
# the unzipping of the files takes place
# If this could be sorted out then try
#
#########################################################

step003 <- step003 [, eval(parse(text = unzip1)),]

#########################################################
#
# Step 3:
#
# Now unzip the fno file which was created in step 2
#
#########################################################

step004 <- step002 [, eval(parse(text = unzip_fno_csv)),]

#########################################################
#
# Step 4:
#
# Read only few lines from the CSV files into R data.table
# https://readr.tidyverse.org/reference/read_delim_chunked.html
#
# The futures data should be read into fo files
# The options data should be read into op files
#
# if (file.exists('D:\\My-Shares\\source-fno-csv\\fo01032011.csv')) fo01032011 = data.table (read_csv_chunked('D:\\My-Shares\\source-fno-csv\\fo01032011.csv', DataFrameCallback$new(f), chunk_size = 5))
# if (file.exists('D:\\My-Shares\\source-fno-csv\\op01032011.csv')) op01032011 = data.table (read_csv_chunked('D:\\My-Shares\\source-fno-csv\\op01032011.csv', DataFrameCallback$new(f), chunk_size = 5))
#
#########################################################
f <- function(x, pos) subset(x, SYMBOL %in% c("BANKNIFTY", "NIFTY") )
#fo <- data.table (read_csv_chunked("D:\\My-Shares\\source-fno-csv\\fo*.csv", DataFrameCallback$new(f), chunk_size = 5) )

step002_yr <- step002 [ Year4 >= 2017 & Year4 <= 2019]

# Do the calculations for Futures data

eval(parse(text = step002_yr$fut_rdata))
all01_fut <- rbindlist(mget(ls(pattern = "fo")), fill = TRUE, idcol = "file_fut")
all01_fut <- all01_fut [, trdate := dmy ( substr(file_fut, 3, 20) ), ]

rm(list = ls( pattern = "^fo") )

# Do the calculations for Options data

eval(parse(text = step002_yr$opt_data))
all01_opt <- rbindlist(mget(ls(pattern = "op")), fill = TRUE, idcol = "file_opt")
all01_opt <- all01_opt [, trdate := dmy ( substr(file_opt, 3, 20) ), ]
setnames(all01_opt, paste("opt", names(all01_opt), sep = "_"))

rm(list = ls( pattern = "^op") )

all01_fut <- all01_fut [, `:=` (exp_mon = month( dmy(EXP_DATE) ),
                                exp_yr = year( dmy(EXP_DATE) ) ), ]

all01_opt <- all01_opt [, `:=` (exp_mon = month( dmy(opt_EXP_DATE) ),
                                exp_yr = year( dmy(opt_EXP_DATE) ) ), ]


all02 <- merge(x = all01_fut,
               y = all01_opt,
               by.x = c("SYMBOL", "exp_mon", "exp_yr", "trdate"),
               by.y = c("opt_SYMBOL", "exp_mon", "exp_yr", "opt_trdate"))


# All the variables
all02names <- names(all02)

# Variables should not be converted to numeric:
all02char <- c("SYMBOL", "EXP_DATE", "opt_EXP_DATE", "exp_mon", "exp_yr", "trdate", "file_fut", "INSTRUMENT", "opt_file_opt", "opt_INSTRUMENT", "opt_OPT_TYPE" )

# Only variables to be converted to numeric
all02num <- setdiff(all02names, all02char)

all02 <- all02 [, (all02num) := lapply(.SD, as.numeric), .SDcols = all02num]
setnames(all02, "OPEN_INT*", "OPEN_INT")

#########################################################
#
# Step 5:
#
# Based on the future data values, calculate the ATM
# 
# min_nifty <- ifelse(nifty0 < 10000, signif(nifty0, 2), signif(nifty0, 3) )
#
#########################################################
all02 <- all02 [, `:=` (atm_hi = ifelse(HI_PRICE < 10000, signif(HI_PRICE, 2), signif(HI_PRICE, 3) ),
                        atm_lo = ifelse(LO_PRICE < 10000, signif(LO_PRICE, 2), signif(LO_PRICE, 3) ),
                        atm_open = ifelse(OPEN_PRICE < 10000, signif(OPEN_PRICE, 2), signif(OPEN_PRICE, 3) ),
                        atm_close = ifelse(CLOSE_PRICE < 10000, signif(CLOSE_PRICE, 2), signif(CLOSE_PRICE, 3) ) ),]
all02 <- all02 [, `:=` (expdt_fut = dmy(EXP_DATE),
                        expdt_opt = dmy(opt_EXP_DATE) ), ]

# Create a combination of expiry date and trading date
lookup001 <- unique( all02 [ , c("SYMBOL", "trdate", "expdt_fut", "expdt_opt"), ])

# Calculate number of days between the trade date and expiry date
lookup001 <- lookup001 [, numdays := as.numeric(expdt_opt - trdate + 1),]

# Only keep the trade dates which are <= 30 days
lookup002 <- lookup001 [ numdays <= 30]

# Sort the data
lookup002 <- lookup002 [ order(SYMBOL, trdate, expdt_fut, expdt_opt)]

# Calculate the expiry date number
lookup002 <- lookup002 [, `:=`(trdid = .GRP, nexp = 1:.N), by =.(SYMBOL, trdate, expdt_fut, expdt_opt)]

# Create 1 record per date to understand the length of the trade
lookup003 <- lookup002[, list(SYMBOL = SYMBOL, expdt_fut = expdt_fut, expdt_opt = expdt_opt,
                              trdate = trdate, nexp = nexp, trdid = trdid, numdays = numdays,
                              rundt = anydate( seq(trdate, expdt_opt, by = "day") ) ), by = 1:nrow(lookup002)]

lookup003 <- lookup003 [, ndaysintrade := 1:.N, by = .(SYMBOL, trdid, expdt_fut, expdt_opt, nexp, numdays)]

# Get the first day of each trade and then merge it with the complete bhavcopy data
lookup004 <- lookup003 [ ndaysintrade == 1, c("SYMBOL", "expdt_fut", "expdt_opt", "trdate", "trdid"), ]

# Merge the possible trade number data with the complete data to get the ATM values from
# day 1

all03 <- merge(x = all02,
               y = lookup004,
               by = c("SYMBOL", "expdt_fut", "expdt_opt", "trdate"),
               all.y = TRUE)

all03 <- all03 [ opt_STR_PRICE == atm_open, 
                 c("SYMBOL", "expdt_fut", "expdt_opt", "trdate", "trdid", "atm_open", "opt_OPEN_PRICE", 
                   "opt_OPT_TYPE"),]
setnames(all03, "atm_open", "atm_open_d1")
setnames(all03, "opt_OPEN_PRICE", "OPEN_PRICE_d1")

all03_1 <- unique ( all03 [ , c("SYMBOL", "expdt_fut", "expdt_opt", "trdate", "trdid", "atm_open_d1"), ] )

all03_2  <- dcast(data = all03,
                  SYMBOL + expdt_fut + expdt_opt + trdate + trdid + atm_open_d1 ~ paste("open_d1", opt_OPT_TYPE, sep="_"),
                  value.var = c("OPEN_PRICE_d1"))

# Merge day 1 information with rest of the data lookup004

all04 <- merge (x= all03_1,
                y = lookup003,
                by = c("SYMBOL", "expdt_fut", "expdt_opt", "trdate", "trdid") )

all05 <- merge (x = all04,
                y = all02,
                by.x = c("SYMBOL", "expdt_fut", "expdt_opt", "rundt", "atm_open_d1"),
                by.y = c("SYMBOL", "expdt_fut", "expdt_opt", "trdate", "opt_STR_PRICE"),
                all.x = TRUE)
all05 <- all05 [ OPEN_PRICE > 0]

# Transpose the data to check if the trade is successful or not
all06 <- dcast(data = all05, 
               SYMBOL + trdid + trdate + rundt + expdt_fut + expdt_opt + 
               numdays + ndaysintrade + atm_open_d1 + atm_open ~ opt_OPT_TYPE,
               value.var = c("opt_OPEN_PRICE") )

all07 <- merge(x = all06,
               y = all03_2,
               by = c("SYMBOL", "expdt_fut", "expdt_opt", "trdate", "trdid", "atm_open_d1") )
all07 <- all07 [, `:=`(opnCEPE = open_d1_CE + open_d1_PE,
                        clsCEPE = CE + PE,
                        diffSTRK = abs(atm_open_d1 - atm_open) ),]
all07 <- all07 [, pnl := clsCEPE - opnCEPE,]

# Create flags to get the monthly and weekly expiry
all07 <- all07 [, `:=` (type = ifelse(expdt_fut == expdt_opt, "Monthly", "Weekly"), 
                        rundt_yr = year(rundt),
                        rundt_mon = month(rundt) ),]

# If the numdays = ndaysintrade subset is done then, it shows the day of expiry
# Check how does the data look like on that last date
all08 <- all07 [ numdays == ndaysintrade ]


all08_1 <- dcast(data = all08 ,#[ type == "Monthly"],
                 SYMBOL + type + rundt_yr + numdays ~ paste("month", rundt_mon, sep="_"),
                 value.var = c("pnl") )

#####################################################################################
#
# End of program
#
#####################################################################################


# (1) Rakesh Pujara strategy
# (2) Short ATM Call and Put on Tuesday and 25% is SL
# (3) Arbitrage opportunity calculations: Long ATM call + Short ATM Put + Short Futures = 0`  `


#https://stackoverflow.com/questions/32870863/extract-certain-files-from-zip
#https://stackoverflow.com/questions/31146263/sys-glob-within-unzip

# 1st step: unzip of the big file and only unzip the zip file:
zipped_names <- grep('\\.zip$', unzip('C:\\Users\\mahajvi1\\Downloads\\temp\\PR03022011.zip', list=TRUE)$Name,
                     ignore.case=TRUE, value=TRUE)
unzip('C:\\Users\\mahajvi1\\Downloads\\temp\\PR03022011.zip',
      exdir = "C:\\Users\\mahajvi1\\Downloads\\temp",
      files=zipped_names)

# 2nd step: unzip the fno file and only extract 2 files:
files = unzip("C:\\Users\\mahajvi1\\Downloads\\temp\\fo03022011.zip", list=TRUE)$Name
unzip("C:\\Users\\mahajvi1\\Downloads\\temp\\fo03022011.zip",
      exdir = "C:\\Users\\mahajvi1\\Downloads\\temp",
      files=files[grepl("^fo[0-9]{8}\\.csv|^op[0-9]{8}\\.csv",files)])

# 3rd stage: read only few lines from the CSV files into R data.table
#https://readr.tidyverse.org/reference/read_delim_chunked.html

library(readr)

f <- function(x, pos) subset(x, SYMBOL %in% c("BANKNIFTY", "NIFTY") )
fo <- data.table (read_csv_chunked("C:\\Users\\mahajvi1\\Downloads\\temp\\fo03022011.csv", DataFrameCallback$new(f), chunk_size = 5) )


library(data.table)
temp = data.table(readxl::read_excel('C:\\Users\\mahajvi1\\Downloads\\temp\\book1.xlsx'))
temp0 <- temp [, c(1, 2, 3), ]

for (i in unique(temp0$Day)) {
  temp0[, (paste('base',i,sep = '')) := ifelse(Day >= i, AVAL[i], NA), by =.(Parameter) ]
  temp0[, (paste('chg',i,sep='')) := (AVAL - get(paste('base',i,sep = '')))  , by =.(Parameter)]
}



library(data.table)
library(anytime)
library(tidyverse)

# Replace this part by the actual dates from the bhavcopy
# Get the expiry dates and the trade dates

temp = data.table(readxl::read_excel('C:\\Users\\mahajvi1\\Downloads\\temp\\book1.xlsx', sheet=2))
temp <- temp [, expdt := anydate(expdate), ]
exp <- exp [exp == 1, c("expdt"),]

dts <- data.table( startdt = anydate("01-Nov-2019"), enddt = anydate("22-Apr-2021") )
dts2 <- dts [, list(trdate = anydate( seq(startdt, enddt, by = "day") )) ]

dts03 <- data.table ( crossing(exp, dts2) )

# Only keep the records for which trade days are before expiry date
dts03 <- dts03 [ expdt >= trdate]

# Sort the data
dts03 <- dts03 [ order(trdate, expdt)]

# Calculate the expory date number
dts03 <- dts03 [, `:=`(trdid = .GRP, nexp = 1:.N), by =.(trdate)]

# Calculate number of days between the trade date and expiry date
dts03 <- dts03 [, numdays := as.numeric(expdt - trdate + 1),]

# Only keep the trade dates which are <= 30 days
dts04 <- dts03 [ numdays <= 30]

# Create 1 record per date to understand the length of the trade
dts05 <- dts04[, list(expdt = expdt, trdate = trdate, nexp = nexp, trdid = trdid, numdays = numdays,
                      rundt = anydate( seq(trdate, expdt, by = "day") ) ), by = 1:nrow(dts04)]

dts05 <- dts05 [, ndaysintrade := 1:.N, by = .(trdid, nexp, numdays)]



# Till 21-12-2018: fo / op csv files
# After 24-12-2018: excel file: https://www1.nseindia.com/archives/combine_report/combined_report07042021.zip

new <- read_excel("D:\\My-Shares\\prgm\\combined_report07042021.xlsx")
names(new)<- str_replace_all(names(new), c(" " = "" , "," = "" )) 
new <- data.table(new)

new0 <- new [ InstrumentType %in% c("OPTIDX", "FUTIDX") & Symbol %in% c("BANKNIFTY", "NIFTY")]

https://www1.nseindia.com/content/indices/ind_nifty50list.csv
https://www1.nseindia.com/content/indices/ind_niftynext50list.csv
https://www1.nseindia.com/content/indices/ind_nifty100list.csv
https://www1.nseindia.com/content/indices/ind_nifty200list.csv
https://www1.nseindia.com/content/indices/ind_nifty500list.csv
https://www1.nseindia.com/content/indices/ind_niftymidcap150list.csv
https://www1.nseindia.com/content/indices/ind_niftymidcap50list.csv
https://www1.nseindia.com/content/indices/ind_niftymidcap100list.csv
https://www1.nseindia.com/content/indices/ind_niftysmallcap250list.csv
https://www1.nseindia.com/content/indices/ind_niftysmallcap50list.csv
https://www1.nseindia.com/content/indices/ind_niftysmallcap100list.csv
https://www1.nseindia.com/content/indices/ind_niftylargemidcap250list.csv
https://www1.nseindia.com/content/indices/ind_niftymidsmallcap400list.csv

https://www1.nseindia.com/content/indices/ind_niftyautolist.csv
https://www1.nseindia.com/content/indices/ind_niftybanklist.csv
https://www1.nseindia.com/content/indices/ind_niftyconsumerdurableslist.csv
https://www1.nseindia.com/content/indices/ind_niftyfinancelist.csv
https://www1.nseindia.com/content/indices/ind_niftyfinancialservices25_50list.csv
https://www1.nseindia.com/content/indices/ind_niftyfmcglist.csv
https://www1.nseindia.com/content/indices/ind_niftyhealthcarelist.csv
https://www1.nseindia.com/content/indices/ind_niftyitlist.csv
https://www1.nseindia.com/content/indices/ind_niftymedialist.csv
https://www1.nseindia.com/content/indices/ind_niftymetallist.csv
https://www1.nseindia.com/content/indices/ind_niftyoilgaslist.csv
https://www1.nseindia.com/content/indices/ind_niftypharmalist.csv
https://www1.nseindia.com/content/indices/ind_nifty_privatebanklist.csv
https://www1.nseindia.com/content/indices/ind_niftypsubanklist.csv
https://www1.nseindia.com/content/indices/ind_niftyrealtylist.csv


i01_nfity50 <- fread('https://www1.nseindia.com/content/indices/ind_nifty50list.csv')
i02_next50 <- fread('https://www1.nseindia.com/content/indices/ind_niftynext50list.csv')
i03_nifty100 <- fread('https://www1.nseindia.com/content/indices/ind_nifty100list.csv')
i04_nifty200 <- fread('https://www1.nseindia.com/content/indices/ind_nifty200list.csv')
i05_nifty500 <- fread('https://www1.nseindia.com/content/indices/ind_nifty500list.csv')
i06_mid150 <- fread('https://www1.nseindia.com/content/indices/ind_niftymidcap150list.csv')
i07_mid50 <- fread('https://www1.nseindia.com/content/indices/ind_niftymidcap50list.csv')
i08_mid100 <- fread('https://www1.nseindia.com/content/indices/ind_niftymidcap100list.csv')
i09_small250 <- fread('https://www1.nseindia.com/content/indices/ind_niftysmallcap250list.csv')
i10_small50 <- fread('https://www1.nseindia.com/content/indices/ind_niftysmallcap50list.csv')
i11_small100 <- fread('https://www1.nseindia.com/content/indices/ind_niftysmallcap100list.csv')
i12_largemid250 <- fread('https://www1.nseindia.com/content/indices/ind_niftylargemidcap250list.csv')
i13_missmall400 <- fread('https://www1.nseindia.com/content/indices/ind_niftymidsmallcap400list.csv')
i14_auto <- fread('https://www1.nseindia.com/content/indices/ind_niftyautolist.csv')
i15_bank <- fread('https://www1.nseindia.com/content/indices/ind_niftybanklist.csv')
i16_consumer <- fread('https://www1.nseindia.com/content/indices/ind_niftyconsumerdurableslist.csv')
i17_finance <- fread('https://www1.nseindia.com/content/indices/ind_niftyfinancelist.csv')
i18_finance25_50 <- fread('https://www1.nseindia.com/content/indices/ind_niftyfinancialservices25_50list.csv')
i19_fmcg <- fread('https://www1.nseindia.com/content/indices/ind_niftyfmcglist.csv')
i20_healthcare <- fread('https://www1.nseindia.com/content/indices/ind_niftyhealthcarelist.csv')
i21_it <- fread('https://www1.nseindia.com/content/indices/ind_niftyitlist.csv')
i22_media <- fread('https://www1.nseindia.com/content/indices/ind_niftymedialist.csv')
i23_metal <- fread('https://www1.nseindia.com/content/indices/ind_niftymetallist.csv')
i24_oilgas <- fread('https://www1.nseindia.com/content/indices/ind_niftyoilgaslist.csv')
i25_pharma <- fread('https://www1.nseindia.com/content/indices/ind_niftypharmalist.csv')
i26_pvtbank <- fread('https://www1.nseindia.com/content/indices/ind_nifty_privatebanklist.csv')
i27_psubank <- fread('https://www1.nseindia.com/content/indices/ind_niftypsubanklist.csv')
i28_realty <- fread('https://www1.nseindia.com/content/indices/ind_niftyrealtylist.csv')

all01_idx <- rbindlist(mget(ls(pattern = "i")), fill = TRUE, idcol = "index")
rm(list = ls( pattern = "^i") )

# 21-Feb-2012 onwards: ma files are in csv format
# https://www1.nseindia.com/archives/equities/mkt/MA210212.csv


step001 <- data.table ( read.xlsx("D:\\My-Shares\\prgm\\0500_rakeshpujara_atmlong.xlsx", 2) )
step0010 <- step001 [, Date := anydate(Date), ]

step0010_yr <- step0010 [ Year4 >= 2017]

# Do the calculations for Futures data

eval(parse(text = step0010_yr$fread_ma05))
all01_ma <- rbindlist(mget(ls(pattern = "MA")), fill = TRUE, idcol = "index_dt")
all01_ma <- all01_ma [, trdate := dmy ( substr(index_dt, 3, 20) ), ]

rm(list = ls( pattern = "^MA") )
