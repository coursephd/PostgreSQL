
library(data.table)
library(RCurl)
library(lubridate)
library(curl)

# Data coming from ICICI breeze documentation:

icici_url <- "https://directlink.icicidirect.com/NewSecurityMaster/SecurityMaster.zip"
download.file(icici_url, "D:/My-Shares/analysis/SecurityMaster.zip")
unzip(zipfile = "D:/My-Shares/analysis/SecurityMaster.zip", exdir = "D:/My-Shares/analysis")

# List of compnay names present in ICICI fno
icici_fno <- fread("D:/My-Shares/analysis/FONSEScripMaster.txt")
icici_fno <- unique( icici_fno [, c("ShortName"), ])

# List of company names present in ICICI master
icici_mstr <- fread("D:/My-Shares/analysis/NSEScripMaster.txt")
icici_mstr <- unique( icici_mstr [, c("ShortName", "CompanyName", "ISINCode"), ])

# Only keep the fno names
icici_fno <- merge(x = icici_fno,
                   y = icici_mstr,
                   by = c("ShortName"), 
                   all.x = TRUE)

# Nifty500 list has the ISIN code for merging
nifty500 <- fread("https://www1.nseindia.com/content/indices/ind_nifty500list.csv")
setnames(nifty500, "ISIN Code", "ISINCode")

# Keep the ICICI name, ISIN code and Symbol coming from NSE
# Symbol + ".NS" is used by yahoo finance to get the company information

icici_fno <- merge(x = icici_fno,
                   y = nifty500,
                   by = c("ISINCode"), 
                   all.x = TRUE)
icici_fno <- icici_fno [ ISINCode != ""]

# Create a new variable to get the ".NS" added at the end of the company name
icici_fno <- icici_fno [, `:=` (nrow = .I, SYMBOL02 = paste(Symbol, ".NS", sep="") ), ]

saveRDS (icici_fno, "D:/My-Shares/analysis/icici_fno.rds")
