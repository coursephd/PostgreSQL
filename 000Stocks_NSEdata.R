library(data.table)
library(tidyverse)
library(fCertificates)

S <- seq(0, 100)
prices <- Straddle(S, X=50, Time=0, r=0.05, r_d=0, sigma=0.2, ratio = 1)
plot(S, prices, type="l", xlab="underlying price", ylab="payoff") 

## Straddle payoff diagram
S <- seq(0, 100)
ps1 <- Straddle(S, X=45, Time=1, r=0.01, r_d=0, sigma=0.3, ratio=1)
ps2 <- Straddle(S, X=45, Time=0, r=0.01, r_d=0, sigma=0.3, ratio=1)
ps3 <- Straddle(S, X=45, Time=1, r=0.01, r_d=0, sigma=0.4, ratio=1)

plot(S, ps2, type="l", col="red", xlab="underlying price", 
     ylab="payoff", main="Straddle")
lines(S, ps1, col="blue")
lines(S, ps3, col="green")
abline(v=45, lty=2, col="gray80")

##
DiscountCertificate(S=40, X=42, Time=1, r=0.035, r_d=0, sigma=0.3, ratio=1)

## payoff diagram
S <- seq(0, 100)
p <- DiscountCertificate(S, X=42, Time=1, r=0.035, r_d=0, sigma=0.3, ratio=1)
p2 <- DiscountCertificate(S, X=42, Time=0, r=0.035, r_d=0, sigma=0.3, ratio=1)
plot(S, p,  type="l", col="red", , ylim=range(p, p2, na.rm=TRUE), 
     xlab="underlying price", ylab="payoff", main="Discount")
lines(S, p2, col="blue")
abline(v=42, lty=2, col="gray80")



library(data.table)
# https://www.nseindia.com/products/content/derivatives/equities/homepage_fo.htm

# Contract size automation
cntrt <- fread("https://www.nseindia.com/content/fo/fo_mktlots.csv")

# Bhavcopy automation
#bhavcopy <- fread("curl https://www.nseindia.com/content/historical/DERIVATIVES/2019/JAN/fo10JAN2019bhav.csv.zip | funzip")
temp <- tempfile()
download.file("https://www.nseindia.com/content/historical/DERIVATIVES/2019/JAN/fo10JAN2019bhav.csv.zip", temp)
bhavcopy <- fread(unzip(temp, files = "fo10JAN2019bhav.csv"))
rm(temp)

# Daily volatality automation
volality <- fread("https://www.nseindia.com/archives/nsccl/volt/FOVOLT_10012019.csv")

# Daily settlement Prices
settle <- fread("https://www.nseindia.com/archives/nsccl/sett/FOSett_prce_10012019.csv")

# Merge contract size and bhavcopy

bhavcopy02 <- merge (x = cntrt,
                     y = bhavcopy,
                     by = c("SYMBOL"))

bhavcopy02 <- merge (x = bhavcopy02,
                     y = volality,
                     by.x = c("SYMBOL"),
                     by.y = c("Symbol"))

bhavcopy03 <- bhavcopy02 [CONTRACTS > 0 & INSTRUMENT == "OPTSTK"]

# Count the total number of CONTRACTS
bhavcopy03 <- bhavcopy03 [, totcontract := sum(CONTRACTS), by = .(SYMBOL, INSTRUMENT, EXPIRY_DT, OPTION_TYP)]


# https://www.investopedia.com/ask/answers/06/putcallratio.asp

unq <- unique(bhavcopy03 [, c("SYMBOL", "OPTION_TYP", "totcontract", "EXPIRY_DT"), ] )
# Create Put and Call counts columns and ratios
bhavcopy030 <- dcast( unq,
                     formula = SYMBOL + EXPIRY_DT ~ OPTION_TYP,
                     value.var =c("totcontract"),
                     fill = "0")
bhavcopy030 <- bhavcopy030 [, putcallratio := PE / CE,]

bhavcopy031 <- merge (x = bhavcopy03,
                      y = bhavcopy030,
                      by = c("SYMBOL", "EXPIRY_DT"))


# A rising put-call ratio or greater than .7 or exceeding 1 means equity traders 
# are buying more puts than calls and indicates a bearish sentiment 
# is building in the market

# A falling put-call ratio or below .7 and approaching .5 is considered bullish 
# since it means more calls are being bought versus puts. 
# In other words, the market has a bullish sentiment. 

# Count total companies >= 500
bhavcopy04 <- bhavcopy03 [ totcontract >= 500]

# Merge 


library(XML)

bhrt <- "https://www.nseindia.com/live_market/dynaContent/live_watch/option_chain/optionKeys.jsp?symbolCode=2002&symbol=BHARTIARTL&symbol=BHARTIARTL&instrument=-&date=-&segmentLink=17&symbolCount=2&segmentLink=17"
        
bhrt.table = readHTMLTable(bhrt, header=T, which=1,stringsAsFactors=F)

https://www.nseindia.com/live_market/dynaContent/live_watch/option_chain/optionDates.jsp?symbol=BHARTIARTL&instrument=OPTSTK&strike=240.00


curl 'https://www.nseindia.com/live_market/dynaContent/live_watch/option_chain/optionKeys.jsp?symbolCode=2002&symbol=BHARTIARTL&symbol=BHARTIARTL&instrument=-&date=-&segmentLink=17&symbolCount=2&segmentLink=17' -H 'Connection: keep-alive' -H 'Cache-Control: max-age=0' -H 'Upgrade-Insecure-Requests: 1' -H 'User-Agent: Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/71.0.3578.98 Safari/537.36' -H 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8' -H 'Accept-Encoding: gzip, deflate, br' -H 'Accept-Language: en-US,en;q=0.9' -H 'Cookie: _ga=GA1.2.892238988.1540559858; sym1=INFY; underlying2=JSWSTEEL; instrument2=FUTSTK; optiontype2=-; expiry2=31JAN2019; strikeprice2=-; underlying3=TCS; instrument3=FUTSTK; optiontype3=-; expiry3=31JAN2019; strikeprice3=-; underlying4=DABUR; instrument4=FUTSTK; optiontype4=-; expiry4=31JAN2019; strikeprice4=-; sym2=BHARTIARTL; _gid=GA1.2.767273783.1547002258; underlying1=BHARTIARTL; instrument1=OPTSTK; optiontype1=PE; expiry1=; strikeprice1=320.00; pointerfo=1; sym3=PETRONET; pointer=3; NSE-TEST-1=1960845322.20480.0000; JSESSIONID=D4838425F43938C9003BAA1D287859E0.tomcat2; ak_bmsc=D06B5BF57A85BE02C18ED9EA55F2ED297370029A7827000006FC375C6B834938~plyckciyyBjWss3r6abUjbQ4QfCFhLe/nrTWanJLdSUxX0wYEZHrZTx0uHH/Z95FTtpWn7T1Jn8Z4hb9GQ2RQpW8vwmyGCNSqbPAucGNoIT9RQ58A8GQe9L3/B+XDzPMiVl86zjtG45xfxVWF46gdanJmGi05kRvFDGUJy4pyU0yOt8BkLWn6q0z95H1i/4WS5q2XaaNQsMXeu3UXFn402M6E7IsaCtLoTg/s/2uL05C0=; bm_sv=2B067AC61EA410F9830F7A333B35CDF1~0+0YjOfBt0xzGJI252K0gDASQjJrEw4xy89pm99lyonEeS1bKYQi8O3BEoRqeJlbd2A1oXbmkWfQrpZqrczYJX0xZjVYJZn6BqCeKVi5O4FTXJojzXefDurRX+1poLlb0oXeW03Iyer7MSbSsLbee2nbiS7Wxv0MAUFLTHNfI60=' --compressed 