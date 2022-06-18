library(TTR)
library(tidyquant)
library(QuantTools)
library(derivmkts)
library(quantmod)

library(data.table)
library(tidyverse)
library(anytime)
library(simstudy)
library(zoo)
library(RCurl)
library(lubridate)
library(curl)
library(reshape)

options(scipen = 999)

library(BatchGetSymbols)

future.seed = TRUE
options(future.rng.onMisuse="ignore")

future::plan(future::multisession, workers = floor(parallel::detectCores()/2 ))

cntrt <- fread('https://www1.nseindia.com/content/indices/ind_nifty500list.csv')
cntrt02 <- cntrt [, `:=` (nrow = .I, SYMBOL02 = paste(Symbol, ".NS", sep="") ), ]
setnames(cntrt02, "ISIN Code", "ISIN")

a01 <-  BatchGetSymbols(
  tickers = cntrt02$SYMBOL02,
  first.date = Sys.Date() - 3600,
  last.date = Sys.Date(),
  thresh.bad.data = 0.75,
  bench.ticker = "^NSEI",
  type.return = "arit",
  freq.data = "daily",
  how.to.aggregate = "last",
  do.complete.data = FALSE,
  do.fill.missing.prices = TRUE,
  do.cache = TRUE,
  cache.folder = file.path(tempdir(), "BGS_Cache"),
  do.parallel = TRUE, # FALSE
  be.quiet = FALSE
)

a01con <- data.table(a01$df.control)
a02 <- data.table(a01$df.tickers)

#
# Extract the Nifty50 data
#

a01nfity50 <-  BatchGetSymbols(
  tickers = "^NSEI",
  first.date = Sys.Date() - 3600,
  last.date = Sys.Date(),
  thresh.bad.data = 0.75,
  bench.ticker = "^NSEI",
  type.return = "arit",
  freq.data = "daily",
  how.to.aggregate = "last",
  do.complete.data = FALSE,
  do.fill.missing.prices = TRUE,
  do.cache = TRUE,
  cache.folder = file.path(tempdir(), "BGS_Cache"),
  do.parallel = TRUE, # FALSE
  be.quiet = FALSE
)

a02nfity50 <- data.table(a01nfity50$df.tickers)
setnames(a02nfity50, paste("NF", names(a02nfity50), sep = "_"))

# Merge the data with remaining data

a03all <- merge(x = a02,
                y = a02nfity50, 
                by.x = c("ref.date"),
                by.y = c("NF_ref.date"))

#
# Calculate Relative price strength
# https://corporatefinanceinstitute.com/resources/knowledge/trading-investing/relative-price-strength-rps/
#
a03all <- a03all [, rs := price.adjusted / NF_price.adjusted, ]
a03all <- a03all [, `:=` (rs_sma55 = SMA(rs, 55), rs_ema55 = EMA(rs, 55),
                          rs_mean55 = runMean(rs, 55),
                          rs_sd55 = runSD(rs, 55) ), by =.(ticker)]

a03all <- a03all [, `:=` (prc_ema55 = SMA(price.adjusted, 55), 
                          prc_ema20 = EMA(price.adjusted, 20),
                          low20 = runMin(price.adjusted, 20),
                          high55 = runMax(price.adjusted, 55), 
                          high200 = runMax(price.adjusted, 200) ), by =.(ticker)]

adx_n <- 14
atr <- as.data.table( ATR(a03all[, c("price.high","price.low","price.close") ], n = adx_n) )
atr <- atr [, allrow := .I, ]


a03all <- a03all [, jdk_rs55 := 100 + ((rs - rs_mean55)/rs_sd55) + 1, ]
a03all <- a03all [, jdk_roc55 := 100 * (shift(jdk_rs55, type = "lag", n=1) / jdk_rs55 - 1), by = .(ticker)]
a03all <- a03all [, `:=` (jdk_roc55_mean55 = runMean(jdk_roc55, 55),
                          jdk_roc55_sd55 = runSD(jdk_roc55, 55) ), by =.(ticker)]

a03all <- a03all [, jdk_momratio55 := 100 + ((jdk_roc55 - jdk_roc55_mean55)/jdk_roc55_sd55) + 1, ]

a04all <- na.omit(a03all)
a04all <- a04all [, qudrant := case_when(jdk_rs55 > 100 & jdk_momratio55 > 100 ~ 1,
                                         jdk_rs55 > 100 & jdk_momratio55 < 100 ~ 2,
                                         jdk_rs55 < 100 & jdk_momratio55 > 100 ~ 3,
                                         jdk_rs55 < 100 & jdk_momratio55 < 100 ~ 4 ), ]

##########################################################################################
#
# Mark Minverveni template 
# https://www.marcellagerwerf.com/how-to-scan-mark-minervinis-trend-template-using-python/
##########################################################################################

mark0001 <- a04all [ , `:=`(sma150 = sma(price.close, n = 150),
                            sma200 = sma(price.close, n = 200),
                            sma50 = sma(price.close, n = 50),
                            low252 = runMin(price.low, n = 252), 
                            high252 = runMax(price.high, n = 252),
                            ibd_score = 1), by = .(ticker)]

mark0001 <- mark0001 [,sma200_20 := runMean( sma200, n = 20), by = .(ticker)]

mark0001 <- mark0001 [, `:=` (rule01 = ifelse(price.close > sma150 & price.close > sma200, 1, 0), 
                              rule02 = ifelse(sma150 > sma200, 1, 0),
                              rule03 = ifelse(sma200_20 > sma200, 1, 0),
                              rule04 = ifelse(sma50 > sma150 & sma50 > sma200, 1, 0),
                              rule05 = ifelse(price.close > 1.3 * low252, 1, 0),
                              rule06 = ifelse(price.close > 0.75 * high252, 1, 0),
                              rule07 = ifelse(qudrant <=2, 1, 0) ),]

mark0001 <- mark0001 [, subset01 := ifelse(rule01 == 1 & rule02 == 1 & rule03 == 1 & rule04 == 1 & rule05 == 1 & rule06 == 1 & rule07 == 1, 1, 0), ]


# Check the number of stocks meeting this on each day
mark0002 <- mark0001 [, .(tot_stocks = uniqueN(ticker),
                          name_stocks = paste(ticker, collapse = ",", sep="") ), by = .(ref.date, subset01)]

mark0002 <- mark0002 [, name_stocks := ifelse(subset01 == 1, name_stocks, "" ), ]

a05all <- a04all [ anydate(ref.date) >= Sys.Date() - 100]

library(ggplot2)
library(gganimate)
library(gapminder)

theme_set(theme_bw())

# Static plot
p <- ggplot(
  a05all, 
  aes(x = jdk_rs55, y=jdk_momratio55, size = price.adjusted, colour = as.factor(qudrant)) ) +
  geom_point(show.legend = FALSE, alpha = 0.7) +
  scale_color_viridis_d() +
  geom_vline(xintercept = 100) +
  geom_hline(yintercept = 100) +
  labs(x = "JdK RS-ratio", y = "JdK RS-momentum")

p

p2 <- p +
  transition_time(ref.date) +
  labs(title = "Date: {frame_time}") +
  shadow_wake(wake_length = 0.4, alpha = FALSE)

ggplotly(p)

library(plotly)

p <- a05all %>%
  plot_ly(
    x = ~jdk_rs55, 
    y = ~jdk_momratio55, 
    size = ~price.adjusted, 
    color = ~as.factor(qudrant), 
    frame = ~ref.date, 
    text = ~ticker, 
    hoverinfo = "text",
    type = 'scatter',
    mode = 'markers'
  )
p

# https://community.rstudio.com/t/what-is-the-simplest-way-to-make-an-interactive-bar-plot-with-multiple-drop-down-filters-with-shiny-and-ggplot-combo/48917/2
library(shiny)

tickers <- unique(a05all$ticker)
dates <- unique(a05all$ref.date)


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Sample Drop Down"),
  
  # Sidebar with dropdown
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "selects", choices = tickers, label = "Select stocks", multiple = TRUE),
      #selectInput(inputId = "selects2", choices = dates, label = "select dates", multiple = TRUE),
      
      dateRangeInput("daterange1", "Date range:", start = min(dates), end = max(dates), min = min(dates), max = max(dates) )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("Plot")
    )
  )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$Plot <- renderPlot({

    data = a05all [ ticker %in% c("TCS.NS", input$selects) & (ref.date >= input$daterange1[1] & ref.date >= input$daterange1[2]) ] 
    
    p <- ggplot(
      data, 
      aes(x = jdk_rs55, y=jdk_momratio55, size = price.adjusted, colour = as.factor(qudrant)) ) +
      geom_point(show.legend = FALSE, alpha = 0.7) +
      scale_color_viridis_d() +
      geom_vline(xintercept = 100) +
      geom_hline(yintercept = 100) +
      labs(x = "JdK RS-ratio", y = "JdK RS-momentum")
    
    p
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

p2 <- p +
  transition_time(ref.date) +
  labs(title = "Date: {frame_time}") +
  shadow_wake(wake_length = 0.4, alpha = FALSE)

p2

%>%
  filter(tickers %in% input$selects) %>%
  select(one_of(c("ref.dates", input$selects2))) %>%
  gather(dates, -tickers)


#########################################
#
# 2nd example of server with plotly
#
#########################################

library(plotly)

# initiate a line shape object
line <- list(
  type = "line",
  line = list(color = "black"),
  xref = 100,
  yref = 100
)

p <- a05all %>%
  plot_ly(
    x = ~jdk_rs55, 
    y = ~jdk_momratio55, 
    size = ~price.adjusted, 
    color = ~as.factor(qudrant), 
    frame = ~ref.date, 
    text = ~ticker, 
    hoverinfo = "text",
    type = 'scatter',
    mode = 'markers' )

p01 <- layout (p, shapes = line)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$Plot <- renderPlot({
    
    data = a05all [ ticker %in% input$selects ] 
    
    p <- ggplot(
      data, 
      aes(x = jdk_rs55, y=jdk_momratio55, size = price.adjusted, colour = as.factor(qudrant)) ) +
      geom_point(show.legend = FALSE, alpha = 0.7) +
      scale_color_viridis_d() +
      geom_vline(xintercept = 100) +
      geom_hline(yintercept = 100) +
      labs(x = "JdK RS-ratio", y = "JdK RS-momentum")
    
    p
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


#
# See if other indices data is present
# Data only for 7 indices are getting extracted
#

a099indices <-  BatchGetSymbols(
  tickers = c('^NSEBANK',	'^CNX100',	'^INDIAVIX',	'LIX15.NS',	'^NSEMDCP50',	'^NSEI',	'^NSMIDCP',	'^CNXDIVOP',	'NIFTYQUALITY30.NS',	'NV20.NS',	'^TV.NS',	'^CNXAUTO',	'^CNXPSUBANK',	'NIFTYPVTBANK.NS',	'^CNXFMCG',	'^CNXMETAL',	'^CNXPSE',	'^CNX200',	'^CNXENERGY',	'^CNXPHARMA',	'^CNXMNC',	'CPSE.NS',	'^CRSLDX',	'^CRSMID',	'^NSEDIV',	'NI15.NS',	'^CNXIT',	'NIFTYTR2XLEV.NS',	'NIFTYPR2XLEV.NS',	'NIFTYMIDLIQ15.NS',	'^CNXINFRA',	'^CNXCONSUM',	'^CNXSERVICE',	'NIFTYTR1XINV.NS',	'^CNXCMDT',	'^CNXREALTY',	'^CNXMEDIA',	'^CNXFIN',	'^CNXSC',	'NIFTYPR1XINV.NS'),
  first.date = Sys.Date() - 3600,
  last.date = Sys.Date(),
  thresh.bad.data = 0.75,
  bench.ticker = "^NSEI",
  type.return = "arit",
  freq.data = "daily",
  how.to.aggregate = "last",
  do.complete.data = FALSE,
  do.fill.missing.prices = TRUE,
  do.cache = TRUE,
  cache.folder = file.path(tempdir(), "BGS_Cache"),
  do.parallel = TRUE, # FALSE
  be.quiet = FALSE
)

a099indices_01 <- data.table(a099indices$df.control)
a099indices_02 <- data.table(a099indices$df.tickers)


a02nfity50 <- data.table(a01nfity50$df.tickers)
setnames(a02nfity50, paste("NF", names(a02nfity50), sep = "_"))


library(BatchGetSymbols)

future.seed = TRUE
options(future.rng.onMisuse="ignore")

future::plan(future::multisession, workers = floor(parallel::detectCores()/2 ))

cntrt <- fread('https://www1.nseindia.com/content/indices/ind_nifty500list.csv')
cntrt02 <- cntrt [, `:=` (nrow = .I, SYMBOL02 = paste(Symbol, ".NS", sep="") ), ]
setnames(cntrt02, "ISIN Code", "ISIN")

a01 <-  BatchGetSymbols(
  tickers = "INDEXNSE:NIFTY_AUTO",
  first.date = Sys.Date() - 700,
  last.date = Sys.Date(),
  thresh.bad.data = 0.75,
  bench.ticker = "^NSEI",
  type.return = "arit",
  freq.data = "daily",
  how.to.aggregate = "last",
  do.complete.data = FALSE,
  do.fill.missing.prices = TRUE,
  do.cache = TRUE,
  cache.folder = file.path(tempdir(), "BGS_Cache"),
  do.parallel = TRUE, # FALSE
  be.quiet = FALSE
)

a01con <- data.table(a01$df.control)
a02 <- data.table(a01$df.tickers)

a01w <-  BatchGetSymbols(
  tickers = cntrt02$SYMBOL02,
  first.date = Sys.Date() - 700,
  last.date = Sys.Date(),
  thresh.bad.data = 0.75,
  bench.ticker = "^NSEI",
  type.return = "arit",
  freq.data = "weekly",
  how.to.aggregate = "last",
  do.complete.data = FALSE,
  do.fill.missing.prices = TRUE,
  do.cache = TRUE,
  cache.folder = file.path(tempdir(), "BGS_Cache"),
  do.parallel = TRUE, # FALSE
  be.quiet = FALSE
)

a01w_con <- data.table(a01w$df.control)
a02w <- data.table(a01w$df.tickers)


require("quantmod")

cntrt02 <- cntrt02 [, grp := round(nrow/ 100), ]
name01 <- cntrt02 [, quote := paste(SYMBOL02, collapse = ";", sep= " "), by = .(grp)]
name01 <- unique ( cntrt02 [, c("quote", "grp"), ])
name01 <- name01 [, get001 := paste("yahoo", grp, " <- as.data.table (getQuote ('", quote, "', what  = yahooQF(c( 'Symbol', 'Name', 'Name (Long)', 'Quote Type', 'Quote Source Name', 'Source Interval', 'Currency', 'Financial Currency', 'Market', 'Market State', 'Exchange', 'Exchange Full Name', 'Exchange Timezone', 'Exchange TZ', 'Exchange Data Delay', 'GMT Offset Millis', 'Tradeable', 'Ask', 'Bid', 'Ask Size', 'Bid Size', 'Last Trade (Price Only)', 'Last Trade Time', 'Change', 'Open', 'Days High', 'Days Low', 'Volume', 'Change in Percent', 'Previous Close', 'Change From 52-week Low', 'Percent Change From 52-week Low', 'Change From 52-week High', 'Percent Change From 52-week High', '52-week Low', '52-week High', '50-day Moving Average', 'Change From 50-day Moving Average', 'Percent Change From 50-day Moving Average', '200-day Moving Average', 'Change From 200-day Moving Average', 'Percent Change From 200-day Moving Average', 'Market Capitalization', 'P/E Ratio', 'Price/EPS Estimate Next Year', 'Price/Book', 'Book Value', 'Average Daily Volume', 'Shares Outstanding', 'Ex-Dividend Date', 'Dividend/Share', 'Dividend Yield', 'Earnings Timestamp', 'Earnings Start Time', 'Earnings End Time', 'Earnings/Share', 'EPS Forward', 'Language', 'Message Board ID', 'Price Hint' ))) )", sep=""), ]

eval(parse(text = name01 [grp ==6]$get001))


a03quote <- as.data.table ( getQuote("TCS.NS;INFY.NS", what = yahooQF(c("Market Capitalization", "Earnings/Share", 
                                "P/E Ratio", "Book Value", "EBITDA", "52-week Range"))) )


a03quote <- getQuote("", what = yahooQF(c("Market Capitalization", "Earnings/Share", "Shares Outstanding",
                                                                        "P/E Ratio", "Book Value", "EBITDA", "52-week Range"))) 

a03quote <- getQuote(eval(name01[grp == 6]$quote), what = yahooQF(c("Market Capitalization", "Earnings/Share", "Shares Outstanding",
                                                        "P/E Ratio", "Book Value", "EBITDA", "52-week Range"))) 

setDT(a03quote, keep.rownames = TRUE)[]

getQuote("PGHL.NS", what=yahooQF())


# Get the data for different indices from investing.com
# Get the list of companies from NSE
# Merge them and understand the 14 day Relative risk profile

dt <- Sys.Date()

# Create the reference values for the previous periods:

# Find previous year start to end
prvyr <- as.numeric(format(as.Date( floor_date(dt, "year") - years(1) ), "%Y"))
prvyr01 <- as.Date(paste(prvyr, "01", "01", sep="-"))


# Get the yearly, monthly, weekly, daily values:
styrdate <- as.numeric(as.POSIXct(prvyr01, format="%Y-%m-%d"))
endaydate <- as.numeric(as.POSIXct(dt+1, format="%Y-%m-%d")) # Vinay update 9th Jan 2021

i01_nifty50 <- fread('https://www1.nseindia.com/content/indices/ind_nifty50list.csv')
t01_nifty50 <- paste('https://in.investing.com/indices/s-p-cnx-nifty-historical-data?end_date=', endaydate, '&st_date=', styrdate, '&interval_sec=monthly&interval_sec=daily', sep=''); t01_nifty50_html <- read_html(t01_nifty50); t01_nifty50_whole  <- t01_nifty50_html  %>% html_nodes('table') %>% html_table(fill = TRUE) %>% .[[2]]; t01_nifty50_whole  <- as.data.table(t01_nifty50_whole);

i03_nifty100 <- fread('https://www1.nseindia.com/content/indices/ind_nifty100list.csv')
t03_nifty100 <- paste('https://in.investing.com/indices/cnx-100-historical-data?end_date=', endaydate, '&st_date=', styrdate, '&interval_sec=monthly&interval_sec=daily', sep=''); t03_nifty100_html <- read_html(t03_nifty100); t03_nifty100_whole  <- t03_nifty100_html  %>% html_nodes('table') %>% html_table(fill = TRUE) %>% .[[2]]; t03_nifty100_whole  <- as.data.table(t03_nifty100_whole);

i04_nifty200 <- fread('https://www1.nseindia.com/content/indices/ind_nifty200list.csv')
t04_nifty200 <- paste('https://in.investing.com/indices/cnx-200-historical-data?end_date=', endaydate, '&st_date=', styrdate, '&interval_sec=monthly&interval_sec=daily', sep=''); t04_nifty200_html <- read_html(t04_nifty200); t04_nifty200_whole  <- t04_nifty200_html  %>% html_nodes('table') %>% html_table(fill = TRUE) %>% .[[2]]; t04_nifty200_whole  <- as.data.table(t04_nifty200_whole);

i05_nifty500 <- fread('https://www1.nseindia.com/content/indices/ind_nifty500list.csv')
t05_nifty500 <- paste('https://in.investing.com/indices/s-p-cnx-500-historical-data?end_date=', endaydate, '&st_date=', styrdate, '&interval_sec=monthly&interval_sec=daily', sep=''); t05_nifty500_html <- read_html(t05_nifty500); t05_nifty500_whole  <- t05_nifty500_html  %>% html_nodes('table') %>% html_table(fill = TRUE) %>% .[[2]]; t05_nifty500_whole  <- as.data.table(t05_nifty500_whole);

i06_mid150 <- fread('https://www1.nseindia.com/content/indices/ind_niftymidcap150list.csv')
t06_mid150 <- paste('https://in.investing.com/indices/nifty-midcap-150-historical-data?end_date=', endaydate, '&st_date=', styrdate, '&interval_sec=monthly&interval_sec=daily', sep=''); t06_mid150_html <- read_html(t06_mid150); t06_mid150_whole  <- t06_mid150_html  %>% html_nodes('table') %>% html_table(fill = TRUE) %>% .[[2]]; t06_mid150_whole  <- as.data.table(t06_mid150_whole);

i07_mid50 <- fread('https://www1.nseindia.com/content/indices/ind_niftymidcap50list.csv')	
t07_mid50 <- paste('https://in.investing.com/indices/nifty-midcap-50-historical-data?end_date=', endaydate, '&st_date=', styrdate, '&interval_sec=monthly&interval_sec=daily', sep=''); t07_mid50_html <- read_html(t07_mid50); t07_mid50_whole  <- t07_mid50_html  %>% html_nodes('table') %>% html_table(fill = TRUE) %>% .[[2]]; t07_mid50_whole  <- as.data.table(t07_mid50_whole);

i08_mid100 <- fread('https://www1.nseindia.com/content/indices/ind_niftymidcap100list.csv')	
t08_mid100 <- paste('https://in.investing.com/indices/cnx-midcap-historical-data?end_date=', endaydate, '&st_date=', styrdate, '&interval_sec=monthly&interval_sec=daily', sep=''); t08_mid100_html <- read_html(t08_mid100); t08_mid100_whole  <- t08_mid100_html  %>% html_nodes('table') %>% html_table(fill = TRUE) %>% .[[2]]; t08_mid100_whole  <- as.data.table(t08_mid100_whole);

i09_small250 <- fread('https://www1.nseindia.com/content/indices/ind_niftysmallcap250list.csv')	
t09_small250 <- paste('https://in.investing.com/indices/nifty-smallcap-250-historical-data?end_date=', endaydate, '&st_date=', styrdate, '&interval_sec=monthly&interval_sec=daily', sep=''); t09_small250_html <- read_html(t09_small250); t09_small250_whole  <- t09_small250_html  %>% html_nodes('table') %>% html_table(fill = TRUE) %>% .[[2]]; t09_small250_whole  <- as.data.table(t09_small250_whole);

i10_small50 <- fread('https://www1.nseindia.com/content/indices/ind_niftysmallcap50list.csv')	
t10_small50 <- paste('https://in.investing.com/indices/nifty-smallcap-50-historical-data?end_date=', endaydate, '&st_date=', styrdate, '&interval_sec=monthly&interval_sec=daily', sep=''); t10_small50_html <- read_html(t10_small50); t10_small50_whole  <- t10_small50_html  %>% html_nodes('table') %>% html_table(fill = TRUE) %>% .[[2]]; t10_small50_whole  <- as.data.table(t10_small50_whole);

i11_small100 <- fread('https://www1.nseindia.com/content/indices/ind_niftysmallcap100list.csv')	
t11_small100 <- paste('https://in.investing.com/indices/cnx-smallcap-historical-data?end_date=', endaydate, '&st_date=', styrdate, '&interval_sec=monthly&interval_sec=daily', sep=''); t11_small100_html <- read_html(t11_small100); t11_small100_whole  <- t11_small100_html  %>% html_nodes('table') %>% html_table(fill = TRUE) %>% .[[2]]; t11_small100_whole  <- as.data.table(t11_small100_whole);

i14_auto <- fread('https://www1.nseindia.com/content/indices/ind_niftyautolist.csv')	
t14_auto <- paste('https://in.investing.com/indices/cnx-auto-historical-data?end_date=', endaydate, '&st_date=', styrdate, '&interval_sec=monthly&interval_sec=daily', sep=''); t14_auto_html <- read_html(t14_auto); t14_auto_whole  <- t14_auto_html  %>% html_nodes('table') %>% html_table(fill = TRUE) %>% .[[2]]; t14_auto_whole  <- as.data.table(t14_auto_whole);

i15_bank <- fread('https://www1.nseindia.com/content/indices/ind_niftybanklist.csv')	
t15_bank <- paste('https://in.investing.com/indices/bank-nifty-historical-data?end_date=', endaydate, '&st_date=', styrdate, '&interval_sec=monthly&interval_sec=daily', sep=''); t15_bank_html <- read_html(t15_bank); t15_bank_whole  <- t15_bank_html  %>% html_nodes('table') %>% html_table(fill = TRUE) %>% .[[2]]; t15_bank_whole  <- as.data.table(t15_bank_whole);

i17_finance <- fread('https://www1.nseindia.com/content/indices/ind_niftyfinancelist.csv')	
t17_finance <- paste('https://in.investing.com/indices/cnx-finance-historical-data?end_date=', endaydate, '&st_date=', styrdate, '&interval_sec=monthly&interval_sec=daily', sep=''); t17_finance_html <- read_html(t17_finance); t17_finance_whole  <- t17_finance_html  %>% html_nodes('table') %>% html_table(fill = TRUE) %>% .[[2]]; t17_finance_whole  <- as.data.table(t17_finance_whole);

i19_fmcg <- fread('https://www1.nseindia.com/content/indices/ind_niftyfmcglist.csv')	
t19_fmcg <- paste('https://in.investing.com/indices/cnx-fmcg-historical-data?end_date=', endaydate, '&st_date=', styrdate, '&interval_sec=monthly&interval_sec=daily', sep=''); t19_fmcg_html <- read_html(t19_fmcg); t19_fmcg_whole  <- t19_fmcg_html  %>% html_nodes('table') %>% html_table(fill = TRUE) %>% .[[2]]; t19_fmcg_whole  <- as.data.table(t19_fmcg_whole);

i21_it <- fread('https://www1.nseindia.com/content/indices/ind_niftyitlist.csv')	
t21_it <- paste('https://in.investing.com/indices/cnx-it-historical-data?end_date=', endaydate, '&st_date=', styrdate, '&interval_sec=monthly&interval_sec=daily', sep=''); t21_it_html <- read_html(t21_it); t21_it_whole  <- t21_it_html  %>% html_nodes('table') %>% html_table(fill = TRUE) %>% .[[2]]; t21_it_whole  <- as.data.table(t21_it_whole);

i22_media <- fread('https://www1.nseindia.com/content/indices/ind_niftymedialist.csv')	
t22_media <- paste('https://in.investing.com/indices/cnx-media-historical-data?end_date=', endaydate, '&st_date=', styrdate, '&interval_sec=monthly&interval_sec=daily', sep=''); t22_media_html <- read_html(t22_media); t22_media_whole  <- t22_media_html  %>% html_nodes('table') %>% html_table(fill = TRUE) %>% .[[2]]; t22_media_whole  <- as.data.table(t22_media_whole);

i23_metal <- fread('https://www1.nseindia.com/content/indices/ind_niftymetallist.csv')	
t23_metal <- paste('https://in.investing.com/indices/cnx-metal-historical-data?end_date=', endaydate, '&st_date=', styrdate, '&interval_sec=monthly&interval_sec=daily', sep=''); t23_metal_html <- read_html(t23_metal); t23_metal_whole  <- t23_metal_html  %>% html_nodes('table') %>% html_table(fill = TRUE) %>% .[[2]]; t23_metal_whole  <- as.data.table(t23_metal_whole);

i25_pharma <- fread('https://www1.nseindia.com/content/indices/ind_niftypharmalist.csv')	
t25_pharma <- paste('https://in.investing.com/indices/cnx-pharma-historical-data?end_date=', endaydate, '&st_date=', styrdate, '&interval_sec=monthly&interval_sec=daily', sep=''); t25_pharma_html <- read_html(t25_pharma); t25_pharma_whole  <- t25_pharma_html  %>% html_nodes('table') %>% html_table(fill = TRUE) %>% .[[2]]; t25_pharma_whole  <- as.data.table(t25_pharma_whole);

i27_psubank <- fread('https://www1.nseindia.com/content/indices/ind_niftypsubanklist.csv')	
t27_psubank <- paste('https://in.investing.com/indices/cnx-psu-bank-historical-data?end_date=', endaydate, '&st_date=', styrdate, '&interval_sec=monthly&interval_sec=daily', sep=''); t27_psubank_html <- read_html(t27_psubank); t27_psubank_whole  <- t27_psubank_html  %>% html_nodes('table') %>% html_table(fill = TRUE) %>% .[[2]]; t27_psubank_whole  <- as.data.table(t27_psubank_whole);

i28_realty <- fread('https://www1.nseindia.com/content/indices/ind_niftyrealtylist.csv')	
t28_realty <- paste('https://in.investing.com/indices/cnx-realty-historical-data?end_date=', endaydate, '&st_date=', styrdate, '&interval_sec=monthly&interval_sec=daily', sep=''); t28_realty_html <- read_html(t28_realty); t28_realty_whole  <- t28_realty_html  %>% html_nodes('table') %>% html_table(fill = TRUE) %>% .[[2]]; t28_realty_whole  <- as.data.table(t28_realty_whole);

all01_idx <- rbindlist(mget(ls(pattern = "^i")), fill = TRUE, idcol = "index")
rm(list = ls( pattern = "^i") )

all01_idx_val <- rbindlist(mget(ls(pattern = "whole$")), fill = TRUE, idcol = "index")
rm(list = ls( pattern = "^t") )

all01_idx_val <- all01_idx_val [, `:=`(date02 = mdy(Date),
                       High = as.numeric( str_remove_all(High, ",") ), 
                       Low = as.numeric( str_remove_all(Low, ",") ) ,
                       Price = as.numeric( str_remove_all(Price, ",") ) ,
                       Open = as.numeric( str_remove_all(Open, ",") ) ),  ]


# Merge the data with remaining data

all02_idx_val <- merge(x = all01_idx_val,
                y = a02nfity50, 
                by.x = c("date02"),
                by.y = c("NF_ref.date"),
                all.x = TRUE)

#
# Calculate Relative price strength
# https://corporatefinanceinstitute.com/resources/knowledge/trading-investing/relative-price-strength-rps/
#
all02_idx_val <- all02_idx_val [, rs := Price / NF_price.adjusted, ]

all02_idx_val <- na.omit(all02_idx_val) # Step was needed to get the moving average calculation
all02_idx_val <- all02_idx_val [, `:=` (rs_mean14 = runMean(rs, 14),
                          rs_sd14 = runSD(rs, 14) ), by =.(index)]

all02_idx_val <- all02_idx_val [, jdk_rs14 := 100 + ((rs - rs_mean14)/rs_sd14) + 1, ]
all02_idx_val <- all02_idx_val [, jdk_roc14 := 100 * (shift(jdk_rs14, type = "lag", n=1) / jdk_rs14 - 1), by = .(index)]
all02_idx_val <- all02_idx_val [, `:=` (jdk_roc14_mean14 = runMean(jdk_roc14, 14),
                          jdk_roc14_sd14 = runSD(jdk_roc14, 14) ), by =.(index)]

all02_idx_val <- all02_idx_val [, jdk_momratio14 := 100 + ((jdk_roc14 - jdk_roc14_mean14)/jdk_roc14_sd14) + 1, ]

all03_idx_val <- na.omit(all02_idx_val)
all03_idx_val <- all03_idx_val [, qudrant := case_when(jdk_rs14 > 100 & jdk_momratio14 > 100 ~ 1,
                                         jdk_rs14 > 100 & jdk_momratio14 < 100 ~ 2,
                                         jdk_rs14 < 100 & jdk_momratio14 > 100 ~ 3,
                                         jdk_rs14 < 100 & jdk_momratio14 < 100 ~ 4 ), ]


all04_idx_val_excel <- all03_idx_val [, c("date02", "index", "qudrant", "jdk_rs14", "jdk_momratio14" ), ]

library(openxlsx)
write.xlsx(all04_idx_val_excel, "D:\\My-Shares\\analysis\\0504_jdk_index.xlsx")   

theme_set(theme_bw())

# Static plot
p <- ggplot(
  all03_idx_val [date02 >= "2021-03-01"], 
  aes(x = jdk_rs14, y=jdk_momratio14, colour = as.factor(qudrant)) ) +
  facet_wrap(~index) +
  geom_point(show.legend = FALSE, alpha = 0.7) +
  scale_color_viridis_d() +
  geom_vline(xintercept = 100) +
  geom_hline(yintercept = 100) +
  labs(x = "JdK RS-ratio", y = "JdK RS-momentum")

p

p2 <- p + facet_wrap(~index) +
  transition_time(date02) +
  labs(title = "Date: {frame_time}") +
  shadow_wake(wake_length = 0.4, alpha = FALSE)

p2



  ggplot(all03_idx_val, aes(x = date02, y = Price, group = index)) +
  geom_candlestick(aes(open = Open, high = High, low = Low, close = Price)) +
  labs(title = "Index Candlestick Chart", 
       subtitle = "Experimenting with Mulitple Stocks",
       y = "Closing Price", x = "") + 
  facet_wrap(~ index, scale = "free_y") + 
  theme_tq()
  # , ncol = 2,  

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


i01_nfity50 <- fread('https://www1.nseindia.com/content/indices/ind_nifty50list.csv')	
t01_nfity50 <- paste('https://in.investing.com/indices/s-p-cnx-nifty-historical-data?end_date=', endaydate, '&st_date=', styrdate, '&interval_sec=monthly&interval_sec=daily', sep=''); t01_nfity50_html <- read_html(t01_nfity50); t01_nfity50_whole  <- t01_nfity50_html  %>% html_nodes('table') %>% html_table(fill = TRUE) %>% .[[2]]; t01_nfity50_whole  <- as.data.table(t01_nfity50_whole);

url_01 <- paste('https://in.investing.com/indices/cnx-midcap-historical-data?end_date=', endaydate, '&st_date=', styrdate, '&interval_sec=monthly&interval_sec=daily', sep=''); url_01_html <- read_html(url_01); url_01_whole  <- url_01_html  %>% html_nodes('table') %>% html_table(fill = TRUE) %>% .[[2]]; url_01_whole  <- as.data.table(url_01_whole);
url_02 <- paste('https://in.investing.com/indices/cnx-smallcap-historical-data?end_date=', endaydate, '&st_date=', styrdate, '&interval_sec=monthly&interval_sec=daily', sep=''); url_02_html <- read_html(url_02); url_02_whole  <- url_02_html  %>% html_nodes('table') %>% html_table(fill = TRUE) %>% .[[2]]; url_02_whole  <- as.data.table(url_02_whole);
url_03 <- paste('https://in.investing.com/indices/nifty-smallcap-50-historical-data?end_date=', endaydate, '&st_date=', styrdate, '&interval_sec=monthly&interval_sec=daily', sep=''); url_03_html <- read_html(url_03); url_03_whole  <- url_03_html  %>% html_nodes('table') %>% html_table(fill = TRUE) %>% .[[2]]; url_03_whole  <- as.data.table(url_03_whole);
url_04 <- paste('https://in.investing.com/indices/cnx-auto-historical-data?end_date=', endaydate, '&st_date=', styrdate, '&interval_sec=monthly&interval_sec=daily', sep=''); url_04_html <- read_html(url_04); url_04_whole  <- url_04_html  %>% html_nodes('table') %>% html_table(fill = TRUE) %>% .[[2]]; url_04_whole  <- as.data.table(url_04_whole);
url_05 <- paste('https://in.investing.com/indices/bank-nifty-historical-data?end_date=', endaydate, '&st_date=', styrdate, '&interval_sec=monthly&interval_sec=daily', sep=''); url_05_html <- read_html(url_05); url_05_whole  <- url_05_html  %>% html_nodes('table') %>% html_table(fill = TRUE) %>% .[[2]]; url_05_whole  <- as.data.table(url_05_whole);
url_06 <- paste('https://in.investing.com/indices/cnx-commodities-historical-data?end_date=', endaydate, '&st_date=', styrdate, '&interval_sec=monthly&interval_sec=daily', sep=''); url_06_html <- read_html(url_06); url_06_whole  <- url_06_html  %>% html_nodes('table') %>% html_table(fill = TRUE) %>% .[[2]]; url_06_whole  <- as.data.table(url_06_whole);
url_07 <- paste('https://in.investing.com/indices/cnx-energy-historical-data?end_date=', endaydate, '&st_date=', styrdate, '&interval_sec=monthly&interval_sec=daily', sep=''); url_07_html <- read_html(url_07); url_07_whole  <- url_07_html  %>% html_nodes('table') %>% html_table(fill = TRUE) %>% .[[2]]; url_07_whole  <- as.data.table(url_07_whole);
url_08 <- paste('https://in.investing.com/indices/cnx-finance-historical-data?end_date=', endaydate, '&st_date=', styrdate, '&interval_sec=monthly&interval_sec=daily', sep=''); url_08_html <- read_html(url_08); url_08_whole  <- url_08_html  %>% html_nodes('table') %>% html_table(fill = TRUE) %>% .[[2]]; url_08_whole  <- as.data.table(url_08_whole);
url_09 <- paste('https://in.investing.com/indices/cnx-fmcg-historical-data?end_date=', endaydate, '&st_date=', styrdate, '&interval_sec=monthly&interval_sec=daily', sep=''); url_09_html <- read_html(url_09); url_09_whole  <- url_09_html  %>% html_nodes('table') %>% html_table(fill = TRUE) %>% .[[2]]; url_09_whole  <- as.data.table(url_09_whole);
url_10 <- paste('https://in.investing.com/indices/cnx-consumption-historical-data?end_date=', endaydate, '&st_date=', styrdate, '&interval_sec=monthly&interval_sec=daily', sep=''); url_10_html <- read_html(url_10); url_10_whole  <- url_10_html  %>% html_nodes('table') %>% html_table(fill = TRUE) %>% .[[2]]; url_10_whole  <- as.data.table(url_10_whole);
url_11 <- paste('https://in.investing.com/indices/cnx-infrastructure-historical-data?end_date=', endaydate, '&st_date=', styrdate, '&interval_sec=monthly&interval_sec=daily', sep=''); url_11_html <- read_html(url_11); url_11_whole  <- url_11_html  %>% html_nodes('table') %>% html_table(fill = TRUE) %>% .[[2]]; url_11_whole  <- as.data.table(url_11_whole);
url_12 <- paste('https://in.investing.com/indices/cnx-it-historical-data?end_date=', endaydate, '&st_date=', styrdate, '&interval_sec=monthly&interval_sec=daily', sep=''); url_12_html <- read_html(url_12); url_12_whole  <- url_12_html  %>% html_nodes('table') %>% html_table(fill = TRUE) %>% .[[2]]; url_12_whole  <- as.data.table(url_12_whole);
url_13 <- paste('https://in.investing.com/indices/cnx-media-historical-data?end_date=', endaydate, '&st_date=', styrdate, '&interval_sec=monthly&interval_sec=daily', sep=''); url_13_html <- read_html(url_13); url_13_whole  <- url_13_html  %>% html_nodes('table') %>% html_table(fill = TRUE) %>% .[[2]]; url_13_whole  <- as.data.table(url_13_whole);
url_14 <- paste('https://in.investing.com/indices/cnx-metal-historical-data?end_date=', endaydate, '&st_date=', styrdate, '&interval_sec=monthly&interval_sec=daily', sep=''); url_14_html <- read_html(url_14); url_14_whole  <- url_14_html  %>% html_nodes('table') %>% html_table(fill = TRUE) %>% .[[2]]; url_14_whole  <- as.data.table(url_14_whole);
url_15 <- paste('https://in.investing.com/indices/cnx-mnc-historical-data?end_date=', endaydate, '&st_date=', styrdate, '&interval_sec=monthly&interval_sec=daily', sep=''); url_15_html <- read_html(url_15); url_15_whole  <- url_15_html  %>% html_nodes('table') %>% html_table(fill = TRUE) %>% .[[2]]; url_15_whole  <- as.data.table(url_15_whole);
url_16 <- paste('https://in.investing.com/indices/cnx-media-historical-data?end_date=', endaydate, '&st_date=', styrdate, '&interval_sec=monthly&interval_sec=daily', sep=''); url_16_html <- read_html(url_16); url_16_whole  <- url_16_html  %>% html_nodes('table') %>% html_table(fill = TRUE) %>% .[[2]]; url_16_whole  <- as.data.table(url_16_whole);
url_17 <- paste('https://in.investing.com/indices/cnx-pharma-historical-data?end_date=', endaydate, '&st_date=', styrdate, '&interval_sec=monthly&interval_sec=daily', sep=''); url_17_html <- read_html(url_17); url_17_whole  <- url_17_html  %>% html_nodes('table') %>% html_table(fill = TRUE) %>% .[[2]]; url_17_whole  <- as.data.table(url_17_whole);
url_18 <- paste('https://in.investing.com/indices/cnx-psu-bank-historical-data?end_date=', endaydate, '&st_date=', styrdate, '&interval_sec=monthly&interval_sec=daily', sep=''); url_18_html <- read_html(url_18); url_18_whole  <- url_18_html  %>% html_nodes('table') %>% html_table(fill = TRUE) %>% .[[2]]; url_18_whole  <- as.data.table(url_18_whole);
url_19 <- paste('https://in.investing.com/indices/cnx-service-sector-historical-data?end_date=', endaydate, '&st_date=', styrdate, '&interval_sec=monthly&interval_sec=daily', sep=''); url_19_html <- read_html(url_19); url_19_whole  <- url_19_html  %>% html_nodes('table') %>% html_table(fill = TRUE) %>% .[[2]]; url_19_whole  <- as.data.table(url_19_whole);
url_20 <- paste('https://in.investing.com/indices/cnx-media-historical-data?end_date=', endaydate, '&st_date=', styrdate, '&interval_sec=monthly&interval_sec=daily', sep=''); url_20_html <- read_html(url_20); url_20_whole  <- url_20_html  %>% html_nodes('table') %>% html_table(fill = TRUE) %>% .[[2]]; url_20_whole  <- as.data.table(url_20_whole)




# Money in the bank strategy:

# banknifty
a01bnk <-  BatchGetSymbols(
  tickers = "^NSEBANK",
  first.date = "2016-01-01",
  last.date = Sys.Date(),
  thresh.bad.data = 0.75,
  bench.ticker = "^NSEI",
  type.return = "arit",
  freq.data = "daily",
  how.to.aggregate = "last",
  do.complete.data = FALSE,
  do.fill.missing.prices = TRUE,
  do.cache = TRUE,
  cache.folder = file.path(tempdir(), "BGS_Cache"),
  do.parallel = TRUE, # FALSE
  be.quiet = FALSE
)

a02bnk <- data.table(a01bnk$df.tickers)
setnames(a02bnk, paste("BNF", names(a02bnk), sep = "_"))


bnf0001 <- a04all [ ticker %in% c('AUBANK.NS', 'AXISBANK.NS', 'BANDHANBNK.NS', 'FEDERALBNK.NS', 'HDFCBANK.NS', 'ICICIBANK.NS', 'IDFCFIRSTB.NS', 'INDUSINDBK.NS', 'KOTAKBANK.NS', 'PNB.NS', 'RBLBANK.NS', 'SBIN.NS')
                    & ref.date >= Sys.Date() - 1200]

# Merge the data with remaining data

bnf0001 <- merge(x = bnf0001,
                 y = a02bnk, 
                 by.x = c("ref.date"),
                 by.y = c("BNF_ref.date"),
                 all.x = TRUE)

bnf0001 <- bnf0001 [, `:=` (lowhigh = price.high - price.low,
                            perchg = round( (price.close - price.open)/ price.open * 100, 1),
                            bnfperchg = (BNF_price.close - BNF_price.open)/ BNF_price.open * 100 ) , ]

# Check how many times BNF was better performing
bnf0001 <- bnf0001 [, perf := ifelse(perchg >= bnfperchg, "Better", "Worse"), ]

bnf0002 <- dcast(data = bnf0001,
                 ref.date + round(bnfperchg, 1) ~ ticker, 
                 value.var = c("perchg") )

bnf0003 <- dcast(data = bnf0001,
                 ref.date + round(bnfperchg, 1) ~ ticker, 
                 value.var = c("perf") )

# Check the count of better /worse

bnaf001 <- bnf001 [, ]


#######################################################
#
# Only execute the process on or after 1st Jan 2016
# Get the indices weight files
#
# Example calls: download the files, once downloaded, unzip
# Bank nifty files
# After unzipping them use tabulizer package to get the pdf
# files into R data.tables
# 
# if (curl_fetch_memory('https://www1.nseindia.com/content/indices/indices_dataJan2016.zip')$status_code == 200) download.file('https://www1.nseindia.com/content/indices/indices_dataJan2016.zip', 'D:\\My-Shares\\source-index-wgt\\42370_indices_dataJan2016.zip')
# if (curl_fetch_memory('https://www1.nseindia.com/content/indices/indices_dataJan2016.zip')$status_code == 200) download.file('https://www1.nseindia.com/content/indices/indices_dataJan2016.zip', 'D:\\My-Shares\\source-index-wgt\\42371_indices_dataJan2016.zip')
# if (curl_fetch_memory('https://www1.nseindia.com/content/indices/indices_dataJan2016.zip')$status_code == 200) download.file('https://www1.nseindia.com/content/indices/indices_dataJan2016.zip', 'D:\\My-Shares\\source-index-wgt\\42372_indices_dataJan2016.zip')
# if (curl_fetch_memory('https://www1.nseindia.com/content/indices/indices_dataJan2016.zip')$status_code == 200) download.file('https://www1.nseindia.com/content/indices/indices_dataJan2016.zip', 'D:\\My-Shares\\source-index-wgt\\42373_indices_dataJan2016.zip')
#
# if (file.exists('D:\\My-Shares\\source-index-wgt\\42370_indices_dataJan2016.zip') ) {files = unzip('D:\\My-Shares\\source-index-wgt\\42370_indices_dataJan2016.zip', list=TRUE)$Name; unzip(zipfile = 'D:\\My-Shares\\source-index-wgt\\42370_indices_dataJan2016.zip', exdir = 'D:\\My-Shares\\source-index-wgt-bnfpdf', files=files[grepl('^NIFTY_Bank*|^niftybank*',files)]);rm(files);}
# if (file.exists('D:\\My-Shares\\source-index-wgt\\42371_indices_dataJan2016.zip') ) {files = unzip('D:\\My-Shares\\source-index-wgt\\42371_indices_dataJan2016.zip', list=TRUE)$Name; unzip(zipfile = 'D:\\My-Shares\\source-index-wgt\\42371_indices_dataJan2016.zip', exdir = 'D:\\My-Shares\\source-index-wgt-bnfpdf', files=files[grepl('^NIFTY_Bank\\.pdf|^niftybank*\\.pdf',files)]);rm(files);}
# if (file.exists('D:\\My-Shares\\source-index-wgt\\42372_indices_dataJan2016.zip') ) {files = unzip('D:\\My-Shares\\source-index-wgt\\42372_indices_dataJan2016.zip', list=TRUE)$Name; unzip(zipfile = 'D:\\My-Shares\\source-index-wgt\\42372_indices_dataJan2016.zip', exdir = 'D:\\My-Shares\\source-index-wgt-bnfpdf', files=files[grepl('^NIFTY_Bank\\.pdf|^niftybank*\\.pdf',files)]);rm(files);}
# if (file.exists('D:\\My-Shares\\source-index-wgt\\43311_indices_dataJul2018.zip') ) {files = unzip('D:\\My-Shares\\source-index-wgt\\43311_indices_dataJul2018.zip', list=TRUE)$Name; unzip(zipfile = 'D:\\My-Shares\\source-index-wgt\\43311_indices_dataJul2018.zip', exdir = 'D:\\My-Shares\\source-index-wgt-bnfpdf', files=files[grepl('^NIFTY_Bank*|^niftybank*',files)]);rm(files);}
#
# Date from excel to R creates an issue:
# origin = "1899-12-30"
# https://stackoverflow.com/questions/43230470/how-to-convert-excel-date-format-to-proper-date-in-r/62334132
#######################################################

step001 <- data.table ( read.xlsx("D:\\My-Shares\\prgm\\0500_rakeshpujara_atmlong.xlsx", 2) )
step002 <- step001 [, Date02 := mdy( paste(Month, Date, Year4) ), ]
step002 <- step002 [ Year4 >= 2016 ]

# Do the calculations for Futures data

eval(parse(text = step002$bnf05download)) # Once the program stopped after downloading files till 2017
eval(parse(text = step002 [Year4 >= 2018]$bnf05download))

# Do the calculations for the pdf extraction to data.table

eval(parse(text = step002$bnf06unzip))


all01_pdf <- rbindlist(mget(ls(pattern = "pdf")), fill = TRUE, idcol = "file_indx")
#all01_pdf <- all01_pdf [ , tmp := as.numeric(substr(file_indx, 5, 15)) , ]
all01_pdf <- all01_pdf [, trdate := anydate( as.Date ( as.numeric(substr(file_indx, 5, 15) ), origin = "1899-12-30" ) ), ]
all01_pdf <- all01_pdf [, monyr := substr (trdate, 1, 7),]
all01_pdf <- all01_pdf [, nrow := .I,]

setnames (all01_pdf, "V1", "Symbol")
setnames (all01_pdf, "V2", "Security_name")
setnames (all01_pdf, "V5", "Close_Price")
setnames (all01_pdf, "V6", "Index_Mcap")
setnames (all01_pdf, "V7", "Weightage")

# Due to the changes in the structure do the following:
all01_pdf <- all01_pdf [, Weightage := ifelse( trdate >= "2017-07-01", Index_Mcap, Weightage), ]

all02_pdf <- all01_pdf [ !trimws(Symbol) == "" & Security_name != "Security Name"]
all02_pdf <- all02_pdf [, Weightage := as.numeric(Weightage), ]
all03_pdf <- all02_pdf [,  .(avgwgt = mean(Weightage)), by =.(monyr, Symbol)]
all03_pdf <- all03_pdf [ order(monyr, -avgwgt, Symbol)]
all03_pdf <- all03_pdf [, rank := 1:.N, by = (monyr)]
all03_pdf <- all03_pdf [, cumperc := cumsum(avgwgt), by =.(monyr)]

saveRDS(all02_pdf, "D:\\My-Shares\\source-index-wgt-bnfpdf\\0504_bnf_idx_wgt.rds")

f <- function(x, pos) subset(x, SYMBOL %in% c("BANKNIFTY", "HDFCBANK", "ICICIBANK", "AXISBANK", "KOTAKBANK" ) )
#fo <- data.table (read_csv_chunked("D:\\My-Shares\\source-fno-csv\\fo*.csv", DataFrameCallback$new(f), chunk_size = 5) )
step002_yr <- step002 [ Year4 >= 2016 & Year4 <= 2019]

# Do the calculations for Futures data

eval(parse(text = step002_yr$fut_rdata))
all01_fut <- rbindlist(mget(ls(pattern = "fo")), fill = TRUE, idcol = "file_fut")
all01_fut <- all01_fut [, trdate := dmy ( substr(file_fut, 3, 20) ), ]

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

saveRDS(all02, "D:\\My-Shares\\source-index-wgt-bnfpdf\\0504_bnf_fno.rds")

