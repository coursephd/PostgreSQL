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
      selectInput(inputId = "selects2", choices = dates, label = "select dates", multiple = TRUE)
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

    data = a05all %>%
      filter(tickers %in% input$selects) %>%
      select(one_of(c("ref.dates", input$selects2))) %>%
      gather(dates, -tickers)
    
    ggplot(
      data, 
      aes(x = jdk_rs55, y=jdk_momratio55, size = price.adjusted, colour = as.factor(qudrant)) ) +
      geom_point(show.legend = FALSE, alpha = 0.7) +
      scale_color_viridis_d() +
      geom_vline(xintercept = 100) +
      geom_hline(yintercept = 100) +
      labs(x = "JdK RS-ratio", y = "JdK RS-momentum")
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


cntrt02 <- cntrt02 [, grp := round(nrow/ 100), ]
name01 <- cntrt02 [, quote := paste(SYMBOL02, collapse = ";", sep= " "), by = .(grp)]
name01 <- unique ( cntrt02 [, c("quote", "grp"), ])

require("quantmod")
a03quote <- as.data.table ( getQuote("TCS.NS;INFY.NS", what = yahooQF(c("Market Capitalization", "Earnings/Share", 
                                "P/E Ratio", "Book Value", "EBITDA", "52-week Range"))) )


a03quote <- getQuote("", what = yahooQF(c("Market Capitalization", "Earnings/Share", "Shares Outstanding",
                                                                        "P/E Ratio", "Book Value", "EBITDA", "52-week Range"))) 

a03quote <- getQuote(eval(name01[grp == 6]$quote), what = yahooQF(c("Market Capitalization", "Earnings/Share", "Shares Outstanding",
                                                        "P/E Ratio", "Book Value", "EBITDA", "52-week Range"))) 

setDT(a03quote, keep.rownames = TRUE)[]

getQuote("PGHL.NS", what=yahooQF())


sbi <- fread("https://www1.nseindia.com/content/mfss/L_SCHEME_REP.csv")

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



