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
  thresh.bad.data = 0.2,
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

#####################################################
#
# Calculate Relative price strength
# https://corporatefinanceinstitute.com/resources/knowledge/trading-investing/relative-price-strength-rps/
#
# EMA: Daily, weekly and monthly 5, 21, and 55 periods
#
#####################################################

a03all <- a03all [, rs := price.adjusted / NF_price.adjusted, ]
a03all <- a03all [, `:=` (rs_sma55 = SMA(rs, 55), rs_ema55 = EMA(rs, 55),
                          rs_mean55 = runMean(rs, 55),
                          rs_sd55 = runSD(rs, 55) ), by =.(ticker)]

a03all <- a03all [, `:=` (prc_ema5d = EMA(price.adjusted, 5), prc_ema21d = EMA(price.adjusted, 21), prc_ema55d = EMA(price.adjusted, 55), 
                          prc_ema5w = EMA(price.adjusted, 5 * 4), prc_ema21w = EMA(price.adjusted, 21 * 4), prc_ema55w = EMA(price.adjusted, 55 * 4),
                          prc_ema5m = EMA(price.adjusted, 5 * 21 ), prc_ema21m = EMA(price.adjusted, 21 * 21), prc_ema55m = EMA(price.adjusted, 55 * 21) ), 
                  by =.(ticker)]

a03all <- a03all [, jdk_rs55 := 100 + ((rs - rs_mean55)/rs_sd55) + 1, ]
a03all <- a03all [, jdk_roc55 := 100 * (shift(jdk_rs55, type = "lag", n=1) / jdk_rs55 - 1), by = .(ticker)]
a03all <- a03all [, `:=` (jdk_roc55_mean55 = runMean(jdk_roc55, 55),
                          jdk_roc55_sd55 = runSD(jdk_roc55, 55) ), by =.(ticker)]

a03all <- a03all [, jdk_momratio55 := 100 + ((jdk_roc55 - jdk_roc55_mean55)/jdk_roc55_sd55) + 1, ]

#############################################################################################
#
# https://stackoverflow.com/questions/36190503/running-sum-in-r-data-table/36190577
# Add an indicator to get some ideas on how many days have passed by from the first instance
# of recent past of high of 20 or high of 55 (in last 25 days)
#
# If the days are more than say 5 then do not take the trade as a breakout trade
#
#############################################################################################
a03all <- a03all [ order(ticker, ref.date)]
a03all <- a03all [, `:=`(ch = price.high - price.low,
                         bh = abs(price.open - price.close),
                         adv_dec = ifelse(ret.adjusted.prices >=0, "Advance", "Decline"),
                         allrow = .I ), ]

atr_n <- 20
atr <- as.data.table( ATR(a03all[, c("price.high","price.low","price.close") ], n = atr_n) )
atr <- atr [, allrow := .I, ]

a04all <- Reduce(function(...) merge(..., by = c("allrow"), all=T),  
                 list( a03all, atr) )

a04all <- a04all [, nrow := .I, by =.(ticker)]

# To avoid any incorrect calculations explained above, remove certain number of rows
a04all <- a04all [ nrow > atr_n ]

a04all <- a04all [, `:=` (ema20 = ema(price.close, 20), 
                          ema50 = ema(price.close, 50),
                          ema200 = ema(price.close, 200),
                          smavol20 = sma(volume, 20),
                          max20 = roll_max(price.close, 20),
                          max55 = roll_max(price.close, 55), 
                          min10 = roll_max(price.close, 10), 
                          min20 = roll_max(price.close, 20) ), by =.(ticker)]

a04all <- a04all [, above200ema := ifelse(price.close >= ema200, "Above_200ema", "Below_200ema"), ]
a04all <- a04all [, `:=`(above55max = ifelse(price.close >= max55, "Above_55", ""),
                         above20max = ifelse(price.close >= max20, "Above_20", ""),
                         above55max02 = ifelse(price.close >= max55, 1, 0),
                         above20max02 = ifelse(price.close >= max20, 1, 0),
                         overalltrnd = ifelse(price.close >= ema20 & ema20 >= ema50 & ema50 >= ema200, 1, 0 ) ), ]

a04all <- na.omit(a04all)
a04all <- a04all [, qudrant := case_when(jdk_rs55 > 100 & jdk_momratio55 > 100 ~ 1,
                                         jdk_rs55 > 100 & jdk_momratio55 < 100 ~ 2,
                                         jdk_rs55 < 100 & jdk_momratio55 > 100 ~ 3,
                                         jdk_rs55 < 100 & jdk_momratio55 < 100 ~ 4 ), ]


a04all <- a04all [, `:=` (above55max02sum = Reduce(`+`, shift(above55max02, 0:24)), 
                          above20max02sum = Reduce(`+`, shift(above20max02, 0:24)) ), by =.(ticker)]

######################################################################
#
#                   F&O use
#
# Use of turtle 20 and 50 day breakout on the f&o stocks
# If the success rate is high then use this idea to buy the ATM CE
#
######################################################################

fno <- fread("https://archives.nseindia.com/content/fo/fo_mktlots.csv")
fno <- fno [, c("SYMBOL"), ]
fno <- fno [, `:=` (nrow = .I, SYMBOL02 = paste(SYMBOL, ".NS", sep="") ), ]

turtle001_20 <- a04all [ overalltrnd == 1 & above20max02 == 1 & above20max02sum == 1 ]
turtle001_50 <- a04all [ overalltrnd == 1 & above55max02 == 1 & above55max02sum == 1 ]

turtle001_all <- rbindlist( list(turtle001_20, turtle001_20), idcol ="Condition")

# Create a counf of number signals per stock per condition
turtle001_all <- turtle001_all [, `:=`(signal = 1:.N, totsignal = .N), by =.(Condition, ticker)]

# Check how many stocks appear on each day
turtle002 <- turtle001_all [ticker %in% fno$SYMBOL02, .(n = uniqueN(ticker),
                                                        stocks = paste(ticker, collapse = ",", sep = "") ), by = .(Condition, ref.date, qudrant)]

# Based on the following page, the heatmap of trades is generated
# https://towardsdatascience.com/time-series-calendar-heatmaps-9f576578fcfe

turtle002 <- turtle002 [, weekday := as.POSIXlt(ref.date)$wday, ]
turtle002 <- turtle002 [, weekdayf := factor(weekday,levels=rev(1:7),labels=rev(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")),ordered=TRUE) , ]
turtle002 <- turtle002 [, monthf := factor(month(ref.date),levels=as.character(1:12),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE) , ]
turtle002 <- turtle002 [, yearmonth := factor(as.yearmon(ref.date)), ]
turtle002 <- turtle002 [, week := as.numeric(format(ref.date,"%W")), ]
turtle002 <- turtle002 [, monthweek := 1+week-min(week), by =.(yearmonth) ]

turtle002 <- turtle002 [, ntrademonth := sum(n), by = .(Condition, yearmonth)]

p <- ggplot(turtle002 [ qudrant ==1 ], aes(monthweek, weekdayf, fill = n)) + 
  geom_tile(colour = "white") + 
  facet_grid(year(ref.date) ~ monthf) + 
  scale_fill_gradient(low="red", high="green") + 
  xlab("Week of Month") + ylab("") + 
  ggtitle("Time-Series Calendar Heatmap: Trades generated by 20 day turtle") + 
  labs(fill = "Number of trades in quadrant 1") 
p



# https://community.rstudio.com/t/what-is-the-simplest-way-to-make-an-interactive-bar-plot-with-multiple-drop-down-filters-with-shiny-and-ggplot-combo/48917/2
library(shiny)

tickers <- unique(turtle001_all$ticker)
dates <- unique(a05all$ref.date)


# Define UI for application that draws a histogram
ui <- fluidPage(
  {
    
    uiOutput("secondSelection")
    
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
  }
)

observe({
  updateSelectInput(session, "User", choices = as.character(dat5[dat5$email==input$Select, date]))
})

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$secondSelection <- renderUI({
    selectInput("User", "Date:", choices = as.character(dat5[dat5$email==input$Select,"date"]))
  })  
  
  output$Plot <- renderPlot({
    
    data = a04all [ ticker %in% c(input$selects) ]
    #data02 = data & (ref.date >= input$daterange1[1] + 15 & ref.date <= input$daterange1[2] - 15) ] 
    
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




library(shiny)
# Reprex: The actual implementation of this uses data from a file:
#    1. Reads data file before ui and server are established
#    2. Does a bunch of calculations
#    3. Identifies dates that exist in data file
#    4. The data file is getting updated in the background from another application.
#    5. Allows user to click the button to update the data file. Reprex shows code
#       that is used to update the date selector based on new data read. Dates are 
#       random in reprex, but would come from data file in actual code.

# Sample 3 dates and disable the rest - actual code reads data file here
#   and parses out dates that exist in records

my_dates <- seq(as.Date('2021-01-01'), as.Date('2021-01-31'), by = "day")
date_choices <- sample(my_dates, 31-3)

ui <- fluidPage(
  uiOutput("date"), textOutput("disabled"),
  actionButton("click", "Click Me")
)

#
# https://stackoverflow.com/questions/65943772/no-datesdisabled-in-updatedateinput-in-r-shiny
#
#
server <- function(input, output, session) {
  dates_disabled <- reactiveVal(NULL)
  
  # Init 'dates_disabled()' once before Shiny flushes the reactive system with callback,
  #   using date_choices that exist in original data set
  
  onFlush(fun = function () {dates_disabled(date_choices)}, once = TRUE)
  
  # dateInput widget
  output$date <- renderUI({
    maxDate <- as.Date(max(setdiff(my_dates, dates_disabled())),
                       origin = "1970-01-01")
    dateInput(input = "date", 
              label = "Select Date",
              min = min(my_dates),
              max = max(my_dates),
              value = maxDate,
              datesdisabled = dates_disabled())
  })
  
  # This output makes it easier to test if it works by showing the enabled dates
  output$disabled <- renderPrint({
    req(dates_disabled()) # only run this when 'dates_disabled' is initialized properly
    Enabled <- as.Date(setdiff(seq(as.Date('2021-01-01'), as.Date('2021-01-31'), by = "day"), 
                               dates_disabled()), 
                       origin = '1970-01-01')
    paste("Enabled:", paste(Enabled[order(Enabled)], collapse = ", "))
  })
  
  # Set new datesdisabled on button click
  #    Actual code would read updated data file and parse new dates
  observeEvent(input$click, {
    SelectedDates <- sample(my_dates, 31-3)
    dates_disabled( SelectedDates )
  })
}

shinyApp(ui, server)