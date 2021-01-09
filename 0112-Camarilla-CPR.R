
dt <- Sys.Date()

# Create the reference values for the previous periods:

# Find previous year start to end
prvyr <- as.numeric(format(as.Date( floor_date(dt, "year") - years(1) ), "%Y"))
prvyr01 <- as.Date(paste(prvyr, "01", "01", sep="-"))


# Get the yearly, monthly, weekly, daily values:
styrdate <- as.numeric(as.POSIXct(prvyr01, format="%Y-%m-%d"))
endaydate <- as.numeric(as.POSIXct(dt+1, format="%Y-%m-%d")) # Vinay update 9th Jan 2021

url_all <- paste("https://in.investing.com/indices/bank-nifty-futures-historical-data?end_date=", endaydate,  
                "&st_date=", styrdate, "&interval_sec=monthly&interval_sec=daily", sep="")

url_html_all <- read_html(url_all)

# extract the HTML table for year
whole_table_all <- url_html_all %>% 
  html_nodes('table') %>%
  html_table(fill = TRUE) %>%
  .[[2]]

whole <- as.data.table(whole_table_all)
whole <- whole [, `:=`(date02 = mdy(Date),
                       High = as.numeric( str_remove_all(High, ",") ), 
                       Low = as.numeric( str_remove_all(Low, ",") ) ,
                       Price = as.numeric( str_remove_all(Price, ",") ) ,
                       Open = as.numeric( str_remove_all(Open, ",") ) ),  ]

whole <- whole [ order (date02) ]
whole <- whole [, `:=`(prvyr = as.numeric(format(as.Date( floor_date(date02, "year") - years(1) ), "%Y")), 
                       prvyr01 = as.Date(paste(prvyr, "01", "01", sep="-")), 
                       prvmnth = floor_date(date02, "month") - months(1), 
                       prvwk = floor_date(date02, "week") - weeks(1),
                       prvday = shift(date02, type ="lag"),
                       
                       curyr = as.numeric(format(as.Date( floor_date(date02, "year") ), "%Y")), 
                       curmnth = floor_date(date02, "month"), 
                       curwk = floor_date(date02, "week"), 
                       curday = date02), ]

# Transpose the data by previous day, week, month, year into 1 dataset
# Transpose the data by current day, week, month, year into 1 dataset

prvdata <- melt(data = whole, 
                id.vars = c("date02", "Price", "Open", "High", "Low", "Volume", "Chg%"),
                measure.vars = c("prvyr", "prvmnth", "prvwk", "prvday"))

prvdata <- prvdata [, value02 := ifelse(! variable == "prvyr", as.character( anydate(value) ), value), ]

# Create high, low, close values for each week, month, year

prvdata <- prvdata [, close := as.numeric( last(Price) ), by = .(variable, value02) ]
prvdata <- prvdata [, `:=`(high_t = max( High ),
                       low_t = min ( Low ) ) , by = .(variable, value02)]


##########################################################################################
#
# Calculate the CPR related parameters:
# (1) Create pivot point which is the average of PP = (high_t + Low_t + close) / 3
# (2) Bottom central level: BC = (high_t + low_t) / 2
# (3) Top central level: TC = PP - BC + PP
#
# (4) CPR width: cprwidth = (ABS(TC - BC)/ PP )*100
# Below is the interpretation of the reading.
#
# CPR Width > 0.5 - Sideways or Trading Range Day,
# CPR Width > 0.75 - increases the likelihood of sideways trading behavior,
# CPR Width < 0.5 - Trending type of day,
# CPR Width < 0.25 - increases the likelihood of a trending market.
#
##########################################################################################

prvdata <- prvdata [, `:=`(pp = (high_t + low_t + close) / 3,
                       bc = (high_t + low_t) / 2 ), by = .(variable, value02)]
prvdata <- prvdata [, tc := pp - bc + pp, by = .(variable, value02)]
prvdata <- prvdata [, cprwidth := (abs(tc - bc)/ pp )*100, by = .(variable, value02)]


#############################################################################
#
# Calculate Camarilla S1, S2, S3, S4, S5, S6 and
#                     R1, R2, R3, R4, R5, R6 values for each time frame
#
# S1 = close - (high_t - low_t) * 1.1/12
# S2 = close - (high_t - low_t) * 1.1/6
# S3 = close - (high_t - low_t) * 1.1/4
# S4 = close - (high_t - low_t) * 1.1/2
# S5 = S4 - (S3 - S4) * 1.168
# S6 = close - (R6 - close)
#
# R1 = close + (high_t - low_t) * 1.1/12
# R2 = close + (high_t - low_t) * 1.1/6
# R3 = close + (high_t - low_t) * 1.1/4
# R4 = close + (high_t - low_t) * 1.1/2
# R5 = R4 + (R4 - R3) * 1.168
# R6 = (high_t/low_t) * close
#
#############################################################################

prvdata <- prvdata [, `:=` (s1 = close - (high_t - low_t) * 1.1/12,
                        s2 = close - (high_t - low_t) * 1.1/6, 
                        s3 = close - (high_t - low_t) * 1.1/4, 
                        s4 = close - (high_t - low_t) * 1.1/2,
                        r1 = close + (high_t - low_t) * 1.1/12,
                        r2 = close + (high_t - low_t) * 1.1/6, 
                        r3 = close + (high_t - low_t) * 1.1/4, 
                        r4 = close + (high_t - low_t) * 1.1/2),  by = .(variable, value02)]

prvdata <- prvdata [, `:=`(s5 = s4 - (s3 - s4) * 1.168,
                       r5 = r4 + (r4 - r3) * 1.168,
                       r6 = (high_t/low_t) * close), by =.(variable, value02)]

prvdata <- prvdata [, s6 := close - (r6 - close), by =.(variable, value02)]
prvdata <- prvdata [, r3s3width := r3 - s3, by =.(variable, value02)]


# Create unique records for the week, month and year

unqprvdata <- unique ( prvdata [variable %in% c("prvyr", "prvmnth", "prvwk"), 
                       c("variable", "value", "value02", "s1", "s2", "s3", "s4", "s5", "s6", 
                         "r1", "r2", "r3", "r4", "r5", "r6", 
                         "pp", "bc", "tc", "cprwidth", "r3s3width"), ] )


unqprvdata_day <- unique ( prvdata [variable %in% c("prvday"), 
                                c("variable", "value", "value02", "s1", "s2", "s3", "s4", "s5", "s6", 
                                  "r1", "r2", "r3", "r4", "r5", "r6", 
                                  "pp", "bc", "tc", "cprwidth", "r3s3width"), ] )


# do the same for current date

curdata <- melt(data = whole, 
                id.vars = c("date02", "Price", "Open", "High", "Low", "Volume", "Chg%"),
                measure.vars = c("curyr", "curmnth", "curwk", "curday"))

curdata <- curdata [, value02 := ifelse(! variable == "curyr", as.character( anydate(value) ), value), ]

# Create high, low, close values for each week, month, year

curdata <- curdata [, close := as.numeric( last(Price) ), by = .(variable, value02) ]
curdata <- curdata [, `:=`(high_t = max( High ),
                           low_t = min ( Low ) ) , by = .(variable, value02)]


##########################################################################################
#
# Calculate the CPR related parameters:
# (1) Create pivot point which is the average of PP = (high_t + Low_t + close) / 3
# (2) Bottom central level: BC = (high_t + low_t) / 2
# (3) Top central level: TC = PP - BC + PP
#
# (4) CPR width: cprwidth = (ABS(TC - BC)/ PP )*100
# Below is the interpretation of the reading.
#
# CPR Width > 0.5 - Sideways or Trading Range Day,
# CPR Width > 0.75 - increases the likelihood of sideways trading behavior,
# CPR Width < 0.5 - Trending type of day,
# CPR Width < 0.25 - increases the likelihood of a trending market.
#
##########################################################################################

curdata <- curdata [, `:=`(pp = (high_t + low_t + close) / 3,
                           bc = (high_t + low_t) / 2 ), by = .(variable, value02)]
curdata <- curdata [, tc := pp - bc + pp, by = .(variable, value02)]
curdata <- curdata [, cprwidth := (abs(tc - bc)/ pp )*100, by = .(variable, value02)]


#############################################################################
#
# Calculate Camarilla S1, S2, S3, S4, S5, S6 and
#                     R1, R2, R3, R4, R5, R6 values for each time frame
#
# S1 = close - (high_t - low_t) * 1.1/12
# S2 = close - (high_t - low_t) * 1.1/6
# S3 = close - (high_t - low_t) * 1.1/4
# S4 = close - (high_t - low_t) * 1.1/2
# S5 = S4 - (S3 - S4) * 1.168
# S6 = close - (R6 - close)
#
# R1 = close + (high_t - low_t) * 1.1/12
# R2 = close + (high_t - low_t) * 1.1/6
# R3 = close + (high_t - low_t) * 1.1/4
# R4 = close + (high_t - low_t) * 1.1/2
# R5 = R4 + (R4 - R3) * 1.168
# R6 = (high_t/low_t) * close
#
#############################################################################

curdata <- curdata [, `:=` (s1 = close - (high_t - low_t) * 1.1/12,
                            s2 = close - (high_t - low_t) * 1.1/6, 
                            s3 = close - (high_t - low_t) * 1.1/4, 
                            s4 = close - (high_t - low_t) * 1.1/2,
                            r1 = close + (high_t - low_t) * 1.1/12,
                            r2 = close + (high_t - low_t) * 1.1/6, 
                            r3 = close + (high_t - low_t) * 1.1/4, 
                            r4 = close + (high_t - low_t) * 1.1/2),  by = .(variable, value02)]

curdata <- curdata [, `:=`(s5 = s4 - (s3 - s4) * 1.168,
                           r5 = r4 + (r4 - r3) * 1.168,
                           r6 = (high_t/low_t) * close), by =.(variable, value02)]

curdata <- curdata [, s6 := close - (r6 - close), by =.(variable, value02)]
curdata <- curdata [, r3s3width := r3 - s3, by =.(variable, value02)]


# Create unique records for the week, month and year

unqcurvdata <- unique ( curdata [variable %in% c("curyr", "curmnth", "curwk"), 
                                c("variable", "value", "value02", "s1", "s2", "s3", "s4", "s5", "s6", 
                                  "r1", "r2", "r3", "r4", "r5", "r6", 
                                  "pp", "bc", "tc", "cprwidth", "r3s3width"), ] )


unqcurdata_day <- unique ( curdata [variable %in% c("curday"), 
                                    c("variable", "value", "value02", "s1", "s2", "s3", "s4", "s5", "s6", 
                                      "r1", "r2", "r3", "r4", "r5", "r6", 
                                      "pp", "bc", "tc", "cprwidth", "r3s3width"), ] )




# Calculate the 5, 20, 50 sma for the cprwidth and r3s3width
# to understand whether the current value is abnormal or normal


