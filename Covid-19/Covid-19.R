# http://datastorm-open.github.io/visNetwork/options.html
# https://stackoverflow.com/questions/22873602/importing-data-into-r-from-google-spreadsheet

#install.packages('XML')
library(data.table)
library(XML)
library(anytime)
library(tidyverse)
library(sqldf)
library(binom)

readGoogleSheet <- function(url, na.string="", header=TRUE){
  stopifnot(require(XML))
  # Suppress warnings because Google docs seems to have incomplete final line
  suppressWarnings({
    doc <- paste(readLines(url), collapse=" ")
  })
  if(nchar(doc) == 0) stop("No content found")
  htmlTable <- gsub("^.*?(<table.*</table).*$", "\\1>", doc)
  ret <- readHTMLTable(htmlTable, header=header, stringsAsFactors=FALSE, as.data.frame=TRUE)
  lapply(ret, function(x){ x[ x == na.string] <- NA; x})
}

cleanGoogleTable <- function(dat, table=1, skip=0, ncols=NA, nrows=-1, header=TRUE, dropFirstCol=NA){
  if(!is.data.frame(dat)){
    dat <- dat[[table]]
  }
  
  if(is.na(dropFirstCol)) {
    firstCol <- na.omit(dat[[1]])
    if(all(firstCol == ".") || all(firstCol== as.character(seq_along(firstCol)))) {
      dat <- dat[, -1]
    }
  } else if(dropFirstCol) {
    dat <- dat[, -1]
  }
  
  if(skip > 0){
    dat <- dat[-seq_len(skip), ]
  }
  
  if(nrow(dat) == 1) return(dat)
  
  if(nrow(dat) >= 2){
    if(all(is.na(dat[2, ]))) dat <- dat[-2, ]
  }
  if(header && nrow(dat) > 1){
    header <- as.character(dat[1, ])
    names(dat) <- header
    dat <- dat[-1, ]
  }
  
  # Keep only desired columns
  if(!is.na(ncols)){
    ncols <- min(ncols, ncol(dat))
    dat <- dat[, seq_len(ncols)]
  }
  
  # Keep only desired rows
  if(nrows > 0){
    nrows <- min(nrows, nrow(dat))
    dat <- dat[seq_len(nrows), ]
  }
  
  # Rename rows
  rownames(dat) <- seq_len(nrow(dat))
  dat
}

#u <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSc_2y5N0I67wDU38DjDh35IZSIS30rQf7_NYZhtYYGU1jJYT6_kDx4YpF-qw0LSlGsBYP8pqM_a1Pd/pubhtml"
u <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSz8Qs1gE_IYpzlkFkCXGcL_BqR8hZieWVi-rphN1gfrO3H4lDtVZs4kd0C3P8Y9lhsT1rhoB-Q_cP4/pubhtml"

g <- readGoogleSheet(u)

g01 <- as.data.table ( cleanGoogleTable(g, table=1) )
g02 <- as.data.table ( cleanGoogleTable(g, table=2) )
g03 <- as.data.table ( cleanGoogleTable(g, table=3) )
g04 <- as.data.table ( cleanGoogleTable(g, table=4) )
g05 <- as.data.table ( cleanGoogleTable(g, table=5) )
g06 <- as.data.table ( cleanGoogleTable(g, table=6) )
g07 <- as.data.table ( cleanGoogleTable(g, table=7) )
g08 <- as.data.table ( cleanGoogleTable(g, table=8) )
g09 <- as.data.table ( cleanGoogleTable(g, table=9) )
g10 <- as.data.table ( cleanGoogleTable(g, table=10) )
g11 <- as.data.table ( cleanGoogleTable(g, table=11) )
g12 <- as.data.table ( cleanGoogleTable(g, table=12) )
#g13 <- as.data.table ( cleanGoogleTable(g, table=13) )
#g14 <- as.data.table ( cleanGoogleTable(g, table=14) )

g01 <- g01 [, c(1:19), ]
setnames(x=g01, old=names(g01), new=gsub(" ","",names(g01)))

g01 <- g01 [, `:=` (DateAnnounced02 = anydate( as.POSIXct( gsub("-", "/", DateAnnounced), format="%d/%m/%Y")),
                    StatusChangeDate02 = anydate( as.POSIXct( gsub("-", "/", StatusChangeDate), format="%d/%m/%Y")),
                    patient = paste("P", PatientNumber, sep=""), 
                    incontact = `ContractedfromwhichPatient(Suspected)`) ,]
g01 <- g01 [, dur := StatusChangeDate02 - DateAnnounced02 + 1,]


# Get the first case in each state:
g01_first <- g01 [ , firstState := min(DateAnnounced02), by = .(DetectedState)]

# Get first in India
g01_first <- g01_first [, firstIndia := min(DateAnnounced02, na.rm = TRUE), ]

# Difference between first in India and each state
g01_first <- g01_first [, tmtofstcase := firstState - firstIndia + 1,]

# SPlit the records inot multiple lines for contact with multiple patients
g01_first02 <- data.table( separate_rows (g01_first, incontact, sep= ",") )
g01_first02 <- g01_first02 [, notes01 := tolower(Notes)]

g01_first02 <- g01_first02 [, crit := case_when( Notes %like% c("Travelled from") ~ "01 International travel",
                                                 tolower(Notes) %like% c("details awaited") ~ "00 Details awaited", 
                                                 tolower(Notes) %like% c("transmission") | 
                                                   tolower(Notes) %like% c("transmitted") | tolower(Notes) %like% c("contact")  ~ "02 Transmission", 
                                                 tolower(Notes) %like% c("mother") | tolower(Notes) %like% c("family") |
                                                   tolower(Notes) %like% c("father") | tolower(Notes) %like% c("spouse") |
                                                   tolower(Notes) %like% c("daughter") | tolower(Notes) %like% c("sister") |
                                                   tolower(Notes) %like% c("son") | tolower(Notes) %like% c("close contact") |
                                                   tolower(Notes) %like% c("wife")| tolower(Notes) %like% c("worker") |
                                                   tolower(Notes) %like% c("relative") ~ "03 Family / maid / help",
                                                 tolower(Notes) %in% c("staff of private hospital") |
                                                   tolower(Notes) %in% c("hospital staff")  ~ "04 Healthcare"), ]



# Announcement Date for each patient 
g01_ancdate <- unique( g01_first02 [, c("patient", "DateAnnounced02"),] )
setnames(g01_ancdate, "DateAnnounced02", "ancdate")

g01_first03 <- merge (x = g01_first02, 
                      y = g01_ancdate, 
                      by.x = c("incontact"),
                      by.y = c("patient"),
                      all.x = TRUE)
g01_first03 <- g01_first03 [, dt2con := as.numeric(DateAnnounced02 - ancdate + 1), ]

fwrite(g01_first03, "D:\\Hospital_data\\ProgresSQL\\covid-19\\analysis\\covid_g01.csv")

# Bar chart race in tableau:
# Cumulative sum of cases by date for each state:
state01 <- unique( g01_first03 [!is.na(DateAnnounced02), c("DetectedState", "DateAnnounced02", "PatientNumber", "patient"), ] )
state01 <- state01 [ order (DetectedState, DateAnnounced02)]
state02 <- state01 [, .(cumpat = uniqueN(patient) ), by = .(DetectedState, DateAnnounced02)]
state02 <- state02 [, cumpat02 := cumsum(cumpat), by = .(DetectedState)]

# unique combinations
dates <- unique ( g01_first03 [ !is.na(DateAnnounced02), c("DateAnnounced02"), ])
states <- unique ( g01_first03 [ !is.na(DateAnnounced02), c("DetectedState"), ])
dtst <- as.data.table ( CJ(DateAnnounced02 = dates$DateAnnounced02, DetectedState =states$DetectedState) )

state03 <- merge (x = state02,
                  y = dtst,
                  by = c("DetectedState", "DateAnnounced02"),
                  all = TRUE)

state03 <- state03 [! is.na(state03$DetectedState)]
state03[is.na(state03)] <- 0
state03 <- state03 [, cumpat03 := cumsum(cumpat), by = .(DetectedState)]
state03 <- state03 [order(DateAnnounced02, -cumpat03, DetectedState)]
state03 <- state03 [, `:=`(tot = .N, ord = 1:.N), by = .(DateAnnounced02)]
state03 <- state03 [, lbl := paste(DetectedState, "(Cum Cases:", cumpat03, "), (Cases on date:", cumpat, ")", sep=""),]

state03 <- state03 [order(DateAnnounced02, -cumpat, DetectedState)]
state03 <- state03 [, `:=`(tot02 = .N, orddt = 1:.N), by = .(DateAnnounced02)]
state03 <- state03 [, orddt := ifelse(orddt ==1 , 1, 99),]
fwrite(state03 [, -c("cumpat02"), ], "D:\\Hospital_data\\ProgresSQL\\covid-19\\analysis\\covid_g01_byCumSumState.csv")


drive_upload("D:/Hospital_data/ProgresSQL/covid-19/analysis/covid_g01.csv",
             path = "/Covid-19/analysis/gs_covid_g01.csv",
             type = "spreadsheet", 
             overwrite = TRUE)

drive_upload("D:/Hospital_data/ProgresSQL/covid-19/analysis/covid_g01_byCumSumState.csv",
             path = "/Covid-19/analysis/gs_covid_g01_byCumSumState.csv",
             type = "spreadsheet", 
             overwrite = TRUE)

################################################################
# Calculate the 7 day rolling change 
# Calculate this for whole of India and individual state:
# If the ratio = 2, then it means: the number of cases have doubled (100% increase)
# If the ratio = 2, then it is doubling of cases in 1 week
#
# If the ratio = 1, then it means: the number of cases have remained the same
# If the ratio = 1.25, then 25% increase compared to 1 week ago
#
################################################################

state01a <- unique( g01_first03 [!is.na(DateAnnounced02), c("DetectedState", "DateAnnounced02", "PatientNumber", "patient"), ] )

state00 <- copy(state01a)
state00 <- state00 [, DetectedState := "** Overall India", ]
state01 <- rbind(state01a, state00)

state01 <- state01 [ order (DetectedState, DateAnnounced02)]
state02 <- state01 [, .(cumpat = uniqueN(patient) ), by = .(DetectedState, DateAnnounced02)]
state02 <- state02 [, cumpat02 := cumsum(cumpat), by = .(DetectedState)]

state03 <- merge (x = state02,
                  y = dtst,
                  by = c("DetectedState", "DateAnnounced02"),
                  all = TRUE)

state03 <- state03 [! is.na(state03$DetectedState)]
state03[is.na(state03)] <- 0
state03 <- state03 [, cumpat03 := cumsum(cumpat), by = .(DetectedState)]
state03 <- state03 [order(DateAnnounced02, -cumpat03, DetectedState)]
state03 <- state03 [, `:=`(tot = .N, ord = 1:.N), by = .(DateAnnounced02)]
state03 <- state03 [, lbl := paste(DetectedState, "(Cum Cases:", cumpat03, "), (Cases on date:", cumpat, ")", sep=""),]
state03 <- state03 [order(DateAnnounced02, -cumpat, DetectedState)]
state03 <- state03 [, `:=`(tot02 = .N, orddt = 1:.N), by = .(DateAnnounced02)]
state03 <- state03 [, orddt := ifelse(orddt ==1 , 1, 99),]

#####################################################
# Calculate weekly rolling change:
# Take ratio of today's cases and cases 7 days ago
#####################################################
state04 <- state03 [, mv7 := shift (cumpat03, n = 6, type = c("lag")), by = .(DetectedState)]
state04 <- state04 [, ntimes := ifelse(mv7>0, round(cumpat03 / mv7, 2), ""),]
state04 <- state04 [, ntimesperc := ifelse(ntimes > 0, round( as.numeric(ntimes) / 2 * 100, 2 ), "" ),]
state04 <- state04 [, disp := ifelse (ntimes >0 , paste(cumpat03, "/", mv7, ", ", ntimes, sep =""), ""),]

#############################################
# Check if there is any new case reported 
#############################################
state04 <- state04 [, prvcase := shift(cumpat03, type = c("lag")), by = .(DetectedState)]
state04 <- state04 [, newcase := ifelse(cumpat03 == prvcase, "No", "Yes"),]

fwrite(state04 [, -c("cumpat02"), ], "D:\\Hospital_data\\ProgresSQL\\covid-19\\analysis\\covid_g01_wklychg_newcase.csv")


########################################################
# Just to see in the R studio the transposed version,
# Use the vertical version in Tableau
########################################################
state04_trn <- dcast(data = state04 [ntimes > 0],
                     DateAnnounced02 ~ DetectedState,
                     value.var = c ("disp"),
                     fill = " ")



####################################################
# Added on 2nd May 2020
# 
# based on the new data structure:
# This part of the code will focus on 
# +ve cases / tested cases
# total testes cases / population (10 lacs)
####################################################

raw0102 <- fread("https://api.covid19india.org/csv/latest/raw_data.csv")
raw03 <- fread("https://api.covid19india.org/csv/latest/raw_data3.csv")

setnames(x=raw0102, old=names(raw0102), new=gsub(" ","",names(raw0102)))
setnames(x=raw03, old=names(raw03), new=gsub(" ","",names(raw03)))

raw0102 <- raw0102 [, DetectedState := ifelse(str_squish(DetectedState) == "", "** Unknown", DetectedState),]
raw0102 <- raw0102 [, DetectedDistrict := ifelse(str_squish(DetectedDistrict) == "", "** Unknown", paste("**", DetectedState, sep =" ") ),]

raw03 <- raw03 [, DetectedState := ifelse(str_squish(DetectedState) == "", "** Unknown", DetectedState),]
raw03 <- raw03 [, DetectedDistrict := ifelse(str_squish(DetectedDistrict) == "", "** Unknown", paste("**", DetectedState, sep =" ") ),]


chk <- raw03 [, .(cnt = sum(NumCases)), by = .(DateAnnounced, DetectedState, DetectedDistrict, CurrentStatus)]


test01 <- fread("https://api.covid19india.org/csv/latest/statewise_tested_numbers_data.csv")
setnames(x=test01, old=names(test01), new=gsub(" ","",names(test01)))
setnames(test01, "Population(Source:UIDAI)", "popnUIDAI")
test01 <- test01 [, DateAnnounced02 := anydate( as.POSIXct( gsub("-", "/", UpdatedOn), format="%d/%m/%Y")), ]

test011 <- copy (test01)
test011 <- test011 [, .(TotalTested = sum(TotalTested), Positive = sum(Positive) ), by = . (DateAnnounced02)]
test011 <- test011 [, State := "** Overall India", ]
test02 <- rbind(test01, test011, fill = TRUE)

# Populate the population column on all rows:
popn <- test01 [!is.na(popnUIDAI) , c("State", "popnUIDAI"),]

popn2 <- popn [, .(popnUIDAI = sum(popnUIDAI)), ]
popn2 <- popn2 [, State := "** Overall India", ]
popn3 <- rbind(popn, popn2)

test02 <- merge (x = test02 [, -c("popnUIDAI"), ],  
                 y = popn3,
                 by = c("State"))


test03 <- test02 [TotalTested >= 0 & Positive >= 0]
test03 <- test03 [, `:=` (TotalTested = as.numeric(TotalTested), Positive = as.numeric(Positive)),]

CI <- binom.confint (test03$Positive, test03$TotalTested, method ="wilson")
CIpop <-  as.data.table(binom.confint (test03$TotalTested, test03$popnUIDAI, method ="wilson") )
setnames(CIpop, "mean", "meanpop")
setnames(CIpop, "lower", "lowerpop")
setnames(CIpop, "upper", "upperpop")

test04 <- cbind(test03, CI, CIpop [,c("meanpop", "lowerpop", "upperpop"), ])
test04 <- test04 [, `:=` (mean = round (mean * 100, 2),
                          lower = round (lower * 100, 2),
                          upper = round (upper * 100, 2),
                          meanpop = round (meanpop * 1000000, 2),
                          lowerpop = round (lowerpop * 1000000, 2), 
                          upperpop = round (upperpop * 1000000, 2) ), ]

test04 <- test04 [, c("State", "DateAnnounced02", "TotalTested", "Positive", 
                      "mean", "lower", "upper",
                      "meanpop", "lowerpop", "upperpop"), ]

fwrite(test04, "D:\\Hospital_data\\ProgresSQL\\covid-19\\analysis\\covid_g01_cases_test_popn.csv")

########################################################################################


chk59 <- g01_first02 [PatientNumber == "59"]
chk <- g01_first02 [, .(nn = .N), by = .(crit)]
chk02 <- g01_first02[ is.na(crit), c("Notes"),]



g01_pat01 <- unique( g01_first02 [, c("PatientNumber", "patient", "incontact"), ] )
g01_pat01 <- g01_pat01 [, PatientNumber := as.numeric(PatientNumber), ]

# Combination of patients using who were incontact
g01_pat02 <- unique ( g01_pat01 [ !is.na(incontact)])
g01_pat02 <- g01_pat02 [, weight := uniqueN (PatientNumber), by =.(incontact)]
g01_pat02 <- g01_pat02 [, `:=` (incontact = as.numeric( str_replace(incontact, "P", "") ),
                                patient = as.numeric( str_replace(patient, "P", "") )),]

# Nodes creation
# Unique patient IDs
g01_pat03 <- unique( g01_first02 [, c("PatientNumber", "patient", "CurrentStatus"), ])
setnames(g01_pat03, "PatientNumber", "id")
setnames(g01_pat03, "patient", "label")

nodes <- g01_pat03 [ !is.na (id)]


g01_first02 <- g01_first02 [, title := paste(Nationality, AgeBracket, Gender, DetectedState, DateAnnounced02), ]

g01_pat01 <- unique( g01_first02 [, c("PatientNumber", "patient", "incontact", "title"), ] )
g01_pat01 <- g01_pat01 [, PatientNumber := as.numeric(PatientNumber), ]

# Combination of patients using who were incontact
g01_pat02 <- unique ( g01_pat01 [ !is.na(incontact)])
g01_pat02 <- g01_pat02 [, weight := uniqueN (PatientNumber), by =.(incontact)]
g01_pat02 <- g01_pat02 [, `:=` (incontact = as.numeric( str_replace(incontact, "P", "") ),
                                patient = as.numeric( str_replace(patient, "P", "") ) ),]

setnames(g01_pat02, "incontact", "from")
setnames(g01_pat02, "patient", "to")
setnames(g01_pat02, "PatientNumber", "id")
edges <- g01_pat02 [, c("from", "to", "title"),]

library(visNetwork)
library(networkD3)

visNetwork(nodes = nodes, edges = edges, width = "100%" ) %>% 
  visIgraphLayout(layout = "layout_with_fr") %>% 
  visEdges(arrows = "from") %>%
  visHierarchicalLayout() %>%
  visOptions(highlightNearest = list(enabled = T, hover = T), nodesIdSelection = T) %>%
  visInteraction(navigationButtons = TRUE)





g01_pat01 <- unique( g01_first03 [, c("PatientNumber", "patient", "incontact", "dt2con"), ] )
g01_pat01 <- g01_pat01 [, PatientNumber := as.numeric(PatientNumber), ]

# Combination of patients using who were incontact
g01_pat02 <- unique ( g01_first03 [ !is.na(incontact)])
g01_pat02 <- g01_pat02 [, weight := uniqueN (PatientNumber), by =.(incontact)]
g01_pat02 <- g01_pat02 [, `:=` (incontact = as.numeric( str_replace(incontact, "P", "") ),
                                patient = as.numeric( str_replace(patient, "P", "") )),]

setnames(g01_pat02, "incontact", "from")
setnames(g01_pat02, "patient", "to")
setnames(g01_pat02, "PatientNumber", "id")
setnames(g01_pat02, "dt2con", "length")

edges <- g01_pat02 [, c("from", "to", "length"),]

# Working solution for:
#, width = "70%", height = "70%"
visNetwork(nodes = nodes, edges = edges ) %>% 
  visIgraphLayout(layout = "layout_with_sugiyama", type ="full") %>% 
  visEdges(arrows = "from") %>%
  visOptions(highlightNearest = list(enabled = T, hover = T), nodesIdSelection = T) %>%
  visInteraction(navigationButtons = TRUE)



# Working solution
# Kamada Kawai layout:

final <- visNetwork(nodes = nodes, edges = edges, width = "70%", height = "70%" ) %>% 
  visLayout(randomSeed = 12345, improvedLayout = TRUE) %>% 
  visEdges(arrows = "from") %>%
  visOptions(highlightNearest = list(enabled = T, hover = T), nodesIdSelection = T, selectedBy ="CurrentStatus") %>%
  visInteraction(navigationButtons = TRUE)


# same as
visSave(final, file = "C://Users//mahajvi1//OneDrive - Novartis Pharma AG//Downloads//network.html", background = "black")
