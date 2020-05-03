# Tableu help
# Use https://www.tableau.com/about/blog/2015/7/use-radar-charts-compare-dimensions-over-several-metrics-41592

library(data.table)
library(tidyverse)
library(sqldf)

# Install the ggradar library
#devtools::install_github("ricardo-bion/ggradar", dependencies = TRUE)

library(ggradar)

all_met_rmsd <- readRDS("D:/Hospital_data/ProgresSQL/analysis/01adsl_met_rmsd.rds")
all_met_rmsd <- all_met_rmsd [, Code := ifelse (Code == " " | Code == "", "** Not yet coded", Code),]
all_met_rmsd <- all_met_rmsd [, description:= ifelse (description == "" | description ==" ", "** Not yet coded", description),]
all_met_rmsd <- all_met_rmsd [, Code02 := paste(distype, ":", Code, ":", description, sep =""), ]
all_met_rmsd <- all_met_rmsd [, Med02 := paste(Type_med, ":", Coded_med, sep =""), ]

all_met_rmsd02 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/all_met_rmsd02.rds")
all_met_rmsd02 <- all_met_rmsd02 [, Code := ifelse (Code == " " | Code == "", "** Not yet coded", Code),]
all_met_rmsd02 <- all_met_rmsd02 [, description:= ifelse (description == "" | description ==" ", "** Not yet coded", description),]
all_met_rmsd02 <- all_met_rmsd02 [, Code02 := paste(distype, ":", Code, ":", description, sep =""), ]
all_met_rmsd02 <- all_met_rmsd02 [, Med02 := paste(Type_med, ":", Coded_med, sep =""), ]

# Disease:
# (1) Distinct number of patients
t10 <- all_met_rmsd02 [, .(cal10 = uniqueN(mr_no)), by =.(refcode, refdesc)]
t10 <- t10 [, perc10 := as.numeric( ntile(cal10, 100) ), ]

# (2) Number of times a disease is reported
totdis <- unique( all_met_rmsd [ , c("Code", "description", "patient_id"),] )
  
t20 <- totdis [, .(cal20 = .N), by =.(Code, description)]
t20 <- t20 [, perc20 := as.numeric( ntile(cal20, 100) ), ]

# (3) Number for a specific disease (chronological number of disease reported by a patient)
# Calculate median number of disease reported for each disease
# Then calculate the percentile for each disease

numdis <- unique( all_met_rmsd [, c("mr_no", "Code", "description", "studyday"),])
numdis <- numdis [ order(mr_no, studyday, Code)]
numdis <- numdis [ , `:=`( COUNT = .N , ndis = 1:.N ),
               by = .(mr_no, Code, description) ]

t30 <- numdis [, .(cal30 = median(ndis)), by =.(Code, description)]
t30 <- t30 [, perc30 := as.numeric( ntile(cal30, 100) ), ]

# (4) Number of diseases before and after the specific disease
banumdis <- unique( all_met_rmsd02 [, c("mr_no", "Code", "description", "period", "periodn", "refcode", "refdesc"),])
banumdis <- banumdis [, classification := ifelse (period >=1 , "After", "Before"), ]
banumdis <- banumdis [ order(mr_no, refcode, refdesc, classification)]
banumdis <- banumdis [ , `:=`( ndis = uniqueN(Code) ),
                   by = .(mr_no, refcode, refdesc, classification) ]

t40 <- banumdis [, .(cal40 = median(ndis)), by =.(refcode, refdesc, classification)]
t40 <- t40 [, perc40 := as.numeric( ntile(cal40, 100) ), ]
t40_trn <- dcast(data = t40,
                 refcode + refdesc ~ classification,
                 value.var = c("cal40", "perc40"), 
                 fill ="0")

# (5) Number of treatments before and after the specific disease
banummed <- unique( all_met_rmsd02 [, c("mr_no", "Med02", "period", "periodn", "refcode", "refdesc"),])
banummed <- banummed [, classification := ifelse (period >=1 , "After", "Before"), ]
banummed <- banummed [ order(mr_no, refcode, refdesc, classification)]
banummed <- banummed [ , `:=`( nmed = uniqueN(Med02)),
                       by = .(mr_no, refcode, refdesc, classification) ]

t50 <- banummed [, .(cal50 = median(nmed)), by =.(refcode, refdesc, classification)]
t50 <- t50 [, perc50 := as.numeric( ntile(cal50, 100) ), ]
t50_trn <- dcast(data = t50,
                 refcode + refdesc ~ classification,
                 value.var = c("cal50", "perc50"), 
                 fill ="0")

#setnames(t10, "Code", "refcode")
setnames(t20, "Code", "refcode")
setnames(t30, "Code", "refcode")
#setnames(t10, "description", "refdesc")
setnames(t20, "description", "refdesc")
setnames(t30, "description", "refdesc")

all01 <- Reduce(function(...) merge(..., all.x = TRUE, by = c("refcode", "refdesc")),
                  list(t40_trn, t10, t20, t30, t50_trn))

all01 <- all01 [ refcode != "sandhigata vaa"]

all01_trn <- melt (data = all01,
                   id.vars = c("refcode", "refdesc"),
                   measure.vars = c("perc10", "perc20", "perc30", 
                                    "perc40_After", "perc40_Before",
                                    "perc50_After", "perc50_Before") )

all01_trn <- as.data.table ( sqldf("select *, 
                   case 
                                   When variable == 'perc10' then '1 Unique patients'
                                   when variable == 'perc20' then '2 no of times disease reported'
                                   when variable == 'perc30' then '3 disease chronology'
                                   when variable == 'perc40_After' then '4 no of diseases before'
                                   when variable == 'perc40_Before' then '5 no of diseases after'
                                   when variable == 'perc50_After' then '6 no of medicines before'
                                   when variable == 'perc50_Before' then '7 no of medicines after'
                                   end as category
                                   from all01_trn"))

# Possible background creation within Tableau
all01_trn <- all01_trn [, valuedumm := 100,]

# Used for the tableau visual
fwrite(all01_trn, file="D:/Hospital_data/ProgresSQL/analysis/300_radar_plot.csv")
