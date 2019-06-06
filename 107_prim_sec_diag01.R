library(data.table)
library(tidyverse)
library(cumstats)

#all_met_rmsd02 <- fread("C://Users//mahajvi1//Downloads//01adsl_met_rmsd.csv")
all_met_rmsd02 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/01adsl_met_rmsd.rds")
all_met_rmsd02 <- all_met_rmsd02 [, Code := ifelse (Code == " " | Code == "", "** Not yet coded", Code),]
all_met_rmsd02 <- all_met_rmsd02 [, description:= ifelse (description == "" | description ==" ", "** Not yet coded", description),]
all_met_rmsd02 <- all_met_rmsd02 [, Code02 := paste(distype, ":", Code, ":", description, sep =""), ]
all_met_rmsd02 <- all_met_rmsd02 [, Med02 := paste(Type_med, ":", Coded_med, sep =""), ]

all_met_rmsd03 <- unique(all_met_rmsd02 [, c("mr_no", "studyday", "vis", "all_vis", "cdur", 
                                             "Code", "description", "Code02",
                                             "Type_med", "Coded_med", "Med02")] )

diag3 <- unique( all_met_rmsd03[, c("mr_no", "Code02", "studyday"), ] ) 

diag4 <- diag3 [, `:=`(primarycode = Code02,
                       primaryday = studyday), ]

diag3 <- diag3 [, c("mr_no", "Code02", "studyday"), ]
diag4 <- diag4 [, -c("Code02", "studyday"), ]

# set the ON clause as keys of the tables:
setkey(diag3, mr_no)
setkey(diag4, mr_no)

# perform the join
prim_diag <- merge(x = diag3,
                   y = diag4, 
                   by = c("mr_no"),
                   all=TRUE, 
                   allow.cartesian = TRUE)

prim_diag02 <- prim_diag [ primaryday <= studyday]
prim_diag02 <- prim_diag02 [, diff := studyday - primaryday,]


t01resp_rel <- prim_diag02 [, .(n = uniqueN(mr_no),
                                mean = mean(diff, na.rm = FALSE),
                                sd = sd(diff, na.rm = FALSE),
                                median = median (diff, na.rm = FALSE),
                                min = min (diff, na.rm = FALSE),
                                max = max (diff, na.rm = FALSE)), by = .(primarycode, Code02)]


fwrite(prim_diag02, "D:/Hospital_data/ProgresSQL/analysis/107_prim_sec_diag01.csv")

####################################################################
#
####################################################################
