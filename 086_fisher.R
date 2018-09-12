library(data.table)
library(tidyverse)

all_met_rmsd <- readRDS("D:/Hospital_data/ProgresSQL/analysis/01adsl_met_rmsd.rds")

# Code ** NOT YET CODED for the missing code values
# Sort the data by patient and day

diag <- unique( all_met_rmsd [, c("mr_no", "Code", "description", "studyday"), with =FALSE] )

diag2 <- diag[, Code2 := ifelse(Code =="", "ZZZ999", Code), ] [order(mr_no, Code2)]
diag2 <- diag2 [, description := ifelse(Code2 == "ZZZ999", "** Uncoded", description), ]

setkeyv (diag2, c("mr_no", "Code2", "description", "studyday"))
diag3 <- unique(diag2)

diag4 <- diag3[, `:=`(primarycode = Code2,
                      primarydesc = description,
                      studyday2 = studyday),]

diag3 <- diag3[, c(1, 3, 4, 5), with =FALSE]
diag4 <- diag4[, c(1, 4, 6, 7, 8), with =FALSE]

# perform the join
prim_diag <- merge(diag3,
                   diag4, 
                   by = c("mr_no", "studyday"),
                   all=TRUE, 
                   allow.cartesian = TRUE)
prim_diag <- prim_diag [ studyday >= studyday2]

#fwrite(prim_diag, "D:/Hospital_data/ProgresSQL/analysis/086_fisher.csv")

analysis <- prim_diag [, .(cntcomb =uniqueN(mr_no)), 
                       by =.(primarycode, primarydesc, Code2, description)]

analysis02 <- prim_diag [, .(primcnt =uniqueN(mr_no)), 
                         by =.(primarycode, primarydesc)]

analysis03 <- prim_diag [, .(seccnt =uniqueN(mr_no)), 
                         by =.(Code2, description)]

analysis04 <- merge ( x = analysis,
                      y = analysis02,
                      by = c("primarycode", "primarydesc"))

analysis05 <- merge ( x = analysis04,
                      y = analysis03,
                      by = c("Code2", "description"))
analysis05 <- analysis05 [, nrow := .I, ]

actgrps <- unique( analysis05 [, c("Code2", "primarycode", "nrow"),])
actgrps <- actgrps [, new := paste( 'ddd', trimws(nrow),'<- prim_diag[ !Code2 %in% c("', trimws(Code2), '","', trimws(primarycode), '"), .(cnt = uniqueN(mr_no), Code2 ="', trimws(Code2), '", primarycode ="', trimws(primarycode), '",nrow =', trimws(nrow),'),]', sep=""), ]

fwrite(actgrps [, c("new"),], "D:/Hospital_data/ProgresSQL/prgm/prim_diag02.txt", 
       col.names = FALSE,
       row.names = FALSE, 
       quote = FALSE)

source("D:/Hospital_data/ProgresSQL/prgm/prim_diag02.txt")

all <- rbindlist(mget(ls(pattern = "ddd*")))
rm(list = ls( pattern='ddd*'))

analysis06 <- merge(x = analysis05, 
                    y = all,
                    by = c("Code2", "primarycode", "nrow"))

analysis06 <- analysis06[, `:=`(pval = fisher.test(matrix(c(primcnt, seccnt, cntcomb, cnt), ncol=2), workspace=1e9)$p.value,
                                pvalchi = chisq.test(matrix(c(primcnt, seccnt, cntcomb, cnt), ncol=2))$p.value),
                         by= nrow]

analysis07 <- analysis06 [ Code2 != primarycode]
analysis07 <- analysis07 [, `:=` (sig = ifelse ( pval < 0.05/ nrow(analysis07), 1, 0),
                                  sigchi = ifelse ( pvalchi < 0.05/ nrow(analysis07), 1, 0) ),]
#=====================================================================================================
# End of program
#=====================================================================================================

library(data.table)
library(tidyverse)

all_met_rmsd <- readRDS("D:/Hospital_data/ProgresSQL/analysis/01adsl_met_rmsd.rds")

# Code ** NOT YET CODED for the missing code values
# Sort the data by patient and day

diag <- unique( all_met_rmsd [, c("mr_no", "Code", "description", "studyday"), with =FALSE] )

diag2 <- diag[, Code2 := ifelse(Code =="", "ZZZ999", Code), ] [order(mr_no, Code2)]
diag2 <- diag2 [, description := ifelse(Code2 == "ZZZ999", "** Uncoded", description), ]

setkeyv (diag2, c("mr_no", "Code2", "description", "studyday"))
diag3 <- unique(diag2)

diag4 <- diag3[, `:=`(primarycode = Code2,
                      primarydesc = description,
                      studyday2 = studyday),]

diag3 <- diag3[, c(1, 3, 4, 5), with =FALSE]
diag4 <- diag4[, c(1, 4, 6, 7, 8), with =FALSE]

# perform the join
prim_diag <- merge(diag3,
                   diag4, 
                   by = c("mr_no", "studyday"),
                   all=TRUE, 
                   allow.cartesian = TRUE)
prim_diag <- prim_diag [ studyday >= studyday2]

fwrite(prim_diag, "D:/Hospital_data/ProgresSQL/analysis/086_fisher.csv")


prim_diag <- fread("C:\\Users\\mahajvi1\\Downloads\\prim_diag.txt", sep = ",")


analysis <- prim_diag [, .(cntcomb =uniqueN(mr_no)), 
                       by =.(primarycode, primarydesc, Code2, description)]

analysis02 <- prim_diag [, .(primcnt =uniqueN(mr_no)), 
                         by =.(primarycode, primarydesc)]

analysis03 <- prim_diag [, .(seccnt =uniqueN(mr_no)), 
                         by =.(Code2, description)]


analysis04 <- merge ( x = analysis,
                      y = analysis02,
                      by = c("primarycode", "primarydesc"))

analysis05 <- merge ( x = analysis04,
                      y = analysis03,
                      by = c("Code2", "description"))
analysis05 <- analysis05 [, nrow := .I, ]

actgrps <- unique( analysis05 [, c("Code2", "primarycode", "nrow"),])
actgrps <- actgrps [, new := paste( 'ddd', trimws(nrow),'<- prim_diag[ Code2 %in% c("', trimws(Code2), '","', trimws(primarycode), '"), .(cnt = uniqueN(mr_no), Code2 ="', trimws(Code2), '", primarycode ="', trimws(primarycode), '",nrow =', trimws(nrow),'),]', sep=""), ]

fwrite(actgrps [, c("new"),], "C:\\Users\\mahajvi1\\Downloads\\prim_diag02.txt", 
       col.names = FALSE,
       row.names = FALSE, 
       quote = FALSE)

source("C:\\Users\\mahajvi1\\Downloads\\prim_diag02.txt")
all <- rbindlist(mget(ls(pattern = "ddd*")))
rm(list = ls( pattern='ddd*'))

analysis06 <- merge(x = analysis05, 
                    y = all,
                    by = c("Code2", "primarycode", "nrow"))

#=====================================================================================================
# End of program
#=====================================================================================================


library(data.table)
library(dplyr)
library(tidyverse)

all_met_rmsd <- readRDS("D:/Hospital_data/ProgresSQL/analysis/01adsl_met_rmsd.rds")

# Code ** NOT YET CODED for the missing code values
# Sort the data by patient and day

diag <- unique( all_met_rmsd [, c("mr_no", "Code", "description", "studyday"), with =FALSE] )

diag2 <- diag[, Code2 := ifelse(Code =="", "ZZZ999", Code), ] [order(mr_no, Code2)]
diag2 <- diag2 [, description := ifelse(Code2 == "ZZZ999", "** Uncoded", description), ]

setkeyv (diag2, c("mr_no", "Code2", "description", "studyday"))
diag3 <- unique(diag2)

diag4 <- diag3[, `:=`(primarycode = Code2,
                      primarydesc = description,
                      studyday2 = studyday),]

diag3 <- diag3[, c(1, 3, 4, 5), with =FALSE]
diag4 <- diag4[, c(1, 4, 6, 7, 8), with =FALSE]

# perform the join
prim_diag <- merge(diag3,
                   diag4, 
                   by = c("mr_no", "studyday"),
                   all=TRUE, 
                   allow.cartesian = TRUE)
prim_diag <- prim_diag [ studyday >= studyday2]

fwrite(prim_diag, "D:/Hospital_data/ProgresSQL/analysis/086_fisher.csv")

a2 <- prim_diag [ primarycode == "A2.0"]
a2 <- a2 [, yesno :=1,]

# unique combinations
unqpat <- unique( prim_diag [primarycode == "A2.0", c("mr_no"), ])
unqprim <- unique( prim_diag [primarycode == "A2.0" , c("primarycode", "primarydesc"), ])
unqdis <- unique( prim_diag [primarycode == "A2.0" , c("Code2", "description"), ])

unqall <- data.table(crossing (unqpat, unqprim, unqdis))

a2_1 <- merge( x = unqall,
               y = a2 [, -c("studyday", "studyday2")],
               by = c("mr_no", "primarycode", "primarydesc", "Code2", "description"),
               all = TRUE)
a2_1[is.na(a2_1)] <- 0
analysis <- a2_1 [, .(cnt =uniqueN(mr_no)), 
                  by =.(primarycode, primarydesc, Code2, description, yesno)]

unqyesno <- unique ( a2_1 [, c("yesno"), ])
unqana <- crossing (unqprim, unqdis, unqyesno)

a2_2 <- merge( x = analysis,
               y = unqana,
               by =  c("primarycode", "primarydesc", "Code2", "description", "yesno"),
               all = TRUE)

analysis <- prim_diag [, .(cnt =uniqueN(mr_no)), by =.(primarycode, primarydesc, Code2, description)] [order(primarycode, primarydesc, -cnt)]


dt[, p.val := fisher.test(matrix(c(pop.count.1, pop.count.2, DB.count.1, DB.count.2), ncol=2), workspace=1e9)$p.value, by=Variant]

df <- as.data.frame(dt)


analysis_t <- dcast(data = analysis, 
                    primarycode + primarydesc ~ Code2, 
                    value.var = c("cnt"),
                    fill =0)

analysis_t <- analysis_t[, -c("primarycode", "primarydesc", "ZZZ999")]
