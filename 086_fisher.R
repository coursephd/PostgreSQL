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
