library(data.table)

all_met_rmsd <- readRDS("D:/Hospital_data/ProgresSQL/analysis/01adsl_met_rmsd.rds")

# Code ** NOT YET CODED for the missing code values
# Sort the data by patient and day

diag <- unique( all_met_rmsd [, c("mr_no", "Code", "description"), with =FALSE] )

diag2 <- diag[, Code2 := ifelse(Code =="", "ZZZ999", Code), ] [order(mr_no, Code2)]
diag2 <- diag2 [, description := ifelse(Code2 == "ZZZ999", "** Uncoded", description), ]

setkeyv (diag2, c("mr_no", "Code2", "description"))
diag3 <- unique(diag2)

diag4 <- diag3[, `:=`(primarycode = Code2,
                      primarydesc = description),]

diag3 <- diag3[, c(1, 3, 4), with =FALSE]
diag4 <- diag4[, c(1, 5, 6), with =FALSE]

# set the ON clause as keys of the tables:
setkey(diag3,mr_no)
setkey(diag4,mr_no)

# perform the join
prim_diag <- merge(diag3,diag4, all=TRUE, allow.cartesian = TRUE)

analysis <- prim_diag [, .(cnt =uniqueN(mr_no)), by =.(primarycode, primarydesc, Code2, description)] [order(primarycode, primarydesc, -cnt)]

analysis_t <- dcast(data = analysis, 
                    primarycode + primarydesc ~ Code2, 
                    value.var = c("cnt"),
                    fill =0)
analysis_t <- analysis_t[, -c("primarycode", "primarydesc", "ZZZ999")]

analysis_t02 <- data.matrix(analysis_t)

library(cooccur)

dd <- cooccur(analysis_t02)
saveRDS (dd, "D:/Hospital_data/ProgresSQL/analysis/dd.rds")

ddco <- readRDS("D:/Hospital_data/ProgresSQL/analysis/dd.rds")

summary(dd)
effect.sizes(dd)
obs.v.exp(dd)
probtbl <- prob.table(dd)

plot(dd)
pair(dd,"P5.0",all=TRUE)

