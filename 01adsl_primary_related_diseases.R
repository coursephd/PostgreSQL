####################################
# Code to generate DIAGNOSIS data
# Main disease and related diseases 
####################################

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
analysis <- analysis [, noofdis := .N -1, by =.(primarycode)]

analysis02 <- unique( analysis[, c("primarycode", "noofdis"), with =FALSE])

################################################
# Do similar calculations for the medicines
################################################

diag <- unique( all_met_rmsd [ medicine_name != "", c("mr_no", "medicine_name"), with =FALSE] )

setkeyv (diag, c("mr_no", "medicine_name"))
diag3 <- unique(diag)

diag4 <- diag3[, `:=`(primarymed = medicine_name)]

diag3 <- diag3[, c(1, 2), with =FALSE]
diag4 <- diag4[, c(1, 3), with =FALSE]

# set the ON clause as keys of the tables:
setkey(diag3,mr_no)
setkey(diag4,mr_no)

# perform the join
prim_med <- merge(diag3,diag4, all=TRUE, allow.cartesian = TRUE)

analysis_med <- prim_med [, .(UnqPat =uniqueN(mr_no)), by =.(primarymed, medicine_name)] [order(primarymed, medicine_name, -UnqPat)]
analysis_med <- analysis_med [, noofmed := .N -1, by =.(primarymed)]
analysis_med <- analysis_med [order(-noofmed, -UnqPat)]
analysis_med02 <- unique( analysis_med[, c("primarymed", "noofmed"), with =FALSE])[order (-noofmed)]
