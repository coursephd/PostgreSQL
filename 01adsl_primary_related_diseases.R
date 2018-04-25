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

library(statnet)
library(igraph)

analysis <- prim_diag [, .(cnt =uniqueN(mr_no)), by =.(primarycode, primarydesc, Code2, description)] [order(primarycode, primarydesc, -cnt)]
analysis <- analysis [, noofdis := .N -1, by =.(primarycode)]
ana <- copy(analysis)

combs <- unique( prim_diag[, c("primarycode", "primarydesc", "Code2", "description")] ) [order(primarycode, Code2)]
combs2 <- combs[, .(discomb = paste("'", trimws(Code2), "'", collapse=",", sep="") ),
                by = .(primarycode)]

subanalysis <- prim_diag [primarycode %in% c('A1.0','A16.0','A2.0','A2.1','A5.0','A6.0','A7.0','A7A.0','J1.0','J1.2','K2.0','K2.5','K7.0','K9.0','M10.1','M2.0','MS R1','MS T1.0','N2.18','N2.21.0','N2.21.1','N4.19','O1.2','P5.0','S14.0','S14.18','S15.42','S16.0','S16.2','S18.0','S2.0','S3.0','V2.0','V2.10','V2.23','V2.36','V2.63','V2.75','V9.0','Y1'), 
                          .(cnt =uniqueN(mr_no)), 
                          by =.(primarycode, primarydesc, Code2, description)] 
[order(primarycode, primarydesc, -cnt)]

analysis_t <-  dcast(subanalysis, 
                     primarycode ~ Code2,
                     value.var = c("cnt"),
                     subset = .(Code2 %in% c('A1.0','A2.0','V2.0','V2.63','J1.0','K2.0','V2.23','S16.0','A7.0','A6.0','N2.21.0','S14.0','A7A.0','K2.5','O1.2','S3.0','S15.42','A16.0','K7.0','N4.19','Y1','N2.21.1','S18.0','S2.0','N2.18','A5.0','M10.1','J1.2','P5.0','V9.0','MS T1.0','S14.18','A2.1','K9.0','M2.0','MS R1','V2.36','S16.2','V2.75','V2.10') | primarycode == "A1.0"),
                     fill =0  ) [, -c("primarycode")]

analysis_t02 <- data.matrix(analysis_t)

net <- as.network(x = analysis_t02, # the network object
                  directed = TRUE, # specify whether the network is directed
                  loops = FALSE, # do we allow self ties (should not allow them)
                  matrix.type = "adjacency" # the type of input
)

pdf("D:/Hospital_data/ProgresSQL/analysis/Network_Plot_1.pdf", # name of pdf (need to include .pdf)
    width = 10, # width of resulting pdf in inches
    height = 10 # height of resulting pdf in inches
) 

inet <- graph_from_adjacency_matrix(analysis_t02)
inet <- simplify(inet, remove.multiple = F, remove.loops = T)
plot(inet, 
     edge.arrow.size=.4, 
     edge.curved=.1)
dev.off() # finishes plotting and finalizes pdf

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
