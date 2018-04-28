
library(data.table)
library(statnet)
library(igraph)
library(linkcomm)

all_met_rmsd <- readRDS("D:/Hospital_data/ProgresSQL/analysis/01adsl_met_rmsd.rds")

######################################################################################
# Merge gender and season variables so that there is less programming update necessary
######################################################################################
all_met_rmsd <- all_met_rmsd [, patient_gender := paste(patient_gender, season)]

# Code ** NOT YET CODED for the missing code values
# Sort the data by patient and day

diag <- unique( all_met_rmsd [Code != "" & patient_gender != "", 
                              c("mr_no", "Code", "description", "patient_gender"), with =FALSE] )

diag2 <- diag[, Code2 := ifelse(Code =="", "ZZZ999", Code), ] [order(mr_no, Code2)]
diag2 <- diag2 [, description := ifelse(Code2 == "ZZZ999", "** Uncoded", description), ]

setkeyv (diag2, c("mr_no", "Code2", "description", "patient_gender"))
diag3 <- unique(diag2)

diag4 <- diag3[, `:=`(primarycode = Code2,
                      primarydesc = description, 
                      primarygender = patient_gender),]

diag3 <- diag3[, c("mr_no", "description", "Code2", "patient_gender"), with =FALSE]
diag4 <- diag4[, c("mr_no", "primarycode", "primarydesc", "primarygender"), with =FALSE]

# set the ON clause as keys of the tables:
setkey(diag3,mr_no, patient_gender)
setkey(diag4,mr_no, primarygender)

# perform the join
prim_diag <- merge(diag3,diag4, all=TRUE, allow.cartesian = TRUE)

combs <- unique( prim_diag[, c("primarycode", "primarydesc", "Code2", "description", "patient_gender")] ) 
combs <- combs [order(primarycode, patient_gender,Code2)]
combs2 <- combs[, .(discomb = paste("'", trimws(Code2), "'", collapse=",", sep=""), 
                    rownum = .GRP),
                by = .(primarycode, primarydesc, patient_gender)]

combs2 <- combs2 [, `:=`(disnum = .GRP, dummy=1, grpnum = 1:.N, maxnum = max(.N),
                         discomb2 = paste("primarycode %in% c(", discomb, ") & Code2 %in% c(", discomb, ") & patient_gender %in% c('", trimws(patient_gender), "')", sep="")), 
                  by = .(primarycode, primarydesc)] 

subanalysis <- prim_diag [,
                          .(cnt =uniqueN(mr_no)), 
                          by =.(primarycode, primarydesc, Code2, description, patient_gender, primarygender)] 


unqdis  <- unique(subanalysis$primarycode)
unqdis <- "V2.63"

for (disease in unqdis ) {
  
  
  pdf( paste("D:/Hospital_data/ProgresSQL/reports/network_GenderSeason/", disease, "_season.pdf", sep="" ), # name of pdf (need to include .pdf)
       width = 20, # width of resulting pdf in inches
       height = 20 # height of resulting pdf in inches
  ) 
  
  print(disease)
  runnum <- combs2[combs2$primarycode == disease]
  
  for (ii in 1:nrow(runnum)) {
    
    print (ii) 
    tmp <- runnum [ grpnum == ii]  
    dis <- subanalysis [primarycode == tmp$primarycode & patient_gender == tmp$patient_gender] 
    
    sub2 <- subanalysis [ (primarycode %in% dis$primarycode & Code2 %in% dis$Code2 & patient_gender %in% dis$patient_gender & primarygender %in% dis$patient_gender) |
                            (Code2 %in% dis$primarycode & primarycode %in% dis$Code2 & patient_gender %in% dis$patient_gender & primarygender %in% dis$patient_gender)  
                          ]
    
    analysis_t <-  dcast(sub2, 
                         primarycode ~ Code2,
                         value.var = c("cnt"),
                         fill =0  ) [, -c("primarycode")]
    
    analysis_t02 <- data.matrix(analysis_t)
    
    inet <- graph_from_adjacency_matrix(analysis_t02)
    inet <- simplify(inet, remove.multiple = F, remove.loops = T)
    
    # Count the number of degree for each node:
    deg=degree(inet, mode="all")
    
    #par(mfrow=c(1,1))
    
    plot(inet, 
         edge.arrow.size=.4, 
         edge.curved=.1, 
         layout = layout.kamada.kawai, #layout.fruchterman.reingold, #layout.graphopt, 
         #vertex.size=deg * 0.1,
         main = paste("Disease:", tmp$primarycode, "Description:", tmp$primarydesc, "Gender:", tmp$patient_gender))
    
  }
  dev.off() # finishes plotting and finalizes pdf
  
}


library(cooccur)
cooccur_dis <- cooccur(mat = analysis_t, type = "spp_site", thresh = TRUE, spp_names = TRUE)
summary(cooccur_dis)

cooccur_dis$results

plot(cooccur_dis,type="cooc")

obs.v.exp(cooccur_dis)
