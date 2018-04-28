
library(data.table)
library(statnet)
library(igraph)
library(linkcomm)

all_met_rmsd <- readRDS("D:/Hospital_data/ProgresSQL/analysis/01adsl_met_rmsd.rds")

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


unqdis  <- unique(subanalysis$primarycode) [1:10]

for (disease in unqdis ) {

  par(mfrow=c(1,2))
  
  pdf( paste("D:/Hospital_data/ProgresSQL/reports/", disease, ".pdf", sep="" ), # name of pdf (need to include .pdf)
       width = 20, # width of resulting pdf in inches
       height = 20 # height of resulting pdf in inches
  ) 
  
  print(disease)
  runnum <- combs2[combs2$primarycode == disease]
  
  for (ii in 1:nrow(runnum)) {

  print (ii) 
  tmp <- runnum [ grpnum == ii]  
  dis <- subanalysis [primarycode == tmp$primarycode & patient_gender == tmp$patient_gender] 

  sub2 <- subanalysis [ (primarycode %in% dis$primarycode & Code2 %in% dis$Code2 & patient_gender %in% dis$patient_gender) |
                        (Code2 %in% dis$primarycode & primarycode %in% dis$Code2 & patient_gender %in% dis$patient_gender)  
                        ]
  
  analysis_t <-  dcast(sub2, 
                       primarycode ~ Code2,
                       value.var = c("cnt"),
                       fill =0  ) [, -c("primarycode")]
  
  analysis_t02 <- data.matrix(analysis_t)
  
  inet <- graph_from_adjacency_matrix(analysis_t02)
  inet <- simplify(inet, remove.multiple = F, remove.loops = T)
  
  plot(inet, 
       edge.arrow.size=.4, 
       edge.curved=.1, 
       layout = layout.graphopt, 
       
       main = paste("Disease:", tmp$primarycode, "Description:", tmp$primarydesc, "Gender:", tmp$patient_gender))
  
}
  dev.off() # finishes plotting and finalizes pdf
  
}


########################################################################################
# End of program
########################################################################################

subanalysis <- prim_diag [primarycode %in% c('A1.0','A11.0','A11.18','A11.3','A12.0','A12.10','A12.11','A12.4','A13.0','A13.3','A15.0','A16.0','A2.0','A3.0','A5.0','A6.0','A6.1','A7.0','A7A.0','A8.0','A8.4','A9.0','A9.4','Anidra','B3.0','B5','D1.0','G1.6','G6.0','G8.0','G9.0','G9.4','G9.7','H2.0','H2.3','J1.0','J1.6','K10.11','K10.15','K10.23','K10.29','K10.38A','K10.52.0','K10.52.8','K2.0','K3.0','K4.10','K4.11','K4.19','K4.21','K4.6','K7a.0','K9.0','M10.0','M10.1','M10.2','M13.0','M16.0','M16.2','M18.11.0','M18.11.2','M18.11.4','M18.9.0','M18.9.1','M18.9.2','M18.9.3','M2.0','M5.1','M5.3','M6.38','M6.43','M6.8','MS G1.0','MS M1.0','MS R1','MS T1.0','N2.1','N2.16','N2.21.0','N2.21.2','N2.21.3','N2.21.7','N4.0','N4.10','N4.12','N4.13','N4.35','N4.40.0','N4.40.14','N4.40.15','N4.40.20.0','N4.40.20.1','N4.46.2.3','N4.47.7','N4.50.2.1','N4.50.9','O1.4','P2.0','P5.0','P5.1','P5.3','P5.4','P5.5','S0.0','S1.0','S13.11','S13.13','S13.19','S13.7','S14.0','S14.16','S14.18','S14.21','S14.4','S14.5','S14.7','S15.47','S16.0','S16.1','S16.10','S16.12','S16.20.0','S16.20.8','S16.21.0','S16.22','S16.29.1','S16.30.10','S16.30.12','S16.30.13','S16.30.22','S16.30.27','S17.0','S17.1','S18.0','S18.1','S2.10','S2.14','S2.16.1','S2.19.1','S2.7','S3.0','S3.3','S4.0','U1.0','U2.4','U4.0','V.10','V1.0','V1.2','V10.29','V11.6','V11.7','V13.0','V13.3','V16.0','V2.0','V2.10','V2.15','V2.2','V2.23','V2.31','V2.34.0','V2.36','V2.4.1','V2.42','V2.44','V2.51','V2.53','V2.56.0','V2.63','V2.65','V2.68','V2.72','V2.73','V2.75','V2.77','V3.1','V3.2','V3.3','V5.0','V5.1','V6.0','V6.17','Y1','Y8.10','Y8.13','Y8.22','k12.0','k12.16','k12.2','k12.5','k12.9') &
                          Code2 %in% c('A1.0','A11.0','A11.18','A11.3','A12.0','A12.10','A12.11','A12.4','A13.0','A13.3','A15.0','A16.0','A2.0','A3.0','A5.0','A6.0','A6.1','A7.0','A7A.0','A8.0','A8.4','A9.0','A9.4','Anidra','B3.0','B5','D1.0','G1.6','G6.0','G8.0','G9.0','G9.4','G9.7','H2.0','H2.3','J1.0','J1.6','K10.11','K10.15','K10.23','K10.29','K10.38A','K10.52.0','K10.52.8','K2.0','K3.0','K4.10','K4.11','K4.19','K4.21','K4.6','K7a.0','K9.0','M10.0','M10.1','M10.2','M13.0','M16.0','M16.2','M18.11.0','M18.11.2','M18.11.4','M18.9.0','M18.9.1','M18.9.2','M18.9.3','M2.0','M5.1','M5.3','M6.38','M6.43','M6.8','MS G1.0','MS M1.0','MS R1','MS T1.0','N2.1','N2.16','N2.21.0','N2.21.2','N2.21.3','N2.21.7','N4.0','N4.10','N4.12','N4.13','N4.35','N4.40.0','N4.40.14','N4.40.15','N4.40.20.0','N4.40.20.1','N4.46.2.3','N4.47.7','N4.50.2.1','N4.50.9','O1.4','P2.0','P5.0','P5.1','P5.3','P5.4','P5.5','S0.0','S1.0','S13.11','S13.13','S13.19','S13.7','S14.0','S14.16','S14.18','S14.21','S14.4','S14.5','S14.7','S15.47','S16.0','S16.1','S16.10','S16.12','S16.20.0','S16.20.8','S16.21.0','S16.22','S16.29.1','S16.30.10','S16.30.12','S16.30.13','S16.30.22','S16.30.27','S17.0','S17.1','S18.0','S18.1','S2.10','S2.14','S2.16.1','S2.19.1','S2.7','S3.0','S3.3','S4.0','U1.0','U2.4','U4.0','V.10','V1.0','V1.2','V10.29','V11.6','V11.7','V13.0','V13.3','V16.0','V2.0','V2.10','V2.15','V2.2','V2.23','V2.31','V2.34.0','V2.36','V2.4.1','V2.42','V2.44','V2.51','V2.53','V2.56.0','V2.63','V2.65','V2.68','V2.72','V2.73','V2.75','V2.77','V3.1','V3.2','V3.3','V5.0','V5.1','V6.0','V6.17','Y1','Y8.10','Y8.13','Y8.22','k12.0','k12.16','k12.2','k12.5','k12.9') &
                          patient_gender == "F" & primarygender == "F",
                          .(cnt =uniqueN(mr_no)), 
                          by =.(primarycode, primarydesc, Code2, description, patient_gender, primarygender)] 

analysis_t <-  dcast(subanalysis, 
                     primarycode ~ Code2,
                     value.var = c("cnt"),
                     fill =0  ) [, -c("primarycode")]

analysis_t02 <- data.matrix(analysis_t)

inet <- graph_from_adjacency_matrix(analysis_t02)
inet <- simplify(inet, remove.multiple = F, remove.loops = T)

mc_edge = as_edgelist(inet)
mc_edge = data.frame(node1 = mc_edge[,1], node2 = mc_edge[,2], weight = 10) #E(inet)$weight)
g_linkcomm = getLinkCommunities(mc_edge, directed = FALSE, plot = FALSE)

plotLinkCommDend(g_linkcomm)
layout.spencer.circle(g_linkcomm)
plotLinkCommMembers(g_linkcomm)
plotLinkCommSummComm(g_linkcomm)
plotLinkCommSumm(g_linkcomm)

plotOCGraph(g_linkcomm)

f<- plot(inet, 
     edge.arrow.size=.4, 
     edge.curved=.1, 
     layout = layout.graphopt)


subanalysis <- prim_diag [primarycode %in% c('A11.0','A13.0','A13.2','A16.0','A2.0','A3.0','A4.0','A5.0','A6.0','A6.1','A6.2','A7.0','A7A.0','A8.0','A9.0','Anidra','B2.0','B3.0','B5','D0.0','D0.1','D0.8','D1.0','G10.0','G8.0','G8.5','G9.0','G9.1','G9.4','G9.7','G9.9','H2.0','H2.3','H2.5','J1.0','J1.6','K10.28','K10.29','K10.38','K10.38A','K2.0','K2.1','K2.5','K3.0','K4.10','K4.12','K4.13','K4.9','K6.0','K6.4','K7.0','K9.0','M1.0','M1.6','M10.0','M10.1','M10.2','M13.0','M14.13','M14.15','M14.5','M16.0','M18.0','M18.10.0','M18.10.10','M18.10.12','M18.11.0','M18.11.2','M18.11.4','M18.9.0','M18.9.1','M18.9.15','M18.9.2','M18.9.25','M18.9.3','M2.0','M5.0','M5.1','M6.0','M6.25','MS G1.0','MS M1.0','MS R1','MS T1.0','N2.1','N2.12','N2.18','N2.21.0','N2.21.1','N2.21.7','N2.9','N4.0','N4.10','N4.35','N4.40.0','N4.40.10','N4.40.19','N4.40.20.0','N4.40.20.1','N4.40.9','N4.45','N4.46.2.0','N4.47.6','N4.50.10','N4.50.2.0','N4.50.9','N4.55.31','N4.6','O1.4','P2.0','P5.0','P5.1','P5.3','P5.4','P5.5','R2.0','S0.0','S1.0','S10.4','S10.5','S13.1','S13.11','S13.13','S13.19','S14.0','S14.1','S14.16','S14.18','S14.2','S14.21','S14.4','S14.7','S15.31','S15.36','S15.42','S16.0','S17.0','S17.1','S18.0','S18.1','S1A.2','S2.10','S2.14','S2.15.2','S2.15.5','S2.16.1','S2.19.1','S3.0','S3.3','S3A.0','S4.0','S4.4','S5.0','U2.4','U3.0','V1.0','V1.5','V10.11','V11.7','V13.0','V13.1.0','V13.1.3','V13.3','V15.0','V16.0','V2.0','V2.10','V2.13','V2.2','V2.21','V2.23','V2.30','V2.34.0','V2.36','V2.56','V2.56.0','V2.57','V2.63','V2.75','V2.8','V5.0','V5.1','V6.0','V9.0','Y1','k12.0','k12.16','k12.5','k12.9','k8.0') &
                          Code2 %in% c('A11.0','A13.0','A13.2','A16.0','A2.0','A3.0','A4.0','A5.0','A6.0','A6.1','A6.2','A7.0','A7A.0','A8.0','A9.0','Anidra','B2.0','B3.0','B5','D0.0','D0.1','D0.8','D1.0','G10.0','G8.0','G8.5','G9.0','G9.1','G9.4','G9.7','G9.9','H2.0','H2.3','H2.5','J1.0','J1.6','K10.28','K10.29','K10.38','K10.38A','K2.0','K2.1','K2.5','K3.0','K4.10','K4.12','K4.13','K4.9','K6.0','K6.4','K7.0','K9.0','M1.0','M1.6','M10.0','M10.1','M10.2','M13.0','M14.13','M14.15','M14.5','M16.0','M18.0','M18.10.0','M18.10.10','M18.10.12','M18.11.0','M18.11.2','M18.11.4','M18.9.0','M18.9.1','M18.9.15','M18.9.2','M18.9.25','M18.9.3','M2.0','M5.0','M5.1','M6.0','M6.25','MS G1.0','MS M1.0','MS R1','MS T1.0','N2.1','N2.12','N2.18','N2.21.0','N2.21.1','N2.21.7','N2.9','N4.0','N4.10','N4.35','N4.40.0','N4.40.10','N4.40.19','N4.40.20.0','N4.40.20.1','N4.40.9','N4.45','N4.46.2.0','N4.47.6','N4.50.10','N4.50.2.0','N4.50.9','N4.55.31','N4.6','O1.4','P2.0','P5.0','P5.1','P5.3','P5.4','P5.5','R2.0','S0.0','S1.0','S10.4','S10.5','S13.1','S13.11','S13.13','S13.19','S14.0','S14.1','S14.16','S14.18','S14.2','S14.21','S14.4','S14.7','S15.31','S15.36','S15.42','S16.0','S17.0','S17.1','S18.0','S18.1','S1A.2','S2.10','S2.14','S2.15.2','S2.15.5','S2.16.1','S2.19.1','S3.0','S3.3','S3A.0','S4.0','S4.4','S5.0','U2.4','U3.0','V1.0','V1.5','V10.11','V11.7','V13.0','V13.1.0','V13.1.3','V13.3','V15.0','V16.0','V2.0','V2.10','V2.13','V2.2','V2.21','V2.23','V2.30','V2.34.0','V2.36','V2.56','V2.56.0','V2.57','V2.63','V2.75','V2.8','V5.0','V5.1','V6.0','V9.0','Y1','k12.0','k12.16','k12.5','k12.9','k8.0') &
                          patient_gender == "F" & primarygender == "F",
                          .(cnt =uniqueN(mr_no)), 
                          by =.(primarycode, primarydesc, Code2, description, patient_gender, primarygender)] 

analysis_t <-  dcast(subanalysis, 
                     primarycode ~ Code2,
                     value.var = c("cnt"),
                     fill =0  ) [, -c("primarycode")]

analysis_t02 <- data.matrix(analysis_t)

inet <- graph_from_adjacency_matrix(analysis_t02)
inet <- simplify(inet, remove.multiple = F, remove.loops = T)
m<- plot(inet, 
         edge.arrow.size=.4, 
         edge.curved=.1, 
         layout = layout_with_kk)




plot(f)
plot(m)


library(cooccur)

combs <- unique( prim_diag[, c("primarycode", "Code2")] ) 
combs <- combs [, `:=`(val=1),]
combs2 <- dcast( combs,
                 primarycode ~ Code2,
                 value.var = c("val"),
                 fill=0) 
combs2 <- combs2 [, -c("primarycode")]

co_occur <- crossprod( as.matrix( combs2))
diag(co_occur) <- 0
co_occur

row.names(combs2) <- combs2$catvar
combs3 <- combs2 [, -c("catvar")]

cooccur_dis <- cooccur(mat = co_occur, type = "spp_site", thresh = TRUE, spp_names = TRUE)

