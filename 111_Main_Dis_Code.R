library(data.table)
library(tidyverse)
library(cumstats)
library(visNetwork)
library(igraph)

#########################################
# For the complete set of 50,000 patients
#########################################

adsl <- fread("D:/Hospital_data/ProgresSQL/analysis/01adsl.csv")
adsl <- adsl [, Code := icd_code,]
adsl <- adsl [, distype := "Dis",]
adsl <- adsl [, Type_med := "Med",]
all_met_rmsd02 <- adsl [, Coded_med := medicine_name,]

########################################
# For the subset of Metabolic and RMSD
########################################

#all_met_rmsd02 <- fread("C://Users//mahajvi1//Downloads//01adsl_met_rmsd.csv")
#all_met_rmsd02 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/01adsl_met_rmsd.rds")
all_met_rmsd02 <- all_met_rmsd02 [, Code := ifelse (Code == " " | Code == "", "** Not yet coded", Code),]
all_met_rmsd02 <- all_met_rmsd02 [, description:= ifelse (description == "" | description ==" ", "** Not yet coded", description),]
all_met_rmsd02 <- all_met_rmsd02 [, Code02 := paste(distype, ":", Code, ":", description, sep =""), ]
all_met_rmsd02 <- all_met_rmsd02 [, Med02 := paste(Type_med, ":", Coded_med, sep =""), ]

code <- unique (all_met_rmsd02 [, c("Code02", "Code", "description"),])
code <- code [, pos := str_locate(Code, "\\."),]
code <- code [, `:=` (main = substr(Code, 1, pos-1),
                      second = substr(Code, pos+1, length(Code) ) ),]

code02 <- code [ second == "0", c("main", "description"),]
#setnames(code02, "main", "maincode")
setnames(code02, "description", "maindesc")

code03 <- merge (x = code, 
                 y = code02, 
                 by = c("main") )


all_met_rmsd03 <- merge(x = all_met_rmsd02,
                        y = code03 [, c("Code02", "main", "maindesc"), ],
                        by = c("Code02") )

all_met_rmsd03 <- all_met_rmsd03 [, main02 := paste(distype, ":", main, ":", maindesc, sep =""), ]

diag3 <- unique( all_met_rmsd03[, c("mr_no", "main02", "studyday"), ] ) 

diag4 <- diag3 [, `:=`(primarycode = main02,
                       primaryday = studyday), ]

diag3 <- diag3 [, c("mr_no", "main02", "studyday"), ]
diag4 <- diag4 [, -c("main02", "studyday"), ]

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
prim_diag02 <- prim_diag02 [, diff := as.numeric(studyday - primaryday),]

links <- prim_diag02 [, .(n = uniqueN(mr_no),
                          mean = mean( diff, na.rm = FALSE),
                          sd = sd(diff, na.rm = FALSE),
                          median = median (diff, na.rm = FALSE),
                          min = min (diff, na.rm = FALSE),
                          max = max (diff, na.rm = FALSE)), by = .(primarycode, main02)]

fwrite(links, "D:/Hospital_data/ProgresSQL/analysis/111_cytoscape_dis.csv")

# Create edges
edges <- links [, `:=`(from = primarycode,
                       to = main02, 
                       value = n), ]

# Create nodes

nodes <- prim_diag02 [, .(value = uniqueN(mr_no)), by = .(primarycode)]
nodes <- nodes [ order(primarycode)]
nodes <- nodes [, `:=` (id = primarycode, #1:.N,
                       group = primarycode), ]

network <- visNetwork(nodes, edges, height = "100%", width = "100%") %>%
  
  visOptions(selectedBy = "group", 
             highlightNearest = list(enabled = TRUE, hover = TRUE), 
             nodesIdSelection = TRUE) %>%
  visPhysics(stabilization = FALSE) %>%
  
  visCollapse( fit = FALSE, resetHighlight = TRUE,
              clusterOptions = NULL, labelSuffix = "(cluster)")

network %>% visSave(file = "C://Users//mahajvi1//Downloads//network.html", background = "black")

visInteraction(navigationButtons = TRUE)%>%
  
  
network02 <-   
  visNetwork(nodes, edges, height = "100%", width = "100%") %>% 
  visEdges(arrows = "from") %>% 
  visHierarchicalLayout() # same as   visLayout(hierarchical = TRUE)   
  
network02 %>% visSave(file = "C://Users//mahajvi1//Downloads//network02.html", background = "black")