
library(data.table)
library(zoo)
library(stringi)
library(stringr)
#library(openxlsx)
library(anytime)
library(statnet)
library(igraph)
library(linkcomm)
library(TraMineR)


#https://r2013-lyon.sciencesconf.org/file/42147

all_met_rmsd <- readRDS("D:/Hospital_data/ProgresSQL/analysis/01adsl_met_rmsd.rds")

diag2 <- all_met_rmsd [Code2 !="", Code2 := ifelse(Code =="", "ZZZ999", Code), ] [order(mr_no, Code2)]
diag2 <- diag2 [, description := ifelse(Code2 == "ZZZ999", "** Uncoded", description), ]

data01 <- unique(diag2 [, c("mr_no", "Code2", "description", "studyday", "age", "all_vis", "vis" ,"patient_gender", "distype"), ]) 
data01<- data01 [order(mr_no, studyday, vis, distype, age, Code2, description)]
data01 <- data01 [, `:=`(subday =1:.GRP), by= .(mr_no, studyday, vis, distype, age)]

data02 <- dcast(data01, 
                mr_no + age + patient_gender + distype + subday ~ paste("day", studyday, sep=""),
                value.var = c("Code"),
                subset = .(studyday <= 500), 
                fill =" ")

unqdis  <- unique(data01$Code)

dd <- seqstatl(data02, 5:500)

seq <- seqdef (data = as.matrix (data02 ),
               5:500,
               labels = dd, 
               states = dd, 
               alphabet = dd)

seqfplot(seq)
seqlength(seq)
seqtransn(seq)


###################
# Identify patients
###################
all_met_rmsd <- readRDS("D:/Hospital_data/ProgresSQL/analysis/01adsl_met_rmsd.rds")

cnt<- unique( all_met_rmsd [patient_gender != "" & Code != "", c("mr_no", "Code"), ])
cnt <- cnt [, val:=1]
pattype <- dcast(cnt, 
                 mr_no ~ Code, 
                 value.var = c("val"),
                 fill =0)

diag2 <- all_met_rmsd [Code2 != "", Code2 := ifelse(Code =="", "ZZZ999", Code), ] [order(mr_no, Code2)]
diag2 <- diag2 [, description := ifelse(Code2 == "ZZZ999", "** Uncoded", description), ]

# Subset only for S16.0

data01 <- unique(diag2 [ mr_no %in% subset(pattype, S16.0 == 1)$mr_no , c("mr_no", "Code2", "description", "studyday", "age", "all_vis", "vis" ,"patient_gender", "distype"), ]) 
data01<- data01 [order(mr_no, studyday, vis, distype, age, Code2, description)]
data01 <- data01 [, `:=`(subday =1:.GRP), by= .(mr_no, studyday, vis, distype, age)]

tmp <- data01 [1:200,]  
tmp.seqe <- seqecreate(id = factor(tmp$mr_no), 
                       timestamp = tmp$studyday, 
                       event = tmp$Code2) 

fsubseq <- seqefsub(tmp.seqe, pMinSupport = 0.05)
plot(fsubseq[1:15], col = "cyan")

discr <- seqecmpgroup(fsubseq, group = cl1.4fac)

data(actcal.tse) 
actcal.seqe <- seqecreate(id = actcal.tse$id, timestamp = actcal.tse$time, event = actcal.tse$event) 
summary(actcal.seqe)

seq <- seqdef(data =data01, 
              informat = "TSE" )

