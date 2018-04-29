
library(data.table)
library(zoo)
library(stringi)
library(stringr)
library(openxlsx)
library(anytime)
library(statnet)
library(igraph)
library(linkcomm)
library(TraMineR)


#https://r2013-lyon.sciencesconf.org/file/42147

data01 <- unique(all_met_rmsd [Code !="", c("mr_no", "Code", "studyday", "age", "patient_gender"), ]) 
data01<- data01 [order(mr_no, studyday, age,Code)]
data01 <- data01 [, `:=`(subday =1:.GRP), by= .(mr_no, studyday, age)]

data02 <- dcast(data01, 
                mr_no + age + patient_gender + subday ~ paste("day", studyday, sep=""),
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
