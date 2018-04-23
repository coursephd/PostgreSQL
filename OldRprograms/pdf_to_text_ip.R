
library(pdftools)
library(data.table)
library(stringi)
library(zoo)

setwd ("C:\\Users\\Lucky\\Documents\\Hospital data\\01_31JUL2016\\source_pdf")
files <- list.files(pattern = "*_IP.pdf")

Lapply2 <- lapply(files, pdf_text)
names(Lapply2) <- files

for(i in files)

Lapply2[[i]]$Source <- i

# This creates List files and must be converted to a data.table
# for rest of the code to work

alldata <- data.table( rbindlist(Lapply2) )

alldata2 <- data.table (
            melt (data = alldata,
                  id.vars = "Source",
                  measure.vars = grep ("V", names(alldata), perl = TRUE ),
                  variable.factor = FALSE) )

alldata3 <- alldata2 [, (V1 = unlist ( strsplit(alldata2$value, fixed = TRUE, split = "\r\n") )), by = Source]

# reference data to create domains
ip_ref <- fread("..\\analysis\\ip_ref.csv")

# Create row variable for keeping the source sort order
iprec <- alldata3 [, row :=1:.N, by = Source]
alldata3 <- alldata3 [, V1 :=stri_trim(V1)]
alldata3 <- alldata3 [, V1 :=stri_replace_all_regex(V1, c(" :", " ,"), c(":", ",") )]

# Merge the 2 datasets on the basis of V1 variable
iprec2 <- merge(iprec, ip_ref, by="V1", all=TRUE) [order (Source, row)]

# Fill up the missing values of Name and Number
iprec2 <- iprec2 [, `:=`(Name  = na.locf(Name),
                         Number= na.locf(Number) ), by = Source ]

# Create number of rows for each of the variables to be used in tranpose
iprec2 <- iprec2 [, `:=`(subrow = 1:.N), by = .(Source, Name)]

# Transpose the data and create 1 variable per 1 domain
iprec2tran <- dcast (data = iprec2[, c("V1", "Name", "subrow", "Source"), with=FALSE],
                     Source + subrow ~ Name,
                     value.var = "V1")
iprec2tran[is.na(iprec2tran)]<- ""
iprec2tran <- iprec2tran [, subrow := ifelse (subrow>1, 99, subrow), ]

# Combine the data into 1 observation
iprec3tran <- iprec2tran [ , .(D1 = paste(D1, collapse=","),
                               D2 = paste(D2, collapse=","),
                               D3 = paste(D3, collapse=","),
                               D4 = paste(D4, collapse=","),
                               D5 = paste(D5, collapse=","),
                               D6 = paste(D6, collapse=","),
                               D7 = paste(D7, collapse=","),
                               D8 = paste(D8, collapse=","),
                               D9 = paste(D9, collapse=","),
                               D10 = paste(D10, collapse=","),
                               D11 = paste(D11, collapse=","),
                               D12 = paste(D12, collapse=","),
                               D13 = paste(D13, collapse=","),
                               D14 = paste(D14, collapse=","),
                               D15 = paste(D15, collapse=","),
                               D16 = paste(D16, collapse=",") ), by = .(Source, subrow) ]

ip8ref <- ip_ref [Name %in% c("D1", "D2", "D3", "D4", "D5", "D6","D7",
                              "D8", "D9", "D10", "D11","D12",
                              "D13", "D14", "D15")]
ip8reft <- melt(data =ip8ref,
                id.vars = c("V1", "Name"),
                measure = patterns("Var"),
                variable.factor=FALSE,
                variable.name ="ref",
                na.rm=TRUE)
ip8reft <- ip8reft [, value2 := gsub(":", "", value1), ][value2 != ""]

d8ip <- data.table( iprec3tran [, c("D1", "D2", "D3", "D4", "D5", "D6", "D7",
                                    "D8", "D9", "D10", "D11","D12",
                                    "D13", "D14", "D15",
                                    "subrow", "Source"), with =FALSE])
d8ipt <-  melt(data =d8ip,
               id.vars = c("Source", "subrow"),
               measure = patterns("D"),
               variable.factor=FALSE,
               variable.name ="dataset",
               na.rm=TRUE)

d8ipt <- d8ipt [, trn:= gsub(" *([a-zA-Z0-9/]*\ *[a-zA-Z0-9/]+,?):", "@\\1:", value1, perl=TRUE),]
d8ipt2 <- d8ipt [, list(X = unlist(strsplit(trn, "@"))), by =.(Source, dataset)][X != ""]
d8ipt2 <- d8ipt2[, c("tmp1", "tmp2") := tstrsplit(X, ":", fill=""),  ]
d8ipt3 <- merge(x = d8ipt2,    by.x =c ("dataset", "tmp1"),
                y = ip8reft,    by.y =c ("Name", "value2"),
                all.x=TRUE)

d8ipt3tran <- dcast (data = d8ipt3,
                     Source + V1 + dataset ~ ref,
                     subset = .(ref != "NA"),
                     value.var ="tmp2") [order (Source, dataset)]
d8ipt3tran[is.na(d8ipt3tran)]<- ""
# Issue with D1, D14
