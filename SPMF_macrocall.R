
library(data.table)
library(tidyverse)
library(sqldf)

###############################################
#
# create a lookup table with combinations of 
# diseases 
# before after time period
# Filenames for the outputs and inputs
# Execute Java program with various algorithms
#
#Disease / Medicine
#
# (1) Before / After
# (2) Unique / All records
# (3) Output file Space / seperated by -1
# (4) Algorithm combination with files
# (5) Output from Java program
# (6) Output from R program CSV file
###############################################
all_met_rmsd02 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/all_met_rmsd02.rds")

#type <- "med"

for( type in c("med", "dis") ) {
  
  if (tolower(type) == "dis")
  {
    all_met_rmsd02 <- all_met_rmsd02 [, Code := ifelse (Code == " " | Code == "", "** Not yet coded", Code),]
    all_met_rmsd02 <- all_met_rmsd02 [, description:= ifelse (description == "" | description ==" ", "** Not yet coded", description),]
    all_met_rmsd02 <- all_met_rmsd02 [, Code02 := paste(Code, ":", description, sep =""), ]
    all_met_rmsd02 <- all_met_rmsd02 [, comdisn := .GRP, by = .(Code02)]
    all_met_rmsd02 <- all_met_rmsd02 [Code != "** Not yet coded"]
    all_met_rmsd02 <- all_met_rmsd02 [ order(mr_no, refcode, refdesc, period)]
    all_met_rmsd03 <- unique( all_met_rmsd02 [, c("mr_no", "refcode", "refdesc", "Code02", "Code", "period",
                                                  "comdisn", "patient_gender", "baseage"), ])
    
  }
  
  if (tolower(type) == "med")
  {
    all_met_rmsd02 <- all_met_rmsd02 [! Coded_med %in% c("", " "),]
    all_met_rmsd02 <- all_met_rmsd02 [, Code02 := paste(Type_med, ":", Coded_med, sep =""), ]
    all_met_rmsd02 <- all_met_rmsd02 [, comdisn := .GRP, by = .(Code02)]
    all_met_rmsd02 <- all_met_rmsd02 [Code != "** Not yet coded"]
    all_met_rmsd02 <- all_met_rmsd02 [ order(mr_no, refcode, refdesc, period)]
    all_met_rmsd03 <- unique( all_met_rmsd02 [, c("mr_no", "refcode", "refdesc", "Code02", "Code", "period",
                                                  "comdisn", "patient_gender", "baseage"), ])
    
  }
  
  refcode <- unique( all_met_rmsd03 [, c("refcode")])
  period <- c(">= 0", "< 0", "< 9999")
  algo <- c("SPADE") # "Apriori"
  
  comb01 <- setDT( crossing (refcode = refcode, period = period, algo = algo) )
  comb01 <- sqldf("select *, 
                   case 
                  When period == '< 0' then 'Before'
                  When period == '>= 0' then 'After'
                  When period == '< 9999' then 'All'
                  end as bfraftr
                  from comb01")
  comb01 <- as.data.table(comb01)
  #comb01 <- comb01 [, bfraftr := ifelse(period == "< 0", "Before", "After"),]
  comb01 <- comb01 [, path := paste(path <- "D:/Hospital_data/ProgresSQL/analysis_spmf_InputsOutputs/", refcode, "/", sep=""),]
  comb01 <- comb01 [, spddis := paste("SPADE_", refcode, bfraftr, "disunq.txt", sep=""),]
  comb01 <- comb01 [, arfdis := paste("ARFF_", refcode, bfraftr, "disunq.txt", sep=""),]
  comb01 <- comb01 [, spdmed := paste("SPADE_", refcode, bfraftr, "medunq.txt", sep=""),]
  comb01 <- comb01 [, arfmed := paste("ARFF_", refcode, bfraftr, "medunq.txt", sep=""),]
  comb01 <- comb01 [, sub01 := paste( "refcode == '", refcode, "'", sep=""),]
  comb01 <- comb01 [, sub02 := paste( "! (Code == '", refcode, "' & period", period, ")", sep=""),]
  comb01 <- comb01 [, java := paste("system('java -jar D:/Hospital_data/ProgresSQL/analysis_spmf/spmf-V2.35-VDate18NOV2018.jar run "),]
  comb01 <- comb01 [, rnum := 1:.GRP, by = .(refcode)]
  comb01 <- comb01 [, rdis := .I, by = .(refcode)]
  comb01 <- comb01 [, step000 := paste("path <-'", path, "'", sep="" ),]
  comb01 <- comb01 [, step001 := paste("all_met_rmsd04 <- all_met_rmsd03 [", sub01, "]" ),]
  comb01 <- comb01 [, step002 := paste("all_met_rmsd040 <- all_met_rmsd03 [", sub02, "]" ),]
  comb01 <- comb01 [, step003 := paste('all_met_rmsd05 <- all_met_rmsd040 [, .(combdis = paste(comdisn, collapse = " ", sep = " " )), 
                                       by = .(mr_no, refcode, refdesc, baseage)]'), ]
  
  comb01 <- comb01 [, step004 := paste('all_met_rmsd06 <- all_met_rmsd05 [, .(combdis02 = paste(combdis, collapse = " -1 ", sep = " " )), 
                                       by = .(mr_no, refcode, refdesc, baseage)] '),]
  comb01 <- comb01 [, step005 := paste('all_met_rmsd06 <- all_met_rmsd06 [, combdis02 := paste(combdis02, " -1 -2", sep = ""), ]'), ]
  
  if (tolower(type) == "dis")
  {
    comb01 <- comb01 [, step006 := paste('fwrite(x = all_met_rmsd06 [, c("combdis02"),], 
                                         col.names = FALSE,
                                         file = paste("', path, spddis, '", sep="") )', sep=""), ]
  }
  
  if (tolower(type) == "med")
  {
    comb01 <- comb01 [, step006 := paste('fwrite(x = all_met_rmsd06 [, c("combdis02"),], 
                                         col.names = FALSE,
                                         file = paste("', path, spdmed, '", sep="") )', sep=""), ]
  }
  
  comb01_t <- melt(data = comb01,
                   id.vars = c("refcode", "period",  "algo",  "bfraftr", "rnum", "rdis", "path"),
                   measure.vars = c("step000", "step001", "step002", "step003", 
                                    "step004", "step005", "step006") )
  
  
  x<- c(1:50)
  comb02 <- setDT( crossing (comb01, x =x ) )
  comb02 <- comb02 [, perc := paste(x, "%", sep=""), ]
  comb02 <- comb02 [, x2 := str_pad(x, 3, side = "left", pad = 0),] 
  
  if (tolower(type) == "dis")
  {
    comb02 <- comb02 [, value := paste(java, " ", algo, ' "', path, spddis, '" ', '"', path, "o", str_replace(spddis, '.txt', ''), x2, 'perc.txt','" ', perc, "')", sep="" ),]
  }
  
  if (tolower(type) == "med")
  {
    comb02 <- comb02 [, value := paste(java, " ", algo, ' "', path, spdmed, '" ', '"', path, "o", str_replace(spdmed, '.txt', ''), x2, 'perc.txt','" ', perc, "')", sep="" ),]
  }
  
  comb02_t <- comb02 [, c("refcode", "period",  "algo",  "bfraftr", "rnum", "rdis", "value", "path"),]
  
  post <- comb01 [, step008 := paste('disnum <- unique( all_met_rmsd02 [, c("Code02", "comdisn"),])', sep=""),]
  post <- post [, step009 := paste('disnum <- disnum [, comdisn := as.numeric(comdisn),]', sep=""),]
  
  if (tolower(type) == "dis")
  {
    post <- post [, step010 := paste( 'list_of_files <- list.files(path = path, pattern = glob2rx("o', algo, "*", bfraftr, "disunq*perc.txt", '"', "))", sep="" ) ,]
  }
  
  if (tolower(type) == "med")
  {
    post <- post [, step010 := paste( 'list_of_files <- list.files(path = path, pattern = glob2rx("o', algo, "*", bfraftr, "medunq*perc.txt", '"', "))", sep="" ) ,]
  }
  
  post <- post [, step020 := paste ('out <- rbindlist( sapply(paste(path, list_of_files, sep=""), fread, simplify = FALSE, sep="!", header = FALSE),
                                    use.names = TRUE, idcol = "temp" )', sep=""),]
  post <- post [, step030 := paste('out <- out [, V1 := paste(temp, "#", V1, sep=""),]', sep=""),]
  post <- post [, step035 := paste('out <- out [, V1 := str_remove(V1, "-1"),]', sep=""),]
  post <- post [, step040 := paste('out <- out [, -c("temp"), ]', sep=""),]
  post <- post [, step050 := paste('out2 <- out [, c("var03", "var01", "var02") := tstrsplit(V1, "#"),]', sep=""),]
  post <- post [, step060 := paste('out3 <- out2 [, c("var021", "var022") := tstrsplit(trimws(var01), "==>"),]', sep=""),]
  post <- post [, step070 := paste('out4 <- out3 [, `:=` (cntvar021 = max(str_count( trimws(var021), " ")) + 1,
                                   cntvar022 = max(str_count( trimws(var022), " ")) + 1 ),]', sep=""),]
  
  post <- post [, step080 := paste('out5 <- out4 [, paste0("stt", 1:max(out4$cntvar021)) := tstrsplit(trimws(var021), " ", fixed = TRUE ),]', sep=""),]
  post <- post [, step090 := paste('out6 <- out5 [, paste0("end", 1:max(out5$cntvar022)) := tstrsplit(trimws(var022), " ", fixed = TRUE),]', sep=""),]
  post <- post [, step100 := paste('out6_tra <- melt (data = out6, 
                                   id.vars = 1:8,
                                   value.factor = FALSE)', sep=""),]
  post <- post [, step110 := paste('out6_tra <- out6_tra [, value := as.numeric(value), ]', sep=""), ]
  post <- post [, step120 := paste('out7 <- merge (x = out6_tra, 
                                   y = disnum,
                                   by.x = c("value"),
                                   by.y = c("comdisn") )', sep=""),]
  
  post <- post [, step130 := paste('out7_tra <- dcast (data = out7,
                                   formula = V1 + var01 + var02 + var03 + var021 + var022 + cntvar021 + cntvar022 ~ variable,
                                   value.var = c("Code02"), 
                                   fill = "")', sep=""),]
  
  post <- post [, step140 := paste('out8 <- out7_tra [, newstt := do.call(paste, c(.SD, sep = " ")), .SDcols = paste0("stt", 1:max(out7_tra$cntvar021)), ]', sep=""),]
  post <- post [, step150 := paste('out8 <- out8 [, newend := do.call(paste, c(.SD, sep = " ")), .SDcols = paste0("end", 1:max(out8$cntvar022)), ]', sep=""),]
  post <- post [, step160 := paste('out9 <- out8 [, c("newstt", "newend", "var02", "var03"),]', sep=""),]
  post <- post [, step170 := paste('out9 <- out9 [ order ( var03)]', sep=""),]
  
  if (tolower(type) == "dis")
  {
    post <- post [, step180 := paste('fwrite(out9, file = paste(path, "o', str_replace(spddis, ".txt", '_formatted.csv'), '"', ", sep='') )", sep="") ,]
  }
  
  if (tolower(type) == "med")
  {
    post <- post [, step180 := paste('fwrite(out9, file = paste(path, "o', str_replace(spdmed, ".txt", '_formatted.csv'), '"', ", sep='') )", sep="") ,]
  }
  
  post <- post [, step190 := paste( "file.remove( paste(path, list_of_files, sep='') )", "\n", sep=""), ]
  post_t <- melt(data = post,
                 id.vars = c("refcode", "period",  "algo",  "bfraftr", "rnum", "rdis", "path"),
                 measure.vars = c("step008", "step009", "step010", "step020", "step030", "step035", "step040",
                                  "step050", "step060", "step070", "step080",
                                  "step090", "step100", "step110", "step120",
                                  "step130", "step140", "step150", "step160",
                                  "step170", "step180", "step190") )
  
  comb01_all <- rbind (comb01_t [, -c("variable")], comb02_t, post_t [, -c("variable")] )
  comb01_all <- comb01_all [, cat := type]
  comb01_all <- comb01_all [ order(rdis)]
  
  
  print(type)
  
  if (tolower(type) == "dis") { final_dis <- copy(comb01_all) }
  if (tolower(type) == "med") { final_med <- copy(comb01_all) }
  
  }

l <- list(final_dis, final_med)
final_all <- rbindlist(l)

comb200 <- final_all [refcode %in% c("V2.63", "A2.0", "M2.0", "P5.0"), c("value", "refcode", "path", "algo", "cat"),]
comb200 <- comb200 [, output := paste0(path, refcode, algo, cat, ".R"), ]

for(i in unique(comb200$output)) {
  da <- comb200[ output == i]
  fwrite(da [, c("value"),], 
         i, # outfile
         row.names = FALSE,
         col.names = FALSE,
         quote = FALSE,
         sep="\n")
}

# create calls using "source" for these R codes into another file

comb300 <- unique( comb200 [, c("output"),])
comb300 <- comb300 [, coderun := paste("source ('", output, "')", sep=""), ]

fwrite(comb300 [, c("coderun"),], 
       "D:/Hospital_data/ProgresSQL/analysis_spmf_InputsOutputs/SPMF_macrocall_coderun.R", # outfile
       row.names = FALSE,
       col.names = FALSE,
       quote = FALSE,
       sep="\n")

source("D:/Hospital_data/ProgresSQL/analysis_spmf_InputsOutputs/SPMF_macrocall_coderun.R")

##########################################################################################

library(data.table)
library(tidyverse)

###############################################
#
# create a lookup table with combinations of 
# diseases 
# before after time period
# Filenames for the outputs and inputs
# Execute Java program with various algorithms
#
###############################################

all_met_rmsd02 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/all_met_rmsd02.rds")
all_met_rmsd02 <- all_met_rmsd02 [! Coded_med %in% c("", " "),]
all_met_rmsd02 <- all_met_rmsd02 [, Code02 := paste(Type_med, ":", Coded_med, sep =""), ]
all_met_rmsd02 <- all_met_rmsd02[, comdisn := .GRP, by = .(Code02)]

all_met_rmsd02 <- all_met_rmsd02[Code != "** Not yet coded"]

all_met_rmsd02 <- all_met_rmsd02 [ order(mr_no, refcode, refdesc, period)]
all_met_rmsd03 <- unique( all_met_rmsd02 [, c("mr_no", "refcode", "refdesc", "Code02", "Code", "period",
                                              "comdisn", "patient_gender", "baseage"), ])


refcode <- unique( all_met_rmsd03 [, c("refcode")])
period <- c(">= 0", "< 0")
algo <- c("SPADE") # "Apriori"

comb01 <- setDT( crossing (refcode = refcode, period = period, algo = algo) )

comb01 <- comb01 [, bfraftr := ifelse(period == "< 0", "Before", "After"),]
comb01 <- comb01 [, path := paste(path <- "D:/Hospital_data/ProgresSQL/analysis_spmf_InputsOutputs/", refcode, "/", sep=""),]
comb01 <- comb01 [, spddis := paste("SPADE_", refcode, bfraftr, "unq.txt", sep=""),]
comb01 <- comb01 [, arfdis := paste("ARFF_", refcode, bfraftr, "unq.txt", sep=""),]
comb01 <- comb01 [, spdmed := paste("SPADE_", refcode, bfraftr, "Medunq.txt", sep=""),]
comb01 <- comb01 [, arfmed := paste("ARFF_", refcode, bfraftr, "Medunq.txt", sep=""),]
comb01 <- comb01 [, sub01 := paste( "refcode == '", refcode, "'", sep=""),]
comb01 <- comb01 [, sub02 := paste( "! (Code == '", refcode, "' & period", period, ")", sep=""),]
comb01 <- comb01 [, java := paste("system('java -jar D:/Hospital_data/ProgresSQL/analysis_spmf/spmf-V2.35-VDate18NOV2018.jar run "),]
comb01 <- comb01 [, rnum := 1:.GRP, by = .(refcode)]
comb01 <- comb01 [, rdis := .I, by = .(refcode)]

comb01 <- comb01 [, step000 := paste("path <-'", path, "'", sep="" ),]
comb01 <- comb01 [, step001 := paste("all_met_rmsd04 <- all_met_rmsd03 [", sub01, "]" ),]
comb01 <- comb01 [, step002 := paste("all_met_rmsd040 <- all_met_rmsd03 [", sub02, "]" ),]
comb01 <- comb01 [, step003 := paste('all_met_rmsd05 <- all_met_rmsd040 [, .(combdis = paste(comdisn, collapse = " ", sep = " " )), 
                                     by = .(mr_no, refcode, refdesc, baseage)]'), ]

comb01 <- comb01 [, step004 := paste('all_met_rmsd06 <- all_met_rmsd05 [, .(combdis02 = paste(combdis, collapse = " -1 ", sep = " " )), 
                                     by = .(mr_no, refcode, refdesc, baseage)] '),]
comb01 <- comb01 [, step005 := paste('all_met_rmsd06 <- all_met_rmsd06 [, combdis02 := paste(combdis02, " -1 -2", sep = ""), ]'), ]
comb01 <- comb01 [, step006 := paste('fwrite(x = all_met_rmsd06 [, c("combdis02"),], 
                                     col.names = FALSE,
                                     file = paste("', path, spdmed, '", sep="") )', sep=""), ]

comb01_t <- melt(data = comb01,
                 id.vars = c("refcode", "period",  "algo",  "bfraftr", "rnum", "rdis"),
                 measure.vars = c("step000", "step001", "step002", "step003", 
                                  "step004", "step005", "step006") )


x<- c(1:50)
comb02 <- setDT( crossing (comb01, x =x ) )
comb02 <- comb02 [, perc := paste(x, "%", sep=""), ]
comb02 <- comb02 [, value := paste(java, " ", algo, ' "', path, spdmed, '" ', '"', path, "o", str_replace(spdmed, '.txt', ''), x, 'perc.txt','" ', perc, "')", sep="" ),]
comb02_t <- comb02 [, c("refcode", "period",  "algo",  "bfraftr", "rnum", "rdis", "value"),]


post <- comb01 [, step008 := paste('disnum <- unique( all_met_rmsd02 [, c("Code02", "comdisn"),])', sep=""),]
post <- post [, step009 := paste('disnum <- disnum [, comdisn := as.numeric(comdisn),]', sep=""),]
post <- post [, step010 := paste( 'list_of_files <- list.files(path = path, pattern = glob2rx("o', algo, "*", bfraftr, "Medunq*perc.txt", '"', "))", sep="" ) ,]
post <- post [, step020 := paste ('out <- rbindlist( sapply(paste(path, list_of_files, sep=""), fread, simplify = FALSE, sep="!", header = FALSE),
                                  use.names = TRUE, idcol = "temp" )', sep=""),]
post <- post [, step030 := paste('out <- out [, V1 := paste(temp, "#", V1, sep=""),]', sep=""),]
post <- post [, step035 := paste('out <- out [, V1 := str_remove(V1, "-1"),]', sep=""),]
post <- post [, step040 := paste('out <- out [, -c("temp"), ]', sep=""),]
post <- post [, step050 := paste('out2 <- out [, c("var03", "var01", "var02") := tstrsplit(V1, "#"),]', sep=""),]
post <- post [, step060 := paste('out3 <- out2 [, c("var021", "var022") := tstrsplit(trimws(var01), "==>"),]', sep=""),]
post <- post [, step070 := paste('out4 <- out3 [, `:=` (cntvar021 = max(str_count( trimws(var021), " ")) + 1,
                                 cntvar022 = max(str_count( trimws(var022), " ")) + 1 ),]', sep=""),]

post <- post [, step080 := paste('out5 <- out4 [, paste0("stt", 1:max(out4$cntvar021)) := tstrsplit(trimws(var021), " ", fixed = TRUE ),]', sep=""),]
post <- post [, step090 := paste('out6 <- out5 [, paste0("end", 1:max(out5$cntvar022)) := tstrsplit(trimws(var022), " ", fixed = TRUE),]', sep=""),]
post <- post [, step100 := paste('out6_tra <- melt (data = out6, 
                                 id.vars = 1:8,
                                 value.factor = FALSE)', sep=""),]
post <- post [, step110 := paste('out6_tra <- out6_tra [, value := as.numeric(value), ]', sep=""), ]
post <- post [, step120 := paste('out7 <- merge (x = out6_tra, 
                                 y = disnum,
                                 by.x = c("value"),
                                 by.y = c("comdisn") )', sep=""),]

post <- post [, step130 := paste('out7_tra <- dcast (data = out7,
                                 formula = V1 + var01 + var02 + var03 + var021 + var022 + cntvar021 + cntvar022 ~ variable,
                                 value.var = c("Code02"), 
                                 fill = "")', sep=""),]

post <- post [, step140 := paste('out8 <- out7_tra [, newstt := do.call(paste, c(.SD, sep = " ")), .SDcols = paste0("stt", 1:max(out7_tra$cntvar021)), ]', sep=""),]
post <- post [, step150 := paste('out8 <- out8 [, newend := do.call(paste, c(.SD, sep = " ")), .SDcols = paste0("end", 1:max(out8$cntvar022)), ]', sep=""),]
post <- post [, step160 := paste('out9 <- out8 [, c("newstt", "newend", "var02", "var03"),]', sep=""),]
post <- post [, step170 := paste('out9 <- out9 [ order ( var03)]', sep=""),]
post <- post [, step180 := paste('fwrite(out9, file = paste(path, "o', str_replace(spdmed, ".txt", '_formatted.csv'), '"', ", sep='') )", "\n", sep="") ,]

post_t <- melt(data = post,
               id.vars = c("refcode", "period",  "algo",  "bfraftr", "rnum", "rdis"),
               measure.vars = c("step008", "step009", "step010", "step020", "step030", "step035", "step040",
                                "step050", "step060", "step070", "step080",
                                "step090", "step100", "step110", "step120",
                                "step130", "step140", "step150", "step160",
                                "step170", "step180") )

comb01_all <- rbind (comb01_t [, -c("variable")], comb02_t, post_t [, -c("variable")] )
comb01_all <- comb01_all [ order(rdis)]


fwrite(comb01_all [refcode == "V2.63", c("value"),], 
       "D:/Hospital_data/ProgresSQL/analysis_spmf_InputsOutputs/chk_t.txt", # outfile
       row.names = FALSE,
       col.names = FALSE,
       quote = FALSE,
       sep="\n")

##########################################################################################

# working
system('java -jar D:/Hospital_data/ProgresSQL/analysis_spmf/spmf-V2.35-VDate18NOV2018.jar run Apriori "D:/Hospital_data/ProgresSQL/analysis_spmf_InputsOutputs/P5.0/ARFF_P5.0_AfterMedunq.txt" "D:/Hospital_data/ProgresSQL/analysis_spmf_InputsOutputs/P5.0/oARFF_P5.0_AfterMedunq_01perc.txt" 1%')

path <- "D:/Hospital_data/ProgresSQL/analysis_spmf_InputsOutputs/P5.0/"

x <- c(1:50)
input <- paste(path, "ARFF_P5.0_AfterMedunq.txt", sep="")
output <- paste(path, "vinayoARFF_P5.0_AfterMedunq_", str_pad(x, 2, side = "left", pad = 0), "perc.txt", sep="")
perc <- paste(x, "%", sep="")

options(useFancyQuotes = FALSE)
ll <- noquote (paste( "'java -jar D:/Hospital_data/ProgresSQL/analysis_spmf/spmf-V2.35-VDate18NOV2018.jar run Apriori ",
             noquote(dQuote(input)) , ' ',  
             noquote(dQuote(output)), ' ', perc, "'", sep="") )

ll2 <- paste("system (", ll, ")", sep="")

write.table(ll2, "D:/Hospital_data/ProgresSQL/analysis_spmf_InputsOutputs/P5.0/chk.txt",
            quote = FALSE, 
            col.names = FALSE,
            row.names = FALSE)

source("D:/Hospital_data/ProgresSQL/analysis_spmf_InputsOutputs/P5.0/chk.txt")







fwrite(comb01 [, c("step001", "step002", "step003", 
                   "step004", "step005", "step006", "step007"),], 
       "D:/Hospital_data/ProgresSQL/analysis_spmf_InputsOutputs/chk.txt", # outfile
       row.names = FALSE,
       col.names = FALSE,
       quote = FALSE,
       sep="\n")

all_met_rmsd04 <- all_met_rmsd03 [ refcode == "P5.0"]
all_met_rmsd040 <- all_met_rmsd04 [ ! (Code == "P5.0" & period >= 0) ]

all_met_rmsd05 <- all_met_rmsd040 [, .(combdis = paste(comdisn, collapse = " ", sep = " " )), 
                                   by = .(mr_no, refcode, refdesc, baseage)]


for(i in unique( head(refcode) ) )
{
  for row in 1:comb01$rnum
{
    print (row)
   #comb02 <- comb01 [ rnum == row]
  }}   


  all_met_rmsd04 <- all_met_rmsd03 [ comb02$sub01 ]
  all_met_rmsd040 <- all_met_rmsd04 [ comb02$sub02 ]
  
  all_met_rmsd05 <- all_met_rmsd040 [, .(combdis = paste(comdisn, collapse = " ", sep = " " )), 
                                     by = .(mr_no, refcode, refdesc, baseage)]
  
# create 1 line per patient
# -1 to seperate itemset and
# -2 to seperate sequence

all_met_rmsd06 <- all_met_rmsd05 [, .(combdis02 = paste(combdis, collapse = " -1 ", sep = " " )), 
                                  by = .(mr_no, refcode, refdesc, baseage)]
all_met_rmsd06 <- all_met_rmsd06 [, combdis02 := paste(combdis02, " -1 -2", sep = ""), ]

fwrite(x = all_met_rmsd06 [, c("combdis02"),], 
       col.names = FALSE,
       file = paste(path, spdmed, sep="") )

disnum <- unique( all_met_rmsd02 [, c("Code02", "comdisn"),])
disnum <- disnum [, comdisn := as.numeric(comdisn),]

# Layout needed for the Associatio rules:
# FPGrowth_association_rules
all_met_rmsd06_arff <- all_met_rmsd05 [, .(combdis02 = paste(combdis, collapse = " ", sep = " " )), 
                                       by = .(mr_no, refcode, refdesc, baseage)]

fwrite(x = all_met_rmsd06_arff [, c("combdis02"),], 
       col.names = FALSE,
       file = paste(comb02$path, comb02$arfmed, sep="") )
  } 
}

system('java -jar D:/Hospital_data/ProgresSQL/analysis_spmf/spmf-V2.35-VDate18NOV2018.jar run Apriori "D:/Hospital_data/ProgresSQL/analysis_spmf_InputsOutputs/P5.0/ARFF_P5.0_AfterMedunq.txt" "D:/Hospital_data/ProgresSQL/analysis_spmf_InputsOutputs/P5.0/oARFF_P5.0_AfterMedunq_01perc.txt" 1%')
# Execute the java program command with Apriori algorithm


Frequent itemset mining:
  
  D:\Hospital_data\ProgresSQL\analysis_spmf

java -jar spmf-V2.35-VDate18NOV2018.jar run Apriori "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\P5.0\ARFF_P5.0_AfterMedunq.txt" "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\P5.0\oARFF_P5.0_AfterMedunq_01perc.txt" 1%
java -jar spmf-V2.35-VDate18NOV2018.jar run Apriori "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\P5.0\ARFF_P5.0_AfterMedunq.txt" "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\P5.0\oARFF_P5.0_AfterMedunq_02perc.txt" 2%
java -jar spmf-V2.35-VDate18NOV2018.jar run Apriori "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\P5.0\ARFF_P5.0_AfterMedunq.txt" "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\P5.0\oARFF_P5.0_AfterMedunq_03perc.txt" 3%
java -jar spmf-V2.35-VDate18NOV2018.jar run Apriori "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\P5.0\ARFF_P5.0_AfterMedunq.txt" "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\P5.0\oARFF_P5.0_AfterMedunq_04perc.txt" 4%
java -jar spmf-V2.35-VDate18NOV2018.jar run Apriori "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\P5.0\ARFF_P5.0_AfterMedunq.txt" "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\P5.0\oARFF_P5.0_AfterMedunq_05perc.txt" 5%
java -jar spmf-V2.35-VDate18NOV2018.jar run Apriori "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\P5.0\ARFF_P5.0_AfterMedunq.txt" "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\P5.0\oARFF_P5.0_AfterMedunq_06perc.txt" 6%
java -jar spmf-V2.35-VDate18NOV2018.jar run Apriori "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\P5.0\ARFF_P5.0_AfterMedunq.txt" "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\P5.0\oARFF_P5.0_AfterMedunq_07perc.txt" 7%
java -jar spmf-V2.35-VDate18NOV2018.jar run Apriori "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\P5.0\ARFF_P5.0_AfterMedunq.txt" "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\P5.0\oARFF_P5.0_AfterMedunq_08perc.txt" 8%
java -jar spmf-V2.35-VDate18NOV2018.jar run Apriori "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\P5.0\ARFF_P5.0_AfterMedunq.txt" "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\P5.0\oARFF_P5.0_AfterMedunq_09perc.txt" 9%
java -jar spmf-V2.35-VDate18NOV2018.jar run Apriori "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\P5.0\ARFF_P5.0_AfterMedunq.txt" "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\P5.0\oARFF_P5.0_AfterMedunq_10perc.txt" 10%
java -jar spmf-V2.35-VDate18NOV2018.jar run Apriori "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\P5.0\ARFF_P5.0_AfterMedunq.txt" "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\P5.0\oARFF_P5.0_AfterMedunq_11perc.txt" 11%
java -jar spmf-V2.35-VDate18NOV2018.jar run Apriori "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\P5.0\ARFF_P5.0_AfterMedunq.txt" "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\P5.0\oARFF_P5.0_AfterMedunq_12perc.txt" 12%
java -jar spmf-V2.35-VDate18NOV2018.jar run Apriori "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\P5.0\ARFF_P5.0_AfterMedunq.txt" "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\P5.0\oARFF_P5.0_AfterMedunq_13perc.txt" 13%
java -jar spmf-V2.35-VDate18NOV2018.jar run Apriori "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\P5.0\ARFF_P5.0_AfterMedunq.txt" "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\P5.0\oARFF_P5.0_AfterMedunq_14perc.txt" 14%
java -jar spmf-V2.35-VDate18NOV2018.jar run Apriori "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\P5.0\ARFF_P5.0_AfterMedunq.txt" "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\P5.0\oARFF_P5.0_AfterMedunq_15perc.txt" 15%
java -jar spmf-V2.35-VDate18NOV2018.jar run Apriori "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\P5.0\ARFF_P5.0_AfterMedunq.txt" "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\P5.0\oARFF_P5.0_AfterMedunq_16perc.txt" 16%
java -jar spmf-V2.35-VDate18NOV2018.jar run Apriori "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\P5.0\ARFF_P5.0_AfterMedunq.txt" "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\P5.0\oARFF_P5.0_AfterMedunq_17perc.txt" 17%
java -jar spmf-V2.35-VDate18NOV2018.jar run Apriori "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\P5.0\ARFF_P5.0_AfterMedunq.txt" "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\P5.0\oARFF_P5.0_AfterMedunq_18perc.txt" 18%
java -jar spmf-V2.35-VDate18NOV2018.jar run Apriori "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\P5.0\ARFF_P5.0_AfterMedunq.txt" "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\P5.0\oARFF_P5.0_AfterMedunq_19perc.txt" 19%
java -jar spmf-V2.35-VDate18NOV2018.jar run Apriori "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\P5.0\ARFF_P5.0_AfterMedunq.txt" "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\P5.0\oARFF_P5.0_AfterMedunq_20perc.txt" 20%
java -jar spmf-V2.35-VDate18NOV2018.jar run Apriori "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\P5.0\ARFF_P5.0_AfterMedunq.txt" "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\P5.0\oARFF_P5.0_AfterMedunq_21perc.txt" 21%
java -jar spmf-V2.35-VDate18NOV2018.jar run Apriori "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\P5.0\ARFF_P5.0_AfterMedunq.txt" "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\P5.0\oARFF_P5.0_AfterMedunq_22perc.txt" 22%
java -jar spmf-V2.35-VDate18NOV2018.jar run Apriori "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\P5.0\ARFF_P5.0_AfterMedunq.txt" "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\P5.0\oARFF_P5.0_AfterMedunq_23perc.txt" 23%
java -jar spmf-V2.35-VDate18NOV2018.jar run Apriori "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\P5.0\ARFF_P5.0_AfterMedunq.txt" "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\P5.0\oARFF_P5.0_AfterMedunq_24perc.txt" 24%
java -jar spmf-V2.35-VDate18NOV2018.jar run Apriori "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\P5.0\ARFF_P5.0_AfterMedunq.txt" "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\P5.0\oARFF_P5.0_AfterMedunq_25perc.txt" 25%
java -jar spmf-V2.35-VDate18NOV2018.jar run Apriori "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\P5.0\ARFF_P5.0_AfterMedunq.txt" "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\P5.0\oARFF_P5.0_AfterMedunq_26perc.txt" 26%
java -jar spmf-V2.35-VDate18NOV2018.jar run Apriori "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\P5.0\ARFF_P5.0_AfterMedunq.txt" "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\P5.0\oARFF_P5.0_AfterMedunq_27perc.txt" 27%
java -jar spmf-V2.35-VDate18NOV2018.jar run Apriori "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\P5.0\ARFF_P5.0_AfterMedunq.txt" "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\P5.0\oARFF_P5.0_AfterMedunq_28perc.txt" 28%
java -jar spmf-V2.35-VDate18NOV2018.jar run Apriori "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\P5.0\ARFF_P5.0_AfterMedunq.txt" "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\P5.0\oARFF_P5.0_AfterMedunq_29perc.txt" 29%
java -jar spmf-V2.35-VDate18NOV2018.jar run Apriori "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\P5.0\ARFF_P5.0_AfterMedunq.txt" "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\P5.0\oARFF_P5.0_AfterMedunq_30perc.txt" 30%
java -jar spmf-V2.35-VDate18NOV2018.jar run Apriori "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\P5.0\ARFF_P5.0_AfterMedunq.txt" "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\P5.0\oARFF_P5.0_AfterMedunq_31perc.txt" 31%
java -jar spmf-V2.35-VDate18NOV2018.jar run Apriori "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\P5.0\ARFF_P5.0_AfterMedunq.txt" "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\P5.0\oARFF_P5.0_AfterMedunq_32perc.txt" 32%
java -jar spmf-V2.35-VDate18NOV2018.jar run Apriori "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\P5.0\ARFF_P5.0_AfterMedunq.txt" "D:\Hospital_data\ProgresSQL\analysis_spmf_InputsOutputs\P5.0\oARFF_P5.0_AfterMedunq_33perc.txt" 33%


# /cygdrive/d/Hospital_data/ProgresSQL/analysis_spmf_InputsOutputs/P5.0
# Combine the files into 1 file 
# gawk '{print FILENAME "#", $0}' oARFF_P5.0_AfterMedunq_*perc.txt > oARFF_Apriori_P5.0_AfterMedunq.txt

# List all txt files including sub-folders
list_of_files <- list.files(path = path, 
                            pattern = glob2rx("oARFF*AfterMedunq_*perc.txt"))


# Read all the files and create a FileName column to store filenames
out <- rbindlist( sapply(paste(path, list_of_files, sep=""), fread, simplify = FALSE, sep="!", header = FALSE),
                  use.names = TRUE, idcol = "temp" )

out <- out [, V1 := paste(temp, "#", V1, sep=""),]
out <- out [, V1 := str_remove(V1, "-1"),]
out <- out [, -c("temp"), ]

# out <- fread( paste(path, "oARFF_Apriori_P5.0_AfterMedunq.txt", sep=""), sep ="!", header = FALSE)

out2 <- out [, c("var03", "var01", "var02") := tstrsplit(V1, "#"),]
out3 <- out2 [, c("var021", "var022") := tstrsplit(trimws(var01), "==>"),]
out4 <- out3 [, `:=` (cntvar021 = max(str_count( trimws(var021), " ")) + 1,
                      cntvar022 = max(str_count( trimws(var022), " ")) + 1 ),]

out5 <- out4 [, paste0("stt", 1:max(out4$cntvar021)) := tstrsplit(trimws(var021), " ", fixed = TRUE ),]
out6 <- out5 [, paste0("end", 1:max(out5$cntvar022)) := tstrsplit(trimws(var022), " ", fixed = TRUE),]
#out6 <- out6 [, nrow := 1:.N,]

out6_tra <- melt (data = out6, 
                  id.vars = 1:8,
                  value.factor = FALSE)

out6_tra <- out6_tra [, value := as.numeric(value), ]

out7 <- merge (x = out6_tra, 
               y = disnum,
               by.x = c("value"),
               by.y = c("comdisn") )

out7_tra <- dcast (data = out7,
                   formula = V1 + var01 + var02 + var03 + var021 + var022 + cntvar021 + cntvar022 ~ variable,
                   value.var = c("Code02"), 
                   fill = "")

out8 <- out7_tra [, newstt := do.call(paste, c(.SD, sep = " ")), .SDcols = paste0("stt", 1:max(out7_tra$cntvar021)), ]
out8 <- out8 [, newend := do.call(paste, c(.SD, sep = " ")), .SDcols = paste0("end", 1:max(out8$cntvar022)), ]

out9 <- out8 [, c("newstt", "newend", "var02", "var03"),]
out9 <- out9 [ order ( var03)]

fwrite(out9, file = paste(path, "oARFF_P5.0_AfterMedunq_formatted.csv", sep="") )
