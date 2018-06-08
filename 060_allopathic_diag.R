library(dplyr)
library(data.table)
library(fuzzyjoin)
library(stringr)
library(stringi)
library(stringdist)
library(quanteda)
library(tm)
library(tidyr)


chkpat <- function (dname, var, dataout = "unqsec") {
  sec <- readRDS( paste("D:/Hospital_data/ProgresSQL/analysis/", noquote(dname) , ".rds", sep="") )
  
  
  sec011 <- sec [, `:=` (orig = get(var),
                         all_diag = toupper( get( var))), ]
  
  ########################################################
  # Replace multiple diseases in 1 row to multiple rows, 
  # Seperate_rows allows: keeping all other rows as is
  #########################################################
  sec011_1 <- separate_rows(sec011, all_diag, sep =",|\r\n|\\?|\\bAND\\b|;" )
  sec011_1 <- sec011_1 [, all_diag := trimws(all_diag),]
  sec011_1 <- sec011_1 [, dname := paste(var, sep = ""), ]
  
  ####################################################################
  # Create unique row per disease, this will be matched against ICD 10
  ####################################################################
  unqsec <- unique( sec011_1 [, c("all_diag", "dname"), ] )
  
  assign(dataout, unqsec, envir=.GlobalEnv)
}

chkpat(dname = "sec011", var= "sec011_var001_Allopathic Diagnosis", dataout = "unqsec011")
chkpat(dname = "sec082", var= "sec082_var001_Allopathic Diagnosis", dataout = "unqsec082")
chkpat(dname = "sec122", var= "sec122_var001_Allopathic Diagnosis", dataout = "unqsec122")
chkpat(dname = "sec123", var= "sec123_var001_Allopathic Diagnosis", dataout = "unqsec123")

secall <- lapply(ls(pattern="unqsec*"), get)
t2all_unqdis <- data.table( rbindlist (secall))

fwrite(t2all_unqdis, "D:/Hospital_data/ProgresSQL/data_chk/_unq_cnt_allopathic_dist.csv")


##########################
# Read the ICD dictionary
##########################
icd10 <- fread("D:/Hospital_data/ProgresSQL/analysis/icd10cm_codes_2018.txt", sep="\t", header= FALSE)
icd10_1 <- icd10[, c("VAR1","VAR5") := tstrsplit(stri_replace_first_fixed(V1, " ", "<>"), "<>", fixed=TRUE), ]
icd10_1 <- icd10_1 [, VAR55 :=toupper(V1)]

icd10high <- icd10_1 [ nchar(VAR1) <= 4]

c <- c("ANKYLOSING SPONDYLITIS")

chk <- icd10high [ str_detect(VAR55, c)]



icd10 <- fread("D:/Hospital_data/ProgresSQL/analysis/icd10cm_order_2018.csv", header= FALSE)
icd10high <- icd10 [ V3 == 0]
  
c <- c("ANKYLOSING SPONDYLITIS")
c <- c("CERVICAL")
c <- c("BRONCHITIS")
c <- c("RHINITIS")
c <- c("RHEUM")
c <- c("OSTEO")
c <- c("OBES")
c <- c("VARICOSE")

chk <- icd10 [ str_detect( toupper(V4), c)]
View(chk)



icd_const <- icd10_1[ VAR55 %like% c("WEIGHT") | VAR55 %like% c("COLD") ]
icd_const <- icd10_1[ VAR1 %like% c("E663") | VAR1 %like% c("J00") ]

row_all <- fread("C:/Users/mahajvi1/Desktop/mydata.txt",
                 sep="\t",
                 header= FALSE)

row_all0 <- row_all[, c("VAR1","VAR2", "VAR3", "VAR4", "VAR5", "VAR6") := tstrsplit(stri_trim(V1), "@", fixed=TRUE), ]
row_all01 <- row_all0 [ VAR4 %in% c('ASSOCIATED SIGNS SYMPTOMS', 'ADDITIONAL COMPLAINT', 'ALLERGIC_IMMUNOLOGIC', 'ASSOCIATED SYMPTOMS', 'HISTORY OF PAST ILLNESS', 'OTHER COMPLAINT', 'HEMOTOLOGICAL_LYMPHATIC', 'MAIN COMPLAINTS', 'PAST HISTORY', 'ASSOCIATED_ILLNESS_UPASHAYA_ANUPASHAYA', 'PSYCHOLOGICAL OCCUPATIONAL HISTORY', 'HISTORY OF PRESENT ILLNESS', 'CHIEF COMPLAINT DURATION', 'FAMILY HISTORY', 'SURGICAL HISTORY', 'DIAGNOSTIC HISTORY', 'ALLOPATHIC DIAGNOSIS', 'ASSOCIATED COMPLAINT WITH ONSET DURATION', 'CHIEF COMPLAINT', 'MEDICAL HISTORY', 'CHIEF COMPLAINT WITH ONSET DURATION', 'COMPLAINT' )]
row_all02 <- row_all01[, .(V1, VAR1, VAR2, VAR3, VAR4, VAR5, VAR6,
                           splitted =unlist(strsplit(VAR5, "[[:punct:]]", perl=TRUE))) ,
                       by=seq_len(nrow(row_all01))]

med_rm <- fread("C:/Users/mahajvi1/Desktop/medical_remove.txt")
med_rm <- med_rm [ , V30 := paste("\\b", str_trim(word), collapse="\\b|", sep="")]
med_rm2 <- unique(med_rm$V30)

row_all03 <- row_all02 [, VAR500 := gsub(med_rm2, "<>", splitted),]
row_all04 <- row_all03[, .(V1, VAR1, VAR2, VAR3, VAR4, VAR5, VAR6,
                           VAR55 =unlist(strsplit(VAR500, "<>", perl=TRUE))) ,
                       by=seq_len(nrow(row_all03))]

row_all04 <- row_all04 [VAR55 != " "]
row_const <- row_all04[ VAR55 %like% c("WEIGHT") | VAR55 %like% c("COLD")]


try <- stringdist_inner_join(x = row_const,
                             y = icd_const,
                             by = "VAR55",
                             ignore_case = TRUE,
                             method = c("jw") ,
                             max_dist=.4),
                             distance_col = "dist")

setnames(try, "VAR55.y", "VAR55_new")

try1 <- try [order(try$seq_len, try$VAR55_new ) ]
try2 <- try1 [, disgrp := 1:.N, by =.(seq_len, VAR55_new)]
try20 <- try2 [ disgrp <= 15]
try3 <- dcast (data=try20,
               seq_len + VAR4 + VAR55.x ~ paste( "dis", str_pad(disgrp, 3, side = "left", pad = 0), sep=""),
               fill=" ",
               value.var = c("VAR55.y"))


try_osa <- amatch (x = row_const,
                   table = icd_const,
                   method = "osa",
                   maxDist = 5)


# O observations
try_osa <- stringdist_inner_join(x = row_const,
                                 y = icd_const,
                                 by = "VAR55",
                                 ignore_case = TRUE,
                                 method = c("osa"), max_dist = 20)



# O observations
try_lv <- stringdist_inner_join(x = row_const,
                                y = icd_const,
                                by = "VAR55",
                                ignore_case = TRUE,
                                method = c("lv"))

# O observations
try_lcs <- stringdist_inner_join(x = row_const,
                                 y = icd_const,
                                 by = "VAR55",
                                 ignore_case = TRUE,
                                 method = c("lcs"))

# O observations
try_qgram <- stringdist_inner_join(x = row_const,
                                   y = icd_const,
                                   by = "VAR55",
                                   ignore_case = TRUE,
                                   method = c("qgram"))


# O observations
try_cosine <- stringdist_inner_join(x = row_const,
                                    y = icd_const,
                                    by = "VAR55",
                                    ignore_case = TRUE,
                                    method = c("cosine"))

