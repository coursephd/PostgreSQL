library(dplyr)
library(data.table)
library(fuzzyjoin)
library(stringr)
library(stringi)
library(stringdist)
library(quanteda)
library(tm)
library(tidyr)
library(sqldf)

########################################################################
# This section creates the allopathic diagnosis as per ICD 10 dictionary
########################################################################
all_met_rmsd <- readRDS("D:/Hospital_data/ProgresSQL/analysis/01adsl_met_rmsd.rds")

all_met_rmsd <- all_met_rmsd [, `:=` (baseage = min(age)), by =.(mr_no)]
all_met_rmsd <- all_met_rmsd [, `:=` (vismon = round( cdur/30.4375, digits = 0))]

# Baseline age
age01 <- unique( all_met_rmsd [, c("mr_no", "baseage", "patient_gender", "cdur")] )

lookup_allopathic_diag <- fread("D:/Hospital_data/ProgresSQL/analysis/lookup_allopathic_diag.txt", sep="|")

chkpat <- function (dname, var, dataout = "unqsec") {
  sec <- readRDS( paste("D:/Hospital_data/ProgresSQL/analysis/", noquote(dname) , ".rds", sep="") )
  
  
  sec011 <- sec [, `:=` (orig = get(var),
                         all_diag = toupper( get( var))), ]
  
  ########################################################
  # Replace multiple diseases in 1 row to multiple rows, 
  # Seperate_rows allows: keeping all other rows as is
  #########################################################
  sec011_1 <- separate_rows(sec011, all_diag, sep =",|\r\n|\\?|\\bAND\\b|;" )
  #sec011_1 <- sec011_1 [, all_diag := trimws(all_diag), ]
  #sec011_1 <- sec011_1 [, dname := paste(var, sep = ""), ]
  
  sec011_1 [, all_diag := trimws(all_diag)]
  sec011_1 [, dname := paste(var, sep = "")]
  
  ####################################################################
  # Create unique row per disease, this will be matched against ICD 10
  ####################################################################
  #unqsec <- unique( sec011_1 [, c("all_diag", "dname"), ] )
  tmp <- sec011_1
  
  assign(dataout, tmp, envir=.GlobalEnv)
}

chkpat(dname = "sec011", var= "sec011_var001_Allopathic Diagnosis", dataout = "dsec011")
chkpat(dname = "sec082", var= "sec082_var001_Allopathic Diagnosis", dataout = "dsec082")
chkpat(dname = "sec122", var= "sec122_var001_Allopathic Diagnosis", dataout = "dsec122")
chkpat(dname = "sec123", var= "sec123_var001_Allopathic Diagnosis", dataout = "dsec123")

dislist <- lapply(ls(pattern="dsec*"), get)
dis_all <- data.table( rbindlist (dislist))

dis_all02 <- merge(x = dis_all,
                   y = lookup_allopathic_diag,
                   all.x = TRUE,
                   allow.cartesian=TRUE,
                   by = c("all_diag", "dname")  )

##########################
# Subset for coded records
##########################
dis_pat <- dis_all02 [ nchar(code01) > 0 ]
dis_pat <- dis_pat[ code01 != c("** Can not be coded")]

dis_pat02 <- merge (x = dis_pat,
                    y = age01,
                    by = c("mr_no"))

unqpat <- dis_pat02 [, .(npat = uniqueN (mr_no)), by = .(combine)]
unqpat_gender <- dis_pat02 [, .(npat = uniqueN (mr_no)), by = .(combine, patient_gender)]


chk01 <- unique( dis_pat02 [, c("mr_no", "code01", "text01", "baseage", 
                                "patient_gender", "combine", "Metabolic", "RMSD", "cdur", "all_vis")])

chk01 <- chk01 [, high := substr(code01, 1, 3)]

#################
# ICD dictionary
#################
icd10 <- fread("D:/Hospital_data/ProgresSQL/analysis/icd10cm_order_2018.csv", header= FALSE)
cats <- data.table( expand.grid( cat1 = LETTERS, 
                                 cat2 = seq (0, 99) ) )
cats <- cats [, high := paste( cat1, str_pad(cat2, 2, side = "left", pad = 0), sep=""), ]

cats <- sqldf("select *, 
              case 
              When cat1 == 'A' OR cat1 == 'B' then 'Certain infectious and parasitic diseases'
              When (cat1 == 'C' OR (cat1 == 'D' AND cat2 < 50)) then 'Neoplasms'
              When (cat1 == 'D' AND cat2 >=50) then 'Diseases of the blood and blood-forming organs and certain disorders involving the immune mechanism'
              When (cat1 == 'E') then 'Endocrine, nutritional and metabolic diseases'
              When (cat1 == 'F') then 'Mental and behavioural disorders'
              When (cat1 == 'G') then 'Diseases of the nervous system'
              When (cat1 == 'H' and cat2 <= 59) then 'Diseases of the eye and adnexa'
              When (cat1 == 'H' and cat2 > 59) then 'Diseases of the ear and mastoid process'
              When (cat1 == 'I') then 'Diseases of the circulatory system'
              When (cat1 == 'J') then 'Diseases of the respiratory system'
              When (cat1 == 'K') then 'Diseases of the digestive system'
              When (cat1 == 'L') then 'Diseases of the skin and subcutaneous tissue'
              When (cat1 == 'M') then 'Diseases of the musculoskeletal system and connective tissue'
              When (cat1 == 'N') then 'Diseases of the genitourinary system'
              When (cat1 == 'O') then 'Pregnancy, childbirth and the puerperium'
              When (cat1 == 'P') then 'Certain conditions originating in the perinatal period'
              When (cat1 == 'Q') then 'Congenital malformations, deformations and chromosomal abnormalities'
              When (cat1 == 'R') then 'Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified'
              When (cat1 == 'S' OR cat1 == 'T') then 'Injury, poisoning and certain other consequences of external causes'
              When (cat1 == 'V' OR cat1 == 'W' OR cat1 == 'X' OR cat1 == 'Y') then 'Injury, poisoning and certain other consequences of external causes'
              When (cat1 == 'Z') then 'Factors influencing health status and contact with health services'
              When (cat1 == 'U') then 'Codes for special purposes'
              end as icd
              from cats")

#############################################
# Get the 2nd level terms from ICD dictionary
#############################################
icd_sub <- icd10 [ V2 %in% unique (chk01$high)]

##############################################################
# Merge the high level terms and 2nd level terms with the data
##############################################################
chk02 <- merge (x = chk01,
                y = cats,
                all.x = TRUE,
                by = c("high"))

chk02 <- merge (x = chk02,
                y = icd_sub [, c("V2", "V5")],
                all.x = TRUE,
                by.x = c("high"),
                by.y = c("V2"))

fwrite(chk02, 
       "D:/Hospital_data/ProgresSQL/analysis/060_allopathic_diag.csv")

high01 <- chk02 [, .(highcnt = uniqueN(mr_no)), by = .(icd)]
high02 <- chk02 [, .(cntpat = uniqueN(mr_no)), by = .(icd, V5)]

high_all <- merge (x = high01, 
                   y = high02,
                   by = c("icd"))

high01_com <- chk02 [, .(highcnt = uniqueN(mr_no)), by = .(combine, icd)]
high02_com <- chk02 [, .(cntpat = uniqueN(mr_no)), by = .(combine, icd, V5)]

high_com_all <- merge (x = high01_com, 
                   y = high02_com,
                   by = c("combine","icd"))

###################################
# End of program 
###################################


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
c <- c("SPONDYLOSIS", "SPONDYLITIS")
c <- c("ACHILLES")
c <- c("ACUTE PANCREATITIS")
c <- c("POLYARTHRALGIA")
c <- c("DIABETE")
c <- c("BAKER")
c <- c("CARPAL")
c <- c("QUERV")
c <- c("GOUT")
c <- c("BARRE")
c <- c("BACK")
c <- c("PARKINSON")
c <- c("HYPERTENSION")
c <- c("CRAMP")
c <- c("abscess")

c2 <- paste(c, collapse="|", sep="")

chk <- icd10 [ str_detect( toupper(V4), toupper(c2)) | 
               str_detect( toupper(V5), toupper(c2))   ]
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

