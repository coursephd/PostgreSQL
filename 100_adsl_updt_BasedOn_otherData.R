library(Hmisc)
library(data.table)
library(stringi)
library(stringr)
library(sqldf)

sec001 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/sec001.rds")
sec001_1 <- sec001 [, (names(sec001) %in% c("mr_no", "patient_id", "subvis") | 
                         grepl("^sec", names(sec001)) ), with =FALSE]

lookup <- fread("D:/Hospital_data/ProgresSQL/analysis/lookup.csv", sep=",")
diab001_015 <- paste("\\b", lookup [sec001_var015_diab != "", c(sec001_var015_diab)], "\\b", collapse = "|", sep="")
htn001_015 <- paste("\\b", lookup [sec001_var015_htn != "", c(sec001_var015_htn)], "\\b", collapse = "|", sep="")
rmsd001_015 <- paste("\\b", lookup [sec001_var015_rmsd != "", c(sec001_var015_rmsd)], "\\b", collapse = "|", sep="")

sec001_10 <- sec001_1 [, `:=` ( temp01diab = ifelse (toupper(sec001_var008_Diabetes) %in% lookup$sec001_var008_Diabetes, 0, 1), 
                                temp01htn = ifelse (toupper(sec001_var009_Hypertension) %in% lookup$sec001_var009_Hypertension, 0, 1),
                                temp01renal = ifelse(toupper(`sec001_var011_Renal Diseases`) %in% lookup$`sec001_var011_Renal Diseases`, 0, 1),
                                t001_015_diab = str_detect(`sec001_var015_Chief Complaint with Onset & Duration`, diab001_015),
                                t001_015_htn = str_detect(`sec001_var015_Chief Complaint with Onset & Duration`, htn001_015), 
                                t001_015_rmsd = str_detect(`sec001_var015_Chief Complaint with Onset & Duration`, rmsd001_015) ), ]


sec001_10 [t001_015_diab ==TRUE, .(cnt = uniqueN(mr_no))]
sec001_10 [t001_015_htn ==TRUE, .(cnt = uniqueN(mr_no))]
sec001_10 [t001_015_rmsd ==TRUE, .(cnt = uniqueN(mr_no))]


sec004 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/sec004.rds")
sec004 <- sec004 [, tmp_backchar004_003 := toupper(`sec004_var003_Diagnostic History`)]
sec004 <- sec004 [, tmp_backchar004_005 := toupper(`sec004_var005_Psychological & Occupational History`)]
lookup_backchar004_003 <- fread("D:/Hospital_data/ProgresSQL/analysis/lookup_backchar004_003.txt", sep="|")
lookup_backchar004_005 <- fread("D:/Hospital_data/ProgresSQL/analysis/lookup_backchar004_005.txt", sep="|")

#############################################
# Background characteristics, type of patient
#############################################
sec004_1 <- merge(x = sec004,
                  y = lookup_backchar004_003,
                  all.x = TRUE,
                  by.x = c("tmp_backchar004_003"),
                  by.y = c("sec004_var003_Diagnostic History")  )

sec004_2 <- merge(x = sec004_1,
                  y = lookup_backchar004_005,
                  all.x = TRUE,
                  by.x = c("tmp_backchar004_005"),
                  by.y = c("sec004_var005_Psychological & Occupational History")  )


cnt <- sec004_2 [subchar004_003 != "", .(cnt004_003 = uniqueN(mr_no))]
cnt02 <- sec004_2 [subchar004_005 != "", .(cnt004_003 = uniqueN(mr_no))]

mr <- unique(sec004_2 [subchar004_003 != "", c("mr_no")])
mr02 <- unique(sec004_2 [subchar004_005 != "", c("mr_no")])

###########################################################
# 2768 unique patients have some type of occupation defined
###########################################################
mr_all <- unique(rbind(mr, mr02))


########################################################################
# This section creates the allopathic diagnosis as per ICD 10 dictionary
########################################################################
all_met_rmsd <- readRDS("D:/Hospital_data/ProgresSQL/analysis/01adsl_met_rmsd.rds")

all_met_rmsd <- all_met_rmsd [, `:=` (baseage = min(age)), by =.(mr_no)]
all_met_rmsd <- all_met_rmsd [, `:=` (vismon = round( cdur/30.4375, digits = 0))]

# Baseline age
age01 <- unique( all_met_rmsd [, c("mr_no", "baseage", "patient_gender")] )

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

dis_pat <- dis_all02 [ nchar(code01) > 0 ]
dis_pat <- dis_pat[ code01 != c("** Can not be coded")]

dis_pat02 <- merge (x = dis_pat,
                    y = age01,
                    by = c("mr_no"))

unqpat <- dis_pat02 [, .(npat = uniqueN (mr_no)), by = .(combine)]
unqpat_gender <- dis_pat02 [, .(npat = uniqueN (mr_no)), by = .(combine, patient_gender)]


chk01 <- unique( dis_pat02 [, c("mr_no", "code01", "text01", "baseage", 
                                "patient_gender", "combine", "Metabolic", "RMSD")])

chk01 <- chk01 [, high := substr(code01, 1, 3)]


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

chk02 <- merge (x = chk01,
                y = cats,
                all.x = TRUE,
                by = c("high"))

highcnt <- chk02 [, .(cntpat = uniqueN(mr_no)), by = .(icd)]
