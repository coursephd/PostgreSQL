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


chk01 <- unique( dis_pat02 [studyday > 0, c("mr_no", "code01", "text01", "baseage", "studyday",
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

chk02 <- chk02 [, minday := min(studyday), by = .(mr_no, icd, V5)]
chk02min <- chk02 [ minday == studyday]

acd <- unique( all_met_rmsd [studyday > 0 & nchar(description) > 0, 
                             c("mr_no", "studyday", "Code", "description", "baseage", 
                               "patient_gender", "combine", "Metabolic", "RMSD", 
                               "cdur", "all_vis") ])

acd02 <- merge(x = acd ,
               y = chk02min [, c("mr_no", "icd", "minday", "V5", "high", "code01", "text01"), ],
               all = TRUE,
               allow.cartesian = TRUE,
               by = c("mr_no")  )

acd03 <- acd02 [ i = minday > studyday, 
                 j = c("icd", "minday", "V5", "high", "code01", "text01") := NA]

madhumeha <- unique( all_met_rmsd [ Code %in% c("M2.0") & diag_type == "P", c("mr_no"), ])
all_madhu <- chk02min [ mr_no %in% madhumeha$mr_no]

gridh <- unique( all_met_rmsd [ Code %in% c("V2.23") & diag_type == "P", c("mr_no"), ])
all_gridh <- chk02min [ mr_no %in% gridh$mr_no]

######################################################################
# End of program
######################################################################