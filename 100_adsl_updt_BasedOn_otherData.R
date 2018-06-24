library(Hmisc)
library(data.table)
library(stringi)
library(stringr)

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

dis_pat <- dis_all02 [ nchar(code01) > 0 , 
                      c("mr_no", "patient_id", 
                                      "code01", "code02", "code03",
                                      "text01", "text02", "text03",
                                      "Code", "description")]
dis_pat <- dis_pat[ code01 != c("** Can not be coded")]

dsec011_1 <- merge(x = dsec011,
                   y = lookup_allopathic_diag [dname == "sec011_var001_Allopathic Diagnosis"],
                   all.x = TRUE,
                   allow.cartesian=TRUE,
                   by = c("all_diag", "dname")  )

chkpat(dname = "sec082", var= "sec082_var001_Allopathic Diagnosis", dataout = "dsec082")
dsec082_1 <- merge(x = dsec082,
                   y = lookup_allopathic_diag [dname == "sec082_var001_Allopathic Diagnosis"],
                   all.x = TRUE,
                   allow.cartesian=TRUE,
                   by = c("all_diag", "dname") )


chkpat(dname = "sec122", var= "sec122_var001_Allopathic Diagnosis", dataout = "dsec122")
dsec122_1 <- merge(x = dsec122,
                   y = lookup_allopathic_diag [dname == "sec122_var001_Allopathic Diagnosis"],
                   all.x = TRUE,
                   allow.cartesian=TRUE,
                   by = c("all_diag", "dname") )

chkpat(dname = "sec123", var= "sec123_var001_Allopathic Diagnosis", dataout = "dsec123")
dsec122_1 <- merge(x = dsec123,
                   y = lookup_allopathic_diag [dname == "sec123_var001_Allopathic Diagnosis"],
                   all.x = TRUE,
                   allow.cartesian=TRUE,
                   by = c("all_diag", "dname")  )