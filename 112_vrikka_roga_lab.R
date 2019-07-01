library(data.table)
library(tidyverse)
library(cumstats)
library(stringi)

#####################
# Get the lab values
#####################

othv16 <- readRDS("D:/Hospital_data/ProgresSQL/analysis_ckd//sec013.rds")
setnames(othv16, "sec013_var001_Investigation Reports", "sec013_var001_InvestigationReports")

othv16_1 <- na.omit(othv16, cols = "sec013_var001_InvestigationReports" )

strfnd <- "creat|crreatini|cea"

crea <- othv16_1 [ grep (strfnd, tolower(sec013_var001_InvestigationReports) ) ]
crea_no <- othv16_1 [! grep (strfnd, tolower(sec013_var001_InvestigationReports) )  ]

#####################################################################################
# Do not run the code, the file has been modified manually for HGB, POT, CREA values
#####################################################################################

# fwrite(crea, "D:/Hospital_data/ProgresSQL/analysis_ckd/001sec013crea_value_chk.csv")

###########################################
# Get the diabetes and heart disease status
# Renal disease
###########################################

strfnd2 <- "absent|nil|no"

othv001 <- readRDS("D:/Hospital_data/ProgresSQL/analysis_ckd//sec001.rds")

othv001_1 <- na.omit(othv001, cols = "sec001_var001_Diabetes" )
othv001_1 <- othv001_1 [ ! grep (strfnd2, tolower(sec001_var001_Diabetes)) ]
othv001_10 <- unique( othv001_1 [ , c("mr_no"), ])
othv001_10 <- othv001_10 [, diabetes := 1, ]

othv001_2 <- na.omit(othv001, cols = "sec001_var002_Hypertension" )
othv001_2 <- othv001_2 [ ! grep (strfnd2, tolower(sec001_var002_Hypertension)) ]
othv001_20 <- unique( othv001_2 [ , c("mr_no"), ])
othv001_20 <- othv001_20 [, htn := 1, ]

othv001_4 <- na.omit(othv001, cols = "sec001_var004_Renal Diseases" )
othv001_4 <- othv001_4 [ ! grep (strfnd2, tolower(`sec001_var004_Renal Diseases`)) ]
othv001_40 <- unique( othv001_4 [ , c("mr_no"), ])
othv001_40 <- othv001_40 [, renal := 1, ]


##################################################
# Get the patients with diabetes and heart disease
##################################################

adsl <- readRDS("D:/Hospital_data/ProgresSQL/analysis//01adsl_vrikka.rds")

adsl1 <- unique ( adsl [ Code %in% c("M2.0", "P5.0", "P5.2", "P5.5"), c("mr_no"), ])
adsl1 <- adsl1 [, diabetes := 1, ]
all_diab <- unique( rbind ( adsl1, othv001_10) )

adsl2 <- unique ( adsl [ Code %in% c("H2.0", "H2.4", "MS R1"), c("mr_no"), ])
adsl2 <- adsl2 [, htn := 1, ]
all_htn <- unique( rbind ( adsl2, othv001_20) )

adsl3 <- unique ( adsl [ Code %in% c("M12.0", "M13.0", "M14.0", "M16.9"), c("mr_no"), ])
adsl3 <- adsl3 [, renal := 1, ]
all_renal <- unique( rbind ( adsl3, othv001_40) )


lab <- fread("D:/Hospital_data/ProgresSQL/analysis_ckd//001sec013crea_value_chk.csv")
lab <- lab [, CREA := as.numeric(CREA),]
lab <- lab [, POT := as.numeric(POT),]
lab <- lab [, HGB := as.numeric(HGB),]


#####################################
# Merge single or multiple diseases
#####################################
patcat <- fread("D:\\Hospital_data\\ProgresSQL\\analysis\\105_trt_dis_unq_mult01vrikka_roga.csv")
patcat2 <- unique( patcat [, c("mr_no", "discat"),])

all_met_rmsd <- readRDS("D:/Hospital_data/ProgresSQL/analysis/01adsl_vrikka.rds")
all_met_rmsd02 <- unique ( all_met_rmsd [, c("mr_no", "patient_gender", "baseage", "mindayVrikkaRoga"), ])

all_met_rmsd02 <- merge (x = all_met_rmsd02,
                         y = patcat2, 
                         by = c("mr_no"), 
                         all.x = TRUE )

lab <- merge (x = all_met_rmsd02,
              y = lab, 
              by = c("mr_no"), 
              all.y = TRUE )
lab <- lab [, refday := as.numeric(studyday - mindayVrikkaRoga + 1), ]
lab <- lab [, refper := ifelse ( refday <=1, "01 Pre-baseline", "02 Post-baseline"), ]

chk <- unique( lab [refday < 0, c("mr_no", "refday"),] )

#summary(lab$CREA); uniqueN( lab [CREA > 0, c("mr_no"), ] )
#summary(lab$POT); uniqueN( lab [POT > 0, c("mr_no"), ] )
#summary(lab$HGB); uniqueN( lab [HGB > 0, c("mr_no"), ] )

t01crea <- lab [CREA > 0, .(n = uniqueN(mr_no),
                   mean = mean(CREA, na.rm = FALSE),
                   sd = sd(CREA, na.rm = FALSE),
                   median = median (CREA, na.rm = FALSE),
                   min = min (CREA, na.rm = FALSE),
                   max = max (CREA, na.rm = FALSE)) ] 

t01crea_per <- lab [CREA > 0, .(n = uniqueN(mr_no),
                            mean = mean(CREA, na.rm = FALSE),
                            sd = sd(CREA, na.rm = FALSE),
                            median = median (CREA, na.rm = FALSE),
                            min = min (CREA, na.rm = FALSE),
                            max = max (CREA, na.rm = FALSE)),
                by = .(refper)] 

t01crea_gen <- lab [CREA > 0, .(n = uniqueN(mr_no),
                            mean = mean(CREA, na.rm = FALSE),
                            sd = sd(CREA, na.rm = FALSE),
                            median = median (CREA, na.rm = FALSE),
                            min = min (CREA, na.rm = FALSE),
                            max = max (CREA, na.rm = FALSE)), 
                , by = .(patient_gender)] 

t01crea_discat <- lab [CREA > 0, .(n = uniqueN(mr_no),
                                mean = mean(CREA, na.rm = FALSE),
                                sd = sd(CREA, na.rm = FALSE),
                                median = median (CREA, na.rm = FALSE),
                                min = min (CREA, na.rm = FALSE),
                                max = max (CREA, na.rm = FALSE)), 
                    , by = .(discat)] 


t01pot <- lab [POT > 0, .(n = uniqueN(mr_no),
                            mean = mean(POT, na.rm = FALSE),
                            sd = sd(POT, na.rm = FALSE),
                            median = median (POT, na.rm = FALSE),
                            min = min (POT, na.rm = FALSE),
                            max = max (POT, na.rm = FALSE)) ] 

t01hgb <- lab [HGB > 0, .(n = uniqueN(mr_no),
                            mean = mean(HGB, na.rm = FALSE),
                            sd = sd(HGB, na.rm = FALSE),
                            median = median (HGB, na.rm = FALSE),
                            min = min (HGB, na.rm = FALSE),
                            max = max (HGB, na.rm = FALSE)) ] 

