library(data.table)
library(tidyverse)
library(cumstats)

#all_met_rmsd02 <- fread("C://Users//mahajvi1//Downloads//01adsl_met_rmsd.csv")
all_met_rmsd02 <- readRDS("D:/Hospital_data/ProgresSQL/analysis/01adsl_met_rmsd.rds")
all_met_rmsd02 <- all_met_rmsd02 [, Code := ifelse (Code == " " | Code == "", "** Not yet coded", Code),]
all_met_rmsd02 <- all_met_rmsd02 [, description:= ifelse (description == "" | description ==" ", "** Not yet coded", description),]
all_met_rmsd02 <- all_met_rmsd02 [, Code02 := paste(distype, ":", Code, ":", description, sep =""), ]
all_met_rmsd02 <- all_met_rmsd02 [, Med02 := paste(Type_med, ":", Coded_med, sep =""), ]

all_met_rmsd03 <- unique(all_met_rmsd02 [, c("mr_no", "studyday", "vis", "all_vis", "cdur", 
                                             "Code", "description", "Code02",
                                             "Type_med", "Coded_med", "Med02")] )

###########################################################
# Take a look at rasa aushadhi's
# Currently subset for the treatments identified as bhasma
# But take a look at this subset based on the new sheet
# provided by Prasan
# Take Bhasma date as the reference date and see the
# before and after period for patients
###########################################################

medcat0 <- unique(all_met_rmsd03 [tolower(Med02) %like% "bhasma", c("mr_no", "cdur"),] )
all_met_rmsd04 <- all_met_rmsd03 [ mr_no %in% medcat0$mr_no]

all_met_rmsd04 <- all_met_rmsd04 [, bhasma := ifelse( tolower(Med02) %like% "bhasma", "Bhasma", "Non-Bhasma" )]
bhasma <- all_met_rmsd04 [bhasma == "Bhasma", .(bhasmamax = max(studyday),
                             bhasmamin = min(studyday)), by = .(mr_no, cdur)]

nonbhasma <- all_met_rmsd04 [bhasma == "Non-Bhasma", .(nobhasmamax = max(studyday),
                                               nobhasmamin = min(studyday)), by = .(mr_no, cdur)]

both <- merge (x = bhasma,
               y = nonbhasma,
               by = c("mr_no", "cdur"),
               all.x = TRUE,
               all.y = TRUE)

##########################################################################
# Calculate the duration of period after bhasma has been administered
# Calculate the duration of period before bhasma has been administered
##########################################################################

both <- both [, `:=` (prebhasmadur = bhasmamin - 1,
                      postbhasmadur = cdur - bhasmamin + 1), ]

all_met_rmsd05 <- merge(x = all_met_rmsd04,
                        y = both,
                        by = c("mr_no", "cdur"), 
                        all.y = TRUE )

all_met_rmsd05 <- all_met_rmsd05 [, time := ifelse(studyday <= bhasmamin, "Pre_Bhasma", "Post_Bhasma"),]

#####################################################################################
# Calculate number of pre and post bhasma unique diseases reported by each patient
#####################################################################################
ndis <- all_met_rmsd05 [, .(numdis = uniqueN (Code02)), by = .(time, mr_no)]
ndis_tr <- dcast (data = ndis,
                  mr_no ~ time,
                  value.var = c("numdis"),
                  fill = "0")

#####################################################################################
# Calculate number of pre and post bhasma unique meds prescribed for each patient
#####################################################################################
nmed <- all_met_rmsd05 [, .(nummed = uniqueN (Med02)), by = .(time, mr_no)]
nmed_tr <- dcast (data = nmed,
                  mr_no ~ time,
                  value.var = c("nummed"),
                  fill = "0")

