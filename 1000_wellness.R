##############################################
# 
# Wellness score
#
##############################################


library(data.table)
library(openxlsx)
library(tidyverse)

# Read the prakriti data
# Create the prakriti variable
#
# The highest prakriti type is presented first, 
# then the 2nd highest and then the 3rd highest for creating the combinations
#
# A few questions:
# Subset the data for "pitha-vatha-kapha" - there are 71 patients where, Pitha has the highest %, 
# then followed by Vatha and Kapha.
#
# The Pitha % goes from 40.3% to 88% -- but the whole group will be considered as "pitha-vatha-kapha" 
# - is this the correct way of classifying?
#
#  Similar observation for "vatha-pitha-kapha" - Vatha goes from 35% to 68% in this category, 
# but the due to the highest % of vatha, the patients are clubbed into this "vatha-pitha-kapha" category.

prakriti <- read.xlsx("D:\\Hospital_data\\WellnessScore\\data\\Prakriti-Data-Sep22.xlsx",
                      sheet=2,
                      startRow = 1)
prakriti <- as.data.table (prakriti)

prakriti <- prakriti [, nrow := .N, by = .(PatientId)]
prakriti02 <- prakriti [ nrow <= 3]
prakriti02 <- prakriti02 [ order(PatientId, -section_score_percentage)]

prakriti03 <- prakriti02 [, .(ptype = paste(prakruti_value, collapse="-", sep=""),
                              pcent = paste(section_score_percentage, collapse = "-", sep="")), by = .(PatientId, Gender, age, Country)]

prakriti04 <- prakriti03 [, .(numpat = uniqueN(PatientId)), by = .(ptype)]
prakriti05 <- prakriti03 [, .(numpat = uniqueN(PatientId)), by = .(ptype, Gender)]


# Read the % score for each of the category and transpose the data

perscr <- read.xlsx("D:\\Hospital_data\\WellnessScore\\data\\Survey_pgm.xlsx",
                      sheet=5,
                      startRow = 1)
perscr <- as.data.table (perscr)
perscr02 <- melt(data = perscr, 
                 id.vars = c("PatientID", "AssessmentName"),
                 measure.vars = c( "Adharaniya.Vega", "Agni", "Dharaniya.Vega", "Kostha",                  
                                   "Mala", "Manas", "Nidra", "Niramata",  
                                   "Vyadhi.Kshamatava.&.Ojas", "Vyayama", "Total"  ) )                

prkper01 <- merge(x = prakriti03,
                  y = perscr02,
                  by.x = c("PatientId"),
                  by.y = c("PatientID"),
                  all = TRUE)                  

prkper01 <- prkper01 [, agecat := case_when( age > 0 & age <= 18 ~ "0 - 18 years",
                                             age > 18 & age <= 35 ~ "18 - 35 years",
                                             age > 36 & age <= 50 ~ "36 - 50 years", 
                                             age > 50 & age <= 70 ~ "50 - 70 years", 
                                             age > 70 ~ "70 years and above", 
                                             TRUE ~ "Not captured"), ]

fwrite(prkper01, "D:\\Hospital_data\\WellnessScore\\analysis\\1000_Prakriti-type-cal.csv")
