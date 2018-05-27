library(survival)
library(survminer)
library(data.table)
library(ggplot2)
library(dplyr)
library(anytime)
library(knitr)
library(DT)

#unq <- data.table(medicine_name = unique(all_met_rmsd$medicine_name))[order(medicine_name)]
#fwrite(unq, "D:/Hospital_data/ProgresSQL/analysis/med_name.csv")

source("D:/Hospital_data/ProgresSQL/prgm/08_nonoverlap.R")

mcallcrt <- function (subset, outfile) {
mcall <- data.table (unique(disease [grepl (paste("^", subset, sep=""), Code), c("Code"), ])) [order(Code)]
mcall0 <- mcall [, z:=1]

unqpat <- data.table( unique( all_met_rmsd [subset == 1, c("mr_no", "patient_gender", "vismon", "Metabolic", "RMSD"), ]))
unqpat <- unqpat [, z:=1]
unqpat02 <- merge(unqpat, mcall0 [, c("z", "Code"),], allow.cartesian = TRUE, by = c("z"))

disease0 <- merge (x = disease [, c("mr_no", "Code", "idurmonth"), ], 
                   y = unqpat02,
                   by = c("mr_no", "Code"),
                   all.y = TRUE,
                   all.x = TRUE)
disease0 <- disease0 [, disdur := ifelse(is.na(idurmonth), vismon, idurmonth), ]
disease0 <- disease0 [, status := ifelse(is.na(idurmonth), 0, 1 )]

mcall <- mcall [, step0001 := paste("#", trimws(Code), sep =" " ),]
mcall <- mcall [, step001 := "```{r, echo = FALSE}", ]
mcall <- mcall [, step01 := paste("tmp <- disease0 [", subset, "== 1 & patient_gender != '' & Code =='", trimws(Code), "'];", sep =""), ]
mcall <- mcall [, step02 := paste("km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);", sep=""), ]
mcall <- mcall [, step03 := paste('p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="', trimws(Code),'");\n', sep =""), ]
mcall <- mcall [, step04 := paste("rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)\n\n```\n"), ]

mcall <- mcall [, step043 := "## Kaplan Meier table\n```{r, echo = FALSE}\nsummary(km_fit, times = durlwr)\n\n```\n", ]
mcall <- mcall [, step044 := "## Survival plot\n```{r, echo = FALSE}\np\n\n```\n", ]
mcall <- mcall [, step05 := "## Hazard ratio plot\n```{r, echo = FALSE}\nrescox;ggforest(rescox, data=tmp)\n\n```\n"]

# This creates mcall.txt file, where the macro call is created
# follwing is one such example

fwrite(mcall [, -c(1, 2),], 
       outfile, 
       row.names = FALSE,
       col.names = FALSE,
       quote = FALSE,
       sep="\n")

#assign(disease0, disease0, envir=.GlobalEnv)

disease0 <<- disease0
}

mcallcrt (subset = quote(Metabolic), outfile = "D:/Hospital_data/ProgresSQL/prgm/call_met.R")

mcallcrt (subset = quote(RMSD), outfile = "D:/Hospital_data/ProgresSQL/prgm/call_rmsd.R")
#########################################################################

