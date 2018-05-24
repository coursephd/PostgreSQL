library(survival)
library(survminer)
library(data.table)
library(ggplot2)
library(dplyr)
library(anytime)
library(knitr)
library(DT)

unq <- data.table(medicine_name = unique(all_met_rmsd$medicine_name))[order(medicine_name)]
unq <- unq [, med_change := medicine_name]

fwrite(unq, "D:/Hospital_data/ProgresSQL/analysis/med_name.csv")

source("D:/Hospital_data/ProgresSQL/prgm/08_nonoverlap.R")

mcall <- data.table (unique(disease [Code != "", c("Code"), ])) [order(Code)]
mcall <- mcall [, step01 := paste("tmp <- disease [patient_gender != '' & Code =='", trimws(Code), "'];", sep =""), ]
mcall <- mcall [, step02 := paste("km_fit <- survfit(Surv(idurmonth) ~ patient_gender, data= tmp);", sep=""), ]
mcall <- mcall [, step03 := paste('ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="', trimws(Code),'");', sep =""), ]
mcall <- mcall [, step04 := paste("rescox <- coxph(Surv(idurmonth) ~ patient_gender, data= tmp);rescox;ggforest(rescox, data=tmp);\n"), ]

# This creates mcall.txt file, where the macro call is created
# follwing is one such example

fwrite(mcall [, -c(1),], "D:/Hospital_data/ProgresSQL/prgm/mcall.txt", 
       row.names = FALSE,
       col.names = FALSE,
       quote = FALSE,
       sep="\n")


tmp <- disease [patient_gender != "" & grepl("^Metabolic", Code)]

ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = FALSE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE
)


```{r}

unqdis <- unique(disease$Code)

for (dis in unqdis[1:3]) {
  
  print(dis)
  
  tmp <- disease [Code == dis & patient_gender != ""]
  tmp <- tmp [, status:=1]
  #km_fit <- do.call(survfit, 
  list(formula = Surv(idurmonth, status ==1) ~ patient_gender, data= tmp))

#ggsurvplot(fit = km_fit, data =tmp)

#str(km_fit)
#summary(km_fit)

#print(km_fit, print.rmean = TRUE)
#quantile(km_fit)

}


lung_fit<- survfit(Surv(time, status) ~ sex, data = lung)

# Drawing survival curves
ggsurvplot(lung_fit, data = lung)

ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = FALSE,        # Add risk table
           risk.table.height = 0.3, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE
)

```


## R Markdown


