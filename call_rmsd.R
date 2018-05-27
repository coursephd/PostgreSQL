# RMSD : A2.0 Aamavaata
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : A2.0 Aamavaata'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : A2.0 Aamavaata");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : A2.1 Aamavaata - Kaphaja
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : A2.1 Aamavaata - Kaphaja'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : A2.1 Aamavaata - Kaphaja");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : A2.2 Aamavaata - Pittaja
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : A2.2 Aamavaata - Pittaja'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : A2.2 Aamavaata - Pittaja");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : A2.3 Aamavaata - Vaataja
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : A2.3 Aamavaata - Vaataja'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : A2.3 Aamavaata - Vaataja");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : A3.0 Abhighataja Shoola
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : A3.0 Abhighataja Shoola'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : A3.0 Abhighataja Shoola");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : S10.0 Stambha
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : S10.0 Stambha'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : S10.0 Stambha");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : S10.1 Stambha - Baahu Stambha
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : S10.1 Stambha - Baahu Stambha'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : S10.1 Stambha - Baahu Stambha");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : S10.10 Stambha - Prishtha Stambha
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : S10.10 Stambha - Prishtha Stambha'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : S10.10 Stambha - Prishtha Stambha");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : S10.12 Stambha - Sandhi Stambha
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : S10.12 Stambha - Sandhi Stambha'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : S10.12 Stambha - Sandhi Stambha");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : S10.13 Stambha - Siraa Stambha
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : S10.13 Stambha - Siraa Stambha'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : S10.13 Stambha - Siraa Stambha");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : S10.14 Stambha - Uru Stambha
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : S10.14 Stambha - Uru Stambha'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : S10.14 Stambha - Uru Stambha");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : S10.4 Stambha - Greevaa Stambha
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : S10.4 Stambha - Greevaa Stambha'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : S10.4 Stambha - Greevaa Stambha");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : S10.5 Stambha - Hanu Stambha
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : S10.5 Stambha - Hanu Stambha'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : S10.5 Stambha - Hanu Stambha");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : S10.6 Stambha - Hridaya Stambha
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : S10.6 Stambha - Hridaya Stambha'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : S10.6 Stambha - Hridaya Stambha");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : S13.0 Sthaanabhedena Graha
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : S13.0 Sthaanabhedena Graha'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : S13.0 Sthaanabhedena Graha");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : S13.1 Sthaanabhedena Graha - Anga Graha
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : S13.1 Sthaanabhedena Graha - Anga Graha'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : S13.1 Sthaanabhedena Graha - Anga Graha");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : S13.11 Sthaanabhedena Graha - Katee Graha
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : S13.11 Sthaanabhedena Graha - Katee Graha'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : S13.11 Sthaanabhedena Graha - Katee Graha");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : S13.13 Sthaanabhedena Graha - Manyaa Graha
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : S13.13 Sthaanabhedena Graha - Manyaa Graha'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : S13.13 Sthaanabhedena Graha - Manyaa Graha");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : S13.14 Sthaanabhedena Graha - Marma Graha
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : S13.14 Sthaanabhedena Graha - Marma Graha'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : S13.14 Sthaanabhedena Graha - Marma Graha");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : S13.17 Sthaanabhedena Graha - Paada Graha
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : S13.17 Sthaanabhedena Graha - Paada Graha'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : S13.17 Sthaanabhedena Graha - Paada Graha");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : S13.18 Sthaanabhedena Graha - Paarshva Graha
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : S13.18 Sthaanabhedena Graha - Paarshva Graha'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : S13.18 Sthaanabhedena Graha - Paarshva Graha");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : S13.19 Sthaanabhedena Graha - Prishtha Graha
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : S13.19 Sthaanabhedena Graha - Prishtha Graha'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : S13.19 Sthaanabhedena Graha - Prishtha Graha");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : S13.20 Sthaanabhedena Graha - Shiro Graha
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : S13.20 Sthaanabhedena Graha - Shiro Graha'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : S13.20 Sthaanabhedena Graha - Shiro Graha");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : S13.22 Sthaanabhedena Graha - Uro Graha
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : S13.22 Sthaanabhedena Graha - Uro Graha'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : S13.22 Sthaanabhedena Graha - Uro Graha");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : S13.23 Sthaanabhedena Graha - Vaak Graha
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : S13.23 Sthaanabhedena Graha - Vaak Graha'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : S13.23 Sthaanabhedena Graha - Vaak Graha");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : S13.3 Sthaanabhedena Graha - Gala Graha
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : S13.3 Sthaanabhedena Graha - Gala Graha'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : S13.3 Sthaanabhedena Graha - Gala Graha");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : S13.5 Sthaanabhedena Graha - Hanu Graha
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : S13.5 Sthaanabhedena Graha - Hanu Graha'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : S13.5 Sthaanabhedena Graha - Hanu Graha");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : S13.6 Sthaanabhedena Graha - Hrid Graha
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : S13.6 Sthaanabhedena Graha - Hrid Graha'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : S13.6 Sthaanabhedena Graha - Hrid Graha");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : S13.7 Sthaanabhedena Graha - Jaanugraha
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : S13.7 Sthaanabhedena Graha - Jaanugraha'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : S13.7 Sthaanabhedena Graha - Jaanugraha");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : S13.8 Sthaanabhedena Graha - Janghaa Graha
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : S13.8 Sthaanabhedena Graha - Janghaa Graha'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : S13.8 Sthaanabhedena Graha - Janghaa Graha");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : S14.0 Sthaanabhedena Shoola
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : S14.0 Sthaanabhedena Shoola'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : S14.0 Sthaanabhedena Shoola");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : S14.11 Sthaanabhedena Shoola - Guda Shoola
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : S14.11 Sthaanabhedena Shoola - Guda Shoola'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : S14.11 Sthaanabhedena Shoola - Guda Shoola");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : S14.13 Sthaanabhedena Shoola - Gulpha Shoola
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : S14.13 Sthaanabhedena Shoola - Gulpha Shoola'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : S14.13 Sthaanabhedena Shoola - Gulpha Shoola");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : S14.14 Sthaanabhedena Shoola - Hanu Shoola
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : S14.14 Sthaanabhedena Shoola - Hanu Shoola'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : S14.14 Sthaanabhedena Shoola - Hanu Shoola");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : S14.15 Sthaanabhedena Shoola - Hasta Shoola
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : S14.15 Sthaanabhedena Shoola - Hasta Shoola'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : S14.15 Sthaanabhedena Shoola - Hasta Shoola");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : S14.16 Sthaanabhedena Shoola - Hrid Shoola
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : S14.16 Sthaanabhedena Shoola - Hrid Shoola'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : S14.16 Sthaanabhedena Shoola - Hrid Shoola");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : S14.17 Sthaanabhedena Shoola - Jaanu Shoola
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : S14.17 Sthaanabhedena Shoola - Jaanu Shoola'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : S14.17 Sthaanabhedena Shoola - Jaanu Shoola");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : S14.18 Sthaanabhedena Shoola - Janghaa Shoola
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : S14.18 Sthaanabhedena Shoola - Janghaa Shoola'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : S14.18 Sthaanabhedena Shoola - Janghaa Shoola");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : S14.19 Sthaanabhedena Shoola - Kantha Shoola
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : S14.19 Sthaanabhedena Shoola - Kantha Shoola'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : S14.19 Sthaanabhedena Shoola - Kantha Shoola");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : S14.21 Sthaanabhedena Shoola - Katee Shoola
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : S14.21 Sthaanabhedena Shoola - Katee Shoola'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : S14.21 Sthaanabhedena Shoola - Katee Shoola");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : S14.23 Sthaanabhedena Shoola - Kukshi Shoola
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : S14.23 Sthaanabhedena Shoola - Kukshi Shoola'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : S14.23 Sthaanabhedena Shoola - Kukshi Shoola");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : S14.24 Sthaanabhedena Shoola - Manyaa Shoola
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : S14.24 Sthaanabhedena Shoola - Manyaa Shoola'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : S14.24 Sthaanabhedena Shoola - Manyaa Shoola");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : S14.3 Sthaanabhedena Shoola - Amsa Shoola
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : S14.3 Sthaanabhedena Shoola - Amsa Shoola'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : S14.3 Sthaanabhedena Shoola - Amsa Shoola");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : S14.4 Sthaanabhedena Shoola - Anga Shoola
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : S14.4 Sthaanabhedena Shoola - Anga Shoola'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : S14.4 Sthaanabhedena Shoola - Anga Shoola");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : S14.5 Sthaanabhedena Shoola - Anguli Shoola
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : S14.5 Sthaanabhedena Shoola - Anguli Shoola'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : S14.5 Sthaanabhedena Shoola - Anguli Shoola");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : S14.6 Sthaanabhedena Shoola - Asthi Shoola
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : S14.6 Sthaanabhedena Shoola - Asthi Shoola'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : S14.6 Sthaanabhedena Shoola - Asthi Shoola");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : S14.7 Sthaanabhedena Shoola - Baahu Shoola
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : S14.7 Sthaanabhedena Shoola - Baahu Shoola'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : S14.7 Sthaanabhedena Shoola - Baahu Shoola");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : S15.28 Sthaanabhedena Shoola - Nakha Shoola
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : S15.28 Sthaanabhedena Shoola - Nakha Shoola'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : S15.28 Sthaanabhedena Shoola - Nakha Shoola");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : S15.31 Sthaanabhedena Shoola - Paada Shoola
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : S15.31 Sthaanabhedena Shoola - Paada Shoola'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : S15.31 Sthaanabhedena Shoola - Paada Shoola");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : S15.32 Sthaanabhedena Shoola - Paarshni Shoola
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : S15.32 Sthaanabhedena Shoola - Paarshni Shoola'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : S15.32 Sthaanabhedena Shoola - Paarshni Shoola");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : S15.34 Sthaanabhedena Shoola - Parva Shoola
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : S15.34 Sthaanabhedena Shoola - Parva Shoola'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : S15.34 Sthaanabhedena Shoola - Parva Shoola");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : S15.36 Sthaanabhedena Shoola - Prishtha Shoola
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : S15.36 Sthaanabhedena Shoola - Prishtha Shoola'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : S15.36 Sthaanabhedena Shoola - Prishtha Shoola");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : S15.41 Sthaanabhedena Shoola - Sakthi Shoola
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : S15.41 Sthaanabhedena Shoola - Sakthi Shoola'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : S15.41 Sthaanabhedena Shoola - Sakthi Shoola");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : S15.42 Sthaanabhedena Shoola - Sandhi Shoola
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : S15.42 Sthaanabhedena Shoola - Sandhi Shoola'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : S15.42 Sthaanabhedena Shoola - Sandhi Shoola");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : S15.43 Sthaanabhedena Shoola - Skandha Shoola
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : S15.43 Sthaanabhedena Shoola - Skandha Shoola'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : S15.43 Sthaanabhedena Shoola - Skandha Shoola");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : S15.44 Sthaanabhedena Shoola - Snaayu Shoola
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : S15.44 Sthaanabhedena Shoola - Snaayu Shoola'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : S15.44 Sthaanabhedena Shoola - Snaayu Shoola");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : S15.45 Sthaanabhedena Shoola - Sphik Shoola
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : S15.45 Sthaanabhedena Shoola - Sphik Shoola'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : S15.45 Sthaanabhedena Shoola - Sphik Shoola");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : S15.46 Sthaanabhedena Shoola - Stanaanta Shoola
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : S15.46 Sthaanabhedena Shoola - Stanaanta Shoola'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : S15.46 Sthaanabhedena Shoola - Stanaanta Shoola");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : S15.47 Sthaanabhedena Shoola - Trika Shoola
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : S15.47 Sthaanabhedena Shoola - Trika Shoola'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : S15.47 Sthaanabhedena Shoola - Trika Shoola");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : S15.48 Sthaanabhedena Shoola - Urah Shoola
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : S15.48 Sthaanabhedena Shoola - Urah Shoola'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : S15.48 Sthaanabhedena Shoola - Urah Shoola");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : S1A.0 Shoola
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : S1A.0 Shoola'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : S1A.0 Shoola");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : V1.0 Vaatarakta
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : V1.0 Vaatarakta'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : V1.0 Vaatarakta");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : V1.1 Vaatarakta - Dvandvaja
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : V1.1 Vaatarakta - Dvandvaja'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : V1.1 Vaatarakta - Dvandvaja");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : V1.2 Vaatarakta - Gambheera
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : V1.2 Vaatarakta - Gambheera'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : V1.2 Vaatarakta - Gambheera");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : V1.3 Vaatarakta - Kapha Vaataja
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : V1.3 Vaatarakta - Kapha Vaataja'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : V1.3 Vaatarakta - Kapha Vaataja");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : V1.4 Vaatarakta - Kaphaadhika Vaatarakta
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : V1.4 Vaatarakta - Kaphaadhika Vaatarakta'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : V1.4 Vaatarakta - Kaphaadhika Vaatarakta");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : V1.5 Vaatarakta - Pittaadhika Vaatarakta
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : V1.5 Vaatarakta - Pittaadhika Vaatarakta'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : V1.5 Vaatarakta - Pittaadhika Vaatarakta");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : V1.7 Vaatarakta - Uttaana
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : V1.7 Vaatarakta - Uttaana'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : V1.7 Vaatarakta - Uttaana");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : V1.8 Vaatarakta - Vaata Kaphaja
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : V1.8 Vaatarakta - Vaata Kaphaja'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : V1.8 Vaatarakta - Vaata Kaphaja");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : V1.9 Vaatarakta - Vaataadhika Vaatarakta
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : V1.9 Vaatarakta - Vaataadhika Vaatarakta'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : V1.9 Vaatarakta - Vaataadhika Vaatarakta");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : V2.0 Vaatavyaadhi
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : V2.0 Vaatavyaadhi'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : V2.0 Vaatavyaadhi");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : V2.12 Vaatavyaadhi - Stabdhagaatra
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : V2.12 Vaatavyaadhi - Stabdhagaatra'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : V2.12 Vaatavyaadhi - Stabdhagaatra");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : V2.16 Vaatavyaadhi - Baahugata Vaata
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : V2.16 Vaatavyaadhi - Baahugata Vaata'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : V2.16 Vaatavyaadhi - Baahugata Vaata");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : V2.23 Vaatavyaadhi - Gridhrasee
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : V2.23 Vaatavyaadhi - Gridhrasee'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : V2.23 Vaatavyaadhi - Gridhrasee");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : V2.30 Vaatavyaadhi - Jaanugata Vaata
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : V2.30 Vaatavyaadhi - Jaanugata Vaata'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : V2.30 Vaatavyaadhi - Jaanugata Vaata");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : V2.31 Vaatavyaadhi - Janghaagata Vaata
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : V2.31 Vaatavyaadhi - Janghaagata Vaata'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : V2.31 Vaatavyaadhi - Janghaagata Vaata");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : V2.36 Vaatavyaadhi - Kateegata Vaata
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : V2.36 Vaatavyaadhi - Kateegata Vaata'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : V2.36 Vaatavyaadhi - Kateegata Vaata");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : V2.42 Vaatavyaadhi - Maamsagata Vaata
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : V2.42 Vaatavyaadhi - Maamsagata Vaata'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : V2.42 Vaatavyaadhi - Maamsagata Vaata");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : V2.43 Vaatavyaadhi - Maamsamedogata Vaata
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : V2.43 Vaatavyaadhi - Maamsamedogata Vaata'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : V2.43 Vaatavyaadhi - Maamsamedogata Vaata");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : V2.44 Vaatavyaadhi - Majjaagata Vaata
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : V2.44 Vaatavyaadhi - Majjaagata Vaata'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : V2.44 Vaatavyaadhi - Majjaagata Vaata");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : V2.45 Vaatavyaadhi - Majjaasthigata Vaata
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : V2.45 Vaatavyaadhi - Majjaasthigata Vaata'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : V2.45 Vaatavyaadhi - Majjaasthigata Vaata");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : V2.46 Vaatavyaadhi - Manyaagata Vaata
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : V2.46 Vaatavyaadhi - Manyaagata Vaata'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : V2.46 Vaatavyaadhi - Manyaagata Vaata");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : V2.47 Vaatavyaadhi - Manyaastambha
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : V2.47 Vaatavyaadhi - Manyaastambha'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : V2.47 Vaatavyaadhi - Manyaastambha");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : V2.48 Vaatavyaadhi - Medogata Vaata
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : V2.48 Vaatavyaadhi - Medogata Vaata'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : V2.48 Vaatavyaadhi - Medogata Vaata");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : V2.61 Vaatavyaadhi - Prishthagata Vaata
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : V2.61 Vaatavyaadhi - Prishthagata Vaata'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : V2.61 Vaatavyaadhi - Prishthagata Vaata");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : V2.63 Vaatavyaadhi - Sandhigata Vaata
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : V2.63 Vaatavyaadhi - Sandhigata Vaata'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : V2.63 Vaatavyaadhi - Sandhigata Vaata");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : V2.64 Vaatavyaadhi - Sarvaangagata Vaata
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : V2.64 Vaatavyaadhi - Sarvaangagata Vaata'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : V2.64 Vaatavyaadhi - Sarvaangagata Vaata");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : V2.65 Vaatavyaadhi - Shaakhaagata Vaata
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : V2.65 Vaatavyaadhi - Shaakhaagata Vaata'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : V2.65 Vaatavyaadhi - Shaakhaagata Vaata");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : V2.68 Vaatavyaadhi - Siraagata Vaata
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : V2.68 Vaatavyaadhi - Siraagata Vaata'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : V2.68 Vaatavyaadhi - Siraagata Vaata");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : V2.69 Vaatavyaadhi - Siraagraha
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : V2.69 Vaatavyaadhi - Siraagraha'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : V2.69 Vaatavyaadhi - Siraagraha");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : V2.70 Vaatavyaadhi - Snaayugata Vaata
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : V2.70 Vaatavyaadhi - Snaayugata Vaata'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : V2.70 Vaatavyaadhi - Snaayugata Vaata");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : V2.72 Vaatavyaadhi - Trikgata Vaata
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : V2.72 Vaatavyaadhi - Trikgata Vaata'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : V2.72 Vaatavyaadhi - Trikgata Vaata");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : V2.73 Vaatavyaadhi - Tvaggata Vaata
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : V2.73 Vaatavyaadhi - Tvaggata Vaata'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : V2.73 Vaatavyaadhi - Tvaggata Vaata");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : V2.74 Vaatavyaadhi - Urugata Vaata
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : V2.74 Vaatavyaadhi - Urugata Vaata'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : V2.74 Vaatavyaadhi - Urugata Vaata");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : V2.75 Vaatavyaadhi - Vaatakantaka
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : V2.75 Vaatavyaadhi - Vaatakantaka'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : V2.75 Vaatavyaadhi - Vaatakantaka");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : V2.77 Vaatavyaadhi - Vishvaachee
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : V2.77 Vaatavyaadhi - Vishvaachee'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : V2.77 Vaatavyaadhi - Vishvaachee");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

# RMSD : V2.9 Vaatavyaadhi - Asthigata Vaata
```{r, echo = FALSE}
tmp <- disease0 [RMSD== 1 & patient_gender != '' & Code =='RMSD : V2.9 Vaatavyaadhi - Asthigata Vaata'];
km_fit <- survfit(Surv(disdur, status) ~ patient_gender, data= tmp);
p <- ggsurvplot(km_fit, 
           data =tmp, 
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           pval.method = TRUE,
           risk.table = TRUE,        # Add risk table
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           surv.median.line = "hv",  # add the median survival pointer.
           cumevents = TRUE, 
           add.all = TRUE, 
           title ="RMSD : V2.9 Vaatavyaadhi - Asthigata Vaata");

rescox <- coxph(Surv(disdur, status) ~ patient_gender, data= tmp)

```

## Kaplan Meier table
```{r, echo = FALSE}
summary(km_fit, times = durlwr)

```

## Survival plot
```{r, echo = FALSE}
p

```

## Hazard ratio plot
```{r, echo = FALSE}
rescox;ggforest(rescox, data=tmp)

```

