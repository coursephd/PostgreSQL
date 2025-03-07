# Metabolic : M10.0 Medoroga
```{r, echo = FALSE}
tmp <- disease0 [Metabolic== 1 & patient_gender != '' & Code =='Metabolic : M10.0 Medoroga'];
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
           title ="Metabolic : M10.0 Medoroga");

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

# Metabolic : M10.1 Medoroga - Sthula medho roga
```{r, echo = FALSE}
tmp <- disease0 [Metabolic== 1 & patient_gender != '' & Code =='Metabolic : M10.1 Medoroga - Sthula medho roga'];
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
           title ="Metabolic : M10.1 Medoroga - Sthula medho roga");

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

# Metabolic : M10.2 Medoroga - Sukshma medho roga
```{r, echo = FALSE}
tmp <- disease0 [Metabolic== 1 & patient_gender != '' & Code =='Metabolic : M10.2 Medoroga - Sukshma medho roga'];
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
           title ="Metabolic : M10.2 Medoroga - Sukshma medho roga");

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

# Metabolic : M2.0 Madhumeha
```{r, echo = FALSE}
tmp <- disease0 [Metabolic== 1 & patient_gender != '' & Code =='Metabolic : M2.0 Madhumeha'];
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
           title ="Metabolic : M2.0 Madhumeha");

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

# Metabolic : P5.0 Prameha
```{r, echo = FALSE}
tmp <- disease0 [Metabolic== 1 & patient_gender != '' & Code =='Metabolic : P5.0 Prameha'];
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
           title ="Metabolic : P5.0 Prameha");

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

# Metabolic : P5.1 Prameha - Krusha
```{r, echo = FALSE}
tmp <- disease0 [Metabolic== 1 & patient_gender != '' & Code =='Metabolic : P5.1 Prameha - Krusha'];
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
           title ="Metabolic : P5.1 Prameha - Krusha");

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

# Metabolic : P5.2 Prameha - Pidaka
```{r, echo = FALSE}
tmp <- disease0 [Metabolic== 1 & patient_gender != '' & Code =='Metabolic : P5.2 Prameha - Pidaka'];
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
           title ="Metabolic : P5.2 Prameha - Pidaka");

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

# Metabolic : P5.3 Prameha - Sthula
```{r, echo = FALSE}
tmp <- disease0 [Metabolic== 1 & patient_gender != '' & Code =='Metabolic : P5.3 Prameha - Sthula'];
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
           title ="Metabolic : P5.3 Prameha - Sthula");

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

# Metabolic : P5.4 Prameha - Upadrava
```{r, echo = FALSE}
tmp <- disease0 [Metabolic== 1 & patient_gender != '' & Code =='Metabolic : P5.4 Prameha - Upadrava'];
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
           title ="Metabolic : P5.4 Prameha - Upadrava");

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

# Metabolic : S16.0 Sthaulya
```{r, echo = FALSE}
tmp <- disease0 [Metabolic== 1 & patient_gender != '' & Code =='Metabolic : S16.0 Sthaulya'];
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
           title ="Metabolic : S16.0 Sthaulya");

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

