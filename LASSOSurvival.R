library(sas7bdat)
library(survival)
library(ggplot2)
library(lubridate)
library(survminer)
library(ggsurvfit)
library(gtsummary)
library(tidymodels)
library(tidyverse)
library(dplyr)
library(survRM2)
library(ggfortify)
library(glmnet)

## Run the two commented lines once, then after only have to run the 
## line below it (benefit of this is its really fast loading in data)

#Ovarian <- read.sas7bdat("ovcasurvival_083022.sas7bdat")
#write.csv(Ovarian, "ovarian.csv")

Ovarian <- data.table::fread("ovarian.csv")

Ovarian.ex <- Ovarian %>% filter(exclude == 0)

Ovarian.ex <- Ovarian.ex %>% 
  mutate(survyears = survmonths / 12) %>%
  mutate(race_labeled = case_when(
    race_new == 0 ~ "Non- Hispanic White",
    race_new == 1 ~ "Asian Indian/ Pakistani",
    race_new == 2 ~ "Chinese",
    race_new == 3 ~ "Filipino",
    race_new == 4 ~ "Japanese",
    race_new == 5 ~ "Korean",
    race_new == 6 ~ "Viatnamese",
    race_new == 7 ~ "Hawaiian/ Pacific Islander",
    race_new == 8 ~ "Other Southeast Asian",
    race_new == 9 ~ "Other Asian",
  ))

# Relevel such that baseline level is NonHispanic White
Ovarian.ex <- Ovarian.ex %>% 
  mutate(race_labeled_releved = relevel(factor(race_labeled), ref = "Non- Hispanic White"))


# Cox Proportional Hazard With Different Variables
## Relevant Variable:
### race_labeled_releved: Race. Categorical
### ses: Socioeconomic status. Categorical
### urban_new: If in urban or not. Categorical
### marital: If married. Categorical
### histology_new: Categorical
### stage: Tumor stage. Categorical
### grade: Tumor grade. Categorical
### site: Organ of cancer. Categorical
### age_cat: Age of diagnosis
### dxtotreat: Time to treatment. Continuous
### primsurgery: Srugery of primary site of treatment. Categprocal
### radiation: Radiation therpay. Categorical
### chemo: Chemo for treatment. Cateogircal
### firstprim: Whether cancer is first primary tumor. Cat
### primary: Whether cancer is only primary tumor. Cat
### microconfirm: If tumor was microscopically confirmed. Cat

# https://cran.r-project.org/web/packages/glmnet/vignettes/Coxnet.pdf

# Covert relevant cats to factors
Ovarian.ex <- Ovarian.ex %>% 
  mutate(ses = as.factor(ses),
         urban_new = as.factor(urban_new),
         marital = as.factor(marital),
         histology_new = as.factor(histology_new),
         stage = as.factor(stage),
         grade = as.factor(grade),
         site = as.factor(site),
         age_cat = as.factor(age_cat),
         primsurgery = as.factor(primsurgery),
         radiation = as.factor(radiation),
         chemo = as.factor(chemo),
         primary = as.factor(primary),
         microconfirm = as.factor(microconfirm))

# LASSO Cox model
Ovarian.without0s <- Ovarian.ex %>% 
  filter(survyears > 0) %>% 
  select(ses, urban_new, marital, histology_new, stage,
         grade, site, age_cat, primsurgery, radiation, chemo,
         primary, microconfirm, race_labeled_releved,
         dxtotreat, survyears, statusca)%>% na.omit()

X <- Ovarian.without0s %>% 
  select(ses, urban_new, marital, histology_new, stage,
         grade, site, age_cat, primsurgery, radiation, chemo,
         primary, microconfirm, race_labeled_releved,
         dxtotreat)



Y <- Ovarian.without0s %>% 
  select(survyears, statusca) %>% 
  rename(time = survyears, status = statusca)

Y <- Surv(Y$time, Y$status)

X <- model.matrix(Y ~ ses + urban_new + marital + 
                    histology_new + stage + grade + site +
                    age_cat + primsurgery + radiation + chemo +
                    primary + microconfirm +
                    race_labeled_releved + dxtotreat, X)


set.seed(1)
cvfit <- cv.glmnet(X, Y, family = "cox", type.measure = "C")
plot(cvfit)

print(cvfit$lambda.min)
# Result is 0.0006006944

plot(survival::survfit(cvfit, s = "lambda.min", x = X, y = Y))

fit1 <- glmnet(X, Y, family = "cox", lambda = 0.0006006944)
coef(fit1)

# Stratified
Y2 <- stratifySurv(Y, Ovarian.without0s$race_labeled_releved)

# This takes a really long time so would recomend not running it
cv.fit.stratified <- cv.glmnet(X, Y2, family = "cox", nfolds = 5)
plot(cv.fit.stratified)

print(cv.fit.stratified$lambda.min)
# Results is 0.0005959298

plot(survival::survfit(cv.fit.stratified, s = "lambda.min", x = X, y = Y2))

fit2 <- glmnet(X, Y2, family = "cox", lambda = 0.0005959298)
coef(fit2)
