###############################################
#
# Net survival and Avoidable deaths
# Date: 25/4/2022
# Version 1.2
#
# Works for multiple cancer sites currently. W
#
#Needs to be synced
#
###############################################


#Avoidable Deaths due to Risk Factors for various Cancer sites
library(readxl)
library(dplyr)
library(tidyverse)
library(stringr)
library(haven)
library(mexhaz)
library(readr)

#Reading all the variables
PAFs <-
  read.csv("~/Documents/R_Projects/Data/combinedPAFs_cases_20.04.22.csv")
survival_merged_all_ages_missing_sites <-
  read_excel("~/Documents/R_Projects/Data/survival_merged_all_ages - missing sites.xlsx") %>%
  as.data.frame()
Cancer_codes <-
  read.csv("~/Documents/R_Projects/Data/dict_cancer.csv") %>%
  as.data.frame()


country_codes<-PAFs%>%summarize(country_code,country_label)%>%distinct()



Thailand <-
  read.csv("~/Documents/R_Projects/Data/survival_Thailand_anchor_ALL.csv") %>%
  as.data.frame()
Israel <-
  read.csv("~/Documents/R_Projects/Data/survival_Israel_anchor_ALL.csv") %>%
  as.data.frame()
HDI <-
  read.csv("~/Documents/R_Projects/Data/HDI_2019.csv") %>% as.data.frame()
Thailand_Survcan <-
  read.csv("~/Documents/R_Projects/Thai Data/ASTHABAN_cc_Oliver.csv") %>% as.data.frame()
Thailand_popmort <-
  read.csv("~/Documents/R_Projects/Thai Data/popmort_Thailand.csv") %>% as.data.frame()%>%
  left_join(country_codes,by=c("region"="country_label"))
  
  
Thailand_pop <-
  read.csv("~/Documents/R_Projects/Thai Data/ASTHABAN_pop.csv") %>% as.data.frame()





#Thai example - first with SURVCAN data and then with real data, scaling with Thai incidence rather than globocan

#expanding life table

Thailand_popmort_2015 <- Thailand_popmort %>% filter(X_year == 2015) %>%
  mutate(X_year = replace(X_year, X_year == 2015, 2016))

Thailand_popmort2 <-
  Thailand_popmort %>% full_join(Thailand_popmort_2015)



#Prepping cancer real world data
Thai_Surv <- Thailand_Survcan %>%
  filter(include == "Included") %>%
  filter(surv_yytot > 0) %>%
  filter(age >= 15) %>%
  filter(age <= 99) %>%
  filter(!is.na(age)) %>%
  filter(!is.na(surv_yytot)) %>%
  mutate(age_cat = cut(
    age,
    breaks = c(-Inf, 15, 64 , 99),
    labels = c("<15", "15-64", "65-99")
  )) %>% #create age categories (to be adjusted)
  mutate(last_FU_age = round(age + surv_yytot)) %>% #creating variable for age of death
  mutate(last_FU_year = round(year + surv_yytot)) %>% #creating variable for year of death
  #mutate(sex = replace(sex, sex == "Male", 1)) %>%
  #mutate(sex = replace(sex, sex == "Female", 2)) %>%  #removed string labels to join popmort data
  mutate(sex = as.integer(sex)) %>%
  left_join(
    Thailand_popmort2,
    by = c(
      "last_FU_age" = "X_age",
      "last_FU_year" = "X_year",
      "country" = "region",
      "sex" = "sex"
    )
  ) %>%
  filter(last_FU_year > 2011 & last_FU_year <= 2016) %>%
  filter(!is.na(mx)) %>% droplevels() %>%
  left_join(Cancer_codes, by = c("cancer" = "label")) #doesn't match cancer codes labels

Thai_Surv$sex <- as.integer(Thai_Surv$sex)

#age categories
Thai_Surv_overall <- Thai_Surv %>% mutate(age_cat = "Overall") #includes some <15 year olds
Thai_Surv_Lower <- Thai_Surv %>% filter(age_cat == "15-64")
Thai_Surv_Upper <- Thai_Surv %>% filter(age_cat == "65-99")

Thai_Surv_age_cats <- Thai_Surv_overall %>% full_join(Thai_Surv)


is.na(Thai_Surv$mx) #60 people have ages above 100 but were between 15-99 years at age of diagnosis. Should they be excluded?



#########################################
#
#
## Fitting a cubic spline by country and adding comparisons.
#
#
#########################################

#Creating time points for predictions
time <- seq(0, 5, le = 1001)

#Names for countries, regions and age groups for next step
#Removes empty age variables for model to be run properly
Thai_Surv <- Thai_Surv %>% droplevels()

cancer_types <-
  as_tibble(names(table(Thai_Surv$cancer))) #Needs to be tibble for predictions
names(cancer_types)[names(cancer_types) == "value"] <- "cancer"
cancer_types <-
  as.data.frame(cancer_types) #For regression needs to be in this form

cancer_codes <-
  as_tibble(names(table(Thai_Surv$cancer_code))) #Needs to be tibble for predictions
names(cancer_codes)[names(cancer_codes) == "value"] <- "cancer_code"
cancer_codes <-
  as.data.frame(cancer_codes) #For regression needs to be in this form

sex <-
  as_tibble(names(table(Thai_Surv$sex))) #Needs to be tibble for predictions
names(sex)[names(sex) == "value"] <- "sex"
sex <- as.data.frame(sex) #For regression needs to be in this form




#Cubic base model BY COUNTRY


Cubic_age_1 <- list()
Cubic_age_2 <- list()
Cubic_overall <- list()

#Cubic base model BY Cancer type

for (i in 1:nrow(cancer_types)) {
  b1 <- Thai_Surv_Lower %>% filter(cancer == cancer_types[i, ])
  b2 <- Thai_Surv_Upper %>% filter(cancer == cancer_types[i, ])
  b3 <- Thai_Surv_overall %>% filter(cancer == cancer_types[i, ])
  
  k1 <- median(b1$surv_yytot)
  k2 <- median(b2$surv_yytot)
  k3 <- median(b3$surv_yytot)
  
  try(Cubic_age_1[[i]] <-
        mexhaz(
          formula = Surv(surv_yytot, dead) ~ 1,
          data = b1,
          base = "exp.ns",
          degree = 3,
          knots = c(k1)
        ))
  
  try(Cubic_age_2[[i]] <-
        mexhaz(
          formula = Surv(surv_yytot, dead) ~ 1,
          data = b2,
          base = "exp.ns",
          degree = 3,
          knots = c(k2)
        ))
  
  try(Cubic_overall[[i]] <-
        mexhaz(
          formula = Surv(surv_yytot, dead) ~ 1,
          data = b3,
          base = "exp.ns",
          degree = 3,
          knots = c(k3)
        ))
}


#Updating the models with mortality rates

#Country updated models list initializing
Cubic_Cancer_age_1 <- list()
Cubic_Cancer_age_2 <- list()
Cubic_Cancer_overall <- list()


#Cubic excess hazard by country
for (i in 1:nrow(cancer_types)) {
  b1 <- Thai_Surv_Lower %>% filter(cancer == cancer_types[i, ])
  b2 <- Thai_Surv_Upper %>% filter(cancer == cancer_types[i, ])
  b3 <- Thai_Surv_overall %>% filter(cancer == cancer_types[i, ])
  
  try(Cubic_Cancer_age_1[[i]] <-
        update(Cubic_age_1[[i]], expected = "mx"))
  try(Cubic_Cancer_age_2[[i]] <-
        update(Cubic_age_2[[i]], expected = "mx"))
  try(Cubic_Cancer_overall[[i]] <-
        update(Cubic_overall[[i]], expected = "mx"))
}


####################

# Running Predictions by country

####################


Predictions_Cubic_Net_age_1 <- list()
Predictions_Cubic_Net_age_2 <- list()
Predictions_Cubic_Net_age_3 <- list()

Predictions_Cubic_All_Cause_age_1 <- list()
Predictions_Cubic_All_Cause_age_2 <- list()
Predictions_Cubic_All_Cause_age_3 <- list()

cancer_types_tibble <-
  cancer_types %>% as_tibble() #predict only works with tibble data structure...
#Cubic predictions by country
for (i in 1:nrow(cancer_types_tibble)) {
  try(AC1 <-
        predict(Cubic_age_1[[i]], time.pts = time, data.val = cancer_types_tibble[i, ]))
  try(AC2 <-
        predict(Cubic_age_2[[i]], time.pts = time, data.val = cancer_types_tibble[i, ]))
  try(AC3 <-
        predict(Cubic_overall[[i]], time.pts = time, data.val = cancer_types_tibble[i, ]))
  #
  #
  try(HP1 <-
        predict(Cubic_Cancer_age_1[[i]],
                time.pts = time,
                data.val = cancer_types_tibble[i, ]))
  try(HP2 <-
        predict(Cubic_Cancer_age_2[[i]],
                time.pts = time,
                data.val = cancer_types_tibble[i, ]))
  try(HP3 <-
        predict(Cubic_Cancer_overall[[i]],
                time.pts = time,
                data.val = cancer_types_tibble[i, ]))
  
  
  Predictions_Cubic_All_Cause_age_1[[i]] <- AC1
  Predictions_Cubic_All_Cause_age_2[[i]] <- AC2
  Predictions_Cubic_All_Cause_age_3[[i]] <- AC3
  
  
  Predictions_Cubic_Net_age_1[[i]] <- HP1
  Predictions_Cubic_Net_age_2[[i]] <- HP2
  Predictions_Cubic_Net_age_3[[i]] <- HP3
}
### Error in UseMethod("predict") : 
### no applicable method for 'predict' applied to an object of class "NULL"

#Extracting prediction data five year avoidable deaths prediction and survival

Net_Survival_Five_Year_age_1 <- matrix(ncol = 5, nrow = (i)) #R(t)
Net_Survival_Five_Year_age_2 <- matrix(ncol = 5, nrow = (i)) #R(t)
Net_Survival_Five_Year_age_3 <- matrix(ncol = 5, nrow = (i)) #R(t)

All_Cause_Survival_age_1 <- matrix(ncol = 5, nrow = (i))    #S(t)
All_Cause_Survival_age_2 <- matrix(ncol = 5, nrow = (i))     #S(t)
All_Cause_Survival_age_3 <- matrix(ncol = 5, nrow = (i))     #S(t)

for (i in 1:nrow(cancer_types_tibble)) {
  s <-  Predictions_Cubic_Net_age_1[[i]]$results
  s <-  s %>% filter(time.pts == 5)
  
  s2 <-  Predictions_Cubic_Net_age_2[[i]]$results
  s2 <-  s2 %>% filter(time.pts == 5)
  s3 <-  Predictions_Cubic_Net_age_3[[i]]$results
  s3 <-  s3 %>% filter(time.pts == 5)
  
  
  sp <- Predictions_Cubic_All_Cause_age_1[[i]]$results
  sp <-  sp %>% filter(time.pts == 5)
  sp2 <- Predictions_Cubic_All_Cause_age_2[[i]]$results
  sp2 <-  sp2 %>% filter(time.pts == 5)
  sp3 <- Predictions_Cubic_All_Cause_age_3[[i]]$results
  sp3 <-  sp3 %>% filter(time.pts == 5)
  
  
  
  Net_Survival_Five_Year_age_1[i, ] <-
    c(cancer_codes[i, ], cancer_types[i, ], s$surv, s$surv.inf, s$surv.sup)
  Net_Survival_Five_Year_age_2[i, ] <-
    c(cancer_codes[i, ],
      cancer_types[i, ],
      s2$surv,
      s2$surv.inf,
      s2$surv.sup)
  Net_Survival_Five_Year_age_3[i, ] <-
    c(cancer_codes[i, ],
      cancer_types[i, ],
      s3$surv,
      s3$surv.inf,
      s3$surv.sup)
  
  All_Cause_Survival_age_1[i, ] <-
    c(cancer_codes[i, ],
      cancer_types[i, ],
      sp$surv,
      sp$surv.inf,
      sp$surv.sup)
  All_Cause_Survival_age_2[i, ] <-
    c(cancer_codes[i, ],
      cancer_types[i, ],
      sp2$surv,
      sp2$surv.inf,
      sp2$surv.sup)
  All_Cause_Survival_age_3[i, ] <-
    c(cancer_codes[i, ],
      cancer_types[i, ],
      sp3$surv,
      sp3$surv.inf,
      sp3$surv.sup)
}

#

Age_names_all <- as.data.frame(c("15-64", "65-99", "Overall"))

Net_Survival_Five_Year_age_1 <-
  as.data.frame(Net_Survival_Five_Year_age_1)
Net_Survival_Five_Year_age_2 <-
  as.data.frame(Net_Survival_Five_Year_age_2)
Net_Survival_Five_Year_age_3 <-
  as.data.frame(Net_Survival_Five_Year_age_3)

All_Cause_Survival_age_1 <- as.data.frame(All_Cause_Survival_age_1)
All_Cause_Survival_age_2 <- as.data.frame(All_Cause_Survival_age_2)
All_Cause_Survival_age_3 <- as.data.frame(All_Cause_Survival_age_3)

colnames(Net_Survival_Five_Year_age_1) <-
  c("cancer_code",
    "cancer",
    "Five_Year_Net_Surv",
    "NS_Lower_CI",
    "NS_Upper_CI")
colnames(Net_Survival_Five_Year_age_2) <-
  c("cancer_code",
    "cancer",
    "Five_Year_Net_Surv",
    "NS_Lower_CI",
    "NS_Upper_CI")
colnames(Net_Survival_Five_Year_age_3) <-
  c("cancer_code",
    "cancer",
    "Five_Year_Net_Surv",
    "NS_Lower_CI",
    "NS_Upper_CI")

colnames(All_Cause_Survival_age_1) <-
  c("cancer_code",
    "cancer",
    "Five_Year_all_cause_Surv",
    "OS_Lower_CI",
    "OS_Upper_CI")
colnames(All_Cause_Survival_age_2) <-
  c("cancer_code",
    "cancer",
    "Five_Year_all_cause_Surv",
    "OS_Lower_CI",
    "OS_Upper_CI")
colnames(All_Cause_Survival_age_3) <-
  c("cancer_code",
    "cancer",
    "Five_Year_all_cause_Surv",
    "OS_Lower_CI",
    "OS_Upper_CI")



Net_Survival_Five_Year_age_1 <-
  Net_Survival_Five_Year_age_1 %>% mutate(age_cat = Age_names_all[1, ])
Net_Survival_Five_Year_age_2 <-
  Net_Survival_Five_Year_age_2 %>% mutate(age_cat = Age_names_all[2, ])
Net_Survival_Five_Year_age_3 <-
  Net_Survival_Five_Year_age_3 %>% mutate(age_cat = Age_names_all[3, ])

All_Cause_Survival_age_1 <-
  All_Cause_Survival_age_1 %>% mutate(age_cat = Age_names_all[1, ])
All_Cause_Survival_age_2 <-
  All_Cause_Survival_age_2 %>% mutate(age_cat = Age_names_all[2, ])
All_Cause_Survival_age_3 <-
  All_Cause_Survival_age_3 %>% mutate(age_cat = Age_names_all[3, ])


Net_Survival_Five_Year <-
  Net_Survival_Five_Year_age_1 %>% full_join(Net_Survival_Five_Year_age_2) %>%
  full_join(Net_Survival_Five_Year_age_3)

All_Cause_Survival <-
  All_Cause_Survival_age_1 %>% full_join(All_Cause_Survival_age_2) %>%
  full_join(All_Cause_Survival_age_3)



NS_OS2 <-
  Net_Survival_Five_Year %>% left_join(
    All_Cause_Survival,
    by = c(
      "cancer" = "cancer",
      "cancer_code" = "cancer_code",
      "age_cat" = "age_cat"
    )
  )


NS_OS2$cancer_code <- as.numeric(as.character(NS_OS2$cancer_code))

Thai_Surv$cancer_code <-
  as.numeric(as.character(Thai_Surv$cancer_code))




NS_OS <-
  NS_OS2 %>% left_join(
    Thai_Surv_age_cats,
    by = c(
      "cancer" = "cancer",
      "cancer_code" = "cancer_code",
      "age_cat" = "age_cat"
    )
  ) %>%
  select(
    cancer,
    cancer_code,
    Five_Year_all_cause_Surv,
    OS_Lower_CI,
    OS_Upper_CI,
    Five_Year_Net_Surv,
    NS_Lower_CI,
    NS_Upper_CI,
    age_cat
  ) %>%
  distinct()

NS_OS$Five_Year_Net_Surv <- as.numeric(NS_OS$Five_Year_Net_Surv)
NS_OS$NS_Lower_CI <- as.numeric(NS_OS$NS_Lower_CI)
NS_OS$NS_Upper_CI <- as.numeric(NS_OS$NS_Upper_CI)
NS_OS$Five_Year_all_cause_Surv <-
  as.numeric(NS_OS$Five_Year_all_cause_Surv)
NS_OS$OS_Lower_CI <- as.numeric(NS_OS$OS_Lower_CI)
NS_OS$OS_Upper_CI <- as.numeric(NS_OS$OS_Upper_CI)
NS_OS$cancer <- as.factor(as.character(NS_OS$cancer))
NS_OS$age_cat <- as.factor(NS_OS$age_cat)

#write.csv(NS_OS, "~/Documents/R_Projects/Data/Thai_NS_OS.csv")

#########################
#
# AD Calculations
#
#########################

#NS_OS <- read.csv("~/Documents/R_Projects/Data/Thai_NS_OS.csv")

#Prepping PAFs data processing and combining by age group
PAFs_age_Cat <- PAFs %>%
  filter(country_label == "Thailand") %>%
  mutate(age_cat = case_when(age >= 4 & age < 14 ~ "15-64",
                             age >= 14 ~ "65-99",
                             FALSE ~ "0-15")) %>%
  filter(age_cat != "0-15")%>%
  group_by(country_label, cancer_label, age_cat) %>%
  summarize(
    country_code,
    country_label,
    cancer_code,
    cancer_label,
    age,
    age_cat,
    cases,
    af.comb,
    cases.prev,
    cases.notprev,
    py,
    total_overall = sum(cases)
  ) %>% as.data.frame()


PAFS_Overall <- PAFs_age_Cat %>% mutate(age_cat = "Overall") %>%
  group_by(country_label, cancer_label, age_cat) %>%
  summarize(
    country_code,
    country_label,
    cancer_code,
    cancer_label,
    age,
    age_cat,
    cases,
    af.comb,
    cases.prev,
    cases.notprev,
    py,
    total_overall = sum(cases)
  ) %>% as.data.frame()


PAFs2 <- PAFs_age_Cat %>%
  full_join(PAFS_Overall) %>%
  as.data.frame() %>%
  droplevels() %>%
  group_by(country_label, cancer_label, age_cat) %>%
  mutate(total_age_prev = sum(cases.prev)) %>%
  as.data.frame() %>%
  mutate(af.comb.agecat = total_age_prev / total_overall) %>%
  summarize(
    country_code,
    country_label,
    cancer_code,
    cancer_label,
    age_cat,
    af.comb.agecat,
    total_overall
  ) %>%
  distinct() %>%
  arrange(cancer_label,
          age_cat)


NS_OS_PAF <-
  NS_OS %>% left_join(PAFs2, by = c("cancer_code" = "cancer_code", "age_cat" =
                                      "age_cat")) %>% droplevels()


#Three AD calcs

#first need to make sure data is in right format (numeric columns)

#Applying the equation from Rutherford 2015 for AD. Needs to be updated to have scaled relative survival
Avoidable_Deaths <- matrix(ncol = 12, nrow = nrow(NS_OS_PAF)) #AD(t)
NS_OS_PAF$cancer <- as.character(NS_OS_PAF$cancer)

for (i in 1:nrow(NS_OS_PAF)) {
  Expected_5_year_surv_mx <-
    (NS_OS_PAF[i, ]$Five_Year_all_cause_Surv) / (NS_OS_PAF[i, ]$Five_Year_Net_Surv) #Calculate expected survival from mortality rates which is all cause surv/ net survival
  
  
  
  #Preventable deaths
  AD_prev <-
    (NS_OS_PAF[i, ]$af.comb.agecat) * NS_OS_PAF[i, ]$total_overall * (1 - NS_OS_PAF[i, ]$Five_Year_Net_Surv) *
    Expected_5_year_surv_mx
  AD_prev_Lower <-
    (NS_OS_PAF[i, ]$af.comb.agecat) * NS_OS_PAF[i, ]$total_overall * (1 - NS_OS_PAF[i, ]$NS_Lower_CI) *
    Expected_5_year_surv_mx
  AD_prev_Upper <-
    (NS_OS_PAF[i, ]$af.comb.agecat) * NS_OS_PAF[i, ]$total_overall * (1 - NS_OS_PAF[i, ]$NS_Upper_CI) *
    Expected_5_year_surv_mx
  
  # #Avoidable deaths (treatable: #check what the lower CI is called in the previous data frame
  
  AD_treat <-
    (0.9 - NS_OS_PAF[i, ]$Five_Year_Net_Surv) * Expected_5_year_surv_mx * (1 -
                                                                             NS_OS_PAF[i, ]$af.comb.agecat) * NS_OS_PAF[i, ]$total_overall
  AD_treat_Lower <-
    (0.9 - NS_OS_PAF[i, ]$NS_Lower_CI) * Expected_5_year_surv_mx * (1 - NS_OS_PAF[i, ]$af.comb.agecat) *
    NS_OS_PAF[i, ]$total_overall
  AD_treat_Upper <-
    (0.9 - NS_OS_PAF[i, ]$NS_Upper_CI) * Expected_5_year_surv_mx * (1 - NS_OS_PAF[i, ]$af.comb.agecat) *
    NS_OS_PAF[i, ]$total_overall
  
  
  #Deaths not avoidable
  
  AD_unavoid <- #should Five_Year_Net_Surv here be same as best survival so 0.9?
    (1 - NS_OS_PAF[i, ]$af.comb.agecat) * NS_OS_PAF[i, ]$total_overall * (1 - 
                                                                            NS_OS_PAF[i, ]$Five_Year_Net_Surv * Expected_5_year_surv_mx)
  AD_unavoid_Lower <- #as above but for LCI
    (1 - NS_OS_PAF[i, ]$af.comb.agecat) * NS_OS_PAF[i, ]$total_overall * (1 -
                                                                            NS_OS_PAF[i, ]$NS_Lower_CI * Expected_5_year_surv_mx)
  AD_unavoid_Upper <- #as above but for UCI
    (1 - NS_OS_PAF[i, ]$af.comb.agecat) * NS_OS_PAF[i, ]$total_overall * (1 -
                                                                            NS_OS_PAF[i, ]$NS_Upper_CI * Expected_5_year_surv_mx)
  
  
  Avoidable_Deaths[i, ] <- c(
    NS_OS_PAF[i, ]$age_cat,
    NS_OS_PAF[i, ]$cancer_code,
    NS_OS_PAF[i, ]$cancer,
    AD_treat,
    AD_treat_Lower,
    AD_treat_Upper,
    AD_prev,
    AD_prev_Lower,
    AD_prev_Upper,
    AD_unavoid,
    AD_unavoid_Lower,
    AD_unavoid_Upper
  )
}

Avoidable_Deaths <- Avoidable_Deaths %>% as.data.frame()

colnames(Avoidable_Deaths) <-
  c(
    "age_cat",
    "cancer_code",
    "cancer",
    "AD_treat",
    "AD_treat_Lower",
    "AD_treat_Upper",
    "AD_prev",
    "AD_prev_Lower",
    "AD_prev_Upper",
    "AD_unavoid",
    "AD_unavoid_Lower",
    "AD_unavoid_Upper"
  )

Avoidable_Deaths


write.csv(Avoidable_Deaths, "~/Documents/R_Projects/Data/Thai_AD.csv")
