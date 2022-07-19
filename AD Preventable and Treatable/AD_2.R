###############################################
#
# Net survival and Avoidable deaths
#  Date: 17/07/2022
# Version 2.3
#
# Works for multiple cancer sites currently.
#
#Needs to be synced
#
###############################################

#Avoidable Deaths due to Risk Factors for various Cancer sites
library(readxl)
# library(data.table)
# library(dtplyr)
library(dplyr)
library(tidyverse)
library(stringr)
library(haven)
library(mexhaz)
library(readr)
library(ggplot2)
library(relsurv)
library(janitor)



########################
#Comment from here


HDI_Region_Mapping<-read.csv("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Research visits\\Oliver_Langselius\\Data\\HDI2018_GLOBOCAN2020.csv")




country_codes <-
  read.csv("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Research visits\\Oliver_Langselius\\Data\\GCO_country_info.csv", stringsAsFactors = FALSE) %>%
  filter(country_code<900) %>%
  select(country_code, country_label)

PAFs10 <- read.csv("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Research visits\\Oliver_Langselius\\Data\\combinedPAFs_cases_12.07.22.csv")

PAFs<-PAFs10%>%
  mutate(cancer_label=as.character(cancer_label))%>%
  #mutate(cancer_label = replace(cancer_label, cancer_label == "Colon", "Colorectal")) %>%
  #mutate(cancer_label = replace(cancer_label, cancer_label == "Rectum", "Colorectal")) %>%
  #mutate(cancer_code = replace(cancer_code, cancer_code == 8, 38))%>%
  #mutate(cancer_code = replace(cancer_code, cancer_code == 9, 38))%>%
  group_by(country_code, sex,
           cancer_code, age)%>%
  filter(sex!=0)%>%
  mutate(af.comb= case_when(cases!=0 ~ sum(cases.prev)/sum(cases),
                            cases==0 ~    af.comb))%>%
  # mutate(cases.prev=sum(cases.prev))%>%
  # mutate(cases.notprev=sum(cases.notprev))%>%
  # mutate(cases=sum(cases))%>%
  ungroup()%>%
  as.data.frame()

survival_merged_all_ages_missing_sites <- read_excel("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Research visits\\Oliver_Langselius\\Data\\survival_merged_all_ages - missing sites.xlsx") %>% as.data.frame()
Cancer_codes <- read.csv("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Research visits\\Oliver_Langselius\\Data\\dict_cancer.csv") %>% as.data.frame()
Cancer_codes_Survcan <- read.csv("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Research visits\\Oliver_Langselius\\Data\\cancer_codes_Survcan.csv") %>% as.data.frame()

# Thailand <- read.csv("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Research visits\\Oliver_Langselius\\Data\\survival_Thailand_anchor_ALL.csv") %>% as.data.frame()
# Israel <- read.csv("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Research visits\\Oliver_Langselius\\Data\\survival_Israel_anchor_ALL.csv") %>% as.data.frame()

Survival_Modelled <- read.csv("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Research visits\\Oliver_Langselius\\Data\\survival_allsites_allcountries.csv") %>% 
   as.data.frame()


HDI <-read.csv("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Research visits\\Oliver_Langselius\\Data\\HDI_2019.csv") %>% as.data.frame()

# Uncomment when rerunning. This takes forever to load
# Thailand_Survcan <-read.csv("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\SURVCANALL_cc.csv")# %>% as.data.frame()Research visits\\Oliver_Langselius\\Thai Data\\ASTHABAN_cc_Oliver.csv
# Thailand_Survcan<-Thailand_Survcan %>% as.data.frame() %>%  filter(country=="Thailand")


Thailand_popmort <-read.csv("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Research visits\\Oliver_Langselius\\Thai Data\\popmort_Thailand.csv") %>% as.data.frame() %>%
  left_join(country_codes, by = c("region" = "country_label"))
Thailand_pop <-read.csv("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Research visits\\Oliver_Langselius\\Thai Data\\ASTHABAN_pop.csv") %>% as.data.frame()

popmort2<-read_dta("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Research visits\\Oliver_Langselius\\Data\\who_ghe_popmort.dta")%>%as.data.frame()%>%
  left_join(country_codes)
popmort<-read_dta("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Research visits\\Oliver_Langselius\\Data\\who_ghe_popmort2.dta")%>%as.data.frame()

p <- read_dta("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Research visits\\Oliver_Langselius\\Data\\who_ghe_group.dta")%>%
  as.data.frame()

MIR_Age_Cats<-read.csv("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Research visits\\Oliver_Langselius\\Data\\MIR_age_cat.csv")%>%
  as.data.frame()%>%select(-mortality,-incidence)%>%
  mutate(MIR=replace(MIR,MIR==Inf, NA))

#same file but Globocan age groups for modeled data
MIR_Globocan<-read.csv("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Research visits\\Oliver_Langselius\\Data\\MIR.csv")%>%
  as.data.frame()%>%
  select(-mortality,
         -incidence)%>%
  mutate(MIR=replace(MIR,MIR==Inf, NA))%>%
  mutate(age = case_when(
    age==4~ 4,
    age>4 & age<=9~ 9,
    age==10~ 10,
    age==11~ 11,
    age==12~ 12,
    age==13~ 13,
    age==14~ 14,
    age==15~ 15,
    age>=16 ~ 16,
  ))%>%
  mutate(age_cat = case_when(
    age>=4 & age<14 ~ "15-64",
    age>=14 ~ "65-99",
    age<4 ~"0-15"))%>%
  select(-sex, -X)%>%
  group_by(country_code, cancer_code, age)%>%
  mutate(MIR=sum(MIR*py)/sum(py))%>%
  mutate(py=sum(py))%>%distinct()%>%
  ungroup()


Thailand_expected_Survival<-read.csv("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Research visits\\Oliver_Langselius\\Data\\Thailand_expected_Survival.csv")%>%as.data.frame()

Reference_Survival<-read.csv("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Research visits\\Oliver_Langselius\\Data\\Reference_Survival.csv")%>%
  as.data.frame()%>%
  select( age,
          cancer_code,
          rel_surv)%>%
  dplyr::rename("surv_ref"="rel_surv")%>%
  distinct()

Reference_Survival_Survcan<-read.csv("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Research visits\\Oliver_Langselius\\Data\\Reference_Survival_Survcan.csv")%>%
  as.data.frame()%>%
  select( age_cat,
          cancer_code,
          rel_surv)%>%
  dplyr::rename("surv_ref"="rel_surv")%>%
  distinct()


###################

load("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Research visits\\Oliver_Langselius\\Data\\ES_dt.RData")


#Reading all the variables
#GCO_country_info.csv has correct country_label variable to match with pop_mort2

HDI_Region_Mapping<-read.csv("~/Documents/R_Projects/Data/HDI2018_GLOBOCAN2020.csv")


country_codes <-
  read.csv("~/Documents/R_Projects/Data/GCO_country_info.csv", stringsAsFactors = FALSE) %>%
  filter(country_code<900) %>%
  select(country_code, country_label)




PAFs <- read.csv("~/Documents/R_Projects/Data/combinedPAFs_cases_12.07.22.csv")%>%
  # mutate(cancer_label = replace(cancer_label, cancer_label == "Colon", "Colorectal")) %>%
  # mutate(cancer_label = replace(cancer_label, cancer_label == "Rectum", "Colorectal")) %>%
  # mutate(cancer_code = replace(cancer_code, cancer_code == 8, 38))%>%
  # mutate(cancer_code = replace(cancer_code, cancer_code == 9, 38))%>%
  group_by(country_code, sex,
           cancer_code, age)%>%
  filter(sex!=0)%>%
  mutate(af.comb= case_when(cases!=0 ~ sum(cases.prev)/sum(cases),
                            cases==0 ~    af.comb))%>%
  # mutate(cases.prev=sum(cases.prev))%>%
  # mutate(cases.notprev=sum(cases.notprev))%>%
  # mutate(cases=sum(cases))%>%
  ungroup()%>%
  as.data.frame()



#survival_merged_all_ages_missing_sites <- read_excel("~/Documents/R_Projects/Data/survival_merged_all_ages - missing sites.xlsx") %>% as.data.frame()
Cancer_codes <- read.csv("~/Documents/R_Projects/Data/dict_cancer.csv") %>% as.data.frame()
Cancer_codes_Survcan <- read.csv("~/Documents/R_Projects/Data/cancer_codes_Survcan.csv") %>% as.data.frame()


Survival_Modelled <- read.csv("~/Documents/R_Projects/Data/survival_allsites_allcountries.csv") %>%
  as.data.frame()




# Thailand <- read.csv("~/Documents/R_Projects/Data/survival_Thailand_anchor_ALL.csv") %>% as.data.frame()
# Israel <- read.csv("~/Documents/R_Projects/Data/survival_Israel_anchor_ALL.csv") %>% as.data.frame()
HDI <-read.csv("~/Documents/R_Projects/Data/HDI_2019.csv") %>% as.data.frame()
Thailand_Survcan <-read.csv("~/Documents/R_Projects/Thai Data/ASTHABAN_cc_Oliver.csv")# %>% as.data.frame()
Thailand_popmort <-read.csv("~/Documents/R_Projects/Thai Data/popmort_Thailand.csv") %>% as.data.frame() %>%
  left_join(country_codes, by = c("region" = "country_label"))
# %>%
#   mutate(s=prob+mx)

# Thailand_pop <-read.csv("~/Documents/R_Projects/Thai Data/ASTHABAN_pop.csv") %>% as.data.frame()

popmort2<-read_dta("~/Documents/R_Projects/Data/who_ghe_popmort.dta")%>%as.data.frame()%>%
  left_join(country_codes)

p <- read_dta("~/Documents/R_Projects/Data/who_ghe_group.dta")%>%
  as.data.frame()

MIR_Age_Cats<-read.csv("~/Documents/R_Projects/Data/MIR_age_cat.csv")%>%
  as.data.frame()%>%
  select(-mortality,-incidence)%>%
  mutate(MIR=replace(MIR,MIR==Inf, NA))

#same file but Globocan age groups for modeled data
MIR_Globocan<-read.csv("~/Documents/R_Projects/Data/MIR.csv")%>%
  as.data.frame()%>%
  select(-mortality,
         -incidence)%>%
  mutate(MIR=replace(MIR,MIR==Inf, NA))%>%
  mutate(age = case_when(
    age==4~ 4,
    age>4 & age<=9~ 9,
    age==10~ 10,
    age==11~ 11,
    age==12~ 12,
    age==13~ 13,
    age==14~ 14,
    age==15~ 15,
    age>=16 ~ 16,
  ))%>%
  mutate(age_cat = case_when(
    age>=4 & age<14 ~ "15-64",
    age>=14 ~ "65-99",
    age<4 ~"0-15"))%>%
  select(-sex, -X)%>%
  group_by(country_code, cancer_code, age)%>%
  mutate(MIR=sum(MIR*py)/sum(py))%>%
  mutate(py=sum(py))%>%distinct()%>%
  ungroup()


Thailand_expected_Survival<-read.csv("~/Documents/R_Projects/Data/Thailand_expected_Survival.csv")%>%as.data.frame()


sapply(Reference_Survival, class)

Reference_Survival<-read.csv("~/Documents/R_Projects/Data/Reference_Survival.csv")%>%
  as.data.frame()%>%
  select( age,
          cancer_code,
          rel_surv)%>%
  dplyr::rename("surv_ref"="rel_surv")%>%
  distinct()

Reference_Survival_Survcan<-read.csv("~/Documents/R_Projects/Data/Reference_Survival_Survcan.csv")%>%
  as.data.frame()%>%
  select( age_cat,
          cancer_code,
          rel_surv)%>%
  dplyr::rename("surv_ref"="rel_surv")%>%
  distinct()




load("~/Documents/R_Projects/Data/ES_dt.RData")

ES2<-ES_dt%>%
  as.data.frame()%>%
  filter(time==1000)%>%
  select(-time)%>%
  dplyr::rename(ES="SurvExp")




# 
# ES_age_Cats<-ES2%>%  
#   mutate(age = case_when(
#     age==0~ 0,
#     age==1~ 1,
#     age==2~ 2,
#     age==3~ 3,
#   age==4~ 4,
#   age>4 & age<=9~ 9,
#   age==10~ 10,
#   age==11~ 11,
#   age==12~ 12,
#   age==13~ 13,
#   age==14~ 14,
#   age==15~ 15,
#   age>=16 ~ 16))%>%
#   mutate(
#     age_cat = case_when(
#       age>=4 & age<14 ~ "15-64",
#       age>=14 ~ "65-99",
#       age<4 ~"0-15"
#     ))%>%left_join
# 

# 


ten_cancer_sites <-
  Cancer_codes %>% 
  filter(cancer_code %in% c(6, 7, 11, 13, 15, 20, 23, 27, 30, 38))%>%
  mutate(cancer_label=replace(cancer_label,cancer_label=="Unspecified sites","Colorectal"))



#Checking cancer codes in various files
a<-PAFs%>%
  dplyr::summarize(country_code, country_label)%>%
  distinct()
b<-simulated_overall%>%
  ungroup()%>%
  dplyr::summarize(country_code, country_label)%>%
  distinct()




#Thai example - first with SURVCAN data and then with real data, scaling with Thai incidence rather than globocan

#expanding life table

Thailand_popmort_2015 <-
  Thailand_popmort %>% filter(X_year == 2015) %>%
  mutate(X_year = replace(X_year, X_year == 2015, 2016))

Thailand_popmort2 <-
  Thailand_popmort %>% full_join(Thailand_popmort_2015)



#Prepping cancer real world data
Thai_Survcan2 <- Thailand_Survcan %>%
  filter(surv_dd>0)%>%
  filter(include == "Included") %>%
  filter(age >= 15) %>%
  filter(age <= 99) %>%
  filter(!is.na(age)) %>%
  filter(!is.na(surv_yy)) %>%
  mutate(age_cat = cut(
    age,
    breaks = c(-Inf, 15, 64 , 99),
    labels = c("<15", "15-64", "65-99")
  )) %>% #create age categories (to be adjusted)
  ungroup()%>%
  droplevels()%>%
  mutate(last_FU_age = round(age + surv_dd/365.15)) %>% #creating variable for age of death
  mutate(last_FU_year = round(year + surv_dd/365.15))%>%  #creating variable for year of death
  mutate(sex = replace(sex, sex == "Male", 1)) %>%
  mutate(sex = replace(sex, sex == "Female", 2)) %>%
  mutate(sex = as.integer(sex)) %>%
  left_join(Thailand_popmort2, by = c(
    "last_FU_age" = "X_age",
    "last_FU_year" = "X_year",
    "country" = "region",
    "sex" = "sex")) %>%
  droplevels()%>%
  filter(last_FU_year > 2009 & last_FU_year <= 2014) %>%
  filter(!is.na(mx)) %>% 
  droplevels() %>%
  left_join(Cancer_codes_Survcan, by = c("cancer" = "cancer"))%>%
  mutate(cancer = replace(cancer, cancer == "Colon (C18)", "Colorectal")) %>%
  mutate(cancer = replace(cancer, cancer == "Rectum (C19-20)", "Colorectal")) %>%
  filter(cancer_code %in% ten_cancer_sites$cancer_code)

Thai_Surv3 <- Thai_Survcan2%>%
  mutate(surv_yydd=surv_dd/365.2425)%>%
  mutate(event1=case_when(dead==1 &      surv_yydd<=5 ~ 1,
                          dead==1 & surv_yydd>5 ~ 0,
                          dead==0 ~ 0
  ))

#modifying so last five years of follow up



Thai_Surv11<- Thai_Surv3%>%
  select(region_lab, country, doi, last_FU_age,age,surv_yytot,year,cancer_code)%>%
  group_by(region_lab,country,cancer_code)%>%
  dplyr::summarize( end_FU=max(year))%>%
  as.data.frame()


Thai_Surv<-Thai_Surv3%>%left_join(Thai_Surv11)#%>%ungroup()%>%
# filter(year>=end_FU-5)
# mutate(surv_yy=case_when(surv_yy<=5 ~ 5
# )
# )

#Thai_Surv$sex <- as.integer(Thai_Surv$sex)

#Thai_Surv_test<-Thai_Surv%>%filter(cancer_code==30)


#age categories
#Thai_Surv_overall <- Thai_Surv %>% mutate(age_cat = "Overall")
Thai_Surv_Lower <- Thai_Surv %>% filter(age_cat == "15-64")
Thai_Surv_Upper <- Thai_Surv %>% filter(age_cat == "65-99")

Thai_Surv_age_cats <- Thai_Surv #Thai_Surv_overall %>% full_join(Thai_Surv)


is.na(Thai_Surv$mx) #60 people have ages above 100 but were between 15-99 years at age of diagnosis. Should they be excluded?



#########################################
#
#
## Fitting a cubic spline by country and adding comparisons.
#
#
#########################################

#Creating time points for predictions
time <- seq(0, 5, le = 5001)

#Names for countries, regions and age groups for next step
#Removes empty age variables for model to be run properly
Thai_Surv <- Thai_Surv %>% droplevels()

cancer_types <- as_tibble(names(table(Thai_Surv$cancer))) #Needs to be tibble for predictions

names(cancer_types)[names(cancer_types) == "value"] <- "cancer"
cancer_types <-  as.data.frame(cancer_types) #For regression needs to be in this form

cancer_codes <- as_tibble(names(table(ten_cancer_sites$cancer_code))) #Needs to be tibble for predictions
names(cancer_codes)[names(cancer_codes) == "value"] <- "cancer_code"
cancer_codes <- as.data.frame(cancer_codes) #For regression needs to be in this form

sex <-as_tibble(names(table(Thai_Surv$sex))) #Needs to be tibble for predictions
names(sex)[names(sex) == "value"] <- "sex"
sex <- as.data.frame(sex) #For regression needs to be in this form




#Cubic base model BY COUNTRY


Cubic_age_1 <- list()
Cubic_age_2 <- list()
#Cubic_overall <- list()

#Cubic base model BY Cancer type

for (i in 1:nrow(cancer_codes)) {
  b1 <- Thai_Surv_Lower %>% filter(cancer_code == cancer_codes[i,])
  b2 <- Thai_Surv_Upper %>% filter(cancer_code == cancer_codes[i,])
  # b3 <- Thai_Surv_overall %>% filter(cancer_code == cancer_codes[i,])
  
  
  # k1<-c(1,2.5,4)
  # k2<-c(1,2.5,4)
  # k3<-c(1,2.5,4)
  
  
  k1 <- median(b1$surv_yydd)
  k2 <- median(b2$surv_yydd)
  #  k3 <- median(b3$surv_yydd)
  
  try(Cubic_age_1[[i]] <-
        mexhaz(
          formula = Surv(surv_yydd, event1) ~ 1,
          data = b1,
          base = "exp.ns",
          # degree = 3,
          knots = k1
          #  numHess=TRUE,
          # fnoptim="optim"
        ))
  
  try(Cubic_age_2[[i]] <-
        mexhaz(
          formula = Surv(surv_yydd, event1) ~ 1,
          data = b2,
          base = "exp.ns",
          # degree = 3,
          knots = k2
          # numHess=TRUE,
          # fnoptim="optim"
        ))
  
  # try(Cubic_overall[[i]] <-
  #       mexhaz(
  #         formula = Surv(surv_yydd, event1) ~ 1,
  #         data = b3,
  #         base = "exp.ns",
  #        # degree = 3,
  #         knots = k3
  #        # numHess=TRUE,
  #        # fnoptim="optim"
  #       ))
}






#Updating the models with mortality rates

#Country updated models list initializing
Cubic_Cancer_age_1 <- list()
Cubic_Cancer_age_2 <- list()
#Cubic_Cancer_overall <- list()

#Cubic excess hazard by country
for (i in 1:nrow(cancer_codes)){
  b1 <- Thai_Surv_Lower %>% filter(cancer_code == cancer_codes[i,])
  b2 <- Thai_Surv_Upper %>% filter(cancer_code == cancer_codes[i,])
  # b3 <- Thai_Surv_overall %>% filter(cancer_code == cancer_codes[i,])
  
  try(Cubic_Cancer_age_1[[i]] <-
        update(Cubic_age_1[[i]], expected = "mx",      numHess=TRUE,
               fnoptim="optim"))
  try(Cubic_Cancer_age_2[[i]] <-
        update(Cubic_age_2[[i]], expected = "mx",      numHess=TRUE,
               fnoptim="optim"))
  # try(Cubic_Cancer_overall[[i]] <-
  #       update(Cubic_overall[[i]], expected = "mx",    numHess=TRUE,
  #              fnoptim="optim"))
}


####################

# Running Predictions by country

####################


Predictions_Cubic_Net_age_1 <- list()
Predictions_Cubic_Net_age_2 <- list()
#Predictions_Cubic_Net_age_3 <- list()

Predictions_Cubic_All_Cause_age_1 <- list()
Predictions_Cubic_All_Cause_age_2 <- list()
#Predictions_Cubic_All_Cause_age_3 <- list()

cancer_types_tibble <-
  cancer_types %>% as_tibble() #predict only works with tibble data structure...

cancer_codes_tibble <-
  cancer_codes %>% 
  mutate(cancer_code=as.integer(cancer_code))%>%
  as_tibble() #predict only works with tibble data structure...
#Cubic predictions by country
for (i in 1:nrow(cancer_codes_tibble)) {
  try(AC1 <- predict(Cubic_age_1[[i]], time.pts = time, data.val = cancer_codes_tibble[i,]))
  try(AC2 <- predict(Cubic_age_2[[i]], time.pts = time, data.val = cancer_codes_tibble[i,]))
  # try(AC3 <- predict(Cubic_overall[[i]], time.pts = time, data.val = cancer_codes_tibble[i,]))
  

  try(HP1 <- predict(Cubic_Cancer_age_1[[i]],
                     time.pts = time,
                     data.val = cancer_codes_tibble[i,]))
  try(HP2 <- predict(Cubic_Cancer_age_2[[i]],
                     time.pts = time,
                     data.val = cancer_codes_tibble[i,]))
  # try(HP3 <- predict(Cubic_Cancer_overall[[i]],
  #               time.pts = time,
  #               data.val = cancer_codes_tibble[i,]))
  
  
  Predictions_Cubic_All_Cause_age_1[[i]] <- AC1
  Predictions_Cubic_All_Cause_age_2[[i]] <- AC2
  #  Predictions_Cubic_All_Cause_age_3[[i]] <- AC3
  
  
  Predictions_Cubic_Net_age_1[[i]] <- HP1
  Predictions_Cubic_Net_age_2[[i]] <- HP2
  #  Predictions_Cubic_Net_age_3[[i]] <- HP3
}


#Extracting prediction data five year avoidable deaths prediction and survival

Net_Survival_Five_Year_age_1 <- matrix(ncol = 5, nrow = nrow(cancer_codes)) #R(t)
Net_Survival_Five_Year_age_2 <- matrix(ncol = 5, nrow = nrow(cancer_codes)) #R(t)
# Net_Survival_Five_Year_age_3 <- matrix(ncol = 5, nrow = nrow(cancer_codes)) #R(t)



All_Cause_Survival_age_1 <- matrix(ncol = 5, nrow = nrow(cancer_codes))    #S(t)
All_Cause_Survival_age_2 <- matrix(ncol = 5, nrow = nrow(cancer_codes) )    #S(t)
# All_Cause_Survival_age_3 <- matrix(ncol = 5, nrow = nrow(cancer_codes) )    #S(t)

for (i in 1:nrow(cancer_codes_tibble)) {
  s <-  Predictions_Cubic_Net_age_1[[i]]$results
  s <-  s %>% filter(time.pts == 5)
  s2 <-  Predictions_Cubic_Net_age_2[[i]]$results
  s2 <-  s2 %>% filter(time.pts == 5)
  # s3 <-  Predictions_Cubic_Net_age_3[[i]]$results
  # s3 <-  s3 %>% filter(time.pts == 5)
  # 
  
  sp <- Predictions_Cubic_All_Cause_age_1[[i]]$results
  sp <-  sp %>% filter(time.pts == 5)
  sp2 <- Predictions_Cubic_All_Cause_age_2[[i]]$results
  sp2 <-  sp2 %>% filter(time.pts == 5)
  # sp3 <- Predictions_Cubic_All_Cause_age_3[[i]]$results
  # sp3 <-  sp3 %>% filter(time.pts == 5)
  # 
  
  Net_Survival_Five_Year_age_1[i,] <-
    c(ten_cancer_sites[i,1],
      ten_cancer_sites[i,2], 
      s$surv, s$surv.inf, s$surv.sup)
  Net_Survival_Five_Year_age_2[i,] <-
    c(ten_cancer_sites[i,1],
      ten_cancer_sites[i,2],
      s2$surv,
      s2$surv.inf,
      s2$surv.sup)
  # Net_Survival_Five_Year_age_3[i,] <-
  #   c(ten_cancer_sites[i,1],
  #     ten_cancer_sites[i,2],
  #     s3$surv,
  #     s3$surv.inf,
  #     s3$surv.sup)
  
  All_Cause_Survival_age_1[i,] <-
    c(ten_cancer_sites[i,1],
      ten_cancer_sites[i,2],
      sp$surv,
      sp$surv.inf,
      sp$surv.sup)
  All_Cause_Survival_age_2[i,] <-
    c(ten_cancer_sites[i,1],
      ten_cancer_sites[i,2],
      sp2$surv,
      sp2$surv.inf,
      sp2$surv.sup)
  # All_Cause_Survival_age_3[i,] <-
  #   c(ten_cancer_sites[i,1],
  #     ten_cancer_sites[i,2],
  #     sp3$surv,
  #     sp3$surv.inf,
  #     sp3$surv.sup)
}

#

Age_names_all <- as.data.frame(c("15-64", "65-99")) #, "Overall"

Net_Survival_Five_Year_age_1 <-
  as.data.frame(Net_Survival_Five_Year_age_1)
Net_Survival_Five_Year_age_2 <-
  as.data.frame(Net_Survival_Five_Year_age_2)
# Net_Survival_Five_Year_age_3 <-
#   as.data.frame(Net_Survival_Five_Year_age_3)

All_Cause_Survival_age_1 <- as.data.frame(All_Cause_Survival_age_1)
All_Cause_Survival_age_2 <- as.data.frame(All_Cause_Survival_age_2)
# All_Cause_Survival_age_3 <- as.data.frame(All_Cause_Survival_age_3)

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
# colnames(Net_Survival_Five_Year_age_3) <-
#   c("cancer_code",
#     "cancer",
#     "Five_Year_Net_Surv",
#     "NS_Lower_CI",
#     "NS_Upper_CI")

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
# colnames(All_Cause_Survival_age_3) <-
#   c("cancer_code",
#     "cancer",
#     "Five_Year_all_cause_Surv",
#     "OS_Lower_CI",
#     "OS_Upper_CI")



Net_Survival_Five_Year_age_1 <-
  Net_Survival_Five_Year_age_1 %>% mutate(age_cat = Age_names_all[1,])
Net_Survival_Five_Year_age_2 <-
  Net_Survival_Five_Year_age_2 %>% mutate(age_cat = Age_names_all[2,])
# Net_Survival_Five_Year_age_3 <-
#   Net_Survival_Five_Year_age_3 %>% mutate(age_cat = Age_names_all[3,])

All_Cause_Survival_age_1 <-
  All_Cause_Survival_age_1 %>% mutate(age_cat = Age_names_all[1,])
All_Cause_Survival_age_2 <-
  All_Cause_Survival_age_2 %>% mutate(age_cat = Age_names_all[2,])
# All_Cause_Survival_age_3 <-
#   All_Cause_Survival_age_3 %>% mutate(age_cat = Age_names_all[3,])


Net_Survival_Five_Year <-
  Net_Survival_Five_Year_age_1 %>% 
  full_join(Net_Survival_Five_Year_age_2) #%>%
# full_join(Net_Survival_Five_Year_age_3)

All_Cause_Survival <-
  All_Cause_Survival_age_1 %>% 
  full_join(All_Cause_Survival_age_2) #%>%
# full_join(All_Cause_Survival_age_3)



NS_OS2 <-Net_Survival_Five_Year %>% left_join(
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
  select(cancer,cancer_code,
         Five_Year_all_cause_Surv,OS_Lower_CI,OS_Upper_CI,
         Five_Year_Net_Surv,NS_Lower_CI,NS_Upper_CI,
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
                             age<4 ~ "0-15")) %>%
  filter(age_cat != "0-15") %>%
  left_join(ES2, by=c("country_code","age","sex"))%>% 
  group_by(country_label, cancer_label, age_cat) %>%
  dplyr::summarize(
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
    total_overall = sum(cases), 
    ES
  ) %>% 
  as.data.frame()%>%
  droplevels()


# PAFS_Overall <- PAFs_age_Cat %>% 
#   mutate(age_cat = "Overall") %>%
#   group_by(country_label, cancer_label, age_cat) %>%
#   dplyr::summarize(
#     country_code,
#     country_label,
#     cancer_code,
#     cancer_label,
#     age,
#     age_cat,
#     cases,
#     af.comb,
#     cases.prev,
#     cases.notprev,
#     total_overall = sum(cases), ES
#   ) %>% 
#   as.data.frame()%>%
#   droplevels()


PAFs2 <- PAFs_age_Cat %>%
  #  full_join(PAFS_Overall) %>%
  as.data.frame() %>%
  droplevels()%>%
  group_by(country_label, cancer_label, age_cat) %>%
  mutate(total_age_prev = sum(cases.prev)) %>%
  mutate(af.comb.agecat = sum(cases.prev) / sum(cases)) %>%
  mutate(ES = sum(ES*cases) / sum(cases)) %>%
  dplyr::summarize(country_code,
                   country_label,
                   cancer_code,
                   cancer_label,
                   age_cat,
                   af.comb.agecat,
                   total_overall=sum(cases),
                   ES
  ) %>%
  distinct() %>%
  arrange(cancer_label, age_cat)




NS_OS_PAF <- NS_OS %>% 
  left_join(PAFs2, by = c("cancer_code" = "cancer_code", "age_cat" ="age_cat")) %>% 
  left_join(Reference_Survival_Survcan,by=c("age_cat","cancer_code"))%>% #Add aggregated values here for the Thailand data. Need to combine age groups
  droplevels()%>%
  mutate(cancer=as.character(cancer))%>%
  distinct()

#Three AD calcs


#Applying the equation from Rutherford 2015 for AD. Needs to be updated to have scaled relative survival
Avoidable_Deaths <- matrix(ncol = 13, nrow = nrow(NS_OS_PAF)) #AD(t)

for (i in 1:nrow(NS_OS_PAF)){
  Expected_5_year_surv_mx <-NS_OS_PAF[i,]$ES
  #Preventable deaths
  AD_prev <- (NS_OS_PAF[i,]$af.comb.agecat) * NS_OS_PAF[i,]$total_overall * 
    (1 - NS_OS_PAF[i,]$Five_Year_Net_Surv) *
    Expected_5_year_surv_mx
  AD_prev_Lower <- (NS_OS_PAF[i,]$af.comb.agecat) * NS_OS_PAF[i,]$total_overall * 
    (1 - NS_OS_PAF[i,]$NS_Upper_CI) *
    Expected_5_year_surv_mx
  AD_prev_Upper <- (NS_OS_PAF[i,]$af.comb.agecat) * NS_OS_PAF[i,]$total_overall *
    (1 - NS_OS_PAF[i,]$NS_Lower_CI) *
    Expected_5_year_surv_mx
  
  # #Avoidable deaths (treatable: #check what the lower CI is called in the previous data frame
  
  AD_treat <-
    (NS_OS_PAF[i,]$surv_ref - NS_OS_PAF[i,]$Five_Year_Net_Surv) *
    Expected_5_year_surv_mx *(1 - NS_OS_PAF[i,]$af.comb.agecat) *
    NS_OS_PAF[i,]$total_overall
  AD_treat_Lower <-
    (NS_OS_PAF[i,]$surv_ref - NS_OS_PAF[i,]$NS_Upper_CI) * 
    Expected_5_year_surv_mx * (1 - NS_OS_PAF[i,]$af.comb.agecat)*
    NS_OS_PAF[i,]$total_overall
  AD_treat_Upper <-
    (NS_OS_PAF[i,]$surv_ref - NS_OS_PAF[i,]$NS_Lower_CI) * 
    Expected_5_year_surv_mx * (1 - NS_OS_PAF[i,]$af.comb.agecat) * 
    NS_OS_PAF[i,]$total_overall
  
  #Deaths not avoidable
  
  AD_unavoid <-
    (1 - NS_OS_PAF[i,]$af.comb.agecat) * NS_OS_PAF[i,]$total_overall * (1-NS_OS_PAF[i,]$surv_ref  * Expected_5_year_surv_mx)
  # AD_unavoid_Lower <-
  #   (1 - NS_OS_PAF[i,]$af.comb.agecat) * NS_OS_PAF[i,]$total_overall * (NS_OS_PAF[i,]$surv_ref - NS_OS_PAF[i,]$NS_Upper_CI * Expected_5_year_surv_mx)
  # AD_unavoid_Upper <-
  #   (1 - NS_OS_PAF[i,]$af.comb.agecat) * NS_OS_PAF[i,]$total_overall * (NS_OS_PAF[i,]$surv_ref - NS_OS_PAF[i,]$NS_Lower_CI * Expected_5_year_surv_mx)
  # 
  Avoidable_Deaths[i, ] <- c(    NS_OS_PAF[i, ]$country_code,
                                 NS_OS_PAF[i, ]$country_label,
                                 NS_OS_PAF[i, ]$age_cat,
                                 NS_OS_PAF[i, ]$cancer_code,
                                 NS_OS_PAF[i, ]$cancer_label,
                                 AD_treat,
                                 AD_treat_Lower,
                                 AD_treat_Upper,
                                 AD_prev,
                                 AD_prev_Lower,
                                 AD_prev_Upper,
                                 AD_unavoid,
                                 # AD_unavoid_Lower,
                                 #  AD_unavoid_Upper,
                                 NS_OS_PAF[i,]$total_overall
  )
}



colnames(Avoidable_Deaths) <- c("country_code",
                                "country_label",
                                "age_cat",
                                "cancer_code",
                                "cancer_label",
                                "AD_treat",
                                "AD_treat_Lower",
                                "AD_treat_Upper",
                                "AD_prev",
                                "AD_prev_Lower",
                                "AD_prev_Upper",
                                "AD_unavoid",
                                # "AD_unavoid_Lower",
                                #  "AD_unavoid_Upper",
                                "total")


MIR_Age_Cats_Thailand<-MIR_Age_Cats%>%
  filter(country_label=="Thailand")%>%
  select(-country_label, -cancer_label, -X)

Avoidable_Deaths<-Avoidable_Deaths%>%
  as.data.frame()%>%
  mutate(AD_treat=as.numeric(AD_treat))%>%
  mutate(AD_treat_Lower=as.numeric(as.character(AD_treat_Lower)))%>%
  mutate(AD_treat_Upper=as.numeric(as.character(AD_treat_Upper)))%>%
  mutate(AD_prev=as.numeric(as.character(AD_prev)))%>%
  mutate(AD_prev_Lower=as.numeric(as.character(AD_prev_Lower)))%>%
  mutate(AD_prev_Upper=as.numeric(as.character(AD_prev_Upper)))%>%
  mutate(AD_unavoid=as.numeric(as.character(AD_unavoid)))%>%
  mutate(total=as.numeric(as.character(total)))%>%
  mutate(AD_sum=AD_treat+AD_prev+AD_unavoid)%>%
  mutate(country_code=as.numeric(as.character(country_code)))%>%
  # filter(total<AD_treatprev)%>%
  mutate(cancer_code=as.numeric(cancer_code))%>%
#  left_join(MIR_Age_Cats_Thailand, by=c("country_code","cancer_code", "age_cat"))
as.data.frame()


Avoidable_Deaths_overall<-Avoidable_Deaths%>%
  group_by(cancer_code,age_cat)%>%
  mutate(age_cat="Overall")%>%
  mutate(total=sum(total))



Avoidable_Deaths_overall<-Avoidable_Deaths%>%
  as.data.frame()%>%
  mutate(age_cat="Overall")%>%
  ungroup()%>%
  group_by(country_code,cancer_label,age_cat)%>%
  dplyr::summarise(country_code, country_label,
                   cancer_code, cancer_label,
         total=sum(total),
         AD_prev= sum(AD_prev),
           AD_prev_Lower= sum(AD_prev_Lower),
         AD_prev_Upper= sum(AD_prev_Upper),
          AD_treat = sum(AD_treat),
           AD_treat_Lower= sum(AD_treat_Lower),
          AD_treat_Upper= sum(AD_treat_Upper),
          AD_unavoid = sum(AD_unavoid),
           AD_sum=sum(AD_sum),
         )%>%
#  mutate(MIR=sum(total*MIR)/sum(total))%>%
  distinct()%>%
  as.data.frame()

Avoidable_Deaths_age_cat<-Avoidable_Deaths%>%
  group_by(cancer_code, age_cat)%>%
  # mutate(AD_prev= sum(AD_prev))%>%
  # mutate(AD_treat = sum(AD_treat))%>%
  # mutate(AD_unavoid = sum(AD_unavoid))%>%
  full_join(Avoidable_Deaths_overall)%>%
  #  mutate(AD_sum=AD_prev + AD_unavoid + AD_treat)%>%
  #  mutate(total_overall=sum(total_overall))%>%
  distinct()%>%
  # arrange(cancer, age_cat)%>%
  as.data.frame()#%>%
#left_join(MIR_Age_Cats_Thailand, by=c("country_code","cancer_code", "age_cat"))



NS_OS_PAF

###############
#
#Exporting Results
#
###############


write.csv(Avoidable_Deaths, "~/Documents/R_Projects/Data/Thai_AD.csv")
write.csv(NS_OS, "~/Documents/R_Projects/Data/Thai_NS_OS.csv")


# 
# library(ranger)
# library(mlr3)
# library(caTools)
# library(survival)
# 
# # Random Forest model as a different approach
# # Using the ranger package 
# # 
# # 
# # 
# # 
# # 
# 
# 
# # ranger model.Too complex for the whole dataset on this laptop
# # would be interesting to try this again with a more powerful computer
# #https://rviews.rstudio.com/2017/09/25/survival-analysis-with-r/
# 
# #creating training and test datasets
Thai_Surv_test<-Thai_Surv%>%
  #full_join(Thai_Surv_overall)%>%
  filter(cancer_code==20)
# 
# ten_cancer_sites
# 
# T_split<-sample.split(Thai_Surv_test,SplitRatio = 0.3)
# 
# T_train=subset(Thai_Surv_test,
#                T_split==TRUE)
# T_test=subset(Thai_Surv_test,
#               T_split==FALSE)
# 
# 
# 
# 
# r_fit <- ranger(Surv(surv_yydd,event1) ~ age_cat,
#                 data = T_train,
#                 mtry = 1,
#                 importance = "permutation",
#                 splitrule = "extratrees",
#                 verbose = TRUE,
#                 num.trees = 100)
# 
# # Average the survival models
# death_times <- r_fit$unique.death.times
# avg_prob <- sapply(surv_prob,mean)
# 
# # Plot the survival models for each patient
# plot(r_fit$unique.death.times,r_fit$survival[1,],
#      type = "l",
#      ylim = c(0,1),
#      xlim=c(0,5),
#      col = "red",
#      xlab = "Years",
#      ylab = "survival",
#      main = "Cancer Survranger Survival Curves")
# 
# #
# 
# cols <- colors()
# for (n in sample(c(2:dim(T_train)[1]), 80)){
#   lines(r_fit$unique.death.times, r_fit$survival[n,], type = "l", col = cols[n])
# }
# lines(death_times, avg_prob, lwd = 2)
# legend(1000, 0.7, legend = c('Average = black'))

#Kaplan Meier Curves for OS

# library(survival)
# library(ggplot2)
# library(ggfortify)
# library(survminer)

#Plotting Survival Curves Using ggplot2 and ggfortify:


Y = Surv(Thai_Surv_test$surv_yy, Thai_Surv_test$event1 == 1)
xlim=c(0,5)


kmfit = survfit(Y ~ 1)

summary(kmfit, times = c(seq(0, 5, by = 100)))

plot(kmfit, xlim=c(0,5),
     lty = c("solid", "dashed"),
     col = c("black", "grey"),
     xlab = "Survival Time In Years",
     ylab = "Survival Probabilities")

title("Breast Cancer Survival Kaplan Meier")

#First setup survival object
km <- Surv(time =Thai_Surv_test$surv_yydd , event = Thai_Surv_test$event1)
#Fit Kaplan Meier, stratifying by treatment
km_breast_cancer<-survfit(km~age_cat, 
                          data=Thai_Surv_test,
                          type='kaplan-meier',conf.type='log')

library(survminer)

ggsurvplot(km_breast_cancer, xlim = c(0, 5))+
  labs(x = " Survival Time (years) ",
       y = "Survival Probabilities",
       title = "Kaplan Meier Of Breast Cancer Patients in Thailand")

# theme(plot.title = element_text(hjust = 0.5),
#       axis.title.x = element_text(face="bold", colour="#FF7A33", size = 12),
#       axis.title.y = element_text(face="bold", colour="#FF7A33", size = 12),
#       legend.title = element_text(face="bold", size = 10))

