###############################################
#
# Net survival and Avoidable deaths
#  Date: 09/06/2022
# Version 2.22
#
# Works for multiple cancer sites currently.
#
#Needs to be synced
#
###############################################


#Avoidable Deaths due to Risk Factors for various Cancer sites
library(readxl)
#library(plyr)
library(dplyr)
library(tidyverse)
library(stringr)
library(haven)
library(mexhaz)
library(readr)
library(ggplot2)
library(relsurv)
library(readstata13)
library(janitor)





# 
# 
# #Data set imports
# #GLobocan
# g<-read.csv("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Research visits\\Oliver_Langselius\\Globocan2020\\Globocan.csv")
# pops_g<-read.csv("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Research visits\\Oliver_Langselius\\Globocan2020\\source\\Pops.csv")
# 
# pops_sorted<-read.csv("C:\\Users\\langseliuso\\OneDrive - IARC\\Desktop\\Avoidable Deaths Breast Cancer Project\\R Analysis\\population2.csv")
# bc<- read.csv("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Research visits\\Oliver_Langselius\\SURVCANALL_cc_breast.csv")
# #Loading prevelance project incidence and mortality
# prev<-read.dta13("\\\\Inti\\cin\\Studies\\Prevalence\\2020\\_data\\incidence\\prelim\\incident_mortality_asr-2020.dta")
# HDI3<-read.csv("\\\\Inti\\cin\\Studies\\Prevalence\\2020\\_data\\hdi\\hdi_2020.csv")
# 


#Reading all the variables


# GCO_country_info.csv has correct country_label variable to match with pop_mort2
missing_CC<-data.frame(c("Seychelles", "Martinique"), c(10001,10002))
colnames(missing_CC) <- c("country_label","country_code")

country_codes <-
  read.csv("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Research visits\\Oliver_Langselius\\Data\\GCO_country_info.csv", stringsAsFactors = FALSE) %>% 
  filter(country_code<900) %>% 
  mutate(country_label = replace(country_label, country_label == "Iran, Islamic Republic of", "Iran")) %>%
  mutate(country_label = replace(country_label, country_label == "Korea, Republic of", "South Korea")) %>%
  select(country_code, country_label)%>% 
  full_join(missing_CC)
  
  #mutate(region = replace(cancer_label, cancer_label == "Colon", "Colorectal")) %>%
  

#life tables

life_table<-read.csv("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\life_table_SURVCAN.csv")%>%
  mutate(region = replace(region, region == "Cote d'Ivoire", "Côte d'Ivoire")) %>%
  mutate(region = replace(region, region == "France", "Martinique")) %>%
  left_join(country_codes, by = c("region"="country_label"))%>%
dplyr::rename("country"="region")%>%
  select(-country)
  

PAFs10 <- read.csv("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Research visits\\Oliver_Langselius\\Data\\combinedPAFs_cases_08.06.2022_Prostate.csv")

PAFs<-PAFs10%>%
  mutate(cancer_label=as.character(cancer_label))%>%
  mutate(cancer_label = replace(cancer_label, cancer_label == "Colon", "Colorectal")) %>%
  mutate(cancer_label = replace(cancer_label, cancer_label == "Rectum", "Colorectal")) %>%
  mutate(cancer_code  = replace(cancer_code, cancer_code == 8, 38))%>%
  mutate(cancer_code  = replace(cancer_code, cancer_code == 9, 38))%>%
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

HDI <-read.csv("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Research visits\\Oliver_Langselius\\Data\\HDI_2019.csv") %>% as.data.frame()


bcan_SURV <-read.csv("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Research visits\\Oliver_Langselius\\SURVCANALL_cc_breast_11May2022.csv")# %>% as.data.frame() Data\\Oliver_Langselius\\SURVCANALL_cc.csv

Thailand_popmort <-read.csv("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Research visits\\Oliver_Langselius\\Thai Data\\popmort_Thailand.csv") %>% as.data.frame() %>%
  left_join(country_codes, by = c("region" = "country_label"))


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


#Thailand_expected_Survival<-read.csv("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Research visits\\Oliver_Langselius\\Data\\Thailand_expected_Survival.csv")%>%as.data.frame()

# Reference_Survival<-read.csv("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Research visits\\Oliver_Langselius\\Data\\Reference_Survival.csv")%>%
#   as.data.frame()%>%
#   select( age, 
#           cancer_code, 
#           rel_surv)%>%
#   rename("surv_ref"="rel_surv")%>%
#   distinct()

Reference_Survival_Survcan<-read.csv("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Research visits\\Oliver_Langselius\\Data\\Reference_Survival_Survcan.csv")%>%
  as.data.frame()%>%
  select( age_cat, 
          cancer_code, 
          rel_surv)%>%
  rename("surv_ref"="rel_surv")%>%
  distinct()




load("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Research visits\\Oliver_Langselius\\Data\\ES_dt.RData")

ES2<-ES_dt%>%
  filter(time==1000)%>%
  select(-time)%>%
  rename("ES"="SurvExp")
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




ten_cancer_sites <-
  Cancer_codes %>% 
  filter(cancer_code %in% c(20))#%>%6, 7, 11, 13, 15, 20, 23, 27, 30, 38
 # mutate(cancer_label=replace(cancer_label,cancer_label=="Unspecified sites","Colorectal"))



#Checking cancer codes in various files
a<-PAFs%>%summarize(cancer_code,cancer_label)%>%distinct()%>%filter(cancer_code%in%ten_cancer_sites$cancer_code)
b<-simulated_overall%>%
  ungroup()%>%
  summarize(cancer_code, cancer_label)%>%
  distinct()
survcancancer<-bSURV%>%
  ungroup()%>%
  summarize(cancer_code, cancer)%>%
  distinct()

country_codes



#Prepping cancer real world data
bcan_SURV2 <- bcan_SURV %>%
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
  mutate(country=replace(country,country=="Cote d'Ivoire", "Côte d'Ivoire"))%>%
  mutate(country=replace(country,country=="France", "Martinique"))%>%
  left_join(country_codes, by = c("country"="country_label"))%>%
  droplevels()%>%
  mutate(last_FU_age = round(age + surv_dd/365.15)) %>% #creating variable for age of death
  mutate(last_FU_year = round(year + surv_dd/365.15))%>%  #creating variable for year of death
  mutate(sex = replace(sex, sex == "Male", 1)) %>%
  mutate(sex = replace(sex, sex == "Female", 2)) %>%
  mutate(sex = as.integer(sex)) %>%
  left_join(life_table, by = c(
    "last_FU_age" = "age",
    "last_FU_year" = "year",
    "country_code")) %>%
  droplevels()%>%
 # filter(last_FU_year > 2009 & last_FU_year <= 2014) %>%
 filter(!is.na(mx)) %>% 
  droplevels() %>%
  left_join(Cancer_codes_Survcan, by = c("cancer" = "cancer"))#%>%
#  mutate(cancer = replace(cancer, cancer == "Colon (C18)", "Colorectal")) %>%
#  mutate(cancer = replace(cancer, cancer == "Rectum (C19-20)", "Colorectal")) %>%
 # filter(cancer_code %in% ten_cancer_sites$cancer_code)
#  filter(is.na(bSURV$mx))

bcan_SURV3 <- bcan_SURV2%>%
  mutate(surv_yydd=surv_dd/365.15)%>%
  mutate(event1=case_when(dead==1 &      surv_yydd<=5 ~ 1,
                          dead==1 & surv_yydd>5 ~ 0,
                          dead==0 ~ 0
  )) %>%
     group_by(country,age_cat)%>%
     mutate(max=max(surv_yydd))%>%
     filter(max>=5)%>%ungroup()


a<-bcan_SURV3%>%select(country,country_code)%>%distinct()%>%as.data.frame()
b<-life_table%>%select(country,country_code)%>%distinct()%>%as.data.frame()



#modifying so last five years of follow up



bcan_SURV11 <- bcan_SURV3%>%
  select(region_lab, country, doi, last_FU_age,age,surv_yytot,year,cancer_code)%>%
  group_by(region_lab, country, cancer_code)%>%
  summarize(end_FU = max(year))%>%
  as.data.frame()


bSURV<-bcan_SURV3%>%left_join(bcan_SURV11)%>%
  ungroup()%>%
  group_by(country)#%>%
# filter(year>=end_FU-5)

# mutate(surv_yy=case_when(surv_yy<=5 ~ 5
# )
# )

#bSURV$sex <- as.integer(bSURV$sex)

#survcan_test<-bSURV%>%filter(cancer_code==30)


#age categories
#bSURV_overall <- bSURV %>% mutate(age_cat = "Overall")
bSURV_Lower <- bSURV %>% filter(age_cat == "15-64")%>% ungroup()%>%  droplevels()
bSURV_Upper <- bSURV %>% filter(age_cat == "65-99")%>% ungroup()%>%  droplevels()

bSURV_age_cats <- bSURV #bSURV_overall %>% full_join(bSURV)




table(is.na(bSURV$mx)) #60 people have ages above 100 but were between 15-99 years at age of diagnosis. Should they be excluded?

names(table(bcan_SURV2$country))

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
bSURV <- bSURV %>% ungroup()%>%  droplevels()

country_names_Survcan<- as_tibble(names(table(bSURV$country)))
names(country_names_Survcan)[names(country_names_Survcan) == "value"] <- "country"
country_names_Survcan <- country_names_Survcan%>%#For regression needs to be in this form
as.data.frame()%>%mutate(country=as.character(country))%>%slice(-c(4,13,19,22,31))

country_codes <- as_tibble(names(table(bSURV$country_code))) #Needs to be tibble for predictions
names(country_codes)[names(country_codes) == "value"] <- "country_code"
country_codes <- as.data.frame(country_codes)%>%slice(-c(4,13,19,22,31)) #For regression needs to be in this form


cancer_types <- names(table(bSURV$cancer))%>%as.data.frame() 
names(cancer_types)[names(cancer_types) == "value"] <- "cancer"
cancer_types <-  as.data.frame(cancer_types) #For regression needs to be in this form

cancer_codes <- as_tibble(names(table(ten_cancer_sites$cancer_code))) #Needs to be tibble for predictions
names(cancer_codes)[names(cancer_codes) == "value"] <- "cancer_code"
cancer_codes <- as.data.frame(cancer_codes) #For regression needs to be in this form

sex <-as_tibble(names(table(bSURV$sex))) #Needs to be tibble for predictions
names(sex)[names(sex) == "value"] <- "sex"
sex <- as.data.frame(sex) #For regression needs to be in this form




#Cubic base model BY COUNTRY


Cubic_age_1 <- list()
Cubic_age_2 <- list()


#Cubic base model BY Cancer type

for (i in 1:nrow(country_codes)) {
  b1 <- bSURV_Lower %>% filter(country_code == country_codes[i,])
  b2 <- bSURV_Upper %>% filter(country_code == country_codes[i,])

  
  
  k1 <- median(b1$surv_yydd)
  k2 <- median(b2$surv_yydd)
  
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

}






#Updating the models with mortality rates

#Country updated models list initializing
Cubic_Cancer_age_1 <- list()
Cubic_Cancer_age_2 <- list()
#Cubic_Cancer_overall <- list()

#Cubic excess hazard by country
for (i in 1:nrow(country_codes)){
  b1 <- bSURV_Lower %>% filter(country_code == country_codes[i,])%>%as.data.frame()
  b2 <- bSURV_Upper %>% filter(country_code == country_codes[i,])%>%as.data.frame()
  
  k1 <- median(b1$surv_yydd)
  k2 <- median(b2$surv_yydd)
  
  try(Cubic_Cancer_age_1[[i]] <-
        update(Cubic_age_1[[i]], expected = "mx",      numHess=TRUE,
               fnoptim="optim"))
  try(Cubic_Cancer_age_2[[i]] <-
        update(Cubic_age_2[[i]], expected = "mx",      numHess=TRUE,
               fnoptim="optim"))

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


country_codes_tibble<-
  country_codes %>%
  mutate(country_code=as.integer(country_code))%>%
  as_tibble()

cancer_types_tibble <-
  cancer_types %>% as_tibble() #predict only works with tibble data structure...

cancer_codes_tibble <-
  cancer_codes %>% 
  mutate(cancer_code=as.integer(cancer_code))%>%
  as_tibble() #predict only works with tibble data structure...
#Cubic predictions by country

for (i in 1:nrow(country_codes_tibble)) {
  try(AC1 <- predict(Cubic_age_1[[i]], time.pts = time, data.val = country_codes_tibble[i,]))
  try(AC2 <- predict(Cubic_age_2[[i]], time.pts = time, data.val = country_codes_tibble[i,]))
  # try(AC3 <- predict(Cubic_overall[[i]], time.pts = time, data.val = cancer_codes_tibble[i,]))
  
  
  try(HP1 <- predict(Cubic_Cancer_age_1[[i]],
                     time.pts = time,
                     data.val = country_codes_tibble[i,]))
  try(HP2 <- predict(Cubic_Cancer_age_2[[i]],
                     time.pts = time,
                     data.val = country_codes_tibble[i,]))

  
  
  Predictions_Cubic_All_Cause_age_1[[i]] <- AC1
  Predictions_Cubic_All_Cause_age_2[[i]] <- AC2

  
  
  Predictions_Cubic_Net_age_1[[i]] <- HP1
  Predictions_Cubic_Net_age_2[[i]] <- HP2

}


#Extracting prediction data five year avoidable deaths prediction and survival

Net_Survival_Five_Year_age_1 <- matrix(ncol = 5, nrow = nrow(country_codes)) #R(t)
Net_Survival_Five_Year_age_2 <- matrix(ncol = 5, nrow = nrow(country_codes)) #R(t)
# Net_Survival_Five_Year_age_3 <- matrix(ncol = 5, nrow = nrow(cancer_codes)) #R(t)



All_Cause_Survival_age_1 <- matrix(ncol = 5, nrow = nrow(country_codes))    #S(t)
All_Cause_Survival_age_2 <- matrix(ncol = 5, nrow = nrow(country_codes) )    #S(t)
# All_Cause_Survival_age_3 <- matrix(ncol = 5, nrow = nrow(cancer_codes) )    #S(t)

for (i in 1:nrow(country_codes_tibble)) {
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

  Net_Survival_Five_Year_age_1[i,] <-
    c(country_codes[i,],
      country_names_Survcan[i,], 
      s$surv, 
      s$surv.inf, 
      s$surv.sup)


  All_Cause_Survival_age_1[i,] <-
    c(country_codes[i,],
      country_names_Survcan[i,], 
      s$surv, 
      s$surv.inf, 
      s$surv.sup)
  All_Cause_Survival_age_2[i,] <-
    c(country_codes[i,],
      country_names_Survcan[i,], 
      s$surv, 
      s$surv.inf, 
      s$surv.sup)
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
  c("country_code",
    "country",
    "Five_Year_Net_Surv",
    "NS_Lower_CI",
    "NS_Upper_CI")
colnames(Net_Survival_Five_Year_age_2) <-
  c("country_code",
    "country",
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
  c("country_code",
    "country",
    "Five_Year_all_cause_Surv",
    "OS_Lower_CI",
    "OS_Upper_CI")
colnames(All_Cause_Survival_age_2) <-
  c("country_code",
    "country",
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
    "country" = "country",
    "country_code" = "country_code",
    "age_cat" = "age_cat"
  )
)


NS_OS2$country_code <- as.numeric(as.character(NS_OS2$country_code))

bSURV$country_code <-
  as.numeric(as.character(bSURV$country_code))




NS_OS <-
  NS_OS2 %>% left_join(
    bSURV_age_cats,
    by = c(
      "country" = "country",
      "country_code" = "country_code",
      "age_cat" = "age_cat"
    )
  ) %>%
  select(country_code, country, cancer,cancer_code,
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
#  filter(country_label == "Thailand") %>%
  mutate(age_cat = case_when(age >= 4 & age < 14 ~ "15-64",
                             age >= 14 ~ "65-99",
                             age<4 ~ "0-15")) %>%
  filter(age_cat != "0-15") %>%
  left_join(ES2, by=c("country_code","age","sex"))%>% 
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
    total_overall = sum(cases), 
    ES
  ) %>% 
  as.data.frame()%>%
  droplevels()


# PAFS_Overall <- PAFs_age_Cat %>% 
#   mutate(age_cat = "Overall") %>%
#   group_by(country_label, cancer_label, age_cat) %>%
#   summarize(
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
  summarize(country_code,
            country_label,
            cancer_code,
            cancer_label,
            age_cat,
            af.comb.agecat,
            total_overall=sum(cases),
            ES
  ) %>%
  distinct() %>%
  arrange(cancer_label, age_cat) %>%
  ungroup()%>%
  select(-country_label)




NS_OS_PAF <- NS_OS %>% 
  left_join(PAFs2, by = c("country_code","cancer_code" = "cancer_code", "age_cat" ="age_cat")) %>% 
  left_join(Reference_Survival_Survcan,by=c("age_cat","cancer_code"))%>% #Add aggregated values here for the Thailand data. Need to combine age groups
  droplevels()%>%
  mutate(cancer=as.character(cancer))%>%
  distinct()


#Three AD calcs


#Applying the equation from Rutherford 2015 for AD. Needs to be updated to have scaled relative survival
Avoidable_Deaths <- matrix(ncol = 9, nrow = nrow(NS_OS_PAF)) #AD(t)

for (i in 1:nrow(NS_OS_PAF)){
  Expected_5_year_surv_mx <-NS_OS_PAF[i,]$ES
  
  
  #Rutherford AD
  
  AD <-
    (NS_OS_PAF[i,]$surv_ref - NS_OS_PAF[i,]$Five_Year_Net_Surv) *
    Expected_5_year_surv_mx  *
    NS_OS_PAF[i,]$total_overall
  AD_Lower <-
    (NS_OS_PAF[i,]$surv_ref - NS_OS_PAF[i,]$NS_Upper_CI) * 
    Expected_5_year_surv_mx * 
    NS_OS_PAF[i,]$total_overall
  AD_Upper <-
    (NS_OS_PAF[i,]$surv_ref - NS_OS_PAF[i,]$NS_Lower_CI) * 
    Expected_5_year_surv_mx *
    NS_OS_PAF[i,]$total_overall
  
  
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
                                 AD,
                                 AD_Lower,
                                 AD_Upper,
                                 #AD_treat,
                                 #AD_treat_Lower,
                                 #AD_treat_Upper,
                                 #AD_prev,
                                 #AD_prev_Lower,
                                 #AD_prev_Upper,
                                 #AD_unavoid,
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
                                 "AD",
                                 "AD_Lower",
                                "AD_Upper",
                                #"AD_treat",
                                # "AD_treat_Lower",
                                # "AD_treat_Upper",
                                # "AD_prev",
                                # "AD_prev_Lower",
                                # "AD_prev_Upper",
                                # "AD_unavoid",
                                # "AD_unavoid_Lower",
                                #  "AD_unavoid_Upper",
                                "total")


#MIR_Age_Cats_Survcan<-MIR_Age_Cats%>%
 # filter(country_label=="Thailand")%>%
#  select(-country_label, -cancer_label, -X)

Avoidable_Deaths<-Avoidable_Deaths%>%
  as.data.frame()%>%
  mutate(AD=as.numeric(AD))%>%
  mutate(AD_Lower=as.numeric(as.character(AD_Lower)))%>%
  mutate(AD_Upper=as.numeric(as.character(AD_Upper)))%>%
  # mutate(AD_treat=as.numeric(AD_treat))%>%
  # mutate(AD_treat_Lower=as.numeric(as.character(AD_treat_Lower)))%>%
  # mutate(AD_treat_Upper=as.numeric(as.character(AD_treat_Upper)))%>%
  # mutate(AD_prev=as.numeric(as.character(AD_prev)))%>%
  # mutate(AD_prev_Lower=as.numeric(as.character(AD_prev_Lower)))%>%
  # mutate(AD_prev_Upper=as.numeric(as.character(AD_prev_Upper)))%>%
  # mutate(AD_unavoid=as.numeric(as.character(AD_unavoid)))%>%
  mutate(total=as.numeric(as.character(total)))%>%
#  mutate(AD_sum=AD_treat+AD_prev+AD_unavoid)%>%
  mutate(country_code=as.numeric(as.character(country_code)))%>%
  # filter(total<AD_treatprev)%>%
  mutate(cancer_code=as.numeric(cancer_code))#%>%
#  left_join(MIR_Age_Cats_Survcan, by=c("country_code","cancer_code", "age_cat"))


Avoidable_Deaths_overall<-Avoidable_Deaths%>%
  as.data.frame()%>%
  mutate(age_cat="Overall")%>%
  ungroup()%>%
  group_by(country_code,cancer_code,age_cat)%>%
  dplyr::summarise(country_code, country_label,
                   cancer_code, cancer_label,
                   total=sum(total),
                    AD= sum(AD),
                    AD_Lower= sum(AD_Lower),
                    AD_Upper= sum(AD_Upper)
                   # AD_prev= sum(AD_prev),
                   # AD_prev_Lower= sum(AD_prev_Lower),
                   # AD_prev_Upper= sum(AD_prev_Upper),
                   # AD_treat = sum(AD_treat),
                   # AD_treat_Lower= sum(AD_treat_Lower),
                   # AD_treat_Upper= sum(AD_treat_Upper),
                   # AD_unavoid = sum(AD_unavoid),
                   # AD_sum=sum(AD_sum),
  )%>%
  #  mutate(MIR=sum(total*MIR)/sum(total))%>%
  distinct()%>%
  as.data.frame()

Avoidable_Deaths_age_cat<-Avoidable_Deaths%>%
  group_by(country_code,cancer_code, age_cat)%>%
  mutate(AD= sum(AD))%>%
  # mutate(AD_prev= sum(AD_prev))%>%
  # mutate(AD_treat = sum(AD_treat))%>%
  # mutate(AD_unavoid = sum(AD_unavoid))%>%
  full_join(Avoidable_Deaths_overall)%>%
  #  mutate(AD_sum=AD_prev + AD_unavoid + AD_treat)%>%
  #  mutate(total_overall=sum(total_overall))%>%
  distinct()%>%
  # arrange(cancer, age_cat)%>%
  as.data.frame()#%>%
#left_join(MIR_Age_Cats_Survcan, by=c("country_code","cancer_code", "age_cat"))



NS_OS_PAF

###############
#
#Exporting Results
#
###############


write.csv(Avoidable_Deaths, "~/Documents/R_Projects/Data/Thai_AD.csv")
write.csv(NS_OS, "~/Documents/R_Projects/Data/Thai_NS_OS.csv")



