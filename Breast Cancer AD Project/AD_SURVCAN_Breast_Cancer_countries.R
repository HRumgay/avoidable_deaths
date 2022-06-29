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
library(dplyr)
library(tidyverse)
library(stringr)
library(haven)
library(mexhaz)
library(readr)
library(ggplot2)
library(relsurv)
# 
# 
# #Data set imports
# life_file_list<-list.files('\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Research visits\\Oliver_Langselius\\lifetables\\Expanded_2018', full.names=TRUE)
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
# #Life tables - used for analysis
# #To clean all names from the _ symbol
# life<-plyr::ldply(life_file_list,read.dta13)
# life<-life%>%as.data.frame()%>%
#   clean_names()%>%
#   filter(!is.na(mx))
# 
# Mauritius<-read.dta13(life_file_list[22])
# Mauritius<-Mauritius%>%clean_names()%>%rename("region"="country")
# 
# life<-life%>%full_join(Mauritius)
# 
# #Load mortality rates for survival analysis. Correct and use probability
# life_table<-life%>%filter(sex==2)%>%
#   select(region,year,age,mx,prob)%>%
#   mutate(region=replace(region,region=="Korea","South Korea"))%>%
#   mutate(region=replace(region,region=="South_Africa","South Africa"))%>%
#   mutate(region=replace(region,region=="Cote_D`ivoire","Cote d'Ivoire"))%>%
#   mutate(region=replace(region,region=="Saudi_Arabia","Saudi Arabia"))%>%
#   mutate(region=replace(region,region=="Costa_Rica","Costa Rica"))%>%
#   mutate(region=replace(region,region=="Bahain","Bahrain"))%>%
#   mutate(region=replace(region,region=="Costa_Rica","Costa Rica"))%>%
#   mutate(region=replace(region,region=="Ethiopy","Ethiopia"))
# 
# 
# #adding the mortality rates of 2015 to the years 2016-2020
# 
# life_table_2019<-life_table%>% filter(year==2015)%>%
#   mutate(year=replace(year,year==2015,2019))
# life_table_2020<-life_table%>% filter(year==2015)%>%
#   mutate(year=replace(year,year==2015,2020))
# 
# life_table_complete<-life_table %>%
#   full_join(life_table_2019) %>%
#   full_join(life_table_2020) 



#Reading all the variables

# GCO_country_info.csv has correct country_label variable to match with pop_mort2
country_codes <-
  read.csv("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Research visits\\Oliver_Langselius\\Data\\GCO_country_info.csv", stringsAsFactors = FALSE) %>% 
  filter(country_code<900) %>% 
  select(country_code, country_label)

PAFs10 <- read.csv("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Research visits\\Oliver_Langselius\\Data\\combinedPAFs_cases_08.06.2022_Prostate.csv")

PAFs<-PAFs10%>%
    mutate(cancer_label=as.character(cancer_label))%>%
  mutate(cancer_label = replace(cancer_label, cancer_label == "Colon", "Colorectal")) %>%
  mutate(cancer_label = replace(cancer_label, cancer_label == "Rectum", "Colorectal")) %>%
  mutate(cancer_code = replace(cancer_code, cancer_code == 8, 38))%>%
  mutate(cancer_code = replace(cancer_code, cancer_code == 9, 38))%>%
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
Thailand_Survcan <-read.csv("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Research visits\\Oliver_Langselius\\Thai Data\\ASTHABAN_cc_Oliver.csv")# %>% as.data.frame()
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
  rename("surv_ref"="rel_surv")%>%
  distinct()

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
  filter(cancer_code %in% c(6, 7, 11, 13, 15, 20, 23, 27, 30, 38))%>%
  mutate(cancer_label=as.character(cancer_label))%>%
  mutate(cancer_label=replace(cancer_label,cancer_label=="Unspecified sites","Colorectal"))

Cancer_codes_Survcan<-Cancer_codes_Survcan%>%
  mutate(cancer=as.character(cancer))


#Checking cancer codes in various files
a<-PAFs%>%summarize(cancer_code,cancer_label)%>%distinct()%>%filter(cancer_code%in%ten_cancer_sites$cancer_code)
b<-simulated_overall%>%
  ungroup()%>%
  summarize(cancer_code, cancer_label)%>%
  distinct()
survcancancer<-Thai_Surv%>%
  ungroup()%>%
  summarize(cancer_code, cancer)%>%
  distinct()

cancer_popmort<-Thailand_popmort2%>%
  ungroup()%>%
  summarize(country_code, region)%>%
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
  mutate(sex=as.character(sex))%>%
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


Thai_Surv11<- Thai_Surv3%>%
  select(region_lab, country, doi, last_FU_age,age,surv_yytot,year,cancer_code)%>%
  group_by(region_lab,country,cancer_code)%>%
  summarize( end_FU=max(year))%>%
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

NS_OS$Five_Year_Net_Surv <- as.numeric(as.character(NS_OS$Five_Year_Net_Surv))
NS_OS$NS_Lower_CI <- as.numeric(as.character(NS_OS$NS_Lower_CI))
NS_OS$NS_Upper_CI <- as.numeric(as.character(NS_OS$NS_Upper_CI))
NS_OS$Five_Year_all_cause_Surv <-
  as.numeric(as.character(NS_OS$Five_Year_all_cause_Surv))
NS_OS$OS_Lower_CI <- as.numeric(as.character(NS_OS$OS_Lower_CI))
NS_OS$OS_Upper_CI <- as.numeric(as.character(NS_OS$OS_Upper_CI))
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
  arrange(cancer_label, age_cat)




NS_OS_PAF <- NS_OS %>% 
  left_join(PAFs2, by = c("cancer_code" = "cancer_code", "age_cat" ="age_cat")) %>% 
  left_join(Reference_Survival_Survcan,by=c("age_cat","cancer_code"))%>% #Add aggregated values here for the Thailand data. Need to combine age groups
  droplevels()%>%
  mutate(cancer=as.character(cancer))%>%
  mutate(cancer_code=as.numeric(cancer_code))%>%
  mutate(country_label=as.character(country_label))%>%
  mutate(country_code=as.numeric(country_code))%>%
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
  mutate(AD_treat=as.numeric(as.character(AD_treat)))%>%
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
  left_join(MIR_Age_Cats_Thailand, by=c("country_code","cancer_code", "age_cat"))


Avoidable_Deaths_overall<-Avoidable_Deaths%>%
  mutate(age_cat="Overall")%>%
  group_by(cancer_code, age_cat)%>%
  mutate(total=sum(total))%>%
  mutate(AD_prev= sum(AD_prev))%>%
  mutate(AD_prev_Lower= sum(AD_prev_Lower))%>%
  mutate(AD_prev_Upper= sum(AD_prev_Upper))%>%
  mutate(AD_treat = sum(AD_treat))%>%
  mutate(AD_treat_Lower= sum(AD_treat_Lower))%>%
  mutate(AD_treat_Upper= sum(AD_treat_Upper))%>%
  mutate(AD_unavoid = sum(AD_unavoid))%>%
  mutate(AD_sum=AD_treat+AD_prev+AD_unavoid)%>%
  mutate(MIR=sum(total*MIR)/sum(total))%>%
  distinct()%>%
  as.data.frame()

Avoidable_Deaths_age_cat<-Avoidable_Deaths%>%
  group_by(cancer_code, age_cat)%>%
  mutate(AD_prev= sum(AD_prev))%>%
  mutate(AD_treat = sum(AD_treat))%>%
  mutate(AD_unavoid = sum(AD_unavoid))%>%
  full_join(Avoidable_Deaths_overall)%>%
  #  mutate(AD_sum=AD_prev + AD_unavoid + AD_treat)%>%
  #  mutate(total_overall=sum(total_overall))%>%
  distinct()%>%
  # arrange(cancer, age_cat)%>%
  as.data.frame()#%>%
#left_join(MIR_Age_Cats_Thailand, by=c("country_code","cancer_code", "age_cat"))



NS_OS_PAF
