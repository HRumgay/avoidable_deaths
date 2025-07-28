###############################################sites
#
# Net survival and Avoidable deaths - File Lead
# Date: 20/01/2023
# Version 4.0
#
# Works for multiple cancer sites currently.
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
library(janitor)
library(data.table)

setwd("C:\\Users\\langseliuso\\Documents\\GitHub\\avoidable_deaths\\AD Preventable and Treatable")


########################
#Comment from here

load("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\GCO_pop2020.RData")

Cancer_codes <- read.csv("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Research visits\\Oliver_Langselius\\Data\\dict_cancer.csv") %>% as.data.frame()


HDI_Region_Mapping<-read.csv("I:\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Data\\HDI2018_GLOBOCAN2020.csv")

# population data

load("I:\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Data\\GCO_pop2020.RData")

country_codes <-
  read.csv("I:\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Data\\GCO_country_info.csv", stringsAsFactors = FALSE) %>%
  filter(country_code<900) %>%
  dplyr::select(country_code, country_label)



globocan <- read.csv("I:\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\PreGlobocan2022\\Globocan2022\\globocan.csv")%>%
  as.data.frame()%>%
  filter(type==0, 
         sex!=0)%>%
  # group_by(country_code, cancer_code, sex, age)%>%
  # mutate(cases=sum(cases),
  #        py=sum(py))%>%
  # ungroup()%>%
  # distinct()
  dplyr::select(-cancer_label)


globocan_mort <- read.csv("I:\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\PreGlobocan2022\\Globocan2022\\globocan.csv")%>%
  as.data.frame()%>%
  filter(type==1, 
         sex!=0)%>%
  # group_by(country_code, cancer_code, sex, age)%>%
  # mutate(cases=sum(cases),
  #        py=sum(py))%>%
  # ungroup()%>%
  # distinct()
  dplyr::select(-cancer_label, -py)%>%
  dplyr::rename("deaths"="cases")%>%
  select(-type, -country_label)


PAFs <- read.csv("I:\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Data\\combinedPAFs_cases_02.06.23.csv")%>%
  as.data.frame()%>%
  distinct()%>%
  dplyr::select(-country_label)%>%
  group_by(country_code, sex,
           cancer_code, age)%>%
  filter(sex!=0)%>%
  # dplyr::mutate(cases=as.numeric(cases),
  #              cases.prev=as.numeric(cases.prev))%>%
  as.data.frame()%>%
  dplyr::select(-cases,-py, -cases.prev,-cases.notprev)%>%
  left_join(globocan, by=c("cancer_code", "country_code", "age", "sex"))%>%
  mutate(cases.prev=cases*af.comb,
         cases.notprev=cases*(1-af.comb))%>%
  filter(!(sex==1& cancer_code%in% c(20, 21, 22, 23, 24, 25)))%>%
  filter(!(sex==2& cancer_code%in% c(26:28)))%>%
  filter(cancer_code!=40)



globocan_UIs2 <- read.csv("I:\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\PreGlobocan2022\\Globocan2022\\source\\Globocan_UIs.csv")%>%
  as.data.frame()

globocan_UIs3<-globocan_UIs2%>%
  filter(type==0,
         sex==0)%>%
  filter(!country_code%in%c(928, # Melanesia
                           954, # Micronesia
                           957, # Polynesia
                           964 #Micronesia/Polynesia
  ))%>%
  dplyr::select(sex, country_code, cancer_code, total, total.low, total.high)%>%
  mutate(var.low = case_when(total!=0 ~ abs((total.low-total)/total), 
                             total==0 ~0),
         var.high= case_when(total!=0 ~  abs((total.high-total)/total),
                             total==0 ~0)
         )%>%
  dplyr::select( country_code, cancer_code, var.low, var.high)
 

globocan_UIs_nesia<-globocan_UIs2%>%
  filter(type==0,
         sex==0)%>%
  filter(country_code%in%c(928, # Melanesia
                            954, # Micronesia
                            957, # Polynesia
                            964 #Micronesia/Polynesia
  ))%>%
  dplyr::select(sex, country_code, cancer_code, total, total.low, total.high)%>%
  mutate(country_code=999)%>%
  group_by(sex, country_code, cancer_code)%>%
  mutate(total=sum(total),
         total.low=sum(total.low),
         total.high=sum(total.high))%>%
  distinct()%>%
  mutate(var.low = case_when(total!=0 ~ abs((total.low-total)/total), 
                             total==0 ~0),
         var.high= case_when(total!=0 ~  abs((total.high-total)/total),
                             total==0 ~0)
  )%>%
  dplyr::select( country_code, cancer_code, var.low, var.high)

globocan_UIs<-globocan_UIs3%>%
  full_join(globocan_UIs_nesia)






# PAFs49<-PAFs9%>%
#     filter(age>=4 & age<9)%>%
#     mutate(age=4)%>%
#     
#     group_by(country_code, sex,
#              cancer_code, age)
#   as.data.frame()%>%
#   dplyr::mutate(af.comb= case_when(cases!=0 ~ sum(cases.prev, na.rm=T)/sum(cases, na.rm=T),
#                             cases==0 ~ af.comb))%>%
#   ungroup()%>%
#   as.data.frame()%>%
#   distinct()


Cancer_codes <- read.csv("I:\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Data\\dict_cancer.csv") %>% as.data.frame()
Cancer_codes_Survcan <- read.csv("I:\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Data\\cancer_codes_Survcan.csv") %>% as.data.frame()

#




# read survival - when new file check coding used  for cancer codes

Survival_Modelled3 <-  read_dta("I:\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Data\\survival_SURVCAN_anchor_ALL_all34_byHDI.dta")%>%
  as.data.frame()%>%
  mutate(country_name=gsub('`"','', country_name),
         country_name=gsub("'",'', country_name),
         country_name=gsub('"','', country_name)
  )%>%
  clean_names()%>%
  filter(cancer_label!="Nasophar")%>%
  dplyr::rename("country_label"="country_name")%>%
  distinct()%>%
  filter(time==5)


Survival_Modelled2<-Survival_Modelled3%>%
  filter(!cancer_code%in%c(41,42))%>%
  dplyr::mutate(cancer_code = case_when(
    cancer_code==2~ 2,
    cancer_code==4~ 4,
    cancer_code==5 ~ 5,
    cancer_code==6 ~ 6,
    cancer_code==7 ~ 7,
    cancer_code==8 ~ 8,
    cancer_code==9 ~ 9,
    cancer_code==10 ~ 10,
    cancer_code==11 ~ 11,
    cancer_code==45 ~ 12,
    cancer_code==13 ~ 13,
    cancer_code==14 ~ 14,
    cancer_code==15 ~ 15,
    cancer_code==16 ~ 16,
    cancer_code==18 ~ 18,
    cancer_code==19 ~ 19,
    cancer_code==20 ~ 20,
    cancer_code==21 ~ 21,
    cancer_code==22 ~ 22,
    cancer_code==23 ~ 23,
    cancer_code==24 ~ 24,
    cancer_code==25 ~ 25,
    cancer_code==26 ~ 26,
    cancer_code==27 ~ 27,
    cancer_code==28 ~ 28,
    cancer_code==29 ~ 29,
    cancer_code==30 ~ 30,
    cancer_code==31 ~ 31,
    cancer_code==32 ~ 32,
    cancer_code==33 ~ 33,
    cancer_code==34 ~ 34,
    cancer_code==35 ~ 35,
    cancer_code==36 ~ 36
  )) %>%
  filter(cancer_label!="Colorectal")#%>%
#filter(age<=16)

#Grouped survival estimates for the

Survival_Modelled2_lip<-Survival_Modelled3%>%
  filter(cancer_code==41)%>%
  dplyr::mutate(cancer_code=1,
                cancer_label="Lip, oral cavity")

Survival_Modelled2_salivary<-Survival_Modelled3%>%
  filter(cancer_code==41)%>%
  dplyr::mutate(cancer_code=2,
                cancer_label="Salivary glands")

#Fix this

Survival_Modelled2_oropharynx<-Survival_Modelled3%>%
  filter(cancer_code==42)%>%
  dplyr::mutate(cancer_code=3,
                cancer_label="Oropharynx")

Survival_Modelled2_naso<-Survival_Modelled3%>%
  filter(cancer_code==42)%>%
  dplyr::mutate(cancer_code=4,
                cancer_label="Nasopharynx")

Survival_Modelled2_hypo<-Survival_Modelled3%>%
  filter(cancer_code==42)%>%
  dplyr::mutate(cancer_code=5,
                cancer_label="Hypoharynx")

Survival_Modelled<-Survival_Modelled2%>%
  full_join(Survival_Modelled2_lip)%>%
  full_join(Survival_Modelled2_salivary)%>%
  full_join(Survival_Modelled2_oropharynx)%>%
  full_join(Survival_Modelled2_naso)%>%
  full_join(Survival_Modelled2_hypo)
  

# Check so cancer codes match up

# globocan <- read.csv("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Research visits\\Oliver_Langselius\\Globocan2020\\Globocan.csv",
#                       stringsAsFactors = FALSE)%>%
#   as.data.frame()%>%
#   filter(country_code<900)


#b<-Survival_Modelled%>%dplyr::select(cancer_code, cancer_label)%>%distinct()%>%arrange(cancer_code)

globocan_cancer_names<-read.csv("I:\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\PreGlobocan2022\\Globocan2022\\globocan.csv")%>%
  as.data.frame()%>%
  filter(type==0, 
         sex!=0)%>%
  dplyr::select(cancer_code, cancer_label)%>%distinct()

# missing_surv_estimates<-PAFs%>%dplyr::select(cancer_code, cancer_label)%>%distinct()%>%filter(!cancer_code%in%b$cancer_code)
# 
# ca<-PAFs%>%dplyr::select(cancer_code, cancer_label)%>%distinct()%>%arrange(cancer_code)

# HDI_File<-Survival_Modelled%>%
#   dplyr::select(country_code, country_label, hdi_group)%>%
#   distinct()

#write.csv(HDI_File, "~/Documents/R_Projects/Data/HDI_file_2019.csv")

#HDI <-read.csv("~/Documents/R_Projects/Data/HDI_2019.csv") %>% as.data.frame()
#Thailand_Survcan <-read.csv("I:\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Data\\ASTHABAN_cc.csv") %>% as.data.frame()

Thailand_popmort <-read.csv("I:\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Data\\popmort_Thailand.csv") %>% as.data.frame() %>%
  left_join(country_codes, by = c("region" = "country_label"))

Thailand_pop <-read.csv("I:\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Data\\ASTHABAN_pop.csv") %>% as.data.frame()

popmort2<-read_dta("I:\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Data\\who_ghe_popmort.dta")%>%as.data.frame()%>%
  left_join(country_codes)

p <- read_dta("I:\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Data\\who_ghe_group.dta")%>%
  as.data.frame()

# MIR_Age_Cats<-read.csv("I:\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Data\\MIR_age_cat.csv")%>%
#   as.data.frame()%>%
#   dplyr::select(-mortality,-incidence)%>%
#   mutate(MIR=replace(MIR,MIR==Inf, NA))




#same file but Globocan age groups for modeled data
# MIR_Globocan<-read.csv("I:\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Data\\MIR.csv")%>%
#   as.data.frame()%>%
#   dplyr::select(-mortality,
#          -incidence)%>%
#   mutate(MIR=replace(MIR,MIR==Inf, NA))%>%
#   mutate(age = case_when(
#     age==4~ 4,
#     age>4 & age<=9~ 9,
#     age==10~ 10,
#     age==11~ 11,
#     age==12~ 12,
#     age==13~ 13,
#     age==14~ 14,
#     age==15~ 15,
#     age>=16 ~ 16,
#   ))%>%
#   mutate(age_cat = case_when(
#     age>=4 & age<14 ~ "15-64",
#     age>=14 ~ "65-99",
#     age<4 ~"0-15"))%>%
#   dplyr::select(-sex, -X)%>%
#   group_by(country_code, cancer_code, age)%>%
#   mutate(MIR=sum(MIR*py)/sum(py))%>%
#   mutate(py=sum(py))%>%distinct()%>%
#   ungroup()


Thailand_expected_Survival<-read.csv("I:\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Data\\Thailand_expected_Survival.csv")%>%as.data.frame()




Reference_Survival<-read.csv("I:\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Data\\Reference_Survival.csv")%>%
  as.data.frame()%>%
  dplyr::select( age,
                 cancer_code,
                 rel_surv)%>%
  dplyr::rename("surv_ref"="rel_surv")%>%
  distinct()%>%
  mutate(surv_ref=case_when(surv_ref>1~ 1,
                            surv_ref<=1~ surv_ref))# %>%
# filter(age<=16)

sapply(Reference_Survival, class)

Reference_Survival_Survcan<-Reference_Survival%>%
  left_join(PAFs,by=c("cancer_code","age"))%>%
  dplyr::select(age, cancer_code, surv_ref, cases)%>%
  mutate(cancer_code = replace(cancer_code, cancer_code == 9, 38))%>%
  mutate(cancer_code = replace(cancer_code, cancer_code == 8, 38))%>%
  mutate(age = case_when(
    age==0~ 0,
    age==1~ 1,
    age==2~ 2,
    age==3~ 3,
    age==4~ 4,
    age>4 & age<9 ~ 4,
    age==9 ~ 9,
    age==10~ 10,
    age==11~ 11,
    age==12~ 12,
    age==13~ 13,
    age==14~ 14,
    age==15~ 15,
    #age>=16 ~ 16,
    age==16 ~ 16,
    age==17 ~ 17,
    age==18 ~ 18,
    
  ))%>%
  mutate(
    age_cat = case_when(
      age>=4 & age<14 ~ "15-64",
      age>=14 ~ "65-99",
      age<4 ~"0-15"
    ))%>%
  filter(age_cat!="0-15")%>%
  dplyr::select(-age)%>%
  ungroup()%>%
  group_by(cancer_code,age_cat)%>%
  mutate(surv_ref=  sum(surv_ref*cases, na.rm=T)/sum(cases, na.rm=T))%>%
  ungroup()%>%
  dplyr::select(-cases)%>%
  distinct() %>%
  filter(!is.na(cancer_code))



Reference_Survival_Survcan<-read.csv("I:\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Data\\Reference_Survival_Survcan.csv")%>%
  as.data.frame()%>%
  dplyr::select( age_cat,
                 cancer_code,
                 rel_surv)%>%
  dplyr::rename("surv_ref"="rel_surv")%>%
  distinct()%>%
  mutate(surv_ref=case_when(surv_ref>1~ 1,
                            surv_ref<=1~ surv_ref))%>%
  filter(!is.na(cancer_code))

load("I:\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Data\\ExpectedSurvival.RData")


ES2<-p%>%
  as.data.frame()%>%
  
  dplyr::rename(ES="es")%>%
  dplyr::select(-country_label, -year)

#
#
#

ES_age_Cats<-ES2%>%
  mutate(age = case_when(
    age==0~ 0,
    age==1~ 1,
    age==2~ 2,
    age==3~ 3,
    age==4~ 4,
    age>4 & age<=9~ 9,
    age==10~ 10,
    age==11~ 11,
    age==12~ 12,
    age==13~ 13,
    age==14~ 14,
    age==15~ 15,
    #age>=16 ~ 16,
    age==16 ~ 16,
    age==17 ~ 17,
    age==18 ~ 18,))%>%
  mutate(
    age_cat = case_when(
      age>=4 & age<14 ~ "15-64",
      age>=14 ~ "65-99",
      age<4 ~"0-15"
    ))
# #
#

#
#
ten_cancer_sites <-
  Cancer_codes %>%
  filter(cancer_code %in% c(6, 7, 11, 13, 15, 20, 23, 27, 30, 38))%>%
  mutate(cancer_label=replace(cancer_label,cancer_label=="Unspecified sites","Colorectal"))
# 
# 
# 
# #Checking cancer codes in various files
# a<-PAFs%>%
#   dplyr::summarize(country_code, country_label)%>%
#   distinct()
# b<-simulated_overall%>%
#   ungroup()%>%
#   dplyr::summarize(country_code, country_label)%>%
#   distinct()
# 
# 
# 
# 
# #Thai example - first with SURVCAN data and then with real data, scaling with Thai incidence rather than globocan
# 
# #expanding life table
# 
# Thailand_popmort_2015 <-
#   Thailand_popmort %>% filter(X_year == 2015) %>%
#   mutate(X_year = replace(X_year, X_year == 2015, 2016))
# 
# Thailand_popmort2 <-
#   Thailand_popmort %>% full_join(Thailand_popmort_2015)
# 
# 
# 
# #Prepping cancer real world data
# Thai_Survcan2 <- Thailand_Survcan %>%
#   filter(surv_dd>0)%>%
#   filter(include == "Included") %>%
#   filter(age >= 15) %>%
#   filter(age <= 99) %>%
#   filter(!is.na(age)) %>%
#   filter(!is.na(surv_yy)) %>%
#   mutate(age_cat = cut(
#     age,
#     breaks = c(-Inf, 15, 64 , 99),
#     labels = c("<15", "15-64", "65-99")
#   )) %>% #create age categories (to be adjusted)
#   ungroup()%>%
#   droplevels()%>%
#   mutate(last_FU_age = round(age + surv_dd/365.15)) %>% #creating variable for age of death
#   mutate(last_FU_year = round(year + surv_dd/365.15))%>%  #creating variable for year of death
#   mutate(sex = replace(sex, sex == "Male", 1)) %>%
#   mutate(sex = replace(sex, sex == "Female", 2)) %>%
#   mutate(sex = as.integer(sex)) %>%
#   left_join(Thailand_popmort2, by = c(
#     "last_FU_age" = "X_age",
#     "last_FU_year" = "X_year",
#     "country" = "region",
#     "sex" = "sex")) %>%
#   droplevels()%>%
#   filter(last_FU_year > 2009 & last_FU_year <= 2014) %>%
#   filter(!is.na(mx)) %>% 
#   droplevels() %>%
#   left_join(Cancer_codes_Survcan, by = c("cancer" = "cancer"))%>%
#   mutate(cancer = replace(cancer, cancer == "Colon (C18)", "Colorectal")) %>%
#   mutate(cancer = replace(cancer, cancer == "Rectum (C19-20)", "Colorectal")) %>%
#   filter(cancer_code %in% ten_cancer_sites$cancer_code)
# 
# Thai_Surv3 <- Thai_Survcan2%>%
#   mutate(surv_yydd=surv_dd/365.2425)%>%
#   mutate(event1=case_when(dead==1 &      surv_yydd<=5 ~ 1,
#                           dead==1 & surv_yydd>5 ~ 0,
#                           dead==0 ~ 0
#   ))
# 
# #modifying so last five years of follow up
# 
# 
# 
# Thai_Surv11<- Thai_Surv3%>%
#   dplyr::select(region_lab, country, doi, last_FU_age,age,surv_yytot,year,cancer_code)%>%
#   group_by(region_lab,country,cancer_code)%>%
#   dplyr::summarize( end_FU=max(year))%>%
#   as.data.frame()
# 
# 
# Thai_Surv<-Thai_Surv3%>%left_join(Thai_Surv11)#%>%ungroup()%>%
# # filter(year>=end_FU-5)
# # mutate(surv_yy=case_when(surv_yy<=5 ~ 5
# # )
# # )
# 
# #Thai_Surv$sex <- as.integer(Thai_Surv$sex)
# 
# #Thai_Surv_test<-Thai_Surv%>%filter(cancer_code==30)
# 
# 
# #age categories
# #Thai_Surv_overall <- Thai_Surv %>% mutate(age_cat = "Overall")
# Thai_Surv_Lower <- Thai_Surv %>% filter(age_cat == "15-64")
# Thai_Surv_Upper <- Thai_Surv %>% filter(age_cat == "65-99")
# 
# Thai_Surv_age_cats <- Thai_Surv #Thai_Surv_overall %>% full_join(Thai_Surv)
# 
# 
# is.na(Thai_Surv$mx) #60 people have ages above 100 but were between 15-99 years at age of diagnosis. Should they be excluded?
# 
# 
# 
# #########################################
# #
# #
# ## Fitting a cubic spline by country and adding comparisons.
# #
# #
# #########################################
# 
# #Creating time points for predictions
# time <- seq(0, 5, le = 5001)
# 
# #Names for countries, regions and age groups for next step
# #Removes empty age variables for model to be run properly
# Thai_Surv <- Thai_Surv %>% droplevels()
# 
# cancer_types <- as_tibble(names(table(Thai_Surv$cancer))) #Needs to be tibble for predictions
# 
# names(cancer_types)[names(cancer_types) == "value"] <- "cancer"
# cancer_types <-  as.data.frame(cancer_types) #For regression needs to be in this form
# 
# cancer_codes <- as_tibble(names(table(ten_cancer_sites$cancer_code))) #Needs to be tibble for predictions
# names(cancer_codes)[names(cancer_codes) == "value"] <- "cancer_code"
# cancer_codes <- as.data.frame(cancer_codes) #For regression needs to be in this form
# 
# sex <-as_tibble(names(table(Thai_Surv$sex))) #Needs to be tibble for predictions
# names(sex)[names(sex) == "value"] <- "sex"
# sex <- as.data.frame(sex) #For regression needs to be in this form
# 
# 
# 
# 
# #Cubic base model BY COUNTRY
# 
# 
# Cubic_age_1 <- list()
# Cubic_age_2 <- list()
# #Cubic_overall <- list()
# 
# #Cubic base model BY Cancer type
# 
# for (i in 1:nrow(cancer_codes)) {
#   b1 <- Thai_Surv_Lower %>% filter(cancer_code == cancer_codes[i,])
#   b2 <- Thai_Surv_Upper %>% filter(cancer_code == cancer_codes[i,])
#   # b3 <- Thai_Surv_overall %>% filter(cancer_code == cancer_codes[i,])
#   
#   
#   # k1<-c(1,2.5,4)
#   # k2<-c(1,2.5,4)
#   # k3<-c(1,2.5,4)
#   
#   
#   k1 <- median(b1$surv_yydd)
#   k2 <- median(b2$surv_yydd)
#   #  k3 <- median(b3$surv_yydd)
#   
#   try(Cubic_age_1[[i]] <-
#         mexhaz(
#           formula = Surv(surv_yydd, event1) ~ 1,
#           data = b1,
#           base = "exp.ns",
#           # degree = 3,
#           knots = k1,
#            numHess=TRUE,
#            fnoptim="optim",n.gleg=100
#         ))
#   
#   try(Cubic_age_2[[i]] <-
#         mexhaz(
#           formula = Surv(surv_yydd, event1) ~ 1,
#           data = b2,
#           base = "exp.ns",
#           # degree = 3,
#           knots = k2,
#            numHess=TRUE,n.gleg=100,
#           fnoptim="optim"
#         ))
#   
#   # try(Cubic_overall[[i]] <-
#   #       mexhaz(
#   #         formula = Surv(surv_yydd, event1) ~ 1,
#   #         data = b3,
#   #         base = "exp.ns",
#   #        # degree = 3,
#   #         knots = k3
#   #        # numHess=TRUE,
#   #        # fnoptim="optim"
#   #       ))
# }
# 
# 
# 
# 
# 
# 
# #Updating the models with mortality rates
# 
# #Country updated models list initializing
# Cubic_Cancer_age_1 <- list()
# Cubic_Cancer_age_2 <- list()
# #Cubic_Cancer_overall <- list()
# 
# #Cubic excess hazard by country
# for (i in 1:nrow(cancer_codes)){
#   b1 <- Thai_Surv_Lower %>% filter(cancer_code == cancer_codes[i,])
#   b2 <- Thai_Surv_Upper %>% filter(cancer_code == cancer_codes[i,])
#   # b3 <- Thai_Surv_overall %>% filter(cancer_code == cancer_codes[i,])
#   
#   try(Cubic_Cancer_age_1[[i]] <-
#         update(Cubic_age_1[[i]], expected = "mx",      numHess=TRUE,
#                fnoptim="optim",n.gleg=100))
#   try(Cubic_Cancer_age_2[[i]] <-
#         update(Cubic_age_2[[i]], expected = "mx",      numHess=TRUE,
#                fnoptim="optim",n.gleg=100))
#   # try(Cubic_Cancer_overall[[i]] <-
#   #       update(Cubic_overall[[i]], expected = "mx",    numHess=TRUE,
#   #              fnoptim="optim"))
# }
# 
# 
# ####################
# 
# # Running Predictions by country
# 
# ####################
# 
# 
# Predictions_Cubic_Net_age_1 <- list()
# Predictions_Cubic_Net_age_2 <- list()
# #Predictions_Cubic_Net_age_3 <- list()
# 
# Predictions_Cubic_All_Cause_age_1 <- list()
# Predictions_Cubic_All_Cause_age_2 <- list()
# #Predictions_Cubic_All_Cause_age_3 <- list()
# 
# cancer_types_tibble <-
#   cancer_types %>% as_tibble() #predict only works with tibble data structure...
# 
# cancer_codes_tibble <-
#   cancer_codes %>% 
#   mutate(cancer_code=as.integer(cancer_code))%>%
#   as_tibble() #predict only works with tibble data structure...
# #Cubic predictions by country
# for (i in 1:nrow(cancer_codes_tibble)) {
#   try(AC1 <- predict(Cubic_age_1[[i]], time.pts = time, data.val = cancer_codes_tibble[i,]))
#   try(AC2 <- predict(Cubic_age_2[[i]], time.pts = time, data.val = cancer_codes_tibble[i,]))
#   # try(AC3 <- predict(Cubic_overall[[i]], time.pts = time, data.val = cancer_codes_tibble[i,]))
#   
# 
#   try(HP1 <- predict(Cubic_Cancer_age_1[[i]],
#                      time.pts = time,
#                      data.val = cancer_codes_tibble[i,]))
#   try(HP2 <- predict(Cubic_Cancer_age_2[[i]],
#                      time.pts = time,
#                      data.val = cancer_codes_tibble[i,]))
#   # try(HP3 <- predict(Cubic_Cancer_overall[[i]],
#   #               time.pts = time,
#   #               data.val = cancer_codes_tibble[i,]))
#   
#   
#   Predictions_Cubic_All_Cause_age_1[[i]] <- AC1
#   Predictions_Cubic_All_Cause_age_2[[i]] <- AC2
#   #  Predictions_Cubic_All_Cause_age_3[[i]] <- AC3
#   
#   
#   Predictions_Cubic_Net_age_1[[i]] <- HP1
#   Predictions_Cubic_Net_age_2[[i]] <- HP2
#   #  Predictions_Cubic_Net_age_3[[i]] <- HP3
# }
# 
# 
# #Extracting prediction data five year avoidable deaths prediction and survival
# 
# Net_Survival_Five_Year_age_1 <- matrix(ncol = 5, nrow = nrow(cancer_codes)) 
# Net_Survival_Five_Year_age_2 <- matrix(ncol = 5, nrow = nrow(cancer_codes)) 
# 
# 
# 
# All_Cause_Survival_age_1 <- matrix(ncol = 5, nrow = nrow(cancer_codes))    
# All_Cause_Survival_age_2 <- matrix(ncol = 5, nrow = nrow(cancer_codes) )    
# 
# for (i in 1:nrow(cancer_codes_tibble)) {
#   s <-  Predictions_Cubic_Net_age_1[[i]]$results
#   s <-  s %>% filter(time.pts == 5)
#   s2 <-  Predictions_Cubic_Net_age_2[[i]]$results
#   s2 <-  s2 %>% filter(time.pts == 5)
# 
#   
#   sp <- Predictions_Cubic_All_Cause_age_1[[i]]$results
#   sp <-  sp %>% filter(time.pts == 5)
#   sp2 <- Predictions_Cubic_All_Cause_age_2[[i]]$results
#   sp2 <-  sp2 %>% filter(time.pts == 5)
# 
#   
#   Net_Survival_Five_Year_age_1[i,] <-
#     c(ten_cancer_sites[i,1],
#       ten_cancer_sites[i,2], 
#       s$surv, s$surv.inf, s$surv.sup)
#   Net_Survival_Five_Year_age_2[i,] <-
#     c(ten_cancer_sites[i,1],
#       ten_cancer_sites[i,2],
#       s2$surv,
#       s2$surv.inf,
#       s2$surv.sup)
# 
#   
#   All_Cause_Survival_age_1[i,] <-
#     c(ten_cancer_sites[i,1],
#       ten_cancer_sites[i,2],
#       sp$surv,
#       sp$surv.inf,
#       sp$surv.sup)
#   All_Cause_Survival_age_2[i,] <-
#     c(ten_cancer_sites[i,1],
#       ten_cancer_sites[i,2],
#       sp2$surv,
#       sp2$surv.inf,
#       sp2$surv.sup)
# 
# }
# 
# #
# 
# Age_names_all <- as.data.frame(c("15-64", "65-99")) 
# 
# Net_Survival_Five_Year_age_1 <-
#   as.data.frame(Net_Survival_Five_Year_age_1)
# Net_Survival_Five_Year_age_2 <-
#   as.data.frame(Net_Survival_Five_Year_age_2)
# 
# 
# All_Cause_Survival_age_1 <- as.data.frame(All_Cause_Survival_age_1)
# All_Cause_Survival_age_2 <- as.data.frame(All_Cause_Survival_age_2)
# 
# colnames(Net_Survival_Five_Year_age_1) <-
#   c("cancer_code",
#     "cancer",
#     "Five_Year_Net_Surv",
#     "NS_Lower_CI",
#     "NS_Upper_CI")
# colnames(Net_Survival_Five_Year_age_2) <-
#   c("cancer_code",
#     "cancer",
#     "Five_Year_Net_Surv",
#     "NS_Lower_CI",
#     "NS_Upper_CI")
# 
# colnames(All_Cause_Survival_age_1) <-
#   c("cancer_code",
#     "cancer",
#     "Five_Year_all_cause_Surv",
#     "OS_Lower_CI",
#     "OS_Upper_CI")
# 
# colnames(All_Cause_Survival_age_2) <-
#   c("cancer_code",
#     "cancer",
#     "Five_Year_all_cause_Surv",
#     "OS_Lower_CI",
#     "OS_Upper_CI")
# 
# 
# Net_Survival_Five_Year_age_1 <-
#   Net_Survival_Five_Year_age_1 %>% mutate(age_cat = Age_names_all[1,])
# Net_Survival_Five_Year_age_2 <-
#   Net_Survival_Five_Year_age_2 %>% mutate(age_cat = Age_names_all[2,])
# 
# All_Cause_Survival_age_1 <-
#   All_Cause_Survival_age_1 %>% mutate(age_cat = Age_names_all[1,])
# All_Cause_Survival_age_2 <-
#   All_Cause_Survival_age_2 %>% mutate(age_cat = Age_names_all[2,])
# 
# 
# Net_Survival_Five_Year <-
#   Net_Survival_Five_Year_age_1 %>% 
#   full_join(Net_Survival_Five_Year_age_2) 
# 
# All_Cause_Survival <-
#   All_Cause_Survival_age_1 %>% 
#   full_join(All_Cause_Survival_age_2) 
# 
# 
# NS_OS2 <-Net_Survival_Five_Year %>% left_join(
#   All_Cause_Survival,
#   by = c(
#     "cancer" = "cancer",
#     "cancer_code" = "cancer_code",
#     "age_cat" = "age_cat"
#   )
# )
# 
# 
# NS_OS2$cancer_code <- as.numeric(as.character(NS_OS2$cancer_code))
# 
# Thai_Surv$cancer_code <-
#   as.numeric(as.character(Thai_Surv$cancer_code))
# 
# 
# NS_OS <-
#   NS_OS2 %>% left_join(
#     Thai_Surv_age_cats,
#     by = c(
#       "cancer" = "cancer",
#       "cancer_code" = "cancer_code",
#       "age_cat" = "age_cat"
#     )
#   ) %>%
#   dplyr::select(cancer,cancer_code,
#          Five_Year_all_cause_Surv,OS_Lower_CI,OS_Upper_CI,
#          Five_Year_Net_Surv,NS_Lower_CI,NS_Upper_CI,
#          age_cat
#   ) %>%
#   distinct()
# 
# NS_OS$Five_Year_Net_Surv <- as.numeric(NS_OS$Five_Year_Net_Surv)
# NS_OS$NS_Lower_CI <- as.numeric(NS_OS$NS_Lower_CI)
# NS_OS$NS_Upper_CI <- as.numeric(NS_OS$NS_Upper_CI)
# NS_OS$Five_Year_all_cause_Surv <-
#   as.numeric(NS_OS$Five_Year_all_cause_Surv)
# NS_OS$OS_Lower_CI <- as.numeric(NS_OS$OS_Lower_CI)
# NS_OS$OS_Upper_CI <- as.numeric(NS_OS$OS_Upper_CI)
# NS_OS$cancer <- as.factor(as.character(NS_OS$cancer))
# NS_OS$age_cat <- as.factor(NS_OS$age_cat)
# 
# 
# #########################
# #
# # AD Calculations
# #
# #########################
# 
# #NS_OS <- read.csv("~/Documents/R_Projects/Data/Thai_NS_OS.csv")
# 
# #Prepping PAFs data processing and combining by age group
# 
# PAFs_age_Cat <- PAFs %>%
#   distinct()%>%
#   filter(country_label == "Thailand") %>%
#   mutate(age_cat = case_when(age >= 4 & age < 14 ~ "15-64",
#                              age >= 14 ~ "65-99",
#                              age<4 ~ "0-15")) %>%
#   filter(age_cat != "0-15") %>%
#   left_join(ES2, by=c("country_code","age","sex"))%>% 
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
#     py,
#     total_overall = sum(cases), 
#     ES
#   ) %>% 
#   as.data.frame()%>%
#   droplevels()
# 
# 
# # PAFS_Overall <- PAFs_age_Cat %>% 
# #   mutate(age_cat = "Overall") %>%
# #   group_by(country_label, cancer_label, age_cat) %>%
# #   dplyr::summarize(
# #     country_code,
# #     country_label,
# #     cancer_code,
# #     cancer_label,
# #     age,
# #     age_cat,
# #     cases,
# #     af.comb,
# #     cases.prev,
# #     cases.notprev,
# #     total_overall = sum(cases), ES
# #   ) %>% 
# #   as.data.frame()%>%
# #   droplevels()
# 
# 
# PAFs2 <- PAFs_age_Cat %>%
#   #  full_join(PAFS_Overall) %>%
#   as.data.frame() %>%
#   droplevels()%>%
#   mutate(cancer_label = replace(cancer_label, cancer_label == "Colon", "Colorectal"))%>%
#   mutate(cancer_label = replace(cancer_label, cancer_label == "Rectum", "Colorectal"))%>%
#   mutate(cancer_code = replace(cancer_code, cancer_code == 9, 38))%>%
#   mutate(cancer_code = replace(cancer_code, cancer_code == 8, 38))%>%
#   group_by(country_label, cancer_label, age_cat) %>%
#   mutate(total_age_prev = sum(cases.prev)) %>%
#   mutate(af.comb = sum(cases.prev) / sum(cases)) %>%
#   mutate(ES = sum(ES*cases) / sum(cases)) %>%
#   dplyr::summarize(country_code,
#                    country_label,
#                    cancer_code,
#                    cancer_label,
#                    age_cat,
#                    af.comb,
#                    total_overall=sum(cases),
#                    ES
#   ) %>%
#   distinct() %>%
#   arrange(cancer_label, age_cat)
# 
# 
# 
# 
# NS_OS_PAF <- NS_OS %>% 
#   left_join(PAFs2, by = c("cancer_code" = "cancer_code", "age_cat" ="age_cat")) %>% 
#   left_join(Reference_Survival_Survcan,by=c("age_cat","cancer_code"))%>% #Add aggregated values here for the Thailand data. Need to combine age groups
#   droplevels()%>%
#   mutate(cancer=as.character(cancer))%>%
#   distinct()%>%
#   mutate(cancer = replace(cancer, cancer == "Liver and intrahepatic bile ducts", "Liver"))%>%
#   mutate(cancer = replace(cancer, cancer == "Trachea, bronchus and lung", "Lung"))
#   
# 
# #Three AD calcs
# 
# 
# #Applying the equation from Rutherford 2015 for AD. Needs to be updated to have scaled relative survival
# 
# 
# Avoidable_Deaths<-NS_OS_PAF%>%
#   group_by(country_code, cancer_code)%>%
#   mutate(AD_prev=af.comb * total_overall * (1 - Five_Year_Net_Surv *  ES ))%>%
#   mutate(AD_prev_Upper=af.comb * total_overall * (1 - NS_Lower_CI *  ES ))%>%
#   mutate(AD_prev_Lower=af.comb * total_overall * (1 - NS_Upper_CI *  ES ))%>%
#   mutate(AD_treat=total_overall *(surv_ref-Five_Year_Net_Surv) * ES)%>%
#   mutate(AD_treat_Upper=total_overall *(surv_ref-NS_Lower_CI) * ES)%>%
#   mutate(AD_treat_Lower=total_overall *(surv_ref- NS_Upper_CI)* ES)%>%
#   mutate(AD_treat_not_prev=(1-af.comb)* total_overall *(surv_ref-Five_Year_Net_Surv) * ES)%>%
#   mutate(AD_unavoid = (1-af.comb)*total_overall*(1-surv_ref*ES))%>%
#   mutate(Expect_deaths=(1-(Five_Year_Net_Surv*ES))*total_overall)%>%
#   mutate(Expect_deaths_Lower=(1-(NS_Upper_CI*ES))*total_overall)%>%
#   mutate(Expect_deaths_Upper=(1-(NS_Lower_CI*ES))*total_overall)%>%
#   dplyr::select("country_code","country_label",
#          "age_cat",
#          "cancer_code","cancer_label",
#          "AD_treat",
#          "AD_treat_Lower",
#          "AD_treat_Upper",
#          "AD_prev",
#          "AD_prev_Lower",
#          "AD_prev_Upper",
#          "AD_unavoid",
#          "Expect_deaths",
#         "Expect_deaths_Lower",
#         "Expect_deaths_Upper",
#          "total_overall")%>%
#   rename("cancer"="cancer_label")
# 
# #Calculating the overall AD
# 
# Avoidable_Deaths_overall<-Avoidable_Deaths%>%
#   group_by(cancer_code)%>%
#   mutate(age_cat="Overall")%>%
#   mutate(AD_treat=sum(AD_treat))%>%
#   mutate(AD_treat_Lower=sum(AD_treat_Lower))%>%
#   mutate(AD_treat_Upper=sum(AD_treat_Upper))%>%
#   mutate(AD_prev=sum(AD_prev))%>%
#   mutate(AD_prev_Lower=sum(AD_prev_Lower))%>%
#   mutate(AD_prev_Upper=sum(AD_prev_Upper))%>%
#   mutate(AD_unavoid=sum(AD_unavoid))%>%
#   mutate(Expect_deaths=sum(Expect_deaths))%>%
#   mutate(Expect_deaths_Lower=sum(Expect_deaths_Lower))%>%
#   mutate(Expect_deaths_Upper=sum(Expect_deaths_Upper))%>%
#   mutate(total_overall=sum(total_overall))%>%
#   mutate(AD_sum = AD_treat+AD_prev+AD_unavoid)%>%
#   ungroup()%>%
#   droplevels()%>%
#   distinct()
# 
# 
# ###############
# #
# #Exporting Results
# #
# ###############
# 
# 
# write.csv(Avoidable_Deaths, "~/Documents/R_Projects/Data/Thai_AD.csv")
# write.csv(NS_OS, "~/Documents/R_Projects/Data/Thai_NS_OS.csv")
# 
# 
# # 
# # library(ranger)
# # library(mlr3)
# # library(caTools)
# # library(survival)
# # 
# # # Random Forest model as a different approach
# # # Using the ranger package 
# # # 
# # # ranger model.Too complex for the whole dataset on this laptop
# # # would be interesting to try this again with a more powerful computer
# # #https://rviews.rstudio.com/2017/09/25/survival-analysis-with-r/
# # 
# # #creating training and test datasets
# #Thai_Surv_test<-Thai_Surv%>%
# #full_join(Thai_Surv_overall)%>%
# #  filter(cancer_code==20)
# # 
# # ten_cancer_sites
# # 
# # T_split<-sample.split(Thai_Surv_test,SplitRatio = 0.3)
# # 
# # T_train=subset(Thai_Surv_test,
# #                T_split==TRUE)
# # T_test=subset(Thai_Surv_test,
# #               T_split==FALSE)
# # 
# # 
# # 
# # 
# # r_fit <- ranger(Surv(surv_yydd,event1) ~ age_cat,
# #                 data = T_train,
# #                 mtry = 1,
# #                 importance = "permutation",
# #                 splitrule = "extratrees",
# #                 verbose = TRUE,
# #                 num.trees = 100)
# # 
# # # Average the survival models
# # death_times <- r_fit$unique.death.times
# # avg_prob <- sapply(surv_prob,mean)
# # 
# # # Plot the survival models for each patient
# # plot(r_fit$unique.death.times,r_fit$survival[1,],
# #      type = "l",
# #      ylim = c(0,1),
# #      xlim=c(0,5),
# #      col = "red",
# #      xlab = "Years",
# #      ylab = "survival",
# #      main = "Cancer Survranger Survival Curves")
# # 
# # #
# # 
# # cols <- colors()
# # for (n in sample(c(2:dim(T_train)[1]), 80)){
# #   lines(r_fit$unique.death.times, r_fit$survival[n,], type = "l", col = cols[n])
# # }
# # lines(death_times, avg_prob, lwd = 2)
# # legend(1000, 0.7, legend = c('Average = black'))
# 
# #Kaplan Meier Curves for OS
# 
# # library(survival)
# # library(ggplot2)
# # library(ggfortify)
# # library(survminer)
# 
# #Plotting Survival Curves Using ggplot2 and ggfortify:
# # 
# # 
# # Y = Surv(Thai_Surv_test$surv_yy, Thai_Surv_test$event1 == 1)
# # xlim=c(0,5)
# # 
# # 
# # kmfit = survfit(Y ~ 1)
# # 
# # summary(kmfit, times = c(seq(0, 5, by = 100)))
# # 
# # plot(kmfit, xlim=c(0,5),
# #      lty = c("solid", "dashed"),
# #      col = c("black", "grey"),
# #      xlab = "Survival Time In Years",
# #      ylab = "Survival Probabilities")
# # 
# # title("Breast Cancer Survival Kaplan Meier")
# # 
# # #First setup survival object
# # km <- Surv(time =Thai_Surv_test$surv_yydd , event = Thai_Surv_test$event1)
# # #Fit Kaplan Meier, stratifying by treatment
# # km_breast_cancer<-survfit(km~age_cat, 
# #                           data=Thai_Surv_test,
# #                           type='kaplan-meier',conf.type='log')
# # 
# # library(survminer)
# # 
# # ggsurvplot(km_breast_cancer, xlim = c(0, 5))+
# #   labs(x = " Survival Time (years) ",
# #        y = "Survival Probabilities",
# #        title = "Kaplan Meier Of Breast Cancer Patients in Thailand")
# 
# # theme(plot.title = element_text(hjust = 0.5),
# #       axis.title.x = element_text(face="bold", colour="#FF7A33", size = 12),
# #       axis.title.y = element_text(face="bold", colour="#FF7A33", size = 12),
# #       legend.title = element_text(face="bold", size = 10))
# 


core.csu_asr <- function(df_data, var_age, var_cases, var_py, group_by=NULL,
                         var_age_group=NULL, missing_age = NULL,db_rate = 100000,
                         first_age = 1, last_age = 18, pop_base = "SEGI", crude_rate = FALSE,
                         var_st_err=NULL,correction_info=FALSE, var_asr="asr",age_dropped = FALSE,
                         pop_base_count = NULL, age_label_list = NULL, Rcan_print=FALSE) 
{
  
  
  
  bool_dum_by <- FALSE
  bool_dum_age <- FALSE
  
  if (is.null(group_by)) {
    
    df_data$CSU_dum_by <- "dummy_by"
    group_by <- "CSU_dum_by"
    bool_dum_by <- TRUE
    
  }
  
  
  
  if (is.null(var_age_group)) {
    
    df_data$CSU_dum_age <- "dummy_age_gr"
    var_age_group <- "CSU_dum_age"
    group_by <- c(group_by, "CSU_dum_age")
    bool_dum_age <- TRUE
    
  }
  
  
  dt_data <- data.table(df_data, key = group_by) 
  setnames(dt_data, var_age, "CSU_A")
  setnames(dt_data, var_cases, "CSU_C")
  setnames(dt_data, var_py, "CSU_P")
  
  if (crude_rate == FALSE)
  {
    if (is.null(pop_base_count)) {
      dt_data <-  dt_data[,list( CSU_C=sum(CSU_C), CSU_P=sum(CSU_P)), by=c(group_by, "CSU_A")]
    }
    
    
    # create index to keep order
    index_order <- c(1:nrow(dt_data))
    dt_data$index_order <- index_order
    
    # missing age 
    dt_data[dt_data$CSU_A %in% missing_age,CSU_A:=NA ] 
    dt_data[is.na(dt_data$CSU_A),CSU_P:=0 ] 
    
    #parse age
    dt_data[,CSU_A :=  as.numeric(gsub(".*?(\\d{1,3}).*$", "\\1",CSU_A, perl=TRUE))]
    if (max(dt_data$CSU_A,na.rm=TRUE) > 25) {
      dt_data[,CSU_A := round((CSU_A/5)+1)]
    }
    
    #create age dummy: 1 2 3 4 --- 19
    dt_data$age_factor <- as.numeric(c(as.factor(dt_data$CSU_A)))
    
    
    # correction factor 
    dt_data$correction <- 1 
    if (!is.null(missing_age)) {
      
      
      dt_data[, total:=sum(CSU_C), by=group_by] #add total
      dt_data[!is.na(dt_data$age_factor) , total_known:=sum(CSU_C), by=group_by] #add total_know
      dt_data$correction <- dt_data$total / dt_data$total_know 
      dt_data[is.na(dt_data$correction),correction:=1 ] 
      dt_data$total <- NULL
      dt_data$total_known <- NULL
      
    }
    
    if (is.null(pop_base_count)) {
      
      # create world population DF for different nb of age group
      SEGI_pop <- c(12000,10000,9000,9000,8000,8000,6000,6000,6000,6000,5000,4000,4000,3000,2000,1000,500,500)
      EURO_pop <- c(8000,7000,7000,7000,7000,7000,7000,7000,7000,7000,7000,6000,5000,4000,3000,2000,1000,1000)
      EURO2_pop <- c(5000,5500,5500,5500,6000,6000,6500,7000,7000,7000,7000,6500,6000,5500,5000,4000,2500,2500)
      WHO_pop <- c(8860,8690,8600,8470,8220,7930,7610,7150,6590,6040,5370,4550,3720,2960,2210,1520,910,630)
      
      if (pop_base == "EURO") {
        pop <- EURO_pop
      } else if (pop_base == "EURO2") {
        pop <- EURO2_pop
      } else if (pop_base == "WHO") {
        pop <- WHO_pop
      } else {
        pop <- SEGI_pop
      }
      
      # calculated total pop for age selected 
      total_pop <- sum(pop[first_age:last_age])
      
      Standard_pop <- data.table(pop = pop, age_factor= c(1:18))
      
      pop[17] <- pop[17]+ pop[18]
      pop[18] <- 0
      Standard_pop$pop17 <- pop
      pop[16] <- pop[16]+ pop[17]
      pop[17] <- 0
      Standard_pop$pop16 <- pop
      pop[15] <- pop[15]+ pop[16]
      pop[16] <- 0
      Standard_pop$pop15 <- pop
      
      
      
      #age dropped option
      if (age_dropped) {
        dt_data$age_factor <- dt_data$age_factor + first_age -1   
      }
      
      
      # keep age selected 
      dt_data=dt_data[dt_data$age_factor %in% c(first_age:last_age) | is.na(dt_data$age_factor), ]
      
      # calculated maximum age group with population data
      if (last_age == 18) {
        dt_data <- merge(dt_data, dt_data[dt_data$CSU_P != 0,list(nb_age_group = max(age_factor)), by=var_age_group], by=var_age_group)  
      } else {
        dt_data$nb_age_group <- 18
      }
      
      # show population with less than 18 age group
      if (last_age == 18) {
        temp <- subset(dt_data,nb_age_group <18, select= c(var_age_group, "nb_age_group"))
        if (nrow(temp) >0) {
          setkey(temp,NULL)
          
          
          if (Rcan_print) {
            
            cat("\n")
            cat("Population with less than 18 age group:\n\n" )
            print
            print(unique(temp), row.names = FALSE)
            cat("\n")
            
          }
        }
        temp <- NULL
      }
      
      #regroup case for population with nb of age group <  18 
      for (i in 15:17) {
        
        if (i %in% dt_data$nb_age_group) {
          
          dt_data[nb_age_group == i & age_factor >= i , CSU_C:=sum(CSU_C), by=group_by] #add total_know
          dt_data[nb_age_group == i & age_factor > i & !is.na(age_factor), CSU_C := 0] 
          
        } 
      }
      
      #add world pop to database 
      dt_data <- merge(dt_data,Standard_pop, by =c("age_factor"), all.x=TRUE )
      
      Standard_pop <- NULL
      dt_data[nb_age_group==17, pop:=pop17]
      dt_data[nb_age_group==16, pop:=pop16]
      dt_data[nb_age_group==15, pop:=pop15]
      
      #return(dt_data)
      
    } else {
      
      #keep age group selected 
      dt_data <- dt_data[age_factor %in% (first_age:last_age), ]
      
      #calcul total pop for canreg
      total_pop <-sum(unique(dt_data[, c("age_factor", pop_base_count), with=FALSE])[[pop_base_count]])
      
      #get age group list variable
      if (is.null(age_label_list)) {
        age_label_list <- var_age
      }
      age_group_list <- as.character(unique(dt_data[[age_label_list]]))
      age_group_list <- paste(age_group_list,  collapse=" ")
      
      #rename variable population reference
      setnames(dt_data, pop_base_count, "pop")
    }
    
    #calcul ASR
    
    dt_data[dt_data$CSU_P != 0,rate:= dt_data$CSU_C[dt_data$CSU_P != 0]/ dt_data$CSU_P[dt_data$CSU_P != 0] * db_rate]
    dt_data$asr <- dt_data$rate * dt_data$pop
    dt_data[is.na(dt_data$asr),asr:=0 ] 
    
    dt_data$st_err <- ( dt_data$rate * (dt_data$pop^2) * (db_rate - dt_data$rate))/dt_data$CSU_P
    dt_data[is.na(dt_data$st_err),st_err:=0 ] 
    
    # to check order 
    dt_data<- dt_data[order(dt_data$index_order ),]
    dt_data<-  dt_data[,list( CSU_C=sum(CSU_C), CSU_P=sum(CSU_P),asr=sum(asr),st_err = sum(st_err),correction = max(correction)), by=group_by]
    
    dt_data$asr <- dt_data$asr / total_pop
    dt_data$asr <- dt_data$asr * dt_data$correction
    dt_data$st_err <- (dt_data$st_err / (total_pop^2))^(1/2)
    dt_data$st_err <- dt_data$st_err * dt_data$correction
    
    
    if (is.null(var_st_err)) {
      dt_data$st_err <- NULL
    } else {
      setnames(dt_data, "st_err", var_st_err)
    }
    
    if (var_asr!="asr") {
      setnames(dt_data, "asr", var_asr)
    }
    
    if (!correction_info) {
      dt_data$correction <- NULL
    }
    
  }
  else 
  {
    
    #parse age
    dt_data[,CSU_A :=  as.numeric(gsub(".*?(\\d{1,3}).*$", "\\1",CSU_A, perl=TRUE))]
    if (max(dt_data$CSU_A,na.rm=TRUE) > 25) {
      dt_data[,CSU_A := round((CSU_A/5)+1)]
    }
    
    #create age dummy: 1 2 3 4 --- 19
    dt_data$age_factor <- as.numeric(c(as.factor(dt_data$CSU_A)))
    
    #age dropped option
    if (age_dropped) {
      dt_data$age_factor <- dt_data$age_factor + first_age -1   
    }
    
    # keep age selected 
    dt_data=dt_data[dt_data$age_factor %in% c(first_age:last_age) | is.na(dt_data$age_factor), ]
    
    dt_data <- dt_data[,list( CSU_C=sum(CSU_C), CSU_P=sum(CSU_P)), by=c(group_by)]
    dt_data <- dt_data[, CSU_RESULT := CSU_C/CSU_P*db_rate]
    if (var_asr=="asr") {
      setnames(dt_data, "CSU_RESULT", 'crude_rate')
    }
    else 
    {
      setnames(dt_data, "CSU_RESULT", var_asr)
    }
    
  }
  
  df_data <- data.frame(dt_data)
  
  if (bool_dum_age) {
    df_data$CSU_dum_age <- NULL
  }
  if (bool_dum_by) {
    df_data$CSU_dum_by <- NULL
  }
  
  setnames(df_data, "CSU_C", var_cases)
  setnames(df_data,  "CSU_P", var_py)
  
  
  if (is.null(pop_base_count)) {
    
    if (Rcan_print) {
      temp <- last_age*5-1
      if (last_age == 18)  temp <- "99+"
      
      if (crude_rate == FALSE)
      {
        cat("ASR have been computed for the age group ", (first_age-1)*5,"-", temp , "\n",  sep="" )
      }
      else {
        cat("Crude rate have been computed for the age group ", (first_age-1)*5,"-", temp , "\n",  sep="" )
      }
      
    }
    temp<- NULL
    
  } else {
    
    #cat("ASR have been computed for the age groups:\n",age_group_list , "\n",  sep="" )
    age_group_list<- NULL
    
  }
  
  return(df_data)
  
}



