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
#library(mexhaz)
library(readr)
library(ggplot2)
#library(relsurv)
library(janitor)
library(data.table)

setwd("/home/langseliuso/AD/Data_SIT")

########################
#Comment from here

load("GCO_pop2020.RData")

Cancer_codes <- read.csv("dict_cancer.csv") %>% as.data.frame()



#Reading all the variables
#GCO_country_info.csv has correct country_label variable to match with pop_mort2

HDI_Region_Mapping<-read.csv("HDI2018_GLOBOCAN2020.csv")

# population data

load("GCO_pop2020.RData")

country_codes <-
  read.csv("GCO_country_info.csv", stringsAsFactors = FALSE) %>%
  filter(country_code<900) %>%
  dplyr::select(country_code, country_label)



globocan <- read.csv("Globocan.csv")%>%
  as.data.frame()%>%
  filter(type==0, 
         sex!=0)%>%
  # group_by(country_code, cancer_code, sex, age)%>%
  # mutate(cases=sum(cases),
  #        py=sum(py))%>%
  # ungroup()%>%
  # distinct()
  dplyr::select(-cancer_label, -country_label)

PAFs <- read.csv("combinedPAFs_cases_02.06.23.csv")%>%
  as.data.frame()%>%
  distinct()%>%
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




Cancer_codes <- read.csv("dict_cancer.csv") %>% as.data.frame()
Cancer_codes_Survcan <- read.csv("cancer_codes_Survcan.csv") %>% as.data.frame()

#




# read survival - when new file check coding used  for cancer codes

Survival_Modelled3 <-  read_dta("survival_SURVCAN_anchor_ALL_all34_byHDI.dta")%>%
  as.data.frame()%>%
  mutate(country_name=gsub('`"','', country_name),
         country_name=gsub("'",'', country_name),
         country_name=gsub('"','', country_name)
  )%>%
  clean_names()%>%
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
  filter(cancer_label!="Colorectal")%>%
  dplyr::select(-rel_surv_vh, -time, -rel_surv_lmh, -icd_code)


Survival_Modelled2_lip<-Survival_Modelled3%>%
  filter(cancer_code==41)%>%
  dplyr::mutate(cancer_code=1,
                cancer_label="Lip, oral cavity")

Survival_Modelled2_salivary<-Survival_Modelled3%>%
  filter(cancer_code==41)%>%
  dplyr::mutate(cancer_code=2,
                cancer_label="Salivary glands")

Survival_Modelled2_oropharynx<-Survival_Modelled3%>%
  filter(cancer_code==42)%>%
  dplyr::mutate(cancer_code=3,
                cancer_label="Oropharynx")

Survival_Modelled2_naso<-Survival_Modelled3%>%
  filter(cancer_code==42)%>%
  dplyr::mutate(cancer_code=4,
                cancer_label="Nasopharynx")

Survival_Modelled<-Survival_Modelled2%>%
  full_join(Survival_Modelled2_lip)%>%
  full_join(Survival_Modelled2_salivary)%>%
  full_join(Survival_Modelled2_oropharynx)%>%
  full_join(Survival_Modelled2_naso)


globocan_cancer_names<-read.csv("Globocan.csv")%>%
  as.data.frame()%>%
  filter(type==0, 
         sex!=0)%>%
  dplyr::select(cancer_code, cancer_label)%>%distinct()


popmort2<-read_dta("who_ghe_popmort.dta")%>%as.data.frame()%>%
  left_join(country_codes)

p <- read_dta("who_ghe_group.dta")%>%
  as.data.frame()



Thailand_expected_Survival<-read.csv("Thailand_expected_Survival.csv")%>%as.data.frame()




Reference_Survival<-read.csv("Reference_Survival.csv")%>%
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



Reference_Survival_Survcan<-read.csv("Reference_Survival_Survcan.csv")%>%
  as.data.frame()%>%
  dplyr::select( age_cat,
                 cancer_code,
                 rel_surv)%>%
  dplyr::rename("surv_ref"="rel_surv")%>%
  distinct()%>%
  mutate(surv_ref=case_when(surv_ref>1~ 1,
                            surv_ref<=1~ surv_ref))%>%
  filter(!is.na(cancer_code))

load("ExpectedSurvival.RData")


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




###############################################
#
# Net survival and Avoidable deaths - Simulated data and new equations
# Date: 20/01/2023
# Version 4.0 - 
#
#Load files and packages in AD_2.R file
#
# Works for multiple cancer sites currently.
#
#
###############################################


#setwd("C:\\Users\\langseliuso\\Documents\\GitHub\\avoidable_deaths\\AD Preventable and Treatable")

# Expected survival

ES3<-p%>%
  as.data.frame()%>%
  dplyr::filter(year==2019)%>%
  dplyr::rename(ES="es")%>%
  select(-country_label, -year)


#Adding so the expected survival is the same for patients aged 70+
# 
# ES4_16<-ES4%>%dplyr::filter(age==15)%>%
#   dplyr::mutate(age=16)
# 
# ES4_17<-ES4%>%dplyr::filter(age==15)%>%
#   dplyr::mutate(age=17)
# 
# ES4_18<-ES4%>%dplyr::filter(age==15)%>%
#   dplyr::mutate(age=18)
# 
# ES3<-ES4%>%
#   full_join(ES4_16)%>%
#   full_join(ES4_17)%>%
#   full_join(ES4_18)%>%
#   distinct()

# Imputing missing countries 

# For For all the French overseas and territories (Polynesia, Guyana, Guadeloupe, Martinique, Reunion, New Caledonia)
# ES_France1<-ES3%>%dplyr::filter(country_code==250)%>%dplyr::mutate(ES3=ES) %>%select(-ES) 
# ES_France2<-ES_France1%>%dplyr::mutate(country_code=254)
# ES_France3<-ES_France1%>%dplyr::mutate(country_code=258)
# ES_France4<-ES_France1%>%dplyr::mutate(country_code=312)
# ES_France5<-ES_France1%>%dplyr::mutate(country_code=474)
# ES_France6<-ES_France1%>%dplyr::mutate(country_code=540)
# ES_France7<-ES_France1%>%dplyr::mutate(country_code=638)
# 
# # Palestine? - Imputed by HDI 0.690 average plus minus 0.1 HDI
# 
# HDI_countries_palestine<-Survival_Modelled%>%
#   dplyr::filter(0.690-0.05<=hdi_value &hdi_value<=0.690+0.05)%>%
#   select(country_code)%>%
#   distinct()
# 
# ES_palestine<-ES3%>%dplyr::filter(country_code%in%HDI_countries_palestine$country_code)%>%
#   dplyr::group_by(age)%>%
#   dplyr::mutate(ES=mean(ES, na.rm=T))%>%
#   dplyr::mutate(country_code=275)%>%
#   distinct()%>%
#   dplyr::mutate(ES3=ES)%>%select(-ES)
# 
# 
# # Western Sahara? - Imputed by HDI 0.690 average plus minus 0.1 HDI
# 
# HDI_countries_palestine<-Survival_Modelled%>%
#   dplyr::filter(0.690-0.05<=hdi_value &hdi_value<=0.690+0.05)%>%
#   select(country_code)%>%
#   distinct()
# 
# ES_palestine<-ES3%>%dplyr::filter(country_code%in%HDI_countries_palestine$country_code)%>%
#   dplyr::group_by(age)%>%
#   dplyr::mutate(ES=mean(ES, na.rm=T))%>%
#   dplyr::mutate(country_code=275)%>%
#   distinct()%>%
#   dplyr::mutate(ES3=ES)%>%select(-ES)
# 
# 
# 
# 
# # Puerto Rico and Guam - Using US
# ES_PR<-ES3%>%dplyr::filter(country_code==840)%>%dplyr::mutate(ES3=ES)%>%select(-ES)%>%dplyr::mutate(country_code=630)
# 
# ES_Guam<-ES3%>%dplyr::filter(country_code==840)%>%dplyr::mutate(ES3=ES)%>%select(-ES)%>%dplyr::mutate(country_code=316)
# 
# ES_Additional<-ES_France2%>%
#   full_join(ES_France3)%>%
#   full_join(ES_France4)%>%
#   full_join(ES_France5)%>%
#   full_join(ES_France6)%>%
#   full_join(ES_France7)%>%
#   full_join(ES_PR)%>%
#   full_join(ES_palestine)%>%
#   full_join(ES_Guam)

ES2<-ES3



Survival_Modelled


# Anchored and combined data set at t=5 with anchored values from Israel and Thailand

popmort<-popmort2%>%
  dplyr::filter(sex!=0)%>%
  dplyr::filter(year==2015)%>%
  dplyr::mutate(mx=1-prob)%>%
  dplyr::mutate(country_code=as.numeric(country_code))%>%
  dplyr::group_by(country_code,age,year)%>%
  dplyr::summarize(country_label, 
            country_code,
            age_label)%>% 
  as.data.frame()%>%
  distinct()



countries_5y<-Survival_Modelled%>%
  filter(cancer_label!="Nasophar")%>%
  dplyr::mutate(rel_surv=case_when(rel_surv>1~ 1,
                            rel_surv<=1~ rel_surv))%>%
  dplyr::mutate(country_label = str_remove( country_label,'"'))%>%
  dplyr::mutate(country_label = str_remove( country_label,"'"))%>%
  dplyr::mutate(country_label = str_remove( country_label,"`"))%>%
  dplyr::mutate(country_label = str_remove( country_label,'"'))%>%
  arrange(country_label)%>%
  distinct()



Countries_Simulated <-countries_5y%>%
  dplyr::mutate(age = case_when(
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
 #   age>=16 ~ 16,
    age==16 ~ 16,
    age==17 ~ 17,
    age==18 ~ 18,
  ))%>%
  dplyr::mutate(
    age_cat = case_when(
      age>=4 & age<14 ~ "15-64",
      age>=14 ~ "65-99",
      age<4 ~"0-15"
    ))%>%
  dplyr::filter(age_cat!="0-15")%>%
  dplyr::group_by(country_label,cancer_label,age_cat)%>%
  dplyr::summarize(country_code,country_label, cancer_code, cancer_label,
            age, age_cat,
            rel_surv, hdi_group)%>%
  as.data.frame()




simulated_overall<-Countries_Simulated%>%
  as.data.frame()%>%
  droplevels()%>%
  dplyr::group_by(country_label,cancer_label, age_cat,age)%>%
  dplyr::summarize(country_code,country_label, cancer_code, cancer_label,rel_surv,
            age_cat, age, hdi_group)%>%
  distinct()%>%
  arrange(country_label,cancer_label, age)



# PAF combinations

PAFs_age_Cat <- PAFs%>%
  distinct()%>%
  as.data.frame()%>%
  dplyr::mutate(age = case_when(
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
  as.data.frame()%>%
  dplyr::mutate(
    age_cat = case_when(
      age>=4 & age<14 ~ "15-64",
      age>=14 ~ "65-99",
      age<4 ~"0-15"
    ))%>%
  dplyr::filter(age_cat!="0-15")%>%
  droplevels()%>%
  as.data.frame()%>%
  left_join(ES2, by=c("country_code","age","sex"))%>%
  ungroup()%>%
  as.data.frame()%>%
  select(country_code,country_label, cancer_code, cancer_label,
         age, sex,age_cat, 
         cases, cases.prev, 
         cases.notprev, af.comb,
         ES)%>%
  distinct()%>%
  group_by(country_label,cancer_label, age,sex) %>%
  distinct()%>%
  as.data.frame()%>%
  group_by(country_label,cancer_label, age, sex) %>%
  # dplyr::mutate(af.comb = case_when(cases!=0 ~  sum(cases.prev, na.rm=T)/sum(cases, na.rm=T),
  #                                  cases.prev==0 ~ 0))
  dplyr::mutate(country_code,country_label, cancer_code, cancer_label,
            age, sex,age_cat, 
            cases=sum(cases, na.rm=T),
            cases.prev=sum(cases.prev, na.rm=T), 
            cases.notprev=sum(cases.notprev, na.rm=T),
            total_overall=cases,
            ES)%>%
  ungroup()%>%
    distinct()%>%
  group_by(country_label,cancer_label, age,sex) %>%
  as.data.frame()%>%
  dplyr::mutate(af.comb = case_when(cases!=0 ~  cases.prev/cases,
                                    cases==0 ~ 0))%>%
  distinct()%>%
  select(country_code,country_label, cancer_code, cancer_label,
  age, sex,age_cat, 
  cases, cases.prev, 
  cases.notprev, af.comb,
  total_overall, ES)%>%
  distinct()%>%
  dplyr::group_by(country_label,cancer_label, age)


PAFs2 <- PAFs_age_Cat%>%
  as.data.frame()%>%
  droplevels()%>%
  dplyr::group_by(country_code,cancer_code,cancer_label, age_cat,age,sex)%>%
 # dplyr::mutate(total_age_prev=sum(cases.prev, na.rm=T))%>%
  as.data.frame()%>%
  
  dplyr::group_by(country_code,cancer_code,age,sex)%>%
  dplyr::select(country_label, cancer_label,
            age_cat, age,  total_overall,cases,
            cases.prev, ES, af.comb)%>%
  distinct()%>%
  arrange(cancer_label,
          age)%>%
  select(-cancer_label,-country_label)


Simulated_Data_PAF_1 <- simulated_overall%>%
  ungroup()%>%
  dplyr::filter(age_cat!="Overall")%>%
  left_join(PAFs2,by=c("country_code"="country_code",
                       "cancer_code"="cancer_code",
                       "age_cat"="age_cat",
                       "age"="age"))%>%
  ungroup()%>%
  dplyr::group_by(country_code,cancer_code, age, sex)%>%
  as.data.frame()%>%
  # dplyr::mutate(af.comb=case_when(cases!=0 ~ sum(cases.prev, na.rm=T)/sum(cases, na.rm=T),
  #                          cases==0  ~    0))%>%
  # dplyr::mutate(rel_surv=case_when(cases!=0 ~ sum(rel_surv*cases, na.rm=T)/sum(cases, na.rm=T),
  #                          cases==0  ~    rel_surv))%>%
  as.data.frame()%>%
  dplyr::group_by(country_code,cancer_code, age, sex)%>%
  dplyr::select(country_code, 
            country_label, 
            cancer_code, cancer_label,
            age_cat, age, total_overall,
            rel_surv,
            af.comb,
            rel_surv,
            ES,
            total_overall,
            hdi_group)%>%
  droplevels()%>%
  ungroup()%>%
  distinct()%>%
  as.data.frame()


#for europe specific analysis - comment out to ignore

HDI_Region_Mapping2 <- HDI_Region_Mapping%>%
  
  dplyr::filter(area<=21)%>%
  dplyr::mutate(
    area = case_when(
      area %in% c(19,20,21)  ~ 19,
      !area %in% c(19,20,21)  ~ area
    ))%>% 
  dplyr::mutate(
    country_label= case_when(
      area %in% c(19)  ~ "Melanesia/ Micronesia/ Polynesia ",
      !area %in% c(19)  ~ country_label
    ))%>% 
  select(-country_label)%>%
  distinct()



Simulated_Data_PAF_All <- Simulated_Data_PAF_1%>%
  dplyr::mutate(rel_surv=as.double(rel_surv))%>%
  dplyr::mutate(af.comb=as.double(af.comb))%>% 
  dplyr::mutate(total_overall=as.double(total_overall))%>% 
  arrange(country_label,cancer_code,age,sex)%>%
  left_join(Reference_Survival, by=c("age","cancer_code"))%>%
  select(-cancer_label)%>%
  left_join(globocan_cancer_names,by=c("cancer_code"))%>%
  left_join(HDI_Region_Mapping2)#%>%
  #filter(continent==5) # change for region speciic analysiss


# Avoidable deaths

# Three AD calcs 

# first need to make sure data is in right format (numeric columns)

# Applying the equation from Rutherford 2015 for AD.

<<<<<<< HEAD:AD Preventable and Treatable/1.AD_Modelled_new_eqns.R
bootstrap_iterations<- 1000 #500 iterations
sd_assumed <- 0.01#/sqrt(bootstrap_iterations) # 1% uncertainty assumed

#one option to guesstimate for variable what the standard deviation and


#initiziaing the data frame
AD_country_all_cancers_int<-data.frame()
AD_country_all_cancers_int_1<-data.frame()
AD_country_all_cancers_int_2<-data.frame()
AD_country_all_cancers_int_3<-data.frame()
AD_country_all_cancers_int_4<-data.frame()
AD_country_all_cancers_int_5<-data.frame()

for (i in 1:bootstrap_iterations) {
# Use the number of rows in the data frame as the length for rnorm() 
  Avoidable_Deaths_Simulated_All3<- Simulated_Data_PAF_All2 %>%
    group_by(country_code, cancer_code, sex, age) %>%
    mutate( # check with Jerome about the errors of the rel_surv var
      rel_surv = rnorm(n(), mean = rel_surv, sd = sd_assumed),
      # first try the PAFs for which we have CIs and when we don't
      # if error estimates are missing is there alternative data sets we can use?
      # can we find any assumptions of SE from other data sets for countries?
      # what is the range of SEs in PAF literature?-> gives an idea of what is reasonable
      # Split up PAFs to run simulation?     
      # Here use SE when you have it and then work with this or see about values from other articles
      af.comb = rnorm(n(), mean = af.comb, sd = sd_assumed),
           # assume Poisson distribution for incidence - Iacopo article 
           total_overall = rnorm(n(), mean = total_overall, sd = total_overall*sd_assumed),
           surv_ref = max(surv_ref))%>% # simulate rel_surv and then find best surv_ref each time
=======
#old equations###################
# dplyr::mutate(AD_prev=af.comb * total_overall * (1 - rel_surv *  ES ))%>%
#   dplyr::mutate(AD_treat=total_overall * (surv_ref-rel_surv) * ES)%>%
#   dplyr::mutate(AD_treat_not_prev = (1-af.comb)* total_overall * (surv_ref-rel_surv) * ES)%>%
#   dplyr::mutate(AD_unavoid = (1-af.comb)*total_overall*(1-surv_ref*ES))%>%
#   dplyr::mutate(Expect_deaths=(1-(rel_surv*ES))*total_overall)%>%
#########################################################

Avoidable_Deaths_Simulated_All3<-Simulated_Data_PAF_All%>%
>>>>>>> parent of 6db619e (Misc):AD Preventable and Treatable/1. AD_Modelled_new_eqns.R
  dplyr::group_by(country_code, cancer_code, age, sex)%>%
  dplyr::mutate(AD_prev= total_overall * af.comb *ES* (1 - rel_surv))%>%
  dplyr::mutate(AD_treat=(1-af.comb) *total_overall * (surv_ref-rel_surv) * ES)%>%
  #dplyr::mutate(AD_treat_not_prev = (1-af.comb) * total_overall * (surv_ref-rel_surv) * ES)%>% #scenario 1
  dplyr::mutate(AD_unavoid =total_overall*(1-ES*surv_ref-af.comb*ES*(1-surv_ref)))%>%
  #dplyr::mutate(total_deaths2=total_overall*(1-ES*rel_surv))%>%
  dplyr::mutate(total_deaths2=AD_prev+AD_treat+AD_unavoid)%>%
  select("country_code","country_label","age_cat","age","sex","cancer_code","cancer_label",   
         "AD_treat",
        # "AD_treat_not_prev",
         "AD_prev",
         "AD_unavoid",
         "total_deaths2",
         "total_overall",
         "hdi_group")%>%
  dplyr::rename("cancer"="cancer_label")


colorectal<-Avoidable_Deaths_Simulated_All3%>%
  dplyr::filter(cancer_code%in%c("8","9"))%>%
  dplyr::mutate(cancer="Colorectal",
         cancer_code=8)%>%
  dplyr::group_by(country_code, cancer_code, age, sex)%>%
  dplyr::mutate(AD_prev=sum(AD_prev),
         AD_treat=sum(AD_treat),
       #  AD_treat_not_prev=sum(AD_treat_not_prev),
         AD_unavoid=sum(AD_unavoid),
         total_deaths2=sum(total_deaths2),
         total_overall=sum(total_overall))%>%
  distinct()%>%
  as.data.frame()

colorectal


Avoidable_Deaths_Simulated_All2<-Avoidable_Deaths_Simulated_All3%>%
  dplyr::filter(!cancer_code%in%c("8","9"))%>%
  full_join(colorectal)%>%
  dplyr::mutate(cancer = replace(cancer, cancer == "Liver and intrahepatic bile ducts", "Liver"))%>%
  dplyr::mutate(cancer = replace(cancer, cancer == "Trachea, bronchus and lung", "Lung"))%>%
  distinct()


Avoidable_Deaths_Simulated_All2

# Globocan population data 

pop20202 <- pop2020%>%
  as.data.frame()%>%
  dplyr::filter(sex!=0)%>%
  dplyr::mutate(age = case_when(
    age==1~ 1,
    age==2~ 2,
    age==3~ 3,
    age==4~ 4,
    age>4 & age<9 ~ 4,
    age==9 ~ 9,
    age==10 ~ 10,
    age==11 ~ 11,
    age==12 ~ 12,
    age==13 ~ 13,
    age==14 ~ 14,
    age==15 ~ 15,
    # age>=16 ~ 16,
    age==16 ~ 16,
    age==17 ~ 17,
    age==18 ~ 18,
  ))%>%
  dplyr::group_by(country_code, age,sex)%>%
  dplyr::mutate(py=sum(py))%>%
  distinct()%>%
  select(-country_label)




# Weights
w2 <- c(12000,10000,9000,9000,8000,8000,6000,6000,6000,6000,5000,4000,4000,3000,2000,1000,500,500) #SEGI_pop weights
age <- c(1:18)
weights2 <- data.frame(w2, age)


weights2 <- weights2%>%dplyr::mutate(age = case_when(
  age==1~ 1,
  age==2~ 2,
  age==3~ 3,
  age==4~ 4,
  age>4 & age<9 ~ 4,
  age==9 ~ 9,
  age==10 ~ 10,
  age==11 ~ 11,
  age==12 ~ 12,
  age==13 ~ 13,
  age==14 ~ 14,
  age==15 ~ 15,
  #age>=16 ~ 16,
  age==16 ~ 16,
  age==17 ~ 17,
  age==18 ~ 18,
))%>%
  dplyr::filter(age>3)%>%
  dplyr::mutate(total=sum(w2))%>%
  dplyr::group_by(age)%>%
  dplyr::mutate(w=sum(w2)/total)%>%
  select(-w2, -total)%>%
  distinct()


Avoidable_Deaths_Simulated_All<- Avoidable_Deaths_Simulated_All2%>%
  as.data.frame()%>%
  dplyr::mutate(age=as.numeric(as.character(age)))%>%
  dplyr::mutate(AD_prev=as.numeric(as.character(AD_prev)))%>%
  dplyr::mutate(AD_unavoid=as.numeric(as.character(AD_unavoid)))%>%
  dplyr::mutate(total_overall=as.numeric(as.character(total_overall)))%>%
  dplyr::mutate(AD_treat=as.numeric(as.character(AD_treat)))%>%
  #dplyr::mutate(AD_treat_not_prev=as.numeric(as.character(AD_treat_not_prev)))%>%
  dplyr::mutate(AD_sum=AD_prev + AD_unavoid + AD_treat)%>%
  dplyr::mutate(cancer_code=as.numeric(cancer_code))%>%
  as.data.frame()%>%distinct()%>%
  dplyr::mutate(country_code=as.integer(country_code))%>%
  left_join(pop20202, c("sex", "country_code", "age"))%>%
  left_join(weights2)%>%
  dplyr::mutate(AD_treat = case_when(AD_treat<0 ~ 0, 
                              # Numerical calculation error causes the countries which are references  to go slightly negative. 
                              # By definition this is zero This is rounded to 0
                              TRUE ~ AD_treat))


AD_country_all_cancers_int_1<-rbind(AD_country_all_cancers_int_1, Avoidable_Deaths_Simulated_All)


print(i)
}


AD_country_all_cancers_int_1<-AD_country_all_cancers_int_1%>%mutate(iteration=1)
AD_country_all_cancers_int_2<-AD_country_all_cancers_int_2%>%mutate(iteration=2)
AD_country_all_cancers_int_3<-AD_country_all_cancers_int_3%>%mutate(iteration=3)
AD_country_all_cancers_int_4<-AD_country_all_cancers_int_4%>%mutate(iteration=4)
AD_country_all_cancers_int_5<-AD_country_all_cancers_int_5%>%mutate(iteration=5)


AD_country_all_cancers_int<-rbind(AD_country_all_cancers_int, AD_country_all_cancers_int_1)
AD_country_all_cancers_int<-rbind(AD_country_all_cancers_int, AD_country_all_cancers_int_2)
AD_country_all_cancers_int<-rbind(AD_country_all_cancers_int, AD_country_all_cancers_int_3)
AD_country_all_cancers_int<-rbind(AD_country_all_cancers_int, AD_country_all_cancers_int_4)
AD_country_all_cancers_int<-rbind(AD_country_all_cancers_int, AD_country_all_cancers_int_5)


#AD_prev, AD_unavoid, total_overall, AD_treat, AD_sum

#function to compute confidence intervals
compute_CIs <- function(data) {
  data <- data %>%
    select(-iteration) %>%
    group_by(country_code, cancer_code, sex, age_cat) %>%
    mutate(
      # Computing CIs
      AD_prev_CI_lower = quantile(AD_prev, c(0.025)),
      AD_prev_CI_upper = quantile(AD_prev, c(0.975)),
      AD_treat_CI_lower = quantile(AD_treat, c(0.025)),
      AD_treat_CI_upper = quantile(AD_treat, c(0.975)),
      AD_unavoid_CI_lower = quantile(AD_unavoid, c(0.025)),
      AD_unavoid_CI_upper = quantile(AD_unavoid, c(0.975)),
      total_deaths2_CI_lower = quantile(total_deaths2, c(0.025)),
      total_deaths2_CI_upper = quantile(total_deaths2, c(0.975)),
      AD_sum_CI_lower= quantile(AD_sum, c(0.025)),
      AD_sum_CI_upper = quantile(AD_sum, c(0.975)),
      total_overall_CI_lower = quantile(total_overall, c(0.025)),
      total_overall_CI_upper = quantile(total_overall, c(0.975)),
      # Computing mean values
      AD_prev = mean(AD_prev),
      AD_treat = mean(AD_treat),
      AD_sum = mean(AD_sum),
      AD_unavoid = mean(AD_unavoid),
      total_deaths2 = mean(total_deaths2),
      total_overall= mean(total_overall)
    ) %>%
    distinct() %>%
    as.data.frame() %>%
    dplyr::select(1:2,
      "cancer_code", "cancer", "sex", "age",
      "AD_prev", "AD_prev_CI_lower", "AD_prev_CI_upper",
      "AD_treat", "AD_treat_CI_lower", "AD_treat_CI_upper",
      "AD_sum", "AD_sum_CI_lower", "AD_sum_CI_upper",
      "AD_unavoid", "AD_unavoid_CI_lower", "AD_unavoid_CI_upper",
      "total_deaths2", "total_deaths2_CI_lower", "total_deaths2_CI_upper"
    )

  return(data)
}


AD_country_all_cancers_int_CI<-compute_CIs(AD_country_all_cancers_int)



data.table::fwrite(AD_country_all_cancers_int_CI, "2.Avoidable_Deaths_Simulated_All2_CIs.csv")
data.table::fwrite(AD_country_all_cancers_int, "2.Avoidable_Deaths_Simulated_All2_int.csv")
# 
# 
(sum(Avoidable_Deaths_Simulated_All$AD_treat)+sum(Avoidable_Deaths_Simulated_All$AD_prev))/sum(Avoidable_Deaths_Simulated_All$AD_sum)
(sum(Avoidable_Deaths_Simulated_All$AD_treat)+sum(Avoidable_Deaths_Simulated_All$AD_prev))
sum(AD_country_all_cancers_int_CI$AD_sum_CI_upper)
# 
# 
# 
# 
# 
# dup<-Avoidable_Deaths_Simulated_All%>%
#   select(country_label, cancer, age, sex)%>%
#   group_by(country_label, cancer, age, sex) %>%
#   summarize(n=n())%>%
#   ungroup()%>%
#   filter(n()>1)
# 
# dup2<-Simulated_Data_PAF_All2 %>%
#   # select(country_label, cancer_label, age, sex)%>%
#   group_by(country_label, cancer_label, age, sex) %>%
#   mutate(n=n())%>%
#   filter(n()>1)




## added new ASR code below
Avoidable_Deaths_Simulated_All_overall <- Avoidable_Deaths_Simulated_All%>%
  dplyr::mutate(age_cat="Overall")%>%
  dplyr::group_by(country_code, cancer_code, age_cat)%>%
  dplyr::mutate(AD_prev.asr=sum(AD_prev/py*100000*w), #ASR calculation here
         AD_treat.asr=sum(AD_treat/py*100000*w),
         AD_sum.asr=sum(AD_sum/py*100000*w),
        # AD_treat_not_prev.asr=sum(AD_treat_not_prev/py*100000*w),
         AD_unavoid.asr=sum(AD_unavoid/py*100000*w),
<<<<<<< HEAD:AD Preventable and Treatable/1.AD_Modelled_new_eqns.R
         total.deaths.asr=sum(total_deaths2/py*100000*w)) %>%
  dplyr::select( -total_overall)%>%
=======
         total.deaths.asr=sum(total_deaths2/py*100000*w)) %>% 
  select( -total_overall)%>%
>>>>>>> parent of 6db619e (Misc):AD Preventable and Treatable/1. AD_Modelled_new_eqns.R
  dplyr::mutate(AD_prev = sum(AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_treat = sum(AD_treat,na.rm=T))%>%
  dplyr::mutate(AD_sum = sum(AD_sum,na.rm=T))%>%
  #dplyr::mutate(AD_treat_not_prev = sum(AD_treat_not_prev,na.rm=T))%>%
  dplyr::mutate(AD_unavoid = sum(AD_unavoid,na.rm=T))%>%
  dplyr::mutate(total_deaths2=sum(total_deaths2,na.rm=T))%>%
  dplyr::mutate(py=sum(py))%>%
  select( -age,-sex)%>%
  ungroup()%>%
  dplyr::group_by(cancer_code, country_code,age_cat)%>%
  dplyr::mutate(w=sum(w/2))%>%
  distinct()%>%
  as.data.frame()


Avoidable_Deaths_Simulated_All_age_cat <- Avoidable_Deaths_Simulated_All%>%
  ungroup()%>%
  dplyr::group_by(country_code, cancer_code, age_cat)%>%
  dplyr::mutate(AD_prev.asr=sum(AD_prev/py*100000*w), #ASR calculation here
         AD_treat.asr=sum(AD_treat/py*100000*w),
         AD_sum.asr=sum(AD_sum/py*100000*w),
       #  AD_treat_not_prev.asr=sum(AD_treat_not_prev/py*100000*w),
         AD_unavoid.asr=sum(AD_unavoid/py*100000*w),
<<<<<<< HEAD:AD Preventable and Treatable/1.AD_Modelled_new_eqns.R
         total.deaths.asr=sum(total_deaths2/py*100000*w)) %>%
  dplyr::select(-total_overall)%>%
=======
         total.deaths.asr=sum(total_deaths2/py*100000*w)) %>% 
  select(-total_overall)%>%
>>>>>>> parent of 6db619e (Misc):AD Preventable and Treatable/1. AD_Modelled_new_eqns.R
  dplyr::mutate(AD_prev = sum(AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_treat = sum(AD_treat,na.rm=T))%>%
  #dplyr::mutate(AD_treat_not_prev = sum(AD_treat_not_prev,na.rm=T))%>%
  dplyr::mutate(AD_unavoid = sum(AD_unavoid,na.rm=T))%>%
  dplyr::mutate(total_deaths2=sum(total_deaths2,na.rm=T))%>%
  dplyr::mutate(AD_sum = sum(AD_sum,na.rm=T))%>%
  dplyr::mutate(py=sum(py))%>%
  select( -age,-sex)%>%
  ungroup()%>%
  dplyr::group_by(cancer_code, country_code,age_cat)%>%
  dplyr::mutate(w=sum(w/2))%>%
  distinct()%>%
  full_join(Avoidable_Deaths_Simulated_All_overall)%>%
  as.data.frame()%>%ungroup()







# Data by country, HDI, etc

AD_by_HDI <- Avoidable_Deaths_Simulated_All%>%
  dplyr::mutate(age_cat="Overall")%>%
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  dplyr::group_by(hdi_group, cancer, age,sex)%>%
  dplyr::mutate(py=sum(py))%>%
  dplyr::mutate(AD_treat_prev=sum(AD_prev+AD_treat))%>%
  dplyr::mutate(AD_treat=sum(AD_treat))%>%
  #dplyr::mutate(AD_treat_not_prev=sum(AD_treat_not_prev))%>%
  ungroup()%>%
<<<<<<< HEAD:AD Preventable and Treatable/1.AD_Modelled_new_eqns.R
  dplyr::select(-country_label,-country_code,  hdi_group, cancer, cancer_code, AD_treat, AD_prev,
=======
  select(-country_label,-country_code,  hdi_group, cancer, cancer_code, AD_treat, AD_prev, 
>>>>>>> parent of 6db619e (Misc):AD Preventable and Treatable/1. AD_Modelled_new_eqns.R
         AD_unavoid, AD_sum,-sex, -age, age_cat,-total_overall)%>%
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  dplyr::mutate(hdi_group=as.numeric(hdi_group))%>%
  ungroup()%>%
  dplyr::group_by(hdi_group,cancer)%>%
  dplyr::mutate(AD_prev.asr=sum(AD_prev/py*100000*w), #ASR calculation here
         AD_treat.asr=sum(AD_treat/py*100000*w),
         AD_sum.asr=sum(AD_sum/py*100000*w),
         AD_treat_prev.asr=sum((AD_treat+AD_prev)/py*100000*w),
       #  AD_treat_not_prev.asr=sum(AD_treat_not_prev/py*100000*w),
         AD_unavoid.asr=sum(AD_unavoid/py*100000*w),
         total.deaths.asr=sum(total_deaths2/py*100000*w)) %>%
  dplyr::group_by(hdi_group,cancer)%>%
  dplyr::mutate(AD_treat_prev=sum(AD_prev+AD_treat))%>%
  dplyr::mutate(total_deaths2=sum(total_deaths2))%>%
  dplyr::mutate(AD_treat=sum(AD_treat))%>%
 # dplyr::mutate(AD_treat_not_prev=sum(AD_treat_not_prev))%>%
  dplyr::mutate(AD_prev=sum(AD_prev))%>%
  dplyr::mutate(AD_unavoid=sum(AD_unavoid))%>%
  dplyr::mutate(total_deaths=total_deaths2)%>%
  dplyr::mutate(pAD_treat=AD_treat/total_deaths)%>%
  #dplyr::mutate(pAD_treat_not_prev=AD_treat_not_prev/total_deaths)%>%
  dplyr::mutate(pAD_prev=AD_prev/total_deaths)%>%
  dplyr::mutate(pAD_unavoid=AD_unavoid/total_deaths)%>%
  dplyr::mutate(pAD_treat_prev=AD_treat_prev/total_deaths)%>%
  dplyr::mutate(w=sum(w)/n())%>%
  select(-AD_sum,-py)%>%
  ungroup()%>%
  distinct()%>%
  as.data.frame()


AD_by_HDI


AD_by_HDI_all <-Avoidable_Deaths_Simulated_All%>%
  dplyr::mutate(age_cat="Overall")%>%
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  dplyr::group_by(hdi_group, cancer, age,sex)%>%
  dplyr::mutate(py=sum(py))%>%
  ungroup()%>%
<<<<<<< HEAD:AD Preventable and Treatable/1.AD_Modelled_new_eqns.R
  dplyr::select(-country_label,-country_code,-cancer_code, -cancer,  hdi_group,  AD_treat, AD_prev,
=======
  select(-country_label,-country_code,-cancer_code, -cancer,  hdi_group,  AD_treat, AD_prev, 
>>>>>>> parent of 6db619e (Misc):AD Preventable and Treatable/1. AD_Modelled_new_eqns.R
         AD_unavoid, total_deaths2, AD_sum,-sex, -age, age_cat,-total_overall)%>%
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  dplyr::mutate(hdi_group=as.numeric(hdi_group))%>%
  ungroup()%>%
  dplyr::group_by(hdi_group)%>%
  dplyr::mutate(AD_prev.asr=sum(AD_prev/py*100000*w), #ASR calculation here
         AD_treat.asr=sum(AD_treat/py*100000*w),
         AD_treat_prev.asr=sum((AD_treat+AD_prev)/py*100000*w),
         AD_sum.asr=sum(AD_sum/py*100000*w),
         #AD_treat_not_prev.asr=sum(AD_treat_not_prev/py*100000*w),
         AD_unavoid.asr=sum(AD_unavoid/py*100000*w),
         total.deaths.asr=sum(total_deaths2/py*100000*w)) %>%
  dplyr::mutate(AD_treat_prev=sum(AD_prev+AD_treat))%>%
  dplyr::mutate(total_deaths2=sum(total_deaths2))%>%
  dplyr::mutate(AD_treat=sum(AD_treat))%>%
  #dplyr::mutate(AD_treat_not_prev=sum(AD_treat_not_prev))%>%
  dplyr::mutate(AD_prev=sum(AD_prev))%>%
  dplyr::mutate(AD_unavoid=sum(AD_unavoid))%>%
  dplyr::mutate(total_deaths=total_deaths2)%>%
  dplyr::mutate(pAD_treat=AD_treat/total_deaths)%>%
  #dplyr::mutate(pAD_treat_not_prev=AD_treat_not_prev/total_deaths)%>%
  dplyr::mutate(pAD_prev=AD_prev/total_deaths)%>%
  dplyr::mutate(pAD_unavoid=AD_unavoid/total_deaths)%>%
  dplyr::mutate(pAD_treat_prev=AD_treat_prev/total_deaths)%>%
  dplyr::mutate(cancer="All Cancer Sites")%>%
  dplyr::mutate(w=sum(w)/n())%>%
<<<<<<< HEAD:AD Preventable and Treatable/1.AD_Modelled_new_eqns.R
  dplyr::select(-AD_sum,-py)%>%

=======
  select(-AD_sum,-py)%>%
  
>>>>>>> parent of 6db619e (Misc):AD Preventable and Treatable/1. AD_Modelled_new_eqns.R
  ungroup()%>%
  distinct()%>%
  as.data.frame()


AD_by_HDI_all


# By country for all cancer sites (number and proportion):

AD_country_all_cancers <-Avoidable_Deaths_Simulated_All%>%
  dplyr::mutate(age_cat="Overall")%>%
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  dplyr::group_by(country_label, cancer, age,sex)%>%
  dplyr::mutate(py=sum(py))%>%
  ungroup()%>%
  select(-age,-sex,-total_overall,-AD_sum)%>% #-af.comb,
  dplyr::mutate(AD_treat_prev=AD_treat+AD_prev)%>%
  ungroup()%>%
  select(  -hdi_group,  )%>%
  dplyr::group_by(country_code)%>%
  dplyr::mutate(AD_prev.asr=sum(AD_prev/py*100000*w), #ASR calculation here
         AD_treat.asr=sum(AD_treat/py*100000*w),
         AD_treat_prev.asr=sum((AD_treat+AD_prev)/py*100000*w),
        # AD_treat_not_prev.asr=sum(AD_treat_not_prev/py*100000*w),
         AD_unavoid.asr=sum(AD_unavoid/py*100000*w),
         total.deaths.asr=sum(total_deaths2/py*100000*w)) %>%
  dplyr::mutate(total_deaths=sum(AD_prev, AD_unavoid, AD_treat,na.rm=T))%>%
  dplyr::mutate(total_deaths2=sum(total_deaths2,na.rm=T))%>%
  dplyr::mutate(AD_treat_prev=sum(AD_treat,AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_treat=sum(AD_treat,na.rm=T))%>%
  #dplyr::mutate(AD_treat_not_prev=sum(AD_treat_not_prev,na.rm=T))%>%
  dplyr::mutate(AD_prev=sum(AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_unavoid=sum(AD_unavoid,na.rm=T))%>%
  dplyr::mutate(pAD_treat=AD_treat/total_deaths)%>%
  #dplyr::mutate(pAD_treat_not_prev=AD_treat_not_prev/total_deaths)%>%
  dplyr::mutate(pAD_prev=AD_prev/total_deaths)%>%
  dplyr::mutate(pAD_unavoid=AD_unavoid/total_deaths)%>%
  dplyr::mutate(pAD_treat_prev=AD_treat_prev/total_deaths)%>%
  dplyr::mutate(cancer="All Cancer Sites")%>%
  dplyr::mutate(cancer_code=1000)%>%
  select(-py,-w)%>%
  distinct()%>%
  ungroup()%>%
  distinct()%>%
  as.data.frame()


# By cancer site

AD_cancer <- Avoidable_Deaths_Simulated_All%>%
  dplyr::mutate(age_cat="Overall")%>%
  dplyr::group_by(cancer, age,sex)%>%
  dplyr::mutate(py=sum(py))%>%
  ungroup()%>%
  dplyr::mutate(AD_treat_prev=AD_treat+AD_prev)%>%
  ungroup()%>%
  select(-hdi_group,  -AD_sum,-age,-sex)%>%
  dplyr::mutate(country_code=1000)%>%
  dplyr::mutate(country_label="All Countries")%>%
  dplyr::group_by(cancer)%>%
  dplyr::mutate(AD_prev.asr=sum(AD_prev/py*100000*w), #ASR calculation here
         AD_treat.asr=sum(AD_treat/py*100000*w),
         AD_treat_prev.asr=sum((AD_treat+AD_prev)/py*100000*w),
        # AD_treat_not_prev.asr=sum(AD_treat_not_prev/py*100000*w),
         AD_unavoid.asr=sum(AD_unavoid/py*100000*w),
         total.deaths.asr=sum(total_deaths2/py*100000*w)) %>%
  dplyr::mutate(total_deaths=sum(total_deaths2,na.rm=T))%>%
  select(-total_deaths2)%>%
  dplyr::mutate(AD_treat_prev=sum(AD_treat,AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_treat=sum(AD_treat,na.rm=T))%>%
  #dplyr::mutate(AD_treat_not_prev=sum(AD_treat_not_prev,na.rm=T))%>%
  dplyr::mutate(AD_prev=sum(AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_unavoid=sum(AD_unavoid,na.rm=T))%>%
  dplyr::mutate(pAD_treat=AD_treat/total_deaths)%>%
  # dplyr::mutate(pAD_treat_not_prev=AD_treat_not_prev/total_deaths)%>%
  dplyr::mutate(pAD_prev=AD_prev/total_deaths)%>%
  dplyr::mutate(pAD_unavoid=AD_unavoid/total_deaths)%>%
  dplyr::mutate(pAD_treat_prev=AD_treat_prev/total_deaths)%>%
  select(-py,-w,-total_overall)%>%
  distinct()%>%
  ungroup()%>%
  distinct()%>%
  as.data.frame()



# Calculating by region. Need a file that links countries to region
HDI_Region_Mapping2 <- HDI_Region_Mapping%>%

  dplyr::filter(area<=21)%>%
  dplyr::mutate(
    area = case_when(
      area %in% c(19,20,21)  ~ 19,
      !area %in% c(19,20,21)  ~ area
    ))%>%
  dplyr::mutate(
    country_label= case_when(
      area %in% c(19)  ~ "Melanesia/ Micronesia/ Polynesia ",
      !area %in% c(19)  ~ country_label
<<<<<<< HEAD:AD Preventable and Treatable/1.AD_Modelled_new_eqns.R
    ))%>%
  dplyr::select(-country_label)%>%
=======
    ))%>% 
  select(-country_label)%>%
>>>>>>> parent of 6db619e (Misc):AD Preventable and Treatable/1. AD_Modelled_new_eqns.R
  distinct()


areas <- HDI_Region_Mapping%>%
  dplyr::filter(country_code>=910& country_code<=931 |
           country_code==905 | country_code==906|
           country_code==954| country_code==957 )%>%
  select(area, country_label)%>%
  dplyr::mutate(
    area = case_when(
      area %in% c(19,20,21)  ~ 19,
      !area %in% c(19,20,21)  ~ area
    ))%>%
  dplyr::mutate(
      country_label= case_when(
        area %in% c(19)  ~ "Melanesia/ Micronesia/ Polynesia ",
        !area %in% c(19)  ~ country_label
      ))%>%
  distinct()


# By region
AD_Region2 <- Avoidable_Deaths_Simulated_All%>%
  left_join(HDI_Region_Mapping2, by=c("country_code"))%>%
  dplyr::mutate(age_cat="Overall")%>%
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  dplyr::group_by(area, cancer, age,sex)%>%
  dplyr::mutate(py=sum(py))%>%
  ungroup()%>%
<<<<<<< HEAD:AD Preventable and Treatable/1.AD_Modelled_new_eqns.R
  dplyr::select(-country_label,-country_code,-cancer_code, -cancer,  hdi_group,  AD_treat, AD_prev,
=======
  select(-country_label,-country_code,-cancer_code, -cancer,  hdi_group,  AD_treat, AD_prev, 
>>>>>>> parent of 6db619e (Misc):AD Preventable and Treatable/1. AD_Modelled_new_eqns.R
         AD_unavoid, total_deaths2, -sex, -age, age_cat,-total_overall)%>%
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  dplyr::mutate(hdi_group=as.numeric(hdi_group))%>%
  ungroup()%>%  dplyr::mutate(AD_treat_prev=AD_treat+AD_prev)%>%
  ungroup()%>%
  select(-hdi_group,-continent,   -AD_sum)%>%
  dplyr::group_by(area)%>%
  dplyr::mutate(AD_prev.asr=sum(AD_prev/py*100000*w), #ASR calculation here
         AD_treat.asr=sum(AD_treat/py*100000*w),
         AD_treat_prev.asr=sum((AD_treat+AD_prev)/py*100000*w),
      #   AD_treat_not_prev.asr=sum(AD_treat_not_prev/py*100000*w),
         AD_unavoid.asr=sum(AD_unavoid/py*100000*w),
         total.deaths.asr=sum(total_deaths2/py*100000*w)) %>%
  dplyr::mutate(total_deaths=sum(total_deaths2,na.rm=T))%>%
  select(-total_deaths2)%>%
  dplyr::mutate(AD_treat_prev=sum(AD_treat,AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_treat=sum(AD_treat,na.rm=T))%>%
 # dplyr::mutate(AD_treat_not_prev=sum(AD_treat_not_prev,na.rm=T))%>%
  dplyr::mutate(AD_prev=sum(AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_unavoid=sum(AD_unavoid,na.rm=T))%>%
  dplyr::mutate(pAD_treat=AD_treat/total_deaths)%>%
  #dplyr::mutate(pAD_treat_not_prev=AD_treat_not_prev/total_deaths)%>%
  dplyr::mutate(pAD_prev=AD_prev/total_deaths)%>%
  dplyr::mutate(pAD_unavoid=AD_unavoid/total_deaths)%>%
  dplyr::mutate(pAD_treat_prev=AD_treat_prev/total_deaths)%>%
  dplyr::mutate(cancer="All Cancer Sites")%>%
  dplyr::mutate(cancer_code=1000)%>%
  select(-py,-w)%>%
  distinct()%>%
  left_join(areas, by=c("area"))%>%
  ungroup()%>%
  distinct()%>%
  left_join(HDI_Region_Mapping, by=c("area", "country_label"))%>%
  as.data.frame()

#region and cancer site

AD_Region_cancer_sites <- Avoidable_Deaths_Simulated_All%>%
  left_join(HDI_Region_Mapping2, by=c("country_code"))%>%
  dplyr::mutate(age_cat="Overall")%>%
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  dplyr::group_by(area, cancer, age,sex)%>%
  dplyr::mutate(py=sum(py))%>%
  ungroup()%>%
<<<<<<< HEAD:AD Preventable and Treatable/1.AD_Modelled_new_eqns.R
  dplyr::select(-country_label,-country_code,-cancer_code, hdi_group,  AD_treat, AD_prev,
=======
  select(-country_label,-country_code,-cancer_code, hdi_group,  AD_treat, AD_prev, 
>>>>>>> parent of 6db619e (Misc):AD Preventable and Treatable/1. AD_Modelled_new_eqns.R
         AD_unavoid, total_deaths2, -sex, -age, age_cat,-total_overall)%>%
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  dplyr::mutate(hdi_group=as.numeric(hdi_group))%>%
  ungroup()%>%  dplyr::mutate(AD_treat_prev=AD_treat+AD_prev)%>%
  ungroup()%>%
  select(-hdi_group,-continent,   -AD_sum)%>%
  dplyr::group_by(area,cancer)%>%
  dplyr::mutate(AD_prev.asr=sum(AD_prev/py*100000*w), #ASR calculation here
         AD_treat.asr=sum(AD_treat/py*100000*w),
         AD_treat_prev.asr=sum((AD_treat+AD_prev)/py*100000*w),
         #AD_treat_not_prev.asr=sum(AD_treat_not_prev/py*100000*w),
         AD_unavoid.asr=sum(AD_unavoid/py*100000*w),
         total.deaths.asr=sum(total_deaths2/py*100000*w)) %>%
  dplyr::mutate(total_deaths=sum(total_deaths2,na.rm=T))%>%
  select(-total_deaths2)%>%
  dplyr::mutate(AD_treat_prev=sum(AD_treat,AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_treat=sum(AD_treat,na.rm=T))%>%
  #dplyr::mutate(AD_treat_not_prev=sum(AD_treat_not_prev,na.rm=T))%>%
  dplyr::mutate(AD_prev=sum(AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_unavoid=sum(AD_unavoid,na.rm=T))%>%
  dplyr::mutate(pAD_treat=AD_treat/total_deaths)%>%
  #dplyr::mutate(pAD_treat_not_prev=AD_treat_not_prev/total_deaths)%>%
  dplyr::mutate(pAD_prev=AD_prev/total_deaths)%>%
  dplyr::mutate(pAD_unavoid=AD_unavoid/total_deaths)%>%
  dplyr::mutate(pAD_treat_prev=AD_treat_prev/total_deaths)%>%
  select(-py,-w)%>%
  distinct()%>%
  left_join(areas, by=c("area"))%>%
  ungroup()%>%
  distinct()%>%
  left_join(HDI_Region_Mapping, by=c("area", "country_label"))%>%
  as.data.frame()%>%
  dplyr::group_by(area)

# age standardizing by region - aggregate by region and then age standardize

countries_regions<-Avoidable_Deaths_Simulated_All%>%
  select(country_code)%>%
  distinct()%>%
  left_join(HDI_Region_Mapping2, by=c("country_code"))


# Table 1 in the manuscript

<<<<<<< HEAD:AD Preventable and Treatable/1.AD_Modelled_new_eqns.R
# Gives us number, percentage of total deaths
table_1_1_without_china2 <- Avoidable_Deaths_Simulated_All %>%
=======
# Gives us number, percentage of total deaths 
table_1_1 <- Avoidable_Deaths_Simulated_All %>%
>>>>>>> parent of 6db619e (Misc):AD Preventable and Treatable/1. AD_Modelled_new_eqns.R
  filter(!country_label%in%c("China","India"))%>%
  select(-country_label, -country_code, -cancer_code, hdi_group)%>%
  dplyr::mutate(age_cat="Overall")%>%
  dplyr::group_by(cancer, age,sex)%>%
  dplyr::mutate(py=sum(py))%>%
  ungroup()%>%
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  dplyr::mutate(hdi_group=as.numeric(hdi_group))%>%
  ungroup()%>%  dplyr::mutate(AD_treat_prev=AD_treat+AD_prev)%>%
  ungroup()%>%
  dplyr::mutate(AD_prev.asr=sum(AD_prev/py*100000*w), #ASR calculation here
         AD_treat.asr=sum(AD_treat/py*100000*w),
         AD_treat_prev.asr=sum((AD_treat + AD_prev)/py*100000*w),
         #AD_treat_not_prev.asr=sum(AD_treat_not_prev/py*100000*w),
         AD_unavoid.asr=sum(AD_unavoid/py*100000*w),
         total.deaths.asr=sum(total_deaths2/py*100000*w)) %>%
  ungroup()%>%
  dplyr::mutate(total_deaths=sum(total_deaths2,na.rm=T))%>%
  dplyr::mutate(AD_treat_prev=sum(AD_treat,AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_treat=sum(AD_treat,na.rm=T))%>%
  #dplyr::mutate(AD_treat_not_prev=sum(AD_treat_not_prev,na.rm=T))%>%
  dplyr::mutate(AD_prev=sum(AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_unavoid=sum(AD_unavoid,na.rm=T))%>%
  dplyr::mutate(pAD_treat=AD_treat/total_deaths)%>%
  #dplyr::mutate(pAD_treat_not_prev=AD_treat_not_prev/total_deaths)%>%
  dplyr::mutate(pAD_prev=AD_prev/total_deaths)%>%
  dplyr::mutate(pAD_unavoid=AD_unavoid/total_deaths)%>%
  dplyr::mutate(pAD_treat_prev=AD_treat_prev/total_deaths)%>%
<<<<<<< HEAD:AD Preventable and Treatable/1.AD_Modelled_new_eqns.R
  dplyr::select(AD_prev, AD_treat,
=======
  select(AD_prev, AD_treat, 
>>>>>>> parent of 6db619e (Misc):AD Preventable and Treatable/1. AD_Modelled_new_eqns.R
        # AD_treat_not_prev,
         AD_treat_prev, AD_unavoid, total_deaths,
         pAD_treat,
       # pAD_treat_not_prev,
        pAD_prev,pAD_treat_prev, pAD_unavoid,
         AD_prev.asr,AD_treat.asr ,
       #AD_treat_not_prev.asr,
         AD_treat_prev.asr, AD_unavoid.asr,total.deaths.asr)%>%
  distinct()


# By Country and all cancer sites

AD_country_all_cancers2<-AD_country_all_cancers%>%  
  ungroup()%>%
  distinct()%>%
  dplyr::mutate(across(6:10,round, -2))%>%
  dplyr::mutate(across(11:15,round, 1))%>%
  dplyr::mutate(across(16:16,round, -2))%>%
  dplyr::mutate(across(17:20, round,3)*100)%>% #dplyr::mutate to show proportion as percentage in export
  select("country_code","country_label",
         "cancer_code", "cancer", 
         "AD_prev",        "pAD_prev",    "AD_prev.asr",
         "AD_treat",       "pAD_treat" ,"AD_treat.asr",
         "AD_treat_prev",  "pAD_treat_prev","AD_treat_prev.asr",
         "AD_unavoid",     "pAD_unavoid" ,   "AD_unavoid.asr",    
         "total_deaths","total.deaths.asr")


<<<<<<< HEAD:AD Preventable and Treatable/1.AD_Modelled_new_eqns.R
# Gives us number, percentage of total deaths
table_1_111 <- Avoidable_Deaths_Simulated_All %>%
  dplyr::select(-country_label, -country_code, -cancer_code, hdi_group)%>%
=======
# Gives us number, percentage of total deaths 
table_1_1 <- Avoidable_Deaths_Simulated_All %>%
  select(-country_label, -country_code, -cancer_code, hdi_group)%>%
>>>>>>> parent of 6db619e (Misc):AD Preventable and Treatable/1. AD_Modelled_new_eqns.R
  dplyr::mutate(age_cat="Overall")%>%
  dplyr::group_by(cancer, age,sex)%>%
  dplyr::mutate(py=sum(py))%>%
  ungroup()%>%
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  dplyr::mutate(hdi_group=as.numeric(hdi_group))%>%
  ungroup()%>%  dplyr::mutate(AD_treat_prev=AD_treat+AD_prev)%>%
  ungroup()%>%
  dplyr::mutate(AD_prev.asr=sum(AD_prev/py*100000*w), #ASR calculation here
         AD_treat.asr=sum(AD_treat/py*100000*w),
         AD_treat_prev.asr=sum((AD_treat+AD_prev)/py*100000*w),
       #  AD_treat_not_prev.asr=sum(AD_treat_not_prev/py*100000*w),
         AD_unavoid.asr=sum(AD_unavoid/py*100000*w),
         total.deaths.asr=sum(total_deaths2/py*100000*w)) %>%
  ungroup()%>%
  dplyr::mutate(total_deaths=sum(total_deaths2,na.rm=T))%>%
  dplyr::mutate(AD_treat_prev=sum(AD_treat,AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_treat=sum(AD_treat,na.rm=T))%>%
  #dplyr::mutate(AD_treat_not_prev=sum(AD_treat_not_prev,na.rm=T))%>%
  dplyr::mutate(AD_prev=sum(AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_unavoid=sum(AD_unavoid,na.rm=T))%>%
  dplyr::mutate(pAD_treat=AD_treat/total_deaths)%>%
 # dplyr::mutate(pAD_treat_not_prev=AD_treat_not_prev/total_deaths)%>%
  dplyr::mutate(pAD_prev=AD_prev/total_deaths)%>%
  dplyr::mutate(pAD_unavoid=AD_unavoid/total_deaths)%>%
  dplyr::mutate(pAD_treat_prev=AD_treat_prev/total_deaths)%>%
<<<<<<< HEAD:AD Preventable and Treatable/1.AD_Modelled_new_eqns.R
  dplyr::select(AD_prev, AD_treat,
=======
  select(AD_prev, AD_treat, 
>>>>>>> parent of 6db619e (Misc):AD Preventable and Treatable/1. AD_Modelled_new_eqns.R
         #AD_treat_not_prev,
         AD_treat_prev, AD_unavoid, total_deaths,
         pAD_treat,
         #pAD_treat_not_prev,
         pAD_prev,pAD_treat_prev, pAD_unavoid,
         AD_prev.asr,AD_treat.asr ,
         #AD_treat_not_prev.asr,
         AD_treat_prev.asr, AD_unavoid.asr,total.deaths.asr)%>%
  distinct()


# By country AND cancer site
# AD by country and cancer site

Avoidable_Deaths_Simulated_All
Avoidable_Deaths_Simulated_All_age_cat
<<<<<<< HEAD:AD Preventable and Treatable/1.AD_Modelled_new_eqns.R
Avoidable_Deaths_Simulated_All_age_cat_overall3<-Avoidable_Deaths_Simulated_All_age_cat%>% #Object to plot overall age standardized on world maps
=======
Avoidable_Deaths_Simulated_All_age_cat_overall<-Avoidable_Deaths_Simulated_All_age_cat%>% #Object to plot overall age standardized on world maps 
>>>>>>> parent of 6db619e (Misc):AD Preventable and Treatable/1. AD_Modelled_new_eqns.R
  as.data.frame()%>%
  dplyr::filter(age_cat=="Overall")%>%
  ungroup()%>%
  dplyr::group_by(country_code,cancer_code)%>%
  dplyr::mutate(total_deaths=sum(total_deaths2,na.rm=T))%>%
  #select(-total_deaths2,-AD_sum)%>%
  dplyr::mutate(AD_treat_prev=sum(AD_treat, AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_treat=sum(AD_treat,na.rm=T))%>%
  #dplyr::mutate(AD_treat_not_prev=sum(AD_treat_not_prev,na.rm=T))%>%
  dplyr::mutate(AD_prev=sum(AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_unavoid=sum(AD_unavoid,na.rm=T))%>%
  dplyr::mutate(pAD_treat=AD_treat/total_deaths)%>%
  #dplyr::mutate(pAD_treat_not_prev=AD_treat_not_prev/total_deaths)%>%
  dplyr::mutate(pAD_prev=AD_prev/total_deaths)%>%
  dplyr::mutate(pAD_unavoid=AD_unavoid/total_deaths)%>%
  dplyr::mutate(pAD_treat_prev=AD_treat_prev/total_deaths)%>%
  as.data.frame()%>%
<<<<<<< HEAD:AD Preventable and Treatable/1.AD_Modelled_new_eqns.R
  mutate(iteration=i)



AD_country_all_cancers2
Avoidable_Deaths_Simulated_All_age_cat_overall3
AD_Region2
table_1_1
AD_by_HDI_all
AD_cancer

#


#Simulated_Data_PAF_All<- read.csv("I:\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Results\\NS_Simulated_All_Countries_intermediate.csv")
# Avoidable_Deaths_Simulated_All2 <- read.csv("I:\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Results\\AD_Simulated_All_Countries_intermediate_inter.csv")
# Avoidable_Deaths_Simulated_All_age_cat <- read.csv("I:\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Results\\AD_Simulated_All_Countries_age_cat_intermediate_inter.csv")
# AD_Region<- read.csv("I:\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Results\\AD_Region_intermediate_inter.csv")
# table_1_11 <- read.csv("I:\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Results\\AD_Total_intermediate_inter.csv")
# AD_by_HDI_all2 <- read.csv("I:\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Results\\AD_HDI_All_Cancers_intermediate_inter.csv")
# AD_country_all_cancers2 <- read.csv("I:\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Results\\AD_Country_All_Cancers_intermediate_inter.csv")
# AD_cancer2 <- read.csv("I:\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Results\\AD_Cancer_by_Site_intermediate_inter.csv")
# Avoidable_Deaths_Simulated_All_age_cat_overall2 <- read.csv("I:\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Results\\AD_country_and_Cancer_by_Site_intermediate_inter.csv")





#function to compute confidence intervals
compute_CIs <- function(data) {
  data <- data %>%
    select(-iteration) %>%
    group_by(1:2, cancer_code, sex, age_cat) %>%
    mutate(
      # Computing CIs
      AD_prev_CI_lower = quantile(AD_prev, c(0.025)),
      AD_prev_CI_upper = quantile(AD_prev, c(0.975)),
      AD_treat_CI_lower = quantile(AD_treat, c(0.025)),
      AD_treat_CI_upper = quantile(AD_treat, c(0.975)),
      AD_treat_prev_CI_lower = quantile(AD_treat_prev, c(0.025)),
      AD_treat_prev_CI_upper = quantile(AD_treat_prev, c(0.975)),
      AD_unavoid_CI_lower = quantile(AD_unavoid, c(0.025)),
      AD_unavoid_CI_upper = quantile(AD_unavoid, c(0.975)),
      total_deaths_CI_lower = quantile(total_deaths, c(0.025)),
      total_deaths_CI_upper = quantile(total_deaths, c(0.975)),
      pAD_prev_CI_lower = quantile(pAD_prev, c(0.025)),
      pAD_prev_CI_upper = quantile(pAD_prev, c(0.975)),
      pAD_treat_CI_lower = quantile(pAD_treat, c(0.025)),
      pAD_treat_CI_upper = quantile(pAD_treat, c(0.975)),
      pAD_treat_prev_CI_lower = quantile(pAD_treat_prev, c(0.025)),
      pAD_treat_prev_CI_upper = quantile(pAD_treat_prev, c(0.975)),
      pAD_unavoid_CI_lower = quantile(pAD_unavoid, c(0.025)),
      pAD_unavoid_CI_upper = quantile(pAD_unavoid, c(0.975)),
      AD_prev.asr_CI_lower = quantile(AD_prev.asr, c(0.025)),
      AD_prev.asr_CI_upper = quantile(AD_prev.asr, c(0.975)),
      AD_treat.asr_CI_lower = quantile(AD_treat.asr, c(0.025)),
      AD_treat.asr_CI_upper = quantile(AD_treat.asr, c(0.975)),
      AD_treat_prev.asr_CI_lower = quantile(AD_treat_prev.asr, c(0.025)),
      AD_treat_prev.asr_CI_upper = quantile(AD_treat_prev.asr, c(0.975)),
      AD_unavoid.asr_CI_lower = quantile(AD_unavoid.asr, c(0.025)),
      AD_unavoid.asr_CI_upper = quantile(AD_unavoid.asr, c(0.975)),
      total.deaths.asr_CI_lower = quantile(total.deaths.asr, c(0.025)),
      total.deaths.asr_CI_upper = quantile(total.deaths.asr, c(0.975)),
      # Computing mean values
      AD_prev = mean(AD_prev),
      AD_treat = mean(AD_treat),
      AD_treat_prev = mean(AD_treat_prev),
      AD_unavoid = mean(AD_unavoid),
      total_deaths = mean(total_deaths),
      pAD_prev = mean(pAD_prev),
      pAD_treat = mean(pAD_treat),
      pAD_treat_prev = mean(pAD_treat_prev),
      pAD_unavoid = mean(pAD_unavoid),
      AD_prev.asr = mean(AD_prev.asr),
      AD_treat.asr = mean(AD_treat.asr),
      AD_treat_prev.asr = mean(AD_treat_prev.asr),
      AD_unavoid.asr = mean(AD_unavoid.asr),
      total.deaths.asr = mean(total.deaths.asr)
    ) %>%
    distinct() %>%
    as.data.frame() %>%
    select(1:2,
      "cancer_code", "cancer", "sex", "age_cat",
      "AD_prev", "AD_prev_CI_lower", "AD_prev_CI_upper",
      "AD_treat", "AD_treat_CI_lower", "AD_treat_CI_upper",
      "AD_treat_prev", "AD_treat_prev_CI_lower", "AD_treat_prev_CI_upper",
      "AD_unavoid", "AD_unavoid_CI_lower", "AD_unavoid_CI_upper",
      "total_deaths", "total_deaths_CI_lower", "total_deaths_CI_upper",
      "pAD_prev", "pAD_prev_CI_lower", "pAD_prev_CI_upper",
      "pAD_treat", "pAD_treat_CI_lower", "pAD_treat_CI_upper",
      "pAD_treat_prev", "pAD_treat_prev_CI_lower", "pAD_treat_prev_CI_upper",
      "pAD_unavoid", "pAD_unavoid_CI_lower", "pAD_unavoid_CI_upper",
      "AD_prev.asr", "AD_prev.asr_CI_lower", "AD_prev.asr_CI_upper",
      "AD_treat.asr", "AD_treat.asr_CI_lower", "AD_treat.asr_CI_upper",
      "AD_treat_prev.asr", "AD_treat_prev.asr_CI_lower", "AD_treat_prev.asr_CI_upper",
      "AD_unavoid.asr", "AD_unavoid.asr_CI_lower", "AD_unavoid.asr_CI_upper",
      "total.deaths.asr", "total.deaths.asr_CI_lower", "total.deaths.asr_CI_upper"
    )

  return(data)
}

# some probs with the variable naming and what is being saved... make sure to check as this needs to be rerun


Avoidable_Deaths_Simulated_All_age_cat_overall3
# data.table::fwrite(Simulated_Data_PAF_All, "I:\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Results\\NS_Simulated_All_Countries_intermediate.csv")
# data.table::fwrite(Avoidable_Deaths_Simulated_All, "I:\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Results\\AD_Simulated_All_Countries_intermediate.csv")
# data.table::fwrite(Avoidable_Deaths_Simulated_All_age_cat, "I:\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Results\\AD_Simulated_All_Countries_age_cat_intermediate.csv")
# data.table::fwrite(AD_Region, "I:\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Results\\AD_Region_intermediate.csv")
# data.table::fwrite(table_1_11, "I:\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Results\\AD_Total_intermediate.csv")
# data.table::fwrite(AD_by_HDI_all2, "I:\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Results\\AD_HDI_All_Cancers_intermediate.csv")
# data.table::fwrite(AD_country_all_cancers2, "I:\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Results\\AD_Country_All_Cancers_intermediate.csv")
# data.table::fwrite(AD_cancer2, "I:\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Results\\AD_Cancer_by_Site_intermediate.csv")
# data.table::fwrite(Avoidable_Deaths_Simulated_All_age_cat_overall3, "I:\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Results\\AD_country_and_Cancer_by_Site_intermediate.csv")
#



Avoidable_Deaths_Simulated_All_age_cat_overall2<-compute_CIs(Avoidable_Deaths_Simulated_All_age_cat_overall2)
AD_country_all_cancers<-compute_CIs(AD_country_all_cancers)
AD_Region2<-compute_CIs(AD_Region2)
table_1_1<-compute_CIs(table_1_1)
AD_by_HDI_all<-compute_CIs(AD_by_HDI_all)
AD_cancer<-compute_CIs(AD_cancer)

# By Country and all cancer sites

AD_country_all_cancers2<-AD_country_all_cancers%>%
  ungroup()%>%
  distinct()%>%
  dplyr::mutate(across(6:10,round, -2))%>%
  dplyr::mutate(across(11:15,round, 1))%>%
  dplyr::mutate(across(16:16,round, -2))%>%
  dplyr::mutate(across(17:20, round,3)*100)%>% #dplyr::mutate to show proportion as percentage in export
  dplyr::select("country_code","country_label",
                "cancer_code", "cancer",
                "AD_prev",        "pAD_prev",    "AD_prev.asr",
                "AD_treat",       "pAD_treat" ,"AD_treat.asr",
                "AD_treat_prev",  "pAD_treat_prev","AD_treat_prev.asr",
                "AD_unavoid",     "pAD_unavoid" ,   "AD_unavoid.asr",
                "total_deaths","total.deaths.asr")


# simplify tables. Needs to be updated the rounding procedures by column as we have CIs

Avoidable_Deaths_Simulated_All_age_cat_overall<-Avoidable_Deaths_Simulated_All_age_cat_overall2%>%
  dplyr::select( "country_code","country_label","cancer_code", "cancer",
                 "AD_prev","pAD_prev",
                 "AD_treat",  "pAD_treat" ,
                 # AD_treat_not_prev, pAD_treat_not_prev,
                 "AD_treat_prev", "pAD_treat_prev",
                 "AD_unavoid",   "pAD_unavoid" ,
                 "total_deaths"
  )%>%
=======
  select( "country_code","country_label","cancer_code", "cancer", 
          "AD_prev","pAD_prev",    
          "AD_treat",  "pAD_treat" ,
         # AD_treat_not_prev, pAD_treat_not_prev,
          "AD_treat_prev", "pAD_treat_prev",
          "AD_unavoid",   "pAD_unavoid" ,        
          "total_deaths"
         )%>%
>>>>>>> parent of 6db619e (Misc):AD Preventable and Treatable/1. AD_Modelled_new_eqns.R
  dplyr::mutate(across(5:5,round,0 ))%>%
  dplyr::mutate(across(6:6, round,4)*100)%>%
  dplyr::mutate(across(7:7,round,0 ))%>%
  dplyr::mutate(across(8:8, round,4)*100)%>%
  dplyr::mutate(across(9:9,round,0 ))%>%
  dplyr::mutate(across(10:10, round,4)*100)%>%
  dplyr::mutate(across(11:11,round,0 ))%>%
  dplyr::mutate(across(12:12, round,4)*100)%>%
  dplyr::mutate(across(13:13,round,0 ))



#By cancer site

AD_cancer2 <- AD_cancer%>%
<<<<<<< HEAD:AD Preventable and Treatable/1.AD_Modelled_new_eqns.R
  dplyr::select( "country_code", "country_label",
                 "cancer_code", "cancer",
                 "AD_prev",
                 "AD_treat",
                 "AD_treat_prev",
                 "AD_unavoid",
                 "total_deaths",
                 "pAD_prev",     "pAD_treat" ,"pAD_treat_prev","pAD_unavoid" ,
                 "AD_prev.asr",
                 "AD_treat.asr",
                 "AD_treat_prev.asr",
                 "AD_unavoid.asr",
                 "total.deaths.asr",
  )%>%
=======
  select( "country_code", "country_label",
          "cancer_code", "cancer", 
          "AD_prev",          
          "AD_treat",   
          "AD_treat_prev",  
          "AD_unavoid",     
          "total_deaths",
          "pAD_prev",     "pAD_treat" ,"pAD_treat_prev","pAD_unavoid" ,     
          "AD_prev.asr",
          "AD_treat.asr",
          "AD_treat_prev.asr",
          "AD_unavoid.asr",  
          "total.deaths.asr",
          )%>%
>>>>>>> parent of 6db619e (Misc):AD Preventable and Treatable/1. AD_Modelled_new_eqns.R
  as.data.frame()%>%
  dplyr::mutate(across(5:9,round, -2))%>%

  dplyr::mutate(across(10:13, round,3)*100)%>%#%>% #dplyr::mutate to show proportion as percentage in export
  dplyr::mutate(across(14:18,round, 1))%>%
<<<<<<< HEAD:AD Preventable and Treatable/1.AD_Modelled_new_eqns.R
  dplyr::select("country_code","country_label",
                "cancer_code", "cancer",
                "AD_prev",        "pAD_prev",    "AD_prev.asr",
                "AD_treat",       "pAD_treat" ,"AD_treat.asr",
                "AD_treat_prev",  "pAD_treat_prev","AD_treat_prev.asr",
                "AD_unavoid",     "pAD_unavoid" ,   "AD_unavoid.asr",
                "total_deaths","total.deaths.asr")
=======
  select("country_code","country_label",
         "cancer_code", "cancer", 
         "AD_prev",        "pAD_prev",    "AD_prev.asr",
         "AD_treat",       "pAD_treat" ,"AD_treat.asr",
         "AD_treat_prev",  "pAD_treat_prev","AD_treat_prev.asr",
         "AD_unavoid",     "pAD_unavoid" ,   "AD_unavoid.asr",    
         "total_deaths","total.deaths.asr")



>>>>>>> parent of 6db619e (Misc):AD Preventable and Treatable/1. AD_Modelled_new_eqns.R




#By region
AD_Region<-AD_Region2%>%
  dplyr::mutate(across(2:4, round, -2))%>%
  dplyr::mutate(across(6:6,round, -2))%>%
  dplyr::mutate(across(7:11,round, 1))%>%
  dplyr::mutate(across(12:12,round, -2))%>%
  dplyr::mutate(across(13:16, round,3)*100)%>% #dplyr::mutate to show proportion as percentage in export
  arrange(continent, country_label)%>%
<<<<<<< HEAD:AD Preventable and Treatable/1.AD_Modelled_new_eqns.R
  dplyr::select("continent","area","country_label","age_cat", "cancer_code", "cancer",
                "AD_prev",        "pAD_prev",    "AD_prev.asr",
                "AD_treat",       "pAD_treat" ,"AD_treat.asr",
                "AD_treat_prev",  "pAD_treat_prev","AD_treat_prev.asr",
                "AD_unavoid",     "pAD_unavoid" ,   "AD_unavoid.asr",
                "total_deaths","total.deaths.asr")
=======
  select("continent","area","country_label","age_cat", "cancer_code", "cancer", 
         "AD_prev",        "pAD_prev",    "AD_prev.asr",
         "AD_treat",       "pAD_treat" ,"AD_treat.asr",
         "AD_treat_prev",  "pAD_treat_prev","AD_treat_prev.asr",
         "AD_unavoid",     "pAD_unavoid" ,   "AD_unavoid.asr",    
         "total_deaths","total.deaths.asr")
>>>>>>> parent of 6db619e (Misc):AD Preventable and Treatable/1. AD_Modelled_new_eqns.R



#World total

table_1_11<-table_1_1%>%
  dplyr::mutate(across(1:5,round, -2))%>%
  dplyr::mutate(across(6:9, round,3)*100)%>% #dplyr::mutate to show proportion as percentage in export
  dplyr::mutate(across(10:13,round, 1))%>%
  dplyr::mutate(across(14:14,round, -2))%>%
  select(
    "AD_prev",        "pAD_prev",    "AD_prev.asr",
    "AD_treat",       "pAD_treat" ,"AD_treat.asr",
    "AD_treat_prev",  "pAD_treat_prev","AD_treat_prev.asr",
    "AD_unavoid",     "pAD_unavoid" ,   "AD_unavoid.asr",
    "total_deaths","total.deaths.asr")


#HDI

AD_by_HDI

AD_by_HDI_all2<-AD_by_HDI_all%>%
  arrange(hdi_group)%>%
  dplyr::mutate(across(2:5, round, -2))%>%
  dplyr::mutate(across(8:13, round, 1))%>%
  dplyr::mutate(across(14:15, round, -2))%>%
  dplyr::mutate(across(16:19, round,3)*100)%>% #dplyr::mutate to show proportion as percentage in export
<<<<<<< HEAD:AD Preventable and Treatable/1.AD_Modelled_new_eqns.R
  dplyr::select( "hdi_group",      "cancer",
                 "AD_prev",        "pAD_prev",    "AD_prev.asr",
                 "AD_treat",       "pAD_treat" ,"AD_treat.asr",
                 "AD_treat_prev",  "pAD_treat_prev","AD_treat_prev.asr",
                 "AD_unavoid",     "pAD_unavoid" ,   "AD_unavoid.asr",
                 "total_deaths","total.deaths.asr")

=======
  select( "hdi_group",      "cancer", 
          "AD_prev",        "pAD_prev",    "AD_prev.asr",
          "AD_treat",       "pAD_treat" ,"AD_treat.asr",
          "AD_treat_prev",  "pAD_treat_prev","AD_treat_prev.asr",
          "AD_unavoid",     "pAD_unavoid" ,   "AD_unavoid.asr",    
          "total_deaths","total.deaths.asr")
  
>>>>>>> parent of 6db619e (Misc):AD Preventable and Treatable/1. AD_Modelled_new_eqns.R


# By risk factor
#
# In a seperate file
#


# Combined table for paper (99%) ready to copy paste


AD_Region
AD_by_HDI
table_1_11
AD_Region
AD_by_HDI_all2
AD_country_all_cancers2
AD_cancer2
table_1_11
Avoidable_Deaths_Simulated_All_age_cat_overall



#writing the files
# data.table::fwrite(Simulated_Data_PAF_All, "I:\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Results\\NS_Simulated_All_Countries.csv")
# data.table::fwrite(Avoidable_Deaths_Simulated_All, "I:\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Results\\AD_Simulated_All_Countries.csv")
# data.table::fwrite(Avoidable_Deaths_Simulated_All_age_cat, "I:\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Results\\AD_Simulated_All_Countries_age_cat.csv")
# data.table::fwrite(AD_Region, "I:\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Results\\AD_Region.csv")
# data.table::fwrite(table_1_11, "I:\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Results\\AD_Total.csv")
# data.table::fwrite(AD_by_HDI_all2, "I:\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Results\\AD_HDI_All_Cancers.csv")
# data.table::fwrite(AD_country_all_cancers2, "I:\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Results\\AD_Country_All_Cancers.csv")
# data.table::fwrite(AD_cancer2, "I:\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Results\\AD_Cancer_by_Site.csv")
# data.table::fwrite(Avoidable_Deaths_Simulated_All_age_cat_overall, "I:\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Results\\AD_country_and_Cancer_by_Site.csv")




#Europe specific analysis
#
# AD_Region<-AD_Region%>%
#   filter(continent==5)
# AD_by_HDI
# table_1_11
# AD_Region
# AD_by_HDI_all2
# AD_country_all_cancers2
# AD_cancer2
# table_1_11
# Avoidable_Deaths_Simulated_All_age_cat_overall


#Summing up the number of cases for the ppt

number_cases<-PAFs_age_Cat %>%
  distinct()%>%
  as.data.frame()%>%
  select(country_label, cancer_label, cases)%>%
  group_by(country_label, cancer_label)%>%
  mutate(cases= sum(cases))%>%
  distinct()

number_cases


