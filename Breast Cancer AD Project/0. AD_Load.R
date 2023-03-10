###############################################
#
# Net survival and Avoidable deaths - SURVCAN Breast Cancer
#  Date: 08/03/2023
# Version 2.23
#
#
###############################################

setwd("C:\\Users\\langseliuso\\Documents\\GitHub\\avoidable_deaths\\Breast Cancer AD Project")

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


#test

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
missing_CC<-data.frame(c("Seychelles"), c(690))
colnames(missing_CC) <- c("country_label","country_code")

country_codes <-
  read.csv("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Research visits\\Oliver_Langselius\\Data\\GCO_country_info.csv", stringsAsFactors = FALSE) %>% 
  filter(country_code<900) %>% 
  mutate(country_label = replace(country_label, country_label == "Iran, Islamic Republic of", "Iran")) %>%
  mutate(country_label = replace(country_label, country_label == "Korea, Republic of", "South Korea")) %>%
  mutate(country_label = replace(country_label, country_label == "France, Martinique", "Martinique")) %>%
  select(country_code, country_label)%>% 
  full_join(missing_CC)

#mutate(region = replace(cancer_label, cancer_label == "Colon", "Colorectal")) %>%


#life tables

life_table<-read.csv("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\life_table_SURVCAN.csv")%>%
  mutate(region = replace(region, region == "Cote d'Ivoire", "C?te d'Ivoire")) %>%
  mutate(region = replace(region, region == "France", "Martinique")) %>%
  mutate(region=replace(region,region=="Korea","South Korea"))%>%
  mutate(region=replace(region,region=="South_Africa","South Africa"))%>%
  mutate(region=replace(region,region=="Cote_D`ivoire","Cote d'Ivoire"))%>%
  mutate(region=replace(region,region=="Saudi_Arabia","Saudi Arabia"))%>%
  mutate(region=replace(region,region=="Costa_Rica","Costa Rica"))%>%
  mutate(region=replace(region,region=="Bahain","Bahrain"))%>%
  mutate(region=replace(region,region=="Costa_Rica","Costa Rica"))%>%
  mutate(region=replace(region,region=="Ethiopy","Ethiopia"))%>%
 left_join(country_codes, by = c("region"="country_label", "country_code"))%>%
  dplyr::rename("country"="region")%>%
  select(-country)%>%
  distinct()


PAFs10 <- read.csv("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Research visits\\Oliver_Langselius\\Data\\combinedPAFs_cases_12.07.22.csv")

PAFs<-PAFs10%>%
  as.data.frame()%>%
  mutate(cancer_label=as.character(cancer_label))%>%
  filter(cancer_label=="Breast")%>%
  distinct()%>%
  group_by(country_code, sex, 
           cancer_code, age)%>%
  filter(sex!=0)%>%
  mutate(af.comb= case_when(cases!=0 ~ sum(cases.prev)/sum(cases),
                            cases==0 ~    af.comb))%>%
  ungroup()%>%
  as.data.frame()%>%
  distinct()

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




load("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\ExpectedSurvival.RData")

ES2<-p%>%as.data.frame()

ten_cancer_sites <-
  Cancer_codes %>% 
  filter(cancer_code %in% c(20))#%>%6, 7, 11, 13, 15, 20, 23, 27, 30, 38
# mutate(cancer_label=replace(cancer_label,cancer_label=="Unspecified sites","Colorectal"))

HDI_Region_Mapping<-read.csv("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Research visits\\Oliver_Langselius\\Data\\HDI2018_GLOBOCAN2020.csv")

