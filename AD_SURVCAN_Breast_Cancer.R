###############################################
#
# Net survival and Avoidable deaths
#  Date: 11/5/2022
# Version 2
#
# Works for countries currently.
#
#Needs to be synced
#
###############################################


library(tidyverse)
library(dplyr)
library(mexhaz)
library(readstata13)#To import Stata files
library(janitor)
library(readr)
library(reshape2)
library(tidyverse)
library(janitor)
library(maps)
library(finalfit)
library(survival)
library(mexhaz)
library(fastmexhaz)
library(readxl)

#Avoidable Deaths due to Risk Factors for various Cancer sites
library(readxl)
library(survival)
library(dplyr)
library(tidyverse)
library(stringr)
library(haven)
library(mexhaz)
library(readr)
library(ggplot2)
#library(relsurv)
library(readstata13)#To import Stata files

#Data set imports
popmort3<-list.files('\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Research visits\\Oliver_Langselius\\lifetables\\Expanded_2018', full.names=TRUE)
#GLobocan
g <-read.csv("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Research visits\\Oliver_Langselius\\Globocan2020\\Globocan.csv")%>%as.data.frame()


pops_sorted<-read.csv("C:\\Users\\langseliuso\\OneDrive - IARC\\Desktop\\Avoidable Deaths Breast Cancer Project\\R Analysis\\population2.csv")%>%as.data.frame()
Survcan<- read.csv("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Research visits\\Oliver_Langselius\\SURVCANALL_cc_breast.csv")%>%as.data.frame()
#Loading prevelance project incidence and mortality
prev<-read.dta13("\\\\Inti\\cin\\Studies\\Prevalence\\2020\\_data\\incidence\\prelim\\incident_mortality_asr-2020.dta")%>%as.data.frame()
HDI<-read.csv("\\\\Inti\\cin\\Studies\\Prevalence\\2020\\_data\\hdi\\hdi_2020.csv")%>%as.data.frame()




#type 1 is mortality, 2 prevelance and 0 incidence

globocan_age_Cat <- g %>%
  mutate(age_cat = case_when(age >= 4 & age < 14 ~ "15-64",
                             age >= 14 ~ "65-99",
                             FALSE ~ "0-15")) %>%
  filter(age_cat != "0-15") %>%
  group_by(country_label, cancer_label, age_cat)%>%
  summarize(
    country_code,
    country_label,
    cancer_code,
    cancer_label,
    age,
    age_cat,
    cases,
    py,
    total_overall = sum(cases)
  ) %>% 
  as.data.frame()%>%
  droplevels()


# globocan_Overall <- globocan_age_Cat %>% 
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
#     py,
#     total_overall = sum(cases)) %>% 
#   as.data.frame()


globocan2 <- globocan_age_Cat %>%
 # full_join(globocan_Overall) %>%
  filter(cancer_code==20)%>%
  as.data.frame() %>%
  droplevels() %>%
  group_by(country_label, cancer_label, age_cat) %>%
  mutate(total_overall = sum(cases)) %>%
  as.data.frame() %>%
  summarize(country_code,
            country_label,
            cancer_code,
            cancer_label,
            age_cat,
            total_overall
  ) %>%
  distinct() %>%
  arrange(cancer_label, age_cat)


popmort2<-plyr::ldply(popmort3,read.dta13)%>%
  as.data.frame()%>%
  clean_names()%>%
  filter(!is.na(mx))

Mauritius<-read.dta13(popmort3[22])
Mauritius<-Mauritius%>%
  clean_names()

popmort2<-popmort2%>%full_join(Mauritius)

#Load mortality rates for survival analysis. Correct and use probability
popmort2<-popmort2%>%#filter(sex==2)%>%
  select(region,year,age,mx,prob)%>%
  mutate(region=replace(region,region=="Korea","South Korea"))%>%
  mutate(region=replace(region,region=="South_Africa","South Africa"))%>%
  mutate(region=replace(region,region=="Cote d Ivoire","Cote d'Ivoire"))%>%
  mutate(region=replace(region,region=="Saudi_Arabia","Saudi Arabia"))%>%
  mutate(region=replace(region,region=="Costa_Rica","Costa Rica"))%>%
  mutate(region=replace(region,region=="Bahain","Bahrain"))%>%
  mutate(region=replace(region,region=="Ethiopy","Ethiopia"))


popmort2

#adding the mortality rates of 2015 to the years 2016-2020

popmort_2019<-popmort2%>% filter(year==2015)%>%
  mutate(year=replace(year,year==2015,2019))
popmort_2020<-popmort2%>% filter(year==2015)%>%
  mutate(year=replace(year,year==2015,2020))

popmort2<-popmort2 %>%
  full_join(popmort_2019) %>%
  full_join(popmort_2020) 




# Country codes. Seychelles doesn't have a country code

df1<-data.frame(9999, "Seychelles")
  
names(df1)<-c("country_code","country_label")


country_codes_Survcan<-g%>%
  summarize(country_label,country_code)%>%
  ungroup()%>%droplevels()%>%
  mutate(country_label=as.character(country_label))%>%
  mutate(country_label=replace(country_label,country_label=="United States of America","USA"))%>%
  mutate(country_label=replace(country_label,country_label=="Russian Federation","Russia"))%>%
  mutate(country_label=replace(country_label,country_label=="Democratic People Republic of Congo","Democratic Republic of the Congo"))%>%
  mutate(country_label=replace(country_label,country_label=="Tanzania (United Republic of)","Tanzania"))%>%
  mutate(country_label=replace(country_label,country_label=="Bolivia (Plurinational State of)","Bolivia"))%>%
  mutate(country_label=replace(country_label,country_label=="Congo","Republic of Congo"))%>%
  mutate(country_label=replace(country_label,country_label=="United Kingdom","UK"))%>%
  mutate(country_label=replace(country_label,country_label=="Venezuela (Bolivarian Republic of)","Venezuela"))%>%
  mutate(country_label=replace(country_label,country_label=="Viet Nam","Vietnam"))%>%
  mutate(country_label=replace(country_label,country_label=="Lao People's Democratic Republic","Laos"))%>%
  mutate(country_label=replace(country_label,country_label=="Czechia","Czech Republic"))%>%
  mutate(country_label=replace(country_label,country_label=="Syrian Arab Republic" ,"Syria"))%>%
  mutate(country_label=replace(country_label,country_label== "Lao People Democratic Republic"  ,"Laos"))%>%
  mutate(country_label=replace(country_label,country_label==   "France (metropolitan)" ,"France"))%>%
  mutate(country_label=replace(country_label,country_label== "Islamic Republic of Iran" ,"Iran"))%>%
  mutate(country_label=replace(country_label,country_label== "Republic of Korea","South Korea"))%>%
  mutate(country_label=replace(country_label,country_label=="Cote d Ivoire","Cote d'Ivoire"))%>%
  full_join(df1)%>%
  distinct()





gm<-g%>%summarize(country_label,country_code)%>%distinct()%>%arrange(country_label)

m<-Survcan_Surv%>%summarize(country)%>%distinct()%>%arrange(country)

#Prepping cancer real world data
Survcan2 <- Survcan %>%
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
  mutate(country=str_replace(country,"France","Martinique"))%>%
  mutate(last_FU_age = round(age + surv_dd/365.15)) %>% #creating variable for age of death
  mutate(last_FU_year = round(year + surv_dd/365.15))%>%  #creating variable for year of death
  #mutate(sex = replace(sex, sex == "Male", 1)) %>%
  #mutate(sex = replace(sex, sex == "Female", 2)) %>%
 # mutate(sex = as.integer(sex)) %>%
  left_join(popmort2, by = c(
      "last_FU_age" = "age",
      "last_FU_year" = "year",
      "country" = "region")) %>%
  droplevels()%>%
 # filter(!is.na(mx)) %>% 
  droplevels()%>%
  left_join(country_codes_Survcan, by = c("country" = "country_label"))
  


 Survcan3 <- Survcan2%>%
   mutate(surv_yydd=surv_dd/365.15)%>%
 mutate(event1=case_when(dead==1 &      surv_yydd<=5 ~ 1,
                                          dead==1 & surv_yydd>5 ~ 0,
                                          dead==0 ~ 0
                                          ))

#modifying so last five years of follow up


Survcan11<- Survcan3%>%
  select(region_lab,country,country_code, doi, last_FU_age,age,surv_yytot,year)%>%
  group_by(region_lab, country)%>%
  summarize( end_FU=max(year),start_FU=min(year))%>%
  ungroup()%>%
  as.data.frame()



Survcan_Surv<-Survcan3%>%left_join(Survcan11)%>%
  filter(end_FU-start_FU>=5)

country_codes_Survcan<-Survcan_Surv%>%
  summarize(country,country_code)%>%
  droplevels()%>%
  distinct()

  


#age categories
Surv_overall <- Survcan_Surv %>% mutate(age_cat = "Overall")
Surv_Lower <- Survcan_Surv %>% filter(age_cat == "15-64")
Surv_Upper <- Survcan_Surv %>% filter(age_cat == "65-99")

Survcan_age_cats <- Surv_overall %>% full_join(Survcan_Surv)


is.na(Survcan_Surv$mx) 


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

Survcan_Surv <- Survcan_Surv %>% droplevels()

country_codes <- as_tibble(names(table(Survcan_Surv$country_code))) #Needs to be tibble for predictions

names(country_codes)[names(country_codes) == "value"] <- "country_code"
country_codes <-  as.data.frame(country_codes) #For regression needs to be in this form

country_codes <- as_tibble(names(table(Survcan_Surv$country_code))) #Needs to be tibble for predictions
names(country_codes)[names(country_codes) == "value"] <- "country_code"
country_codes <- as.data.frame(country_codes) #For regression needs to be in this form


#Cubic base model BY COUNTRY


Cubic_age_1 <- list()
Cubic_age_2 <- list()
Cubic_overall <- list()

#Cubic base model BY Cancer type

for (i in 1:nrow(country_codes)) {
  b1 <- Surv_Lower %>% filter(country_code == country_codes[i,])
  b2 <- Surv_Upper %>% filter(country_code == country_codes[i,])
  b3 <- Surv_overall %>% filter(country_code == country_codes[i,])
  

   # k1<-c(1,2.5,4)
   # k2<-c(1,2.5,4)
   # k3<-c(1,2.5,4)
     
  
    k1 <- median(b1$surv_yydd)
    k2 <- median(b2$surv_yydd)
    k3 <- median(b3$surv_yydd)
    
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
        mexhaz(formula = Surv(surv_yydd, event1) ~ 1,
          data = b2,
          base = "exp.ns",
         # degree = 3,
          knots = k2
         # numHess=TRUE,
         # fnoptim="optim"
        ))
  
  try(Cubic_overall[[i]] <-mexhaz(formula = Surv(surv_yydd, event1) ~ 1,
          data = b3,
          base = "exp.ns",
         # degree = 3,
          knots = k3
         # numHess=TRUE,
         # fnoptim="optim"
        ))
}






#Updating the models with mortality rates

#Country updated models list initializing
Cubic_Cancer_age_1 <- list()
Cubic_Cancer_age_2 <- list()
Cubic_Cancer_overall <- list()

#Cubic excess hazard by country
for (i in 1:nrow(country_codes)){
  b1 <- Surv_Lower %>% filter(country_code == country_codes[i,])
  b2 <- Surv_Upper %>% filter(country_code == country_codes[i,])
  b3 <- Surv_overall %>% filter(country_code == country_codes[i,])
  
  try(Cubic_Cancer_age_1[[i]] <-
        update(Cubic_age_1[[i]], expected = "mx",      numHess=TRUE,
               fnoptim="optim"))
  try(Cubic_Cancer_age_2[[i]] <-
        update(Cubic_age_2[[i]], expected = "mx",      numHess=TRUE,
               fnoptim="optim"))
  try(Cubic_Cancer_overall[[i]] <-
        update(Cubic_overall[[i]], expected = "mx",    numHess=TRUE,
               fnoptim="optim"))
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


country_codes_tibble <-
  country_codes %>% 
  mutate(country_code=as.integer(country_code))%>%
  as_tibble() #predict only works with tibble data structure...

#Cubic predictions by country
for (i in 1:nrow(country_codes_tibble)) {
  try(AC1 <-
        predict(Cubic_age_1[[i]], time.pts = time, data.val = country_codes_tibble[i,]))
  try(AC2 <-
        predict(Cubic_age_2[[i]], time.pts = time, data.val = country_codes_tibble[i,]))
  try(AC3 <-
        predict(Cubic_overall[[i]], time.pts = time, data.val = country_codes_tibble[i,]))


  try(HP1 <-
        predict(Cubic_Cancer_age_1[[i]],
                time.pts = time,
                data.val = country_codes_tibble[i,]))
  try(HP2 <-
        predict(Cubic_Cancer_age_2[[i]],
                time.pts = time,
                data.val = country_codes_tibble[i,]))
  try(HP3 <-
        predict(Cubic_Cancer_overall[[i]],
                time.pts = time,
                data.val = country_codes_tibble[i,]))


  Predictions_Cubic_All_Cause_age_1[[i]] <- AC1
  Predictions_Cubic_All_Cause_age_2[[i]] <- AC2
  Predictions_Cubic_All_Cause_age_3[[i]] <- AC3


  Predictions_Cubic_Net_age_1[[i]] <- HP1
  Predictions_Cubic_Net_age_2[[i]] <- HP2
  Predictions_Cubic_Net_age_3[[i]] <- HP3
}


#Extracting prediction data five year avoidable deaths prediction and survival

Net_Survival_Five_Year_age_1 <- matrix(ncol = 5, nrow=nrow(country_codes)) #R(t)
Net_Survival_Five_Year_age_2 <- matrix(ncol = 5, nrow=nrow(country_codes)) #R(t)
Net_Survival_Five_Year_age_3 <- matrix(ncol = 5, nrow=nrow(country_codes)) #R(t)

All_Cause_Survival_age_1 <- matrix(ncol = 5, nrow=nrow(country_codes))    #S(t)
All_Cause_Survival_age_2 <- matrix(ncol = 5, nrow=nrow(country_codes))     #S(t)
All_Cause_Survival_age_3 <- matrix(ncol = 5, nrow=nrow(country_codes))     #S(t)

country_codes_Survcan<-Survcan_Surv%>%
  summarize(country,country_code)%>%
  distinct()%>%
  ungroup()%>%
  droplevels()

for (i in 1:nrow(country_codes_tibble)) {

  s <-  Predictions_Cubic_Net_age_1[[i]]$results
 try( s <-  s %>% filter(time.pts == 5))
  s2 <-  Predictions_Cubic_Net_age_2[[i]]$results
try(s2 <-  s2 %>% filter(time.pts == 5))  
  s3 <-  Predictions_Cubic_Net_age_3[[i]]$results
  try( s3 <-  s3 %>% filter(time.pts == 5))

  
  sp <- Predictions_Cubic_All_Cause_age_1[[i]]$results
  try( sp <-  sp %>% filter(time.pts == 5))
  sp2 <- Predictions_Cubic_All_Cause_age_2[[i]]$results
  try(sp2 <-  sp2 %>% filter(time.pts == 5))
  sp3 <- Predictions_Cubic_All_Cause_age_3[[i]]$results
  try(sp3 <-  sp3 %>% filter(time.pts == 5))
  
  

  
  
  Net_Survival_Five_Year_age_1[i,] <-
    c(country_codes_Survcan[i,1],
      country_codes_Survcan[i,2], 
      s$surv, s$surv.inf, s$surv.sup)
  Net_Survival_Five_Year_age_2[i,] <-
    c(country_codes_Survcan[i,1],
      country_codes_Survcan[i,2],
      s2$surv,
      s2$surv.inf,
      s2$surv.sup)
  Net_Survival_Five_Year_age_3[i,] <-
    c(country_codes_Survcan[i,1],
      country_codes_Survcan[i,2],
      s3$surv,
      s3$surv.inf,
      s3$surv.sup)
  
  All_Cause_Survival_age_1[i,] <-
    c(country_codes_Survcan[i,1],
      country_codes_Survcan[i,2],
      sp$surv,
      sp$surv.inf,
      sp$surv.sup)
  All_Cause_Survival_age_2[i,] <-
    c(country_codes_Survcan[i,1],
      country_codes_Survcan[i,2],
      sp2$surv,
      sp2$surv.inf,
      sp2$surv.sup)
  All_Cause_Survival_age_3[i,] <-
    c(country_codes_Survcan[i,1],
      country_codes_Survcan[i,2],
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
  c("c",
    "country_code",
    "Five_Year_Net_Surv",
    "NS_Lower_CI",
    "NS_Upper_CI")
colnames(Net_Survival_Five_Year_age_2) <-
  c("country_code",
    "country_code",
    "Five_Year_Net_Surv",
    "NS_Lower_CI",
    "NS_Upper_CI")
colnames(Net_Survival_Five_Year_age_3) <-
  c("country_code",
    "country_code",
    "Five_Year_Net_Surv",
    "NS_Lower_CI",
    "NS_Upper_CI")

colnames(All_Cause_Survival_age_1) <-
  c("country_code",
    "country_code",
    "Five_Year_all_cause_Surv",
    "OS_Lower_CI",
    "OS_Upper_CI")
colnames(All_Cause_Survival_age_2) <-
  c("country_code",
    "country_code",
    "Five_Year_all_cause_Surv",
    "OS_Lower_CI",
    "OS_Upper_CI")
colnames(All_Cause_Survival_age_3) <-
  c("country_code",
    "country_code",
    "Five_Year_all_cause_Surv",
    "OS_Lower_CI",
    "OS_Upper_CI")



Net_Survival_Five_Year_age_1 <-
  Net_Survival_Five_Year_age_1 %>% mutate(age_cat = Age_names_all[1,])
Net_Survival_Five_Year_age_2 <-
  Net_Survival_Five_Year_age_2 %>% mutate(age_cat = Age_names_all[2,])
Net_Survival_Five_Year_age_3 <-
  Net_Survival_Five_Year_age_3 %>% mutate(age_cat = Age_names_all[3,])

All_Cause_Survival_age_1 <-
  All_Cause_Survival_age_1 %>% mutate(age_cat = Age_names_all[1,])
All_Cause_Survival_age_2 <-
  All_Cause_Survival_age_2 %>% mutate(age_cat = Age_names_all[2,])
All_Cause_Survival_age_3 <-
  All_Cause_Survival_age_3 %>% mutate(age_cat = Age_names_all[3,])


Net_Survival_Five_Year <-
  Net_Survival_Five_Year_age_1 %>% full_join(Net_Survival_Five_Year_age_2) %>%
  full_join(Net_Survival_Five_Year_age_3)

All_Cause_Survival <-
  All_Cause_Survival_age_1 %>% full_join(All_Cause_Survival_age_2) %>%
  full_join(All_Cause_Survival_age_3)



NS_OS2 <-Net_Survival_Five_Year %>% left_join(
    All_Cause_Survival,
    by = c(
      "country_code" = "country_code",
      "age_cat" = "age_cat"
    )
  )


NS_OS2$country_code <- as.numeric(as.character(NS_OS2$country_code))

Survcan_Surv$country_code <-
  as.numeric(as.character(Survcan_Surv$country_code))




NS_OS <-
  NS_OS2 %>% left_join(
    Survcan_age_cats,
    by = c(
      "country_code" = "country_code",
      "age_cat" = "age_cat"
    )
  ) %>%
  select(cancer,country_code,
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
NS_OS$age_cat <- as.factor(NS_OS$age_cat)


#########################
#
# AD Calculations
#
#########################

#Prepping PAFs data processing and combining by age group




NS_OS2 <- NS_OS %>% 
  left_join(globocan2, by = c("country_code" = "country_code", "age_cat" ="age_cat")) %>% 
  droplevels()


#Three AD calculations


#Applying the equation from Rutherford 2015 for AD. Needs to be updated to have scaled relative survival
Avoidable_Deaths <- matrix(ncol = 7, nrow = nrow(NS_OS2)) #AD(t)
NS_OS2$cancer <- as.character(NS_OS2$cancer)
NS_Ref<-0.9 #Reference countries survival. Decide on for each data site. Use simulated highest survival for each cancer site

for (i in 1:nrow(NS_OS2)) {
  Expected_5_year_surv_mx <-
    (NS_OS2[i,]$Five_Year_all_cause_Surv) / (NS_OS2[i,]$Five_Year_Net_Surv) #Calculate expected survival from mortality rates which is all cause surv/ net survival
 
  # #Avoidable deaths (treatable: #check what the lower CI is called in the previous data frame
  
  AD_treat <-
    (NS_Ref - NS_OS2[i,]$Five_Year_Net_Surv) * Expected_5_year_surv_mx  * NS_OS2[i,]$total_overall
  AD_treat_Lower <-
    (NS_Ref - NS_OS2[i,]$NS_Lower_CI) * Expected_5_year_surv_mx *
    NS_OS2[i,]$total_overall
  AD_treat_Upper <-
    (NS_Ref - NS_OS2[i,]$NS_Upper_CI) * Expected_5_year_surv_mx * 
    NS_OS2[i,]$total_overall
  
  Avoidable_Deaths[i,] <- c(NS_OS2[i,]$age_cat,
    NS_OS2[i,]$country_code,
    NS_OS2[i,]$country_label,
    AD_treat,
    AD_treat_Lower,
    AD_treat_Upper,
    # AD_prev,
    # AD_prev_Lower,
    # AD_prev_Upper,
    # AD_unavoid,
    # AD_unavoid_Lower,
    # AD_unavoid_Upper,
    NS_OS2[i,]$total_overall
  )
}

Avoidable_Deaths <- Avoidable_Deaths %>% as.data.frame()

colnames(Avoidable_Deaths) <-
  c("age_cat",
    "country_code",
    "country_label",
    "AD_treat",
    "AD_treat_Lower",
    "AD_treat_Upper",
    # "AD_prev",
    # "AD_prev_Lower",
    # "AD_prev_Upper",
    # "AD_unavoid",
    # "AD_unavoid_Lower",
    # "AD_unavoid_Upper",
    "total"
    )

Avoidable_Deaths<-Avoidable_Deaths%>%
  mutate(AD_treat=as.numeric(as.character(AD_treat)))%>%
#  mutate(AD_prev=as.numeric(as.character(AD_prev)))%>%
 # mutate(AD_unavoid=as.numeric(as.character(AD_unavoid)))%>%
 # mutate(AD_treatprev=AD_treat+AD_prev+AD_unavoid)%>%
  mutate(country_code=as.numeric(country_code))

NS_OS2


#write.csv(Avoidable_Deaths, "~/Documents/R_Projects/Data/Thai_AD.csv")
#write.csv(NS_OS, "~/Documents/R_Projects/Data/Thai_NS_OS.csv")




