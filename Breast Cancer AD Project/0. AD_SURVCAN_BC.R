###############################################
#
# Net survival and Avoidable deaths
#  Date: 7/2/2022
# Version 2.22
#
# Works for multiple cancer sites currently.
#
#Needs to be synced
#
###############################################


#Avoidable Deaths due to Risk Factors for various Cancer sites
library("survival")
library("mexhaz")
library("tidyverse")

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
missing_CC<-data.frame(c("Seychelles"), c(10001))
colnames(missing_CC) <- c("country_label","country_code")

country_codes <-
  read.csv("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Research visits\\Oliver_Langselius\\Data\\GCO_country_info.csv", stringsAsFactors = FALSE) %>% 
  filter(country_code<900) %>% 
  dplyr::mutate(country_label = replace(country_label, country_label == "Iran, Islamic Republic of", "Iran")) %>%
  dplyr::mutate(country_label = replace(country_label, country_label == "Korea, Republic of", "South Korea")) %>%
  dplyr::mutate(country_label = replace(country_label, country_label == "France, Martinique", "Martinique")) %>%
  select(country_code, country_label)%>% 
  full_join(missing_CC)
  
  #dplyr::mutate(region = replace(cancer_label, cancer_label == "Colon", "Colorectal")) %>%
  

#life tables

life_table<-read.csv("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\life_table_SURVCAN.csv")%>%
  dplyr::mutate(region = replace(region, region == "Cote d'Ivoire", "C?te d'Ivoire")) %>%
  dplyr::mutate(region = replace(region, region == "France", "Martinique")) %>%
  dplyr::mutate(region=replace(region,region=="Korea","South Korea"))%>%
  dplyr::mutate(region=replace(region,region=="South_Africa","South Africa"))%>%
  dplyr::mutate(region=replace(region,region=="Cote_D`ivoire","Cote d'Ivoire"))%>%
  dplyr::mutate(region=replace(region,region=="Saudi_Arabia","Saudi Arabia"))%>%
  dplyr::mutate(region=replace(region,region=="Costa_Rica","Costa Rica"))%>%
  dplyr::mutate(region=replace(region,region=="Bahain","Bahrain"))%>%
  dplyr::mutate(region=replace(region,region=="Costa_Rica","Costa Rica"))%>%
  dplyr::mutate(region=replace(region,region=="Ethiopy","Ethiopia"))%>%
  left_join(country_codes, by = c("region"="country_label"))%>%
  dplyr::rename("country"="region")%>%
  select(-country)
  

PAFs10 <- read.csv("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Research visits\\Oliver_Langselius\\Data\\combinedPAFs_cases_08.06.2022_Prostate.csv")

PAFs<-PAFs10%>%
  dplyr::mutate(cancer_label=as.character(cancer_label))%>%
  dplyr::mutate(cancer_label = replace(cancer_label, cancer_label == "Colon", "Colorectal")) %>%
  dplyr::mutate(cancer_label = replace(cancer_label, cancer_label == "Rectum", "Colorectal")) %>%
  dplyr::mutate(cancer_code  = replace(cancer_code, cancer_code == 8, 38))%>%
  dplyr::mutate(cancer_code  = replace(cancer_code, cancer_code == 9, 38))%>%
  group_by(country_code, sex, 
           cancer_code, age)%>%
  filter(sex!=0)%>%
  dplyr::mutate(af.comb= case_when(cases!=0 ~ sum(cases.prev)/sum(cases),
                            cases==0 ~    af.comb))%>%
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
  dplyr::mutate(MIR=replace(MIR,MIR==Inf, NA))

#same file but Globocan age groups for modeled data 
MIR_Globocan<-read.csv("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Research visits\\Oliver_Langselius\\Data\\MIR.csv")%>%
  as.data.frame()%>%
  select(-mortality,
         -incidence)%>%
  dplyr::mutate(MIR=replace(MIR,MIR==Inf, NA))%>%
  dplyr::mutate(age = case_when(
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
  dplyr::mutate(age_cat = case_when(
    age>=4 & age<14 ~ "15-64",
    age>=14 ~ "65-99",
    age<4 ~"0-15"))%>%
  select(-sex, -X)%>%
  group_by(country_code, cancer_code, age)%>%
  dplyr::mutate(MIR=sum(MIR*py)/sum(py))%>%
  dplyr::mutate(py=sum(py))%>%distinct()%>%
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



ten_cancer_sites <-
  Cancer_codes %>% 
  filter(cancer_code %in% c(20))


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
  filter(surv_ddtot>0)%>%
  filter(include == "Included") %>%
  filter(age >= 15) %>%
  filter(age <= 99) %>%
  filter(!is.na(age)) %>%
  filter(!is.na(surv_ddtot)) %>%
  dplyr::mutate(age_cat = cut(
    age,
    breaks = c(-Inf, 15, 64 , 99),
    labels = c("<15", "15-64", "65-99")
  )) %>% #create age categories (to be adjusted)
  ungroup()%>%
  dplyr::mutate(country=replace(country,country=="Cote d'Ivoire", "C?te d'Ivoire"))%>%
  dplyr::mutate(country=replace(country,country=="France", "Martinique"))%>%
  left_join(country_codes, by = c("country"="country_label"))%>%
  droplevels()%>%
  dplyr::mutate(last_FU_age = round(age + surv_ddtot/365.25)) %>% #creating variable for age of death
  dplyr::mutate(last_FU_year = round(year + surv_ddtot/365.25))%>%  #creating variable for year of death
  dplyr::mutate(sex = replace(sex, sex == "Male", 1)) %>%
  dplyr::mutate(sex = replace(sex, sex == "Female", 2)) %>%
  dplyr::mutate(sex = as.integer(sex)) %>%
  left_join(life_table, by = c(
    "last_FU_age" = "age",
    "last_FU_year" = "year",
    "country_code")) %>%
  droplevels()%>%
  #filter(last_FU_year > 2009 & last_FU_year <= 2014) %>%
 #filter(!is.na(mx)) %>% 
  droplevels() %>%
  left_join(Cancer_codes_Survcan, by = c("cancer" = "cancer"))
#  dplyr::mutate(cancer = replace(cancer, cancer == "Colon (C18)", "Colorectal")) %>%
#  dplyr::mutate(cancer = replace(cancer, cancer == "Rectum (C19-20)", "Colorectal")) %>%
 # filter(cancer_code %in% ten_cancer_sites$cancer_code)
#  filter(is.na(bSURV$mx))

bcan_SURV3 <- bcan_SURV2%>%
  dplyr::mutate(surv_yydd=surv_ddtot/365.25)%>%
  dplyr::mutate(event1=case_when(dead==1 & surv_yydd<=5 ~ 1,
                          dead==1 & surv_yydd>5 ~ 0,
                          dead==0 ~ 0)) %>%
  ungroup()%>%
  group_by(country)%>%
dplyr::mutate(end_FU=max(year))%>%
filter(year>=end_FU-5)%>%
  ungroup()
#      group_by(country,age_cat)%>%
#      dplyr::mutate(max=max(surv_yydd))%>%
#      filter(max>=5)%>%ungroup()


a<-bcan_SURV2%>%select(country,country_code)%>%distinct()%>%as.data.frame()
b<-life_table%>%select(country,country_code)%>%distinct()%>%as.data.frame()



#modifying so last five years of follow up



bcan_SURV11 <- bcan_SURV3%>%
  select(region_lab, country, doi, last_FU_age,age,surv_yytot,year,cancer_code)%>%
  group_by(region_lab, country, cancer_code)%>%
  summarize(end_FU = max(year))%>%
  as.data.frame()


bSURV<-bcan_SURV3%>%left_join(bcan_SURV11)%>%
  ungroup()%>%
  group_by(country)%>%
 filter(year>=end_FU-5)
# 
#  dplyr::mutate(surv_yydd=case_when(surv_yydd<=5 ~ 5,
#                           surv_yydd>5 ~ surv_yydd
 # )
 # )

#bSURV$sex <- as.integer(bSURV$sex)

#survcan_test<-bSURV%>%filter(cancer_code==30)


#age categories
#bSURV_overall <- bSURV %>% dplyr::mutate(age_cat = "Overall")
bSURV_Lower <- bSURV %>% filter(age_cat == "15-64")%>% ungroup()%>%  droplevels()
bSURV_Upper <- bSURV %>% filter(age_cat == "65-99")%>% ungroup()%>%  droplevels()
bSURV_age_cats <- bSURV #bSURV_overall %>% full_join(bSURV)




missingmx <- bSURV %>%filter(is.na(mx)) #60 people have ages above 100 but were between 15-99 years at age of diagnosis. Should they be excluded?

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
bSURV <- bSURV %>% ungroup()%>%  droplevels()%>%arrange(country)

#bSURV%>%select(country,bSURV$country_code)


country_names_Survcan <- bSURV %>% select(country_code, country)%>%distinct()

#   country_names_Survcan<- as_tibble(names(table(bSURV$country)))
# names(country_names_Survcan)[names(country_names_Survcan) == "value"] <- "country"
# country_names_Survcan <- country_names_Survcan%>%#For regression needs to be in this form
# as.data.frame()%>%dplyr::mutate(country=as.character(country))%>%slice(-c(4,13,19,22,31))

country_codes <- as_tibble(names(table(bSURV$country_code))) #Needs to be tibble for predictions
names(country_codes)[names(country_codes) == "value"] <- "country_code"
country_codes <- as.data.frame(country_codes)#%>%slice(-c(4,13,19,22,31)) #For regression needs to be in this form


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
  dplyr::mutate(country_code=as.integer(country_code))%>%
  as_tibble()

cancer_types_tibble <-
  cancer_types %>% as_tibble() #predict only works with tibble data structure...

cancer_codes_tibble <-
  cancer_codes %>% 
  dplyr::mutate(cancer_code=as.integer(cancer_code))%>%
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

Net_Survival_Five_Year_age_1 <- matrix(ncol = 4, nrow = nrow(country_codes)) #R(t)
Net_Survival_Five_Year_age_2 <- matrix(ncol = 4, nrow = nrow(country_codes)) #R(t)
# Net_Survival_Five_Year_age_3 <- matrix(ncol = 5, nrow = nrow(cancer_codes)) #R(t)



All_Cause_Survival_age_1 <- matrix(ncol = 4, nrow = nrow(country_codes))    #S(t)
All_Cause_Survival_age_2 <- matrix(ncol = 4, nrow = nrow(country_codes) )    #S(t)
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

  try(Net_Survival_Five_Year_age_1[i,] <-
      c(country_codes[i,],
      #country_names_Survcan[i,1], 
      s$surv, 
      s$surv.inf, 
      s$surv.sup))
  
  try(Net_Survival_Five_Year_age_2[i,] <-
        c(country_codes[i,],
          #country_names_Survcan[i,1], 
          s2$surv, 
          s2$surv.inf, 
          s2$surv.sup))


  try(All_Cause_Survival_age_1[i,] <-
    c(country_codes[i,],
      #country_names_Survcan[i,], 
      sp$surv, 
      sp$surv.inf, 
      sp$surv.sup))
  try(All_Cause_Survival_age_2[i,] <-
    c(country_codes[i,],
     # country_names_Survcan[i,], 
      sp2$surv, 
      sp2$surv.inf, 
      sp2$surv.sup))
}

#

Age_names_all <- as.data.frame(c("15-64", "65-99")) #, "Overall"

Net_Survival_Five_Year_age_1 <- as.data.frame(Net_Survival_Five_Year_age_1)
Net_Survival_Five_Year_age_2 <- as.data.frame(Net_Survival_Five_Year_age_2)

All_Cause_Survival_age_1 <- as.data.frame(All_Cause_Survival_age_1)
All_Cause_Survival_age_2 <- as.data.frame(All_Cause_Survival_age_2)

colnames(Net_Survival_Five_Year_age_1) <-
  c("country_code",
    #"country",
    "Five_Year_Net_Surv",
    "NS_Lower_CI",
    "NS_Upper_CI")
colnames(Net_Survival_Five_Year_age_2) <-
  c("country_code",
   # "country",
    "Five_Year_Net_Surv",
    "NS_Lower_CI",
    "NS_Upper_CI")

colnames(All_Cause_Survival_age_1) <-
  c("country_code",
    "Five_Year_all_cause_Surv",
    "OS_Lower_CI",
    "OS_Upper_CI")
colnames(All_Cause_Survival_age_2) <-
  c("country_code",
    "Five_Year_all_cause_Surv",
    "OS_Lower_CI",
    "OS_Upper_CI")


Net_Survival_Five_Year_age_1 <-
  Net_Survival_Five_Year_age_1 %>% dplyr::mutate(age_cat = Age_names_all[1,])
Net_Survival_Five_Year_age_2 <-
  Net_Survival_Five_Year_age_2 %>% dplyr::mutate(age_cat = Age_names_all[2,])


All_Cause_Survival_age_1 <-
  All_Cause_Survival_age_1 %>% dplyr::mutate(age_cat = Age_names_all[1,])
All_Cause_Survival_age_2 <-
  All_Cause_Survival_age_2 %>% dplyr::mutate(age_cat = Age_names_all[2,])



Net_Survival_Five_Year <-
  Net_Survival_Five_Year_age_1 %>% 
  full_join(Net_Survival_Five_Year_age_2)%>% 
  dplyr::mutate(country_code=as.numeric(country_code))%>% 
  left_join(country_names_Survcan)

All_Cause_Survival <-
  All_Cause_Survival_age_1 %>% 
  full_join(All_Cause_Survival_age_2) %>% 
  dplyr::mutate(country_code=as.numeric(country_code))%>% 
  left_join(country_names_Survcan)



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
  select(country_code, country,
         Five_Year_all_cause_Surv,OS_Lower_CI,OS_Upper_CI,
         Five_Year_Net_Surv,NS_Lower_CI,NS_Upper_CI,
         age_cat
  ) %>%
  distinct()%>%
  dplyr::mutate(cancer_code=20)

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
  dplyr::mutate(age_cat = case_when(age >= 4 & age < 14 ~ "15-64",
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
#   dplyr::mutate(age_cat = "Overall") %>%
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
  dplyr::mutate(total_age_prev = sum(cases.prev)) %>%
  dplyr::mutate(af.comb.agecat = sum(cases.prev) / sum(cases)) %>%
  dplyr::mutate(ES = sum(ES*cases) / sum(cases)) %>%
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
  dplyr::mutate(cancer=as.character(cancer))%>%
  distinct()

#Format this nicely to match the equations below


# Three AD calcs 

# first need to make sure data is in right format (numeric columns)

# Applying the equation from Rutherford 2015 for AD.

Avoidable_Deaths_Simulated_All2_old<-NS_OS_PAF%>%
  group_by(country_code, cancer_code, age, sex)%>%
  dplyr::mutate(AD_prev=af.comb * total_overall * (1 - rel_surv *  ES ))%>%
  dplyr::mutate(AD_treat=total_overall * (surv_ref-rel_surv) * ES)%>%
  dplyr::mutate(AD_treat_not_prev = (1-af.comb)* total_overall * (surv_ref-rel_surv) * ES)%>%
  dplyr::mutate(AD_unavoid = (1-af.comb)*total_overall*(1-surv_ref*ES))%>%
  dplyr::mutate(Expect_deaths=(1-(rel_surv*ES))*total_overall)%>%
  select("country_code","country_label","age_cat","age","cancer_code","cancer_label",   
         "AD_treat",
         AD_treat_not_prev,
         "AD_prev",
         "AD_unavoid",
         "Expect_deaths",
         "total_overall",
         #  "af.comb",
         "hdi_group")%>%
  dplyr::rename("cancer"="cancer_label")





Avoidable_Deaths_Simulated_All2_old

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
    age>=16 ~ 16,
  ))%>%
  group_by(country_code, age,sex)%>%
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
  age>=16 ~ 16,
))%>%
  dplyr::filter(age>3)%>%
  dplyr::mutate(total=sum(w2))%>%
  group_by(age)%>%
  dplyr::mutate(w=sum(w2)/total)%>%
  select(-w2, -total)%>%
  distinct()


Avoidable_Deaths_Simulated_All_old<- Avoidable_Deaths_Simulated_All2_old%>%
  as.data.frame()%>%
  dplyr::mutate(age=as.numeric(as.character(age)))%>%
  dplyr::mutate(AD_prev=as.numeric(as.character(AD_prev)))%>%
  dplyr::mutate(AD_unavoid=as.numeric(as.character(AD_unavoid)))%>%
  dplyr::mutate(total_overall=as.numeric(as.character(total_overall)))%>%
  dplyr::mutate(AD_treat=as.numeric(as.character(AD_treat)))%>%
  dplyr::mutate(AD_treat_not_prev=as.numeric(as.character(AD_treat_not_prev)))%>%
  dplyr::mutate(AD_sum=AD_prev + AD_unavoid + AD_treat_not_prev)%>%
  dplyr::mutate(cancer_code=as.numeric(cancer_code))%>%
  as.data.frame()%>%distinct()%>%
  dplyr::mutate(country_code=as.integer(country_code))%>%
  left_join(pop20202, c("sex", "country_code", "age"))%>%
  left_join(weights2)%>%
  dplyr::mutate(AD_treat = case_when(AD_treat<0 ~ 0, 
                                     # Numerical calculation error causes the countries which are references  to go slightly negative. 
                                     # By definition this is zero This is rounded to 0
                                     TRUE ~ AD_treat))


## added new ASR code below
Avoidable_Deaths_Simulated_All_overall_old <- Avoidable_Deaths_Simulated_All_old%>%
  dplyr::mutate(age_cat="Overall")%>%
  group_by(country_code, cancer_code, age_cat)%>%
  dplyr::mutate(AD_prev.asr=sum(AD_prev/py*100000*w), #ASR calculation here
                AD_treat.asr=sum(AD_treat/py*100000*w),
                AD_sum.asr=sum(AD_sum/py*100000*w),
                AD_treat_not_prev.asr=sum(AD_treat_not_prev/py*100000*w),
                AD_unavoid.asr=sum(AD_unavoid/py*100000*w),
                Expect_deaths.asr=sum(Expect_deaths/py*100000*w)) %>% 
  select( -total_overall)%>%
  dplyr::mutate(AD_prev = sum(AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_treat = sum(AD_treat,na.rm=T))%>%
  dplyr::mutate(AD_sum = sum(AD_sum,na.rm=T))%>%
  dplyr::mutate(AD_treat_not_prev = sum(AD_treat_not_prev,na.rm=T))%>%
  dplyr::mutate(AD_unavoid = sum(AD_unavoid,na.rm=T))%>%
  dplyr::mutate(Expect_deaths=sum(Expect_deaths,na.rm=T))%>%
  dplyr::mutate(py=sum(py))%>%
  select( -age,-sex)%>%
  ungroup()%>%
  group_by(cancer_code, country_code,age_cat)%>%
  dplyr::mutate(w=sum(w/2))%>%
  distinct()%>%
  as.data.frame()


Avoidable_Deaths_Simulated_All_age_cat_old <- Avoidable_Deaths_Simulated_All_old%>%
  ungroup()%>%
  group_by(country_code, cancer_code, age_cat)%>%
  dplyr::mutate(AD_prev.asr=sum(AD_prev/py*100000*w), #ASR calculation here
                AD_treat.asr=sum(AD_treat/py*100000*w),
                AD_sum.asr=sum(AD_sum/py*100000*w),
                AD_treat_not_prev.asr=sum(AD_treat_not_prev/py*100000*w),
                AD_unavoid.asr=sum(AD_unavoid/py*100000*w),
                Expect_deaths.asr=sum(Expect_deaths/py*100000*w)) %>% 
  select(-total_overall)%>%
  dplyr::mutate(AD_prev = sum(AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_treat = sum(AD_treat,na.rm=T))%>%
  dplyr::mutate(AD_treat_not_prev = sum(AD_treat_not_prev,na.rm=T))%>%
  dplyr::mutate(AD_unavoid = sum(AD_unavoid,na.rm=T))%>%
  dplyr::mutate(Expect_deaths=sum(Expect_deaths,na.rm=T))%>%
  dplyr::mutate(AD_sum = sum(AD_sum,na.rm=T))%>%
  dplyr::mutate(py=sum(py))%>%
  select( -age,-sex)%>%
  ungroup()%>%
  group_by(cancer_code, country_code,age_cat)%>%
  dplyr::mutate(w=sum(w/2))%>%
  distinct()%>%
  full_join(Avoidable_Deaths_Simulated_All_overall_old)%>%
  as.data.frame()%>%ungroup()







# Data by country, HDI, etc

AD_by_HDI_old <- Avoidable_Deaths_Simulated_All_old%>%
  dplyr::mutate(age_cat="Overall")%>%
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  group_by(hdi_group, cancer, age,sex)%>%
  dplyr::mutate(py=sum(py))%>%
  dplyr::mutate(AD_treat_prev=sum(AD_prev+AD_treat_not_prev))%>%
  dplyr::mutate(AD_treat=sum(AD_treat))%>%
  dplyr::mutate(AD_treat_not_prev=sum(AD_treat_not_prev))%>%
  ungroup()%>%
  select(-country_label,-country_code,  hdi_group, cancer, cancer_code, AD_treat, AD_prev, 
         AD_unavoid, AD_sum,-sex, -age, age_cat,-total_overall)%>%
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  dplyr::mutate(hdi_group=as.numeric(hdi_group))%>%
  ungroup()%>%
  group_by(hdi_group,cancer)%>%
  dplyr::mutate(AD_prev.asr=sum(AD_prev/py*100000*w), #ASR calculation here
                AD_treat.asr=sum(AD_treat/py*100000*w),
                AD_sum.asr=sum(AD_sum/py*100000*w),
                AD_treat_prev.asr=sum((AD_treat_not_prev+AD_prev)/py*100000*w),
                AD_treat_not_prev.asr=sum(AD_treat_not_prev/py*100000*w),
                AD_unavoid.asr=sum(AD_unavoid/py*100000*w),
                Expect_deaths.asr=sum(Expect_deaths/py*100000*w)) %>% 
  dplyr::mutate(AD_treat_prev=sum(AD_prev+AD_treat_not_prev))%>%
  dplyr::mutate(Expect_deaths=sum(Expect_deaths))%>%
  dplyr::mutate(AD_treat=sum(AD_treat))%>%
  dplyr::mutate(AD_treat_not_prev=sum(AD_treat_not_prev))%>%
  dplyr::mutate(AD_prev=sum(AD_prev))%>%
  dplyr::mutate(AD_unavoid=sum(AD_unavoid))%>%
  dplyr::mutate(total_deaths=Expect_deaths)%>%
  dplyr::mutate(pAD_treat=AD_treat/total_deaths)%>%
  dplyr::mutate(pAD_treat_not_prev=AD_treat_not_prev/total_deaths)%>%
  dplyr::mutate(pAD_prev=AD_prev/total_deaths)%>%
  dplyr::mutate(pAD_unavoid=AD_unavoid/total_deaths)%>%
  dplyr::mutate(pAD_treat_prev=AD_treat_prev/total_deaths)%>%
  dplyr::mutate(w=sum(w)/n())%>%
  select(-AD_sum,-py)%>%
  ungroup()%>%
  distinct()%>%
  as.data.frame()


AD_by_HDI_old


AD_by_HDI_all_old <-Avoidable_Deaths_Simulated_All_old%>%
  dplyr::mutate(age_cat="Overall")%>%
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  group_by(hdi_group, cancer, age,sex)%>%
  dplyr::mutate(py=sum(py))%>%
  ungroup()%>%
  select(-country_label,-country_code,-cancer_code, -cancer,  hdi_group,  AD_treat, AD_prev, 
         AD_unavoid, Expect_deaths, AD_sum,-sex, -age, age_cat,-total_overall)%>%
  
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  dplyr::mutate(hdi_group=as.numeric(hdi_group))%>%
  ungroup()%>%
  group_by(hdi_group)%>%
  dplyr::mutate(AD_prev.asr=sum(AD_prev/py*100000*w), #ASR calculation here
                AD_treat.asr=sum(AD_treat/py*100000*w),
                AD_treat_prev.asr=sum((AD_treat_not_prev+AD_prev)/py*100000*w),
                AD_sum.asr=sum(AD_sum/py*100000*w),
                AD_treat_not_prev.asr=sum(AD_treat_not_prev/py*100000*w),
                AD_unavoid.asr=sum(AD_unavoid/py*100000*w),
                Expect_deaths.asr=sum(Expect_deaths/py*100000*w)) %>% 
  dplyr::mutate(AD_treat_prev=sum(AD_prev+AD_treat_not_prev))%>%
  dplyr::mutate(Expect_deaths=sum(Expect_deaths))%>%
  dplyr::mutate(AD_treat=sum(AD_treat))%>%
  dplyr::mutate(AD_treat_not_prev=sum(AD_treat_not_prev))%>%
  dplyr::mutate(AD_prev=sum(AD_prev))%>%
  dplyr::mutate(AD_unavoid=sum(AD_unavoid))%>%
  dplyr::mutate(total_deaths=Expect_deaths)%>%
  dplyr::mutate(pAD_treat=AD_treat/total_deaths)%>%
  dplyr::mutate(pAD_treat_not_prev=AD_treat_not_prev/total_deaths)%>%
  dplyr::mutate(pAD_prev=AD_prev/total_deaths)%>%
  dplyr::mutate(pAD_unavoid=AD_unavoid/total_deaths)%>%
  dplyr::mutate(pAD_treat_prev=AD_treat_prev/total_deaths)%>%
  dplyr::mutate(cancer="All Cancer Sites")%>%
  dplyr::mutate(w=sum(w)/n())%>%
  select(-AD_sum,-py)%>%
  
  ungroup()%>%
  distinct()%>%
  as.data.frame()


AD_by_HDI_all_old 
# By country for all cancer sites (number and proportion): 

AD_country_all_cancers_old <-Avoidable_Deaths_Simulated_All_old%>%
  dplyr::mutate(age_cat="Overall")%>%
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  group_by(country_label, cancer, age,sex)%>%
  dplyr::mutate(py=sum(py))%>%
  ungroup()%>%
  select(-age,-sex,-total_overall,-AD_sum)%>% #-af.comb,
  dplyr::mutate(AD_treat_prev=AD_treat+AD_prev)%>%
  ungroup()%>%
  select(  -hdi_group,  )%>%
  group_by(country_code)%>%
  dplyr::mutate(AD_prev.asr=sum(AD_prev/py*100000*w), #ASR calculation here
                AD_treat.asr=sum(AD_treat/py*100000*w),
                AD_treat_prev.asr=sum((AD_treat_not_prev+AD_prev)/py*100000*w),
                AD_treat_not_prev.asr=sum(AD_treat_not_prev/py*100000*w),
                AD_unavoid.asr=sum(AD_unavoid/py*100000*w),
                Expect_deaths.asr=sum(Expect_deaths/py*100000*w)) %>% 
  dplyr::mutate(total_deaths=sum(AD_prev, AD_unavoid, AD_treat_not_prev,na.rm=T))%>%
  dplyr::mutate(Expect_deaths=sum(Expect_deaths,na.rm=T))%>%
  dplyr::mutate(AD_treat_prev=sum(AD_treat_not_prev,AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_treat=sum(AD_treat,na.rm=T))%>%
  dplyr::mutate(AD_treat_not_prev=sum(AD_treat_not_prev,na.rm=T))%>%
  dplyr::mutate(AD_prev=sum(AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_unavoid=sum(AD_unavoid,na.rm=T))%>%
  dplyr::mutate(pAD_treat=AD_treat/total_deaths)%>%
  dplyr::mutate(pAD_treat_not_prev=AD_treat_not_prev/total_deaths)%>%
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

AD_cancer_old <- Avoidable_Deaths_Simulated_All_old%>%
  dplyr::mutate(age_cat="Overall")%>%
  group_by(cancer, age,sex)%>%
  dplyr::mutate(py=sum(py))%>%
  ungroup()%>%
  dplyr::mutate(AD_treat_prev=AD_treat+AD_prev)%>%
  ungroup()%>%
  select(-hdi_group,  -AD_sum,-age,-sex)%>%
  dplyr::mutate(country_code=1000)%>%
  dplyr::mutate(country_label="All Countries")%>%
  group_by(cancer)%>%
  dplyr::mutate(AD_prev.asr=sum(AD_prev/py*100000*w), #ASR calculation here
                AD_treat.asr=sum(AD_treat/py*100000*w),
                AD_treat_prev.asr=sum((AD_treat_not_prev+AD_prev)/py*100000*w),
                AD_treat_not_prev.asr=sum(AD_treat_not_prev/py*100000*w),
                AD_unavoid.asr=sum(AD_unavoid/py*100000*w),
                Expect_deaths.asr=sum(Expect_deaths/py*100000*w)) %>% 
  dplyr::mutate(total_deaths=sum(Expect_deaths,na.rm=T))%>%
  select(-Expect_deaths)%>%
  dplyr::mutate(AD_treat_prev=sum(AD_treat_not_prev,AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_treat=sum(AD_treat,na.rm=T))%>%
  dplyr::mutate(AD_treat_not_prev=sum(AD_treat_not_prev,na.rm=T))%>%
  dplyr::mutate(AD_prev=sum(AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_unavoid=sum(AD_unavoid,na.rm=T))%>%
  dplyr::mutate(pAD_treat=AD_treat/total_deaths)%>%
  dplyr::mutate(pAD_treat_not_prev=AD_treat_not_prev/total_deaths)%>%
  dplyr::mutate(pAD_prev=AD_prev/total_deaths)%>%
  dplyr::mutate(pAD_unavoid=AD_unavoid/total_deaths)%>%
  dplyr::mutate(pAD_treat_prev=AD_treat_prev/total_deaths)%>%
  select(-py,-w,-total_overall)%>%
  distinct()%>%
  ungroup()%>%
  distinct()%>%
  as.data.frame()



# Calculating by region. Need a file that links countries to region 
# HDI_Region_Mapping2 <- HDI_Region_Mapping%>%
#   select(-country_label)%>%
#   dplyr::filter(area<=21)
# 
# areas <- HDI_Region_Mapping%>%
#   dplyr::filter(country_code>=910& country_code<=931 | 
#                   country_code==905 | country_code==906| 
#                   country_code==954| country_code==957 )%>%
#   select(area, country_label)



# By region
AD_Region2_old <- Avoidable_Deaths_Simulated_All_old%>%
  left_join(HDI_Region_Mapping2, by=c("country_code"))%>%
  dplyr::mutate(age_cat="Overall")%>%
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  group_by(area, cancer, age,sex)%>%
  dplyr::mutate(py=sum(py))%>%
  ungroup()%>%
  select(-country_label,-country_code,-cancer_code, -cancer,  hdi_group,  AD_treat, AD_prev, 
         AD_unavoid, Expect_deaths, -sex, -age, age_cat,-total_overall)%>%
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  dplyr::mutate(hdi_group=as.numeric(hdi_group))%>%
  ungroup()%>%  dplyr::mutate(AD_treat_prev=AD_treat+AD_prev)%>%
  ungroup()%>%
  select(-hdi_group,-continent,   -AD_sum)%>%
  group_by(area)%>%
  dplyr::mutate(AD_prev.asr=sum(AD_prev/py*100000*w), #ASR calculation here
                AD_treat.asr=sum(AD_treat/py*100000*w),
                AD_treat_prev.asr=sum((AD_treat_not_prev+AD_prev)/py*100000*w),
                AD_treat_not_prev.asr=sum(AD_treat_not_prev/py*100000*w),
                AD_unavoid.asr=sum(AD_unavoid/py*100000*w),
                Expect_deaths.asr=sum(Expect_deaths/py*100000*w)) %>% 
  dplyr::mutate(total_deaths=sum(Expect_deaths,na.rm=T))%>%
  select(-Expect_deaths)%>%
  dplyr::mutate(AD_treat_prev=sum(AD_treat_not_prev,AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_treat=sum(AD_treat,na.rm=T))%>%
  dplyr::mutate(AD_treat_not_prev=sum(AD_treat_not_prev,na.rm=T))%>%
  dplyr::mutate(AD_prev=sum(AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_unavoid=sum(AD_unavoid,na.rm=T))%>%
  dplyr::mutate(pAD_treat=AD_treat/total_deaths)%>%
  dplyr::mutate(pAD_treat_not_prev=AD_treat_not_prev/total_deaths)%>%
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
AD_Region_cancer_sites_old <- Avoidable_Deaths_Simulated_All_old%>%
  left_join(HDI_Region_Mapping2, by=c("country_code"))%>%
  dplyr::mutate(age_cat="Overall")%>%
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  group_by(area, cancer, age,sex)%>%
  dplyr::mutate(py=sum(py))%>%
  ungroup()%>%
  select(-country_label,-country_code,-cancer_code, hdi_group,  AD_treat, AD_prev, 
         AD_unavoid, Expect_deaths, -sex, -age, age_cat,-total_overall)%>%
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  dplyr::mutate(hdi_group=as.numeric(hdi_group))%>%
  ungroup()%>%  dplyr::mutate(AD_treat_prev=AD_treat+AD_prev)%>%
  ungroup()%>%
  select(-hdi_group,-continent,   -AD_sum)%>%
  group_by(area,cancer)%>%
  dplyr::mutate(AD_prev.asr=sum(AD_prev/py*100000*w), #ASR calculation here
                AD_treat.asr=sum(AD_treat/py*100000*w),
                AD_treat_prev.asr=sum((AD_treat_not_prev+AD_prev)/py*100000*w),
                AD_treat_not_prev.asr=sum(AD_treat_not_prev/py*100000*w),
                AD_unavoid.asr=sum(AD_unavoid/py*100000*w),
                Expect_deaths.asr=sum(Expect_deaths/py*100000*w)) %>% 
  dplyr::mutate(total_deaths=sum(Expect_deaths,na.rm=T))%>%
  select(-Expect_deaths)%>%
  dplyr::mutate(AD_treat_prev=sum(AD_treat_not_prev,AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_treat=sum(AD_treat,na.rm=T))%>%
  dplyr::mutate(AD_treat_not_prev=sum(AD_treat_not_prev,na.rm=T))%>%
  dplyr::mutate(AD_prev=sum(AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_unavoid=sum(AD_unavoid,na.rm=T))%>%
  dplyr::mutate(pAD_treat=AD_treat/total_deaths)%>%
  dplyr::mutate(pAD_treat_not_prev=AD_treat_not_prev/total_deaths)%>%
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
  group_by(area)

# age standardizing by region - aggregate by region and then age standardize

countries_regions_old<-Avoidable_Deaths_Simulated_All_old%>%
  select(country_code)%>%distinct()%>%
  left_join(HDI_Region_Mapping2, by=c("country_code"))


# Table 1 in the manuscript

# Gives us number, percentage of total deaths 
table_1_1_old <- Avoidable_Deaths_Simulated_All_old %>%
  select(-country_label, -country_code, -cancer_code, hdi_group)%>%
  dplyr::mutate(age_cat="Overall")%>%
  group_by(cancer, age,sex)%>%
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
                AD_treat_prev.asr=sum((AD_treat_not_prev+AD_prev)/py*100000*w),
                AD_treat_not_prev.asr=sum(AD_treat_not_prev/py*100000*w),
                AD_unavoid.asr=sum(AD_unavoid/py*100000*w),
                Expect_deaths.asr=sum(Expect_deaths/py*100000*w)) %>% 
  ungroup()%>%
  dplyr::mutate(total_deaths=sum(Expect_deaths,na.rm=T))%>%
  dplyr::mutate(AD_treat_prev=sum(AD_treat_not_prev,AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_treat=sum(AD_treat,na.rm=T))%>%
  dplyr::mutate(AD_treat_not_prev=sum(AD_treat_not_prev,na.rm=T))%>%
  dplyr::mutate(AD_prev=sum(AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_unavoid=sum(AD_unavoid,na.rm=T))%>%
  dplyr::mutate(pAD_treat=AD_treat/total_deaths)%>%
  dplyr::mutate(pAD_treat_not_prev=AD_treat_not_prev/total_deaths)%>%
  dplyr::mutate(pAD_prev=AD_prev/total_deaths)%>%
  dplyr::mutate(pAD_unavoid=AD_unavoid/total_deaths)%>%
  dplyr::mutate(pAD_treat_prev=AD_treat_prev/total_deaths)%>%
  select(AD_prev, AD_treat, 
         AD_treat_not_prev,
         AD_treat_prev, AD_unavoid, total_deaths, 
         pAD_treat, pAD_treat_not_prev,pAD_prev,pAD_treat_prev, pAD_unavoid,
         AD_prev.asr,AD_treat.asr ,AD_treat_not_prev.asr,
         AD_treat_prev.asr, AD_unavoid.asr,Expect_deaths.asr)%>%
  distinct()


# By Country and all cancer sites

AD_country_all_cancers2_old<-AD_country_all_cancers_old%>%  
  ungroup()%>%
  distinct()%>%
  dplyr::mutate(across(6:11,round, -2))%>%
  dplyr::mutate(across(12:17,round, 1))%>%
  dplyr::mutate(across(18:18,round, -2))%>%
  dplyr::mutate(across(19:23, round,3)*100)%>% #dplyr::mutate to show proportion as percentage in export
  select("country_code","country_label",
         "cancer_code", "cancer", 
         "AD_prev",        "pAD_prev",    "AD_prev.asr",
         "AD_treat",       "pAD_treat" ,"AD_treat.asr",
         "AD_treat_prev",  "pAD_treat_prev","AD_treat_prev.asr",
         "AD_unavoid",     "pAD_unavoid" ,   "AD_unavoid.asr",    
         "total_deaths","Expect_deaths.asr")


# Gives us number, percentage of total deaths 
table_1_1_old <- Avoidable_Deaths_Simulated_All_old %>%
  select(-country_label, -country_code, -cancer_code, hdi_group)%>%
  dplyr::mutate(age_cat="Overall")%>%
  group_by(cancer, age,sex)%>%
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
                AD_treat_prev.asr=sum((AD_treat_not_prev+AD_prev)/py*100000*w),
                AD_treat_not_prev.asr=sum(AD_treat_not_prev/py*100000*w),
                AD_unavoid.asr=sum(AD_unavoid/py*100000*w),
                Expect_deaths.asr=sum(Expect_deaths/py*100000*w)) %>% 
  ungroup()%>%
  dplyr::mutate(total_deaths=sum(Expect_deaths,na.rm=T))%>%
  dplyr::mutate(AD_treat_prev=sum(AD_treat_not_prev,AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_treat=sum(AD_treat,na.rm=T))%>%
  dplyr::mutate(AD_treat_not_prev=sum(AD_treat_not_prev,na.rm=T))%>%
  dplyr::mutate(AD_prev=sum(AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_unavoid=sum(AD_unavoid,na.rm=T))%>%
  dplyr::mutate(pAD_treat=AD_treat/total_deaths)%>%
  dplyr::mutate(pAD_treat_not_prev=AD_treat_not_prev/total_deaths)%>%
  dplyr::mutate(pAD_prev=AD_prev/total_deaths)%>%
  dplyr::mutate(pAD_unavoid=AD_unavoid/total_deaths)%>%
  dplyr::mutate(pAD_treat_prev=AD_treat_prev/total_deaths)%>%
  select(AD_prev, AD_treat, 
         AD_treat_not_prev,
         AD_treat_prev, AD_unavoid, total_deaths, 
         pAD_treat, pAD_treat_not_prev,pAD_prev,pAD_treat_prev, pAD_unavoid,
         AD_prev.asr,AD_treat.asr ,AD_treat_not_prev.asr,
         AD_treat_prev.asr, AD_unavoid.asr,Expect_deaths.asr)%>%
  distinct()


# By country AND cancer site
# AD by country and cancer site

Avoidable_Deaths_Simulated_All_old
Avoidable_Deaths_Simulated_All_age_cat_old
Avoidable_Deaths_Simulated_All_age_cat_overall_old<-Avoidable_Deaths_Simulated_All_age_cat_old%>% #Object to plot overall age standardized on world maps 
  as.data.frame()%>%
  dplyr::filter(age_cat=="Overall")%>% 
  group_by(country_code,cancer_code)%>%
  dplyr::mutate(total_deaths=sum(Expect_deaths,na.rm=T))%>%
  select(-Expect_deaths,-AD_sum)%>%
  dplyr::mutate(AD_treat_prev=sum(AD_treat_not_prev, AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_treat=sum(AD_treat,na.rm=T))%>%
  dplyr::mutate(AD_treat_not_prev=sum(AD_treat_not_prev,na.rm=T))%>%
  dplyr::mutate(AD_prev=sum(AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_unavoid=sum(AD_unavoid,na.rm=T))%>%
  dplyr::mutate(pAD_treat=AD_treat/total_deaths)%>%
  dplyr::mutate(pAD_treat_not_prev=AD_treat_not_prev/total_deaths)%>%
  dplyr::mutate(pAD_prev=AD_prev/total_deaths)%>%
  dplyr::mutate(pAD_unavoid=AD_unavoid/total_deaths)%>%
  dplyr::mutate(pAD_treat_prev=AD_treat_prev/total_deaths)%>%
  as.data.frame()%>%
  dplyr::mutate(across(6:10,round,0 ))%>%
  dplyr::mutate(across(13:13,round,0))%>%
  dplyr::mutate(across(14:19, round,4)*100)%>% #dplyr::mutate to show proportion as percentage in export
  select( "country_code","country_label","cancer_code", "cancer", 
          "AD_prev","pAD_prev",    
          "AD_treat",  "pAD_treat" ,
          AD_treat_not_prev, pAD_treat_not_prev,
          "AD_treat_prev", "pAD_treat_prev",
          "AD_unavoid",   "pAD_unavoid" ,        
          "total_deaths"  )



#By cancer site

AD_cancer2_old <- AD_cancer_old%>%
  dplyr::mutate(across(6: 11, round, -2))%>%
  dplyr::mutate(across(12:16, round,4)*100)%>% #dplyr::mutate to show proportion as percentage in export
  select( "country_code", "country_label",
          "cancer_code", "cancer", 
          "AD_prev",        "pAD_prev",    
          "AD_treat",       "pAD_treat" ,
          AD_treat_not_prev, pAD_treat_not_prev,
          "AD_treat_prev",  "pAD_treat_prev",
          "AD_unavoid",     "pAD_unavoid" ,        
          "total_deaths")






#By region
AD_Region_old<-AD_Region2_old%>%
  dplyr::mutate(across(2:5, round, -2))%>%
  dplyr::mutate(across(7:7,round, -2))%>%
  dplyr::mutate(across(8:13,round, 1))%>%
  dplyr::mutate(across(14:14,round, -2))%>%
  dplyr::mutate(across(15:19, round,3)*100)%>% #dplyr::mutate to show proportion as percentage in export
  arrange(continent, country_label)%>%
  select("continent","area","country_label","age_cat", "cancer_code", "cancer", 
         "AD_prev",        "pAD_prev",    "AD_prev.asr",
         "AD_treat",       "pAD_treat" ,"AD_treat.asr",
         "AD_treat_prev",  "pAD_treat_prev","AD_treat_prev.asr",
         "AD_unavoid",     "pAD_unavoid" ,   "AD_unavoid.asr",    
         "total_deaths","Expect_deaths.asr")



#World total

table_1_11_old<-table_1_1_old%>%dplyr::mutate(across(1:6,round, -2))%>%
  dplyr::mutate(across(7:11, round,3)*100)%>% #dplyr::mutate to show proportion as percentage in export
  dplyr::mutate(across(12:17,round, 1))%>%
  select(
    "AD_prev",        "pAD_prev",    "AD_prev.asr",
    "AD_treat",       "pAD_treat" ,"AD_treat.asr",
    "AD_treat_prev",  "pAD_treat_prev","AD_treat_prev.asr",
    "AD_unavoid",     "pAD_unavoid" ,   "AD_unavoid.asr",    
    "total_deaths","Expect_deaths.asr")



#HDI

AD_by_HDI_old

AD_by_HDI_all2<-AD_by_HDI_all_old%>%
  arrange(hdi_group)%>%
  dplyr::mutate(across(2:6, round, -2))%>%
  dplyr::mutate(across(9:14, round, 1))%>%
  dplyr::mutate(across(15:17, round, -2))%>%
  dplyr::mutate(across(18:22, round,3)*100)%>% #dplyr::mutate to show proportion as percentage in export
  select( "hdi_group",      "cancer", 
          "AD_prev",        "pAD_prev",    "AD_prev.asr",
          "AD_treat",       "pAD_treat" ,"AD_treat.asr",
          "AD_treat_prev",  "pAD_treat_prev","AD_treat_prev.asr",
          "AD_unavoid",     "pAD_unavoid" ,   "AD_unavoid.asr",    
          "total_deaths","Expect_deaths.asr")

# By risk factor 
#
# In a seperate file
#


#writing the files
# write.csv(Simulated_Data_PAF_All_old, "~/Documents/R_Projects/Data/NS_Simulated_All_Countries_old.csv")
# write.csv(Avoidable_Deaths_Simulated_All_old, "~/Documents/R_Projects/Data/AD_Simulated_All_Countries_old.csv")
# write.csv(Avoidable_Deaths_Simulated_All_age_cat_old, "~/Documents/R_Projects/Data/AD_Simulated_All_Countries_age_cat_old.csv")
# write.csv(AD_Region_old, "~/Documents/R_Projects/Data/AD_Region_old_old.csv")
# write.csv(table_1_11_old, "~/Documents/R_Projects/Data/AD_Total_old.csv")
# write.csv(AD_by_HDI_all2, "~/Documents/R_Projects/Data/AD_HDI_All_Cancers_old.csv")
# write.csv(AD_country_all_cancers2_old, "~/Documents/R_Projects/Data/AD_Country_All_Cancers_old.csv")
# write.csv(AD_cancer2_old, "~/Documents/R_Projects/Data/AD_Cancer_by_Site.csv")
# write.csv(Avoidable_Deaths_Simulated_All_age_cat_overall_old, "~/Documents/R_Projects/Data/AD_country_and_Cancer_by_Site_old.csv")


AD_Region_old
AD_by_HDI_all2
AD_country_all_cancers2_old
AD_cancer2_old
table_1_11_old
Avoidable_Deaths_Simulated_All_age_cat_overall_old










###############
#
#Exporting Results
#
###############


write.csv(Avoidable_Deaths, "~/Documents/R_Projects/Data/Thai_AD.csv")
write.csv(NS_OS, "~/Documents/R_Projects/Data/Thai_NS_OS.csv")



