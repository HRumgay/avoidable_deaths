# file to calculate avoidable deaths - HR

# open libraries
library(tidyverse)
library(data.table)


# set working directory
setwd("//Inti/cin/Users/RumgayH/RProjects/RProj_Cdrive/testpackrat/Avoid_deaths")

# import data needed in script ----

# country codes
load("DATA/country_codes.RData")
# Globocan 2020
DATA_GLOB	<- as.data.table(read.csv("//Inti/cin/DataShare/Globocan2020/Globocan.csv"))
DATA_GLOB %>% 
  filter(country_code<900,type==0,cancer_code==39) %>% 
  select(country_code,country_label,age,sex,py) %>% 
  unique() -> pop2020
#import HDI
HDI <-
  read.csv("DATA/hdi_2018.csv") %>% as.data.frame()
HDIm <-
  read.csv("DATA/HDI_Globocan_2018_missing.csv") %>% as.data.frame()
HDIm$hdi_label <- "Low HDI"
HDIm$hdi_label[HDIm$hdi_value>0.550] <- "Medium HDI"
HDIm$hdi_label[HDIm$hdi_value>=0.700] <- "High HDI"
HDIm$hdi_label[HDIm$hdi_value>=0.800] <- "Very high HDI"
HDI %>% 
  bind_rows(HDIm) -> HDI

# PAF
PAFs <- read.csv("DATA/combinedPAFs_cases_12.07.22.csv", stringsAsFactors = FALSE)

#  expected survival
load("RESULTS/ES_dt.RData")
ES<-ES_dt%>%
  filter(time==1000)%>%
  select(-time)%>%
  rename("es"="SurvExp")

# survival estimates
Survival_Modelled <- read.csv("DATA/survival_allsites_allcountries.csv")%>% 
  as.data.frame()%>%
  mutate(rel_surv=case_when(rel_surv>1~ 1,
                            TRUE ~ rel_surv))

# reference survival
Reference_Survival<-read.csv("DATA/Reference_Survival.csv") %>% 
  as.data.frame()%>%
  select( age,
          cancer_code,
          rel_surv)%>%
  dplyr::rename("surv_ref"="rel_surv")%>%
  distinct()%>%
  mutate(surv_ref=case_when(surv_ref>1~ 1,
                            surv_ref<=1~ surv_ref))


# merge datasets ----
Survival_Modelled %>% 
  full_join(Reference_Survival) -> d

# expand age groups in survival datasets
d %>% 
  bind_rows(d %>% 
              filter(age==4) %>% 
              mutate(age=5)) %>% 
  bind_rows(d %>% 
              filter(age==4) %>% 
              mutate(age=6)) %>% 
  bind_rows(d %>% 
              filter(age==4) %>% 
              mutate(age=7)) %>% 
  bind_rows(d %>% 
              filter(age==4) %>% 
              mutate(age=8)) %>% 
  bind_rows(d %>% 
              filter(age==16) %>% 
              mutate(age=17)) %>% 
  bind_rows(d %>% 
              filter(age==16) %>% 
              mutate(age=18)) %>% 
  full_join(ES) %>%      # merge expected survival data
  full_join(PAFs %>%     # merge PAF data
              select(-country_label,-cancer_label, -af.tob:-af.uv) %>% 
              filter(cancer_code!=40, sex!=0, age>3)) %>%  # remove all cancers combine, both sexes and ages <15
  filter(country_code==566) -> d2 # filter to Nigeria


# calculate avoidable deaths ----
d2 %>% 
  filter(cancer_code%in%c(6,7,11,15,20,23,27)) %>% 
  mutate(prevd1 = (1-(rel_surv*es))*cases.prev,   # new formula for prev
         prevd2 = (1-rel_surv)*es*cases.prev,     # previous formula for prev
         treatd = (surv_ref-rel_surv)*es*cases.notprev, # treatable
         treatd2 = (surv_ref-rel_surv)*es*cases,  # treatable of all cases (not just non-prev)
         unavoidd = (1-(surv_ref*es))*cases.notprev,    # unavoidable
         unavoidd2 = (1-(surv_ref*es))*cases,    # unavoidable all cases
         expdsum1 = prevd1+treatd+unavoidd,       # total deaths including new formula for prev
         expdsum2 = prevd2+treatd+unavoidd,       # total deaths including previous formula for prev
         expdsum3 = treatd2+unavoidd2,       # total deaths not including prev
         expd = (1-(rel_surv*es))*cases #expected deaths
  ) %>% 
  group_by(cancer_code,country_code) %>% 
  mutate(prevd1 = sum(prevd1, na.rm=T),
         prevd2 = sum(prevd2, na.rm=T),
         treatd = sum(treatd, na.rm=T),
         treatd2 = sum(treatd2, na.rm=T),
         unavoidd = sum(unavoidd, na.rm=T),
         unavoidd2 = sum(unavoidd2, na.rm=T),
         expdsum1 = sum(expdsum1, na.rm=T),
         expdsum2 = sum(expdsum2, na.rm=T),
         expdsum3 = sum(expdsum3, na.rm=T),
         expd = sum(expd, na.rm=T),
         prop.prevd1 = prevd1/expdsum1*100,
         prop.treat1 = treatd/expdsum1*100,
         prop.prevd2 = prevd2/expdsum2*100,
         prop.treat2 = treatd/expdsum2*100,
         prop.treat3 = treatd2/expdsum3*100) %>% 
  select(-rel_surv,-es,-anchor,-surv_ref,-age,-sex,-cases:-py,-hdi_value,-hdi_rank) %>% unique() -> t2
write.csv(t2,"RESULTS/testformulas_Nigeria.csv",row.names=FALSE)
