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
PAFs <- read.csv("RESULTS/combinedPAFs_cases_12.07.22.csv", stringsAsFactors = FALSE)

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
  full_join(ES) %>%     # merge expected survival data
  full_join(PAFs %>%     # merge PAF data
              select(-country_label,-cancer_label, -af.tob:-af.uv) %>% 
              filter(cancer_code!=40)) %>% 
  filter(sex!=0, age>3, country_code!=732) -> d2  # remove all cancers combine, both sexes and ages <15


# calculate avoidable deaths ----
d2 %>% 
  mutate(prevd1 = (1-(rel_surv*es))*cases.prev,   # new formula for prev
         prevd2 = (1-rel_surv)*es*cases.prev,     # previous formula for prev
         treatd = (surv_ref-rel_surv)*es*cases.notprev, # treatable
         treatd2 = (surv_ref-rel_surv)*es*cases,  # treatable of all cases (not just non-prev)
         unavoidd = (1-(surv_ref*es))*cases.notprev,    # unavoidable
         unavoidd2 = (1-(surv_ref*es))*cases,    # unavoidable all cases
         expdsum1 = prevd1+treatd+unavoidd,       # total deaths including new formula for prev
         expdsum2 = prevd2+treatd+unavoidd,       # total deaths including previous formula for prev
         expdsum3 = treatd2+unavoidd2,       # total deaths not including prev
         avoid = prevd1+treatd,       # total deaths including new formula for prev
         expd = (1-(rel_surv*es))*cases #expected deaths
  ) -> t2
# add total for both sexes combined
t2 %>% 
  select(-es,-rel_surv,-surv_ref,-af.comb:-cases.notprev,-py) %>% 
  bind_rows(t2 %>% 
              select(-es,-rel_surv,-surv_ref,-af.comb:-cases.notprev,-py) %>% 
              group_by(cancer_code,country_code,age) %>% 
              mutate(prevd1 = sum(prevd1, na.rm=T),
                     prevd2 = sum(prevd2, na.rm=T),
                     treatd = sum(treatd, na.rm=T),
                     treatd2 = sum(treatd2, na.rm=T),
                     unavoidd = sum(unavoidd, na.rm=T),
                     unavoidd2 = sum(unavoidd2, na.rm=T),
                     expdsum1 = sum(expdsum1, na.rm=T),
                     expdsum2 = sum(expdsum2, na.rm=T),
                     expdsum3 = sum(expdsum3, na.rm=T),
                     avoid = sum(avoid, na.rm=T),
                     expd = sum(expd, na.rm=T),
                     cases = sum(cases, na.rm=T),
                     sex=0) %>% unique()) ->t2

# total avoidable deaths per cancer, country and sex
t2 %>% 
  group_by(cancer_code,country_code,sex) %>% 
  mutate(prevd1 = sum(prevd1, na.rm=T),
         prevd2 = sum(prevd2, na.rm=T),
         treatd = sum(treatd, na.rm=T),
         treatd2 = sum(treatd2, na.rm=T),
         unavoidd = sum(unavoidd, na.rm=T),
         unavoidd2 = sum(unavoidd2, na.rm=T),
         expdsum1 = sum(expdsum1, na.rm=T),
         expdsum2 = sum(expdsum2, na.rm=T),
         expdsum3 = sum(expdsum3, na.rm=T),
         avoid = sum(avoid, na.rm=T),
         expd = sum(expd, na.rm=T),
         prop.prevd1 = prevd1/expdsum1*100,
         prop.treat1 = treatd/expdsum1*100,
         prop.prevd2 = prevd2/expdsum2*100,
         prop.treat2 = treatd/expdsum2*100,
         prop.treat3 = treatd2/expdsum3*100) %>% 
  select(-anchor,-age,-cases,-hdi_value,-hdi_rank) %>% 
  select(-prevd2,-expdsum1,-expdsum2,-expdsum3,-prop.prevd1:-prop.treat3) %>% unique() -> t3

# calculate Age-standardised rate of avoidable death ----

# add py for age <15
t2 %>%
  bind_rows(t2 %>% 
              filter(age==4) %>% 
              mutate(age=1,
                     across(prevd1:expd,~0))) %>%
  bind_rows(t2 %>% 
              filter(age==4) %>% 
              mutate(age=2,
                     across(prevd1:expd,~0))) %>% 
  bind_rows(t2 %>% 
              filter(age==4) %>% 
              mutate(age=3,
                     across(prevd1:expd,~0))) %>% 
  full_join(pop2020 %>% 
              select(-country_label)) -> t3

# calculate Age-standardised rate of avoidable death
t3 %>% 
  csu_asr(
    var_age = "age",
    var_cases = "prevd1",
    var_py = "py",
    group_by = c("country_code","country_label", "sex", "cancer_code","cancer_label"),
    first_age = 4,
    #var_age_group = "cancer_label",
    var_asr = "prev.asr"
  ) %>% 
  left_join(t3 %>% 
              csu_asr(
                var_age = "age",
                var_cases = "treatd",
                var_py = "py",
                group_by = c("country_code","country_label", "sex", "cancer_code","cancer_label"),
                first_age = 4,
                #var_age_group = "cancer_label",
                var_asr = "treat.asr"
              )) %>%
  left_join(t3 %>% 
              csu_asr(
                var_age = "age",
                var_cases = "avoid",
                var_py = "py",
                group_by = c("country_code","country_label", "sex", "cancer_code","cancer_label"),
                first_age = 4,
                #var_age_group = "cancer_label",
                var_asr = "avoid.asr"
              )) %>%
  left_join(t3 %>% 
              csu_asr(
                var_age = "age",
                var_cases = "unavoidd",
                var_py = "py",
                group_by = c("country_code","country_label", "sex", "cancer_code","cancer_label"),
                first_age = 4,
                #var_age_group = "cancer_label",
                var_asr = "unavoid.asr"
              )) %>%
  left_join(t3 %>% 
              csu_asr(
                var_age = "age",
                var_cases = "expd",
                var_py = "py",
                group_by = c("country_code","country_label", "sex", "cancer_code","cancer_label"),
                first_age = 4,
                #var_age_group = "cancer_label",
                var_asr = "expd.asr"
              )) %>% 
  mutate(prop.prev = prevd1/expd,
         prop.treat = treatd/expd,
         prop.avoid = avoid/expd,
         prop.unavoid = unavoidd/expd) -> asr

write.csv(asr,"RESULTS/AD_modelled_country.csv",row.names=FALSE)

# calculate world total
t3 %>% 
  csu_asr(
    var_age = "age",
    var_cases = "prevd1",
    var_py = "py",
    group_by = c("sex", "cancer_code","cancer_label"),
    first_age = 4,
    #var_age_group = "cancer_label",
    var_asr = "prev.asr"
  ) %>% 
  left_join(t3 %>% 
              csu_asr(
                var_age = "age",
                var_cases = "treatd",
                var_py = "py",
                group_by = c("sex", "cancer_code","cancer_label"),
                first_age = 4,
                #var_age_group = "cancer_label",
                var_asr = "treat.asr"
              )) %>%
  left_join(t3 %>% 
              csu_asr(
                var_age = "age",
                var_cases = "avoid",
                var_py = "py",
                group_by = c("sex", "cancer_code","cancer_label"),
                first_age = 4,
                #var_age_group = "cancer_label",
                var_asr = "avoid.asr"
              )) %>%
  left_join(t3 %>% 
              csu_asr(
                var_age = "age",
                var_cases = "unavoidd",
                var_py = "py",
                group_by = c("sex", "cancer_code","cancer_label"),
                first_age = 4,
                #var_age_group = "cancer_label",
                var_asr = "unavoid.asr"
              )) %>%
  left_join(t3 %>% 
              csu_asr(
                var_age = "age",
                var_cases = "expd",
                var_py = "py",
                group_by = c( "sex", "cancer_code","cancer_label"),
                first_age = 4,
                #var_age_group = "cancer_label",
                var_asr = "expd.asr"
              )) %>% 
  mutate(prop.prev = prevd1/expd,
         prop.treat = treatd/expd,
         prop.avoid = avoid/expd,
         prop.unavoid = unavoidd/expd) -> asr
write.csv(asr,"RESULTS/AD_modelled_world.csv",row.names=FALSE)



# need to add region grouping to scale estimates up to account for missing countries


