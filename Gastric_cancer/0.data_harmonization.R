###############################################sites
#
# Avoidable deaths stomach - File Lead
# Date: 20/08/2026
# Version 1.0
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

setwd("C:\\Users\\langseliuso\\OneDrive - International Agency for Research on Cancer\\Documents\\GitHub\\avoidable_deaths\\Gastric_cancer")


#Globocan_Data_dictionaries

globocan_pop_labels <- read.csv("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\_work\\Oliver_Langselius\\GLOBOCAN_2024\\id_dict.csv")%>%
  as.data.frame()%>%
  dplyr::select(-short_label, -country_iso3)%>%
  arrange(country_code)


globocan_pop_labels_ISO3 <- read.csv("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\_work\\Oliver_Langselius\\GLOBOCAN_2024\\id_dict.csv")%>%
  as.data.frame()%>%
  dplyr::select(-short_label, -country_label)%>%
  arrange(country_code)


globocan_cancer_labels <- read.csv("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\_work\\Oliver_Langselius\\GLOBOCAN_2024\\cancer_dict.csv")%>%
  as.data.frame()%>%
  dplyr::select(-short_label, -icd_label)


# Globocan Incidence data

globocan <- read.csv("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\_work\\Oliver_Langselius\\GLOBOCAN_2024\\data.csv")%>%
  as.data.frame()%>% 
  filter(cancer_code==7,
         type==0, 
         sex!=0)%>%
  dplyr::select(-type)%>%
  left_join(globocan_pop_labels, by=c("country_code"))%>%
  mutate(cancer_label="Stomach")%>%
  filter(country_code<900) # get only country incidence 


#HDI 2022

HDI_2022_raw <-  read.csv("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\_work\\Oliver_Langselius\\AD_PREV_TREAT\\Gastric2024\\Data\\hdi_2022.csv")%>%
  as.data.frame()%>%
  dplyr::select(country_code, value, hdi_group)


HDI_2022 <- HDI_2022_raw %>%
  bind_rows(
    tibble(country_code = c(254, 258, 312, 474, 540, 638),   # French territories
           value        = HDI_2022_raw$value[HDI_2022_raw$country_code == 250],
           hdi_group    = HDI_2022_raw$hdi_group[HDI_2022_raw$country_code == 250]),
    tibble(country_code = c(316, 630),                        # US territories
           value        = HDI_2022_raw$value[HDI_2022_raw$country_code == 840],
           hdi_group    = HDI_2022_raw$hdi_group[HDI_2022_raw$country_code == 840]),
  tibble(country_code = 408,                                # DPR Korea
         value        = NA_real_,
         hdi_group    = 1)
  )%>%
  distinct()


# HDI and region mapping


HDI_Region_Mapping<-read.csv("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\_work\\Oliver_Langselius\\AD_PREV_TREAT\\Data\\HDI2018_GLOBOCAN2020.csv")%>%
  as.data.frame()%>%
  mutate(
    country_code = replace(country_code, country_code == 160, 156), # China
    # country_code = replace(country_code, country_code == 160, 156), # Curacao
    
  )



# country_code continent area country_label

#PAFs - define below

# load infections data set for 2024

#PAF_infections

PAF_infections2 <-  read_xlsx("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\_work\\Oliver_Langselius\\AD_PREV_TREAT\\Gastric2024\\Data\\infectious_related_cancer_cases_in_2024_20260825.xlsx",
                                  sheet="DATA")%>%
  as.data.frame()%>%
  filter(cancer_label%in% c("Non-cardia (C16.1-16.9)", "Cardia (C16.0)", "Stomach"))%>%
  left_join(globocan_pop_labels_ISO3, by=c("country_iso3"))%>%
  dplyr::select(-cancer_label, -country_iso3, -py)%>%
  mutate(cancer_code=7)


HP <- PAF_infections2%>%
  filter(agent_label=="HP")%>%
  rename("cases_hp"="cases_attrib")%>%
  dplyr::select(-agent_label)%>%
  dplyr::group_by(country_code, sex, age, cancer_code)%>%
  dplyr::summarise(cases_hp=sum(cases_hp, na.rm=T), .groups="drop")%>%
  as.data.frame()

EBV <- PAF_infections2%>%
  filter(agent_label=="EBV")%>%
  rename("cases_ebv"="cases_attrib")%>%
  dplyr::select(-agent_label)%>%
  dplyr::group_by(country_code, sex, age, cancer_code)%>%
  dplyr::summarise(cases_ebv=sum(cases_ebv, na.rm=T), .groups="drop")%>%
  as.data.frame()

EBV

PAF_infections <-  HP%>%
  full_join(EBV, by=c("cancer_code", "country_code", "age", "sex"))%>%
  mutate(cases_hp=replace_na(cases_hp, 0),
         cases_ebv=replace_na(cases_ebv, 0))%>%
  left_join(globocan, by=c("cancer_code", "country_code", "age", "sex"))%>%
  mutate(af.hp = case_when (cases >=0.000001 ~ cases_hp/cases, # some zero values causing problems hence this boundary instead of 0 as the cut off
                            cases<0.000001 ~ 0),
         af.ebv = case_when (cases >=0.000001 ~ cases_ebv/cases,
                             cases<0.000001 ~ 0)
  )%>%
  # attributable cases come from the two subsites, cases from the stomach
  # total -- if they disagree the fraction can exceed 1, which would flip
  # the sign of (1-af) in af.comb
  mutate(af.hp = case_when(af.hp>1 ~ 1,
                           TRUE ~ af.hp),
         af.ebv = case_when(af.ebv>1 ~ 1,
                            TRUE ~ af.ebv))%>%
  mutate(af.hp=replace_na(af.hp, 0),
         af.ebv=replace_na(af.ebv, 0))%>%
  dplyr::select(country_code, sex, age, af.hp, af.ebv)%>%
  ungroup()%>%
  distinct()


  PAF_infections
# Load 2022 data (Fink et al 2026)

PAF_obesity_tobacco <-  read.csv("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\_work\\Oliver_Langselius\\AD_PREV_TREAT\\Gastric2024\\Data\\paf_total_2022.csv")%>%
  as.data.frame()%>%
  filter(cancer_main_label=="Stomach")%>%
  select(country_code, country_label, sex, OBS, SMK)%>%
  mutate(cancer_code=7)

#Fix wrong country codes:

PAF_obesity_tobacco <- PAF_obesity_tobacco %>%
  mutate(
    country_code = replace(country_code, country_code == 160, 156), # China
   # country_code = replace(country_code, country_code == 160, 156), # Curacao
    
  )%>%
  select(-country_label)

#Harmonizing the data into one file. joining by country_code, cancer_code, sex and age group



PAFs <- globocan%>%
  # join PAF data
  left_join(PAF_infections, by=c("country_code", "age", "sex"))%>%
  left_join(PAF_obesity_tobacco, by=c("country_code", "sex", "cancer_code"))%>%

  # joining net and expected survival
  # left_join(rel_surv, by=c("country_code", "age"))%>%
  # left_join(ES, by=c("country_code", "sex", "age"))%>%

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
  group_by(country_code, sex,
           cancer_code, age)%>%
  filter(sex!=0)%>%
  # dplyr::mutate(cases=as.numeric(cases),
  #              cases.prev=as.numeric(cases.prev))%>%
  as.data.frame()%>%
  mutate(af.comb=1-(1-OBS)*(1-SMK)*(1-af.hp)*(1-af.ebv), #infections
    cases.prev=cases*af.comb,
         cases.notprev=cases*(1-af.comb))%>%
  
  
  #Arranging data to make easy to read 
  dplyr::select(country_label,country_code, cancer_label, cancer_code, 
                sex, age_cat, age, cases.prev,cases.notprev, cases, py, 
                OBS, SMK, af.comb, af.hp, af.ebv
            #    rel_surv, ES                
            )%>%
  ungroup()%>%
  distinct()%>%
  left_join(HDI_2022, by = c("country_code"))%>%
  filter(country_code!=531) #Curacao needs to get estimates for all things which it currently does not have. 

  
#expected survival 
popmort<-  read.csv("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\_work\\Oliver_Langselius\\AD_PREV_TREAT\\Gastric2024\\Data\\GHE_mortality_2021.csv")%>%
  as.data.frame()%>%
  filter(GHE_cause=="All Causes",
         sex!=0,
        # year==2021
         )%>%
  left_join(HDI_2022, by=c("country_code"))%>%
  as.data.frame()




library(data.table)



# Convert to data.table

setDT(popmort)



# Calculate age-specific mortality rate

popmort[, mx := cases / py]



# Order data

setorder(popmort, country_code, sex, year, age)



# Mortality rate in the next age group

popmort[, mx_next := shift(mx, type = "lead"),
   
   by = .(country_code, sex, year)]



# Expected 5-year survival assuming diagnosis occurs

# in the middle of the age interval

popmort[, prob := exp(-2.5 * mx - 2.5 * mx_next)]



# Oldest age group (no next age group available)

popmort[is.na(mx_next), prob := exp(-5 * mx)]

# Remove temporary variables if desired

popmort[, c("mx", "mx_next") := NULL]

# View result

popmort
  

ES2<-popmort%>%
  as.data.frame()%>%
  rename("ES"="prob")%>%
  dplyr::select(country_code, sex, age, age_label, ES, value)



ES_expanded <- ES2 %>%
  mutate(
    age_new = case_when(
      age == 1 ~ list(c(1)),
      age == 2 ~ list(c(2,3)),                # 5-14 -> 5-9,10-14
      age == 3 ~ list(c(4,5,6)),             # 15-29 -> 15-19,20-24,25-29
      age == 4 ~ list(c(7,8,9,10)),          # 30-49 -> 30-34,35-39,40-44,45-49
      age == 5 ~ list(c(11,12)),             # 50-59 -> 50-54,55-59
      age == 6 ~ list(c(13,14)),             # 60-69 -> 60-64,65-69
      age == 7 ~ list(c(15,16, 17, 18)),             # 70+ -> 70-74,75-79, 80-84, 85+

    )
  ) %>%
  unnest(age_new) %>%
  mutate(age = age_new) %>%
  select(-age_new)

ES_expanded <- ES_expanded %>%
  mutate(
    age_cat = case_when(
      age < 4 ~ "0-15",
      age >= 4 & age < 14 ~ "15-64",
      age >= 14 ~ "65-99"
    )
  )

#--------------------------------------------------
# Add countries not present in mortality file
#--------------------------------------------------

impute_from <- function(df, source_code, target_codes) {
  missing <- setdiff(target_codes, unique(df$country_code))
  if (length(missing) == 0) return(df)
  message("Imputing from ", source_code, ": ",
          paste(missing, collapse = ", "))
  bind_rows(df,
            map_dfr(missing, ~ df %>%
                      filter(country_code == source_code) %>%
                      mutate(country_code = .x)))
}

ES_expanded <- ES_expanded %>%
  impute_from(250, c(254, 258, 312, 474, 540, 638)) %>%   # French territories
  impute_from(840, c(316, 630, 275))                      # US territories


#--------------------------------------------------
# Gaza Strip and West Bank (275)
# Use mean ES from countries with similar HDI
#--------------------------------------------------

ES_expanded <- ES_expanded %>%
  group_by(sex, age) %>%
  mutate(
    ES = ifelse(
      country_code == 275,
      mean(
        ES[
          value >= (0.715 - 0.05) &
            value <= (0.71 + 0.05)
        ],
        na.rm = TRUE
      ),
      ES
    )
  ) %>%
  ungroup()

# Remove HDI if no longer needed
ES_expanded <- ES_expanded %>%
  select(-value)





# Net survival -- Replace with updated file 


Survival_Modelled <-  read_dta("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\_work\\Oliver_Langselius\\AD_PREV_TREAT\\Data\\survival_SURVCAN_anchor_ALL_all34_byHDI.dta")%>%
  as.data.frame()%>%
  mutate(country_name=gsub('`"','', country_name),
         country_name=gsub("'",'', country_name),
         country_name=gsub('"','', country_name)
  )%>%
  clean_names()%>%
  filter(cancer_label=="Stomach")%>%
  dplyr::rename("country_label"="country_name")%>%
  distinct()%>%
  filter(time==5)%>%
  select(-hdi_group)%>%
  mutate(
    country_code = replace(country_code, country_code == 160, 156), # China
    # country_code = replace(country_code, country_code == 160, 156), # Curacao
    
  )
  


# Reference survival
Reference_Survival <- read.csv("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\_work\\Oliver_Langselius\\AD_PREV_TREAT\\Data\\Reference_Survival.csv")%>%
  as.data.frame()%>%
  dplyr::select( age,
                 cancer_code,
                 rel_surv)%>%
  dplyr::rename("surv_ref"="rel_surv")%>%
  distinct()%>%
  mutate(surv_ref=case_when(surv_ref>1~ 1,
                            surv_ref<=1~ surv_ref))








