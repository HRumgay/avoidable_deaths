library(Rcan)

Avoidable_Deaths_Simulated_All%>%
  group_by(country_code,cancer_code, age, sex)%>%
  left_join(HDI_Region_Mapping2, by=c("country_code"))%>%
  mutate(AD_treat_prev=sum(AD_treat_not_prev+AD_prev,na.rm=T))%>%
  ungroup()%>%
  as.data.frame()%>%
  bind_rows(Avoidable_Deaths_Simulated_All %>% 
              filter(age==4) %>% 
              mutate(age=1,
                     across(AD_prev:Expect_deaths,~0))) %>%
  bind_rows(Avoidable_Deaths_Simulated_All %>% 
              filter(age==4) %>% 
              mutate(age=2,
                     across(AD_prev:Expect_deaths,~0))) %>% 
  bind_rows(Avoidable_Deaths_Simulated_All %>% 
              filter(age==4) %>% 
              mutate(age=3,
                     across(AD_prev:Expect_deaths,~0))) %>% 
  full_join(pop2020 %>% 
              select(-country_label)) -> t3

# age standardized proportions by cancer site, country, sex

asr_countries_by_sex<-t3 %>% 
  csu_asr(
    var_age = "age",
    var_cases = "AD_prev",
    var_py = "py",
    group_by = c("country_code","country_label", "sex", "cancer_code","cancer"),
    first_age = 4,
    #var_age_group = "cancer_label",
    var_asr = "prev.asr"
  )%>% 
  left_join(t3 %>% 
              csu_asr(
                var_age = "age",
                var_cases = "AD_treat",
                var_py = "py",
                group_by = c("country_code","country_label", "sex", "cancer_code","cancer"),
                first_age = 4,
                #var_age_group = "cancer_label",
                var_asr = "treat.asr"
              )) %>%
  left_join(t3 %>% 
              csu_asr(
                var_age = "age",
                var_cases = "AD_treat_prev",
                var_py = "py",
                group_by = c("country_code","country_label", "sex", "cancer_code","cancer"),
                first_age = 4,
                #var_age_group = "cancer_label",
                var_asr = "treat_prev.asr"
              )) %>%
  left_join(t3 %>% 
              csu_asr(
                var_age = "age",
                var_cases = "AD_unavoid",
                var_py = "py",
                group_by = c("country_code","country_label", "sex", "cancer_code","cancer"),
                first_age = 4,
                #var_age_group = "cancer_label",
                var_asr = "unavoid.asr"
              )) %>%
  left_join(t3 %>% 
              csu_asr(
                var_age = "age",
                var_cases = "Expect_deaths",
                var_py = "py",
                group_by = c("country_code","country_label", "sex", "cancer_code","cancer"),
                first_age = 4,
                #var_age_group = "cancer_label",
                var_asr = "expd.asr"
              )) %>% 
  mutate(prop.prev = AD_treat/Expect_deaths,
         prop.treat = AD_prev/Expect_deaths,
         prop.treat_prev = AD_treat_prev/Expect_deaths,
         prop.unavoid =AD_unavoid/Expect_deaths) %>%
  filter(!is.na(cancer))

# age standardized proportions by cancer site, country

asr_world_by_cancer_site<-t3 %>% 
  csu_asr(
    var_age = "age",
    var_cases = "AD_prev",
    var_py = "py",
    group_by = c( "cancer_code","cancer"),
    first_age = 4,
    #var_age_group = "cancer_label",
    var_asr = "prev.asr"
  )%>% 
  
  left_join(t3 %>% 
              csu_asr(
                var_age = "age",
                var_cases = "AD_treat",
                var_py = "py",
                group_by = c( "cancer_code","cancer"),
                first_age = 4,
                #var_age_group = "cancer_label",
                var_asr = "treat.asr"
              )) %>%
  left_join(t3 %>% 
              csu_asr(
                var_age = "age",
                var_cases = "AD_treat_prev",
                var_py = "py",
                group_by = c( "cancer_code","cancer"),
                first_age = 4,
                #var_age_group = "cancer_label",
                var_asr = "treat_prev.asr"
              )) %>%
  left_join(t3 %>% 
              csu_asr(
                var_age = "age",
                var_cases = "AD_unavoid",
                var_py = "py",
                group_by = c(  "cancer_code","cancer"),
                first_age = 4,
                #var_age_group = "cancer_label",
                var_asr = "unavoid.asr"
              )) %>%
  left_join(t3 %>% 
              csu_asr(
                var_age = "age",
                var_cases = "Expect_deaths",
                var_py = "py",
                group_by = c( "cancer_code","cancer"),
                first_age = 4,
                #var_age_group = "cancer_label",
                var_asr = "expd.asr"
              )) %>% 
  mutate(prop.prev = AD_treat/Expect_deaths,
         prop.treat = AD_prev/Expect_deaths,
         prop.treat_prev = AD_treat_prev/Expect_deaths,
         prop.unavoid =AD_unavoid/Expect_deaths) %>%
  filter(!is.na(cancer))

# age standardized proportions global total



asr_world_all_sites<-t3 %>% 
  csu_asr(
    var_age = "age",
    var_cases = "AD_prev",
    var_py = "py",
    group_by = NULL,
    first_age = 4,
    #var_age_group = "cancer_label",
    var_asr = "prev.asr"
  )%>% 
  
  left_join(t3 %>% 
              csu_asr(
                var_age = "age",
                var_cases = "AD_treat",
                var_py = "py",
                group_by = NULL,
                first_age = 4,
                #var_age_group = "cancer_label",
                var_asr = "treat.asr"
              )) %>%
  left_join(t3 %>% 
              csu_asr(
                var_age = "age",
                var_cases = "AD_treat_prev",
                var_py = "py",
                group_by = NULL,
                first_age = 4,
                #var_age_group = "cancer_label",
                var_asr = "treat_prev.asr"
              )) %>%
  left_join(t3 %>% 
              csu_asr(
                var_age = "age",
                var_cases = "AD_unavoid",
                var_py = "py",
                group_by =  NULL,
                first_age = 4,
                #var_age_group = "cancer_label",
                var_asr = "unavoid.asr"
              )) %>%
  left_join(t3 %>% 
              csu_asr(
                var_age = "age",
                var_cases = "Expect_deaths",
                var_py = "py",
                group_by = NULL,
                first_age = 4,
                #var_age_group = "cancer_label",
                var_asr = "expd.asr"
              )) %>% 
  mutate(prop.prev = AD_treat/Expect_deaths,
         prop.treat = AD_prev/Expect_deaths,
         prop.treat_prev = AD_treat_prev/Expect_deaths,
         prop.unavoid =AD_unavoid/Expect_deaths) 




#ASR by hdi group


asr_HDI<-t3 %>% 
  csu_asr(
    var_age = "age",
    var_cases = "AD_prev",
    var_py = "py",
    group_by = c( "hdi_group"),
    first_age = 4,
    #var_age_group = "cancer_label",
    var_asr = "prev.asr"
  )%>% 
  
  left_join(t3 %>% 
              csu_asr(
                var_age = "age",
                var_cases = "AD_treat",
                var_py = "py",
                group_by = c("hdi_group"),
                first_age = 4,
                #var_age_group = "cancer_label",
                var_asr = "treat.asr"
              )) %>%
  left_join(t3 %>% 
              csu_asr(
                var_age = "age",
                var_cases = "AD_treat_prev",
                var_py = "py",
                group_by = c("hdi_group"),
                first_age = 4,
                #var_age_group = "cancer_label",
                var_asr = "treat_prev.asr"
              )) %>%
  left_join(t3 %>% 
              csu_asr(
                var_age = "age",
                var_cases = "AD_unavoid",
                var_py = "py",
                group_by = c("hdi_group"),
                first_age = 4,
                #var_age_group = "cancer_label",
                var_asr = "unavoid.asr"
              )) %>%
  left_join(t3 %>% 
              csu_asr(
                var_age = "age",
                var_cases = "Expect_deaths",
                var_py = "py",
                group_by = c( "hdi_group"),
                first_age = 4,
                #var_age_group = "cancer_label",
                var_asr = "expd.asr"
              )) %>% 
  mutate(prop.prev = AD_treat/Expect_deaths,
         prop.treat = AD_prev/Expect_deaths,
         prop.treat_prev = AD_treat_prev/Expect_deaths,
         prop.unavoid =AD_unavoid/Expect_deaths)%>%
  select(hdi_group,prev.asr, treat.asr, treat_prev.asr, unavoid.asr, expd.asr)
#%>%
 # mutate(across(2:5, round,-2))

#By region

  
HDI_Region_Mapping2 <- HDI_Region_Mapping%>%select(-country_label)%>%
  filter(area<=21)

areas <- HDI_Region_Mapping%>%
  filter(country_code>=910 & country_code<=931 | 
           country_code==905 | country_code==906| 
           country_code==954 | country_code==957 )%>%
  select(area, country_label)


asr_by_region <- t3%>%
  csu_asr(
    var_age = "age",
    var_cases = "AD_prev",
    var_py = "py",
    group_by = c("area"),
    first_age = 4,
    #var_age_group = "cancer_label",
    var_asr = "prev.asr"
  )%>%


  left_join(t3 %>% 
              csu_asr(
                var_age = "age",
                var_cases = "AD_treat",
                var_py = "py",
                group_by = c("area"),
                first_age = 4,
                #var_age_group = "cancer_label",
                var_asr = "treat.asr"
              )) %>%
  left_join(t3 %>% 
              csu_asr(
                var_age = "age",
                var_cases = "AD_treat_prev",
                var_py = "py",
                group_by = c("area"),
                first_age = 4,
                #var_age_group = "cancer_label",
                var_asr = "treat_prev.asr"
              )) %>%
  left_join(t3 %>% 
              csu_asr(
                var_age = "age",
                var_cases = "AD_unavoid",
                var_py = "py",
                group_by = c("area"),
                first_age = 4,
                #var_age_group = "cancer_label",
                var_asr = "unavoid.asr"
              )) %>%
  left_join(t3 %>% 
              csu_asr(
                var_age = "age",
                var_cases = "Expect_deaths",
                var_py = "py",
                group_by = c( "area"),
                first_age = 4,
                #var_age_group = "cancer_label",
                var_asr = "expd.asr"
              )) %>% 
  mutate(prop.prev = AD_treat/Expect_deaths,
         prop.treat = AD_prev/Expect_deaths,
         prop.treat_prev = AD_treat_prev/Expect_deaths,
         prop.unavoid =AD_unavoid/Expect_deaths)%>%
  select(area, 
        prev.asr, treat.asr, treat_prev.asr, unavoid.asr, expd.asr)%>%
  left_join(areas, by=c("area"))

asr_by_region



#left_join(areas)

#calling the results
asr_countries_by_sex
asr_world_by_cancer_site
asr_world_all_sites
asr_HDI
asr_by_region


