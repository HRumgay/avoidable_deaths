##########################################################
#
# Reference for all cancer sites by age groups
#
##########################################################
#Combining datasets at the time point of interest 5 years and combining to HDI

hvh_HDI<-Israel%>%filter(time==5)%>% #Upper survival values
  left_join(HDI, by="country_code")%>%
  select(-c(country_label, hdi_rank, year, X))%>%
  filter(hdi_group%in% c(3,4))

lm_HDI<-Thailand%>%filter(time==5)%>% #lower HDI survival values
  left_join(HDI, by="country_code")%>%
  select(-c(country_label, hdi_rank, year,X.1, X))%>%
  filter(hdi_group%in% c(1,2))

# Anchored and combined data set at t=5 with anchored values from Israel and Thailand

popmort<-popmort2%>%
  filter(sex!=0)%>%
  filter(year==2015)%>%
  mutate(mx=1-prob)%>%
  mutate(country_code=as.numeric(country_code))%>%
  group_by(country_code,age,year)%>%
  summarize(mx=sum(cases*mx)/sum(cases), 
            prob=sum(prob*cases)/sum(cases),
            country_label, 
            country_code,
            age_label)%>% #This needs to be adjusted with population weights
  as.data.frame()%>%
  distinct()


countries_5y<-lm_HDI%>% #seems like there is an issue with countries missing. Not sure why
  full_join(hvh_HDI)%>%
  arrange(country_name)%>%
  left_join(popmort,by=c("age"="age","country_code"="country_code"))%>%
  select(-country_label)



Countries_Simulated <-countries_5y%>%
  mutate(
    age_cat = case_when(
      age>=4 & age<14 ~ "15-64",
      age>=14 ~ "65-99",
      FALSE ~"0-15"
    ))%>%
  filter(age_cat!="0-15")%>%
  group_by(country_name,cancer_label,age_cat)%>%
  summarize(country_code,country_name, cancer_code, cancer_label,
            age, age_cat,
            rel_surv, mx)%>%as.data.frame()


Countries_Simulated_Overall<-Countries_Simulated%>%
  mutate(age_cat="Overall")%>%
  group_by(country_name,cancer_label)%>%
  summarize(country_code,country_name, cancer_code, cancer_label,age, 
            age_cat,rel_surv,mx)




R1<-Countries_Simulated%>%
  ungroup()%>%
  filter(age_cat!="Overall")%>%
  left_join(PAFs2,by=c("country_code"="country_code","cancer_code"="cancer_code","age_cat"="age_cat","age"))%>%
  ungroup()%>%
  group_by(cancer_code,age)%>%
  filter(rel_surv==max(rel_surv))%>%
  as.data.frame()%>%
  group_by(cancer_code,age_cat)%>%
  summarize(cancer_code, cancer_label,
            age_cat, age, 
            total_overall=sum(total_overall),
            rel_surv=sum(rel_surv*cases)/sum(cases),
            af.comb=sum(cases.prev)/sum(cases),
            rel_surv,
            Expected_5_year_surv_mx=sum(mx*cases)/sum(cases),
            total_overall)%>%
  droplevels()%>%
  select(-age)%>%
  distinct()%>%
  as.data.frame()

R2<-Countries_Simulated_Overall%>%
  as.data.frame()%>%
  ungroup()%>%
  filter(age_cat=="Overall")%>%
  left_join(PAFs2,by=c("country_code"="country_code","cancer_code"="cancer_code","age_cat"="age_cat","age"))%>%
  ungroup()%>%
  group_by(cancer_code,age)%>%
  filter(rel_surv==max(rel_surv))%>%
  as.data.frame()%>%
  group_by(cancer_code,age_cat)%>%
  summarize(cancer_code, cancer_label,
            age_cat, age, 
            total_overall=sum(total_overall),
            rel_surv=sum(rel_surv*cases)/sum(cases),
            af.comb=sum(cases.prev)/sum(cases),
            rel_surv,
            Expected_5_year_surv_mx=sum(mx*cases)/sum(cases),
            total_overall)%>%
  droplevels()%>%
  select(-age)%>%
  distinct()%>%
  as.data.frame()

Reference_Survival<-R1%>%full_join(R2)



write.csv(Reference_Survival,"~/Documents/R_Projects/Data/Reference_Survival.csv")
