##########################################################
#
# Reference for all cancer sites by age groups
#
##########################################################
#Combining datasets at the time point of interest 5 years and combining to HDI

setwd("C:\\Users\\langseliuso\\Documents\\GitHub\\avoidable_deaths\\AD Preventable and Treatable")

#updated file 

Survival_Modelled

# Anchored and combined data set at t=5 with anchored values from Israel and Thailand

# popmort<-popmort2%>%
#   filter(sex!=0)%>%
#   filter(year==2015)%>%
#   mutate(mx=1-prob)%>%
#   mutate(country_code=as.numeric(country_code))%>%
#   group_by(country_code,age,year)%>%
#   summarize(mx=sum(cases*mx)/sum(cases), 
#             prob=sum(prob*cases)/sum(cases),
#             country_label, 
#             country_code,
#             age_label)%>% #This needs to be adjusted with population weights
#   as.data.frame()%>%
#   distinct()


countries_5y<-Survival_Modelled%>%
  arrange(country_label)
 # left_join(popmort,by=c("age"="age","country_code"="country_code","country_label"="country_label" ))#%>%
#  select(-country_label)



Countries_Simulated <-countries_5y%>%
  mutate(age_cat = case_when(
      age>=4 & age<14 ~ "15-64",
      age>=14 ~ "65-99",
      FALSE ~"0-15"
    ))%>%
  filter(age_cat!="0-15")%>%
  group_by(country_label,cancer_label,age_cat)%>%
  summarize(country_code,country_label, cancer_code, cancer_label,
            age, age_cat,
            rel_surv)%>%
  as.data.frame()



# Countries_Simulated_Overall<-Countries_Simulated%>%
#   mutate(age_cat="Overall")%>%
#   group_by(country_name,cancer_label)%>%
#   summarize(country_code,country_name, cancer_code, cancer_label,age, 
#             age_cat,rel_surv,mx)


#PAF combinations

PAFs_age_Cat<-PAFs%>%
  mutate(
    age_cat = case_when(
      age>=4 & age<14 ~ "15-64",
      age>=14 ~ "65-99",
      FALSE ~"0-15"
    ))%>%
  filter(age_cat!="0-15")%>%
  droplevels()%>%
  group_by(country_code,cancer_label, age_cat,age)%>%
  summarize(country_code,country_label, cancer_code, cancer_label,
            age, age_cat, cases=sum(cases),   cases.prev=sum(cases.prev), 
            cases.notprev=sum(cases.notprev),
            af.comb=sum(cases.prev)/cases)%>%
  distinct()%>%
  group_by(country_label,cancer_label, age_cat)%>%
  mutate(total_overall=sum(cases))



# PAFS_Overall<-PAFs_age_Cat%>%mutate(age_cat="Overall")%>%
#   droplevels()%>%
#   group_by(country_code,cancer_label, age_cat,age)%>%
#   summarize(country_code,country_label, cancer_code, cancer_label,
#             age, age_cat, cases=sum(cases),   cases.prev=sum(cases.prev),
#             cases.notprev=sum(cases.notprev),
#             af.comb=sum(cases.prev)/cases)%>%
#   distinct()%>%
#   group_by(country_code,cancer_label, age_cat)%>%
#   mutate(total_overall=sum(cases))

PAFs2<-PAFs_age_Cat%>%
 # full_join(PAFS_Overall)%>%
  as.data.frame()%>%
  droplevels()%>%
  group_by(country_code,cancer_code, age_cat,age)%>%
  mutate(total_age_prev=sum(cases.prev))%>%
  as.data.frame()%>%
  #mutate(af.comb.agecat=total_age_prev/total_overall)%>%
  group_by(country_code,cancer_code,age)%>%
  summarize(country_label, cancer_label,
            age_cat,  total_overall,cases,
            cases.prev)%>%
  distinct()%>%
  arrange(cancer_label,
          age_cat)%>%
  select(-cancer_label)



R1<-Countries_Simulated%>%
  ungroup()%>%
  select(-country_label)%>%
  left_join(PAFs2,by=c("country_code"="country_code",
                       "cancer_code"="cancer_code",
                       "age"="age",
                       "age_cat"="age_cat"))%>%
  ungroup()%>%
  group_by(cancer_code,age)%>%
  filter(rel_surv==max(rel_surv))%>%
  droplevels()%>%
  distinct()%>%
  as.data.frame()


 
# R2<-Countries_Simulated_Overall%>%
#   as.data.frame()%>%
#   ungroup()%>%
#   filter(age_cat=="Overall")%>%
#   left_join(PAFs2,by=c("country_code"="country_code","cancer_code"="cancer_code","age_cat"="age_cat","age"))%>%
#   ungroup()%>%
#   group_by(cancer_code,age)%>%
#   filter(rel_surv==max(rel_surv))%>%
#   as.data.frame()%>%
#   group_by(cancer_code,age_cat)%>%
#   summarize(cancer_code, cancer_label,
#             age_cat, age, 
#             total_overall=sum(total_overall),
#             rel_surv=sum(rel_surv*cases)/sum(cases),
#             af.comb=sum(cases.prev)/sum(cases),
#             rel_surv,
#             Expected_5_year_surv_mx=sum(mx*cases)/sum(cases),
#             total_overall)%>%
#   droplevels()%>%
#   select(-age)%>%
#   distinct()%>%
#   as.data.frame()


Reference_Survival<-R1%>%select(country_label, country_code, 
                                cancer_label, cancer_code, 
                                age, age_cat, rel_surv,cases)%>%
  arrange(cancer_label, age)


#calculating agregated sums 

Ref_overall<-Reference_Survival%>%
  mutate(age_cat="Overall")%>%
  group_by(cancer_label, age_cat)%>%
  mutate(rel_surv = sum(rel_surv*cases)/sum(cases))%>%
  select(-cases,-age)%>%
  distinct()

Reference_Survival_Survcan<-Reference_Survival%>%
  group_by(cancer_label, age_cat)%>%
  mutate(rel_surv = sum(rel_surv*cases)/sum(cases))%>%
  select(-c(cases, age))%>%
  distinct()%>%full_join(Ref_overall)

names(R1)


Reference_Survival<-Reference_Survival%>%select(-cases)%>%distinct()

write.csv(Reference_Survival,"I:\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Data\\Reference_Survival.csv")
write.csv(Reference_Survival_Survcan,"I:\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Data\\Reference_Survival_Survcan.csv")



