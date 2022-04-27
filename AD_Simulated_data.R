
###############################################
#
# Net survival and Avoidable deaths - Simulated data
# Date: 25/4/2022
# Version 1.2
#
#
#Load files and packages in AD_2.R file
#
# Works for multiple cancer sites currently.
#
#Needs to be synced
#
###############################################


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

Thailand_popmort2015<-Thailand_popmort%>% #need to fix this here... What about the overall "mx" for all genders? 
  filter(X_year==2015)%>%
  group_by(X_age,X_year)%>% 
  summarize(mx=mean(mx),prob=mean(prob),region, country_code)%>% #VERY crude calculations
  distinct()%>%mutate(   age = case_when(
    X_age>=0 & X_age<=4 ~ 1,
    X_age>=5 & X_age<=9 ~ 2,
    X_age>=10 & X_age<=14 ~ 3,
    X_age>=15 & X_age<=19 ~ 4,
    X_age>=20 & X_age<=24 ~ 5,
    X_age>=25 & X_age<=29 ~ 6,
    X_age>=30 & X_age<=34 ~ 7,
    X_age>=35 & X_age<=39 ~ 8,
    X_age>=40 & X_age<=44 ~ 9,
    X_age>=45 & X_age<=49 ~ 10,
    X_age>=50 & X_age<=54 ~ 11,
    X_age>=55 & X_age<=59 ~ 12,
    X_age>=60 & X_age<=64 ~ 13,
    X_age>=65 & X_age<=69 ~ 14,
    X_age>=70 & X_age<=74 ~ 15,
    X_age>=75 & X_age<=79 ~ 16,
    X_age>=80 & X_age<=84 ~ 17,
    X_age>=85 ~ 18,
  ))


countries_5y<-lm_HDI%>% #seems like there is an issue with countries missing. Not sure why
  full_join(hvh_HDI)%>%
  arrange(country_name)%>%
  left_join(Thailand_popmort2015,by=c("age"="age","country_code"="country_code"))%>%
  select(-region)

  
  
Countries_Simulated <-countries_5y%>%
  filter(country_name=="Thailand")%>%
  mutate(
    age_cat = case_when(
      age>=4 & age<14 ~ "15-64",
      age>=14 ~ "65-99",
      FALSE ~"0-15"
    ))%>%
  filter(age_cat!="0-15")%>%
  group_by(country_name,cancer_label,age_cat)%>%
  summarize(country_code,country_name, cancer_code, cancer_label,age,
            age_cat,rel_surv,mx)%>%as.data.frame()

Countries_Simulated_Overall<-Countries_Simulated%>%
  mutate(age_cat="Overall")%>%
  group_by(country_name,cancer_label)%>%
  summarize(country_code,country_name, cancer_code, cancer_label,age, 
            age_cat,rel_surv,mx)


simulated_overall<-Countries_Simulated%>%
  full_join(Countries_Simulated_Overall)%>% 
  as.data.frame()%>%
  droplevels()%>%
  group_by(country_name,cancer_label, age_cat,age)%>%
  summarize(country_code,country_name, cancer_code, cancer_label,rel_surv,mx,
            age_cat, age)%>%
  distinct()%>%
  arrange(cancer_label,
          age_cat)

#PAF combinations

PAFs_age_Cat<-PAFs%>%
  filter(country_label=="Thailand")%>%
  mutate(
    age_cat = case_when(
      age>=4 & age<14 ~ "15-64",
      age>=14 ~ "65-99",
      FALSE ~"0-15"
    ))%>%
  filter(age_cat!="0-15")%>%
  droplevels()%>%
  group_by(country_label,cancer_label, age_cat,age)%>%
  summarize(country_code,country_label, cancer_code, cancer_label,
            age, age_cat, cases=sum(cases),   cases.prev=sum(cases.prev), 
            cases.notprev=sum(cases.notprev),
            af.comb=sum(cases.prev)/cases)%>%
  distinct()%>%
  group_by(country_label,cancer_label, age_cat)%>%
  mutate(total_overall=sum(cases))



PAFS_Overall<-PAFs_age_Cat%>%mutate(age_cat="Overall")%>%
  droplevels()%>%
  group_by(country_label,cancer_label, age_cat,age)%>%
  summarize(country_code,country_label, cancer_code, cancer_label,
            age, age_cat, cases=sum(cases),   cases.prev=sum(cases.prev), 
            cases.notprev=sum(cases.notprev),
            af.comb=sum(cases.prev)/cases)%>%
  distinct()%>%
  group_by(country_label,cancer_label, age_cat)%>%
  mutate(total_overall=sum(cases))

PAFs2<-PAFs_age_Cat%>%
  full_join(PAFS_Overall)%>%
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



Simulated_Data_PAF_1<-simulated_overall%>%
  ungroup()%>%
  filter(age_cat!="Overall")%>%
  left_join(PAFs2,by=c("country_code"="country_code","cancer_code"="cancer_code","age_cat"="age_cat","age"))%>%
  ungroup()%>%
  group_by(cancer_code,age_cat)%>%
  summarize(country_code, 
            country_label, 
            cancer_code, cancer_label,
            age_cat, age, total_overall,
            rel_surv=sum(rel_surv*cases)/sum(cases),
            af.comb=sum(cases.prev)/sum(cases),
            rel_surv,
            Expected_5_year_surv_mx=sum(mx*cases)/sum(cases),
            total_overall)%>%
  droplevels()%>%
  select(-age)%>%
  distinct()%>%
  as.data.frame()

  
  Simulated_Data_PAF_2<-simulated_overall%>%
    ungroup()%>%
    filter(age_cat=="Overall")%>%
    left_join(PAFs2,by=c("country_code"="country_code","cancer_code"="cancer_code","age_cat"="age_cat","age"))%>%
    ungroup()%>%
    group_by(cancer_code,age_cat)%>%
    summarize(country_code, 
              country_label, 
              cancer_code, cancer_label,
              age_cat, age, total_overall,
              rel_surv=sum(rel_surv*cases)/sum(cases),
              af.comb=sum(cases.prev)/sum(cases),
              rel_surv,
              Expected_5_year_surv_mx=sum(mx*cases)/sum(cases),
              total_overall)%>%
    droplevels()%>%
    select(-age)%>%
    distinct()%>%
    as.data.frame()
  
  
  Simulated_Data_PAF<-Simulated_Data_PAF_1%>%full_join(Simulated_Data_PAF_2)
#Avoidable deaths

#Three AD calcs 

#first need to make sure data is in right format (numeric columns)

#Applying the equation from Rutherford 2015 for AD. Needs to be updated to have scaled relative survival
Avoidable_Deaths_Simulated <- matrix(ncol = 6, nrow = nrow(Simulated_Data_PAF)) #AD(t)

for (i in 1:nrow(Avoidable_Deaths_Simulated)) {
  
  Expected_5_year_surv_mx <- Simulated_Data_PAF[i,]$Expected_5_year_surv_mx #Crude calculations of expected survival
  
  #Preventable deaths
    AD_prev<-(Simulated_Data_PAF[i,]$af.comb)*Simulated_Data_PAF[i,]$total_overall*(1-Simulated_Data_PAF[i,]$rel_surv)*Expected_5_year_surv_mx
  #AD_prev_Lower<-(Simulated_Data_PAF[i,]$af.comb.agecat)*Simulated_Data_PAF[i,]$total_overall*(1-Simulated_Data_PAF[i,]$NS_Lower_CI)*Expected_5_year_surv_mx
  #AD_prev_Upper<-(Simulated_Data_PAF[i,]$af.comb.agecat)*Simulated_Data_PAF[i,]$total_overall*(1-Simulated_Data_PAF[i,]$NS_Upper_CI)*Expected_5_year_surv_mx

  # #Avoidable deaths (treatable: #check what the lower CI is called in the previous data frame
  
  AD_treat<-(0.9-Simulated_Data_PAF[i,]$rel_surv)*Expected_5_year_surv_mx*(1-Simulated_Data_PAF[i,]$af.comb)*Simulated_Data_PAF[i,]$total_overall
  #AD_treat_Lower<-(0.9-Simulated_Data_PAF[i,]$NS_Lower_CI)*Expected_5_year_surv_mx*(1-Simulated_Data_PAF[i,]$af.comb.agecat)*Simulated_Data_PAF[i,]$total_overall
  #AD_treat_Upper<-(0.9-Simulated_Data_PAF[i,]$NS_Upper_CI)*Expected_5_year_surv_mx*(1-Simulated_Data_PAF[i,]$af.comb.agecat)*Simulated_Data_PAF[i,]$total_overall
  
  
  #Deaths not avoidable 
  
  AD_unavoid<-(1-Simulated_Data_PAF[i,]$af.comb)*Simulated_Data_PAF[i,]$total_overall*(1-Simulated_Data_PAF[i,]$rel_surv*Expected_5_year_surv_mx)
  #AD_unavoid_Lower<-(1-Simulated_Data_PAF[i,]$af.comb.agecat)*Simulated_Data_PAF[i,]$total_overall*(1-Simulated_Data_PAF[i,]$NS_Lower_CI*Expected_5_year_surv_mx)
  #AD_unavoid_Upper<-(1-Simulated_Data_PAF[i,]$af.comb.agecat)*Simulated_Data_PAF[i,]$total_overall*(1-Simulated_Data_PAF[i,]$NS_Upper_CI*Expected_5_year_surv_mx)
  
  
  Avoidable_Deaths_Simulated[i, ] <- c(Simulated_Data_PAF[i, ]$age_cat,
    Simulated_Data_PAF[i, ]$cancer_code,
    Simulated_Data_PAF[i, ]$cancer_label,
  
    AD_treat,
    #AD_treat_Lower,
    #AD_treat_Upper,
    AD_prev,
    #AD_prev_Lower,
    #AD_prev_Upper,
    AD_unavoid
    #AD_unavoid_Lower,
    #AD_unavoid_Upper
  )
}

Avoidable_Deaths_Simulated<-Avoidable_Deaths_Simulated%>%as.data.frame()

colnames(Avoidable_Deaths_Simulated)<-c("age_cat","cancer_code","cancer",   "AD_treat",
                              #"AD_treat_Lower", 
                              #"AD_treat_Upper",
                              "AD_prev",
                            #  "AD_prev_Lower",
                            #  "AD_prev_Upper",
                              "AD_unavoid"
                             # "AD_unavoid_Lower",
                            #  "AD_unavoid_Upper"
                            )

Avoidable_Deaths_Simulated


write.csv(Avoidable_Deaths_Simulated,"~/Documents/R_Projects/Data/Thai_AD_Simulated.csv")





  



