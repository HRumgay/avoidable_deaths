###############################################
#
# Net survival and Avoidable deaths - Simulated data Thailand Example
# Date: 09/06/2022
# Version 2.22
#
#
#Load files and packages in AD_2.R file
#
# Works for multiple cancer sites currently.
#
#
###############################################


#Combining datasets at the time point of interest 5 years and combining to HDI

# hvh_HDI<-Israel%>%filter(time==5)%>% #Upper survival values
#   left_join(HDI, by="country_code")%>%
#   select(-c(country_label, hdi_rank, year, X))%>%
#   filter(hdi_group%in% c(3,4))
# 
# lm_HDI<-Thailand%>%filter(time==5)%>% #lower HDI survival values
#   left_join(HDI, by="country_code")%>%
#   select(-c(country_label, hdi_rank, year,X.1, X))%>%
#  # filter(hdi_group%in% c(1,2))%>%
#   filter(!hdi_group%in%c(3,4))

#New combined file
Survival_Modelled


# Anchored and combined data set at t=5 with anchored values from Israel and Thailand

Thai_pop<-Thailand_pop%>%
  mutate(age = case_when(
  age=="0 to 4 years" ~ 1,
  age=="5 to 9 years"  ~ 2,
  age=="10 to 14 years"~ 3,
  age=="15 to 19 years"~ 4,
  age=="20 to 24 years" ~ 5,
  age==  "25 to 29 years"~ 6,
  age== "30 to 34 years"~ 7,
  age== "35 to 39 years"~ 8,
  age== "40 to 44 years"~ 9,
  age==  "45 to 49 years"  ~ 10,
  age== "50 to 54 years"~ 11,
  age==  "55 to 59 years"~ 12,
  age== "60 to 64 years"~ 13,
  age=="65 to 69 years"     ~ 14,
  age== "70 to 74 years"~ 15,
  age==  "75 to 79 years"  ~ 16,
  age== "80 to 84 years" ~ 17,
  age== "85 years and over"~ 18
  ))%>%
  mutate(sex=replace(sex,sex=="Male",1))%>%
  mutate(sex=replace(sex,sex=="Female",2))%>%
  as.data.frame()%>%
  mutate(sex=as.integer(sex))%>%mutate(country_code=764)%>%
  mutate(country_code=as.numeric(country_code))

Thailand_popmort2015<-Thailand_popmort%>% #need to fix this here... What about the overall "mx" for all genders? 
  filter(X_year==2012)%>%
  group_by(X_age,X_year)%>%
  filter(sex!=0)%>%
  rename("year"="X_year")%>%
  rename("age"="X_age")%>%
   distinct()%>%
  mutate(age = case_when(
    age>=0 & age<=4 ~ 1,
    age>=5 & age<=9 ~ 2,
    age>=10 & age<=14 ~ 3,
    age>=15 & age<=19 ~ 4,
    age>=20 & age<=24 ~ 5,
    age>=25 & age<=29 ~ 6,
    age>=30 & age<=34 ~ 7,
    age>=35 & age<=39 ~ 8,
    age>=40 & age<=44 ~ 9,
    age>=45 & age<=49 ~ 10,
    age>=50 & age<=54 ~ 11,
    age>=55 & age<=59 ~ 12,
    age>=60 & age<=64 ~ 13,
    age>=65 & age<=69 ~ 14,
    age>=70 & age<=74 ~ 15,
    age>=75 & age<=79 ~ 16,
    age>=80 & age<=84 ~ 17,
    age>=85 ~ 18
  ))%>%
     as.data.frame()%>%
  distinct()



countries_5y<-Survival_Modelled%>%
  arrange(country_name)%>%
  left_join(Thailand_popmort2015, by=c("age"="age","country_code"="country_code"))%>%
  select(-region)

  
Countries_modelled <-countries_5y%>%
  filter(country_name=="Thailand")%>%
  mutate(age = case_when(
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
  mutate(
    age_cat = case_when(
      age>=4 & age<14 ~ "15-64",
      age>=14 ~ "65-99",
      age<4 ~"0-15"
    ))%>%
  filter(age_cat!="0-15")%>%
  group_by(country_name,cancer_label,age)%>%
  summarize(country_code,country_name, cancer_code, cancer_label,age,
            age_cat,rel_surv)%>%as.data.frame()%>%distinct()

# Countries_modelled_Overall<-Countries_modelled%>%
#   mutate(age_cat="Overall")%>%
#   group_by(country_name,cancer_label)%>%
#   dplyr::summarize(country_code,country_name, cancer_code, cancer_label,age, 
#             age_cat,rel_surv,mx)


simulated_overall<-Countries_modelled%>%
 #full_join(Countries_modelled_Overall)%>% 
  as.data.frame()%>%
  droplevels()%>%
  group_by(country_name,cancer_label, age)%>%
  dplyr::summarize(country_code,country_name, 
            cancer_code, cancer_label,rel_surv,
            age_cat, age)%>%
  distinct()%>%
  arrange(cancer_label,
          age)



#PAF combinations


Thailand_expected_Survival2<-Thailand_expected_Survival%>%
   filter(age>=4)
  # mutate(age=replace(age, age==19,18))%>%
  # mutate(age=replace(age, age==20,18))%>%
  # mutate(
  #   age_cat = case_when(
  #     age>=4 & age<14 ~ "15-64",
  #     age>=14 ~ "65-99",
  #     FALSE ~"0-15"
  #   ))%>%
  # filter(age_cat!="0-15")%>%
  # group_by(age_cat)%>%
  # dplyr::summarize(ES)
    
PAFs_simulated<-PAFs



PAFs_age_Cat2<-PAFs_simulated%>%
  filter(country_label=="Thailand")%>%
  mutate(age = case_when(
    age==4~ 4,
    age>4 & age<=9~ 9,
    age==10~ 10,
    age==11~ 11,
    age==12~ 12,
    age==13~ 13,
    age==14~ 14,
    age==15~ 15,
    age>=16 ~ 16
  ))%>%
  mutate(
    age_cat = case_when(
      age>=4 & age<14 ~ "15-64",
      age>=14 ~ "65-99",
      age<4 ~"0-15"
    ))%>%
  filter(age_cat!="0-15")%>%
  droplevels()%>%
  left_join(Thailand_expected_Survival2, by=c("age","sex"))%>%
  group_by(country_label,cancer_label, age) %>%
  mutate(ES= case_when(cases!=0 ~ sum(ES*cases)/sum(cases),
                       cases==0 ~ ES)) %>%
  dplyr::dplyr::summarize(country_code,country_label, cancer_code, cancer_label,
            age, age_cat, 
            cases=sum(cases),
            cases.prev=sum(cases.prev), 
            cases.notprev=sum(cases.notprev),
            # af.comb= sum(cases.prev)/sum(cases),
            af.comb= case_when(cases!=0 ~  sum(cases.prev)/sum(cases),
                               cases.prev==0 ~ 0),
            total_overall=sum(cases),
            ES)%>% #Summarizing by sex. This is a bit crude but won't be used in calcs anyways
  mutate(ES= case_when(cases!=0 ~ sum(ES*cases)/sum(cases),
                       cases==0 ~ ES))%>%
  distinct()%>%
  group_by(country_label,cancer_label, age)


# PAFS_Overall<-PAFs_age_Cat2%>%mutate(age_cat="Overall")%>%
#   droplevels()%>%
#   group_by(country_label,cancer_label, age_cat,age)%>%
#   dplyr::summarize(country_code,country_label, cancer_code, cancer_label,
#             age, age_cat, cases=sum(cases),   cases.prev=sum(cases.prev), 
#             cases.notprev=sum(cases.notprev),
#             af.comb=sum(cases.prev)/cases,
#             ES=sum(ES*cases)/sum(cases))%>%
#   distinct()%>%
#   group_by(country_label,cancer_label, age_cat)%>%
#   mutate(total_overall=sum(cases))

PAFs3<-PAFs_age_Cat2%>%
 #full_join(PAFS_Overall)%>%
  as.data.frame()%>%
  droplevels()%>%
  group_by(country_code,cancer_code, age_cat,age)%>%
  mutate(total_age_prev=sum(cases.prev))%>%
  as.data.frame()%>%
 #mutate(af.comb.agecat=total_age_prev/total_overall)%>%
  group_by(country_code,cancer_code,age)%>%
  dplyr::summarize(country_label, cancer_label,
            age_cat,  total_overall,cases,
            cases.prev,
            ES,af.comb)%>%
  distinct()%>%
  arrange(cancer_label,
          age)%>%
  select(-cancer_label)


Simulated_Data_PAF_1<-simulated_overall%>%
  ungroup()%>%
  filter(age_cat!="Overall")%>%
  left_join(PAFs3,by=c("country_code"="country_code","cancer_code"="cancer_code","age_cat"="age_cat","age"))%>%
 #left_join(Thailand_expected_Survival2, by=c("a"))%>%
  ungroup()%>%
  group_by(cancer_code,age_cat, age)%>%
  dplyr::summarize(country_code, country_label,
            cancer_code, cancer_label,
            age_cat, age, total_overall,
            af.comb= case_when(total_overall!=0 ~ sum(cases.prev)/sum(cases),
                               total_overall==0 ~ af.comb),
            rel_surv= case_when(total_overall!=0 ~ sum(rel_surv*cases)/sum(cases),
                                total_overall==0 ~ rel_surv),
            Expected_5_year_surv=case_when(total_overall!=0 ~ sum(ES*cases)/sum(cases),
                                           total_overall==0 ~ ES),
            # = case_when(total_overall!=0 ~ sum(ES*cases)/sum(cases),
            #                                 total_overall==0 ~ ES),
            rel_surv,
            af.comb,
            Expected_5_year_surv,
            total_overall)%>%
  droplevels()%>%
 # select(-age)%>%
  distinct()%>%
  as.data.frame()

  
  # Simulated_Data_PAF_2<-simulated_overall%>%
  #   ungroup()%>%
  #   filter(age_cat=="Overall")%>%
  #   left_join(PAFs3,by=c("country_code"="country_code","cancer_code"="cancer_code","age_cat"="age_cat","age"))%>%
  #   ungroup()%>%
  #   group_by(cancer_code,age_cat, age)%>%
  #   dplyr::summarize(country_code, 
  #             country_label, 
  #             cancer_code, cancer_label,
  #             age_cat, age, total_overall,
  #             rel_surv=sum(rel_surv*cases)/sum(cases),
  #             af.comb=sum(cases.prev)/sum(cases),
  #             rel_surv,
  #             Expected_5_year_surv=sum(ES*cases)/sum(cases),
  #             total_overall)%>%
  #   droplevels()%>%
  #   select(-age)%>%
  #   distinct()%>%
  #   as.data.frame()
  
  
  Simulated_Data_PAF<-Simulated_Data_PAF_1%>%
    #full_join(Simulated_Data_PAF_2)%>%
    left_join(Reference_Survival,by=c("age","cancer_code"))%>%
    distinct
  
  
  
  
#Avoidable deaths

#Three AD calcs 

#first need to make sure data is in right format (numeric columns)

#Applying the equation from Rutherford 2015 for AD. Needs to be updated to have scaled relative survival

Avoidable_Deaths_modelled <- matrix(ncol = 8, nrow = nrow(Simulated_Data_PAF)) #AD(t)
#NS_Ref<-0.9 #Reference countries survival


for (i in 1:nrow(Avoidable_Deaths_modelled)) {

  
  Expected_5_year_surv  <- Simulated_Data_PAF[i,]$Expected_5_year_surv 
  
  #Preventable deaths
  AD_prev <- (Simulated_Data_PAF[i,]$af.comb) * 
    Simulated_Data_PAF[i,]$total_overall * 
    (1 - Simulated_Data_PAF[i,]$rel_surv) *
    (Expected_5_year_surv )
  #AD_prev_Lower<-(Simulated_Data_PAF[i,]$af.comb.agecat)*Simulated_Data_PAF[i,]$total_overall*(1-Simulated_Data_PAF[i,]$NS_Lower_CI)*Expected_5_year_surv 
  #AD_prev_Upper<-(Simulated_Data_PAF[i,]$af.comb.agecat)*Simulated_Data_PAF[i,]$total_overall*(1-Simulated_Data_PAF[i,]$NS_Upper_CI)*Expected_5_year_surv 

  # #Avoidable deaths (treatable: #check what the lower CI is called in the previous data frame
  
  AD_treat<-(Simulated_Data_PAF[i,]$surv_ref-Simulated_Data_PAF[i,]$rel_surv)*
    (1-Simulated_Data_PAF[i,]$af.comb)*
    (Simulated_Data_PAF[i,]$total_overall)*
    (Expected_5_year_surv )
  #AD_treat_Lower<-(0.9-Simulated_Data_PAF[i,]$NS_Lower_CI)*Expected_5_year_surv *(1-Simulated_Data_PAF[i,]$af.comb.agecat)*Simulated_Data_PAF[i,]$total_overall
  #AD_treat_Upper<-(0.9-Simulated_Data_PAF[i,]$NS_Upper_CI)*Expected_5_year_surv *(1-Simulated_Data_PAF[i,]$af.comb.agecat)*Simulated_Data_PAF[i,]$total_overall
  
  
  #Deaths not avoidable 
  
  AD_unavoid<-(1-Simulated_Data_PAF[i,]$af.comb)*
    Simulated_Data_PAF[i,]$total_overall*
    (Simulated_Data_PAF[i,]$surv_ref-Simulated_Data_PAF[i,]$rel_surv*
       (Expected_5_year_surv ))
  #AD_unavoid_Lower<-(1-Simulated_Data_PAF[i,]$af.comb.agecat)*Simulated_Data_PAF[i,]$total_overall*(1-Simulated_Data_PAF[i,]$NS_Lower_CI*Expected_5_year_surv )
  #AD_unavoid_Upper<-(1-Simulated_Data_PAF[i,]$af.comb.agecat)*Simulated_Data_PAF[i,]$total_overall*(1-Simulated_Data_PAF[i,]$NS_Upper_CI*Expected_5_year_surv )
  
  
  Avoidable_Deaths_modelled[i, ] <- c(
    Simulated_Data_PAF[i, ]$age_cat,
    Simulated_Data_PAF[i, ]$age,
    Simulated_Data_PAF[i, ]$cancer_code,
    Simulated_Data_PAF[i, ]$cancer_label,
    AD_treat,
    #AD_treat_Lower,
    #AD_treat_Upper,
    AD_prev,
    #AD_prev_Lower,
    #AD_prev_Upper,
    AD_unavoid,
    #AD_unavoid_Lower,
    #AD_unavoid_Upper,
    Simulated_Data_PAF[i,]$total_overall
  )
}


colnames(Avoidable_Deaths_modelled)<-c("age_cat","age", "cancer_code","cancer",   "AD_treat",
                              #"AD_treat_Lower", 
                              #"AD_treat_Upper",
                              "AD_prev",
                            #  "AD_prev_Lower",
                            #  "AD_prev_Upper",
                              "AD_unavoid",
                             # "AD_unavoid_Lower",
                            #  "AD_unavoid_Upper",
                            "total_overall")

#Mortality incidence ratios modified here. 
MIR_Age_Cats_Thailand<-MIR_Age_Cats%>%
  filter(country_label=="Thailand")%>%
  select(-country_label, - country_code, -cancer_label, -X)

MIR_Globocan_Thailand<-MIR_Globocan%>%
  ungroup()%>%
  filter(age>=4)%>%
  group_by(country_code, cancer_code, age)%>%
  mutate(MIR=sum(MIR*py)/sum(py))%>%
  mutate(py=sum(py))%>%distinct()%>%
  filter(country_label=="Thailand")%>%
  ungroup()%>%
  select(-country_label, -country_code, -cancer_label)





Avoidable_Deaths_modelled<-Avoidable_Deaths_modelled%>%
  as.data.frame()%>%
  group_by(cancer_code,age)%>%
  mutate(age=as.numeric(as.character(age)))%>%
  mutate(AD_prev=as.numeric(as.character(AD_prev)))%>%
  mutate(AD_unavoid=as.numeric(as.character(AD_unavoid)))%>%
  mutate(total_overall=as.numeric(as.character(total_overall)))%>%
  mutate(AD_treat=as.numeric(as.character(AD_treat)))%>%
  mutate(AD_sum=AD_prev + AD_unavoid + AD_treat)%>%
  mutate(cancer_code=as.numeric(cancer_code))%>%
 # filter(total_overall<AD_sum)%>%
  as.data.frame()%>%distinct()

Avoidable_Deaths_modelled2<-Avoidable_Deaths_modelled%>%
  mutate(age=as.numeric(age))%>%
  mutate(age_cat=as.character(age_cat))#%>%
 # left_join(MIR_Globocan_Thailand)

Avoidable_Deaths_modelled_overall<-Avoidable_Deaths_modelled2%>%
  mutate(age_cat="Overall")%>%
 # ungroup()%>%
  group_by(cancer_code, age_cat)%>%
  select(-age)%>%
  mutate(total_overall=sum(total_overall))%>%
  mutate(AD_prev= sum(AD_prev))%>%
  mutate(AD_treat = sum(AD_treat))%>%
  mutate(AD_unavoid = sum(AD_unavoid))%>%
  mutate(AD_sum=AD_prev + AD_unavoid + AD_treat)%>%
  #mutate(total_overall=sum(total_overall))%>%
  distinct()%>%
  as.data.frame()

Avoidable_Deaths_modelled_age_cat<-Avoidable_Deaths_modelled2%>%
  ungroup()%>%
  group_by(cancer_code, age_cat)%>%
  mutate(AD_prev= sum(AD_prev))%>%
  mutate(AD_treat = sum(AD_treat))%>%
  mutate(AD_unavoid = sum(AD_unavoid))%>%
  mutate(AD_sum=AD_prev + AD_unavoid + AD_treat)%>%
  mutate(total_overall=sum(total_overall))%>%
  full_join(Avoidable_Deaths_modelled_overall)%>%
  select(-age)%>%

  distinct()%>%
  arrange(cancer, age_cat)%>%
  as.data.frame()
 # left_join(MIR_Age_Cats_Thailand, by=c("cancer_code", "age_cat"))


#The AD by age groups (larger vs smaller age categories)

Avoidable_Deaths_modelled2
Avoidable_Deaths_modelled_age_cat

#Writing the export files
write.csv(Avoidable_Deaths_modelled2, "~/Documents/R_Projects/Data/Thai_AD_Modelled.csv")
write.csv(Avoidable_Deaths_modelled_age_cat, "~/Documents/R_Projects/Data/Thai_AD_Modelled_More_age_Cats.csv")
write.csv(Simulated_Data_PAF, "~/Documents/R_Projects/Data/Thai_NS_Modelled.csv")




