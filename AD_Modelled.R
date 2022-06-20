
###############################################
#
# Net survival and Avoidable deaths - Simulated data
#  Date: 09/06/2022
# Version 2.2
#
#
#Load files and packages in AD_2.R file
#
# Works for multiple cancer sites currently.
#
#
###############################################

#Expected survival

ES2<-ES_dt%>%
  filter(time==1000)%>%
  select(-time)%>%
  rename("ES"="SurvExp")



#Combining datasets at the time point of interest 5 years and combining to HDI

#check country_code for China in HDI dataset and Thailand/Israel datasets match up

hvh_HDI<-Israel%>%filter(time==5)%>% #Upper survival values
  left_join(HDI, by="country_code")%>%
  select(-c(country_label, hdi_rank, year, X))%>%
  filter(hdi_group%in% c(3,4))

lm_HDI<-Thailand%>%filter(time==5)%>% #lower HDI survival values
  left_join(HDI, by="country_code")%>%
  select(-c(country_label, hdi_rank, year,X.1, X))%>%
  #filter(hdi_group%in% c(1,2))%>%
  filter(!hdi_group%in%c(3,4))

lm_HDI%>%summarize(country_name,country_code, hdi_group)%>%distinct()%>%
  filter(!hdi_group%in%c(3,4)) #includes low, medium and missing HDI groups

# Anchored and combined data set at t=5 with anchored values from Israel and Thailand

popmort<-popmort2%>%
  filter(sex!=0)%>%
  filter(year==2015)%>%
  mutate(mx=1-prob)%>%
  mutate(country_code=as.numeric(country_code))%>%
  group_by(country_code,age,year)%>%
  mutate(mx= case_when(cases!=0 ~ sum(mx*cases)/sum(cases),
                            cases==0 ~    mx))%>%
  mutate(prob= case_when(cases!=0 ~ sum(prob*cases)/sum(cases),
                            cases==0 ~    prob))%>%
  summarize(mx, 
            prob,
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
  group_by(country_name,cancer_label,age_cat)%>%
  summarize(country_code,country_name, cancer_code, cancer_label,
            age, age_cat,
            rel_surv, mx)%>%as.data.frame()


# Countries_Simulated_Overall<-Countries_Simulated%>%
#   mutate(age_cat="Overall")%>%
#   group_by(country_name,cancer_label)%>%
#   summarize(country_code,country_name, cancer_code, cancer_label,age, 
#             age_cat,rel_surv,mx)


simulated_overall<-Countries_Simulated%>%
 # full_join(Countries_Simulated_Overall)%>% 
  as.data.frame()%>%
  droplevels()%>%
  group_by(country_name,cancer_label, age_cat,age)%>%
  summarize(country_code,country_name, cancer_code, cancer_label,rel_surv,mx,
            age_cat, age)%>%
  distinct()%>%
  arrange(cancer_label, age)



#PAF combinations

PAFs_age_Cat<-PAFs%>%
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
  droplevels()%>%
  left_join(ES2, by=c("country_code","age","sex"))%>% 
  group_by(country_label,cancer_label, age) %>%
  mutate(ES= case_when(cases!=0 ~ sum(ES*cases)/sum(cases),
                       cases==0 ~ ES)) %>%
  summarize(country_code,country_label, cancer_code, cancer_label,
            age, age_cat, 
            cases=sum(cases),
            cases.prev=sum(cases.prev), 
            cases.notprev=sum(cases.notprev),
            # af.comb= sum(cases.prev)/sum(cases),
            af.comb= case_when(cases!=0 ~  sum(cases.prev)/sum(cases),
                               cases.prev==0 ~ 0),
            total_overall=sum(cases),
            ES)%>% #Summarizing by sex
  mutate(ES= case_when(cases!=0 ~ sum(ES*cases)/sum(cases),
                       cases==0 ~ ES))%>%
  distinct()%>%
  group_by(country_label,cancer_label, age)


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
  group_by(country_code,cancer_code,cancer_label, age_cat,age)%>%
  mutate(total_age_prev=sum(cases.prev))%>%
  as.data.frame()%>%
  #mutate(af.comb.agecat=total_age_prev/total_overall)%>%
  group_by(country_code,cancer_code,age)%>%
  summarize(country_label, cancer_label,
            age_cat, age,  total_overall,cases,
            cases.prev, ES, af.comb)%>%
  distinct()%>%
  arrange(cancer_label,
          age)%>%
  select(-cancer_label)



Simulated_Data_PAF_1<-simulated_overall%>%
  ungroup()%>%
  filter(age_cat!="Overall")%>%
  left_join(PAFs2,by=c("country_code"="country_code",
                       "cancer_code"="cancer_code",
                       "age_cat"="age_cat",
                       "age"="age"))%>%
  ungroup()%>%
  group_by(country_code,cancer_code, age)%>%
  # mutate(ES= case_when(cases!=0 ~ sum(ES*cases)/sum(cases),
  #                      cases==0 ~ ES))%>%
  # mutate(af.comb= case_when(cases!=0 ~ sum(cases.prev*cases)/sum(cases),
  #                           af_comb~    af.comb))%>%
  # mutate(rel_surv= case_when(cases!=0 ~ sum(rel_surv*cases)/sum(cases),
  #                            cases==0 ~    rel_surv))%>%
  # mutate(Expected_5_year_surv=case_when(cases!=0 ~ sum(ES*cases)/sum(cases),
  #                                       cases==0 ~ ES))%>%
  mutate(af.comb=case_when(cases!=0 ~ sum(cases.prev)/sum(cases),
                           cases==0  ~    0))%>%
  mutate(rel_surv=case_when(cases!=0 ~ sum(rel_surv*cases)/sum(cases),
                            cases==0  ~    rel_surv))%>%
  summarize(country_code, 
            country_label, 
            cancer_code, cancer_label,
            age_cat, age, total_overall,
            rel_surv,
            af.comb,
            rel_surv,
            ES,
            Expected_5_year_surv_mx,
            total_overall)%>%
  droplevels()%>%
  #select(-age)%>%
  distinct()%>%
  as.data.frame()

  
  

# Simulated_Data_PAF_2<-simulated_overall%>%
  #   ungroup()%>%
  #   filter(age_cat=="Overall")%>%
  #   left_join(PAFs2,by=c("country_code"="country_code","cancer_code"="cancer_code","age_cat"="age_cat","age"))%>%
  #   ungroup()%>%
  #   group_by(country_code,cancer_code,age_cat)%>%
  #   summarize(country_code, 
  #             country_label, 
  #             cancer_code, cancer_label,
  #             age_cat, age, total_overall,
  #             rel_surv=sum(rel_surv*cases)/sum(cases),
  #             af.comb=sum(cases.prev)/sum(cases),
  #             rel_surv,
  #             ES=sum(mx*cases)/sum(cases),
  #             total_overall)%>%
  #   droplevels()%>%
  #   select(-age)%>%
  #   distinct()%>%
  #   as.data.frame()
  
  
  Simulated_Data_PAF_All <- Simulated_Data_PAF_1%>%
    mutate(rel_surv=as.double(rel_surv))%>%
    mutate(af.comb=as.double(af.comb))%>% 
    mutate(total_overall=as.double(total_overall))%>%
  #  full_join(Simulated_Data_PAF_2)%>%
    arrange(country_label,cancer_code,age_cat)%>%
    left_join(Reference_Survival,by=c("age","cancer_code"))%>%
    as_tibble()
  

#Avoidable deaths

#Three AD calcs 

#first need to make sure data is in right format (numeric columns)

#Applying the equation from Rutherford 2015 for AD. Needs to be updated to have scaled relative survival
Avoidable_Deaths_Simulated_All <- matrix(ncol = 11, nrow = nrow(Simulated_Data_PAF_All)) #AD(t)

#NS_Ref<-0.9 #Reference countries survival


for (i in 1:nrow(Avoidable_Deaths_Simulated_All)) {
  
  ES <- Simulated_Data_PAF_All[i,]$ES #Crude calculations of expected survival
  
  #Preventable deaths
  AD_prev <- (Simulated_Data_PAF_All[i,]$af.comb) * 
    Simulated_Data_PAF_All[i,]$total_overall * 
    (1 - Simulated_Data_PAF_All[i,]$rel_surv) *
    (ES) #Change to expected survival from Hadriens code
  #AD_prev_Lower<-(Simulated_Data_PAF_All[i,]$af.comb.agecat)*Simulated_Data_PAF_All[i,]$total_overall*(1-Simulated_Data_PAF_All[i,]$NS_Lower_CI)*ES
  #AD_prev_Upper<-(Simulated_Data_PAF_All[i,]$af.comb.agecat)*Simulated_Data_PAF_All[i,]$total_overall*(1-Simulated_Data_PAF_All[i,]$NS_Upper_CI)*ES

  # #Avoidable deaths (treatable: #check what the lower CI is called in the previous data frame
  
  AD_treat<-(1-Simulated_Data_PAF_All[i,]$af.comb) *
    (Simulated_Data_PAF_All[i,]$total_overall) *
    (Simulated_Data_PAF_All[i,]$surv_ref-Simulated_Data_PAF_All[i,]$rel_surv) *
    (ES)
  #AD_treat_Lower<-(0.9-Simulated_Data_PAF_All[i,]$NS_Lower_CI)*ES*(1-Simulated_Data_PAF_All[i,]$af.comb.agecat)*Simulated_Data_PAF_All[i,]$total_overall
  #AD_treat_Upper<-(0.9-Simulated_Data_PAF_All[i,]$NS_Upper_CI)*ES*(1-Simulated_Data_PAF_All[i,]$af.comb.agecat)*Simulated_Data_PAF_All[i,]$total_overall
  
  
  #Deaths not avoidable 
  
  AD_unavoid<-(1-Simulated_Data_PAF_All[i,]$af.comb)*
    Simulated_Data_PAF_All[i,]$total_overall*
    (Simulated_Data_PAF_All[i,]$surv_ref-Simulated_Data_PAF_All[i,]$rel_surv*
       (ES))
  #AD_unavoid_Lower<-(1-Simulated_Data_PAF_All[i,]$af.comb.agecat)*Simulated_Data_PAF_All[i,]$total_overall*(1-Simulated_Data_PAF_All[i,]$NS_Lower_CI*ES)
  #AD_unavoid_Upper<-(1-Simulated_Data_PAF_All[i,]$af.comb.agecat)*Simulated_Data_PAF_All[i,]$total_overall*(1-Simulated_Data_PAF_All[i,]$NS_Upper_CI*ES)
  
  
  Avoidable_Deaths_Simulated_All[i, ] <- c(Simulated_Data_PAF_All[i, ]$country_code,
                                       Simulated_Data_PAF_All[i, ]$country_label,
                                       Simulated_Data_PAF_All[i, ]$age_cat,
                                       Simulated_Data_PAF_All[i, ]$age,
    Simulated_Data_PAF_All[i, ]$cancer_code,
    Simulated_Data_PAF_All[i, ]$cancer_label,
  
    AD_treat,
    #AD_treat_Lower,
    #AD_treat_Upper,
    AD_prev,
    #AD_prev_Lower,
    #AD_prev_Upper,
    AD_unavoid,
    #AD_unavoid_Lower,
    #AD_unavoid_Upper,
    Simulated_Data_PAF_All[i,]$total_overall,
    Simulated_Data_PAF_All[i,]$af.comb
    )
}



colnames(Avoidable_Deaths_Simulated_All)<-c("country_code","country_label","age_cat","age","cancer_code","cancer",   
                                            "AD_treat",
                            #"AD_treat_Lower", 
                            #"AD_treat_Upper",
                            "AD_prev",
                            #  "AD_prev_Lower",
                            #  "AD_prev_Upper",
                            "AD_unavoid",
                            # "AD_unavoid_Lower",
                            #  "AD_unavoid_Upper",
                            "total_overall",
                            "af.comb"
                            )



#The sum of the total deaths are more than the total number of cases in some places

#Mortality incidence ratios modified here. 
MIR_Age_Cats_2 <-MIR_Age_Cats%>%
  select(-country_label, -cancer_label)


MIR_Globocan_2 <-MIR_Globocan%>%
  select(-country_label, -cancer_label)



Avoidable_Deaths_Simulated_All<-Avoidable_Deaths_Simulated_All%>%
  as.data.frame()%>%
  mutate(AD_prev=as.numeric(as.character(AD_prev)))%>%
  mutate(AD_unavoid=as.numeric(as.character(AD_unavoid)))%>%
  mutate(total_overall=as.numeric(as.character(total_overall)))%>%
  mutate(AD_treat=as.numeric(as.character(AD_treat)))%>%
  mutate(AD_sum=AD_prev + AD_unavoid + AD_treat)%>%
  mutate(cancer_code=as.numeric(cancer_code))%>%
  # filter(total_overall<AD_sum)%>%
  as.data.frame()%>%distinct()%>%
  mutate(country_code=as.integer(country_code))

Avoidable_Deaths_simulated_All2<-Avoidable_Deaths_Simulated_All%>%
  left_join(MIR_Globocan_2 )


Avoidable_Deaths_Simulated_All_overall<-Avoidable_Deaths_Simulated_All%>%
  mutate(age_cat="Overall")%>%
  group_by(country_code, cancer_code, age_cat)%>%
  mutate(total_overall=sum(total_overall))%>%
  mutate(AD_prev= sum(AD_prev))%>%
  mutate(AD_treat = sum(AD_treat))%>%
  mutate(AD_unavoid = sum(AD_unavoid))%>%
  mutate(AD_sum=sum(AD_sum))%>%
  select(-af.comb, -age)%>%
  distinct()%>%
  as.data.frame()


Avoidable_Deaths_Simulated_All_age_cat <- Avoidable_Deaths_Simulated_All%>%
  ungroup()%>%
  group_by(country_code, cancer_code, age_cat)%>%
  mutate(AD_prev= sum(AD_prev))%>%
  mutate(AD_treat = sum(AD_treat))%>%
  mutate(AD_unavoid = sum(AD_unavoid))%>%
  mutate(total_overall=sum(total_overall))%>%
  full_join(Avoidable_Deaths_Simulated_All_overall)%>%
  select(-age, -af.comb)%>%
  mutate(AD_sum=AD_prev + AD_unavoid + AD_treat)%>%
  distinct()%>%
  arrange(cancer, age_cat)%>%
  as.data.frame()%>%
  left_join(MIR_Age_Cats_2, 
            by=c("cancer_code", "age_cat", "country_code"))%>%
  select(-X)%>%
  distinct()


#The AD by age groups (larger vs smaller age categories)


Avoidable_Deaths_Simulated_All
Avoidable_Deaths_Simulated_All_age_cat


#writing the files
write.csv(Simulated_Data_PAF_All, "~/Documents/R_Projects/Data/NS_Simulated_All_Countries.csv")
write.csv(Avoidable_Deaths_Simulated_All, "~/Documents/R_Projects/Data/AD_Simulated_All_Countries.csv")
write.csv(Avoidable_Deaths_Simulated_All_age_cat, "~/Documents/R_Projects/Data/AD_Simulated_All_Countries_age_cat.csv")
