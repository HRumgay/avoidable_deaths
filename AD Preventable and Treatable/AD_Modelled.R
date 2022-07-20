
###############################################
#
# Net survival and Avoidable deaths - Simulated data
#  Date: 17/07/2022
# Version 2.3
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


# ES_missing<-ES2%>%
#   filter(is.na(ES))%>%
#   select(country_code)%>%distinct()
# 

#Combining datasets at the time point of interest 5 years and combining to HDI

#check country_code for China in HDI dataset and Thailand/Israel datasets match up

# hvh_HDI<-Israel%>%filter(time==5)%>% #Upper survival values
#   left_join(HDI, by="country_code")%>%
#   select(-c(country_label, hdi_rank, year, X))%>%
#   filter(hdi_group%in% c(3,4))
# 
# lm_HDI<-Thailand%>%filter(time==5)%>% #lower HDI survival values
#   left_join(HDI, by="country_code")%>%
#   select(-c(country_label, hdi_rank, year,X.1, X))%>%
#   #filter(hdi_group%in% c(1,2))%>%
#   filter(!hdi_group%in%c(3,4))
# 
# lm_HDI%>%summarize(country_name,country_code, hdi_group)%>%distinct()%>%
#   filter(!hdi_group%in%c(3,4)) #includes low, medium and missing HDI groups

#new combined data set of survival

Survival_Modelled


# Anchored and combined data set at t=5 with anchored values from Israel and Thailand

popmort<-popmort2%>%
  filter(sex!=0)%>%
  filter(year==2015)%>%
  mutate(mx=1-prob)%>%
  mutate(country_code=as.numeric(country_code))%>%
  group_by(country_code,age,year)%>%
  # mutate(mx= case_when(cases!=0 ~ sum(mx*cases)/sum(cases),
  #                           cases==0 ~    mx))%>%
  # mutate(prob= case_when(cases!=0 ~ sum(prob*cases)/sum(cases),
  #                           cases==0 ~    prob))%>%
  summarize(country_label, 
            country_code,
            age_label)%>% #This needs to be adjusted with population weights
  as.data.frame()%>%
  distinct()



countries_5y<-Survival_Modelled%>%
  mutate(rel_surv=case_when(rel_surv>1~ 1,
                            rel_surv<=1~ rel_surv))%>%
  # rename("country_label"="country_name")%>%
  mutate(country_label = str_remove( country_label,'"'))%>%
  mutate(country_label = str_remove( country_label,"'"))%>%
  mutate(country_label = str_remove( country_label,"`"))%>%
  mutate(country_label = str_remove( country_label,'"'))%>%
  arrange(country_label)%>%
  #left_join(popmort,by=c("age"="age","country_code"="country_code"))%>%
  #select(-country_label)%>%xx
  distinct()



Countries_Simulated <-countries_5y%>%
  mutate(age = case_when(
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
    age>=16 ~ 16,
  ))%>%
  mutate(
    age_cat = case_when(
      age>=4 & age<14 ~ "15-64",
      age>=14 ~ "65-99",
      age<4 ~"0-15"
    ))%>%
  filter(age_cat!="0-15")%>%
  group_by(country_label,cancer_label,age_cat)%>%
  summarize(country_code,country_label, cancer_code, cancer_label,
            age, age_cat,
            rel_surv, hdi_group)%>%
  as.data.frame()


# Countries_Simulated_Overall<-Countries_Simulated%>%
#   mutate(age_cat="Overall")%>%
#   group_by(country_name,cancer_label)%>%
#   summarize(country_code,country_name, cancer_code, cancer_label,age, 
#             age_cat,rel_surv,mx)


simulated_overall<-Countries_Simulated%>%
  # full_join(Countries_Simulated_Overall)%>% 
  as.data.frame()%>%
  droplevels()%>%
  group_by(country_label,cancer_label, age_cat,age)%>%
  summarize(country_code,country_label, cancer_code, cancer_label,rel_surv,
            age_cat, age, hdi_group)%>%
  distinct()%>%
  arrange(country_label,cancer_label, age)



#PAF combinations

PAFs_age_Cat <- PAFs%>%
  mutate(age = case_when(
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
  mutate(ES= case_when(cases!=0 ~ sum(ES*cases, na.rm=T)/sum(cases, na.rm=T),
                       cases==0 ~ ES)) %>%
  summarize(country_code,country_label, cancer_code, cancer_label,
            age, age_cat, 
            cases=sum(cases, na.rm=T),
            cases.prev=sum(cases.prev, na.rm=T), 
            cases.notprev=sum(cases.notprev, na.rm=T),
            # af.comb= sum(cases.prev)/sum(cases),
            af.comb= case_when(cases!=0 ~  sum(cases.prev, na.rm=T)/sum(cases, na.rm=T),
                               cases.prev==0 ~ 0),
            total_overall=sum(cases, na.rm=T),
            ES)%>% #Summarizing by sex
  mutate(ES= case_when(cases!=0 ~ sum(ES*cases, na.rm=T)/sum(cases, na.rm=T),
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

PAFs2 <- PAFs_age_Cat%>%
  # full_join(PAFS_Overall)%>%
  as.data.frame()%>%
  droplevels()%>%
  group_by(country_code,cancer_code,cancer_label, age_cat,age)%>%
  mutate(total_age_prev=sum(cases.prev, na.rm=T))%>%
  as.data.frame()%>%
  #mutate(af.comb.agecat=total_age_prev/total_overall)%>%
  group_by(country_code,cancer_code,age)%>%
  summarize(country_label, cancer_label,
            age_cat, age,  total_overall,cases,
            cases.prev, ES, af.comb)%>%
  distinct()%>%
  arrange(cancer_label,
          age)%>%
  select(-cancer_label,-country_label)


Simulated_Data_PAF_1 <- simulated_overall%>%
  ungroup()%>%
  filter(age_cat!="Overall")%>%
  left_join(PAFs2,by=c("country_code"="country_code",
                       #"country_label"="country_label",
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
  mutate(af.comb=case_when(cases!=0 ~ sum(cases.prev, na.rm=T)/sum(cases, na.rm=T),
                           cases==0  ~    0))%>%
  mutate(rel_surv=case_when(cases!=0 ~ sum(rel_surv*cases, na.rm=T)/sum(cases, na.rm=T),
                            cases==0  ~    rel_surv))%>%
  summarize(country_code, 
            country_label, 
            cancer_code, cancer_label,
            age_cat, age, total_overall,
            rel_surv,
            af.comb,
            rel_surv,
            ES,
            #   Expected_5_year_surv_mx,
            total_overall,
            hdi_group)%>%
  droplevels()%>%
  #select(-age)%>%
  distinct()%>%
  as.data.frame()




# Simulated_Data_PAF_2 <- simulated_overall%>%
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
  left_join(Reference_Survival, by=c("age","cancer_code")) 


#Avoidable deaths

#Three AD calcs 

#first need to make sure data is in right format (numeric columns)

#Applying the equation from Rutherford 2015 for AD. Needs to be updated to have scaled relative survival
Avoidable_Deaths_Simulated_All2 <- matrix(ncol = 12, nrow = nrow(Simulated_Data_PAF_All)) #AD(t)

#NS_Ref<-0.9 #Reference countries survival


for (i in 1:nrow(Avoidable_Deaths_Simulated_All2)) {
  
  ES <- Simulated_Data_PAF_All[i,]$ES #Crude calculations of expected survival
  
  #Preventable deaths
  AD_prev <-     (Simulated_Data_PAF_All[i,]$af.comb) * 
    Simulated_Data_PAF_All[i,]$total_overall * 
    (1 - Simulated_Data_PAF_All[i,]$rel_surv) *
    (ES) 
  #AD_prev_Lower<-(Simulated_Data_PAF_All[i,]$af.comb.agecat)*Simulated_Data_PAF_All[i,]$total_overall*(1-Simulated_Data_PAF_All[i,]$NS_Lower_CI)*ES
  #AD_prev_Upper<-(Simulated_Data_PAF_All[i,]$af.comb.agecat)*Simulated_Data_PAF_All[i,]$total_overall*(1-Simulated_Data_PAF_All[i,]$NS_Upper_CI)*ES
  
  # #Avoidable deaths (treatable: #check what the lower CI is called in the previous data frame
  
  AD_treat <- (1-Simulated_Data_PAF_All[i,]$af.comb) *
    (Simulated_Data_PAF_All[i,]$total_overall) *
    (Simulated_Data_PAF_All[i,]$surv_ref-Simulated_Data_PAF_All[i,]$rel_surv) *
    (ES)
  #AD_treat_Lower<-(0.9-Simulated_Data_PAF_All[i,]$NS_Lower_CI)*ES*(1-Simulated_Data_PAF_All[i,]$af.comb.agecat)*Simulated_Data_PAF_All[i,]$total_overall
  #AD_treat_Upper<-(0.9-Simulated_Data_PAF_All[i,]$NS_Upper_CI)*ES*(1-Simulated_Data_PAF_All[i,]$af.comb.agecat)*Simulated_Data_PAF_All[i,]$total_overall
  
  
  #Deaths not avoidable 
  
  AD_unavoid <- (1-Simulated_Data_PAF_All[i,]$af.comb)* 
    Simulated_Data_PAF_All[i,]$total_overall*
    (1-Simulated_Data_PAF_All[i,]$surv_ref*(ES))
  #AD_unavoid_Lower<-(1-Simulated_Data_PAF_All[i,]$af.comb.agecat)*Simulated_Data_PAF_All[i,]$total_overall*(1-Simulated_Data_PAF_All[i,]$NS_Lower_CI*ES)
  #AD_unavoid_Upper<-(1-Simulated_Data_PAF_All[i,]$af.comb.agecat)*Simulated_Data_PAF_All[i,]$total_overall*(1-Simulated_Data_PAF_All[i,]$NS_Upper_CI*ES)
  
  
  Avoidable_Deaths_Simulated_All2[i, ] <- c(Simulated_Data_PAF_All[i, ]$country_code,
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
                                            Simulated_Data_PAF_All[i,]$af.comb,
                                            Simulated_Data_PAF_All[i,]$hdi_group
  )
}



colnames(Avoidable_Deaths_Simulated_All2) <- c("country_code","country_label","age_cat","age","cancer_code","cancer",   
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
                                             "af.comb",
                                             "hdi_group"
)



#The sum of the total deaths are more than the total number of cases in some places

#Mortality incidence ratios modified here. 
# MIR_Age_Cats_2 <- MIR_Age_Cats%>%
#   select(-country_label, -cancer_label)
# 
# 
# MIR_Globocan_2 <- MIR_Globocan%>%
#   select(-country_label, -cancer_label)
# 


#Globocan population data 

pop20202 <- pop2020%>%as.data.frame()%>%
  filter(sex!=0)%>%
  select(-sex)%>%
  mutate(age = case_when(
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
  group_by(country_code, age)%>%
  mutate(py=sum(py))%>%
  distinct()%>%
  select(-country_label)



#Weights
w2 <- c(12000,10000,9000,9000,8000,8000,6000,6000,6000,6000,5000,4000,4000,3000,2000,1000,500,500) #SEGI_pop weights
age <- c(1:18)
weights2 <- data.frame(w2, age)


weights2 <- weights2%>%mutate(age = case_when(
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
  mutate(total=sum(w2))%>%
  group_by(age)%>%
  mutate(w=sum(w2)/total)%>%
  select(-w2, -total)%>%
  distinct()


Avoidable_Deaths_Simulated_All<- Avoidable_Deaths_Simulated_All2%>%
  as.data.frame()%>%
  mutate(age=as.numeric(as.character(age)))%>%
  mutate(AD_prev=as.numeric(as.character(AD_prev)))%>%
  mutate(AD_unavoid=as.numeric(as.character(AD_unavoid)))%>%
  mutate(total_overall=as.numeric(as.character(total_overall)))%>%
  mutate(AD_treat=as.numeric(as.character(AD_treat)))%>%
  mutate(AD_sum=AD_prev + AD_unavoid + AD_treat)%>%
  mutate(cancer_code=as.numeric(cancer_code))%>%
  # filter(total_overall<AD_sum)%>%
  as.data.frame()%>%distinct()%>%
  mutate(country_code=as.integer(country_code))%>%
  left_join(weights2)%>%
  left_join(pop20202)



Avoidable_Deaths_Simulated_All_overall <- Avoidable_Deaths_Simulated_All%>%
  mutate(age_cat="Overall")%>%
  group_by(country_code, cancer_code, age_cat)%>%
  select(-total_overall)%>%
  #mutate(total_overall=sum(total_overall,na.rm=T))%>%
  mutate(AD_prev = sum(AD_prev,na.rm=T))%>%
  mutate(AD_treat = sum(AD_treat,na.rm=T))%>%
  mutate(AD_unavoid = sum(AD_unavoid,na.rm=T))%>%
  mutate(AD_sum = sum(AD_sum,na.rm=T))%>%
  mutate(py=sum(py))%>%
  select(-af.comb, -age)%>%
  ungroup()%>%
  group_by(cancer_code, country_code,age_cat)%>%
  mutate(w=sum(w))%>%
  distinct()%>%
  as.data.frame()


Avoidable_Deaths_Simulated_All_age_cat <- Avoidable_Deaths_Simulated_All%>%
  ungroup()%>%
  group_by(country_code, cancer_code, age_cat)%>%
  select(-total_overall)%>%
  #mutate(total_overall=sum(total_overall,na.rm=T))%>%
  mutate(AD_prev = sum(AD_prev,na.rm=T))%>%
  mutate(AD_treat = sum(AD_treat,na.rm=T))%>%
  mutate(AD_unavoid = sum(AD_unavoid,na.rm=T))%>%
  mutate(AD_sum = sum(AD_sum,na.rm=T))%>%
  mutate(py=sum(py))%>%
  select(-af.comb, -age)%>%
  ungroup()%>%
  group_by(cancer_code, country_code,age_cat)%>%
  mutate(w=sum(w))%>%
  distinct()%>%
  full_join(Avoidable_Deaths_Simulated_All_overall)%>%
  as.data.frame()




# age standardized proportions 


AD_prop_Age_stand <- Avoidable_Deaths_Simulated_All%>%
  group_by(country_code, cancer_code,age)%>%
  #mutate(pAD_treat=AD_treat/total_overall*w)%>%
  #mutate(pAD_prev=AD_prev/total_overall*w)%>%
  #  mutate(pAD_unavoid=AD_unavoid/total_overall*w)%>%
  mutate(AD_treat = case_when(py!=0 ~ AD_treat/py*w*10**5, #change to py (population size)
                              py==0 ~ 0))%>%
  mutate(AD_prev = case_when(py!=0 ~ AD_prev/py*w*10**5,
                             py==0 ~ 0))%>%
  mutate(AD_unavoid = case_when(py!=0 ~ AD_unavoid/py*w*10**5,
                                py==0 ~ 0))%>%
  select(country_code, country_label, cancer_code, cancer, age, 
         AD_treat, #pAD_treat, 
         AD_prev, #pAD_prev,
         AD_unavoid #pAD_unavoid
  )



ADprop3 <- Avoidable_Deaths_Simulated_All%>%
  group_by(country_code, cancer_code,age)%>%
  ungroup()%>%
  mutate(AD_treat_prev = case_when(py!=0 ~ (AD_treat + AD_prev)/ py*w*10**5,
                                   py==0 ~ 0))%>%
  mutate(AD_treat = case_when(py!=0 ~ AD_treat/py*w*10**5,
                              py==0 ~ 0))%>%
  mutate(AD_prev = case_when(py!=0 ~ AD_prev/py*w*10**5,
                             py==0 ~ 0))%>%
  mutate(AD_unavoid = case_when(py!=0 ~ AD_unavoid/py*w*10**5,
                                py==0 ~ 0))%>%
  ungroup()%>%
  group_by(country_code, cancer_code,age_cat)%>%
  mutate(AD_treat=sum(AD_treat,na.rm=T))%>%
  mutate(AD_prev=sum(AD_prev,na.rm=T))%>%
  mutate(AD_unavoid=sum(AD_unavoid,na.rm=T))%>%
  mutate(AD_treat_prev=sum(AD_treat_prev,na.rm=T))%>%
  ungroup()%>%
  group_by(country_code, cancer_code,age_cat)%>%
  select(country_code, country_label, cancer_code, cancer, age_cat, 
         AD_treat, 
         AD_prev, 
         AD_treat_prev, 
         AD_unavoid, 
         hdi_group)%>%
  as.data.frame%>%distinct()




ADprop4 <- Avoidable_Deaths_Simulated_All%>%
  ungroup()%>%
  mutate(age_cat="Overall")%>%
  group_by(country_code, cancer_code,age)%>%
  mutate(AD_treat=sum(AD_treat,na.rm=T))%>%
  mutate(AD_prev=sum(AD_prev,na.rm=T))%>%
  mutate(AD_unavoid=sum(AD_unavoid,na.rm=T))%>%
  mutate(AD_treat_prev = case_when(py!=0 ~ (AD_treat + AD_prev)/ py*w*(10**5),
                                   py==0 ~ 0))%>%
  mutate(AD_treat = case_when(py!=0 ~ AD_treat/py*w*(10**5),
                              py==0 ~ 0))%>%
  mutate(AD_prev = case_when(py!=0 ~ AD_prev/py*w*(10**5),
                             py==0 ~ 0))%>%
  mutate(AD_unavoid = case_when(py!=0 ~ AD_unavoid/py*w*(10**5),
                                py==0 ~ 0))%>%
  ungroup()%>%
  group_by(country_code, cancer_code,age_cat)%>%
  mutate(AD_treat=sum(AD_treat,na.rm=T))%>%
  mutate(AD_prev=sum(AD_prev,na.rm=T))%>%
  mutate(AD_treat_prev=sum(AD_treat_prev,na.rm=T))%>%
  mutate(AD_unavoid=sum(AD_unavoid,na.rm=T))%>%
  select(country_code, country_label, cancer_code, cancer, age_cat, 
         AD_treat, 
         AD_prev, 
         AD_treat_prev, 
         AD_unavoid, 
         hdi_group)%>%
  as.data.frame%>%distinct()



AD_prop_Age_stand_age_cat <- ADprop3%>%full_join(ADprop4)%>%
  arrange(country_label, cancer_code, age_cat)



# 


#writing the files
write.csv(Simulated_Data_PAF_All, "~/Documents/R_Projects/Data/NS_Simulated_All_Countries.csv")
write.csv(Avoidable_Deaths_Simulated_All, "~/Documents/R_Projects/Data/AD_Simulated_All_Countries.csv")
write.csv(Avoidable_Deaths_Simulated_All_age_cat, "~/Documents/R_Projects/Data/AD_Simulated_All_Countries_age_cat.csv")


#HDI 

HDI2 <- HDI%>%select(country_code,hdi_group)

#Data by country, HDI, etc

AD_by_HDI <- Avoidable_Deaths_Simulated_All_age_cat%>%
  as.data.frame()%>%
  # left_join(HDI2, by=c("country_code"))%>%
  select(hdi_group, cancer, cancer_code, AD_treat, AD_prev, 
         AD_unavoid, AD_sum, age_cat, w)%>%
  mutate(AD_treat=as.numeric(AD_treat))%>%
  mutate(AD_prev=as.numeric(AD_prev))%>%
  mutate(AD_unavoid=as.numeric(as.character(AD_unavoid)))%>%
  mutate(AD_sum=as.numeric(as.character(AD_sum)))%>%
  #mutate(total_overall=as.numeric(as.character(total_overall)))%>%
  #mutate(cancer_code=as.numeric(cancer_code))%>%
  filter(!is.na(AD_treat))%>%
  #select(-total_overall)%>%
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  mutate(hdi_group=as.numeric(hdi_group))%>%
  group_by(cancer_code, age_cat,hdi_group)%>%
  mutate(AD_treat=sum(AD_treat))%>%
  mutate(AD_prev=sum(AD_prev))%>%
  mutate(AD_unavoid=sum(AD_unavoid))%>%
  mutate(AD_sum=sum(AD_sum))%>%
 # mutate(total_overall=sum(total_overall))%>%
  ungroup()%>%
  distinct()%>%
  as.data.frame()

AD_by_HDI

AD_by_HDI_overall <- AD_by_HDI%>%filter(age_cat=="Overall")%>%select(-age_cat)






AD_by_HDI_all <- AD_by_HDI_overall%>%
  ungroup()%>%
  select(-w,-total_overall)%>%
  group_by(hdi_group)%>%
  mutate(AD_treat=sum(AD_treat,na.rm=T))%>%
  mutate(AD_prev=sum(AD_prev,na.rm=T))%>%
  mutate(AD_unavoid=sum(AD_unavoid,na.rm=T))%>%
  mutate(AD_sum=sum(AD_sum,na.rm=T))  %>%
  #mutate(total_overall=sum(total_overall))%>%
  #mutate(total_overall=sum(total_overall,na.rm=T))  %>%
  mutate(cancer="All Cancer Sites")%>%
  mutate(cancer_code=1000)%>%
  ungroup()%>%
  distinct()%>%
  as.data.frame()%>%
  mutate(total_deaths=sum(AD_sum))






#Calculating by region. Need a file that links countries to region 
HDI_Region_Mapping2 <- HDI_Region_Mapping%>%select(-country_label)%>%
  filter(area<19)

areas <- HDI_Region_Mapping%>%
  filter(country_code>=910& country_code<=931 | country_code==905 | country_code==906| country_code==954| country_code==957 )%>%
  select(area, country_label)



#by region
AD_Region <- Avoidable_Deaths_Simulated_All_age_cat%>%
  left_join(HDI_Region_Mapping2, by=c("country_code"))%>%
  filter(age_cat=="Overall")%>%
  mutate(AD_treat_prev=AD_treat+AD_prev)%>%
  ungroup()%>%
  mutate(total_deaths=sum(AD_prev, AD_unavoid, AD_treat,na.rm=T))%>%
  select(-hdi_group,-continent, -country_label,-country_code,  -AD_sum)%>%
  group_by(area)%>%
  mutate(AD_treat_prev=sum(AD_treat_prev,na.rm=T))%>%
  mutate(AD_treat=sum(AD_treat,na.rm=T))%>%
  mutate(AD_prev=sum(AD_prev,na.rm=T))%>%
  mutate(AD_unavoid=sum(AD_unavoid,na.rm=T))%>%
  mutate(pAD_treat=AD_treat/total_deaths)%>%
  mutate(pAD_prev=AD_prev/total_deaths)%>%
  mutate(pAD_unavoid=AD_unavoid/total_deaths)%>%
  mutate(pAD_treat_prev=AD_treat_prev/total_deaths)%>%
  #mutate(total_overall=sum(total_overall,na.rm=T))  %>%
  mutate(cancer="All Cancer Sites")%>%
  mutate(cancer_code=1000)%>%
  left_join(areas)%>%
  ungroup()%>%
  distinct()%>%
  as.data.frame()


#age standardizing by region - aggregate by region and then age standardize

AD_prop_Age_stand_age_cat_region <- Avoidable_Deaths_Simulated_All%>%
  left_join(HDI_Region_Mapping2, by=c("country_code"))%>%
  select(-hdi_group,-continent, 
         -country_label,-country_code,
         -total_overall, -AD_sum, 
         -af.comb)%>%
  ungroup()%>%
  mutate(age_cat="Overall")%>%
  mutate(cancer="All Cancer Sites")%>%
  mutate(cancer_code=1000)%>%
  group_by(area,age)%>%
  mutate(py=sum(py))%>%
  mutate(AD_treat=sum(AD_treat,na.rm=T))%>%
  mutate(AD_prev=sum(AD_prev,na.rm=T))%>%
  mutate(AD_unavoid=sum(AD_unavoid,na.rm=T))%>%
  ungroup%>%
  group_by(area)%>%
  mutate(AD_treat_prev = case_when(py!=0 ~ (AD_treat + AD_prev)/ py*w*10**5,
                                   py==0 ~ 0))%>%
  mutate(AD_treat = case_when(py!=0 ~ AD_treat/py*w*10**5,
                              py==0 ~ 0))%>%
  mutate(AD_prev = case_when(py!=0 ~ AD_prev/py*w*10**5,
                             py==0 ~ 0))%>%
  mutate(AD_unavoid = case_when(py!=0 ~ AD_unavoid/py*w*10**5,
                                py==0 ~ 0))%>%
  select(-w, -py)%>%
  distinct()%>%
  mutate(AD_treat=sum(AD_treat,na.rm=T))%>%
  mutate(AD_prev=sum(AD_prev,na.rm=T))%>%
  mutate(AD_treat_prev=sum(AD_treat_prev,na.rm=T))%>%
  mutate(AD_unavoid=sum(AD_unavoid,na.rm=T))%>%
  ungroup()%>%
  left_join(areas)%>%
  select(country_label,area, cancer_code, cancer,
         AD_treat, 
         AD_prev, 
         AD_treat_prev, 
         AD_unavoid)%>%
  as.data.frame%>%
  distinct()










#Table 1 in the manuscript

# Gives us number, percentage of total deaths 
table_1_1 <- Avoidable_Deaths_Simulated_All_age_cat  %>%
  select(-country_label, -country_code, -cancer_code, -cancer)%>%
  filter(age_cat=="Overall")%>%
  ungroup()%>%
  mutate(total_deaths=sum(AD_treat+AD_prev+AD_unavoid))%>%
  mutate(AD_treat_prev=sum(AD_treat+AD_prev,na.rm=TRUE))%>%
  mutate(AD_treat=sum(AD_treat,na.rm=TRUE))%>%
  mutate(AD_prev=sum(AD_prev,na.rm=TRUE))%>%
  mutate(AD_unavoid=sum(AD_unavoid,na.rm=TRUE))%>%
  ungroup()%>%
  mutate(pAD_treat=AD_treat/total_deaths)%>%
  mutate(pAD_prev=AD_prev/total_deaths)%>%
  mutate(pAD_unavoid=AD_unavoid/total_deaths)%>%
  mutate(pAD_treat_prev=AD_treat_prev/total_deaths)%>%
  select(AD_treat, AD_prev, AD_treat_prev, AD_unavoid, total_deaths, 
         pAD_treat, pAD_prev,pAD_treat_prev, pAD_unavoid)%>%
  distinct()

#Overall ASR
table_1_2<-Avoidable_Deaths_Simulated_All%>%
  select(-country_label, -country_code, -cancer_code, -cancer,-hdi_group)%>%
  group_by(age)%>%
  mutate(py=sum(py))%>%
  mutate(AD_treat=sum(AD_treat,na.rm=TRUE))%>%
  mutate(AD_prev=sum(AD_prev,na.rm=TRUE))%>%
  mutate(AD_unavoid=sum(AD_unavoid,na.rm=TRUE))%>%
  mutate(AD_treat_prev = case_when(py!=0 ~ (AD_treat + AD_prev)/ py*w*10**5,
                                   py==0 ~ 0))%>%
  mutate(AD_treat = case_when(py!=0 ~ AD_treat/py*w*10**5,
                              py==0 ~ 0))%>%
  mutate(AD_prev = case_when(py!=0 ~ AD_prev/py*w*10**5,
                             py==0 ~ 0))%>%
  mutate(AD_unavoid = case_when(py!=0 ~ AD_unavoid/py*w*10**5,
                                py==0 ~ 0))%>%
  ungroup()%>%
  select(AD_treat, AD_prev, AD_treat_prev,
         AD_unavoid)%>%
  distinct()%>%
  ungroup()%>%
  mutate(AD_treat=sum(AD_treat,na.rm=TRUE))%>%
  mutate(AD_prev=sum(AD_prev,na.rm=TRUE))%>%
  mutate(AD_treat_prev=sum(AD_treat_prev,na.rm=TRUE))%>%
  mutate(AD_unavoid=sum(AD_unavoid,na.rm=TRUE))%>%
  distinct()




#By country AND cancer site

# AD by country and cancer site

Avoidable_Deaths_Simulated_All
Avoidable_Deaths_Simulated_All_age_cat
Avoidable_Deaths_Simulated_All_age_cat_overall<-Avoidable_Deaths_Simulated_All_age_cat%>% #Object to plot overall age standardized on world maps 
  filter(age_cat=="Overall")
# age standardized 

AD_prop_Age_stand #for reference...
AD_prop_Age_stand_age_cat
AD_prop_Age_stand_age_cat_overall<-AD_prop_Age_stand_age_cat%>% #Object to plot overall age standardized on world maps 
  filter(age_cat=="Overall")
#By HDI

AD_by_HDI
AD_by_HDI_all

#Age standardized


#By region
AD_Region

#Age standardized
AD_prop_Age_stand_age_cat_region

#World total

table_1_1

table_1_2



#ASR with package better? 
library(Rcan)

data(csu_registry_data_1)

# Age standardized rate (ASR) with no missing age cases.
result <- Avoidable_Deaths_Simulated_All%>%
  ungroup()%>%
  #mutate(cancer_code=1000)%>%
  #mutate(country_code=0)%>%
  mutate(AD_treat=as.numeric(AD_treat))%>%
  select(
    "age", "AD_treat", "py","country_code", "cancer_code"
  )%>%
  csu_asr("age", "AD_treat", "py",
          group_by = c("country_code", "cancer_code"),
          var_age_group = c("country_code","cancer_code" ),
          pop_base = "SEGI")

