
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

# Expected survival

ES3<-ES_dt%>%
  filter(time==1000)%>%
  select(-time)%>%
  rename("ES"="SurvExp")


# Imputing missing countries 

# For For all the French overseas and territories (Polynesia, Guyana, Guadeloupe, Martinique, Reunion, New Caledonia)
ES_France1<-ES3%>%filter(country_code==250)%>%mutate(ES3=ES) %>%select(-ES) 
ES_France2<-ES_France1%>%mutate(country_code=254)
ES_France3<-ES_France1%>%mutate(country_code=258)
ES_France4<-ES_France1%>%mutate(country_code=312)
ES_France5<-ES_France1%>%mutate(country_code=474)
ES_France6<-ES_France1%>%mutate(country_code=540)
ES_France7<-ES_France1%>%mutate(country_code=638)

# Palestine? - Imputed by HDI 0.690 average plus minus 0.1 HDI

HDI_countries_palestine<-Survival_Modelled%>%
  filter(0.690-0.05<=hdi_value &hdi_value<=0.690+0.05)%>%
  select(country_code)%>%
  distinct()

ES_palestine<-ES3%>%filter(country_code%in%HDI_countries_palestine$country_code)%>%
  group_by(age)%>%
  mutate(ES=mean(ES, na.rm=T))%>%
  mutate(country_code=275)%>%
  distinct()%>%
  mutate(ES3=ES)%>%select(-ES)


# Western Sahara? - Imputed by HDI 0.690 average plus minus 0.1 HDI

HDI_countries_palestine<-Survival_Modelled%>%
  filter(0.690-0.05<=hdi_value &hdi_value<=0.690+0.05)%>%
  select(country_code)%>%
  distinct()

ES_palestine<-ES3%>%filter(country_code%in%HDI_countries_palestine$country_code)%>%
  group_by(age)%>%
  mutate(ES=mean(ES, na.rm=T))%>%
  mutate(country_code=275)%>%
  distinct()%>%
  mutate(ES3=ES)%>%select(-ES)




# Puerto Rico and Guam - Using US
ES_PR<-ES3%>%filter(country_code==840)%>%mutate(ES3=ES)%>%select(-ES)%>%mutate(country_code=630)

ES_Guam<-ES3%>%filter(country_code==840)%>%mutate(ES3=ES)%>%select(-ES)%>%mutate(country_code=316)

ES_Additional<-ES_France2%>%
  full_join(ES_France3)%>%
  full_join(ES_France4)%>%
  full_join(ES_France5)%>%
  full_join(ES_France6)%>%
  full_join(ES_France7)%>%
  full_join(ES_PR)%>%
  full_join(ES_palestine)%>%
  full_join(ES_Guam)

ES2<-ES3%>%
  group_by(country_code, age)%>%
  left_join(ES_Additional, by=c("country_code", "age", "sex"))%>%
  mutate(ES=case_when(
    country_code%in%c(254, 258, 312, 474, 540, 638, # French territories
                      630, 275, 316 #puerto rico
    ) ~ ES3, 
    TRUE~ ES))%>%
  select(-ES3)





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



# PAF combinations

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
  group_by(country_label,cancer_label, age,sex) %>%
  mutate(ES= case_when(cases!=0 ~ sum(ES*cases, na.rm=T)/sum(cases, na.rm=T),
                       cases==0 ~ ES)) %>%
  summarize(country_code,country_label, cancer_code, cancer_label,
            age, sex,age_cat, 
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
  group_by(country_code,cancer_code,cancer_label, age_cat,age,sex)%>%
  mutate(total_age_prev=sum(cases.prev, na.rm=T))%>%
  as.data.frame()%>%
  #mutate(af.comb.agecat=total_age_prev/total_overall)%>%
  group_by(country_code,cancer_code,age,sex)%>%
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
  group_by(country_code,cancer_code, age, sex)%>%
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
  arrange(country_label,cancer_code,age,sex)%>%
  left_join(Reference_Survival, by=c("age","cancer_code")) 


# Avoidable deaths

# Three AD calcs 

# first need to make sure data is in right format (numeric columns)

# Applying the equation from Rutherford 2015 for AD.

Avoidable_Deaths_Simulated_All2<-Simulated_Data_PAF_All%>%
  group_by(country_code, cancer_code, age, sex)%>%
  mutate(AD_prev=af.comb * total_overall * (1 - rel_surv *  ES ))%>%
  mutate(AD_treat=total_overall *(surv_ref-rel_surv) * ES)%>%
  mutate(AD_treat_not_prev=(1-af.comb)* total_overall *(surv_ref-rel_surv) * ES)%>%
  mutate(AD_unavoid = (1-af.comb)*total_overall*(1-surv_ref*ES))%>%
  mutate(Expect_deaths=(1-(rel_surv*ES))*total_overall)%>%
  select("country_code","country_label","age_cat","age","cancer_code","cancer_label",   
         "AD_treat",
         AD_treat_not_prev,
         "AD_prev",
         "AD_unavoid",
         "Expect_deaths",
         "total_overall",
         "af.comb",
         "hdi_group")%>%
  rename("cancer"="cancer_label")

# Globocan population data 

pop20202 <- pop2020%>%
  as.data.frame()%>%
  filter(sex!=0)%>%
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
  group_by(country_code, age,sex)%>%
  mutate(py=sum(py))%>%
  distinct()%>%
  select(-country_label)




# Weights
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
  filter(age>3) %>% 
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
  mutate(AD_treat_not_prev=as.numeric(as.character(AD_treat_not_prev)))%>%
  mutate(AD_sum=AD_prev + AD_unavoid + AD_treat_not_prev)%>%
  mutate(cancer_code=as.numeric(cancer_code))%>%
  as.data.frame()%>%distinct()%>%
  mutate(country_code=as.integer(country_code))%>%
  left_join(weights2)%>%
  left_join(pop20202, c("sex", "country_code", "age"))%>%
  mutate(AD_treat = case_when(AD_treat<0 ~ 0, 
                              # Numerical calculation error causes the countries which are references  to go slightly negative. 
                              # By definition this is zero This is rounded to 0
                              TRUE ~ AD_treat))


## added new ASR code below
Avoidable_Deaths_Simulated_All_overall <- Avoidable_Deaths_Simulated_All%>%
  mutate(age_cat="Overall")%>%
  group_by(country_code, cancer_code, age_cat)%>%
  mutate(AD_prev.asr=sum(AD_prev/py*100000*w), #ASR calculation here
         AD_treat.asr=sum(AD_treat/py*100000*w),
         AD_sum.asr=sum(AD_sum/py*100000*w),
         AD_treat_not_prev.asr=sum(AD_treat_not_prev/py*100000*w),
         AD_unavoid.asr=sum(AD_unavoid/py*100000*w),
         Expect_deaths.asr=sum(Expect_deaths/py*100000*w)) %>% 
  select( -total_overall)%>%
  mutate(AD_prev = sum(AD_prev,na.rm=T))%>%
  mutate(AD_treat = sum(AD_treat,na.rm=T))%>%
  mutate(AD_sum = sum(AD_sum,na.rm=T))%>%
  mutate(AD_treat_not_prev = sum(AD_treat_not_prev,na.rm=T))%>%
  mutate(AD_unavoid = sum(AD_unavoid,na.rm=T))%>%
  mutate(Expect_deaths=sum(Expect_deaths,na.rm=T))%>%
  mutate(py=sum(py))%>%
  select(-af.comb, -age,-sex)%>%
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
  mutate(AD_treat_not_prev = sum(AD_treat_not_prev,na.rm=T))%>%
  mutate(AD_unavoid = sum(AD_unavoid,na.rm=T))%>%
  mutate(Expect_deaths=sum(Expect_deaths,na.rm=T))%>%
  mutate(AD_sum = sum(AD_sum,na.rm=T))%>%
  mutate(py=sum(py))%>%
  select(-af.comb, -age,-sex)%>%
  ungroup()%>%
  group_by(cancer_code, country_code,age_cat)%>%
  mutate(w=sum(w))%>%
  distinct()%>%
  full_join(Avoidable_Deaths_Simulated_All_overall)%>%
  as.data.frame()%>%ungroup()





# HDI 

#HDI2 <- HDI%>%select(country_code,hdi_group)

# Data by country, HDI, etc

AD_by_HDI <- Avoidable_Deaths_Simulated_All_age_cat%>%
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  select(-country_code,-country_label, -py,  hdi_group, cancer, cancer_code, AD_treat, AD_prev, 
         AD_unavoid, AD_sum, age_cat, w)%>%
  
  mutate(AD_treat=as.numeric(AD_treat))%>%
  mutate(AD_treat_not_prev=as.numeric(AD_treat_not_prev))%>%
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
  mutate(AD_treat_prev=(AD_prev+AD_treat_not_prev))%>%
  group_by(cancer_code, age_cat,hdi_group)%>%
  mutate(AD_treat_prev=sum(AD_prev+AD_treat_not_prev))%>%
  mutate(Expect_deaths=sum(Expect_deaths))%>%
  mutate(AD_treat=sum(AD_treat))%>%
  mutate(AD_treat_not_prev=sum(AD_treat_not_prev))%>%

  #mutate(AD_treat_prev=sum(AD_treat,AD_treat_not_prev,na.rm=T))%>%
  mutate(AD_prev=sum(AD_prev))%>%
  mutate(AD_unavoid=sum(AD_unavoid))%>%
  mutate(total_deaths=sum(Expect_deaths))%>%
  # mutate(total_overall=sum(total_overall))%>%
  select(-AD_sum)%>%
  ungroup()%>%
  distinct()%>%
  as.data.frame()


AD_by_HDI

AD_by_HDI_overall <- AD_by_HDI%>%filter(age_cat=="Overall")%>%select(-age_cat)


AD_by_HDI_all <- AD_by_HDI_overall%>%
  ungroup()%>%
  group_by(hdi_group)%>%
  mutate(total_deaths=sum(Expect_deaths,na.rm=T))%>%
  select(-Expect_deaths)%>%
  mutate(AD_treat_prev=sum((AD_treat+AD_treat_not_prev),na.rm=T))%>%


  mutate(AD_treat=sum(AD_treat,na.rm=T))%>%
  mutate(AD_treat_not_prev=sum(AD_treat_not_prev,na.rm=T))%>%
  mutate(AD_prev=sum(AD_prev,na.rm=T))%>%
  mutate(AD_unavoid=sum(AD_unavoid,na.rm=T))%>%
  mutate(pAD_treat=AD_treat/total_deaths)%>%
  mutate(pAD_treat_not_prev=AD_treat_not_prev/total_deaths)%>%
  mutate(pAD_prev=AD_prev/total_deaths)%>%
  mutate(pAD_unavoid=AD_unavoid/total_deaths)%>%
  mutate(pAD_treat_prev=AD_treat_prev/total_deaths)%>%
  #mutate(total_overall=sum(total_overall,na.rm=T))  %>%
  mutate(cancer="All Cancer Sites")%>%
  mutate(cancer_code=1000)%>%
  distinct()%>%
  select(-w)%>%
  ungroup()%>%
  distinct()%>%
  arrange(hdi_group)%>%
  as.data.frame()


# By country for all cancer sites (number and proportion): 

AD_country_all_cancers <- Avoidable_Deaths_Simulated_All_age_cat%>%
  filter(age_cat=="Overall")%>%
  mutate(AD_treat_prev=AD_treat+AD_prev)%>%
  ungroup()%>%
  select(  -hdi_group,  -AD_sum)%>%
  group_by(country_code)%>%
  mutate(total_deaths=sum(AD_prev, AD_unavoid, AD_treat_not_prev,na.rm=T))%>%
  mutate(AD_treat_prev=sum(AD_treat_not_prev,AD_prev,na.rm=T))%>%
  mutate(AD_treat=sum(AD_treat,na.rm=T))%>%
  mutate(AD_treat_not_prev=sum(AD_treat_not_prev,na.rm=T))%>%
  mutate(AD_prev=sum(AD_prev,na.rm=T))%>%
  mutate(AD_unavoid=sum(AD_unavoid,na.rm=T))%>%
  mutate(pAD_treat=AD_treat/total_deaths)%>%
  mutate(pAD_treat_not_prev=AD_treat_not_prev/total_deaths)%>%
  mutate(pAD_prev=AD_prev/total_deaths)%>%
  mutate(pAD_unavoid=AD_unavoid/total_deaths)%>%
  mutate(pAD_treat_prev=AD_treat_prev/total_deaths)%>%
  #mutate(total_overall=sum(total_overall,na.rm=T))  %>%
  mutate(cancer="All Cancer Sites")%>%
  mutate(cancer_code=1000)%>%
  select(-py)%>%
  distinct()%>%
  ungroup()%>%
  distinct()%>%
  as.data.frame()


# By cancer site

AD_cancer <- Avoidable_Deaths_Simulated_All_age_cat%>%
  filter(age_cat=="Overall")%>%
  mutate(AD_treat_prev=AD_treat+AD_prev)%>%
  ungroup()%>%
  select(-hdi_group,  -AD_sum, -w)%>%
  group_by(cancer_code)%>%
  mutate(total_deaths=sum(Expect_deaths,na.rm=T))%>%
  select(-Expect_deaths)%>%
  mutate(AD_treat_prev=sum(AD_treat_not_prev,AD_prev,na.rm=T))%>%
  mutate(AD_treat=sum(AD_treat,na.rm=T))%>%
  mutate(AD_treat_not_prev=sum(AD_treat_not_prev,na.rm=T))%>%
  mutate(AD_prev=sum(AD_prev,na.rm=T))%>%
  #  mutate(Expect_deaths=sum(Expect_deaths,na.rm=T))%>%
  mutate(AD_unavoid=sum(AD_unavoid,na.rm=T))%>%
  mutate(pAD_treat=AD_treat/total_deaths)%>%
  mutate(pAD_treat_not_prev=AD_treat_not_prev/total_deaths)%>%
  mutate(pAD_prev=AD_prev/total_deaths)%>%
  mutate(pAD_unavoid=AD_unavoid/total_deaths)%>%
  mutate(pAD_treat_prev=AD_treat_prev/total_deaths)%>%
  #mutate(total_overall=sum(total_overall,na.rm=T))  %>%
  mutate(country_label="All Countries")%>%
  mutate(country_code=1000)%>%
  select(-py)%>%
  distinct()%>%
  ungroup()%>%
  distinct()%>%
  as.data.frame()


# Calculating by region. Need a file that links countries to region 
HDI_Region_Mapping2 <- HDI_Region_Mapping%>%select(-country_label)%>%
  filter(area<=21)

areas <- HDI_Region_Mapping%>%
  filter(country_code>=910& country_code<=931 | 
           country_code==905 | country_code==906| 
           country_code==954| country_code==957 )%>%
  select(area, country_label)



# By region
AD_Region2 <- Avoidable_Deaths_Simulated_All_age_cat%>%
  left_join(HDI_Region_Mapping2, by=c("country_code"))%>%
  filter(age_cat=="Overall")%>%
  mutate(AD_treat_prev=AD_treat+AD_prev)%>%
  ungroup()%>%
  select(-hdi_group,-continent, -country_label,-country_code,  -AD_sum)%>%
  group_by(area)%>%
  mutate(total_deaths=sum(Expect_deaths,na.rm=T))%>%
  select(-Expect_deaths)%>%
  mutate(AD_treat_prev=sum(AD_treat_not_prev,AD_prev,na.rm=T))%>%
  mutate(AD_treat=sum(AD_treat,na.rm=T))%>%
  mutate(AD_treat_not_prev=sum(AD_treat_not_prev,na.rm=T))%>%
  mutate(AD_prev=sum(AD_prev,na.rm=T))%>%
  mutate(AD_unavoid=sum(AD_unavoid,na.rm=T))%>%
  mutate(pAD_treat=AD_treat/total_deaths)%>%
  mutate(pAD_treat_not_prev=AD_treat_not_prev/total_deaths)%>%
  mutate(pAD_prev=AD_prev/total_deaths)%>%
  mutate(pAD_unavoid=AD_unavoid/total_deaths)%>%
  mutate(pAD_treat_prev=AD_treat_prev/total_deaths)%>%
  #mutate(total_overall=sum(total_overall,na.rm=T))  %>%
  mutate(cancer="All Cancer Sites")%>%
  mutate(cancer_code=1000)%>%
  select(-py)%>%
  distinct()%>%
  left_join(areas, by=c("area"))%>%
  ungroup()%>%
  distinct()%>%
  left_join(HDI_Region_Mapping, by=c("area", "country_label"))%>%
  as.data.frame()

# age standardizing by region - aggregate by region and then age standardize

countries_regions<-Avoidable_Deaths_Simulated_All%>%
  select(country_code)%>%distinct()%>%
  left_join(HDI_Region_Mapping2, by=c("country_code"))





# Table 1 in the manuscript

# Gives us number, percentage of total deaths 
table_1_1 <- Avoidable_Deaths_Simulated_All_age_cat  %>%
  select(-country_label, -country_code, -cancer_code, -cancer)%>%
  filter(age_cat=="Overall")%>%
  ungroup()%>%
  mutate(total_deaths=sum(Expect_deaths,na.rm=T))%>%
  mutate(AD_treat_prev=sum(AD_treat_not_prev,AD_prev,na.rm=T))%>%
  mutate(AD_treat=sum(AD_treat,na.rm=T))%>%
  mutate(AD_treat_not_prev=sum(AD_treat_not_prev,na.rm=T))%>%
  mutate(AD_prev=sum(AD_prev,na.rm=T))%>%
  mutate(AD_unavoid=sum(AD_unavoid,na.rm=T))%>%
  mutate(pAD_treat=AD_treat/total_deaths)%>%
  mutate(pAD_treat_not_prev=AD_treat_not_prev/total_deaths)%>%
  mutate(pAD_prev=AD_prev/total_deaths)%>%
  mutate(pAD_unavoid=AD_unavoid/total_deaths)%>%
  mutate(pAD_treat_prev=AD_treat_prev/total_deaths)%>%
  select(AD_treat, AD_prev, 
         AD_treat_not_prev,
         AD_treat_prev, AD_unavoid, total_deaths, 
         pAD_treat, pAD_treat_not_prev,pAD_prev,pAD_treat_prev, pAD_unavoid)%>%
  distinct()




# By Country and all cancer sites

AD_country_all_cancers2<-AD_country_all_cancers%>%  
  ungroup()%>%
  mutate(across(6:10,round, -2))%>%
  mutate(across(12:13,round, -2))%>%
  mutate(across(14:18, round,4)*100)%>% #mutate to show proportion as percentage in export
  select("country_code","country_label",
         "cancer_code", "cancer", 
         "AD_prev","pAD_prev",    
         "AD_treat",  "pAD_treat" ,
         AD_treat_not_prev, pAD_treat_not_prev,
         "AD_treat_prev", "pAD_treat_prev",
         "AD_unavoid",   "pAD_unavoid" ,        
         "total_deaths"  )%>%
  distinct()


# By country AND cancer site
# AD by country and cancer site

Avoidable_Deaths_Simulated_All
Avoidable_Deaths_Simulated_All_age_cat
Avoidable_Deaths_Simulated_All_age_cat_overall<-Avoidable_Deaths_Simulated_All_age_cat%>% #Object to plot overall age standardized on world maps 
  as.data.frame()%>%
  filter(age_cat=="Overall")%>% 
  group_by(country_code,cancer_code)%>%
  mutate(total_deaths=sum(Expect_deaths,na.rm=T))%>%
  select(-Expect_deaths,-AD_sum)%>%
  mutate(AD_treat_prev=sum(AD_treat_not_prev, AD_prev,AD_prev,na.rm=T))%>%
  mutate(AD_treat=sum(AD_treat,na.rm=T))%>%
  mutate(AD_treat_not_prev=sum(AD_treat_not_prev,na.rm=T))%>%
  mutate(AD_prev=sum(AD_prev,na.rm=T))%>%
  mutate(AD_unavoid=sum(AD_unavoid,na.rm=T))%>%
  mutate(pAD_treat=AD_treat/total_deaths)%>%
  mutate(pAD_treat_not_prev=AD_treat_not_prev/total_deaths)%>%
  mutate(pAD_prev=AD_prev/total_deaths)%>%
  mutate(pAD_unavoid=AD_unavoid/total_deaths)%>%
  mutate(pAD_treat_prev=AD_treat_prev/total_deaths)%>%
  as.data.frame()%>%
  mutate(across(6:9,round,0 ))%>%
  mutate(across(13:13,round,0))%>%
  mutate(across(14:19, round,4)*100)%>% #mutate to show proportion as percentage in export
  select( "country_code","country_label","cancer_code", "cancer", 
          "AD_prev","pAD_prev",    
          "AD_treat",  "pAD_treat" ,
          AD_treat_not_prev, pAD_treat_not_prev,
          "AD_treat_prev", "pAD_treat_prev",
          "AD_unavoid",   "pAD_unavoid" ,        
          "total_deaths"  )



#By cancer site

AD_cancer2 <- AD_cancer%>%
  mutate(across(6: 11, round, -2))%>%
  mutate(across(12:16, round,4)*100)%>% #mutate to show proportion as percentage in export
  select( "country_code", "country_label",
          "cancer_code", "cancer", 
          "AD_prev",        "pAD_prev",    
          "AD_treat",       "pAD_treat" ,
          AD_treat_not_prev, pAD_treat_not_prev,
          "AD_treat_prev",  "pAD_treat_prev",
          "AD_unavoid",     "pAD_unavoid" ,        
          "total_deaths")

AD_by_HDI_all2<-AD_by_HDI_all%>%
  mutate(across(3:6, round, -2))%>%
  mutate(across(8:9, round, -2))%>%
  mutate(across(10:14, round,4)*100)%>% #mutate to show proportion as percentage in export
  select( "hdi_group",     "cancer_code", "cancer", 
          "AD_prev",        "pAD_prev",    
          "AD_treat",       "pAD_treat" ,
          AD_treat_not_prev, pAD_treat_not_prev,
          "AD_treat_prev",  "pAD_treat_prev",
          "AD_unavoid",     "pAD_unavoid" ,        
          "total_deaths")




#By region
AD_Region<-AD_Region2%>%
  mutate(across(4:7, round, -2))%>%
  mutate(across(10:11,round, -2))%>%
  mutate(across(12:16, round,4)*100)%>% #mutate to show proportion as percentage in export
  arrange(continent, country_label)%>%
  select("age_cat", "cancer_code", "cancer", 
         "AD_prev","pAD_prev",    
         "AD_treat",  "pAD_treat" ,
         "AD_treat_prev", "pAD_treat_prev",
         AD_treat_not_prev, pAD_treat_not_prev,
         "AD_unavoid",   "pAD_unavoid" ,        
         "total_deaths" , 
         "area"       ,            
         "country_label" , "country_code" ,  "continent")




#World total

table_1_11<-table_1_1%>%mutate(across(1:6,round, -2))%>%
  mutate(across(7:11, round,4)*100)%>% #mutate to show proportion as percentage in export
  select(
    "AD_prev","pAD_prev",    
    "AD_treat",  "pAD_treat" ,
    "AD_treat_prev", "pAD_treat_prev",
    AD_treat_not_prev, pAD_treat_not_prev,
    "AD_unavoid",   "pAD_unavoid" ,        
    "total_deaths" )

table_1_2

#HDI

AD_by_HDI

AD_by_HDI_all2<-AD_by_HDI_all%>%
  mutate(across(3:6,round, -2))%>%
  mutate(across(8:9,round, -2))%>%
  mutate(across(10:14, round,4)*100)%>% #mutate to show proportion as percentage in export
  select( "hdi_group",     "cancer_code", "cancer", 
          "AD_prev",        "pAD_prev",    
          "AD_treat",       "pAD_treat" ,
          "AD_treat_prev",  "pAD_treat_prev",
          "AD_unavoid",     "pAD_unavoid" ,        
          "total_deaths")

# By risk factor 

AD_Risk_factors<-Avoidable_Deaths_Simulated_All_age_cat%>%
  
  
  
  
  
#writing the files
write.csv(Simulated_Data_PAF_All, "~/Documents/R_Projects/Data/NS_Simulated_All_Countries.csv")
write.csv(Avoidable_Deaths_Simulated_All, "~/Documents/R_Projects/Data/AD_Simulated_All_Countries.csv")
write.csv(Avoidable_Deaths_Simulated_All_age_cat, "~/Documents/R_Projects/Data/AD_Simulated_All_Countries_age_cat.csv")
write.csv(AD_Region, "~/Documents/R_Projects/Data/AD_Region.csv")
write.csv(table_1_11, "~/Documents/R_Projects/Data/AD_Total.csv")
write.csv(AD_by_HDI_all2, "~/Documents/R_Projects/Data/AD_HDI_All_Cancers.csv")
write.csv(AD_country_all_cancers2, "~/Documents/R_Projects/Data/AD_Country_All_Cancers.csv")
write.csv(AD_cancer2, "~/Documents/R_Projects/Data/AD_Cancer_by_Site.csv")


AD_Region
AD_by_HDI_all2
AD_country_all_cancers2
AD_cancer2
table_1_11 


