
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
  summarize(country_label, 
            country_code,
            age_label)%>% #This needs to be adjusted with population weights
  as.data.frame()%>%
  distinct()



countries_5y<-Survival_Modelled%>%
  mutate(rel_surv=case_when(rel_surv>1~ 1,
                            rel_surv<=1~ rel_surv))%>%
  mutate(country_label = str_remove( country_label,'"'))%>%
  mutate(country_label = str_remove( country_label,"'"))%>%
  mutate(country_label = str_remove( country_label,"`"))%>%
  mutate(country_label = str_remove( country_label,'"'))%>%
  arrange(country_label)%>%
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


simulated_overall<-Countries_Simulated%>%
  as.data.frame()%>%
  droplevels()%>%
  group_by(country_label,cancer_label, age_cat,age)%>%
  summarize(country_code,country_label, cancer_code, cancer_label,rel_surv,
            age_cat, age, hdi_group)%>%
  distinct()%>%
  arrange(country_label,cancer_label, age)



# PAF combinations

 PAFs_age_Cat_RF <- PAFs%>%
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

            af.comb= case_when(cases!=0 ~  sum(cases.prev, na.rm=T)/sum(cases, na.rm=T),
                               cases.prev==0 ~ 0),
            af.tob= case_when(cases!=0 ~  sum(cases*af.tob, na.rm=T)/sum(cases, na.rm=T),
                               cases.prev==0 ~ 0),
            af.alc= case_when(cases!=0 ~  sum(cases*af.alc, na.rm=T)/sum(cases, na.rm=T),
                              cases.prev==0 ~ 0),
            af.inf= case_when(cases!=0 ~  sum(cases*af.inf, na.rm=T)/sum(cases, na.rm=T),
                              cases.prev==0 ~ 0),
            af.obe= case_when(cases!=0 ~  sum(cases*af.obe, na.rm=T)/sum(cases, na.rm=T),
                              cases.prev==0 ~ 0),
            af.uv= case_when(cases!=0 ~  sum(cases*af.uv, na.rm=T)/sum(cases, na.rm=T),
                              cases.prev==0 ~ 0),
            total_overall=sum(cases, na.rm=T),
            ES)%>% #Summarizing by sex
  mutate(ES= case_when(cases!=0 ~ sum(ES*cases, na.rm=T)/sum(cases, na.rm=T),
                       cases==0 ~ ES))%>%
  distinct()%>%
  group_by(country_label,cancer_label, age)




PAFs2_RF <-  PAFs_age_Cat_RF%>%
  as.data.frame()%>%
  droplevels()%>%
  group_by(country_code,cancer_code,cancer_label, age_cat,age,sex)%>%
  mutate(total_age_prev=sum(cases.prev, na.rm=T))%>%
  as.data.frame()%>%
  group_by(country_code,cancer_code,age,sex)%>%
  summarize(country_label, cancer_label,
            age_cat, age,  total_overall,cases,
            cases.prev, ES, af.comb,
            af.tob, af.alc, af.inf, af.obe, af.uv )%>%
  distinct()%>%
  arrange(cancer_label,
          age)%>%
  select(-cancer_label,-country_label)


Simulated_Data_PAF_1_RF <- simulated_overall%>%
  ungroup()%>%
  filter(age_cat!="Overall")%>%
  left_join(PAFs2_RF,by=c("country_code"="country_code",
                       #"country_label"="country_label",
                       "cancer_code"="cancer_code",
                       "age_cat"="age_cat",
                       "age"="age"))%>%
  ungroup()%>%
  group_by(country_code,cancer_code, age, sex)%>%
  mutate(af.comb=case_when(cases!=0 ~ sum(cases.prev, na.rm=T)/sum(cases, na.rm=T),
                           cases==0  ~    0))%>%
mutate(af.tob= case_when(cases!=0 ~  sum(cases*af.tob, na.rm=T)/sum(cases, na.rm=T),
                  cases.prev==0 ~ 0))%>%
  mutate(af.alc= case_when(cases!=0 ~  sum(cases*af.alc, na.rm=T)/sum(cases, na.rm=T),
                  cases.prev==0 ~ 0))%>%
  mutate(af.inf= case_when(cases!=0 ~  sum(cases*af.inf, na.rm=T)/sum(cases, na.rm=T),
                  cases.prev==0 ~ 0))%>%
         mutate(af.obe= case_when(cases!=0 ~  sum(cases*af.obe, na.rm=T)/sum(cases, na.rm=T),
                  cases.prev==0 ~ 0))%>%
                mutate(af.uv= case_when(cases!=0 ~  sum(cases*af.uv, na.rm=T)/sum(cases, na.rm=T),
                 cases.prev==0 ~ 0))%>%
  mutate(rel_surv=case_when(cases!=0 ~ sum(rel_surv*cases, na.rm=T)/sum(cases, na.rm=T),
                            cases==0  ~    rel_surv))%>%
  summarize(country_code, 
            country_label, 
            cancer_code, cancer_label,
            age_cat, age, total_overall,
            rel_surv,
            af.comb,  
            af.tob, 
            af.alc, 
            af.inf,
            af.obe, 
            af.uv,
            rel_surv,
            ES,
            total_overall,
            hdi_group)%>%
  droplevels()%>%
  distinct()%>%
  as.data.frame()


Simulated_Data_PAF_All_RF <- Simulated_Data_PAF_1_RF%>%
  mutate(rel_surv=as.double(rel_surv))%>%
  mutate(af.comb=as.double(af.comb))%>% 
  mutate(af.tob=as.double(af.tob))%>% 
  mutate(af.alc=as.double(af.alc))%>% 
  mutate(af.inf=as.double(af.inf))%>% 
  mutate(af.obe=as.double(af.obe))%>% 
  mutate(af.uv=as.double(af.uv))%>% 
  mutate(total_overall=as.double(total_overall))%>% 
  arrange(country_label,cancer_code,age,sex)%>%
  left_join(Reference_Survival, by=c("age","cancer_code")) 



Avoidable_Deaths_Simulated_All2_RF<-Simulated_Data_PAF_All_RF%>%
  group_by(country_code, cancer_code, age, sex)%>%
  mutate(AD_prev=af.comb * total_overall * (1 - rel_surv *  ES ))%>%
  mutate(AD_prev_tob=af.tob * total_overall * (1 - rel_surv *  ES ))%>%
  mutate(AD_prev_alc=af.alc * total_overall * (1 - rel_surv *  ES ))%>%
  mutate(AD_prev_inf=af.inf * total_overall * (1 - rel_surv *  ES ))%>%
  mutate(AD_prev_obe=af.obe * total_overall * (1 - rel_surv *  ES ))%>%
  mutate(AD_prev_uv=af.uv * total_overall * (1 - rel_surv *  ES ))%>%
  mutate(AD_treat=total_overall *(surv_ref-rel_surv) * ES)%>%
  mutate(AD_treat_not_prev=(1-af.comb)* total_overall *(surv_ref-rel_surv) * ES)%>%
  mutate(AD_unavoid = (1-af.comb)*total_overall*(1-surv_ref*ES))%>%
  mutate(Expect_deaths=(1-(rel_surv*ES))*total_overall)%>%
  select("country_code","country_label","age_cat","age","cancer_code","cancer_label",   
         "AD_treat",
         "AD_treat_not_prev",
         "AD_prev",
         "AD_prev_tob",
         "AD_prev_alc",
         "AD_prev_inf",
         "AD_prev_obe",
         "AD_prev_uv",
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
  filter(age>3)%>%
  mutate(total=sum(w2))%>%
  group_by(age)%>%
  mutate(w=sum(w2)/total)%>%
  select(-w2, -total)%>%
  distinct()


Avoidable_Deaths_Simulated_All_RF<- Avoidable_Deaths_Simulated_All2_RF%>%
  as.data.frame()%>%
  mutate(age=as.numeric(as.character(age)))%>%
  mutate(AD_prev=as.numeric(as.character(AD_prev)))%>%
  mutate(AD_prev_tob=as.numeric(as.character(AD_prev_tob)))%>%
  mutate(AD_prev_alc=as.numeric(as.character(AD_prev_alc)))%>%
  mutate(AD_prev_inf=as.numeric(as.character(AD_prev_inf)))%>%
  mutate(AD_prev_obe=as.numeric(as.character(AD_prev_obe)))%>%
  mutate(AD_prev_uv=as.numeric(as.character(AD_prev_uv)))%>%
  mutate(AD_unavoid=as.numeric(as.character(AD_unavoid)))%>%
  mutate(total_overall=as.numeric(as.character(total_overall)))%>%
  mutate(AD_treat=as.numeric(as.character(AD_treat)))%>%
  mutate(AD_treat_not_prev=as.numeric(as.character(AD_treat_not_prev)))%>%
  mutate(AD_sum=AD_prev + AD_unavoid + AD_treat_not_prev)%>%
  mutate(cancer_code=as.numeric(cancer_code))%>%
  as.data.frame()%>%distinct()%>%
  mutate(country_code=as.integer(country_code))%>%
  left_join(pop20202, c("sex", "country_code", "age"))%>%
  left_join(weights2)%>%
  mutate(AD_treat = case_when(AD_treat<0 ~ 0, 
                              # Numerical calculation error causes the countries which are references  to go slightly negative. 
                              # By definition this is zero This is rounded to 0
                              TRUE ~ AD_treat))


## added new ASR code below
Avoidable_Deaths_Simulated_All_overall_RF <- Avoidable_Deaths_Simulated_All_RF%>%
  mutate(age_cat="Overall")%>%
  group_by(country_code, cancer_code, age_cat)%>%
  mutate(AD_prev.asr=sum(AD_prev/py*100000*w), #ASR calculation here
         AD_prev_tob.asr=sum(AD_prev_tob/py*100000*w),
         AD_prev_alc.asr=sum(AD_prev_alc/py*100000*w),
         AD_prev_inf.asr=sum(AD_prev_inf/py*100000*w),
         AD_prev_obe.asr=sum(AD_prev_obe/py*100000*w),
         AD_prev_uv.asr=sum(AD_prev_uv/py*100000*w),
         AD_treat.asr=sum(AD_treat/py*100000*w),
         AD_sum.asr=sum(AD_sum/py*100000*w),
         AD_treat_not_prev.asr=sum(AD_treat_not_prev/py*100000*w),
         AD_unavoid.asr=sum(AD_unavoid/py*100000*w),
         Expect_deaths.asr=sum(Expect_deaths/py*100000*w)) %>% 
  select( -total_overall)%>%
  mutate(AD_prev = sum(AD_prev,na.rm=T))%>%
  mutate(AD_prev_tob = sum(AD_prev_tob,na.rm=T))%>%
  mutate(AD_prev_alc = sum(AD_prev_alc,na.rm=T))%>%
  mutate(AD_prev_inf = sum(AD_prev_inf,na.rm=T))%>%
  mutate(AD_prev_obe = sum(AD_prev_obe,na.rm=T))%>%
  mutate(AD_prev_uv = sum(AD_prev_uv,na.rm=T))%>%
  mutate(AD_treat = sum(AD_treat,na.rm=T))%>%
  mutate(AD_sum = sum(AD_sum,na.rm=T))%>%
  mutate(AD_treat_not_prev = sum(AD_treat_not_prev,na.rm=T))%>%
  mutate(AD_unavoid = sum(AD_unavoid,na.rm=T))%>%
  mutate(Expect_deaths=sum(Expect_deaths,na.rm=T))%>%
  mutate(py=sum(py))%>%
  select(-af.comb, -age,-sex)%>%
  ungroup()%>%
  group_by(cancer_code, country_code,age_cat)%>%
  mutate(w=sum(w/2))%>%
  distinct()%>%
  as.data.frame()


Avoidable_Deaths_Simulated_All_age_cat_RF <- Avoidable_Deaths_Simulated_All_RF%>%
  ungroup()%>%
  group_by(country_code, cancer_code, age_cat)%>%
  mutate(AD_prev.asr=sum(AD_prev/py*100000*w), #ASR calculation here
         AD_prev_tob.asr=sum(AD_prev_tob/py*100000*w),
         AD_prev_alc.asr=sum(AD_prev_alc/py*100000*w),
         AD_prev_inf.asr=sum(AD_prev_inf/py*100000*w),
         AD_prev_obe.asr=sum(AD_prev_obe/py*100000*w),
         AD_prev_uv.asr=sum(AD_prev_uv/py*100000*w),
         AD_treat.asr=sum(AD_treat/py*100000*w),
         AD_sum.asr=sum(AD_sum/py*100000*w),
         AD_treat_not_prev.asr=sum(AD_treat_not_prev/py*100000*w),
         AD_unavoid.asr=sum(AD_unavoid/py*100000*w),
         Expect_deaths.asr=sum(Expect_deaths/py*100000*w)) %>% 
  select(-total_overall)%>%
  #mutate(total_overall=sum(total_overall,na.rm=T))%>%
  mutate(AD_prev = sum(AD_prev,na.rm=T))%>%
  mutate(AD_prev_tob = sum(AD_prev_tob,na.rm=T))%>%
  mutate(AD_prev_alc = sum(AD_prev_alc,na.rm=T))%>%
  mutate(AD_prev_inf = sum(AD_prev_inf,na.rm=T))%>%
  mutate(AD_prev_obe = sum(AD_prev_obe,na.rm=T))%>%
  mutate(AD_prev_uv = sum(AD_prev_uv,na.rm=T))%>%
  mutate(AD_treat = sum(AD_treat,na.rm=T))%>%
  mutate(AD_treat_not_prev = sum(AD_treat_not_prev,na.rm=T))%>%
  mutate(AD_unavoid = sum(AD_unavoid,na.rm=T))%>%
  mutate(Expect_deaths=sum(Expect_deaths,na.rm=T))%>%
  mutate(AD_sum = sum(AD_sum,na.rm=T))%>%
  mutate(py=sum(py))%>%
  select(-af.comb, -age,-sex)%>%
  ungroup()%>%
  group_by(cancer_code, country_code,age_cat)%>%
  mutate(w=sum(w/2))%>%
  distinct()%>%
  full_join(Avoidable_Deaths_Simulated_All_overall_RF)%>%
  as.data.frame()%>%ungroup()


#
AD_cancer_RF <- Avoidable_Deaths_Simulated_All_RF%>%
  mutate(age_cat="Overall")%>%
  group_by(cancer, age,sex)%>%
  mutate(py=sum(py))%>%
  ungroup()%>%
  mutate(AD_treat_prev=AD_treat+AD_prev)%>%
  ungroup()%>%
  select(-hdi_group,  -AD_sum,-af.comb,-age,-sex)%>%
  mutate(country_code=1000)%>%
  mutate(country_label="All Countries")%>%
  group_by(cancer)%>%
  mutate(AD_prev.asr=sum(AD_prev/py*100000*w), #ASR calculation here
         AD_treat.asr=sum(AD_treat/py*100000*w),
         AD_treat_prev.asr=sum((AD_treat_not_prev+AD_prev)/py*100000*w),
         AD_treat_not_prev.asr=sum(AD_treat_not_prev/py*100000*w),
         AD_unavoid.asr=sum(AD_unavoid/py*100000*w),
         Expect_deaths.asr=sum(Expect_deaths/py*100000*w)) %>% 
  mutate(total_deaths=sum(Expect_deaths,na.rm=T))%>%
  select(-Expect_deaths)%>%
  mutate(AD_treat_prev=sum(AD_treat_not_prev,AD_prev,na.rm=T))%>%
  mutate(AD_treat=sum(AD_treat,na.rm=T))%>%
  mutate(AD_treat_not_prev=sum(AD_treat_not_prev,na.rm=T))%>%
  mutate(AD_prev=sum(AD_prev,na.rm=T))%>%
  mutate(AD_prev_tob = sum(AD_prev_tob,na.rm=T))%>%
  mutate(AD_prev_alc = sum(AD_prev_alc,na.rm=T))%>%
  mutate(AD_prev_inf = sum(AD_prev_inf,na.rm=T))%>%
  mutate(AD_prev_obe = sum(AD_prev_obe,na.rm=T))%>%
  mutate(AD_prev_uv = sum(AD_prev_uv,na.rm=T))%>%
  mutate(AD_unavoid=sum(AD_unavoid,na.rm=T))%>%
  mutate(pAD_treat=AD_treat/total_deaths)%>%
  mutate(pAD_treat_not_prev=AD_treat_not_prev/total_deaths)%>%
  mutate(pAD_prev=AD_prev/total_deaths)%>%
  mutate(pAD_prev_tob=AD_prev_tob/total_deaths)%>%
  mutate(pAD_prev_alc=AD_prev_alc/total_deaths)%>%
  mutate(pAD_prev_inf=AD_prev_inf/total_deaths)%>%
  mutate(pAD_prev_obe=AD_prev_obe/total_deaths)%>%
  mutate(pAD_prev_uv=AD_prev_uv/total_deaths)%>%
  mutate(pAD_unavoid=AD_unavoid/total_deaths)%>%
  mutate(pAD_treat_prev=AD_treat_prev/total_deaths)%>%
  select(-py,-w,-total_overall)%>%
  distinct()%>%
  ungroup()%>%
  distinct()%>%
  as.data.frame()


# Calculating by region. Need a file that links countries to region 
HDI_Region_Mapping2 <- HDI_Region_Mapping%>%
  select(-country_label)%>%
  filter(area<=21)

areas <- HDI_Region_Mapping%>%
  filter(country_code>=910 & country_code<=931 | 
           country_code==905 | country_code==906| 
           country_code==954| country_code==957 )%>%
  select(area, country_label)



# By region
AD_Region_AF2 <- Avoidable_Deaths_Simulated_All_RF%>%
  left_join(HDI_Region_Mapping2, by=c("country_code"))%>%
  mutate(age_cat="Overall")%>%
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  group_by(area, cancer, age,sex)%>%
  mutate(py=sum(py))%>%
  ungroup()%>%
  select(-country_label,-country_code,-cancer_code, -cancer,  hdi_group,  AD_treat, 
         AD_prev, AD_prev_tob, AD_prev_alc, AD_prev_inf, AD_prev_obe, AD_prev_uv, 
         
         AD_unavoid, Expect_deaths, -sex, -age, age_cat,-total_overall,-af.comb)%>%
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  mutate(hdi_group=as.numeric(hdi_group))%>%
  ungroup()%>%  mutate(AD_treat_prev=AD_treat+AD_prev)%>%
  ungroup()%>%
  select(-hdi_group,-continent,   -AD_sum)%>%
  group_by(area)%>%
  mutate(AD_prev.asr=sum(AD_prev/py*100000*w), #ASR calculation here
         AD_prev_tob.asr=sum(AD_prev_tob/py*100000*w),
         AD_prev_alc.asr=sum(AD_prev_alc/py*100000*w),
         AD_prev_inf.asr=sum(AD_prev_inf/py*100000*w),
         AD_prev_obe.asr=sum(AD_prev_obe/py*100000*w),
         AD_prev_uv.asr=sum(AD_prev_uv/py*100000*w),
         AD_treat.asr=sum(AD_treat/py*100000*w),
         AD_treat_prev.asr=sum((AD_treat_not_prev+AD_prev)/py*100000*w),
         AD_treat_not_prev.asr=sum(AD_treat_not_prev/py*100000*w),
         AD_unavoid.asr=sum(AD_unavoid/py*100000*w),
         Expect_deaths.asr=sum(Expect_deaths/py*100000*w)) %>% 
  mutate(total_deaths=sum(Expect_deaths,na.rm=T))%>%
  select(-Expect_deaths)%>%
  mutate(AD_treat_prev=sum(AD_treat_not_prev,AD_prev,na.rm=T))%>%
  mutate(AD_treat=sum(AD_treat,na.rm=T))%>%
  mutate(AD_treat_not_prev=sum(AD_treat_not_prev,na.rm=T))%>%
  mutate(AD_prev=sum(AD_prev,na.rm=T))%>%
  mutate(AD_prev_tob = sum(AD_prev_tob,na.rm=T))%>%
  mutate(AD_prev_alc = sum(AD_prev_alc,na.rm=T))%>%
  mutate(AD_prev_inf = sum(AD_prev_inf,na.rm=T))%>%
  mutate(AD_prev_obe = sum(AD_prev_obe,na.rm=T))%>%
  mutate(AD_prev_uv = sum(AD_prev_uv,na.rm=T))%>%
  mutate(AD_unavoid=sum(AD_unavoid,na.rm=T))%>%
  mutate(pAD_treat=AD_treat/total_deaths)%>%
  mutate(pAD_treat_not_prev=AD_treat_not_prev/total_deaths)%>%
  mutate(pAD_prev=AD_prev/total_deaths)%>%
  mutate(pAD_prev_tob=AD_prev_tob/total_deaths)%>%
  mutate(pAD_prev_alc=AD_prev_alc/total_deaths)%>%
  mutate(pAD_prev_inf=AD_prev_inf/total_deaths)%>%
  mutate(pAD_prev_obe=AD_prev_obe/total_deaths)%>%
  mutate(pAD_prev_uv=AD_prev_uv/total_deaths)%>%
  mutate(pAD_unavoid=AD_unavoid/total_deaths)%>%
  mutate(pAD_treat_prev=AD_treat_prev/total_deaths)%>%
  mutate(cancer="All Cancer Sites")%>%
  mutate(cancer_code=1000)%>%
  select(-py,-w)%>%
  distinct()%>%
  left_join(areas, by=c("area"))%>%
  ungroup()%>%
  distinct()%>%
  left_join(HDI_Region_Mapping, by=c("area", "country_label"))%>%
  as.data.frame()

#By HDI

AD_by_HDI_all <-Avoidable_Deaths_Simulated_All_RF%>%
  mutate(age_cat="Overall")%>%
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  group_by(hdi_group, cancer, age,sex)%>%
  mutate(py=sum(py))%>%
  ungroup()%>%
  select(-country_label,-country_code,-cancer_code, -cancer,  hdi_group,  AD_treat, 
         AD_prev, AD_prev_tob, AD_prev_alc, AD_prev_inf, AD_prev_obe, AD_prev_uv, 
         AD_unavoid, Expect_deaths, AD_sum,-sex, -age, age_cat,-total_overall)%>%
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  mutate(hdi_group=as.numeric(hdi_group))%>%
  ungroup()%>%
  group_by(hdi_group)%>%
  mutate(AD_prev.asr=sum(AD_prev/py*100000*w), #ASR calculation here
         AD_prev_tob.asr=sum(AD_prev_tob/py*100000*w),
         AD_prev_alc.asr=sum(AD_prev_alc/py*100000*w),
         AD_prev_inf.asr=sum(AD_prev_inf/py*100000*w),
         AD_prev_obe.asr=sum(AD_prev_obe/py*100000*w),
         AD_prev_uv.asr=sum(AD_prev_uv/py*100000*w),
         AD_treat.asr=sum(AD_treat/py*100000*w),
         AD_treat_prev.asr=sum((AD_treat_not_prev+AD_prev)/py*100000*w),
         AD_treat_not_prev.asr=sum(AD_treat_not_prev/py*100000*w),
         AD_unavoid.asr=sum(AD_unavoid/py*100000*w),
         Expect_deaths.asr=sum(Expect_deaths/py*100000*w)) %>% 
  mutate(AD_treat_prev=sum(AD_prev+AD_treat_not_prev))%>%
  mutate(Expect_deaths=sum(Expect_deaths))%>%
  mutate(AD_treat=sum(AD_treat))%>%
  mutate(AD_treat_not_prev=sum(AD_treat_not_prev))%>%
  mutate(AD_prev=sum(AD_prev,na.rm=T))%>%
  mutate(AD_prev_tob = sum(AD_prev_tob,na.rm=T))%>%
  mutate(AD_prev_alc = sum(AD_prev_alc,na.rm=T))%>%
  mutate(AD_prev_inf = sum(AD_prev_inf,na.rm=T))%>%
  mutate(AD_prev_obe = sum(AD_prev_obe,na.rm=T))%>%
  mutate(AD_prev_uv = sum(AD_prev_uv,na.rm=T))%>%
  mutate(AD_unavoid=sum(AD_unavoid))%>%
  mutate(total_deaths=Expect_deaths)%>%
  mutate(pAD_treat=AD_treat/total_deaths)%>%
  mutate(pAD_treat_not_prev=AD_treat_not_prev/total_deaths)%>%
  mutate(pAD_prev=AD_prev/total_deaths)%>%
  mutate(pAD_prev_tob=AD_prev_tob/total_deaths)%>%
  mutate(pAD_prev_alc=AD_prev_alc/total_deaths)%>%
  mutate(pAD_prev_inf=AD_prev_inf/total_deaths)%>%
  mutate(pAD_prev_obe=AD_prev_obe/total_deaths)%>%
  mutate(pAD_prev_uv=AD_prev_uv/total_deaths)%>%
  mutate(pAD_unavoid=AD_unavoid/total_deaths)%>%
  mutate(pAD_treat_prev=AD_treat_prev/total_deaths)%>%
  mutate(cancer="All Cancer Sites")%>%
  mutate(w=sum(w)/n())%>%
  select(-AD_sum,-af.comb,-py)%>%
  ungroup()%>%
  distinct()%>%
  as.data.frame()
#Worldwide summary
table_1_1 <- Avoidable_Deaths_Simulated_All_RF %>%
  select(-country_label, -country_code, -cancer_code, hdi_group)%>%
  mutate(age_cat="Overall")%>%
  group_by(cancer, age,sex)%>%
  mutate(py=sum(py))%>%
  ungroup()%>%
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  mutate(hdi_group=as.numeric(hdi_group))%>%
  ungroup()%>%  mutate(AD_treat_prev=AD_treat+AD_prev)%>%
  ungroup()%>%
  mutate(AD_prev.asr=sum(AD_prev/py*100000*w), #ASR calculation here
         AD_prev_tob.asr=sum(AD_prev_tob/py*100000*w),
         AD_prev_alc.asr=sum(AD_prev_alc/py*100000*w),
         AD_prev_inf.asr=sum(AD_prev_inf/py*100000*w),
         AD_prev_obe.asr=sum(AD_prev_obe/py*100000*w),
         AD_prev_uv.asr=sum(AD_prev_uv/py*100000*w),
         AD_treat.asr=sum(AD_treat/py*100000*w),
         AD_treat_prev.asr=sum((AD_treat_not_prev+AD_prev)/py*100000*w),
         AD_treat_not_prev.asr=sum(AD_treat_not_prev/py*100000*w),
         AD_unavoid.asr=sum(AD_unavoid/py*100000*w),
         Expect_deaths.asr=sum(Expect_deaths/py*100000*w)) %>% 
  ungroup()%>%
  mutate(total_deaths=sum(Expect_deaths,na.rm=T))%>%
  mutate(AD_treat_prev=sum(AD_treat_not_prev,AD_prev,na.rm=T))%>%
  mutate(AD_treat=sum(AD_treat,na.rm=T))%>%
  mutate(AD_treat_not_prev=sum(AD_treat_not_prev,na.rm=T))%>%
  mutate(AD_prev=sum(AD_prev,na.rm=T))%>%
  mutate(AD_prev_tob = sum(AD_prev_tob,na.rm=T))%>%
  mutate(AD_prev_alc = sum(AD_prev_alc,na.rm=T))%>%
  mutate(AD_prev_inf = sum(AD_prev_inf,na.rm=T))%>%
  mutate(AD_prev_obe = sum(AD_prev_obe,na.rm=T))%>%
  mutate(AD_prev_uv = sum(AD_prev_uv,na.rm=T))%>%
  mutate(AD_unavoid=sum(AD_unavoid,na.rm=T))%>%
  mutate(pAD_treat=AD_treat/total_deaths)%>%
  mutate(pAD_treat_not_prev=AD_treat_not_prev/total_deaths)%>%
  mutate(pAD_prev=AD_prev/total_deaths)%>%
  mutate(pAD_prev_tob=AD_prev_tob/total_deaths)%>%
  mutate(pAD_prev_alc=AD_prev_alc/total_deaths)%>%
  mutate(pAD_prev_inf=AD_prev_inf/total_deaths)%>%
  mutate(pAD_prev_obe=AD_prev_obe/total_deaths)%>%
  mutate(pAD_prev_uv=AD_prev_uv/total_deaths)%>%
  mutate(pAD_unavoid=AD_unavoid/total_deaths)%>%
  mutate(pAD_treat_prev=AD_treat_prev/total_deaths)%>%
  select(AD_prev, AD_prev_tob, AD_prev_alc, AD_prev_inf, AD_prev_obe, AD_prev_uv, 
         AD_treat, 
         AD_treat_not_prev,
         AD_treat_prev, AD_unavoid, total_deaths, 
         pAD_prev, pAD_prev_tob, pAD_prev_alc, pAD_prev_inf, pAD_prev_obe, pAD_prev_uv, 
         pAD_treat, pAD_treat_not_prev,pAD_treat_prev, pAD_unavoid,
         AD_prev.asr, AD_prev_tob.asr, AD_prev_alc.asr, AD_prev_inf.asr, AD_prev_obe.asr, AD_prev_uv.asr, 
         AD_treat.asr ,AD_treat_not_prev.asr,
         AD_treat_prev.asr, AD_unavoid.asr,Expect_deaths.asr)%>%
  distinct()

#prepping table for export


#By region
AD_Region_AF<-AD_Region_AF2%>%
  mutate(across(2:10, round, -2))%>%
  mutate(across(12:12,round, -2))%>%
  mutate(across(13:23  ,round, 1))%>%
  mutate(across(24:24,round, -2))%>%
  mutate(across(25:34, round,3)*100)%>% #mutate to show proportion as percentage in export
  arrange(continent, country_label)%>%
  select("continent","area","country_label","age_cat", "cancer_code", "cancer", 
         "AD_prev", "pAD_prev",    "AD_prev.asr",
         "AD_prev_tob", "pAD_prev_tob",    "AD_prev_tob.asr",
         "AD_prev_alc", "pAD_prev_alc",    "AD_prev_alc.asr",
         "AD_prev_inf", "pAD_prev_inf",    "AD_prev_inf.asr",
         "AD_prev_obe", "pAD_prev_obe",    "AD_prev_obe.asr",
         "AD_prev_uv", "pAD_prev_uv",    "AD_prev_uv.asr",
    #     "AD_treat",       "pAD_treat" ,"AD_treat.asr",
     #    "AD_treat_prev",  "pAD_treat_prev","AD_treat_prev.asr",
      #   "AD_unavoid",     "pAD_unavoid" ,   "AD_unavoid.asr",    
         "total_deaths","Expect_deaths.asr")



#World total

table_1_11_RF<-table_1_1%>%
  mutate(across(1:11,round, -2))%>%
  mutate(across(12:21, round,3)*100)%>% #mutate to show proportion as percentage in export
  mutate(across(22:32,round, 1))%>%
  select(
    "AD_prev", "pAD_prev",    "AD_prev.asr",
    "AD_prev_tob", "pAD_prev_tob",    "AD_prev_tob.asr",
    "AD_prev_alc", "pAD_prev_alc",    "AD_prev_alc.asr",
    "AD_prev_inf", "pAD_prev_inf",    "AD_prev_inf.asr",
    "AD_prev_obe", "pAD_prev_obe",    "AD_prev_obe.asr",
    "AD_prev_uv", "pAD_prev_uv",    "AD_prev_uv.asr",
    #     "AD_treat",       "pAD_treat" ,"AD_treat.asr",
    #    "AD_treat_prev",  "pAD_treat_prev","AD_treat_prev.asr",
    #   "AD_unavoid",     "pAD_unavoid" ,   "AD_unavoid.asr",    
    "total_deaths","Expect_deaths.asr")



#HDI

AD_by_HDI

AD_by_HDI_all2_RF<-AD_by_HDI_all%>%
  mutate(across(2:11, round, -2))%>%
  mutate(across(14:24, round, 1))%>%
  mutate(across(25:26, round, -2))%>%
  mutate(across(27:36, round,3)*100)%>% #mutate to show proportion as percentage in export
  select( "hdi_group",      "cancer", 
          "AD_prev", "pAD_prev",    "AD_prev.asr",
          "AD_prev_tob", "pAD_prev_tob",    "AD_prev_tob.asr",
          "AD_prev_alc", "pAD_prev_alc",    "AD_prev_alc.asr",
          "AD_prev_inf", "pAD_prev_inf",    "AD_prev_inf.asr",
          "AD_prev_obe", "pAD_prev_obe",    "AD_prev_obe.asr",
          "AD_prev_uv", "pAD_prev_uv",    "AD_prev_uv.asr",
          #     "AD_treat",       "pAD_treat" ,"AD_treat.asr",
          #    "AD_treat_prev",  "pAD_treat_prev","AD_treat_prev.asr",
          #   "AD_unavoid",     "pAD_unavoid" ,   "AD_unavoid.asr",    
          "total_deaths","Expect_deaths.asr")
          



#All files for the table 

AD_Region_AF
AD_by_HDI_all2_RF
table_1_11_RF

write.csv(AD_Region_AF, "~/Documents/R_Projects/Data/AD_Region_AF_Risk_factors.csv")
write.csv(table_1_11_RF, "~/Documents/R_Projects/Data/AD_Total_Risk_factors.csv")
write.csv(AD_by_HDI_all2_RF, "~/Documents/R_Projects/Data/AD_HDI_All_Cancers_Risk_factors.csv")





