



###############################################
#
# Net survival and Avoidable deaths - Simulated data and new equations
# Date: 20/01/2023
# Version 4.0 - 
#
#Load files and packages in AD_2.R file
#
# Works for multiple cancer sites currently.
#
#
###############################################


setwd("C:\\Users\\langseliuso\\Documents\\GitHub\\avoidable_deaths\\AD Preventable and Treatable")

# Expected survival

ES3<-p%>%
  as.data.frame()%>%
  dplyr::filter(year==2019)%>%
  dplyr::rename(ES="es")%>%
  select(-country_label, -year)


#Adding so the expected survival is the same for patients aged 70+
# 
# ES4_16<-ES4%>%dplyr::filter(age==15)%>%
#   dplyr::mutate(age=16)
# 
# ES4_17<-ES4%>%dplyr::filter(age==15)%>%
#   dplyr::mutate(age=17)
# 
# ES4_18<-ES4%>%dplyr::filter(age==15)%>%
#   dplyr::mutate(age=18)
# 
# ES3<-ES4%>%
#   full_join(ES4_16)%>%
#   full_join(ES4_17)%>%
#   full_join(ES4_18)%>%
#   distinct()

# Imputing missing countries 

# For For all the French overseas and territories (Polynesia, Guyana, Guadeloupe, Martinique, Reunion, New Caledonia)
# ES_France1<-ES3%>%dplyr::filter(country_code==250)%>%dplyr::mutate(ES3=ES) %>%select(-ES) 
# ES_France2<-ES_France1%>%dplyr::mutate(country_code=254)
# ES_France3<-ES_France1%>%dplyr::mutate(country_code=258)
# ES_France4<-ES_France1%>%dplyr::mutate(country_code=312)
# ES_France5<-ES_France1%>%dplyr::mutate(country_code=474)
# ES_France6<-ES_France1%>%dplyr::mutate(country_code=540)
# ES_France7<-ES_France1%>%dplyr::mutate(country_code=638)
# 
# # Palestine? - Imputed by HDI 0.690 average plus minus 0.1 HDI
# 
# HDI_countries_palestine<-Survival_Modelled%>%
#   dplyr::filter(0.690-0.05<=hdi_value &hdi_value<=0.690+0.05)%>%
#   select(country_code)%>%
#   distinct()
# 
# ES_palestine<-ES3%>%dplyr::filter(country_code%in%HDI_countries_palestine$country_code)%>%
#   dplyr::group_by(age)%>%
#   dplyr::mutate(ES=mean(ES, na.rm=T))%>%
#   dplyr::mutate(country_code=275)%>%
#   distinct()%>%
#   dplyr::mutate(ES3=ES)%>%select(-ES)
# 
# 
# # Western Sahara? - Imputed by HDI 0.690 average plus minus 0.1 HDI
# 
# HDI_countries_palestine<-Survival_Modelled%>%
#   dplyr::filter(0.690-0.05<=hdi_value &hdi_value<=0.690+0.05)%>%
#   select(country_code)%>%
#   distinct()
# 
# ES_palestine<-ES3%>%dplyr::filter(country_code%in%HDI_countries_palestine$country_code)%>%
#   dplyr::group_by(age)%>%
#   dplyr::mutate(ES=mean(ES, na.rm=T))%>%
#   dplyr::mutate(country_code=275)%>%
#   distinct()%>%
#   dplyr::mutate(ES3=ES)%>%select(-ES)
# 
# 
# 
# 
# # Puerto Rico and Guam - Using US
# ES_PR<-ES3%>%dplyr::filter(country_code==840)%>%dplyr::mutate(ES3=ES)%>%select(-ES)%>%dplyr::mutate(country_code=630)
# 
# ES_Guam<-ES3%>%dplyr::filter(country_code==840)%>%dplyr::mutate(ES3=ES)%>%select(-ES)%>%dplyr::mutate(country_code=316)
# 
# ES_Additional<-ES_France2%>%
#   full_join(ES_France3)%>%
#   full_join(ES_France4)%>%
#   full_join(ES_France5)%>%
#   full_join(ES_France6)%>%
#   full_join(ES_France7)%>%
#   full_join(ES_PR)%>%
#   full_join(ES_palestine)%>%
#   full_join(ES_Guam)

ES2<-ES3



Survival_Modelled


# Anchored and combined data set at t=5 with anchored values from Israel and Thailand

popmort<-popmort2%>%
  dplyr::filter(sex!=0)%>%
  dplyr::filter(year==2015)%>%
  dplyr::mutate(mx=1-prob)%>%
  dplyr::mutate(country_code=as.numeric(country_code))%>%
  dplyr::group_by(country_code,age,year)%>%
  dplyr::summarize(country_label, 
                   country_code,
                   age_label)%>% 
  as.data.frame()%>%
  distinct()



countries_5y<-Survival_Modelled%>%
  dplyr::mutate(rel_surv=case_when(rel_surv>1~ 1,
                                   rel_surv<=1~ rel_surv))%>%
  dplyr::mutate(country_label = str_remove( country_label,'"'))%>%
  dplyr::mutate(country_label = str_remove( country_label,"'"))%>%
  dplyr::mutate(country_label = str_remove( country_label,"`"))%>%
  dplyr::mutate(country_label = str_remove( country_label,'"'))%>%
  arrange(country_label)%>%
  distinct()



Countries_Simulated <-countries_5y%>%
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
    #   age>=16 ~ 16,
    age==16 ~ 16,
    age==17 ~ 17,
    age==18 ~ 18,
  ))%>%
  dplyr::mutate(
    age_cat = case_when(
      age>=4 & age<14 ~ "15-64",
      age>=14 ~ "65-99",
      age<4 ~"0-15"
    ))%>%
  dplyr::filter(age_cat!="0-15")%>%
  dplyr::group_by(country_label,cancer_label,age_cat)%>%
  dplyr::summarize(country_code,country_label, cancer_code, cancer_label,
                   age, age_cat,
                   rel_surv, hdi_group)%>%
  as.data.frame()




simulated_overall<-Countries_Simulated%>%
  as.data.frame()%>%
  droplevels()%>%
  dplyr::group_by(country_label,cancer_label, age_cat,age)%>%
  dplyr::summarize(country_code,country_label, cancer_code, cancer_label,rel_surv,
                   age_cat, age, hdi_group)%>%
  distinct()%>%
  arrange(country_label,cancer_label, age)



# PAF combinations

PAFs_age_Cat_RF <- PAFs%>%
  distinct()%>%
  as.data.frame()%>%
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
  left_join(ES2, by=c("country_code","age","sex"))%>%
  ungroup()%>%
  as.data.frame()%>%
  select(country_code,country_label, cancer_code, cancer_label,
         age, sex,age_cat, 
         cases, cases.prev,
         af.tob, af.alc, af.inf, af.obe, af.uv,
         cases.notprev, af.comb,
         ES)%>%
  distinct()%>%
  group_by(country_label,cancer_label, age,sex) %>%
  distinct()%>%
  as.data.frame()%>%
  group_by(country_label,cancer_label, age, sex) %>%
  # dplyr::mutate(af.comb = case_when(cases!=0 ~  sum(cases.prev, na.rm=T)/sum(cases, na.rm=T),
  #                                  cases.prev==0 ~ 0))
  dplyr::mutate(country_code,country_label, cancer_code, cancer_label,
                age, sex,age_cat, 
                cases=sum(cases, na.rm=T),
                cases.prev=sum(cases.prev, na.rm=T), 
                cases.notprev=sum(cases.notprev, na.rm=T),
                total_overall=cases,
                ES)%>%
  ungroup()%>%
  distinct()%>%
  group_by(country_label,cancer_label, age,sex) %>%
  as.data.frame()%>%
  dplyr::mutate(af.comb= case_when(cases!=0 ~  sum(cases.prev, na.rm=T)/sum(cases, na.rm=T),
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
                                 cases.prev==0 ~ 0))%>%
  distinct()%>%
  select(country_code,country_label, cancer_code, cancer_label,
         age, sex,age_cat, 
         cases, cases.prev, 
         cases.notprev, af.comb, af.tob, af.alc, af.inf, af.obe, af.uv,
         total_overall, ES)%>%
  distinct()%>%
  dplyr::group_by(country_label,cancer_label, age)


PAFs2_RF <- PAFs_age_Cat_RF%>%
  as.data.frame()%>%
  droplevels()%>%
  dplyr::group_by(country_code,cancer_code,cancer_label, age_cat,age,sex)%>%
  # dplyr::mutate(total_age_prev=sum(cases.prev, na.rm=T))%>%
  as.data.frame()%>%
  
  dplyr::group_by(country_code,cancer_code,age,sex)%>%
  dplyr::select(country_label, cancer_label,
                age_cat, age,  total_overall,cases,
                cases.prev, ES, af.comb, af.tob, af.alc, af.inf, af.obe, af.uv,)%>%
  distinct()%>%
  arrange(cancer_label,
          age)%>%
  select(-cancer_label,-country_label)


Simulated_Data_PAF_1_RF <- simulated_overall%>%
  ungroup()%>%
  dplyr::filter(age_cat!="Overall")%>%
  left_join(PAFs2_RF,by=c("country_code"="country_code",
                       "cancer_code"="cancer_code",
                       "age_cat"="age_cat",
                       "age"="age"))%>%
  ungroup()%>%
  dplyr::group_by(country_code,cancer_code, age, sex)%>%
  as.data.frame()%>%
  # dplyr::mutate(af.comb=case_when(cases!=0 ~ sum(cases.prev, na.rm=T)/sum(cases, na.rm=T),
  #                          cases==0  ~    0))%>%
  # dplyr::mutate(rel_surv=case_when(cases!=0 ~ sum(rel_surv*cases, na.rm=T)/sum(cases, na.rm=T),
  #                          cases==0  ~    rel_surv))%>%
  as.data.frame()%>%
  dplyr::group_by(country_code,cancer_code, age, sex)%>%
  dplyr::select(country_code, 
                country_label, 
                cancer_code, cancer_label,
                age_cat, age, total_overall,
                rel_surv,
                af.comb, af.tob, af.alc, af.inf, af.obe, af.uv,
                rel_surv,
                ES,
                total_overall,
                hdi_group)%>%
  droplevels()%>%
  ungroup()%>%
  distinct()%>%
  as.data.frame()


Simulated_Data_PAF_All_RF <- Simulated_Data_PAF_1_RF%>%
  dplyr::mutate(rel_surv=as.double(rel_surv))%>%
  dplyr::mutate(af.comb=as.double(af.comb))%>% 
  dplyr::mutate(total_overall=as.double(total_overall))%>% 
  arrange(country_label,cancer_code,age,sex)%>%
  left_join(Reference_Survival, by=c("age","cancer_code"))%>%
  select(-cancer_label)%>%
  left_join(globocan_cancer_names,by=c("cancer_code"))


# Avoidable deaths

# Three AD calcs 

# first need to make sure data is in right format (numeric columns)

# Applying the equation from Rutherford 2015 for AD.

#old equations###################
# dplyr::mutate(AD_prev=af.comb * total_overall * (1 - rel_surv *  ES ))%>%
#   dplyr::mutate(AD_treat=total_overall * (surv_ref-rel_surv) * ES)%>%
#   dplyr::mutate(AD_treat_not_prev = (1-af.comb)* total_overall * (surv_ref-rel_surv) * ES)%>%
#   dplyr::mutate(AD_unavoid = (1-af.comb)*total_overall*(1-surv_ref*ES))%>%
#   dplyr::mutate(Expect_deaths=(1-(rel_surv*ES))*total_overall)%>%
#########################################################

Avoidable_Deaths_Simulated_All3_RF<-Simulated_Data_PAF_All_RF%>%
  dplyr::group_by(country_code, cancer_code, age, sex)%>%
  dplyr::mutate(AD_prev= total_overall * af.comb *ES* (1 - rel_surv))%>%
  # Risk factor AD
  dplyr::mutate(AD_prev.tob = total_overall * af.tob *ES* (1 - rel_surv))%>%
  dplyr::mutate(AD_prev.alc = total_overall * af.alc *ES* (1 - rel_surv))%>%
  dplyr::mutate(AD_prev.inf = total_overall * af.inf *ES* (1 - rel_surv))%>%
  dplyr::mutate(AD_prev.obe = total_overall * af.obe *ES* (1 - rel_surv))%>%
  dplyr::mutate(AD_prev.uv = total_overall * af.uv *ES* (1 - rel_surv))%>%
  
  
  dplyr::mutate(AD_treat=(1-af.comb) *total_overall * (surv_ref-rel_surv) * ES)%>%
  #dplyr::mutate(AD_treat_not_prev = (1-af.comb) * total_overall * (surv_ref-rel_surv) * ES)%>% #scenario 1
  dplyr::mutate(AD_unavoid =total_overall*(1-ES*surv_ref-af.comb*ES*(1-surv_ref)))%>%
  #dplyr::mutate(total_deaths2=total_overall*(1-ES*rel_surv))%>%
  dplyr::mutate(total_deaths2=AD_prev+AD_treat+AD_unavoid)%>%
  select("country_code","country_label","age_cat","age","sex","cancer_code","cancer_label",   
         "AD_treat",
         # "AD_treat_not_prev",
         "AD_prev", "AD_prev.tob","AD_prev.alc","AD_prev.inf","AD_prev.obe","AD_prev.uv",
         "AD_unavoid",
         "total_deaths2",
         "total_overall",
         "hdi_group")%>%
  dplyr::rename("cancer"="cancer_label")


colorectal_RF<-Avoidable_Deaths_Simulated_All3_RF%>%
  dplyr::filter(cancer_code%in%c("8","9"))%>%
  dplyr::mutate(cancer="Colorectal",
                cancer_code=8)%>%
  dplyr::group_by(country_code, cancer_code, age, sex)%>%
  dplyr::mutate(AD_prev=sum(AD_prev),
                
                AD_prev.tob=sum(AD_prev.tob),
                AD_prev.alc=sum(AD_prev.alc),
                AD_prev.inf=sum(AD_prev.inf),
                AD_prev.obe=sum(AD_prev.obe),
                AD_prev.uv=sum(AD_prev.uv),
                
                AD_treat=sum(AD_treat),
                #  AD_treat_not_prev=sum(AD_treat_not_prev),
                AD_unavoid=sum(AD_unavoid),
                total_deaths2=sum(total_deaths2),
                total_overall=sum(total_overall))%>%
  distinct()%>%
  as.data.frame()

colorectal_RF


Avoidable_Deaths_Simulated_All2_RF<-Avoidable_Deaths_Simulated_All3_RF%>%
  dplyr::filter(!cancer_code%in%c("8","9"))%>%
  full_join(colorectal_RF)%>%
  dplyr::mutate(cancer = replace(cancer, cancer == "Liver and intrahepatic bile ducts", "Liver"))%>%
  dplyr::mutate(cancer = replace(cancer, cancer == "Trachea, bronchus and lung", "Lung"))%>%
  distinct()


Avoidable_Deaths_Simulated_All2_RF

# Globocan population data 

pop20202 <- pop2020%>%
  as.data.frame()%>%
  dplyr::filter(sex!=0)%>%
  dplyr::mutate(age = case_when(
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
    # age>=16 ~ 16,
    age==16 ~ 16,
    age==17 ~ 17,
    age==18 ~ 18,
  ))%>%
  dplyr::group_by(country_code, age,sex)%>%
  dplyr::mutate(py=sum(py))%>%
  distinct()%>%
  select(-country_label)




# Weights
w2 <- c(12000,10000,9000,9000,8000,8000,6000,6000,6000,6000,5000,4000,4000,3000,2000,1000,500,500) #SEGI_pop weights
age <- c(1:18)
weights2 <- data.frame(w2, age)


weights2 <- weights2%>%dplyr::mutate(age = case_when(
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
  #age>=16 ~ 16,
  age==16 ~ 16,
  age==17 ~ 17,
  age==18 ~ 18,
))%>%
  dplyr::filter(age>3)%>%
  dplyr::mutate(total=sum(w2))%>%
  dplyr::group_by(age)%>%
  dplyr::mutate(w=sum(w2)/total)%>%
  select(-w2, -total)%>%
  distinct()


Avoidable_Deaths_Simulated_All_RF<- Avoidable_Deaths_Simulated_All2_RF%>%
  as.data.frame()%>%
  dplyr::mutate(age=as.numeric(as.character(age)))%>%
  dplyr::mutate(AD_prev=as.numeric(as.character(AD_prev)))%>%
  dplyr::mutate(AD_unavoid=as.numeric(as.character(AD_unavoid)))%>%
  dplyr::mutate(total_overall=as.numeric(as.character(total_overall)))%>%
  dplyr::mutate(AD_treat=as.numeric(as.character(AD_treat)))%>%
  #dplyr::mutate(AD_treat_not_prev=as.numeric(as.character(AD_treat_not_prev)))%>%
  dplyr::mutate(AD_sum=AD_prev + AD_unavoid + AD_treat)%>%
  dplyr::mutate(cancer_code=as.numeric(cancer_code))%>%
  as.data.frame()%>%distinct()%>%
  dplyr::mutate(country_code=as.integer(country_code))%>%
  left_join(pop20202, c("sex", "country_code", "age"))%>%
  left_join(weights2)%>%
  dplyr::mutate(AD_treat = case_when(AD_treat<0 ~ 0, 
                                     # Numerical calculation error causes the countries which are references  to go slightly negative. 
                                     # By definition this is zero This is rounded to 0
                                     TRUE ~ AD_treat))


## added new ASR code below
Avoidable_Deaths_Simulated_All_overall_RF <- Avoidable_Deaths_Simulated_All_RF%>%
  dplyr::mutate(age_cat="Overall")%>%
  dplyr::group_by(country_code, cancer_code, age_cat)%>%
  dplyr::mutate(AD_prev.asr=sum(AD_prev/py*100000*w), #ASR calculation here
                AD_prev.tob.asr=sum(AD_prev.tob/py*100000*w), #ASR calculation here
                AD_prev.alc.asr=sum(AD_prev.alc/py*100000*w), #ASR calculation here
                AD_prev.inf.asr=sum(AD_prev.inf/py*100000*w), #ASR calculation here
                AD_prev.obe.asr=sum(AD_prev.obe/py*100000*w), #ASR calculation here
                AD_prev.uv.asr=sum(AD_prev.uv/py*100000*w), #ASR calculation here
                
                AD_treat.asr=sum(AD_treat/py*100000*w),
                AD_sum.asr=sum(AD_sum/py*100000*w),
                # AD_treat_not_prev.asr=sum(AD_treat_not_prev/py*100000*w),
                AD_unavoid.asr=sum(AD_unavoid/py*100000*w),
                total.deaths.asr=sum(total_deaths2/py*100000*w)) %>% 
  select( -total_overall)%>%
  dplyr::mutate(AD_prev = sum(AD_prev,na.rm=T))%>%
  
  dplyr::mutate(AD_prev.tob = sum(AD_prev.tob,na.rm=T))%>%
  dplyr::mutate(AD_prev.alc = sum(AD_prev.alc,na.rm=T))%>%
  dplyr::mutate(AD_prev.inf = sum(AD_prev.inf,na.rm=T))%>%
  dplyr::mutate(AD_prev.obe = sum(AD_prev.obe,na.rm=T))%>%
  dplyr::mutate(AD_prev.uv = sum(AD_prev.uv,na.rm=T))%>%
  
  
  dplyr::mutate(AD_treat = sum(AD_treat,na.rm=T))%>%
  dplyr::mutate(AD_sum = sum(AD_sum,na.rm=T))%>%
  #dplyr::mutate(AD_treat_not_prev = sum(AD_treat_not_prev,na.rm=T))%>%
  dplyr::mutate(AD_unavoid = sum(AD_unavoid,na.rm=T))%>%
  dplyr::mutate(total_deaths2=sum(total_deaths2,na.rm=T))%>%
  dplyr::mutate(py=sum(py))%>%
  select( -age,-sex)%>%
  ungroup()%>%
  dplyr::group_by(cancer_code, country_code,age_cat)%>%
  dplyr::mutate(w=sum(w/2))%>%
  distinct()%>%
  as.data.frame()


Avoidable_Deaths_Simulated_All_age_cat_RF <- Avoidable_Deaths_Simulated_All_RF%>%
  ungroup()%>%
  dplyr::group_by(country_code, cancer_code, age_cat)%>%
  dplyr::mutate(AD_prev.asr=sum(AD_prev/py*100000*w), #ASR calculation here
                AD_prev.tob.asr=sum(AD_prev.tob/py*100000*w), #ASR calculation here
                AD_prev.alc.asr=sum(AD_prev.alc/py*100000*w), #ASR calculation here
                AD_prev.inf.asr=sum(AD_prev.inf/py*100000*w), #ASR calculation here
                AD_prev.obe.asr=sum(AD_prev.obe/py*100000*w), #ASR calculation here
                AD_prev.uv.asr=sum(AD_prev.uv/py*100000*w), #ASR calculation here
                
                
                AD_treat.asr=sum(AD_treat/py*100000*w),
                AD_sum.asr=sum(AD_sum/py*100000*w),
                #  AD_treat_not_prev.asr=sum(AD_treat_not_prev/py*100000*w),
                AD_unavoid.asr=sum(AD_unavoid/py*100000*w),
                total.deaths.asr=sum(total_deaths2/py*100000*w)) %>% 
  select(-total_overall)%>%
  dplyr::mutate(AD_prev = sum(AD_prev,na.rm=T))%>%
  
  dplyr::mutate(AD_prev.tob = sum(AD_prev.tob,na.rm=T))%>%
  dplyr::mutate(AD_prev.alc = sum(AD_prev.alc,na.rm=T))%>%
  dplyr::mutate(AD_prev.inf = sum(AD_prev.inf,na.rm=T))%>%
  dplyr::mutate(AD_prev.obe = sum(AD_prev.obe,na.rm=T))%>%
  dplyr::mutate(AD_prev.uv = sum(AD_prev.uv,na.rm=T))%>%
  
  
  dplyr::mutate(AD_treat = sum(AD_treat,na.rm=T))%>%
  #dplyr::mutate(AD_treat_not_prev = sum(AD_treat_not_prev,na.rm=T))%>%
  dplyr::mutate(AD_unavoid = sum(AD_unavoid,na.rm=T))%>%
  dplyr::mutate(total_deaths2=sum(total_deaths2,na.rm=T))%>%
  dplyr::mutate(AD_sum = sum(AD_sum,na.rm=T))%>%
  dplyr::mutate(py=sum(py))%>%
  select( -age,-sex)%>%
  ungroup()%>%
  dplyr::group_by(cancer_code, country_code,age_cat)%>%
  dplyr::mutate(w=sum(w/2))%>%
  distinct()%>%
  full_join(Avoidable_Deaths_Simulated_All_overall_RF)%>%
  as.data.frame()%>%ungroup()







# Data by country, HDI, etc

AD_by_HDI_RF <- Avoidable_Deaths_Simulated_All_RF%>%
  dplyr::mutate(age_cat="Overall")%>%
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  dplyr::group_by(hdi_group, cancer, age,sex)%>%
  dplyr::mutate(py=sum(py))%>%
  dplyr::mutate(AD_treat_prev=sum(AD_prev+AD_treat))%>%
  dplyr::mutate(AD_treat=sum(AD_treat))%>%
  #dplyr::mutate(AD_treat_not_prev=sum(AD_treat_not_prev))%>%
  ungroup()%>%
  select(-country_label,-country_code,  hdi_group, cancer, cancer_code, AD_treat, AD_prev, AD_prev.tob, AD_prev.alc, AD_prev.inf, AD_prev.obe, AD_prev.uv,
         AD_unavoid, AD_sum,-sex, -age, age_cat,-total_overall)%>%
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  dplyr::mutate(hdi_group=as.numeric(hdi_group))%>%
  ungroup()%>%
  dplyr::group_by(hdi_group,cancer)%>%
  dplyr::mutate(AD_prev.asr=sum(AD_prev/py*100000*w), #ASR calculation here
                AD_prev.tob.asr=sum(AD_prev.tob/py*100000*w), #ASR calculation here
                AD_prev.alc.asr=sum(AD_prev.alc/py*100000*w), #ASR calculation here
                AD_prev.inf.asr=sum(AD_prev.inf/py*100000*w), #ASR calculation here
                AD_prev.obe.asr=sum(AD_prev.obe/py*100000*w), #ASR calculation here
                AD_prev.uv.asr=sum(AD_prev.uv/py*100000*w), #ASR calculation here
                
                AD_treat.asr=sum(AD_treat/py*100000*w),
                AD_sum.asr=sum(AD_sum/py*100000*w),
                AD_treat_prev.asr=sum((AD_treat+AD_prev)/py*100000*w),
                #  AD_treat_not_prev.asr=sum(AD_treat_not_prev/py*100000*w),
                AD_unavoid.asr=sum(AD_unavoid/py*100000*w),
                total.deaths.asr=sum(total_deaths2/py*100000*w)) %>%
  dplyr::group_by(hdi_group,cancer)%>%
  dplyr::mutate(AD_treat_prev=sum(AD_prev+AD_treat))%>%
  dplyr::mutate(total_deaths2=sum(total_deaths2))%>%
  dplyr::mutate(AD_treat=sum(AD_treat))%>%
  # dplyr::mutate(AD_treat_not_prev=sum(AD_treat_not_prev))%>%
  dplyr::mutate(AD_prev=sum(AD_prev))%>%
  dplyr::mutate(AD_prev.tob = sum(AD_prev.tob,na.rm=T))%>%
  dplyr::mutate(AD_prev.alc = sum(AD_prev.alc,na.rm=T))%>%
  dplyr::mutate(AD_prev.inf = sum(AD_prev.inf,na.rm=T))%>%
  dplyr::mutate(AD_prev.obe = sum(AD_prev.obe,na.rm=T))%>%
  dplyr::mutate(AD_prev.uv = sum(AD_prev.uv,na.rm=T))%>%
  
  
  
  dplyr::mutate(AD_unavoid=sum(AD_unavoid))%>%
  dplyr::mutate(total_deaths=total_deaths2)%>%
  dplyr::mutate(pAD_treat=AD_treat/total_deaths)%>%
  #dplyr::mutate(pAD_treat_not_prev=AD_treat_not_prev/total_deaths)%>%
  dplyr::mutate(pAD_prev=AD_prev/total_deaths)%>%
  
  dplyr::mutate(pAD_prev.tob=AD_prev.tob/total_deaths)%>%
  dplyr::mutate(pAD_prev.alc=AD_prev.alc/total_deaths)%>%
  dplyr::mutate(pAD_prev.inf=AD_prev.inf/total_deaths)%>%
  dplyr::mutate(pAD_prev.obe=AD_prev.obe/total_deaths)%>%
  dplyr::mutate(pAD_prev.uv=AD_prev.uv/total_deaths)%>%
  
  
  dplyr::mutate(pAD_unavoid=AD_unavoid/total_deaths)%>%
  dplyr::mutate(pAD_treat_prev=AD_treat_prev/total_deaths)%>%
  dplyr::mutate(w=sum(w)/n())%>%
  select(-AD_sum,-py)%>%
  ungroup()%>%
  distinct()%>%
  as.data.frame()


AD_by_HDI_RF


AD_by_HDI_all_RF <-Avoidable_Deaths_Simulated_All_RF%>%
  dplyr::mutate(age_cat="Overall")%>%
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  dplyr::group_by(hdi_group, cancer, age,sex)%>%
  dplyr::mutate(py=sum(py))%>%
  ungroup()%>%
  select(-country_label,-country_code,-cancer_code, -cancer,  hdi_group,  AD_treat, AD_prev, AD_prev.tob, AD_prev.alc, AD_prev.inf, AD_prev.obe, AD_prev.uv,
         AD_unavoid, total_deaths2, AD_sum,-sex, -age, age_cat,-total_overall)%>%
  
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  dplyr::mutate(hdi_group=as.numeric(hdi_group))%>%
  ungroup()%>%
  dplyr::group_by(hdi_group)%>%
  dplyr::mutate(AD_prev.asr=sum(AD_prev/py*100000*w), #ASR calculation here
                AD_prev.tob.asr=sum(AD_prev.tob/py*100000*w), #ASR calculation here
                AD_prev.alc.asr=sum(AD_prev.alc/py*100000*w), #ASR calculation here
                AD_prev.inf.asr=sum(AD_prev.inf/py*100000*w), #ASR calculation here
                AD_prev.obe.asr=sum(AD_prev.obe/py*100000*w), #ASR calculation here
                AD_prev.uv.asr=sum(AD_prev.uv/py*100000*w), #ASR calculation here
                
                AD_treat.asr=sum(AD_treat/py*100000*w),
                AD_treat_prev.asr=sum((AD_treat+AD_prev)/py*100000*w),
                AD_sum.asr=sum(AD_sum/py*100000*w),
                #AD_treat_not_prev.asr=sum(AD_treat_not_prev/py*100000*w),
                AD_unavoid.asr=sum(AD_unavoid/py*100000*w),
                total.deaths.asr=sum(total_deaths2/py*100000*w)) %>% 
  dplyr::mutate(AD_treat_prev=sum(AD_prev+AD_treat))%>%
  dplyr::mutate(total_deaths2=sum(total_deaths2))%>%
  dplyr::mutate(AD_treat=sum(AD_treat))%>%
  #dplyr::mutate(AD_treat_not_prev=sum(AD_treat_not_prev))%>%
  dplyr::mutate(AD_prev=sum(AD_prev))%>%
  dplyr::mutate(AD_prev.tob = sum(AD_prev.tob,na.rm=T))%>%
  dplyr::mutate(AD_prev.alc = sum(AD_prev.alc,na.rm=T))%>%
  dplyr::mutate(AD_prev.inf = sum(AD_prev.inf,na.rm=T))%>%
  dplyr::mutate(AD_prev.obe = sum(AD_prev.obe,na.rm=T))%>%
  dplyr::mutate(AD_prev.uv = sum(AD_prev.uv,na.rm=T))%>%
  
  dplyr::mutate(AD_unavoid=sum(AD_unavoid))%>%
  dplyr::mutate(total_deaths=total_deaths2)%>%
  dplyr::mutate(pAD_treat=AD_treat/total_deaths)%>%
  #dplyr::mutate(pAD_treat_not_prev=AD_treat_not_prev/total_deaths)%>%
  dplyr::mutate(pAD_prev=AD_prev/total_deaths)%>%
  
  dplyr::mutate(pAD_prev.tob=AD_prev.tob/total_deaths)%>%
  dplyr::mutate(pAD_prev.alc=AD_prev.alc/total_deaths)%>%
  dplyr::mutate(pAD_prev.inf=AD_prev.inf/total_deaths)%>%
  dplyr::mutate(pAD_prev.obe=AD_prev.obe/total_deaths)%>%
  dplyr::mutate(pAD_prev.uv=AD_prev.uv/total_deaths)%>%
  
  
  
  dplyr::mutate(pAD_unavoid=AD_unavoid/total_deaths)%>%
  dplyr::mutate(pAD_treat_prev=AD_treat_prev/total_deaths)%>%
  dplyr::mutate(cancer="All Cancer Sites")%>%
  dplyr::mutate(w=sum(w)/n())%>%
  select(-AD_sum,-py)%>%
  
  ungroup()%>%
  distinct()%>%
  as.data.frame()


AD_by_HDI_all_RF 


# By country for all cancer sites (number and proportion): 

AD_country_all_cancers_RF <-Avoidable_Deaths_Simulated_All_RF%>%
  dplyr::mutate(age_cat="Overall")%>%
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  dplyr::group_by(country_label, cancer, age,sex)%>%
  dplyr::mutate(py=sum(py))%>%
  ungroup()%>%
  select(-age,-sex,-total_overall,-AD_sum)%>% #-af.comb,
  dplyr::mutate(AD_treat_prev=AD_treat+AD_prev)%>%
  ungroup()%>%
  select(  -hdi_group,  )%>%
  dplyr::group_by(country_code)%>%
  dplyr::mutate(AD_prev.asr=sum(AD_prev/py*100000*w), #ASR calculation here
                AD_prev.tob.asr=sum(AD_prev.tob/py*100000*w), #ASR calculation here
                AD_prev.alc.asr=sum(AD_prev.alc/py*100000*w), #ASR calculation here
                AD_prev.inf.asr=sum(AD_prev.inf/py*100000*w), #ASR calculation here
                AD_prev.obe.asr=sum(AD_prev.obe/py*100000*w), #ASR calculation here
                AD_prev.uv.asr=sum(AD_prev.uv/py*100000*w), #ASR calculation here
                
                AD_treat.asr=sum(AD_treat/py*100000*w),
                AD_treat_prev.asr=sum((AD_treat+AD_prev)/py*100000*w),
                # AD_treat_not_prev.asr=sum(AD_treat_not_prev/py*100000*w),
                AD_unavoid.asr=sum(AD_unavoid/py*100000*w),
                total.deaths.asr=sum(total_deaths2/py*100000*w)) %>% 
  dplyr::mutate(total_deaths=sum(AD_prev, AD_unavoid, AD_treat,na.rm=T))%>%
  dplyr::mutate(total_deaths2=sum(total_deaths2,na.rm=T))%>%
  dplyr::mutate(AD_treat_prev=sum(AD_treat,AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_treat=sum(AD_treat,na.rm=T))%>%
  #dplyr::mutate(AD_treat_not_prev=sum(AD_treat_not_prev,na.rm=T))%>%
  dplyr::mutate(AD_prev=sum(AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_prev.tob = sum(AD_prev.tob,na.rm=T))%>%
  dplyr::mutate(AD_prev.alc = sum(AD_prev.alc,na.rm=T))%>%
  dplyr::mutate(AD_prev.inf = sum(AD_prev.inf,na.rm=T))%>%
  dplyr::mutate(AD_prev.obe = sum(AD_prev.obe,na.rm=T))%>%
  dplyr::mutate(AD_prev.uv = sum(AD_prev.uv,na.rm=T))%>%
  dplyr::mutate(AD_unavoid=sum(AD_unavoid,na.rm=T))%>%
  dplyr::mutate(pAD_treat=AD_treat/total_deaths)%>%
  #dplyr::mutate(pAD_treat_not_prev=AD_treat_not_prev/total_deaths)%>%
  dplyr::mutate(pAD_prev=AD_prev/total_deaths)%>%
  
  dplyr::mutate(pAD_prev.tob=AD_prev.tob/total_deaths)%>%
  dplyr::mutate(pAD_prev.alc=AD_prev.alc/total_deaths)%>%
  dplyr::mutate(pAD_prev.inf=AD_prev.inf/total_deaths)%>%
  dplyr::mutate(pAD_prev.obe=AD_prev.obe/total_deaths)%>%
  dplyr::mutate(pAD_prev.uv=AD_prev.uv/total_deaths)%>%
  
  dplyr::mutate(pAD_unavoid=AD_unavoid/total_deaths)%>%
  dplyr::mutate(pAD_treat_prev=AD_treat_prev/total_deaths)%>%
  dplyr::mutate(cancer="All Cancer Sites")%>%
  dplyr::mutate(cancer_code=1000)%>%
  select(-py,-w)%>%
  distinct()%>%
  ungroup()%>%
  distinct()%>%
  as.data.frame()


# By cancer site

AD_cancer_RF <- Avoidable_Deaths_Simulated_All_RF%>%
  dplyr::mutate(age_cat="Overall")%>%
  dplyr::group_by(cancer, age,sex)%>%
  dplyr::mutate(py=sum(py))%>%
  ungroup()%>%
  dplyr::mutate(AD_treat_prev=AD_treat+AD_prev)%>%
  ungroup()%>%
  select(-hdi_group,  -AD_sum,-age,-sex)%>%
  dplyr::mutate(country_code=1000)%>%
  dplyr::mutate(country_label="All Countries")%>%
  dplyr::group_by(cancer)%>%
  dplyr::mutate(AD_prev.asr=sum(AD_prev/py*100000*w), #ASR calculation here
                AD_prev.tob.asr=sum(AD_prev.tob/py*100000*w), #ASR calculation here
                AD_prev.alc.asr=sum(AD_prev.alc/py*100000*w), #ASR calculation here
                AD_prev.inf.asr=sum(AD_prev.inf/py*100000*w), #ASR calculation here
                AD_prev.obe.asr=sum(AD_prev.obe/py*100000*w), #ASR calculation here
                AD_prev.uv.asr=sum(AD_prev.uv/py*100000*w), #ASR calculation here
                
                AD_treat.asr=sum(AD_treat/py*100000*w),
                AD_treat_prev.asr=sum((AD_treat+AD_prev)/py*100000*w),
                # AD_treat_not_prev.asr=sum(AD_treat_not_prev/py*100000*w),
                AD_unavoid.asr=sum(AD_unavoid/py*100000*w),
                total.deaths.asr=sum(total_deaths2/py*100000*w)) %>% 
  dplyr::mutate(total_deaths=sum(total_deaths2,na.rm=T))%>%
  select(-total_deaths2)%>%
  dplyr::mutate(AD_treat_prev=sum(AD_treat,AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_treat=sum(AD_treat,na.rm=T))%>%
  #dplyr::mutate(AD_treat_not_prev=sum(AD_treat_not_prev,na.rm=T))%>%
  dplyr::mutate(AD_prev=sum(AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_prev.tob = sum(AD_prev.tob,na.rm=T))%>%
  dplyr::mutate(AD_prev.alc = sum(AD_prev.alc,na.rm=T))%>%
  dplyr::mutate(AD_prev.inf = sum(AD_prev.inf,na.rm=T))%>%
  dplyr::mutate(AD_prev.obe = sum(AD_prev.obe,na.rm=T))%>%
  dplyr::mutate(AD_prev.uv = sum(AD_prev.uv,na.rm=T))%>%
  dplyr::mutate(AD_unavoid=sum(AD_unavoid,na.rm=T))%>%
  dplyr::mutate(pAD_treat=AD_treat/total_deaths)%>%
  # dplyr::mutate(pAD_treat_not_prev=AD_treat_not_prev/total_deaths)%>%
  dplyr::mutate(pAD_prev=AD_prev/total_deaths)%>%
  dplyr::mutate(pAD_prev.tob=AD_prev.tob/total_deaths)%>%
  dplyr::mutate(pAD_prev.alc=AD_prev.alc/total_deaths)%>%
  dplyr::mutate(pAD_prev.inf=AD_prev.inf/total_deaths)%>%
  dplyr::mutate(pAD_prev.obe=AD_prev.obe/total_deaths)%>%
  dplyr::mutate(pAD_prev.uv=AD_prev.uv/total_deaths)%>%
  dplyr::mutate(pAD_unavoid=AD_unavoid/total_deaths)%>%
  dplyr::mutate(pAD_treat_prev=AD_treat_prev/total_deaths)%>%
  select(-py,-w,-total_overall)%>%
  distinct()%>%
  ungroup()%>%
  distinct()%>%
  as.data.frame()



# Calculating by region. Need a file that links countries to region 
HDI_Region_Mapping2 <- HDI_Region_Mapping%>%
  select(-country_label)%>%
  dplyr::filter(area<=21)

areas <- HDI_Region_Mapping%>%
  dplyr::filter(country_code>=910& country_code<=931 | 
                  country_code==905 | country_code==906| 
                  country_code==954| country_code==957 )%>%
  select(area, country_label)



# By region
AD_Region2_RF <- Avoidable_Deaths_Simulated_All_RF%>%
  left_join(HDI_Region_Mapping2, by=c("country_code"))%>%
  dplyr::mutate(age_cat="Overall")%>%
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  dplyr::group_by(area, cancer, age,sex)%>%
  dplyr::mutate(py=sum(py))%>%
  ungroup()%>%
  select(-country_label,-country_code,-cancer_code, -cancer,  hdi_group,  AD_treat, AD_prev, 
         AD_unavoid, total_deaths2, -sex, -age, age_cat,-total_overall)%>%
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  dplyr::mutate(hdi_group=as.numeric(hdi_group))%>%
  ungroup()%>%  dplyr::mutate(AD_treat_prev=AD_treat+AD_prev)%>%
  ungroup()%>%
  select(-hdi_group,-continent,   -AD_sum)%>%
  dplyr::group_by(area)%>%
  dplyr::mutate(AD_prev.asr=sum(AD_prev/py*100000*w), #ASR calculation here
                AD_prev.tob.asr=sum(AD_prev.tob/py*100000*w), #ASR calculation here
                AD_prev.alc.asr=sum(AD_prev.alc/py*100000*w), #ASR calculation here
                AD_prev.inf.asr=sum(AD_prev.inf/py*100000*w), #ASR calculation here
                AD_prev.obe.asr=sum(AD_prev.obe/py*100000*w), #ASR calculation here
                AD_prev.uv.asr=sum(AD_prev.uv/py*100000*w), #ASR calculation here
                AD_treat.asr=sum(AD_treat/py*100000*w),
                AD_treat_prev.asr=sum((AD_treat+AD_prev)/py*100000*w),
                #   AD_treat_not_prev.asr=sum(AD_treat_not_prev/py*100000*w),
                AD_unavoid.asr=sum(AD_unavoid/py*100000*w),
                total.deaths.asr=sum(total_deaths2/py*100000*w)) %>% 
  dplyr::mutate(total_deaths=sum(total_deaths2,na.rm=T))%>%
  select(-total_deaths2)%>%
  dplyr::mutate(AD_treat_prev=sum(AD_treat,AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_treat=sum(AD_treat,na.rm=T))%>%
  # dplyr::mutate(AD_treat_not_prev=sum(AD_treat_not_prev,na.rm=T))%>%
  dplyr::mutate(AD_prev=sum(AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_prev.tob = sum(AD_prev.tob,na.rm=T))%>%
  dplyr::mutate(AD_prev.alc = sum(AD_prev.alc,na.rm=T))%>%
  dplyr::mutate(AD_prev.inf = sum(AD_prev.inf,na.rm=T))%>%
  dplyr::mutate(AD_prev.obe = sum(AD_prev.obe,na.rm=T))%>%
  dplyr::mutate(AD_prev.uv = sum(AD_prev.uv,na.rm=T))%>%
  dplyr::mutate(AD_unavoid=sum(AD_unavoid,na.rm=T))%>%
  dplyr::mutate(pAD_treat=AD_treat/total_deaths)%>%
  #dplyr::mutate(pAD_treat_not_prev=AD_treat_not_prev/total_deaths)%>%
  dplyr::mutate(pAD_prev=AD_prev/total_deaths)%>%
  dplyr::mutate(pAD_prev.tob=AD_prev.tob/total_deaths)%>%
  dplyr::mutate(pAD_prev.alc=AD_prev.alc/total_deaths)%>%
  dplyr::mutate(pAD_prev.inf=AD_prev.inf/total_deaths)%>%
  dplyr::mutate(pAD_prev.obe=AD_prev.obe/total_deaths)%>%
  dplyr::mutate(pAD_prev.uv=AD_prev.uv/total_deaths)%>%
  dplyr::mutate(pAD_unavoid=AD_unavoid/total_deaths)%>%
  dplyr::mutate(pAD_treat_prev=AD_treat_prev/total_deaths)%>%
  dplyr::mutate(cancer="All Cancer Sites")%>%
  dplyr::mutate(cancer_code=1000)%>%
  select(-py,-w)%>%
  distinct()%>%
  left_join(areas, by=c("area"))%>%
  ungroup()%>%
  distinct()%>%
  left_join(HDI_Region_Mapping, by=c("area", "country_label"))%>%
  as.data.frame()

#region and cancer site
AD_Region_cancer_sites_RF <- Avoidable_Deaths_Simulated_All_RF%>%
  left_join(HDI_Region_Mapping2, by=c("country_code"))%>%
  dplyr::mutate(age_cat="Overall")%>%
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  dplyr::group_by(area, cancer, age,sex)%>%
  dplyr::mutate(py=sum(py))%>%
  ungroup()%>%
  select(-country_label,-country_code,-cancer_code, hdi_group,  AD_treat, AD_prev, 
         AD_unavoid, total_deaths2, -sex, -age, age_cat,-total_overall)%>%
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  dplyr::mutate(hdi_group=as.numeric(hdi_group))%>%
  ungroup()%>%  dplyr::mutate(AD_treat_prev=AD_treat+AD_prev)%>%
  ungroup()%>%
  select(-hdi_group,-continent,   -AD_sum)%>%
  dplyr::group_by(area,cancer)%>%
  dplyr::mutate(AD_prev.asr=sum(AD_prev/py*100000*w), #ASR calculation here
                AD_prev.tob.asr=sum(AD_prev.tob/py*100000*w), #ASR calculation here
                AD_prev.alc.asr=sum(AD_prev.alc/py*100000*w), #ASR calculation here
                AD_prev.inf.asr=sum(AD_prev.inf/py*100000*w), #ASR calculation here
                AD_prev.obe.asr=sum(AD_prev.obe/py*100000*w), #ASR calculation here
                AD_prev.uv.asr=sum(AD_prev.uv/py*100000*w), #ASR calculation here
                AD_treat.asr=sum(AD_treat/py*100000*w),
                AD_treat_prev.asr=sum((AD_treat+AD_prev)/py*100000*w),
                #AD_treat_not_prev.asr=sum(AD_treat_not_prev/py*100000*w),
                AD_unavoid.asr=sum(AD_unavoid/py*100000*w),
                total.deaths.asr=sum(total_deaths2/py*100000*w)) %>% 
  dplyr::mutate(total_deaths=sum(total_deaths2,na.rm=T))%>%
  select(-total_deaths2)%>%
  dplyr::mutate(AD_treat_prev=sum(AD_treat,AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_treat=sum(AD_treat,na.rm=T))%>%
  #dplyr::mutate(AD_treat_not_prev=sum(AD_treat_not_prev,na.rm=T))%>%
  dplyr::mutate(AD_prev=sum(AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_prev.tob = sum(AD_prev.tob,na.rm=T))%>%
  dplyr::mutate(AD_prev.alc = sum(AD_prev.alc,na.rm=T))%>%
  dplyr::mutate(AD_prev.inf = sum(AD_prev.inf,na.rm=T))%>%
  dplyr::mutate(AD_prev.obe = sum(AD_prev.obe,na.rm=T))%>%
  dplyr::mutate(AD_prev.uv = sum(AD_prev.uv,na.rm=T))%>%
  dplyr::mutate(AD_unavoid=sum(AD_unavoid,na.rm=T))%>%
  dplyr::mutate(pAD_treat=AD_treat/total_deaths)%>%
  #dplyr::mutate(pAD_treat_not_prev=AD_treat_not_prev/total_deaths)%>%
  dplyr::mutate(pAD_prev=AD_prev/total_deaths)%>%
  dplyr::mutate(pAD_prev.tob=AD_prev.tob/total_deaths)%>%
  dplyr::mutate(pAD_prev.alc=AD_prev.alc/total_deaths)%>%
  dplyr::mutate(pAD_prev.inf=AD_prev.inf/total_deaths)%>%
  dplyr::mutate(pAD_prev.obe=AD_prev.obe/total_deaths)%>%
  dplyr::mutate(pAD_prev.uv=AD_prev.uv/total_deaths)%>%
  dplyr::mutate(pAD_unavoid=AD_unavoid/total_deaths)%>%
  dplyr::mutate(pAD_treat_prev=AD_treat_prev/total_deaths)%>%
  select(-py,-w)%>%
  distinct()%>%
  left_join(areas, by=c("area"))%>%
  ungroup()%>%
  distinct()%>%
  left_join(HDI_Region_Mapping, by=c("area", "country_label"))%>%
  as.data.frame()%>%
  dplyr::group_by(area)

# age standardizing by region - aggregate by region and then age standardize

countries_regions<-Avoidable_Deaths_Simulated_All_RF%>%
  select(country_code)%>%distinct()%>%
  left_join(HDI_Region_Mapping2, by=c("country_code"))


# Table 1 in the manuscript

# Gives us number, percentage of total deaths 
table_1_1_RF <- Avoidable_Deaths_Simulated_All_RF %>%
  select(-country_label, -country_code, -cancer_code, hdi_group)%>%
  dplyr::mutate(age_cat="Overall")%>%
  dplyr::group_by(cancer, age,sex)%>%
  dplyr::mutate(py=sum(py))%>%
  ungroup()%>%
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  dplyr::mutate(hdi_group=as.numeric(hdi_group))%>%
  ungroup()%>%  dplyr::mutate(AD_treat_prev=AD_treat+AD_prev)%>%
  ungroup()%>%
  dplyr::mutate(AD_prev.asr=sum(AD_prev/py*100000*w), #ASR calculation here
                AD_prev.tob.asr=sum(AD_prev.tob/py*100000*w), #ASR calculation here
                AD_prev.alc.asr=sum(AD_prev.alc/py*100000*w), #ASR calculation here
                AD_prev.inf.asr=sum(AD_prev.inf/py*100000*w), #ASR calculation here
                AD_prev.obe.asr=sum(AD_prev.obe/py*100000*w), #ASR calculation here
                AD_prev.uv.asr=sum(AD_prev.uv/py*100000*w), #ASR calculation here
                
                AD_treat.asr=sum(AD_treat/py*100000*w),
                AD_treat_prev.asr=sum((AD_treat + AD_prev)/py*100000*w),
                #AD_treat_not_prev.asr=sum(AD_treat_not_prev/py*100000*w),
                AD_unavoid.asr=sum(AD_unavoid/py*100000*w),
                total.deaths.asr=sum(total_deaths2/py*100000*w)) %>% 
  ungroup()%>%
  dplyr::mutate(total_deaths=sum(total_deaths2,na.rm=T))%>%
  dplyr::mutate(AD_treat_prev=sum(AD_treat,AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_treat=sum(AD_treat,na.rm=T))%>%
  #dplyr::mutate(AD_treat_not_prev=sum(AD_treat_not_prev,na.rm=T))%>%
  dplyr::mutate(AD_prev=sum(AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_prev.tob = sum(AD_prev.tob,na.rm=T))%>%
  dplyr::mutate(AD_prev.alc = sum(AD_prev.alc,na.rm=T))%>%
  dplyr::mutate(AD_prev.inf = sum(AD_prev.inf,na.rm=T))%>%
  dplyr::mutate(AD_prev.obe = sum(AD_prev.obe,na.rm=T))%>%
  dplyr::mutate(AD_prev.uv = sum(AD_prev.uv,na.rm=T))%>%
  
  dplyr::mutate(AD_unavoid=sum(AD_unavoid,na.rm=T))%>%
  dplyr::mutate(pAD_treat=AD_treat/total_deaths)%>%
  #dplyr::mutate(pAD_treat_not_prev=AD_treat_not_prev/total_deaths)%>%
  dplyr::mutate(pAD_prev=AD_prev/total_deaths)%>%
  
  dplyr::mutate(pAD_prev.tob=AD_prev.tob/total_deaths)%>%
  dplyr::mutate(pAD_prev.alc=AD_prev.alc/total_deaths)%>%
  dplyr::mutate(pAD_prev.inf=AD_prev.inf/total_deaths)%>%
  dplyr::mutate(pAD_prev.obe=AD_prev.obe/total_deaths)%>%
  dplyr::mutate(pAD_prev.uv=AD_prev.uv/total_deaths)%>%
  
  dplyr::mutate(pAD_unavoid=AD_unavoid/total_deaths)%>%
  dplyr::mutate(pAD_treat_prev=AD_treat_prev/total_deaths)%>%
  select(AD_prev,  AD_prev.tob, AD_prev.alc, AD_prev.inf, AD_prev.obe, AD_prev.uv,
         AD_treat, 
         # AD_treat_not_prev,
         AD_treat_prev, AD_unavoid, total_deaths, 
         pAD_treat, 
         # pAD_treat_not_prev,
         pAD_prev,pAD_prev.tob, pAD_prev.alc, pAD_prev.inf, pAD_prev.obe, pAD_prev.uv,
         pAD_treat_prev, pAD_unavoid,
         AD_prev.asr, AD_prev.tob.asr, AD_prev.alc.asr,AD_prev.inf.asr,AD_prev.obe.asr,AD_prev.uv.asr,
         AD_treat.asr ,
         #AD_treat_not_prev.asr,
         AD_treat_prev.asr, AD_unavoid.asr,total.deaths.asr)%>%
  distinct()


# By Country and all cancer sites

AD_country_all_cancers2_RF<-AD_country_all_cancers_RF%>%  
  ungroup()%>%
  distinct()%>%
  dplyr::mutate(across(6:10,round, -2))%>%
  dplyr::mutate(across(11:15,round, 1))%>%
  dplyr::mutate(across(16:16,round, -2))%>%
  dplyr::mutate(across(17:20, round,3)*100)%>% #dplyr::mutate to show proportion as percentage in export
  select("country_code","country_label",
         "cancer_code", "cancer", 
         "AD_prev",        "pAD_prev",    "AD_prev.asr",
         "AD_prev.tob", "pAD_prev.tob",   "AD_prev.tob.asr",
         "AD_prev.alc", "pAD_prev.alc",  "AD_prev.alc.asr",
         "AD_prev.inf", "pAD_prev.inf",   "AD_prev.inf.asr",
         "AD_prev.obe", "pAD_prev.obe",  "AD_prev.obe.asr",
         "AD_prev.uv",  "pAD_prev.uv",   "AD_prev.uv.asr",
         "AD_treat",       "pAD_treat" ,"AD_treat.asr",
         "AD_treat_prev",  "pAD_treat_prev","AD_treat_prev.asr",
         "AD_unavoid",     "pAD_unavoid" ,   "AD_unavoid.asr",    
         "total_deaths","total.deaths.asr")


# Gives us number, percentage of total deaths 
table_1_1_RF <- Avoidable_Deaths_Simulated_All_RF %>%
  select(-country_label, -country_code, -cancer_code, hdi_group)%>%
  dplyr::mutate(age_cat="Overall")%>%
  dplyr::group_by(cancer, age,sex)%>%
  dplyr::mutate(py=sum(py))%>%
  ungroup()%>%
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  dplyr::mutate(hdi_group=as.numeric(hdi_group))%>%
  ungroup()%>%  dplyr::mutate(AD_treat_prev=AD_treat+AD_prev)%>%
  ungroup()%>%
  dplyr::mutate(AD_prev.asr=sum(AD_prev/py*100000*w), #ASR calculation here
                AD_prev.tob.asr=sum(AD_prev.tob/py*100000*w), #ASR calculation here
                AD_prev.alc.asr=sum(AD_prev.alc/py*100000*w), #ASR calculation here
                AD_prev.inf.asr=sum(AD_prev.inf/py*100000*w), #ASR calculation here
                AD_prev.obe.asr=sum(AD_prev.obe/py*100000*w), #ASR calculation here
                AD_prev.uv.asr=sum(AD_prev.uv/py*100000*w), #ASR calculation here
                
                AD_treat.asr=sum(AD_treat/py*100000*w),
                AD_treat_prev.asr=sum((AD_treat+AD_prev)/py*100000*w),
                #  AD_treat_not_prev.asr=sum(AD_treat_not_prev/py*100000*w),
                AD_unavoid.asr=sum(AD_unavoid/py*100000*w),
                total.deaths.asr=sum(total_deaths2/py*100000*w)) %>% 
  ungroup()%>%
  dplyr::mutate(total_deaths=sum(total_deaths2,na.rm=T))%>%
  dplyr::mutate(AD_treat_prev=sum(AD_treat,AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_treat=sum(AD_treat,na.rm=T))%>%
  #dplyr::mutate(AD_treat_not_prev=sum(AD_treat_not_prev,na.rm=T))%>%
  dplyr::mutate(AD_prev=sum(AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_prev.tob = sum(AD_prev.tob,na.rm=T))%>%
  dplyr::mutate(AD_prev.alc = sum(AD_prev.alc,na.rm=T))%>%
  dplyr::mutate(AD_prev.inf = sum(AD_prev.inf,na.rm=T))%>%
  dplyr::mutate(AD_prev.obe = sum(AD_prev.obe,na.rm=T))%>%
  dplyr::mutate(AD_prev.uv = sum(AD_prev.uv,na.rm=T))%>%
  
  dplyr::mutate(AD_unavoid=sum(AD_unavoid,na.rm=T))%>%
  dplyr::mutate(pAD_treat=AD_treat/total_deaths)%>%
  # dplyr::mutate(pAD_treat_not_prev=AD_treat_not_prev/total_deaths)%>%
  dplyr::mutate(pAD_prev=AD_prev/total_deaths)%>%
  
  dplyr::mutate(pAD_prev.tob=AD_prev.tob/total_deaths)%>%
  dplyr::mutate(pAD_prev.alc=AD_prev.alc/total_deaths)%>%
  dplyr::mutate(pAD_prev.inf=AD_prev.inf/total_deaths)%>%
  dplyr::mutate(pAD_prev.obe=AD_prev.obe/total_deaths)%>%
  dplyr::mutate(pAD_prev.uv=AD_prev.uv/total_deaths)%>%
  
  dplyr::mutate(pAD_unavoid=AD_unavoid/total_deaths)%>%
  dplyr::mutate(pAD_treat_prev=AD_treat_prev/total_deaths)%>%
  select(AD_prev,  AD_prev.tob, AD_prev.alc, AD_prev.inf, AD_prev.obe, AD_prev.uv,
         AD_treat, 
         # AD_treat_not_prev,
         AD_treat_prev, AD_unavoid, total_deaths, 
         pAD_treat, 
         # pAD_treat_not_prev,
         pAD_prev,pAD_prev.tob, pAD_prev.alc, pAD_prev.inf, pAD_prev.obe, pAD_prev.uv,
         pAD_treat_prev, pAD_unavoid,
         AD_prev.asr, AD_prev.tob.asr, AD_prev.alc.asr,AD_prev.inf.asr,AD_prev.obe.asr,AD_prev.uv.asr,
         AD_treat.asr ,
         #AD_treat_not_prev.asr,
         AD_treat_prev.asr, AD_unavoid.asr,total.deaths.asr)%>%
  distinct()


# By country AND cancer site
# AD by country and cancer site

Avoidable_Deaths_Simulated_All_RF
Avoidable_Deaths_Simulated_All_age_cat_RF
Avoidable_Deaths_Simulated_All_age_cat_overall_RF<-Avoidable_Deaths_Simulated_All_age_cat_RF%>% #Object to plot overall age standardized on world maps 
  as.data.frame()%>%
  dplyr::filter(age_cat=="Overall")%>% 
  ungroup()%>%
  dplyr::group_by(country_code,cancer_code)%>%
  dplyr::mutate(total_deaths=sum(total_deaths2,na.rm=T))%>%
  #select(-total_deaths2,-AD_sum)%>%
  dplyr::mutate(AD_treat_prev=sum(AD_treat, AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_treat=sum(AD_treat,na.rm=T))%>%
  #dplyr::mutate(AD_treat_not_prev=sum(AD_treat_not_prev,na.rm=T))%>%
  dplyr::mutate(AD_prev=sum(AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_prev.tob = sum(AD_prev.tob,na.rm=T))%>%
  dplyr::mutate(AD_prev.alc = sum(AD_prev.alc,na.rm=T))%>%
  dplyr::mutate(AD_prev.inf = sum(AD_prev.inf,na.rm=T))%>%
  dplyr::mutate(AD_prev.obe = sum(AD_prev.obe,na.rm=T))%>%
  dplyr::mutate(AD_prev.uv = sum(AD_prev.uv,na.rm=T))%>%
  dplyr::mutate(AD_unavoid=sum(AD_unavoid,na.rm=T))%>%
  dplyr::mutate(pAD_treat=AD_treat/total_deaths)%>%
  #dplyr::mutate(pAD_treat_not_prev=AD_treat_not_prev/total_deaths)%>%
  dplyr::mutate(pAD_prev=AD_prev/total_deaths)%>%
  dplyr::mutate(pAD_prev.tob=AD_prev.tob/total_deaths)%>%
  dplyr::mutate(pAD_prev.alc=AD_prev.alc/total_deaths)%>%
  dplyr::mutate(pAD_prev.inf=AD_prev.inf/total_deaths)%>%
  dplyr::mutate(pAD_prev.obe=AD_prev.obe/total_deaths)%>%
  dplyr::mutate(pAD_prev.uv=AD_prev.uv/total_deaths)%>%
  dplyr::mutate(pAD_unavoid=AD_unavoid/total_deaths)%>%
  dplyr::mutate(pAD_treat_prev=AD_treat_prev/total_deaths)%>%
  as.data.frame()%>%
  select( "country_code","country_label","cancer_code", "cancer", 
          "AD_prev","pAD_prev",  
          "AD_prev.tob", "pAD_prev.tob",
          "AD_prev.alc", "pAD_prev.alc", 
          "AD_prev.inf", "pAD_prev.inf", 
          "AD_prev.obe", "pAD_prev.obe",  
          "AD_prev.uv",  "pAD_prev.uv",  
          "AD_treat",  "pAD_treat" ,
          # AD_treat_not_prev, pAD_treat_not_prev,
          "AD_treat_prev", "pAD_treat_prev",
          "AD_unavoid",   "pAD_unavoid" ,        
          "total_deaths"
  )%>%
  dplyr::mutate(across(5:5,round,0 ))%>%
  dplyr::mutate(across(6:6, round,4)*100)%>% 
  dplyr::mutate(across(7:7,round,0 ))%>%
  dplyr::mutate(across(8:8, round,4)*100)%>% 
  dplyr::mutate(across(9:9,round,0 ))%>%
  dplyr::mutate(across(10:10, round,4)*100)%>% 
  dplyr::mutate(across(11:11,round,0 ))%>%
  dplyr::mutate(across(12:12, round,4)*100)%>% 
  dplyr::mutate(across(13:13,round,0 ))%>%
  dplyr::mutate(across(14:14, round,4)*100)%>% 
  dplyr::mutate(across(15:15,round,0 ))%>%
  dplyr::mutate(across(16:16, round,4)*100)%>% 
  dplyr::mutate(across(17:17,round,0 ))%>%
  dplyr::mutate(across(18:18, round,4)*100)%>% 
  dplyr::mutate(across(19:19,round,0 ))%>%
  dplyr::mutate(across(20:20, round,4)*100)%>% 
  dplyr::mutate(across(21:21,round,0 ))%>%
  dplyr::mutate(across(22:22, round,4)*100)%>% 
  dplyr::mutate(across(23:23,round,0 ))



#By cancer site

AD_cancer2_RF <- AD_cancer_RF%>%
  select( "country_code", "country_label",
          "cancer_code", "cancer", 
          "AD_prev",     
          "AD_prev.tob", 
          "AD_prev.alc",
          "AD_prev.inf",
          "AD_prev.obe",   
          "AD_prev.uv", 
          "AD_treat",   
          "AD_treat_prev",  
          "AD_unavoid",     
          "total_deaths",
          "pAD_prev",     "pAD_prev.tob",   "pAD_prev.alc",  
          "pAD_prev.inf",  "pAD_prev.obe",   "pAD_prev.uv", 
          "pAD_treat" ,"pAD_treat_prev","pAD_unavoid" ,     
          "AD_prev.asr",
          "AD_prev.tob.asr",
          "AD_prev.alc.asr",
          "AD_prev.inf.asr", 
          "AD_prev.obe.asr",
          "AD_prev.uv.asr",
          "AD_treat.asr",
          "AD_treat_prev.asr",
          "AD_unavoid.asr",  
          "total.deaths.asr",
  )%>%
  as.data.frame()%>%
  dplyr::mutate(across(5:9,round, -2))%>%
  
  dplyr::mutate(across(10:13, round,3)*100)%>%#%>% #dplyr::mutate to show proportion as percentage in export
  dplyr::mutate(across(14:18,round, 1))%>%
  select("country_code","country_label",
         "cancer_code", "cancer", 
         "AD_prev",        "pAD_prev",    "AD_prev.asr",
         "AD_prev.tob", "pAD_prev.tob",   "AD_prev.tob.asr",
         "AD_prev.alc", "pAD_prev.alc",  "AD_prev.alc.asr",
         "AD_prev.inf", "pAD_prev.inf",   "AD_prev.inf.asr",
         "AD_prev.obe", "pAD_prev.obe",  "AD_prev.obe.asr",
         "AD_prev.uv",  "pAD_prev.uv",   "AD_prev.uv.asr",
         "AD_treat",       "pAD_treat" ,"AD_treat.asr",
         "AD_treat_prev",  "pAD_treat_prev","AD_treat_prev.asr",
         "AD_unavoid",     "pAD_unavoid" ,   "AD_unavoid.asr",    
         "total_deaths","total.deaths.asr")







#By region
AD_Region_RF<-AD_Region2_RF%>%
  dplyr::mutate(across(2:4, round, -2))%>%
  dplyr::mutate(across(6:6,round, -2))%>%
  dplyr::mutate(across(7:11,round, 1))%>%
  dplyr::mutate(across(12:12,round, -2))%>%
  dplyr::mutate(across(13:16, round,3)*100)%>% #dplyr::mutate to show proportion as percentage in export
  arrange(continent, country_label)%>%
  select("continent","area","country_label","age_cat", "cancer_code", "cancer", 
         "AD_prev",        "pAD_prev",    "AD_prev.asr",
         "AD_prev.tob", "pAD_prev.tob",   "AD_prev.tob.asr",
         "AD_prev.alc", "pAD_prev.alc",  "AD_prev.alc.asr",
         "AD_prev.inf", "pAD_prev.inf",   "AD_prev.inf.asr",
         "AD_prev.obe", "pAD_prev.obe",  "AD_prev.obe.asr",
         "AD_prev.uv",  "pAD_prev.uv",   "AD_prev.uv.asr",
         "AD_treat",       "pAD_treat" ,"AD_treat.asr",
         "AD_treat_prev",  "pAD_treat_prev","AD_treat_prev.asr",
         "AD_unavoid",     "pAD_unavoid" ,   "AD_unavoid.asr",    
         "total_deaths","total.deaths.asr")



#World total

table_1_11_RF<-table_1_1_RF%>%
  dplyr::mutate(across(1:10,round, -2))%>%
  dplyr::mutate(across(11:19, round,3)*100)%>% #dplyr::mutate to show proportion as percentage in export
  dplyr::mutate(across(20:29,round, 1))%>%
#  dplyr::mutate(across(31:31,round, -2))%>%
  select(
    "AD_prev",     "pAD_prev",    "AD_prev.asr",
    "AD_prev.tob", "pAD_prev.tob",   "AD_prev.tob.asr",
    "AD_prev.alc", "pAD_prev.alc",  "AD_prev.alc.asr",
    "AD_prev.inf", "pAD_prev.inf",   "AD_prev.inf.asr",
    "AD_prev.obe", "pAD_prev.obe",  "AD_prev.obe.asr",
    "AD_prev.uv",  "pAD_prev.uv",   "AD_prev.uv.asr",
    "AD_treat",       "pAD_treat" ,"AD_treat.asr",
    "AD_treat_prev",  "pAD_treat_prev","AD_treat_prev.asr",
    "AD_unavoid",     "pAD_unavoid" ,   "AD_unavoid.asr",    
    "total_deaths","total.deaths.asr")



#HDI

AD_by_HDI_RF

AD_by_HDI_all2_RF<-AD_by_HDI_all_RF%>%
  arrange(hdi_group)%>%
  dplyr::mutate(across(2:5, round, -2))%>%
  dplyr::mutate(across(8:13, round, 1))%>%
  dplyr::mutate(across(14:15, round, -2))%>%
  dplyr::mutate(across(16:19, round,3)*100)%>% #dplyr::mutate to show proportion as percentage in export
  select( "hdi_group",      "cancer", 
          "AD_prev.tob", "pAD_prev.tob",   "AD_prev.tob.asr",
          "AD_prev.alc", "pAD_prev.alc",  "AD_prev.alc.asr",
          "AD_prev.inf", "pAD_prev.inf",   "AD_prev.inf.asr",
          "AD_prev.obe", "pAD_prev.obe",  "AD_prev.obe.asr",
          "AD_prev.uv",  "pAD_prev.uv",   "AD_prev.uv.asr",
          "AD_treat",       "pAD_treat" ,"AD_treat.asr",
          "AD_treat_prev",  "pAD_treat_prev","AD_treat_prev.asr",
          "AD_unavoid",     "pAD_unavoid" ,   "AD_unavoid.asr",    
          "total_deaths","total.deaths.asr")




#writing the files
# write.csv(Simulated_Data_PAF_All_RF, "I:\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Results\\NS_Simulated_All_Countries.csv")
# write.csv(Avoidable_Deaths_Simulated_All_RF, "I:\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Results\\AD_Simulated_All_Countries.csv")
# write.csv(Avoidable_Deaths_Simulated_All_age_cat_RF, "I:\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Results\\AD_Simulated_All_Countries_age_cat.csv")
# write.csv(AD_Region_RF, "I:\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Results\\AD_Region_RF.csv")
# write.csv(table_1_11_RF, "I:\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Results\\AD_Total.csv")
# write.csv(AD_by_HDI_all2_RF, "I:\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Results\\AD_HDI_All_Cancers.csv")
# write.csv(AD_country_all_cancers2_RF, "I:\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Results\\AD_Country_All_Cancers.csv")
# write.csv(AD_cancer2_RF,"I:\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Results\\AD_Cancer_by_Site.csv")
# write.csv(Avoidable_Deaths_Simulated_All_age_cat_overall_RF, "I:\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Results\\AD_country_and_Cancer_by_Site.csv")
# 

AD_Region_RF
AD_by_HDI_all2_RF
AD_country_all_cancers2_RF
AD_cancer2_RF
table_1_11_RF
Avoidable_Deaths_Simulated_All_age_cat_overall_RF

