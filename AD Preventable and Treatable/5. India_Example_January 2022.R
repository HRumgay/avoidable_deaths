
#Survcan_website_NS<-
  read.csv("~/Documents/R_Projects/Data/SURVCAN_NS_5y.csv")%>%
  group_by(Cancer.label)%>%
  select(Cancer.label)%>%distinct()

# Malaysia seems like a good reference here. China and Turkey seem like they could be political
# 



Reference_india <-  Survival_Modelled%>%
  filter(country_label=="Malaysia")%>%
  as.data.frame()%>%
  select( age,
          cancer_code,
          rel_surv)%>%
  dplyr::rename("surv_ref"="rel_surv")%>%
  distinct()%>%
  mutate(surv_ref=case_when(surv_ref>1~ 1,
                            surv_ref<=1~ surv_ref))  
  
  PAF_India<-Simulated_Data_PAF_1%>%
  filter(country_label=="India")%>%
  dplyr::mutate(rel_surv=as.double(rel_surv))%>%
  dplyr::mutate(af.comb=as.double(af.comb))%>% 
  dplyr::mutate(total_overall=as.double(total_overall))%>% 
  arrange(country_label,cancer_code,age,sex)%>%
  left_join(Reference_india, by=c("age","cancer_code"))%>%
              filter(cancer_label%in%c("Prostate","Breast", "Stomach", "Stomach", "Liver", 
                                       "Trachea, bronchus and lung", "Liver and intrahepatic bile ducts",
                                       "Cervix uteri", "Non-Hodgkin lymphoma",
                                       "Bladder", "Rectum", "Colon", "Lip, oral cavity",
                                       "Nasopharynx", "Ovary", "Leukaemia",
                                       "Oesophagus"))
  
 


# Avoidable deaths

# Three AD calcs 

# first need to make sure data is in right format (numeric columns)

# Applying the equation from Rutherford 2015 for AD.


Avoidable_Deaths_Simulated_All3_india <-  PAF_India%>%
  dplyr::group_by(country_code, cancer_code, age, sex)%>%
  
  dplyr::mutate(AD_prev= total_overall * af.comb *ES* (1 - rel_surv))%>%
  dplyr::mutate(AD_treat=(1-af.comb) *total_overall * (surv_ref-rel_surv) * ES)%>%
  #dplyr::mutate(AD_treat_not_prev = (1-af.comb) * total_overall * (surv_ref-rel_surv) * ES)%>% #scenario 1
  dplyr::mutate(AD_unavoid =total_overall*(1-ES*surv_ref-af.comb*ES*(1-surv_ref)))%>%
  dplyr::mutate(Expect_deaths=total_overall*(1-ES*rel_surv))%>%
  #  dplyr::mutate(AD_sum=AD_prev+AD_treat_not_prev+AD_unavoid)%>%
  # dplyr::mutate(Expect_deaths=(1-(rel_surv*ES))*total_overall)#%>%
  select("country_code","country_label","age_cat","age","cancer_code","cancer_label",   
         "AD_treat",
         # "AD_treat_not_prev",
         "AD_prev",
         "AD_unavoid",
         "Expect_deaths",
         "total_overall",
         "hdi_group")%>%
  dplyr::rename("cancer"="cancer_label")


colorectal_india<-Avoidable_Deaths_Simulated_All3_india%>%
  dplyr::filter(cancer_code%in%c("8","9"))%>%
  dplyr::mutate(cancer="Colorectal",
                cancer_code=8)%>%
  dplyr::group_by(country_code, cancer_code, age, sex)%>%
  dplyr::mutate(AD_prev=sum(AD_prev),
                AD_treat=sum(AD_treat),
                #  AD_treat_not_prev=sum(AD_treat_not_prev),
                AD_unavoid=sum(AD_unavoid),
                Expect_deaths=sum(Expect_deaths),
                total_overall=sum(total_overall))%>%
  distinct()

colorectal_india


Avoidable_Deaths_Simulated_All2_india<-Avoidable_Deaths_Simulated_All3_india%>%
  dplyr::filter(!cancer_code%in%c("8","9"))%>%
  full_join(colorectal_india)%>%
  dplyr::mutate(cancer = replace(cancer, cancer == "Liver and intrahepatic bile ducts", "Liver"))%>%
  dplyr::mutate(cancer = replace(cancer, cancer == "Trachea, bronchus and lung", "Lung"))


Avoidable_Deaths_Simulated_All2_india

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
    age>=16 ~ 16,
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
  age>=16 ~ 16,
))%>%
  dplyr::filter(age>3)%>%
  dplyr::mutate(total=sum(w2))%>%
  dplyr::group_by(age)%>%
  dplyr::mutate(w=sum(w2)/total)%>%
  select(-w2, -total)%>%
  distinct()


Avoidable_Deaths_Simulated_All_india<- Avoidable_Deaths_Simulated_All2_india%>%
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
Avoidable_Deaths_Simulated_All_overall_india <- Avoidable_Deaths_Simulated_All_india%>%
  dplyr::mutate(age_cat="Overall")%>%
  dplyr::group_by(country_code, cancer_code, age_cat)%>%
  dplyr::mutate(AD_prev.asr=sum(AD_prev/py*100000*w), #ASR calculation here
                AD_treat.asr=sum(AD_treat/py*100000*w),
                AD_sum.asr=sum(AD_sum/py*100000*w),
                # AD_treat_not_prev.asr=sum(AD_treat_not_prev/py*100000*w),
                AD_unavoid.asr=sum(AD_unavoid/py*100000*w),
                Expect_deaths.asr=sum(Expect_deaths/py*100000*w)) %>% 
  select( -total_overall)%>%
  dplyr::mutate(AD_prev = sum(AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_treat = sum(AD_treat,na.rm=T))%>%
  dplyr::mutate(AD_sum = sum(AD_sum,na.rm=T))%>%
  #dplyr::mutate(AD_treat_not_prev = sum(AD_treat_not_prev,na.rm=T))%>%
  dplyr::mutate(AD_unavoid = sum(AD_unavoid,na.rm=T))%>%
  dplyr::mutate(Expect_deaths=sum(Expect_deaths,na.rm=T))%>%
  dplyr::mutate(py=sum(py))%>%
  select( -age,-sex)%>%
  ungroup()%>%
  dplyr::group_by(cancer_code, country_code,age_cat)%>%
  dplyr::mutate(w=sum(w/2))%>%
  distinct()%>%
  as.data.frame()


Avoidable_Deaths_Simulated_All_age_cat_india <- Avoidable_Deaths_Simulated_All_india%>%
  ungroup()%>%
  dplyr::group_by(country_code, cancer_code, age_cat)%>%
  dplyr::mutate(AD_prev.asr=sum(AD_prev/py*100000*w), #ASR calculation here
                AD_treat.asr=sum(AD_treat/py*100000*w),
                AD_sum.asr=sum(AD_sum/py*100000*w),
                #  AD_treat_not_prev.asr=sum(AD_treat_not_prev/py*100000*w),
                AD_unavoid.asr=sum(AD_unavoid/py*100000*w),
                Expect_deaths.asr=sum(Expect_deaths/py*100000*w)) %>% 
  select(-total_overall)%>%
  dplyr::mutate(AD_prev = sum(AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_treat = sum(AD_treat,na.rm=T))%>%
  #dplyr::mutate(AD_treat_not_prev = sum(AD_treat_not_prev,na.rm=T))%>%
  dplyr::mutate(AD_unavoid = sum(AD_unavoid,na.rm=T))%>%
  dplyr::mutate(Expect_deaths=sum(Expect_deaths,na.rm=T))%>%
  dplyr::mutate(AD_sum = sum(AD_sum,na.rm=T))%>%
  dplyr::mutate(py=sum(py))%>%
  select( -age,-sex)%>%
  ungroup()%>%
  dplyr::group_by(cancer_code, country_code,age_cat)%>%
  dplyr::mutate(w=sum(w/2))%>%
  distinct()%>%
  full_join(Avoidable_Deaths_Simulated_All_overall_india)%>%
  as.data.frame()%>%ungroup()


# By country for all cancer sites (number and proportion): 

AD_country_all_cancers_india <-Avoidable_Deaths_Simulated_All_india%>%
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
                AD_treat.asr=sum(AD_treat/py*100000*w),
                AD_treat_prev.asr=sum((AD_treat+AD_prev)/py*100000*w),
                # AD_treat_not_prev.asr=sum(AD_treat_not_prev/py*100000*w),
                AD_unavoid.asr=sum(AD_unavoid/py*100000*w),
                Expect_deaths.asr=sum(Expect_deaths/py*100000*w)) %>% 
  dplyr::mutate(total_deaths=sum(AD_prev, AD_unavoid, AD_treat,na.rm=T))%>%
  dplyr::mutate(Expect_deaths=sum(Expect_deaths,na.rm=T))%>%
  dplyr::mutate(AD_treat_prev=sum(AD_treat,AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_treat=sum(AD_treat,na.rm=T))%>%
  #dplyr::mutate(AD_treat_not_prev=sum(AD_treat_not_prev,na.rm=T))%>%
  dplyr::mutate(AD_prev=sum(AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_unavoid=sum(AD_unavoid,na.rm=T))%>%
  dplyr::mutate(pAD_treat=AD_treat/total_deaths)%>%
  #dplyr::mutate(pAD_treat_not_prev=AD_treat_not_prev/total_deaths)%>%
  dplyr::mutate(pAD_prev=AD_prev/total_deaths)%>%
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

AD_cancer_india <- Avoidable_Deaths_Simulated_All_india%>%
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
                AD_treat.asr=sum(AD_treat/py*100000*w),
                AD_treat_prev.asr=sum((AD_treat+AD_prev)/py*100000*w),
                # AD_treat_not_prev.asr=sum(AD_treat_not_prev/py*100000*w),
                AD_unavoid.asr=sum(AD_unavoid/py*100000*w),
                Expect_deaths.asr=sum(Expect_deaths/py*100000*w)) %>% 
  dplyr::mutate(total_deaths=sum(Expect_deaths,na.rm=T))%>%
  select(-Expect_deaths)%>%
  dplyr::mutate(AD_treat_prev=sum(AD_treat,AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_treat=sum(AD_treat,na.rm=T))%>%
  #dplyr::mutate(AD_treat_not_prev=sum(AD_treat_not_prev,na.rm=T))%>%
  dplyr::mutate(AD_prev=sum(AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_unavoid=sum(AD_unavoid,na.rm=T))%>%
  dplyr::mutate(pAD_treat=AD_treat/total_deaths)%>%
  # dplyr::mutate(pAD_treat_not_prev=AD_treat_not_prev/total_deaths)%>%
  dplyr::mutate(pAD_prev=AD_prev/total_deaths)%>%
  dplyr::mutate(pAD_unavoid=AD_unavoid/total_deaths)%>%
  dplyr::mutate(pAD_treat_prev=AD_treat_prev/total_deaths)%>%
  select(-py,-w,-total_overall)%>%
  distinct()%>%
  ungroup()%>%
  distinct()%>%
  as.data.frame()



# By Country and all cancer sites

 AD_country_all_cancers2_india<-AD_country_all_cancers_india%>%  
  ungroup()%>%
  distinct()%>%
  dplyr::mutate(across(6:10,round, -2))%>%
  dplyr::mutate(across(11:15,round, 1))%>%
  dplyr::mutate(across(16:16,round, -2))%>%
  dplyr::mutate(across(17:20, round,3)*100)%>% #dplyr::mutate to show proportion as percentage in export
  select("country_code","country_label",
         "cancer_code", "cancer", 
         "AD_prev",        "pAD_prev",    "AD_prev.asr",
         "AD_treat",       "pAD_treat" ,"AD_treat.asr",
         "AD_treat_prev",  "pAD_treat_prev","AD_treat_prev.asr",
         "AD_unavoid",     "pAD_unavoid" ,   "AD_unavoid.asr",    
         "total_deaths","Expect_deaths.asr")




# By country AND cancer site
# AD by country and cancer site

Avoidable_Deaths_Simulated_All_india
Avoidable_Deaths_Simulated_All_age_cat_india
Avoidable_Deaths_Simulated_All_age_cat_overall_india<-Avoidable_Deaths_Simulated_All_age_cat_india%>% #Object to plot overall age standardized on world maps 
  as.data.frame()%>%
  dplyr::filter(age_cat=="Overall")%>% 
  ungroup()%>%
  dplyr::group_by(country_code,cancer_code)%>%
  dplyr::mutate(total_deaths=sum(Expect_deaths,na.rm=T))%>%
  #select(-Expect_deaths,-AD_sum)%>%
  dplyr::mutate(AD_treat_prev=sum(AD_treat, AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_treat=sum(AD_treat,na.rm=T))%>%
  #dplyr::mutate(AD_treat_not_prev=sum(AD_treat_not_prev,na.rm=T))%>%
  dplyr::mutate(AD_prev=sum(AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_unavoid=sum(AD_unavoid,na.rm=T))%>%
  dplyr::mutate(pAD_treat=AD_treat/total_deaths)%>%
  #dplyr::mutate(pAD_treat_not_prev=AD_treat_not_prev/total_deaths)%>%
  dplyr::mutate(pAD_prev=AD_prev/total_deaths)%>%
  dplyr::mutate(pAD_unavoid=AD_unavoid/total_deaths)%>%
  dplyr::mutate(pAD_treat_prev=AD_treat_prev/total_deaths)%>%
  as.data.frame()%>%
  select( "country_code","country_label","cancer_code", "cancer", 
          "AD_prev","pAD_prev",    
          "AD_treat",  "pAD_treat" ,
          # AD_treat_not_prev, pAD_treat_not_prev,
          "AD_treat_prev", "pAD_treat_prev",
          "AD_unavoid",   "pAD_unavoid" ,        
          "total_deaths"  )%>%
  dplyr::mutate(across(5:5,round,0 ))%>%
  dplyr::mutate(across(6:6, round,4)*100)%>% 
  dplyr::mutate(across(7:7,round,0 ))%>%
  dplyr::mutate(across(8:8, round,4)*100)%>% 
  dplyr::mutate(across(9:9,round,0 ))%>%
  dplyr::mutate(across(10:10, round,4)*100)%>% 
  dplyr::mutate(across(11:11,round,0 ))%>%
  dplyr::mutate(across(12:12, round,4)*100)%>% 
  dplyr::mutate(across(13:13,round,0 ))

Avoidable_Deaths_Simulated_All_age_cat_overall_india

#writing the file
write.csv(Avoidable_Deaths_Simulated_All_age_cat_overall_india, "~/Documents/R_Projects/Data/AD_India_Cancer_by_Site.csv")



#Data by cancer site prep
AD_by_cancer_site_india <-Avoidable_Deaths_Simulated_All_age_cat_overall_india

AD_by_cancer_site_3_india<-AD_by_cancer_site_india%>%
  select(cancer, cancer_code, AD_treat, pAD_treat)%>%
  rename("AD"="AD_treat")%>%
  rename("pAD"="pAD_treat")%>%
  dplyr::mutate(AD_cat="Treatable")

AD_by_cancer_site_2_india<-AD_by_cancer_site_india%>%
  select(cancer, cancer_code, AD_prev, pAD_prev)%>%
  rename("AD"="AD_prev")%>%
  rename("pAD"="pAD_prev")%>%
  dplyr::mutate(AD_cat="Preventable")

AD_by_cancer_site_1_india <- AD_by_cancer_site_india%>%
  select(cancer, cancer_code, AD_treat_prev, pAD_treat_prev)%>%
  rename("AD"="AD_treat_prev")%>%
  rename("pAD"="pAD_treat_prev")%>%
  dplyr::mutate(AD_cat="Avoidable")%>%
  full_join(AD_by_cancer_site_2_india)%>%
  full_join(AD_by_cancer_site_3_india)


#plotting them 

pAD_india <- AD_by_cancer_site_1_india %>%
  #group_by(AD_cat,cancer)%>%
  ggplot(
    aes(cancer, pAD, fill="AD_cat",
        ymin = 0,
        ymax = 100),
    mapping = aes(
      reorder(cancer, pAD),pAD,drop=FALSE, fill=AD_cat,
    )) +
  xlab("Cancer Site") +
  ylab("Proportion Preventable Deaths (pAD, %)") +
  ggtitle("Proportion Risk Factor Preventable, Treatable and Total Avoidable Deaths in India ") +
 # scale_fill_manual(values = c('#de2d26','#fc9272',  '#fee0d2')) + #choose some nice colours for your bars
  geom_bar(stat = "identity", 
           position = "dodge") +
  coord_flip() 

pAD_india

