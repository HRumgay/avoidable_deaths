#old equations and net survival file 

AD_Region_old
AD_by_HDI_all2
AD_country_all_cancers2_old
AD_cancer2_old
table_1_11_old
Avoidable_Deaths_Simulated_All_age_cat_overall_old
Avoidable_Deaths_Simulated_All_old
# new equations and net survival files
AD_Region
AD_by_HDI_all2
AD_country_all_cancers2
AD_cancer2
table_1_11
Avoidable_Deaths_Simulated_All_age_cat_overall
Avoidable_Deaths_Simulated_All

#Combining into one data frame and calculating the difference
AD_old<-Avoidable_Deaths_Simulated_All_age_cat_overall_old%>%
  dplyr::rename("AD_prev_old"="AD_prev")%>%
  dplyr::rename("AD_treat_old"="AD_treat")%>%
  dplyr::rename("AD_treat_prev_old"="AD_treat_prev")%>%
  select(country_code, cancer_code, AD_prev_old,AD_treat_old,AD_treat_prev_old)


AD_diff_countries<-Avoidable_Deaths_Simulated_All_age_cat_overall%>%
   left_join(AD_old)%>%
   mutate(prev_diff=(AD_prev_old-AD_prev)/AD_prev_old)%>%
   mutate(treat_diff=(AD_treat_old-AD_treat)/AD_treat_old)%>%
   mutate(tp_diff=(AD_treat_prev_old-AD_treat_prev)/AD_treat_prev_old)
  filter(abs(AD_prev_old-AD_prev)<0.01)%>%
  filter(abs(AD_treat_old-AD_treat)<0.01)%>%
  filter(abs(AD_treat_prev_old-AD_treat_prev)<0.01) 
#Last three lines To prevent the function from blowing up with small nubmer subraction/ division

 
#Preventable
 
 AD_diff_countries_prev<-AD_diff_countries%>%
   select(country_code, country_label, cancer_code, cancer, prev_diff)%>%
   filter(prev_diff!=0)%>%
   filter(abs(prev_diff)!=Inf)%>%
   filter(abs(prev_diff)>0.2)
 
# Treatable
 AD_diff_countries_treat<-AD_diff_countries%>%
   select(country_code, country_label, cancer_code, cancer, treat_diff)%>%
   filter(treat_diff!=0)%>%
   filter(abs(treat_diff)!=Inf)%>%
   filter(abs(treat_diff)>0.2)
 
 #Avoidable 
   
 AD_diff_countries_tp<-AD_diff_countries%>%
   select(country_code, country_label, cancer_code, cancer, AD_treat_prev_old, AD_treat_prev, tp_diff)%>%
   filter(tp_diff!=0)%>%
   filter(abs(tp_diff)!=Inf)%>%
   filter(abs(tp_diff)>0.2)
   
   
   # by cancer site

 AD_cancer_old<-AD_cancer2_old%>%
   dplyr::rename("AD_prev_old"="AD_prev")%>%
   dplyr::rename("AD_treat_old"="AD_treat")%>%
   dplyr::rename("AD_treat_prev_old"="AD_treat_prev")%>%
   select(cancer_code, AD_prev_old,AD_treat_old,AD_treat_prev_old)
 
 
 AD_diff_cancer<-AD_cancer2%>%
   left_join( AD_cancer_old)%>%
   mutate(prev_diff=(AD_prev_old-AD_prev)/AD_prev_old)%>%
   mutate(treat_diff=(AD_treat_old-AD_treat)/AD_treat_old)%>%
   mutate(tp_diff=(AD_treat_prev_old-AD_treat_prev)/AD_treat_prev_old)%>%
   filter(abs(AD_prev_old-AD_prev)<0.01)%>%
   filter(abs(AD_treat_old-AD_treat)<0.01)%>%
   filter(abs(AD_treat_prev_old-AD_treat_prev)<0.1) 
 
 
 
 #Preventable
 
 AD_diff_cancer_prev<-AD_diff_cancer%>%
   select(cancer_code, cancer, prev_diff)%>%
   filter(prev_diff!=0)%>%
   filter(abs(prev_diff)!=Inf)%>%
   filter(abs(prev_diff)>0.2)
 
 # Treatable
 AD_diff_cancer_treat<-AD_diff_cancer%>%
   select(cancer_code, cancer, treat_diff)%>%
   filter(treat_diff!=0)%>%
   filter(abs(treat_diff)!=Inf)%>%
   filter(abs(treat_diff)>0.2)
 
 #Avoidable 
 
 AD_diff_cancer_tp<-AD_diff_cancer%>%
   select(cancer_code, cancer, tp_diff)%>%
   filter(tp_diff!=0)%>%
   filter(abs(tp_diff)!=Inf)%>%
   filter(abs(tp_diff)>0.2)
 
 
 #age group 
 
 
 AD_age_sex_old<-Avoidable_Deaths_Simulated_All_old%>%
   dplyr::rename("AD_prev_old"="AD_prev")%>%
   dplyr::rename("AD_treat_old"="AD_treat_not_prev")%>%
   mutate(AD_treat_prev_old=AD_treat_old+AD_prev_old)%>%
   select(sex,age,country_code, cancer_code, AD_prev_old,AD_treat_old,AD_treat_prev_old)%>%
   distinct()
 
 
 AD_age_sex<-Avoidable_Deaths_Simulated_All%>%
   mutate(AD_treat_prev=AD_prev+AD_treat)%>%
   left_join(AD_age_sex_old, by=c("country_code","cancer_code", "sex", "age"))%>%
   ungroup()%>%
   mutate(prev_diff=(AD_prev_old-AD_prev)/AD_prev_old)%>%
   mutate(treat_diff=(AD_treat_old-AD_treat)/AD_treat_old)%>%
   mutate(tp_diff=(AD_treat_prev_old-AD_treat_prev)/AD_treat_prev_old)%>%
   filter(abs(AD_prev_old-AD_prev)<0.01)%>%
   filter(abs(AD_treat_old-AD_treat)<0.01)%>%
   filter(abs(AD_treat_prev_old-AD_treat_prev)<0.1)
 
 
 #Overall by country, age, sex, cancer site... 
 
 #Preventable
 
 AD_diff_age_sex_prev<- AD_age_sex%>%
   select(sex, age,country_code, country_label,  cancer_code, cancer, prev_diff)%>%
   filter(prev_diff!=0)%>%
   filter(abs(prev_diff)!=Inf)%>%
   filter(abs(prev_diff)>0.2)
 
 # Treatable
 AD_diff_age_sex_treat<- AD_age_sex%>%
   select(sex, age,country_code, country_label,  cancer_code, cancer, treat_diff)%>%
   filter(treat_diff!=0)%>%
   filter(abs(treat_diff)!=Inf)%>%
   filter(abs(treat_diff)>0.2)
 
 
 # Avoidable 
 
 AD_diff_age_sex_tp<- AD_age_sex%>%
   select(sex, age,country_code, country_label, cancer_code, cancer, tp_diff)%>%
   filter(tp_diff!=0)%>%
   filter(abs(tp_diff)!=Inf)%>%
   filter(abs(tp_diff)>0.2)
 
 
 #By country site, sex and cancer site only
AD_country_sex_Cancer <-AD_age_sex%>%
   select(-age)%>%
   filter(tp_diff!=0)%>%
   filter(abs(tp_diff)!=Inf)%>%
   filter(prev_diff!=0)%>%
   filter(abs(prev_diff)!=Inf)%>%
   filter(treat_diff!=0)%>%
   filter(abs(treat_diff)!=Inf)%>%
   group_by(country_code, cancer_code,sex)%>%
   mutate(prev_diff=sum(prev_diff))%>%
   mutate(treat_diff=sum(treat_diff))%>%
   mutate(tp_diff=sum(tp_diff))%>%
   select( country_label,sex,cancer,cancer_code, prev_diff, treat_diff, tp_diff)%>%distinct()
   
 
#By sex and cancer site only
AD_sex_Cancer <-AD_age_sex%>%
  select(-age,-country_label,-country_code)%>%
  filter(tp_diff!=0)%>%
  filter(abs(tp_diff)!=Inf)%>%
  filter(prev_diff!=0)%>%
  filter(abs(prev_diff)!=Inf)%>%
  filter(treat_diff!=0)%>%
  filter(abs(treat_diff)!=Inf)%>%
  group_by( cancer_code,sex)%>%
  mutate(prev_diff=sum(prev_diff))%>%
  mutate(treat_diff=sum(treat_diff))%>%
  mutate(tp_diff=sum(tp_diff))%>%
  select(sex,cancer,cancer_code, prev_diff, treat_diff, tp_diff)%>%
  distinct()

#By sex only
AD_sex <-AD_age_sex%>%
  select(-age,-country_label,-country_code,-cancer, -cancer_code)%>%
  filter(tp_diff!=0)%>%
  filter(abs(tp_diff)!=Inf)%>%
  filter(prev_diff!=0)%>%
  filter(abs(prev_diff)!=Inf)%>%
  filter(treat_diff!=0)%>%
  filter(abs(treat_diff)!=Inf)%>%
  group_by(sex)%>%
  mutate(prev_diff=sum(prev_diff))%>%
  mutate(treat_diff=sum(treat_diff))%>%
  mutate(tp_diff=sum(tp_diff))%>%
  select(sex, prev_diff, treat_diff, tp_diff)%>%
  distinct()


#By age and cancer site only
AD_age <-AD_age_sex%>%
  select(-country_label,-country_code,-sex,-cancer,-cancer_code)%>%
  filter(tp_diff!=0)%>%
  filter(abs(tp_diff)!=Inf)%>%
  filter(prev_diff!=0)%>%
  filter(abs(prev_diff)!=Inf)%>%
  filter(treat_diff!=0)%>%
  filter(abs(treat_diff)!=Inf)%>%
  group_by( age)%>%
  mutate(prev_diff=sum(prev_diff))%>%
  mutate(treat_diff=sum(treat_diff))%>%
  mutate(tp_diff=sum(tp_diff))%>%
  select(age, prev_diff, treat_diff, tp_diff)%>%
  distinct()






 # Visualization 
 
 
 
 

 