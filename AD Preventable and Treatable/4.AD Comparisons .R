#old equations and net survival file 

AD_Region_old
AD_by_HDI_all2
AD_country_all_cancers2_old
AD_cancer2_old
table_1_11_old
Avoidable_Deaths_Simulated_All_age_cat_overall_old

# new equations and net survival files
AD_Region
AD_by_HDI_all2
AD_country_all_cancers2
AD_cancer2
table_1_11
Avoidable_Deaths_Simulated_All_age_cat_overall


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
   select(country_code, country_label, cancer_code, cancer, tp_diff)%>%
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
   mutate(tp_diff=(AD_treat_prev_old-AD_treat_prev)/AD_treat_prev_old)
 
 
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

 