#################################################################
#
#Code to check avoidable deaths calculations
#Aggregated overall survival and incidence currently checked here
#
#################################################################
PAFs <- read.csv("~/Documents/R_Projects/Data/combinedPAFs_cases_12.07.22.csv")%>%
  group_by(country_code, sex,
                   cancer_code, age)%>%
  filter(sex!=0)%>%
  mutate(af.comb= case_when(cases!=0 ~ sum(cases.prev)/sum(cases),
                            cases==0 ~ af.comb))%>%
  ungroup()%>%
  as.data.frame()%>%
  distinct()

#Checking survival aggregated by age group and other
check2<- countries_5y%>%
  left_join(PAFs)%>%
  select(country_code,country_label, cancer_code, cancer_label,
         age, rel_surv, cases)%>%
  mutate(cancer_code = replace(cancer_code, cancer_code == 9, 38))%>%
  mutate(cancer_code = replace(cancer_code, cancer_code == 8, 38))%>%
  mutate(cancer_label = replace(cancer_label, cancer_label == "Colon", "Colorectal")) %>%
  mutate(cancer_label = replace(cancer_label, cancer_label == "Rectum", "Colorectal")) %>%
  mutate(
    age_cat = case_when(
      age>=4 & age<14 ~ "15-64",
      age>=14 ~ "65-99",
      age<4 ~"0-15"
    ))%>%
  filter(age_cat!="0-15")%>%
  filter(cases!=0)%>%
  group_by(country_code, cancer_code,age_cat)%>%
  mutate(rel_surv= sum(rel_surv*cases, na.rm=T)/sum(cases, na.rm=T))%>%
    select(-cases,-age)%>%
  as.data.frame()%>%
  distinct()


check<- countries_5y%>%
  left_join(PAFs)%>%
  ungroup()%>%
  select(country_code,country_label, cancer_code, cancer_label,
         age , rel_surv, cases )%>%
  mutate(cancer_code = replace(cancer_code, cancer_code == 9, 38))%>%
  mutate(cancer_code = replace(cancer_code, cancer_code == 8, 38))%>%
  mutate(cancer_label = replace(cancer_label, cancer_label == "Colon", "Colorectal")) %>%
  mutate(cancer_label = replace(cancer_label, cancer_label == "Rectum", "Colorectal")) %>%
  mutate(
    age_cat = case_when(
      age>=4 & age<14 ~ "15-64",
      age>=14 ~ "65-99",
      age<4 ~"0-15"
    ))%>%
  filter(age_cat!="0-15")%>%
  mutate(age_cat="Overall")%>%
  select(-age)%>%
  filter(cases!=0)%>%
  group_by(country_code, cancer_code)%>%
  mutate(rel_surv=sum(rel_surv*cases, na.rm=T)/sum(cases, na.rm=T))%>%
  select(-cases)%>%
  as.data.frame()%>%
  distinct()%>%
  full_join(check2)

check_summary<-check%>%
  group_by(cancer_code, age_cat)%>%
  dplyr::summarize(cancer_label,min=min(rel_surv), max=max(rel_surv), 
          sd=sd(rel_surv), mean=mean(rel_surv))%>%
  as.data.frame()%>%
  distinct()%>%
  arrange(age_cat,cancer_label)

check_summary   #Survival check of what the values are and where they fall


#checking so the incidence matches...

Incidence_check<-PAFs%>%
  distinct()%>% 
  ungroup()%>%
  select(cancer_code, cancer_label, age , cases )%>%
  mutate(cancer_code = replace(cancer_code, cancer_code == 9, 38))%>%
  mutate(cancer_code = replace(cancer_code, cancer_code == 8, 38))%>%
  mutate(cancer_label = replace(cancer_label, cancer_label == "Colon", "Colorectal")) %>%
  mutate(cancer_label = replace(cancer_label, cancer_label == "Rectum", "Colorectal")) %>%
  mutate(age_cat = case_when(
      age>=4 & age<14 ~ "15-64",
      age>=14 ~ "65-99",
      age<4 ~"0-15" ))%>%
  filter(age_cat!="0-15")%>%
  mutate(age_cat="Overall")%>%
  select(-age)%>%
  group_by(cancer_code)%>%
  mutate(cases=as.numeric(cases))%>%
  mutate(cases=sum(cases, na.rm=T))%>%
  as.data.frame()%>%
  distinct()%>%
  filter(cancer_code!=40)



Incidence_check_country <- PAFs%>%
  distinct()%>%
  ungroup()%>%
  select(country_code, country_label, cancer_code, cancer_label, age , cases )%>%
  mutate(cancer_code = replace(cancer_code, cancer_code == 9, 38))%>%
  mutate(cancer_code = replace(cancer_code, cancer_code == 8, 38))%>%
  mutate(cancer_label = replace(cancer_label, cancer_label == "Colon", "Colorectal")) %>%
  mutate(cancer_label = replace(cancer_label, cancer_label == "Rectum", "Colorectal")) %>%
  mutate(
      age_cat = case_when(
      age>=4 & age<14 ~ "15-64",
      age>=14 ~ "65-99",
      age<4 ~"0-15"
    ))%>%
  filter(age_cat!="0-15")%>%
  mutate(age_cat="Overall")%>%
  select(-age)%>%
  group_by(country_code, cancer_code)%>%
  mutate(cases=as.integer(cases))%>%
  mutate(cases=sum(cases, na.rm=T))%>%
  as.data.frame()%>%
  distinct()%>%
  filter(cancer_code!=40)


Incidence_check #Seeing that incidence adds up by cancer site


Incidence_check_country #Seeing that incidence adds up by country

sum(Incidence_check$cases)

AD_incidence<-Avoidable_Deaths_Simulated_All2%>%
  ungroup()%>%
  select(cancer_code,cancer, total_overall)%>%
  mutate(age_cat="Overall")%>%
  #select(-age)%>%
  group_by(cancer_code)%>%
  mutate(total_overall=sum(total_overall, na.rm=T))%>%
  as.data.frame()%>%
  distinct()%>%
  filter(cancer_code!=40)

#seeing so cases are equal at end and beginning
sum(AD_incidence$total_overall)==sum(Incidence_check$cases)

sum(AD_incidence$total_overall)

sum(Incidence_check$cases)

write.csv(check_summary, "~/Documents/R_Projects/Data/Survival_check.csv")
write.csv(Incidence_check_country, "~/Documents/R_Projects/Data/Incidence_check_country.csv")
write.csv(AD_incidence, "~/Documents/R_Projects/Data/AD_incidence.csv")





#distinct()

# PAF file check

# checking for duplicate rows
PAFs_duplicates2 <- read.csv("~/Documents/R_Projects/Data/combinedPAFs_cases_12.07.22.csv")%>%
  group_by(country_code, sex,
           cancer_code, age)%>%
  filter(sex!=0)%>%
  # mutate(af.comb= case_when(cases!=0 ~ sum(cases.prev)/sum(cases),
  #                           cases==0 ~ af.comb))%>%
  ungroup()%>%
  as.data.frame()

PAFs_duplicates<-PAFs_duplicates2%>%
  group_by(country_code,cancer_code, sex, age)%>%
  mutate(n=n())%>%
  filter(n==2)

# calculating the excess cases...
sum(PAFs_duplicates$cases)/2

# What cancer sites are problematic with doubles

PAFs_duplicates %>%
  ungroup() %>%
  select(cancer_label) %>%
  distinct()



#Checking which cancer sites have a large jump in between the last two age groups

surv_15<-countries_5y%>%
  filter(age%in%c(15))%>%
  group_by(country_code, country_label,cancer_code, cancer_label)%>%
  select(-age)%>%
  dplyr::rename("rel15"="rel_surv")

surv_16<-countries_5y%>%
  filter(age%in%c(16))%>%
  group_by(country_code, country_label,cancer_code, cancer_label)%>%
  select(-age)%>%
  dplyr::rename("rel16"="rel_surv")

surv_differences<-surv_15%>%
  left_join(surv_16)%>%
  mutate(surv_dif=rel15-rel16)%>%
  select(-rel15,-rel16)%>%
  ungroup()#%>%
  #filter(surv_dif>=0.20)
  
#Forest plot of the problematic cancer sites

library(ggplot2)
# Basic dot plot
p<-ggplot(surv_differences, aes(x=cancer_label, y=surv_dif)) + 
  geom_point()+ 
  coord_flip()+
  labs(x = "Cancer", y = "Net Survival Difference between ages 70-74 and 75+")
p

ggsave("surv_Differences.pdf",width = 20, height = 10, pointsize = 12) 

  
#looking in entire dataset
  

surv_16<-countries_5y%>%
  filter(age%in%c(16))%>%
  group_by(country_code, country_label,cancer_code, cancer_label)%>%
  select(-age)%>%
  dplyr::rename("rel16"="rel_surv")

surv_differences_all<-countries_5y%>%
  group_by(country_code, country_label,cancer_code, cancer_label)%>%
  mutate(surv_dif = abs(rel_surv - lag(rel_surv, order_by = c(age))))%>%
  mutate(age_group=paste(as.character(age),"-",lag(age,  order_by = age)))%>%
  ungroup()%>%
  filter(age!=4)%>%
  filter(abs(surv_dif)>0.20)
 

#Forest plot of all age groups

library(ggplot2)
# Basic dot plot
p2<-ggplot(surv_differences_all, aes(x=cancer_label, y=surv_dif)) + 
  geom_point()+ 
  coord_flip()+
  labs(title="Absolute Net Survival Difference larger than 0.2 between various consecutive Globocan age groups",
       x = "Cancer", y = "Net Survival Difference")+
  facet_grid(1 ~ age_group)
p2

ggsave("surv_Differences_all_age_groups.pdf",width = 20, height = 10, pointsize = 12) 



#avoidable deaths comparisons 

#Computing differences between new and old, filtering, by country and age group...

#By country and cancer site


surv_nordic<-Survival_Modelled%>%filter(country_label%in%c("Denmark", "Sweden", "Finland", "Norway", "Iceland" ))%>%
  filter(cancer_label=="Breast")%>%
  group_by(country_label)


p3<-ggplot(surv_nordic, aes(x=age, y=rel_surv, color=country_label)) + 
  geom_line()+ 
  #coord_flip()+
  labs(title="Prostate cancer survival modelled in Germany",
       x = "Globocan age group", y = "Net Survival(%)")

p3


