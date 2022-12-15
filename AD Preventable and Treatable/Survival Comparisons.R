


check2<- countries_5y%>%
  left_join(PAFs)%>%
  select(country_code,country_label, cancer_code, cancer_label,
         age, rel_surv, cases)%>%
  mutate(cancer_code = replace(cancer_code, cancer_code == 9, 38))%>%
  mutate(cancer_code = replace(cancer_code, cancer_code == 8, 38))%>%
  mutate(cancer_label = replace(cancer_label, cancer_label == "Colon", "Colorectal")) %>%
  mutate(cancer_label = replace(cancer_label, cancer_label == "Rectum", "Colorectal")) %>%
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
  mutate(age_cat="Overall")%>%
  select(-age)%>%
  group_by(cancer_code)%>%
  mutate(cases=as.numeric(cases))%>%
  mutate(cases=sum(cases, na.rm=T))%>%
  as.data.frame()%>%
  distinct()%>%
  filter(cancer_code!=40)


  

Incidence_check #Seeing that incidence adds up 

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



