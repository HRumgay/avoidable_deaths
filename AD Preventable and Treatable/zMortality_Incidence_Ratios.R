Globocan <- read.csv("I:/DataShare/Globocan2020/Globocan.csv")

Ginc<-Globocan%>%
  filter(type==0)%>%
  mutate(incidence=cases)%>%
  select(-type, -total,-cases)


GIncmort<-Globocan%>%
  filter(type==1)%>%
  mutate(mortality=cases)%>%
  select(-type, -total,-cases)%>%
  left_join(Ginc,by=c("country_label","country_code", 
                      "cancer_label", "cancer_code",  
                      "sex", "age", "py"))%>%
  filter(sex!=0)%>%
  mutate(MIR= 1-mortality/incidence)


G2<-Globocan%>%
  filter(sex!=0)%>%
  mutate(
    age_cat = case_when(
      age>=4 & age<14 ~ "15-64",
      age>=14 ~ "65-99",
      FALSE ~"0-15"
    ))%>%
  filter(age>=4)

# By age category

Ginc_age_cat<-G2%>%
  filter(type==0)%>%
  group_by(country_code, cancer_code, age_cat)%>%
  mutate(cases=sum(cases))%>%
  mutate(total=sum(total))%>%
  select(-age, -sex, -py)%>%
  mutate(incidence=cases)%>%
  select(-type, -total,-cases)%>%distinct()

Gmort_Overall<-G2%>%
  filter(type==1)%>%
  mutate(age_cat="Overall")%>%
  group_by(country_code, cancer_code, age_cat)%>%
  mutate(cases=sum(cases))%>%
  mutate(total=sum(total))%>%
  select(-age, -sex, -py)%>%
  mutate(mortality=cases)%>%
  select(-type, -total,-cases)%>%distinct()

Ginc_Overall<-G2%>%
  filter(type==0)%>%
  mutate(age_cat="Overall")%>%
  group_by(country_code, cancer_code, age_cat)%>%
  mutate(cases=sum(cases))%>%
  mutate(total=sum(total))%>%
  select(-age, -sex, -py)%>%
  mutate(incidence=cases)%>%
  select(-type, -total,-cases)%>%distinct()%>%
  left_join(Gmort_Overall)



GIncmort_age_cat<-G2%>%
  filter(type==1)%>%
  group_by(country_code, cancer_code, age_cat)%>%
  mutate(cases=sum(cases))%>%
  mutate(total=sum(total))%>%
  select(-age, -sex, -py)%>%
  mutate(mortality=cases)%>%
  select(-type, -total,-cases)%>%
  left_join(Ginc_age_cat,by=c("country_label","country_code", 
                              "cancer_label", "cancer_code", "age_cat"
  ))%>%
  full_join(Ginc_Overall)%>%
  distinct()%>%
  mutate(MIR= 1-mortality/incidence)%>%
  arrange(country_code, cancer_label, age_cat)




write.csv(GIncmort_age_cat, "MIR_age_cat.csv")
write.csv(GIncmort, "MIR.csv")



