#################################################################
#
#Code to check avoidable deaths calculations
#Aggregated overall survival and incidence currently checked here
#
#################################################################
setwd("C:\\Users\\langseliuso\\Documents\\GitHub\\avoidable_deaths\\AD Preventable and Treatable")


PAFs <- read.csv("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Research visits\\Oliver_Langselius\\Data\\combinedPAFs_cases_12.07.22.csv")%>%
  as.data.frame()%>%
  group_by(country_code, sex,
           cancer_code, age)%>%
  filter(sex!=0)%>%
  # mutate(af.comb= case_when(cases!=0 ~ sum(cases.prev)/sum(cases),
  #                           cases==0 ~ af.comb))%>%
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

#a

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



#Specific checks - Modify below

HDI <- read_dta("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Survival Checks\\hdi_2020.dta")%>%as.data.frame()
# 
# israel<-read_dta("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Survival Checks\\survival_Israel_anchor_ALL_all34.dta")%>%
#   as.data.frame()%>%
#   filter(time==5)%>%
#   select(-country_name)%>%
#   left_join(HDI)%>%
#   filter(hdi_group%in%c(3,4))
# 
# thailand <- read_dta("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Survival Checks\\survival_Thailand_anchor_ALL_all34.dta")%>%
#   as.data.frame()%>%
#   filter(time==5)%>%
#     select(-country_name)%>%
#   left_join(HDI)%>%
#   filter(hdi_group%in%c(1,2))
# 
# 
# surv_new <- thailand%>%
#   full_join(israel)%>%
#   dplyr::mutate(rel_surv=case_when(rel_surv>1~ 1,
#                                    rel_surv<=1~ rel_surv))%>%
#   dplyr::mutate(hdi_group=case_when(hdi_group==1~ "1) Low",
#                                     hdi_group==2~ "2) Medium",
#                                     hdi_group==3~ "3) High",
#                                     hdi_group==4~ "4) Very High"))
# 
# 
# 
# 

surv_new2 <- read_dta("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Survival Checks\\survival_SURVCAN_anchor_ALL_all34_byHDI.dta")%>%
  as.data.frame()

surv_new<-surv_new2%>%
  filter(time==5)%>%
  select(-country_name)%>%
  left_join(HDI, by=c("country_code", "hdi_group"))%>%
  dplyr::mutate(rel_surv=case_when(rel_surv>1~ 1,
                                   rel_surv<=1~ rel_surv))%>%
  dplyr::mutate(hdi_group=case_when(hdi_group==1~ "1) Low",
                                    hdi_group==2~ "2) Medium",
                                    hdi_group==3~ "3) High",
                                    hdi_group==4~ "4) Very High"))



surv_new
surv_16_new<-surv_new%>%
  filter(age%in%c(16))%>%
  group_by(country_code, country_label,cancer_code, cancer_label)%>%
  select(-age)%>%
  dplyr::rename("rel16"="rel_surv")

surv_differences_all_new<-surv_new%>%
  group_by(country_code, country_label,cancer_code, cancer_label)%>%
  mutate(surv_dif = abs(rel_surv - lag(rel_surv, order_by = c(age))))%>%
  mutate(surv_dif_2 = rel_surv - lag(rel_surv, order_by = c(age)))%>%
  mutate(age_group=paste(as.character(age),"-",lag(age,  order_by = age)))%>%
  ungroup()%>%
  filter(age!=4)%>%
  filter(abs(surv_dif)>0.20)


surv_differences_all_new_0<-surv_new%>%
  group_by(country_code, country_label,cancer_code, cancer_label)%>%
  mutate(surv_dif = abs(rel_surv - lag(rel_surv, order_by = c(age))))%>%
  mutate(surv_dif_2 = rel_surv - lag(rel_surv, order_by = c(age)))%>%
  mutate(age_group=paste(as.character(age),"-",lag(age,  order_by = age)))%>%
  ungroup()%>%
  filter(age!=4)%>%
  #filter(abs(surv_dif)>0.20)
  filter(abs(surv_dif)!=0)


#Forest plot of all age groups

library(ggplot2)
# Basic dot plot
p2_new<-ggplot(surv_differences_all_new, aes(x=cancer_label, y=surv_dif)) + 
  geom_point()+ 
  coord_flip()+
  labs(title="Absolute Net Survival Difference larger than 0.2 between various consecutive Globocan age groups",
       x = "Cancer", y = "Net Survival Difference (units)")+
  facet_grid(1 ~ age_group)

p2_new

p2_new_0<-ggplot(surv_differences_all_new_0, aes(x=cancer_label, y=surv_dif)) + 
  geom_point()+ 
  coord_flip()+
  labs(title="Absolute Net Survival Difference between various consecutive Globocan age groups",
       x = "Cancer", y = "Net Survival Difference (units)")+
  facet_grid(1 ~ age_group)

p2_new_0


p2_new_HDI<-ggplot(surv_differences_all_new, aes(x=cancer_label, y=surv_dif)) + 
  geom_point()+ 
  coord_flip()+
  labs(title="Absolute Net Survival Difference larger than 0.2 between various consecutive Globocan age groups by HDI",
       x = "Cancer", y = "Net Survival Difference (units)")+
  facet_grid(hdi_group ~ age_group)

p2_new_HDI


p2_new_HDI_0<-ggplot(surv_differences_all_new_0, aes(x=cancer_label, y=surv_dif)) + 
  geom_point()+ 
  coord_flip()+
  labs(title="Absolute Net Survival Difference between various consecutive Globocan age groups",
       x = "Cancer", y = "Net Survival Difference (units)")+
  facet_grid(hdi_group ~ age_group)

p2_new_HDI_0

#Leukemia
leuk<-surv_differences_all_new_0%>%
  filter(cancer_code==36)


p2_new_0_leuk<-ggplot(leuk, aes(x=cancer_label, y=surv_dif)) + 
  geom_point()+ 
  coord_flip()+
  labs(title="Absolute Net Survival Difference between various consecutive Globocan age groups for Leukemia",
       x = "Cancer", y = "Net Survival Difference (units)")+
  facet_grid(1 ~ age_group)

p2_new_0_leuk


p2_new_HDI_0_leuk<-ggplot(leuk, aes(x=cancer_label, y=surv_dif)) + 
  geom_point()+ 
  coord_flip()+
  labs(title="Absolute Net Survival Difference between various consecutive Globocan age groups for Leukemia",
       x = "Cancer", y = "Net Survival Difference (units)")+
  facet_grid(hdi_group ~ age_group)

p2_new_HDI_0_leuk


#without absolute values in the outputs

p2_new_neg<-ggplot(surv_differences_all_new, aes(x=cancer_label, y=surv_dif_2)) + 
  geom_point()+ 
  coord_flip()+
  labs(title="  Net Survival Difference larger than 0.2 between various consecutive Globocan age groups",
       x = "Cancer", y = "Net Survival Difference (units)")+
  facet_grid(1 ~ age_group)

p2_new_neg



p2_new_0_neg<-ggplot(surv_differences_all_new_0, aes(x=cancer_label, y=surv_dif_2)) + 
  geom_point()+ 
  coord_flip()+
  labs(title="Net Survival Difference between various consecutive Globocan age groups",
       x = "Cancer", y = "Net Survival Difference (units)")+
  facet_grid(1 ~ age_group)

p2_new_0_neg


p2_new_HDI_neg<-ggplot(surv_differences_all_new, aes(x=cancer_label, y=surv_dif_2)) + 
  geom_point()+ 
  coord_flip()+
  labs(title="  Net Survival Difference larger than 0.2 between various consecutive Globocan age groups by HDI",
       x = "Cancer", y = "Net Survival Difference (units)")+
  facet_grid(hdi_group ~ age_group)

p2_new_HDI_neg


p2_new_HDI_0_neg<-ggplot(surv_differences_all_new_0, aes(x=cancer_label, y=surv_dif_2)) + 
  geom_point()+ 
  coord_flip()+
  labs(title="Net Survival Difference between various consecutive Globocan age groups",
       x = "Cancer", y = "Net Survival Difference (units)")+
  facet_grid(hdi_group ~ age_group)

p2_new_HDI_0_neg

#Leukemia
leuk<-surv_differences_all_new_0%>%
  filter(cancer_code==36)


p2_new_0_leuk_neg<-ggplot(leuk, aes(x=cancer_label, y=surv_dif_2)) + 
  geom_point()+ 
  coord_flip()+
  labs(title="Net Survival Difference between various consecutive Globocan age groups for Leukemia",
       x = "Cancer", y = "Net Survival Difference (units)")+
  facet_grid(1 ~ age_group)

p2_new_0_leuk_neg


p2_new_HDI_0_neg_leuk<-ggplot(leuk, aes(x=cancer_label, y=surv_dif_2)) + 
  geom_point()+ 
  coord_flip()+
  labs(title="Net Survival Difference between various consecutive Globocan age groups for Leukemia",
       x = "Cancer", y = "Net Survival Difference (units)")+
  facet_grid(hdi_group ~ age_group)

p2_new_HDI_0_neg_leuk


#
# Saving the outputs

ggsave("surv_Differences_all_age_groups_bigger_20.pdf", width = 20, height = 10, pointsize = 12, 
       path="\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Survival Checks\\", plot=p2_new) 
ggsave("surv_Differences_all_age_groups_all.pdf", width = 20, height = 14, pointsize = 12, 
       path="\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Survival Checks\\", plot=p2_new_0) 
ggsave("surv_Differences_all_age_groups_HDI_bigger_20.pdf", width = 20, height = 10, pointsize = 12, 
       path="\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Survival Checks\\", plot=p2_new_HDI) 
ggsave("surv_Differences_all_age_groups_HDI_all.pdf", width = 20, height = 14, pointsize = 12, 
       path="\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Survival Checks\\", plot=p2_new_HDI_0) 
ggsave("surv_Differences_all_age_groups_Leukemia.pdf", width = 20, height = 10, pointsize = 12, 
       path="\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Survival Checks\\", plot=p2_new_0_leuk) 
ggsave("surv_Differences_all_age_groups_HDI_all_Leukemia.pdf", width = 20, height = 10, pointsize = 12, 
       path="\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Survival Checks\\", plot=p2_new_HDI_0_leuk) 

# All differences with direction

ggsave("surv_Differences_all_age_groups_bigger_20_posneg.pdf", width = 20, height = 10, pointsize = 12, 
       path="\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Survival Checks\\", plot=p2_new_neg) 
ggsave("surv_Differences_all_age_groups_all_posneg.pdf", width = 20, height = 14, pointsize = 12, 
       path="\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Survival Checks\\", plot=p2_new_0_neg) 
ggsave("surv_Differences_all_age_groups_HDI_bigger_20_posneg.pdf", width = 20, height = 10, pointsize = 12, 
       path="\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Survival Checks\\", plot=p2_new_HDI_neg) 
ggsave("surv_Differences_all_age_groups_HDI_all_posneg.pdf", width = 20, height = 14, pointsize = 12, 
       path="\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Survival Checks\\", plot=p2_new_HDI_0_neg) 
ggsave("surv_Differences_all_age_groups_Leukemia_posneg.pdf", width = 20, height = 10, pointsize = 12, 
       path="\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Survival Checks\\", plot=p2_new_0_leuk_neg) 
ggsave("surv_Differences_all_age_groups_HDI_all_Leukemia_posneg.pdf", width = 20, height = 10, pointsize = 12, 
       path="\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Survival Checks\\", plot=p2_new_HDI_0_neg_leuk) 


#Checking so survival is decreasing between times 1,3,5 years


surv_new<-surv_new2%>%
  filter(time%in%c(1,2,3,4,5))%>% #comment to check all survival time differences 
  select(-country_name)%>%
  left_join(HDI, by=c("country_code", "hdi_group"))%>%
  # dplyr::mutate(rel_surv=case_when(rel_surv>1~ 1,
  #                                  rel_surv<=1~ rel_surv))%>%
  dplyr::mutate(hdi_group=case_when(hdi_group==1~ "1) Low",
                                    hdi_group==2~ "2) Medium",
                                    hdi_group==3~ "3) High",
                                    hdi_group==4~ "4) Very High"))

#computing 1-3 and 3-5 years differences

survt_differences<-surv_new%>%
  group_by(country_code, country_label,cancer_code, cancer_label, age)%>%
  mutate(survt_dif = rel_surv - lag(rel_surv, order_by = c(time)))%>%
 # mutate(surv_dif_2 = rel_surv - lag(rel_surv, order_by = c(time)))%>%
  mutate(time_dif=paste(as.character(time),"-",lag(time,  order_by = time)))%>%
  ungroup()%>%
  filter(time!=1)#%>%
  #filter(abs(surv_dif)>0.20)

#see which increased... we want all changes to be negative

survt_differences_pos<-survt_differences%>%
  filter(survt_dif<0)

#see which had no change

survt_differences_null<-survt_differences%>%
  filter(survt_dif==0)


# if error produce list of problems. Otherwise not produced. 


if(nrow(survt_differences_pos)>0){
write.csv(survt_differences_pos,"\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Survival Checks\\survt_pos_dif.csv")
  } else {
      print("No increase in relative survival for later survival time")
    }

if(nrow(survt_differences_null)>0){ 
  write.csv(survt_differences_null,"\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Survival Checks\\survt_null_dif.csv")
} else {
  print("No null changes in relative survival for later survival time")
}




# Figures to compare relative survival at different times: by hdi, age, etc. 

survt_differences_large_difs<-survt_differences%>%
  filter(survt_dif< (-0.20))#%>%
 # filter(survt_dif!=0 )

survt_differences_all_difs<-survt_differences#%>%
  #filter(survt_dif< (-0.20))#%>%

#By age 

plot_survt_big_dif_ages<-ggplot(survt_differences_large_difs, aes(x=cancer_label, y=survt_dif)) + 
  geom_point()+ 
  coord_flip()+
  labs(title="  Net Survival Difference larger than 0.2 between various consecutive time points (years) by age group",
       x = "Cancer", y = "Net Survival Difference (units)")+
  facet_grid(age ~ time_dif)

plot_survt_big_dif_ages


plot_survt_big_dif_ages_all<-ggplot(survt_differences, aes(x=cancer_label, y=survt_dif)) + 
  geom_point()+ 
  coord_flip()+
  labs(title="  Net Survival Difference larger than 0.2 between various consecutive time points (years) by age group",
       x = "Cancer", y = "Net Survival Difference (units)")+
  facet_grid(age ~ time_dif)

plot_survt_big_dif_ages_all


survt_differences

plot_survt_big_dif_HDI<-ggplot(survt_differences_large_difs, aes(x=cancer_label, y=survt_dif)) + 
  geom_point()+ 
  coord_flip()+
  labs(title="  Net Survival Difference larger than 0.2 between various consecutive Globocan time points (years) by HDI group",
       x = "Cancer", y = "Net Survival Difference (units)")+
  facet_grid(hdi_group ~ time_dif)

plot_survt_big_dif_HDI


plot_survt_big_dif_all<-ggplot(survt_differences_all_difs, aes(x=cancer_label, y=survt_dif)) + 
  geom_point()+ 
  coord_flip()+
  labs(title="  Net Survival Difference between consequtive time points (years) overall by cancer site",
       x = "Cancer", y = "Net Survival Difference (units)")+
  facet_grid(1 ~ time_dif)

plot_survt_big_dif_all


#Writing the new diagnostic plots
ggsave("time_surv_dif_ages.pdf", width = 20, height = 18, pointsize = 12, 
       path="\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Survival Checks\\", plot=plot_survt_big_dif_ages) 
ggsave("time_surv_dif_ages_all.pdf", width = 20, height = 50, pointsize = 12,  limitsize = FALSE,
       path="\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Survival Checks\\", plot=plot_survt_big_dif_ages_all) 
ggsave("time_surv_dif_HDI.pdf", width = 20, height = 10, pointsize = 12, 
       path="\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Survival Checks\\", plot=plot_survt_big_dif_HDI) 
ggsave("time_surv_dif_all.pdf", width = 20, height = 10, pointsize = 12, 
       path="\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Survival Checks\\", plot=plot_survt_big_dif_all) 


plot_survt_big_dif_ages
plot_survt_big_dif_HDI
plot_survt_big_dif_all
plot_survt_big_dif_ages_all

