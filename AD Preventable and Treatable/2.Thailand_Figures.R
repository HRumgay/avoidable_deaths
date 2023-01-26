
# load libraries needed for some charts
library(tidyverse)
library(data.table)
library(Rcan)



# Avoidable_Deaths2 <- Avoidable_Deaths %>% dplyr::mutate(Scenario = "RWD")%>%
#   select(-cancer)
#   
#   
# Avoidable_Deaths_Simulated2 <-
#   Avoidable_Deaths_Simulated %>% dplyr::mutate(Scenario = "Modeled")%>%
#   select(-cancer)
# 
# 
# 
# AD_plotable <-
#   Avoidable_Deaths2 %>% full_join(Avoidable_Deaths_Simulated2)%>%
#   dplyr::mutate(cancer_code=as.integer(cancer_code))%>%
#   dplyr::mutate_if(is.numeric, replace_na, 0)%>%left_join(Cancer_codes, by="cancer_code")%>%
#   filter(!is.na(AD_treat))


#Creating a plotable object comparing Thailand simulated and RWD

Avoidable_Deaths2 <- Avoidable_Deaths_overall %>% 
  dplyr::mutate(Scenario = "SURVCAN Data")%>%
mutate_all(funs(ifelse(. < 0, 0, .)))

Avoidable_Deaths_Simulated2 <- Avoidable_Deaths_Simulated_All_age_cat %>% 
  dplyr::mutate(Scenario = "Modelled Survival")%>%
  filter(country_label=="Thailand")%>%
  filter(!is.na(AD_treat))%>%
  filter(age_cat=="Overall")

Cancer_codes2 <- cancer_codes %>%select(cancer_code)

AD_plotable <-  Avoidable_Deaths2 %>%
  as.data.frame()%>%
  ungroup()%>%
#  select(-cancer_label)%>%
 # rename("total_overall"="total")%>%
  full_join(Avoidable_Deaths_Simulated2)%>%
  dplyr::mutate(cancer_code=as.integer(cancer_code))%>%
  dplyr::mutate_if(is.numeric, replace_na, 0)%>%
 left_join(ten_cancer_sites, by=c("cancer_code"))%>%
  dplyr::mutate(AD_treat_prop=AD_treat/AD_sum*100)%>%
  dplyr::mutate(AD_prev_prop=AD_prev/AD_sum*100)%>%
  dplyr::mutate(AD_unavoid_prop=AD_unavoid/AD_sum*100)%>%
  filter(!is.na(cancer_label))%>%
  mutate(cancer = replace(cancer, cancer == "Liver and intrahepatic bile ducts", "Liver"))%>%
  mutate(cancer = replace(cancer, cancer == "Trachea, bronchus and lung", "Lung"))

#Data by cancer site prep
AD_by_cancer_site <-Avoidable_Deaths_Simulated_All_age_cat%>%
 filter(age_cat=="Overall")

AD_by_cancer_site_3<-AD_by_cancer_site%>%
  select(cancer, cancer_code, AD_treat)%>%
  rename("AD"="AD_treat")%>%
  dplyr::mutate(AD_cat="Treatable")

AD_by_cancer_site_2<-AD_by_cancer_site%>%
  select(cancer, cancer_code, AD_prev)%>%
  rename("AD"="AD_prev")%>%
  dplyr::mutate(AD_cat="Preventable")

AD_by_cancer_site_1<-AD_by_cancer_site%>%
  select(cancer, cancer_code, AD_unavoid)%>%
  rename("AD"="AD_unavoid")%>%
  dplyr::mutate(AD_cat="Unavoidable")%>%
  full_join(AD_by_cancer_site_2)%>%
  full_join(AD_by_cancer_site_3)


AD_barplot_treat <- AD_plotable %>%
  group_by(Scenario, cancer_label)%>%
  ggplot(
    aes(cancer_label, AD_treat, fill=Scenario,
        ymin = min(AD_treat),
        ymax = max(AD_treat)),
    mapping = aes(
      reorder(cancer_label, AD_treat),AD_treat,
      fill = Scenario,drop=FALSE,na.rm = TRUE
    )
  ) +
  xlab("Cancer Site") +
  ylab("AD") +
  scale_fill_manual(values = c('#de2d26','#fc9272',  '#fee0d2')) + #choose some nice colours for your bars
  ggtitle("Potentially Avoidable Deaths for Thailand due to treatment") +
  geom_col(position = "dodge")+
  coord_flip() 

#ggplot(cabbage_exp, aes(x = Date, y = Weight, fill = Cultivar)) +


AD_barplot_treat 

#+
  # geom_errorbar(
  #   aes(x = cancer, ymin = AD_treat_Lower, ymax = AD_treat_Upper),
  #   width = 0.4,
  #   alpha = 0.9,
  #   size = 1.3
  # ) +
#facet_grid(. ~ Scenario,drop=FALSE,scales = "free_y")




AD_barplot_prev <- AD_plotable %>%
  group_by(Scenario, cancer)%>%
  ggplot(
    aes(cancer, AD_prev,  fill=Scenario,
        ymin = min(AD_prev),
        ymax = max(AD_prev)),
    mapping = aes(
      reorder(cancer, AD_prev),AD_prev,
      fill = Scenario,drop=FALSE
    )
  ) +
  xlab("Cancer Site") +
  ylab("AD") +
  ggtitle("Number Avoidable Deaths from Risk Factor Prevention in Thailand") +
  scale_fill_manual(values = c('#de2d26','#fc9272',  '#fee0d2')) + #choose some nice colours for your bars
  geom_bar(stat = "identity", 
           position = "dodge") +
  coord_flip() #+
  # geom_errorbar(
  #   aes(x = cancer, ymin = AD_treat_Lower, ymax = AD_treat_Upper),
  #   width = 0.4,
  #   alpha = 0.9,
  #   size = 1.3
  # ) +
  #facet_grid(. ~ Scenario,drop=FALSE,scales = "free_y")
AD_barplot_prev


# AD_barplot_unavoid <- AD_plotable %>%
#   group_by(cancer)%>%
#   ggplot(
#     aes(cancer, AD_unavoid,  
#         ymin = min(AD_unavoid),
#         ymax = max(AD_unavoid)),
#     mapping = aes(
#       reorder(cancer, AD_unavoid),AD_unavoid,
#       fill = age_cat,drop=FALSE
#     )
#   ) +
#   xlab("Cancer Site") +
#   ylab("AD") +
#   scale_fill_manual(values = c('#de2d26','#fc9272',  '#fee0d2')) + #choose some nice colours for your bars
#   ggtitle("AD Unavoidable") +
#   geom_bar(stat = "identity",
#            position = "dodge") +
#   coord_flip() +
#   # geom_errorbar(
#   #   aes(x = cancer, ymin = AD_treat_Lower, ymax = AD_treat_Upper),
#   #   width = 0.4,
#   #   alpha = 0.9,
#   #   size = 1.3
#   # ) +
#   facet_grid(. ~ Scenario,drop=FALSE,scales = "free_y")
# 

AD_barplot_treat_prop <- AD_plotable %>%
  group_by(cancer)%>%
  ggplot(
    aes(cancer, AD_treat_prop,  
        ymin = min(AD_treat_prop),
        ymax = max(AD_treat_prop)),
    mapping = aes(
      reorder(cancer, AD_treat_prop),AD_treat_prop,
      fill = age_cat,drop=FALSE,na.rm = TRUE
    )
  ) +
  xlab("Cancer Site") +
  ylab("AD") +
  scale_fill_manual(values = c('#de2d26','#fc9272',  '#fee0d2')) + #choose some nice colours for your bars
  ggtitle("AD due to treatment  (%)") +
  geom_bar(stat = "identity", 
           position = "dodge") +
  coord_flip() +
  # geom_errorbar(
  #   aes(x = cancer, ymin = AD_treat_prop_Lower, ymax = AD_treat_prop_Upper),
  #   width = 0.4,
  #   alpha = 0.9,
  #   size = 1.3
  # ) +
  facet_grid(. ~ Scenario,drop=FALSE,scales = "free_y")

AD_barplot_prev_prop <- AD_plotable %>%
  group_by(cancer)%>%
  ggplot(
    aes(cancer, AD_prev_prop,  
        ymin = min(AD_prev_prop),
        ymax = max(AD_prev_prop)),
    mapping = aes(
      reorder(cancer, AD_prev_prop),AD_prev_prop,
      fill = age_cat,drop=FALSE
    )
  ) +
  xlab("Cancer Site") +
  ylab("AD") +
  ggtitle("Proportion avoidable deaths Preventable due to Risk Factors  (%)") +
  scale_fill_manual(values = c('#de2d26','#fc9272',  '#fee0d2')) + #choose some nice colours for your bars
  geom_bar(stat = "identity", 
           position = "dodge") +
  coord_flip() +
  # geom_errorbar(
  #   aes(x = cancer, ymin = AD_treat_prop_Lower, ymax = AD_treat_prop_Upper),
  #   width = 0.4,
  #   alpha = 0.9,
  #   size = 1.3
  # ) +
  facet_grid(. ~ Scenario,drop=FALSE,scales = "free_y")

# AD_barplot_unavoid_prop <- AD_plotable %>%
#   group_by(cancer)%>%
#   ggplot(
#     aes(cancer, AD_unavoid_prop,  
#         ymin = min(AD_unavoid_prop),
#         ymax = max(AD_unavoid_prop)),
#     mapping = aes(
#       reorder(cancer, AD_unavoid_prop),AD_unavoid_prop,
#       fill = age_cat,drop=FALSE
#     )
#   ) +
#   xlab("Cancer Site") +
#   ylab("AD") +
#   scale_fill_manual(values = c('#de2d26','#fc9272',  '#fee0d2')) + #choose some nice colours for your bars
#   ggtitle("AD Unavoidable (%)") +
#   geom_bar(stat = "identity",
#            position = "dodge") +
#   coord_flip() +
#   # geom_errorbar(
#   #   aes(x = cancer, ymin = AD_treat_prop_Lower, ymax = AD_treat_prop_Upper),
#   #   width = 0.4,
#   #   alpha = 0.9,
#   #   size = 1.3
#   # ) +
#   facet_grid(. ~ Scenario,drop=FALSE,scales = "free_y")
# 

#Calling the plots 

AD_barplot_treat

AD_barplot_prev

AD_barplot_treat_prop

AD_barplot_prev_prop



ggsave("~/Documents/R Figures/AD_barplot_treat.png",AD_barplot_treat, height=10, width=10)
ggsave("~/Documents/R Figures/AD_barplot_prev.png",AD_barplot_prev, height=10, width=10)
ggsave("~/Documents/R Figures/AD_barplot_treat_prop.png",AD_barplot_treat, height=10, width=10)
ggsave("~/Documents/R Figures/AD_barplot_prev_prop.png",AD_barplot_prev, height=10, width=10)


# Net Survival

#Creating a plotable object comparing Thailand simulated and RWD

NS_SURVCAN <- NS_OS_PAF %>% 
  dplyr::mutate(Scenario = "SURVCAN Data")%>%
  rename("rel_surv"="Five_Year_Net_Surv")%>%
  group_by(cancer_code)%>%
  mutate(age_cat="Overall")%>%
  mutate(rel_surv= sum(rel_surv*total_overall, na.rm=T)/sum(total_overall, na.rm=T))%>%
  select(Scenario, cancer_label, cancer_code,age_cat, rel_surv)%>%
  distinct()


NS_Modelled <- Simulated_Data_PAF_All %>% 
  dplyr::mutate(Scenario = "Modelled Survival")%>%
  full_join(colorectal)%>%
  mutate(cancer_label = replace(cancer_label, cancer_label == "Liver and intrahepatic bile ducts", "Liver"))%>%
  mutate(cancer_label = replace(cancer_label, cancer_label == "Trachea, bronchus and lung", "Lung"))%>%
  mutate(cancer_label = replace(cancer_label, cancer_label == "Colon", "Colorectal"))%>%
  mutate(cancer_label = replace(cancer_label, cancer_label == "Rectum", "Colorectal"))%>%
  mutate(cancer_code = replace(cancer_code, cancer_code == 9, 38))%>%
  mutate(cancer_code = replace(cancer_code, cancer_code == 8, 38))%>%
  filter(country_label=="Thailand")%>%
  select(-age,-sex)%>%
  group_by(cancer_code)%>%
  mutate(age_cat="Overall")%>%
  mutate(rel_surv= sum(rel_surv*total_overall, na.rm=T)/sum(total_overall, na.rm=T))%>%
  select(Scenario, cancer_label, cancer_code,age_cat, rel_surv)%>%
  distinct()%>%
  filter(cancer_code %in% NS_SURVCAN$cancer_code)

Cancer_codes2 <- cancer_codes %>%select(cancer_code)

NS_plotable <-  NS_Modelled%>%
  as.data.frame()%>%
  ungroup()%>%
  #  select(-cancer_label)%>%
  # rename("total_overall"="total")%>%
  full_join(NS_SURVCAN)%>%
  dplyr::mutate(cancer_code=as.integer(cancer_code))%>%
#  left_join(ten_cancer_sites, by=c("cancer_code"))%>%
  dplyr::mutate(rel_surv=rel_surv*100)%>%
 filter(!is.na(cancer_label))%>%
  mutate(cancer_label = replace(cancer_label, cancer_label == "Liver and intrahepatic bile ducts", "Liver"))%>%
  mutate(cancer_label = replace(cancer_label, cancer_label == "Trachea, bronchus and lung", "Lung"))

#Data by cancer site prep

NS_barplot <- NS_plotable  %>%
  group_by(Scenario, cancer_label)%>%
  ggplot(
    aes(cancer_label, rel_surv, fill=Scenario,
        ymin = 0,
        ymax = 100),
    mapping = aes(
      reorder(cancer_label, rel_surv),rel_surv,
      fill = Scenario,drop=FALSE,na.rm = TRUE
    )
  ) +
  xlab("Cancer Site") +
  ylab("Five-year net survival (%)") +
 # scale_fill_manual(values = c('#de2d26','#fc9272',  '#fee0d2')) + #choose some nice colours for your bars
  ggtitle("Net survival estimates, SURVCAN versus Modelled") +
  geom_col(position = "dodge")+
  coord_flip() 

#ggplot(cabbage_exp, aes(x = Date, y = Weight, fill = Cultivar)) +


NS_barplot



