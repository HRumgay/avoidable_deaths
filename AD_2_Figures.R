Avoidable_Deaths2 <- Avoidable_Deaths %>% mutate(Scenario = "RWD")%>%
  select(-cancer)
  
  
Avoidable_Deaths_Simulated2 <-
  Avoidable_Deaths_Simulated %>% mutate(Scenario = "Modeled")%>%
  select(-cancer)



AD_plotable <-
  Avoidable_Deaths2 %>% full_join(Avoidable_Deaths_Simulated2)%>%
  mutate(cancer_code=as.integer(cancer_code))%>%
  mutate_if(is.numeric, replace_na, 0)%>%left_join(Cancer_codes, by="cancer_code")%>%
  filter(!is.na(AD_treat))


# AD_plotable %>% ggplot(aes(x = "", y = value, fill = group)) +
#   geom_col(color = "black") +
#   geom_text(aes(label = value),
#             position = position_stack(vjust = 0.5)) +
#   coord_polar(theta = "y") +
#   guides(fill = guide_legend(title = "Title")) +
#   facet_grid(. ~ Scenario)
# 
# 
# mutate_if(.<0,0)
# 
# 
# mutate_if(any_column_NA,replace_NA_0)



AD_barplot_treat <- AD_plotable %>%
  group_by(cancer_label)%>%
  ggplot(
    aes(cancer_label, AD_treat,  
        ymin = min(AD_treat),
        ymax = max(AD_treat),),
    mapping = aes(
      reorder(cancer_label, AD_treat),AD_treat,
      fill = age_cat,drop=FALSE,na.rm = TRUE
    )
  ) +
  xlab("Cancer Site") +
  ylab("AD") +
  ggtitle("AD due to treatment") +
  geom_bar(stat = "identity", alpha = 0.4,
           position = "dodge") +
  coord_flip() +
  # geom_errorbar(
  #   aes(x = cancer_label, ymin = AD_treat_Lower, ymax = AD_treat_Upper),
  #   width = 0.4,
  #   alpha = 0.9,
  #   size = 1.3
  # ) +
facet_grid(. ~ Scenario,drop=FALSE,scales = "free_x")




AD_barplot_prev <- AD_plotable %>%
  group_by(cancer_label)%>%
  ggplot(
    aes(cancer_label, AD_prev,  
        ymin = min(AD_prev),
        ymax = max(AD_prev),),
    mapping = aes(
      reorder(cancer_label, AD_prev),AD_prev,
      fill = age_cat,drop=FALSE
    )
  ) +
  xlab("Cancer Site") +
  ylab("AD") +
  ggtitle("AD Preventable due to Risk Factors") +
  geom_bar(stat = "identity", alpha = 0.4,
           position = "dodge") +
  coord_flip() +
  # geom_errorbar(
  #   aes(x = cancer_label, ymin = AD_treat_Lower, ymax = AD_treat_Upper),
  #   width = 0.4,
  #   alpha = 0.9,
  #   size = 1.3
  # ) +
  facet_grid(. ~ Scenario,drop=FALSE,scales = "free_x")

AD_barplot_unavoid <- AD_plotable %>%
  group_by(cancer_label)%>%
  ggplot(
    aes(cancer_label, AD_unavoid,  
        ymin = min(AD_unavoid),
        ymax = max(AD_unavoid),),
    mapping = aes(
      reorder(cancer_label, AD_unavoid),AD_unavoid,
      fill = age_cat,drop=FALSE
    )
  ) +
  xlab("Cancer Site") +
  ylab("AD") +
  ggtitle("AD Unavoidable") +
  geom_bar(stat = "identity", alpha = 0.4,
           position = "dodge") +
  coord_flip() +
  # geom_errorbar(
  #   aes(x = cancer_label, ymin = AD_treat_Lower, ymax = AD_treat_Upper),
  #   width = 0.4,
  #   alpha = 0.9,
  #   size = 1.3
  # ) +
  facet_grid(. ~ Scenario,drop=FALSE,scales = "free_x")



#Calling the plots 

AD_barplot_treat

AD_barplot_prev

AD_barplot_unavoid




