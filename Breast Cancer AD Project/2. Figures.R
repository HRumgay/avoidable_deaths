#Script for main figures in manuscript 
AD_all2
AD_region2
AD_HDI2
Avoidable_Deaths_age_cat2


# select(
#   "country_code", "country_label","age_cat",
#   "cancer_code", "cancer_label",
#   "AD", "AD_Lower", "AD_Upper",
#   "pAD", "pAD_Lower", "pAD_Upper",
#   "AD_med", "AD_Lower_med","AD_Upper_med", 
#   "pAD_med","pAD_Lower_med", "pAD_Upper_med",
#   "AD_max", "AD_Lower_max", "AD_Upper_max",
#   "pAD_max", "pAD_Lower_max", "pAD_Upper_max",
#   "total_deaths")

AD_11<-Avoidable_Deaths_age_cat2%>%
  select(  "country_code", "country_label","age_cat",
           "cancer_code", "cancer_label",
           "pAD_max", "pAD_Lower_max", "pAD_Upper_max")%>%
  rename("pAD"="pAD_max")%>%
  rename("pAD"="pAD_Lower_max")%>%
  rename("pAD"="pAD_Upper_max")%>%
  mutate(Reference="Max by HDI")

AD_12<-Avoidable_Deaths_age_cat2%>%
  select(  "country_code", "country_label","age_cat",
           "cancer_code", "cancer_label",
           "pAD_med", "pAD_Lower_med","pAD_Upper_med")%>%
  rename("pAD"="pAD_med")%>%
  rename("pAD"="pAD_Lower_med")%>%
  rename("pAD"="pAD_Upper_med")%>%
  mutate(Reference="Median by HDI")

AD_props<-Avoidable_Deaths_age_cat2 %>%
  select(  "country_code", "country_label","age_cat",
           "cancer_code", "cancer_label",
           "pAD", "pAD_Lower", "pAD_Upper" ) %>%
  mutate(Reference = "93%") %>%
  full_join(AD_11) %>%
  full_join(AD_12)


#Figure 1 lollipop chart 
Avoidable_Deaths_age_cat2 %>% 
  filter(age_cat=="Overall")%>%
  ggdotchart(x = "country_label", y = "pAD",
             color = "Reference",                             # Color by groups
             palette = c( "blue", "#FC4E07"), # Custom color palette    "#00AFBB", "#E7B800",
             sorting = "descending",                       # Sort value in descending order
             add = "segments", 
             group = "Reference", # Add segments from y = 0 to dots
             rotate = TRUE,                                # Rotate vertically                               # Order by groups
             dot.size = 8,                                 # Large dot size
             label = round(Avoidable_Deaths_age_cat2$pAD,1),                        # Add mpg values as dot labels
             font.label = list(color = "white", size = 9, 
                               vjust = 0.5),               # Adjust label parameters
             # position = position_dodge2(1),
             xlab="Country", 
             ylab="Proportion Treatable Avoidable Deaths (pAD, %)",
             title="Proportion Treatable Avoidable Deaths for Breast Cancer in SURVCAN-3, 
             Reference when reference survival is 93%",
             ggtheme = theme_pubr())   ->pAD_plot_3

pAD_india_2_alt_plot_3