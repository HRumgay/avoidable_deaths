library(grid)
library(ggpubr)



#Script for main figures in manuscript 
AD_all2
AD_continent2
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
  dplyr::rename("pAD"="pAD_max")%>%
  dplyr::rename("pAD_Lower"="pAD_Lower_max")%>%
  dplyr::rename("pAD_Upper"="pAD_Upper_max")%>%
  mutate(Reference="100% Net Survival")

# AD_12<-Avoidable_Deaths_age_cat2%>%
#   select(  "country_code", "country_label","age_cat",
#            "cancer_code", "cancer_label",
#            "pAD_med", "pAD_Lower_med","pAD_Upper_med")%>%
#   dplyr::rename("pAD"="pAD_med")%>%
#   #  dplyr::rename("pAD"="pAD_Lower_med")%>%
#   #  dplyr::rename("pAD"="pAD_Upper_med")%>%
#   dplyr::mutate(Reference="Median by HDI")

AD_props <- Avoidable_Deaths_age_cat2 %>%
  select(  "country_code", "country_label","age_cat",
           "cancer_code", "cancer_label",
           "pAD", "pAD_Lower", "pAD_Upper" ) %>%
  mutate(Reference = "Sweden") %>%
  full_join(AD_11) %>%
  #full_join(AD_12)%>%
  filter(pAD!=0)

AD_props_overall<-AD_props%>%
  filter(age_cat=="Overall")  

AD_props_low<-AD_props%>%
  filter(age_cat=="15-49")  

AD_props_mid<-AD_props%>%
  filter(age_cat=="50-64")  

AD_props_upp<-AD_props%>%
  filter(age_cat=="65-99")  


#Figure 2 - plot with CIs and color by continent or HDI. Facet by age group three columns

library(RColorBrewer)
#display.brewer.all()

continents2<-continents %>% dplyr::rename("continent_label"="country_label")

AD_props%>% 
  as.data.frame()%>%
  left_join(HDI%>%select(-country_label),
            by=c("country_code"))%>%
  left_join(HDI_Region_Mapping2) %>%
  mutate(hdi_group=as.character(hdi_group)) %>%
  # left_join(areas) %>%
  select(-country_code, -area)%>%
  left_join(continents2,by=c("continent"))%>%
  arrange(continent, desc(pAD))%>%
  group_by(continent_label, age_cat) %>%
  filter(Reference=="Sweden")->AD_props_2

AD_prop_order<-AD_props_2%>%
  ungroup%>%
  filter(age_cat=="Overall")%>%
  select(continent, country_label,  pAD)%>%
  group_by(continent)%>%
  dplyr::arrange(desc(pAD))%>%
  ungroup()%>%
  dplyr::mutate(order=row_number())%>%
  ungroup%>%
  select(-pAD) 

AD_props_2<-AD_props_2%>%
  left_join(AD_prop_order, 
            by=c("continent","country_label"))%>%
  arrange(continent_label, order)%>%
  mutate(hdi_group = case_when(hdi_group==1 ~ "Low",
                               hdi_group==2 ~ "Medium",
                               hdi_group==3 ~ "High",
                               hdi_group==4 ~ "Very High"))


AD_props_2%>%
  ggplot(aes(x =reorder(country_label, -order), 
             y =pAD ,
             color = hdi_group
             )) + 
  
  geom_errorbar(aes(ymin=pAD_Lower,
                    ymax=pAD_Upper,
                    width=0.2),
                #colour="black"
  )+
  ylim(0,100)+
  geom_point()+                                              # Change color brewer palette
  scale_colour_brewer(palette = "Set1")+
  coord_flip()+
  labs(y =  "Avoidable Deaths Breast Cancer Deaths (%)", x ="Country")+
  ggtitle("Proportion Avoidable Deaths for Breast Cancer in SURVCAN-3, by Age Group")+
  theme_minimal()+
  facet_wrap(continent_label ~ age_cat, scales = "free", 
  #switch = "both",
  strip.position = c("top")
  ) +
  scale_color_discrete(name="HDI Group")->figure_2

figure_2

ggsave(plot=figure_2, "\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\Breast Cancer\\Figures\\figure_2_props.png", 
       width=15 , height=10)

#Figure 3 - lollipop chart comparing the secondary analysis 

AD_props_low %>%
  ggdotchart(x = "country_label", y = "pAD",
             color = "Reference",                             # Color by groups
             palette = c("blue", "#FC4E07", "#E7B800"), # Custom color palette    "#00AFBB", "#E7B800",
             sorting = "descending",                       # Sort value in descending order
             add = "segments", 
             group = "Reference", # Add segments from y = 0 to dots
             rotate = TRUE,                                # Rotate vertically                           
             # Order by groups
             dot.size = 8,                                 # Large dot size
             label = round(AD_props_low$pAD,1),                        # Add mpg values as dot labels
             font.label = list(color = "white", size = 9, 
                               vjust = 0.5),               # Adjust label parameters
             # position = position_dodge2(1),
             xlab="Country", 
             ylab="Proportion Treatable Avoidable Deaths (pAD, %)",
             title="Proportion Treatable Avoidable Deaths for Breast Cancer in SURVCAN-3 in 2022, 
             Reference when reference survival is that of Swedebe, and the theoretical maximum survival",
ggtheme = theme_pubr()) -> pAD_Breast_plot_low



AD_props_mid %>% 
  ggdotchart(x = "country_label", y = "pAD",
             color = "Reference",                             # Color by groups
             palette = c( "blue", "#FC4E07", "#E7B800"), # Custom color palette    "#00AFBB", "#E7B800",
             sorting = "descending",                       # Sort value in descending order
             add = "segments", 
             group = "Reference", # Add segments from y = 0 to dots
             rotate = TRUE,                                # Rotate vertically                           
             # Order by groups
             dot.size = 8,                                 # Large dot size
             label = round(AD_props_mid$pAD,1),                        # Add mpg values as dot labels
             font.label = list(color = "white", size = 9, 
                               vjust = 0.5),               # Adjust label parameters
             # position = position_dodge2(1),
             xlab="Country", 
             ylab="Proportion Treatable Avoidable Deaths (pAD, %)",
             title="Proportion Treatable Avoidable Deaths for Breast Cancer in SURVCAN-3 in 2022, 
             Reference when reference survival is that of Swedebe, and the theoretical maximum survival",
ggtheme = theme_pubr())   ->pAD_Breast_plot_mid



AD_props_upp %>% 
  ggdotchart(x = "country_label", y = "pAD",
             color = "Reference",                             # Color by groups
             palette = c( "blue", "#FC4E07", "#E7B800"), # Custom color palette    "#00AFBB", "#E7B800",
             sorting = "descending",                       # Sort value in descending order
             add = "segments", 
             group = "Reference", # Add segments from y = 0 to dots
             rotate = TRUE,                                # Rotate vertically                           
             # Order by groups
             dot.size = 8,                                 # Large dot size
             label = round(AD_props_upp$pAD,1),                        # Add mpg values as dot labels
             font.label = list(color = "white", size = 9, 
                               vjust = 0.5),               # Adjust label parameters
             # position = position_dodge2(1),
             xlab="Country", 
             ylab="Proportion Treatable Avoidable Deaths (pAD, %)",
             title="Proportion Treatable Avoidable Deaths for Breast Cancer in SURVCAN-3 in 2022, 
             Reference when reference survival is that of Swedebe, and the theoretical maximum survival",
ggtheme = theme_pubr())   ->pAD_Breast_plot_upp



AD_props_overall_order<-AD_props_overall%>%
  ungroup%>%
  filter(Reference=="Sweden")%>%
  select(country_label,  pAD)%>%
  group_by(country_label)%>%
  dplyr::arrange(desc(pAD))%>%
  ungroup()%>%
  dplyr::mutate(order=row_number())%>%
  ungroup()%>%
  select(-pAD)

AD_props_overall<-AD_props_overall%>%
  left_join(AD_props_overall_order, 
            by=c("country_label"))%>%
  arrange(order)

AD_props_overall<-AD_props_overall %>%
  mutate(country_label = factor(country_label, levels = unique(country_label))) 


pAD_Breast_plot_overall <- ggplot(AD_props_overall, aes(x = country_label, y = pAD, color = Reference)) +
  geom_point(size = 9) +
  geom_segment(aes(xend = country_label, yend = 0), color = "black") +
  geom_errorbar(aes(ymin = pAD_Lower, ymax = pAD_Upper), width = 0.4) +
  geom_text(aes(label = round(pAD, 1)), color = "white", size = 3.5, vjust = 0.5) +
  coord_flip() +
  labs(x = "Country", y = "Proportion Treatable Avoidable Deaths (pAD, %)",
       title ="Proportion Treatable Avoidable Deaths for Breast Cancer in SURVCAN-3 in 2022, 
             Reference when reference survival is that of Sweden, and the theoretical maximum survival") +
  scale_color_manual(values = c("blue", "#FC4E07", "green")) +
  theme_pubr()

pAD_Breast_plot_overall






pAD_Breast_plot_low
pAD_Breast_plot_mid

pAD_Breast_plot_upp
pAD_Breast_plot_overall

#Saving the outputs


ggsave(plot=pAD_Breast_plot_overall, "\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\Breast Cancer\\Figures\\primary_secondary_lollipop_overall.png", 
       width=20 , height=15)




