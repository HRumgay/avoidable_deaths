library(grid)
library(ggpubr)

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
  dplyr::rename("pAD"="pAD_max")%>%
   dplyr::rename("pAD_Lower"="pAD_Lower_max")%>%
    dplyr::rename("pAD_Upper"="pAD_Upper_max")%>%
  mutate(Reference="Max by HDI")

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
  mutate(Reference = "93%") %>%
  full_join(AD_11) %>%
#  full_join(AD_12)%>%
  filter(pAD!=0)

AD_props_overall<-AD_props%>%
  filter(age_cat=="Overall")  

AD_props_low<-AD_props%>%
  filter(age_cat=="15-64")  

AD_props_upp<-AD_props%>%
  filter(age_cat=="65-99")  


#Figure 1 lollipop chart 

AD_props_low %>% 
  
  ggdotchart(x = "country_label", y = "pAD",
             color = "Reference",                             # Color by groups
             palette = c( "blue", "#FC4E07", "#E7B800"), # Custom color palette    "#00AFBB", "#E7B800",
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
             title="Proportion Treatable Avoidable Deaths for Breast Cancer in SURVCAN-3, 
Reference when reference survival is 93%, the median survival by HDI, and max survival by HDI",
             ggtheme = theme_pubr())   ->pAD_Breast_plot_low


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
             title="Proportion Treatable Avoidable Deaths for Breast Cancer in SURVCAN-3, 
Reference when reference survival is 93%, the median survival by HDI, and max survival by HDI",
             ggtheme = theme_pubr())   ->pAD_Breast_plot_upp


AD_props_overall %>% 
  
  ggdotchart(x = "country_label", y = "pAD",
             color = "Reference",                             # Color by groups
             palette = c( "blue", "#FC4E07", "#E7B800"), # Custom color palette    "#00AFBB", "#E7B800",
             sorting = "descending",                       # Sort value in descending order
             add = "segments", 
             group = "Reference", # Add segments from y = 0 to dots
             rotate = TRUE,                                # Rotate vertically                           
             # Order by groups
             dot.size = 8,                                 # Large dot size
             label = round(AD_props_overall$pAD,1),                        # Add mpg values as dot labels
             font.label = list(color = "white", size = 9, 
                               vjust = 0.5),               # Adjust label parameters
             # position = position_dodge2(1),
             xlab="Country", 
             ylab="Proportion Treatable Avoidable Deaths (pAD, %)",
             title="Proportion Treatable Avoidable Deaths for Breast Cancer in SURVCAN-3, 
Reference when reference survival is 93%, the median survival by HDI, and max survival by HDI",
             ggtheme = theme_pubr())   -> pAD_Breast_plot_overall


pAD_Breast_plot_low
pAD_Breast_plot_upp
pAD_Breast_plot_overall


#plot1_legend<-
AD_props_overall %>%
  ggplot(aes(x = 2, y = pAD, fill = "Reference")) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  scale_fill_identity("Reference", labels = "Reference",
                      guide = "legend") +
  guides(fill = guide_legend(reverse = TRUE))+
  theme(plot.background= element_blank(),
        plot.title = element_text(size=12, margin=margin(0,0,0,0),hjust = 0.5),
        panel.background = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_blank()) -> plot1_legend

plot1_legend

# function to extract legend from plot
get_only_legend <- function(plot) {
  plot_table <- ggplot_gtable(ggplot_build(plot))
  legend_plot <- which(sapply(plot_table$grobs, function(x) x$name) == "guide-box")
  legend <- plot_table$grobs[[legend_plot]]
  return(legend)
}

# extract legend from plot1 using above function
legend <- get_only_legend(plot1_legend)   

# final combined plot with shared legend
combined_plot <-   grid.arrange(pAD_Breast_plot_low,
                                pAD_Breast_plot_upp,
                                pAD_Breast_plot_overall, ncol = 3)
Top_4_cancer<- grid.arrange(combined_plot, legend, ncol = 2, widths= c(0.85, 0.15))

Top_4_cancer
#Saving the output
ggsave("pie.all.pdf",Top_4_cancer,width=15 ,height=10)


