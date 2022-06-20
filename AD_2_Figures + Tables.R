# Avoidable_Deaths2 <- Avoidable_Deaths %>% mutate(Scenario = "RWD")%>%
#   select(-cancer)
#   
#   
# Avoidable_Deaths_Simulated2 <-
#   Avoidable_Deaths_Simulated %>% mutate(Scenario = "Modeled")%>%
#   select(-cancer)
# 
# 
# 
# AD_plotable <-
#   Avoidable_Deaths2 %>% full_join(Avoidable_Deaths_Simulated2)%>%
#   mutate(cancer_code=as.integer(cancer_code))%>%
#   mutate_if(is.numeric, replace_na, 0)%>%left_join(Cancer_codes, by="cancer_code")%>%
#   filter(!is.na(AD_treat))
# 

AD_by_cancer_site <- Avoidable_Deaths_Simulated_All%>%
  select(country_code, country_label, cancer, cancer_code, AD_treat, AD_prev, 
         AD_unavoid, AD_sum, total_overall)%>%
  mutate(AD_treat=as.numeric(AD_treat))%>%
  mutate(AD_prev=as.numeric(AD_prev))%>%
  mutate(AD_unavoid=as.numeric(AD_unavoid))%>%
  mutate(cancer_code=as.numeric(cancer_code))%>%
  filter(!is.na(AD_treat))%>%
as.data.frame()%>%
  select(-country_code,-country_label, -total_overall)%>%
  ungroup()%>%
  group_by(cancer_code)%>%
  mutate(AD_treat=sum(AD_treat))%>%
  mutate(AD_prev=sum(AD_prev))%>%
  mutate(AD_unavoid=sum(AD_unavoid))%>%
  mutate(AD_sum=sum(AD_sum))  %>%
  ungroup()%>%
  distinct()

AD_by_cancer_site_3<-AD_by_cancer_site%>%
  select(cancer, cancer_code, AD_treat)%>%
  rename("AD"="AD_treat")%>%
  mutate(AD_cat="Treatable")

AD_by_cancer_site_2<-AD_by_cancer_site%>%
  select(cancer, cancer_code, AD_prev)%>%
  rename("AD"="AD_prev")%>%
  mutate(AD_cat="Preventable")

AD_by_cancer_site_1<-AD_by_cancer_site%>%
  select(cancer, cancer_code, AD_unavoid)%>%
  rename("AD"="AD_unavoid")%>%
  mutate(AD_cat="Unavoidable")%>%
  full_join(AD_by_cancer_site_2)%>%
  full_join(AD_by_cancer_site_3)

# Figure 4 - Global burden of cancer by cancer site pie chart 

library(ggrepel)

Figure_4 <- AD_by_cancer_site_1%>%
  ungroup()%>%
  arrange(desc(AD)) %>%
  mutate(  wght = runif(length(AD)))%>%
  mutate(wght = wght/sum(wght))%>%
  mutate(pos = (cumsum(c(0, wght)) + c(wght / 2, .01))[1:nrow(AD_by_cancer_site_1)])%>%
  ggplot(aes(x="", y=wght, fill = AD_cat)) +
  geom_col(color = 'black', 
           position = position_stack(reverse = TRUE), 
           show.legend = TRUE) +
  geom_text_repel(aes(x = 1.4, y = pos, label = cancer), 
                  nudge_x = .3, 
                  segment.size = .7, 
                  show.legend = TRUE) +
  scale_fill_discrete(name = "Type of avoidable deaths", 
                      labels = c("Preventable", "Treatable", "Unavoidable")) +
  coord_polar("y", start=0) +
  labs(title="Preventable and treatable avoidable deaths globally by cancer site")+
  theme_void()

Figure_4


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
        ymax = max(AD_treat)),
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
        ymax = max(AD_prev)),
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
        ymax = max(AD_unavoid)),
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


ggsave("CHARTS/bars/AD_barplot_treat.png",AD_barplot_treat, height=10, width=10)
ggsave("CHARTS/bars/AD_barplot_prev.png",AD_barplot_prev, height=10, width=10)
ggsave("CHARTS/bars/AD_barplot_unavoid.png",AD_barplot_unavoid, height=10, width=10)


AD_plotable %>% 
  pivot_longer(cols = starts_with("AD_"),
               names_to = "type",
               values_to = "deaths") -> AD_plotable_L


AD_plotable_L %>% 
  filter(age_cat=="Overall", type %in% c("AD_treat", "AD_prev", "AD_unavoid")) %>% 
  ggplot(
    aes(
      x = cancer_label,
      y = deaths,
      fill = factor(type, levels = c("AD_prev","AD_treat", "AD_unavoid"))
    )
  ) + 
  geom_bar(stat = 'identity', position = position_fill(reverse = TRUE), colour = 'black', size = .1) +
  #    geom_text(aes(label=paste0(sprintf("%1.1f", count.value))))+
  coord_flip() +
  labs(x = '', y = '', fill = '') +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c('#fc8d59','#ffffbf',  '#99d594')) +
  #facet_grid(REGION_LABEL ~ SEX_LABEL, scales = 'free_y', space = 'free',  labeller = label_wrap_gen(width = 10, multi_line = TRUE)) +
  facet_wrap(~Scenario, nrow = 1, drop = TRUE, scales = 'free_y') +
  theme_minimal() +
  theme(
    legend.position = 'bottom',
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
    panel.grid.major = element_blank(),
    strip.text.y = element_text(angle = 0),
    text = element_text(size = 7)
  )
ggsave("CHARTS/bars/AD_barplot_overall.pdf", height=10, width=10)


AD_plotable_L %>% 
  filter(age_cat=="15-64", type %in% c("AD_treat", "AD_prev", "AD_unavoid")) %>% 
  ggplot(
    aes(
      x = cancer_label,
      y = deaths,
      fill = factor(type, levels = c("AD_prev", "AD_treat",  "AD_unavoid"))
    )
  ) + 
  geom_bar(stat = 'identity', position = position_fill(reverse = TRUE), colour = 'black', size = .1) +
  #    geom_text(aes(label=paste0(sprintf("%1.1f", count.value))))+
  coord_flip() +
  labs(x = '', y = '', fill = '') +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c('#fc8d59','#ffffbf',  '#99d594')) +
  #facet_grid(REGION_LABEL ~ SEX_LABEL, scales = 'free_y', space = 'free',  labeller = label_wrap_gen(width = 10, multi_line = TRUE)) +
  facet_wrap(~Scenario, nrow = 1, drop = TRUE, scales = 'free_y') +
  theme_minimal() +
  theme(
    legend.position = 'bottom',
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
    panel.grid.major = element_blank(),
    strip.text.y = element_text(angle = 0),
    text = element_text(size = 7)
  )
ggsave("CHARTS/bars/AD_barplot_1564.pdf", height=10, width=10)

AD_plotable_L %>% 
  filter(age_cat=="65-99", type %in% c("AD_treat", "AD_prev", "AD_unavoid")) %>% 
  ggplot(
    aes(
      x = cancer_label,
      y = deaths,
      fill = factor(type, levels = c("AD_prev", "AD_treat",  "AD_unavoid"))
    )
  ) + 
  geom_bar(stat = 'identity', position = position_fill(reverse = TRUE), colour = 'black', size = .1) +
  #    geom_text(aes(label=paste0(sprintf("%1.1f", count.value))))+
  coord_flip() +
  labs(x = '', y = '', fill = '') +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c('#fc8d59','#ffffbf',  '#99d594')) +
  #facet_grid(REGION_LABEL ~ SEX_LABEL, scales = 'free_y', space = 'free',  labeller = label_wrap_gen(width = 10, multi_line = TRUE)) +
  facet_wrap(~Scenario, nrow = 1, drop = TRUE, scales = 'free_y') +
  theme_minimal() +
  theme(
    legend.position = 'bottom',
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
    panel.grid.major = element_blank(),
    strip.text.y = element_text(angle = 0),
    text = element_text(size = 7)
  )
ggsave("CHARTS/bars/AD_barplot_6599.pdf", height=10, width=10)





