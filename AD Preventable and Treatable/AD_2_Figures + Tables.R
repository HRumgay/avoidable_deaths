
# load libraries needed for some charts
library(tidyverse)
library(data.table)
library(Rcan)



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


#Creating a plotable object comparing Thailand simulated and RWD

Avoidable_Deaths2 <- Avoidable_Deaths_age_cat %>% mutate(Scenario = "RWD")


Avoidable_Deaths_Simulated2 <-
  Avoidable_Deaths_modelled_age_cat %>% mutate(Scenario = "Simulated")

Cancer_codes2<-
  cancer_codes %>%select(cancer_code)

AD_plotable <-  Avoidable_Deaths2 %>%
  ungroup()%>%
  select(-cancer_label)%>%
  rename("total_overall"="total")%>%
  full_join(Avoidable_Deaths_Simulated2)%>%
  mutate(cancer_code=as.integer(cancer_code))%>%
  mutate_if(is.numeric, replace_na, 0)%>%
 left_join(ten_cancer_sites, by=c("cancer_code"))%>%
  filter(!is.na(AD_treat))%>%
  mutate(AD_treat_prop=AD_treat/AD_sum*100)%>%
  mutate(AD_prev_prop=AD_prev/AD_sum*100)%>%
  mutate(AD_unavoid_prop=AD_unavoid/AD_sum*100)

#Data by cancer site prep
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

Figure_4_1 <- AD_by_cancer_site_1%>%
  ungroup()%>%
  mutate(wght = runif(length(AD)))%>%
  mutate(wght = wght/sum(wght))%>%
  mutate(pos = (cumsum(c(0, wght)) + c(wght / 2, .01))[1:nrow(AD_by_cancer_site_1)])%>%
  group_by(AD_cat)%>%
  arrange(desc(AD), .by_group = TRUE) %>%
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

Figure_4_1

Figure_4_2 <- AD_by_cancer_site_1%>%
  group_by(AD_cat)%>%
  summarize(AD=sum(AD))%>%
  distinct()%>%
  ungroup()%>%
  mutate(wght = runif(length(AD)))%>%
  mutate(wght = wght/sum(wght))%>%
  mutate(pos = (cumsum(c(0, wght)) + c(wght / 2, .01))[1:3])%>%
  group_by(AD_cat)%>%
  arrange(desc(AD), .by_group = TRUE) %>%
  
  ggplot(aes(x="", y=pos, fill = AD_cat)) +
  geom_col(color = 'black', 
           position = position_stack(reverse = TRUE), 
           show.legend = TRUE) +
  geom_text_repel(aes(x = 1.4, y = wght, label = AD_cat), 
                  nudge_x = .3, 
                  segment.size = .7, 
                  show.legend = TRUE) +
  scale_fill_discrete(name = "Type of avoidable deaths", 
                      labels = c("Preventable", "Treatable", "Unavoidable")) +
  coord_polar("y", start=0) +
  labs(title="Preventable and treatable avoidable deaths globally for ten cancer sites")+
  theme_void()



Figure_4_2


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
  scale_fill_manual(values = c('#de2d26','#fc9272',  '#fee0d2')) + #choose some nice colours for your bars
  ggtitle("AD due to treatment") +
  geom_bar(stat = "identity", 
           position = "dodge") +
  coord_flip() +
  # geom_errorbar(
  #   aes(x = cancer_label, ymin = AD_treat_Lower, ymax = AD_treat_Upper),
  #   width = 0.4,
  #   alpha = 0.9,
  #   size = 1.3
  # ) +
facet_grid(. ~ Scenario,drop=FALSE,scales = "free_y")




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
  scale_fill_manual(values = c('#de2d26','#fc9272',  '#fee0d2')) + #choose some nice colours for your bars
  geom_bar(stat = "identity", 
           position = "dodge") +
  coord_flip() +
  # geom_errorbar(
  #   aes(x = cancer_label, ymin = AD_treat_Lower, ymax = AD_treat_Upper),
  #   width = 0.4,
  #   alpha = 0.9,
  #   size = 1.3
  # ) +
  facet_grid(. ~ Scenario,drop=FALSE,scales = "free_y")

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
  scale_fill_manual(values = c('#de2d26','#fc9272',  '#fee0d2')) + #choose some nice colours for your bars
  ggtitle("AD Unavoidable") +
  geom_bar(stat = "identity",
           position = "dodge") +
  coord_flip() +
  # geom_errorbar(
  #   aes(x = cancer_label, ymin = AD_treat_Lower, ymax = AD_treat_Upper),
  #   width = 0.4,
  #   alpha = 0.9,
  #   size = 1.3
  # ) +
  facet_grid(. ~ Scenario,drop=FALSE,scales = "free_y")


AD_barplot_treat_prop <- AD_plotable %>%
  group_by(cancer_label)%>%
  ggplot(
    aes(cancer_label, AD_treat_prop,  
        ymin = min(AD_treat_prop),
        ymax = max(AD_treat_prop)),
    mapping = aes(
      reorder(cancer_label, AD_treat_prop),AD_treat_prop,
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
  #   aes(x = cancer_label, ymin = AD_treat_prop_Lower, ymax = AD_treat_prop_Upper),
  #   width = 0.4,
  #   alpha = 0.9,
  #   size = 1.3
  # ) +
  facet_grid(. ~ Scenario,drop=FALSE,scales = "free_y")




AD_barplot_prev_prop <- AD_plotable %>%
  group_by(cancer_label)%>%
  ggplot(
    aes(cancer_label, AD_prev_prop,  
        ymin = min(AD_prev_prop),
        ymax = max(AD_prev_prop)),
    mapping = aes(
      reorder(cancer_label, AD_prev_prop),AD_prev_prop,
      fill = age_cat,drop=FALSE
    )
  ) +
  xlab("Cancer Site") +
  ylab("AD") +
  ggtitle("AD Preventable due to Risk Factors  (%)") +
  scale_fill_manual(values = c('#de2d26','#fc9272',  '#fee0d2')) + #choose some nice colours for your bars
  geom_bar(stat = "identity", 
           position = "dodge") +
  coord_flip() +
  # geom_errorbar(
  #   aes(x = cancer_label, ymin = AD_treat_prop_Lower, ymax = AD_treat_prop_Upper),
  #   width = 0.4,
  #   alpha = 0.9,
  #   size = 1.3
  # ) +
  facet_grid(. ~ Scenario,drop=FALSE,scales = "free_y")

AD_barplot_unavoid_prop <- AD_plotable %>%
  group_by(cancer_label)%>%
  ggplot(
    aes(cancer_label, AD_unavoid_prop,  
        ymin = min(AD_unavoid_prop),
        ymax = max(AD_unavoid_prop)),
    mapping = aes(
      reorder(cancer_label, AD_unavoid_prop),AD_unavoid_prop,
      fill = age_cat,drop=FALSE
    )
  ) +
  xlab("Cancer Site") +
  ylab("AD") +
  scale_fill_manual(values = c('#de2d26','#fc9272',  '#fee0d2')) + #choose some nice colours for your bars
  ggtitle("AD Unavoidable (%)") +
  geom_bar(stat = "identity",
           position = "dodge") +
  coord_flip() +
  # geom_errorbar(
  #   aes(x = cancer_label, ymin = AD_treat_prop_Lower, ymax = AD_treat_prop_Upper),
  #   width = 0.4,
  #   alpha = 0.9,
  #   size = 1.3
  # ) +
  facet_grid(. ~ Scenario,drop=FALSE,scales = "free_y")


#Calling the plots 

AD_barplot_treat

AD_barplot_prev

AD_barplot_unavoid

AD_barplot_treat_prop

AD_barplot_prev_prop

AD_barplot_unavoid_prop


ggsave("~/Documents/R Figures/AD_barplot_treat.png",AD_barplot_treat, height=10, width=10)
ggsave("~/Documents/R Figures/AD_barplot_prev.png",AD_barplot_prev, height=10, width=10)
ggsave("~/Documents/R Figures/AD_barplot_unavoid.png",AD_barplot_unavoid, height=10, width=10)
ggsave("~/Documents/R Figures/AD_barplot_treat_prop.png",AD_barplot_treat, height=10, width=10)
ggsave("~/Documents/R Figures/AD_barplot_prev_prop.png",AD_barplot_prev, height=10, width=10)
ggsave("~/Documents/R Figures/AD_barplot_unavoid_prop.png",AD_barplot_unavoid, height=10, width=10)

# By HDI and region



AD_by_HDI_3<-AD_by_HDI%>%
  select(age_cat, hdi_group, cancer, cancer_code, AD_treat)%>%
  rename("AD"="AD_treat")%>%
  mutate(AD_cat="Treatable")

AD_by_HDI_2<-AD_by_HDI%>%
  select(age_cat, hdi_group, cancer, cancer_code, AD_prev)%>%
  rename("AD"="AD_prev")%>%
  mutate(AD_cat="Preventable")

AD_by_HDI_1<-AD_by_HDI%>%
  select(age_cat, hdi_group, cancer, cancer_code, AD_unavoid)%>%
  rename("AD"="AD_unavoid")%>%
  mutate(AD_cat="Unavoidable")%>%
  full_join(AD_by_HDI_2)%>%
  full_join(AD_by_HDI_3)



AD_by_HDI_Plot<- AD_by_HDI_1 %>%
  as.data.frame()%>%
  #mutate(hdi_group = replace_na(hdi_group, "NA"))%>%
  group_by(cancer,hdi_group)
  ggplot(
    aes(cancer, AD,
        ymin = min(AD),
        ymax = max(AD)),
    mapping = aes(
      reorder(cancer, AD),AD,
      fill = age_cat,drop=FALSE
    )
  ) +
  geom_bar(stat = 'identity', colour = 'black', size = .1)
  xlab("HDI group") +
  ylab("AD") +
  scale_fill_manual(values = c('#de2d26','#fc9272',  '#fee0d2')) + #choose some nice colours for your bars
  ggtitle("AD by HDI group") +
  geom_bar(stat = "identity",
           position = "dodge") +
  coord_flip() +
  # geom_errorbar(
  #   aes(x = cancer, ymin = AD_treat_Lower, ymax = AD_treat_Upper),
  #   width = 0.4,
  #   alpha = 0.9,
  #   size = 1.3
  # ) +
  facet_grid(. ~ hdi_group, drop=FALSE,scales = "free_y")

  AD_by_HDI_overall_plot<-  AD_by_HDI_overall %>%
    as.data.frame()%>%
    #mutate(hdi_group = replace_na(hdi_group, "NA"))%>%
    group_by(cancer,hdi_group)
  ggplot(
    aes(cancer, AD_treat,
        ymin = min(AD_treat),
        ymax = max(AD_treat)),
    mapping = aes(
      reorder(cancer, AD_treat),AD_treat,
      fill = age_cat,drop=FALSE
    )
  ) +
    geom_bar(stat = 'identity', colour = 'black', size = .1)
  xlab("HDI group") +
    ylab("AD") +
    scale_fill_manual(values = c('#de2d26','#fc9272',  '#fee0d2')) + #choose some nice colours for your bars
    ggtitle("AD by HDI group") +
    geom_bar(stat = "identity",
             position = "dodge") +
    coord_flip() +
    # geom_errorbar(
    #   aes(x = cancer, ymin = AD_treat_Lower, ymax = AD_treat_Upper),
    #   width = 0.4,
    #   alpha = 0.9,
    #   size = 1.3
    # ) +
    facet_grid(. ~ hdi_group, drop=FALSE,scales = "free_y")
  
  
    
    
  AD_by_HDI_ten_plot<-  AD_by_HDI_ten








#--- code for horizontal two-sided stacked bar charts----
#convert data to long format
AD_plotable %>% 
  select(-c(AD_treat_Lower, AD_treat_Upper,
            AD_prev_Lower,AD_prev_Upper
            ))%>%
  pivot_longer(cols = starts_with("AD_"),
               names_to = "type",
               values_to = "deaths") -> AD_plotable_L


AD_plotable_L %>% 
  filter(age_cat=="Overall", type %in% c("AD_treat", "AD_prev", "AD_unavoid")) %>% 
  ggplot(
    aes(
      x = cancer,
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


# create stacked bar for age 15-64 
AD_plotable_L %>% 
  filter(age_cat=="15-64", type %in% c("AD_treat", "AD_prev", "AD_unavoid")) %>% 
  ggplot(
    aes(
      x = cancer,
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
ggsave("~/Documents/R Figures/AD_barplot_1564.pdf", height=10, width=10)

# create stacked bar for age 65-99
AD_plotable_L %>% 
  filter(age_cat=="65-99", type %in% c("AD_treat", "AD_prev", "AD_unavoid")) %>% 
  ggplot(
    aes(
      x = cancer,
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
ggsave("~/Documents/R Figures/AD_barplot_6599.pdf", height=10, width=10)

# create stacked bar for age Overall
AD_plotable_L %>% 
  filter(age_cat=="Overall", type %in% c("AD_treat", "AD_prev", "AD_unavoid")) %>% 
  ggplot(
    aes(
      x = cancer,
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
ggsave("~/Documents/R Figures/AD_barplot_Overall.pdf", height=10, width=10)





#--- example code for horizontal two-sided stacked bar charts ----
#make sure data are in long format for stacked bars
#in this code the variable 'level' is being stacked, so could change this to scenario (prev, treat, unavoid) or age?
#you could also make this by cancer site instead of by world region

# produce horizontal bars split males and females by world region
# create dual bar chart, males on left, females on right
pafsplit_subregion2 %>% 
  mutate(pafplot = case_when(sex == 1 ~ grptotpaf * -1,  #create negative values to plot left of y axis
                             sex == 2 ~ grptotpaf)) -> pafsplit_subregion2t
pafsplit_subregion2t <- as.data.table(pafsplit_subregion2t)

#order world regions by proportion or ASR
dt_label_order3 <- setkeyv(unique(pafs_subregion2[cancer == "All cancers but non-melanoma skin cancer_M" & sex==1, c("subregion_label_2", "grptotpaf"), with=FALSE]), c("grptotpaf"))
pafsplit_subregion2t$subregion_label_2 <- factor(pafsplit_subregion2t$subregion_label_2, levels = unique(dt_label_order3$subregion_label_2,fromLast=TRUE))

#remove unnecessary rows
pafsplit_subregion2t %>% 
  filter(sex!=0) %>% 
  dplyr::select(-age_num, -grpaacasesage, -grppyage) %>% unique() %>% 
  filter(cancer=="All cancers but non-melanoma skin cancer_M", level<4) -> pafsplit_subregion2t

#create lists of labels for axes
tick_minor_list <- Rcan:::core.csu_tick_generator(max = max(pafs_subregion2[cancer == "All cancers but non-melanoma skin cancer_M" & sex==1]$grptotpaf, na.rm = TRUE ), 0)$tick_list
nb_tick <- length(tick_minor_list) 
tick_space <- tick_minor_list[nb_tick] - tick_minor_list[nb_tick-1]
if ((tick_minor_list[nb_tick] -  max(pafs_subregion2[cancer == "All cancers but non-melanoma skin cancer_M" & sex==1]$grptotpaf))/tick_space < 1/4){
  tick_minor_list[nb_tick+1] <- tick_minor_list[nb_tick] + tick_space
}
tick_major <- tick_minor_list[1:length(tick_minor_list) %% 2  == 1]
tick_major_list <- c(rev(-tick_major),tick_major[tick_major!=0])
tick_label <- c(rev(tick_major),tick_major[tick_major!=0])
tick_minor_list <- c(rev(-tick_minor_list),tick_minor_list[tick_minor_list!=0])

#create two-sided stacked bar by world region and sex
pafsplit_subregion2t %>% 
  mutate(sex = case_when(sex==1 ~ "Male", sex==2~"Female"),
         level = case_when(level==1 ~ "<20",level==2 ~ "20-60", level==3~">60")) %>% 
  ggplot(aes(x=subregion_label_2, y=pafplot, fill=factor(level, levels = c(">60","20-60","<20"))))+ 
  geom_bar(stat = 'identity', colour = 'black', size = .1)+
  geom_hline(yintercept = 0, size=1)+
  coord_flip(ylim = c(tick_minor_list[1]-(tick_space*0.25),tick_minor_list[length(tick_minor_list)]+(tick_space*0.25)), expand = FALSE)+
  scale_fill_manual(values = c('#de2d26','#fc9272',  '#fee0d2')) + #choose some nice colours for your bars
  labs(x = 'World Region', y = 'Population Attributable Fraction (%)', fill = 'Consumption level\n         (g/day)') +
  scale_y_continuous(breaks=tick_major_list,
                     minor_breaks = tick_minor_list,
                     labels=scales::percent(tick_label, accuracy=1))+
  theme_minimal()+
  theme(legend.position = 'bottom', axis.text.x=element_text(angle=90,hjust=1, vjust = .5)) + 
  guides(fill=guide_legend(reverse = TRUE))
ggsave("levels_subregion2_mf_horiz.pdf", width = 15, height = 10)

