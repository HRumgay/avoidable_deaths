
# load libraries needed for some charts
library(tidyverse)
library(data.table)
library(Rcan)
library(ggrepel)




AD_Region
AD_by_HDI_all2
AD_country_all_cancers2
AD_cancer2
table_1_11 



# Figure 1 - 

# Figure 2 - 

# Figure 4 - Global burden of cancer by cancer site pie chart 

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

# Pie Chart

Figure_4_1 <- AD_by_cancer_site_1%>%
  filter(AD_cat==)
  ungroup()%>%
  mutate(wght = runif(length(AD)))%>%
  mutate(wght = wght/sum(wght))%>%
  mutate(pos = (cumsum(c(0, wght)) + c(wght / 2, .01))[1:nrow(AD_by_cancer_site_1)])%>%
  group_by(AD_cat)%>%
  distinct()
  arrange(desc(AD), .by_group = TRUE) %>%
  ggplot(aes(x="", y=wght, fill = AD_cat)) +
  geom_col(color = 'black', 
           position = position_stack(reverse = TRUE), 
           show.legend = TRUE) +
  geom_text_repel(aes(x = 1.4, y = pos, label = cancer), 
                  nudge_x = .3, 
                  segment.size = .7, 
                  show.legend = TRUE,max.overlaps = 42) +
  scale_fill_discrete(name = "Type of avoidable deaths", 
                      labels = c("Preventable", "Treatable", "Unavoidable")) +
  coord_polar("y", start=0) +
  labs(title="Preventable and treatable avoidable deaths globally by cancer site")+
  theme_void()

Figure_4_1

# Barplot

Figure_4_1
