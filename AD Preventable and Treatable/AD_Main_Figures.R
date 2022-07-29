
# load libraries needed for some charts
library(plyr) #load this before dplyr/tidyverse otherwise it masks some dplyr functions
library(tidyverse)
library(data.table)
library(Rcan)
library(ggrepel)
library("ggplot2")
library(ggsci)


AD_Region
AD_by_HDI_all2
AD_country_all_cancers2
AD_cancer2
table_1_11 

# Figure 1 - Overall % avoidable deaths 

Figure_11<-table_1_11%>%select("AD_prev", "pAD_prev" )%>%
  rename("AD"="AD_prev")%>%
  rename("pAD"="pAD_prev")%>%
  mutate(AD_cat="Preventable")


Figure_12<-table_1_11%>%select("AD_treat", "pAD_treat" )%>%
  rename("AD"="AD_treat")%>%
  rename("pAD"="pAD_treat")%>%
  mutate(AD_cat="Treatable")


Figure_1<-table_1_11%>%select("AD_treat_prev", "pAD_treat_prev" )%>%
  rename("AD"="AD_treat_prev")%>%
  rename("pAD"="pAD_treat_prev")%>%
  mutate(AD_cat="Treatable + Preventable")%>%
  full_join(Figure_11)%>%
  full_join(Figure_12)

Figure_1_percentAD <- Figure_1  %>%
  group_by(AD_cat)%>%
  ungroup()%>%
  arrange(AD)%>%
  ggplot(
    aes(AD_cat, pAD,  
        ymin = 0,
        ymax = 1),
    mapping = aes(
      reorder(AD_cat, pAD),pAD,
      fill =AD_cat,drop=FALSE,na.rm = TRUE
    )) +
  xlab("Type of avoidable Deaths") +
  ylab("Percentage avoidable deaths (pAD)") +
 # scale_fill_manual(values = c('#de2d26','#fc9272',  '#fee0d2')) + #choose some nice colours for your bars
  ggtitle("Total") +
  geom_bar(stat = "identity", 
           position = "dodge") +
  guides(fill=guide_legend(title="Proportion Avoidable Deaths Type"))
  #coord_flip() 

Figure_1_percentAD

ggsave("Figure_1_percentAD.pdf",width = 40, height = 30, pointsize = 12) 



# Figure 2 - Barplot by HDI


# Pie Chart

AD_HDI_by_cancer_site_3<-AD_by_HDI %>%
  filter(age_cat=="Overall")%>%
  select(hdi_group,cancer, cancer_code, AD_treat)%>%
  rename("AD"="AD_treat")%>%
  mutate(AD_cat="Treatable")

AD_HDI_by_cancer_site_1<-AD_by_HDI %>%
  filter(age_cat=="Overall")%>%
  select(hdi_group,cancer, cancer_code, AD_prev)%>%
  rename("AD"="AD_prev")%>%
  mutate(AD_cat="Preventable")%>%
  mutate(hdi_group=as.numeric(hdi_group))

AD_HDI_by_cancer_site_1<-AD_by_HDI %>%
  filter(age_cat=="Overall")%>%
  select(hdi_group, cancer, cancer_code, AD_treat_prev)%>%
  mutate(hdi_group=as.numeric(hdi_group))%>%
  rename("AD"="AD_treat_prev")%>%
  mutate(AD_cat="Total")%>%
  ungroup()%>%
full_join(AD_HDI_by_cancer_site_2)%>%
 full_join(AD_HDI_by_cancer_site_3)



AD_HDI_by_cancer_site_11<-AD_HDI_by_cancer_site_1%>%
  group_by(hdi_group)%>%
  filter(AD_cat=="Total")%>% 
  slice_max(AD, n = 7)%>%
  select(cancer_code)%>%
  mutate(max="max")
  
  
getPalette = colorRampPalette(brewer.pal(9, "Set1"))
colourCount = 35

# 
# AD_levels<-names(sort(tapply( AD_HDI_by_cancer_site_1$AD,AD_HDI_by_cancer_site_1$cancer, sum)))
# 
# 
# # Examine the default factor order
# AD_by_cancer_site_1$AD_cat<-as.factor(AD_by_cancer_site_1$cancer)
# levels(AD_by_cancer_site_1$cancer)
# 
# ## Reorder fullname based on the the sum of the other columns
# AD_by_cancer_site_1$cancer <- reorder(AD_by_cancer_site_1$cancer, AD_by_cancer_site_1$AD)%>%group_by(AD_cat)
# 
# ## Examine the new factor order
# levels(AD_by_cancer_site_1$cancer)
# attributes(AD_by_cancer_site_1$AD_cat)
# 
# 
# dsfa<-AD_by_HDI %>%
#   ungroup()%>%
#   filter(age_cat=="Overall")%>%
#   mutate(sumAD=sum(AD_prev))
# 
# dsfa$sumAD

Figure_2 <- AD_HDI_by_cancer_site_1%>%
  left_join(AD_HDI_by_cancer_site_11)%>%
  filter(max=="max")%>%
  ungroup()%>%
#  group_by(hdi_group, AD_cat)%>%
  #slice_max(AD, n = 7)%>%
  distinct()%>%
  ungroup()%>%
#  group_by(hdi_group, cancer, AD_cat)%>%
  group_by(AD_cat,hdi_group)%>%
  arrange(-AD)%>%
  # mutate(AD_cat=factor(AD_cat,      # Reordering group factor levels
  #                      levels = c("Preventable","Treatable","Total")))%>%
  ggplot(aes(AD_cat, AD,
         ymin = 0,
         ymax = max(AD)),
    mapping = aes(x=cancer, y=AD, reorder(AD_cat, AD),
      fill=factor(cancer, levels = c("Preventable","Treatable","Total")), drop=FALSE,na.rm = TRUE)) +
  xlab("Type of avoidable deaths") +
  ylab("Number of avoidable deaths") +
  # scale_fill_manual(values = c('#de2d26','#fc9272',  '#fee0d2')) + #choose some nice colours for your bars
  ggtitle("Top seven cancer sites by number preventable, treatable and overall avoidable deaths by HDI group") +
  geom_col(aes(fill = hdi_group), width = 0.7)+
  guides(fill=guide_legend(title="HDI group"))+
  #options( scipen = 999 )++
  theme_bw() + scale_color_lancet()+
 # scale_fill_manual(values = colorRampPalette(brewer.pal(8, "Set1"))(colourCount))+
  facet_wrap(~ AD_cat,nrow=2,scales = "free_x")+
  coord_flip() 


Figure_2

# Figure 3 - Maps (See other three map files)

# Figure 4 


# Figure 4 - Global burden of cancer by cancer site pie chart 

AD_by_cancer_site_3<-AD_cancer2%>%
  select(cancer, cancer_code, AD_treat)%>%
  rename("AD"="AD_treat")%>%
  mutate(AD_cat="Treatable")

AD_by_cancer_site_2<-AD_cancer2%>%
  select(cancer, cancer_code, AD_prev)%>%
  rename("AD"="AD_prev")%>%
  mutate(AD_cat="Preventable")

AD_by_cancer_site_1<-AD_cancer2%>%
  select(cancer, cancer_code, AD_treat_prev)%>%
  rename("AD"="AD_treat_prev")%>%
  mutate(AD_cat="Total")%>%
  full_join(AD_by_cancer_site_2)%>%
  full_join(AD_by_cancer_site_3)

# Pie Chart

library(RColorBrewer) 

getPalette = colorRampPalette(brewer.pal(9, "Set1"))
colourCount = 35

#scale_fill_manual(values = colorRampPalette(brewer.pal(8, "Set2"))(colourCount))+


Figure_4_1 <- AD_by_cancer_site_1%>%
 # filter(AD_cat==)
  select(-cancer_code)%>%
  ungroup()%>%
  mutate(total2=sum(AD))%>%
  group_by(AD_cat)%>%
  arrange(AD_cat,-AD)%>%
  
  
  mutate(posit = 1:n())%>%
  mutate(cancer=case_when(posit <=7 ~ cancer,
                          posit>7 ~ "All other sites"
                          ))%>%
  select(-posit)%>%
  group_by(AD_cat,cancer)%>%
  mutate(AD=sum(AD))%>%
  distinct()%>%
  ungroup%>%
  group_by(AD_cat)%>%
  mutate(total=sum(AD))%>%
  mutate(n = n())%>%
  mutate(wght = AD/total)%>%
  mutate(pos = (cumsum(wght)))%>%
  mutate(scale=total/total2)%>%
  distinct()%>%
  ungroup()%>%
  mutate(AD_cat=factor(AD_cat,      # Reordering group factor levels
                levels = c("Preventable","Treatable","Total")))%>%
  ggplot(aes(x=scale/2, y=(wght), fill = cancer, width = scale)) +
  geom_col(color = 'black', 
           position = position_stack(reverse = TRUE), 
           show.legend = TRUE) +
  # geom_text_repel(aes(x = 1.4*scale, y = pos*wght, label = AD),
  #                 nudge_x = .3,
  #                 segment.size = .7,
  #                 show.legend = TRUE,max.overlaps = 42) +
  coord_polar("y", start=0) +
  labs(title="Top seven cancer sites by number preventable, treatable and overall avoidable deaths globally")+
  theme_void()+
  # theme(legend.key.width= unit(2.6, "cm"), 
  #       legend.key.height= unit(1.4, "cm"),
  #       legend.direction= "vertical",
  #       legend.text = element_text(size=24),
  #       legend.title = element_text(size=24, hjust = 1),
  #       legend.title.align=0.5,
  #       legend.position =c(0.3, -0.02),
  #       legend.background = element_rect(fill="transparent"),
  #       plot.margin = unit(c(0,0,0,0),"lines"))+
  guides(fill=guide_legend(title="Cancer Type"))+
  scale_fill_manual(values = colorRampPalette(brewer.pal(8, "Set1"))(colourCount))+
 # scale_fill_lancet()+
  facet_wrap( ~ AD_cat,nrow=2)

Figure_4_1

ggsave("Figure_1_Pie_Chart_cancer_site_scaled_percent.pdf",width = 30, height = 40, pointsize = 12) 



# pie charts ----
# alternative pie charts script using some elements from Bar_and_Pie_base.R script using globocan colours
col <- read.csv("cancer_color_2018.csv", stringsAsFactors = F)

Avoidable_Deaths_Simulated_All %>% 
  group_by(cancer) %>% 
  mutate(AD_treat_prev=sum(AD_treat_not_prev,AD_prev,na.rm=T),
         AD_prev=sum(AD_prev,na.rm=T),
         AD_treat=sum(AD_treat,na.rm=T),
         Expect_deaths=sum(Expect_deaths,na.rm=T)) %>% 
  filter(!is.na(cancer_code)) %>% 
  select(cancer,cancer_code,AD_prev,AD_treat,AD_treat_prev,Expect_deaths)%>% 
  unique() %>% 
  left_join(col %>% select(cancer_label:Color.Hex) %>% filter(cancer_label!="Colorectum")) %>% 
  pivot_longer(AD_prev:Expect_deaths,
               names_to="AD_cat",
               values_to = "AD") %>%
  group_by(AD_cat) %>% 
  mutate(percent=sum(AD),
         percent = AD/percent) %>% 
  dplyr::arrange(AD) %>% 
  mutate(rankc = as.numeric(dplyr::row_number())) %>% 
  group_by(AD_cat) %>% 
  mutate(AD=case_when(rankc<32~sum(AD[rankc<32]),
                      TRUE~AD),
         percent=case_when(rankc<32~sum(percent[rankc<32]),
                           TRUE~percent), 
         rankc=case_when(rankc<32~1,
                         TRUE~rankc),
         Color.Hex=case_when(rankc==1~"#DCDCDC",
                             TRUE~Color.Hex),
         cancer=case_when(rankc==1~"Other sites",
                          TRUE~cancer)) %>% 
  select(cancer,Color.Hex,AD_cat,AD,percent,rankc) %>% 
  unique() -> AD_by_cancer_site_1

Avoidable_Deaths_Simulated_All %>%
  filter(!is.na(cancer_code)) %>%
  group_by(cancer,cancer_code,hdi_group) %>% 
  mutate(AD_treat_prev=sum(AD_treat_not_prev,AD_prev,na.rm=T),
         AD_prev=sum(AD_prev,na.rm=T),
         AD_treat=sum(AD_treat,na.rm=T),
         Expect_deaths=sum(Expect_deaths,na.rm=T)) %>% 
  select(hdi_group, cancer,cancer_code,AD_prev,AD_treat,AD_treat_prev,Expect_deaths)%>% 
  unique() %>% 
  left_join(col%>% select(cancer_label:Color.Hex)%>% filter(cancer_label!="Colorectum")) %>% 
  pivot_longer(AD_prev:Expect_deaths,
               names_to="AD_cat",
               values_to = "AD") %>%
  group_by(AD_cat,hdi_group) %>%
  mutate(percent=sum(AD),
         percent = AD/percent) %>% 
  arrange(AD) %>% 
  mutate(rankc = as.numeric(dplyr::row_number())) %>% 
  group_by(AD_cat, hdi_group) %>% 
  mutate(AD=case_when(rankc<32~sum(AD[rankc<32]),
                      TRUE~AD),
         percent=case_when(rankc<32~sum(percent[rankc<32]),
                           TRUE~percent), 
         rankc=case_when(rankc<32~1,
                         TRUE~rankc),
         Color.Hex=case_when(rankc==1~"#DCDCDC",
                             TRUE~Color.Hex),
         cancer=case_when(rankc==1~"Other sites",
                          TRUE~cancer)) %>% 
  select(hdi_group, cancer,Color.Hex,AD_cat,AD,percent,rankc) %>% 
  unique() -> AD_by_cancer_site_1_HDI

#preventable pie
pied <- as.data.table(AD_by_cancer_site_1 %>%filter(AD_cat=="AD_prev") )
pied %>%
  ggplot(aes(x = 2, y = percent, fill = factor(rankc,levels = unique(pied$rankc),
                                               labels = unique(pied$Color.Hex)))) +
  geom_bar(width = 1, stat = "identity") +
  geom_text(aes(label = paste0(formatC(round_any(AD,100), format="f", big.mark=",", digits=0),"\n ", 
                               scales::percent(percent, accuracy = 1)), x = 2.75),
            position = position_stack(vjust=0.5),
            size=2) +
  coord_polar(theta = "y") +
  #xlim(c(0.5,3))+  # to make into donut
  scale_fill_identity("Cancer site", labels = pied$cancer,
                      guide = "legend") +
  guides(fill = guide_legend(reverse = TRUE))+
  #labs(title = "Preventable deaths")+
  theme(plot.background= element_blank(),
        #plot.title = element_text(size=12, margin=margin(0,0,0,0),hjust = 0.5),
        panel.background = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_blank()) -> pie.prev
pie.prev
ggsave("pie.prev.pdf",pie.prev,width=5.43 ,height=2.43)

#treatable pie
pied <- as.data.table(AD_by_cancer_site_1 %>%filter(AD_cat=="AD_treat") )
pied %>%
  ggplot(aes(x = 2, y = percent, fill = factor(rankc,levels = unique(pied$rankc),
                                               labels = unique(pied$Color.Hex)))) +
  geom_bar(width = 1, stat = "identity") +
  geom_text(aes(label = paste0(formatC(round_any(AD,100), format="f", big.mark=",", digits=0),"\n ", 
                               scales::percent(percent, accuracy = 1)), x = 2.75),
            position = position_stack(vjust=0.5),
            size=2) +
  coord_polar(theta = "y") +
  #xlim(c(0.5,3))+   # to make into donut
  scale_fill_identity("Cancer site", labels = pied$cancer,
                      guide = "legend") +
  guides(fill = guide_legend(reverse = TRUE))+
  #labs(title = "Treatable deaths")+
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
        strip.background = element_blank()) -> pie.treat
pie.treat
ggsave("pie.treat.pdf",pie.treat,width=5.43 ,height=2.43)

#avoidable pie
pied <- as.data.table(AD_by_cancer_site_1 %>%filter(AD_cat=="AD_treat_prev") )
pied %>%
  ggplot(aes(x = 2, y = percent, fill = factor(rankc,levels = unique(pied$rankc),
                                               labels = unique(pied$Color.Hex)))) +
  geom_bar(width = 1, stat = "identity") +
  geom_text(aes(label = paste0(formatC(round_any(AD,100), format="f", big.mark=",", digits=0),"\n ", 
                               scales::percent(percent, accuracy = 1)), x = 2.75),
            position = position_stack(vjust=0.5),
            size=2) +
  coord_polar(theta = "y") +
  #xlim(c(0.5,3))+    # to make into donut
  scale_fill_identity("Cancer site", labels = pied$cancer,
                      guide = "legend") +
  guides(fill = guide_legend(reverse = TRUE))+
  #labs(title = "Avoidable deaths")+
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
        strip.background = element_blank()) -> pie.avoid
pie.avoid
ggsave("pie.avoid.pdf",pie.avoid,width=5.43 ,height=2.43)


#avoidable pie for HDI group 1
pied <- as.data.table(AD_by_cancer_site_1_HDI %>%filter(AD_cat=="AD_treat_prev",hdi_group==1) )
pied %>%
  ggplot(aes(x = 2, y = percent, fill = factor(rankc,levels = unique(pied$rankc),
                                               labels = unique(pied$Color.Hex)))) +
  geom_bar(width = 1, stat = "identity") +
  geom_text(aes(label = paste0(formatC(round_any(AD,100), format="f", big.mark=",", digits=0),"\n ", 
                               scales::percent(percent, accuracy = 1)), x = 2.75),
            position = position_stack(vjust=0.5),
            size=2) +
  coord_polar(theta = "y") +
  #xlim(c(0.5,3))+    # to make into donut
  scale_fill_identity("Cancer site", labels = pied$cancer,
                      guide = "legend") +
  guides(fill = guide_legend(reverse = TRUE))+
  labs(title = "Low HDI")+
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
        strip.background = element_blank()) -> pie.avoid.hdi1
pie.avoid.hdi1
ggsave("pie.avoid.hdi1.pdf",pie.avoid.hdi1,width=5.43 ,height=2.43)


pied <- as.data.table(AD_by_cancer_site_1_HDI %>%filter(AD_cat=="AD_treat_prev",hdi_group==2) )
pied %>%
  ggplot(aes(x = 2, y = percent, fill = factor(rankc,levels = unique(pied$rankc),
                                               labels = unique(pied$Color.Hex)))) +
  geom_bar(width = 1, stat = "identity") +
  geom_text(aes(label = paste0(formatC(round_any(AD,100), format="f", big.mark=",", digits=0),"\n ", 
                               scales::percent(percent, accuracy = 1)), x = 2.75),
            position = position_stack(vjust=0.5),
            size=2) +
  coord_polar(theta = "y") +
  #xlim(c(0.5,3))+    # to make into donut
  scale_fill_identity("Cancer site", labels = pied$cancer,
                      guide = "legend") +
  guides(fill = guide_legend(reverse = TRUE))+
  labs(title = "Medium HDI")+
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
        strip.background = element_blank()) -> pie.avoid.hdi2
pie.avoid.hdi2
ggsave("pie.avoid.hdi2.pdf",pie.avoid.hdi2,width=5.43 ,height=2.43)

pied <- as.data.table(AD_by_cancer_site_1_HDI %>%filter(AD_cat=="AD_treat_prev",hdi_group==3) )
pied %>%
  ggplot(aes(x = 2, y = percent, fill = factor(rankc,levels = unique(pied$rankc),
                                               labels = unique(pied$Color.Hex)))) +
  geom_bar(width = 1, stat = "identity") +
  geom_text(aes(label = paste0(formatC(round_any(AD,100), format="f", big.mark=",", digits=0),"\n ", 
                               scales::percent(percent, accuracy = 1)), x = 2.75),
            position = position_stack(vjust=0.5),
            size=2) +
  coord_polar(theta = "y") +
  #xlim(c(0.5,3))+    # to make into donut
  scale_fill_identity("Cancer site", labels = pied$cancer,
                      guide = "legend") +
  guides(fill = guide_legend(reverse = TRUE))+
  labs(title = "High HDI")+
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
        strip.background = element_blank()) -> pie.avoid.hdi3
pie.avoid.hdi3
ggsave("pie.avoid.hdi3.pdf",pie.avoid.hdi3,width=5.43 ,height=2.43)


pied <- as.data.table(AD_by_cancer_site_1_HDI %>%filter(AD_cat=="AD_treat_prev",hdi_group==4) )
pied %>%
  ggplot(aes(x = 2, y = percent, fill = factor(rankc,levels = unique(pied$rankc),
                                               labels = unique(pied$Color.Hex)))) +
  geom_bar(width = 1, stat = "identity") +
  geom_text(aes(label = paste0(formatC(round_any(AD,100), format="f", big.mark=",", digits=0),"\n ", 
                               scales::percent(percent, accuracy = 1)), x = 2.75),
            position = position_stack(vjust=0.5),
            size=2) +
  coord_polar(theta = "y") +
  #xlim(c(0.5,3))+    # to make into donut
  scale_fill_identity("Cancer site", labels = pied$cancer,
                      guide = "legend") +
  guides(fill = guide_legend(reverse = TRUE))+
  labs(title = "Very High HDI")+
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
        strip.background = element_blank()) -> pie.avoid.hdi4
pie.avoid.hdi4
ggsave("pie.avoid.hdi4.pdf",pie.avoid.hdi4,width=5.43 ,height=2.43)

# Figure 5

