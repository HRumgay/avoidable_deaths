
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
  ggtitle("Overall preventable, treatable and total percentage avoidable deaths globally") +
  geom_bar(stat = "identity", 
           position = "dodge") +
  guides(fill=guide_legend(title="Proportion Avoidable Deaths Type"))
  #coord_flip() 

Figure_1_percentAD

ggsave("Figure_1_percentAD.pdf",width = 40, height = 30, pointsize = 12) 



# Figure 2 - Barplot by HDI

# Figure 3 - Maps (See other three map files)

# Figure 4 


# Figure 4 - Global burden of cancer by cancer site pie chart 

AD_by_cancer_site_3<-AD_cancer2%>%
  select(cancer, cancer_code, pAD_treat)%>%
  rename("AD"="pAD_treat")%>%
  mutate(AD_cat="Treatable")

AD_by_cancer_site_2<-AD_cancer2%>%
  select(cancer, cancer_code, pAD_prev)%>%
  rename("AD"="pAD_prev")%>%
  mutate(AD_cat="Preventable")

AD_by_cancer_site_1<-AD_cancer2%>%
  select(cancer, cancer_code, pAD_treat_prev)%>%
  rename("AD"="pAD_treat_prev")%>%
  mutate(AD_cat="Total Proportion Avoidable Deaths (Preventable + Treatable)")%>%
  full_join(AD_by_cancer_site_2)%>%
  full_join(AD_by_cancer_site_3)

# Pie Chart

library(RColorBrewer) 

getPalette = colorRampPalette(brewer.pal(9, "Set1"))
colourCount = 35

Figure_4_1 <- AD_by_cancer_site_1%>%
 # filter(AD_cat==)
  ungroup()%>%
  mutate(total2=sum(AD))%>%
  group_by(AD_cat)%>%
  mutate(total=sum(AD))%>%
  mutate(n = n())%>%
  mutate(wght = AD/sum(AD))%>%
  mutate(pos = (cumsum(wght)))%>%
  mutate(scale=total)%>%
  distinct()%>%
  arrange(desc(AD), .by_group = TRUE)%>%
  ggplot(aes(x=scale/2, y=wght, fill = cancer, width = scale)) +
  geom_col(color = 'black', 
           position = position_stack(reverse = TRUE), 
           show.legend = TRUE) +
  # geom_text_repel(aes(x = 1.4, y = pos, label = cancer), 
  #                 nudge_x = .3, 
  #                 segment.size = .7, 
  #                 show.legend = TRUE,max.overlaps = 42) +
  # scale_fill_discrete(name = "Type of avoidable deaths", 
  #                     labels = c("Preventable", "Treatable", "Unavoidable")) +
  coord_polar("y", start=0) +
  labs(title="Percentage preventable and treatable avoidable deaths globally by cancer site")+
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
  facet_wrap( ~ AD_cat,nrow=2) 

Figure_4_1

ggsave("Figure_1_Pie_Chart_cancer_site_scaled_percent.pdf",width = 30, height = 40, pointsize = 12) 



# alternative pie charts script using some elements from Bar_and_Pie_base.R script using globocan colours
pied <- AD_by_cancer_site_1

pied %>%
  filter(grptotaacases!=0, sex==0) %>%
  ggplot(aes(x = 2, y = percent, fill = factor(rank,levels = unique(pafd[sex==0 & grptotaacases!=0,]$rank), labels = unique(pafd[sex==0 & grptotaacases!=0,]$Color.Hex)))) +
  geom_bar(width = 1, stat = "identity") +
  geom_text(aes(label = paste0(formatC(round_any(grptotaacases,100), format="f", big.mark=" ", digits=0),"\n ",scales::percent(percent, accuracy = 1)), x = 2.75),
            position = position_stack(vjust=0.5),
            size=2) +
  coord_polar(theta = "y") +
  xlim(c(0.5,3))+
  scale_fill_identity("Cancer site", labels = pafd[sex==0 & grptotaacases!=0,]$cancer_label,
                      guide = "legend") +
  guides(fill = guide_legend(reverse = TRUE))+
  labs(title = "Both sexes")+
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
        strip.background = element_blank())


# Figure 5

