
# load libraries needed for some charts
library(plyr) #added plyr here for rounding for the pie charts but it might break some tidyverse functions
library(tidyverse)
library(data.table)
library(Rcan)
library(ggrepel)
library("ggplot2")
library(ggsci)
library(grid)


setwd("C:\\Users\\langseliuso\\OneDrive - International Agency for Research on Cancer\\Documents\\GitHub\\avoidable_deaths\\Gastric_cancer")


text_high <- textGrob("Highest\nvalue", gp=gpar(fontsize=13, fontface="bold"))
text_low <- textGrob("Lowest\nvalue", gp=gpar(fontsize=13, fontface="bold"))

AD_Region2
AD_by_HDI_all2
AD_country_all_cancers
AD_cancer2
table_1_11 



# Pie Charts

AD_by_HDI <-AD_by_HDI%>%
  filter(sex==0)%>%
  dplyr::select(-sex)


AD_HDI_by_cancer_site_3<-AD_by_HDI %>%
  filter(age_cat=="Overall")%>%
  dplyr::select(hdi_group,cancer, cancer_code, AD_treat)%>%
  dplyr::rename("AD"="AD_treat")%>%
  dplyr::mutate(AD_cat="Treatable")

AD_HDI_by_cancer_site_2<-AD_by_HDI %>%
  filter(age_cat=="Overall")%>%
  dplyr::select(hdi_group,cancer, cancer_code, AD_prev)%>%
  dplyr::rename("AD"="AD_prev")%>%
  dplyr::mutate(AD_cat="Preventable")%>%
  dplyr::mutate(hdi_group=as.numeric(hdi_group))

AD_HDI_by_cancer_site_1<-AD_by_HDI %>%
  filter(age_cat=="Overall")%>%
  dplyr::select(hdi_group, cancer, cancer_code, AD_treat_prev)%>%
  dplyr::mutate(hdi_group=as.numeric(hdi_group))%>%
  dplyr::rename("AD"="AD_treat_prev")%>%
  dplyr::mutate(AD_cat="Total")%>%
  ungroup()%>% 
  full_join(AD_HDI_by_cancer_site_2)%>%
  full_join(AD_HDI_by_cancer_site_3)



AD_HDI_by_cancer_site_11<-AD_HDI_by_cancer_site_1%>%
  group_by(hdi_group)%>%
  filter(AD_cat=="Total")%>% 
  slice_max(AD, n = 5)%>%
  dplyr::select(cancer_code)%>%
  dplyr::mutate(max="max")


# getPalette = colorRampPalette(brewer.pal(9, "Set1"))
# colourCount = 35

AD_by_HDI_all2<-AD_by_HDI_all%>%
  dplyr::rename("hdi_group"="country_code")%>%
  mutate(hdi_group=case_when(hdi_group==984~1,
                             hdi_group==983~3,
                             hdi_group==982~2,
                             hdi_group==982~4))%>%
  arrange(hdi_group)%>%
  dplyr::mutate(across(2:5, round, -2))%>%
  dplyr::mutate(across(8:13, round, 1))%>%
  dplyr::mutate(across(14:15, round, -2))%>%
  dplyr::mutate(pAD_prev_tot=AD_prev/sum(total_deaths))%>%
  dplyr::mutate(pAD_treat_tot=AD_treat/sum(total_deaths))%>%
  dplyr::mutate(pAD_treat_prev_tot=AD_treat_prev/sum(total_deaths))%>%
  dplyr::mutate(across(16:19, round,3)*100)%>% #dplyr::mutate to show proportion as percentage in export
  dplyr::mutate(across(21:23, round,3)*100)%>% #dplyr::mutate to show proportion as percentage in export
  dplyr::select( "hdi_group",      "cancer", 
                 "AD_prev",        "pAD_prev",    "AD_prev.asr",
                 "AD_treat",       "pAD_treat" ,"AD_treat.asr",
                 "AD_treat_prev",  "pAD_treat_prev","AD_treat_prev.asr",
                 "AD_unavoid",     "pAD_unavoid" ,   "AD_unavoid.asr",    
                 "total_deaths","total.deaths.asr",
                 "pAD_prev_tot","pAD_treat_tot", "pAD_treat_prev_tot")




# getPalette = colorRampPalette(brewer.pal(9, "Set1"))
# colourCount = 35




# Pie Chart

library(RColorBrewer) 

getPalette = colorRampPalette(brewer.pal(9, "Set1"))
colourCount = 35


table_1_1
# pie charts ----
# alternative pie charts script using some elements from Bar_and_Pie_base.R script using globocan colours
col <- read.csv("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Data\\cancer_color_2018.csv", sep=",",stringsAsFactors = F)


Avoidable_Deaths_Simulated_All %>% 
  filter(sex==0)%>%
  group_by(cancer) %>% 
  dplyr::mutate(AD_treat_prev=sum(AD_treat, AD_prev,na.rm=T),
                AD_prev=sum(AD_prev,na.rm=T),
                AD_treat=sum(AD_treat,na.rm=T),
                total_deaths2=sum(total_deaths2,na.rm=T)) %>% 
  filter(!is.na(cancer_code)) %>% 
  dplyr::select(cancer,cancer_code,AD_prev,AD_treat,AD_treat_prev,total_deaths2)%>% 
  unique() %>% 
  left_join(col %>% 
              dplyr::select(cancer_label:Color.Hex) %>%
              filter(cancer_label!="Colorectum")) %>% 
  pivot_longer(AD_prev:total_deaths2,
               names_to="AD_cat",
               values_to = "AD") %>%
  group_by(AD_cat) %>% 
  dplyr::mutate(percent=sum(AD),
                percent = AD/percent) %>% 
  dplyr::arrange(AD) %>% 
  dplyr::mutate(rankc = as.numeric(dplyr::row_number())) %>% 
  group_by(AD_cat) %>% 
  # dplyr::mutate(AD=case_when(rankc<30~sum(AD[rankc<30]),
  #                            TRUE~AD),
  #               percent=case_when(rankc<30~sum(percent[rankc<30]),
  #                                 TRUE~percent), 
  #               rankc=case_when(rankc<30~1,
  #                               TRUE~rankc),
  #               Color.Hex=case_when(rankc==1~"#DCDCDC",
  #                                   TRUE~Color.Hex),
  #               cancer=case_when(rankc==1~"Other sites",
  #                                TRUE~cancer)) %>% 
  dplyr::select(cancer,Color.Hex,AD_cat,AD,percent,rankc) %>% 
  unique() -> AD_by_cancer_site_1


Avoidable_Deaths_Simulated_All %>%
  filter(!is.na(cancer_code)) %>%
  group_by(cancer,cancer_code,hdi_group) %>% 
  dplyr::mutate(AD_treat_prev=sum(AD_treat, AD_prev,na.rm=T),
                AD_prev=sum(AD_prev,na.rm=T),
                AD_treat=sum(AD_treat,na.rm=T),
                total_deaths2=sum(total_deaths2,na.rm=T)) %>% 
  dplyr::select(hdi_group, cancer,cancer_code,AD_prev,AD_treat,AD_treat_prev,total_deaths2)%>% 
  unique() %>% 
  left_join(col%>% dplyr::select(cancer_label:Color.Hex)%>% filter(cancer_label!="Colorectum")) %>% 
  pivot_longer(AD_prev:total_deaths2,
               names_to="AD_cat",
               values_to = "AD") %>%
  group_by(AD_cat,hdi_group) %>%
  dplyr::mutate(percent=sum(AD),
                percent = AD/percent) %>% 
  arrange(AD) %>% 
  dplyr::mutate(rankc = as.numeric(dplyr::row_number())) %>% 
  group_by(AD_cat, hdi_group) %>% 
  # dplyr::mutate(AD=case_when(rankc<30~sum(AD[rankc<30]),
  #                            TRUE~AD),
  #               percent=case_when(rankc<30~sum(percent[rankc<30]),
  #                                 TRUE~percent), 
  #               rankc=case_when(rankc<30~1,
  #                               TRUE~rankc),
  #               Color.Hex=case_when(rankc==1~"#DCDCDC",
  #                                   TRUE~Color.Hex),
  #               cancer=case_when(rankc==1~"Other sites",
  #                                TRUE~cancer)) %>% 
  dplyr::select(hdi_group, cancer,Color.Hex,AD_cat,AD,percent,rankc) %>% 
  unique() -> AD_by_cancer_site_1_HDI


#preventable pie
piedp <- as.data.table(AD_by_cancer_site_1 %>%filter(AD_cat=="AD_prev") )
piedp %>%
  ggplot(aes(x = 2, y = percent, fill = factor(rankc,levels = unique(piedp$rankc),
                                               labels = unique(piedp$Color.Hex)), 
             width=2*sqrt(table_1_1$AD_prev/table_1_1$AD_treat_prev/pi))) +
  geom_bar(width = 1, stat = "identity") +
  geom_text(aes(label = paste0(formatC(round_any(AD,100), format="f", big.mark=",", digits=0),"\n ", 
                               scales::percent(percent, accuracy = 0.1L)), x = 2.75),
            position = position_stack(vjust=0.5),
            size=5) +
  coord_polar(theta = "y") +
  scale_fill_identity("Cancer site", labels = piedp$cancer,
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
        strip.background = element_blank(),
        plot.caption = element_text(hjust = 0.5, face = "italic"))+
  theme(legend.position = "none")+ 
  
  labs(title="Primary Prevention", caption= paste(formatC(round(table_1_1$AD_prev,-3), format="d", big.mark=",")," total deaths, ", 
                                                  round(table_1_1$pAD_prev,3)*100, "%"))+
  theme(plot.caption = element_text(hjust = 0.5, face = "italic", size = 18))+
  theme(plot.title = element_text(size = 18))-> pie.prev
pie.prev
# ggsave("pie.prev.png",pie.prev,width=5.43 ,height=2.43, dpi=700,
#        path ="\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\oliver_langselius\\AD_PREV_TREAT\\Figures") 

#treatable pie
piedt <- as.data.table(AD_by_cancer_site_1 %>%filter(AD_cat=="AD_treat") )
piedt %>%
  ggplot(aes(x = 2, y = percent, fill = factor(rankc,levels = unique(piedt$rankc),
                                               labels = unique(piedt$Color.Hex)),
             width=2*sqrt(table_1_1$AD_treat/table_1_1$AD_treat_prev/pi))) +
  geom_bar(width = 1, stat = "identity") +
  geom_text(aes(label = paste0(formatC(round_any(AD,100), format="f", big.mark=",", digits=0),"\n ", 
                               scales::percent(percent, accuracy = 0.1L)), x = 2.75),
            position = position_stack(vjust=0.5),
            size=5) +
  coord_polar(theta = "y") +
  scale_fill_identity("Cancer site", labels = piedt$cancer,
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
        strip.background = element_blank(),
        plot.caption = element_text(hjust = 0.5, face = "italic"))+# move caption to the left)+
  theme(legend.position = "none")+ 
  labs(title="Early detection and treatment", caption= paste(formatC(round(table_1_1$AD_treat,-3), format="d", big.mark=",")," total deaths, ", 
                                                             round(table_1_1$pAD_treat,3)*100, "%"))+
  theme(plot.caption = element_text(hjust = 0.5, face = "italic", size = 18))+
  theme(plot.title = element_text(size = 18))-> pie.treat
pie.treat

# ggsave("pie.treat.png",pie.treat,width=5.43 ,height=2.43,dpi=700,
#        path ="\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\oliver_langselius\\AD_PREV_TREAT\\Figures") 

#avoidable pie
pieda <- as.data.table(AD_by_cancer_site_1 %>%filter(AD_cat=="AD_treat_prev") )
pieda %>%
  ggplot(aes(x = 2, y = percent, fill = factor(rankc,levels = unique(pieda$rankc),
                                               labels = unique(pieda$Color.Hex)), 
             width=2)) +
  geom_bar(width = 1, stat = "identity") +
  geom_text(aes(label = paste0(formatC(round_any(AD,100), format="f", big.mark=",", digits=0),"\n ", 
                               scales::percent(percent, accuracy = 0.1L)), x = 2.75),
            position = position_stack(vjust=0.5),
            size=5) +
  coord_polar(theta = "y") +
  scale_fill_identity("Cancer site", labels = pieda$cancer,
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
        strip.background = element_blank(),
        plot.caption = element_text(hjust = 0.5, face = "italic"))+
  theme(legend.position = "none")+ 
  labs(title="Prevention and early detection and treatment", caption= paste(formatC(round(table_1_1$AD_treat_prev,-3), format="d", big.mark=",")," total deaths, ",
                                                                            round(table_1_1$pAD_treat_prev,3)*100, "%"))+
  theme(plot.caption = element_text(hjust = 0.5, face = "italic", size = 18))+
  theme(plot.title = element_text(size = 18)) -> pie.avoid
pie.avoid
# ggsave("pie.avoid.png",pie.avoid,width=5.43 ,height=2.43,dpi=700,
#        path ="\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\oliver_langselius\\AD_PREV_TREAT\\Figures") 


#arranging in grid
library(ggpubr)
library(gridExtra)
#http://www.sthda.com/english/wiki/wiki.php?id_contents=7930 code modified from here




# cancer_colors_pied<- cancer_colors%>%filter(cancer%in%pied2$cancer)
# palette1_named_pied =  setNames(object = cancer_colors_pied$Color.Hex, nm = cancer_colors_pied$cancer)


# Object to define colors and rank for overall AD
Avoidable_Deaths_Simulated_All %>%
  as.data.frame()%>%
 # filter(cancer_code%in% c(20, 23, 8, 11, 15, 34, 6, 27, 7, 10 ))%>% #ten cancer sites plus last one to create the other category
  group_by(cancer_code) %>%
  dplyr::mutate(AD_treat_prev = sum(AD_treat, AD_prev, na.rm = TRUE),
                #AD_prev = sum(AD_prev, na.rm = TRUE),
                #AD_treat = sum(AD_treat, na.rm = TRUE),
                #total_deaths2 = sum(total_deaths2, na.rm = TRUE)
  ) %>%
  filter(!is.na(cancer_code)) %>%
  dplyr::select(cancer, cancer_code, #AD_prev, AD_treat, 
                AD_treat_prev#, total_deaths2
  ) %>%
  unique() %>%
  left_join(col %>%
              dplyr::select(cancer_label:Color.Hex) %>%
              filter(cancer_label != "Colorectum")) %>%
  pivot_longer(AD_treat_prev,
               names_to = "AD_cat",
               values_to = "AD") %>%
  as.data.frame()%>%
  group_by(AD_cat) %>%
  dplyr::mutate(total = sum(AD))%>%
  group_by(cancer_label)%>%
  dplyr::mutate(percent = AD / total)%>%
  arrange(AD) %>%
  ungroup()%>%
  dplyr::mutate(rankc = row_number()) %>%
  group_by(AD_cat) %>%
  # mutate(AD = case_when(rankc == 1 ~ sum(AD[rankc == 1]),
  #                       TRUE ~ AD),
  #        percent = case_when(rankc == 1 ~ sum(percent[rankc == 1]),
  #                            TRUE ~ percent),
  #        rankc = case_when(rankc == 1 ~ 1,
  #                          TRUE ~ rankc),
  #        Color.Hex = case_when(rankc == 1 ~ "#DCDCDC",
  #                              TRUE ~ Color.Hex),
  #        cancer = case_when(rankc == 1 ~ "Other sites",
  #                           TRUE ~ cancer)) %>%
  dplyr::select(cancer, Color.Hex, AD_cat, AD, percent, 
                rankc) %>%
  unique() -> AD_by_cancer_site_labels



#Creating combined legend 


pies91 <-   as.data.table(AD_by_cancer_site_labels)%>%
  dplyr::select(-AD,-AD_cat,-rankc)%>%
  distinct()%>%
  group_by(cancer)%>%
  mutate(percent=sum(percent))%>%
  ungroup()%>%
  distinct()%>%
  dplyr::mutate(rankc=row_number())%>%
  #arrange(desc(cancer))%>%
  # arrange(desc(cancer))%>%
  filter(cancer!="Other sites")

pies9 <-   as.data.table(AD_by_cancer_site_labels)%>%
  dplyr::select(-AD,-AD_cat,-rankc)%>%
  distinct()%>%
  group_by(cancer)%>%
  mutate(percent=sum(percent))%>%
  ungroup()%>%
  distinct()%>%
  dplyr::mutate(rankc=row_number())%>%
  arrange(desc(cancer))%>%
  filter(cancer=="Other sites")%>%
  full_join(pies91)

#plot1_legend<-
pies9 %>%
  ggplot(aes(x = 2, y = percent, fill = factor(rankc,levels = unique(rankc),
                                               labels = unique(Color.Hex)))) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  scale_fill_identity("Cancer site", labels = pies9$cancer,
                      guide = "legend") +
  guides(fill = guide_legend(reverse = TRUE))+
  theme(plot.background= element_blank(),
        plot.title = element_text(size=12, margin=margin(0,0,0,0),hjust = 0.5),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
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
combined_plot <-   grid.arrange(pie.prev, pie.treat, pie.avoid, ncol = 3)
Top_4_cancer<- grid.arrange(combined_plot, legend, ncol = 2, widths= c(0.85, 0.15))

Top_4_cancer
#Saving the output
# ggsave("Figure_2.pdf",Top_4_cancer,width=19 ,height=8, dpi=700,
#        path ="\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\oliver_langselius\\AD_PREV_TREAT\\Figures") 

#avoidable pie for HDI group 1
pied1 <- as.data.table(AD_by_cancer_site_1_HDI %>%filter(AD_cat=="AD_treat_prev",hdi_group==1) )
pied1 %>%
  ggplot(aes(x = 2, y = percent, fill = factor(rankc,levels = unique(pied1$rankc),
                                               labels = unique(pied1$Color.Hex)))) +
  geom_bar(width = 1, stat = "identity") +
  geom_text(aes(label = paste0(formatC(round_any(AD,100), format="f", big.mark=",", digits=0),"\n ", 
                               scales::percent(percent, accuracy = 0.1L)), x = 2.75),
            position = position_stack(vjust=0.5),
            size=5) +
  coord_polar(theta = "y") +
  scale_fill_identity("Cancer site", labels = pied1$cancer,
                      guide = "legend") +
  guides(fill = guide_legend(reverse = TRUE))+
  theme(plot.background= element_blank(),
        plot.title = element_text(size=12, margin=margin(0,0,0,0),hjust = 0.5),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        panel.background = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_blank(),
        plot.caption = element_text(hjust = 0.5, face = "italic"))+
  theme(legend.position = "none")+ 
  labs(title="Low HDI", caption= paste(formatC(round(AD_by_HDI_all2[1,]$AD_treat_prev,-3), format="d", big.mark=",")," total deaths, ",
                                       sprintf("%.1f", round(AD_by_HDI_all2[1,]$pAD_treat_prev_tot, 2)), "%"))+
  theme(plot.caption = element_text(hjust = 0.5, face = "italic", size = 18))+
  theme(plot.title = element_text(size = 18)) -> pie.avoid.hdi1
pie.avoid.hdi1
# ggsave("pie.avoid.hdi1.png",pie.avoid.hdi1,width=5.43 ,height=2.43,
#        path ="\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\oliver_langselius\\AD_PREV_TREAT\\Figures") 


pied2 <- as.data.table(AD_by_cancer_site_1_HDI %>%filter(AD_cat=="AD_treat_prev",hdi_group==2) )
pied2 %>%
  ggplot(aes(x = 2, y = percent, fill = factor(rankc,levels = unique(pied2$rankc),
                                               labels = unique(pied2$Color.Hex)))) +
  geom_bar(width = 1, stat = "identity") +
  geom_text(aes(label = paste0(formatC(round_any(AD,100), format="f", big.mark=",", digits=0),"\n ", 
                               scales::percent(percent, accuracy = 0.1L)), x = 2.75),
            position = position_stack(vjust=0.5),
            size=5) +
  coord_polar(theta = "y") +
  scale_fill_identity("Cancer site", labels = pied2$cancer,
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
        strip.background = element_blank(),
        plot.caption = element_text(hjust = 0.5, face = "italic"))+
  theme(legend.position = "none")+ 
  labs(title="Medium HDI", caption= paste(formatC(round(AD_by_HDI_all2[3,]$AD_treat_prev,-3), format="d", big.mark=",")," total deaths, ", 
                                          sprintf("%.1f", round(AD_by_HDI_all2[3,]$pAD_treat_prev_tot, 2)), "%"))+
  theme(plot.caption = element_text(hjust = 0.5, face = "italic", size = 18))+
  theme(plot.title = element_text(size = 18)) -> pie.avoid.hdi2
pie.avoid.hdi2
# ggsave("pie.avoid.hdi2.png",pie.avoid.hdi2,width=5.43 ,height=2.43,
#        path ="\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\oliver_langselius\\AD_PREV_TREAT\\Figures") 

pied3 <- as.data.table(AD_by_cancer_site_1_HDI %>%filter(AD_cat=="AD_treat_prev",hdi_group==3) )
pied3 %>%
  ggplot(aes(x = 2, y = percent, fill = factor(rankc,levels = unique(pied3$rankc),
                                               labels = unique(pied3$Color.Hex)))) +
  geom_bar(width = 1, stat = "identity") +
  geom_text(aes(label = paste0(formatC(round_any(AD,100), format="f", big.mark=",", digits=0),"\n ", 
                               scales::percent(percent, accuracy = 0.1L)), x = 2.75),
            position = position_stack(vjust=0.5),
            size=5) +
  coord_polar(theta = "y") +
  scale_fill_identity("Cancer site", labels = pied3$cancer,
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
        strip.background = element_blank(),
        plot.caption = element_text(hjust = 0.5, face = "italic"))+
  theme(legend.position = "none")+ 
  labs(title="High HDI", caption= paste(formatC(round(AD_by_HDI_all2[2,]$AD_treat_prev,-3), format="d", big.mark=",")," total deaths, ", 
                                        sprintf("%.1f", round(AD_by_HDI_all2[2,]$pAD_treat_prev_tot, 2)), "%"))+
  theme(plot.caption = element_text(hjust = 0.5, face = "italic", size = 18))+
  theme(plot.title = element_text(size = 18)) -> pie.avoid.hdi3
pie.avoid.hdi3
# ggsave("pie.avoid.hdi3.png",pie.avoid.hdi3,width=5.43 ,height=2.43,
#        path ="\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\oliver_langselius\\AD_PREV_TREAT\\Figures") 


pied4 <- as.data.table(AD_by_cancer_site_1_HDI %>%filter(AD_cat=="AD_treat_prev",hdi_group==4) )
pied4 %>%
  ggplot(aes(x = 2, y = percent, fill = factor(rankc,levels = unique(pied4$rankc),
                                               labels = unique(pied4$Color.Hex)))) +
  geom_bar(width = 1, stat = "identity") +
  geom_text(aes(label = paste0(formatC(round_any(AD,100), format="f", big.mark=",", digits=0),"\n ", 
                               scales::percent(percent, accuracy = 0.1L)), x = 2.75),
            position = position_stack(vjust=0.5),
            size=5) +
  coord_polar(theta = "y") +
  scale_fill_identity("Cancer site", labels = pied4$cancer,
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
        strip.background = element_blank(),
        plot.caption = element_text(hjust = 0.5, face = "italic"))+
  theme(legend.position = "none")+ 
  labs(title="Very High HDI", caption= paste(formatC(round(AD_by_HDI_all2[4,]$AD_treat_prev,-3), format="d", big.mark=",")," total deaths, ", 
                                             sprintf("%.1f", round(AD_by_HDI_all2[4,]$pAD_treat_prev_tot, 2)), "%"))+
  theme(plot.caption = element_text(hjust = 0.5, face = "italic", size = 18))+
  theme(plot.title = element_text(size = 18)) -> pie.avoid.hdi4
pie.avoid.hdi4
# ggsave("pie.avoid.hdi4.png",pie.avoid.hdi4,width=5.43 ,height=2.43,
#        path ="\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\oliver_langselius\\AD_PREV_TREAT\\Figures") 

#combined plot - edit below to match the HDI plots

#Creating combined legend 


pies101 <- as.data.table(AD_by_cancer_site_1_HDI)%>%
  dplyr::select(-AD,-AD_cat,-rankc, -hdi_group)%>%
  distinct()%>%
  group_by(cancer)%>%
  mutate(percent=sum(percent))%>%
  ungroup()%>%
  distinct()%>%
  dplyr::mutate(rankc=row_number())%>%
  arrange(desc(cancer))%>%
  mutate(cancer=as.factor(cancer))%>%
  filter(cancer!="Other sites")


pies10 <- as.data.table(AD_by_cancer_site_1_HDI)%>%
  dplyr::select(-AD,-AD_cat,-rankc, -hdi_group)%>%
  distinct()%>%
  group_by(cancer)%>%
  mutate(percent=sum(percent))%>%
  ungroup()%>%
  distinct()%>%
  dplyr::mutate(rankc=row_number())%>%
  arrange(desc(cancer))%>%
  #  mutate(cancer=as.factor(cancer))%>%
  filter(cancer=="Other sites")%>%
  full_join(pies101)%>%
  #fixing order so its by how they are displayed, left from right
 # mutate(rank=case_when( cancer=="Cervix uteri"  ~ 12, 
  #                        cancer=="Breast"  ~ 11,
  #                        cancer=="Liver"  ~ 10,
  #                        cancer=="Stomach"  ~ 9,
  #                        cancer=="Colorectal"  ~ 8,
  #                        cancer=="Lung"  ~ 7,
  #                        cancer=="Lip, oral cavity"  ~ 6,
  #                        cancer=="Oesophagus"  ~ 5,
  #                        cancer=="Pancreas"  ~ 4,
  #                        cancer=="Other sites"  ~ 1,
  # ))%>%
  #filter(!cancer%in%c("Prostate", "Leukaemia", "Non-Hodgkin lymphoma"))%>%
  arrange(rank)%>%
  mutate(cancer=as.factor(cancer))

pies10 %>%
  ggplot(aes(x = 2, y = percent, fill =
               factor(rankc,levels = unique(rankc),
                      labels = unique(Color.Hex)))) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  scale_fill_identity("Cancer site", labels = pies10$cancer,
                      guide = "legend") +
  guides(fill = guide_legend(reverse = TRUE))+
  theme(plot.background= element_blank(),
        plot.title = element_text(size=12, margin=margin(0,0,0,0),hjust = 0.5),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        panel.background = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_blank()) -> plot2_legend


plot2_legend

# function to extract legend from plot
get_only_legend <- function(plot) {
  plot_table <- ggplot_gtable(ggplot_build(plot))
  legend_plot <- which(sapply(plot_table$grobs, function(x) x$name) == "guide-box")
  legend <- plot_table$grobs[[legend_plot]]
  return(legend)
}

# extract legend from plot1 using above function

legend2 <- get_only_legend(plot2_legend)   

# final combined plot with shared legend

combined_plothdi <-   grid.arrange(pie.avoid.hdi1, 
                                   pie.avoid.hdi2, 
                                   pie.avoid.hdi3, 
                                   pie.avoid.hdi4, ncol = 2,nrow=2)

Top_4_cancerhdi <-    grid.arrange(combined_plothdi, 
                                   legend2, 
                                   ncol = 2, 
                                   widths= c(0.85, 0.15))

Top_4_cancerhdi

#Saving the output
# ggsave("Figure_3.pdf", Top_4_cancerhdi, width=15, height=10, dpi=700,
#        path ="\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\oliver_langselius\\AD_PREV_TREAT\\Figures") 



# ============================================================================
# Regional Analysis: Preventable, Treatable, and Avoidable Deaths by Region
# Top 6 regions plus "Other regions"
# ============================================================================

# Prepare regional data - group by region for all cancers in AD_Region2
AD_Region_by_country <- AD_Region2 %>%
  filter(sex==0)%>%
  group_by(country_label) %>%
  dplyr::mutate(
    AD_treat_prev = sum(AD_treat, AD_prev, na.rm = TRUE),
    AD_prev = sum(AD_prev, na.rm = TRUE),
    AD_treat = sum(AD_treat, na.rm = TRUE),
    total_deaths = sum(total_deaths, na.rm = TRUE)
  ) %>%
  dplyr::select(country_label, sex, AD_prev, AD_treat, AD_treat_prev, total_deaths) %>%
  unique() %>%
  as.data.frame()

# Create regional summary with top 6 + other regions
AD_Region_by_country_summary <- AD_Region_by_country %>%
  dplyr::mutate(percent_prev = AD_prev / sum(AD_prev, na.rm = TRUE),
         percent_treat = AD_treat / sum(AD_treat, na.rm = TRUE),
         percent_avoid = AD_treat_prev / sum(AD_treat_prev, na.rm = TRUE)) %>%
  arrange(desc(AD_treat_prev)) %>%
  dplyr::mutate(rankc = as.numeric(dplyr::row_number()))

# select what sexes to include



# Get top 6 and create other_regions
top6_prev <- AD_Region_by_country_summary %>%
  group_by(sex)%>%
  arrange(desc(AD_prev)) %>%
  mutate(rankc_new = row_number()) %>%
  mutate(
    country_label = case_when(
      rankc_new <= 6 ~ country_label,
      TRUE ~ "Other regions"
    ),
    AD_prev = case_when(
      rankc_new <= 6 ~ AD_prev,
      TRUE ~ sum(AD_prev[rankc_new > 6], na.rm = TRUE)
    ),
    percent_prev = case_when(
      rankc_new <= 6 ~ percent_prev,
      TRUE ~ sum(percent_prev[rankc_new > 6], na.rm = TRUE)
    ),
    rankc_new = case_when(
      rankc_new <= 6 ~ rankc_new + 1,
      TRUE ~ 1
    )
  ) %>%
  select(country_label, sex, AD_prev, percent_prev, rankc_new) %>%
  distinct() %>%
  arrange(rankc_new)

top6_prev_other<-top6_prev %>% filter(country_label=="Other regions")

top6_prev <-top6_prev %>%
  filter(country_label!="Other regions")%>%
  arrange(desc(rankc_new))

top6_prev <- top6_prev_other%>%
  full_join(top6_prev)%>%
  mutate(country_label = factor(country_label, levels = unique(country_label)))

top6_treat <- AD_Region_by_country_summary %>%
  group_by(sex)%>%
  arrange(desc(AD_treat)) %>%
  mutate(rankc_new = row_number()) %>%
  mutate(
    country_label = case_when(
      rankc_new <= 6 ~ country_label,
      TRUE ~ "Other regions"
    ),
    AD_treat = case_when(
      rankc_new <= 6 ~ AD_treat,
      TRUE ~ sum(AD_treat[rankc_new > 6], na.rm = TRUE)
    ),
    percent_treat = case_when(
      rankc_new <= 6 ~ percent_treat,
      TRUE ~ sum(percent_treat[rankc_new > 6], na.rm = TRUE)
    ),
    rankc_new = case_when(
      rankc_new <= 6 ~ rankc_new + 1,
      TRUE ~ 1
    )
  ) %>%
  select(country_label, AD_treat, percent_treat, rankc_new) %>%
  distinct() %>%
  arrange(rankc_new) 


top6_treat_other<-top6_treat %>% filter(country_label=="Other regions")

top6_treat <-top6_treat %>%
  filter(country_label!="Other regions")%>%
  arrange(desc(rankc_new))

top6_treat<- top6_treat_other%>%
  full_join(top6_treat)%>%
  mutate(country_label = factor(country_label, levels = unique(country_label)))


top6_avoid <- AD_Region_by_country_summary %>%
  group_by(sex)%>%
  arrange(desc(AD_treat_prev)) %>%
  mutate(rankc_new = row_number()) %>%
  mutate(
    country_label = case_when(
      rankc_new <= 6 ~ country_label,
      TRUE ~ "Other regions"
    ),
    AD_treat_prev = case_when(
      rankc_new <= 6 ~ AD_treat_prev,
      TRUE ~ sum(AD_treat_prev[rankc_new > 6], na.rm = TRUE)
    ),
    percent_avoid = case_when(
      rankc_new <= 6 ~ percent_avoid,
      TRUE ~ sum(percent_avoid[rankc_new > 6], na.rm = TRUE)
    ),
    rankc_new = case_when(
      rankc_new <= 6 ~ rankc_new + 1,
      TRUE ~ 1
    )
  ) %>%
  select(sex, country_label, AD_treat_prev, percent_avoid, rankc_new) %>%
  distinct() %>%
  arrange(rankc_new) 


top6_avoid_other<-top6_avoid %>% filter(country_label=="Other regions")

top6_avoid <-top6_avoid %>%
  filter(country_label!="Other regions")%>%
  arrange(desc(rankc_new))

top6_avoid <-top6_avoid_other %>%
  full_join(top6_avoid)%>%
  mutate(country_label = factor(country_label, levels = unique(country_label)))


# Define colors for regions
nb.cols <- nrow(top6_avoid)
mycolors <- colorRampPalette(brewer.pal(12, "Paired"))(nb.cols)

# Preventable pie chart by region
pieregion_prev <- as.data.table(top6_prev) %>%
  filter(percent_prev != 0) %>%
  ggplot(aes(x = "", y = percent_prev, fill = country_label)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  geom_text_repel(
    aes(label = paste0(formatC(round_any(AD_prev, 100), format = "f", big.mark = ",", digits = 0), "\n ",
                       scales::percent(percent_prev, accuracy = 1)), x = 2.75),
    position = position_stack(vjust = 0.5),
    size = 4
  ) +
  guides(fill = guide_legend(reverse = TRUE, title = "Region")) +
  scale_fill_manual(values = mycolors) +
  theme(plot.background = element_blank(),
        plot.title = element_text(size = 12, margin = margin(0, 0, 0, 0), hjust = 0.5),
        panel.background = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_blank(),
        plot.caption = element_text(hjust = 0.5, face = "italic")) +
  theme(legend.position = "none") +
  labs(title="Primary Prevention", caption= paste(formatC(round(table_1_1$AD_prev,-3), format="d", big.mark=",")," total deaths, ", 
                                                  round(table_1_1$pAD_prev,3)*100, "%"))+
  theme(plot.caption = element_text(hjust = 0.5, face = "italic", size = 14)) +
  theme(plot.title = element_text(size = 14))

pieregion_prev
ggsave("pieregion_prev.png", pieregion_prev, width = 5.43, height = 2.43, dpi = 700,
       path = "\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\_work\\Oliver_Langselius\\AD_PREV_TREAT\\Gastric2024")

# Treatable pie chart by region
pieregion_treat <- as.data.table(top6_treat) %>%
  filter(percent_treat != 0) %>%
  ggplot(aes(x = "", y = percent_treat, fill = country_label)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  geom_text_repel(
    aes(label = paste0(formatC(round_any(AD_treat, 100), format = "f", big.mark = ",", digits = 0), "\n ",
                       scales::percent(percent_treat, accuracy = 1)), x = 2.75),
    position = position_stack(vjust = 0.5),
    size = 4
  ) +
  guides(fill = guide_legend(reverse = TRUE, title = "Region")) +
  scale_fill_manual(values = mycolors) +
  theme(plot.background = element_blank(),
        plot.title = element_text(size = 12, margin = margin(0, 0, 0, 0), hjust = 0.5),
        panel.background = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_blank(),
        plot.caption = element_text(hjust = 0.5, face = "italic")) +
  theme(legend.position = "none") +
  labs(title="Early detection and treatment", caption= paste(formatC(round(table_1_1$AD_treat,-3), format="d", big.mark=",")," total deaths, ", 
                                                  round(table_1_1$pAD_treat,3)*100, "%"))+
  theme(plot.caption = element_text(hjust = 0.5, face = "italic", size = 14)) +
  theme(plot.title = element_text(size = 14))

pieregion_treat
ggsave("pieregion_treat.png", pieregion_treat, width = 5.43, height = 2.43, dpi = 700,
       path = "\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\_work\\Oliver_Langselius\\AD_PREV_TREAT\\Gastric2024")

# Avoidable pie chart by region
pieregion_avoid <- as.data.table(top6_avoid) %>%
  filter(percent_avoid != 0) %>%
  ggplot(aes(x = "", y = percent_avoid, fill = country_label)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  geom_text_repel(
    aes(label = paste0(formatC(round_any(AD_treat_prev, 100), format = "f", big.mark = ",", digits = 0), "\n ",
                       scales::percent(percent_avoid, accuracy = 1)), x = 2.75),
    position = position_stack(vjust = 0.5),
    size = 4
  ) +
  guides(fill = guide_legend(reverse = TRUE, title = "Region")) +
  scale_fill_manual(values = mycolors) +
  theme(plot.background = element_blank(),
        plot.title = element_text(size = 12, margin = margin(0, 0, 0, 0), hjust = 0.5),
        panel.background = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_blank(),
        plot.caption = element_text(hjust = 0.5, face = "italic")) +
  theme(legend.position = "none") +
  labs(title="Primary Prevention, early detection and treatment", caption= paste(formatC(round(table_1_1$AD_treat_prev,-3), format="d", big.mark=",")," total deaths, ", 
                                                  round(table_1_1$pAD_treat_prev,3)*100, "%"))+
  theme(plot.caption = element_text(hjust = 0.5, face = "italic", size = 14)) +
  theme(plot.title = element_text(size = 14))

pieregion_avoid
ggsave("pieregion_avoid.png", pieregion_avoid, width = 5.43, height = 2.43, dpi = 700,
       path = "\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\_work\\Oliver_Langselius\\AD_PREV_TREAT\\Gastric2024")

# Create legend
pieregion_legend <- as.data.table(top6_avoid) %>%
  filter(percent_avoid != 0) %>%
  ggplot(aes(x = "", y = percent_avoid, fill = country_label)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  guides(fill = guide_legend(reverse = TRUE, title = "Region")) +
  scale_fill_manual(values = mycolors) +
  theme(plot.background = element_blank(),
        plot.title = element_text(size = 12, margin = margin(0, 0, 0, 0), hjust = 0.5),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        panel.background = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_blank())

legend_region <- get_only_legend(pieregion_legend)

# Combined regional plot with legend
combined_region_plot <- grid.arrange(pieregion_prev, pieregion_treat, pieregion_avoid, ncol = 3)
Regional_pies_final <- grid.arrange(combined_region_plot, legend_region, ncol = 2, widths = c(0.85, 0.15))

Regional_pies_final

# Save the combined figure
ggsave("Regional_AD_Preventable_Treatable_Avoidable.pdf", Regional_pies_final, width = 19, height = 8, dpi = 700,
       path = "\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\_work\\Oliver_Langselius\\AD_PREV_TREAT\\Gastric2024")

ggsave("Regional_AD_Preventable_Treatable_Avoidable.png", Regional_pies_final, width = 19, height = 8, dpi = 700,
       path = "\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\_work\\Oliver_Langselius\\AD_PREV_TREAT\\Gastric2024")


# by sex -- no big visual differences here... 
# ============================================================================
# Regional Analysis by Sex: Avoidable Deaths by Region and Sex
# Top 6 regions plus "Other regions" for Male and Female
# ============================================================================
# 
# # Prepare regional data by sex - group by region and sex for avoidable deaths
# AD_Region_by_country_sex <- AD_Region2 %>%
#   group_by(country_label, sex) %>%
#   dplyr::mutate(
#     AD_treat_prev = sum(AD_treat, AD_prev, na.rm = TRUE),
#     total_deaths = sum(total_deaths, na.rm = TRUE)
#   ) %>%
#   dplyr::select(country_label, sex, AD_treat_prev, total_deaths) %>%
#   unique() %>%
#   as.data.frame()
# 
# # Create regional summary with top 6 + other regions for each sex
# AD_Region_by_country_sex_summary <- AD_Region_by_country_sex %>%
#   group_by(sex) %>%
#   dplyr::mutate(percent_avoid = AD_treat_prev / sum(AD_treat_prev, na.rm = TRUE)) %>%
#   arrange(desc(AD_treat_prev)) %>%
#   dplyr::mutate(rankc = as.numeric(dplyr::row_number())) %>%
#   ungroup()
# 
# # Get top 6 and create other_regions for sex-based analysis
# top6_avoid_sex <- AD_Region_by_country_sex_summary %>%
#   group_by(sex) %>%
#   arrange(desc(AD_treat_prev)) %>%
#   mutate(rankc_new = row_number()) %>%
#   mutate(
#     country_label = case_when(
#       rankc_new <= 6 ~ country_label,
#       TRUE ~ "Other regions"
#     ),
#     AD_treat_prev = case_when(
#       rankc_new <= 6 ~ AD_treat_prev,
#       TRUE ~ sum(AD_treat_prev[rankc_new > 6], na.rm = TRUE)
#     ),
#     percent_avoid = case_when(
#       rankc_new <= 6 ~ percent_avoid,
#       TRUE ~ sum(percent_avoid[rankc_new > 6], na.rm = TRUE)
#     ),
#     rankc_new = case_when(
#       rankc_new <= 6 ~ rankc_new + 1,
#       TRUE ~ 1
#     )
#   ) %>%
#   select(sex, country_label, AD_treat_prev, percent_avoid, rankc_new) %>%
#   distinct() %>%
#   arrange(rankc_new) %>%
#   ungroup()
# 
# # Define colors for regions
# nb.cols_sex <- n_distinct(top6_avoid_sex$country_label)
# mycolors_sex <- colorRampPalette(brewer.pal(12, "Paired"))(nb.cols_sex)
# 
# # Avoidable pie chart by region for sex 1 (Male)
# top6_avoid_sex_male <- top6_avoid_sex %>% filter(sex == 1)
# pieregion_avoid_sex1 <- as.data.table(top6_avoid_sex_male) %>%
#   filter(percent_avoid != 0) %>%
#   ggplot(aes(x = "", y = percent_avoid, fill = country_label)) +
#   geom_bar(width = 1, stat = "identity") +
#   coord_polar("y", start = 0) +
#   geom_text_repel(
#     aes(label = paste0(formatC(round_any(AD_treat_prev, 100), format = "f", big.mark = ",", digits = 0), "\n ",
#                        scales::percent(percent_avoid, accuracy = 1)), x = 2.75),
#     position = position_stack(vjust = 0.5),
#     size = 4
#   ) +
#   guides(fill = guide_legend(reverse = TRUE, title = "Region")) +
#   scale_fill_manual(values = mycolors_sex) +
#   theme(plot.background = element_blank(),
#         plot.title = element_text(size = 12, margin = margin(0, 0, 0, 0), hjust = 0.5),
#         panel.background = element_blank(),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         panel.border = element_blank(),
#         panel.grid = element_blank(),
#         axis.ticks = element_blank(),
#         strip.background = element_blank(),
#         plot.caption = element_text(hjust = 0.5, face = "italic")) +
#   theme(legend.position = "none") +
#   labs(title = "Male", 
#        caption = paste(formatC(round(sum(top6_avoid_sex_male$AD_treat_prev, na.rm = TRUE), -3), format = "d", big.mark = ","), " total deaths, ",
#                        round(sum(top6_avoid_sex_male$percent_avoid, na.rm = TRUE) * 100, 1), "%")) +
#   theme(plot.caption = element_text(hjust = 0.5, face = "italic", size = 14)) +
#   theme(plot.title = element_text(size = 14))
# 
# pieregion_avoid_sex1
# ggsave("pieregion_avoid_sex1_male.png", pieregion_avoid_sex1, width = 5.43, height = 2.43, dpi = 700,
#        path = "\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\_work\\Oliver_Langselius\\AD_PREV_TREAT\\Gastric2024")
# 
# # Avoidable pie chart by region for sex 2 (Female)
# top6_avoid_sex_female <- top6_avoid_sex %>% filter(sex == 2)
# pieregion_avoid_sex2 <- as.data.table(top6_avoid_sex_female) %>%
#   filter(percent_avoid != 0) %>%
#   ggplot(aes(x = "", y = percent_avoid, fill = country_label)) +
#   geom_bar(width = 1, stat = "identity") +
#   coord_polar("y", start = 0) +
#   geom_text_repel(
#     aes(label = paste0(formatC(round_any(AD_treat_prev, 100), format = "f", big.mark = ",", digits = 0), "\n ",
#                        scales::percent(percent_avoid, accuracy = 1)), x = 2.75),
#     position = position_stack(vjust = 0.5),
#     size = 4
#   ) +
#   guides(fill = guide_legend(reverse = TRUE, title = "Region")) +
#   scale_fill_manual(values = mycolors_sex) +
#   theme(plot.background = element_blank(),
#         plot.title = element_text(size = 12, margin = margin(0, 0, 0, 0), hjust = 0.5),
#         panel.background = element_blank(),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         panel.border = element_blank(),
#         panel.grid = element_blank(),
#         axis.ticks = element_blank(),
#         strip.background = element_blank(),
#         plot.caption = element_text(hjust = 0.5, face = "italic")) +
#   theme(legend.position = "none") +
#   labs(title = "Female", 
#        caption = paste(formatC(round(sum(top6_avoid_sex_female$AD_treat_prev, na.rm = TRUE), -3), format = "d", big.mark = ","), " total deaths, ")) +
#   theme(plot.caption = element_text(hjust = 0.5, face = "italic", size = 14)) +
#   theme(plot.title = element_text(size = 14))
# 
# pieregion_avoid_sex2
# ggsave("pieregion_avoid_sex2_female.png", pieregion_avoid_sex2, width = 5.43, height = 2.43, dpi = 700,
#        path = "\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\_work\\Oliver_Langselius\\AD_PREV_TREAT\\Gastric2024")
# 
# # Create legend for regional sex charts
# pieregion_legend_sex <- as.data.table(top6_avoid_sex) %>%
#   filter(percent_avoid != 0) %>%
#   ggplot(aes(x = "", y = percent_avoid, fill = country_label)) +
#   geom_bar(width = 1, stat = "identity") +
#   coord_polar("y", start = 0) +
#   guides(fill = guide_legend(reverse = TRUE, title = "Region")) +
#   scale_fill_manual(values = mycolors_sex) +
#   theme(plot.background = element_blank(),
#         plot.title = element_text(size = 12, margin = margin(0, 0, 0, 0), hjust = 0.5),
#         legend.text = element_text(size = 14),
#         legend.title = element_text(size = 14),
#         panel.background = element_blank(),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         panel.border = element_blank(),
#         panel.grid = element_blank(),
#         axis.ticks = element_blank(),
#         strip.background = element_blank())
# 
# legend_region_sex <- get_only_legend(pieregion_legend_sex)
# 
# # Combined regional sex plot with legend
# combined_region_sex_plot <- grid.arrange(pieregion_avoid_sex1, pieregion_avoid_sex2, ncol = 2)
# Regional_sex_pies_final <- grid.arrange(combined_region_sex_plot, legend_region_sex, ncol = 2, widths = c(0.85, 0.15))
# 
# Regional_sex_pies_final
# 
# # Save the combined figure
# ggsave("Regional_AD_Avoidable_by_Sex.pdf", Regional_sex_pies_final, width = 15, height = 8, dpi = 700,
#        path = "\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\_work\\Oliver_Langselius\\AD_PREV_TREAT\\Gastric2024")
# 
# ggsave("Regional_AD_Avoidable_by_Sex.png", Regional_sex_pies_final, width = 15, height = 8, dpi = 700,
#        path = "\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\_work\\Oliver_Langselius\\AD_PREV_TREAT\\Gastric2024")
# 
# 
# 


