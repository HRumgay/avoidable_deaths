
# load libraries needed for some charts
library(plyr) #added plyr here for rounding for the pie charts but it might break some tidyverse functions
library(tidyverse)
library(data.table)
library(Rcan)
library(ggrepel)
library("ggplot2")
library(ggsci)
library(grid)


setwd("C:\\Users\\langseliuso\\Documents\\GitHub\\avoidable_deaths\\AD Preventable and Treatable")


text_high <- textGrob("Highest\nvalue", gp=gpar(fontsize=13, fontface="bold"))
text_low <- textGrob("Lowest\nvalue", gp=gpar(fontsize=13, fontface="bold"))




#Alternative data frames for these figures

table_1_1_alt <- Avoidable_Deaths_Simulated_All %>%
  ungroup()%>%
  filter(!country_label%in%c("China","India"))%>%
  select(-country_label, -country_code, -cancer_code, hdi_group)%>%
  dplyr::mutate(age_cat="Overall")%>%
  dplyr::group_by(cancer, age,sex)%>%
  dplyr::mutate(py=sum(py))%>%
  ungroup()%>%
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  dplyr::mutate(hdi_group=as.numeric(hdi_group))%>%
  ungroup()%>%  dplyr::mutate(AD_treat_prev=AD_treat+AD_prev)%>%
  ungroup()%>%
  dplyr::mutate(AD_prev.asr=sum(AD_prev/py*100000*w), #ASR calculation here
                AD_treat.asr=sum(AD_treat/py*100000*w),
                AD_treat_prev.asr=sum((AD_treat + AD_prev)/py*100000*w),
                #AD_treat_not_prev.asr=sum(AD_treat_not_prev/py*100000*w),
                AD_unavoid.asr=sum(AD_unavoid/py*100000*w),
                total.deaths.asr=sum(total_deaths2/py*100000*w)) %>% 
  ungroup()%>%
  dplyr::mutate(total_deaths=sum(total_deaths2,na.rm=T))%>%
  dplyr::mutate(AD_treat_prev=sum(AD_treat,AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_treat=sum(AD_treat,na.rm=T))%>%
  #dplyr::mutate(AD_treat_not_prev=sum(AD_treat_not_prev,na.rm=T))%>%
  dplyr::mutate(AD_prev=sum(AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_unavoid=sum(AD_unavoid,na.rm=T))%>%
  dplyr::mutate(pAD_treat=AD_treat/total_deaths)%>%
  #dplyr::mutate(pAD_treat_not_prev=AD_treat_not_prev/total_deaths)%>%
  dplyr::mutate(pAD_prev=AD_prev/total_deaths)%>%
  dplyr::mutate(pAD_unavoid=AD_unavoid/total_deaths)%>%
  dplyr::mutate(pAD_treat_prev=AD_treat_prev/total_deaths)%>%
  select(AD_prev, AD_treat, 
         # AD_treat_not_prev,
         AD_treat_prev, AD_unavoid, total_deaths, 
         pAD_treat, 
         # pAD_treat_not_prev,
         pAD_prev,pAD_treat_prev, pAD_unavoid,
         AD_prev.asr,AD_treat.asr ,
         #AD_treat_not_prev.asr,
         AD_treat_prev.asr, AD_unavoid.asr,total.deaths.asr)%>%
  distinct()

#HDI - removing china + india from figure

AD_by_HDI_alt <-Avoidable_Deaths_Simulated_All%>%
  filter(!country_label%in%c("China","India"))%>%
  dplyr::mutate(age_cat="Overall")%>%
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  dplyr::group_by(hdi_group, cancer, age,sex)%>%
  dplyr::mutate(py=sum(py))%>%
  dplyr::mutate(AD_treat_prev=sum(AD_prev+AD_treat))%>%
  dplyr::mutate(AD_treat=sum(AD_treat))%>%
  #dplyr::mutate(AD_treat_not_prev=sum(AD_treat_not_prev))%>%
  ungroup()%>%
  select(-country_label,-country_code,  hdi_group, cancer, cancer_code, AD_treat, AD_prev, 
         AD_unavoid, AD_sum,-sex, -age, age_cat,-total_overall)%>%
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  dplyr::mutate(hdi_group=as.numeric(hdi_group))%>%
  ungroup()%>%
  dplyr::group_by(hdi_group,cancer)%>%
  dplyr::mutate(AD_prev.asr=sum(AD_prev/py*100000*w), #ASR calculation here
                AD_treat.asr=sum(AD_treat/py*100000*w),
                AD_sum.asr=sum(AD_sum/py*100000*w),
                AD_treat_prev.asr=sum((AD_treat+AD_prev)/py*100000*w),
                #  AD_treat_not_prev.asr=sum(AD_treat_not_prev/py*100000*w),
                AD_unavoid.asr=sum(AD_unavoid/py*100000*w),
                total.deaths.asr=sum(total_deaths2/py*100000*w)) %>%
  dplyr::group_by(hdi_group,cancer)%>%
  dplyr::mutate(AD_treat_prev=sum(AD_prev+AD_treat))%>%
  dplyr::mutate(total_deaths2=sum(total_deaths2))%>%
  dplyr::mutate(AD_treat=sum(AD_treat))%>%
  # dplyr::mutate(AD_treat_not_prev=sum(AD_treat_not_prev))%>%
  dplyr::mutate(AD_prev=sum(AD_prev))%>%
  dplyr::mutate(AD_unavoid=sum(AD_unavoid))%>%
  dplyr::mutate(total_deaths=total_deaths2)%>%
  dplyr::mutate(pAD_treat=AD_treat/total_deaths)%>%
  #dplyr::mutate(pAD_treat_not_prev=AD_treat_not_prev/total_deaths)%>%
  dplyr::mutate(pAD_prev=AD_prev/total_deaths)%>%
  dplyr::mutate(pAD_unavoid=AD_unavoid/total_deaths)%>%
  dplyr::mutate(pAD_treat_prev=AD_treat_prev/total_deaths)%>%
  dplyr::mutate(w=sum(w)/n())%>%
  select(-AD_sum,-py)%>%
  ungroup()%>%
  distinct()%>%
  as.data.frame()

  



  



# Pie Charts

AD_HDI_by_cancer_site_3_alt<-AD_by_HDI_alt %>%
  filter(age_cat=="Overall")%>%
  select(hdi_group,cancer, cancer_code, AD_treat)%>%
  dplyr::rename("AD"="AD_treat")%>%
  dplyr::mutate(AD_cat="Treatable")

AD_HDI_by_cancer_site_2_alt<-AD_by_HDI %>%
  filter(age_cat=="Overall")%>%
  select(hdi_group,cancer, cancer_code, AD_prev)%>%
  dplyr::rename("AD"="AD_prev")%>%
  dplyr::mutate(AD_cat="Preventable")%>%
  dplyr::mutate(hdi_group=as.numeric(hdi_group))

AD_HDI_by_cancer_site_1_alt<-AD_by_HDI %>%
  filter(age_cat=="Overall")%>%
  select(hdi_group, cancer, cancer_code, AD_treat_prev)%>%
  dplyr::mutate(hdi_group=as.numeric(hdi_group))%>%
  dplyr::rename("AD"="AD_treat_prev")%>%
  dplyr::mutate(AD_cat="Total")%>%
  ungroup()%>% 
  full_join(AD_HDI_by_cancer_site_2_alt)%>%
  full_join(AD_HDI_by_cancer_site_3_alt)



AD_HDI_by_cancer_site_11_alt<-AD_HDI_by_cancer_site_1_alt%>%
  group_by(hdi_group)%>%
  filter(AD_cat=="Total")%>% 
  slice_max(AD, n = 5)%>%
  select(cancer_code)%>%
  dplyr::mutate(max="max")



# getPalette = colorRampPalette(brewer.pal(9, "Set1"))
# colourCount = 35


AD_by_HDI_all_alt <-Avoidable_Deaths_Simulated_All%>%
  filter(!country_label%in%c("China","India"))%>%
  dplyr::mutate(age_cat="Overall")%>%
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  dplyr::group_by(hdi_group, cancer, age,sex)%>%
  dplyr::mutate(py=sum(py))%>%
  ungroup()%>%
  select(-country_label,-country_code,-cancer_code, -cancer,  hdi_group,  AD_treat, AD_prev, 
         AD_unavoid, total_deaths2, AD_sum,-sex, -age, age_cat,-total_overall)%>%
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  dplyr::mutate(hdi_group=as.numeric(hdi_group))%>%
  ungroup()%>%
  dplyr::group_by(hdi_group)%>%
  dplyr::mutate(AD_prev.asr=sum(AD_prev/py*100000*w), #ASR calculation here
                AD_treat.asr=sum(AD_treat/py*100000*w),
                AD_treat_prev.asr=sum((AD_treat+AD_prev)/py*100000*w),
                AD_sum.asr=sum(AD_sum/py*100000*w),
                #AD_treat_not_prev.asr=sum(AD_treat_not_prev/py*100000*w),
                AD_unavoid.asr=sum(AD_unavoid/py*100000*w),
                total.deaths.asr=sum(total_deaths2/py*100000*w)) %>% 
  dplyr::mutate(AD_treat_prev=sum(AD_prev+AD_treat))%>%
  dplyr::mutate(total_deaths2=sum(total_deaths2))%>%
  dplyr::mutate(AD_treat=sum(AD_treat))%>%
  #dplyr::mutate(AD_treat_not_prev=sum(AD_treat_not_prev))%>%
  dplyr::mutate(AD_prev=sum(AD_prev))%>%
  dplyr::mutate(AD_unavoid=sum(AD_unavoid))%>%
  dplyr::mutate(total_deaths=total_deaths2)%>%
  dplyr::mutate(pAD_treat=AD_treat/total_deaths)%>%
  #dplyr::mutate(pAD_treat_not_prev=AD_treat_not_prev/total_deaths)%>%
  dplyr::mutate(pAD_prev=AD_prev/total_deaths)%>%
  dplyr::mutate(pAD_unavoid=AD_unavoid/total_deaths)%>%
  dplyr::mutate(pAD_treat_prev=AD_treat_prev/total_deaths)%>%
  dplyr::mutate(cancer="All Cancer Sites")%>%
  dplyr::mutate(w=sum(w)/n())%>%
  select(-AD_sum,-py)%>%
  
  ungroup()%>%
  distinct()%>%
  as.data.frame()


AD_by_HDI_all_alt2<-AD_by_HDI_all_alt%>%
  arrange(hdi_group)%>%
  dplyr::mutate(across(2:5, round, -2))%>%
  dplyr::mutate(across(8:13, round, 1))%>%
  dplyr::mutate(across(14:15, round, -2))%>%
  dplyr::mutate(pAD_prev_tot=AD_prev/sum(total_deaths))%>%
  dplyr::mutate(pAD_treat_tot=AD_treat/sum(total_deaths))%>%
  dplyr::mutate(pAD_treat_prev_tot=AD_treat_prev/sum(total_deaths))%>%
  dplyr::mutate(across(16:19, round,3)*100)%>% #dplyr::mutate to show proportion as percentage in export
  dplyr::mutate(across(21:23, round,3)*100)%>% #dplyr::mutate to show proportion as percentage in export
  select( "hdi_group",      "cancer", 
          "AD_prev",        "pAD_prev",    "AD_prev.asr",
          "AD_treat",       "pAD_treat" ,"AD_treat.asr",
          "AD_treat_prev",  "pAD_treat_prev","AD_treat_prev.asr",
          "AD_unavoid",     "pAD_unavoid" ,   "AD_unavoid.asr",    
          "total_deaths","total.deaths.asr",
          "pAD_prev_tot","pAD_treat_tot", "pAD_treat_prev_tot")



# Pie Chart

library(RColorBrewer) 

getPalette = colorRampPalette(brewer.pal(9, "Set1"))
colourCount = 35


table_1_1_alt
# pie charts ----
# alternative pie charts script using some elements from Bar_and_Pie_base.R script using globocan colours
col <- read.csv("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\AD_PREV_TREAT\\Data\\cancer_color_2018.csv", sep=",",stringsAsFactors = F)




Avoidable_Deaths_Simulated_All %>% 
  filter(!country_label%in%c("China","India"))%>%
  group_by(cancer) %>% 
  dplyr::mutate(AD_treat_prev=sum(AD_treat, AD_prev,na.rm=T),
         AD_prev=sum(AD_prev,na.rm=T),
         AD_treat=sum(AD_treat,na.rm=T),
         total_deaths2=sum(total_deaths2,na.rm=T)) %>% 
  filter(!is.na(cancer_code)) %>% 
  select(cancer,cancer_code,AD_prev,AD_treat,AD_treat_prev,total_deaths2)%>% 
  unique() %>% 
  left_join(col %>% 
              select(cancer_label:Color.Hex) %>%
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
  dplyr::mutate(AD=case_when(rankc<29~sum(AD[rankc<29]),
                      TRUE~AD),
         percent=case_when(rankc<29~sum(percent[rankc<29]),
                           TRUE~percent), 
         rankc=case_when(rankc<29~1,
                         TRUE~rankc),
         Color.Hex=case_when(rankc==1~"#DCDCDC",
                             TRUE~Color.Hex),
         cancer=case_when(rankc==1~"Other sites",
                          TRUE~cancer)) %>% 
  select(cancer,Color.Hex,AD_cat,AD,percent,rankc) %>% 
  unique() -> AD_HDI_by_cancer_site_11_alt


Avoidable_Deaths_Simulated_All %>%
  filter(!country_label%in%c("China","India"))%>%
  filter(!is.na(cancer_code)) %>%
  group_by(cancer,cancer_code,hdi_group) %>% 
  dplyr::mutate(AD_treat_prev=sum(AD_treat, AD_prev,na.rm=T),
         AD_prev=sum(AD_prev,na.rm=T),
         AD_treat=sum(AD_treat,na.rm=T),
         total_deaths2=sum(total_deaths2,na.rm=T)) %>% 
  select(hdi_group, cancer,cancer_code,AD_prev,AD_treat,AD_treat_prev,total_deaths2)%>% 
  unique() %>% 
  left_join(col%>% select(cancer_label:Color.Hex)%>% filter(cancer_label!="Colorectum")) %>% 
  pivot_longer(AD_prev:total_deaths2,
               names_to="AD_cat",
               values_to = "AD") %>%
  group_by(AD_cat,hdi_group) %>%
  dplyr::mutate(percent=sum(AD),
         percent = AD/percent) %>% 
  arrange(AD) %>% 
  dplyr::mutate(rankc = as.numeric(dplyr::row_number())) %>% 
  group_by(AD_cat, hdi_group) %>% 
  dplyr::mutate(AD=case_when(rankc<29~sum(AD[rankc<29]),
                      TRUE~AD),
         percent=case_when(rankc<29~sum(percent[rankc<29]),
                           TRUE~percent), 
         rankc=case_when(rankc<29~1,
                         TRUE~rankc),
         Color.Hex=case_when(rankc==1~"#DCDCDC",
                             TRUE~Color.Hex),
         cancer=case_when(rankc==1~"Other sites",
                          TRUE~cancer)) %>% 
  select(hdi_group, cancer,Color.Hex,AD_cat,AD,percent,rankc) %>% 
  unique() -> AD_by_cancer_site_1_HDI


#preventable pie
piedp <- as.data.table(AD_HDI_by_cancer_site_11_alt %>%filter(AD_cat=="AD_prev") )
piedp %>%
  ggplot(aes(x = 2, y = percent, fill = factor(rankc,levels = unique(piedp$rankc),
                                               labels = unique(piedp$Color.Hex)), 
             width=2*sqrt(table_1_1_alt$AD_prev/table_1_1_alt$AD_treat_prev/pi))) +
  geom_bar(width = 1, stat = "identity") +
  geom_text(aes(label = paste0(formatC(round_any(AD,100), format="f", big.mark=",", digits=0),"\n ", 
                               scales::percent(percent, accuracy = 1)), x = 2.75),
            position = position_stack(vjust=0.5),
            size=3) +
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
  
  labs(title="Preventable", caption= paste(formatC(round(table_1_1_alt$AD_prev,-3), format="d", big.mark=",")," total deaths, ",round(100*table_1_1_alt$pAD_prev,2) ,"%"))-> pie.prev
pie.prev
# ggsave("pie.prev.png",pie.prev,width=5.43 ,height=2.43,
#        path ="\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\oliver_langselius\\AD_PREV_TREAT\\Figures") 

#treatable pie
piedt <- as.data.table(AD_HDI_by_cancer_site_11_alt %>%filter(AD_cat=="AD_treat") )
piedt %>%
  ggplot(aes(x = 2, y = percent, fill = factor(rankc,levels = unique(piedt$rankc),
                                               labels = unique(piedt$Color.Hex)),
           width=2*sqrt(table_1_1_alt$AD_treat/table_1_1_alt$AD_treat_prev/pi))) +
  geom_bar(width = 1, stat = "identity") +
  geom_text(aes(label = paste0(formatC(round_any(AD,100), format="f", big.mark=",", digits=0),"\n ", 
                               scales::percent(percent, accuracy = 1)), x = 2.75),
            position = position_stack(vjust=0.5),
            size=3) +
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
        plot.caption = element_text(hjust = 0.5, face = "italic"))+  # move caption to the left)+
  theme(legend.position = "none")+ 
  labs(title="Treatable", caption= paste(formatC(round(table_1_1_alt$AD_treat,-3), format="d", big.mark=",")," total deaths, ",round(100*table_1_1_alt$pAD_treat,2) ,"%"))-> pie.treat
pie.treat

# ggsave("pie.treat.png",pie.treat,width=5.43 ,height=2.43,
#        path ="\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\oliver_langselius\\AD_PREV_TREAT\\Figures") 

#avoidable pie
pieda <- as.data.table(AD_HDI_by_cancer_site_11_alt %>%filter(AD_cat=="AD_treat_prev") )
pieda %>%
  ggplot(aes(x = 2, y = percent, fill = factor(rankc,levels = unique(pieda$rankc),
                                               labels = unique(pieda$Color.Hex)), 
             width=2)) +
  geom_bar(width = 1, stat = "identity") +
  geom_text(aes(label = paste0(formatC(round_any(AD,100), format="f", big.mark=",", digits=0),"\n ", 
                               scales::percent(percent, accuracy = 1)), x = 2.75),
            position = position_stack(vjust=0.5),
            size=3) +
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
  labs(title="Avoidable", caption= paste(formatC(round(table_1_1_alt$AD_treat_prev,-3), format="d", big.mark=",")," total deaths, ",round(100*table_1_1_alt$pAD_treat_prev,2) ,"%"))-> pie.avoid
pie.avoid
# ggsave("pie.avoid.png",pie.avoid,width=5.43 ,height=2.43,
#        path ="\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\oliver_langselius\\AD_PREV_TREAT\\Figures") 


#arranging in grid
library(ggpubr)
library(gridExtra)
#http://www.sthda.com/english/wiki/wiki.php?id_contents=7930 code modified from here




# cancer_colors_pied<- cancer_colors%>%filter(cancer%in%pied2$cancer)
# palette1_named_pied =  setNames(object = cancer_colors_pied$Color.Hex, nm = cancer_colors_pied$cancer)
# 

#


#Creating combined legend 


pies91 <-   as.data.table(AD_by_cancer_site_labels)%>%
  select(-AD,-AD_cat,-rankc)%>%
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
  select(-AD,-AD_cat,-rankc)%>%
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
combined_plot <-   grid.arrange(pie.prev, pie.treat,pie.avoid, ncol = 3)
Top_4_cancer<- grid.arrange(combined_plot, legend, ncol = 2, widths= c(0.85, 0.15))

Top_4_cancer
#Saving the output
ggsave("pie.all_alternate.png",Top_4_cancer,width=15 ,height=10,
       path ="\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\oliver_langselius\\AD_PREV_TREAT\\Figures") 

#avoidable pie for HDI group 1

#avoidable pie for HDI group 1
pied1 <- as.data.table(AD_by_cancer_site_1_HDI %>%filter(AD_cat=="AD_treat_prev",hdi_group==1) )
pied1 %>%
  ggplot(aes(x = 2, y = percent, fill = factor(rankc,levels = unique(pied1$rankc),
                                               labels = unique(pied1$Color.Hex)))) +
  geom_bar(width = 1, stat = "identity") +
  geom_text(aes(label = paste0(formatC(round_any(AD,100), format="f", big.mark=",", digits=0),"\n ", 
                               scales::percent(percent, accuracy = 1)), x = 2.75),
            position = position_stack(vjust=0.5),
            size=2) +
  coord_polar(theta = "y") +
  scale_fill_identity("Cancer site", labels = pied1$cancer,
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
  labs(title="Low HDI", caption= paste(formatC(round(AD_by_HDI_all_alt2[1,]$AD_treat_prev,-3), format="d", big.mark=",")," total deaths, ", round(AD_by_HDI_all_alt2[1,]$pAD_treat_prev_tot, 2), "%")) -> pie.avoid.hdi1_alt
pie.avoid.hdi1_alt
# ggsave("pie.avoid.hdi1_alt.png",pie.avoid.hdi1_alt,width=5.43 ,height=2.43,
#        path ="\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\oliver_langselius\\AD_PREV_TREAT\\Figures") 


pied2 <- as.data.table(AD_by_cancer_site_1_HDI %>%filter(AD_cat=="AD_treat_prev",hdi_group==2) )
pied2 %>%
  ggplot(aes(x = 2, y = percent, fill = factor(rankc,levels = unique(pied2$rankc),
                                               labels = unique(pied2$Color.Hex)))) +
  geom_bar(width = 1, stat = "identity") +
  geom_text(aes(label = paste0(formatC(round_any(AD,100), format="f", big.mark=",", digits=0),"\n ", 
                               scales::percent(percent, accuracy = 1)), x = 2.75),
            position = position_stack(vjust=0.5),
            size=2) +
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
  labs(title="Medium HDI", caption= paste(formatC(round(AD_by_HDI_all_alt2[2,]$AD_treat_prev,-3), format="d", big.mark=",")," total deaths, ", round(AD_by_HDI_all_alt2[2,]$pAD_treat_prev_tot, 2), "%")) -> pie.avoid.hdi2_alt
pie.avoid.hdi2_alt
# ggsave("pie.avoid.hdi2_alt.png",pie.avoid.hdi2_alt,width=5.43 ,height=2.43,
#        path ="\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\oliver_langselius\\AD_PREV_TREAT\\Figures") 

pied3 <- as.data.table(AD_by_cancer_site_1_HDI %>%filter(AD_cat=="AD_treat_prev",hdi_group==3) )
pied3 %>%
  ggplot(aes(x = 2, y = percent, fill = factor(rankc,levels = unique(pied3$rankc),
                                               labels = unique(pied3$Color.Hex)))) +
  geom_bar(width = 1, stat = "identity") +
  geom_text(aes(label = paste0(formatC(round_any(AD,100), format="f", big.mark=",", digits=0),"\n ", 
                               scales::percent(percent, accuracy = 1)), x = 2.75),
            position = position_stack(vjust=0.5),
            size=2) +
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
  labs(title="High HDI", caption= paste(formatC(round(AD_by_HDI_all_alt2[3,]$AD_treat_prev,-3), format="d", big.mark=",")," total deaths, ", round(AD_by_HDI_all_alt2[3,]$pAD_treat_prev_tot, 2), "%")) -> pie.avoid.hdi3_alt
pie.avoid.hdi3_alt
# ggsave("pie.avoid.hdi3_alt.png",pie.avoid.hdi3_alt,width=5.43 ,height=2.43,
#        path ="\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\oliver_langselius\\AD_PREV_TREAT\\Figures") 


pied4 <- as.data.table(AD_by_cancer_site_1_HDI %>%filter(AD_cat=="AD_treat_prev",hdi_group==4) )
pied4 %>%
  ggplot(aes(x = 2, y = percent, fill = factor(rankc,levels = unique(pied4$rankc),
                                               labels = unique(pied4$Color.Hex)))) +
  geom_bar(width = 1, stat = "identity") +
  geom_text(aes(label = paste0(formatC(round_any(AD,100), format="f", big.mark=",", digits=0),"\n ", 
                               scales::percent(percent, accuracy = 1)), x = 2.75),
            position = position_stack(vjust=0.5),
            size=2) +
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
  labs(title="Very High HDI", caption= paste(formatC(round(AD_by_HDI_all_alt2[4,]$AD_treat_prev,-3), format="d", big.mark=",")," total deaths, ", round(AD_by_HDI_all_alt2[4,]$pAD_treat_prev_tot, 2), "%")) -> pie.avoid.hdi4_alt
pie.avoid.hdi4_alt
# ggsave("pie.avoid.hdi4_alt.png",pie.avoid.hdi4_alt,width=5.43 ,height=2.43,
#        path ="\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\oliver_langselius\\AD_PREV_TREAT\\Figures") 

#combined plot - edit below to match the HDI plots

#Creating combined legend 


pies10 <- as.data.table(AD_by_cancer_site_1_HDI)%>%
  select(-AD,-AD_cat,-rankc, -hdi_group)%>%
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
  mutate(rank=case_when( cancer=="Cervix uteri"  ~ 12,
                         cancer=="Breast"  ~ 11,
                         cancer=="Liver"  ~ 10,
                         cancer=="Stomach"  ~ 9,
                         cancer=="Colorectal"  ~ 8,
                         cancer=="Lung"  ~ 7,
                         cancer=="Lip, oral cavity"  ~ 6,
                       #  cancer=="Oesophagus"  ~ 5,
                         cancer=="Pancreas"  ~ 4,
                         cancer=="Other sites"  ~ 1,
  ))%>%
  filter(!cancer%in%c("Prostate", "Leukaemia", "Non-Hodgkin lymphoma", "Oesophagus"))%>%
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

combined_plothdi <-   grid.arrange(pie.avoid.hdi1_alt, 
                                   pie.avoid.hdi2_alt, 
                                   pie.avoid.hdi3_alt, 
                                   pie.avoid.hdi4_alt, ncol = 2,nrow=2)

Top_4_cancerhdi <-    grid.arrange(combined_plothdi, 
                                   legend2, 
                                   ncol = 2, 
                                   widths= c(0.85, 0.15))

Top_4_cancerhdi

#Saving the output
ggsave("piesHDI_alternate.png",Top_4_cancerhdi,width=15 ,height=10,
       path ="\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\oliver_langselius\\AD_PREV_TREAT\\Figures") 

