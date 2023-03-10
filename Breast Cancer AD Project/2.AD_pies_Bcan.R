
# load libraries needed for some charts
library(plyr) #added plyr here for rounding for the pie charts but it might break some tidyverse functions
library(tidyverse)
library(data.table)
library(Rcan)
library(ggrepel)
library("ggplot2")
library(ggsci)
library(grid)
text_high <- textGrob("Highest\nvalue", gp=gpar(fontsize=13, fontface="bold"))
text_low <- textGrob("Lowest\nvalue", gp=gpar(fontsize=13, fontface="bold"))
AD_all2
AD_region2
AD_HDI2
Avoidable_Deaths_age_cat2




# Pie Charts

AD_HDI_by_cancer_site_3<-AD_HDI %>%
  filter(age_cat=="Overall")%>%
  select(hdi_group,cancer, cancer_code, AD_treat)%>%
  dplyr::rename("AD"="AD_treat")%>%
  dplyr::mutate(AD_cat="Treatable")

AD_HDI_by_cancer_site_2<-AD_HDI %>%
  filter(age_cat=="Overall")%>%
  select(hdi_group,cancer, cancer_code, AD_prev)%>%
  dplyr::rename("AD"="AD_prev")%>%
  dplyr::mutate(AD_cat="Preventable")%>%
  dplyr::mutate(hdi_group=as.numeric(hdi_group))

AD_HDI_by_cancer_site_1<-AD_HDI %>%
  filter(age_cat=="Overall")%>%
  select(hdi_group, cancer, cancer_code, AD_treat_prev)%>%
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
  select(cancer_code)%>%
  dplyr::mutate(max="max")


getPalette = colorRampPalette(brewer.pal(9, "Set1"))
colourCount = 35




# Pie Chart

library(RColorBrewer) 

getPalette = colorRampPalette(brewer.pal(9, "Set1"))
colourCount = 35


table_1_1
# pie charts ----
# alternative pie charts script using some elements from Bar_and_Pie_base.R script using globocan colours
col <- read.csv("~/Documents/R_Projects/Data/cancer_color_2018.csv", sep=",",stringsAsFactors = F)

view(col)

Avoidable_Deaths_Simulated_All %>% 
  group_by(cancer) %>% 
  dplyr::mutate(AD_treat_prev=sum(AD_treat, AD_prev,na.rm=T),
         AD_prev=sum(AD_prev,na.rm=T),
         AD_treat=sum(AD_treat,na.rm=T),
         Expect_deaths=sum(Expect_deaths,na.rm=T)) %>% 
  filter(!is.na(cancer_code)) %>% 
  select(cancer,cancer_code,AD_prev,AD_treat,AD_treat_prev,Expect_deaths)%>% 
  unique() %>% 
  left_join(col %>% 
              select(cancer_label:Color.Hex) %>%
              filter(cancer_label!="Colorectum")) %>% 
  pivot_longer(AD_prev:Expect_deaths,
               names_to="AD_cat",
               values_to = "AD") %>%
  group_by(AD_cat) %>% 
  dplyr::mutate(percent=sum(AD),
         percent = AD/percent) %>% 
  dplyr::arrange(AD) %>% 
  dplyr::mutate(rankc = as.numeric(dplyr::row_number())) %>% 
  group_by(AD_cat) %>% 
  dplyr::mutate(AD=case_when(rankc<30~sum(AD[rankc<30]),
                      TRUE~AD),
         percent=case_when(rankc<30~sum(percent[rankc<30]),
                           TRUE~percent), 
         rankc=case_when(rankc<30~1,
                         TRUE~rankc),
         Color.Hex=case_when(rankc==1~"#DCDCDC",
                             TRUE~Color.Hex),
         cancer=case_when(rankc==1~"Other countries",
                          TRUE~cancer)) %>% 
  select(cancer,Color.Hex,AD_cat,AD,percent,rankc) %>% 
  unique() -> AD_by_cancer_site_1


Avoidable_Deaths_Simulated_All %>%
  filter(!is.na(cancer_code)) %>%
  group_by(cancer,cancer_code,hdi_group) %>% 
  dplyr::mutate(AD_treat_prev=sum(AD_treat, AD_prev,na.rm=T),
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
  dplyr::mutate(percent=sum(AD),
         percent = AD/percent) %>% 
  arrange(AD) %>% 
  dplyr::mutate(rankc = as.numeric(dplyr::row_number())) %>% 
  group_by(AD_cat, hdi_group) %>% 
  dplyr::mutate(AD=case_when(rankc<30~sum(AD[rankc<30]),
                      TRUE~AD),
         percent=case_when(rankc<30~sum(percent[rankc<30]),
                           TRUE~percent), 
         rankc=case_when(rankc<30~1,
                         TRUE~rankc),
         Color.Hex=case_when(rankc==1~"#DCDCDC",
                             TRUE~Color.Hex),
         cancer=case_when(rankc==1~"Other countries",
                          TRUE~cancer)) %>% 
  select(hdi_group, cancer,Color.Hex,AD_cat,AD,percent,rankc) %>% 
  unique() -> AD_by_cancer_site_1_HDI


#preventable pie
piedp <- as.data.table(AD_by_cancer_site_1 %>%filter(AD_cat=="AD_prev") )
piedp %>%
  ggplot(aes(x = 2, y = percent, fill = factor(rankc,levels = unique(piedp$rankc),
                                               labels = unique(piedp$Color.Hex)), 
             width=2*sqrt(table_1_1$AD_prev/table_1_1$AD_treat_prev/pi))) +
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
  
  labs(title="Preventable", caption= paste(formatC(round(table_1_1$AD_prev,-3), format="d", big.mark=",")," total deaths"))-> pie.prev
pie.prev
ggsave("pie.prev.pdf",pie.prev,width=5.43 ,height=2.43)

#treatable pie
piedt <- as.data.table(AD_by_cancer_site_1 %>%filter(AD_cat=="AD_treat") )
piedt %>%
  ggplot(aes(x = 2, y = percent, fill = factor(rankc,levels = unique(piedt$rankc),
                                               labels = unique(piedt$Color.Hex)),
           width=2*sqrt(table_1_1$AD_treat/table_1_1$AD_treat_prev/pi))) +
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
        plot.caption = element_text(hjust = 0.5, face = "italic"))+# move caption to the left)+
  theme(legend.position = "none")+ 
  labs(title="Treatable", caption= paste(formatC(round(table_1_1$AD_treat,-3), format="d", big.mark=",")," total deaths"))-> pie.treat
pie.treat

ggsave("pie.treat.pdf",pie.treat,width=5.43 ,height=2.43)

#avoidable pie
pieda <- as.data.table(AD_by_cancer_site_1 %>%filter(AD_cat=="AD_treat_prev") )
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
  labs(title="Avoidable", caption= paste(formatC(round(table_1_1$AD_treat_prev,-3), format="d", big.mark=",")," total deaths"))-> pie.avoid
pie.avoid
ggsave("pie.avoid.pdf",pie.avoid,width=5.43 ,height=2.43)


#arranging in grid
library(ggpubr)
library(gridExtra)
#http://www.sthda.com/english/wiki/wiki.php?id_contents=7930 code modified from here




cancer_colors_pied<- cancer_colors%>%filter(cancer%in%pied2$cancer)
palette1_named_pied =  setNames(object = cancer_colors_pied$Color.Hex, nm = cancer_colors_pied$cancer)


#

#Creating combined legend 


pies91 <-   as.data.table(AD_by_cancer_site_1)%>%
  select(-AD,-AD_cat,-rankc)%>%
  distinct()%>%
  group_by(cancer)%>%
  mutate(percent=sum(percent))%>%
    ungroup()%>%
    distinct()%>%
  dplyr::mutate(rankc=row_number())%>%
  arrange(desc(cancer))%>%
  filter(cancer!="Other countries")

pies9 <-   as.data.table(AD_by_cancer_site_1)%>%
  select(-AD,-AD_cat,-rankc)%>%
  distinct()%>%
  group_by(cancer)%>%
  mutate(percent=sum(percent))%>%
  ungroup()%>%
  distinct()%>%
  dplyr::mutate(rankc=row_number())%>%
  arrange(desc(cancer))%>%
  filter(cancer=="Other countries")%>%
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
ggsave("pie.all.pdf",Top_4_cancer,width=15 ,height=10)
