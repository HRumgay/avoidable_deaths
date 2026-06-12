#Regional UVR cases for melanoma

setwd("C:\\Users\\langseliuso\\OneDrive - International Agency for Research on Cancer\\Documents\\GitHub\\avoidable_deaths\\AD Preventable and Treatable")

# load libraries needed for some charts
library(plyr) #added plyr here for rounding for the pie charts but it might break some tidyverse functions
library(tidyverse)
library(data.table)
library(Rcan)
library(ggrepel)
library(ggplot2)
library(ggsci)
library(grid)
library(RColorBrewer)

text_high <- textGrob("Highest\nvalue", gp=gpar(fontsize=13, fontface="bold"))
text_low <- textGrob("Lowest\nvalue", gp=gpar(fontsize=13, fontface="bold"))

#PAF_uv_Region2



# Pie Charts

PAF_uv_region2<-AD_Region2%>%
  filter(age_cat=="Overall")%>%
  as.data.frame()

# Pie Chart

library(RColorBrewer) 

#getPalette = colorRampPalette(brewer.pal(8, "Set1"))

# pie charts ----
# alternative pie charts script using some elements from Bar_and_Pie_base.R script using globocan colours
col <- read.csv("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\_work\\Oliver_Langselius\\AD_PREV_TREAT\\Data\\cancer_color_2018.csv", sep=",",stringsAsFactors = F)

PAF_uv_region2

PAF_uv_region2 %>% 
  as.data.frame()%>%
  filter(age_cat=="Overall")%>%
  #dplyr::filter(sex==3)%>%
  ungroup()%>%
  #group_by(sex)%>% 
  dplyr::mutate(percent=sum(AD_treat_prev),
         percent =case_when(percent != 0 ~ AD_treat_prev/percent,
                            percent == 0 ~ 0)) %>% 
  dplyr::arrange(AD_treat_prev) %>% 
  dplyr::mutate(rankc = as.numeric(dplyr::row_number())) %>% 
  group_by(country_label) %>%
  dplyr::select(age_cat, country_label, AD_treat_prev, percent,rankc) %>% 
  unique()%>%
  as.data.frame()-> PAF_UV_Pie2

PAF_other<-PAF_UV_Pie2%>% # Error here somewhere as same numbers produced on each category
  as.data.frame()%>%
  filter(percent<0.015)%>%
  ungroup()%>%
  mutate(country_label="Other regions")%>%
  as.data.frame()%>%
#  group_by(sex)%>%
  as.data.frame()%>%
  #  group_by(sex)%>%
  dplyr::mutate(percent=sum(percent))%>%
  dplyr::mutate(AD_treat_prev=sum(AD_treat_prev))%>%
  mutate(rankc=21-11)%>%
  distinct()

PAF_other
  
  
PAF_UV_Pie<-PAF_UV_Pie2%>%
  filter(percent>=0.015)%>%
  arrange(percent)


PAF_UV_Pie<-PAF_other%>%full_join(PAF_UV_Pie)

#Overall
piedp_overall <- PAF_UV_Pie %>%
#  filter(sex==3)%>%
  mutate(country_label = factor(country_label, levels = country_label))%>%
  as.data.table()

piedp_overall$country_label<-as.factor(piedp_overall$country_label)


  
  
# Define the number of colors you want
nb.cols <- 12
mycolors <- colorRampPalette(brewer.pal(12, "Paired"))(nb.cols)

# Barplot
pafpie<-piedp_overall%>%
  filter(percent!=0)%>%
  ggplot(aes(x="", y=percent, fill=country_label, width=2))+
  geom_bar(width = 1, stat = "identity")+
  coord_polar("y", start=0)+
  geom_text_repel(aes(label = paste0(formatC(round_any(AD_treat_prev,100), format="f", big.mark=",", digits=0),"\n ", 
                                     scales::percent(percent, accuracy = 1)), x = 2.75),
                  position = position_stack(vjust=0.5),
                  size=5)+
  guides(fill = guide_legend(reverse = TRUE, title= "Region"))+
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
scale_fill_manual(values = mycolors) +
  
  theme(legend.position="none") + 
#  scale_fill_brewer(palette="Dark2")+
 # labs(title="Both Sexes", caption= paste(formatC(round(PAF_uv_total2_nordcan[12,]$cases.prev,-3), format="d", big.mark=","),"UVR attributable cases
#out of",formatC(round(PAF_uv_total2_nordcan[12,]$cases,-3), format="d", big.mark=",")))+
  theme(plot.caption = element_text(hjust = 0.5, face = "italic", size = 18))+
  theme(plot.title = element_text(size = 18))
#-> pie.prev

pafpie

#ggsave("paf_pie.pdf", width = 10, height =10, limitsize = FALSE,plot=pafpie,path ="\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\_work\\oliver_langselius\\Melanoma\\Figures") 

# add by cat

piedp_treat <- PAF_UV_Pie %>%
  filter(sex==1)%>%
  mutate(country_label = factor(country_label, levels = country_label))%>%
  as.data.table()

piedp_treat$country_label<-as.factor(piedp_treat$country_label)




# Define the number of colors you want
nb.cols <- 12
mycolors <- colorRampPalette(brewer.pal(12, "Paired"))(nb.cols)

# Barplot
pafpie_male<-piedp_treat%>%
  filter(percent!=0)%>%
  ggplot(aes(x="", y=percent, fill=country_label,
            # width=2*2*sqrt(PAF_uv_total2_nordcan[10,]$cases.prev/PAF_uv_total2_nordcan[12,]$cases.prev/pi)
             ))+
  geom_bar(width = 1, stat = "identity")+
  coord_polar("y", start=0)+
  geom_text_repel(aes(label = paste0(formatC(round_any(AD_treat,100), format="f", big.mark=",", digits=0),"\n ", 
                                     scales::percent(percent, accuracy = 1)), x = 2.75),
                  position = position_stack(vjust=0.5),
                  size=5)+
  guides(fill = guide_legend(reverse = TRUE, title= "Region"))+
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
  scale_fill_manual(values = mycolors) +
  
  theme(legend.position="none") + 
  #  scale_fill_brewer(palette="Dark2")+
#  labs(title="Male", caption= paste(formatC(round(PAF_uv_total2_nordcan[10,]$cases.prev,-3), format="d", big.mark=","),"UVR attributable cases
#out of", formatC(round(PAF_uv_total2_nordcan[10,]$cases,-3), format="d", big.mark=",")))+
  theme(plot.caption = element_text(hjust = 0.5, face = "italic", size = 18))+
  theme(plot.title = element_text(size = 18))

pafpie_male

#female
piedp_female <- PAF_UV_Pie %>%
  filter(sex==2)%>%
  mutate(country_label = factor(country_label, levels = country_label))%>%
  as.data.table()

piedp_female$country_label<-as.factor(piedp_female$country_label)



# Define the number of colors you want
nb.cols <- 11
mycolors <- colorRampPalette(brewer.pal(12, "Paired"))(nb.cols)

# Barplot
pafpie_female<-piedp_female%>%
  filter(percent!=0)%>%
  ggplot(aes(x="", y=percent, fill=country_label,
             width=2*2*sqrt(PAF_uv_total2_nordcan[11,]$cases.prev/PAF_uv_total2_nordcan[12,]$cases.prev/pi)))+ #factor of two to scale the plots to look nicer
  geom_bar(width = 1, stat = "identity")+
  coord_polar("y", start=0)+
geom_text_repel(aes(label = paste0(formatC(round_any(cases.prev,100), format="f", big.mark=",", digits=0),"\n ", 
                                   scales::percent(percent, accuracy = 1)), x = 2.75),
                position = position_stack(vjust=0.5),
                size=5)+
  guides(fill = guide_legend(reverse = TRUE, title= "Region"))+
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
  scale_fill_manual(values = mycolors) +
  theme(legend.position="none") + 
  #  scale_fill_brewer(palette="Dark2")+
  labs(title="Female", caption= paste(formatC(round(PAF_uv_total2_nordcan[11,]$cases.prev,-3), format="d", big.mark=","),"UVR attributable cases
out of", formatC(round(PAF_uv_total2_nordcan[11,]$cases,-3), format="d", big.mark=",")))+
  theme(plot.caption = element_text(hjust = 0.5, face = "italic", size = 18))+
  theme(plot.title = element_text(size = 18))
#-> pie.prev

pafpie_female




#arranging in grid
library(ggpubr)
library(gridExtra)
#http://www.sthda.com/english/wiki/wiki.php?id_contents=7930 code modified from here



#plot1_legend<-
piedp_female%>%
  filter(percent!=0)%>%
  ggplot(aes(x="", y=percent, fill=country_label))+
  geom_bar(width = 1, stat = "identity")+
  coord_polar("y", start=0)+
  geom_text(aes(label = paste0(formatC(round_any(cases.prev,100), format="f", big.mark=",", digits=0),"\n ", 
                               scales::percent(percent, accuracy = 1)), x = 2.75),
            position = position_stack(vjust=0.5),
            size=5) +
  guides(fill = guide_legend(reverse = TRUE, title= "Region"))+
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
  scale_fill_manual(values = mycolors) ->plot1_legend
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
combined_plot <-   ggarrange(pafpie, 
                                pafpie_male,
                                pafpie_female,
                                ncol = 3,
                             labels =c("A)",  "B)", "C)"),
                             font.label = list(size = 45, color = "black"))


PAF.pies.c<- grid.arrange(combined_plot, legend, ncol = 2, widths= c(0.85, 0.15)#,
                          #top=textGrob("Ultraviolet (UVR) attributable melanoma cancer cases in 2022", gp=gpar(fontsize=20,font=3))
                          )

ggsave("paf_pie.pdf", width = 20, height =9,dpi=700, limitsize = FALSE,plot=PAF.pies.c,path ="\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\_work\\oliver_langselius\\Melanoma\\Figures") 

