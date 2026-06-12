## script to plot maps of cancer cases and deaths 2020

library(data.table)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggrepel)
library(terra)
library(sf)
library(ggpubr)


setwd("C:\\Users\\langseliuso\\OneDrive - International Agency for Research on Cancer\\Documents\\GitHub\\avoidable_deaths\\Gastric_cancer")

AD_country_all_cancers %>% select(country_label)%>% distinct()


# load AD results


AD_country_all_cancers2_editable

#I:\Studies\Survival\SurvCan\Data\Oliver_Langselius\AD_PREV_TREAT\Data

AD_Map <- as.data.table(AD_country_all_cancers2_editable)

# load id for each country

dict_id <-  as.data.table(read.csv("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\_work\\Oliver_Langselius\\AD_PREV_TREAT\\Data\\_shape\\id_OMS_official_general_map.csv", sep=","))
dict_id %>% dplyr::select(-country_label)-> dict_id

# merge paf data with dict_id
AD_Map <- merge(AD_Map, dict_id, by = c("country_code"), all.x = T) 

#------ map shape ------------------

# create a blank ggplot theme
theme_opts <- list(theme(panel.grid.minor = element_blank(),
                         panel.grid.major = element_blank(),
                         panel.background = element_blank(),
                         plot.background = element_blank(),
                         panel.border = element_blank(),
                         axis.line = element_blank(),
                         axis.text.x = element_blank(),
                         axis.text.y = element_blank(),
                         axis.ticks = element_blank(),
                         axis.title.x = element_blank(),
                         axis.title.y = element_blank(),
                         legend.position=c(.5, -0.05),
                         legend.key.width= unit(2, "cm"), #1
                         legend.key.height= unit(0.4, "cm"), #0.2
                         plot.title = element_text(size=18)))

# projection for area
proj <- "+proj=eck3"

# read shapefile using terra/sf
df_map <- vect("//inti/cin/Xchange/Mathieu/data/_shape/OMS_official_general", layer="general_2013")
df_map <- st_as_sf(df_map)
df_map$id = c(0:(nrow(df_map)-1))
df_map$int_map_index = c(1:(nrow(df_map)))

# Rename UN_CODE to country_code for consistency
names(df_map)[names(df_map) == "UN_CODE"] <- "country_code"

# Update China's country code from 156 to 160
df_map$country_code[df_map$country_code == 156] <- 160

df_poly <- vect("//inti/cin/Xchange/Mathieu/data/_shape/OMS_official_general", layer="maskpoly_general_2013")
df_poly <- st_as_sf(df_poly)
df_poly$id = c(0:(nrow(df_poly)-1))

df_poly_layout <-  read.csv("//inti/cin/Xchange/Mathieu/data/_shape/id_OMS_official_general_poly.csv", sep=",")
df_poly_layout$line_color <- as.factor(df_poly_layout$line_color)
df_poly_layout$line_type <- as.factor(df_poly_layout$line_type)
df_poly_layout$poly_fill <- as.factor(df_poly_layout$poly_fill)
df_poly <-  merge(df_poly, df_poly_layout, by = c("id"), all.x=TRUE, sort=F)
df_poly<- df_poly[order(df_poly$id),]

color_official <- c("grey100","#d6d6d6")

df_line <- vect("//inti/cin/Xchange/Mathieu/data/_shape/OMS_official_general", layer="maskline_general_2013")
df_line <- st_as_sf(df_line)
df_line$id = c(0:(nrow(df_line)-1))

df_line_layout <-  read.csv("//inti/cin/Xchange/Mathieu/data/_shape/id_OMS_official_general_line.csv", sep=",")
df_line_layout$line_color <- as.factor(df_line_layout$line_color)
df_line_layout$line_type <- as.factor(df_line_layout$line_type)
df_line <-  merge(df_line, df_line_layout, by = c("id"), all.x=TRUE, sort=F)
df_line<- df_line[order(df_line$id),]

# color vectors ----

colors_blue_GCO<-  c( "#084594","#2171b5", "#4292c6","#6baed6","#9ecae1", "#c6dbef","#deebf7", "#f7fbff") # colours for 8 bins

colors_green_GCO <- c("#005a32", "#238b45", "#41ab5d","#74c476", "#a1d99b","#c7e9c0", "#e5f5e0", "#f7fcf5") 

colors_red_GCO <- c("#99000d","#cb181d","#ef3b2c","#fb6a4a", "#fc9272", "#fcbba1", "#fee0d2", "#fff5f0") 


#--- maps for males----

allc <- AD_Map[sex == 1,]

break_quantile <- unique(quantile(allc$pAD_treat_prev, probs = seq(0, 1, by = 0.125), na.rm = T))

allc$cutpoint <- cut(allc$pAD_treat_prev, breaks = rev(break_quantile), include.lowest = TRUE)
table(allc$cutpoint)
allc$cutpoint <- factor(allc$cutpoint, levels = rev(levels(allc$cutpoint)))

#merge with shapefile
df_data_map <- merge(df_map, allc, by = c("country_code"), all.x=TRUE, sort=F )
df_data_map<- df_data_map[order(df_data_map$int_map_index),]

#For the automatic legend
break_quantile_t <- paste(round(break_quantile,2),"", sep="")

labels_leg <-  c( paste("< ", break_quantile_t[2]),
                  paste(break_quantile_t[2], " - ", break_quantile_t[3]),
                  paste(break_quantile_t[3], " - ", break_quantile_t[4]),
                  paste(break_quantile_t[4], " - ", break_quantile_t[5]),
                  paste(break_quantile_t[5], " - ", break_quantile_t[6]),
                  paste(break_quantile_t[6], " - ", break_quantile_t[7]),
                  paste(break_quantile_t[7], " - ", break_quantile_t[8]),
                  paste(">= ", break_quantile_t[8]))


labels_leg <- rev(labels_leg)

ggplot() +
  geom_sf(data=df_data_map[!is.na(df_data_map$cutpoint),],
          aes(fill=cutpoint),
          colour="grey10",
          size = 0.4,
          show.legend=TRUE)+
  geom_sf(data=df_data_map[is.na(df_data_map$cutpoint),],
          fill = "#cccccc",
          colour="grey10",
          size = 0.4,
          show.legend=FALSE)+
  geom_sf(data=df_data_map[df_data_map$id == 82,],
          aes(fill=cutpoint),
          colour="grey10",
          size = 0.4,
          show.legend=FALSE)+
  geom_sf(data=df_map[df_map$id == 59,],
          fill = "#d6d6d6",
          colour="grey10",
          size = 0.4,
          show.legend=FALSE)+
  geom_sf(data=df_poly[df_poly$poly_fill == 888,], fill = "grey100",
          show.legend=FALSE)+
  geom_sf(data=df_poly[df_poly$poly_fill == 999,], fill = "#d6d6d6",
          show.legend=FALSE)+
  geom_sf(data=df_poly,aes( color=line_color,linetype=line_type), size = 0.4,
          show.legend=FALSE)+
  geom_sf(data=df_line[!is.na(df_line$line_color), ],aes( color=line_color,linetype=line_type),
          size = 0.4,
          show.legend=FALSE)+
  coord_sf(crs = proj) +
  theme_opts +
  theme(legend.key.width= unit(2.6, "cm"),
        legend.key.height= unit(1.4, "cm"),
        legend.direction= "vertical",
        legend.text = element_text(size=20),
        legend.title = element_text(size=20, hjust = 1),
        legend.title.align=0.5,
        legend.position =c(0.18, -0.02),
        legend.background = element_rect(fill="transparent"),
        plot.margin = unit(c(0,0,0,0),"lines"))+
  scale_fill_manual(name = str_wrap("Proportion of deaths avoidable through primary prevention and early detection and treatment combined (%), males", 25),
                    
                    values= colors_green_GCO,
                    labels= labels_leg,
                    na.value = "#cccccc",
                    drop=FALSE)+
  guides(fill = guide_legend(reverse = FALSE))+
  scale_color_manual(values=c("grey100", "grey10", "grey50"), guide="none")+
  scale_linetype_manual(values=c("solid", "11", "dashed"), guide="none") -> AD_map_prev
# 
# ggsave("map_AD_stomach_prop_male.png",width = 40, height = 30, pointsize = 12,
#        path ="\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\_work\\oliver_langselius\\AD_PREV_TREAT\\Gastric2024")


#--- maps for females---

allc <- AD_Map[sex == 2,]

break_quantile <- unique(quantile(allc$pAD_treat_prev, probs = seq(0, 1, by = 0.125), na.rm = T))

allc$cutpoint <- cut(allc$pAD_treat_prev, breaks = rev(break_quantile), include.lowest = TRUE)
table(allc$cutpoint)
allc$cutpoint <- factor(allc$cutpoint, levels = rev(levels(allc$cutpoint)))

#merge with shapefile
df_data_map <- merge(df_map, allc, by = c("country_code"), all.x=TRUE, sort=F )
df_data_map<- df_data_map[order(df_data_map$int_map_index),]

#For the automatic legend
break_quantile_t <- paste(round(break_quantile,2),"", sep="")

labels_leg <-  c( paste("< ", break_quantile_t[2]),
                  paste(break_quantile_t[2], " - ", break_quantile_t[3]),
                  paste(break_quantile_t[3], " - ", break_quantile_t[4]),
                  paste(break_quantile_t[4], " - ", break_quantile_t[5]),
                  paste(break_quantile_t[5], " - ", break_quantile_t[6]),
                  paste(break_quantile_t[6], " - ", break_quantile_t[7]),
                  paste(break_quantile_t[7], " - ", break_quantile_t[8]),
                  paste(">= ", break_quantile_t[8]))


labels_leg <- rev(labels_leg)

ggplot() +
  geom_sf(data=df_data_map[!is.na(df_data_map$cutpoint),],
          aes(fill=cutpoint),
          colour="grey10",
          size = 0.4,
          show.legend=TRUE)+
  geom_sf(data=df_data_map[is.na(df_data_map$cutpoint),],
          fill = "#cccccc",
          colour="grey10",
          size = 0.4,
          show.legend=FALSE)+
  geom_sf(data=df_data_map[df_data_map$id == 82,],
          aes(fill=cutpoint),
          colour="grey10",
          size = 0.4,
          show.legend=FALSE)+
  geom_sf(data=df_map[df_map$id == 59,],
          fill = "#d6d6d6",
          colour="grey10",
          size = 0.4,
          show.legend=FALSE)+
  geom_sf(data=df_poly[df_poly$poly_fill == 888,], fill = "grey100",
          show.legend=FALSE)+
  geom_sf(data=df_poly[df_poly$poly_fill == 999,], fill = "#d6d6d6",
          show.legend=FALSE)+
  geom_sf(data=df_poly,aes( color=line_color,linetype=line_type), size = 0.4,
          show.legend=FALSE)+
  geom_sf(data=df_line[!is.na(df_line$line_color), ],aes( color=line_color,linetype=line_type),
          size = 0.4,
          show.legend=FALSE)+
  coord_sf(crs = proj) +
  theme_opts +
  theme(legend.key.width= unit(2.6, "cm"),
        legend.key.height= unit(1.4, "cm"),
        legend.direction= "vertical",
        legend.text = element_text(size=20),
        legend.title = element_text(size=20, hjust = 1),
        legend.title.align=0.5,
        legend.position =c(0.18, -0.02),
        legend.background = element_rect(fill="transparent"),
        plot.margin = unit(c(0,0,0,0),"lines"))+
  scale_fill_manual(name = str_wrap("Proportion of deaths avoidable through primary prevention and early detection and treatment combined (%), females", 25),
                    
                    values= colors_green_GCO,
                    labels= labels_leg,
                    na.value = "#cccccc",
                    drop=FALSE)+
  guides(fill = guide_legend(reverse = FALSE))+
  scale_color_manual(values=c("grey100", "grey10", "grey50"), guide="none")+
  scale_linetype_manual(values=c("solid", "11", "dashed"), guide="none") -> AD_map_treatable

ggsave("map_AD_all_cancers_prop_female.png",width = 40, height = 30, pointsize = 12,
       path ="\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\_work\\oliver_langselius\\AD_PREV_TREAT\\Gastric2024") 



#--- maps for Overall sex

allc <- AD_Map[sex == 0,]

break_quantile <- unique(quantile(allc$pAD_treat_prev, probs = seq(0, 1, by = 0.125), na.rm = T))

allc$cutpoint <- cut(allc$pAD_treat_prev, breaks = rev(break_quantile), include.lowest = TRUE)
table(allc$cutpoint)
allc$cutpoint <- factor(allc$cutpoint, levels = rev(levels(allc$cutpoint)))

#merge with shapefile
df_data_map <- merge(df_map, allc, by = c("country_code"), all.x=TRUE, sort=F )
df_data_map<- df_data_map[order(df_data_map$int_map_index),]

#For the automatic legend
break_quantile_t <- paste(round(break_quantile,2),"", sep="")

labels_leg <-  c( paste("< ", break_quantile_t[2]),
                  paste(break_quantile_t[2], " - ", break_quantile_t[3]),
                  paste(break_quantile_t[3], " - ", break_quantile_t[4]),
                  paste(break_quantile_t[4], " - ", break_quantile_t[5]),
                  paste(break_quantile_t[5], " - ", break_quantile_t[6]),
                  paste(break_quantile_t[6], " - ", break_quantile_t[7]),
                  paste(break_quantile_t[7], " - ", break_quantile_t[8]),
                  paste(">= ", break_quantile_t[8]))


labels_leg <- rev(labels_leg)

ggplot() +
  geom_sf(data=df_data_map[!is.na(df_data_map$cutpoint),],
          aes(fill=cutpoint),
          colour="grey10",
          size = 0.4,
          show.legend=TRUE)+
  geom_sf(data=df_data_map[is.na(df_data_map$cutpoint),],
          fill = "#cccccc",
          colour="grey10",
          size = 0.4,
          show.legend=FALSE)+
  geom_sf(data=df_data_map[df_data_map$id == 82,],
          aes(fill=cutpoint),
          colour="grey10",
          size = 0.4,
          show.legend=FALSE)+
  geom_sf(data=df_map[df_map$id == 59,],
          fill = "#d6d6d6",
          colour="grey10",
          size = 0.4,
          show.legend=FALSE)+
  geom_sf(data=df_poly[df_poly$poly_fill == 888,], fill = "grey100",
          show.legend=FALSE)+
  geom_sf(data=df_poly[df_poly$poly_fill == 999,], fill = "#d6d6d6",
          show.legend=FALSE)+
  geom_sf(data=df_poly,aes( color=line_color,linetype=line_type), size = 0.4,
          show.legend=FALSE)+
  geom_sf(data=df_line[!is.na(df_line$line_color), ],aes( color=line_color,linetype=line_type),
          size = 0.4,
          show.legend=FALSE)+
  coord_sf(crs = proj) +
  theme_opts +
  theme(legend.key.width= unit(2.6, "cm"),
        legend.key.height= unit(1.4, "cm"),
        legend.direction= "vertical",
        legend.text = element_text(size=20),
        legend.title = element_text(size=20, hjust = 1),
        legend.title.align=0.5,
        legend.position =c(0.18, -0.02),
        legend.background = element_rect(fill="transparent"),
        plot.margin = unit(c(0,0,0,0),"lines"))+
  scale_fill_manual(name = str_wrap("Proportion of deaths avoidable through primary prevention and early detection and treatment combined (%), both sexes combined", 25),
                    values= colors_green_GCO,
                    labels= labels_leg,
                    na.value = "#cccccc",
                    drop=FALSE)+
  guides(fill = guide_legend(reverse = FALSE))+
  scale_color_manual(values=c("grey100", "grey10", "grey50"), guide="none")+
  scale_linetype_manual(values=c("solid", "11", "dashed"), guide="none") -> AD_map_total

# 
# ggsave("map_AD_all_cancers_prop_both_sexes.png",width = 40, height = 30, pointsize = 12,
#        path ="\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\_work\\oliver_langselius\\AD_PREV_TREAT\\Gastric2024") 


ggarrange(AD_map_prev, AD_map_treatable, AD_map_total,
          labels = c("a)", "b)", "c)"),
          ncol = 1, nrow = 3,
          font.label = list(size = 60, color = "black"))

ggsave("Figure_1_sex.pdf", width = 37*0.75, height =70*0.8, dpi=600, limitsize = FALSE,
       path ="\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\_work\\oliver_langselius\\AD_PREV_TREAT\\Gastric2024") 

ggsave("Figure_1_sex.png", width = 37*0.75, height =70*0.8, dpi=400, limitsize = FALSE,
       path ="\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\_work\\oliver_langselius\\AD_PREV_TREAT\\Gastric2024") 



