## script to plot maps of cancer cases and deaths 2020

library(data.table)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggrepel)
library(rgdal)
library(pastecs)
library(ggpubr)


AD_country_all_cancers%>%select(country_label)%>%distinct()


# load AD results

AD_country_all_cancers2



AD_Map <- as.data.table(AD_country_all_cancers2)

# load id for each country
dict_id <-  as.data.table(read.csv("~/Documents/R_Projects/Data/_shape/id_OMS_official_general_map.csv", sep=","))
dict_id %>% dplyr::select(-country_label)-> dict_id

# merge paf data with dict_id
AD_Map <- merge(AD_Map, dict_id, by = c("country_code"), all.x = T) 

#------ map shape ------------------

# create a blank ggplot theme
{ theme_opts <- list(theme(panel.grid.minor = element_blank(),
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

#http://projectionwizard.org/

#eckert:
temp_proj<- "+proj=eck3"

# read shapefile official general (3 part)

shp_temp <- readOGR(dsn="~/Documents/R_Projects/Data/_shape/OMS_official_general", layer="general_2013")
shp_temp <- spTransform(shp_temp, CRS("+proj=eck3"))
df_map <- fortify(shp_temp)
int_map_index <- c(1:nrow(df_map))
df_map$int_map_index <- int_map_index

shp_temp <- readOGR(dsn="~/Documents/R_Projects/Data/_shape/OMS_official_general", layer="maskpoly_general_2013")
shp_temp <- spTransform(shp_temp, CRS("+proj=eck3"))
df_poly <- fortify(shp_temp)
int_map_index <- c(1:nrow(df_poly))
df_poly$int_map_index <- int_map_index

df_poly_layout <-  read.csv("~/Documents/R_Projects/Data/_shape/id_OMS_official_general_poly.csv", sep=",")
df_poly_layout$line_color <- as.factor(df_poly_layout$line_color)
df_poly_layout$line_type <- as.factor(df_poly_layout$line_type)
df_poly_layout$poly_fill <- as.factor(df_poly_layout$poly_fill)
df_poly <-  merge(df_poly, df_poly_layout, by = c("id"), all.x=TRUE, sort=F)
df_poly<- df_poly[order(df_poly$int_map_index),]

color_official <- c("grey100","#d6d6d6")

shp_temp <- readOGR(dsn="~/Documents/R_Projects/Data/_shape/OMS_official_general", layer="maskline_general_2013")
shp_temp <- spTransform(shp_temp, CRS("+proj=eck3"))
df_line <- fortify(shp_temp)
int_map_index <- c(1:nrow(df_line))
df_line$int_map_index <- int_map_index

df_line_layout <-  read.csv("~/Documents/R_Projects/Data/_shape/id_OMS_official_general_line.csv", sep=",")
df_line_layout$line_color <- as.factor(df_line_layout$line_color)
df_line_layout$line_type <- as.factor(df_line_layout$line_type)
df_line <-  merge(df_line, df_line_layout, by = c("id"), all.x=TRUE, sort=F)
df_line<- df_line[order(df_line$int_map_index),] 
}

# color vectors ----

colors_blue_GCO<-  c( "#084594","#2171b5", "#4292c6","#6baed6","#9ecae1", "#c6dbef","#deebf7", "#f7fbff") # colours for 8 bins

colors_green_GCO <- c("#005a32", "#238b45", "#41ab5d","#74c476", "#a1d99b","#c7e9c0", "#e5f5e0", "#f7fcf5") 

colors_red_GCO <- c("#99000d","#cb181d","#ef3b2c","#fb6a4a", "#fc9272", "#fcbba1", "#fee0d2", "#fff5f0") 


#--- maps for Preventable AD----

allc <- AD_Map#[sex == 2,][type==1,]

break_quantile <- quantile(allc$pAD_prev, probs = seq(0, 1, by = 0.125), na.rm = T)

allc$cutpoint <- cut(allc$pAD_prev, breaks = rev(break_quantile), include.lowest = TRUE)
table(allc$cutpoint)
allc$cutpoint <- factor(allc$cutpoint, levels = rev(levels(allc$cutpoint)))

#merge with shapefile
df_AD_map <- merge(df_map, allc, by = c("id"), all.x=TRUE, sort=F )
df_AD_map<- df_AD_map[order(df_AD_map$int_map_index),]

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
  geom_polygon(data=df_AD_map,
               aes(x=long, y=lat,fill=cutpoint, group= group))+
  geom_polygon(data=df_AD_map,
               aes(x=long, y=lat,fill=cutpoint, group= group),   
               colour="grey10", 
               size = 0.4,
               show.legend=FALSE)+
  geom_polygon(data=df_AD_map[df_AD_map$id == 82,],
               aes(x=long, y=lat,fill=cutpoint,group= group),
               colour="grey10",
               size = 0.4,
               show.legend=FALSE)+
  geom_polygon(data=df_map[df_map$id == 59,],
               aes(x=long, y=lat,group= group),
               fill = "#d6d6d6",
               colour="grey10",
               size = 0.4,
               show.legend=FALSE)+
  geom_polygon(data=df_poly[df_poly$poly_fill == 888,], 
               aes(x=long, y=lat,group= group),
               fill = "grey100" ,
               show.legend=FALSE)+
  geom_polygon(data=df_poly[df_poly$poly_fill == 999,], 
               aes(x=long, y=lat,group= group),
               fill = "#d6d6d6" ,
               show.legend=FALSE)+
  geom_path(data=df_poly ,
            aes(x=long, y=lat, color=line_color,linetype=line_type,group= group),
            size = 0.4,
            show.legend=FALSE)+
  geom_path(data = df_line[df_line$line_color != 0, ],
            aes(x=long, y=lat,color=line_color,linetype=line_type,group=group),
            size = 0.4,
            show.legend=FALSE)+
  #      labs(title = paste0(cancer,", ",gender))+
  coord_equal() + 
  theme_opts +
  theme(legend.key.width= unit(2.6, "cm"), 
        legend.key.height= unit(1.4, "cm"),
        legend.direction= "vertical",
        legend.text = element_text(size=24),
        legend.title = element_text(size=24, hjust = 1),
        legend.title.align=0.5,
        legend.position =c(0.18, -0.02),
        legend.background = element_rect(fill="transparent"),
        plot.margin = unit(c(0,0,0,0),"lines"))+
  scale_fill_manual(name = "Proportion of preventable deaths (%)",
                    values= colors_green_GCO,
                    labels= labels_leg, 
                    na.value = "#cccccc",
                    drop=FALSE)+
  guides(fill = guide_legend(reverse = FALSE))+
  scale_color_manual(values=c("grey100", "grey10"))+
  scale_linetype_manual(values=c("solid", "11"))->AD_map_prev

ggsave("map_AD_all_cancers_prev_prop.pdf",width = 40, height = 30, pointsize = 12) 


#--- maps for Treatable AD----

allc <- AD_Map#[sex == 2,][type==1,]

break_quantile <- quantile(allc$pAD_treat, probs = seq(0, 1, by = 0.125), na.rm = T)

allc$cutpoint <- cut(allc$pAD_treat, breaks = rev(break_quantile), include.lowest = TRUE)
table(allc$cutpoint)
allc$cutpoint <- factor(allc$cutpoint, levels = rev(levels(allc$cutpoint)))

#merge with shapefile
df_AD_map <- merge(df_map, allc, by = c("id"), all.x=TRUE, sort=F )
df_AD_map<- df_AD_map[order(df_AD_map$int_map_index),]

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
  geom_polygon(data=df_AD_map,
               aes(x=long, y=lat,fill=cutpoint, group= group))+
  geom_polygon(data=df_AD_map,
               aes(x=long, y=lat,fill=cutpoint, group= group),   
               colour="grey10", 
               size = 0.4,
               show.legend=FALSE)+
  geom_polygon(data=df_AD_map[df_AD_map$id == 82,],
               aes(x=long, y=lat,fill=cutpoint,group= group),
               colour="grey10",
               size = 0.4,
               show.legend=FALSE)+
  geom_polygon(data=df_map[df_map$id == 59,],
               aes(x=long, y=lat,group= group),
               fill = "#d6d6d6",
               colour="grey10",
               size = 0.4,
               show.legend=FALSE)+
  geom_polygon(data=df_poly[df_poly$poly_fill == 888,], 
               aes(x=long, y=lat,group= group),
               fill = "grey100" ,
               show.legend=FALSE)+
  geom_polygon(data=df_poly[df_poly$poly_fill == 999,], 
               aes(x=long, y=lat,group= group),
               fill = "#d6d6d6" ,
               show.legend=FALSE)+
  geom_path(data=df_poly ,
            aes(x=long, y=lat, color=line_color,linetype=line_type,group= group),
            size = 0.4,
            show.legend=FALSE)+
  geom_path(data = df_line[df_line$line_color != 0, ],
            aes(x=long, y=lat,color=line_color,linetype=line_type,group=group),
            size = 0.4,
            show.legend=FALSE)+
  #      labs(title = paste0(cancer,", ",gender))+
  coord_equal() + 
  theme_opts +
  theme(legend.key.width= unit(2.6, "cm"), 
        legend.key.height= unit(1.4, "cm"),
        legend.direction= "vertical",
        legend.text = element_text(size=24),
        legend.title = element_text(size=24, hjust = 1),
        legend.title.align=0.5,
        legend.position =c(0.18, -0.02),
        legend.background = element_rect(fill="transparent"),
        plot.margin = unit(c(0,0,0,0),"lines"))+
  scale_fill_manual(name = "Proportion of treatable deaths (%)",
                    values= colors_green_GCO,
                    labels= labels_leg, 
                    na.value = "#cccccc",
                    drop=FALSE)+
  guides(fill = guide_legend(reverse = FALSE))+
  scale_color_manual(values=c("grey100", "grey10"))+
  scale_linetype_manual(values=c("solid", "11"))->AD_map_treatable

ggsave("map_AD_all_cancers_treatable_prop.pdf",width = 40, height = 30, pointsize = 12) 



#--- maps for Overall AD (treat + prev)----

allc <- AD_Map#[sex == 2,][type==1,]

break_quantile <- quantile(allc$pAD_treat_prev, probs = seq(0, 1, by = 0.125), na.rm = T)

allc$cutpoint <- cut(allc$pAD_treat_prev, breaks = rev(break_quantile), include.lowest = TRUE)
table(allc$cutpoint)
allc$cutpoint <- factor(allc$cutpoint, levels = rev(levels(allc$cutpoint)))

#merge with shapefile
df_AD_map <- merge(df_map, allc, by = c("id"), all.x=TRUE, sort=F )
df_AD_map<- df_AD_map[order(df_AD_map$int_map_index),]

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
  geom_polygon(data=df_AD_map,
               aes(x=long, y=lat, fill=cutpoint, group = group))+
  geom_polygon(data=df_AD_map,
               aes(x=long, y=lat, fill=cutpoint, group = group),   
               colour="grey10", 
               size = 0.4,
               show.legend=FALSE)+
  geom_polygon(data=df_AD_map[df_AD_map$id == 82,],
               aes(x=long, y=lat, fill=cutpoint, group = group),
               colour="grey10",
               size = 0.4,
               show.legend=FALSE)+
  geom_polygon(data=df_map[df_map$id == 59,],
               aes(x=long, y=lat, group= group),
               fill = "#d6d6d6",
               colour="grey10",
               size = 0.4,
               show.legend=FALSE)+
  geom_polygon(data=df_poly[df_poly$poly_fill == 888,], 
               aes(x=long, y=lat, group = group),
               fill = "grey100" ,
               show.legend=FALSE)+
  geom_polygon(data=df_poly[df_poly$poly_fill == 999,], 
               aes(x=long, y=lat, group = group),
               fill = "#d6d6d6" ,
               show.legend=FALSE)+
  geom_path(data=df_poly ,
            aes(x=long, y=lat, color=line_color, linetype = line_type, group = group),
            size = 0.4,
            show.legend=FALSE)+
  geom_path(data = df_line[df_line$line_color != 0, ],
            aes(x=long, y=lat,color=line_color,linetype=line_type,group=group),
            size = 0.4,
            show.legend=FALSE)+
  #      labs(title = paste0(cancer,", ",gender))+
  coord_equal() + 
  theme_opts +
  theme(legend.key.width= unit(2.6, "cm"), 
        legend.key.height= unit(1.4, "cm"),
        legend.direction= "vertical",
        legend.text = element_text(size=24),
        legend.title = element_text(size=24, hjust = 1),
        legend.title.align=0.5,
        legend.position =c(0.18, -0.02),
        legend.background = element_rect(fill="transparent"),
        plot.margin = unit(c(0,0,0,0),"lines"))+
  scale_fill_manual(name = "Proportion of avoidable deaths (%)",
                    values= colors_green_GCO,
                    labels= labels_leg, 
                    na.value = "#cccccc",
                    drop=FALSE)+
  guides(fill = guide_legend(reverse = FALSE))+
  scale_color_manual(values=c("grey100", "grey10"))+
  scale_linetype_manual(values=c("solid", "11"))->AD_map_total


ggsave("map_AD_all_cancers_preventable_treatable_prop.pdf",width = 40, height = 20, pointsize = 12) 


ggarrange( AD_map_prev, AD_map_treatable, AD_map_total,
          labels = c("a)", "b)", "c)"),
          ncol = 1, nrow = 3,
          font.label = list(size = 60, color = "black"))


ggsave("map_AD_prop.pdf", width = 40, height =67, limitsize = FALSE) 

