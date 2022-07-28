

## script to plot maps of cancer deaths by 

library(data.table)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggrepel)
library(rgdal)
library(pastecs)
library(ggsci)


AD_Map <- as.data.table(Avoidable_Deaths_Simulated_All_age_cat_overall)


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

# # color vectors ----
# 
# colors_blue_GCO<-  c( "#084594","#2171b5", "#4292c6","#6baed6","#9ecae1", "#c6dbef","#deebf7", "#f7fbff") # colours for 8 bins
# 
# colors_green_GCO <- c("#005a32", "#238b45", "#41ab5d","#74c476", "#a1d99b","#c7e9c0", "#e5f5e0", "#f7fcf5") 
# 
# colors_red_GCO <- c("#99000d","#cb181d","#ef3b2c","#fb6a4a", "#fc9272", "#fcbba1", "#fee0d2", "#fff5f0") 


#--- maps for Preventable AD----

#merge with shapefile
allc<-AD_Map%>%
  group_by(country_code)%>%
  filter(AD_prev==max(AD_prev))
df_AD_map <- merge(df_map, allc, by = c("id"), all.x=TRUE, sort=F )
df_AD_map<- df_AD_map[order(df_AD_map$int_map_index),]

#Printing the map

df_AD_map%>%
ggplot() + 
  geom_polygon(data=df_AD_map,
               aes(x=long, y=lat,fill=cancer, group= group))+
  geom_polygon(data=df_AD_map,
               aes(x=long, y=lat,fill=cancer, group= group),   
               colour="grey10", 
               size = 0.4,
               show.legend=FALSE
               )+
  geom_polygon(data=df_AD_map[df_AD_map$id == 82,],
               aes(x=long, y=lat,fill=cancer,group= group),
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
            aes(x=long, y=lat, color=line_color,linetype=line_type,group=group),
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
  
  guides(fill=guide_legend(title="Cancer site with the highest proportion preventable deaths in each country"))+
  scale_color_manual(values=c("grey100", "grey10"))+
  scale_fill_lancet()+
  scale_linetype_manual(values=c("solid", "11"))

ggsave("map_AD_all_cancers_prev_prop_max_country.pdf",width = 40, height = 30, pointsize = 12) 

#--- maps for Treatable AD----

#merge with shapefile
allc<-AD_Map%>%
  group_by(country_code)%>%
  filter(AD_treat==max(AD_treat))
df_AD_map <- merge(df_map, allc, by = c("id"), all.x=TRUE, sort=F )
df_AD_map<- df_AD_map[order(df_AD_map$int_map_index),]

#Printing the map

df_AD_map%>%
  ggplot() + 
  geom_polygon(data=df_AD_map,
               aes(x=long, y=lat,fill=cancer, group= group))+
  geom_polygon(data=df_AD_map,
               aes(x=long, y=lat,fill=cancer, group= group),   
               colour="grey10", 
               size = 0.4,
               show.legend=FALSE
  )+
  geom_polygon(data=df_AD_map[df_AD_map$id == 82,],
               aes(x=long, y=lat,fill=cancer,group= group),
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
            aes(x=long, y=lat, color=line_color,linetype=line_type,group=group),
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
  
  guides(fill=guide_legend(title="Cancer site with the highest proportion treatable deaths by cancer site"))+
  scale_color_manual(values=c("grey100", "grey10"))+
  scale_fill_lancet()+
  scale_linetype_manual(values=c("solid", "11"))

ggsave("map_AD_all_cancers_treatable_prop_max_country.pdf",width = 40, height = 30, pointsize = 12) 

#--- maps for total AD----

#merge with shapefile
allc<-AD_Map%>%
  group_by(country_code)%>%
  filter(AD_treat_prev==max(AD_treat_prev))
df_AD_map <- merge(df_map, allc, by = c("id"), all.x=TRUE, sort=F )
df_AD_map<- df_AD_map[order(df_AD_map$int_map_index),]

#Printing the map

df_AD_map%>%
  ggplot() + 
  geom_polygon(data=df_AD_map,
               aes(x=long, y=lat,fill=cancer, group= group))+
  geom_polygon(data=df_AD_map,
               aes(x=long, y=lat,fill=cancer, group= group),   
               colour="grey10", 
               size = 0.4,
               show.legend=FALSE
  )+
  geom_polygon(data=df_AD_map[df_AD_map$id == 82,],
               aes(x=long, y=lat,fill=cancer,group= group),
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
            aes(x=long, y=lat, color=line_color,linetype=line_type,group=group),
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
  
  guides(fill=guide_legend(title="Cancer site with the highest proportion total (preventable + treatable) deaths in each country"))+
  scale_color_manual(values=c("grey100", "grey10"))+
  scale_fill_lancet()+
  scale_linetype_manual(values=c("solid", "11"))

ggsave("map_AD_all_cancers_treat_prev_prop_max_country.pdf",width = 40, height = 30, pointsize = 12) 

