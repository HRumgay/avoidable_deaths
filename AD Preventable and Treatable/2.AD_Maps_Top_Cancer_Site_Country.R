## script to plot maps of cancer deaths by 

library(data.table)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggrepel)
library(rgdal)
library(pastecs)
library(ggsci)
library(scales)
library(ggpubr)

AD_Map <- as.data.table(Avoidable_Deaths_Simulated_All_age_cat_overall)
cancerss<-Avoidable_Deaths_Simulated_All_age_cat_overall%>%select(cancer_code, cancer)%>%distinct()
cancer_colors<-read.csv("~/Documents/R_Projects/Data/cancer_color_2018.csv", sep=",")%>%
  as.data.frame()%>%
  select(cancer_code, Color.Hex)%>%
 # dplyr::rename("cancer"="cancer_label")%>%
  filter(Color.Hex!="")%>%
  filter(Color.Hex!="#")%>%
  left_join(cancerss, by=c("cancer_code"))%>%
  select(-cancer_code)
  

cancer_colors
palette1_named = setNames(object = cancer_colors$Color.Hex, nm = cancer_colors$cancer)
print(palette1_named)

# load id for each country
dict_id <-  as.data.table(read.csv("~/Documents/R_Projects/Data/_shape/id_OMS_official_general_map.csv", sep=","))
dict_id %>% dplyr::select(-country_label)-> dict_id

# merge paf data with dict_id
AD_Map <- merge(AD_Map, dict_id, by = c("country_code"), all.x = T)
 # left_join(cancer_colors, by=c("cancer_code"))


# Color scale
#myColors <- AD_Map$Color.Hex
#names(myColors) <- levels(AD_Map$cancer)
#colScale <- scale_colour_manual(name =cancer,values = myColors)
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

#Color specific file


#merge with shapefile
allc<-AD_Map%>%
  group_by(country_code)%>%
  filter(AD_prev==max(AD_prev))
df_AD_map <- merge(df_map, allc, by = c("id"), all.x=TRUE, sort=F )
df_AD_map<- df_AD_map[order(df_AD_map$int_map_index),]%>%
  arrange(desc(cancer))

#Color specific file
allc_prev<-allc%>%select(cancer)%>%distinct()

cancer_colors_prev<- cancer_colors%>%filter(cancer%in%allc_prev$cancer)
palette1_named_prev =  setNames(object = cancer_colors_prev$Color.Hex, nm = cancer_colors_prev$cancer)



#Printing the map

df_AD_map%>%
ggplot() + 
  geom_polygon(data=df_AD_map,
               aes(x=long, y=lat,fill=cancer ,group= group))+
  geom_polygon(data=df_AD_map,
               aes(x=long, y=lat,fill=cancer, group= group),   
               colour="grey10", 
               size = 0.4,
               show.legend=FALSE
               )+
  geom_polygon(data=df_AD_map[df_AD_map$id == 82,],
               aes(x=long, y=lat,fill=cancer, group= group),
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
        legend.position =c(0.18, -0.02),
        legend.background = element_rect(fill="transparent"),
        plot.margin = unit(c(0,0,0,0),"lines"))+
  guides(fill=guide_legend(title="Cancer site with the highest number preventable deaths"))+
  #scale_color_manual(name = cancer,values=df_AD_map$Color.Hex)+
  scale_fill_manual(values = palette1_named_prev)+
  #scale_fill_lancet()+
  scale_linetype_manual(values=c("solid", "11"))->max_prev

ggsave("map_AD_all_cancers_prev_max_country.pdf",width = 40, height = 30, pointsize = 12) 

#--- maps for Treatable AD----

#merge with shapefile
allc<-AD_Map%>%
  group_by(country_code)%>%
  filter(AD_treat==max(AD_treat))
df_AD_map <- merge(df_map, allc, by = c("id"), all.x=TRUE, sort=F )
df_AD_map<- df_AD_map[order(df_AD_map$int_map_index),]%>%
  arrange(desc(cancer))

#colors 
allc_treat<-allc%>%ungroup()%>%select(cancer)%>%distinct()

cancer_colors_treat<- cancer_colors%>%filter(cancer%in%allc_treat$cancer)
palette1_named_treat =  setNames(object = cancer_colors_treat$Color.Hex, nm = cancer_colors_treat$cancer)


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
        legend.background = element_rect(fill= "transparent"),
        plot.margin = unit(c(0,0,0,0),"lines"))+
  
  guides(fill=guide_legend(title="Cancer site with the highest number treatable deaths"))+
  scale_fill_manual(values = palette1_named_treat)+
  #scale_color_manual(values=c("grey100", "grey10"))+
#  scale_fill_lancet()+
  scale_linetype_manual(values=c("solid", "11"))-> max_treat

ggsave("map_AD_all_cancers_treatable_max_country.pdf",width = 40, height = 30, pointsize = 12) 

#--- maps for total AD----

#merge with shapefile
allc<-AD_Map%>%
  group_by(country_code)%>%
  filter(AD_treat_prev==max(AD_treat_prev))
df_AD_map <- merge(df_map, allc, by = c("id"), all.x=TRUE, sort=F )
df_AD_map<- df_AD_map[order(df_AD_map$int_map_index),]%>%
  arrange(desc(cancer))

#colors 
allc_treat_prev<-allc%>%
  select(cancer)%>%
  distinct()

cancer_colors_treat_prev<- cancer_colors%>%
  filter(cancer%in%allc_treat_prev$cancer)

palette1_named_treat_prev =  setNames(object = cancer_colors_treat_prev$Color.Hex, nm = cancer_colors_treat_prev$cancer)


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
  
  guides(fill=guide_legend(title="Cancer site with the highest number avoidable deaths"))+
  scale_fill_manual(values = palette1_named_treat_prev)+
 # scale_fill_lancet()+
  scale_linetype_manual(values=c("solid", "11"))->max_total

ggsave("map_AD_all_cancers_treat_prev_max_country.pdf", width = 40, height = 30, pointsize = 12) 



ggarrange(max_prev, max_treat,max_total,  
          labels = c("a)", "b)", "c)"),
          ncol = 1, nrow = 3,
          font.label = list(size = 60, color = "black"))
ggsave("map_AD_max.pdf",width = 40, height =70,limitsize = FALSE) 

