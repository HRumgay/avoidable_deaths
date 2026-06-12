

library(ggplot2)
library(terra)
library(sf)
library(data.table)

#  create a blank ggplot theme
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
                         legend.key.width= unit(2, "cm"), # 1
                         legend.key.height= unit(0.4, "cm"), # 0.2
                         plot.title = element_text(size=18)))




proj <- "+proj=eck3"
df_map <- vect("//inti/cin/Xchange/Mathieu/data/_shape/OMS_official_general", layer="general_2013")
df_map <- st_as_sf(df_map)
df_map$id = c(0:(nrow(df_map)-1))
df_map$int_map_index = c(1:(nrow(df_map)))

#create dt_shape for link to id
dt_shape <- as.data.table(df_map)
dt_shape <- dt_shape[, .(CNTRY_TERR, UN_CODE, id)]
names(dt_shape) <- c("country_label", "country_code", "id")
# add special china code 
dt_temp <- dt_shape[country_code == 156][, country_code := 160]
dt_shape <- rbind(dt_shape,dt_temp)
setorder(dt_shape, id)
# add missing code from the shape file ?!
dt_shape[country_label == "West Bank and Gaza Strip", country_code := 275]
dt_shape[country_label == "Greenland", country_code := 304]
dt_shape[country_label == "Montenegro", country_code := 499]
dt_shape[country_label == "Serbia", country_code := 688]
dt_shape[country_code == 0]


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

#load file example
print("----------/n")
print("load files example/n")
print('df_data <- read.csv(paste(dir_maps_data, file_list[1], sep="/"), sep=",")')


plot_gen_map <- function(legend_title = "", legend_reverse=FALSE) {
  
  
  plot <- ggplot() + 
    geom_sf(data=df_data_map[!is.na(df_data_map$fill),],
                 aes(fill=fill),   
                 colour="grey10", 
                 size = 0.4,
                 show.legend=TRUE)+
    geom_sf(data=df_data_map[is.na(df_data_map$fill),],
                 fill = "#808080",
                 colour="grey10",
                 size = 0.4,
                 show.legend=FALSE)+
    geom_sf(data=df_data_map[df_data_map$id == 82,],
                 aes(fill=fill),
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
    coord_sf(crs = proj)+
    theme_opts +
    theme(legend.key.width= unit(2.6, "cm"), # 1.5 before
          legend.key.height= unit(1.4, "cm"),
          legend.direction= "vertical",
          legend.text = element_text(size=28),
          legend.title = element_text(size=28, hjust = 1),
          legend.position =c(0.085, 0.1), # c(0.5, -0.1) before
          legend.background = element_rect(fill="transparent"),
          plot.margin = unit(c(0,0,0,0),"lines"))+
    scale_fill_manual(name = legend_title,
                      values= color_graph,
                      labels= labels_leg, 
                      na.value = "#808080",
                      drop=FALSE)+
    guides(fill = guide_legend(reverse = legend_reverse))+
    scale_color_manual(values=c("grey100", "grey10"), guide="none")+
    scale_linetype_manual(values=c("solid", "11"), guide="none")
  
  
  return(plot)
  
}

