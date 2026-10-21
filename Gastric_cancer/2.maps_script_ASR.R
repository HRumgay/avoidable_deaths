## script to plot maps of preventable and treatable ASRs

library(data.table)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggrepel)
library(terra)
library(sf)
library(ggpubr)


setwd("C:\\Users\\langseliuso\\OneDrive - International Agency for Research on Cancer\\Documents\\GitHub\\avoidable_deaths\\Gastric_cancer")



# load AD results


AD_country_all_cancers2



AD_Map <- as.data.table(AD_country_all_cancers2_editable)%>%filter(sex==0)

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

#create dt_shape for link to id
dt_shape <- as.data.table(df_map)
dt_shape <- dt_shape[, .(CNTRY_TERR, country_code, id)]
names(dt_shape) <- c("country_label", "country_code", "id")
# add special china code
dt_temp <- dt_shape[country_code == 156][, country_code := 160]
dt_shape <- rbind(dt_shape,dt_temp)
setorder(dt_shape, id)
# add missing code from the shape file ?!
 dt_shape[country_label == "West Bank and Gaza Strip", country_code := 275]
# dt_shape[country_label == "Greenland", country_code := 304]
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

# color vectors ----

colors_blue_GCO<-  c( "#084594","#2171b5", "#4292c6","#6baed6","#9ecae1", "#c6dbef","#deebf7", "#f7fbff") # colours for 8 bins

colors_green_GCO <- c("#005a32", "#238b45", "#41ab5d","#74c476", "#a1d99b","#c7e9c0", "#e5f5e0", "#f7fcf5")

colors_red_GCO <- c("#99000d","#cb181d","#ef3b2c","#fb6a4a", "#fc9272", "#fcbba1", "#fee0d2", "#fff5f0")


colors_blue_red_GCO <- c("#99000d","#ef3b2c","#fb6a4a", "#fc9272", "#fff5f0","#9ecae1", "#6baed6", "#2171b5", "#084594")


#--- maps for Preventable AD----

allc <- AD_Map#[sex == 2,][type==1,]

break_quantile <- unique(quantile(allc$AD_prev.asr, probs = seq(0, 1, by = 0.125), na.rm = T))

allc$cutpoint <- cut(allc$AD_prev.asr, breaks = rev(break_quantile), include.lowest = TRUE)
table(allc$cutpoint)
allc$cutpoint <- factor(allc$cutpoint, levels = rev(levels(allc$cutpoint)))

#merge with shapefile
df_data_map <- merge(df_map, allc, by = c("country_code"), all.x=TRUE, sort=F )
df_data_map<- df_data_map[order(df_data_map$int_map_index),]

#For the automatic legend
break_quantile_t <- paste(round(break_quantile,1),"", sep="")

labels_leg <- c()
for(i in 2:length(break_quantile_t)) {
  if(i == 2) {
    labels_leg[i-1] <- paste("< ", break_quantile_t[i])
  } else if(i == length(break_quantile_t)) {
    labels_leg[i-1] <- paste(">= ", break_quantile_t[i])
  } else {
    labels_leg[i-1] <- paste(break_quantile_t[i-1], " - ", break_quantile_t[i])
  }
}

labels_leg <- rev(labels_leg)

color_graph <- colors_blue_red_GCO

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
  scale_fill_manual(name = str_wrap("Total ASR of deaths avoidable through primary prevention (Deaths per 100,000 People)", 30),
                    values= colors_blue_red_GCO,
                    labels= labels_leg,
                    na.value = "#cccccc",
                    drop=FALSE)+
  guides(fill = guide_legend(reverse = FALSE))+
  scale_color_manual(values=c("grey100", "grey10", "grey50"), guide="none")+
  scale_linetype_manual(values=c("solid", "11", "dashed"), guide="none") -> AD_map_prev.asr

ggsave("map_AD_all_cancers_prev_ASR.png",width = 40, height = 30, pointsize = 12,
       path ="\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\_work\\oliver_langselius\\AD_PREV_TREAT\\Gastric2024")


#--- maps for Treatable AD----

allc <- AD_Map#[sex == 2,][type==1,]

break_quantile <- unique(quantile(allc$AD_treat.asr, probs = seq(0, 1, by = 0.125), na.rm = T))

allc$cutpoint <- cut(allc$AD_treat.asr, breaks = rev(break_quantile), include.lowest = TRUE)
table(allc$cutpoint)
allc$cutpoint <- factor(allc$cutpoint, levels = rev(levels(allc$cutpoint)))

#merge with shapefile
df_data_map <- merge(df_map, allc, by = c("country_code"), all.x=TRUE, sort=F )
df_data_map<- df_data_map[order(df_data_map$int_map_index),]

#For the automatic legend
break_quantile_t <- paste(round(break_quantile,1),"", sep="")

labels_leg <- c()
for(i in 2:length(break_quantile_t)) {
  if(i == 2) {
    labels_leg[i-1] <- paste("< ", break_quantile_t[i])
  } else if(i == length(break_quantile_t)) {
    labels_leg[i-1] <- paste(">= ", break_quantile_t[i])
  } else {
    labels_leg[i-1] <- paste(break_quantile_t[i-1], " - ", break_quantile_t[i])
  }
}

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
  scale_fill_manual(name = str_wrap("Total ASR of deaths avoidable through early detection and treatment (Deaths per 100,000 People)", 30),
                    values= colors_blue_red_GCO,
                    labels= labels_leg,
                    na.value = "#cccccc",
                    drop=FALSE)+
  guides(fill = guide_legend(reverse = FALSE))+
  scale_color_manual(values=c("grey100", "grey10", "grey50"), guide="none")+
  scale_linetype_manual(values=c("solid", "11", "dashed"), guide="none") -> AD_map_treatable.asr

ggsave("map_AD_all_cancers_treatable_ASR.png",width = 40, height = 30, pointsize = 12,
       path ="\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\_work\\oliver_langselius\\AD_PREV_TREAT\\Gastric2024")



#--- maps for Overall AD (treat + prev)----

allc <- AD_Map#[sex == 2,][type==1,]

break_quantile <- unique(quantile(allc$AD_treat_prev.asr, probs = seq(0, 1, by = 0.125), na.rm = T))

allc$cutpoint <- cut(allc$AD_treat_prev.asr, breaks = rev(break_quantile), include.lowest = TRUE)
table(allc$cutpoint)
allc$cutpoint <- factor(allc$cutpoint, levels = rev(levels(allc$cutpoint)))

#merge with shapefile
df_data_map <- merge(df_map, allc, by = c("country_code"), all.x=TRUE, sort=F )
df_data_map<- df_data_map[order(df_data_map$int_map_index),]

#For the automatic legend
break_quantile_t <- paste(round(break_quantile,1),"", sep="")

labels_leg <- c()
for(i in 2:length(break_quantile_t)) {
  if(i == 2) {
    labels_leg[i-1] <- paste("< ", break_quantile_t[i])
  } else if(i == length(break_quantile_t)) {
    labels_leg[i-1] <- paste(">= ", break_quantile_t[i])
  } else {
    labels_leg[i-1] <- paste(break_quantile_t[i-1], " - ", break_quantile_t[i])
  }
}

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
  scale_fill_manual(name = str_wrap("Total ASR of deaths avoidable through primary prevention and early detection and treatment combined (Deaths per 100,000 People)", 30),
                    values= colors_blue_red_GCO,
                    labels= labels_leg,
                    na.value = "#cccccc",
                    drop=FALSE)+
  guides(fill = guide_legend(reverse = FALSE))+
  scale_color_manual(values=c("grey100", "grey10", "grey50"), guide="none")+
  scale_linetype_manual(values=c("solid", "11", "dashed"), guide="none") -> AD_map_total.asr

ggsave("map_AD_all_cancers_preventable_treatable_ASR.png",width = 40, height = 30, pointsize = 12,
       path ="\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\_work\\oliver_langselius\\AD_PREV_TREAT\\Gastric2024")



#add distributions
library(ggpubr)
library(ggridges)
library(stringr)

colors_blue_red_GCO <- c("#99000d","#ef3b2c","#fb6a4a", "#fc9272", "#fff5f0",
                         "#9ecae1", "#6baed6", "#2171b5", "#084594")

# Prepare data for density plot - combining preventable, treatable, and avoidable deaths
density_data_preventable <- AD_Map %>%
  select(country_code, country_label, AD_prev.asr) %>%
  rename("AD.asr" = "AD_prev.asr") %>%
  mutate(AD_cat = "Preventable")

density_data_treatable <- AD_Map %>%
  select(country_code, country_label, AD_treat.asr) %>%
  rename("AD.asr" = "AD_treat.asr") %>%
  mutate(AD_cat = "Treat")

density_data_data <- AD_Map %>%
  select(country_code, country_label, AD_treat_prev.asr) %>%
  rename("AD.asr" = "AD_treat_prev.asr") %>%
  mutate(AD_cat = "Avoidable") %>%
  full_join(density_data_preventable) %>%
  full_join(density_data_treatable)

# Reusable function -- replicates the SAME quantile-cut binning used in the maps
make_density_plot <- function(data, category) {
  d <- data %>% filter(AD_cat == category)
  
  # Same quantile-break logic as the map code
  break_quantile <- unique(quantile(d$AD.asr, probs = seq(0, 1, by = 0.125), na.rm = TRUE))
  mids     <- (break_quantile[-1] + break_quantile[-length(break_quantile)]) / 2
  cols     <- rev(colors_blue_red_GCO[seq_along(mids)])   # low -> high
  stops    <- c(min(break_quantile), mids, max(break_quantile))
  cols_ext <- c(cols[1], cols, cols[length(cols)])        # end stops stop NA tails
  
  ggplot(d, aes(x = AD.asr, y = 0, fill = after_stat(x))) +
    geom_density_ridges_gradient(
      color = NA,
      scale = 3,
      rel_min_height = 0.01
    ) +
    scale_fill_gradientn(
      colours = cols_ext,
      values  = scales::rescale(stops, from = range(break_quantile)),
      limits  = range(break_quantile),
      oob     = scales::squish,
      guide   = "none"
    ) +
    labs(
      x = "Age standardized Avoidable Deaths",
      y = NULL
    ) +
    theme_minimal(base_size = 7) +
    theme(
      axis.title = element_text(size = 30),
      axis.text = element_text(size = 20),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.minor = element_blank(),
      plot.margin = margin(2, 2, 2, 2),
      legend.position = "none"
    )
}

density_plot_avoidable   <- make_density_plot(density_data_data, "Avoidable")
density_plot_preventable <- make_density_plot(density_data_data, "Preventable")
density_plot_treatable   <- make_density_plot(density_data_data, "Treat")


# larger legend, applied only inside the composite (the standalone map PNGs are unaffected)
big_legend <- theme(
  legend.text       = element_text(size = 34),
  legend.title      = element_text(size = 34, hjust = 0),
  legend.key.width  = unit(3.4, "cm"),
  legend.key.height = unit(2.2, "cm")
)

# Helper: left-side column = small density on top, legend below
make_left_column <- function(map_plot, density_plot) {
  # the legend comes from the MAP; "right" gives get_legend() a standard position
  legend_grob <- get_legend(map_plot + big_legend + theme(legend.position = "right"))
  density_no_legend <- density_plot + theme(legend.position = "none")
  
  ggarrange(
    density_no_legend,
    as_ggplot(legend_grob),
    ncol = 1,
    heights = c(0.35, 0.65)
  )
}

# each map is paired with its OWN density plot
left_a <- make_left_column(AD_map_prev.asr,      density_plot_preventable)
left_b <- make_left_column(AD_map_treatable.asr, density_plot_treatable)
left_c <- make_left_column(AD_map_total.asr,     density_plot_avoidable)

# the maps no longer carry their own legend
panel_a <- ggarrange(left_a, AD_map_prev.asr      + theme(legend.position = "none"),
                     ncol = 2, nrow = 1, widths = c(0.25, 0.75))
panel_b <- ggarrange(left_b, AD_map_treatable.asr + theme(legend.position = "none"),
                     ncol = 2, nrow = 1, widths = c(0.25, 0.75))
panel_c <- ggarrange(left_c, AD_map_total.asr     + theme(legend.position = "none"),
                     ncol = 2, nrow = 1, widths = c(0.25, 0.75))

# NULL rows are spacers, so b) and c) no longer sit under the previous panel
ggarrange(
  panel_a, NULL, panel_b, NULL, panel_c,
  labels  = c("a)", "", "b)", "", "c)"),
  ncol = 1, nrow = 5,
  heights = c(1, 0.08, 1, 0.08, 1),
  font.label = list(size = 60, color = "black")
)

ggsave("map_AD_asr.pdf", width = 40*0.85, height =60*0.8, dpi=600, limitsize = FALSE,
       path ="\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\_work\\oliver_langselius\\AD_PREV_TREAT\\Gastric2024") 


ggsave("map_AD_asr.png", width = 40*0.85, height =60*0.8, dpi=500, limitsize = FALSE,
       path ="\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\_work\\oliver_langselius\\AD_PREV_TREAT\\Gastric2024") 

