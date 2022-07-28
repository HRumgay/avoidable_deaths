library(ggplot2)

setwd("D:/Script/Pie")
rm(list=ls())

#####################
## GLOBOCAN classics
#####################
library(dplyr)
library(tidyr)

dat <- AD_by_cancer_site_1
# Color
library(stringr)
library(ggtern)
col <- read.csv("~/Documents/R_Projects/Data/cancer_color_2018.csv", stringsAsFactors = F)
col$Color.Hex <- ifelse(col$Color.Hex %in% c("","#"), rgb2hex(as.numeric(word(col$color,1)),as.numeric(word(col$color,2)),as.numeric(word(col$color,3))), col$Color.Hex)
col <- subset(col, select=c(cancer_code, Color.Hex))
col <- unique(col)
dat <- merge(dat, col, by="cancer_code", all.x = TRUE)
names(dat)[names(dat)=="Color.Hex"] <- "color"




# Bar
dbar <- dat %>% #filter(country_label=="World" & !cancer_code %in% c(17,37,38,39,40) & sex==0 & AD_cat!=2) %>%
  #select(cancer_code, cancer, AD_cat, cases, color) %>%
  arrange(AD_cat,-AD)

dbar <- dbar %>% 
  group_by(AD_cat) %>% 
  mutate(rank=row_number()) %>% 
  mutate(pct = AD / sum(AD), label_y=1-cumsum(pct)) %>%
filter(rank <= 7) 

dbargph <- subset(dbar, AD_cat=="Preventable")

# plot <- ggplot(dbargph, aes(x=AD_cat, y=pct, fill=reorder(cancer,-cases))) + 
#   geom_bar(stat="identity", width = 0.3) +
#   geom_text(aes(y=label_y, label=cancer), vjust=-1) +
#   xlab("") + ylab("Number of cancer cases (%)") +
#   theme_bw() +
#   ggtitle("") +   
#   theme(legend.position="right") +
#   theme(legend.title=element_blank()) +
#   scale_fill_manual(values = dbargph$color)
# 
# plot

dbargph2 <- as.matrix(cbind(rev(dbargph$cancer),rev(dbargph$pct)))
barplot(dbargph2,
        main="AD",
        xlab = "",
        ylab="Number of cancer cases (%)",
        col=rev(dbargph$color), border = "slate grey",
        space = 2, las=2,
        xlim = c(2,6)
)
text(4.8, dbargph$label_y+0.075, adj=1, labels = dbargph$cancer, cex = 1.4)
#legend("topleft", dbargph$cancer, fill = dbargph$color, inset = 0.05)

# Pie
dpie <- dat %>% #filter(country_label=="World" & !cancer_code %in% c(17,37,38,40) & sex==0 & AD_cat!=2) %>%
  select(cancer_code, cancer, AD, AD_cat) %>% arrange(AD_cat, -AD)

dpie <- dpie %>% group_by(AD_cat) %>% mutate(rank=row_number()) %>% filter(rank <= 7)
dpie$rank <- ifelse(dpie$rank>1,2,dpie$rank)
dpie$cancer <- ifelse(dpie$rank,"Top cancers", dpie$cancer)

dpie <- dpie %>% 
  group_by(AD,  AD_cat,  rank) %>% 
  mutate(AD=sum(AD)) %>%
  arrange(AD_cat, rank)

dpieo <- dpie %>% ungroup() %>% select(AD_cat, rank, AD) #%>% 
  #spread(rank, AD)
dpieo <- dpieo %>% #mutate(AD = `1` - `2`) %>% 
  select(AD_cat, AD) %>%
  mutate(cancer="Other cancers", rank=99)

dpie <- rbind(as.data.frame(dpie), as.data.frame(dpieo))
dpie <- dpie %>% filter(rank != 1) %>% arrange(AD_cat, rank)

dpie <- dpie %>% group_by(AD_cat) %>% mutate(pct=round(AD/sum(AD)*100,1))
dpie <- dpie %>% mutate(labls = paste(cancer, paste(as.character(pct), "%", sep = ""), sep = "\n")) %>%
                mutate(labls2 =paste(as.character(pct), "%", sep = ""))




# Preventable
dpiegph <- subset(dpie, AD_cat=="Preventable")
dtot <- dat %>% filter(AD_cat=="Preventable" ) %>% select(AD)
dtitle <- paste(as.character(dtot$cases),"new cases", sep = "\n")

#pie(dpiegph$cases, labels=dpiegph$labls, init.angle=90, col=c("#1a62a2", "#dcdcdc"), main="Preventable")
pie(dpiegph$AD, labels = NA, init.angle=90, col=c("#1a62a2", "#dcdcdc"), main="Preventable", border = "slate grey",sub=dtitle)
text(c(-0.4,0.4), c(0.3,-0.3), adj = 0.5, labels = dpiegph$labls2, cex = 1.2)

pdf("Bar_and_Pie_base.pdf",width = 10, height = 7, pointsize = 12)
#svg("Bar_and_Pie_fig3.svg",width = 10, height = 7, pointsize = 12)
par(mar=c(3,5,2,4), oma = c(2, 0, 2, 1)) # c(bottom, left, top, right), the default is c(5, 4, 4, 2)
par(mfrow=c(1,2))
barplot(dbargph2,
        xlab = "",
        ylab="Proportion of cases among top cancers (%)",
        col=rev(dbargph$color), border = "slate grey",
        space = 2.4, las=2,
        xlim = c(5,6)
)
text(5.6, dbargph$label_y+0.075, adj=1, labels = dbargph$cancer, cex = 1.4)
pie(dpiegph$cases, labels = dpiegph$cancer, init.angle=90, col=c("#1a62a2", "#dcdcdc"), border = "slate grey")
text(c(-0.4,0.4), c(0.2,-0.2), adj = 0.5, labels = dpiegph$labls2, cex = 1.8)
mtext("Preventable", outer = TRUE, adj = 0.6, cex = 2)
mtext(dtitle, side = 1, outer = TRUE, adj = 0.6, cex = 1.6)


#####################

