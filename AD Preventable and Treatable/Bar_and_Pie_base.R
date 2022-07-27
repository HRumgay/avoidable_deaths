library(ggplot2)

setwd("D:/Script/Pie")
rm(list=ls())

#####################
## GLOBOCAN classics
#####################
library(dplyr)
library(tidyr)
dat <- read.csv("D:/DataShare/Globocan2018/Globocan_asr_cases_cumrisk_CRC.csv", stringsAsFactors = FALSE)

# Color
library(stringr)
library(ggtern)
col <- read.csv("../_data/cancer_color_2018.csv", stringsAsFactors = F)
col$Color.Hex <- ifelse(col$Color.Hex %in% c("","#"), rgb2hex(as.numeric(word(col$color,1)),as.numeric(word(col$color,2)),as.numeric(word(col$color,3))), col$Color.Hex)
col <- subset(col, select=c(cancer_code, Color.Hex))
col <- unique(col)
dat <- merge(dat, col, by="cancer_code", all.x = TRUE)
names(dat)[names(dat)=="Color.Hex"] <- "color"

# Bar
dbar <- dat %>% filter(country_label=="World" & !cancer_code %in% c(17,37,38,39,40) & sex==0 & type!=2) %>%
  select(cancer_code, cancer_label, type, cases, color) %>% arrange(type, -cases)

dbar <- dbar %>% group_by(type) %>% mutate(rank=row_number()) %>% filter(rank <= 5) %>%
  mutate(pct = cases / sum(cases), label_y=1-cumsum(pct)) 
dbar$type <- ifelse(dbar$type==0,"Incidence",ifelse(dbar$type==1,"Mortality","TYPE"))

dbargph <- subset(dbar, type=="Incidence")

# plot <- ggplot(dbargph, aes(x=type, y=pct, fill=reorder(cancer_label,-cases))) + 
#   geom_bar(stat="identity", width = 0.3) +
#   geom_text(aes(y=label_y, label=cancer_label), vjust=-1) +
#   xlab("") + ylab("Number of cancer cases (%)") +
#   theme_bw() +
#   ggtitle("") +   
#   theme(legend.position="right") +
#   theme(legend.title=element_blank()) +
#   scale_fill_manual(values = dbargph$color)
# 
# plot

dbargph2 <- as.matrix(cbind(rev(dbargph$cancer_label),rev(dbargph$pct)))

barplot(dbargph2,
        main="Incidence",
        xlab = "",
        ylab="Number of cancer cases (%)",
        col=rev(dbargph$color), border = "slate grey",
        space = 2, las=2,
        xlim = c(2,6)
)
text(4.8, dbargph$label_y+0.075, adj=1, labels = dbargph$cancer_label, cex = 1.4)
#legend("topleft", dbargph$cancer_label, fill = dbargph$color, inset = 0.05)

# Pie
dpie <- dat %>% filter(country_label=="World" & !cancer_code %in% c(17,37,38,40) & sex==0 & type!=2) %>%
  select(cancer_code, cancer_label, type, cases) %>% arrange(type, -cases)

dpie <- dpie %>% group_by(type) %>% mutate(rank=row_number()) %>% filter(rank <= 6)
dpie$rank <- ifelse(dpie$rank>1,2,dpie$rank)
dpie$cancer_label <- ifelse(dpie$rank>1,"Top cancers",dpie$cancer_label)
dpie$type <- ifelse(dpie$type==0,"Incidence",ifelse(dpie$type==1,"Mortality","TYPE"))

dpie <- dpie %>% group_by(cancer_label, type, rank) %>% summarise(cases=sum(cases)) %>% arrange(type, rank)
dpieo <- dpie %>% ungroup() %>% select(type, rank, cases) %>% spread(rank, cases)
dpieo <- dpieo %>% mutate(cases = `1` - `2`) %>% select(type, cases) %>% mutate(cancer_label="Other cancers", rank=99)

dpie <- rbind(as.data.frame(dpie), as.data.frame(dpieo))
dpie <- dpie %>% filter(rank != 1) %>% arrange(type, rank)

dpie <- dpie %>% group_by(type) %>% mutate(pct=round(cases/sum(cases)*100,1))
dpie <- dpie %>% mutate(labls = paste(cancer_label, paste(as.character(pct), "%", sep = ""), sep = "\n")) %>%
                mutate(labls2 =paste(as.character(pct), "%", sep = ""))

# Incidence
dpiegph <- subset(dpie, type=="Incidence")
dtot <- dat %>% filter(type==0 & country_label=="World" & cancer_code %in% c(39) & sex==0) %>% select(cases)
dtitle <- paste(as.character(dtot$cases),"new cases", sep = "\n")

#pie(dpiegph$cases, labels=dpiegph$labls, init.angle=90, col=c("#1a62a2", "#dcdcdc"), main="Incidence")
pie(dpiegph$cases, labels = NA, init.angle=90, col=c("#1a62a2", "#dcdcdc"), main="Incidence", border = "slate grey",sub=dtitle)
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
text(5.6, dbargph$label_y+0.075, adj=1, labels = dbargph$cancer_label, cex = 1.4)
pie(dpiegph$cases, labels = dpiegph$cancer_label, init.angle=90, col=c("#1a62a2", "#dcdcdc"), border = "slate grey")
text(c(-0.4,0.4), c(0.2,-0.2), adj = 0.5, labels = dpiegph$labls2, cex = 1.8)
mtext("Incidence", outer = TRUE, adj = 0.6, cex = 2)
mtext(dtitle, side = 1, outer = TRUE, adj = 0.6, cex = 1.6)
dev.off()

#####################

